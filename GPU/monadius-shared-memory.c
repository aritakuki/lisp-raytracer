#define _GNU_SOURCE

#include <errno.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define MONADIUS_SHARED_MAGIC UINT64_C(0x4d4f4e4152495553)
#define MONADIUS_SHARED_VERSION UINT32_C(2)
#define MONADIUS_BUFFER_COUNT UINT32_C(3)
#define MONADIUS_PIXEL_FORMAT_RGBA8 UINT32_C(1)
#define MONADIUS_NO_READER UINT32_MAX

enum producer_state {
  PRODUCER_CREATED = 0,
  PRODUCER_RUNNING = 1,
  PRODUCER_FAILED = 2,
  PRODUCER_STOPPED = 3,
};

typedef struct __attribute__((aligned(64))) monadius_shared_header {
  uint64_t magic;
  uint32_t version;
  uint32_t header_bytes;
  uint32_t width;
  uint32_t height;
  uint32_t buffer_count;
  uint32_t pixel_format;
  uint64_t pixel_bytes;
  uint64_t total_bytes;
  uint32_t producer_pid;
  uint32_t requested_stage;
  uint64_t reserved1;
  uint64_t generation;
  uint32_t front_index;
  uint32_t reader_index;
  uint32_t stop_requested;
  uint32_t producer_state;
  int32_t producer_error;
  uint32_t reserved2;
  uint64_t heartbeat;
  unsigned char reserved3[24];
} monadius_shared_header;

_Static_assert(sizeof(monadius_shared_header) == 128,
               "live background shared header must be 128 bytes");
_Static_assert(__builtin_offsetof(monadius_shared_header, generation) == 64,
               "live background atomic fields moved");
_Static_assert(__builtin_offsetof(monadius_shared_header, requested_stage) == 52,
               "live background stage field moved");
_Static_assert(__builtin_offsetof(monadius_shared_header, heartbeat) == 96,
               "live background protocol layout changed");

typedef struct monadius_shared_context {
  monadius_shared_header* header;
  unsigned char* pixels;
  size_t mapping_bytes;
  uint32_t next_index;
} monadius_shared_context;

static uint32_t atomic_load_u32(const uint32_t* value) {
  return __atomic_load_n(value, __ATOMIC_SEQ_CST);
}

static void atomic_store_u32(uint32_t* target, uint32_t value) {
  __atomic_store_n(target, value, __ATOMIC_SEQ_CST);
}

static uint64_t atomic_add_u64(uint64_t* target, uint64_t value) {
  return __atomic_add_fetch(target, value, __ATOMIC_SEQ_CST);
}

static int parse_positive_pid(const char* text, pid_t* result) {
  char* end = NULL;
  errno = 0;
  long value = text == NULL ? 0 : strtol(text, &end, 10);
  if (errno != 0 || text == NULL || *text == '\0' || *end != '\0' ||
      value <= 0) {
    return 0;
  }
  *result = (pid_t)value;
  return 1;
}

// Arm this before Quicklisp or cl-cuda is loaded. If Main is killed, SIGKILL
// prevents an orphaned renderer from retaining the CUDA context. The parent
// check closes the race where Main dies immediately before PR_SET_PDEATHSIG.
int monadiusSharedArmParentDeath(void) {
  pid_t expected_parent = 0;
  if (!parse_positive_pid(getenv("MONADIUS_RAY_PARENT_PID"),
                          &expected_parent)) {
    fprintf(stderr, "MONADIUS_RAY_PARENT_PID is missing or invalid.\n");
    return 0;
  }
  if (prctl(PR_SET_PDEATHSIG, SIGKILL) != 0) {
    fprintf(stderr, "Could not arm parent-death handling: %s.\n",
            strerror(errno));
    return 0;
  }
  if (getppid() != expected_parent) {
    fprintf(stderr, "Monadius exited before the Lisp renderer attached.\n");
    return 0;
  }
  return 1;
}

void* monadiusSharedAttach(int fd) {
  struct stat status;
  if (fd < 0 || fstat(fd, &status) != 0 || status.st_size < 128) {
    fprintf(stderr, "Invalid live background shared-memory descriptor.\n");
    if (fd >= 0) close(fd);
    return NULL;
  }
  const size_t mapping_bytes = (size_t)status.st_size;
  void* mapping = mmap(NULL, mapping_bytes, PROT_READ | PROT_WRITE,
                       MAP_SHARED, fd, 0);
  const int mapping_errno = errno;
  close(fd);
  if (mapping == MAP_FAILED) {
    fprintf(stderr, "Could not map live background memory: %s.\n",
            strerror(mapping_errno));
    return NULL;
  }

  monadius_shared_header* header = (monadius_shared_header*)mapping;
  int valid = header->magic == MONADIUS_SHARED_MAGIC &&
              header->version == MONADIUS_SHARED_VERSION &&
              header->header_bytes == sizeof(monadius_shared_header) &&
              header->buffer_count == MONADIUS_BUFFER_COUNT &&
              header->pixel_format == MONADIUS_PIXEL_FORMAT_RGBA8 &&
              header->width > 0 && header->height > 0 &&
              header->pixel_bytes ==
                  (uint64_t)header->width * (uint64_t)header->height * 4 &&
              header->total_bytes == mapping_bytes &&
              header->total_bytes == sizeof(monadius_shared_header) +
                                         header->pixel_bytes *
                                             MONADIUS_BUFFER_COUNT;
  if (!valid) {
    fprintf(stderr, "Live background shared-memory protocol mismatch.\n");
    munmap(mapping, mapping_bytes);
    return NULL;
  }

  monadius_shared_context* context =
      (monadius_shared_context*)calloc(1, sizeof(*context));
  if (context == NULL) {
    fprintf(stderr, "Could not allocate live background producer context.\n");
    munmap(mapping, mapping_bytes);
    return NULL;
  }
  context->header = header;
  context->pixels = (unsigned char*)(header + 1);
  context->mapping_bytes = mapping_bytes;
  context->next_index = 0;
  header->producer_pid = (uint32_t)getpid();
  atomic_store_u32(&header->producer_state, PRODUCER_RUNNING);
  return context;
}

int monadiusSharedWidth(void* opaque) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  return context == NULL ? 0 : (int)context->header->width;
}

int monadiusSharedHeight(void* opaque) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  return context == NULL ? 0 : (int)context->header->height;
}

int monadiusSharedStage(void* opaque) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  if (context == NULL) return 1;
  const uint32_t stage = atomic_load_u32(&context->header->requested_stage);
  return stage >= 1 && stage <= 3 ? (int)stage : 1;
}

int monadiusSharedShouldStop(void* opaque) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  return context == NULL ||
         atomic_load_u32(&context->header->stop_requested) != 0;
}

static unsigned char byte_channel(float value) {
  if (!isfinite(value)) return 0;
  if (value < 0.0f) value = 0.0f;
  if (value > 1.0f) value = 1.0f;
  return (unsigned char)(value * 255.0f + 0.5f);
}

int monadiusSharedPublishRgb(void* opaque, const float* red,
                            const float* green, const float* blue, int width,
                            int height) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  if (context == NULL || red == NULL || green == NULL || blue == NULL ||
      width != (int)context->header->width ||
      height != (int)context->header->height) {
    return 0;
  }

  const uint32_t front = atomic_load_u32(&context->header->front_index);
  const uint32_t reader = atomic_load_u32(&context->header->reader_index);
  uint32_t writable = MONADIUS_BUFFER_COUNT;
  for (uint32_t offset = 1; offset <= MONADIUS_BUFFER_COUNT; ++offset) {
    const uint32_t candidate =
        (context->next_index + offset) % MONADIUS_BUFFER_COUNT;
    if (candidate != front && candidate != reader) {
      writable = candidate;
      break;
    }
  }
  if (writable >= MONADIUS_BUFFER_COUNT) {
    // With one reader, one front, and three buffers this cannot occur unless
    // the shared header was corrupted. Do not publish a partially-known slot.
    return 0;
  }

  unsigned char* output =
      context->pixels + (size_t)writable * context->header->pixel_bytes;
  const size_t pixel_count =
      (size_t)context->header->width * context->header->height;
  for (size_t pixel = 0; pixel < pixel_count; ++pixel) {
    const size_t offset = pixel * 4;
    output[offset] = byte_channel(red[pixel]);
    output[offset + 1] = byte_channel(green[pixel]);
    output[offset + 2] = byte_channel(blue[pixel]);
    output[offset + 3] = 255;
  }

  context->next_index = writable;
  atomic_store_u32(&context->header->front_index, writable);
  const uint64_t generation =
      atomic_add_u64(&context->header->generation, UINT64_C(1));
  __atomic_store_n(&context->header->heartbeat, generation, __ATOMIC_SEQ_CST);
  return 1;
}

void monadiusSharedFail(void* opaque, int error_code) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  if (context == NULL) return;
  __atomic_store_n(&context->header->producer_error, (int32_t)error_code,
                   __ATOMIC_SEQ_CST);
  atomic_store_u32(&context->header->producer_state, PRODUCER_FAILED);
}

void monadiusSharedClose(void* opaque) {
  monadius_shared_context* context = (monadius_shared_context*)opaque;
  if (context == NULL) return;
  if (atomic_load_u32(&context->header->producer_state) != PRODUCER_FAILED) {
    atomic_store_u32(&context->header->producer_state, PRODUCER_STOPPED);
  }
  munmap(context->header, context->mapping_bytes);
  free(context);
}
