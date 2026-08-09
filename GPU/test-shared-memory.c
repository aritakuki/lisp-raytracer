#define _GNU_SOURCE

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <sys/mman.h>
#include <sys/syscall.h>
#include <unistd.h>

typedef struct __attribute__((aligned(64))) test_shared_header {
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
  uint32_t reserved0;
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
} test_shared_header;

_Static_assert(sizeof(test_shared_header) == 128, "test protocol size");
_Static_assert(__builtin_offsetof(test_shared_header, generation) == 64,
               "test protocol atomics");

int monadiusSharedPublishRgb(void*, const float*, const float*, const float*,
                            int, int);
int monadiusSharedShouldStop(void*);
void* monadiusSharedAttach(int);
void monadiusSharedClose(void*);

static int require(int condition, const char* message) {
  if (condition) return 1;
  fprintf(stderr, "shared-memory self-test failed: %s\n", message);
  return 0;
}

int main(void) {
  const uint32_t width = 2;
  const uint32_t height = 1;
  const uint64_t pixel_bytes = width * height * 4;
  const size_t total_bytes = sizeof(test_shared_header) + pixel_bytes * 3;
  const int fd = (int)syscall(SYS_memfd_create, "monadius-ray-test", 0);
  if (!require(fd >= 0, "memfd_create")) return 1;
  if (!require(ftruncate(fd, (off_t)total_bytes) == 0, "ftruncate")) return 1;
  test_shared_header* header = mmap(NULL, total_bytes, PROT_READ | PROT_WRITE,
                                    MAP_SHARED, fd, 0);
  if (!require(header != MAP_FAILED, "mmap")) return 1;
  memset(header, 0, total_bytes);
  header->magic = UINT64_C(0x4d4f4e4152495553);
  header->version = 1;
  header->header_bytes = sizeof(*header);
  header->width = width;
  header->height = height;
  header->buffer_count = 3;
  header->pixel_format = 1;
  header->pixel_bytes = pixel_bytes;
  header->total_bytes = total_bytes;
  header->reader_index = UINT32_MAX;

  void* context = monadiusSharedAttach(dup(fd));
  if (!require(context != NULL, "producer attach")) return 1;
  if (!require(header->producer_state == 1, "producer running state")) return 1;

  const float red[2] = {0.0f, 1.0f};
  const float green[2] = {0.5f, NAN};
  const float blue[2] = {-1.0f, 2.0f};
  if (!require(monadiusSharedPublishRgb(context, red, green, blue, 2, 1) == 1,
               "first publication")) return 1;
  if (!require(header->generation == 1 && header->front_index == 1,
               "first generation metadata")) return 1;
  unsigned char* pixels = (unsigned char*)(header + 1) + pixel_bytes;
  const unsigned char expected[8] = {0, 128, 0, 255, 255, 0, 255, 255};
  if (!require(memcmp(pixels, expected, sizeof(expected)) == 0,
               "float-to-RGBA conversion")) return 1;

  // Slot 2 is claimed by the consumer and slot 1 is front. The next publish
  // must therefore use slot 0 rather than overwrite either protected slot.
  header->reader_index = 2;
  if (!require(monadiusSharedPublishRgb(context, red, green, blue, 2, 1) == 1,
               "publication with reader claim")) return 1;
  if (!require(header->generation == 2 && header->front_index == 0,
               "triple-buffer selection")) return 1;

  header->stop_requested = 1;
  if (!require(monadiusSharedShouldStop(context) == 1, "stop request")) return 1;
  monadiusSharedClose(context);
  if (!require(header->producer_state == 3, "producer stopped state")) return 1;

  munmap(header, total_bytes);
  close(fd);
  puts("Shared-memory protocol self-test passed.");
  return 0;
}
