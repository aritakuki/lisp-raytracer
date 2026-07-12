#include "int.h"
#include "float.h"
#include "float3.h"
#include "float4.h"
#include "double.h"
#include "double3.h"
#include "double4.h"
#include "curand.h"


/**
 *  Kernel globals
 */



/**
 *  Kernel function prototypes
 */

extern "C" __global__ void gpu_raytracer_raytrace_kernel( float* out_r, float* out_g, float* out_b, int width, int height, float width_f, float height_f, float* sphere_cx, float* sphere_cy, float* sphere_cz, float* sphere_r, float* sphere_col_r, float* sphere_col_g, float* sphere_col_b, float* sphere_refl, int num_spheres, float eye_x, float eye_y, float eye_z, float f_x, float f_y, float f_z, float r_x, float r_y, float r_z, float u_x, float u_y, float u_z, float scale, float sky_yr_min, float sky_yr_max );


/**
 *  Kernel function definitions
 */

__global__ void gpu_raytracer_raytrace_kernel( float* out_r, float* out_g, float* out_b, int width, int height, float width_f, float height_f, float* sphere_cx, float* sphere_cy, float* sphere_cz, float* sphere_r, float* sphere_col_r, float* sphere_col_g, float* sphere_col_b, float* sphere_refl, int num_spheres, float eye_x, float eye_y, float eye_z, float f_x, float f_y, float f_z, float r_x, float r_y, float r_z, float u_x, float u_y, float u_z, float scale, float sky_yr_min, float sky_yr_max )
{
  {
    int ix = ((blockIdx.x * blockDim.x) + threadIdx.x);
    {
      int iy = ((blockIdx.y * blockDim.y) + threadIdx.y);
      if ((ix < width)) {
        if ((iy < height)) {
          {
            int pixel_idx = ((iy * width) + ix);
            {
              float inv_w = (1.0f / width_f);
              {
                float inv_h = (1.0f / height_f);
                {
                  float sx = ((2.0f * ((float( ix ) + 0.5f) * inv_w)) - 1.0f);
                  {
                    float sy = (1.0f - (2.0f * ((float( iy ) + 0.5f) * inv_h)));
                    {
                      float rx_dir = (f_x + (((r_x * sx) * scale) + ((u_x * sy) * scale)));
                      {
                        float ry_dir = (f_y + (((r_y * sx) * scale) + ((u_y * sy) * scale)));
                        {
                          float rz_dir = (f_z + (((r_z * sx) * scale) + ((u_z * sy) * scale)));
                          {
                            float dir_len = sqrtf( ((rx_dir * rx_dir) + ((ry_dir * ry_dir) + (rz_dir * rz_dir))) );
                            {
                              float inv_dir_len = (1.0f / dir_len);
                              {
                                float dx = (rx_dir * inv_dir_len);
                                {
                                  float dy = (ry_dir * inv_dir_len);
                                  {
                                    float dz = (rz_dir * inv_dir_len);
                                    {
                                      float ox = eye_x;
                                      {
                                        float oy = eye_y;
                                        {
                                          float oz = eye_z;
                                          {
                                            float accum_r = 0.0f;
                                            {
                                              float accum_g = 0.0f;
                                              {
                                                float accum_b = 0.0f;
                                                {
                                                  float throughput = 1.0f;
                                                  {
                                                    int active = 1;
                                                    for ( int depth = 0; ! (depth >= 3); depth = (depth + 1) )
                                                    {
                                                      if ((active == 1)) {
                                                        {
                                                          float hit_t = 10000000000.0f;
                                                          int hit_type = 0;
                                                          int hit_idx = -1;
                                                          for ( int i = 0; ! (i >= num_spheres); i = (i + 1) )
                                                          {
                                                            {
                                                              float cx = sphere_cx[i];
                                                              {
                                                                float cy = sphere_cy[i];
                                                                {
                                                                  float cz = sphere_cz[i];
                                                                  {
                                                                    float r = sphere_r[i];
                                                                    {
                                                                      float vx = (ox - cx);
                                                                      {
                                                                        float vy = (oy - cy);
                                                                        {
                                                                          float vz = (oz - cz);
                                                                          {
                                                                            float b_prime = (((vx * dx) + (vy * dy)) + (vz * dz));
                                                                            {
                                                                              float c_val = ((((vx * vx) + (vy * vy)) + (vz * vz)) - (r * r));
                                                                              {
                                                                                float disc = ((b_prime * b_prime) - c_val);
                                                                                if ((disc >= 0.0f)) {
                                                                                  {
                                                                                    float sqrt_disc = sqrtf( disc );
                                                                                    {
                                                                                      float t1 = (float_negate( b_prime ) - sqrt_disc);
                                                                                      {
                                                                                        float t2 = (float_negate( b_prime ) + sqrt_disc);
                                                                                        {
                                                                                          float t_val = ((t1 > 0.001f) ? t1 : ((t2 > 0.001f) ? t2 : 10000000000.0f));
                                                                                          if ((t_val < hit_t)) {
                                                                                            hit_t = t_val;
                                                                                            hit_type = 1;
                                                                                            hit_idx = i;
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          {
                                                            float den = float_negate( dy );
                                                            {
                                                              float abs_den = ((den > 0.0f) ? den : float_negate( den ));
                                                              if ((abs_den > 0.00000001f)) {
                                                                {
                                                                  float hit_t_plane = ((500.0f - oy) / den);
                                                                  if ((hit_t_plane > 0.001f)) {
                                                                    {
                                                                      float ix_p = (ox + (hit_t_plane * dx));
                                                                      {
                                                                        float iz_p = (oz + (hit_t_plane * dz));
                                                                        {
                                                                          float abs_ix_p = ((ix_p > 0.0f) ? ix_p : float_negate( ix_p ));
                                                                          {
                                                                            float iz_p_diff = (iz_p - -1400.0f);
                                                                            {
                                                                              float abs_iz_p_diff = ((iz_p_diff > 0.0f) ? iz_p_diff : float_negate( iz_p_diff ));
                                                                              if ((abs_ix_p <= 2500.0f)) {
                                                                                if ((abs_iz_p_diff <= 2500.0f)) {
                                                                                  if ((hit_t_plane < hit_t)) {
                                                                                    hit_t = hit_t_plane;
                                                                                    hit_type = 2;
                                                                                    hit_idx = -1;
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                          if ((hit_type == 0)) {
                                                            {
                                                              float sky_t = ((dy < sky_yr_min) ? 0.0f : ((dy > sky_yr_max) ? 1.0f : ((dy - sky_yr_min) / (sky_yr_max - sky_yr_min))));
                                                              {
                                                                float sky_t_pow = powf( sky_t, 0.3f );
                                                                {
                                                                  float sky_r = (((1.0f - sky_t_pow) * 1.0f) + (sky_t_pow * 0.2f));
                                                                  {
                                                                    float sky_g = (((1.0f - sky_t_pow) * 1.0f) + (sky_t_pow * 0.5f));
                                                                    {
                                                                      float sky_b = (((1.0f - sky_t_pow) * 1.0f) + (sky_t_pow * 1.0f));
                                                                      if ((depth == 0)) {
                                                                        accum_r = sky_r;
                                                                        accum_g = sky_g;
                                                                        accum_b = sky_b;
                                                                      } else {
                                                                        {
                                                                          float lum = (0.333f * ((sky_r + sky_g) + sky_b));
                                                                          accum_r = (accum_r + (throughput * lum));
                                                                          accum_g = (accum_g + (throughput * lum));
                                                                          accum_b = (accum_b + (throughput * lum));
                                                                        }
                                                                      }
                                                                      active = 0;
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          } else {
                                                            {
                                                              float hit_x = (ox + (hit_t * dx));
                                                              {
                                                                float hit_y = (oy + (hit_t * dy));
                                                                {
                                                                  float hit_z = (oz + (hit_t * dz));
                                                                  {
                                                                    float nx = 0.0f;
                                                                    {
                                                                      float ny = 0.0f;
                                                                      {
                                                                        float nz = 0.0f;
                                                                        {
                                                                          float col_r = 0.0f;
                                                                          {
                                                                            float col_g = 0.0f;
                                                                            {
                                                                              float col_b = 0.0f;
                                                                              {
                                                                                float refl_base = 0.0f;
                                                                                if ((hit_type == 1)) {
                                                                                  {
                                                                                    float cx = sphere_cx[hit_idx];
                                                                                    {
                                                                                      float cy = sphere_cy[hit_idx];
                                                                                      {
                                                                                        float cz = sphere_cz[hit_idx];
                                                                                        {
                                                                                          float r = sphere_r[hit_idx];
                                                                                          {
                                                                                            float inv_r = (1.0f / r);
                                                                                            nx = ((hit_x - cx) * inv_r);
                                                                                            ny = ((hit_y - cy) * inv_r);
                                                                                            nz = ((hit_z - cz) * inv_r);
                                                                                            col_r = sphere_col_r[hit_idx];
                                                                                            col_g = sphere_col_g[hit_idx];
                                                                                            col_b = sphere_col_b[hit_idx];
                                                                                            refl_base = sphere_refl[hit_idx];
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                } else {
                                                                                  nx = 0.0f;
                                                                                  ny = -1.0f;
                                                                                  nz = 0.0f;
                                                                                  {
                                                                                    float val = (sinf( ((3.1415927f / 140.0f) * hit_x) ) * sinf( ((3.1415927f / 140.0f) * (hit_z - -1400.0f)) ));
                                                                                    {
                                                                                      bool is_even = (val > 0.0f);
                                                                                      if (is_even) {
                                                                                        col_r = 0.9f;
                                                                                        col_g = 0.9f;
                                                                                        col_b = 0.9f;
                                                                                      } else {
                                                                                        col_r = 0.2f;
                                                                                        col_g = 0.2f;
                                                                                        col_b = 0.2f;
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                  refl_base = 0.05f;
                                                                                }
                                                                                {
                                                                                  float shadowed_sum = 0.0f;
                                                                                  for ( float s_idx_f = 0.0f; ! (s_idx_f >= 64.0f); s_idx_f = (s_idx_f + 1.0f) )
                                                                                  {
                                                                                    {
                                                                                      float golden_angle = 2.3999631f;
                                                                                      {
                                                                                        float r_v = (45.0f * sqrtf( ((s_idx_f + 0.5f) / 64.0f) ));
                                                                                        {
                                                                                          float theta = (s_idx_f * golden_angle);
                                                                                          {
                                                                                            float lp_x = (600.0f + (r_v * cosf( theta )));
                                                                                            {
                                                                                              float lp_y = 300.0f;
                                                                                              {
                                                                                                float lp_z = (200.0f + (r_v * sinf( theta )));
                                                                                                {
                                                                                                  float sh_dx = (lp_x - hit_x);
                                                                                                  {
                                                                                                    float sh_dy = (lp_y - hit_y);
                                                                                                    {
                                                                                                      float sh_dz = (lp_z - hit_z);
                                                                                                      {
                                                                                                        float sh_dist = sqrtf( ((sh_dx * sh_dx) + ((sh_dy * sh_dy) + (sh_dz * sh_dz))) );
                                                                                                        {
                                                                                                          float inv_sh_dist = (1.0f / sh_dist);
                                                                                                          {
                                                                                                            float sh_dir_x = (sh_dx * inv_sh_dist);
                                                                                                            {
                                                                                                              float sh_dir_y = (sh_dy * inv_sh_dist);
                                                                                                              {
                                                                                                                float sh_dir_z = (sh_dz * inv_sh_dist);
                                                                                                                {
                                                                                                                  float eps = (0.0005f * sh_dist);
                                                                                                                  {
                                                                                                                    float off_x = (hit_x + (nx * eps));
                                                                                                                    {
                                                                                                                      float off_y = (hit_y + (ny * eps));
                                                                                                                      {
                                                                                                                        float off_z = (hit_z + (nz * eps));
                                                                                                                        {
                                                                                                                          float dist_offset = sqrtf( (((lp_x - off_x) * (lp_x - off_x)) + (((lp_y - off_y) * (lp_y - off_y)) + ((lp_z - off_z) * (lp_z - off_z)))) );
                                                                                                                          {
                                                                                                                            int blocked = 0;
                                                                                                                            for ( int k = 0; ! (k >= num_spheres); k = (k + 1) )
                                                                                                                            {
                                                                                                                              if ((blocked == 0)) {
                                                                                                                                {
                                                                                                                                  int is_current_sphere = 0;
                                                                                                                                  if ((hit_type == 1)) {
                                                                                                                                    if ((hit_idx == k)) {
                                                                                                                                      is_current_sphere = 1;
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  if ((is_current_sphere == 0)) {
                                                                                                                                    {
                                                                                                                                      float cx = sphere_cx[k];
                                                                                                                                      {
                                                                                                                                        float cy = sphere_cy[k];
                                                                                                                                        {
                                                                                                                                          float cz = sphere_cz[k];
                                                                                                                                          {
                                                                                                                                            float r = sphere_r[k];
                                                                                                                                            {
                                                                                                                                              float vx = (off_x - cx);
                                                                                                                                              {
                                                                                                                                                float vy = (off_y - cy);
                                                                                                                                                {
                                                                                                                                                  float vz = (off_z - cz);
                                                                                                                                                  {
                                                                                                                                                    float b_prime = (((vx * sh_dir_x) + (vy * sh_dir_y)) + (vz * sh_dir_z));
                                                                                                                                                    {
                                                                                                                                                      float c_val = ((((vx * vx) + (vy * vy)) + (vz * vz)) - (r * r));
                                                                                                                                                      {
                                                                                                                                                        float disc = ((b_prime * b_prime) - c_val);
                                                                                                                                                        if ((disc >= 0.0f)) {
                                                                                                                                                          {
                                                                                                                                                            float sqrt_disc = sqrtf( disc );
                                                                                                                                                            {
                                                                                                                                                              float t1 = (float_negate( b_prime ) - sqrt_disc);
                                                                                                                                                              {
                                                                                                                                                                float t2 = (float_negate( b_prime ) + sqrt_disc);
                                                                                                                                                                {
                                                                                                                                                                  float t_val = ((t1 > 0.05f) ? t1 : ((t2 > 0.05f) ? t2 : 10000000000.0f));
                                                                                                                                                                  if ((t_val < dist_offset)) {
                                                                                                                                                                    blocked = 1;
                                                                                                                                                                  }
                                                                                                                                                                }
                                                                                                                                                              }
                                                                                                                                                            }
                                                                                                                                                          }
                                                                                                                                                        }
                                                                                                                                                      }
                                                                                                                                                    }
                                                                                                                                                  }
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                            if ((blocked == 0)) {
                                                                                                                              if ((hit_type == 1)) {
                                                                                                                                {
                                                                                                                                  float den_sh = float_negate( sh_dir_y );
                                                                                                                                  {
                                                                                                                                    float abs_den_sh = ((den_sh > 0.0f) ? den_sh : float_negate( den_sh ));
                                                                                                                                    if ((abs_den_sh > 0.00000001f)) {
                                                                                                                                      {
                                                                                                                                        float hit_t_sh = ((500.0f - off_y) / den_sh);
                                                                                                                                        if ((hit_t_sh > 0.05f)) {
                                                                                                                                          if ((hit_t_sh < dist_offset)) {
                                                                                                                                            {
                                                                                                                                              float ix_sh = (off_x + (hit_t_sh * sh_dir_x));
                                                                                                                                              {
                                                                                                                                                float iz_sh = (off_z + (hit_t_sh * sh_dir_z));
                                                                                                                                                {
                                                                                                                                                  float abs_ix_sh = ((ix_sh > 0.0f) ? ix_sh : float_negate( ix_sh ));
                                                                                                                                                  {
                                                                                                                                                    float iz_sh_diff = (iz_sh - -1400.0f);
                                                                                                                                                    {
                                                                                                                                                      float abs_iz_sh_diff = ((iz_sh_diff > 0.0f) ? iz_sh_diff : float_negate( iz_sh_diff ));
                                                                                                                                                      if ((abs_ix_sh <= 2500.0f)) {
                                                                                                                                                        if ((abs_iz_sh_diff <= 2500.0f)) {
                                                                                                                                                          blocked = 1;
                                                                                                                                                        }
                                                                                                                                                      }
                                                                                                                                                    }
                                                                                                                                                  }
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                            if ((blocked == 1)) {
                                                                                                                              shadowed_sum = (shadowed_sum + 0.02f);
                                                                                                                            } else {
                                                                                                                              shadowed_sum = (shadowed_sum + 1.0f);
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    {
                                                                                      float sf = (shadowed_sum / 64.0f);
                                                                                      {
                                                                                        float lc_x = (600.0f - hit_x);
                                                                                        {
                                                                                          float lc_y = (300.0f - hit_y);
                                                                                          {
                                                                                            float lc_z = (200.0f - hit_z);
                                                                                            {
                                                                                              float lc_dist = sqrtf( ((lc_x * lc_x) + ((lc_y * lc_y) + (lc_z * lc_z))) );
                                                                                              {
                                                                                                float inv_lc_dist = (1.0f / lc_dist);
                                                                                                {
                                                                                                  float lc_dir_x = (lc_x * inv_lc_dist);
                                                                                                  {
                                                                                                    float lc_dir_y = (lc_y * inv_lc_dist);
                                                                                                    {
                                                                                                      float lc_dir_z = (lc_z * inv_lc_dist);
                                                                                                      {
                                                                                                        float dot_lc_n = ((lc_dir_x * nx) + ((lc_dir_y * ny) + (lc_dir_z * nz)));
                                                                                                        {
                                                                                                          float parent_lambert = dot_lc_n;
                                                                                                          {
                                                                                                            float lambert = ((parent_lambert > 0.0f) ? parent_lambert : 0.0f);
                                                                                                            {
                                                                                                              float diff = (sf * lambert);
                                                                                                              {
                                                                                                                float dot_l_n = ((float_negate( lc_dir_x ) * nx) + ((float_negate( lc_dir_y ) * ny) + (float_negate( lc_dir_z ) * nz)));
                                                                                                                {
                                                                                                                  float rx_l = (float_negate( lc_dir_x ) - ((2.0f * dot_l_n) * nx));
                                                                                                                  {
                                                                                                                    float ry_l = (float_negate( lc_dir_y ) - ((2.0f * dot_l_n) * ny));
                                                                                                                    {
                                                                                                                      float rz_l = (float_negate( lc_dir_z ) - ((2.0f * dot_l_n) * nz));
                                                                                                                      {
                                                                                                                        float dot_r_v = ((rx_l * float_negate( dx )) + ((ry_l * float_negate( dy )) + (rz_l * float_negate( dz ))));
                                                                                                                        {
                                                                                                                          float vdot = ((dot_r_v > 0.0f) ? dot_r_v : 0.0f);
                                                                                                                          {
                                                                                                                            float spec_pow = powf( vdot, 8.0f );
                                                                                                                            {
                                                                                                                              float spec = (1.5f * (sf * spec_pow));
                                                                                                                              {
                                                                                                                                float base = (0.25f + ((0.7f * diff) + spec));
                                                                                                                                {
                                                                                                                                  float base_r = (col_r * base);
                                                                                                                                  {
                                                                                                                                    float base_g = (col_g * base);
                                                                                                                                    {
                                                                                                                                      float base_b = (col_b * base);
                                                                                                                                      {
                                                                                                                                        float dot_v_n = ((float_negate( dx ) * nx) + ((float_negate( dy ) * ny) + (float_negate( dz ) * nz)));
                                                                                                                                        {
                                                                                                                                          float vdot_refl = ((dot_v_n > 0.0f) ? dot_v_n : 0.0f);
                                                                                                                                          {
                                                                                                                                            float refl = (refl_base + ((1.0f - refl_base) * powf( (1.0f - vdot_refl), 5.0f )));
                                                                                                                                            if ((depth == 0)) {
                                                                                                                                              accum_r = base_r;
                                                                                                                                              accum_g = base_g;
                                                                                                                                              accum_b = base_b;
                                                                                                                                              throughput = refl;
                                                                                                                                            } else {
                                                                                                                                              {
                                                                                                                                                float lum = (0.333f * ((base_r + base_g) + base_b));
                                                                                                                                                accum_r = (accum_r + (throughput * lum));
                                                                                                                                                accum_g = (accum_g + (throughput * lum));
                                                                                                                                                accum_b = (accum_b + (throughput * lum));
                                                                                                                                                throughput = (throughput * refl);
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            {
                                                                                                                                              float dot_d_n = ((dx * nx) + ((dy * ny) + (dz * nz)));
                                                                                                                                              {
                                                                                                                                                float rx_dir_n = (dx - ((2.0f * dot_d_n) * nx));
                                                                                                                                                {
                                                                                                                                                  float ry_dir_n = (dy - ((2.0f * dot_d_n) * ny));
                                                                                                                                                  {
                                                                                                                                                    float rz_dir_n = (dz - ((2.0f * dot_d_n) * nz));
                                                                                                                                                    {
                                                                                                                                                      float norm_rn = sqrtf( ((rx_dir_n * rx_dir_n) + ((ry_dir_n * ry_dir_n) + (rz_dir_n * rz_dir_n))) );
                                                                                                                                                      {
                                                                                                                                                        float inv_norm_rn = (1.0f / norm_rn);
                                                                                                                                                        ox = (hit_x + (nx * 0.001f));
                                                                                                                                                        oy = (hit_y + (ny * 0.001f));
                                                                                                                                                        oz = (hit_z + (nz * 0.001f));
                                                                                                                                                        dx = (rx_dir_n * inv_norm_rn);
                                                                                                                                                        dy = (ry_dir_n * inv_norm_rn);
                                                                                                                                                        dz = (rz_dir_n * inv_norm_rn);
                                                                                                                                                      }
                                                                                                                                                    }
                                                                                                                                                  }
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            {
                                                                                                                                              int should_deactivate = 0;
                                                                                                                                              if ((throughput < 0.0001f)) {
                                                                                                                                                should_deactivate = 1;
                                                                                                                                              }
                                                                                                                                              if ((refl <= 0.0f)) {
                                                                                                                                                should_deactivate = 1;
                                                                                                                                              }
                                                                                                                                              if ((should_deactivate == 1)) {
                                                                                                                                                active = 0;
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                      out_r[pixel_idx] = accum_r;
                                                      out_g[pixel_idx] = accum_g;
                                                      out_b[pixel_idx] = accum_b;
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
