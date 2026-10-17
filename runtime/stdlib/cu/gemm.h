#pragma once

// Row-major A[2][3], B[3][4], C[2][4]. C = alpha*A*B + beta*C.
// Host buffers must contain 6, 12, and 8 elements respectively.
// Return 0 on success, positive CUDA status, or negative cuBLAS status.
extern "C" int blink_cu_sgemm_2x3_3x4(const float* a, const float* b, float* c, float alpha, float beta);
extern "C" int blink_cu_dgemm_2x3_3x4(const double* a, const double* b, double* c, double alpha, double beta);
