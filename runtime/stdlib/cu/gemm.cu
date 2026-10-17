#include "gemm.h"

#include <cublas_v2.h>
#include <cuda_runtime.h>

namespace {
    // All multiplication is delegated to cuBLAS. Template dimensions keep native
    // buffer sizes consistent with the fixed-size Blink signatures.
    template <typename T, int M, int N, int K, typename Gemm>
    int gemm(const T* a, const T* b, T* c, T alpha, T beta, Gemm multiply) {
        if (!a || !b || !c)
            return -static_cast<int>(CUBLAS_STATUS_INVALID_VALUE);

        cublasHandle_t handle = nullptr;
        cublasStatus_t blas = cublasCreate(&handle);
        if (blas != CUBLAS_STATUS_SUCCESS)
            return -static_cast<int>(blas);

        constexpr size_t a_count = M * K;
        constexpr size_t b_count = K * N;
        constexpr size_t c_count = M * N;
        T* device = nullptr;
        cudaError_t status = cudaMalloc(reinterpret_cast<void**>(&device), (a_count + b_count + c_count) * sizeof(T));
        if (status == cudaSuccess) {
            T* da = device;
            T* db = device + a_count;
            T* dc = db + b_count;
            status = cudaMemcpy(da, a, a_count * sizeof(T), cudaMemcpyHostToDevice);
            if (status == cudaSuccess)
                status = cudaMemcpy(db, b, b_count * sizeof(T), cudaMemcpyHostToDevice);
            // With beta=0 cuBLAS does not read C, so skip uploading it.
            if (status == cudaSuccess && beta != T(0))
                status = cudaMemcpy(dc, c, c_count * sizeof(T), cudaMemcpyHostToDevice);
            if (status == cudaSuccess) {
                // cuBLAS is column-major: compute C^T = B^T*A^T by swapping
                // operands and M/N. No transpose kernel or host repacking.
                blas = multiply(handle, CUBLAS_OP_N, CUBLAS_OP_N, N, M, K, &alpha, db, N, da, K, &beta, dc, N);
                if (blas == CUBLAS_STATUS_SUCCESS)
                    // Blocking D2H copy waits for GEMM on the default stream.
                    status = cudaMemcpy(c, dc, c_count * sizeof(T), cudaMemcpyDeviceToHost);
            }
        }

        int result = status != cudaSuccess ? static_cast<int>(status) : -static_cast<int>(blas);
        if (device) {
            status = cudaFree(device);
            if (result == 0)
                result = static_cast<int>(status);
        }
        blas = cublasDestroy(handle);
        return result == 0 ? -static_cast<int>(blas) : result;
    }
} // namespace

extern "C" int blink_cu_sgemm_2x3_3x4(const float* a, const float* b, float* c, float alpha, float beta) {
    return gemm<float, 2, 4, 3>(a, b, c, alpha, beta, cublasSgemm);
}

extern "C" int blink_cu_dgemm_2x3_3x4(const double* a, const double* b, double* c, double alpha, double beta) {
    return gemm<double, 2, 4, 3>(a, b, c, alpha, beta, cublasDgemm);
}
