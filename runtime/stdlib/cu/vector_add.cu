#include <cuda_runtime.h>

namespace {
    constexpr int count = 4;
    constexpr size_t bytes = count * sizeof(double);

    __global__ void add_arrays(const double* left, const double* right, double* result) {
        const int i = blockIdx.x * blockDim.x + threadIdx.x;
        if (i < count)
            result[i] = left[i] + right[i];
    }
} // namespace

// Host pointers refer to four contiguous doubles, matching Blink's [f64; 4].
// Return a CUDA status; only write result after the kernel succeeds.
extern "C" int blink_cu_add_f64_4(const double* left, const double* right, double* result) {
    if (!left || !right || !result)
        return static_cast<int>(cudaErrorInvalidValue);

    // A single allocation holds all three device arrays.
    double* device = nullptr;
    cudaError_t status = cudaMalloc(reinterpret_cast<void**>(&device), 3 * bytes);
    if (status != cudaSuccess)
        return static_cast<int>(status);

    status = cudaMemcpy(device, left, bytes, cudaMemcpyHostToDevice);
    if (status == cudaSuccess)
        status = cudaMemcpy(device + count, right, bytes, cudaMemcpyHostToDevice);
    if (status == cudaSuccess) {
        add_arrays<<<1, 32>>>(device, device + count, device + 2 * count);
        status = cudaGetLastError();
    }
    if (status == cudaSuccess)
        status = cudaDeviceSynchronize();
    if (status == cudaSuccess)
        status = cudaMemcpy(result, device + 2 * count, bytes, cudaMemcpyDeviceToHost);

    const cudaError_t cleanup = cudaFree(device);
    return static_cast<int>(status == cudaSuccess ? cleanup : status);
}
