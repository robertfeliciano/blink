#include "../../runtime/stdlib/cu/gemm.h"

#include <cmath>
#include <cstdio>
#include <limits>

namespace {
    template <typename T, typename Gemm>
    bool test_gemm(const char* name, Gemm multiply) {
        const T a[6] = {1, 2, 3, 4, 5, 6};
        const T b[12] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};
        const T product[8] = {38, 44, 50, 56, 83, 98, 113, 128};
        const T alphas[3] = {T(1), T(-0.5), T(0)};
        const T betas[3] = {T(0), T(2), T(0.5)};
        for (int trial = 0; trial < 3; ++trial) {
            T c[8];
            for (int i = 0; i < 8; ++i)
                c[i] = trial == 0 ? std::numeric_limits<T>::quiet_NaN() : T(i - 3);
            const int status = multiply(a, b, c, alphas[trial], betas[trial]);
            if (status != 0) {
                std::fprintf(stderr, "%s failed: status %d\n", name, status);
                return false;
            }
            for (int i = 0; i < 8; ++i) {
                const T expected = alphas[trial] * product[i] + betas[trial] * T(i - 3);
                if (!std::isfinite(c[i]) || std::abs(c[i] - expected) > T(1e-5)) {
                    std::fprintf(stderr, "%s trial %d element %d mismatch\n", name, trial, i);
                    return false;
                }
            }
        }
        
        T aliased[12];
        for (int i = 0; i < 12; ++i)
            aliased[i] = b[i];
        if (multiply(a, aliased, aliased, T(1), T(0)) != 0)
            return false;
        for (int i = 0; i < 12; ++i) {
            if (aliased[i] != (i < 8 ? product[i] : b[i]))
                return false;
        }
        T c[8] = {};
        if (multiply(nullptr, b, c, T(1), T(0)) >= 0 || multiply(a, nullptr, c, T(1), T(0)) >= 0 ||
            multiply(a, b, nullptr, T(1), T(0)) >= 0)
            return false;
        for (T value : c) {
            if (value != T(0))
                return false;
        }
        return true;
    }
} // namespace

int main() {
    if (!test_gemm<float>("SGEMM", blink_cu_sgemm_2x3_3x4) || !test_gemm<double>("DGEMM", blink_cu_dgemm_2x3_3x4))
        return 1;
    std::puts("cuBLAS SGEMM/DGEMM tests passed");
    return 0;
}
