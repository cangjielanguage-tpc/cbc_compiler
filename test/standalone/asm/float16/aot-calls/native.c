#include <stdint.h>

uint64_t half_arch(void) {
#if defined(__aarch64__)
    return 1;
#else
    return 0;
#endif
}
