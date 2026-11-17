#include <stdint.h>

uint64_t is_amd64(void) {
#if defined(__x86_64__) || defined(_M_X64)
    return 1;
#else
    return 0;
#endif
}
