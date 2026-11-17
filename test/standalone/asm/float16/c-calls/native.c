#include <stdint.h>
#include <string.h>

typedef struct { _Float16 a, b, c; } Half3;
_Static_assert(sizeof(Half3) == 6, "three packed half values");

_Float16 half_identity(_Float16 x) { return x; }
Half3 half_rotate(Half3 x) { return (Half3) { x.c, x.a, x.b }; }

uint64_t half_check(Half3 x) {
    uint16_t bits[3];
    memcpy(bits, &x, sizeof(bits));
    return bits[0] == 0x7d55 && bits[1] == 0x8000 && bits[2] == 1;
}

_Float16 half_many(_Float16 a, _Float16 b, _Float16 c, _Float16 d,
                  _Float16 e, _Float16 f, _Float16 g, _Float16 h,
                  Half3 x, _Float16 i) {
    return (_Float16)(a + b + c + d + e + f + g + h + x.a + x.b + x.c + i);
}

uint64_t half_arch(void) {
#if defined(__aarch64__)
    return 1;
#else
    return 0;
#endif
}
