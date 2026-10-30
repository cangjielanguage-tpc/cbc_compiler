#include <stdint.h>

struct S4 {
    int32_t a;
};

struct S8 {
    int64_t a;
};

struct S16 {
    int64_t a;
    int64_t b;
};

struct Inner {
    int64_t x;
    int64_t y;
};

struct Complex {
    int8_t c;
    int32_t d;
    int64_t big;
    struct Inner inner;
    int g[3];
};

struct S4 retS4(void)
{
    struct S4 r;
    r.a = 111;
    return r;
}

struct S8 retS8(void)
{
    struct S8 r;
    r.a = 2222222222LL;
    return r;
}

struct S16 retS16(void)
{
    struct S16 r;
    r.a = 333;
    r.b = 444;
    return r;
}

struct Complex retComplex(void)
{
    struct Complex r;
    r.c = 1;
    r.d = 2;
    r.big = 3;
    r.inner.x = 4;
    r.inner.y = 5;
    r.g[0] = 6;
    r.g[1] = 7;
    r.g[2] = 8;
    return r;
}

struct F2 {
    double a;
    double b;
};

struct F3f {
    float a;
    float b;
    float c;
};

struct F4f {
    float a;
    float b;
    float c;
    float d;
};

struct Mix {
    double a;
    int64_t b;
};

struct F3d {
    double a;
    double b;
    double c;
};

struct F2 retF2(void)
{
    struct F2 r;
    r.a = 1.5;
    r.b = -2.5;
    return r;
}

struct F3f retF3f(float a, float b, float c)
{
    struct F3f r;
    r.a = a;
    r.b = b;
    r.c = c;
    return r;
}

struct F4f retF4f(float a, float b, float c, float d)
{
    struct F4f r;
    r.a = a;
    r.b = b;
    r.c = c;
    r.d = d;
    return r;
}

struct Mix retMix(int64_t x, double f)
{
    struct Mix r;
    r.a = f;
    r.b = x;
    return r;
}

struct F3d retF3d(int64_t x, int64_t y, int64_t z)
{
    struct F3d r;
    r.a = x + 0.5;
    r.b = y + 0.5;
    r.c = z + 0.5;
    return r;
}
