#include <stdarg.h>
#include <stdint.h>

struct LargeArg {
    int64_t a, b, c;
};

int32_t vlarge(int32_t tag, ...)
{
    va_list ap;
    va_start(ap, tag);
    struct LargeArg value = va_arg(ap, struct LargeArg);
    int64_t marker = va_arg(ap, int64_t);
    va_end(ap);
    return tag == 17 && value.a == 11 && value.b == -22 && value.c == 33 && marker == 123456789;
}

int32_t vlarges(int32_t tag, ...)
{
    va_list ap;
    va_start(ap, tag);
    struct LargeArg first = va_arg(ap, struct LargeArg);
    struct LargeArg second = va_arg(ap, struct LargeArg);
    int64_t marker = va_arg(ap, int64_t);
    va_end(ap);
    return tag == 17 && first.a == 11 && first.b == -22 && first.c == 33 &&
           second.a == -44 && second.b == 55 && second.c == -66 && marker == 123456789;
}

int32_t varray(int32_t tag, ...)
{
    va_list ap;
    va_start(ap, tag);
    int32_t *values = va_arg(ap, int32_t *);
    int64_t marker = va_arg(ap, int64_t);
    va_end(ap);
    return tag == 17 && marker == 123456789 && values != 0 &&
           values[0] == 111 && values[1] == -222 && values[2] == 333;
}

double vsum(int32_t n, ...)
{
    va_list ap;
    va_start(ap, n);
    double s = 0.0;
    for (int32_t i = 0; i < n; i++) {
        s += va_arg(ap, double);
    }
    va_end(ap);
    return s;
}

int64_t vmix(double first, int32_t k, ...)
{
    va_list ap;
    va_start(ap, k);
    int64_t s = (int64_t)first;
    for (int32_t i = 0; i < k; i++) {
        s += va_arg(ap, int32_t);
        s += (int64_t)va_arg(ap, double);
    }
    va_end(ap);
    return s;
}

int32_t vints6(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e, ...)
{
    va_list ap;
    va_start(ap, e);
    int32_t s = a + b + c + d + e;
    s += va_arg(ap, int32_t);
    s += va_arg(ap, int32_t);
    va_end(ap);
    return s;
}
