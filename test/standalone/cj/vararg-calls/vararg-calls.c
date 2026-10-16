#include <stdarg.h>
#include <stdint.h>

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