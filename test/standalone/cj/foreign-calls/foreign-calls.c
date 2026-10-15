#include <stdint.h>

int64_t fcAdd2(int64_t a, int64_t b)
{
    return a + b;
}

int32_t fcSum6(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e, int32_t f)
{
    return a + b + c + d + e + f;
}

double fcFAdd2(double a, double b)
{
    return a + b;
}

double fcFSum8(double a, double b, double c, double d, double e, double f, double g, double h)
{
    return a + b + c + d + e + f + g + h;
}

int64_t st10(
    int64_t a1, int64_t a2, int64_t a3, int64_t a4, int64_t a5,
    int64_t a6, int64_t a7, int64_t a8, int64_t a9, int64_t a10)
{
    return a1 + 2 * a2 + 3 * a3 + 4 * a4 + 5 * a5 + 6 * a6 + 7 * a7 + 8 * a8 + 9 * a9 + 10 * a10;
}

double sf12(
    double a1, double a2, double a3, double a4, double a5, double a6,
    double a7, double a8, double a9, double a10, double a11, double a12)
{
    return a1 + 2 * a2 + 3 * a3 + 4 * a4 + 5 * a5 + 6 * a6 + 7 * a7 + 8 * a8 + 9 * a9 + 10 * a10 +
           11 * a11 + 12 * a12;
}

double stmix(
    int64_t x1, double d1, int64_t x2, double d2, int64_t x3, double d3, int64_t x4, double d4,
    int64_t x5, double d5, int64_t x6, double d6, int64_t x7, double d7, int64_t x8, double d8)
{
    return x1 + 2 * x2 + 3 * x3 + 4 * x4 + 5 * x5 + 6 * x6 + 7 * x7 + 8 * x8 +
           d1 + 2 * d2 + 3 * d3 + 4 * d4 + 5 * d5 + 6 * d6 + 7 * d7 + 8 * d8;
}