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
int64_t fcBytes(
    int8_t a1, int8_t a2, int8_t a3, int8_t a4, int8_t a5, int8_t a6, int8_t a7, int8_t a8,
    int8_t a9, int8_t a10, int8_t a11, int8_t a12, int8_t a13, int8_t a14, int8_t a15,
    int8_t a16, int8_t a17, int8_t a18, int8_t a19, int8_t a20)
{
    return a1 + 2 * (int64_t)a2 + 3 * (int64_t)a3 + 4 * (int64_t)a4 + 5 * (int64_t)a5 +
           6 * (int64_t)a6 + 7 * (int64_t)a7 + 8 * (int64_t)a8 + 9 * (int64_t)a9 + 10 * (int64_t)a10 +
           11 * (int64_t)a11 + 12 * (int64_t)a12 + 13 * (int64_t)a13 + 14 * (int64_t)a14 +
           15 * (int64_t)a15 + 16 * (int64_t)a16 + 17 * (int64_t)a17 + 18 * (int64_t)a18 +
           19 * (int64_t)a19 + 20 * (int64_t)a20;
}

int64_t fcNarrowMix(
    uint8_t a1, int16_t a2, uint8_t a3, int32_t a4, int8_t a5, uint16_t a6, int64_t a7,
    uint8_t a8, int16_t a9, int32_t a10, int8_t a11, uint8_t a12, int16_t a13, int64_t a14,
    uint8_t a15, int8_t a16, int32_t a17, uint16_t a18, int8_t a19, uint8_t a20)
{
    return a1 + 2 * (int64_t)a2 + 3 * (int64_t)a3 + 4 * (int64_t)a4 + 5 * (int64_t)a5 +
           6 * (int64_t)a6 + 7 * a7 + 8 * (int64_t)a8 + 9 * (int64_t)a9 + 10 * (int64_t)a10 +
           11 * (int64_t)a11 + 12 * (int64_t)a12 + 13 * (int64_t)a13 + 14 * a14 +
           15 * (int64_t)a15 + 16 * (int64_t)a16 + 17 * (int64_t)a17 + 18 * (int64_t)a18 +
           19 * (int64_t)a19 + 20 * (int64_t)a20;
}

double fcFloats32(
    float a1, float a2, float a3, float a4, float a5, float a6, float a7, float a8,
    float a9, float a10, float a11, float a12)
{
    return a1 + 2.0 * a2 + 3.0 * a3 + 4.0 * a4 + 5.0 * a5 + 6.0 * a6 + 7.0 * a7 + 8.0 * a8 +
           9.0 * a9 + 10.0 * a10 + 11.0 * a11 + 12.0 * a12;
}

double fcMixStack(
    int16_t a1, float b1, int16_t a2, float b2, int16_t a3, float b3, int16_t a4, float b4,
    int16_t a5, float b5, int16_t a6, float b6, int16_t a7, float b7, int16_t a8, float b8,
    int16_t a9, float b9, int16_t a10, float b10, int16_t a11, float b11, int16_t a12, float b12)
{
    return 1.0 * a1 + 2.0 * b1 + 3.0 * a2 + 4.0 * b2 + 5.0 * a3 + 6.0 * b3 + 7.0 * a4 + 8.0 * b4 +
           9.0 * a5 + 10.0 * b5 + 11.0 * a6 + 12.0 * b6 + 13.0 * a7 + 14.0 * b7 + 15.0 * a8 +
           16.0 * b8 + 17.0 * a9 + 18.0 * b9 + 19.0 * a10 + 20.0 * b10 + 21.0 * a11 +
           22.0 * b11 + 23.0 * a12 + 24.0 * b12;
}
