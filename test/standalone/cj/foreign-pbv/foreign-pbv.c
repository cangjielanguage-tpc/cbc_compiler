#include <stdint.h>

/* --- small structs (0/4/8/12/16/24 bytes) --- */

struct S0 {};
struct S4f1 { float a; };
struct S8i { int64_t a; };
struct S8i2 { int32_t a, b; };
struct S8f { double a; };
struct S8f2 { float a, b; };
struct S12f3 { float a, b, c; };
struct S16i { int64_t a, b; };
struct S16f { double a, b; };
struct S16m { int64_t a; double b; };
struct S16m2 { double a; int64_t b; };
struct S16f4 { float a, b, c, d; };
struct S24i { int64_t a, b, c; };
struct S24f { double a, b, c; };
struct S24m { int64_t a; double b, c; };

/* --- large structs --- */

struct S32 { int64_t a, b, c, d; };
struct S32f { double a, b, c, d; };
struct S48m { int64_t a; double b; int64_t c; double d; int64_t e; double f; };
struct S64 { int64_t a, b, c, d, e, f, g, h; };

/* --- structs in structs --- */

struct N16 { struct S8i p; struct S8f q; };
struct N16f { struct S8f2 p, q; };
struct N24 { struct S16i p; struct S8i q; };
struct N32f { struct S16f p, q; };
struct N40 { struct S24i p; struct S16i q; };

/* Float16 is not supported by the cbc toolchain yet.
struct S2f16 { _Float16 a; };
struct S4f16 { _Float16 a, b; };
struct S6f16 { _Float16 a, b, c; };
struct S8f16 { _Float16 a, b, c, d; };
*/

int64_t pbvS0(int64_t x, struct S0 s, int64_t y)
{
    return x + 2 * y;
}

double pbvS4f1(struct S4f1 s)
{
    return s.a;
}

int64_t pbvS8i(struct S8i s)
{
    return s.a;
}

int64_t pbvS8i2(struct S8i2 s)
{
    return s.a + 2 * (int64_t)s.b;
}

double pbvS8f(struct S8f s)
{
    return s.a;
}

double pbvS8f2(struct S8f2 s)
{
    return s.a + 2.0 * s.b;
}

double pbvS12f3(struct S12f3 s)
{
    return s.a + 2.0 * s.b + 3.0 * s.c;
}

int64_t pbvS16i(struct S16i s)
{
    return s.a + 2 * s.b;
}

double pbvS16f(struct S16f s)
{
    return s.a + 2.0 * s.b;
}

double pbvS16m(struct S16m s)
{
    return (double)s.a + 2.0 * s.b;
}

double pbvS16m2(struct S16m2 s)
{
    return s.a + 2.0 * (double)s.b;
}

double pbvS16f4(struct S16f4 s)
{
    return s.a + 2.0 * s.b + 3.0 * s.c + 4.0 * s.d;
}

int64_t pbvS24i(struct S24i s)
{
    return s.a + 2 * s.b + 3 * s.c;
}

double pbvS24f(struct S24f s)
{
    return s.a + 2.0 * s.b + 3.0 * s.c;
}

double pbvS24m(struct S24m s)
{
    return (double)s.a + 2.0 * s.b + 3.0 * s.c;
}

int64_t pbvS32(struct S32 s)
{
    return s.a + 2 * s.b + 3 * s.c + 4 * s.d;
}

double pbvS32f(struct S32f s)
{
    return s.a + 2.0 * s.b + 3.0 * s.c + 4.0 * s.d;
}

double pbvS48m(struct S48m s)
{
    return (double)s.a + 2.0 * s.b + 3.0 * (double)s.c + 4.0 * s.d + 5.0 * (double)s.e + 6.0 * s.f;
}

int64_t pbvS64(struct S64 s)
{
    return s.a + 2 * s.b + 3 * s.c + 4 * s.d + 5 * s.e + 6 * s.f + 7 * s.g + 8 * s.h;
}

double pbvN16(struct N16 n)
{
    return (double)n.p.a + 2.0 * n.q.a;
}

double pbvN16f(struct N16f n)
{
    return n.p.a + 2.0 * n.p.b + 3.0 * n.q.a + 4.0 * n.q.b;
}

int64_t pbvN24(struct N24 n)
{
    return n.p.a + 2 * n.p.b + 3 * n.q.a;
}

double pbvN32f(struct N32f n)
{
    return n.p.a + 2.0 * n.p.b + 3.0 * n.q.a + 4.0 * n.q.b;
}

int64_t pbvN40(struct N40 n)
{
    return n.p.a + 2 * n.p.b + 3 * n.p.c + 4 * n.q.a + 5 * n.q.b;
}

double pbvMixA(int64_t x, struct S16m s, double d, int64_t y)
{
    return (double)x + 10.0 * (double)s.a + 100.0 * s.b + 1000.0 * d + 10000.0 * (double)y;
}

int64_t pbvMixB(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, struct S8i s)
{
    return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * s.a;
}

double pbvMixC(double a, double b, double c, double d, double e, double f, double g, double h, struct S8f s)
{
    return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h + 9 * s.a;
}

int64_t pbvMixD(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, struct S16i s)
{
    return a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * s.a + 7 * s.b;
}

double pbvMixG(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e, int64_t f, struct S16m s)
{
    return (double)(a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f) + 7.0 * (double)s.a + 8.0 * s.b;
}

/* Float16 is not supported by the cbc toolchain yet.
double pbvS2f16(struct S2f16 s)
{
    return s.a;
}

double pbvS4f16(struct S4f16 s)
{
    return s.a + 2.0 * s.b;
}

double pbvS6f16(struct S6f16 s)
{
    return s.a + 2.0 * s.b + 3.0 * s.c;
}

double pbvS8f16(struct S8f16 s)
{
    return s.a + 2.0 * s.b + 3.0 * s.c + 4.0 * s.d;
}
*/
