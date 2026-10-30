#include <stdio.h>
#include <stdint.h>

struct CStruct {
    int8_t c;
    int32_t g[3];
};

void f(struct CStruct s)
{
    printf("%d %d %d\n", s.g[0], s.g[1], s.g[2]);
}

unsigned long size_of_CStruct()
{
    printf("CStruct size is %lu\n", sizeof(struct CStruct));
    return sizeof(struct CStruct);
}

long sumI32x3(int g[3])
{
    return g[0] + g[1] + g[2];
}

double sumF32x3(float g[3])
{
    return g[0] + g[1] + g[2];
}

void fillI32x3(int g[3])
{
    g[0] = 10;
    g[1] = 20;
    g[2] = 30;
}
