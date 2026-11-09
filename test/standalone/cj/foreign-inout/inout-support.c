#include <stdint.h>
#include <stddef.h>

void PlusOne(int32_t* ptr)
{
    if (ptr == NULL) {
        return;
    }
    *ptr += 1;
}

typedef struct {
    int8_t a;
    int16_t b;
    int32_t c;
    int64_t d;
} Data;

void PlusOneEachField(Data* data)
{
    if (data == NULL) {
        return;
    }
    data->a += 1;
    data->b += 1;
    data->c += 1;
    data->d += 1;
}

void PlusHalf(float* ptr)
{
    if (ptr == NULL) {
        return;
    }
    *ptr += 0.5f;
}
