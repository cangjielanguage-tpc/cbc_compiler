#include <stdint.h>
#include <stddef.h>

void PlusOne(int32_t* ptr)
{
    if (ptr == NULL) {
        return;
    }
    *ptr += 1;
}
