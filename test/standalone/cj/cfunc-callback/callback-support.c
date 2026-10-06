#include <stdint.h>

int64_t applyCb(int64_t (*cb)(int64_t))
{
    return cb(10);
}