#include <stdint.h>
#include <time.h>

int64_t sleepInC(int64_t n)
{
    struct timespec ts = { 0, 200 * 1000 };
    nanosleep(&ts, NULL);
    return n;
}
