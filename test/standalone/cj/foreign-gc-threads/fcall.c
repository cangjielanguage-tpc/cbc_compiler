#include <stdint.h>
#include <time.h>

int64_t sleepInC(int64_t n)
{
    // Stay inside the native call long enough for GC to hit while the C ABI adapter frame is on the stack.
    struct timespec ts = { 0, 200 * 1000 };
    nanosleep(&ts, NULL);
    return n;
}
