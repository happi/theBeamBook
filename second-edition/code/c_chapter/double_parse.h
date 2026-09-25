#ifndef DOUBLE_PARSE_H
#define DOUBLE_PARSE_H
#include <stddef.h>
#include <stdint.h>
#include <limits.h>

/* Strict decimal payload, no terminator required. Result must fit int32. */
static int double_parse(const char *data, size_t len, int32_t *result)
{
    size_t pos = 0;
    uint32_t value = 0;
    int negative = 0;
    uint32_t limit;
    if (len == 0 || len > 11) return 0;
    if (data[pos] == '-') { negative = 1; ++pos; }
    if (pos == len) return 0;
    limit = negative ? UINT32_C(1073741824) : UINT32_C(1073741823);
    for (; pos < len; ++pos) {
        unsigned digit = (unsigned char)data[pos] - (unsigned)'0';
        if (digit > 9 || value > (limit - digit) / 10) return 0;
        value = value * 10 + digit;
    }
    /* Widen before negation and doubling, then narrow the checked result. */
    *result = (int32_t)((negative ? -(int64_t)value : (int64_t)value) * 2);
    return 1;
}
#endif
