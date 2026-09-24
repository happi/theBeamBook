#include "../double_parse.h"
#include <assert.h>
#include <string.h>

int main(void)
{
    int32_t value;
    const char raw[] = {'1', '2'}; /* Deliberately no terminator. */
    const char *invalid[] = {"", "-", "+1", " 1", "1x", "1073741824",
                             "-1073741825", "2147483647", "-2147483648",
                             "999999999999999999999999"};
    assert(double_parse(raw, sizeof(raw), &value) && value == 24);
    assert(double_parse("1073741823", 10, &value) && value == 2147483646);
    assert(double_parse("-1073741824", 11, &value) && value == INT32_MIN);
    assert(double_parse("0", 1, &value) && value == 0);
    assert(!double_parse("1\0x", 3, &value));
    for (size_t i = 0; i < sizeof(invalid)/sizeof(invalid[0]); ++i)
        assert(!double_parse(invalid[i], strlen(invalid[i]), &value));
    return 0;
}
