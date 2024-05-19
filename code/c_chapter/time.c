// system_time.c
#include <stdio.h>
#include <time.h>

void get_system_time()
{
    time_t rawtime;
    struct tm *timeinfo;

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    printf("Current Date and Time: %s", asctime(timeinfo));
}

int main()
{
    get_system_time();
    return 0;
}
