#include "erl_driver.h"
#include "double_parse.h"
#include <inttypes.h>
#include <stdio.h>

typedef struct { ErlDrvPort port; } double_data;

static ErlDrvData double_drv_start(ErlDrvPort port, char *command)
{
    double_data *d = driver_alloc(sizeof(*d));
    (void)command;
    if (d == NULL) return ERL_DRV_ERROR_GENERAL;
    d->port = port;
    return (ErlDrvData)d;
}

static void double_drv_stop(ErlDrvData handle)
{
    driver_free((void *)handle);
}

static void double_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT len)
{
    double_data *d = (double_data *)handle;
    int32_t result;
    char reply[32];
    int size;
    if (!double_parse(buff, len, &result)) {
        driver_output(d->port, "error", 5);
        return;
    }
    size = snprintf(reply, sizeof(reply), "%" PRId32, result);
    driver_output(d->port, reply, (ErlDrvSizeT)size);
}

static ErlDrvEntry double_driver_entry = {
    .start = double_drv_start,
    .stop = double_drv_stop,
    .output = double_drv_output,
    .driver_name = "double_drv",
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
    .driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING
};

DRIVER_INIT(double_drv) { return &double_driver_entry; }
