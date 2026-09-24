/* A delayed reply uses a driver timer; output never sleeps. */
#include "erl_driver.h"

int foo(int x);
int bar(int y);

typedef struct {
    ErlDrvPort port;
    char reply[2];
} example_data;

static ErlDrvData bp_drv_start(ErlDrvPort port, char *command)
{
    example_data *d = driver_alloc(sizeof(*d));
    (void)command;
    if (d == NULL) return ERL_DRV_ERROR_GENERAL;
    d->port = port;
    return (ErlDrvData)d;
}

static void bp_drv_stop(ErlDrvData handle)
{
    example_data *d = (example_data *)handle;
    driver_cancel_timer(d->port);
    driver_free(d);
}

static void bp_drv_timeout(ErlDrvData handle)
{
    example_data *d = (example_data *)handle;
    driver_output(d->port, d->reply, 2);
    set_busy_port(d->port, 0);
}

static void bp_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT len)
{
    example_data *d = (example_data *)handle;
    unsigned char fn, arg, id;
    if (len != 3) {
        driver_failure_atom(d->port, "badarg");
        return;
    }
    fn = (unsigned char)buff[0];
    arg = (unsigned char)buff[1];
    id = (unsigned char)buff[2];
    if (!((fn == 1 && arg < 255) || (fn == 2 && arg < 128))) {
        driver_failure_atom(d->port, "badarg");
        return;
    }
    d->reply[0] = (char)(fn == 1 ? foo(arg) : bar(arg));
    d->reply[1] = (char)id;
    if (fn == 2 && id < 14) {
        set_busy_port(d->port, 1);
        if (driver_set_timer(d->port, 1000) < 0)
            driver_failure_atom(d->port, "timer_failed");
        /* Return now; the timer callback sends the result later. */
    } else {
        driver_output(d->port, d->reply, 2);
    }
}

static ErlDrvEntry bp_driver_entry = {
    .start = bp_drv_start,
    .stop = bp_drv_stop,
    .output = bp_drv_output,
    .driver_name = "busy_port_drv",
    .timeout = bp_drv_timeout,
    .extended_marker = ERL_DRV_EXTENDED_MARKER,
    .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
    .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
    .driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING
};

DRIVER_INIT(busy_port_drv) { return &bp_driver_entry; }
