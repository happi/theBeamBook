/* Derived from port_driver.c
   https://www.erlang.org/doc/system/c_portdriver.html

*/

#include "erl_driver.h"
#include <stdio.h>
#include <unistd.h> // Include for sleep function

int foo(int x);
int bar(int y);

typedef struct
{
    ErlDrvPort port;
} example_data;

static ErlDrvData bp_drv_start(ErlDrvPort port, char *buff)
{
    example_data *d = (example_data *)driver_alloc(sizeof(example_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void bp_drv_stop(ErlDrvData handle)
{
    driver_free((char *)handle);
}

static void bp_drv_output(ErlDrvData handle, char *buff,
                          ErlDrvSizeT bufflen)
{
    example_data *d = (example_data *)handle;
    char fn = buff[0], arg = buff[1], id = buff[2];
    static char res[2];

    if (fn == 1)
    {
        res[0] = foo(arg);
    }
    else if (fn == 2)
    {
        res[0] = bar(arg);
        if (id > 14)
        {
            // Signal that the port is free
            set_busy_port(d->port, 0);
        }
        else
        {
            // Signal that the port is busy
            // This is not essential for this example
            // However, if multiple processes attempted to use the port
            // in parallel, we would need to signal that the port is busy
            // This would make even the foo function block.
            set_busy_port(d->port, 1);
            // Simulate processing delay
            sleep(1);
            set_busy_port(d->port, 0);
        }
    }
    res[1] = id;
    driver_output(d->port, res, 2);
}

ErlDrvEntry bp_driver_entry = {
    NULL,                           /* F_PTR init, called when driver is loaded */
    bp_drv_start,                   /* L_PTR start, called when port is opened */
    bp_drv_stop,                    /* F_PTR stop, called when port is closed */
    bp_drv_output,                  /* F_PTR output, called when erlang has sent */
    NULL,                           /* F_PTR ready_input, called when input descriptor ready */
    NULL,                           /* F_PTR ready_output, called when output descriptor ready */
    "busy_port_drv",                /* char *driver_name, the argument to open_port */
    NULL,                           /* F_PTR finish, called when unloaded */
    NULL,                           /* void *handle, Reserved by VM */
    NULL,                           /* F_PTR control, port_command callback */
    NULL,                           /* F_PTR timeout, reserved */
    NULL,                           /* F_PTR outputv, reserved */
    NULL,                           /* F_PTR ready_async, only for async drivers */
    NULL,                           /* F_PTR flush, called when port is about
                       to be closed, but there is data in driver
                       queue */
    NULL,                           /* F_PTR call, much like control, sync call
                       to driver */
    NULL,                           /* unused */
    ERL_DRV_EXTENDED_MARKER,        /* int extended marker, Should always be
                       set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be
                       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be
                       set to this value */
    0,                              /* int driver_flags, see documentation */
    NULL,                           /* void *handle2, reserved for VM use */
    NULL,                           /* F_PTR process_exit, called when a
                       monitored process dies */
    NULL                            /* F_PTR stop_select, called to close an
                       event object */
};

DRIVER_INIT(busy_port_drv) /* must match name in driver_entry */
{
    return &bp_driver_entry;
}