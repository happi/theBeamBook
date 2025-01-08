#include "erl_driver.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct
{
    ErlDrvPort port;
} double_data;

// Start function: initialize driver state
static ErlDrvData double_drv_start(ErlDrvPort port, char *command)
{
    double_data *d = (double_data *)driver_alloc(sizeof(double_data));
    d->port = port;
    return (ErlDrvData)d;
}

// Stop function: clean up resources
static void double_drv_stop(ErlDrvData handle)
{
    driver_free((char *)handle);
}

// Output function: process data sent from Erlang
static void double_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen)
{
    double_data *d = (double_data *)handle;

    // Convert input buffer to an integer
    int input = atoi(buff);

    // Perform the operation (double the input value)
    int result = input * 2;

    // Convert the result back to a string
    char result_str[32];
    snprintf(result_str, sizeof(result_str), "%d", result);

    // Send the result back to Erlang
    driver_output(d->port, result_str, strlen(result_str));
}

// Define the driver entry struct
static ErlDrvEntry double_driver_entry = {
    NULL,                           // init
    double_drv_start,               // start
    double_drv_stop,                // stop
    double_drv_output,              // output
    NULL,                           // ready_input
    NULL,                           // ready_output
    "double_drv",                   // driver_name
    NULL,                           // finish
    NULL,                           // handle
    NULL,                           // control
    NULL,                           // timeout
    NULL,                           // outputv
    NULL,                           // ready_async
    NULL,                           // flush
    NULL,                           // call
    NULL,                           // event
    ERL_DRV_EXTENDED_MARKER,        // extended marker
    ERL_DRV_EXTENDED_MAJOR_VERSION, // major version
    ERL_DRV_EXTENDED_MINOR_VERSION, // minor version
    0,                              // driver flags
    NULL,                           // handle2
    NULL,                           // process_exit
    NULL                            // stop_select
};

// Driver initialization macro
DRIVER_INIT(double_drv)
{
    return &double_driver_entry;
}
