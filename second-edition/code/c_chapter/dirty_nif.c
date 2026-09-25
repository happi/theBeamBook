#include "erl_nif.h"
#include <unistd.h>

static ERL_NIF_TERM heavy_work(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int seconds;
    if (!enif_get_int(env, argv[0], &seconds)) {
        return enif_make_badarg(env);
    }
    // Simulate a blocking operation that would freeze a standard scheduler
    sleep(seconds);
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    // ERL_NIF_DIRTY_JOB_CPU_BOUND registers this function to run on the dirty CPU pool
    {"heavy_work", 1, heavy_work, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(dirty_nif, nif_funcs, NULL, NULL, NULL, NULL);
