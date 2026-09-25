#include "erl_nif.h"
#include <limits.h>

static ERL_NIF_TERM nif_add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int a, b;
    if (argc != 2 || !enif_get_int(env, argv[0], &a) || !enif_get_int(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }
    if ((b > 0 && a > INT_MAX - b) ||
        (b < 0 && a < INT_MIN - b)) {
        return enif_make_badarg(env);
    }
    return enif_make_int(env, a + b);
}

static ErlNifFunc nif_funcs[] = {
    {"add", 2, nif_add, 0}
};

ERL_NIF_INIT(my_nif, nif_funcs, NULL, NULL, NULL, NULL);