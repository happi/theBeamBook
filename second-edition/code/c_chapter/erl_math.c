/* Include from erl_math.c in a private OTP 29 source tree.
 * This example adds math:factorial/1; it is not an OTP API.
 */
#define BOOK_FACTORIAL_MAX 10000
#define BOOK_FACTORIAL_WORD_LIMIT 8192
#define BOOK_FACTORIAL_SLICE_WORK 4096

static Export book_factorial_continue_export;
void erts_init_book_factorial(void);

static BIF_RETTYPE book_factorial_continue(BIF_ALIST_3)
{
    Uint n = unsigned_val(BIF_ARG_1);
    Uint i = unsigned_val(BIF_ARG_2);
    Eterm acc = BIF_ARG_3;
    Eterm *src = NULL, *dst = NULL, *tmp, *hp;
    Eterm multiplier[2];
    Uint capacity = 8, need, words, work = 0;

    /* Only this BIF constructs the continuation arguments. Copy a boxed
     * accumulator before changing buffers; it currently belongs to the process.
     */
    if (is_big(acc)) {
        words = BIG_SIZE(big_val(acc)) + 1;
        while (capacity < words + 1) capacity *= 2;
        ASSERT(capacity <= BOOK_FACTORIAL_WORD_LIMIT);
        src = erts_alloc(ERTS_ALC_T_TMP, capacity * sizeof(Eterm));
        dst = erts_alloc(ERTS_ALC_T_TMP, capacity * sizeof(Eterm));
        sys_memcpy(src, big_val(acc), words * sizeof(Eterm));
        acc = make_big(src);
    }

    while (i <= n && work < BOOK_FACTORIAL_SLICE_WORK) {
        if (is_small(acc) && unsigned_val(acc) <= (Uint)MAX_SMALL / i) {
            acc = make_small(unsigned_val(acc) * i);
            ++work;
        } else {
            need = is_big(acc) ? BIG_SIZE(big_val(acc)) + 2 : 3;
            if (src == NULL) {
                src = erts_alloc(ERTS_ALC_T_TMP, capacity * sizeof(Eterm));
                dst = erts_alloc(ERTS_ALC_T_TMP, capacity * sizeof(Eterm));
            }
            if (need > capacity) {
                /* The input bound keeps this doubling and byte count bounded.
                 * src and dst always own the current allocations, including
                 * after a realloc or a swap. Rebuild the tagged alias too.
                 */
                capacity *= 2;
                ASSERT(capacity <= BOOK_FACTORIAL_WORD_LIMIT);
                src = erts_realloc(ERTS_ALC_T_TMP, src,
                                   capacity * sizeof(Eterm));
                dst = erts_realloc(ERTS_ALC_T_TMP, dst,
                                   capacity * sizeof(Eterm));
                acc = make_big(src);
            }
            work += need;
            if (is_small(acc))
                acc = small_times(signed_val(acc), (Sint)i, dst);
            else
                acc = big_times(acc, small_to_big((Sint)i, multiplier), dst);
            ASSERT(is_big(acc));
            tmp = src; src = dst; dst = tmp;
        }
        ++i;
    }

    /* No native buffer survives a return or trap. HAlloc does not run GC;
     * all term construction finishes before the continuation can be scheduled.
     */
    if (is_big(acc)) {
        words = BIG_SIZE(big_val(acc)) + 1;
        hp = HAlloc(BIF_P, words);
        sys_memcpy(hp, big_val(acc), words * sizeof(Eterm));
        acc = make_big(hp);
    }
    if (src != NULL) {
        erts_free(ERTS_ALC_T_TMP, src);
        erts_free(ERTS_ALC_T_TMP, dst);
    }
    if (i <= n) {
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP3(&book_factorial_continue_export, BIF_P,
                  make_small(n), make_small(i), acc);
    }
    BIF_RET2(acc, 1 + work / 32);
}

BIF_RETTYPE math_factorial_1(BIF_ALIST_1)
{
    Eterm args[3];
    if (!is_small(BIF_ARG_1) || signed_val(BIF_ARG_1) < 0 ||
        signed_val(BIF_ARG_1) > BOOK_FACTORIAL_MAX)
        BIF_ERROR(BIF_P, BADARG);
    args[0] = BIF_ARG_1;
    args[1] = make_small(2);
    args[2] = make_small(1);
    return book_factorial_continue(BIF_P, args, A__I);
}

void erts_init_book_factorial(void)
{
    erts_init_trap_export(&book_factorial_continue_export,
                         am_math, am_book_factorial_continue, 3,
                         &book_factorial_continue);
}
