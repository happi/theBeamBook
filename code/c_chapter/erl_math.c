BIF_RETTYPE math_factorial_1(BIF_ALIST_1)
{
    /* Calculate factorial = n! for n >= 0 */
    Sint64 i, n, reds;
    Uint64 factorial = 1;
    Eterm result;
    Eterm *hp;
    Eterm big_factorial;
    ErtsDigit *temp_digits_a;
    ErtsDigit *temp_digits_b;
    ErtsDigit *src;
    ErtsDigit *dest;
    ErtsDigit *temp;
    dsize_t temp_size;
    dsize_t curr_size;
    dsize_t new_size;
    int ops_since_yield = 0;

    if (is_not_integer(BIF_ARG_1) || (!term_to_Sint64(BIF_ARG_1, &n)) || n < 0)
    {
        BIF_ERROR(BIF_P, BADARG);
    }

    /* Initial allocation */
    temp_size = 2 * (n + 1);
    temp_digits_a = (ErtsDigit *)erts_alloc(ERTS_ALC_T_TMP, temp_size * sizeof(ErtsDigit));
    temp_digits_b = (ErtsDigit *)erts_alloc(ERTS_ALC_T_TMP, temp_size * sizeof(ErtsDigit));
    src = temp_digits_a;
    dest = temp_digits_b;

    for (i = 1; i <= n; ++i)
    {
        if (!IS_USMALL(0, factorial * i))
        {
            // Initial conversion to bignum in our temp buffer
            hp = (Eterm *)src;
            big_factorial = uint_to_big(factorial, hp);
            curr_size = BIG_SIZE(big_val(big_factorial));

            for (; i <= n; ++i)
            {
                if (++ops_since_yield >= 1000)
                {
                    YCF_YIELD();
                    ops_since_yield = 0;
                }
                new_size = curr_size + 1;

                if (new_size > temp_size)
                {
                    dsize_t alloc_size = new_size * 2;
                    src = (ErtsDigit *)erts_realloc(ERTS_ALC_T_TMP, src, alloc_size * sizeof(ErtsDigit));
                    dest = (ErtsDigit *)erts_realloc(ERTS_ALC_T_TMP, dest, alloc_size * sizeof(ErtsDigit));
                    temp_size = alloc_size;
                }

                big_factorial = big_times_small(big_factorial, i, (Eterm *)dest);

                temp = src;
                src = dest;
                dest = temp;
            }

            // Only now allocate on process heap and copy the final result
            hp = HAlloc(BIF_P, BIG_SIZE(big_val(big_factorial)) + 1);
            sys_memcpy(hp, big_val(big_factorial), (BIG_SIZE(big_val(big_factorial)) + 1) * sizeof(Eterm));
            result = make_big(hp);

            erts_free(ERTS_ALC_T_TMP, temp_digits_a);
            erts_free(ERTS_ALC_T_TMP, temp_digits_b);

            goto done;
        }
        factorial *= i;
    }

    result = make_small(factorial);

done:
    reds = n / 1000 + 1;
    BIF_RET2(result, reds);
}
