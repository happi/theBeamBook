# is_nonempty_list_fS
    test byte ptr [rbx], 2
    jnz label_3

; ... omitted emitted instructions ...

# get_tl_Sd
    mov rdi, qword ptr [rbx]
    mov rsi, qword ptr [rdi+7]
    mov qword ptr [rbx], rsi
# i_call_f
.db 0x90
    call len/1

; ... omitted emitted instructions ...

label_3:
# is_nil_fS
    cmp byte ptr [rbx], 59
    jnz label_1
# i_move_sd
    mov qword ptr [rbx], 15
# return
    dec r14d
    jl L16
    ret
