# i_plus_ssjd
    mov rsi, qword ptr [rbx]
    mov rdx, qword ptr [rbx+8]
# are both operands small?
    mov eax, esi
    and eax, edx
    and al, 15
    cmp al, 15
    short jnz L14
    lea rax, qword ptr [rsi-15]
    add rax, rdx
    short jno L13
L14:
    call L15
L13:
    mov qword ptr [rbx+8], rax
# i_move_sd
    mov qword ptr [rbx+24], 687
# i_move_sd
    mov r10, qword ptr [rbx]
    mov qword ptr [rbx+16], r10
# i_call_only_f
    jmp collect/4
