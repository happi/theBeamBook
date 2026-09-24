; Interpreter (emu flavor) handlers for the body of dispatch:run/2.
;
; Captured from an OTP 29.0 x86-64 source build with `make FLAVOR=emu`,
; which produces bin/x86_64-pc-linux-gnu/beam.emu next to beam.smp.
; The handlers are labels inside process_main, and the build keeps debug
; information, so gdb finds them by name and interleaves the generated C
; (x86_64-pc-linux-gnu/opt/emu/beam_hot.h) with the machine code:
;
;   gdb -batch -ex 'set disassembly-flavor intel' \
;       -ex 'info line process_main:lb_move_cx' beam.emu
;   gdb -batch -ex 'set disassembly-flavor intel' \
;       -ex 'disassemble /s 0x229c16,0x229c30' beam.emu
;
; Register use inside process_main in this build (it differs from the
; JIT, where rbx is the X register base):
;   rbx  I, the pointer into the threaded code; I[0] is the handler address
;   r12  base of the X register array, so [r12+off] is an X register
;   r13  E, the stack pointer, so [r13+off] is a Y register
;   r14  FCALLS, the reduction budget
;   rbp  scratch; each handler loads the next handler's address into it
;
; The loaded code, from erts_debug:df(dispatch) under `erl -emu_flavor emu`:
;
;   i_plus_xxjd x(0) x(1) j(0) x(1)
;   move_cx `42` x(3)
;   move_rx r(0) x(2)
;   i_call_only_f loc(`dispatch`:`collect`/4)
;
; Addresses and local jump targets depend on the build.

; tag::i_plus_xxjd[]
; OpCase(i_plus_xxjd): fetch both operands, then share plus__execute
    endbr64
    mov    rax, QWORD PTR [rbx+0x8]    ; I[1]: both X offsets, packed
    mov    rbp, rbx                    ; remember I
    movzx  ecx, ax                     ; low 16 bits: offset of x(0)
    shr    rax, 0x10                   ; high bits: offset of x(1)
    mov    r15, QWORD PTR [r12+rcx*1]  ; PlusOp1 = x(0)
    mov    rdx, QWORD PTR [r12+rax*1]  ; PlusOp2 = x(1)
    jmp    0x227318                    ; goto plus__execute

; plus__execute, shared by every i_plus_* handler
    mov    rcx, QWORD PTR [rbp+0x10]   ; I[2]: fail label and dst, packed
    mov    rax, rcx
    shr    rax, 0x20                   ; dst
    lea    rbx, [rax+r13*1-0x1]        ; REG_TARGET_PTR, Y register case
    test   al, 0x1
    jne    0x227330
    lea    rbx, [r12+rax*1]            ; it is an X register
    mov    rax, r15                    ; is_both_small(PlusOp1, PlusOp2)
    and    rax, rdx
    not    rax
    test   al, 0xf
    jne    0x22f94d                    ; not both small: erts_mixed_plus
    mov    rax, rdx                    ; rhs_untagged = PlusOp2 & ~tag
    and    rax, 0xfffffffffffffff0
    add    rax, r15                    ; + lhs_tagged
    jo     0x22f94d                    ; overflow: erts_mixed_plus
    mov    QWORD PTR [rbx], rax        ; *dst_ptr = res
    lea    rbx, [rbp+0x18]             ; SET_I(I+3)
    jmp    QWORD PTR [rbp+0x18]        ; Goto(*I)
; end::i_plus_xxjd[]

; tag::move_cx[]
; OpCase(move_cx): move a constant into an X register
    endbr64
    mov    rbp, QWORD PTR [rbx+0x18]   ; I[3]: the next handler
    mov    rdx, QWORD PTR [rbx+0x8]    ; I[1]: the constant (687 here)
    add    rbx, 0x18                   ; I += 3
    mov    rax, QWORD PTR [rbx-0x8]    ; I[2]: X register offset (24)
    mov    QWORD PTR [r12+rax*1], rdx  ; xb(I[2]) = I[1]
    jmp    rbp                         ; GotoPF(next_pf)
; end::move_cx[]

; tag::move_rx[]
; OpCase(move_rx): move x(0) into another X register
    endbr64
    mov    rbp, QWORD PTR [rbx+0x10]   ; I[2]: the next handler
    mov    rdx, QWORD PTR [rbx+0x8]    ; I[1]: X register offset (16)
    add    rbx, 0x10                   ; I += 2
    mov    rcx, QWORD PTR [r12]        ; x(0)
    mov    QWORD PTR [r12+rdx*1], rcx  ; xb(I[1]) = x(0)
    jmp    rbp                         ; GotoPF(next_pf)
; end::move_rx[]

; tag::i_call_only_f[]
; OpCase(i_call_only_f): tail call, one reduction
    endbr64
    mov    rax, QWORD PTR [rbx+0x8]    ; I[1]: offset to collect/4
    lea    rdx, [rbx+rax*8]            ; I += I[1]
    mov    rax, r14
    mov    rbp, QWORD PTR [rdx]        ; dis_next = *I
    mov    rbx, rdx
    test   r14, r14                    ; FCALLS > 0 ?
    jle    0x230a40                    ; no: context_switch
    lea    r14, [rax-0x1]              ; FCALLS--
    jmp    rbp                         ; Goto(dis_next)
; end::i_call_only_f[]
