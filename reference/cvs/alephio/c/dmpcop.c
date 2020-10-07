#if defined(ALEPH_DEC) && defined(MACRO)
        .TITLE  DMPCOP
        .IDENT  /1.0/
        .PSECT  COPY
;-----------------------------------------------------------------------
;       SUBROUTINE DMPCOP(A,B,NCOL,NROW)
;C!     Copies NROW values from array A into B
;CKEY PRESS DMPCOP /INTERNAL
;       AUTHOR:   D.Harvatis   MAY 1989
;
;       A(1) is copied into B(1)
;       A(2) into B(1+NCOL)
;       ...................
;       A(n) into B(1+(n-1)*NCOL)
;-----------------------------------------------------------------------
#ifndef DOC
        .ENTRY  DMPCOP,^M<IV,R2,R3,R4>
        MOVL    B^4(AP), R3             ; R3 contains address of A array
        MOVL    B^8(AP), R4             ; R4 contains address of B array
        MOVL    @B^12(AP), R1           ; store NCOL in R1
        MOVL    @B^16(AP), R0           ; store NROW in R0
        CLRL    R2                      ; R2 = 0
LOOPC:  MOVL    (R3)+, (R4)[R2]         ; B(R2)=A(i) , i=i+1
        ADDL2   R1, R2                  ; R2=R2+NCOL
        DECL    R0                      ; decrease R0
        BNEQ    LOOPC                   ; if R0<>0 continue
        RET
        .END
#endif
#endif
#if defined(ALEPH_C)

/* DMPCOP(A,B,NCOL,NROW) */

#include "cfromf.h"

#define WORDLENGTH 32

FORT_CALL(dmpcop) (a,b,nc,nr)

int a[];
int b[];
int *nr;
int *nc;

{

   int ib;
   int ia;
   int nrow;

         ib = 0;
         ia = 0;
         nrow = *nr;
loopc:   b[ib] = a[ia++];
         ib = ib + *nc;
         nrow--;
         if (nrow == 0) return;
         goto loopc;

}
#endif
