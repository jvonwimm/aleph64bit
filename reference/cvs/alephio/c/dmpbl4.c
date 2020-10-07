#if defined(ALEPH_DEC) && defined(MACRO)
;-----------------------------------------------------------------------
;       SUBROUTINE DMPBL4(A,B,NROW,NBITS,NCOL,ILOW)
;C!     Decompress array A, convert, and store into array B
;CKEY PRESS DMPBL4 /INTERNAL
;       AUTHOR :    D.Harvatis    MAY 1989
;
;       INPUT :     A : Integer array that contains a compressed column.
;                NROW : Number of compressed values contained in the A
;                       array. Normaly it is the number of rows in the
;                       original BOS bank.
;                   L : number of bits for each number in the compressed
;                       array.
;                  NC : number of columns of the original BOS bank.
;                ILOW : conversion parameter.The conversion is
;                       done using the following formula :
;                           i = i + ILOW
;
;       OUTPUT:     B : Integer array. Decompressed and converted values
;                       are stored into this array by row (as in BOS
;                       arrays). That means that the first value
;                       extracted from the A array is stored in B(1),
;                       the second in B(NC+1), the Nth in B((N-1)*NC+1).
;-----------------------------------------------------------------------
#ifndef DOC
        .TITLE  DMPBL4
        .IDENT  /1.0/
        .PSECT  BLOW4
S16B:   ASHL    #-1, R0, R5             ; R5=R0/2
        BEQL    LL16B
L16B:   MOVL    (R3)+, R7               ; R7=A(IA)
        EXTZV   #16, R8, R7, R9         ; extract upper bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        MOVZWL  R7, R9                  ; extract lower bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        SOBGTR  R5, L16B                ; decrease R5, if R5>0 continue
LL16B:  BICL2   #-2, R0                 ; R0=R0.AND.1
        BEQL    RB6                     ; if R0=0 return
        MOVL    (R3)+, R7
        EXTZV   #16, R8, R7, R9         ; extract last bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
RB6:    RET
;
S8B:    ASHL    #-2, R0, R5             ; R5=R0/4
        BEQL    LL8B                    ; if R5=0 go to LL8
L8B:    MOVL    (R3)+, R7               ; R7=A(IA)
        EXTZV   #24, R8, R7, R9         ; extract upper bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        EXTZV   #16, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        EXTZV   #8, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        MOVZBL  R7, R9                  ; extract lower bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        SOBGTR  R5, L8B                 ; decrease R5, if R5>0 continue
LL8B:   BICL2   #-4, R0                 ; R0=R0.AND.3
        BEQL    RTSB                    ; if R0=0 return
        MOVL    (R3)+, R7               ; R7=A(IA)
        EXTZV   #24, R8, R7, R9         ; extract upper bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        DECL    R0                      ; decrease R0
        BEQL    RTSB                    ; if R0=0 return
        EXTZV   #16, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        DECL    R0                      ; decrease R0
        BEQL    RTSB                    ; if R0=0 return
        EXTZV   #8, R8, R7, R9          ; extract last bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
RTSB:   RET
;
        .ENTRY  DMPBL4,^M<IV,R2,R3,R4,R5,R6,R7,R8,R9,R10>
        MOVL    B^4(AP), R3             ; R3 contains address of A array
        MOVL    B^8(AP), R4             ; R4 contains address of B array
        MOVL    @B^12(AP), R0           ; store NROW in R0
        MOVL    @B^16(AP), R8           ; store L in R8
        MOVL    @B^20(AP), R1           ; store NCOL in R1
        MOVL    @B^24(AP), R10          ; store ILOW in R10
        CLRL    R2                      ; R2 is IB
        CMPB    R8, #16
        BNEQ    CONT
        BRW     S16B                    ; if L=16 go to S16
CONT:   CMPB    R8, #8
        BNEQ    C2
        BRW     S8B                     ; if L=8 go to S8
C2:     CMPB    R8, #4
        BEQL    S4B                     ; if L=4 go to S4
        MOVZBL  #32, R5                 ; I=32
        MOVL    (R3)+, R7               ; R7=A(1)
        MOVL    (R3)+, R6               ; R6=A(2)
LOOP2:  SUBB2   R8, R5                  ; R5=I=I-L
        BLSS    L1012                   ; if L>I goto L101
        EXTZV   R5, R8, R7, R9          ; extract bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        SOBGTR  R0, LOOP2               ; decrease R0, if R0>0 continue
        RET
L1012:  BICB2   #224, R5                ; R5=I=I.AND.31
        EXTZV   R5, R8, R6, R9
        MOVL    R6, R7
        MOVL    (R3)+, R6
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        SOBGTR  R0, LOOP2               ; decrease R0, if R0>0 continue
        RET
;
S4B:    ASHL    #-3, R0, R5             ; R5=R0/8
        BEQL    LL4B
L4B:    MOVL    (R3)+, R7               ; R7=A(IA)
        EXTZV   #28, R8, R7, R9         ; extract upper bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        EXTZV   #24, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        EXTZV   #20, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        EXTZV   #16, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        EXTZV   #12, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        EXTZV   #8, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        EXTZV   #4, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        EXTZV   #0, R8, R7, R9          ; extract lower bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        SOBGTR  R5, L4B                 ; decrease R5, if R5>0 continue
LL4B:   BICL2   #-8, R0                 ; R0=R0.AND.7
        BEQL    RTS4B
        MOVL    (R3)+, R7               ; R7=A(IA)
        EXTZV   #28, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        DECL    R0
        BEQL    RTS4B
        EXTZV   #24, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        DECL    R0
        BEQL    RTS4B
        EXTZV   #20, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        DECL    R0
        BEQL    RTS4B
        EXTZV   #16, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        DECL    R0
        BEQL    RTS4B
        EXTZV   #12, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2
        DECL    R0
        BEQL    RTS4B
        EXTZV   #8, R8, R7, R9
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
        ADDL2   R1, R2                  ; increase R2 by NCOL
        DECL    R0
        BEQL    RTS4B
        EXTZV   #4, R8, R7, R9          ; extract lower bit field
        ADDL3   R9, R10, (R4)[R2]       ; B(IB)=M+ILOW
RTS4B:  RET
;
#endif
#endif
#if defined(ALEPH_C)

/* DMPBL4(A,B,NROW,NBITS,NCOL,ILOW) */

#include "cfromf.h"

#define WORDLENGTH 32

FORT_CALL(dmpbl4) (a,b,nr,l,nc,ilow)

int a[];
int b[];
int *nr;
int *l;
int *nc;
int *ilow;

{

   int nrow;   /* r0 */
   int ib;     /* r2 */
   int i;      /* r5 - bit position [0..31] */
   int r6;     /* r6 */
   int r7;     /* r7 */
   int m;      /* r9 - extracted integer */
   int fact;   /* r11 */
   int aindex;
   int j;

       ib = 0;
       aindex = 0;
       nrow = *nr;

       if (*l == 16) goto s16b;
       if (*l == 8) goto s8b;
       if (*l == 4) goto s4b;

       r7 = a[aindex++];
       r6 = a[aindex++];
       i = WORDLENGTH;

loop2: j = i;
       i = i - *l;
       if (0 > i) goto l1012;
       extzv(i,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow > 0) goto loop2;
       return;

l1012: i = i & (WORDLENGTH - 1);
       extzv(i,*l,r6,&m);
       m = m | ((r7 & ((1 << j) - 1)) << (WORDLENGTH - i));
       r7 = r6;
       r6 = a[aindex++];
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow > 0) goto loop2;
       return;

s16b:  i = nrow >> 1;
       if (i == 0) goto ll16b;
l16b:  r7 = a[aindex++];
       extzv(16,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       m = r7 & 65535;
       b[ib] = m + *ilow;
       ib = ib + *nc;
       i--;
       if (i > 0) goto l16b;
ll16b: nrow = nrow & 1;
       if (nrow == 0) return;
       r7 = a[aindex++];
       extzv(16,*l,r7,&m);
       b[ib] = m + *ilow;
       return;

s8b:   i = nrow >> 2;
       if (i == 0) goto ll8b;
l8b:   r7 = a[aindex++];
       extzv(24,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(16,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(8,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       m = r7 & 255;
       b[ib] = m + *ilow;
       ib = ib + *nc;
       i--;
       if (i > 0) goto l8b;
ll8b:  nrow = nrow & 3;
       if (nrow == 0) return;
       r7 = a[aindex++];
       extzv(24,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(16,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(8,*l,r7,&m);
       b[ib] = m + *ilow;
       return;

s4b:   i = nrow >> 3;
       if (nrow == 0) goto ll4b;
l4b:   r7 = a[aindex++];
       extzv(28,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(24,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(20,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(16,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(12,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(8,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(4,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       extzv(0,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       i--;
       if (i > 0) goto l4b;
ll4b:  nrow = nrow & 7;
       if (nrow == 0) return;
       r7 = a[aindex++];
       extzv(28,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(24,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(20,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(16,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(12,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(8,*l,r7,&m);
       b[ib] = m + *ilow;
       ib = ib + *nc;
       nrow--;
       if (nrow == 0) return;
       extzv(4,*l,r7,&m);
       b[ib] = m + *ilow;
       return;

}
#endif
