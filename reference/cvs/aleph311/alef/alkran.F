      INTEGER FUNCTION ALKRAN (NSEQ,IS)
C -------------------------------------------------------------------
C  B. Bloch march 98, to keep seeds of NSEQ random sequence
C! Build kine run header KRAN
C - Input  :  NSEQ    = # of random sequences
C             IS      = array of initial seeds
C - Output :  ALKRAN  = KRAN bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "kranjj.h"
      DIMENSION IS(4)
C --------------------------------------------------------------------
C - Book the bank 'KRAN'
      CALL AUBOS ('KRAN',0,LMHLEN+LKRANA*NSEQ,JKRAN,IGARB)
      IF (JKRAN.EQ.0) GOTO 999
      IW(JKRAN+LMHCOL) = LKRANA
      IW(JKRAN+LMHROW) = NSEQ
      CALL BKFMT ('KRAN','I')
C
C - fill the  2 words of the row
C
      I = 1
      do J = 1,NSEQ
         KKRAN = JKRAN + LMHLEN + (J-1)*LKRANA
         IW(KKRAN+JKRAFS) = IS(I)
         IW(KKRAN+JKRASS) = IS(I+1)
         I = I + 2
      enddo
C
 999  CONTINUE
      ALKRAN = JKRAN
      END
#endif
