      INTEGER FUNCTION ALKMAR(NSEQ,NRDN,NRDN2)
C------------------------------------------------------------------
C - B.Bloch - 980313
C! Create kine event header KMAR
C - Input :  NRDN,NRDN2 = seeds for the random sequences used
C            NSEQ = number of sequences used
C - Output : ALKMAR = KMAR bank index
C                     0 means not enough space to book the bank
C
C   --------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "kmarjj.h"
      INTEGER NRDN(3),NRDN2(3)
      DATA IFI /0/
C ----------------------------------------------------------------------
C - Book the 'KMAR' bank
      CALL AUBOS ('KMAR',0,LMHLEN+NSEQ*LKMARA,JKMAR,IGARB)
      IF (JKMAR.EQ.0) GOTO 999
      IW(JKMAR+LMHCOL) = LKMARA
      IW(JKMAR+LMHROW) = NSEQ
      CALL BKFMT ('KMAR','I')
C
C - Fill 'KMAR'
      I = 1
      do j = 1,nseq
         KKMAR = JKMAR + LMHLEN + LKMARA*(J-1)
         IW(KKMAR+JKMARN) = NRDN(I)
         IW(KKMAR+JKMASR) = NRDN(I+1)
         IW(KKMAR+JKMATR) = NRDN(I+2)
         I = I + 3
      enddo
C
 999  CONTINUE
      ALKMAR = JKMAR
      RETURN
      END
#endif
