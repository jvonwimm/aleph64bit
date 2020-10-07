      INTEGER FUNCTION ALKMAR(NSEQ,NRDN,NRDN2)
C------------------------------------------------------------------  
C - B.Bloch - 980313
C! Create kine event header KMAR
C - Input :  NRDN = seeds for the random sequences used
C            NSEQ = number of sequences used 
C - Output : ALKMAR = KMAR bank index
C                     0 means not enough space to book the bank
C
C   --------------------------------------------------------
C#ifndef DOC
C#include "bcs.h"
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C#include "kmarjj.h"
      INTEGER JKMARN,JKMASR,JKMATR,LKMARA
      PARAMETER(JKMARN=1,JKMASR=2,JKMATR=3,LKMARA=3)
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
      INTEGER FUNCTION ALKRAN (NSEQ,IS) 
C -------------------------------------------------------------------
C  B. Bloch march 98, to keep seeds of NSEQ random sequence
C! Build kine run header KRAN
C - Input  :  NSEQ    = # of random sequences 
C             IS1,... = initial seeds 
C - Output :  ALKRAN  = KRAN bank index
C                       0 means not enough space to book the bank
C  ---------------------------------------------------
C#ifndef DOC
C#include "bcs.h"
C*CA BCS
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C#include "kranjj.h"
C*CD KRANJJ
      PARAMETER( JKRAFS=1,JKRASS=2, LKRANA=2)      
      dimension IS(4)
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
