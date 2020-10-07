      SUBROUTINE QMCLR
CKEY INIT /INTERNAL
C----------------------------------------------------------------------
C! Clear ALPHA banks, commons, etc.
C! called from : QMREAD
C! calls : none
C!                                                  H.Albrecht 20.09.88
C!                                     Modified:    E.Blucher  31.01.92
C!                                     Modified:    J.Boucrot  08.10.93
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C
C         ALPHA banks :
C
      KQVEC = IW(NAQVEC)
      KOQVEC = KQVEC + LMHLEN - KCQVEC
      KQVRT = IW(NAQVRT)
      KOQVRT = KQVRT + LMHLEN - KCQVRT
      KOQDET = IW(NAQDET) + LMHLEN
C
C         KSAME pointer
C
      DO 10 I=KLUST+1,KFFRT-1
   10 IW(KOQVEC+I*KCQVEC+JQVESP) = I
C Clear main vertex :
      JQVRM = KQVRT + LMHLEN
      DO 5 I = JQVRVX,JQVRVZ
 5    RW(JQVRM+I) = 0.
      DO 6 J = JQVREM,JQVRCF
 6    RW(JQVRM+J) = 0.
C
C         clear loci; leave 50 free locations
C
      KFFRT = KLUST + 51
      KFFRD = KLUST + 51
      DO 30 M = KSOVT, KSMCT
        KLOCUS(1,M) = KFFRT
        KLOCUS(2,M) = KFFRT - 1
   30 KLOCUS(3,M) = 0
      KNRET = 0
      KNCOT = 0
C
      KFREV = KLUSV + 1
      KFMCV = KLUSV + 1
      KFFRV = KLUSV + 1
      KLREV = KLUSV
      KLMCV = KLUSV
      KNREV = 0
      KNMCV = 0
C
C         clear QLIN
C
      KOQLIN = IW(NAQLIN) + LMHLEN
      DO 40 M=KOQLIN+1,KOQLIN+KNQLIN
   40 IW(M) = 0
      KNQLIN = 0
C
C      dummy detector bank, work bank :
C
      KQZER = IW(NAQZER)
      IW(KOQDET+JQDEAF) = KQZER
      IW(KOQDET+JQDEAL) = KQZER
      IW(KOQDET+JQDEAT) = KQZER
      IW(KOQDET+JQDEAE) = KQZER
      IW(KOQDET+JQDEAH) = KQZER
      IW(KOQDET+JQDEAM) = KQZER
      IW(KOQDET+JQDEEC) = KQZER
      IW(KOQDET+JQDEET) = KQZER
      IW(KOQDET+JQDEHC) = KQZER
      IW(KOQDET+JQDEFI) = KQZER
      IW(KOQDET+JQDEEF) = KQZER
      IW(KOQDET+JQDEPC) = KQZER
      IW(KOQDET+JQDEEG) = KQZER
      IW(KOQDET+JQDEMU) = KQZER
      KQWRK = IW(NAQWRK)
C
C         clear QFPA
C
      KOQFPA = IW(NAQFPA) + LMHLEN - KCQFPA
      IF (KMQFPA .LE. 2)  THEN
        DO 60 M=1,KNQFPA
          IW(KOQFPA+M*KCQFPA+1) = 0
   60   IW(KOQFPA+M*KCQFPA+2) = 0
      ELSE
        DO 70 M=1,KNQFPA
          DO 70 N=1,KMQFPA
   70   IW(KOQFPA+M*KCQFPA+N) = 0
      ENDIF
C
      KMQFPA = 2
      DO 80 N=3,KCQFPA
        KCLACO(N)  = KUNDEF
        KCLAFR(N)  = 0
   80 KCLARM(N)  = 0
C
C         clear LOCK
C
      DO 90 M=1,KLOCKM
        KLOCK0(M,1) = 0
        KLOCK0(M,2) = 0
        KLOCK1(M,1) = 0
        KLOCK1(M,2) = 0
        KLOCK2(M,1) = 0
   90 KLOCK2(M,2) = 0
      XLREV(1) = .FALSE.
      XLREV(2) = .FALSE.
      XLREV2(1) = .FALSE.
      XLREV2(2) = .FALSE.
C
C         clear ECAL wire energies
C
      DO 91 I=1,36
        QEECWI(I)=0.
 91   CONTINUE
C
      KQPAR = IW(NAQPAR)
      KOQPAR = KQPAR + LMHLEN - KCQPAR
      KOQPLI = IW(NAQPLI) + LMHLEN
      KOQTRA = IW(NAQTRA) + LMHLEN
      KOQPBT = IW(NAQPBT) + LMHLEN
      KOQMTL = IW(NAQMTL) + LMHLEN
      IW(KOQMTL+1) = KQZER
      KOQMTS = IW(NAQMTS) + LMHLEN
      KNQMTX = 1
C
C       options for QJ routines
C
      KJOPTN(1,1) = 3
      KJOPTN(1,2) = 0
      KJOPTN(2,1) = 2
      KJOPTN(2,2) = 0
      KTDROP = 0
C
C Reset all user's flag in all "tracks" :
C
      DO 120 ITR=1,LROWS(KQVEC)
         KKVEC=KROW(KQVEC,ITR)
         DO 110 ID=JQVEUS,KCQVEC
            IW(KKVEC+ID)=0
            RW(KKVEC+ID)=0.
 110     CONTINUE
 120  CONTINUE
      END
#endif
