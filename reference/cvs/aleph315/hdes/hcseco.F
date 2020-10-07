      SUBROUTINE HCSECO
C------------------------------------------------------------------
CKEY HCALDES HCAL GEOM CONSTANT / INTERNAL
C                                                                 C
C! Calculate Geometrical quantities from the D.B. constants       C
C!                                  Author:G.Catanesi 87/10/10    C
C!                                                                C
C!          Called by:HCIRUN                                      C
C!          Calls    :HCBOUN                                      C
C!                                                                C
C!-----------------------------------------------------------------
#ifndef DOC
#include "alcons.h"
#include "hcgeom.h"
C
C
C. Towers boundaries in theta
C.
      CALL HCBOUN
C
C.      lower edges of iron spacers in end caps (sextant Ref.Sys)
C.
      DO 10 JJ=1,LPHCT
         HCAPDE(JJ) = HCDREC + HCDSTP + (HCDSTP+HCAPSL)*(JJ-1)
   10 CONTINUE
C.
C.    number of columns in each tower region
C.
      DO 20 J=1,LHCRE
         NTHCFI(J) = 2**J
   20 CONTINUE
C.
C.   phi size of tower in region I
C.
      DO 30 I=1,LHCRE
         HCFITW(I) = PIBY6/NTHCFI(I)
   30 CONTINUE
C.
C   spacers position  and # of tubes  in layers (barrel)
C
      DO 40 N=1,LHCNL
         NHCTU1(N) = NEITHC(N) * NHCEIT
         HCBLSP(N,1) = HCDEWI + NEITSP(N,1)*HCEIWI
         HCBLSP(N,2) = HCDEWI + NEITSP(N,2)*HCEIWI + HCSPLT(N,1)
C
         IF(NEITSP(N,1).EQ.-1)HCBLSP(N,1) = 0.
         IF(NEITSP(N,2).EQ.-1)HCBLSP(N,2) = 0.
   40 CONTINUE
C.
C. Phi width in a tower row
C.
      DO 50 JJ=1,LHCTR
         KFAC = 1
         IF(JJ.LE.IHCTID(2).OR.JJ.GT.(LHCTR-IHCTID(2)))KFAC = 2
         IF(JJ.LE.IHCTID(1).OR.JJ.GT.(LHCTR-IHCTID(1)))KFAC = 4
         PHCTOR(JJ) = (TWOPI*KFAC)/NHCPHC
   50 CONTINUE
C.
C.   Layer radius and width (barrel)
C.
      TAN15 = TAN(PIBY12)
      HCLARA(1) = HFSRBL
      HCLAWI(1) = HFSRBL*TAN15
C
      DO 60 JJ=2,NHCBLA
         HCLARA(JJ) = HFSRBL + (JJ-1)*HCSMTH
         HCLAWI(JJ) = (HCLARA(JJ))*TAN15
   60 CONTINUE
C
      DO 80 J=1,NHCTWR/2
C
         IHCREG(J) = 1
         DO 70 K=1,NHCREF-1
            IF(J.GT.IHCTID(K))THEN
               IHCREG(J) = K+1
            ENDIF
   70    CONTINUE
         IHCREG(LHCTR+1-J) = IHCREG(J)
   80 CONTINUE
C
C?            get overall dimensions (for analog readout)
C
        YBAST1=HCRMIN(1)+FLOAT(NHCBFS-1)*(HCIRTH+HCTUTH)+HCTUTH+0.5
        YBARMX=HCRMAX(1)-HCLSLA
        ZENST1=HCZMIN(2)+FLOAT(NHCEFS)*(HCIRTH+HCTUTH)
        ZENDMX=HCZMAX(3)-HCLSLA
C
C?            get constants for digital readout
C
        XBARR0=HCDEWI+0.5*HCSABL
        XENDC0=HCDREC+0.5*HCSAEC
C
C
        RETURN
        END
#endif
