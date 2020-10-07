*DK tpsize
      SUBROUTINE TPSIZE(NEL)
C
C! Fast sim : Return the ionization charge (Landau Fluctuations)
C! for a super-broken segment containing NEL primary clusters.
C
C  Called from:  T2DEDX
C  Calls:        RNDM
C
C  Input:    FASTER:       -- NITLAN, to get index of bank TLAN
C                          -- NITIND, to get index of bank TIND
C                          -- INTWRD, index of work bank TWRD
C            PASSED:       --NEL, the number of primaries in the segment
C
C  Outputs:  PASSED:       --NEL, the number of electrons in the segment
C
C  P. Janot .  05/12/87
C
#include "faster.h"
#include "bcs.h"
C
      IF(NEL.EQ.0) RETURN
C
      X = RNDM(F)
C
      NEL0 = NEL
      IF(NEL.GT.95) THEN
C
C  If primary number is greater than 95, we use the linear
C  extrapolation based on the results obtained with 100
C  primaries
C
        NEL = 100
      ELSEIF(NEL.GT.25) THEN
C
C  If primary number is greater than 25, we use the linear
C  extrapolation based on the results obtained with 30,40,
C  50,60,70,80 or 90 primaries.
C
        NEL = (NEL+4)/10 *10
      ENDIF
C
C  Get pointer and word number of the NEL primary case "BANK"
C
      INDEX  = IW(NITLAN) + IW(IW(NITIND)+2+NEL) - 1
      NDWRDS = IW(INTWRD+2+NEL)
C
C  Determine the number of secondaries corresponding to NEL
C  primaries
C
      DO 2 I10 = 1,NDWRDS,10
C
C   First, count in tens (in order to save CPU time)
C
        I100 = INDEX+I10
        IF ( X .LE.RW(I100)) THEN
C
C   If X less than RW(INDEX+1), there are as secondaries as
C   primaries
C
           IF(I10.EQ.1) GOTO 60
           DO 3 I1=1,10
C
C   And then, find the right number of secondaries
C
             I = I100+I1-10
             IF ( X .LE. RW(I) ) THEN
               J   = I10+I1-11
C
C   Do the linear extrapolation
C
               NEL = FLOAT(NEL+J)*FLOAT(NEL0)/FLOAT(NEL)
               GOTO 60
             ENDIF
   3       CONTINUE
        ENDIF
   2  CONTINUE
      NEL = FLOAT(NEL+NDWRDS)*FLOAT(NEL0)/FLOAT(NEL)
   60 CONTINUE
      RETURN
      END
