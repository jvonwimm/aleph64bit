      SUBROUTINE MINDTYP(IFLAG)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Tels if one deals with LEP1 or LEP2 real data or MCarlo data
C
C     Author: Jacques Boucrot      02-Sep-98
C
C    Output  : IFLAG  = 1 if LEP1 real data or MCarlo data  
C                     = 2 if LEP2 real data or MCarlo data
C                     = 0 if unkown (should never happen !)
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
      DATA IMCRU,IGEOL1,IRUNL1 / 2000, 1995 , 37853 /      
C
#include "bmacro.h"
C-----------------------------------------------------------------------
      IFLAG=0
      CALL ABRUEV(IRUN,IEVT)
      IF (IRUN.LT.IMCRU) GO TO 100
C Real data
      IF (IRUN.LE.IRUNL1) THEN
         IFLAG=1
      ELSE
         IFLAG=2
      ENDIF
      GO TO 999
C Monte Carlo data :
 100  IGEOYR=0
      IASIM=IW(NAMIND('ASIM'))
      IF (IASIM.GT.0) THEN
         IYG=ITABL(IASIM,1,1)
         IGEOYR=1900+IYG/100
      ENDIF
      IF (IGEOYR.LE.0) GO TO 999
      IF (IGEOYR.LE.IGEOL1) THEN
         IFLAG=1
      ELSE
         IFLAG=2
      ENDIF
C
 999  RETURN
      END
#endif









