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
      DATA IG95 / 9510 /
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
C year of geometry is stored as yymm in the ASIM bank
      IF (IASIM.GT.0) THEN
         IYG=ITABL(IASIM,1,1)
         IF (IYG.LT.10000) THEN
            IGEOYR=1900+IYG/100
         ELSE
            IGEOYR=IYG/100
         ENDIF
      ENDIF
      IF (IGEOYR.LE.0) GO TO 999
      IF (IGEOYR.LE.IGEOL1) THEN
         IFLAG=1
      ELSE
         IFLAG=2
      ENDIF
C Monte Carlo for 1995 LEP 1.5 have a special geometry
C and must be flagged as LEP2 :
      IF (IFLAG.EQ.1.AND.IYG.GE.IG95) IFLAG=2
C
 999  RETURN
      END
#endif
