      INTEGER FUNCTION GTYEAR()
CKEY YEAR
C-----------------------------------------------------------------------------
C! returns the data taking year or the MC geometry year
C  Author - F.Ranjard - 991201
C-----------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      INTEGER NAMIND,NAASIM,IDUM,ADBRUN,JASIM,IYY
      INTEGER YEARID,IRUN,IEVT,IROW,ISETUP
      DATA NAASIM/0/
#include "bmacro.h"
C----------------------------------------------------------------------------
      IF(NAASIM.EQ.0) NAASIM=NAMIND('ASIM')
C
      JASIM = IW(NAASIM)
      IF (JASIM.GT.0) THEN 
        IYY=ITABL(JASIM,1,1)
      ELSE
        CALL ABRUEV(IRUN,IEVT)
        IDUM = ADBRUN('VD',IROW,IYY,IRUN,ISETUP)
      ENDIF
      IYY = IYY/100
      IF (IYY.LT.100) then
         YEARID = 1900+IYY
      ELSE
         YEARID = IYY
      ENDIF
      IF (YEARID.LT.1989 .OR. YEARID.GT.2002) YEARID=0
C
      GTYEAR = YEARID
      RETURN
C
      END
#endif
