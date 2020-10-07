      LOGICAL FUNCTION XESTAB(KRUN)
CKEY  XLUMOK TRIG/INTERNAL
C----------------------------------------------------------------------
C!  Get Energy stability status for year 2000
C!  Called from XLUMOK/LLUMOK/SLUMOK
C!    Author:     B.Bloch             March 9,2000
C!            define the Estable bit if in year 2000, if High Energy
C!            and MRMP card not present, also possibly modify the
C!            first run number with miniramp by data card FRMP.
C!
C!    Input:      - HV event word
C!    Output:     - XESTAB /L   Energy stable bit
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      PARAMETER (ELEPHE= 110.)
      PARAMETER (IBIT = 18)
      INTEGER XHVBIT
      REAL XYZ(3),DXYZ(3)
      EXTERNAL NAMIND,XHVBIT
      DATA IFI/0/
      IF ( IFI.eq.0) then
C   define first run for which miniramp bit exists
C   this is just to allow to mofify it if we start HE without
C   a reliable info implemented in the hardware !
         IND = NAMIND('FRMP')
         KRUN2K = 53000
         IF ( IW(IND).GT.0) KRUN2K = IW(IND+1)
         IFI = 1
C   check if data card MRMP present : force to ignore miniramp bit
         IMRMP = 0
         IND = NAMIND('MRMP')
         IF ( IW(IND).GT.0) IMRMP = 1
      ENDIF
      XESTAB= .TRUE.
C for runs before year 2000 (or first run implemeted), stable
      IF (KRUN.lt.KRUN2K) go to 999
C for non HE runs ( that is Z runs ) always stable
      CALL GETLEP(KRUN,IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
      IF (ELEP.lt.ELEPHE) go to 999
C if data card MRMP present, ignore Estab bit
      IF (IMRMP.gt.0) go to 999
C now the real stuff !
C Get detector HV status
      iret = XHVBIT(jhvst)
C now check Estab bit
      XESTAB = BTEST(JHVST,(IBIT-1))
999   RETURN
      END
#endif
