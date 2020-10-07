      SUBROUTINE HCMODN(ITH,IPH,ISUB,IMD1,IMD2)
C--------------------------------------------------------------------
C
C!  Finds the module number for a given HCAL tower
C!
C!          Author : A. Sciaba` 02/07/1998
C! input:
C!         ITH   : tower theta index
C!         IPH   : tower phi index
C! output:
C!         ISUB  : subcomponent (1=barrel,2=overlap,3=endcaps)
C!         IMD1  : module number for the barrel part of the tower
C!         IMD2  : module number for the endcap part of the tower
C!
C!        -Called by : HCFORA
C!        -Calls     : MDARD, NAMIND
C-------------------------------------------------------
#ifndef DOC

C
C     ISUB = 1 (BARREL), 2(OVERLAP), 3(ENDCAP)
C
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "iocom.h"
#include "hcnamc.h"
#include "bmacro.h"

      ISUB = 0
      IMD1 = 0
      IMD2 = 0
C
      NHNGR = NAMIND('HNGR')
      JHNGR = IW(NHNGR)
      IF (JHNGR .EQ. 0) THEN
         GOTO 999
      ENDIF
      IHNGR = ITABL(JHNGR,ITH,IPH)
      IHBA = MOD(IHNGR,1000)
      IHEC = IHNGR/1000
      IF (IHBA .GT. 0) THEN
         ISUB = 1
         IMD1 = (IPH-1)/4 + 1
         IMD2 = 0
         IF (IHNGR .GT. 1000) THEN
            ISUB = 2
            IMD2 = (IHEC-1)/10 + 1
         ENDIF
      ELSE
         ISUB = 3
         IMD1 = 0
         IMD2 = (IHEC-1)/10 + 1
      ENDIF

 999  RETURN
      END


#endif
