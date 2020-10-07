      INTEGER FUNCTION SIGTWA(IDUM)
C----------------------------------------------------------
C!  - Get wagon number from whatever reliable source of information
C!    This routine should eventually go to the ALEPHLIB
C!
C!  Author : B. Bloch -Devaux  25 October 1994
C!
C!  Input  :  IDUM dummy
C!            SILH bank
C!
C!  Output :  SIGTWA  = wagon number in the train
C----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "silhjj.h"
#include "schujj.h"
C     defaults values for the ADC with wagon delay
      PARAMETER ( IPED = 212 , ISTEP = 470 , IDEF = 685)
#include "bmacro.h"
C----------------------------------------------------------
C  Access train info from SILH
      KSILH = IW(NASILH)
      KSCHU = IW(NASCHU)
      IWAG = IDEF
      IF ( KSILH.GT.0) THEN
         IF ( LCOLS(KSILH).GT.14) IWAG = ITABL(KSILH,1,JSILWI)
      ELSE IF (KSCHU.GT.0) THEN
         IWAG = ITABL(KSCHU,1,JSCHWI)
      ENDIF
      NWAG = NINT ( FLOAT(IWAG-IPED)/FLOAT(ISTEP) )
      NWAG = MAX (1,NWAG)
      SIGTWA = NWAG
      RETURN
      END
#endif
