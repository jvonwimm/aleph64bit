      SUBROUTINE SITRIN
C***********************************************************************
C                                                                      *
C   Author   :- B. Bloch-Devaux      30-OCT-1991                       *
C                                                                      *
C! Init the trigger constants for SICAL                                *
C                                                                      *
C   Inputs        :  SRCO bank,SITC bank                               *
C   Output        :  SITRCO common                                     *
C                                                                      *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
      CHARACTER*4 KEY,CHAINT
#include "sitrco.h"
#include "sitcjj.h"
#include "bmacro.h"
C     Get the conversion factor from SIFO ADC count to SIXA ADC count
         SIFOXA = 0.228
         SIDIFO = 0.00913
         ISITHR(1) =  9
         ISITHR(2) = 12
         ISITHR(3) = 22
         ISITHR(4) = 41
         ISIDIS(1) = 0
         ISIDIS(2) = 0
         NASIFO =  NAMIND ('SIFO')
         JSRCO = IW ( NAMIND('SRCO'))
         IF ( JSRCO.GT.0 ) THEN
            DO  10 I= 1,LROWS(JSRCO)
               KEY = CHAINT(ITABL(JSRCO,I,1))
               IF (KEY.EQ.'SIXA') SIFOXA = 0.001*ITABL(JSRCO,I,2)
               IF (KEY.EQ.'SIFO') SIDIFO = 0.00001*ITABL(JSRCO,I,2)
 10         CONTINUE
         ENDIF
         NASITC =  NAMIND ('SITC')
         KSITC  = IW(NASITC)
         IF ( KSITC.GT.0 ) THEN
         ITH = ITABL(KSITC,1,JSITTT)
         DO 100 I = 1,4
            ISITHR(I) = IBITS(ITH,8*(I-1),8)
 100     CONTINUE
         ISIDIS(1) = ITABL(KSITC,1,JSITDO)
         ISIDIS(2) = ITABL(KSITC,1,JSITDE)
         ENDIF
         WRITE(IW(6),110) SIDIFO,SIFOXA,ISITHR,ISIDIS
  110 FORMAT(/2X,'SICAL Trigger conditions :  '
     5    /16X,'Conversion SIFO ADC per SIDI Mev:', F8.4,'     '
     5    /16X,'Conversion SIXA ADC per SIFO ADC:', F8.4,'     '
     2    /16X,'Thresholds in SIXA ADC counts : ',I8  ,' Very low',
     3    /16X,'                                ',I8  ,' Low      '
     4    /16X,'                                ',I8  ,' High     '
     4    /16X,'                                ',I8  ,' Very high'
     5    /16X,'Disable sector words odd/even  :',2I8,'     '
     7                )
      RETURN
      END
#endif
