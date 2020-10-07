*DK sizini
      SUBROUTINE SIZINI
C.----------------------------------------------------------------
C  B.Bloch-Devaux   January 93
C! SCAL : Get zero suppression scheme and conversion constants
C     - Called by SIIRUN
C     - Banks SRCO , SFTH , SZTH
C
C.----------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "jobcom.h"
#include "siconst.h"
      CHARACTER*4 KEY,CHAINT
#include "bmacro.h"
C---------------------------------------------------------------------
C Default values for quantities in SICONST
C Zero suppression threshold on SIDI data   ( Mev       )
      ISIZCT = 20.
C Zero suppression threshold on SIFO data   ( ADC count )
      ISIFCT = 2
C Conversion factor from signal to SIFO ADC ( Mev/ADC count )
      SIMPCT = 109.53
C this value is a guess for 500 Kev tracking cut
      SIPGEV = 0.0042
      JSRCO = IW ( NAMIND('SRCO'))
      IF ( JSRCO.GT.0 ) THEN
         DO  10 I= 1,LROWS(JSRCO)
            KEY = CHAINT(ITABL(JSRCO,I,1))
            IF (KEY.EQ.'SIFO') SIMPCT = 1.E05/ITABL(JSRCO,I,2)
            IF (KEY.EQ.'PERG') SIPGEV = 10./ITABL(JSRCO,I,2)
 10      CONTINUE
      ENDIF
      JSZTH = IW ( NAMIND('SZTH'))
      IF ( JSZTH.GT.0 )  ISIZCT = IFIX(RTABL(JSZTH,1,2))
      JSFTH = IW ( NAMIND('SFTH'))
      IF ( JSFTH.GT.0 )  ISIFCT = ITABL(JSFTH,1,2)
      IF (IPRIJO(9).GT.0) WRITE(IW(6),100) ISIZCT,ISIFCT,SIMPCT
  100 FORMAT(/2X,'SICAL Analog signals and digitization constants :  '
     2    /16X,'Zero supp. threshold  for SIDI= ',I8  ,' Mev'
     3    /16X,'Zero supp. threshold  for SIFO= ',I8  ,' ADC count'
     5    /16X,'Conversion from signal to SIFO= ',F8.4,' Mev/ADC count'
     7                )
      RETURN
      END
#endif
