      REAL FUNCTION SIEPCR(RE,MODU,IST,IPH,IRD)
C------------------------------------------------------------
C!  - Return SICAL PAD ENERGY with all corrections
C!
C!    Author E.Mannelli   February 93
C!    Input:  RE   pad energy in GeV
C!            MODU  Module ( 1 or 2)
C!            IST  Layer z number ( 1 to 12)
C!            IPH  Phi bin number ( 1 to 32)
C!            IRD  Rad bin number ( 1 to 16)
C!    Output: RE  pad energy in Gev after calibration correction( real)
C!    BANKS:  SCFD,SCPH and SCAX, SCGA through call to SIGACO
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
C
      DIMENSION ISIFAC(32),ISIFAC2(32)
      DATA IFIR/0/
      DATA ISIFAC/0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,
     &  1,1,0,0,1,1/
      DATA ISIFAC2/0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,
     &  0,1,0,1,0,1/
C--------------------------------------------------------------------
#include "bmacro.h"
C -------------------------------------------------------------------
      IF (IFIR.EQ.0) THEN
        IFIR = 1
        NASCPH = NAMIND ('SCPH')
        NASCFD = NAMIND ('SCFD')
      ENDIF
      KSCPH = IW(NASCPH)
      KSCFD = IW(NASCFD)
      IF (KSCPH.LE.0) GO TO 98
      IF (KSCFD.LE.0) GO TO 98
C ADD JOHN'S SUPERFIX
      SUPFAC=1.0+RTABL(KSCFD,1,1)*FLOAT(IABS(ISIFAC(IPH)-(MODU-1)))
C try to correct for odd even
      SUPFAC2=RTABL(KSCFD,2,1)*FLOAT(IABS(ISIFAC2(IPH)-(MODU-1)))
C correct for variation across mother board
      IF(MODU.EQ.1) THEN
        IF(IPH.LE.16) THEN
          SUPFAC3=RTABL(KSCFD,3,1)*ABS(FLOAT(IPH)-16.5)/16.
        ELSE
          SUPFAC3=RTABL(KSCFD,3,1)*ABS(FLOAT(IPH)-32.5)/16.
        ENDIF
      ELSE
        IF(IPH.LE.16) THEN
          SUPFAC3=RTABL(KSCFD,4,1)*ABS(8.-ABS(FLOAT(IPH)-16.5))/16.
        ELSE
          SUPFAC3=RTABL(KSCFD,4,1)*ABS(8.-ABS(FLOAT(IPH)-32.5))/16.
        ENDIF
      ENDIF
      IF ( LROWS(KSCFD).GE.6 ) THEN
         IF(MODU.EQ.1) THEN
            IF(IPH.LE.8) SUPFAC5=-RTABL(KSCFD,5,1)
            IF(IPH.GT.8.AND.IPH.LE.16) SUPFAC5=RTABL(KSCFD,5,1)
            IF(IPH.GT.16.AND.IPH.LE.24) SUPFAC5=RTABL(KSCFD,6,1)
            IF(IPH.GT.24.AND.IPH.LE.32) SUPFAC5=-RTABL(KSCFD,6,1)
         ELSE
            IF(IPH.LE.8) SUPFAC5=-RTABL(KSCFD,6,1)
            IF(IPH.GT.8.AND.IPH.LE.16) SUPFAC5=RTABL(KSCFD,6,1)
            IF(IPH.GT.16.AND.IPH.LE.24) SUPFAC5=-RTABL(KSCFD,5,1)
            IF(IPH.GT.24.AND.IPH.LE.32) SUPFAC5=RTABL(KSCFD,5,1)
         ENDIF
      ELSE
        SUPFAC5 = 0.
      ENDIF
C give up on phi and just put in add hoc correction
      SUPFAC4=RTABL(KSCPH,MODU,IPH)
      SUPFAC=SUPFAC+SUPFAC2+SUPFAC3+SUPFAC5
      EJOHN =SUPFAC*RE*SUPFAC4
      IE = IFIX(EJOHN*1000.)
      RC = SIGACO(IE,MODU,IST,IPH,IRD)
      SIEPCR=RC*.001
      GOTO 999
C
   98 CONTINUE
      SIEPCR = RE
  999 RETURN
      END
#endif
