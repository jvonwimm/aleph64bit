      SUBROUTINE SIDSUM(ICLU,IDEAD,ESUM)
C------------------------------------------------------------
C! -For pad # IDEAD compute energy sums in cluster ICLU from pads
C! IPH-1 to IPH+1 which has the same IR , in layers IZ-1 ,IZ and IZ+1
C  B. Bloch -Devaux  June  93
C  Structure : SUBROUTINE
C           External references:NAMIND
C           Comdecks references:BCS,BMACRO,SPDAJJ,SCLSJJ
C  Input : ICLU cluster number to be corrected
C          IDEAD pad address
C  Output : ESUM(3)  sums over 3 phi pads width in 3 z layers
C  Banks  : input  - SPDA ,SMAP
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "spdajj.h"
      DIMENSION ESUM(3)
C
#include "bmacro.h"
C -------------------------------------------------------------------
C
      NASMAP = NAMIND('SMAP')
      NASPDA = NAMIND('SPDA')
      CALL VZERO(ESUM,3)
      KSPDA = IW(NASPDA)
      KSMAP = IW(NASMAP)
      IF (KSPDA.LE.0) GO TO 999
      NSPDA = LROWS(KSPDA)
      IF (NSPDA.LE.0) GO TO 999
      CALL SIDCOZ(IDEAD,MODD,ISDD,IPDD,IRDD)
      DO 7 IPL  = -1,1
         ISZ = ISDD +IPL
         IF ( ISZ.LE.0 ) GO TO 7
         IF ( ISZ.GT.12) GO TO 7
         DO 6 IPH  = -1, 1
            IPHI = IPDD+IPH
            IF (IPHI.LE.0 ) IPHI = 32
            IF (IPHI.GT.32) IPHI = 1
            CALL SIENCZ( IAD,MODD,ISZ,IPHI,IRDD)
            ITOW = ITABL(KSMAP,IAD,1)
            IF ( ITOW.LE.0) GO TO  6
            KLL = KROW(KSPDA,ITOW)
            ICL = IW(KLL+JSPDSC)
            IF ( ICL.NE.ICLU ) GO TO 6
            RE = RW(KLL+JSPDEN)
            ESUM(IPL+2) = ESUM(IPL+2) + RE
  6      CONTINUE
    7 CONTINUE
  999 RETURN
      END
#endif
