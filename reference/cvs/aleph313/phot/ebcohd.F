      SUBROUTINE EBCOHD( PHOT , CORH )
C -----------------------------------------------------------------
C   AUTHOR   : J.Badier   09/10/89
C!  Hadronic energy.
CKEY PHOTONS HADRON CORRECTION / INTERNAL
C
C   The energy is corrected by the pion/electron ratio.
C
C   Input  :    PHOT    Number of storeys of the cluster ICLN.
C
C   Output :    CORH     Hadronic correction factor.
C
C   BANKS :
C     INPUT   : ECNS
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
#ifndef DOC
      PARAMETER ( PIE1 = 1.5 , PIE2 = 1.3 , PIE3 = 1. )
      DIMENSION PHOT(*)
#include "ecnsjj.h"
#include "bcs.h"
      SAVE
      DATA KDEB / 0 /
#include "bmacro.h"
      IF( KDEB .EQ. 0 ) THEN
        KECNS = IW(NAMIND('ECNS'))
        IF (KECNS .NE. 0) THEN
      HAD3 = RTABL(KECNS,1,JECNR3)
      HAD1 = RTABL(KECNS,1,JECNR1) - HAD3
      HAD2 = RTABL(KECNS,1,JECNR2) - HAD3
         ELSE
         HAD3 = PIE3
         HAD1 = PIE1 - PIE3
         HAD2 = PIE2 - PIE3
      ENDIF
      KDEB = 1
      ENDIF
      CORH = PHOT(2) * HAD1 + PHOT(3) * HAD2 + HAD3
      RETURN
      END
#endif
