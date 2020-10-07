      INTEGER FUNCTION VSENSI (IVIEW,SAMIN,SAMAX,SBMIN,SBMAX)
C ----------------------------------------------------------------------
CKEY VDETDES WAFER / USER
C!  Returns the boundaries of the sensitive region of a wafer and view
C - Steve Wasserbaech, February 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VSENSI / I  = VDOK if successful;
C               = VDERR if IVIEW is invalid.
C   SAMIN  / R  Minimum a coordinate (cm) of sensitive region
C   SAMAX  / R  Maximum a coordinate (cm) of sensitive region
C   SBMIN  / R  Minimum b coordinate (cm) of sensitive region
C   SBMAX  / R  Maximum b coordinate (cm) of sensitive region
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
#include "vdetge.h"
C
C     Arguments:
      INTEGER IVIEW
      REAL SAMIN, SAMAX, SBMIN, SBMAX
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
C
        SAMIN = AMNSRZ
        SAMAX = AMXSRZ
        SBMIN = BMNSRZ
        SBMAX = BMXSRZ
        VSENSI = VDOK
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
C
        SAMIN = AMNSRP
        SAMAX = AMXSRP
        SBMIN = BMNSRP
        SBMAX = BMXSRP
        VSENSI = VDOK
C
      ELSE
C
C     Invalid view:
C
        SAMIN = 0.
        SAMAX = 0.
        SBMIN = 0.
        SBMAX = 0.
        VSENSI = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
