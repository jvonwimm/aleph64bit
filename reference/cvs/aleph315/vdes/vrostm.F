      INTEGER FUNCTION VROSTM (IVIEW,NROSTR,RPITCH,IRFREQ)
C ----------------------------------------------------------------------
CKEY VDETDES STRIP / USER
C!  Returns information about the readout strips on a wafer
C - Steve Wasserbaech, February 1994
C
C - Input:
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VROSTM / I  = VDOK if successful;
C               = VDERR if IVIEW is invalid.
C   NROSTR / I  Number of readout strips per wafer
C   RPITCH / R  Readout strip pitch (cm)
C   IRFREQ / I  Readout strip frequency; e.g., if IRFREQ = 2 then
C               every second physical strip is read out
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vwgeco.h"
#include "vrdoco.h"
C
C     Arguments:
      INTEGER IVIEW, NROSTR, IRFREQ
      REAL RPITCH
C
C ----------------------------------------------------------------------
C
      IF (IVIEW .EQ. VVIEWZ) THEN
C
C     z view:
        NROSTR = NRDSTZ
        RPITCH = STPITZ * FLOAT(NREFRZ)
        IRFREQ = NREFRZ
        VROSTM = VDOK
C
      ELSEIF (IVIEW .EQ. VVIEWP) THEN
C
C     r-phi view:
        NROSTR = NRDSTP
        RPITCH = STPITP * FLOAT(NREFRP)
        IRFREQ = NREFRP
        VROSTM = VDOK
C
      ELSE
C
C     invalid view:
        NROSTR = 0
        RPITCH = 0.
        IRFREQ = 0
        VROSTM = VDERR
C
      ENDIF
C
      RETURN
      END
#endif
