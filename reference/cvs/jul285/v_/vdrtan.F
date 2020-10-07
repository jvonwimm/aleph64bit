      SUBROUTINE VDRTAN(JWAF,SXYZ,TNG)
C ----------------------------------------------------------------------
C!  Given the hit position, this routine returns the cotangents necessary
C!  to calculate the Jacobian (to get the alignment errors)
CKEY V_DECK VDET
C -  Author         Manoj Thulasidas  9-May-1996
C  input:  JWAF - the global wafer address
C          SXYZ - the global hit position array(3), real
C  output: TNG  - the cotangents array(2), real
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"
C  arguments
      INTEGER JWAF
      REAL SXYZ(3), TNG(2)
C  functions
      REAL VPHIFN, VWTHCK, VDOT
      INTEGER VJFACW
      EXTERNAL VPHIFN, VWTHCK, VDOT, VJFACW
C  locals
      REAL XDIFF(3), UVEC(2), CT, WID, PHIN
      INTEGER I, JFAC, IERR
C=======================================================================
C get the global face from JWAF
      JFAC = VJFACW(JWAF)
C-- default value, 45 degree
      TNG(1) = 1.0
      TNG(2) = 1.0
C-- wafer thickness
      WID = VWTHCK()
      IF (WID.LE.0.0) THEN
        WRITE (IW(6),*)' VDRTAN:Arithmatic trap, divide by zero.'
        RETURN
      ENDIF
      IERR = VPHIFN(JFAC,PHIN)
      CALL VDINCP(WID,PHIN,SXYZ,XDIFF)
      TNG(1) = XDIFF(3)/WID
C-- a unit vector along u direction is (-sin cos 0)
      UVEC(1) = -SIN(PHIN)
      UVEC(2) = COS(PHIN)
      CT = VDOT(UVEC,XDIFF,2)
      TNG(2) = CT/WID
      RETURN
      END
#endif
