      SUBROUTINE QVSXZB(ITK,TSMR,YB,XB,VXB,ZB,VZB)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Finds track impact with beam plane
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  ITK IS ALPHA TRACK INDEX
C  *  TSMR IS ADDITIONAL TRACK SMEARING
C  *  YB IS Y COORDINATE OF BEAM PLANE
C  Output Arguments :
C  *  XB,ZB ARE TRACK IMPACTS ON HORIZONTAL PLANE OF BEAM
C  *  VXB,VZB ARE VARIANCES IN XB, ZB
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION DJET(3),PVTX(3)
#include "qcde.h"
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
C CREATE VERTICAL JET
      DJET(1)=0.
      DJET(2)=1.
      DJET(3)=0.
C AND VERTEX AT BEAM PLANE
      PVTX(1)=0.
      PVTX(2)=YB
      PVTX(3)=0.
C
C CALCULATE TRACK IMPACT, ERRORS, ETC. AT BEAM PLANE
      CALL QVSTKP(ITK,PVTX,DJET,
     > XB,DXDT,D2XDT2,ZB,DZDT,D2ZDT2,VXB,VZB,VXZ)
C
C Z IN ROTATED SYSTEM TURNS OUT TO BE -Z IN ALEPH SYSTEM
      ZB=-ZB
C
C ADD THE SMEARING
      VXB=VXB+TSMR**2*(1.+DXDT**2)
      VZB=VZB+TSMR**2*(1.+DZDT**2)
C
      RETURN
      END
#endif
