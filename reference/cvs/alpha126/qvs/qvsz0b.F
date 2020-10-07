      SUBROUTINE QVSZ0B(ITK,PVXY,ZB,VZB)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Returns ZB = z at distance of closest approach to beam axis
C     and VZB=variance (sigma-squared)
C     given ALPHA track index ITK, and beam axis PVXY(),
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION PVXY(2)
#include "qcde.h"
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
C GET HELIX PARAMETERS
      RI=QFRFIR(ITK)
      TL=QFRFTL(ITK)
      P0=QFRFP0(ITK)
      D0=QFRFD0(ITK)
      Z0=QFRFZ0(ITK)
C FIND COORDINATES AT CLOSEST APPROACH TO ORIGIN (NOT BEAM)
      SP=SIN(P0)
      CP=COS(P0)
      X0=D0*SP
      Y0=-D0*CP
C RESTATE RELATIVE TO BEAM
      XB=X0-PVXY(1)
      YB=Y0-PVXY(2)
C ROTATE INTO TRACK DIRECTION
      XBR=XB*CP+YB*SP
      YBR=YB*CP-XB*SP
C EXTRAPOLATE TO BEAM PLANE (IN ROTATED X DIRECTION)
      ZB=Z0-XBR*TL
C GET VARIANCE ON Z0
      VZB=QFRFEM(ITK,5,5)
      RETURN
      END
#endif
