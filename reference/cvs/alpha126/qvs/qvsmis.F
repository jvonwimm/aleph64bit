      SUBROUTINE QVSMIS(ITK,VTX,D0V,Z0V,R3D)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Calculates vertex miss distance
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  ITK IS ALPHA TRACK NUMBER
C  *  VTX() IS VERTEX COORDINATES
C  Output Arguments :
C  *  D0V IS D0 WITH RESPECT TO VERTEX
C  *  Z0V IS Z0 WITH RESPECT TO VERTEX
C  *  R3D IS 3D MISS DISTANCE
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION VTX(3)
#include "qcde.h"
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
C XY TRACK DIRECTION
      P0=QFRFP0(ITK)
      SP=SIN(P0)
      CP=COS(P0)
C
C TRACK POSITION AT CLOSEST APPROACH TO ALEPH Z AXIS
      D0=QFRFD0(ITK)
      X0= D0*SP
      Y0=-D0*CP
      Z0=QFRFZ0(ITK)
C
C ROTATE VERTEX TO SYSTEM WHERE TRACK PHI=0
      XV= CP*VTX(1)+SP*VTX(2)
      YV=-SP*VTX(1)+CP*VTX(2)
C
C D0 WITH RESPECT TO VERTEX
      D0V=D0+YV
C
C CURVATURE CORRECTION
      RINV=QFRFIR(ITK)
      D0V=D0V-.5*RINV*XV**2
C
C Z0 AT POINT OF CLOSEST APPROACH TO XV,YV AXIS
      TL=QFRFTL(ITK)
      ZV=Z0+TL*XV
C
C LONGITUDINAL VERTEX MISS
      Z0V=ZV-VTX(3)
C
C ANGLE FROM BEAM DIRECTION
      STH=1./SQRT(1.+TL**2)
      CTH=TL*STH
C
C TRACK DIRECTION AT CLOSEST APPROACH
      DX=CP*STH
      DY=SP*STH
      DZ=CTH
C
C FIND 3D POINT OF CLOSEST APPROACH TO VERTEX
C TRACK LINE IS X+S*D=(X0,Y0,X0)+S*(DX,DY,DZ)
C VERTEX POINT IS V=VTX(1,2,3)
C     DIST**2=(X+S*D-V)**2
C     D/DS   = 2 (X+SMIN*D-V) DOT D = 0
C     SMIN   = ((V-X) DOT D) / (D DOT D) = (V-X) DOT D
C
      SMIN=(VTX(1)-X0)*DX + (VTX(2)-Y0)*DY + (VTX(3)-Z0)*DZ
C
C SHORTEST VECTOR FROM TRACK TO VERTEX
      XC=X0+SMIN*DX-VTX(1)
      YC=Y0+SMIN*DY-VTX(2)
      ZC=Z0+SMIN*DZ-VTX(3)
C
C 3D DISTANCE FROM VERTEX
      R3D=SQRT(XC**2+YC**2+ZC**2)
C
      RETURN
      END
#endif
