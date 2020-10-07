      SUBROUTINE QVSTKV(ITK,XYZA,DIRA,CRVA)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Returns 3-vectors for position, direction, curvature, error
C     IN ALEPH COORDINATE SYSTEM FOR ALPHA TRACK ITK
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C     ITK IS ALPHA TRACK INDEX
C  Output Arguments :
C  *  XYZA() IS COORDINATES OF CLOSEST APPROACH TO ALEPH Z AXIS
C  *  DIRA() IS NORMALIZED DIRECTION VECTOR AT THAT POINT
C  *  CRVA() IS ORTHOGONAL DIRECTION FOR CURVATURE
C        POSITION VS DISTANCE S IS THEN (FOR SMALL CRVA AND SMALL S)
C        XYZ() = XYZA() + DIRA()*S + .5*CRVA()*S*S
C
#ifndef DOC
      DIMENSION XYZA(3),DIRA(3),CRVA(3)
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
C
C ANGLE FACTORS
      SP=SIN(P0)
      CP=COS(P0)
      ST=1./SQRT(1.+TL**2)
C COORDINATES AT CLOSEST APPROACH TO Z AXIS
      XYZA(1)=D0*SP
      XYZA(2)=-D0*CP
      XYZA(3)=Z0
C DIRECTION AT XYZA
      DIRA(1)=ST*CP
      DIRA(2)=ST*SP
      DIRA(3)=ST*TL
C CURVATURE VECTOR
      CRV=RI*ST
      CRVA(1)=-DIRA(2)*CRV
      CRVA(2)=DIRA(1)*CRV
      CRVA(3)=0.
C
      RETURN
      END
#endif
