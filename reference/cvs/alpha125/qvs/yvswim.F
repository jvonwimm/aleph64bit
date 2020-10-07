      SUBROUTINE YVSWIM(S,XYZ,DIR,CRV,XYZS,DIRS)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Swims track description vectors to path-length s
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  XYZ(3) IS TRACK POSITION  AT S=0
C  *  DIR(3) IS TRACK DIRECTION AT S=0
C  *  CRV(3) IS TRACK CURVATURE AT S=0
C       AT ANY S, XYZS(I) = XYZ(I) + DIR(I)*S + .5*CRV(I)*S*S
C  Output Arguments :
C  *  XYZS(3) IS TRACK POSITION  AT S
C  *  DIRS(3) IS TRACK DIRECTION AT S
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION XYZ(3),DIR(3),CRV(3)
      DIMENSION XYZS(3),DIRS(3)
C ------------------------------------------------------------------------------
      SS=.5*S*S
      XYZS(1)=XYZ(1)+DIR(1)*S+CRV(1)*SS
      XYZS(2)=XYZ(2)+DIR(2)*S+CRV(2)*SS
      XYZS(3)=XYZ(3)+DIR(3)*S+CRV(3)*SS
      DIRS(1)=DIR(1)+CRV(1)*S
      DIRS(2)=DIR(2)+CRV(2)*S
      DIRS(3)=DIR(3)+CRV(3)*S
C
      RETURN
      END
#endif
