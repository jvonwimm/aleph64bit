      SUBROUTINE YVSTRJ(PVTX,DJET,XYZ,XYZR)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Tranforms point in XYZ system to XYZR system
C     (ORIGIN AT PVTX, ROTATED TO DJET DIRECTION)
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  PVTX() IS ORIGIN OF NEW SYSTEM (PRIMARY VERTEX)
C  *  DJET() IS NORMALIZED DIRECTION VECTOR
C  *  XYZ() IS POINT TO BE TRANSFORMED
C  Output Argument :
C  *  XYZR() IS POINT IN NEW SYSTEM
C       XYZR(3) COMPONENT IS ALONG DJET DIRECTION
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION PVTX(3),DJET(3),XYZ(3),XYZR(3)
C ------------------------------------------------------------------------------
C FIRST DO TRANSLATION
      XYZR(1)=XYZ(1)-PVTX(1)
      XYZR(2)=XYZ(2)-PVTX(2)
      XYZR(3)=XYZ(3)-PVTX(3)
C
C THEN ROTATION
      CALL YVSRTJ(DJET,XYZR,XYZR)
C
      RETURN
      END
#endif
