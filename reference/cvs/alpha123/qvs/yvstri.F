      SUBROUTINE YVSTRI(PVTX,DJET,XYZR,XYZ)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Inverse-tranforms point in XYZR system to XYZ system
C     (ORIGIN AT PVTX, ROTATED TO DJET DIRECTION)
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  PVTX() IS ORIGIN OF NEW SYSTEM (PRIMARY VERTEX)
C  *  DJET() IS NORMALIZED DIRECTION VECTOR
C  *  XYZR() IS POINT TO BE TRANSFORMED
C       XYZR(3) COMPONENT IS ALONG DJET DIRECTION
C  Output Argument :
C  *  XYZ() IS POINT IN ORIGINAL SYSTEM
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION PVTX(3),DJET(3),XYZ(3),XYZR(3)
C ------------------------------------------------------------------------------
C FIRST UNDO ROTATION
      CALL YVSRTI(DJET,XYZR,XYZ)
C
C THEN UNDO TRANSLATION
      XYZ(1)=XYZ(1)+PVTX(1)
      XYZ(2)=XYZ(2)+PVTX(2)
      XYZ(3)=XYZ(3)+PVTX(3)
C
      RETURN
      END
#endif
