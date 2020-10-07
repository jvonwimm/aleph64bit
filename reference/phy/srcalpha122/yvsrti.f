      SUBROUTINE YVSRTI(DJET,XYZR,XYZ)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Inverse-rotates vector XYZ from DJET direction
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  DJET() IS NORMALIZED DIRECTION VECTOR
C  *  XYZR() IS VECTOR TO BE TRANSFORMED
C       XYZR(3) COMPONENT IS ALONG DJET DIRECTION
C  Output Argument :
C  *  XYZ() IS VECTOR IN NEW SYSTEM
C       CAN BE SAME FORTRAN ARRAY AS XYZR
C
C ----------------------------------------------------------------------
      DIMENSION DJET(3),XYZR(3),XYZ(3)
      DIMENSION ROT(3,3)
      REAL*8 T1,T2,T3
C ----------------------------------------------------------------------
C
C MAKE FORWARD ROTATION MATRIX
      CALL YVSROM(DJET,ROT)
C
C COPY THE INPUT (IN CASE IT'S SAME AS OUTPUT)
      T1=XYZR(1)
      T2=XYZR(2)
      T3=XYZR(3)

C ROTATE THE VECTOR
C TRANSPOSE MATRIX ON RIGHT
C OUTPUT SUBSCRIPT SAME AS LAST MATRIX SUBSCRIPT
      XYZ(1)=T1*ROT(1,1)+T2*ROT(2,1)+T3*ROT(3,1)
      XYZ(2)=T1*ROT(1,2)+T2*ROT(2,2)+T3*ROT(3,2)
      XYZ(3)=T1*ROT(1,3)+T2*ROT(2,3)+T3*ROT(3,3)
C
      RETURN
      END
