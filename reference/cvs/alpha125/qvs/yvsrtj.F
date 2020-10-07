      SUBROUTINE YVSRTJ(DJET,XYZ,XYZR)
CKEY  QVSRCH / INTERNAL
C ------------------------------------------------------------------------------
C! Rotates vector XYZ into DJET direction
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Arguments :
C  *  DJET() IS NORMALIZED DIRECTION VECTOR
C  *  XYZ() IS VECTOR TO BE TRANSFORMED
C  Output Argument :
C  *  XYZR() IS VECTOR IN NEW SYSTEM
C       XYZR(3) COMPONENT IS ALONG DJET DIRECTION
C       CAN BE SAME FORTRAN ARRAY AS XYZ
C
C ------------------------------------------------------------------------------
#ifndef DOC
      DIMENSION DJET(3),XYZ(3),XYZR(3)
      DIMENSION ROT(3,3)
      REAL*8 T1,T2,T3
C ------------------------------------------------------------------------------
C
C MAKE FORWARD ROTATION MATRIX
      CALL YVSROM(DJET,ROT)
C
C COPY THE INPUT (IN CASE IT'S SAME AS OUTPUT)
      T1=XYZ(1)
      T2=XYZ(2)
      T3=XYZ(3)

C ROTATE THE VECTOR
C VECTOR ON RIGHT OF MATRIX
C OUTPUT SUBSCRIPT SAME AS FIRST MATRIX SUBSCRIPT
      XYZR(1)=ROT(1,1)*T1+ROT(1,2)*T2+ROT(1,3)*T3
      XYZR(2)=ROT(2,1)*T1+ROT(2,2)*T2+ROT(2,3)*T3
      XYZR(3)=ROT(3,1)*T1+ROT(3,2)*T2+ROT(3,3)*T3
C
      RETURN
      END
#endif
