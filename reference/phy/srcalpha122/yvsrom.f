      SUBROUTINE YVSROM(DIR,ROT)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C! Makes forward rotation matrix
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C
C  Input Argument :
C     DIR() IS NORMALIZED DIRECTION VECTOR
C  Output Argument :
C     ROT(,) IS MATRIX THAT ROTATES DIR() TO DIR'=(0,0,+1)
C        DIR'(I)= ROT(I,J)*DIR(J), SUMMED OVER J
C
C ----------------------------------------------------------------------
      DIMENSION DIR(3),ROT(3,3)
C ----------------------------------------------------------------------
      T=SQRT(DIR(1)**2+DIR(2)**2)
      IF (T .NE. 0.) THEN
        RT=1./T
        ROT(1,1)= DIR(2)*RT
        ROT(1,2)=-DIR(1)*RT
        ROT(1,3)=0.
        ROT(2,1)= DIR(1)*RT*DIR(3)
        ROT(2,2)= DIR(2)*RT*DIR(3)
        ROT(2,3)=-T
        ROT(3,1)= DIR(1)
        ROT(3,2)= DIR(2)
        ROT(3,3)= DIR(3)
      ELSE
        CALL UZERO(ROT,1,9)
        ROT(1,1)=1.
        ROT(2,2)=DIR(3)
        ROT(3,3)=DIR(3)
      ENDIF
      RETURN
      END
