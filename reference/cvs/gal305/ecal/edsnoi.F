*DK edsnoi
      FUNCTION EDSNOI(ISTK,JPHI,KTET)
C----------------------------------------------------------------------
C     O.CALLOT  29-NOV-85
C!   Modified :- E. Lancon              3-DEC-1990
C! Compute noise on storeys
C. Initialise array SIGMN on first call
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "edpara.h"
      DIMENSION SIGMN(3,114)
      DIMENSION CAPA0(3)
C
C?   Normalize noise to data
C
      DIMENSION XNORM (3,2)
      DATA (XNORM(I,1),I=1,3)   / 0.86, 1.42, 1.27 /
      DATA (XNORM(I,2),I=1,3)   / 0.61, 0.98, 0.95 /
      SAVE  NCALL,SIGMN
      DATA  CAPA0  /  2.  ,  3.  ,  2.  /
      DATA  NCALL  / 0 /
C
#include "bmacro.h"
C
C  ===  definition of the function ArgsinH
      ASINH(X) = ALOG( X + SQRT( 1. + X*X) )
C ----------------------------------------------------------------------
      IF(NCALL.NE.0) GO TO 100
C     if ENNO bank exists then set XNORM from there
      JENNO = IW(NAMIND('ENNO'))
      IF (JENNO .GT. 0) THEN
         DO IREG=1,2
            DO ISAMP=1,3
               XNORM(ISAMP,IREG) = RTABL (JENNO,IREG,ISAMP+1)
            ENDDO
         ENDDO
      ENDIF
C
      ZMAX = 2315.
      YMAX = 1871.
      EPSI = YMAX/64. * ASINH(ZMAX/YMAX)
      DO 10 I=1,64
        SIGMN(1,115-I) = COSH(FLOAT(I)*EPSI/YMAX)
   10 CONTINUE
      ZE = 2550.
      ZF = 2946.
      Y0 =  565.
      PIS12 = ACOS(-1.) / 12.
      D = ASINH(Y0/ZF * PIS12 )
      ETA = ZE/45. * ( ASINH(YMAX/ZMAX) - D )
      COEF = 8. * ZE/YMAX * ETA/EPSI
      ROWN = 4.
      DO 20 I=1,50
        SIGMN(1,I) = COEF/ROWN * SINH(2.*(FLOAT(I)*ETA/ZE + D ) )
        IF(I.EQ.8) ROWN = 8.
        IF(I.EQ.24) ROWN = 12.
        IF(I.EQ.40) ROWN = 16.
        IF(I.EQ.45) COEF = COEF * EPSI/ETA * ZE/ZMAX
   20 CONTINUE
C
      IREG = 1
      DO 30 I=1,114
        IF (I.GE.51) IREG = 2
        DEP = SIGMN(1,I)
        DO 40 J=1,3
          CAPA = DEP * CAPA0(J)
          SIGMN(J,I) = SQRT( 308.*CAPA**2 + 665. ) * EDTNOI(J)
          SIGMN(J,I) = SIGMN(J,I) * XNORM(J,IREG)
   40   CONTINUE
   30 CONTINUE
      NCALL = 1
C
  100 CONTINUE
      IF(KTET.LE.114) THEN
        SIG = SIGMN(ISTK,KTET)
      ELSE
        SIG = SIGMN(ISTK,229-KTET)
      ENDIF
      EDSNOI = SIG
      RETURN
      END
#endif
