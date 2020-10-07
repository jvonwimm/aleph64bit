         SUBROUTINE ECYLND(RCYL,ZCYL,POINT,VECT,IMPACT)
C--------------------------------------------------------------
CKEY ECALDES NEUTRAL IMPACT CYLINDER / USER
C     H.Videau      Creation 10/12/88
C! Impact of a neutral on a cylinder.
C  This routine computes the impact of a sliding vector on a cylinder
C  de revolution around oz with radius RCYL and half-height ZCYL
C   Input :
C           RCYL radius                           REAL
C           ZCYL half_height                      REAL
C           POINT origine of the sliding vector   REAL(3)
C           VECT  vector                          REAL(3)
C   Output:
C           IMPACT point of impact                REAL(3)
C   Calls: none
C   Called by USER ,EPERCE
C---------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C   Input
         REAL RCYL,ZCYL,VECT(3),POINT(3)
C  Output
         REAL IMPACT(3)
C Locales
         REAL L(4),A,B,C,DISCR,LMIN
         INTEGER I
C Execution
C Impact dans les bouchons ,in the end caps
      IF(VECT(3).EQ.0.) THEN
         L(1)=999.
         L(2)=999.
                        ELSE
         L(1)=(ZCYL-POINT(3))/VECT(3)
         L(2)=(-ZCYL-POINT(3))/VECT(3)
                        END IF
C   Impact dans le cylindre , in the cylinder
         A=VECT(1)**2+VECT(2)**2
        IF(A.EQ.0.) THEN
         L(3)=999.
         L(4)=999.
                     ELSE
         B=POINT(1)*VECT(1)+POINT(2)*VECT(2)
         C= POINT(1)**2+POINT(2)**2-RCYL**2
         DISCR=B**2-A*C
         IF(DISCR.GT.0.) THEN
         DISCR=SQRT(DISCR)
         L(3)=(-B+DISCR)/A
         L(4)=(-B-DISCR)/A
                         END IF
                    END IF
C choix du plus letit l positif , get smaller positive l
         LMIN=990.
         DO 1 I=1,4
         IF(L(I).LT.0.) GO TO 1
         IF(L(I).LT.LMIN) LMIN=L(I)
 1       CONTINUE
         IMPACT(1)=POINT(1)+LMIN*VECT(1)
         IMPACT(2)=POINT(2)+LMIN*VECT(2)
         IMPACT(3)=POINT(3)+LMIN*VECT(3)
         END
#endif
