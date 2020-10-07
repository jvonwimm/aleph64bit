C!======================================================================
      SUBROUTINE XMOMI(ZIT,LAMDA)
C!======================================================================
#ifndef DOC
#include "qcde.h"
      INTEGER I,J,KK,M
      REAL LAMDA(3),ZIT(3,3)
      REAL*8 LAMDAA,LAMDAB,LAMDAC,XTWOPI,FOURPI,Q,R,
     >E2OVE3,E1OVE3,AOVER3,SQRTQ,A,B,C,THETA,THIRDR
#include "qmacro.h"
      XTWOPI  =  6.28318531
      FOURPI  = 12.56637061
C!======================================================================
C!. ON DIAGONALISE LA MATRICE ZIT(I,J) .
C!. A,B,C SONT LES CONSTANTES  DU TRINOMIAL.
      A=ZIT(1,1)+ZIT(2,2)+ZIT(3,3)
      A=-A
      B=ZIT(1,1)*ZIT(2,2)+ZIT(1,1)*ZIT(3,3)+ZIT(2,2)*ZIT(3,3)
     >-ZIT(1,3)*ZIT(3,1)-ZIT(1,2)*ZIT(2,1)-ZIT(2,3)*ZIT(2,3)
      C=ZIT(1,1)*ZIT(2,2)*ZIT(3,3)+ZIT(1,2)*ZIT(2,3)*ZIT(3,1)
     >+ZIT(1,3)*ZIT(2,1)*ZIT(3,2)-ZIT(3,1)*ZIT(2,2)*ZIT(1,3)
     >-ZIT(1,1)*ZIT(3,2)*ZIT(2,3)-ZIT(3,3)*ZIT(2,1)*ZIT(1,2)
      C=-C
C!.ON VERIFIE QU'IL Y A TROIS SOLUTIONS REELLES .
C!.(REF. NUMERICAL RECIPES BY PRESS ET AL, P. 146.)
      Q=(A**2-3.*B)/9.
      R=(2.*A**3-9.*A*B+27.*C)/54.
      AOVER3=A/3.
      SQRTQ=SQRT(Q)
      IF(Q**3.GE.R**2) THEN
         IF(SQRTQ**3.GT.ABS(R)) THEN
               THETA=ACOS(R/SQRTQ**3)
         ELSE
               IF(R.LT.0.) THEN
                     THETA = ACOS(-1.)
                ELSE
                     THETA = ACOS(1.)
               ENDIF
         ENDIF
         LAMDAA=-2.*SQRTQ*COS(THETA/3.)-AOVER3
         LAMDAB=-2.*SQRTQ*COS((THETA+XTWOPI)/3.)-AOVER3
         LAMDAC=-2.*SQRTQ*COS((THETA+FOURPI)/3.)-AOVER3
         IF(LAMDAA.LT.LAMDAB.AND.LAMDAA.LT.LAMDAC)  THEN
             LAMDA(1)=LAMDAA
             IF(LAMDAB.LT.LAMDAC) THEN
                LAMDA(2)=LAMDAB
                LAMDA(3)=LAMDAC
             ELSE
                LAMDA(3)=LAMDAB
                LAMDA(2)=LAMDAC
             ENDIF
         ENDIF
         IF(LAMDAB.LT.LAMDAA.AND.LAMDAB.LT.LAMDAC)  THEN
             LAMDA(1)=LAMDAB
             IF(LAMDAA.LT.LAMDAC) THEN
                LAMDA(2)=LAMDAA
                LAMDA(3)=LAMDAC
             ELSE
                LAMDA(3)=LAMDAA
                LAMDA(2)=LAMDAC
             ENDIF
         ENDIF
         IF(LAMDAC.LT.LAMDAB.AND.LAMDAC.LT.LAMDAA)  THEN
             LAMDA(1)=LAMDAC
             IF(LAMDAB.LT.LAMDAA) THEN
                LAMDA(2)=LAMDAB
                LAMDA(3)=LAMDAA
             ELSE
                LAMDA(3)=LAMDAB
                LAMDA(2)=LAMDAA
             ENDIF
         ENDIF
      ELSE
         THIRDR=(SQRT(R**2-Q**3)+ABS(R))
         THIRDR=LOG(THIRDR)/3.
         THIRDR=EXP(THIRDR)
         IF(R.GT.0.) THEN
           LAMDA(3)=-(THIRDR+Q/THIRDR)-AOVER3
         ELSE
           LAMDA(3)=(THIRDR+Q/THIRDR)-AOVER3
         ENDIF
         LAMDA(1)=0.
         LAMDA(2)=LAMDA(3)
      ENDIF
      RETURN
      END
*AF
#endif
