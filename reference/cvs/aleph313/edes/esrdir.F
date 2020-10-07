      SUBROUTINE ESRDIR(RW,CL,THETA,PHI)
C-----------------------------------------------------------------
CKEY ECALDES STOREY ADDRESS DIRECTION THETA PHI / USER
C     H.Videau      Creation 30/01/90           Modification 14/02/90
C! Real i,j gives theta,phi.
C  Computes the theta and phi of the direction of the tower designated
C by RW and CL interpolating the row and column numbers inside the tower
C  To do that we look for the tower defined by:
C     INT(RW) and INT(CL)) then the point is the intersection of the
C  planes linear combinations of the limiting planes of the tower with
C  weights  1.-FRACT(RW), FRACT(RW) and the same for CL.
C   It is done in the Aleph system
C Example: to get the direction of the centre of the tower
C (row=24,column=32) specify RW=24.5  and CL=32.5
C
C   Input :
C           RW    real :      row index of storey
C           CL    real :      column index of storey
C   Output:
C           THETA   REAL  radians
C           PHI     REAL  radians
C   Calls: CROSS, ENORME
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "alcons.h"
#include "ecccom.h"
C    Variables d'input
      REAL RW,CL
C     Variable d'output
      REAL THETA,PHI
C     Locales
      INTEGER LEFMPL,BARIL
      PARAMETER (LEFMPL=2,BARIL=2)
      INTEGER I,J,LEPLAN,IRW,IHRW,ICL,RG,SC,MD,SS,LOCCL,ITCL
      PARAMETER (LEPLAN=4)
      REAL PLANES(3,LEPLAN),BIPLAN(3,2),WEIGHT(2)
      REAL POIPRO(3),CT,L
      INTEGER CLINF,CLSUP,RWINF,RWSUP
      PARAMETER (CLINF=1,CLSUP=2,RWINF=3,RWSUP=4)
C    Relation FAMILY>PLAN
      INTEGER FMTOPL (2,LEFMPL)
      DATA FMTOPL/1,2,3,4/
C
      IRW= INT(RW)
      WEIGHT(2)= AMOD(RW,1.)
      ICL= INT(CL)
      WEIGHT(1)= AMOD(CL,1.)
      IHRW=EXROER(IRW)
      RG = EXRGER(EROWEX(IHRW))
      SC = EXROES(IRW)
      MD=FLOAT(ICL-1)/FLOAT(EREGCN(RG))-FLOAT(ESCOCF(SC))*.5+1.
C  Attention a l'offset dans les bouchons
      LOCCL=FLOAT(ICL)-
     &       (FLOAT(MD-1)+FLOAT(ESCOCF(SC))*.5)*FLOAT(EREGCN(RG))
C   INTRODUIRE LES BORDS DE MODULES  A VOIR
      IF(MD.EQ.13)MD=1
      ITCL=EREGFR(RG)+LOCCL-1
      IF(SC.NE.BARIL.AND.LOCCL.GT.EREGCN(RG)/2)THEN
        SS=2
      ELSE
        SS=1
      ENDIF
      DO 1 I=1,ECALLS-1
        PLANES(I,CLINF)=+ECOLPC(2,ITCL)    *EALICL(I,1,MD,SC)-
     &                   ECOLPC(1,ITCL)    *EALICL(I,2,MD,SC)
        PLANES(I,CLSUP)=-ECOLPC(2,ITCL+1)  *EALICL(I,1,MD,SC)+
     &                   ECOLPC(1,ITCL+1)  *EALICL(I,2,MD,SC)
        PLANES(I,RWINF)=+EXROPC(2,IRW+SC-1)*EALIRW(I,1,SS,MD,SC)-
     &                   EXROPC(1,IRW+SC-1)*EALIRW(I,2,SS,MD,SC)
        PLANES(I,RWSUP)=-EXROPC(2,IRW+SC)  *EALIRW(I,1,SS,MD,SC)+
     &                   EXROPC(1,IRW+SC)  *EALIRW(I,2,SS,MD,SC)
 1    CONTINUE
C    Normaliser les plans
      DO 3 I=1,LEPLAN
        CALL ENORME(PLANES(1,I))
 3    CONTINUE
      DO 4 I=1,LEFMPL
        DO 2 J=1,3
          BIPLAN(J,I)=(1.-WEIGHT(I)) * PLANES(J,FMTOPL(1,I))-
     &                    WEIGHT(I)  * PLANES(J,FMTOPL(2,I))
 2      CONTINUE
 4    CONTINUE
      CALL CROSS(BIPLAN(1,2),BIPLAN(1,1),POIPRO)
      L=SQRT(POIPRO(1)**2+POIPRO(2)**2+POIPRO(3)**2)
      IF(L.GT.0.) THEN
        CT=POIPRO(3)/L
        THETA=ACOS(CT)
        PHI=ATAN2(POIPRO(2),POIPRO(1))
        IF(PHI.LT.0.)PHI=PHI+TWOPI
      ELSE
        THETA=0.
        PHI=0.
      ENDIF
      END
#endif
