      SUBROUTINE EGPTLG(PTLG)
C.----------------------------------------------------------------------
CKEY ECALDES PLANE LIMITS ROW / INTERNAL
C     H.Videau      Creation 15/12/85   Modification 11/03/90
C! Gen. points on row limit planes
C  Generates points situated on the planes delimiting the rows
C  ~theta = constant.  This is done for the module types in their
C  construction position. The points are in the y,z plane.
C The theoretical values are then calculated to take care of the
C reduced thickness of the petals: the end cap points are trans-
C ported to the back of the modules.
C   Calls: EGTLNF
C   Called by EDEFLG (Internal)
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Locales
      INTEGER I,J,RW,A,BARIL,SC
      PARAMETER (BARIL=2)
      REAL E1,E2,C,CSPIDZ,X,PLAN(ECALLS),PTRW(ECALLS,ECALRW+1)
      REAL PLLIZ(ECALSC)
C    Output variables
      REAL PTLG(ECALLS,*)
C   Inline function
      REAL ARGSH
      ARGSH(X)=ALOG(X+SQRT(X**2+1.))
C
C Les coordonnees ainsi determinees sont fausses d'a peu pres O,2 mm
C comparees aux valeurs de sciage pour le baril.
C
      CALL EGTLNF('Cap int z sup',PLAN)
      PLLIZ(1)= PLAN(4)
      PLLIZ(3)=-PLAN(4)
      PLLIZ(2)=PLLIZ(1)
      CSPIDZ=COS(ECALAP/2.)
      A= (ESCOLS(2)-ESCOFR(2))/2
      CALL EGTLNF('Bar int R inf   ',PLAN)
      E1=ARGSH(0.5*ECALBL/ECALBO)/FLOAT(A)
      C =ARGSH(ECALEI*CSPIDZ/ECALEL)
      E2=(ARGSH(ECALBO/(0.5*ECALBL))-C)/FLOAT(ECALPR)
      RW=0
      DO 2023 I=0,ECALPR-1
        RW=RW+1
        PTRW(1,RW)=0.
        PTRW(2,RW)=-ECALEW*SINH(FLOAT(I)*E2+C)
        PTRW(3,RW)=ECALEW
        PTRW(4,RW)=1.
 2023 CONTINUE
      DO 2022 I=A,-A,-1
        RW=RW+1
        PTRW(1,RW)=0.
        PTRW(2,RW)=-ECALBO
        PTRW(3,RW)=ECALBO*SINH(FLOAT(I)*E1)
        PTRW(4,RW)=1.
 2022 CONTINUE
      DO 2024 I=ECALPR-1,0,-1
        RW=RW+1
        PTRW(1,RW)=0.
        PTRW(2,RW)=-ECALEW*SINH(FLOAT(I)*E2+C)
        PTRW(3,RW)=-ECALEW
        PTRW(4,RW)=1.
 2024 CONTINUE
C
C transfert des points from physical row to soft row
      DO 1 I=1,ECALXW
        SC=EXROES(I)
        DO 2 J=1,ECALLS
          PTLG(J,I+SC-1)=PTRW(J,EXROER(I))
 2      CONTINUE
 1    CONTINUE
C   Completons l'end cap A ,B et le baril
      DO 3 J=1,ECALLS
        PTLG(J,51)=PTLG(J,57)
        PTLG(J,ECALXW+3)=PTRW(J,ECALRW+1)
        PTLG(J,180)=PTLG(J,186)
 3    CONTINUE
C   Achtung! ceci est une correction pour prendre en compte les effets
C   au bout du baril du calorimetre dus a la derive des electrons
C   perpendiculaire aux fils.
      PTLG(2,52)=PLAN(4)
      PTLG(2,180)=PLAN(4)
C   dans les endcaps on passe au plan arriere pour gerer l'erreur
C    de projectivite due a leur epaisseur.
      DO 4 I=1,ECALXW+3
        IF(I.LE.51)THEN
          SC=1
          PTLG(1,I)=PTLG(1,I)/PTLG(3,I)*PLLIZ(SC)
          PTLG(2,I)=PTLG(2,I)/PTLG(3,I)*PLLIZ(SC)
          PTLG(3,I)=PLLIZ(SC)
        ENDIF
        IF(I.GE.181)THEN
          SC=3
          PTLG(1,I)=PTLG(1,I)/PTLG(3,I)*PLLIZ(SC)
          PTLG(2,I)=PTLG(2,I)/PTLG(3,I)*PLLIZ(SC)
          PTLG(3,I)=PLLIZ(SC)
        ENDIF
 4    CONTINUE
      END
#endif
