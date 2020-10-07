      SUBROUTINE ESRPT(SYSTM,RW,CL,ST,POINT)
C-----------------------------------------------------------------
CKEY ECALDES STOREY ADDRESS POINT COORDINATES / USER
C     H.Videau      Creation 30/03/87   modification 08/02/88
C! Real i,j,k gives x,y,z.
C  Computes the POINT in space (x,y,z) corresponding to the values
C  RW,CL,ST interpolating the row,column and stack numbers.
C  To do that we look for the storey defined by:
C   INT(RW), INT(CL), INT(ST) then the point is the intersection of the
C  planes linear combinations of the limiting planes of the storeys with
C  weights  1.-FRACT(RW), FRACT(RW) and the same for CL AND ST.
C  It first gets the limiting planes, then computes the intersection
C  of the weighted planes for each family. The relation plane>family
C  is defined by the FMtoPL table.
C   Input :
C         SYSTM character*5 coordinate system name  'ALEPH' for example
C         RW    real :      row index of storey
C         CL    real :      column index of storey
C         ST    real :      stack #
C   Output:  POINT   3 coordinates in SYSTM reference system
C   Calls: ESRPL, VECT4
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Variables d'input
        CHARACTER*5 SYSTM
        REAL RW,CL,ST
C     Variable d'output
        REAL POINT(ECALLS-1)
C     Locales
        INTEGER LEFMPL
        PARAMETER (LEFMPL=3)
        INTEGER I,J,LEPLAN,IRW,ICL,IST
        REAL PLANES(ECALLS,6),FNOR,BIPLAN(ECALLS,LEFMPL),WEIGHT(LEFMPL)
        REAL POIPRO(ECALLS)
C    A VOIR
C    Relation FAMILY>PLAN
        INTEGER FMTOPL (2,LEFMPL)
        DATA FMTOPL/1,2,3,4,5,6/
C
        IRW= INT(RW)
        WEIGHT(3)= AMOD(RW,1.)
        ICL= INT(CL)
        WEIGHT(2)= AMOD(CL,1.)
        IST= INT(ST)
        WEIGHT(1)= AMOD(ST,1.)
        CALL ESRPL(SYSTM,IRW,ICL,IST,LEPLAN,PLANES)
        DO 1 I=1,LEFMPL
        DO 2 J=1,ECALLS
        BIPLAN(J,I)=(1.-WEIGHT(I)) * PLANES(J,FMTOPL(1,I))-
     &                  WEIGHT(I)  * PLANES(J,FMTOPL(2,I))
 2      CONTINUE
 1      CONTINUE
        CALL VECT4(POIPRO,BIPLAN(1,1),BIPLAN(1,2),BIPLAN(1,3))
C    test et le cas echeant division pour normer le point a t=1
        IF(POIPRO(4).NE.0.) THEN
                FNOR=1./POIPRO(4)
                DO 3 J=1,ECALLS-1
                POINT(J)=FNOR*POIPRO(J)
 3              CONTINUE
        END IF
        END
#endif
