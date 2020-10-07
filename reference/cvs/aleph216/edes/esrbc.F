      SUBROUTINE ESRBC(SYSTM,RW,CL,ST,POINT)
C-----------------------------------------------------------------
CKEY ECALDES STOREY BARYCENTRE / USER
C     H.Videau      Creation 26/10/86   modification 08/02/88
C! Computes storey "barycentre"
C  Computes the barycentre of a storey (RW,CL,ST) in the system 'SYSTM'.
C  It first gets the limiting planes, then computes the intersection
C  of the bissector planes for each family. The relation plane>family
C  is defined by the FMtoPL table.
C   Input :
C           SYSTM   coordinate system name ('ALEPH' for example)
C           RW      row index of storey      integer
C           CL      column index of storey   integer
C           ST      stack #                  integer
C   Output:
C           POINT   3 coordinates of barycentre in SYSTM ref.system
C   Calls: ESRPL, VECT4
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Variables d'input
        CHARACTER*5 SYSTM
        INTEGER RW,CL,ST
C     Variables d'output
        REAL POINT(ECALLS-1)
C     Locales
        INTEGER LEFMPL
        PARAMETER (LEFMPL=3)
        INTEGER I,J,LEPLAN
        REAL PLANES(ECALLS,6),FNOR,BIPLAN(ECALLS,LEFMPL),POIPRO(ECALLS)
C     A VOIR
C    Relation FAMILY>PLAN
        INTEGER FMTOPL (2,LEFMPL)
        DATA FMTOPL/1,2,3,4,5,6/
C
        CALL ESRPL(SYSTM,RW,CL,ST,LEPLAN,PLANES)
        DO 1 I=1,LEFMPL
        DO 2 J=1,ECALLS
        BIPLAN(J,I)=PLANES(J,FMTOPL(1,I))-PLANES(J,FMTOPL(2,I))
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
