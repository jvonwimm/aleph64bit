      SUBROUTINE ESRCR(SYSTM,RW,CL,ST,LEPOIN,CORNER)
C-------------------------------------------------------------------
CKEY ECALDES STOREY CORNERS / USER
C     H.Videau      Creation 26/10/86   modification 08/02/88
C! Computes storey corners
C  Computes the LEPOIN corners of a storey (RW,CL,ST) in the 'SYSTM'.
C  It first gets the limiting planes, then computes their intersection
C  3 by 3 to get the corners. The relation plane>corner is defined
C  by the type of the volume 'Storey'
C   Input :
C           SYSTM   coordinate system name     character*5
C                   ('ALEPH' for example)
C           RW      row index of storey        integer
C           CL      column index of storey     integer
C           ST      stack #                    integer
C   Output:
C           LEPOIN  Number of corners computed
C           CORNER  3 coordinates of corners in SYSTEM ref. system
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
        INTEGER LEPOIN
        REAL CORNER(ECALLS-1,*)
C     Locales
        INTEGER I,LEPLAN,J,TYPE
        REAL PLANES(ECALLS,8),FNOR,CORPRO(ECALLS)
C
C Etablir le type A VOIR
        TYPE = 1
        CALL ESRPL(SYSTM,RW,CL,ST,LEPLAN,PLANES)
        LEPOIN=ETYVCR(TYPE)
        DO 1 I=ETYVFR(TYPE),ETYVLR(TYPE)
        CALL VECT4(CORPRO(1),PLANES(1,ECRPP1(I)),
     &            PLANES(1,ECRPP2(I) ),PLANES(1,ECRPP3(I)))
C    test sur corpro(4) et le cas echeant division pour normer le point
         IF(CORPRO(4).NE.0.) THEN
                FNOR=1./CORPRO(4)
                DO 2 J=1,ECALLS-1
                CORNER(J,ECRPEC(I))=FNOR*CORPRO(J)
 2              CONTINUE
                              END IF
1      CONTINUE
        END
#endif
