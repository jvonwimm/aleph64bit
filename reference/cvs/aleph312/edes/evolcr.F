      SUBROUTINE EVOLCR(VOLNAM,LEPLAN,PLANES,LEPOIN,CORNER)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME CORNER PLANE / USER
C     H.Videau      creation 28/04/87   modification 8/04/87
C! Computes volume corners from the planes.
C  Computes the corners of a volume named VOLNAM for which the limiting
C  planes are given in PLANES. There are LEPLAN planes and LEPOIN corne
C  It computes the corners as intersection of the three planes related
C  to that corner by the relationships ECRP to EFTY named
C  ECRPP1, ECRPP2, ECRPP3.
C   Input :
C           VOLNAM   Name of the volume             character*16
C           LEPLAN   Number of planes               integer
C           PLANES   4 coefficients of the planes   real
C   Output:
C           LEPOIN  Number of corners computed
C           CORNER  3 coordinates of corners in SYSTEM ref. system
C   Calls: VECT4
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Variables d'input
        CHARACTER*16 VOLNAM
        INTEGER LEPLAN
        REAL PLANES(ECALLS,*)
C     Variables d'output
        INTEGER LEPOIN
        REAL CORNER(ECALLS-1,*)
C     Locales
        INTEGER I,J,IDEVOL,ECECRP
        REAL CORPRO(ECALLS,20),FNOR
C
      LEPOIN=0
C  Get the volume from VOLNAM
      DO 1 I=1,NEVOL
        IF(VOLNAM.EQ.EVOLNM(I)) GO TO 3
 1    CONTINUE
C  Erreur
      GO TO 999
 3    CONTINUE
      IDEVOL=EVOLID(I)
C
C    Loop over the corners by type
      DO 4 I=1,NECRP
      IF(ECRPET(I).NE.EVOLET(IDEVOL)) GO TO 4
        LEPOIN=LEPOIN+1
        ECECRP=ECRPEC(I)
        CALL VECT4(CORPRO(1,ECECRP),PLANES(1,ECRPP1(I)),
     &             PLANES(1,ECRPP2(I)),PLANES(1,ECRPP3(I)))
C    test sur corpro(4) et le cas echeant division pour normer le point
        IF(CORPRO(4,ECECRP).NE.0.) THEN
                FNOR=1./CORPRO(4,ECECRP)
                DO 2 J=1,ECALLS-1
                CORNER(J,ECECRP)=FNOR*CORPRO(J,ECECRP)
 2              CONTINUE
        END IF
 4      CONTINUE
C
C   Close
 999    CONTINUE
        END
#endif
