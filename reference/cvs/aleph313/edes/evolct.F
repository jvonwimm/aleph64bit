      SUBROUTINE EVOLCT(VOLNAM,LEPLAN,PLANES,LECON,XCONTR,YCONTR,ZCONTR)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME CONTOUR / USER
C     H.Videau      creation 15/06/87   modification 15/06/87
C! Computes volume contour.
C   Input :
C           VOLNAM: name of the volume
C           LEPLAN: number of planes
C           PLANES: planes coefficients
C   Output:
C           XCONTR:  X of the point of the contour
C           YCONTR:  y of the point of the contour
C           ZCONTR:  z of the point of the contour
C   Computes the LEPOIN corners of a volume with name VOLNAM from the
C   LEPLAN planes which have been found previously by using EVOLPL
C   or from a storage and stores their coordinates in the arrays XCONTR

C  YCONTR, ZCONTR in such a way that the contour of the storey can be
C  drawn in one go.The number of points is LECON.
C  This ugly definition is adopted to conform to the fortran binding
C  of GKS.
C  The ordering of the points is given by the relationship ECNC.
C   Calls: VECT4
C   Called by USER
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Variables d'input
        CHARACTER*16 VOLNAM
        INTEGER LEPLAN
        REAL PLANES(ECALLS,*)
C     Variables d'output
        INTEGER LECON
        REAL XCONTR(*),YCONTR(*),ZCONTR(*)
C     Locales
        INTEGER I,J,IDEVOL,ECECRP,LEPOIN
        REAL CORNER(ECALLS,20),FNOR
C
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
      LEPOIN=0
      DO 4 I=1,NECRP
      IF(ECRPET(I).NE.EVOLET(IDEVOL)) GO TO 4
        LEPOIN=LEPOIN+1
        ECECRP=ECRPEC(I)
        CALL VECT4(CORNER(1,ECECRP),PLANES(1,ECRPP1(I)),
     &             PLANES(1,ECRPP2(I)),PLANES(1,ECRPP3(I)))
C    test sur corpro(4) et le cas echeant division pour normer le point
        IF(CORNER(4,ECECRP).NE.0.) THEN
                FNOR=1./CORNER(4,ECECRP)
                DO 2 J=1,ECALLS-1
                CORNER(J,ECECRP)=FNOR*CORNER(J,ECECRP)
 2              CONTINUE
        END IF
 4      CONTINUE
C
C  We put the points in the contour list
        LECON=0
        DO 10 I=1,NECNC
              IF(ECNCET(I).NE.EVOLET(IDEVOL))   GO TO 10
              LECON=LECON+1
              XCONTR(ECNCEQ(I)) = CORNER(1,ECNCEC(I))
              YCONTR(ECNCEQ(I)) = CORNER(2,ECNCEC(I))
              ZCONTR(ECNCEQ(I)) = CORNER(3,ECNCEC(I))
 10      CONTINUE

C
C   Close
 999    CONTINUE
C
C
      END
#endif
