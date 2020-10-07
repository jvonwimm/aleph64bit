      SUBROUTINE EVOLFC(VOLNAM,LEFACE,PLANES,PTRFCE,
     &     LEPOIN,XFACE,YFACE,ZFACE)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME FACE / USER
C     H.Videau      creation 16/06/87   modification 16/06/87
C! Computes volume faces
C   Input :
C           VOLNAM:  name of the volume
C           LEFACE:  number of faces. (commonly 6 or 8)
C           PLANES:  coefficients of the planes
C   Output:
C           PTRFCE:  pointer          (LEFACE+1)
C           LEPOIN:  number of points
C           XFACE :  X of the point of the face
C           YFACE :  Y of the point of the face
C           ZFACE :  Z of the point of the face
C    Computes the faces of a volume named VOLNAM given its limiting
C   PLANES. . The LEPOIN points defining  the contour of the faces in
C   their planes are stored in XFACE,YFACE, ZFACE.
C   PTRFCE contains LEFACE pointers on these arrays to the
C   starting point of each face.
C     This ugly definition is adopted to conform to the fortran binding
C   of GKS for a fill area set.
C   The ordering of the corners to build faces is such that the points
C   go in the trigonometric sense around the normale to the plane.
C   Calls: VECT4
C   Called by USER.
C-----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Variables d'input
        INTEGER LEFACE
        CHARACTER*16 VOLNAM
        REAL PLANES(ECALLS,*)
C     Variables d'output
        INTEGER LEPOIN
        INTEGER PTRFCE(*)
        REAL XFACE(*), YFACE(*), ZFACE(*)
C     Locales
        INTEGER I,J,IDEVOL,LECOIN,ECECRP
        PARAMETER (LECOIN=20)
        REAL FNOR,CORNER(ECALLS,LECOIN)
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
      DO 4 I=1,NECRP
      IF(ECRPET(I).NE.EVOLET(IDEVOL)) GO TO 4
        ECECRP=ECRPEC(I)
        CALL VECT4(CORNER(1,ECECRP),PLANES(1,ECRPP1(I)),
     &             PLANES(1,ECRPP2(I)),PLANES(1,ECRPP3(I)))
C    test sur corner(4) et le cas echeant division pour normer le point
        IF(CORNER(4,ECECRP).NE.0.) THEN
                FNOR=1./CORNER(4,ECECRP)
                DO 2 J=1,ECALLS-1
                CORNER(J,ECECRP)=FNOR*CORNER(J,ECECRP)
 2              CONTINUE
        END IF
 4      CONTINUE
C Mise en place des points de la face
        LEPOIN=0
        DO 10 I=1,NEFAC
      IF(EFACET(I).NE.EVOLET(IDEVOL)) GO TO 10
              LEPOIN=LEPOIN+1
              XFACE(EFACEF(I)) = CORNER(1,EFACEC(I))
              YFACE(EFACEF(I)) = CORNER(2,EFACEC(I))
              ZFACE(EFACEF(I)) = CORNER(3,EFACEC(I))
 10     CONTINUE
C Mise en place des pointeurs   A VOIR
        DO 11 I=1,NELTY
      IF(ELTYET(I).NE.EVOLET(IDEVOL)) GO TO 11
      PTRFCE(ELTYEA(I)) = ELTYEF(I)
 11     CONTINUE
      PTRFCE(LEFACE+1)=LEPOIN+1
C
C   Close
 999    CONTINUE
      END
#endif
