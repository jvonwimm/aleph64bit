      SUBROUTINE ESRFC(SYSTM,RW,CL,ST,LEFACE,PLANES,PTRFCE,
     &     LEPOIN,XFACE,YFACE,ZFACE)
C-----------------------------------------------------------------
CKEY ECALDES STOREY FACES / USER
C     H.Videau      Creation 06/03/87   modification 08/02/88
C! Computes storey faces
C  Computes the faces of a storey given by its row, column and stack
C  numbers ( RW, CL, ST). The coordinates are given in the frame
C  'SYSTM'. We have LEFACE (<=8) faces, the coordinates of the face
C  planes are given in PLANES(LSP,LEFACE). The LEPOIN points defining
C  the contour of the faces in their planes are stored in XFACE,YFACE,
C  ZFACE. PTRFCE contains LEFACE pointers on these arrays to the
C  starting point of each face.
C     This ugly definition is adopted to conform to the fortran binding
C  of GKS for a fill area set.Please read GKS notice if not done!
C  The planes limiting the storey are first computed, then the corners
C  The points inside the storey (convex) have a positive power with
C  respect to the plan equations except pathologic cases.
C    The ordering of the corners to build faces is given by the relation
C  EFAC according to the type. It is such that the points go in the
C  trigonometric sense around the normale to the plane.
C   Input :
C           SYSTM              character*5
C           RW,CL,ST           integers
C   Output:
C           LEFACE             integer
C           PLANES(4,LEFACE)   real
C           PTRFCE(LEFACE)     integer
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
        INTEGER LEPOIN, LEFACE
        INTEGER PTRFCE(*)
        REAL PLANES(ECALLS,*)
        REAL XFACE(*), YFACE(*),ZFACE(*)
C     Locales
        INTEGER I,J, LECOIN,TYPE
        PARAMETER (LECOIN=20)
        REAL FNOR,CORNER(ECALLS-1,LECOIN),CORPRO(ECALLS)
C
C Etablir le type A VOIR
        TYPE = 1
        CALL ESRPL(SYSTM,RW,CL,ST,LEFACE,PLANES)
C
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
      LEPOIN=0
        DO 10 I=ETYVFF(TYPE),ETYVLF(TYPE)
      LEPOIN=LEPOIN+1
              XFACE(EFACEF(I)) = CORNER(1,EFACEC(I))
              YFACE(EFACEF(I)) = CORNER(2,EFACEC(I))
              ZFACE(EFACEF(I)) = CORNER(3,EFACEC(I))
 10     CONTINUE
C Mise en place des pointeurs
        DO 11 I=1,NELTY
      IF(ELTYET(I).NE.TYPE) GO TO 11
      PTRFCE(ELTYEA(I)) = ELTYEF(I)
 11     CONTINUE
      PTRFCE(LEFACE+1)=LEPOIN+1
C
        END
#endif
