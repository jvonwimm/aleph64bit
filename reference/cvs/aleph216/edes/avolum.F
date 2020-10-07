      REAL FUNCTION AVOLUM(LEFACE,PLANES,PTRFCE,
     &     LEPOIN,XFACE,YFACE,ZFACE)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME / USER
C     H.Videau      creation le 13/02/88
C! Computes volumes volume
C  Computes the volume of a convex polyhedron.
C  The LEPOIN points defining  the contour of the faces in
C  their LEFACE planes are stored in XFACE,YFACE, ZFACE.
C  PTRFCE contains LEFACE pointers on these arrays to the
C  starting point of each face.
C     This ugly definition is adopted to conform to the fortran binding
C  of GKS for a fill area set.
C    The ordering of the corners to build faces is such that the points
C  go in the trigonometric sense around the normale to the plane.
C   Input :
C           LEFACE  number of faces
C           PLANES  4 coefficients of the planes of the faces
C           PTRFCE:  pointer
C           LEPOIN:  number of points
C           XFACE :  X of the point of the face
C           YFACE :  Y of the point of the face
C           ZFACE :  Z of the point of the face
C   Output:
C           AVOLUM volume
C   Calls: none
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C
C  Input variables
      INTEGER LEFACE,LEPOIN,PTRFCE(*)
      REAL PLANES(4,*),XFACE(*),YFACE(*),ZFACE(*)
C  Locales
      INTEGER I,J,K
      REAL VECT(3),VOLU
C
      VOLU = 0
      DO 3 K=1,LEFACE
      DO 2 I=1,3
      VECT(I)=0.
 2    CONTINUE
      DO 1 I=PTRFCE(K),PTRFCE(K+1)-1
      J=I+1
      IF(J.EQ.PTRFCE(K+1)) J=PTRFCE(K)
      VECT(1)=VECT(1)+YFACE(I)*ZFACE(J)-YFACE(J)*ZFACE(I)
      VECT(2)=VECT(2)+ZFACE(I)*XFACE(J)-ZFACE(J)*XFACE(I)
      VECT(3)=VECT(3)+XFACE(I)*YFACE(J)-XFACE(J)*YFACE(I)
 1    CONTINUE
      VOLU=VOLU+(VECT(1)*PLANES(1,K)+VECT(2)*PLANES(2,K)+
     & VECT(3)*PLANES(3,K))*PLANES(4,K)
 3    CONTINUE
      AVOLUM=VOLU/6.
      END
#endif
