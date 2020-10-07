      REAL FUNCTION AIRE(PLANE,LEPOIN,XFACE,YFACE,ZFACE)
C.----------------------------------------------------------------------
CKEY ECALDES POLYGON AREA / USER
C     H.Videau      creation le 13/02/88
C! Computes the area of a polygon on plane
C  Computes the area of a convex polygon. The LEPOIN corners belong to a
C  plane with coefficients PLANE. The sign of the area depends on the
C  position of the origin with regard to the normal of the plane.
C   Input :
C           PLANE   4 coefficients of the plane
C           LEPOIN  Number of corners
C           XFACE,YFACE,ZFACE  3 coordinates of corners
C   Output:
C           AIRE
C   Calls : none
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C
C  Input variables
      INTEGER LEPOIN
      REAL PLANE(4),XFACE(*),YFACE(*),ZFACE(*)
C  Locales
      INTEGER I,J
      REAL VECT(3)
C
      DO 2 I=1,3
      VECT(I)=0.
 2    CONTINUE
      DO 1 I=1,LEPOIN
      J=I+1
      IF(I.EQ.LEPOIN)J=1
      VECT(1)=VECT(1)+YFACE(I)*ZFACE(J)-YFACE(J)*ZFACE(I)
      VECT(2)=VECT(2)+ZFACE(I)*XFACE(J)-ZFACE(J)*XFACE(I)
      VECT(3)=VECT(3)+XFACE(I)*YFACE(J)-XFACE(J)*YFACE(I)
 1    CONTINUE
      AIRE=.5*(VECT(1)*PLANE(1)+VECT(2)*PLANE(2)+VECT(3)*PLANE(3))
      END
#endif
