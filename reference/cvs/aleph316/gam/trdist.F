      FUNCTION TRDIST(Q,P)
C.----------------------------------------------------------------------
CKEY GAMPACK DISTANCE / INTERNAL
C   AUTHOR   : A. Rouge       3/01/90
C! Distance track/straight line
C
C   Input :
C           Q    Q(6) Line parameters: x0 y0 z0 cx cy cx         REAL
C                coordinates of a point and direction cosines
C
C           P    P(3)  Point coordinates : x y z                 REAL
C   Output:
C           Function = distance track / straight line
C   Calls: None
C   Called by GAMPEX
C.----------------------------------------------------------------------
#ifndef DOC
      DIMENSION Q(6),P(3),PP(3)
C
      DO  I = 1,3
        PP(I) = P(I)-Q(I)
      ENDDO
C
      PVX = Q(3+2)*PP(3) - Q(3+3)*PP(2)
      PVY = Q(3+3)*PP(1) - Q(3+1)*PP(3)
      PVZ = Q(3+1)*PP(2) - Q(3+2)*PP(1)
      TRD = SQRT( PVX**2 +PVY**2 + PVZ**2)
      TRDIST=TRD
      RETURN
      END
#endif
