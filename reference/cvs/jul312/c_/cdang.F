      SUBROUTINE CDANG(X1,X2,V1,ANGL,IFENC)
C***********************************************************************
C! Compute distance between two polygons  with respect to direction v1 *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   X1     : track number                                             *
C!   X2     : Table of EcObs                                           *
C!   V1     : Direction of reference                                   *
C!   ANGLE  : Output (radians)                                         *
C!   IFENC  : 1 if Hcal polygon is in endcap                           *
C!                                                                     *
C!  MODIFICATIONS :                                                    *
C!        P. COMAS (18-OCT-1994)                                       *
C!           Restrict COMN1 to [-1.,1.] to avoid precision problems    *
C***********************************************************************
#ifndef DOC
#include "rlunit.h"
      DIMENSION X1(3,*),X2(3,*),COSA(4,4),V1(*)
      DIMENSION Y1(3,2),Y2(3,2)
C
C?  make table of cosines of angles between all corners (2 by 2)
C?   with resp. to V1
C
      CALL COSLST(X1,X2,V1,COSA,4)
      COMN1=-2.
C
C?   Find the two closest corners
C
      DO 200 I1=1,4
      DO 200 I2=1,4
      IF(COSA(I1,I2).LE.COMN1)GO TO 200
      COMN1=COSA(I1,I2)
      IMIN1=I1
      IMIN2=I2
  200 CONTINUE
      COSMN=COMN1
      IF(COSMN.GT.1.)COSMN=0.99999
C
C?   Transform cosine into angle
C
      ANGL=ACOS(COSMN)
C
C?  if the angle is small, we look if the two faces,
C?     projected in the direction of v1,
C?      have a common point
C?   if yes, the distance is nil
C
      IF(ANGL.LT.0.5)THEN
         CALL CPTCOM(V1,X1,X2,IPTC)
            IF(IPTC.EQ.1)THEN
            ANGL=0.
            GO TO 999
         ENDIF
      ENDIF
C
C?  If the endcap is involved, we have to make bissections
C?  on the two ledges of each face which end on the
C?  closest corners
C
      IF(IFENC.EQ.1)THEN
         COMN2=-2.
         DO 300 I=1,3,2
         I1=IMIN1+I
         IF(I1.GT.4)I1=I1-4
         DO 300 J=1,3,2
         I2=IMIN2+J
         IF(I2.GT.4)I2=I2-4
         IF(COSA(I1,I2).LE.COMN2)GO TO 300
         COMN2=COSA(I1,I2)
         JMIN1=I1
         JMIN2=I2
  300    CONTINUE
         DO 320 ICOR=1,3
C
C?   here, we fill tables Y with the two closest corners we just found
C?   plus the closest other corner on each face
C
         Y1(ICOR,1)=X1(ICOR,IMIN1)
         Y1(ICOR,2)=X1(ICOR,JMIN1)
         Y2(ICOR,1)=X2(ICOR,IMIN2)
         Y2(ICOR,2)=X2(ICOR,JMIN2)
  320    CONTINUE
         IMIN1=1
         JMIN1=2
         IMIN2=1
         JMIN2=2
         IFOIS=0
  310    CONTINUE
C
C?   make middles of the segments
C
         DO 400 I=1,3
         TRA1=Y1(I,IMIN1)
         TRA2=(Y1(I,1)+Y1(I,2))*0.5
         Y1(I,1)=TRA1
         Y1(I,2)=TRA2
         TRA1=Y2(I,IMIN2)
         TRA2=(Y2(I,1)+Y2(I,2))*0.5
         Y2(I,1)=TRA1
         Y2(I,2)=TRA2
  400    CONTINUE
C
C?  establish again the list of cosines
C
         CALL COSLST(Y1,Y2,V1,COSA,2)
         COMN1=-2.
C
C?   select the largest cosine (smallest angle)
C
         DO 500 I1=1,2
         I2DEP=3-I1
         DO 500 I2=I2DEP,2
         IF(COSA(I1,I2).LT.COMN1)GO TO 500
         COMN1=COSA(I1,I2)
         IM1=I1
         IM2=I2
  500    CONTINUE
C?   restrict COMN1 to [-1.,1.]
         IF (ABS(COMN1).GT.1.) COMN1=SIGN(1.,COMN1)
         ANGL2=ACOS(COMN1)
C
C?   convergence test
C
         IF(ABS(ANGL-ANGL2).LT.0.05)GO TO 600
         IF(ANGL2.LT.ANGL)THEN
            IMIN1=IM1
            IMIN2=IM2
            ANGL=ANGL2
         ELSE
            IMIN1=1
            IMIN2=1
         ENDIF
         IFOIS=IFOIS+1
C
C?   if convergence is not obtained after 5 bissections,
C?      issue an error message and stop
         IF(IFOIS.GT.5)GO TO 998
         GO TO 310
  600    CONTINUE
      ENDIF
  999 CONTINUE
      RETURN
  998 WRITE(LOUTRL,*)' CDANG --> MORE THAN 5 ITERATIONS XEC,XHC :'
      WRITE(LOUTRL,901)((X1(IPR,JPR),X2(IPR,JPR),IPR=1,3),JPR=1,3)
      WRITE(LOUTRL,901)((X1(IPR,JPR),X2(IPR,JPR),IPR=1,3),JPR=1,3)
  901 FORMAT(3(2(5X,E12.5)/)/)
      IFLAG = 4
      END
#endif
