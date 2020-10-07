      SUBROUTINE CPTCOM(V1,XEC,XHC,IPTC)
C***********************************************************************
C! See if faces have a common point                                    *
C! in the direction of v1                                              *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : KINE,VERT                                              *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   V1      : Reference direction                                     *
C!   XEC,XHC : Ecal and Hcal faces                                     *
C!   IPTC    : Output, 1 if there is a common point, 0 else            *
C!                                                                     *
C***********************************************************************
#ifndef DOC
      DIMENSION V1(*),VN(3),XEC(3,*),XHC(3,*),XUSE(5,3)
C
C?  say that the two faces have no common point
C
      IPTC=0
      DO 60 I=1,3
      XUSE(1,I)=XHC(I,3)-XHC(I,1)
      XUSE(2,I)=XHC(I,2)-XHC(I,1)
      XUSE(3,I)=XHC(I,4)-XHC(I,1)
   60 CONTINUE
      SURPR=0.
      I1=1
C
C   COMPUTE AREA NORMAL TO V1 OF THE ENTRY FACE OF HCAL STOREY
C
      DO 61 I=2,3
      SURPR=SURPR+ABS(
     SV1(1)*(XUSE(I,2)*XUSE(I1,3)-XUSE(I,3)*XUSE(I1,2))
     S+V1(2)*(XUSE(I,3)*XUSE(I1,1)-XUSE(I,1)*XUSE(I1,3))
     S+V1(3)*(XUSE(I,1)*XUSE(I1,2)-XUSE(I,2)*XUSE(I1,1)))
   61 CONTINUE
      SURMJ=SURPR+0.2
      SMIN=100000.
      DO 20 IEC=1,4
      DO 10 I=1,3
      XUSE(1,I)=XHC(I,1)-XEC(I,IEC)
      XUSE(2,I)=XHC(I,2)-XEC(I,IEC)
      XUSE(3,I)=XHC(I,3)-XEC(I,IEC)
      XUSE(4,I)=XHC(I,4)-XEC(I,IEC)
   10 CONTINUE
C
C   LOOP ON CORNERS OF THE ECAL STOREY EXIT FACE
C
      DO 22 J=1,4
      PSC=0.
C
C   PROJECT THE CORNER OF ECAL ONTO THE FACE OF HCAL,
C       FOLLOWING THE DIRECTION V1
C
      DO 21 I=1,3
   21 PSC=PSC+V1(I)*XUSE(J,I)
      DO 23 I=1,3
      XUSE(J,I)=XUSE(J,I)-V1(I)*PSC
   23 CONTINUE
   22 CONTINUE
      DO 30 I=1,3
   30 XUSE(5,I)=XUSE(1,I)
      SURF=0.
C
C  COMPUTE THE TOTAL AREA OF THE 4 TRIANGLES BUILT ON THE EDGES OF
C  THE HCAL FACE, ONE SUMMIT BEING THE PROJECTED ECAL CORNER
C
      DO 40 I=1,4
      I1=I+1
      SURF=SURF+ABS(
     SV1(1)*(XUSE(I,2)*XUSE(I1,3)-XUSE(I,3)*XUSE(I1,2))
     S+V1(2)*(XUSE(I,3)*XUSE(I1,1)-XUSE(I,1)*XUSE(I1,3))
     S+V1(3)*(XUSE(I,1)*XUSE(I1,2)-XUSE(I,2)*XUSE(I1,1)))
   40 CONTINUE
C
C   IF THE AREA OF THE 4 TRIANGLES IS MUCH LARGER THAN THE TOTAL
C   PROJECTED AREA, THE ECAL POINT PROJECTS OUTSIDE OF THE HCAL
C   STOREY FACE
C
      IF(SURF.GT.SURMJ)GO TO 20
   50 IPTC=1
      RETURN
   20 CONTINUE
      RETURN
      END
#endif
