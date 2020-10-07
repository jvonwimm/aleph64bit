      SUBROUTINE COSLST(X1,X2,V1,COSA,NP)
C***********************************************************************
C! Make list of cosines          between hcal and ecal faces           *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  870831                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!   X1,X2   : Coordinates of the corners of the two faces             *
C!   V1      : Direction cosines of the reference idrection            *
C!   COSA    : Table of Cosines                                        *
C!   NP      : Number of corners in tables X1 and X2                   *
C!                                                                     *
C***********************************************************************
#ifndef DOC
      DIMENSION X1(3,*),X2(3,*),COSA(4,*),V2(3),V1(*)
      DO 100 I1=1,NP
      DO 100 I2=1,NP
      V2MOD=0.
      PSC=0.
      DO 120 J2=1,3
      V2J=X2(J2,I2)-X1(J2,I1)
      PSC=PSC+V1(J2)*V2J
  120 V2MOD=V2MOD+V2J**2
      V2MOD=SQRT(V2MOD)
      COSA(I1,I2)=PSC/V2MOD
  100 CONTINUE
      RETURN
      END
#endif
