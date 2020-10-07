      SUBROUTINE YNV0VE(KP1,KP2,PS1,PS2,PV0,PVMAT,PV1,PV2,CPV,
     $   VERT,VVMAT,PX,PXM,CHI2,IFAIL)
C-------------------------------------------------------------
C! V0 vertex fit first performance
CKEY YV0 VERTEX FIT /INTERNAL
C
C     AUTHOR: M.A.CIOCCI,L.ROLANDI 23/2/88
C     MODIFIED:M.A.CIOCCI 23/2/90
C
C       INPUT : KP1/I,KP2/I    Numbers of the two tracks in the
C                              FRFT bank
C             : PS1/R,PS2/R    Psi angles of the two tracks at the
C                              approximate vertex position
C
C      OUTPUT : PV0 /R         V0 momentum
C             : PVMAT/D        Covariance matrix of V0 momentum
C             : PV1/R          Refitted momenta of positive charged
C                              particle
C             : PV2/R          Refitted momenta of negative charged
C                              particle
C             : CPV/R          Covariance matrix of pv1 pv2
C             : VERT/R         Coordinates of the vertex
C             : VVMAT/D        Covariance matrix of the vertex
C             : PX/R           Mass constaints (r,b of aleph note...)
C             : PXM/D          Covariance matrix of the mass constraint
C             : CHI2/R         Chi square of the vertex fit
C             : IFAIL/I        If not equal to 0 troubles in
C                              inverting matrices.
C
C       CALLED:
C         YMFV0V
C
C       CALLS:
C         YV0ONH  Calculates the point close at vertex for one
C                 of two helix and the inverse of covariance
C                 matrix for this point
C
C         LIBRARY REQUIRED: CERN
C
C                 DESCRIPTION
C                 ===========
C
C   Call the subroutine YV0ONH to calculate the matrix of
C   the derivatives of the trajectory parameters respect to
C   the coordinates and momenta, computed at the point defined
C   by the angles PS1 and PS2. Then fits the vertex in 3d.
C   The procedure is iterated defining new starting points
C   for the fit. For a complete description see aleph note on v0
C
C-------------------------------------------------------------
#ifndef DOC
      SAVE
      REAL FIELM
C
      REAL ALFAC
C
C
        REAL PS1,PS2
        REAL PV0(3),VERT(3),PX(2),CHI2,PV1(3),PV2(3)
        DOUBLE PRECISION PVMAT(3,3),VVMAT(3,3),PXM(2,2),
     $  CPV(6,6)
        DOUBLE PRECISION BV1(6),BV2(6)
        DOUBLE PRECISION WVEC(9),WMAT(6,2),WMAT1(3,3)
        DOUBLE PRECISION CC2,PT12,PT22,PT2,PT,P12,P22
        DOUBLE PRECISION XMAT1(6,6),XMAT2(6,6),Z1(5,6),Z2(5,6)
        DOUBLE PRECISION XMT(9,9),VT(9)
        DOUBLE PRECISION XMATP(6,6)
        DOUBLE PRECISION PF1(6),PF2(6)
        DOUBLE PRECISION PV(2),DMAT(2,6),PMAT(2,2)
        DOUBLE PRECISION DPS1,DPS2
        DOUBLE PRECISION DMY
        INTEGER KP1,KP2,IFAIL
        LOGICAL LASIT
        DATA ALFAC/0.29979E-3/
C  STEERING DATA FOR THE MAIN LOOP
C
C VCCUT = IF CHI**2 IS LESS THEN VCCUT STOP LOOPING
C VITMX = IF MORE THEN VITMX ITERATION STOP LOOPING
C VDCUT = IF RELATIVE CHANGE OF CHI**2 IS LESS THEN
C          VDCUT STOP LOOPING
C
        DATA VCCUT/1./,VITMX/5./,VDCUT/.1/
C
C+ MAGNETIC FIELD
C
        FIELM=ALFIEL(DUM)
        FIELM=ABS(FIELM)
        NITE=0
        CHIOL=1.E30
   1    CONTINUE
        NITE=NITE+1
C
C+   CALL YV0ONH FOR THE FIRST TRACK (POSITIVE)
C
        CALL YV0ONH(KP1,PS1,XMAT1,Z1,BV1,IFAIL)
           IF (IFAIL.NE.0) GOTO 200
C
C+  AND NOW THE SECOND TRACK (NEGATIVE)
C
        CALL YV0ONH(KP2,PS2,XMAT2,Z2,BV2,IFAIL)
        IF (IFAIL.NE.0) GOTO 200
C
C+ NOW BUILDS THE MATRIX XT FOR A FIT OF A COMMON VERTEX
C+ OF THE TWO TRACKS
C
C                 XA11   XA12             XB11   XB12
C       XMAT1 =  t               XMAT2 = t
C                 XA12   XA22             XB12   XB22
C
C WHERE XA.. XB..  ARE 3x3 MATRICES
C
C              XA11+XB11     XA12      XB12
C                t
C       XMT  =    XA12       XA22       0
C                t
C                 XB12         0       XB22
C
C   AND
C
C    VT(1..3)= XA11 * BV1(1..3) + XA12 * BV1(4..6) +
C              XB11 * BV2(1..3) + XB12 * BV2(4..6)
C             t
C    VT(4..6)= XA12 * BV1(1..3) + XA22 * BV1(1..3)
C             t
C    VT(7..9)= XB12 * BV2(1..3) + XB22 * BV2(4..6)
C
        DO 20 I=1,6
        VT(I)=0
        DO 10 J=1,6
        VT(I)=VT(I)+XMAT1(I,J)*BV1(J)
        XMT(I,J)=XMAT1(I,J)
   10   CONTINUE
   20   CONTINUE
C
        DO 40 I=1,3
        VT(I+6)=0
        DO 30 J=1,6
        VT(I)=VT(I)+XMAT2(I,J)*BV2(J)
        VT(I+6)=VT(I+6)+XMAT2(I+3,J)*BV2(J)
   30   CONTINUE
        DO 35 J=1,3
        XMT(I,J)=XMT(I,J)+XMAT2(I,J)
        XMT(I+6,J+6)=XMAT2(I+3,J+3)
        XMT(I,J+6)  =XMAT2(I,J+3)
        XMT(I+6,J)  =XMAT2(I+3,J)
        XMT(I+3,J+6)=0
        XMT(I+6,J+3)=0
   35   CONTINUE
   40   CONTINUE
C
C  AND NOW INVERT THE MATRIX XM SOLVING THE LINEAR
C  SISTEM . ON OUTPUT VT CONTAINS :
C   VT(1..3)  COORDINATES OF THE VERTEX POINT
C   VT(4..6)  MOMENTUM OF 1ST PARTICLE (IN CM!)
C   VT(7..9)    "      "  2ND    "     (IN CM!)
C
#ifndef BIT64
        CALL DEQINV (9,XMT,9,WVEC,IFAIL,1,VT)
#else
        CALL REQINV (9,XMT,9,WVEC,IFAIL,1,VT)
#endif
        IF (IFAIL.NE.0) THEN
        GOTO 200
        ENDIF
C
C+ COMPUTES THE CHI SQUARE AT MINIMUM
C
        CC2=0.
        DO 90 I=1,6
        I1=I
        IF (I1.GT.3) I1=I1+3
        DO 80 J=1,3
        CC2=CC2+(VT(I)-BV1(I))*XMAT1(I,J)*(VT(J)-BV1(J))+
     $          (VT(I1)-BV2(I))*XMAT2(I,J)*(VT(J)-BV2(J))
  80    CONTINUE
        DO 81 J=4,6
        CC2=CC2+(VT(I)-BV1(I))*XMAT1(I,J)*(VT(J)-BV1(J))+
     $          (VT(I1)-BV2(I))*XMAT2(I,J)*(VT(J+3)-BV2(J))
  81    CONTINUE
  90    CONTINUE
        DC=(CHIOL-SNGL(CC2))/CHIOL
        LASIT=(SNGL(CC2).LT.VCCUT.OR.FLOAT(NITE)
     $  .GT.VITMX.OR.DC.LT.VDCUT)
C
C   PREPARE THE OUTPUT
C
        IF (NITE.EQ.1.OR.DC.GT.0.) THEN
C
C     NOW SAVES THE VERTEX, THE MOMENTUM AND THEIR
C     COVARIANCE MATRIX FOR THE OUTPUT
C
        DO 60 I=1,3
        VERT(I)=SNGL(VT(I))
        PV0(I)=SNGL(VT(I+3)+VT(I+6))*ALFAC*FIELM
        PV1(I)=SNGL(VT(I+3))*ALFAC*FIELM
        PV2(I)=SNGL(VT(I+6))*ALFAC*FIELM
        DO 50 J=1,3
        VVMAT(I,J)=XMT(I,J)
        PVMAT(I,J)=(XMT(I+3,J+3)+XMT(I+3,J+6)+
     $   XMT(I+6,J+3)+XMT(I+6,J+6))*DBLE(ALFAC*FIELM)
     $*DBLE(ALFAC*FIELM)
        CPV(I,J)=(XMT(I+3,J+3))*DBLE(ALFAC*FIELM)
     $  *DBLE(ALFAC*FIELM)
        CPV(I+3,J+3)=(XMT(I+6,J+6))*DBLE(ALFAC*FIELM)
     $  *DBLE(ALFAC*FIELM)
        CPV(I,J+3)=(XMT(I+3,J+6))*DBLE(ALFAC*FIELM)
     $  *DBLE(ALFAC*FIELM)
        CPV(I+3,J)=CPV(I,J+3)
  50    CONTINUE
  60    CONTINUE
        CHI2=CC2
C
C  COPY FOR SIMPLICITY THE RESULT IN TWO VECTORS
C  PF1(6), PF2(6) CONTAINING FOR EACH PARTICLE
C  COORDINATES AND MOMENTA
C
        DO 70 I=1,3
        PF1(I)=VT(I)
        PF2(I)=VT(I)
        PF1(I+3)=VT(I+3)
        PF2(I+3)=VT(I+6)
  70    CONTINUE
C
C  NOW SAVES THE COVARIANCE MATRIX OF THE MOMENTA
C
       DO 130 I=1,6
       DO 120 J=1,6
       XMATP(I,J)=XMT(I+3,J+3)
 120   CONTINUE
 130   CONTINUE
C
        ENDIF
C
C CECK IF IT IS THE LAST ITERATION
C
        IF (LASIT) GOTO 98
        CHIOL=CC2
C
C+ COMPUTE THE ANGLE VARIATION  DPS1 AND DPS2
C
         DPS1=0.
         DPS2=0.
         DO 91 I=1,6
         DPS1=DPS1+Z1(3,I)*(PF1(I)-BV1(I))
         DPS2=DPS2-Z2(3,I)*(PF2(I)-BV2(I))
  91     CONTINUE
         DPS1=DPS1+(-BV1(4)*(PF1(5)-BV1(5))+
     $       BV1(5)*(PF1(4)-BV1(4)))/(BV1(4)*BV1(4)+BV1(5)*BV1(5))
         DPS2=DPS2-(-BV2(4)*(PF2(5)-BV2(5))+
     $       BV2(5)*(PF2(4)-BV2(4)))/(BV2(4)*BV2(4)+BV2(5)*BV2(5))
         PS1=PS1+SNGL(DPS1)
         PS2=PS2+SNGL(DPS2)
C
C GO TO THE NEXT ITERATION
C
         GOTO 1
  98     CONTINUE
C
C  COMPUTES NOW THE TERM PV
C
       P12=PF1(4)*PF1(4)+PF1(5)*PF1(5)+PF1(6)*PF1(6)
       P22=PF2(4)*PF2(4)+PF2(5)*PF2(5)+PF2(6)*PF2(6)
       PT2=(PF1(4)+PF2(4))*(PF1(4)+PF2(4))+(PF1(5)+PF2(5))*
     $     (PF1(5)+PF2(5))+(PF1(6)+PF2(6))*(PF1(6)+PF2(6))
       PV(1)=(P12-P22)/PT2
       PT=SQRT(PT2)
       WVEC(1)=( PF1(5)*PF2(6)-PF1(6)*PF2(5))/PT
       WVEC(2)=(-PF1(4)*PF2(6)+PF1(6)*PF2(4))/PT
       WVEC(3)=( PF1(4)*PF2(5)-PF1(5)*PF2(4))/PT
       PV(2)=SQRT(WVEC(1)*WVEC(1)+WVEC(2)*WVEC(2)+WVEC(3)*WVEC(3))
       DO 110 I=1,3
       WVEC(I)=WVEC(I)/PV(2)
 110   CONTINUE
       DMAT(1,1)=2.D0*( PF1(4)-(PF1(4)+PF2(4))*PV(1))/PT2
       DMAT(1,2)=2.D0*( PF1(5)-(PF1(5)+PF2(5))*PV(1))/PT2
       DMAT(1,3)=2.D0*( PF1(6)-(PF1(6)+PF2(6))*PV(1))/PT2
       DMAT(1,4)=2.D0*(-PF2(4)-(PF1(4)+PF2(4))*PV(1))/PT2
       DMAT(1,5)=2.D0*(-PF2(5)-(PF1(5)+PF2(5))*PV(1))/PT2
       DMAT(1,6)=2.D0*(-PF2(6)-(PF1(6)+PF2(6))*PV(1))/PT2
       DMAT(2,1)=( PF2(5)*WVEC(3)-PF2(6)*WVEC(2))/PT
     $           -PV(2)*(PF1(4)+PF2(4))/PT2
       DMAT(2,2)=(-PF2(4)*WVEC(3)+PF2(6)*WVEC(1))/PT
     $           -PV(2)*(PF1(5)+PF2(5))/PT2
       DMAT(2,3)=( PF2(4)*WVEC(2)-PF2(5)*WVEC(1))/PT
     $           -PV(2)*(PF1(6)+PF2(6))/PT2
       DMAT(2,4)=(-PF1(5)*WVEC(3)+PF1(6)*WVEC(2))/PT
     $           -PV(2)*(PF1(4)+PF2(4))/PT2
       DMAT(2,5)=(+PF1(4)*WVEC(3)-PF1(6)*WVEC(1))/PT
     $           -PV(2)*(PF1(5)+PF2(5))/PT2
       DMAT(2,6)=(-PF1(4)*WVEC(2)+PF1(5)*WVEC(1))/PT
     $           -PV(2)*(PF1(6)+PF2(6))/PT2
C
C
#ifndef BIT64
       CALL DMMLT(6,6,2,XMATP(1,1),XMATP(1,2),XMATP(2,1),
     $ DMAT(1,1),DMAT(2,1),DMAT(1,2),WMAT(1,1),WMAT(1,2),WMAT(2,1),DMY)
       CALL DMMLT(2,6,2,DMAT(1,1),DMAT(1,2),DMAT(2,1),WMAT(1,1),
     $ WMAT(1,2),WMAT(2,1),PMAT(1,1),PMAT(1,2),PMAT(2,1),DMY)
#else
       CALL RMMLT(6,6,2,XMATP(1,1),XMATP(1,2),XMATP(2,1),
     $ DMAT(1,1),DMAT(2,1),DMAT(1,2),WMAT(1,1),WMAT(1,2),WMAT(2,1),DMY)
       CALL RMMLT(2,6,2,DMAT(1,1),DMAT(1,2),DMAT(2,1),WMAT(1,1),
     $ WMAT(1,2),WMAT(2,1),PMAT(1,1),PMAT(1,2),PMAT(2,1),DMY)
#endif
C
       PX(1)=SNGL(PV(1))
       PX(2)=SNGL(PV(2))*ALFAC*FIELM
       PXM(1,1)=PMAT(1,1)
       PXM(1,2)=PMAT(1,2)*DBLE(ALFAC*FIELM)
       PXM(2,1)=PXM(1,2)
       PXM(2,2)=PMAT(2,2)*DBLE(ALFAC*FIELM*ALFAC*FIELM)
 200   CONTINUE
        RETURN
        END
#endif
