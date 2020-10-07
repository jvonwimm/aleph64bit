      SUBROUTINE YNV0NF(KP1,KP2,PS1,PS2,PV0,PVMAT,
     $   PV1,PV2,CPV,VERT,VVMAT,PX,PXM,CHI2,IFAIL)
C-------------------------------------------------------------
C! V0 vertex without curvature (WITHOUT FIT)
CKEY YV0 VERTEX /INTERNAL
C
C     AUTHOR: M.A.CIOCCI,L.ROLANDI 23/2/88
C     MODIFIED:M.A.CIOCCI 2/2/90
C              M.SCMITT  21/7/93  add a protection against XMT huge
C                                 values or VT huge values
C              A.Giassi  26/7/93  replace the protection with a
C                                 less hard one
C
C       INPUT : KP1/I,KP2/I    Numbers of the two tracks in the
C                              FRFT bank
C             : PS1/R,PS2/R    Psi angles of the two tracks at the
C                              approximate vertex position
C
C      OUTPUT : PV0 /R         V0 momentum
C             : PVMAT/D        Covariance matrix of V0 momentum
C             : PV1/R          momenta of positive charged particle
C             : PV2/R          momenta of negative charged particle
C             : CPV/D          Covariance matrix of PV1 PV2
C             : VERT/R         Coordinates of the vertex
C             : VVMAT/D        Covariance matrix of the vertex
C             : PX/R           Mass constaints
C             : PXM/D          Covariance matrix of the mass constraint
C             : CHI2/R         Chi square of the vertex fit
C             : IFAIL/I        If not equal to 0 troubles in
C                              inverting matrices.
C
C       CALLED:
C         YMFV0V
C
C       CALLS:
C         YV0ONG  Calculates the point close at vertex for one
C                 of two helix and the inverse of covariance
C                 matrix for this point
C
C         LIBRARY REQUIRED: CERN
C
C                 DESCRIPTION
C                 ===========
C
C   Call the subroutine YV0ONG to calculate the matrix of
C   the derivatives of the trajectory parameters respect to
C   coordinates and momenta, computed at the point defined
C   by the angles PS1 and PS2. Then the v0 vertex is assumed to be the
C   mean value between the starting points of fit the V0 momenta is assu
C   to be the sum of the two track momenta calculated at the starting po
C   of fit for each helix,  chi2 and covariance matrices are evaluated.
C   For a complete description see aleph note ...
C
C-------------------------------------------------------------
#ifndef DOC
      SAVE
      REAL  FIELM
C
      REAL ALFAC
C
C
        REAL PS1,PS2
        REAL PV0(3),VERT(3),PX(2),CHI2,PV1(3),PV2(3)
        REAL WS(3),WV(3)
        DOUBLE PRECISION PVMAT(3,3),VVMAT(3,3),PXM(2,2),
     $  CPV(6,6)
        DOUBLE PRECISION BV1(6),BV2(6)
        DOUBLE PRECISION XX1(3,3),XI1(3,3),PP1(3,3)
        DOUBLE PRECISION XX2(3,3),XI2(3,3),PP2(3,3)
        DOUBLE PRECISION WVEC(3),WMAT(6,2)
        DOUBLE PRECISION XMT(3,3)
        DOUBLE PRECISION CC2,PT12,PT22,PT2,PT,P12,P22
        DOUBLE PRECISION XMAT1(3,3),XMAT2(3,3)
        DOUBLE PRECISION VT(3)
        DOUBLE PRECISION XMATP(6,6)
        DOUBLE PRECISION PF1(6),PF2(6)
        DOUBLE PRECISION DMY
        DOUBLE PRECISION PV(2),DMAT(2,6),PMAT(2,2)
        INTEGER KP1,KP2,IFAIL
        DOUBLE PRECISION LABS
        DOUBLE PRECISION LC, PVT
        DATA ALFAC/0.29979E-3/
        DATA LC /38.23/
        LABS(DMY) = LOG10(ABS(DMY))
C
C+      MAGNETIC FIELD
C
          FIELM=ALFIEL(DUM)
          FIELM=ABS(FIELM)
C
C+   CALL YV0ONG FOR THE FIRST TRACK (POSITIVE)
C
        CALL YV0ONG(KP1,PS1,XX1,PP1,BV1,XI1,IFAIL)
           IF (IFAIL.NE.0) GOTO 200
C
C+  AND NOW THE SECOND TRACK (NEGATIVE)
C
        CALL YV0ONG(KP2,PS2,XX2,PP2,BV2,XI2,IFAIL)
        IF (IFAIL.NE.0) GOTO 200
C
C+   THE VERTEX IS THE MEDIUM POINT AND THE MOMENTUM IS THE SUM
C
C
C
C+  CALCULATE THE MOMENTUM FOR FIRST
C+  AND FOR SECOND
         DO 1 I=1,3
           VT(I)=0.D0
           DO 2 J=1,3
            VT(I)=VT(I)+XI1(I,J)*BV1(J)+XI2(I,J)*BV2(J)
   2       CONTINUE
   1     CONTINUE
C
         DO 20 I=1,3
           DO 10 J=1,3
               XMT(I,J)=XI1(I,J)+XI2(I,J)
  10       CONTINUE
  20     CONTINUE
C
C     CALCULATES THE COVARIANCE MATRIX OF VERTEX
C     AND THE VERTEX AS A WEIGHTED MEAN
C
C
C     Protection against overflow errors in DEQINV on the VAX
C
        PVT = MAX (LABS(XMT(1,1)),LABS(XMT(2,1)),LABS(XMT(3,1)))
        IF(ABS(LABS(XMT(1,1))+LABS(XMT(2,2))+LABS(XMT(3,3))+PVT).GT.LC
     + .OR.ABS(LABS(XMT(1,2))+LABS(XMT(2,3))+LABS(XMT(3,1))+PVT).GT.LC
     + .OR.ABS(LABS(XMT(1,3))+LABS(XMT(2,1))+LABS(XMT(3,2))+PVT).GT.LC
     + .OR.ABS(LABS(XMT(1,1))+LABS(XMT(2,3))+LABS(XMT(3,2))+PVT).GT.LC
     + .OR.ABS(LABS(XMT(1,2))+LABS(XMT(2,1))+LABS(XMT(3,3))+PVT).GT.LC
     + .OR.ABS(LABS(XMT(1,3))+LABS(XMT(2,2))+LABS(XMT(3,1))+PVT).GT.LC)
     +   GOTO 201
#ifndef BIT64
           CALL DEQINV(3,XMT,3,WV,IFAIL,1,VT)
#else
           CALL REQINV(3,XMT,3,WV,IFAIL,1,VT)
#endif
               IF(IFAIL.NE.0)THEN
         GOTO 200
              ENDIF
C
C+ COMPUTES THE CHI SQUARE AT MINIMUM
C
          DO 90 I=1,3
            DO 91 J=1,3
             XMAT1(I,J)=XMT(I,J)+XX1(I,J)
             XMAT2(I,J)=XMT(I,J)+XX2(I,J)
  91        CONTINUE
  90      CONTINUE
#ifndef BIT64
          CALL DINV(3,XMAT1,3,WS,IFAIL)
#else
          CALL RINV(3,XMAT1,3,WS,IFAIL)
#endif
               IF(IFAIL.NE.0)THEN
         GOTO 200
              ENDIF
#ifndef BIT64
          CALL DINV(3,XMAT2,3,WS,IFAIL)
#else
          CALL RINV(3,XMAT2,3,WS,IFAIL)
#endif
               IF(IFAIL.NE.0)THEN
         GOTO 200
              ENDIF
        CC2=0.
        DO 81 I=1,3
          DO 80 J=1,3
           CC2=CC2+(VT(I)-BV1(I))*XMAT1(I,J)*(VT(J)-BV1(J))+
     $          (VT(I)-BV2(I))*XMAT2(I,J)*(VT(J)-BV2(J))
  80    CONTINUE
  81    CONTINUE
C
C     NOW SAVES THE VERTEX, THE MOMENTUM AND THEIR
C     COVARIANCE MATRIX FOR THE OUTPUT
C
        DO 60 I=1,3
        VERT(I)=SNGL(VT(I))
        PV0(I)=(BV1(I+3)+BV2(I+3))*DBLE(ALFAC)*DBLE(FIELM)
        PV1(I)=(BV1(I+3))*DBLE(ALFAC)*DBLE(FIELM)
        PV2(I)=(BV2(I+3))*DBLE(ALFAC)*DBLE(FIELM)
        DO 50 J=1,3
        VVMAT(I,J)=XMT(I,J)
        PVMAT(I,J)=(PP1(I,J)+PP2(I,J))*DBLE(ALFAC*FIELM)
     $*DBLE(ALFAC*FIELM)
       CPV(I,J)=(PP1(I,J))*DBLE(ALFAC*FIELM)
     $ *DBLE(ALFAC*FIELM)
       CPV(I+3,J+3)=(PP2(I,J))*DBLE(ALFAC*FIELM)
     $ *DBLE(ALFAC*FIELM)
       CPV(I,J+3)=0.D0
       CPV(I+3,J)=0.D0
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
        PF1(I+3)=BV1(I+3)
        PF2(I+3)=BV2(I+3)
  70    CONTINUE
C
C  NOW SAVES THE COVARIANCE MATRIX OF THE MOMENTA
C
       DO 130 I=1,3
       DO 120 J=1,3
       XMATP(I,J)=PP1(I,J)
       XMATP(I,J+3)=0.D0
       XMATP(I+3,J)=0.D0
       XMATP(I+3,J+3)=PP2(I,J)
 120   CONTINUE
 130   CONTINUE
C
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
       GOTO 200
C
C - error: calling DEQINV would be fatal
 201   IFAIL =1
C
C - end
 200   CONTINUE
       END
#endif
