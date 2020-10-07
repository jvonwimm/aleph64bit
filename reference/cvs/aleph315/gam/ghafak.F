      SUBROUTINE GHAFAK (TABHAD,PFAKE,WARNING,D12,D23)
C-------------------------------------------------------------------
C  M.Verderi
C! Main routine for hadronic fake photon likelihood computation
C  Input : tabhad(19) (real)
C        : tabhad( 1) Raw energy of cluster
C        : tabhad( 2) Theta barycenter
C        : tabhad( 3) Phi   barycenter
C        : tabhad( 4) Distance barycenter (no corr. from Scurve) track
C        : tabhad( 5) Energy of storeys fired the track extrapolation
C        : tabhad( 6) float(number of storeys in stack 1)
C        : tabhad( 7)   "  (  "    "    "     "    "   2)
C        : tabhad( 8)   "  (  "    "    "     "    "   3)
C        : tabhad( 9) Fraction of energy in stack 1
C        : tabhad(10)    "     "    "    "    "   2
C        : tabhad(11) Energy of outside storeys touching the peak
C                     Storeys
C        : tabhad(12) Theta barycenter stack 1
C        : tabhad(13) Phi      "         "   1
C        : tabhad(14) Theta    "         "   2
C        : tabhad(15) Phi      "         "   2
C        : tabhad(16) Theta    "         "   3
C        : tabhad(17) Phi      "         "   3
C        : tabhad(18) EIDT energy of four central storeys in stack 1
C                     associated to the track
C        : tabhad(19)                    "                         2
C
C  Output: PFAKE,WARNING,D12,D23
C
C          WARNING (integer) = 100 no computation done (cos>.95)
C                               10 overlap region (special case)
C                                5 photon in NEUTRAL PECO=>ROUGH
C                                  ESTIMATION
C                                1 no EIDT information available for
C                                  closest track
C
C          PFAKE   (real)    = hadronic fake photon likelihood
C                              Special cases:
C                              -1.  if no computation done (cos>.95)
C                              ~1.2 if photon recognized as a cluster
C                                   of charged track
C          D12, D23          = angular distances between stack1-stack2
C                              and stack2-stack3 barycenters (computed
C                              with dstack)
C
C  Called from GAMPEX
C-------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER WARNING
      REAL TABHAD(19),PFAKE,PGOOD
      REAL ERAW, THEBARY, PHIBARY, DTRACK, EFIRED, X1, X2, X3,
     &     R1, R2, EFRONE, THEBS1, PHIBS1, THEBS2, PHIBS2,
     &     THEBS3, PHIBS3, EC4TRA1, EC4TRA2
      INTEGER N1,N2,N3,NFAKE,NGOOD,IDZ,M1,IH
      REAL    ER12,DSTACK,D12,D23,PF,PG,GXHAD,DSTAIJ,RSTA12,ENVIR
      EXTERNAL DSTACK,GXHAD

      ERAW    = TABHAD( 1)
      THEBARY = TABHAD( 2)
      PHIBARY = TABHAD( 3)
      DTRACK  = TABHAD( 4)
      EFIRED  = TABHAD( 5)
      X1      = TABHAD( 6)
      X2      = TABHAD( 7)
      X3      = TABHAD( 8)
      R1      = TABHAD( 9)
      R2      = TABHAD(10)
      EFRONE  = TABHAD(11)
      THEBS1  = TABHAD(12)
      PHIBS1  = TABHAD(13)
      THEBS2  = TABHAD(14)
      PHIBS2  = TABHAD(15)
      THEBS3  = TABHAD(16)
      PHIBS3  = TABHAD(17)
      EC4TRA1 = TABHAD(18)
      EC4TRA2 = TABHAD(19)

      N1 = INT(X1+.1)
      N2 = INT(X2+.1)
      N3 = INT(X3+.1)

C Distance angulaire entre barycentre stack1/2 et stack2/3:
      D12 = DSTACK(THEBS1,PHIBS1,THEBS2,PHIBS2)
      D23 = DSTACK(THEBS3,PHIBS3,THEBS2,PHIBS2)

C Flag de precaution:
C -------------------
C Outside acceptance: no treatment done:
C --------------------------------------
      WARNING =  100
      PFAKE   = -1.0
      IF (ABS(COS(THEBARY)).GT.0.95) GOTO 999

C Likelihood computation:
C -----------------------
      WARNING = 0

C Numero depart histogramme:
      NFAKE = 550
      NGOOD = 650
      IF (ABS(COS(THEBARY)).GT.0.76) THEN
         NFAKE = 750
         NGOOD = 850
      ENDIF
Calcul des probas:
      PFAKE = 1.0
      PGOOD = 1.0

C PHOTONS TOUCHED BY THE TRACK: proba. sets to ~1.2:
C---------------------------------------------------
      IF (EFIRED.GT.0.) THEN
         IF (DTRACK.LT.3.) THEN
            PFAKE = 1.00
            PGOOD = -.166
            GOTO 100
         ENDIF
         IF (EFIRED/ERAW.GT.0.25) THEN
            PFAKE = 1.00
            PGOOD = -.166
            GOTO 100
         ENDIF
      ENDIF

C Photons dans la zone fin de barrel (efired = 0):
C-------------------------------------------------

      IF (ABS(COS(THEBARY)).GT.0.76.AND.
     &    ABS(COS(THEBARY)).LT.0.81) THEN
         IF (DTRACK.LT.5.00) THEN
            PFAKE = 1.00
            PGOOD = -.166
            WARNING = 10
            GOTO 100
         ENDIF
      ENDIF

C Les autres:
C------------

C Flag de distance a la trace:
C-----------------------------
      IDZ = 1
      IF (DTRACK.GT.05.) IDZ = 2
      IF (DTRACK.GT.10.) IDZ = 3


C Energies raw par bandes de trace, mip/non mip et n1 =/> 0:
C-----------------------------------------------------------
      m1 = 0
      IF (N1.EQ.0) M1 = 1
      IF (EC4TRA1.LT.0.250) THEN
         IF (EC4TRA2.LT.0.450) THEN
            IH = 00 + IDZ + M1*09
         ELSE
            IH = 03 + IDZ + M1*09
         ENDIF
      ELSE
         IH = 06 + IDZ + M1*09
      ENDIF
C     Ec4 trace pas definie:
      IF (EC4TRA1.LT.0..OR.EC4TRA2.LT.0.) THEN
         IH = 06 + IDZ + M1*09
         WARNING = 01
      ENDIF
C     Photon dans PECO neutre:
      IF (DTRACK.GT.2000.) THEN
         IH = 00 + IDZ + M1*09
         WARNING = 05
      ENDIF
      ER12 = ERAW*(R1+R2)
      PF = GXHAD (NFAKE+IH,ER12)
      PG = GXHAD (NGOOD+IH,ER12)
      IF (PF+PG.NE.0.) THEN
         PFAKE = PFAKE*PF/(PF+PG)
         PGOOD = PGOOD*PG/(PF+PG)
      ENDIF

C Distance stack1-2, stack2-3:
C-----------------------------
      IF (N1.NE.0) THEN
         DSTAIJ = D12
         IH = 0
         IF (N1+N2.EQ.3.OR.N1+N2.EQ.4) IH = 19
         IF (N1+N2.EQ.5.OR.N1+N2.EQ.6) IH = 20
         IF (N1+N2.GE.7)               IH = 21
      ELSE
         DSTAIJ = D23
         IH = 0
         IF (N2+N3.GT.2)               IH = 22
      ENDIF
      IF (IH.NE.0) THEN
         PF = GXHAD (NFAKE+IH,DSTAIJ)
         PG = GXHAD (NGOOD+IH,DSTAIJ)
         IF (PF+PG.NE.0.) THEN
            PFAKE = PFAKE*PF/(PF+PG)
            PGOOD = PGOOD*PG/(PF+PG)
         ENDIF
      ENDIF

C Fraction stack1+2:
C-----------------------------
      IF (N1.NE.0) THEN
         RSTA12 = R1+R2
         IF (N1+N2.LT.5)                IH = 23
         IF (N1+N2.GT.4)                IH = 24
      ELSE
         RSTA12 = R2
         IF (N2   .LT.5)                IH = 25
         IF (N2   .GT.4)                IH = 26
      ENDIF
      PF = GXHAD (NFAKE+IH,RSTA12)
      PG = GXHAD (NGOOD+IH,RSTA12)
      IF (PF+PG.NE.0.) THEN
         PFAKE = PFAKE*PF/(PF+PG)
         PGOOD = PGOOD*PG/(PF+PG)
      ENDIF
      IF (N1.GT.0) THEN
         IF (N3.EQ.0)                   IH = 27
         if (n3.gt.0)                   ih = 28
         PF = GXHAD (NFAKE+IH,R1)
         PG = GXHAD (NGOOD+IH,R1)
         IF (PF+PG.NE.0.) THEN
            PFAKE = PFAKE*PF/(PF+PG)
            PGOOD = PGOOD*PG/(PF+PG)
         ENDIF
      ENDIF


C Environnement du photon:
C------------------------
      ENVIR = EFRONE/ERAW
      IF (N1.NE.0) THEN
                         IH = 29
         IF (ERAW.GT.1.) IH = 30
         IF (ERAW.GT.2.) IH = 31
      ELSE
                         IH = 32
         IF (ERAW.GT.1.) IH = 33
         IF (ERAW.GT.2.) IH = 34
      ENDIF
      PF = GXHAD (NFAKE+IH,ENVIR)
      PG = GXHAD (NGOOD+IH,ENVIR)
      IF (PF+PG.NE.0.) THEN
         PFAKE = PFAKE*PF/(PF+PG)
         PGOOD = PGOOD*PG/(PF+PG)
      ENDIF

100   CONTINUE

      PFAKE = PFAKE/(PFAKE+PGOOD)

999   CONTINUE
      RETURN
      END
#endif
