      SUBROUTINE QBMTAG(METHOD,NTRACK,NJET,TRKJET,PROBTRK,FRF2TRK,
     +                  PVTX,SVTX,DJET,
     +                  CLMASS,CTSTAR,NSEC,CLMASJ,CTSTAJ,NSECJ,
     +                  LISTTRK,LISTEH,LISTJET)
C-----------------------------------------------------------------------
C! Invariant mass b tag.
C!   Can be called after QIPBTAG or QVSRCH, to further increase b purity.
C!
C  For each event-half, QBMTAG loops over tracks in order of decreasing
C  inconsistency with primary vertex, and stops when their combined
C  invariant mass exceeds 1.8 GeV. (i.e. The mass of a typical charmed
C  hadron). It then returns a tag, CLMASS, indicating the probablity the
C  that the "last track" used came from primary vertex.
C    For a very high purity tag, CLMASS should be combined with the
C  QIPTBAG or QVSRCH tags. The optimal linear combination (together with
C  suggested cuts) is
C         0.7*CLMASS - 0.3*log10(QIPBTAG) > 2.4
C    or   0.7*CLMASS + 0.3*QVSRCH > 8.3
C!
C!    Author: I.Tomalin  24/3/95
C!    Mod: I. Tomalin 10/3/99 - Output confidence levels for jets.
C!
C! Input Arguments:
C!    INTE METHOD    : =1 if using QIPBTAG, =2 if using QVSRCH.
C!  Following only needed if METHOD=1 (dummy otherwise):
C!    INTE NTRACK    : Number of tracks used by QIPBTAG.
C!    INTE NJET      : Number of jets considered by QIPBTAG.
C!    INTE TRKJET(*) : ALPHA track number of QIPBTAG jets.
C!    REAL PROBTRK(*): QIPBTAG track probabilities.
C!    INTE FRF2TRK(*): Row in FRFT bank of QIPBTAG tracks.
C!  Following only needed if METHOD=2 (dummy otherwise):
C!    REAL PVTX(3)   : Position of primary vertex from QVSRCH.
C!    REAL SVTX(3,2) : Positions of two secondary vertices in jet
C!                     coordinate system from QVSRCH.
C!    REAL DJET(3,2) : Unitized jet directions from QVSRCH.
C!
C! Output Arguments. (Dimensioned 2 to correspond to two event-halves):
C!    REAL CLMASS(2) : For METHOD=1, = -log10(confidence level) for
C!                     "last track" to come from primary vertex
C!                     (c.l. in range 0-1).
C!                     For METHOD=2, = QSQT(CHI**2 (primary) minus
C!                     CHI**2 (secondary)) for "last track".
C!    REAL CTSTAR(2) : Decay angle of "last track" in "b" rest frame.
C!    INTE NSEC(2)   : Number of tracks used to reach 1.8 GeV mass.
C!                     (zero if couldn't reach 1.8 GeV).
C!
C! Following jet info available only if METHOD=1:
C!    REAL CLMASJ(*) : Same variables for jets (ordered as in TRKJET)
C!    REAL CLSTAJ(*) :            "
C!    INTE NSECJ(*)  :            "
C!
C!    INTE LISTTRK(*): List of all ALPHA charged tracks and V0s which
C!                     were considered by QBMTAG.
C!                     If the track is neutral, it is a V0. If a V0
C!                     is not in the range KFV0T-KLV0T, then it has
C!                     been created using YTOP, and its daughters
C!                     are (refitted) copies of tracks from the
C!                     KFCHT-KLCHT section.
C!    INTE LISTEH(*) : If track comtributed to 1.8 GeV hemisphere mass,
C!                     then =1 or =2 to show which hemisphere it's in.
C!                     If =0, then track was rejected.
C!    INTE LISTJET(*): If track comtributed to 1.8 GeV jet mass,
C!                     then =ALPHA track number of jet.
C!                     If =0, then track was rejected.
C
C N.B. IF METHOD=1, then one can obtain higher purities by using the
C BNEG card. This allows QIPBTAG and QBMTAG to use information from
C both +ve and -ve impact parameter tracks. (See ALNEWS 1389 in
C OFFLINE folder). If one omits this card, however, then in the same
C way as one can measure the uds efficiency of QIPBTAG using the -ve
C hemisphere tag NPROBHEMI, it is possible to create a -ve tag from
C QBMTAG for the same purpose. This can be done by calling it with
C argument NPROBTRK instead of PROBTRK.
C
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER (MAXTRK=400,MAXJET=40,MAXV0=20,MAXDAU=10)
C
C Calling arguments
      INTEGER TRKJET(MAXJET),FRF2TRK(MAXTRK)
      REAL    PROBTRK(MAXTRK)
C
      DIMENSION PVTX(3),SVTX(3,2),DJET(3,2)
      DIMENSION CLMASS(2),CTSTAR(2),NSEC(2),
     &          CLMASJ(MAXJET),CTSTAJ(MAXJET),NSECJ(MAXJET)
C
      DIMENSION LISTTRK(MAXTRK),LISTEH(MAXTRK),LISTJET(MAXTRK)
C
C Minimum acceptable mass for secondary vertex.
      PARAMETER (WCUT=1.8)
C Minimum acceptable number of tracks assigned to secondary vertex.
      PARAMETER (NUMCUT=0)
C Minimum allowed decay length for QVSRCH.
      PARAMETER (DLENCT=0.1)
C Confidence level cut on QIPBTAG V0's.
      PARAMETER (V0CUT=0.05)
C
      DIMENSION CLTRAK(MAXTRK),IINDEX(MAXTRK),IEHTRK(MAXTRK),
     &          JETTRK(MAXTRK),ISUM(MAXJET),ITOT(MAXJET),JLIST(MAXJET),
     &          LV0DAU(2)
C
#include "btagraw.h"
C
C Common from QIPBTAG.
C--   COMMON / BTAGRAW /
C        JJET(ntrack)     array(ntrack) with track # of ass.jet # found
C                         value(jjet(i))=1,...,njet
C        JHEMI(ntrack)    array(njet) with track # of ass.hemispere # found
C                         value(jhemi(i))=1,2
C        IP(3)/ERR_IP(3,3),CHI2_DOF:  IP and error on IP & CHI**2
C        BP(3),ERR_BP(3),SIZE_BP(3),ERR2_BP(3) Beamspot & size & errors
C        TPAR             track parameters
C        TERR             errors on track impact parameters (TANL,P0,D0,Z0)
C        JETS             jet-3-momenta
C        DMIN / S_DMIN    min distance IP-track + error
C        PERP             Unit vector along impact parameter direction
C        PHIP             Direction of DMIN in thetahat-phihat plane
C        JDIST            Distance along jet direction of closest approach
C                         between track helix and jet axis.
C        LDIST / S_LDIST  Distance between track helix and jet axis at
C                         point of closest approach, with error
C        NPROBEVT         Event probability calculated using NPROBTRK.
C        NPROBHEMI(2)     Hemisphere probability calculated using NPROBTRK.
C        NPROBJET(MAXJET) Jet probability calculated using NPROBTRK.
C        NPROBTRK(MAXTRK) Negative Track probability (-PROBTRK).
C        NV0              Number of V0 candidates found
C        V0TYPE           V0 type 1=photon conversion, 2=Ks, 3=Lambda
C        V0PAIR           FRFT track numbers of V0 constituents
C        V0PAT            OR of VDET hit patterns of V0 daughter tracks
C        V0DK             Space point of V0 decay
C        V0DERR           Error matrix on decay point
C        V0PROB           Chisquared probability of combined mass+vertex fit
C        V0PAR            Track parameters of V0 (1st element=momentum)
C        V0ERR            Error matrix of V0 track paramters
C        HAXIS            Axis defining hemisphere separation
C        NDAU             # of daughters associated with a user track
C        TRKDAU           FRFT track number of user track daughters
C
C QIPBTAG common indicating if BNEG card is being used.
      LOGICAL NEGPROB
      COMMON/BTGEN/NEGPROB
C
#include "qcde.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
C Note ALPHA track numbers of jets used by QIPBTAG/QVSRCH.
      IF (METHOD.EQ.1) THEN
        JET = KPDIR('QIPBJETS',KRECO)
      ELSE
        JET = KPDIR('QVSJETS',KRECO)
      END IF
      NUMJET = 0
   10 CONTINUE
      IF (JET.GT.0.AND.NUMJET.LT.MAXJET) THEN
        NUMJET = NUMJET + 1
        JLIST(NUMJET) = JET
        JET = KFOLLO(JET)
        GOTO 10
      END IF
C
C For convenience, put jets in same order as QIPBTAG considers them.
      IF (METHOD.EQ.1) THEN
        DO 30 JETQIP = 1,NJET
          DO 20 JET = 1,NUMJET
            IF (TRKJET(JETQIP).EQ.JLIST(JET)) THEN
              JLIST(JET)    = JLIST (JETQIP)
              JLIST(JETQIP) = TRKJET(JETQIP)              
            END IF
   20     CONTINUE
   30   CONTINUE
      END IF
C
C How likely each track is to come from primary/secondary vertex ?
      NTRAK = 0
C
      IF (METHOD.EQ.1) THEN
C
C%%%%%%%%%%% Using QIPBTAG %%%%%%%%%%%%
C
C Drop any V0 created in previous calls to QBMTAG.
        CALL QVDROP('QIPBV0',KRECO)
C
C Loop over QIPBTAG tracks.
        DO 175 ITRK = 1,NTRACK
C If BNEG card is missing, only tracks with +ve impact parameter used.
          IF (.NOT.NEGPROB.AND.PROBTRK(ITRK).LE.0.0) GOTO 175
C Find corresponding ALPHA track number.
          IF (FRF2TRK(ITRK).LT.10000) THEN
            IALPHA = KFCHT - 1 + FRF2TRK(ITRK)
          ELSE
            IV0QIP = FRF2TRK(ITRK) - 10000
C Only accept QIPBTAG V0's if their confidence level is reasonable.
            IF (V0PROB(IV0QIP).LT.V0CUT) GOTO 175
            IDAU1 = KFCHT - 1 + V0PAIR(1,IV0QIP)
            IDAU2 = KFCHT - 1 + V0PAIR(2,IV0QIP)
C Take V0 from YV0V if it already exists there.
            DO 110 JV0 = KFV0T,KLV0T
              LV0DAU(1) = KCHT(KDAU(JV0,1))
              LV0DAU(2) = KCHT(KDAU(JV0,2))
              IF (IDAU1.EQ.LV0DAU(1).AND.IDAU2.EQ.LV0DAU(2)
     &        .OR.IDAU1.EQ.LV0DAU(2).AND.IDAU2.EQ.LV0DAU(1))
     &        THEN
                IALPHA = JV0
                GOTO 125
              END IF
  110       CONTINUE
C V0 not found in YV0V, so refit it using YTOP.
C First create copies of daughter tracks, so as not to mess up originals.
            LV0DAU(1) = KVSAVE(IDAU1,' ')
            LV0DAU(2) = KVSAVE(IDAU2,' ')
            IALPHA = KVFITN(2,LV0DAU,'QIPBV0')
C If vertex fit for V0 failed, it is probably not real.
            IF (IALPHA.LE.0) GOTO 175
C
  125       CONTINUE
C
            IF (V0TYPE(IV0QIP).EQ.1) THEN
              CALL QVSETM(IALPHA,QPMASS('gamma'))
            ELSE IF (V0TYPE(IV0QIP).EQ.2) THEN
              CALL QVSETM(IALPHA,QPMASS('K0s'))
            ELSE IF (V0TYPE(IV0QIP).EQ.3) THEN
              CALL QVSETM(IALPHA,QPMASS('Lam0'))
            END IF
          END IF
C
          NTRAK = NTRAK + 1
C Note ALPHA track number.
          LISTTRK(NTRAK) = IALPHA
C Note associated hemisphere and jet.
          IEHTRK(NTRAK) = JHEMI(ITRK)
          JETTRK(NTRAK) = 0
          DO 150 J = 1,NUMJET
            IF (XSAME(IALPHA,JLIST(J))) JETTRK(NTRAK) = J
  150     CONTINUE
C Note confidence level for track to come from primary vertex.
          IF (NEGPROB) THEN
            IF (PROBTRK(ITRK).GE.0.0) THEN
              CLTRAK(NTRAK) = 0.5*PROBTRK(ITRK)
            ELSE
              CLTRAK(NTRAK) = 1.0 + 0.5*PROBTRK(ITRK)
            END IF
          ELSE
            CLTRAK(NTRAK) = PROBTRK(ITRK)
          END IF
          CLTRAK(NTRAK) = -LOG10(CLTRAK(NTRAK))
C Other initialization 
          LISTEH(NTRAK) = 0
          LISTJET(NTRAK) = 0          
  175   CONTINUE
C
      ELSE
C
C%%%%%%%%%%% Using QVSRCH %%%%%%%%%%%%
C
C Loop over QVSRCH tracks.
        DO 275 IALPHA = KFCHT,KLCHT
          CALL QVSTVA(IALPHA,PVTX,DJET,SVTX,IEH,IVX)
          CALL QVSTCHI2 (DCHI2)
C Track should be consistent with either primary or secondary vertex.
          IF (IVX.GE.1) THEN
            NTRAK = NTRAK + 1
C
            NTRAK = MIN(NTRAK,MAXTRK)
C Note ALPHA track number.
            LISTTRK(NTRAK) = IALPHA
C Note associated hemisphere jet.
            IEHTRK(NTRAK) = IEH
            JETTRK(NTRAK) = 0
            DO 250 J = 1,NUMJET
              IF (XSAME(IALPHA,JLIST(J))) JETTRK(NTRAK) = J
  250       CONTINUE
C Note relative consistency of primary vs. secondary vertex in sigma.
            CLTRAK(NTRAK) = QSQT(DCHI2)
C Other initialization 
            LISTEH(NTRAK) = 0
            LISTJET(NTRAK) = 0          
          END IF
  275   CONTINUE
      END IF
C
C Order tracks according to decreasing inconsistency with primary vertex
      CALL SORTZV(CLTRAK,IINDEX,NTRAK,1,1,0)
C
C%%%%%%%%%%% Calculate hemisphere invariant mass tag %%%%%%%%%%%
C
C Loop over event-halves.
C Combine tracks to create invariant mass of at least 1.8 GeV.
C (Tracks all required to be in same jet).
C
      DO 375 JEH = 1,2
C Initialize.
        NSEC(JEH) = 0
        IF (METHOD.EQ.1) THEN
          CLMASS(JEH) = 0.0
        ELSE
          CLMASS(JEH) = -9.99
        END IF
        CTSTAR(JEH) = 0.0
        DO 310 I = 1,MAXJET
          ITOT(I) = 0
  310   CONTINUE
C
        IF (METHOD.EQ.2) THEN
C QVSRCH gives lots of background with negative or small positive
C decay lengths so ...
          IF (SVTX(3,JEH).LT.DLENCT) GOTO 375
        END IF
C
C Loop over tracks in this hemisphere.
        DO 350 J = 1,NTRAK
          JPOS = IINDEX(J)
          IF (JEH.EQ.IEHTRK(JPOS)) THEN
            JET = JETTRK(JPOS)
C Veto a minute number of tracks accepted by QIPBTAG/QVSRCH but
C rejected by ENFLW (so not associated to a jet).
            IF (JET.EQ.0) GOTO 350
            IF (ITOT(JET).EQ.0) ISUM(JET) = KVNEW(FLOAT(J))
            CALL QVADDN(ISUM(JET),LISTTRK(JPOS))
            ITOT(JET) = ITOT(JET) + 1
C
C See if these tracks give a mass of over WCUT and a multiplicity of
C at least NUMCUT.
            IF (QM(ISUM(JET)).GT.WCUT.AND.ITOT(JET).GE.NUMCUT) THEN
C
C Note how likely it is that final track added comes from primary vertex
              CLMASS(JEH) = MAX(CLMASS(JEH),CLTRAK(JPOS))
C Note also it's cos(theta*) of track in b rest frame.
              CTSTAR(JEH) = QDECAN(ISUM(JET),LISTTRK(JPOS))
C Store list of tracks used
              DO 325 I = 1,J
                IPOS = IINDEX(I)
                IJET = JETTRK(IPOS)
                IF (IJET.EQ.JET) THEN
                  NSEC(JEH) = NSEC(JEH) + 1
                  LISTEH(IPOS) = JEH
                END IF
  325         CONTINUE
              GOTO 375
C
            END IF
          END IF
C
  350   CONTINUE
  375 CONTINUE
C
C%%%%%%%%%%% Calculate jet invariant mass tag %%%%%%%%%%%
C
C Loop over jets.
C Combine tracks to create invariant mass of at least 1.8 GeV.
C
      JSUM = KVNEW(DUM)
      DO 475 JET = 1,NUMJET
C Initialize.
        NSECJ(JET) = 0
        IF (METHOD.EQ.1) THEN
          CLMASJ(JET) = 0.0
        ELSE
          CLMASJ(JET) = -9.99
        END IF
        CTSTAJ(JET) = 0.0
        JTOT = 0
        CALL QVZERO(JSUM)
C
C       IF (METHOD.EQ.2) THEN
C QVSRCH gives lots of background with negative or small positive
C decay lengths so ...
C         IF (SVTX(3,JEH).LT.DLENCT) GOTO 475
C       END IF
C
C Loop over tracks in this jet
        DO 450 J = 1,NTRAK
          JPOS = IINDEX(J)
          IF (JET.EQ.JETTRK(JPOS)) THEN
            CALL QVADDN(JSUM,LISTTRK(JPOS))
            JTOT = JTOT + 1
C
C See if these tracks give a mass of over WCUT and a multiplicity of
C at least NUMCUT.
            IF (QM(JSUM).GT.WCUT.AND.JTOT.GE.NUMCUT) THEN
C
C Note how likely it is that final track added comes from primary vertex
              CLMASJ(JET) = MAX(CLMASJ(JET),CLTRAK(JPOS))
C Note also it's cos(theta*) of track in b rest frame.
              CTSTAJ(JET) = QDECAN(JSUM,LISTTRK(JPOS))
C Store list of tracks used
              DO 425 I = 1,J
                IPOS = IINDEX(I)
                IJET = JETTRK(IPOS)
                IF (IJET.EQ.JET) THEN
                  NSECJ(JET) = NSECJ(JET) + 1
                  LISTJET(IPOS) = JET
                END IF
  425         CONTINUE
              GOTO 475
C
            END IF
          END IF
C
  450   CONTINUE
  475 CONTINUE

      END
#endif










