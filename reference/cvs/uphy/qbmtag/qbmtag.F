      SUBROUTINE QBMTAG(METHOD,NTRACK,PROBTRK,FRF2TRK,PVTX,SVTX,DJET,
     +                  CLMASS,CTSTAR,NSEC,LISTSC)
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
C!    Author I.Tomalin  24/3/95
C!
C! Input Arguments:
C!    INTE METHOD    : =1 if using QIPBTAG, =2 if using QVSRCH.
C!  Following only needed if METHOD=1 (dummy otherwise):
C!    INTE NTRACK    : Number of tracks used by QIPBTAG.
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
C!    INTE LISTSC(400,2): ALPHA track numbers of these tracks, in order
C!                        of increasing consistency with primary vertex.
C!                        If the track is neutral, it is a V0. If a V0
C!                        is not in the range KFV0T-KLV0T, then it has
C!                        been created using YTOP, and its daughters
C!                        are (refitted) copies of tracks from the
C!                        KFCHT-KLCHT section.
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
C
      PARAMETER (MAXTRK=400,MAXJET=40,MAXV0=20,MAXDAU=10)
C
      DIMENSION PROBTRK(*)
      INTEGER   FRF2TRK(*)
      DIMENSION PVTX(3),SVTX(3,2),DJET(3,2)
      DIMENSION CLMASS(2),CTSTAR(2),NSEC(2),LISTSC(MAXTRK,2)
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
      DIMENSION NTRAK(2),IALP(MAXTRK,2),CLTRAK(MAXTRK,2),IINDEX(MAXTRK),
     +JETTRK(MAXTRK,2),JLIST(MAXJET),ISUM(MAXJET),ITOT(MAXJET),LV0DAU(2)
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
      INTEGER TFLAG(MAXTRK)
      INTEGER JHEMI(MAXTRK),JJET(MAXTRK)
      INTEGER NV0,V0TYPE(MAXV0),V0PAIR(2,MAXV0),V0PAT(MAXV0)
      INTEGER NDAU(MAXTRK),TRKDAU(MAXDAU,MAXTRK)
      REAL V0DK(3,MAXV0),V0DERR(3,3,MAXV0),V0PROB(MAXV0)
      REAL V0PAR(5,MAXV0),V0ERR(5,5,MAXV0),HAXIS(3)
      REAL    TPAR(5,MAXTRK),TERR(4,4,MAXTRK),JETS(3,MAXJET)
      REAL    DMIN(MAXTRK),S_DMIN(MAXTRK)
      REAL    IP(3),ERR_IP(3,3),CHI2_DOF
      REAL    BP(3),ERR_BP(3),SIZE_BP(3),ERR2_BP(3),PHIP(MAXTRK)
      REAL    JDIST(MAXTRK),LDIST(MAXTRK)
      REAL    S_LDIST(MAXTRK),PERP(3,MAXTRK)
      REAL    NPROBEVT,NPROBHEMI(2),NPROBJET(MAXJET),NPROBTRK(MAXTRK)
C--   corresponding common block:
      COMMON / BTAGRAW /
     &     BP,ERR_BP,SIZE_BP,ERR2_BP,IP,ERR_IP,CHI2_DOF,
     &     JJET,JHEMI,TPAR,TERR,JETS,
     &     TFLAG,DMIN,S_DMIN,PHIP,JDIST,LDIST,S_LDIST,
     &     PERP,NPROBEVT,NPROBHEMI,NPROBJET,NPROBTRK,
     &     NV0,V0TYPE,V0PAIR,V0PAT,V0DK,V0DERR,V0PROB,
     &     V0PAR,V0ERR,HAXIS,NDAU,TRKDAU
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
      NJET = 0
   10 CONTINUE
      IF (JET.GT.0.AND.NJET.LT.MAXJET) THEN
        NJET = NJET + 1
        JLIST(NJET) = JET
        JET = KFOLLO(JET)
        GOTO 10
      END IF
C
C How likely each track is to come from primary/secondary vertex ?
      NTRAK(1) = 0
      NTRAK(2) = 0
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
          IEH = JHEMI(ITRK)
          NUM = NTRAK(IEH) + 1
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
     +        .OR.IDAU1.EQ.LV0DAU(2).AND.IDAU2.EQ.LV0DAU(1))
     +        THEN
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
          NTRAK(IEH) = NUM
C Note ALPHA track number.
          IALP(NUM,IEH) = IALPHA
C Note associated jet.
          JETTRK(NUM,IEH) = 0
          DO 150 J = 1,NJET
            IF (XSAME(IALPHA,JLIST(J))) JETTRK(NUM,IEH) = J
  150     CONTINUE
C Note confidence level for track to come from primary vertex.
          IF (NEGPROB) THEN
            IF (PROBTRK(ITRK).GE.0.0) THEN
              CLTRAK(NUM,IEH) = 0.5*PROBTRK(ITRK)
            ELSE
              CLTRAK(NUM,IEH) = 1.0 + 0.5*PROBTRK(ITRK)
            END IF
          ELSE
            CLTRAK(NUM,IEH) = PROBTRK(ITRK)
          END IF
          CLTRAK(NUM,IEH) = -LOG10(CLTRAK(NUM,IEH))
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
            NUM = NTRAK(IEH) + 1
C
            NTRAK(IEH) = MIN(NUM,MAXTRK)
C Note ALPHA track number.
            IALP(NUM,IEH) = IALPHA
C Note associated jet.
            JETTRK(NUM,IEH) = 0
            DO 250 J = 1,NJET
              IF (XSAME(IALPHA,JLIST(J))) JETTRK(NUM,IEH) = J
  250       CONTINUE
C Note relative consistency of primary vs. secondary vertex in sigma.
            CLTRAK(NUM,IEH) = QSQT(DCHI2)
          END IF
  275   CONTINUE
      END IF
C
C%%%%%%%%%%% Calculate invariant mass tag %%%%%%%%%%%
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
C Order tracks according to decreasing inconsistency with primary vertex
        CALL SORTZV(CLTRAK(1,JEH),IINDEX,NTRAK(JEH),1,1,0)
C
C Loop over tracks
        DO 350 J = 1,NTRAK(JEH)
          JPOS = IINDEX(J)
          JET = JETTRK(JPOS,JEH)
C Veto a minute number of tracks accepted by QIPBTAG/QVSRCH but
C rejected by ENFLW (so not associated to a jet).
          IF (JET.EQ.0) GOTO 350
          IF (ITOT(JET).EQ.0) ISUM(JET) = KVNEW(FLOAT(J))
          CALL QVADDN(ISUM(JET),IALP(JPOS,JEH))
          ITOT(JET) = ITOT(JET) + 1
C
C See if these tracks give a mass of over WCUT and a multiplicity of
C at least NUMCUT.
          IF (QM(ISUM(JET)).GT.WCUT.AND.ITOT(JET).GE.NUMCUT) THEN
C
C Note how likely it is that final track added comes from primary vertex
            CLMASS(JEH) = MAX(CLMASS(JEH),CLTRAK(JPOS,JEH))
C Note also it's cos(theta*) of track in b rest frame.
            CTSTAR(JEH) = QDECAN(ISUM(JET),IALP(JPOS,JEH))
C Store list of tracks used
            DO 325 I = 1,J
              IPOS = IINDEX(I)
              IJET = JETTRK(IPOS,JEH)
              IF (IJET.EQ.JET) THEN
                NSEC(JEH) = NSEC(JEH) + 1
                LISTSC(NSEC(JEH),JEH) = IALP(IPOS,JEH)
              END IF
  325       CONTINUE
            GOTO 375
C
          END IF
C
  350   CONTINUE
  375 CONTINUE
      END
#endif
      SUBROUTINE BTAG_PAR
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C! Define cuts and histos for QIPBTAG
C  Called from QIPBTAG
C
C  read the cuts used for the B-tagging and then store them
C  All cards are now optional.
C
C  Author  Dave Brown  29-1-93
C  Modified Ian Tomalin  7-7-95  DMIN cut increased to 0.4 cm.
C                                FITP updated using new DMIN cut.
C  03/05/95  Stephan Schael, L_SCUT CHANGED FROM 2.0/3.0 TO 1.3/1.3
C  07/11/95  Stephan Schael, data card to read in NUMJET
C
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "btpar.h"
C
C  Jet resolution common
C
      REAL JRES(3),JMAX,JSCALE
      COMMON/JSIG/JRES,JMAX,JSCALE
C
C  Local variables
C
      INTEGER JTRA2,JCALB,JNUMJ, JBHIS,JVCUT,JBNEG,JBTRK
      INTEGER IBIN, IND, IPAR, ICHAR, IFIT, ITYPE, ID, NJET
      INTEGER MTYPE
      INTEGER NLINK,NAMIND
      CHARACTER*4 CHAINT
      CHARACTER*8 UNAME
      REAL DMIN_BINS(101)
C
C  Duplicate local variables for putting track parameters into common
C
      INTEGER L_MINN(3),L_IDB,L_NUMJET,L_IMASK,L_NVIEW(MAXTYPE)
      REAL L_D0_CUT,L_Z0_CUT,L_MINMOM,L_MAXMOM
      REAL L_MAX_ERR,L_CHI_CUT
      REAL L_DOTCUT
      REAL L_MXDMIN(MAXTYPE),L_MXSDMIN(MAXTYPE)
      REAL L_LCUT(MAXTYPE),L_LSIGCUT(MAXTYPE),L_SCUT(MAXTYPE)
      REAL L_PCUT
C
      INTEGER L_NTYPE
      REAL L_FITP(NFIT,MAXTYPE)
      LOGICAL L_CALIB,L_PARS
      CHARACTER*80 L_CALFILE
C
      LOGICAL L_NEGPROB
C
C  Data statements; these are the default track cuts and calibrations
C
      DATA L_PARS/.FALSE./
      DATA L_MINN/1,4,4/,L_NUMJET/3/,L_D0_CUT/0.5/,L_Z0_CUT/0.5/
      DATA L_MINMOM/0.4/,L_MAXMOM/25./,L_CHI_CUT/5.0/,L_MAX_ERR/0.2/
      DATA L_DOTCUT/0.7/
      DATA L_NEGPROB/.FALSE./
      DATA L_LCUT/10*0.075/,L_LSIGCUT/10*3.0/
      DATA L_PCUT /10./
      DATA L_SCUT/5*1.3,5*1.3/
      DATA L_MXSDMIN/5*0.1,5*.15/
      DATA L_MXDMIN/10*0.40/
      DATA L_NVIEW/2,2,2,1,1,1,1,3*2/
C
C********* New FITP cards ***********.
C
      DATA L_FITP/
     &  0.312213 , 0.0433339 , 1.02684 , 0.941368 , 3.69612,
     &  0.293634 , 0.0114367 , 1.00822 , 0.803617 , 3.76202,
     &  0.490645 , 0.0498683 , 0.92277 , 0.755991 , 2.13515,
     &  0.0317326, 0.779388  , 1.2503  , 7.50509  , 2.34848,
     &  0.117911 , 0.29361   , 1.02236 , 6.26721  , 2.22552,
     &  0.0492471, 0.388075  , 1.20794 , 5.30374  , 1.88222,
     &  0.292455 , 0.        , 0.969725, 1.70387  , 1.     ,
     &  0.190106 , 0.        ,1.02786  , 2.17761  , 1.     ,
     &  0.190106 , 0.        ,1.02786  , 2.17761  , 1.     ,
     &  0.190106 , 0.        ,1.02786  , 2.17761  ,1.      /
C
      DATA L_NTYPE/7/
C
C  Inlines
C
#include "qmacro.h"
C ------------------------------------------------------------------------------
C
C  Make sure we only setup once
C
      IF(L_PARS)GOTO 2000
      WRITE(IW(6),*)'QIPBTAG: initializing parameters'
      L_PARS = .TRUE.
      PARS = .TRUE.
C
C  Load default parameters into common
C
      DO IPAR=1,3
        MINN(IPAR) = L_MINN(IPAR)
      END DO
      DO ITYPE=1,MAXTYPE
        LCUT(ITYPE) = L_LCUT(ITYPE)
        LSIGCUT(ITYPE) = L_LSIGCUT(ITYPE)
        SCUT(ITYPE) = L_SCUT(ITYPE)
        MXSDMIN(ITYPE) = L_MXSDMIN(ITYPE)
        MXDMIN(ITYPE) = L_MXDMIN(ITYPE)
        USRNAME(ITYPE) = 'AAAAbbbb'
        NVIEW(ITYPE) = L_NVIEW(ITYPE)
      END DO
      DOTCUT = L_DOTCUT
      NUMJET = L_NUMJET
      D0_CUT = L_D0_CUT
      Z0_CUT = L_Z0_CUT
      MINMOM = L_MINMOM
      MAXMOM = L_MAXMOM
      CHI_CUT = L_CHI_CUT
      PCUT    = L_PCUT
      MAX_ERR = L_MAX_ERR
      NTYPE = L_NTYPE
      INTYPE = NTYPE
      NEGPROB = L_NEGPROB
      DO IFIT=1,NFIT
        DO ITYPE=1,MAXTYPE
          FITP(IFIT,ITYPE) = L_FITP(IFIT,ITYPE)
        END DO
      END DO
C
C  Check for calibration cards
C
      IND = NLINK('FITP',1)
      IF(IND .GT. 0)THEN
        WRITE(IW(6),*)'QIPBTAG: reading calibration from card FITP'
        NTYPE = LROWS(IND)
        DO ITYPE=1,NTYPE
          DO IFIT=1,NFIT
            FITP(IFIT,ITYPE) = RTABL(IND,ITYPE,IFIT)
          END DO
        END DO
      END IF
C
C  Check if user-selected tracks are being used
C
      JBTRK = IW(NAMIND('BTRK'))
      DO WHILE(JBTRK.GT.0)
C
C  Get the type and 'particle' name for each track
C
        ITYPE = IW(JBTRK+1)+INTYPE
        IF(ITYPE.LE.INTYPE.OR.ITYPE.GT.MAXTYPE)THEN
          WRITE(IW(6),*)'QIPBTAG: BTRK # out of range',ITYPE
          GOTO 444
        END IF
        UNAME = '        '
        DO ICHAR=1,MIN(2,(IW(JBTRK)-1))
          UNAME(1+(ICHAR-1)*4:ICHAR*4) = CHAINT(IW(JBTRK+1+ICHAR))
        END DO
        USRNAME(ITYPE) = UNAME
        NVIEW(ITYPE) = 2
C
C  If no calibration has been given for this type, assume that they
C  have the same calibration as 1-hit 2-d tracks
C
        IF(ITYPE.GT.NTYPE)THEN
          DO IFIT=1,NFIT
            FITP(IFIT,ITYPE) = L_FITP(IFIT,1)
          END DO
          NTYPE = ITYPE
          WRITE(6,*)' Default calibration for user track, type ',
     &         USRNAME(ITYPE),ITYPE
        ELSE
          WRITE(6,*)' FITP calibration for user track, type ',
     &         USRNAME(ITYPE),ITYPE
        END IF
C
C  Chain to the next bank
C
        JBTRK = IW(JBTRK-1)
      END DO
 444  CONTINUE
C
C  Check for new track cuts
C
      JTRA2 = IW(NAMIND('TRA2'))
      IF(JTRA2 .GT. 0)THEN
        WRITE(IW(6),*)'QIPBTAG: OVERWRITING track selection cuts'
        WRITE(IW(6),1100)
        MTYPE = LROWS(JTRA2)
        DO ITYPE=1,MTYPE
          LCUT(ITYPE)    = RTABL(JTRA2,ITYPE,1)
          LSIGCUT(ITYPE) = RTABL(JTRA2,ITYPE,2)
          SCUT(ITYPE)    = RTABL(JTRA2,ITYPE,3)
          MXDMIN(ITYPE)  = RTABL(JTRA2,ITYPE,4)
          MXSDMIN(ITYPE) = RTABL(JTRA2,ITYPE,5)
          WRITE(IW(6),1101)ITYPE,LCUT(ITYPE),LSIGCUT(ITYPE),
     &         SCUT(ITYPE),MXDMIN(ITYPE),MXSDMIN(ITYPE)
        END DO
      END IF
C
C Check for new jet momentum cut
C
      IND = IW(NAMIND('PJET'))
      IF ( IND .GT. 0 ) THEN
        PCUT = RW(IND+1)
        WRITE (IW(6),*)'QIPBTAG: Jet momentum cut changed from',
     .                           L_PCUT,' to ',PCUT,' GeV/c'
      ENDIF
C
C  check for jet resolution bank
C
      IND = NLINK('JRES',1)
      IF(IND .GT. 0)THEN
C
C  Dummy call to jeterr to clear the 'FIRST' flag (prevent variable overwrite)
C
        CALL JETERR(0,JRES,JRES)
        DO IPAR=1,3
          JRES(IPAR) = RW(IND+IPAR)
        END DO
        WRITE(IW(6),*)'QIPBTAG: OVERWRITING jet resolution parameters'
        WRITE(IW(6),777) JRES
 777    FORMAT(1X,'Angle resolution = ',E11.4,' + ',
     &        E11.4,'*P + ',E11.4,'*P^2')
      END IF
C
C  Check for negative probability track use
C
      JBNEG = IW(NAMIND('BNEG'))
      NEGPROB = JBNEG .GT. 0
      IF(NEGPROB)THEN
        WRITE(IW(6),*)'QIPBTAG: using + and - tracks in probability'
      END IF
C
C  Check if different # of jets is requested
C
      JNUMJ = IW(NAMIND('NUMJ'))
      IF(JNUMJ .GT. 0)THEN
        NJET = IW(JNUMJ+1)
        IF (NJET.LE.0 .OR. NJET.GT.10) NJET = 3
        WRITE(IW(6),'(A,I6,A,I6)')
     .              'QIPBTAG: number of jets used changed from ',
     .               NUMJET,' to ',NJET
        NUMJET = NJET
      ENDIF
C
C  Check if diagnostic histograms are requested
C
      JBHIS = IW(NAMIND('BHIS'))
      IF(JBHIS .GT. 0)THEN
        IDB = IW(JBHIS+1)
        WRITE(IW(6),*)'QIPBTAG: diagnostic histograms on, offset = ',IDB
C
C  Track selection histograms
C
        CALL HBOOK1(IDB+100,'TRSEL',20,-.5,19.5,0.0)
        CALL HBOOK1(IDB+101,'QP',100,0.,20.,0.0)
        CALL HBOOK1(IDB+102,'NITC',10,-.5,9.5,0.0)
        CALL HBOOK1(IDB+103,'NTPC',30,-.5,29.5,0.0)
        CALL HBOOK1(IDB+104,'Track flag',11,-.5,10.5,0.0)
        CALL HBOOK1(IDB+106,'D0',100,-.5,.5,0.0)
        CALL HBOOK1(IDB+107,'Z0',100,-1.,1.,0.0)
        CALL HBOOK1(IDB+108,'CHI/DOF',100,0.,10.,0.0)
        CALL HBOOK1(IDB+109,'SIG_D0',100,0.,.2,0.0)
        CALL HBOOK1(IDB+110,'SIG_Z0',100,0.,.2,0.0)
        CALL HBOOK1(IDB+111,'IJET',10,-.5,9.5,0.0)
        CALL HBOOK1(IDB+112,'MAXDOT',100,-1.,1.0,0.0)
C
C V0 selection
C
        CALL HBOOK1(IDB+200,'TRSEL',20,-.5,19.5,0.0)
        CALL HBOOK1(IDB+204,'Track flag',11,-.5,10.5,0.0)
        CALL HBOOK1(IDB+206,'D0',100,-.5,.5,0.0)
        CALL HBOOK1(IDB+207,'Z0',100,-1.,1.,0.0)
        CALL HBOOK1(IDB+209,'SIG_D0',100,0.,.2,0.0)
        CALL HBOOK1(IDB+210,'SIG_Z0',100,0.,.2,0.0)
        CALL HBOOK1(IDB+211,'IJET',10,-.5,9.5,0.0)
        CALL HBOOK1(IDB+212,'MAXDOT',100,-1.,1.0,0.0)
C
C  Final selection
C
        DO ITYPE=1,INTYPE
          ID = IDB+ITYPE*1000
          CALL HBOOK1(ID+150,'FINAL CUT IN TRSEL2',10,-0.5,9.5,0.0)
          CALL HBOOK1(ID+151,'DMIN',100,
     &         -1.5*MXDMIN(ITYPE),1.5*MXDMIN(ITYPE),0.0)
          CALL HBOOK1(ID+152,'S_DMIN',100,
     &         0.0,1.5*MXSDMIN(ITYPE),0.0)
          CALL HBOOK1(ID+153,'LDIST',100,
     &         -1.5*LCUT(ITYPE),1.5*LCUT(ITYPE),0.0)
          CALL HBOOK1(ID+154,'LDIST/S_LDIST',100,
     &         -1.5*LSIGCUT(ITYPE),1.5*LSIGCUT(ITYPE),0.0)
          CALL HBOOK1(ID+155,'JDIST',100,
     &         -1.5*SCUT(ITYPE),1.5*SCUT(ITYPE),0.0)
        END DO
C
C  V0 removal histograms
C
        CALL HBOOK1(IDB+60,'N V0s per event',11,-0.5,10.5,0.0)
        CALL HBOOK1(IDB+61,'N V0s by type',4,-0.5,3.5,0.0)
        CALL HBOOK1(IDB+62,'V0 probability',100,0.,1.,0.0)
C
C  Generic
C
        CALL HBOOK1(IDB+10,'QIPBTAG return code',21,-10.5,10.5,0.0)
        CALL HBOOK1(IDB+20,'Track prob',200,-1.,1.,0.0)
        CALL HBOOK1(IDB+21,'Jet prob',200,0.,1.00001,0.0)
        CALL HBOOK1(IDB+22,'Hemi prob',200,0.,1.00001,0.0)
        CALL HBOOK1(IDB+23,'Event prob',200,0.,1.00001,0.0)
        CALL HBOOK1(IDB+24,'N Jet prob',200,0.,1.00001,0.0)
        CALL HBOOK1(IDB+25,'N Hemi prob',200,0.,1.00001,0.0)
        CALL HBOOK1(IDB+26,'N Event prob',200,0.,1.00001,0.0)
      ELSE
        IDB=0
      END IF
C
C  See if the CALB card is present; if so, book the histograms
C
      JCALB = IW(NAMIND('CALB'))
      IF(JCALB.GT.0 .OR. IDB.GT.0)THEN
C
C  Special uneven-bin histograms for fitting DMIN/SIG; separate by
C  the track flag.
C
        DO IBIN=1,51
          DMIN_BINS(IBIN) = (IBIN-1)*.08
        END DO
        DO IBIN=1,40
          DMIN_BINS(51+IBIN) = 4.+ IBIN*.8
        END DO
        DO IBIN=1,10
          DMIN_BINS(91+IBIN) = 36.+ IBIN*8.
        END DO
        DO ITYPE=1,NTYPE
            CALL HBOOKB(IDB+150+ITYPE,'Neg signifigance',
     .                                 100,DMIN_BINS,0.0)
            CALL HBARX(IDB+150+ITYPE)
        END DO
      END IF
      IF(JCALB.GT.0)THEN
        WRITE(IW(6),*)'QIPBTAG: calibration on, histos booked'
        CALIB = .TRUE.
C
C  Get the calibration file name
C
        DO ICHAR=1,MIN(20,IW(JCALB))
          CALFILE(1+(ICHAR-1)*4:ICHAR*4) = CHAINT(IW(JCALB+ICHAR))
        END DO
        DO ICHAR=IW(JCALB)+1,20
          CALFILE(1+(ICHAR-1)*4:ICHAR*4) = '    '
        END DO
      ELSE
        CALIB = .FALSE.
      END IF
C
 2000 CONTINUE
      RETURN
C ----------------------------------------------------------------------
 1100 FORMAT(10X,'type ','|ldist| ','|ldist|/sig ','|sdist|',
     &     ' |dmin|',' s_dmin')
 1101 FORMAT(10X,I2,1X,5(F8.3,1X))
      END
#endif
      SUBROUTINE QVSTVA(ITK,PVTX,DJET,SVTX,JET,IVX)
CKEY  QVSRCH / INTERNAL
C ----------------------------------------------------------------------
C
C! Track-Jet-Vertex association
C  Author : T. MATTISON  U.A.BARCELONA/SLAC  1 DECEMBER 1992
C  Modified : I.TOMALIN  24/3/94 to also output DCHI2.
C  Modified : G.Graefe   25-MAR-1995 adapted to NanoDst
C
C  Input Arguments :
C   * ITK IS ALPHA TRACK NUMBER
C   * PVTX() IS PRIMARY VERTEX IN ALEPH COORDINATES
C   * DJET(,) IS TWO NORMALIZED JET DIRECTIONS IN ALEPH COORDINATES
C   * SVTX(,) IS TWO SECONDARY VERTICES IN TRANSLATED-ROTATED COORDINATES
C       ORIGIN AT PVTX, 3D AXIS IN DJET DIRECTIONS
C  Output Arguments :
C   * JET=1 OR 2 ACCORDING TO JET DIRECTION
C   * IVX=1 FOR PRIMARY,
C   * IVX=2 FOR SECONDARY (IN ABOVE JET)
C   * IVX=0 FOR TRACK PASSING CUTS BUT NOT CLOSE TO EITHER VERTEX
C   * IVX=-1 FOR TRACK FAILING CUTS
C  Entry point : QVSTCHI2 (DCHI2)
C   * DCHI2 IS CHI**2 FOR TRACK TO COME FROM PRIMARY VERTEX MINUS
C              CHI**2 TO COME FROM SECONDARY.
C
C ------------------------------------------------------------------------------
#ifndef DOC
      SAVE DCHI2
      REAL DCHI2
      DIMENSION PVTX(3),DJET(3,2),SVTX(3,2)
      DIMENSION EPVTX(3),ESVTX(3)
      LOGICAL QVSGTK2
#include "nancom.h"
C ------------------------------------------------------------------------------
C  IF NANO, CALL NVSTVA
C
      IF (XNANO) THEN
        CALL NVSTVA(ITK,JET,IVX)
        RETURN
      ENDIF
C
C  CHECK WHICH JET
      CALL QVSTKJ(ITK,DJET,JET)
C
C  ASSUME IT FAILS CUTS
      IVX=-1
C
C  CHECK (LOOSE) TRACK CUTS
      DKL=ABS(SVTX(3,JET))
      IF (.NOT. QVSGTK2(ITK,PVTX,DKL)) RETURN
C
C  MAKE UP SOME NOMINAL VERTEX ERRORS
      EPVTX(1)=.0050
      EPVTX(2)=.0020
      EPVTX(3)=.0050
      ESVTX(1)=.0050
      ESVTX(2)=.0050
      ESVTX(3)=.0350
C
C  GET NORMALIZED AND SIGNED IMPACT PARAMETERS
C  RELATIVE TO BOTH VERTICES
      CALL QVSIMP(ITK,PVTX,DJET(1,JET),SVTX(1,JET),EPVTX,ESVTX,
     > DXP,DZP,DTP,DXS,DZS,DTS)
C
C  NORMALIZED DISTANCE CUT
      SIGCUT=3.0
C
C  WE WON'T USE SIGN INFORMATION
      DXP=ABS(DXP)
      DZP=ABS(DZP)
      DXS=ABS(DXS)
      DZS=ABS(DZS)
C
      IF (MIN(DXP,DZP,DXS,DZS) .GT. SIGCUT) THEN
C  TRACK IS FAR FROM EVERYTHING
        IVX=0
        DCHI2 = 0.0
      ELSEIF (MIN(DZP,DZS) .GT. SIGCUT) THEN
C  FAR FROM BOTH IN Z, BUT X IS OK
        IF (DXS .LT. DXP) THEN
          IVX=2
        ELSE
          IVX=1
        ENDIF
        DCHI2 = DXP**2 - DXS**2
      ELSEIF (MIN(DXP,DXS) .GT. SIGCUT) THEN
C  FAR FROM BOTH IN X, BUT Z IS OK
        IF (DZS .LT. DZP) THEN
          IVX=2
        ELSE
          IVX=1
        ENDIF
        DCHI2 = DZP**2 - DZS**2
      ELSE
C  WITHIN SIGCUT IN BOTH X AND Z TO ONE VERTEX OR THE OTHER
C  (ALTHOUGH NOT NECESSARILY THE SAME ONE!)
C  THIS IS THE NORMAL (AND PREFERRED) PATH
C
C  CALCULATE CHI SQUARE TO BOTH VERTICES
        CHI2P=DXP**2+DZP**2
        CHI2S=DXS**2+DZS**2
C
C  ASSIGN ACCORDING TO CHI SQUARE
        IF (CHI2S .LT. CHI2P) THEN
          IVX=2
        ELSE
          IVX=1
        ENDIF
        DCHI2 = CHI2P - CHI2S
      ENDIF
C
      RETURN
C
      ENTRY QVSTCHI2 (TCHI2)
      TCHI2 = DCHI2
C
      END
#endif

