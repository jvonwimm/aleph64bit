      SUBROUTINE FINDDMIN(NTRACK,FRF2TRK,TPAR,TERR,NJET,JETS,JJET,TFLAG,
     &     NDAU,TRKDAU,DMIN,S_DMIN,PHIP,JDIST,LDIST,S_LDIST,PERP)
CKEY   QIPBTAG / INTERNAL
C-----------------------------------------------------------------------
C!  Find the minimum distance between a track and the IP,
C!  with its error,sign and direction, plus related information.
C  Called from QIPBTAG
C  Author                                               Dave Brown 8-12-
C
C  Inputs;
C          NTRACK       Number of tracks
C          FRF2TRK      FRFT number of track
C          TPAR         Track parameters
C          TERR         linear track matrix
C          NJET         Number of jets
C          JETS         Jet momentum vector
C          JJET         Jet-track association pointer
C          TFLAG        Track flag
C          NDAU         # of 'daughter' tracks (=1 for normal tracks)
C          TRKDAU       Track #s of daughter tracks
C
C  Outputs;
C          DMIN         Distance of closest approach to IP
C          S_DMIN       Error on distance of closest approach to IP
C          PHIP         Angle of closest approach to jet in track frame
C          JDIST        Distance along jet axis from IP at closest appro
C          LDIST        Distance of closest approach to jet axis
C          S_LDIST      Error on distance of closest approach to jet axi
C          PERP         Unit vector in direction of closest approach vec
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
C  INPUTS; number of tracks and jets,
C  Jet associated with each track
C  track parameters and diagonal D0-Z0 error matrix fragment
C  jet direction unit vectors,
C  Interaction point with error matrix.
C
C
C --     MAX. NUMBER OF TRACKS, jets, V0s ALLOWED
C
      INTEGER    MAXTRK,MAXJET,MAXV0,MAXDAU
      PARAMETER ( MAXTRK = 400, MAXJET = 40 , MAXV0 = 20 , MAXDAU = 10 )
C
C  Track probabilty function koeffizients
C
      INTEGER         NFIT,MAXTYPE,NTYPE,INTYPE
      PARAMETER      (NFIT=5,MAXTYPE=10)
      INTEGER NVIEW(MAXTYPE)
      REAL            FITP(NFIT,MAXTYPE)
      LOGICAL CALIB,PARS
      CHARACTER*80 CALFILE
      CHARACTER*8 USRNAME(MAXTYPE)
      COMMON/FITPAR/FITP,CALIB,PARS,CALFILE,NTYPE,INTYPE,USRNAME,NVIEW
C
C  Track cuts
C
      INTEGER MINN(3),IDB,NUMJET
      REAL D0_CUT,Z0_CUT,MINMOM,MAXMOM
      REAL MAX_ERR,CHI_CUT,DOTCUT
      REAL LCUT(MAXTYPE),LSIGCUT(MAXTYPE),SCUT(MAXTYPE)
      REAL MXDMIN(MAXTYPE),MXSDMIN(MAXTYPE)
      COMMON/TRCUT/D0_CUT,Z0_CUT,MINMOM,MAXMOM,
     & MAX_ERR,CHI_CUT,MINN,IDB,
     & MXDMIN,MXSDMIN,NUMJET,DOTCUT,LCUT,LSIGCUT,SCUT
C
C  Jet momentum cut
C
      REAL PCUT
      COMMON / JTCUT / PCUT
C
C  Generic cuts
C
      LOGICAL NEGPROB
      COMMON/BTGEN/NEGPROB
      INTEGER NTRACK,NJET,FRF2TRK(*)
      INTEGER JJET(*),TFLAG(*),NDAU(*),TRKDAU(MAXDAU,*)
      REAL TPAR(5,*),TERR(4,4,*),JETS(3,*)
C  OUTPUTS;
C  Minimum distance with error, direction of minimum distance vector
C
      REAL DMIN(*),S_DMIN(*),PHIP(*)
      REAL JDIST(*),LDIST(*),S_LDIST(*),PERP(3,*)
C
C  Local variables
C
      INTEGER IFLAG,IVIEW,ITRK,IPAT
      INTEGER ITRACK,IJET,ICOR,JCOR,IPAR,JPAR
      INTEGER NUSED(2)
      INTEGER IV0
      REAL IP(3),SIG2_IP(3,3)
      REAL J1(3,MAXJET)
      REAL TANL,PHI,D0,Z0
      REAL COSL,SINL
      REAL T0(3),T1(3)
      REAL DOT,DOTP,DOTT,JDOTP,JDOTT
      REAL JPERP(3),SDIST
      REAL NORM,TWOPI
      REAL JETSIG2(MAXJET)
C
C  Inline functions
C
C
C  Inline functions for QIPBTAG routines.  As these include the
C  variable declarations, they should be the first inline comdeck
C  included
C
      INTEGER IPTT,IVEW,ITYP,IFLG
      INTEGER TTYPE,TVIEW,TVDPAT,TPACK
      LOGICAL TWOHIT,INNERHIT,ONEHIT,TWORPHIT,TWOZHIT,RPHIT,ZHIT
      LOGICAL THREED,RPHI,RZ
C
C  These select tracks based on vdet hit pattern
C
      ONEHIT(IPTT) = (IAND(IPTT,3).GT.0.AND.
     &                IAND(ISHFT(IPTT,-4),3).GT.0).OR.
     &               (IAND(ISHFT(IPTT,-2),3).GT.0.AND.
     &                IAND(ISHFT(IPTT,-6),3).GT.0)
      INNERHIT(IPTT) = (IAND(IPTT,3).GT.0.AND.
     &                IAND(ISHFT(IPTT,-4),3).GT.0)
      TWOHIT(IPTT) = IAND(IPTT,3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-2),3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-4),3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-6),3).GT.0
      RPHIT(IPTT) = IAND(IPTT,3).GT.0 .OR.
     &               IAND(ISHFT(IPTT,-2),3).GT.0
      ZHIT(IPTT) = IAND(ISHFT(IPTT,-4),3).GT.0 .OR.
     &               IAND(ISHFT(IPTT,-6),3).GT.0
      TWORPHIT(IPTT) = IAND(IPTT,3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-2),3).GT.0
      TWOZHIT(IPTT) = IAND(ISHFT(IPTT,-4),3).GT.0 .AND.
     &               IAND(ISHFT(IPTT,-6),3).GT.0
C
C  Split and pack the track flag fields
C
      TTYPE(IFLG) = IAND(IFLG,15)
      TVIEW(IFLG) = IAND(ISHFT(IFLG,-4),3)
      TVDPAT(IFLG) = IAND(ISHFT(IFLG,-6),255)
      TPACK(ITYP,IVEW,IPTT) = ITYP+ISHFT(IVEW,4)+ISHFT(IPTT,6)
C ----------------------------------------------------------------------
C  Loop over the jets to normalize their momenta and get their
C  angular resolution
C
      DO IJET=1,NJET
        NORM = 0.0
        DO ICOR=1,3
          NORM = NORM + JETS(ICOR,IJET)**2
        END DO
        NORM = SQRT(NORM)
        DO ICOR=1,3
          J1(ICOR,IJET) = JETS(ICOR,IJET)/NORM
        END DO
      END DO
C
C  Get the jet angular errors
C
      CALL JETERR(NJET,JETS,JETSIG2)
C
C  Loop over the tracks
C
      DO ITRACK=1,NTRACK
C
C  Remove the effects of the 'daughter' tracks from the
C  primary vertex finding; for normal tracks, this is just
C  the track itself
C
        CALL IPTRK(NDAU(ITRACK),TRKDAU(1,ITRACK),IP,SIG2_IP,NUSED)
C
C  Check that some IP information still remains after removing the track
C
        IF(NUSED(1).LT.0)THEN
          DMIN(ITRACK) = 10.0
          S_DMIN(ITRACK) = 0.01
          S_LDIST(ITRACK) = 0.01
          GOTO 1234
        END IF
C
C  Get the jet associated with this track
C
        IJET = JJET(ITRACK)
C
C  Find the distance of closest approach both to the jet axis and
C  the IP.
C
        IFLAG = TFLAG(ITRACK)
        IVIEW = TVIEW(IFLAG)
        IF(IVIEW.EQ.3)THEN
          CALL DMIN3D(TPAR(1,ITRACK),IP,J1(1,IJET),
     &         JDIST(ITRACK),DMIN(ITRACK),LDIST(ITRACK),
     &         SDIST,PERP(1,ITRACK),JPERP)
        ELSE
          CALL DMIN2D(IVIEW,TPAR(1,ITRACK),IP,J1(1,IJET),
     &         JDIST(ITRACK),DMIN(ITRACK),
     &         SDIST,PERP(1,ITRACK))
C
C  Fill by hand the remaining 3-d information as dummy
C
          LDIST(ITRACK) = 0.0
          DO ICOR=1,3
            JPERP(ICOR) = PERP(ICOR,ITRACK)
          END DO
        END IF
C
C  Calculate the errors on the impact paramater
C
        CALL DMINERR(TPAR(1,ITRACK),TERR(1,1,ITRACK),
     &       PERP(1,ITRACK),JPERP,JETSIG2(IJET),JDIST(ITRACK),
     &       SDIST,SIG2_IP,S_DMIN(ITRACK),S_LDIST(ITRACK),PHIP(ITRACK))
C
C  Done
C
 1234   CONTINUE
      END DO
      RETURN
      END
