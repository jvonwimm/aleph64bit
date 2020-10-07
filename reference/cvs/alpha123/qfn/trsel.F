      SUBROUTINE TRSEL(IP,NJET,TRKJET,NV0,V0PAIR,V0PAR,V0ERR,V0PAT,
     &  HAXIS,NDAU,TRKDAU,NTRACK,TPAR,TERR,TFLAG,VDHITP,FRF2TRK,
     &     JJET,JHEMI)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Select tracks for impact parameter physics, and pack them into a loca
C  Dave Brown 5-7-91
C  Modified:
C  ---------
C  03/05/95   STEFAN SCHAEL, WORK CORRECTLY WITH USER SELECTED TRACKS
C  04/09/96   John Carr redefine track type
C
C            ITYPE = 1  2 VDET space point hits
C                  = 2  1 VDET space point hit , expect only 1
C                  = 3  V0
C                  = 4  1 VDET space point hit , expect 2
C                  = 5  Lots of ITC hits, no VDET at all,expect 0
C                  = 6  Lots of ITC hits, no VDET at all,expect some
C                  = 7  1 single r-phi or z hit, expect 1
C                  = 8  1 single r-phi or z hit, expect 2
C                  = 9  2 single view VDET hits
C
C  Called from QIPBTAG
C
C  INPUT :
C          IP(3)     :   IP position
C          NJET      :   # OF JETS PASSED FOR ANALYSIS
C          TRKJET    :   track # after sorting the jets      i=1,ntrack
C          NV0       :   # of found V0s
C          V0PAIR    :   Track #s of V0s, to be ignored in the analysis
C          V0PAR     :   Track parameters of reconstructed V0s
C          V0ERR     :   Error matrix of reconstructed V0 parameters
C          V0PAT     :   VDET Hit pattern on V0 track
C          HAXIS     :   Hemisphere definition axis
C          VDHITP    : Track vdet hit pattern
C
C  OUTPUT:
C
C          FRF2TRK   : row # OF FRFT bank for track i        i=1,ntrack
C          TFLAG     : Track type flag
C          NTRACK    : # of tracks used for analysis
C          JJET(*)   : # of assigned Jet                     i=1,ntrack
C          JHEMI(*)  : # of assigned hemisphere              i=1,ntrack
C          TPAR(5,*) : Track parameter
C          TERR(4,4,*): Track fit errors parameter
C
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Global includes
C
#include "qdecl.h"
#include "qcde.h"
#include "maxtrk.h"
      INTEGER NJET,TRKJET(*)
      INTEGER NTRACK
      INTEGER TFLAG(*),VDHITP(*),FRF2TRK(*),JJET(*),JHEMI(*)
      INTEGER NV0,V0PAIR(2,*),V0PAT(*)
      INTEGER NDAU(*),TRKDAU(MAXDAU,*)
      REAL    V0PAR(5,*),V0ERR(5,5,*)
      REAL    TPAR(5,*),TERR(4,4,*)
      REAL    IP(3)
C
#include "btpar.h"
C
C  Locals
C
      INTEGER NLINK
      INTEGER NEXTRA, KVTXT, IVHI, IEXFLAG, IHF, NEX
      INTEGER IPAR,JPAR,IVIEW,IPAT,ITYPE,MTRACK
      INTEGER JET,ICOR
      INTEGER IALTRK,JALTRK,ITRK,IJET,KJET,KHEMI,IV0
      INTEGER NU,NW,NDOF
      INTEGER IDAU
      REAL P, CTH, U, W
      REAL D0,PHI,Z0,DUMMY
      REAL SIG2_D0,SIG2_Z0,CHI,CHIDOF
      REAL SIG_D0,SIG_Z0
      REAL TANL,COSL,SINL,COSP,SINP,SDIST
      REAL MAXDOT,JDOT,HAXIS(3)
      LOGICAL USEDTRK(MAXTRK)
      LOGICAL NEWCOD
C ----------------------------------------------------------------------
C  Inline functions
C
#include "btaginl.h"
#include "qmacro.h"
C ----------------------------------------------------------------------
      CALL CODSEL(NEWCOD)
C
C  Start by zeroing a few pointers
C
      NTRACK = 0
      CALL VZERO(USEDTRK,KNCHT)
C
C  Look for user-selected tracks.  These will always be accepted, even
C  if the tracks would otherwise fail a cut.
C
      DO ITYPE=INTYPE+1,NTYPE
        IALTRK = KPDIR(USRNAME(ITYPE),KRECO)
        DO WHILE(IALTRK.GT.0)
          NTRACK = NTRACK + 1
C
C  Pack the parameters and error matrix.  This also determines the
C  'daughter' tracks, and the associated jet and hemisphere.
C
          CALL TPARSET(IALTRK,HAXIS,NJET,TRKJET,
     &         NDAU(NTRACK),TRKDAU(1,NTRACK),
     &         TPAR(1,NTRACK),TERR(1,1,NTRACK),
     &         JJET(NTRACK),JHEMI(NTRACK))
C
C  Find the vdet hit pattern (OR of all daughters), and flag off
C  the daughters from further use
C
          IPAT = 0
          DO IDAU=1,NDAU(NTRACK)
            ITRK = TRKDAU(IDAU,NTRACK)
            IPAT = IOR(IPAT,VDHITP(ITRK))
            USEDTRK(ITRK) = .TRUE.
          END DO
          IVIEW = 3
          TFLAG(NTRACK) = TPACK(ITYPE,IVIEW,IPAT)
C
C  If this is an FRFT track (exactly 1 'daughter'),
C  then the track is the daughter.  Otherwise, just use the
C  alpha track number + 20000
C
C SSC 03/05/95
C
          IF (NDAU(NTRACK) .EQ. 1) THEN
             ITRK            = TRKDAU(1,NTRACK)
             FRF2TRK(NTRACK) = ITRK
             IF (ITRK.LE.MAXTRK) USEDTRK(ITRK) = .TRUE.
          ELSE
             FRF2TRK(NTRACK) = 20000 + IALTRK
          ENDIF
C
C  Move to the next track of this user type
C
          IALTRK = KFOLLO(IALTRK)
        END DO
      END DO
C
C  Loop over the V0s.  These have been pre-selected in basic ways
C  so some cuts are skipped.
C
      DO IV0=1,NV0
        IPAT = V0PAT(IV0)
        IF(IDB.GT.0)CALL HF1(IDB+200,0.,1.)
C
C  Flag the constituent tracks for removal
C
        DO ITRK=1,2
          USEDTRK(V0PAIR(ITRK,IV0))=.TRUE.
        END DO
C
C  Cut on the hits on the constituent tracks
C
        IALTRK = V0PAIR(1,IV0)+KFCHT-1
        JALTRK = V0PAIR(2,IV0)+KFCHT-1
        IF(ONEHIT(IPAT))THEN
          ITYPE = 3
        ELSE
          ITYPE = 0
        END IF
        IF(IDB.GT.0)CALL HF1(IDB+204,FLOAT(ITYPE),1.)
        IF(ITYPE.LE.0)GOTO 998
        IF(IDB.GT.0)CALL HF1(IDB+200,5.,1.)
C
C  Unpack the needed parameters
C
        TANL = V0PAR(2,IV0)
        PHI = V0PAR(3,IV0)
        D0  = V0PAR(4,IV0)
        Z0  = V0PAR(5,IV0)
C
C  Move D0 and Z0 to IP
C
        COSP = COS(PHI)
        SINP = SIN(PHI)
        COSL = 1./SQRT(1.0+TANL**2)
        SINL = TANL*COSL
        D0 = D0 - SINP*IP(1) + COSP*IP(2)
        SDIST = IP(1)*COSP + IP(2)*SINP
        Z0 = Z0 + TANL*SDIST - IP(3)
C
C  Calculate d0,z0 errors
C
        SIG2_D0 = V0ERR(4,4,IV0)
        SIG2_Z0 = V0ERR(5,5,IV0)/(1.+TANL**2)
        if(SIG_D0.gt.0.) SIG_D0 = SQRT(SIG2_D0)
        if(SIG_Z0.gt.0.) SIG_Z0 = SQRT(SIG2_Z0)
C
C  Cut on absolute distance from IP; this should be a loose cut
C
        IF(IDB.GT.0)THEN
          CALL HF1(IDB+206,D0,1.0)
          CALL HF1(IDB+207,Z0,1.0)
          CALL HF1(IDB+209,SIG_D0,1.0)
          CALL HF1(IDB+210,SIG_Z0,1.0)
        END IF
        IF(SIG_D0.GT.MAX_ERR.OR.SIG_Z0.GT.MAX_ERR.OR.
     &       ABS(D0).GT.D0_CUT .OR. ABS(Z0).GT.Z0_CUT)GOTO 998
C
C  Assign tracks to jets
C
        KJET=0
        MAXDOT=-1000.
        DO IJET = 1 , NJET
          JET=TRKJET(IJET)
          JDOT = (QX(JET)*COSL*COSP + QY(JET)*COSL*SINP +
     &          QZ(JET)*SINL)/QP(JET)
          IF ( JDOT.GT.MAXDOT ) THEN
            MAXDOT=JDOT
            KJET  = IJET
          END IF
        END DO
C
C  Cut on the jet assignment; remove tracks assigned to 4th or smaller
C  jets, and require the angle between track and jet be 'small'
C
        IF(IDB.GT.0)CALL HF1(IDB+211,FLOAT(KJET),1.0)
        IF(KJET.GT.NUMJET)GOTO 998
        IF(IDB.GT.0)CALL HF1(IDB+200,11.,1.)
        IF(IDB.GT.0)CALL HF1(IDB+212,MAXDOT,1.0)
        IF(MAXDOT .LT. DOTCUT )GOTO 998
        IF(IDB.GT.0)CALL HF1(IDB+200,12.,1.)
C
C  Good track; fill the arrays
C
        NTRACK = NTRACK + 1
        DO IPAR=1,5
          TPAR(IPAR,NTRACK) = V0PAR(IPAR,IV0)
        END DO
C
C  Error matrix; we skip the curvature term
C
        DO IPAR=2,5
          DO JPAR=2,5
            TERR(IPAR-1,JPAR-1,NTRACK) = V0ERR(IPAR,JPAR,IV0)
          END DO
        END DO
        FRF2TRK(NTRACK)=10000 + IV0
        JJET(NTRACK) = KJET
        TFLAG(NTRACK) = TPACK(ITYPE,3,IPAT)
C
C  Determine which hemisphere to put the track
C
        JDOT = HAXIS(1)*COSP + HAXIS(2)*SINP + HAXIS(3)*TANL
        IF ( JDOT.GT.0. )THEN
          JHEMI(NTRACK) = 1
        ELSE
          JHEMI(NTRACK) = 2
        END IF
C
C  Set the daughter tracks
C
        NDAU(NTRACK) = 2
        DO IDAU=1,2
          TRKDAU(IDAU,NTRACK) = V0PAIR(IDAU,IV0)
        END DO
 998    CONTINUE
      END DO
C
C ----------------------------------------------------------------------
C  Now the regular tracks
C
      DO IALTRK=KFCHT,KLCHT
        ITRK = IALTRK-KFCHT+1
        IPAT = VDHITP(ITRK)
C
C  Skip the ones coming from V0s
C
        IF(.NOT.USEDTRK(ITRK))THEN
C
C  Cut on momentum
C
          IF(IDB.GT.0)CALL HF1(IDB+100,0.,1.)
          IF(IDB.GT.0)CALL HF1(IDB+101,QP(IALTRK),1.0)
          IF( QP(IALTRK).LT.MINMOM.OR.QP(IALTRK).GT.MAXMOM)GOTO 999
          IF(IDB.GT.0)CALL HF1(IDB+100,1.,1.)
C
C  Cut on number of TPC hits
C
          IF(IDB.GT.0)THEN
            CALL HF1(IDB+102,FLOAT(KFRTNI(IALTRK)),1.0)
            CALL HF1(IDB+103,FLOAT(KFRTNT(IALTRK)),1.0)
          END IF
          IF(KFRTNT(IALTRK) .LT. MINN(3))GOTO 999
          IF(IDB.GT.0)CALL HF1(IDB+100,3.,1.)
C
C  Cut on number of VDET hits
C
          IF (.NOT.NEWCOD) THEN
          IF(TWOHIT(IPAT))THEN
            ITYPE = 2 !  2 space point hits
            IVIEW = 3
          ELSE IF(ONEHIT(IPAT))THEN
            ITYPE = 1 ! 1 space point hit
            IVIEW = 3
          ELSE IF(TWORPHIT(IPAT).OR.TWOZHIT(IPAT))THEN
            ITYPE = 4 !  2 single view hits
            IF(TWORPHIT(IPAT))THEN
              IVIEW = 1
            ELSE
              IVIEW = 2
            END IF
          ELSE IF(RPHIT(IPAT))THEN
            ITYPE = 5  !single r-phi hit
            IVIEW = 1
          ELSE IF(ZHIT(IPAT))THEN
            ITYPE = 6 !  1 single view Z hit
            IVIEW = 2
          ELSE IF(KFRTNI(IALTRK).GT.MINN(2))THEN
            ITYPE = 7 !  Lots of ITC hits, no VDET at all
            IVIEW = 1
          ELSE
            ITYPE = 0
            IVIEW = 1
          END IF
          ELSE
C
C  CHANGE DEFINITION OF TRACK TYPE "ITYPE"  (JC 3 SEPT 1996)
C       find number of intersections of track with VDET wafers
C
          NEXTRA=0
          KVTXT=NLINK('VTXT',ITRK)
          IF(KVTXT.GT.0) THEN
            NEX=IW(KVTXT+2)
C          test if extrapolation near edge of wafer in W
            DO IVHI=1,NEX
              IEXFLAG=ITABL(KVTXT,IVHI,2)
              IF(IAND(IEXFLAG,16).GT.0) NEXTRA=NEXTRA+1
            ENDDO
          ENDIF
          IF(TWOHIT(IPAT))THEN
            ITYPE = 1 !  2 space point hits
            IVIEW = 3
          ELSE IF(ONEHIT(IPAT))THEN
             IF(NEXTRA.EQ.1) THEN
                ITYPE = 2 ! 1 space point hit , expect only 1
             ELSE
                ITYPE = 4 ! 1 space point hit , expect 2 (or 0)
             ENDIF
            IVIEW = 3
          ELSE IF(TWORPHIT(IPAT).OR.TWOZHIT(IPAT))THEN
            ITYPE = 9 !  2 single view hits
            IF(TWORPHIT(IPAT))THEN
              IVIEW = 1
            ELSE
              IVIEW = 2
            END IF
          ELSE IF(RPHIT(IPAT))THEN
            IF(NEXTRA.EQ.1) THEN
               ITYPE = 7  !single r-phi hit, expect 1
            ELSE
               ITYPE = 8  !single r-phi hit, expect 2
            ENDIF
            IVIEW = 1
          ELSE IF(ZHIT(IPAT))THEN
            IF(NEXTRA.EQ.1) THEN
               ITYPE = 7  !single Z hit, expect 1
            ELSE
               ITYPE = 8  !single Z hit, expect 2
            ENDIF
            IVIEW = 2
          ELSE IF(KFRTNI(IALTRK).GT.MINN(2))THEN
            IF(NEXTRA.EQ.0) THEN
                ITYPE = 5 !  Lots of ITC hits, no VDET at all,expect 0
             ELSE
                ITYPE = 6 !  Lots of ITC hits, no VDET at all,expect som
             ENDIF
            IVIEW = 1
          ELSE
            ITYPE = 0
            IVIEW = 1
          END IF
          P=QP(IALTRK)
          CTH=QCT(IALTRK)
          IF(NEXTRA.GT.0) THEN
             IHF=ITABL(KVTXT,1,2)
             U=RTABL(KVTXT,1,3)
             W=RTABL(KVTXT,1,4)
          ENDIF
          ENDIF
C-----------------------END OF CHANGES

          IF(IDB.GT.0)CALL HF1(IDB+104,FLOAT(ITYPE),1.0)
          IF(ITYPE.LE.0.OR.ITYPE.GT.NTYPE)GOTO 999
          IF(IDB.GT.0)CALL HF1(IDB+100,5.,1.)
C
C  Unpack the needed parameters
C
          D0  = QFRFD0(IALTRK)
          PHI = QFRFP0(IALTRK)
          Z0  = QFRFZ0(IALTRK)
          TANL = QFRFTL(IALTRK)
          IF(KFRFDF(IALTRK).GT.0)THEN
            CHI = QFRFC2(IALTRK)
            NDOF = KFRFDF(IALTRK)
            CHIDOF = CHI/NDOF
          ELSE
            CHIDOF = QFRFC2(IALTRK)
          END IF
          SIG2_D0 = QFRFEM(IALTRK,4,4)
          SIG2_Z0 = QFRFEM(IALTRK,5,5)/(1.+TANL**2)
C
C  Move D0 and Z0 to IP
C
          COSP = COS(PHI)
          SINP = SIN(PHI)
          D0 = D0 - SINP*IP(1) + COSP*IP(2)
          SDIST = IP(1)*COSP + IP(2)*SINP
          Z0 = Z0 + TANL*SDIST - IP(3)
C
C  Cut on chisquared/DOF
C
          IF(IDB.GT.0)CALL HF1(IDB+108,CHIDOF,1.0)
          IF(CHIDOF.GT.CHI_CUT)GOTO 999
          IF(IDB.GT.0)CALL HF1(IDB+100,8.,1.)
C
C  Cut on track errors and distance to the IP.  This depends on which
C  track views are being used
C
          if(SIG_D0.gt.0.) SIG_D0 = SQRT(SIG2_D0)
          if(SIG_Z0.gt.0.) SIG_Z0 = SQRT(SIG2_Z0)
          IF( IVIEW.EQ.3 )THEN  ! 3-d track
            IF(IDB.GT.0)THEN
              CALL HF1(IDB+106,D0,1.0)
              CALL HF1(IDB+107,Z0,1.0)
              CALL HF1(IDB+109,SIG_D0,1.0)
              CALL HF1(IDB+110,SIG_Z0,1.0)
            END IF
            IF(SIG_D0.GT.MAX_ERR.OR.SIG_Z0.GT.MAX_ERR .OR.
     &         ABS(D0).GT.D0_CUT .OR. ABS(Z0).GT.Z0_CUT)GOTO 999
          ELSE IF(IVIEW.EQ.1)THEN  ! R-phi only track
            IF(IDB.GT.0)THEN
              CALL HF1(IDB+106,D0,1.0)
              CALL HF1(IDB+109,SIG_D0,1.0)
            END IF
            IF(SIG_D0.GT.MAX_ERR.OR.ABS(D0).GT.D0_CUT)GOTO 999
          ELSE IF(IVIEW.EQ.2)THEN ! R-Z only track
            IF(IDB.GT.0)THEN
              CALL HF1(IDB+107,Z0,1.0)
              CALL HF1(IDB+110,SIG_Z0,1.0)
            END IF
            IF(SIG_Z0.GT.MAX_ERR .OR. ABS(Z0).GT.Z0_CUT)GOTO 999
          END IF
          IF(IDB.GT.0)CALL HF1(IDB+100,10.,1.)
C
C  Conditionally a good track; pack the parameters
C
          MTRACK = NTRACK+1
          CALL TPARSET(IALTRK,HAXIS,NJET,TRKJET,
     &         NDAU(MTRACK),TRKDAU(1,MTRACK),
     &         TPAR(1,MTRACK),TERR(1,1,MTRACK),
     &         KJET,JHEMI(MTRACK))
C
C  Cut on the jet assignment; remove tracks assigned to 4th or smaller
C  jets, and require the angle between track and jet be 'small'
C
          MAXDOT = QCOSA(IALTRK,TRKJET(KJET))
          IF(IDB.GT.0)CALL HF1(IDB+111,FLOAT(KJET),1.0)
          IF(KJET.GT.NUMJET)GOTO 999
          IF(IDB.GT.0)CALL HF1(IDB+100,11.,1.)
          IF(IDB.GT.0)CALL HF1(IDB+112,MAXDOT,1.0)
          IF(MAXDOT .LT. DOTCUT )GOTO 999
          IF(IDB.GT.0)CALL HF1(IDB+100,12.,1.)
C
C  Good track; finish filling the arrays
C
          NTRACK = NTRACK + 1
          FRF2TRK(NTRACK)=ITRK
          TFLAG(NTRACK) = TPACK(ITYPE,IVIEW,IPAT)
          JJET(NTRACK) = KJET
C
  999     CONTINUE
        END IF
      END DO
      RETURN
      END
#endif
