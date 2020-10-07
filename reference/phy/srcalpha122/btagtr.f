      SUBROUTINE BTAGTR(FITP,NTRACK,DMIN,S_DMIN,TFLAG,PROBTRK)
C-----------------------------------------------------------------------
C! Calculate the probability of the track to come from IP
CKEY   QIPBTAG / INTERNAL
C  Called from QIPBTAG
C  Author  Dave Brown  29-1-93
C
C  SUBROUTINE BTAGTR
C  ==================
C
C  INPUT :
C          FITP      #koeffizents of track fit polynomial...
C
C          NTRACK == # OF TRACKS PASSED FOR ANALYSIS
C
C          DMIN(i)                                           i=1,ntrack
C          --  distance of track from the primary vertex
C
C          S_DMIN(i)                                         i=1,ntrack
C          --  error on this distance
C
C          TFLAG(i)                                         i=1,ntrack
C          --  flag for this track
C
C OUTPUT:
C          PROBTRK(i)
C          --  probability of a track
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTRACK, TFLAG(*)
      REAL DMIN(*), S_DMIN(*), PROBTRK(*)
      REAL FITP(5,*)
C
C  Local variables
C
      INTEGER ITRK,ITYPE,IFLAG,IVIEW
      REAL*8 EXPMAX,PROB,SIG,ROOT2,GAUMAX
      DATA GAUMAX/10.0D0/
      DATA EXPMAX/50.0D0/,ROOT2/1.41421356/
C
C  Inline functions
C
      INTEGER IIII
      REAL*8 XXXX
      REAL*8 F1,F2,F3,TPROB,G1,G2,G3,TPROB1
      REAL*8 DERFC
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
C
C  Probability functions; these are the integrals of common
C  functions.
C
      F1(XXXX,IIII) = EXP(-MIN((XXXX/FITP(3,IIII))**2/2.,EXPMAX))
      F2(XXXX,IIII) = (1.+ABS(XXXX)/FITP(4,IIII))*
     &                EXP(-MIN(ABS(XXXX)/FITP(4,IIII),EXPMAX))
      F3(XXXX,IIII) = (1.+ABS(XXXX)/FITP(5,IIII))*
     &                EXP(-MIN(ABS(XXXX)/FITP(5,IIII),EXPMAX))
      TPROB(XXXX,IIII) = SIGN(
     & (1.-FITP(1,IIII)-FITP(2,IIII))*F1(XXXX,IIII)+
     & FITP(1,IIII)*F2(XXXX,IIII)+ FITP(2,IIII)*F3(XXXX,IIII),XXXX)
C
C  Now also some functions for 1-view tracks
C
      G1(XXXX,IIII) = DERFC(MIN(ABS(XXXX)/(FITP(3,IIII)*ROOT2),
     &      GAUMAX))
      G2(XXXX,IIII) = EXP(-MIN(ABS(XXXX)/FITP(4,IIII),EXPMAX))
      G3(XXXX,IIII) = EXP(-MIN(ABS(XXXX)/FITP(5,IIII),EXPMAX))
      TPROB1(XXXX,IIII) = SIGN(
     & (1.-FITP(1,IIII)-FITP(2,IIII))*G1(XXXX,IIII)+
     & FITP(1,IIII)*G2(XXXX,IIII)+ FITP(2,IIII)*G3(XXXX,IIII),XXXX)
C
C-----------------------------------------------------------------------
C
C  Loop over the tracks
C
      DO ITRK=1,NTRACK
C
C  Compute the probability for this track to have come from the
C  IP given that it's distance/error is DMIN/SIG OR LARGER
C  Separate probability functions by number of hits
C
        SIG = DMIN(ITRK)/S_DMIN(ITRK)
        IFLAG = TFLAG(ITRK)
        ITYPE = TTYPE(IFLAG)
        IVIEW = TVIEW(IFLAG)
        IF(IVIEW.EQ.3)THEN
          PROB = TPROB(SIG,ITYPE)
        ELSE
          PROB = TPROB1(SIG,ITYPE)
        END IF
        PROBTRK(ITRK)= PROB
      END DO
      RETURN
      END
