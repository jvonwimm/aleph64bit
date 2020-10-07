      SUBROUTINE MAKE2D(IFRFT,TPAR,TERR,TFLAG,JETS,PHIP,PERP,
     &     JDIST,DMIN,S_DMIN)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C
C! Convert a 3-d track into a 2-d
C   Author  Dave Brown, 11-12-93
C
C   Called from TRSEL2
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C  In/out variables
C
      INTEGER IFRFT,TFLAG
      REAL JETS(3),PHIP,TPAR(5),TERR(4,4)
      REAL PERP(3)
      REAL JDIST,LDIST,DMIN,S_DMIN
C
C  Local variables
C
      INTEGER ICOR,NUSED(2),IVIEW,ITYPE,IPAT
      REAL IP(3),SIG2_IP(3,3)
      REAL NORM,J1(3),JETSIG2,JPERP(3)
      REAL MPHI,SDIST,S_LDIST
      REAL TWOPI/6.28318531/,PI/3.14159265/,HALFPI/1.57079633/
      REAL PHICUT/1.0/
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
C
C  Get the IP without this track
C
      CALL IPTRK(1,IFRFT,IP,SIG2_IP,NUSED)
C
C  Normalize the jet axis for this jet, and get the jet direction error
C
      NORM = 0.0
      DO ICOR=1,3
        NORM = NORM + JETS(ICOR)**2
      END DO
      NORM = SQRT(NORM)
      DO ICOR=1,3
        J1(ICOR) = JETS(ICOR)/NORM
      END DO
      CALL JETERR(1,JETS,JETSIG2)
C
C  Choose which view based on the direction
C  of the closest approach to the jet
C
      MPHI = ABS(PHIP)
      IF(MPHI.GT.HALFPI)MPHI=PI-MPHI
      IF(MPHI.LE.PHICUT)THEN
C
C  The distance to the jet axis points mostly in the r-z direction;
C  assume there's a pattern recognition error there and take the track
C  only in the r-phi view
C
        IVIEW=1
      ELSE
C
C  Same thing, only take the r-z view
C
        IVIEW=2
      END IF
      CALL DMIN2D(IVIEW,TPAR,IP,J1,JDIST,DMIN,SDIST,PERP)
C
C  Reset the track flag
C
      ITYPE = TTYPE(TFLAG)
      IPAT = TVDPAT(TFLAG)
      IF(ITYPE.EQ.2)THEN
        ITYPE = 4
      ELSE
        IF(IVIEW.EQ.1)THEN
          ITYPE = 5
        ELSE
          ITYPE = 6
        END IF
      END IF
      TFLAG = TPACK(ITYPE,IVIEW,IPAT)
C
C  Copy the perpendicular vector to the jet perpendicular direction
C
      DO ICOR=1,3
        JPERP(ICOR) = PERP(ICOR)
      END DO
C
C  Get the error on the impact parameter
C
      CALL DMINERR(TPAR,TERR,PERP,JPERP,JETSIG2,JDIST,
     &     SDIST,SIG2_IP,S_DMIN,S_LDIST,PHIP)
      RETURN
      END
