      SUBROUTINE TCROSS(TPAR,TFLAG,IP,JET,JDIST,SDIST,LDIST,
     &  T0,T1,PERP,IFLAG)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Finds the point of closest approach between a track helix and a line
C! in 3-space.  It returns the arc distance
C  along the track, relative to the nominal track 'origin',
C  and the perpendicular distance between the
C  track and the line at the closest approach.
C  Author                                            Dave Brown, 13-4-92
C  Called from FINDDMIN
C ----------------------------------------------------------------------
      IMPLICIT NONE
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
      INTEGER ICOR
      INTEGER NLOOP,ILOOP,MAXLOOP,IFLAG,TFLAG
      REAL TPAR(5),IP(3),JET(3)
      REAL IR,TANL,PHI,D0,Z0
      REAL COSL,SINL,COSP,SINP,COSP0,SINP0
      REAL T0(3),T1(3),PERP(3)
      REAL SDIST,LDIST,JDIST
      REAL DS_CUT,FACTOR
      REAL CHARGE
      REAL*8 DJDIST,DLDIST
      REAL*8 DOT,QVEC(3),DS_PERP,DS,NORM
      DATA DS_CUT/0.0002/,FACTOR/.6/,MAXLOOP/5/
C ----------------------------------------------------------------------
C  Unpack the track parameters
C
      IR  = TPAR(1)
      TANL= TPAR(2)
      PHI = TPAR(3)
      D0  = TPAR(4)
      Z0  = TPAR(5)
C
C  Z direction stuff doesn't change because of curvature; calcluate it
C  just once.
C
      COSL = 1./SQRT(1.+TANL**2)
      SINL = TANL*COSL
      COSP0 = COS(PHI)
      SINP0 = SIN(PHI)
      IF(TFLAG.LE.2)THEN
C
C  Loop until the track position doesn't appreciably change.  The conver
C  test is made using the change in distance BEFORE projecting it along
C  the track direction, this to prevent oscillation problems for tracks
C  nearly parallel to the jet direction.
C
        DS_PERP = 1.
        SDIST = 0.0
        ILOOP=0
        DO WHILE(ABS(DS_PERP) .GT. DS_CUT .AND. ILOOP.LT.MAXLOOP)
C
C  Approximate the track as a line, evaluated at the current point.
C  T1 is a unit vector pointing in the track tangential direction,
C  T0 is a point on the track.
C
          COSP = COS(IR*SDIST+PHI)
          SINP = SIN(IR*SDIST+PHI)
          T1(1) =  COSL*COSP
          T1(2) =  COSL*SINP
          T1(3) =  SINL
          T0(1) =   (SINP-SINP0)/IR + D0*SINP0
          T0(2) =  -(COSP-COSP0)/IR - D0*COSP0
          T0(3) =  Z0+TANL*SDIST
C
C  Solve for the vector perpendicular to the track, but lying in
C  a plane that also includes the jet direction.
C
          DOT = 0.0
          DO ICOR=1,3
            DOT = DOT + JET(ICOR)*T1(ICOR)
          END DO
          DO ICOR=1,3
            QVEC(ICOR) = T1(ICOR)-DOT*JET(ICOR)
          END DO
C
C  Solve for the arc length change
C
          DS_PERP = 0.0
          DOT = 0.0
          DO ICOR=1,3
            DS_PERP  = DS_PERP  + QVEC(ICOR)*(IP(ICOR)-T0(ICOR))
            DOT = DOT + QVEC(ICOR)*T1(ICOR)
          END DO
          IF(DOT.EQ.0.0)DOT=0.000000001
          DS = DS_PERP/DOT
C
C  Update the value of the total arc-length; damp this by a factor,
C  to prevent oscillations.
C
          SDIST = SDIST + FACTOR*DS
          ILOOP = ILOOP + 1
        END DO
C
C  Test
C
        IF(IDB.GT.0)THEN
          CALL HFILL(IDB+50,FLOAT(ILOOP),1.0,1.0)
        END IF
C
        IF(ILOOP .LT. MAXLOOP)THEN
          IFLAG=0
        ELSE
          IFLAG=1
        END IF
      ELSE
C
C  Neutral tracks have a closed-form solution
C
        T1(1) =  COSL*COSP0
        T1(2) =  COSL*SINP0
        T1(3) =  SINL
        T0(1) =  D0*SINP0
        T0(2) =  -D0*COSP0
        T0(3) =  Z0
        DOT = 0.0
        DO ICOR=1,3
          DOT = DOT + JET(ICOR)*T1(ICOR)
        END DO
        DO ICOR=1,3
          QVEC(ICOR) = T1(ICOR)-DOT*JET(ICOR)
        END DO
C
C  Solve for the arc length change
C
        DS_PERP = 0.0
        DOT = 0.0
        DO ICOR=1,3
          DS_PERP  = DS_PERP  + QVEC(ICOR)*(IP(ICOR)-T0(ICOR))
          DOT = DOT + QVEC(ICOR)*T1(ICOR)
        END DO
        IF(DOT.EQ.0.0)DOT=0.000000001
        DS = DS_PERP/DOT
      END IF
C
C  Solve for the distance along the jet direction
C  of the closest point
C
      DJDIST = 0.0
      DO ICOR=1,3
        DJDIST = DJDIST + JET(ICOR)*(T0(ICOR)+T1(ICOR)*DS-IP(ICOR))
      END DO
C
C  Now, solve for the distance between the track and line at this point.
C  For this, we construct the vector perpendicular to the track and the
C  line.
C
      PERP(1) =  T1(2)*JET(3)-T1(3)*JET(2)
      PERP(2) = -T1(1)*JET(3)+T1(3)*JET(1)
      PERP(3) =  T1(1)*JET(2)-T1(2)*JET(1)
      NORM = 0.0
      DLDIST = 0.0
      DO ICOR=1,3
        DLDIST = DLDIST + PERP(ICOR)*(T0(ICOR)-IP(ICOR))
        NORM = NORM + PERP(ICOR)**2
      END DO
      NORM=MAX(SQRT(NORM),1.0D-20)
      DLDIST = DLDIST/NORM
      DO ICOR=1,3
        PERP(ICOR)=PERP(ICOR)/NORM
      END DO
      JDIST = DJDIST
      LDIST = DLDIST
C
      RETURN
      END
