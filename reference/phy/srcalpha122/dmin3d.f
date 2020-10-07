      SUBROUTINE DMIN3D(TPAR,IP,J1,JDIST,DMIN,LDIST,SDIST,PERP,JPERP)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Finds the point of closest approach between a track and the IP in 3 d
C  Author                                            Dave Brown, 13-4-92
C
C  INPUTS
C     TPAR  :  Standard 5-parameter track description
C     IP    :  Interaction point
C     J1   :  Unit vector in the associated jet direction
C
C  OUTPUTS
C     JDIST :  Distance between IP and closest approach between the
C              track and the jet along jet direction
C     DMIN  :  Distance between track and IP at closest approach
C     LDIST :  Distance between track and jet at closest approach
C     SDIST :  Distance along track at closest approach to the IP
C     PERP  :  Direction between track and IP at closest approach
C     JPERP :  Direction between track and jet at closest approach
C
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
C  Generic cuts
C
      LOGICAL NEGPROB
      COMMON/BTGEN/NEGPROB
      INTEGER ICOR
      REAL TPAR(5),IP(3),J1(3)
      REAL IR,TANL,PHI0,D0,Z0
      REAL COSL,SINL,COSP0,SINP0
      REAL T0(3),T1(3),JPERP(3),PERP(3)
      REAL SDIST,LDIST,JDIST,DMIN
      REAL NHAT(3)
      REAL*8 DENOM,DOT1,DOT2
      REAL*8 DJDIST,DLDIST
      REAL*8 DOT,QVEC(3),DS_JPERP,DS,NORM
      REAL*8 DMIN2,DSDIST,DDMIN
C ----------------------------------------------------------------------
C  Unpack the track parameters
C
      IR  = TPAR(1)
      TANL= TPAR(2)
      PHI0 = TPAR(3)
      D0  = TPAR(4)
      Z0  = TPAR(5)
C
C  Calculate a few things
C
      COSL = 1./SQRT(1.+TANL**2)
      SINL = TANL*COSL
      COSP0 = COS(PHI0)
      SINP0 = SIN(PHI0)
C
C  Take a linear approximation to the tracks (this is exact for neutrals
C
      T1(1) =  COSL*COSP0
      T1(2) =  COSL*SINP0
      T1(3) =  SINL
      T0(1) =  D0*SINP0
      T0(2) =  -D0*COSP0
      T0(3) =  Z0
C
C  Find the vector perpendicular to the jet but 'along' the track direct
C
      DOT = 0.0
      DO ICOR=1,3
        DOT = DOT + J1(ICOR)*T1(ICOR)
      END DO
      DO ICOR=1,3
        QVEC(ICOR) = T1(ICOR)-DOT*J1(ICOR)
      END DO
C
C  Solve for the arc length change
C
      DS_JPERP = 0.0
      DOT = 0.0
      DO ICOR=1,3
        DS_JPERP  = DS_JPERP  + QVEC(ICOR)*(IP(ICOR)-T0(ICOR))
        DOT = DOT + QVEC(ICOR)*T1(ICOR)
      END DO
      IF(DOT.EQ.0.0D0)DOT=1.0D-20
      DS = DS_JPERP/DOT
C
C  Solve for the distance along the J1 direction
C  of the closest point
C
      DJDIST = 0.0
      DO ICOR=1,3
        DJDIST = DJDIST + J1(ICOR)*(T0(ICOR)+T1(ICOR)*DS-IP(ICOR))
      END DO
C
C  Now, solve for the distance between the track and line at this point.
C  For this, we construct the vector perpendicular both to the track and
C  jet direction.
C
      JPERP(1) =  T1(2)*J1(3)-T1(3)*J1(2)
      JPERP(2) = -T1(1)*J1(3)+T1(3)*J1(1)
      JPERP(3) =  T1(1)*J1(2)-T1(2)*J1(1)
      NORM = 0.0
      DLDIST = 0.0
      DO ICOR=1,3
        DLDIST = DLDIST + JPERP(ICOR)*(T0(ICOR)-IP(ICOR))
        NORM = NORM + JPERP(ICOR)**2
      END DO
      NORM=MAX(SQRT(NORM),1.0D-20)
      DLDIST = DLDIST/NORM
      DO ICOR=1,3
        JPERP(ICOR)=JPERP(ICOR)/NORM
      END DO
C
C  Solve for the distance between the track and the IP
C
      DMIN2 = 0.0
      DSDIST = 0.0
      DO ICOR=1,3
        DMIN2 = DMIN2 + (IP(ICOR)-T0(ICOR))**2
        DSDIST  = DSDIST  + (IP(ICOR)-T0(ICOR))*T1(ICOR)
      END DO
      DMIN2 = DMIN2 - DSDIST**2
      DDMIN = SQRT(MAX(1D-20,DMIN2))
C
C  Calculate the unit vector in the impact parameter direction
C
      DO ICOR=1,3
        PERP(ICOR) = (T0(ICOR)+DSDIST*T1(ICOR)-IP(ICOR))/DDMIN
      END DO
C
C  Convert to single precision output variables
C
      JDIST = DJDIST
      LDIST = DLDIST
      SDIST = DSDIST
      DMIN = DDMIN
C
C  Sign DMIN according to which hemisphere the track-jet crossing
C  is in, relative to the IP.
C
      DMIN = SIGN(DMIN,JDIST)
C
C  Done
C
 1000 CONTINUE
      RETURN
      END
