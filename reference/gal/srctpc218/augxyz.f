      SUBROUTINE AUGXYZ(X,VECT,AMOM,QCHRG,PLEN,CC,XX)
C--------------------------------------------------------------------
C  Subroutine to the determine the space coordinates of a point given a
C  helical path length, the origin of the helical path length, and the
C  direction cosines at the origin of the path length.
C  Units -- Mev,nanoseconds,centimeters,KGauss
C
C  Calls:  None
C
C  Input:   PASSED:  --X,     coords at beginning of helical path
C                    --VECT,  tangential vector at beginning of helical
C                             path
C                    --AMOM,  abs value of momentum of particle
C                    --QCHRG, charge of particle
C                    --PLEN,  path length
C
C  Output:  PASSED:  --XX,    coords after path length PLEN
C                    --CC,    tangential vector after path length PLEN
C  A. Caldwell
C
C--------------------------------------------------------------------
C
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT,GRANNO(1000)
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
C
C
      DIMENSION X(3),XX(3),VECT(3),CC(3)
      DOUBLE PRECISION ARG1,ARG2,ARG3,SINTH,RAD,PHI0,SPHI0,CPHI0,DPHI
C
C  Now parametrize according to arclength t, 0 <= t <= PLEN
C
C  X(t) = X0 + R*COS(PHI0-t*SIN(THETA)/R)
C  Y(t) = Y0 + R*SIN(PHI0-t*SIN(THETA)/R)
C  Z(t) = Z0 + t*COS(THETA)
C
C  Where X0,Y0 are the coordinates of the center of the circle in r-phi
C  and Z0 is the starting z.  PHI0 is the starting phi for t=0.
C
C  Trouble when R gets very large.  Use series expansion
C
C          R*COS(X)      = R*( 1 - (X**2)/2 + (X**4)/24 ...)
C          R*SIN(X)      = R*( X - (X**3)/6 ...)
C
C   so that if R*(X**3)/6 < eps,  then we have accuracy eps
C   here, X = t*SIN(THETA)/R,  so
C                R*(X**3)/6 = (T**3) * ( SIN(THETA)**3 ) / ( 6*(R**2) )
C   so if the condition is satified, we take the first two terms for COS
C   and the first term for SIN.
C   Note that the radius is positive or negative depending on the charge
C
C  If no field, take linear step
      IF (ABS(CFIELD).LE.0.0001) THEN
         XX(1) = X(1) + PLEN*VECT(1)
         XX(2) = X(2) + PLEN*VECT(2)
         XX(3) = X(3) + PLEN*VECT(3)
         CC(1) = VECT(1)
         CC(2) = VECT(2)
         CC(3) = VECT(3)
         GO TO 999
      ENDIF
      ARG1 = 1. - VECT(3)*VECT(3)
      SINTH = SQRT(ARG1)
      SS = PLEN * SINTH
C
      RAD = AMOM*SINTH/(QCHRG*BCFMEV)
C
C  PHI0 is the angle of the momentum vector with the x axis
C
      ARG2 = VECT(1)
      ARG3 = VECT(2)
C
      PHI0 = ATAN2(ARG3,ARG2)
      SPHI0 = DSIN(PHI0)
      CPHI0 = DCOS(PHI0)
C
C  Check for approximation
C
      IF ( (ABS(SS)**3)/(6.*RAD*RAD) .LE. 1.E-5 ) THEN
C
C  Approximate as described above
C
           XX(1) = X(1) - SS*SS*SPHI0/(2.*RAD) + SS*CPHI0
           XX(2) = X(2) - SS*SS*CPHI0/(2.*RAD) + SS*SPHI0
           XX(3) = X(3) + PLEN*VECT(3)
C
      ELSE
C
C  No approximation
C
         X0 = X(1) + RAD*SPHI0
         Y0 = X(2) - RAD*CPHI0
C
         DPHI = PLEN*SINTH/RAD
         XX(1) = X0 - RAD*SIN(PHI0-DPHI)
         XX(2) = Y0 + RAD*COS(PHI0-DPHI)
         XX(3) = X(3) + PLEN*VECT(3)
C
      ENDIF
C
C  Get the direction cosines
C
      SDPHI = SIN( DPHI )
      CDPHI = COS( DPHI )
C
      CC(1) = VECT(1)*CDPHI + VECT(2)*SDPHI
      CC(2) = VECT(2)*CDPHI - VECT(1)*SDPHI
      CC(3) = VECT(3)
C
  999 RETURN
      END
