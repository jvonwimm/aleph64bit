      SUBROUTINE TPDMOM(AMMG,AMVC,AMASS,AGAMA,DLKE,DLMMG,DLMVC)
C-----------------------------------------------------------------------
C!  Calculate the momentum (magnitude and direction) of a delta-ray from
C!  simple kinematics
C
C  Called from:  TSDEDX
C  Calls:        None
C
C  Inputs:   PASSED:     --AMMG,  the magnitude of the momentum of the
C                                 particle producing the delta-ray
C                        --AMVC,  the normalized momentum vector of the
C                                 incident particle
C                        --AMASS, the rest mass of the incident particle
C                        --AGAMA, the gamma of the incident particle
C                        --DLKE,  the kinetic energy of the delta
C
C  Outputs:  PASSED:     --DLMMG, the magnitude of the momentum of the
C                                 delta
C                        --DLMVC, the normalized momentum vector of the
C                                 delta
C  A. Caldwell, D. DeMille
C-----------------------------------------------------------------------
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER(CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C  Additional constants for TPCSIM
C  Units -- Mev,Joules,deg Kelvin,Coulombs
C
      REAL ELMASS,CROOT2,CKBOLT,CROOMT,ECHARG
      PARAMETER (ELMASS = 0.511)
      PARAMETER (CROOT2 = 1.41421356)
      PARAMETER (CKBOLT = 1.380662E-23)
      PARAMETER (CROOMT = 300.)
      PARAMETER (ECHARG = 1.602189E-19)
C
      DIMENSION AMVC(3),DLMVC(3)
C
C  Calculate the polar angle of the ejected delta as a function of the
C  outgoing energy and the mass and momentum of the incident particle.
C  Formula from Sauli, should be elementary kinematics.
C
C      DLEMX = 2.*(AMMG**2)/AMASS    WRONG!!
      DLEMX = 2.*AMMG**2*ELMASS/(AMASS**2)
C
C  There is a kinematic cutoff for the delta's kinetic energy which is
C  not taken account of elsewhere.  If this cutoff is exceeded, set the
C  delta's KE to the maximum value
C
      IF ( DLKE .GT. DLEMX ) DLKE = DLEMX
C
      DLTHT = ACOS( SQRT(DLKE/DLEMX) )
C
C  Now generate an azimuthal angle randomly, then find the
C  projections in the reference system where the track points along X.
C
      DLPHI = TWOPI*RNDM(Q)
C
      PZ = SIN(DLTHT)*COS(DLPHI)
      PY = SIN(DLTHT)*SIN(DLPHI)
      PX = COS(DLTHT)
C
C  Now using the angles of the momentum vector, rotate into detector
C  reference system and get the new projections on x,y,z.
C
      DLPHG = ATAN2(AMVC(2),AMVC(1))
      DLTHG = ASIN( SQRT( 1.-AMVC(3)*AMVC(3) ) )
C
      CPHG = COS(DLPHG)
      SPHG = SIN(DLPHG)
      CTHG = COS(DLTHG)
      STHG = SIN(DLTHG)
C
      DLMVC(1) = PX*CPHG*STHG - PY*SPHG - PZ*CTHG*CPHG
      DLMVC(2) = PX*STHG*SPHG + PY*CPHG - PZ*CTHG*SPHG
      DLMVC(3) = PX*CTHG + PZ*STHG
C
C  Get the magnitude of the delta's momentum
C
      DLMMG = SQRT(2*DLKE*ELMASS + DLKE*DLKE)
C
      RETURN
      END
