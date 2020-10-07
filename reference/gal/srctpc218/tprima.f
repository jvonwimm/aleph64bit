      FUNCTION TPRIMA(POI,ZB)
C-----------------------------------------------------------------------
C!  Return the KE of a primary electron; KE = 0 means not a delta
C
C  Called from:  TSDEDX
C  Calls:        RNDM
C
C  Inputs:                --POI, average number of collisions per cm
C                         --ZB,  charge**2/beta**2 of primary particle
C            /TPCONS/     --CRUTH, the constant on the 1/E (Rutherford)
C                                  part of the primary energy
C                                  distribution
C                         --DEMIN, the minimum energy for a delta-ray
C                         --DEMAX, the maximum energy for a delta-ray
C
C  Outputs:  TPRIMA, the primary kinetic energy ( = 0. if not a delta)
C
C  A. Caldwell
C
C  Modified:  Dave Casper, clean up energy generation, account for prima
C             momentum.
C-----------------------------------------------------------------------
C
C  TPCONS contains physical constants for TPC simulation
C
      COMMON /DELTA/ CDELTA,DEMIN,DEMAX,DELCLU,RADFAC,CYLFAC
      PARAMETER (MXGAMV = 8)
      COMMON /TGAMM/ GAMVAL(MXGAMV),GAMLOG(MXGAMV),POIFAC(MXGAMV),
     &               POIMAX,POIMIN,CFERMI,CA,CB,POIRAT,POICON
      PARAMETER (MXBINC = 20)
      COMMON /CLUST/ EBINC(MXBINC),CONCLU,WRKFUN,MXCL,CKNMIN,CFANO,CRUTH
     &              ,POWERC
      COMMON /AVALA/ THETA,ETHETA
      COMMON /TPTIME/ NTMXSH,NTMXNO,NTMXAN,NTMXDI,NTSCAN,NTBNAS,NTBAPD
      COMMON /TPELEC/ TPRPAR,TPRSER,TPCFET,TCFEED,TMVPEL,TSIGMX,NTPBIT
C
      DATA ICALLS/0/
C
C  If this is the first call, set up the delta-ray probability limits
C  from the energy limits
C
      PRMIN = (ZB * CRUTH)*(1./DEMIN - 1./DEMAX)/POI
C
 1    X = RNDM(P)
C
C  Check for delta-ray
C
      IF ( X .GE. PRMIN ) THEN
C
C  Not called a delta-ray, set default energy value
C
          TPRIMA = 0.
C
      ELSE
C
C  We are over the limit for the minimum energy of a delta
C  Toss a new random number to see what the energy of the delta is,
C  and whether its energy is over DEMAX.  Note that this is done in
C  a different probability scale to preserve precision.
C
          X = RNDM(P)
          TPRIMA = DEMIN*DEMAX/(DEMAX*(1.-X) + DEMIN*X)
      ENDIF
C
      RETURN
      END
