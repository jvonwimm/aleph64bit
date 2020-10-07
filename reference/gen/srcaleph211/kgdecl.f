      REAL FUNCTION KGDECL(P,AM,T0)
C-----------------------------------------------------------------------
C! Generates decay length in cm
CKEY KINE KINGAL DECAY /  INTERNAL
C      B.Bloch-Devaux        september 86
C
C - structure: REAL FUNCTION subprogram
C              User Entry Name: KBDECL
C              External References: RNDM(CERNLIB)
C              Comdeck referenced: ALCONS
C
C - Usage   : DECL = KGDECL (P,AM,T0)
C - Input   : P      = momentum of the particle
C             AM     = particle mass
C             T0     = particle life time
C - Output  : KGDECL = decay length in cm
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (CLITS = CLGHT * 1.E+9)
      EXTERNAL RNDM
C ------------------------------------------------------------
      Z=RNDM(0)
      IF (Z.EQ.0.) Z=1.
      KGDECL=-P*CLITS*T0*ALOG(Z)/AM
      RETURN
      END
