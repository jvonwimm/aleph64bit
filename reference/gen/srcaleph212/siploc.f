      REAL FUNCTION SIPLOC(PHI,Z)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE POSITION/ USER
C     B.BLOCH       February 92
C! Find local position inside Phi bin
C   Input :
C          PHI  Azimuth (rad)
C          Z    Z position (cm)
C   Output:
C          SIPLOC Position   within Phi bin (0.,1.)
C                -1.means outside detector or error
C   Called by USER program
C.---------------------------------------------------------------------
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
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
      PARAMETER ( EPS  = 0.001 )
C-------------------------------------------------------
      SIPLOC = -1.
C GET module
      IMD = 1
      IF (Z.LT.0.) IMD = 2
C GET STACK   bin
      IST = INT((ABS(Z)+EPS-Z0SNSI(IMD)-DPOSSI(3,IMD))/ZWIDSI)+1
      IF ( IST.LE.0 .OR. IST.GT.NZBNSI) GO TO 999
C GET PHI     bin
      ITP = MOD(IST-1,3)+1
      PHIT= PHI-PHSHFT(ITP,IMD)
      PHIT = MOD(PHIT,TWOPI)
      IF (PHIT.LT.0.) PHIT= PHIT+TWOPI
      IF (PHIT.GT.TWOPI) PHIT= PHIT-TWOPI
      SIPLOC =(MOD(PHIT,PHISTP))/PHISTP
 999  RETURN
      END
