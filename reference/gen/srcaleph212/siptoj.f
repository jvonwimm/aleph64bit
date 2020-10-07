      INTEGER FUNCTION SIPTOJ(PHI,IMD,IST)
C.---------------------------------------------------------------------
CKEY SCALDES ENCODE ADDRESS / USER
C     B.BLOCH       February 92
C! Find PHI bin number frm PHI position in module
C   Input :
C          PHI  PHI (radian)
C          IMD  Module number ( 1 or 2)
C          IST  Stack number  ( 1 to 12)
C   Output:
C          SIPTOJ integer Phi bin ( 1 to 32)
C                 0 means outside detector or error
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
      SIPTOJ = 0
C GET PHI bin
      ITP = MOD(IST-1,3)+1
      PHIT= PHI-PHSHFT(ITP,IMD)
      PHIT = MOD(PHIT,TWOPI)
      IF (PHIT.LT.0.) PHIT= PHIT+TWOPI
      IF (PHIT.GT.TWOPI) PHIT= PHIT-TWOPI
      IPH = INT(PHIT/PHISTP)+1
      IF ( IPH.LE.0 .OR. IPH.GT.NPBNSI) GO TO 999
      SIPTOJ = IPH
 999  RETURN
      END
