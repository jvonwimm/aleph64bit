      SUBROUTINE UTSWCO(IL,IWIRE,ZSW,RSW,PHSW)
C-----------------------------------------------------------------------
C! Get sense wire coord (+sag) for fitting
C!
CKEY COMPUTE FIT /INTERNAL
C!
C!    Author          :- J. Sedgbeer
C!    Last Modified   :- J. Sedgbeer   89/09/01
C!    Changed to use ZSW : W.B. Atwood 89/10/4
C!    Input:
C!      IL     /I  : layer number
C!      IWIRE  /I  : wire number                      [1,960]
C!      ZSW    /R  : Z from where ever
C!                   /ITWICC/  ITC sense wire geom.
C!                   /ISWPHI/  ITC sense wire coords.
C!                   /IRESOL/  for sigma z
C!                   ALCONS
C!
C!    Output:
C!      RSW    /R  : Radius of sense wire (cm.)
C!      PHSW   /R  : Phi of sense wire    (radians.)
C!
C!    calls     : none
C!
C!    Libraries required : none
C!
C? Get hit wire coord from wire number
C? calculate wire sag at this Z
C? Correct wire coord. due to sag.
C-----------------------------------------------------------------------
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
      PARAMETER (JWIRIT=8,MWIRIT=960)
      COMMON/ITWICC/RWIRIT(JWIRIT),NWIRIT(JWIRIT),IWIRIT(JWIRIT),
     +              PHWRIT(JWIRIT),CELWIT(JWIRIT),WZMXIT,SGMXIT
      INTEGER JSPAIZ,JLAYIZ,IBN0IZ,ITLOIZ,ITHIIZ,IBNDIZ
      REAL BWIDIZ,EXP8IZ,SBNDIZ
      LOGICAL FZCOIZ
      PARAMETER (JSPAIZ=3,JLAYIZ=8)
      COMMON/IZFECC/BWIDIZ,IBN0IZ,EXP8IZ,SBNDIZ(JSPAIZ),IBNDIZ,
     +              ITLOIZ(JLAYIZ),ITHIIZ(JLAYIZ),FZCOIZ
      PARAMETER (JSWPIS=960)
      COMMON/ISWPHI/PHSWIS(JSWPIS)
      INTEGER JRESIR
      REAL SMAXIR,SIGRIR,SIGZIR
      PARAMETER (JRESIR=8)
      COMMON/IRESOL/SMAXIR(JRESIR),SIGRIR(JRESIR),SIGZIR(JRESIR)
C-----------------------------------------------------------------------
C
C Get sense wire coord. unsagged in ITC frame.
C
      RSW   = RWIRIT(IL)
      PHSW  = PHSWIS(IWIRE)
C
C Calculate the sag at ZSW (assume a parabola)
C
      SAG = SGMXIT*(1.0 - (ZSW/WZMXIT)**2)
C
C Calculate change in RSW and PHSW due to sag
C
      DELR = SAG*SIN(PHSW)
      RSW  = RSW - DELR
      DPHI = SAG*COS(PHSW)/RSW
      PHSW   = PHSW - DPHI
C
C Angles to be in range 0 - 2pi
C
      IF(PHSW.LT.0.0)   PHSW = PHSW + TWOPI
      IF(PHSW.GE.TWOPI) PHSW = PHSW - TWOPI
C
  999 CONTINUE
      END
