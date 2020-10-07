      SUBROUTINE ITSWCO(IL,IWIRE,IZDIG,RSW,PHSW,ZSW,SIGZ)
C-----------------------------------------------------------------------
C! Get sense wire coord (+sag).
C!
CKEY IPREDATA ITC /INTERNAL
C!    Author     :- J. Sedgbeer
C!    Modified   :- J. Sedgbeer   90/01/04 Check use of Z and tidy
C!                                                    Z resolution.
C!    Modified   :- J. Sedgbeer   92/02/07 Use Z non-linearity from
C!                                              /IZNLCC/
C!
C!    Input:
C!      IL     /I  : layer number
C!      IWIRE  /I  : wire number                      [1,960]
C!      IZDIG  /I  : Z TDC value
C!      need commons /IZFECC/  Z front-end parameters
C!                   /IZNLCC/  Z non-linearity params.
C!                   /ITWICC/  ITC sense wire geom.
C!                   /ISWPHI/  ITC sense wire coords.
C!                   /IZRSCC/  for sigma z
C!                   ALCONS
C!
C!    Output:
C!      RSW    /R  : Radius of sense wire (cm.)
C!      PHSW   /R  : Phi of sense wire    (radians.)
C!      ZSW    /R  : Z position           (cm.)
C!      SIGZ   /R  : Sigma Z
C!                   > 0   if all O.K.
C!                  set to large value if IZDIG out of range (ZSW = 0.0)
C!
C!    calls     : none
C!
C!    Libraries required : none
C!
C? Get hit wire coord from wire number
C? calculate z coord from Z scalar value
C? calculate wire sag at this Z
C? Correct wire coord. due to sag.
C-----------------------------------------------------------------------
      SAVE
C I/O commons etc.
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
      INTEGER JOFSLN,JCZNLN
      REAL OFSLIZ,CZNLIZ
      PARAMETER (JOFSLN=8,JCZNLN=3)
      COMMON/IZNLCC/OFSLIZ(JOFSLN),CZNLIZ(JCZNLN)
      PARAMETER (JSWPIS=960)
      COMMON/ISWPHI/PHSWIS(JSWPIS)
      INTEGER JCOFIZ,JLYRIZ
      REAL ZRTRIZ,ZRESIZ
      PARAMETER (JCOFIZ=2,JLYRIZ=8)
      COMMON/IZRSCC/ZRTRIZ(JLYRIZ),ZRESIZ(JCOFIZ,JLYRIZ)
C-----------------------------------------------------------------------
C Get sense wire coord. unsagged in ITC frame.
C Set default Z (zero) and error (about 10 times length of chamber).
C
      RSW   = RWIRIT(IL)
      PHSW  = PHSWIS(IWIRE)
      SIGZ  = 2000.
      ZSW   = 0.0
C
C Check Z TDC data O.K.  and check IZDIG value
C
      IF(.NOT.FZCOIZ) GOTO 900
      IF(IZDIG.LE.ITLOIZ(IL)) GOTO 900
C
C Calculate Z coord. and Z error. Z error set to value for track
C finding and fitting (true z resol. only used in special applications).
C If S-bend correction flag, IBNDIZ, is zero then do s-bend correction.
C
      SIGZ0 = ZRTRIZ(IL)
      EFACT = (RSW/RWIRIT(JWIRIT))/EXP8IZ
      TDIFF = (FLOAT(IBN0IZ-IZDIG))*BWIDIZ*EFACT
      Z0    = 0.5*CLGHT*TDIFF
      AA    = TWOPI/CZNLIZ(3)
      IF(IBNDIZ.EQ.0) THEN
        Z0 = Z0 + OFSLIZ(IL)
        ZSW = CZNLIZ(1)*Z0 + CZNLIZ(2)*SIN(AA*Z0)
      ELSE
        ZSW = Z0
      ENDIF
      SIGZ  = SIGZ0
CCC      SIGZ  = SIGZ0*(1. + AA*CZNLIZ(2)*COS(AA*Z0))
C
C Check ZSW in range.  If outside range increase error.
C
      IF(ABS(ZSW).GT.WZMXIT) THEN
         ZSW = WZMXIT*(ABS(ZSW)/ZSW)
         SIGZ  = SIGZ + (ABS(ZSW)-WZMXIT)*3.0
      ENDIF
C
C Calculate the sag at ZSW (assume a parabola)
C
  900 CONTINUE
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
