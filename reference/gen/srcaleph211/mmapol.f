      SUBROUTINE MMAPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon middle angle : convert local XX,YY to polar coordinates
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
      SAVE
      PARAMETER (NSUBCO=3,NLAYRS=2,NSLBAR=24,NSLMDA=38,NSLEND=16)
      PARAMETER (NSLBSM=16)
      COMMON/MMG1DA/ZWIDTM(NSLMDA),WIDTHM(NSLMDA),CENDVM(NSLMDA), RINTEM
     +(NSLMDA),INPHIM(NSLMDA),ZMODUL(NSLMDA),THICKM
      COMMON/MRDDAT/XPITCH,YPITCH,XXOFFS(NSUBCO,NLAYRS), YYOFFS(NSUBCO,
     +NLAYRS),ZZOFFS(NSUBCO,NLAYRS),WDEIMU,WD16MU(NSUBCO),OFTBMU
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
C
C?    values for shift of modules on B side taken from A side
C?    rotating by pi around y global
C
      NSL=NSLOT-NSLBAR
      PHI=PI/6.*FLOAT(INPHIM(NSL)-1)
      YCS=WIDTHM(NSL)/2.
      SIGNZ=1.
      IF (NSL.GE.20) SIGNZ=-1.
      DYCS = SIGNZ*(YCS - YY)+ CENDVM(NSL)
      ZL = SIGNZ*(ZMODUL(NSL) + XX)
      RINT=RINTEM(NSL)+0.5*THICKM+ZZOFFS(ISUBC,LAYER)
      DFI=ATAN2(DYCS,RINT)
      PHI=PHI+DFI
      RR=RINT/COS(DFI)
      RHO = SQRT(RR*RR + ZL*ZL)
      THETA=ACOS(ZL/RHO)
C
      RETURN
      END
