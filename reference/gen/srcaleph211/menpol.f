      SUBROUTINE MENPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon endcaps : convert local XX,YY to polar coordinates
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
      SAVE
      PARAMETER (NSUBCO=3,NLAYRS=2,NSLBAR=24,NSLMDA=38,NSLEND=16)
      PARAMETER (NSLBSM=16)
      COMMON/MCG1DA/XCENTR(NSLEND),YCENTR(NSLEND),XWIDTH(NSLEND), YWIDTH
     +(NSLEND),ZENCAP(NSLEND),THICKC
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
C
      NSL=NSLOT-NSLBAR-NSLMDA
      NSL1=1+MOD(NSL-1,4)
      IF(NSL.LE.8)THEN
C
C?      A side
C
         SIGNZ=1.
         IF(NSL1.EQ.1.OR.NSL1.EQ.4)THEN
            XEC=YY
         ELSE
            XEC=-YY
         END IF
C
C?      B side
C
      ELSE
         SIGNZ=-1.
         IF(NSL1.LE.2)THEN
            XEC=YY
         ELSE
            XEC=-YY
         END IF
      ENDIF
C
C?     position of modules in individual slots taken into account
C?     XEC and YEC are now hit coordinates in aleph-system
C
      IF(NSL1.EQ.1)YEC=SIGNZ*(YWIDTH(NSL)-XX)
      IF(NSL1.EQ.2)YEC=XX
      IF(NSL1.EQ.3)YEC=SIGNZ*(-YWIDTH(NSL)+XX)
      IF(NSL1.EQ.4)YEC=-XX
      DX=ABS(XCENTR(NSL))-XWIDTH(NSL)/2.
      DY=ABS(YCENTR(NSL))-YWIDTH(NSL)/2.
      XEC=XEC+DX*(XCENTR(NSL)/ABS(XCENTR(NSL)))*SIGNZ
      YEC=YEC+DY*(YCENTR(NSL)/ABS(YCENTR(NSL)))
      PHI=ATAN2(YEC,XEC)
      IF(PHI.LT.0.)PHI=PHI+TWOPI
      RR=SQRT(XEC*XEC+YEC*YEC)
      ZENC=SIGNZ*(ZENCAP(NSL)+0.5*THICKC+ZZOFFS(ISUBC,LAYER))
      RHO = SQRT(RR*RR + ZENC*ZENC)
      THETA=ACOS(ZENC/RHO)
C
      RETURN
      END
