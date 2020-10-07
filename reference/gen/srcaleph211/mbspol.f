      SUBROUTINE MBSPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon special barrel : convert local XX,YY to polar coordinates
C!
C!   G.Capon/R.Xu                 871029
C!
C------------------------------------------------------------------
      SAVE
      PARAMETER (NSUBCO=3,NLAYRS=2,NSLBAR=24,NSLMDA=38,NSLEND=16)
      PARAMETER (NSLBSM=16)
      COMMON/MBG1DA/WIDTHB(NSLBAR),ZWIDTB(NSLBAR),CENDVB(NSLBAR), RINTEB
     +(NSLBAR),INPHIB(NSLBAR),THICKB
      COMMON/MSG1DA/WTPLBN(NSLBSM),XTPYBT(NSLBSM)
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
C     Transform X coordinate of slot # 79,80 (posision # 1,2)
C     to X of MUB1 4 local reference system.
C     Transform Y coordinate of slot # 81,82,83 (posision # 3,4,5)
C     to Y of MUB1 9 local reference system.
C     Transform Y coordinate of slot # 84,85,86 (posision # 6,7,8)
C     to Y of MUB1 11 local reference system.
C     Transform X coordinate of slot # 87,88 (posision # 9,10)
C     to X of MUB2 4 local reference system.
C     Transform Y coordinate of slot # 89,90,91 (posision # 11,12,13)
C     to Y of MUB2 9 local reference system.
C     Transform X coordinate of slot # 92,93,94 (posision # 14,15,16)
C     to Y of MUB2 11 local reference system.
C
      IF(NSLOT.LE.80) THEN
          NSLT=4
          GOTO 100
      ELSE IF(NSLOT.LE.83) THEN
          NSLT=9
          GOTO 200
      ELSE IF(NSLOT.LE.86) THEN
          NSLT=11
          GOTO 200
      ELSE IF(NSLOT.LE.88) THEN
          NSLT=16
          GOTO 100
      ELSE IF(NSLOT.LE.91) THEN
          NSLT=21
          GOTO 200
      ELSE
      NSLT=23
      ENDIF
 200  XCS=WIDTHB(NSLT)/2.
      DXCC=XX-XCS+CENDVB(NSLT)
      DHEMS=YY-WTPLBN(NSLOT-78)/2.+XTPYBT(NSLOT-78)
      GOTO 300
 100  XCS=WTPLBN(NSLOT-78)/2.
      DXCC=XX-XCS+CENDVB(NSLT)+XTPYBT(NSLOT-78)
      DHEMS=YY-ZWIDTB(NSLT)/2.
 300  RINT=RINTEB(NSLT)+0.5*THICKB+ZZOFFS(ISUBC,LAYER)
      DFI=ATAN2(DXCC,RINT)
      PHI=PI/6.*FLOAT(INPHIB(NSLT)-1)
      PHI=PHI+DFI
      RR=RINT/COS(DFI)
      RHO = SQRT(RR*RR + DHEMS*DHEMS)
      THETA=ACOS(DHEMS/RHO)
C
      END
