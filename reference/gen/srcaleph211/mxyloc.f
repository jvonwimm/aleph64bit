         SUBROUTINE MXYLOC(ISUBC,LAYER,NSTRX,LCLUX,NSTRY,LCLUY,XX,YY)
C----------------------------------------------------------------
C!  converts x,y strip numbers to local coordinates
C!  NSTRPX,NSTRPY=strip #   LCLUX,LCLUY=cluster width
C!  XX,YY = local coordinates in muon chamber ref system
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
      SAVE
      PARAMETER (NSUBCO=3,NLAYRS=2,NSLBAR=24,NSLMDA=38,NSLEND=16)
      PARAMETER (NSLBSM=16)
      COMMON/MRDDAT/XPITCH,YPITCH,XXOFFS(NSUBCO,NLAYRS), YYOFFS(NSUBCO,
     +NLAYRS),ZZOFFS(NSUBCO,NLAYRS),WDEIMU,WD16MU(NSUBCO),OFTBMU
C
      FND8=NSTRX/16
      FNS8=MOD(NSTRX,16)/8
      FNSTR=MOD(NSTRX,8)
      XX=OFTBMU+FND8*WD16MU(ISUBC)+FNS8*WDEIMU+(FNSTR+0.5)*XPITCH
      IF (LCLUX.GT.0) THEN
         NLAST=NSTRX+LCLUX
         FND8=NLAST/16
         FNS8=MOD(NLAST,16)/8
         FNSTR=MOD(NLAST,8)
         XX=0.5*(XX+OFTBMU+FND8*WD16MU(ISUBC)+FNS8*WDEIMU+
     +   (FNSTR+0.5)*XPITCH)
      ENDIF
C
      FNY=FLOAT(NSTRY)+0.5*FLOAT(LCLUY)
      XX=XX+XXOFFS(ISUBC,LAYER)
      YY=FNY*YPITCH+YYOFFS(ISUBC,LAYER)
C
      RETURN
      END
