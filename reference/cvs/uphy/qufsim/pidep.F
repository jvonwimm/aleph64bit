      SUBROUTINE PIDEP(ENEP,XDEP)
C----------------------------------------------------------------
C! XDEP for pions
C----------------------------------------------------------------
      PARAMETER( DEP= .35, SIG= .05 )
C     PARAMETER( ALPHA= .9, XEN= .45, SIGMA= .9)
C     PARAMETER( ALPHA= .9, XEN= .6  , SIGMA= 3.5)
      PARAMETER( ALPHA= .9, XEN= .6  , SIGMA= 3. )
      PARAMETER( XMA= 2. )
      PARAMETER( NRANMX= 1000 )
C
      XDEP= 0.
      BETA= 1.-ALPHA
      ENEM= XEN*ENEP
C
10    CONTINUE
      GMAX= ALPHA*FLAND(0.)+BETA*FGAUS(0.)
      NRAN= 0
1     CONTINUE
      XW= RNDM(XW)*XMA*ENEP
      NRAN= NRAN+1
      IF(NRAN.GT.NRANMX) GOTO 2
      XLA= (XW-DEP)/SIG
      XGA= (XW-ENEM)/(SIGMA*SQRT(ENEM))
      GLA= 0.
      IF(XLA.GT.(-5.).AND.XLA.LT.(15.)) GLA= FLAND(XLA)
      GGA= 0.
      IF(XGA.GT.(-5.).AND.XGA.LT.(5.)) GGA= FGAUS(XGA)
      GX= GLA*ALPHA + GGA*BETA
      GR= RNDM(GR)
      IF(GR.GT.(GX/GMAX)) GOTO 1
C
      XDEP= XW
      IF(XDEP.LT.(0.)) XDEP= 0.
2     CONTINUE
      XFAC= 0.
      IF(ENEP.GT.(1.)) XFAC= 1.-EXP(-2.*(ENEP-1.))
      XDEP= XFAC*XDEP
C
      RETURN
      END
