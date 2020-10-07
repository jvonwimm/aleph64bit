      SUBROUTINE LANDEP(XDEP)
C----------------------------------------------------------------
C! XDEP following pseudoLandau distribution
C----------------------------------------------------------------
      PARAMETER( DEP= .35, SIG= .05 )
      PARAMETER( XMA= 20., XMI= -5. )
      PARAMETER( NRANMX= 1000 )
C
      XDEP= 0.
10    CONTINUE
      GMAX= FLAND(0.)
      NRAN= 0
1     CONTINUE
      XLA= RNDM(XLA)*XMA + XMI
      NRAN= NRAN+1
      IF(NRAN.GT.NRANMX) GOTO 2
      GX= FLAND(XLA)
      GR= RNDM(GR)
      IF(GR.GT.(GX/GMAX)) GOTO 1
C
      XDEP= XLA*SIG + DEP
      IF(XDEP.LT.(0.)) XDEP= 0.
2     CONTINUE
C
      RETURN
      END
