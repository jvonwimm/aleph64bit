      REAL FUNCTION FLAND(XLA)
C------------------------------------------------------------------
C! Pseudo Landau distribution
C------------------------------------------------------------------
      PARAMETER( QQPI= 3.141592 )
C
      WLA= EXP(-XLA) + XLA
      WLA= EXP( -WLA/2. )/SQRT(2.*QQPI)
      FLAND= WLA
C
      RETURN
      END
