      REAL FUNCTION FGAUS(XGA)
C------------------------------------------------------------------
C! Gaussian distribution
C------------------------------------------------------------------
      PARAMETER( QQPI= 3.141592 )
C
      WGA= EXP(-XGA*XGA/2.)
      WGA= WGA/SQRT(2.*QQPI)
      FGAUS= WGA
C
      RETURN
      END
