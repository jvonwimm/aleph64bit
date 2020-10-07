*DK eprimi
      FUNCTION EPRIMI(X)
C.------------------------------------------------------------------
C! returns the wire signal value integrated during X ns
C  Author M.Bernard/D.Pallin
CKEY ECAL / INTERNAL
C - Input  : X      / R   = time in ns
C - Output : EPRIMI / R   = wire signal value integrated during X ns
C  Called by EDWBRU
C ----------------------------------------------------------
#ifndef DOC
      PARAMETER(B=2.6943E-3)
C ---------------------------------------------------------
      EPRIMI=0.
      IF(X.LT.0.)RETURN
      Y=B*X
      EPRIMI=1.-(1.+Y+Y**2/2.+Y**3/6.+Y**4/24.)*EXP(-Y)
      RETURN
      END
#endif
