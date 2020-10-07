      REAL FUNCTION VDGAUS(MEAN, SIGMA)
C ----------------------------------------------------------------------
C!  Returns the Quantity MEAN smeared by SIGMA
C!
C -  Author        Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C
C  Input :   MEAN    - mean
C            SIGMA   - sigma (Gaussian)
C
C  Output :  VDGAUS  - smeared random variable
C
C -----------------------------------------------------------------------
#ifndef DOC
      REAL MEAN, SIGMA
      REAL SVRN, RG
      DATA SVRN/-1000001.0/
C--  call rannor and save the extra random # we get there free.
      IF (SVRN.LE.-1000000.0) THEN
        CALL RANNOR(RG,SVRN)
      ELSE
        RG = SVRN
        SVRN = -1000001.0
      ENDIF
      VDGAUS = MEAN + SIGMA * RG
      RETURN
      END
#endif
