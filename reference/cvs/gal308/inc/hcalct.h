C
      REAL CALTUB, CALTUE, CALIMB(24), CALIME(12), CALBA(24),
     &     CALEC(12), CALMCB(24), CALMCE(12), CT0MCB, CT0MCE,
     &     CT0DAB, CT0DAE, CALTBA, CALTEC
C
      COMMON /HCALCT/ CALTUB, CALTUE, CALIMB, CALIME, CALBA,
     &     CALEC, CALMCB, CALMCE, CT0MCB, CT0MCE, CT0DAB,
     &     CT0DAE, CALTBA, CALTEC
C
#if DOC
C! HCAL calibration constants
    CALTUB, CALTUE = online calibration constant
    CALIMB(24), CALIME(12) = calibration constants on the FIC
    CALBA(24), CALEC(12) = intermodule calibration (HIMC)
    CALMCB(24), CALMCE(12) = Monte Carlo intermodule calibration
    CT0MCB, CT0MCE, CT0DAB, CT0DAE = global calibration constants (HT0C)
    CALTBA, CALTEC = time dependent global calibration constants (HSTD)
#endif
