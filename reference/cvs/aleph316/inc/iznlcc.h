      INTEGER JOFSLN,JCZNLN
      REAL OFSLIZ,CZNLIZ
      PARAMETER (JOFSLN=8,JCZNLN=3)
      COMMON/IZNLCC/OFSLIZ(JOFSLN),CZNLIZ(JCZNLN)
#if defined(DOC)
C ITC Z Non Linearity parameters from DB bank IZNL (from Jan 1992 -
C       before this the constants were from IZFE)
C
C OFSLIZ(i) =  Layer offset  (cm)
C CZNLIZ(i) =  Coeffs. of non-linearity parametrisation
C              i=1 linear term
C              i=2 Amplitude (of sine) in cm.
C              i=3 Period    (of sine) in cm.
#endif
