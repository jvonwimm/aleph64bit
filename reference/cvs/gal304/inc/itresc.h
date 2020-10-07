*CD itresc
      COMMON /ITRESC/RESITP(5,8),RESITN(5,8)
      REAL RESITP,RESITN
C
#if defined(DOC)
      Parameters of ITC R-phi resolution.
      RESITP = Positive polynomial resolution coefficients.
      RESITN = Negative polynomial resolution coefficients.
#endif
