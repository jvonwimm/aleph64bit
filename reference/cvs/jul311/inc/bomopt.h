C!  BOM optics constants
      INTEGER NUMCOR
      PARAMETER (NUMCOR = 4)
      REAL RIBOPT, RBIOPT, PIBOPT, RCBOPT, RCIOPT, AMCIOP, ZMOPTI
      COMMON /BOMOPT/ RIBOPT(4,4,2), RBIOPT(4,4,2), PIBOPT(4,4,2),
     &                RCBOPT(4,4,2,NUMCOR), RCIOPT(4,4,2,NUMCOR),
     &                AMCIOP(4,4,2,NUMCOR), ZMOPTI(4,4,2)
#if defined(DOC)
C
C   NUMCOR  Number of corrector magnets per side of the IP
C   RIBOPT  Matrix giving transformation between IP and BOM
C   RBIOPT  Matrix giving transformation between BOM and IP
C   PIBOPT  Inverse of matrix RIBOPT
C   RCBOPT  Matrix giving transformation between corrector and BOM
C   RCIOPT  Matrix giving transformation between corrector and IP
C   AMCIOP  Matrix used in optics
C   ZMOPTI  Matrix used in optics
C
#endif
