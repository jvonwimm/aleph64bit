      SUBROUTINE UBPACK(NAME,NUM,KTAG,LTAG,JTAG,IER)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Remove marked rows from a Tabular Bank
C - Input :
C -   NAME  = Bank name
C -   NUM   = Bank number
C -   KTAG  = Column where check is to be done. Let IVAL be the content
C -           This column should contain integers
C -   LTAG  = Value to which IVAL is to be compared
C -   JTAG  = Indicates which type of comparison is to be made
C -           -1 means : Kill row if IVAL < LTAG
C -           +1 means : Kill row if IVAL > LTAG
C -            0 means : Kill row if IVAL = LTAG
C - Output :
C -   IER = 1 if Unsuccessful
C  ----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER*4 NAME
#include "bmacro.h"
C ----------------------------------------------------------------------
C
      KINDX = NLINK(NAME,NUM)
      IF(KINDX.LE.0) GO TO 999
      NCOLS = LCOLS(KINDX)
      IF(KTAG.LE.0.OR.KTAG.GT.NCOLS) GO TO 999
      NROWS = LROWS(KINDX)
      JNEXT = KNEXT(KINDX)
C
      NKILL = 0
      JINDX = JNEXT
      DO 1 IROW = NROWS,1,-1
        JINDX = JINDX - NCOLS
        IVAL = IW(JINDX+KTAG)
        IF(JTAG.LT.0) THEN
          IF(IVAL.GE.LTAG) GO TO 1
        ELSE IF(JTAG.GT.0) THEN
          IF(IVAL.LE.LTAG) GO TO 1
        ELSE
          IF(IVAL.NE.LTAG) GO TO 1
        ENDIF
        I1 = JINDX + NCOLS + 1
        I2 = JNEXT
        DO 2 I = I1,I2
          IW(I-NCOLS) = IW(I)
    2   CONTINUE
        NKILL = NKILL + 1
    1 CONTINUE
C
      IW(KINDX+2) = NROWS - NKILL
      LTOTX = NCOLS*(NROWS-NKILL)+LMHLEN
      KINDX = NBANK(NAME,NUM,LTOTX)
      IF(KINDX.LE.0) GO TO 999
      GO TO 100
C
  999 CONTINUE
      IER = 1
C
  100 CONTINUE
      END
#endif
