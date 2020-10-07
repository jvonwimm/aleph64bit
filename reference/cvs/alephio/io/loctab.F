      FUNCTION LOCTAB(ITABL,ISTEP,LENG,INDX,IVAL)
C------------------------------------------------------------------
C.    O.CALLOT   7-MAR-86
C! Locate a value in a table
C   This function is analog to LOCATI ( E106 in Kernlib ) but
C   work on two dimension array: we look for the INDXth element in
C   LENG row of ISTEP word each for position of IVAL. The
C   function value is the row number, with same conventions
C   as for LOCATI : j : ITABL(INDX,j) = IVAL
C                   0 :                    IVAL < ITABL(INDX,1)
C                  -j : ITABL(INDX,j)    < IVAL < ITABL(INDX,j+1)
C               -LENG : ITABL(INDX,LENG) < IVAL
C  -----------------------------------------------------
#ifndef DOC
      DIMENSION ITABL(ISTEP,LENG)
      PARAMETER (MINROW=20)
C
C** if small number of row, simple DO loop
C
      IF(LENG.GT.MINROW)  GO TO 200
      DO 10 I=1,LENG
        IF(IVAL.GT.ITABL(INDX,I))  GO TO 10
        IF(IVAL.EQ.ITABL(INDX,I)) THEN
          LOCTAB = I
          RETURN
        ELSE
          LOCTAB = 1-I
          RETURN
        ENDIF
   10 CONTINUE
      LOCTAB = -LENG
      RETURN
C
C** binary search in ITABL
C
  200 IND = 0
      ISTP = LENG
  210 IST2 = ( ISTP + 1 ) / 2
      IF(IST2.EQ.0) GO TO 250
      IF(IVAL.LT.ITABL(INDX,IND+IST2)) THEN
        ISTP = IST2 - 1
        GO TO 210
      ELSE IF(IVAL.GT.ITABL(INDX,IND+IST2)) THEN
        IND  = IND + IST2
        ISTP = ISTP - IST2
        GO TO 210
      ELSE
        LOCTAB = IND + IST2
        RETURN
      ENDIF
  250 LOCTAB = - IND
      RETURN
      END
#endif
