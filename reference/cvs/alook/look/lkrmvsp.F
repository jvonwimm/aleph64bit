      SUBROUTINE LKRMVSP(LLIN,NOUT)
C --------------------------------------------------------------
C - A.Bonissent
C! Remove all spaces in a character string
C --------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*)LLIN
      CHARACTER*132 LOCAL
C --------------------------------------------------------------
      NC=LEN(LLIN)
      NOUT=0
      DO 10 IC=1,NC
      IF(LLIN(IC:IC).NE.' ')THEN
         NOUT=NOUT+1
         LOCAL(NOUT:NOUT)=LLIN(IC:IC)
      ENDIF
   10 CONTINUE
      LLIN(1:NOUT)=LOCAL(1:NOUT)
      RETURN
      END
#endif
