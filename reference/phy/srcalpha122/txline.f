      SUBROUTINE TXLINE(STR,IKOD)
CKEY TOOLS / USER
C-----------------------------------------------------------------------
C! Auxiliary to TXTREE : output a character string in the LaTex file
C  Author   : A. Bonissent October 1992
C If IKOD = 0, the line is actually output on the file.
C Else, the character string is appended to the current line
C-----------------------------------------------------------------------
      CHARACTER*132 LINE
      CHARACTER*(*) STR
      COMMON / TXTRLU / LUNTXT
      DATA LN/0/
C-----------------------------------------------------------------------
      LUN = LUNTXT
      NC=LEN(STR)
      IF (NC+LN.GT.132) THEN
         WRITE(LUN,900)LINE(1:LN)
         LN=0
      ENDIF
      NCO=NC
      DO NL=1,NC
        NCUR=NC+1-NL
        IF(STR(NCUR:NCUR).NE.' ')THEN
           NC=NCUR+1
           IF(NC.GT.NCO)NC=NCO
           GO TO 10
        ENDIF
      ENDDO
   10 CONTINUE
      LINE(LN+1:LN+NC)=STR
      LN=LN+NC
      IF(IKOD.EQ.1)THEN
         WRITE(LUN,900)LINE(1:LN)
         LN=0
      ENDIF
  900 FORMAT(1X,A)
      RETURN
      END
