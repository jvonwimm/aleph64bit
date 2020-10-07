      INTEGER FUNCTION LKLWRD(STR)
C ----------------------------------------------------------
C! find the length of a string
C ----------------------------------------------------------
#ifndef DOC
      CHARACTER STR*(*)
C -----------------------------------------------------------
C - len1 = position of last character preceding the 1st " "
      LEN1=INDEX(STR,' ')-1
C - len2 = position of last character preceding the 1st "/"
      LEN2=INDEX(STR,'/')-1
C - if there is a "/" before the 1st " " or there is no " "
C   then len1 = position of last char. before the "/"
      IF (LEN2.GT.-1.AND.LEN2.LT.LEN1.OR.LEN1.EQ.-1) LEN1=LEN2
C - len2 = position of 1st '"'
      LEN2=INDEX(STR(1:),'"')
C - if there is a '"' look for the 2nd '"'
C   the length of the string is defined by the position of the 2nd '"'
      IF (LEN2.LE.LEN1.AND.LEN2.GT.0) LEN1=INDEX(STR(LEN2+1:),'"')+LEN2
      IF (LEN1.EQ.-1) LEN1=LEN(STR)
      LKLWRD=LEN1
      RETURN
      END
#endif
