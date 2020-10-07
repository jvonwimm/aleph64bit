      FUNCTION JULREC (ATYP,CHDEVI)
C --------------------------------------------------------------------
CKEY ALEF USER LREC / USER
C - F.Ranjard - 900307
C! return the  record length
C - Input     : ATYP  / CHAR     aleph type
C               CHDEVI / CHAR    file device
C - Output    : JULREC           record length in bytes
C
C Look for the keyword LREC in FDEVI
C IF it is found decode the record length which follows and reduce
C    the CHDEVI length
C ELSE return the aleph record length for the ATYP type.
C
C -------------------------------------------------------------------
      CHARACTER*(*) ATYP,CHDEVI
C -------------------------------------------------------------------
      ICHAR0 = ICHAR ('0')
      LENDEV = LNBLNK (CHDEVI)
      NUMB = 0
      LR = INDEX (CHDEVI,'LREC')
      IF (LR.GT.0) THEN
         DO 69 I= LR+5,LENDEV
            IF (CHDEVI(I:I).EQ.' ') GOTO 69
            N = ICHAR(CHDEVI(I:I)) - ICHAR0
            IF (N.GE.0 .AND. N.LE.9) NUMB = 10*NUMB + N
 69      CONTINUE
         CHDEVI = CHDEVI (1:LR-2)
         LREC = NUMB
      ENDIF
      IF (NUMB.EQ.0) LREC = KALREC(ATYP)
C
      JULREC = LREC
C
      END
