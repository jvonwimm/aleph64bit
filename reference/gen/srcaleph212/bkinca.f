      INTEGER FUNCTION BKINCA(CHSTR)
C-----------------------------------------------------------------------
C! Transforms the character representation of an integer into this integ
CKEY BOOK  CHARACTER INTEGER / USER
C Input argument :
C   CHSTR = character string containing only an integer ( 9 digits max)
C   + or - as first character is allowed ( e.g. :  '-123456'  )
C   any other character inside the string stops the conversion
C Return value BKINCA = integer value ( with eventually its sign )
C                     = 0 if too large integer
C-----------------------------------------------------------------------
      CHARACTER*(*) CHSTR
      CHARACTER*1 DIG
C
      DATA MAX /1286608615/
      BKINCA=0
      IVAL=0
      LNC=LNBLNK(CHSTR)
      IDIG=0
C Loop on all digits
      DO 1 I=LNC,1,-1
         IDIG=IDIG+1
         DIG=CHSTR(I:I)
         IF (DIG.EQ.'-') GO TO 200
         IF (ICHAR(DIG).GT.ICHAR('9')) GO TO 100
         IF (ICHAR(DIG).LT.ICHAR('0')) GO TO 100
         IVDI=ICHAR(DIG)-ICHAR('0')
         IVAL=IVAL+IVDI*(10**(IDIG-1))
         IF (IVAL.GT.MAX) GO TO 999
 1    CONTINUE
C
 100  BKINCA=IVAL
      GO TO 999
C Negative integer
 200  BKINCA=-IVAL
C
 999  RETURN
      END
