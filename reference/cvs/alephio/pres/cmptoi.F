      LOGICAL FUNCTION CMPTOI(STR,I)
C-----------------------------------------------------------------------
C!     Convert a character string to integer
CKEY PRESS CMPTOI COMPRESS /INTERNAL
C!     AUTHOR: D.Harvatis   MAR 89
C!
C!     INPUT :   STR  : character string to be converted to integer.
C!     OUTPUT:   I    : integer variable in which the converted value
C!                      will be placed.
C!     RETURN VALUE   : .FALSE. for successfull conversion
C!                      .TRUE. if conversion fails
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) STR
      LOGICAL START
C-----------------------------------------------------------------------
      START=.TRUE.
      L=LEN(STR)
      K0=ICHAR('0')
      I=0
      IS=1
      J=0
C     find sign
      IF (STR(1:1).EQ.'-') THEN
          IS=-1
          J=1
      ELSE IF (STR(1:1).EQ.'+') THEN
          J=1
      END IF
10        J=J+1
C         skip leading spaces
          IF ((STR(J:J).EQ.' ').AND.(START)) GO TO 10
C         in case of '.',SPACE, or end of string terminate conversion
          IF ((STR(J:J).EQ.'.').OR.(J.GT.L).OR.(STR(J:J).EQ.' '))
     +     GO TO 11
          K=ICHAR(STR(J:J))-K0
C         if character not digit -> failed
          IF ((K.LT.0).OR.(K.GT.9)) GO TO 12
          LIM=(2147483647-K)/10
C         if too large -> failed
          IF (I.GT.LIM) GO TO 12
          START=.FALSE.
          I=I*10+K
          GO TO 10
C
C     succesfull conversion
11    I=IS*I
      CMPTOI=.FALSE.
      RETURN
C
C     character that is not a digit or SPACE or '.' found
C     convertion failed
C     I = so far found value
12    I=IS*I
      CMPTOI=.TRUE.
      RETURN
      END
#endif
