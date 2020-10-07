      SUBROUTINE ALSTRI(STR,CHLIM,NWMAX,CHOUT,NWORD)
C---------------------------------------------------------------------
C!  Blow character string STR into character array CHOUT, limiter CHLIM
CKEY ALEF CARD STRING / USER
C   Author     :Ronald Hagelberg        17-APR-91
C
C  Remove all laeding and trailing blanks of the character string STR
C  Blow character string STR into character array CHOUT according to
C  limiter character CHLIM. Generaly CHLIM will be the blank ' ' , but
C  could be as well e.g. '|' .
C
C!   Inputs: STR    CHARARACTER string with length to be defined
C!                        in the calling program
C!           CHLIM  CHARARACTER*1 delimiting the substrings
C!           NWMAX  Dimension of CHOUT, Maximum Number of substrings
C!
C!   Output: CHOUT  ARRAY of CHARARACTER Strings, with dimension
C!                  and length to be defined in calling program.
C!           NWORD  Number of substrings found
C!
C?   Example of character string:
C?                         SMCF   I25138  -K   Wait  SIZE   200 FS 1
C?   taken here from a FILI card:
C? FILI 'ALDATA | EPIO |   SMCF   I25138  -K   Wait  SIZE   200 FS 1 '
C?   The syntactical separator CHLIM  is here the blank ' '.
C?   ABCD 'PARM1 | XYZ   | PARM3 | | PARM5|PARM6   '
C?   The syntactical separator CHLIM  is here  '|'.
C?=====================================================================
C
      DIMENSION CHOUT(*)
      CHARACTER*(*) CHOUT
      CHARACTER*1 CHLIM
      CHARACTER*1 BLANK
      CHARACTER*(*) STR
      PARAMETER (BLANK=' ')
      IYW=0
      IFW=0
      ILW=0
      JW=0
C            dimension of CHOUT is NWMAX
      DO I=1,NWMAX
         CHOUT(I)=' '
      ENDDO
C
      L=LNBLNK(STR)
C
      DO 20 I=1,L
C            check if first character blank or limiter
         IF (IYW.EQ.0) THEN
            IF (STR(I:I).EQ.BLANK) THEN
C            get rid of leading blanks
               CONTINUE
            ELSEIF (STR(I:I).EQ.CHLIM) THEN
C            if limiter non blank then it is significant
               JW=JW+1
               CHOUT(JW)=' '
            ELSEIF (STR(I:I).NE.BLANK) THEN
               IFW=I
               ILW=I
               IYW=1
               JW=JW+1
            ENDIF
         ENDIF
         IF (IYW.NE.0) THEN
C            after start of word (IYW.NE.0) , check for limiter
            IF (STR(I:I).EQ.CHLIM) THEN
               CHOUT(JW)=STR(IFW:ILW)
               IYW=0
C            check if last character of string
            ELSEIF (I.EQ.L) THEN
               ILW=I
               CHOUT(JW)=STR(IFW:ILW)
C            get rid of trailing blanks
               LCJW=LNBLNK(CHOUT(JW))
               CHOUT(JW)=CHOUT(JW)(:LCJW)
C            check if non blank character after start of word (IYW.NE.0)
            ELSEIF (STR(I:I).NE.BLANK) THEN
               ILW=I
            ENDIF
         ENDIF
   20 CONTINUE
C
      NWORD=JW
      RETURN
      END
