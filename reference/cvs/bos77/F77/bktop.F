      SUBROUTINE BKTOP(JW,NAME,NR,P,MP,NP,MC,NC)
*
*     COPY DATA FROM BANK 'NAME',NR TO TWODIM. ARRAY P
*
*     NO COPY WITH NC=0, IF
*        BANK NOT FOUND
*        CONTRADICTION IN BANK LENGTH AND FIRST WORD
*        LENGTH = 1 + FIRST WORD * INTEGER
*
      INTEGER JW(1000),P(MP,NP)
      CHARACTER*4 NAME
      MC=0
      NC=0
      IND=MLINK(JW,NAME,NR)
      IF(IND.EQ.0) GOTO 100
      MC=JW(IND+1)
      IF(MC.EQ.0) THEN
         NC=0
      ELSE
         NC=(JW(IND)-1)/MC
      END IF
      IF(NC*MC+1.NE.JW(IND)) NC=0
      IF(NC.EQ.0) GOTO 100
      IF(MC.EQ.MP) THEN
         CALL UCOPY(JW(IND+2),P,MC*NC)
      ELSE
         IOF=IND+1
         DO 30 J=1,NC
         DO 10 I=1,MIN(MC,MP)
   10    P(I,J)=JW(IOF+I)
         IOF=IOF+MC
         IF(MC.LT.MP) THEN
            DO 20 I=MC+1,MP
   20       P(I,J)=0
         END IF
   30    CONTINUE
      END IF
  100 RETURN
      END
