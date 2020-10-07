      FUNCTION MREADC(JW)

*     READ ONE BANK FROM CARDS AND RETURN INDEX
*     ZERO IS RETURN FOR 'ENDQ' CARD
*     FOR EOF ZERO IS RETURNED AND JW(2)=7
C - modified by : F.Ranjard - 900522
C                 add entry point MRESET(LDUM) to reset LUNC=-1
C                 and IW(5)=LDUM
C                 i.e.   CALL BREADC    !will read from IW(5)=5
C                        IDUM=MRESET(15)!will set IW(5)=15 , LUNC=-1
C                        CALL BREADC    !will read from IW(5)=15
C                        IDUM=MRESET(5) !will set IW(5)=5 , LUNC=-1
C                        CALL BREADC    !will read from IW(5)=5
C - modified by : F.Ranjard - 920629
C                 add SAVE statment
#include "boscom.h"
#include "param.h"
      INTEGER JW(*),IFMT(4)
      EQUIVALENCE (IPR,IW(4))
      SAVE LUNCS, TXTS
      CHARACTER*72 TXT,TXTS,TXTR
      CHARACTER*4 CHAINT,NAME
      COMMON/COMCNV/LABEL,ISL,NT,NFR(128),LFR(128)
      DATA IFMT/0,4,0,183/,LIN/0/,LUNC/-1/
      IF(LUNC.LT.0) THEN
         LUNC=IW(5)
         LUNCS=LUNC
      END IF
      JW(1)=0
      JW(2)=0
      MREADC=0
   10 IF(LUNC.EQ.0) THEN
         JW(2)=7
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) ' ','(EOF)'
         GOTO 100
      END IF
      IF(LIN.EQ.1) THEN
         TXT=TXTS
         GOTO 25
      END IF
      IF(LIN.LT.0) GOTO 90
   20 READ(LUNC,101,END=40) TXTR
      IS = 1
      TXT=' '
      IF (TXTR(1:1).EQ.' ') IS = 2
      TXT = TXTR(IS:72)
      IF(TXT(1:1).EQ.'*') THEN
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) '*',TXT
         GOTO 20
      END IF
      TXTS=TXT
   25 CALL CNVCHA(TXT)
      LIN=1
*     check and process $FMT input, which is: name $FMT 'format'
      LA=INDEX(TXT,'$FMT')
      IF(LA.NE.0) THEN
         IF(MREADC.NE.0) GOTO 100
         LB=INDEX(TXT(LA+4:72),'''')+LA+3
         IF(LB.EQ.LA+3) THEN
*           no starting ' - assume end
            LC=73
         ELSE
            LC=INDEX(TXT(LB+1:72),'''')+LB
*           no ending ' - assume end
            IF(LC.EQ.LB) LC=73
         END IF
         NAME=CHAINT(LABEL)
         IF(IPR.EQ.1.AND.JW(LUP).NE.0) WRITE(JW(LUP),103) 'F',TXT
         CALL BKFMT(NAME,TXT(LB+1:LC-1))
         GOTO 20
      END IF
*     CHECK SPECIAL CARDS

   30 JPR=IPR
      IF     (LABEL.EQ.INTCHA('UNIT')) THEN
         LUNC=NFR(1)
      ELSE IF(LABEL.EQ.INTCHA('POFF')) THEN
         IPR=0
      ELSE IF(LABEL.EQ.INTCHA('PON ')) THEN
         IPR=1
      ELSE IF(LABEL.NE.INTCHA('ENDQ')) THEN
         GOTO 50
      END IF

*     PRINT SPECIAL CARD

      IF(IPR+JPR.NE.0.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) '+',TXT

      IF(MREADC.NE.0) THEN
         LIN=0
         IF(LABEL.EQ.INTCHA('ENDQ')) LIN=-1
      ELSE
         IF(LABEL.NE.INTCHA('ENDQ')) GOTO 20
         LIN=0
      END IF
      GOTO 100

*     EOF CASES

   40 IF(LUNC.EQ.LUNCS) THEN
         LUNC=0
      ELSE
         LUNC=LUNCS
      END IF
      IF(MREADC.NE.0) GOTO 90
      GOTO 10

*     CREATE BANK OR ADD TO BANK

   50 NP=0
      IF(MREADC.EQ.0) THEN
         ND=0
         NAME=CHAINT(LABEL)
         NAMI=NAMIND(NAME)
C
C        RESET LINK BANKS
C
         JND=JW(IDL)+NAMI-NSYST
         IF(JW(JND).NE.0) JW(JW(JND))=0

         IF(ISL.EQ.0) THEN
            NR=0
            IND=NAMIND(NAME)+1
   55       IND=JW(IND-1)
            IF(IND.NE.0) THEN
               NR=JW(IND-2)+1
               GOTO 55
            END IF
         ELSE
            NP=1
            NR=NFR(1)
         END IF
         LI=INDEX(TXT,'$TEXT')
         IF(LI.NE.0) THEN
            LL=INDEX(TXT,'$TEXTC')
*           SPECIAL TEXT INPUT BY  $TEXT, DEFINE FORMAT '18A'
            IFMT(1)=INTCHA(NAME)
            CALL BBFMT(NAMI,IFMT,4)

            MREADC=MBANK(JW,NAME,NR,0)
            IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),102) NAME,NR,TXT
            IF(MREADC.EQ.0) GOTO 200
            NW=0
   70       READ(LUNC,101,END=40) TXT
            IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) ' ',TXT
            IF(TXT(1:4).EQ.'END$'.OR.TXT(1:4).EQ.'ENDQ') THEN
*              PROCESS DATA FOR $TEXTC
               IF(LL.NE.0) CALL BCALLC(JW,NAME,NR)
               IF(TXT(1:4).EQ.'END$') GOTO 90
               LIN=-1
               GOTO 100
            END IF
            MREADC=MBANK(JW,NAME,NR,NW+18)
            IF(MREADC.EQ.0) GOTO 200
            DO 75 I=1,18
   75       JW(MREADC+NW+I)=INTCHA(TXT(4*I-3:4*I))
            NW=NW+18
            GOTO 70
         END IF
         MREADC=MBANK(JW,NAME,NR,NT-NP)
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),102) NAME,NR,TXT
         IF(MREADC.EQ.0) GOTO 200
      ELSE
         IF(CHAINT(LABEL).NE.'    ') GOTO 100
         ND=JW(MREADC)
         MREADC=MBANK(JW,NAME,NR,ND+NT)
         IF(MREADC.EQ.0) GOTO 200
         IF(IPR.EQ.1.AND.JW(LUP).GT.0) WRITE(JW(LUP),103) ' ',TXT
      END IF
      DO 60 I=1,NT-NP
   60 JW(MREADC+ND+I)=NFR(I+NP)
      GOTO 20
   90 LIN=0
  100 RETURN
  200 IF(JW(LUP).GT.0) WRITE(JW(LUP),104)
      CALL BABEND('MREADC')
      GOTO 100
C
C - entry point to reset LUNC=-1 and IW(5)
C
      ENTRY MRESET(LDUM)
      IW(5)= LDUM
      LUNC = -1
      RETURN
C
  101 FORMAT(A72)
  102 FORMAT(' -----------  ',A4,I10,2X,A)
  103 FORMAT(28X,A1,1X,A)
  104 FORMAT('0M/BREADC---  INSUFFICIENT SPACE - ABEND')
      END
