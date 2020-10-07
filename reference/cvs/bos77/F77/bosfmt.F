      SUBROUTINE BOSFMT(NAME,TEXTA)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*(*) TEXTA
      CHARACTER*4 NAME, TEXT*110, NUMBER(0:9)*1
      DATA NUMBER/'0','1','2','3','4','5','6','7','8','9'/
      TEXT=' '
      I   =NAMIND(NAME)
      NAMA=IW(IDNAM+I)
      ID=IDFMT+I-NSYST
      IF(IW(ID).EQ.0) THEN
         TEXT(1:11)='I (default)'
         L=11
         GOTO 30
      END IF
      ID=IW(ID)
      IF(IW(ID+3).NE.0) THEN
         L=3
         IF(IW(ID+3).EQ.1) TEXT='B32'
         IF(IW(ID+3).EQ.2) TEXT='B16'
         IF(IW(ID+3).EQ.3) TEXT='B08'
      ELSE
         L=0
         J=4
   10    IF(IW(ID).GE.J) THEN
            IF(L.GE.100) THEN
               L2=L+2
               TEXT(L:L2)='...'
               L=L2
               GOTO 30
            END IF
            JL=IW(ID+J)/10
            JR=MOD(IW(ID+J),10)
            J =J+1
            IF(JL.EQ.0) THEN
               L=L+1
               TEXT(L:L)=')'
               GOTO 10
            ELSE
            IF(L.NE.0) THEN
               IF(TEXT(L:L).NE.'(') THEN
                  L=L+1
                  TEXT(L:L)=','
               END IF
            END IF
C
C              INSERT NUMBER
C
               IF(JL.NE.1.AND.JL.NE.100000) THEN
                  NDIV=10000
                  LSTART=L
                  DO 20 KL=1,5
                  K=MOD(JL/NDIV,10)
                  NDIV=NDIV/10
                  IF(K.NE.0.OR.L.NE.LSTART) THEN
                     L=L+1
                     TEXT(L:L)=NUMBER(K)
                  END IF
   20             CONTINUE
               END IF
               IF(JR.EQ.0) THEN
                  L=L+1
                  TEXT(L:L)='('
               ELSE
                  L=L+1
                  IF(JR.EQ.1) TEXT(L:L)='F'
                  IF(JR.EQ.2) TEXT(L:L)='I'
                  IF(JR.EQ.3) TEXT(L:L)='A'
                  IF(JR.EQ.4) TEXT(L:L)='J'
                  IF(JR.EQ.5) TEXT(L:L)='K'
               END IF
               GOTO 10
            END IF
         END IF
      END IF
   30 TEXTA=TEXT
      IF(L.GT.LEN(TEXTA)) THEN
         L=LEN(TEXTA)
         TEXTA(L-2:L)='...'
      END IF
  100 RETURN
      END
