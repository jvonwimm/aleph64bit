      SUBROUTINE BPRTAB (NAMI,IND,LROW1,LROW2)
C -------------------------------------------------------
C! print tabular named bank
CKEY PRINT BOS BANK
C - F.Ranjard - 900830    from V.Blobel WPRNT/BOS77
C - Input      : NAMI  / I  = bank name-index
C                IND   / I  = bank index
C                LROW1 / I  = print from row# LROW1
C                LROW2 / I  =       to   row# LROW2
C - Bank       : UFMT  / Input
C

C -------------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL IGTFMT
#include "bcs.h"
      CHARACTER*4 CHAINT
      INTEGER LW(30)
      LOGICAL FTABL
      DATA NUFMT /0/
C ---------------------------------------------------------

      IF(IND.LE.0) GOTO 20

C - get the bank format: if IFMT=0 then no format

         IF (NUFMT.EQ.0) NUFMT = NAMIND ('UFMT')
         JUFMT = IW(NUFMT)
         IF (JUFMT.NE.0) THEN
C          if there is a user format bank look if the bank
C          name to be printed is the one given in the format
            IF (IW(IND-3).EQ.IW(JUFMT+1)) THEN
               IFMT = JUFMT
            ELSE
               IFMT = IGTFMT(NAMI)
            ENDIF
         ELSE
            IFMT=IGTFMT(NAMI)
         ENDIF
         IF(IFMT.EQ.0) GOTO 10
C
C        FORMAT FOR THIS BANK IS DEFINED
C
         IF(IW(IFMT+3).EQ.0) THEN
C
C           FORMAT CODES
C
            DO 30 L = 4,IW(IFMT)
            MODA=MOD(IW(IFMT+L),10)
            IF(MODA.NE.3.AND.MODA.NE.0) GOTO 40
   30       CONTINUE
*           only text - use text format
            WRITE(IW(6),1007) (CHAINT(IW(IND+L)),L=1,IW(IND))
            GOTO 50
*           use format description
   40       CALL FPRROW(IW(6),IW(IND+LMHLEN+1),IW(IND+LMHCOL),
     &                  LROW1,LROW2,IW(IFMT+1),IW(IFMT))
            IF (IW(IFMT).LE.0) THEN
C             the format is not tabular : use normal printing
               IW(IFMT) = - IW(IFMT)
               CALL WPRNT (IW,IND)
            ENDIF
   50       CONTINUE
         ELSE
C
C           FORMAT IS B32 OR B16 OR B8
C
            IFMT=IW(IFMT+3)
            N1=1
            N3=10*IFMT
            IF(IFMT.EQ.3) IFMT=4
            NWRD=IFMT*IW(IND)
            IF(NWRD.EQ.0) GOTO 20
    2       N3=MIN(N3,NWRD-N1+1)
               NP=MOD(N1+N3-1,1000)
            IF(IFMT.EQ.1) THEN
               CALL BLO32W(IW(IND+1),2*(N1-1)+1,LW,1,N3)
               WRITE(IW(6),1003) N1,NP,(LW(L),L=1,N3)
            ELSE IF(IFMT.EQ.2) THEN
               CALL BLO16W(IW(IND+1),N1,LW,1,N3)
               WRITE(IW(6),1004) N1,NP,(LW(L),L=1,N3)
            ELSE
               CALL BLO8W (IW(IND+1),N1,LW,1,N3)
               WRITE(IW(6),1005) N1,NP,(LW(L),L=1,N3)
            END IF
            N1=N1+N3
            IF(N1.LT.NWRD) GOTO 2
         ENDIF
      GOTO 20
   10 CALL BBUFM(IW(IND+1),1,IW(IND),IW(6))
   20 CONTINUE
C
  100 RETURN
 1003 FORMAT(1X,I5,'-',I3,1X,10I12)
 1004 FORMAT(1X,I5,'-',I3,1X,20I 6)
 1005 FORMAT(1X,I5,'-',I3,1X,30I 4)
 1007 FORMAT(30X,18A4)
      END
#endif
