      SUBROUTINE BKFMT(NAME,FMT)
C
C     DEFINE FORMAT OF BANKS 'NAME'
C
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      CHARACTER*4 NAME
      CHARACTER*(*) FMT
      CHARACTER*1 FM
      INTEGER IFMT(196)
      EQUIVALENCE (INTA(1),IFMT(1))
C     ...
      IF(IW(ICK).NE.12345) CALL BBNOT(IW,'BKFMT')
      NAMI=NAMIND(NAME)
      ID=IDFMT+NAMI-NSYST
C
C     IGNORE CALL, IF FORMAT ALREADY SPECIFIED BY A CALL
C
C          CONTENT OF +FMT BANK
C
C      1   NAME
C      2   ORIGIN (=3 DEFINED BY CALL, =4 DEFINED IN BOS)
C      3   BIT CODE (=1 B32, =2 B16, =3 B08)
C      4   FMT CODES, IF BIT CODE = 0
C
C      FMT CODE   TRANSLATED FROM...
C
C      N*10       N'('
C             0    ')'
C      N*10 + 1   N'F'
C      N*10 + 2   N'I'
C      N*10 + 3   N'A'
C
      IF(IW(ID).NE.0) THEN
          IF(IW(IW(ID)+2).EQ.3) GOTO 100
      END IF
C
C                ARRAY IFMT
C
      CALL TRFMT(FMT,IFMT,NFMT)
      IFMT(1)=IW(IDNAM+NAMI-NSYST)
      IF(NFMT.EQ.0) THEN
         IF(IW(LUP).GT.0) WRITE(IW(LUP),1001) NAME,FMT
         IEFMT=-1
         GOTO 100
      END IF
C
C     STORE IN WORK BANK
C
   20 CALL BBFMT(NAMI,IFMT,NFMT)
  100 RETURN
 1001 FORMAT('0BOS--BKFMT-  FORMAT ERROR'/
     1       14X,'CALL BKFMT(',A4,',',A,')')
      END
