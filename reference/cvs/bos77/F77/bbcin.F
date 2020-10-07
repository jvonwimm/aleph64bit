      SUBROUTINE BBCIN(ICA,NBC)
*
*     CONVERT TO EXTERNAL FORMAT
*
      INTEGER ICA(100)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      INTEGER LV(3,10)
      CHARACTER*4 CHAINT
      DATA NAMA/0/,ISC/0/

      IF(NBC.EQ.0) THEN
         ISC=0
         GOTO 100
      END IF

      IF(ISC.NE.0) THEN
         IF(ISC.EQ.5) THEN
            IND=0
            ID=IW(IDFMT+NAMJ-NSYST)
         ELSE
            IND=5-ISC
         END IF
C        WRITE(6,711) IND,NBC,ISC
 711     FORMAT(' BBCIN IND NBC ISC',3I10)
         GOTO (11,12,13,14,15),ISC
      END IF
      IND=4
      ISC=1

*     CONVERT NAME
   11 IF(IND-3.GT.NBC) GOTO 100
      IF(ICA(IND-3).NE.NAMA) THEN
         NAMA=ICA(IND-3)
         NASC=NAMA
#include "toasc.h"
         NAMC=NASC
*        DETERMINE NAMJ = NAME-INDEX FOR INTEGER NAMA
*                      (ALWAYS NONZERO)
         NWNAM=IW(IDNAM)
         IW(IDNAM)=NAMA
         NAMJ=IW(IDNAM-2)+MOD(IABS(IW(IDNAM)),NPRIM)-3
    1    NAMJ=IW(IDPTR+NAMJ)
         IF(IW(IDNAM+NAMJ).NE.IW(IDNAM)) GOTO 1
         IW(IDNAM)=NWNAM
         NAMJ=NAMJ+NSYST
         IF(NAMJ.EQ.NSYST) NAMJ=NAMIND(CHAINT(NAMA))
      END IF
      ICA(IND-3)=NAMC
      ISC=2

*     CONVERT NUMBER
   12 IF(IND-2.GT.NBC) GOTO 100
      CALL CTOIBM(ICA(IND-2),1,2)
      ISC=3

*     SET POINTER TO ZERO
   13 IF(IND-1.GT.NBC) GOTO 100
      ICA(IND-1)=0
      ISC=4

*     CONVERT NW
   14 IF(IND  .GT.NBC) GOTO 100
      NW =ICA(IND)
      ICO=NW
      CALL CTOIBM(ICA(IND  ),1,2)
      ISC=5
      IF(NW.EQ.0) GOTO 70

*     CONVERT BANK DATA TO IBM FORMAT
*     DEFINE N AND K

*     N = INFINITY
      N =999 999 999
      K =0
      ID=IW(IDFMT+NAMJ-NSYST)
      IF(ID.EQ.0) THEN
*        NO FORMAT DEFINED, ASSUME I FORMAT
         K =2
         GOTO 50
      END IF
      IF(IW(ID+3).EQ.0) GOTO 30
*     BIT FORMAT CODES
      IF(IW(ID+3).EQ.1) THEN
*        32 BIT WORDS ARE CONVERTED
         K=2
      END IF
      GOTO 50
*     FORMAT FROM FORMAT BANK
   30 I    =3
      ITERM=3
      LEV=0

*     GET NEXT FORMAT CODE

   40 I=I+1
      IF(I.GT.IW(ID)) THEN
         I=ITERM
*        REPEAT FORMAT CODES FROM ITERM
         GOTO 40
      END IF

*     RIGHT PARENTHESIS

      IF(IW(ID+I).EQ.0) THEN
         LV(3,LEV)=LV(3,LEV)+1
         IF(LV(3,LEV).GE.LV(2,LEV)) THEN
            ITERM=LV(1,LEV)-1
            LEV=LEV-1
         ELSE
            I=LV(1,LEV)
         END IF
*
      ELSE
         N=IW(ID+I)/10
         K=IW(ID+I)-10*N
         IF(K.EQ.0) THEN

*        LEFT PARENTHESIS

            LEV=LEV+1
            LV(1,LEV)=I
            LV(2,LEV)=N
            LV(3,LEV)=0
         ELSE

*           FORMAT F OR I OR A

            IF(IW(ID).NE.4) THEN
               IF(I.NE.IW(ID)-1)    GOTO 50
               IF(LEV.EQ.0)         GOTO 50
               IF(I.NE.LV(1,LEV)+1) GOTO 50
            END IF
*           N = INFINITY
            N=999 999 999
            GOTO 50
         END IF
      END IF
      GOTO 40

*     FORMAT FROM FORMAT BANK IS: N WORDS TO BE CONVERTED ACCCORDING K
   50 CONTINUE
   15 IF(N.EQ.0) GOTO 40
  101 FORMAT(' CONVERT',I4,' WORDS WITH CODE',2I4)
      M=MIN(N,ICO,NBC-IND)
      N=N-M
C     WRITE(6,101) N,K,M
*     CONVERSION
      IF(K.EQ.1) THEN
*        CONVERT TO FLOATING POINT
         CALL CTOIBM(ICA(IND+1),M,3)
      ELSE IF(K.EQ.2) THEN
*        CONVERT TO INTEGER
         CALL CTOIBM(ICA(IND+1),M,2)
      ELSE IF(K.EQ.3) THEN
*        CONVERT TO A4 FORMAT
         DO 60 LL=1,M
         NASC=ICA(IND+LL)
#include "toasc.h"
   60    ICA(IND+LL)=NASC
      END IF
      ICO=ICO-M
      IND=IND+M

   70 IF(IND.EQ.NBC) THEN
         IF(ICO.EQ.0) ISC=0
         GOTO 100
      END IF
      IF(ICO.GT.0) GOTO 40
*     BANK FINISHED
      IND=IND+4
      ISC=1
      GOTO 11

  100 RETURN
      END
