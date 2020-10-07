      SUBROUTINE BBEPF(JW,LUN,IBF,LIST)
C
C     WRITE SET OF FORMAT BANKS USING EPIO WRITE
C     FLR - 960219
C     if the data card COMP 'NOFM' is present
C     do not write but RETURN
C
      CHARACTER*(*) LIST, NAME*4
#include "boscom.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      INTEGER JW(*)
#include "param.h"
#include "mparam.h"
C
C - FLR corrections - 960219 =========================
      CHARACTER*4 CHAINT
      INTEGER NACOMP /0/
      LOGICAL NOFMFL /.FALSE./
      IF (NOFMFL) RETURN
      IF (NACOMP.EQ.0) THEN
        NACOMP = NAMIND('COMP')
        JCOMP = IW(NACOMP)
        IF (JCOMP.GT.0) THEN
          DO I=1,IW(JCOMP)
            IF (CHAINT(IW(JCOMP+I)).EQ.'NOFM')THEN
              NOFMFL=.TRUE.
              GOTO 100
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C - End of FLR corrections - 960219 ==================
C
      IC=0
      ISTART=0
      ICOUNT=0
      JCOUNT=0
*     work bank to copy to
      NBN=(IW(IBF+2)-12)*16/NBITWD
      NBC=0
#include "unpackio.h"
*
*     loop on all banks
*
#include "listnn.h"
      IF(NAMI.EQ.0) GOTO 82
      ID=IW(IDFMT+NAMI-NSYST)
      IF(ID.EQ.0) GOTO 3
      ISTART=ID-3
      ICOUNT=IW(ID)+4
*     transmit this bank
   80 JSTART=ISTART
      JCOUNT=ICOUNT
   81 JCOPY =MIN0(JCOUNT,NBN-NBC)
      CALL UCOPY(IW(JSTART),IW(IBC+NBC+1),JCOPY)
      JSTART=JSTART+JCOPY
      JCOUNT=JCOUNT-JCOPY
      NBC   =NBC   +JCOPY
      IF(JCOUNT.EQ.0.AND.NAMI.NE.0) GOTO 90
   82 IF(NBC.EQ.0) GOTO 90
*     next logical record
      IF(NAMI.EQ.0.AND.JCOUNT.EQ.0) THEN
         IF(IC.GE.1) IC=3
      ELSE
         IF(IC.LE.1) IC=IC+1
      END IF
      IW(IBC)=NBC
*     format conversion
      CALL BBCIN(IW(IBC+1),NBC)
*
*     change status word 5 (=logical record type identifier = logical
*        record header word 2) to IC
*
      CALL EPSETW(LUN,5,IC,IER)
      IF(IER.NE.0) GOTO 202
C
C     3 = PACK DATA BEFORE WRITING, GIVEN AS 32 BIT RIGHT ADJUSTED
C     NBC    = NR OF WORDS TO BE WRITTEN IN 32 BIT UNITS
C     IW(IBC+1   = AREA CONTAINING DATA
C     IW(IBF+21) = BUFFER PROVIDED
      CALL EPGETW(LUN,11,IBLOCK,IDUMER)
C
C =========================================================
C FLR correction to know wheither physical record is full and written
C     out before the present buffer is used
C     the following test assume that
C      - the header is written in 16bit words : status word(3)=16
C      - logical record can be spanned        : status word(8)=1
C
      CALL EPGETW(LUN,14,NWFLR,IER)
      CALL EPGETW(LUN,1,NTFLR,IER)
      IF (NWFLR+4.GT.NTFLR) IBLOCK=IBLOCK+1
C FLR end of correction  ==================================
C
      IF(IBLOCK.NE.IW(IBF+4)) THEN
         IW(IBF+4)=IBLOCK
         IW(IBF+5)=0
      END IF
C
      CALL EPOUTS(LUN,3,NBC   ,IW(IBC+1),IW(IBF+21),IER)
      NBC=0
      IW(IBF+5)=IW(IBF+5)+1
      IF(IW(IBF+9).EQ.0) IW(IBF+9)=1000*(IW(IBF+4)+1)+IW(IBF+5)
      RW(IBF+8)=RW(IBF+8)+4.0E-6*FLOAT(NBC)
      IF(IER.NE.0) GOTO 202
      IF(JCOUNT.NE.0) GOTO 81
   90 ISTART=0
      ICOUNT=0
      IF(NAMI.NE.0) GOTO 3
*     bank formats finished
  100 RETURN
  202 IF(IW(LUP).GT.0) WRITE(IW(LUP),1002) IER
      CALL BABEND('BBEPF')
      GOTO 100
 1002 FORMAT('0BOS--BBEPF-  EPIO ERROR',I3,' - STOP')
      END
