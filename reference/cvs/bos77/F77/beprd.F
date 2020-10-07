      SUBROUTINE BEPRD(JW,LUN,LIST,*,*)
C
C     READ FROM UNIT LUN
C
      INTEGER JW(*),LHEAD(200)
      EQUIVALENCE (LHEAD(1),INTA(1))
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*(*) LIST
*     ...
      IF(JW(1).LT.0) THEN
*     end-of-file
         JW(1)=0
         JW(2)=0
         GOTO 102
      END IF
      LRPIN=JW(1)
      JW(1)=0
      JW(3)=0
      LRP=0

*     initialize EPIO package
      IF(LEPIO.EQ.0) THEN
         CALL EPINIT
*        CALL EPSTAT
         LEPIO=1
*        IF(IW(6).NE.0) CALL EPIOT(IW(6))
      END IF
      NBLK=0

*     bufferbank for unit LUN

      IBF= NLINK('+BUF',LUN)
      IOAC=0
      IOMD=1
      IOST=0
      IF(IBF.EQ.0) GOTO 10
#include "unpackio.h"
*     stop, if wrong unit
      IF(IOMD.NE.1.OR.IOST.EQ.2) THEN
         IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) LUN
         CALL BABEND('BEPRD')
      END IF
      IF(IOST.EQ.1) GOTO 15

*     initialize data set by reading first physical record header

   10 IW(1)=2
      IBF= NBANK('+BUF',LUN,20)
      IF(IBF.EQ.0) GOTO 201
      IOST=1
#include "packio.h"
      CALL WGARB(IW)
      IF (IW(IBF+2).EQ.0) THEN
         NBLK16=((IW(IWK)-IW(IGP))*NBITWD)/16
         NBLK16=MIN(NBLK16,32767)
      ELSE
         NBLK16=IW(IBF+2)
      ENDIF

*     set status word 1 (= physical block length in 16 bit words)
*     to largest possible number

      CALL EPSETW(LUN,1,NBLK16,IER)
      IF(IER.NE.0) GOTO 202

*     SET STATUS WORD 27 (BYTE SWAPPING CHECK)
      CALL EPSETW(LUN,27,1,IER)
      IF(IER.NE.0) GOTO 202

*     30 = GET NEXT PHYSICAL HEADER (UNPACKED AS 16 BIT/WORD)
*     NW = NR OF WORDS TRANSFERED INTO LHEAD
*     LHEAD = AREA PROVIDED TO STORE THE HEADER
*     IW(IBF+21) = BUFFER PROVIDED
      IW(IBF+ 3) = 0
      IW(IBF+ 4) = 0
      IW(IBF+ 5) = 0
C      in direct access mode, record length has to be known
      IF(IOAC.EQ.1) LHEAD(1)=IW(IBF+2)
      NRECR = 0
      NSEGR = 0
      CALL BBEPRD(LUN,IOAC,IW(IBF+4),NRECR,IW(IBF+5),NSEGR,
     +                30,NW,LHEAD,IW(IBF+21),IER)
      IF (IER.EQ.1 .OR. IER.EQ.3) GOTO 50
      IF(IER.NE.0) GOTO 202
      NBLK16=LHEAD(1)
      NBLK=(NBLK16*16-1)/NBITWD+1
      IW(1)=1
      IBF= NBANK('+BUF',LUN,20+NBLK)
      IF(IBF.EQ.0) GOTO 201

*     SET STATUS WORD 1 (= PHYSICAL BLOCK LENGTH IN 16 BIT WORDS)
*     TO ACTUAL PHYSICAL RECORD LENGTH

      CALL EPSETW(LUN,1,NBLK16,IER)
      IF(IER.NE.0) GOTO 202
      IOST=1
#include "packio.h"
      DO 14 L=6,20
   14 IW(IBF+L)=0

*     initialize new record

   15 NRECR=0
      LIC=0
      IC=0
      IFLG=0
      IF(LIST.EQ.' ') THEN
         IRD=MDROP(JW,'+REC',LUN)
      ELSE
         IRD=MLINK(JW,'+REC',LUN)
         IF(IRD.NE.0) GOTO 29
      END IF
      ISTART=0
      ICOUNT=0
      IF(LRPIN.NE.0) THEN
*        read record using record pointer
         NRECR=IABS(LRPIN)/1000
         NSEGR=MOD(IABS(LRPIN),1000)
      END IF
*     READ LOGICAL RECORD HEADER

   20 IF(IW(IBF+11).EQ.0) THEN
C
C        20 = GET HEADER OF NEXT LOGICAL RECORD
C             UNPACKED IN UNITS (16 OR 32 BIT WORDS)
C        NW = NR OF WORDS TRANSFERED INTI LHEAD
C        IW(IBF+11) = AREA PROVIDED TO STORE THE HEADER
C        IW(IBF+21) = BUFFER PROVIDED
C
         IF(NRECR.EQ.0) THEN
            NRECR=IW(IBF+4)
            NSEGR=IW(IBF+5)+1
         END IF
         CALL BBEPRD(LUN,IOAC,IW(IBF+4),NRECR,IW(IBF+5),NSEGR,
     +               20,NW,IW(IBF+11),IW(IBF+21),IER)
C        CALL EPREAD(LUN,20,NW,IW(IBF+11),IW(IBF+21),IER)
C        CALL EPGETW(LUN,11,IBLOCK,IDUMER)
         NRECR=0
         LRPIN=0
C        IF(IBLOCK.NE.IW(IBF+4)) THEN
C           IW(IBF+4)=IBLOCK
C           IW(IBF+5)=0
C        END IF
C        IW(IBF+5)=IW(IBF+5)+1
         IF(LRP.EQ.0) LRP=1000*IW(IBF+4)+IW(IBF+5)
*        E.O.F. OR E.O.TAPE
         IF(IER.EQ.1.OR.IER.EQ.3) GOTO 50
C        READ ERROR
         IF(IER.NE.0) GOTO 30
      END IF

*     CREATE +REC BANK
      IF(ISTART.EQ.0) THEN
         IRD=MBANK(JW,'+REC',LUN,0)
         IF(IRD.EQ.0) GOTO 201
         ISTART=JW(IGP)
      END IF

*     CHECK RECORD BEFORE READING

      IW11=IW(IBF+11)
      NBLK=(IW11*16-1)/NBITWD+1
      IC=IW(IBF+12)
      IW(IBF+11)=0
      IF(IFLG.NE.0) GOTO 40
      IF(IC.LT.0.OR.IC.GT.3) GOTO 40
      IF(LIC.GE.1.AND.IC.LE.1) GOTO 40
      IF(ISTART+ICOUNT+NBLK+4.GE.JW(IWK)) CALL WGARB(JW)
      IF(ISTART+ICOUNT+NBLK+4.GE.JW(IWK)) THEN
         IC=-1
         IFLG=3
         GOTO 40
      END IF
C
C     READ DATA
C
C
C        13 = GET DATA OF CURRENT LOGICAL RECORD
C             DATA UNPACKED AS 32 BIT BYTES/WORD
C        NW = NR OF WORDS TRANSFERED INTO AREA
C        IW(ISTART+ICOUNT) = AREA PROVIDED TO STORE DATA
C        IW(IBF+21) = BUFFER PROVIDED
C
      CALL BBEPRD(LUN,IOAC,NRECA,NRECR,NSEGA,NSEGR,
     +                13,NW,JW(ISTART+ICOUNT),IW(IBF+21),IER)
C     CALL EPREAD(LUN,13,NW,JW(ISTART+ICOUNT),IW(IBF+21),IER)
      IF(IER.EQ.1.OR.IER.EQ.3) GOTO 50
      IF(IER.NE.0) GOTO 30
      ICOUNT=ICOUNT+NW
      LIC=IC
      IF(LIC.NE.0.AND.LIC.NE.3) GOTO 20

*     RECORD COMPLETED, DEFINE +REC BANK WITH COMPLETE RECORD

      JW(1)=1
      IRD=MBANK(JW,'+REC',LUN,ICOUNT)
      IF(IRD.EQ.0) THEN
         IRD=MDROP(JW,'+REC',LUN)
         GOTO 201
      END IF

*     CHECK BANK LENGTH, CONVERT FORMAT AND
*     INSERT POINTER INTO POS. JW(IND-1)

      RW(IBF+8)=RW(IBF+8)+4.0E-6*FLOAT(ICOUNT)
      IW(IBF+6)=IW(IBF+6)+1
      IW(IBF+7)=MAX0(IW(IBF+7),ICOUNT)

*     CONVERT FIRST BANK

      CALL BBNIC(JW,LUN,1)
      JW2=JW(2)
      JW(2)=JW2
      IF(JW(2).NE.0) GOTO 40
C
C     STORE FMT INFORMATION IN WORK BANKS
C
      IF(JW(IRD+1).EQ.INTCHA('+FMT')) THEN
         IW(IBF+6)=IW(IBF+6)-1
         CALL BBNIC(JW,LUN,2)
         IF(JW(2).NE.0) GOTO 40
         IND=IRD
         JND=IND
         IND=IND+4
   27    IF(IND-JND.GT.ICOUNT) THEN
            IND=MDROP(JW,'+REC',LUN)
            GOTO 15
         END IF
         JW(IND+2)=1
         NAMI=NAMINC(JW(IND+1))

         ID=IDFMT+NAMI-NSYST
         IF(IW(ID).NE.0) THEN
*           format bank exists
            IF(IW(IW(ID)).NE.JW(IND)) THEN
               CALL WBANC(IW,IW(ID),JW(IND),*201)
            END IF
         ELSE
*           format bank does not exist, create work bank
            CALL WBANC(IW,IW(ID),JW(IND),*201)
            IW(IW(ID)-3)=INTCHA('+FMT')

         END IF
         DO 28 L=1,JW(IND)
   28    IW(IW(ID)+L)=JW(IND+L)
C        CALL BBFMT(NAMI,JW(IND+1),JW(IND))
         IND=IND+JW(IND)+4
         GOTO 27
      END IF

*     RETURN FOR BLANK LIST
      JW(3)=IRD+4
      IF(LIST.EQ.' ') GOTO 100

*     DEFINE LIST AND INSERT POINTER

   29 CALL BBNIC(JW,LUN,2)
      IF(JW(2).NE.0) GOTO 40
      JW(3)=IRD+4
      CALL BBINS(JW,LUN,LIST)
      JW(2)=0
      GOTO 100
C
C     READ ERROR
C
   30 IFLG=1
      IW(IBF+11)=0
      GOTO 20
C
C     FORMAL ERROR
C
   40 IF(IFLG.EQ.0) IFLG=2
      IF(IC.NE.0.AND.IC.NE.1) GOTO 20
      IW(IBF+11)=IW11
      IW(IBF+3)=IW(IBF+3)+1
      JW(2)=3+IFLG
      CALL BBRET(JW,JW(2),'BEPRD','    ')
      JW(IRC+JW(2))=JW(IRC+JW(2))+1
      GOTO 101
C
C     END OF FILE
C
   50 JW(2)=0
      IF(IER.EQ.3) JW(2)=7
      IF(JW(LUP).NE.0)
     1   WRITE(JW(LUP),1003) LUN
      GOTO 102
C
  100 CONTINUE
      CALL BENDSL(LUN,LRP,JW(JW(3)-3))
      JW(2)=LRP
      RETURN
  101 CALL BENDSL(LUN,0,0)
      RETURN 1
  102 CALL BENDSL(LUN,0,0)
      RETURN 2
  201 CALL BBSPC(IW,'BEPRD')
      GOTO 100
  202 IF(JW(LUP).GT.0) WRITE(JW(LUP),1002) IER
      CALL BABEND('BEPRD')
      GOTO 100
 1001 FORMAT('0BOS--BEPRD-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
 1002 FORMAT('0BOS--BEPRD-  EPIO ERROR',I3,' - STOP')
 1003 FORMAT('0BOS--BEPRD-  END-OF-FILE ON UNIT',I3)
      END
