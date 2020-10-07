      SUBROUTINE BOSRD(JW,LUN,LISTR,*,*)
*
*     FORT read from unit LUN
*     at entry: JW(1) = record pointer or zero
*
*     at return: JW(2) = record pointer
*
*
      INTEGER JW(*)
#include "boscom.h"
#include "param.h"
#include "mparam.h"
      REAL RW(1000)
      EQUIVALENCE (IW(1),RW(1))
      CHARACTER*(*) LISTR
*     ...
      IF(JW(1).LT.0) THEN
*        end-of-file
         JW(1)=0
         JW(2)=0
         GOTO 102
      END IF

      LRPIN=JW(1)
      LRP=0
      JW(3)=0
      NBLK=0

*     buffer bank for unit LUN

      IBF= NLINK('+BUF',LUN)
      IF(IBF.EQ.0) GOTO 10
#include "unpackio.h"

*     stop, if wrong unit
      IF(IOMD.NE.0.OR.IOST.EQ.2) THEN
*        if not 'FORT' or if 'WRITE'
         IF(JW(LUP).GT.0) WRITE(JW(LUP),1001) LUN
         CALL BABEND('BOSRD')
      END IF

      IF(IOST.EQ.1) GOTO 15
*     if read mode already ...
      NBLK=IW(IBF+2)

*     move (or create) 11-word bank to end of array
   10 IW(1)=2
      IBF= NBANK('+BUF',LUN,11)
      IF(IBF.EQ.0) GOTO 201
      DO 12 M=1,11
   12 IW(IBF+M)=0
      IOST=1
#include "packio.h"
      IW(IBF+2)=NBLK

*     initialize new record
   15 NRECR=0
      LIC =0
      IC  =0
      IFLG=0
      NFRS=0
*     treat +REC bank
      IF(LISTR.EQ.' ') THEN
*        drop record bank for preliminary read
         IRD=MDROP(JW,'+REC',LUN)
      ELSE
*        otherwise accept record bank, if present
         IRD=MLINK(JW,'+REC',LUN)
         IF(IRD.NE.0) GOTO 29
      END IF
      IPT=IW(IBF+10)
      ISTART=0
      ICOUNT=0

*

      IF(LRPIN.NE.0) THEN
*        read record with rec pointer LRPIN
         NRECR=IABS(LRPIN)/1000
         IF(NRECR.NE.IW(IBF+4)) THEN
            IPT=0
            IW(IBF+5)=0
         END IF
      END IF

*     if buffer empty  read next record

   20 IF(IPT.EQ.0) THEN
*        organize buffer
         IF(IW(IBF).EQ.11) THEN
*           move to end, if short
            IW(1)=2
            IBF= NBANK('+BUF',LUN,11)
            IF(IBF.EQ.0) GOTO 201
            CALL WGARB(IW)
            NDIM=IW(IWK)-IW(IGP)
         ELSE
*           standard buffer size
            NDIM=IW(IBF)-10
         END IF
*        read record
C        CALL BBFRD(LUN,IW(IBF+11),IW(IBF+12),NDIM,*30,*50)
*        read next data record
         IF(NRECR.EQ.0) NRECR=IW(IBF+4)+1
         CALL BBFRDS(LUN,IOAC,IW(IBF+4),NRECR,
     +                   IW(IBF+11),IW(IBF+12),NDIM,*30,*50)
C        IBFR=NPRNT('+BUF',LUN)
         NRECR=0
         IW(IBF+5)=0
         RW(IBF+8)=RW(IBF+8)+1.0E-6*FLOAT(NBYTWD*(IW(IBF+11)+1))

         IF(IW(IBF).EQ.11) THEN
*           short buffer bank
            IF(IW(IBF+12).EQ.INTCHA('HIDD')) THEN
*              make buffer long for packed ('HIDD') records
               NBLK=IW(IBF+11)+7
               IF(IW(IBF+2).EQ.0) IW(IBF+2)=NBLK
               IW(1)=1
               IBF= NBANK('+BUF',LUN,NBLK+10)
               IF(IBF.EQ.0) GOTO 201
C              IDUM=NPRNT('+BUF',LUN)
            ELSE
*              copy to +REC region for unpacked (old) records
               ISTART=JW(IGP)+4
               CALL UCOPY2(IW(IBF+12),JW(ISTART),IW(IBF+11))
               ICOUNT=IW(IBF+11)
               JW(1)=1
               IRD=MBANK(JW,'+REC',LUN,ICOUNT)
               IF(IRD.EQ.0) GOTO 201
               GOTO 25
            END IF
         ELSE
*           long buffer bank
            IF(IW(IBF+12).NE.INTCHA('HIDD')) GOTO 201
*           error - HIDD bank expected, but not present
         END IF
         IPT=15
      END IF

*     pointer position
      IF(LRPIN.NE.0) THEN
*        find segment in record, if rec. pointer defined
         NSEGM=MOD(IABS(LRPIN),1000)-1
         LRPIN=-IABS(LRPIN)
   22    IF(NSEGM.GT.IW(IBF+5)) THEN
            NW=IW(IBF+IPT)-1
            IPT=IPT+NW+5
            IW(IBF+5)=IW(IBF+5)+1
            IF(IPT-15.GE.IW(IBF+11)) GOTO 40
            GOTO 22
         END IF
*        now segment is positioned
         LRPIN=0
         NRECR=0
      END IF

*     create +REC bank
      IF(ISTART.EQ.0) THEN
         IRD=MBANK(JW,'+REC',LUN,0)
         IF(IRD.EQ.0) GOTO 201
         ISTART=JW(IGP)
      END IF

*     record statistic, actual record pointer

      IW(IBF+5)=IW(IBF+5)+1
      IF(LRP.EQ.0) LRP=1000*IW(IBF+4)+IW(IBF+5)
      IC=IW(IBF+IPT+1)
      IF(NFRS.EQ.0) NFRS=IW(IBF+IPT+2)
      NW=IW(IBF+IPT)-1

*     check record  before copy to +REC bank

      IF(IFLG.NE.0)            GOTO 40
      IF(IC.LT.0.OR.IC.GT.3)   GOTO 40
      IF(LIC.GE.1.AND.IC.LE.1) GOTO 40
*     garbage collection in work bank area, if insufficient space
      IF(ISTART+NW+4.GE.JW(IWK)) CALL WGARB(JW)
      IF(ISTART+NW+4.GE.JW(IWK)) THEN
         IC=-1
         IF(NFRS.EQ.INTCHA('+FMT')) THEN
*           error stop, if during format data
            IF(JW(LUP).GT.0) WRITE(JW(LUP),1003)
            CALL BABEND('BOSRD')
         END IF
         IFLG=3
         GOTO 40
      END IF

*     copy (part of) record to gap

      CALL UCOPY(IW(IBF+IPT+2),JW(ISTART),NW)
      ISTART=ISTART+NW
      ICOUNT=ICOUNT+NW
      IPT=IPT+NW+5
      IF(IPT-15.GT.IW(IBF+11)) GOTO 40
      IF(IPT-15.EQ.IW(IBF+11)) IPT=0
      LIC=IC
      IF(LIC.NE.0.AND.LIC.NE.3) GOTO 20

*     record completed, define +REC bank with complete record

      IW(IBF+10)=IPT
      JW(1)=1
      IRD=MBANK(JW,'+REC',LUN,ICOUNT)
      IF(IRD.EQ.0) THEN
         IRD=MDROP(JW,'+REC',LUN)
         GOTO 201
      END IF
C     IRD=MPRNT(JW,'+REC',LUN)

*     store fmt information in work banks

   25 IF(NFRS.EQ.INTCHA('+FMT')) THEN
         IND=IRD
         JND=IND
         IND=IND+4
   27    IF(IND-JND.GT.ICOUNT) THEN
            IRD=MDROP(JW,'+REC',LUN)
*           finished - get next record
            GOTO 15
         END IF
         JW(IND+2)=1
         NAMI=NAMINC(JW(IND+1))
         ID=IDFMT+NAMI-NSYST

         IF(IW(ID).NE.0) THEN
*           format bank exists
            IF(IW(IW(ID)).NE.JW(IND)) THEN
*              adjust length
               CALL WBANC(IW,IW(ID),JW(IND),*201)
            END IF
         ELSE
*           format bank does not exist, create work bank
            CALL WBANC(IW,IW(ID),JW(IND),*201)
            IW(IW(ID)-3)=INTCHA('+FMT')
         END IF

         CALL UCOPY(JW(IND+1),IW(IW(ID)+1),JW(IND))
         IND=IND+JW(IND)+4
         GOTO 27
      END IF

*     make buffer bank short for next record
      IF(IPT.EQ.0) THEN
         IBF= NBANK('+BUF',LUN,11)
         IF(IBF.EQ.0) GOTO 201
      END IF
      JW(2)=0
      IW(IBF+6)=IW(IBF+6)+1
      IW(IBF+7)=MAX0(IW(IBF+7),ICOUNT)

*     return for blank list
      JW(3)=IRD+4

      IF(LISTR.EQ.' ') GOTO 100

*     check internal structure and determine name indices

   29 CALL BBNIN(JW,LUN)
      IF(JW(2).NE.0) GOTO 40
      JW(3)=IRD+4

*     define list and insert pointers

      CALL BBINS(JW,LUN,LISTR)
      GOTO 100

*     read error (in read statement)

   30 IFLG=1
      IPT=0
      GOTO 20

*     formal error in record
   40 CONTINUE
C     CALL BPRNT(IW,'+BUF')
      IF(IFLG.EQ.0) IFLG=2
      IF(IC.EQ.0.OR.IC.EQ.1) THEN
         JW(2)=3+IFLG
         IW(IBF+10)=IPT
         IW(IBF+ 3)=IW(IBF+3)+1
         JW(IRC+JW(2))=JW(IRC+JW(2))+1
         CALL BBRET(JW,JW(2),'BOSRD','    ')
         GOTO 101
      END IF
      IPT=IPT+IW(IBF+IPT)+4
      IF(IPT-10.GT.IW(IBF+11)) IPT=0
      GOTO 20

*     end of file

   50 JW(2)=0
      IW(IBF+11)=0
      IBF=NBANK('+BUF',LUN,11)
      IF(JW(LUP).NE.0)
     1   WRITE(JW(LUP),1002) LUN
      GOTO 102

  100 CALL BENDSL(LUN,LRP,JW(JW(3)-3))
      JW(2)=LRP
      RETURN
  101 CALL BENDSL(LUN,0,0)
      RETURN 1
  102 CALL BENDSL(LUN,0,0)
      RETURN 2
  201 CALL BBSPC(IW,'BOSRD')
      GOTO 100
 1001 FORMAT('0BOS--BOSRD-  WRONG UNIT - STOP',
     1       14X,'UNIT =',I3)
 1002 FORMAT('0BOS--BOSRD-  END-OF-FILE ON UNIT',I3)
 1003 FORMAT('0BOS--BOSRD-  INSUFFICIENT SPACE TO STORE FMT DATA -STOP')
      END
