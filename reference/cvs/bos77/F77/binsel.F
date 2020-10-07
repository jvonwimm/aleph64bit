      SUBROUTINE BINSEL(LUN,NRCPTR)
#include "boscom.h"
#include "param.h"
#include "bitconst.h"
      SAVE IV, IVMAX
      INTEGER NCLASS(30)
      LOGICAL START
      LOGICAL BSELEC
      DATA START/.TRUE./
*     internal routine to handle select input, to be called before new
*     input
*     910729 - modified by F.Ranjard
*              try to read next $PTS if there is one (2 extra words
*              at the end of $PTS bank which means IVMAX*4<IW(IND))
*     920630 - modified by F.Ranjard
*              add SAVE statment
      IF(NRE.EQ.0) THEN
*        default
         NRE=NRE+1
         NAMERE(NRE)=INTCHA('HEAD')
         NUMMRE(NRE)=0
         IRUNRE(NRE)=2
         IEVTRE(NRE)=3
      END IF
      IF(START) THEN
         START=.FALSE.
         IF(IW(LUP).EQ.0) GOTO 5
         WRITE(IW(LUP),1003) LUNDAT,LUNSEL
         IF(LUNDAT.EQ.0) THEN
            WRITE(IW(LUP),1001) ' No selection'
         ELSE
            IF(LUTDAT.EQ.0) WRITE(IW(LUP),1004) 'input',LUNDAT
            IF(LUTDAT.NE.0) WRITE(IW(LUP),1004) 'output',LUTDAT
            IF(LUNSEL.NE.0.AND.MASKR.EQ.0) THEN
               WRITE(IW(LUP),1001) 'Record selection all classes'
            END IF
            IF(LUNSEL.NE.0.AND.MASKR.NE.0) THEN
               LMASK=MASKR
               CALL BCLASS(MASK,NCLASS)
               WRITE(IW(LUP),1001) 'Record selection classes = ',NCLASS
            END IF
            WRITE(IW(LUP),1001)
     +      'Run and event number in banks with'
            WRITE(IW(LUP),1002)
     +     (NAMERE(I),NUMMRE(I),IRUNRE(I),IEVTRE(I),I=1,NRE)
         END IF
      END IF
    5 NRCPTR=0
      IF(LUN.NE.LUNDAT) GOTO 100
*     return record number for next record to be read or zero
      IF(LUNSEL.EQ.0) GOTO 100

      IND=NLINK('$PTS',LUNSEL)
*     is there a pointer inside a record-pointer bank
      IF(IND.NE.0) GOTO 20
*     read new bank
  7   IND=NDROP('$PTS',LUNSEL)
*     touch unit
      IBF= NLINK('+BUF',LUNSEL)
      IW(1)=0
      IF(IBF.EQ.0) THEN
         CALL BOSRD(IW,LUNSEL,'Z',*101,*102)
      ELSE
      IOAC=MOD(IW(IBF+1),8)
      IOMD=MOD(IW(IBF+1)/8,8)
      IOST=MOD(IW(IBF+1)/64,8)
*        test read
         IF(IOST.EQ. 2) CALL BABEND('WRONG UNIT IN BREAD')
         IF(IOMD.EQ. 0) THEN
            CALL BOSRD(IW,LUNSEL,'Z',*101,*102)
         ELSE IF(IOMD.EQ.1) THEN
            CALL BEPRD(IW,LUNSEL,'Z',*101,*102)
         ELSE IF(IOMD.EQ.2) THEN
            CALL BTERD(IW,LUNSEL,'Z',*101,*102)
         ELSE
            CALL BABEND('WRONG UNIT IN BREAD')
         END IF
      END IF
      IND=NLINK('$PTS',0)
      IF(IND.EQ.0) CALL BOSBK(IW)
      IF(IND.EQ.0) CALL BABEND('Wrong select file used')
      IND=NSWAP('$PTS',LUNSEL,'$PTS',0)
      IF(IND.EQ.0) CALL BABEND('Error in select file')
*     number of entries in this bank
      IVMAX=IW(IND)/4
      IV=0
   20 IF(LUNSE2.NE.0) THEN
         CALL BINSE2(KSRUN,KSEVT,IER)
         IF(IER.NE.0) THEN
           IND = NDROP('$PTS',LUNSEL)
           GOTO 102
         ENDIF
      ENDIF
   10 IF(IV.GE.IVMAX) THEN
        IF (IVMAX*4 .LT. IW(IND)) GOTO 7
        IND=NDROP('$PTS',LUNSEL)
        IF(LUNSE2.NE.0) IND=NDROP('$PTS',LUNSE2)
        GOTO 102
      ENDIF
      IV=IV+1
*     next run and event number from selct file
      INRUN=IW(IND+4*IV-1)
      INEVT=IW(IND+4*IV)
      LMASK=IW(IND+4*IV-2)
C
      IF(LUNSE2.NE.0) THEN
        IF(INRUN.NE.KSRUN .OR. INEVT.NE.KSEVT) GOTO 10
        GOTO 90
      ENDIF
C
*     test MASK (reading mask)
C
      IF(.NOT.BSELEC(LMASK,MASKR))GOTO 10
*     test special banks for event/run selection
      CALL BASEVT(IW,INRUN,INEVT,IRET)
      IF(IRET.EQ.0) GOTO 10
      IF(IRET.EQ.1) GOTO 90
*        not within list of runs to be ignored
      IF(IRET.GE.7) THEN
        IND=NDROP('$PTS',LUNSEL)
        GOTO 102
      END IF
   90 NRCPTR=IABS(IW(IND+4*IV-3))
  100 IW(1)=NRCPTR
      RETURN
  101 CALL BABEND('Read error in select file')
*     end-of-data flag
  102 NRCPTR=-1
      GOTO 100
 1001 FORMAT(1X,A,3(5I1,1X,5I1,2X))
 1002 FORMAT(10X,'Name = ',A4,', nr =',I3,' in words',I3,',',I3)
 1003 FORMAT('0Input data unit   =',I3,'      Input select unit =',I3)
 1004 FORMAT('0Any select output refers to  ',A,' data  unit =',I3)
      END
