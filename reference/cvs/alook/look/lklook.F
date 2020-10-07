      SUBROUTINE LKLOOK
C -----------------------------------------------------------
C! give a command , decode it, call the right subroutine
C -----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkevt.h"
#include "lkfil.h"
#include "lklast.h"
      CHARACTER*1 ERLIST
      CHARACTER*80 COMAND,PROMPT*6,KEY*10,PRECOM(20),DCOM,CH*1,INISTR,
     +             KW(21)*10,BNAM*4,C*1
      CHARACTER CHSHEL*80
#if defined(ALEPH_DEC) && ! defined(UNIX)
      INTEGER SMG$CREATE_VIRTUAL_KEYBOARD
      INTEGER SMG$READ_STRING, LIB$SPAWN
#endif
      INTEGER NUMCOM,ICLR(30),ICLW(30)
      INTEGER*4 KEYID
      LOGICAL LKOPTI
      CHARACTER*4 NLIST
      EXTERNAL LKOPTI,NLIST
C ------------------------------------------------------------
      KW(1)='READ'
      KW(2)='LOOK'
      KW(3)='FORMATS'
      KW(4)='BANKS'
      KW(5)='STAT'
      KW(6)='WRITE'
      KW(7)='SLOW'
      KW(8)='SCAN'
      KW(9)='HELP'
      KW(10)='EXIT'
      KW(11)='OPEN'
      KW(12)='COMMAND'
      KW(13)='+'
      KW(14)='-'
      KW(15)='RECALL'
      KW(16)='DECOMPRESS'
      KW(17)='DROP'
      KW(18)='REWIND'
      KW(19)='CLASS'
      KW(20)='QUIT'
      KW(21)='XEDIT'
      NUMCOM=21
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      I=SMG$CREATE_VIRTUAL_KEYBOARD(KEYID,'SYS$COMMAND',,,%REF(20))
#endif
      PROMPT='LOOK> '
      INISTR=' '
      NCOM=0
      WRITE(LOUT,*)
   10 CONTINUE
      IW(6)=6
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      IF (INISTR.NE.' ') THEN
          LEN=LENOCC(INISTR)
          STAT=SMG$READ_STRING(KEYID,COMAND,PROMPT,,,,,,,,INISTR(1:LEN))
      ELSE
          STAT=SMG$READ_STRING(KEYID,COMAND,PROMPT)
      ENDIF
#else
      IF (INISTR.NE.' ') THEN
          LEN=LNBLNK(INISTR)
          CALL LKREAD0(PROMPT,INISTR(1:LEN),COMAND)
      ELSE
          CALL LKREAD0(PROMPT,' ',COMAND)
      ENDIF
#endif
      INISTR=' '
C
      IBL = INDEX (COMAND,' ')
      IF (IBL.EQ.0) IBL = LNBLNK (COMAND)
C     convert keyword to upper case, keep filename as it was given
      CALL CLTOU (COMAND(1:IBL))
      DCOM=COMAND
C
      CALL LKKEY(COMAND,KW,NUMCOM,KEY,*998)
      IF (KEY.NE.'RECALL') CALL LKSTCOM(PRECOM,DCOM,NCOM)
C
C     the command parameter has not been converted to upper case
      DCOM = COMAND
#ifndef UNIX
C     on VMS the command parameter is converted to upper case
      CALL CLTOU (DCOM)
#endif
C     Comand is upper case
      CALL CLTOU (COMAND)
      IF (KEY.EQ.'LOOK') THEN
          IF (LKOPTI(COMAND,'DAF')) THEN
              CALL LKGTNAM(COMAND,BNAM,*1000)
              CALL LKGTINT(COMAND,2,' ',I,C)
              IF (C.EQ.'A') GOTO 1000
              CALL LKRDAF(COMAND)
              CALL LKNICE(COMAND)
          ELSEIF (LKOPTI(COMAND,'DDL')) THEN
              IF (LKOPTI(COMAND,'PRINT')) THEN
                  CALL LKPRDDL(COMAND)
              ELSE
                  CALL LKDDL(COMAND)
              ENDIF
          ELSEIF (LKOPTI(COMAND,'BOS')) THEN
              IF (LKOPTI(COMAND,'PRINT')) THEN
                  CALL LKPRIN(COMAND)
              ELSE
                  CALL LKBANK(COMAND)
              ENDIF
          ELSE
              IF (LKOPTI(COMAND,'PRINT')) THEN
                  CALL LKPRBNK(COMAND)
              ELSE
                  CALL LKNICE(COMAND)
              ENDIF
          ENDIF
      ELSEIF (KEY.EQ.'DROP') THEN
          CALL LKGTNAM(COMAND,BNAM,*1000)
          CALL LKGTINT(COMAND,2,' ',I,C)
          IF (C.EQ.'A') GOTO 1000
          IF (C.EQ.' ') THEN
             CALL BDROP (IW,BNAM)
          ELSE
             IDRP = NDROP (BNAM,I)
          ENDIF
      ELSEIF (KEY.EQ.'SLOW') THEN
          JSLOW = NBANK ('SLOW',0,0)
          IF (JSLOW.EQ.0) THEN
              WRITE(LOUT,*) ' no space to create SLOW bank'
          ENDIF
      ELSEIF (KEY.EQ.'DECOMPRESS') THEN
          CALL DMPLIS('E',ERLIST,IER)
          IF (IER.NE.0) THEN
              WRITE(LOUT,*) ' Decompression Error ',IER
          ENDIF
          CALL DMPLIS('C',ERLIST,IER)
          IF (IER.NE.0) THEN
              WRITE(LOUT,*) ' Decompression Error ',IER
          ENDIF
      ELSEIF (KEY.EQ.'+') THEN
          CALL LKPLUS(COMAND)
      ELSEIF (KEY.EQ.'-') THEN
          CALL LKMINS(COMAND)
      ELSEIF (KEY.EQ.'READ') THEN
          IF (LKOPTI(COMAND,'CARD')) THEN
C             filename must kept as it was given
C             use DCOM which preserves the case
C             COMAND is in upper case
              CALL LKRCAR(DCOM)
          ELSEIF (LKOPTI(COMAND,'DAF')) THEN
              CALL LKRDAF(COMAND)
          ELSEIF (LKOPTI(COMAND,'RUN')) THEN
              CALL LKRREC(COMAND)
          ELSEIF (LKOPTI(COMAND,'ULIST')) THEN
              CALL LKGTUNP(COMAND)
              CALL LKRREC(COMAND)
          ELSEIF (LKOPTI(COMAND,'NODEC')) THEN
              CALL LKSTUNP ('NODE')
              CALL LKRREC(COMAND)
          ELSE
              CALL LKRREC(COMAND)
          ENDIF
      ELSEIF (KEY.EQ.'FORMATS') THEN
          CALL BOSFM
      ELSEIF (KEY.EQ.'BANKS') THEN
          IF (LKOPTI(COMAND,'DAF')) THEN
              IF (DIRFIL) THEN
                  LDBAS = JUNIDB(0)
                  CALL BDAPR(LDBAS,'    ')
              ELSE
                  WRITE(LOUT,*) '[DAF file not opened]'
              ENDIF
          ELSEIF (LKOPTI(COMAND,'ALL')) THEN
              CALL BOSBK(IW)
          ELSEIF (LKOPTI(COMAND,'LEN')) THEN
              IF (NLIST(IW,1,'C').NE.' ') CALL LKBLEN('C')
              IF (NLIST(IW,1,'E').NE.' ') CALL LKBLEN('E')
              IF (NLIST(IW,1,'S').NE.' ') CALL LKBLEN('S')
              IF (NLIST(IW,1,'T').NE.' ') CALL LKBLEN('T')
          ELSE
              IF (NLIST(IW,1,'C').NE.' ') CALL AUBLIS('C')
              IF (NLIST(IW,1,'E').NE.' ') CALL AUBLIS('E')
              IF (NLIST(IW,1,'S').NE.' ') CALL AUBLIS('S')
              IF (NLIST(IW,1,'T').NE.' ') CALL AUBLIS('T')
          ENDIF
      ELSEIF (KEY.EQ.'OPEN') THEN
          COMAND = DCOM
          IF (LKOPTI(COMAND,'DAF')) THEN
              CALL LKOPDAF(COMAND)
          ELSE
              CALL LKNWSEQ(COMAND)
          ENDIF
      ELSEIF (KEY.EQ.'COMMAND') THEN
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
          IF (COMAND.NE.' ') THEN
              I=LIB$SPAWN(COMAND)
          ELSE
              WRITE(LOUT,*) '[Type "LOGOUT" to return from process]'
              I=LIB$SPAWN()
          ENDIF
#else
#if defined(IBM)
          IF (COMAND.NE.' ') THEN
              CALL VMCMS(COMAND,IRC)
              WRITE(LOUT,*) ' RC = ',IRC
          ELSE
              WRITE(LOUT,*) '[Type "RETURN" to return from process]'
              CALL VMCMS('SUBSET',IRC)
              WRITE(LOUT,*) ' RC = ',IRC
          ENDIF
#else
#if defined(UNIX)
          IF (COMAND.NE.' ') THEN
              CALL SYSTEM (COMAND)
          ELSE
              WRITE(LOUT,*) '[Type "exit" to return from process]'
              CALL GETENV ('SHELL',CHSHEL)
              CALL SYSTEM (CHSHEL)
          ENDIF
#endif
#endif
#endif
      ELSEIF (KEY.EQ.'RECALL') THEN
          IF (LKOPTI(COMAND,'ALL')) THEN
              CALL LKPRCOM(PRECOM,NCOM)
          ELSE
              CALL LKRECAL(COMAND,PRECOM,NCOM,INISTR)
          ENDIF
      ELSEIF (KEY.EQ.'STAT') THEN
          CALL LKSTAT
      ELSEIF (KEY.EQ.'WRITE') THEN
          IF (LKOPTI(COMAND,'OUTPUT')) THEN
              CALL LKWRLIS(COMAND)
          ELSE
              CALL LKWRCAR(COMAND)
          ENDIF
      ELSEIF (KEY.EQ.'HELP') THEN
          CALL LKHELP
      ELSEIF (KEY.EQ.'EXIT' .OR. KEY.EQ.'QUIT') THEN
          DO I=1,3
            IF (FLGFIL(I)) CALL BWRITE (IW,LFILE(I),'0')
          ENDDO
          CALL ACLOSE(0,IER)
          CALL DROPDK
          GOTO 999
      ELSE IF(KEY.EQ.'SCAN') THEN
          CALL LKSCAN
      ELSE IF(KEY.EQ.'REWIND') THEN
          CALL LKRWND (COMAND)
      ELSE IF(KEY.EQ.'CLASS') THEN
          CALL ABGTRCL(MASKR)
          CALL ABGTWCL(MASKW)
          IRD = 0
          IWR = 0
          DO 1 IB=1,30
             ICLR(IB)=0
             IF (BTEST(MASKR,IB-1)) THEN
                IRD=IRD+1
                ICLR(IRD)=IB
             ENDIF
             ICLW(IB)=0
             IF (BTEST(MASKW,IB-1)) THEN
                IWR=IWR+1
                ICLW(IWR)=IB
             ENDIF
 1        CONTINUE
          IF (IRD.EQ.0) THEN
             WRITE(LOUT,*) 'NO read class_bit set'
          ELSE
             WRITE(LOUT,'(1X,A,30I3)') 'read class_bits set ',
     &                               (ICLR(IB),IB=1,IRD)
          ENDIF
          IF (IWR.EQ.0) THEN
             WRITE(LOUT,*) 'NO write class bit set'
          ELSE
             WRITE(LOUT,'(1X,A,30I3)') 'write class_bits set ',
     &                               (ICLW(IB),IB=1,IWR)
          ENDIF
      ELSE IF(KEY.EQ.'XEDIT') THEN  
#if !defined(ALEPH_DEC)
          WRITE(LOUT,*) 'this option works on ALPHA machines only'
#else          
          IF (LKOPTI(COMAND,'DAF')) THEN
              CALL LKGTNAM(COMAND,BNAM,*1000)
              CALL LKGTINT(COMAND,2,' ',I,C)
              IF (C.EQ.'A') GOTO 1000
              CALL LKRDAF(COMAND)
          ENDIF
          CALL LKEDITB(COMAND)
#endif
      ELSE
          WRITE(LOUT,*)
     +    'There''s something wrong! I shouldn''t know this command!'
      ENDIF
      GOTO 10
 1000 WRITE(LOUT,*) '[Please use 4-letter bank name and optional bank',
     &            'number]'
      GOTO 10
  998 CONTINUE
      IF (COMAND.NE.' ') CALL LKSTCOM(PRECOM,DCOM,NCOM)
      GOTO 10
  999 CONTINUE
      RETURN
      END
#endif
