      SUBROUTINE QMSTCLR
C ----------------------------------------------------------------------
C! Stage clear all tapes/files from 'FILI' cards if SCLR card present
C  J. Boucrot        14-Feb-2000
C  Called from QMTERM
C  Calls ALSTIN,BKINCA                          from ALEPHLIB
C  Input bank needed :  'FILI'  ( from BOS data cards )
C ----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#if defined(UNIX)
      PARAMETER ( MAXFI = 999 , LUNQR = 98 , NMUSD = 2 )
      INTEGER BKINCA
      CHARACTER TAPE*6,CARTF*200,FILEX*5
      character comand*100,comqry*100,fiqry*50,qrlin*100
C ----------------------------------------------------------------------
C Execute only if the is a "SCLR" data card:
      IF (IW(NAMIND('SCLR')).LE.0) GO TO 999
      WRITE (IW(6),1200)
C
C LOOP on all 'FILI' data cards, if any
C
      JFILI=NAMIND('FILI')+1
 100  JFILI=IW(JFILI-1)
      IF (JFILI.LE.0) GO TO 800
      NDAFI=IW(JFILI)
      IF (NDAFI.LE.0) GO TO 100
      TAPE=' '
      FILEX=' '
      IFILE=0
C Translate into character string ans get tape and file number:
      CALL ALSTIN(IW(JFILI+1),NDAFI,CARTF)
      CALL QANFILI(CARTF,TAPE,FILEX)
C Tape and file found:
C
      IFILE=BKINCA(FILEX)
      IF (TAPE.EQ.' '.OR.FILEX.EQ.' '.OR.IFILE.gt.MAXFI) THEN
         WRITE (IW(6),1234) TAPE,IFILE
         GO TO 100
      ENDIF
C
C Determine the number of times this file was used in the stageing pool:
C The parameter -u in the stageqry command ensures that only tape/files
C staged by the owner of the present job are checked
C
      fiqry=' '
      fiqry='stageqry.output'
      lqry=lnblnk(fiqry)
      call system(' rm -f '//fiqry(1:lqry))
      comqry=' '
      comqry=' stageqry -V '//tape//' -u -q '//filex(1:lnblnk(filex))//
     +       ' > '//fiqry(1:lqry)
      call system(comqry(1:lnblnk(comqry)))
      call aopen(lunqr,fiqry,'CARDS','DISK',IER)
      if (ier.ne.0) go to 100
      rewind (lunqr)
      ilin=0
      iused=0
 400  read (lunqr , '(A)' , err=500, end =500  ) qrlin
      ilin=ilin+1
      if (ilin.eq.1) go to 400
      read (qrlin,1300) iused
 500  continue
      close (lunqr)
      call system(' rm -f '//fiqry(1:lqry))
C This tape was staged by another user : it will not be stage cleared:
      if (ilin.le.1) then 
         write (iw(6),1302) tape,filex
         go to 100
      endif
C
C If this tape/file is not staged,
C or has been used more than nmusd times,
C don't erase it from the stage pool:
C
      if (iused.gt.nmusd) then
         write (iw(6),1301) tape,filex,iused
         go to 100
      endif
C
C Otherwise ...
C Stageclear this tape/file :
C
      comand=' '
      comand=' stageclr -V '//tape//' -q '//filex(1:lnblnk(filex))
      write (iw(6),1201) comand(1:lnblnk(comand))
      call system(comand(1:lnblnk(comand)))
      go to 100
C
C All finished:
C
 800  continue
#endif
 999  RETURN
C
 1200 FORMAT (/' _QMSTCLR_ Data card SCLR present:',
     +         ' try to stage clear all files from FILI cards'/)
 1201 FORMAT (' _QMSTCLR_ Command executed: ',2x,(A))
 1234 FORMAT (' _QMSTCLR_ Error: Unexpected tape, file =',2x,a6,i8)
 1300 FORMAT (50X,i4)
 1301 FORMAT (' _QMSTCLR_ tape/file =',2x,a6,2x,a3,' no stageclr ',
     +        'done : nb of uses =',i5)
 1302 FORMAT (' _QMSTCLR_ tape/file =',2x,a6,2x,a3,' no stageclr ',
     +        'done : owned by somebody else')
      END
#endif
