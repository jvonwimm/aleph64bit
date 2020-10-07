      SUBROUTINE QMSTGIN
C ----------------------------------------------------------------------
C! Stage all tapes/files from 'FILI' cards if STGI card present
C  J. Boucrot        14-Mar-2000
C  Called from QMINIT
C  Calls ALSTIN,BKINCA                          from ALEPHLIB
C  Input bank needed :  'FILI'  ( from BOS data cards )
C ----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#if defined(UNIX)
      PARAMETER ( MAXFI = 999 , NTSTM = 50 )
      INTEGER BKINCA,IFLMI(NTSTM),IFLMX(NTSTM)
      character*6 tapst(ntstm)
      character*3 filmi(ntstm),filmx(ntstm)
      CHARACTER TAPE*6,CARTF*200,FILEX*5,FMI*3,FMX*3,COMAND*100
C ----------------------------------------------------------------------
C Execute only if the is a "STGI" data card:
      IF (IW(NAMIND('STGI')).LE.0) GO TO 999
      WRITE (IW(6),1200)
      NTAPS=0
      DO 1 IK=1,NTSTM
         IFLMI(IK)=10000
         IFLMX(IK)=-1
         TAPST(IK)=' '
         FILMI(IK)=' '
         FILMX(IK)=' '
 1    CONTINUE
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
         WRITE (IW(6),1300) TAPE,IFILE
         GO TO 100
      ENDIF
C Store it :
      iok=0
      do 20 il=1,ntaps
         if (tapst(il).ne.tape) go to 20
         iok=il
         go to 30
 20   continue
 30   if (iok.eq.0) then
         ntaps=ntaps+1
         if (ntaps.gt.ntstm) then
            write (iw(6),1301) ntaps
         endif
         tapst(ntaps)=tape
         iok=ntaps
      endif
      ilmi=min0(ifile,iflmi(iok))
      ilmx=max0(ifile,iflmx(iok))
      if (ilmi.ne.iflmi(iok)) then
         iflmi(iok)=ilmi
         filmi(iok)=filex(1:lnblnk(filex))
      endif 
      if (ilmx.ne.iflmx(iok)) then
         iflmx(iok)=ilmx
         filmx(iok)=filex(1:lnblnk(filex))
      endif
C
      go to 100
C If no tapes found, quit :
 800  if (ntaps.le.0) go to 999
C
C Otherwise ...
C Stage in turn all tape/files :
C
      do 850 k=1,ntaps
         fmi=filmi(k)
         lfmi=lnblnk(fmi)
         fmx=filmx(k)
         lfmx=lnblnk(fmx)
         comand=' '
         comand=' alstagein -V '//tapst(k)//' -q '//
     +            fmi(1:lfmi)//'-'//fmx(1:lfmx)
         write (iw(6),1201) comand(1:lnblnk(comand))
         call system(comand(1:lnblnk(comand)))
 850  continue 
C
C All finished:
C
#endif
 999  RETURN
C
 1200 FORMAT (/' _QMSTGIN_ Data card STGI present:',
     +         '  begin stageing of all files from FILI cards'/)
 1201 FORMAT (' _QMSTGIN_ Command executed: ',2x,(A))
C
 1300 FORMAT (' _QMSTGIN_ Error: Unexpected tape, file =',2x,a6,i8)
 1301 FORMAT (' _QMSTGIN_ Error: too many tapes =',i5,' : no stage')
      END
#endif
