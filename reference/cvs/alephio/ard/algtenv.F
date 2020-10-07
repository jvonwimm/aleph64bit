      INTEGER FUNCTION ALGTENV (ANAME,FNAME)
C --------------------------------------------------------------------
CKEY ALREAD FILE ENVIRONMENT
C! return the true filename from the ALEPH filename
C - F.Ranjard - 920513
C - Input   : ANAME    / A = ALEPH filename or any filename
C
C - Output  : FNAME    / A = true filename which could be used in OPEN
C             ALGTENV / I = return code
C                            =1 FNAME has been found
C                            =0 FNAME has not been found (FNAME =' ')
C --------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      CHARACTER*(*) ANAME, FNAME
      SAVE          ADATA, AEDIR, SYSTEM, LDATA, LEDIR
      CHARACTER*60  ADATA, AEDIR
      CHARACTER     SUBR*8, SYSTEM*4, FKEEP*30, FDIR*30
      PARAMETER (LCRNA = 33)
      PARAMETER (LENVIR=11)
      CHARACTER*12 AENVIR(LENVIR)
      DATA AENVIR / 'ADBSCONS    ','ADBSTEST    ','BANKALFMT   ',
     &              'JULIACARDS  ','ALPHACARDS  ','LOOKCARDS   ',
     &              'GALEPHCARDS ','RUNCARTSLIST','ADBS8990    ',
     &              'DBASBANK    ','BEAMPOSITION'/
      DATA IFI /0/
C ---------------------------------------------------------------------
      SUBR = 'ALGTENV'
      LAN  = LNBLNK(ANAME)
      IST  = 0
      FNAME = ANAME
C
#if defined(UNIX)
C --------------------------  UNIX code ---------------------------
      IF (IFI.EQ.0) THEN
         IFI = 1
         SYSTEM = 'UNIX'
         CALL GETENVF ('ALEDIR',AEDIR)
         LEDIR = LNBLNK(AEDIR)
         IF (LEDIR.GT.0) THEN
            AEDIR = AEDIR(1:LEDIR)//'/'
            LEDIR = LEDIR+1
         ENDIF
         CALL GETENVF ('ALDATA',ADATA)
         LDATA = LNBLNK(ADATA)
         IF (LDATA.GT.0) THEN
            ADATA = ADATA(1:LDATA)//'/'
            LDATA = LDATA+1
         ENDIF
      ENDIF
C - On UNIX machine try to get the filename from the environment
      IF (INDEX(ANAME,'/').EQ.0 ) THEN
         IF (INDEX(ANAME,'.') .EQ. 0) THEN
            CALL  GETENVF (ANAME,FNAME)
            IF (FNAME.EQ. '  ') THEN
               IST =0
               FNAME = ANAME
            ELSE
               IST = 1
            ENDIF
         ELSE
            IF (ANAME(1:1).EQ.'A' .OR. ANAME(1:1).EQ.'Y') THEN
               IF (INDEX(ANAME,'EDIR') .GT. 0) THEN
                  FNAME = AEDIR(1:LEDIR)//ANAME(1:LAN)
                  IST = 1
               ELSEIF (INDEX(ANAME,'EPIO').GT.0 .OR.
     &                 INDEX(ANAME,'NAT') .GT. 0) THEN
                  FNAME = ADATA(1:LDATA)//ANAME(1:LAN)
               ENDIF
               IST = 1
               CALL CUTOL (FNAME(1:LNBLNK(FNAME)))
            ENDIF
         ENDIF
      ENDIF
#include "prenvir.h"
      GOTO 999
C ---------------------- end of UNIX code -------------------------
#endif
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
C ------------------------------  VAX code ------------------------
      SYSTEM = 'VAX '
C
C - if a directory is given assume that the filename is the final one
      IF (INDEX(ANAME,':').GT.0 .OR. INDEX(ANAME,']').GT.0) GOTO 999
C
C - is it a standard EDIR or EPIO file ?  (i.e. Axxxxx.EDIR)
      IEDIR = INDEX(ANAME,'EDIR')
      IEPIO = INDEX(ANAME,'EPIO')
      IF (IEDIR.GT.0 .OR. IEPIO.GT.0) THEN
        IF (ANAME(1:1).EQ.'A' .OR. ANAME(1:1).EQ.'Y') THEN
          IDOT = INDEX(ANAME,'.')
          IF (IDOT.EQ.0) IDOT = INDEX(ANAME,' ')
          IF (IDOT.EQ.7 .OR. ANAME(1:1).EQ.'Y') THEN
            IF (IEDIR.GT.0) THEN
               FNAME = 'AL$EDIR:'//ANAME(1:LAN)
            ELSE
               FNAME = 'AL$DATA:'//ANAME(1:LAN)
            ENDIF
            IF (ANAME(2:2).EQ.'M') FNAME='AL$MC'//FNAME(4:)
            LE = LNBLNK(FNAME)
            IBLK = INDEX(FNAME(1:LE),' ')
            IF (IBLK.GT.0) FNAME(IBLK:IBLK) = '.'
            IST = 1
          ENDIF
        ENDIF
      ELSE
C
C - is it an ALEPH standard file ?
         DO L=1,LENVIR
            IF (ANAME(1:LAN).EQ.AENVIR(L)(1:LAN)) THEN
               IST = 1
               ICARD = INDEX (ANAME,'CARDS')
               IF (ICARD.GT.0) THEN
C            data card file then bild a temporary name
                  IF (ANAME(1:1).EQ.'L') THEN
                     FNAME = 'LOOK.CARDS'
                   ELSE
                     FNAME = ANAME(1:1)//'XXXXXXX.CARDS'
                   ENDIF
               ELSEIF (ANAME(1:4) .EQ. 'ADBS') THEN
C             data base file fulfill the directory name
                  FNAME = 'DBASE:'//ANAME(1:LAN)//'.'//'DAF'
               ELSEIF (INDEX(ANAME,'FMT') .GT. 0) THEN
C             format file give the directory name
                  FNAME = 'BANKDOC:BANKAL.FMT'
               ELSEIF (INDEX(ANAME,'LIST') .GT. 0) THEN
C             runcart list give directory
                  FNAME = 'BOOK:RUNCARTS.LIST'
               ELSEIF (INDEX(ANAME,'BANK') .GT. 0) THEN
C              dbas bank give directory
                  FNAME = 'PHY:DBAS.BANK'
               ELSEIF (INDEX(ANAME,'BEAM') .GT. 0) THEN
C              beam position give directory
                  FNAME = 'PHY:BEAM.POSITION'
               ENDIF
               GOTO 999
            ENDIF
         ENDDO
      ENDIF
C
C ------------------------------- end of VAX code -----------------
#endif
#if defined(IBM)
C ----------------------------- IBM code ---------------------------
      SYSTEM = 'IBM '
C
C - Is it a standard EPIO or EDIR file ?
      IEDIR = INDEX(ANAME,'EDIR')
      IEPIO = INDEX(ANAME,'EPIO')
      IEXT  = MAX (IEDIR,IEPIO)
      IF ( (IEDIR.GT.0 .OR. IEPIO.GT.0) .AND.
     &     (ANAME(1:1).EQ.'A' .OR. ANAME(1:1).EQ.'I') ) THEN
         FNAME = ANAME(1:LAN)
         IF (ANAME(LAN-1:LAN-1).NE.' ') FNAME = FNAME(1:LAN)//' *'
         IST = 1
      ELSE
C
C - Is it an ALEPH standard file ?
         DO L=1,LENVIR
            IF (ANAME(1:LAN).EQ.AENVIR(L)(1:LAN)) THEN
               IST = 1
               ICARD = INDEX (ANAME,'CARDS')
               IF (ICARD.GT.0) THEN
C            data card file build a temporary name
                  IF (ANAME(1:1) .EQ.'L') THEN
                     FNAME = 'LOOK CARDS *'
                   ELSE
                     FNAME = ANAME(1:1)//'XXXXXXX CARDS *'
                   ENDIF
               ELSEIF (ANAME(1:4) .EQ. 'ADBS') THEN
C             data base file assume that a GIME DBASE is done outside
                  FNAME = ANAME(1:LAN)//' '//'DAF *'
               ELSEIF (INDEX(ANAME,'FMT') .GT. 0) THEN
C             format file give the directory name
                  FNAME = 'BANKAL FMT *'
               ELSEIF (INDEX(ANAME,'LIST') .GT. 0) THEN
C             runcart list : assume that GIME ALSOFT 230 is done
                  FNAME = 'RUNCARTS LIST *'
               ELSEIF (INDEX(ANAME,'BANK') .GT. 0) THEN
C              dbas bank :assume that GIME PHY is done
                  FNAME = 'DBAS BANK *'
               ELSEIF (INDEX(ANAME,'BEAM') .GT. 0) THEN
C              beam position :assume that GIME PHY is done
                  FNAME = 'BEAM POSITION *'
               ENDIF
               GOTO 999
            ENDIF
         ENDDO
      ENDIF
C
C - it's not an ALEPH file name may be a true file name
      IST = 0
      GOTO 999
C
C ------------------------------- end of IBM code -----------------
#endif
C - end
 999  CONTINUE
#include "prenvir.h"
      ALGTENV = IST
      END
#endif
