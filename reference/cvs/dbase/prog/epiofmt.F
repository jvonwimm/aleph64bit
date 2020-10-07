      PROGRAM EPIOFMT
C----------------------------------------------------------------------
C! create a  BANKAL.FMT and/or a ADBSCONS.EPIO
C!
C!   Author   :-    F.RANJARD                   09-JULY-1990
C!
C!   data cards:
C!
C!        ******* read current ADBSCONS ************************
C!        ******* write new BOS format file ********************
C!        * open/read data base file
C!        FDBA  'DBASE:ADBSCONS.DAF'
C!        * open/write BANKAL.FMT
C!        FFMT  'DBS:BANKAL.FMT | CARD'
C!        * open/write NEW ADBSCONS.EPIO
C!        FEPI  'DBS:ADBSCONS.EPIO | EPIO'
C!        ENDQ
C!
C!   Libraries required: ALEPHLIB, BOS77, CERNLIB
C!
C!   Description
C!   ===========
C!  read a data base file
C!  write the BOS formats onto file FFMT
C!  write the data base EPIO file onto file FEPI
C?
C!======================================================================
#ifndef DOC
      CHARACTER*80 TFIL,FTYP,FDEV,TOUT,TFULL
      CHARACTER*4 CHAINT,OUT,TOU,TAB,COL,NAME,PASSW
      CHARACTER*120 MSG,MSGE,MSGB
#include "lbos.h"
#include "bmacro.h"
C
C ------------------------------------------------------------------
C
C - Initialize BOS
       CALL BNAMES (NAMAX)
       CALL BOS (IW,LBOS)
       IW(5) = 7
       IW(7) = 100000
C
C - Read data card file
       CALL GETENVF ('EPIOFMTCARDS',TFIL)
       IF (TFIL.NE.' ') CALL AOPEN (IW(5),TFIL,'CARD','DISK',IER)
       CALL BREADC
C
C - Open data base EPIO file  on LUEPI=25
         LUEPI = 25
         CALL ACDARG('FEPI','EPIO','*',TFIL,FTYP,FDEV,IER)
         IF (IER.EQ.-1) THEN
            LUEPI = 0
         ELSEIF(IER.NE.0) THEN
            WRITE (IW(6),*) ' problem with FEPI card - EXIT'
            CALL EXIT
         ELSE
C        write in default record length 32040
CFLR            LDUM = NALREC(3600)
         ENDIF
#include "erase.h"
         IF (LUEPI.GT.0) THEN
            CALL AOPENW(LUEPI,TFIL,FTYP,FDEV,IER)
            IF (IER.NE.0) THEN
               CALL AWERRC (IW(6),'EPIOFMT',TFIL,IER)
               CALL EXIT
            ENDIF
         ENDIF
C
C - open current data base ADBSCONS.DAF in read mode
C
      TFULL = '    '
      LUFUL = NUNIDB(15)
      LUFUL = JUNIDB(0)
      IRECL = JULREC('DAF','DISK')
      IF (LUFUL.GT.0) THEN
C      open/read
         CALL AOPDBS (TFULL,IER)
         IF (IER.NE.0) THEN
           CALL AWERRC(IW(6),'EPIOFMT',TFULL,IER)
         ENDIF
      ENDIF
C
C - open the ASCII file to fill bank formats
C
       LUFMT = 18
       CALL AGTFIL ('FFMT','WRIT',LUFMT,IER)
       IF (IER.EQ.-1) THEN
          LUFMT = 0
       ELSEIF (IER.GT.0) THEN
          CALL EXIT
       ENDIF
C
C
C         create and fill FMT file
       JCPRO = MDARD (IW,LUFUL,'CPRO',0)
       JTPRO = MDARD (IW,LUFUL,'TPRO',0)
       CALL CRTFMT (LUFMT,'CPRO','TPRO')
       JCADB = MDARD (IW,LUFUL,'CADB',0)
       JTADB = MDARD (IW,LUFUL,'TADB',0)
       CALL CRTFMT (LUFMT,'CADB','TADB')
C
C - copy all banks except ADAMO tables
C
          IF (LUEPI.GT.0) THEN
  200        JNAME = MDARS (IW,LUFUL)
             IF (IW(2) .NE. 0) GOTO 997
             IF (JNAME .EQ. 0) THEN
                WRITE (IW(6),*) ' not enough space, skip it '
                GOTO 200
             ENDIF
             NAME = CHAINT (IW(JNAME-3))
             NR = IW(JNAME-2)
             CALL BEPWS (IW,LUEPI,NAME,NR)
             JNAME = NDROP (NAME,NR)
             CALL BGARB (IW)
             GOTO 200
          ENDIF
C
 997   CONTINUE
       IF (LUEPI.GT.0) CALL BWRITE(IW,LUEPI,'0')

       CALL BOSIO
       CALL BOSTA
       CALL ACLOSE (0,IER)
       STOP
C
C - error in opening file
C
 90   CONTINUE
      STOP
      END
#endif
