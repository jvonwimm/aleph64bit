      PROGRAM NEWDAF
C---------------------------------------------------------
C - F.Ranjard - 900328
C! replace TAB2 and COL2 by new ADAMO tables
C  read ADBSFULL.DAF current version
C  create NEWDAF with the same number of records as the
C  current DAF.
C  copy all tables except COL2 and TAB2 to NEWDAF
C  get .COL and .TAB from ADAMO.DAF
C  copy them onto NEWDAF
C - data cards:
C         FDBA  'name of current version of data base'
C         FDBA  'name of the new ADAMO table DAF'
C               the order of the 2 FDBA is relevant
C         DROP  list of banks to be dropped
C               all banks with these names will be dropped
C               if SWAP then DROP 'TAB2' 'COL2'
C               if NOT SWAP then DROP '.TAB'  '.COL'
C         SWAP  get .TAB and .COL from ADAMO
C               change .TAB,NR=0 to TAB2,NR=1
C               change .COL,NR=0 to COL2,NR=1
C         ENDQ
C--------------------------------------------------------
#ifndef DOC
#include "lbos.h"
C
      PARAMETER (LJW=400000)
      COMMON /FCS/   JW(LJW)
C
      CHARACTER*80 TFIL
      CHARACTER*4   NAME,PASSW,NAMB
      CHARACTER*4   CHAINT, NLIST
      DATA PASSW/'ADAM'/
#include "bmacro.h"
C -----------------------------------------------------------------
      CALL BNAMES(NAMAX)
      CALL BOS(IW,LBOS)
      CALL BOS(JW,LJW)
C
C***     PRINTOUT ON FILE   *****
C
      IW(5) = 7
      IW(6) = 6
      IW(7) = 10000
      IW(8) = 10000
      JW(8) = IW(8)
      JW(7) = IW(7)
      JW(6) = IW(6)
      LCARD = IW(5)
      LTERM  = 6
      WRITE(LTERM,'(//A/)') ' ------ NEWDAF PROCESS  ------ '
C
C *****       read data cards
C
      CALL GETENVF ('NEWDAFCARDS',TFIL)
      IF (TFIL.NE.' ') CALL AOPEN(LCARD,TFIL,'CARD','DISK',IER)
      CALL BREADC
      JDROP = NLINK('DROP',0)
      JSWAP = NLINK('SWAP',0)
C
C *****       OPEN current ADBSFULL daf       *****
C
      WRITE (LTERM,*) ' open the ADBSFULL.DAF current version'
      LDAF = JUNIDB(0)
      CALL AOPDBS ('   ',IER)
      IF (IER.NE.0) THEN
         CALL AWERRC (IW(6),'NEWDAF','ADBSFULL.DAF',IER)
         CALL EXIT
      ENDIF
      IBF = NLINK ('+BUF',LDAF)
      IF (IBF.EQ.0) THEN
         WRITE(IW(6),*) ' Programming error: bank +BUF does not exist'
         GOTO 999
      ENDIF
      JBLOC = IW(IBF+12)
      CALL BDAPR(LDAF,'    ')
      CALL BOSIO
C
C ****         OPEN ADAMO new DAF ********
C
      WRITE (LTERM,*) ' open ADAMO new DAF'
      LADAMO = NUNIDB(14)
      LADAMO = JUNIDB(0)
      CALL AOPDBS ('   ',IER)
      IF (IER.NE.0) THEN
         CALL AWERRC (IW(6),'NEWDAF','ADAMO.DAF',IER)
         CALL EXIT
      ENDIF
C
      CALL BDAPR(LADAMO,'    ')
      CALL BOSIO
C
C Create NEWDAF on unit LUNEW=23
C
      LUNEW = 23
      NBREC = JBLOC+20
      IRECL = JULREC ('DAF','DISK')
      WRITE (LTERM,*) ' NEWADBS DAF is created with ',NBREC,' records'
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
       OPEN(LUNEW,FILE='NEWADBS.DAF',STATUS='NEW',ACCESS='DIRECT',
     &      ORGANIZATION='RELATIVE',RECL=IRECL,FORM='UNFORMATTED',
     &      ERR=90)
       IW(1) = 2
       CALL INITDU (LUNEW,IRECL,NBREC,'ADAM')
       IW(1) = 0
       CALL BDABF (LUNEW,IRECL,'NEWADBS.DAF','ADAM')
#endif
C
      WRITE (LTERM,*) ' copy existing DAF to NEWADBS : wait...'
  200 JNAME = MDARS (JW,LDAF)
      IF (JW(2) .NE. 0) GOTO 210
C
      IF (JNAME .EQ. 0) THEN
         WRITE (IW(6),*) ' not enough space for the bank, skip it '
         GOTO 200
      ENDIF
      NAME = CHAINT (JW(JNAME-3))
      NR = JW(JNAME-2)
C
C - drop banks NR=1 and LROWS=0
      IF (NR.EQ.1) THEN
         IF (JW(JNAME+LMHROW).EQ.0) THEN
            WRITE(IW(6),*) 'drop bank with 0 rows ',NAME,NR,
     &                      (JW(JNAME+MM),MM=0,2)
            ID1 = MDROP(JW,NAME,NR)
            GOTO 200
         ENDIF
      ENDIF
C - drop banks from DROP data cards
      IF (JDROP.NE.0) THEN
         DO 201 I=1,IW(JDROP)
            IF (NAME.EQ.CHAINT(IW(JDROP+I))) THEN
            WRITE(IW(6),*) 'drop bank from DROP card ',NAME,NR,
     &                      (JW(JNAME+MM),MM=0,2)
               ID1 = MDROP(JW,NAME,NR)
               GOTO 200
            ENDIF
 201     CONTINUE
      ENDIF
C - copy the bank to NEWDAF
      CALL BDAWR (JW,LUNEW,NAME,NR)
      IDRP = MDROP (JW,NAME,NR)
      IF ((JW(16)-JW(15)).LT.5000) CALL BGARB(JW)
C - next bank
      GOTO 200
C
C  End of current DAF reached
C
 210  CONTINUE
      WRITE(IW(6),*) ' end of OLD DAF '
C
C - read .TAB and .COL from ADAMO new DAF
C   rename them to TAB2 and COL2
C   write them to NEWDAF
      JNAME = MDARD (IW,LADAMO,'.TAB',0)
      JNAME = MDARD (IW,LADAMO,'.COL',0)
C - swap if require
      IF (JSWAP.NE.0) THEN
         JNAME = MSWAP (IW,'.TAB',0,'TAB2',1)
         JNAME = MSWAP (IW,'.COL',0,'COL2',1)
      ENDIF
C - write onto NEWDAF
      CALL BDAWR (IW,LUNEW,'TAB2',1)
      CALL BDAWR (IW,LUNEW,'COL2',1)
C
      CALL BDAPR(LUNEW,'    ')
C
 800  CONTINUE
      WRITE(IW(6),'(//A)')
     +          ' ------ DATABASE STATUS AFTER NEWDAF ------ '
      CALL BOSIO
C
  999 STOP
C
C - Error opening new DAF
 90   WRITE (IW(6),*) ' error opening NEWADBS DAF - IOP = ', IOP
      STOP
C
      END
#endif
