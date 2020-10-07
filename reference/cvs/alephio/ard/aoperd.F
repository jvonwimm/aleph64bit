      SUBROUTINE AOPERD( LINDAT, LINSEL, LINSE2, IER )
C-----------------------------------------------------------------------
CKEY ALREAD  OPEN EDIR
C  Author:     D.Schlatter                          April 89
C  Modified by M. Talby                             June  89
C  Modified by M. Talby                             May   89
C
C! OPENs input data files (EPIO, NATIVE) and event directory files (TEXT
C  in READ mode in sequential or direct access.
C  Returns logical input units:
C  Steering is done with FILI card.
C
C-----------------------------------------------------------------------
C  Input :     LINDAT  /I   logical unit for INPUT data file
C
C  Output:     LINSEL  /I   logical unit for INPUT select file
C              LINSE2  /I   logical unit for second select file
C              IER     /I = 0 SUCCESSFUL OPENING
C
C  Called by: ALOPEN
C
C  Calls:   AFILMI,AOPEN,BREAD,BSEQH,BUNIT,NLINK
C
C-----------------------------------------------------------------------
#ifndef DOC
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      INCLUDE '($LNMDEF)'
      INCLUDE '($SSDEF)'
      INTEGER*2 ITMLS(8) /10,LNM$_STRING,6*0/
      EQUIVALENCE (ITMLS(3),IADDR)
      EQUIVALENCE (ITMLS(5),LEN)
      INTEGER SYS$TRNLNM
      INTEGER STATUS
      CHARACTER*10 NODE
#endif
      SAVE NFILM,ALDAT,LDAT
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*80  NAMDAT,DEVDAT,NAMSEL
      CHARACTER*80  FNAME,FTYPE,FDEV
      CHARACTER*50  CPARA,CWORK
      CHARACTER*80  ALDAT,VXSTG
      CHARACTER*6   TYPDAT
      CHARACTER*4   BFORMA,CARD
      CHARACTER*3   FTYP3
      CHARACTER     SUBR*8
      INTEGER       LINDAT(*),LINSEL(*),LINSE2(*)
      INTEGER       ACARD0,ACARD1,ACARD3
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C --
C - 1st entry
      IF(FIRST) THEN
#if defined(ALEPH_DEC) &&  ! defined(UNIX)
C        get the VAX node
        IADDR = %LOC(NODE)
        LEN = %LOC(LENNA)
        STATUS = SYS$TRNLNM(,'LNM$SYSTEM_TABLE','SYS$CLUSTER_NODE',
     &                      ,ITMLS)
C       IF (.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
#endif
        SUBR  =' AOPERD '
        FIRST = .FALSE.
        FNAME='    '
        FTYPE='    '
        FDEV ='    '
        NFILM = NAMIND('FILM')
C       Define header bank for selection
        CALL BSEQH('EVEH',0,2,6)
        CALL BSEQH('RUNR',0,2,2)
        CALL BSEQH('RUNH',0,2,2)
        CALL GETENVF ('ALDATA',ALDAT)
        LDAT = LNBLNK(ALDAT)
        IF (LDAT.NE.0) THEN
           ALDAT = ALDAT(1:LDAT)//'/'
           LDAT = LDAT+1
        ENDIF
      ELSE
C       if there was an .EDIR input file check if there is
C       anymore FILM cards
        IF (LINSEL(1).NE.0) THEN
           IF (IW(NFILM).EQ.0) THEN
              CALL BTERD (IW,LINSEL(1),'T',*20,*20)
           ENDIF
           CALL AFILMI(NAMDAT,TYPDAT,DEVDAT,IRFILM)
#include "prfilm.h"
           IF (IRFILM.NE.0) GOTO 20
           IO = 1
           GOTO 45
         ENDIF
      ENDIF
C --
C - next entry
 20   CONTINUE
      IRFILM = -1
C --
C     Analize the new input streams if any
C
      CARD = 'FILI'
      IO = 1
C
C        set data logical unit, select logical units to 0
         LINDAT(IO) = 0
         LINSEL(1) = 0
         LINSE2(1) = 0
C
C        get one card
C
         CALL ACDARG(CARD,'EPIO','*',FNAME,FTYPE,FDEV,IRFILI)
         IER = IRFILI
C
C        IF there was no FILI card or something went wrong with
C        this card THEN skip it
         IF(IRFILI.NE.0) GOTO 997
C
         NAFILI(IO) = CARD
         NAMI = ACARD0(NRFILI(IO))
         IFTHRU=ACARD3(2,'THRU',CPARA)
C
C        FILI card given with no filename
C        let assume that filedef or assign is used.
C
         FTYP3 = FTYPE(1:3)
         IF(FNAME(1:1).EQ.' '.AND.FTYP3.NE.' ')THEN
           IER   = 0
           LINDAT(IO) = JUNIAL('INP',3*IO-2, 0)
           LUNIT = LINDAT(IO)
           BFORMA='FORT'
           IF(FTYP3.EQ.'EPI') THEN
             BFORMA='EPIO'
           ELSEIF(FTYP3.EQ.'EDI') THEN
             LINSEL(IO) = JUNIAL('INP',3*IO-1, 0)
             LUNIT = LINSEL(IO)
             BFORMA='TEXT'
           ENDIF
           LREC = JULREC (FTYP3,FDEVI)
           CALL BUNIT(LUNIT,BFORMA,LREC)
           IF(IFTHRU.GT.0) THEN
             LINSE2(IO) = JUNIAL('INP',3*IO,0)
             BFORMA='TEXT'
             CALL BUNIT(LINSE2(IO),BFORMA,LREC)
           ENDIF
           GOTO 998
         ENDIF
C
C        FILI card given with filename: skip the stream if file
C        not found
C
 45      CONTINUE
         IF(FNAME(1:1).NE.' ') THEN
            IER = 0
C=======================================================
C                 treat select file(s) if there
C=======================================================
            IF(FTYP3.EQ.'EDI') THEN
               LINSEL(IO) = JUNIAL('INP',3*IO-1, 0)
               IF(IFTHRU.NE.0) THEN
                 FDEV = CPARA(6:IFTHRU)
                 LINSE2(IO) = JUNIAL('INP',3*IO,0)
               ENDIF
C              if the EDIR file has not changed skip the setting
C              up of the file (still a FILM card on the EDIR file)
               IF(IRFILM.NE.0) THEN
                  IF(IFTHRU.GT.0) THEN
C                 go through 2 event directories (2 select files)
                    CALL AOPEN(LINSEL(IO),FNAME,FTYPE,FDEV,IER)
                    IEDIR = INDEX(FDEV,'EDIR')
                    NAMSEL=FDEV(1:IEDIR+4)//GMODEN
                    CALL AOPEN(LINSE2(IO),NAMSEL,FTYPE,FDEV,IER)
                  ELSE
C                 One select-file only
                    NAMSEL=FNAME
                    CALL AOPEN(LINSEL(IO),NAMSEL,FTYPE,FDEV,IER)
                  ENDIF
                  IF(IER.NE.0) THEN
                    IF (IW(6) .GT. 0) WRITE(IW(6),*)
     &              ' _AOPERD_  CANNOT open input EDIR file IER= ',IER
                    LINSEL(IO) = 0
                    LINSE2(IO) = 0
                    GOTO 998
                  ENDIF
               ENDIF
C              Get data filename
               IF (IFTHRU.GT.0) THEN
                  CALL BTERD(IW,LINSE2(IO),'R',*997,*997)
               ENDIF
               IF (IRFILM.NE.0) THEN
                  CALL BTERD(IW,LINSEL(IO),'T',*997,*997 )
                  CALL AFILMI(NAMDAT,TYPDAT,DEVDAT,IRET)
#include "prfilm.h"
                  IER = IRET
                  IF(IER.NE.0) THEN
                     LINSEL(IO) = 0
                     LINSE2(IO) = 0
                     GOTO 997
                  ENDIF
               ENDIF
#if defined(UNIX)
C              replace ALWS directory by ALUNIX pathname if it is
C              a  VAX FILM card
               IVAX = INDEX(NAMDAT,':')
               IF (IVAX.NE.0) THEN
C               FILM card comes from VAX
                  LNAM = LNBLNK(NAMDAT)
                  IF (INDEX(NAMDAT,'AL$DATA').NE.0 .OR.
     &                INDEX(NAMDAT,'AL$MCDATA') .NE.0 ) THEN
C                 IBM and RS6K compilers do not understand
C                          NAMDAT = 'blabala'//NAMDAT
                     NAMSEL = NAMDAT
                     NAMDAT = ALDAT(1:LDAT)//NAMSEL(IVAX+1:LNAM)
                  ENDIF
                  CALL CUTOL (NAMDAT)
               ELSEIF (INDEX(DEVDAT,'GIME') .GT. 0) THEN
C               FILM card comes from IBM
                  DEVDAT = 'CART '//NAMDAT(1:6)//'.1.SL'
                  NAMDAT = 'ALDATA'
               ENDIF
#else
#if defined(ALEPH_DEC)
C              On ALWS and if IBM FILM card replace GIME by CART
C              to get the file through SHIFT alstagein
               IF (NODE(1:4) .EQ. 'ALWS') THEN
                 IVAX = INDEX(NAMDAT,':')
                 IF (IVAX.EQ.0) THEN
                    IGIME = INDEX(DEVDAT,'GIME')
                    IF (IGIME.GT.0) THEN
C                    FILM card comes from IBM
                      DEVDAT = 'CART '//NAMDAT(1:6)//'.1.SL'
                      NAMDAT = 'ALDATA'
                    ENDIF
                 ENDIF
               ENDIF
#endif
#endif
C
            ELSE
C=======================================================
C            no select files , define names
C=======================================================
               NAMDAT=FNAME
               TYPDAT=FTYPE
               DEVDAT=FDEV
            ENDIF
C
C=======================================================
C                open data  file
C=======================================================
#ifndef UNIX
C =============== IBM ====== VAX ==========================
C           there is a problem with a data file on TAPE or CART
C           such a file must be stagein, but the system opens it

C           in sequential mode. So if we want to use an event dir.
C           the file has to be reopened in direct access mode.
C           the only way found so far is to AOPEN it for the STAGEin
C           on a different unit and then to AOPEN the resulting disk
C           file in direct mode on the standard unit.
            IF (FTYP3.EQ.'EDI') THEN
C              logical unit for data file opened in direct access
               LINDAT(IO) = JUNIAL('INP',3*IO-2, 0)
               IF(DEVDAT(1:4).NE.'    '.AND.DEVDAT(1:4).NE.'DISK') THEN
                  LUN = JUNIAL('INP',3*(IO+1)-2, 0)
                  CALL AOPEN (LUN,NAMDAT,TYPDAT,DEVDAT,IER)
                  IF (IER.NE.0) THEN
                     IF (IW(6).GT.0) THEN
                        WRITE(IW(6),*)
     &                  ' _AOPERD_ cannot stage data file ',DEVDAT,
     &                  ' type= ',TYPDAT(1:3),' label= ',NAMDAT
                     ENDIF
                     LINDAT(IO) = 0
                     GOTO 998
                  ENDIF
#if defined(IBM)
                  IPOSS = INDEX(DEVDAT,'.SL')
                  IPOSN = INDEX(DEVDAT,'.NL')
                  IPOSC = INDEX(DEVDAT,'.sl')
                  IPOS  = IPOSS+IPOSN+IPOSC
                  IF(IPOS.NE.0) THEN
                     NAMDAT = 'T'//DEVDAT(6:11)//' FSEQ'
     &                        //DEVDAT(IPOS-1:IPOS-1)//' *'
                  ELSE
                     NAMDAT = 'T'//DEVDAT(6:11)//' FSEQ1 *'
                  ENDIF
#endif
#if defined(ALEPH_DEC)
                  IF (NODE(1:4).EQ.'ALWS')  THEN
                     NAMDAT = 'FOR000'
                     CALL CSETDI (LUN,NAMDAT,5,6)
                  ELSE
                     NAMDAT = VXSTG//DEVDAT(6:11)//'_'//DEVDAT(6:11)
     &                        //DEVDAT(12:13)//'_EBCDIC '
                  ENDIF
#endif
                  DEVDAT = 'DISK  '
               ENDIF
            ELSE
C              logical unit for data file opened in sequential access
               LINDAT(IO) = JUNIAL('INP',3*(IO+1)-2,0)
            ENDIF
#else
C ===============UNIX machines ========================================
            IF (FTYP3.EQ.'EDI') THEN
C              logical unit for data file opened with edir
               LINDAT(IO) = JUNIAL('INP',3*IO-2, 0)
            ELSE
C              logical unit for data file opened without edir
               LINDAT(IO) = JUNIAL('INP',3*(IO+1)-2,0)
            ENDIF
C =============== end of UNIX ========================================
#endif
C           open the data file
            CALL AOPEN (LINDAT(IO),NAMDAT,TYPDAT,DEVDAT,IER)
            IF(IER.NE.0) THEN
               IF (IW(6).GT.0) WRITE(IW(6),*)
     &             ' _AOPERD_  CANNOT open input data file ',NAMDAT
               LINDAT(IO) = 0
               GOTO 998
            ENDIF
         ENDIF
C --
      GOTO 998
C
C -- error
C
  997 IF (IER .GT. 0) THEN
C - ACDARG error or FILI or FILM card
         GOTO 998
      ELSEIF (IER .EQ. -1) THEN
C - no more FILI card
         IER = -3
      ELSE
C - problem with EDIR file
         IER = -2
      ENDIF
 998  RETURN
      END
#endif
