      SUBROUTINE ALRWEV
C----------------------------------------------------------------------
C - F.Ranjard - 910804          from M.Talby
C - modified by - F.Ranjard - 921105
C   keep events with LUPA bank
CKEY EDIR READ WRITE / USER
C! Steering routine to read, classify and/or write events.
C-
C   Called by   : USER
C   Calls  : ALPHARD and EDIR packages
C -
C   Input data cards : FILI, FILO, SEVT, SRUN, IRUN, NEVT
C                      DEBUG   recmn  recmx
C                              recmn = 1st record to debug
C                              recmx = last record to debug
C                or    DEBU    recmx
C                              recmx = 1st recmx records to debug
C                      MINI    the input file is a MINI
C-
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
C --
#include "revhjj.h"
C --
      CHARACTER*80 ULIST
      DIMENSION LUTDAT(4)
      LOGICAL FWRIT
      PARAMETER (ILT=21)
      CHARACTER LISTB*5, RLIST*1
      PARAMETER (LISTB='CERST')
      LOGICAL BTEST
      DATA ULIST/'   '/
C --
C ------------------------------------------------------------------------------
C - open data base that is necessary when class word does not exist
      CALL AOPDBS ('    ',IER)
      IF (IER .NE. 0) THEN
         CALL AWERRC (IW(6),'ALRWEV','data base file ',IER)
      ENDIF
C
C - debug flags
      JDEBU = IW(NAMIND('DEBU'))
      IF (JDEBU.EQ.0) THEN
         IRECMN = 0
         IRECMX = 0
      ELSEIF (IW(JDEBU).EQ.0) THEN
         IRECMN = 1
         IRECMX = 100000
      ELSEIF (IW(JDEBU).EQ.1) THEN
         IRECMN = 1
         IRECMX = IW(JDEBU+1)
      ELSEIF (IW(JDEBU).EQ.2) THEN
         IRECMN = IW(JDEBU+1)
         IRECMX = ABS (IW(JDEBU+2))
      ENDIF
C   the class word of LUPA events is modified to keep only
C   class 7,8,11,12,13
C   the minimum number of banks per event to appear on the EDIR file
C   is 3 for MINI and 4 for DST unless it contains a LUPA bank
C   the number of banks per LUPA event is 3 for MINI and 4 for DST
      IF (IW(NAMIND('MINI')).NE.0) THEN
         NBKLU = 3
         NBKMN = 3
      ELSE
         NBKLU = 4
         NBKMN = 4
      ENDIF
C
      NREVH = NAMIND ('REVH')
      NLUPA = NAMIND ('LUPA')
      IFILE= 0
      RLIST = 'E'
C                 open  input/output  unit
 9    IFILE = IFILE+1
      IREC = 0
      ISLOW = 0
      IRUNR = 0
      IEVR = 0
      IEV = 0
      NRUN = 0
C
C                 read a record
 10   CALL ABRSEL(RLIST,ULIST,IRET)
      IF (IREC.EQ.0) THEN
         WRITE (6,*) ' ALOPEN IFILE= ',IFILE,' IRET= ',IRET
         CALL ABUNIT(LINDAT,LINSEL,LINSE2 , LUTDAT,LUTSEL)
         IF(IRET.GT.5) GOTO 9999
      ENDIF
      IREC=IREC+1
      IF (IRET.GE.17) GOTO 10
      IF (IRET.GE.6) GOTO 9999
C                 if end of file or end of data open next file
      IF(IRET.GE.4) THEN
        WRITE (6,*) ' FILE ',IFILE,' RUN ',NRUN,' IREC= ',IREC
     &  ,' EVT= ',IEV,' RUNR= ',IRUNR,' EVTR= ',IEVR
     &             ,' SLOW= ',ISLOW
        GOTO 9
      ENDIF
C
C      set write flag to TRUE
C
      FWRIT = .TRUE.
C --
C      If LUTSEL.gt.0 Then events are classified and Edir written
C --
      IF (LUTSEL .EQ. 0) GOTO 30
C
C - get the run and event #s.
      CALL ABRUEV (NRUN,NEVT)
C - get number of banks in the record
      NBK = IGTLEN (IW,RLIST)
      KREVH = IW (NREVH)
      KCLASW = 0
C
C - event record
C
      IF (IRET.EQ.1) THEN
         IEVR = IEVR+1
C --
C     Check the number of banks in the 'R' list before REVH analysis.
C     IF the number of banks (NBK) is less than NBKMN and the event
C     does not contain a LUPA bank THEN skip  the event.
C     Otherwise get the class word from REVH bank or SELEVT subroutine.
C     IF NBK <= NBKLU and LUPA bank is there THEN
C     set class 7,8,11,12,13 when they are present,
C     forget the other ones.
C     ELSE set class word according to REVH bank.
C --
        IF(NBK.LT.NBKMN .AND. IW(NLUPA).EQ.0) THEN
          FWRIT = .FALSE.
        ELSE
          IF(KREVH.GT.0 .AND. IW(KREVH+LMHCOL).GE.JREVEC) THEN
C          get write class word from REVH bank
             KCLASW = IW(KREVH+LMHLEN+JREVEC)
          ELSE
C          build the EDIR  class word
             CALL ALCLASW (KCLASW)
C          reset write class word before filling it again
             CALL ABCLAS (0)
          ENDIF
          IEV = IEV+1
          IF (NBK.LE.NBKLU .AND. IW(NLUPA).GT.0) THEN
            IF (BTEST(KCLASW,6)) CALL ABCLAS (7)
            IF (BTEST(KCLASW,7)) CALL ABCLAS (8)
            IF (BTEST(KCLASW,10)) CALL ABCLAS (11)
            IF (BTEST(KCLASW,11)) CALL ABCLAS (12)
            IF (BTEST(KCLASW,12)) CALL ABCLAS (13)
          ELSE
            CALL ABSTCL (KCLASW)
          ENDIF
        ENDIF
C
C - run record
C
      ELSEIF (IRET.EQ.2) THEN
         IRUNR = IRUNR+1
         ICLAS  = -1
         CALL ABCLAS (ICLAS )
C
C - slow control record
C
      ELSEIF (IRET.EQ.3) THEN
         ISLOW = ISLOW+1
         ICLAS = 25
         CALL ABCLAS (ICLAS)
      ENDIF
C
C            write the record
C
 30   CONTINUE
      IF (FWRIT) THEN
          CALL ABWSEL (RLIST)
          IF (LUTSEL.NE.0) THEN
             CALL ABGTWCL (KCLASW)
          ELSE
             CALL ABGTCL (KCLASW)
          ENDIF
          CALL ALSUMCL (KCLASW)
      ENDIF
C
C - Debug
      IF (IREC.GE.IRECMN .AND. IREC.LE.IRECMX)
     &   WRITE (6,*) ' IREC= ',IREC,' FWRIT= ',FWRIT
     &           ,' IRET= ',IRET,' NRUN= ',NRUN
     &           ,' NEVT= ',NEVT,' NBK= ',NBK,' CLAS= ',KCLASW
C --
      GOTO 10
C --
 9999 CONTINUE
        WRITE (6,*) ' return code= ',IRET
        WRITE (6,*) ' FILE ',IFILE,' RUN ',NRUN,' IREC= ',IREC
     &  ,' EVT= ',IEV,' RUNR= ',IRUNR,' EVTR= ',IEVR
     &             ,' SLOW= ',ISLOW
      CALL ABWEND
      CALL ALSUMCL (-1)
      CALL BOSIO
      CALL ACLOSE (0,IER)
C --
      END
#endif
