      SUBROUTINE OLSPOT(IFLAG)
C----------------------------------------------------------------------
C!  - Create run header bank BLQP and 
C!    store beam spot positions from LEP BOM data
C!    and QS0 position measurements in bank BLQP
C! 
C!   Input argument:  IFLAG= 0 for event and slow control record processing
C!   --------------   IFLAG= 1 for end of run processing
C!                    IFLAG= 2 for end of job processing
C!
C!   Notes:  1. This routine should not be called at run initialization by
C!              JULIA or ALPHA. It will detect itself a change of run number
C!              when processing a call with IFLAG=0. This is because slow
C!              control records can appear between run records and the first
C!              event record in a run. Since the run initialization (in JULIA
C!              and ALPHA) is performed only when the first event record is
C!              encountered one would miss any such slow control records ...
C!
C!           2. The end_of_run call with IFLAG=1 should be done from 
C!              routine ABOLDR and at the end of the job.
C!
C!           3. The behaviour of this routine can be changed with card OLSP:
C!                OLSP -1     ---> routine OLSPOT becomes a dummy routine
C!                OLSP +1     ---> routine OLSPOT write a lot of debug output
C!                OLSP +1 34  ---> routine OLSPOT write a lot of debug output
C!                                  on logical unit 34
C! 
C!           4. The cuts and control flags are taken from bank BLQC; 
C!              this bank is read from the database, unless card BLQC -1/ 
C!              is provided; if no BLQC bank is found, a default BLQC bank 
C!              is created.
C!               
C!   Author:  O. Schneider
C!
C!   History: 07-JUN-1996 first version for private tests
C!            20-JUN-1996 incorporated in test version of JULIA after 
C!                         adding call to AUBPRS; 
C!            19-JUL-1996 incorporated in JULIA 282 after fixing bug 
C!                         when counting good mini-chunks; 
C!            22-AUG-1996 modified to run OBSPOT and save the BOM "alignement"
C!                         in first row of BLQP; allow DUMRUN to become
C!                         .FALSE. after is was set to .TRUE. (needed 
C!                         to run the code in ALPHA when there is a 
C!                         special record before the run  record, leading
C!                         to DUMRUN=.TRUE. because the run number is zero)
C?
C!======================================================================
#ifndef DOC
C
C***********************************************************************
C
C     =========================
C     Non-executable statements
C     =========================
C
      IMPLICIT NONE
C
C     Argument 
C
      INTEGER IFLAG
C
C     Externals
C
      INTEGER NAMIND,NLINK,INTCHA,NPRNT,ALSECS
      CHARACTER*4 CHAINT,NLIST
C
C     HAC parameter definitions
C
C#include "runrjj.h" ! the HAC parameters we need for RUNR are in runhjj.h !!!
#include "runhjj.h"
#include "blqpjj.h"
#include "evehjj.h"
#include "lxdajj.h"
#include "alpbjj.h"
C
C     Other parameters
C
      INTEGER LRINI
      PARAMETER(LRINI=25)
      INTEGER MCOUNT
      PARAMETER(MCOUNT=11)
      INTEGER LWCOL,LWROW
      PARAMETER(LWCOL=2,LWROW=100)
C
C     Non-static local variables
C
      CHARACTER*8 RNAME
      DATA RNAME/'OLSPOT'/
      INTEGER KOLSP
      INTEGER KRUNR,KRUNH,KBLQP,KEVEH,KLXHA,KLXDA
#ifdef ALP2
      INTEGER KALPB
#endif
      INTEGER ISTAT,IRUN,ERUN,LENGTH,IGARB,EVNT,EVNT2
      INTEGER DATE,TIME,SECS,IROW,I,K,IDATE
      LOGICAL MCRUN,ELIST
      INTEGER KDATA(2),KDATE(2),S(2),INDX
      CHARACTER*4 CDATA(2),CNAME
      CHARACTER*32 CHTEXT(MCOUNT)
      DATA CHTEXT/
     1    'runs',
     2    'runs, not dummy',
     3    'time (minutes)',
     4    'LEP slow control records',
     5    '          all mini-chunks',
     6    '          non-empty mini-chunks',
     7    '          good mini-chunks',
     8    'event records',
     9    'events in all mini-chunks',
     &    'events in non-empty mini-chunks',
     1    'events in good mini-chunks'/
C
C     Static local variables (i.e. saved variables)
C
      LOGICAL FIRST,DEBUG,DUMMY,DUMRUN
      SAVE    FIRST,DEBUG,DUMMY,DUMRUN
      DATA    FIRST,DEBUG,DUMMY,DUMRUN/.TRUE.,.FALSE.,.FALSE.,.TRUE./
      INTEGER IDUMRUN
      SAVE    IDUMRUN
      DATA    IDUMRUN/-1/
      INTEGER NARUNR,NARUNH,NABLQP,NAEVEH,NALXHA,NALXDA,NALXIP,NABOMQ
      SAVE    NARUNR,NARUNH,NABLQP,NAEVEH,NALXHA,NALXDA,NALXIP,NABOMQ
      DATA    NARUNR,NARUNH,NABLQP,NAEVEH,NALXHA,NALXDA,NALXIP,NABOMQ/
     &             0,     0,     0,     0,     0,     0,     0,     0/
      INTEGER COUNT(MCOUNT),COUTOT(MCOUNT)
      SAVE    COUNT,COUTOT
      DATA    COUNT,COUTOT/MCOUNT*0,MCOUNT*0/
      INTEGER LUNIT,FSECS,LSECS,FEVNT,LEVNT,SSECS
      SAVE    LUNIT,FSECS,LSECS,FEVNT,LEVNT,SSECS
      DATA    LUNIT,FSECS,LSECS,FEVNT,LEVNT,SSECS/0,0,0,0,0,0/
C
C     Common block to store index of work bank (required by BOS !)
C
      INTEGER KWORK
      COMMON/OLSP/KWORK
#ifndef ALP2
C
C     OBSPOT common block
C
#include "obscom.h"
#endif
C
C     BOS common block
C
#include "bcs.h"
C
C     Standard function definition statements
C
#include "bmacrod.h"
#include "bmacro.h"
C
C***********************************************************************
C
      WRITE(RNAME(7:8),'(SP,I2)',IOSTAT=ISTAT) IFLAG
C
C     ==================
C     Initialize routine
C     ==================
C
      IF(FIRST) THEN  
        FIRST=.FALSE.
C
C       Decode OLSP card (if present)
C
        KOLSP=IW(NAMIND('OLSP'))
        IF(KOLSP.GT.0) THEN 
          IF(IW(KOLSP).GT.0) THEN 
            DEBUG=IW(KOLSP+1).GT.0
            DUMMY=IW(KOLSP+1).LT.0
          ENDIF
          IF(IW(KOLSP).GE.2) LUNIT=IW(KOLSP+2)
        ENDIF
        IF(LUNIT.LE.0) LUNIT=IW(6)
        IF(KOLSP.GT.0) WRITE(IW(6),7000) RNAME,DUMMY,DEBUG,LUNIT
        IF(DUMMY) GOTO 9003
C
C       Get name index of each bank used by this routine
C
        NARUNR=NAMIND('RUNR')
        NARUNH=NAMIND('RUNH')
        NABLQP=NAMIND('BLQP')
        NAEVEH=NAMIND('EVEH')
        NALXHA=NAMIND('LXHA')
        NALXDA=NAMIND('LXDA')
        NALXIP=NAMIND('LXIP')
        NABOMQ=NAMIND('BOMQ')
C
C       Create work bank
C
        KWORK=0
        LENGTH=LMHLEN+LWROW*LWCOL
        CALL WBANK(IW,KWORK,LENGTH,*9002)
        IW(KWORK-3)=INTCHA('WOLS')
        IW(KWORK+LMHCOL)=LWCOL
        IW(KWORK+LMHROW)=0
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'work bank created, nrow = ',LWROW
      ENDIF
C
C***********************************************************************
C
C     =====================
C     Decide what to do ...
C     =====================
C
C     Act as dummy routine if requested by OLSP card
C
      IF(DUMMY) RETURN
C
C     Get run number
C
      KRUNR=IW(NARUNR)
      IF(KRUNR.LE.0) KRUNR=IW(NARUNH)
      IF(KRUNR.LE.0) THEN 
        CALL RERROR(RNAME,-1,'Missing RUNR/RUNH banks; run set to 0')
        IF(DEBUG) WRITE(LUNIT,3000) RNAME,'RUNR/RUNH'
        IRUN=0
      ELSE
        IRUN=IW(KRUNR+JRUNRN)
      ENDIF
C
C     Create empty BLQP bank for this run
C
      IF(COUNT(1).LE.0.OR.(DUMRUN.AND.IRUN.NE.IDUMRUN)) GOTO 1
C
C     Do nothing if BLQP bank not properly created for this run
C
      IF(DUMRUN) RETURN
C
C     Make sure that BLQP bank still exists for this run
C
      KBLQP=IW(NABLQP)
      IF(KBLQP.LE.0) THEN 
        CALL RERROR(RNAME,-2,'Missing BLQP bank')
        IF(DEBUG) WRITE(LUNIT,3000) RNAME,'BLQP'
        RETURN
      ENDIF
      IF(IRUN.NE.IW(KBLQP-2)) THEN 
        CALL RERROR(RNAME,-3,'run and BLQP bank number mismatch')
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'run and BLQP bank number mismatch',IRUN,IW(KBLQP-2)
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'start recovery procedure for end of run ',IW(KBLQP-2)
        GOTO 3
      ENDIF
C
C     Fill BLQP bank
C
      GOTO (2,3,4) IFLAG+1
C
C     Invalid argument
C
      CALL RERROR(RNAME,-4,'Routine called with invalid argument')
      IF(DEBUG) WRITE(LUNIT,2000) RNAME,'invalid argument',IFLAG
      RETURN
C
C***********************************************************************
C
    1 CONTINUE
C
C     =======================
C     Start of run processing
C     =======================
C
      IF(DEBUG) WRITE(LUNIT,2000) RNAME,'start of run',IRUN
      COUNT(1)=COUNT(1)+1
C
C     Reset work bank
C
      IW(KWORK+LMHROW)=0
C
C     Is this a Monte Carlo run ?
C
      MCRUN=IRUN.LE.2000.OR.IW(NAMIND('AJOB')).NE.0
      IF(MCRUN.AND.DEBUG) WRITE(LUNIT,1000) RNAME,
     &                    'this is a Monte Carlo run'
      DUMRUN=MCRUN
      IF(.NOT.DUMRUN) THEN 
C
C       Create BLQP bank for this run
C
        CALL BDROP(IW,'BLQP')
        LENGTH=LMHLEN+LRINI*LBLQPA
        CALL AUBOS('BLQP',IRUN,LENGTH,KBLQP,IGARB)
        IF(IGARB.EQ.2) THEN 
          CALL RERROR(RNAME,-5,'No space to create BLQP bank')
          IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &      'no space to create BLQP bank'
          CALL BDROP(IW,'BLQP')
          DUMRUN=.TRUE.
        ELSE
          IF(IGARB.EQ.1.AND.DEBUG) WRITE(LUNIT,1000)
     &     'garbage collection occurred'
          IW(KBLQP+LMHCOL)=LBLQPA
          IW(KBLQP+LMHROW)=0
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &      'bank BLQP created for run ',IRUN
        ENDIF
C
C       Get date and time of start of run 
C
        KRUNH=IW(NARUNH)
        IF(KRUNH.LE.0) THEN 
          CALL RERROR(RNAME,-6,
     &     'Missing RUNH bank; start of run time set to 0')
          IF(DEBUG) WRITE(LUNIT,3000) RNAME,'RUNH'
          FSECS=0
        ELSE IF(IRUN.NE.IW(KRUNH+JRUNRN)) THEN 
          CALL RERROR(RNAME,-7,
     &     'Bad run number in RUNH bank; start of run time set to 0')
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &     'bad run number in RUNH = ',IW(KRUNH+JRUNRN)
          FSECS=0
        ELSE
          DATE=IW(KRUNH+JRUNSD)
          TIME=IW(KRUNH+JRUNST)
          FSECS=ALSECS(DATE,TIME)
          IF(FSECS.LT.0) THEN 
            CALL RERROR(RNAME,8,
     &       'Error returned by ALSECS; start of run time set to 0')
            IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &                'error returned by ALSECS',FSECS
            FSECS=0
          ENDIF
          IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &     'start of run date and time: ',DATE,TIME,FSECS
        ENDIF
        LSECS=FSECS
        FEVNT=0
        LEVNT=FEVNT
        SSECS=0
      ENDIF
C
C     Print result of start of run processing
C
      IF(DUMRUN) THEN 
        IDUMRUN=IRUN
        IF(DEBUG) THEN 
          WRITE(LUNIT,1000) RNAME,'start of run processing failed'
          WRITE(LUNIT,1000) RNAME,'nothing will be done for this run !'
        ENDIF
        RETURN
      ENDIF
      IDUMRUN=-1
      IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &          'start of run processing successful'
      COUNT(2)=COUNT(2)+1
      CALL OBSINR
C
C***********************************************************************
C
    2 CONTINUE
C
C     ====================
C     Process event record
C     ====================
C
      KEVEH=IW(NAEVEH)
      IF(KEVEH.GT.0) THEN 
        COUNT(8)=COUNT(8)+1
        CALL OBSPOT
        EVNT=IW(KEVEH+JEVEEV)
        ERUN=IW(KEVEH+JEVERN)
C
C       Check run number 
C
        IF(IRUN.NE.ERUN) THEN 
          CALL RERROR(RNAME,9,'Event with wrong run number')
          IF(DEBUG) THEN 
            WRITE(LUNIT,2000) RNAME,
     &       'event with wrong run number; run, event = ',ERUN,EVNT
            WRITE(LUNIT,1000) ' ','this event ignored !'
          ENDIF
          RETURN
        ENDIF
C
C       Get date and time of event
C
        DATE=IW(KEVEH+JEVEDA)
        TIME=IW(KEVEH+JEVETI)
        SECS=ALSECS(DATE,TIME/100)
        IF(SECS.LT.0) THEN 
          CALL RERROR(RNAME,10,'Error returned by ALSECS')
          IF(DEBUG) THEN 
            WRITE(LUNIT,2000) RNAME,'error returned by ALSECS',SECS
            WRITE(LUNIT,2000) ' ','run, event, date, time = ',
     &                             ERUN,EVNT,DATE,TIME
            WRITE(LUNIT,1000) ' ','this event ignored !'
          ENDIF
          RETURN
        ENDIF
        IF(MOD(TIME,100).GE.50) SECS=SECS+1
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'run, event, date, time, secs = ',ERUN,EVNT,DATE,TIME,SECS
        IF(FEVNT.EQ.0) FEVNT=EVNT
        LSECS=SECS
        LEVNT=EVNT
C
C       Store info in work bank
C
        I=LROWS(KWORK)
        IF(I.LT.2.OR.ITABL(KWORK,I  ,2).NE.SECS
     &           .OR.ITABL(KWORK,I-1,2).NE.SECS) THEN 
          IF(LFRROW(KWORK).LT.1) THEN
            LENGTH=IW(KWORK)+LWROW*LCOLS(KWORK)
            CALL WBANK(IW,KWORK,LENGTH,*9001)
            IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &       'work bank extended, nrow = ',LROWS(KWORK)
          ENDIF
          IW(KNEXT(KWORK)+2)=SECS
          IW(KWORK+LMHROW)=IW(KWORK+LMHROW)+1
        ENDIF
        IW(KROW(KWORK,LROWS(KWORK))+1)=EVNT
      ENDIF
C
C     ===========================
C     Process slow control record
C     ===========================
C
      KDATA(1)=IW(NALXIP)
      KDATA(2)=IW(NABOMQ)
      KDATE(1)=IW(NALXDA)
      KDATE(2)=IW(NALXHA)
C
C     Loop on BOMQ and LXIP banks
C
      DO K=1,2
        S(K)=0
        IF(KDATA(K).GT.0) THEN 
          CDATA(K)=CHAINT(IW(KDATA(K)-3))
C
C         Find date and time of BOMQ or LXIP bank
C
          DATE=0
          TIME=0
          INDX=0
          DO IDATE=1,2
           IF(KDATE(IDATE).GT.0) THEN 
            DO I=1,LROWS(KDATE(IDATE))
             IF(CHAINT(ITABL(KDATE(IDATE),I,JLXDBA)).EQ.CDATA(K)) THEN 
              INDX=KROW(KDATE(IDATE),I)
              GOTO 23
             ENDIF
            ENDDO
           ENDIF
          ENDDO
   23     CONTINUE
          IF(INDX.LE.0) THEN
            CALL RERROR(RNAME,11,'LEP record without '//CDATA(K)//
     &                          ' time in LXDA/LXHA')
            IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &       'LEP record without '//CDATA(K)//' time in LXDA/LXHA'
            S(K)=0
          ELSE 
            DATE=IW(INDX+JLXDDA)
            TIME=IW(INDX+JLXDDA+1)
            S(K)=ALSECS(DATE,TIME)
            IF(S(K).LT.0) THEN
              CALL RERROR(RNAME,12,'error returned by ALSECS')
              IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &                  'error returned by ALSECS',S(K)
              S(K)=0
            ENDIF
          ENDIF
          IF(DEBUG) THEN 
            IF(IDATE.LE.2) THEN 
              CNAME=CHAINT(IW(KDATE(IDATE)-3))
            ELSE
              CNAME=' '
            ENDIF
            WRITE(LUNIT,2000) RNAME,
     &      'data_bank, date_bank, date, time, secs = '//
     &      CDATA(K)//' '//CNAME,DATE,TIME,S(K)
          ENDIF
C
C         Ignore BOMQ or LXIP data if data/time is invalid 
C         or older than start of run by 60 seconds. 
C
          IF(S(K).LT.FSECS-60) THEN 
            IF(S(K).GT.0.AND.DEBUG) WRITE(LUNIT,1000) RNAME,
     &       'bank '//CDATA(K)//' too old ! Ignored ...'
            S(K)=0
          ENDIF
        ENDIF
      ENDDO
C
C     Synchronize BOMQ and LXIP data, i.e. ignore the oldest bank
C     if they don't have the same age within 60 seconds.
C
      SECS=MAX0(S(1),S(2))
      DO K=1,2
        IF(SECS-S(K).GT.60) THEN 
          IF(S(K).GT.0.AND.DEBUG) WRITE(LUNIT,1000) RNAME,
     &       'bank '//CDATA(K)//' out of time ! Ignored ...'
          S(K)=0
        ENDIF
      ENDDO
C
C     Add data to BLQP bank
C
      IF(KDATA(1).GT.0.OR.KDATA(2).GT.0) THEN
        IF(SECS.LE.SSECS) THEN 
          IF(DEBUG.AND.SECS.NE.0) WRITE(LUNIT,1000) RNAME,
     &     'LXIP/BOMQ data not new ! Ignored ...'
        ELSE
          IF(LROWS(KWORK).EQ.0.AND.LROWS(KBLQP).LE.2) THEN
            IW(KROW(KWORK,1)+1)=1
            IW(KROW(KWORK,1)+2)=SECS
            IW(KWORK+LMHROW)=1
          ENDIF
          COUNT(4)=COUNT(4)+1
          CALL OLSPOU(KBLQP,KWORK,S(1).NE.0,S(2).NE.0,SECS,DEBUG,LUNIT)
          SSECS=SECS
        ENDIF
      ENDIF
      RETURN
C
C***********************************************************************
C
    3 CONTINUE
C
C     =====================
C     End of run processing
C     =====================
C
      IF(FEVNT.NE.0) THEN 
        COUNT(3)=COUNT(3)+NINT(FLOAT(LSECS-FSECS)/60.)
        COUNT(9)=COUNT(9)+LEVNT-FEVNT+1
      ENDIF
C
C     Add final empty row to BLQP, if necessary
C
      CALL OLSPOU(KBLQP,KWORK,.FALSE.,.FALSE.,0,DEBUG,LUNIT)
C
C     Resize BLQP bank
C
      LENGTH=LMHLEN+LROWS(KBLQP)*LCOLS(KBLQP)
      CALL AUBOS('BLQP',IW(KBLQP-2),LENGTH,KBLQP,IGARB)
      IF(IGARB.EQ.2) THEN 
        CALL RERROR(RNAME,13,'Cannot resize BLQP bank')
        IF(DEBUG) WRITE(LUNIT,1000) RNAME,'Cannot resize BLQP bank'
        DO I=LROWS(KBLQP)+1,(IW(KBLQP)-LMHLEN)/LCOLS(KBLQP)
          IW(KROW(KBLQP,I)+JBLQFE)=LEVNT*10
        ENDDO
      ELSE IF(DEBUG) THEN 
        IF(IGARB.EQ.1) WRITE(LUNIT,1000)
     &   'garbage collection occurred'
        WRITE(LUNIT,2000) RNAME,'bank BLQP resized for run ',IW(KBLQP-2)
      ENDIF
      IF(DEBUG) LENGTH=NPRNT('BLQP',IW(KBLQP-2))
C
C  Compress BLQP bank (c.f. ALPB in VBSCLR)  M.C. 20/6/96
C
      CALL AUBPRS('BLQP')
C
C     Perform fit with tracks to determine the BOM "alignement"; 
C     save results of fit in first row of BLQP
C
      CALL OBSCLR
#ifdef ALP2
      KALPB=NLINK('ALP2',IW(KBLQP-2))
#endif
      IF(KALPB.GT.0) THEN
#ifdef ALP2
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'Bank ALP2 created for run ',IW(KBLQP-2)
#else
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'Work bank ALP2 created for run ',IW(KBLQP-2)
#endif
        IF(LROWS(KALPB).GT.0.AND.LROWS(KBLQP).GT.0) THEN
          IF(ITABL(KBLQP,1,JBLQFE).LE.0) THEN
            RW(KBLQP+LMHLEN+JBLQBO  )=
     &        10000.*BSPFAC*FLOAT(ITABL(KALPB,1,JALPXP))
            RW(KBLQP+LMHLEN+JBLQBO+1)=
     &        10000.*BSPFAC*FLOAT(ITABL(KALPB,1,JALPYP))
            RW(KBLQP+LMHLEN+JBLQBO+2)=
     &        10000.*BSEFAC*FLOAT(ITABL(KALPB,1,JALPXE))
            RW(KBLQP+LMHLEN+JBLQBO+3)=
     &        10000.*BSEFAC*FLOAT(ITABL(KALPB,1,JALPYE))
#ifdef ALP2
            IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &       'Content of ALP2 bank copied into first row of BLQP'
#else
            IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &       'Content of ALP2 work bank copied into first row of BLQP'
#endif
          ENDIF
        ENDIF
#ifdef ALP2
CCC     CALL BDROP(IW,'ALP2')
#else
        CALL WDROP(IW,KALPB)
#endif
      ELSE
#ifdef ALP2
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'No ALP2 bank created for run ',IW(KBLQP-2)
#else
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'No ALP2 work bank created for run ',IW(KBLQP-2)
#endif
        CALL RERROR(RNAME,-14,'Cannot determine BOM+QS0 alignment')
      ENDIF
C
C     Fast analysis of BLQP bank
C
      DO I=2,LROWS(KBLQP)
        COUNT(5)=COUNT(5)+1
        EVNT=ITABL(KBLQP,I,JBLQFE)
        IF(I.LT.LROWS(KBLQP)) THEN 
          EVNT2=ITABL(KBLQP,I+1,JBLQFE)
        ELSE
          EVNT2=LEVNT+1
        ENDIF
        ISTAT=IOR(ITABL(KBLQP,I,JBLQSF),ITABL(KBLQP,I,JBLQSF+1))
        IF(ISTAT.NE.('FFFFFFFF'X)) THEN 
          COUNT(6)=COUNT(6)+1
          COUNT(10)=COUNT(10)+EVNT2-EVNT
          IF(IAND(ISTAT,'FFFFFF00'X).EQ.0) THEN 
            COUNT(7)=COUNT(7)+1
            COUNT(11)=COUNT(11)+EVNT2-EVNT
          ENDIF
        ENDIF
      ENDDO
C
C     Print statistics
C
      DO I=1,MCOUNT
        COUTOT(I)=COUTOT(I)+COUNT(I)
      ENDDO
      WRITE(LUNIT,8000) RNAME,IW(KBLQP-2),
     & (CHTEXT(I),COUNT(I),COUTOT(I),I=1,MCOUNT)
C**      CALL PRINT_BLQP(LUNIT)
C
C     Zero all run counters
C
      CALL UZERO(COUNT,1,MCOUNT)
      IF(IRUN.NE.IW(KBLQP-2)) THEN 
        IF(DEBUG) WRITE(LUNIT,2000) RNAME,
     &   'end of recovery procedure for end of run ',IW(KBLQP-2)
        GOTO 1
      ENDIF
    4 RETURN
C
C***********************************************************************
C
C     ============
C     WBANK errors
C     ============
C
 9001 CALL RERROR(RNAME,-15,'Insufficient space to extend work bank')
      IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &   'insufficient space to extend work bank'
      RETURN
 9002 CALL RERROR(RNAME,-16,'Insufficient space to create work bank')
      IF(DEBUG) WRITE(LUNIT,1000) RNAME,
     &   'insufficient space to create work bank'
      DUMMY=.TRUE.
 9003 CALL RERROR(RNAME,17,'Routine will act as dummy')
      IF(DEBUG) WRITE(LUNIT,1000) RNAME,'Routine will act as dummy'
      RETURN
C
C***********************************************************************
C
C     =======
C     Formats
C     =======
C
 1000 FORMAT(1X,A,': ',A)
 2000 FORMAT(1X,A,': ',A,1X,5I10)
 3000 FORMAT(1X,A,': missing ',A,' bank')
 7000 FORMAT(1X,A,': Interpretation of OLSP card:',/,
     &       9X,'- dummy mode            = ',L4,/,
     &       9X,'- debug mode            = ',L4,/,
     &       9X,'- printout unit         = ',I4)
 8000 FORMAT(1X,A,': end of run statistics:',7X,
     &       'run',I7,'    all runs',/,(1X,A,3X,2I12))
      END
#endif
