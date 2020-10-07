      SUBROUTINE QMRDSB(IEND)
CKEY EVENT /INTERNAL
C----------------------------------------------------------------------
C! read next event.
C  called from QMREAD
C                                        Author:    H.Albrecht 20.09.88
C                                        Modified:  E.Blucher  01.06.89
C                                        Modified:  M.Talby    17.07.89
C                                        Modified:  E.Blucher  27.07.90
C                                        Modified:  G.Graefe   13.07.94
C                                        Modified:  T.Oest     26.07.95
C                                        Modified:  I.Tomalin  14.05.97 
C                                        Modified:  J.Boucrot  21.06.01
C----------------------------------------------------------------------
#ifndef DOC
      SAVE LNEWR,IROLD,IEOLD
C
      DIMENSION LINDAT(2),LUTDAT(5)
      DIMENSION IPV(10),IAV(10)
#include "qcdesh.h"
      INTEGER BKINCA
      LOGICAL LNEWR
      LOGICAL CHKCLAS
      CHARACTER * 50 MESS
      COMMON / ALFORMA / LUFORM,IFALARM
C
      DATA LNEWR /.TRUE./, IROLD /0/, IEOLD /0/
C----------------------------------------------------------------------
      IEND=0
C
C       get next record
C
 1    CONTINUE
      KCLASW = 0
      CALL ABRSEL ('E', CQUNPK, IRET)
C       get record class word if read from EDIR
      CALL ABGTCL ( KCLASW )
C
C       determine whether we are reading a Mini or a Nano , or not :
C
      IF (IRET.LE.2) THEN
         CALL ALVSN(ITYP,IPV,IAV,IYR)
         INDATA=ITYP
         IF (ITYP.EQ.5) THEN
            XMINI = .TRUE.
         ELSE
            XMINI = .FALSE.
         ENDIF
         IF (ITYP.EQ.7) THEN
            XNANO = .TRUE.
         ELSE
            XNANO = .FALSE.
         ENDIF
      ELSE
         INDATA = 0
      ENDIF
C
C If one reads a MINI and if the BOS banks format reading was not OK ,
C one must stop the job :
C
      IF (XMINI.AND.IFALARM.NE.0) THEN
         CALL QMTERM
     +   ('_QMRDSB_ Error in BOS formats when reading a MINI')
         GO TO 900
      ENDIF
C
C       clear ALPHA banks
C
      IF (.NOT. XCOPYJ)  CALL QMCLR
C
      GO TO ( 10, 20, 30,  1, 50, 60, 70, 80, 90,130,
     +       110,120,130,140,150,160,170,180,190,200,210), IRET
C----------------------------------------------------------------------
C       event record
C
 10   CONTINUE
      IF (KCLASW.EQ.0) THEN
C       do NOT check class if only 1 bank (laser event)
C
        IF (IGTLEN(IW,'E') .GT. 1) THEN
C         class word not read from EDIR , get it from REVH bank
C         skip the event if not selected
C         Don't check for class if input is a NanoDst
          IF (.NOT.CHKCLAS(KCLASW).AND..NOT.XNANO) GO TO 1
        ELSE
C         skip empty events if specific edir classes requested.   
          CALL ABGTRCL (MASKR)        
          IF (MASKR.NE.0) GO TO 1
        ENDIF
      ENDIF
C
      KNREIN = KNREIN + 1
      KFFILL = 0
      KNEVT = KNEVT + 1
      KNEFIL = KNEFIL + 1
      XWREVT = .FALSE.
      CALL ABRUEV (KRUN, KEVT)
      CALL ALEVNUM(1,KRUN,KEVT)
      IF (IROLD - KRUN)  12, 13, 11
C
 11   IF (.NOT.XNSEQ) CALL QWMESE
     +  ('_QMREAD_ Run numbers on input file not in increasing order')
 12   IF (.NOT. LNEWR)
     +  CALL QWMESE ('_QMREAD_ New run - no run record found')
      CALL ABOLDR (-KRUN)
      LNEWR = .FALSE.
      IROLD = KRUN
      IEOLD = 0
      GO TO 19
 13   IF (LNEWR)  THEN
C         run record was preceded by events of the same run :
        CALL QMNEWR (KRUN, KRUN)
        LNEWR = .FALSE.
      ENDIF
      IF (IEOLD - KEVT)  19, 14, 15
 14   MESS = 'Same run/event number as previous event. Skipped'
      GO TO 400
 15   IF(.NOT.XNSEQ)CALL QWMESE
     +  ('_QMREAD_ Event numbers on input file not increasing order')
 19   IF (KNEFIL .EQ. 1)  CALL QWMESE
     +    ('_QMREAD_ First selected event on input file ')
#if defined(UNIX)
C_PVM
C
C    PVM: send new event to slave
C
      IEOLD = KEVT
      CALL QPVMES(KRUN,KEVT,IRET,IEND)
C_PVM
#endif
      GO TO 900
C----------------------------------------------------------------------
C       run record
C
 20   KNREIN = KNREIN + 1
C
C       special treatment of run number = infinity
C       remove if run numbers on POT are ok.
C       In future, these run numbers may be ok !
C
      CALL ABRUEV (I, DUMMY)
      IF (I .GT. 99999)  THEN
        CALL QWMESE ('_QMREAD_ New run number > 99999')
        GO TO 1
      ENDIF
      KRUN = I
C       Replace the previous statements by
C...  CALL ABRUEV (KRUN, DUMMY)
      IF (KRUN .NE. IROLD)  KEVT = 0
      LNEWR = .TRUE.
C
      XWRRUN = .FALSE.
      IF (KFEOUT .EQ. 2)  XWRRUN = .TRUE.
      IF (KFEOUT .EQ. 0)  CALL QWRUNR
C
      IF (IROLD.GT.KRUN.AND.(.NOT.XNSEQ))  CALL QWMESE
     +  ('_QMREAD_ Run numbers on input file not in increasing order')
      GO TO 1
C----------------------------------------------------------------------
C       special records
C
 30   KNREIN = KNREIN + 1
      KEVT = 0
      XWREVT = .FALSE.
      CALL QMSREC
      IF (.NOT. XSREC)  GO TO 1
C       write special records only if the current run is selected
      CALL ABSEVT (KRUN, 0, IRET)
      IF (IRET .NE. 1)  GO TO 1
      CALL QWRITE
      CALL ABCLAS (-1)
      GO TO 1
C
C       end of file
C
 50   CONTINUE
      KNEFIL = 0
      KNREIN = 0
      GO TO 1
C
C       normal end of job
C
 60   MESS = 'All event input files are processed'
      GO TO 300
 70   MESS = 'NEVT card : all events processed'
      GO TO 300
 80   IF(XNSEQ)GOTO 1
      MESS = 'SEVT / SRUN card : all events processed'
      GO TO 300
 90   MESS = 'Time Limit Reached'
      GO TO 300
 110  MESS = 'Error in SEVT,NEVT,SRUN,IRUN,TIME, or CLAS card'
      GO TO 300
 120  MESS = 'Read error in event directory'
      GO TO 300
 130  MESS = 'Error in FILI cards'
      GO TO 300
 140  MESS = 'Input file cannot be opened'
      GO TO 300
 150  MESS = 'Error in FILO cards'
      GO TO 300
 160  MESS = 'Output file cannot be opened'
      GO TO 300
C----------------------------------------------------------------------
C       error conditions :
C
 170  MESS = 'Read error. Record skipped.'
      GO TO 400
 180  MESS = 'Error on decompressing. Record skipped.'
      GO TO 400
 190  MESS = 'Not enough space for unpacking. Record skipped.'
      GO TO 400
 200  MESS = 'TPC unpacking error. Record skipped.'
      GO TO 400
 210  MESS = 'Error in data base reading. Record skipped.'
      GO TO 400
C
 300  CALL QWMESE ('0_QMREAD_ ' // MESS)
      IEND=1
      GOTO 900
C
 400  CALL QWMESE ('_QMREAD_ ' // MESS)
      GO TO 1
C
 900  CONTINUE
#if defined(UNIX)
C_PVM
C
C    GOTO 1, IF PVM MASTER PROCESS
C
      IF (IEND.EQ.0) CALL QPVMGT(*1)
C_PVM
#endif
      END
#endif
