      SUBROUTINE RLOOPR
C----------------------------------------------------------------------
C! Loop on records
C!
C!   Author        S.Orteu, J.Knobloch
C!   Modified      Z. Qian              15-jul-1989
C!   Modified:   H. Meinhard       24-Aug-1989
C!   Modified :- E. Lancon              5-FEB-1992
C!                Use ABRSEL routine from alephlib to read events
C!               F.Ranjard             28-APR-1994
C!                create XTCN bank from X1RG bank
C!               P.Comas               25-AUG-1994
C!                Only the EVEH of events with DAQ error as evtype
C!                (998) is passed on to the POT. These events are
C!                not reconstructed
C!               P.Comas               8-MAY-1995
C!                Write bunch number used for reconstruction in EVEH
C!               P.Comas               8-AUG-1996
C!                Filter out TPC laser events writing only the EVEH
C!               M.Cattaneo            5-DEC-1997
C!                Fix format statement when event length > 1Mbyte
C!               M.Cattaneo           27-JUL-1998
C!                Print also processing time of previous event
C!               M.Cattaneo           14-SEP-1998
C!                Modify format statement
C!               B.Bloch              may 2001 
C!                add call ALEVNUM for official production monitoring
C?
C!======================================================================
#ifndef DOC
      CHARACTER*50 BID
#include "bcs.h"
#include "rcurnt.h"
#include "revent.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "rtimes.h"
#include "evehjj.h"
#include "runhjj.h"
#include "jestjj.h"
#include "jcberr.h"
      CHARACTER*1  CLIST
      CHARACTER*80  OUNPK
      CHARACTER*60 MESSAGE
      LOGICAL FIRST
      LOGICAL RMOD
      DATA FIRST / .TRUE. /
      DATA NUMRU / 0 /
#include "bmacro.h"
C
C?   unpack every thing execpt HCAL and MUONS in case of reprocessing
C?   (the 2 alephlib routines give problem, for hcal the unpacking is
C?   redone in HPREDA, for the muons there is no packing anyway).
C
      OUNPK = ' '
      IF ( FREPRO )
     &        OUNPK = 'VD IT TP TE EC LC SA FI CR SO'
      IREAD = 0
  111 CONTINUE
C
C     This is the loop over RECORDS within a file
C
      CALL RNXMOD(MODREA,RMOD)
C
      CALL ABRSEL ( 'E', OUNPK, IRET)

C
C?   Get Input and Output unit numbers
C?   ABRSEL MUST have been called once !
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL ABUNIT (JULIN, IDUM1, IDUM2, JULOU, IDUM3)
        IF ( JULIN.EQ.0 ) THEN
          MESSAGE = ' Error in I/O Definition '
          GOTO 700
        ENDIF
      ENDIF
C
      CLIST = 'E'
      IF ( IRET.EQ.2 ) CLIST = 'R'
      SIZERE = 4./1000.* FLOAT( LENLIS( CLIST ) )

      GOTO ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
     &  18, 19, 20, 21) IRET


C
C?   ----->   E V E N T record
C
    1 CONTINUE
      IREAD = IREAD + 1
      IND = IW(NAMIND('EVEH'))
      NUMRU = IW(IND+JEVERN)
      NUMEV = IW(IND+JEVEEV)
C Set debug flag
      FDEBRF = IREAD.GE.IDEBBC.AND.IREAD.LE.IDEBTC
      IF (.NOT.INIRUC) THEN
C Check that some constants are there.
C If not, set run number to the number from event header.
        IF (IRUNRC.EQ.0) THEN
          IF (FDEBRF) THEN
            WRITE(LOUTRL,'(A)')     '0RLOOPR- No run number defined'
            WRITE(LOUTRL,'(A,I6)')
     *                              ' RLOOPR- Run number set to ',NUMRU
            IRUNRC = NUMRU
          ENDIF
        ENDIF
C Run not initialised
        CALL RINRUN
        INIRUC = .TRUE.
      ENDIF
      ISQNRC = ISQNRC + 1
C Check data consistency
      IEVTRC = NUMEV
      IF (IRUNRC.NE.NUMRU) CALL REJECT(NUMRU,NUMEV)
      IETYP=0
      KEVEH=IW(NAMIND('EVEH'))
      IETYP=IW(KEVEH+JEVEES)
      ILASE=IW(KEVEH+JEVEM1)
      AEVTIM=AEVFRT-AEVSRT
      IF (FDEBRF.AND.JDBDRF(JULSK).GE.2)
     *          WRITE(LOUTRL,'(A,I6,A,I6,A,F7.2,A,I5,A,I4,A,F6.2)')
     *          ' RLOOPR-Evt',NUMEV,' Run',NUMRU,' ',
     *          MAX(SIZERE,0.),' Kb. Nr.in job=',IREAD,' Evtyp',
     *          IETYP,' Time(s):',AEVTIM
        js = 3
        call alevnum(js,numru,numev)
C       in case of event records look for "non physical events"
C       type=128  pause_event
C       type=129  pause_event_copy
C       type=130  last_event (in a run)
C       type=131  l3_reject
C       type=132  change_event (last_event in a run -fast run change)
C       these records will be passed on to the POT without any processin
C       type>990  DAQ error (type=998 'real' DAQ error)
C
      IF(IETYP.GE.128.AND.IETYP.LE.132)THEN
C                level 3 reject
        DO 112 I=1,NWRTMX
          IF(JOUTRL(I).NE.NOOUTP) CALL ABWSEL ('E')
  112   CONTINUE
C
      ELSE IF( (IETYP.EQ.998).OR.(BTEST(ILASE,23)) )THEN
        CALL RSHRIN(IW,'E',TEMLIS)
        L = LNBLNK(TEMLIS)
        CALL BDROP(IW,TEMLIS(5:L))
        CALL BLIST(IW,'E=','EVEH')
        DO 113 I=1,NWRTMX
          IF(JOUTRL(I).NE.NOOUTP) CALL ABWSEL ('E')
  113   CONTINUE
C
      ELSE
C
C build XTCN trigger from X1RG trigger bank when X1RG exists and XTCN
C does not exist
        CALL XTCNCR

C
C Write bunch number for reconstruction in word 10 of EVEH starting
C from '95 data (run > 35000)
        CALL RWJUBW

C
C Reconstruct the event
        CALL RECONS
C
      ENDIF
C
      GOTO 111
C
C?   ----->     R U N record
C
    2 CONTINUE
C
      IND = IW(3)
      NUMRU = IW(IND+JRUNRN)
      IF (INIRUC .AND. NUMRU.NE.IRUNRC) THEN
        INIRUC = .FALSE.
      ENDIF
      IRUNRC = NUMRU
C-----------------------------------------------------------------------
C         drop some banks from the run header to avoid that they go
C         to the POT
      CALL BDROP(IW,'HPB1HPB2HPEAHPEB')
C-  TDVV from run header as nr = 0
      IDROP = NDROP ('TDVV',0)
C-  LDST from run header as nr = 1
      IF(IRUNRC.LT.14000) THEN
        IDROP = NDROP ('LDST',1)
      ENDIF
C-----------------------------------------------------------------------
      IF (FDEBRF.AND.JDBDRF(JULSK).GE.1)
     *        WRITE(LOUTRL,'(A,I6,A,F6.2,A)')
     *        '0RLOOPR- Run ',NUMRU,' found ',SIZERE,' Kbytes read.'
C
C?   If Run already initialized write out the record
C?   if run is not initialized the record will be written out after
C?   run initilisation (in RINRUN)
C
      IF ( INIRUC ) CALL ABWSEL ('R')
C
      GOTO 111
C
C?   It is an U N K N O W N type (nowadays called slow control) record?
C?   Write it out only if one run record has been read in (numru <> 0)
C
    3 CONTINUE
      IF (FDEBRF.AND.JDBDRF(JULSK).GE.1)
     *        WRITE(LOUTRL,'(A,F6.2,A)')
     *        '0RLOOPR- Unknown Record found ',SIZERE,' Kbytes read.'
      IF ( NUMRU.NE.0 ) CALL RUNKNO
      GOTO 111
C
C?   Future application
C
    4 CONTINUE
      GOTO 111
C
C?   End of file condition, next input file is now opened
C
    5 CONTINUE
      GOTO 111
C
C?   No more input files
C
    6 CONTINUE
      MESSAGE = 'No more input files'
      GOTO 700
C
C?   NEVT limit reached
C
    7 CONTINUE
      MESSAGE = 'NEVT limit reached'
      GOTO 700
C
C?   End of selected data according to SEVT/SRUN cards
C
    8 CONTINUE
      MESSAGE = 'End of selected data according to SEVT/SRUN cards'
      GOTO 700
C
C?   Time limit reached
C
    9 CONTINUE
      MESSAGE = 'Time limit reached'
      GOTO 700
C
C?   Input file not properly opened
C
   10 CONTINUE
      MESSAGE = 'Input file not properly opened'
      GOTO 700
C
C?   Syntax error in SEVT, SRUN, IRUN, NEVT, TIME, CLAS cards
C
   11 CONTINUE
      MESSAGE =
     &    'Syntax error in SEVT, SRUN, IRUN, NEVT, TIME, CLAS cards'
      GOTO 700
C
C?   Read error on event directory
C
   12 CONTINUE
      MESSAGE = 'Read error on event directory'
      GOTO 700
C
C?   Error in FILI cards
C
   13 CONTINUE
      MESSAGE = 'Error in FILI cards'
      GOTO 700
C
C?   Cannot open input file
C
   14 CONTINUE
      MESSAGE = 'Cannot open input file'
      GOTO 700
C
C?   Error on FILO card
C
   15 CONTINUE
      MESSAGE = 'Error on FILO card'
      GOTO 700
C
C?   Cannot open output file
C
   16 CONTINUE
      MESSAGE = 'Cannot open output file'
      GOTO 700
C
C?   17 ... 21 : Error conditions. Call ABRREC again.
C
C
C?   Read error
C
   17 CONTINUE
      MESSAGE = 'Read error'
      GOTO 800
C
C?   Error in decompressing
C
   18 CONTINUE
      MESSAGE = 'Error in decompressing'
      GOTO 800
C
C?   Not enough space for unpacking
C
   19 CONTINUE
      MESSAGE = 'Not enough space for unpacking'
      GOTO 800
C
C?   Error in TPC unpacking
C
   20 CONTINUE
      MESSAGE = 'Error in TPC unpacking'
      GOTO 800
C
C?   Error during data base reading
C
   21 CONTINUE
      MESSAGE = 'Error during data base reading'
      GOTO 800
C
C?   Fatal Error
C
  700 CONTINUE
      WRITE (LOUTRL,'(2a)') ' RLOOPR - ', MESSAGE
      GOTO 999
C
C?   Recoverable Error
C
  800 CONTINUE
      WRITE (LOUTRL,'(//,2a)') ' RLOOPR - ', MESSAGE
      GOTO 111
C
C?     ____________
C?                |
C?                | The Way Out
C?                |
C?                V
C?
C
  999 CONTINUE
C
C?   Close the run if it was open
C
      IF (INIRUC) CALL RCLRUN
C
      IW(1) = 0
      RETURN
      END
#endif
