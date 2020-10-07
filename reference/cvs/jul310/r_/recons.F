      SUBROUTINE RECONS
C--------------------------------------------------------------------
C!  - Prepare and reconstruct an event
C!
C!   Author   :- J. Bunn
C!   Modified :- S. Orteu              25-FEB-1987
C!   Modified :- E. Lancon             24-FEB-1993
C!   Modified :- I. Tomalin            15-AUG-1995 - Call TSUMVD.
C!   Modified :- M. Cattaneo              JUN-1996 - Call BMSPOT
C!   Modified :- I.Tomalin             15-MAY-1997
C!             moved call to TSUMVD from RECONS to RECEVT
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "rstate.h"
#include "rtimes.h"
#include "jsumjj.h"
#include "ravtim.h"
#include "rpass0.h"
      LOGICAL RMOD, BTEST
#include "bmacro.h"
C--------------------------------------------------------------------
C
C
C?   Get Starting Time
C
      CALL ALTIME(AEVSRT)
      IF(FDEBRF.AND.JDBDRF(JULSK).GE.3) THEN
        WRITE(LOUTRL,'(A)') '0RECONS- List of banks read'
        CALL AUBLIS ('E')
      ENDIF
C
C?   If PASS0, STOP it if enough events processed
C
      IF ( FPASS0 ) THEN
        KJSUM = IW(NAMIND('JSUM'))
        IF ( KJSUM.GT.0 ) THEN
          NEVT  = ITABL (KJSUM,1,JJSUTN)
          IF ( NEVT.GE.NPEVMA ) GOTO  999
        ENDIF
      ENDIF
C
C?   Convert KINE to FKIN
C
      NFKIN=NAMIND('FKIN')
      NKINE=NAMIND('KINE')
      KKINE=IW(NKINE)
      KFKIN=IW(NFKIN)
      IF (KFKIN.EQ.0.AND.KKINE.NE.0) THEN
        CALL FYKINE('DROP','NOGARB',IER)
        IF (IER.NE.0) GO TO 888
      ENDIF
C
C?   User Init
C
      CALL USIEVT(IREJ)
C
C?   Falcon Init
C
      IF(FDETRF(JULFA))CALL RFALIE(IREJ)
      IF (IREJ.NE.0) THEN
        NEVTRR(2) = NEVTRR(2) + 1
        GOTO 888
      ENDIF
C
C?   In case of DST-DST reprocessing, Skip reconstruction
C?   of events with less than 4 banks (LCAL events cannot be
C?   re-processed from DST nor POT)
C
      IF ( FREPRO ) THEN
        L = IGTLEN(IW,'E')
        IF ( L.LE.4 ) GOTO 777
      ENDIF
C
C?   Clean input data if requested
C
      CALL RCLEAN
C
C?   Prepare the data from this event
C
      CALL RPREDA(IREJ)
      IF (IREJ.NE.0) THEN
        NEVTRR(4) = NEVTRR(4) + 1
        GOTO 888
      ENDIF
C
C?   Reconstruct the event
C
      CALL RECEVT(IREJ)
      IF (IREJ.NE.0) THEN
        NEVTRR(5) = NEVTRR(5) + 1
        GOTO 888
      ENDIF
      NEVTRS(1) = NEVTRS(1) + 1
C
C Make POT banks
      CALL RNXMOD(MODPOT,RMOD)
      CALL RDEFPO
        CALL RDEFMI
C
C?   Build GAMPEC Bank.
C
      IF ( .NOT.FPASS0 ) THEN
        IF ( IW(NAMIND('GAMP')).NE.0 .AND. FDETRF(JULEC) ) THEN
          CALL RNXMOD (MODGMP,RMOD)
          CALL GASTEER
        ENDIF
      ELSE
      ENDIF
C
  777 CONTINUE
C
C?   Built REVH The event reconstruction header bank
C
      CALL RREVHE
C
C?   Build the Beam spot position (need REVH !!!!)
C
      CALL RNXMOD(MODBSP,RMOD)
      CALL VBSPOT
C
C?   Fill the LEP bom beam spot
C
      CALL OLSPOT(0)
C
C?   Fill the Monitoring bank (MONE)
C
      CALL RNXMOD(MODMON,RMOD)
      CALL RMONIT
C
C?   Write the event and fill JSUM
C
      CALL RWREVT
      CALL RNXMOD(MODEND,RMOD)
C
C?   Histogram the event
C
      CALL RNXMOD(MODHIS,RMOD)
      CALL RHSEVT(IREJ)
      CALL RFALEE
      CALL RNXMOD(MODEND,RMOD)
C
C?   --------------> Here we GO OUT
C
  888 CONTINUE
C
C?   Get Ending Time
C
      CALL RNXMOD (-1,RMOD)
      CALL ALTIME(AEVFRT)
      TIME = AEVFRT-AEVSRT
      STIMRA = STIMRA + TIME
      IF (JDBDRF(JULSK).GT.2) WRITE(LOUTRL,'(A,F7.2,A)')
     1    '0RECONS- This event took ',TIME,' CPU seconds'
C Clean lists
      CALL BDROP(IW,'T')
  999 CONTINUE
      END
#endif
