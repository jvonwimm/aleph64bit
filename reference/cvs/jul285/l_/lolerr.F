      SUBROUTINE LOLERR(IERR)
C-----------------------------------------------------------------------
C! Check for online errors
C!
C!   Author   : P. H. Hansen  890905
C!
C!     Input   : 'LWDI' 'LTDI' 'LDWP' 'LDST' 'LOLE'
C!     Output  :  IERR (=0 if OK)
C!               'LOLE'
C!
C!     Description :
C!     ===========
C?     Check if LOLE bank exists - otherwise we have a problem
C?     Check for Fastbus errors
C?     Check for Missing marker channels (every 32nd always read)
C?     Check for HV trips and current exeeding limit
C?       *** 7/5/92 Current trips happen to often. I stop setting
C?       ***        the global error bit because of this
C?     Check for sparks (or events following a spark):
C?      IF
C?         (  >7.5 GeV in one plane AND
C?            >80% of the energy in that plane)
C?         OR < -1 GeV on the analog wiresum in a module
C?         OR < -4 GeV in a storey layer
C?         OR > 60 GeV in a storey layer
C?      THEN store plane address in LOLE
C?     (cells that are put dead via the banks LDST and LDWP
C?      are disregarded in the spark check)
C?     In case of any error set IERR = -1 and report
C?
C!======================================================================
#ifndef DOC
#include "lcnamc.h"
#include "lwdijj.h"
#include "ltdijj.h"
#include "lolejj.h"
#include "ldstjj.h"
#include "ldwpjj.h"
#include "rflags.h"
#include "bcs.h"
#include "rcurnt.h"
      LOGICAL SPARK
      EXTERNAL NAMIND
      DIMENSION MEVST(3,4)
      DATA RMXPL,MINPL,RMXST,RMNST,RMNPL/83.,-1000,658.,-44.,0.8/
C
#include "bmacro.h"
#include "lcmacr.h"
C-----------------------------------------------------------------------
      IERR = 0
C
C The LOLE bank must exist - otherwise the detector hangs on timeout
      KLOLE = IW(NAMIND('LOLE'))
      IF(KLOLE.LE.0) THEN
        LEN = LLOLEA + LMHLEN
        CALL AUBOS('LOLE',0,LEN,KLOLE,IGARB)
        IF (IGARB.EQ.2)                                  GOTO 998
        IW(KLOLE+1) = LLOLEA
        IW(KLOLE+2) = 1
        IW(KLOLE+LMHLEN+JLOLER) = -1
        IW(KLOLE+LMHLEN+JLOLTO) =  1
        CALL RERROR('LPREDA',1,
     & 'Readout error - no data transmitted')
        GOTO 900
      ENDIF
C
C Check for fastbus errors and HV trips
      IF(ITABL(KLOLE,1,JLOLER).NE.0) THEN
        DO 9 I=1,4
          IFL = ITABL(KLOLE,1,JLOLFB+I-1)
          IF(IFL.NE.0) THEN
            CALL RERROR('LPREDA',2,
     &      'FastBus error')
            GOTO 900
          ENDIF
          IFL = ITABL(KLOLE,1,JLOLMA+I-1)
          IF(IFL.NE.0) THEN
            CALL RERROR('LPREDA',3,
     &      'Marker word missing')
            GOTO 900
          ENDIF
    9   CONTINUE
        IFL = ITABL(KLOLE,1,JLOLHV)
C
C HV bits set due to calibration with HV on. We remove them again.
        IF(IRUNRC.GE.10605.AND.IRUNRC.LE.10611) THEN
          IFL = 0
          IW(KLOLE+LMHLEN+JLOLHV) = 0
          IW(KLOLE+LMHLEN+JLOLER) = 0
        ENDIF
C
        IF(IFL.NE.0) THEN
C
C Here reset the global error bit in case of a current trip
          IF(IFL.GE.16) IW(KLOLE+LMHLEN+JLOLER) = 0
C
          CALL RERROR('LPREDA',4,
     &    'HV trip during beam crossing')
        ENDIF
      ENDIF
C
C Check for sparking plane  :
C   A plane with more than 7.5 GeV and more than 80%
C   or Analog wiresum less than -1 GeV
      SPARK = .FALSE.
      KLWDI = IW(NALWDI)
      KLDWP = IW(NALDWP)
      IADDR = 1
      IF(KLWDI.GT.0) THEN
        DO 11 I=1,LROWS(KLWDI)
          JMAX = 0
          IEMX = 0
          ISUM = 0
          DO 10 J=2,39
C
C Disregard planes on the dead list
            IF(KLDWP.GT.0.AND.LROWS(KLDWP).GE.1) THEN
              DO 21 IDED=1,LROWS(KLDWP)
                IAD = ITABL(KLDWP,I,JLDWAD)
                MODU = IAD/64+1
                IPLAN = IAD - (MODU-1)*64
                IF(MODU.EQ.I.AND.IPLAN.EQ.J-1) GOTO 10
   21         CONTINUE
            ENDIF
C
            IENE = ITABL(KLWDI,I,J)
            ISUM = ISUM + IENE
            IF(IENE.GT.IEMX) THEN
              IEMX = IENE
              JMAX = J
            ENDIF
   10     CONTINUE
          REMX=FLOAT(IEMX)/RENLEP
          IF(REMX.GT.RMXPL.AND.ISUM.NE.0) THEN
            IF(FLOAT(IEMX)/FLOAT(ISUM).GT.0.80) THEN
               SPARK = .TRUE.
            ENDIF
               IADDR = (I-1)*64+JMAX-1
          ENDIF
          IF(ITABL(KLWDI,I,40).LT.MINPL) SPARK=.TRUE.
   11   CONTINUE
      ENDIF
C
C Then check for spark events using storeys:
C     A Storey layer with more than 60 GeV
C     A Storey layer with less than -4 GeV
C
      KLTDI = IW(NALTDI)
      KLDST = IW(NALDST)
      IF(KLTDI.GT.0) THEN
        CALL VZERO(MEVST,12)
        DO 13 I=1,LROWS(KLTDI)
          IAD = ITABL(KLTDI,I,JLTDAD)
C
C Disregard cells on the dead list
          IF(KLDST.GT.0.AND.LROWS(KLDST).GE.1) THEN
            DO 22 IDED=1,LROWS(KLDST)
             IF(ITABL(KLDST,IDED,JLDSAD).EQ.IAD) GOTO 13
   22       CONTINUE
          ENDIF
C
C Check also for wrong module number
C Do not reject event - but put address=1
          MODU = LCMOD(IAD)
          IF(MODU.LE.0.OR.MODU.GE.5) THEN
C            IW(KLOLE+LMHLEN+JLOLER) = -1
            IW(KLOLE+LMHLEN+JLOLTO) =  ITABL(KLTDI,I,JLTDAD)
            KDI = KROW(KLTDI,I)
            IW(KDI+JLTDAD)=1
            CALL RERROR('LPREDA',6,'Module number out of range')
            GOTO 900
          ENDIF
C
C Sum up the energy in the storey layers
          DO 12 J=1,3
            MEVST(J,MODU) = MEVST(J,MODU)
     &        + ITABL(KLTDI,I,J+1)
   12     CONTINUE
   13   CONTINUE
C
        DO 15 MODU=1,4
        DO 14 J=1,3
          REVST = FLOAT(MEVST(J,MODU))/RENLEP
          IF(REVST.LT.RMNST) SPARK=.TRUE.
          IF(REVST.GT.RMXST) SPARK=.TRUE.
   14   CONTINUE
   15   CONTINUE
      ENDIF
C
      IF(SPARK) THEN
        IF(RENLEP.LT.100.) IW(KLOLE+LMHLEN+JLOLER) = -1
        IW(KLOLE+LMHLEN+JLOLSP) = IADDR
        CALL RERROR('LPREDA',5,'Spark event')
      ENDIF
C
C Set output error flag
  900 IERR  = ITABL(KLOLE,1,JLOLER)
C
      GOTO 999
  998 CALL REPORT('LOLERR','AUBOS failure',1)
  999 CONTINUE
      END
#endif
