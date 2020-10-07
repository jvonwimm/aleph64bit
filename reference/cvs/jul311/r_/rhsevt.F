      SUBROUTINE RHSEVT(IREJ)
C----------------------------------------------------------------------
C!  - Histogram this event
C!
C!  Author    J. Bunn
C!
C!  Modified  S. Orteu                  9-FEB-1987
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "ravtim.h"
#include "revent.h"
#include "rtimes.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
      REAL AVTIM(10)
      IREJ = 0
C--------------------------------------------------------------------
      IF (JHISRF(JULIT).GE.1) CALL ITHIST
      IF (JHISRF(JULTP).GE.1) CALL TPHIST
      IF (JHISRF(JULVD).GE.1) CALL VDHIST
      IF (JHISRF(JULEC).GE.1) CALL ECHIST
      IF (JHISRF(JULLC).GE.1) CALL LCHIST
      IF (JHISRF(JULSA).GE.1) CALL SAHIST
      IF (JHISRF(JULSI).GE.1) CALL SIHIST
      IF (JHISRF(JULHC).GE.1) CALL HCHIST
      IF (JHISRF(JULMU).GE.1) CALL MUHIST
      IF (JHISRF(JULBO).GE.1) CALL OMHIST
      IF (JHISRF(JULYR).GE.1.OR.JHISRF(JULYT).GE.1) CALL YYHIST
      CALL USHIST(IREJ)
C
      CALL ALTIME(AEVFRT)
      TIME = AEVFRT-AEVSRT
      IF (JDBDRF(JULSK).GT.2) WRITE(LOUTRL,'(A,F7.2,A)')
     1    '0RHSEVT- This event took ',TIME,' CPU seconds'
      TIME2 = TIME*TIME
      STIMRA = STIMRA + TIME
C
      TKBYRE = TKBYRE + SIZERE
      IF (JHISRF(JULSK).GE.1) THEN
        JOFF = JULSK*1000
        CALL HFILL(JOFF+1,SIZERE,0.,1.)
C
C For the first ten events save the time
C On the tenth event take the average of all ten, and book
C a histogram with scale twice this average, in millisecs
C For subsequent events, enter straight into histogram
C
        IF (ISQNRC.LE.10) THEN
          AVTIM(ISQNRC)=TIME
        ENDIF
        IF (ISQNRC.EQ.10) THEN
          AVRG=STIMRA*400.0
          CALL HBOOK1 (JOFF,'TIME PER EVENT (MSECS)$',100,0.,AVRG,0.)
          DO 10 I=1,10
   10     CALL HF1(JOFF,AVTIM(I)*1000.,1.0)
        ELSEIF (ISQNRC.GT.10) THEN
          CALL HF1(JOFF,TIME*1000.,1.0)
        ENDIF
      ENDIF
C
C--------------------------------------------------------------------
      END
#endif
