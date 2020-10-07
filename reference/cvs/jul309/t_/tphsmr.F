C ---------------------------------------------------------------------------
      subroutine TPHSMR
C ---------------------------------------------------------------------------
C!  TPHSMR smears the TPCO coordinates in accordance with the data
C!         base bank TPSM (NR=tpc setup) for Monte Carlo events
C!
C - Author         Manoj Thulasidas  5-Dec-1994
C  
C   Input    TPCO and TPSM banks
C   Output   Smeared TPCO Bank
C ---------------------------------------------------------------------------
#ifndef DOC
      implicit none
C  globals
#include "bcs.h"
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  arguments
C  functions
      integer  NLINK, NAMIND
      EXTERNAL NLINK, NAMIND
C  locals
      integer MAXHIT
      parameter (MAXHIT = 21)
      real EMATR(MAXHIT,MAXHIT), RVEC(MAXHIT), EMATZ(MAXHIT,MAXHIT),
     $   ZVEC(MAXHIT), SCALR, SCALZ, EVECZ(MAXHIT,MAXHIT),
     $   EVECR(MAXHIT,MAXHIT), EVALZ(MAXHIT), EVALR(MAXHIT)
      integer  NATPRK, NATPZK, KTPRK, KTPZK, IRETZ, IRETR
      INTEGER IRUN,IEVT
      logical FIRST
      data FIRST /.TRUE./
      save EMATZ, EMATR, SCALZ, SCALR, EVECZ, EVALZ, EVECR, EVALR
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "bmacrod.h"
#include "bmacro.h"
C============================================================================
C  if the event is not Monte Carlo, do nothing
      CALL ABRUEV(IRUN,IEVT)
      IF(IRUN.GT.2000) GOTO 999
C--  one could check the EVEH to make sure that this event hasn't
C--  been smeared.  Not needed if called by RECEVT
      if (FIRST) then
        FIRST = .FALSE.
        call TPSMGT(MAXHIT,'TPSM',1,EMATZ,IRETZ)
        call TPSMGT(MAXHIT,'TPSM',2,EMATR,IRETR)
C  if there is a problem with the banks, quietly exit
        if (IRETZ.NE.0 .OR. IRETR.NE.0) then
          goto 999
        endif
C  read in the factor to be applied, if given through datacards
        NATPZK = NAMIND('TPZK')
        KTPZK = IW(NATPZK)
        IF (KTPZK.NE.0) THEN
          SCALZ = RW(KTPZK+1)
        ELSE 
          SCALZ = 1.0
        ENDIF
        NATPRK = NAMIND('TPRK')
        KTPRK = IW(NATPRK)
        IF (KTPRK.NE.0) THEN
          SCALR = RW(KTPRK+1)
        ELSE 
          SCALR = 1.0
        ENDIF
C  diagonalize the error matrices
        call TPDIAG(MAXHIT,EMATR,EVECR,EVALR)
        call TPDIAG(MAXHIT,EMATZ,EVECZ,EVALZ)
      endif
C  generate a random vector of length MAXHIT
      call TPRNDM(MAXHIT,EVECR,EVALR,RVEC)
      call TPRNDM(MAXHIT,EVECZ,EVALZ,ZVEC)
      call VSCALE(RVEC,SCALR,RVEC,MAXHIT)
      call VSCALE(ZVEC,SCALZ,ZVEC,MAXHIT)
C  smear hits in both views
      call TPSMR1(MAXHIT,ZVEC,RVEC)
 999  return
      end
#endif
