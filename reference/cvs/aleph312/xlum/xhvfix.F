      SUBROUTINE XHVFIX(XHV)
CKEY XLUMOK TRIG HV /INTERNAL
C----------------------------------------------------------------------
C!   Fix HV status bits
C!   Called from XHVSTA
C!   Author   :-       B. Bloch October 98   general fix routine 
C!
C!   Inputs:           Data Base bank XHVB
C!
C!   Outputs:          logical array XHV(24)
C!
C!   Libraries required: Alephlib
C!
C!   Description:
C!     Load year dependent XHVB bank from database.
C!     Returns the High Voltage status of ALEPH according to
C!     the bit mapping. Special care is taken to repair 
C!     some run-specific cases where the normal HV bits do not reflect 
C!     the actual detector status.
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      INTEGER irun,iruno,ievt,iper,ipero,irow,iadb,ista,istup,ir,idet
      INTEGER NAXHVB,KXHVB
      LOGICAL FIRST
      SAVE FIRST,NAXHVB,iruno,ipero
#include "bcs.h"
#include "xhvbjj.h"
#include "xhvbit.h"
#include "bmacrod.h"
      LOGICAL XHV(NHVBIT)
      INTEGER ADBRUN,AGETDB,NAMIND
      EXTERNAL ADBRUN,AGETDB,NAMIND
      DATA FIRST /.TRUE./,iruno/-1/,ipero/-1/
#include "bmacro.h"
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        NAXHVB = NAMIND('XHVB')
        FIRST = .FALSE.
      ENDIF
C get run,event number
      call abruev(irun,ievt)
C get which period to load, if still valid use current one
      if ( irun.ne.iruno) then
         iadb = adbrun(' ',irow,iper,irun,istup)
         if (iadb.eq.0) go to 999
         iruno = irun
         if ( iper.ne.ipero) then
            iadb = agetdb('XHVB',irun)
            if (iadb.eq.0) go to 999
            ipero = iper
         endif
      endif
      kxhvb = iw(NAXHVB)
C check if run/event is in the current list of fixes, then fix
      do 10 ir = 1,lrows(KXHVB)
         if (irun.eq.itabl(kxhvb,ir,jxhvru)) then
           if((ievt.ge.itabl(kxhvb,ir,jxhvfe)).and.(ievt.le.itabl(kxhvb,
     &        ir,jxhvle))) then
              idet = itabl(kxhvb,ir,jxhvde)
              ista = itabl(kxhvb,ir,jxhvhv)
              if (ista.eq.0) xhv(idet)= .false.
              if (ista.eq.1) xhv(idet)= .true.
            endif
         endif
 10   continue
C
 999  RETURN 
      END
#endif
