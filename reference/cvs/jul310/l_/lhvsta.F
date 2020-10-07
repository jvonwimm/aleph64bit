      SUBROUTINE LHVSTA(ITYPE)
C-----------------------------------------------------------------------
C! Check the high voltage status bits
C!
C!   Author   : P. H. Hansen  891017
C!   Modified : J. Wear       891119
C!              J. Wear       900322  ! Added physics trigger check
C!   Modified : P. H. Hansen  960501  ! Use X1RG bank
C!   Modified : B. Bloch      9711    ! use XLUMOK from ALEPHLIB
C!                            Output:
C!                                      ITYPE=0 Nothing turned on
C!                                      ITYPE=1   ECAL+HCAL+LCAL HV on
C!                                                and no LOLE error
C!                                      ITYPE=2   also ITC+TPC HV on
C!                                                and basic triggers on
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rcurnt.h"
#include "rflags.h"
C
      logical xlumok,llumok,xlu,llu
      external xlumok,llumok
C
C---------------------------------------------------------------------
      ITYPE=0
      IF (FMCRUN) THEN
        ITYPE = 2
        GOTO 999
      ENDIF
      xlu = xlumok(dum)
      llu = llumok(dum)
      if ( xlu) then
         itype=2
         go to 999
      else if (llu) then
         itype = 1
         go to 999
      endif
 999  RETURN
      END
#endif
