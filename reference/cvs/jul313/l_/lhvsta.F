      SUBROUTINE LHVSTA(ITYPE)
C-----------------------------------------------------------------------
C! Check the high voltage status bits
C!
C!   Author   : P. H. Hansen  891017
C!   Modified : J. Wear       891119
C!              J. Wear       900322  ! Added physics trigger check
C!   Modified : P. H. Hansen  960501  ! Use X1RG bank
C!   Modified : B. Bloch      9711    ! use XLUMOK from ALEPHLIB
C!   Modified : B. Bloch      0003    ! new XLUM2K logical
C!                            Output:
C!                                      ITYPE=0 Nothing turned on
C!                                      ITYPE=1 LCAL HV on, trigs enabled
C!                                              no LOLE error
C!                                      ITYPE=2 standard xlumok, false
C!                                              during mini ramp
C!                                      ITYPE=3 standard xlumok, but
C!                                              tolerating mini ramp
C!
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rcurnt.h"
#include "rflags.h"
C
      logical xlumok,llumok,xlum2k,xlu,llu,xl2
      external xlumok,llumok,xlum2k
C
C---------------------------------------------------------------------
      ITYPE=0
      IF (FMCRUN) THEN
        ITYPE = 2
        GOTO 999
      ENDIF
      xlu = xlumok(dum)
      llu = llumok(dum)
      xl2 = xlum2k(dum)
      if ( xlu) then
         itype=2
         go to 999
      else if (xl2) then
         itype=3
         goto 999
      else if (llu) then
         itype = 1
         go to 999
      endif
 999  RETURN
      END
#endif
