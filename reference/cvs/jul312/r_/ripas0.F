      SUBROUTINE RIPAS0
C----------------------------------------------------------------------
C!  - Initialize PASS0 conditions
C!
C!   Author   :- E. Lancon              3-APR-1992
C!
C!   Modified :- E. Lancon             17-MAY-1993
C!        Bswap previous VHOT bank to write it out if not enough events
C!        at end of run
C!   Modified :- I. Tomalin            15-SEP-1996
C!        Look for PVGG card - get V_d from VDET & gamma/gamma events option.
C!        Avoid deletion of VHOT for VDET II so V_d can use it.
C?
C!======================================================================
#ifndef DOC
#include "rpass0.h"
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "pascjj.h"
      DATA NEVTL,NEVTH,DTLAS,DTC / 50,999999,-1.,0.2 /
      INTEGER AGETDB,GTSTUP
      EXTERNAL AGETDB,GTSTUP
      LOGICAL USEVGG
#include "bmacro.h"
C
      NPEVMI = NEVTL
      NPEVMA = NEVTH
      TMTLAS = DTLAS
      DT0CUT = DTC
C
      NAPASC = NAMIND('PASC')
      KPASC = IW(NAPASC)
C
      IF (KPASC.LE.0) THEN
        WRITE (LOUTRL,'(5X,A)')
     &    ' No PASC cards found - DAF default values taken '
        IDUMM = AGETDB('PASC',-IRUNRC)
        KPASC = IW(NAPASC)
      ENDIF
C
      IF ( KPASC.LE.0 ) THEN
        WRITE (LOUTRL,'(A)')
     &    ' ++++ PASC not found on DAF, defaults taken '
      ELSE
C
        NPASC = LCOLS(KPASC)
        IF (NPASC.GE.1) NPEVMI = ITABL(KPASC,1,JPASMI)
        IF (NPASC.GE.2) NPEVMA = ITABL(KPASC,1,JPASMA)
        IF (NPASC.GE.3) TMTLAS = RTABL(KPASC,1,JPASET)
        IF (NPASC.GE.4) DT0CUT = RTABL(KPASC,1,JPASDT)
      ENDIF
C
C? Should TPC drift velocity be obtained from VDET & gamma/gamma events ?
      IVSET = GTSTUP('VD',IRUNRC)
      USEVGG = IW(NAMIND('PVGG')).GT.0.AND.IVSET.GE.8
C
      WRITE (LOUTRL,
     &  '(5X,A,I6,A,I6,A,/,5X,A,F7.2,/,5X,A,F7.2,/,5X,A,L4)')
     &                '         Number of Evts required    : ['
     &                , NPEVMI, '-', NPEVMA, ']',
     &                ' TLAS maximum elapsed time in hours : ', TMTLAS,
     &                ' Maximum allowed T0 variation (mus.): ', DT0CUT,
     &                ' Get V_d from VDET & gamma/gamma ?  : ', USEVGG
C
C?  get Number of VDET HOT chanels from previous run
C
      NAVHOT = NAMIND('VHOT')
      KVHOT  = IW(NAVHOT)
      NHOTBF = LROWS(KVHOT)
C
C?   Swap previous VHOT bank. Don't for VDET II as VHOT came with data.
C
      IVSET = GTSTUP('VD',IRUNRC)
      IF (IVSET.LE.7) CALL BSWAP (IW,'VHOT','TOHV')
C
  999 RETURN
      END
#endif
