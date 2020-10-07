      SUBROUTINE MINTHR (THRU,CTHR)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill thrust bank DTHR for Mini-DST.
C
C     Author: Agnieszka Jacholkowska 1-Oct-94
C
C     Input  : QJTHRU output
C     Output : DTHR bank
C
C     Called by QUEVT - MINALGO
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
#include "dthrjj.h"
C
      LOGICAL GOTIT
C
      DIMENSION CTHR(4)
C
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C
C
      GOTIT = .FALSE.
C
C++   Create the DTHR bank.
C
      IBNK = 0
      NDTHR = 1
      LEN = LMHLEN + LDTHRA
      CALL AUBOS('DTHR',IBNK,LEN, KDTHR,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINTHR: Cannot create DTHR bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         WRITE(IW(6),'('' MINTHR: IGARB NE 0. '')')
      ENDIF
c     WRITE(IW(6),'('' MINTHR: DTHR bank lifted'')')
c     PRINT * ,THRU, CTHR(1), CTHR(2), CTHR(3), CTHR(4)
      IW(KDTHR+LMHCOL) = LDTHRA
      IW(KDTHR+LMHROW) = NDTHR
C
C++   Loop over THRUST storing information in DTHR.
C
      IW(KROW(KDTHR,1)+JDTHTH) = NINT(AFACTM * THRU)
      IW(KROW(KDTHR,1)+JDTHPX) = NINT(AFACTM * CTHR(1))
      IW(KROW(KDTHR,1)+JDTHPY) = NINT(AFACTM * CTHR(2))
      IW(KROW(KDTHR,1)+JDTHPZ) = NINT(AFACTM * CTHR(3))
      IW(KROW(KDTHR,1)+JDTHPE) = NINT(AFACTM * CTHR(4))

      GOTIT = .TRUE.
C
 1000 CONTINUE
C
C++   Add the banks to the Mini list.
C
      IF (GOTIT) CALL MINLIS('DTHR')
C
      RETURN
      END

*=======================================================================
#endif
