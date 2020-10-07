      SUBROUTINE MINJLT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Reconstructed tagged lepton bank DDLT for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    14-Nov-94
C
C     Input  : PLJT bank
C     Output : DLJT bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
#include "pljtjj.h"
#include "dljtjj.h"

C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C-----------------------------------------------------------------------
C++   Pick up PLJT bank.
C
      KPLJT = NLINK('PLJT',0)
      IF (KPLJT.LE.0) RETURN
      NPLJT = LROWS(KPLJT)
      IF (NPLJT.LE.0) RETURN
C
C++   Create the DLJT bank.
C
      NDLJT = NPLJT
      LEN = LMHLEN + LDLJTA * NDLJT
      CALL AUBOS('DLJT',0,LEN, KDLJT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINJLT: Cannot create DLJT bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPLJT = NLINK('PLJT',0)
      ENDIF
      IW(KDLJT+LMHCOL) = LDLJTA
      IW(KDLJT+LMHROW) = NDLJT
C
C++   Loop over PLJT storing information in DLJT.
C
      DO 100 I=1,NDLJT
         IW(KROW(KDLJT,I)+JDLJPX) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPX))
         IW(KROW(KDLJT,I)+JDLJPY) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPY))
         IW(KROW(KDLJT,I)+JDLJPZ) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPZ))
         IW(KROW(KDLJT,I)+JDLJPE) = NINT(AFACTM * RTABL(KPLJT,I,JPLJPE))
         IW(KROW(KDLJT,I)+JDLJNO) = ITABL(KPLJT,I,JPLJNO)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DLJT')
C
      RETURN
      END
#endif
