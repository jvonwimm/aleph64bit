      SUBROUTINE MINHCO
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Hcal calorimeter bank DHCO for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PHCO bank
C     Output : DHCO bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "phcojj.h"
#include "dhcojj.h"
#include "bmacro.h"
C
C++   Pick up PHCO bank.
C
      KPHCO = NLINK('PHCO',0)
      IF(KPHCO.GT.0) THEN
         NPHCO = LROWS(KPHCO)
      ELSE
         NPHCO = 0
      ENDIF
      IF(NPHCO.LE.0) RETURN
C
C++   Create DHCO bank.
C
      NDHCO = NPHCO
      LEN = LMHLEN + LDHCOA * NDHCO
      CALL AUBOS('DHCO',0,LEN, KDHCO,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINHCO: Cannot create DHCO bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPHCO = NLINK('PHCO',0)
      ENDIF
      IW(KDHCO+LMHCOL) = LDHCOA
      IW(KDHCO+LMHROW) = NDHCO
C
C++   Loop over PHCO storing information in DHCO.
C
      DO 100 I=1,NPHCO
         IW(KROW(KDHCO,I)+JDHCE0) = NINT(EFACTM * RTABL(KPHCO,I,JPHCEC))
         IW(KROW(KDHCO,I)+JDHCTH) = NINT(AFACTM * RTABL(KPHCO,I,JPHCTH))
         IW(KROW(KDHCO,I)+JDHCPH) = NINT(AFACTM * RTABL(KPHCO,I,JPHCPH))
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DHCO')
C
      RETURN
      END
#endif
