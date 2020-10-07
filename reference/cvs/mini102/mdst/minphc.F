      SUBROUTINE MINPHC
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PHCO bank from DHCO.
C
C     Author: Stephen Haywood      03-Apr-90
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dhcojj.h"
#include "phcojj.h"
#include "minfac.h"
#include "bmacro.h"
C
      KDHCO = NLINK('DHCO',0)
      IF(KDHCO.LE.0) RETURN
C
C++   Create PHCO bank.
C
      NPHCO = LROWS(KDHCO)
      IF(NPHCO.LE.0) RETURN
      LEN = LMHLEN + LPHCOA * NPHCO
      CALL AUBOS('PHCO',0,LEN, KPHCO,IGARB)
      CALL BLIST(IW,'S+','PHCO')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDHCO = NLINK('DHCO',0)
      ENDIF
      IW(KPHCO+LMHCOL) = LPHCOA
      IW(KPHCO+LMHROW) = NPHCO
C
C++   Fill PHCO bank.
C
      DO 100 I=1,NPHCO
         RW(KROW(KPHCO,I)+JPHCEC) = FLOAT(ITABL(KDHCO,I,JDHCE0))/EFACTM
         RW(KROW(KPHCO,I)+JPHCTH) = FLOAT(ITABL(KDHCO,I,JDHCTH))/AFACTM
         RW(KROW(KPHCO,I)+JPHCPH) = FLOAT(ITABL(KDHCO,I,JDHCPH))/AFACTM
  100 CONTINUE
C
      RETURN
      END
#endif
