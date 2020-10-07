      SUBROUTINE MINECO
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Ecal calorimeter bank DECO for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PECO bank
C     Output : DECO bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pecojj.h"
#include "decojj.h"
#include "bmacro.h"
C
C++   Pick up PECO bank.
C
      KPECO = NLINK('PECO',1)
      IF (KPECO.LE.0) KPECO = IW(NAMIND('PECO'))
      IF (KPECO.LE.0) RETURN
      NPECO = LROWS(KPECO)
      IF (NPECO.LE.0) RETURN
      NR = IW(KPECO-2)
C
C++   Create DECO bank.
C
      NDECO = NPECO
      LEN = LMHLEN + LDECOA * NDECO
      CALL AUBOS('DECO',NR,LEN, KDECO,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINECO: Cannot create DECO bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPECO = NLINK('PECO',NR)
      ENDIF
      IW(KDECO+LMHCOL) = LDECOA
      IW(KDECO+LMHROW) = NDECO
C
C++   Loop over PECO storing information in DECO.
C
      DO 100 I=1,NPECO
         IW(KROW(KDECO,I)+JDECE0) = NINT(EFACTM * RTABL(KPECO,I,JPECEC))
         IW(KROW(KDECO,I)+JDECTH) = NINT(AFACTM * RTABL(KPECO,I,JPECTH))
         IW(KROW(KDECO,I)+JDECPH) = NINT(AFACTM * RTABL(KPECO,I,JPECPH))
         IW(KROW(KDECO,I)+JDECEF+0) = NINT(1000.* RTABL(KPECO,I,JPECE1))
         IW(KROW(KDECO,I)+JDECEF+1) = NINT(1000.* RTABL(KPECO,I,JPECE2))
         IW(KROW(KDECO,I)+JDECCC) = ITABL(KPECO,I,JPECCC)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DECO')
C
      RETURN
      END
#endif
