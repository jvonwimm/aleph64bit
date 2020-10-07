      SUBROUTINE MINPSC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill  bank PMSC for Alpha   .
C
C     Author: Agnieszka Jacholkowska    30-Sep-94
C
C     Input  : DMSC bank
C     Output : PMSC bank
C
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
#include "dmscjj.h"
#include "pmscjj.h"
C
      LOGICAL FIRST,XFRID,PACK
      DATA FIRST / .TRUE. /
C
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C
C++   Pick up DMSC bank.
C
      KDMSC = NLINK('DMSC',0)
      IF(KDMSC.LE.0) RETURN
C
C++   Identify the number of tracks.
C++   If no tracks, return without creating PMSC bank.
C
      NDMSC = LROWS(KDMSC)
      IF(NDMSC.LE.0) RETURN
      NPMSC = NDMSC
      IR = IW(KDMSC-2)
      IRM = IR
      IF(IR.EQ.3) IRM = 0
C
C++   Create the PMSC bank.
C
      LEN = LMHLEN + LPMSCA * NPMSC
      CALL AUBOS('PMSC',IRM,LEN, KPMSC,IGARB)
      CALL BLIST(IW,'S+','PMSC')
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINPSC: Cannot create PMSC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDMSC = NLINK('DMSC',IR)
      ENDIF
      IW(KPMSC+LMHCOL) = LPMSCA
      IW(KPMSC+LMHROW) = NPMSC
C
C++   Loop over the DMSC bank and fill the PMSC bank.
C
      DO 100 I=1,NDMSC
C
C++      Track parameters.
C
         RW(KROW(KPMSC,I)+JPMSSA) = FLOAT(ITABL(KDMSC,I,JDMSSA))/AFACTM

  100 CONTINUE
C
      RETURN
      END
#endif
