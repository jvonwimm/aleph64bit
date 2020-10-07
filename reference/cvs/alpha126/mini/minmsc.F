      SUBROUTINE MINMSC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill  bank DMSC for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    30-Sep-1994
C     Modified : J. Boucrot             19-Jan-2001
C
C     Input  : FRFT bank
C     Output : DMSC bank (multiple scattering angle)
C
C     Called by MINDST
C
C     FRFT is  used after call to MINTRA
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
C
#include "alcons.h"
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
C
#include "frftjj.h"
#include "pfrfjj.h"
#include "dmscjj.h"
C
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C-----------------------------------------------------------------------
C++   Ensure FRFT bank is unpacked from PFRF.
C
      KFRFT = IW(NAMIND('FRFT'))
      IF(KFRFT.LE.0) CALL FPTOJ('  ',IER)
C Implicit loop on all FRFT banks: 
      KFRFT=NAMIND('FRFT')+1
 10   KFRFT=IW(KFRFT-1)
      IF (KFRFT.LE.0) GO TO 500
C
C++   Identify the number of tracks.
C++   If no tracks, return without creating DMSC bank.
C
      NFRFT = LROWS(KFRFT)
      IF(NFRFT.LE.0) GO TO 10
      NDMSC = NFRFT
      IRM = IW(KFRFT-2)
C
C++   Create the DMSC bank.
C
      LEN = LMHLEN + LDMSCA * NDMSC
      CALL AUBOS('DMSC',IRM,LEN, KDMSC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINMSC: Cannot create DMSC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFRFT = NLINK('FRFT',IRM)
      ENDIF
      IW(KDMSC+LMHCOL) = LDMSCA
      IW(KDMSC+LMHROW) = NDMSC
C
C++   Loop over the FRFT bank and fill the DMSC bank.
C
      DO 100 I=1,NFRFT
C
C++      Track parameters.
C
         IW(KROW(KDMSC,I)+JDMSSA) = NINT(AFACTM * RTABL(KFRFT,I,JFRFAL))
  100 CONTINUE
      GO TO 10
C
C++   Add the bank to the Mini list.
C
 500  CALL MINLIS('DMSC')
C
      RETURN
      END
#endif
