      SUBROUTINE MINMSC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill  bank DMSC for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    30-Sep-94
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
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
C
#include "frftjj.h"
#include "pfrfjj.h"
#include "dmscjj.h"
C
      INTEGER GTSTUP
      LOGICAL FIRST,XFRID,PACK
      SAVE INDXE,INDXA,C1,HC2,UNDFL,FIRST,NR
      DATA FIRST / .TRUE. /
C
C!    set of intrinsic functions to handle BOS banks
#include "bmacro.h"
C
C
C++   Initialisation.
C
      IF(FIRST) THEN
C
C++      Determine which FRFT bank to use. Default is with Vdet.
C++      If FRF0 is given, no swap is made in ALPHA and FRFT/0 exists.
C++      For pre-1991 (data and MC), force FRFT/0. Since the FRFT/2 bank
C++      is unusable, drop it to be sure. FRFT/0 will appear as FRFT/3.
C
         IF(NLINK('FRF0',0).LE.0) THEN
            NR = 2
         ELSE
            NR = 0
         ENDIF
         CALL ABRUEV(IRUN,IEVT)
         IF (GTSTUP('VD',IRUN).LE.2) KFRFT = NDROP('FRFT',2)
         FIRST = .FALSE.
      ENDIF
C
C++   Ensure FRFT bank is unpacked from PFRF.
C
      KFRFT = IW(NAMIND('FRFT'))
      IF(KFRFT.LE.0) CALL FPTOJ('  ',IER)
      KFRFT = NLINK('FRFT',NR)
      IF(KFRFT.LE.0) KFRFT = IW(NAMIND('FRFT'))
C
C++   Identify the number of tracks.
C++   If no tracks, return without creating DMSC bank.
C
      IF(KFRFT.LE.0) RETURN
      NFRFT = LROWS(KFRFT)
      IF(NFRFT.LE.0) RETURN
      NDMSC = NFRFT
      IR = IW(KFRFT-2)
c!!   IRM = IR
      IRM = 0
      IF(IR.EQ.3) IRM = 0
      KPFRF = NLINK('PFRF',IRM)
C
C++   Create the DMSC bank.
C
      LEN = LMHLEN + LDMSCA * NDMSC
      CALL AUBOS('DMSC',IRM,LEN, KDMSC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINMSC: Cannot create DMSC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFRFT = NLINK('FRFT',IR)
      ENDIF
      IW(KDMSC+LMHCOL) = LDMSCA
      IW(KDMSC+LMHROW) = NDMSC
c     WRITE(IW(6),'('' MINMSC: DMSC bank lifted'')')
C
C++   Loop over the FRFT bank and fill the DMSC bank.
C
      DO 100 I=1,NFRFT
C
C++      Track parameters.
C
         IW(KROW(KDMSC,I)+JDMSSA) = NINT(AFACTM * RTABL(KFRFT,I,JFRFAL))
         I1   = IW(KROW(KDMSC,I)+JDMSSA)
c        PRINT * ,'DMSC', I1, IRM, NFRFT, NDMSC

  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DMSC')
C
      RETURN
      END
#endif
