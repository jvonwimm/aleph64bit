      SUBROUTINE ECPAD (LUN,IER)
C ------------------------------------------------------------------
C - B.Bloch   January 1990  as courtesy from F. Ranjard
CKEY ECAL DBASE / JULIA USER
C! reads the EcalPadGainCorrection file in sequential mode
C  opens the file in sequential mode on unit LUN
C
C - Input  : LUN    / I     = logical unit for ECAL file
C - Output : IER    / I     = error code rom AOPEN
C                             0 means OK
C
C - Calls  :  AOPEN,ACDARG,AWERRC              from ALEPHLIB
C             BREAD                            from BOS
C ------------------------------------------------------------------
#ifndef DOC
      CHARACTER*80 FSQNAM,FNAM,FDEVI
      CHARACTER*4 ATYP
#include "bcs.h"
      DATA IFIR /0/
C-------------------------------------------------------------------
C
      IER = 0
      IF (IFIR .EQ. 0) THEN
         IFIR = 1
         CALL AGTFIL ('FEPA','READ',LUN,IER)
         IF (IER .NE. 0 ) GOTO 999
         IF (IW(6).GT.0)
     &     WRITE (IW(6),'(/1X,''+++ECPAD+++ ECPAD file is opened'')')
C
C - read the ECPAD file
C
  10     CALL BREAD (IW,LUN,'R',*10,*30)
         IW(1) = 0
C
      ENDIF
C
C    next entry
C
  30  CONTINUE
      IW(1) = 0
C
 999  RETURN
      END
#endif
