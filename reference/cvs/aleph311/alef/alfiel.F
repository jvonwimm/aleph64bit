      FUNCTION ALFIEL(DUM)
C --------------------------------------------------------------
C - F.Ranjard - 890803       modified - 900419
C - modified by: D.Colling - 930111
C                take 92 current offset into account
C                F.Ranjard - 931213
C                the 92 current offset is valid for 92 and 93
C                F.Ranjard - 940624
C                polarity was wrong for run 25261 and 25265
C                give the correct mag.field value
C! Returns the magnetic field
CKEY FIELD
C - Input    : DUM             = dummy argument
C - Output   : ALFIEL  / R     = magnetic field value
C
C - IF (FIEL data card exists) THEN
C      ALFIEL = FIEL word(1)
C   ELSE IF montecarlo (AFID exists) THEN
C      ALFIEL = AFID word(JAFIMF)
C   ELSE IF raw data (RALE exists) THEN
C      ALFIEL = RALE word(13)*word(14) * nominal field/current
C   ELSE no bank
C      ALFIEL = 15. (nominal field)
C   ENDIF
C   IF (no FIEL and  RUNH or RUNR) THEN
C      IF (run number > 6000) ALFIEL = -ALFIEL
C   ENDIF
C ------------------------------------------------------------------
#ifndef DOC
      SAVE NFIEL, NRALE, NRUNH, NRUNR
C
#include "bcs.h"
C     FIENOM = nominal magnetic field
C     CURNOM = current giving a nominal magnetic field
C     ICOCUR = compesation coil current
      PARAMETER (FIENOM=15., CURNOM=4963750., ICOCUR=800000)
C     CUR92C = current offset for 92,93
      PARAMETER (CUR92C=17700.)
C
#include "runhjj.h"
#include "afidjj.h"
#include "ralejj.h"
      DATA NAFID /0/
#include "bmacro.h"
C --------------------------------------------------------------------
C - set name-indices
      IF (NAFID .EQ. 0) THEN
         NAFID = NAMIND ('AFID')
         NFIEL = NAMIND ('FIEL')
         NRALE = NAMIND ('RALE')
         NRUNH = NAMIND ('RUNH')
         NRUNR = NAMIND ('RUNR')
      ENDIF
C
C - get the mag. field
C
      IF (IW(NFIEL) .NE. 0) THEN
C        data card FIEL
         FIELD = RW(IW(NFIEL)+1)
C
      ELSEIF (IW(NAFID) .NE. 0) THEN
C        montecarlo run
         FIELD = RTABL (IW(NAFID),1,JAFIMF)
C
      ELSEIF (IW(NRALE) .NE. 0) THEN
C        raw data
C
C        get the run number
         IF (IW(NRUNH).GT.0) THEN
            JRUN = IW(NRUNH)
         ELSEIF(IW(NRUNR).GT.0) THEN
            JRUN = IW(NRUNR)
         ELSE
            GOTO 999
         ENDIF
         IRUN = IW(JRUN+JRUNRN)
C
         POLA  = ITABL (IW(NRALE),1,JRALMP)
         CURR  = ITABL (IW(NRALE),1,JRALMC)
         ICA   = ITABL (IW(NRALE),1,JRALMA)
         ICB   = ITABL (IW(NRALE),1,JRALMB)
C
C        1.1% correction if compensation coils are off
         CCOR  = 1.
         IF (ICA.LT.ICOCUR .OR. ICB.LT.ICOCUR) CCOR = 1.011
C
C        correct for 92 offset 14000 < run < 25000
         IF (IRUN.GT.14000 .AND. IRUN.LT.25000) THEN
           CURR = CURR - CUR92C
         ENDIF
C
         FIELD = POLA*CURR*CCOR*FIENOM/CURNOM
C
C        change the sign if run number > 6000
         IF (IRUN .GT. 6000) FIELD = -FIELD
C
C        give the correct mag.field value
         IF (IRUN .EQ.11961) FIELD = 15.011
         IF (IRUN .EQ.25261 .OR. IRUN.EQ.25265) FIELD = 15.021
C
      ELSE
C        no bank
         FIELD = FIENOM
      ENDIF
C
 999  CONTINUE
      ALFIEL = FIELD
C
      END
#endif
