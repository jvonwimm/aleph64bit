      SUBROUTINE FYIRUN(BFIEL,TKDRO,SHDRO,CUTRA)
C ----------------------------------------------------------
CKEY FYXX MCARLO KINE INIT /USER
C J.Hilgart - 871307   B.Bloch 901010,980316
C! Fxxx DST format one-time initialization.
C
C INPUTS:
C       BFIEL       = Magnetic field in kGauss
C       TKDRO       = IF true THEN drop history of interactions
C                     in ITC electronics and TPC endplate
C       SHDRO       = IF true THEN drop history of interactions
C                     in calorimeters
C       CUTRA       = IF not 0. THEN drop track with momentum
C                     below CUTRA
C
C - Called from    USER              from ALEPHLIB.HLB
C - Calls   BKFMT                    from BOS77.HLB
#ifndef DOC
      SAVE
#include "bcs.h"
#include "alcons.h"
#include "fyrela.h"
#include "kgjjpar.h"
      LOGICAL SHDRO,TKDRO
#include "bmacro.h"
C ----------------------------------------------------------------------
C Some constants
      ENMINC = -1.0
      FSHDRO = SHDRO
      FTKDRO = TKDRO
      CUTRAC = CUTRA
      BFLCLT = BFIEL*CLGHT*1.E-5
C
C What the Fxxx adds to the E list: (6 banks)
      ELISAD = 'FKINFVERFPOIFPOLFZFRFSHO'
C
C Their formats:
      CALL BKFMT('FKIN','2I,(4F,4I)')
      CALL BKFMT('FVER','2I,(4F,3I,2A)')
      CALL BKFMT('FPOL','2I,(I,3F)')
      CALL BKFMT('FZFR','2I,(F)')
      CALL BKFMT('FSHO','I')
      CALL BKFMT('FPOI','I')
C
      NAJOB = NAMIND('AJOB')
      KAJOB = IW(NAJOB)
      IF (KAJOB .GT. 0) THEN
         ISIML = ITABL(KAJOB,1,2)
         IF (ISIML.EQ.101 .OR. ISIML.EQ.111) THEN
C FAST or SIMDST: add the SIMDST banks to 'E'list
            ELISAD = ELISAD(1:LNBLNK(ELISAD))//
     +               'FSTRFSCOFLTRFLCOFTOCFTTMFTCM'
            CALL BKFMT('FSCO','2I,(F,I,7F,I)')
            CALL BKFMT('FLTR','I')
            CALL BKFMT('FLCO','I')
            CALL BKFMT('FTOC','I')
            CALL BKFMT('FTTM','I')
            CALL BKFMT('FTCM','I')
         ENDIF
      ENDIF
C
C - get history type from KRUN bank
      IHTYPE = 0
      NKRUN = NAMIND('KRUN')
      JKRUN = IW(NKRUN)
      IF (JKRUN .GT. 0) THEN
         IGCOD = ITABL(JKRUN,JKRUGI,1)
         IHTYPE = MOD (IGCOD,1000) / 100
      ENDIF
C
C Work bank indices one-time initialization
      DO 1 I=1,LFXWBK
  1   JDFXWB(I) = 0
C
 990  CONTINUE
      END
#endif
