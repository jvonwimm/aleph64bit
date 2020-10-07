      SUBROUTINE SIECOR(ICLU,ECLU)
C------------------------------------------------------------
C!  - For cluster # ICLU in bank SCLS returns the calibration corrected
C! energy of the cluster
C  B. Bloch -Devaux  June 1992  , E. Mannelli Feb 1993
C  Structure : SUBROUTINE
C           External references:NAMIND
C           Comdecks references:BCS,BMACRO,SPDAJJ,SCLSJJ
C  Input : ICLU cluster number in SCLS bank
C  Output : ECLU cluster energy ( GEV) corrected for calibration
C           ECLU = 0. means no update is possible
C  Banks  : input  - SCLS,SPDA
C  Calls  : SIEPCR to apply gain calibration to pad energies
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "spdajj.h"
#include "sclsjj.h"
#include "secajj.h"
      DATA IFIR/0/
#include "bmacro.h"
C -------------------------------------------------------------------
C
      IF (IFIR.EQ.0) THEN
        IFIR = 1
        NASCLS = NAMIND ('SCLS')
        NASPDA = NAMIND ('SPDA')
        NASECA = NAMIND ('SECA')
      ENDIF
C
      ECLU = 0.
      KSCLU = IW(NASCLS)
      KSPDA = IW(NASPDA)
      KSECA = IW(NASECA)
      IF (KSCLU.LE.0) GO TO 999
      IF (KSPDA.LE.0) GO TO 999
      IF ( ICLU.GT.LROWS(KSCLU)) GO TO 999
      IPAD  = ITABL(KSCLU,ICLU,JSCLSP)
    8 CONTINUE
C   Look at SPDA data    Energies in Gev
      RE = RTABL(KSPDA,IPAD,JSPDEN)
      IA = ITABL(KSPDA,IPAD,JSPDAD)
      CALL SIDCOZ(IA,MODU,IST,IPH,IRD)
      RE = SIEPCR(RE,MODU,IST,IPH,IRD)
      ECLU = ECLU + RE* RTABL(KSECA,MODU,JSECPC)
      IPAD  = ITABL(KSPDA,IPAD,JSPDSP)
      IF (IPAD.GT.0) GO TO 8
  999 RETURN
      END
#endif
