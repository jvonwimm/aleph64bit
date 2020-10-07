      SUBROUTINE SIPSUM(ICLU,IPHM)
C------------------------------------------------------------
C! - For cluster # ICLU in bank SCLS returns the Cluster PHI index
C!   which has the maximum energy sum over R bins in the sum of planes
C!   2 and 5
C  B. Bloch -Devaux  April 93
C  Structure : SUBROUTINE
C           External references:NAMIND
C           Comdecks references:BCS,BMACRO,SPDAJJ,SCLSJJ
C  Input : ICLU cluster number in SCLS bank
C  Output : IPHM     Phi index [1,32] with max enrgy sum over planes 2-5
C  Banks  : input  - SCLS,SPDA
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "spdajj.h"
#include "sclsjj.h"
      DIMENSION EPHI(2,32),PHES(32)
      DATA IFIR/0/
#include "bmacro.h"
C -------------------------------------------------------------------
C
C
      CALL VZERO(EPHI,64)
      CALL VZERO(PHES,32)
      JSCLS = IW(NASCLS)
      JSPDA = IW(NASPDA)
      IF (JSCLS.LE.0) GO TO 999
      IF (JSPDA.LE.0) GO TO 999
      IPAD  = ITABL(JSCLS,ICLU,JSCLSP)
    8 CONTINUE
C   Look at SPDA data
      IA = ITABL(JSPDA,IPAD,JSPDAD)
      ID = ITABL(JSPDA,IPAD,JSPDSM)
      RE = RTABL(JSPDA,IPAD,JSPDEN)
      CALL SIDCOZ(IA,MODU,IST,IPH,IRD)
      IF ( IST.EQ.2   .OR.   IST.EQ.5 ) THEN
         KST = 1
         IF ( IST.EQ.5 ) KST = 2
         EPHI(KST,IPH) = EPHI(KST,IPH) + RE
      ENDIF
      IPAD  = ITABL(JSPDA,IPAD,JSPDSP)
      IF (IPAD.GT.0) GO TO 8
C   now look for phi index with max energy sum
C   in combination of 2 layers with same phi shift ( modulo 3)
          SMAX = 0.
          DO 30 IRR =  1,32
            PHES(IRR) = EPHI(1,IRR) + EPHI(2,IRR)
            IF(PHES(IRR).GT.SMAX) THEN
              SMAX = PHES(IRR)
              IPHM = IRR
            ENDIF
   30     CONTINUE
  999 RETURN
      END
#endif
