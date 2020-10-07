      SUBROUTINE SIESUM(ICLU,ECLU,IFL)
C------------------------------------------------------------
C!  - For cluster # ICLU in bank SCLS returns the Cluster energy as
C!    IFL =1  sum of pad energies  as in SPDA
C!    IFL =2  sum of raw energies  as in SPDA
C!    IFL =3  sum of raw energies  as in SPDA  applying calibration
C  B. Bloch -Devaux  February 93
C  Structure : SUBROUTINE
C           External references:NAMIND
C           Comdecks references:BCS,BMACRO,SPDAJJ,SCLSJJ
C  Input : ICLU cluster number in SCLS bank
C  Output : ECLU cluster energy ( GEV) corrected for calibration
C           ECLU = 0. means no update is possible
C  Banks  : input  - SCLS,SPDA,SMUX,SGUN
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "spdajj.h"
#include "sclsjj.h"
#include "sinamc.h"
#include "bmacro.h"
C -------------------------------------------------------------------
C
      ECLU = 0.
      KSCLU = IW(NASCLS)
      KSPDA = IW(NASPDA)
      IF (KSCLU.LE.0) GO TO 999
      IF (KSPDA.LE.0) GO TO 999
      IF ( ICLU.GT.LROWS(KSCLU)) GO TO 999
      IPAD  = ITABL(KSCLU,ICLU,JSCLSP)
    8 CONTINUE
C   Look at SPDA data
      IA = ITABL(KSPDA,IPAD,JSPDAD)
      IF ( IFL.EQ.1) THEN
        RE = RTABL(KSPDA,IPAD,JSPDEN)
      ELSE IF ( IFL.EQ.2) THEN
        RE = RTABL(KSPDA,IPAD,JSPDER)
      ELSE IF ( IFL.EQ.3) THEN
        RA = RTABL(KSPDA,IPAD,JSPDER)
        CALL SIDCOZ(IA,MODU,IST,IPH,IRD)
        RE = SIEPCR(RA,MODU,IST,IPH,IRD)
      ENDIF
      ECLU = ECLU + RE
      IPAD  = ITABL(KSPDA,IPAD,JSPDSP)
      IF (IPAD.GT.0) GO TO 8
  999 RETURN
      END
#endif
      SUBROUTINE SIFSUM(ICLU,EFRA,NPAD)
C------------------------------------------------------------
C!  - For cluster # ICLU in bank SCLS returns the Cluster energy per
C!    plane and the number of pads
C  B. Bloch -Devaux  February 93
C  Structure : SUBROUTINE
C           External references:NAMIND
C           Comdecks references:BCS,BMACRO,SPDAJJ,SCLSJJ
C  Input : ICLU cluster number in SCLS bank
C  Output : EFRA(12) cluster energy fractions in the 12 planes
C           NPAD(1)  number of pads in cluster ( ALL)
C           NPAD(2)  number of dead pads in cluster
C  Banks  : input  - SCLS,SPDA
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sinamc.h"
#include "spdajj.h"
      DIMENSION EFRA(12) , NPAD(2)
#include "bmacro.h"
C -------------------------------------------------------------------
C
C
      CALL VZERO(EFRA,12)
      CALL VZERO(NPAD,2)
      JSPDA = IW(NASPDA)
      IF (JSPDA.LE.0) GO TO 999
      NSPDA = LROWS(JSPDA)
      IF (NSPDA.LE.0) GO TO 999
C   Look at SPDA data
      DO 10 II= 1, NSPDA
        IC = ITABL(JSPDA,II,JSPDSC)
        IF ( IC.NE.ICLU) GO TO 10
        ID = ITABL(JSPDA,II,JSPDSM)
        IA = ITABL(JSPDA,II,JSPDAD)
        RE = RTABL(JSPDA,II,JSPDEN)
        CALL SIDCOZ(IA,MODU,IST,IPH,IRD)
        EFRA(IST) = EFRA(IST) + RE
        NPAD(1)   = NPAD(1)   + 1
        IF ( ID.GT.0) NPAD(2)   = NPAD(2)   + 1
   10 CONTINUE
  999 RETURN
      END
#endif
