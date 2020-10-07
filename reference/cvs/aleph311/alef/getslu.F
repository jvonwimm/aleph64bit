      SUBROUTINE GETSLU(IRUN,IFOUN,ILUQR,NBHA,XLUSI,BAK,EWCOR)
C----------------------------------------------------------------------
C! Gets the SICAL Luminosity for run IRUN
C  Author  J.Boucrot  16-Mar-1993
C  Modified 19-May-1993 to get informations from Sical bank 'SLUM'
CKEY ALEF LFIL
C Input argument :
C  IRUN = run number to be searched in bank 'RSLU'
C Output arguments :
C   IFOUN = 0 if no information found for run IRUN
C         = 1 if information found in bank 'RSLU' of ADBSCONS DAF
C         = 2 if information found in run header bank 'SLUM'
C
C The following output arguments are defined ONLY if IFOUN.GT.0  :
C   ILUQR = SICAL Flag  for Luminosity  :
C           0 = unknown     1 = provisional value   2 = definitive value
C    NBHA = Number of SICAL Bhabha events
C  XLUSI  = Best estimate of lumi from SICAL , in nb**-1
C The following quantities are defined ONLY if IFOUN = 1
C    BAK  = background Bhabha events
C  EWCOR  = Electroweak correction
C
C Description:
C gets the SICAL LUMI informations from the 'RSLU' bank.
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rslujj.h"
#include "slumjj.h"
      INTEGER ALGTRO
#include "bmacro.h"
C----------------------------------------------------------------------
      IFOUN = 0
      JRSLU = ALGTRO ('RSLU',IRUN,JRSLRN,JROWR)
      IF (JRSLU.NE.0 .AND. JROWR.GT.0) THEN
C  RSLU bank found : get the output arguments
         JRSLU = IABS (JRSLU)
         IFOUN = 1
         KRSLU = KROW(JRSLU,JROWR)
         ILUQR = IW(KRSLU+JRSLLQ)
         NBHA  = IW(KRSLU+JRSLNB)
         XLUSI = RW(KRSLU+JRSLLU)
         BAK   = RW(KRSLU+JRSLBK)
         EWCOR = RW(KRSLU+JRSLEW)
C  Nothing found in RSLU : try the run header bank 'SLUM' :
      ELSE
         JSLUM=NLINK('SLUM',IRUN)
         IF (JSLUM.GT.0) THEN
            IMETH=2
            NBHA = RTABL (JSLUM,IMETH,JSLUTI)
            XLUSI = RTABL (JSLUM,IMETH,JSLULU)
            ILUQR = 1
            BAK = 0.
            EWCOR = 0.
            IFOUN = 2
         ENDIF
      ENDIF
C
 999  RETURN
      END
#endif
