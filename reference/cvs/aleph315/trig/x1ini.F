      SUBROUTINE X1INI(IRUN)
C ----------------------------------------------------------------------
C.
C. - Author: E. Blucher - 15-FEB-1989.
C. ----Modified version of X1IRUN by A. Putzer, C. Geweniger.
C. - Modified : C.Geweniger - 890900 for ALEPHLIB 9.9
C. - MODIFIED : B. BLOCH-DEVAUX 940107 TO INCLUDE INIT OF SICAL BANKS
C.
C! - Initialize the Level1 Trigger for run# IRUN
#ifndef DOC
      SAVE
C
      EXTERNAL NAMIND
      INTEGER AGETDB
C
#include "bcs.h"
#include "xtrcom.h"
#include "x1tcom.h"
#include "x1namc.h"
      DATA IFI /0/
C.
C ----------------------------------------------------------------------
C.
C - first entry
C
      IF (IFI .EQ. 0) THEN
         IFI = 1
         NAX1TH = NAMIND ('X1TH')
         NAXTEB = NAMIND ('XTEB')
         NAX1AD = NAMIND ('X1AD')
         NASIXA = NAMIND ('SIXA')
         NASIX2 = NAMIND ('SIX2')
         NASIFO = NAMIND ('SIFO')
      ENDIF
C.
C.  -  Reset Level1 Physics Trigger counters
C
      NACCL1  = 0
      DO 98 I = 1,NPHTR
 98   NPHYTR(I) = 0
C.
C.  -  Get table containing trigger thresholds for this run
C      Print a message if X1TH bank is not found
C.
      IND = AGETDB('X1TH',IRUN)
      IF (IND.EQ.0) THEN
         WRITE(IW(6),991) IRUN
 991     FORMAT(//1X,'+++X1INI+++ X1TH bank is missing for run= ',I5/
     &            13X,'====> No Thresholds found for this run '/
     &            13X,'Thresholds from previous run will be used if',
     &             1X,'still exist'///)
      ENDIF
C
      END
#endif
