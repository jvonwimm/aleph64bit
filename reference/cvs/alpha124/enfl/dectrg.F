      SUBROUTINE DECTRG(IPECO,ITRGT1,ITRGT2)
C------------------------------------------------------------------
C! Trigger decoding
C
C Modif : L. Duflot (30 Aug 95) : Include new trigger schemes
C------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      INTEGER altrig
#include "qhac.h"
#include "qmacro.h"
C------------------------------------------------------------------
      itrgw  = 0
      itrgfl = 0
      itrgt1 = 0
      itrgt2 = 0
      ifwev = NLINK('FWEV',0)
      ieaux = NLINK('EAUX',0)
      IF     ( ipeco .LE. 0 .AND. ifwev .GT. 0 ) THEN
        itrgw = IW(ifwev+lmhlen+1)
      ELSEIF ( XMINI .AND. ieaux .GT. 0 ) THEN
        itrgw = IW(ieaux+lmhlen+1)
      ELSE
        IF ( krun .LT. 4000 ) then
          IF ( NLINK('X1RG',0) .GT. 0 ) THEN
            CALL x1redo
          ELSE
            CALL x1trg
          ENDIF
        ENDIF
        itrgfl = ALTRIG(itrg1,itrg2,itrg3)
        IF ( itrgfl .LE. 0 ) THEN
C         PRINT *,'!!! No trigger bank !!!'
C         PRINT *,'ALTRIG :',itrgfl
          itrgw = 0
        ELSE
         itrgw = itrg1
        ENDIF
      ENDIF
C -- Trigger Type
C -- ECAL Barrel Total Energy
      itrgt1 =          8 * IBITS(itrgw,17,1)
C -- ECAL End Cap Coincidence
      itrgt1 = itrgt1 + 1 * IBITS(itrgw,20,1)
C -- ECAL End Cap A Total Energy
      IF ( krun .LT. 6000 .AND. NLINK('X1RG',0) .LE. 0 ) THEN
        itrgt1 = itrgt1 + 2 * IBITS(itrgw,24,1)
      ELSE
        itrgt1 = itrgt1 + 2 * IBITS(itrgw,18,1)
      ENDIF
C -- ECAL End Cap B Total Energy
      IF ( krun .LT. 6000 .AND. NLINK('X1RG',0) .LE. 0 ) THEN
        itrgt1 = itrgt1 + 4 * IBITS(itrgw,25,1)
      ELSE
        itrgt1 = itrgt1 + 4 * IBITS(itrgw,19,1)
      ENDIF
C -- Single Muon (HCAL Penetration) Trigger
      itrgt2 =          1 * IBITS(itrgw,8,1)
C -- Single Charged Electromagnetic Trigger
      itrgt2 = itrgt2 + 2 * IBITS(itrgw,9,1)
C --
      RETURN
      END
#endif
