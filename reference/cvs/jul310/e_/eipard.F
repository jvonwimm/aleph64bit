      SUBROUTINE EIPARD
C----------------------------------------------------------------------
C!  READ PARAMETERS FOR ELECTRON-ID FROM BOS CARDS OR DATA BASE
C
C   AUTHOR : A. FALVARD             26/07/89
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "eibpjj.h"
#include "eibpr2.h"
#include "rlunit.h"
C
C  LOOK FOR ELECTRON IDENTIFICATION PARAMETERS IN BOS CARDS
C
        IND=IW(NAMIND('EIBP'))
        IF(IND.EQ.0)THEN
C
C  READ THE EIBP BANK FROM THE DATA BASE
C
          IND=MDARD(IW,LRCONS,'EIBP',0)
          IF(IND.LE.0)THEN
C
C  INITIALIZE ELECTRON IDENTIFICATION PARAMETERS IF
C  THEY ARE NOT GIVEN BY A BOS CARDS OR IN THE DATA BASE
C
            XE4000=0.825
            E4LRS0=0.18
            R2CUT1=-3.
            R2CUT2=1000.
            FBSA00=0.1892
            SBSA00=0.02852
            FSBSA0=0.0414
            SSBSA0=0.015
            FUSA00=0.258
            SUSA00=0.0184
            FSUSA0=0.025
            SSUSA0=0.085
            R3CUT1=-2.4
            R3CUT2=3.0
            R4CUT1=-10.
            R4CUT2=10.
            R1CUT1=-2.0
            R1CUT2=2.0
            C1BSA0=0.00205
            C2BSA0=0.
            C1USA0=0.
            C2USA0=0.
            C1SBSA=0.00184
            C2SBSA=0.
            C1SUSA=0.
            C2SUSA=0.
            SATURE=8.E-4
          ENDIF
        ENDIF
        IF(IND.NE.0)THEN
          INEIB=IND+2
          XE4000=RW(INEIB+JEIBE4)
          E4LRS0=RW(INEIB+JEIBER)
          R2CUT1=RW(INEIB+JEIBR1)
          R2CUT2=RW(INEIB+JEIBR2)
          FBSA00=RW(INEIB+JEIBB1)
          SBSA00=RW(INEIB+JEIBB2)
          FSBSA0=RW(INEIB+JEIBS1)
          SSBSA0=RW(INEIB+JEIBS2)
          FUSA00=RW(INEIB+JEIBU1)
          SUSA00=RW(INEIB+JEIBU2)
          FSUSA0=RW(INEIB+JEIBSO)
          SSUSA0=RW(INEIB+JEIBST)
          R3CUT1=RW(INEIB+JEIBR3)
          R3CUT2=RW(INEIB+JEIBR4)
          R4CUT1=RW(INEIB+JEIBR5)
          R4CUT2=RW(INEIB+JEIBR6)
          R1CUT1=RW(INEIB+JEIBR7)
          R1CUT2=RW(INEIB+JEIBR8)
          C1BSA0=RW(INEIB+JEIBC1)
          C2BSA0=RW(INEIB+JEIBC2)
          C1USA0=RW(INEIB+JEIBC3)
          C2USA0=RW(INEIB+JEIBC4)
          C1SBSA=RW(INEIB+JEIBC5)
          C2SBSA=RW(INEIB+JEIBC6)
          C1SUSA=RW(INEIB+JEIBC7)
          C2SUSA=RW(INEIB+JEIBC8)
          SATURE=RW(INEIB+JEIBSU)
        ENDIF
        RETURN
        END
#endif
