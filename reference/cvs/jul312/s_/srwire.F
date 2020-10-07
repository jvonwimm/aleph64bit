      SUBROUTINE SRWIRE(JCRA,JCAR,JTDC,IOR,ISIDE,ILAYE,ISECT,IWIRE)
C----------------------------------------------------------------------
C! Calculate wire and sector numbers
C!
C!    Author:     H. Meinhard       02-Sep-1987
C!    Modified:   H. Meinhard       07-Mar-1991  (2)
C!
C!    Input:        - JCRA    /I      crate number (1...3)
C!                  - JCAR    /I      card number (1...24)
C!                  - JTDC    /I      TDC number on card (1...16)
C!                  - IOR     /I      number of remaining ored wires
C!    Output:       - ISIDE   /I      side number (1...2)
C!                  - ILAYE   /I      layer number (1...9)
C!                  - ISECT   /I      sector number (1...8)
C!                  - IWIRE   /I      wire number (1...14)
C!                  - IOR     /I      number of remaining ored wires
C!
C!    Description
C!    ===========
C!    This routine contains the mapping from electronics addresses
C!    (crate, card, TDC) to the wire positions (side, layer, sector,
C!    wire). The conversion depends on the oring mode; so some details
C!    of the oring modes are encoded in this routine.
C!    If a fired channel ores wires, IOR is set to the number of
C!    wires still to be described, thus indicating that this routine
C!    must be called once more in order to get the complete mapping
C!    information.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sgeomc.h"
#include "sanamc.h"
#include "swtujj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C check whether data come from final setup or from test setup
      IF (MDORSG(1) .GT. 0) THEN
C
C ---> final setup
C calculate side, layer
        ISIDE = (JCAR-1) / (MCRDSG/2) + 1
        JCAR1 = MOD(JCAR-1,MCRDSG/2) + 1
        ILAYE = JCRA + MCRASG*((JCAR1-1)/MGRSG)
        IGROU = MOD(JCAR1-1,MGRSG) + 1
        MODE  = MDORSG(ILAYE)
C
C calculate sector, wire
C Simple oring: two wires of same number in same half layer
        IF (MODE .GT. 0 .AND. MODE .LT. 10) THEN
          IWIRE = JTDC
          IF (IOR .EQ. 0) THEN
            ISECT = KSECSG(1,IGROU,MODE)
            IOR = 1
          ELSE
            ISECT = KSECSG(2,IGROU,MODE)
            IOR = 0
          ENDIF
        ENDIF
C
C Innermost two wires ored fourfold, 3...5 not ored, 6...14 ored two-
C fold
        IF (MODE .GT. 10 .AND. MODE .LT. 20) THEN
          IF (JTDC .EQ. 1) THEN
            IF (MOD(IGROU,2).NE.0) IWIRE = 1
            IF (MOD(IGROU,2).EQ.0) IWIRE = 2
            IF (IGROU .LE. MGRSG/2) THEN
              IADD = 0
            ELSE
              IADD = MSECSG/2
            ENDIF
            IF (IOR .EQ. 0) THEN
              ISECT = 1 + IADD
              IOR = 3
            ELSE
              ISECT = 5 - IOR + IADD
              IOR = IOR - 1
            ENDIF
          ENDIF
          IF (JTDC .EQ. 2 .OR. JTDC .EQ. 3 .OR. JTDC .EQ. 4) THEN
            IWIRE = JTDC+1
            ISECT = 2*IGROU-1
          ENDIF
          IF (JTDC .EQ. 5 .OR. JTDC .EQ. 6 .OR. JTDC .EQ. 7) THEN
            IWIRE = JTDC-2
            ISECT = 2*IGROU
          ENDIF
          IF (JTDC .GE. 8) THEN
            IWIRE = JTDC-2
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-10)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-10)
              IOR = 0
            ENDIF
          ENDIF
        ENDIF
C
C Wires 1, 2, 5...14 ored twofold, wires 3, 4 not ored
        IF (MODE .GT. 20 .AND. MODE .LT. 30) THEN
          IF (JTDC .GE. 1 .AND. JTDC .LE. 2) THEN
            IWIRE = JTDC
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-20)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-20)
              IOR = 0
            ENDIF
          ELSE IF (JTDC .EQ. 3) THEN
            IWIRE = 3
            ISECT = KSECSG(1,IGROU,MODE-20)
          ELSE IF (JTDC .EQ. 4) THEN
            IWIRE = 3
            ISECT = KSECSG(2,IGROU,MODE-20)
          ELSE IF (JTDC .EQ. 5) THEN
            IWIRE = 4
            ISECT = KSECSG(1,IGROU,MODE-20)
          ELSE IF (JTDC .EQ. 6) THEN
            IWIRE = 4
            ISECT = KSECSG(2,IGROU,MODE-20)
          ELSE IF (JTDC .GE. 7) THEN
            IWIRE = JTDC - 2
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-20)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-20)
              IOR = 0
            ENDIF
          ENDIF
        ENDIF
C
C Wires 1...3, 6...14 ored twofold, wires 4, 5 not ored
        IF (MODE .GT. 30 .AND. MODE .LT. 40) THEN
          IF (JTDC .GE. 1 .AND. JTDC .LE. 3) THEN
            IWIRE = JTDC
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-30)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-30)
              IOR = 0
            ENDIF
          ELSE IF (JTDC .EQ. 4) THEN
            IWIRE = 4
            ISECT = KSECSG(1,IGROU,MODE-30)
          ELSE IF (JTDC .EQ. 5) THEN
            IWIRE = 4
            ISECT = KSECSG(2,IGROU,MODE-30)
          ELSE IF (JTDC .EQ. 6) THEN
            IWIRE = 5
            ISECT = KSECSG(1,IGROU,MODE-30)
          ELSE IF (JTDC .EQ. 7) THEN
            IWIRE = 5
            ISECT = KSECSG(2,IGROU,MODE-30)
          ELSE IF (JTDC .GE. 8) THEN
            IWIRE = JTDC - 2
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-30)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-30)
              IOR = 0
            ENDIF
          ENDIF
        ENDIF
C
C-------> Insert here code for additional oring modes
C
C fix of sectors with exchanged sockets
        JSWTU = IW(NASWTU)
        IF (JSWTU .NE. 0) THEN
          DO 300 IENT = 1, LROWS(JSWTU)
            IF (ISIDE .EQ. ITABL(JSWTU,IENT,JSWTSI) .AND.
     +          ILAYE .EQ. ITABL(JSWTU,IENT,JSWTLA) .AND.
     +          ISECT .EQ. ITABL(JSWTU,IENT,JSWTSC)) THEN
              IWIRE = MWIRSG + 1 - IWIRE
              GOTO 301
            ENDIF
  300     CONTINUE
  301     CONTINUE
        ENDIF
C
      ELSE
C
C ---> test setups
C CERN SPS tests in 1986, 1987
C calculate side, layer, wire
        IF (MDORSG(1) .GE. -99 .AND. MDORSG(1) .LE. -90) THEN
          ISIDE = 1
          ILAYE = JCAR
          IWIRE = 15 - MOD(JTDC-1,8)
C
C calculate sector
          IF (MDORSG(ILAYE) .EQ. -90) ISECT = (JTDC-1)/8 + 1
          IF (MDORSG(ILAYE) .EQ. -91) ISECT = (JTDC-1)/8 + 2
          IF (MDORSG(ILAYE) .EQ. -92) ISECT = (JTDC-1)/8 + 3
        ENDIF
C
C CERN SPS test in 1988
        IF (MDORSG(1) .GE. -29 .AND. MDORSG(1) .LE. -20) THEN
C
C calculate side, layer
          ISIDE = 1
          JCAR1 = MOD(JCAR-1,MCRDSG/2) + 1
          ILAYE = JCRA + MCRASG*((JCAR1-1)/MGRSG)
          IF (JCAR1 .NE. JCAR) ILAYE = ILAYE - 2
          IGROU = MOD(JCAR1-1,MGRSG) + 1
          MODE = - MDORSG(ILAYE)
C
C calculate sector, wire
          IF (JTDC .GE. 1 .AND. JTDC .LE. 2) THEN
            IWIRE = JTDC
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-20)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-20)
              IF (ILAYE .EQ. 1 .AND. IGROU .EQ. 2) IWIRE = IWIRE + 7
              IOR = 0
            ENDIF
          ELSE IF (JTDC .EQ. 3) THEN
            IWIRE = 3
            ISECT = KSECSG(1,IGROU,MODE-20)
          ELSE IF (JTDC .EQ. 4) THEN
            IWIRE = 3
            ISECT = KSECSG(2,IGROU,MODE-20)
            IF (ILAYE .EQ. 1 .AND. IGROU .EQ. 2) IWIRE = IWIRE + 7
          ELSE IF (JTDC .EQ. 5) THEN
            IWIRE = 4
            ISECT = KSECSG(1,IGROU,MODE-20)
          ELSE IF (JTDC .EQ. 6) THEN
            IWIRE = 4
            ISECT = KSECSG(2,IGROU,MODE-20)
            IF (ILAYE .EQ. 1 .AND. IGROU .EQ. 2) IWIRE = IWIRE + 7
          ELSE IF (JTDC .GE. 7) THEN
            IWIRE = JTDC - 2
            IF (IOR .EQ. 0) THEN
              ISECT = KSECSG(1,IGROU,MODE-20)
              IOR = 1
            ELSE
              ISECT = KSECSG(2,IGROU,MODE-20)
              IF (ILAYE .EQ. 1 .AND. IGROU .EQ. 2)
     +          IWIRE = MOD(IWIRE+6,14) + 1
              IOR = 0
            ENDIF
          ENDIF
        ENDIF
C
C-------> Insert here code for additional oring modes
C
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
