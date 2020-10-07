*DK ecdfcd
      SUBROUTINE ECDFCD
C.-----------------------------------------------------------------
C  M.Rumpf may 86                            modification may 87
C!   Modified :- E. Lancon              3-DEC-1990
C! ECAL Run cond. interface
C  Correspondance  Integers <--> Characters
C  Can be changed using Steering data cards
C           RUNC   'ECAL' n1  n2 ....    where n1 etc are integers
C  - Called by ECIRUN
C.------------------------------------------------------------------
C
C     EXTERNAL   INTERNAL                      VALUES
C     Integer    Character     Default
C     --------------------------------------------------------------
C      ICECJO                     0                1            2
C     --------------------------------------------------------------
C        (1)     TCUTRK       NO CUT           CUT TRACK        -
C        (2)     TSMEAR       NO SMEARING      SMEARING         -
C        (3)     TIRAGE       FLUCTUATED       VERY FAST        -
C        (4)     TEDEPO       FLUCTUATED       VERY FAST        -
C        (5)     TPARAM       NO               ELEC           ALL
C        (6)     EDNOXT       +ONE             HITS           ALL
C        (7)     EDMIXR       NO               YES              -
C        (8)     EDSAVG       NO               YES              -
C        (9)     EDZSUP       SINGLE CUT       DOUBLE CUT       -
C       (10)     not used         -                -            -
C     ------------------------------------------------------------
#ifndef DOC
#include "ehcond.h"
#include "edcond.h"
#include "jobcom.h"
C
C For Analog Signals
C
      IF( ICECJO(1).EQ.0)     TCUTRK = 'NO CUT'
      IF( ICECJO(1).EQ.1)     TCUTRK = 'CUT TRACK'
      IF( ICECJO(2).EQ.0)     TSMEAR = 'NO SMEARING'
      IF( ICECJO(2).EQ.1)     TSMEAR = 'SMEARING'
      IF( ICECJO(3).EQ.0)     TIRAGE = 'FLUCTUATED'
      IF( ICECJO(3).EQ.1)     TIRAGE = 'VERY FAST'
      IF( ICECJO(4).EQ.0)     TEDEPO = 'FLUCTUATED'
      IF( ICECJO(4).EQ.1)     TEDEPO = 'VERY FAST'
      IF( ICECJO(5).EQ.0)     TPARAM = 'NO'
      IF( ICECJO(5).EQ.1)     TPARAM = 'ELEC'
      IF( ICECJO(5).EQ.2)     TPARAM = 'ALL'

C
C For Digitization
C
      IF( ICECJO(6).EQ.0)     EDNOXT = '+ONE'
      IF( ICECJO(6).EQ.1)     EDNOXT = 'HITS'
      IF( ICECJO(6).EQ.2)     EDNOXT = 'ALL'
      IF( ICECJO(7).EQ.0)     EDMIXR = 'NO'
      IF( ICECJO(7).EQ.1)     EDMIXR = 'YES'
      IF( ICECJO(8).EQ.0)     EDSAVG = 'NO'
      IF( ICECJO(8).EQ.1)     EDSAVG = 'YES'
      IF( ICECJO(9).EQ.0)     EDZSUP = 'SINGLE CUT'
      IF( ICECJO(9).EQ.1)     EDZSUP = 'DOUBLE CUT'
C
      END
#endif
