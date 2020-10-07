*DK ehcutp
      SUBROUTINE EHCUTP
C.----------------------------------------------------------------
C R.CLIFFT
C! Fast tracking init
C *** CONSTANTS FOR FAST TRACKING DECISION IN ECAL MODULES
C
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ehfcon.h"
C
C *** CUT OFF ENERGY
C
      CUTFEH=0.04
C
C *** CONSTANTS USED IN PARAMETRISATION OF EDGE PROXIMITY
C
      CEDGEH(1)=0.1676
      CEDGEH(2)=1.1199
      CEDGEH(3)=0.4096
      CEDGEH(4)=-1.5224
C
C *** CONSTANTS FOR PARAMETRISATION DECISION IN STOREYS 1/2
C
      CST1EH(1)=13.1
      CST1EH(2)=51.8
      CST1EH(3)=0.83
C
C *** CONSTANTS FOR PARAMETRISATION DECISION IN BARREL STOREY 3
C
      CBS3EH(1)=6.676
      CBS3EH(2)=6.295
      CBS3EH(3)=5.439
      CBS3EH(4)=3.692
      CBS3EH(5)=33.71
      CBS3EH(6)=32.07
      CBS3EH(7)=28.82
      CBS3EH(8)=21.83
      CBS3EH(9)=1.03
C
C *** CONSTANTS FOR PARAMETRISATION DECISION IN END CAP STOREY 3 ?
C
      CCS3EH(1)=6.912
      CCS3EH(2)=6.107
      CCS3EH(3)=5.075
      CCS3EH(4)=3.667
      CCS3EH(5)=34.54
      CCS3EH(6)=30.88
      CCS3EH(7)=26.58
      CCS3EH(8)=21.27
      CCS3EH(9)=1.03
C
      RETURN
      END
#endif
