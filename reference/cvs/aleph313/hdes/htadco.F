      SUBROUTINE HTADCO(ITUB,ILAY,IMOD,IPOR,IADR)
C---------------------------------------------------------------
CKEY HCALDES HCAL TUBE ADRESS /USER
C
C! Code the tube adress for the HCAL in a standard way
C!
C!                                   Author:G.Catanesi 30/5/89
C!
C!
C!         INPUT:
C!                 ITUB/I = Tube#
C!                 ILAY/I = Layer#
C!                 IMOD/I = Module#
C!                 IPOR/I = Portion#
C!
C!        OUTPUT:
C!                 IADR/I = Tube Adress
C!
C---------------------------------------------------------------
#ifndef DOC
C
      IADR = ITUB + ILAY*1000 + IMOD*100000 + IPOR*10000000
C
      END
#endif
