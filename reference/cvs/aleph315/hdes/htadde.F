      SUBROUTINE HTADDE(IADR,ITUB,ILAY,IMOD,IPOR)
C--------------------------------------------------------------
CKEY HCALDES HCAL TUBE ADRESS /USER
C
C! Decode the tube adress for the HCAL in a standard way
C!
C!                                   Author:G.Catanesi 30/5/89
C!
C!
C!         INPUT:
C!                 IADR/I = Tube Adress
C!
C!        OUTPUT:
C!                 ITUB/I = Tube#
C!                 ILAY/I = Layer#
C!                 IMOD/I = Module#
C!                 IPOR/I = Portion#
C!
C------------------------------------------------------
#ifndef DOC
C
      N   = IADR
      ITUB = MOD (N,1000)
      N = N/1000
      ILAY = MOD (N,100)
      N = N/100
      IMOD = MOD (N,100)
      IPOR = N/100
C
      END
#endif
