      SUBROUTINE EDTRGI(KTHEB)
C------------------------------------------------------------------
C    G.de BOUARD   4-OCT-87
C! Initialisation of trigger region theta limits in ECAL
C
C. - called from ECTRIG                              this .HLB
C------------------------------------------------------------------
      SAVE
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
      DIMENSION KTHEB(NTHSG)
C
      KTHEB( 1) = 8
      KTHEB( 2) = 24
      KTHEB( 3) = 46
      KTHEB( 4) = 50
      KTHEB( 5) = 88
      KTHEB( 6) = 114
      KTHEB( 7) = 140
      KTHEB( 8) = 178
      KTHEB( 9) = 182
      KTHEB(10) = 204
      KTHEB(11) = 220
      KTHEB(12) = 228
C
      RETURN
      END