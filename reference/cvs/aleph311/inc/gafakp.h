C! Arrays used in fake photon analysis:
      PARAMETER ( NFPHO1 = 20 )
      COMMON / GAFAKE / PF (5,NFPHO1)
C  Additional informations:
      COMMON / GGMORE / GAMORE(21,NFPHO1)
C --- max number of cluster per PECO cluster
      PARAMETER ( NKLUM1 = 50     )
      DIMENSION GFL   (10,NKLUM1)
      DIMENSION GAMFL (28),      IGAMFL(28)
      EQUIVALENCE(GAMFL,IGAMFL)
C
