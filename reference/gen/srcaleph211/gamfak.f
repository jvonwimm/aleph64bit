      SUBROUTINE GAMFAK( IGAM, PFAKE )
C.----------------------------------------------------------------------
C   M.Verderi       Creation 26/09/94
C   Returns the real array PFAKE(4) = output of fake photon analysis
C   Input : IGAM  photon number of last gampex call.
C   Output:
C           PFAKE (01) Fake photon likelihood with electro. hypothesis
C           PFAKE (02) Fake photon likelihood with hadroni. hypothesis
C           PFAKE (03) Float ( 1000 * Iwarn(Electro) + IWarn(Hadron) )
C           PFAKE (04) Float ( Gampex photon number of parent derived
C                      the electrom. analysis)
C
C   Called by USER
C.----------------------------------------------------------------------
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
      REAL PFAKE(4)
C
C
      PFAKE ( 1) = PF (  1, IGAM )
      PFAKE ( 2) = PF (  4, IGAM )
      PFAKE ( 3) = 1000. * PF (  3, IGAM ) + PF (  5, IGAM )
      PFAKE ( 4) = PF (  2, IGAM )
      RETURN
      END
