      SUBROUTINE CACUTG (MFLAG)
C --------------------------------------------------------------
C - J.Badier  F.Ranjard - 870817
C! decide wheither Yes/No a geantino is created
C - MFLAG = 0 no creation
C           1 creation
C - called from   CALHIT                           from this .HLB
C --------------------------------------------------
C - user stop particle flag
      PARAMETER (NOMOR=3)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      LOGICAL FTINO, FMUON, FELEC, FHADC
C - geantino particle
      FTINO = ITRKEL(11).EQ.6
C - muon particle
      FMUON = ITRKEL(11).EQ.5
C - e+/e- particle
      FELEC = ITRKEL(11).EQ.2
C - charged hadron
      FHADC = ITRKEL(11).EQ.4
C
C ---------------------------------------------------------------
C
       MFLAG = 0
C
      END
