      SUBROUTINE QJLUCL (NJETS,CNAM,ICLASS,MINCLU,DMAX1,DMAX2,MULSYM,
     &                   TGEN,DMIN)
CKEY JETS /USER
C----------------------------------------------------------------------
C   Author   : P. Perez       20-APR-1989
C
C   Description
C   ===========
C!   Set up and call QGLUCL Jet finding routine. (LUCLUS)
C  input:
C         MINCLU min. # of clusters to be reconstructed
C                 (if <0, work space momenta are used as a start)
C                 (usually=1)
C         DMAX1 max. distance to form starting clusters(usually=0.25GeV)
C         DMAX2  "      "     to join 2 clusters       (usually=2.5 GeV)
C         MULSYM = 1 for symmetric distance criterion (usual)
C                = 2  "  multicity    "
C output: NJET number of reconstructed jets
C              = -1 if not enough particles
C              = -2 not enough working space (KTBOMX
C         TGEN generalized thrust
C         DMIN minimum distance between 2 jets
C              = 0  when only 1 jet
C              = -1 , -2 as for NJET
C         QCTBUF.INC contains jet momenta and track-jet association
C======================================================================
      CHARACTER *(*) CNAM
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGLUCL(MINCLU,DMAX1,DMAX2,MULSYM,
     &            NJETS,TGEN,DMIN)
C
      IF (CNAM.NE.' ')  THEN
        IDR=0
        DO 1 IJET = NJETS, 1, -1
          CALL QJSAVE (CNAM,IJET,ICLASS,IDR,0,1)
          IDR=1
    1   CONTINUE
      ENDIF
C
      END
