      SUBROUTINE QJLUCL74 (NJETS,CNAM,ICLASS,MINJET,DMAX1,DMAX2,MULSYM,
     &                   TGEN,DMIN)
CKEY JETS /USER
C----------------------------------------------------------------------
C Author   : J. von Wimmersperg       26-AUG-2019
C
C   Description
C   ===========
C!   Set up and call QGLUCL74 Jet finding routine. (LUCLUS jetset7.4 versn)
C input :
C         MINJET min. # of jets to be reconstructed (usually=1)
C         DMAX1 max. distance to form starting clusters(usually=0.25GeV)
C         DMAX2  "      "     to join 2 clusters       (usually=2.5 GeV)
C         MULSYM = 1 symmetric distance criterion + reassignment (usual)
C                = 2 symmetric distance criterion no reassignment
C                = 3 Mass distance meas., fixed max cluster mass (usual)
C                = 4 Mass distance meas., scaled (Evis) max cluster mass
C                = 5 Durham distance measure
C         *** note only MULSYM = 1 has reassignment procedure
C-----------------------------------------------------------------------------
C         also NOTE convenient way to force to NJETS by setting DMAX2=-NJETS 
C-----------------------------------------------------------------------------
C         KTBI            # of particles           (from QCTBUF)
C         QTBIX,Y,Z,E(i)  four momentum (i=1,KTBI) ( "     "   )
C output: NJET number of reconstructed jets
C              = -1 if not enough particles
C              = -2 not enough working space (MXWORK)
C              = -3 failed to reconstruct as requested
C         TGEN generalized thrust = -1 if failed
C         DMIN minimum distance between 2 jets = -1 if failed
C         QCTBUF.INC contains jet momenta and track-jet association
C======================================================================
#ifndef DOC
      CHARACTER *(*) CNAM
C-----------------------------------------------------------------------
      CALL QJSETU(ICLASS)
C
      CALL QGLUCL74(MINJET,DMAX1,DMAX2,MULSYM,
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
#endif
