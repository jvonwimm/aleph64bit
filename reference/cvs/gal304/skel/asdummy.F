*DK asdummy
      SUBROUTINE ASKUSE (IDEV,ISTA,NITR,NIVX,ECMS,WEIT)
C ----------------------------------------------------------------------
C! calls the user event generator
C
C - Output arguments:
C          IDEV     = event process identification
C          ISTA     = event status word  ( 0 means OK )
C          NITR     = # of generated tracks
C          NIVX     = # of generated vertices
C          ECMS     = beam energy
C          WEIT     = event weight
C ---------------------------------------------------------------------
#ifndef DOC
      SAVE
      RETURN
      END
#endif
      SUBROUTINE ASKUSI (IGCOD)
C ----------------------------------------------------------------------
C! initializes the user event generator
C
C - Output arguments:
C          IGCOD     = generator code
C ---------------------------------------------------------------------
#ifndef DOC
      RETURN
      END
#endif
