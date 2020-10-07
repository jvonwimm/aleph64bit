      SUBROUTINE FPIDEN
C------------------------------------------------------------------
C! Fills bank FRID containing bit patterns for tracks and calorimeters
C! and particle ID.
C!
C! This routine is now obsolete and here only for backward compatibility
C! Call FCFRID(1) directly
C!======================================================================
#ifndef DOC
      CALL FCFRID(1)
      RETURN
      END
#endif
