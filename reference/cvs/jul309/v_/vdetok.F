      LOGICAL FUNCTION VDETOK(DUMMY)
C----------------------------------------------------------------------
C!  - Determine HV state of VDET
C! 
C! Obsolete, call XVDEOK directly
C!
C!   Author   :- HCJ Seywerd            2-AUG-1991
C?
C!======================================================================
#ifndef DOC
      REAL DUMMY
      LOGICAL XVDEOK
      EXTERNAL XVDEOK

      VDETOK = XVDEOK(dummy)

      END
#endif
