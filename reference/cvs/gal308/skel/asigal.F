*DK asigal
      SUBROUTINE ASIGAL
C ---------------------------------------------------------------------
C! GALEPH initialization routine
C - F.Ranjard - 950425
C -------------------------------------------------------------------
#ifndef DOC
      PARAMETER(LIW=1000000, LNAMES=500)
      COMMON /BCS/   IW(LIW)
      CHARACTER*80 FNAME
C ------------------------------------------------------------------
C
C - Initialize ALEPH memory manager
C
      CALL BNAMES (LNAMES)
      CALL BOS (IW,LIW)
      IW(5) = 7
      IW(8) = 100000
C
C - initialize RANECU
C
      DUMMY = RECUST()
C
C - job initialization : initialize variables and then
C   read STEERING data cards to set necessary flags to run the job
C   according to flags defined previously get the
C   right constants from tape or data base or data cards.
C - Interface to GEANT3 : fill GEANT3 common blocks
C - Build the geometry in the GEANT3 framework
C - Initialize various packages which could be used during the run
C   such as :GD3, LUND, GHEISHA, ect....
C
      FNAME = ' '
      CALL GETENVF ('GALEPHCARDS',FNAME)
      IF (FNAME.NE.' ') CALL AOPEN (IW(5),FNAME,'CARD','DISK',IER)
      CALL ASIJOB
C
      END
#endif
