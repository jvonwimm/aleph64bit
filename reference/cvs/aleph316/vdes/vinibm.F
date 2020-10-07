      SUBROUTINE VINIBM(IRUN,IFLAG)
C ----------------------------------------------------------------
CKEY VDETDES INDEX / USER
C----------------------------------------------------------------------
C! Initialization of VDET geography and Bonding Maps
C!   Author   :- J.Rothberg                   13-OCT-1995
C!
C!   Inputs:
C!   IRUN   / I run number
C!
C!   Outputs:
C!   IFLAG  / I = 1 if subroutine ended successfully
C!              = 0 if an error occurred
C!                  in this case the program should STOP
C!   Libraries required:
C!
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
C -------------------------------------------------------
#include "vglobl.h"
#include "vgindx.h"
C
C Arguments
      INTEGER IRUN, IFLAG
C
C Local Variables
      INTEGER IRET, JMOD, ISMOD, IGET
      INTEGER LOUT, LUNDAF
C Functions
      INTEGER JUNIDB, VINIVG, GTSTUP, VSMJMD, VDARGE
C --------------------------------------------------------
      IFLAG = 0
      LOUT = 6
      LUNDAF = JUNIDB(0)
C
C     Get setup code and read the database banks if necessary:
C
      IGET = GTSTUP('VD',IRUN)
C
      IF (IGET .LE. 0) THEN
C       Something went wrong...
        GO TO 999
      ENDIF
C ------------------------------------------------------------------
C read data base
      IRET = VDARGE(LUNDAF,IGET)
      IF( IRET .NE. VDOK) THEN
         GOTO 999
      ENDIF
C ------------------------------------------------------------------
C initialize geography
      IRET = VINIVG()
      IF( IRET .NE. VDOK) THEN
         GOTO 999
      ENDIF
C --------------------------------------------------
C Banks VMBE,    bonding errors
C read unpack banks; store in  VMBU
        CALL VRVMBE
C
C store errors in arrays by global module number
       IRET = GTSTUP('VD',irun)
       DO JMOD = 1, NMODUL
         IRET = VSMJMD(JMOD,ISMOD)
         IF(IRET .EQ. VDOK) THEN
             CALL VGTBER(ISMOD,JMOD)
         ENDIF
C
       ENDDO
C ---------------------------------------------------------
       IFLAG = 1
C
  999 CONTINUE
      END
#endif
