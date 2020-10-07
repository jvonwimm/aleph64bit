      SUBROUTINE VHUPCK(ID, WAFER, IHIT)
C ---------------------------------------------------------------------
C! Unpack the wafer number and hit number from a unique ID
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 1-11-94
C
C  INPUT:  integer ID    - pack hit ID
C OUTPUT:  integer WAFER - VDet wafer number
C          integer IHIT  - hit number within wafer
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE

      INTEGER WAFER, IHIT, ID

      WAFER = ID / 1000
      IHIT = ID - WAFER*1000

      RETURN
      END
#endif
