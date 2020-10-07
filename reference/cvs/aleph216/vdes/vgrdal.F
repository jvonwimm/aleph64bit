      SUBROUTINE VGRDAL (LUN,IRUN,IFLAG)
C ----------------------------------------------------------------------
CKEY VDETDES GEOMETRY
C!  Read alignment banks and fill the VGPAAL common
C - Francesco Forti, 18 August 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C  Check existence of the alignment banks in the BOS common and their
C  validity range.
C  If they do not exist yet or are no longer valid try to load them
C  from the data base file. IFLAG is set to 0 if an error occurs.
C  The routine uses the alignment banks to compute the complete
C  transformation that goes from the wafer coordinate to the ALEPH
C  main coordinates and vice versa and stores it in the common VGPAAL.
C
C  Called by :    VDET initialisation routine
C  Calls     :    FUNCTION ALGTDB               from ALEPHLIB
C                 VGEXRO, VGCMTR, VGINTR        from ALEPHLIB
C
C - Input:
C   LUN   / I  Logical unit number of DAF file
C   IRUN  / I  Run number
C
C - Output:
C   IFLAG / I  = 1 if routine ends successfully;
C              = 0 if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vagbjj.h"
#include "valcjj.h"
#include "vgpaal.h"
#include "bcs.h"
      INTEGER LUN, IRUN, IFLAG
      INTEGER ALGTDB,GTSTUP
      EXTERNAL ALGTDB,GTSTUP
      INTEGER I, IRO, ILAY, IWFF, IFAC, IVIEW, IND, NAMIND
      INTEGER VNRFAC
      INTEGER KLC, KGB, IFAIL
      REAL VLOC(LVALCA)
#include "bmacrod.h"
C
      CHARACTER*8 LIST1
      SAVE LIST1
      DATA LIST1 /'VAGBVALC'/
C
C     Transformation structures to be used for intermediate results:
C
      REAL TELC(LVTEXP), TEGB(LVTEXP), TENO(LVTEXP), TE(LVTEXP)
C
#include "bmacro.h"
C ----------------------------------------------------------------------
C
      IFLAG = 1
C
C! Get banks in LIST1 from DB depending on run and setup code
C
      IF (IRUN.LE.2000) THEN
         ITP = GTSTUP ('VD',IRUN)
      ELSE
         ITP = IRUN
      ENDIF
      IND = ALGTDB(LUN,LIST1,-ITP)
C
C     Return if banks have not been correctly accessed:
C
      IF (IND .EQ. 0) THEN
        IFLAG = 0
        GO TO 999
      ENDIF
C
C     Get the indices to banks just read in:
C
      KLC = IW(NAMIND('VALC'))
      KGB = IW(NAMIND('VAGB'))
      IF ((KLC .LE. 0) .OR. (KGB .LE. 0)) THEN
        IFLAG = 0
        GO TO 999
      ENDIF
C
C     Initialize the matrices to zero:
C
      CALL VZERO(VTEXPD,LVTEXP*NVFLMX*NVWFMX*NVLAYR)
      CALL VZERO(VTEXPI,LVTEXP*NVFLMX*NVWFMX*NVLAYR)
C
C     Construct the global transformation:
C
      CALL VGEXRO(1,RW(KROW(KGB,1)+JVAGTR),TEGB)
C
C     Loop on the rows of VALC to get transformation of each wafer:
C
      DO IRO=1,LROWS(KLC)
C
C     Decode the wafer index, and make sure this is a valid wafer:
C
        CALL VADEWA(ITABL(KLC,IRO,JVALWI),ILAY,IWFF,IFAC,IVIEW)
        IF (IFAC .LE. VNRFAC(ILAY)) THEN
C
C Modify local alignment for face bending
C
          CALL UCOPY(RW(KROW(KLC,IRO)+JVALTR),VLOC,LVALCA-1)
          CALL VALFCO(VLOC,ILAY,IWFF,IFAC,IRUN)
C
C     Local alignment of the wafer:
C
          CALL VGEXRO(2,VLOC,TELC)
C
C     Nominal transformation for the wafer:
C
          CALL VGGTNO(ILAY,IWFF,IFAC,TENO)
C
C     Compute the total transformation for the wafer:
C
          CALL VGCMTR(TELC,TENO,TE)
          CALL VGCMTR(TE,TEGB,VTEXPD(1,IFAC,IWFF,ILAY))
C
C     Compute the inverse transformation:
C
          CALL VGINTR(VTEXPD(1,IFAC,IWFF,ILAY),
     &                VTEXPI(1,IFAC,IWFF,ILAY),IFAIL)
          IF (IFAIL .NE. 0) THEN
            IFLAG = 0
          ENDIF
C
        ENDIF
      ENDDO
C
C     Calculate the distance to wafer midplanes:
C
      CALL VGCADI
C
C     Routine is terminated and flag has been set properly.
C
  999 CONTINUE
      RETURN
      END
#endif
