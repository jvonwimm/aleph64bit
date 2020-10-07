      SUBROUTINE VGCADI
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN / INTERNAL
C!  Calculate distances from wafer midplanes to z axis, with alignment
C - Jochen A. Lauber       8-OCT-1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C
C   Initialize the RFDIST array in VGPAAL.  Called by VGRDAL.
C
C - Input:
C   Structures VTEXPD in COMMON VGPAAL
C
C - Output:
C   Array RFDIST in COMMON VGPAAL
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vgpaal.h"
C
C     Local variables
C
      INTEGER IXYZ, IFAC, IWFF, ILAY, JFAC, NFACE, NWAFEF, IRET
      INTEGER VNTFAC, VNRWFF, VIFACI
      LOGICAL VSLOTF
      DOUBLE PRECISION DIST
C
#include "vgvmcr.h"
#include "vgmacr.h"
C ----------------------------------------------------------------------
C
C     Initialize RFDIST:
C
      CALL VZERO(RFDIST,NVFLMX*NVWFMX*NVLAYR)
C
C     Loop over all filled slots:
C
      NWAFEF = VNRWFF()
      NFACE = VNTFAC()
      DO JFAC=1,NFACE
        IF (VSLOTF(JFAC)) THEN
          IRET = VIFACI(JFAC,ILAY,IFAC)
C
C     Loop over wafers in the face:
C
          DO IWFF=1,NWAFEF
C
C     Dot product:
C     first three entries in VTEXPD are vector to wafer center,
C     next three entries are normal vector to wafer.
C
            DIST = 0.D0
            DO IXYZ=0,2
              DIST = DIST +
     >                 DBLE(VTEXPD(JVTETR+IXYZ,IFAC,IWFF,ILAY)) *
     >                 DBLE(VTEXPD(KVINDX(1,1)+IXYZ,IFAC,IWFF,ILAY))
C
            ENDDO
            RFDIST(IFAC,IWFF,ILAY) = SNGL(DIST)
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
#endif
