      SUBROUTINE VGTBER(ISMOD,JMOD)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!     Access unpacked bonding error banks, place into commons
C
C - Joe Rothberg, August 1995
C
C - Input:
C   ISMOD   / I  Module serial number
C   JMOD    / I  Global module number
C
C - Output:
C--------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C -------------------------------------------------------------
C Arguments
      INTEGER JMOD, ISMOD
#include "bcs.h"
#include "vduefc.h"
#include "vbnder.h"
C
C local variables
      INTEGER ie, j, ivview, nerr
      INTEGER  kvmbu, nvmbu
C
C -----------------------------------------------------------
      INTEGER NLINK
#include "bmacrod.h"
#include "bmacro.h"
C ----------------------------------------------------------

C Bank VMBU, unpacked errors

        kvmbu  = 0
        kvmbu = NLINK('VMBU',ismod)

       IF(kvmbu .EQ. 0) THEN
         GOTO 999
       ENDIF
C
       nvmbu = LROWS(kvmbu)
C
       IF(nvmbu .EQ. 0) THEN
         GOTO 999
       ENDIF
C
       numerr(JMOD,1) = 0
       numerr(JMOD,2) = 0
C
       DO ie = 1, MIN(maxerr,nvmbu)
C
         ivview = ITABL(kvmbu,ie,1)
         numerr(JMOD,ivview) = numerr(JMOD,ivview) + 1
         nerr = numerr(JMOD,ivview)
         DO j = 2,6
            ibnerr(JMOD,ivview,nerr,j-1)=ITABL(kvmbu,ie,j)
         ENDDO
C
C convert address from IC/Pisa system to CERN DataChannels
         ibnerr(JMOD,ivview,nerr,1) =
     >        ibnerr(JMOD,ivview,nerr,1) + vexch(ivview)
         ibnerr(JMOD,ivview,nerr,2) =
     >        ibnerr(JMOD,ivview,nerr,2) + vexch(ivview)
C
       ENDDO
C
 999   CONTINUE
C
       RETURN
       END
#endif
