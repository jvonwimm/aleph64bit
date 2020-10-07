      INTEGER FUNCTION VDCFLT(IDATC,IVIEW,ISTRS,IFLTC)
C
CKEY VDETDES INDEX / USER
C!  Returns strips, fault code
C - Joe Rothberg, June 1995
C
C - Input:
C   IDATC   / I  Data Channel
C   IVIEW   / I  View
C
C
C - Output:
C   ISTRS(3) / I  strips
C   IFLTC(3) / I  fault code by wafer
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C ----------------------------------------------------------------------
#include "velchn.h"
#include "vglobl.h"
#include "vsltco.h"
#include "vduefc.h"
C --------------------------------------------------------
C   Arguments:
      INTEGER  IWFRS(3),ISTRS(3),IFLTC(3), MMOD, IVIEW, IDATC
C
      INTEGER I
C ---------------------------------------------------------
      IF(IDATC .LE. 0 .OR. IDATC .GT. 1023) THEN
         VDCFLT = VDERR
         DO I =1,3
           ISTRS(I) = -1
           IFLTC(I) = -1
         ENDDO
         GOTO 999
      ENDIF
C ----------------------------------------------------------
      VDCFLT = VDOK
C
      IF(IVIEW .EQ. vviewz) THEN
        DO I=1,3
          ISTRS(I) =  IELCHZ(IDATC,I)
          IFLTC(I) =  IELCHZ(IDATC,I+3)
        ENDDO
C
C correct r phi view for readout order when finding strips (temporary method)
      ELSEIF(IVIEW .EQ. vviewp) THEN
        DO I=1,3
          ISTRS(I) = 1022 - IELCHP(IDATC,I)
          IFLTC(I) =  IELCHP(IDATC,I+3)
        ENDDO
C
      ELSE
C
        VDCFLT = VDERR
C
      ENDIF
C
 999  RETURN
      END
#endif
