      INTEGER FUNCTION VSCHAN(IFLBE,JMOD,IVIEW,IDATC,ISTRSB,IFLTC)
C ---------------------------------------------------------------
CKEY VDETDES INDEX / USER
C! Returns strip number in each wafer corresponding to Data Channel
C
C   Author   :- J. Rothberg                   12-OCT-1995
C
C   Inputs:
C   IFLBE   / I     0 = nominal strips;  1 = Bonding errors used
C   JMOD    / I     Global Module number
C   IVIEW   / I     View
C   IDATC   / I     Data Channel (CERN convention: 0,1,...)
C
C   Outputs:
C   ISTRSB(3)  /I    Strip numbers in each wafer (1,2,3...)
C   IFLTC(3)   /I    Fault code by wafer (same as VMBE convention)
C
C   Libraries required:
C
C   Description
C   ===========
C     Returns strip number in each wafer corresponding to Data Channel
C           Bonding errors are taken into account.
C
C======================================================================
#ifndef DOC
      IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vglobl.h"
#include "vgindx.h"
#include "vduefc.h"
C ---------------------------------------------------------------------
C Arguments
       INTEGER IFLBE, IVIEW, JMOD, IDATC, ISTRSB(3), IFLTC(3)
C Functions
       INTEGER VDCFLT, VDCWRS
C Local Variables
       INTEGER I, IWFRS(3), ISTRS(3)
       INTEGER IRET, IV
       INTEGER JMODLAST/0/
       SAVE JMODLAST
C -----------------------------------------------------------------
       VSCHAN = VDERR
       IF(JMOD .LE. 0 .OR. JMOD .GT. NMODUL) GOTO 999
       IF(IVIEW .LE. 0 .OR. IVIEW .GT. 2) GOTO 999
       IF(IDATC .LT. 0 .OR. IDATC .GT. 1023) GOTO 999
C
C    IF VIGBM =1 igore bond maps
       IF(IFLBE .EQ. 1 .AND. VIGBM .EQ. 0) THEN
C new module requested, initialize tables
         IF(JMOD .NE. JMODLAST) THEN
            CALL VINIST
            DO IV=1,2
               CALL VCORMP(IV,JMOD)
            ENDDO
            JMODLAST = JMOD
         ENDIF
C
         IRET = VDCFLT(IDATC,IVIEW,ISTRSB,IFLTC)
         VSCHAN = IRET
C
         ELSEIF(IFLBE .EQ. 0 .OR. VIGBM .EQ. 1) THEN
C nominal wafers, strips
            DO I= 1,3
                   IWFRS(I)=-1
                   ISTRSB(I)=-1
                   IFLTC(I) = 0
            ENDDO
C
            IRET = VDCWRS(IDATC,IVIEW,IWFRS,ISTRS)
            DO I = 1,3
               IF(IWFRS(I) .GT. 0) ISTRSB(IWFRS(I)) = ISTRS(I)
            ENDDO
C
            VSCHAN = IRET
         ENDIF
C -------------------------------------------------------------------
  999 RETURN
      END
#endif
