      INTEGER FUNCTION VDMWRS (IDATC,IVIEW,MMOD,IWFRS,ISTRS)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Calculates the strips and wafers for given data channel,view,module.
C!  returns ALL wafers and readout strips. (3 for rphi; 2 for z). VDET95
C!  This will contain bonding error information.
C - Joe Rothberg, August 1995
C
C - Input:
C   IDATC  / I  Data Channel number
C   IVIEW  / I  View
C   MMOD   / I  signed module number
C
C - Output:
C   IWFRS(3)   / I  Local wafer index (2 or 3 )
C   ISTRS(3)   / I  Readout Strips (2 or 3 )
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
C ----------------------------------------------------------------------
#include "vglobl.h"
#include "vsltco.h"
#include "vduefc.h"
C ----------------------------------------------------------------------
C
C     Arguments:
      INTEGER  IWFRS(3), ISTRS(3), MMOD, IVIEW, IDATC
C
C     Local variables
      INTEGER IRET, JMOD, IWAF, IROS, IWAF2, IROS2, I
C
C     External references:
      INTEGER  VJMODM, VDACRS, VZRSRS
C ----------------------------------------------------------------------
C
      VDMWRS = VDERR
C
      IF ((MMOD.GE.-NSLOTS).AND.(MMOD.LE.NSLOTS).AND.(MMOD.NE.0)) THEN
        JMOD =  VJMODM(MMOD)
        IF(IVIEW .EQ. 1 .OR. IVIEW .EQ. 2) THEN
C first wafer
           IRET = VDACRS (IDATC,IVIEW,IWAF,IROS)
           IF(IRET .EQ. VDOK) THEN
C find partners
              IF(IVIEW .EQ. VVIEWP) THEN
                 DO I = 1,3
                    IWFRS(I) = I
                    ISTRS(I) = IROS
                 ENDDO
                 VDMWRS = VDOK
              ELSEIF (IVIEW .EQ. VVIEWZ) THEN
                 IRET =  VZRSRS(IWAF,IROS,IWAF2,IROS2)
                    IF(IRET .EQ. VDOK) THEN
                       IWFRS(1) = IWAF
                       ISTRS(1) = IROS
                       IWFRS(2) = IWAF2
                       ISTRS(2) = IROS2
                       VDMWRS = VDOK
                    ENDIF
              ENDIF
           ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
#endif
