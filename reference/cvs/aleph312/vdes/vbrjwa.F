      INTEGER FUNCTION VBRJWA (JWAF, IVIEW, NBROM)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Convert global wafer index JWAF to decimal encoded readout module
C - Manoj Thulasidas, 5 November 1994
C - Modified: Steve Wasserbaech, 29 March 1995
C
C   This function converts a global wafer index JWAF and a view number
C   to the decimal encoded address of the corresponding readout module
C   (encoding scheme as in VAENWA).
C
C - Input:
C   JWAF   / I  Global wafer index
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C
C - Output:
C   VBRJWA / I  = VDOK if successful
C               = VDERR if error occurred
C   NBROM  / I  Decimal encoded readout module address
C ----------------------------------------------------------------------
#ifndef DOC
C      IMPLICIT NONE
#include "vglobl.h"
C
C     Arguments:
C
      INTEGER JWAF, IVIEW, NBROM
C
C     Local variables
C
      INTEGER IRET, ILAY, IFAC, IMOD, IWAF, IROM
      INTEGER VIWAFI, VIROMW

C ----------------------------------------------------------------------
C
      NBROM = -1
      VBRJWA = VDERR
C
C     Convert to local indices:
C
      IRET = VIWAFI(JWAF,ILAY,IFAC,IMOD,IWAF)
C
C     Find the readout module index:
C
      IF (IRET .EQ. VDOK) THEN
        IRET = VIROMW(IMOD,IWAF,IVIEW,IROM)
C
C     Encode the readout module address.
C     We always use IVIEW = VVIEWZ so that the last digit of
C     NBROM will be zero, for use as a bank number:
C
        IF (IRET .EQ. VDOK) THEN
          CALL VAENWA(NBROM,ILAY,IROM,IFAC,VVIEWZ)
          VBRJWA = VDOK
        ENDIF
      ENDIF
C
      RETURN
      END
#endif
