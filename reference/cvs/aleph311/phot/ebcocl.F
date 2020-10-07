       SUBROUTINE EBCOCL(METH,NUST,PHOT,CORR)
C--------------------------------------------------------------------------
CKEY PHOTONS CORRECTION STOREY THRESHOLD / INTERNAL
C
C       AUTHOR :        JP Albanese - 891010
C                       R.Edgecock  - 900115
C! Compute correction between cluster energy and total energy deposited
C! in ECAL. For now, use the same idea as the original, and calculate
C! the required parameters on the first entry only. This can be modified
C! later if the thresholds start varying between modules.
C
C   Input     :    METH Option index.
C                  NUST      Number of storeys
C                  PHOT      Raw energy , stacks content ,...
C
C   Output    :    CORR      Correction factor.
C
C   Banks     Input     RUNH
C
C--------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "runhjj.h"
      REAL CLCCOF(2,3)
      DIMENSION PHOT(*)
      INTEGER KODE(4),NREG(3)
      SAVE ISTAR,CLCCOF,IRUNKP,NRUNH
C     for safety SAVE all
      SAVE
      DATA ISTAR /0/
      DATA IRUNKP /0/
      DATA NRUNH /0/
C
      CORR = 1.
      IF( METH .EQ. 1 ) GO TO 98
C Coefficients may change between runs, so put a private run change
C check here
C
      IF (NRUNH.EQ.0) NRUNH=NAMIND('RUNH')
      KRUNH=IW(NRUNH)
      IF (KRUNH.EQ.0) THEN
         IRUNRC = 2001
      ELSE
         IRUNRC = IW(KRUNH+JRUNRN)
      ENDIF
C
      IF (IRUNKP.NE.IRUNRC) ISTAR = 0
C
      IF (ISTAR .EQ. 0) THEN
        ISTAR = 1
        IRUNKP = IRUNRC
C Find the correction coefficients
        CALL EBCLCF(CLCCOF)
      ENDIF
C
C Find out which subcomponent this cluster is in and use the appropriate
C coefficients
C
      CALL EBCDRG(PHOT(5),PHOT(6),ITRW,JFCL,KODE,NREG,IRC)
      IF (IRC.EQ.0) THEN
         ISUBC = NREG(1)
      ELSE
         ISUBC = 2
      ENDIF
C
      ENER = ABS( PHOT(1) )
      ECOCL = CLCCOF(1,ISUBC)/SQRT(ENER) + CLCCOF(2,ISUBC)
      CORR = 1. + ECOCL
   98 CONTINUE
C
       RETURN
       END
#endif
