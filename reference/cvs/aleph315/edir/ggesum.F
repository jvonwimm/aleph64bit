      SUBROUTINE GGESUM
C----------------------------------------------------------------------
CKEY EDIR ECAL WIRE GAMMA_GAMMA
C! Calculate Ecal wire energy.
C-
C   Input  : None
C   Output : None
C-
C   Called by   : SELGGE
C   Calls  : None
C   Input banks : PEWI
C   Result put in common EWIR
C-
C                          Authors: M.N. Minard + M. Pepe     15/09/89
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
C --
      COMMON / EWIR / EWIRE ( 36 )
      EXTERNAL NAMIND
      DATA NAPEWI, NAPWEI / 0,0 /
C --
#include "bmacro.h"
C --
      IF( NAPEWI .EQ. 0 ) NAPEWI = NAMIND( 'PEWI' )
      KPEWI = IW(NAPEWI)
      IF (KPEWI.EQ.0) THEN
        IF( NAPWEI .EQ. 0 ) NAPWEI = NAMIND( 'PWEI' )
        KPEWI = IW(NAPWEI)
      ENDIF
C --
      CALL VZERO ( EWIRE,36)
      IF ( KPEWI.EQ.0) GO TO 900
      NROW = LROWS ( KPEWI)
      DO 10 IROW = 1,NROW
        JROW = KROW ( KPEWI,IROW)
        IMOD = IW (JROW+1)
        IF ( IMOD.LT.1.OR.IMOD.GT.36) GO TO 10
        DO 20 IK =1,45
           EWIRE( IMOD ) = EWIRE(IMOD) + FLOAT(IW(JROW+1+IK))/1000000.
 20     CONTINUE
 10   CONTINUE
C --
 900     CONTINUE
C --
         RETURN
         END
#endif
