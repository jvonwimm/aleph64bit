      LOGICAL FUNCTION MUTRGS (IDUMMY)
C***********************************************************
CKEY EDIR CLASS26
C! Muon tagging through trigger bits                       *
C  Author: R.Tenchini  920416                              *
C  Modified: M.Cattaneo, 960530                            *
C     For LEP200, ask for 2 ITC track masks instead of     *
C     vetoing with SNG_C_EM trigger bit                    *
C                                                          *
C  INPUT Banks : IXTR + uses ALTRIG, GTSTUP                *
C  OUTPUT Banks : none                                     *
C                                                          *
C  INPUT Arguments :                                       *
C                                                          *
C  IDUMMY = Dummy Argument                                 *
************************************************************
#ifndef DOC
#include "bcs.h"
      LOGICAL BTEST
      INTEGER ALTRIG, NAMIND, GTSTUP
      EXTERNAL ALTRIG, NAMIND, GTSTUP
      INTEGER NAIXTR, IROLD, ICODE
      SAVE NAIXTR, IROLD, ICODE
      DATA NAIXTR / 0 /, IROLD / 0 /
C
      MUTRGS=.FALSE.
      IALT=ALTRIG(IDUM,INFO,INF1)
      IF(IALT.EQ.0) RETURN
C
C Get the beam setup code once per run
      CALL ABRUEV( IRUN, IEVT )
      IF( IRUN .NE. IROLD ) THEN
        ICODE = GTSTUP( 'BE', IRUN )
        IF( ICODE .EQ. -1 ) RETURN
        IROLD = IRUN
      ENDIF
C
C Decide if LEP1 or LEP2 from beam setup code. LEP2 defined to start June 96
      IF( ICODE .LT. 7 ) THEN 
C LEP1
        IF((BTEST(INFO,8).AND.BTEST(INF1,8)).AND.
     &     (BTEST(INFO,27).AND.BTEST(INF1,27)).AND.
     &    .NOT.(BTEST(INFO,9).AND.BTEST(INF1,9))) MUTRGS=.TRUE.
      ELSE
C LEP2
        IF( NAIXTR .EQ. 0 ) NAIXTR = NAMIND( 'IXTR' )
        IF( NAIXTR .EQ. 0 ) RETURN
        KIXTR = IW( NAIXTR )
        IF( KIXTR .EQ. 0 ) RETURN
C
        nITtrk = IAND( IW(KIXTR+2+62),'FF'x )
        IF((BTEST(INFO,8).AND.BTEST(INF1,8)).AND.
     &     (BTEST(INFO,27).AND.BTEST(INF1,27)).AND.
     &     nITtrk.EQ.2 ) MUTRGS=.TRUE.
      ENDIF

      RETURN
      END
#endif

