      LOGICAL FUNCTION MUTRGS (IDUMMY)
C***********************************************************
CKEY EDIR CLASS26
C! Muon tagging through trigger bits                       *
C  Author: R.Tenchini  920416                              *
C                                                          *
C  INPUT Banks : none (uses ALTRIG)                        *
C  OUTPUT Banks : none                                     *
C                                                          *
C  INPUT Arguments :                                       *
C                                                          *
C  IDUMMY = Dummy Argument                                 *
************************************************************
      LOGICAL BTEST
      INTEGER ALTRIG
      EXTERNAL ALTRIG
C
      MUTRGS=.FALSE.
      IALT=ALTRIG(IDUM,INFO,INF1)
      IF(IALT.EQ.0) RETURN
      IF((BTEST(INFO,8).AND.BTEST(INF1,8)).AND.
     &   (BTEST(INFO,27).AND.BTEST(INF1,27)).AND.
     &  .NOT.(BTEST(INFO,9).AND.BTEST(INF1,9))) MUTRGS=.TRUE.
      RETURN
      END
