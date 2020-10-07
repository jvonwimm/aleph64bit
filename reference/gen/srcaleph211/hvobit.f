        SUBROUTINE HVOBIT (HVECAL,HVTPC,HVITC,HVLCAL)
C----------------------------------------------------------------------
CKEY EDIR HIGH VOLTAGE
C! Gives the detector's high voltage setup.
C-
C   Input  : None
C   Output : HVECAL is true if HV on Ecal is ON
C             HVTPC  "      "    "    TPC    "
C             HVITC  "      "    "    ITC    "
C             HVLCAL "      "    "    LCAL   "
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : LOLE,REVH
C-
C                           Authors: Ed.Blucher + M.Talby   19-Oct-1989
C-----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JLOLFB=1,JLOLSP=5,JLOLTO=6,JLOLMA=7,JLOLHV=11,JLOLER=12,
     +          LLOLEA=12)
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
C --
      LOGICAL HVECAL,HVTPC,HVITC,HVLCAL
      LOGICAL BTEST
C --
      HVLCAL=.FALSE.
      HVECAL=.FALSE.
      HVTPC=.FALSE.
      HVITC=.FALSE.
C --
C   Get Lcal high voltage setup
C --
      KLOLE = IW(NAMIND('LOLE'))
      IF(KLOLE.NE.0) THEN
        IF(IW(KLOLE+2+JLOLER).EQ.0) HVLCAL=.TRUE.
      ENDIF
C --
C   Get Ecal, TPC and ITC high voltage setup
C --
      JREVH = IW(NAMIND('REVH')) + LMHLEN
      IF(JREVH.EQ.LMHLEN) GOTO 999
C --
      KREVDS=IW(JREVH+JREVDS)
      HVECAL=BTEST(KREVDS,0).AND.BTEST(KREVDS,1).AND.BTEST(KREVDS,2)
      HVTPC=BTEST(KREVDS,4).OR.BTEST(KREVDS,15)
      HVITC=BTEST(KREVDS,5)
C --
  999 RETURN
      END
