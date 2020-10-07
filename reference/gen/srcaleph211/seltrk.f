        SUBROUTINE SELTRK(TRKHOK)
C----------------------------------------------------------------------
CKEY EDIR GOOD TRACKS
C! Select good events according to QQbar group selection.
C! Good event = number of good tracks >=5 + E(track)/ELEP > 10.
C-
C   Input  : none
C   Output : TRKHOK is true if good event for QQbar selection
C-
C   Called by   : SELEVT
C   Calls  : TRKHAD
C   Input banks : EVEH
C-
C                                      Author: M. Talby    19-Oct-1989
C----------------------------------------------------------------------
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
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C --
      LOGICAL TRKHOK
      DATA LRUN/0/
C --
      TRKHOK = .FALSE.
C --
      KEVEH = IW(NAMIND('EVEH'))
      IF(KEVEH.LE.0) GOTO 999
C --
C   Get LEP energy for run number NRUN
C --
      NRUN = IW(KEVEH+JEVERN)
      NEVT = IW(KEVEH+JEVEEV)
      IF(NRUN.NE.LRUN) THEN
        LRUN = NRUN
        ENLEP = ALELEP(NRUN)
        IF(ENLEP.LT.80. .OR. ENLEP.GT.100.) ENLEP = 91.1
      ENDIF
C --
C   Get the number of good tracks NGDTR and the sum of their energy
C   ECHRG
C --
      CALL TRKHAD(NGDTR,ECHRG)
      FECHRG = ECHRG/ENLEP
      IF(NGDTR.GE.5 .AND. FECHRG.GT.0.1) TRKHOK = .TRUE.
C --
999   RETURN
      END
