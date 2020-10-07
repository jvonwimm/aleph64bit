      SUBROUTINE ESWEH (ETOT)
C----------------------------------------------------------------------
CKEY EDIR EVENT ENERGY
C! Calculate Energy of event.
C-
C   Input  : None
C   Output : ETOT  = Total Ecal energy of the event
C-
C   Called by   : SELCAL
C   Calls  : ECPHCO
C   Input banks : EVEH,PHCO
C-
C                          Authors: M.N. Minard + M. Pepe     15/09/89
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
      PARAMETER(JPHCER=1,JPHCTH=2,JPHCPH=3,JPHCEC=4,JPHCKD=5,JPHCCC=6,
     +          JPHCRB=7,JPHCNF=8,JPHCPC=9,LPHCOA=9)
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C --
      COMMON / EWIR / EWIRE ( 36 )
      DATA LRUN / 0 /
C --
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C --
C   Calculate Lep energy
C --
      ETOT = 0.
      KEVEH = IW(NAMIND('EVEH'))
      IF (KEVEH.LE.0) THEN
         GO TO 999
      ENDIF
      NRUN = IW(KEVEH+JEVERN)
      NEVT = IW(KEVEH+JEVEEV)
      IF (NRUN.NE.LRUN) THEN
          LRUN = NRUN
          ENLEP = ALELEP(NRUN)
          IF(ENLEP.LT.80..OR.ENLEP.GT.100.) ENLEP=91.1
      ENDIF
C --
C   Calculate E-From Ecal wires
C --
      EWECAL = 0
      DO 10 IECAL = 1, 36
      EWECAL = EWECAL + EWIRE (IECAL)
 10   CONTINUE
C --
C   Calculate Hcal Energy
C --
      EMATCH = 0
      NAPHCO = NAMIND('PHCO')
      KPHCO = IW (NAPHCO)
      NPHCO = 0
      IF (KPHCO.GT.0) NPHCO = LROWS(KPHCO)
      EPHCO = 0.
      DO 30 IPHCO = 1,NPHCO
C --
C   Add up Hcal tower energy which matches the digital information
C --
      CALL ECPHCO(IPHCO,IOK)
C --
C   fix up due to absence of digital readout in hcal endcaps
C --
      IF(IOK.GT.0) EMATCH = EMATCH + RTABL(KPHCO,IPHCO,JPHCEC)
      EPHCO = EPHCO + RTABL ( KPHCO,IPHCO ,JPHCER)
 30   CONTINUE
C --
C   Now calculate sum from differents contributions
C --
      ETOT =(EWECAL + EMATCH)/ENLEP
  999 RETURN
      END
