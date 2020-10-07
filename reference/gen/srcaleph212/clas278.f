      SUBROUTINE CLAS278 (KLASW)
C ---------------------------------------------------------------------
C! Set Lepton Tagging bits 27 and 28 in REVH class-word
C - M.N Minard   28-Nov-1994
CKEY EDIR
C - Output : KLASW / I = updated REVH class word
C     Bit 27 = Electron selection (Rt>-3, -3<Rl<3))
C     Bit 28 = Muon selection ( QMUIDO flag 13 or 14 )
C  Called from QFLEPT
C ---------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JREVDS=1,JREVFE=2,JREVNE=4,JREVSB=6,JREVTI=7,JREVRB=8,
     +          JREVEC=10,LREVHA=10)
      INTEGER JPDLPA,JPDLJT,JPDLPI,JPDLPE,JPDLVP,JPDLFR,LPDLTA
      PARAMETER(JPDLPA=1,JPDLJT=2,JPDLPI=3,JPDLPE=4,JPDLVP=5,JPDLFR=6,
     +          LPDLTA=6)
      INTEGER KLASW
      DATA NAREVH,NAPDLT /2*0/
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
C ---------------------------------------------------------------------
      IF (NAREVH.EQ.0) THEN
         NAREVH = NAMIND('REVH')
         NAPDLT = NAMIND('PDLT')
      ENDIF
C
      KREVH = IW(NAREVH)
      IF ( KREVH.NE.0) THEN
        IF (LCOLS(KREVH).GE.JREVEC) THEN
C
C - get existing class word
          KLASW = ITABL(KREVH,1,JREVEC)
C
C - Look to lepton bank content
          KPDLT = IW(NAPDLT)
          IF ( KPDLT.GT.0) THEN
            NLMU = 0
            NLEL = 0
            DO IK = 1,LROWS(KPDLT)
              ITYP = ITABL(KPDLT,IK,JPDLPA)
              IF (MOD(ITYP,10).EQ.2.OR.MOD(ITYP,10).EQ.3) NLEL = NLEL+1
              IF (MOD(ITYP,10).EQ.5.OR.MOD(ITYP,10).EQ.6) NLMU = NLMU+1
            ENDDO
            IF ( NLEL.GT.0) KLASW = IBSET (KLASW,27)
            IF ( NLMU.GT.0) KLASW = IBSET (KLASW,28)
          ENDIF
C
C - Update REVH Bank
          IW(KREVH+LMHLEN+JREVEC) = KLASW
        ENDIF
      ENDIF
      END
