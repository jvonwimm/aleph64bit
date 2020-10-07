      SUBROUTINE HCALEN(EHCAL)
C----------------------------------------------------------------------
CKEY EDIR HCAL PADS ENERGY
C! HCAL total pads energy.
C-
C   Input  : None
C   Output : EHCAL : Hcal total pads energy
C-
C   Called by   : ECALSL
C   Calls  : None
C   Input banks : PHCO
C-
C                                        Author: M. Talby September 89
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
      EHCAL = 0.
C --
      KPHCO=IW(NAMIND('PHCO'))
      IF(KPHCO.LE.0) GOTO 999
C --
C   Total Hcal pads energy
C --
      NPHCO = LROWS(KPHCO)
      DO 30 NHC = 1,NPHCO
        EHCAL = EHCAL + RTABL(KPHCO,NHC,JPHCEC)
   30 CONTINUE
C --
  999 RETURN
      END
