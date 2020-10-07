      SUBROUTINE ECALWR(IMODUL,ECAPEA,ECALBE,ECAPEB)
C----------------------------------------------------------------------
CKEY EDIR ECAL WIRE ENERGY
C! ECAL wire energy.
C-
C   Input  : None
C   Output : IMODUL : Number of Ecal modules with energy > 2.5 GeV each
C            ECAPEA : Ecal endcap A wire energy
C            ECALBE : Ecal barrel wire energy
C            ECAPEB : Ecal endcap B wire energy
C-
C   Called by   : ECALSL
C   Calls  : None
C   Input banks : PEWI
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
      PARAMETER(JPEWMN=1,JPEWPD=2,JPEWSS=47,JPEWTI=55,LPEWIA=55)
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
      IMODUL = 0
      ECAPEA = 0.
      ECAPEB = 0.
      ECALBE = 0.
C --
      KPEWI=IW(NAMIND('PEWI'))
      IF(KPEWI.EQ.0) KPEWI =IW(NAMIND('PWEI'))
      IF(KPEWI.LE.0) GOTO 999
C --
      NMODUL = LROWS(KPEWI)
C --
C   Loop on Ecal modules above threshlod
C --
      DO 10 N = 1,NMODUL
        MN = ITABL(KPEWI,N,JPEWMN)
        IF(MN.LT.1 .OR. MN.GT.36) GOTO 10
C --
C   Add all planes energy per module
C   Planes with energy =< -100 MeV are not counted
C --
        EMODUL = 0.
        DO 20 NP = JPEWPD,JPEWSS-1
          EPLAN = FLOAT(ITABL(KPEWI,N,NP))*0.000001
          IF(EPLAN.LT.-0.1) GOTO 20
          EMODUL = EMODUL+EPLAN
   20   CONTINUE
        IF(MN.LE.12) ECAPEA = ECAPEA + EMODUL
        IF(MN.GT.12 .AND. MN.LE.24) ECALBE = ECALBE + EMODUL
        IF(MN.GT.24) ECAPEB = ECAPEB + EMODUL
        IF(EMODUL.LT.2.5) GOTO 10
        IMODUL = IMODUL + 1
   10 CONTINUE
C --
  999 RETURN
      END
