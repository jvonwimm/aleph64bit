      SUBROUTINE MUELID(NMUON,NELEC)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS9->10
C! Muons and electrons identification.
C-
C   Input  : None
C   Output : NMUON  = Number of muons (HMAD) with energy >= 3 GeV
C            NELEC  = Number of electrons with energy >= 2 GeV
C-
C   Called by   : SELEVT
C   Calls  : None
C   Input banks : HMAD,EIDT,PEID
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
      PARAMETER(JHMANF=1,JHMANE=2,JHMANL=3,JHMAMH=4,JHMAIG=5,JHMAED=6,
     +          JHMACS=7,JHMAND=8,JHMAIE=9,JHMAIT=10,JHMAIF=11,
     +          JHMATN=12,LHMADA=12)
      PARAMETER(JEIDIF=1,JEIDR1=2,JEIDR2=3,JEIDR3=4,JEIDR4=5,JEIDR5=6,
     +          JEIDR6=7,JEIDR7=8,JEIDEC=9,JEIDIP=10,JEIDE1=11,
     +          JEIDE2=12,JEIDE3=13,JEIDFR=14,JEIDPE=15,LEIDTA=15)
      PARAMETER(JPEIIF=1,JPEIR1=2,JPEIR2=3,JPEIR3=4,JPEIR4=5,JPEIR6=6,
     +          JPEIR7=7,JPEIEC=8,JPEIET=9,JPEIP1=10,JPEIP2=11,
     +          JPEIPF=12,LPEIDA=12)
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
      NMUON = 0
      NELEC = 0
C --
      KHMAD=IW(NAMIND('HMAD'))
      IF(KHMAD.LE.0) GOTO 888
C --
      NMATRK = LROWS(KHMAD)
C --
C   Loop on charged tracks associated with Hcal
C   and count those with muon flag and energy deposit >= 3 GeV
C --
      DO 10 NMT = 1,NMATRK
        IFMU = ITABL(KHMAD,NMT,JHMAIF)
        IF(IFMU.NE.1) GOTO 10
        IF(RTABL(KHMAD,NMT,JHMAED).LT.3.) GOTO 10
        NMUON = NMUON+1
   10 CONTINUE
C --
  888 CONTINUE
C --
      KPEID=IW(NAMIND('PEID'))
      KEIDT=IW(NAMIND('EIDT'))
      IF(KPEID.GT.0) THEN
        NEATRK = LROWS(KPEID)
      ELSEIF(KEIDT.GT.0) THEN
        NEATRK = LROWS(KEIDT)
      ELSE
        GOTO 999
      ENDIF
C --
C   Loop on charged tracks associated with Ecal and count those
C   with P >= 2.GeV and satisfying the standard R2 and R3 cuts
C --
      DO 20 NET = 1,NEATRK
        R2 = -1000.
        R3 = -1000.
        IF(KPEID.GT.0) THEN
          IR2 = ITABL(KPEID,NET,JPEIR2)
          IR3 = ITABL(KPEID,NET,JPEIR3)
          IF(IR2.EQ.128) R2 = 1000.
          IF(ABS(IR2).EQ.127) R2 = 999.*SIGN(1.,FLOAT(IR2))
          IF(R2.EQ.-1000.) R2 = FLOAT(IR2)/10.
          IF(IR3.EQ.128) R3 = 1000.
          IF(ABS(IR3).EQ.127) R3 = 999.*SIGN(1.,FLOAT(IR3))
          IF(R3.EQ.-1000.) R3 = FLOAT(IR3)/10.
          NT = ITABL(KPEID,NET,JPEIPF)
        ELSE
          R2 = RTABL(KEIDT,NET,JEIDR2)
          R3 = RTABL(KEIDT,NET,JEIDR3)
          NT = ITABL(KEIDT,NET,JEIDFR)
        ENDIF
        IF(R2.LE.-3.5) GOTO 20
        IF(R3.LE.-3.5 .OR. R3.GE.4.) GOTO 20
        CALL QPTRCK(NT,PZ,PTOT)
        IF(PTOT.LT.2.) GOTO 20
        NELEC = NELEC+1
   20 CONTINUE
C --
  999 RETURN
      END
