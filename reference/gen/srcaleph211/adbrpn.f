      INTEGER FUNCTION ADBRPN ( DET,IROW,IPERIOD,IRUN,ISETUP)
C -----------------------------------------------------------
C - F.Ranjard - 901018
CKEY ALEF ADBR PERIOD
C! get elements of an ADBR period
C
C - Input  : DET      / A = CHARACTER*2 variable to define the det:
C                           'BP VD IT TP EC LC SA HC MU TR GE DB SI BE'
C            IPERIOD  / I = period #
C - Output : IROW     / I = ADBR row # valid for this period
C            IRUN     / I = 1st run of this period
C            ISETUP   / I = detector setup code
C                           = 0 if DET does not exist
C            ADBRPN   / I = valid period #
C                           = 0 if no bank or row # does not exist
C
C -------------------------------------------------------------
      CHARACTER*2 DET
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JADBPN=1,JADBFR=2,JADBBP=3,JADBVD=4,JADBIT=5,JADBTP=6,
     +          JADBEC=7,JADBLC=8,JADBSA=9,JADBHC=10,JADBMU=11,
     +          JADBTR=12,JADBGE=13,JADBDB=14,JADBSI=15,JADBBE=16,
     +          LADBRA=16)
      CHARACTER*28 DLIST
      DATA DLIST / 'BPVDITTPECLCPMHCMUTRGEDBSIBE'/
      DATA NADBR / 0/
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
C ------------------------------------------------------------
C - 1st entry : get ADBR bank from data base
C! get ADBR bank from data base
      IF (NADBR.EQ.0) THEN
         NADBR = NAMIND('ADBR')
         LDBAS = JUNIDB(0)
         JADBR = MDARD (IW,LDBAS,'ADBR',0)
         IF (JADBR.EQ.0) GOTO 998
      ENDIF
C
C - next entry
      JADBR = IW(NADBR)
      IF (JADBR .EQ. 0) GOTO 998
      LADBR = LROWS(JADBR)
      IDET = INDEX (DLIST,DET)
      IF (IDET.EQ.0) THEN
         JDETJJ = 0
      ELSE
         JDETJJ = JADBFR+IDET/2+1
      ENDIF
C
C
C - return row# valid for a given period and the 1st run
      IP1 = ITABL(JADBR,1,JADBPN)
      IF (IPERIOD.LT.IP1) GOTO 998
      IP2 = ITABL(JADBR,LADBR,JADBPN)
      IF (IPERIOD.EQ.IP1) THEN
         IROW = 1
      ELSEIF (IPERIOD.GE.IP2) THEN
         IROW = LADBR
      ELSE
         DO 10 I=2,LADBR
            IP2 = ITABL(JADBR,I,JADBPN)
            IF (IPERIOD.GE.IP1 .AND.IPERIOD.LT.IP2) THEN
               IROW = I-1
               GOTO 20
            ENDIF
            IP1 = IP2
 10      CONTINUE
         GOTO 998
      ENDIF
C
C - return content of the row
 20   CONTINUE
      IRUN = ITABL(JADBR,IROW,JADBFR)
      ISETUP = 0
      IF (JDETJJ.GT.0) ISETUP = ITABL(JADBR,IROW,JDETJJ)
      ADBRPN = ITABL(JADBR,IROW,JADBPN)
      RETURN
C
C - period not found
 998  ADBRPN = 0
      END
