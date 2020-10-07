      FUNCTION ECETDI (JT,IP)
C
C-    M.N Minard                   29-07-94
C!    For each storey give the correction factor
C!    from bank ECMC
C     JT = Tower number in theta unit
C     IP = Tower number in phi unit
C---------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JECMCO=1,JECMPP=2,JECMPR=3,JECMTE=4,LECMCA=4)
      DATA NAECMC / 0 /
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
      ECETDI = 1.
      IF (NAECMC.EQ.0) NAECMC = NAMIND('ECMC')
      IF ( IW(NAECMC).EQ.0) GO TO 999
      KECMC = IW(NAECMC)
C
C     From tower unit calculate module number
C
       CALL EMDTOW(JT,IP,ISCO,IMDO,IRG)
       IMODU = (ISCO-1)*12+IMDO
       ECETDI = RTABL(KECMC,IMODU,1)
 999   RETURN
       END
