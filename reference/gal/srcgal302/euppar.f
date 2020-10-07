      SUBROUTINE EUPPAR
C----------------------------------------------
C-   M.N Minard              10.3.95
C    Update digitisation parameters for ECAL
C    when available
C    Called from ECIRUN
C----------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
       PARAMETER (JEDPN1=1,JEDPN2=2,JEDPN3=3,JEDPG1=4,JEDPG2=5,
     & JEDPG3=6,JEDPE1=7,JEDPE2=8,JEDPE3=9,JEDPO1=10,JEDPO2=11,
     & JEDPO3=12,LEDPAA=12)
      COMMON /EDPARA/ EDTMCO,EDTSCO,EDTNOI(3),EDWMCO,EDWSCO,EDWNOI,
     +                EDTGAI(3),EDTGER(3),KALEDT(3),EDWGAI,EDWGER,
     +                KALEDW,EDCUT1,EDCUT2,EDWCUT,ETTNOI(12,3),
     +                ETSTWE(3), EDZTHR(3,3)
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
      NAEDPA = NAMIND('EDPA')
      KEDPA = IW(NAEDPA)
      IF ( KEDPA.EQ.0) GO TO 900
C
C-   EDPA BANK exist update constant value
C
      EDTNOI(1) = RTABL(KEDPA,1,JEDPN1)
      EDTNOI(2) = RTABL(KEDPA,1,JEDPN2)
      EDTNOI(3) = RTABL(KEDPA,1,JEDPN3)
      KALEDT(1) = ITABL(KEDPA,1,JEDPG1)
      KALEDT(2) = ITABL(KEDPA,1,JEDPG2)
      KALEDT(3) = ITABL(KEDPA,1,JEDPG3)
      EDTGAI(1) = 1./FLOAT(KALEDT(1))
      EDTGAI(2) = 1./FLOAT(KALEDT(2))
      EDTGAI(3) = 1./FLOAT(KALEDT(3))
      EDTGER(1) = RTABL(KEDPA,1,JEDPE1)
      EDTGER(2) = RTABL(KEDPA,1,JEDPE2)
      EDTGER(3) = RTABL(KEDPA,1,JEDPE3)
      DO I=1,12
         ETTNOI(I,1) = RTABL(KEDPA,1,JEDPO1)
         ETTNOI(I,2) = RTABL(KEDPA,1,JEDPO2)
         ETTNOI(I,3) = RTABL(KEDPA,1,JEDPO3)
      ENDDO
 900  CONTINUE
      RETURN
      END
