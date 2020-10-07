      SUBROUTINE EBCOHD( PHOT , CORH )
C -----------------------------------------------------------------
C   AUTHOR   : J.Badier   09/10/89
C!  Hadronic energy.
CKEY PHOTONS HADRON CORRECTION / INTERNAL
C
C   The energy is corrected by the pion/electron ratio.
C
C   Input  :    PHOT    Number of storeys of the cluster ICLN.
C
C   Output :    CORH     Hadronic correction factor.
C
C   BANKS :
C     INPUT   : ECNS
C     OUTPUT  : NONE
C     CREATED : NONE
C
C ----------------------------------------------------
      PARAMETER ( PIE1 = 1.5 , PIE2 = 1.3 , PIE3 = 1. )
      DIMENSION PHOT(*)
      PARAMETER(JECNID=1,JECNVR=2,JECNEH=4,JECNEL=5,JECNCT=6,JECNSS=7,
     +          JECNSM=8,JECNPM=9,JECNEP=10,JECNER=13,JECNEI=16,
     +          JECNPR=17,JECNEM=18,JECNEB=19,JECNME=21,JECNR1=22,
     +          JECNR2=23,JECNR3=24,JECNRG=25,JECNCF=26,LECNSA=26)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      SAVE
      DATA KDEB / 0 /
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
      IF( KDEB .EQ. 0 ) THEN
        KECNS = IW(NAMIND('ECNS'))
        IF (KECNS .NE. 0) THEN
      HAD3 = RTABL(KECNS,1,JECNR3)
      HAD1 = RTABL(KECNS,1,JECNR1) - HAD3
      HAD2 = RTABL(KECNS,1,JECNR2) - HAD3
         ELSE
         HAD3 = PIE3
         HAD1 = PIE1 - PIE3
         HAD2 = PIE2 - PIE3
      ENDIF
      KDEB = 1
      ENDIF
      CORH = PHOT(2) * HAD1 + PHOT(3) * HAD2 + HAD3
      RETURN
      END
