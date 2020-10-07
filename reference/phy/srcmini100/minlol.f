      SUBROUTINE MINLOL
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill LOLE bank from DEVT or DHEA.
C
C     Author: Stephen Haywood      03-Apr-90
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JDEVLE=1,JDEVWT=2,JDEVNX=3,JDEVNV=4,JDEVWR=5,JDEVWA=6,
     +          JDEVWB=7,JDEVPR=8,JDEVPA=9,JDEVPB=10,JDEVLA=11,
     +          JDEVLB=12,JDEVHR=13,JDEVHA=14,JDEVHB=15,LDEVTA=15)
      PARAMETER(JDHEFP=1,JDHENX=2,JDHENP=3,JDHENM=4,JDHENV=5,JDHENJ=6,
     +          JDHEEC=7,JDHEEL=8,JDHEPF=9,JDHETH=10,JDHEPH=11,
     +          JDHEEF=12,JDHEET=13,JDHET1=14,JDHEP1=15,JDHET2=16,
     +          JDHEP2=17,JDHEE1=18,JDHEE2=19,JDHEE3=20,JDHERS=21,
     +          JDHEWT=22,LDHEAA=22)
      PARAMETER(JLOLFB=1,JLOLSP=5,JLOLTO=6,JLOLMA=7,JLOLHV=11,JLOLER=12,
     +          LLOLEA=12)
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
C
      KDEVT = NLINK('DEVT',0)
      KDHEA = NLINK('DHEA',0)
      IF (KDEVT.LE.0 .AND. KDHEA.LE.0) RETURN
C
C++   Create LOLE bank.
C
      LEN = LMHLEN + LLOLEA
      CALL AUBOS('LOLE',0,LEN, KLOLE,IGARB)
      CALL BLIST(IW,'S+','LOLE')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDEVT = NLINK('DEVT',0)
         KDHEA = NLINK('DHEA',0)
      ENDIF
      IW(KLOLE+LMHCOL) = LLOLEA
      IW(KLOLE+LMHROW) = 1
C
      IF (KDEVT.GT.0) THEN
         IER = ITABL(KDEVT,1,JDEVLE)
      ELSE
         IER = ITABL(KDHEA,1,JDHERS)
      ENDIF
      IW(KROW(KLOLE,1)+JLOLER) = IER
C
      RETURN
      END
