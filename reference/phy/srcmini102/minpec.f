      SUBROUTINE MINPEC
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PECO bank from DECO.
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
      PARAMETER(JDECE0=1,JDECTH=2,JDECPH=3,JDECEF=4,JDECCC=6,LDECOA=6)
      PARAMETER(JDCRDE=1,JDCRDH=2,JDCRDT=3,LDCRLA=3)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
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
C++   Determine the bank number (not from DECO/100).
C
      KDECO = IW(NAMIND('DECO'))
      NR = IW(KDECO-2)
C
C++   Pick up links.
C
      KDECO = NLINK('DECO',100)
      IF (KDECO.LE.0) THEN
         CALL MINUPD('DECO')
         KDECO = NLINK('DECO',100)
         IF (KDECO.LE.0) RETURN
      ENDIF
C
C++   Create PECO bank.
C
      NPECO = LROWS(KDECO)
      IF(NPECO.LE.0) RETURN
      LEN = LMHLEN + LPECOA * NPECO
      CALL AUBOS('PECO',NR,LEN, KPECO,IGARB)
      CALL BLIST(IW,'S+','PECO')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDECO = NLINK('DECO',100)
      ENDIF
      IW(KPECO+LMHCOL) = LPECOA
      IW(KPECO+LMHROW) = NPECO
C
C++   Fill PECO bank.
C
      DO 100 I=1,NPECO
         RW(KROW(KPECO,I)+JPECEC) = FLOAT(ITABL(KDECO,I,JDECE0))/EFACTM
         RW(KROW(KPECO,I)+JPECTH) = FLOAT(ITABL(KDECO,I,JDECTH))/AFACTM
         RW(KROW(KPECO,I)+JPECPH) = FLOAT(ITABL(KDECO,I,JDECPH))/AFACTM
         RW(KROW(KPECO,I)+JPECE1) = FLOAT(ITABL(KDECO,I,JDECEF+0))/1000.
         RW(KROW(KPECO,I)+JPECE2) = FLOAT(ITABL(KDECO,I,JDECEF+1))/1000.
         IW(KROW(KPECO,I)+JPECCC) = ITABL(KDECO,I,JDECCC)
  100 CONTINUE
C
C++   Now fill relational bits from DCRL.
C
      KDCRL = NLINK('DCRL',0)
      IF(KDCRL.LE.0) RETURN
      NDCRL = LROWS(KDCRL)
      IF(NDCRL.LE.0) RETURN
C
      DO 200 I=1,NDCRL
         IDECO = ITABL(KDCRL,I,JDCRDE)
         IF(IDECO.LE.0) GOTO 200
         IRBIT = ITABL(KPECO,IDECO,JPECRB)
         IF(ITABL(KDCRL,I,JDCRDT).GT.0) CALL SBIT1(IRBIT,1)
         IF(ITABL(KDCRL,I,JDCRDH).GT.0) CALL SBIT1(IRBIT,2)
         IW(KROW(KPECO,IDECO)+JPECRB) = IRBIT
  200 CONTINUE
C
      RETURN
      END
