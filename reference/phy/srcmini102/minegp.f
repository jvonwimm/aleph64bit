      SUBROUTINE MINEGP
C
CKEY MDST /USER
C---------------------------------------------------------------
C! Fill EGPC bank from DFOT.
C
C     Author: Stephen Haywood      04-Nov-91
C---------------------------------------------------------------
C
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JEGPPX=1,JEGPPY=2,JEGPPZ=3,JEGPR1=4,JEGPR2=5,JEGPF4=6,
     +          JEGPDM=7,JEGPST=8,JEGPQU=9,JEGPPE=10,LEGPCA=10)
      PARAMETER(JDFOPX=1,JDFOPY=2,JDFOPZ=3,JDFOQF=4,JDFODE=5,LDFOTA=5)
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
C++   Pick up DFOT bank.
C
      KDFOT = NLINK('DFOT',0)
      IF(KDFOT.LE.0) RETURN
      NEGPC = LROWS(KDFOT)
      IF(NEGPC.LE.0) RETURN
C
C++   Create EGPC bank.
C
      LEN = LMHLEN + LEGPCA * NEGPC
      CALL AUBOS('EGPC',0,LEN, KEGPC,IGARB)
      CALL BLIST(IW,'S+','EGPC')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDFOT = NLINK('DFOT',0)
      ENDIF
      IW(KEGPC+LMHCOL) = LEGPCA
      IW(KEGPC+LMHROW) = NEGPC
C
C++   Fill EGPC bank.
C
      DO 100 I=1,NEGPC
         RW(KROW(KEGPC,I)+JEGPPX) = FLOAT(ITABL(KDFOT,I,JDFOPX))/EFACTM
         RW(KROW(KEGPC,I)+JEGPPY) = FLOAT(ITABL(KDFOT,I,JDFOPY))/EFACTM
         RW(KROW(KEGPC,I)+JEGPPZ) = FLOAT(ITABL(KDFOT,I,JDFOPZ))/EFACTM
         IQUAL = 0
         IFLAG = ITABL(KDFOT,I,JDFOQF)
         IF(MOD(IFLAG,2).EQ.1) IQUAL = 1
         IF(IFLAG.GT.1) IQUAL = IQUAL + 1110
         IW(KROW(KEGPC,I)+JEGPQU) = IQUAL
         IW(KROW(KEGPC,I)+JEGPPE) = ITABL(KDFOT,I,JDFODE)
  100 CONTINUE
C
      RETURN
      END
