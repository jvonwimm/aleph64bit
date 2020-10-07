      SUBROUTINE MINFKI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill FKIN from DTMC.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : DTMC bank
C     Output : FKIN bank
C
C     Called by MINFIL
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
      PARAMETER(JFKIPX=1,JFKIPY=2,JFKIPZ=3,JFKIMA=4,JFKIPA=5,JFKIOV=6,
     +          JFKIEV=7,JFKIHC=8,LFKINA=8)
      PARAMETER(JDTMPX=1,JDTMPY=2,JDTMPZ=3,JDTMMA=4,JDTMPA=5,JDTMOV=6,
     +          JDTMEV=7,JDTMHC=8,LDTMCA=8)
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
C++   Pick up DTMC bank.
C
      KDTMC = NLINK('DTMC',0)
      IF(KDTMC.LE.0) RETURN
      NDTMC = LROWS(KDTMC)
      IF(NDTMC.LE.0) RETURN
C
C++   Create the FKIN bank.
C
      LEN = LMHLEN + LFKINA * NDTMC
      CALL AUBOS('FKIN',0,LEN, KFKIN,IGARB)
      CALL BLIST(IW,'S+','FKIN')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTMC = NLINK('DTMC',0)
      ENDIF
      IW(KFKIN+LMHCOL) = LFKINA
      IW(KFKIN+LMHROW) = NDTMC
C
C++   Fill FKIN bank.
C++   Note: there is a check to ensure that the pt is not 0;
C++   if it is, it is modified to protect later calculations.
C++   In reality, such tracks will never be used.
C
      I=0
      DO 10 KI=1,NDTMC
         IPX = ITABL(KDTMC,KI,JDTMPX)
         IPY = ITABL(KDTMC,KI,JDTMPY)
         IF(IPX.EQ.0 .AND. IPY.EQ.0) IPX = 1
         IPA = ITABL(KDTMC,KI,JDTMPA)
         IFV = ITABL(KDTMC,KI,JDTMOV)
         ILV = ITABL(KDTMC,KI,JDTMEV)
         IF (IPA.LE.0.OR.(IFV.EQ.0.AND.ILV.EQ.0)) GO TO 10
         I=I+1
         RW(KROW(KFKIN,I)+JFKIPX) = FLOAT(IPX)/EFACTM
         RW(KROW(KFKIN,I)+JFKIPY) = FLOAT(IPY)/EFACTM
         RW(KROW(KFKIN,I)+JFKIPZ) = FLOAT(ITABL(KDTMC,KI,JDTMPZ))/EFACTM
         RW(KROW(KFKIN,I)+JFKIMA) = FLOAT(ITABL(KDTMC,KI,JDTMMA))/EFACTM
         CALL UCOPY(IW(KROW(KDTMC,KI)+JDTMPA),IW(KROW(KFKIN,I)+
     +              JFKIPA),4)
 10   CONTINUE
      IW(KFKIN+LMHROW) = I
C
      RETURN
      END
