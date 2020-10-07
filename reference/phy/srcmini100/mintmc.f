      SUBROUTINE MINTMC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill track MC bank DTMC for Mini-DST.
C
C     Author: Stephen Haywood      21-Nov-90
C
C     Input  : FKIN bank
C     Output : DTMC bank
C
C     Called by MINDST
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
C++   Pick up FKIN bank.
C
      KFKIN = NLINK('FKIN',0)
      IF (KFKIN.LE.0) RETURN
      NFKIN = LROWS(KFKIN)
      IF(NFKIN.LE.0) RETURN
C
C++   Create the DTMC bank.
C
      LEN = LMHLEN + LDTMCA * NFKIN
      CALL AUBOS('DTMC',0,LEN, KDTMC,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINTMC: Cannot create DTMC bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KFKIN = NLINK('FKIN',0)
      ENDIF
      IW(KDTMC+LMHCOL) = LDTMCA
      IW(KDTMC+LMHROW) = NFKIN
C
C++   Copy FKIN to DTMC, integerising momenta and masses.
C
      DO I=1,NFKIN
         IW(KROW(KDTMC,I)+JDTMPX) = NINT(EFACTM * RTABL(KFKIN,I,JFKIPX))
         IW(KROW(KDTMC,I)+JDTMPY) = NINT(EFACTM * RTABL(KFKIN,I,JFKIPY))
         IW(KROW(KDTMC,I)+JDTMPZ) = NINT(EFACTM * RTABL(KFKIN,I,JFKIPZ))
         IW(KROW(KDTMC,I)+JDTMMA) = NINT(EFACTM * RTABL(KFKIN,I,JFKIMA))
         CALL UCOPY(IW(KROW(KFKIN,I)+JFKIPA),IW(KROW(KDTMC,I)+JDTMPA),4)
      ENDDO
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DTMC')
C
      RETURN
      END
