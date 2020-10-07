      SUBROUTINE MINPTH
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PTHR bank from DTHR.
C
C     Author: Agnieszka Jacholkowska      19-Sep-94
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
C
      PARAMETER(JDTHPR=1,JDTHPX=2,JDTHPY=3,JDTHPZ=4,JDTHE0=5,LDTHRA=5)
      PARAMETER(JPTHPR=1,JPTHPX=2,JPTHPY=3,JPTHPZ=4,JPTHPE=5,LPTHRA=5)
      PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
C!    set of intrinsic functions to handle BOS banks
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
C
      IBNK = 0
      KDTHR = NLINK('DTHR',IBNK)
      IF(KDTHR.LE.0) GOTO 1000
C
C++   Create PTHR bank.
C
c     NPTHR = LROWS(KDTHR)
      NPTHR = 1
      IF(NPTHR.LE.0) GOTO 1000
      LEN = LMHLEN + LPTHRA * NPTHR
      CALL AUBOS('PTHR',IBNK,LEN, KPTHR,IGARB)
      CALL BLIST(IW,'S+','PTHR')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTHR = NLINK('DTHR',IBNK)
      ENDIF
      IW(KPTHR+LMHCOL) = LPTHRA
      IW(KPTHR+LMHROW) = NPTHR
C
C++   Fill PTHR bank.
C
      RW(KROW(KPTHR,1)+JPTHPR) = FLOAT(ITABL(KDTHR,1,JDTHPR))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPX) = FLOAT(ITABL(KDTHR,1,JDTHPX))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPY) = FLOAT(ITABL(KDTHR,1,JDTHPY))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPZ) = FLOAT(ITABL(KDTHR,1,JDTHPZ))/AFACTM
      RW(KROW(KPTHR,1)+JPTHPE) = FLOAT(ITABL(KDTHR,1,JDTHE0))/AFACTM

c     WRITE(IW(6),'('' MINPTH: PTHR bank lifted'')')
 1000 CONTINUE
C
      RETURN
      END
