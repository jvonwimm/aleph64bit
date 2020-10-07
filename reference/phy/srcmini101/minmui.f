      SUBROUTINE MINMUI
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Modify and save muon banks for Mini-DST.
C
C     Author: Stephen Haywood      28-Jul-92
C
C     Input  : HMAD, MCAD, MUID
C     Output : HMAD, MCAD, MUID
C
C     Called by MINDST.
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
      PARAMETER(JHMANF=1,JHMANE=2,JHMANL=3,JHMAMH=4,JHMAIG=5,JHMAED=6,
     +          JHMACS=7,JHMAND=8,JHMAIE=9,JHMAIT=10,JHMAIF=11,
     +          JHMATN=12,LHMADA=12)
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
C++   Only keep HMAD entries which have N10 > 0 or Muon Chamber hits.
C
      KHMAD = NLINK('HMAD',0)
      IF (KHMAD.GT.0) THEN
         NHMAD = LROWS(KHMAD)
      ELSE
         NHMAD = 0
      ENDIF
C
      KOUNT = 0
      DO 100 I=1,NHMAD
         ITRAK = ITABL(KHMAD,I,JHMATN)
         CALL AMUID(ITRAK, IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,XMULT,
     &     RAPP,ANG,ISHAD,SUDNT,IDF,IMCF,IER)
         IF (N10.LE.0 .AND. IM1.LE.0 .AND. IM2.LE.0) GOTO 100
         KOUNT = KOUNT + 1
         IF (I.GT.KOUNT) CALL UCOPY(IW(KROW(KHMAD,I)+1),
     &     IW(KROW(KHMAD,KOUNT)+1),LHMADA)
  100 CONTINUE
C
C++   Modify size of HMAD if necessary and add to Mini list.
C
      IF (KOUNT.GT.0) THEN
         IF (KOUNT.NE.NHMAD) THEN
            LEN = LMHLEN + LHMADA * KOUNT
            CALL AUBOS('HMAD',0,LEN, KHMAD,IGARB)
            IW(KHMAD+LMHROW) = KOUNT
         ENDIF
         CALL MINLIS('HMAD')
      ENDIF
C
C++   Add MCAD to the Mini list provided it is not empty.
C
      KMCAD = NLINK('MCAD',0)
      IF (KMCAD.GT.0) THEN
         NMCAD = LROWS(KMCAD)
      ELSE
         NMCAD = 0
      ENDIF
      IF (NMCAD.GT.0) CALL MINLIS('MCAD')
C
C++   Add MUID to the Mini list provided it is not empty.
C
      KMUID = NLINK('MUID',0)
      IF (KMUID.GT.0) THEN
         NMUID = LROWS(KMUID)
      ELSE
         NMUID = 0
      ENDIF
      IF (NMUID.GT.0) CALL MINLIS('MUID')
C++   Add D4CD to the MINI list
      CALL MINLIS('D4CD')
C
      RETURN
      END
