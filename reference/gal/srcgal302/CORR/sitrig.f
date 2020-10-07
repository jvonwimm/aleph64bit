      SUBROUTINE SITRIG
C.----------------------------------------------------------------
C  B.Bloch-Devaux  October 91 -January 93
C! SCAL : Form Trigger signals , apply zero suppression when creating
C!        SIFO analog sums
C     - Called by SIDIGI
C     - Calls     BDROP,BLIST                    from BOS lib
C     - Calls     ALBOS                          from ALEPHlib
C.----------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/SINAMC/NASIHT,NASIHI,NASIDI,NASIX2,NASIXA,NASIFO
      COMMON/SICONST/ISIFCT,ISIZCT,SIMPCT,SIPGEV
      PARAMETER(JSIFAD=1,JSIFA1=2,JSIFA2=3,JSIFA3=4,LSIFOA=4)
      PARAMETER(JSIHAD=1,JSIHE1=2,JSIHE2=3,JSIHE3=4,LSIHTA=4)
C   MASK for address without R bin
      PARAMETER ( MSKRSI = 32707)
C     PARAMETER ( MSKRSI = 'FC3'X)
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
C-----------------------------------------------------------
C - Drop existing banks
      CALL BDROP (IW,'SIFO')
C - Create new bank SIFO from SIHT info
      JSIHT = IW(NASIHT)
      IF ( JSIHT.LE.0) GO TO 999
      LSROW = LROWS(JSIHT)
      IF ( LSROW.LE.0) GO TO 999
      LEN = LSROW *LSIFOA+LMHLEN
      CALL ALBOS('SIFO',0,LEN,JSIFO,IGARB)
      IW(JSIFO+LMHCOL) = LSIFOA
      IW(JSIFO+LMHROW) = 0
      IF ( IGARB.GT.0) JSIHT = IW(NASIHT)
C - Add to the 'E' list
      CALL BLIST(IW,'E+','SIFO')
C Make summation over Amplex ( all R bins)
      DO 10 I = 1,LSROW
         IAD = IAND(ITABL(JSIHT,I,JSIHAD),MSKRSI)
         NSIFO = LROWS(JSIFO)
C No row filled yet , fill first address
         IF ( NSIFO.EQ.0 ) THEN
            IW(KNEXT(JSIFO)+JSIFAD) = IAD
            IW(JSIFO+LMHROW) = IW(JSIFO+LMHROW)+1
            NSIFO = LROWS(JSIFO)
         ENDIF
C  Look if this address already exists in SIFO
         IROW = 0
         DO 20 J= 1,NSIFO
            IF  (IAD.EQ.ITABL(JSIFO,J,JSIFAD)) IROW = J
 20      CONTINUE
C  This is a new row
         IF ( IROW.EQ.0) THEN
            IROW = LROWS(JSIFO)+1
            IW(KNEXT(JSIFO)+JSIFAD) = IAD
            IW(JSIFO+LMHROW) = IW(JSIFO+LMHROW)+1
         ENDIF
C  Update content
         DO 30 K = 1,3
            IW(KROW(JSIFO,IROW)+JSIFAD+K)=IW(KROW(JSIFO,IROW)+JSIFAD+K)
     $                      + IW(KROW(JSIHT,I)+JSIHAD+K)
 30      CONTINUE
 10   CONTINUE
      NSIFO = LROWS(JSIFO)
      KFIN = 0
      DO 50 I = 1,NSIFO
C Now transform into ADC counts ( SIMPCT Mev per ADC count ) and apply
C Zero suppression : at least one value in each triplet is above ISIFCT
         KSIF = KROW(JSIFO,I)
         DO 40 K = 1,3
            IW(KSIF+JSIFAD+K)=IW(KSIF+JSIFAD+K)/SIMPCT
 40      CONTINUE
         IF( (IW(KSIF+JSIFA1).GT.ISIFCT).OR.(IW(KSIF+JSIFA2).GT.ISIFCT)
     $   .OR.     (IW(KSIF+JSIFA3).GT.ISIFCT)) THEN
            KFIN = KFIN + 1
            IF ( KFIN.NE.I) THEN
               KSFO = KROW(JSIFO,KFIN)
               IW(KSFO+JSIFAD) = IW(KSIF+JSIFAD)
               IW(KSFO+JSIFA1) = IW(KSIF+JSIFA1)
               IW(KSFO+JSIFA2) = IW(KSIF+JSIFA2)
               IW(KSFO+JSIFA3) = IW(KSIF+JSIFA3)
            ENDIF
         ENDIF
  50  CONTINUE
      IW(JSIFO+LMHROW) = KFIN
C
 999  RETURN
      END
