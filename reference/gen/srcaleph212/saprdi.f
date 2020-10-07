      SUBROUTINE SAPRDI
C ----------------------------------------------------------------
C! Print out SADIgitization Bank in readable format
C                    H.Burkhardt    27/05/87
C  Modified          H. Meinhard    14/03/89 (crate numbering corr.)
C.
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      EXTERNAL NAMIND

C
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
C ----------------------------------------------------------
C - Get output unit #
C
      LOUT = IW(6)
      NASADI=NAMIND('SADI')
      KSADI=IW(NASADI)
      IF(KSADI.EQ.0) THEN
        WRITE(LOUT,'(/1X,''+++SAPRDI no SADI bank found'')')
        GOTO 900
      ENDIF
      NHIT=LROWS(KSADI)
      KSA=KSADI+LMHLEN
      WRITE(LOUT,'(/1X,''+++SAPRDI printout of SADI bank,'',
     &   '' number of data words or TDCs :'',I5,
     &   /''        raw  data        decoded in'',/'' #word'',
     &   ''     in HEX      JCRAT     JCARD     JTDCN     ITDCC'')')
     &   NHIT
      DO 10 I=1,NHIT
        IDAT=IW(KSA+I)
        JCARD=IBITS(IDAT,16,5)+1
        JCRAT=IBITS(IDAT,21,3)
        JTDCN=IBITS(IDAT,24,8)+1
        ITDCC=IBITS(IDAT,0,16)
        WRITE(LOUT,'(I5,5X,Z8,5I10)') I,IDAT,JCRAT,JCARD,JTDCN,ITDCC
   10 CONTINUE
  900 CONTINUE
      END
