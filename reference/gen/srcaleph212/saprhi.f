      SUBROUTINE SAPRHI
C ----------------------------------------------------------------
C! Print out SAHIt Bank in readable format
C                    H.Burkhardt    27/05/87
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
      DATA NASAH /0/
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
      IF (NASAH .EQ. 0) NASAH=NAMIND('SAHT')
      KSAHT=IW(NASAH)
      IF(KSAHT.EQ.0) THEN
        WRITE(LOUT,'(/1X,''+++SAPRHI no SAHI bank found'')')
        GOTO 900
      ENDIF
      NHIT=LROWS(KSAHT)
      KSA=KSAHT+LMHLEN
      WRITE(LOUT,'(/1X,''+++SAPRHI printout of SAHI bank,'',
     &   '' number of data words or hits  :'',I5,
     &   /''        raw  data        decoded in'',/'' #word'',
     &   ''     in HEX       ISIDe     ILAYer    ISECtor   IWIRe'',
     &   ''     DIST in cm'')') NHIT
      DO 10 I=1,NHIT
        IDAT=IW(KSA+I)
        ISID=IBITS(IDAT,28,3)
        ILAY=IBITS(IDAT,24,4)
        ISEC=IBITS(IDAT,20,4)
        IWIR=IBITS(IDAT,16,4)
        DIST=FLOAT(IBITS(IDAT, 0,16))/65536.
        WRITE(LOUT,'(I5,5X,Z8,4I10,3X,F7.4)') I,IDAT,ISID,ILAY,ISEC,
     &    IWIR,DIST
   10 CONTINUE
  900 CONTINUE
      END
