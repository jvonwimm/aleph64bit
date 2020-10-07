      SUBROUTINE PTEXSJ(LIST,IER)
C
C-----------------------------------------------------------------------
C! Transfer the PTEX bank to the TEXS bank
C!
C!    Author:   R. Johnson  16-06-88
C!    Modified: D. Cinabro  07-04-91 Also transfer PT2X to T2XS
C!    Modified: D. Casper   16-11-95 Also transfer PTPX to TPXS
C!
C!    Input :   LIST      /C    BOS event list
C!                              if LIST(2:2).eq.'-' POT bank is dropped
C!
C!    Output:   IER       /I    Error return=0 if operation successful
C                                            -1   OK but garbage coll.
C                                             2   not enough space
C                                            >2   TPC errors
C!
C!    Called by TPTOJ
C!
C-----------------------------------------------------------------------
      SAVE
      CHARACTER*(*) LIST, PLIST*8, JLIST*8
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPTESL=1,JPTETM=2,JPTEUL=3,JPTENS=4,JPTEAD=5,JPTETN=6,
     +          JPTESF=7,LPTEXA=7)
      PARAMETER(JTEXSI=1,JTEXTM=2,JTEXTL=3,JTEXNS=4,JTEXAD=5,JTEXTN=6,
     +          JTEXSF=7,LTEXSA=7)
      PARAMETER(JPT2SL=1,JPT2TM=2,JPT2UL=3,JPT2NS=4,JPT2AD=5,JPT2T1=6,
     +          JPT2T2=7,LPT2XA=7)
      PARAMETER(JT2XSI=1,JT2XTM=2,JT2XTL=3,JT2XNS=4,JT2XAD=5,JT2XT1=6,
     +          JT2XT2=7,LT2XSA=7)
      PARAMETER(JPTUID=1,JPTUVR=2,JPTURS=4,JPTUPS=5,JPTUZS=6,JPTUSR=7,
     +          JPTUSZ=8,JPTUPB=9,JPTUZB=10,JPTUTM=11,JPTUTL=12,
     +          JPTUAD=13,JPTURP=14,LPTUNA=14)
      INTEGER JPTPSL,JPTPTM,JPTPUL,JPTPNS,JPTPAD,JPTPTN,LPTPXA
      PARAMETER(JPTPSL=1,JPTPTM=2,JPTPUL=3,JPTPNS=4,JPTPAD=5,JPTPTN=6,
     +          LPTPXA=6)
      INTEGER JTPXSI,JTPXTM,JTPXTL,JTPXNS,JTPXAD,JTPXTN,JTPXSF,LTPXSA
      PARAMETER(JTPXSI=1,JTPXTM=2,JTPXTL=3,JTPXNS=4,JTPXAD=5,JTPXTN=6,
     +          JTPXSF=7,LTPXSA=7)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
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
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTEXS=NAMIND('TEXS')
        CALL BKFMT('TEXS','2I,(I,2F,I,F,2I)')
        NPTEX=NAMIND('PTEX')
        NT2XS=NAMIND('T2XS')
        CALL BKFMT('T2XS','2I,(I,2F,I,F,2I)')
        NPT2X=NAMIND('PT2X')
        NTPXS=NAMIND('TPXS')
        CALL BKFMT('TPXS','2I,(I,4F,I)')
        NPTPX=NAMIND('PTPX')
        NPTUN=NAMIND('PTUN')
      ENDIF
C
      IER = 0
      KPTUN=IW(NPTUN)
      IF (KPTUN.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
      JPTEX=IW(NPTEX)
      IF (JPTEX.EQ.0) GOTO 900
      IF (LROWS(JPTEX).EQ.0) GOTO 900
C
      LEN=LTEXSA*LROWS(JPTEX)+LMHLEN
      IW(1)=1
      CALL AUBOS('TEXS',0,LEN,JTEXS,IER)
      IF (IER.EQ.2) GOTO 999
      IF (IER.EQ.1) JPTEX = IW(NPTEX)
      JLIST = 'TEXS'
      IW(JTEXS+LMHCOL)=LTEXSA
      IW(JTEXS+LMHROW)=LROWS(JPTEX)
C
      KPTUN=IW(NPTUN)+LMHLEN
      DO 500 I=1,LROWS(JPTEX)
        IW(KROW(JTEXS,I)+JTEXSI)=ITABL(JPTEX,I,JPTESL)
        RW(KROW(JTEXS,I)+JTEXTM)=
     &             FLOAT(ITABL(JPTEX,I,JPTETM))*RW(KPTUN+JPTUTM)
        RW(KROW(JTEXS,I)+JTEXTL)=
     &             FLOAT(ITABL(JPTEX,I,JPTEUL))*RW(KPTUN+JPTUTL)
        IW(KROW(JTEXS,I)+JTEXNS)=ITABL(JPTEX,I,JPTENS)
        RW(KROW(JTEXS,I)+JTEXAD)=
     &             FLOAT(ITABL(JPTEX,I,JPTEAD))*RW(KPTUN+JPTUAD)
        IW(KROW(JTEXS,I)+JTEXTN)=ITABL(JPTEX,I,JPTETN)
C
C++     Old POT versions do not have the saturation flag stored
C
        IF (LCOLS(JPTEX).GE.JPTESF) THEN
          IW(KROW(JTEXS,I)+JTEXSF)=ITABL(JPTEX,I,JPTESF)
        ELSE
          IW(KROW(JTEXS,I)+JTEXSF)=0
        ENDIF
  500 CONTINUE
C
  900 CONTINUE
      JPT2X=IW(NPT2X)
      IF (JPT2X.EQ.0) GOTO 950
      IF (LROWS(JPT2X).EQ.0) GOTO 950
      LEN=LT2XSA*LROWS(JPT2X)+LMHLEN
      IW(1)=1
      CALL AUBOS('T2XS',0,LEN,JT2XS,IER)
      IF (IER.EQ.2) GOTO 998
      IF (IER.EQ.1) JPT2X = IW(NPT2X)
      JLIST = 'TEXST2XS'
      IW(JT2XS+LMHCOL)=LT2XSA
      IW(JT2XS+LMHROW)=LROWS(JPT2X)
C
      KPTUN=IW(NPTUN)+LMHLEN
      DO I=1,LROWS(JPT2X)
        IW(KROW(JT2XS,I)+JT2XSI)=ITABL(JPT2X,I,JPT2SL)
        RW(KROW(JT2XS,I)+JT2XTM)=
     &             FLOAT(ITABL(JPT2X,I,JPT2TM))*RW(KPTUN+JPTUTM)
        RW(KROW(JT2XS,I)+JT2XTL)=
     &             FLOAT(ITABL(JPT2X,I,JPT2UL))*RW(KPTUN+JPTUTL)
        IW(KROW(JT2XS,I)+JT2XNS)=ITABL(JPT2X,I,JPT2NS)
        RW(KROW(JT2XS,I)+JT2XAD)=
     &             FLOAT(ITABL(JPT2X,I,JPT2AD))*RW(KPTUN+JPTUAD)
        IW(KROW(JT2XS,I)+JT2XT1)=ITABL(JPT2X,I,JPT2T1)
        IW(KROW(JT2XS,I)+JT2XT2)=ITABL(JPT2X,I,JPT2T2)
      ENDDO
C
  950 CONTINUE
      JPTPX=IW(NPTPX)
      IF (JPTPX.EQ.0) GOTO 998
      IF (LROWS(JPTPX).EQ.0) GOTO 998
C
      LEN=LTPXSA*LROWS(JPTPX)+LMHLEN
      IW(1)=1
      CALL AUBOS('TPXS',0,LEN,JTPXS,IER)
      IF (IER.EQ.2) GOTO 999
      IF (IER.EQ.1) JPTPX = IW(NPTPX)
      JLIST = 'TEXST2XSTPXS'
      IW(JTPXS+LMHCOL)=LTPXSA
      IW(JTPXS+LMHROW)=LROWS(JPTPX)
C
      KPTUN=IW(NPTUN)+LMHLEN
      DO I=1,LROWS(JPTPX)
        IW(KROW(JTPXS,I)+JTPXSI)=ITABL(JPTPX,I,JPTPSL)
        RW(KROW(JTPXS,I)+JTPXTM)=
     &             FLOAT(ITABL(JPTPX,I,JPTPTM))*RW(KPTUN+JPTUTM)
        RW(KROW(JTPXS,I)+JTPXTL)=
     &             FLOAT(ITABL(JPTPX,I,JPTPUL))*RW(KPTUN+JPTUTL)
        RW(KROW(JTPXS,I)+JTPXNS)=FLOAT(ITABL(JPTPX,I,JPTPNS))/100.
        RW(KROW(JTPXS,I)+JTPXAD)=
     &             FLOAT(ITABL(JPTPX,I,JPTPAD))*RW(KPTUN+JPTUAD)
        IW(KROW(JTPXS,I)+JTPXTN)=ITABL(JPTPX,I,JPTPTN)
      ENDDO
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
        PLIST = 'PTEXPT2XPTPX'
C! add JLIST to S-list, drop PLIST if required
      IF (LNBLNK(LIST).EQ.2) THEN
         IF (LIST(2:2).EQ.'-' .AND. LNBLNK(PLIST).GE.4) THEN
            CALL BDROP (IW,PLIST)
            CALL BLIST (IW,LIST,PLIST(1:LNBLNK(PLIST)))
         ENDIF
      ENDIF
      CALL BLIST (IW,'S+',JLIST(1:LNBLNK(JLIST)))
C
C
      IF (IER .EQ.1) IER = -1
C
  999 CONTINUE
      RETURN
      END
