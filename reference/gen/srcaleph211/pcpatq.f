      SUBROUTINE PCPATQ(IER)
C----------------------------------------------------------------------
CKEY PTOJ PCPA PCQA / USER
C----------------------------------------------------------------------
C!  - prepare PARLOC array ready to fill PCQA bank from PCPA bank.
C!
C!     IER =  0 OK
C!         = -1 banks missing
C!   Author   :-J. Carr    8 May 1991
C!    modified:-J. Carr    20 June 1991
C!
C!======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPCQNA=1,JPCQPX=2,JPCQPY=3,JPCQPZ=4,JPCQEN=5,LPCQAA=5)
      PARAMETER(JPCPNA=1,JPCPEN=2,JPCPTE=3,JPCPFI=4,JPCPR1=5,JPCPR2=6,
     +          JPCPPC=7,LPCPAA=7)
C
      PARAMETER (NMLINE=50)
      DIMENSION LINEPC(NMLINE)
      PARAMETER ( MXNEUT=300 )
      DIMENSION PARLOC(MXNEUT,5)
      DIMENSION IPEPC(MXNEUT),IPCOB(MXNEUT),ITYPE(MXNEUT)
      LOGICAL PARGON(MXNEUT)
C
      LOGICAL FIRST
      LOGICAL HADRO,GAMMA,GAMEX,GARBA,LCALO
      DATA FIRST/.TRUE./
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
      HADRO(IY) = IY.GE.13.AND.IY.LE.20
      GAMMA(IY) = IY.LT.13.AND.IY.NE.5
      GAMEX(IY) = IY.EQ.5
      GARBA(IY) = IY.EQ.21
      LCALO(IY) = IY.EQ.22
C ------------------------------------------------------------------
C
      IF(FIRST) THEN
        MPCPA = NAMIND ('PCPA')
        MPECO = NAMIND ('PECO')
        MPCOB = NAMIND ('PCOB')
        MNEMO = NAMIND ('NEMO')
        FIRST=.FALSE.
      ENDIF
C
      JPCPA = IW(MPCPA)
      IF(JPCPA.EQ.0)GOTO 999
      LPCPA = IW(JPCPA-1)
      NMAXP = MIN(IW(JPCPA+LMHLEN),MXNEUT)
C
      DO 30 I=1,NMAXP
        EN = RTABL(JPCPA,I,JPCPEN)
        TH = RTABL(JPCPA,I,JPCPTE)
        PH = RTABL(JPCPA,I,JPCPFI)
        IY = ITABL(JPCPA,I,JPCPNA)
        ITYPE(I)=IY
        IPEPC(I)=0
        IPCOB(I)=0
        IF(IY.EQ.19.OR.IY.EQ.20)THEN
           PSUM = RTABL(JPCPA,I,JPCPR2)
           PN = RTABL(JPCPA,I,JPCPR1)
        ELSE
           PSUM=0.
           PN = EN
        ENDIF
        CP = COS (PH)
        SP = SIN (PH)
        CT = COS (TH)
        ST = SIN (TH)
        PT = PN * ST
        PARLOC(I,1) = PT * CP
        PARLOC(I,2) = PT * SP
        PARLOC(I,3) = PN * CT
        PARLOC(I,4) = EN
        PARLOC(I,5) = PSUM
        PARGON(I)=.FALSE.
        IF(GARBA(IY)) PARGON(I)=.TRUE.
  30    CONTINUE
C
C   store PECO and PCOB if want to merge PCPA particles later in PCMECU
C       ( no merge if CARD NEMO included )
C
      IF(IW(MNEMO).NE.0) GO TO 42
C
C   Now get rid of some particles
C
      KPECO  = IW (MPECO)
      IF (KPECO .EQ. 0) goto 999
C
C Find which PCPA objects are from same PECO object
      LPECO = LROWS (KPECO)
      DO 40 IPECO=1,LPECO
C
         CALL NVPECO(IPECO, LINEPC, NLINE, NMLINE, IER)
         IF(IER.LT.0) GO TO 999
C
         DO 21 I=1,NLINE
           IF(LINEPC(I).LE.MXNEUT) IPEPC(LINEPC(I))=IPECO
  21     CONTINUE
  40  CONTINUE
  42  CONTINUE
C
C Find which PCPA objects are from same PCOB object
      KPCOB  = IW (MPCOB)
      IF (KPCOB .EQ. 0) goto 999
      LPCOB = LROWS (KPCOB)
      IMERGE=0
      DO 41 IPCB=1,LPCOB
C
         CALL NVPCOB(IPCB, LINEPC, NLINE, NMLINE, IER)
         IF(IER.LT.0) GO TO 999
C
         DO 23 I=1,NLINE
            IF(LINEPC(I).LE.MXNEUT) IPCOB(LINEPC(I))=IPCB
  23     CONTINUE
  41  CONTINUE
C
      CALL PCMECU(NMAXP,MXNEUT,PARLOC,IPEPC,IPCOB,ITYPE,PARGON)
C
      IER=0
      RETURN
  999 CONTINUE
      IER=-1
      RETURN
      END
