      SUBROUTINE MINCRL
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill calorimeter relation bank DCRL for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PCRL bank
C     Output : DCRL bank
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
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
      PARAMETER(JDCRDE=1,JDCRDH=2,JDCRDT=3,LDCRLA=3)
C
      COMMON / DCRWRK / KPCRLL

      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
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
C++   Initialise work bank.
C
      IF(FIRST) THEN
         KPCRLL = 0
         FIRST = .FALSE.
      ENDIF
C
C++   Pick up PCRL bank.
C
      KPCRL = NLINK('PCRL',0)
      IF(KPCRL.GT.0) THEN
         NPCRL = LROWS(KPCRL)
      ELSE
         NPCRL = 0
      ENDIF
      IF(NPCRL.LE.0) RETURN
C
C++   Create DCRL bank.
C
      NDCRL = NPCRL
      LEN = LMHLEN + LDCRLA * NDCRL
      CALL AUBOS('DCRL',0,LEN, KDCRL,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINCRL: Cannot create DCRL bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPCRL = NLINK('PCRL',0)
      ENDIF
      IW(KDCRL+LMHCOL) = LDCRLA
      IW(KDCRL+LMHROW) = NDCRL
C
C++   In removing PCOB and PPOB from PCRL, there may be some repetition
C++   in PECO/PFRF/PHCO which should be removed.
C
C++   First step is to assign each PCRL an identifier containing
C++   PECO/PFRF/PHCO - this should be unique.
C++   In practice, use a decimal code, which may not be unique,
C++   but the likelihood of this must be neglible.
C++   The code is negated if it does not represent an interesting
C++   relation.
C
      CALL WBANK(IW,KPCRLL,2*NPCRL+1,*999)
C
      DO 100 I=1,NPCRL
         NREL = 0
         IPECO = ITABL(KPCRL,I,JPCRPE)
         IPFRF = ITABL(KPCRL,I,JPCRPF)
         IPHCO = ITABL(KPCRL,I,JPCRPH)
         IF(IPECO.GT.0) NREL = NREL + 1
         IF(IPFRF.GT.0) NREL = NREL + 1
         IF(IPHCO.GT.0) NREL = NREL + 1
         KODE = 10000*IPECO + 100*IPFRF + IPHCO
         IF(NREL.LE.1) KODE = -KODE
         IW(KPCRLL+I) = KODE
  100 CONTINUE
C
C++   Sort the PCRL banks according to the PECO/PFRF/PHCO code.
C
      CALL SORTZV(IW(KPCRLL+1),IW(KPCRLL+NPCRL+1),NPCRL,-1,0,0)
C
C++   Loop over sorted PCRL bank, according to index.
C++   Fill DCRL provided entry is different from previous one.
C++   PFRF, PECO and PHCO are parallel to DTRA, DECO and DHCO.
C
      LAST = -1
      IDCRL = 0
C
      DO 200 I=1,NPCRL
         INDX = IW(KPCRLL+NPCRL+I)
         KODE = IW(KPCRLL+INDX)
         IF(KODE.LE.   0) GOTO 200
         IF(KODE.EQ.LAST) GOTO 200
         LAST = KODE
         IDCRL = IDCRL + 1
         IW(KROW(KDCRL,IDCRL)+JDCRDE) = ITABL(KPCRL,INDX,JPCRPE)
         IW(KROW(KDCRL,IDCRL)+JDCRDT) = ITABL(KPCRL,INDX,JPCRPF)
         IW(KROW(KDCRL,IDCRL)+JDCRDH) = ITABL(KPCRL,INDX,JPCRPH)
  200 CONTINUE
C
C++   Compress bank to required size, and add it to the Mini list.
C
      NDCRL = IDCRL
      IF(NDCRL.GT.0) THEN
         LEN = LMHLEN + LDCRLA * NDCRL
         CALL AUBOS('DCRL',0,LEN, KDCRL,IGARB)
         IW(KDCRL+LMHROW) = NDCRL
         CALL MINLIS('DCRL')
      ELSE
         CALL BDROP(IW,'DCRL')
      ENDIF
C
C++   Drop work banks.
C
      CALL WDROP(IW,KPCRLL)
C
      RETURN
C
C++   Faillure.
C
  999 WRITE(IW(6),'('' MINCRL: Cannot create work bank for DCRL'')')
C
      RETURN
      END
