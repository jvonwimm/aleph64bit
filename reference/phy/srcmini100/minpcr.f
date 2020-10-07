      SUBROUTINE MINPCR
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PCRL bank from DCRL.
C
C     Author: Stephen Haywood      03-Apr-90
C
C     It appears that the PCRL bank is essential for the operation of
C     ALPHA.
C     If no DCRL bank exists, a dummy PCRL with one empty row is made.
C     This is necessary since, if there are simple isolated calobjects,
C     they will not appear in the DCRL bank, and if all objects are of
C     this form, then the DCRL bank will be empty.
C     It would be better to create rows in the PCRL bank with the
C     trivial relations in (ie. one object, and the other entries zero).
C     However, this requires looking at PECO/PHCO banks and reduces
C     simplicity.
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
      PARAMETER(JDCRDE=1,JDCRDH=2,JDCRDT=3,LDCRLA=3)
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
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
      KDCRL = NLINK('DCRL',0)
      IF(KDCRL.GT.0) THEN
         NDCRL = LROWS(KDCRL)
      ELSE
         NDCRL = 0
      ENDIF
      IF(NDCRL.GT.0) THEN
         NPCRL = NDCRL
      ELSE
         NPCRL = 1
      ENDIF
C
C++   Create PCRL bank.
C
      LEN = LMHLEN + LPCRLA * NPCRL
      CALL AUBOS('PCRL',0,LEN, KPCRL,IGARB)
      CALL BLIST(IW,'S+','PCRL')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDCRL = NLINK('DCRL',0)
      ENDIF
      IW(KPCRL+LMHCOL) = LPCRLA
      IW(KPCRL+LMHROW) = NPCRL
C
C++   Fill PCRL bank.
C
      DO 100 I=1,NDCRL
         IW(KROW(KPCRL,I)+JPCRPE) = ITABL(KDCRL,I,JDCRDE)
         IW(KROW(KPCRL,I)+JPCRPF) = ITABL(KDCRL,I,JDCRDT)
         IW(KROW(KPCRL,I)+JPCRPH) = ITABL(KDCRL,I,JDCRDH)
  100 CONTINUE
C
      RETURN
      END
