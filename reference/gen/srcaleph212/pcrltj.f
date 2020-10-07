      SUBROUTINE PCRLTJ(IERR)
C ----------------------------------------------------------------------
C - J.F. Grivaz 15-03-1989
C! Split PCRL bank into PFER, PFHR, PHER, PHPR relation banks
C - Output
C -   IERR = 0 if successful unpacking
C          =-1 OK but garbage collection
C          = 1 PCRL bank missing or structure error
C          = 2 if unsuccessful ( No room to book banks )
C
      SAVE
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DATA IFRST/0/
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
C ----------------------------------------------------------------------
C
      IF(IFRST.LE.0) THEN
        CALL BKFMT('PFER','2I,(2I)')
        CALL BKFMT('PFHR','2I,(2I)')
        CALL BKFMT('PHER','2I,(2I)')
        CALL BKFMT('PHPR','2I,(2I)')
        NAPCRL=NAMIND('PCRL')
        IFRST = 1
      ENDIF
C
      CALL BDROP(IW,'PFERPFHRPHERPHPR')
      IERR = 1
      IF(IW(NAPCRL).LE.0) GO TO 100
      NUM = 0
      IER = 0
      IG = 0
C
      ICL1 = MIN0(JPCRPF,JPCRPE)
      ICL2 = MAX0(JPCRPF,JPCRPE)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PFER',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PFER')
C
      ICL1 = MIN0(JPCRPF,JPCRPH)
      ICL2 = MAX0(JPCRPF,JPCRPH)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PFHR',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PFHR')
C
      ICL1 = MIN0(JPCRPH,JPCRPE)
      ICL2 = MAX0(JPCRPH,JPCRPE)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PHER',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PHER')
C
      ICL1 = MIN0(JPCRPH,JPCRPP)
      ICL2 = MAX0(JPCRPH,JPCRPP)
      CALL UTWOCL('PCRL',NUM,ICL1,ICL2,'PHPR',IGARB,IER)
      IF(IER.NE.0) GO TO 100
      IF(IGARB.EQ.2) GO TO 999
      IF(IGARB.EQ.1) IG = IG + 1
      CALL BLIST(IW,'S+','PHPR')
C
      IERR = 0
      IF (IG.NE.0) IERR = -1
      GO TO 100
C
  999 CONTINUE
      IERR = 2
C
  100 CONTINUE
      RETURN
      END
