      SUBROUTINE MUCATR(ITRA,PR1OUT,PR2OUT,IFLAG)
C---------------------------------------------------------------
C! Probability for a track to be a muon from MUCALO
C  Author:    R.Tenchini     900128
CKEY MUCAL TRACK CALO / USER
C  Input :
C     ITRA = track index in PFRF
C  Output:
C     PR1OUT  = Probability (from 0 to 100%) to have muon in
C               the calobject linked to that track
C     PR2OUT  = Same meaning as PR1, but for prompt muons only
C               (i.e. exiting HCAL)
C     IFLAG = return flag.
C                          1 = OK
C                          0 = Calob without Digital Patterns
C                         -1 = Track not linked to calobs
C                         -2 = Maklis or Mucalo vectors underdimensioned
C
C - Input banks : PCRL
C
C--------------------------------------------------------------------
      SAVE
      PARAMETER (LENVEC=1000,LASPLN=23,MUFLAG=LASPLN+1)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
      DIMENSION IPLIS(LENVEC),ICLOS(LENVEC)
      PARAMETER(MAXL=100)
      DIMENSION NCAVEC(MAXL)
      DATA IFI /0/
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
C-----------------------------------------------------------------------
C
C - 1st entry : set name-indices
C
      IF (IFI .EQ. 0) THEN
         IFI = 1
         NAPCRL = NAMIND('PCRL')
      ENDIF
C
C - next entry
C
      IPCRL = IW(NAPCRL)
      IF(IPCRL.LE.0) GOTO 909
C
      IFLAG=-2
      CALL MAKLIS(IPCRL,JPCRPF,JPCRPC,ITRA,NCLOB,ICLOS,IER)
      IF(IER.NE.0) RETURN
      IFLAG=-1
      IF(NCLOB.EQ.0) RETURN
      PR1OUT=0.
      PR2OUT=0.
      IFLAG=0
      DO 100 J=1,NCLOB
         ICAL=ICLOS(J)
         CALL MAKLIS(IPCRL,JPCRPC,JPCRPP,ICAL,NPATT,IPLIS,IER)
         IF(IER.NE.0) THEN
            IFLAG=-2
            RETURN
         ENDIF
         IF(NPATT.EQ.0) GO TO 100
         CALL MUCALO(ICAL,PR1,ERPR1,PR2,ERPR2,NCAL,NCAVEC,MAXL,IER)
         IF(IER.NE.0) THEN
            IFLAG=-2
            RETURN
         ENDIF
         IFLAG=1
         IF(PR1OUT.LT.PR1) PR1OUT=PR1
         IF(PR2OUT.LT.PR2) PR2OUT=PR2
100   CONTINUE
909   RETURN
      END
