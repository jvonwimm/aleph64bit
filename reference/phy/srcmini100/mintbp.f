      SUBROUTINE MINTBP
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex bank DTBP for Mini-DST.
C
C     Author: Stephen Haywood      04-Apr-90
C
C     Input  : XTRB(or XTEB) bank
C     Output : DTBP bank
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
      PARAMETER(JXTRRP=1,JXTRRN=4,LXTRBA=4)
      PARAMETER(JXTET1=1,JXTET2=2,JXTEL2=3,JXTEHT=4,JXTEHW=16,JXTELW=28,
     +          JXTEEW=40,JXTELT=52,JXTETE=56,JXTEIT=58,JXTETP=62,
     +          LXTEBA=65)
      PARAMETER(JDTBT1=1,JDTBT2=2,JDTBL2=3,LDTBPA=3)
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
C++   Pick up trigger banks.
C
      KXTRB = NLINK('XTRB',0)
      KXTEB = NLINK('XTEB',0)
      IF(KXTRB.LE.0 .AND. KXTEB.LE.0) RETURN
C
C++   Create the DTBP bank.
C
      LEN = LMHLEN + LDTBPA
      CALL AUBOS('DTBP',0,LEN, KDTBP,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINTBP: Cannot create DTBP bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KXTRB = NLINK('XTRB',0)
         KXTEB = NLINK('XTEB',0)
      ENDIF
      IW(KDTBP+LMHCOL) = LDTBPA
      IW(KDTBP+LMHROW) = 1
C
C++   Copy words from trigger banks.
C
      IF(KXTRB.GT.0) THEN
         ITRG1 = ITABL(KXTRB,1,JXTRRP+0)
         ITRG2 = ITABL(KXTRB,1,JXTRRP+1)
         ITRG3 = ITABL(KXTRB,1,JXTRRP+2)
      ELSE
         ITRG1 = ITABL(KXTEB,1,JXTET1)
         ITRG2 = ITABL(KXTEB,1,JXTET2)
         ITRG3 = ITABL(KXTEB,1,JXTEL2)
      ENDIF
C
      IW(KROW(KDTBP,1)+JDTBT1) = ITRG1
      IW(KROW(KDTBP,1)+JDTBT2) = ITRG2
      IW(KROW(KDTBP,1)+JDTBL2) = ITRG3
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DTBP')
C
      RETURN
      END
