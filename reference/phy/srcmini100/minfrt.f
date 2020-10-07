      SUBROUTINE MINFRT
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill FRTL bank from DTRA.
C
C     Author: Stephen Haywood      03-Apr-90
C
C     The number of hits is available in the bit pattern stored in the
C     DTRA bank. This word (or parts of it) is used as the index to a
C     look up table, K, which contains the bit sums.
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
      PARAMETER(JDTRCH=1,JDTRP0=2,JDTRTH=3,JDTRPH=4,JDTRD0=5,JDTRZ0=6,
     +          JDTRER=7,JDTRTF=12,JDTRHO=13,JDTRHM=14,JDTRVB=15,
     +          JDTRQF=16,JDTREA=17,JDTRVI=27,LDTRAA=27)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
C
      COMMON / MINLOC / K(0:255)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      SAVE FIRST
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
C++   Initialisation of K array.
C
      IF(FIRST) THEN
        DO 10 I=0,255
   10   K(I) = JBIT(I,1) + JBIT(I,2) + JBIT(I,3) + JBIT(I,4) +
     &         JBIT(I,5) + JBIT(I,6) + JBIT(I,7) + JBIT(I,8)
        FIRST = .FALSE.
      ENDIF
C
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
C
C++   Create FRTL bank.
C
      NFRTL = LROWS(KDTRA)
      IF(NFRTL.LE.0) RETURN
      LEN = LMHLEN + LFRTLA * NFRTL
      CALL AUBOS('FRTL',0,LEN, KFRTL,IGARB)
      CALL BLIST(IW,'S+','FRTL')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KFRTL+LMHCOL) = LFRTLA
      IW(KFRTL+LMHROW) = NFRTL
C
C++   Fill FRTL bank from bit pattern in DTRA.
C
      DO 100 I=1,NFRTL
         IH = ITABL(KDTRA,I,JDTRHO)
         IW(KROW(KFRTL,I)+JFRTNV) = K(JBYT(IH, 1,2))
         IW(KROW(KFRTL,I)+JFRTNI) = K(JBYT(IH, 3,8))
         IW(KROW(KFRTL,I)+JFRTNT) = K(JBYT(IH,11,8)) + K(JBYT(IH,19,8))
     &                            + K(JBYT(IH,27,5))
  100 CONTINUE
C
      RETURN
      END
