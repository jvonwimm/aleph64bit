      SUBROUTINE MINEGI
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill EGID bank from DGAM.
C
C     Author: Stephen Haywood      17-Jan-91
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
      PARAMETER(JDGANA=1,JDGAE1=2,JDGAE2=3,JDGAE3=4,JDGAE5=5,JDGAE6=6,
     +          JDGAE8=7,JDGAE0=8,JDGATH=9,JDGAPH=10,JDGADE=11,
     +          LDGAMA=11)
      PARAMETER(JEGIIF=1,JEGIE1=2,JEGIE2=3,JEGIE3=4,JEGIE4=5,JEGIE5=6,
     +          JEGIE6=7,JEGIE7=8,JEGIE8=9,JEGINA=10,JEGICE=11,
     +          JEGITH=12,JEGIPH=13,JEGIPE=14,
     +          LEGIDA=14)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
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
      KDGAM = NLINK('DGAM',100)
      IF (KDGAM.LE.0) THEN
         CALL MINUPD('DGAM')
         KDGAM = NLINK('DGAM',100)
         IF (KDGAM.LE.0) RETURN
      ENDIF
C
C++   Create EGID bank.
C
      NEGID = LROWS(KDGAM)
      IF(NEGID.LE.0) RETURN
      LEN = LMHLEN + LEGIDA * NEGID
      CALL AUBOS('EGID',0,LEN, KEGID,IGARB)
      CALL BLIST(IW,'S+','EGID')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDGAM = NLINK('DGAM',100)
      ENDIF
      IW(KEGID+LMHCOL) = LEGIDA
      IW(KEGID+LMHROW) = NEGID
C
C++   Fill EGID bank.
C
      DO 100 I=1,NEGID
         IWORD = ITABL(KDGAM,I,JDGANA)
         IW(KROW(KEGID,I)+JEGIIF) = MOD(IWORD,16)
         IW(KROW(KEGID,I)+JEGINA) = IWORD / 16
         RW(KROW(KEGID,I)+JEGIE1) = FLOAT(ITABL(KDGAM,I,JDGAE1))/10.
         RW(KROW(KEGID,I)+JEGIE2) = FLOAT(ITABL(KDGAM,I,JDGAE2))/10.
         RW(KROW(KEGID,I)+JEGIE3) = FLOAT(ITABL(KDGAM,I,JDGAE3))/1000.
         RW(KROW(KEGID,I)+JEGIE5) = FLOAT(ITABL(KDGAM,I,JDGAE5))/10.
         RW(KROW(KEGID,I)+JEGIE6) = FLOAT(ITABL(KDGAM,I,JDGAE6))/10.
         RW(KROW(KEGID,I)+JEGIE8) = FLOAT(ITABL(KDGAM,I,JDGAE8))/EFACTM
         RW(KROW(KEGID,I)+JEGICE) = FLOAT(ITABL(KDGAM,I,JDGAE0))/EFACTM
         RW(KROW(KEGID,I)+JEGITH) = FLOAT(ITABL(KDGAM,I,JDGATH))/AFACTM
         RW(KROW(KEGID,I)+JEGIPH) = FLOAT(ITABL(KDGAM,I,JDGAPH))/AFACTM
         IW(KROW(KEGID,I)+JEGIPE) = ITABL(KDGAM,I,JDGADE)
  100 CONTINUE
C
      RETURN
      END
