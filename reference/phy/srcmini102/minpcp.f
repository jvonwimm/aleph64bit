      SUBROUTINE MINPCP
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PCPA bank from DNEU and DRES.
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
      PARAMETER(JDNENA=1,JDNEE0=2,JDNETH=3,JDNEPH=4,JDNER1=5,JDNER2=6,
     +          JDNEPC=7,JDNEDE=8,LDNEUA=8)
      PARAMETER(JDRENA=1,JDREE0=2,JDRETH=3,JDREPH=4,JDREP0=5,JDREPS=6,
     +          JDREPC=7,LDRESA=7)
      PARAMETER(JPCPNA=1,JPCPEN=2,JPCPTE=3,JPCPFI=4,JPCPR1=5,JPCPR2=6,
     +          JPCPPC=7,LPCPAA=7)
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
      KDNEU = NLINK('DNEU',0)
      IF(KDNEU.GT.0) THEN
         NDNEU = LROWS(KDNEU)
      ELSE
         NDNEU = 0
      ENDIF
C
      KDRES = NLINK('DRES',0)
      IF(KDRES.GT.0) THEN
         NDRES = LROWS(KDRES)
      ELSE
         NDRES = 0
      ENDIF
C
C++   Create PCPA bank.
C
      NPCPA = NDNEU + NDRES
      IF(NPCPA.LE.0) RETURN
      LEN = LMHLEN + LPCPAA * NPCPA
      CALL AUBOS('PCPA',0,LEN, KPCPA,IGARB)
      CALL BLIST(IW,'S+','PCPA')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDNEU = NLINK('DNEU',0)
         KDRES = NLINK('DRES',0)
      ENDIF
      IW(KPCPA+LMHCOL) = LPCPAA
      IW(KPCPA+LMHROW) = NPCPA
C
C++   Fill PCPA bank from DNEU.
C
      J = 0
      DO 100 I=1,NDNEU
         J = J + 1
         IW(KROW(KPCPA,J)+JPCPNA) = ITABL(KDNEU,I,JDNENA)
         RW(KROW(KPCPA,J)+JPCPEN) = FLOAT(ITABL(KDNEU,I,JDNEE0))/EFACTM
         RW(KROW(KPCPA,J)+JPCPTE) = FLOAT(ITABL(KDNEU,I,JDNETH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPFI) = FLOAT(ITABL(KDNEU,I,JDNEPH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPR1) = FLOAT(ITABL(KDNEU,I,JDNER1))/10.
         RW(KROW(KPCPA,J)+JPCPR2) = FLOAT(ITABL(KDNEU,I,JDNER2))/10.
  100 CONTINUE
C
C++   Fill PCPA bank from DRES.
C
      DO 200 I=1,NDRES
         J = J + 1
         IW(KROW(KPCPA,J)+JPCPNA) = ITABL(KDRES,I,JDRENA)
         RW(KROW(KPCPA,J)+JPCPEN) = FLOAT(ITABL(KDRES,I,JDREE0))/EFACTM
         RW(KROW(KPCPA,J)+JPCPTE) = FLOAT(ITABL(KDRES,I,JDRETH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPFI) = FLOAT(ITABL(KDRES,I,JDREPH))/AFACTM
         RW(KROW(KPCPA,J)+JPCPR1) = FLOAT(ITABL(KDRES,I,JDREP0))/EFACTM
         RW(KROW(KPCPA,J)+JPCPR2) = FLOAT(ITABL(KDRES,I,JDREPS))/EFACTM
  200 CONTINUE
C
      RETURN
      END
