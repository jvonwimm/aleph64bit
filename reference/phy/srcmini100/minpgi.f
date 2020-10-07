      SUBROUTINE MINPGI
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PGID bank from DGID.
C
C     Author: Stephen Haywood      02-Mar-93
C-----------------------------------------------------------------------
C
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JPGIIF=1,JPGIDE=2,JPGICM=3,JPGIM1=4,JPGIM2=5,JPGIM3=6,
     +          JPGICE=7,JPGITH=8,JPGIPH=9,JPGIPE=10,LPGIDA=10)
      PARAMETER(JDGIIF=1,JDGIDE=2,JDGICM=3,JDGIM1=4,JDGIM2=5,JDGIM3=6,
     +          JDGICE=7,JDGITH=8,JDGIPH=9,JDGIPE=10,LDGIDA=10)
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
C++   Pick up DGID bank.
C
      KDGID = NLINK('DGID',0)
      IF (KDGID.LE.0) RETURN
      NDGID = LROWS(KDGID)
      IF (NDGID.LE.0) RETURN
C
C++   Create PGID bank.
C
      LEN = LMHLEN + LPGIDA * NDGID
      CALL AUBOS('PGID',0,LEN, KPGID,IGARB)
      CALL BLIST(IW,'S+','PGID')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDGID = NLINK('DGID',0)
      ENDIF
      IW(KPGID+LMHCOL) = LPGIDA
      IW(KPGID+LMHROW) = NDGID
C
C++   Fill PGID bank.
C
      DO 100 I=1,NDGID
         IW(KROW(KPGID,I)+JPGIIF) = ITABL(KDGID,I,JDGIIF)
         RW(KROW(KPGID,I)+JPGIDE) = FLOAT(ITABL(KDGID,I,JDGIDE))/100.
         RW(KROW(KPGID,I)+JPGICM) = FLOAT(ITABL(KDGID,I,JDGICM))/100.
         RW(KROW(KPGID,I)+JPGIM1) = FLOAT(ITABL(KDGID,I,JDGIM1))/100.
         RW(KROW(KPGID,I)+JPGIM2) = FLOAT(ITABL(KDGID,I,JDGIM2))/100.
         RW(KROW(KPGID,I)+JPGIM3) = FLOAT(ITABL(KDGID,I,JDGIM3))/EFACTM
         RW(KROW(KPGID,I)+JPGICE) = FLOAT(ITABL(KDGID,I,JDGICE))/EFACTM
         RW(KROW(KPGID,I)+JPGITH) = FLOAT(ITABL(KDGID,I,JDGITH))/AFACTM
         RW(KROW(KPGID,I)+JPGIPH) = FLOAT(ITABL(KDGID,I,JDGIPH))/AFACTM
         IW(KROW(KPGID,I)+JPGIPE) = ITABL(KDGID,I,JDGIPE)
  100 CONTINUE
C
      RETURN
      END
