      SUBROUTINE MINPML
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PMLT bank from DMLT.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
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
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
      PARAMETER(JPMLFL=1,JPMLPO=2,JPMLCH=3,JPMLSP=4,JPMLLE=5,JPMLME=6,
     +          JPMLFR=7,JPMLKT=8,LPMLTA=8)
      PARAMETER(JDMLFL=1,JDMLPO=2,JDMLCH=3,JDMLSP=4,JDMLLE=5,JDMLME=6,
     +          JDMLFR=7,JDMLKT=8,LDMLTA=8)

C!    set of intrinsic functions to handle BOS banks
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
C
C++   Pick up DMLT bank.
C
      KDMLT = NLINK('DMLT',0)
      IF (KDMLT.LE.0) RETURN
      NDMLT = LROWS(KDMLT)
      IF (NDMLT.LE.0) RETURN
C
C++   Create PMLT bank.
C
      NPMLT = NDMLT
      LEN = LMHLEN + LPMLTA * NDMLT
      CALL AUBOS('PMLT',0,LEN, KPMLT,IGARB)
      CALL BLIST(IW,'S+','PMLT')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDMLT = NLINK('DMLT',0)
      ENDIF
      IW(KPMLT+LMHCOL) = LPMLTA
      IW(KPMLT+LMHROW) = NDMLT
C
C++   Fill PMLT bank.
C
      DO 100 I=1,NDMLT
         IW(KROW(KPMLT,I)+JPMLFL) = ITABL(KDMLT,I,JDMLFL)
         IW(KROW(KPMLT,I)+JPMLPO) = ITABL(KDMLT,I,JDMLPO)
         IW(KROW(KPMLT,I)+JPMLCH) = ITABL(KDMLT,I,JDMLCH)
         IW(KROW(KPMLT,I)+JPMLSP) = ITABL(KDMLT,I,JDMLSP)
         IW(KROW(KPMLT,I)+JPMLLE) = ITABL(KDMLT,I,JDMLLE)
         IW(KROW(KPMLT,I)+JPMLME) = ITABL(KDMLT,I,JDMLME)
         IW(KROW(KPMLT,I)+JPMLKT) = ITABL(KDMLT,I,JDMLKT)

         IW(KROW(KPMLT,I)+JPMLFR) = ITABL(KDMLT,I,JDMLFR)
  100 CONTINUE
C
      RETURN
      END
