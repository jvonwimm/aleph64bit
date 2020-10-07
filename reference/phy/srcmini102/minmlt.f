      SUBROUTINE MINMLT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill History of tagged lepton bank DMLT for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    24-Oct-94
C
C     Input  : PMLT bank
C     Output : DMLT bank
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
C++   Pick up PMLT bank.
C
      KPMLT = NLINK('PMLT',0)
      IF (KPMLT.LE.0) RETURN
      NPMLT = LROWS(KPMLT)
      IF (NPMLT.LE.0) RETURN
C
C++   Create the DMLT bank.
C
      NDMLT = NPMLT
      LEN = LMHLEN + LDMLTA * NDMLT
      CALL AUBOS('DMLT',0,LEN, KDMLT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINMLT: Cannot create DMLT bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPMLT = NLINK('PMLT',0)
      ENDIF
      IW(KDMLT+LMHCOL) = LDMLTA
      IW(KDMLT+LMHROW) = NDMLT
c     WRITE(IW(6),'('' MINMLT: DMLT bank lifted'')')
c     PRINT *, LDMLTA, NPMLT, NDMLT, LMHROW, IW(KDMLT+LMHROW)
C
C++   Loop over PMLT storing information in DMLT.
C
      DO 100 I=1,NDMLT
         IW(KROW(KDMLT,I)+JDMLFL) = ITABL(KPMLT,I,JPMLFL)
         IW(KROW(KDMLT,I)+JDMLPO) = ITABL(KPMLT,I,JPMLPO)
         IW(KROW(KDMLT,I)+JDMLCH) = ITABL(KPMLT,I,JPMLCH)
         IW(KROW(KDMLT,I)+JDMLSP) = ITABL(KPMLT,I,JPMLSP)
         IW(KROW(KDMLT,I)+JDMLLE) = ITABL(KPMLT,I,JPMLLE)
         IW(KROW(KDMLT,I)+JDMLME) = ITABL(KPMLT,I,JPMLME)
         IW(KROW(KDMLT,I)+JDMLKT) = ITABL(KPMLT,I,JPMLKT)

         IW(KROW(KDMLT,I)+JDMLFR) = ITABL(KPMLT,I,JPMLFR)
  100 CONTINUE
c     PRINT *, LDMLTA, NPMLT, NDMLT, LMHROW, IW(KDMLT+LMHROW)
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DMLT')
C
      RETURN
      END
