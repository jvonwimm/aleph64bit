      SUBROUTINE MINSIC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill Sical bank DSIC for Mini-DST.
C
C     Author: Stephen Haywood      24-May-93
C
C     Input  : SILU bank
C     Output : DSIC bank
C
C     Called by MINDST
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
      PARAMETER(JSILIS=1,JSILEC=2,JSILCR=17,JSILCP=19,JSILZC=22,
     +          JSILXI=27,JSILYI=28,JSILNP=31,JSILND=32,LSILUA=82)
      PARAMETER(JDSIIS=1,JDSIEC=2,JDSICR=3,JDSICP=4,JDSIZC=5,JDSIXI=6,
     +          JDSIYI=7,JDSINP=8,JDSIND=9,LDSICA=9)
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
C++   Pick up SILU bank.
C
      KSILU = NLINK('SILU',0)
      IF (KSILU.LE.0) RETURN
      NSILU = LROWS(KSILU)
      IF (NSILU.LE.0) RETURN
C
C++   Create the DSIC bank.
C
      NDSIC = NSILU
      LEN = LMHLEN + LDSICA * NDSIC
      CALL AUBOS('DSIC',0,LEN, KDSIC,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINSIC: Cannot create DSIC bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KSILU = NLINK('SILU',0)
      ENDIF
      IW(KDSIC+LMHCOL) = LDSICA
      IW(KDSIC+LMHROW) = NDSIC
C
C++   Loop over SILU storing information in DSIC.
C
      DO 100 I=1,NSILU
         IW(KROW(KDSIC,I)+JDSIIS) = ITABL(KSILU,I,JSILIS)
         IW(KROW(KDSIC,I)+JDSIEC) = NINT(EFACTM * RTABL(KSILU,I,JSILEC))
         IW(KROW(KDSIC,I)+JDSICR) = NINT(DFACTM * RTABL(KSILU,I,JSILCR))
         IW(KROW(KDSIC,I)+JDSICP) = NINT(AFACTM * RTABL(KSILU,I,JSILCP))
         IW(KROW(KDSIC,I)+JDSIZC) = NINT(DFACTM * RTABL(KSILU,I,JSILZC))
         IW(KROW(KDSIC,I)+JDSIXI) = NINT(DFACTM * RTABL(KSILU,I,JSILXI))
         IW(KROW(KDSIC,I)+JDSIYI) = NINT(DFACTM * RTABL(KSILU,I,JSILYI))
         IW(KROW(KDSIC,I)+JDSINP) = ITABL(KSILU,I,JSILNP)
         IW(KROW(KDSIC,I)+JDSIND) = ITABL(KSILU,I,JSILND)
  100 CONTINUE
C
C++   Add the bank to the Mini list. Also add SILH.
C
      CALL MINLIS('DSIC')
      CALL MINLIS('SILH')
C
      RETURN
      END
