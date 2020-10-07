      SUBROUTINE MINGPC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill photon bank DGPC for Mini-DST.
C
C     Author: Stephen Haywood      02-Mar-93
C
C     Input  : PGPC bank
C     Output : DGPC bank
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
      PARAMETER(JPGPEC=1,JPGPTC=2,JPGPPC=3,JPGPR1=4,JPGPR2=5,JPGPF4=6,
     +          JPGPDM=7,JPGPST=8,JPGPQU=9,JPGPQ1=10,JPGPQ2=11,
     +          JPGPM1=12,JPGPM2=13,JPGPMA=14,JPGPER=15,JPGPTR=16,
     +          JPGPPR=17,JPGPPE=18,LPGPCA=18)
      PARAMETER(JDGPEC=1,JDGPTC=2,JDGPPC=3,JDGPR1=4,JDGPR2=5,JDGPF4=6,
     +          JDGPDM=7,JDGPST=8,JDGPQU=9,JDGPQ1=10,JDGPQ2=11,
     +          JDGPM1=12,JDGPM2=13,JDGPMA=14,JDGPER=15,JDGPTR=16,
     +          JDGPPR=17,JDGPPE=18,LDGPCA=18)
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
C++   Pick up PGPC bank.
C
      KPGPC = NLINK('PGPC',0)
      IF (KPGPC.LE.0) RETURN
      NPGPC = LROWS(KPGPC)
      IF (NPGPC.LE.0) RETURN
C
C++   Create the DGPC bank.
C
      NDGPC = NPGPC
      LEN = LMHLEN + LDGPCA * NDGPC
      CALL AUBOS('DGPC',0,LEN, KDGPC,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINGPC: Cannot create DGPC bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPGPC = NLINK('PGPC',0)
      ENDIF
      IW(KDGPC+LMHCOL) = LDGPCA
      IW(KDGPC+LMHROW) = NDGPC
C
C++   Loop over PGPC storing information in DGPC.
C
      DO 100 I=1,NPGPC
         IW(KROW(KDGPC,I)+JDGPEC) = NINT(EFACTM * RTABL(KPGPC,I,JPGPEC))
         IW(KROW(KDGPC,I)+JDGPTC) = NINT(AFACTM * RTABL(KPGPC,I,JPGPTC))
         IW(KROW(KDGPC,I)+JDGPPC) = NINT(AFACTM * RTABL(KPGPC,I,JPGPPC))
         IW(KROW(KDGPC,I)+JDGPR1) = NINT(1000. * RTABL(KPGPC,I,JPGPR1))
         IW(KROW(KDGPC,I)+JDGPR2) = NINT(1000. * RTABL(KPGPC,I,JPGPR2))
         IW(KROW(KDGPC,I)+JDGPF4) = NINT(1000. * RTABL(KPGPC,I,JPGPF4))
         DISTM = RTABL(KPGPC,I,JPGPDM)
         IF (DISTM.GT.1000.) DISTM = -0.1
         IW(KROW(KDGPC,I)+JDGPDM) = NINT(10. * DISTM)
         IW(KROW(KDGPC,I)+JDGPST) = NINT(RTABL(KPGPC,I,JPGPST))
         IW(KROW(KDGPC,I)+JDGPQU) = ITABL(KPGPC,I,JPGPQU)
         IW(KROW(KDGPC,I)+JDGPQ1) = NINT(100. * RTABL(KPGPC,I,JPGPQ1))
         IW(KROW(KDGPC,I)+JDGPQ2) = NINT(100. * RTABL(KPGPC,I,JPGPQ2))
         IW(KROW(KDGPC,I)+JDGPM1) = NINT(100. * RTABL(KPGPC,I,JPGPM1))
         IW(KROW(KDGPC,I)+JDGPM2) = NINT(100. * RTABL(KPGPC,I,JPGPM2))
         IW(KROW(KDGPC,I)+JDGPMA) = NINT(EFACTM * RTABL(KPGPC,I,JPGPMA))
         IW(KROW(KDGPC,I)+JDGPER) = NINT(EFACTM * RTABL(KPGPC,I,JPGPER))
         IW(KROW(KDGPC,I)+JDGPTR) = NINT(AFACTM * RTABL(KPGPC,I,JPGPTR))
         IW(KROW(KDGPC,I)+JDGPPR) = NINT(AFACTM * RTABL(KPGPC,I,JPGPPR))
         IW(KROW(KDGPC,I)+JDGPPE) = ITABL(KPGPC,I,JPGPPE)
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DGPC')
C
      RETURN
      END
