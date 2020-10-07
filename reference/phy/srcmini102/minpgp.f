      SUBROUTINE MINPGP
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill PGPC bank from DGPC.
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
C++   Pick up DGPC bank.
C
      KDGPC = NLINK('DGPC',0)
      IF (KDGPC.LE.0) RETURN
      NDGPC = LROWS(KDGPC)
      IF (NDGPC.LE.0) RETURN
C
C++   Create PGPC bank.
C
      LEN = LMHLEN + LPGPCA * NDGPC
      CALL AUBOS('PGPC',0,LEN, KPGPC,IGARB)
      CALL BLIST(IW,'S+','PGPC')
      IF (IGARB.GE.2) THEN
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KDGPC = NLINK('DGPC',0)
      ENDIF
      IW(KPGPC+LMHCOL) = LPGPCA
      IW(KPGPC+LMHROW) = NDGPC
C
C++   Fill PGPC bank.
C
      DO 100 I=1,NDGPC
         RW(KROW(KPGPC,I)+JPGPEC) = FLOAT(ITABL(KDGPC,I,JDGPEC))/EFACTM
         RW(KROW(KPGPC,I)+JPGPTC) = FLOAT(ITABL(KDGPC,I,JDGPTC))/AFACTM
         RW(KROW(KPGPC,I)+JPGPPC) = FLOAT(ITABL(KDGPC,I,JDGPPC))/AFACTM
         RW(KROW(KPGPC,I)+JPGPR1) = FLOAT(ITABL(KDGPC,I,JDGPR1))/1000.
         RW(KROW(KPGPC,I)+JPGPR2) = FLOAT(ITABL(KDGPC,I,JDGPR2))/1000.
         RW(KROW(KPGPC,I)+JPGPF4) = FLOAT(ITABL(KDGPC,I,JDGPF4))/1000.
         MDIST = ITABL(KDGPC,I,JDGPDM)
         IF (MDIST.EQ.-1) MDIST = +99990
         RW(KROW(KPGPC,I)+JPGPDM) = FLOAT(MDIST)/10.
         RW(KROW(KPGPC,I)+JPGPST) = FLOAT(ITABL(KDGPC,I,JDGPST))
         IW(KROW(KPGPC,I)+JPGPQU) = ITABL(KDGPC,I,JDGPQU)
         RW(KROW(KPGPC,I)+JPGPQ1) = FLOAT(ITABL(KDGPC,I,JDGPQ1))/100.
         RW(KROW(KPGPC,I)+JPGPQ2) = FLOAT(ITABL(KDGPC,I,JDGPQ2))/100.
         RW(KROW(KPGPC,I)+JPGPM1) = FLOAT(ITABL(KDGPC,I,JDGPM1))/100.
         RW(KROW(KPGPC,I)+JPGPM2) = FLOAT(ITABL(KDGPC,I,JDGPM2))/100.
         RW(KROW(KPGPC,I)+JPGPMA) = FLOAT(ITABL(KDGPC,I,JDGPMA))/EFACTM
         RW(KROW(KPGPC,I)+JPGPER) = FLOAT(ITABL(KDGPC,I,JDGPER))/EFACTM
         RW(KROW(KPGPC,I)+JPGPTR) = FLOAT(ITABL(KDGPC,I,JDGPTR))/AFACTM
         RW(KROW(KPGPC,I)+JPGPPR) = FLOAT(ITABL(KDGPC,I,JDGPPR))/AFACTM
         IW(KROW(KPGPC,I)+JPGPPE) = ITABL(KDGPC,I,JDGPPE)
  100 CONTINUE
C
      RETURN
      END
