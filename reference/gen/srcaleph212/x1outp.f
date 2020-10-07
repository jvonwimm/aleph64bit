      SUBROUTINE X1OUTP
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 87/04/04  FOR GALEPH 17.0
C. - Modified : A. Putzer  - 87/10/28  FOR GALEPH 19.3
C. - Modified : C. Geweniger - 88/10/11  for GALEPH 20.1
C. - Modified : E. Blucher - 89/15/2 for ALEPHLIB
C. - Modified : C. Geweniger - 89/09/00 for ALEPHLIB 9.9
C.
C! - Fill Level 1 Trigger ADC bank
C.
C? - The energy values are converted into ADC counts.
C?
C? - The trigger output bank 'X1AD' is filled.
C.
C.
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NTRHL=4,NBTWD=2*NTRHL,NPHTR=9)
      PARAMETER (NSEGM=72,NSEGL=24,NSEGI=4,NTOEV=4,NTEEW=2*NTOEV)
      PARAMETER (NFSEG=60,NBITVW=32,NBYTVW=NBITVW/8)
      COMMON/X1TCOM/IHTSUM(NSEGM),IHWSUM(NSEGM),IETSUM(NSEGM),
     *              IEWSUM(NSEGM),ILTSUM(NSEGL),IITSUM(NSEGI),
     *              IECTTE(NTOEV),IHCTTE(NTOEV),IECWTE(NTEEW),
     *              IHCNPL(NTOEV),NTEBIT,NLTBIT(NBTWD),
     *              NTRBIT,NPHYTR(NPHTR),
     *              NHTBIT(NBTWD),NHWBIT(NBTWD),NETBIT(NBTWD),
     *              NEWBIT(NBTWD),NITBIT(NBTWD),
     *              ITRG12,ITRG11,ITRG22,ITRG21,ITRG32,ITRG31,
     *              ITRG42,ITRG41,ITRG52,ITRG51,ITRG62,ITRG61,
     *              ITRG72,ITRG71,ITRG82,ITRG81,ITRG92,ITRG91
      COMMON/X1NAMC/NAXTBN,NAXTCN,NAXTDI,NAXTEB,NAXSGE,NAXSHI,
     &              NAXSSC,NAX1AD,NAX1SC,NAX1TH,NASIXA,NASIX2,NASIFO,
     &              NAX1RG,NAX1IP,NAX1TV
C.
C ----------------------------------------------------------------------
C.
      PARAMETER (LXT1=1,NSEG2=NSEGM/2,NHCT=1,NHCW=NHCT+NSEG2)
      PARAMETER (NECT=NHCW+NSEG2, NECW=NECT+NSEG2, NXT1=NECW+NSEG2)
      PARAMETER (NPADC=96,NCADC=4096)
      PARAMETER (NFSET=5,NFSHW=150,NFSHT=5,NFSEW=10)
      PARAMETER (NFTET=40,NFTHT=40,NFTEW=80,NFSLT=40)
      PARAMETER (I2P16=65536)
C.
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
C ----------------------------------------------------------------------
C.
C  - Convert segment energies into ADC counts and fill them into the
C    bank X1AD
C.
      KX1AD=IW(NAX1AD)+LMHLEN
C.
C.
C -  HC tower trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('HCT ')
      DO 101 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSHT channels/GeV
C -                (Input values in MeV)
C -
        KX1AD = KX1AD + 1
        IW(KX1AD) = MIN((IHTSUM(I)/NFSHT + NPADC),NCADC)*I2P16 +
     +              MIN((IHTSUM(I+1)/NFSHT + NPADC),NCADC)
  101 CONTINUE
C.
C.
C -  HC wire trigger
C
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('HCW ')
      DO 201 I = 1,NSEGM,2
C.
C -  ADC values :  0 planes = channel NPADC
C -                for each plane fired NFSHW channels are added
C.
        KX1AD = KX1AD + 1
        IW(KX1AD) = MIN((IHWSUM(I)*NFSHW + NPADC),NCADC)*I2P16 +
     +              MIN((IHWSUM(I+1)*NFSHW + NPADC),NCADC)
  201 CONTINUE
C.
C.
C -  EC Tower Trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('ECT ')
      DO 301 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSET channels/GeV
C -                (Input values in MeV)
C -
        KX1AD = KX1AD + 1
        IW(KX1AD) = MAX(MIN((IETSUM(I)/NFSET + NPADC),NCADC),0)*I2P16 +
     +              MAX(MIN((IETSUM(I+1)/NFSET + NPADC),NCADC),0)
  301 CONTINUE
C.
C.
C -  EC wire trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('ECW ')
      DO 401 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSEW channels/GeV
C -                (Input values in MeV)
C -
        KX1AD = KX1AD + 1
        IW(KX1AD) = MIN((IEWSUM(I)/NFSEW + NPADC),NCADC)*I2P16 +
     +              MIN((IEWSUM(I+1)/NFSEW + NPADC),NCADC)
  401 CONTINUE
C.
C.
C -  LC tower trigger
C.
C.
      KX1AD=KX1AD+1
      IW(KX1AD)=INTCHA('LCT ')
      DO 501 I = 1,NSEGL,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSLT channels/GeV
C -                (Input values in MeV)
C -
        KX1AD=KX1AD+1
        IW(KX1AD) = MIN((ILTSUM(I)/NFSLT + NPADC),NCADC)*I2P16 +
     +              MIN((ILTSUM(I+1)/NFSLT + NPADC),NCADC)
  501 CONTINUE
C
  999 CONTINUE
      RETURN
      END
