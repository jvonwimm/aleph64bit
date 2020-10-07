      SUBROUTINE X1INP(IERR)
C----------------------------------------------------------------------
C   Author   :- E. Blucher     10-FEB-1989
C      (Comments from J. Boudreau, C. Geweniger.)
C   Modified : C.Geweniger - 890900 for ALEPHLIB 9.9
C
C   Inputs:
C        - banks X1AD, XTEB
C   Outputs:
C        - IHTSUM, IHWSUM, IETSUM, IEWSUM, ILTSUM, IITSUM, IHCTTE,
C          IECTTE, IECWTE
C        - IERR / I  = error code
C                      0 means OK
C                      1 means input bank(s) missing
C
C   Description
C   ===========
C   Read and unpack trigger ADC bank (X1AD).  Function is the reverse
C   of GALEPH routine X1OUTP.  In addition, the functions of routine
C   X1MIXI (except for filling IHTSUM, IHWSUM, IETSUM, IEWSUM, ILTSUM)
C   are duplicated. IITSUM is filled from bank XTEB
C----------------------------------------------------------------------
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
      PARAMETER(JXTET1=1,JXTET2=2,JXTEL2=3,JXTEHT=4,JXTEHW=16,JXTELW=28,
     +          JXTEEW=40,JXTELT=52,JXTETE=56,JXTEIT=58,JXTETP=62,
     +          LXTEBA=65)
      PARAMETER(JX1AIT=1,JX1AAV=2,LX1ADA=37)
C.
C ----------------------------------------------------------------------
C.
      PARAMETER (LXT1=1,NSEG2=NSEGM/2,NHCT=1,NHCW=NHCT+NSEG2)
      PARAMETER (NECT=NHCW+NSEG2, NECW=NECT+NSEG2, NXT1=NECW+NSEG2)
      PARAMETER (NPADC=96,NCADC=4096)
      PARAMETER (NFSET=5,NFSHW=150,NFSHT=5,NFSEW=10)
      PARAMETER (NFTET=40,NFTHT=40,NFTEW=80,NFSLT=40)
      PARAMETER (I2P16=65536)
      PARAMETER (NSEGE=12)

      CHARACTER*4 CHAINT,IDET
      EXTERNAL CHAINT
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
C
      IERR=0
      DO 1 IFL =1,NTOEV
         IECTTE(IFL) = 0
         IHCTTE(IFL) = 0
 1    CONTINUE
      DO 2 IFL =1,NTEEW
         IECWTE(IFL) = 0
 2    CONTINUE
C
      KXTEB = IW(NAXTEB)
      KX1AD = IW(NAX1AD)
      IF (KXTEB.EQ.0.OR.KX1AD.EQ.0) THEN
          IERR = 1
          RETURN
      ENDIF
      NPTR = LROWS(KX1AD)
      DO 10 ITR=1,NPTR
        IDET=CHAINT(ITABL(KX1AD,ITR,JX1AIT))
c
C---fill different trigger sums
c
C -  HC tower trigger
        IF (IDET.EQ.'HCT ') THEN
          DO 101 I = 1,NSEGM,2
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSHT channels/GeV
C -                (Output values in MeV)
C -
            JI=JX1AAV + (I-1)/2
            IHITI=ITABL(KX1AD,ITR,JI)/I2P16
            IHITI1=ITABL(KX1AD,ITR,JI) - IHITI*I2P16
            IHTSUM(I)=NFSHT*(IHITI - NPADC)
            IHTSUM(I+1)=NFSHT*(IHITI1 - NPADC)
  101     CONTINUE
C
C -  HC wire trigger
        ELSEIF (IDET.EQ.'HCW ') THEN
          DO 201 I = 1,NSEGM,2
C.
C -  ADC values :  0 planes = channel NPADC
C -                for each plane fired NFSHW channels are added
C.
            JI=JX1AAV + (I-1)/2
            IHITI=ITABL(KX1AD,ITR,JI)/I2P16
            IHITI1=ITABL(KX1AD,ITR,JI) - IHITI*I2P16
            IHWSUM(I)=(IHITI - NPADC)/NFSHW
            IHWSUM(I+1)=(IHITI1 - NPADC)/NFSHW
  201     CONTINUE
C.

C -  EC Tower Trigger
        ELSEIF (IDET.EQ.'ECT ') THEN
          DO 301 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSET channels/GeV
C -                (Output values in MeV)
C -
            JI=JX1AAV + (I-1)/2
            IHITI=ITABL(KX1AD,ITR,JI)/I2P16
            IHITI1=ITABL(KX1AD,ITR,JI) - IHITI*I2P16
            IETSUM(I)=NFSET*(IHITI - NPADC)
            IETSUM(I+1)=NFSET*(IHITI1 - NPADC)
  301     CONTINUE
C.
C -  EC wire trigger
        ELSEIF (IDET.EQ.'ECW ') THEN
          DO 401 I = 1,NSEGM,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSEW channels/GeV
C -                (Output values in MeV)
C -
            JI=JX1AAV + (I-1)/2
            IHITI=ITABL(KX1AD,ITR,JI)/I2P16
            IHITI1=ITABL(KX1AD,ITR,JI) - IHITI*I2P16
            IEWSUM(I)=NFSEW*(IHITI - NPADC)
            IEWSUM(I+1)=NFSEW*(IHITI1 - NPADC)
  401     CONTINUE
C.
C -  LC tower trigger
        ELSEIF (IDET.EQ.'LCT ') THEN
          DO 501 I = 1,NSEGL,2
C -
C -  ADC values :  0.0 GeV = channel NPADC
C -                add 1000/NFSLT channels/GeV
C -                (Output values in MeV)
C -
            JI=JX1AAV + (I-1)/2
            IHITI=ITABL(KX1AD,ITR,JI)/I2P16
            IHITI1=ITABL(KX1AD,ITR,JI) - IHITI*I2P16
            ILTSUM(I)=NFSLT*(IHITI - NPADC)
            ILTSUM(I+1)=NFSLT*(IHITI1 - NPADC)
  501     CONTINUE
        ENDIF
   10 CONTINUE
c
c---TOTAL ENERGY (Endcap A, Endcap B, Barrel, Total)
c
c---HC tower trigger.
c
      DO 701 I = 1,NSEGM
        IF (I.LE.NSEGE) THEN
C -  ENDCAP A
          KST = 1
        ELSE IF (I.GT.(NSEGM-NSEGE)) THEN
C -  ENDCAP B
          KST = 2
        ELSE
C -  BARREL
          KST = 3
        ENDIF
        IHCTTE(KST) = IHCTTE(KST) + IHTSUM(I)
  701 CONTINUE
      IHCTTE(4) = IHCTTE(1) + IHCTTE(2) + IHCTTE(3)
C
C -  EC Tower Trigger
C
      DO 801 I = 1,NSEGM
        IF (I.LE.NSEGE) THEN
C -  ENDCAP A
          KST = 1
        ELSE IF (I.GT.(NSEGM-NSEGE)) THEN
C -  ENDCAP B
          KST = 2
        ELSE
C -  BARREL
          KST = 3
        ENDIF
        IECTTE(KST) = IECTTE(KST) + IETSUM(I)
  801 CONTINUE
      IECTTE(4) = IECTTE(1) + IECTTE(2) + IECTTE(3)
C.
C.
C -  EC wire trigger
C.
C.
      DO 1001 I = 1,NSEGM,2
        IF (I.LE.24) THEN
C -  ENDCAP A
          KST = 1
        ELSE IF (I.GT.48) THEN
C -  ENDCAP B
          KST = 2
        ELSE
C -  BARREL
          KST = 3
        ENDIF
        IECWTE(KST) = IECWTE(KST) + IEWSUM(I)
        IECWTE(KST+NTOEV) = IECWTE(KST+NTOEV) + IEWSUM(I+1)
 1001 CONTINUE
      IECWTE(4) = IECWTE(1) + IECWTE(2) + IECWTE(3)
      IECWTE(4+NTOEV)=IECWTE(1+NTOEV)+IECWTE(2+NTOEV)+IECWTE(3+NTOEV)
C
C--------ITC-------------
C---Fill IITSUM from XTEB
C
      IF (KXTEB.NE.0) THEN
        KK = KXTEB + LMHLEN + JXTEIT
        IITSUM(1)=IW(KK)
        IITSUM(2)=IW(KK+1)
        IITSUM(3)=IW(KK+2)
        IITSUM(4)=IW(KK+3)
      ENDIF
C
  999 RETURN
      END
