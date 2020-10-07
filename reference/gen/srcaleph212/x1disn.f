      SUBROUTINE X1DISN
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204
C.
C.
C! - Discriminate Level1 trigger signals
C.
C? - The analog trigger sources for the Level1 trigger are discriminated
C?   using up to 4 sets of thresholds.
C.
C ------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/X1NAMC/NAXTBN,NAXTCN,NAXTDI,NAXTEB,NAXSGE,NAXSHI,
     &              NAXSSC,NAX1AD,NAX1SC,NAX1TH,NASIXA,NASIX2,NASIFO,
     &              NAX1RG,NAX1IP,NAX1TV
      PARAMETER (NTRHL=4,NBTWD=2*NTRHL,NPHTR=9)
      PARAMETER (NSEGM=72,NSEGL= 8,NSEGI=4,NTOEV=4,NTEEW=2*NTOEV)
      PARAMETER (NFSEG=60,NBITVW=32,NBYTVW=NBITVW/8)
      COMMON/X1TSTO/ IHWSUM(NSEGM),IETSUM(NSEGM),
     *              IEWSUM(NSEGM),ILWSUM(NSEGL),IITSUM(NSEGI),
     *              IECTTE(NTOEV)              ,IECWTE(NTEEW),
     *                            NTEBIT,NLWBIT,
     *              NTRBIT,NPHYTR(NPHTR),
     *              NHTBIT(NBTWD),NHWBIT(NBTWD),NETBIT(NBTWD),
     *              NEWBIT(NBTWD),NITBIT(NBTWD),
     *              ITRG12,ITRG11,ITRG22,ITRG21,ITRG32,ITRG31,
     *              ITRG42,ITRG41,ITRG52,ITRG51,ITRG62,ITRG61,
     *              ITRG72,ITRG71,ITRG82,ITRG81,ITRG92,ITRG91
      PARAMETER(JX1TTT=1,JX1TTV=2,LX1TVA=5)
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
C
C ----------------------------------------------------------------------
C
      LOGICAL BTEST
C
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
C.
C. - Loop over the 72 trigger segments (ECW + HCW)   and apply
C.   the four different sets of tresholds.
C. - After discrimination the two barrel theta bins are combined
C.

      DO 99 I=1,NBTWD
        NHWBIT(I) = 0
        NEWBIT(I) = 0
        NITBIT(I) = 0
 99   CONTINUE
      NLWBIT = 0
      NTEBIT = 0
C
C  - Get pointers into threshold bank (X1TV)
C
      ID = IW(NAX1TV)
      IADD  = LCOLS(ID)
      IVAL    = ID + LMHLEN + 1
      DO 100 I = 1, LROWS(ID)
        IF (IW(IVAL).EQ.INTCHA('HCWI')) THEN
          IHCWI = IVAL
        ELSEIF (IW(IVAL).EQ.INTCHA('ECEC')) THEN
          IECEC = IVAL
        ELSEIF (IW(IVAL).EQ.INTCHA('ECBA')) THEN
          IECBA = IVAL
        ELSEIF (IW(IVAL).EQ.INTCHA('ETEC')) THEN
          IETEC = IVAL
        ELSEIF (IW(IVAL).EQ.INTCHA('ETBA')) THEN
          IETBA = IVAL
        ELSEIF (IW(IVAL).EQ.INTCHA('LCEC')) THEN
          ILCEC = IVAL
        ENDIF
        IVAL = IVAL + IADD
 100  CONTINUE
C
C
C  - HCAL wire trigger sources
C
      DO 101 I=1,NTRHL
        IBIT = 1
        DO 111 J=1,NSEGM
          IF (IBIT.GT.NBITVW) THEN
            JBIT = IBIT - NBITVW - 1
            K = 2*I - 1
          ELSE
            JBIT = IBIT - 1
            K = 2*I
          ENDIF
          IF (IHWSUM(J).GE.IW(IHCWI+I)) THEN
            NHWBIT(K)=IBSET(NHWBIT(K),JBIT)
          ENDIF
C
          IBIT = IBIT + 1
          IF (J.EQ.36) IBIT = 25
 111    CONTINUE
 101  CONTINUE
C.
C.
C. - ECAL wire trigger sources:
C.   * Do coincidence between odd and even planes
C.   * Map onto trigger segments
C.
C.
      DO 201 I=1,NTRHL
        KECEC = IW(IECEC+I)/2
        KECBA = IW(IECBA+I)/2
        DO 202 J=1,NSEGM,2
          IF (J.LT.24) THEN
C -  ENDCAP A
            IF (IEWSUM(J).LE.KECEC.OR.IEWSUM(J+1).LE.KECEC) GO TO 202
            JBIT = (J+3)/4 - 1
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT)
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT+6)
C
            JBIT = (J+23)/2
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT)
            IF (JBIT.EQ.23) JBIT = 11
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT+1)
          ELSE IF (J.GT.48) THEN
C - ENDCAP B
            IF (IEWSUM(J).LE.KECEC.OR.IEWSUM(J+1).LE.KECEC) GO TO 202
            JBIT = (J+15)/4
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT)
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT+6)
C
            JBIT = (J-41)/2
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT)
            IF (JBIT.EQ.15) JBIT = 3
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT+1)
          ELSE
C - BARREL
            IF (IEWSUM(J).LE.KECBA.OR.IEWSUM(J+1).LE.KECBA) GO TO 202
            JBIT = J/2
            NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT)
            NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT-8)
            IF (JBIT.LT.20) THEN
              NEWBIT(2*I) = IBSET(NEWBIT(2*I),JBIT+12)
            ELSE
              NEWBIT(2*I-1) = IBSET(NEWBIT(2*I-1),JBIT-20)
            ENDIF
          ENDIF
  202   CONTINUE
  201 CONTINUE
C
C  -  LCAL wire trigger sources
C
      LW_A = ILWSUM(1) + ILWSUM(2) + ILWSUM(3) + ILWSUM(4)
      LW_B = ILWSUM(5) + ILWSUM(6) + ILWSUM(7) + ILWSUM(8)
      DO 301 I=1,NTRHL
        KLCEC = IW(ILCEC+I)
        IF (I.LT.4) THEN
          IF (LW_A.GT.KLCEC) NLWBIT = IBSET(NLWBIT,I-1)
          IF (LW_B.GT.KLCEC) NLWBIT = IBSET(NLWBIT,I+3)
        ELSE
          IF ((LW_A+LW_B).GT.KLCEC) NLWBIT = IBSET(NLWBIT,3)
        ENDIF
  301 CONTINUE
C
C  - SICAL  trigger sources
C
      CALL SIX2MK
C
C  -  Get the ITC segment trigger bits
C
      IBIT = 1
      DO 401 J=1,NSEGM
        IF (IBIT.GT.NBITVW) THEN
          JBIT = IBIT - NBITVW - 1
          K = 1
        ELSE
          JBIT = IBIT - 1
          K = 2
        ENDIF
        IF (J.LE.NBITVW) THEN
          KK = 1
          KB = J - 1
        ELSE IF (J.GT.2*NBITVW) THEN
          KK = 3
          KB = J - 2*NBITVW - 1
        ELSE
          KK = 2
          KB = J - NBITVW - 1
        ENDIF
        IF (BTEST(IITSUM(KK),KB)) NITBIT(K)=IBSET(NITBIT(K),JBIT)
        IBIT = IBIT + 1
        IF (J.EQ.36) IBIT = 25
 401  CONTINUE
C
C - Total Energy  (ECAL Wires)
C
      DO 501 I = 1,NTRHL
        KETEC = IW(IETEC+I)/2
        KETBA = IW(IETBA+I)/2
        IF (IECWTE(1).GE.KETEC.AND.IECWTE(5).GE.KETEC)
     *      NTEBIT = IBSET(NTEBIT,4*I-4)
        IF (IECWTE(2).GE.KETEC.AND.IECWTE(6).GE.KETEC)
     *      NTEBIT = IBSET(NTEBIT,4*I-3)
        IF (IECWTE(3).GE.KETBA.AND.IECWTE(7).GE.KETBA)
     *      NTEBIT = IBSET(NTEBIT,4*I-2)
 501  CONTINUE
      RETURN
      END
