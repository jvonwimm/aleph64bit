      SUBROUTINE X1REDO
C ----------------------------------------------------------------------
C
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204
C.
C! - Redo trigger analysis with different thresholds
C.
C.
C.   Bank     : X1IP                         is read
C ----------------------------------------------------------------------
      SAVE
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      COMMON/X1NAMC/NAXTBN,NAXTCN,NAXTDI,NAXTEB,NAXSGE,NAXSHI,
     &              NAXSSC,NAX1AD,NAX1SC,NAX1TH,NASIXA,NASIX2,NASIFO,
     &              NAX1RG,NAX1IP,NAX1TV
C.
C ----------------------------------------------------------------------
C.
      PARAMETER (I2P16=65536,ISMAX=I2P16-1)
C.
C ----------------------------------------------------------------------
C.
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
C.
C-   Get name index for bank ('SIX2')
C.
      NASIX2 = NAMIND('SIX2')
C.
C-   Check if trigger bank ('X1RG') available
C.
      NAX1RG = NAMIND('X1RG')
      KX1RG = IW(NAX1RG)
      IF (KX1RG.EQ.0) GOTO 999
C-   Check if threshold bank ('X1TV') available
C.
      NAX1TV = NAMIND('X1TV')
      KX1MT = IW(NAX1TV)
      IF (KX1MT.EQ.0) GOTO 999
C.
C-   Check if trigger input bank ('X1IP') available
C.
      NAX1IP = NAMIND('X1IP')
      KX1IP = IW(NAX1IP)
      IF (KX1IP.EQ.0) GOTO 999
      KX1IP = KX1IP + LMHLEN
      NRO   = IW(KX1IP)
      NCO   = IW(KX1IP-1)
      DO 100 I = 1, NRO
        LX1IP = KX1IP+1
        IF (IW(KX1IP+1).EQ.INTCHA('HCW ')) THEN
          DO 201 J = 1, NSEGM, 2
            LX1IP = LX1IP + 1
            IHWSUM(J)   = IW(LX1IP)/I2P16
            IHWSUM(J+1) = MOD(IW(LX1IP),I2P16)
  201     CONTINUE
        ELSEIF (IW(KX1IP+1).EQ.INTCHA('ECW ')) THEN
          DO 202 J = 1, NSEGM, 2
            LX1IP = LX1IP + 1
            IEWSUM(J)   = IW(LX1IP)/I2P16 - 200
C           IEWSUM(J+1) = MOD(IW(LX1IP),I2P16)
            IEWSUM(J+1) = IW(LX1IP) - (IEWSUM(J)+200)*I2P16 - 200
  202     CONTINUE
        ELSEIF (IW(KX1IP+1).EQ.INTCHA('MISC')) THEN
          DO 203 J = 1, NSEGL
            LX1IP = LX1IP + 1
            ILWSUM(J)   = IW(LX1IP)
  203     CONTINUE
          DO 204 J = 1, NTEEW
            LX1IP = LX1IP + 1
            IECWTE(J)   = IW(LX1IP)
  204     CONTINUE
          DO 205 J = 1, NSEGI
            LX1IP = LX1IP + 1
            IITSUM(J)   = IW(LX1IP)
  205     CONTINUE
        ENDIF
        KX1IP = KX1IP + NCO
  100 CONTINUE
C
      CALL X1DISN
C
      CALL X1APTN
C
  999 CONTINUE
      RETURN
      END
