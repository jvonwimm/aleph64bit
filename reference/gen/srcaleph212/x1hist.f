      SUBROUTINE X1HIST
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 87/04/04  FOR GALEPH 17.0
C.
C.
C! - Fill the Level1  Trigger Histograms
C.
C.
C.
C. - Called by      X1TRIG                        from this .HLB
C. - Calls          HF1                           from HBOOK
C.
      SAVE
C.
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL BTEST
      DIMENSION IEQUI(2)
      EQUIVALENCE (IEQUI ,ITRG12)
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
C  - Fill accepted trigger segments for each trigger
C.
      DO 101 K=1,NPHTR
        IF(IEQUI(2*K).EQ.0.AND.IEQUI(2*K-1).EQ.0)  GOTO 101
        DO 102 L=1,NFSEG
          IF (L.GT.NBITVW) THEN
            N  = 2*K - 1
            IS = L - NBITVW - 1
          ELSE
            N  = 2*K
            IS = L - 1
          ENDIF
          IF(BTEST(IEQUI(N),IS)) CALL HF1(1100+K,FLOAT(L),1.)
 102    CONTINUE
 101  CONTINUE
C
C -  Fill physics trigger bits
C
      DO 111 L=1,NPHTR
        IF(BTEST(NTRBIT,L-1)) CALL HF1(1111,FLOAT(L),1.)
 111  CONTINUE
C
C -  Fill total energy values
C
      DO 121 L=1,NTOEV
       CALL HF1(1120+L,FLOAT(IECTTE(L))/1000.,1.)
       CALL HF1(1124+L,FLOAT(IHCTTE(L))/1000.,1.)
  121 CONTINUE
      RETURN
      END
