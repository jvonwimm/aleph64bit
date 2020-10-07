      SUBROUTINE X1INI(IRUN)
C ----------------------------------------------------------------------
C.
C. - Author: E. Blucher - 15-FEB-1989.
C. ----Modified version of X1IRUN by A. Putzer, C. Geweniger.
C. - Modified : C.Geweniger - 890900 for ALEPHLIB 9.9
C. - MODIFIED : B. BLOCH-DEVAUX 940107 TO INCLUDE INIT OF SICAL BANKS
C.
C! - Initialize the Level1 Trigger for run# IRUN
      SAVE
C
      EXTERNAL NAMIND
      INTEGER AGETDB
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/XTRCOM/NACCTR,NACCL1,NACCL2,NACCL3
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
      DATA IFI /0/
C.
C ----------------------------------------------------------------------
C.
C - first entry
C
      IF (IFI .EQ. 0) THEN
         IFI = 1
         NAX1TH = NAMIND ('X1TH')
         NAXTEB = NAMIND ('XTEB')
         NAX1AD = NAMIND ('X1AD')
         NASIXA = NAMIND ('SIXA')
         NASIX2 = NAMIND ('SIX2')
         NASIFO = NAMIND ('SIFO')
      ENDIF
C.
C.  -  Reset Level1 Physics Trigger counters
C
      NACCL1  = 0
      DO 98 I = 1,NPHTR
 98   NPHYTR(I) = 0
C.
C.  -  Get table containing trigger thresholds for this run
C      Print a message if X1TH bank is not found
C.
      IND = AGETDB('X1TH',IRUN)
      IF (IND.EQ.0) THEN
         WRITE(IW(6),991) IRUN
 991     FORMAT(//1X,'+++X1INI+++ X1TH bank is missing for run= ',I5/
     &            13X,'====> No Thresholds found for this run '/
     &            13X,'Thresholds from previous run will be used if',
     &             1X,'still exist'///)
      ENDIF
C
      END
