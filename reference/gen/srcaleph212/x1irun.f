      SUBROUTINE X1IRUN(IRUN)
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/03/20  FOR ALEPHLIB 205
C.
C! - Initialize the Level1 Trigger Part of GALEPH
C.
C. - Banks    : X1TV (Trigger thresholds taken from data base)
C.              Bank formats defined for trigger output banks
C.
      SAVE
      EXTERNAL NAMIND ,GTSTUP,AGETDB
      INTEGER AGETDB ,GTSTUP
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
C.  -  Reset Level1 Physics Trigger counters
C.
      DO 98 I = 1,NPHTR
 98   NPHYTR(I) = 0
      NAX1TV = NAMIND ('X1TV')

C.  -  Get Sical thresholds if needed and trigger related constants
      MCRUN = 1
      ISTUP = GTSTUP ('SI',MCRUN)
      IF ( ISTUP.GT.0) THEN
         JND = AGETDB('SITCSRCO',ISTUP)
         IF (JND.NE.0) GO TO 97
         WRITE(IW(6),990)
 990  FORMAT(////,'  Warning No SICAL Thresholds found for this run  ',
     $       /,'  Trigger will be applied using defaults settings',
     $       ///)
 97      CONTINUE
         CALL SITRIN
      ENDIF
C.
C.
C.  -  Get table containing trigger thresholds for this run
C.
C.
      IND = AGETDB('X1TV',IRUN)
      IF (IND.NE.0) GO TO 99
      WRITE(IW(6),991)
 991  FORMAT(////,'  Warning No Thresholds found for this run; ',
     X          /,'  Trigger cannot be applied!!!!',
     X          /,'  Therefore the job will be stopped.',
     X          /,'  Resubmit without requiring the trigger'
     X          /,'  part or provide the bank X1TV',///)
      CALL ALTELL('X1IRUN Missing bank X1TV',0,'STOP')
      GO TO 999
  99  CONTINUE
      DO 100 I = 1,NPHTR
 100  NPHYTR(I) = 0
C
C - Define bank formats
C
      CALL BKFMT('X1IP','2I,(A,36I)')
      NASIXA = NAMIND ('SIXA')
      CALL BKFMT('SIXA','I')
      NASIX2 = NAMIND ('SIX2')
      CALL BKFMT('SIX2','I')
      NASIFO = NAMIND ('SIFO')
      CALL BKFMT('SIFO','I')
C
 999  CONTINUE
      RETURN
      END
