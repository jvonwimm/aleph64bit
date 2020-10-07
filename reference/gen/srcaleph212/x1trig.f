      SUBROUTINE X1TRIG
C ----------------------------------------------------------------------
C
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204
C.
C.
C! - Apply Level1 trigger conditions
C.
C.
C. - Banks filled in the GALEPH trigger part : X1RG (as for real data)
C.                                             but only row TPR filled
C.                                             X1IP (MC only)
C.
C. - Called  by       ASTRIG                   from GALEPH
C. - Calls            X1MIXN                   from this .HLB
C.                    X1DISN                      "
C.                    X1APTN                      "
C.                    X1HISN                      "
C ----------------------------------------------------------------------
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
      PARAMETER(JX1IIP=1,JX1ICO=2,LX1IPA=37)
      PARAMETER(JX1RNA=1,JX1RCO=2,LX1RGA=4)
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
      EXTERNAL GTSTUP
      INTEGER GTSTUP
C
C     Initialize Level1 trigger banks
C
      NAX1RG = NAMIND('X1RG')
      NAX1IP = NAMIND('X1IP')
      NAX1TV = NAMIND('X1TV')
      IF (IW(NAX1RG).NE.0.OR.IW(NAX1IP).NE.0) CALL BDROP(IW,'X1RGX1IP')
C
      LEN=LMHLEN+LX1RGA
      CALL AUBOS('X1RG',0,LEN,KX1RG,IGARB)
      IF (KX1RG .EQ. 0) GOTO 999
      IW(KX1RG+1)=LX1RGA
      IW(KX1RG+2)=1
C
      LEN=LMHLEN+3*LX1IPA
      CALL AUBOS('X1IP',0,LEN,KX1IP,IGARB)
      IF (KX1IP .EQ. 0) THEN
         CALL BDROP (IW,'X1RG')
         GOTO 999
      ENDIF
      IW(KX1IP+1)=LX1IPA
      IW(KX1IP+2)=3
C
      CALL BLIST(IW,'E+','X1RGX1IP')
C  If required drop SICAL existing trigger banks and add them to E list
      MCRUN = 1
      ISTUP = GTSTUP ('SI',MCRUN)
      IF ( ISTUP.GT.0) THEN
        CALL BDROP(IW,'SIXASIX2')
        CALL BLIST(IW,'E+','SIX2SIXA')
      ENDIF
      NAX1RG = NAMIND('X1RG')
      NAX1IP = NAMIND('X1IP')
C
      CALL X1MIXN
C.
      CALL SIXAMK
C.
      CALL X1DISN
C.
      CALL X1APTN
C.
      CALL X1HISN
C.
 999  CONTINUE
C.
      RETURN
      END
