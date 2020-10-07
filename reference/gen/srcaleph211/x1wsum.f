      SUBROUTINE X1WSUM
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/15  FOR ALEPHLIB 204
C.
C.
C! - Print Level1 Trigger Summary
C.
C.
C. - Called by      ASWSUM                        from .GALEPH
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
      COMMON/XTRCOM/NACCTR,NACCL1,NACCL2,NACCL3
C ----------------------------------------------------------------------
C
C.
C. - Print trigger summary
C.
      WRITE(IW(6),100) NACCL1,NPHYTR
 100  FORMAT(///,
     x 22X,' Events accepted by Level1        :',I10,////,
     x 26X,' Single Muon            Trigger   :',I10,/,
     x 26X,' Single Charged El.mag. Trigger   :',I10,/,
     x 26X,' Single Neutral El.mag. Trigger   :',I10,/,
     x 26X,' Total Energy Barrel    Trigger   :',I10,/,
     x 26X,' Total Energy Endcap_A  Trigger   :',I10,/,
     x 26X,' Total Energy Endcap_B  Trigger   :',I10,/,
     x 26X,' Total Energy A*B       Trigger   :',I10,/,
     x 26X,' Bhabha LCAL            Trigger   :',I10,/,
     x 26X,' Bhabha SICAL           Trigger   :',I10)
C.
      RETURN
      END
