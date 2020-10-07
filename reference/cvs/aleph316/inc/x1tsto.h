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
#if defined(DOC)
      NTRHL  =    Number of TResHoLd values
      NBTWD  =    Number of BiTWorDs needed
      NPHTR  =    Number of PHysics TRiggers
      NSEGM  =    Number of trigger SEGMents
      NSEGL  =    Number of trigger signals from Lc wires
      NTOEV  =    Number of total energy values
      NTEEW  =    Number of Total Energy values for Ec Wires
      NFSEG  =    Number of Final trigger SEGments
      NBITVW =    Number of bits per VAX word
      NBYTVW =    Number of bytes per VAX word
#endif
