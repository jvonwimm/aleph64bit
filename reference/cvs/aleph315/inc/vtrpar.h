      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
#if defined(DOC)
C! internal common to VDTRACK
        MAXCLS      Maximum number of cluster loaded in VTUC and VTWC
        MAXCOM      Maximum number of combinaisons allowed in VTSC
        IVFRFT      FRFT bank Id for the TPC+ITC+VDET track fit
        C2PRCL      Chisquare increase per layer
        SEACUT      Number of st. dev. for the clsuter area search
        CI2CUT      Chisquare cut for the cluster combinaisons in U or W
        BIGERR      U or W error set for missing cluster
        PULMIN      Min value for the pulse size. (use for shared hits)
        USNOIS      Typical signal/noise for U cluster
        WSNOIS      Typical signal/noise for U cluster
        HBIGER      Half value of BIGERR
        NLYRMX      maximum number of layers on a track
#endif
