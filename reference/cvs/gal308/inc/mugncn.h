*CD mugncn
C! The general constants to create MU signals
      COMMON/MUGNCN/WDEIMU,WDTBMU,OFTBMU,WDATMU,HTATMU,HFHTMU,
     *              SGLSMU,PSCRMU,PTSPMU,DSSPMU,
     *              PTXSMU,PTYSMU,SNSXMU(4),SNSYMU(4)
#if defined(DOC)
        WDEIMU -- width of eightfold tube
        WDTBMU -- width of tube
        OFTBMU -- offset of first tube inside eightfold tube
        WDATMU -- width of active area in tube
        HTATMU -- height of active area in tube
        HFHTMU -- half height of active area in tube
        SGLSMU -- at least length of segment to fire a tube
        PSCRMU -- possibility of that a segment fires the current tube
        DSSPMU -- distance along tube between two spacers
        PTSPMU -- pitch of spacers
        PTXSMU -- pitch of X-strip
        PTYSMU -- pitch of Y-strip
        SNSXMU -- sensitivity of X-strip (possibility of having a signal
                  at the current, the first adjacent and tne second
                  adjacent X strips by a streamer)
        SNSYMU -- sensitivity of Y-strip (possibility of having a signal
                  at the current, the first adjacent and tne second
                  adjacent Y strips by a streamer)
#endif
