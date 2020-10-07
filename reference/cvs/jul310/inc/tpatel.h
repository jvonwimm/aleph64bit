      COMMON/TPATEL/ IPDRPP(LTPDCH,LTPD),IPADPP(LTPDCH,LTPD),
     1       IWIRPP(LTPDCH,LTPD),ITPDNR(LTPDRO,200)
             INTEGER  IPDRPP,IPADPP,IWIRPP,ITPDNR
#if defined(DOC)
C
C!        Patch-panel for TPC90 electronic read-out
C
C        IPDRPP(ic,itpd) = padrow in given channel per TPD
C        IPADPP(ic,itpd) = pad number for a given channel per TPD
C        IWIRPP(ic,itpd) = wire number for a given channel per TPD
C        ITPDNR(ir,ip)   = TPD number for given row and pad
#endif
C---------------------------------------------------------------------
