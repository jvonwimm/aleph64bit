*CD tpstat
      PARAMETER (LTPST=44)
      COMMON /TPSTAT/   JTPSTA (LTPST)
      REAL RTPSTA(LTPST)
      EQUIVALENCE(RTPSTA(1),JTPSTA(1))
C
#if defined(DOC)
      JTPSTA     1-11   statistics on padrow hits
                12-22                 trigger pad hits
                23-33   statistics on tracks
               44      time spent in TPCSIM
#endif
