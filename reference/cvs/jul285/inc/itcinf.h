      INTEGER JDIMMX, JLAYMX, JRESMX
      PARAMETER (JDIMMX=3, JLAYMX=8, JRESMX=80)
      INTEGER ITKITC,NHTITC,GBWITC,CLWITC,FLGITC,ICOITC
      REAL    DTMITC,DDSITC,DERITC,WRXITC,WRDITC,HTXITC,HTDITC,
     *        DCAITC,RSDITC,RSFITC,RSQITC
      COMMON/ITCINF/
     *   NTKITC,
     *   ITKITC(JRESMX), NHTITC(JRESMX), RSQITC(JRESMX),
     *   GBWITC(JLAYMX,JRESMX), ICOITC(JLAYMX,JRESMX),
     *   CLWITC(JLAYMX,JRESMX), DTMITC(JLAYMX,JRESMX),
     *   DDSITC(JLAYMX,JRESMX), DERITC(JLAYMX,JRESMX),
     *   FIWITC(JLAYMX,JRESMX),
     *   WRXITC(JDIMMX,JLAYMX,JRESMX), WRDITC(JDIMMX,JLAYMX,JRESMX),
     *   HTXITC(JDIMMX,JLAYMX,JRESMX), HTDITC(JDIMMX,JLAYMX,JRESMX),
     *   DCAITC(JLAYMX,JRESMX), RSDITC(JLAYMX,JRESMX),
     *   RSFITC(JLAYMX,JRESMX), FLGITC(JLAYMX,JRESMX),
     *   NDHITC(JRESMX),TERITC(JRESMX),XISITC(JRESMX),RMNITC(JRESMX),
     *   NGHITC(JRESMX),LRXITC(JRESMX),NRHITC(JRESMX)
#if defined(DOC)
C
C! Common block containing ITC Track Information
C
C JDIMMX        Array dimension - (k)
C JLAYMX        Array dimension - no of ITC layers  (layer i)
C JRESMX        Array dimension - max. no of allowed tracks (track j)
C
C NTKITC        No. of tracks in this bank
C ITKITC(j)     JULIA Track No.'s
C NHTITC(j)     No. of ITC hits on track
C RSQITC(j)     ITC Xi**2/hit
C GBWITC(i,j)   global wire no.
C ICOITC(i,j)   Coord. no. in ITCO
C CLWITC(i,j)   Used wire no. in fit (may not be GBW!!!)
C DTMITC(i,j)   Drift time (ns)
C DDSITC(i,j)   (sgn)Drift dist. (cm),
C DERITC(i,j)   Drift error (cm)
C FIWITC(i,j)   FI of each wire (rad)
C WRXITC(k,i,j) point on wire,
C WRDITC(k,i,j) dir. cos wire,
C HTXITC(k,i,j) point on trk,
C HTDITC(k,i,j) dir. trk.
C DCAITC(i,j)   DOCA (trk-wire),
C RSDITC(i,j)   DOCA Resid.,
C RSFITC(i,j)   Fit Resid.
C FLGITC(i,j)   Hit useage word: 0 =used in fit,
C                               -1 =mis-matched,
C                                1 =outside 1st road in IASIGN,
C                                3 =Ineff. wire,
C                                4 =Dead wire,
C                                5 =Track misses layer
C                                6 =wiped ,track has too few latch+dead wires
C                                7 =wiped ,track has too few hits in road
C                                9 =outside 2nd road in ITCCUT
C                                10=hit killed because wanted by 3 or more track
C                                11=hit killed because wanted by 2 tracks too cl
C                                12=robbed by another track
C                                13 =cut by Xi2 filter in ITCCUT
C                                14 =cut by Xi2 on only 2 hits associated
C NDHITC(j)     No. of ITC dead wires in road
C TERITC(j)     tracking error of 1st pass fit at R=20
C XISITC(j)     xis relative to mean residual to 1st pass fit
C RMNITC(j)     mean residual to 1st pass fit
C NGHITC(j)     No. of ITC hits in road after all cuts.
C LRXITC(j)     layer with maximum residual in road
C NRHITC(j)     No. of ITC hits robbed by sharing algorithim
#endif
