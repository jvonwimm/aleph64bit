      PARAMETER (MXPLCL=3*LMXPCL/2,MXSPCL=LMXSCL*LMXPCL/2,
     &           MXSBCL=MXPLCL*3/2)
      PARAMETER (JSPWT0=1,JSPWNS=2,JSPWOF=3,JSPWPK=4,JSPWTC=5,
     &           JSPWSZ=6,JSPWS2=7,JSPWQF=8,JSPWPF=9,JSPWPB=10,
     &           JSPWPU=11,LENSPW=11)
      PARAMETER (JIPDOP=1,JIPDNP=2,JIPDOU=3,LPIDXW=3)
      PARAMETER (JICLNS=1,JICLOF=2,JICLPN=3,JICLQF=4,JICLCA=5,
     &           JICLTA=6,JICLRW=7,JICLZW=8,JICLCV=9,LENICL=9)
      PARAMETER (JTPDT0=1,JTPDNS=2,JTPDOF=3,JTPDPN=4,JTPDTP=5,LWTPAD=10)
      COMMON/TCLCOW/ ITPADW,ITPDIW,ITSPUW,ISCLUS,ITPRTW,NPADSW,
     &               IPIDXW(LPIDXW,LMXPCL),LVLSAT,JTANLM,JFSTBK,
     &               JPLMXD,JWNDOW,ISLTCL
#if defined(DOC)
C!  Internal working arrays and workbank indices for the TPC cluster
C  analysis module (TCLCOR).  The definitions of LMXPCL and LMXSCL
C  can be found in the comdeck TPARAC.
C
C  ITPADW = Index for workbank of unpacked hit words for single cluster
C  ITPDIW = Index for workbank of unpacked pad samples for single clust
C  ITSPUW = Workbank index for unsorted subpulse parameters for all
C           subpulses in a single cluster
C  ITPRTW = Workbank index for pointers into the ITSPUW workbank
C  ISCLUS = Workbank index of subclusters within a single cluster
C  IPIDXW = Pulse and subpulse pointers for pads in a single cluster
C     -----------------------------------------------------------------
C     |  IPIDXW  column definitions:                                  |
C     |                                                               |
C     |  JIPDOP    1       Offset to first pulse in ITPADW            |
C     |  JIPDNP    2       Number of pulses on the pad                |
C     |  JIPDOU    3       Pointer to first unused subpulse in ITSPUW |
C     |                    for this pad (=0 if none)                  |
C     -----------------------------------------------------------------
C  NPADSW = Number of pads in the current cluster in TCLCOR.
C  LVLSAT = Saturation level of TPDs (= 255-pedestal)
C  JTANLM = 1 if tan(lambda) of cluster < RTLCTL, otherwise set to 2
C  JFSTBK = First bucket number of the cluster
C  JPLMXD = Definition of a significant peak in TPLANA.  This is
C           derived from three parameters from bank TLCT plus
C           the cosine of the polar angle of the cluster.
C  JWNDOW = Z window for this cluster (dip angle dependant)
C  MXPLCL = Initial maximum number of pulses in workbank ITPADW.
C           The workbank will be extended in case this limit is exceeded
C  MXSPCL = Initial maximum number of samples in workbank ITPDIW.
C           The workbank will be extended in case this limit is exceeded
C  MXSBCL = Maximum number of subpulses in workbank ITSPUW.
C           The workbank will be extended in case this limit is exceeded
C  ISLTCL = TPC sector slot number that the cluster is in
C
C++   Work Bank definitions:
C
C     -----------------------------------------------------------------
C     |  +--------+                                                   |
C     |  | ITPADW |        Unpacked hit information                   |
C     |  +--------+                                                   |
C     |                    Number of words per hit                    |
C     |                    Number of hits                             |
C     |---------------------------------------------------------------|
C     |       1   /I   t0 (time of the first sample)       JTPDT0     |
C     |       2   /I   number of samples                   JTPDNS     |
C     |       3   /I   offset of first sample in ITPDIW    JTPDOF     |
C     |       4   /I   pad number                          JTPDPN     |
C     |       5   /I   index into TPUL                     JTPDTP     |
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     |  +--------+                                                   |
C     |  | ITPDIW |        Unpacked samples for a single cluster      |
C     |  +--------+                                                   |
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     |  +--------+                                                   |
C     |  | ITSPUW |        Unsorted subpulses                         |
C     |  +--------+                                                   |
C     |                    Number of words per subpulse               |
C     |                    Number of subpulses                        |
C     |---------------------------------------------------------------|
C     |  JSPWT0    1       Time of the first sample                   |
C     |  JSPWNS    2       Number of samples in the subpulse          |
C     |  JSPWOF    3       Offset into ITPDIW for the first sample    |
C     |  JSPWPK    4       peak sample number                         |
C     |  JSPWTC    5       Total charge of the subpulse               |
C     |  JSPWSZ    6       Sum of bucket number times charge          |
C     |  JSPWS2    7       Sum of bucket-number**2 times charge       |
C     |  JSPWQF    8       Quality word of the subpulse               |
C     |  JSPWPF    9       Pointer to the next unused subpulse        |
C     |  JSPWPB   10       Pointer to the previous unused subpulse    |
C     |  JSPWPU   11       pulse number in ITPADW workbank            |
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     |  +--------+                                                   |
C     |  | ISCLUS |        Subclusters within current cluster         |
C     |  +--------+                                                   |
C     |                    Number of words per subcluster             |
C     |                    Number of subclusters                      |
C     |---------------------------------------------------------------|
C     |                                                               |
C     |  JICLNS  1  number of subpulses in the subcluster             |
C     |  JICLOF  2  offset into ITPRTW for the first subpulse pointer |
C     |  JICLPN  3  first pad number in the subcluster                |
C     |  JICLQF  4  quality flag                                      |
C     |  JICLCA  5  charge algorithm                                  |
C     |  JICLTA  6  time algorithm                                    |
C     |  JICLRW  7  mean square length in r*phi                       |
C     |  JICLZW  8  mean square width in z                            |
C     |  JICLCV  9  covariance of z and r*phi mean squares            |
C     -----------------------------------------------------------------
C
C-----------------------------------------------------------------------
#endif
