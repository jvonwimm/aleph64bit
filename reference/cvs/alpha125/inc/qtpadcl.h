      PARAMETER (MNPDTP = 2, MXPDTP = 4)
      COMMON/TPADCL/ PNRMCL(LTSECT),JPTRCL, MNSPTP
#if defined(DOC)
C!    Calibration constants for TPC pad dE/dx.
C     These come from database banks TC1X and TP1X
C
C       MNPDTP  =   Minimum number of required pads in subcluster
C       MXPDTP  =   Maximum number of allowed pads in subcluster
C       PNRMCL  =   Sector normalization factor
C       JPTRCL  =   percentage of samples at high end to truncate
C       MNSPTP  =   minimum number of samples for pad dE/dx
#endif
