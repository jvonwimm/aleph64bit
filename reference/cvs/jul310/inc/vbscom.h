      INTEGER NBSBUF, LBPTRK, IBSUNI
      REAL BSPFAC, BSEFAC
      INTEGER IBSTRK, IBSBUF, JBSBUF, KBSBUF, IBSFEV
      INTEGER IVBPC, ICHOP, NCHOP
      REAL BSD0SCUT, XBMC, YBMC, SIGX0, SIGY0, S2MIN
      PARAMETER ( NBSBUF = 2 )
      PARAMETER ( LBPTRK = 4 )
      PARAMETER ( IBSUNI = 32 )
      PARAMETER ( BSPFAC = 0.00005 )
      PARAMETER ( BSEFAC = 0.00001 )
      COMMON /VBSCOM/ IBSTRK(NBSBUF), IBSBUF, JBSBUF, KBSBUF, IBSFEV,
     &          BSD0SCUT, IVBPC, ICHOP, NCHOP, XBMC, YBMC,
     &          SIGX0, SIGY0, S2MIN
#if defined(DOC)
NBSBUF   = number of buffers
LBPTRK   = number of attributes of work banks
IBSUNI   = unit number for printout of ALPB bank (for debug)
BSPFAC   = packing factor for beam position
BSEFAC   = packing factor for uncertainty on beam position
IBSTRK   = indices of work banks
IBSBUF   = buffer number for active buffer
JBSBUF   = buffer number for backup buffer
KBSBUF   = buffer number (used for swapping the other buffer numbers)
IBSFEV   = first event number in run
BSD0SCUT = d0 sigma cut for the VCBFIT
IVBPC    = row index in VBPC = LEP energy regime number

The following permit the accumulated tracks in the active buffer
to be fitted in customized chunks.  Under normal conditions,
ICHOP = JCHOP = 0.

ICHOP    = chunk number to be fitted
NCHOP    = total number of chunks

XBMC,YBMC = effective beam position to be used for extracting the
impact parameters in Monte Carlo events only.  Under normal conditions,
no beam position measurement is made for Monte Carlo.

SIGX0,SIGY0 = rms sizes of luminous region

S2MIN    = sigma(d0)**2 corresponding to the maximum weight a track
should be given in the *first iteration* of the beam position fit.
#endif
