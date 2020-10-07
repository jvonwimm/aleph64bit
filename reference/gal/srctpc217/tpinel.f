      SUBROUTINE TPINEL
C-----------------------------------------------------------------------
C!  Initialize BOS work banks for the electronics routines of
C!  the TPC simulation
C
C  Called from:  TSINIT
C  Calls:        WBANK, TPSHAP, TPNOIS, WDROP, WGARB
C
C  Inputs:   TPCONS.INC:  --NTMXAN, the max num of bins in an analog
C                                   signal
C                         --NTMXDI, the max num of bins in a digitized
C                                   signal
C
C  Outputs:  BANKS:       --work bank id ITMADC, bank for single
C                           digitized pulse in electronics routine
C                           TPRESP, where it is filled as needed;
C                           dropped in TSFINI
C  D. DeMille
C
C  Modifications:
C
C     1.  22feb89 DFC -- Add in error message in case the call to WBANK
C                        gives an error return.
C-----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
C  TPCBOS contains parameters for handling BOS banks used in the
C  generation of analog and digitized signals in the TPC
C  NCHAN = number of channel types for analog signals and digitizations
C  at present, NCHAN = 3; 1 = wires, 2 = pads, 3 = trigger pads.
      PARAMETER ( NCHAN = 3 )
C
C  Work bank id's.  INDREF(ich) = index for signal reference bank for
C  channel of type ich; INDSIG = index for signal bank for current
C  channel.  IDCLUS = index for cluster bank
C
      COMMON/WORKID/INDREF(NCHAN),INDSIG,IDCLUS,ITSHAP,ITDSHP,
     *              ITPNOI,ITSNOI,ITPULS,ITMADC,INDBRT,INDHL,INDDI
C
C  Parameters for analog signal work banks:  for each type of channel,
C  include max number of channels, default number of channels in
C  signal bank, and number of channels by which to extend signal bank
C  if it becomes full; also keep counter for number of blocks actually
C  filled in signal bank
C
      COMMON/ANLWRK/MAXNCH(NCHAN),NDEFCH,NEXTCH,NTSGHT
C
C  Parameters for digitises (TPP) output banks
C
      COMMON/DIGBNK/NDIDEF(3),NDIEXT(3)
C
C  Hit list and digitization bank parameters: for each type of channel
C  include name nam, default length ndd, and length of extension nde.
C
      COMMON/TPBNAM/DIGNAM(2*NCHAN)
      CHARACTER*4 DIGNAM
C  Name index for track element bank
      COMMON/TPNAMI/NATPTE
C
C
C  TPCONS contains physical constants for TPC simulation
C
      COMMON /DELTA/ CDELTA,DEMIN,DEMAX,DELCLU,RADFAC,CYLFAC
      PARAMETER (MXGAMV = 8)
      COMMON /TGAMM/ GAMVAL(MXGAMV),GAMLOG(MXGAMV),POIFAC(MXGAMV),
     &               POIMAX,POIMIN,CFERMI,CA,CB,POIRAT,POICON
      PARAMETER (MXBINC = 20)
      COMMON /CLUST/ EBINC(MXBINC),CONCLU,WRKFUN,MXCL,CKNMIN,CFANO,CRUTH
     &              ,POWERC
      COMMON /AVALA/ THETA,ETHETA
      COMMON /TPTIME/ NTMXSH,NTMXNO,NTMXAN,NTMXDI,NTSCAN,NTBNAS,NTBAPD
      COMMON /TPELEC/ TPRPAR,TPRSER,TPCFET,TCFEED,TMVPEL,TSIGMX,NTPBIT
C
C  Book the banks for handling single raw analog and shaped/digitized
C  signals in the electronics routines
C
      CALL WBANK(IW,ITMADC,NTMXDI,*999)
C
C  Fill banks with the shaping signal and its derivative
C
      CALL TPSHAP
C
C  Fill banks with noise arrays
C
      CALL TPNOIS
C
C  Drop the bank containing the derivative of the shaping signal
C  and mop up the extra room now in the work bank area.  This should
C  not be too big an operation, and may keep us from doing bigger
C  moves and copies later.
C
      CALL WDROP(IW,ITDSHP)
      CALL WGARB(IW)
C
      RETURN
  999 WRITE(6,'(/'' +++TPINEL+++ Insufficient BOS array space!''/)')
      RETURN
      END
