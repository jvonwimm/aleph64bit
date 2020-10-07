*CD tpcbos
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
