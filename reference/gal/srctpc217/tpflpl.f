      SUBROUTINE TPFLPL(ICHRF,IBIN1,NLEN)
C--------------------------------------------------------------------
C!  Fill the single-pulse work bank from the appropriate blocks
C!  in the full signal bank
C
C  Called from:  TSRESP
C  Calls:        UCOPY, VZERO
C
C  Inputs:   PASSED:      --ICHRF,  the pointer to the current channel's
C                                   block in the signal reference bank
C            /TPCBOS/     --NTSCAN, the number of divisions of the
C                                   total analog signal collection time
C                         --NTBNAS, the number of bins in each division
C                         --INDSIG, the full signal bank id
C                         --ITPULS, the single-pulse bank id
C
C  Outputs:  PASSED:      --IBIN1, the first bin of the signal
C                         --NLEN, the length of the signal
C            BANKS:       --ITPULS, (work bank id) single-pulse work
C                                   bank filled with signal from full
C                                   collection time for current channel
C                           IW(ITPULS+I) = electrons in Ith bin
C  D. DeMille
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C  Get pointers and fill pulse bank
C
      DO 1 I = 1, NTSCAN
C
         NPNT = IW(ICHRF+2+I)
         IPLB1 = ITPULS + (I-1)*NTBNAS
C
         IF ( NPNT .NE. 0 ) THEN
            IDSS = INDSIG + 2 + (NPNT-1)*IW(INDSIG+1)
            CALL UCOPY(IW(IDSS+1),IW(IPLB1+1),NTBNAS)
         ELSE
            CALL VZERO(IW(IPLB1+1),NTBNAS)
         ENDIF
C
 1    CONTINUE
C
C  First bin and length
C
      IBIN1 = IW(ICHRF+1)
      IBINL = IW(ICHRF+2)
      NLEN = IBINL - IBIN1 + 1
C
      RETURN
      END
