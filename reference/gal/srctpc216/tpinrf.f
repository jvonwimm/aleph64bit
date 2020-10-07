      SUBROUTINE TPINRF
C-----------------------------------------------------------------------
C! Initialize BOS signal reference banks for analog signals for each
C!  active channel type
C
C  Called from:  TSINIT
C  Calls:        BDROP, WBANK
C
C  Inputs:
C            TPCONS.INC:  --NTSCAN, the number of sections into which
C                                   to divide the full analog signal
C                                   collection time
C            TPCOND.INC:  --TPDGBN, time bin length
C                         --DRFVEL, drift velocity
C                         --LTANSV, channel types for which to save
C                                   raw analog signals
C
C  Outputs:
C            BANKS:       --named bank ANLHED, NR 0: analog signal
C                           header bank, containing flags for channel
C                           types saved, plus drift velocity and time
C                           bin length.
C                         --work bank id INDREF, analog signal reference
C                           bank, filled in TPSGNL
C
C                           IW(INDREF+1) = num of channels of type
C                           IW(INDREF+2) = words per channel
C
C                           KREF = INDREF + 2 + (JCH-1)*IW(INDREF+2)
C                           IW(KREF+1) = 1st time bin hit on JCH
C                           IW(KREF+2) = last time bin hit on JCH
C                           IW(KREF+2+JSC) = pointer into signal bank
C                                            for JSCth section of analog
C                                            signal for channel JCH
C
C                         --work bank id ITPULS, bank for analog signal
C                           pulse on one channel over full signal
C                           collection time; filled as needed in TPRESP
C                           and TORAWO; dropped in TSFINI
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
C  TPCOND  conditions under which this simulation
C  will be performed
C
      COMMON /DEBUGS/ NTPCDD,NCALDD,NTPCDT,NCALDT,NTPCDA,NCALDA,
     &                NTPCDC,NCALDC,NTPCDS,NCALDS,NTPCDE,NCALDE,
     &                NTPCDI,NCALDI,NTPCSA,NCALSA,NTPCDR,NCALDR,
     &                LTDEBU
      LOGICAL LTDEBU
      COMMON /SIMLEV/ ILEVEL
      CHARACTER*4 ILEVEL
      COMMON /GENRUN/ NUMRUN,MXEVNT,NFEVNT,INSEED(3),LEVPRO
      COMMON /RFILES/ TRKFIL,DIGFIL,HISFIL
      CHARACTER*64 TRKFIL,DIGFIL,HISFIL
      COMMON /TLFLAG/ LTWDIG,LTPDIG,LTTDIG,LWREDC,FTPC90,LPRGEO,
     &                LHISST,LTPCSA,LRDN32,REPIO,WEPIO,LDROP,LWRITE
      COMMON /TRANSP/ MXTRAN,CFIELD,BCFGEV,BCFMEV,
     &                        DRFVEL,SIGMA,SIGTR,ITRCON
      COMMON /TPCLOK/ TPANBN,TPDGBN,NLSHAP,NSHPOF
      COMMON /AVLNCH/ NPOLYA,AMPLIT
      COMMON /COUPCN/ CUTOFF,NCPAD,EFFCP,SIGW,SIGH,HAXCUT
      COMMON /TGCPCN/ TREFCP,SIGR,SIGARC,RAXCUT,TCSCUT
      COMMON /DEFAUL/ PEDDEF,SPEDEF,SGADEF,SDIDEF,WPSCAL,NWSMAX,THRZTW,
     &                LTHRSH,NPRESP,NPOSTS,MINLEN,
     &                LTHRS2,NPRES2,NPOST2,MINLE2
      COMMON /SHAOPT/ WIRNRM,PADNRM,TRGNRM
C
      LOGICAL LTWDIG,LTPDIG,LTTDIG,LPRGEO,
     &        LWREDC,LTPCSA,LHISST,FTPC90,LRND32,
     &        REPIO,WEPIO,LDROP,LWRITE
C
      LOGICAL LTDIGT(3)
      EQUIVALENCE (LTWDIG,LTDIGT(1))
C
      REAL FACNRM(3)
      EQUIVALENCE (WIRNRM,FACNRM(1))
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
C  Set the analog signal time bin
C
      TPANBN = TPDGBN / FLOAT(NTBAPD)
C
C  Open the single-pulse bank
C
      CALL WBANK(IW,ITPULS,NTMXAN,*999)
C
C  Now we're ready to open reference banks.  Set the number of words
C  per channel to allow 1 word for the earliest time bin hit, one word
C  for the latest time bin hit, and one word for a pointer into the
C  signal bank for each division of the analog signal
C
      NWPCH = 2 + NTSCAN
C
      DO 3 KCHAN = 1,NCHAN
C
C  If this channel type is on, open the reference bank for it
C  nd = lhdr + max num of channels of this type * num words per channel
C
         IF ( .NOT. LTDIGT(KCHAN) ) GOTO 3
C
         NDREF = 2 + MAXNCH(KCHAN)*NWPCH
         CALL WBANK(IW,INDREF(KCHAN),NDREF,*999)
C
C  Fill the ref bank header.
C  + 1 = nrows = number of channels of this type
C  + 2 = ncols = num of words per channel
C
         IW(INDREF(KCHAN)+1) = MAXNCH(KCHAN)
         IW(INDREF(KCHAN)+2) = NWPCH
C
C  Next channel type
C
 3    CONTINUE
C
      RETURN
C
  999 WRITE(6,'(/'' +++TPINRF+++ Insufficient BOS array space!''/)')
      RETURN
C
      END
