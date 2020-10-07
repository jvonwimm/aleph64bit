      SUBROUTINE TSFINI
C-----------------------------------------------------------------------
C!  Close files, flush buffers, output histograms, drop work banks
C
C  Called from: TSMAIN (main program)
C  Calls:       WDROP, BWRITE, BOSTA, HISTDO, HSTORE
C
C  Inputs:   /TPCOND/  --Condition flags
C            /TPCBOS/  --work bank id's
C
C  Outputs:  WRITE:       --BOS buffer, unit 25
C                         --Histograms, units 6 and 15
C  D. DeMille
C  D. Cowen   12 Feb 88  --Use BWRITE instead of BOSWR so we can
C                          specify EPIO vs. FORT (native) formats.
C                          An earlier BUNIT call sets these formats.
C
C                        --Don't do OPENs or CLOSEs on units 25 and
C                          33 if using EPIO.
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
      COMMON /AVLNCH/ NPOLYA,AMPLIT,GRANNO(1000)
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
C
C  FASTER : variables used in fast simulation
C
      COMMON / LANDAU / NITLAN,NITIND,INTWRD
      COMMON / EXBEFF / XPROP,TTTT,XXXX(1001),XSHFT(50)
      COMMON / EVT / IEVNT,ISECT
      COMMON / T3TR / JSEGT,NSEGT,ITYPE,WIRRAD(4),WIRPHI(4)
     &               ,AVTIM(4),NELE(4),NCL,SIGT(4)
      COMMON / TPSG / CC(3),XX(3),TOTDIS,XLEN,ISTY,IE
      COMMON / XBIN / IBIN(4),NAVBIN(4),NB
      DIMENSION XEX(4,4)
      PARAMETER (SQ2=1.4142136,SQ3=1.7320508)
C
C  Close track element file, but only if we're not using EPIO
C
C
C  Drop Landau Fluctuation Banks
C
       CALL BDROP(IW,'TLAN')
       CALL BDROP(IW,'TIND')
      CALL WDROP(IW,INTWRD)
C
C  Drop analog signal reference banks and single-pulse bank
C
      DO 1 KCHAN = 1, NCHAN
         IF ( INDREF(KCHAN) .NE. 0 ) CALL WDROP(IW,INDREF(KCHAN))
    1 CONTINUE
         CALL WDROP(IW,ITPULS)
C
C  If we did electronics simulation, drop banks used for it, flush
C  the BOS buffer for the digitizations file, and close the file if
C  we're not writing in EPIO format.
C
      IF ( LTWDIG .OR. LTPDIG .OR. LTTDIG ) THEN
C
         CALL WDROP(IW,ITSNOI)
         CALL WDROP(IW,ITPNOI)
         CALL WDROP(IW,ITSHAP)
         CALL WDROP(IW,ITMADC)
C
         IF(LWRITE) CALL BWRITE(IW,25,'0')
C
      ENDIF
C
C  Output final status of BOS
C
      CALL BOSTA
C
C  Write out histograms
C
      NDEBSM = NTPCDE+NTPCDD+NTPCDT+NTPCDA+NTPCDC+NTPCDR+NTPCDI
       IF (NDEBSM .GT. 0) CALL HISTDO
C
      RETURN
      END
