      SUBROUTINE TPNOIS
C-----------------------------------------------------------------------
C!  Produce two noise spectra: one resulting from PARALLEL noise,
C!  the other from SERIES noise.
C
C  Called from:  TPINEL
C  Calls:        WBANK, NORMCO
C
C  Inputs:  TPCONS.INC:  --TPRSER, serial resistance in preamp
C                        --TPRPAR, parallel resistance in preamp
C                        --NTMXNO, the maximum number of bins to
C                                  be put in the noise signal
C           TPCOND.INC:  --TPANBN, the analog signal time bin
C                        --NLSHAP, the length of the shaping signal
C           BANKS:       --work bank id ITSHAP, containing the
C                          shaping signal
C                        --work bank id ITDSHP, containing the
C                          derivative of the shaping signal
C
C  Outputs:  BANKS:      --work bank id ITPNOI, containing a noise
C                          spectrum arising from parallel noise
C                          convoluted with the shaping signal
C                          IW(ITPNOI+I) = parallel noise in Ith bin
C                        --work bank id ITSNOI, containing a noise
C                          spectrun arising from series noise
C                          convoluted with the derivative of the shaping
C                          signal
C
C  Note on weight due to Thevenin equivalence for series noise:
C  The series noise spectrum should include a mulplicative weighting
C  factor RS*CTOT.  However, we wish to use this noise
C  spectrum for different detector elements with different capacitances.
C  Thus, we here simply leave out this weighting factor and include it
C  when we actually need to add the noise to a signal.
C
C  M. Takashima     D. DeMille
C     16.9.84          13.1.85
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
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER(CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
C  Additional constants for TPCSIM
C  Units -- Mev,Joules,deg Kelvin,Coulombs
C
      REAL ELMASS,CROOT2,CKBOLT,CROOMT,ECHARG
      PARAMETER (ELMASS = 0.511)
      PARAMETER (CROOT2 = 1.41421356)
      PARAMETER (CKBOLT = 1.380662E-23)
      PARAMETER (CROOMT = 300.)
      PARAMETER (ECHARG = 1.602189E-19)
C
C  Open a BOS work bank for each type of noise.
C
      CALL WBANK(IW,ITPNOI,NTMXNO,*999)
      CALL WBANK(IW,ITSNOI,NTMXNO,*999)
C
C  Calculate number of RMS electrons in one second
C
      PELPS = ( 2.D0 * CKBOLT * CROOMT ) / ( TPRSER * (ECHARG**2) )
      SELPS = ( 2.D0 * CKBOLT * CROOMT ) / ( TPRPAR * (ECHARG**2) )
C
C  We will randomize the distributions according to a sigma
C  depending on avg. number of electrons per bin.
C
      PELPB = PELPS * ( TPANBN * 1.E-9 )
      SELPB = SELPS * ( TPANBN * 1.E-9 )
C
      SIGP = SQRT(SELPB)
      SIGS = SQRT(PELPB)
C
C  Now we're ready to fill the noise arrays.  Note that we subtract
C  out the DC levels and include only the variation around them.
C
      DO 1 I = 1,NTMXNO
         CALL RANNOR(RN1,RN2)
         RW(ITPNOI+I) = SIGP*RN1
         RW(ITSNOI+I) = SIGS*RN2
    1 CONTINUE
C
C  Now fold the parallel noise with the input response function and the
C  series noise with the derivative of the irf.  Note that,
C  because we need the spectrum before a point to get the result of
C  the convolution integral at that point, we must abandon the first
C  NLSHAP = length of shaping signal points of the noise spectrum.
C  This is taken care of as we go.
C
      IFIN = NTMXNO - NLSHAP
C
      DO 3 INOI = 1,IFIN
C
          SUMP = 0.
          SUMS = 0.
          ITOP = INOI + NLSHAP
C
          DO 2 ISHP = 1,NLSHAP
             SUMP = SUMP + RW(ITPNOI+ITOP-ISHP)*RW(ITSHAP+ISHP)
             SUMS = SUMS + RW(ITSNOI+ITOP-ISHP)*RW(ITDSHP+ISHP)
 2        CONTINUE
C
          RW(ITPNOI+INOI) = SUMP
          RW(ITSNOI+INOI) = SUMS
C
 3    CONTINUE
C
C  Let go of the garbage at the end of the BOS banks
C
      CALL WBANK(IW,ITPNOI,IFIN,*999)
      CALL WBANK(IW,ITSNOI,IFIN,*999)
C
      RETURN
C
  999 WRITE(6,'(/'' +++TPNOIS+++ Insufficient BOS array space!''/)')
      RETURN
C
      END
