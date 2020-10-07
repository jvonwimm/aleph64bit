      SUBROUTINE TSSHAM(ITYPE,KCHAN,JCHN,IPLS1,NLPLS,IADC1,NLADC)
C--------------------------------------------------------------------
C!  Here we simulate the shaping and amplification of raw input signals
C!  as well as the noise inherent in the amplification.
C
C  Called from:  TSRESP
C  Calls:        TPCDET
C
C  Inputs:   PASSED:      --ITYPE,  sector type
C                         --KCHAN,  channel type
C                         --JCHN,   channel number
C                         --IPLS1,  first bin of raw pulse
C                         --NLPLS,  number of bins in raw pulse
C            TPCONS.INC:  --TPCFET, FET capacitance, preamp
C                         --TCFEED, feedback capacitance, preamp
C                         --TPRSER, series resistance, preamp
C                         --NTMXDI, max num of bins in shaped signal
C                         --NTBAPD, number of analog signal bins per
C                                   digitized signal bin
C            TPCBOS.INC:  --NLSHAP, length of shaping signal
C                         --NSHPOF, offset of shaping signal from zero
C            BANKS:       --id ITPULS, the input raw pulse
C                         --id ITPNOI, parallel noise spectrum
C                         --id ITSNOI, series noise spectrum
C                         --id ITSHAP, shaping signal
C
C  Outputs:  BANKS:       --id ITMADC, the shaped and amplified signal
C                                      (created in TPINEL)
C                           IW(ITMADC+I) = Ith bin of signal
C
C  R. Richter 29.11.84; D. DeMille
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
C
      LOGICAL LDBS2
      DATA ICALL/0/
C
      ICALL = ICALL + 1
C
C  Debug levels
C
      LDBS2 = ( NTPCSA .GE. 2 .AND. ICALL .LE. NCALSA )
C
C  Zero-index of input pulse
C
      IDPLS = ITPULS + IPLS1 - 1
C
C  Choose random addresses for beginning to look at the noise signals;
C  one each for series and parallel noise.
C  LNOIS is chosen to ensure that we remain in the noise signal
C
      LNOIS = IW(ITPNOI) - (NLPLS + NLSHAP + 1)
      IDPNO = ITPNOI + INT(LNOIS*RNDM(PN)) + 1
      IDSNO = ITSNOI + INT(LNOIS*RNDM(SN)) + 1
C
C  We must add a scaling factor to the series noise to account for
C  differing detector channel capacitances; see ALEPH-TPC note 84-89
C  and routine TPNOIS
C
      CDET = TPCDET(ITYPE,KCHAN,JCHN)
      CTOT = TPCFET + TCFEED + CDET
      SERFC = TPRSER * CTOT
C
C  Since we are usually compressing the analog signal into fewer (and
C  thus longer) time bins for digitizing, we need to decide how to make
C  the correspondence.  We decide to say that the shaped signal begins
C  in bucket IADC1 if the signal begins before the halfway point of
C  that bucket.  First we find the bucket in the shaped signal time
C  scale in which the raw analog signal actually begins:
C
      IADC1 = (IPLS1-1)/NTBAPD + 1
C
C  Now find the position of the beginning of the raw signal within
C  this bucket; if this position is at least halfway along the length of
C  the bucket, begin filling the shaped signal (and sampling the raw
C  signal) only in the next bucket.
C
      INBK = IPLS1 - (IADC1-1)*NTBAPD
      IBUK1 = (NTBAPD+1)/2 - INBK + 1
C
      IF ( INBK .GT. (NTBAPD+1)/2 )  THEN
         IADC1 = IADC1 + 1
         IBUK1 = IBUK1 + NTBAPD
      ENDIF
C
C  The shaped signal is the result of the convolution integral
C
C                     |\\ +inf
C                     |
C  shaped_signal(T) = |    input_pulse(T-T')*response(T')*dT'
C                     |
C                    \\| 0
C
C  IRANGE limits T so that it never exceeds the time range covered by
C  the raw signal nor the significant range determined by the end of the
C  raw signal plus the end of the significant part of the response funct
C
      IRANG = MIN0( NTMXAN, NLPLS+NSHPOF+NLSHAP-1 )
      NLADC = ( IRANG - IBUK1 ) / NTBAPD
C
C  If drift time is near upper limit, JBUCK can exceed the predefined
C  length of the ITMADC bank.  Correct for this below.  DFC 15-OCT-88.
C
      IF ( (IADC1 + NLADC) .GT. NTMXDI ) THEN
         NLADC = NTMXDI - IADC1
         IRANG = NTBAPD*NLADC + IBUK1
      ENDIF
C
      IBUCK = 0
C
C  Loop over bins of input pulse to be sampled
C
      DO 13 IT = IBUK1, IRANG, NTBAPD
C
C  IBUCK is the number of time steps in the shaped-signal binning
C
         IBUCK = IBUCK + 1
         JBUCK = ITMADC + IADC1 + IBUCK
C
C  Here's the integration; IRSMX and IRSMN limit the range of the
C  double loop so that we stay within the limits of the pulse and of
C  the shaping signal
C
         IRSMX = MIN0( NSHPOF+NLSHAP, IT-1 )
         IRSMN = MAX0( NSHPOF, IT - NLPLS )
C
         RW(JBUCK) = 0.
         DO 12 ITPRM = IRSMN, IRSMX
 12        RW(JBUCK) = RW(JBUCK) +
     *                 IW(IDPLS+IT-ITPRM)*RW(ITSHAP+1+ITPRM-NSHPOF)
C
C  Add in the noise.  Note that we add in noise over the length of one
C  analog signal bin, not over the length of one digitized signal bin;
C  this is because the rest of the electronics chain will see
C  instantaneous noise, not noise over a finite range, and one analog
C  signal bin is as instantaneous as we can get.
C
         RW(JBUCK) = RW(JBUCK) + RW(IDPNO+IBUCK) +
     *                           SERFC*RW(IDSNO+IBUCK)
C
C  Include normalization factor for different amplifications on
C  the different channel types.  This should be needed only for wires,
C  since the electronics response and its normalization are based on
C  the design and performance of the pad electronics.
C
         RW(JBUCK) = RW(JBUCK)/FACNRM(KCHAN)
C
C  Next bin of raw analog signal
C
 13   CONTINUE
C
      RETURN
      END
