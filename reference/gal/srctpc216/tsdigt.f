      SUBROUTINE TSDIGT(KCHAN,IADC1,NLADC)
C--------------------------------------------------------------------
C!  Digitization of the analog signal
C
C  Called from:  TSRESP
C  Calls:        RANNOR, RNDM
C
C  Inputs:   PASSED:      --IADC1, the first bin of the signal
C                         --NLADC, the number of bins in the signal
C                         --KCHAN, channel type 1=wire,2=pad,3=trig
C            BANKS:       --work bank id ITMADC, containing the shaped
C                           signal
C            TPCONS.INC:  --TSIGMX, max. value of the shaping signal
C                         --NTPBIT, num. of bits in the FADC
C            TPCOND.INC:  --SDIDEF, differential nonlinearity level
C                         --SGADEF, sigma for gain variation
C                         --SPEDEF, sigma for pedestal variation
C                         --PEDDEF, pedestal
C  Outputs:  BANKS:       --id ITMADC, the digitized signal;
C                           IW(ITMADC+I) = signal in Ith bin
C
C  Written by A. Peisert
C  Version 1.0  12.12.84
C  Version 1.1   1.11.85  D.D.
C
C  Modifications:
C
C  1. D. Casper 12 Oct 92 - alter treatment of pedestals, and diff.
C     non-linearity.  Make variation for pads half that for wires.
C
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
C
      LOGICAL LDBI2,LDBI3,LGPVR,LDFNL
C
      DATA ICALL/0/
C
C  Debug levels
C
      ICALL = ICALL + 1
      LDBI2 = ( NTPCDI .GE. 2 .AND. ICALL .LE. NCALDI )
      LDBI3 = ( NTPCDI .GE. 3 .AND. ICALL .LE. NCALDI )
C
C  If this is the first call, set up constants
C
      IF ( ICALL .EQ. 1 ) THEN
C
         MAXDIG = 2**NTPBIT - 1
         SDIFF = SDIDEF * TSIGMX / FLOAT(MAXDIG)
C
         LGPVR = ( SGADEF .NE. 0. .OR. SPEDEF .NE. 0. )
         LDFNL = ( SDIDEF .NE. 0. )
C
         IF ( LDBI2 ) WRITE(6,104) PEDDEF,SPEDEF,100.*SGADEF,SDIDEF
C
      ENDIF
C
C  Now we need to add the pedestal, as well as the gain variation,
C  pedestal variation, and differential nonlinearity to the shaped
C  signal.  The gain and pedestal variation should be the same
C  throughout each pulse, so get random numbers for these variations on
C  this pulse now.
C
      IF ( LGPVR ) THEN
         CALL RANNOR(RGAVR,RPDVR)
         IF(KCHAN.EQ.2)THEN
             RGAVR = RGAVR * 0.5
             RPDVR = RPDVR * 0.5
         ENDIF
      ELSE
         RGAVR = 0.
         RPDVR = 0.
      ENDIF
      IF( LDFNL ) THEN
          IF(KCHAN.EQ.2)THEN
              RDIFNL = SDIFF * 0.5
          ELSE
              RDIFNL = SDIFF
          ENDIF
      ELSE
           RDIFNL = 0.
      ENDIF
C
      DO 11 I = 1,NLADC
C
         IBK = IADC1 + I
C
C  Include gain variation
C
         RW(ITMADC+IBK) = RW(ITMADC+IBK) * (1. + SGADEF*RGAVR)
C
C  Add pedestal and pedestal variation
C
         RW(ITMADC+IBK) = RW(ITMADC+IBK) + PEDDEF
         RW(ITMADC+IBK) = RW(ITMADC+IBK) + RPDVR*SPEDEF
C
C  Differential nonlinearity; this changes with every time bucket
C
         IF ( LDFNL ) RW(ITMADC+IBK) =
     1    RW(ITMADC+IBK) + (RNDM(I) - 0.5) * RDIFNL * RW(ITMADC+IBK)
C
C  Now do the digitization using the calculated parameters and
C  keeping within range.
C
         IW(ITMADC+IBK) =  INT( MAXDIG * RW(ITMADC+IBK) / TSIGMX )
         IF ( IW(ITMADC+IBK) .LT. 0 )      IW(ITMADC+IBK) = 0
         IF ( IW(ITMADC+IBK) .GT. MAXDIG ) IW(ITMADC+IBK) = MAXDIG
C
 11   CONTINUE
C
      RETURN
C_______________________________________________________________________
C
 104  FORMAT(/,' +++TSDIGT+++     ADC analog pedestal = ',F6.3,
     &        /'                  VOLTS with sigma    = ',F6.3,
     &        /'                  GAIN variation      = ',F6.3,' %',
     &        /'           Diff. nonlinearity of FADC = ',F5.2,
     &                                                  ' LSB',//)
C
      END
