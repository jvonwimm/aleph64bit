      SUBROUTINE TPSHAP
C-----------------------------------------------------------------------
C!  Calculate the response function of the TPC electronics
C!  and its derivative
C
C  Called from:  TPINEL
C  Calls:        WBANK, TPCFLT
C
C  Inputs:   TPCONS.INC:  --NTMXSH, the maximum number of bins for the
C                                   shaping signal
C                         --TMVPEL, the peak of the response function
C            TPCOND.INC:  --TPANBN, the length of the analog signal time
C                                   bin
C
C  Outputs:  TPCOND.INC:  --NLSHAP, the length of the shaping sign
C                         --NSHPOF, the offset of the first bin of
C                                   the shaping signal from time 0
C            BANKS:       --id ITSHAP, the shaping signal
C                           IW(ITSHAP+I) = Ith bin of shaping signal
C                         --id ITDSHP, the derivative of the shaping
C                                      signal
C                           IW(ITDSHP+I) = Ith bin of derivative
C
C  Described in ALEPH-TPC note #83-4 (27.5.83)
C  R. Richter-- 28.2.84 --Updated 11.1.85 D.D.
C
C  Modifications:
C
C     1.  22feb89 DFC -- Add in error message in case the call to WBANK
C                        gives an error return.
C-----------------------------------------------------------------------
C
      IMPLICIT COMPLEX(A)
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
      REAL AMPLIT
      DIMENSION DRC(2,13),DTINV(13),AZERO(10),APOLE(10)
C
C  ESGMN:  Minimum value of shaping signal to consider significant
C  VERST:   Internal factor
C
      DATA ESGMN,EDSMN/0.005,0.01/
      DATA VERST/3.D0/
C
C  DRC contains resistance and capacitance values for the various
C  components and sections of the TPC electronics chain.
C  Note that log tail comparator is not included.
C
C                RESIST.(OHM)    CAPACIT.(F)
      DATA DRC /     0. E0,        1.E-12,
     2               2.0E6,        1.E-12,
C
C  fake R&C values for preamp-, driver- and cable- risetime
C
     3              40.,           1.E -9,
     4               1.,           1.E -9,
     5              30.,           1.E -9,
C
C  end of cable
C
     6              95. E3,  100 000.E-12,
     7              4.77E3,       20.E-12,
     8              11. E3,       10.E-12,
     9               0.1E3,      330.E-12,
C
C  end of first integrator
C
     Z               5.6E3,  100 000.E-12,
     1               6.8E3,       22.E-12,
     2               5.6E3,       10.E-12,
     3               0.1E3,      560.E-12/
C
C  end of second integrator
C
C-----------------------------------------------------------------------
C  To avoid under-/over-flow problems the internal unit of
C  time is the micro-second.  The poles and zeros are then
C  divided by 10**6 and time is multiplied by 10**6
C-----------------------------------------------------------------------
C
C  Open BOS work banks for the shaping signal and its derivative
C
      CALL WBANK(IW,ITSHAP,NTMXSH,*999)
      CALL WBANK(IW,ITDSHP,NTMXSH,*999)
C
C  Compute the inverses of the RC time constants (time unit = micro-sec)
C  (Skip the first one since R = 0)
C
      DO 1 I = 2,13
 1       DTINV(I) = 1./ (DRC(1,I) * DRC(2,I) * 1.E6)
C
C  Set poles, zeros and constants for pre-amp, driver ,cable
C
      NZEROS = 0
      NPOLES = 4
C
C  I = 1 : signal behind pre-amp
C  I = 2 : signal behind pre-amp with risetime
C  I = 3 : signal behind cable driver
C  I = 4 : signal behind cable
C
      APOLE(1) = DTINV(2)
      DNORM = DRC(2,1) / DRC(2,2)
C
      DO 2 I = 2,NPOLES
         DNORM = DNORM * DTINV(I+1)
         IF ( I .EQ. 3 ) DNORM = DNORM * VERST
         APOLE(I) = DTINV(I+1)
 2    CONTINUE
C
C  Set poles, zeros, and constants for first integrator
C  formula for DNORM, TZ, APOLE are from RR electronics book P.74,80
C
      NZEROS = 1
      NPOLES = 7
      DRPRLL = DRC(1,6) * DRC(1,7) / (DRC(1,6) + DRC(1,7))
      DNORM  = DNORM * (DRC(1,8)/DRPRLL) * DTINV(8) * DTINV(9)
      DTZ     = DRC(2,7) * (DRC(1,6) + DRC(1,7)) * 1.E6
      AZERO(1)  = 1. / DTZ
      APOLE(5)  = DTINV(7)
      ADISCR = ( .25*DTINV(9) - DTINV(8) ) * DTINV(9)
      APOLE(6)  = .5*CMPLX(DTINV(9),0.) + CSQRT(ADISCR)
      APOLE(7)  = .5*CMPLX(DTINV(9),0.) - CSQRT(ADISCR)
C
C  Set poles, zeros, constants for second integrator
C
      NZEROS = 1
      NPOLES = 9
      DRPRLL = DRC(1,10)
C
      DNORM  = DNORM * (DRC(1,12) / DRPRLL) * DTINV(12) * DTINV(13)
      DTZ     = DRC(2,11)*(DRC(1,10)+DRC(1,11)) * 1.E6
      AZERO(2)  = 1. / DTZ
      ADISCR = ( .25*DTINV(13) - DTINV(12) ) * DTINV(13)
      APOLE(8)  = .5*CMPLX(DTINV(13),0.) + CSQRT(ADISCR)
      APOLE(9)  = .5*CMPLX(DTINV(13),0.) - CSQRT(ADISCR)
      APOLE(10) = DTINV(11)
C
C  Initialize TPCFLT
C
      CALL TPCFLT(1,NZEROS,AZERO,NPOLES,APOLE,DUM,DUM)
C
C  Step along in time to produce the circuit response signal
C  Note that this is the response to a 1V step at T=0.
C  DTIME goes in microsecs = nsec * 1E-3
C
      DO 3 I = 1, NTMXSH
         DTIME = FLOAT(I-1) * TPANBN * 1.E-3
         CALL TPCFLT(0,NZEROS,AZERO,NPOLES,APOLE,DTIME,DRESP)
         RW(ITSHAP+I) = DRESP * DNORM
 3    CONTINUE
C
C  Compute the derivative of the response signal; dt = TPANBN nanosecs
C  Make sure to take care of derivatives at the endpoints
C
      ADT = TPANBN*1.E-9
      NDRVMX = NTMXSH - 1
C
      DO 4 I = 2, NDRVMX
 4       RW(ITDSHP+I) = .5 * ( -RW(ITSHAP+I-1) + RW(ITSHAP+I+1) ) / ADT
C
      RW(ITDSHP+1) = ( RW(ITSHAP+2) - RW(ITSHAP+1) ) / ADT
      RW(ITDSHP+NTMXSH) = ( RW(ITSHAP+NTMXSH) - RW(ITSHAP+NTMXSH-1) ) /
     *                                       ADT
C
C  Now we have the full shaping signal (and its derivative) in terms of
C  the response to an input 1V step.  What we really want is to
C  normalize the signal so that we input a number of electrons and get
C  out the signal in millivolts.  To get this, we first find the
C  maximum of the signals, then include an experimental factor which
C  converts from # of electrons at input to millivolts at output
C
      ESGMX = 0.
      EDSMX = 0.
C
      DO 5 I = 1, NTMXSH
         IF ( ABS(RW(ITSHAP+I)) .GT. ESGMX ) ESGMX = ABS(RW(ITSHAP+I))
         IF ( ABS(RW(ITDSHP+I)) .GT. EDSMX ) EDSMX = ABS(RW(ITDSHP+I))
 5    CONTINUE
C
      DO 6 I = 1, NTMXSH
         RW(ITSHAP+I) = RW(ITSHAP+I) * TMVPEL / ESGMX
         RW(ITDSHP+I) = RW(ITDSHP+I) * TMVPEL / ESGMX
 6    CONTINUE
C
      EDSMX = EDSMX * TMVPEL / ESGMX
      ESGMX = TMVPEL
C
C  Now look for the range of significant values of the signals
C
      KBEGIN = 0
      IFIRST = 0
      ILAST = 0
C
      DO 7 I = 1, NTMXSH
C
         IF ( KBEGIN .EQ. 0 ) THEN
C
            IF (  (ABS(RW(ITSHAP+I))/ESGMX) .GT. ESGMN  .OR.
     *            (ABS(RW(ITDSHP+I))/EDSMX) .GT. EDSMN )    THEN
               KBEGIN = 1
               IFIRST = I
            ENDIF
C
         ELSE
C
            IF (  (ABS(RW(ITSHAP+I))/ESGMX) .LT. ESGMN  .AND.
     *            (ABS(RW(ITDSHP+I))/EDSMX) .LT. EDSMN )      THEN
               ILAST = I - 1
               GOTO 8
            ENDIF
C
         ENDIF
C
 7    CONTINUE
C
 8    IF ( ILAST .EQ. 0 ) ILAST = NTMXSH
      NLSHAP = ILAST - IFIRST + 1
      NSHPOF = IFIRST - 1
C
C  Now close off the work banks
C
      CALL WBANK(IW,ITSHAP,NLSHAP,*999)
      CALL WBANK(IW,ITDSHP,NLSHAP,*999)
C
C  Print out shaping and derivative signals
C
      IF (LTPCSA) THEN
         WRITE (6,900) TPANBN,(RW(ITSHAP+N),N=1,NLSHAP)
  900    FORMAT(//,' +++TPSHAP+++  Shaping/amplification response fn.',
     &          ' in',F8.1,' nsec bins', //,(5(2X,E11.3),/) )
         WRITE (6,901) (RW(ITDSHP+N),N=1,NLSHAP)
  901    FORMAT(//,' Derivative of response function',
     &          //,(5(2X,E11.3),/) )
         WRITE (6,902)
  902    FORMAT(//)
      ENDIF
C  Done.
C
      RETURN
C
  999 WRITE(6,'(/'' +++TPSHAP+++ Insufficient BOS array space!''/)')
      RETURN
C
      END
