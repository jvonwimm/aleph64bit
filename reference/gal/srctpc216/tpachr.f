      SUBROUTINE TPACHR(NEL,QTOT)
C-----------------------------------------------------------------------
C!  Generate the total charge produced during an avalanche; account for
C!  statistical fluctuations at wire amplification.
C
C  Called from:  TSAVAL
C  Calls:        RNDM,RANNOR
C
C  Input:  PASSED     --NEL, num of electrons producing the avlanche
C          /TPCONS/   --THETA, width parameter (see Lapique & Piuz,
C                              NIM 175 (1980) 297-318 )
C                     --ETHETA, exp(THETA)
C          /TPCOND/   --AMPLIT, avg. size of avalanche from 1 electron
C                     --NPOLYA, cutoff for Polya/Gaussian distribution
C  Output: PASSED     --QTOT total charge produced
C
C  A. Caldwell
C  M. Mermikides  1/9/86  Set limit to total charge to avoid numeric
C                         overflows.
C  D. Casper      8/10/92 Correct error in Polya width
C-----------------------------------------------------------------------
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
      DATA QMAX/1.E8/
C
      IF ( NEL .LE. NPOLYA ) THEN
C
C  Polya distribution
C
 1       ZZ = -ALOG(RNDM(F))
         ZZCH = ETHETA * (ZZ**THETA) * EXP(-THETA*ZZ)
C
         ZZ1 = RNDM(F)
         IF ( ZZCH .LT. ZZ1 ) GOTO 1
C
         QTOT = NEL*AMPLIT*ZZ
C
      ELSE
C
C  Gaussian distribution
C
         CALL RANNOR(FL,DUM)
C
         QZER0 = NEL*AMPLIT
         WIDTH = AMPLIT * SQRT(FLOAT(NEL)/(1.+THETA))
         QTOT  = QZER0 + FL*WIDTH
C
      ENDIF
C
C Set limit to total charge produced
      IF(QTOT.GT.QMAX) THEN
C        PRINT '('' +++TPACHR+++ Exceeded max charge, QTOT,NEL'',
C     &             G15.3,I10)',QTOT,NEL

         QTOT = QMAX
      ENDIF
      RETURN
C
      END
