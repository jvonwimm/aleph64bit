      SUBROUTINE TSAVAL(NEL,TTOT,IRET)
C-----------------------------------------------------------------------
C!  Subroutine to simulate the avalanche of one (or several) electrons
C!  at a TPC anode wire.
C
C  Called from:  TSTEER
C  Calls:        TPACHR
C
C  Inputs:   PASSED:      --NEL,    number of electrons forming the
C                                   avalanche
C                         --TTOT,   the time of the avalanche
C            /TPCOND/     --TPANBN, the length of the analog signal
C                                   time bin
C            /TPCONS/     --NTMXAN, the largest allowed time bin
C
C  Outputs:  PASSED:      --IRET,   return code to indicate if
C                                   avalanche is in a legal time bin
C            /CHANNL/     --NAVELE, the number of electrons released by
C                                   the avalanche
C                         --IBIN1,  the time bin of the avalanche
C A. Caldwell, D. DeMille
C-----------------------------------------------------------------------
C
C  CHANNL carries the analog signals on all channels due to one
C  wire avalanche
C
C  Wire avalanche information
C  NAVELE     -- The charge from an avalanche
C  IBIN1  -- The first bin to get charge
C
      COMMON / AVALAN / NAVELE,IBIN1
C
C  Long pad coupling information
C  MPPUL   -- The array containing the coupled avalanches
C  NAMP   -- Pad 'name' generating this pulse
C            = (pad row-1)*150 + pad number
C
      PARAMETER (MXPHIT=15)
      COMMON / PADPUL / MPPUL(MXPHIT),NAMP(MXPHIT)
C
C  Trigger pad coupling information
C  MTPUL   -- The array containing the coupled avalanches
C  NAMT   -- Pad 'name' generating this pulse
C            = (tpad row# - 1)*(max # tpads/row) + tpad number
C
      PARAMETER (MXTHIT=2)
      COMMON / TRGPUL / MTPUL(MXTHIT),NAMT(MXTHIT)
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
      COMMON / HISCOM / IHDEDX,IHTRAN,IHAVAL,IHCOUP,IHTRCP,IHBOS,IHTOT
C
      LOGICAL LDBA1,LDBA3
C
      DATA ICALL/0/
C
      ICALL = ICALL + 1
C
C  Set the return code.  IRET = 1 for invalid bin
C
      IRET = 0
C
C  Debug levels
C
      LDBA1 = ( NTPCDA .GE. 1 .AND. ICALL .LE. NCALDA )
      LDBA3 = ( NTPCDA .GE. 3 .AND. ICALL .LE. NCALDA )
C
C  Get the total charge produced.
C
      CALL TPACHR(NEL,QTOT)
C
      IF ( LDBA3 ) WRITE (6,100) NEL,QTOT
      IF ( LDBA1 ) THEN
           IF ( QTOT .GT. 0. ) THEN
              CALL HF1(IHAVAL+1,ALOG10(QTOT),1.)
           ELSE
              WRITE(6,201) QTOT,NEL
           ENDIF
      ENDIF
C
C  Determine the time bin affected and make sure it is a legal value
C
      IBIN1 = INT( TTOT / TPANBN ) + 1
      IF ( IBIN1 .LE. 0 .OR. IBIN1 .GE. NTMXAN ) THEN
         IRET = 1
         IF ( LDBA1 ) WRITE(6,202) IBIN1
         RETURN
      ENDIF
C
C  Find the number of electrons in the avalanche and fill the common
C  block with the value
C
      NAVELE = INT(QTOT)
      IF ( LDBA3 ) WRITE (6,101) IBIN1,NAVELE
C
      RETURN
C_______________________________________________________________________
C
 100  FORMAT(//,10X,'    START AVALANCHE PROCESS ',
     *        /,' NUMBER OF ELECTRONS IN AVALANCHE :',I4,
     *        /,' TOTAL ELECTRONS DEPOSITED        :',E12.5)
 101  FORMAT(/ ,' FIRST TIME BIN AFFECTED          :',I5,
     *       / ,' ELECTRONS IN BIN                 :',I12)
C
 201  FORMAT(/ ,' +++TSAVAL+++ 0 CHARGE',F10.5,' NO. OF ELECTRONS,',
     *         I12)
 202  FORMAT(/ ,' +++TSAVAL+++ ILLEGAL BIN VALUE:',I12)
C
      END
