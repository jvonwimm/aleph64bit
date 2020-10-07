      SUBROUTINE TSINIT
C--------------------------------------------------------------------
C!  Initialize geometry, I-O, histograms, and work banks
C
C  Called from:   TPMAIN (Main program)
C  Calls:         TSCDEF,TSRCON,TSCNST,TSCBOS,TPINRF,TPINEL,
C                 TOBKHS,TSAVCN,TPFILL,OPENDB
C
C  Inputs:   READ:  --Run conditions, unit 12
C  Outputs:  See individual subroutines
C
C  A. Caldwell, D. DeMille, M. Mermikides
C  Modifications
C     1. F. Ranjard  05 May 88 --Load TPC geometry using the TPCDES
C                                package from ALEPHLIB.
C     2. F. Ranjard  05 May 88 --Open database using OPENDB (ALEPHLIB)
C     3. P. Janot    06 May 88 --Do NOT load TPC geometry because
C                                this has to be done for each run.
C                                Now done in SUBROUTINE TOEVIN.
C     4. M. Rumpf    08 Nov 89
C -For Apollo version : provide 2 links in your home directory :
C   TRKFIL :to your 'Track Element Input File'
C   DIGFIL :to your 'Track Element Output File'
C  You will be prompted for card file pathname.
C--------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      LOGICAL LEX, OPENDB
      CHARACTER*4 TFORM
      DATA LBASE/4/,LINP/33/,ILNDB/1024/
C
C  Set up default condititions
C
      CALL TSCDEF
C
C  Read run cards to update conditions
C
      CALL TSRCON
C
C  Physical constants for TPC simulation
C
      CALL TSCNST
C
C Initialise BOS bank names, formats and default sizes
C
      CALL TSCBOS
C
C  Open Database file
C
      LEX = OPENDB (LBASE)
C
C  If we want to do electronics, initialize appropriate banks and
C  open the output file
C
      CALL TPINRF
C
      IF ( LTWDIG .OR. LTPDIG .OR. LTTDIG ) THEN
         CALL TPINEL
      ENDIF
C
C  Make histograms
C
      NDEBSM = NTPCDE+NTPCDD+NTPCDT+NTPCDA+NTPCDC+NTPCDR+NTPCDI
      IF(NDEBSM .GT. 0) CALL TOBKHS(0)
C
C  Save conditions in BOS bank (TSIM) and add to C list
C
      CALL TSAVCN(INTCHA('TPC '))
C
C  Read Landau Fluctuation and ExB effect constants for FAST
C  simulation
C
      IF(ILEVEL .EQ. 'FAST') THEN
         CALL TPFILL(IRET)
         IF(IRET.EQ.1) STOP 99
      ENDIF
      RETURN
      END
