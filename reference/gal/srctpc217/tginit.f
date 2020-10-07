      SUBROUTINE TGINIT (GFIEL,ICTP)
C--------------------------------------------------------------------
C!  Initialize geometry, I-O, histograms, and work banks
C
C  Called from:  GALEPH/TPIRUN
C  Calls:        TSCDEF, TSCNST, TGCBOS, TSRCON
C                TPINEL, TOBKHS, TSAVCN, TPFILL
C
C  Input: GFIEL / R  = GALEPH magnetic field
C         ICTP  / I  = GALEPH run conditions
C
C  A. Caldwell, D. DeMille, M. Mermikides
C  Modifications
C     1. D. Cowen    24 Jun 88 --This routine originally was TSINIT,
C                                modified here for use with GALEPH.
C     2. F.Ranjard   28 Mar 89 --add arguments in the calling sequence
C                                and suppress references to GALEPH *CD
C     3. D.Casper    22 Nov 93 --ensure that Gaussian lookup table is
C                                symmetric with respect to zero.
C--------------------------------------------------------------------
      INTEGER ICTP(*)
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /TINAVA/ GRANNO(2000)
C
C TPCSIM version number
C
      CALL TPCVER(VERS)
      WRITE(IW(6),10) VERS
C
C  Set default indices for running TPCSIM from GALEPH
C
C   Create TPTE and TPHT bank
      ICTP(4) = 1
C   Get rid of TPCO, TCRL and TPCH
      ICTP(6) = 0
C
C  Set up default condititions
C
      CALL TSCDEF
C
C  Assign to CFIELD (in Comdeck TPCOND) the value GFIEL
C
      CFIELD = GFIEL
      BCFGEV = CLGHT*CFIELD*1.E-5
      BCFMEV = BCFGEV*1000.
C
C  Physical constants for TPC simulation
C
      CALL TSCNST
C
C Initialise BOS bank names, formats and default sizes
C
      CALL TGCBOS
C
C  Calculate parameters for breaking track elements at sector
C  boundaries
C
      CALL TPBGEO
C
C  If we want to do electronics, initialize appropriate banks and
C  open the output file
C
      CALL TPINRF
      CALL TPINEL
C
C  Make histograms
C
C     CALL TOBKHS(1,0)
C
C  Save conditions in BOS bank (TSIM,NR=0) and add to C list
C
      CALL TSAVCN(INTCHA('GAL '))
      CALL BLIST(IW,'C+','TSIM')
C   print TSIM written on output
      IF (ICTP(5).GT.0) THEN
         CALL PRTABL('TSIM',0)
      ENDIF
C
C  Read Landau Fluctuation and ExB effect constants for TPCSIM
C
      CALL TPFILL(IRET)
      IF (IRET .EQ. 1) STOP 99
C
C  Initialize avalanche process
C
      N = 0
      DO I = 1, 500
         CALL RANNOR(X1,X2)
         GRANNO(N+1) = X1
         GRANNO(N+2) = X2
         GRANNO(N+3) = -X1
         GRANNO(N+4) = -X2
         N = N+4
      ENDDO

C
10    FORMAT(//9X,' TPC digitizations will be done with',
     &         1X,'TPCSIM version ',F4.2/)
      RETURN
      END
