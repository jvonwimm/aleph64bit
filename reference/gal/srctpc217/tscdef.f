      SUBROUTINE TSCDEF
C--------------------------------------------------------------------
C! Define default TPCSIM conditions.
C  Called from:  TSINIT
C
C M. Mermikides  5/11/87
C modified by:
C        1. F.Ranjard  28 Mar 89 -- reset 3 random number seeds to 0
C        2. F.Ranjard  10 Jul 92 -- read TSIM bank from DB
C--------------------------------------------------------------------
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
      PARAMETER(JTSITV=1,JTSIMX=2,JTSIIT=3,JTSINS=4,JTSINF=5,JTSINP=6,
     +          JTSINC=7,JTSILT=8,JTSINR=9,JTSINO=10,JTSIMI=11,
     +          JTSILH=12,JTSINE=13,JTSINT=14,JTSIMN=15,JTSINW=16,
     +          JTSIDV=17,JTSISA=18,JTSISR=19,JTSITA=20,JTSITD=21,
     +          JTSIAM=22,JTSICU=23,JTSIFF=24,JTSISW=25,JTSISH=26,
     +          JTSIHX=27,JTSIEF=28,JTSISI=29,JTSISC=30,JTSIRX=31,
     +          JTSITS=32,JTSIPE=33,JTSISP=34,JTSISG=35,JTSISD=36,
     +          JTSIWN=37,JTSIPN=38,JTSITN=39,JTSICF=40,JTSIWS=41,
     +          JTSITZ=42,JTSIIL=43,JTSIIG=44,LTSIMA=44)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER GTSTUP,AGETDB
C ------------------------------------------------------
C Debug levels and no. of calls
      NTPCDD = 0
      NCALDD = 5000
      NTPCDT = 0
      NCALDT = 5000
      NTPCDA = 0
      NCALDA = 5000
      NTPCDC = 0
      NCALDC = 5000
      NTPCDS = 0
      NCALDS = 5000
      NTPCDI = 0
      NCALDI = 5000
      NTPCDE = 0
      NCALDE = 100
      NTPCSA = 0
      NCALSA = 5000
      NTPCDR = 0
      NCALDR = 5000
      LTDEBU = .FALSE.
C Run number, first event and number of events
      NUMRUN = 1
      NFEVNT = 1
      MXEVNT = 1000000
C Processing level (default full TPCSIM)
      ILEVEL = 'FAST'
C EPIO read flag (default read is native (FORT))
      REPIO = .FALSE.
C EPIO write flag (default write is native (FORT))
      WEPIO = .FALSE.
C Use machine-independent random # generator
      LRND32 = .FALSE.
C Drop track element BANKs
      LDROP = .FALSE.
C Printout of TPC geometry
      LPRGEO = .FALSE.
C Circuit response documentation
      LTPCSA = .FALSE.
C File names
      TRKFIL = 'Input_file'
      DIGFIL = 'Output_file'
C Random number seeds
      INSEED(1) = 0
      INSEED(2) = 0
      INSEED(3) = 0
C Bucket length (nsec)
      TPDGBN = 100.0
C Transport
      MXTRAN = 10
      DRFVEL = .00505
C Diffusion
      SIGMA = 0.06000
      SIGTR = 0.00831
C Gating scheme
      ITRCON = 2
C Avalanche
      NPOLYA = 10
      AMPLIT = 1.0E4
C Pad coupling
      CUTOFF = 0.0005
      NCPAD = 2
      EFFCP = 0.21600
      SIGW =  0.37000
      SIGH =  0.40000
      HAXCUT = 1.07
      TREFCP = 0.21600
      TCSCUT = 0.00050
      SIGR =   0.32000
      SIGARC = 0.32000
      RAXCUT = 0.01500
C Pedestal(mV),variation,gain variation
      PEDDEF = 0.00000
      SPEDEF = 1.0
      SGADEF = 0.02
      SDIDEF = 0.0
C Normalisation ("gain knob")
      WIRNRM = .8240
      PADNRM = .8820
      TRGNRM = 1.
C Saving of digital signals
      LTWDIG = .TRUE.
      LTPDIG = .TRUE.
      LTTDIG = .FALSE.
C Zero suppression (WIRES and PADS)
      LTHRSH =  6
      NPRESP =  2
      NPOSTS =  2
      MINLEN =  3
      LTHRS2 =  4
      NPRES2 =  2
      NPOST2 =  2
      MINLE2 =  2
C Magnetic field
      CFIELD = 15.0
C Wire data reduction
      LWREDC = .TRUE.
      WPSCAL = 0.5
      NWSMAX = 16
      THRZTW = 0.05
C
C - get the TPC setup code
      ISTP = GTSTUP ('TP',1)
C - get TSIM  bank from data base depending on the setup code
      LBAS = JUNIDB(0)
      JTSIM = MDARD (IW,LBAS,'TSIM',ISTP)
      CALL BKFMT('TSIM','18I,26F,2A')
C
C - If TSIM exists update parameters with TSIM values
      IF (JTSIM.EQ.0) THEN
         WRITE (IW(6),*) ' TPCSIM/TSCDEF no TSIM ,NR= ',ISTP,
     &                   ' found on DB - defaults will be used.'
      ELSE
C        replace defaults by TSIM values
         KTSIM = JTSIM+LMHLEN
         MXTRAN = IW(KTSIM+JTSIMX)
         ITRCON = IW(KTSIM+JTSIIT)
         NPOLYA = IW(KTSIM+JTSINP)
         NCPAD  = IW(KTSIM+JTSINC)
         LTHRSH = IW(KTSIM+JTSILT)
         NPRESP = IW(KTSIM+JTSINR)
         NPOSTS = IW(KTSIM+JTSINO)
         MINLEN = IW(KTSIM+JTSIMI)
         LTHRS2 = IW(KTSIM+JTSILH)
         NPRES2 = IW(KTSIM+JTSINE)
         NPOST2 = IW(KTSIM+JTSINT)
         MINLE2 = IW(KTSIM+JTSIMN)
         NWSMAX = IW(KTSIM+JTSINW)
         DRFVEL = RW(KTSIM+JTSIDV)
         SIGMA  = RW(KTSIM+JTSISA)
         SIGTR  = RW(KTSIM+JTSISR)
         TPDGBN = RW(KTSIM+JTSITD)
         AMPLIT = RW(KTSIM+JTSIAM)
         CUTOFF = RW(KTSIM+JTSICU)
         EFFCP  = RW(KTSIM+JTSIFF)
         SIGW   = RW(KTSIM+JTSISW)
         SIGH   = RW(KTSIM+JTSISH)
         HAXCUT = RW(KTSIM+JTSIHX)
         TREFCP = RW(KTSIM+JTSIEF)
         SIGR   = RW(KTSIM+JTSISI)
         SIGARC = RW(KTSIM+JTSISC)
         RAXCUT = RW(KTSIM+JTSIRX)
         TCSCUT = RW(KTSIM+JTSITS)
         PEDDEF = RW(KTSIM+JTSIPE)
         SPEDEF = RW(KTSIM+JTSISP)
         SGADEF = RW(KTSIM+JTSISG)
         SDIDEF = RW(KTSIM+JTSISD)
         WIRNRM = RW(KTSIM+JTSIWN)
         PADNRM = RW(KTSIM+JTSIPN)
         TRGNRM = RW(KTSIM+JTSITN)
         CFIELD = RW(KTSIM+JTSICF)
         WPSCAL = RW(KTSIM+JTSIWS)
         THRZTW = RW(KTSIM+JTSITZ)
         CALL BDROP (IW,'TSIM')
      ENDIF
C
C - set derived constants
      BCFGEV = CLGHT*CFIELD*1.E-5
      BCFMEV = BCFGEV*1000.
      RETURN
      END
