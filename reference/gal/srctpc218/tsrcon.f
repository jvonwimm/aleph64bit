      SUBROUTINE TSRCON
C-----------------------------------------------------------------
C! Read BOS free format "cards" and fill run conditions
C  M. Mermikides 17 Dec 1987
C  D. Cowen  13 Feb 1988  --Correct error in OPTI write (tried
C                           to write CHAINT()); add EPIO read/
C                           write flags INEPIO/OUTEPIO, respectively.
C  D. Cowen  09 Mar 1988  --Add FAST/FULL option so user can choose
C                           Janot's fast TPCSIM or regular TPCSIM.
C  P. Janot  26 Mar 1988  --Add option to drop the GALEPH track
C                           element BOS_BANKs (TPTE,TTHT,TTHE,TPHE
C                           and TPHT).
C  D. Cowen  21 Oct 1988  --Add additional DEBU option TDBG to
C                           call TDEBUG from standalone.
C  P. Janot  11 oct 1988  --Add an option to enable Trigger pad
C                           simulation
C  F.Ranjard 28 Mar 1989  --read 1 to 3 random number seeds
C                           RNDM   : 1 seed
C                           RANECU : 2 seeds
C                           RANMAR : 1st initialisation
C                                    = 2 lab seeds and 3rd = -1
C                                    resart on a given event
C                                    = 3 seeds
C-----------------------------------------------------------------
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
      CHARACTER*8 KEYID
C
      CHARACTER*4 CHAINT,NAME
C
    1 KARTA = MREADC(IW)
      IF(KARTA.EQ.0) GO TO 10
      CALL BLIST(IW,'C+',CHAINT(IW(KARTA-3)) )
      GO TO 1
C
C  TRIG bank. Get range of events to process
  10  IND = NLINK('TRIG',0)
      IF (IND.NE.0) THEN
         NFEVNT = IW(IND + 1)
         MXEVNT = IW(IND + 2) - NFEVNT + 1
         CALL BDROP(IW,'TRIG')
      ENDIF
C
C  RNDM  Define random seed
      IND = NLINK('RNDM',0)
      IF (IND.NE.0) THEN
         DO 4 M = 1,IW(IND)
   4      INSEED(M) = IW(IND + M)
         CALL BDROP(IW,'RNDM')
      ENDIF
C
C  FILI Input file name (Galeph or private Track element file)
      IND = NLINK('FILI',0)
      IF (IND.NE.0) THEN
         DO 5 M = 1,IW(IND)
            K = 4*M
            TRKFIL(K-3:K) = CHAINT(IW(IND + M))
    5    CONTINUE
         CALL BDROP(IW,'FILI')
      ENDIF
C
C  FILO Digitization output file name
      IND = NLINK('FILO',0)
      IF (IND.NE.0) THEN
         DO 6 M = 1,IW(IND)
            K = 4*M
            DIGFIL(K-3:K) = CHAINT(IW(IND + M))
    6    CONTINUE
         CALL BDROP(IW,'FILO')
      ENDIF
      LWRITE = DIGFIL.NE.'NONE'
C
C  PRIN Printout: RESP = Print out electronic response shape
C                 GEOM = Print out TPC geometry
C
      IND = NLINK('PRIN',0)
      IF (IND.NE.0) THEN
         DO 7 M=1,IW(IND)
            IF (CHAINT(IW(IND+M)).EQ.'RESP') THEN
               LTPCSA = .TRUE.
            ELSEIF (CHAINT(IW(IND+M)).EQ.'GEOM') THEN
               LPRGEO = .TRUE.
            ELSE
               WRITE(6,1001) CHAINT(IW(IND+M))
 1001          FORMAT(' +++TSRCON+++ Unknown PRIN key ',A6,' ignored')
            ENDIF
    7    CONTINUE
         CALL BDROP(IW,'PRIN')
      ENDIF
C
C OPTI Options: NRED    = suppress wire data reduction
C               TPC9    = TPC90 simulation (not implemented yet)
C               INEPio  = Read in files using BOS EPIO format
C               OUTEpio = Write out files using BOS EPIO format
C               FAST    = Use P. Janot's fast version of TPCSIM
C               FULL    = Use original full version of TPCSIM
C               DTEB    = Drop track element BANKs
C
      IND = NLINK('OPTI',0)
      IF (IND.NE.0) THEN
         DO 8 M=1,IW(IND)
            NAME = CHAINT(IW(IND+M))
            IF (NAME.EQ.'NRED') THEN
               LWREDC = .FALSE.
            ELSEIF (NAME.EQ.'TPC9') THEN
               FTPC90 = .TRUE.
            ELSEIF (NAME.EQ.'INEP') THEN
               REPIO = .TRUE.
            ELSEIF (NAME.EQ.'OUTE') THEN
               WEPIO = .TRUE.
            ELSEIF (NAME.EQ.'FAST') THEN
               ILEVEL = 'FAST'
            ELSEIF (NAME.EQ.'FULL') THEN
               ILEVEL = 'FULL'
            ELSEIF (NAME.EQ.'RN32') THEN
               LRND32 = .TRUE.
            ELSEIF (NAME.EQ.'DTEB') THEN
               LDROP = .TRUE.
            ELSEIF (NAME .EQ. 'TPAD') THEN
               LTTDIG = .TRUE.
            ELSEIF (NAME.EQ.'IO  ') THEN
            ELSEIF (NAME.EQ.'PIO ') THEN
            ELSE
               WRITE(6,1002) NAME
 1002          FORMAT(' +++TSRCON+++ Unknown OPTI key ',A4,' ignored')
            ENDIF
    8    CONTINUE
         CALL BDROP(IW,'OPTI')
      ENDIF
C
C DEBU  (Process, Level, # calls after which debug is turned off)
C
      IDEBU = NLINK('DEBU',0)
      IF (IDEBU.NE.0) THEN
         IND = IDEBU
    9    IF (CHAINT(IW(IND+1)).EQ.'TREL') THEN
            NTPCDE = IW(IND+2)
            NCALDE = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'DEDX') THEN
            NTPCDD = IW(IND+2)
            NCALDD = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'TRAN') THEN
            NTPCDT = IW(IND+2)
            NCALDT = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'AVAL') THEN
            NTPCDA = IW(IND+2)
            NCALDA = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'PCPL') THEN
            NTPCDC = IW(IND+2)
            NCALDC = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'TCPL') THEN
            NTPCDR = IW(IND+2)
            NCALDR = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'SIGN') THEN
            NTPCDS = IW(IND+2)
            NCALDS = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'SHAM') THEN
            NTPCSA = IW(IND+2)
            NCALSA = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'DIGI') THEN
            NTPCDI = IW(IND+2)
            NCALDI = IW(IND+3)
         ELSEIF (CHAINT(IW(IND+1)).EQ.'TDBG') THEN
            LTDEBU = .TRUE.
         ELSE
            WRITE(6,1003) CHAINT(IW(IND+1))
 1003       FORMAT(' +++TSRCON+++ Unknown DEBU key ',A6,' ignored')
         ENDIF
         IND = IND + 3
         IF (IND - IDEBU .LT. IW(IDEBU)) GO TO 9
         CALL BDROP(IW,'DEBU')
      ENDIF
C
C  RUNC bank
C
      IRUNC = NLINK('RUNC',0)
      IF (IRUNC.EQ.0) GO TO 999
      IND = IRUNC
C
C Get the next key and update variable of the same name
C
    2 KEYID(1:4) = CHAINT(IW(IND+1))
      KEYID(5:8) = CHAINT(IW(IND+2))
      IF(KEYID(1:6).EQ.'CFIELD') THEN
         CFIELD = RW(IND+3)
         BCFGEV = CLGHT*CFIELD*1.E-5
         BCFMEV = BCFGEV*1000.
      ELSEIF(KEYID(1:6).EQ.'MXTRAN') THEN
         MXTRAN = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'DRFVEL') THEN
         DRFVEL = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SIGMA  ') THEN
         SIGMA = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SIGTR  ') THEN
         SIGTR = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'ITRCON') THEN
         ITRCON = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NPOLYA') THEN
         NPOLYA = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'AMPLIT') THEN
         AMPLIT = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'CUTOFF') THEN
         CUTOFF = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NCPAD') THEN
         NCPAD = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'EFFCP ') THEN
         EFFCP = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SIGW  ') THEN
         SIGW = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SIGH  ') THEN
         SIGH = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'HAXCUT') THEN
         HAXCUT = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'TREFCP') THEN
         TREFCP = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'TCSCUT') THEN
         TCSCUT = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SIGR  ') THEN
         SIGR = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SIGARC') THEN
         SIGARC = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'RAXCUT') THEN
         RAXCUT = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'PEDDEF') THEN
         PEDDEF = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SPEDEF') THEN
         SPEDEF = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SGADEF') THEN
         SGADEF = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'SDIDEF') THEN
         SDIDEF = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'WIRNRM') THEN
         WIRNRM = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'PADNRM') THEN
         PADNRM = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'TRGNRM') THEN
         TRGNRM = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'TPDGBN') THEN
         TPDGBN = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'LTHRSH') THEN
         LTHRSH = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NPRESP') THEN
         NPRESP = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NPOSTS') THEN
         NPOSTS = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'MINLEN') THEN
         MINLEN = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'LTHRS2') THEN
         LTHRS2 = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NPRES2') THEN
         NPRES2 = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NPOST2') THEN
         NPOST2 = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'MINLE2') THEN
         MINLE2 = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'WPSCAL') THEN
         WPSCAL = RW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'NWSMAX') THEN
         NWSMAX = IW(IND+3)
      ELSEIF(KEYID(1:6).EQ.'THRZTW') THEN
         THRZTW = RW(IND+3)
      ELSE
         WRITE(6,1004) KEYID
 1004    FORMAT(' +++TSRCON+++ Unknown key ',A10,'  STOP RUN')
         STOP
      ENDIF
      IND = IND + 3
      IF (IND-IRUNC.LT.IW(IRUNC)) GO TO 2
      CALL BDROP(IW,'RUNC')
C
C
  999 RETURN
      END
