      SUBROUTINE TSAVCN(IPROG)
C-----------------------------------------------------------------------
C!  Saves simulation conditions in BOS bank 'TSIM'
C
C  Called from:  TSINIT
C  M. Mermikides  18/4/86
C
C Input :    Passed:   IPROG : Program invoking TPCSIM ('TPC'
C                              or 'GAL')
C
C  D. Cowen     12 FEB 88    Do not add TSIM to C list until
C                            after RUNH etc. so JULIA can
C                            use fast BOS read.
C  P. Janot     29 May 88    Use AUBOS from ALEPHLIB !
C  P. Janot     26 Aug 88    Add TPCSIM version number in TSIM (3rd
C                            word).
C                            Add TPCSIM level (FAST or FULL) in 45th
C                            TSIM word.
C  P. Janot     29 Sep 88    Add a word to say whether TPCSIM ran
C                            in GALEPH or alone.
C  P. Janot     03 Oct 88    Re-order TSIM .
C  P. Janot     11 oct 88    Use HAC to build TSIM
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
      PARAMETER(JTSITV=1,JTSIMX=2,JTSIIT=3,JTSINS=4,JTSINF=5,JTSINP=6,
     +          JTSINC=7,JTSILT=8,JTSINR=9,JTSINO=10,JTSIMI=11,
     +          JTSILH=12,JTSINE=13,JTSINT=14,JTSIMN=15,JTSINW=16,
     +          JTSIDV=17,JTSISA=18,JTSISR=19,JTSITA=20,JTSITD=21,
     +          JTSIAM=22,JTSICU=23,JTSIFF=24,JTSISW=25,JTSISH=26,
     +          JTSIHX=27,JTSIEF=28,JTSISI=29,JTSISC=30,JTSIRX=31,
     +          JTSITS=32,JTSIPE=33,JTSISP=34,JTSISG=35,JTSISD=36,
     +          JTSIWN=37,JTSIPN=38,JTSITN=39,JTSICF=40,JTSIWS=41,
     +          JTSITZ=42,JTSIIL=43,JTSIIG=44,LTSIMA=44)
      CALL AUBOS('TSIM',0,46,IND,KGARB)
      CALL TPCVER(VERTPC)
      IF(KGARB.EQ.2) GOTO 800
      IW(IND+ 1       ) = LTSIMA
      IW(IND+ 2       ) = 1
      IW(IND+ 2+JTSITV) = IFIX(VERTPC*100.01)
      IW(IND+ 2+JTSIMX) = MXTRAN
      IW(IND+ 2+JTSIIT) = ITRCON
      IW(IND+ 2+JTSINS) = NLSHAP
      IW(IND+ 2+JTSINF) = NSHPOF
      IW(IND+ 2+JTSINP) = NPOLYA
      IW(IND+ 2+JTSINC) = NCPAD
      IW(IND+ 2+JTSILT) = LTHRSH
      IW(IND+ 2+JTSINR) = NPRESP
      IW(IND+ 2+JTSINO) = NPOSTS
      IW(IND+ 2+JTSIMI) = MINLEN
      IW(IND+ 2+JTSILH) = LTHRS2
      IW(IND+ 2+JTSINE) = NPRES2
      IW(IND+ 2+JTSINT) = NPOST2
      IW(IND+ 2+JTSIMN) = MINLE2
      IW(IND+ 2+JTSINW) = NWSMAX
      RW(IND+ 2+JTSIDV) = DRFVEL
      RW(IND+ 2+JTSISA) = SIGMA
      RW(IND+ 2+JTSISR) = SIGTR
      RW(IND+ 2+JTSITA) = TPANBN
      RW(IND+ 2+JTSITD) = TPDGBN
      RW(IND+ 2+JTSIAM) = AMPLIT
      RW(IND+ 2+JTSICU) = CUTOFF
      RW(IND+ 2+JTSIFF) = EFFCP
      RW(IND+ 2+JTSISW) = SIGW
      RW(IND+ 2+JTSISH) = SIGH
      RW(IND+ 2+JTSIHX) = HAXCUT
      RW(IND+ 2+JTSIEF) = TREFCP
      RW(IND+ 2+JTSISI) = SIGR
      RW(IND+ 2+JTSISC) = SIGARC
      RW(IND+ 2+JTSIRX) = RAXCUT
      RW(IND+ 2+JTSITS) = TCSCUT
      RW(IND+ 2+JTSIPE) = PEDDEF
      RW(IND+ 2+JTSISP) = SPEDEF
      RW(IND+ 2+JTSISG) = SGADEF
      RW(IND+ 2+JTSISD) = SDIDEF
      RW(IND+ 2+JTSIWN) = WIRNRM
      RW(IND+ 2+JTSIPN) = PADNRM
      RW(IND+ 2+JTSITN) = TRGNRM
      RW(IND+ 2+JTSICF) = CFIELD
      RW(IND+ 2+JTSIWS) = WPSCAL
      RW(IND+ 2+JTSITZ) = THRZTW
      IW(IND+ 2+JTSIIL) = INTCHA(ILEVEL)
      IW(IND+ 2+JTSIIG) = IPROG
C
      GO TO 999
 800  WRITE(6,'('' +++TSAVCN+++ Error booking TSIM bank'')')
 999  RETURN
      END
