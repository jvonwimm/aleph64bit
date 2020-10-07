      SUBROUTINE AGMUCH
C-----------------------------------------------------------
C!  Implement Muon chambers geometry
C  Author :   B. Bloch-Devaux       4 October 85
C           - Taijie Wang  last  revised 30 November 86
C           - modified for DAF and new bank layout
C             by F.Bossi,D.Kuhn,R.Xu 87-09-14
C            - adapted to GAL196 by B. Bloch 25-01-88
C           - modified for final dbase
C             by A. Antonelli, F. Bossi 1 July 89
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSTMED,GSVOLU,GSPOS,GSPOSP,
C          GSROTM                      from  GEANT3
C.
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'MUON' level
C.
C-----------------------------------------------------------
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
C
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (NHTIN = 40 , INCHT = 10 , NAVTD = 4)
      COMMON/MUNAMC/ NAMUHT,NAMUDI,NAMUDT,NAMUTD,NAMUDG
     +             , NAMUOG,NAMBAG,NAMBSG,NAMBTG,NAMBBG
     +             , NAMECG,NAMEBG,NAMETG,NAMESG
     +             , NAMMAG,NAMMBG,NAMMTG,NAMMSG
     &             , JDMUST,JDMTHT
C
C! The parameters needed by the geometry routine AGMUCH
      PARAMETER (NMBIN = 12, NMBOU = 12 )
      PARAMETER (NMMIN = 10, NMMOU =  9, NMMA = NMMIN+NMMOU, IMMBT =  9)
      PARAMETER (NMCIN = 4, NMCOU = 4, NMCA = NMCIN+NMCOU)
      PARAMETER (NMMBI = NMMA+NMMIN, NMCBI = NMCA+NMCIN)
      COMMON /MUG1PR/   MMADPR(12,4)
C
      PARAMETER(JMUOID=1,JMUOVR=2,JMUONS=4,JMUOWI=5,JMUOHE=6,JMUOTU=7,
     +          JMUOAC=8,JMUOSP=9,JMUODI=10,JMUODE=11,JMUOXS=12,
     +          JMUOYS=13,JMUOSE=14,JMUOET=15,JMUOEX=16,JMUOEY=17,
     +          JMUOX1=18,JMUOX2=19,JMUOX3=20,JMUOX4=21,JMUOY1=22,
     +          JMUOY2=23,JMUOY3=24,JMUOY4=25,LMUOGA=25)
      PARAMETER(JMBAID=1,JMBAVR=2,JMBASU=4,JMBANB=5,JMBATH=6,JMBALE=7,
     +          JMBAR1=8,JMBAR2=9,JMBAPD=10,JMBAY1=11,JMBAY2=12,
     +          LMBAGA=12)
      PARAMETER(JMBSID=1,JMBSVR=2,JMBSNO=4,JMBSVO=5,JMBSZC=6,JMBSRC=7,
     +          JMBSDE=8,JMBST1=9,JMBST2=10,JMBSTA=11,JMBSNA=12,
     +          JMBSK1=13,JMBSK2=14,LMBSGA=14)
      PARAMETER(JMBTID=1,JMBTVR=2,JMBTNA=4,JMBTZB=5,JMBTRB=6,JMBTY1=7,
     +          JMBTY2=8,JMBTNX=9,JMBTZT=10,JMBTRT=11,JMBTW1=12,
     +          LMBTGA=12)
      PARAMETER(JMBBID=1,JMBBVR=2,JMBBB1=4,JMBBL1=5,JMBBU1=6,JMBBB2=7,
     +          JMBBL2=8,JMBBU2=9,JMBBB3=10,JMBBL3=11,JMBBU3=12,
     +          JMBBB4=13,JMBBL4=14,JMBBU4=15,JMBBP3=16,JMBBP4=17,
     +          LMBBGA=17)
      PARAMETER(JMECID=1,JMECVR=2,JMECSU=4,JMECNS=5,JMECZI=6,JMECZE=7,
     +          JMECPD=8,JMECTH=9,JMECDZ=10,JMECXO=11,LMECGA=11)
      PARAMETER(JMEBID=1,JMEBVR=2,JMEBB1=4,JMEBL1=5,JMEBU1=6,JMEBB2=7,
     +          JMEBL2=8,JMEBU2=9,JMEBB3=10,JMEBL3=11,JMEBU3=12,
     +          JMEBB4=13,JMEBL4=14,JMEBU4=15,LMEBGA=15)
      PARAMETER(JMETID=1,JMETVR=2,JMETNA=4,JMETXB=5,JMETYB=6,JMETX1=7,
     +          JMETX2=8,JMETYS=9,JMETPI=10,JMETNX=11,JMETNY=12,
     +          JMETNP=13,JMETN2=14,JMETN1=15,JMETLE=16,LMETGA=40)
      PARAMETER(JMESID=1,JMESVR=2,JMESNO=4,JMESXC=5,JMESYC=6,JMESZC=7,
     +          JMESTA=8,JMESNA=9,JMESK1=10,JMESK2=11,LMESGA=11)
      PARAMETER(JMMAID=1,JMMAVR=2,JMMASU=4,JMMANS=5,JMMAZ0=6,JMMAPD=7,
     +          JMMATH=8,JMMAPI=9,JMMADS=10,JMMAZ1=11,JMMATB=12,
     +          LMMAGA=12)
      PARAMETER(JMMBID=1,JMMBVR=2,JMMBNO=4,JMMBB1=5,JMMBO1=6,JMMBB2=7,
     +          JMMBO2=8,JMMBB3=9,JMMBO3=10,JMMBB4=11,JMMBO4=12,
     +          JMMBB5=13,JMMBO5=14,JMMBL1=15,JMMBU1=16,JMMBL2=17,
     +          JMMBU2=18,JMMBL3=19,JMMBU3=20,JMMBL4=21,JMMBU4=22,
     +          JMMBL5=23,JMMBU5=24,JMMBL6=25,JMMBU6=26,JMMBL7=27,
     +          JMMBU7=28,JMMBL8=29,JMMBU8=30,JMMBL9=31,JMMBU9=32,
     +          JMMBL0=33,JMMBU0=34,LMMBGA=34)
      PARAMETER(JMMTID=1,JMMTVR=2,JMMTNA=4,JMMTZB=5,JMMTRB=6,JMMTZT=7,
     +          JMMTRT=8,JMMTNX=9,LMMTGA=9)
      PARAMETER(JMMSID=1,JMMSVR=2,JMMSNO=4,JMMSL1=5,JMMSL2=6,JMMSR1=7,
     +          JMMSR2=8,JMMSTL=9,JMMSRL=10,JMMSNY=11,JMMSX1=12,
     +          JMMSX2=13,JMMSDZ=14,JMMSZC=15,JMMSRC=16,JMMSDE=17,
     +          JMMSNA=18,JMMSTA=19,JMMSOS=20,JMMSVO=21,JMMSK1=22,
     +          JMMSK2=23,LMMSGA=23)
C
C       Angle region of middle angle modules -- MMIA, MMOA,
C       MMBA, MMIB, MMOB and MMBB is defined by numbers directly.
C       Unless the design being chenged a lot, it should not
C       make troubles.
C
      PARAMETER (ANGM1 = -55., ANGM2 = 235.)
      EXTERNAL JHOCHA,NAMIND
      PARAMETER (LTAB1 = 5, LTAB2=5, LMED=2 )
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2)
      DATA TAB1/10.,0.05,0.2,1.0,0.1,
     *          10.,0.05,0.2,1.0,0.1/
C
C       Notice, this is the constants of LEAD !!!
C
      DATA TAB2/207.19,82.,11.35,0.56,18.5/
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C!     INDICES OF MUON-BANKS FOR USE WITHIN ROUTINES
      JMBAG = IW(NAMBAG)
      JMUOG = IW(NAMUOG)
      JMBBG = IW(NAMBBG)
      JMBSG = IW(NAMBSG)
      JMBTG = IW(NAMBTG)
C
      JMECG = IW(NAMECG)
      JMEBG = IW(NAMEBG)
      JMETG = IW(NAMETG)
      JMESG = IW(NAMESG)
C
      JMMAG = IW(NAMMAG)
      JMMBG = IW(NAMMBG)
      JMMTG = IW(NAMMTG)
      JMMSG = IW(NAMMSG)
C
C
C      Material for muon chamber set to sensitive air
C
      IAGMED=IAGMED+1
C
C      Define sensitive volume flag as defined if SET 'MUON' is selected
C
C      IAG is the material number of the active gas
C
C       Run condition ICMUJO(4) decides which material is filled
C       inside the sensitive volume:
C         = 0,  air; otherwise, some streamer tube material.
C
      ISV=IDETJO(8)
      IF( ICMUJO(4) .EQ. 0 )THEN
        IAG = 15
        CALL GSTMED(IAGMED,'MUON SENSITIVE GAS $',IAG,ISV,0,ALFIEL,
     *       TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
      ELSE
        IAGMAT = IAGMAT + 1
        CALL GSMATE(IAGMAT,'STREAMER TUBE MATTER$',
     *       TAB2(1),TAB2(2),TAB2(3),TAB2(4),TAB2(5),0,0)
        IAG = IAGMAT
        CALL GSTMED(IAGMED,'MUON SENSITIVE GAS $',IAG,ISV,0,ALFIEL,
     *       TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
      ENDIF
C
C       IMDIN is the insensitive medium outside magnetic field
C
      IMDIN = 2
C
C       The barrel modules
C
      THCKB = RTABL(JMBAG,1,JMBATH)
C
C       Define sensitive volume 'MUB1' for inner barrel module
      FLMB=RTABL(JMBAG,1,JMBALE)
      PTAB(1) = 0.5 * RTABL(JMBTG,1,JMBTRB)
      PTAB(2) = 0.5 * FLMB
      PTAB(3) = 0.5 * THCKB
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('MUB1')
      IAGSEN(IAGSLV,2) = 4
      CALL GSVOLU('MUB1','BOX ',IAGMED,PTAB,3,IVOL)
C
C       Define 'MUB1' inside 'HBAR' only once, because
C       'HBAR' was defined 12 times
C
      X = RTABL(JMBAG,1,JMBAR1)
      Y = 0.
      Z = 0.
C
C       Define a rotation matrix for turning Z to X, Y to Z and X to Y
C
      IAGROT = IAGROT + 1
      CALL GSROTM(IAGROT,D90,D90,0.,0.,D90,0.)
      IR = IAGROT
C
      CALL GSPOS('MUB1',1,'HBAR',X,Y,Z,IR,'ONLY')
C
C       Fake volume MUBO containing 12 outer barrel modules.
C       MUBO is inside MUON.
C
      PTAB(1) = AGLIMR(8)
      PTAB(2) = ALRMAX
      PTAB(3) = 0.5 * FLMB
      CALL GSVOLU('MUBO','TUBE',IMDIN,PTAB,3,IVOL)
      X = 0.
      Y = 0.
      Z = 0.
      IR = 0
      CALL GSPOS('MUBO',1,'MUON',X,Y,Z,IR,'ONLY')
C
C       Define sensitive volume MUB2 for outer barrel modules
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('MUB2')
      IAGSEN(IAGSLV,2) = 4
      CALL GSVOLU('MUB2','BOX ',IAGMED,PTAB,0,IVOL)
C
C       12 outer barrel modules
C
      IAGFHB=IAGROT+1
      PHI = 0.
      LHSEC=12
      DPHI=TWOPI/LHSEC
      DO 3 N=1,LHSEC
         IAGROT = IAGROT + 1
         PHID = PHI*RADEG
         CALL GSROTM(IAGROT,D90,PHID + D90,0.,0.,D90,PHID)
         PHI = PHI+DPHI
   3  CONTINUE
      PTAB(2) = 0.5 * FLMB
      PTAB(3) = 0.5 * THCKB
      NPAR = 3
      NSLOP=-1
      DO 10 I = 18,34
         NSLOT = ITABL(JMBSG,I,JMBSVO)
         IF (NSLOT.EQ.NSLOP) GO TO 10
         NSLOP=NSLOT
         PHI = (NSLOT-1)*PIBY6
         RIN = RTABL(JMBAG,1,JMBAR2)
         DELTA = RTABL(JMBSG,I,JMBSDE)
         IF(NSLOT.EQ.4) DELTA = 0.
         COSP = COS( PHI )
         SINP = SIN( PHI )
         X = RIN*COSP - DELTA*SINP
         Y = RIN*SINP + DELTA*COSP
         ITYPE=ITABL(JMBSG,I,JMBSK2)
         IF(ITYPE.EQ.8) ITYPE=5
         PTAB(1) = 0. 5* RTABL(JMBTG,ITYPE,JMBTRB)
         IR = IAGFHB + NSLOT - 1
         CALL GSPOSP('MUB2',NSLOT,'MUBO',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  10  CONTINUE
 888  CONTINUE
C
C       Define two intermediate modules 'MUEA' and 'MUEB'
C
      PTAB(1) = 0.
      PTAB(2) = D360
      PTAB(3) = 4
      PTAB(4) = 0.5 * FLMB
      PTAB(5) = AGLIMR(8)
      PTAB(6) = ALRMAX
      PTAB(7) = AGLIMZ(4)-0.01
      PTAB(8) = PTAB(5)
      PTAB(9) = PTAB(6)
      PTAB(10) = AGLIMZ(4)
      PTAB(11) = AGLIMR(4)
      PTAB(12) = PTAB(6)
      PTAB(13) = ALZMAX
      PTAB(14) = PTAB(11)
      PTAB(15) = PTAB(6)
      CALL GSVOLU('MUEA','PCON',IMDIN,PTAB,15,IVOL)
      X = 0.
      Y = 0.
      Z = 0.
      CALL GSPOS('MUEA',1,'MUON',X,Y,Z,0,'ONLY')
C
C       'MUEB' is defined as the same as 'MUEA' with a rotation
C
      CALL GSVOLU('MUEB','PCON',IMDIN,PTAB,15,IVOL)
      IR = 2
      CALL GSPOS('MUEB',1,'MUON',X,Y,Z,IR,'ONLY')
C
C       The middle angle modules
C
      THCK10 = RTABL(JMMAG,1,JMMATH)
      THCKM = RTABL(JMMAG,1,JMMATB)
C
C       Find the minimum and the maximum radius for inner and outer
C       layers as well as widest width --- A side
C
C
      RMNOU = ALRMAX
      WDSIN = 0.
C
      DO 30 I=1,NMMIN
         IF( I .EQ. IMMBT )GOTO 30
         INDVL = ITABL(JMMSG,I,JMMSK2)
         WIDTH = RTABL(JMMTG,INDVL,JMMTZB)
         WDSIN = MAX( WIDTH, WDSIN )
   30 CONTINUE
C
      MMIN = ITABL(JMMSG,10,JMMSID)
      Z0PLAN = RTABL(JMMAG,1,JMMAZ0)
      Z10PLA = RTABL(JMMAG,1,JMMAZ1)
      PLANDS = RTABL(JMMAG,1,JMMAPD)
      DO 40 I=1,NMMOU
         IMMRW = MMIN + I
         RC = RTABL(JMMSG,IMMRW,JMMSRC)
         IF(I.GE.2.AND.I.LE.6) THEN
           RIN = RC - Z0PLAN
         ELSE
           RIN = RC + PLANDS + Z0PLAN - THCKM
         ENDIF
         RMNOU = MIN( RIN, RMNOU )
   40 CONTINUE
      RMINA = RMNOU
C
C       Use RMNOU instead of RMAXIN  as Rmax of 'MMIA'.
C       In this way, avoid MMIA touching the modules of
C       outer layer.
C
      PTAB(1) = AGLIMR(8)
      PTAB(2) = RMNOU
      PTAB(3) = 0.5*WDSIN
      PTAB(4) = ANGM1
      PTAB(5) = ANGM2
      CALL GSVOLU('MMIA','TUBS',IMDIN,PTAB,5,IVOL)
      X = 0.
      Y = 0.
      Z = 0.5  * ( FLMB + WDSIN )
      ZMMIA = Z
      CALL GSPOS('MMIA',1,'MUEA',X,Y,Z,0,'ONLY')
      PTAB(1) = RMNOU
      PTAB(2) = ALRMAX
      WIDMO = RTABL(JMESG,5,JMESZC) - RTABL(JMECG,1,JMECZE) - 0.5*FLMB
      PTAB(3) = 0.5 * WIDMO
      PTAB(4) = ANGM1
      PTAB(5) = ANGM2
      CALL GSVOLU('MMOA','TUBS',IMDIN,PTAB,5,IVOL)
      X = 0.
      Y = 0.
      Z = 0.5 * ( FLMB + WIDMO )
      ZMMOA = Z
      CALL GSPOS('MMOA',1,'MUEA',X,Y,Z,0,'ONLY')
C
C       Find the minimum and the maximum radius for inner and outer
C       layers as well as widest width --- B side
C
C
      RMNOU = ALRMAX
      WDSIN = 0.
C
      MMIO = ITABL(JMMSG,19,JMMSID)
      DO 60 I=1,NMMIN
          IMMRW = MMIO + I
          IF( I .EQ. IMMBT )GOTO 60
          INDVL = ITABL(JMMSG,IMMRW,JMMSK2)
          WIDTH = RTABL(JMMTG,INDVL,JMMTZB)
          WDSIN = MAX( WIDTH, WDSIN )
   60 CONTINUE
C
      MMIOI = ITABL(JMMSG,29,JMMSID)
      DO 70 I=1,9
         IMMRW = MMIOI + I
         RC = RTABL(JMMSG,IMMRW,JMMSRC)
         IF(I.GE.2.AND.I.LE.6) THEN
           RIN = RC - Z0PLAN
         ELSE
           RIN = RC + PLANDS + Z0PLAN - THCKM
         ENDIF
          RMNOU = MIN( RIN, RMNOU )
   70 CONTINUE
      RMINB = RMNOU
C
      PTAB(1) = AGLIMR(8)
      PTAB(2) = RMNOU
      PTAB(3) = 0.5*WDSIN
      PTAB(4) = ANGM1
      PTAB(5) = ANGM2
      CALL GSVOLU('MMIB','TUBS',IMDIN,PTAB,5,IVOL)
      X = 0.
      Y = 0.
      Z = 0.5 * ( FLMB + WDSIN )
      ZMMIB = Z
      CALL GSPOS('MMIB',1,'MUEB',X,Y,Z,0,'ONLY')
      PTAB(1) = RMNOU
      PTAB(2) = ALRMAX
      PTAB(3) = 0.5*WIDMO
      PTAB(4) = ANGM1
      PTAB(5) = ANGM2
      CALL GSVOLU('MMOB','TUBS',IMDIN,PTAB,5,IVOL)
      X = 0.
      Y = 0.
      Z = 0.5 * ( FLMB + WIDMO )
      ZMMOB = Z
      CALL GSPOS('MMOB',1,'MUEB',X,Y,Z,0,'ONLY')
C
C       Rotation matrices for middle angle modules
C       different from barrels', because the tube
C       orientations are not the same.
C
      IAGMU = IAGROT + 1
      PHI = 0.
C
      DO 80 I=1,12
         IAGROT = IAGROT +1
         PHID = PHI * RADEG
         CALL GSROTM(IAGROT,0.,0.,D90,PHID-D90,D90,PHID)
         PHI = PHI + PIBY6
   80 CONTINUE
C
C       Define sensitive volume MUM1  for inner middle angle modules
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('MUM1')
      IAGSEN(IAGSLV,2) = 5
      CALL GSVOLU('MUM1','BOX ',IAGMED,PTAB,0,IVOL)
C
C       Define sensitive volume MUM2  for outer middle angle modules
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('MUM2')
      IAGSEN(IAGSLV,2) = 5
      CALL GSVOLU('MUM2','BOX ',IAGMED,PTAB,0,IVOL)
C
C       Inner layer module at +Z side -- 'MMIA'
C
      NPAR = 3
      PTAB(3) = 0.5 * THCKM
      DO 90 I=1,NMMIN
         NSLOT = ITABL(JMMSG,I,JMMSVO)
         PHI = (NSLOT-1)*PIBY6
         RC = RTABL(JMMSG,I,JMMSRC)
         IF(I.GE.2.AND.I.LE.6) THEN
           RIN = RC - Z0PLAN
         ELSE
           RIN = RC + PLANDS + Z0PLAN - THCKM
         ENDIF
         RIN = RIN + PTAB(3)
         DELTA = RTABL(JMMSG,I,JMMSDE)
         COSP = COS( PHI )
         SINP = SIN( PHI )
         X = RIN*COSP - DELTA*SINP
         Y = RIN*SINP + DELTA*COSP
         IDVOL = ITABL(JMMSG,I,JMMSK2)
         PTAB(1) = 0.5*RTABL(JMMTG,IDVOL,JMMTZB)
         PTAB(2) = 0.5*RTABL(JMMTG,IDVOL,JMMTRB)
         ZIN = RTABL(JMMSG,I,JMMSZC)
         Z = ZIN - ZMMIA
         NR = PHI/PIBY6 + 1.5
         IF( I .NE. IMMBT )THEN
            IR = IAGMU + NSLOT - 1
            CALL GSPOSP('MUM1',NSLOT,'MMIA',X,Y,Z,IR,'ONLY',PTAB,NPAR)
         ENDIF
   90 CONTINUE
C
C       Bottom module at +Z side as the 9th one of the inner layer
C
      PTAB(3) = 0.5*THCK10
      ZIN = RTABL(JMMSG,IMMBT,JMMSZC)
      NSLOT = ITABL(JMMSG,IMMBT,JMMSVO)
      PHI = (NSLOT-1)*PIBY6
      RMMBA = RTABL(JMMSG,IMMBT,JMMSRC) + PLANDS + Z10PLA - THCK10
      RIN = RMMBA + PTAB(3)
      DELTA = RTABL(JMMSG,IMMBT,JMMSDE)
      COSP = COS( PHI )
      SINP = SIN( PHI )
      X = RIN*COSP - DELTA*SINP
      Y = RIN*SINP + DELTA*COSP
      IDVOL = ITABL(JMMSG,IMMBT,JMMSK2)
      PTAB(1) = 0.5 * RTABL(JMMTG,IDVOL,JMMTZB)
      PTAB(2) = 0.5 * RTABL(JMMTG,IDVOL,JMMTRB)
      Z = ZIN
      CALL GSVOLU('MMBA','BOX ',IMDIN,PTAB,3,IVOL)
      IR = IAGMU + NSLOT - 1
      CALL GSPOS('MMBA',1,'MUEA',X,Y,Z,IR,'ONLY')
      X = 0.
      Y = 0.
      Z = 0.
      CALL GSPOSP('MUM1',NSLOT,'MMBA',X,Y,Z,0,'ONLY', PTAB,NPAR)
C
C       Outer layer modules at +Z side -- 'MMOA'
C
      PTAB(3) = 0.5*THCKM
      DO 100 I=1,NMMOU
         IMMRW = MMIN+I
         NSLOT = ITABL(JMMSG,IMMRW,JMMSVO)
         PHI = (NSLOT-1)*PIBY6
         RC = RTABL(JMMSG,IMMRW,JMMSRC)
         IF(I.GE.2.AND.I.LE.6) THEN
           RIN = RC - Z0PLAN
         ELSE
           RIN = RC + PLANDS + Z0PLAN - THCKM
         ENDIF
         RIN = RIN + PTAB(3)
         DELTA = RTABL(JMMSG,IMMRW,JMMSDE)
         COSP = COS( PHI )
         SINP = SIN( PHI )
         X = RIN*COSP - DELTA*SINP
         Y = RIN*SINP + DELTA*COSP
         IDVOL = ITABL(JMMSG,IMMRW,JMMSK2)
         PTAB(1) = 0.5*RTABL(JMMTG,IDVOL,JMMTZB)
         PTAB(2) = 0.5*RTABL(JMMTG,IDVOL,JMMTRB)
         ZIN = RTABL(JMMSG,IMMRW,JMMSZC)
         Z = ZIN - ZMMOA
         IR = IAGMU + NSLOT - 1
         CALL GSPOSP('MUM2',NSLOT,'MMOA',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  100 CONTINUE
C
C       Inner layer module at -Z side -- 'MMIB'
C       Notice that for B side, the phi position of the module
C       in MUEB is different from what NSLOT defines
C
      DO 110 I=1,NMMIN
         IMMRW = MMIO + I
         NSLOT = ITABL(JMMSG,IMMRW,JMMSVO)
         NPHI =  8- NSLOT + (NSLOT/8)*12
         PHI = (NPHI-1)*PIBY6
         RC = RTABL(JMMSG,IMMRW,JMMSRC)
         IF(I.GE.2.AND.I.LE.6) THEN
           RIN = RC - Z0PLAN
         ELSE
           RIN = RC + PLANDS + Z0PLAN - THCKM
         ENDIF
         RIN = RIN + PTAB(3)
         DELTA = -RTABL(JMMSG,IMMRW,JMMSDE)
         COSP = COS( PHI )
         SINP = SIN( PHI )
         X = RIN*COSP - DELTA*SINP
         Y = RIN*SINP + DELTA*COSP
         IDVOL = ITABL(JMMSG,IMMRW,JMMSK2)
         PTAB(1) = 0.5*RTABL(JMMTG,IDVOL,JMMTZB)
         PTAB(2) = 0.5*RTABL(JMMTG,IDVOL,JMMTRB)
         ZIN = RTABL(JMMSG,IMMRW,JMMSZC)
         Z = -ZIN  - ZMMIB
         IF( I .NE. IMMBT )THEN
           IR = IAGMU + NPHI - 1
           CALL GSPOSP('MUM1',NSLOT,'MMIB',X,Y,Z,IR,'ONLY', PTAB,NPAR)
         ENDIF
  110 CONTINUE
C
C       Bottom module at -Z side as the 9th one in the inner layer
C
      IMMRW = ITABL(JMMSG,28,JMMSID)
      PTAB(3) = 0.5*THCK10
      ZIN = RTABL(JMMSG,IMMRW,JMMSZC)
      NSLOT = ITABL(JMMSG,IMMRW,JMMSVO)
      NPHI = 8 - NSLOT + (NSLOT/8)*12
      PHI = (NPHI-1)*PIBY6
      RMMBB = RTABL(JMMSG,IMMRW,JMMSRC) + PLANDS + Z10PLA - THCK10
      RIN = RMMBB + PTAB(3)
      DELTA = -RTABL(JMMSG,IMMRW,JMMSDE)
      COSP = COS( PHI )
      SINP = SIN( PHI )
      X = RIN*COSP - DELTA*SINP
      Y = RIN*SINP + DELTA*COSP
      IDVOL = ITABL(JMMSG,IMMRW,JMMSK2)
      PTAB(1) = 0.5 * RTABL(JMMTG,IDVOL,JMMTZB)
      PTAB(2) = 0.5 * RTABL(JMMTG,IDVOL,JMMTRB)
      Z = -ZIN
      CALL GSVOLU('MMBB','BOX ',IMDIN,PTAB,3,IVOL)
      IR = IAGMU + NPHI - 1
      CALL GSPOS('MMBB',1,'MUEB',X,Y,Z,IR,'ONLY')
      X = 0.
      Y = 0.
      Z = 0.
      CALL GSPOSP('MUM1',NSLOT,'MMBB',X,Y,Z,0,'ONLY', PTAB,NPAR)
C
C       Outer layer modules at -Z side -- 'MMOB'
C
      PTAB(3) = 0.5*THCKM
      DO 120 I=1, NMMOU
         IMMRW = MMIOI + I
         NSLOT = ITABL(JMMSG,IMMRW,JMMSVO)
         NPHI =  8- NSLOT + (NSLOT/8)*12
         PHI = (NPHI-1)*PIBY6
         RC = RTABL(JMMSG,IMMRW,JMMSRC)
         IF(I.GE.2.AND.I.LE.6) THEN
           RIN = RC - Z0PLAN
         ELSE
           RIN = RC + PLANDS + Z0PLAN - THCKM
         ENDIF
         RIN = RIN + PTAB(3)
         DELTA = -RTABL(JMMSG,IMMRW,JMMSDE)
         COSP = COS( PHI )
         SINP = SIN( PHI )
         X = RIN*COSP - DELTA*SINP
         Y = RIN*SINP + DELTA*COSP
         IDVOL = ITABL(JMMSG,IMMRW,JMMSK2)
         PTAB(1) = 0.5*RTABL(JMMTG,IDVOL,JMMTZB)
         PTAB(2) = 0.5*RTABL(JMMTG,IDVOL,JMMTRB)
         ZIN = RTABL(JMMSG,IMMRW,JMMSZC)
         Z = - ZIN  - ZMMOB
         IR = IAGMU + NPHI - 1
         CALL GSPOSP('MUM2',NSLOT,'MMOB',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  120 CONTINUE
C
C       The end cap modules
C
       THCKC = RTABL(JMECG,1,JMECTH)
C
C       Fake volume MCIA containing 4 inner endcap modules at +Z side
C
      ZEO = RTABL(JMESG,5,JMESZC)-RTABL(JMECG,1,JMECZE)
C
      PTAB(1) = RMINA
      PTAB(2) = 0.5 * ( RMINA + RMMBA )
      WDCIA = ZEO - AGLIMZ(4)
      PTAB(3) = 0.5 * WDCIA
      CALL GSVOLU('MCIA','BOX ',IMDIN,PTAB,3,IVOL)
      X = 0.
      Y = 0.5 * ( RMINA - RMMBA )
      Z = AGLIMZ(4) + 0.5 * WDCIA
      XMCIA = X
      YMCIA = Y
      ZMCIA = Z
      IR = 0
      CALL GSPOS('MCIA',1,'MUEA',X,Y,Z,IR,'MANY')
C
C       Fake volume MCOA containing 4 outer endcap modules at +Z side
C
      PTAB(1) = ALRMAX
      PTAB(2) = 0.5 * ( ALRMAX + RMMBA )
      WDCOA = ALZMAX - ZEO
      PTAB(3) = 0.5 * WDCOA
C
      CALL GSVOLU('MCOA','BOX ',IMDIN,PTAB,3,IVOL)
      X = 0.
      Y = 0.5 * ( ALRMAX - RMMBA )
      Z = ZEO + 0.5*WDCOA
      XMCOA = X
      YMCOA = Y
      ZMCOA = Z
      IR = 0
      CALL GSPOS('MCOA',1,'MUEA',X,Y,Z,IR,'ONLY')
C
C       Fake volume MCIB containing 4 inner endcap modules at -Z side
C
      PTAB(1) = RMINB
      PTAB(2) = 0.5 * ( RMINB + RMMBB )
      WDCIB = WDCIA
      PTAB(3) = 0.5 * WDCIA
      CALL GSVOLU('MCIB','BOX ',IMDIN,PTAB,3,IVOL)
      ZEI = RTABL(JMESG,1,JMESZC)-RTABL(JMECG,1,JMECZI)
      X = 0.
      Y = 0.5 * ( RMINB - RMMBB )
      Z = ZEI + 0.5*WDCIB
      XMCIB = X
      YMCIB = Y
      ZMCIB = Z
      IR = 0
      CALL GSPOS('MCIB',1,'MUEB',X,Y,Z,IR,'MANY')
C
C       Fake volume MCOB containing 4 outer endcap modules at -Z side
C
      PTAB(1) = ALRMAX
      PTAB(2) = 0.5 * ( ALRMAX + RMMBB )
      WDCOB = WDCOA
      PTAB(3) = 0.5 * WDCOB
      CALL GSVOLU('MCOB','BOX ',IMDIN,PTAB,3,IVOL)
      X = 0.
      Y = 0.5 * ( ALRMAX - RMMBB )
      Z = ZEO + 0.5*WDCOB
      XMCOB = X
      YMCOB = Y
      ZMCOB = Z
      IR = 0
      CALL GSPOS('MCOB',1,'MUEB',X,Y,Z,IR,'ONLY')
C
C       The rotation matrices needed by endcap
C       different from matrix #1 and #2 because
C       all tubes on endcap modules are horrizontal
C
      IAGROT = IAGROT + 1
      IRMC1 = IAGROT
      CALL GSROTM(IAGROT,D90,D90,D90,D180,0.,0.)
      IAGROT = IAGROT + 1
      IRMC2 = IAGROT
      CALL GSROTM(IAGROT,D90,D270,D90,0.,0.,0.)
C
C       Define sensitive volume MUC1 for inner endcap modules
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('MUC1')
      IAGSEN(IAGSLV,2) = 5
      CALL GSVOLU('MUC1','BOX ',IAGMED,PTAB,0,IVOL)
C
C       Define sensitive volume MUC2 for outer endcap modules
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('MUC2')
      IAGSEN(IAGSLV,2) = 5
      CALL GSVOLU('MUC2','BOX ',IAGMED,PTAB,0,IVOL)
C
C       Inner endcap at +Z side  -- 'MCIA'
C
      Z = ZEI + 0.5*THCKC -ZMCIA
      PTAB(3) = 0.5 * THCKC
      NPAR = 3
      DO 130 I=1,NMCIN
         X = RTABL(JMESG,I,JMESXC)
         IF( X .LT. 0. )THEN
            IR = IRMC1
         ELSE
            IR = IRMC2
         ENDIF
         X = X - XMCIA
         Y = RTABL(JMESG,I,JMESYC) - YMCIA
         IDVOL = ITABL(JMESG,I,JMESK2)
         PTAB(1) = 0.5* RTABL(JMETG,IDVOL,JMETYB)
         PTAB(2) = 0.5* RTABL(JMETG,IDVOL,JMETXB)
         NR = I
         CALL GSPOSP('MUC1',NR,'MCIA',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  130 CONTINUE
C
C       Outer endcap at +Z side  -- 'MCOA'
C
      Z = ZEO + 0.5*THCKC - ZMCOA
      MCIN = ITABL(JMESG,4,JMESID)
      DO 140 I=1,NMCOU
         IMCRW = MCIN+I
         X = RTABL(JMESG,IMCRW,JMESXC)
         IF( X .LT. 0. )THEN
            IR = IRMC1
         ELSE
            IR = IRMC2
         ENDIF
         X = X - XMCOA
         Y = RTABL(JMESG,IMCRW,JMESYC) - YMCOA
         IDVOL = ITABL(JMESG,IMCRW,JMESK2)
         PTAB(1) = 0.5* RTABL(JMETG,IDVOL,JMETYB)
         PTAB(2) = 0.5* RTABL(JMETG,IDVOL,JMETXB)
         NR = I
         CALL GSPOSP('MUC2',NR,'MCOA',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  140 CONTINUE
C
C       Inner endcap at -Z side  -- 'MCIB'
C
      Z = ( ZEI + 0.5*THCKC ) - ZMCIB
      MEIO = ITABL(JMESG,8,JMESID)
      DO 150 I=1,NMCIN
         IMCRW = MEIO + I
         X = RTABL(JMESG,IMCRW,JMESXC)
         IF( X .GT. 0. )THEN
            IR = IRMC1
         ELSE
            IR = IRMC2
         ENDIF
         X = -X - XMCIB
         Y = RTABL(JMESG,IMCRW,JMESYC) - YMCIB
         IDVOL = ITABL(JMESG,IMCRW,JMESK2)
         PTAB(1) = 0.5* RTABL(JMETG,IDVOL,JMETYB)
         PTAB(2) = 0.5* RTABL(JMETG,IDVOL,JMETXB)
         NR = I
         CALL GSPOSP('MUC1',NR,'MCIB',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  150 CONTINUE
C
C       Outer endcap at -Z side  -- 'MCOB'
C
      Z = ( ZEO + 0.5*THCKC ) - ZMCOB
      MEIOI = ITABL(JMESG,12,JMESID)
      DO 160 I=1,NMCOU
         IMCRW = MEIOI + I
         X = RTABL(JMESG,IMCRW,JMESXC)
         IF( X .GT. 0. )THEN
            IR = IRMC1
         ELSE
            IR = IRMC2
         ENDIF
         X = -X - XMCOB
         Y = RTABL(JMESG,IMCRW,JMESYC) - YMCOB
         IDVOL = ITABL(JMESG,IMCRW,JMESK2)
         PTAB(1) = 0.5 * RTABL(JMETG,IDVOL,JMETYB)
         PTAB(2) = 0.5 * RTABL(JMETG,IDVOL,JMETXB)
         NR = I
         CALL GSPOSP('MUC2',NR,'MCOB',X,Y,Z,IR,'ONLY', PTAB,NPAR)
  160 CONTINUE
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
        CALL AGDIMP('MUB1',4,'MUON')
        CALL AGDIMP('MUM1',5,'MUON')
        CALL AGDIMP('MUC1',5,'MUON')
      RETURN
C
C - not enough space to save sensitive volume
C
 998  CALL ALTELL('AGMUCH: too many sensitive volumes ',0,'STOP')
C
      END
