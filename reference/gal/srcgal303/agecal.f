      SUBROUTINE AGECAL
C-----------------------------------------------------
C!     Build e/gamma calorimeter geometry (Barrel and End-caps)
C
C    Author:B. Bloch-Devaux                  April 85
C                        modified sept 87 to use  Ecal volume package
C                        some more addition of material June 89
C                       when possible use automatic division to speed up
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSMATE,GSMIXT,GSTMED,GSVOLU,GSPOS,GSPOSP,GSROTM from  GEANT3
C.
C. -Stores extra material and mixtures required
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'ECBL' level for Barrel part
C. -Builds geometry levels below 'ECEA' and 'ECEB' for end-cap part
C.
C----------------------------------------------------
      EXTERNAL GHSIGM,JHOCHA
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
      PARAMETER (D30=2.*D15   , D60 = 2.*D30  )
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
C! Auxillary common for ECAL geometry
      COMMON/AGEAUX/EGRIEC,IEGROT
C! Indices for Ecal related materials
      COMMON/AGECMA/IEMALU,IEMAIR,IEMPVC,IEMBLA,IEMEAV,IEMB12,IEMBS3,
     1        IEMARB,IEMCS0,IEMC12,IEMCS3,IEMLAV,IEMRAV,IEMBAV,IEMCAV
      CHARACTER*4 ECN(2),ECP(2)
      PARAMETER (LTAB1=5    ,LMED=8)
      DIMENSION TAB1(LTAB1,LMED)
      DIMENSION PLAN(4,8),CORN(3,12)
      CHARACTER*16 VOLN(43)
      DATA VOLN/'B external      ','B internal      ','B sensitive     '
     1         ,'B base plate    ','B stack1        ','B stack2        '
     2         ,'B stack3        ','B stack12       ','B rail          '
     3         ,'B endplate 1    ','B endplate 2    ','E external      '
     4         ,'E internal      ','E sensitive     ','E front plate   '
     5         ,'E stack1        ','E stack2        ','E stack3        '
     6         ,'E stack12       ','E resin bag     ','E back plate    '
     7         ,'E grand external','E forward hole  ','E back hole     '
     8         ,'B positive end  ','B negative end  ','E stack0        '
     9         ,'E stack1 prime  ','Storey          ','B electr box 1  '
     &         ,'B electr box 2  ','B electr box 3  ','B electr box 4  '
     &         ,'E electr box 1  ','E elect box 2   ','E grand internal'
     &         ,'B PVC layer 1   ','B PVC layer 2   ','E Alu layer 1   '
     &         ,'E Alu layer 2   ','E grand resinbag','E grand forwhole'
     7         ,'E grand backhole'/
C
      DATA ECN/'ECEA','ECEB'/
      DATA ECP/'ECPA','ECPB'/
      DATA NPHI /12/
C
C =====================================================================
C
C     Medium are defined as:
C    1-EC barrel volume filled with air    material IEMAIR
C    2-EC aluminum frames volume           material IEMALU
C    3-EC passive average material         material IEMLAV side of petal
C                                    or    material IEMBLA side of barre
C                                    or    material IEMEAV end of barrel
C    4-EC stack 1+2 barrel medium          material: IEMB12
C    5-EC stack3 barrel medium             material: IEMBS3
C    6-EC stack 1+2 endcap medium          material: IEMC12
C    7-EC stack3 endcap medium             material: IEMCS3
C    8-EC pressure bag medium              material: IEMARB
C
C    For each medium the two following rows are filled:
C     TMXFD , DXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     2 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     3 20.    ,  0.5  , 0.1  , 0.02 , .05,
     4 20.    ,  0.5  , 0.1  , 0.1  , 1. ,
     5 20.    ,  0.5  , 0.1   , 0.1  , 0.5 ,
     6 20.    ,  0.5  , 0.1  , 0.1 , 1.,
     7 20.    ,  0.5  , 0.1   , 0.1  , 0.5  ,
     8 20.    ,  0.5  , 0.1   , 0.1  , 0.2  /
C ====================================================================
C
      ISV=IDETJO(4)
      RMX=AGLIMR(6)
C
C   Get the tilt value
C
      TILT=ECTILT(DUM)
C
C
C   There are two  possible geometry levels for now
C       -the 3 stacks are filled with an average material
C       stacks 1 and 2 being considered as a single volume including
C       the pvc separators.
C       - the three stacks are different volumes , the PVC sheet are
C        implemented separatly. Stack 1 and stack 2 are made of the same
C        average material as they have the same sampling.
C
      MD=0
      ITB=2
      ITC=1
      CALL EVOLPL(VOLN(1),ITB,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(1),LEP,PLAN,LEC,CORN)
C
C   First define the Overall Barrel ( AIR )
C
      DPHI = PIBY6
      COS15=COS(0.5*DPHI)
      PTAB(1)=ABS(CORN(2,1))
      PTAB(2)=RMX
      PTAB(3)=CORN(3,1)
      IAGMED=IAGMED+1
      IMEDR=IAGMED
      CALL GSTMED(IAGMED,'EC BARREL VOLUME$',IEMAIR,0,IAGFLD,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
      CALL GSVOLU('EBAL','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSPOS('EBAL',1,'ECBL',0.,0.,0.,0,'ONLY')
CBB  Try the division option
      C0 = TILT*RADEG-30.
      IF (TILT.LT.0.) C0 = TILT*RADEG
      CALL GSDVN2('EBAR','EBAL',NPHI,2,C0,IAGMED)
C
C  Define barrel module 'EBMO' as TRD1 made of aluminum
C
      PTAB(1) = CORN(1,3)
      PTAB(2) = CORN(1,7)
      PTAB(3) = CORN(3,1)
      PTAB(4) = 0.5*(CORN(2,1)-CORN(2,5))
      ZMID1=-0.5*(CORN(2,1)+CORN(2,5))
C
      IAGMED=IAGMED+1
      IMFRM=IAGMED
      CALL GSTMED(IAGMED,'EC ALU FRAMES   $',IEMALU,0,IAGFLD,ALFIEL,
     1         TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
C
      CALL GSVOLU('EBMO','TRD1', IAGMED, PTAB, 4,IVOL)
C
C  place corresponding rail
C
      CALL EVOLPL(VOLN(9),ITB,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(9),LEP,PLAN,LEC,CORN)
      PTAB(1) = CORN(1,3)
      PTAB(2) = CORN(1,7)
      PTAB(3) = CORN(3,1)
      PTAB(4) = 0.5*(CORN(2,1)-CORN(2,5))
      ZMID9=-0.5*(CORN(2,1)+CORN(2,5))
C
      CALL GSVOLU('EBRA','TRD1',IAGMED,PTAB,4,IVOL)
C
      IEGROT=IAGROT+1
      PHI = TILT-0.5*DPHI
      IF (TILT.LT.0.) PHI=PHI+DPHI
C      ROTATIONS FOR E/GAMMA MODULES
      IAGROT=IAGROT+1
      CALL GSROTM(IAGROT,D90,D90,0.,0.,D90,0.)
      CALL GSPOS('EBRA',1,'EBAR',ZMID9,0.,0.,IAGROT,'ONLY')
      CALL GSPOS('EBMO',1,'EBAR',ZMID1,0.,0.,IAGROT,'ONLY')
C
C    EBMO CONTENT:
C                   -Internal gas box leaving out Aluminum
C                     Front and Back plates and overall case
C
      CALL EVOLPL(VOLN(2),ITB,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(2),LEP,PLAN,LEC,CORN)
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'ECBL AV. PASSIVE MED$',IEMBLA,0,IAGFLD,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      PTAB(1) = CORN(1,3)
      PTAB(2) = CORN(1,7)
      PTAB(3) = CORN(3,1)
      PTAB(4) = 0.5*(CORN(2,1)-CORN(2,5))
      ZMID2=-0.5*(CORN(2,1)+CORN(2,5))
      CALL GSVOLU('EBIN','TRD1',IAGMED,PTAB,4,IVOL)
      YEBIN=PTAB(3)
      ZEBIN=PTAB(4)
      X=0.
      Y=0.
      Z=ZMID2-ZMID1
      CALL GSPOS('EBIN',1,'EBMO',X,Y,Z,0,'ONLY')
C
C   Now fill EBIN with:
C                         -END PART  at each edge
C                         - 3 Stacks with the following content
C                         made of an average material(light or dense)
C  *  stacks 1 & 2  considered as made of the same matter(light average)
C  *  stack  3  made of the matter dense average
C     or if Level 2 geometry required for ECAL :
C  *   stack 1 , stack 2 made of light average material
C  *   PVC sheets non sensitive
C  *   stack 3 made of dense average material
C
C   Define End Part of Barrel
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'EC BL ENDPART MEDIUM$',IEMEAV,0,IAGFLD,ALFIEL,
     &      TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
C
C     Get endpart  dimensions
C
      CALL EVOLPL(VOLN(25),ITB,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(25),LEP,PLAN,LEC,CORN)
      PTAB(1) = CORN(1,3)
      PTAB(2) = CORN(1,7)
      PTAB(3) = 0.5*(CORN(3,1)-CORN(3,2))
      PTAB(4) = 0.5*(CORN(2,1)-CORN(2,5))
      CALL GSVOLU('EBND','TRD1',IAGMED,PTAB,4,IVOL)
      X=0.
      Y=YEBIN-PTAB(3)
      Z=0.
      CALL GSPOS('EBND',1,'EBIN',X,Y,Z,0,'ONLY')
      CALL GSPOS('EBND',2,'EBIN',X,-Y,Z,3,'ONLY')
C
C     Get endplate dimensions
C
      CALL EVOLPL(VOLN(10),ITB,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(10),LEP,PLAN,LEC,CORN)
      PTAB(1) = ABS( CORN(1,1))
      PTAB(2)=0.5*(CORN(3,1)-CORN(3,2))
      PTAB(3)=ZEBIN
      CALL GSVOLU('EBEP','BOX ',IMFRM,PTAB,3,IVOL)
      X=0.
      Y=-Y+0.5*(CORN(3,1)+CORN(3,2))
      Z=0.
      CALL GSPOS('EBEP',1,'EBND',X,Y,Z,0,'ONLY')
C     Define PVC separator sheet with dimensions defined later
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'PVC STACK SEPARATOR $',IEMPVC,0,IAGFLD,
     1   ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
         CALL GSVOLU('EBSP','TRD1',IAGMED,PTAB,0,IVOL)
C      Define sensitive volume flag as defined if SET 'ECAL' is selected
C
         IAGMED=IAGMED+1
         CALL GSTMED(IAGMED,'EC STACK 1+2 BARREL$',IEMB12,ISV,IAGFLD,
     1   ALFIEL,TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
C     Get stack 1
         CALL EVOLPL(VOLN(5),ITB,MD,LEP,PLAN)
         CALL EVOLCR(VOLN(5),LEP,PLAN,LEC,CORN)
         PTAB(1)=CORN(1,3)
         PTAB(2)=CORN(1,7)
         PTAB(3)=CORN(3,1)
         PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
         CALL GSVOLU('EBS1','TRD1',IAGMED,PTAB,4,IVOL)
         X=0.
         Y=0.
         Z=-ZEBIN+PTAB(4)
         CALL GSPOS('EBS1',1,'EBIN',X,Y,Z,0,'ONLY')
         Z=Z+PTAB(4)
C     Get first PVC sheet
         CALL EVOLPL(VOLN(37),ITB,MD,LEP,PLAN)
         CALL EVOLCR(VOLN(37),LEP,PLAN,LEC,CORN)
C   Implement first PVC sheet
         PTAB(1)=CORN(1,3)
         PTAB(2)=CORN(1,7)
         PTAB(3)=CORN(3,1)
         PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
         Z=Z+PTAB(4)
         CALL GSPOSP('EBSP',1,'EBIN',X,Y,Z,0,'ONLY',PTAB,4)
         Z=Z+PTAB(4)
C     Get stack 2
         CALL EVOLPL(VOLN(6),ITB,MD,LEP,PLAN)
         CALL EVOLCR(VOLN(6),LEP,PLAN,LEC,CORN)
C   Implement stack 2
         PTAB(1)=CORN(1,3)
         PTAB(2)=CORN(1,7)
         PTAB(3)=CORN(3,1)
         PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
         CALL GSVOLU('EBS2','TRD1',IAGMED,PTAB,4,IVOL)
         X=0.
         Y=0.
         Z=Z+PTAB(4)
         CALL GSPOS('EBS2',1,'EBIN',X,Y,Z,0,'ONLY')
         Z=Z+PTAB(4)
C     Get second PVC sheet
         CALL EVOLPL(VOLN(38),ITB,MD,LEP,PLAN)
         CALL EVOLCR(VOLN(38),LEP,PLAN,LEC,CORN)
C   Implement second PVC sheet
         PTAB(1)=CORN(1,3)
         PTAB(2)=CORN(1,7)
         PTAB(3)=CORN(3,1)
         PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
         Z=Z+PTAB(4)
         CALL GSPOSP('EBSP',2,'EBIN',X,Y,Z,0,'ONLY',PTAB,4)
         Z=Z+PTAB(4)
C
C   Define where to find slot number
C
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('EBS1')
         IAGSEN(IAGSLV,2)=4
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('EBS2')
         IAGSEN(IAGSLV,2)=4
C     Get stack 3
      CALL EVOLPL(VOLN(7),ITB,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(7),LEP,PLAN,LEC,CORN)
      IAGMED=IAGMED+1
C Use same sensitive volume flag as selected by 'ECAL' SET card
      CALL GSTMED(IAGMED,'EC STACK 3 BARREL$',IEMBS3,ISV,IAGFLD,ALFIEL,
     1         TAB1(1,5),TAB1(2,5),TAB1(3,5),TAB1(4,5),TAB1(5,5),0,0)
      PTAB(1)=CORN(1,3)
      PTAB(2)=CORN(1,7)
      PTAB(3)=CORN(3,1)
      PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
      CALL GSVOLU('EBS3','TRD1',IAGMED,PTAB,4,IVOL)
      Z=ZEBIN-PTAB(4)
      CALL GSPOS('EBS3',1,'EBIN',0.,0.,Z,0,'ONLY')
C
C   Define where to find slot number
C
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('EBS3')
      IAGSEN(IAGSLV,2)=4
C
C   THEN DEFINE THE OVERALL END CAP( AIR )
C
      CALL EVOLPL(VOLN(22),ITC,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(22),LEP,PLAN,LEC,CORN)
      PTAB(1)=0.
      PTAB(2)=D360
      PTAB(3)=2
      PTAB(4)=CORN(3,2)
      PTAB(5)=ABS(CORN(2,1))
      PTAB(6)=ABS(CORN(2,5))/COS15
      PTAB(7)=CORN(3,1)
      PTAB(8)=PTAB(5)
      PTAB(9)=PTAB(6)
      EGRIEC=PTAB(6)
      CALL GSVOLU('ECPA','PCON',IMEDR,PTAB,9,IVOL)
      CALL GSVOLU('ECPB','PCON',IMEDR,PTAB,9,IVOL)
         X=0.
         Y=0.
         Z=0.
      DO  20 I=1,2
      DOFF=TILT*RADEG-D15
      IF(I.EQ.2) DOFF=-TILT*RADEG-D15
      IAGROT=IAGROT+1
      CALL GSROTM(IAGROT,D90,DOFF,D90,D90+DOFF,0.,0.)
      CALL GSPOS(ECP(I),1,ECN(I),X,Y,Z,IAGROT,'ONLY')
 20   CONTINUE
CBB  Try the phi division option
      CALL GSDVN('ECMA','ECPA',NPHI,2)
      CALL GSDVN('ECMB','ECPB',NPHI,2)
C
C  Define  end cap  module 'ECMO' as TRD1 made of aluminum
C
      PTAB(1) = CORN(1,3)
      PTAB(2) = CORN(1,7)
      PTAB(3) = 0.5*(CORN(3,1)-CORN(3,2))
      PTAB(4) = 0.5*(CORN(2,1)-CORN(2,5))
C
      CALL GSVOLU('ECMO','TRD1', IMFRM, PTAB, 4,IVOL)
C
CB PLACE one ECMO in each ECMA,ECMB with the appropriate rotation matrix
C
      YMI =-0.5*(CORN(2,1) + CORN(2,5))
      ZMI =0.5*(CORN(3,1)+CORN(3,2))
      IAGROT = IAGROT + 1
      CALL GSROTM(IAGROT,D90,D90, 0., 0., D90, 0.)
      CALL GSPOS('ECMO',1,'ECMA',YMI,0.,ZMI,IAGROT,'ONLY')
      CALL GSPOS('ECMO',1,'ECMB',YMI,0.,ZMI,IAGROT,'ONLY')
C
C    Carve out  front and back flanges with air
C
      CALL GSVOLU('ECFX','TRD1',IMEDR,PTAB,0,IVOL)
C
      CALL EVOLPL(VOLN(42),ITC,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(42),LEP,PLAN,LEC,CORN)
C
      PTAB(1)=CORN(1,3)
      PTAB(2)=CORN(1,7)
      PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
      PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
      X=0.
      Z=0.
      Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
      CALL GSPOSP('ECFX',1,'ECMO',X,Y,Z,0,'ONLY',PTAB,4)
      CALL EVOLPL(VOLN(43),ITC,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(43),LEP,PLAN,LEC,CORN)
      CALL GSVOLU('ECBX','TRD1',IMEDR,PTAB,0,IVOL)
C
      PTAB(1)=CORN(1,3)
      PTAB(2)=CORN(1,7)
      PTAB(3) = 0.5*ABS( CORN(3,2) - CORN(3,1) )
      PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
      X=0.
      Z=0.
      Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
      CALL GSPOSP('ECBX',1,'ECMO',X,Y,Z,0,'ONLY',PTAB,4)
C     get internal part of module
      CALL EVOLPL(VOLN(36),ITC,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(36),LEP,PLAN,LEC,CORN)
C     store some quantities for later use

      X1=CORN(1,3)
      X2 = X1
      Y2=CORN(3,1)
      Z11=-CORN(2,1)
C
C     Fill ECMO with an average material
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'EC LEFT PASSIVE MIX$',IEMCAV,0,IAGFLD,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      PTAB(1)=CORN(1,3)
      PTAB(2)=CORN(1,7)
      PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
      PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
      CALL GSVOLU('ECIN','TRD1',IAGMED,PTAB,4,IVOL)
      X=0.
      Z=-0.5*(CORN(2,1)+CORN(2,5))-YMI
      Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
      CALL GSPOS('ECIN',1,'ECMO',X,Y,Z,0,'ONLY')
      YMI=-0.5*(CORN(2,1)+CORN(2,5))
      ZMI=0.5*(CORN(3,1)+CORN(3,2))
C
C     Implement pressure bag
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'EC ENDCAP RESINE BAG$',IEMARB,0,IAGFLD,ALFIEL,
     1    TAB1(1,8),TAB1(2,8),TAB1(3,8),TAB1(4,8),TAB1(5,8),0,0)
      CALL EVOLPL(VOLN(41),ITC,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(41),LEP,PLAN,LEC,CORN)
C   Store some more quantities for later use
      Y1=CORN(3,1)
C
      PTAB(1)=CORN(1,3)
      PTAB(2)=CORN(1,7)
      PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
      PTAB(4)=0.5*(CORN(2,1)-CORN(2,5))
      CALL GSVOLU('ECRB','TRD1',IAGMED,PTAB,4,IVOL)
      X=0.
      Z=0.
      Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
      CALL GSPOS('ECRB',1,'ECIN',X,Y,Z,0,'ONLY')
C
C *** NOW FILL ECIN WITH STACKS 1+2 AND 3
C
      IAGMED=IAGMED+1
      IMST1=IAGMED
      CALL GSTMED(IAGMED,'EC STACK 1+2 ENDCAP$',IEMC12,ISV,IAGFLD,ALFIEL
     1        ,TAB1(1,6),TAB1(2,6),TAB1(3,6),TAB1(4,6),TAB1(5,6),0,0)
C     Define ALU separator sheet
          IAGMED=IAGMED+1
          CALL GSTMED(IAGMED,'ALU STACK SEPARATOR $',IEMALU,0,IAGFLD,
     1    ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
          CALL GSVOLU('ECSP','TRD1',IAGMED,PTAB,0,IVOL)
          IAGMED=IAGMED+1
          CALL GSTMED(IAGMED,'EC STACK 0 ENDCAP$',IEMCS0,ISV,IAGFLD,
     1    ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
C     Get stack 0
          CALL EVOLPL(VOLN(27),ITC,MD,LEP,PLAN)
          CALL EVOLCR(VOLN(27),LEP,PLAN,LEC,CORN)
          PTAB(1)=0.5*(CORN(1,3)-CORN(1,1))
          PTAB(2)=0.5*(CORN(1,7)-CORN(1,5))
          PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
          PTAB(4)=0.5*(CORN(2,1)-0.5*(CORN(2,5)+CORN(2,7)))
          CALL GSVOLU('ECS0','TRD1',IAGMED,PTAB,0,IVOL)
          CALL GSVOLU('EDS0','TRD1',IAGMED,PTAB,0,IVOL)
C position the two pieces with proper dimensions
          X=0.5*(CORN(1,1)+CORN(1,3))
          Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
          Z=-0.5*(CORN(2,1)+0.5*(CORN(2,5)+CORN(2,7)))-YMI
          CALL GSPOSP('ECS0',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
          PTAB(1)=PTAB(2)
          PTAB(2)=0.5*(CORN(1,11)-CORN(1,9))
          PTAB(4)=0.5*(0.5*(CORN(2,5)+CORN(2,7))-CORN(2,9))
          Z=-0.5*(0.5*(CORN(2,5)+CORN(2,7))+CORN(2,9))-YMI
          CALL GSPOSP('EDS0',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
C                         Get stack 1
          CALL EVOLPL(VOLN(28),ITC,MD,LEP,PLAN)
          CALL EVOLCR(VOLN(28),LEP,PLAN,LEC,CORN)
          CALL GSVOLU('ECS1','TRD1',IMST1 ,PTAB,0,IVOL)
          CALL GSVOLU('EDS1','TRD1',IMST1 ,PTAB,0,IVOL)
          PTAB(1)=0.5*(CORN(1,3)-CORN(1,1))
          PTAB(2)=0.5*(CORN(1,7)-CORN(1,5))
          PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
          PTAB(4)=0.5*(CORN(2,1)-0.5*(CORN(2,5)+CORN(2,7)))
          X=0.5*(CORN(1,1)+CORN(1,3))
          Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
          Z=-0.5*(CORN(2,1)+0.5*(CORN(2,5)+CORN(2,7)))-YMI
          CALL GSPOSP('ECS1',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
          Z1=Z
          PTAB(1)=PTAB(2)
          PTAB(2)=0.5*(CORN(1,11)-CORN(1,9))
          PTAB(4)=0.5*(0.5*(CORN(2,5)+CORN(2,7))-CORN(2,9))
          Z=-0.5*(0.5*(CORN(2,5)+CORN(2,7))+CORN(2,9))-YMI
          Z2=Z
          CALL GSPOSP('EDS1',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
C     Get first Alu sheet
          CALL EVOLPL(VOLN(39),ITC,MD,LEP,PLAN)
          CALL EVOLCR(VOLN(39),LEP,PLAN,LEC,CORN)
C   Implement first ALU  separator
          PTAB(1)=0.5*(CORN(1,3)-CORN(1,1))
          PTAB(2)=0.5*(CORN(1,7)-CORN(1,5))
          PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
          PTAB(4)=0.5*(CORN(2,1)-0.5*(CORN(2,5)+CORN(2,7)))
          X=0.5*(CORN(1,1)+CORN(1,3))
          Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
          Z=-0.5*(CORN(2,1)+0.5*(CORN(2,5)+CORN(2,7)))-YMI
          CALL GSPOSP('ECSP',1,'ECIN',X,Y,Z1,0,'ONLY',PTAB,4)
          PTAB(1)=PTAB(2)
          PTAB(2)=0.5*(CORN(1,11)-CORN(1,9))
          PTAB(4)=0.5*(0.5*(CORN(2,5)+CORN(2,7))-CORN(2,9))
          Z=-0.5*(0.5*(CORN(2,5)+CORN(2,7))+CORN(2,9))-YMI
          CALL GSPOSP('ECSP',2,'ECIN',X,Y,Z2,0,'ONLY',PTAB,4)
C     Get stack 2
          CALL EVOLPL(VOLN(17),ITC,MD,LEP,PLAN)
          CALL EVOLCR(VOLN(17),LEP,PLAN,LEC,CORN)
C      implement stack 2
          CALL GSVOLU('ECS2','TRD1',IMST1 ,PTAB,0,IVOL)
          CALL GSVOLU('EDS2','TRD1',IMST1 ,PTAB,0,IVOL)
          PTAB(1)=0.5*(CORN(1,3)-CORN(1,1))
          PTAB(2)=0.5*(CORN(1,7)-CORN(1,5))
          PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
          PTAB(4)=0.5*(CORN(2,1)-0.5*(CORN(2,5)+CORN(2,7)))
          X=0.5*(CORN(1,1)+CORN(1,3))
          Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
          Z=-0.5*(CORN(2,1)+0.5*(CORN(2,5)+CORN(2,7)))-YMI
          CALL GSPOSP('ECS2',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
          PTAB(1)=PTAB(2)
          PTAB(2)=0.5*(CORN(1,11)-CORN(1,9))
          PTAB(4)=0.5*(0.5*(CORN(2,5)+CORN(2,7))-CORN(2,9))
          Z=-0.5*(0.5*(CORN(2,5)+CORN(2,7))+CORN(2,9))-YMI
          CALL GSPOSP('EDS2',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
C     Get second Alu sheet
         CALL EVOLPL(VOLN(40),ITC,MD,LEP,PLAN)
         CALL EVOLCR(VOLN(40),LEP,PLAN,LEC,CORN)
C   Implement second ALU  separator
         PTAB(1)=0.5*(CORN(1,3)-CORN(1,1))
         PTAB(2)=0.5*(CORN(1,7)-CORN(1,5))
         PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
         PTAB(4)=0.5*(CORN(2,1)-0.5*(CORN(2,5)+CORN(2,7)))
         X=0.5*(CORN(1,1)+CORN(1,3))
         Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
         Z=-0.5*(CORN(2,1)+0.5*(CORN(2,5)+CORN(2,7)))-YMI
         CALL GSPOSP('ECSP',3,'ECIN',X,Y,Z1,0,'ONLY',PTAB,4)
         PTAB(1)=PTAB(2)
         PTAB(2)=0.5*(CORN(1,11)-CORN(1,9))
         PTAB(4)=0.5*(0.5*(CORN(2,5)+CORN(2,7))-CORN(2,9))
         Z=-0.5*(0.5*(CORN(2,5)+CORN(2,7))+CORN(2,9))-YMI
         CALL GSPOSP('ECSP',4,'ECIN',X,Y,Z2,0,'ONLY',PTAB,4)
C
C   Define where to find slot number
C
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('ECS0')
         IAGSEN(IAGSLV,2)=4
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('ECS1')
         IAGSEN(IAGSLV,2)=4
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('ECS2')
         IAGSEN(IAGSLV,2)=4
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('EDS0')
         IAGSEN(IAGSLV,2)=4
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('EDS1')
         IAGSEN(IAGSLV,2)=4
         IF (IAGSLV.GE.LSENV) GOTO 998
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('EDS2')
         IAGSEN(IAGSLV,2)=4
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'EC STACK 3 ENDCAP$',IEMCS3,ISV,IAGFLD,ALFIEL,
     1         TAB1(1,7),TAB1(2,7),TAB1(3,7),TAB1(4,7),TAB1(5,7),0,0)
C     Define parameters later at positioning time as stack
C   is made of 2 pieces
C
      CALL GSVOLU('ECS3','TRD1',IAGMED,PTAB,0,IVOL)
      CALL GSVOLU('EDS3','TRD1',IAGMED,PTAB,0,IVOL)
C
C   Define where to find slot number
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('ECS3')
      IAGSEN(IAGSLV,2)=4
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('EDS3')
      IAGSEN(IAGSLV,2)=4
C                         Get stack 3
      CALL EVOLPL(VOLN(18),ITC,MD,LEP,PLAN)
      CALL EVOLCR(VOLN(18),LEP,PLAN,LEC,CORN)
C     Store some more quantities for later use
      Z22=-CORN(2,1)
      PTAB(1)=0.5*(CORN(1,3)-CORN(1,1))
      PTAB(2)=0.5*(CORN(1,7)-CORN(1,5))
      PTAB(3)=0.5*(CORN(3,1)-CORN(3,2))
      PTAB(4)=0.5*(CORN(2,1)-0.5*(CORN(2,5)+CORN(2,7)))
      X=0.5*(CORN(1,1)+CORN(1,3))
      Y=0.5*(CORN(3,1)+CORN(3,2))-ZMI
      Z=-0.5*(CORN(2,1)+0.5*(CORN(2,5)+CORN(2,7)))-YMI
      CALL GSPOSP('ECS3',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
      PTAB(1)=PTAB(2)
      PTAB(2)=0.5*(CORN(1,11)-CORN(1,9))
      PTAB(4)=0.5*(0.5*(CORN(2,5)+CORN(2,7))-CORN(2,9))
      Z=-0.5*(0.5*(CORN(2,5)+CORN(2,7))+CORN(2,9))-YMI
      CALL GSPOSP('EDS3',1,'ECIN',X,Y,Z,0,'ONLY',PTAB,4)
C   Add heavier material at narrow end of petal
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'EC NARROW PASSIVMIX$',IEMBAV,0,IAGFLD,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      PTAB(1)=X1
      PTAB(2)=X2
      PTAB(3)=0.5*(Y2-Y1)
      PTAB(4)=0.5*(Z22-Z11)
      CALL GSVOLU('ECNE','TRD1',IAGMED,PTAB,4,IVOL)
      X=0.
      Z=0.5*(Z22+Z11)-YMI
      Y=0.5*(Y1+Y2)-ZMI
      CALL GSPOS('ECNE',1,'ECIN',X,Y,Z,0,'ONLY')
C
C   Implement barrel electronic boxes and passive material
C   if requested
C
      IF (IGEOJO(12).GT.0) THEN
         CALL AGECPM(1)
      ENDIF
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
        CALL AGDIMP('ECBL',2,'ECAL')
        CALL AGDIMP('ECPA',3,'ECAL')
        CALL AGDIMP('ECPB',3,'ECAL')
      GOTO 999
C
C - not enough space to save sensitive module
C
 998  CONTINUE
      CALL ALTELL('AGECAL: too many sensitive volumes ',0,'STOP')
C
C - end
C
 999  CONTINUE
      RETURN
      END
