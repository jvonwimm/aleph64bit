      SUBROUTINE AGTPCH
C-----------------------------------------------------------------
C! Implement TPC geometry
C   Author :B. Bloch-Devaux                       18 March 85
C.              M.E.Mermikides                    25/11/85
C.                    add  Detailed endplate geometry
C.               B. Bloch and M. Mermikides        sept. 87
C.                    Data base connection
C.               B.Bloch update materials for central membrane,supports
C.               gas mix and inner cage.              april 1991
C.               B.Bloch update material for gas mix august 1995
C.
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSTMED,GSVOLU,GSPOS  from  GEANT3
C.
C. -Stores extra Tracking Media needed
C. -Builds geometry levels from  'TPCR'  and downwards
C ----------------------------------------------------------------------
      EXTERNAL JHOCHA
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
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
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
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
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
      PARAMETER(JPMGID=1,JPMGVR=2,JPMGNA=4,JPMGRI=8,JPMGRO=9,JPMGZN=10,
     +          JPMGZX=11,JPMGPN=12,JPMGPX=13,JPMGNM=14,LPMG1A=14)
C
      PARAMETER (D52P5 = 52.5)
C
      PARAMETER (LTAB1=5, LTAB2=5,  LMED=5)
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2,LMED)
      REAL ATEP(2),ZTEP(2),WTEP(2)
      REAL ATEP2(2),ZTEP2(2),WTEP2(2)
C
C ======================================================================
C
C   Volumes:  TPC     Active volume       (TUBE) positioned in TPCR
C             TPWI    Inner wall of TPC   (TUBE)      "      "  "
C             TPWO    Outer wall of TPC   (TUBE)      "      "  "
C             TPEP    Endplate (x2)       (TUBE)      "      "  "
C             TPHF    Half active volume (x2)(TUBE)   "      "  "
C             TPIS    inner cage support ring(TUBE)   "      " TPHF
C             TPMB    central mylar membrane(*3)(TUBE)   "     TPHF
C             TPMI    central membrane inner support(TUBE)   " TPHF
C             TPMO    central membrane outer support(TUBE)   " TPHF
C
C      --->   TPFR    Endplate frame      (TUBE)      "      " TPEP
C     |       TPAV    Endframe material   (TUBE)      "      " TPEP
C     |       TPKA    K-Sector sub-shape  (TRD1)      "      " TPFR
C     |       TPKB     "     "      "     (TRD1)      "      "  "
C     |       TPKC     "     "      "     (TRD1)      "      " TPKA
C  Detail     TPMA    M-sector      "     (TRD1)      "      " TPFR
C  Level 2    TPMB     "            "     (TRD1)      "      "  "
C     |       TPMC     "            "     (TRD1)      "      "  "
C     |       TPMD     "            "     (TRD1)      "      "  "
C     |       TPWA    W-sector      "     (TRD1)      "      "  "
C     |       TPWB     "            "     (TRD1)      "      "  "
C     |       TPWC     "            "     (TRD1)      "      "  "
C      --->   TPWD     "            "     (TRD1)      "      "  "
C
C     Medium are defined as:
C    1- TPC inner wall volume           material: IMTWI
C    2- TPC active gas volume           material: IMTGA
C    3- TPC outer wall volume           material: IMTWO
C    4- TPC endplate volume             material: IMTEP   mixture
C    5- TPC endplate av. mat. vol.      material: IMTAV
C            (Level 2 geom)
C
C   For each medium the two following rows are filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1  0.1, 0.05 , 0.1 , 0.02 , .1 ,
     2  90., 0.2  , 0.2 , 0.02 , .1 ,
     3  0.1, 0.05 , 0.1 , 0.02 , .1 ,
     4  20., 0.10 , 0.1 , 0.2  ,  0.1 ,
     5  10., 0.05 , 0.1 , 0.1  ,  0.1 /
C
C        A    , Z    , Dens  , Radl  ,Absl
C  for inner wall,Rhoacell,outer wall,gas mixture(Ar-CH4)
      DATA TAB2 /
     1 18.175,8.869  ,0.929   ,34.84  ,114.66 ,
     2  6.7  ,3.6    ,0.07    ,805.8  ,1204.79,
     3 22.96 ,11.09  ,0.62    ,44.07  ,190.15 ,
C     4 38.774,17.492,1.684E-03,11866.9,79875.3,5*0./
C  Temperature and Pressure conditions in Geneva !!!
     4 38.774,17.492,1.504E-03,13284.,90390.,5*0./
C
C  Al/air mixture for TPC endplate frame section
C
      DATA NTEP,ATEP,ZTEP,WTEP,DTEP/
     1       2 ,26.98 ,14.61,
     2          13.0  ,7.3  ,
     3          .9973 ,0.0027 , 0.405     /
C  Mixture for detailed level
      DATA NTEP2, ATEP2, ZTEP2, WTEP2, DTEP2/
     1       2  , 26.98, 14.61,
     2            13.0,   7.3,
     3            .9992, .0008, .9305 /
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
C --------------------------------------------------------------------
      IAGMAT=IAGMAT+1
      IMTWI=IAGMAT
      CALL GSMATE(IMTWI,'TPC Inner cage matter',TAB2(1,1),TAB2(2,1),
     1            TAB2(3,1),TAB2(4,1),TAB2(5,1),0,0)
      IAGMAT=IAGMAT+1
      IMTRH = IAGMAT
      CALL GSMATE(IMTRH,'TPC Rohacell support',TAB2(1,2),TAB2(2,2),
     1            TAB2(3,2),TAB2(4,2),TAB2(5,2),0,0)
      IAGMAT=IAGMAT+1
      IMTWO=IAGMAT
      CALL GSMATE(IMTWO,'TPC Outer cage matter',TAB2(1,3),TAB2(2,3),
     1            TAB2(3,3),TAB2(4,3),TAB2(5,3),0,0)
      IAGMAT=IAGMAT+1
      IMTGA = IAGMAT
      CALL GSMATE(IMTGA,'TPC GAS mixture Ar/CH4',TAB2(1,4),TAB2(2,4),
     1            TAB2(3,4),TAB2(4,4),TAB2(5,4),0,0)
      IMTMY = 26
      IMTAV = 9
      MDAIR = 1
C =====================================================================
C
C              DEFINE TRACKING MEDIA
C
      IAGMED=IAGMED+1
      IMWAL = IAGMED
      CALL GSTMED(IMWAL,'TPC inner wall med',IMTWI,0,IAGFLD,ALFIEL,
     1         TAB1(1,1),TAB1(2,1),TAB1(3,1),TAB1(4,1),TAB1(5,1),0,0)
C
      IAGMED = IAGMED+1
      IMACT = IAGMED
      CALL GSTMED (IMACT,'TPC GAS medium',IMTGA,IDETJO(3),IAGFLD,ALFIEL
     &  , TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
C
C    Overall TPC volume
C
      PTAB(1)=AGLIMR(3)
      PTAB(2)=AGLIMR(5)
      PTAB(3)=AGLIMZ(1)
      CALL GSVOLU('TPCR','TUBE',1,PTAB,3,IVOL)
      CALL GSPOS('TPCR',1,'CDET',0.,0.,0.,0,'ONLY')
C
C  Active TPC volume, place in TPCR
C
      PTAB(1) = RTPCMN
      PTAB(2) = RTPCMX
      PTAB(3) = ZTPCMX
      CALL GSVOLU('TPC ','TUBE',IMACT, PTAB, 3, IVOL)
C
C  Divide TPC volume along HV plane to split track elements crossing it
C
      PTAB(1) = RTPCMN
      PTAB(2) = RTPCMX
      PTAB(3) = 0.5*ZTPCMX
      CALL GSVOLU('TPHF','TUBE',IMACT,PTAB,3,IVOL)
      Z = 0.5 * ZTPCMX
      CALL GSPOS('TPHF',2,'TPC ',0.,0.,Z,0,'ONLY')
      CALL GSPOS('TPHF',1,'TPC ',0.,0.,-Z,2,'ONLY')
C
C   Define where to find slot number
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1) = JHOCHA('TPHF')
      IAGSEN(IAGSLV,2) = 5
C
      CALL GSPOS('TPC ',1,'TPCR',0.,0.,0.,0,'ONLY')
C
C   Inner wall, place in TPCR
C
      PTAB(1)=RTPCMN-DRTPMN
      PTAB(2)=RTPCMN
      PTAB(3)=ZTPCMX
      CALL GSVOLU('TPWI','TUBE',IMWAL, PTAB, 3, IVOL)
      CALL GSPOS('TPWI',1,'TPCR',0.,0.,0., 0,  'ONLY')
C
C   OUTER WALL
C
      IAGMED=IAGMED+1
      CALL GSTMED (IAGMED,'TPC OUTER WALL med',IMTWO,0,IAGFLD,ALFIEL
     &  , TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
C
C  Outer wall, place in TPC
C
      PTAB(1)=RTPCMX
      PTAB(2)=RTPCMX+DRTPMX
      PTAB(3)=ZTPCMX
      CALL GSVOLU('TPWO','TUBE',IAGMED,PTAB,3,IVOL)
      CALL GSPOS('TPWO',1,'TPCR',0.,0.,0., 0,  'ONLY')
C
C   TPC inner cage supports
C
      IAGMED = IAGMED +1
      CALL GSTMED(IAGMED,'TPC INNERCAGE SUPPORT',IMTAV,0,IAGFLD,ALFIEL,
     $  TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      KPM = IW(NAMIND('PMG1'))
      IF (KPM.GT.0) THEN
          IF (LROWS(KPM).GT.80) THEN
             PTAB(1) = RTABL(KPM,81,JPMGRI)
             PTAB(2) = RTABL(KPM,81,JPMGRO)
             PTAB(3) = 0.5 *(ZTPCMX-RTABL(KPM,81,JPMGZN))
             CALL GSVOLU('TPIS','TUBE',IAGMED,PTAB,3,IVOL)
             Z = 0.5 * ZTPCMX-PTAB(3)
             CALL GSPOS('TPIS',1,'TPHF',0.,0.,Z,0,'ONLY')
          ENDIF
C
C  Central membrane elements
C
          IF (LROWS(KPM).GT.81) THEN
C   INNER SUPPORT RING  : rhoacell ( + some glass fiber and holes)
             IAGMED = IAGMED +1
             CALL GSTMED(IAGMED,'TPC Mylar inner sup',IMTRH,0,IAGFLD,
     $       ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,
     $       0)
             PTAB(1) = RTABL(KPM,82,JPMGRI)
             PTAB(2) = RTABL(KPM,82,JPMGRO)
             PTAB(3) = .25 *(RTABL(KPM,82,JPMGZX)-RTABL(KPM,82,JPMGZN))
             CALL GSVOLU('TPMI','TUBE',IAGMED,PTAB,3,IVOL)
             Z =PTAB(3)-0.5 *ZTPCMX
             CALL GSPOS('TPMI',1,'TPHF',0.,0.,Z,0,'ONLY')
C   OUTER SUPPORT RING  :Aluminum
             IAGMED = IAGMED +1
             CALL GSTMED(IAGMED,'TPC Mylar outer sup',IMTAV,0,IAGFLD,
     $       ALFIEL,TAB1(1,5),TAB1(2,5),TAB1(3,5),TAB1(4,5),TAB1(5,5),0,
     $       0)
             PTAB(1) = RTABL(KPM,83,JPMGRI)
             PTAB(2) = RTABL(KPM,83,JPMGRO)
             PTAB(3) = .25 *(RTABL(KPM,83,JPMGZX)-RTABL(KPM,83,JPMGZN))
             CALL GSVOLU('TPMO','TUBE',IAGMED,PTAB,3,IVOL)
             Z =PTAB(3)-0.5 *ZTPCMX
             CALL GSPOS('TPMO',1,'TPHF',0.,0.,Z,0,'ONLY')
C   MYLAR MEMBRANE
             IAGMED = IAGMED +1
             CALL GSTMED(IAGMED,'TPC MYLAR MEMBRANE ',IMTMY,0,IAGFLD,
     $       ALFIEL,TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,
     $       0)
             CALL GSVOLU('TPMB','TUBE',IAGMED,PTAB,0,IVOL)
             PTAB(1) = RTABL(KPM,84,JPMGRI)
             PTAB(2) = RTABL(KPM,84,JPMGRO)
             PTAB(3) = .25 *(RTABL(KPM,84,JPMGZX)-RTABL(KPM,84,JPMGZN))
             Z =PTAB(3)-0.5 *ZTPCMX
             CALL GSPOSP('TPMB',1,'TPHF',0.,0.,Z,0,'ONLY',PTAB,3)
C   INNER MYLAR REINFORCMENT
             PTAB(1) = RTABL(KPM,85,JPMGRI)
             PTAB(2) = RTABL(KPM,85,JPMGRO)
             PTAB(3) = .25 *(RTABL(KPM,85,JPMGZX)-RTABL(KPM,85,JPMGZN))
             Z =PTAB(3)-0.5 *ZTPCMX
             CALL GSPOSP('TPMB',2,'TPHF',0.,0.,Z,0,'ONLY',PTAB,3)
C   OUTER MYLAR REINFORCMENT
             PTAB(1) = RTABL(KPM,86,JPMGRI)
             PTAB(2) = RTABL(KPM,86,JPMGRO)
             PTAB(3) = .25 *(RTABL(KPM,86,JPMGZX)-RTABL(KPM,86,JPMGZN))
             Z =PTAB(3)-0.5 *ZTPCMX
             CALL GSPOSP('TPMB',3,'TPHF',0.,0.,Z,0,'ONLY',PTAB,3)
          ENDIF
      ENDIF
C
C  END PLATES
C
C   Simple ENDPLATE description, fill volume with average
C   air/aliminium mixture
C
      IAGMAT = IAGMAT+1
      IMTEP = IAGMAT
      CALL GSMIXT(IMTEP,'TPC ENDPLATE MATTER$',ATEP,ZTEP,DTEP,NTEP,WTEP)
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'TP ENDPLATE VOLUME$',IMTEP,0,IAGFLD,ALFIEL,
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
C
      PTAB(1)=RTPCMN-DRTPMN
      PTAB(2)=RTPCMX+DRTPMX
      PTAB(3) = DZTPMX *0.5
      CALL GSVOLU('TPEP','TUBE',IAGMED,PTAB,3,IVOL)
C
C  Place endplates in TPCR
C
      Z=ZTPCMX+DZTPMX/2.
      CALL GSPOS('TPEP',1,'TPCR',0.,0., Z , 0, 'ONLY')
      CALL GSPOS('TPEP',2,'TPCR',0.,0.,-Z , 2, 'ONLY')
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
C - the order of the CALLs is important because 'TPC' is inside 'TPCR'
C   CALL them from inside to outside
      CALL AGDIMP ('TPC ',4,'TPCO')
      CALL AGDIMP ('TPCR',3,'TPCI')
      GOTO 999
C
C - not enough space to save sensitive module
C
 998  CONTINUE
      CALL ALTELL('AGTPCH: too many sensitive volumes ',0,'STOP')
C
C - end
C
  999 CONTINUE
      END
