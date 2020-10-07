*DK aghcal
      SUBROUTINE AGHCAL
C----------------------------------------------------------
C! Implement   Hadron calorimeter geometry
C   Author :        B. Bloch-Devaux                    18 march 85
C                   modified for Data Base access     September 87
C                    suppress HCDETAIL / HCAVERAG     October 88
C        Modified by L.Silvestris   5/9/90  (mixture gas changed)
C        Modified by B.Bloch       15/5/91  for improved geometry
C        Modified L.Silvestris     10/10/91 for geant3.14
C        Modified F.Ranjard        10/10/91 get new cuts from data base
C                                           depending on GEANT version
C - modified by : L.Silvestris - 900905
C                 F.Ranjard    - 911002
C                 change COIL component # to 13 instead of 9
C
C        Modify by L.Silvestris   5/9/90  (mixture gas changed)
C        Modify by B.Bloch       15/5/91  for improved geometry
C        Modify by L.Silvestris  16/5/93 (improved materials)
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSMATE,GSTMED,GSVOLU,GSPOS,GSROTM from  GEANT3
C.
C. -Stores extra material  required
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'HCBL' level for Barrel part
C. -Builds geometry levels below 'HCEA' and 'HCEB' for end-cap part
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
      EXTERNAL GHSIGM,JHOCHA
#include "hgeajj.h"
#include "iocom.h"
#include "bcs.h"
#include "hcgega.h"
#include "hccong.h"
#include "hccoun.h"
#include "agcons.h"
#include "agecom.h"
#include "alfgeo.h"
#include "jobcom.h"
#include "agpmma.h"
      PARAMETER (LTAB1=5   ,LMED=4)
      DIMENSION TAB1(LTAB1,LMED)
      DIMENSION AISO(2),ZISO(2),WISO(2)
C - cuts adapted at GEANT 3.13
      DATA   CUTG,  CUTE,  CUTH,  CUTN,   CUTM,   BCUT,   PCUT,  DCUT
     &    / 0.0004,0.0004, 0.008, 0.010,  0.03,  0.03,   0.01,  0.03 /
C     Define parameters for Isobutane Hcal gas
      DATA AISO/12.01,1.01/, ZISO/6.,1./
      DATA WISO/0.826,0.174/, DISO/0.00267/, NISO/2/
C     air gap in streamer tube
      DATA AIRGP/0.75/
C.
C   Streamer gap is made of mixture,PVC walls , pad and strip planes
C                  and an air gap
C
C =====================================================================
C     Medium are defined as:
C    1- HCAL calo volume           material: air IMHRG
C    2- HCAL Iron-Air non sensitive volume material :
C            average matter IMHAV in the detailed geometry (barrel)
C    3- HCAL Iron-Air non sensitive volume material :
C            average matter IMHAC in the detailed geometry (end-caps)
C    4- HCAL  sensitive volume in streamer tubes  material :IMHSE
C
C   For each medium the two following rows are filled:
C                 TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1             0.   ,  0.5  ,  0.1  ,  0.1  ,  1.   ,
     2             1.   ,  0.1  ,  0.2  ,  0.1  ,  0.1  ,
     3             1.   ,  0.1  ,  0.2  ,  0.1  ,  0.1  ,
     4             1.   ,  0.1  ,  0.2  ,  0.1  ,  0.1  /
C
#include "bmacro.h"
C--------------------------------------------------------------------
C
C - get cuts valid for the GEANT version used from the data base
C
      JHGEA  = IW(NAMIND('HGEA'))
      IF (JHGEA.EQ.0) THEN
C     use GEANT 3.13 cuts given in data statment
      ELSE
         CUTE = RTABL(JHGEA,1,JHGECE)
         CUTG = RTABL(JHGEA,1,JHGECG)
         CUTM = RTABL(JHGEA,1,JHGECM)
         CUTN = RTABL(JHGEA,1,JHGECN)
         CUTH = RTABL(JHGEA,1,JHGECH)
         BCUT = RTABL(JHGEA,1,JHGEBM)
         PCUT = RTABL(JHGEA,1,JHGEPM)
         DCUT = RTABL(JHGEA,1,JHGEDM)
      ENDIF
C     Define some materials
      IMALL = 9
      IMCAR = 6
      IMHYD = 1
      IMARG = 19
      IMCO2 = 24
      IMHRG = 15
      IMPVC = 25
      IMMYL = 26
      IMCOP = 11
      IMG10 = 27
C ===================================================================
C
C     Define materials
C
C     Define HCAL Isobutane
      IAGMAT=IAGMAT+1
      IMISO=IAGMAT
C
        CALL GSMIXT(IMISO,'HCAL ISOBUTANE$',AISO,ZISO,DISO,NISO,WISO)
C
C     Define HCAL iron
      IAGMAT=IAGMAT+1
      IMIRO=IAGMAT
C
        CALL GSMATE(IMIRO,'HCAL IRON $',55.847,26.0,7.87,1.76,17.1,0,0)
C
C     Define HCAL nichel
      IAGMAT=IAGMAT+1
      IMNIC=IAGMAT
C
        CALL GSMATE(IMNIC,'HCAL NICHEL$',58.69,28.0,8.91,1.46,15.3,0,0)
C
C     Define HCAL manganese
      IAGMAT=IAGMAT+1
      IMMAN=IAGMAT
C
        CALL GSMATE(IMMAN,'HCAL MANGANE$',54.94,25.0,7.43,2.01,17.9,0,0)
C
C     Define HCAL P (Fosforo)
      IAGMAT=IAGMAT+1
      IMFOS=IAGMAT
C
        CALL GSMATE(IMFOS,'HCAL FOS $',30.97,15.0,1.82,11.8,60.4,0,0)
C
C     Define HCAL S (Zolfo)
      IAGMAT=IAGMAT+1
      IMSUL=IAGMAT
C
        CALL GSMATE(IMSUL,'HCAL Solfo $',32.07,16.0,2.07,9.55,53.7,0,0)
C
C
C     Define Hcal iron (A37) mixture
      IAGMAT=IAGMAT+1
      IMA37=IAGMAT
C
      JTAB(1) = IMIRO
      JTAB(2) = IMCAR
      JTAB(3) = IMFOS
      JTAB(4) = IMSUL
      JTAB(5) = IMNIC
      JTAB(6) = IMMAN
      PTAB(11) = 0.9888
      PTAB(12) = 0.0020
      PTAB(13) = 0.0007
      PTAB(14) = 0.0005
      PTAB(15) = 0.005
      PTAB(16) = 0.003
      NAG = 6
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENSD)
        CALL GSMIXT(IMA37,'HCAL A37 MIXTURE$',PTAB(21),PTAB(31),
     +                     DENSD , NAG,PTAB(41))
C
C     Define Hcal streamer tube mixture
      IAGMAT=IAGMAT+1
      IMGAS=IAGMAT
C
      JTAB(1) = IMISO
      JTAB(2) = IMARG
      JTAB(3) = IMCO2
      PTAB(11) = 0.30
      PTAB(12) = 0.225
      PTAB(13) = 0.475
      NAG = 3
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENSD)
        CALL GSMIXT(IMGAS,'HCAL GAS MIXTURE$',PTAB(21),PTAB(31),
     +                     DENSD , NAG,PTAB(41))
C
C     Define tube streamer structure (2.2cm of mylar,PVC,vetronite...)
      IAGMAT=IAGMAT+1
      IMMIX=IAGMAT
C
      JTAB(1) = IMMYL
      JTAB(2) = IMCOP
      JTAB(3) = IMG10
      JTAB(4) = IMPVC
      JTAB(5) = IMALL
      PTAB(11) = 0.02
      PTAB(12) = 0.01
      PTAB(13) = 0.1
      PTAB(14) = 0.4
      PTAB(15) = 0.02
      NAG = 5
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENSD)
        CALL GSMIXT(IMMIX,'HCAL MIXTURE MATE',PTAB(21),PTAB(31),
     +                     DENSD , NAG,PTAB(41))
C
C     Define HCAL sensitive material  (Streamer tube)
      IAGMAT=IAGMAT+1
      IMHSE=IAGMAT
C
      JTAB(1) = IMMIX
      JTAB(2) = IMGAS
      PTAB(11) = 0.55
      PTAB(12) = 0.90
      NAG = 2
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENSD)
        CALL GSMIXT(IMHSE,'HCAL SENS. MATERIAL$',PTAB(21),PTAB(31),
     +                     DENSD , NAG,PTAB(41))
C
C     Define HCAL  barrel passive material
      IAGMAT=IAGMAT+1
      IMHAV = IAGMAT
C
      JTAB(1) = IMHRG
      JTAB(2) = IMA37
      PTAB(11)= LHCNL*AIRGP
      PTAB(12)= (LHCNL-1)*HCIRTH + HCTIRF(2)
      NAG = 2
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENSD)
        CALL GSMIXT(IMHAV,'HCBL PASSIVE MATERIAL$',PTAB(21),PTAB(31),
     +                     DENSD , NAG,PTAB(41))
C
C     Define HCAL  average material
      IAGMAT=IAGMAT+1
      IMAVE = IAGMAT
C
      JTAB(1) = IMHSE
      JTAB(2) = IMHAV
      PTAB(11)= LHCNL*HSTREA
      PTAB(12)= (LHCNL-1)*(HCIRTH+7.5) + (HCTIRF(2)+7.5)
      NAG = 2
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENSD)
        CALL GSMIXT(IMAVE,'HCBL AVERAGE MATERIAL$',PTAB(21),PTAB(31),
     +                     DENSD , NAG,PTAB(41))
C
C     Define HCAL end-caps passive material
      IAGMAT=IAGMAT+1
      IMHAC=IAGMAT
C
      JTAB(1) = IMHRG
      JTAB(2) = IMA37
      PTAB(11)= (LHCNL-1)*AIRGP
      PTAB(12)= (LHCNL-1)*HCIRTH
      NAG = 2
C
        CALL AGMIX(NAG,JTAB(1),PTAB(11),PTAB(21),PTAB(31),
     +                 PTAB(41),DENCD)
        CALL GSMIXT(IMHAC,'HCAP AVERAGE MATTER$',PTAB(21),PTAB(31),
     +                     DENCD , NAG,PTAB(41))
C
C     Define some tracking media
C
C     Define Hcal sensitive tracking media
      IAGMED = IAGMED + 1
      IMHSN = IAGMED
      ISV = IDETJO(7)
C   Remember there is no field in the streamer gap
        CALL GSTMED(IMHSN,'HCAL HSTREAMER TUBES$',IMHSE,ISV,IAGFLO,
     +                     ALFIEL,TAB1(1,4),TAB1(2,4),TAB1(3,4),
     +                     TAB1(4,4),TAB1(5,4),0,0)
C
C     Define Hcal barrel non sensitive tracking media
      IAGMED = IAGMED + 1
      IMHBL= IAGMED
C
        CALL GSTMED(IMHBL,'HCBL NON SENSITIVE$',IMHAV,0,IAGFLI,ALFIEL,
     +                     TAB1(1,2),TAB1(2,2),TAB1(3,2),
     +                     TAB1(4,2),TAB1(5,2),0,0)
C
C     Define tracking media for the last plane
      IAGMED=IAGMED+1
      IMEDI=IAGMED
C
        CALL GSTMED(IMEDI,'LAST IRON PLATE MEDIUM$',IMIRO,0,IAGFLI,
     +                     ALFIEL,TAB1(1,2),TAB1(2,2),TAB1(3,2),
     +                     TAB1(4,2),TAB1(5,1),0,0)
C
C   Overall barrel module including first muon plane
      IAGMED=IAGMED+1
      IMHNS=IAGMED
C
        CALL GSTMED(IMHNS,'HCAL CALO VOLUME$',IMHRG,0,IAGFLO,ALFIEL,
     +                     TAB1(1,1),TAB1(2,1),TAB1(3,1),
     +                     TAB1(4,1),TAB1(5,1),0,0)
C
C    notch #1 is filled mainly by TPC cables or equivalent
      IAGMED = IAGMED+1
      INOTCH= IAGMED
C
        CALL GSTMED(INOTCH,'CABLES IN NOTCHES $',IPMTCA,0,IAGFLI,
     +                      ALFIEL,TAB1(1,2),TAB1(2,2),TAB1(3,2),
     +                      TAB1(4,2),TAB1(5,2),0,0)
C     Define Hcal endcaps non sensitive tracking media
      IAGMED = IAGMED + 1
      IMHEN= IAGMED
C
        CALL GSTMED(IMHEN,'HCAP NON SENSITIVE$',IMHAC,0,IAGFLI,ALFIEL,
     +                     TAB1(1,3),TAB1(2,3),TAB1(3,3),
     +                     TAB1(4,3),TAB1(5,3),0,0)
C
C   Modify tracking cuts in the Barrel and endcaps
C
C     modify cut for gammas in all tracking media
      CALL GSTPAR(IMHBL,'CUTGAM',CUTG)
      CALL GSTPAR(IMHEN,'CUTGAM',CUTG)
      CALL GSTPAR(IMHSN,'CUTGAM',CUTG)
      CALL GSTPAR(IMEDI,'CUTGAM',CUTG)
C     modify cut for electrons in all tracking media
      CALL GSTPAR(IMHBL,'CUTELE',CUTE)
      CALL GSTPAR(IMHEN,'CUTELE',CUTE)
      CALL GSTPAR(IMHSN,'CUTELE',CUTE)
      CALL GSTPAR(IMEDI,'CUTELE',CUTE)
C     modify cut muon and hadron bremm in all tracking media
      CALL GSTPAR(IMHBL,'BCUTM',BCUT)
      CALL GSTPAR(IMHEN,'BCUTM',BCUT)
      CALL GSTPAR(IMHSN,'BCUTM',BCUT)
      CALL GSTPAR(IMEDI,'BCUTM',BCUT)
C     modify cut total energy for pair production by muons
C     in all tracking media
      CALL GSTPAR(IMHBL,'PPCUTM',PCUT)
      CALL GSTPAR(IMHEN,'PPCUTM',PCUT)
      CALL GSTPAR(IMHSN,'PPCUTM',PCUT)
      CALL GSTPAR(IMEDI,'PPCUTM',PCUT)
C     modify cut for delta rays by muons in all tracking media
      CALL GSTPAR(IMHBL,'DCUTM',DCUT)
      CALL GSTPAR(IMHEN,'DCUTM',DCUT)
      CALL GSTPAR(IMHSN,'DCUTM',DCUT)
      CALL GSTPAR(IMEDI,'DCUTM',DCUT)
C     modify cut for charged hadrons in all tracking media
      CALL GSTPAR(IMHBL,'CUTHAD',CUTH)
      CALL GSTPAR(IMHEN,'CUTHAD',CUTH)
      CALL GSTPAR(IMHSN,'CUTHAD',CUTH)
      CALL GSTPAR(IMEDI,'CUTHAD',CUTH)
C     modify cut for neutral hadrons in all tracking media
      CALL GSTPAR(IMHBL,'CUTNEU',CUTN)
      CALL GSTPAR(IMHEN,'CUTNEU',CUTN)
      CALL GSTPAR(IMHSN,'CUTNEU',CUTN)
      CALL GSTPAR(IMEDI,'CUTNEU',CUTN)
C     modify cut for muons in all tracking media
      CALL GSTPAR(IMHBL,'CUTMUO',CUTM)
      CALL GSTPAR(IMHEN,'CUTMUO',CUTM)
      CALL GSTPAR(IMHSN,'CUTMUO',CUTM)
      CALL GSTPAR(IMEDI,'CUTMUO',CUTM)
C     modify energy loss flag in all tracking media
C     CALL GSTPAR(IMHBL,'LOSS',2.)
C     CALL GSTPAR(IMHEN,'LOSS',2.)
C     CALL GSTPAR(IMHSN,'LOSS',2.)
C     CALL GSTPAR(IMEDI,'LOSS',2.)
C
C     Definitions of barrel
C
      LHSEC=LPHCBM/2
      DPHI=TWOPI/LHSEC
      COSX=COS(0.5*DPHI)
      TPHB2=TAN(0.5*DPHI)
      PHI0 =  HCPHOF
C   BARREL 'RIGHT' IS A TRAP
C
      PTAB(1)=HCZMAX(1)
      PTAB(2)=0.
      PTAB(3)=0.
      PTAB(4)=0.5*(HCRMAX(1)-HCLSLA-HCRMIN(1))
      PTAB(5)=0.5*HCRMIN(1)*TPHB2
      PTAB(6)=0.5*(HCRMAX(1)-HCLSLA)*TPHB2
      PTAB(7)=D7P5
      PTAB(8)=PTAB(4)
      PTAB(9)=PTAB(5)
      PTAB(10)=PTAB(6)
      PTAB(11)=PTAB(7)
      XMID=0.5*(PTAB(5)+PTAB(6))
      YMID = PTAB(4)
      CALL GSVOLU('HBMO','TRAP',IMHBL,PTAB,11,IVOL)
      Z = 0.
      DPHB4=0.25*DPHI
      COS15=COS(DPHB4)
      R =( HCRMIN(1) + PTAB(4))/COS15
      PTAB(1)=HCRMIN(1)
      PTAB(2)=AGLIMR(8)
      PTAB(3)=HCZMAX(1)
      CALL GSVOLU('HBAL','TUBE',IMHNS,PTAB,3,IVOL)
      CALL GSPOS('HBAL',1,'HCBL',0.,0.,0.,0,'ONLY')
CBB  try the division option
      PHIOF= (PHI0-0.5*DPHI)*RADEG
      CALL GSDVN2('HBAR','HBAL',LHSEC,2,PHIOF,IMHNS)
C
C   Last iron plate
C
      PTAB(1)=HCZMAX(1)
      PTAB(2)=0.
      PTAB(3)=0.
      PTAB(4)=0.5*HCLSLA
      PTAB(5)=0.5*(HCRMAX(1)-HCLSLA)*TPHB2
      PTAB(6)=0.5*HCRMAX(1)*TPHB2
      PTAB(7)=D7P5
      PTAB(8)=PTAB(4)
      PTAB(9)=PTAB(5)
      PTAB(10)=PTAB(6)
      PTAB(11)=PTAB(7)
      XMID2=0.5*(PTAB(5)+PTAB(6))
      YMID2= PTAB(4)
      CALL GSVOLU('HBME','TRAP',IMEDI,PTAB,11,IVOL)
C
C     Position 2 modules HBMO in each HBAR
C
      X=0.5*(HCRMIN(1)+HCRMAX(1)-HCLSLA)
      X2=HCRMAX(1)-0.5*HCLSLA
      Y2=0.5*X2*TAN(DPHB4*2.)
      Y=X*TAN(DPHB4*2.)*.5
      Z=0.
      IAGROT=IAGROT+1
      CALL GSROTM(IAGROT,D90,-D90,D90,0.,0.,0.)
      CALL GSPOS('HBMO',1,'HBAR',X,-Y,Z,IAGROT,'ONLY')
      CALL GSPOS('HBME',1,'HBAR',X2,-Y2,Z,IAGROT,'ONLY')
      IAGROT=IAGROT+1
      CALL GSROTM(IAGROT,D90,D90,D90,0.,D180,0.)
      CALL GSPOS('HBMO',2,'HBAR',X,Y,Z,IAGROT,'ONLY')
      CALL GSPOS('HBME',2,'HBAR',X2,Y2,Z,IAGROT,'ONLY')
C
C   PUT NOTCHES FOR CABLES
C
      PTAB(1)=0.5*HCWINO(1)
      PTAB(2)=YMID
      PTAB(3)=0.5*HCLTNO(1)
      CALL GSVOLU('HBN1','BOX ',INOTCH,PTAB,0,IVOL)
      X=-XMID+PTAB(1)
      Y=0.
      Z=HCZMAX(1)-PTAB(3)
      CALL GSPOSP('HBN1',1,'HBMO',X,Y,Z,0,'ONLY',PTAB,3)
      CALL GSPOSP('HBN1',2,'HBMO',X,Y,-Z,0,'ONLY',PTAB,3)
      PTAB(2)=YMID2
      X=-XMID2+PTAB(1)
      Y=0.
      Z=HCZMAX(1)-PTAB(3)
      CALL GSPOSP('HBN1',1,'HBME',X,Y,Z,0,'ONLY',PTAB,3)
      CALL GSPOSP('HBN1',2,'HBME',X,Y,-Z,0,'ONLY',PTAB,3)
      PTAB(1)=0.5*HCWINO(2)
      PTAB(2)=YMID
      PTAB(3)=0.5*HCLTNO(2)
      PTAB(4)=15.
      PTAB(5)=0.
      PTAB(6)=0.
      CALL GSVOLU('HBN2','PARA',IMHNS,PTAB,0,IVOL)
      X=XMID-PTAB(1)
      Y=0.
      Z=HCZMAX(1)-PTAB(3)
      CALL GSPOSP('HBN2',1,'HBMO',X,Y,Z,0,'ONLY',PTAB,6)
      CALL GSPOSP('HBN2',2,'HBMO',X,Y,-Z,0,'ONLY',PTAB,6)
      PTAB(2)=YMID2
      X=XMID2-PTAB(1)
      Y=0.
      Z=HCZMAX(1)-PTAB(3)
      CALL GSPOSP('HBN2',1,'HBME',X,Y,Z,0,'ONLY',PTAB,6)
      CALL GSPOSP('HBN2',2,'HBME',X,Y,-Z,0,'ONLY',PTAB,6)
C
C      DEFINE BARREL LAYERS
C
      CALL GSVOLU('HBLA','BOX ',IMHSN,PTAB,0,IVOL)
      CALL GSVOLU('HBL1','BOX ',IMHSN,PTAB,0,IVOL)
      CALL GSVOLU('HBL2','BOX ',IMHSN,PTAB,0,IVOL)
C
      RM =0.5* (HCRMIN(1)+HCRMAX(1)-HCLSLA)
      WM = RM*TPHB2
      PTAB(2) =0.5* HSTREA
      EPS =  0.5*HSTREA*TPHB2
      DO 20 IB = 1, LHCNL
         Y = -RM + HCLARA(IB)
         X = (HCLAWI(IB)-EPS-WM)*.5
         Z = 0.
         PTAB(1) = 0.5*(HCLAWI(IB)-EPS)
         PTAB(3) = HCZMAX(1)-HCLTNO(1)
         CALL GSPOSP('HBLA',IB,'HBMO',X,Y,Z,0,'ONLY',PTAB,3)
         PTAB(1) = 0.5*(HCLAWI(IB)-EPS-HCWINO(2)-HCWINO(1))
         PTAB(3) = 0.5*HCLTNO(1)
         X = 0.5*(HCLAWI(IB)-EPS-WM)-0.5*(HCWINO(2)-HCWINO(1))
         Z=HCZMAX(1)-0.5*HCLTNO(2)
         CALL GSPOSP('HBL1',IB,'HBMO',X,Y,Z,0,'ONLY',PTAB,3)
         CALL GSPOSP('HBL2',IB,'HBMO',X,Y,-Z,0,'ONLY',PTAB,3)
   20 CONTINUE
      IF(IAGSLV.GE.LSENV) GOTO 70
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('HBLA')
      IAGSEN(IAGSLV,2) = 4
      IF(IAGSLV.GE.LSENV) GOTO 70
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('HBL1')
      IAGSEN(IAGSLV,2) = 4
      IF(IAGSLV.GE.LSENV) GOTO 70
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('HBL2')
      IAGSEN(IAGSLV,2) = 4
C
C   Endcaps
C
      PTAB(1)=0.
      PTAB(2)=D360
      PTAB(3)=4
      PTAB(4)=HCZMIN(2)
      PTAB(5)=HCRMIN(2)
      PTAB(6)=HCRMAX(2)/COSX
      PTAB(7)=HCZMAX(2)-0.001
      PTAB(8)=PTAB(5)
      PTAB(9)=PTAB(6)
      PTAB(10)=HCZMAX(2)+0.001
      PTAB(11)=HCRMIN(3)
      PTAB(12)=HCRMAX(3)/COSX
      PTAB(13)=HCZMAX(3)
      PTAB(14)=PTAB(11)
      PTAB(15)=PTAB(12)
      CALL GSVOLU('HCPA','PCON',IMHNS,PTAB,15,IVOL)
      CALL GSVOLU('HCPB','PCON',IMHNS,PTAB,15,IVOL)
C
      CALL GSPOS('HCPA',1,'HCEA',0.,0.,0.,0,'ONLY')
      CALL GSPOS('HCPB',1,'HCEB',0.,0.,0.,0,'ONLY')
C
C     Place 6 sectors
C
CBB  Try the division option :6 HCMA IN HCPA,6 HCMB IN HCPB
      CALL GSDVN('HCMA','HCPA',LPHCES,2)
      CALL GSDVN('HCMB','HCPB',LPHCES,2)
      D60=D180/3.
      PTAB(2)=D60
      CALL GSVOLU('HCMO','PCON',IMHEN,PTAB,15,IVOL)
C
C   Redefine the module HCMO with adequate Tracking medium and axes
C   starting at lower Phi limit for tubes numbering consistency
      IAGROT = IAGROT + 1
      CALL GSROTM(IAGROT,D90,-0.5*D60,D90,D90-0.5*D60,0.,0.)
      CALL GSPOS('HCMO',1,'HCMA',0.,0.,0.,IAGROT,'ONLY')
      CALL GSPOS('HCMO',1,'HCMB',0.,0.,0.,IAGROT,'ONLY')
      CALL GSVOLU('HCLA','PCON',IMHSN,PTAB,0,IVOL)
      IF(IAGSLV.GE.LSENV) GOTO 70
      IAGSLV = IAGSLV + 1
      IAGSEN(IAGSLV,1) = JHOCHA('HCLA')
      IAGSEN(IAGSLV,2) = 4
      PTAB(1)=PTAB(14)
      PTAB(2)=PTAB(15)
      PTAB(3)=0.5*HCLSLA
      PTAB(4)=0.
      PTAB(5)=D60
      CALL GSVOLU('HCME','TUBS',IMEDI,PTAB,5,IVOL)
      Z=HCZMAX(3)-PTAB(3)
      CALL GSPOS('HCME',1,'HCMO',0.,0.,Z,0,'ONLY')
C
C   Implement passive material lying above inner part of end cap
C   that is : Ecal barrel boxes, Ecal petal boxes
C             Hcal electronics boxes
C             all cables and piping going to the notches
C
      IF (IGEOJO(13)*IGEOJO(4)*IGEOJO(12).GT.0) THEN
         PTAB(1)=HCRMAX(2)/COSX
         PTAB(2)=AGLIMR(6)
         PTAB(3)=0.5*(AGLIMZ(3)-AGLIMZ(2))
         Z=AGLIMZ(2)+PTAB(3)
         CALL GSPOSP('EPBX',1,'HCEA',0.,0.,Z,0,'ONLY',PTAB,3)
         CALL GSPOSP('EPBX',1,'HCEB',0.,0.,Z,0,'ONLY',PTAB,3)
         CALL AGECPM(2)
      ENDIF
C
      PTAB(1) = 0.
      PTAB(2) = D60
      PTAB(3) = 2.
      PTAB(4) = -0.5*HSTREA
      PTAB(5) = HCRMIN(2)
      PTAB(6) = HCRMAX(2)/COSX
      PTAB(7) = -PTAB(4)
      PTAB(8) = PTAB(5)
      PTAB(9) = PTAB(6)
      Z = HCZMIN(2)+HCIRTH+0.5*HCTUTH
      DO 50 IB=1, NHCINL
         CALL GSPOSP('HCLA',IB,'HCMO',0.,0.,Z,0,'ONLY',PTAB,9)
         Z = Z + HCSMTH
   50 CONTINUE
      PTAB(5) = HCRMIN(3)
      PTAB(6) = HCRMAX(3)/COSX
      PTAB(8) = PTAB(5)
      PTAB(9) = PTAB(6)
      DO 60 IB=NHCINL+1,NHCINL+NHCOUL
         CALL GSPOSP('HCLA',IB,'HCMO',0.,0.,Z,0,'ONLY',PTAB,9)
         Z = Z + HCSMTH
   60 CONTINUE
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('HBMO',4,'HCAL')
      CALL AGDIMP('HCPA',3,'HCAL')
      CALL AGDIMP('HCPB',3,'HCAL')
      GOTO 80
C
C - not enough space to save sensitive module
C
   70 CONTINUE
      CALL ALTELL('AGHCAL: too many sensitive volumes ',0,'STOP')
C
C -  End
C
   80 RETURN
      END
#endif
