*DK agtpch
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
C.
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSTMED,GSVOLU,GSPOS  from  GEANT3
C.
C. -Stores extra Tracking Media needed
C. -Builds geometry levels from  'TPCR'  and downwards
C ----------------------------------------------------------------------------
#ifndef DOC
      EXTERNAL JHOCHA
#include "iocom.h"
#include "bcs.h"
#include "agcons.h"
#include "alfgeo.h"
#include "agecom.h"
#include "jobcom.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "pmg1jj.h"
C
      PARAMETER (D52P5 = 52.5)
C
      PARAMETER (LTAB1=5, LTAB2=5,  LMED=5)
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2,LMED)
      REAL ATEP(2),ZTEP(2),WTEP(2)
      REAL ATEP2(2),ZTEP2(2),WTEP2(2)
C
C ========================================================================
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
C  Temperature and Pressure conditions in Geneva !!!
     4 38.774,17.492,1.504E-03,13284.,90390.,5*0./
C     4 38.774,17.492,1.684E-03,11866.9,79875.3,5*0./
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
#include "bmacro.h"
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
#if defined(INTER)
      TAB1(1,2) = 3.
#endif
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
#endif
