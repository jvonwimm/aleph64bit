*DK agquad
      SUBROUTINE AGQUAD
C-----------------------------------------------------------------------
C!  Implement Forward and Backward quadrupoles geometry
C  Author :      B. Bloch-Devaux                     18 March 85
C                Modified for Data base access      Septmeber 87
C
C     called from AGEOME                            from this .HLB
C     calls       GSMATE,GSTMED,GSVOLU,GSPOS           from GEANT3
C.
C. -Stores extra material needed
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below 'QUEA' and 'QUEB' levels
C.
C-----------------------------------------------------------------------
#ifndef DOC
      EXTERNAL GHSIGM
#include "bcs.h"
#include "jobcom.h"
#include "agcons.h"
#include "alfgeo.h"
#include "agecom.h"
      EXTERNAL NAMIND
      CHARACTER*4 QUR(2),QU(2)
      PARAMETER (LTAB1=5,LTAB2=4, LMED=4 )
      INTEGER AGMEDI
      PARAMETER ( INOF=0)
C    INOF means  field tracking flag in quadrupole region , normally 0
C    IAGFLD means field tracking flag inside quadrupole  , normally 3
C
      DIMENSION TAB1(LTAB1,LMED),TAB2(LTAB2,LMED)
C
      DATA QUR/'QUEA','QUEB'/,QU/'QUAA','QUAB'/
C ===================================================================
C     Medium are defined as:
C    1- squad tube volume                    material: air    IMSRG
C    2- squad tube body                      material: average
C    3- pumps and valves medium             material: average
C    4- vacuum  in pipe  medium             material: vacuum
C
C   For each medium the two following rows are filled:
C     TMXFD , DMXMS , DEEMX , EPSIL , STMIN
      DATA TAB1 /
     1  20.   ,  0.5  , 0.1  , 0.1  , 1. ,
     2  20.   ,  0.5  ,  0.1  , 1.  ,  1.  ,
     3  20.   ,  0.5  ,  0.1  , 1.  ,  1.  ,
     4  20.    ,  0.5  ,  0.1  , 1.  ,  1.
     & /
C
C        A    , Z    , DENS  , RADL
      DATA TAB2 /
     1 4*0.            ,
     2      59.7  ,   27.5   ,  2.37   ,   5.62   ,
     3      55.85 ,   26.    ,  0.84   ,   16.5     ,
     4      4*0.
     & /
C -----------------------------------------------------------------
#include "bmacro.h"
      IMSRG = 15
      IAGMAT=IAGMAT+1
      IMPUM=IAGMAT
      ABSL=10000.*TAB2(1,3)/(6.022*TAB2(3,3)*GHSIGM(5.,8,TAB2(1,3)))
      CALL GSMATE(IMPUM,'PUMPING STATION AVERA$',TAB2(1,3),TAB2(2,3),
     1            TAB2(3,3),TAB2(4,3),ABSL,0,0)
      IMVAC=16
C ======================================================================
      RMAX=AGLIMR(4)
      KQU=IW(NAMIND('QUG1'))
      IF (KQU.EQ.0) GO TO 99
C
C   DEFINE MATERIAL AND TRACKING MEDIA FOR QUADR
C
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'PUMPS AND VALVES MED$',IMPUM,0,INOF ,ALFIEL,
     1         TAB1(1,3),TAB1(2,3),TAB1(3,3),TAB1(4,3),TAB1(5,3),0,0)
      IMEDP=IAGMED
      CALL GSVOLU('QUPU','TUBE',IMEDP,PTAB,0,IVOL)
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'VACUUM IN PIPE MED$',IMVAC,0,INOF ,ALFIEL,
     1         TAB1(1,4),TAB1(2,4),TAB1(3,4),TAB1(4,4),TAB1(5,4),0,0)
      IMEDV=IAGMED
      CALL GSVOLU('QUVA','TUBE',IMEDV,PTAB,0,IVOL)
      IMEDQ = AGMEDI(' ')
      IMEDQ = AGMEDI('BEAM VACUUM  MED')
      CALL GSVOLU('QUVI','TUBE',IMEDQ,PTAB,0,IVOL)
      IAGMAT=IAGMAT+1
C
C   Recompute the absorption length for a 5 gev pi+ in this material
C
      ABSL=10000.*TAB2(1,2)/(6.022*TAB2(3,2)*GHSIGM(5.,8,TAB2(1,2)))
      CALL GSMATE(IAGMAT,'QUADR TUBE MATTER$',TAB2(1,2),TAB2(2,2),
     1            TAB2(3,2),TAB2(4,2),ABSL,0,0)
      IAGMED=IAGMED+1
      CALL GSTMED(IAGMED,'QUADR TUBE BODY $',IAGMAT,0,IAGFLD,ALFIEL
     1      ,  TAB1(1,2),TAB1(2,2),TAB1(3,2),TAB1(4,2),TAB1(5,2),0,0)
C
      DO 10 I=1,2
C
C
C  Define  QUAD geometry
         PTAB(1)=0.
         PTAB(2) = RTABL(KQU,2*I-1,9)
         PTAB(3) = 0.5*(ALZMAX-RTABL(KQU,2*I-1,10))
         CALL GSVOLU(QU(I),'TUBE', IAGMED, PTAB,3, IVOL)
C
C Place 'QUAA' and 'QUAB' in 'QUEA(B)'
         Z=ALZMAX-PTAB(3)
         Y=RTABL(KQU,2*I-1,12)
         CALL GSPOS(QU(I),1,QUR(I),0.,Y,Z,0,'ONLY')
C
         PTAB(2)=RTABL(KQU,2*I-1,8)
         Z=0.
         CALL GSPOSP('QUVI',1,QU(I),0.,-Y, Z,0,'ONLY',PTAB,3)
C
C   Implement pumping station regions
C
         PTAB(1)=0.
         PTAB(2)=RTABL(KQU,2*I,9)
         PTAB(3)=0.5*(RTABL(KQU,2*I-1,10)-AGLIMZ(2))
         Z=AGLIMZ(2)+PTAB(3)
         CALL GSPOSP('QUPU',1,QUR(I),0.,0.,Z,0,'ONLY',PTAB,3)
      IF (I.EQ.1) THEN
        PTAB(2)=RTABL(KQU,2*I,8)
        CALL GSPOSP('QUVA',1,'QUPU',0.,0.,0.,0,'ONLY',PTAB,3)
      ENDIF
  10  CONTINUE
  99  RETURN
      END
#endif
