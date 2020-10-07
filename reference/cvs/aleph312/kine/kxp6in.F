       SUBROUTINE KXP6IN (IPART,JKLIN)
C --------------------------------------------------
C - B.Bloch - January 98 adapted to PYTHIA 6.1 from KXL7PA (JETSET 7.3 )
C! fill 'PART' bank with PYTHIA particles
CKEY KINE KINGAL LUND7 PART  /  USER
C  Create the KLUN bank with version and date of PYTHIA library
C  Get  the NOtracking marker word NOTRK from KRUN bank
C  Fill KLIN bank with LUND particle# which correspond
C       to GEANT particles ( or ALEPH particles)
C  Fill Antilambda C when necessary
C  Get  LUND particles and transfer them to PART bank
C       if they can be produced at LEP energies,
C       with a GEANT# and a tracking type set to NOTRK
C       because they are not used by GEANT.
C  Reduce PART and KLIN banks to their normal size
C  Make sure Jetset uses ALEPH values for masses and life times
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXP6IN
C              External References: NAMIND(BOS77)
C                                   KGPART/KBKLIN/KBPART/KXP6TO/KMPART
C                                   ADBVER/ALKLUN/PRTABL/KXP6CO/AUBPRS
C                                   (ALEPHLIB)
C                                   PYCHGE/PYMASS/PYNAME/PYCOMP(PYTHIA)
C                                   IUCOMP(CERNLIB)
C              Comdecks referenced: BCS, BMACRO,KMACRO
C
C - Usage    : CALL KXP6IN (IPART,JKLIN)
C - Output   : IPART   = KBPART return flag
C                        .gt. 0 means OK
C              JKLIN   = KBKLIN return flag
C                        .gt. 0 means OK
#ifndef DOC
#include "pyt6com.h"
#include "bcs.h"
#include "partjj.h"
C
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first
C                   part of PART bank ( used by GEANT)
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of
C                   the PART bank
      PARAMETER ( ELEP = 205.)
      PARAMETER ( IDBNW = 114)
      PARAMETER ( DMAS =0.  , IANTI = 0)
      PARAMETER ( LLUGE=52 ,   LLUAL =315)
C    Particle listing limits
      PARAMETER (NOPA1=83,NOPA2=89,NOPA3=94,NOPA4=101,LASTP=4000012)
      PARAMETER (NOPA5=700, NOPA6 = 1000, NOPA7= 5600 , NOPA8=10000)
      PARAMETER (NOPA9=10600,NOPA10=20100,NOPA11= 20600,NOPA12=100400)
      PARAMETER (NOPA13=100600,NOPA14=1000000)
      PARAMETER (NOPA15=1000040,NOPA16=2000000)
      PARAMETER (NOPA17=2000050,NOPA18=4000000)
      INTEGER ILUGE(LLUGE),ILUAL(LLUAL)
      EXTERNAL NAMIND,ALKLUN
      INTEGER  ALKLUN
      CHARACTER TNAM*16
      REAL*4 TIMLIF,zmas,char,wmas,tlif,rtabl,tlif_temp
      DATA ILUGE /22,-11,11,0,-13,13,111,211,-211,130,321,-321,2112,
     +           2212,-2212,310,221,3122,3222,3212,3112,
     +           3322,3312,3334,-2112,-3122,-3222,-3212,-3112,
     +           -3322,-3312,-3334,-15,15,411,-411,421,-421,
     +           431,-431,4122,24,-24,23,8*0/
      DATA ILAM /4122/
      DATA ILUAL /-4122,25,551,311,-311,12,-12,14,-14,16,-16,20213,
     &-20213,20113,10221,10111,331,10441,20443,445,443,100443,441,213,
     &-213,323,-323,313,-313,423,-423,413,-413,433,-433,113,223,333,
     &10551,20553,555,553,100553,661,10661,20663,665,21,
     &2,1,3,4,5,6,-2,-1,-3,-4,-5,-6,663,
     &30663,-521,521,511,-511,531,-531,-541,541,-523,523,513,-513,
     &533,-533,-543,543,621,-621,611,-611,631,-631,641,-641,
     &651,-651,623,-623,613,-613,
     &633,-633,643,-643,653,-653,2224,-2224,2214,-2214,2114,-2114,1114,
     &-1114,3224,-3224,3214,-3214,3114,-3114,3324,-3324,3314,-3314,4222,
     &-4222,4212,-4212,4112,-4112,4322,-4322,4312,-4312,4332,-4332,4232,
     &-4232,4132,-4132,4224,-4224,4214,-4214,4114,-4114,4324,-4324,4314,
     &-4314,4334,-4334,4422,-4422,4412,-4412,4432,-4432,4424,-4424,4414,
     &-4414,4434,-4434,4444,-4444,5222,-5222,5212,-5212,5112,-5112,5322,
     &-5322,5312,-5312,5332,-5332,5242,-5242,5142,-5142,5342,-5342,5442,
     &-5442,5522,-5522,5512,-5512,5532,-5532,5542,-5542,6222,-6222,6212,
     &-6212,6112,-6112,6232,-6232,6132,-6132,6332,-6332,6242,-6242,6142,
     &-6142,6342,-6342,6442,-6442,5122,-5122,5232,-5232,5132,-5132,5422,
     &-5422,5412,-5412,5432,-5432,6122,-6122,6322,-6322,6312,-6312,6422,
     &-6422,6412,-6412,6432,-6432,5224,-5224,5214,-5214,5114,-5114,5324,
     &-5324,5314,-5314,5334,-5334,5424,-5424,5414,-5414,5434,-5434,5444,
     &-5444,6224,-6224,6214,-6214,6114,-6114,6324,-6324,6314,-6314,6334,
     &-6334,6424,-6424,6414,-6414,6434,-6434,6444,-6444,6524,-6524,6514,
     &-6514,6534,-6534,6544,-6544,6554,-6554,6252,-6252,6152,-6152,6352,
     &-6352,6452,-6452,6552,-6552,6522,-6522,6512,-6512,6532,-6532,6542,
     &-6542,5524,-5524,5514,-5514,5534,-5534,5544,-5544,5554,-5554,-11,
     &11,5*0/
C
#include "bmacro.h"
#include "kmacro.h"
C
C ------------------------------------------------------
C - Get NAPAR name-index of PART bank
      NAPAR = NAMIND ('PART')
C - Get number of columns in PART bank
      IDPAR = IW(NAPAR)
      LCOPA = LCOLS(IDPAR)
C - Get Data Base version number
      CALL ADBVER(IVERS,IDATE)
C
C - NOtrack marker word stored in KRUN bank
      KNOTRK = ITABL (IW(NAMIND('KRUN')),1,2)
C
C - Get JETSET version # and date of last modification
C   create bank KLUN with this info
C
      ILUVER = MSTP(181)*1000+ MSTP(182)
      ILULMD = MOD(MSTP(183),100)*10000 + MSTP(184)*100 + MSTP(185)
      IKLUN =  ALKLUN(ILUVER,ILULMD)
      IF (IKLUN.LE.0) CALL EXIT
      CALL PRTABL ('KLUN',0)
C
C - Fill KLIN with LUGEN particle# for the GEANT particles
C   which are the 1st LLUGE particles of PART
C
C
      DO 1 IPART=1,LLUGE
         JKLIN = KBKLIN (IPART,ILUGE(IPART))
         IF (JKLIN .LE. 0) GOTO 998
 1    CONTINUE
C - extend the KLIN bank
         DO 2 IPART = LLUGE+1,LLUAL+LLUGE
            JKLIN = KBKLIN (IPART,ILUAL(IPART-LLUGE))
            IF (JKLIN .LE. 0) GOTO 998
  2      CONTINUE
C -- Set  parameters by data cards  ,use the library routine KXP6CO
C    this has to be done after the bank KLIN creation
      CALL KXP6CO (LUPAR)
C add an extra _ to b_1 to avoid ambiguity with B_1 once in upper case
      kc = pycomp(10113)
      chaf(kc,1)= 'b__10'
      kc = pycomp(10213)
      chaf(kc,1)= 'b__1+'
      chaf(kc,2)= 'b__1-'
C
C - Get LUGEN particles and transfer them to PART
C   if their mass is in ELEP energy range
C   these particles are not tracked so their GEANT#
C   and tracking type are set to KNOTRK
C
C Modify listing of particles
      DO 1000 MYPP=1, LASTP
         MYP = MYPP
         IF (MYP.EQ.81.OR.MYP.EQ.82.OR.MYP.EQ.110) GOTO 1000
         IF (MYP.EQ.220.OR.MYP.EQ.330.OR.MYP.EQ.440) GOTO 1000
         IF (MYP.EQ.210.OR.MYP.EQ.2110.OR.MYP.EQ.2210) GOTO 1000
         IF (MYP.GT.NOPA1 .AND. MYP.LT.NOPA2) GOTO 1000
         IF (MYP.GT.NOPA3 .AND. MYP.LT.NOPA4) GOTO 1000
         IF (MYP.GT.NOPA5 .AND. MYP.LT.NOPA6) GOTO 1000
         IF (MYP.GT.NOPA7 .AND. MYP.LT.NOPA8) GOTO 1000
         IF (MYP.GT.NOPA9 .AND. MYP.LT.NOPA10) GOTO 1000
         IF (MYP.GT.NOPA11 .AND. MYP.LT.NOPA12) GOTO 1000
         IF (MYP.GT.NOPA13 .AND. MYP.LT.NOPA14) GOTO 1000
         IF (MYP.GT.NOPA15 .AND. MYP.LT.NOPA16) GOTO 1000
         IF (MYP.GT.NOPA17 .AND. MYP.LT.NOPA18) GOTO 1000
C  Is the particle in the Aleph data-base ?
         IALP = IUCOMP(MYP,ILUGE,LLUGE)
         IF (IALP.GT.0) GO TO 1000
         IF ((LCOPA.EQ.LPARTA).AND.(IVERS.GE.IDBNW) .AND.
     +                   ( IUCOMP(MYPP,ILUAL,LLUAL).GT.0)) GO TO 1000
         TNAM = ' '
         CALL PYNAME (MYP,TNAM)
         IF (TNAM .EQ. ' ') GOTO 1000
         CHAR = PYCHGE (MYP) /3.
         CALL KXP6TO(MYP,TLIF)
         KC = PYCOMP(MYP)
         ZMAS = PMAS (KC,1)
         tlif_temp = 3.33E-12*PMAS(KC,4)
         IF ( tlif.eq.0.) tlif = max(tlif,tlif_temp)
C
         IF (ZMAS.GT.ELEP ) GO TO 1000
C          store the new particle# IPART
         IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,CHAR,TLIF)
         IF (IPART.LE.0) GOTO 998
C          store the user generator particle# of the new particle
         JKLIN = KBKLIN (IPART,MYPP)
         IF (JKLIN.LE.0) GOTO 998
C
C          do the same for the antiparticle except if identical
C         KC = PYCOMP(MYP)
         IF ( KC.LE.0 ) GO TO 998
         JANTI = KCHG(KC,3)
         WIDM  = PMAS(KC,2)
         IF (JANTI.EQ.0) THEN
           IF (LCOPA.EQ.LPARTA ) THEN
              MPART = KMPART (IPART,WIDM,IPART)
              IF (MPART.LE.0) GO TO 998
           ENDIF
         ELSE
           CALL PYNAME (-MYP,TNAM)
           IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)
           IF (IPART.LE.0) GOTO 998
           IF (LCOPA.EQ.LPARTA ) THEN
              MPART = KMPART (IPART,WIDM,IPART-1)
              IF (MPART.LE.0) GO TO 998
              MPART = KMPART (IPART-1,WIDM,IPART)
              IF (MPART.LE.0) GO TO 998
           ENDIF
           JKLIN = KBKLIN (IPART,-MYPP)
           IF (JKLIN.LE.0) GOTO 998
         ENDIF
C
 1000 CONTINUE
      CALL AUBPRS ('PARTKLIN')
C
C  Make sure that the standard particle data is used by Jetset
      IDPAR = IW(NAPAR)
      IDKLI = IW(NAMIND('KLIN'))
      DO 9999 I = 1, LLUAL+LLUGE
         ZMAS = RTABL(IDPAR,I,JPARMA)
         TLIF = RTABL(IDPAR,I,JPARLT)
         WMAS = RTABL(IDPAR,I,JPARMW)
         KFCOD = IABS(ITABL(IDKLI,I,1))
C  Doesn't work for quarks, because 0 not acceptable to Jetset 7.3
         IF (KFCOD.LE.6) GOTO 9999
         KCODE = PYCOMP(KFCOD)
         IF ( KCODE.LE.0) GO TO 9999
         PMAS(KCODE,1) = ZMAS
         PMAS(KCODE,2) = WMAS
         IF (TLIF.NE.1.E15 )PMAS(KCODE,4) = TLIF/3.33E-12
 9999 CONTINUE
C
      GOTO 999
C - not enough space
 998  CONTINUE
C - End
 999  CONTINUE
C
      END
#endif
