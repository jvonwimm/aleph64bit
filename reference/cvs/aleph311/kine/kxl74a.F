       SUBROUTINE KXL74A (IPART,JKLIN)
C --------------------------------------------------
C - B.Bloch - March 94 adapted to JETSET 7.4 from KXL7PA (JETSET 7.3 )
C! fill 'PART' bank with LUND7 particles
CKEY KINE KINGAL LUND7 PART  /  USER
C  Create the KLUN bank with version and date of JETSET library
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
C              User Entry Name: KXL74A
C              External References: NAMIND(BOS77)
C                                   KGPART/KBKLIN/KBPART/KXL7TO/KMPART
C                                   ADBVER/ALKLUN/PRTABL/KXL7CO/AUBPRS
C                                   (ALEPHLIB)
C                                   LUCHGE/ULMASS/LUNAME/LUCOMP(JETSET)
C                                   IUCOMP(CERNLIB)
C              Comdecks referenced: BCS, BMACRO,KMACRO
C
C - Usage    : CALL KXL74A (IPART,JKLIN)
C - Output   : IPART   = KBPART return flag
C                        .gt. 0 means OK
C              JKLIN   = KBKLIN return flag
C                        .gt. 0 means OK
#ifndef DOC
      SAVE
#include "bcs.h"
#include "lun7com.h"
#include "partjj.h"
C     ILUGE (LLUGE) are the LUND numbers corresponding to the first
C                   part of PART bank ( used by GEANT)
C     ILUAL (LLUAL) are the LUND numbers corresponding to the rest of
C                   the PART bank
      PARAMETER ( ELEP = 205.)
      PARAMETER ( IDBNW = 114)
      PARAMETER ( DMAS =0.  , IANTI = 0)
      PARAMETER ( LLUGE=52 ,   LLUAL =315)
C    Particle listing limits
      PARAMETER ( NOPA1=83, NOPA2=89, NOPA3=94, NOPA4=101, LASTP=30885)
      PARAMETER ( NOPA5=700, NOPA6 = 1000, NOPA7= 5400 , NOPA8=10000)
      INTEGER ILUGE(LLUGE),ILUAL(LLUAL)
      EXTERNAL NAMIND,ALKLUN
      INTEGER  ALKLUN
      CHARACTER TNAM*16
      DATA ILUGE /22,-11,11,0,-13,13,111,211,-211,130,321,-321,2112,
     +           2212,-2212,310,221,3122,3222,3212,3112,
     +           3322,3312,3334,-2112,-3122,-3222,-3212,-3112,
     +           -3322,-3312,-3334,-15,15,411,-411,421,-421,
     +           431,-431,4122,24,-24,23,8*0/
      DATA ILAM /4122/
      DATA ILUAL /-4122,25,551,311,-311,12,-12,14,-14,16,-16,20213,
     &-20213,20113,10221,10111,331,10441,20443,445,443,30443,441,213,
     &-213,323,-323,313,-313,423,-423,413,-413,433,-433,113,223,333,
     &10551,20553,555,553,30553,661,10661,20663,665,21,
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
      ILUVER = MSTU(181)*1000+ MSTU(182)
      ILULMD = MOD(MSTU(183),100)*10000 + MSTU(184)*100 + MSTU(185)
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
C    Avoid mass-smearing when using ULMASS for the database
      MSTJ24 = MSTJ(24)
      MSTJ(24) = 0
C - if new PART bank format and content, extend the KLIN bank
      IF (LCOPA.EQ.LPARTA .AND. IVERS.GE.IDBNW) THEN
         DO 2 IPART = LLUGE+1,LLUAL+LLUGE
            JKLIN = KBKLIN (IPART,ILUAL(IPART-LLUGE))
            IF (JKLIN .LE. 0) GOTO 998
  2      CONTINUE
C If old content complete with antiLambdac and update new format
C if needed
      ELSEIF (IVERS.LT.IDBNW) THEN
C
C - Fill Antilambda C
C
         IALAM = KGPART (ILAM)
         NAPAR = NAMIND ('PART')
         TLIF  = TIMLIF (IALAM)
         CHAR  = LUCHGE (ILAM) / 3.
         ZMAS  = ULMASS (ILAM)
         TNAM  = ' '
         CALL LUNAME (-ILAM,TNAM)
         IPART = KBPART (KNOTRK,TNAM,KNOTRK,ZMAS,-CHAR,TLIF)
         IF (IPART .LE. 0) GOTO 998
         JKLIN = KBKLIN (IPART,-ILAM)
         IF (JKLIN .LE. 0) GOTO 998
         IF (LCOPA.EQ.LPARTA) THEN
            MPART = KMPART (IPART,DMAS,IALAM)
            IF (MPART.LE.0) GO TO 998
            MPART = KMPART (IALAM,DMAS,IPART)
            IF (MPART.LE.0) GO TO 998
         ENDIF
      ENDIF
C -- Set  parameters by data cards  ,use the library routine KXL7CO
C    this has to be done after the bank KLIN creation
      CALL KXL7CO (LUPAR)
C
C - Get LUGEN particles and transfer them to PART
C   if their mass is in ELEP energy range
C   these particles are not tracked so their GEANT#
C   and tracking type are set to KNOTRK
C
CTM Modify listing of particles
      DO 1000 MYPP=1, LASTP
         MYP = MYPP
         IF (MYP.EQ.81.OR.MYP.EQ.82) GOTO 1000
         IF (MYP.EQ.210.OR.MYP.EQ.2110.OR.MYP.EQ.2210) GOTO 1000
         IF (MYP.GT.NOPA1 .AND. MYP.LT.NOPA2) GOTO 1000
         IF (MYP.GT.NOPA3 .AND. MYP.LT.NOPA4) GOTO 1000
         IF (MYP.GT.NOPA5 .AND. MYP.LT.NOPA6) GOTO 1000
         IF (MYP.GT.NOPA7 .AND. MYP.LT.NOPA8) GOTO 1000
CTM  Is the particle in the Aleph data-base ?
         IALP = IUCOMP(MYP,ILUGE,LLUGE)
         IF (IALP.GT.0) GO TO 1000
         IF ((LCOPA.EQ.LPARTA).AND.(IVERS.GE.IDBNW) .AND.
     +                   ( IUCOMP(MYPP,ILUAL,LLUAL).GT.0)) GO TO 1000
         TNAM = ' '
         CALL LUNAME (MYP,TNAM)
         IF (TNAM .EQ. ' ') GOTO 1000
         CHAR = LUCHGE (MYP) /3.
         CALL KXL7TO(MYP,TLIF)
         ZMAS = ULMASS (MYP)
C        TLIF = 3.33E-12*PMAS (LUCOMP(MYP),4)
         IF ( tlif.eq.0.) tlif = max(tlif,3.33E-12*PMAS(LUCOMP(MYP),4))
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
         KC = LUCOMP(MYP)
         IF ( KC.LE.0 ) GO TO 998
         JANTI = KCHG(KC,3)
         WIDM  = PMAS(KC,2)
         IF (JANTI.EQ.0) THEN
           IF (LCOPA.EQ.LPARTA ) THEN
              MPART = KMPART (IPART,WIDM,IPART)
              IF (MPART.LE.0) GO TO 998
           ENDIF
         ELSE
           CALL LUNAME (-MYP,TNAM)
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
CTM  Make sure that the standard particle data is used by Jetset
      IDPAR = IW(NAPAR)
      IDKLI = IW(NAMIND('KLIN'))
      DO 9999 I = 1, LLUAL+LLUGE
         ZMAS = RTABL(IDPAR,I,JPARMA)
         TLIF = RTABL(IDPAR,I,JPARLT)
         WMAS = RTABL(IDPAR,I,JPARMW)
         KFCOD = IABS(ITABL(IDKLI,I,1))
CTM  Doesn't work for quarks, because 0 not acceptable to Jetset 7.3
         IF (KFCOD.LE.6) GOTO 9999
         KCODE = LUCOMP(KFCOD)
         IF ( KCODE.LE.0) GO TO 9999
         PMAS(KCODE,1) = ZMAS
         PMAS(KCODE,2) = WMAS
         IF (TLIF.NE.1.E15 )PMAS(KCODE,4) = TLIF/3.33E-12
 9999 CONTINUE
C
C
      GOTO 999
C - not enough space
 998  CONTINUE
C - End
 999  CONTINUE
C    Restore the original mass-smearing status-word.
      MSTJ(24) = MSTJ24
C
      END
#endif
