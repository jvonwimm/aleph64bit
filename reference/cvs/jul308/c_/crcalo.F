      SUBROUTINE CRCALO(IER)
C***********************************************************************
C!Create Calorimeter Object bank                                       *
C!                                                                     *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   :
C!    OUTPUT  :
C!    CREATED :
C!                                                                     *
C!   ier   = Return code ( 0 if normal)                                *
C!                                                                     *
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "ecnamc.h"
#include "ccnamc.h"
#include "cct1jj.h"
#include "ecobjj.h"
#include "etckjj.h"
#include "ctc2jj.h"
#include "crl3jj.h"
#include "cocrjj.h"
#include "calojj.h"
#include "bmacro.h"
      KETCK=IW(NAETCK)
      KCCT1=IW(NACCT1)
      KCTC2=IW(NACTC2)
      KCRL3=IW(NACRL3)
      KCOCR=IW(NACOCR)
      KECOB=IW(NAECOB)
      NHCLU=LROWS(KCCT1)
      NCRL3=LROWS(KCRL3)
      NECOB=LROWS(KCOCR)
C
C  Max number of calobjects is the number of crl3 relations +
C    number of Hcal clusters +
C    number of EcObjects
C
      MXCAL=NECOB+NCRL3+NHCLU
C
C?  Create CalObjects bank
C
      CALL AUBOS('CALO',0,MXCAL*LCALOA+LMHLEN,KCALO,IGARB)
      IF(IGARB.EQ.2)GO TO 998
C
C?   Update indices, in case of garbage collection
C
      KCCT1=IW(NACCT1)
      KCTC2=IW(NACTC2)
      KCRL3=IW(NACRL3)
      KCOCR=IW(NACOCR)
       KECOB = IW(NAECOB)
       KETCK = IW(NAETCK)
C
C?   Fill miniheader
C
      IW(KCALO+LMHCOL)=LCALOA
      IW(KCALO+LMHROW)=0
C
C?   Put calo bank in E list
C
      CALL BLIST(IW,'T+','CALO')
C
C?  For each relation :
C?     See if ecob or hclu already belong to a calobject
C?          If not, create calobject
C?     If yes,
C?        if same ecob, fine
C?        else, the two calobjects are merged, ie
C?               one of them is suppressed
C?        endif
C?     endif
C?     if calobject number is zero, create new calobject
C?
C?        all participants to the new relation belong to
C?          the calobject.
C
      DO 100 IREL=1,NCRL3
      ICALO=0
      ICALE=0
      IECOB=ITABL(KCRL3,IREL,JCRLCO)
      IF(IECOB.NE.0)THEN
         ICALE=ITABL(KCOCR,IECOB,JCOCCA)
         ICALO=ICALE
      ENDIF
      IHCLU=ITABL(KCRL3,IREL,JCRLCC)
      IF(IHCLU.NE.0)THEN
         ICALH=ITABL(KCCT1,IHCLU,JCCTCA)
         IF(ICALH.NE.0)THEN
            IF(ICALE.EQ.0)THEN
               ICALO=ICALH
            ELSEIF(ICALE.EQ.ICALH)THEN
               ICALO=ICALE
            ELSE
C
C?  We have constituents of two different calobjects :
C?      suppress one
C
               ICALO=MIN0(ICALH,ICALE)
               ISUPP=MAX0(ICALH,ICALE)
               CALL CALREP(ISUPP,ICALO,KCCT1,JCCTCA)
               CALL CALREP(ISUPP,ICALO,KCTC2,JCTCCA)
               CALL CALREP(ISUPP,ICALO,KCOCR,JCOCCA)
               CALL CALSUP(ISUPP,KCALO)
            ENDIF
         ENDIF
      ENDIF
      ITRAK=ITABL(KCRL3,IREL,JCRLCT)
      IF(ITRAK.NE.0)THEN
         ICALT=ITABL(KCTC2,ITRAK,JCTCCA)
         IF(ICALT.NE.0)THEN
            IF(ICALO.EQ.0)THEN
               ICALO=ICALT
            ELSEIF(ICALO.NE.ICALT)THEN
C
C?  Again, we have constituents of two different calobjects :
C?      suppress one
C
               ISUPP=MAX0(ICALO,ICALT)
               ICALO=MIN0(ICALO,ICALT)
               CALL CALREP(ISUPP,ICALO,KCCT1,JCCTCA)
               CALL CALREP(ISUPP,ICALO,KCTC2,JCTCCA)
               CALL CALREP(ISUPP,ICALO,KCOCR,JCOCCA)
               CALL CALSUP(ISUPP,KCALO)
            ENDIF
         ENDIF
      ENDIF
      ITRAK=ITABL(KCRL3,IREL,JCRLCT)
      IF(ITRAK.NE.0)THEN
         ICALT=ITABL(KCTC2,ITRAK,JCTCCA)
         IF(ICALT.NE.0)THEN
            IF(ICALO.EQ.0)THEN
               ICALO=ICALT
            ELSEIF(ICALO.NE.ICALT)THEN
C
C?  Again, we have constituents of two different calobjects :
C?      suppress one
C
               ISUPP=MAX0(ICALO,ICALT)
               ICALO=MIN0(ICALO,ICALT)
               CALL CALREP(ISUPP,ICALO,KCCT1,JCCTCA)
               CALL CALREP(ISUPP,ICALO,KCTC2,JCTCCA)
               CALL CALREP(ISUPP,ICALO,KCOCR,JCOCCA)
               CALL CALSUP(ISUPP,KCALO)
            ENDIF
         ENDIF
      ENDIF
      IF(ICALO.EQ.0)THEN
C
C?   Here, we create a new calobject
C
         NCALO=LROWS(KCALO)+1
         IW(KCALO+LMHROW)=NCALO
         ICALO=NCALO
      ENDIF
      ITRAC=ITABL(KCRL3,IREL,JCRLCT)
      IF(ITRAC.NE.0)THEN
         IW(KROW(KCTC2,ITRAC)+JCTCCA)=ICALO
      ENDIF
      IF(IHCLU.NE.0)THEN
         IW(KROW(KCCT1,IHCLU)+JCCTCA)=ICALO
      ENDIF
      IF(IECOB.NE.0)THEN
         IW(KROW(KCOCR,IECOB)+JCOCCA)=ICALO
      ENDIF
  100 CONTINUE
C
C?   Create calobjects for isolated ecobs
C
      DO 200 IEC=1,NECOB
      ICALO=ITABL(KCOCR,IEC,JCOCCA)
      IF(ICALO.EQ.0)THEN
         NCALO=LROWS(KCALO)+1
         IW(KCALO+LMHROW)=NCALO
         IW(KROW(KCOCR,IEC)+JCOCCA)=NCALO
         ITRAC=ITABL(KECOB,IEC,JECOET)
         IF(ITRAC.NE.0)THEN
  210       IW(KROW(KCTC2,ITRAC)+JCTCCA)=NCALO
            ITRAC=ITABL(KETCK,ITRAC,JETCET)
            IF(ITRAC.NE.0)GO TO 210
         ENDIF
      ENDIF
  200 CONTINUE
C
C?  Create calobjects for isolated hclusters
C
      DO 300 IHCLU=1,NHCLU
      ICALO=ITABL(KCCT1,IHCLU,JCCTCA)
      IF(ICALO.EQ.0)THEN
         NCALO=LROWS(KCALO)+1
         IW(KCALO+LMHROW)=NCALO
         IW(KROW(KCCT1,IHCLU)+JCCTCA)=NCALO
      ENDIF
  300 CONTINUE
      IER=0
      RETURN
  998 CALL RERROR(' CRCALO',1,' Too little space to create CALO bank')
      IER=1
      RETURN
      END
#endif
