      SUBROUTINE ECRMIP
C-----------------------------------------------------------------
C!  LOOK FOR MINIMUM IONIZING PARTICLE
C!
C!  AUTHOR   : J.P. ALBANESE
C!  MODIFIED : F. ETIENNE      87/08/27
C!  MODIFIED : D. PALLIN       88/12/19
C!
C!  BANKS :
C!    INPUT   : ECLU,ECT1,ERL3,ETP1
C!    OUTPUT  : NONE
C!    CREATED :
C!
C----------------------------------------------------------------
C
#ifndef DOC
#include "bcs.h"
#include "eminic.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "eclujj.h"
#include "ect1jj.h"
#include "erl3jj.h"
#include "etp1jj.h"
#include "emipjj.h"
      PARAMETER(NTRMAX=5)
#include "bmacro.h"
C
C ---------------------
C? CREATE EMIP BANK
C ---------------------
C
C  WE RESERVE ONE ROW FOR EACH TRACK
C GET NBER  OF TRACKS
      IER=0
      NAFRFT=NAMIND('FRFT')
      KFRFT=IW(NAFRFT)
      NTRKS=LROWS(KFRFT)
C CREATE BANK
      CALL AUBOS('EMIP',0,NTRKS*LEMIPA+LMHLEN,KEMIP,IGARB)
      IF(IGARB.EQ.2)GO TO 998
      CALL BLIST(IW,'T+','EMIP')
      IW(KEMIP+LMHCOL)=LEMIPA
      IW(KEMIP+LMHROW)=0
C
C ---------------------
C? FIND M.I.P
C ---------------------
C
      KESDA = IW(NAESDA)
      KECLU = IW(NAECLU)
      KERL3 = IW(NAERL3)
      KECT1 = IW(NAECT1)
      KETP1 = IW(NAETP1)
C
      NECLU=LROWS(KECLU)
C
C  LOOP ON CLUSTERS
C
C
      DO 1 ICC=1,NECLU
      ICT = 0
      CALL VZERO(ENLVCL,3)
      CALL VZERO(NTRACL,NMAX)
      CALL VZERO(PTPCCL,NMAX)
C
C? GET ASSOCIATED TRACKS FOR EACH CLUSTER
C
      N1 = ITABL(KECT1,ICC,JECTER)
 11   CONTINUE
      IF (N1 .NE. 0 .AND. ICT .LT. NTRMAX)  THEN
         ICT =ICT + 1
         NTRACL(ICT) = ITABL(KERL3,N1,JERLET)
         PTPCCL(ICT) = RTABL(KETP1,NTRACL(ICT),JETPPT)
         N1          = ITABL(KERL3,N1,JERLER)
         GO TO 11
      ENDIF
      IF(ICT.EQ.0)GOTO 1
C
C? NBER OF MODULE INVOLVED IN THIS CLUSTER
C? NBMODU=1 ONE MODULE  NBMODU=2 MORE THAN ONE
C
      NBMODU=0
      NMOD=0
      IF (NTRACL(2).EQ.0.)THEN
          ISTO=ITABL(KECLU,ICC,JECLES)
 3        IF(ISTO.EQ.0)GO TO 2
          NMOD1=ITABL(KESDA,ISTO,JESDER)
          IF(NMOD1.NE.NMOD)THEN
                    NBMODU=NBMODU+1
                    IF(NBMODU.GT.1)GOTO 2
                    NMOD=NMOD1
                    ENDIF
          ENTO=RTABL(KESDA,ISTO,JESDME)
          KSTO=ITABL(KESDA,ISTO,JESDDK)
          ENLVCL(KSTO)=ENLVCL(KSTO)+ENTO
          ISTO=ITABL(KESDA,ISTO,JESDES)
          GOTO 3
      ENDIF
 2    CONTINUE
C?   FOR NOW, ONLY ATTEMPT MIP IDENTIFICATION
C?   FOR CLUSTERS IN ONLY ONE MODULE, AND ASSOCIATED TO A SINGLE TRACK
C?   IFLAMP =1 IF MIP PARTICLE IDENTIFIED
C      IF (NTRACL(2).EQ.0.AND.NBMODU.EQ.1) THEN
      IF (NTRACL(2).EQ.0)THEN
C      RO
         BARYCL(1) = RTABL(KECLU,ICC,JECLR4)
C      THETA
         BARYCL(2) = RTABL(KECLU,ICC,JECLT4)
C      PHI
         BARYCL(3) = RTABL(KECLU,ICC,JECLF4)
C
         CALL ECFMIP
C
C ---------------------
C? FILL BANK EMIP
C ---------------------
C
         IF(IFLAMP.EQ.1)THEN
            NEMI=IW(KEMIP+LMHROW)
            NEMI=NEMI+1
            IW(KEMIP+LMHROW)=NEMI
            IW(KROW(KEMIP,NEMI)+JEMIFR)=NTRACL(1)
         ENDIF
C
      ENDIF
  1   CONTINUE
C
C
C ---------------------
C? CLOSE MIP BANK
C ---------------------
C
C NBER OF USEFUL  DATA IN EMIP BK
      IUSE=LMHLEN+IW(KEMIP+LMHCOL)*IW(KEMIP+LMHROW)
      CALL AUBOS('EMIP',0,IUSE,KEMIP,IGARB)
      RETURN
  998 CONTINUE
      CALL RERROR('ECRMIP',1,' exit')
      IER=1
      IND = NDROP('EMIP',0)
      RETURN
      END
#endif
