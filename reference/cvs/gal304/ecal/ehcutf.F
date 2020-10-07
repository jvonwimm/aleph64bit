*DK ehcutf
      SUBROUTINE EHCUTF(MFLAG)
C.----------------------------------------------------------------
C  R.Clifft                       mod   M.Rumpf March 87
C! Fast tracking or not?
C  MFLAG = 0|1 --> no|yes
C  called by ECHIT
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "trkcom.h"
#include "gcking.h"
#include "ehcond.h"
#include "ehpash.h"
      CHARACTER NAME*4
C
      MFLAG=0
C
C - Do not FAST track particles with a total energy before the step > EC
C   (ECUTTE is set in EHPASR)
C
         IF (TRKELE(8).GT.ECUTTE) RETURN
C
C *** if  electron OR positron
C
      IF (ITRKEL(4).EQ.2 .OR. ITRKEL(4).EQ.3) THEN
         IF(ITRKEL(9).NE.0)            GO TO 999
C
C *** get track segment start parameters
C
         PV=TRKNXT(7)
         XV=TRKNXT(1)
         YV=TRKNXT(2)
         ZV=TRKNXT(3)
         UV=TRKNXT(4)
         VV=TRKNXT(5)
         WV=TRKNXT(6)
         TGTET = ABS (SQRT(XV**2+YV**2)/ZV)
C
C-       TEST TRACK ELEMENT IN OVERLAP REGION
C
         IF ( TGTET . LT . 0.95 .AND. TGTET . GT. 0.8 )  GO TO 999
C
C *** find which segment we are in.  nst = 1,2,3,4  for
C ***  barrel-stacks1/2 , end cap-stacks1/2 , barrel-stack3 ,
C ***  end cap-stack3
C
         CALL UHTOC(ITRKEL(5),4,NAME,4)
         IF(NAME(3:3).EQ.'S')           THEN
           NST=3
           IF(NAME(2:2).EQ.'C') NST=4
         ELSE
           NST=1
           IF(NAME(2:2).EQ.'C') NST=2
         ENDIF
C
C *** call EHEDGE to check proximity to edge of wanted region
C
         CALL EHEDGE (NST,PV,XV,YV,ZV,UV,VV,WV,
     &                DPERP,DTOT,MFLEG)
         IF (MFLEG.EQ.0)  GO TO 999
C
C *** check path length to back criterion for parametrisation
C
         PTB=DPERP
         DTB=DTOT
         CALL EHCHEK (NST,DTB,PTB,PV,MFLSL)
         IF (MFLSL.EQ.0)  GO TO 999
C
C *** we contain shower, so set necessary parameters
C
         MFLAG=1
      ENDIF

  999 RETURN
      END
#endif
