      SUBROUTINE BDAWI(JW)
*     Internal SR to write bank to direct access daat set
*     ABEND IF SPACE ON DATA SET INSUFFICIENT
#include "dacom.h"
#include "param.h"
      INTEGER LUNAR(29)
      EQUIVALENCE (LUNAR(1),LUNDA)
      COMMON/BCS/IW(1000)
      INTEGER JW(*)
      CHARACTER*4 NAME,CHAINT
*     test directory entry
      CALL DADIR(1)
      NDTC=NDTD
*     ADD TO DATA AND DIRECTORY RECORDS

      IF(NRD.EQ.0) THEN
*        TEST REMAINING SPACE ON DATA SET
         NN=1+NDDA/NWRDA
         NN=1+(NDDA+5*NN)/NWRDA

         IF(NN.GE.IW(IUB+14)-IW(IUB+13)-1) GOTO 90
*        BANK IS NEW, ADD TO DATA AND DIRECTORY
         CALL DADAT(3,JW,*80)
         CALL DADIR(3)
         IF(JW(LUP).GT.0.AND.JW(LUW).GT.0) THEN
            WRITE(JW(LUP),101) LUNDA,CHAINT(NADA),NRDA
            JW(LUW)=JW(LUW)-1
         END IF
      ELSE
*        ALREADY ON DATA SET
         IF(NDDA.EQ.NDDR) THEN
*           SAME LENGTH
            CALL DADAT(4,JW,*80)
            CALL DADIR(4)
            IF(JW(LUP).GT.0.AND.JW(LUW).GT.0) THEN
               WRITE(JW(LUP),102) LUNDA,CHAINT(NADA),NRDA
               JW(LUW)=JW(LUW)-1
            END IF
         ELSE
*        TEST REMAINING SPACE ON DATA SET
            NN=1+NDDA/NWRDA
            NN=1+(NDDA+5*NN)/NWRDA
            IF(NN.GE.IW(IUB+14)-IW(IUB+13)-1) GOTO 90
*           DIFFERENT LENGHT
            CALL DADAT(2,JW,*80)
            CALL DADAT(3,JW,*80)
            CALL DADIR(4)
            IF(JW(LUP).GT.0.AND.JW(LUW).GT.0) THEN
               WRITE(JW(LUP),102) LUNDA,CHAINT(NADA),NRDA
               JW(LUW)=JW(LUW)-1
            END IF
         END IF
      END IF
      GOTO 100

   80 IF(JW(LUP).GT.0) THEN
         WRITE(JW(LUP),105) LUNDA,CHAINT(NADA),NRDA
         WRITE(JW(LUP),107) LUNAR
         CALL WPRNT(IW,IDR)
         CALL WPRNT(IW,IDD)
      END IF
      CALL DADAT(2,JW,*85)
   85 IF(NRD.EQ.0) CALL DADIR(2)
      CALL DAFWR(0,1)
      CALL DAFWR(0,0)
      CALL DADES
      CALL BABEND('BDAWR')

   90 IF(JW(LUP).GT.0) THEN
         WRITE(JW(LUP),103) LUNDA
      END IF
      CALL BABEND('BDAWR')

  100 RETURN
  101 FORMAT(' -----------  DA-UNIT',I3,'   BANK ',A4,I10,' ADDED')
  102 FORMAT(' -----------  DA-UNIT',I3,'   BANK ',A4,I10,' REPLACED')
  103 FORMAT(' -----------  DA-UNIT',I3,' NO MORE SPACE ON DATA SET',
     +   ' - PROGRAM STOPS WITH ABEND')
  105 FORMAT(' -----------  DA-UNIT',I3,' INCONSISTENCY FOUND BETWEEN ',
     +   'DIRECTORY AND DATA RECORDS FOR BANK ',A4,I9/
     +   13X,'TO REMOVE INCONSISTENCY THE BANK IS DELETED AND THE ',
     +   'PROGRAM STOPS')
  107 FORMAT(1X,10I12)
      END
