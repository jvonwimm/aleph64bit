      SUBROUTINE EFNICR(X,CL,RW,ST,WRONG)
C.----------------------------------------------------------------------
CKEY ECALDES TOWER ROW COLUMN STACK / USER
C     H.Videau      Creation 23/11/88   Modification 14/03/90
C! Finds column, row and stack floating numbers
C  Looks for the column,the row and the stack containing the point X.
C  Interpolates between the limiting planes to get a REAL position
C  The pathological storeys are not taken into account.
C  We check that the point is correct by calling ETZSMD.
C   Input :
C           X       point coordinates in  the Aleph system
C   Output: CL    column number   i      REAL
C           RW    row   number    j      REAL
C           ST    stack number    k      REAL
C           WRONG error flag      :      INTEGER
C                                     0  CORRECT
C                                     1  wrong SUBCOMPONENT
C                                     2  wrong MODULE
C                                     3  wrong REGION
C                                     4  wrong COLUMN
C                                     6  wrong ROW
C                                     7  wrong STACK
C                                     8  wrong ZONE
C   Calls: EPLSCC,EPLSCN,EPLSCF,EPLSQF,ETZSMD
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C   Input variables
      REAL X(ECALLS-1)
C   Output variables
      REAL CL,RW,ST
      INTEGER WRONG
C   Called functions
      INTEGER EPLSCC,EPLSCN
      REAL    EPLSCF, EPLSQF
C   Locales
      INTEGER RG,SS,BARIL,I,LRW
      INTEGER SC,MD,PL,IST,ZONE(3)
      REAL LOC1,LOC2,TESTC,TESTS
      LOGICAL NATZON
      PARAMETER (BARIL=2)
C---------------------------------
      WRONG=0
C----------------------------------
C    search for the subcomponent
C  --    SC = EFNDSC( X)  --
      SC=1
      DO 1 I=2,ECALSC
        IF(ESCOLI(1,I)*X(1)+ESCOLI(2,I)*X(2)+
     &     ESCOLI(3,I)*X(3)+ESCOLI(4,I)     .LT.0.) SC=I
 1    CONTINUE
C  error diagnostic
      IF(SC.EQ.0) THEN
                  WRONG=1
                  GO TO 999
                  END IF
C----------------------------------
C   search for the module
C  --    MD = EFNDMD( SC,X)   --
      MD=EPLSCC(ESCOMD(1,1,SC),ECALMD,EMODPC(1,1),X)
C  error diagnostic
      IF(MD.EQ.0.OR.MD.GT.ECALMD) THEN
                  WRONG=2
                  GO TO 999
                  END IF
C----------------------------------
C   Tests if the point is in the sensitive region
      CALL ETZSMD(X,SC,MD,NATZON,ZONE)
      IF(.NOT.NATZON)  WRONG=8
C ---------------------------------
                         IF(SC.EQ.BARIL) THEN
                               RG=ECALRG
                               SS=1
                                         ELSE
C  --------------------------------
C    search for the sector
C  --    SS=EFNDSS(SC,MD,X,SYSTM)  --
      IF(X(1)*EALISE(1,MD,SC)+X(2)*EALISE(2,MD,SC)+
     &   X(3)*EALISE(3,MD,SC)+     EALISE(4,MD,SC).LE.0)
     &THEN
          SS=1
      ELSE
          SS=2
      END IF
C
C----------------------------------
C   search for the region
C  --    RG=EFNDRG(SC,MD,X,SYSTM)  --
         RG=EPLSCN(EALIRW(1,1,SS,MD,SC),ECALXG+1,EXRGPC(1,1),X)
         IF(RG.GE.ECALXG+1.OR.RG.EQ.0) THEN
                            RG=0
                            ELSE
                        RG=EXRGER(RG)
                            END IF
                                         END IF
C  error diagnostic
       IF(RG.EQ.0) THEN
                   WRONG = 3
                   GO TO 999
                   END IF
C
C---------------------------------
C   search for the column
C  --    CL=EFNDCL(SC,MD,X,SYSTM)  --
C  finds the column in the region
       CL=EPLSCF(EALICL(1,1,MD,SC),EREGCN(RG)+1,ECOLPC(1,EREGFR(RG)),X)
C  error diagnostic
      IF( CL.GE.EREGCN(RG)+1 .OR. CL.LT.1.) THEN
                   WRONG = 4
                   GO TO 999
                   END IF
C
C  Takes into account the offset in the numbering of the columns
C  between the end caps and the barrel
      CL=CL+NINT((MD-1+.5*ESCOCF(SC))*EREGCN(RG))
      IF(CL.LT.1.)   CL=CL+ECALMD*EREGCN(RG)
C--------------------------------
C   search for the row
C --     RW=EFNDLG(SC,MD,X,SYSTM)  --
        LRW=ESCOLS(SC)-ESCOFR(SC)+1
      RW=EPLSQF(EALIRW(1,1,SS,MD,SC),LRW,EXROPC(1,ESCOFR(SC)),X)
C    point hors limites
      IF(RW.GT.LRW.OR.RW.LT.1.) THEN
        RW=RW+ESCORF(SC)
        WRONG = 6
        GO TO 999
      END IF
C      introduction de l'offset de sous-composante.
      RW=RW+ESCORF(SC)
C
C--------------------------------
C   Search for the stack
C  --    ST=EFNDST(SC,MD,X,SYSTM) mais flottant --
       LOC1= EALIPL(1,1,MD,SC)*X(1)+EALIPL(2,1,MD,SC)*X(2)+
     &       EALIPL(3,1,MD,SC)*X(3)+EALIPL(4,1,MD,SC)
       LOC2= EALIPL(1,2,MD,SC)*X(1)+EALIPL(2,2,MD,SC)*X(2)+
     &       EALIPL(3,2,MD,SC)*X(3)+EALIPL(4,2,MD,SC)
      DO 2 IST=1 ,ECALST
         TESTC=-LOC2*ESSCPC(1,IST,SC)+LOC1*ESSCPC(2,IST,SC)
           IF(TESTC.GE.0.) GO TO 100
 2    CONTINUE
      IST=ECALST+1
 100    CONTINUE
      IST=IST-1
      IF(IST.EQ.0) THEN
              WRONG=7
                  IST=1
                   END IF
C    teste que l'on n'est pas au dela du stack
       TESTS= -ESSCPS(1,IST,SC) * LOC2 + ESSCPS(2,IST,SC) * LOC1
      IF(TESTS.LT.0.) WRONG=7
       TESTC=-LOC2*ESSCPC(1,IST,SC)+LOC1*ESSCPC(2,IST,SC)
      ST=FLOAT(IST) -TESTC/(-TESTC+TESTS)
C--------------------------------
 999   CONTINUE
      END
#endif
