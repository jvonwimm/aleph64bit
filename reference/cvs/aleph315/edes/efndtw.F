      SUBROUTINE EFNDTW(X,SYSTM,CL,RW,ST,SC,MD,PL,WRONG)
C.----------------------------------------------------------------------
CKEY ECALDES TOWER ROW COLUMN STACK / USER
C     H.Videau      Creation 23/10/87   Modification 14/03/90
C! Finds column, row and stack numbers
C  Looks for the column,the row and the stack containing the point X.
C   Input :
C           X       point coordinates in the Aleph system  REAL
C           SYSTM   defines the reference system (not implemented yet)
C                                        CHARACTER*5
C   Output:
C           SC    subcomponent number    INTEGER
C           MD    module number          INTEGER
C           CL    column number   i      INTEGER
C           RW    row   number    j      INTEGER
C           ST    stack number    k      INTEGER
C           PL    plane number           INTEGER
C           WRONG character flag  :      CHARACTER*16
C                                 CORRECT
C                                 SUBCOMPONENT
C                                 MODULE
C                                 REGION
C                                 COLUMN
C                                 PLANE
C                                 ROW
C                                 STACK
C ACHTUNG!!! the notion of i and j is different in Ecal and Hcal
C last parameter is CHARACTER in Ecal and INTEGER in Hcal
C   Calls: EPLSCC,EPLSCN, EPLSQL
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C
C   Input variables
      REAL X(ECALLS-1)
      CHARACTER*(*) SYSTM
C   Output variables
      INTEGER SC,MD,CL,RW,ST,PL
      CHARACTER*(*) WRONG
C   Called functions
      INTEGER EPLSCC,EPLSCN, EPLSQL
C   Locales
      INTEGER RG,SS,BARIL,I,LRW
      REAL LOC1,LOC2,TEST
      PARAMETER (BARIL=2)
C---------------------------------
      WRONG='CORRECT'
C----------------------------------
C    search for the subcomponent
C  --    SC = EFNDSC( X)  --
      SC=1
      DO 1 I=2,ECALSC
        IF(ESCOLI(1,I)*X(1)+ESCOLI(2,I)*X(2)+
     &     ESCOLI(3,I)*X(3)+ESCOLI(4,I)    .LT.0.) SC=I
 1    CONTINUE
C  error diagnostic
      IF(SC.EQ.0) THEN
                  WRONG='SUBCOMPONENT'
                  GO TO 999
                  END IF
C----------------------------------
C   search for the module
C  --    MD = EFNDMD( SC,X)   --
       MD=EPLSCC(ESCOMD(1,1,SC),ECALMD,EMODPC(1,1),X)
C  error diagnostic
      IF(MD.EQ.0.OR.MD.GT.ECALMD) THEN
                  WRONG='MODULE'
                  GO TO 999
                  END IF
C----------------------------------
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
C ------------------------------------
C   search for the region
C  --    RG=EFNDRG(SC,MD,X,SYSTM)  --
         RG=EPLSCN(EALIRW(1,1,SS,MD,SC),ECALXG+1,EXRGPC(1,1),X)
         IF(RG.GT.0.AND.RG.LT.ECALXG+1) THEN
                        RG=EXRGER(RG)
                            ELSE
C  error diagnostic
                   WRONG = 'REGION'
                        RG=1
                            END IF
                                         END IF
C
C---------------------------------
C   search for the column
C  --    CL=EFNDCL(SC,MD,X,SYSTM)  --
C  finds the column in the region
       CL=EPLSQL(EALICL(1,1,MD,SC),EREGCN(RG)+1,ECOLPC(1,EREGFR(RG)),X)
C  error diagnostic
      IF( CL.GE.EREGCN(RG)+1 .OR. CL.LE.0 ) THEN
                   WRONG = 'COLUMN'
                   END IF
C
C  Takes into account the offset in the numbering of the columns
C  between the end caps and the barrel
      CL=CL+NINT((MD-1+.5*ESCOCF(SC))*EREGCN(RG))
      IF(CL.LE.0)   CL=CL+ECALMD*EREGCN(RG)
      IF(CL.GT.ECALMD*EREGCN(RG)) CL=CL-ECALMD*EREGCN(RG)
C---------------------------------
C  recherche du plan
C --     PL=EFNDPL(SC,MD,X,SYSTM)  --
       PL=EPLSQL(EALIPL(1,1,MD,SC),ECALPL+1,EPSCPC(1,1,SC),X)
         IF(PL.EQ.0.OR.PL.GE.ECALPL+1) THEN
                     WRONG='PLANE'
                     END IF
C
C--------------------------------
C   search for the row
C --     RW=EFNDLG(SC,MD,X,SYSTM)  --
        LRW=ESCOLS(SC)-ESCOFR(SC)+1
      RW=EPLSQL(EALIRW(1,1,SS,MD,SC),LRW,EXROPC(1,ESCOFR(SC)),X)
C    point hors limites
      IF(RW.GE.LRW.OR.RW.LE.0) THEN
                   WRONG = 'ROW'
      RW=RW+ESCORF(SC)
                                ELSE
C      introduction de l'offset de sous-composante.
      RW=RW+ESCORF(SC)
C   traitement des zones pathologiques du baril.
                  IF(SC.EQ.BARIL.AND.PL.NE.0)   THEN
      IF(RW.LE.EPLNPI(ECALPL).OR.RW.GE.EPLNPS(ECALPL)) THEN
         IF(RW.GE.EPLNPS(ECALPL)) RW=MIN(RW,EPLNPS(PL))
         IF(RW.LE.EPLNPI(ECALPL)) RW=MAX(RW,EPLNPI(PL))
                                                       END IF
                                               END IF
                                END IF
C
C--------------------------------
C   Search for the stack
C  --    ST=EFNDST(SC,MD,X,SYSTM)  --
      IF(PL.EQ.0) THEN
                  ST = 0
                 WRONG = 'STACK'
      ELSE IF(PL.GT.ECALPL) THEN
                  ST = ECALST+1
                 WRONG = 'STACK'
                  ELSE
              ST=EPLNES(PL)
       LOC1= EALIPL(1,1,MD,SC)*X(1)+EALIPL(2,1,MD,SC)*X(2)+
     &       EALIPL(3,1,MD,SC)*X(3)+EALIPL(4,1,MD,SC)
       LOC2= EALIPL(1,2,MD,SC)*X(1)+EALIPL(2,2,MD,SC)*X(2)+
     &       EALIPL(3,2,MD,SC)*X(3)+EALIPL(4,2,MD,SC)
       TEST= -ESSCPS(1,ST,SC) * LOC2 + ESSCPS(2,ST,SC) * LOC1
                  END IF
C--------------------------------
 999   CONTINUE
      END
#endif
