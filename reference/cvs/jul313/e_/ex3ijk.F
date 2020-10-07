      SUBROUTINE EX3IJK(X,IPH,JTH,ST,RG,IFLAG,SC,MD)
C----------------------------------------------------------------------
C!If point X in ECAL gets IPH,JTH,ST..
C     ?                                                         ?
C                                       modified 25/03/88 by H.Videau
C Computes the subcomponent SC and module MD containing the point X
C Checks if the point X is in the sensitive region of the module.
C See IFLAG.
C Computes the row, column, stack and region of the module containing X
C      INPUT       X(3)   point
C      OUTPUT      IPH    column number. phi
C                  JTH    row number   . theta
C                  ST     stack number
C                  RG     region number
C                  IFLAG    0  point inside ECAL
C                           1  in the dead region of the overlap
C                           2  in the sensitive region
C                           3  in the endcap hole
C                           4  outside ECAL
C                  SC     subcomponent number
C                  MD     module number
C----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "rlunit.h"
      CHARACTER*120 MESAG
C   Input variables
      REAL X(*)
C   Output variables
      INTEGER IPH,JTH,ST,RG,IFLAG,SC,MD
C   Externals
C   Locales
      CHARACTER*16 WRONG
      LOGICAL INCAL
      INTEGER PL,J ,MXCOL
      INTEGER IZON(3)
      INTEGER ICASE(-1:1,-1:1,3)
C    ICASE(Z,R,SC)
C
      DATA ICASE/
     + 0,3,4,
     + 0,2,4,
     + 4,4,4,
     +
     + 0,0,0,
     + 1,2,1,
     + 4,4,4,
     +
     + 4,3,0,
     + 4,2,0,
     + 4,4,4/
C
      CALL EFNDTW(X,'ALEPH',IPH,JTH,ST,SC,MD,PL,WRONG)
      CALL ETZSMD(X,SC,MD,INCAL,IZON)
      IFLAG=ICASE(IZON(1),IZON(2),SC)
      IF(JTH.EQ.0)      THEN
         RG = 0
      ELSE
         CALL ECRWRG(JTH,RG,MXCOL)
      ENDIF
      IF(.NOT.INCAL) GO TO 999
      IF(WRONG.EQ.'CORRECT') GO TO 999
C Point in PVC layer between stacks
      IF(WRONG.EQ.'STACK'.AND.ST.GT.0.AND.ST.LE.3) GO TO 999
C There is a problem!
      WRITE(MESAG,900) (X(J),J=1,3),
     &               IPH,JTH,ST,SC,MD,PL
 900  FORMAT(3E13.5,6(1X,I4))
      CALL RERROR( 'EX3IJK',1,'Wrong stack;X,IPH,JTH,ST,SC,MD,PL->')
      CALL RERROR( 'EX3IJK',2,MESAG)
      IFLAG=4
 999  CONTINUE
      RETURN
      END
#endif
