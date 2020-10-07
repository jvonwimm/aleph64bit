      INTEGER FUNCTION EFNDCL(SC,MD,X,SYSTM)
C.----------------------------------------------------------------------
CKEY ECALDES COLUMN / USER
C     H.Videau      Creation 15/12/85   Modification 04.02/88
C! Finds column number
C  Looks for the column containing the point X in the module MD of
C  the subcomponent SC.
C   If the module number is wrong or if there is some discrepancy
C  between the module limits and the column limits ( precision or
C  alignement) EFNDCL can correspond to a column outside (by 1) the
C  module.
C   Input :
C           SC      sub component #           INTEGER
C           MD      module #                  INTEGER
C           X       point coordinates in      REAL(3)
C           SYSTM   coordinate system name    CHARACTER * 5
C                   ('ALEPH' for example)
C   Output:
C           EFNDCL  column index 1<= EFNDCL<= 384
C   Calls: EPLSQL,EFNDRG
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C  input variables
      INTEGER SC,MD
      REAL X(ECALLS-1)
      CHARACTER*5 SYSTM
C  functions called
      INTEGER EPLSQL,EFNDRG
C  local variables
      INTEGER RG,CL
C
C
C     finds the region of the module MD of the subcomponent SC
C     which contains the point X
       RG=EFNDRG(SC,MD,X,SYSTM)
       IF(RG.EQ.0) THEN
           EFNDCL=0
           GO TO 999
       END IF
C
C  finds the column in the region
      IF(SYSTM.EQ.'ALEPH')    THEN
       CL=EPLSQL(EALICL(1,1,MD,SC),EREGCN(RG)+1,ECOLPC(1,EREGFR(RG)),X)
                              ELSE
       CL=EPLSQL(ELOCCL(1,1),EREGCN(RG)+1,ECOLPC(1,EREGFR(RG)),X)
                              END IF
C
C  Takes into account the offset in the numbering of the columns
C  between the end caps and the barrel
      EFNDCL=CL+NINT((MD-1+.5*ESCOCF(SC))*EREGCN(RG))
      IF(EFNDCL.LE.0)  EFNDCL=EFNDCL+ECALMD*EREGCN(RG)
      IF(EFNDCL.GT.ECALMD*EREGCN(RG)) EFNDCL=EFNDCL-ECALMD*EREGCN(RG)
C
  999 RETURN
C
      END
#endif
