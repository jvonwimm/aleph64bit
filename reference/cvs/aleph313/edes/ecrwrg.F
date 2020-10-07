      SUBROUTINE ECRWRG(RW,RG,MXCOL)
C.---------------------------------------------------------------
CKEY ECALDES PLANE ROW REGION / USER
C  M.Rumpf          Creation 09/87      Modification H.Videau 01/88
C! Row # -> Region #,Max Columns
C   Input :
C           RW    row number in Theta  INTEGER
C   Output:
C           RG    region #   [1 -> 4]  INTEGER
C           MXCOL number of column in region RG
C   Called by USER and GALEPH
C.----------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "ecccom.h"
C    Input variables
       INTEGER RW
C    Output variables
       INTEGER RG,MXCOL
C
C      on pourrait construire la relation produit EXRO -> EREG
       RG = EXRGER(EROWEX(EXROER(RW)))
       MXCOL = ECALMD * EREGCN(RG)
       END
#endif
