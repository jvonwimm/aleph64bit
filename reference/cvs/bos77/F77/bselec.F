      LOGICAL FUNCTION BSELEC (IWORD,MASK)
C --------------------------------------------------------------
C - F.Ranjard - 891115  from V.Blobel BINSEL code
C! select a record using the class word and the user mask
C  in the BOS version a record is selected when the user mask is not 0
C  and the AND(user mask,class word) is not 0
C  the user can have its own BSELEC routine
C
C - Input   : IWORD  / I = record class word
C             MASK   / I = user mask
C - Output  : BSELEC / L = .TRUE. if the record is selected
C --------------------------------------------------------------
#include "boscom.h"
C ----------------------------------------------------------------
      BSELEC = .TRUE.
      IF (MASK.NE.0 .AND. IAND(MASK,IWORD).EQ.0) BSELEC = .FALSE.
      END
