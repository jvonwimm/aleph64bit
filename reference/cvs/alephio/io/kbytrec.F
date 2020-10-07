      INTEGER FUNCTION KBYTREC (ATYPE,FDEVI)
C ----------------------------------------------------------------
CKEY ALEF RECORD LENGTH
C - F.Ranjard - 920402
C!  return the aleph record length in bytes
C - Input:  ATYPE   / A  = aleph file type
C                          'EPIO', 'NAT', 'DAF', 'CARD', 'EDIR'
C           FDEVI   / A  = device name
C - Output: KBYTREC / I  = logical record length in bytes
C
C - called by USER, AOPENW, AWRTAP, AOPEN
C   -------------------------------------------------
#ifndef DOC
#include "albitw.h"
      CHARACTER*(*) ATYPE, FDEVI
C --------------------------------------------------------------
      LREC = JULREC (ATYPE,FDEVI)
      IF (ATYPE(1:3).EQ.'DAF') THEN
         LREC = LREC * NBYTW
      ENDIF
      KBYTREC = LREC
      END
#endif
