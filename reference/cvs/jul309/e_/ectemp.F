      SUBROUTINE ECTEMP
C=======================================================================
C!    Creates temporary banks ECTE and HCTE where the
C!    energies subtracted from each storeys are kept
C-
C-     Authors : M.N. Minard , M. Pepe       26/02/89
C-     Modified: M.N  Minard                 04/01/91
C?
C!======================================================================
#ifndef DOC
#include "ecnmsk.h"
#include "esdajj.h"
#include "eclujj.h"
#include "hsdajj.h"
      PARAMETER (NWECTE=2,NWHCTE=2)
#include "bcs.h"
      DATA NAECTE /0/
#include "bmacro.h"
C
      IF (NAECTE.EQ.0) THEN
        NAECTE = NAMIND('ECTE')
        NAHSDA = NAMIND('HSDA')
        NAHCTE = NAMIND('HCTE')
      ENDIF

C- Counts number of storeys (ESDA)
C
      KESDA = IW (NAESDA)
      NESDA = 0
      IF (KESDA.GT.0) NESDA = LROWS (KESDA)
      IF ( NESDA.EQ.0 ) GO TO 150
C
C- Creates bank ECTE for that cluster
C
      NESDA  = LROWS(KESDA)
      NLENG  = NWECTE*NESDA+LMHLEN
      CALL AUBOS('ECTE',0,NLENG,KECTE,IG)
      IF (KECTE.EQ.0) GOTO 900
      CALL BLIST(IW,'T+','ECTE')
      IW(KECTE+LMHCOL) = NWECTE
      IW(KECTE+LMHROW)=0
C
C-   Build temporary bank HCTE
C
  150 CONTINUE
      KHSDA = IW ( NAHSDA )
      NHSDA = 0
      IF (KHSDA.GT.0) NHSDA = LROWS(KHSDA)
      IF ( NHSDA.EQ.0 ) GO TO 950
C
C-    Lift temporary bank if it does not exist
C
      NLENG = NWHCTE*NHSDA+LMHLEN
      CALL AUBOS('HCTE',0,NLENG,KHCTE,IGARB)
      IF (KHCTE.EQ.0) GO TO 900
      CALL BLIST(IW,'T+','HCTE')
      IW(KHCTE+LMHCOL) = NWHCTE
      IW(KHCTE+LMHROW) = 0
      RETURN
  900 CALL RERROR('ECTEMP',3,'Problem to lift temporary bank')
  950 CONTINUE
      END
#endif
