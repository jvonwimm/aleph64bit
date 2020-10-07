*DK capsto
      SUBROUTINE CAPSTO (IGTRA)
C ------------------------------------------------------------------
C       M.N Minard 880509
C - fill bank CAPA for geantino with track# IGTRA
C - called by CAPGET                                from this .HLB
C - call   ALTELL                                   from ALEPHLIB
C ========================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "capajj.h"
#include "capano.h"
C -------------------------------------------------------------------
C
C - it is a geantino, IF the bank CAPA,NR=IGTRA does not exist THEN error
      JCAPA = NLINK ('CAPA',IGTRA)
      IF (JCAPA.EQ.0) THEN
         CALL ALBOS ('CAPSTO: bank CAPA is missing ',11,'NEXT')
      ELSE
         KCAPA = JCAPA + LMHLEN
C        fill bank 'CAPA' using common /CAPANO/
         IW(KCAPA+JCAPST) = NATGER
         IW(KCAPA+JCAPPT) = METHCO
         RW(KCAPA+JCAPTE) = TOTNRJ
         RW(KCAPA+JCAPEM) = EMGNRJ
         RW(KCAPA+JCAPEA) = EMALFA
         RW(KCAPA+JCAPEB) = EMBETA
         RW(KCAPA+JCAPA1) = EMALM1
         RW(KCAPA+JCAPER) = EMAEXP
         RW(KCAPA+JCAPEF) = EMFACT
         RW(KCAPA+JCAPED) = EMUDP0
         RW(KCAPA+JCAPEZ) = EMUDP2
         RW(KCAPA+JCAPHE) = HADNRJ
         RW(KCAPA+JCAPHA) = HALPHA
         RW(KCAPA+JCAPHB) = HABETA
         RW(KCAPA+JCAPHD) = HADRAY
         RW(KCAPA+JCAPHP) = HAPUIS
         IW(KCAPA+JCAPNA) = INTCHA(EMNAME)
         RW(KCAPA+JCAPSX) = SMAXLR
         RW(KCAPA+JCAPSA) = SMAXLA
      ENDIF
      END
#endif
