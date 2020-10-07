*DK capget
      SUBROUTINE CAPGET
C ------------------------------------------------------------------
C - F.Ranjard - 880505
C! Geantino track element : get CAPA bank
C! IF 1st entry (bank does not exist) THEN
C!    create the CAPA bank with the bank# = track# IGTRA
C!    fill it with the shower parameters
C! ELSE
C!    increment the total rad. length and abs. length
C!    copy the CAPA bank to common /CAPANO/
C! ENDIF
C - called by GUSTEP                                from this .HLB
C - calls ALBOS, ALTELL                             from ALEPHLIB
C         CAHPAR, CAEPAR, CAPSTO                    from this .HLB
C ========================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "capajj.h"
#include "capano.h"
#include "ehpash.h"
#include "gcmate.h"
#include "gckine.h"
#include "gctrak.h"
       CHARACTER*4 CHAINT
       EXTERNAL CHAINT
C -------------------------------------------------------------------
         JCAPA = NLINK ('CAPA',IGTRA)
         IF (JCAPA .EQ. 0.) THEN
            CALL ALBOS ('CAPA',IGTRA,LCAPAA+LMHLEN,JCAPA,IGARB)
            IW(JCAPA+LMHCOL) = LCAPAA
            IW(JCAPA+LMHROW) = 1
            CALL BLIST (IW,'T+','CAPA')
            CALL CAHPAR (GETOT,IGPART)
            CALL CAEPAR
            CALL CAPSTO (IGTRA)
         ELSE
            KCAPA = JCAPA + LMHLEN
C
C-          TEST IF ELECTRON GEANTINO ABSORBED
C
            RW(KCAPA+JCAPTX) = RW(KCAPA+JCAPTX) + GSTEP/GRADL
            RW(KCAPA+JCAPTL) = RW(KCAPA+JCAPTL) + GSTEP/GABSL
C              fill common /CAPANO/
            NATGER = IW(KCAPA+JCAPST)
            METHCO = IW(KCAPA+JCAPPT)
            TOTNRJ = RW(KCAPA+JCAPTE)
            TINOX0 = RW(KCAPA+JCAPTX)
            TINOL0 = RW(KCAPA+JCAPTL)
            EMGNRJ = RW(KCAPA+JCAPEM)
            EMALFA = RW(KCAPA+JCAPEA)
            EMBETA = RW(KCAPA+JCAPEB)
            EMALM1 = RW(KCAPA+JCAPA1)
            EMAEXP = RW(KCAPA+JCAPER)
            EMFACT = RW(KCAPA+JCAPEF)
            EMUDP0 = RW(KCAPA+JCAPED)
            EMUDP2 = RW(KCAPA+JCAPEZ)
            HADNRJ = RW(KCAPA+JCAPHE)
            HALPHA = RW(KCAPA+JCAPHA)
            HABETA = RW(KCAPA+JCAPHB)
            HADRAY = RW(KCAPA+JCAPHD)
            HAPUIS = RW(KCAPA+JCAPHP)
            HGAMMA = RW(KCAPA+JCAPGA)
            EMNAME = CHAINT(IW(KCAPA+JCAPNA))
            SMAXLR = RW(KCAPA+JCAPSX)
            SMAXLA = RW(KCAPA+JCAPSA)
            CALL CAABSB(EMGNRJ,EMBETA,EMALFA,TINOX0,IER)
            IF ( IER.NE.0) THEN
               IGSTOP = 3
               GO TO 999
            ENDIF
         ENDIF
 999     CONTINUE
         RETURN
      END
#endif
