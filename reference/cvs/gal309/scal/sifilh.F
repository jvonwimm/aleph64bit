*DK sifilh
      SUBROUTINE SIFILH(IPR)
C.-----------------------------------------------------------------
CKEY SICAL FILL HISTO
C! Fill SCAL histograms
C  B. BLOCH December 91
C. - Input  IPR  flag to select bank content to be analysed
C.               1 = analyse SIHT
C.               2 = analyse SIDI , SIFO
C. - Called by  SIASIG,SIDIGI                    from this .HLB
C.-----------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "sinamc.h"
      PARAMETER ( JSIAD = 1 , JSIDE = 2 )
      DIMENSION EPAD(2),EPLA(12,2),NPAD(12,2)
#include "bmacro.h"
C
C    filling of histograms
C
      IF (IPR.EQ.1) THEN
C PAD data  from SIHT bank
         KSI = IW(NASIHT)
         IDH = 900
      ELSE IF ( IPR.EQ.2) THEN
C PAD data  from SIDI bank
         KSI = IW(NASIDI)
         IDH = 910
      ENDIF
C Get weight
      W = 1.
      CALL VZERO(EPAD,2)
      CALL VZERO(EPLA,24)
      CALL VZERO(NPAD,24)
      NPAC = 0
      NPAC2= 0
      IF(KSI.GT.0) THEN
         DO 10 I=1,LROWS(KSI)
         DO 20 IS = 1,3
            E  = ITABL(KSI,I,JSIDE+IS-1)*0.001
            IF ( E.GT.0. ) THEN
               IA = ITABL(KSI,I,JSIAD)
               CALL SIDCOD(IA,IS-1,MODU,ISTA,IPHI,IRAD)
               EPLA(ISTA,MODU) = EPLA(ISTA,MODU) + E
               NPAD(ISTA,MODU) = NPAD(ISTA,MODU) + 1
               EPAD(MODU) = EPAD(MODU) + E
               NPAC = NPAC + 1
               IF ( E.GT.0.02 ) NPAC2 = NPAC2 +1
            ENDIF
   20    CONTINUE
   10    CONTINUE
         CALL HFILL(IDH+ 1,EPAD(1)+EPAD(2),DUM,W)
         CALL HFILL(IDH+ 2,EPAD(1),EPAD(2),W)
         CALL HFILL(IDH+ 3,FLOAT(NPAC),DUM,W)
         CALL HFILL(IDH+ 6,FLOAT(NPAC2),DUM,W)
         DO 11 IPL=1,12
         DO 11 IM = 1,2
           IF ( EPAD(IM).GT.0.) THEN
              CALL HFILL(IDH+ 5,FLOAT(NPAD(IPL,IM)),FLOAT(IPL),W)
              CALL HFILL(IDH+ 4,FLOAT(IPL),DUM,W*EPLA(IPL,IM)/EPAD(IM))
           ENDIF
  11     CONTINUE
      ENDIF
      RETURN
      END
#endif
