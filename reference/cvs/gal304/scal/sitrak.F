*DK sitrak
      SUBROUTINE SITRAK
C--------------------------------------------------------------
C! - controls tracking in SCAL
C. - B.Bloch-Devaux     910115
C. - Called by  SIHIT                            from this .HLB
C. - Calls  SIXTOA,CAHIST                        from this .HLB
C -----------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "trkcom.h"
#include "sistat.h"
#include "siconst.h"
#include "sinamc.h"
      DIMENSION POS(3)
C   This is the value to be deposited per Gev for 20Kev tracking cut
C--------------------------------------------------------------
C-    not 1st step in sensitive medium
      IF(ITRKEL(8).NE.1) THEN
C-    Get subcomponents from track elemnt position
         POS(1) = TRKELE(1)
         POS(2) = TRKELE(2)
         POS(3) = TRKELE(3)
         ZPOS = POS(3) +SIGN ( 0.1,TRKELE(3))
         CALL SIXTOA(IAD,IOR,POS(1),POS(2),ZPOS  ,IOK)
         IF (IOK.NE.0 ) THEN
            IF (NSIPRT.GT.1) THEN
                WRITE(LOUTIO,*) ' +++ SITRAK : the following space ',
     &          'point is outside the sensitive area of crystals: ',
     &          'X,Y,Z,R ',
     &          POS ,SQRT(POS(1)*POS(1)+POS(2)*POS(2)),IOK
            ENDIF
         ELSE
C-    Deposit energy on the relevant address (MEV)
            IDE = IFIX(TRKELE(12)*1000./SIPGEV)
            IF (IDE.GT.0 ) THEN
               CALL CAHIST(NASIHI,IAD,IOR+1,IDE)
C-    Count energy elements entering SICAL
               ESICOU(1) = ESICOU(1) +TRKELE(12)*1000./SIPGEV
               IF ( TRKELE(3).GT.0.) THEN
                  ESICOU(2) = ESICOU(2) +TRKELE(12)*1000./SIPGEV
               ELSE
                  ESICOU(3) = ESICOU(3) +TRKELE(12)*1000./SIPGEV
               ENDIF
               IF (NSIPRT.GT.2) WRITE(LOUTIO,1100)IAD,IOR+1,IDE
            ENDIF
         ENDIF
      ENDIF
      RETURN
 1100 FORMAT (' ++++ SITRAK : Triplet address ',I8,' position ',I5,
     &        ' Energy deposit ',I10,' Mev ')
      END
#endif
