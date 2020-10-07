      SUBROUTINE ECRUNQ ( NCL1 , NCL2 , NAUTO )
C----------------------------------------------------------------------
C!  - Storey cleaning and Autopeds info to POT
C!     Bank output ECRQ
C!
C!     Lee F. Thompson. 23rd April 1990.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      EXTERNAL NAMIND
#include "ecrqjj.h"
      INTEGER NCL1(36) , NCL2(36) , NAUTO(36) , NECRQ
      DATA NECRQ /36/
#include "bmacro.h"
C
C-    Build ECRQ bank
C
      KECRQ = NDROP('ECRQ',0)
      NLENG = NECRQ*LECRQA + LMHLEN
      CALL AUBOS ('ECRQ', 0, NLENG , KECRQ , IGARB )
      IF ( IGARB .EQ.2 ) GO TO 990
      CALL BLIST (IW,'E+','ECRQ')
      IW(KECRQ+LMHCOL) = LECRQA
      IW(KECRQ+LMHROW) = 0
C
C-    Pack info into two data words per module.
C-    Word 1 has Module Number (bits 1-6), Autopeds activity
C-    (bits 7-20). Word 2 has number of storeys cleaned after
C-    LCAL trigger procedure (bits 1-14) and after wire planes
C-    procedure (bits 15-28).
C
      IECRQ = 0
      DO 10 MOD = 1, NECRQ
          IF (NCL1(MOD).GT.0.OR.NCL2(MOD).GT.0.OR.NAUTO(MOD).GT.0)THEN
              IECRQ = IECRQ + 1
              JECRQ = KROW(KECRQ,IECRQ)
              IF ( NAUTO ( MOD ) .GE. 16384 ) THEN
                  CALL RERROR ( 'ECRUNQ' , 1 ,
     *                 'PAD Autopeds activity exceeds 16383' )
                  NAUTO ( MOD ) = 16833
              END IF
              IF ( NCL1 ( MOD ) .GE. 16384 ) THEN
                  CALL RERROR ( 'ECRUNQ' , 2 ,
     *                 'PAD LCAL cleaning activity exceeds 16383' )
                  NCL1 ( MOD ) = 16833
              END IF
              IF ( NCL2 ( MOD ) .GE. 16384 ) THEN
                  CALL RERROR ( 'ECRUNQ' , 3 ,
     *             'PAD wire cleaning activity exceeds 16383' )
                  NCL2 ( MOD ) = 16833
              END IF
              IW(JECRQ+1) = MOD + ISHFT(NAUTO(MOD),6)
              IW(JECRQ+2) = NCL1(MOD) + ISHFT(NCL2(MOD),14)
          END IF
   10 CONTINUE
      IW(KECRQ+LMHROW) = IECRQ
      CALL AUBPRS('ECRQ')
      GO TO 900
  990 CALL RERROR(' ECRUNQ',1,' no space for bank ECRQ')
  900 CONTINUE
      RETURN
      END
#endif
