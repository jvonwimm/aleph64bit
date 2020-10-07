*DK cahmod
      SUBROUTINE CAHMOD ( IPAR,IHPLN)
C
C   M.N Minard 880508
C   1 st : lift working bank HCVL
C          initialisation start volume in HCAL
C          HCVL allows to store volume parameter for geantino tracking
C   ELSE
C          Calculate absorption and radiation length in non sensistive
C          volume
C          Fill common CAHTRA at the end of volume
C - Output arguments:
C              IPAR = 1 if bank HCVL and common /CAHTRA/ are filled
C              IHPLN= plane # where the shower starts
C-  Called  in HCSHOW
C-  Calls HCSETP
C
C   Modify by M.N. Minard    29/1/93
C ------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "capajj.h"
#include "capano.h"
#include "gcmate.h"
#include "hcloc.h"
#include "hccong.h"
#include "hcgega.h"
#include "trkcom.h"
#include "cahtra.h"
#include "hcvljj.h"
      LOGICAL FPSEN , FCSEN
C -------------------------------------------------------------
C--   INITIALISATION
C
      IPAR = 0
C     current volume sensitive flag
      FCSEN = ITRKEL(7).GT.0
      JHCVL = NLINK ('HCVL',ITRKEL(1))
C
C - IF 1st entry THEN
C      create bank 'HCVL',NR=track#
C      fill it with absoption and radiation lengths
C      set the sensitive volume flag to 0 (volume not sensitive)
C
      IF (JHCVL .EQ. 0) THEN
C
         CALL ALBOS('HCVL',ITRKEL(1),LHCVLA+LMHLEN,JHCVL,IGARB)
         IW ( JHCVL+LMHCOL ) = LHCVLA
         IW ( JHCVL+LMHROW ) = 1
         CALL BLIST (IW,'T+','HCVL')
         KHCVL = JHCVL + LMHLEN
C
         DO 10 IL = 1, LETRK
            RW(KHCVL+JHCVTE+IL-1) = TRKELE (IL)
  10     CONTINUE
         RW ( KHCVL+JHCVRV ) = TRKELE (11)/GRADL
         RW ( KHCVL+JHCVAV ) = TRKELE (11)/GABSL
         RW ( KHCVL+JHCVTL ) = TRKELE (11)
         RW ( KHCVL+JHCVTA ) = TINOL0 - TRKELE (11)/GABSL
         RW ( KHCVL+JHCVTR ) = TINOX0 - TRKELE (11)/GRADL
         IW ( KHCVL+JHCVUH ) = 0
C
      ELSE
C
C - ELSE (bank HCVL,NR=ITRKEL(1) exists)
C      IF current volume is not sensitive THEN
C         IF previous volume was sensitive THEN
C            reset 'HCVL' and set sensitive flag to 0
C         ELSE previous volume was not sensitive
C            update 'HCVL' , sensitive flag = 1
C         ENDIF
C      ELSE current volume is sensitive
C         IF previous volume was not sensitive THEN
C            find out where the interaction occured
C            update 'HCVL', set sensitive flag to 1
C            fill common /CAHPAR/
C            set IPAR to 1
C
         KHCVL = JHCVL+LMHLEN
C        previous volume sensitive flaf
         FPSEN = IW ( KHCVL + JHCVUH ) .GT. 0
         IF (.NOT.FCSEN) THEN
            IF (FPSEN) THEN
               RW ( KHCVL+JHCVRV ) = TRKELE (11)/GRADL
               RW ( KHCVL+JHCVAV ) = TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTL ) = TRKELE (11)
               RW ( KHCVL+JHCVTA ) = TINOL0 - TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTR ) = TINOX0 - TRKELE (11)/GRADL
               IW ( KHCVL+JHCVUH ) = 0
            ELSE
               RW ( KHCVL+JHCVRV ) = RW ( KHCVL+JHCVRV )
     +                                   + TRKELE (11)/GRADL
               RW ( KHCVL+JHCVAV ) = RW ( KHCVL+JHCVAV )
     +                                 + TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTL ) = RW( KHCVL+JHCVTL )
     +                                   + TRKELE (11)
            ENDIF
         ELSE
            IF (.NOT.FPSEN) THEN
               CALL HCSETP
               SLENG = HCPAZ0 + HCTIRF(IHCPOR)+HCTINS + 0.1
               IHPLN = ITRKEL(6)
               IW ( KHCVL+JHCVUH ) = 1
               RW ( KHCVL+JHCVRV ) = RW ( KHCVL+JHCVRV )
     +                                   + TRKELE (11)/GRADL
               RW ( KHCVL+JHCVAV ) = RW ( KHCVL+JHCVAV )
     +                                   + TRKELE (11)/GABSL
               RW ( KHCVL+JHCVTL ) = RW( KHCVL+JHCVTL )
     +                                   + TRKELE (11)
C
               DO 20 IL = 1,LETRK
                  HCTRKM ( IL,1 ) = RW (KHCVL+JHCVTE+IL-1)
                  IF(IL.LE.LNTRK)HCTRKM ( IL,2 ) = TRKNXT (IL)
  20           CONTINUE
               HTIN01 = RW (KHCVL + JHCVTA)
               HTIX01 = RW (KHCVL + JHCVTR)
               HSHGRA = RW (KHCVL+JHCVTL)/RW(KHCVL+JHCVRV)
               HSHGAB = RW (KHCVL+JHCVTL)/RW(KHCVL+JHCVAV)
               IPAR = 1
            ENDIF
         ENDIF
       ENDIF
       RETURN
       END
#endif
