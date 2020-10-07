      SUBROUTINE ITCOPR(NUM)
C-----------------------------------------------------------------------
C! Print ITCO bank number NUM.
C!
C!    Author     :- J. Sedgbeer   87/03/02
C!    Modified   :- J. Sedgbeer   88/05/05
C!    Modified   :- I. Tomalin    88/11/17
C!
C!    Input:
C!      NUM /I4 : Number of ITCO bank to be printed
C!      ITCO bank of ITC coordinates
C!      need commons     /BCS/  BOS
C!                       /RCURNT/
C!           parameters  ITCOJJ   parameters for ITCO bank
C!
C!    Output:
C!
C!    calls:    none
C!
C!    Libraries required: BOS
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
C-----------------------------------------------------------------------
C I/O commons and parameters
#include "rcurnt.h"
#include "bcs.h"
#include "bosext.h"
#include "itcojj.h"
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      KITCO=NLINK('ITCO',NUM)
      IF (KITCO.EQ.0) THEN
        IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) CALL RERROR('ITCOPR',1,
     +  'Not printing ITCO bank as it does not exist.')
        GOTO 999
      ENDIF
C
      NROW=LROWS(KITCO)
      WRITE(LDEBRL,1000) IRUNRC,IEVTRC
 1000 FORMAT(1H0,'ITCOPR: Dump of ITCO bank for Run/Event',2I8,
     + /2X,'Coord',1X,'Layer/Wire',2X,'Radius',4X,'Phi1',4X,'Phi2',
     +  7X,'Z',2X,'Sig(RP)**2',2X,'Sig(Z)**2',2X,'Time')
C
      JITCO = KITCO + LMHLEN
      DO 100 I=1,NROW
         K1 = JITCO + JITCWN
         K2 = JITCO + JITCRA
         K8 = JITCO + JITCDT
         WRITE(LDEBRL,1001) I,IW(K1),(RW(J),J=K2,K8)
         JITCO = JITCO + LITCOA
  100 CONTINUE
 1001 FORMAT(1X,I6,I11,F8.2,2F8.4,F8.1,F12.5,F11.1,F6.1)
C
  999 CONTINUE
      END
#endif
