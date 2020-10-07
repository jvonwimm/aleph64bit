*DK lcnois
      SUBROUTINE LCNOIS
C--------------------------------------------------------------
C! Make noise in LCal
C. - J.Dines Hansen & P.Hansen - 860417
C.   Modified P.Hansen - 890429
C. - Coherent and incoherent noise in towers and wire-planes
C.   Adds coherent and incoherent noise in towers and wires
C.   Adds also extra smearing to DEtot/Etot from sampling fluct
C.   Note that no noise is added to wireplanes in modules
C.   that contains no energy to start with.
C. - Modifies banks LTDI, LWDI and LTTR
C. - Called by  LCDIGI                           from this .HLB
C. - Calls      LCPAD                            from this .HLB
C. -            LCAMP                            from this .HLB
C. -            RANNOR                           from   CERNLIB
C -----------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "lcnamc.h"
#include "lccomc.h"
      DIMENSION NOISE(3)
      DIMENSION RANOI(4),CONOI(4)
      EXTERNAL LCAMP
#include "bmacro.h"
C -------------------------------------------------------------
C
C Generate energy-dependent and coherent noise
C independently in each module
      DO 100 I=1,4
        CALL RANNOR(RANOI(I),CONOI(I))
  100 CONTINUE
C
C - Modify 'LWDI' bank
      KLWDI = IW(NALWDI)
      IF(KLWDI .LE. 0)                    GOTO 999
C
C - Loop over modules containing energy
      DO 130 ILWD = 1,LROWS(KLWDI)
         KADR = KROW(KLWDI,ILWD)
         LAY = 0
         DO 120 ISTOR = 1,3
           LAYMX = LCNLAY(ISTOR)
           DO 110 L = 1,LAYMX
             LAY = LAY + 1
             CALL RANNOR(RAN,DUM)
             MODU = IW(KADR+1)
C Coherent noise
             NOICH  = NINT(ADCOLC*COHNLC*CONOI(MODU)/16.)
C Energy dependent noise
             ENE=FLOAT(IW(KADR+1+LAY))
             IF(ENE.GT.100.) THEN
               NOIED = NINT(ENE*
     +                 (SCONLC+SSAMLC/SQRT(ENE*0.001))*
     +                  RANOI(MODU))
             ENDIF
C Total noise
             MVDIG = NINT(ADCOLC*WNOILC(ISTOR)*RAN)+NOICH+NOIED
             IW(KADR+1+LAY) = IW(KADR+1+LAY)+MVDIG
  110      CONTINUE
  120    CONTINUE
  130 CONTINUE
C
C - Coherent noise for tower storeys
      DO 140 I=1,4
         CALL RANNOR (CONOI(I),DUM)
  140 CONTINUE
C
C - Loop only over hit pads to save time
C ( the zero suppression will remove other signals )
      KLTDI = IW(NALTDI)
      IF(KLTDI.GT.0) THEN
        DO 210 IT=1,LROWS(KLTDI)
          IPAD=ITABL(KLTDI,IT,1)
C         Loop over storeys
          DO 200 ISTOR = 1,3
            ENE = FLOAT(ITABL(KLTDI,IT,1+ISTOR))
            CALL RANNOR(RAN,DUM)
C Coherent noise
            NOICH  = NINT(ADCOLC*COHNLC*CONOI(MODU))
C Energy dependent noise
            IF(ENE.GT.100.) THEN
              NOIED = NINT(ENE*
     +                (SCONLC+SSAMLC/SQRT(ENE*0.001))*RANOI(MODU))
            ENDIF
C Total noise
            NOISE(ISTOR) = NINT(ADCOLC*SNOILC(ISTOR)*RAN)+NOICH+NOIED
            IF(NOISE(ISTOR).NE.0)
     &        CALL LCPAD(NALTDI,ISTOR,IPAD,NOISE(ISTOR))
  200     CONTINUE
  210   CONTINUE
      ENDIF
C
C - Modify LTTR
      KLTTR = IW(NALTTR)
      IF(KLTTR .LE. 0)                   GOTO 999
C
        DO 310 ISEG = 1,LROWS(KLTTR)
C
C Module number
        IF(ISEG.GE.16.AND.ISEG.LE.21) MODU = 1
        IF(ISEG.GE.22.OR.(ISEG.GE.13.AND.ISEG.LE.15)) MODU = 2
        IF(ISEG.GE.4.AND.ISEG.LE.9) MODU = 3
        IF(ISEG.LE.3.OR.(ISEG.GE.10.AND.ISEG.LE.12)) MODU = 4
C
        KADR = KROW(KLTTR,ISEG)
C         Loop over storeys
          DO 320 ISTOR = 1,3
            CALL RANNOR(RAN,DUM)
C Coherent noise
            NOICH  = NINT(ADCOLC*COHNLC*CONOI(MODU))
C Total noise
            MVDIG   = NINT(ADCOLC*TNOILC(ISTOR)*RAN)+NOICH
            IW(KADR+ISTOR) = MAX0(0,IW(KADR+ISTOR)+MVDIG)
  320     CONTINUE
  310   CONTINUE
  300 CONTINUE
C
  999 RETURN
      END
#endif
