      SUBROUTINE SAHIST
C----------------------------------------------------------------------
C! fill SATR histograms
C!
C!    Author:     H. Meinhard       08-Oct-1987
C!    Modified:   H. Meinhard       08-Oct-1989  (3)
C!
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rparac.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "sparam.h"
#include "srtdjj.h"
#include "sscpjj.h"
#include "sstpjj.h"
#include "stcpjj.h"
#include "strkjj.h"
      EXTERNAL PROB
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
      LHIST = 1000 * JULSA
C
C link to raw data
      KSRTD = IW(NASRTD)
      IF (KSRTD .NE. 0) THEN
        CALL HFILL(LHIST+MRDSP,FLOAT(LROWS(KSRTD))+.5,0.,1.)
      ELSE
        CALL HFILL(LHIST+MRDSP,.5,0.,1.)
      ENDIF
C
C link to coordinates
      KSCOO = IW(NASCOO)
      KSSCP = IW(NASSCP)
      IF (KSSCP .NE. 0) THEN
        NCORA = ITABL(KSSCP,MLAYSG,JSSCPE)-ITABL(KSSCP,1,JSSCPB)+1
        NCORB = ITABL(KSSCP,2*MLAYSG,JSSCPE)-
     +              ITABL(KSSCP,MLAYSG+1,JSSCPB)+1
        CALL HFILL(LHIST+MCASP,FLOAT(NCORA)+.5,0.,1.)
        CALL HFILL(LHIST+MCBSP,FLOAT(NCORB)+.5,0.,1.)
        IF (KSCOO .NE. 0) THEN
          DO 295 ICOR = 1, LROWS(KSCOO)
            ILAYE = ITABL(KSCOO,ICOR,JSCOLA)
            ZLAYE = ZZERSG - ZOFFSG - REAL(MLAYSG-ILAYE)*ZDELSG
            RDIFF = ZLAYE * (RTABL(KSCOO,ICOR,JSCOTT+1) -
     +        RTABL(KSCOO,ICOR,JSCOTT)) / 2.
            CALL HFILL(LHIST+MM1SP,RDIFF,0.,1.)
  295     CONTINUE
        ENDIF
      ELSE
        CALL HFILL(LHIST+MCASP,.5,0.,1.)
        CALL HFILL(LHIST+MCBSP,.5,0.,1.)
      ENDIF
C
C link to tracks
      KSTRK = IW(NASTRK)
      KSSTP = IW(NASSTP)
      KSTCP = IW(NASTCP)
      IF (KSSTP .NE. 0) THEN
        NTRKA = ITABL(KSSTP,1,JSSTTE)-ITABL(KSSTP,1,JSSTPB)+1
        NTRKB = ITABL(KSSTP,2,JSSTTE)-ITABL(KSSTP,2,JSSTPB)+1
        NUPAA = ITABL(KSSTP,1,JSSTPE)-ITABL(KSSTP,1,JSSTPS)+1
        NUPAB = ITABL(KSSTP,2,JSSTPE)-ITABL(KSSTP,2,JSSTPS)+1
        CALL HFILL(LHIST+MTASP,FLOAT(NTRKA)+.5,0.,1.)
        CALL HFILL(LHIST+MTBSP,FLOAT(NTRKB)+.5,0.,1.)
        CALL HFILL(LHIST+MPCSP,FLOAT(NUPAA)+.5,0.,1.)
        CALL HFILL(LHIST+MPDSP,FLOAT(NUPAB)+.5,0.,1.)
C
        IF (KSTRK .NE. 0) THEN
          DO 330 ISIDE = 1, 2
            IMAX = 0
            CHMAX = 0.
            DO 310 ITR = ITABL(KSSTP,ISIDE,JSSTPB),
     +                   ITABL(KSSTP,ISIDE,JSSTPE)
              THETA = RTABL(KSTRK,ITR,JSTRTH)
              PHI   = RTABL(KSTRK,ITR,JSTRPH)
              CALL SRUNCO(THETA,PHI,THUNC,PHUNC)
              IQF   = ITABL(KSTRK,ITR,JSTRQF)
              AX    = TAN(THETA)*COS(PHI)
              AY    = TAN(THETA)*SIN(PHI)
              AXUNC = TAN(THUNC)*COS(PHUNC)
              AYUNC = TAN(THUNC)*SIN(PHUNC)
              IF (IQF .EQ. 1) THEN
                IF (ISIDE .EQ. 1) THEN
                  CALL HFILL(LHIST+MDASP,THETA,0.,1.)
                  CALL HFILL(LHIST+MPASP,PHI,0.,1.)
                  CALL HFILL(LHIST+MSASP,AX,AY,1.)
                ELSE
                  CALL HFILL(LHIST+MDBSP,PI-THETA,0.,1.)
                  CALL HFILL(LHIST+MPBSP,MOD(PI+PHI,TWOPI),0.,1.)
                  CALL HFILL(LHIST+MSBSP,AX,AY,1.)
                ENDIF
C track related to coordinates?
                IF (KSTCP .NE. 0) THEN
                  IF (ITABL(KSTCP,ITR,JSTCTC) .NE. 0) THEN
                    NPAR = ITABL(KSTRK,ITR,JSTRNP)
                    CHI2 = RTABL(KSTRK,ITR,JSTRC2)
                    CHPR = PROB(CHI2,NPAR-2)
                    IF (CHPR .GT. CHMAX) THEN
                      CHMAX = CHPR
                      IMAX  = ITR
                    ENDIF
                    CALL HFILL(LHIST+MN1SP,REAL(NPAR)+.5,0.,1.)
                    CALL HFILL(LHIST+MC1SP,CHPR,0.,1.)
C loop over coordinates
                    DO 300 IPAR = 1, NPAR
                      IADR = ITABL(KSTCP,ITR,JSTCTC+IPAR-1)
                      ICO = IADR/2
                      ITH = MOD(IADR,2)
                      ILAYE = ITABL(KSCOO,ICO,JSCOLA)
                      ISECT = ITABL(KSCOO,ICO,JSCOSC)
                      TTHET = RTABL(KSCOO,ICO,JSCOTT+ITH)
                      CALL HFILL(LHIST+MS1SP,REAL(ILAYE)+.5,0.,1.)
C calculate residual
                      Z = ZZERSG - ZOFFSG - REAL(MLAYSG-ILAYE)*ZDELSG
                      IF (ISIDE .EQ. 2) Z = -Z
                      PHSEC = PHIXXX(ILAYE,ISECT)
                      RFIT = (AXUNC*COS(PHSEC) + AYUNC*SIN(PHSEC)) * Z
                      RMES = ABS(Z*TTHET)
                      RWIR = Z * (RTABL(KSCOO,ICO,JSCOTT) +
     +                  RTABL(KSCOO,ICO,JSCOTT+1)) / 2.
                      CALL HFILL(LHIST+MR1SP,RFIT-RMES,0.,1.)
                      CALL HFILL(LHIST+MK1SP,RMES-RWIR,0.,1.)
                      CALL HFILL(LHIST+MF1SP,RFIT-RWIR,0.,1.)
  300               CONTINUE
                  ENDIF
                ENDIF
              ELSE IF (IQF .EQ. 0) THEN
                IF (ISIDE .EQ. 1) THEN
                  CALL HFILL(LHIST+MTCSP,THETA,0.,1.)
                  CALL HFILL(LHIST+MBCSP,PHI,0.,1.)
                ELSE
                  CALL HFILL(LHIST+MTDSP,PI-THETA,0.,1.)
                  CALL HFILL(LHIST+MBDSP,MOD(PI+PHI,TWOPI),0.,1.)
                ENDIF
              ENDIF
C bottom of loop over tracks on side
  310       CONTINUE
C fill histograms for best track, if any
            IF (IMAX .NE. 0) THEN
              THETA = RTABL(KSTRK,IMAX,JSTRTH)
              PHI   = RTABL(KSTRK,IMAX,JSTRPH)
              CALL SRUNCO(THETA,PHI,THUNC,PHUNC)
              AX    = TAN(THETA)*COS(PHI)
              AY    = TAN(THETA)*SIN(PHI)
              AXUNC = TAN(THUNC)*COS(PHUNC)
              AYUNC = TAN(THUNC)*SIN(PHUNC)
              NPAR = ITABL(KSTRK,IMAX,JSTRNP)
              CALL HFILL(LHIST+MN2SP,REAL(NPAR)+.5,0.,1.)
              CALL HFILL(LHIST+MC2SP,CHMAX,0.,1.)
C loop over coordinates
              DO 320 IPAR = 1, NPAR
                IADR = ITABL(KSTCP,IMAX,JSTCTC+IPAR-1)
                ICO = IADR/2
                ITH = MOD(IADR,2)
                ILAYE = ITABL(KSCOO,ICO,JSCOLA)
                ISECT = ITABL(KSCOO,ICO,JSCOSC)
                TTHET = RTABL(KSCOO,ICO,JSCOTT+ITH)
                CALL HFILL(LHIST+MS2SP,REAL(ILAYE)+.5,0.,1.)
C calculate residual
                Z = ZZERSG - ZOFFSG - REAL(MLAYSG-ILAYE)*ZDELSG
                IF (ISIDE .EQ. 2) Z = -Z
                PHSEC = PHIXXX(ILAYE,ISECT)
                RFIT = (AXUNC*COS(PHSEC) + AYUNC*SIN(PHSEC)) * Z
                RMES = ABS(Z*TTHET)
                RWIR = Z * (RTABL(KSCOO,ICO,JSCOTT) +
     +            RTABL(KSCOO,ICO,JSCOTT+1)) / 2.
                CALL HFILL(LHIST+MR2SP,RFIT-RMES,0.,1.)
                CALL HFILL(LHIST+MK2SP,RMES-RWIR,0.,1.)
                CALL HFILL(LHIST+MF2SP,RFIT-RWIR,0.,1.)
  320         CONTINUE
            ENDIF
C bottom of loop over sides
  330     CONTINUE
        ENDIF
      ELSE
        CALL HFILL(LHIST+MTASP,.5,0.,1.)
        CALL HFILL(LHIST+MTBSP,.5,0.,1.)
        CALL HFILL(LHIST+MPCSP,.5,0.,1.)
        CALL HFILL(LHIST+MPDSP,.5,0.,1.)
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
