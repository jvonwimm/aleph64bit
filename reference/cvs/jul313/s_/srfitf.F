      SUBROUTINE SRFITF(IER)
C----------------------------------------------------------------------
C! Perform track fit
C!
C!    Author:     H. Meinhard       28-Jan-1988
C!    Modified:   H. Meinhard       21-Feb-1989 (2)
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    For every combination of wire paralleles which corresponds
C!    to a patch (BOS bank SWPA), perform a straight line
C!    fit through the origin. If the chisquare probability is too bad,
C!    try with less wire paralleles
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "skanjj.h"
#include "skcpjj.h"
#include "srecpc.h"
#include "sskpjj.h"
#include "swpajj.h"
      REAL THRAW(MLASG),PHRAW(MLASG),SIG(MLASG),COVXY(2,2),COMAX(2,2)
C IPOI: array for combinations of KPAR numbers out of NPAR numbers
C 1...NPAR; IMAX: array for combination with greatest chisquare prob.
      PARAMETER (MPOI=MLASG+1)
      INTEGER IPOI(MPOI),IMAX(MLASG)
      EXTERNAL PROB
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C link to coordinates and wire parallele samples
      KSCOO = IW(NASCOO)
      KSWPA = IW(NASWPA)
      IF (KSCOO.EQ.0 .OR. KSWPA.EQ.0)                       GOTO 901
      NWPA = LROWS(KSWPA)
C
C create banks for track candidates, for pointers from track candidates
C to coordinates, for pointers from sides to track candidates
      CALL AUBOS('SKAN',0,LMHLEN+NWPA*LSKANA,KSKAN,IGARB)
      IF (KSKAN.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSWPA = IW(NASWPA)
        KSKAN = IW(NASKAN)
      ENDIF
      IW(KSKAN+LMHCOL) = LSKANA
C
      CALL AUBOS('SKCP',0,LMHLEN+NWPA*LSKCPA,KSKCP,IGARB)
      IF (KSKCP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSWPA = IW(NASWPA)
        KSKAN = IW(NASKAN)
        KSKCP = IW(NASKCP)
      ENDIF
      IW(KSKCP+LMHCOL) = LSKCPA
C
      CALL AUBOS('SSKP',0,LMHLEN+2*LSSKPA,KSSKP,IGARB)
      IF (KSSKP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSWPA = IW(NASWPA)
        KSKAN = IW(NASKAN)
        KSKCP = IW(NASKCP)
        KSSKP = IW(NASSKP)
      ENDIF
      IW(KSSKP+LMHCOL) = 2
      IW(KSSKP+LMHROW) = 2
C
C get sigma in theta
      SIGM = SIGMSR / (ZZERSG-ZOFFSG-FLOAT(MLAYSG)*ZDELSG/2.)
C
C top of loop over wire parallele samples
      DO 350 IWPA = 1, NWPA
        NPAR = ITABL(KSWPA,IWPA,JSWPNP)
C
C we start with the full set of wire parallels
        KPAR = NPAR
C
C top of loop over number of wire parallels in fit
  300   CONTINUE
        IF (KPAR .LT. NPARSR)                             GOTO 350
        CHMAX = 0.
        CALL VZERO(IMAX,MLAYSG)
C
C top of loop over combinations of wire parallels
        IPOI(1) = 0
  310   CONTINUE
        CALL COMBI(IPOI,NPAR,KPAR)
        IF (IPOI(1) .EQ. 0)                               GOTO 330
C
C fill input vectors for track fit subroutine
        DO 320 IPAR = 1, KPAR
          IADR = ITABL(KSWPA,IWPA,JSWPMC+IPOI(IPAR)-1)
          ICOR = IADR/2
          ITHE = MOD(IADR,2)
          THRAW(IPAR) = RTABL(KSCOO,ICOR,JSCOTT+ITHE)
          PHRAW(IPAR) = PHIXXX(ITABL(KSCOO,ICOR,JSCOLA),
     +        ITABL(KSCOO,ICOR,JSCOSC))
          SIG(IPAR) = SIGM
  320   CONTINUE
C
C perform straight line fit
        CALL SRFIT0(KPAR,THRAW,PHRAW,SIG,AX,AY,CHISQ,COVXY,IER)
C
C if track fit impossible, take next wire parallele sample. Otherwise,
C look for chisquare prob.
        IF (IER .NE. 0) THEN
          IER = 0
        ELSE
          IF (CHISQ .LE. 0.) CHISQ = 1.E-12
          CHIPR = PROB(CHISQ,KPAR-2)
          IF (CHIPR .GT. MAX(CHPRSR,CHMAX)) THEN
            AXMAX = AX
            AYMAX = AY
            CHMAX = CHIPR
            C2MAX = CHISQ
            CALL RMCPY(2,2,COVXY(1,1),COVXY(1,2),COVXY(2,1),
     +        COMAX(1,1),COMAX(1,2),COMAX(2,1))
            CALL UCOPY(IPOI,IMAX,KPAR)
          ENDIF
        ENDIF
C
C next combination
        GOTO 310
C
C come here if there is no combination left
  330   CONTINUE
C
C if chisquare prob. is not okay, try with one parallel less
        IF (IMAX(1) .EQ. 0) THEN
          KPAR = KPAR - 1
          GOTO 300
        ENDIF
C
C chisquare probability is okay. Fill candidate in bank
        IW(KNEXT(KSKAN)+JSKASI) = ITABL(KSWPA,IWPA,JSWPSI)
        IW(KNEXT(KSKAN)+JSKANP) = KPAR
        RW(KNEXT(KSKAN)+JSKAC2) = C2MAX
        RW(KNEXT(KSKAN)+JSKAAX) = AXMAX
        RW(KNEXT(KSKAN)+JSKAAY) = AYMAX
        RW(KNEXT(KSKAN)+JSKAXX) = COMAX(1,1)
        RW(KNEXT(KSKAN)+JSKAXY) = COMAX(1,2)
        RW(KNEXT(KSKAN)+JSKAYY) = COMAX(2,2)
        DO 340 IPAR = 1, KPAR
          IW(KNEXT(KSKCP)+JSKCKC+IPAR-1) =
     +      ITABL(KSWPA,IWPA,JSWPMC+IMAX(IPAR)-1)
  340   CONTINUE
        IW(KNEXT(KSKCP)+JSKCKM) = IWPA
        IW(KSKAN+LMHROW) = LROWS(KSKAN)+1
        IW(KSKCP+LMHROW) = LROWS(KSKCP)+1
C
C bottom of loop over wire parallele samples
  350 CONTINUE
C
C compress SKAN and SKCP banks to actual size
      CALL AUBPRS('SKANSKCP')
C
C fill the SSKP bank
      IW(KROW(KSSKP,1)+JSSKPB) = 1
      IW(KROW(KSSKP,2)+JSSKPE) = LROWS(KSKAN)
C look for the first track candidate from side two
      KSKAN = IW(NASKAN)
      NFTWO = LROWS(KSKAN) + 1
      DO 360 IKAN = 1, LROWS(KSKAN)
        IF (ITABL(KSKAN,IKAN,JSKASI) .EQ. 2) THEN
          NFTWO = IKAN
          GOTO 370
        ENDIF
  360 CONTINUE
  370 CONTINUE
      IW(KROW(KSSKP,1)+JSSKPE) = NFTWO - 1
      IW(KROW(KSSKP,2)+JSSKPB) = NFTWO
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRFITF',-IER,
     +  'No space for new bank SKAN, SKCP, or SSKP')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
