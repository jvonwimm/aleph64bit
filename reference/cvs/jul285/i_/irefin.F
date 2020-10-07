      SUBROUTINE IREFIN
C-----------------------------------------------------------------------
C! Refine/improve ITC tracks.
C!
C!   Author     :- W.Atwood, J.Sedgbeer 15/01/90
C!   Modified   :- J.Sedgbeer  25/04/90 Check Phi in range 0-2pi in ICCO
C!                  + Bug fix: explicitly test track number in loop 210
C!   Modified   :- J.Sedgbeer 09/05/90 Protect against divide checks.
C!   Modified   :- J.Carr     13/2/91 Various changes
C!   Modified   :- J.Sedgbeer 21/10/91 Bug fix. Set Xhit,Yhit even for
C!                                     very small DOCAs.
C!   Modified   :-  J.Sedgbeer 24/10/91 Use function IRESRP to set r-phi
C!                                      resolution in ICCO
C!   Input:
C!     need commons /BCS/    for BOS banks
C!                  /ITCINF/ coord. reassignment info.
C!                  /IASIGC/ cuts for reassignment
C!          params: ICCOJJ
C!                  ICTRJJ
C!                  ITCOJJ
C!                  IGTLJJ
C!                  ALCONS
C!
C!   Output: updataed coord. lists, IGTL and IGCL, and ICCO bank.
C!
C!   called by : ITCREC
C!   calls     : IASIGN - fill /ITCINF/
C!               ISHUFL - shuffle coord. lists
C!
C!   Libraries required: BOS
C!
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C----------------------------------------------------------------------
#include "bcs.h"
#include "bosext.h"
#include "iccojj.h"
#include "ictrjj.h"
#include "itcojj.h"
#include "igtljj.h"
#include "alcons.h"
C----------------------------------------------------------------------
#include "iasigc.h"
#include "itcinf.h"
      EXTERNAL IRESRP,AUARCM
      REAL IRESRP,AUARCM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
#include "bmacro.h"
C----------------------------------------------------------------------
C Initialise.
      IF (FIRST) THEN
        NICCO = NAMIND('ICCO')
        NICTR = NAMIND('ICTR')
        FIRST = .FALSE.
      ENDIF
C
C Locate banks
C
      KITCO = NLINK('ITCO',0)
      IF (KITCO.LE.0) GOTO 998
      NCO = LROWS(KITCO)
      IF(NCO.LE.0) GOTO 998
      KITF1 = NLINK('ITFT',1)
      IF (KITF1.LE.0) GOTO 998
      NTRK = LROWS(KITF1)
      IF(NTRK.LE.0) GOTO 998
      KIGT1 = NLINK('IGTL',1)
      IF (KIGT1.LE.0) GOTO 998
      KIGC1 = NLINK('IGCL',1)
      IF (KIGC1.LE.0) GOTO 998
C
      KITF2 = NLINK('ITFT',2)
      KIGT2 = NLINK('IGTL',2)
      KIGC2 = NLINK('IGCL',2)
      IF(KITF2.GT.0) THEN
        IF(KIGT2.LE.0.OR.KIGC2.LE.0) GOTO 998
      ENDIF
C
C Create latched wire bank (wire-to-coord. reln. IWCR) - used in IASIGN.
C                                 Add to T list.
      CALL ILATCH('T+',IER)
      IF(IER.GT.0) GOTO 998
C
C Loop over selected tracks; fill /ITCINF/ by call to IASIGN
C
      NTKITC = 0
      DO 50 I=1,NTRK
        KG = KROW(KIGT1,I)
        NC1 = IW(KG+JIGTN1)
        KK = KROW(KITF1,I)
        CALL IASIGN(SMAXIA,ADJCIA,I,RW(KK+1),IER)
   50 CONTINUE
C
C----------------------------------------------------------------------
C Create ICTR bank for coord.-to-track relation and associated
C quantities. (3 columns: Track No.(JULIA); DEV; Coord.ambiguity sign)
C
      LICTR  = NCO*LICTRA + LMHLEN
      CALL AUBOS('ICTR',0,LICTR,KICTR,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IW(KICTR+LMHROW) = NCO
      IW(KICTR+LMHCOL) = LICTRA
      CALL BLIST(IW,'T+','ICTR')
C
C     Apply cuts to points on tracks in /ITCINF/ commons
      CALL ITCCUT
C
C Fill ICTR bank from ICOITC(..) etc. Loop over tracks in
C /ITCINF/, loop over layers. For each coord. find which track it is
C assigned to. If more than 1, then choose closest one. Only allow
C coords. which have FLGITC(..) = 0.
C
      DO 110 IT=1,NTKITC
        IF(NGHITC(IT).EQ.0) GO TO 110
        IJT = ITKITC(IT)
        DO 100 IL=1,8
          ICO = ICOITC(IL,IT)
          IF(ICO.EQ.0) GOTO 100
          IF(FLGITC(IL,IT).GT.0) GOTO 100
          KK = KROW(KICTR,ICO)
          IF(IW(KK+JICTTN).NE.0) THEN
            DEVO = RW(KK+JICTDV)
            DEVN = ABS(RSFITC(IL,IT)/DERITC(IL,IT))
            IF(DEVO.LT.DEVN) THEN
                FLGITC(IL,IT)=14
                GOTO 100
            ENDIF
          ENDIF
          IW(KK+JICTTN) = IJT
          RW(KK+JICTDV) = ABS(RSFITC(IL,IT)/DERITC(IL,IT))
          IW(KK+JICTAM) = SIGN(1.,DDSITC(IL,IT))
  100   CONTINUE
  110 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        WRITE(LDEBRL,1000)
 1000   FORMAT(' IREFIN: Dump ICTR bank',/5X,'Coord.',3X,'Track',
     +          6X,'Dev.',' Ambiguity')
        DO 120 I=1,NCO
          KK = KROW(KICTR,I)
          WRITE(LDEBRL,1001) I,IW(KK+1),RW(KK+2),IW(KK+3)
 1001     FORMAT(1X,2I10,F10.4,I10)
  120   CONTINUE
      ENDIF
C
C----------------------------------------------------------------------
C Shuffle coord. lists (to allow for reassigned coords.)
C
      CALL ISHUFL(NTKITC,ITKITC(1),NCMNIA,NSMNIA,IER)
      IF(IER.GT.0) GOTO 998
C
C----------------------------------------------------------------------
C Locate/Create ICCO bank
      KICCO = IW(NICCO)
      IF(KICCO.GT.0) IDUM=NDROP('ICCO',0)
      LICCO  = NCO*LICCOA + LMHLEN
      CALL AUBOS('ICCO',0,LICCO,KICCO,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IW(KICCO+LMHROW) = NCO
      IW(KICCO+LMHCOL) = LICCOA
      CALL BLIST(IW,'E+','ICCO')
      KICTR = IW(NICTR)
      KITCO = NLINK('ITCO',0)
C
C Fill ICCO bank: use /ITCINF/ info and ICTR bank.
C
      DO 210 ITK=1,NTKITC
        JTK = ITKITC(ITK)
        DO 200 IL=1,8
          ICO = ICOITC(IL,ITK)
          IF(ICO.EQ.0) GOTO 200
          KC = KROW(KICTR,ICO)
          IJT = IW(KC+JICTTN)
          IF(IJT.EQ.JTK) THEN
            KK = KROW(KICCO,ICO)
            KI = KROW(KITCO,ICO)
C
C  Compute the x,y Point of the measurement...
C  Protect against very small DOCAs
             IF(ABS(DCAITC(IL,ITK)).LT.1.E-5) THEN
               DXS = -DDSITC(IL,ITK)*SIN(FIWITC(IL,ITK))
               DYX =  DDSITC(IL,ITK)*COS(FIWITC(IL,ITK))
             ELSE
               DRAT = ABS(DDSITC(IL,ITK)/DCAITC(IL,ITK))
               DXVEC = HTXITC(1,IL,ITK) - WRXITC(1,IL,ITK)
               DYVEC = HTXITC(2,IL,ITK) - WRXITC(2,IL,ITK)
               DXS   = DXVEC*DRAT
               DYS   = DYVEC*DRAT
             ENDIF
             XHIT  = WRXITC(1,IL,ITK)+ DXS
             YHIT  = WRXITC(2,IL,ITK)+ DYS
C
C  Store the results away in the Bank ICCO
             RW(KK+JICCRV) = SQRT(XHIT**2 + YHIT**2)
             PHI           = ATAN2(YHIT,XHIT)
             RW(KK+JICCPH) = AMOD(PHI+TWOPI,TWOPI)
             PHEN = ATAN2(HTDITC(2,IL,ITK),HTDITC(1,IL,ITK))
             PHTR = ATAN2(HTXITC(2,IL,ITK),HTXITC(1,IL,ITK))
             CALP = COS( AUARCM(PHEN - PHTR) )
             RW(KK+JICCSR) = ( IRESRP(IL,DDSITC(IL,ITK),CALP) )**2
CC Z of track = RW(KK+JICCZV) = HTXITC(3,IL,ITK)
             RW(KK+JICCZV) = RW(KI+JITCZH)
             RW(KK+JICCSZ) = RW(KI+JITCSZ)
          ENDIF
  200   CONTINUE
  210 CONTINUE
C
      IF(FDEBRF.AND.JDBDRF(JULIT).GE.4) THEN
        WRITE(LDEBRL,2000)
 2000   FORMAT(' IREFIN: ICCO bank')
        DO 220 I=1,NCO
          KK = KROW(KICCO,I)
          WRITE(LDEBRL,2001) I,(RW(KK+J),J=1,5)
 2001     FORMAT(1X,I6,3F8.3,2E12.4)
  220   CONTINUE
      ENDIF
C----------------------------------------------------------------------
C Tidy up
  998 KICTR = NLINK('ICTR',0)
      IF (KICTR.GT.0) IDUM = NDROP('ICTR',0)
      KIWCR = NLINK('IWCR',0)
      IF (KIWCR.GT.0) IDUM = NDROP('IWCR',0)
  999 CONTINUE
      END
#endif
