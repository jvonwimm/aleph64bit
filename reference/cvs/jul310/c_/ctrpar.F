      SUBROUTINE CTRPAR(ITR,RBARY,ETRL)
C***********************************************************************
C!Find the expected contribution of the track to the calobject         *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  901206                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!  MODIFICATIONS :                                                    *
C!        A. BONISSENT, P. COMAS (29-SEP-1994)                         *
C!           Avoid division by zero: no need to step in the helix if   *
C!           STEP = 0                                                  *
C***********************************************************************
#ifndef DOC
#include "cthres.h"
#include "bcs.h"
#include "hmadjj.h"
      DIMENSION X(8)
      DIMENSION ETRL(*)
      LOGICAL FIRST
      INTEGER UFITQL
      SAVE FIRST,NAHMAD
      DATA FIRST/.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAHMAD=NAMIND('HMAD')
      ENDIF
      KHMAD=IW(NAHMAD)
C
C? First, we look at the quality flag, to know if this track
C?  should be used for energy flow. Flag=2 means this track has
C?  a poor precision on the momentum; better use the calorimeters.
C?  It is enough to set its contribution to 0
C?  29 Aug. 1991 : do the same for some bad tracks which were
C?    associated anyway
C
      INEWFL=UFITQL(ITR)
      IF(INEWFL.EQ.2.OR.INEWFL.EQ.4)THEN
         DO 30 IC=1,4
   30    ETRL(IC)=0.
      ELSE
         CALL EINITK(ITR,IFLAG,X)
C
C? If there is no residual energy, the barycenter is equal to 0
C?  Then, our best estimate is the ECAL entry point
C
         IF(RBARY.GT.1.)THEN
            RPT=SQRT(X(1)**2+X(2)**2+X(3)**2)
            STEP=RBARY-RPT
C? If STEP = 0 no need to step in the helix (A. Bonissant,P. Comas)
            IF (STEP.NE.0) THEN
              CALL EINHLX(STEP,1,X)
              CALL EHLIX(X)
            ENDIF
         ENDIF
         XMOD=SQRT(X(1)**2+X(2)**2+X(3)**2)
         EESTI=X(7)
C
C   Special treatment for muons : estimate deposited energy to just 3.5 Gev
C
         NHMAD=0
         IF(KHMAD.NE.0)THEN
            NHMAD=LROWS(KHMAD)
         ENDIF
         DO 20 IHMAD=1,NHMAD
         ITREF=ITABL(KHMAD,IHMAD,JHMATN)
         IF(ITREF.EQ.ITR)THEN
            IFMU=ITABL(KHMAD,IHMAD,JHMAIF)
            IF(IFMU.EQ.1)EESTI=EDMUCT
            GO TO 21
         ENDIF
   20    CONTINUE
   21    CONTINUE
         DO 10 I=1,3
   10    ETRL(I)=EESTI*X(I)/XMOD
         ETRL(4)=EESTI
      ENDIF
      RETURN
      END
#endif
