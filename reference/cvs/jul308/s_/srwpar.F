      SUBROUTINE SRWPAR(IER)
C----------------------------------------------------------------------
C! Find wire paralleles corresponding to patches
C!
C!    Author:     H. Meinhard       27-Jan-1988
C!    Modified:   H. Meinhard       27-Feb-1990  (2)
C!    Modified:   H. Meinhard       06-Feb-1990  (2)
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    Look for correspondings between the patches (found only on
C!    fired tubes, i.e. without drift time information) and the wire
C!    paralleles which are given by the drift distance in each fired
C!    tube.
C!    - loop over all patches
C!      - loop over hits of patch
C!        - loop over wire paralleles
C!          - does the wire parallele correspond to the patch?
C!          - if yes, wire parallele becomes part of the sample
C!    - bottom of all loops
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "srecpc.h"
#include "swpajj.h"
C MWPA is the maximum number of wire parallele samples
      PARAMETER (MWPA=400)
C The patches have a maximum of ten corners
      PARAMETER (MXCOR=10)
      REAL AX(MXCOR),AY(MXCOR),AXBAK(MXCOR),AYBAK(MXCOR)
C IBUFFi is a buffer containing 0 (no parallele matches),
C 1 (first ...), 2 (second ...), 3 (both ...)
      INTEGER IBUFF(MLASG)
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C look if SWPA bank exists; if yes, drop it
      IF (IW(NASWPA) .NE. 0) CALL BDROP(IW,'SWPA')
C
C link to coordinates and patches
      KSCOO = IW(NASCOO)
      KSPAT = IW(NASPAT)
      KSPCP = IW(NASPCP)
      IF (KSCOO.EQ.0 .OR. KSPAT.EQ.0 .OR. KSPCP.EQ.0)       GOTO 901
C
C create bank for wire parallele samples
      CALL AUBOS('SWPA',0,LMHLEN+MWPA*LSWPAA,KSWPA,IGARB)
      IF (KSWPA.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSPAT = IW(NASPAT)
        KSPCP = IW(NASPCP)
        KSWPA = IW(NASWPA)
      ENDIF
      IW(KSWPA+LMHCOL) = LSWPAA
C
C get the half width of zones around the wire paralleles
      DTBY2 = DTHCSR/2.
C
C top of loop over patches
      DO 350 IPAT = 1, LROWS(KSPAT)
        DO 300 IB = 1, MLASG
  300   IBUFF(IB) = 0
        IBSUM = 0
        IBPRO = 1
C
C get the patch coordinates into xxBAK
        NCBAK = ITABL(KSPAT,IPAT,JSPANC)
        DO 305 IC = 1, NCBAK
          AXBAK(IC) = RTABL(KSPAT,IPAT,JSPAAX+IC-1)
          AYBAK(IC) = RTABL(KSPAT,IPAT,JSPAAY+IC-1)
  305   CONTINUE
C
C top of loop over hits of patch
        NHIT = ITABL(KSPAT,IPAT,JSPANW)
        DO 320 IHIT = 1, NHIT
C
C get coordinate number, elements of coordinate
          ICOOR = ITABL(KSPCP,IPAT,IHIT)
          ILAY  = ITABL(KSCOO,ICOOR,JSCOLA)
          ISEC  = ITABL(KSCOO,ICOOR,JSCOSC)
          PHI   = PHIXXX(ILAY,ISEC)
C
C top of loop over wire paralleles
          DO 310 ITH = 1, 2
            TT = RTABL(KSCOO,ICOOR,JSCOTT+ITH-1)
C
C copy the patch to NC,AX,AY
            CALL SRCCOP(NCBAK,AXBAK,AYBAK,NC,AX,AY)
C
C clip patch at wire parallele
            CALL SRCLIP(NC,AX,AY,TT-DTBY2,PHI,2)
            IF (NC .EQ. 0)                                  GOTO 310
            CALL SRCLIP(NC,AX,AY,TT+DTBY2,PHI,1)
            IF (NC .EQ. 0)                                  GOTO 310
C
C wire parallele matches
            IBUFF(IHIT) = IBUFF(IHIT)+ITH
C
C bottom of loop over wire paralleles
  310     CONTINUE
C
C bottom of loop over hits of patch
          IF (IBUFF(IHIT) .NE. 0) IBSUM = IBSUM+1
          IF (IBUFF(IHIT) .EQ. 3) IBPRO = 2*IBPRO
  320   CONTINUE
C
C are there matching wire paralleles? If yes, fill BOS bank
        IF (IBSUM .NE. 0) THEN
          DO 340 IWPA = 1, IBPRO
            IF (LROWS(KSWPA)+1 .GT. MWPA)                   GOTO 903
            IW(KNEXT(KSWPA)+JSWPSI) = ITABL(KSPAT,IPAT,JSPASI)
            IW(KNEXT(KSWPA)+JSWPNP) = IBSUM
            IW(KNEXT(KSWPA)+JSWPMP) = IPAT
            NHT = 0
            NJUMP = 0
            DO 330 IHIT = 1, NHIT
              IF (IBUFF(IHIT) .EQ. 0)                       GOTO 330
              NHT = NHT+1
              IF (IBUFF(IHIT).EQ.1 .OR. IBUFF(IHIT).EQ.2) THEN
                IW(KNEXT(KSWPA)+JSWPMC+NHT-1) =
     +            ITABL(KSPCP,IPAT,IHIT)*2 + IBUFF(IHIT)-1
              ELSE IF (IBUFF(IHIT) .EQ. 3) THEN
                IW(KNEXT(KSWPA)+JSWPMC+NHT-1) =
     +            ITABL(KSPCP,IPAT,IHIT)*2 + IBITS(IWPA-1,NJUMP,1)
                NJUMP = NJUMP+1
              ENDIF
  330       CONTINUE
            IF (NHT .NE. IBSUM)                             GOTO 904
            IW(KSWPA+LMHROW) = LROWS(KSWPA)+1
  340     CONTINUE
        ENDIF
C
C bottom of loop over patches
  350 CONTINUE
C
C compress SWPA bank to actual size
      CALL AUBPRS('SWPA')
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRWPAR',-IER,'No space for new bank SWPA')
      GOTO 999
  903 IER = 3
      CALL RERROR('SRWPAR',IER,
     +  'Maximum number of sets of wire parallels exdeeded')
      GOTO 999
  904 IER = 4
      CALL RERROR('SRWPAR',-IER,'Wrong number of hits detected')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
