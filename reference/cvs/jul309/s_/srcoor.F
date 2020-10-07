      SUBROUTINE SRCOOR(IER)
C----------------------------------------------------------------------
C! Calculate SATR coordinates
C!
C!    Author:     H. Meinhard       09-Mar-1987
C!    Modified:   H. Meinhard       02-May-1991  (5)
C!       Drop informations from hot channels as given by SHOT
C!    Modified:   H. Meinhard       31-May-1991  (6)
C!       Drop TDC signals with TDC overflow
C!
C!    Output:     - IER       /I    error flag, .ne. 0 in case of error
C!
C!    Description
C!    ===========
C!    Converts raw data and raw time information to coordinates.
C!    Raw data and raw time information is read from 'SRTD'
C?    encode electronics channel from raw data address
C?    convert electronics channel to wire number
C?    convert raw time to drift time and drift time to drift distance
C?    store coordinates in 'SCOO'
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scoojj.h"
#include "scalbc.h"
#include "scrpjj.h"
#include "sgeomc.h"
#include "shotjj.h"
#include "srtdjj.h"
#include "sscpjj.h"
C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
#include "sflagc.h"
C>>> END HM890307
      INTEGER NDAT(MLASG)
#include "bosext.h"
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C look whether bank SCOO exists; if yes, return
      IF (IW(NASCOO).NE.0)                                  GOTO 999
C
C link to raw data and raw time information
      KSRTD = IW(NASRTD)
      IF (KSRTD.EQ.0)                                       GOTO 901
      NDATA = LROWS(KSRTD)
C
C create banks for coordinates, for pointers from coordinates
C to raw data and for pointers from layers to coordinates
      CALL AUBOS('SCOO',0,LMHLEN+4*NDATA*LSCOOA,KSCOO,IGARB)
      IF (KSCOO.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSRTD = IW(NASRTD)
        KSCOO = IW(NASCOO)
      ENDIF
      IW(KSCOO+LMHCOL) = LSCOOA
C
      CALL AUBOS('SCRP',0,LMHLEN+4*NDATA*LSCRPA,KSCRP,IGARB)
      IF (KSCRP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSRTD = IW(NASRTD)
        KSCOO = IW(NASCOO)
        KSCRP = IW(NASCRP)
      ENDIF
      IW(KSCRP+LMHCOL) = LSCRPA
C
      CALL AUBOS('SSCP',0,LMHLEN+2*MLAYSG*LSSCPA,KSSCP,IGARB)
      IF (KSSCP.EQ.0 .OR. IGARB.EQ.2)                      GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSRTD = IW(NASRTD)
        KSCOO = IW(NASCOO)
        KSCRP = IW(NASCRP)
        KSSCP = IW(NASSCP)
      ENDIF
      IW(KSSCP+LMHCOL) = LSSCPA
      IW(KSSCP+LMHROW) = 2*MLAYSG
C
C top of loop over sides
      DO 330 IS = 1, 2
        CALL VZERO(NDAT,MLAYSG)
        IW(KROW(KSSCP,(IS-1)*MLAYSG+1)+JSSCPB) = LROWS(KSCOO)+1
C
C top of loop over data of one event
        DO 310 IDAT = 1, NDATA
C get raw data word
          JT = ITABL(KSRTD,IDAT,JSRTIR)
          TIME = REAL(ITABL(KSRTD,IDAT,JSRTTR))/1.E12
C skip hit if TDC overflow
          ITDCC = JTDCC(JT)
          IF (IBITS(ITDCC,10,2) .NE. 0)                     GOTO 310
          IF (ITDCC .GT. 1022)                              GOTO 310
C calculate side, layer, sector, wire, distance track - wire
          JCRA = JCRAT(JT)
C>>> HM890307 TEMPORARY MOD FOR BACKWARD COMPATIBILITY
          IF (LOLGSF) JCRA = JCRA+1
C>>> END HM890307
          JCAR = JCARD(JT)
          JTDC = JTDCN(JT)
C get electronics channel number; if channel out of action, skip hit
          JSHOT = IW(NASHOT)
          IF (JSHOT .NE. 0) THEN
            DO 295 IHOT = 1, LROWS(JSHOT)
              IF (JCRA   .EQ. ITABL(JSHOT,IHOT,JSHOCR) .AND.
     +            JCAR-1 .EQ. ITABL(JSHOT,IHOT,JSHOCA) .AND.
     +            JTDC-1 .EQ. ITABL(JSHOT,IHOT,JSHOCH))     GOTO 310
  295       CONTINUE
          ENDIF
          JCHAN = (JCRA-1)*MCRDSG*MTDCSG + (JCAR-1)*MTDCSG + JTDC-1
          JWD = IBITS(JCHAN,5,27) + 1
          JBT = IBITS(JCHAN,0,5)
          IF (IBITS(JWIRSC(JWD),JBT,1) .EQ. 0)              GOTO 310
          IOR = 0
  300     CALL SRWIRE(JCRA,JCAR,JTDC,IOR,ISIDE,ILAYE,ISECT,IWIRE)
          IF (ISIDE.NE.IS)                                  GOTO 310
C conversion of TDC time to drift distance
          CALL SRDIST(TIME,DIST,IER)
          IF (IER .EQ. 11) THEN
            IER = 0
            GOTO 310
          ENDIF
C
C calculate coordinates
          Z   = ZZERSG - ZOFFSG - FLOAT(MLAYSG-ILAYE)*ZDELSG
          TH1 = (RZERSG + (FLOAT(IWIRE)-.5)*RDELSG - DIST) / Z
          TH2 = (RZERSG + (FLOAT(IWIRE)-.5)*RDELSG + DIST) / Z
C
C fill coordinates work bank
          IW(KNEXT(KSCOO)+JSCOSI) = ISIDE
          IW(KNEXT(KSCOO)+JSCOLA) = ILAYE
          IW(KNEXT(KSCOO)+JSCOSC) = ISECT
          RW(KNEXT(KSCOO)+JSCOTT) = TH1
          RW(KNEXT(KSCOO)+JSCOTT+1) = TH2
          IW(KNEXT(KSCRP)+JSCRCR) = IDAT
          IW(KSCOO+LMHROW) = LROWS(KSCOO)+1
          IW(KSCRP+LMHROW) = LROWS(KSCRP)+1
          NDAT(ILAYE) = NDAT(ILAYE)+1
C
C if ored wires remain, repeat calculation of coordinates
          IF (IOR .NE. 0) GOTO 300
C
C bottom of loop over data of one event
  310   CONTINUE
C
C fill pointer bank from layers to coordinates
        IW(KROW(KSSCP,(IS-1)*MLAYSG+1)+JSSCPE) =
     +    ITABL(KSSCP,(IS-1)*MLAYSG+1,JSSCPB) + NDAT(1) - 1
        DO 320 KLAY = 2, MLAYSG
          IW(KROW(KSSCP,(IS-1)*MLAYSG+KLAY)+JSSCPB) =
     +      ITABL(KSSCP,(IS-1)*MLAYSG+KLAY-1,JSSCPE) + 1
          IW(KROW(KSSCP,(IS-1)*MLAYSG+KLAY)+JSSCPE) =
     +      ITABL(KSSCP,(IS-1)*MLAYSG+KLAY,JSSCPB) + NDAT(KXX(KLAY)) - 1
  320   CONTINUE
C
C bottom of loop over sides
  330 CONTINUE
C
C compress SCOO and SCRP banks to actual size
      CALL AUBPRS('SCOOSCRP')
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRCOOR',-IER,
     +  'No space for new bank SCOO, SCRP, or SSCP')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
