      SUBROUTINE SRTDCR
C----------------------------------------------------------------------
C! Create raw data SRTD from coordinates SCOO
C!
C!    Author:     H. Meinhard       20-Aug-1991
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "sanamc.h"
#include "scalbc.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "srtdjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
      CALL BDROP(IW,'SFTRPSTRPSPO')
      CALL PSCOOJ('E-',IER)
      CALL BLIST(IW,'S-','SCOO')
      IF (IER .EQ. 2)                                       GOTO 901
      IF (IER .EQ. 1)                                       GOTO 999
      NHITB = 0
C print original SCOO bank if desired
      IF (JDBDRF(JULSA) .GE. 3) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Coordinates read in from POT:'
        CALL SCOOPR(LOUTRL)
      ENDIF
C link to SCOO bank
      JSCOO = IW(NASCOO)
      IF (JSCOO .EQ. 0)                                     GOTO 999
C create SRTD bank
      LEN = LMHLEN + LROWS(JSCOO) * LSRTDA
      CALL AUBOS('SRTD',0,LEN,JSRTD,IGARB)
      IF (JSRTD .EQ. 0 .OR. IGARB .EQ. 2)                   GOTO 902
      IF (IGARB .EQ. 1) JSCOO = IW(NASCOO)
      IW(JSRTD+LMHCOL) = LSRTDA
C loop over entries of SCOO bank
      DO 310 ICOR = 1, LROWS(JSCOO)
C get electronics address
        CALL SRTDCN(ICOR,ICRAT,ICARD,ITDCN)
        IF (ICRAT .EQ. -99) GOTO 310
        IR = 0
        CALL MVBITS(ICRAT,0,3,IR,21)
        CALL MVBITS(ICARD-1,0,5,IR,16)
        CALL MVBITS(ITDCN-1,0,4,IR,24)
C check whether we have already an entry from that channel
        DO 300 IRAW = 1, LROWS(JSRTD)
          IF (IBITS(IR,16,12) .EQ.
     +        IBITS(ITABL(JSRTD,IRAW,JSRTIR),16,12))        GOTO 310
  300   CONTINUE
C entry is not yet there. Create one
        KSRTD = KNEXT(JSRTD)
        IW(JSRTD+LMHROW) = LROWS(JSRTD) + 1
        IW(KSRTD+JSRTIR) = IR
C calculate distance of signal to wire
        ILAYE = ITABL(JSCOO,ICOR,JSCOLA)
        TT1   = RTABL(JSCOO,ICOR,JSCOTT)
        TT2   = RTABL(JSCOO,ICOR,JSCOTT+1)
        Z     = ZZERSG - ZOFFSG - FLOAT(MLAYSG-ILAYE)*ZDELSG
        DIST  = (TT2 - TT1) * Z / 2.0
C convert distance to TDC time
        IF (DIST .LE. TSPASC(1)) THEN
          TIME = TSTASC + 0.001*TDELSC
          GOTO 306
        ENDIF
        IF (DIST .GE. TSPASC(MPOISC)) THEN
          TIME = TSTASC + (REAL(MPOISC)-1.001) * TDELSC
          GOTO 306
        ENDIF
        DO 305 IPOIN = 1, MPOISC-1
          IF (TSPASC(IPOIN  ) .LE. DIST .AND.
     +        TSPASC(IPOIN+1) .GT. DIST) THEN
            APOIN = (           DIST - TSPASC(IPOIN)) /
     +              (TSPASC(IPOIN+1) - TSPASC(IPOIN)) +
     +              REAL(IPOIN-1)
            TIME = TSTASC + APOIN * TDELSC
            GOTO 306
          ENDIF
  305   CONTINUE
C conversion was not possible. Something must have been wrong!!!!!!
                                                            GOTO 903
  306   CONTINUE
        TIME = TOFFSC - TIME
        IF (.NOT.FMCRUN) THEN
C time corrections due to crate delays, singly equipped channels
C (reverses corrections done in SREALD)
          TIME = TIME - TCRASC(ICRAT)
          IF (ITDCN .GE. 3 .AND. ITDCN .LE. 6) TIME = TIME - TCHASC
        ENDIF
        IW(KSRTD+JSRTTR) = NINT(TIME * 1.0E12)
C calculate side, count hits on side B
        ISIDE = (ICARD-1)/12 + 1
        IF (ISIDE .EQ. 2) NHITB = NHITB + 1
  310 CONTINUE
C shrink SRTD to actual size, drop coordinates
      CALL AUBPRS('SRTD')
C write out run, event number if event is suited for our needs
      IF (NHITB .LT. 10) WRITE (LOUTRL,'('' >>> '',2I10)') IRUNRC,IEVTRC
      GOTO 999
C----------------------------------------------------------------------
  901 CALL RERROR('SRTDCR',1,'No space for unpacking of PSCO bank')
      GOTO 999
  902 CALL RERROR('SRTDCR',2,'No space for new SRTD bank')
      GOTO 999
  903 CALL RERROR('SRTDCR',3,'Conversion to drift time failed')
      GOTO 999
  999 CONTINUE
      CALL BDROP(IW,'SCOOPSCO')
      END
#endif
