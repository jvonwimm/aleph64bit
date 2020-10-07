      SUBROUTINE SRFTRK(IER)
C----------------------------------------------------------------------
C! Find SATR tracks
C!
C!    Author:     H. Meinhard       09-Feb-1988
C!    Modified:   H. Meinhard       20-Nov-1989  (4)
C!
C!    Output:     - IER       /I    error flag (0 = no error)
C!
C!    Description
C!    ===========
C!    For each side, loop over the track candidates, find the best one
C!    (currently, this means most wire parallels, and, if there are
C!    several, the one with the smallest chisquare), and for the remai-
C!    ning candidates, remove all wire paralleles using the same hit
C!    as a wire parallele in the selected candidate, and redo the fit
C!    if necessary. Repeat this procedure until no more candidates are
C!    available.
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
#include "sstpjj.h"
#include "stcpjj.h"
#include "strkjj.h"
#include "swrkbk.h"
      LOGICAL LMOD
      REAL THRAW(MLASG),PHRAW(MLASG),SIG(MLASG),COVXY(2,2)
#include "bosext.h"
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C look whether bank STRK exists; if yes, return
      IF (IW(NASTRK) .NE. 0)                                GOTO 999
C
C link to banks needed
      KSCOO = IW(NASCOO)
      KSKAN = IW(NASKAN)
      KSKCP = IW(NASKCP)
      KSSKP = IW(NASSKP)
      IF (KSCOO .EQ. 0 .OR. KSKAN .EQ. 0 .OR. KSKCP .EQ. 0 .OR.
     +               KSSKP .EQ. 0)                          GOTO 901
      NKAN = LROWS(KSKAN)
C
C copy SKAN and SKCP to work banks
      CALL BKTOW(IW,'SKAN',0,IW,KWK1SW,*903)
      IW(KWK1SW-3) = INTCHA('SKAN')
      CALL BKTOW(IW,'SKCP',0,IW,KWK2SW,*903)
      IW(KWK2SW-3) = INTCHA('SKCP')
C
C create banks for tracks and for pointers from tracks to coordinates
      CALL AUBOS('STRK',0,LMHLEN+NKAN*LSTRKA,KSTRK,IGARB)
      IF (KSTRK.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSKAN = IW(NASKAN)
        KSKCP = IW(NASKCP)
        KSSKP = IW(NASSKP)
        KSTRK = IW(NASTRK)
      ENDIF
      IW(KSTRK+LMHCOL) = LSTRKA
C
      CALL AUBOS('STCP',0,LMHLEN+NKAN*LSTCPA,KSTCP,IGARB)
      IF (KSTCP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSKAN = IW(NASKAN)
        KSKCP = IW(NASKCP)
        KSSKP = IW(NASSKP)
        KSTRK = IW(NASTRK)
        KSTCP = IW(NASTCP)
      ENDIF
      IW(KSTCP+LMHCOL) = LSTCPA
C
      CALL AUBOS('SSTP',0,LMHLEN+2*LSSTPA,KSSTP,IGARB)
      IF (KSSTP.EQ.0 .OR. IGARB.EQ.2)                       GOTO 902
      IF (IGARB .EQ. 1) THEN
        KSCOO = IW(NASCOO)
        KSKAN = IW(NASKAN)
        KSKCP = IW(NASKCP)
        KSSKP = IW(NASSKP)
        KSTRK = IW(NASTRK)
        KSTCP = IW(NASTCP)
        KSSTP = IW(NASSTP)
      ENDIF
      IW(KSSTP+LMHCOL) = LSSTPA
      IW(KSSTP+LMHROW) = 2
C
C get sigma in theta
      SIGM = SIGMSR / (ZZERSG-ZOFFSG-FLOAT(MLAYSG)*ZDELSG/2.)
C
C top of loop over sides
      DO 380 ISIDE = 1, 2
        IW(KROW(KSSTP,ISIDE)+JSSTPB) = LROWS(KSTRK) + 1
C
C NKAN is the number of track candidates still to be considered on side
        NKAN = ITABL(KSSKP,ISIDE,JSSKPE) - ITABL(KSSKP,ISIDE,JSSKPB) + 1
C
C find best candidate
  300   IF (NKAN .EQ. 0)                                    GOTO 370
        NPMAX = 0
        CHMIN = 1000.
        DO 310 IKAN = ITABL(KSSKP,ISIDE,JSSKPB),
     +                ITABL(KSSKP,ISIDE,JSSKPE)
          IF (ITABL(KWK1SW,IKAN,JSKASI) .GT. 0) THEN
            NPAR  = ITABL(KWK1SW,IKAN,JSKANP)
            CHISQ = MAX(RTABL(KWK1SW,IKAN,JSKAC2),1.E-12)
            IF (NPAR .GT. NPMAX .OR.
     +        (NPAR .EQ. NPMAX .AND. CHISQ .LT. CHMIN)) THEN
              NPMAX = NPAR
              CHMIN = CHISQ
              IMAX = IKAN
            ENDIF
          ENDIF
  310   CONTINUE
C
C fill best candidate into BOS bank, mark candidate
        NP = 0
        DO 320 ICOR = 1, MLAYSG
          IADR = ITABL(KWK2SW,IMAX,JSKCKC+ICOR-1)
          IF (IADR .NE. 0) THEN
            NP = NP + 1
            IW(KROW(KWK2SW,IMAX)+JSKCKC+NP-1) = IADR
          ENDIF
  320   CONTINUE
        CALL SRFLTR(IMAX)
        IW(KROW(KWK1SW,IMAX)+JSKASI) = 0
        NKAN = NKAN-1
C
C in remaining candidates, remove all wire paralleles being derived from
C the same hits as wire paralleles in the selected candidate
C
C top of loop over remaining candidates
        DO 360 IKAN = ITABL(KSSKP,ISIDE,JSSKPB),
     +                ITABL(KSSKP,ISIDE,JSSKPE)
          IF (ITABL(KWK1SW,IKAN,JSKASI) .LE. 0)              GOTO 360
C
C LMOD = .TRUE. if track candidate IKAN was modified
          LMOD = .FALSE.
C
C compare coordinates of selected candidate with coordinates of
C remaining candidate
          DO 340 ICOR1 = 1, MLAYSG
            IADR1 = ITABL(KWK2SW,IMAX,JSKCKC+ICOR1-1)/2
            IF (IADR1 .EQ. 0)                               GOTO 340
            CALL SRTDCN(IADR1,ICRA1,ICAR1,ITDC1)
            IELE1 = (ICRA1-1)*MCRDSG*MTDCSG + (ICAR1-1)*MTDCSG + ITDC1
            DO 330 ICOR2 = 1, MLAYSG
              IADR2 = ITABL(KWK2SW,IKAN,JSKCKC+ICOR2-1)/2
              IF (IADR2 .EQ. 0)                             GOTO 330
              CALL SRTDCN(IADR2,ICRA2,ICAR2,ITDC2)
              IELE2 = (ICRA2-1)*MCRDSG*MTDCSG + (ICAR2-1)*MTDCSG + ITDC2
              IF (IELE1 .EQ. IELE2) THEN
                LMOD = .TRUE.
                IW(KROW(KWK1SW,IKAN)+JSKANP) =
     +            ITABL(KWK1SW,IKAN,JSKANP)-1
                IW(KROW(KWK2SW,IKAN)+JSKCKC+ICOR2-1) = 0
                GOTO 340
              ENDIF
  330       CONTINUE
  340     CONTINUE
C
          IF (LMOD) THEN
C
C not enough wire paralleles for track fit
            IF (ITABL(KWK1SW,IKAN,JSKANP) .LT. NPARSR) THEN
              IW(KROW(KWK1SW,IKAN)+JSKASI) = -1
              NKAN = NKAN-1
C
C if enough wire paralleles, redo the fit
            ELSE
              NP = 0
              DO 350 I = 1, MLAYSG
                IADR = ITABL(KWK2SW,IKAN,JSKCKC+I-1)
                IF (IADR .NE. 0) THEN
                  NP = NP + 1
                  ICOOR = IADR / 2
                  THRAW(NP) = RTABL(KSCOO,ICOOR,JSCOTT+IBITS(IADR,0,1))
                  PHRAW(NP) = PHIXXX(ITABL(KSCOO,ICOOR,JSCOLA),
     +              ITABL(KSCOO,ICOOR,JSCOSC))
                  SIG(NP) = SIGM
                ENDIF
  350         CONTINUE
              CALL SRFIT0(NP,THRAW,PHRAW,SIG,AX,AY,CHISQ,COVXY,IER)
              IF (IER .NE. 0) THEN
                IW(KROW(KWK1SW,IKAN)+JSKASI) = -1
                NKAN = NKAN - 1
                IER = 0
              ELSE
                RW(KROW(KWK1SW,IKAN)+JSKAC2) = CHISQ
                RW(KROW(KWK1SW,IKAN)+JSKAAX) = AX
                RW(KROW(KWK1SW,IKAN)+JSKAAY) = AY
                RW(KROW(KWK1SW,IKAN)+JSKAXX) = COVXY(1,1)
                RW(KROW(KWK1SW,IKAN)+JSKAXY) = COVXY(1,2)
                RW(KROW(KWK1SW,IKAN)+JSKAYY) = COVXY(2,2)
              ENDIF
            ENDIF
          ENDIF
C
C bottom of loop over remaining candidates
  360   CONTINUE
        GOTO 300
C
C bottom of loop over sides
  370   IW(KROW(KSSTP,ISIDE)+JSSTPE) = LROWS(KSTRK)
  380 CONTINUE
C
C compress STRK and STCP banks to actual size, drop work banks
      CALL AUBPRS('STRKSTCP')
C
      CALL WDROP(IW,KWK1SW)
      CALL WDROP(IW,KWK2SW)
      CALL WGARB(IW)
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRFTRK',-IER,
     +  'No space for new bank STRK, STCP, or SSTP')
      GOTO 998
  903 IER = 3
      CALL RERROR('SRFTRK',-IER,'No space for new work banks')
      GOTO 998
  998 CALL WDROP(IW,KWK1SW)
      CALL WDROP(IW,KWK2SW)
      CALL WGARB(IW)
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
