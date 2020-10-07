      SUBROUTINE SRBTRK(IER)
C----------------------------------------------------------------------
C! Build the STRK bank
C!
C!    Author:     H. Meinhard       23-Jun-1988
C!    Modified:   H. Meinhard       03-Jul-1989  (3)
C!
C!    Output:     - IER       /I    error flag, 0 = no error
C!
C!    Description
C!    ===========
C!    Build the STRK bank from tracks and patches. Thus all objects
C!    found during SATR reconstruction are stored in the STRK bank.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "sanamc.h"
#include "sstpjj.h"
#include "ssupjj.h"
#include "stcpjj.h"
#include "strkjj.h"
#include "sucpjj.h"
#include "supajj.h"
#include "swrkbk.h"
      LOGICAL TRACK,PATCH
#include "bmacro.h"
C----------------------------------------------------------------------
C link to track bank
      KSTRK = IW(NASTRK)
      KSTCP = IW(NASTCP)
      KSSTP = IW(NASSTP)
      IF (KSTRK .NE. 0 .AND. KSTCP .NE. 0 .AND. KSSTP .NE. 0) THEN
        TRACK = .TRUE.
        NTRK = LROWS(KSTRK)
      ELSE
        TRACK = .FALSE.
        NTRK = 0
      ENDIF
C
C link to patch bank
      KSUPA = IW(NASUPA)
      KSUCP = IW(NASUCP)
      KSSUP = IW(NASSUP)
      IF (KSUPA .NE. 0 .AND. KSUCP .NE. 0 .AND. KSSUP .NE. 0) THEN
        PATCH = .TRUE.
        NPAT = LROWS(KSUPA)
      ELSE
        PATCH = .FALSE.
        NPAT = 0
      ENDIF
C
C neither tracks nor patches: return with error status
      IF (.NOT. TRACK .AND. .NOT. PATCH)                    GOTO 901
C
C if there are both tracks and patches, store tracks in work bank
      IF (TRACK .AND. PATCH) THEN
        CALL BKTOW(IW,'STRK',0,IW,KWK1SW,*905)
        CALL BKTOW(IW,'STCP',0,IW,KWK2SW,*905)
      ENDIF
C
C if there are patches, create/enlarge STRK bank and STCP bank
      IF (PATCH) THEN
        CALL BDROP(IW,'STRKSTCP')
        CALL AUBOS('STRK',0,LMHLEN+(NTRK+NPAT)*LSTRKA,KSTRK,IGARB)
        IF (KSTRK .EQ. 0 .OR. IGARB .EQ. 2)                 GOTO 902
        IF (IGARB .EQ. 1) THEN
          KSUPA = IW(NASUPA)
          KSUCP = IW(NASUCP)
          KSSUP = IW(NASSUP)
        ENDIF
        IW(KSTRK+LMHCOL) = LSTRKA
        IW(KSTRK+LMHROW) = 0
        CALL AUBOS('STCP',0,LMHLEN+(NTRK+NPAT)*LSTCPA,KSTCP,IGARB)
        IF (KSTCP .EQ. 0 .OR. IGARB .EQ. 2)                 GOTO 903
        IF (IGARB .EQ. 1) THEN
          KSUPA = IW(NASUPA)
          KSUCP = IW(NASUCP)
          KSSUP = IW(NASSUP)
          KSTRK = IW(NASTRK)
        ENDIF
        IW(KSTCP+LMHCOL) = LSTCPA
        IW(KSTCP+LMHROW) = 0
        IF (.NOT. TRACK) THEN
          CALL AUBOS('SSTP',0,LMHLEN+2*LSSTPA,KSSTP,IGARB)
          IF (KSSTP .EQ. 0 .OR. IGARB .EQ. 2)               GOTO 904
          IF (IGARB .EQ. 1) THEN
            KSUPA = IW(NASUPA)
            KSUCP = IW(NASUCP)
            KSSUP = IW(NASSUP)
            KSTRK = IW(NASTRK)
            KSTCP = IW(NASTCP)
          ENDIF
          IW(KSSTP+LMHCOL) = LSSTPA
          IW(KSSTP+LMHROW) = 2
        ENDIF
      ENDIF
C
C if there are patches, copy both tracks and patches into new bank
      IF (PATCH) THEN
        DO 360 ISIDE = 1, 2
C
C copy the tracks
          NSTRT = ITABL(KSSTP,ISIDE,JSSTPB)
          NSTOP = ITABL(KSSTP,ISIDE,JSSTPE)
          IW(KROW(KSSTP,ISIDE)+JSSTPB) = LROWS(KSTRK) + 1
          IF (TRACK) THEN
            DO 320 ITRK = NSTRT,NSTOP
              DO 300 IWORD = 1, LSTRKA
  300         IW(KNEXT(KSTRK)+IWORD) = ITABL(KWK1SW,ITRK,IWORD)
              IW(KNEXT(KSTRK)+JSTRQF) = 1
              IW(KSTRK+LMHROW) = LROWS(KSTRK) + 1
              DO 310 IWORD = 1, LSTCPA
  310         IW(KNEXT(KSTCP)+IWORD) = ITABL(KWK2SW,ITRK,IWORD)
              IW(KSTCP+LMHROW) = LROWS(KSTCP) + 1
  320       CONTINUE
          ENDIF
          IW(KROW(KSSTP,ISIDE)+JSSTTE) = LROWS(KSTRK)
C
C copy the patches
          IW(KROW(KSSTP,ISIDE)+JSSTPS) = LROWS(KSTRK) + 1
          DO 350 IPAT = ITABL(KSSUP,ISIDE,JSSUPB),
     +                  ITABL(KSSUP,ISIDE,JSSUPE)
            DO 330 IWORD = 1, LSUPAA
  330       IW(KNEXT(KSTRK)+IWORD) = ITABL(KSUPA,IPAT,IWORD)
            IW(KNEXT(KSTRK)+JSTRQF) = 0
            IW(KSTRK+LMHROW) = LROWS(KSTRK) + 1
            DO 340 IWORD = 1, LSTCPA
  340       IW(KNEXT(KSTCP)+IWORD) = ITABL(KSUCP,IPAT,IWORD) * 2
            IW(KSTCP+LMHROW) = LROWS(KSTCP) + 1
  350     CONTINUE
          IW(KROW(KSSTP,ISIDE)+JSSTPE) = LROWS(KSTRK)
C
C bottom of loop over sides
  360   CONTINUE
C
C else clause: if no patches, simply set the quality flag to 1
      ELSE
        DO 370 ITRK = 1, NTRK
  370   IW(KROW(KSTRK,ITRK)+JSTRQF) = 1
        DO 380 ISIDE = 1, 2
          IW(KROW(KSSTP,ISIDE)+JSSTTE) = ITABL(KSSTP,ISIDE,JSSTPE)
          IW(KROW(KSSTP,ISIDE)+JSSTPS) = ITABL(KSSTP,ISIDE,JSSTPE) + 1
  380   CONTINUE
      ENDIF
C
C drop the work banks, if generated
      IF (TRACK .AND. PATCH) THEN
        CALL WDROP(IW,KWK1SW)
        CALL WDROP(IW,KWK2SW)
        CALL WGARB(IW)
      ENDIF
C
      GOTO 999
C----------------------------------------------------------------------
  901 IER = 1
      GOTO 999
  902 IER = 2
      CALL RERROR('SRBTRK',-IER,'No space for new/enlarged bank STRK')
      GOTO 998
  903 IER = 3
      CALL RERROR('SRBTRK',-IER,'No space for new/enlarged bank STCP')
      GOTO 998
  904 IER = 4
      CALL RERROR('SRBTRK',-IER,'No space for new bank SSTP')
      GOTO 998
  905 IER = 5
      CALL RERROR('SRBTRK',-IER,'No space for new work banks')
      GOTO 998
  998 IF (TRACK .AND. PATCH) THEN
        CALL WDROP(IW,KWK1SW)
        CALL WDROP(IW,KWK2SW)
        CALL WGARB(IW)
      ENDIF
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
