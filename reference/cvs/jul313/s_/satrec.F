      SUBROUTINE SATREC(MODE,LCHCK)
C----------------------------------------------------------------------
C! Top level SATR reconstruction
C!
C!    Author:     H. Meinhard       23-Mar-1987
C!    Modified:   H. Meinhard       24-Apr-1991  (6)
C!    Modified:   H. Meinhard       31-May-1991  (7)
C!       Calculate patches if too many wire parallels
C!
C!    Input:      - MODE      /I    reconstruction mode:
C!                                  0 = normal, try first double, then
C!                                      single arm fits
C!                                  1 = double arm fits excluded
C!                                  2 = double arm fits only
C!                - LCHCK     /L    .TRUE. = trigger and LCAL informa-
C!                                      tion should be checked before
C!                                      SATR reconstruction
C!                                  .FALSE. = no checks, try SATR re-
C!                                      construction in any case
C!    Created:    - banks 'SCOO' 'SFTR' 'STCP'
C!                  all other banks which were created in subroutines
C!                  called from SATREC are dropped at the end of SATREC
C!
C!    Description
C!    ===========
C!    Steering routine for SATR event reconstruction: Find tracks from
C!    SATR coordinates
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "lcnamc.h"
#include "rcurnt.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "sanamc.h"
#include "ssppjj.h"
      LOGICAL LCHCK
      LOGICAL TLCAL,TRAND,TPHYS,TSATR,FAIL,SREC
#include "bosext.h"
      DATA ICALL /0/
#include "bmacro.h"
C----------------------------------------------------------------------
C     initialization
      IF (ICALL .EQ. 0) THEN
        SREC = .FALSE.
        NSREC = NAMIND('SREC')
        IF (IW(NSREC) .NE. 0) SREC = .TRUE.
        ICALL = 1
      ENDIF
C
C>>>>>> Error handling is done only preliminary
C
      IER = 0
C
C? check whether coordinates exist; if not, no reconstruction
      IF (IW(NASCOO) .EQ. 0)                                GOTO 998
C
C? Do we want to check the trigger and the LCAL reconstruction result?
      IF (LCHCK  .AND. .NOT. SREC) THEN
C? process only for LCAL only triggers, and if there are at least
C? two LCAL clusters, or the SATR trigger bit was set.
C? IF THERE IS AN SREC CARD IN THE DATA CARD INPUT,
C? SATR RECONSTRUCTION WILL BE DONE.
        CALL RTRIGF(IRUNRC,TLCAL,TRAND,TPHYS,TSATR,FAIL)
        IF (.NOT. FAIL) THEN
          IF (.NOT.TLCAL .OR. TPHYS)                        GOTO 998
        ENDIF
        IF (FAIL .OR. (.NOT. FAIL .AND. .NOT. TSATR)) THEN
          JLIDT = IW(NALIDT)
          IF (JLIDT .EQ. 0)                                 GOTO 998
          IF (JLIDT .NE. 0) THEN
            IF (LROWS(JLIDT) .LE. 1)                        GOTO 998
          ENDIF
        ENDIF
      ENDIF
C
C? calculate correction to azimuth angles due to magnetic field
      CALL SRDPHB
C
C? find clusters of fired wires in same spatial orientation
      CALL SRCLUS(IER)
      IF (IER .EQ. 1)                                       GOTO 998
      IF (IER .NE. 0)                                       GOTO 901
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Clusters found:'
        CALL SCLUPR(LOUTRL)
      ENDIF
C
C? find patches of clusters
      CALL SRPATC(IER)
      IF (IER .EQ. 1)                                       GOTO 998
      IF (IER .NE. 0)                                       GOTO 901
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Patches found:'
        CALL SPATPR(LOUTRL)
      ENDIF
C
C? if patches on both sides, try double arm pattern recognition and
C? track fit
      JSSPP = IW(NASSPP)
      IF (ITABL(JSSPP,1,JSSPPE) .GE. ITABL(JSSPP,1,JSSPPB) .AND.
     +    ITABL(JSSPP,2,JSSPPE) .GE. ITABL(JSSPP,2,JSSPPB) .AND.
     +    MODE .NE. 1) THEN
C
C? create bank with corrected coordinates
        CALL SACOCR(IER)
        IF (IER .EQ. 1)                                     GOTO 998
        IF (IER .NE. 0)                                     GOTO 901
        IF (JDBDRF(JULSA) .GE. 4) THEN
          WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +      'Corrected coordinates found:'
          CALL SACOPR(LOUTRL)
        ENDIF
C
C? perform double arm pattern recognition
        CALL SRWPA2(IER)
        IF (IER .EQ. 1)                                     GOTO 300
        IF (IER .EQ. 3)                                     GOTO 300
        IF (IER .NE. 0)                                     GOTO 901
        IF (JDBDRF(JULSA) .GE. 4) THEN
          WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +      'Wire parallels for double arm fit found:'
          CALL SWP2PR(LOUTRL)
        ENDIF
C
C? double arm fit
        CALL SRDBLE(IER)
        IF (IER .EQ. 1 .OR. IER .EQ. -1)                    GOTO 300
        IF (IER .NE. 0)                                     GOTO 901
        IF (JDBDRF(JULSA) .GE. 4) THEN
          WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +      'Double arm tracks fitted:'
          CALL SFTRPR(LOUTRL)
        ENDIF
C
C? select double arm fit
        CALL SRSEL2(IER)
        IF (IER .EQ. 1)                                     GOTO 300
        IF (IER .NE. 0)                                     GOTO 901
        IF (JDBDRF(JULSA) .GE. 2) THEN
          WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +      'Double arm tracks selected:'
          CALL SFTRPR(LOUTRL)
        ENDIF
        GOTO 998
      ENDIF
C
  300 CONTINUE
      IER = 0
C
C only double arm reconstruction?
      IF (MODE .EQ. 2)                                      GOTO 998
C
C? find wire paralleles corresponding to patches
      CALL SRWPAR(IER)
      IF (IER .EQ. 1)                                       GOTO 998
      IF (IER .EQ. 3)                                       GOTO 310
      IF (IER .NE. 0)                                       GOTO 901
      IF (JDBDRF(JULSA) .GE. 4) THEN
        WRITE (LOUTRL,'(/A)') ' ++++    SATR reconstruction: '//
     +    'Wire parallels corresponding to patches found:'
        CALL SWPAPR(LOUTRL)
      ENDIF
C
C? get track candidates
      CALL SRKAND(IER)
      IF (IER .EQ. 1)                                       GOTO 310
      IF (IER .NE. 0)                                       GOTO 901
C
C? find tracks
      CALL SRTRCK(IER)
      IF (IER .EQ. 1)                                       GOTO 310
      IF (IER .NE. 0)                                       GOTO 901
C
C? if no tracks found, look for parameters of patches
  310 IER = 0
      CALL SRUPAT(IER)
      IF (IER .EQ. 1)                                       GOTO 320
      IF (IER .NE. 0)                                       GOTO 901
C
C? build the STRK bank from tracks and patches
  320 IER = 0
      CALL SRBTRK(IER)
      IF (IER .EQ. 1)                                       GOTO 998
      IF (IER .NE. 0)                                       GOTO 901
C
C? apply alignment corrections to tracks and patch parameters
      CALL SRALIG
      IF (JDBDRF(JULSA) .GE. 4) THEN
        KSTRK = IW(NASTRK)
        IF (KSTRK .NE. 0) THEN
          WRITE (LOUTRL,'(/A)')
     +    ' ++++    SATR reconstruction: Alignment correction done. '//
     +    'Tracks/patches found:'
          CALL STRKPR(LOUTRL)
        ENDIF
      ENDIF
C
C? build bank SFTR from STRK
      CALL SRBFTR(IER)
      IF (IER .EQ. 1)                                       GOTO 998
      IF (IER .NE. 0)                                       GOTO 901
      IF (JDBDRF(JULSA) .GE. 2) THEN
        IF (IW(NASFTR) .NE. 0) THEN
          WRITE (LOUTRL,'(/A)')
     +    ' ++++    SATR reconstruction: SFTR bank built. '//
     +    'Tracks / patches found: '
          CALL SFTRPR(LOUTRL)
        ENDIF
      ENDIF
C
      IF (JDBDRF(JULSA) .GE. 2) WRITE (LOUTRL,'(1X)')
C
C? if desired, print reconstruction results and fill statistics array
  998 CONTINUE
      CALL SRSTAT(0)
C
C drop intermediate banks
      CALL BDROP(IW,'SACOSCCPSCLUSKANSKCPSPATSPCPSSBPSSCPSSKPSSPPSSTP')
      CALL BDROP(IW,'SSUPSTRKSUCPSUPASWPA')
C
C? SATR reconstruction done
      GOTO 999
C----------------------------------------------------------------------
  901 CALL SRSTAT(1)
      CALL BDROP(IW,'SACOSCCPSCLUSKANSKCPSPATSPCPSSBPSSCPSSKPSSPPSSTP')
      CALL BDROP(IW,'SSUPSTRKSUCPSUPASWPA')
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
