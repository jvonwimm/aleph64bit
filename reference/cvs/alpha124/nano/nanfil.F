      SUBROUTINE NANFIL
CKEY NANO IN ALPHA /USER
C----------------------------------------------------------------------
C! Steering of NANO unpacking in ALPHA
C! Called from QFILL
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I      DISABLED
C!                          return code  0=created all requested banks
C!                                       1=input is NOT a NANO-DST
C!                                       2=input is a NANO version < 112
C!                                       3=no space to book BOS banks
C!                                       4=found NONE
C!                                       5=unknown error
C!
C!   Libraries required: BOS77
C!
C!   Description
C!   ===========
C!
C?   Steering routine for the recreation of DST banks from NANO-DST banks.
C?   Banks are not recreated if one of the corresponding NOxx cards from
C?   ALPHA is found.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      SAVE XFIRST,NXNONE,NXNOV0,NXNOMC,NXNOCH,NXNOGA,NXNOEJ,IORUN1,
     &     IORUN2
#include "qdecl.h"
#include "qcde.h"
      INTEGER NLINK,NVERS,NAMIND
      CHARACTER*4 NCVERS,CHAINT,NOSTR,NCSTR
      INTEGER INOV0,INOCH,INONE,INOGA,INOMC,IEFLW,IEFLJ,IRET,IRC,INDST
      INTEGER INDMS,INDNT,INANO,IORUN1,IORUN2,I,INTCHA
      INTEGER IOSTR,ICSTR
      EQUIVALENCE (NCSTR,ICSTR)
      LOGICAL XFIRST,XMOCA,NXNONE,NXNOV0,NXNOMC,NXNOCH,NXNOGA,NXNOEJ
      CHARACTER*60 OUTSTR
      DATA    IORUN1,IORUN2/0,0/
      DATA XFIRST,NXNONE,NXNOV0,NXNOMC,NXNOCH,NXNOGA,NXNOEJ/.TRUE.,
     &     .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
C----------------------------------------------------------------------
C
C!..NANO DST ?
C
      IRET=0
      IF(NXNONE)THEN
        IRET=4
        RETURN
      ENDIF
      IF(.NOT.XNANO)THEN
        IRET=1
        CALL ALTELL('_NANFIL_ Input banks are NOT NANO',0,'FATAL')
        RETURN
      ENDIF
C
C!..NANO VERSION > 111 ?
C
      IF(IORUN1.NE.KRUN)THEN
        IORUN1=KRUN
        INDST=IW(NAMIND('NDST'))
        IF(INDST.EQ.0)THEN
          CALL ALTELL('_NANFIL_ Can`t find NDST bank. Aborted',0,
     &                'FATAL')
          IRET=1
          RETURN
        ELSE
C
C!..TRY TO REPAIR CHARACTER VARIABLES (EBCDIC/ASCII, BYTE SWAPPING..)
C
          IOSTR=IW(INDST+LMHLEN+18)
          IRC=0
          NCSTR='CH  '
          CALL NREBYT(IOSTR,ICSTR,IRC)
          IF(IRC.NE.0)THEN
            IRC=0
            NCSTR='RE  '
            CALL NREBYT(IOSTR,ICSTR,IRC)
          ENDIF
          IF(IRC.NE.0)THEN
            IRC=0
            NCSTR='CO  '
            CALL NREBYT(IOSTR,ICSTR,IRC)
          ENDIF
          IF(IRC.NE.0)THEN
            CALL QWMESE(
     &        '### NANFIL ### Can`t convert strings. Try to continue')
          ELSE
C..CONVERT CHARACTERS
C..POT BANK LIST
            DO 100 I=1,IW(INDST+LMHLEN+95)
              IOSTR=IW(INDST+LMHLEN+95+I)
              CALL NBYTSW(IOSTR,ICSTR,IRC)
              IW(INDST+LMHLEN+95+I)=ICSTR
  100       CONTINUE
C..NANO BANK LIST
            DO 110 I=1,IW(INDST+LMHLEN+117)
              IOSTR=IW(INDST+LMHLEN+117+I)
              CALL NBYTSW(IOSTR,ICSTR,IRC)
              IW(INDST+LMHLEN+117+I)=ICSTR
  110       CONTINUE
C..MISCANCELLOUS
            IOSTR=IW(INDST+LMHLEN+18)
            CALL NBYTSW(IOSTR,ICSTR,IRC)
            IW(INDST+LMHLEN+18)=ICSTR
            DO 120 I=138,142
              IOSTR=IW(INDST+LMHLEN+I)
              CALL NBYTSW(IOSTR,ICSTR,IRC)
              IW(INDST+LMHLEN+I)=ICSTR
  120       CONTINUE
          ENDIF
          NCVERS=CHAINT(IW(INDST+LMHLEN+138))
          IF (NCVERS.LE.' 111') THEN
            CALL ALTELL(
     &        '_NANFIL_ ALPHA cannot read NanoDst`s < 112.',0,'FATAL')
            IRET=2
            RETURN
          ENDIF
        ENDIF
C
C!..MONTE CARLO ?
C
        INDMS=IW(NAMIND('NDMS'))
        INDNT=IW(NAMIND('NDNT'))
        IF(INDMS.NE.0.OR.INDNT.NE.0)THEN
          XMOCA=.TRUE.
        ELSE
          XMOCA=.FALSE.
        ENDIF
      ENDIF
C
C!..NOxx CARDS
C
      IF(XFIRST)THEN
        INOCH=IW(NAMIND('NOCH'))
        INOMC=IW(NAMIND('NOMC'))
        INOGA=IW(NAMIND('NOGA'))
        INOV0=IW(NAMIND('NOV0'))
        INONE=IW(NAMIND('NONE'))
        IEFLW=IW(NAMIND('EFLW'))
        IEFLJ=IW(NAMIND('EFLJ'))
        INANO=IW(NAMIND('NANO'))
C!..CHARGED TRACKS
        IF(INOCH.NE.0)THEN
          NXNOCH=.TRUE.
          NXNOV0=.TRUE.
        ELSE
          NXNOCH=.FALSE.
          NXNOV0=.FALSE.
        ENDIF
C!..EFLOW JETS
        IF(IEFLW.EQ.0.AND.IEFLJ.EQ.0)THEN
          NXNOEJ=.TRUE.
        ELSE
          NXNOEJ=.FALSE.
        ENDIF
C!..MONTE CARLO TRUTH
        IF(INOMC.NE.0)THEN
          NXNOMC=.TRUE.
        ELSE
          NXNOMC=.FALSE.
        ENDIF
C!..PHOTONS
        IF(INOGA.NE.0)THEN
          NXNOGA=.TRUE.
        ELSE
          NXNOGA=.FALSE.
        ENDIF
C!..V0
        IF(INOV0.NE.0)THEN
          NXNOV0=.TRUE.
        ELSE
          NXNOV0=.FALSE.
        ENDIF
C!..NONE
        IF(INONE.NE.0)THEN
          NXNONE=.TRUE.
        ELSE
          NXNONE=.FALSE.
        ENDIF
C!..WRITE OUT NANO DST ?
        IF(INANO.NE.0)THEN
          XWNANO=.TRUE.
        ELSE
          XWNANO=.FALSE.
        ENDIF
        XFIRST=.FALSE.
      ENDIF
C PCPA and error matrix flags for ALPHA should be always .FALSE. :
      XFILPC=.FALSE.
      XFILEM=.FALSE.
C!..DROP 'S' LIST. HERE WE PUT LATER THE RESTORED DST BANKS:
      CALL BDROP(IW,'S')
C
C!..CALL ROUTINES TO RESTORE THE DST BANKS
C
C!..PART BANK
      IF(IORUN2.NE.KRUN)THEN
        IORUN2=KRUN
        CALL NREPAR(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring PART: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..EFLOW OBJECTS
      IF(.NOT.(NXNONE.OR.NXNOEJ))THEN
        CALL NREEOB(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring EFOL: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..CHARGED TRACKS
      IF(.NOT.(NXNONE.OR.NXNOCH))THEN
        CALL NREFRF(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring FRFT: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
        CALL NREFRT(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring FRTL: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
        CALL NREPTE(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring PTEX: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..EFLOW JETS
      IF(.NOT.(NXNONE.OR.NXNOEJ))THEN
        CALL NREEJE(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring EJET: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..MAIN VERTEX
      IF(.NOT.NXNONE)THEN
        CALL NREPYE(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring PYER: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..MONTE CARLO TRUTH
      IF((.NOT.(NXNONE.OR.NXNOMC)).AND.XMOCA)THEN
        CALL NREFKI(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring FKIN: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..LEPTONS
      IF(.NOT.NXNONE)THEN
        CALL NRELEP(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR =
     &      '### NANFIL ### Error while restoring PDLT/PMLT: rc=xxx'
          WRITE(OUTSTR(52:54),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..GAMMAS
      IF(.NOT.(NXNONE.OR.NXNOGA))THEN
        CALL NREPGP(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring PGAC: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..GOODIES
      IF(.NOT.(NXNONE))THEN
        CALL NREGOO(IRC)
        IF (IRC.GT.2) THEN
        OUTSTR = '### NANFIL ### Error while restoring goodies: rc=xxx'
          WRITE(OUTSTR(50:52),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
C!..V0
      IF(.NOT.(NXNONE.OR.NXNOV0.OR.NXNOCH))THEN
        CALL NREYV0(IRC)
        IF (IRC.GT.1) THEN
          OUTSTR = '### NANFIL ### Error while restoring YV0V: rc=xxx'
          WRITE(OUTSTR(47:49),'(I3)') IRC
          CALL QWMESE(OUTSTR)
          IF(IRC.EQ.2)IRET=3
        ENDIF
      ENDIF
  999 RETURN
      END
#endif
