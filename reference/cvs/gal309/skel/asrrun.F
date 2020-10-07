*DK asrrun
      SUBROUTINE ASRRUN
C ------------------------------------------------------------------
C - F.Ranjard - 861013
C!  Decode contents of RUNH and ASRU
C - Called from   ASIJOB, ASPRUN                  from this .HLB
C ------------------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "iocom.h"
#include "alfgeo.h"
#include "kincom.h"
#include "jqcom.h"
#include "ajjpar.h"
#include "kgjjpar.h"
#include "rlepjj.h"
      PARAMETER (LNAM=4)
      CHARACTER*4 CHAINT,TNAME(LNAM)
      DATA TNAME /'LUND','SJET','PART','USER'/
#include "bmacro.h"
C ------------------------------------------------------------------
C
C - Decode RUNH bank
      KRUNH  = IW(NARUNH)
      KEVEH = IW(NAEVEH)
      IF (KRUNH .EQ. 0) KRUNH = KEVEH
      IF (KRUNH .GT. 0) THEN
        IEXPJO = IW(KRUNH+1)
        IRUNJO = IW(KRUNH+2)
        IPROKI = IW(KRUNH+3)
        MPRO   = IPROKI / LOFFMC
        IF (MPRO .EQ. 0) THEN
          CALL ALTELL ('ASRRUN: not a MonteCarlo runtype ',0,'STOP')
        ELSE
          MPRO = MIN (MPRO,LNAM)
          IF (KRUNH .NE. KEVEH) THEN
            WRITE (LOUTIO,810) IRUNJO,IW(KRUNH+3),TNAME(MPRO)
     &                     ,(IW(KRUNH+M),M=4,5)
  810       FORMAT (/1X,'+++ASRRUN+++ RUN # ',I6,3X,'run type = '
     &            ,I6,2X,A4,3X,'date and time ',I8,2X,I8/)
          ELSE
            WRITE (LOUTIO,820) IRUNJO,IW(KRUNH+3),TNAME(MPRO)
  820       FORMAT (/1X,'+++ASRRUN+++ RUN # ',I6,3X,'run type = '
     &                    ,I6,2X,A4)
          ENDIF
        ENDIF
      ENDIF
C
C - Decode RLEP bank
      JRLEP = IW(NAMIND('RLEP'))
      IF (JRLEP .NE. 0) THEN
        ALECMS = 2.*REAL(ITABL(JRLEP,1,JRLELE)) * .001
      ENDIF
C
C - Decode Axxx banks
      JATIT = IW(NAMIND('ATIT'))
      IF (JATIT .NE. 0) THEN
        NATIT = LROWS(JATIT)
        WRITE(LOUTIO,*)' +++ASRRUN+++ Galeph title: ',
     &                 (CHAINT(IW(JATIT+LMHLEN+J)),J=1,NATIT)
      ENDIF
C
      JAKIN = IW(NAMIND('AKIN'))
      IF (JAKIN .NE. 0) THEN
        KAKIN = JAKIN + LMHLEN
        TKINJO = CHAINT (IW(KAKIN+JAKIKT))
        LKIN = MIN (LKINP,LCOLS(JAKIN)-JAKIKP+1)
        CALL UCOPY (RW(KAKIN+JAKIKP),BKINJO(1),LKIN)
      ENDIF
C
      JAFID = IW(NAMIND('AFID'))
      IF (JAFID .NE. 0) THEN
        IF (IPROJO(2) .EQ. 0) THEN
          ALRMAX = RTABL(JAFID,1,JAFIAR)
          ALZMAX = RTABL(JAFID,1,JAFIAZ)
          ALFIEL = RTABL(JAFID,1,JAFIMF)
          ALECMS = RTABL(JAFID,1,JAFIBE)
        ENDIF
      ENDIF
C
C - Decode KRUN bank
      JKRUN = IW(NAKRUN)
      IF (JKRUN .NE. 0) THEN
        NOTRKI = ITABL (JKRUN,1,JKRUNO)
        IPROKI = ITABL (JKRUN,1,JKRUGI)
        WRITE (LOUTIO,*)' +++ASRRUN+++ Kine title: ',
     &    (CHAINT(IW(JKRUN+LMHLEN+I)),I=JKRURT,JKRUFS-1)
        WRITE (LOUTIO,*)
     &     '  - generator identifier: ',IPROXI,
     &     '  - NOtracking marker word: ',NOTRKI
      ENDIF
C
      RETURN
      END
#endif
