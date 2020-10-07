      SUBROUTINE BKINI(IER)
C---------------------------------------------------------------------
C! Find BOS bank indices - PHCO, PECO, PCOB, PEST, ETDI, EWHE, PEWI ..
C
C  Patrick Janot -- 18 Apr 1990
C---------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
      DIMENSION ier(10),msg(10)
      DATA msg/10*0/,msgmx/20/
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C---------------------------------------------------------------------
      CALL vzero(ier(1),10)
      IF ( idbg .GE. 10 ) THEN
        CALL prtabl('PECO',1)
        CALL prtabl('PHCO',0)
        CALL prtabl('PCRL',0)
        CALL prtabl('PGAC',0)
      ENDIF
C
      ipcob = 0
      npcob = 0
      napcob = namind('PCOB')
      IF ( napcob .GT. 0 ) ipcob = iw( napcob )
      IF ( ipcob .GT. 0) THEN
        npcob = lrows(ipcob)
      ELSEIF ( msg(1) .LE. msgmx ) THEN
        msg(1) = msg(1) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1007) krun,kevt
        ier(1) = 1
      ENDIF
C
C     ipeco = 0
C     npeco = 0
C     IF ( napeco .GT. 0 ) ipeco = iw(napeco)
      ipeco = NLINK('PECO',1)
      IF ( ipeco .GT. 0) THEN
        npeco = lrows(ipeco)
      ELSEIF ( msg(2) .LE. msgmx ) THEN
        msg(2) = msg(2) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1002) krun,kevt
        ier(2) = 1
      ENDIF
C
      iphco = 0
      nphco = 0
      IF ( naphco .GT. 0 ) iphco = iw(naphco)
      IF ( iphco .GT. 0) THEN
        nphco = lrows(iphco)
      ELSEIF ( msg(3) .LE. msgmx ) THEN
        msg(3) = msg(3) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1006) krun,kevt
        ier(3) = 1
      ENDIF
C
      ipest = 0
      npest = 0
      IF ( napest .NE. 0 ) ipest = iw(napest)
      IF ( ipest .NE. 0) THEN
        npest = lrows(ipest)
      ELSEIF ( msg(4) .LE. msgmx ) THEN
        msg(4) = msg(4) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1003) krun,kevt
        ier(4) = 1
      ENDIF
C
      ietdi = 0
      netdi = 0
      IF ( naetdi .GT. 0 ) ietdi = iw(naetdi)
      IF ( ietdi .GT. 0) THEN
        netdi = lrows(ietdi)
      ELSEIF ( msg(5) .LE. msgmx ) THEN
        msg(5) = msg(5) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1004) krun,kevt
        ier(5) = 1
      ENDIF
C
      iewhe = 0
      newhe = 0
      naewhe = namind('EWHE')
      IF ( naewhe .GT. 0 ) iewhe = iw( naewhe )
      IF ( iewhe .GT. 0) THEN
        newhe = lrows(iewhe)
      ELSEIF ( msg(6) .LE. msgmx ) THEN
        msg(6) = msg(6) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1001) krun,kevt
        ier(6) = 1
      ENDIF
C
      ipewi = 0
      npewi = 0
      napewi = namind('PEWI')
      napwei = namind('PWEI')
      IF ( napewi .GT. 0 ) ipewi = iw( napewi )
      IF ( napwei .GT. 0 ) ipwei = iw( napwei )
      IF ( ipewi .LE. 0 ) ipewi = ipwei
      IF ( ipewi .GT. 0) THEN
        npewi = lrows(ipewi)
      ELSEIF ( msg(7) .LE. msgmx ) THEN
        msg(7) = msg(7) + 1
        IF ( idbg .GE. 1 ) WRITE (IW(6),1009) krun,kevt
        ier(7) = 1
      ENDIF
C
      iplpd = 0
      nplpd = 0
      naplpd = namind('PLPD')
      IF ( naplpd .GT. 0 ) iplpd = iw( naplpd )
      IF ( iplpd .GT. 0) THEN
        nplpd = lrows(iplpd)
      ELSE
      ENDIF
C
      iplsd = 0
      nplsd = 0
      naplsd = namind('PLSD')
      IF ( naplsd .GT. 0 ) iplsd = iw( naplsd )
      IF ( iplsd .GT. 0) THEN
        nplsd = lrows(iplsd)
      ELSE
      ENDIF
C
      isilu = 0
      nsilu = 0
      nasilu = namind('SILU')
      IF ( nasilu .GT. 0 ) isilu = iw( nasilu )
      IF ( isilu .GT. 0) THEN
        nsilu = lrows(isilu)
      ELSE
      ENDIF
C
C     DO ibk = 1 , 9
C       ier(10) = ier(10) + ier(ibk)
C     ENDDO
C
 999  CONTINUE
      RETURN
C------------------------------------------------------------------
 1001 FORMAT(1X,'EWHE missing in Run #',I5,1x,'in Event #',I5)
 1002 FORMAT(1X,'PECO missing in Run #',I5,1x,'in Event #',I5)
 1003 FORMAT(1X,'PEST missing in Run #',I5,1x,'in Event #',I5)
 1004 FORMAT(1X,'ETDI missing in Run #',I5,1x,'in Event #',I5)
 1006 FORMAT(1x,'PHCO missing in Run #',I5,1x,'in Event #',I5)
 1007 FORMAT(1x,'PCOB missing in Run #',I5,1x,'in Event #',I5)
 1008 FORMAT(1x,'PLPD missing in Run #',I5,1x,'in Event #',I5)
 1009 FORMAT(1x,'PEWI missing in Run #',I5,1x,'in Event #',I5)
      END
#endif
