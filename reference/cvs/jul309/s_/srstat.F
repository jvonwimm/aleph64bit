      SUBROUTINE SRSTAT(IFLAG)
C----------------------------------------------------------------------
C!    Print SATR event reconstr. summary, fill statistics array
C!
C!    Author:     H. Meinhard       17-Aug-1987
C!    Modified:   H. Meinhard       06-Mar-1990  (2)
C!
C!    Input:      - IFLAG     /I    0 = normal call
C!                                  1 = call on error in SATREC
C!                - banks 'SRTD','SCOO','SSCP','SFTR','SSCP','SSBP',
C!                        'SSPP','SSKP','SWPA'
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "sanamc.h"
#include "scoojj.h"
#include "sftrjj.h"
#include "sgeomc.h"
#include "srtdjj.h"
#include "ssbpjj.h"
#include "sscpjj.h"
#include "sskpjj.h"
#include "ssppjj.h"
#include "sstatc.h"
#include "swpajj.h"
      INTEGER NRAW(2),NCOO(2),NTRK(2),NQF(2),NCLU(2),NPAT(2),NWPA(2),
     +  NKAN(2)
#include "bmacro.h"
#include "smacro.h"
C----------------------------------------------------------------------
C link to banks
      JSRTD = IW(NASRTD)
      JSSCP = IW(NASSCP)
      JSCOO = IW(NASCOO)
      JSSBP = IW(NASSBP)
      JSSPP = IW(NASSPP)
      JSWPA = IW(NASWPA)
      JSSKP = IW(NASSKP)
      JSFTR = IW(NASFTR)
C reset counters
      CALL VZERO(NRAW,2)
      CALL VZERO(NCOO,2)
      CALL VZERO(NTRK,2)
      CALL VZERO(NQF,2)
      CALL VZERO(NCLU,2)
      CALL VZERO(NPAT,2)
      CALL VZERO(NWPA,2)
      CALL VZERO(NKAN,2)
C count objects
      IF (JSRTD .NE. 0) THEN
        DO 300 IRAW = 1, LROWS(JSRTD)
          ISIDE = (JCARD(ITABL(JSRTD,IRAW,JSRTIR))- 1)/(MCRDSG/2) + 1
          NRAW(ISIDE) = NRAW(ISIDE) + 1
  300   CONTINUE
      ENDIF
      IF (JSSCP .NE. 0) THEN
        NCOO(1) = ITABL(JSSCP,9,JSSCPE) - ITABL(JSSCP,1,JSSCPB) + 1
        NCOO(2) = ITABL(JSSCP,18,JSSCPE) - ITABL(JSSCP,10,JSSCPB) + 1
      ELSE IF (JSCOO .NE. 0) THEN
        DO 310 ICOO = 1, LROWS(JSCOO)
          ISIDE = ITABL(JSCOO,ICOO,JSCOSI)
          NCOO(ISIDE) = NCOO(ISIDE) + 1
  310   CONTINUE
      ENDIF
      IF (JSSBP .NE. 0) THEN
        NCLU(1) = ITABL(JSSBP,3,JSSBPE) - ITABL(JSSBP,1,JSSBPB) + 1
        NCLU(2) = ITABL(JSSBP,6,JSSBPE) - ITABL(JSSBP,4,JSSBPB) + 1
      ENDIF
      IF (JSSPP .NE. 0) THEN
        NPAT(1) = ITABL(JSSPP,1,JSSPPE) - ITABL(JSSPP,1,JSSPPB) + 1
        NPAT(2) = ITABL(JSSPP,2,JSSPPE) - ITABL(JSSPP,2,JSSPPB) + 1
      ENDIF
      IF (JSWPA .NE. 0) THEN
        DO 320 IWPA = 1, LROWS(JSWPA)
          ISIDE = ITABL(JSWPA,IWPA,JSWPSI)
          NWPA(ISIDE) = NWPA(ISIDE) + 1
  320   CONTINUE
      ENDIF
      IF (JSSKP .NE. 0) THEN
        NKAN(1) = ITABL(JSSKP,1,JSSKPE) - ITABL(JSSKP,1,JSSKPB) + 1
        NKAN(2) = ITABL(JSSKP,2,JSSKPE) - ITABL(JSSKP,2,JSSKPB) + 1
      ENDIF
      IF (JSFTR .NE. 0) THEN
        DO 330 ITRK = 1, LROWS(JSFTR)
          THETA = RTABL(JSFTR,ITRK,JSFTTH)
          IQF = ITABL(JSFTR,ITRK,JSFTQF)
          ISIDE = INT(THETA/PIBY2) + 1
          NTRK(ISIDE) = NTRK(ISIDE) + 1
          NQF(ISIDE) = IQF
  330   CONTINUE
      ENDIF
C fill statistics
      IF (NRAW(1) + NRAW(2) .NE. 0 .OR. NCOO(1) + NCOO(2) .NE. 0)
     +  NERDSS = NERDSS + 1
      NRAWSS(1) = NRAWSS(1) + NRAW(1)
      NRAWSS(2) = NRAWSS(2) + NRAW(2)
      NCOOSS(1) = NCOOSS(1) + NCOO(1)
      NCOOSS(2) = NCOOSS(2) + NCOO(2)
      IF (NTRK(1) .NE. 0) NETRSS(1,NQF(1)) = NETRSS(1,NQF(1)) + 1
      IF (NTRK(2) .NE. 0) NETRSS(2,NQF(2)) = NETRSS(2,NQF(2)) + 1
      NTRKSS(1,NQF(1)) = NTRKSS(1,NQF(1)) + NTRK(1)
      NTRKSS(2,NQF(2)) = NTRKSS(2,NQF(2)) + NTRK(2)
C If no SATR printing requested, return
      IF (JDBDRF(JULSA) .LE. 0 .OR. IFLAG .NE. 0) GOTO 999
      WRITE (LOUTRL,500) NRAW,NCOO,(NTRK(I),NQF(I),I=1,2),
     +  NCLU,NPAT,NWPA,NKAN
  500 FORMAT(' SATR: A/B: raw data: ',I3,'/',I3,'; coord.: ',I3,'/',
     +  I3,'; tracks: ',I3,'(',I1,')/',I3,'(',I1,'); SCLU: ',I3,'/',
     +  I3,'; SPAT: ',I3,'/',I3,'; SWPA: ',I3,'/',I3,'; SKAN: ',
     +  I3,'/',I3,'.')
  999 CONTINUE
      END
#endif
