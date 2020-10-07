      SUBROUTINE LUMICA(IECOBJ,IOK)
C---------------------------------------------------------------------
C! LCAL cleaning
C
C  Patrick Janot -- 26 April 1990
C---------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qhac.h"
#include "qmacro.h"
C---------------------------------------------------------------------
      iok = 0
      mod1 = 0
      mod2 = 0
      icluec = ktn(iecobj)
      etotrw = qpecer(iecobj)
      DO 1 itow = 1 , nplsd
        IF ( itabl(iplsd, itow, jplspe) .NE. icluec ) GOTO 1
        iaddr = itabl(iplsd, itow, jplsad)
        modu = iaddr/512 + 1
        IF ( modu .LE. 0 .OR. modu .GT. 4 ) GOTO 1
        IF ( mod1 .EQ. 0 ) THEN
          mod1 = modu
        ELSEIF ( mod2 .EQ. 0 .AND. mod1 .NE. modu ) THEN
          mod2 = modu
        ENDIF
    1 CONTINUE
      epadl = padl(mod1)
      ewirl = wirl(mod1)
      edifl = difl(mod1)
      IF ( mod2 .NE. 0 ) THEN
        epadl = epadl + padl(mod2)
        ewirl = ewirl + wirl(mod2)
        edifl = edifl + difl(mod2)
      ENDIF
C
      IF ( idbg .GE. 10 .AND.
     .   ( epadl .NE. 0 .OR. ewirl .NE. 0 ) ) THEN
        CALL hfill(120,epadl,ewirl,1.)
        CALL hfill(121,edifl,0.,1.)
        CALL hfill(122,epadl/(ewirl+.01),0.,1.)
        CALL hfill(122+mod1,epadl/(ewirl+.01),0.,1.)
      ENDIF
      IF ( idbg .GE. 10 ) WRITE (IW(6),*) 'Lumi : modules/clu/pad/fil ',
     &   mod1, mod2, qe(iecobj), epadl, ewirl

      IF ( idbg .GE. 1 ) CALL looses('LCALCLUS',1)

      IF ( qe(iecobj)/5. .GT. ewirl ) GOTO 999
      IF ( idbg .GE. 1 ) CALL looses('LCALCLUS',2)
C
      IF ( etotrw - ewirl .GT. 2. ) THEN
        corfac = ewirl/etotrw
        IF ( idbg .GE. 11 )
     .  WRITE (IW(6),*) ' LCAL cluster energy scaled by : ',corfac
        rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecer) =
     .  rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecer) * corfac
        rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecec) =
     .  rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecec) * corfac
        etotrw = qpecer(iecobj)
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('LCALCLUS',3)
C
C Quality of cluster
C
      IF ( edifl .GE. 2.) THEN
        pp3  = 1. - qpece1(iecobj) - qpece2(iecobj)
        IF ( qpece1(iecobj) .GT. .99 .OR.
     .       qpece2(iecobj) .GT. .99 .OR.
     .       pp3            .GT. .99  )
     .  GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('LCALCLUS',4)
C
      iok = 1
  999 RETURN
      END
#endif
