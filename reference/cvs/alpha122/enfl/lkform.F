      FUNCTION LKFORM(ICLUEC,IECOBJ,CORFAC)
C----------------------------------------------------------------------
C!  Checks clusters
C  Returns  1 if the cluster is good ,  0 otherwise
C  diftot = difference pad-wire in the cluster module .
C
C----------------------------------------------------------------------
#ifndef DOC
#include "parabank.h"
#include "parcut.h"
#include "qhac.h"
#include "qcde.h"
      DIMENSION QT(KCQVEC,1), KT(KCQVEC,1), QV(KCQVRT,1), KV(KCQVEC,1)
#include "qmacro.h"
C----------------------------------------------------------------------
      esmax = 0.
      diftot = 0.
      nstocl = 0
      mod1 = 0
      mod2 = 0
      mod3 = 0
      corfac = 1.
      etotrw = rtabl(ipeco, icluec, jpecer)
      IF ( idbg .GE. 1 ) CALL looses('BADCLUST',1)
C
C  Energye of cluster is too big
C
      IF ( etotrw .GE. 100. ) THEN
        lkform = 1
        GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('BADCLUST',2)
      IF ( ipest .LE. 0 .OR. ietdi .LE. 0 ) GOTO 997
C
C  Loop on storeys. Storey maxi. Difference pad/wire
C
      DO 1 ipads = 1 , npest
        IF ( itabl(ipest, ipads, jpespe) .NE. icluec ) GOTO 1
        IF ( itabl(ipest, ipads, jpesed) .NE. 0) GOTO 1
        ntow  = itabl(ipest, ipads, jpeset)
        itlab = itabl(ietdi, ntow,  jetdtl)
        irow  = ibits(itlab, 16,    8     )
        icol  = ibits(itlab,  2,    9     )
        IF ( invalid(irow,icol) .EQ. 0 ) GOTO 1
        CALL emdtow(irow, icol, ksbd, kmodu, krg)
        modu = kmodu + 12*(ksbd-1)
        IF ( mod1 .EQ. 0 ) THEN
          mod1 = modu
        ELSEIF ( mod2 .EQ. 0 .AND. modu .NE. mod1 ) THEN
          mod2 = modu
        ELSEIF ( mod3 .EQ. 0 .AND. modu .NE. mod2 .AND.
     .                             modu .NE. mod1 ) THEN
          mod3 = modu
        ENDIF
        esto = rtabl(ipest, ipads, jpeser)
        IF ( esto .GT. esmax ) THEN
          esmax = esto
          modmx = modu
        ENDIF
    1 CONTINUE
      diftot = ediff(mod1)
      padene = epad (mod1)
      wirene = ewire(mod1)
      IF ( mod2 .NE. 0 ) THEN
        diftot = diftot + ediff(mod2)
        padene = padene + epad (mod2)
        wirene = wirene + ewire(mod2)
      ENDIF
      IF ( mod3 .NE. 0 ) THEN
        diftot = diftot + ediff(mod3)
        padene = padene + epad (mod3)
        wirene = wirene + ewire(mod3)
      ENDIF
      IF ( idbg .GE. 10 )
     .  WRITE (IW(6),*) 'Modules/pad/fil/raw : ',
     .              mod1,mod2,mod3,padene,wirene,etotrw
C
C  Wire energy less than  50 MeV  --> bad cluster
C
      IF ( wirene .LT. .05 ) THEN
        lkform = 1
        GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('BADCLUST',3)
C
C  Ratio E-cluster/E_fil > 5 --> bad cluster
C
      IF ( etotrw .GT. .5 .AND. etotrw/wirene .GE. 5. ) THEN
        lkform = 1
        GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('BADCLUST',4)
C
C  Difference E_cluster-wire > 500 MeV --> rescale
C
C     IF ( etotrw-wirene .GT. .5 .and. rpsf .LE. 1.001 ) THEN
      IF ( etotrw-wirene .GT. .5 ) THEN
        corfac = wirene/etotrw
        IF ( idbg .GE. 10 ) WRITE (IW(6),*) 'All storeys corrected by ',
     .                                  corfac
        rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecer) =
     .  rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecer) * corfac
        rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecec) =
     .  rw(ipeco+lmhlen+(icluec-1)*iw(ipeco+1)+jpecec) * corfac
        etotrw = etotrw * corfac
        esmax  = esmax  * corfac
        DO 2 ipads = 1 , npest
          IF ( itabl(ipest, ipads, jpespe) .NE. icluec ) GOTO 2
C         IF ( itabl(ipest, ipads, jpesed) .NE. 0) GOTO 2
          rw(ipest+lmhlen+(ipads-1)*iw(ipest+1)+jpeser) =
     .    rw(ipest+lmhlen+(ipads-1)*iw(ipest+1)+jpeser) * corfac
    2   CONTINUE
      ENDIF
C
C Quality of cluster
C
997   lkform = 0
      pp3  = 1. - qpece1(iecobj) - qpece2(iecobj)
      nhlk = knhcal(iecobj)
C     IF ( qpecer(iecobj) .LE. 1. .OR.
C    .     diftot .GE. qpecer(iecobj)/5. ) THEN
      IF ( diftot .ge. AMAX1(.5,qpecer(iecobj)/5.) ) then
        IF ( qpece1(iecobj) .GT. .99 .OR.
     .       qpece2(iecobj) .GT. .99 .OR.
     .     ( pp3 .GT. .99 .AND. nhlk .EQ. 0 ) ) THEN
          lkform = 1
          GOTO 999
        ENDIF
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('BADCLUST',5)
C
      IF ( diftot .GE. AMAX1(.5,qpecer(iecobj)/5.) .AND.
     .     etotrw .GT. 1. .AND. esmax .LT. .2 ) THEN
        lkform = 1
        GOTO 999
      ENDIF
      IF ( idbg .GE. 1 ) CALL looses('BADCLUST',6)
C
  999 IF ( lkform .EQ. 1 .AND. idbg .GE. 10 )
     .    WRITE (IW(6),*) 'ECAL tue : clus/st1/st2/pad/wire : ',
     .    etotrw/corfac,qpece1(iecobj),qpece2(iecobj),padene,wirene
      RETURN
      END
#endif
