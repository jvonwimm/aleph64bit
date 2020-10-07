      SUBROUTINE REBUILD
C--------------------------------------------------------------------
C! Rebuild the original environment when reading the MiniDST.
C
C  P. Janot   18 july  1992
C
C--------------------------------------------------------------------
#ifndef DOC
      DIMENSION pga(20,20)
      CHARACTER*8 prognam
      CHARACTER*4 CHAINT
      INTEGER IPV(10),IAV(10)
      DATA krun0/0/
#include "parcut.h"
#include "parabank.h"
#include "intval.h"
#include "eauxjj.h"
#include "qcde.h"
#include "qmacro.h"
C--------------------------------------------------------------------
      CALL vzero(itkflg(1),mxtrk)
      CALL vzero(idflag(1),mxtrk)
      CALL vzero(itkmue(1),mxtrk)
      CALL vzero(muflag(1),mxtrk)
      CALL vzero(muword(1),mxtrk)
      CALL vzero(umhcal(1),mxtrk)
      CALL vzero(leflag(1),mxtrk)
      CALL vzero(elrt(1),mxtrk)
      CALL vzero(elrl(1),mxtrk)
      CALL vzero(xxmult(1), mxtrk)
      CALL vzero(xrapp (1), mxtrk)
      CALL vzero(xang  (1), mxtrk)
      CALL vzero(xsudnt(1), mxtrk)
      CALL vzero(iishad(1), mxtrk)
      CALL vzero(iimcf (1), mxtrk)
      CALL vzero(kan(1),200)
      CALL vzero(kav(1),200)
      CALL vzero(kam(1),200)
      CALL vzero(kag(1),200)
      nchflg = 0
      nstore = 0
      nstov0 = 0
C
C First modify the bank EFOL for older MINI version (< 9.0), so that
C electron and muon Id be recovered.
C
      IF ( krun .NE. krun0 ) THEN
        krun0 = krun
        CALL ALVSN(ITYP,IPV,IAV,IYR)
        miniver=IPV(5)
      ENDIF
C
      IF ( miniver .LT. 90 ) THEN
        DO nefol = 1, LROWS(iefol)
          jefol = iefol + lmhlen + lefola*(nefol-1)
          IF ( IW(jefol+jefoty) .EQ. 1 ) RW(jefol+jefowe) = 1.
          IF ( IW(jefol+jefoty) .EQ. 2 ) RW(jefol+jefowe) = 3.
          IF ( IW(jefol+jefoty) .EQ. 0 ) RW(jefol+jefowe) = 0.
          IF ( IW(jefol+jefoty) .EQ. 3 ) RW(jefol+jefowe) = 0.
C
          jch = IW(jefol+jefolt)
          DO ich = kfcht, klcht
            IF ( KTN(ich) .EQ. jch ) GOTO 5
          ENDDO
C
    5     CONTINUE
          IF ( xmui(ich) ) THEN
            IF ( kmuiif(ich) .GT. 0 ) RW(jefol+jefowe) = 3.
          ENDIF
          IF ( xeid(ich) ) THEN
            IF ( keidip(ich) .EQ. 1 ) RW(jefol+jefowe) = 1.
          ENDIF
C
        ENDDO
      ENDIF
C
C  Then find the ALPHA track corresponding the EFOL charged objects
C
      DO 1 ikpar = 1, 2
        DO 2 mefol = 1, nefol
          IF ( ikpar .LE. 2 ) THEN
            IF ( newefol(indefol(mefol)) .EQ. 999 ) GOTO 2
            jefol = iefol + lmhlen + lefola*(indefol(mefol)-1)
          ELSE
            jefol = iefol + lmhlen + lefola*(mefol-1)
          ENDIF
          IF ( ikpar .EQ. 1 .AND. IW(jefol+jefoty) .GT. 2 ) GOTO 2
          IF ( ikpar .EQ. 2 .AND. IW(jefol+jefoty) .NE. 3 ) GOTO 2
          itk = IW(jefol+jefolt)
          DO 3 ich = kfcht, klcht
            IF ( ktn(ich) .EQ. itk ) GOTO 4
    3     CONTINUE
          GOTO 2
C
C  Store the correspondance ALPHA track <--> ENFLW object
C
    4     nstore = nstore + 1
          IF ( ikpar .EQ. 1 ) kan(nstore) = ich
          IF ( ikpar .EQ. 2 ) THEN
            nstov0 = nstov0 + 1
            kav(nstov0) = ich
          ENDIF
C
C  Store the electron and muon identification
C
          itkmue(nstore) = ich
          IF ( xeid(ich) ) THEN
            leflag(nstore) = 1 + keidip(ich) + 3 * keidif(ich)
            elrt  (nstore) = qeidri(ich,2)
            elrl  (nstore) = qeidri(ich,3)
          ENDIF
          CALL qmuido(ich,irun,ibe,ibt,im1,im2,nexp,nfir,n10,n03,
     .                xmult,rapp,ang,ishad,sudnt,idf,imcf,ier)
          IF ( idf .EQ. -10 ) idf = -4
          nfirs =   NINT(RW(jefol+jefowe)*100.) / 200 / 100
          nexps = ( NINT(RW(jefol+jefowe)*100.)
     .            - nfirs*20000 ) / 5 / 100
          iflgs = ( NINT(RW(jefol+jefowe)*100.)
     .            - nfirs*20000 - nexps*500 ) /100
          hcals =   RW(jefol+jefowe)*100.
     .            - FLOAT(nfirs)*20000. - FLOAT(nexps)*500.
     .            - FLOAT(iflgs)*100.
          IF ( n10+im1+im2 .EQ. 0 ) THEN
            nexp = nexps
            nfir = nfirs
          ENDIF
          muflag(nstore) = idf + 20 * nexp
          muword(nstore) = im1+im2 + 10*n03 + 100*n10 + 2000*nfir
          xxmult(nstore) = xmult
          xrapp (nstore) = rapp
          xang  (nstore) = ang
          xsudnt(nstore) = sudnt
          iishad(nstore) = ishad
          iimcf (nstore) = imcf
          umhcal(nstore) = hcals
C Local flag
          IF ( iflgs .GT. 0 ) THEN
            nchflg = nchflg + 1
            itkflg(nchflg) = ich
            idflag(nchflg) = iflgs
          ENDIF
    2   CONTINUE
    1 CONTINUE
C
C  Store the photon information
C
      nphot = 0
      CALL vzero(kag(1),200)
      CALL vzero(nphoch(1),200)
      CALL vzero(nphocr(1),200)
C
C  First find the ALPHA ECAL-object corresponding the EFOL photon
C
      DO 11 mefol = 1, nefol
        jefol = iefol + lmhlen + lefola*(mefol-1)
        IF ( IW(jefol+jefoty) .NE. 4 ) GOTO 11
        IF ( miniver .LT. 90 ) RW(jefol+jefowe) = 0.
        nphot = nphot + 1
        kag(nphot) = IW(jefol+jefole)
        nphocr(nphot) = NINT(RW(jefol+jefowe))
        nphoch(nphot) = NINT(10.*(RW(jefol+jefowe)-nphocr(nphot)))
   11 CONTINUE
C
C  Store the indices of the V0 track sisters.
C
      nv0tr = 0
      CALL vzero(kam(1),200)
C -- Find a V0 track
      DO 19 mefov = 1, nefol
        jefov = iefol + lmhlen + lefola*(indefol(mefov)-1)
        IF ( IW(jefov+jefoty) .NE. 3 ) GOTO 19
C -- Is it the first or the last of the pair ?
        nvtot = 0
        DO 20 mefol = indefol(mefov) , nefol
          jefol = iefol + lmhlen + lefola*(mefol-1)
          IF ( IW(jefol+jefoty) .GT. 3 ) GOTO 21
          IF ( nvtot .EQ. 0 ) THEN
            nvtot = 2
          ELSE
            nvtot = 0
          ENDIF
   20   CONTINUE
C -- Here is the sister
   21   jefos = iefol + lmhlen + lefola*(indefol(mefov)-nvtot)
C -- ALPHA index of the sister
        itk = IW(jefos+jefolt)
        DO 22 ich = kfcht, klcht
          IF ( ktn(ich) .EQ. itk ) GOTO 23
   22   CONTINUE
        GOTO 19
C -- Store it
   23   nv0tr = nv0tr + 1
        kam(nv0tr) = ich
   19 CONTINUE
C
C  Recall some quantities from EAUX
C
      ieaux = NLINK('EAUX',0)
      nphco = IW(ieaux+lmhlen+jeauca)/1000
      npeco = IW(ieaux+lmhlen+jeauca)-1000*nphco
      hadtot = RW(ieaux+lmhlen+jeauhe)
      hadkil = RW(ieaux+lmhlen+jeauhk)
      ene12  = RW(ieaux+lmhlen+jeaue2)
C
  999 RETURN
      END
#endif
