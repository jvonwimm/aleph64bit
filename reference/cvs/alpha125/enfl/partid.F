      SUBROUTINE PARTID
C-----------------------------------------------------------------------
C! Identify charged particles as muon or electrons.
C
C  Patrick Janot -- 07 Jun 1990
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
#include "cuphco.h"
#include "intval.h"
      DIMENSION pga(20,20)
      LOGICAL first
      DATA first/.TRUE./
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      keidpe(ki)=iw(iw(kjqdet(ki)+jqdeae)+jeidpe)
C
      CALL vzero(itkflg(1),mxtrk)
      CALL vzero(itkmue(1),mxtrk)
      CALL vzero(idflag(1),mxtrk)
      CALL vzero(ncht(1),mxtrk)
      CALL vzero(delt(1),mxtrk)
      CALL vzero(pexhca(1,1),3*mxtrk)
      CALL vzero(muonch(1),mxtrk)
      CALL vzero(muonhc(1),mxtrk)
      CALL vzero(enmuon(1),mxtrk)
      CALL vzero(nlecch(1),mxtrk)
      CALL vzero(enelec(1),mxtrk)
      CALL vzero(pcelec(1),mxtrk)
      CALL vzero(npelec(1),mxtrk)
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
      kchatt = 0
      nchflg = 0
      nbmuon = 0
      nelect = 0
      nstore = 0
C
C -- Loop over charged particles
C
      DO 10 ich = kfcht , klcht
C
C -- Run QEIDO if JULIA did not
C
C  Commented on Ed and Stephen request ---
C
C       IF ( julver .LE. 241 ) CALL qeido(ich,ier)
        IF ( julver .LE. 241 ) THEN
          IF ( first ) THEN
            first = .FALSE.
            WRITE (IW(6),*)
     .      '            ++++ ENFLW Warning ++++                  '
            WRITE (IW(6),*)
     .      ' You are using events processed with JULIA ',julver
            WRITE (IW(6),*)
     .      ' in which QEIDO was not called. You should thus call '
            WRITE (IW(6),*)
     .      ' it in QUENFL for each track before running QUENFLW. '
          ENDIF
        ENDIF
C
C -- Good tracks only.
C
        IF ( xlock(ich) ) GOTO 10
C
C  Store electron information
C
        nstore = nstore + 1
        itkmue(nstore) = ich
        IF ( xeid(ich) ) THEN
          leflag(nstore) = 1 + keidip(ich) + 3 * keidif(ich)
          elrt  (nstore) = qeidri(ich,2)
          elrl  (nstore) = qeidri(ich,3)
        ENDIF
C
C -- p > 800 MeV/c for the local identification.
C
        IF ( qp(ich).LT..8 ) GOTO 10
C
C -- Extrapolation up to HCAL to prepare the re-link
C
        CALL extrhc(ich)
C
C -- Muon identification
C
        CALL qmuido(ich,irun,ibe,ibt,im1,im2,nexp,nfir,n10,n03,
     .              xmult,rapp,ang,ishad,sudnt,idf,imcf,ier)
C -- Store muon information ...
        IF ( .NOT. xhma(ich) ) GOTO 100
        IF ( idf .EQ. -10 ) idf = -4
        muflag(nstore) = idf + 20 * nexp
        muword(nstore) = im1+im2 + 10*n03 + 100*n10 + 2000*nfir
        xxmult(nstore) = xmult
        xrapp (nstore) = rapp
        xang  (nstore) = ang
        xsudnt(nstore) = sudnt
        iishad(nstore) = ishad
        iimcf (nstore) = imcf
C -- Find the associated PHCO
        icluhc = 0
        IF ( qhmaed(ich) .GT. 0. ) THEN
          IF ( idbg .GE. 1 ) CALL looses('QHMAEDOK',1)
          enhcal = qhmaed(ich)
          deltae = 999.
          DO 30 iclhc = 1 , nphco
            enphco = RTABL(iphco, iclhc, jphcec)
            deltan = ABS(enhcal-enphco)
            IF ( deltan .LT. deltae ) THEN
              deltae = deltan
              icluhc = iclhc
            ENDIF
   30     CONTINUE
        ELSE
          IF ( idbg .GE. 1 ) CALL looses('QHMAEDOK',2)
          deltae = 999.
          DO 31 iobj = 1 , knhcal(ich)
            ihcobj = khcal(ich,iobj)
            deltan = ABS(qe(ihcobj)-4.6)
            IF ( deltan .LT. deltae ) THEN
              deltae = deltan
              icluhc = ktn(ihcobj)
            ENDIF
   31     CONTINUE
        ENDIF
        IF ( icluhc .EQ. 0 ) THEN
          IF ( idbg .GE. 1 ) CALL looses('QHMAEDOK',3)
          GOTO 100
        ENDIF
        enphco = RTABL(iphco, icluhc, jphcec)
        umhcal(nstore) = enphco
C -- Avoid very energetic pions with punch trough
        IF ( qp(ich) .GT. 20. .AND. enphco .GT. 20. ) GOTO 100
C -- Tight identification : QMUIDO flag
        IF ( idf .GT. 0 .AND. nexp .GE. 10 ) GOTO 20
        IF ( idf .GT. 0 .AND. im1+im2 .GE. 1 ) GOTO 20
C -- Loose identification for low momentum particles
        IF ( qp(ich) .GT. 10. ) GOTO 100
        IF ( nexp .GE. 15 .AND. n03 .GE. 1 .AND.
     .       FLOAT(nfir)/FLOAT(nexp) .GE. 0.4 ) GOTO 20
        IF ( qp(ich) .LE. 5. .AND. nfir .GE. 8 .AND.
     .     ( n10 .GE. 3 .OR. n03 .GE. 1 ) ) GOTO 20
C -- Not a muon
        GOTO 100
   20   CONTINUE
C -- Store muon information
        nbmuon = nbmuon + 1
        muonch(nbmuon) = ich
        muonhc(nbmuon) = icluhc
        npla = MAX0(nexp,nfir)
        eexp = .4*FLOAT(npla)
        IF ( enphco .LT. eexp ) THEN
          enmuon(nbmuon) = enphco
        ELSE
          enmuon(nbmuon) = eexp
        ENDIF
        nchflg = nchflg + 1
        itkflg(nchflg) = ich
        IF ( (im1+im2 .GE. 1 .OR. nexp .GE. 15) .AND.
     .        idf .GT. 0 ) THEN
          idflag(nchflg) = 3
          IF ( idbg .GE. 1 ) CALL looses('MUONS   ',1)
        ELSE
          idflag(nchflg) = 4
          IF ( idbg .GE. 1 ) CALL looses('MUONS   ',2)
        ENDIF
C And lock the track!!!! 
C (Correction 22/11/96 - Thanks to Steven Wasserbaech)
        CALL qltrk(ich)
C -- Print out
        IF ( idbg .GE. 10 ) THEN
          WRITE (IW(6),*) ' Muon !!  TPC : ',qe(ich),ich
          WRITE (IW(6),*) '         HCAL : ',enphco,icluhc
          WRITE (IW(6),*) ' Energy (GeV) : ',enmuon(nbmuon)
          WRITE (IW(6),*) ' Idf,nexp,nfir,n10,n03 : ',idf,nexp,nfir,
     &                      n10,n03
        ENDIF
        GOTO 10
  100   CONTINUE
C
C  -- Electron identification
C
        IF ( .NOT.xeid(ich) ) GOTO 10
C -- Tight identification
        IF ( keidip(ich) .EQ. 1 ) GOTO 40
C -- Loose identification
        enpeco = 0.
        jpeco = keidpe(ich)
        IF ( jpeco .GT. 0 ) enpeco = RTABL(ipeco,jpeco,jpecec)
        IF ( ABS ( qeidri(ich,3) ) .LT. 3. .AND.
     .       qp(ich) .GT. 30. .AND. enpeco .GT. 25. ) GOTO 40
        GOTO 10
   40   CONTINUE
        nelect = nelect + 1
C -- Store electron information
        nchflg = nchflg + 1
        itkflg(nchflg) = ich
        IF ( keidip(ich) .EQ. 1 ) THEN
          idflag(nchflg) = 1
          IF ( idbg .GE. 1 ) CALL looses('ELECTRON',1)
          pcelec(nelect) = qeidec(ich)
        ELSE
          idflag(nchflg) = 2
          IF ( idbg .GE. 1 ) CALL looses('ELECTRON',2)
          pcelec(nelect) = enpeco
        ENDIF
        nlecch(nelect) = ich
        enelec(nelect) = AMIN1(qp(ich),qelep/2.)
C -- Find the associated PECO
        icluec = 0
        IF ( keidpe(ich) .GT. 0 ) THEN
          IF ( idbg .GE. 1 ) CALL looses('KEIDPEOK',1)
          icluec = keidpe(ich)
        ELSE
          IF ( idbg .GE. 1 ) CALL looses('KEIDPEOK',2)
          deltae = 999.
          DO 50 iobj = 1 , knecal(ich)
            iecobj = kecal(ich,iobj)
            enpeco = qe(iecobj)
            deltan = ABS(pcelec(nelect)-enpeco)
            IF ( deltan .LT. deltae ) THEN
              deltae = deltan
              icluec = ktn(iecobj)
            ENDIF
   50     CONTINUE
        ENDIF
        IF ( icluec .LE. 0 ) THEN
          IF ( idbg .GE. 1 ) CALL looses('KEIDPEOK',3)
        ENDIF
        npelec(nelect) = icluec
        enpeco = RTABL(ipeco, icluec, jpecec)
C And lock the track
        CALL qltrk(ich)
C Print out if required
        IF ( idbg .GE. 10 ) THEN
          WRITE (IW(6),*) 'electron !!'
          WRITE (IW(6),*) 'ich,qeidec(ich),npelec(nelect),enpeco'
          WRITE (IW(6),*) ich,qeidec(ich),npelec(nelect),enpeco
        ENDIF
C
C -- End of identification
C
   10 CONTINUE
C     WRITE (IW(6),*) '# muons/electrons : ',nbmuon,nelect
 999  RETURN
      END
#endif
