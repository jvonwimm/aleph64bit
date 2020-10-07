      SUBROUTINE BUILDAL
C--------------------------------------------------------------------
C! Build the ALPHA environment for the energy flow particles
C
C  P. Janot   20 April 1990
C
C  Modif -- 01 oct 1990 : Work only in class KRECO
C  Modif -- 17 oct 1990 : Prevent particle energies to be > sqrt(s)/2
C  Modif -- 17 oct 1990 : Incorporate particle identification
C--------------------------------------------------------------------
#ifndef DOC
      SAVE INIT,IERR,IEFLW,NEFLW,CUTSI,EPSIL,NRUSI
      INTEGER IRFSI(5),IRLSI(5)
      CHARACTER*8 prognam
      CHARACTER*4 CHAINT
#include "parcut.h"
#include "parabank.h"
#include "intval.h"
#include "qcde.h"
      DATA IRFSI / 14647 , 16600 , 3*0 /
      DATA IRLSI / 14831 , 20000 , 3*0 /
      DATA NRUSI / 2 /
      DATA INIT / 0 / , IERR / 0 / , CUTSI / 0.058 / , EPSIL / 1.E-08 /
#include "qmacro.h"
C
C Check on real data runs of 1992 with SICAL in the setup :
C Check also the presence of NOSC cards to kill Sical clusters anyway
C
      IRSIB = 0
      DO 5 IRU = 1,NRUSI
         IF (KRUN.GE.IRFSI(IRU).AND.KRUN.LE.IRLSI(IRU)) IRSIB = IRU
 5    CONTINUE
      IF ( NLINK('NOSC',0) .GT. 0 ) irsib = 999
C
      IF ( irsib .GE. 1 .AND. XMINI ) THEN
        mindat = 0
        narhah = NAMIND('RHAH')
        IF ( narhah .GT. 0 ) irhah = IW(narhah)
        IF ( irhah .GT. 0 ) THEN
          DO istep = 1, LROWS(irhah)
            prognam(1:4) = CHAINT(ITABL(irhah,istep,1))
            prognam(5:8) = CHAINT(ITABL(irhah,istep,2))
            IF ( prognam .EQ. 'MINI    ' ) mindat = ITABL(irhah,istep,3)
            IF ( mindat .LT. 800000 ) mindat = mindat + 20000000
          ENDDO
        ENDIF
      ENDIF
C--------------------------------------------------------------------
C  Initialise  pseudo-particles
C
C - 4-moment total
      ltot = kvnew(dummy)
      ltot = kvsavc(ltot, ' ', kreco)
C - Lumi
      lum = kvnew(dummy)
      lum = kvsavc(lum, ' ', kreco)
C - Photons
      lmg = kvnew(dummy)
      lmg = kvsavc(lmg, ' ', kreco)
C - Neutral hadrons
      lmh = kvnew(dummy)
      lmh = kvsavc(lmh, ' ', kreco)
C - Charged Particles
      lch = kvnew(dummy)
      lch = kvsavc(lch, ' ', kreco)
C - V0s
      lv0 = kvnew(dummy)
      lv0 = kvsavc(lv0, ' ', kreco)
C - ECAL killed
      lkg = kvnew(dummy)
      lkg = kvsavc(lkg, ' ', kreco)
C - HCAL killed
      lkh = kvnew(dummy)
      lkh = kvsavc(lkh, ' ', kreco)
C - New particle
      inew = kvnew(dummy)
      inew = kvsavc(inew,' ',kreco)
C
C
C First reorganize good tracks from EFOL
C
      iefol = NLINK('EFOL',3)
      nefol = IW(iefol+lmhlen)
      CALL vzero(newefol(1),200)
      DO mefol = 1, nefol
        newefol(mefol) = 999
      ENDDO
C
      DO 500 mefol = 1, nefol
        jefol = iefol + lmhlen + lefola*(mefol-1)
        IF ( IW(jefol+jefoty) .GT. 3 ) GOTO 500
        newefol(mefol) = IW(jefol+jefolt)
  500 CONTINUE
      IF ( nefol .GT. 0 )
     .CALL sortzv(newefol,indefol,nefol,-1,0,0)
C
C Then fill the ENFW section
C
      CALL vzero(kfrtot(1),7)
      CALL vzero(klstot(1),7)
      kfrefw = 0
      klsefw = 0
      nchn = 0
      nhn = 0
      kskip = 0
      DO 1000 ikpar = 1 , 6
        DO 1500 mefol = 1, nefol
C
          IF ( ikpar .LE. 2 ) THEN
            IF ( newefol(indefol(mefol)) .EQ. 999 ) GOTO 1500
            kefol = indefol(mefol)-1
          ELSE
            kefol = mefol-1
          ENDIF
          jefol = iefol + lmhlen + lefola*kefol
C
          IF ( ikpar .EQ. 1 .AND. IW(jefol+jefoty) .GT. 2 ) GOTO 1500
          IF ( ikpar .EQ. 2 .AND. IW(jefol+jefoty) .NE. 3 ) GOTO 1500
          IF ( ikpar .EQ. 3 .AND. IW(jefol+jefoty) .NE. 4 ) GOTO 1500
          IF ( ikpar .EQ. 4 .AND. IW(jefol+jefoty) .NE. 5 ) GOTO 1500
          IF ( ikpar .EQ. 5 .AND. IW(jefol+jefoty) .NE. 7 ) GOTO 1500
          IF ( ikpar .EQ. 6 .AND. IW(jefol+jefoty) .NE. 8 ) GOTO 1500
          vec(1) = RW(jefol+jefopx)
          vec(2) = RW(jefol+jefopy)
          vec(3) = RW(jefol+jefopz)
          vec(4) = RW(jefol+jefoew)
C
C Special patch to kill SICAL clusters from 1992 real data only :
C and for all data if NOSC card is present
C
          IF ( irsib .NE. 0 ) THEN
            IF ( ikpar .EQ. 6 ) THEN
              kskip = kskip + 1
              GOTO 1500
            ENDIF
            IF ( ikpar .EQ. 5 .AND.
     .           XMINI .AND. mindat .LT. 931128 ) THEN
              pz = vec(3)
              pt = SQRT ( vec(1)**2 + vec(2)**2 )
              angn = PIBY2
              IF ( ABS(pz) .GT. epsil ) angn = ABS(pt/pz)
              IF ( angn .GT. cutsi ) GOTO 1501
              IF ( irsib.GT.1 .OR. (irsib.EQ.1.AND.pz.LT.0.)) THEN
                kskip = kskip + 1
                GOTO 1500
              ENDIF
            ENDIF
          ENDIF
 1501     CONTINUE
C
          IF ( ikpar .LE. 2 ) THEN
            vec(4) = SQRT(vec(1)**2+vec(2)**2+vec(3)**2+.1395675**2)
          ELSEIF ( ikpar .EQ. 3 ) THEN
            vec(4) = SQRT(vec(1)**2+vec(2)**2+vec(3)**2+.001**2)
          ELSE
            xmas2 = vec(4)**2-vec(1)**2-vec(2)**2-vec(3)**2
            IF ( xmas2 .LT. 0.001 ) THEN
              vec(4) = SQRT(vec(1)**2+vec(2)**2+vec(3)**2+.001**2)
            ELSE
              vec(4) = RW(jefol+jefoew)
            ENDIF
          ENDIF
C Copy the corresponding particle of the section EFT
          isave = kvsave(kfeft+kefol-kskip, 'ENFW')
          CALL qvset4(isave,vec)
C Store the charge of the particles!
          IF ( ikpar .LE. 2 ) THEN
             itk = IW(jefol+jefolt)
             rnr = RTABL(IW(nafrft), itk, jfrfir)
             IF ( rnr .GT. 0. ) THEN
               RW(koqvec+isave*kcqvec+jqvech) = -1.
             ELSE
               RW(koqvec+isave*kcqvec+jqvech) = +1.
             ENDIF
C  Set the original FRF2 index for charged particles
             IW(koqvec+isave*kcqvec+jqvetn) = itk
          ENDIF
C Proportion of ECAL energy in neutral hadrons
          IF ( ikpar .EQ. 4 ) THEN
            nhn = nhn + 1
            propec(nhn) = RW(jefol+jefowe)
          ENDIF
C Store the indices of ENFW Section
          IF ( kfrtot(ikpar) .EQ. 0 ) kfrtot(ikpar) = isave
          IF ( kfrefw        .EQ. 0 ) kfrefw        = isave
          klstot(ikpar) = isave
          IF ( ikpar .LE. 6 ) klsefw        = isave
 1500   CONTINUE
 1000 CONTINUE
C
C Fill the not already set ENFW indices
      IF ( kfrtot(1) .EQ. 0 ) THEN
        kfrtot(1) = kfrefw
        klstot(1) = kfrtot(1) - 1
      ENDIF
      DO ik = 2 , 7
        IF ( kfrtot(ik) .EQ. 0 ) THEN
          kfrtot(ik) = klstot(ik-1) + 1
          klstot(ik) = klstot(ik-1)
        ENDIF
      ENDDO
C Rebuild everything from the mini-DST!
      IF ( XMINI ) CALL rebuild
C And fill final flags for analysis
      nchflg = 0
      CALL vzero(idtflg(1), 200)
      CALL vzero(muflg(1), 200)
      CALL vzero(muwrd(1), 200)
      CALL vzero(ehcal(1), 200)
      CALL vzero(leflg(1), 200)
      CALL vzero(elrtr(1), 200)
      CALL vzero(elrlg(1), 200)
      CALL vzero(dzero(1), 200)
      CALL vzero(yxmult(1), 200)
      CALL vzero(yrapp (1), 200)
      CALL vzero(yang  (1), 200)
      CALL vzero(ysudnt(1), 200)
      CALL vzero(jishad(1), 200)
      CALL vzero(jimcf (1), 200)
      DO 1 itk = kfrtot(1) , klstot(2)
       nchflg = nchflg + 1
       ich = kalpha(itk)
       iid = 0
       ifl = 0
       DO 2 nch = 1 , 200
C
          IF ( ( iid .EQ. 1 .OR. itkmue(nch) .EQ. 0 ) .AND.
     .         ( ifl .EQ. 1 .OR. itkflg(nch) .EQ. 0 ) ) GOTO 1
C
          IF ( itkmue(nch) .EQ. ich ) THEN
            muflg(nchflg) = muflag(nch)
            leflg(nchflg) = leflag(nch)
            muwrd(nchflg) = muword(nch)
            ehcal(nchflg) = umhcal(nch)
            elrtr(nchflg) = elrt  (nch)
            elrlg(nchflg) = elrl  (nch)
            dzero(nchflg) = qdb(ich)
            yxmult(nchflg) = xxmult(nch)
            yrapp (nchflg) = xrapp (nch)
            yang  (nchflg) = xang  (nch)
            ysudnt(nchflg) = xsudnt(nch)
            jishad(nchflg) = iishad(nch)
            jimcf (nchflg) = iimcf (nch)
            iid = 1
          ENDIF
C
          IF ( itkflg(nch) .EQ. ich ) THEN
            idtflg(nchflg) = idflag(nch)
            ifl = 1
          ENDIF
C
    2   CONTINUE
    1 CONTINUE
C
C  Establish relations between V0 tracks and their sister
C
      DO 10 itk = kfrtot(2) , klstot(2)
        ich = kalpha(itk)
        ist = ksister(itk)
        DO 11 jtk = kfrtot(1), klstot(2)
          jch = kalpha(jtk)
          IF ( jch .EQ. KCHT(ist) ) THEN
            kam(itk-kfrtot(2)+1) = jtk
          GOTO 10
          ENDIF
   11   CONTINUE
   10 CONTINUE
C
C  Fill the "sum-particles"
C
      enetot = 0.
      enetra = 0.
      enelon = 0.
      CALL vzero(enechq(1),5)
      DO 20 ikpar = 1 , 6
        DO 21 itk = kfrtot(ikpar), klstot(ikpar)
          CALL qvaddn(ltot,itk)
          CALL qvaddn(lps(ikpar),itk)
          IF ( ikpar .GE. 5 ) ene12 = ene12 + qe(itk)
   21   CONTINUE
   20 CONTINUE
C
      IF ( idbg .GE. 5 ) CALL enfwlist(kreco)
C
      RETURN
      END
#endif
