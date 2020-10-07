      SUBROUTINE qucaob
C-----------------------------------------------------------------------
C! Build the ECAL and HCAL objects
C
C  Patrick Janot -- 17 Nov 1993
C
C  Modification : M. Kado -- 13 mai 1998 Neutral hadronic energy
C                                        deposition
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "fkclstr.h"
#include "calib.h"
#include "parcut.h"
#include "fsimid.h"
#include "psfrjj.h"
#include "trexjj.h"
#include "hadpho.h"

      DIMENSION par(6), parex(6), hmom(23), vecsav(4)
      DIMENSION frac(10),vha(4),vphsat(4)
      PARAMETER ( ngenu = 4 )
      CHARACTER*12 genuin(ngenu)
      DATA genuin/'n','n#','K0l','gamma'/
#include "ftrig.h"
#include "coddat.h"
#include "psfrfu.h"
#include "qmacro.h"
C
      nhn    = 0
      CALL vzero(ehcal(1), 200)
      CALL vzero(vhn(1,1), 4*maxclu)
      CALL vzero(propec(1),200)
      CALL vzero(mobj,maxclu)
C
      qcteca = COS(ecaacc*qqpi/180.)
      qcthca = COS(hcaacc*qqpi/180.)
      qcttpc = COS(tpcacc*qqpi/180.)
C
      hadtot = 0.
      inlkfs = 0
      isatfs = 0
      iflufs = 0 
      isatph = 0

C
C Loop over detected charged particles tracks (good and V0 tracks)
C

      ich = 0
      DO 1 ikpar = 1, 2
        itk = kpdir(prnam(ikpar),kreco)
    2   IF ( itk .EQ. 0 ) GOTO 1
        ich   = ich + 1
        kpsfr = ktn(itk)
        ial   = KPSFAL(kpsfr)
C
C Ignore tracks that do not extrapolate to the ECAL
C
        jtrex = NLINK('TREX',kpsfr)
        IF ( jtrex .LE. 0 ) GOTO 3
        nvett = IW(jtrex+2)
        IF ( nvett .LT. 2 ) GOTO 3
C
C Ignore electrons and muons
C
        ktp = ktpcod(ial)
        IF ( idtflg(ich) .NE. 0 .AND.
     &       (ktp.eq.2 .OR. ktp.eq.3 .OR.
     &        ktp.eq.5 .OR. ktp.eq.6     )  ) THEN
        GOTO 3
        ELSEIF ( idtflg(ich) .NE. 0 ) THEN
C
C FTRIG : rescue interactions of hadrons identified as e/mu
C
          theta = ATAN2 ( SQRT ( RTABL(jtrex,1,1)**2 +
     .                           RTABL(jtrex,1,2)**2 ) ,
     .                           RTABL(jtrex,1,3)       )
          phi   = ATAN2 (        RTABL(jtrex,1,2)      ,
     .                           RTABL(jtrex,1,1)       )
          pmom  = qp(ial)
          vec(1) = pmom * SIN(theta) * COS(phi)
          vec(2) = pmom * SIN(theta) * SIN(phi)
          vec(3) = pmom * COS(theta)
          vec(4) = qe(ial)
          itkn = KVNEW(dummy)
          call QVSET4(itkn,vec)
          inl = 0
          CALL quhneu(itkn,inl,vec,itd,ier)
          IF (itd .ne. 0) goto 3
          IF ( ier .EQ. 0 ) THEN
            call fmodule(theta*QQRADP,phi*QQRADP,M_ECAL,M_HCAL)
            E_ECAL(M_ECAL) = E_ECAL(M_ECAL) + max(pmom,0.)
          ENDIF
          GOTO 3
        ENDIF
C
C Count the track momentum (-) and the shower energy (+)
C

        theta = ATAN2 ( SQRT ( RTABL(jtrex,1,1)**2 +
     .                         RTABL(jtrex,1,2)**2 ) ,
     .                         RTABL(jtrex,1,3)       )
        phi   = ATAN2 (        RTABL(jtrex,1,2)      ,
     .                         RTABL(jtrex,1,1)       )

C
C Probability for no link 
C
       POL = (180./qqpi)*acos(qct(ial))

C
C  For the Barrel region ...
C
       IF (POL .GT. 46 .AND. POL .LT. 134) THEN
       prb = exp(-qe(ial)-.75)+.01-.00045*qe(ial)
C
C  For the Endcaps ...
C
       ELSEIF (POL .LE. 36 .OR. POL .GE. 144) THEN
       prb = exp(-.707-1.*qe(ial))+.010-.00045*qe(ial)
C
C  For the Overlap ...
C
       ELSE
       prb = exp(-.8-.5*qe(ial))+.0090-.00045*qe(ial)
       ENDIF
       IF (rndm(0.).lt.prb*pnlk) THEN
          inlkfs = inlkfs + 1
          inl = 1
          goto 33
       ENDIF
       
        vec(1) = -qp(itk) * SIN(theta) * COS(phi)
        vec(2) = -qp(itk) * SIN(theta) * SIN(phi)
        vec(3) = -qp(itk) * COS(theta)
        vec(4) = -qe(itk)
        nhn = nhn + 1
        CALL ucopy(vec(1),vhn(1,nhn),4)
        mobj(nhn) = ial
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '--------------------------------------------'
          WRITE(6,*) 'Charged particle : ',itk,cqtpn(itk)
          WRITE(6,*) '                   ',vec
        ENDIF

C
C No calorimeter energy if the charged particle decays in the TPC
C
   33   CONTINUE
        iv2 = KENDV(ial)
        IF ( iv2 .GT. 0 ) THEN
          rend = SQRT ( qvx(iv2)**2 + qvy(iv2)**2 )
          zend = ABS  ( qvz(iv2)                  )
          IF ( rend .LT. rdime(7) .AND. zend .LT. zdime(7) ) THEN
          GOTO 3
          ENDIF
        ENDIF
C
        pmom  = qe(ial) 
        CALL ucopy(RW(koqvec+ial*kcqvec+jqveqx),vecsav(1),4)
        RW(koqvec+ial*kcqvec+jqveqx)= pmom * SIN(theta) * COS(phi)
        RW(koqvec+ial*kcqvec+jqveqy)= pmom * SIN(theta) * SIN(phi)
        RW(koqvec+ial*kcqvec+jqveqz)= pmom * COS(theta)
        RW(koqvec+ial*kcqvec+jqveqe)= pmom
        RW(koqvec+ial*kcqvec+jqveqp)= pmom
        CALL quhneu(ial,inl,vec,itd,ier)
        CALL ucopy(vecsav(1),RW(koqvec+ial*kcqvec+jqveqx),4)
        IF (itd .ne. 0) goto 3 
        IF ( ier .NE. 0 ) THEN
        GOTO 3
        ENDIF
        nhn = nhn + 1
        CALL ucopy(vec(1),vhn(1,nhn),4)
        mobj(nhn) = ial
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '--------------------------------------------'
          WRITE(6,*) 'Charged hadron : ',itk,cqtpn(itk)
          WRITE(6,*) '                   ',vec
        ENDIF

C
C Sort ECAL energy fraction
C
       ich = kch(ial)
       CALL quecef(ial,nhn,ich,eef)
C
C Sort satellites
C

        CALL qustls(vec,vha,vphsat,iha,iph,ier)
        IF (iha.ge.1) THEN
           isatfs = isatfs+1
           itksh = KVNEW(DUMMY)
           CALL QVSET4(itksh,vha)
           nhn = nhn+1
           CALL ucopy(vha(1),vhn(1,nhn),4)
           mobj(nhn) = itksh

C Sort photon satellites

        IF (iph.ge.1) THEN
           isatph = isatph+1
           itkph = KVNEW(DUMMY)
           CALL QVSET4(itkph,vphsat)
           nph = nph+1
           mobjp(nph) = itkph
           CALL ucopy(vphsat(1),vph(1,nph),4)
        ENDIF

C Sort ECAL energy fraction

        ich = kch(ial)
        CALL quecef(itksh,nhn,ich,eef)
        ENDIF

C Subtract the satelite energy to the main cluster

        IF (iha.ge.1.and.iph.ge.1) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vhn(4,nhn)-vph(4,nph)
        ELSEIF (iha .ge.1 .and.iph.eq.0) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vhn(4,nhn)
        ELSEIF (iph .ge.1 .and.iha.eq.0) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vph(4,nph)
        ENDIF
C
    3   itk = kfollo(itk)
        GOTO 2
    1 CONTINUE

C
C Now do the same for the un-recontructed hadrons, muons and
C electrons
C

      DO 10 kpsfr = 1, npsfr
C
        itk = KPSFAL(kpsfr)
        IF ( xlock(itk) ) GOTO 10
      IF (( ktpcod(itk) .EQ. 2 .OR. ktpcod(itk) .EQ. 3)
     .      .AND. .NOT.( ABS(COS(theta)) .LT. qctlu1 .AND.
     .     ABS(COS(theta)) .GT. qctlu2) ) GOTO 10
C
C Ignore tracks that begins after the end of the HCAL
C
        IF (
     .  SQRT( QPSFXV(kpsfr)**2+QPSFYV(kpsfr)**2 ) .GT. rdime(13) .OR.
     .  ABS ( QPSFZV(kpsfr)                     ) .GT. zdime(13) )
     .  THEN
        GOTO 10
        ENDIF
C
C  Extrapolate to the ECAL, and to the HCAL
C
        CALL qutrex(kpsfr)
C
C Ignore tracks that do not extrapolate to the ECAL
C
        jtrex = NLINK('TREX',kpsfr)
        IF ( jtrex .LE. 0 ) GOTO 10
        nvett = IW(jtrex+2)
        IF ( nvett .LT. 2 ) GOTO 10
C
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '---------------------------------------------'
          WRITE(6,*) 'Unreconstructed ',cqtpn(itk),' : ',
     .                kpsfr,qp(itk),qct(itk)
          WRITE(6,*) 'Vertex de depart         : ',
     .                QPSFXV(kpsfr), QPSFYV(kpsfr), QPSFZV(kpsfr)
          CALL prtabl('TREX',kpsfr)
        ENDIF
C
C It is in the acceptance of the ECAL
C
        theta = ATAN2 ( SQRT ( RTABL(jtrex,1,1)**2 +
     .                         RTABL(jtrex,1,2)**2 ) ,
     .                         RTABL(jtrex,1,3)       )
        phi   = ATAN2 (        RTABL(jtrex,1,2)      ,
     .                         RTABL(jtrex,1,1)       )

        IF ( ABS(COS(theta)) .GT. qcteca ) GOTO 10
C
C Compute the equivalent of RT (for hadrons) and NEXP (for muons)
C
        IF ( ktpcod(itk) .EQ. 5 .OR. ktpcod(itk) .EQ. 6 ) THEN
C
C Muons only in the HCAL
C

          IF ( ABS(COS(theta)) .GT. qcthca ) GOTO 10
          nfir  = 0
          CALL qunexp(kpsfr,jexp,nenf,igfl,iebm,hmom)
          hmomu = 0.
          DO iplan = 23, 1, -1
            IF ( hmomu .EQ. 0. ) hmomu = hmom(iplan)
            IF ( jbit(iebm,iplan) .EQ. 1 ) THEN
              proba = 0.80 * (1.-EXP(-hmom(iplan)-0.5))
              IF ( RNDM(iplan) .Lt. proba ) nfir = nfir + 1
            ENDIF
          ENDDO
          IF ( nfir .LE. 0 ) GOTO 10
          pmom = 0.230 * FLOAT(nfir)
          vec (1) = pmom * SIN(theta) * COS(phi)
          vec (2) = pmom * SIN(theta) * SIN(phi)
          vec (3) = pmom * COS(theta)
          vec (4) = pmom
          IF ( idbg .GE. 10 ) WRITE(6,*) ' Nfir/vec : ',nfir,vec
C
C FTRIG : store plane penetration
C
          phideg = phi*QQRADP
          if ( RTABL(jtrex,1,1) .lt. 0.) phideg = phideg + 180.
          phideg = mod(phideg+360.,360.)
          thetadeg = theta*QQRADP
          call fmodule(thetadeg,phideg,M_ECAL,M_HCAL)
          T_HCAL(M_HCAL) = max(T_HCAL(M_HCAL),nfir)
C
        ELSE
C
C Hadrons in the ECAL only
C
          IF ( ABS(COS(theta)) .GT. qcthca ) THEN
            CALL rannor(rt,rl)
            rt0 = (0.2-qp(itk))/(0.22*sqrt(qp(itk))) + qp(itk)/5.
            art = .7 + qp(itk)/5.
            rtr = rt0 - ALOG(RNDM(rt0))*art + rt/2.
            pmom = qp(itk) + 0.22*sqrt(qp(itk))*rtr
C
C FTRIG : fraction of energy in ECAL
C
            call fmodule(theta*QQRADP,phi*QQRADP,M_ECAL,M_HCAL)
            E_ECAL(M_ECAL) = E_ECAL(M_ECAL) + max(pmom,0.) 
            IF ( pmom .LE. 0.300 ) GOTO 10
            vec (1) = pmom * SIN(theta) * COS(phi)
            vec (2) = pmom * SIN(theta) * SIN(phi)
            vec (3) = pmom * COS(theta)
            vec (4) = pmom
            IF ( idbg .GE. 10 ) WRITE(6,*) ' RT/vec : ',rtr,vec
          ELSE
C
C Hadrons/Electrons in the acceptance of the HCAL
C
            pmom  = qe(itk)
            CALL ucopy(RW(koqvec+ial*kcqvec+jqveqx),vecsav(1),4)
            RW(koqvec+itk*kcqvec+jqveqx)= pmom*SIN(theta)*COS(phi)
            RW(koqvec+itk*kcqvec+jqveqy)= pmom*SIN(theta)*SIN(phi)
            RW(koqvec+itk*kcqvec+jqveqz)= pmom*COS(theta)
            RW(koqvec+itk*kcqvec+jqveqe)= pmom
            RW(koqvec+itk*kcqvec+jqveqp)= pmom
            inl = 0
            CALL quhneu(itk,inl,vec,itd,ier)
            CALL ucopy(vecsav(1),RW(koqvec+ial*kcqvec+jqveqx),4)
            IF (itd .ne. 0) goto 10
            IF ( ier .NE. 0 ) GOTO 10
            IF ( idbg .GE. 10 ) WRITE(6,*) 'vec : ',vec
          ENDIF
        ENDIF

C
C Store the neutral hadron
C

        nhn = nhn + 1
        CALL ucopy(vec(1),vhn(1,nhn),4)
        mobj(nhn) = itk
C
C Sort ECAL energy fraction
C 
       ich = kch(itk)
       CALL quecef(itk,nhn,ich,eef)
C
C
C Sort satelites
C

        CALL qustls(vec,vha,vphsat,iha,iph,ier)
        IF (iha.ge.1) THEN
           isatfs = isatfs + 1
           itksh = KVNEW(DUMMY)
           CALL QVSET4(itksh,vha)
           nhn = nhn+1
           CALL ucopy(vha(1),vhn(1,nhn),4)
           mobj(nhn) = itksh

C Sort photon satellites

        IF (iph.ge.1) THEN
           isatph = isatph + 1
           itkph = KVNEW(DUMMY)
           CALL QVSET4(itkph,vphsat)
           nph = nph+1
           mobjp(nph) = itkph
           CALL ucopy(vphsat(1),vph(1,nph),4)
        ENDIF

C Sort ECAL energy fraction

        CALL quecef(itksh,nhn,0,eef)
        ENDIF
        IF (iha.ge.1.and.iph.ge.1) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vhn(4,nhn)-vph(4,nph)
        ELSEIF (iha .ge.1 .and.iph.eq.0) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vhn(4,nhn)
        ELSEIF (iph .ge.1 .and.iha.eq.0) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vph(4,nph)
        ENDIF

   10 CONTINUE

      DO 20 igenu = 1, ngenu
        itk = KPDIR(genuin(igenu),kmonte)
   21   IF ( itk .EQ. 0 ) GOTO 20
        IF ( kstabc(itk).LT. 1 ) GOTO 22
        IF ( xlock(itk) ) GOTO 22
C
C Extrapolate to the ECAL
C

        ivr = KORIV(itk)
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '----------------------------'
          WRITE(6,*) 'Neutral hadron E/costh : ',
     .               itk,cqtpn(itk),qe(itk),qct(itk)
          WRITE(6,*) 'Vertex de depart         : ',
     .                QVX(ivr), QVY(ivr), QVZ(ivr)
        ENDIF
C
        par(1) = qvx(ivr)
        par(2) = qvy(ivr)
        par(3) = qvz(ivr)
        par(4) = qx(itk)/qp(itk)
        par(5) = qy(itk)/qp(itk)
        par(6) = qz(itk)/qp(itk)
        IF ( ABS(par(4)).LE.1E-6 .AND. ABS(par(5)).LE.1E-6 ) GOTO 22
        IF ( ABS(par(4)).LE.1E-6 ) par(4) = 1E-6
        IF ( ABS(par(5)).LE.1E-6 ) par(5) = 1E-6
        CALL aulcyl (rdime(8), zdime(8), par, parex, icode)
        IF ( icode .EQ. 0 ) GOTO 22
C
        theta = ATAN2 ( SQRT ( parex(1)**2 +
     .                         parex(2)**2 ) ,
     .                         parex(3)       )
        phi   = ATAN2 (        parex(2)      ,
     .                         parex(1)       )
C
        IF ( ABS(COS(theta)) .GT. qcteca ) GOTO 22
C
C Neutral hadrons in the ECAL only
C
        IF ( ABS(COS(theta)) .GT. qcthca ) THEN
C
          CALL rannor(rt,rl)
          rt0 = (0.2-qp(itk))/(0.22*sqrt(qp(itk))) + qp(itk)/5.
          art = .7 + qp(itk)/5.
          rtr = rt0 - ALOG(RNDM(rt0))*art + rt/2.
          pmom = qp(itk) + 0.22*sqrt(qp(itk))*rtr
C
C FTRIG : fraction of energy in ECAL
C
          call fmodule(theta*QQRADP,phi*QQRADP,
     &                 M_ECAL,M_HCAL)
          E_ECAL(M_ECAL) = E_ECAL(M_ECAL) + max(pmom,0.)

          IF ( pmom .LE. 0.300 ) GOTO 22
          vec (1) = pmom * SIN(theta) * COS(phi)
          vec (2) = pmom * SIN(theta) * SIN(phi)
          vec (3) = pmom * COS(theta)
          vec (4) = pmom
          IF ( idbg .GE. 10 ) WRITE(6,*) 'RT/vec : ',rtr,vec
C
        ELSE
C
C Neutral hadrons in the acceptance of the HCAL
C
          pmom  = qe(itk)
          CALL ucopy(RW(koqvec+ial*kcqvec+jqveqx),vecsav(1),4)
          RW(koqvec+itk*kcqvec+jqveqx)= pmom*SIN(theta)*COS(phi)
          RW(koqvec+itk*kcqvec+jqveqy)= pmom*SIN(theta)*SIN(phi)
          RW(koqvec+itk*kcqvec+jqveqz)= pmom*COS(theta)
          RW(koqvec+itk*kcqvec+jqveqe)= pmom
          RW(koqvec+itk*kcqvec+jqveqp)= pmom
          inl = 0
          CALL quhneu(itk,inl,vec,itd,ier)
          CALL ucopy(vecsav(1),RW(koqvec+ial*kcqvec+jqveqx),4)
          IF (itd .ne. 0) goto 22
          IF ( ier .NE. 0 ) GOTO 22
          IF ( idbg .GE. 10 ) WRITE(6,*) 'Vec : ',vec

C
C FTRIG : penetration in HCAL
C
C Hadron penetration   as in QUIDEM
C
C original PJ stuff
C Hadron penetration
c        CALL rannor(gauss,dudu)
c        anhd = -3.8 + 2./qp(itk)
c        nhad  = NINT(anhd*ALOG(RNDM(nhad))-1.
c     .        + (1.2+dudu)*2.0*(1.-exp(-qp(itk)/15.)))

          call hadint(ial,start,xlen)
          nhad = min((start+xlen)/LambdaHCAL*24.,23.)
C... here we would like to introduce geometrical cracks as in QUIDEM
          call fmodule(theta*QQRADP,phi*QQRADP,M_ECAL,M_HCAL)
          T_HCAL(M_HCAL) = max(T_HCAL(M_HCAL),nhad)
          
          
        ENDIF
C
C Store the neutral hadron
C

        nhn = nhn + 1
        CALL ucopy(vec(1),vhn(1,nhn),4)
        mobj(nhn) = itk

C
C Sort ECAL energy fraction
C      

       ich = kch(itk)
       CALL quecef(itk,nhn,ich,eef)
C
C
C Sort satelites
C
C
        CALL qustls(vec,vha,vphsat,iha,iph,ier)
        IF (iha.ge.1) THEN
           isatfs =isatfs +1
           itksh = KVNEW(DUMMY)
           CALL QVSET4(itksh,vha)
           nhn = nhn+1
           CALL ucopy(vha(1),vhn(1,nhn),4)
           mobj(nhn) = itksh
C Sort photon satellites
        IF (iph.ge.1) THEN
           isatph = isatph + 1
           itkph = KVNEW(DUMMY)
           CALL QVSET4(itkph,vphsat)
           nph = nph+1
           mobjp(nph) = itkph
           CALL ucopy(vphsat(1),vph(1,nph),4)
        ENDIF
C Sort ECAL energy fraction
        CALL quecef(itksh,nhn,0,eef)
        ENDIF
C Subtract the satelite energy to the main cluster
        IF (iha.ge.1.and.iph.ge.1) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vhn(4,nhn)-vph(4,nph)
        ELSEIF (iha .ge.1 .and.iph.eq.0) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vhn(4,nhn)
        ELSEIF (iph .ge.1 .and.iha.eq.0) THEN
           vhn(4,nhn-1) = vhn(4,nhn-1)-vph(4,nph)
        ENDIF

   22   itk = KFOLLO(itk)
        GOTO 21
   20 CONTINUE

  999 RETURN
      END
