      SUBROUTINE quidem
C-----------------------------------------------------------------------
C! Perform electron and muon identification
C
C  Patrick Janot -- 03 Nov 1993
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "parcut.h"
#include "hadpho.h"
#include "psfrjj.h"
#include "trexjj.h"
      REAL helix(6),pmom(3),xve1(3),xve2(3),xc(2)
      REAL ecal1(3),ecal2(3),hmom(23)
#include "ftrig.h"
#include "coddat.h"
#include "qmacro.h"
#include "psfrfu.h"
C
      CALL vzero(execal(1,1),3*maxclu)
      CALL vzero(exhcal(1,1),3*maxclu)
C
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
      CALL vzero(iandx(1),maxclu)
C
      qcthca = COS(hcaacc*qqpi/180.)
C
C Loop over detected charged particles tracks (good and V0 tracks)
C
      nch = 0
      DO 1 ikpar = 1, 2
        itk = kpdir(prnam(ikpar),kreco)
    2   IF ( itk .EQ. 0 ) GOTO 1
        nch = nch + 1
        kpsfr = ktn(itk)
        dzero(nch) = QPSFD0(kpsfr)
        iandx(nch) = itk
        ial        = KPSFAL(kpsfr)
C
C Build the bank TREX for each charged particle track
C
        CALL qutrex(kpsfr)
C
C Make the lepton id only if p > 1 GeV/c
C
        IF ( qp(itk) .LT. 1.0 ) GOTO 3
C
        jtrex = NLINK('TREX',kpsfr)
        IF ( jtrex .LE. 0 ) GOTO 3
        nvett = IW(jtrex+2)
        IF ( nvett .LT. 2 ) GOTO 3
        ifail = RTABL(jtrex,1,7)
        IF ( idbg .GE. 15 ) THEN
          WRITE(6,*) '--------------------------------'
          WRITE(6,*) 'Track # ',kpsfr,qp(itk),ktpcod(ial)
          WRITE(6,*)  'Ifail = ',ifail
          CALL prtabl('TREX',kpsfr)
        ENDIF
C
        execal(nch,1) = RTABL(jtrex,1,1)
        execal(nch,2) = RTABL(jtrex,1,2)
        execal(nch,3) = RTABL(jtrex,1,3)
C
C Make the electron Id
C
        idip   = 0
        icrak  = 0
C
        ecal1(1) = execal(nch,1)
        ecal1(2) = execal(nch,2)
        ecal1(3) = execal(nch,3)
        ecal2(1) = RTABL(jtrex,2,1)
        ecal2(2) = RTABL(jtrex,2,2)
        ecal2(3) = RTABL(jtrex,2,3)
        pol1   = ATAN2(SQRT(ecal1(1)**2+ecal1(2)**2),ecal1(3))*180./pi
        azi1   = ATAN2(ecal1(2),ecal1(1))                     *180./pi
        pol2   = ATAN2(SQRT(ecal2(1)**2+ecal2(2)**2),ecal1(3))*180./pi
        azi2   = ATAN2(ecal2(2),ecal2(1))                     *180./pi
        worse  = ( flufac( pol1         , azi1         )
     .           + flufac((pol1+pol2)/2.,(azi1+azi2)/2.)
     .           + flufac( pol2         , azi2         ) ) / 3.
        IF ( pol1 .LT. ecaacc ) GOTO 5
        IF ( pol2 .LT. ecaacc ) GOTO 5
        IF ( worse .GT. 1.01 ) icrak = 1
C
        CALL rannor(rt,rl)
C For the electrons
        IF ( ktpcod(ial) .EQ. 2 .OR. ktpcod(ial) .EQ. 3 ) THEN
          elrtr (nch) = rt*worse - (worse-1.)*RNDM(worse)
          elrlg (nch) = rl*worse - (worse-1.)*RNDM(worse)
C
C FTRIG : energy deposit in ECAL
C
          call fmodule(pol1,azi1,M_ECAL,M_HCAL)
C could nor find the correspondance between Rt and E... so simply
c          E_ECAL(M_ECAL) = E_ECAL(M_ECAL) + (1.+rt)*
          call rannor(aa,bb)
          E_ECAL(M_ECAL) = E_ECAL(M_ECAL) +
     &      (1.+aa*worse*0.18/sqrt(qe(ial)))*qe(ial)

C For the other charged particles
        ELSE
          rt0 = (0.2-qp(ial))/(0.22*sqrt(qp(ial))) + qp(ial)/5.
          rl0 = -4.0+2.*exp(-qp(ial)/2)
          art = .7 + qp(ial)/5.
          arl = 1.6
          elrtr (nch) = rt0 - ALOG(RNDM(rt0))*art + rt/2.
          elrlg (nch) = rl0 - ALOG(RNDM(rl0))*arl + rl/2.
          IF ( RNDM(rl0) .LT. worse*(0.37+EXP(-(1.+qp(ial))/1.6)) )
     .    elrlg (nch) = 1000.
        ENDIF
C No associated ECAL object
        IF ( qp(ial) + 0.22*sqrt(qp(ial))*elrtr(nch) .LT. .300 ) THEN
          elrtr(nch) = 0.
          elrlg(nch) = 0.
          leflg(nch) = 0
          GOTO 5
        ENDIF
C Crack
        IF ( icrak .EQ. 1 .AND. elrlg(nch) .GE. 999. ) THEN
          IF ( RNDM(icrak) .LE. 0.07 + exp(-qp(ial)/1.8) ) THEN
            elrtr(nch) = 1000.
          ELSE
            elrtr(nch) = 0.
            elrlg(nch) = 0.
            leflg(nch) = 0
            GOTO 5
          ENDIF
        ENDIF
C IDIP flag
        IF ( elrtr(nch) .GT. -3.0 .AND.
     .       elrlg(nch) .GT. -3.0 .AND.
     .       elrlg(nch) .LT.  3.0 ) idip = 1
C LEFLG flag
        leflg(nch) = 1 + idip + 3*icrak
        IF ( idip .EQ. 1 ) THEN
          idtflg(nch) = 1
        ELSEIF ( ABS(elrlg(nch)) .LT. 3. .AND. qp(ial) .GT. 30. ) THEN
          idtflg(nch) = 2
        ENDIF
C
C Make the Muon Id...
C
    5   CONTINUE
C
        icb = 0
        DO 10 ipl = 1, LROWS(jtrex)
          Irgm1 = ITABL(jtrex,ipl,JTRERG)
          IF ( irgm1 .LE. 2500 ) GO TO 10
          icb = icb + 1
C
          IF ( icb .EQ. 1 ) THEN
            exhcal(nch,1) = RTABL(jtrex,ipl,1)
            exhcal(nch,2) = RTABL(jtrex,ipl,2)
            exhcal(nch,3) = RTABL(jtrex,ipl,3)
            GOTO 11
          ENDIF
C
   10    CONTINUE
C
   11   CALL qunexp(kpsfr,jexp,nenf,igfl,iebm,hmom)
C       nrld  = jexp - 10
C       nrlt  = jexp - 3
C       nexp  = jexp - nenf
        nexp  = 0
        nls3  = 0
        nl10  = 0
        nfir  = 0
        n10   = 0
        n03   = 0
        mult  = 0
        im1   = 0
        im2   = 0
        ishad = 0
        idf   = 0
        hmomu = 0.
C original PJ stuff
C Hadron penetration
c        CALL rannor(gauss,dudu)
c        anhd = -3.8 + 2./qp(ial)
c        nhad  = NINT(anhd*ALOG(RNDM(nhad))-1.
c     .        + (1.2+dudu)*2.0*(1.-exp(-qp(ial)/15.)))

        call hadint(ial,start,xlen)
        nhad = min((start+xlen)/LambdaHCAL*24.,23.)

C In the HCAL, assume a 80% plane efficiency for energetic muons
        DO iplan = 23, 1, -1
          IF ( hmomu .EQ. 0. ) hmomu = hmom(iplan)
          IF ( jbit(iebm,iplan) .EQ. 1 ) THEN
            nexp = nexp + 1
C For the muons (Reminder : ionisation goes like momentum)
            IF ( ktpcod(ial) .EQ. 5 .OR. ktpcod(ial) .EQ. 6 ) THEN
              proba = 0.80 * (1.-EXP(-hmom(iplan)-0.5))
              IF ( idbg .GE. 15 ) WRITE(6,*) iplan,hmom(iplan),proba
              IF ( RNDM(iplan) .Lt. proba ) THEN
                IF ( nexp .LE.  3 ) n03 = n03 + 1
                IF ( nexp .LE. 10 ) n10 = n10 + 1
                nfir = nfir + 1
              ENDIF
C For the charged hadrons
            ELSEIF ( ktpcod(ial).NE.2 .AND. ktpcod(ial).NE.3 ) THEN
              IF ( iplan .LE. nhad ) THEN
                IF ( nexp .LE.  3 ) n03 = n03 + 1
                IF ( nexp .LE. 10 ) n10 = n10 + 1
                nfir = nfir + 1
              ENDIF
            ELSE
            ENDIF
          ELSE
C for hadrons, forget about cracks
            IF ( ktpcod(ial).NE.2 .AND. ktpcod(ial).NE.3 .AND.
     &           ktpcod(ial).NE.5 .AND. ktpcod(ial).NE.6 ) THEN
              if ( start.eq.0.                !  have interacted in ECAL
     &             .OR. QE(ial).gt.5. ) then  !  energetic enough
                if (iplan .LE. nhad ) nfir = nfir + 1
              endif
            ENDIF
          ENDIF
        ENDDO
C
C FTRIG : store plane penetration
C
        IF ( ifail .EQ. 1 ) THEN
          if ( abs(exhcal(nch,1)) .lt. 1e-6 ) then
            phi = sign(90.,exhcal(nch,2)*exhcal(nch,1))
          else
            phi = atan(exhcal(nch,2)/exhcal(nch,1))*QQRADP
          endif
          if (exhcal(nch,1) .lt. 0.) phi = phi + 180.
          phi = mod(phi+360.,360.)
          theta = acos(exhcal(nch,3)/
     &     sqrt(exhcal(nch,1)**2+exhcal(nch,2)**2+exhcal(nch,3)**2))
     &     *QQRADP
          call fmodule(theta,phi,M_ECAL,M_HCAL)
          T_HCAL(M_HCAL) = max(T_HCAL(M_HCAL),nfir)
        ENDIF


C In the MUCH, assume a 88% plane efficiency over 97% of the space
C for energetic muons
        IF ( ifail .EQ. 1 ) THEN
          kvett = KROW(jtrex,nvett)
          xmu   = RW(kvett+1)
          ymu   = RW(kvett+2)
          zmu   = RW(kvett+3)
          theta = ATAN2(SQRT(xmu**2+ymu**2),ABS(zmu)) * 180. / qqpi
          IF ( theta .GT. hcaacc ) THEN
C For the muons
            probmu = 1.-EXP(-hmomu-.5)
            IF ( ktpcod(ial) .EQ. 5 .OR. ktpcod(ial) .EQ. 6 ) THEN
              IF ( RNDM(ial) .LE. 0.97*probmu ) THEN
                im1 = NINT(4.*RNDM(im1))
                im2 = NINT(4.*RNDM(im2))
              ENDIF
C For the charged hadrons
            ELSEIF ( ktpcod(ial).NE.2 .AND. ktpcod(ial).NE.3 ) THEN
              IF ( nhad .GE. 21 .AND. RNDM(ial) .LE. 0.97*probmu ) THEN
                im1 = NINT(4.*RNDM(im1))
                im2 = NINT(4.*RNDM(im2))
              ELSE
                im1 = -0.25*ALOG(RNDM(im1))
                im2 = -0.25*ALOG(RNDM(im2))
                im1 = im1*(1.+1.10*RNDM(im1))
                im2 = im2*(1.+1.10*RNDM(im2))
              ENDIF
            ELSE
            ENDIF
          ENDIF
        ENDIF
C Now the multiplicity in the last ten plane
        IF ( ktpcod(ial) .EQ. 5 .OR. ktpcod(ial) .EQ. 6 ) THEN
          IF ( n10 .GT. 0 ) mult = 100. - 9.*ALOG(RNDM(mult))
        ELSEIF ( ktpcod(ial).NE.2 .AND. ktpcod(ial).NE.3 ) THEN
          mult = 120
          IF ( n10 .GT. 0. ) THEN
C           mult = NINT(100.*(3.-2.*SQRT(RNDM(mult))))
            mult = mult + 130.*RNDM(mult)
          ENDIF
        ELSE
        ENDIF
C
    6   CALL mmuidf(nexp,nfir,im1,im2,n10,n03,mult,ishad,idf)
C
        IF ( ktpcod(ial) .NE. 5 .AND. ktpcod(ial) .NE. 6 ) THEN
          IF ( idf .EQ. 12 .OR. idf .EQ. 15 ) THEN
            IF ( RNDM(idf) .LE. 0.6 ) idf = 10
          ENDIF
C         idf = -idf
        ENDIF
C
        IF ( idbg .GE. 15 ) THEN
          WRITE(6,1000) jexp,nenf,nexp,(JBIT(iebm,i),i=1,23)
          WRITE(6,*) 'fired/10/03/m1/2 : ',nfir,n10,n03,im1,im2
          WRITE(6,*) 'mult/idf         : ',mult,idf
        ENDIF
        muflg(nch) = idf + 20 * nexp
        muwrd(nch) = im1+im2 + 10*n03 + 100*n10 + 2000*nfir
C -- Tight identification : QMUIDO flag
        IF ( idf .GT. 0 .AND. nexp .GE. 10 ) GOTO 4
        IF ( idf .GT. 0 .AND. im1+im2 .GE. 1 ) GOTO 4
C -- Loose identification for low momentum particles
        IF ( qp(itk) .GT. 10. ) GOTO 3
        IF ( nexp .GE. 15 .AND. n03 .GE. 1 .AND.
     .       FLOAT(nfir)/FLOAT(nexp) .GE. 0.4 ) GOTO 4
        IF ( qp(itk) .LE. 5. .AND. nfir .GE. 8 .AND.
     .     ( n10 .GE. 3 .OR. n03 .GE. 1 ) ) GOTO 4
C -- Not a muon
        GOTO 3
    4   CONTINUE
        IF ( (im1+im2.GE.1 .OR. nexp.GE.15) .AND. idf .GT. 0 ) THEN
          idtflg(nch) = 3
        ELSE
          idtflg(nch) = 4
        ENDIF
C
    3   itk = kfollo(itk)
        GOTO 2
    1 CONTINUE
C
  999 RETURN
 1000 FORMAT(' Jexp/Nenf/Nexp/Bit Map : ',3I3,1X,23I1)
      END
