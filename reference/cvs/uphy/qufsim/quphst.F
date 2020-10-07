      SUBROUTINE quphst
C-----------------------------------------------------------------------
C! Store the photons
C
C  Patrick Janot -- 17 Nov 1993
C
C  Modification: M. Kado 13 May 1998 Transfer photon clustering to 
C                                    qufsim.F, after photon satellites 
C                                    are generated in qucaob.F
C
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "parcut.h"
#include "fsimid.h"
#include "psfrjj.h"
#include "trexjj.h"
#include "hadpho.h"
      DIMENSION par(6), parex(6), vecsav(4)

#include "ftrig.h"
#include "coddat.h"
#include "psfrfu.h"
#include "qmacro.h"
C
      nph    = 0
      CALL vzero(vph(1,1), 4*maxclu)
      CALL vzero(mobjp,maxclu)
C
      qcteca = COS(ecaacc*qqpi/180.)
      qctlu1 = COS(polcor(3)*qqpi/180.)
      qctlu2 = COS(polcor(4)*qqpi/180.)
C
C Loop over the photons
C
      itk = KPDIR('gamma',kmonte)
    2 IF ( itk .EQ. 0 ) GOTO 1
C
      IF ( kstabc(itk) .LT. 1 ) GOTO 3
      ivr = KORIV(itk)
C
      IF ( idbg .GE. 10 ) THEN
        WRITE(6,*) '----------------------------'
        WRITE(6,*) 'Photon E/costh : ',itk,qe(itk),qct(itk)
        WRITE(6,*) 'Vertex de depart         : ',
     .              QVX(ivr), QVY(ivr), QVZ(ivr)
      ENDIF
C
C It is in the acceptance of the ECAL
C
      par(1) = qvx(ivr)
      par(2) = qvy(ivr)
      par(3) = qvz(ivr)
C.. ld add protection
      par(4) = sign(max(abs(qx(itk)),1.e-9),qx(itk))/qp(itk)
      par(5) = sign(max(abs(qy(itk)),1.e-9),qy(itk))/qp(itk)
      par(6) = sign(max(abs(qz(itk)),1.e-9),qz(itk))/qp(itk)
      IF ( ABS(par(4)) .LE. 1E-6 .AND. ABS(par(5)) .LE. 1E-6 ) GOTO 3
      IF ( ABS(par(4)) .LE. 1E-6 ) par(4) = 1E-6
      IF ( ABS(par(5)) .LE. 1E-6 ) par(5) = 1E-6
      CALL aulcyl (rdime(8), zdime(8), par, parex, icode)
      IF ( icode .EQ. 0 ) GOTO 3
C
      pmom  = qe(itk)
      theta = ATAN2 ( SQRT ( parex(1)**2 +
     .                       parex(2)**2 ) ,
     .                       parex(3)       )
      phi   = ATAN2 (        parex(2)      ,
     .                       parex(1)       )
C
C Accpetance of lumi
C
      IF ( ABS(COS(theta)) .GT. qcteca ) GOTO 3
C
C Hole lumi/endcap
C
      IF ( ABS(COS(theta)) .LT. qctlu1 .AND.
     .     ABS(COS(theta)) .GT. qctlu2 ) GOTO 3
C
      CALL ucopy(RW(koqvec+ial*kcqvec+jqveqx),vecsav(1),4)
      RW(koqvec+itk*kcqvec+jqveqx)= pmom * SIN(theta) * COS(phi)
      RW(koqvec+itk*kcqvec+jqveqy)= pmom * SIN(theta) * SIN(phi)
      RW(koqvec+itk*kcqvec+jqveqz)= pmom * COS(theta)
      RW(koqvec+itk*kcqvec+jqveqe)= pmom
      RW(koqvec+itk*kcqvec+jqveqp)= pmom
      IF ( idbg .GE. 10 )
     .WRITE(6,*) 'After extrapolation      : ',qp(itk),qct(itk)
C
C
C Simulate the energy measurement in the ECAL
C
      CALL quphot(itk,vec,ier)
      CALL ucopy(vecsav(1),RW(koqvec+ial*kcqvec+jqveqx),4)
      IF ( ier .NE. 0 ) GOTO 3
      IF ( idbg .GE. 10 ) WRITE(6,*) 'After smearing : ',vec(4),ier
C
C FTRIG : add photon energy deposit
C
      call fmodule(acos(qct(itk))*QQRADP,qph(itk)*QQRADP,M_ECAL,M_HCAL)
      E_ECAL(M_ECAL) = E_ECAL(M_ECAL) + vec(4)
C
C Check that it's not too close from a reconstructed track
C except if this track is identified as an electron.
C
      DO 4 ich = 1, nch
        dist = SQRT ( (parex(1)-execal(ich,1))**2
     .              + (parex(2)-execal(ich,2))**2
     .              + (parex(3)-execal(ich,3))**2 )
        IF ( dist .LE. 2. ) THEN
          ial = iandx(ich)
          IF ( idbg .GE. 10 ) THEN
            WRITE(6,*) 'Too close from track ',ial,idtflg(ich),qp(ial)
            WRITE(6,*) (parex(i),i=1,3)
            WRITE(6,*) (execal(ich,i),i=1,3)
            WRITE(6,*) 'Distance = ',dist,' cm'
          ENDIF
          IF ( idtflg(ich) .EQ. 1 .OR. idtflg(ich) .EQ. 2 ) THEN
            sigmapt = flutpc*qpt(ial)**2
            sigmaec = fluche*SQRT(qp(ial)) + flucon*qp(ial)
            sigmatt = SQRT(sigmapt**2+sigmaec**2)
            IF ( vec(4) .GE. (3.-elrtr(ich))*sigmatt ) THEN
              IF ( idbg .GE. 10 )
     .              WRITE(6,*) ' But it is recovered because ',
     .              vec(4),' > ',(3.-elrtr(ich))*sigmatt
              GOTO 4
            ENDIF
          ENDIF
          GOTO 3
        ENDIF
    4 CONTINUE
C
C Store the photon
C
      CALL qltrk(itk)
      nph = nph + 1
      mobjp(nph) = itk
      CALL ucopy(vec(1),vph(1,nph),4)
C
    3 itk = KFOLLO(itk)
      GOTO 2
    1 CONTINUE
C
C Now do the same for the un-recontructed electrons
C
      DO 10 kpsfr = 1, npsfr
C
        itk = KPSFAL(kpsfr)
        IF ( xlock(itk) ) GOTO 10
C
        IF ( ktpcod(itk) .NE. 2 .AND. ktpcod(itk) .NE. 3 ) GOTO 10
C
C Ignore tracks that begins after the second stack of the ECAL
C
        IF (
     .  SQRT( QPSFXV(kpsfr)**2+QPSFYV(kpsfr)**2 ) .GT. rdime(10) .OR.
     .  ABS ( QPSFZV(kpsfr)                     ) .GT. zdime(10) )
     .  GOTO 10
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
        ifail = RTABL(jtrex,1,7)
C
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '---------------------------------------------'
          WRITE(6,*) 'Unreconstructed electron : ',
     .                kpsfr,qp(itk),qct(itk)
          WRITE(6,*) 'Vertex de depart         : ',
     .                QPSFXV(kpsfr), QPSFYV(kpsfr), QPSFZV(kpsfr)
          CALL prtabl('TREX',kpsfr)
        ENDIF
C
        pmom  = qe(itk)
        theta = ATAN2 ( SQRT ( RTABL(jtrex,1,1)**2 +
     .                         RTABL(jtrex,1,2)**2 ) ,
     .                         RTABL(jtrex,1,3)       )
        phi   = ATAN2 (        RTABL(jtrex,1,2)      ,
     .                         RTABL(jtrex,1,1)       )
C
C It is in the acceptance of the ECAL
C
        IF ( ABS(COS(theta)) .GT. qcteca ) GOTO 10
C
C Hole lumi/endcap
C
        IF ( ABS(COS(theta)) .LT. qctlu1 .AND.
     .       ABS(COS(theta)) .GT. qctlu2 ) GOTO 10
C
        CALL ucopy(RW(koqvec+ial*kcqvec+jqveqx),vecsav(1),4)
        RW(koqvec+itk*kcqvec+jqveqx)= pmom * SIN(theta) * COS(phi)
        RW(koqvec+itk*kcqvec+jqveqy)= pmom * SIN(theta) * SIN(phi)
        RW(koqvec+itk*kcqvec+jqveqz)= pmom * COS(theta)
        RW(koqvec+itk*kcqvec+jqveqe)= pmom
        RW(koqvec+itk*kcqvec+jqveqp)= pmom
        IF ( idbg .GE. 10 )
     .  WRITE(6,*) 'After extrapolation      : ',qp(itk),qct(itk)
C
C Simulate the energy measurement in the ECAL
C
        CALL quphot(itk,vec,ier)
        CALL ucopy(vecsav(1),RW(koqvec+ial*kcqvec+jqveqx),4)
        IF ( ier .NE. 0 ) GOTO 10
        IF ( idbg .GE. 10 )
     .  WRITE(6,*) 'After smearing           : ',vec(4),qct(itk)
C
C FTRIG : add photon energy deposit
C
        call fmodule(acos(qct(itk))*QQRADP,qph(itk)*QQRADP,
     &               M_ECAL,M_HCAL)
        E_ECAL(M_ECAL) = E_ECAL(M_ECAL) + vec(4)
C
C Store the photon
C
        CALL qltrk(itk)
        nph = nph + 1
        mobjp(nph) = itk
        CALL ucopy(vec(1),vph(1,nph),4)
C
   10 CONTINUE
C
  999 RETURN
      END
