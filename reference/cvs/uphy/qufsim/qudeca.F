      SUBROUTINE QUDECA
C-----------------------------------------------------------------------
C! Decay of unstable particles: LUND procedure is followed
C  Input      :  none
C  Output     :  none
C
C  G.Ganis   21 Mar 1991
C
C  Modifications
C  o Patrick Janot -- 21 Oct 1993 : Interface to JETSET7.3
C  o Patrick Janot -- 01 Nov 1993 : CALL this routine only once
C  o Marumi  Kado  -- 13 May 1998 : Correction in the MC particles loop
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "parcut.h"
#include "resolu.h"
#include "lundcom.h"
      PARAMETER (maxver=50)
      INTEGER ivert(maxver), mvert(maxver)
      REAL xyz(3),vect(3),xx(3),cc(3),pin(7),pout(6)
      DIMENSION k7vr(ljnpar), k7en(ljnpar)
#include "coddat.h"
#include "qmacro.h"
C
      j1(j) = koqvec + isave * kcqvec + j
      jv(i,j) = koqvrt + ivert(i) * kcqvrt + j
C
C  Loop over MC particles
C
      DO 1 itk = kfmct, klmct
C
C  Keep only those that have not decayed
C
        IF ( kstabc(itk) .LT. 1 ) GOTO 1
C
C  Do not decay charged pions/kaons if p < 1 GeV/c, since no
C  identification is performed in that case.
C  (DWC change: K+ can generate pi0, so only pions are ignored)
C
        IF ( ((ktpcod(itk).EQ.8).OR.(KTPCOD(ITK).EQ.9))
     .                    .AND. qp(itk) .LE. 1. ) GOTO 1
C
C  Decay if it is unstable
C
        IF ( .NOT.KINS(ktpcod(itk)) .AND. .NOT.KAPI(ktpcod(itk)) .AND.
     .           ASTAB(ktpcod(itk)) ) GOTO 1

C
C  This shouldn't happen :
C
        IF ( .NOT.ASTAB(ktpcod(itk)) .AND.
     .       .NOT.KINS( ktpcod(itk)) .AND.
     .       .NOT.KAPI( ktpcod(itk)) .AND.
     .                  ktpcod(itk)  .LE. 360  ) THEN
          WRITE(6,*) '-------------------------------------------------'
          WRITE(6,*) '*** QUDECA *** Stable Particle not in ',
     .               'the Official QUFSIM List !!! :'
          WRITE(6,*) itk,ktpcod(itk),cqtpn(itk)
          WRITE(6,*) '-------------------------------------------------'
          GOTO 1
        ENDIF
C
        n7lu = 0
        CALL vzero(ivert(1),maxver)
        CALL vzero(mvert(1),maxver)
        CALL vzero(k7vr (1),ljnpar)
        CALL vzero(k7en (1),ljnpar)
C
C -- First, find out the nature of the particle and fill LUJETS
C -- correspondingly ...
C
        DO l1 = 1, nstab
          IF ( ktpcod(itk). EQ. kcodes(l1) ) THEN
            n7lu= 1
            k7lu(n7lu,1) = 1
            k7lu(n7lu,2) = kcolu(l1)
            p7lu(n7lu,1) = qx(itk)
            p7lu(n7lu,2) = qy(itk)
            p7lu(n7lu,3) = qz(itk)
            p7lu(n7lu,4) = qe(itk)
            p7lu(n7lu,5) = qm(itk)
            v7lu(n7lu,1) = qvx(koriv(itk)) * 10.
            v7lu(n7lu,2) = qvy(koriv(itk)) * 10.
            v7lu(n7lu,3) = qvz(koriv(itk)) * 10.
            v7lu(n7lu,4) = 0.
            v7lu(n7lu,5) = 0.
            GOTO 10
          ENDIF
        ENDDO
10      CONTINUE
        IF ( n7lu .EQ. 0 ) GOTO 1
C
C -- Execute the decay
C
        CALL luexec
        IF ( idbg .GE. 10 ) THEN
          WRITE(6,*) '---------------------------------------------'
          WRITE(6,*) '+++ QUDECA +++ An unstable particle has been ',
     .               'decayed : '
          CALL lulist(1)
        ENDIF
C
C -- Add new vertices to the ALPHA record
C
        jorig = 0
        jvert = 1
        ivert(jvert) = KORIV(itk)
        k7vr (1)     = jvert
C
        DO 5 i7lu = 1, n7lu
C
          IF ( k7lu(i7lu,1) .EQ. 0 ) THEN
            DO j7lu = i7lu, n7lu
              IF ( k7lu(j7lu,3) .EQ. i7lu ) k7lu(j7lu,1) = 0
            ENDDO
C
          ELSEIF ( k7lu(i7lu,3) .NE. jorig ) THEN
            jorig = k7lu(i7lu,3)
C
C Ignore decay products of charged pions/kaons below 1 GeV/c
C (DWC change: charged kaons can generate pi0, so only pions are ignored.)
C
C
            amom = plu(jorig,8)
            IF ( amom .LT. 1. .AND. (
     .           ABS(k7lu(jorig,2)) .EQ. 211 ) ) THEN
              DO j7lu = i7lu, n7lu
                IF ( k7lu(j7lu,3) .EQ. jorig ) k7lu(j7lu,1) = 0
              ENDDO
              GOTO 5
            ENDIF
C
C -- Compute the length of flight
C
            xyz( 1) = v7lu(jorig,1)/10.
            xyz( 2) = v7lu(jorig,2)/10.
            xyz( 3) = v7lu(jorig,3)/10.
            vect(1) = p7lu(jorig,1)/amom
            vect(2) = p7lu(jorig,2)/amom
            vect(3) = p7lu(jorig,3)/amom
            qchrg = plu(jorig,6)
            plen = plu(jorig,8)/plu(jorig,5) * v7lu(jorig,5) / 10.
C
C -- Check that the particle didn't enter the HCAL during the flight
C
            IF ( ABS(qchrg) .GT. 1E-3 ) THEN
              CALL ucopy(xyz(1),pin(1),3)
              CALL ucopy(vect(1),pin(4),3)
              pin(7) = amom
              CALL vzero(pout(1),6)
              CALL auhcyl(rdime(12),zdime(12),bfield,qchrg,pin,pout,ic)
              plhcal = (pout(3)-pin(3))/pin(6)
              IF ( idbg .GE. 10 ) THEN
                WRITE(6,*) 'Path Length : ',plen,' cm'
                WRITE(6,*) 'HCAL in at  : ',plhcal,' cm'
              ENDIF
              IF( plhcal .LT. plen .AND. ic .NE. 0 ) THEN
                DO j7lu = i7lu, n7lu
                  IF ( k7lu(j7lu,3) .EQ. jorig ) k7lu(j7lu,1) = 0
                ENDDO
                IF ( ABS(k7lu(jorig,2)) .NE. 211 .AND.
     .               ABS(k7lu(jorig,2)) .NE. 321 )
     .                   k7lu(jorig,2) =  NINT(SIGN(1.0,qchrg)) * 2212

                GOTO 5
              ENDIF
            ENDIF
C
C -- Create a new ALPHA vertex
C
            jvert = jvert + 1
            ivert(jvert) = KVERSV(dummy)
            k7en (jorig) = jvert
C
C -- Extrapolate
C
            call augxyz(
     .              xyz,vect,amom*1000.,qchrg,bfield,plen,cc,xx,ct,st)
C
C -- Drop the mother- unstable particle if it decays early
C -- Drop all the daugthers if it decays in the calorimeters
C
            ray = SQRT ( xx(1)**2+xx(2)**2 )
            zed = ABS ( xx(3) )
            IF ( idbg .GE. 10 ) THEN
              WRITE(6,*) 'Decay : particle code = ',k7lu(jorig,2)
              WRITE(6,*) 'R,Z of the decay      = ',ray,zed
              WRITE(6,*) 'Decay products        : '
              DO j7lu = i7lu, n7lu
                IF ( k7lu(j7lu,3) .EQ. jorig ) WRITE(6,*) k7lu(j7lu,2)
              ENDDO
            ENDIF
            IF ( ABS(k7lu(jorig,2)) .NE. 211 .AND.
     .           ABS(k7lu(jorig,2)) .NE. 321 ) THEN
              IF ( ray .GT. rdime(7) .OR. zed .GT. zdime(7) ) THEN
                IF ( ABS(qchrg) .LT. 1E-3 ) THEN
                  k7lu(jorig,2) =   130
                ELSEIF ( qchrg .GT. 1E-3 ) THEN
                  k7lu(jorig,2) =  2212
                ELSE
                  k7lu(jorig,2) = -2212
                ENDIF
                DO j7lu = i7lu, n7lu
                  IF ( k7lu(j7lu,3) .EQ. jorig ) k7lu(j7lu,1) = 0
                ENDDO
              ELSEIF ( ray .GT. rdime(6) ) THEN
                IF ( ABS(qchrg) .LT. 1E-3 ) THEN
                  k7lu(jorig,1) = 0
                ELSEIF ( qchrg .GT. 1E-3 ) THEN
                  k7lu(jorig,2) =  2212
                ELSE
                  k7lu(jorig,2) = -2212
                ENDIF
              ELSE
                k7lu(jorig,1) = 0
              ENDIF
C
C -- Special care is taken for charged pi/K
C
            ELSE
C
              IF (
     .         ray .GT. rdime(8)+RNDM(ray)*(rdime(12)-rdime(8)) .OR.
     .         zed .GT. zdime(8)+RNDM(ray)*(zdime(12)-zdime(8)) ) THEN
                DO j7lu = i7lu, n7lu
                  IF ( k7lu(j7lu,3) .EQ. jorig ) k7lu(j7lu,1) = 0
                ENDDO
              ELSEIF ( ray .GT. rdime(6) ) THEN
                DO j7lu = i7lu, n7lu
                  IF ( k7lu(j7lu,3) .EQ. jorig .AND.
     .                  plu(j7lu,6) .EQ. qchrg ) THEN
                    k7lu(jorig,2) = k7lu(j7lu,2)
                  ENDIF
                ENDDO
                deltaR = ray-AMAX1(rdime(6),SQRT(xyz(1)**2+xyz(2)**2))
                deltaZ = ABS(xx(3)-xyz(3))
                IF ( idbg .GE. 10 ) THEN
                  WRITE(6,*) 'Vertex de depart : ',xyz
                  WRITE(6,*) 'Vertex d''arrivee : ',xx
                  WRITE(6,*) 'DeltaR,DeltaZ     : ',deltaR,deltaZ
                ENDIF
                IF ( deltaR .LT. (rdime(7)-rdime(6))/3. .AND.
     .               deltaZ .LT.  zdime(7)          /3. ) THEN
                  k7lu(jorig,1) = 0
                  IF ( idbg .GE. 10 )
     .            WRITE(6,*) 'La particule initiale est viree'
                ELSE
                  DO j7lu = i7lu, n7lu
                    IF ( k7lu(j7lu,3) .EQ. jorig ) k7lu(j7lu,1) = 0
                  ENDDO
                  IF ( idbg .GE. 10 )
     .            WRITE(6,*) 'Les particules finales sont virees'
                ENDIF
              ELSE
                k7lu(jorig,1) = 0
              ENDIF
            ENDIF
C
            IF ( k7lu(i7lu,1) .EQ. 0 ) THEN
              DO j7lu = i7lu, n7lu
                IF ( k7lu(j7lu,3) .EQ. i7lu ) k7lu(j7lu,1) = 0
              ENDDO
            ENDIF
C
            RW(jv(jvert,JQVRVX)) = xx(1)
            RW(jv(jvert,JQVRVY)) = xx(2)
            RW(jv(jvert,JQVRVZ)) = xx(3)
C
            DO j7lu = i7lu, n7lu
              px = p7lu(j7lu,1)*ct + p7lu(j7lu,2)*st
              py = p7lu(j7lu,2)*ct - p7lu(j7lu,1)*st
              p7lu(j7lu,1) = px
              p7lu(j7lu,2) = py
              IF ( k7lu(j7lu,3) .EQ. jorig ) THEN
                k7vr(j7lu)   = jvert
                v7lu(j7lu,1) = xx(1) * 10.
                v7lu(j7lu,2) = xx(2) * 10.
                v7lu(j7lu,3) = xx(3) * 10.
              ENDIF
            ENDDO
C
          ENDIF
C
    5   CONTINUE
C
        IF ( idbg .GE. 10 ) CALL lulist(1)
C
C Special treatment for charged pions/kaons.
C (DWC change: Charged kaon decays into pi+ pi0 should not be ignored)
C
C        IF ( ABS(k7lu(1,2)).EQ.211 .OR. ABS(k7lu(1,2)).EQ.321 ) THEN
        IF ( ABS(k7lu(1,2)).EQ.211 ) THEN
          ndec = 0
          DO i7lu = 1, n7lu
            IF ( k7lu(i7lu,1) .NE. 0 ) ndec = ndec + 1
          ENDDO
          IF ( ndec .EQ. 1 ) GOTO 1
        ENDIF
C
        IW(jv(1,jqvrip)) = itk
        IW(koqvec+itk*kcqvec+jqveev) = ivert(1)
C
C -- Add new "stable" states to the MCT section in ALPHA
C
        kunkn = 0
C
        DO 20 i7lu = 1, n7lu
C
C Deal only with detectable particles
C
          IF (k7lu(i7lu,1).EQ. 0 ) GOTO 20
C
C Check that the remaining particles are stable, indeed.
C
          IF ( .NOT.fstab(abs(k7lu(i7lu,2))) ) THEN
            kunkn= kunkn+1
            WRITE(6,*) ' +++ QUDECA +++ Unknown stable particle !!! ',
     .                  k7lu(i7lu,2),kunkn
            IF ( kunkn .EQ. 5 ) STOP 99
            GOTO 20
          ENDIF
C
C Search for the LUND code
C
          kpa= 0
          DO l1 = 1, istab
            IF ( k7lu(i7lu,2) .EQ. kcolu1(l1) ) THEN
              kpa = l1
              GOTO 21
            ENDIF
          ENDDO
21        CONTINUE
          IF ( kpa .EQ. 0 ) THEN
            WRITE(*,*) ' QUDECA *** KPA not found *** ',K7lu(i7lu,2)
            GOTO 20
          ENDIF
C
C And add the particles to the ALPHA MCT section
C
          DO 22 l4 = 1 , 4
   22     vec(l4)= p7lu(i7lu,l4)
C
          p2 = vec(1)**2 + vec(2)**2 + vec(3)**2
          IF ( vec(4)**2 .LT. p2 ) vec(4)= SQRT(p2+1.e-6)
          CALL qvset4(inew,vec)
          CALL qvsetm(inew,p7lu(i7lu,5))
          isave= KVSAVC(inew,parna(kpa),kmonte)
          RW(j1(jqvech)) = PLU(i7lu,6)
          RW(j1(jqveqp)) = PLU(i7lu,8)
          IW(j1(jqvepa)) = KCOAL(kpa)
C
C Vertex attributes
C
C
          IW(j1(jqveov)) = ivert(k7vr(i7lu))
          IF ( k7en(i7lu) .GT. 0 )
     .    IW(j1(jqveev)) = ivert(k7en(i7lu))
C
C Stability code
C
          IW(j1(jqvesc)) = 1
C
C Mother/Daughter relation
C
          CALL qadau(itk,1,isave)
          CALL qamoth(itk,1,isave)
C
C  Set bit masks
C
          CALL vzero(IW(j1(jqvebm)) , 10)
          CALL qsmctm(isave)
C
          IF ( idbg .GE. 10 ) THEN
            WRITE(6,*) 'Saved : ',isave,cqtpn(isave),qe(isave)
          ENDIF
C
   20   CONTINUE
C
C  De-stabilize the original track
C
        RW(koqvec+itk*kcqvec+jqvesc)=-1
C
        IF ( idbg .GE. 5 ) THEN
          CALL luedit(0)
          CALL lulist(1)
        ENDIF
C
    1 CONTINUE
C
  999 RETURN
      END
