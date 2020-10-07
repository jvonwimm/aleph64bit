      SUBROUTINE QUTPV0
C-----------------------------------------------------------------------
C! Find vzero tracks and fill the V0 section
C
C  Input : -- none
C  Output: -- A set of V0 tracks
C
C  Patrick Janot -- 29 Oct 1993
C-----------------------------------------------------------------------
#include "qcde.h"
#include "qhac.h"
#include "resolu.h"
#include "parcut.h"
#include "fsimid.h"
#include "psfrjj.h"
      PARAMETER( pimass= .1395675 )
      REAL VTP(4)
#include "coddat.h"
#include "qmacro.h"
#include "psfrfu.h"
      j1(j) = koqvec+isave*kcqvec+j
C
      v0mult = 0.
      nv0mlt = 0
      CALL vzero(kav(1),200)
      CALL vzero(kam(1),200)
C
      DO 1 ibad = 1, npsfr
C
C Skip good tracks (previously locked)
C
        itk = KPSFAL(ibad)
        ivr = KORIV(itk)
        IF ( xlock(itk) ) GOTO 1
C
C Require enough hits in the TPC
C
        nhits = KPSFN0(ibad)
        IF ( nhits .LT. n0 ) GOTO 1
C
C Look for one or several track(s) coming from the same vertex
C
        dmin = 999999.
        DO 2 jbad = 1, npsfr
C
          IF ( jbad .EQ. ibad ) GOTO 2
C
          nhjts = KPSFN0(jbad)
          IF ( nhjts .LT. n0 ) GOTO 2
C
          jtk = KPSFAL(jbad)
          jvr = KORIV(jtk)
          IF ( jvr .NE. ivr ) GOTO 2
C
          qqqi  = SIGN(1.0, -QPSFIR(ibad))
          qqqj  = SIGN(1.0, -QPSFIR(jbad))
          IF ( qqqi*qqqj .GE. 0. ) GOTO 2
C
          pxv = QPSFPX(ibad) + QPSFPX(jbad)
          pyv = QPSFPY(ibad) + QPSFPY(jbad)
          pzv = QPSFPZ(ibad) + QPSFPZ(jbad)
C
C Compute d0/z0 for this vertex, and require a nice pointing
C
          alphvx = - (pxv*qvx(ivr)+pyv*qvy(ivr)) / (pxv**2+pyv**2)
          distvx = (qvx(ivr) + alphvx*pxv)**2
     .           + (qvy(ivr) + alphvx*pyv)**2
          distvx = SQRT(distvx)
          zcorvx = qvz(ivr) + alphvx * pzv
          dist   = SQRT(distvx**2+zcorvx**2)
          IF ( idbg .GE. 10 ) THEN
            WRITE(6,*) 'V0 candidate : '
            WRITE(6,*) 'ibad,itk : ',ibad,itk
            WRITE(6,*) 'jbad,jtk : ',jbad,jtk
            WRITE(6,*) 'Vertex   : ',ivr,qvx(ivr),qvy(ivr),qvz(ivr)
            WRITE(6,*) 'd0/z0/d  : ',distvx,zcorvx,dist
          ENDIF
          IF ( distvx .GT. 2.*d0 .OR. abs(zcorvx) .GT. 2.*z0 ) GOTO 2
C
          IF ( dist .LT. dmin ) THEN
            dmin = dist
            iv01 = ibad
            iv02 = jbad
            moth = ivr
          ENDIF
    2   CONTINUE
C
C Fill the V0 section
C
        IF ( dmin .LT. 999. ) THEN
          nv0mlt = nv0mlt + 1
          v0mult = v0mult + 1.
          kav(nv0mlt) = KPSFAL(iv01)
          kam(nv0mlt) = KPSFAL(iv02)
C
          vtp(1) = QPSFPX(iv01)
          vtp(2) = QPSFPY(iv01)
          vtp(3) = QPSFPZ(iv01)
          vtp(4) = SQRT(vtp(1)**2+vtp(2)**2+vtp(3)**2+pimass**2)
C
          CALL addve1(vtp,2)
          nv0tot = nv0tot+1
          imotv0(nv0tot) = KPSFAL(iv01)
          RW(j1(jqvech)) = SIGN(1.,-QPSFIR(iv01))
          IW(j1(jqvesc)) = 1
          IW(j1(jqvepa)) = ktpcod(itk)
          IW(j1(jqvetn)) = iv01
          jv02 = KPSFAL(iv02)
          CALL qltrk(itk)
          IF ( idbg .GE. 10 ) WRITE(6,*)
     .          'V0 Candidate kept : ',iv01,iv02
C
C Re-define the sister momentum if stored as good track
C
          ivv = kpdir(prnam(1),kreco)
   10     IF ( ivv .EQ. 0 ) GOTO 20
          IF ( KTN(ivv) .EQ. iv02 ) THEN
            RW(koqvec+ivv*kcqvec+jqveqx) = QPSFPX(iv02)
            RW(koqvec+ivv*kcqvec+jqveqy) = QPSFPY(iv02)
            RW(koqvec+ivv*kcqvec+jqveqz) = QPSFPZ(iv02)
            GOTO 20
          ENDIF
   15     ivv = kfollo(ivv)
          GOTO 10
   20     CONTINUE
C
      ELSE
      ENDIF
C
    1 CONTINUE
C
  999 RETURN
      END
