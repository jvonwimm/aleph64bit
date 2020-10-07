      SUBROUTINE V0SUM
C-----------------------------------------------------------------------
C!  Determine the energy carried by V0's (K0L, Lambda , conv. photon)
C
C  Patrick Janot -- 18 Apr 1990
C-----------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      DIMENSION mot(200)
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      v0mult = 0.
      nv0mlt = 0
      CALL vzero(kav(1),200)
      CALL vzero(kam(1),200)
      CALL vzero(mot(1),200)
C
C  Loop on bad tracks  (CHT sector)
C
      DO 2 jch = kfcht, klcht
C -- Unlocked tracks
        IF ( xlock(jch) ) GOTO 2
C-- Bad tracks only ( but with more than 4 TPC points )
        IF ( kfrtnt(jch)   .LT. n0 ) GOTO 2
        IF ( ABS(qzb(jch)) .LT. z0 .AND. ABS(qdb(jch)) .LT. d0 .AND.
     .     ( qp(jch) .LE. 15. .OR. kfrtni(jch) .GE. 1 .OR.
     .       kfrtnt(jch) .GE. 8 .OR. keidip(jch) .EQ. 1 ) ) GOTO 2
        IF ( idbg .GE. 10 ) WRITE (IW(6),*) 'Track : ',jch
C-- Track already associated :
        DO itaken = 1, 200
          IF ( kam(itaken) .EQ. 0 ) GOTO 20
          IF ( jch .EQ. KCHT(kam(itaken)) ) THEN
            dstmn = 0.
            mothmn = mot(itaken)
            iv0mn = kam(itaken)
            GOTO 21
          ENDIF
        ENDDO
   20   CONTINUE
C
C  Loop on V0 associated tracks ( DCT sector )
C
        dstmn = 999.
        DO 1 iv0 = kfdct , kldct
          ich = kcht(iv0)
          IF ( ich .NE. jch ) GOTO 1
C
C -- Loop on corresponding V0s
C
          DO 10 kv0 = 1 , knmoth(iv0)
            mothv0 = kmoth(iv0,kv0)
            IF ( idbg .GE. 10 )
     .      WRITE (IW(6),*) 'V0 : ',kcht(kdau(mothv0,1)),
     .                         kcht(kdau(mothv0,2))
C Sister already associated !
            IF (kcht(kdau(mothv0,1)) .EQ. jch ) is = 2
            IF (kcht(kdau(mothv0,2)) .EQ. jch ) is = 1
            DO 11 jv0mlt = 1 , nv0mlt
            IF (kcht(kdau(mothv0,is)) .EQ.      kav(jv0mlt) ) GOTO 10
   11       IF (kcht(kdau(mothv0,is)) .EQ. kcht(kam(jv0mlt))) GOTO 10
C Check n_TPC (Ian's Bug)
            IF ( kfrtnt(kcht(kdau(mothv0,is))) .LT. n0 ) GOTO 10
C-- Minimal distance from V0 to vertex ( d0 , z0 )
            alphvx = - ( qx(mothv0)*qyv0vx(mothv0)
     .               +   qy(mothv0)*qyv0vy(mothv0) )
     .               / ( qx(mothv0)**2 + qy(mothv0)**2 )
            distvx = ( qyv0vx(mothv0) + alphvx*qx(mothv0) ) **2
     .             + ( qyv0vy(mothv0) + alphvx*qy(mothv0) ) **2
            distvx = sqrt(distvx)
            zcorvx = qyv0vz(mothv0) + alphvx * qz(mothv0)
            dist = SQRT(distvx**2+zcorvx**2)
            IF ( idbg .GE. 10 ) THEN
              WRITE (IW(6),*) 'V0 origin : ',qyv0vX(mothv0),
     &                         qyv0vY(mothv0),qyv0vZ(mothv0)
              WRITE (IW(6),*) 'V0 impuls. : ',qx(mothv0),qy(mothv0),
     .                                   qz(mothv0)
              WRITE (IW(6),*) 'V0 d0/z0/dist  : ', distvx,zcorvx,dist
            ENDIF
            IF ( distvx      .GT.  5.0 .OR.
     .           abs(zcorvx) .GT. 15.0 ) GOTO 10
            IF ( dist .LT. dstmn ) THEN
              dstmn  = dist
              mothmn = mothv0
              iv0mn  = iv0
            ENDIF
   10     CONTINUE
    1   CONTINUE
C
   21   IF ( dstmn .LT. 999. ) THEN
          mothv0 = mothmn
          i1s = kdau(mothv0,1)
          i2s = kdau(mothv0,2)
          iv0 = iv0mn
          nv0mlt = nv0mlt + 1
          v0mult = v0mult + 1.
          kav(nv0mlt) = jch
          IF ( KCHT(i1s) .EQ. jch ) kam(nv0mlt) = i2s
          IF ( KCHT(i2s) .EQ. jch ) kam(nv0mlt) = i1s
          mot(nv0mlt) = mothv0
          IF ( idbg .GE. 10 ) THEN
            isis = kam(nv0mlt)
            WRITE (IW(6),*) ' Track V0 found : ',jch,iv0
            WRITE (IW(6),*) ' Sister         : ',KCHT(isis),isis
          ENDIF
          vec(1) = qx(iv0)
          vec(2) = qy(iv0)
          vec(3) = qz(iv0)
          vec(4) = qe(iv0)
          CALL addvec(vec,2,jch,kam(nv0mlt),1.)
C
        ELSE
          CALL qltrk(jch)
        ENDIF
C
    2 CONTINUE
C
      RETURN
      END
#endif
