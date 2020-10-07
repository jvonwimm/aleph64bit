      SUBROUTINE TPARSET(IALTRK,HAXIS,NJET,TRKJET,
     &     NDAU,TRKDAU,TPAR,TERR,KJET,KHEMI)
CKEY QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C
C! Pack  ALPHA track parameters into the arrays needed by QIPBTAG
C    If the track is a 'mother' track,
C    it is also searched back to its FRFT origins.
C  Author Dave Brown, 12-4-94
C
C  Input arguments :
C
C     IALTRK       :  Alpha track number
C     HAXIS        :  Hemisphere definition axis
C     NJET         :  # of reconstructed jets
C     TRKJET       :  ALPHA track number of jets
C
C  Output arguments :
C
C     NDAU         :  # of 'daughter' tracks (1 for FRFT track)
C     TRKDAU       :  FRFT track number of 'daughters'
C     TPAR         :  5 track parameters.  In case of a neutral track, the
C                     1st element is the momentum
C     TERR         :  Lower 4 parameter error matrix (full)
C     KJET         :  Associated jet number
C     KHEMI        :  Associated hemisphere number
C
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Global includes
C
#include "qdecl.h"
#include "qcde.h"
C
C  IO variables
C
      INTEGER IALTRK
      INTEGER NJET,TRKJET(*),NDAU,TRKDAU(*)
      INTEGER KJET,KHEMI
      REAL TPAR(5),TERR(4,4)
      REAL HAXIS(3)
C
C  Local variables
C
      INTEGER MAXMOTH
      PARAMETER (MAXMOTH=10)
      INTEGER JALTRK,ICOPY
      INTEGER IJET,JET
      INTEGER ICOR,JCOR,IPAR,JPAR,ICOUNT,IVTX
      INTEGER KCHARGE,IFAIL
      INTEGER NMOTH,MMOTH,JMOTH,PMOTH,IMOTH(MAXMOTH),LMOTH(MAXMOTH)
      INTEGER IDAU,JDAU,MDAU,LDAU
      REAL MAXDOT,JDOT
      REAL VTX(3),VVTX(6),PSUM(3),VPSUM(6),PERR(3,3),VPSVI(3,3)
      REAL TFERR(15)
C
C  Inline functions
C
      INTEGER JQVRT
#include "qmacro.h"
      JQVRT(ICOR,JCOR) = KOQVRT + ICOR * KCQVRT + JCOR
C ------------------------------------------------------------------------------
C
C  Assign to jets
C
      KJET=0
      MAXDOT=-1000.
      DO IJET = 1 , NJET
        JET=TRKJET(IJET)
        JDOT= QCOSA(JET,IALTRK)
        IF ( JDOT.GT.MAXDOT ) THEN
          MAXDOT=JDOT
          KJET  = IJET
        END IF
      END DO
C
C  Assign to a hemisphere
C
      JDOT = HAXIS(1)*QX(IALTRK)+HAXIS(2)*QY(IALTRK)+
     &     HAXIS(3)*QZ(IALTRK)
      IF ( JDOT.GT.0. )THEN
        KHEMI = 1
      ELSE
        KHEMI = 2
      END IF
C
C  Chain back through the copies to the original
C
      JALTRK = IALTRK
      ICOPY = KSAME(IALTRK)
      DO WHILE(ICOPY.NE.IALTRK.AND.
     &     (JALTRK.GT.KLCHT.OR.JALTRK.LT.KFCHT))
        JALTRK = ICOPY
        ICOPY = KSAME(ICOPY)
      END DO
C
C  If this is just a track, keep it as such
C
      IF(JALTRK.GE.KFCHT.AND.JALTRK.LE.KLCHT)THEN
        NDAU = 1
        TRKDAU(1) = JALTRK-KFCHT+1
        TPAR(1)=QFRFIR(JALTRK)
        TPAR(2)=QFRFTL(JALTRK)
        TPAR(3)=QFRFP0(JALTRK)
        TPAR(4)=QFRFD0(JALTRK)
        TPAR(5)=QFRFZ0(JALTRK)
C
C  Error matrix; skip the curvature term
C
        DO IPAR=2,5
          DO JPAR=2,5
            TERR(IPAR-1,JPAR-1)=QFRFEM(JALTRK,IPAR,JPAR)
          END DO
        END DO
      ELSE
C
C  Find the component tracks of this track
C
        NDAU = 0
        NMOTH = 1
        IMOTH(1) = JALTRK
        DO WHILE(NMOTH.GT.0)
          MMOTH = 0
          DO JMOTH=1,NMOTH
            PMOTH = IMOTH(JMOTH)
            MDAU = KNDAU(PMOTH)
            DO IDAU=1,MDAU
              LDAU = KDAU(PMOTH,IDAU)
C
C  Chain these back too
C
              JDAU = LDAU
              ICOPY = KSAME(JDAU)
              DO WHILE(ICOPY.NE.LDAU.AND.
     &             (JDAU.GT.KLCHT.OR.JDAU.LT.KFCHT))
                JDAU = ICOPY
                ICOPY = KSAME(ICOPY)
              END DO
              IF(JDAU.GE.KFCHT.AND.JDAU.LE.KLCHT)THEN
C
C  FRFT daughter
C
                NDAU = NDAU + 1
                TRKDAU(NDAU) = JDAU-KFCHT+1
              ELSE
C
C  New mother; record it
C
                MMOTH = MMOTH + 1
                LMOTH(MMOTH) = JDAU
              END IF
            END DO
          END DO
C
C  Copy the new mothers into the loop array
C
          NMOTH = MMOTH
          DO JMOTH=1,MMOTH
            IMOTH(JMOTH) = LMOTH(JMOTH)
          END DO
        END DO
C
C  Create the track parameters and error matrix from the YTOP arrays.
C  First, unpack the positions and errors
C
        IVTX = KENDV(JALTRK)
        VTX(1) = QVX(IVTX)
        VTX(2) = QVY(IVTX)
        VTX(3) = QVZ(IVTX)
        CALL QVGET3(PSUM,JALTRK)
        CALL QVGETS(PERR,JALTRK)
        KCHARGE = KCH(JALTRK)
C
C  Restack the error arrays as YTOP wants them; how ugly!!!
C
        ICOUNT = 0
        DO ICOR = 1,3
          DO JCOR=1,ICOR
            ICOUNT = ICOUNT + 1
            VVTX(ICOUNT) = QVEM(IVTX,ICOR,JCOR)
            VPSUM(ICOUNT) = PERR(ICOR,JCOR)
          END DO
        END DO
C
C  Now get the correlation matrix between the vertex errors and the
C  momentum errors; the ugliest thing yet!!!
C
        DO ICOR=1,3
          DO JCOR = 1,3
            VPSVI(ICOR,JCOR) = RW(JQVRT(IVTX,3*(ICOR-1) +
     &           JCOR + JQVRET-1))
          END DO
        END DO
C
C  Convert the parameters and errors into standard, 5-parameter
C  format
C
        CALL YTPAR(KCHARGE,VTX,VVTX,PSUM,VPSUM,VPSVI,
     &                  TPAR,TFERR,IFAIL)
C
C  Convert the full error back to qipbtag internal convention; another
C  ugliness
C
        ICOUNT = 0
        DO IPAR=1,5
          DO JPAR=1,IPAR
            ICOUNT = ICOUNT + 1
            IF(IPAR.GT.1.AND.JPAR.GT.1)THEN
              TERR(IPAR-1,JPAR-1) = TFERR(ICOUNT)
              TERR(JPAR-1,IPAR-1) = TFERR(ICOUNT)
            END IF
          END DO
        END DO
      END IF
C
C  Done!
C
      RETURN
      END
#endif
