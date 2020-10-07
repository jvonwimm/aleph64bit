      SUBROUTINE VDHITMATCH
CKEY MATCHING / INTERNAL
C-------------------------------------------------------------------------------
C! Auxiliary to VDHMATCH
C  Match VDET hits with MC truth.  This uses the VDHT,FPOI banks for the
C  MC truth, VCPL,VDXY and VDZY for the reconstructed hits.
C  Author      Dave Brown, 30-6-94
C  Modified: Paul Rensing 30-1-96
C      Use new VDET geometry package.
C      Assume VDET geometry is already initialized.
C      Fix problem with MC hits right at edge of wafer.
C-------------------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT NONE
C
C  Alpha commons :
#include "qdecl.h"
#include "qcde.h"
C  VDET commons :
#include "vhmat.h"
C  Specific HAC commons :
#include "vdhtjj.h"
#include "fpoijj.h"
#include "vcpljj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
C
      INTEGER IMCNEF
      PARAMETER (IMCNEF = 536870912)
      REAL WINDOW
      PARAMETER (WINDOW = 0.05)
C
C  Local variables
C
      SAVE IVDHT,IVCPL,IFPOI,IVDXY,IVDZT
      SAVE FIRST
      SAVE WAFLIMIT, NWAFER
      INTEGER I
      INTEGER UFLAG(MAXHT,NVWMAX),WFLAG(MAXHT,NVWMAX)
      INTEGER IVDHT,IVCPL,IFPOI,IVDXY,IVDZT
      INTEGER JVDHT,JVCPL,JFPOI,JVDXY,JVDZT
      INTEGER NGHIT,NHIT,IHIT,IERR
      INTEGER INUM,ILAY,IWAF,IPHI,IVIEW,JWAF,INDEX
      INTEGER IKINE,ISAVED
      INTEGER IGAL,IUHIT,IWHIT
      INTEGER IFKIN,ITRK,INUMXY,INDXY
      INTEGER NTRUE,NWRONG(2),NMISS(2),NDOUBLE(2)
      INTEGER IALTRK,IMCTRK
      REAL UPOS_G(MAXHT,NVWMAX),WPOS_G(MAXHT,NVWMAX)
      REAL UPOS_J(MAXHT,NVWMAX),WPOS_J(MAXHT,NVWMAX)
      REAL MINU,MINW,DELU,DELW
      REAL XYZ1(3), XYZ2(3), XYZA(3), VUW1(3), VUW2(3), VUWA(3)
      REAL WAFLIMIT(2,2,NVVIEW)
      INTEGER NWAFER
      LOGICAL FIRST
C
C  Functions
C
      INTEGER NAMIND, VNRWAF, VNRMOD
C
C  Inline functions
C
      LOGICAL MCINEFF
      INTEGER IQUAL
C
      DATA FIRST/.TRUE./
C
C
      MCINEFF(IQUAL) = IAND(IQUAL,IMCNEF) .GT. 0
C
#include "qmacro.h"
C-------------------------------------------------------------------------------
C  First time around, setup VDET geometry and indexing
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IVDHT = NAMIND('VDHT')
        IVCPL = NAMIND('VCPL')
        IFPOI = NAMIND('FPOI')
        IVDXY = NAMIND('VDXY')
        IVDZT = NAMIND('VDZT')
C
        DO IVIEW = 1, NVVIEW
           CALL VSENSI(IVIEW, WAFLIMIT(1,1,IVIEW), WAFLIMIT(2,1,IVIEW),
     $          WAFLIMIT(1,2,IVIEW), WAFLIMIT(2,2,IVIEW))
        END DO
        NWAFER = VNRWAF() * VNRMOD()
      END IF
C
C  Zero the counters
C
      CALL VZERO(NGAL,NVWMAX)
      CALL VZERO(NU,NVWMAX)
      CALL VZERO(NW,NVWMAX)
      CALL VZERO(ITRK_U,NVWMAX*MAXHT)
      CALL VZERO(ITRK_W,NVWMAX*MAXHT)
      CALL VZERO(ITRK_G,NVWMAX*MAXHT)
      CALL VZERO(UFLAG,NVWMAX*MAXHT)
      CALL VZERO(WFLAG,NVWMAX*MAXHT)
      CALL VZERO(IUMATCH,NVWMAX*MAXHT)
      CALL VZERO(IWMATCH,NVWMAX*MAXHT)
      CALL VZERO(NUMATCH,NVWMAX*MAXHT)
      CALL VZERO(NWMATCH,NVWMAX*MAXHT)
C
C  Find the MC truth banks
C
      JVDHT = IW(IVDHT)
      JFPOI = IW(IFPOI)
      IF (JVDHT .LE. 0.OR.JFPOI .LE. 0) THEN
        GOTO 999
      END IF
C-------------------------------------------------------------------------------
C  Loop over the hits, and store local positions by wafer
C
      NGHIT = LROWS(JVDHT)
      DO IHIT = 1,NGHIT
        IKINE = ITABL(JVDHT,IHIT,JVDHTN)
        ILAY = ITABL(JVDHT,IHIT,JVDHLN)
        IPHI = ITABL(JVDHT,IHIT,JVDHPN)
C
C  Translate the KINE track number to FKIN
C
        IFKIN = ITABL(JFPOI,IKINE,JFPOIP)
        ISAVED = ITABL(JFPOI,IKINE,JFPOIS)
        IF (ISAVED.EQ.0) THEN
C
C  Convert the entry/exit point into a local (wafer depth center) coordinate
C
           DO I = 1, 3
              XYZ1(I) = RTABL(JVDHT,IHIT,JVDHXE+i-1)
              XYZ2(I) = RTABL(JVDHT,IHIT,JVDHXL+i-1)
              XYZA(I) = 0.5 * (XYZ1(I) + XYZ2(I))
           END DO
C
C  Since the wafer isn't specified, try them all, and take the one
C  with the smallest W (Z relative to the wafer center)
C
          MINW = 100.
          DO IWAF = 1,NWAFER
            CALL VAENWA(INUM,ILAY,IWAF,IPHI,1)
            CALL VGWFVU(INUM,XYZA,VUWA)
            IF (ABS(VUWA(3)) .LT. MINW) THEN
              MINW = ABS(VUWA(3))
              JWAF = IWAF
            END IF
          END DO
          CALL VAENWA(INUM,ILAY,JWAF,IPHI,1)
          CALL VGWFVU(INUM,XYZ1,VUW1)
          CALL VGWFVU(INUM,XYZ2,VUW2)
          CALL VGWFVU(INUM,XYZA,VUWA)
C
C  Check if the hit is within the active area
C
          IF ((VUW1(2) .LE. WAFLIMIT(2,2,2) .AND.
     $         VUW1(2) .GE. WAFLIMIT(1,2,2) .AND.
     $         VUW1(3) .LE. WAFLIMIT(2,1,1) .AND.
     $         VUW1(3) .GE. WAFLIMIT(1,1,1))
     $         .OR.
     $         (VUW2(2) .LE. WAFLIMIT(2,2,2) .AND.
     $         VUW2(2) .GE. WAFLIMIT(1,2,2) .AND.
     $         VUW2(3) .LE. WAFLIMIT(2,1,1) .AND.
     $         VUW2(3) .GE. WAFLIMIT(1,1,1))) THEN
C
C  Store the position and galeph track by wafer index
C
             CALL VJWFFW(ILAY, IPHI, JWAF, INDEX)
             IF (NGAL(INDEX) .EQ. MAXHT) THEN
                WRITE(6,*) 'VDHITMATCH: Too many MC hits in wafer ',INUM
                WRITE(6,*) '     Try increasing parameter MAXHT'
             ELSE
                IGAL = NGAL(INDEX) + 1
                NGAL(INDEX) = IGAL
                UPOS_G(IGAL,INDEX) = VUWA(2)
                WPOS_G(IGAL,INDEX) = VUWA(3)
                ITRK_G(IGAL,INDEX) = IFKIN
             END IF
          END IF
        END IF
      END DO
C-------------------------------------------------------------------------------
C  Now unpack the VDXY bank; just chain through from the first
C
      JVDXY = IW(IVDXY)
      DO WHILE(JVDXY .GT. 0)
        INUM = IW(JVDXY-2)
        CALL VADEWA(INUM,ILAY,IWAF,IPHI,IVIEW)
        CALL VJWFFW(ILAY, IPHI, IWAF, INDEX)
        NU(INDEX) = MIN(LROWS(JVDXY),MAXHT)
        DO IHIT = 1,NU(INDEX)
          UPOS_J(IHIT,INDEX) = RTABL(JVDXY,IHIT,JVDXUC)
          UFLAG(IHIT,INDEX) = ITABL(JVDXY,IHIT,JVDXQF)
        END DO
        JVDXY = IW(JVDXY-1)
      END DO
C
C  Same for VDZT
C
      JVDZT = IW(IVDZT)
      DO WHILE(JVDZT .GT. 0)
        INUM = IW(JVDZT-2)
        CALL VADEWA(INUM,ILAY,IWAF,IPHI,IVIEW)
        CALL VJWFFW(ILAY, IPHI, IWAF, INDEX)
        NW(INDEX) = MIN(LROWS(JVDZT),MAXHT)
        DO IHIT = 1,NW(INDEX)
          WPOS_J(IHIT,INDEX) = RTABL(JVDZT,IHIT,JVDZWC)
          WFLAG(IHIT,INDEX) = ITABL(JVDZT,IHIT,JVDZQF)
        END DO
        JVDZT = IW(JVDZT-1)
      END DO
C-------------------------------------------------------------------------------
C  Loop through the VCPL banks, and flag the associated hits with their track #
C
      JVCPL = IW(IVCPL)
      DO WHILE(JVCPL .GT. 0)
        NHIT = LROWS(JVCPL)
        ITRK = IW(JVCPL-2)
        DO IHIT = 1,NHIT
          INUM = ITABL(JVCPL,IHIT,JVCPZB)
          INUMXY = ITABL(JVCPL,IHIT,JVCPXB)
          CALL VADEWA(INUM,ILAY,IWAF,IPHI,IVIEW)
          CALL VJWFFW(ILAY, IPHI, IWAF, INDEX)
          CALL VADEWA(INUMXY,ILAY,IWAF,IPHI,IVIEW)
          CALL VJWFFW(ILAY, IPHI, IWAF, INDXY)
          IUHIT = ITABL(JVCPL,IHIT,JVCPNX)
          IWHIT = ITABL(JVCPL,IHIT,JVCPNZ)
          IF (IUHIT .GT. 0) THEN
C
C  Check if the hit is really present (not inefficiency).  If not,
C  flag by a negative track number
C
            IF (MCINEFF(UFLAG(IUHIT,INDXY))) THEN
              ITRK_U(IUHIT,INDXY) = -ITRK
            ELSE
              ITRK_U(IUHIT,INDXY) = ITRK
            END IF
          END IF
          IF (IWHIT .GT. 0) THEN
            IF (MCINEFF(WFLAG(IWHIT,INDEX))) THEN
              ITRK_W(IWHIT,INDEX) = -ITRK
            ELSE
              ITRK_W(IWHIT,INDEX) = ITRK
            END IF
          END IF
        END DO
        JVCPL = IW(JVCPL-1)
      END DO
C-------------------------------------------------------------------------------
C  Now, match reconstructed hits with the true hits.
C
      DO INDEX = 1,NVWMAX
C
C Determine the global wafer number of the corresponding XY wafer
C   (complex, but uses the functions completely!)
C
         CALL VFWAFI(INDEX, ILAY, IPHI, IWAF)
         CALL VAENWA(INUM,ILAY,IWAF,IPHI,2)
         CALL VRMWF(INUM, 2, INUM)
         CALL VADEWA(INUM,ILAY,IWAF,IPHI,IVIEW)
         CALL VJWFFW(ILAY, IPHI, IWAF, INDXY)
C
        DO IGAL = 1,NGAL(INDEX)
          MINU = 1000.
          MINW = 1000.
          IUHIT = 0
          IWHIT = 0
          DO IHIT = 1,NU(INDXY)
            DELU = ABS(UPOS_J(IHIT,INDXY)-UPOS_G(IGAL,INDEX))
            IF (DELU .LT. MINU) THEN
              MINU = DELU
              IUHIT = IHIT
            END IF
          END DO
          DO IHIT = 1,NW(INDEX)
            DELW = ABS(WPOS_J(IHIT,INDEX)-WPOS_G(IGAL,INDEX))
            IF (DELW .LT. MINW) THEN
              MINW = DELW
              IWHIT = IHIT
            END IF
          END DO
C
C  If the hits match, store the matching
C
          IF (MINU .LE. WINDOW) THEN
            NUMATCH(IUHIT,INDXY) = NUMATCH(IUHIT,INDXY) + 1
            ITRK = ITRK_U(IUHIT,INDXY)
            IF (ITRK .GT. 0) THEN
              IUMATCH(IGAL,INDEX) = IUHIT
            END IF
          END IF
          IF (MINW .LE. WINDOW) THEN
            NWMATCH(IWHIT,INDEX) = NWMATCH(IWHIT,INDEX) + 1
            ITRK = ITRK_W(IWHIT,INDEX)
            IF (ITRK .GT. 0) THEN
              IWMATCH(IGAL,INDEX) = IWHIT
            END IF
          END IF
        END DO
      END DO
C
 999  CONTINUE
      RETURN
      END
#endif
