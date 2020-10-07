      SUBROUTINE VKAERR(TRK, WAFER, WHIT, UHIT,
     $     vuw, xyz, uErr2, zErr2, WNPulH, UNPulH)
C ---------------------------------------------------------------------
C! Compute the error for the Kalman fit of a VDet hit for a given track
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 21-11-94
C
C Given a hit specified by wafer number and hit number, compute the
C     error needed by the Kalman filter.
C
C  INPUT:
C    integer trk      - FRFT track number
C    integer wafer    - wafer number
C    integer whit     - w wafer hit number; 0 means no hit
C    integer uhit     - u wafer hit number; 0 means no hit
C    
C OUTPUT:
C    real vuw(3)      - local wafer coordinates of the hit
C                        extrapolation point is used for missing hits
C    real xyz(3)      - global coordinates of hit
C    real uErr2       - the r-phi error**2 used by the Kalman
C    real ZErr2       - the z error**2 used by the Kalman
C    real WNPulH      - the w hit's normalized pulse height
C    real UNPulH      - the u hit's normalized pulse height
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C Function parameters
C
      INTEGER trk, WAFER, WHIT, UHIT
      REAL vuw(3), xyz(3), uErr2, ZErr2, UNPulH, WNPulH
C
C  Global includes
C
#include "bcs.h"
#include "vglbcm.h"
C
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vtxtjj.h"
#include "frftjj.h"
C
      real PI
      parameter (PI = 3.141592)
C
C  Function calls
C
      INTEGER NLINK, NAMIND, VDYEAR
      EXTERNAL NLINK, NAMIND, VDYEAR
C
C  Local variables
C
      INTEGER KVDXZ, NamFRFT, NamVALC
      integer indFRFT
      INTEGER hitbank
      REAL PNORM, PULSE
      REAL THETA
      REAL GAMMA,VUW_OR(3),ORIGIN(3), UW(2)
      INTEGER indVALC
      INTEGER IROW,JROW,NROW
      REAL VErr,UErr,WErr, RErr
      real UHitErr, WHitErr
      real gmom(3)
      real PVEC(3), UExtErr, rerrp(10)
      integer ilay, ized, iphi, jview, index, ix
      integer ilay2, ized2, iphi2, ierr
      real deltau, deltar, r_or
      logical noext
      integer year
C
      SAVE ORIGIN
      SAVE NamVALC, NamFRFT
C
#include "bmacrod.h"
C
      INTEGER IIROW,IICOL,IIDIAG,IIVAC,IIWND
      REAL CORMAT
C
      DATA ORIGIN/0.0,0.0,0.0/
      DATA NamVALC/0/
C
C  Inline functions
C
#include "bmacro.h"
C
C  CORMAT: This returns the error correlation matrix from the
C  VALC bank (local alignment).  by definition, the error on the
C  global alignment is 0.  IIWND here refers to the standard
C  wafer index, as returned by the ALEPHLIB routine VDINDX.
C  The BOS index to the VALC bank must be specified (=IIVAC).
C  The IIROW,IICOL values are interchangeable, being markers for
C  the alignment quantities (in order);
C   V displacement
C   U displacement
C   W displacement
C   V rotation
C   U rotation
C   W rotation
C   The returned quantities are SQUARED errors (in the case of diagonal
C   elements).
C
      IIDIAG(IIROW,IICOL) = (MAX(IIROW,IICOL)-1)*MAX(IIROW,IICOL)/2
     &  + MIN(IIROW,IICOL)
      CORMAT(IIVAC,IIWND,IIROW,IICOL) =
     &  RTABL(IIVAC,IIWND,7+IIDIAG(IIROW,IICOL))
C
      IF (NamVALC .eq. 0) then
         NamVALC = NAMIND('VALC')
         NamFRFT = NAMIND('FRFT')
      end if
C
      year = vdyear()
C
      index = NLINK('VTXT', trk)
      if (index .le. 0) then
         call RERROR('VKAERR', -1, ' Missing VTXT bank')
         goto 999
      end if
C
C We need the local momentum vector to calculate the correct Kalman
C     uncertainties. If either hit is missing, we use the extrapolated
C     point to fill in the missing co-ordinate. Also, if the u hit is
C     missing, we use the u extrapolation error to estimate the
C     uncertainty on the radius.
C
      vuw(1) = 0.0
      noext = .false.
      uexterr = -1.0
      CALL vadewa(wafer,ilay,ized,iphi,jview)
      do ix = 1, LROWS(index)
         if (wafer .eq. ITABL(index, ix, JVTXWI)) then
            pvec(1) = RTABL(index, ix, JVTXPV)
            pvec(2) = RTABL(index, ix, JVTXPU)
            pvec(3) = RTABL(index, ix, JVTXPW)
            UExtErr = RTABL(index, ix, JVTXSU)
            vuw(2) = RTABL(index, ix, JVTXUC)
            vuw(3) = RTABL(index, ix, JVTXWC)
            goto 88
         else
            CALL vadewa(ITABL(index, ix, JVTXWI),
     $           ilay2,ized2,iphi2,jview)
            if (ilay .eq. ilay2 .and. iphi .eq. iphi2) then
               gmom(1) = RTABL(index, ix, JVTXXC)
               gmom(2) = RTABL(index, ix, JVTXYC)
               gmom(3) = RTABL(index, ix, JVTXZC)
               UExtErr = RTABL(index, ix, JVTXSU)
            end if
         end if
      end do
      if (UExtErr .le. 0.0) then
         call RERROR('VKAERR', 2,' Wafer not in VTXT bank')
         noext = .true.
         goto 88
      end if
C
C If we did not find the exact wafer, we can calculate the correct
C     co-ordinates. 
C      
      indFRFT = IW(namFRFT)
      if (indFRFT .le. 0) then
         call RERROR('VKAERR', 3, ' Missing FRFT bank')
         noext = .true.
         goto 88
      end if
C
      call vtxnwt(wafer, RW(KROW(indFRFT,TRK)+JFRFIR),
     $     gmom, uw, xyz, pvec, rerrp, ierr)
      vuw(2) = uw(1)
      vuw(3) = uw(2)
      if (ierr .ne. 0) then
         call RERROR('VKAERR', 4, ' Error getting extrapolation')
         noext = .true.
      end if
 88   continue
C
C VDXY and VDZT do not have the same HAC parameters
C
      if (whit .gt. 0) then
         call vrmwf(wafer, 1, hitbank)
         kvdxz = nlink('VDZT', hitbank)
         if (kvdxz .eq. 0) then
            call RERROR('VKAERR', -5,' cannot find VDZT bank')
            goto 999
         end if
C
         vuw(3) = RTABL(KVDXZ,whit,JVDZWC)
         PULSE = RTABL(KVDXZ,whit,JVDZPH) 
         if (noext) then
            WHitErr= RTABL(KVDXZ,whit,JVDZSW)
         else
            WNPulH = (PULSE / PHNOMI) * ABS( pvec(1) ) /
     $           SQRT( pvec(1)**2 + pvec(2)**2 + pvec(3)**2 )
            if (year.eq.95) then
                CALL VHERR1(1, PVEC, WNPulH, WHitErr)
            else
                CALL VHERR(1, PVEC, WNPulH, WHitErr)
            endif
         end if
      else
         WHitErr = 0.0
         WNPulH = 0.0
      end if
C            
      if (uhit .gt. 0) then
         call vrmwf(wafer, 2, hitbank)
         kvdxz = nlink('VDXY', hitbank)
         if (kvdxz .eq. 0) then
            call RERROR('VKAERR', -6,' cannot find VDXY bank')
            goto 999
         end if
C
         vuw(2) = RTABL(KVDXZ,uhit,JVDXUC)
         PULSE = RTABL(KVDXZ,uhit,JVDXPH)
         if (noext) then
            UHitErr= RTABL(KVDXZ,uhit,JVDXSU)
         else
            UNPulH = (PULSE / PHNOMI) * ABS( pvec(1) ) /
     $           SQRT( pvec(1)**2 + pvec(2)**2 + pvec(3)**2 )
            if (year.eq.95) then
                CALL VHERR1(2, PVEC, UNPulH, UHitErr)
            else
                CALL VHERR(2, PVEC, UNPulH, UHitErr)
            endif
         end if
      else
C
C If we do not have a hit in U, use the extrapolation error as the
C     uncertainty. This is important because the uncertainty in U causes
C     an uncertainty in radius.
C
         UHitErr = UExtErr
         UNPulH = 0.0
      end if
C
C Convert local to global co-ordinates.
C
      CALL VGWFXY(WAFER, VUW, xyz)
C
C  Convert the angle (in the r-phi plane) between the ALEPH co-ordinates
C     and the wafer coordinates
C
      CALL VGWFVU(Wafer,ORIGIN,VUW_OR)
      GAMMA = ATAN2(VUW(2)-VUW_OR(2),-VUW_OR(1))
C
C  Resolution due to alignment; First check that VALC is there
C
      indVALC = IW(NamVALC)
      If (indVALC .GT. 0) Then
C
C  Find the wafer
C
        JROW = 0
        DO IROW = 1, LROWS(indVALC)
           If (Wafer .EQ. ITABL(indVALC,IROW,1)) Then
              JROW = IROW
              GOTO 89
           END IF
        END DO
 89     CONTINUE
        If (JROW.GT.0) Then
C
C Compute first directly the errors in the V,U, and W directions. This
C     is a full 1st order calculation using the full error correlation
C     matrix of the local alignment. Note that this depends on the local
C     position of the hits. Note that these are uncertainties**2!!
C
           VErr = CORMAT(indVALC,JROW,1,1)
     &          + CORMAT(indVALC,JROW,6,6)*VUW(2)**2
     &          + CORMAT(indVALC,JROW,5,5)*VUW(3)**2
     &          -2*CORMAT(indVALC,JROW,6,1)*VUW(2)
     &          +2*CORMAT(indVALC,JROW,5,1)*VUW(3)
     &          -2*CORMAT(indVALC,JROW,6,5)*VUW(2)*VUW(3)
C
           UErr = CORMAT(indVALC,JROW,2,2)
     &          + CORMAT(indVALC,JROW,4,4)*VUW(3)**2
     &          -2*CORMAT(indVALC,JROW,4,2)*VUW(3)
C
           WErr = CORMAT(indVALC,JROW,2,2)
     &          + CORMAT(indVALC,JROW,4,4)*VUW(2)**2
     &          +2*CORMAT(indVALC,JROW,4,3)*VUW(2)
        ELSE
           CALL RERROR('VKAERR',7,' Alignment wafer not found')
           VErr = 0.0
           UErr = 0.0
           WErr = 0.0
        END IF
      ELSE
         CALL RERROR('VKAERR',8,' Alignment bank not found')
         VErr = 0.0
         UErr = 0.0
         WErr = 0.0
      END IF
C
C Add the Hit errors to the computed alignment error, project z errors onto
C u and v using the track direction, and then rotate the co-ordinate system
C from v-u to r-phi
C
      WErr = WErr + WHitErr**2
      UErr = UErr + UHitErr**2
      RErr = VErr*cos(gamma)**2 + UErr*sin(gamma)**2
      uErr2 = VErr*(sin(gamma)**2 + (pvec(2)/pvec(1))**2)
     &      + UErr*cos(gamma)**2
      zErr2 = WErr + VErr*(pvec(3)/pvec(1))**2
C      
 999  continue
C
C If we did not actually measure a co-ordinate (ie hit=0), set the
C     uncertainty to BIGERR.
C
      if (whit .le. 0) zErr2 = BIGERR
      if (uhit .le. 0) uErr2 = BIGERR
      return
      end
#endif
