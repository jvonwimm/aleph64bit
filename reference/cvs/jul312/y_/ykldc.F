      subroutine YKLdC(Trk, noVDET, Npts, r, u, z, sigU, sigZ,
     $     vdHits, nvdet, wafers, uVDet, wVDet)
C---------------------------------------------------------------------
C
C! Load kink coordinate arrays for an FRFT track
C
C   Author: Paul Rensing    08/04/97
C
C   Inputs:     FRFT, FRTL, TPCO, ICCO, VCPL and VDCO banks
C               Trk     Track number in FRFT
C               noVDET  Logical flag specifying whether to skip the
C                       loading of VDET hits
C
C   Outputs:    Npts    Number of coordinates on this track
C               r       Array of coordinate radii
C               u       Array of coordinate r-phi
C               z       Array of coordinate z
C               sigU    Array of coordinate r-phi errors 
C               sigZ    Array of coordnate z errors
C               vdhits  Array of VDET hit numbers from VCPL
C               nvdet   Number of VDET hits
C               wafers  Array of VDET wafers crossed by the track
C               uVDet   Array of VDET u coordinates
C               wVDet   Array of VDET w coordinates
C---------------------------------------------------------------------
#ifndef DOC
      implicit none

      integer trk
      integer npts
      real r(*), u(*), z(*), sigU(*), sigZ(*)
      integer vdHits(8)
      integer nvdet
      integer wafers(*)
      real uVDet(*), wVDet(*)
      logical noVDET

#include "bcs.h"

#include "frftjj.h"
#include "frtljj.h"
#include "tpcojj.h"
#include "iccojj.h"
#include "vdcojj.h"
#include "vcpljj.h"

      INTEGER NLINK, NAMIND
      INTEGER VHPCK
      external NLINK, NAMIND

      INTEGER NamTPCO, NamFTCL, NamFRTL, NamFRFT, NamICCO
      SAVE    NamTPCO, NamFTCL, NamFRTL, NamFRFT, NamICCO
      Integer NamVDCO, NamFVCL, NamFICL
      save    NamVDCO, NamFVCL, NamFICL
      
      INTEGER IndTPCO,IndFTCL,IndFRTL,IndFRFT, IndICCO
      Integer IndVDCO, IndFVCL, IndVCPL, IndFICL

      integer i, nc, offset
      integer jj, row
      real xyz(3), vuw(3)

#include "bmacrod.h"
      INTEGER WW, HH
C
      DATA NamFRFT/0/
C
#include "bmacro.h"
C
C Statement function to create a single integer ID for a hit.
C
      VHPCK(WW, HH) = WW*1000 + HH

      IF ( NamFRFT .LE. 0 ) THEN
        NamTPCO = NAMIND('TPCO')
        NamFTCL = NAMIND('FTCL')
        NamFRTL = NAMIND('FRTL')
        NamFRFT = NAMIND('FRFT')
        NamICCO = NAMIND('ICCO')
        NamVDCO = NAMIND('VDCO')
        NamFICL = NAMIND('FICL')
        NamFVCL = NAMIND('FVCL')
      END IF
C
      npts = 0
C
C  Find the track banks
C
      IndFRTL = IW(NamFRTL)
      IndFRFT = IW(NamFRFT)
      IF (IndFRTL.LE.0 .OR. IndFRFT.LE.0) THEN
        CALL RERROR('YKLdC',1,' Cannot find track bank(s)')
        GOTO 999
      END IF
C
C Pick up the TPC co-ordinates
C
      NC = ITABL(IndFRTL, TRK, JFRTNT)
      if (nc .gt. 0) then
         IndTPCO = IW(NamTPCO)
         IndFTCL = IW(NamFTCL)
         IF (IndTPCO.LE.0 .OR. IndFTCL.LE.0) THEN
            CALL RERROR('YKLdC',2,' Cannot find TPC coord bank(s)')
            GOTO 999
         END IF
         offset = ITABL(IndFRTL, TRK, JFRTIT) + NC
         DO i = 1, NC
            ROW = ITABL(IndFTCL, offset-I+1, 1)
            npts = npts + 1
            R(NPTS) = RTABL(IndTPCO, ROW, JTPCRV)
            U(NPTS) = RTABL(IndTPCO, ROW, JTPCPH) * R(NPTS)
            Z(NPTS) = RTABL(IndTPCO, ROW, JTPCZV)
            SIGU(NPTS) = RTABL(IndTPCO, ROW, JTPCSR)
            SIGZ(NPTS) = RTABL(IndTPCO, ROW, JTPCSZ)
         END DO
      end if
C
C ITC co-ordinates
C
      NC = ITABL(IndFRTL, TRK, JFRTNI)
      if (NC .gt. 0) then
         IndICCO = IW(NamICCO)
         IndFICL = IW(NamFICL)
         IF (IndICCO.LE.0 .OR. IndFICL.LE.0) THEN
            CALL RERROR('YKLdC',3,' Cannot find ITC coord bank(s)')
            GOTO 999
         END IF
         offset = ITABL(IndFRTL, TRK, JFRTII)
         DO I = 1, NC
            row = abs( ITABL(IndFICL, offset+I, 1) )
C  Protection against ITC coordinates with zero radius
            if (RTABL(IndICCO,row,JICCRV).NE.0) THEN
                npts = npts + 1
                R(npts) = RTABL(IndICCO, row, JICCRV)
                U(npts) = RTABL(IndICCO, row, JICCPH) * R(npts)
                Z(npts) = RTABL(IndICCO, row, JICCZV)
                SIGU(npts) = RTABL(IndICCO, row, JICCSR)
                SIGZ(npts) = RTABL(IndICCO, row, JICCSZ)
            endif
         end do
      ENDIF
C
      if (noVDET) return
C
C VDet co-ordinates
C Pick up the VCPL bank so we can map the hit numbers
C
      call vzero(vdhits, 8)
      nvdet = 0
      NC = ITABL(IndFRTL, TRK, JFRTNV)
      if (NC .gt. 0) then
         IndVDCO = IW(NamVDCO)
         IndVCPL = NLINK('VCPL', trk)
         IndFVCL = IW(NamFVCL)
         IF (IndVDCO.LE.0 .OR. IndFVCL.LE.0 .or. IndVCPL.le.0) THEN
            CALL RERROR('YKLdC',4,' Cannot find VDet coord bank(s)')
            GOTO 999
         END IF
         offset = ITABL(IndFRTL, TRK, JFRTIV)
         jj = 1
         DO I = 1, NC
            row = ITABL(IndFVCL, offset+I, 1)
            npts = npts + 1
            R(npts) = RTABL(IndVDCO, row, JVDCR0)
            U(npts) = RTABL(IndVDCO, row, JVDCPH) * R(npts)
            Z(npts) = RTABL(IndVDCO, row, JVDCZ0)
            SIGU(npts) = RTABL(IndVDCO, row, JVDCSR)
            SIGZ(npts) = RTABL(IndVDCO, row, JVDCSZ)
C
            if (ITABL(IndVCPL,i,JVCPZB) .ge. 10000 .or.
     $           ITABL(IndVCPL,i,JVCPZB) .ge. 10000) jj = max(5,jj)
            if (ITABL(IndVCPL,i,JVCPNZ) .gt. 0) then
               vdhits(jj) = VHPCK(ITABL(IndVCPL,i,JVCPZB),
     $              ITABL(IndVCPL,i,JVCPNZ))
            end if
            if (ITABL(IndVCPL,i,JVCPNX) .gt. 0) then
               vdhits(jj+1) = VHPCK(ITABL(IndVCPL,i,JVCPXB),
     $              ITABL(IndVCPL,i,JVCPNX))
            end if
            jj = jj + 2
C
C we need wafer local co-ordinates to update VDMS
C
            nvdet = nvdet + 1
            wafers(nvdet) = ITABL(IndVDCO, row, JVDCWI)
            xyz(1) = r(npts) * cos(RTABL(IndVDCO, row, JVDCPH))
            xyz(2) = r(npts) * sin(RTABL(IndVDCO, row, JVDCPH))
            xyz(3) = z(npts)
            call VGWFVU(wafers(nvdet), xyz, vuw)
            uVDet(nvdet) = vuw(2)
            wVDet(nvdet) = vuw(3)
         end do
      ENDIF

 999  continue
      return
      end
#endif
