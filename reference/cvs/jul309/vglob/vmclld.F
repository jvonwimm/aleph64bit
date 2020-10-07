      SUBROUTINE VMCLLD(ITRK,VIEW,ERRMAT)
C-------------------------------------------------------------
C! collect VDET hits within a given area around a track
CKEY VGLOB VDET TRACK
C
C  Author      : A. Bazarko 31-Oct-94
C
C   This routine creates a VMTC bank, which is the collection
C   of VDET hits within a road of specified size per 
C   VDET layer. (VMTC bank number = VDET layer number) 
C   Calls VMOVER which combines hits in the wafer overlap 
C   regions to create so-called eta hits. 
C
C
C  INPUTS:
C    INTEGER ITRK     = FRFT track number
C    INTEGER VIEW     = Flag specifying which view to consider
C                         1 if in z (W); 2 if in r-phi (U);
C    REAL ERRMAT(8,8) = Error matrix of the track extrapolation
C
C  OUTPUTS:
C    VMTC banks 1, 2  
C
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      REAL    ERRMAT(8,8)
      INTEGER ITRK,VIEW
C
C  Global includes
C
#include "bcs.h"
#include "vglbcm.h"
C
#include "vmtcjj.h"
#include "vdmrjj.h"
#include "vdgcjj.h"
#include "frftjj.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vtxtjj.h"

      INTEGER NROWVMTC
      PARAMETER (NROWVMTC = 200)
C
C  Function calls
C
      INTEGER NLINK, NBANK, NAMIND
      INTEGER VNRWAF, VDYEAR, VNRMOD, VSENSI
      EXTERNAL NLINK, NBANK, NAMIND
      EXTERNAL VNRWAF, VDYEAR, VNRMOD, VSENSI
C
C  Local variables
C
      INTEGER indVMTC(NLAYER), INDEX
      INTEGER Ihit, Ilay, iphi, j
      INTEGER IROWS(2), Nlay(NLAYER)
      INTEGER IZED, wafer, ioff, iwaf
      INTEGER Ierr
      INTEGER IBANK, nwafers
      INTEGER indFRFT, NamFRFT
      integer nwafused(2), nwafext(NLAYER), lstwaf
      integer waflist, banklist, jview
      logical added
      INTEGER indVTXT, indVDXZ, NASS, NAssCl, IQFL
      REAL RES, PULSE, Xhit(2), PNORM
      REAL WUPAIR(2), WUERR(2)
      REAL SIGhit, SIGwindow
      REAL PTOT, PCOR, PVEC(3), THETA, DERR(10)
      REAL XYZ(3), XYZ0(3), UW(2), TPAR(5)
      real waflimit(2,2)
      CHARACTER*4 hitbnk(NVIEW)
C
      integer indVDGC, indVDMR, NamVDGC, year
      integer irun, ievt, kvtdp, nvmtc, itrkdp, jtrkdp
      integer IRET
C
      save hitbnk, NamFRFT, NamVDGC
C
C - bit 1 (IVETOC=2) is set in VDXY and VDZT quality flag to indicate
C   a generic veto hit.
C   bit 30 (IMCNEF=536870912) is set in VDXY and VDZT quality flag
C   to indicate a MC inefficiency rejected hit.
      INTEGER IVETOC, IMCNEF, IMASK
      PARAMETER (IVETOC=2, IMCNEF=536870912)
C
C  Declare statement function variables
C
#include "bmacrod.h"
C
      INTEGER WW, HH, VHPCK
C
C  Data statements
C
      DATA NamFRFT/0/
      DATA HITBnk/'VDZT','VDXY'/
C
C  Inline functions
C
#include "bmacro.h"
C
C Statement function to create a single integer ID for a hit.
C
      VHPCK(WW, HH) = WW*1000 + HH
C
C  First time, find the FRFT name index
C
      IF (NamFRFT.LE.0) THEN
         NamFRFT = NAMIND('FRFT')
         NamVDGC = NAMIND('VDGC')
      END IF
      indFRFT = IW(NamFRFT)
C
      indVMTC(1) = 0
      indVMTC(2) = 0
C
      year = VDYEAR()
      if (year .eq. 95) then
         indVDGC = IW(NamVDGC)
         if (indVDGC .eq. 0) then
            call rerror('VMCLLD', -1, ' Missing VDGC Bank')
            goto 999
         end if
      end if
C
C Limits of sensitive region
C
      iret = VSENSI(view, waflimit(1,1), waflimit(2,1),
     $     waflimit(1,2), waflimit(2,2))
C
C  Find the bank with the track extrapolation to the VDET
C
      indVTXT = NLINK('VTXT', itrk)
      if (indVTXT.eq.0) then
         call rerror('VMCLLD', -2, ' Missing VTXT Bank')
         goto 999
      end if
      IMASK = IVETOC + IMCNEF
C 
C  Create the VMTC bank, 1 for inner layer, 2 for outer layer 
C   initially with room for nrowvmtc hits per layer
C
      DO J = 1, 2
         indVMTC(J) = NBANK('VMTC', J, LMHLEN + nrowvmtc*LVMTCA)
         IF (indVMTC(J) .LE. 0) THEN
            CALL RERROR('VMCLLD',-3,' Cant build VMTC banks')
            call bosbk(iw)
            GOTO 999
         END IF
         CALL VZERO(IW(indVMTC(J)+1), IW(indVMTC(J)))
         IW(indVMTC(J)+LMHCOL) = LVMTCA
         nwafused(j) = 0
         nwafext(j) = 0
      END DO
C
C First, build a list of the wafers to search. Start with the wafers in
C     VTXT and then expand the search if the extrapolation window is
C     bigger.
C
C Tmp banks to store the wafer lists
      waflist = NBANK('VGXX',0, LMHLEN+36)
      banklist = NBANK('VGXX',1, LMHLEN+36)
      if (waflist .le. 0 .or. banklist .le. 0) then
         call RERROR('VMCLLD', -4, ' Out of memory')
         goto 999
      end if
      iw(waflist+LMHCOL) = 1
      iw(waflist+LMHROW) = 0
      iw(banklist+LMHCOL) = 1
      iw(banklist+LMHROW) = 0
      nwafers = VNRWAF() * VNRMOD()
      do iwaf = 1, LROWS(indVTXT)
         wafer = ITABL(indVTXT, iwaf, jvtxwi)
C First, pack the wafer from VTXT
         call vrmwf(wafer, view, ibank)
         call vaddia(banklist, ibank, ADDED)
         if (added) call vaddia(waflist, wafer, ADDED)
C
C Add the two adjacent Z-wafers
C
         CALL vadewa(wafer, ilay, ized, iphi, jview)
         do j = max(1,ized-1), min(nwafers, ized+1)
            if (j .ne. ized) then
               CALL vaenwa(wafer, ilay, j, iphi, jview)
               call vrmwf(wafer, view, ibank)
               call vaddia(banklist, ibank, ADDED)
               if (added) call vaddia(waflist, wafer, ADDED)
            end if
         end do
      end do
C
C  Loop over the wafers to be searched
C
      lstwaf = -1
      DO IWaf = 1, LROWS(waflist)
         Wafer = ITABL(waflist, iwaf,1)
C 
C If this wafer was in VTXT bank, pick up the info
C
         do j = 1, LROWS(indVTXT)
            if (ITABL(indVTXT, j, jvtxwi) .eq. wafer) then
               WUPAIR(2)    = RTABL(indVTXT, j, jvtxuc)
               WUPAIR(1)    = RTABL(indVTXT, j, jvtxwc)
               wuerr(2) = RTABL(indVTXT, j, jvtxsu)
               wuerr(1) = RTABL(indVTXT, j, jvtxsw)
               pvec(1)   = RTABL(indVTXT, j, jvtxpv)
               pvec(2)   = RTABL(indVTXT, j, jvtxpu)
               pvec(3)   = RTABL(indVTXT, j, jvtxpw)
               XYZ0(1) = RTABL(indVTXT, j, jvtxxc)
               XYZ0(2) = RTABL(indVTXT, j, jvtxyc)
               XYZ0(3) = RTABL(indVTXT, j, jvtxzc)
               goto 40
            end if
         end do
C
C Not found in VTXT. Compute new extrapolation but leave errors alone.
C
         CALL VTXNWT(WAFER, RW(KROW(indFRFT,ITRK)+JFRFIR), XYZ0,
     $        UW, XYZ, PVEC, DERR, IERR)
         IF (IERR .ne. 0) THEN
            call rerror('VMCLLD', 5,' Error in VTXNWT extrapolation')
            goto 200
         end if
         WUPAIR(2) = UW(1)
         WUPAIR(1) = UW(2)
C
 40      continue
C 
C Check that the track hits the wafer in both views
C
         do jview = 1, 2
            if (((WUPAIR(jview) + WUERR(jview)*NSIGRD) .lt.
     $           waflimit(1,jview)) .or.
     $           ((WUPAIR(jview) - WUERR(jview)*NSIGRD) .gt.
     $           waflimit(2,jview))) then
               goto 200
            end if
         end do
C
C  Decode the wafer address
C
         CALL vadewa(WAFER,ilay,ized,iphi,jview)
         nwafext(ilay) = nwafext(ilay) + 1
C 
C Pick up the hit bank. Bank does not exist if there are no hits in the
C wafer.
C
         call vrmwf(wafer, view, ibank)
         indVDXZ = NLINK(HitBnk(view),IBANK)
         IF (indVDXZ .EQ. 0) GOTO 200
C
C pulseheight correction factor (PCOR=costheta) for inclined tracks
C
         PTOT = SQRT( pvec(1)**2 + pvec(2)**2 + pvec(3)**2 )
         PCOR = ABS( pvec(1) ) / PTOT
C
C loop over all the hits for this wafer, calculate residual
C
         do Ihit = 1, LROWS(indVDXZ)
            IF (VIEW.EQ.2) THEN
               Xhit(VIEW) = RTABL(indVDXZ,Ihit,JVDXUC) 
               NASS = ITABL(indVDXZ,Ihit,JVDXNA)
               PULSE = RTABL(indVDXZ,Ihit,JVDXPH) 
               IQFL = ITABL(indVDXZ,Ihit,JVDXQF)
               SIGhit = RTABL(indVDXZ,Ihit,JVDXSU)
            ELSE
               Xhit(VIEW) = RTABL(indVDXZ,Ihit,JVDZWC) 
               NASS = ITABL(indVDXZ,Ihit,JVDZNA)
               PULSE = RTABL(indVDXZ,Ihit,JVDZPH) 
               IQFL = ITABL(indVDXZ,Ihit,JVDZQF)
               SIGhit = RTABL(indVDXZ,Ihit,JVDZSW)
            ENDIF
            RES = Xhit(VIEW) - WUPAIR(VIEW)
C
C apply cuts
C
            IF (IAND(IQFL,IMASK) .NE. 0) GO TO 100
C
C a hit can be assign twice if the pulse height is big enough. If the
C     hit was already assigned once, NAss = -1 indicates that the
C     previous assignment CAN be  a double hit.
C
            PNORM = PULSE*PCOR/PHNOMI
C
C For VDET 95, check the number of times the cluster has been
C     associated.
C
            if (year .eq. 95) then
               indVDMR = NLINK('VDMR', ibank+view-1)
               IF (indVDMR .le. 0) then
                  call RERROR('VMCLLD', -6, ' VDMR bank is missing')
                  goto 999
               end if
               NAssCl = ITABL(indVDGC, ITABL(indVDMR,ihit,JVDMVD),
     $              JVDGNA)
               j = max(abs(NAss), abs(NAssCl))
               if (j .eq. 1 .and. NAss .ne. 1 .and. NAssCl .ne. 1) then
                  NAss = -1
               else
                  NAss = j
               end if
            end if
C
            If (NAss .gt. 0) goto 100
            if (NAss .eq. -1 .and. PNorm .lt. PHTHRN) goto 100
C
C calculate the sigma and cut on a given number of sigma
C
            if (year.eq.95) then
                CALL VHERR1(VIEW,PVEC,PNORM,SIGhit)
            else
                CALL VHERR(VIEW,PVEC,PNORM,SIGhit)
            endif
            SIGwindow = SIGhit + wuerr(VIEW)
            IF (ABS(RES) .GT. NSIGRD * SIGwindow) GO TO 100
C           
C This is a keeper, fill VMTC
C
            if (lstwaf .ne. wafer) then
               nwafused(ilay) = nwafused(ilay) + 1
               lstwaf = wafer
            end if
C
            IF (LFRROW(indVMTC(Ilay)) .LT. 1) THEN
               CALL RERROR('VMCLLD', 7, ' Not enough space in VMTC')
               indVMTC(Ilay) = NBank('VMTC', Ilay,
     $              IW(indVMTC(Ilay))+50*LVMTCA)
               if (indVMTC(Ilay) .le. 0) then
                  call RERROR('VMCLLD', -8, ' Out of memory')
                  goto 999
               end if
            END IF
C
            Index = KNEXT(indVMTC(Ilay))
            IW(Index+JVMTHW) = VHPCK(wafer, ihit)
            RW(Index+JVMTHR) = RES
            RW(Index+JVMTHE) = SIGhit
            RW(Index+JVMTPH) = PNORM
C
            IW(indVMTC(Ilay) + LMHROW) = IW(indVMTC(Ilay) + LMHROW) + 1
C
C end of loop over hits in the wafer
C
 100        continue
         enddo
 200     CONTINUE
C
C end of loop over wafers
C      
      END DO
C
C Compute overlap hits if there was more than 1 wafer in a layer.
C
      ioff=ABS(VIEW-2)
      if (nwafused(1) .gt. 1) then
         Irows(1)=1+ioff
         Irows(2)=3+ioff
         CALL VMOVER(View, 1,ERRMAT,IROWS)
         indVMTC(1) = NLINK('VMTC', 1)
      end if
      if (nwafused(2) .gt. 1) then
         Irows(1)=5+ioff
         Irows(2)=7+ioff
         CALL VMOVER(View, 2,ERRMAT,IROWS)
         indVMTC(2) = NLINK('VMTC', 2)
      end if
C
C Add a null hit row to VMTC
C
 999  CONTINUE
      do ilay = 1, NLAYER
         if (indVMTC(ilay) .gt. 0) then
            IF (LFRROW(indVMTC(ilay)) .LT. 1) THEN
               CALL RERROR('VMCLLD', 9, ' Not enough space in VMTC')
               indVMTC(Ilay) = NBank('VMTC',ilay,
     $              IW(indVMTC(Ilay))+LVMTCA)
               if (indVMTC(Ilay) .le. 0) then
                  call RERROR('VMCLLD', -10, ' Out of memory')
                  goto 1001
               end if
            END IF
C
            Index = KNEXT(indVMTC(ilay))
            call vzero(IW(Index+1), LCOLS(indVMTC(ilay)) )
            if (nwafext(ilay) .gt. 0) then
               IW(Index+JVMTHW)   = -1
            end if
            IW(indVMTC(ilay)+LMHROW) = IW(indVMTC(ilay)+LMHROW) + 1
         end if
      enddo
C
C  Done
C
 1001 continue
      call abruev(irun,ievt)
      kvtdp = nlink('VTDP',ievt)
      if (kvtdp.gt.0) then
        nvmtc = namind('VMTC')
        do itrkdp = 1, iw(kvtdp)
            jtrkdp = iw(kvtdp+itrkdp)
            if (itrk.eq.jtrkdp) then
                write(iw(6),1) jtrkdp, view
 1              format(1X,'VMCLLD - VMTC dump for track ',I6,
     &             ' View',I3)
                write(iw(6),*) 'Layer 1:'
                call bprtab(nvmtc,indvmtc(1),1,lrows(indvmtc(1)))
                write(iw(6),*) 'Layer 2:'
                call bprtab(nvmtc,indvmtc(2),1,lrows(indvmtc(2)))
            endif
        enddo
      endif
      call bdrop(IW,'VGXX')
C
      RETURN
      END
#endif
