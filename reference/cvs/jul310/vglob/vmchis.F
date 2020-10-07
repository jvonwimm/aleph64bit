      SUBROUTINE VMCHIS(ITRK, IVIEW, ERRMAT)
C-------------------------------------------------------------
C!Calculate simple chisquared for 2-layer VDET combinations
CKEY VGLOB VDET TRACK
C
C  Author      : D. Brown 7-11-94
C
C   This routine takes as input the two VMTC banks (1 for each layer)
C   of matched hits for a given track.  It loops explicitly over all
C   possible combinations, and computes the chisquared of their association.
C   The chisquared is computed in a simple way, using the track
C   error matrix.
C
C  INPUTS:
C    INTEGER ITRK     = FRFT track row
C    INTEGER IVIEW    = View being considered, 1=Z, 2=rho-phi
C    REAL ERRMAT(8,8) = Error matrix of a track in each of the intersected
C                       wafers, with full correlations.  This should include
C                       the intrinsic hit errors as well
C    banks VMTC 1 and 2
C
C  OUTPUTS:
C    VMWC or VMUC bank, # itrk, depending on the view
C
C-------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      INTEGER ITRK,IVIEW
      REAL ERRMAT(8,8)
C
C  Global includes
C
#include "bcs.h"
#include "vglbcm.h"
C
#include "vmtcjj.h"
#include "vmucjj.h"
#include "vtxtjj.h"
C
C  Function calls
C
      INTEGER NLINK, NBANK, NAMIND
      EXTERNAL NLINK, NBANK, NAMIND
C
C  Local variables
C
      INTEGER ILAY,IROW,JROW, IHIT,JHIT
      INTEGER ICOL,JCOL,IADD,IMIN,JMIN
      INTEGER IERR, i
      INTEGER IPOINT
      INTEGER indVMTC(NLAYER),NHIT(NLAYER)
      INTEGER indVMCN
      INTEGER NNUL,MNUL
      INTEGER wafer, hit
      INTEGER NVDET,MVDET
      INTEGER IADDVEC(4)
      REAL CHIWORST, CHISQ, FULLCHI
      REAL*8 ERR(4,4)
      REAL RESVEC(4),ERRVEC(4)
      LOGICAL BLANK(4)
      CHARACTER*4 BNAMES(2)
      integer indVTXT
      integer phirow(4), errrow(4)
      integer iphi, ized, jview
      integer irun,ievt,kvtdp,nvmcn,itrkdp,jtrkdp
      integer icind(4)
      real err0(4,4)
C
      SAVE BNAMES
C
#include "bmacrod.h"
C
C  Data statements
C
      DATA BNAMES/'VMWC','VMUC'/
C
C  Inline functions
C
#include "bmacro.h"
C
      indVMCN = 0
C
C  Find the VMTC banks
C
      DO ILAY = 1, NLAYER
        indVMTC(ILAY) = NLINK('VMTC',ILAY)
        IF (indVMTC(ILAY) .LE. 0) THEN
          CALL RERROR('VMCHIS', -1, ' Missing VMTC Bank')
          GOTO 999
        END IF
        NHIT(ILAY) = LROWS(indVMTC(ILAY))
      END DO
C
C Find the VTXT bank and determine which ladder is in which slot in the
C     error matrix.
C
      indVTXT = NLINK('VTXT', itrk)
      If (indVTXT .le. 0) Then
         call RError('VMCHIS', -2, ' Missing VTXT Bank')
         goto 999
      End If
      icol = 1
      do i = 1, LROWS(indVTXT)
         CALL vadewa(ITABL(indVTXT,i,JVTXWI),ilay,ized,iphi,jview)
         if (ilay .eq. 2) icol = MAX(icol, 3)
         phirow(icol) = iphi
         icol = icol + 1
      end do
C
C  Build the VM(U,W)C bank
C
      indVMCN = NBANK(BNAMES(IVIEW),ITRK,LMHLEN + LVMUCA*MAXNCHI)
      IF (indVMCN.LE.0) THEN
        CALL RERROR('VMCHIS', -3,' Out of memory')
        GOTO 999
      END IF
      IW(indVMCN + LMHCOL) = LVMUCA
      IW(indVMCN + LMHROW) = 0
C
C  Loop over the banks individually, and determine
C  which rows are 'blank' (IE, which rows only have 0 for a hit address)
C
      DO ICOL = 1, 4
        BLANK(ICOL) = .TRUE.
      END DO
      DO ILAY = 1, NLAYER
        DO IHIT = 1, NHIT(ILAY)
          DO ICOL = 1, NGRK
            IF (ITABL(indVMTC(ILAY),IHIT,JVMTHW+ICOL-1) .gt. 0) then
               BLANK(ICOL+2*(ILAY-1)) = .FALSE.
            END IF
          END DO
        END DO
      END DO
C
C  Loop over the hit combinations and compute the chisquared (inner layer)
C
      DO IHIT = 1, NHIT(1)
C
C  Build the 4-hit residual vector and the hit errors, and the angle
C
        RESVEC(1) = RTABL(indVMTC(1),IHIT,JVMTHR)
        RESVEC(2) = RTABL(indVMTC(1),IHIT,JVMTHR+1)
        ERRVEC(1) = MAX(RTABL(indVMTC(1),IHIT,JVMTHE),MINERR)
        ERRVEC(2) = MAX(RTABL(indVMTC(1),IHIT,JVMTHE+1),MINERR)
C
C  Check for null hits
C
        NNUL = 0
        NVDET = 0
        DO ICOL = 1, 2
          JCOL = ICOL
          IADDVEC(JCOL) = ITABL(indVMTC(1),IHIT,ICOL)
          IF (IADDVEC(JCOL) .LE. 0) THEN
            IF (.NOT. BLANK(JCOL)) THEN
              IADDVEC(JCOL) = -1
              NNUL = NNUL + 1
            END IF
          ELSE
            NVDET = NVDET + 1
            call vhupck(iaddvec(jcol), wafer, hit)
            CALL vadewa(wafer,ilay,ized,iphi,jview)
            if (phirow(1) .eq. iphi) then
               errrow(jcol) = 1 + (2-iview)
            else
               errrow(jcol) = 3 + (2-iview)
            end if
          END IF
        END DO
C
C outer layer
C
        DO JHIT = 1, NHIT(2)
          RESVEC(3) = RTABL(indVMTC(2),JHIT,JVMTHR)
          RESVEC(4) = RTABL(indVMTC(2),JHIT,JVMTHR+1)
          ERRVEC(3) = MAX(RTABL(indVMTC(2),JHIT,JVMTHE),MINERR)
          ERRVEC(4) = MAX(RTABL(indVMTC(2),JHIT,JVMTHE+1),MINERR)
C
C  Check for null hits
C
          MNUL = NNUL
          MVDET = NVDET
          DO ICOL = 1, 2
            JCOL = ICOL+2
            IADDVEC(JCOL) = ITABL(indVMTC(2),JHIT,ICOL)
            IF (IADDVEC(JCOL) .LE. 0) THEN
              IF (.NOT. BLANK(JCOL)) THEN
                IADDVEC(JCOL) = -1
                MNUL = MNUL + 1
              END IF
            ELSE
              MVDET = MVDET + 1
              call vhupck(iaddvec(jcol), wafer, hit)
              CALL vadewa(wafer,ilay,ized,iphi,jview)
              if (phirow(3) .eq. iphi) then
                 errrow(jcol) = 5 + (2-iview)
              else
                 errrow(jcol) = 7 + (2-iview)
              end if
            END IF
          END DO
C
C  Add the correct diagonal terms to the error matrix, and suppress
C  the rows not being used
C
          IMIN = 0
          DO ICOL = 1, 4
            IF (IADDVEC(ICOL) .GT. 0) THEN
              IMIN = IMIN + 1
              JMIN = 0
              DO JCOL = 1, 4
                IF (IADDVEC(JCOL) .GT. 0) THEN
                  JMIN = JMIN + 1
                  ERR(JMIN,IMIN) = ERRMAT(errrow(JCOL),errrow(ICOL))
                  IF (ICOL.EQ.JCOL)
     $                 ERR(JMIN,IMIN) = ERR(JMIN,IMIN) +
     &                 ERRVEC(ICOL)**2
                END IF
              END DO
            END IF
          END DO
C
C  Invert the matrix if its non-trivial
C
          IF (IMIN .GT. 0) THEN
            do icol = 1, 4
                do jcol = 1, 4
                    err0(jcol,icol) = err(jcol,icol)
                enddo
            enddo
            CALL DSINV(IMIN, ERR, 4, IERR)
            IF (IERR .NE. 0) GOTO 800
C
C  Compute the chisquared
C
            CHISQ = 0.0
            IMIN = 0
            DO ICOL = 1, 4
              IF (IADDVEC(ICOL) .GT. 0) THEN
                IMIN = IMIN + 1
                icind(imin) = icol
                JMIN = 0
                DO JCOL = 1, 4
                  IF (IADDVEC(JCOL) .GT. 0) THEN
                    JMIN = JMIN + 1
C                    jcind(jmin) = jcol
                    CHISQ = CHISQ + RESVEC(ICOL)*RESVEC(JCOL)*
     &                   ERR(JMIN,IMIN)
                  END IF
                END DO
              END IF
            END DO
          ELSE
            CHISQ = 0.0
          END IF
C
C  Add the null hit penalty to form the 'full' chisquared
C
          FULLCHI = CHISQ + MNUL*CHINUL
          call abruev(irun,ievt)
          kvtdp = nlink('VTDP',ievt)
          nvmcn = namind(bnames(iview))
          if (kvtdp.gt.0) then
            do itrkdp = 1, iw(kvtdp)
              jtrkdp = iw(kvtdp+itrkdp)
              if (jtrkdp.eq.itrk)then
                write(iw(6),3) iview,itrk
                write(iw(6),2) chisq,fullchi,(iaddvec(icol),icol=1,4)
                write(iw(6),*) (resvec(icind(icol)),icol=1,imin)
                write(iw(6),*) 'Weight mat:',
     &           ((err(jcol,icol),jcol=1,jmin),
     &            icol=1,imin)
                write(iw(6),*) 'Error mat:',
     &           ((err0(jcol,icol),jcol=1,jmin),icol=1,imin)
 3              format(1X,'VMCHIS - View ',I1,' for track',I6)
 2              format(1X,'Chi2:',F12.5,2x,F12.5,4(2x,I10))
              endif
            enddo
          endif


C
C  Make a 'sanity check' cut on the chisquared to remove things that
C  might cause the Kalman to blow
C
          IF (FULLCHI .LT. MAXCHIS) THEN
C
C  If there are still free rows, add this to the list
C
            IPOINT = 0
            IF (LFRROW(indVMCN) .GT. 0) THEN
               IPOINT = KNEXT(indVMCN)
               IW(indVMCN + LMHROW) = IW(indVMCN + LMHROW) + 1
            ELSE
C
C  The bank is full. Replace the worst if this is better.
C
               CHIWORST = FULLCHI
               DO IROW = 1, MAXNCHI
                  IF (RTABL(indVMCN,IROW,JVMUCP) .GT. CHIWORST) THEN
                     CHIWORST = RTABL(indVMCN,IROW,JVMUCP)
                     IPOINT = KROW(indVMCN,IROW)
                  END IF
               END DO
            END IF
C
            IF (IPOINT .GT. 0) THEN
C
C  Fill the VM(U,W)C bank; we use the VMUC hac parameters, as
C  the two are identical.
C
              RW(IPOINT+JVMUCP) = FULLCHI
              RW(IPOINT+JVMUCK) = CHISQ
C  hit addresses
              DO ICOL = 1, 4
                IW(IPOINT+JVMUHW+ICOL-1)   = IADDVEC(ICOL)
              END DO
C  Pulseheight
              RW(IPOINT+JVMUPH)   = RTABL(indVMTC(1),IHIT,JVMTPH)
              RW(IPOINT+JVMUPH+1) = RTABL(indVMTC(1),IHIT,JVMTPH+1)
              RW(IPOINT+JVMUPH+2) = RTABL(indVMTC(2),JHIT,JVMTPH)
              RW(IPOINT+JVMUPH+3) = RTABL(indVMTC(2),JHIT,JVMTPH+1)
            END IF
          END IF
C
C end of loop over layer 2
C
 800      CONTINUE
        END DO
C
C end of loop over layer 1
C
      END DO
C
 999  CONTINUE
C
C  Remake the VM(U,W)C bank to size
C
      IF (indVMCN .eq. 0 .or. LROWS(indVMCN) .EQ. 0) THEN
C
C No combinations found. If the bank exists, create a single "no hit" row.
C
         call RERROR('VMCHIS', 5,' No combinations for a track')
         indVMCN = NBANK(BNAMES(IVIEW),ITRK, LMHLEN + LVMUCA)
         if (indVMCN .eq. 0) then
            call RERROR('VMCHIS', -6,' Out of memory')
         else
            IW(indVMCN + LMHCOL) = LVMUCA
            IW(indVMCN + LMHROW) = 1
            CALL VZERO(RW(indVMCN + LMHROW + 1), LVMUCA)
         end if
      else
C shrink the bank.         
         indVMCN = NBANK(BNAMES(IVIEW),ITRK,
     $        LMHLEN + LCOLS(indVMCN)*LROWS(indVMCN))
         call abruev(irun,ievt)
         kvtdp = nlink('VTDP',ievt)
         nvmcn = namind(bnames(iview))
         if (kvtdp.gt.0) then
            do itrkdp = 1, iw(kvtdp)
                jtrkdp = iw(kvtdp+itrkdp)
                if (jtrkdp.eq.itrk)then
                    write(iw(6),1) bnames(iview),jtrkdp
 1                  format(1X,'VMCHIS - Bank ',A4,' for track',I6)
                    call bprtab(nvmcn,indvmcn,1,lrows(indvmcn))
                endif
            enddo
         endif
      END IF
C
      RETURN
      END
#endif
