      SUBROUTINE VFtAll(FIELD, IFRFT, ISWIM)
C ---------------------------------------------------------------------
C! Refit all the tracks with Kalman, including MS to origin
CKEY VGLOB VDET TRACK
C
C      Paul Rensing   - 4-5-95
C
C Loop through all the tracks in the given FRFT bank and refit them. If
C     a fit fails, use the existing fit, but add the extra uncertainty
C     due to propogating the track from the last point to the origin.
C     (if ISWIM = 1)
C 
C Tracks identified as originating at a secondary vertex have their
C errors and energy loss propagated only to the corresponding point.
C
C  INPUT:
C     REAL FIELD - the magnetic field
C     INTEGER IFRFT - the FRFT bank number to fill
C     INTEGER ISWIM - swim flag for Kalman 
C                       0 = helix at innermost co-ordinate
C                       1 = helix at origin of co-ordinate system
C OUTPUT:
C      FRFT number IFRFT is modified
C ------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C Function parameters
C
      REAL FIELD
      INTEGER IFRFT, ISWIM
C
C  Global includes
C
#include "bcs.h"
C
#include "frftjj.h"
#include "frtljj.h"
#include "vdcojj.h"
#include "fxtrjj.h"
#include "ylv0jj.h"
#include "ynlijj.h"
#include "ynvhjj.h"
#include "yknkjj.h"
C
C  Function calls
C
      INTEGER NLINK, NBANK, NAMIND, NDROP, UFTTRA, UFSWIM, UFTRAD
      EXTERNAL NLINK, NBANK, NAMIND, NDROP, UFTTRA, UFSWIM, UFTRAD
      INTEGER ICODE,IRET
C
C  Local variables
C
      integer namFRTL, namFVCL, namFTCL, namFICL, namVDCO
      integer namFXTR, namYLV0, namYNLI, namYNVH, namYKNK
C
      INTEGER indFRFT, indFRTL, indFVCL
      integer indFTCL, indFICL, indVDCO
      integer indFXTR, indYLV0, indYNLI, indYNVH, indYKNK
      integer jFRFT, jfrtl
      INTEGER itrk, j, iRow, iVert
      integer idummy, igarb
      real dummy
      logical rebuild
      integer nVDCO
C
C Parameter for the Kalman filter
C
      REAL VV0(6),CC0(21)
      REAL CHI2, startRad
      INTEGER NDOF
C
      save namFRTL, namFVCL, namFTCL, namFICL, namVDCO
      save namFXTR, namYLV0, namYNVH, namYNLI, namYKNK
C
#include "bmacrod.h"
C
C Data statements
C
      DATA namFRTL/0/
C
C  Inline functions
C
#include "bmacro.h"
C
      IF (namFRTL .eq. 0) THEN
         namFRTL = NAMIND('FRTL')
         namFVCL = NAMIND('FVCL')
         namFTCL = NAMIND('FTCL')
         namFICL = NAMIND('FICL')
         namVDCO = NAMIND('VDCO')
         namFXTR = NAMIND('FXTR')
         namYLV0 = NAMIND('YLV0')
         namYNVH = NAMIND('YNVH')
         namYNLI = NAMIND('YNLI')
         namYKNK = NAMIND('YKNK')
      END IF
C
      rebuild = .FALSE.
C
      indFRFT = NLINK('FRFT', IFRFT)
      indFVCL = IW(namFVCL)
      indFRTL = IW(namFRTL)
      indFTCL = IW(namFTCL)
      indFICL = IW(namFICL)
      indFXTR = IW(namFXTR)
      indYLV0 = IW(namYLV0)
      indYNLI = IW(namYNLI)
      indYNVH = IW(namYNVH)
      indYKNK = IW(namYKNK)
      IF (indFRFT .EQ. 0 .OR. indFRTL .EQ. 0 .OR. 
     $     indFVCL .EQ. 0 .OR. indFICL .EQ. 0.OR. indFTCL .EQ. 0) THEN
         CALL RERROR('VFTALL', 1, ' Cannot find track banks')
         GOTO 999 
      END IF
C
C-- setup the KALMAN filter
C
      iret = UFSWIM(ISWIM)
C
C Loop over the tracks in the event
C
      DO itrk = 1, LROWS(indFRFT)
         jFRTL = KROW(indFRTL,itrk)
C
C do not fit ITC only tracks
C         
         if (IW(jFRTL+JFRTNV)+IW(jFRTL+JFRTNT) .gt. 2) then
            jFRFT = KROW(indFRFT, itrk)
C
C see if the track originates in a secondary vertex
C
            if (indFXTR .gt. 0) then
                startRad = 0.
                if (ITABL(indFXTR,itrk,jfxtv0).ne.0) then
C look for track in V0 bank
                  if (indYLV0 .gt. 0) then
                    iRow = 1
                    iVert = 0
                    do while (iRow.le.LROWS(indYLV0) .and. iVert.eq.0)
                      if (ITABL(indYLV0,iRow,jylvk1).eq.itrk .or.
     &                        ITABL(indYLV0,iRow,jylvk2).eq.itrk) then
                        iVert = iRow
                        startRad = sqrt(RTABL(indYLV0,iRow,jylvvx)**2 +
     &                        RTABL(indYLV0,iRow,jylvvy)**2)
                      else
                        iRow = iRow + 1
                      endif
                    enddo
                  end if
                else if (itabl(indFXTR,itrk,jfxtnc).ne.0) then
C look for track in nuclear bank
                else if (itabl(indFXTR,itrk,jfxtkn).ne.0) then
C look for track in kink bank
                endif
                iret = UFTRAD(startRad)
            endif
C
C call ufttra to refit the track
C
            icode = UFTTRA(itrk, field,
     $           RW(jFRFT+JFRFIR), RW(jFRFT+JFRFC2),
     $           IW(jFRTL+JFRTNT), IW(jFRTL+JFRTNI), IW(jFRTL+JFRTNV),
     $           IW(indFTCL+LMHLEN+IW(jFRTL+JFRTIT)+1),
     $           IW(indFICL+LMHLEN+IW(jFRTL+JFRTII)+1),
     $           IW(indFVCL+LMHLEN+IW(jFRTL+JFRTIV)+1),
     $           VV0,CC0,CHI2,NDOF)
            iret = UFTRAD(0.)
            if (icode.eq.0 .and. chi2 .lt. 1.0E10) then
C
C no error. Put in the new track parameters.
C
               DO j = 1,6
                  RW(jFRFT+JFRFIR+j-1) = VV0(j)
               end do
               DO j = 1,21
                  RW(jFRFT+JFRFEM+j-1) = CC0(j)
               end do
               RW(jFRFT+JFRFC2) = CHI2
               IW(jFRFT+JFRFDF) = NDOF
               IW(jFRFT+JFRFNO) = 90
            else
C
C Kalman fit failed. If this track has VDET hits, something is very wrong.
C
               if (IW(jFRTL+JFRTNV) .gt. 0) then
                  call RERROR('VFTALL',icode,'Kalman Filter error')
                  indVDCO = IW(namVDCO)
                  do j = 1, IW(jFRTL+JFRTNV)
                     IW(KROW(indVDCO,
     $                    IW(indFVCL+LMHLEN+IW(jFRTL+JFRTIV)+j))
     $                    +JVDCTN) = 10000
                  end do
                  IW(jFRTL+JFRTNV) = 0
                  j = NDROP('VCPL',itrk)
                  rebuild = .TRUE.
               end if
C
C use old result but add MS error to primary vertex if called for
C
               if (ISWIM .eq. 1) then
                  CALL VDMSUP2(ITrk, -1, idummy, dummy, dummy, IGARB)
                  CALL UFTMSO(ITrk, CC0)
C
C This is the CHANGE in the covariance matrix
C
                  DO J = 1, 15
                     RW(jFRFT + JFRFEM + J -1) =
     $                    RW(jFRFT + JFRFEM + J - 1) + CC0(J)
                  END Do
               end if
            end if
         end if
C               
C end of loop over tracks
C
      end do
C
C Rebuild the VDCO and FVCL banks if needed. This should be rare!
C
      if (rebuild) then
C SORTI maintains order of rows which have the same track number. This
C    will preserve the match between the rows in VDCO and VCPL.
         CALL SORTI(RW(indVDCO+LMHLEN+1),LCOLS(indVDCO),
     $        LROWS(indVDCO),JVDCTN)
         nVDCO = 0
         DO itrk = 1, LROWS(indFRFT)
            IW(KROW(indFRTL,itrk) + JFRTIV) = nVDCO
            nVDCO = nVDCO + ITABL(indFRTL,itrk,JFRTNV)
         end do
         IW(indVDCO + LMHROW) = nVDCO
         IW(indFVCL + LMHROW) = nVDCO
         call AUBPRS('VDCOFVCL')
      end if
C
 999  CONTINUE
C
C-- put the error matrix calculation back at the innermost coordinates
C
      iret = UFSWIM(0)
C
      return
      end
#endif
