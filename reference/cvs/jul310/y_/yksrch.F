      Subroutine YKSrch(SrchVD)
C----------------------------------------------------------------------
C! Main Kink search routine
CKEY YKINK KINK
C
C   Author: Paul Rensing   4-5-95
C
C   Modified: Dave Casper  17-4-97  Store a record of what mass cuts
C                                   are passed in YKNK
C                          08-8-97  Allow more than one bit to be set in
C                                   the hypothesis mask, if candidate
C                                   passes more than one cut; if is INCOMING
C                                   to two kink candidates, save only the
C                                   best candidate (smallest chi^2) to avoid
C                                   confusing DALI, ENFLW, etc.  Also drop
C                                   output bank at entry, if present.
C                                   
C
C This routine looks through all pairs of tracks. Each pair is tested as
C     a possible kink. First, a simple calculation is done to see if the
C     tracks for a vertex. If they do, an vertex fit is made. Cuts are
C     made on the charge of the two tracks, the quality of the vertex
C     (before and after fitting), and the match with a known kink
C     species. The result is the YKNK bank.
C
C INPUT:
C   LOGICAL SrchVD  - if .TRUE., look in VDET for free hits which match
C
C OUTPUT:
C   Bank YKNK,0 is created and filled
C
C-------------------------------------------------------------
#ifndef DOC
      implicit none
C
C Function call parameters
C
      Logical SrchVD
C
#include "bcs.h"
#include "rconds.h"
      REAL FIELRC,ENERRC
C
#include "frftjj.h"
#include "frtljj.h"
#include "ftcljj.h"
#include "tpcojj.h"
#include "yknkjj.h"
C
#include "yknkcm.h"
#include "yknkpt.h"
C
C Parameters
C
C
C Maximum radius for any hit on inner track. Same reason as for MTPCCRD.
      Real MRADIN
      Parameter (MRADIN = 155.0)

C Functions

      integer NAMIND, NBANK, NLINK
      real YKNeM2

      Integer NamFRFT, NamFRTL, namYKNK, namTPCO, namFTCL
      integer IndFRFT, IndFRTL, IndYKNK, indTPCO, indFTCL
      integer NamYKCI, NamYKSP

      integer ntrk, inner, outer
      integer jInner, jOuter, jYKNK
      real qq, delqq
      integer rc
      real vertex(3), vertexCov(6), minDistXY, minDistZ
      integer nBadHits
      integer dummy
      real psi, x(3), covx(3,3), p(3), covp(3,3)
      integer i, j, k, nc
      integer vdhits(8)
      logical keep
      real nMass2
      real cutrad
      integer igarb, offset
      integer irun, ievt, lrun
      integer kmask
      real chi2BestIn

      integer NRowYKNK

      external NAMIND, NBANK, NLINK, YKNEM2

#include "bmacrod.h"

      DATA NamFRFT/0/
      DATA NRowYKNK/20/
      DATA lrun/-1/

#include "bmacro.h"

      IF ( NamFRFT .LE. 0 ) THEN
        NamFRFT = NAMIND('FRFT')
        NamFRTL = NAMIND('FRTL')
        NamTPCO = NAMIND('TPCO')
        NamFTCL = NAMIND('FTCL')
        NamYKNK = NAMIND('YKNK')
        NamYKCI = NAMIND('YKCI')
        NamYKSP = NAMIND('YKSP')
      END IF

C Drop output bank, if present

      Call BDROP(IW,'YKNK')

      Call ABRUEV(irun,ievt)
      if (irun.ne.lrun) then
        Call YKInit
        lrun = irun
      endif

C Check presence of needed parameter banks

      if(IW(NamYKCI).eq.0 .or. IW(NamYKSP).eq.0) return

C Create the output YKNK bank

      Call AUBOS ('YKNK', 0, NRowYKNK*LYKNKA + LMHLEN,
     $     indYKNK, IGarb)
      If (IGarb .gt. 1) then
         call RERROR('YKSrch', -1,' Out of memory')
         goto 999
      end if
      IW(indYKNK + LMHROW) = 0
      IW(indYKNK + LMHCOL) = LYKNKA

C  Find the track banks

      IndFRFT = IW(NamFRFT)
      IndFRTL = IW(NamFRTL)
      IF (IndFRFT .LE. 0 .or. IndFRTL .le. 0) THEN
        CALL RERROR('YKSrch', 2,' Cannot find track bank(s)')
        GOTO 999
      END IF

C loop over all pairs of tracks

      ntrk = LROWS(IndFRFT)
      do inner = 1, ntrk

         NC = ITABL(IndFRTL, inner, JFRTNT)

C inner track must be measured in TPC, otherwise it can't be fit (ITC
C coordinates are zeroed)

         if (NC.EQ.0) goto 20

C check that the inner track does not have any co-ordinates in spiral
C     branches.

         if (ITABL(IndFRTL, inner, JFRTNE) .gt. 0
     $        .or. ITABL(IndFRTL, inner, JFRTNR) .gt. 0) goto 20

C skip tracks which make it to the outer-most layers of the TPC
C    (obviously cannot be the inner track of a measurable kink).

         if (nc .gt. 0) then
            IndTPCO = IW(NamTPCO)
            IndFTCL = IW(NamFTCL)
            IF (IndTPCO.LE.0 .OR. IndFTCL.LE.0) THEN
               CALL RERROR('YKSrch',2,' Cannot find TPC coord bank(s)')
               GOTO 999
            END IF
            offset = ITABL(IndFRTL, inner, JFRTIT) + NC
            DO i = 1, NC
               IF (RTABL(IndTPCO, ITABL(IndFTCL, offset-I+1, 1),
     $              JTPCRV)  .gt. MRADIN) goto 20
            end do
         end if
C
C Save the best kink found for this track as incoming
C         
         chi2BestIn = -1.
         do outer = 1, ntrk
            if (inner .eq. outer) goto 10

C Check that outer track is measured in the TPC

            if (ITABL(IndFRTL, outer, JFRTNT) .lt. 4) goto 10

            jInner = KROW(IndFRFT, inner)
            jOuter = KROW(IndFRFT, outer)

C check that the two tracks have the same charge within errors
            
            qq = RW(jInner + JFRFIR) *
     $           RW(jOuter + JFRFIR)
            delqq = abs(qq) * sqrt(
     $           RW(jInner + JFRFEM) / RW(jInner + JFRFIR)**2 +
     $           RW(jOuter + JFRFEM) / RW(jOuter + JFRFIR)**2 ) 
            if (qq / delqq .lt. firstChgCut) goto 10

C load the co-ordinates of the inner track into the common block
C This is done here, instead of before the inner loop, because the list 
C    of hits can be changed when a reasonable candidate is found.

            call YKLdC(inner, .FALSE., nptsIn, rIn, uIn, zIn, sigUIn,
     $           sigZIn, vdhits, nvdet, wafers, uVDet, wVDet)

C load the co-ordinates of the outer into the common block
C With 2nd argument .TRUE., vdet variable are not changed.

            call YKLdC(outer, .TRUE., nptsOut, rOut, uOut, zOut,
     $           sigUOut, sigZOut,
     $           vdhits, nvdet, wafers, uVDet, wVDet)

C do a simple kink fit and cut on miss distance and number of hits on
C     wrong side of kink.

            Call YKSFit(RW(jInner + JFRFIR), RW(jInner + JFRFEM),
     $           RW(jOuter + JFRFIR), RW(jOuter + JFRFEM),
     $           vertex, vertexCov, minDistXY, minDistZ, nBadHits, rc)
            if (rc .ne. 0) goto 10

C check that there is enough space

            if (LFRROW(indYKNK) .lt. 1) then
               call rerror('YKSrch', 3,' Had to expand YKNK bank')
               NRowYKNK = NRowYKNK + 10
               Call AUBOS ('YKNK', 0, NRowYKNK*LYKNKA + LMHLEN,
     $              indYKNK, IGarb)
               If (IGarb .eq. 1) Then
                  IndFRFT = IW(NamFRFT)
                  IndFRTL = IW(NamFRTL)
               Else If (IGarb .gt. 1) then
                  call RERROR('YKSrch', -4,' Out of memory')
                  goto 999
               end if
            end if

C store the result

            jYKNK = KNEXT(indYKNK)
            IW(jYKNK + JYKNIT) = inner
            IW(jYKNK + JYKNOT) = outer

            call ucopy(vertex, RW(jYKNK + JYKNVX), 3)
            call ucopy(vertexCov, RW(jYKNK + JYKNVC), 6)

            RW(jYKNK + JYKNMX) = minDistXY
            RW(jYKNK + JYKNMZ) = minDistZ

            call ucopy(RW(jInner + JFRFIR), RW(jYKNK + JYKNHI), 5)
            call ucopy(RW(jInner + JFRFEM), RW(jYKNK + JYKNHC), 15)

C Search for VDET hits consistent with the inner track. If the inner
C     track does not have enough z hits, the kink vertex and/or the
C     primary vertex is (temporarily) added to the track.
C     If the search is successful, the helix parameters with the
C     constraints included are return (as input for later fits), vdhits
C     represents the list of found hits, and the YKNKPT common is
C     updated to include the found points.
C
C This code commented out because D.Casper thinks it is not useful
C Left here in case we want to reimplement it in future
C
C            if (SrchVD) Then
C               if (NVDet .eq. 0) then
C                  call YKVDet(fielrc, vertex, vertexCov,
C     $                 RW(jYKNK + JYKNHI), RW(jYKNK + JYKNHC),
C     $                 vdhits)
C for safety
C                  indYKNK = IW(namYKNK)
C                  jYKNK = KNEXT(indYKNK)
C               end if
C            end if
            call ucopy(vdhits, IW(jYKNK + JYKNVH), 8)

C do the final good fit

            call YKFFit(fielrc, LROWS(indYKNK) + 1, rc)
            if (rc .ne. 0) goto 10
C for safety
            indYKNK = IW(namYKNK)
            jYKNK = KNEXT(indYKNK)

C chi^2 cut
            if (RW(jYKNK + JYKNC2) .gt. Chi2VtxCut) goto 10

C mass cut: loop over all kink species and keep the candidate if the
C     neutral mass is within the window of any neutral decay product.

            keep = .false.
            i = 1
            kmask = 0
            do while (i .le. ykNspec)
               nMass2 = YKNeM2(IW(indYKNK + LMHROW) + 1,
     $              YKParM(i), YKDauM(i))
               if (abs(nMass2 - YKNeutM(i)**2) .lt. YKMCut(i)) then
                    keep = .true.
                    kmask = ibset(kmask,i-1)
               endif
               i = i + 1
            end do

            if (keep) then
C  Only save one kink per incoming track (the best)
               if ((RW(jYKNK+JYKNC2) .lt. chi2BestIn)
     &                  .or. (chi2BestIn .lt. 0.) ) then
                    IW(jYKNK+ JYKNBH) = kmask
                    if (chi2BestIn .gt. 0.) then
C  We have to overwrite the previous best for this incoming track
                        call ucopy(RW(jYKNK+JYKNIT),
     &                      RW(jYKNK-LYKNKA+JYKNIT),
     &                      LYKNKA)
                    else
C  This row can stay
                        IW(indYKNK + LMHROW) = IW(indYKNK + LMHROW) + 1
                    end if
C  Record best chi^2 so far for this track as incoming
                    chi2BestIn = RW(jYKNK+JYKNC2)
               end if
            end if

C end of loop over outer track
 10         continue
         end do

C end of loop over inner track
 20      continue
      end do

 999  continue

C recreate the VTXT bank using the default fits in FRFT

      call vtrkex(fielrc,igarb)

C shrink the bank to size

      if (LROWS(indYKNK) .gt. 0) then
         Call AUBOS ('YKNK', 0, LROWS(indYKNK)*LYKNKA + LMHLEN,
     $        indYKNK, IGarb)
      else
         call BDROP(IW, 'YKNK')
      end if

      return
      end
#endif
