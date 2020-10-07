      Subroutine YKSFit(helInner, covInner, helOuter, covOuter,
     $     vertex, vertexCov, minDistXY, minDistZ, nBadHits, rc)
C----------------------------------------------------------------------
C! Simple fit of two tracks to seach for a kink
CKEY YKINK KINK
C
C Fit for a kink using only the pre-existing helix information.
C   If there is more than one crossing point for the two helices, 
C   the chosen point is the one with the fewest "bad" hits (hits on the
C   wrong side of the kink), with distance of closest approach as a
C   second arbitrator. Cuts are made to ensure that the kink position
C   is within the tracking volume.
C   
C   Input:      helInner    Helix parameters of inner track
C               covInner    Error matrix for inner track
C               helOuter    Helix parameter of outer track
C               covOuter    Error matrix for outer track
C   Output:     vertex      Candidate decay position
C               vertexCov   Error matrix of vertex
C               minDistXY   Closest distance of approach in XY
C               minDistZ    Closest distance of approach in Z
C               nBadHits    Total number of hits assigned to these
C                           tracks which are inconsistent with the
C                           reconstructed vertex
C               rC          Status return code; 0 if no error
C
C   Author: Paul Rensing   4-5-95
C   Modified: Dave Casper  8-4-97  Calculation of vertex covariant matrix
C                                  not working as intended - call YTOP
C                                  instead.
C   Modified: M.Cattaneo  13-5-97  Call to yftvtx with identical input and 
C                                  output vertex array was not working, because
C                                  yftvtx initialises output to zero before
C                                  input is looked at.
C
C             D.Casper    03-5-97  Check basic quality cuts (minimum
C                                  distance and #bad hits) here rather
C                                  than in calling routine, to avoid
C                                  asking YTOP for impossible fits
C-------------------------------------------------------------
#ifndef DOC
      implicit none

#include "alcons.h"

      real helInner(5), helOuter(5)
      real covInner(15), covOuter(15)
      
      real vertex(3), vertexCov(6)
      real vertexOut(3)
      real minDistXY, minDistZ
      integer ihit, nBadHits
      integer rc

C hard coded upper limit of number of bad hits
      integer MAXBHITS
      parameter (MAXBHITS = 10)

#include "yknkcm.h"
#include "yknkpt.h"
      
      integer YKBHit

      Real rho1, d01, k1
      Real rho2, d02, k2
      Real x1,y1, x2,y2, x3,y3, x4,y4
      Integer ind
      Real x(3), y(3), r(3), minD(3)
      Integer nposs
      Real radIn(3), radOut(3)
      real qin, qout
      integer found
      Real pos(2), posCov(3)
      Real z, sum
      Real delz, t
      Integer delzOK
      integer i, nbad, iret
      real helBuf(5,2),covBuf(15,2),dum,chi2
      integer helIndex(2),ifail,idum

C Use YDISMC to calculate the candidate point in the XY plane.
C YDISMC uses strange helix parameters.

      qin = sign(1.0, -helInner(1))
      qout = sign(1.0, -helOuter(1))
      rho1 = abs(1.0 / helInner(1))
      d01 = -qin * helInner(4)
      k1 = -qin / (rho1 - d01)
      rho2 = abs(1.0 / helOuter(1))
      d02 = -qout * helOuter(4)
      k2 = -qout / (rho2 - d02)
      call YDISMC(k1, d01, helInner(3), helInner(5), helInner(2),
     $     qin, k2, d02, helOuter(3), helOuter(5),
     $     helOuter(2), qout, x1, y1, x2, y2, x3, y3,
     $     x4, y4, ind)
   

      nposs = 0
      if (ind .eq. 6) then
         rc = -1
         return
      else if (ind .eq. -1 .or. ind .eq. -2) then
         nposs = nposs + 1
         x(nposs) = (x1+x2)/2.0
         y(nposs) = (y1+y2)/2.0
         r(nposs) = sqrt( x(nposs)**2 + y(nposs)**2 )
         radIn(nposs) = sqrt(x1*x1 + y1*y1)
         radOut(nposs) = sqrt(x2*x2 + y2*y2)
         minD(nposs) = sqrt( (x1-x2)**2 + (y1-y2)**2 )
      else
         if (ind .eq. 1) then
            
C there are two crossing point plus the symmetry line
            nposs = nposs + 1
            x(nposs) = x2
            y(nposs) = y2
            r(nposs) = sqrt(x2*x2 + y2*y2)
            radIn(nposs) = r(nposs)
            radOut(nposs) = r(nposs)
            minD(nposs) = 0.0
      
C symmetry line
            nposs = nposs + 1
            radIn(nposs) = sqrt(x3*x3 + y3*y3)
            radOut(nposs) = sqrt(x4*x4 + y4*y4)
            x(nposs) = (x3 + x4)/2.0
            y(nposs) = (y3 + y4)/2.0
            r(nposs) = sqrt(x(nposs)**2 +y(nposs)**2)
            minD(nposs) = sqrt( (x3-x4)**2 + (y3-y4)**2 )
         end if

C first crossing point
         nposs = nposs + 1
         x(nposs) = x1
         y(nposs) = y1
         r(nposs) = sqrt(x1*x1 + y1*y1)
         radIn(nposs) = r(nposs)
         radOut(nposs) = r(nposs)
         minD(nposs) = 0.0
      end if

      nBadHits = 1000000
      minDistXY = 1.0e32
      minDistZ = 1.0e6
      call vzero(vertexCov, 6)

      found = -1
      do i = 1, nposs
         If (r(i) .gt. fidOuterRad .or. r(i) .lt. fidInnerRad)
     $        goto 10
C count the number of hits on each track which are inconsistent with this
C vertex (i.e. hits on the inner track outside the vertex, hits on the outer
C track inside the vertex.

         nbad = 0
         do ihit = 1, nptsIn
            if (rIn(ihit) .gt. r(i)) nbad = nbad + 1
         end do
         do ihit = 1, nptsOut
            if (rOut(ihit) .lt. r(i)) nbad = nbad + 1
         end do

C is the number of bad hits at all reasonable?
         If (nbad .gt. MAXBHITS) goto 10

         If ( nbad .gt. nBadHits .or. (nbad .eq. nBadHits .and.
     $        minD(i) .ge. minDistXY) )
     $        goto 10
      
         z = 0.0
         sum = 0.0
         delz = 0.0
         delzOK = 0

C compute the phi and z co-ordinates along the inner helix at the radius
C     of the kink.

         call uhelix2(radIn(i), helInner, covInner, pos, posCov, iret)
         if (iret .ne. 0) goto 10

C check that the computed phi position (which, by construction, is on
C     the first 1/2 arch of the helix) agrees with the desired vertex
C     position.

         t = abs(pos(1) - atan2(y(i), x(i)))
         do while (t .ge. PI) 
            t = t - TWOPI
         end do
         If (abs(t) .gt. 0.05) goto 10

C if the z calculation contains information, use it. 

         If (posCov(3) .gt. 0.0 .and. posCov(3) .lt. 1.0e3) Then
            delz = pos(2)
            delzOK = delzOK + 1
            z = z + pos(2)/posCov(3)
            sum = sum + 1.0/posCov(3)
         end if

C now the outer track.

         call uhelix2(radOut(i), helOuter, covOuter, pos, posCov, iret)
         if (iret .ne. 0) goto 10

         t = abs(pos(1)-atan2(y(i), x(i)))
         do while (t .ge. PI)
            t = t - TWOPI
         end do
         If (abs(t) .gt. 0.05) goto 10

         If (posCov(3) .gt. 0.0 .and. posCov(3) .lt. 1.0e3) Then

C if the inner-most point are in the TPC, then the z
C    uncertainty is overestimated because the MS between the last point
C    and the IP is included (and should not be). (This does not have 
C    to be exact.)
            if (radOut(i) .gt. 30.0 .and. rOut(nptsOut) .gt. 30.0
     $           .and. posCov(3) .gt. 25.0) then
               posCov(3) = posCov(3) / 4.0
            end if

            delz = delz - pos(2)
            delzOK = delzOK + 1
            z = z + pos(2)/posCov(3)
            sum = sum + 1.0/posCov(3)
         end if

C compute the average z position

         If (sum .gt. 0.0) Then
            z = z / sum
         else
            goto 10
         end if
         If (abs(z) .gt. fidMaxZ .or. 
     $      ((delzok.eq.2).and.(abs(delz).gt.minDistZ))) goto 10

         nBadHits = nbad
         minDistXY = minD(i)
         If (delzOK .eq. 2) Then
            minDistZ = abs(delz)
         else
            minDistZ = 0.0
         end if
         found = i
        
         vertex(1) = x(i)
         vertex(2) = y(i)
         vertex(3) = z
 10      continue
      end do

C did we find a vertex?

      If ((found .lt. 0) .or. 
     $          (nBadHits.gt.maxBadHits) .or.
     1          (minDistXY.gt.minDistXYCut) .or.
     2          (minDistZ.gt.minDistZCut)) Then 
         Rc = -2
         return
      end if

C Do a fast fit of the vertex using YTOP

      call ucopy(helInner,helBuf(1,1),5)
      call ucopy(helOuter,helBuf(1,2),5)
      call ucopy(covInner,covBuf(1,1),15)
      call ucopy(covOuter,covBuf(1,2),15)
      helIndex(1) = 1
      helIndex(2) = 2
      call yftvtc(0,2,0,0,.false.,vertex,dum,
     &  helIndex,5,15,helBuf,covBuf,
     &  idum,idum,idum,dum,dum,
     &  idum,idum,idum,dum,dum,
     &  vertexOut,vertexCov,chi2,ifail)

      vertexOut(1) = vertex(1)
      vertexOut(2) = vertex(2)
      vertexOut(3) = vertex(3)

      if (ifail.ne.0 .or. chi2.gt.chi2VtxCut) then
        rc = -3
        return
      endif

      Rc = 0
      return
      end
#endif
