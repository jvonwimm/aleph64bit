        SUBROUTINE YHELIX(IFRFT,POINT,SIGZ2,PSI,PTRK,IER)
*************************************************************
C!  Determine the turning angle and momentum of an FRFT track at a point
C
C               D. Casper   10-Aug-1997
C
C       Input:  IFRFT       Row number in FRFT bank
C               POINT(3)    Point on or near the track
C               SIGZ2       Z uncertainty of POINT (squared)
C          
C       Output: PSI         Turning angle at the point
C               PTRK(3)     Momentum vector at vertex
C               IER         0 if no error
C                           -1 if no track bank or 0 field
C
C   This routine generalizes UHELIX to deal the more complicated and
C   ambiguous situations which arise in secondary interactions.
C   The test relies on the fact that all the coordinates on a track
C   must lie between psi = 0 and psi = pi (by definition).  Ambiguities are
C   resolved using z information if possible.
*************************************************************
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"

C arguments
        real point(3), ptrk(3)
C output of UHELIX 
        real puh(3), pcovuh(3,3), xuh(3), xcovuh(3,3)
        real lam

        data nfrft/0/
#include "bmacro.h"

C initialize
        ier = -1
        field = alfiel(dum)
        if (field.eq.0) return
        pscale = ((clght/100.)*(field/10.))/100.
        if (nfrft.eq.0) then
            nfrft = namind('FRFT')
            nfrtl = namind('FRTL')
        endif
        kfrft = iw(nfrft)
        kfrtl = iw(nfrtl)
        if (kfrft.eq.0 .or. kfrtl.eq.0) return 
C track parameters
        rin = -rtabl(kfrft,ifrft,jfrfir)
        lam = rtabl(kfrft,ifrft,jfrftl)
        fi0 = rtabl(kfrft,ifrft,jfrfp0)
        d0  = rtabl(kfrft,ifrft,jfrfd0)*sign(1.,-rin)
        z0  = rtabl(kfrft,ifrft,jfrfz0)
        q   = sign(1.,rin)
        ro  = q/rin
        sf0 = sin(fi0)
        cf0 = cos(fi0)

C See what UHELIX says

        call uhelix(ifrft,3,rxy,psi,point,field,
     &      xuh,xcovuh,puh,pcovuh,ieruh)

C About half the time, UHELIX gives a psi which is out of range (-pi,pi).

        if (psi.lt.-pi) then
            psi = psi + 2*pi
        else if (psi.gt.pi) then
            psi = psi - 2*pi
        endif

C UHELIX always returns psi, and only sets error flag if covariance matrix is 
C singular - who cares?  Because of the above "feature", though, calculate
C the momentum over

C        if (ieruh.ne.0) then
            puh(1) = ro*cos(q*psi-fi0)
            puh(2) = -ro*sin(q*psi-fi0)
            puh(3) = ro*lam
            xuh(1) = q*(ro-d0)*sf0-q*puh(2)
            xuh(2) = -q*(ro-d0)*cf0+q*puh(1)
            xuh(3) = z0+psi*puh(3)
            puh(1) = puh(1)*pscale
            puh(2) = puh(2)*pscale
            puh(3) = puh(3)*pscale
            if (ieruh.ne.0) call vzero(xcovuh,9)
C        endif

C Make sure UHELIX didn't blow it by checking the point it gave us with what
C we gave it.  Only works for tracks with z information which are not so
C steep that z doesn't change.

        if (itabl(kfrtl,ifrft,jfrtnt).ge.3) then
C  See how much the track moves in z over a full circle
          deltaz_2pi = 2*pi*puh(3)/pscale
C  Can we resolve this change given the uncertainty in the vertex?
C  (Note: this implicitly protects against numerical problems if
C  lambda and pz are small)
          if (deltaz_2pi**2.gt.(abs(sigz2)+abs(xcovuh(3,3)))) then
C  If so, see whether the input and output z coordinates agree
            deltaz = point(3) - xuh(3)
            deltapsi = pscale*deltaz/puh(3)
C  If psi is wrong, it should be wrong by +/-2pi
            if (deltapsi.lt.-pi) then
              call abruev(krun,kevt)
D              type *,'Adjusting psi:',deltapsi,point,krun,kevt,ifrft
              deltapsi = -2*pi
            else if (deltapsi.gt.pi) then
              call abruev(krun,kevt)
D              type *,'Adjusting psi:',deltapsi,point,krun,kevt,ifrft
              deltapsi = 2*pi
            else
              deltapsi = 0.
            endif
            if (deltapsi.ne.0.) then
              psi = psi+deltapsi
              puh(1) = ro*cos(q*psi-fi0)
              puh(2) = -ro*sin(q*psi-fi0)
              puh(3) = ro*lam
              xuh(1) = q*(ro-d0)*sf0-q*puh(2)
              xuh(2) = -q*(ro-d0)*cf0+q*puh(1)
              xuh(3) = z0+psi*puh(3)
              puh(1) = puh(1)*pscale
              puh(2) = puh(2)*pscale
              puh(3) = puh(3)*pscale
            endif
          endif
        endif
        call ucopy(puh,ptrk,3)
        ier = 0
        return
        end
#endif
