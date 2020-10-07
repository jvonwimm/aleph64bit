      subroutine qddx(itk, nhyp, rmass, Q, ns, 
     &     npad,extimator, ier)
CKEY DEDX /USER
C----------------------------------------------------------------------
C! dE/dx analysis in ALPHA
c By Tommaso Boccali, 1999
c computes the best extimation for de/dx, using wires and pads
c INPUT:     
c     itk         : ALPHA TRACK
c     nhyp        : Number of hypotheses to be tested
c     rmass       : vector(1..nhyp) of the masses of the tests
c     Q           : vector(1..nhyp) of the charge of the tests
c OUTPUT:
c     ns          : number of wires available, -1 if no wires info
c     npad        : number of pads (REAL!!!!), -1 if no pad info
c     extimator   : (ri-rexp)/sigma for each test particle
c     ier         : 1 if no information was available
c                 : 0 if an estimator was calculated
C----------------------------------------------------------------------
C
      implicit none
#include "qdecl.h"
#include "qcde.h"
      integer itk, nhyp,ier,ns
      real rmass(*),q(*), npad,extimator(*), tl
      real p,ct
      integer i,j,k
      real bestsigma, bestri
      real denom2,denom,w(2),matrix(2,2),imatrix(2,2)
      real padre(nhyp),padsg(nhyp),filre(nhyp),filsg(nhyp)
      real padri,filri,padtl,filtl
      real correlation
      real findcorr
      real temp(2)
      integer ifail,padier,filier
#include"qmacro.h"
C----------------------------------------------------------------------      
      ier=1
C
      call qpadx(itk,nhyp,rmass,q,Padri,npad,Padtl,Padre,Padsg
     &     ,Padier)
      call qdedx(itk,nhyp,rmass,q,filri,ns,filtl,filre,filsg,filier)
C
      if (filier.ne.0) ns =-1
      if (padier.ne.0) npad =-1.
c
c various cases
c
      if (padier.eq.0.and.filier.ne.0) then
         ier=0
         do i=1,nhyp
            extimator(i) = (padri-padre(i))/padsg(i)
         enddo
      endif
c
      if (filier.eq.0.and.padier.ne.0) then
         ier=0
         do i=1,nhyp
            extimator(i) = (filri-filre(i))/filsg(i)
         enddo
      endif
c
      if (padier.eq.0.and.filier.eq.0) then         
         ier=0
c
c here I can combine with blue
c     
         p = qp(itk)
         ct =qct(itk)
c        
         do k=1,nhyp
            correlation = findcorr(k,nhyp,rmass,p,ct)
            matrix(1,1) = padsg(k)**2
            matrix(2,2) = filsg(k)**2
            matrix(1,2) = correlation * padsg(k) * filsg(k)
            matrix(2,1) = matrix(1,2)
c           
            do i=1,2
               do j=1,2
                  imatrix(i,j)=matrix(i,j)
               enddo
            enddo
c           
            call rinv(2,imatrix,2,temp,ifail)
            if (ifail.ne.0) then
               ier=1
               return
            endif
c           
            denom =0.
c           
            do i=1,2
               do j=1,2
                  denom = denom +imatrix(i,j)
               enddo
            enddo
c            
            do i=1,2
               w(i)=0.
               do j=1,2
                  w(i) = w(i) + imatrix(i,j)/denom
               enddo
            enddo            
c     
c     this should be 1 by construction, but let's check..
c     
            denom2 = w(1)+w(2)
c                       
            bestsigma = 0.
            bestri = w(1)*(padri-padre(k))+w(2)*
     &           (filri-filre(k))
            do i=1,2
               do j=1,2
                  bestsigma = bestsigma +w(i)*matrix(i,j)*w(j)/denom2
               enddo
            enddo
            bestsigma = sqrt(bestsigma)
            extimator(k) =  bestri/bestsigma            
         enddo
c        
      endif
c     
      return
      end
      real function findcorr(k,nhyp,rmass,p,ct)
C----------------------------------------------------------------------
C Auxiliary to qddx
C T. Boccali  March 1999
C----------------------------------------------------------------------
      implicit none
#include "qdecl.h"
#include "qcde.h"
      integer k,nhyp
      real ct
      real correlation
      real rmass(*)
      real p
      integer findindexct,findindexbg
c
c  parameterization:
c
c 1 - betagamma
c 2 - cos(theta)
C     
      real correH(3,2)
      real corree(2)
      real bg
      logical goodwindow
      integer ict,inbg
c
c binning:
c
      data correH /0.53, 0.48, 0.51, 0.35, 0.34, 0.44/ 
      data corree /0.43, 0.36/
#include "qmacro.h"
C----------------------------------------------------------------------
      bg = log10(p/rmass(k))
      ict = findindexct(ct)
      inbg = findindexbg(bg)      
c
c look if it's an electron first. 
C In that case use specific parameterization
c
      if (goodwindow(rmass(k),qcmass(3))) then
         correlation = correE(ict)         
      else                                 
         correlation = correH(inbg,ict)
      endif
C         
      findcorr= correlation
C      
      return
      end      
      integer function findindexct(ct)
C----------------------------------------------------------------------
C Auxiliary to findcorr - qddx 
C T. Boccali  March 1999
C----------------------------------------------------------------------
      implicit none
      real ct
      integer temp
C----------------------------------------------------------------------
      temp=0
c      
      if (abs(ct).lt..75) then
         temp=1
      else
         temp=2   
      endif
c
      findindexct = temp
c      
      return
      end
      integer function findindexbg(bg)
C----------------------------------------------------------------------
C Auxiliary to findcorr - qddx
C T. Boccali  March 1999
C----------------------------------------------------------------------
      implicit none
      real  bg
      integer temp
C----------------------------------------------------------------------
      temp=0      
      if (bg.lt.0.) then
         temp=1
      elseif(bg.ge.0.and.bg.lt..5) then 
         temp=2
      else
         temp=3
      endif
c
      findindexbg = temp
c      
      return
      end
      logical function goodwindow(mass1,mass2)
C----------------------------------------------------------------------
C Auxiliary to findcorr - qddx
C T. Boccali  March 1999
C----------------------------------------------------------------------
      implicit none
      real mass1, mass2
C----------------------------------------------------------------------
      goodwindow=.false.
      if ((mass1*1.1).gt.mass2.and.(mass1*.9).lt.mass2)  
     &     goodwindow=.true.
      return
      end
