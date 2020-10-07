      subroutine bcaldata(ebeam,imod,pmt,apd,sil,
     .                    e,th,ph,Rcn,Phcn,ierr)
c
c Author : G. Merino - 19970424
c
c Description: This subroutine returns information of the 
c              tagged electron 3-momenta in the IP taking as
c              input the raw data information. 
c
c Inputs:   
c          ebeam = Nominal beam energy [REAL] (GeV)
c          imod = BCAL++ module number [INTEGER] 
c          pmt = PMT Signal [REAL] (GeV)  
c          apd = APD Signal [REAL] (GeV)  
c          sil(16,6) = Silicon signals <-> (Radial,Azimuthal) [INTEGER] (ADC counts)  
c
c Outputs: e = Measured energy [REAL] (GeV) 
c          th = Measured Theta angle in the IP [REAL] (Rad)
c          ph = Measured Phi angle in the IP [REAL] (Rad) 
c          Rcn = Radial centroid [REAL] (cm)
c          Phcn = Azimuthal centroid [REAL] (degrees)
c          ierr = Error Flag : 0=> No error; 1=>Some problem in the reconstruction  
c
c Modifications:
c
c   19971210 G.Merino = Add Rcen and Phcen as Outputs
c                              
      implicit none
c
      integer i,j
      integer ierr,imod,sil(16,6)
      real pmt,apd
      integer rmax,phmax,epad1,epad2,esilc
      real e,th,ph
      real rcen,phcen,sndmom
      real r,p
      real ebeam,xbcal(2),pIP(3)
      real Rcn,Phcn,pi
c
      pi = acos(-1.)
      e = 0.
      th = 0.
      ph = 0.
      Rcn = 0.
      Phcn = 0.
      ierr = 0
c
c Process the silicon information
c
      call silinfo(sil,rcen,phcen,sndmom,epad1,epad2,esilc,rmax,phmax)
      Rcn  = rcen*100.
      Phcn = phcen*180./pi
c
c Correct the Energy measurement for the scintillator 
c inhomogeneity effect (1st on R and on PHI afterwards):
c

      call REcorr(rcen,r)
      call PhiEcorr(phcen,p)
c
c We use the APD as Energy measurement for the moment...
c Do not reconstruct energies smaller than 1 GeV.
c
      e = apd/r/p
      if (e.lt.1.) then
       ierr = 1
       goto 999
      endif
c     
c Unfold the Quadrupole effect and recover the 3-momenta at the IP
c
      xbcal(1) = rcen*cos(phcen)
      xbcal(2) = rcen*sin(phcen)
c      
      call quad_inv(ebeam,e,xbcal,pIP,ierr)      
      if (ierr.ne.0) goto 999
c
      call angles (imod,pIP,th,ph)
c      
 999  continue
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine silinfo(sil,rcen,phcen,sndmom,
     .                   epad1,epad2,esilc,rmax,phmax)
c
c Author : G. Merino - 19970424
c
c Description: Translates the Silicon Raw data into meaningful quantities such
c              as the centroids.
c 
c Input:   sil(16,6) = Silicon signals <-> (Radial,Azimuthal) [INTEGER] (ADC counts)  
c 
c Outputs: rcen = Radial centroid [REAL] (Meters)
c          phcen = Azimuthal centroid [REAL] (Rad)
c          sndmom = Second momentum of the radial energy distribution [REAL] (Meters)
c          epad1 = Energy deposited in the 1st radial pad [INTEGER] (ADC counts)
c          epad2 = Energy deposited in the 2nd radial pad [INTEGER] (ADC counts)
c          esilc = Total energy deposited in the silicon [INTEGER] (ADC counts)
c          rmax = Radial pad # in which the maximum deposition took place [INTEGER] 
c          phmax = Azimuthal pad # in which the maximum deposition took place [INTEGER] 
c
      implicit none
c
      integer i,j
      integer ierr
      integer sil(16,6),epad1,epad2,esilc,rmax,phmax
      integer smax,wght
      real rcen,phcen,sndmom
      real dwaf,drad
      real rpos(16),phpos(16,6)
      real pi
c
      common /silmap/ rpos,phpos
c
      pi = acos(-1.)
c
c Localize the most energetic pad
c
      smax = 0
      do i = 1,6
         do j = 1,16
            if (j.eq.1.or.j.eq.2) then
               wght = 2
            else
               wght = 1
            endif
            if (wght*sil(j,i).gt.smax) then
               smax = wght*sil(j,i)
               rmax = j
               phmax = i
            endif
         enddo
      enddo
c
c Centroid computation and 2 1st pads energy
c
      epad1 = 0
      epad2 = 0
      rcen = 0
      phcen = 0
      esilc = 0
c
      do i = 1,6
         do j = 1,16
            dwaf = abs(i - phmax)
            drad = abs(j - rmax)
            if (dwaf.le.1) then
               if (j.eq.1) epad1 = epad1 + sil(j,i)
               if (j.eq.2) epad2 = epad2 + sil(j,i)
               if ((dwaf+drad).le.2) then
                  esilc = esilc + sil(j,i)
                  rcen = rcen + float(sil(j,i))*rpos(j)
                  phcen = phcen + float(sil(j,i))*phpos(j,i)
               endif
            endif
         enddo
      enddo
c
      if(esilc.ne.0) then
         rcen = rcen/esilc
         phcen = phcen/esilc
      endif
c
c Compute the second momentum of the radial energy distribution
c
      if (esilc.ne.0) then
         do i = 1,6
            do j = 1,16
               sndmom = sndmom + float(sil(j,i))*(rpos(j)-rcen)**2
            enddo
         enddo
         sndmom = sqrt(sndmom/esilc)
      else
         sndmom = 0.
      endif
c
c Correct for the S_shape on Phi
c
cc      call PhiSShCorr(phcen,ierr)
c
c Change units: rcen (microns -> meters) , phcen (degrees -> radians)
c
      rcen = rcen*1.e-6
      phcen = phcen*pi/180
c
 999  continue
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine PhiSShCorr(phcen,ierr)
c
c Author : G. Boix  and G. Merino - 19970424
c
c Description: Corrects the Phi centroid value for the S_shape distortion 
c          Takes from /PhSS/ common the parameters of the fit to the s-shape.
c
c Input:  phcen before correcting [REAL] (deg)
c
c Output: phcen once corrected [REAL] (deg)
c
      implicit none
c
      real pcor,phcen,pno,preal,psi
      real PhSSpar(5)
c
      integer i,ierr
c
      common /PhSS/ PhSSpar
c
      ierr = 0
c
c Compute the centroid position in pad units 
c and normalised between (0,1) 
c
      psi = phcen/11.25
      if (psi.gt.0.) then
         pno = psi - float(int(psi))
      else
         pno = psi + float(int(abs(psi))+1)
      endif
c     
      if (abs(psi).gt.4) then
         ierr = 1
         goto 100
      endif
c
c Correct for the S_shape on phi direction.
c
      call Fcorr_Phi_SSh(pno,PhSSpar,pcor)
      preal = (psi - pcor)*11.25
c
      phcen = preal
c
 100  return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine Fcorr_Phi_SSh(x,par,pcor)
c  
c Author : G. Merino - 19970424
c
c Description: Subroutine that computes the correction to the 
c              phi value of the centroid
c              due to the S_shape distortion.
c
c Inputs: x = Phi centroid value (in pad units, normalised between 0 and 1)
c         par(5) = Correction parameters
c Output: pcor = Corrected Phi centroid  (in pad units, normalised between 0 and 1)  
c
      implicit none
c
      real fact,fit,p1,p2,p3,p4,p5,p6,p7,p8,par(5),pcor,x
c
        x = x - 0.5
c
        p1 = par(1)
        p2 = par(2)
        p5 = par(3)
        p6 = par(4)
c
        p8 = 2*p5*p2+p6
c
        p3 = 2.*p5*p1+p6
c
        p7 = p2*(p8-p5*p2-p6)
c
        p4 = p7+p1*(p5*p1+p6-p3)
c
        fact = 1.
c
        if(x.gt.0.0) then 
          x = -x
          fact = -1.
        endif
c
        if(x.lt.p1) then 
c
          fit = p3*x+p4
c
        elseif(x.lt.p2) then
c
          fit = p5*x*x+p6*x+p7
c
        else
c
          fit = p8*x
c
        endif
c
            fit = fit*fact+par(5)
c
        pcor = fit
c
        return
        end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine PhiEcorr(ph,p)
c
c Author : G. Boix and G. Merino - 19970424
c
c Description: Computes the correction factor for the Energy measurement 
c              due to the the Scintillator 
c              non-uniform light yield along the Phi direction.
c
c Input:   ph = Phi centroid measured [REAL] (rad)
c Output:  p = Correction factor
c
      implicit none
c
      real ph
      real p,phcen
      real PhEpar(5)
      real pi
c
      integer n
c
      common /PhE/ PhEpar
c
c Change Phcen units: Radians -> Degrees
c
      pi = acos(-1.)
      phcen = ph*180./pi
c
c Phi leakage correction function: p4
c
      p = PhEpar(1) + PhEpar(2) * phcen   
     .     +  PhEpar(3) *phcen*phcen + PhEpar(4) *phcen*phcen*phcen
     .     +  PhEpar(5) *phcen*phcen*phcen*phcen 
c  
      return 
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      subroutine REcorr(r,f)
c
c Author : G. Merino - 19970424
c
c Description: Computes the correction factor for the Energy measurement 
c              due to the Scintillator 
c              non-uniform light yield along the Radial direction.
c
c Input:   r = Radial centroid measured [REAL] (Meters)
c Output:  f = Correction factor
c
      implicit none
c
      real r,f
      real rcen,p6,REpar(6)
      real rmax,fmax
c
      common /RE/ REpar
c
      p6 = (REpar(1)-REpar(4))*REpar(6)*REpar(6)+
     .        (REpar(2)-REpar(5))*REpar(6)+REpar(3)
c
c Change Rcen units: Meters -> Centimeters
c
      rcen = r*100.
c
c radial leakage correction function: 2 parabolas
c
      if (rcen.gt.REpar(6)) then
         f = REpar(4)*rcen*rcen+REpar(5)*rcen+p6
       else
         f = REpar(1)*rcen*rcen+REpar(2)*rcen+REpar(3)
      endif
c  
c Normalise it to have max. value = 1
c
      rmax = - REpar(5)/2./REpar(4)
      fmax = REpar(4)*rmax*rmax+REpar(5)*rmax+p6 
      f = f/fmax
c
      return 
      end
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine angles(imod,pIP,th,ph)
c
c Author : G. Merino - 19970424
c
c Purpose: Returns the measurable quantities in the proper Ref. System
c 
c Inputs: imod = BCAL++ Module number [INTEGER]
c         pIP(3) = Measured 3-momenta in the IP [REAL] (GeV)
c 
c Outputs: e = Energy [REAL] (GeV)         
c          th = Measured Theta angle in the IP [REAL] (Rad)
c          ph = Measured Phi angle in the IP [REAL] (Rad) 
c 
      implicit none
c
      integer imod
      real pIP(3)
      real e,th,ph
      real pi
c
      pi = acos(-1.)
c
      e = sqrt(pIP(1)**2+pIP(2)**2+pIP(3)**2)
c
      if (imod.eq.2) then
         pIP(1) = - abs(pIP(1))
      elseif (imod.eq.3) then
         pIP(3) = - abs(pIP(3))
      elseif (imod.eq.4) then
         pIP(3) = - abs(pIP(3))
         pIP(1) = - abs(pIP(1))
      endif
c
      th = acos(pIP(3)/e)
      ph = atan2(pIP(2),pIP(1))
c
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
