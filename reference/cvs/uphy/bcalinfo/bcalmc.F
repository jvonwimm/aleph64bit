            subroutine bcalmc(Ebeam,Year,Npar,Ipar,P,Vtx,E,Th,Ph,
     .                        Rcn,Phcn,Ierr)
c------------------------------------------------------------------------------------------
c Author : G. Merino - 19970424
c
c  Description: Fast BCAL++ response simulation for MC events    
c
c  Inputs: Ebeam = Beam nominal Energy [REAL] (GeV)
c          Year  = 97 or 98 (for the Geometry) [INTEGER] 
c          Npar  = Number of MC particles to be tracked [INTEGER]
c          Ipar(2,Npar) = Particle code and charge: [INTEGER]
c                         Ipar(1,Npar) -> (=0 gamma,=1 e,=2 mu,=3 had)
c                         Ipar(2,Npar) -> (-1,0,1)
c          P(4,Npar) = MC Particles 4-momenta in the interaction point [REAL] (GeV)
c          VTX(3,Npar) = MC Particles origin vertex [REAL] (Centimeters) 
c
c  Outputs: E  = Energy measured in a certain BCAL++ module [REAL] (GeV)
c           TH = Theta angle in the IP measured by BCAL++, assuming it was an e+(e-)[REAL] (rad)
c           PH = Phi angle in the IP measured by BCAL++, assuming it was an e+(e-) [REAL] (rad)
c           Rcn = Measured radial centroid in BCAL++ [REAL] (cm)
c           Phcn = Measured azimuthal centroid in BCAL++ [REAL] (degrees)            
c           Ierr  = Error flag : 0=> No error ; 1=> Npar is too large (max 50) [INTEGER]
c                                   
c  Modifications:
c
c   19971205 = Add the possibility of having several particles arriving
c                       to the same module.    
c   19980108 = Add the Rcn and Phcn outputs
c   19980611 = Add the "Year" input
c------------------------------------------------------------------------------------------
      implicit none
C
      integer Nmx
      parameter ( Nmx = 50 )  ! Max value for Npar 
C
      real Ebeam,P(4,Nmx),Vtx(3,Nmx)
      real E,Ph,Th
      real pipmeas(3),pmod,pout(3),psmea,thip,
     .     xout(3),xsmea,x(2),ysmea
      real pVec(3),vtxVec(3)
      real EtotMeas,XtotMeas,YtotMeas
      real pi
      real Rcn,Phcn
      integer Npar,Ipar(2,Nmx),C(Nmx),Ierr
      integer ich,ie,itag,iflag
      integer imc,imod
      integer IModule
      logical first
      integer Year
c = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      pi = acos(-1.)
C
C Initialise output values to zero
C
      Ierr = 0
      E = 0.
      Th = 0.
      Ph = 0.
      if (Npar.gt.Nmx) then
         Ierr = 1
         goto 999
      endif
C
C Loop over the Npar MC particles
C
      EtotMeas = 0.
      XtotMeas = 0.
      YtotMeas = 0.
      first = .true.
      do imc = 1,Npar
C
C Safety cuts in 3-momenta module and Theta angle
C
C         P > 1 GeV & Theta > 1 mrad 
C
         pmod = P(4,imc)
         thip = acos(abs(P(3,imc))/pmod)
         pVec(1) = P(1,imc)
         pVec(2) = P(2,imc)
         pVec(3) = P(3,imc)
         if (pmod.lt.1..or.thip.lt.0.001) goto 100

C
C Which module might be hitted?
C
         call module(pVec,imod)
         if (first) then
            IModule = imod
            first = .false. 
cc         else
cc            if (imod.ne.IModule) print*,'bcalmc:Diff mod!!!!'
         endif
C     
C Neutral Particles
C
         if (Ipar(2,imc).eq.0) then 
            call neutral(pVec,xout,itag)
         else
C
C Consider the particle charge for the 4pole behaviour
C 
            if ((Ipar(2,imc).eq.-1.and.(imod.eq.1.or.imod.eq.2)).or.
     .         (Ipar(2,imc).eq.1.and.(imod.eq.3.or.imod.eq.4))) then
             ich = 1 
            else 
             ich = 0
            endif
C
C Simulate the effect of the quadrupole
C
            vtxVec(1) = Vtx(1,imc)/100.
            vtxVec(2) = Vtx(2,imc)/100.
            vtxVec(3) = Vtx(3,imc)/100.           
            call quad_sim(Ebeam,ich,vtxVec,pVec,pout,xout,itag)
         endif
         if (itag.eq.0) goto 100
C     
C BCAL++ "Fast simulation" of 1 particle
C              
         call FastSim(Ipar(1,imc),pmod,xout(1),xout(2),
     .                psmea,xsmea,ysmea)
C
C Add the Energy and Centroid of this particle to all the others
C which also arrived to the module.   
C
         EtotMeas = EtotMeas + psmea
         XtotMeas = XtotMeas + psmea*xsmea
         YtotMeas = YtotMeas + psmea*ysmea
c
 100     continue
      enddo
c
c Reconstruct the total centroid
c
      if (EtotMeas.gt.0.) then
         XtotMeas = XtotMeas/EtotMEas
         YtotMeas = YtotMeas/EtotMEas
         Rcn = sqrt(XtotMeas**2+YtotMeas**2)*100.       
         Phcn = atan2(YtotMeas,abs(Xtotmeas))*180./pi         
c     
c Invert the 4pole trajectory to get the 3-momenta in the IP
c
         x(1) = XtotMeas
         x(2) = YtotMeas
         call quad_inv (Ebeam,EtotMeas,x,pipmeas,ie)   
         if (ie.ne.0) then 
            Ierr = 1
            goto 999
         endif            
c     
         E = EtotMeas
         if (IModule.eq.1.or.IModule.eq.2) then
            th = acos(pipmeas(3)/EtotMeas) 
         else
            th = pi - acos(pipmeas(3)/EtotMeas) 
         endif
         ph = atan2(pipmeas(2),pipmeas(1))
         if (ph.lt.0) then
            ph = 2.*pi + ph
         endif
c
      endif
c     
 999  continue
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SMEARING(E,X,Y,EF,XF,YF)
C
C Author : G. Merino - 19970424
C
C Description: Routine to simulate the detector smearing.
C 
C    INPUT:    E    MC TRUE ENERGY      (  GeV )      
C              X    MC TRUE X POSITION  (Meters)   
C              Y    MC TRUE Y POSITION  (Meters)
C
C    OUTPUT:   EF   SMEARED ENERGY     ( GeV  )  
C              XF   SMEARED X POSITION (Meters)  
C              YF   SMEARED Y POSITION (Meters)  
C     
      IMPLICIT NONE
C
      REAL GAUSS(3)
      REAL SIGMA_E,SIGMA_PHI,SIGMA_R,E,X,Y,EF,XF,YF,
     +     RTRUE,PHITRUE,RF,PHF
      real fleak
C
      COMMON /SMEAR/ SIGMA_E,SIGMA_R,SIGMA_PHI

      RTRUE = SQRT(X**2+Y**2)
      PHITRUE = ATAN2(Y,X)
C
      CALL RNORML(GAUSS,3)
C
C Energy smearing
C
       EF = E + (SIGMA_E*SQRT(E)*GAUSS(1))*Fleak(rtrue*100.)  
C
C Position smearing 
C
       RF = RTRUE + SIGMA_R/SQRT(E)*GAUSS(2)
C
       PHF = PHITRUE + (SIGMA_PHI)*3.141592/180/SQRT(E)*GAUSS(3)
C
       XF = RF*COS(PHF)
       YF = RF*SIN(PHF)
C
       RETURN 
       END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine FastSim(Icode,Etrue,Xtrue,Ytrue,Emeas,Xmeas,Ymeas)
c
c  Author : G. Merino - 19971206
c
c Description : BCAL++ Fast simulation 
c
c Inputs: Icode = MC Particle code [INTEGER]
c                 (=0 gamma,=1 e,=2 mu,=3 had)
c         Etrue = True energy of the particle arriving BCAL++ [REAL] (GeV)
c         Xtrue = True X of the particle arriving BCAL++ [REAL] (Meters)
c         Ytrue = True Y of the particle arriving BCAL++ [REAL] (Meters)
c 
c Outputs: Emeas = Measured Energy in BCAL++ [REAL] (GeV)
c          Xmeas = Measured X in BCAL++ [REAL] (Metres)
c          Ymeas = Measured Y in BCAL++ [REAL] (Metres)
c
      implicit none
c
      real Etrue,Xtrue,Ytrue,Emeas,Xmeas,Ymeas
      integer Icode
c
      Emeas = 0.
      Xmeas = 0.
      Ymeas = 0.
c
c Naive approx: only e+,e- and gammas deposit their energy in BCAL++
c
      If (Icode.eq.0.or.Icode.eq.1) then
       call smearing (Etrue,Xtrue,Ytrue,Emeas,Xmeas,Ymeas)
      else
       goto 999
      endif
c
 999  continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      function Fleak(R)
c
c Author : G. Merino - 19980117
c
c Description: Parametrisation of the dependence of the energy resoution with the
c              radius.
c
c Input:   R = Radius (cm) [REAL]
c
      implicit none
c
      real Fleak
      real R
      real EResPar(4)
c
      common /REMC/ EResPar
c
      Fleak = EResPar(1)*R**3+EResPar(2)*R**2
     .         +EResPar(3)*R+EResPar(4)
cc      Fleak = 1.
c
      end
