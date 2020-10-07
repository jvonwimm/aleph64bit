ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine quad_sim(ebeam,ich,vtx,p,pout,xout,iflag)
C
C Author : G. Merino and F. Sanchez - 19970424
C
C Description: Routine to simulate the Quadrupole bending of the e- tracks
C 
C Inputs: P(3) = 3-momenta at the IP (GeV).
C         ICH = 1 => particle charge matches with the 4pole. 
C         ICH = 0 =>    "       "  doesn't match with the 4pole.
C         VTX(3) = Vertex position (Meters)
C Output: POUT(3) = 3-momenta at BCAL++ front end (GeV).
C         XOUT(3) = position at BCAL++ front end (Meters).
C         IFLAG = 0=> Particle not tagged in BCAL++
C
      implicit none
c
      integer imod
      real d,d1,d2,d3,d4,rin,rinbc,rotbc,phabc,qksc,ebeam
      real p(3),ph,ptot,pout(3),ppp,r,squad,tmp,xout(3)
      real m1(8),m2(8),m3(8),mqsc(8),mlum(8)
      real vtx(3)
      integer iflag,ich
c
      common/quad_par/qksc,d1,d2,d3,d4,rin
      common/bcal/rinbc,rotbc,phabc
c
      iflag = 1
c
      if (ich.eq.0) then
         tmp = p(1) 
         p(1) = p(2)
         p(2) = tmp
         tmp = vtx(1)
         vtx(1) = vtx(2)
         vtx(2) = tmp  
      endif
c
      ptot = sqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
c
      squad = qksc*ebeam/ptot
c
c to the end of the quad.
c
      if (p(3).gt.0.) then
         d = d1 - vtx(3)
      else
         d = d1 + vtx(3)
      endif
c
      call quad(0.,d,m1)
      call quad(squad,d2,m2)
      call matmul(m1,m2,mqsc)
c
      xout(1) = mqsc(2)*p(1)/ptot + mqsc(1)*vtx(1)
      xout(2) = mqsc(6)*p(2)/ptot + mqsc(5)*vtx(2)
c
      if(sqrt(xout(1)**2.+xout(2)**2.).gt.rin) then
         iflag = 0
      endif
c
c at the end of the flange.
c
      call quad(0.,d4,m1)
      call matmul(mqsc,m1,mlum)
c
      xout(1) = mlum(2)*p(1)/ptot + mqsc(1)*vtx(1)
      xout(2) = mlum(6)*p(2)/ptot + mqsc(5)*vtx(2)
c
      if(sqrt(xout(1)**2.+xout(2)**2.).gt.rin) then
         iflag = 0
      endif
c
c to bcal++
c
      call quad(0.,d3-d4,m1)
      call matmul(mlum,m1,mqsc)
c
      xout(1) = mqsc(2)*p(1)/ptot + mqsc(1)*vtx(1)
      xout(2) = mqsc(6)*p(2)/ptot + mqsc(5)*vtx(2)
      xout(3) = d1+d2+d3
c
      pout(1) = mqsc(4)*p(1)
      pout(2) = mqsc(8)*p(2)
c
      ppp = pout(1)*pout(1) + pout(2)*pout(2)
      if (ppp.ge.ptot*ptot) then
        iflag = 0
        goto 999
      endif       
      pout(3) = sqrt(ptot*ptot-ppp)
c
      if (ich.eq.0) then
         tmp = pout(1) 
         pout(1) = pout(2)
         pout(2) = tmp
         tmp = xout(1)
         xout(1) = xout(2)
         xout(2) = tmp
      endif
c
      r = sqrt(xout(1)**2.+xout(2)**2.)
      ph = atan2(abs(xout(2)),abs(xout(1)))*180./3.141592
c
      if(r.gt.rinbc.and.r.lt.rotbc.and.ph.lt.phabc) then 
       continue    
      else
       iflag = 0
      endif
c
 999  continue
c
c Restore the input variables
c
      if (ich.eq.0) then
         tmp = p(2) 
         p(2) = p(1)
         p(1) = tmp
         tmp = vtx(2)
         vtx(2) = vtx(1)
         vtx(1) = tmp
      endif
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine quad_inv(ebeam,ptot,xout,p,iflag)
c
c Author : G. Merino - 19970424
c
c Description: Routine to invert the quad effect and recover the 
c              3-vector of the e- at the IP from the BCAL++ observables.
c
c inputs : ebeam = Nominal beam energy (GeV) [REAL]
c          ptot = 3-momenta module measured (GeV) [REAL] 
c          xout(2) = X,Y position at the BCAL++ front end (measured) (Meters)
c 
c output : p(3) = 3-momenta at the ip (GeV) [REAL]
c          iflag = 0 (no problem) [INTEGER]
c                  1 (problems in reconstruction)
c
      implicit none
c
      real d1,d2,d3,d4,rin,rinbc,rotbc,phabc,qksc,ebeam 
      real m1(8),m2(8),m3(8),mqsc(8),mlum(8)
      real p(3),ppp,ptot,xout(2),squad
      integer iflag
c
      common/quad_par/qksc,d1,d2,d3,d4,rin
c
      iflag = 0
c
      squad = qksc*ebeam/ptot
c
      call quad(0.,d1,m1)
      call quad(squad,d2,m2)
      call matmul(m1,m2,mqsc)
c
      call quad(0.,d3,m1)
      call matmul(mqsc,m1,mlum)
c
c (x,y) >>> (px,py)
c
      if(abs(mlum(2)).lt.1.e-20.or.abs(mlum(6)).lt.1.e-20) then
       iflag = 1 
       return 
      endif
c
      p(1) = xout(1)*ptot/mlum(2)
      p(2) = xout(2)*ptot/mlum(6)
c
      ppp = p(1)*p(1) + p(2)*p(2)
c
      if(ppp.gt.ptot*ptot) then
       iflag = 1 
       return 
      endif
c
      p(3) = sqrt(ptot*ptot-ppp)
c
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine quad(k,s,mq)
c
c Author : F. Sanchez 
c
c Description: Quadrupole transfermatrix
c
      real mx(8),my(8),mq(8),k,qk,s
      if(k.eq.0.)goto 950
      qk=sqrt(abs(k))
      eh=exp(qk*s)
      mx(1)=cos(qk*s)
      mx(2)=sin(qk*s)/qk
      mx(3)=-mx(2)*qk*qk
      mx(4)=mx(1)
      my(1)=(eh+1./eh)/2
      my(2)=(eh-1./eh)/(2.*qk)
      my(3)=+my(2)*qk*qk
      my(4)=my(1)
      if(k.gt.0.)goto 920
      do 910 i=1,4
      mq(i)=mx(i)
  910 mq(i+4)=my(i)
      return
  920 do 930 i=1,4
      mq(i)=my(i)
  930 mq(i+4)=mx(i)
      return
  950 mx(1)=1.
      mx(2)=s
      mx(3)=0.
      mx(4)=1.
      do 960 i=1,4
      mq(i)=mx(i)
  960 mq(i+4)=mx(i)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine matmul(mx,my,mz)
c
c Author : F. Sanchez
c
c Description: Multipl. of two (2*2)-matrices:mz)ij=my)ik*mx)kj
c
      real mx(8),my(8),mz(8)
      mz(1)=my(1)*mx(1)+my(2)*mx(3)
      mz(2)=my(1)*mx(2)+my(2)*mx(4)
      mz(3)=my(3)*mx(1)+my(4)*mx(3)
      mz(4)=my(3)*mx(2)+my(4)*mx(4)
      mz(5)=my(5)*mx(5)+my(6)*mx(7)
      mz(6)=my(5)*mx(6)+my(6)*mx(8)
      mz(7)=my(7)*mx(5)+my(8)*mx(7)
      mz(8)=my(7)*mx(6)+my(8)*mx(8)
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine Bcal_Tag(Ebeam,ICh,P,Vtx,IMod)
c
c Author : G. Merino - 19971206
c
c Description: Looks if a certain particle arrives to BCAL++ 
c
c Inputs: Ebeam = LEP Beam energy [REAL] (GeV)
c         ICh = Particle charge [INTEGER]
c         P(4) = Particle 4-momenta [REAL] (GeV)
c         Vtx(3) = Particle Origin Vertex [REAL] (cm)
c
c Outputs: IMod = BCAL++ Module number in which the particle 
c                 hitted (0 if it didn't hit any module)
c
      implicit none
c
      integer ICh,IMod
      real Ebeam,P(4),Vtx(3)
      real x(3)
      real pVec(3),pout(3),xout(3)
      real Theta
      integer itag,ichflg
c
      IMod = 0
c
c Safety cuts in 3-momenta module and Theta angle
c
c         P > 1 GeV & Theta > 1 mrad 
c
      Theta = acos(abs(P(3))/P(4))
      if (P(4).lt.1..or.Theta.lt.0.001) goto 999
c
c Which module might be hitted?
c
      call module(P,IMod)
C     
C Neutral Particles
C
      if (ICh.eq.0) then 
         call neutral(P,x,itag)
         if (itag.eq.0) then
            IMod = 0
         endif
         goto 999
C
C Consider the particle charge for the 4pole behaviour
C 
      elseif ((ICh.eq.-1.and.(imod.eq.1.or.imod.eq.2)).or.
     .         (ICh.eq.1.and.(imod.eq.3.or.imod.eq.4))) then
         ichflg = 1 
      else 
         ichflg = 0
      endif
C     
C Simulate the effect of the quadrupole
C
      Vtx(1) = Vtx(1)/100.
      Vtx(2) = Vtx(2)/100.
      Vtx(3) = Vtx(3)/100.  
      pVec(1) = P(1)
      pVec(2) = P(2)
      pVec(3) = P(3)
      call quad_sim(Ebeam,ichflg,Vtx,pVec,pout,xout,itag)
      if (itag.eq.0) IMod = 0
c
 999  continue
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine module(p,imod)
C
C Author : G. Merino - 19970424
C
C Description: Returns the # of BCAL++ module which will be hitted by the e- with
C              3-momenta P(3) 
C
C Inputs:   P(3) = MC true 3-momenta of the incident particle (REAL)
C   
C Outputs:  IMOD = # of module hitted (1,2,3,4) (INTEGER)
C
C
C
C                          LEP center
C
C                            X ^
C          __                  |                  __
C         |__| #3              |                 |__| #1
C                     e- --->  |  <--- e+
C       -----------------------.---------------------------> Z
C          __                                     __
C         |__| #4                                |__| #2
C                               
C                             

      implicit none
c
      real p(3)
      integer imod
c
      if (p(3).gt.0.) then
         if (p(1).gt.0.) then
            imod = 1
         else
            imod = 2
         endif
      else
         if (p(1).gt.0.) then
            imod = 3
         else
            imod = 4
         endif
      endif
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine neutral(p,xout,itag)
C
C Author : G. Merino - 19970424
C
C Description: Propagates neutral tracks to BCAL++ position
C
C Input: P(3) = Particle 3-momenta in the interaction point (REAL)
C
C Output:
C        xout(3) = Position of the particle at the BCAL++ entrance (REAL) 
C        itag = (1 => tag ; 0 => not tag) (INTEGER)
C
      implicit none
c
      real d1,d2,d3,d4,rin,rinbc,rotbc,phabc,qksc
      real th,thmin,thmax,pmod,ph,xout(3),p(3)
      integer itag
c
      common/quad_par/qksc,d1,d2,d3,d4,rin
      common/bcal/rinbc,rotbc,phabc     
c
      itag = 0
c
      pmod = sqrt(p(1)**2+p(2)**2+p(3)**2)
      th = acos(abs(p(3))/pmod)
      ph = atan2(abs(p(2)),abs(p(1)))
c
      thmin = rinbc/(d1+d2+d3)
      thmax = rin/(d1+d2+d4)
c
      if (th.lt.thmax.and.th.gt.thmin.and.
     .    ph*180./3.141593.lt.phabc) then 
         itag = 1 
         xout(1) = (d1+d2+d3)*tan(th)*cos(ph)
         xout(2) = (d1+d2+d3)*tan(th)*sin(ph)
         xout(3) = (d1+d2+d3)
      endif
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      blockdata param
c
c Author : G. Merino - 19970424
c
c Description: Blockdata with all the parameters used in the 
c              subroutines BCALDATA and BCALMC
c
      implicit none
c
      real rpos(16),phpos(16,6)
      real PhSSpar(5)
      real PhEpar(5)
      real REpar(6)
c     
      real d1,d2,d3,d4,qksc,rin
      real sigma_e,sigma_r,sigma_phi
      real rinbc,rotbc,phabc
c
      common /quad_par/qksc,d1,d2,d3,d4,rin 
      common/bcal/rinbc,rotbc,phabc
      common /smear/ sigma_e,sigma_r,sigma_phi
      common /silmap/ rpos,phpos
      common /PhSS/ PhSSpar
      common /PhE/ PhEpar
      common /RE/ REpar
C
C BCAL++ Monitor "effective" dimensions
C
C  RINBC = interal radius (metres) 
C  ROTBC = external radius (metres)
C  PHABC = phi acceptance (degrees)
C     
      data rinbc / 0.06555 /    ! Interal radius (metres)      
      data rotbc / 0.103   /    ! External radius (metres)
      data phabc / 33.75   /    ! Phi acceptance (degrees)
c
c Smearing parameters for the BCAL++ Fast Simulation
c
      data sigma_e   / 0.25  /  ! Sigma_Energy (GeV^1/2)
      data sigma_r   / 0.004 /  ! Sigma_R (METRES*GEV^1/2) 
      data sigma_phi / 10.   /  ! Sigma_phi (DEG*GEV^1/2)
c
c rpos :  radial position of silicon pads (microns)
c phpos : azimuthal  position of silicon pads (degrees)
c
      data rpos/ 
     .     64925.,66175.,68050.,70550.,73050.,75550.,78050.,80550.,
     .     83050.,85550.,88050.,90550.,93050.,95550.,98050.,100550./
      
c
      data phpos/ 
     .-28.125,-28.125,-28.125,-28.125,-28.125,-28.125,-28.125,-28.125,
     .-28.125,-28.125,-28.125,-28.125,-28.125,-28.125,-28.125,-28.125,
     .-16.875,-16.875,-16.875,-16.875,-16.875,-16.875,-16.875,-16.875,
     .-16.875,-16.875,-16.875,-16.875,-16.875,-16.875,-16.875,-16.875,
     .-5.625,-5.625,-5.625,-5.625,-5.625,-5.625,-5.625,-5.625,
     .-5.625,-5.625,-5.625,-5.625,-5.625,-5.625,-5.625,-5.625,
     . 5.625,5.625,5.625,5.625,5.625,5.625,5.625,5.625, 
     . 5.625,5.625,5.625,5.625,5.625,5.625,5.625,5.625, 
     . 16.875,16.875,16.875,16.875,16.875,16.875,16.875,16.875,
     . 16.875,16.875,16.875,16.875,16.875,16.875,16.875,16.875,
     . 28.125,28.125,28.125,28.125,28.125,28.125,28.125,28.125, 
     . 28.125,28.125,28.125,28.125,28.125,28.125,28.125,28.125/
c
c Parameters for the S_shape Phi correction (Fitted for 90 GeV e-)
c
      data PhSSpar/-.1105633,-.1469902E-04,-29.90563,-5.919351,
     .         -.1725036E-01/
c
c Energy correction depending on Phi
c
      data PhEpar/1.0011,0.98521E-03,-0.90767E-05,0.18612E-06,
     .            -0.58218E-07/
c
c Energy correction depending on R
c
      data REpar/-706.49,10055.,-34235.,-66.920,977.87,7.0957/
c
c Quadrupole related parameters
c
c  x-------xxxxxxx-----xx----------[  ]
c ip        quad      flng.       bcal
c   <----> <-----><----->
c     d1     d2      d4
c                 <--------------->
c                        d3
c
      data qksc / 0.16106 /  ! quad strength
      data rin  / 0.077 /       ! Beam pipe internal radius.
c     
      data d1  / 3.72 /          ! ip to internal quad border. 
      data d2  / 2.0  /          ! quad length.
      data d3  / 1.98 /          ! external quad border to monitor.
      data d4  / 1.4  /          ! Straigth line after dipole to the end of 2nd flange.
c
      end
