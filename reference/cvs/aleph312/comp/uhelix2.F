      subroutine uhelix2(r, vv0, vv0Cov, pos, posCov, iret)
C------------------------------------------------------------------------
C! Given helix and covariance, compute pos and error at radius.
CKEY USER TRACK HELIX
C
C Paul Rensing - 15-08-1995
C
C Given a radius and a set of helix parameters and covariance matrix
C     (standard FRFT definitions), compute the position and its
C     covariance. POS = (phi, z). POSCOV = ( phi, phi-z, z ).
C
C INPUT:
C   REAL R     = radius
C   REAL VV0(5)  = helix parameters (as in FRFT)
C   REAL VV0COV(15)  = helix covariance (as in FRFT)
C
C OUTPUT:
C   REAL POS(2)   = position (phi, z)
C   REAL POSCOV(3)  = positon covariance (phi, phi-z, z)
C   INTEGER IRET    = return code, =  0 if successful
C                                  = -1 helix does not reach R
C                                  = -2 bad helix parameters
C----------------------------------------------------------------------
#ifndef DOC
      implicit none
C
      real r, vv0(5), vv0cov(15), pos(2), posCov(3)
      integer iret
C
      double precision sst
      double precision helCov(5,5)
      double precision b, b2, X, Y, a, c
      double precision deriv(2,5)
      integer ind, i, j, k, l
      double precision ccov

      sst = sign(1.0, vv0(1))
      if (r .ge. sst*(2.0/vv0(1) - vv0(4))-0.01 .or.
     &     r .le. abs(vv0(4))+0.01 ) then
         iret = -1
         return
      end if
      
      if (vv0(1)*vv0(4) .ge. 1.0) then
         iret = -2
         return
      end if
      
C copy the helix covar. matrix into a symmetric matrix
      ind = 1
      do i = 1, 5
         do j = 1, i
            helCov(i,j) = vv0Cov(ind)
            helCov(j,i) = helCov(i,j)
            ind = ind + 1
         end do
      end do
      
      b = 1.0 - vv0(4)*vv0(1)
      b2 = b*b
      
      X = (vv0(1)/2.0) * sqrt( (r-vv0(4))*(r+vv0(4)) / b )
      
      Y = sst * ( (vv0(1)*(r-vv0(4))*(r+vv0(4))) /
     $     (-2.0*r*b) + (vv0(4)/r) )
      
      pos(1) = vv0(3) - sst * asin(Y)
      pos(2) = vv0(5) - 2.0 * (vv0(2)/vv0(1)) * asin(- X)
      
C     derivative of phi
      a = 2.0* r * sqrt( (1.0-Y)*(1.0+Y) )
      
      deriv(1,1) = - (vv0(4)*vv0(4) - r*r) / (a * b2)
      deriv(1,2) = 0.0
      deriv(1,3) = 1.0
      deriv(1,4) = -1.0/a * ( 1.0 + (1.0-vv0(1)*vv0(1)*r*r)/b2 )
      deriv(1,5) = 0.0
      
C     derivatives of z
      c = X / sqrt( (1.0-X)*(1.0+X) )
      deriv(2,1) = (vv0(5)-pos(2))/vv0(1) +
     $     (vv0(2)/(vv0(1)*vv0(1))) * c * (2.0 + vv0(4)*vv0(1)/b)
      deriv(2,2) = (pos(2) - vv0(5))/vv0(2)
      deriv(2,3) = 0.0
      deriv(2,4) = 2.0 * vv0(2)/vv0(1) * c *
     $     ( vv0(1)/(2.0*b) - vv0(4)/(r*r - vv0(4)*vv0(4)) )
      deriv(2,5) = 1.0
      
C     multiply the matrices to get the covariance of phi and z
      ind = 1
      do i = 1, 2
         do j = 1, i
            ccov = 0.0
            do k = 1, 5
               do l = 1, 5
                  ccov = ccov + deriv(i,k) * helCov(k,l) * deriv(j,l)
               end do
            end do
            posCov(ind) = ccov
            ind = ind + 1
         end do
      end do

      iret = 0
      return
      end
#endif
