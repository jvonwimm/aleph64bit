*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//                                                                                         //
*//                               Pseudo-CLASS  BVR                                         //
*//                                                                                         //
*//             B-functions Real and Virtual and YFS form-factors                           //
*//                                                                                         //
*//                                                                                         //
*//    WARNIG:  !!!!!!!!!!!!!!!!!!!!!!![[[[[[[[]]]]]]]!!!!!!!!!!!!!!!                       //
*//    Function BVR_SBvirt modified temporarily to avoid problem with pi**2/beta            //
*//    WARNIG:  !!!!!!!!!!!!!!!!!!!!!!![[[[[[[[]]]]]]]!!!!!!!!!!!!!!!                       //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE BVR_MakeF2Pair(svar,amex,Alfpi,Q,F2Pair)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION Alfpi,Q,svar,amex,F2Pair
      INTEGER          iKF,NCf
      DOUBLE PRECISION Masf, Qf,T3f, Ene,Sum
      DOUBLE PRECISION BVR_F2PairS, F2PairS
      DOUBLE PRECISION Mquark(6)
*                       d       u       s       c       b       t
      DATA Mquark / 0.20d0, 0.20d0, 0.20d0, 1.30d0, 5.00d0, 175.0d0 /
cc      DATA Mquark / 0.40d0, 0.40d0, 0.40d0, 1.30d0, 5.00d0, 175.0d0 /
      SAVE Mquark
*-------------------------------
      INTEGER          icont
      DATA icont /0/
      SAVE icont
      icont=icont+1
*-------------------------------
      Ene =SQRT(svar)
      Sum = 0d0
      DO iKF=1,16
         IF(iKF.LE.6 .OR. iKF.GE.11) THEN
            CALL BornV_GetParticle(iKF, Masf, Qf,T3f,NCf)
            IF(iKF.LE.6) Masf=Mquark(iKF)                       ! replace with dispersive mass
            IF( (Ene.GT.2*Masf) .AND. (ABS(Qf).GT.1d-6) ) THEN  ! only charged above threshols
               F2PairS = BVR_F2PairS(svar,Masf,amex)
               F2PairS = F2PairS *NCf*Qf**2 *Alfpi
               Sum = Sum + F2PairS
****           IF(icont.LT.100) write(*,*) '===, iKF, F2PairS =',iKF, F2PairS!
            ENDIF
         ENDIF
      ENDDO
      F2Pair = Sum *Q**2 *Alfpi
****  IF(icont.LT.100) write(*,*) '--- BVR_MakeF2Pair: F2Pair=',F2Pair,' Ene=', Ene 
      END

      DOUBLE PRECISION FUNCTION BVR_F2PairS(svar,ampair,amel)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// s-channel one-pair virtual correction to ee vertex                                      //
*//\bibitem{BURGERS}G.J.H. Burgers, {\it Phys. Lett.}, {\bf B164}, (1985), {167}.           //
*//\bibitem{KNIEHL}B. A. Kniehl,    {\it Phys. Lett.}, {\bf B237}, (1990), {127}.           //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION svar,ampair,amel
      DOUBLE PRECISION slog,const
*
      slog=DLOG(svar/ampair**2)
      IF ( ABS(ampair/amel-1) .LT. 0.01d0 ) THEN
        const=383d0/108d0 -11d0/36d0*m_pi**2 
      ELSE
        const=-1/3d0*m_zeta3 -19d0/18d0*m_pi**2/6d0 +3355d0/1296d0
      ENDIF
      BVR_F2PairS=
     $   -1d0/36d0                     *slog**3 
     $   +19d0/72d0                    *slog**2 
     $   +(1d0/18d0*m_pi**2 -265d0/216d0) *slog 
     $   +const
      END

      DOUBLE PRECISION FUNCTION BVR_F2PairT(tvar,ampair,amel)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//   NOT USED, kept for futute.                                                            //
*// t-channel one-pair virtual correction to ee vertex                                      //
*//\bibitem{BURGERS}G.J.H. Burgers, {\it Phys. Lett.}, {\bf B164}, (1985), {167}.           //
*//\bibitem{KNIEHL}B. A. Kniehl,    {\it Phys. Lett.}, {\bf B237}, (1990), {127}.           //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION tvar,ampair,amel
      DOUBLE PRECISION tlog,const
*
      tlog=DLOG(tvar/ampair**2)
      IF ( ABS(ampair/amel-1) .LT. 0.01d0 ) THEN
ccc      IF (ampair.EQ.amel) THEN
        const=383d0/108d0 -11d0/36d0*m_pi**2 
      ELSE
        const=-1d0/3d0*m_zeta3 -19d0/18d0*m_pi**2/6d0 +3355d0/1296d0
      ENDIF
      BVR_F2PairT=
     $   -1d0/36d0                     *tlog**3 
     $   +19d0/72d0                    *tlog**2 
     $   +(-1d0/36d0*m_pi**2 -265d0/216d0) *tlog 
     $   +const
     $   +19d0/72d0*m_pi**2
      END

      SUBROUTINE  BVR_MakeVini(Alfpi,Q,p1,m1,p2,m2,ph, V1,V2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Initial state virtual correction to single bremsstrahlung                             //
*//   IR subtracted, in small mass approximation.                                           //
*//   Second order LL  (+NLL???) is included                                                //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION   Alfpi,Q,m1,m2,p1(4),p2(4),ph(4)
      DOUBLE COMPLEX     V1,V2, cL, V2a
      DOUBLE COMPLEX     BVR_CnuA
      DOUBLE PRECISION   p1p2,Svar,r1,r2,zz
      DOUBLE PRECISION   BVR_Dilog
*
      p1p2  =  p1(4)*p2(4) -p1(3)*p2(3) -p1(2)*p2(2) -p1(1)*p2(1)
      Svar  = 2*p1p2 +m1**2+m2**2
      r1 = (p1(4)*ph(4)-p1(3)*ph(3)-p1(2)*ph(2)-p1(1)*ph(1))/p1p2
      r2 = (p2(4)*ph(4)-p2(3)*ph(3)-p2(2)*ph(2)-p2(1)*ph(1))/p1p2

      cL = BVR_CnuA(Svar,m1,m2)-1d0                 !! <-- this is just ln(s/m**2)-i*pi -1
***   cL = DCMPLX( DLOG(Svar/m1/m2)-1d0, -1d0 )

      V1 = (Alfpi*Q**2) *0.5d0*cL                   !! constant LL part

      V2a= (Alfpi*Q**2) *( -0.25d0*cL*DLOG((1d0-r1)*(1d0-r2)) ) !! LL formula averaged over r1,r2

      V2 = (Alfpi*Q**2) *0.5d0*(
     $     +DLOG(r1)*DLOG(1-r2)  +DLOG(r2)*DLOG(1-r1)           !! LL part
     $     +BVR_Dilog(r1)        +BVR_Dilog(r2)                 !! NLL this and all the rest
     $     -1d0/2*DLOG(1-r1)**2  -1d0/2*DLOG(1-r2)**2
     $     +3d0/2*DLOG(1-r1)     +3d0/2*DLOG(1-r2)
     $     +1d0/2*r1*(1-r1)/(1+(1-r1)**2)
     $     +1d0/2*r2*(1-r2)/(1+(1-r2)**2)
     $     )
      END

      SUBROUTINE  BVR_MakeVfin(Alfpi,Q,p3,m3,p4,m4,ph, V1,V2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Final state virtual correction to single bremsstrahlung                               //
*//   IR subtracted, in small mass approximation.                                           //
*//   Second order LL  (+NLL???) is included                                                //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION   Alfpi,Q,m3,m4,p3(4),p4(4),ph(4)
      DOUBLE COMPLEX     V1,V2, cL, V2a
      DOUBLE COMPLEX     BVR_CnuA
      DOUBLE PRECISION   p3p4,Svar,r1,r2,zz,s1,s2
      DOUBLE PRECISION   BVR_Dilog
*
      p3p4  =  p3(4)*p4(4) -p3(3)*p4(3) -p3(2)*p4(2) -p3(1)*p4(1)
      Svar  = 2*p3p4 +m3**2+m4**2
      s1 = (p3(4)*ph(4)-p3(3)*ph(3)-p3(2)*ph(2)-p3(1)*ph(1))/p3p4
      s2 = (p4(4)*ph(4)-p4(3)*ph(3)-p4(2)*ph(2)-p4(1)*ph(1))/p3p4
* normal definition as in O(alf1) single-photon case
      r1 = s1/( 1d0 +s1 +s2 )
      r2 = s2/( 1d0 +s1 +s2 )

      cL = BVR_CnuA(Svar,m3,m4)-1d0                 !! <-- this is just ln(s/m**2)-i*pi -1
***   cL = DCMPLX( DLOG(Svar/m3/m4)-1d0, -1d0 )

      V1 = (Alfpi*Q**2) *0.5d0*cL                   !! constant LL part

      V2= (Alfpi*Q**2) *( -0.25d0*cL*DLOG((1d0-r1)*(1d0-r2)) )  !! LL formula averaged over r1,r2
      V2= V2+ (Alfpi*Q**2) *( +0.50d0*cL*DLOG((1d0-r1)*(1d0-r2)) ) !! corr. due YFS formfactor

* this ISR formula is to be replaced
cc      V2 = (Alfpi*Q**2) *0.5d0*(
cc     $     +DLOG(r1)*DLOG(1-r2)  +DLOG(r2)*DLOG(1-r1)         !! LL part
cc     $     +BVR_Dilog(r1)        +BVR_Dilog(r2)               !! NLL this and all the rest
cc     $     -1d0/2*DLOG(1-r1)**2  -1d0/2*DLOG(1-r2)**2
cc     $     +3d0/2*DLOG(1-r1)     +3d0/2*DLOG(1-r2)
cc     $     +1d0/2*r1*(1-r1)/(1+(1-r1)**2)
cc     $     +1d0/2*r2*(1-r2)/(1+(1-r2)**2)
cc     $     )
cc      V2= V2-(Alfpi*Q**2) *( -0.50d0*cL*DLOG((1d0-r1)*(1d0-r2)) )

      END

      SUBROUTINE  BVR_MakeF1ini(Svar,Mas1,Mas2,Alfpi,Q,F1_1,F1_2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Initial state vertex correction, IR subtracted, in small mass approximation.          //
*//   Second order LL+NLL is included.                                                      //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION   Svar, Mas1,Mas2, Alfpi, Q
      DOUBLE COMPLEX     F1_1, F1_2, cL
      DOUBLE COMPLEX     BVR_CnuA
*
      cL = BVR_CnuA(Svar,Mas1,Mas2) - DCMPLX(1d0)         !!! <-- this is just ln(s/m**2)-i*pi -1
***   cL = DCMPLX( DLOG(Svar/Mas1/Mas2)-1d0, -1d0 )

      F1_1 = (Alfpi*Q**2)   *0.5d0*cL
      F1_2 = F1_1
     $     +(Alfpi*Q**2)**2 *(
     $              +cL**2/8d0 
     $              +cL*( 3d0/32 -3d0/4*m_zeta2 +3d0/2*m_zeta3 ) 
     $     )
*------------------------------------------------------------
* Subleading terms from Berends, Burgers, Van Neerveen
* Cooked up by S.J. long time ago (1989?)
*      dels2 =  charg2**2*alfpi**2  *0.5d0*bilg**2
*     $     +charg2**2*alfpi**2*(
*     $     -(13d0/16d0 +1.5d0*zet2 -3d0*zet3)*bilg
*     $     -16d0/5d0*zet2*zet2 +51d0/8d0*zet2 +13d0/4d0
*     $     -4.5d0*zet3 -6d0*zet2*log(2d0) )
*------------------------------------------------------------
      END

      SUBROUTINE  BVR_MakeF1fin(Svar,Mas1,Mas2,Alfpi,Q,F1_1,F1_2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Final state vertex correction, second order                                           //
*//   finite mass  only for first order part                                                //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION   Svar,Mas1,Mas2,Alfpi,Q
      DOUBLE COMPLEX     F1_1,F1_2,cL
      DOUBLE COMPLEX     BVR_CnuA
*
      cL = BVR_CnuA(Svar,Mas1,Mas2) - DCMPLX(1d0)       !!! <-- this is just ln(s/m**2)-i*pi -1
***   cL = DCMPLX( DLOG(Svar/Mas1/Mas2)-1d0, -1d0 )

      F1_1 = (Alfpi*Q**2)   *0.5d0*cL
      F1_2 = F1_1
     $     +(Alfpi*Q**2)**2 *(
     $              +cL**2/8d0 
     $              +cL*( 3d0/32 -3d0/4*m_zeta2 +3d0/2*m_zeta3 ) 
     $     )
      END

      DOUBLE COMPLEX  FUNCTION BVR_CnuA(Svar,Mas1,Mas2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Function nu*A(nu) Complex version appropriate for s and t-chanels.                    //
*//   No small mass approximation.                                                          //
*//                                                                                         //
*//       s-chanel:  Nu = (-s+m1**2+m2**2)/2 = -p1p2 < 0   p1 and p2 incoming or outgoing   //
*//       t-chanel:  Nu = (-t+m1**2+m2**2)/2 =  p1p2 > 0   p1 incoming p2 outgoing          //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION      Svar,Mas1,Mas2
      DOUBLE COMPLEX        Eps,Nu,Mas12,xlam,z
      DOUBLE COMPLEX        BVR_CDLN
*----------
      Eps = DCMPLX( +1d0,0.d0)   ! plus was adjusted empiricaly
      IF( DABS(Mas1*Mas2) .LT. 1d-10 ) GOTO 900
      Mas12 = DCMPLX(Mas1*Mas2)
      Nu    = DCMPLX( (-Svar +Mas1**2+Mas2**2)/2d0 )
      xlam  = CDSQRT( ( Nu - Mas12)*( Nu + Mas12) )
* take care of numerical stability
      IF( svar .GT. 0d0) THEN
         z = Mas12/(Nu-xlam)
      ELSE
         z = (Nu + xlam)/Mas12
      ENDIF
      BVR_CnuA  = Nu/xlam *BVR_CDLN( z ,Eps)
      RETURN
 900  CONTINUE
      WRITE(*,*) '++++++ BVR_CnuA: STOP due to zero mass: ',Mas1,Mas2
      STOP
      END


      SUBROUTINE BVR_RBoxGG(MasPhot,s,t,u,ReBox,ImBox,ReIR,DelBox)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//   Box Gamma-Gamma, TEST version, no use of COMPLEX                                      //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION      MasPhot,s,t,u,ReBox,ImBox,ReIR,DelBox
      DOUBLE PRECISION      ct,cp,cm,lcm,lcp
*------
      cm = ABS(t/s)
      cp = ABS(u/s)
      ct = cp-cm
      lcm = DLOG(cm)
      lcp = DLOG(cp)
      ReBox = DLOG(cm/cp)*DLOG(MasPhot**2/s) -ct/(2*cp*cp)*0.5d0*lcm**2  +lcm /(2*cp)
      ImBox = DLOG(cm/cp)*m_pi               -ct/(2*cp*cp)*m_pi *lcm     +m_pi/(2*cp)
      ReIR  = DLOG(cm/cp)*DLOG(MasPhot**2/DSQRT(u*t)) +DLOG(cm/cp)/2d0
      DelBox = 
     $     2*DLOG(cm/cp)*DLOG(MasPhot**2/s)
     $    +1/(1+ct*ct)*(2*cp*lcm - 2*cm*lcp -ct*lcm**2 -ct*lcp**2)
      DelBox = DelBox - 2*ReIR
      END

      DOUBLE COMPLEX  FUNCTION BVR_CBoxGG(MasPhot,s,t,u)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Box Gamma-Gamma, taken from   KORAZ/KORALB programs                                   //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION      MasPhot,s,t,u
      DOUBLE COMPLEX  MG,Eps,SS,TT,UU,CiPi,Clnt
      DOUBLE COMPLEX  BVR_Spence,BVR_CDLN
*---------
      Eps = DCMPLX(-1.D0,0.D0)
      MG  = DCMPLX(MasPhot, 0d0)
      SS  = DCMPLX(s, 0d0)
      TT  = DCMPLX(t, 0d0)
      UU  = DCMPLX(u, 0d0)
      CiPi = DCMPLX(0d0, m_pi)
      Clnt = BVR_CDLN( (-TT/SS) ,Eps)
      BVR_CBoxGG= 
     $     BVR_CDLN( (TT/UU) ,Eps) *(BVR_CDLN( MG**2/SS ,Eps) +CiPi) !!!   <-- Infrared part
     $     +DCMPLX(0.5d0)*SS*(UU-TT)/UU**2 *( DCMPLX(0.5d0)*Clnt**2 +CiPi*Clnt)
     $     -DCMPLX(0.5d0)*SS/UU*( Clnt +Cipi)
      END

      DOUBLE COMPLEX  FUNCTION BVR_CBoxGZ(MasPhot,MassZ,GammZ,s,t,u)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// Box Gamma-Z, From W. Brown, R. Decker, E. Pashos, Phys. Rev. Lett., 52 (1984), 1192     //
*// Programmed similarly as in KORALZ                                                       //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION      MasPhot,MassZ,GammZ,s,t,u
      DOUBLE COMPLEX  MZ2,MG2,Eps,SS,TT,UU
      DOUBLE COMPLEX  BVR_Spence,BVR_CDLN
*
      Eps = DCMPLX(-1.D0,0.D0)
      MZ2  = DCMPLX(MassZ**2, -MassZ*GammZ)
      MG2  = DCMPLX(MasPhot**2, 0d0)
      SS  = DCMPLX(s, 0d0)
      TT  = DCMPLX(t, 0d0)
      UU  = DCMPLX(u, 0d0)
      BVR_CBoxGZ = 
     $        BVR_CDLN( (TT/UU) ,Eps) *BVR_CDLN( MG2/CDSQRT(TT*UU) ,Eps) !!!<-- Infrared part
     $     -2*BVR_CDLN( (TT/UU) ,Eps) *BVR_CDLN( ((MZ2-SS)/MZ2 )   ,Eps)
     $     +BVR_Spence( ((MZ2+UU)/MZ2) ,Eps)
     $     -BVR_Spence( ((MZ2+TT)/MZ2) ,Eps)
     $     +(MZ2-SS)*(UU-TT-MZ2)/UU/UU *(
     $              BVR_CDLN( (-TT/SS) ,Eps) *BVR_CDLN( ((MZ2-SS)/MZ2) ,Eps)
     $             +BVR_Spence( ((MZ2+TT)/MZ2) ,Eps)  
     $             -BVR_Spence( ((MZ2-SS)/MZ2) ,Eps)  )
     $     +(MZ2-SS)*(MZ2-SS)/UU/SS *BVR_CDLN( ((MZ2-SS)/MZ2) ,Eps) 
     $     +(MZ2-SS)/UU             *BVR_CDLN( (-TT/MZ2)      ,Eps)
      END


      DOUBLE COMPLEX  FUNCTION BVR_IntReson(MasPhot,MassZ,GammZ,s,t,u)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Resonant part of virtual formfactor                                                   //
*//   Needed for Greco-Pancheri-Srivastava exponentiation                                   //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION      MasPhot,MassZ,GammZ,s,t,u
      DOUBLE COMPLEX  MZ2,MG2,Eps,SS,TT,UU
      DOUBLE COMPLEX  BVR_Spence,BVR_CDLN
*
      Eps = DCMPLX(-1.D0,0.D0)
      MZ2  = DCMPLX(MassZ**2, -MassZ*GammZ)
      MG2  = DCMPLX(MasPhot**2, 0d0)
      SS  = DCMPLX(s, 0d0)
      TT  = DCMPLX(t, 0d0)
      UU  = DCMPLX(u, 0d0)
      BVR_IntReson = 
     $     -2*BVR_CDLN( (TT/UU) ,Eps) *BVR_CDLN( ((MZ2-SS)/MZ2 )   ,Eps)
ccc     $    +BVR_CDLN( (TT/UU) ,Eps) *BVR_CDLN( MG2/CDSQRT(TT*UU) ,Eps)
ccc     $     +BVR_Spence( ((MZ2+UU)/MZ2) ,Eps)
ccc     $     -BVR_Spence( ((MZ2+TT)/MZ2) ,Eps)
      END


      DOUBLE COMPLEX  FUNCTION BVR_IntIR(MasPhot,s,t,u)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Virtual 2*(B(t)-B(u)) Intereference IR part to be subtracted from boxes               //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION      MasPhot,s,t,u
      DOUBLE COMPLEX  BVR_CDLN,Eps,MG2,TT,UU
*-------
      Eps = DCMPLX(-1.D0,0.D0)
      MG2 = DCMPLX(MasPhot**2, 0d0)
      TT  = DCMPLX(t, 0d0)
      UU  = DCMPLX(u, 0d0)
      BVR_IntIR =
     $      BVR_CDLN( (TT/UU) ,Eps) *BVR_CDLN( (MG2/CDSQRT(TT*UU)) ,Eps)
     $     +DCMPLX(0.5d0)*BVR_CDLN( (TT/UU) ,Eps)
      END

      DOUBLE PRECISION  FUNCTION  BVR_SForFac(alfpic, p1,m1, p2,m2, Emin, MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   s-channel YFS formfactor for  acollinear fermion pair.                                //
*//   Mass effects are eaxct.                                                               //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
*
      DOUBLE PRECISION    alfpic, p1(4),m1, p2(4),m2, Emin, MasPhot
      DOUBLE PRECISION    Breal, Bvirt, p1p2
      DOUBLE PRECISION    BVR_Btilda, BVR_SBvirt
*----------
      p1p2  =  p1(4)*p2(4) -p1(3)*p2(3) -p1(2)*p2(2) -p1(1)*p2(1)
      Breal = BVR_Btilda( alfpic, p1p2, p1(4),p2(4), m1, m2,  Emin, MasPhot) !! Exact
      Bvirt = BVR_SBvirt( alfpic, p1p2, m1, m2, MasPhot)                     !! Exact
      BVR_SForFac = EXP( Breal + Bvirt)
      END                       !!! BVR_SForFac !!!


      DOUBLE PRECISION  FUNCTION BVR_TForFac(alfpic, p1,m1, p2,m2, Emin, MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   T-channel YFS formfactor for acollinear fermion pair.                                 //
*//   m1 is assumed to be small, m2 can be finite.                                          //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
*
      DOUBLE PRECISION    alfpic, p1(4),m1, p2(4),m2, Emin, MasPhot, FormFac
      DOUBLE PRECISION    Breal, Bvirt, t, p1p2
      DOUBLE PRECISION    BVR_Btilda, BVR_TBvirt
*----------
      p1p2  =  p1(4)*p2(4) -p1(3)*p2(3) -p1(2)*p2(2) -p1(1)*p2(1)
      t     = -2*p1p2 +m1**2 +m2**2
      Breal   = BVR_Btilda( alfpic, p1p2, p1(4),p2(4), m1, m2,  Emin, MasPhot) !! Exact!!!
      Bvirt   = BVR_TBvirt( alfpic, p1p2, m1, m2, MasPhot) !!!! m1<<m2 approximation here!!!
      BVR_TForFac = EXP( Breal + Bvirt)
      END                       !!! BVR_TForFac !!!


      DOUBLE PRECISION   FUNCTION BVR_SBvirt(alfpic,p1p2,m1,m2,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Real part of B-VIRTUAL S-CHANNEL, Exact.                                              //
*//   Present version according to eq.(12) in uthep-95-0801                                 //
*//   Notation: mu->Nu, mu*(1+rho)->nu+xlam, rho=xlam/nu                                    //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION    alfpic,p1p2,m1,m2,MasPhot
*
      DOUBLE PRECISION     BVR_Dilog,xlam,Nu,s
*-----
      IF( p1p2 .LE. m1*m2 ) GOTO 900
      Nu = p1p2
      s = 2d0*p1p2 +m1*m1 +m2*m2
      xlam = SQRT( Nu**2 -(m1*m2)**2 )
      BVR_SBvirt = alfpic*(
     $      (Nu/xlam *LOG((Nu+xlam)/m1/m2) -1d0) *LOG(MasPhot**2/m1/m2)
     $     +xlam/s *LOG((Nu+xlam)/m1/m2)
     $     +(m1**2-m2**2)/(2d0*s) *LOG(m1/m2)
*###################################################[[[[[[[[[[[[[[[[[[[[[[[[[
ccc     $     +Nu/xlam* m_pi**2                    !!!<---  Important pi**2/beta of Schwinger
     $     +m_pi**2                    !!!<--- temporary solution for the coulomb problem
*###################################################]]]]]]]]]]]]]]]]]]]]]]]]]
     $     +Nu/xlam*( 
     $         -0.5d0*LOG((Nu+xlam)/m1**2) *LOG((Nu+xlam)/m2**2)
     $         -0.5d0 *LOG( (m1*m1 +(Nu+xlam))/(m2*m2 + Nu+xlam) )**2
     $         -BVR_Dilog( 2d0*xlam/(m1*m1 +Nu+xlam) )
     $         -BVR_Dilog( 2d0*xlam/(m2*m2 +Nu+xlam) )
     $     -1d0)
     $     )
      RETURN
 900  CONTINUE
c{{{{{
ccc      CALL  KK2f_Print1(6)
c}}}}}}
      WRITE(*,'(a,5g20.12)') '##### STOP in BVR_SBvirt: p1p2,m1,m2 = ', p1p2,m1,m2
      STOP
      END                       !!!BVR_SBvirt!!!


      DOUBLE PRECISION   FUNCTION BVR_TBvirt(alfpic,p1p2,m1,m2,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Real part of B-VIRTUAL T-CHANNEL  m1 is assumed to be very small.                     //
*//   Present version according to eq.(14) in uthep-95-0801 with COSMETIC changes.          //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION    alfpic,p1p2,m1,m2,MasPhot
*
      DOUBLE PRECISION    t,ta,zeta,BVR_Dilog

      t  = m1*m1 +m2*m2 -2d0*p1p2
      ta = ABS(t)
      zeta = 1+ m2**2/ta
      BVR_TBvirt = alfpic*(
     $     (DLOG(2d0*p1p2/(m1*m2)) -1d0)*DLOG(MasPhot**2/(m1*m2))
     $     +0.5d0*zeta*DLOG(ta*zeta/(m1*m2))
     $     -0.5d0*DLOG(ta/m1**2)*DLOG(ta/m2**2)
     $     +BVR_Dilog(1/zeta) -1d0
     $     +0.5d0*(zeta -1d0)*DLOG(m1/m2)
     $     -DLOG(zeta)*(DLOG(ta/(m1*m2)) +0.5d0*DLOG(zeta))
     $     )
      END                       !!! BVR_TBvirt !!!


      DOUBLE PRECISION   FUNCTION BVR_TBvirt2(alfpic,p1p2,m1,m2,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   B-virtual t-channel  m1 is assumed to be very small.                                  //
*//   Present version according to eq.(14) in uthep-95-0801                                 //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION    alfpic,p1p2,m1,m2,MasPhot
*
      DOUBLE PRECISION    t,ta,zeta,BVR_Dilog

      t  = m1*m1 +m2*m2 -2d0*p1p2
      ta = ABS(t)
      zeta = 1+ m2**2/ta
      BVR_TBvirt2 = alfpic*(
     $     (DLOG(ta/(m1*m2)) + DLOG(zeta) -1d0)*DLOG(MasPhot**2/m1**2)
     $     +0.5d0*zeta*(DLOG(ta/(m1*m2)) + DLOG(zeta))
     $     -0.5d0*DLOG(ta/m1**2)*DLOG(ta/m2**2)
     $     -DLOG(m2/m1)*(DLOG(ta/(m1*m2)) +DLOG(zeta) +.5d0*zeta -1.5d0)
     $     -DLOG(zeta)*(DLOG(ta/(m1*m2)) +0.5d0*DLOG(zeta))
     $     +BVR_Dilog(1/zeta) -1d0
     $     )
      END                       !!! BVR_TBvirt2 !!!


      DOUBLE PRECISION   FUNCTION BVR_TBvirtExact(alfpic,p1p2,m1,m2,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   B-virtual t-channel, masses treated exactly.                                          //
*//   Based on 1986 internal note by SJ.                                                    //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BVR.h'
      DOUBLE PRECISION    alfpic,p1p2,m1,m2,MasPhot
*
      DOUBLE PRECISION    BVR_Dilog,nu,xlam,w1,w2,A,A1,A3,ta
*------------
      nu  = p1p2
      ta  = 2d0*nu -m1*m1 -m2*m2
      xlam = DSQRT((nu-m1*m2)*(nu+m1*m2))
      w1  =  m1**2/(xlam+nu-m1**2)
      w2  =  m2**2/(xlam+nu-m2**2)
      A   =  1d0/xlam *DLOG((nu+xlam)/(m1*m2))
      A1  = -2d0  +(m1*m1 -m2*m2)/(-ta)*DLOG(m1/m2)
     $            -2d0*xlam**2/(-ta)*A
      A3  = DLOG(ta/(m1*m2))*A
     $     +0.5d0/xlam*( -0.5d0*DLOG(w1)**2 +0.5d0*DLOG(1d0+w1)**2
     $                   -DLOG(1d0+w1+w2)*DLOG(w1/(1d0+w1))
     $                   -BVR_Dilog((1d0+w1)/(1d0+w1+w2)) +BVR_Dilog(w1/(1d0+w1+w2)) )
     $     +0.5d0/xlam*( -0.5d0*DLOG(w2)**2 +0.5d0*DLOG(1d0+w2)**2
     $                   -DLOG(1d0+w1+w2)*DLOG(w2/(1d0+w2))
     $                   -BVR_Dilog((1d0+w2)/(1d0+w1+w2)) +BVR_Dilog(w2/(1d0+w1+w2)) )
*
      BVR_TBvirtExact = alfpic*( (nu*A-1d0)*DLOG(MasPhot**2/(m1*m2)) -nu*A3 +0.5d0*A1 )
      END                       !!! BVR_TBvirtExact !!!


      DOUBLE PRECISION   FUNCTION BVR_Btilda(alfpi,p1p2,E1,E2,Mas1,Mas2,Kmax,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//  Full/Complete/exact 2-particle Btilde function.                                        //
*//  Exact treatment of masses, numericaly stable in high energy limit.                     //
*//  Equivalent of routine Btilde programmed by W. Placzek (S.J.)                           //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION    alfpi,p1p2,E1,E2,Mas1,Mas2,Kmax,MasPhot
      DOUBLE PRECISION    BVR_A, BVR_A4, BVR_A4sng
*-------------
      BVR_Btilda = alfpi*(
     $     (p1p2*BVR_A( p1p2,Mas1,Mas2) -1 )*2*LOG(2*Kmax/MasPhot)
     $     +p1p2*BVR_A4(p1p2,E1,E2,Mas1,Mas2)
     $     -0.5d0*Mas1**2*BVR_A4sng(E1,Mas1)
     $     -0.5d0*Mas2**2*BVR_A4sng(E2,Mas2)
     $     )
      END


      DOUBLE PRECISION    FUNCTION BVR_Btildc(alfpi,p1p2,E1,E2,Mas1,Mas2,Kmax,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// Crude/Truncated (crude MC) 2-particle Btilde equivalent S.J.                            //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION    alfpi,p1p2,E1,E2,Mas1,Mas2,Kmax,MasPhot
      DOUBLE PRECISION    BVR_A, BVR_A4
*-------------
      BVR_Btildc = alfpi*(
     $      p1p2*BVR_A( p1p2      ,Mas1,Mas2)*2*LOG(2*Kmax/MasPhot)
     $     +p1p2*BVR_A4(p1p2,E1,E2,Mas1,Mas2)
     $     )
      END

      DOUBLE PRECISION  FUNCTION BVR_A(p1p2,Mas1,Mas2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//   Function A(p1,p2) real version appropriate for B-tilde calculation                    //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION     p1p2,Mas1,Mas2
      DOUBLE PRECISION     Mas12,xlam
*----------
      Mas12 = Mas1*Mas2
      IF ( (p1p2-Mas12) .LT. 1d-10) THEN
         BVR_A=0d0
         WRITE(*,*) '+++++++ BVR_A:: WARNING, p1p2 = ',p1p2
         RETURN
      ENDIF
      xlam = SQRT( (p1p2 - Mas12)*(p1p2 + Mas12) )
      BVR_A  = 1/xlam *LOG( (p1p2 + xlam)/Mas12 )
      END

      DOUBLE PRECISION   FUNCTION BVR_A4sng(E1,Mas1)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// Function (p1*p1)*A4(p1,p1) equal argument momenta.                                      //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION   E1,Mas1
      DOUBLE PRECISION   bet1,b1ln
*-------
      bet1 = SQRT(1-Mas1**2/E1**2)
      b1ln = 2*LOG( (1+bet1)*E1/Mas1 )
      BVR_A4sng = -1/Mas1**2/bet1 *b1ln
      END

      DOUBLE PRECISION   FUNCTION BVR_A4(p1p2,En1,En2,xm1,xm2)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//! This function provides an analytical result for the integral         !                 //
*//! A4(p1,p2) being a part of the YFS IR function B-tilde.               !                 //
*//! Note: This is a general case without any approximation!              !                 //
*//! INPUT: p1p2    - scalar product of the 4-momenta p1 and p2;          !                 //
*//!        E1,E2   - particles energies;                                 !                 //
*//!        xm1,xm2 - particles masses;                                   !                 //
*//!----------------------------------------------------------------------!                 //
*//! Written by:  Wieslaw Placzek                Knoxville, January  1996 !                 //
*//! Last update: 30.01.1996                by: W.P.                      !                 //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION     p1p2,En1,En2,xm1,xm2
      DOUBLE PRECISION     E1,E2,Mas1,Mas2,p1s,p2s,p2,Ep,Em,sm,dm,Q2,xl,xq,qp,qm
      DOUBLE PRECISION     et0,et1,vet0,vet1,Eln,y1,y2,y3,y4
      DOUBLE PRECISION     BVR_Yijeta
! Statement function
      DOUBLE PRECISION     etaln,x1,x2,x3,x4,z
      etaln(x1,x2,x3,x4,z) = LOG(ABS( (z-x1)*(z-x2)/(z-x3)/(z-x4) ))
*-----------
! Some auxiliary variables
      E1 = En1
      E2 = En2
      Mas1 = xm1
      Mas2 = xm2
      p1s = E1**2 - Mas1**2
      p2s = E2**2 - Mas2**2
      IF (p1s.lt.p2s) THEN
        Mas1 = xm2
        Mas2 = xm1
        E1 = En2
        E2 = En1
      ENDIF
      Ep  = E1 + E2
      Em  = E1 - E2
      sm  = Mas1 + Mas2 
      dm  = Mas1 - Mas2
      Q2  = 2*p1p2 - Mas1**2 - Mas2**2
      xl  = SQRT( (Q2 + sm**2)*(Q2 + dm**2) )
      xq  = SQRT(Q2 + Em**2)
      qp = xq + Em
      qm = xq - Em
      et0 = SQRT(E2**2 - Mas2**2)
      IF (p1p2.gt.E1*E2) et0 = -et0
      et1 = SQRT(E1**2 - Mas1**2) + xq
      y1  = 0.5d0*( (xq - Ep) + (sm*dm + xl)/qp )
      y2  = y1 - xl/qp
      y3  = 0.5d0*( (xq + Ep) + (sm*dm + xl)/qm )
      y4  = y3 - xl/qm       
! Some auxiliary functions
      IF (ABS(Em).gt.1d-10) THEN
        Eln = LOG(ABS(qm/qp))*( etaln(y1,y4,y2,y3,et1) 
     &                        - etaln(y1,y4,y2,y3,et0) )
      ELSE
        Eln = 0
      ENDIF
      Vet0 = BVR_Yijeta(y1,y4,et0) + BVR_Yijeta(y2,y1,et0)
     &     + BVR_Yijeta(y3,y2,et0) - BVR_Yijeta(y3,y4,et0)
     &     + 0.5d0*etaln(y1,y2,y3,y4,et0)*etaln(y2,y3,y1,y4,et0)
      Vet1 = BVR_Yijeta(y1,y4,et1) + BVR_Yijeta(y2,y1,et1)
     &     + BVR_Yijeta(y3,y2,et1) - BVR_Yijeta(y3,y4,et1)
     &     + 0.5d0*etaln(y1,y2,y3,y4,et1)*etaln(y2,y3,y1,y4,et1)
! Function A4(p1,p2) 
      BVR_A4 = 1/xl*(Eln + Vet1 - Vet0 )
      END

      DOUBLE PRECISION   FUNCTION BVR_Yijeta(yi,yj,eta)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*/ !----------------------------------------------------------------------!                 //
*/ ! Some auxiliary function (combination of Logs and Dilogs) used in     !                 //
*/ ! the function A4anal for A4(p1,p2).                                   !                 //
*/ !----------------------------------------------------------------------!                 //
*/ ! Written by:  Wieslaw Placzek                Knoxville, January  1996 !                 //
*/ ! Last update: 30.01.1996                by: W.P.                      !                 //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION    yi,yj,eta
      DOUBLE PRECISION    BVR_Dilog
*-----
      BVR_Yijeta = 2*BVR_Dilog( ( yj-yi)/(eta-yi) ) 
     &         + 0.5d0*LOG(ABS( (eta-yi)/(eta-yj) ))**2
      END



      DOUBLE PRECISION FUNCTION BVR_Btilde(alfinv,p1,p2,am1,am2,aKmax,MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//  UNUSED!!!, Btilde is replaced by the equivalent function Btildf                        //
*//                                                                                         //
*//  !----------------------------------------------------------------------!               //
*//  ! This function provides a value of YFS real photon IR function        !               //
*//  ! B-tilde for any pair of charged particles.                           !               //
*//  ! INPUT: p1,p2   - particles 4-momenta;                                !               //
*//  !        am1,am2 - particles masses;                                   !               //
*//  !        MasPhot   - 'photon mass' (IR regulator)                      !               //
*//  !        aKmax   - maximum soft photon energy [GeV]                    !               //
*//  !----------------------------------------------------------------------!               //
*//  ! Written by:  Wieslaw Placzek                Knoxville, January  1996 !               //
*//  ! Last update: 30.01.1996                by: W.P.                      !               //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      DOUBLE PRECISION pi
      PARAMETER ( pi = 3.1415926535897932D0)
      DOUBLE PRECISION  p1(4),p2(4)
      DOUBLE PRECISION  BVR_A4
      DOUBLE PRECISION  alfpi,bet1,bet2,xlam,a,a4,btian,b1ln,b2ln,p1p2
      DOUBLE PRECISION  am2,akmax,alfinv,am1,e2,am12,masphot,e1
*---------------------------------------------------------------------------------------------
      alfpi = 1/alfinv/pi
      BVR_Btilde = 0
      E1 = p1(4)
      E2 = p2(4)
      am12 = am1*am2
      p1p2 = p1(4)*p2(4) - p1(3)*p2(3) - p1(2)*p2(2) - p1(1)*p2(1)
      IF (p1p2-am12.lt.1d-10) RETURN
      xlam = SQRT( (p1p2 - am12)*(p1p2 + am12) )
! Function A(p1,p2)
      A  = 1/xlam *LOG( (p1p2 + xlam)/am12 )
      bet1 = SQRT(1-am1**2/E1**2)
      bet2 = SQRT(1-am2**2/E2**2)
      b1ln = 2*LOG( (1+bet1)*E1/am1 )
      b2ln = 2*LOG( (1+bet2)*E2/am2 )
! Function A4(p1,p2)
      A4 = BVR_A4(p1p2,E1,E2,am1,am2)
! B-tilde(p1,p2;aKmax,MasPhot)
      Btian = (p1p2*A - 1) *2*LOG(2*aKmax/MasPhot)
     &       + p1p2*A4 + 0.5/bet1*b1ln + 0.5/bet2*b2ln
      BVR_Btilde = alfpi*Btian
      END

      SUBROUTINE BVR_wform( CMSene,alfinv,q1,q2,amf,delta,eps,dyfs)
*/////////////////////////////////////////////////////////////////////////////////////////////
*// UNUSED, kept for some future tests, small mass approx.                                  //
*// Yennie-Frautschi-Suura formfactors for the final state ferm. pair                       //
*// S.J. 1987                                                                               //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION CMSene,alfinv,q1(4),q2(4),amf,delta,eps,dyfs

      DOUBLE PRECISION pi
      PARAMETER (pi=3.1415926535897932d0)
      DOUBLE PRECISION alf1,q1q2,em,ep,remn,dl,e1,e2,ene,svar,delb,gamf2
      DOUBLE PRECISION bvr_dilog
*---------------------------------------------------------------------------------------------
      alf1=1/alfinv/pi
      svar  = cmsene**2
      ene= cmsene/2
* momenta q1,q2 should be in cms
      e1 = q1(4)
      e2 = q2(4)
      gamf2 = 2*alf1* dlog(svar /amf**2)
      DelB  = gamf2*dlog(ene/SQRT(e1*e2)*eps/delta)
      ep    = e1+e2
      em    = e1-e2
      q1q2  = q1(4)*q2(4)-q1(3)*q2(3)-q1(2)*q2(2)-q1(1)*q2(1)
      dl    = SQRT( 2*q1q2 +em**2 )
      remn  = pi**2/2
     $        -0.50d0*dlog(e1/e2)**2
     $        -0.25d0*dlog((dl+em)**2/(4*e1*e2))**2
     $        -0.25d0*dlog((dl-em)**2/(4*e1*e2))**2
     $        - BVR_Dilog((dl+ep)/(dl+em)) -BVR_Dilog((dl-ep)/(dl-em))
     $        - BVR_Dilog((dl-ep)/(dl+em)) -BVR_Dilog((dl+ep)/(dl-em))
      dyfs  = exp( DelB +alf1*remn )
      END



      DOUBLE PRECISION FUNCTION BVR_Dilog(x)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// dilogarithm FUNCTION: dilog(x)=int( -ln(1-z)/z ) , 0 < z < x .                          //
*// this is the cernlib version.                                                            //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  x
*
      DOUBLE PRECISION  a,b,y,s,t,z
*---------------------------------------------------------------------------------------------
      z=-1.644934066848226d0
      IF(x  .LT. -1.d0) GOTO 1
      IF(x  .LE.  0.5d0) GOTO 2
      IF(x  .EQ.  1.d0) GOTO 3
      IF(x  .LE.  2.d0) GOTO 4
      z=3.289868133696453d0
    1 t=1.d0/x
      s=-0.5d0
      z=z-0.5d0*dlog(dabs(x))**2
      GOTO 5
    2 t=x
      s=0.5d0
      z=0.d0
      GOTO 5
    3 BVR_Dilog=1.644934066848226d0
      RETURN
    4 t=1.d0-x
      s=-0.5d0
      z=1.644934066848226d0-dlog(x)*dlog(dabs(t))
    5 y=2.666666666666667d0*t+0.666666666666667d0
      b=      0.000000000000001d0
      a=y*b  +0.000000000000004d0
      b=y*a-b+0.000000000000011d0
      a=y*b-a+0.000000000000037d0
      b=y*a-b+0.000000000000121d0
      a=y*b-a+0.000000000000398d0
      b=y*a-b+0.000000000001312d0
      a=y*b-a+0.000000000004342d0
      b=y*a-b+0.000000000014437d0
      a=y*b-a+0.000000000048274d0
      b=y*a-b+0.000000000162421d0
      a=y*b-a+0.000000000550291d0
      b=y*a-b+0.000000001879117d0
      a=y*b-a+0.000000006474338d0
      b=y*a-b+0.000000022536705d0
      a=y*b-a+0.000000079387055d0
      b=y*a-b+0.000000283575385d0
      a=y*b-a+0.000001029904264d0
      b=y*a-b+0.000003816329463d0
      a=y*b-a+0.000014496300557d0
      b=y*a-b+0.000056817822718d0
      a=y*b-a+0.000232002196094d0
      b=y*a-b+0.001001627496164d0
      a=y*b-a+0.004686361959447d0
      b=y*a-b+0.024879322924228d0
      a=y*b-a+0.166073032927855d0
      a=y*a-b+1.935064300869969d0
      BVR_Dilog=s*t*(a-b)+z
      END

      DOUBLE COMPLEX  FUNCTION BVR_Spence(Y,E)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//  Spence function of y+i*REAL(E) where E is an infinitesimal                             //
*//  Programmed probably by R. Stuart                                                       //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      DOUBLE COMPLEX  Y,E
      DOUBLE PRECISION  B(9),FACT
      DOUBLE COMPLEX  A,CLN,PISQ6,PROD,TERM,X,Z,ZSQ
      DOUBLE COMPLEX  BVR_CDLN
      INTEGER    j,i1,i2
*---------------------------------------------------------------------------------------------
      B(1)=1.D0/6.D0
      B(2)=-1.D0/30.D0
      B(3)=1.D0/42.D0
      B(4)=B(2)
      B(5)=5.D0/66.D0
      B(6)=-691.D0/2730.D0
      B(7)=7.D0/6.D0
      B(8)=-3617.D0/510.D0
      B(9)=43867.D0/798.D0
      PISQ6=(1.6449340668482264D0,0.D0)
      I1=0
      I2=0
      X=Y
      A=E
      IF(X.EQ.(0.D0,0.D0))THEN
        BVR_Spence=(0.D0,0.D0)
        RETURN
      ENDIF
      IF(X.EQ.(1.D0,0.D0))THEN
        BVR_Spence=PISQ6
        RETURN
      ENDIF
C  IF X LIES OUTSIDE THE UNIT CIRCLE THEN EVALUATE BVR_Spence(1/X)
      IF(CDABS(X).GT.1.D0)THEN
        X=1.D0/X
        A=-A
        I1=1
      ENDIF
C  IF REAL(X)>1/2 THEN EVALUATE BVR_Spence(1-X)
      IF(DREAL(X).GT.0.5D0)THEN
        X=1.D0-X
        A=-A
        I2=1
      ENDIF
C  EVALUATE SERIES FOR BVR_Spence(X)
      Z=-BVR_CDLN(1.D0-X,-A)
      ZSQ=Z*Z
      BVR_Spence=Z-ZSQ/4.D0
      PROD=Z
      FACT=1.D0
      DO 10 J=2,18,2
      FACT=FACT*DCMPLX(DBLE((J+1)*J))
      PROD=PROD*ZSQ
      TERM=B(J/2)/FACT*PROD
      BVR_Spence=BVR_Spence+TERM
      IF(CDABS(TERM/BVR_Spence).LT.1.D-20) GOTO 20
10    CONTINUE
C  ADD APPROPRIATE LOGS TO OBTAIN SPENCE FUNCTION OF ORIGINAL ARGUEMENT
20    IF(I2.EQ.1)THEN
        BVR_Spence=-BVR_Spence+PISQ6-BVR_CDLN(X,A)*BVR_CDLN(1.D0-X,-A)
        X=1.D0-X
        A=-A
      ENDIF
      IF(I1.EQ.1)THEN
        CLN=BVR_CDLN(-X,-A)
        BVR_Spence=-BVR_Spence-PISQ6-CLN*CLN/2.D0
      ENDIF
      RETURN
      END

      DOUBLE COMPLEX  FUNCTION BVR_CDLN(X,A)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//  Complex functions that take account of the I*Epsilon prescription                      //
*//  Complex logarithm of X+I*REAL(A) where a is an infinitesimal                           //
*//  Programmed probably by R. Stuart                                                       //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE COMPLEX  A,X
      DOUBLE COMPLEX  PI
*--------
      PI=(3.141592653589793238462643D0,0.D0)
      IF(DIMAG(X) .EQ. 0.D0 .AND. DREAL(X) .LE. 0.D0) THEN
         BVR_CDLN =CDLOG(-X) +(0.D0,1.D0)*PI*DSIGN(1.D0,DREAL(A))
      ELSE
         BVR_CDLN =CDLOG(X)
      END IF
      IF(DIMAG(BVR_CDLN).GT.DREAL(PI) ) BVR_CDLN =BVR_CDLN -(0.D0,1.D0)*PI
      IF(DIMAG(BVR_CDLN).LT.DREAL(-PI)) BVR_CDLN =BVR_CDLN +(0.D0,1.D0)*PI
      RETURN
      END


*/////////////////////////////////////////////////////////////////////////////////////////////
*//                   End of class BVR                                                      //
*/////////////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//                         Pseudo-CLASS  GPS                                       //
*//           ***     will be  Pseudo-CLASS  CEEX    ***                            //
*//                                                                                 //
*//             Purpose:  Calculate CEEX spin amplitudes                            //
*//                                                                                 //
*//    WARNIG:  !!!!!!!!!!!!!!!!!!!!!!![[[[[[[[]]]]]]]!!!!!!!!!!!!!!!               //
*//    Function BVR_SBvirt modified temporarily to avoid problem with pi**2/beta    //
*//    Not important for anything in practice                                       //
*//    WARNIG:  !!!!!!!!!!!!!!!!!!!!!!![[[[[[[[]]]]]]]!!!!!!!!!!!!!!!               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE GPS_Initialize
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    Class initialization                                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INCLUDE 'BXformat.h'
      INCLUDE 'GPS.h'
      INTEGER j1,j2,k,KeyPia
      INTEGER init
      DOUBLE PRECISION    alfa

      SAVE    init
      DATA init/0/
*------------------------------------
      IF(init .EQ. 1) RETURN
      init = 1
*------------------------------------
      m_out    = 16
*
*/////////////////////////////////////////////////////////////////////////////////////
*//                        Import from   BornV                                      //
*/////////////////////////////////////////////////////////////////////////////////////
* EW parameters
      CALL BornV_GetSwsq(  m_Sw2 )
      CALL BornV_GetGmu(   m_Gmu)
      CALL BornV_GetMZ(    m_MZ)
      CALL BornV_GetGammZ( m_GammZ)
      CALL BornV_GetAlfInv(m_AlfInv)
      alfa   = 1d0/m_AlfInv
      m_e_QED  = DSQRT( 4d0*m_pi*alfa)
      m_Alfpi  = alfa/m_pi
      CALL BornV_GetKeyElw(m_KeyElw)
      CALL BornV_GetKeyZet(m_KeyZet)
*
      CALL KK2f_GetKeyISR(m_KeyISR)
      CALL KK2f_GetKeyFSR(m_KeyFSR)
      CALL KK2f_GetKeyINT(m_KeyINT)
      CALL KK2f_GetKeyGPS(m_KeyGPS)
      CALL KK2f_GetVcut(  m_Vcut)
*
*/////////////////////////////////////////////////////////////////////////////////////
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '  GPS   Initializator                '
      WRITE(m_out,bxl1f) m_MZ    ,   'Z mass     [GeV]   ','MZ    ','a1'
      WRITE(m_out,bxl1f) m_GammZ,    'Z width    [GeV]   ','GammZ ','a2'
      WRITE(m_out,bxl1f) m_Sw2,      'sin(theta_w)**2    ','Sw2   ','a3'
      WRITE(m_out,bxl1f) m_AlfInv,   '1/alfa_QED  at  Q=0','AlfInv','a4'
      WRITE(m_out,bxtxt) 'Test switches:                         '
      WRITE(m_out,bxl1i) m_KeyZet,   'Z on/off   switch  ','KeyZet','a5'
      WRITE(m_out,bxl1i) m_KeyElw,   'Electroweak lib.   ','KeyElw','a6'
      WRITE(m_out,bxl1i) m_KeyGPS,   'CEEX level         ','KeyGPS','a7'
      WRITE(m_out,bxl1i) m_KeyISR,   'ISR emission       ','KeyISR','a8'
      WRITE(m_out,bxl1i) m_KeyFSR,   'FSR emission       ','KeyFSR','a9'
      WRITE(m_out,bxl1i) m_KeyINT,   'ISR*FSR interferenc','KeyINT','a10'
      WRITE(m_out,bxclo)
*/////////////////////////////////////////////////////////////////////////////////////
* Key for switching on/off the use of m_b, can be reset with GPS_SetKeyArb
      m_KeyArb = 0  ! default zero value, m_b=Xi is assumed
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '     Initialization of GPS class      '
      WRITE(m_out,bxclo)
********************************
*     Define Pauli matrices
      DO k = 0,3
         DO j1 = 1,2
            DO j2 = 1,2
               m_Pauli( k,j1,j2) = DCMPLX(0d0,0d0)
            ENDDO
         ENDDO
      ENDDO
* Sigma0
      m_Pauli( 0,1,1) = DCMPLX( 1d0, 0d0)
      m_Pauli( 0,2,2) = DCMPLX( 1d0, 0d0)
* SigmaX
      m_Pauli( 1,1,2) = DCMPLX( 1d0, 0d0)
      m_Pauli( 1,2,1) = DCMPLX( 1d0, 0d0)
* SigmaY
      m_Pauli( 2,1,2) = DCMPLX( 0d0,-1d0)
      m_Pauli( 2,2,1) = DCMPLX( 0d0, 1d0)
* SigmaZ
      m_Pauli( 3,1,1) = DCMPLX( 1d0, 0d0)
      m_Pauli( 3,2,2) = DCMPLX(-1d0, 0d0)
*
* The other notation for 4-vector index
      DO k = 1,3
         DO j1 = 1,2
            DO j2 = 1,2
               m_Pauli4( k,j1,j2) = m_Pauli( k,j1,j2)
            ENDDO
         ENDDO
      ENDDO
      DO j1 = 1,2
         DO j2 = 1,2
            m_Pauli4( 4,j1,j2) = m_Pauli( 0,j1,j2)
         ENDDO
      ENDDO
********************************
*     Define GPS vectors in CMS
*     Note that they define uniquely the inner products
*     and spin quantization axises in fermion rest frame
      DO k = 1,4
         m_Xi(k)  = 0d0
         m_Eta(k) = 0d0
      ENDDO
      m_Xi( 4)  =  1d0
      m_Xi( 1)  =  1d0
      m_Eta(2)  =  1d0
* axial vectors (arbitrary lightlike ) for photon polarization
      m_b1( 1)   =  0.8723d0
      m_b1( 2)   = -0.7683d0
      m_b1( 3)   =  0.3348d0
      m_b1( 4)   =  DSQRT(m_b1( 1)**2 + m_b1( 2)**2 +m_b1( 3)**2)
* another random setting
      m_b2( 1)   = -0.78833d0
      m_b2( 2)   =  0.34788d0
      m_b2( 3)   = -0.33282d0
      m_b2( 4)   =  DSQRT(m_b2( 1)**2 + m_b2( 2)**2 +m_b2( 3)**2)
* this setting is very close to b=Xi, for special tests
      m_b3( 1)   =  1d0
      m_b3( 2)   =  1d-7
      m_b3( 3)   =  0d0
      m_b3( 4)   =  DSQRT(m_b3( 1)**2 + m_b3( 2)**2 +m_b3( 3)**2)
*
* default setting to m_b1, can be changed with CALL GPS_Setb2,3
      DO k=1,4
         m_b( k) = m_b1( k)
      ENDDO
*
      CALL KarFin_GetKeyPia(KeyPia)
      IF( (m_KeyFSR .EQ. 1) .AND.  (KeyPia .EQ. 0) ) THEN
         WRITE(*,*) ' +++++ STOP in GPS: you cannot have KeyPia=0 for GPS and FSR '
         STOP
      ENDIF
*
      END                       !!!end of GPS_Initialize!!!


      SUBROUTINE GPS_Make
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Spin amplitudes O(alf1) and O(alf0) for exponentiation.                       //
*//   Helicities of emitted photons are choosen randomly!!!                         //
*//                                                                                 //
*//   INPUT: is transfered from  BornV and KK2f using Getters                       //
*//                                                                                 //
*//   OUTPUT:                                                                       //
*//   m_AmpExpo0 =  O(alf0) spin amplitudes                                         //
*//   m_AmpExpo1 =  O(alf1) spin amplitudes                                         //
*//   m_AmpExpo2 =  O(alf2) spin amplitudes                                         //
*//   m_AmpExpo2p=  O(alf2) spin amplitudes with virtual pairs                      //
*//   Wt0     =  O(alf0) exponentiation weight                                      //
*//   Wt1     =  O(alf1) exponentiation weight                                      //
*//                                                                                 //
*//   COMMON working space:                                                         //
*//   m_AmpExpo*  is working space, used by HiniPlus, HfinPlus, HfinMinus           //
*//   m_AmpBorn   is working space, used by HfinMinus                               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER               KFi,KFf,Nphot
      DOUBLE PRECISION      p1(4),p2(4),p3(4),p4(4),Phot(m_phmax,4)
*
      DOUBLE PRECISION      ph(4),ph1(4),ph2(4),PX(4),PP(4),QQ(4)
      INTEGER               i,j,k,l,n,j1,j2,last,loop,loop2,Hel,Hel1,Hel2
      DOUBLE PRECISION      ChaIni,ChaFin
      DOUBLE PRECISION      svar,svarX,svarX1,svarQ,Ene,betaf
      DOUBLE COMPLEX        GPS_soft,GPS_softb
      DOUBLE COMPLEX        Sini(2,m_phmax),Sfin(2,m_phmax)
      DOUBLE COMPLEX        CKine,sProd,Sactu,Sactu1,Sactu2,Cfact0,Cfact2,SactuA,SactuB
      DOUBLE PRECISION      Fleps,Massf,Mbeam,m1,m2,m3,m4,mph
      DOUBLE PRECISION      XborSum,XboxSum,CrudSum,Exp0Sum,Exp1Sum
      DOUBLE PRECISION      Xborn,Xboxy
      DOUBLE PRECISION      DistCru,BornCru,fLLux
      DOUBLE PRECISION      CrudNorm, ExpoNorm
      DOUBLE PRECISION      RhoCru3
      DOUBLE PRECISION      alfQED,alfpini,alfpfin, alfpmix, Emin, MasPhot
      DOUBLE PRECISION      BVR_SForFac,BVR_TForFac
      DOUBLE PRECISION      Yisr,Yfsr,Yint
      DOUBLE PRECISION      YFSkonIni, YFSkonFin, YFS_IRini, YFS_IRfin, YFS_isr, YFS_fsr
      DOUBLE PRECISION      BornV_GetMass, BornV_GetCharge, BornV_Simple,BornV_Differential
      DOUBLE PRECISION      Wt0,Wt1
      DOUBLE PRECISION      dummy
      INTEGER               BornV_GetColor, NCf
      INTEGER               iSaveCPU
*-----------------------------------------------
      INTEGER   Icont
      SAVE      Icont
      DATA      Icont /0/
*-----------------------------------------------
      CALL GPS_Initialize
*
      CALL KK2f_GetKFini(   KFi)     ! Normaly for beam KFi=11 is electron
      CALL KarLud_GetKFfin( KFf)     ! Actual KFcode of the final fermion
      Mbeam  =  BornV_GetMass(   KFi)
      Massf  =  BornV_GetMass(   KFf)
      ChaIni =  BornV_GetCharge( KFi)
      ChaFin =  BornV_GetCharge( KFf)
      NCf    =  BornV_GetColor(  KFf)

      Fleps =  1d-100
      m1  = Mbeam
      m2  = Mbeam
      m3  = Massf
      m4  = Massf
      mph = Fleps
      CALL KK2f_GetBeams(    p1,p2)
      CALL KK2f_GetFermions( p3,p4)
      CALL KK2f_GetPhotAll(Nphot,Phot) ! ordered in energy
      CALL KK2f_GetEmin( Emin)
      CALL KK2f_GetMasPhot(MasPhot)
*-------------------------------------------------------------
      DO k=1,4
         PP(k) = p1(k)+p2(k)
         QQ(k) = p3(k)+p4(k)
      ENDDO
      svar  = PP(4)**2 - PP(3)**2 - PP(2)**2 - PP(1)**2
      svarQ = QQ(4)**2 - QQ(3)**2 - QQ(2)**2 - QQ(1)**2
      Ene   = SQRT(svar)/2d0
*-------------------------------------------------------------
* Overall normalization factors
      CrudNorm  =  1d0
      ExpoNorm  =  2d0/(4d0*m_pi)*NCf  ! it is still quasi-empirical...
*////////////////////////////////////////////////////////////////////////////////////
*//                         YFS  FormFactors                                       //
*//  Note that FSR formfactor below cannot be used for KeyPia=0, Emin is in CMS!!! //
*////////////////////////////////////////////////////////////////////////////////////
      alfQED   = m_e_QED**2/(4d0*m_pi)
      alfpini  = m_Alfpi*ChaIni**2
      alfpfin  = m_Alfpi*ChaFin**2
      alfpmix  = m_Alfpi*ChaFin*ChaIni
      IF( m_KeyISR .NE. 0) THEN
         CALL  BornV_GetYFS_IR( YFS_IRini )
         CALL  BornV_GetYFSkon( YFSkonIni )
****>>   YFS_isr =  YFS_IRini*YFSkonIni
         YFS_isr =  YFS_IRini       !!!<<**  YFSkon not in WtCrud (historical reasons)
         CrudNorm  = CrudNorm *YFS_isr
         Yisr= BVR_SForFac( alfpini, p1,Mbeam, p2,Mbeam, Emin, MasPhot)
         ExpoNorm  = ExpoNorm *Yisr
      ENDIF
      IF( m_KeyFSR .NE. 0) THEN
         CALL KarFin_GetYFS_IR( YFS_IRfin )
         CALL KarFin_GetYFSkon( YFSkonFin )
****>>   YFS_fsr =  YFS_IRfin*YFSkonFin
         YFS_fsr =  YFS_IRfin       !!!<<** YFSkon not in WtCrud (historical reasons)
         CrudNorm  = CrudNorm *YFS_fsr
         Yfsr= BVR_SForFac( alfpfin, p3,Massf, p4,Massf, Emin, MasPhot)
         ExpoNorm  = ExpoNorm *Yfsr
      ENDIF
* Remember Yint depends on Emin and provides angular asymmetry (MasPhot is dummy)
      IF(  m_KeyINT .NE. 0 .AND. m_KeyISR .NE. 0 .AND.  m_KeyFSR .NE. 0  ) THEN
         Yint= BVR_TForFac( alfpmix, p1,Mbeam, p3,Massf, Emin, MasPhot)
     $        *BVR_TForFac( alfpmix, p2,Mbeam, p4,Massf, Emin, MasPhot)
     $        *BVR_TForFac(-alfpmix, p1,Mbeam, p4,Massf, Emin, MasPhot)
     $        *BVR_TForFac(-alfpmix, p2,Mbeam, p3,Massf, Emin, MasPhot)
         ExpoNorm  = ExpoNorm *Yint
      ENDIF
*//////////////////////////////////////////////////////////////////
*//                       S-factors                              //
*//////////////////////////////////////////////////////////////////
* List of soft-factors, note that we calculate them for helicity=+1
* The other one helicity=-1 is just minus complex conjugate!
* Sig = 3-2*Hel, Hel=1,2 --> Sig=1,-1
      DO j=1,nphot
         CrudNorm = CrudNorm *1d0/(2d0*m_pi)**3    !!<-- photon phase-space factor
         ExpoNorm = ExpoNorm *1d0/(2d0*m_pi)**3    !!<-- photon phase-space factor
         DO k=1,4
            ph(k) = Phot(j,k)
         ENDDO
         IF( m_KeyArb .EQ. 0 ) THEN
            Sini(1,j)  =  DCMPLX(ChaIni*m_e_QED) *GPS_soft(  1,ph,p1,p2)
            Sfin(1,j)  = -DCMPLX(ChaFin*m_e_QED) *GPS_soft(  1,ph,p3,p4)
         ELSE
            Sini(1,j)  =  DCMPLX(ChaIni*m_e_QED) *GPS_softb( 1,ph,p1,m1,p2,m2)
            Sfin(1,j)  = -DCMPLX(ChaFin*m_e_QED) *GPS_softb( 1,ph,p3,m3,p4,m4)
         ENDIF
         Sini(2,j) = -DCONJG(Sini(1,j))
         Sfin(2,j) = -DCONJG(Sfin(1,j))
      ENDDO
*//////////////////////////////////////////////////////////////////
*//             Define (randomly) photon helicities              //
*//////////////////////////////////////////////////////////////////
* Photon gelicities are now generated in the main module KK2f and imported here
* This is better in case of multiple calls of GPS for the single event
      CALL KK2f_GetPhel(m_Phel)
***   CALL GPS_PhelRandom(nphot)
***   WRITE(*,'(a,20i2)') 'Phel=   ',(m_Phel(j),j=1,nphot)
      CALL GPS_BornZero(m_AmpExpo0)
      CALL GPS_BornZero(m_AmpExpo1)
      CALL GPS_BornZero(m_AmpExpo2)
      CALL GPS_BornZero(m_AmpExpo2p)
      CrudSum = 0d0
      XborSum = 0d0
      XboxSum = 0d0
      iSaveCPU = 0
*//////////////////////////////////////////////////////////////////////
*//  *************************************************************   //
*//              LOOP OVER PARTITIONS STARTS HERE                    //
*//  Initialize loop over partitions, m_isr(j)=1,0 denotes isr,fsr   //
*//  *************************************************************   //
*//////////////////////////////////////////////////////////////////////
      CALL GPS_PartitionStart(nphot,last)
      DO loop=1,10000000
*        Calculate reduced 4-momentum to be used in gamma/Z propagator
         DO k=1,4
            PX(k) = p1(k)+p2(k)
         ENDDO
*   /////////////////////////////////////////////////////////
*   //         ===============================             //
*   //         Soft-part, soft-factors, m-zero             //
*   //         ===============================             //
*   /////////////////////////////////////////////////////////
         sProd=DCMPLX(1d0,0d0)
         DO j=1,nphot
            Hel  = m_Phel(j)+1
            Sactu  = m_isr(j)*Sini(Hel,j) + (1-m_isr(j))*Sfin(Hel,j)
            sProd  = sProd*Sactu
            DO k=1,4
               PX(k) = PX(k) -m_isr(j)*Phot(j,k)
            ENDDO
         ENDDO
         svarX = PX(4)**2 - PX(3)**2 - PX(2)**2 - PX(1)**2
         Cfact0 = sProd  *(svarX/svarQ)
         CALL GPS_BornPlus(iSaveCPU,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4, Cfact0,Xborn,Xboxy)
*        ==================================================================================
*****    IF(nphot.EQ.0) CALL GPS_BPrint(6,' AmpBoxy ',m_AmpBoxy)
*****    IF(nphot.EQ.0) WRITE(*,'(a,5g20.10)') '#######>>> Boxy/Born = ',Xborn
*****    CALL GPS_BPrint(6,' AmpExp0',m_AmpExpo0)
*****    CALL GPS_BPrint(6,' AmpBoxy',m_AmpBoxy)
*****    WRITE(*,'(a,5g20.10)') '#######>>> Boxy/Born = ',Xborn
*  //////////////////////////////////////////////////////////////////////////////////////
*  //    Crude distribution as in low level Monte Carlo, see also KK2f_DsigOverDtau    //
*  //    We DO NOT use BornCru memorized in KK2f in order to be able to recalculate    //
*  //    Weights with another input parameters like MZ etc.                            //
*  //    This trick is susspended, have to think if it could be restored?              //
*  //////////////////////////////////////////////////////////////////////////////////////
*****    BornCru = 4d0/3d0*BornV_Simple( KFi,KFf,svarX, 0d0  )
         BornCru = 4d0/3d0*BornV_Differential(0,KFf,svarX,0d0,0d0,0d0,0d0,0d0)
         BornCru = BornCru *(svar/svarX)                                  !!<-- Born(svar)*svar
         fLLux   = svarX/svarQ                                            !!<-- extra LL factor
         betaf = DSQRT( 1d0 - 4*Massf**2/svarQ )                          !!<-- 2-body phase spase
         DistCru = BornCru/(4d0*m_pi) *fLLux *2d0/betaf *CDABS(sProd)**2  !!<-- CRUDE
         CrudSum =       CrudSum  + DistCru
         XborSum =       XborSum  + XBorn
         XboxSum =       XboxSum  + XBoxy
***      WRITE(*,'(a,5g20.10)') ' vv, Xborn/DistCru ', 1-svarX/svar, Xborn/DistCru
*   /////////////////////////////////////////////////////////////////
*   //         1-photon IR-finite contribution     ms-one           //
*   /////////////////////////////////////////////////////////////////
         iSaveCPU = 1
         IF(nphot .EQ. 0) GOTO 300
*        =========================
         DO j1=1,nphot
            DO k=1,4
               ph1(k) = Phot(j1,k)
            ENDDO
            IF( ph1(4)/Ene .GT. m_Vcut(1) ) THEN ! accept 1 hard only
               Hel1  = m_Phel(j1)+1
               Sactu1  = m_isr(j1)*Sini(Hel1,j1) + (1-m_isr(j1))*Sfin(Hel1,j1)
               SvarX1  = (QQ(4)+ph1(4))**2-(QQ(3)+ph1(3))**2-(QQ(2)+ph1(2))**2-(QQ(1)+ph1(1))**2 !
               CKine   = (svarX1/svarQ)
               IF( m_isr(j1) .EQ. 1) THEN
                  CALL GPS_HiniPlus(KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph1,mph,Hel1,Sactu1,sProd) !
!                  IF (NPHOT.EQ.1)
                  CALL GPS_HiniPlusW(1,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph1,mph,Hel1,Sactu1,sProd) ! One calculates part due to W for neutrinos for 1 subtract for -1, fixed transfer approx for 0.
C[[[[[[[[[[
C                  IF (NPHOT.EQ.2.and.phot(1,4)/ene.gt.m_Vcut(2).and.phot(2,4)/ene.gt.m_Vcut(2)) then
C                   CALL GPS_HiniPlusW(1,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph1,mph,Hel1,Sactu1,sProd) ! check 
C                  ENDIF
C]]]]]]]]]]

               ELSE
                  CALL GPS_HfinPlus(KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph1,mph,Hel1,Sactu1,sProd,CKine) !
               ENDIF
            ENDIF
         ENDDO
*   /////////////////////////////////////////////////////////////////
*   //         2-photon IR-finite contribution   ms-two            //
*   /////////////////////////////////////////////////////////////////
*   O(alf2) correction to 2 photons in ISR or FSR, 
*   implicit Bose Einstein symmetrization inside HiiPlus, HffPlus
         DO j1=1,nphot
            DO j2=j1+1,nphot
               DO k=1,4
                  ph1(k) = Phot(j1,k)
                  ph2(k) = Phot(j2,k)
               ENDDO
               IF( (ph1(4)/Ene .GT. m_Vcut(2)) .AND. (ph2(4)/Ene .GT. m_Vcut(2)) ) THEN ! accept 2 hard only
                  Hel1  = m_Phel(j1)+1
                  Hel2  = m_Phel(j2)+1
                  Sactu2  = ( m_isr(j1)*Sini(Hel1,j1) + (1-m_isr(j1))*Sfin(Hel1,j1) ) !
     $                     *( m_isr(j2)*Sini(Hel2,j2) + (1-m_isr(j2))*Sfin(Hel2,j2) ) !
                  Cfact2 = sProd/Sactu2
                  IF(     (m_isr(j1) .EQ. 1) .AND. (m_isr(j2) .EQ. 1) ) THEN ! ini-ini
                     CALL GPS_HiiPlus(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2) !
                     CALL GPS_HiiPlus(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2p) !
                     SactuA  = ( m_isr(j1)*Sini(Hel1,j1) + (1-m_isr(j1))*Sfin(Hel1,j1) ) !
                     SactuB  = ( m_isr(j2)*Sini(Hel2,j2) + (1-m_isr(j2))*Sfin(Hel2,j2) ) !
                     CALL GPS_HiniPlusW(-1,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph1,mph,Hel1,SactuA,sProd) ! remove ?
                     CALL GPS_HiniPlusW(-1,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph2,mph,Hel2,SactuB,sProd) ! remove ?
                     CALL GPS_HiiPlusW(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2)  
                     CALL GPS_HiiPlusW(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2p) 
                  ELSEIF( (m_isr(j1) .EQ. 0) .AND. (m_isr(j2) .EQ. 0) ) THEN ! fin-fin
                     CALL GPS_HffPlus(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2) !
                     CALL GPS_HffPlus(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2p) !
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
*   O(alf2) correction to 1 photon ISR and 1 photon FSR, explicit Bose-Einstein symmetrisation
         DO j1=1,nphot
            DO j2=1,nphot
               IF( j1 .NE. j2) THEN
                  DO k=1,4
                     ph1(k) = Phot(j1,k)
                     ph2(k) = Phot(j2,k)
                  ENDDO
                  IF( (ph1(4)/Ene .GT. m_Vcut(2)) .AND. (ph2(4)/Ene .GT. m_Vcut(2)) ) THEN ! accept 2 hard only
                     Hel1  = m_Phel(j1)+1
                     Hel2  = m_Phel(j2)+1
                     Sactu2  = ( m_isr(j1)*Sini(Hel1,j1) + (1-m_isr(j1))*Sfin(Hel1,j1) ) !
     $                        *( m_isr(j2)*Sini(Hel2,j2) + (1-m_isr(j2))*Sfin(Hel2,j2) ) !
                     Cfact2 = sProd/Sactu2
                     IF(     (m_isr(j1) .EQ. 1) .AND. (m_isr(j2) .EQ. 0) ) THEN ! ini-fin
                        CALL GPS_HifPlus(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2) !
                        CALL GPS_HifPlus(Cfact2,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Hel1,ph1,Hel2,ph2,mph,m_AmpExpo2p) !
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
*-------------------
*****    WRITE(*,'(f20.10,a,20i2)') sqrt(svarX),' >>> m_isr(j)= ',(m_isr(j),j=1,nphot)
*/////////////////////////////////////////////////////////
*//   Update m_isr, check if it is the last partition   //
*/////////////////////////////////////////////////////////
         IF(last .EQ. 1) GOTO 300
         CALL GPS_PartitionPlus(nphot,last)
         IF(last .EQ. 2) GOTO 300
      ENDDO
      write(*,*) '########### INCORRECT EXIT from loop over partitions'
      STOP
 300  CONTINUE
*//////////////////////////////////////////////////////////////////////
*//  *************************************************************   //
*//              LOOP OVER PARTITIONS ENDS HERE                      //
*//  *************************************************************   //
*//////////////////////////////////////////////////////////////////////

      m_RhoCrud = CrudSum *CrudNorm   !<-- Crude (unpolarized for the time being)
      CALL GPS_MakeRho(ExpoNorm)      !<-- Defines m_RhoExp0, m_RhoExp1, m_RhoExp2

*/////////////////////////////////////////////////////////////////////////////////
*//                     Assigning weight list                                   //
*/////////////////////////////////////////////////////////////////////////////////
      IF( m_KeyINT .EQ. 0 ) THEN
         m_WtSet( 51) =   m_RhoExp0 /m_RhoCrud    !!! Interference OFF
         m_WtSet( 52) =   m_RhoExp1 /m_RhoCrud
         m_WtSet( 53) =   m_RhoExp2 /m_RhoCrud
         m_WtSet( 63) =   m_RhoExp2p/m_RhoCrud    !!! Pairs ON
         m_WtBest     =   m_WtSet( 53)
      ELSE
         m_WtSet( 1)  =   m_RhoExp0 /m_RhoCrud    !!! Interference ON
         m_WtSet( 2)  =   m_RhoExp1 /m_RhoCrud
         m_WtSet( 3)  =   m_RhoExp2 /m_RhoCrud
         m_WtSet(13)  =   m_RhoExp2p/m_RhoCrud    !!! Pairs ON
         m_WtBest     =   m_WtSet( 3)
      ENDIF
*/////////////////////////////////////////////////////////////////////////////////
*//                     X-Checks   Printouts                                    //
*/////////////////////////////////////////////////////////////////////////////////
* debug variables for tests
      m_debg( 3) = XBorSum
      m_debg( 4) = XBoxSum
*
      IF(Icont .GE. 100 )  RETURN
***      WRITE(*,'(3a)') '**************************************************'
***     $                            ,' !!!GPS_Make!!! ',
***     $                '**************************************************'
***      WRITE( *,'(a,i10,6g20.12)') 'GPS_Make::  nphot, vq, wt0,wt1,wt2',
***     $     nphot, 1-svarQ/svar, m_WtSet( 1),m_WtSet( 2),m_WtSet( 3)
***      CALL GPS_BPrint(6,'m_AmpEx0',m_AmpExpo0)
***      CALL GPS_BPrint(6,'m_AmpEx1',m_AmpExpo1)
***      CALL GPS_BPrint(6,'m_AmpEx2',m_AmpExpo2)
***   IF(Wt1 .LT. 10d0 )  RETURN
***   IF( (1-svarQ/svar) .LT. 0.1d0 ) RETURN
***   IF( nphot.NE.0 ) RETURN
***   Wt0 = Exp0Sum/XborSum/2**nphot
***   Wt1 = Exp1Sum/XborSum/2**nphot
***   CALL KK2f_Print1(6)
* X-check with DsigOverDtau makes sense ONLY for KeyINT=0
***   CALL  KK2f_DsigOverDtau(6,RhoCru3)
***   WRITE(*,'(a,5g20.12)') '***  nphot, vq, wt0,wt1', nphot, 1-svarQ/svar,wt0,wt1
***   WRITE(*,'(a,5g20.12)') '*** CDABS(sProd)**2 = ', CDABS(sProd)**2 *CrudNorm
***   WRITE(*,'(a,5g20.12)') '*** m_RhoCrud/RhoCru3 = ', m_RhoCrud/RhoCru3, m_RhoCrud

      Icont = Icont +1
      END                       !!!end of GPS_Make!!!

      SUBROUTINE GPS_MakeRho(ExpoNorm)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Calculate differential distributions (normalized to LIPS) from spin ampl.  //
*//                UNPOLARIZED final fermions                                    //
*//                                                                              //
*//   To be done:                                                                //
*//   One needs to put wigner rotation for initial polarizations somewhere       //
*//   either in setter called in KK2f, or in flight, for every event             //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
* params
      DOUBLE PRECISION      ExpoNorm
* locals
      INTEGER               j1,j2,j3,j4,i1,i2,i3,i4
      DOUBLE PRECISION      Sum0, Sum1, Sum2, Sum2p
      DOUBLE PRECISION      polar1,  polar2
      DOUBLE COMPLEX        SDMprod, Tensor0, Tensor1, Tensor2, Tensor2p
*----------------------------------
* Normalized factor to LIPS for CEEX amplitudes is memorized for further use
      m_ExpoNorm = ExpoNorm
*
      polar1 = SQRT(ABS(m_PolBeam1(3)**2 +m_PolBeam1(2)**2 +m_PolBeam1(1)**2)) !
      polar2 = SQRT(ABS(m_PolBeam2(3)**2 +m_PolBeam2(2)**2 +m_PolBeam2(1)**2)) !
      Sum0 = 0d0
      Sum1 = 0d0
      Sum2 = 0d0
      Sum2p = 0d0
      IF( (polar1+polar2) .LT. 1d-6) THEN
*        The case with UNPOLARIZED beams and UNPOLARIZED final fermions
         DO j1 = 1,2
            DO j2 = 1,2
               DO j3 = 1,2
                  DO j4 = 1,2
                     Sum0 = Sum0 +m_AmpExpo0(j1,j2,j3,j4)*DCONJG(m_AmpExpo0(j1,j2,j3,j4)) !
                     Sum1 = Sum1 +m_AmpExpo1(j1,j2,j3,j4)*DCONJG(m_AmpExpo1(j1,j2,j3,j4)) !
                     Sum2 = Sum2 +m_AmpExpo2(j1,j2,j3,j4)*DCONJG(m_AmpExpo2(j1,j2,j3,j4)) !
                     Sum2p= Sum2p +m_AmpExpo2p(j1,j2,j3,j4)*DCONJG(m_AmpExpo2p(j1,j2,j3,j4)) !
                  ENDDO                  
               ENDDO
            ENDDO
         ENDDO
      ELSE
*        The case with POLARIZED beams and UNPOLARIZED final fermions
         DO j1=1,2
         DO i1=1,2
            DO j2=1,2
            DO i2=1,2
               DO j3=1,2
                  DO j4=1,2
                     SDMprod = m_SDMat1(i1,j1)*m_SDMat2(i2,j2)
                     Tensor0 = m_AmpExpo0(i1,i2,j3,j4)*DCONJG(m_AmpExpo0(j1,j2,j3,j4)) !
                     Tensor1 = m_AmpExpo1(i1,i2,j3,j4)*DCONJG(m_AmpExpo1(j1,j2,j3,j4)) !
                     Tensor2 = m_AmpExpo2(i1,i2,j3,j4)*DCONJG(m_AmpExpo2(j1,j2,j3,j4)) !
                     Tensor2p = m_AmpExpo2p(i1,i2,j3,j4)*DCONJG(m_AmpExpo2p(j1,j2,j3,j4)) !
                     Sum0 = Sum0 + Tensor0 *SDMprod
                     Sum1 = Sum1 + Tensor1 *SDMprod
                     Sum2 = Sum2 + Tensor2 *SDMprod
                     Sum2p = Sum2p + Tensor2p *SDMprod
                  ENDDO
               ENDDO
            ENDDO
            ENDDO
         ENDDO
         ENDDO
      ENDIF
      m_RhoExp0 = Sum0 *ExpoNorm
      m_RhoExp1 = Sum1 *ExpoNorm
      m_RhoExp2 = Sum2 *ExpoNorm
      m_RhoExp2p = Sum2p *ExpoNorm
      END                       !!!GPS_MakeRho!!!

      SUBROUTINE GPS_MakeRho2(wt0,wt1,wt2)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Used in Taupair_ImprintSpin                                                //
*//                                                                              //
*//   Calculate differential distributions (normalized to LIPS) from spin ampl.  //
*//                  POLARIZED final fermions                                    //
*//                                                                              //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
* params
      DOUBLE PRECISION   wt0,wt1,wt2
* locals
      INTEGER      j1,j2,j3,j4,i1,i2,i3,i4
      DOUBLE PRECISION        Sum0, Sum1, Sum2
      DOUBLE PRECISION        polar1,  polar2
      DOUBLE COMPLEX          Tensor0, Tensor1, Tensor2, SDMprod
      DOUBLE PRECISION        Rho0, Rho1, Rho2
*----------------------------------
      polar1 = SQRT(ABS(m_PolBeam1(3)**2 +m_PolBeam1(2)**2 +m_PolBeam1(1)**2))
      polar2 = SQRT(ABS(m_PolBeam2(3)**2 +m_PolBeam2(2)**2 +m_PolBeam2(1)**2))
      Sum0 = 0d0
      Sum1 = 0d0
      Sum2 = 0d0
      IF( (polar1+polar2) .LT. 1d-6) THEN
*        The case with UNPOLARIZED beams and POLARIZED final fermions
         DO j1 = 1,2
            DO j2 = 1,2
               DO j3 = 1,2
               DO i3 = 1,2
                     DO j4 = 1,2
                     DO i4 = 1,2
                        SDMprod = m_SDMat3(j3,i3)*m_SDMat4(j4,i4)
                        Tensor0 = m_AmpExpo0(j1,j2,i3,i4)*DCONJG(m_AmpExpo0(j1,j2,j3,j4))
                        Tensor1 = m_AmpExpo1(j1,j2,i3,i4)*DCONJG(m_AmpExpo1(j1,j2,j3,j4))
                        Tensor2 = m_AmpExpo2(j1,j2,i3,i4)*DCONJG(m_AmpExpo2(j1,j2,j3,j4))
                        Sum0 = Sum0 + Tensor0 *SDMprod
                        Sum1 = Sum1 + Tensor1 *SDMprod
                        Sum2 = Sum2 + Tensor2 *SDMprod
                     ENDDO                  
                     ENDDO
               ENDDO
               ENDDO
            ENDDO
         ENDDO
      ELSE
*        The case with POLARIZED beams and POLARIZED final fermions
         DO j1=1,2
         DO i1=1,2
            DO j2=1,2
            DO i2=1,2
               DO j3=1,2
               DO i3=1,2
                  DO j4=1,2
                  DO i4=1,2
                     SDMprod = m_SDMat1(i1,j1)*m_SDMat2(i2,j2)
     $                        *m_SDMat3(j3,i3)*m_SDMat4(j4,i4)
                     Tensor0 = m_AmpExpo0(i1,i2,i3,i4)*DCONJG(m_AmpExpo0(j1,j2,j3,j4))
                     Tensor1 = m_AmpExpo1(i1,i2,i3,i4)*DCONJG(m_AmpExpo1(j1,j2,j3,j4))
                     Tensor2 = m_AmpExpo2(i1,i2,i3,i4)*DCONJG(m_AmpExpo2(j1,j2,j3,j4))
                     Sum0 = Sum0 + Tensor0 *SDMprod
                     Sum1 = Sum1 + Tensor1 *SDMprod
                     Sum2 = Sum2 + Tensor2 *SDMprod
                  ENDDO
                  ENDDO
               ENDDO
               ENDDO
            ENDDO
            ENDDO
         ENDDO
         ENDDO
      ENDIF
* distributions with polarized final fermions
      Rho0 = Sum0 *m_ExpoNorm
      Rho1 = Sum1 *m_ExpoNorm
      Rho2 = Sum2 *m_ExpoNorm
* Spin weight for  polarized final fermions
      wt0 = Rho0/m_RhoExp0
      wt1 = Rho1/m_RhoExp1
      wt2 = Rho2/m_RhoExp2
      END                       !!!GPS_MakeRho2!!!



      SUBROUTINE GPS_BornPlus(Mode,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,Cfac,Xborn,Xboxy)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Two-stroke version of GPS_Born, optimized for summation over partitions       //
*//   Virtual corrections (boxes vertices) to ms-one are also here!!!               //
*//   Warning! Input masses are TRUE (not signed as in GPS_Born)                    //
*//                                                                                 //
*//   Born spin amplitudes calculated with spinor methods.                          //
*//   Mass of the final fermion kept exactly.                                       //
*//                                                                                 //
*//   Input:                                                                        //
*//   KFi, Kff = beam and final fermion flavour codes (to define charges)           //
*//   PX       = s-chanel momentum for gamma and Z propagators (not for spinors)    //
*//   pi,mi    are for spinors, not for gamma and Z propagators                     //
*//   p1,m1    =fermion momentum and mass (beam)                                    //
*//   p2,m2    =fermion momentum and mass (beam)                                    //
*//   p3,m3    =fermion momentum and mass final state                               //
*//   p4,m4    =fermion momentum and mass final state                               //
*//                                                                                 //
*//   Output:                                                                       //
*//   Born     = spin summed squared amplitudes                                     //
*//                                                                                 //
*//   Common working space:                                                         //
*//   m_AmpBorn   is working space, used by HfinMinus                               //
*//   m_AmpExpo*  is working space, used by HiniPlus, HfinPlus, HfinMinus           //
*//                                                                                 //
*//   Notes:                                                                        //
*//   Electron mass neglected in spinors, this is why we may use Chisholm!          //
*//   Final fermion mass kept exactly.                                              //
*//   Gamma and Z in s-chanel.                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      SAVE                      !!! <-- necessary !!!
*
      INTEGER    Mode,KFi,KFf,Level
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4)
      DOUBLE PRECISION      m1,m2,m3,m4,Xborn,Xboxy
*
      DOUBLE PRECISION      MasPhot
      DOUBLE PRECISION      BornSum,BoxySum
      DOUBLE PRECISION      SvarP, SvarQ, SvarX, Svar, s,t,u
      DOUBLE PRECISION      Fleps
*-----------------------------------------------------------------------------
      INTEGER         i,j,k,l
      INTEGER         j1,j2,j3,j4
      INTEGER         Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  Cfac,AmpBorn,AmpBoxy
      DOUBLE COMPLEX  PropGam,PropZet
      DOUBLE COMPLEX  s31,s24,s14,s32
      DOUBLE COMPLEX  FFacTT(2),      FFacUU(2)
      DOUBLE COMPLEX  FFacTG(2),FFacTZ(2),      FFacUG(2),FFacUZ(2)
      DOUBLE COMPLEX  SpinoTT(2,2,2,2), SpinoUU(2,2,2,2)
      DOUBLE COMPLEX  BoxGG(2,2,2,2),   BoxGZ(2,2,2,2)
      DOUBLE COMPLEX  AmpBornW(2,2,2,2)
      DOUBLE COMPLEX  BVR_CBoxGG, BVR_CBoxGZ, BVR_IntIR, BVR_IntReson
      DOUBLE COMPLEX  Coef, IntIR
      DOUBLE COMPLEX  TT,UU
      DOUBLE COMPLEX  GPS_iProd1
      DOUBLE COMPLEX  GPS_iProd2
      DOUBLE PRECISION  F1iniPair,F1finPair
*-----------------------------------------------------------------------------
* Electroweak
      INTEGER           NCf,NCe
      DOUBLE PRECISION  T3e,Qe
      DOUBLE PRECISION  T3f,Qf
      DOUBLE COMPLEX    Ve,Ae,Vf,Af, VVCor, GamVPi, ZetVPi
      DOUBLE PRECISION  RsqV,RsqA ! QCD corrs.
      DOUBLE PRECISION  Svar9,CosThetD
*-----------------------------------------------------------------------------
      DOUBLE PRECISION        PP(4),QQ(4),dummy
c[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      INTEGER    icont
c      DATA       icont /0/
c]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*-----------------------------------------------------------------------------
      Fleps =  1d-100
* Pure spinor part, does not depend on PX, Mode introduced to save CPU time
      IF(Mode .EQ. 0) THEN
         DO k=1,4
            PP(k) = p1(k)+p2(k)
            QQ(k) = p3(k)+p4(k)
         ENDDO
         SvarP  = PP(4)**2 - PP(3)**2 - PP(2)**2 - PP(1)**2
         SvarQ  = QQ(4)**2 - QQ(3)**2 - QQ(2)**2 - QQ(1)**2
*=============================================================
* Get charges, izospin, color
         CALL BornV_GetParticle(KFi, dummy, Qe,T3e,NCe)
         CALL BornV_GetParticle(KFf, dummy, Qf,T3f,NCf)
         CALL KK2f_GetMasPhot(MasPhot)
*=============================================================
* Loop below correcponds to
****> CALL GPS_Born(KFi,KFf,PX, p1,Mbeam, p2,-Mbeam,  p3,Massf, p4,-Massf,m_AmpBorn) !!!!<****
         DO j1 = 1,2
            DO j2 = 1,2
               DO j3 = 1,2
                  DO j4 = 1,2
                     Hel1 = 3-2*j1
                     Hel2 = 3-2*j2
                     Hel3 = 3-2*j3
                     Hel4 = 3-2*j4
                     TT  = DCMPLX(0d0,0d0)
                     UU  = DCMPLX(0d0,0d0)
                     IF( Hel2 .EQ. -Hel1) THEN !!! <--helicity conservation imposed
                        s31 = GPS_iProd2(  Hel3, p3, m3,      Hel1, p1, Fleps) ! t
                        s24 = GPS_iProd2(  Hel2, p2,-Fleps,   Hel4, p4,-m4)    ! t1
                        s32 = GPS_iProd2(  Hel3, p3, m3,      Hel2, p2, Fleps) ! u1
                        s14 = GPS_iProd2(  Hel1, p1,-Fleps,   Hel4, p4,-m4)    ! u
                        TT  = s31*s24
                        UU  = s32*s14
                     ENDIF
                     SpinoTT(j1,j2,j3,j4) =  TT
                     SpinoUU(j1,j2,j3,j4) =  UU
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDIF                     !!! Mode.EQ.1
*////////////////////////////////////////////////////////////////////////////////////////////
*//                          Partition (PX) dependent part                                 //
*////////////////////////////////////////////////////////////////////////////////////////////
* Boxes: 
* IR-subtracted, helicity conservation imposed (small mass approx.)
      SvarX = PX(4)**2-PX(3)**2-PX(2)**2-PX(1)**2
      IF(SvarX .LE. (ABS(m3)+ABS(m4))**2 ) RETURN
      CALL KinLib_ThetaD(PX,p1,p2,p3,p4,Svar9,CosThetD)
      s =  SvarX
      t = -s*(1d0-CosThetD)/2d0
      u = -s*(1d0+CosThetD)/2d0
      Coef  = DCMPLX(m_Alfpi*Qe*Qf)
      IF(  m_KeyINT .NE. 0 .AND. m_KeyISR .NE. 0 .AND.  m_KeyFSR .NE. 0  ) THEN
* Virtual 2*(B(t)-B(u)) Intereference IR part to be subtracted from boxes
         IntIR      = Coef*BVR_IntIR(MasPhot,s,t,u)                 !!<- asymetric in (t,u)
         m_IntReson = Coef*BVR_IntReson(MasPhot,m_MZ,m_GammZ,s,t,u) !!<- asymetric in (t,u)
         m_BoxGGtu  = Coef*( BVR_CBoxGG(MasPhot,             s,t,u)) -IntIR
         m_BoxGZtu  = Coef*( BVR_CBoxGZ(MasPhot,m_MZ,m_GammZ,s,t,u)) -IntIR
         m_BoxGGut  = Coef*(-BVR_CBoxGG(MasPhot,             s,u,t)) -IntIR
         m_BoxGZut  = Coef*(-BVR_CBoxGZ(MasPhot,m_MZ,m_GammZ,s,u,t)) -IntIR
* Exponentiate Resonance BigLogs according to Greco et al.
         IF( m_KeyInt .EQ. 2) THEN
            m_BoxGZtu = m_BoxGZtu -m_IntReson
            m_BoxGZut = m_BoxGZut -m_IntReson
         ENDIF
      ELSE
         m_IntReson = DCMPLX(0d0,0d0)
         m_BoxGGtu  = DCMPLX(0d0,0d0)
         m_BoxGGut  = DCMPLX(0d0,0d0)
         m_BoxGZtu  = DCMPLX(0d0,0d0)
         m_BoxGZut  = DCMPLX(0d0,0d0)
      ENDIF
c[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
c[[[[  switching off boxes by hand
c      m_BoxGZtu = DCMPLX(0d0,0d0)
c      m_BoxGZut = DCMPLX(0d0,0d0)
c      m_BoxGGtu = DCMPLX(0d0,0d0)
c      m_BoxGGut = DCMPLX(0d0,0d0)
c]]]]
c]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  Hel1 = 3-2*j1
                  Hel2 = 3-2*j2
                  Hel3 = 3-2*j3
                  Hel4 = 3-2*j4
                  BoxGG(j1,j2,j3,j4) =DCMPLX(0d0,0d0)
                  BoxGZ(j1,j2,j3,j4) =DCMPLX(0d0,0d0)
                  IF((Hel2 .EQ. -Hel1) .AND. (Hel4 .EQ. -Hel3)) THEN !!<--helicity conserv.
                     IF( Hel1*Hel3 .EQ. 1) THEN
                        BoxGG(j1,j2,j3,j4) = m_BoxGGtu
                        BoxGZ(j1,j2,j3,j4) = m_BoxGZtu
                     ELSE
                        BoxGG(j1,j2,j3,j4) = m_BoxGGut
                        BoxGZ(j1,j2,j3,j4) = m_BoxGZut
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
      ENDDO
c[[[[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      icont=icont+1
c      IF(icont.LE.10) THEN
c         write(m_out,*) ' //////////////////////GPS///////////////////////////////////////'
cc         write(*,'(a,5g22.14)') 'CosThetD= ',CosThetD
cc         write(*,'(a,5g22.14)') 'Sw2= ',m_Sw2
c      ENDIF
c]]]]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* Propagators, with s-dependent width
      PropGam =    DCMPLX(  1d0/SvarX,  0d0)
      PropZet =    1d0/DCMPLX(SvarX-m_MZ**2, m_GammZ*SvarX/m_MZ)
* Possibility to switch off Z or gamma, etc.
      IF(m_KeyZet .LE. 0) PropZet =  DCMPLX(0d0)
      IF(m_KeyZet .EQ. 9) PropGam =  DCMPLX(0d0)
      IF(m_KeyZet .EQ.-1) PropZet =  1d0/DCMPLX(SvarX-m_MZ**2, m_GammZ*m_MZ)
* Exponentiate Resonance BigLogs according to Greco et al.
      IF(  m_KeyINT .EQ. 2 .AND. m_KeyISR .NE. 0 .AND.  m_KeyFSR .NE. 0  ) THEN
         PropZet = PropZet * EXP(m_IntReson)
      ENDIF
*////////////////////////////////////////////////////////////////////////////////////////////
*//                        ElectroWeak Corrections                                         //    
*////////////////////////////////////////////////////////////////////////////////////////////
      CALL GPS_EWFFact(KFi,KFf,SvarX,CosThetD,Ve,Vf,Ae,Af,VVcor,GamVPi,ZetVPi,RsqV,RsqA) !
*////////////////////////////////////////////////////////////////////////////////////////////
*//     Primitives formfactor-type for construction of spin amplitudes                     //
*//     For boxes we need separately photon and Z parts                                    //
*//     (Ve -Hel1*Ae)*(Vf +Hel1*Af) is expanded because of double-vector f-factor          //
*////////////////////////////////////////////////////////////////////////////////////////////
      DO j1 = 1,2
         Hel1 = 3-2*j1
         FFacTG(j1) = PropGam*GamVPi *Qe*Qf *RsqV
         FFacTZ(j1) = PropZet*ZetVPi *(Ve*Vf*VVCor*RsqV -Hel1*Ae*Vf*RsqV +Hel1*Ve*Af*RsqA -Ae*Af*RsqA) !
         FFacUG(j1) = PropGam*GamVPi *Qe*Qf *RsqV
         FFacUZ(j1) = PropZet*ZetVPi *(Ve*Vf*VVCor*RsqV -Hel1*Ae*Vf*RsqV -Hel1*Ve*Af*RsqA +Ae*Af*RsqA) !
         FFacTT(j1) = FFacTG(j1)+FFacTZ(j1)
         FFacUU(j1) = FFacUG(j1)+FFacUZ(j1)
c[[[[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         IF(icont.LE.10) THEN
c            write(m_out,'(a,i5,5g22.14)') 'j1,FFacUU,TT= ',j1,FFacUU(j1),FFacTT(j1)
c         ENDIF
c]]]]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ENDDO
*///////////////////////////////////////////////////////////////////////////////////
*//       QED vertex  FFactor F1 minus B-vrtual (IR removed), exact mass terms    //
*///////////////////////////////////////////////////////////////////////////////////
      m_F1ini1 = DCMPLX(0d0)
      m_F1fin1 = DCMPLX(0d0)
      m_F1ini2 = DCMPLX(0d0)
      m_F1fin2 = DCMPLX(0d0)
      m_F1iniPair2 = DCMPLX(0d0)
      m_F1finPair2 = DCMPLX(0d0)
      IF( m_KeyISR .NE. 0) THEN
         CALL  BVR_MakeF1ini(SvarP,m1,m2,m_Alfpi,Qe,m_F1ini1,m_F1ini2)
         CALL BVR_MakeF2Pair(SvarP,m1,m_Alfpi,Qe,F1iniPair)
         m_F1iniPair2 = F1iniPair ! real to Complex
      ENDIF
      IF( m_KeyFSR .NE. 0) THEN
         CALL  BVR_MakeF1fin(SvarQ,m3,m4,m_Alfpi,Qf,m_F1fin1,m_F1fin2)
         CALL BVR_MakeF2Pair(SvarQ,m3,m_Alfpi,Qf,F1finPair)
         m_F1finPair2 = F1finPair ! real to Complex
      ENDIF
c[[[[[[[
c      IF( DABS(SvarQ/SvarP-1d0) .LT. 1d-10 ) THEN
c      write(*,*) ' GPS:ABS(SvarQ/SvarP-1)', DABS(SvarQ/SvarP-1d0)
c      write(*,*) ' GPS: alf1 ',CDABS(1+ m_F1ini1)**2*CDABS(1+ m_F1fin1)**2
c      write(*,*) ' GPS: alf2 ',CDABS(1+ m_F1ini2)**2*CDABS(1+ m_F1fin2)**2
ccc      write(*,*) ' GPS: STOP'
ccc      STOP
c      ENDIF
c]]]]]]]

*///////////////////////////////////////////////////////////////////////////////////
*//                      Total result = Spinors*Formfactor                        //
*///////////////////////////////////////////////////////////////////////////////////
      Level=1  ! W amplitude calculated separately for  level=1, and masses are treated as in BornPlus
      call GPS_BornWPlus(Mode,Level,KFi,KFf,s,t,u,p1,m1,p2,m2,p3,m3,p4,m4,AmpBornW)
      BornSum = 0d0
      BoxySum  = 0d0

      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
* Born,  Zero order
                  AmpBorn = SpinoTT(j1,j2,j3,j4)* FFacTT(j1)
     $                     +SpinoUU(j1,j2,j3,j4)* FFacUU(j1)
     $                     +AmpBornW(j1,j2,j3,j4)
* Boxes, First order
                  AmpBoxy = SpinoTT(j1,j2,j3,j4)* FFacTG(j1) *BoxGG(j1,j2,j3,j4)
     $                     +SpinoTT(j1,j2,j3,j4)* FFacTZ(j1) *BoxGZ(j1,j2,j3,j4)
     $                     +SpinoUU(j1,j2,j3,j4)* FFacUG(j1) *BoxGG(j1,j2,j3,j4)
     $                     +SpinoUU(j1,j2,j3,j4)* FFacUZ(j1) *BoxGZ(j1,j2,j3,j4)
* Store results
                  m_AmpBorn( j1,j2,j3,j4) = AmpBorn
                  m_AmpBoxy( j1,j2,j3,j4) = AmpBoxy*Cfac
                  m_AmpExpo0(j1,j2,j3,j4) = m_AmpExpo0(j1,j2,j3,j4) 
     $                 +Cfac*AmpBorn                               !!! pure Born
                  m_AmpExpo1(j1,j2,j3,j4) = m_AmpExpo1(j1,j2,j3,j4) 
     $                 +Cfac*AmpBorn*(1 +m_F1ini1)*(1 +m_F1fin1 )  !!! Born, O(alf1) FFactors
     $                 +Cfac*AmpBoxy                               !!! O(alf1) boxes
*///////// new !!!!!!!! in construction !!!!!!! ////////////////////
                  m_AmpExpo2(j1,j2,j3,j4) = m_AmpExpo2(j1,j2,j3,j4) 
     $                 +Cfac*AmpBorn*(1 +m_F1ini2)*(1 +m_F1fin2 )  !!! Born, O(alf2) FFactors
     $                 +Cfac*AmpBoxy                               !!! O(alf1) boxes
                  m_AmpExpo2p(j1,j2,j3,j4) = m_AmpExpo2p(j1,j2,j3,j4) 
     $                 +Cfac*AmpBorn*(1 +m_F1ini2+m_F1iniPair2)*(1 +m_F1fin2+m_F1finPair2 ) ! +Pairs
     $                 +Cfac*AmpBoxy                               !!! O(alf1) boxes
*/////////////////////////////
                  BornSum = BornSum +Cfac*AmpBorn*DCONJG(Cfac*AmpBorn)
                  BoxySum = BoxySum +2*DREAL(Cfac*AmpBoxy*DCONJG(Cfac*AmpBorn))
               ENDDO                  
            ENDDO
         ENDDO
      ENDDO
      Xborn = BornSum
      Xboxy = BoxySum
* debug variables for tests
      m_debg( 1) = BornSum
      m_debg( 2) = BoxySum
      END                       !!!GPS_BornPlus!!!


      SUBROUTINE GPS_BornWPlus(Mode,Level,KFi,KFf,s,t,u,p1,m1,p2,m2,p3,m3,p4,m4,AmpBorn)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   correction  of W exchange to born                                             //
*//   Warning! Input masses are TRUE (not signed as in GPS_Born)                    //
*//                                                                                 //
*//   Born spin amplitudes calculated with spinor methods.                          //
*//   Mass of the final fermion kept exactly.                                       //
*//                                                                                 //
*//   Input:                                                                        //
*//   KFi, Kff = beam and final fermion flavour codes (to define charges)           //
*//   PX       = s-chanel momentum for W propagator (not for spinors)               //
*//   pi,mi    are for spinors, not for W propagator                                //
*//   p1,m1    =fermion momentum and mass (beam)                                    //
*//   p2,m2    =fermion momentum and mass (beam)                                    //
*//   p3,m3    =fermion momentum and mass final state                               //
*//   p4,m4    =fermion momentum and mass final state                               //
*//                                                                                 //
*//   Mode     time saving facility: not active                                     //
*//   Level    type of treatment of spinor products, probably irrelevant            //
*//   JakKoralZ=0 internal input for some old tests                                 //
*//                                                                                 //
*//   Output:                                                                       //
*//   AmpBorn     = spin amplitude                                                  //
*//                                                                                 //
*//                                                                                 //
*//   Notes:                                                                        //
*//   Electron mass neglected in spinors, this is why we may use Chisholm!          //
*//   Final fermion mass kept exactly.                                              //
*//   W in t-chanel.                                                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      SAVE                      !!! <-- necessary !!!
*
      INTEGER    Mode,KFi,KFf,Level,JakKoralZ
      DOUBLE PRECISION      p1(4),p2(4),p3(4),p4(4)
      DOUBLE PRECISION      m1,m2,m3,m4,Xborn
*
      DOUBLE PRECISION      BornSum
      DOUBLE PRECISION      s,t,u
      DOUBLE PRECISION      Fleps,m_MW, m_GammW
*-----------------------------------------------------------------------------
      INTEGER    i,j,k,l
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  Cfac,AmpBornW
      DOUBLE COMPLEX  PropW
      DOUBLE COMPLEX  s31,s24,s14,s32
      DOUBLE COMPLEX  FFacTT(2),      FFacUU(2)
      DOUBLE COMPLEX  FFacTG(2),FFacTZ(2),      FFacUG(2),FFacUZ(2)
      DOUBLE COMPLEX  SpinoTT(2,2,2,2), SpinoUU(2,2,2,2)
      DOUBLE COMPLEX  AmpBorn(2,2,2,2)
      DOUBLE COMPLEX  Coef, IntIR
      DOUBLE COMPLEX  TT,UU
      DOUBLE COMPLEX  GPS_iProd1
      DOUBLE COMPLEX  GPS_iProd2
*-----------------------------------------------------------------------------
* Electroweak
      INTEGER      NCf,NCe
      DOUBLE PRECISION        T3e,Qe
      DOUBLE PRECISION        T3f,Qf
      DOUBLE COMPLEX    Ve,Ae,Vf,Af, VVCor, WVPi
*-----------------------------------------------------------------------------
      DOUBLE PRECISION        dummy
c[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      INTEGER    icont
c      DATA       icont /0/
c]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*-----------------------------------------------------------------------------
      IF (level.EQ.1) THEN
        DO j1 = 1,2
           DO j2 = 1,2
              DO j3 = 1,2
                 DO j4 = 1,2
                    AmpBorn( j1,j2,j3,j4) = 0D0
                 ENDDO                  
              ENDDO
           ENDDO
        ENDDO
        ENDIF
      IF (ABS(KFf) .NE. 12)  RETURN

      JakKoralZ=0  ! warning: defined in 2 places ! to fix a potential problem .eq.1 like KORALZ .eq.0 possiby OK
      Fleps =  1d-100

* Pure spinor part, does not depend on PX, Mode introduced to save CPU time
! zbw      IF(Mode .EQ. 0) THEN
*=============================================================
* Get charges, izospin, color
         CALL BornV_GetParticle(KFi, dummy, Qe,T3e,NCe)
         CALL BornV_GetParticle(KFf, dummy, Qf,T3f,NCf)
*=============================================================
* Loop below correcponds to
****> CALL GPS_Born(KFi,KFf,PX, p1,Mbeam, p2,-Mbeam,  p3,Massf, p4,-Massf,m_AmpBorn) !!!!<****
         DO j1 = 1,2
            DO j2 = 1,2
               DO j3 = 1,2
                  DO j4 = 1,2
                     Hel1 = 3-2*j1
                     Hel2 = 3-2*j2
                     Hel3 = 3-2*j3
                     Hel4 = 3-2*j4
                     TT  = DCMPLX(0d0,0d0)
                     UU  = DCMPLX(0d0,0d0)
                     IF( Hel2 .EQ. -Hel1) THEN !!! <--helicity conservation imposed
                       IF(level.eq.1) THEN
                        s31 = GPS_iProd2(  Hel3, p3, m3,      Hel1, p1, Fleps) ! t
                        s24 = GPS_iProd2(  Hel2, p2,-Fleps,   Hel4, p4,-m4)    ! t1
                        s32 = GPS_iProd2(  Hel3, p3, m3,      Hel2, p2, Fleps) ! u1
                        s14 = GPS_iProd2(  Hel1, p1,-Fleps,   Hel4, p4,-m4)    ! u
                       ELSE
                        s31 = GPS_iProd2(  Hel3, p3, m3,      Hel1, p1, m1)    ! t
                        s24 = GPS_iProd2(  Hel2, p2, m2,      Hel4, p4, m4)    ! t1
                        s32 = GPS_iProd2(  Hel3, p3, m3,      Hel2, p2,-m2)    ! u1
                        s14 = GPS_iProd2(  Hel1, p1,-m1,      Hel4, p4, m4)    ! u
                       ENDIF
                        TT  = s31*s24
                        UU  = s32*s14
                     ENDIF
                     SpinoTT(j1,j2,j3,j4) =  TT
                     SpinoUU(j1,j2,j3,j4) =  UU
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
! zbw      ENDIF                     !!! Mode.EQ.1
*////////////////////////////////////////////////////////////////////////////////////////////
*//                          Partition (PX) dependent part                                 //
*////////////////////////////////////////////////////////////////////////////////////////////

      IF(s .LE. (ABS(m3)+ABS(m4))**2 ) RETURN
c]]]]
*////////////////////////////////////////////////////////////////////////////////////////////
*//                        ElectroWeak Corrections in/and W propagator                     //    
*////////////////////////////////////////////////////////////////////////////////////////////
      Coef  =1.d0/2.d0/m_Sw2

      IF(JakKoralZ.eq.1) then
         CALL GPS_EWFFactW(KFi,KFf,s,u,PropW,WVPi) !W Propagator: it looks crazy in this case ....
      ELSE
        CALL GPS_EWFFactW(KFi,KFf,s,t,PropW,WVPi)
      ENDIF


*////////////////////////////////////////////////////////////////////////////////////////////
*//     Primitives formfactor-type for construction of spin amplitudes                     //
*////////////////////////////////////////////////////////////////////////////////////////////
      IF(JakKoralZ.eq.1) then ! switch for some tests
        Ve=-0.5  ! 
        Vf= 0.5  ! 
        Ae= 0.5 ! 
        Af= 0.5
      ELSE
        Ve= 0.5  ! 
        Vf= 0.5  ! 
        Ae= 0.5 ! 
        Af= 0.5
      ENDIF
      DO j1 = 1,2
         Hel1 = 3-2*j1
         FFacTZ(j1) = PropW*WVPi *(Ve*Vf -Hel1*Ae*Vf +Hel1*Ve*Af -Ae*Af)
         FFacUZ(j1) = PropW*WVPi *(Ve*Vf -Hel1*Ae*Vf -Hel1*Ve*Af +Ae*Af)
         FFacTT(j1) = FFacTZ(j1) * Coef
         FFacUU(j1) = FFacUZ(j1) * Coef
      ENDDO
*///////////////////////////////////////////////////////////////////////////////////
*//                      Total result = Spinors*Formfactor                        //
*///////////////////////////////////////////////////////////////////////////////////
      BornSum = 0d0
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
* Born,  Zero order
                  AmpBornW = SpinoTT(j1,j2,j3,j4)* FFacTT(j1)
     $                     +SpinoUU(j1,j2,j3,j4)* FFacUU(j1)
                  AmpBorn( j1,j2,j3,j4) = AmpBornW +AmpBorn( j1,j2,j3,j4)
*/////////////////////////////
                  BornSum = BornSum +Cfac*AmpBornW*DCONJG(Cfac*AmpBornW)
               ENDDO                  
            ENDDO
         ENDDO
      ENDDO
!      write(*,*) ampbornw
!      stop
      Xborn = BornSum
* debug variables for tests
!      m_debg( 1) = BornSum
      END                       !!!GPS_BornWPlus!!!

      SUBROUTINE GPS_EWFFactW(KFi,KFf,s,t,PropW,WVPi)
*////////////////////////////////////////////////////////////////////////////////////////////
*//                        ElectroWeak Corrections                                         //    
*//                        empty prepared for nunu chanel                                  //
*////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
* Input
      INTEGER    KFi,KFf
      DOUBLE PRECISION      s,t,u
* Output
      DOUBLE COMPLEX    Ve,Vf,Ae,Af,VVcor,GamVPi,WVPi
* Local
      INTEGER            NCf,NCe,Jak
      DOUBLE PRECISION   T3e,Qe
      DOUBLE PRECISION   T3f,Qf,m_MW,m_GammW,m_MW_true
      DOUBLE COMPLEX     PropW,Coef,Coef1,DelW
      DOUBLE COMPLEX     RhoEW, VPgamma, CorEle, CorFin, CorEleFin, VVCef, CosThetD
      DOUBLE PRECISION   Deno,  dummy,row,Dum1,Dum2,Dum3,Dum4,Dum5,Dum6
      DOUBLE COMPLEX     GSW(100)
      INTEGER icont
      DATA icont /0/
      icont = icont +1
*===============================================================================
C we do it like for photon. I was unable to properly understand normalization
C this is half-guess half observation of the KK-KORALZ differences in code.
C it can be easily wrong. nothing like this tem is present in KORALZ, 
C nonetheless KORALZ results are 137/127 higher than KK for pure W exchange.
C in koralz this vacpol factor is installed at born step mode 1/0 ratio
C here not
      Jak= 1
      IF (Jak.eq.0) then ! old version for some tests only
        m_MW  =m_MZ*dsqrt(1d0-m_Sw2)
        m_GammW = 2.5d0
        PropW   =    1d0/DCMPLX(t-m_MW**2, m_GammW*m_MW)
        WVPi=DCMPLX(1.06794821,-0.0185004916)
        WVPi=DCMPLX(1.0,0.0)
      ELSE
        m_MW  =m_MZ*dsqrt(1d0-m_Sw2)
        IF ( m_KeyElw.GE.1) THEN
*         next will be mass fro common /cdzwsm/ from  DZface.f it is actually raw mass as above ...
          CALL BornV_GetMW(m_MW)
*         use of DZface forbiden, disk-mode does not work
*         CALL DZface_GetPrm( Dum1,Dum2,Dum3,m_MW,Dum4,Dum5,Dum6)
        ENDIF
        PropW   =    1d0/DCMPLX(t-m_MW**2,0D0 )
        WVPi=DCMPLX(1.06794821,-0.0185004916)
        WVPi=DCMPLX(1.0,0.0)
        Coef  =1.d0/2.d0/m_Sw2
c[[[[        Coef1=m_Gmu*m_MW**2* m_Alfinv/4D0 ! ERROR
        Coef1=m_Gmu*m_MW**2* m_Alfinv/(DSQRT(2d0)*m_pi)
        WVPi= WVPi*coef1/coef  ! effective coupling
        IF ( m_KeyElw.GE.1) THEN
           CosThetD=1+2*t/s 
           CALL BornV_InterpoGSW(KFf,S,CosThetD)
           CALL BornV_GetGSW(GSW)
*     use of rhocc forbiden, disk-mode does not work
c           CALL rhocc(s,-t,-s-t,-1D0,1D0,0D0,0D0,ROW)
c           IF(icont.LE.100) WRITE(*,*) '%%%%%%   ',GSW(5),ROW,'   ',s,t
c           IF (ABS(row-gsw(5)).GT.0.0002.AND.s.GT.10000) WRITE(*,*) GSW(5),ROW,'   ',s,t 
c           IF (ABS(row-1).gt.0.016) STOP

* this was added to GSW(5) in BornV_Dizet
           DelW= 1D0/m_AlfInv/m_pi/2*(-3D0/2*LOG(s/m_MW**2)+1D0/2*(LOG(-t/s))**2-4d0/6d0*m_pi**2+2D0) ! (b)
cccc       DelW= 1D0/m_AlfInv/m_pi/2*(-3D0/2*LOG(s/m_MW**2)+1D0/2*(LOG(-t/s))**2-1d0/6d0*m_pi**2+2D0) ! (a)
           WVPi= WVPi*(GSW(5)+DeLW)
        ENDIF
      ENDIF
      END


      SUBROUTINE GPS_Born(KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,AmpBorn)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Used in construction of the hard Non-IR parts: in GPS_HiniPlus, GPS_HfinPlus  //
*//   It is esentialy a simplified clone of GPS_BornPlus                            //
*//                                                                                 //
*//   CAREFUL!!! p_i are sometimes be substituted for photons!!!                    //
*//                                                                                 //
*//   Mass of the final fermion kept exactly.                                       //
*//                                                                                 //
*//   Input:                                                                        //
*//   KFi, Kff = beam and final fermion flavour codes (to define charges)           //
*//   PX       = s-chanel momentum for gamma and Z propagators (not for spinors)    //
*//   pi,mi    are for spinors, not for gamma and Z propagators                     //
*//   p1,m1    =fermion momentum and mass (beam)                                    //
*//   p2,m2    =fermion momentum and mass (beam)                                    //
*//   p3,m3    =fermion momentum and mass final state                               //
*//   p4,m4    =fermion momentum and mass final state                               //
*//                                                                                 //
*//   Output:                                                                       //
*//   AmpBorn   = spin amplitudes                                                   //
*//                                                                                 //
*//   Notes:                                                                        //
*//   Electron mass neglected in spinors, this is why we may use Chisholm!          //
*//   Final fermion mass kept exactly.                                              //
*//   Gamma and Z in s-chanel.                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    KFi,KFf,Mode,Level
      DOUBLE PRECISION  PX(4),p1(4),p2(4),p3(4),p4(4)
      DOUBLE PRECISION  m1,m2,m3,m4
      DOUBLE COMPLEX        AmpBorn(2,2,2,2),AmpBornW(2,2,2,2)
      DOUBLE COMPLEX        Cfac
      DOUBLE PRECISION      Xborn
*
      DOUBLE PRECISION  T3e,Qe
      DOUBLE PRECISION  T3f,Qf
      DOUBLE COMPLEX    Ve,Vf,Ae,Af,VVcor,GamVPi,ZetVPi
      DOUBLE PRECISION  RsqV,RsqA ! QCD corrs.
      INTEGER           NCf,NCe
      DOUBLE PRECISION  svarX
*-----------------------------------------------------------------------------
      INTEGER           i,j,k,l
      INTEGER           j1,j2,j3,j4
      INTEGER           Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX    PropGam,PropZet
      DOUBLE COMPLEX    s31,s24,s14,s32
      DOUBLE COMPLEX    FFacTT(2),      FFacUU(2)
      DOUBLE COMPLEX    SpinoTT(2,2,2,2),SpinoUU(2,2,2,2)
      DOUBLE COMPLEX    TT,UU
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE COMPLEX    GPS_iProd2
      DOUBLE PRECISION  dummy
*-----------------------------------------------------------------------------
      CALL GPS_Initialize
*=============================================================
* Get charges, izospin, color
      CALL BornV_GetParticle(KFi, dummy, Qe,T3e,NCe)
      CALL BornV_GetParticle(KFf, dummy, Qf,T3f,NCf)
*=============================================================
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  Hel1 = 3-2*j1
                  Hel2 = 3-2*j2
                  Hel3 = 3-2*j3
                  Hel4 = 3-2*j4
                  TT  = DCMPLX(0d0,0d0)
                  UU  = DCMPLX(0d0,0d0)
                  IF( Hel2 .EQ. -Hel1) THEN   !!! <--helicity conservation imposed
                     s31 = GPS_iProd2(  Hel3, p3, m3,   Hel1, p1, m1) ! t
                     s24 = GPS_iProd2(  Hel2, p2, m2,   Hel4, p4, m4) ! t1
                     s32 = GPS_iProd2(  Hel3, p3, m3,   Hel2, p2,-m2) ! u1
                     s14 = GPS_iProd2(  Hel1, p1,-m1,   Hel4, p4, m4) ! u
                     TT  = s31*s24
                     UU  = s14*s32
                  ENDIF
                  SpinoTT(j1,j2,j3,j4) =  TT
                  SpinoUU(j1,j2,j3,j4) =  UU
               ENDDO
            ENDDO
         ENDDO
      ENDDO
*////////////////////////////////////////////////////////////////////////////////////////////
*//                        ElectroWeak Corrections                                         //
*//   CosThetD = 0d0 is not mistake, we neglect miniscule second order effects only        //
*////////////////////////////////////////////////////////////////////////////////////////////
      svarX=PX(4)**2-PX(3)**2-PX(2)**2-PX(1)**2
      IF(svarX .LE. (ABS(m3)+ABS(m4))**2 ) RETURN
      CALL GPS_EWFFact(KFi,KFf,SvarX,0d0 ,Ve,Vf,Ae,Af,VVcor,GamVPi,ZetVPi,RsqV,RsqA) !
* Propagators, with s-dependent width
      PropGam =    DCMPLX(  1d0/svarX,  0d0)
      PropZet =    1d0/DCMPLX(svarX-m_MZ**2, m_GammZ*svarX/m_MZ)
* Possibility to switch off Z or gamma, etc.
      IF(m_KeyZet .LE. 0) PropZet =  DCMPLX(0d0)
      IF(m_KeyZet .EQ. 9) PropGam =  DCMPLX(0d0)
      IF(m_KeyZet .EQ.-1) PropZet =  1d0/DCMPLX(SvarX-m_MZ**2, m_GammZ*m_MZ)
* Exponentiate Resonance BigLogs according to Greco et al.
      IF(  m_KeyINT .EQ. 2 .AND. m_KeyISR .NE. 0 .AND.  m_KeyFSR .NE. 0  ) THEN
         PropZet = PropZet * EXP(m_IntReson)
      ENDIF
*////////////////////////////////////////////////////////////////////////////////////////////
*//     Primitives formfactor-type for construction of spin amplitudes                     //
*//     (Ve -Hel1*Ae)*(Vf +Hel1*Af) is expanded because of double-vector f-factor          //
*////////////////////////////////////////////////////////////////////////////////////////////
      DO j1 = 1,2
         Hel1 = 3-2*j1
         FFacTT(j1) = PropGam*GamVPi *Qe*Qf *RsqV
     $               +PropZet*ZetVPi *(Ve*Vf*VVCor*RsqV -Hel1*Ae*Vf*RsqV +Hel1*Ve*Af*RsqA -Ae*Af*RsqA) !
         FFacUU(j1) = PropGam*GamVPi *Qe*Qf *RsqV
     $               +PropZet*ZetVPi *(Ve*Vf*VVCor*RsqV -Hel1*Ae*Vf*RsqV -Hel1*Ve*Af*RsqA +Ae*Af*RsqA) !
      ENDDO
*////////////////////////////////////////////////////////////////////////////////////////////
*//                     Total result = Spinors*Formfactor                                  //
*////////////////////////////////////////////////////////////////////////////////////////////
!>>>>      Mode=0
!>>>>      Level=1  ! t-dependent W-propagator for level=1
!>>>>      call GPS_BornWPlus(Mode,Level,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,AmpBornW)
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  AmpBorn(j1,j2,j3,j4) =  SpinoTT(j1,j2,j3,j4)* FFacTT(j1)
     $                                   +SpinoUU(j1,j2,j3,j4)* FFacUU(j1)
!>>>>     $                                   +AmpBornW(j1,j2,j3,j4)

               ENDDO                  
            ENDDO
         ENDDO
      ENDDO
      END                       !!!GPS_Born!!!


      SUBROUTINE GPS_EWFFact(KFi,KFf,Svar,CosThetD,Ve,Vf,Ae,Af,VVcor,GamVPi,ZetVPi,RsqV,RsqA) !
*////////////////////////////////////////////////////////////////////////////////////////////
*//                        ElectroWeak Corrections                                         //    
*//  They are in Vector Couplings (multiplied by correcting f-factors)                     //
*//  Because of cost(theta) depenedence of WW boxes we need to define CosThetD variable    //
*////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
* Input
      INTEGER    KFi,KFf
      DOUBLE PRECISION      Svar,CosThetD
* Output
      DOUBLE COMPLEX    Ve,Vf,Ae,Af,VVcor,GamVPi,ZetVPi
      DOUBLE PRECISION  RsqV,RsqA ! QCD corrs.
* Local
      INTEGER           NCf,NCe
      DOUBLE PRECISION  T3e,Qe
      DOUBLE PRECISION  T3f,Qf
      DOUBLE COMPLEX    GSW(100)
      DOUBLE COMPLEX    RhoEW, VPgamma, CorEle, CorFin, CorEleFin, VVCef 
      DOUBLE PRECISION  Deno,  dummy
*===============================================================================
* Get charges, izospin, color
      CALL BornV_GetParticle(KFi, dummy, Qe,T3e,NCe)
      CALL BornV_GetParticle(KFf, dummy, Qf,T3f,NCf)
*
******IF( m_KeyElw .EQ. 0 .OR.  CosThetD .EQ. 0d0 ) THEN   !!! TEST TEST TEST
      IF( m_KeyElw .EQ. 0 ) THEN
* Vacuum polarization factors
         GamVPi = DCMPLX(1d0)
         ZetVPi = DCMPLX(1d0)
         VVCor  = DCMPLX(1d0)
* Couplings costants
         Deno   = DSQRT(16d0*m_Sw2*(1d0-m_Sw2))
         Ve     = (2*T3e -4*Qe*m_Sw2)/Deno
         Vf     = (2*T3f -4*Qf*m_Sw2)/Deno
         Ae     =  2*T3e             /Deno
         Af     =  2*T3f             /Deno
c[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c         Ve     = 2*T3e             /Deno
c         Vf     = 2*T3e             /Deno
c         Ae     =  0d0
c         Af     =  0d0
c]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ELSE
* Get EW form-factors
         CALL BornV_InterpoGSW(KFf,Svar,CosThetD)
         CALL BornV_GetGSW(GSW)
         CALL BornV_GetQCDcor2(KFf,RSQV,RSQA)
         RhoEW     = GSW(1)
         VPgamma   = GSW(6)
         CorEle    = GSW(2)
         CorFin    = GSW(3)
         CorEleFin = GSW(4)
* Vacuum polarization factors
         GamVPi = 1d0   /(2d0-VPgamma)
         ZetVPi = m_Gmu *m_MZ**2 *m_AlfInv /(DSQRT(2.d0)*8.d0*m_pi)
     $            *(m_Sw2*(1d0-m_Sw2)) *16d0
     $            *RhoEW
* Coupling costants times EW form-factors
         Deno   = DSQRT(16d0*m_Sw2*(1d0-m_Sw2))
         Ve     = (2*T3e -4*Qe*m_Sw2*CorEle)/Deno
         Vf     = (2*T3f -4*Qf*m_Sw2*CorFin)/Deno
         Ae     =  2*T3e             /Deno
         Af     =  2*T3f             /Deno
* Angle dependent double-vector extra-correction
         VVCef  = ( (2*T3e)      *(2*T3f) 
     $             -(4*Qe*m_Sw2) *(2*T3f)      *CorEle 
     $             -(4*Qf*m_Sw2) *(2*T3e)      *CorFin
     $             +(4*Qe*m_Sw2) *(4*Qf*m_Sw2) *CorEleFin )/Deno**2
         VVCor  = VVCef/(Ve*Vf)
* CosThetD = 1d0 is special
         IF( CosThetD .EQ. 0d0) VVCor  = DCMPLX(1d0)
      ENDIF
      END
      SUBROUTINE  GPS_KinExt(p1,m1,p2,m2,p3,m3,p4,m4,ph,mph,phv,p1v,p2v,p3v,p4v)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   kinematical extrapolation of complete to momenum conservation                 //
*//   pv3,pv4 are to replace p3 and p4 fulfilling that                              //
*//  used in reduction procedure for electron neutrino channel                      //
*//   p1,m1,p2,m2,p3,m3,p4,m4,ph,mph INPUT                                          //
*//   pv3,pv4                        OUTPUT                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph(4),PFAT(4),PSUM(4),PTEST(4),p1v(4),p2v(4),p3v(4),p4v(4),phv(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph,F0,F1
      INTEGER k
       DO K=1,4
        PFAT(K)=p3(k)+p4(k)+ph(k)
        PTEST(K)=p1(k)+p2(k)-ph(k)
        psum(k)=p3(k)+p4(k)+ph(k)
        p1v(k)=p1(k)
        p2v(k)=p2(k)
        p3v(k)=p3(k)
        p4v(k)=p4(k)
        phv(k)=ph(k)
       ENDDO
CCCC       return
       CALL KinLib_BostQ(1,Psum,ph,phv)
       CALL KinLib_BostQ(1,Psum,p3,p3v)
       CALL KinLib_BostQ(1,Psum,p4,p4v)
       CALL KinLib_BostQ(1,PFAT,ptest,ptest)
       CALL KinLib_BostQ(1,PFAT,psum,psum)
       F0=psum(4)/2
       F1=sqrt((F0**2-m3**2))
       p1v(4)=F0
       p2v(4)=F0
       p1v(3)=F1*p1(3)/abs(p1(3))
       p2v(3)=-p1v(3)
       DO K=1,2
         p1v(k)=0
         p2v(k)=0
       ENDDO


       CALL KinLib_BostQ(-1,PFAT,p1v,p1v)
       CALL KinLib_BostQ(-1,PFAT,p2v,p2v)
       CALL KinLib_BostQ(-1,PFAT,p3v,p3v)
       CALL KinLib_BostQ(-1,PFAT,p4v,p4v)
       CALL KinLib_BostQ(-1,PFAT,phv,phv)

       RETURN
C --   tests are below     
       write(*,*) '3 body kinematics missing momentum visible in last line'
       write(*,*) '--------------'
       write(*,*) p3
       write(*,*) p4
       do k=1,4
         write(*,*) p1(k)+p2(k),ph(k)+p3(k)+p4(k)
       enddo
       write(*,*) ' '
       write(*,*) ' after reparation (dirt moved under beams) '
       write(*,*) phv
       write(*,*) p1v
       write(*,*) p2v
       write(*,*) p3v
       write(*,*) p4v
       do k=1,4
         write(*,*) p1v(k)+p2v(k),phv(k)+p3v(k)+p4v(k)
       enddo
       stop
      END
      SUBROUTINE  GPS_KinExt2(p1,m1,p2,m2,p3,m3,p4,m4,ph1,ph2,mph,ph1v,ph2v,p1v,p2v,p3v,p4v)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   kinematical extrapolation of complete to momenum conservation                 //
*//   pv3,pv4 are to replace p3 and p4 fulfilling that                              //
*//  used in reduction procedure for electron neutrino channel                      //
*//   p1,m1,p2,m2,p3,m3,p4,m4,ph,mph INPUT                                          //
*//   pv3,pv4                        OUTPUT                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph1(4),PFAT(4),PSUM(4),PTEST(4)
      DOUBLE PRECISION      p1v(4),p2v(4),p3v(4),p4v(4),ph1v(4),ph2v(4),ph2(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph,F0,F1
      INTEGER k
       DO K=1,4
        PFAT(K)=p3(k)+p4(k)+ph1(k)+ph2(k)
        PTEST(K)=p1(k)+p2(k)-ph1(k)-ph2(k)
        psum(k)=p3(k)+p4(k)+ph1(k)+ph2(k)
        p1v(k)=p1(k)
        p2v(k)=p2(k)
        ph1v(k)=ph1(k)
        ph2v(k)=ph2(k)
       ENDDO
       CALL KinLib_BostQ(1,Psum,ph1,ph1v)
       CALL KinLib_BostQ(1,Psum,ph2,ph2v)
       CALL KinLib_BostQ(1,Psum,p3,p3v)
       CALL KinLib_BostQ(1,Psum,p4,p4v)
       CALL KinLib_BostQ(1,PFAT,ptest,ptest)
       CALL KinLib_BostQ(1,PFAT,psum,psum)
       F0=psum(4)/2
       F1=sqrt((F0**2-m3**2))
       p1v(4)=F0
       p2v(4)=F0
       p1v(3)=F1*p1(3)/abs(p1(3))
       p2v(3)=-p1v(3)
       DO K=1,2
         p1v(k)=0
         p2v(k)=0
       ENDDO


       CALL KinLib_BostQ(-1,PFAT,p1v,p1v)
       CALL KinLib_BostQ(-1,PFAT,p2v,p2v)
       CALL KinLib_BostQ(-1,PFAT,p3v,p3v)
       CALL KinLib_BostQ(-1,PFAT,p4v,p4v)
       CALL KinLib_BostQ(-1,PFAT,ph1v,ph1v)
       CALL KinLib_BostQ(-1,PFAT,ph2v,ph2v)

       RETURN
C --   tests are below     
       write(*,*) '3 body kinematics missing momentum visible in last line'
       write(*,*) '--------------'
       write(*,*) p3
       write(*,*) p4
       do k=1,4
         write(*,*) p1(k)+p2(k),ph1(k)+ph2(k)+p3(k)+p4(k)
       enddo
       write(*,*) ' '
       write(*,*) ' after reparation (dirt moved under beams) '
       write(*,*) ph1v
       write(*,*) ph2v
       write(*,*) p1v
       write(*,*) p2v
       write(*,*) p3v
       write(*,*) p4v
       do k=1,4
         write(*,*) p1v(k)+p2v(k),ph1v(k)+ph2v(k)+p3v(k)+p4v(k)
       enddo
       stop
      END
      SUBROUTINE  GPS_KinExtB(p1,m1,p2,m2,p3,m3,p4,m4,ph,mph,phv,p1v,p2v,p3v,p4v)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   kinematical extrapolation of complete to momenum conservation                 //
*//   pv3,pv4 are to replace p3 and p4 fulfilling that                              //
*//  used in reduction procedure for electron neutrino channel                      //
*//   p1,m1,p2,m2,p3,m3,p4,m4,ph,mph INPUT                                          //
*//   pv3,pv4                        OUTPUT                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph(4),PFAT(4),PSUM(4),PTEST(4),p1v(4),p2v(4),p3v(4),p4v(4),phv(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph,F0,F1
      INTEGER k
       DO K=1,4
        PFAT(K)=p1(k)+p2(k)-ph(k)
        PTEST(K)=p1(k)+p2(k)-ph(k)
        psum(k)=p3(k)+p4(k) 
        p1v(k)=p1(k)
        p2v(k)=p2(k)
        phv(k)=ph(k)
       ENDDO
       CALL KinLib_BostQ(1,Psum,p3,p3v)
       CALL KinLib_BostQ(1,Psum,p4,p4v)
       CALL KinLib_BostQ(1,PFAT,ptest,ptest)
       CALL KinLib_BostQ(1,PFAT,psum,psum)
       F0=ptest(4)/(p3v(4)+p4v(4))
       F1=sqrt((ptest(4)**2-4*m3**2)/((p3v(4)+p4v(4))**2-4*m3**2))
       p3v(4)=p3v(4)*F0
       p4v(4)=p4v(4)*F0
       DO K=1,3
         p3v(k)=p3v(k)*F1
         p4v(k)=p4v(k)*F1
       ENDDO


       CALL KinLib_BostQ(-1,PFAT,p3v,p3v)
       CALL KinLib_BostQ(-1,PFAT,p4v,p4v)
       RETURN
C --   tests are below     
       write(*,*) '3 body kinematics missing momentum visible in last line'
       write(*,*) '--------------'
       write(*,*) p3
       write(*,*) p4
       do k=1,4
         write(*,*) p1(k)+p2(k),ph(k)+p3(k)+p4(k)
       enddo
       write(*,*) ' '
       write(*,*) ' after reparation (dirt moved under neutrionos) '
       write(*,*) p3v
       write(*,*) p4v
       do k=1,4
         write(*,*) p1(k)+p2(k),ph(k)+p3v(k)+p4v(k)
       enddo
       stop
      END
      SUBROUTINE GPS_HiniPlus(KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph,mph,Hel,Sactu,sProd)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   IR-finite part od 1-photon amplitudes for ISR  (equiv. to GPS_Hini)           //
*//   Photon helicity imported from the calling program                             //
*//                                                                                 //
*//   m_AmpExpo*  is working space                                                  //
*//   m_AmpBorn   is hidden INPUT                                                   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER               KFi,KFf
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph
      DOUBLE COMPLEX        Sactu,sProd,GPS_Sof1,GPS_Sof1b
      INTEGER               Hel
      DOUBLE COMPLEX        AmpBornU(2,2,2,2)
      DOUBLE COMPLEX        AmpBornV(2,2,2,2)
      DOUBLE COMPLEX        Csum1,Csum2,U(2,2),V(2,2),s1(2),s2(2)
      DOUBLE COMPLEX        AmpExpo1,AmpExpo2,AmpBorn
      INTEGER               j,j1,j2,j3,j4,k,Sig
      DOUBLE PRECISION      pr1,pr2,Fleps
      DOUBLE PRECISION      BornV_GetCharge,Qe
      DOUBLE COMPLEX        Vir1,Vir2
*----------------------------------------
      Fleps =  1d-100
      Qe =  BornV_GetCharge( KFi)

* Virtual corrections
      CALL  BVR_MakeVini(m_Alfpi,Qe,p1,m1,p2,m2,ph, Vir1,Vir2)
***   WRITE(*,*) 'ph/ene,Vir1,Vir2  =',ph(4)/p1(4),Vir1,Vir2

* ISR non-infrared two parts: (1) p1 -> photon, contracted with U-matrix
*                             (2) p2 -> photon, contracted with V-matrix
* Calculate Born spin amplitudes
****> CALL GPS_Born(KFi,KFf,PX, p1,Mbeam,  p2,-Mbeam,  p3,Massf,p4,-Massf,m_AmpBorn) !!!!<****

      CALL GPS_Born     (    KFi,KFf,PX,       ph,mph,    p2,-Fleps,  p3,m3,   p4,-m4,   AmpBornU)
      CALL GPS_Born(         KFi,KFf,PX,       p1,Fleps,  ph,-mph,    p3,m3,   p4,-m4,   AmpBornV)
* Fermion propagarotors
      pr1 = 1d0/(p1(4)*ph(4)-p1(3)*ph(3)-p1(2)*ph(2)-p1(1)*ph(1))/2d0
      pr2 =-1d0/(p2(4)*ph(4)-p2(3)*ph(3)-p2(2)*ph(2)-p2(1)*ph(1))/2d0
      Sig = 3-2*Hel
      IF( m_KeyArb .EQ. 0 ) THEN
         CALL GPS_MakeU(ph,Sig,  ph,mph,  p1,m1,    U)
         CALL GPS_MakeV(ph,Sig,  p2,m2,   ph,mph,   V)
      ELSE
         CALL GPS_MakeUb(ph,Sig, ph,mph,  p1,m1,    U)
         CALL GPS_MakeVb(ph,Sig, p2,m2,   ph,mph,   V)
      ENDIF
*--------------------
      IF( m_KeyArb  .EQ.  0 ) THEN
         s1(1)  = -GPS_Sof1( 1,ph,p1)
         s2(1)  =  GPS_Sof1( 1,ph,p2)
      ELSE
         s1(1)  = -GPS_Sof1b( 1,ph,p1,m1)
         s2(1)  =  GPS_Sof1b( 1,ph,p2,m2)
      ENDIF
      s1(2) = -DCONJG(s1(1))
      s2(2) = -DCONJG(s2(1))
*
      
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Csum1=DCMPLX(0d0,0d0)
                  Csum2=DCMPLX(0d0,0d0)
                  DO j=1,2
                     Csum1=Csum1 +DCMPLX(Qe *m_e_QED) *U(j,j1)*pr1 *AmpBornU( j,j2,j3,j4)
                     Csum2=Csum2 +DCMPLX(Qe *m_e_QED) *V(j2,j)*pr2 *AmpBornV(j1, j,j3,j4)
                  ENDDO
                  AmpBorn  =  m_AmpBorn(j1,j2,j3,j4) ! this is possibly both small and wrong
                  AmpExpo1 =  sProd/Sactu*(Csum1+Csum2)
**/////////////// under construction
** (1+Vir1)*AmpBorn is already included in AmpExpo0 so we drop it to avoid double counting
** Note that remaining Vir2*AmpBorn is IR-finite because Vir2->0 in the IR limit
                  AmpExpo2 =  
     $                 sProd/Sactu*(Csum1+Csum2) *(1+Vir1+Vir2)*(1+m_F1fin1) ! non-IR sigle bremss. part
     $                       +sProd*AmpBorn*Vir2                             ! add virtual_non_IR*Born
**//////////////////////
                  m_AmpExpo1(j1,j2,j3,j4) =m_AmpExpo1(j1,j2,j3,j4) +AmpExpo1
                  m_AmpExpo2(j1,j2,j3,j4) =m_AmpExpo2(j1,j2,j3,j4) +AmpExpo2
                  m_AmpExpo2p(j1,j2,j3,j4)=m_AmpExpo2p(j1,j2,j3,j4)+AmpExpo2
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       !!! GPS_HiniPlus

      SUBROUTINE GPS_HiniPlusW(Ibeta,KFi,KFf,PX,p1,m1,p2,m2,p3,m3,p4,m4,ph,mph,Hel,Sactu,sProd)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   IR-finite part od 1-photon amplitudes for ISR W-exch only (ext. of GPS_Hini)  //
*//   Photon helicity imported from the calling program                             //
*//   Ibeta=1 normal mode of operation                                              //
*//   Ibeta=-1 removes action of Ibeta=1 (for m_AmpExpo2, m_AmpExpo2p)              //
*//   Ibeta=0 action at the `fixed transfer level'                                  //
*//                                                                                 //
*//   m_AmpExpo*  is working space                                                  //
*//   m_AmpBorn   is hidden INPUT (defunct)                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    KFi,KFf    ,NUM,kiju,n,itest
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph(4),PTEST(4)
      DOUBLE PRECISION      phv(4),p1v(4),p2v(4),p3v(4),p4v(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph,CosThetD,s0,t0,u0,sa,ta,ua,sb,tb,ub
      DOUBLE COMPLEX        Sactu,sProd,GPS_Sof1,GPS_Sof1b,GPS_Sof1x,GPS_Sof1bx
      INTEGER               Hel
      DOUBLE COMPLEX        AmpBornU(2,2,2,2)
      DOUBLE COMPLEX        AmpBornV(2,2,2,2),AmpBornW(2,2,2,2),AmpBornWT(2,2,2,2)
      DOUBLE COMPLEX        Csum1,Csum2,Csum3,Csum4,U(2,2),V(2,2),UW(2,2),VW(2,2),s1v(2),s2v(2)
      DOUBLE COMPLEX        UWX(2,2),UWX0(2,2),VWX(2,2),VWX0(2,2)
      DOUBLE COMPLEX        Cnor,UW0(2,2),VW0(2,2),WC(2,2)
      DOUBLE COMPLEX        AmpExpo1,AmpExpo2,AmpBorn
      DOUBLE COMPLEX        PropW0,PropWa,PropWb,WVPi0,WVPia,WVPib,EpsDot(2)
      INTEGER               j,j1,j2,j3,j4,k,Sig,JakKoralZ,n1,n2,n3,n4
      DOUBLE PRECISION      pr1,pr2,pr1v,pr2v,pr1vx,pr2vx,Fleps,F,G,f0,g0,h,h0,R,R0
      DOUBLE PRECISION      BornV_GetCharge,Qe
      DOUBLE COMPLEX        Vir1,Vir2
      INTEGER Ibeta,kk,nn
      DOUBLE PRECISION ssum0,ssuma,ssumb
*----------------------------------------


      IF (ABS(KFf).NE.12) RETURN
      IF (IBETA.LT.0)  RETURN

      Fleps =  1d-100
      Cnor  =  1D0
!      Ibeta = 1   ! dip switch for t-transfers 1/0/-1 on/off/remove other wrong !!
      Qe =  BornV_GetCharge( KFi)

* Virtual corrections
      CALL  BVR_MakeVini(m_Alfpi,Qe,p1,m1,p2,m2,ph, Vir1,Vir2)
***   WRITE(*,*) 'ph/ene,Vir1,Vir2  =',ph(4)/p1(4),Vir1,Vir2

* ISR non-infrared two parts: (1) p1 -> photon, contracted with U-matrix
*                             (2) p2 -> photon, contracted with V-matrix
* Calculate Born spin amplitudes
****> CALL GPS_Born(KFi,KFf,PX, p1,Mbeam,  p2,-Mbeam,  p3,Massf,p4,-Massf,m_AmpBorn) !!!!<****

      CALL GPS_Born     (    KFi,KFf,PX,       ph,mph,    p2,-Fleps,  p3,m3,   p4,-m4,   AmpBornU)
      CALL GPS_Born(         KFi,KFf,PX,       p1,Fleps,  ph,-mph,    p3,m3,   p4,-m4,   AmpBornV)
        CALL GPS_BornZero(AmpBornU)
        CALL GPS_BornZero(AmpBornV)
        CALL KinLib_ThetaD(PX,p1,p2,p3,p4,s0,CosThetD)
        t0 = -s0*(1d0-CosThetD)/2d0
        u0 = -s0*(1d0+CosThetD)/2d0
        CALL GPS_BornZero(AmpBornW)
        CALL GPS_BornWPlus(1,0,KFi,KFf,s0,t0,u0, p1,Fleps,    p2,-Fleps,  p3,m3,   p4,-m4,   AmpBornW)
C --   call on test routine
        CALL GPS_BornWPlusT(KFi,KFf,s0,t0,u0, p1,Fleps,    p2,-Fleps,  p3,m3,   p4,-m4,   AmpBornW,AmpBornWT)
C--------
C reduction procedure ....
        CALL GPS_KinExt(p1,m1,p2,m2,p3,m3,p4,m4,ph,mph,phv,p1v,p2v,p3v,p4v)
C--------
         sa=(p1v(4)+p2v(4))**2-(p1v(3)+p2v(3))**2-(p1v(2)+p2v(2))**2-(p1v(1)+p2v(1))**2
         ta=(p4v(4)-p2v(4))**2-(p4v(3)-p2v(3))**2-(p4v(2)-p2v(2))**2-(p4v(1)-p2v(1))**2
         ua=(p3v(4)-p2v(4))**2-(p3v(3)-p2v(3))**2-(p3v(2)-p2v(2))**2-(p3v(1)-p2v(1))**2
         IF (Ibeta.EQ.0) THEN
            sa=s0
            ta=t0
            ua=t0
         ENDIF
         CALL GPS_BornWPlus(1,0,KFi,KFf,sa,ta,ua, ph,mph,    p2,-Fleps,  p3,m3,   p4,-m4,   AmpBornU)

C--------
         sb=(p1v(4)+p2v(4))**2-(p1v(3)+p2v(3))**2-(p1v(2)+p2v(2))**2-(p1v(1)+p2v(1))**2
         tb=(p3v(4)-p1v(4))**2-(p3v(3)-p1v(3))**2-(p3v(2)-p1v(2))**2-(p3v(1)-p1v(1))**2
         ub=(p4v(4)-p1v(4))**2-(p4v(3)-p1v(3))**2-(p4v(2)-p1v(2))**2-(p4v(1)-p1v(1))**2
         IF (Ibeta.EQ.0) THEN
            sb=s0
            tb=t0
            ub=t0
         ENDIF
         CALL GPS_BornWPlus(1,0,KFi,KFf,sb,tb,ub, p1,Fleps,  ph,-mph,    p3,m3,   p4,-m4,   AmpBornV)
* W propagators
         nn=1000
         if (nn.gt.5 ) goto 177
         if (kk.lt.nn.and.p1(4)+p2(4).gt.ph(4)+p3(4)+p4(4)-5.and.ph(4).LT.0.01) then
           kk=kk+1
           ssum0=ssum0+t0
           ssuma=ssuma+max(ta,tb)
           ssumb=ssumb+tb
           write(*,*) kk,nn, t0,ta,tb
         endif
         if (kk.eq.nn) then
           write(*,*) ssum0/nn, ssuma/nn, ssumb/nn
           stop
         endif
 177     continue
         JakKoralZ=0 ! warning: defined in 2 places
         IF(JakKoralZ.eq.1) then
*         W Propagator: it looks crazy in this case ....
          CALL GPS_EWFFactW(KFi,KFf,s0,u0,PropW0,WVPi0)
          CALL GPS_EWFFactW(KFi,KFf,sa,ua,PropWa,WVPia)
          CALL GPS_EWFFactW(KFi,KFf,sb,ub,PropWb,WVPib)
         ELSE
          CALL GPS_EWFFactW(KFi,KFf,s0,t0,PropW0,WVPi0)
          CALL GPS_EWFFactW(KFi,KFf,sa,ta,PropWa,WVPia)
          CALL GPS_EWFFactW(KFi,KFf,sb,tb,PropWb,WVPib)
         ENDIF
         WVPia=WVPi0 ! to keep gauge invariance we install t-transfer in formfactor at 0 order
         WVPib=WVPi0 ! to keep gauge invariance we install t-transfer in formfactor at 0 order
* Fermion propagarotors
      pr1 = 1d0/(p1(4)*ph(4)-p1(3)*ph(3)-p1(2)*ph(2)-p1(1)*ph(1))/2d0
      pr2 =-1d0/(p2(4)*ph(4)-p2(3)*ph(3)-p2(2)*ph(2)-p2(1)*ph(1))/2d0
      pr1v = 1d0/(p1v(4)*phv(4)-p1v(3)*phv(3)-p1v(2)*phv(2)-p1v(1)*phv(1))/2d0
      pr2v =-1d0/(p2v(4)*phv(4)-p2v(3)*phv(3)-p2v(2)*phv(2)-p2v(1)*phv(1))/2d0
      pr1vx= 1d0/(p1v(4)*phv(4))/2d0 * 1D4  ! arbitrary cut off set at 1D4
      pr2vx=-1d0/(p2v(4)*phv(4))/2d0 * 1D4
      Sig = 3-2*Hel
      IF( m_KeyArb .EQ. 0 ) THEN
         CALL GPS_MakeU(ph,Sig,  ph,mph,  p1,m1,    U)
         CALL GPS_MakeV(ph,Sig,  p2,m2,   ph,mph,   V)
         CALL GPS_MakeUW(Cnor,ph,Sig,  p3,m3,   p1,m1,    UW)
         CALL GPS_MakeVW(Cnor,ph,Sig,  p2,m2,   p4,m4,    VW)
      ELSE
         CALL GPS_MakeUb(ph,Sig, ph,mph,  p1,m1,    U)
         CALL GPS_MakeVb(ph,Sig, p2,m2,   ph,mph,   V)
         CALL GPS_MakeUWb(Cnor,ph,Sig, p3,m3,   p1,m1,    UW)
         CALL GPS_MakeVWb(Cnor,ph,Sig, p2,m2,   p4,m4,    VW)
      ENDIF

         CALL GPS_MakeUX(Cnor,ph,Fleps, p3,m3,   p1,m1,    UWX) ! v-a inside
         CALL GPS_MakeVX(Cnor,ph,Fleps, p2,m2,   p4,m4,    VWX) ! v-a inside

         CALL  GPS_MakeUt(KFf,ph,Sig, p2,m2,   p4,m4, p3,m3,   p1,m1,UW,VW) ! test routine !!
         EpsDot(1)=0d0
*--------------------
      IF( m_KeyArb  .EQ.  0 ) THEN
         s1v(1)  = -GPS_Sof1( 1,phv,p1v)
         s2v(1)  =  GPS_Sof1( 1,phv,p2v)
         EpsDot(1)=0.5D0*(-GPS_Sof1x( 1,phv,p1v)+GPS_Sof1x( 1,phv,p3v)  ! 0.5 to compensate 2 from Sof1(b)x
     $                    +GPS_Sof1x( 1,phv,p2v)-GPS_Sof1x( 1,phv,p4v)) ! minis sign is in pr2/4
      ELSE
         s1v(1)  = -GPS_Sof1b( 1,phv,p1v,m1)
         s2v(1)  =  GPS_Sof1b( 1,phv,p2v,m2)
         EpsDot(1)=0.5D0*(-GPS_Sof1bx( 1,phv,p1v,m1)+GPS_Sof1bx( 1,phv,p3v,m3)  ! 0.5 to compensate 2 from Sof1(b)x
     $                    +GPS_Sof1bx( 1,phv,p2v,m2)-GPS_Sof1bx( 1,phv,p4v,m4)) ! minis sign is in pr2/4
      ENDIF
      s1v(2) = -DCONJG(s1v(1))
      s2v(2) = -DCONJG(s2v(1))
      EpsDot(2)=-DCONJG(EpsDot(1))
*
      
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Csum1=DCMPLX(0d0,0d0)
                  Csum2=DCMPLX(0d0,0d0)
                  Csum3=DCMPLX(0d0,0d0)
                  Csum4=DCMPLX(0d0,0d0)
                  IF (Ibeta.EQ.0) THEN
                   DO j=1,2
                     Csum1=Csum1 +      DCMPLX(Qe *m_e_QED) *U(j,j1)*pr1 *AmpBornU( j,j2,j3,j4)
                     Csum2=Csum2 +      DCMPLX(Qe *m_e_QED) *V(j2,j)*pr2 *AmpBornV(j1, j,j3,j4)
                   ENDDO
                  ELSE
                   DO j=1,2
                     Csum1=Csum1 +Ibeta*DCMPLX(Qe *m_e_QED) *U(j,j1)*pr1 *AmpBornU( j,j2,j3,j4)
                     Csum2=Csum2 +Ibeta*DCMPLX(Qe *m_e_QED) *V(j2,j)*pr2 *AmpBornV(j1, j,j3,j4)
                   ENDDO
                  ENDIF 
                     Csum4=Csum4 +Ibeta*DCMPLX(    m_e_QED)*PropWa*WVPia*PropWb*WVPib/WVpi0 !denominator for gauge invariance see up
     $                            *2            ! from  feynman diagram
     $                            *(-0.5D0)     ! fixup originating from  test in  GPS_BornWPlusT
     $                            *(UW(j3,j1)*VWX(j2,j4)-VW(j2,j4)*UWX(j3,j1)) ! non-infrared part of emission from W

                     Csum3=Csum3 +Ibeta*DCMPLX(m_e_QED)*AmpBornW(j1,j2,j3,j4)/PropW0/WVPi0*(
     $                      Qe*s1v(Hel) *(PropWa*WVPia-PropW0*WVPi0)                   ! t-channel W-prop variation
     $                    + Qe*s2v(Hel) *(PropWb*WVPib-PropW0*WVPi0)                   ! t-channel W-prop variation
     $                    + EpsDot(Hel)* PropWa*WVPia*PropWb*WVPib                    ! basically IR emission from W, reduction procedure used only here
     $                                                                               )

C>>>      include 'GPS-t.h'   !!  printouts for tests
C XXX  stability due to gauge and rounding
                 IF (ABS(Csum3).GT.0D0) THEN
!                    include 'GPS-t.h'   !!  printouts for tests
                    IF (pr1vx/pr1v.LT.p1(4)/phv(4)) Csum3=0D0
                    IF (pr2vx/pr2v.LT.p1(4)/phv(4)) Csum3=0D0

C>>                     write(*,*) '==============='
C>>                     write(*,*) pr1vx/pr1v,pr2vx/pr2v,'   ',Csum3
C>>                     write(*,*) 'p1v',p1v
C>>                     write(*,*) 'p2v',p2v
C>>                     write(*,*) 'phv',phv
                  ENDIF
C XXX
                  AmpBorn  =  m_AmpBorn(j1,j2,j3,j4) ! this is possibly both small and wrong
                  AmpExpo1 =  sProd/Sactu*(Csum1+Csum2+Csum3+Csum4)
**/////////////// under construction
** (1+Vir1)*AmpBorn is already included in AmpExpo0 so we drop it to avoid double counting
** Note that remaining Vir2*AmpBorn is IR-finite because Vir2->0 in the IR limit
                  AmpExpo2 =  
     $                 sProd/Sactu*(Csum1+Csum2+Csum3+Csum4) *(1+Vir1+Vir2)*(1+m_F1fin1) ! non-IR sigle bremss. part
**     $                       +sProd*AmpBorn*Vir2 *0                            ! add virtual_non_IR*Born
**//////////////////////
                  IF (Ibeta.NE.-1) THEN
                    m_AmpExpo1(j1,j2,j3,j4) =m_AmpExpo1(j1,j2,j3,j4) +AmpExpo1
                  ENDIF
                    m_AmpExpo2(j1,j2,j3,j4) =m_AmpExpo2(j1,j2,j3,j4) +AmpExpo2
                    m_AmpExpo2p(j1,j2,j3,j4)=m_AmpExpo2p(j1,j2,j3,j4)+AmpExpo2
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       !!! GPS_HiniPlusW


      SUBROUTINE GPS_HfinPlus(KFi,KFf,PX, p1,m1,p2,m2,p3,m3,p4,m4,ph,mph,Hel,Sactu,sProd,CKine)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   IR-finite part od 1-photon amplitudes for FSR  (equiv. to GPS_HfinPlus)       //
*//   Photon helicity is give by the calling program                                //
*//                                                                                 //
*//   Missing contribution in FSR non-IR part due to  svarX/svarQ                   //
*//   Contribution -svarX/svarQ from HERE cancels exactly with svarX/svarQ in beta0 //
*//                                                                                 //
*//   m_AmpExpo*  is working space                                                  //
*//   m_AmpBorn   is hidden INPUT                                                   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    KFi,KFf
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph
*
      DOUBLE COMPLEX        Sactu,sProd,CKine
      INTEGER               Hel
      DOUBLE COMPLEX        AmpBornU(2,2,2,2)
      DOUBLE COMPLEX        AmpBornV(2,2,2,2)
      DOUBLE COMPLEX        Csum1,Csum2,U(2,2),V(2,2)
      INTEGER               j,j1,j2,j3,j4,k,Sig
      DOUBLE PRECISION      pr1,pr2,Fleps
      DOUBLE PRECISION      BornV_GetCharge,Qf
      DOUBLE COMPLEX        Vir1,Vir2
      DOUBLE COMPLEX        AmpExpo1,AmpExpo2,AmpBorn
*----------------------------------------
      Fleps =  1d-100
      Qf =  BornV_GetCharge( KFf)
* Virtual corrections
      CALL  BVR_MakeVfin(m_Alfpi,Qf,p3,m3,p4,m4,ph, Vir1,Vir2)
***      WRITE(*,*) 'ph/ene,Vir1,Vir2 =',ph(4)/p1(4),Vir1,Vir2
* FSR non-infrared two parts: (1) p1 -> photon, contracted with U-matrix
*                             (2) p2 -> photon, contracted with V-matrix
****> CALL GPS_Born(KFi,KFf,PX, p1,Mbeam, p2,-Mbeam,  p3,Massf, p4,-Massf,m_AmpBorn) !!!!<****
      CALL GPS_Born(KFi,KFf,PX, p1,Fleps, p2,-Fleps,  ph,mph,   p4,-m4,   AmpBornU)
      CALL GPS_Born(KFi,KFf,PX, p1,Fleps, p2,-Fleps,  p3,m3,    ph,-mph,  AmpBornV)
* Fermion propagarotors
      pr1 = 1d0/(p3(4)*ph(4)-p3(3)*ph(3)-p3(2)*ph(2)-p3(1)*ph(1))/2d0
      pr2 =-1d0/(p4(4)*ph(4)-p4(3)*ph(3)-p4(2)*ph(2)-p4(1)*ph(1))/2d0
      Sig = 3-2*Hel
      IF( m_KeyArb .EQ. 0 ) THEN
         CALL GPS_MakeU(ph,Sig,    p3,m3,  ph,mph,   U)
         CALL GPS_MakeV(ph,Sig,    ph,mph, p4,m4,    V)
      ELSE
         CALL GPS_MakeUb(ph,Sig,   p3,m3,  ph,mph,   U)
         CALL GPS_MakeVb(ph,Sig,   ph,mph, p4,m4,    V)
      ENDIF
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Csum1=DCMPLX(0d0,0d0)
                  Csum2=DCMPLX(0d0,0d0)
                  DO j=1,2
                     Csum1=Csum1 +DCMPLX(Qf *m_e_QED) *U(j3,j)*pr1* AmpBornU(j1,j2, j,j4)
                     Csum2=Csum2 +DCMPLX(Qf *m_e_QED) *V(j,j4)*pr2* AmpBornV(j1,j2,j3, j)
                  ENDDO
                  AmpBorn  = m_AmpBorn(j1,j2,j3,j4)
*///// first order
                  AmpExpo1 =  
     $                 +sProd/Sactu*(Csum1+Csum2)             !! non-IR sigle bremss. part
     $                 -sProd*CKine*AmpBorn +sProd*AmpBorn    !! compensate for (svarX1/svarQ)
*///// second order
** (1+Vir1)*AmpBorn is already included in AmpExpo2 so we drop it to avoid double counting
** the remaining Vir2*AmpBorn is IR-finite because Vir2->0 in the IR limit
                  AmpExpo2 =  
     $                 +sProd/Sactu*(Csum1+Csum2)*(1+Vir1+Vir2)*(1+m_F1ini1) ! non-IR sigle bremss. part
     $                 +sProd*(-CKine*AmpBorn+AmpBorn)*(1+Vir1)*(1+m_F1ini1) ! compensate for (svarX1/svarQ)
     $                 +sProd*AmpBorn*Vir2                                   ! add virtual_non_IR*Born
*///////////////////////
                  m_AmpExpo1(j1,j2,j3,j4) =m_AmpExpo1(j1,j2,j3,j4) +AmpExpo1
                  m_AmpExpo2(j1,j2,j3,j4) =m_AmpExpo2(j1,j2,j3,j4) +AmpExpo2
                  m_AmpExpo2p(j1,j2,j3,j4)=m_AmpExpo2p(j1,j2,j3,j4)+AmpExpo2
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       !!! GPS_HfinPlus !!!






      SUBROUTINE GPS_HffPlus(CNorm,KFi,KFf,PX,pA,mA,pB,mB,pC,mC,pD,mD,Hel1,ph1,Hel2,ph2,mph,AmpWork) !
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Genuine IR-finite non 1-photon amplitudes for FSR-FSR are added to AmpWork    //
*//   Photon helicity imported from the calling program.                            //
*//                                                                                 //
*//   For Y_IR=1 IR-finite 1-photon contributions are calculated here as well.      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                                    FSR                                            //
*      //                                                                                   //
*      //                            1                  2                                   //
*      //                            |                  |                                   //
*      //             c              |                  |          d                        //
*      //    u  ------<------OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO----<----- v                 //
*      //                                     |                                             //
*      //                                     |X                                            //
*      //                                     |                                             //
*      //                                                                                   //
*      ///////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER            KFi,KFf
      DOUBLE PRECISION   PX(4),pA(4),pB(4),pC(4),pD(4),ph1(4),ph2(4),ph(4)
      DOUBLE PRECISION   mA,mB,mC,mD,mph
      DOUBLE COMPLEX     CNorm,sProd
      INTEGER            Hel1, Hel2
      DOUBLE COMPLEX     BornAB1D(2,2,2,2), BornAB2D(2,2,2,2), BornABCD(2,2,2,2)
      DOUBLE COMPLEX     BornABC1(2,2,2,2), BornABC2(2,2,2,2)
      DOUBLE COMPLEX     BornAB12(2,2,2,2), BornAB21(2,2,2,2)
      DOUBLE COMPLEX     AmpWork(2,2,2,2)
      DOUBLE COMPLEX     Uc11(2,2),   V11d(2,2),   U122(2,2),  V221(2,2)
      DOUBLE COMPLEX     Uc22(2,2),   V22d(2,2),   U211(2,2),  V112(2,2)
      DOUBLE COMPLEX     U21c(2,2),   Vd12(2,2),   Uc12(2,2),  V21d(2,2)
      DOUBLE COMPLEX     U12c(2,2),   Vd21(2,2),   Uc21(2,2),  V12d(2,2)
      DOUBLE COMPLEX     U121(2,2),   V121(2,2),   U212(2,2),  V212(2,2)
      DOUBLE COMPLEX     Su1,Su2,Su3
      INTEGER            j,j1,j2,j3,j4,k,Sig,l
      DOUBLE PRECISION   Fprop1, Fprop2
      DOUBLE PRECISION   prC1, prD1, prC2, prD2, prC12, prD12
      DOUBLE PRECISION   BornV_GetCharge, ChaIni,ChaFin
      DOUBLE COMPLEX     GPS_Sof1,GPS_Sof1b
      DOUBLE COMPLEX     sC(2,2),sD(2,2)
      DOUBLE COMPLEX     gF
      INTEGER            Y_IR, N_IR
      DOUBLE PRECISION   PP12(4),PP1(4),PP2(4),QQ(4),SvarQ,SvarX12,SvarX1,SvarX2
*------------------------------------------------------------------------------------------
      Y_IR=1                  ! YES, IR included
      Y_IR=0                  ! No,  IR not included
      N_IR=1-Y_IR
*--------------------
      ChaFin =  BornV_GetCharge( KFf)
      gF = DCMPLX(ChaFin *m_e_QED)
      IF( m_KeyArb  .EQ.  0 ) THEN
         sC(1,1)  =  gF *GPS_Sof1( 1,ph1,pC)
         sC(2,1)  =  gF *GPS_Sof1( 1,ph2,pC)
         sD(1,1)  = -gF *GPS_Sof1( 1,ph1,pD)
         sD(2,1)  = -gF *GPS_Sof1( 1,ph2,pD)
      ELSE
         sC(1,1)  =  gF *GPS_Sof1b( 1,ph1,pC,mC)
         sC(2,1)  =  gF *GPS_Sof1b( 1,ph2,pC,mC)
         sD(1,1)  = -gF *GPS_Sof1b( 1,ph1,pD,mD)
         sD(2,1)  = -gF *GPS_Sof1b( 1,ph2,pD,mD)
      ENDIF
      sC(1,2) = -DCONJG(sC(1,1))
      sC(2,2) = -DCONJG(sC(2,1))
      sD(1,2) = -DCONJG(sD(1,1))
      sD(2,2) = -DCONJG(sD(2,1))
* Calculate Born spin amplitudes, also with substitutions
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  pC,   mC,   pD,   -mD, BornABCD) ! Standard
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  ph1, mph,   pD,   -mD, BornAB1D) ! C->1
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  pC,   mC,   ph1, -mph, BornABC1) ! D->1
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  ph2, mph,   pD,   -mD, BornAB2D) ! C->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  pC,   mC,   ph2, -mph, BornABC2) ! D->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  ph1, mph,   ph2, -mph, BornAB12) ! C->1,D->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,  pB,-mB,  ph2, mph,   ph1, -mph, BornAB21) ! C->2,D->1
      DO k=1,4
         PP12(k) = pC(k)+pD(k)+ph1(k)+ph2(k)
         PP1 (k) = pC(k)+pD(k)+ph1(k)
         PP2 (k) = pC(k)+pD(k)+ph2(k)
         QQ(k)   = pC(k)+pD(k)
      ENDDO
      svarX12 = PP12(4)**2 -PP12(3)**2 -PP12(2)**2 -PP12(1)**2
      svarX1  =  PP1(4)**2  -PP1(3)**2  -PP1(2)**2  -PP1(1)**2
      svarX2  =  PP2(4)**2  -PP2(3)**2  -PP2(2)**2  -PP2(1)**2
      svarQ   =   QQ(4)**2   -QQ(3)**2   -QQ(2)**2   -QQ(1)**2
* Fermion propagarotors 1
      prC1=  1d0/(pC(4)*ph1(4)-pC(3)*ph1(3)-pC(2)*ph1(2)-pC(1)*ph1(1))/2d0
      prD1= -1d0/(pD(4)*ph1(4)-pD(3)*ph1(3)-pD(2)*ph1(2)-pD(1)*ph1(1))/2d0
* Fermion propagarotors 2
      prC2=  1d0/(pC(4)*ph2(4)-pC(3)*ph2(3)-pC(2)*ph2(2)-pC(1)*ph2(1))/2d0
      prD2= -1d0/(pD(4)*ph2(4)-pD(3)*ph2(3)-pD(2)*ph2(2)-pD(1)*ph2(1))/2d0
* Double propagators
      prC12= 1d0/( pC(4)*ph1(4)- pC(3)*ph1(3)- pC(2)*ph1(2)- pC(1)*ph1(1)
     $           + pC(4)*ph2(4)- pC(3)*ph2(3)- pC(2)*ph2(2)- pC(1)*ph2(1)
     $           +ph1(4)*ph2(4)-ph1(3)*ph2(3)-ph1(2)*ph2(2)-ph1(1)*ph2(1))/2d0
      prD12=-1d0/( pD(4)*ph1(4)- pD(3)*ph1(3)- pD(2)*ph1(2)- pD(1)*ph1(1)
     $            +pD(4)*ph2(4)- pD(3)*ph2(3)- pD(2)*ph2(2)- pD(1)*ph2(1)
     $           +ph1(4)*ph2(4)-ph1(3)*ph2(3)-ph1(2)*ph2(2)-ph1(1)*ph2(1))/2d0
      Fprop1= (1d0/prC1+1d0/prC2)*prC12 -1d0
      Fprop2= (1d0/prD1+1d0/prD2)*prD12 -1d0
      Sig = 3-2*Hel1
      IF( m_KeyArb  .EQ.  0 ) THEN
* end of line
         CALL GPS_MatrV( gF, ph1,Sig,  ph1,mph, pD,mD,     V11d) ! <1|{1}|D>
         CALL GPS_MatrU( gF, ph1,Sig,  pC,mC,   ph1,mph,   Uc11) ! <C|[1]|1>
* false second
         CALL GPS_MatrV( gF, ph1,Sig,  ph2,mph, pD,mD,     V21d) ! <2|{1}|D>
         CALL GPS_MatrU( gF, ph1,Sig,  pC,mC,   ph2,mph,   Uc12) ! <C|[1]|2>
* reverse order
         CALL GPS_MatrV( gF, ph1,Sig,  pD,mD,   ph2,mph,   Vd12) ! <D|{1}|2>
         CALL GPS_MatrU( gF, ph1,Sig,  ph2,mph, pC,mC,     U21c) ! <2|[1]|C>
* xk-xk term case, ph2 first
         CALL GPS_MatrV( gF, ph1,Sig,  ph1,mph, ph2,mph,   V112) ! <1|{1}|2>
         CALL GPS_MatrU( gF, ph1,Sig,  ph2,mph, ph1,mph,   U211) ! <2|[1]|1>
* xk-xk term case ph2 first
         CALL GPS_MatrV( gF, ph1,Sig,  ph2,mph, ph2,mph,   V212) ! <2|{1}|2>
         CALL GPS_MatrU( gF, ph1,Sig,  ph2,mph, ph2,mph,   U212) ! <2|[1]|2>
      ELSE
         CALL GPS_MatrVb(gF, ph1,Sig,  ph1,mph, pD,mD,     V11d)
         CALL GPS_MatrUb(gF, ph1,Sig,  pC,mC,   ph1,mph,   Uc11)
* falSe second
         CALL GPS_MatrVb(gF, ph1,Sig,  ph2,mph, pD,mD,     V21d)
         CALL GPS_MatrUb(gF, ph1,Sig,  pC,mC,   ph2,mph,   Uc12)
* reverse order
         CALL GPS_MatrVb(gF, ph1,Sig,  pD,mD,   ph2,mph,   Vd12)
         CALL GPS_MatrUb(gF, ph1,Sig,  ph2,mph, pC,mC,     U21c)
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrVb(gF, ph1,Sig,  ph1,mph, ph2,mph,   V112)
         CALL GPS_MatrUb(gF, ph1,Sig,  ph2,mph, ph1,mph,   U211)
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrVb(gF, ph1,Sig,  ph2,mph, ph2,mph,   V212)
         CALL GPS_MatrUb(gF, ph1,Sig,  ph2,mph, ph2,mph,   U212)
      ENDIF
      Sig = 3-2*Hel2
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrV( gF, ph2,Sig,  ph2,mph,  pD,mD,    V22d) ! <2|{2}|D>
         CALL GPS_MatrU( gF, ph2,Sig,  pC,mC,   ph2,mph,   Uc22) ! <C|[2]|2>
* falSe second
         CALL GPS_MatrV( gF, ph2,Sig,  ph1,mph,  pD,mD,    V12d) ! <1|{2}|D>
         CALL GPS_MatrU( gF, ph2,Sig,  pC,mC,   ph1,mph,   Uc21) ! <C|[2]|1>
* reverse order
         CALL GPS_MatrV( gF, ph2,Sig,  pD,mD,   ph1,mph,   Vd21) ! <D|{2}|1>
         CALL GPS_MatrU( gF, ph2,Sig,  ph1,mph, pC,mC,     U12c) ! <1|[2]|C>
* xk-xk term, ph1 first
         CALL GPS_MatrV( gF, ph2,Sig,  ph2,mph, ph1,mph,   V221) ! <2|{2}|1>
         CALL GPS_MatrU( gF, ph2,Sig,  ph1,mph, ph2,mph,   U122) ! <1|[2]|2>
* xk-xk term, ph1 first 
         CALL GPS_MatrV( gF, ph2,Sig,  ph1,mph, ph1,mph,   V121) ! <1|{2}|1>
         CALL GPS_MatrU( gF, ph2,Sig,  ph1,mph, ph1,mph,   U121) ! <1|[2]|1>
      ELSE
         CALL GPS_MatrVb(gF, ph2,Sig,  ph2,mph,  pD,mD,    V22d)
         CALL GPS_MatrUb(gF, ph2,Sig,  pC,mC,   ph2,mph,   Uc22)
* falSe second
         CALL GPS_MatrVb(gF, ph2,Sig,  ph1,mph,  pD,mD,    V12d)
         CALL GPS_MatrUb(gF, ph2,Sig,  pC,mC,   ph1,mph,   Uc21)
* reverse order
         CALL GPS_MatrVb(gF, ph2,Sig,  pD,mD,   ph1,mph,   Vd21)
         CALL GPS_MatrUb(gF, ph2,Sig,  ph1,mph, pC,mC,     U12c)
* for the case when there was ph1 first xk-xk term
         CALL GPS_MatrVb(gF, ph2,Sig,  ph2,mph, ph1,mph,   V221)
         CALL GPS_MatrUb(gF, ph2,Sig,  ph1,mph, ph2,mph,   U122)
* for the case when there was ph1 first xk-xk term
         CALL GPS_MatrVb(gF, ph2,Sig,  ph1,mph, ph1,mph,   V121)
         CALL GPS_MatrUb(gF, ph2,Sig,  ph1,mph, ph1,mph,   U121)
      ENDIF
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Su1 = DCMPLX(0d0,0d0)
                  DO j=1,2
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     1|            2|                                              //
*      //               c      |    c+m+1    |    c+m+1+2          -d                       //
*      //       u  -----<------S-----<-------U------<-------O-------<------ v               //
*      //                                                   |X                              //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +(sC(1,Hel1)*(prC12-prC2*N_IR))*Uc22(j3,j)*BornAB2D(j1,j2,j,j4) !<c|(1)c[2]2|X|d>
                     Su1=Su1  +sC(1,Hel1)* prC12            *Uc21(j3,j)*BornAB1D(j1,j2,j,j4) !<c|(1)c[2]1|X|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     2|            1|                                              //
*      //               c      |    c+m+2    |    c+m+1+2          -d                       //
*      //       u  -----<------S-----<-------U------<-------O-------<------ v               //
*      //                                                   |X                              //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+(sC(2,Hel2)*(prC12-prC1*N_IR))*Uc11(j3,j)*BornAB1D(j1,j2,j,j4) !<c|(2)c[1]1|X|d>
                     Su1=Su1 +sC(2,Hel2)* prC12            *Uc12(j3,j)*BornAB2D(j1,j2,j,j4) !<c|(2)c[1]2|X|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                                    |1             |2                              //
*      //               c          -d+m-1-2  |   -d+m-2     |       -d                      //
*      //       u  -----<------O-----<-------V-----<--------S--------<----- v               //
*      //                     X|                                                            //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +BornABC1(j1,j2,j3,j)*V11d(j,j4)*( sD(2,Hel2)*(prD12-prD1*N_IR))!<c|X|1{1}d(2)|d>
                     Su1=Su1 +BornABC2(j1,j2,j3,j)*V21d(j,j4)*  sD(2,Hel2)* prD12            !<c|X|2{1}d(2)|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                                    |2             |1                              //
*      //               c          -d+m-1-2  |   -d+m-1     |       -d                      //
*      //       u  -----<------O-----<-------V-----<--------S--------<----- v               //
*      //                     X|                                                            //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +BornABC2(j1,j2,j3,j)*V22d(j,j4)*( sD(1,Hel1)*(prD12-prD2*N_IR))!<c|X|2{2}d(1)|d>
                     Su1=Su1 +BornABC1(j1,j2,j3,j)*V12d(j,j4)  *sD(1,Hel1)* prD12            !<c|X|1{2}d(1)|d>
                  ENDDO
                  Su3 = DCMPLX(0d0,0d0)
                  DO j=1,2
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     2|                            |1                              //
*      //               c      |    c+m+1         c+m+1     |      -d                       //
*      //       u  -----<------U-----<-------O------<-------S-------<------ v               //
*      //                                    |X                                             //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su3=Su3 +Uc22(j3,j)*prC2  *BornAB2D(j1,j2,j,j4) *sD(1,Hel1) *Y_IR !<c|[2]c|X|1(1)|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     1|                            |2                              //
*      //               c      |    c+m+1         c+m+2     |      -d                       //
*      //       u  -----<------U-----<-------O------<-------S-------<------ v               //
*      //                                    |X                                             //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su3=Su3 +Uc11(j3,j)*prC1 *BornAB1D(j1,j2,j,j4)  *sD(2,Hel2) *Y_IR !<c|[1]c|X|2(2)|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     2|                            |1                              //
*      //               c      |    c+m+2         c+m+1     |      -d                       //
*      //       u  -----<------S-----<-------O------<-------U-------<------ v               //
*      //                                    |X                                             //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su3=Su3 +sC(2,Hel2) *BornABC1(j1,j2,j3,j) *prD1 *V11d(j,j4) *Y_IR !<c|(2)c|X|1{1}|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     1|                            |2                              //
*      //               c      |    c+m+1         c+m+2     |      -d                       //
*      //       u  -----<------S-----<-------O------<-------U-------<------ v               //
*      //                                    |X                                             //
*      ///////////////////////////////////////////////////////////////////////////////////////
                     Su3=Su3 +sC(1,Hel1) *BornABC2(j1,j2,j3,j) *prD2 *V22d(j,j4) *Y_IR !<c|(1)c|X|2{2}|d>
                  ENDDO
                  Su2 = DCMPLX(0d0,0d0)
                  DO j=1,2
                     DO l=1,2
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                      |2                           |1                              //
*      //               c      |    c+m+2        -d+m-1     |       -d                      //
*      //       u  -----<------U-----<-------O-----<--------V--------<----- v               //
*      //                                    |X                                             //
*      ///////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Uc22( j3,l)*prC2 *BornAB21(j1,j2,l,j ) *V11d( j,j4)*prD1 !<c|[2]2|X|1{1}|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                      |1                           |2                              //
*      //               c      |    c+m+1        -d+m-2     |       -d                      //
*      //       u  -----<------U-----<-------O-----<--------V--------<----- v               //
*      //                                    |X                                             //
*      ///////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Uc11( j3,l)*prC1 *BornAB12(j1,j2,l,j ) *V22d( j,j4)*prD2 !<c|[1]1|X|2{2}|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     1|            2|                                              //
*      //               c      |    c+m+1    |    c+m+1+2          -d                       //
*      //       u  -----<------U-----<-------O------<-------V-------<------ v               //
*      //                                                  X|                               //
*      ///////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Uc11( j3,l)*prC1  *U122(l,j)*prC12  *BornAB2D(j1,j2,j ,j4) ! <c|[1]1[2]2|X|d>
                        Su2=Su2 +Uc11( j3,l)*prC1  *U121(l,j)*prC12  *BornAB1D(j1,j2,j ,j4) ! <c|[1]1[2]1|X|d>
                        Su2=Su2 +Uc11( j3,l)*prC1  *U12c(l,j)*prC12  *BornABCD(j1,j2,j ,j4) ! <c|[1]1[2]c|X|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                     2|            1|                                              //
*      //               c      |    c+m+2    |    c+m+1+2          -d                       //
*      //       u  -----<------U-----<-------U------<-------O-------<------ v               //
*      //                                                  X|                               //
*      ///////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Uc22( j3,l)*prC2  *U211(l,j)*prC12  *BornAB1D(j1,j2,j ,j4) ! <c|[2]2[1]1|X|d>
                        Su2=Su2 +Uc22( j3,l)*prC2  *U212(l,j)*prC12  *BornAB2D(j1,j2,j ,j4) ! <c|[2]2[1]2|X|d>
                        Su2=Su2 +Uc22( j3,l)*prC2  *U21c(l,j)*prC12  *BornABCD(j1,j2,j ,j4) ! <c|[2]2[1]c|X|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                                    |2             |1                              //
*      //               c          -d+m-1-2  |   -d+m-1     |       -d                      //
*      //       u  -----<------O-----<-------V-----<--------V--------<----- v               //
*      //                      |X                                                           //
*      ///////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +BornABC2(j1,j2,j3,j ) *V221(j,l )*prD12 *V11d( l,j4)*prD1  ! <c|X|2{2}1{1}|d>
                        Su2=Su2 +BornABC1(j1,j2,j3,j ) *V121(j,l )*prD12 *V11d( l,j4)*prD1  ! <c|X|1{2}1{1}|d>
                        Su2=Su2 +BornABCD(j1,j2,j3,j ) *Vd21(j,l )*prD12 *V11d( l,j4)*prD1  ! <c|X|d{2}1{1}|d>
*      ///////////////////////////////////////////////////////////////////////////////////////
*      //                                    |1             |2                              //
*      //               c          -d+m-1-2  |   -d+m-2     |       -d                      //
*      //       u  -----<------O-----<-------V-----<--------V--------<----- v               //
*      //                      |X                                                           //
*      ///////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +BornABC1(j1,j2,j3,j ) *V112(j,l )*prD12 *V22d( l,j4)*prD2  ! <c|X|1{1}2{2}|d>
                        Su2=Su2 +BornABC2(j1,j2,j3,j ) *V212(j,l )*prD12 *V22d( l,j4)*prD2  ! <c|X|2{1}2{2}|d>
                        Su2=Su2 +BornABCD(j1,j2,j3,j ) *Vd12(j,l )*prD12 *V22d( l,j4)*prD2  ! <c|X|d{1}2{2}|d>
                     ENDDO
                  ENDDO
                  sProd = (sC(1,Hel1)+sD(1,Hel1)) *( sC(2,Hel2)+sD(2,Hel2)) !
                  AmpWork(j1,j2,j3,j4) = AmpWork(j1,j2,j3,j4)
     $                 +CNorm*( Su1 +Su2 +Su3)
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*( sC(1,Hel1)*sC(2,Hel2)*Fprop1   !
     $                                               +sD(1,Hel1)*sD(2,Hel2)*Fprop2 ) !
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*sProd *(1d0 -svarX12/svarQ)  *N_IR !
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*sProd *( svarX1/svarQ -1d0)  *N_IR !
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*sProd *( svarX2/svarQ -1d0)  *N_IR !
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*sProd                        *Y_IR !
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       ! GPS_HffPlus



      SUBROUTINE GPS_HiiPlus(CNorm,KFi,KFf,PX,pA,mA,pB,mB,pC,mC,pD,mD,Hel1,ph1,Hel2,ph2,mph,AmpWork) !
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Genuine IR-finite non 1-photon amplitudes for ISR-ISR are added to AmpWork    //
*//   That is for dip-switch Y_IR=0.                                                //
*//   For Y_IR=1 IR-finite 1-photon contributions are calculated here as well       //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*//                                        |                                        //
*//                              1         |          2                             //
*//                              |         |X         |                             //
*//      _       -b              |         |          |          a                  //
*//      v  ------<------OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO----<----- u           //
*//                                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           KFi,KFf
      DOUBLE PRECISION  PX(4),pA(4),pB(4),pC(4),pD(4),ph1(4),ph2(4),ph(4)
      DOUBLE PRECISION  mA,mB,mC,mD,mph
      DOUBLE COMPLEX    CNorm
      INTEGER           Hel1, Hel2
      DOUBLE COMPLEX    Born1BCD(2,2,2,2), Born2BCD(2,2,2,2), BornABCD(2,2,2,2)
      DOUBLE COMPLEX    BornA1CD(2,2,2,2), BornA2CD(2,2,2,2)
      DOUBLE COMPLEX    Born12CD(2,2,2,2), Born21CD(2,2,2,2)
      DOUBLE COMPLEX    AmpWork(2,2,2,2)
      DOUBLE COMPLEX    U11a(2,2),Vb11(2,2),U221(2,2),V122(2,2)
      DOUBLE COMPLEX    U22a(2,2),Vb22(2,2),U112(2,2),V211(2,2)
      DOUBLE COMPLEX    Ua12(2,2),V21b(2,2),U21a(2,2),Vb12(2,2)
      DOUBLE COMPLEX    Ua21(2,2),V12b(2,2),U12a(2,2),Vb21(2,2)
      DOUBLE COMPLEX    U121(2,2),V121(2,2),U212(2,2),V212(2,2)
      DOUBLE COMPLEX    Su1,Su2,sProd
      INTEGER           j,j1,j2,j3,j4,k,Sig,l
      DOUBLE PRECISION  prA1,prB1,prA2,prB2,prA12,prB12
      DOUBLE PRECISION  BornV_GetCharge,ChaIni
      DOUBLE PRECISION  Fprop1,Fprop2
      DOUBLE COMPLEX    gI
      DOUBLE COMPLEX    GPS_Sof1,GPS_Sof1b
      DOUBLE COMPLEX    sA(2,2),sB(2,2)
      INTEGER           Y_IR,N_IR
*------------------------------------------------------------------------------------------
      Y_IR=1                  ! YES, IR included
      Y_IR=0                  ! No,  IR not included
      N_IR=1-Y_IR
*--------------------
      ChaIni =  BornV_GetCharge( KFi)
      gI = DCMPLX(ChaIni*m_e_QED)
      IF( m_KeyArb  .EQ.  0 ) THEN
         sA(1,1)  = -gI*GPS_Sof1( 1,ph1,pA)
         sA(2,1)  = -gI*GPS_Sof1( 1,ph2,pA)
         sB(1,1)  =  gI*GPS_Sof1( 1,ph1,pB)
         sB(2,1)  =  gI*GPS_Sof1( 1,ph2,pB)
      ELSE
         sA(1,1)  = -gI*GPS_Sof1b( 1,ph1,pA,mA)
         sA(2,1)  = -gI*GPS_Sof1b( 1,ph2,pA,mA)
         sB(1,1)  =  gI*GPS_Sof1b( 1,ph1,pB,mB)
         sB(2,1)  =  gI*GPS_Sof1b( 1,ph2,pB,mB)
      ENDIF
      sA(1,2) = -DCONJG(sA(1,1))
      sA(2,2) = -DCONJG(sA(2,1))
      sB(1,2) = -DCONJG(sB(1,1))
      sB(2,2) = -DCONJG(sB(2,1))
* Calculate Born spin amplitudes
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    pB,-mB,      pC,MC,   pD,-mD,   BornABCD) ! Standard
      CALL GPS_Born(KFi,KFf,PX, ph1,mph,  pB,-mB,      pC,mC,   pD,-mD,   Born1BCD) ! A->1
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    ph1,-mph,    pC,mC,   pD,-mD,   BornA1CD) ! B->1
      CALL GPS_Born(KFi,KFf,PX, ph2,mph,  pB,-mB,      pC,mC,   pD,-mD,   Born2BCD) ! A->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    ph2,-mph,    pC,mC,   pD,-mD,   BornA2CD) ! B->2
      CALL GPS_Born(KFi,KFf,PX, ph1,mph,  ph2,-mph,    pC,mC,   pD,-mD,   Born12CD) ! A->1,B->2
      CALL GPS_Born(KFi,KFf,PX, ph2,mph,  ph1,-mph,    pC,mC,   pD,-mD,   Born21CD) ! A->2,B->1
* Fermion propagarotors ini1
      prA1= 1d0/(pA(4)*ph1(4)-pA(3)*ph1(3)-pA(2)*ph1(2)-pA(1)*ph1(1))/2d0
      prB1=-1d0/(pB(4)*ph1(4)-pB(3)*ph1(3)-pB(2)*ph1(2)-pB(1)*ph1(1))/2d0
* Fermion propagarotors ini2
      prA2= 1d0/(pA(4)*ph2(4)-pA(3)*ph2(3)-pA(2)*ph2(2)-pA(1)*ph2(1))/2d0
      prB2=-1d0/(pB(4)*ph2(4)-pB(3)*ph2(3)-pB(2)*ph2(2)-pB(1)*ph2(1))/2d0
* DOUBLE propagators
      prA12= 1d0/( pA(4)*ph1(4)- pA(3)*ph1(3)- pA(2)*ph1(2)- pA(1)*ph1(1)
     $           + pA(4)*ph2(4)- pA(3)*ph2(3)- pA(2)*ph2(2)- pA(1)*ph2(1)
     $           -ph1(4)*ph2(4)+ph1(3)*ph2(3)+ph1(2)*ph2(2)+ph1(1)*ph2(1)
     $           )/2d0
      prB12=-1d0/( pB(4)*ph1(4)- pB(3)*ph1(3)- pB(2)*ph1(2)- pB(1)*ph1(1)
     $            +pB(4)*ph2(4)- pB(3)*ph2(3)- pB(2)*ph2(2)- pB(1)*ph2(1)
     $           -ph1(4)*ph2(4)+ph1(3)*ph2(3)+ph1(2)*ph2(2)+ph1(1)*ph2(1)
     $           )/2d0
      Fprop1=(1d0/prA1+1d0/prA2)*prA12-1d0
      Fprop2=(1d0/prB1+1d0/prB2)*prB12-1d0
      Sig = 3-2*Hel1
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrU( gI, ph1,Sig,  ph1,mph, pA,mA,     U11a) ! <1|[1]|a>
         CALL GPS_MatrV( gI, ph1,Sig,  pB,mB,   ph1,mph,   Vb11) ! <b|{1}|1>
* falSe second
         CALL GPS_MatrU( gI, ph1,Sig,  ph2,mph, pA,mA,     U21a) ! <2|[1]|a>
         CALL GPS_MatrV( gI, ph1,Sig,  pB,mB,   ph2,mph,   Vb12) ! <b|{1}|2>
* reverse order
         CALL GPS_MatrU( gI, ph1,Sig,  pA,mA,   ph2,mph,   Ua12) ! <a|[1]|2>
         CALL GPS_MatrV( gI, ph1,Sig,  ph2,mph, pB,mB,     V21b) ! <2|{1}|b>
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrU( gI, ph1,Sig,  ph1,mph, ph2,mph,   U112) ! <1|[1]|2>
         CALL GPS_MatrV( gI, ph1,Sig,  ph2,mph, ph1,mph,   V211) ! <2|{1}|1>
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrU( gI, ph1,Sig,  ph2,mph, ph2,mph,   U212) ! <2|[1]|2>
         CALL GPS_MatrV( gI, ph1,Sig,  ph2,mph, ph2,mph,   V212) ! <2|{1}|2>
      ELSE
         CALL GPS_MatrUb(gI, ph1,Sig,  ph1,mph, pA,mA,     U11a)
         CALL GPS_MatrVb(gI, ph1,Sig,  pB,mB,   ph1,mph,   Vb11)
* falSe second
         CALL GPS_MatrUb(gI, ph1,Sig,  ph2,mph, pA,mA,     U21a)
         CALL GPS_MatrVb(gI, ph1,Sig,  pB,mB,   ph2,mph,   Vb12)
* reverse order
         CALL GPS_MatrUb(gI, ph1,Sig,  pA,mA,   ph2,mph,   Ua12)
         CALL GPS_MatrVb(gI, ph1,Sig,  ph2,mph, pB,mB,     V21b)
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrUb(gI, ph1,Sig,  ph1,mph, ph2,mph,   U112)
         CALL GPS_MatrVb(gI, ph1,Sig,  ph2,mph, ph1,mph,   V211)
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrUb(gI, ph1,Sig,  ph2,mph, ph2,mph,   U212)
         CALL GPS_MatrVb(gI, ph1,Sig,  ph2,mph, ph2,mph,   V212)
      ENDIF
      Sig = 3-2*Hel2
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrU( gI, ph2,Sig,  ph2,mph,  pA,mA,    U22a) ! <2|[2]|a>
         CALL GPS_MatrV( gI, ph2,Sig,  pB,mB,   ph2,mph,   Vb22) ! <b|{2}|2>
* falSe second
         CALL GPS_MatrU( gI, ph2,Sig,  ph1,mph,  pA,mA,    U12a) ! <1|[2]|a>
         CALL GPS_MatrV( gI, ph2,Sig,  pB,mB,   ph1,mph,   Vb21) ! <b|{2}|1>
* reverse order
         CALL GPS_MatrU( gI, ph2,Sig,  pA,mA,   ph1,mph,   Ua21) ! <a|[2]|1>
         CALL GPS_MatrV( gI, ph2,Sig,  ph1,mph, pB,mB,     V12b) ! <1|{2}|b>
* for the case when there was ph1 first xk-xk term
         CALL GPS_MatrU( gI, ph2,Sig,  ph2,mph, ph1,mph,   U221) ! <2|[2]|1>
         CALL GPS_MatrV( gI, ph2,Sig,  ph1,mph, ph2,mph,   V122) ! <1|{2}|2>
c for the case when there was ph1 first xk-xk term
         CALL GPS_MatrU( gI, ph2,Sig,  ph1,mph, ph1,mph,   U121) ! <1|[2]|1>
         CALL GPS_MatrV( gI, ph2,Sig,  ph1,mph, ph1,mph,   V121) ! <1|{2}|1>
      ELSE
         CALL GPS_MatrUb(gI, ph2,Sig,  ph2,mph,  pA,mA,    U22a)
         CALL GPS_MatrVb(gI, ph2,Sig,  pB,mB,   ph2,mph,   Vb22)
c falSe second
         CALL GPS_MatrUb(gI, ph2,Sig,  ph1,mph,  pA,mA,    U12a)
         CALL GPS_MatrVb(gI, ph2,Sig,  pB,mB,   ph1,mph,   Vb21)
c reverse order
         CALL GPS_MatrUb(gI, ph2,Sig,  pA,mA,   ph1,mph,   Ua21)
         CALL GPS_MatrVb(gI, ph2,Sig,  ph1,mph, pB,mB,     V12b)
c for the case when there was ph1 first xk-xk term
         CALL GPS_MatrUb(gI, ph2,Sig,  ph2,mph, ph1,mph,   U221)
         CALL GPS_MatrVb(gI, ph2,Sig,  ph1,mph, ph2,mph,   V122)
c for the case when there was ph1 first xk-xk term
         CALL GPS_MatrUb(gI, ph2,Sig,  ph1,mph, ph1,mph,   U121)
         CALL GPS_MatrVb(gI, ph2,Sig,  ph1,mph, ph1,mph,   V121)
      ENDIF
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Su1 = DCMPLX(0d0,0d0)
                  DO j=1,2
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  1               2                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-2    |      a                  //
*        //      v  ------<----O--------<---------U--------<------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+Born1BCD(j,j2,j3,j4) *U11a(j,j1)*(prA12-prA1*N_IR)*sA(2,Hel2) !<b|X|1[1]a(2)|a>
                     Su1=Su1+Born2BCD(j,j2,j3,j4) *U21a(j,j1)*   prA12         *sA(2,Hel2) !<b|X|2[1]a(2)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  2               1                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-1    |      a                  //
*        //      v  ------<----O--------<---------U-------<-------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+Born2BCD(j,j2,j3,j4) *U22a(j,j1)*(prA12-prA2*N_IR)*sA(1,Hel1) !<b|X|2[2]a(1)|a>
                     Su1=Su1+Born1BCD(j,j2,j3,j4) *U12a(j,j1)* prA12           *sA(1,Hel1) !<b|X|1[2]a(1)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  1               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+2       |    -b+m+1+2   |      a                  //
*        //      v  ------<----S--------<---------V--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+( sB(2,Hel2)*(-prB1*N_IR+prB12))*Vb11(j2,j)*BornA1CD(j1,j,j3,j4)!<b|(2)b[1]1|X|a>
                     Su1=Su1+( sB(2,Hel2))           *prB12  *Vb12(j2,j)*BornA2CD(j1,j,j3,j4)!<b|(2)b[1]2|X|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  2               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+1       |    -b+m+1+2   |      a                  //
*        //      v  ------<----S--------<---------V--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +(sB(1,Hel1)*(-prB2*N_IR+prB12))*Vb22(j2,j)*BornA2CD(j1,j,j3,j4)!<b|(1)b[2]2|X|a>
                     Su1=Su1 +(sB(1,Hel1))           *prB12  *Vb21(j2,j)*BornA1CD(j1,j,j3,j4)!<b|(1)b[2]1|X|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  |               1                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+2       |     a+m-1     |      a                  //
*        //      v  ------<----S--------<---------O--------<------U------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +sB(2,Hel2)    *Born1BCD(j,j2,j3,j4) *prA1*U11a(j,j1) *Y_IR !<b|(2)2|X|1[1]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  |               2                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+1       |     a+m-2     |      a                  //
*        //      v  ------<----S--------<---------O--------<------U------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +sB(1,Hel1)    *Born2BCD(j,j2,j3,j4) *prA2*U22a(j,j1) *Y_IR !<b|(1)1|X|2[2]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  |               2                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+1       |     a+m-2     |      a                  //
*        //      v  ------<----V--------<---------O--------<------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Vb11(j2,j)*prB1 *BornA1CD(j1,j,j3,j4) *sA(2,Hel2)    *Y_IR !<b|[1]1|X|a[2]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  |               1                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+2       |     a+m-1     |      a                  //
*        //      v  ------<----V--------<---------O--------<------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Vb22(j2,j)*prB2 *BornA2CD(j1,j,j3,j4) *sA(1,Hel1)    *Y_IR !<b|[2]2|X|a(1)|a>
                  ENDDO
                  Su2 = DCMPLX(0d0,0d0)
                  DO j=1,2
                     DO l=1,2
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  |               1                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+2       |     a+m-1     |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2  +Vb22(j2,l)*prB2  *Born12CD(j,l,j3,j4 )  *U11a(j,j1)*prA1 ! <b|[2]2|X|1[1]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  |               2                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+1       |     a+m-2     |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2  +Vb11(j2,l)*prB1  *Born21CD(j,l,j3,j4 )  *U22a(j,j1)*prA2 ! <b|[1]1|X|2[2]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  2               1                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-1    |      a                  //
*        //      v  ------<----O--------<---------O--------<------*------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Born1BCD(j,j2,j3,j4) *U121(j,l)*prA12 *U11a(l,j1)*prA1  ! <b|X|1[2]1(1)|a>
                        Su2=Su2 +Born2BCD(j,j2,j3,j4) *U221(j,l)*prA12 *U11a(l,j1)*prA1  ! <b|X|2[2]1(1)|a>
                        Su2=Su2 -BornABCD(j,j2,j3,j4) *Ua21(j,l)*prA12 *U11a(l,j1)*prA1  ! <b|X|a[2]1(1)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  1               2                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-2    |      a                  //
*        //      v  ------<----O--------<---------O--------<------*------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Born2BCD(j,j2,j3,j4) *U212(j,l)*prA12  *U22a(l,j1)*prA2 ! <b|X|2[1]2(2)|a>
                        Su2=Su2 +Born1BCD(j,j2,j3,j4) *U112(j,l)*prA12  *U22a(l,j1)*prA2 ! <b|X|1[1]2(2)|a>
                        Su2=Su2 -BornABCD(j,j2,j3,j4) *Ua12(j,l)*prA12  *U22a(l,j1)*prA2 ! <b|X|a[1]2(2)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  2               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+1       |    -b+m+1+2   |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Vb11(j2,l)*prB1 *V121(l,j)*prB12 *BornA1CD(j1,j,j3,j4) ! <b|(1)1[2]1|X|a>
                        Su2=Su2 +Vb11(j2,l)*prB1 *V122(l,j)*prB12 *BornA2CD(j1,j,j3,j4) ! <b|(1)1[2]2|X|a>
                        Su2=Su2 -Vb11(j2,l)*prB1 *V12b(l,j)*prB12 *BornABCD(j1,j,j3,j4) ! <b|(1)1[2]b|X|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  1               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+2       |    -b+m+1+2   |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Vb22(j2,l)*prB2 *V212(l,j)*prB12  *BornA2CD(j1,j,j3,j4) ! <b|(2)2[1]2|X|a>
                        Su2=Su2 +Vb22(j2,l)*prB2 *V211(l,j)*prB12  *BornA1CD(j1,j,j3,j4) ! <b|(2)2[1]1|X|a>
                        Su2=Su2 -Vb22(j2,l)*prB2 *V21b(l,j)*prB12  *BornABCD(j1,j,j3,j4) ! <b|(2)2[2]b|X|a>
                     ENDDO
                  ENDDO
                  sProd = ( sA(1,Hel1)+sB(1,Hel1))*( sA(2,Hel2)+sB(2,Hel2))
                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4)
     $                 +CNorm*( Su1+Su2 )
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*( sA(1,Hel1)*sA(2,Hel2)*Fprop1  !
     $                                               +sB(1,Hel1)*sB(2,Hel2)*Fprop2) !
     $                 +CNorm*BornABCD(j1,j2,j3,j4)* sProd                   *Y_IR  !
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       ! GPS_HiiPlus    


      SUBROUTINE GPS_HiiPlusW(CNorm,KFi,KFf,PX,pAo,mA,pBo,mB,pCo,mC,pDo,mD,Hel1,ph1o,Hel2,ph2o,mph,AmpWork) !
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   IR-semi-finite (only double IR-excluded) W-exchange amplitudes add to AmpWork //
*//   That is for dip-switch Y_IR=0, Y_IR1=1, other settings not checked.           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*//                                        |                                        //
*//                              1         |          2                             //
*//                              |         |X         |                             //
*//      _       -b              |         |          |          a                  //
*//      v  ------<------OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO----<----- u           //
*//                                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           KFi,KFf
      DOUBLE PRECISION  pAo(4),pBo(4),pCo(4),pDo(4),ph1o(4),ph2o(4)
      DOUBLE PRECISION  PX(4),pA(4),pB(4),pC(4),pD(4),ph1(4),ph2(4),ph(4)
      DOUBLE PRECISION  mA,mB,mC,mD,mph
      DOUBLE COMPLEX    CNorm,Cnor
      INTEGER           Hel1, Hel2
      DOUBLE COMPLEX    Born1BCD(2,2,2,2), Born2BCD(2,2,2,2), BornABCD(2,2,2,2)
      DOUBLE COMPLEX    BornA1CD(2,2,2,2), BornA2CD(2,2,2,2)
      DOUBLE COMPLEX    Born12CD(2,2,2,2), Born21CD(2,2,2,2)
      DOUBLE COMPLEX    AmpWork(2,2,2,2)
      DOUBLE COMPLEX    U11a(2,2),Vb11(2,2),U221(2,2),V122(2,2)
      DOUBLE COMPLEX    U22a(2,2),Vb22(2,2),U112(2,2),V211(2,2)
      DOUBLE COMPLEX    Ua12(2,2),V21b(2,2),U21a(2,2),Vb12(2,2)
      DOUBLE COMPLEX    Ua21(2,2),V12b(2,2),U12a(2,2),Vb21(2,2)
      DOUBLE COMPLEX    U121(2,2),V121(2,2),U212(2,2),V212(2,2)
      DOUBLE COMPLEX    UCAW1(2,2),VBDW1(2,2),UC2W1(2,2),V2DW1(2,2)
      DOUBLE COMPLEX    UCAW2(2,2),VBDW2(2,2),UC1W2(2,2),V1DW2(2,2)
      DOUBLE COMPLEX    UCAWX1(2,2),VBDWX1(2,2),UC2WX1(2,2),V2DWX1(2,2)
      DOUBLE COMPLEX    UCAWX2(2,2),VBDWX2(2,2),UC1WX2(2,2),V1DWX2(2,2)
      DOUBLE COMPLEX    UCAWXB(2,2),VBDWXA(2,2)
      DOUBLE COMPLEX    UCAWXD(2,2),VBDWXC(2,2)
      DOUBLE COMPLEX    UC2WXB(2,2),V2DWXA(2,2),UC2WXD(2,2),V2DWXC(2,2)
      DOUBLE COMPLEX    UC1WXB(2,2),V1DWXA(2,2),UC1WXD(2,2),V1DWXC(2,2)
      DOUBLE COMPLEX    Su1,Su2,sProd,eps1(4),eps2(4),eps1D2
      INTEGER           j,j1,j2,j3,j4,k,Sig,l
      DOUBLE PRECISION  prA1,prB1,prA2,prB2,prA12,prB12
      DOUBLE PRECISION  eps1pA,eps2pA,eps1pB,eps2pB,eps1pC,eps2pC,eps1pD,eps2pD,eps1p2,eps2p1
      DOUBLE PRECISION  BornV_GetCharge,ChaIni
      DOUBLE PRECISION  s0,t0,u0,CosThetD
      DOUBLE PRECISION  s,t,u,s1,t1,u1,s2,t2,u2,s12,t12,u12
      DOUBLE COMPLEX    PropW0,WVPi0,PropW,WVPi,PropW1,WVPi1,PropW2,WVPi2,PropW12,WVPi12,CPF0,CPF,CPF1,CPF2,CPF12
      DOUBLE COMPLEX    Fprop1,Fprop2,EpsDot1(2),EpsDot12(2),EpsDot2(2),EpsDot21(2)
      DOUBLE COMPLEX    gI
      DOUBLE COMPLEX    GPS_Sof1,GPS_Sof1b,GPS_Sof1x,GPS_Sof1bx
      DOUBLE COMPLEX    sA(2,2),sB(2,2)
      INTEGER           Y_IR,Y_IR1,N_IR
*------------------------------------------------------------------------------------------
      IF (ABS(KFf).NE.12) RETURN
      RETURN
*------------------------------------------------------------------------------------------
      Y_IR=1                   ! YES, IR included
      Y_IR=0                   ! No,  IR not included
      Y_IR1=1                  ! YES, IR times beta 0 included
      N_IR=1-Y_IR1
*--------------------
      ChaIni =  BornV_GetCharge( KFi)
      gI = DCMPLX(ChaIni*m_e_QED)
        CALL KinLib_ThetaD(PX,pAo,pBo,pCo,pDo,s0,CosThetD)
        t0 = -s0*(1d0-CosThetD)/2d0
        u0 = -s0*(1d0+CosThetD)/2d0
C--------
C reduction procedure ....
        CALL GPS_KinExt2(pAo,mA,pBo,mB,pCo,mC,pDo,mD,ph1o,ph2o,mph,ph1,ph2,pA,pB,pC,pD)
C--------
C transfers
        s=(pA(4)+pB(4))**2-(pA(3)+pB(3))**2-(pA(2)+pB(2))**2-(pA(1)+pB(1))**2
        t=(pC(4)-pA(4))**2-(pC(3)-pA(3))**2-(pC(2)-pA(2))**2-(pC(1)-pA(1))**2
        u=(pD(4)-pA(4))**2-(pD(3)-pA(3))**2-(pD(2)-pA(2))**2-(pD(1)-pA(1))**2

        s1=(pA(4)+pB(4))**2-(pA(3)+pB(3))**2-(pA(2)+pB(2))**2-(pA(1)+pB(1))**2
        t1=(pC(4)-pA(4)+ph1(4))**2-(pC(3)-pA(3)+ph1(3))**2-(pC(2)-pA(2)+ph1(2))**2-(pC(1)-pA(1)+ph1(1))**2
        u1=(pD(4)-pA(4)+ph1(4))**2-(pD(3)-pA(3)+ph1(3))**2-(pD(2)-pA(2)+ph1(2))**2-(pD(1)-pA(1)+ph1(1))**2


        s2=(pA(4)+pB(4))**2-(pA(3)+pB(3))**2-(pA(2)+pB(2))**2-(pA(1)+pB(1))**2
        t2=(pC(4)-pA(4)+ph2(4))**2-(pC(3)-pA(3)+ph2(3))**2-(pC(2)-pA(2)+ph2(2))**2-(pC(1)-pA(1)+ph2(1))**2
        u2=(pD(4)-pA(4)+ph2(4))**2-(pD(3)-pA(3)+ph2(3))**2-(pD(2)-pA(2)+ph2(2))**2-(pD(1)-pA(1)+ph2(1))**2


        s12=(pA(4)+pB(4))**2-(pA(3)+pB(3))**2-(pA(2)+pB(2))**2-(pA(1)+pB(1))**2
        t12=(pC(4)-pA(4)+ph1(4)+ph2(4))**2-(pC(3)-pA(3)+ph1(3)+ph2(3))**2
     $     -(pC(2)-pA(2)+ph1(2)+ph2(2))**2-(pC(1)-pA(1)+ph1(1)+ph2(1))**2
        u12=(pD(4)-pA(4)+ph1(4)+ph2(4))**2-(pD(3)-pA(3)+ph1(3)+ph2(3))**2
     $     -(pD(2)-pA(2)+ph1(2)+ph2(2))**2-(pD(1)-pA(1)+ph1(1)+ph2(1))**2

C all W propagators ... some simplifications on vac pol set
          CALL GPS_EWFFactW(KFi,KFf,s0,t0,PropW0,WVPi0)
          CALL GPS_EWFFactW(KFi,KFf,s,t,PropW,WVPi)
          CALL GPS_EWFFactW(KFi,KFf,s1,t1,PropW1,WVPi1)
          CALL GPS_EWFFactW(KFi,KFf,s2,t2,PropW2,WVPi2)
          CALL GPS_EWFFactW(KFi,KFf,s12,t12,PropW12,WVPi12)
          WVPi  =1D0            !!!  WVPi0
          WVPi1 =1D0            !!!  WVPi0
          WVPi2 =1D0            !!!  WVPi0
          WVPi12=1D0            !!!  WVPi0
          CPF0 =PropW0 *1D0     !!!   *WVPi0
          CPF  =PropW  *WVPi
          CPF1 =PropW1 *WVPi1
          CPF2 =PropW2 *WVPi2
          CPF12=PropW12*WVPi12

!      RETURN
      IF( m_KeyArb  .EQ.  0 ) THEN
         sA(1,1)  = -gI*GPS_Sof1( 1,ph1,pA)
         sA(2,1)  = -gI*GPS_Sof1( 1,ph2,pA)
         sB(1,1)  =  gI*GPS_Sof1( 1,ph1,pB)
         sB(2,1)  =  gI*GPS_Sof1( 1,ph2,pB)
         EpsDot1(1) =0.5D0*(-GPS_Sof1x( 1,ph1,pA)+GPS_Sof1x( 1,ph1,pC)  
     $                      +GPS_Sof1x( 1,ph1,pB)-GPS_Sof1x( 1,ph1,pD)-GPS_Sof1x( 1,ph1,ph2))
         EpsDot12(1)=0.5D0*(-GPS_Sof1x( 1,ph1,pA)+GPS_Sof1x( 1,ph1,pC)-GPS_Sof1x( 1,ph1,ph2) 
     $                      +GPS_Sof1x( 1,ph1,pB)-GPS_Sof1x( 1,ph1,pD))
         EpsDot2(1) =0.5D0*(-GPS_Sof1x( 1,ph2,pA)+GPS_Sof1x( 1,ph2,pC)  
     $                      +GPS_Sof1x( 1,ph2,pB)-GPS_Sof1x( 1,ph2,pD)-GPS_Sof1x( 1,ph2,ph1))
         EpsDot21(1)=0.5D0*(-GPS_Sof1x( 1,ph2,pA)+GPS_Sof1x( 1,ph2,pC)-GPS_Sof1x( 1,ph2,ph1) 
     $                      +GPS_Sof1x( 1,ph2,pB)-GPS_Sof1x( 1,ph2,pD))
      ELSE
         sA(1,1)  = -gI*GPS_Sof1b( 1,ph1,pA,mA)
         sA(2,1)  = -gI*GPS_Sof1b( 1,ph2,pA,mA)
         sB(1,1)  =  gI*GPS_Sof1b( 1,ph1,pB,mB)
         sB(2,1)  =  gI*GPS_Sof1b( 1,ph2,pB,mB)
         EpsDot1(1) =0.5D0*(-GPS_Sof1bx( 1,ph1,pA,mA)+GPS_Sof1bx( 1,ph1,pC,mC)  
     $                      +GPS_Sof1bx( 1,ph1,pB,mB)-GPS_Sof1bx( 1,ph1,pD,mD)-GPS_Sof1bx( 1,ph1,ph2,mph))
         EpsDot12(1)=0.5D0*(-GPS_Sof1bx( 1,ph1,pA,mA)+GPS_Sof1bx( 1,ph1,pC,mC)-GPS_Sof1bx( 1,ph1,ph2,mph) 
     $                      +GPS_Sof1bx( 1,ph1,pB,mB)-GPS_Sof1bx( 1,ph1,pD,mD))
         EpsDot2(1) =0.5D0*(-GPS_Sof1bx( 1,ph2,pA,mA)+GPS_Sof1bx( 1,ph2,pC,mC)  
     $                      +GPS_Sof1bx( 1,ph2,pB,mB)-GPS_Sof1bx( 1,ph2,pD,mD)-GPS_Sof1bx( 1,ph2,ph1,mph))
         EpsDot21(1)=0.5D0*(-GPS_Sof1bx( 1,ph2,pA,mA)+GPS_Sof1bx( 1,ph2,pC,mC)-GPS_Sof1bx( 1,ph2,ph1,mph) 
     $                      +GPS_Sof1bx( 1,ph2,pB,mB)-GPS_Sof1bx( 1,ph2,pD,mD))

      ENDIF
      sA(1,2) = -DCONJG(sA(1,1))
      sA(2,2) = -DCONJG(sA(2,1))
      sB(1,2) = -DCONJG(sB(1,1))
      sB(2,2) = -DCONJG(sB(2,1))
      EpsDot1 (2) = -DCONJG(EpsDot1 (1))
      EpsDot12(2) = -DCONJG(EpsDot12(1))
      EpsDot2 (2) = -DCONJG(EpsDot2 (1))
      EpsDot21(2) = -DCONJG(EpsDot21(1))

C photon polarization 4-vectors calculated explicitelly ...
      Sig = 3-2*Hel1
      CALL GPS_Make_eps(ph1,Sig,eps1)
      Sig = 3-2*Hel2
      CALL GPS_Make_eps(ph2,Sig,eps2)
      eps1D2=eps1(4)*eps2(4)-eps1(3)*eps2(3)-eps1(2)*eps2(2)-eps1(1)*eps2(1)
      eps1pA=eps1(4)*pA(4)-eps1(3)*pA(3)-eps1(2)*pA(2)-eps1(1)*pA(1)
      eps2pA=eps2(4)*pA(4)-eps2(3)*pA(3)-eps2(2)*pA(2)-eps2(1)*pA(1)
      eps1pB=eps1(4)*pB(4)-eps1(3)*pB(3)-eps1(2)*pB(2)-eps1(1)*pB(1)
      eps2pB=eps2(4)*pB(4)-eps2(3)*pB(3)-eps2(2)*pB(2)-eps2(1)*pB(1)
      eps1pC=eps1(4)*pC(4)-eps1(3)*pC(3)-eps1(2)*pC(2)-eps1(1)*pC(1)
      eps2pC=eps2(4)*pC(4)-eps2(3)*pC(3)-eps2(2)*pC(2)-eps2(1)*pC(1)
      eps1pD=eps1(4)*pD(4)-eps1(3)*pD(3)-eps1(2)*pD(2)-eps1(1)*pD(1)
      eps2pD=eps2(4)*pD(4)-eps2(3)*pD(3)-eps2(2)*pD(2)-eps2(1)*pD(1)

      eps1p2=eps1(4)*ph2(4)-eps1(3)*ph2(3)-eps1(2)*ph2(2)-eps1(1)*ph2(1)
      eps2p1=eps2(4)*ph1(4)-eps2(3)*ph1(3)-eps2(2)*ph1(2)-eps2(1)*ph1(1)

* Calculate Born spin amplitudes
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, pA,mA,    pB,-mB,      pC,MC,   pD,-mD,   BornABCD) ! Standard
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, ph1,mph,  pB,-mB,      pC,mC,   pD,-mD,   Born1BCD) ! A->1
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, pA,mA,    ph1,-mph,    pC,mC,   pD,-mD,   BornA1CD) ! B->1
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, ph2,mph,  pB,-mB,      pC,mC,   pD,-mD,   Born2BCD) ! A->2
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, pA,mA,    ph2,-mph,    pC,mC,   pD,-mD,   BornA2CD) ! B->2
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, ph1,mph,  ph2,-mph,    pC,mC,   pD,-mD,   Born12CD) ! A->1,B->2
      CALL GPS_BornWPlus(1,1,KFi,KFf,s0,t0,u0, ph2,mph,  ph1,-mph,    pC,mC,   pD,-mD,   Born21CD) ! A->2,B->1
* Fermion propagarotors ini1
      prA1= 1d0/(pA(4)*ph1(4)-pA(3)*ph1(3)-pA(2)*ph1(2)-pA(1)*ph1(1))/2d0
      prB1=-1d0/(pB(4)*ph1(4)-pB(3)*ph1(3)-pB(2)*ph1(2)-pB(1)*ph1(1))/2d0
* Fermion propagarotors ini2
      prA2= 1d0/(pA(4)*ph2(4)-pA(3)*ph2(3)-pA(2)*ph2(2)-pA(1)*ph2(1))/2d0
      prB2=-1d0/(pB(4)*ph2(4)-pB(3)*ph2(3)-pB(2)*ph2(2)-pB(1)*ph2(1))/2d0
* DOUBLE propagators
      prA12= 1d0/( pA(4)*ph1(4)- pA(3)*ph1(3)- pA(2)*ph1(2)- pA(1)*ph1(1)
     $           + pA(4)*ph2(4)- pA(3)*ph2(3)- pA(2)*ph2(2)- pA(1)*ph2(1)
     $           -ph1(4)*ph2(4)+ph1(3)*ph2(3)+ph1(2)*ph2(2)+ph1(1)*ph2(1)
     $           )/2d0
      prB12=-1d0/( pB(4)*ph1(4)- pB(3)*ph1(3)- pB(2)*ph1(2)- pB(1)*ph1(1)
     $            +pB(4)*ph2(4)- pB(3)*ph2(3)- pB(2)*ph2(2)- pB(1)*ph2(1)
     $           -ph1(4)*ph2(4)+ph1(3)*ph2(3)+ph1(2)*ph2(2)+ph1(1)*ph2(1)
     $           )/2d0
      Fprop1=(1d0/prA1+1d0/prA2)*prA12*CPF12/CPF0-1d0
      Fprop2=(1d0/prB1+1d0/prB2)*prB12*CPF/CPF0-1d0
      Sig = 3-2*Hel1
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrU( gI, ph1,Sig,  ph1,mph, pA,mA,     U11a) ! <1|[1]|a>
         CALL GPS_MatrV( gI, ph1,Sig,  pB,mB,   ph1,mph,   Vb11) ! <b|{1}|1>
* falSe second
         CALL GPS_MatrU( gI, ph1,Sig,  ph2,mph, pA,mA,     U21a) ! <2|[1]|a>
         CALL GPS_MatrV( gI, ph1,Sig,  pB,mB,   ph2,mph,   Vb12) ! <b|{1}|2>
* reverse order
         CALL GPS_MatrU( gI, ph1,Sig,  pA,mA,   ph2,mph,   Ua12) ! <a|[1]|2>
         CALL GPS_MatrV( gI, ph1,Sig,  ph2,mph, pB,mB,     V21b) ! <2|{1}|b>
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrU( gI, ph1,Sig,  ph1,mph, ph2,mph,   U112) ! <1|[1]|2>
         CALL GPS_MatrV( gI, ph1,Sig,  ph2,mph, ph1,mph,   V211) ! <2|{1}|1>
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrU( gI, ph1,Sig,  ph2,mph, ph2,mph,   U212) ! <2|[1]|2>
         CALL GPS_MatrV( gI, ph1,Sig,  ph2,mph, ph2,mph,   V212) ! <2|{1}|2>
      ELSE
         CALL GPS_MatrUb(gI, ph1,Sig,  ph1,mph, pA,mA,     U11a)
         CALL GPS_MatrVb(gI, ph1,Sig,  pB,mB,   ph1,mph,   Vb11)
* falSe second
         CALL GPS_MatrUb(gI, ph1,Sig,  ph2,mph, pA,mA,     U21a)
         CALL GPS_MatrVb(gI, ph1,Sig,  pB,mB,   ph2,mph,   Vb12)
* reverse order
         CALL GPS_MatrUb(gI, ph1,Sig,  pA,mA,   ph2,mph,   Ua12)
         CALL GPS_MatrVb(gI, ph1,Sig,  ph2,mph, pB,mB,     V21b)
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrUb(gI, ph1,Sig,  ph1,mph, ph2,mph,   U112)
         CALL GPS_MatrVb(gI, ph1,Sig,  ph2,mph, ph1,mph,   V211)
* for the case when there was ph2 first xk-xk term
         CALL GPS_MatrUb(gI, ph1,Sig,  ph2,mph, ph2,mph,   U212)
         CALL GPS_MatrVb(gI, ph1,Sig,  ph2,mph, ph2,mph,   V212)
      ENDIF
      Sig = 3-2*Hel2
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrU( gI, ph2,Sig,  ph2,mph,  pA,mA,    U22a) ! <2|[2]|a>
         CALL GPS_MatrV( gI, ph2,Sig,  pB,mB,   ph2,mph,   Vb22) ! <b|{2}|2>
* falSe second
         CALL GPS_MatrU( gI, ph2,Sig,  ph1,mph,  pA,mA,    U12a) ! <1|[2]|a>
         CALL GPS_MatrV( gI, ph2,Sig,  pB,mB,   ph1,mph,   Vb21) ! <b|{2}|1>
* reverse order
         CALL GPS_MatrU( gI, ph2,Sig,  pA,mA,   ph1,mph,   Ua21) ! <a|[2]|1>
         CALL GPS_MatrV( gI, ph2,Sig,  ph1,mph, pB,mB,     V12b) ! <1|{2}|b>
* for the case when there was ph1 first xk-xk term
         CALL GPS_MatrU( gI, ph2,Sig,  ph2,mph, ph1,mph,   U221) ! <2|[2]|1>
         CALL GPS_MatrV( gI, ph2,Sig,  ph1,mph, ph2,mph,   V122) ! <1|{2}|2>
c for the case when there was ph1 first xk-xk term
         CALL GPS_MatrU( gI, ph2,Sig,  ph1,mph, ph1,mph,   U121) ! <1|[2]|1>
         CALL GPS_MatrV( gI, ph2,Sig,  ph1,mph, ph1,mph,   V121) ! <1|{2}|1>
      ELSE
         CALL GPS_MatrUb(gI, ph2,Sig,  ph2,mph,  pA,mA,    U22a)
         CALL GPS_MatrVb(gI, ph2,Sig,  pB,mB,   ph2,mph,   Vb22)
c falSe second
         CALL GPS_MatrUb(gI, ph2,Sig,  ph1,mph,  pA,mA,    U12a)
         CALL GPS_MatrVb(gI, ph2,Sig,  pB,mB,   ph1,mph,   Vb21)
c reverse order
         CALL GPS_MatrUb(gI, ph2,Sig,  pA,mA,   ph1,mph,   Ua21)
         CALL GPS_MatrVb(gI, ph2,Sig,  ph1,mph, pB,mB,     V12b)
c for the case when there was ph1 first xk-xk term
         CALL GPS_MatrUb(gI, ph2,Sig,  ph2,mph, ph1,mph,   U221)
         CALL GPS_MatrVb(gI, ph2,Sig,  ph1,mph, ph2,mph,   V122)
c for the case when there was ph1 first xk-xk term
         CALL GPS_MatrUb(gI, ph2,Sig,  ph1,mph, ph1,mph,   U121)
         CALL GPS_MatrVb(gI, ph2,Sig,  ph1,mph, ph1,mph,   V121)
      ENDIF
C fermion lines including v-a coupling contracted with photon polarization ... 
         Cnor=1D0
      IF( m_KeyArb .EQ. 0 ) THEN
         Sig = 3-2*Hel1
         CALL GPS_MakeUW(Cnor,ph1,Sig, pC,  mC,   pA,  mA,     UCAW1) ! v-a inside
         CALL GPS_MakeVW(Cnor,ph1,Sig, pB,  mB,   pD,  mD,     VBDW1) ! v-a inside
         CALL GPS_MakeUW(Cnor,ph1,Sig, pC,  mC,   ph2, mph,    UC2W1) ! v-a inside
         CALL GPS_MakeVW(Cnor,ph1,Sig, ph2,mph,   pD,  mD,     V2DW1) ! v-a inside
         Sig = 3-2*Hel2
         CALL GPS_MakeUW(Cnor,ph2,Sig, pC,  mC,   pA,   mA,    UCAW2) ! v-a inside
         CALL GPS_MakeVW(Cnor,ph2,Sig, pB,  mB,   pD,   mD,    VBDW2) ! v-a inside
         CALL GPS_MakeUW(Cnor,ph2,Sig, pC,  mC,   ph1, mph,    UC1W2) ! v-a inside
         CALL GPS_MakeVW(Cnor,ph2,Sig, ph1,mph,   pD,   mD,    V1DW2) ! v-a inside
      ENDIF
C fermion lines including v-a coupling contracted with incoming momentum ...

         CALL GPS_MakeUX(Cnor,ph1,mph, pC,mC,   pA,mA,    UCAWX1) ! v-a inside
         CALL GPS_MakeVX(Cnor,ph1,mph, pB,mB,   pD,mD,    VBDWX1) ! v-a inside
         CALL GPS_MakeUX(Cnor,ph2,mph, pC,mC,   pA,mA,    UCAWX2) ! v-a inside
         CALL GPS_MakeVX(Cnor,ph2,mph, pB,mB,   pD,mD,    VBDWX2) ! v-a inside

 
         CALL GPS_MakeUX(Cnor,pB,mB, pC,mC,   pA,mA,    UCAWXB) ! v-a inside
         CALL GPS_MakeVX(Cnor,pA,mA, pB,mB,   pD,mD,    VBDWXA) ! v-a inside
         CALL GPS_MakeUX(Cnor,pD,mD, pC,mC,   pA,mA,    UCAWXD) ! v-a inside
         CALL GPS_MakeVX(Cnor,pC,mC, pB,mB,   pD,mD,    VBDWXC) ! v-a inside

         CALL GPS_MakeUX(Cnor,ph1,mph, pC, mC,   ph2,mph,    UC2WX1) ! v-a inside
         CALL GPS_MakeVX(Cnor,ph1,mph, ph2,mph,  pD, mD,     V2DWX1) ! v-a inside
         CALL GPS_MakeUX(Cnor,ph2,mph, pC, mC,   ph1,mph,    UC1WX2) ! v-a inside
         CALL GPS_MakeVX(Cnor,ph2,mph, ph1,mph,  pD, mD,     V1DWX2) ! v-a inside


         CALL GPS_MakeUX(Cnor,pB,mB, pC, mC,   ph2,mph,    UC2WXB) ! v-a inside
         CALL GPS_MakeVX(Cnor,pA,mA, ph2,mph,  pD, mD,     V2DWXA) ! v-a inside
         CALL GPS_MakeUX(Cnor,pB,mB, pC, mC,   ph1,mph,    UC1WXB) ! v-a inside
         CALL GPS_MakeVX(Cnor,pA,mA, ph1,mph,  pD, mD,     V1DWXA) ! v-a inside


         CALL GPS_MakeUX(Cnor,pD,mD, pC, mC,   ph2,mph,    UC2WXD) ! v-a inside
         CALL GPS_MakeVX(Cnor,pC,mC, ph2,mph,  pD, mD,     V2DWXC) ! v-a inside
         CALL GPS_MakeUX(Cnor,pD,mD, pC, mC,   ph1,mph,    UC1WXD) ! v-a inside
         CALL GPS_MakeVX(Cnor,pC,mC, ph1,mph,  pD, mD,     V1DWXC) ! v-a inside

      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Su1 = DCMPLX(0d0,0d0)
                  DO j=1,2
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  1               2                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-2    |      a                  //
*        //      v  ------<----O--------<---------U--------<------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+Born1BCD(j,j2,j3,j4) *U11a(j,j1)*(prA12-prA1*N_IR)*sA(2,Hel2)*CPF12/CPF0 !<b|X|1[1]a(2)|a>
                     Su1=Su1+Born2BCD(j,j2,j3,j4) *U21a(j,j1)*   prA12         *sA(2,Hel2)*CPF12/CPF0 !<b|X|2[1]a(2)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  2               1                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-1    |      a                  //
*        //      v  ------<----O--------<---------U-------<-------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+Born2BCD(j,j2,j3,j4) *U22a(j,j1)*(prA12-prA2*N_IR)*sA(1,Hel1) *CPF12/CPF0!<b|X|2[2]a(1)|a>
                     Su1=Su1+Born1BCD(j,j2,j3,j4) *U12a(j,j1)* prA12           *sA(1,Hel1) *CPF12/CPF0!<b|X|1[2]a(1)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  1               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+2       |    -b+m+1+2   |      a                  //
*        //      v  ------<----S--------<---------V--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1+( sB(2,Hel2)*(-prB1*N_IR+prB12))*Vb11(j2,j)*BornA1CD(j1,j,j3,j4)*CPF/CPF0!<b|(2)b[1]1|X|a>
                     Su1=Su1+( sB(2,Hel2))           *prB12  *Vb12(j2,j)*BornA2CD(j1,j,j3,j4)*CPF/CPF0!<b|(2)b[1]2|X|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  2               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+1       |    -b+m+1+2   |      a                  //
*        //      v  ------<----S--------<---------V--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +(sB(1,Hel1)*(-prB2*N_IR+prB12))*Vb22(j2,j)*BornA2CD(j1,j,j3,j4)*CPF/CPF0!<b|(1)b[2]2|X|a>
                     Su1=Su1 +(sB(1,Hel1))           *prB12  *Vb21(j2,j)*BornA1CD(j1,j,j3,j4)*CPF/CPF0!<b|(1)b[2]1|X|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  |               1                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+2       |     a+m-1     |      a                  //
*        //      v  ------<----S--------<---------O--------<------U------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +sB(2,Hel2)    *Born1BCD(j,j2,j3,j4) *prA1*U11a(j,j1) *CPF1/CPF0*Y_IR1 !<b|(2)2|X|1[1]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  |               2                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+1       |     a+m-2     |      a                  //
*        //      v  ------<----S--------<---------O--------<------U------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +sB(1,Hel1)    *Born2BCD(j,j2,j3,j4) *prA2*U22a(j,j1) *CPF2/CPF0*Y_IR1 !<b|(1)1|X|2[2]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  |               2                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+1       |     a+m-2     |      a                  //
*        //      v  ------<----V--------<---------O--------<------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Vb11(j2,j)*prB1 *BornA1CD(j1,j,j3,j4) *sA(2,Hel2) *CPF2/CPF0   *Y_IR1 !<b|[1]1|X|a[2]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  |               1                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+2       |     a+m-1     |      a                  //
*        //      v  ------<----V--------<---------O--------<------S------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                      Su1=Su1 +Vb22(j2,j)*prB2 *BornA2CD(j1,j,j3,j4) *sA(1,Hel1)*CPF1/CPF0      *Y_IR1 !<b|[2]2|X|a(1)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                                       E--2                                      //
*        //                                       |               1                         //
*        //                                       |X              |                         //
*        //      _       -b                       |     a+m-1     |      a                  //
*        //      v  ------<-------------<---------O--------<------U------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +EpsDot21(Hel2)  *Born1BCD(j,j2,j3,j4) *prA1*U11a(j,j1)  *CPF1*CPF12/CPF0  !<b|(2)2|X|1[1]|a>
     $                       -DCMPLX(m_e_QED)*prA1*U11a(j,j1)*CPF1*CPF12*WVpi0
     $                       *( UC1W2(j3,j )*(VBDWX2(j2,j4)+VBDWXA(j2,j4)-VBDWX1(j2,j4)-VBDWXC(j2,j4) )
     $                         -VBDW2(j2,j4)*(UC1WX2(j3,j ) +UC1WXB(j3,j1)-UC1WXD(j3,j1) )           )
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                                       E--1                                      //
*        //                                       |               2                         //
*        //                                       |X              |                         //
*        //      _       -b                       |     a+m-2     |      a                  //
*        //      v  ------<-------------<---------O--------<------U------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +EpsDot12(Hel1)  *Born2BCD(j,j2,j3,j4) *prA2*U22a(j,j1)  *CPF2*CPF12/CPF0 !<b|(1)1|X|2[2]|a>
     $                       -DCMPLX(m_e_QED)*prA2*U22a(j,j1)*CPF2*CPF12*WVpi0
     $                       *( UC2W1(j3,j )*(VBDWX1(j2,j4)+VBDWXA(j2,j4)-VBDWX2(j2,j4)-VBDWXC(j2,j4) )
     $                         -VBDW1(j2,j4)*(UC2WX1(j3,j )+UC2WXB(j3,j1)-UC2WXD(j3,j1) ) )
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                                       E--2                                      //
*        //                    1                  |                                         //
*        //                    |                  |X                                        //
*        //      _       -b    |     -b+m+1       |                      a                  //
*        //      v  ------<----V--------<---------O--------<-------------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Vb11(j2,j)*prB1 *BornA1CD(j1,j,j3,j4) *EpsDot2(Hel2) *CPF *CPF2/CPF0    !<b|[1]1|X|a[2]|a>
     $                       -DCMPLX(m_e_QED)*Vb11(j2,j)*prB1*CPF*CPF2*WVpi0 
     $                       *( UCAW2(j3,j1)*(V1DWX2(j ,j4)+V1DWXA(j2,j4)-V1DWXC(j2,j4) )
     $                         -V1DW2(j ,j4)*(UCAWX2(j3,j1) +UCAWXB(j3,j1)-UCAWX1(j3,j1)-UCAWXD(j3,j1)) )
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                                       E--1                                      //
*        //                    2                  |                                         //
*        //                    |                  |X                                        //
*        //      _       -b    |     -b+m+2       |                      a                  //
*        //      v  ------<----V--------<---------O--------<-------------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Vb22(j2,j)*prB2 *BornA2CD(j1,j,j3,j4) *EpsDot1(Hel1)*CPF *CPF1/CPF0  !<b|[2]2|X|a(1)|a>
     $                       -DCMPLX(m_e_QED)*Vb22(j2,j)*prB2*CPF*CPF1*WVpi0 
     $                       *( UCAW1(j3,j1)*(V2DWX1(j ,j4)+V2DWXA(j ,j4)-V2DWXC(j ,j4) )
     $                         -V2DW1(j ,j4)*(UCAWX1(j3,j1)+UCAWXB(j3,j1)-UCAWX2(j3,j1)-UCAWXD(j3,j1) ) )
                  ENDDO
                  Su2 = DCMPLX(0d0,0d0)
                  DO j=1,2
                     DO l=1,2
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  |               1                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+2       |     a+m-1     |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2  +Vb22(j2,l)*prB2  *Born12CD(j,l,j3,j4 )  *U11a(j,j1)*prA1*CPF1/CPF0 ! <b|[2]2|X|1[1]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  |               2                         //
*        //                    |                  |X              |                         //
*        //      _       -b    |     -b+m+1       |     a+m-2     |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2  +Vb11(j2,l)*prB1  *Born21CD(j,l,j3,j4 )  *U22a(j,j1)*prA2*CPF2/CPF0 ! <b|[1]1|X|2[2]|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  2               1                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-1    |      a                  //
*        //      v  ------<----O--------<---------O--------<------*------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Born1BCD(j,j2,j3,j4) *U121(j,l)*prA12 *U11a(l,j1)*prA1*CPF12/CPF0  ! <b|X|1[2]1(1)|a>
                        Su2=Su2 +Born2BCD(j,j2,j3,j4) *U221(j,l)*prA12 *U11a(l,j1)*prA1*CPF12/CPF0  ! <b|X|2[2]1(1)|a>
                        Su2=Su2 -BornABCD(j,j2,j3,j4) *Ua21(j,l)*prA12 *U11a(l,j1)*prA1*CPF12/CPF0  ! <b|X|a[2]1(1)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    |                  1               2                         //
*        //                    |X                 |               |                         //
*        //      _       -b    |      b+m-1-2     |      a+m-2    |      a                  //
*        //      v  ------<----O--------<---------O--------<------*------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Born2BCD(j,j2,j3,j4) *U212(j,l)*prA12  *U22a(l,j1)*prA2*CPF12/CPF0 ! <b|X|2[1]2(2)|a>
                        Su2=Su2 +Born1BCD(j,j2,j3,j4) *U112(j,l)*prA12  *U22a(l,j1)*prA2*CPF12/CPF0 ! <b|X|1[1]2(2)|a>
                        Su2=Su2 -BornABCD(j,j2,j3,j4) *Ua12(j,l)*prA12  *U22a(l,j1)*prA2*CPF12/CPF0 ! <b|X|a[1]2(2)|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    1                  2               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+1       |    -b+m+1+2   |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Vb11(j2,l)*prB1 *V121(l,j)*prB12 *BornA1CD(j1,j,j3,j4)*CPF/CPF0 ! <b|(1)1[2]1|X|a>
                        Su2=Su2 +Vb11(j2,l)*prB1 *V122(l,j)*prB12 *BornA2CD(j1,j,j3,j4)*CPF/CPF0 ! <b|(1)1[2]2|X|a>
                        Su2=Su2 -Vb11(j2,l)*prB1 *V12b(l,j)*prB12 *BornABCD(j1,j,j3,j4)*CPF/CPF0 ! <b|(1)1[2]b|X|a>
*        /////////////////////////////////////////////////////////////////////////////////////
*        //                    2                  1               |                         //
*        //                    |                  |               |X                        //
*        //      _       -b    |     -b+m+2       |    -b+m+1+2   |      a                  //
*        //      v  ------<----*--------<---------O--------<------O------<----- u           //
*        /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Vb22(j2,l)*prB2 *V212(l,j)*prB12  *BornA2CD(j1,j,j3,j4)*CPF/CPF0 ! <b|(2)2[1]2|X|a>
                        Su2=Su2 +Vb22(j2,l)*prB2 *V211(l,j)*prB12  *BornA1CD(j1,j,j3,j4)*CPF/CPF0 ! <b|(2)2[1]1|X|a>
                        Su2=Su2 -Vb22(j2,l)*prB2 *V21b(l,j)*prB12  *BornABCD(j1,j,j3,j4)*CPF/CPF0 ! <b|(2)2[2]b|X|a>
                     ENDDO
                  ENDDO
                  sProd = ( sA(1,Hel1)+sB(1,Hel1))*( sA(2,Hel2)+sB(2,Hel2))
                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4)
     $                 +CNorm*( Su1+Su2 )
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*( sA(1,Hel1)*sA(2,Hel2)*Fprop1  !
     $                 +                             +sA(1,Hel1)*sB(2,Hel2)*(CPF1/CPF0-1d0)
     $                 +                             +sB(1,Hel1)*sA(2,Hel2)*(CPF2/CPF0-1d0)
     $                 +                             +sB(1,Hel1)*sB(2,Hel2)*Fprop2
     $                 +                             +sA(1,Hel1)*CPF1*CPF12/CPF0*EpsDot21(Hel2) ! terms due to diag WWgamma
     $                 +                             +sA(2,Hel2)*CPF2*CPF12/CPF0*EpsDot12(Hel1)
     $                 +                             +sB(1,Hel1)*CPF*CPF1/CPF0*EpsDot2(Hel2)
     $                 +                             +sB(2,Hel2)*CPF*CPF2/CPF0*EpsDot1(Hel1)
     $                 +                             +CPF*CPF1*CPF12/CPF0*EpsDot1(Hel1)*EpsDot21(Hel2)
     $                 +                             +CPF*CPF2*CPF12/CPF0*EpsDot2(Hel2)*EpsDot12(Hel1)
     $                                                                                      ) !
     $                 +CNorm*BornABCD(j1,j2,j3,j4)* sProd                   *Y_IR  !

                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4)   ! terms due to remaining single WWgam coupling
     $                 +UCAW1(j3,j1)*( VBDWX1(j2,j4)+VBDWXA(j2,j4)-VBDWX2(j2,j4)-VBDWXC(j2,j4) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *CPF2*CPF12*WVpi0*sA(2,Hel2)
     $                 +UCAW1(j3,j1)*( VBDWX1(j2,j4)+VBDWXA(j2,j4)-VBDWXC(j2,j4) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *sB(2,Hel2)*CPF*CPF1*WVpi0
     $                  -VBDW1(j2,j4)*( UCAWX1(j3,j1)+UCAWXB(j3,j1)-UCAWXD(j3,j1) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *CPF2*CPF12*WVpi0*sA(2,Hel2)
     $                  -VBDW1(j2,j4)*( UCAWX1(j3,j1)+UCAWXB(j3,j1)-UCAWX2(j3,j1)-UCAWXD(j3,j1) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *sB(2,Hel2)*CPF*CPF1*WVpi0
C
     $                 +UCAW2(j3,j1)*( VBDWX2(j2,j4)+VBDWXA(j2,j4)-VBDWX1(j2,j4)-VBDWXC(j2,j4) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *CPF1*CPF12*WVpi0*sA(1,Hel1)
     $                 +UCAW2(j3,j1)*( VBDWX2(j2,j4)+VBDWXA(j2,j4)-VBDWXC(j2,j4) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *sB(1,Hel1)*CPF*CPF2*WVpi0
     $                 -VBDW2(j2,j4)*( UCAWX2(j3,j1)+UCAWXB(j3,j1)-UCAWXD(j3,j1) )
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *CPF1*CPF12*WVpi0*sA(1,Hel1)
     $                 -VBDW2(j2,j4)*( UCAWX2(j3,j1)+UCAWXB(j3,j1)-UCAWX1(j3,j1)-UCAWXD(j3,j1) ) !
     $                 *CNorm*(-1D0) *DCMPLX(m_e_QED) *sB(1,Hel1)*CPF*CPF2*WVpi0
! terms due to ph1 ph2 attached to W
                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4) + DCMPLX(m_e_QED**2)*CPF*CPF1*CPF12*WVpi0*( 
     $                                  BornABCD(j1,j2,j3,j4)*(2*eps1pC-2*eps1pA)*(2*eps2pB-2*eps2pD)     ! 1
     $                                 +(2*eps1pC-2*eps1pA)  *VBDW2 (j2,j4) * UCAWX1(j3,j1)               ! 2
     $                                 +(2*eps1pC-2*eps1pA)*2*VBDWX2(j2,j4) * UCAW2 (j3,j1)               ! 3
     $                                 - 2*UCAWX1(j3,j1)*(2*eps2pB-2*eps2pD)* VBDW1 (j2,j4)               ! 4
     $                                 - 2*UCAWX1(j3,j1)*(eps1pC-eps1pA)    * VBDW2 (j2,j4)               ! 5
     $                                 + eps1D2*(-2*UCAWX1(j3,j1))*        2* VBDWX2(j2,j4)               ! 6
     $                                 + UCAW1(j3,j1)*(2*VBDWX1(j2,j4)+VBDWX2(j2,j4))*(2*eps2pB-2*eps2pD) ! 7
     $                                 + UCAW1(j3,j1)*(-t)*VBDW2(j2,j4)                                   ! 8
     $                                 + UCAW1(j3,j1)*(-eps2pB+eps2pD+2*eps2p1)*2*VBDWX2(j2,j4)           ! 9
     $                                                                            )
! terms due to ph2 ph1 attached to W
                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4) * DCMPLX(m_e_QED**2)*CPF*CPF2*CPF12*WVpi0*( 
     $                                  BornABCD(j1,j2,j3,j4)*(2*eps2pC-2*eps2pA)*(2*eps1pB-2*eps1pD)     ! 1
     $                                 +(2*eps2pC-2*eps2pA)  *VBDW1 (j2,j4) * UCAWX2(j3,j1)               ! 2
     $                                 +(2*eps2pC-2*eps2pA)*2*VBDWX1(j2,j4) * UCAW1 (j3,j1)               ! 3
     $                                 - 2*UCAWX2(j3,j1)*(2*eps1pB-2*eps1pD)* VBDW2 (j2,j4)               ! 4
     $                                 - 2*UCAWX2(j3,j1)*(eps2pC-eps2pA)    * VBDW1 (j2,j4)               ! 5
     $                                 + eps1D2*(-2*UCAWX2(j3,j1))*        2* VBDWX1(j2,j4)               ! 6
     $                                 + UCAW2(j3,j1)*(2*VBDWX2(j2,j4)+VBDWX1(j2,j4))*(2*eps1pB-2*eps1pD) ! 7
     $                                 + UCAW2(j3,j1)*(-t)*VBDW1(j2,j4)                                   ! 8
     $                                 + UCAW2(j3,j1)*(-eps1pB+eps1pD+2*eps1p2)*2*VBDWX1(j2,j4)           ! 9
     $                                                                            )
                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4)   ! terms due to 4 boson coupling
     $                 +CNorm*(-1D0) *DCMPLX(0,-m_e_QED**2)*CPF*CPF2*WVpi0
     $                 *(BornABCD(j1,j2,j3,j4)*2*eps1D2-UCAW1(j3,j1)*VBDW2(j2,j4)-UCAW2(j3,j1)*VBDW1(j2,j4))

               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       ! GPS_HiiPlusW

      SUBROUTINE GPS_HifPlus(CNorm,KFi,KFf,PX,pA,mA,pB,mB,pC,mC,pD,mD,Hel1,ph1,Hel2,ph2,mph,AmpWork) !
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Genuine IR-finite non 1-photon amplitudes for ISR-FSR are added to AmpWork    //
*//   1-st photon in Initial  state,  symmetrisation 1<-->2 is required!            //
*//                                                                                 //
*//   For Y_IR=1 IR-finite 1-photon contributions are calculated here as well       //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                              1                2                                 //
*//                              |                |                                 //
*//                              |                |                                 //
*//               c              |    OOOOOOOOOOOOOOOOOO          d                 //
*//     u  -------<------------- | ---OOOOOOOOOOOOOOOOOO----------<----- v          //
*//                              |         |                                        //
*//                              |         |X                                       //
*//                              |         |                                        //
*//      _       -b          OOOOOOOOOOOOOOOOOOOOO                a                 //
*//      v  ------<----------OOOOOOOOOOOOOOOOOOOOO----------------<----- u          //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           KFi,KFf
      DOUBLE PRECISION  PX(4),pA(4),pB(4),pC(4),pD(4),ph1(4),ph2(4),ph(4)
      DOUBLE PRECISION  mA,mB,mC,mD,mph
      DOUBLE COMPLEX    CNorm
      INTEGER           Hel1,Hel2
      DOUBLE COMPLEX    Born1BCD(2,2,2,2),BornAB2D(2,2,2,2),BornABCD(2,2,2,2)
      DOUBLE COMPLEX    BornA1CD(2,2,2,2),BornABC2(2,2,2,2)
      DOUBLE COMPLEX    Born1B2D(2,2,2,2),Born1BC2(2,2,2,2)
      DOUBLE COMPLEX    BornA12D(2,2,2,2),BornA1C2(2,2,2,2)
      DOUBLE COMPLEX    AmpWork(2,2,2,2)
      DOUBLE COMPLEX    U11a(2,2),Vb11(2,2),Uc22(2,2),V22d(2,2)
      DOUBLE COMPLEX    Su1,Su2
      DOUBLE COMPLEX    GPS_soft,GPS_softb
      DOUBLE COMPLEX    Sini(2),Sfin(2)
      INTEGER           j,j1,j2,j3,j4,k,Sig,l
      DOUBLE PRECISION  prA1,prB1,prC2,prD2
      DOUBLE PRECISION  BornV_GetCharge,ChaIni,ChaFin
      DOUBLE COMPLEX    gI,gF,sProd
      INTEGER           Y_IR,N_IR
      DOUBLE PRECISION  PP2(4),QQ(4),SvarQ,SvarX2
*----------------------------------------
      Y_IR=1                  ! YES, IR included
      Y_IR=0                  ! No,  IR not included
      N_IR=1-Y_IR
*--------------------
      ChaIni =  BornV_GetCharge( KFi)
      ChaFin =  BornV_GetCharge( KFf)
      gI = DCMPLX(ChaIni *m_e_QED)
      gF = DCMPLX(ChaFin *m_e_QED)
      IF( m_KeyArb  .EQ.  0 ) THEN
         Sini(1)  =  gI *GPS_soft(  1,ph1,pA,pB)
         Sfin(1)  = -gF *GPS_soft(  1,ph2,pC,pD)
      ELSE
         Sini(1)  =  gI *GPS_softb( 1,ph1,pA,mA,pB,mB)
         Sfin(1)  = -gF *GPS_softb( 1,ph2,pC,mC,pD,mD)
      ENDIF
      Sini(2) = -DCONJG(Sini(1))
      Sfin(2) = -DCONJG(Sfin(1))
* Calculate Born spin amplitudes
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    pB,-mB,    pC,MC,    pD,-md,   BornABCD) ! standard
      CALL GPS_Born(KFi,KFf,PX, ph1,mph,  pB,-mB,    pC,mC,    pD,-mD,   Born1BCD) ! A->1
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    ph1,-mph,  pC,mC,    pD,-mD,   BornA1CD) ! B->1
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    pB,-mB,    ph2,mph,  pD,-mD,   BornAB2D) ! C->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    pB,-mB,    pC,mC,    ph2,-mph, BornABC2) ! D->2
      CALL GPS_Born(KFi,KFf,PX, ph1,mph,  pB,-mB,    ph2,mph,  pD,-mD,   Born1B2D) ! A->1,C->2
      CALL GPS_Born(KFi,KFf,PX, ph1,mph,  pB,-mB,    pC,mC,    ph2,-mph, Born1BC2) ! A->1,D->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    ph1,-mph,  ph2,mph,  pD,-mD,   BornA12D) ! B->1,C->2
      CALL GPS_Born(KFi,KFf,PX, pA,mA,    ph1,-mph,  pC,mC,    ph2,-mph, BornA1C2) ! B->1,D->2
      DO k=1,4
         PP2 (k) = pC(k)+pD(k)+ph2(k)
         QQ(k)   = pC(k)+pD(k)
      ENDDO
      svarX2  =  PP2(4)**2  -PP2(3)**2  -PP2(2)**2  -PP2(1)**2
      svarQ   =   QQ(4)**2   -QQ(3)**2   -QQ(2)**2   -QQ(1)**2
* Fermion propagarotors ini
      prA1=  1d0/(pA(4)*ph1(4)-pA(3)*ph1(3)-pA(2)*ph1(2)-pA(1)*ph1(1))/2d0
      prB1= -1d0/(pB(4)*ph1(4)-pB(3)*ph1(3)-pB(2)*ph1(2)-pB(1)*ph1(1))/2d0
* Fermion propagarotors fin
      prC2=  1d0/(pC(4)*ph2(4)-pC(3)*ph2(3)-pC(2)*ph2(2)-pC(1)*ph2(1))/2d0
      prD2= -1d0/(pD(4)*ph2(4)-pD(3)*ph2(3)-pD(2)*ph2(2)-pD(1)*ph2(1))/2d0
      Sig = 3-2*Hel1
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrU( gI, ph1,Sig,  ph1,mph,  pA,mA,    U11a) ! <1|[1]|a>
         CALL GPS_MatrV( gI, ph1,Sig,  pB,mB,   ph1,mph,   Vb11) ! <b|{1}|1>
      ELSE
         CALL GPS_MatrUb(gI, ph1,Sig,  ph1,mph,  pA,mA,    U11a)
         CALL GPS_MatrVb(gI, ph1,Sig,  pB,mB,   ph1,mph,   Vb11)
      ENDIF
      Sig = 3-2*Hel2
      IF( m_KeyArb  .EQ.  0 ) THEN
         CALL GPS_MatrU( gF, ph2,Sig,  pC,mC,   ph2,mph,   Uc22) ! <c|[2]|2>
         CALL GPS_MatrV( gF, ph2,Sig,  ph2,mph,  pD,mD,    V22d) ! <2|{2}|d>
      ELSE
         CALL GPS_MatrUb(gF, ph2,Sig,  pC,mC,   ph2,mph,   Uc22) ! 
         CALL GPS_MatrVb(gF, ph2,Sig, ph2,mph,  pD,mD,     V22d) ! 
      ENDIF
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Su1= DCMPLX(0d0,0d0)
                  DO j=1,2
*      /////////////////////////////////////////////////////////////////////////////////////
*      //               c                |2                             d                 //
*      //      u  ------<-------SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS-----<----- v          //
*      //                                        |                                        //
*      //                                        |X             |1                        //
*      //      _       -b                        |     a+m-1    |       a                 //
*      //      v  ------<------------------------O--------------O-------<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Born1BCD( j,j2,j3,j4) *U11a(j,j1)*prA1 *Sfin(Hel2)*Y_IR !<b|X|1[1]|a><c|(+2)|X|(+2)|d>
*      /////////////////////////////////////////////////////////////////////////////////////
*      //               c                |2                            -d                 //
*      //      u  ------<-------SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS-----<----- v          //
*      //                                        |                                        //
*      //                          |1            |X                                       //
*      //      _       -b          |   -b+m+1    |                      a                 //
*      //      v  ------<----------O-------------O----------------------<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Vb11(j2,j)*prB1 *BornA1CD(j1,j,j3,j4) *Sfin(Hel2)*Y_IR !<b|[1]1|X|a><c|(+2)|X|(+2)|d>
*      /////////////////////////////////////////////////////////////////////////////////////
*      //                         |2                                                      //
*      //               c         |   c+m+2                            -d                 //
*      //      u  ------<---------O--------------O----------------------<----- v          //
*      //                                        |X                                       //
*      //      _       -b                        |       |1             a                 //
*      //      v  ------<------SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS-----<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +Uc22(j3,j)*prC2 *BornAB2D(j1,j2,j,j4) *Sini(Hel1)*Y_IR !<b|(+1)|X|(+1)|a><c|[2]2|X|d>
*      /////////////////////////////////////////////////////////////////////////////////////
*      //                                                       |2                        //
*      //               c                            -d+m+2     |      -d                 //
*      //      u  ------<------------------------O----------------------<----- v          //
*      //                                        |X                                       //
*      //      _       -b                        |       |1             a                 //
*      //      v  ------<------SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS-----<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                     Su1=Su1 +BornABC2(j1,j2,j3,j) *V22d(j,j4)*prD2  *Sini(Hel1)*Y_IR !<b|(+1)|X|(+1)|a><c|X|2[2]d>
                  ENDDO
                  Su2= DCMPLX(0d0,0d0)
                  DO j=1,2
                     DO l=1,2
*      /////////////////////////////////////////////////////////////////////////////////////
*      //                         |2                                                      //
*      //               c         |   c+m+2                             d                 //
*      //      u  ------<---------O--------------O----------------------<----- v          //
*      //                                        |                                        //
*      //                                        |X             |1                        //
*      //      _       -b                        |     a+m-1    |       a                 //
*      //      v  ------<------------------------O--------------O-------<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Uc22(j3,l )*prC2 *Born1B2D(j,j2,l,j4) *U11a(j,j1)*prA1 !<b|X|1[1]|a><c|[2]2|X|d>
*      /////////////////////////////////////////////////////////////////////////////////////
*      //                                                       |2                        //
*      //               c                             -d+m-2    |       d                 //
*      //      u  ------<------------------------O--------------O-------<----- v          //
*      //                                        |                                        //
*      //                                        |X             |1                        //
*      //      _       -b                        |     a+m-1    |       a                 //
*      //      v  ------<------------------------O--------------O-------<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Born1BC2(j,j2,j3,l) *U11a(j,j1)*prA1  *V22d(l,j4)*prD2 !<b|X|1[1]|a><c|X|2{2}|d>
*      /////////////////////////////////////////////////////////////////////////////////////
*      //                       |2                                                        //
*      //               c       |    c+m+2                              d                 //
*      //      u  ------<-------O----------------O----------------------<----- v          //
*      //                                        |                                        //
*      //                       |1               |X                                       //
*      //      _       -b       |   -b+m+1       |                      a                 //
*      //      v  ------<-------O----------------O----------------------<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Vb11(j2,j)*prB1  *Uc22(j3,l )*prC2 *BornA12D(j1,j,l,j4)!<b|{1}1|X|a><c|[2]2|X|d>
*      /////////////////////////////////////////////////////////////////////////////////////
*      //                                                       |2                        //
*      //               c                             -d+m-2    |       d                 //
*      //      u  ------<------------------------O--------------O-------<----- v          //
*      //                                        |                                        //
*      //                       |1               |X                                       //
*      //      _       -b       |   -b+m+1       |                      a                 //
*      //      v  ------<-------O----------------O----------------------<----- u          //
*      /////////////////////////////////////////////////////////////////////////////////////
                        Su2=Su2 +Vb11(j2,j)*prB1  *BornA1C2(j1,j,j3,l) *V22d(l,j4)*prD2 !<b|{1}1|X|a><c|X|2[2]|d>
                     ENDDO
                  ENDDO
                  sProd = Sini(Hel1)* Sfin(Hel2)
                  AmpWork(j1,j2,j3,j4) =AmpWork(j1,j2,j3,j4) 
     $                 +CNorm*( Su1+Su2 )
     $                 +CNorm*BornABCD(j1,j2,j3,j4)*sProd                      *Y_IR !
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       ! GPS_HifPlus


*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//                         Pseudo-CLASS  GPS                                       //
*//         This will become a separate class in the near future                    //
*//                                                                                 //
*//       Purpose:  Calculation of spin amplitudes using spinor methods             //
*//                                                                                 //
*//                 Library of basic tools used in CEEX.f                           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////

 
      SUBROUTINE GPS_PhelStart(nphot,last)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//    Not Used                                                               //
*//    Initialize first photon helicity combination                           //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER   nphot,last
      INTEGER   i
*----------  Initialize first partition -----------------
      last=0
      DO i = 1,nphot
         m_Phel(i) = 0
      ENDDO
      END

      SUBROUTINE GPS_PhelPlus(nphot,last)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//    Not Used                                                               //
*//   update m_Phel, check if it is last combination                          //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER   nphot,last
      INTEGER   i
*
      IF(nphot .EQ. 1) last=1
      m_Phel(1)=m_Phel(1)+1       !!!
      DO i=1,nphot
         IF( m_Phel(i).EQ. 2 ) THEN
            m_Phel(i)=0          !!!
            m_Phel(i+1)=m_Phel(i+1)+1 !!!
            IF( m_Phel(nphot) .EQ. 2 ) last=2
         ENDIF
      ENDDO
      END

      SUBROUTINE GPS_PhelRandom(nphot)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//    Not Used                                                               //
*//   Generate photon helicities randomly                                     //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER   nphot
      INTEGER   i
      REAL                 rvec(m_phmax)
* 
      CALL PseuMar_MakeVec(rvec,nphot)
      DO i=1,nphot
         IF( rvec(i) .GT. 0.5d0 ) THEN
            m_Phel(i)=0
         ELSE
            m_Phel(i)=1
         ENDIF
      ENDDO
      END

      SUBROUTINE GPS_PartitionStart(nphot,last)
*///////////////////////////////////////////////////////////////////////////////
*//   Initialize first partition                                              //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER   nphot,last
      INTEGER   i
*----------
      IF(     (m_KeyISR. EQ. 1) .AND. (m_KeyFSR. EQ. 1) ) THEN
         IF(      m_KeyINT .NE. 0) THEN
*     Normal case, ISR+FSR
            DO i = 1,nphot
               m_isr(i) = 0     ! Start with all FSR
            ENDDO
            last=0              ! Run through all partitions
         ELSE
*     INTerference OFF, Copy partition from crude MC
            CALL KK2f_GetIsr(m_isr)
            last=1              ! Exit next time
         ENDIF
      ELSEIF( (m_KeyISR. EQ. 1) .AND. (m_KeyFSR. EQ. 0) ) THEN
*     Special test, ISR ONLY
         DO i = 1,nphot
            m_isr(i) = 1        ! Start with all ISR
         ENDDO
         last=1                 ! Exit next time
      ELSEIF( (m_KeyISR. EQ. 0) .AND. (m_KeyFSR. EQ. 1) ) THEN
*     Special test, FSR ONLY
         DO i = 1,nphot
            m_isr(i) = 0        ! Start with all FSR
         ENDDO
         last=1                 ! Exit next time
      ELSE
         WRITE(*,*) '#### GPS_PartitionStart: Wrong KeyISR,KeyFSR = ',m_KeyISR,m_KeyFSR
         STOP
      ENDIF
      END

      SUBROUTINE GPS_PartitionPlus(nphot,last)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   update m_isr, check if it is last partition                             //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER   nphot,last
      INTEGER   i
*
      IF(nphot .EQ. 1) last=1   !!! Exit next time
      m_isr(1)=m_isr(1)+1
      DO i=1,nphot
         IF( m_isr(i).EQ. 2 ) THEN
            m_isr(i)=0
            m_isr(i+1)=m_isr(i+1)+1
            IF( m_isr(nphot) .EQ. 2 ) last=2 !!! Immediate exit
         ENDIF
      ENDDO
      END


      SUBROUTINE GPS_BornZero(AmpBorn)
*//////////////////////////////////////////////////////////////////////////////////
*//   Set AmpBorn to zero                                                        //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      DOUBLE COMPLEX  AmpBorn(2,2,2,2)
      INTEGER    j1,j2,j3,j4
*
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  AmpBorn(j1,j2,j3,j4) = DCMPLX(0d0,0d0)
               ENDDO                  
            ENDDO
         ENDDO
      ENDDO
      END                       !!!GPS_BornZero!!!

      SUBROUTINE GPS_BornCopy(AmpBorn,AmpBorn2)
*//////////////////////////////////////////////////////////////////////////////////
*//   Copy AmpBorn into AmpBorn2                                                 //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      DOUBLE COMPLEX  AmpBorn(2,2,2,2),AmpBorn2(2,2,2,2)
      INTEGER    j1,j2,j3,j4
*
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  AmpBorn2(j1,j2,j3,j4) = AmpBorn(j1,j2,j3,j4)
               ENDDO                  
            ENDDO
         ENDDO
      ENDDO
      END                       !!!GPS_BornCopy!!!

      SUBROUTINE GPS_BornSumSq(AmpBorn,Sum)
*//////////////////////////////////////////////////////////////////////////////////
*//   Sum up Born amplitudes squared                                             //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      DOUBLE COMPLEX  AmpBorn(2,2,2,2)
      DOUBLE PRECISION      Sum
*
      INTEGER    j1,j2,j3,j4
*----------------------------------
      Sum = 0d0
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  Sum = Sum +AmpBorn(j1,j2,j3,j4)*DCONJG(AmpBorn(j1,j2,j3,j4))
               ENDDO                  
            ENDDO
         ENDDO
      ENDDO
      END                       !!!GPS_BornSumSq!!!


      SUBROUTINE GPS_TralorPrepare(QQ,id)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Prepares transformation for Tralor, according to GPS rules              //
*//   The resulting Lorenz transformation matrix is stored for multiple use!  //
*//   This organization saves time in the case of multiple calls for many     //
*//   decay products.                                                         //
*//   Ident id=1,2 for two fermions only, later on may be extended.           //
*//   Note that Tralor is a good candidate for separate class!                //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER id                ! Ident of fermion, id=1,2 for 2 fermions
      DOUBLE PRECISION   QQ(4)             ! Fermion 4-momentum in LAB
      DOUBLE PRECISION   Lorenz(4,4),Lorinv(4,4)
      DOUBLE PRECISION   Rot(4,4),   Rotinv(4,4)
      DOUBLE PRECISION   EQ,MQ,sum,a,b
      DOUBLE PRECISION   xi(4),eta(4)
      INTEGER k,l
*--------------------------------------------------
* Construct Lorenz transformation from/to QQ rest frame (Jackson style)
      CALL KinLib_DefBostQQ(-1,QQ,Lorenz) ! from ferm. rest frame to LAB
      CALL KinLib_DefBostQQ( 1,QQ,Lorinv) ! from LAB to ferm. rest frame

* Additional 3-rotation according to GPS rules
      CALL KinLib_VecTrasform(Lorinv, m_Xi,  xi)
      CALL KinLib_VecTrasform(Lorinv, m_Eta, eta)
      CALL GPS_GPS(xi,eta,Rot)
* Inverse matrix is just transpose of Rot
      CALL KinLib_LorCopy(Rot,Rotinv)
      CALL KinLib_RotTranspose(Rotinv)
* Consctruct total GPS transformation
      CALL KinLib_LorMult(Lorenz,      Rot,  Lorenz) ! towards LAB
      CALL KinLib_LorMult(Rotinv,   Lorinv,  Lorinv)
* Memorize transformation matrices for further use
      IF( id .EQ. 1) THEN
         CALL KinLib_LorCopy(Lorenz,m_Loren1) ! towards LAB
         CALL KinLib_LorCopy(Lorinv,m_Lorin1)
      ELSEIF(id .EQ. 2) THEN
         CALL KinLib_LorCopy(Lorenz,m_Loren2) ! towards LAB
         CALL KinLib_LorCopy(Lorinv,m_Lorin2)
      ELSE
         GOTO 900
      ENDIF
      RETURN
 901  WRITE(*,*) '++++++++ GPS_TralorMake: WRONG QQ, not timelike'
      STOP
 900  WRITE(*,*) '++++++++ GPS_TralorMake: WRONG id=',id
      STOP
      END


      SUBROUTINE GPS_GPS(xi,eta,Rot)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*// Defines basis vectors e1,e2,e2 from xi and eta                            //
*// Columns in Rot are e1,e2,e2                                               //
*// Called in GPS_TralorPrepare                                               //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      DOUBLE PRECISION   xi(4),eta(4)
      DOUBLE PRECISION   Rot(4,4)
      DOUBLE PRECISION   e1(4),e2(4),e3(4),a
      INTEGER k
*---------------------------------------
* GPS rule 1: z-axis antiparalel to xi
      a = SQRT(xi(1)**2 +xi(2)**2 +xi(3)**2 )
      e3(1) = -xi(1)/a
      e3(2) = -xi(2)/a
      e3(3) = -xi(3)/a
      e3(4) = 0d0
* GPS rule 2: x-axis in plane (+eta, -xi),
* that is y-axis perpendicular to (eta,xi), i.e. e2 = eta X xi
      e2(1) =  eta(2)*xi(3) -eta(3)*xi(2)
      e2(2) = -eta(1)*xi(3) +eta(3)*xi(1)
      e2(3) =  eta(1)*xi(2) -eta(2)*xi(1)
      a = SQRT(e2(1)**2 +e2(2)**2 +e2(3)**2 )
      e2(1) = e2(1)/a
      e2(2) = e2(2)/a
      e2(3) = e2(3)/a
      e2(4) = 0d0
      e1(1) = e2(2)*e3(3) -e2(3)*e3(2)
      e1(2) =-e2(1)*e3(3) +e2(3)*e3(1)
      e1(3) = e2(1)*e3(2) -e2(2)*e3(1)
      e1(4) = 0d0
* Define additional rotation matrix from GPS (e1,e2,e3) frame 
* to actual fermion rest frame defined with matrix Lorenz 
      CALL KinLib_RotColumn(e1,e2,e3,Rot)
      END


      SUBROUTINE GPS_TralorDoIt(id,pp,q)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Transform pp from rest frame of fermion id to LAB, q is result          //
*//   It uses Lorenz transfromation prepared and memorized in                 //
*//   the subprogram GPS_TralorPrepare, which has to be called first!         //
*//   This organization saves time in the case of multiple calls for many     //
*//   decay products.                                                         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER id                ! Ident of fermion, id=1,2 for 2 fermions
      DOUBLE PRECISION   pp(4),q(4)        ! fermion moms to be transformed TO LAB
      INTEGER k,l
      DOUBLE PRECISION   p(4),sum
*-----------------
      DO k=1,4
         p(k)=pp(k)
      ENDDO
* Transform vector p, i.e. multiply by matrix Loren1 or Loren2
      IF( id .EQ. 1) THEN
         DO k=1,4
            sum = 0d0
            DO l=1,4
               sum=sum+ m_Loren1(k,l)*p(l)
            ENDDO
            q(k) = sum
         ENDDO
      ELSEIF( id .EQ. 2) THEN
         DO k=1,4
            sum = 0d0
            DO l=1,4
               sum=sum+ m_Loren2(k,l)*p(l)
            ENDDO
            q(k) = sum
         ENDDO
      ELSE
         GOTO 900
      ENDIF
*-----------
      RETURN
 900  WRITE(*,*) '++++++++ WRONG id in GPS_TralorDoIt =',id
      STOP
      END

      SUBROUTINE GPS_TralorUnDo(id,pp,q)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//               Inverse of GPS_TralorDoIt                                   //
*//                                                                           //
*//   Transform pp from LAB to rest frame of fermion, q is result             //
*//   It uses Lorenz transfromation prepared and memorized in                 //
*//   the subprogram GPS_TralorPrepare, which has to be called first!         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER id                ! Ident of fermion, id=1,2 for 2 fermions
      DOUBLE PRECISION   pp(4),q(4)        ! fermion moms to be transformed TO LAB
      INTEGER k,l
      DOUBLE PRECISION   p(4),sum
*-----------------
      DO k=1,4
         p(k)=pp(k)
      ENDDO
* Transform vector p, i.e. multiply by matrix Loren1 or Loren2
      IF( id .EQ. 1) THEN
         DO k=1,4
            sum = 0d0
            DO l=1,4
               sum=sum+ m_Lorin1(k,l)*p(l)
            ENDDO
            q(k) = sum
         ENDDO
      ELSEIF( id .EQ. 2) THEN
         DO k=1,4
            sum = 0d0
            DO l=1,4
               sum=sum+ m_Lorin2(k,l)*p(l)
            ENDDO
            q(k) = sum
         ENDDO
      ELSE
         GOTO 900
      ENDIF
*-----------
      RETURN
 900  WRITE(*,*) '++++++++ WRONG id in GPS_TralorUnDo =',id
      STOP
      END

      SUBROUTINE GPS_TraJacobWick(Mode,QQ,pp,rr)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Tralor-type transformation for Jacob Wick quantization axies.           //
*//   Not optimized, mainly for tests.                                        //
*//                                                                           //
*//   Mode =-1 from ferm_rest to LAB  (normal mode for KORALB Tralor)         //
*//   Mode =+1 from LAB to ferm_rest                                          //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER Mode
      DOUBLE PRECISION   QQ(4)             ! Fermion 4-momentum in LAB
      DOUBLE PRECISION   pp(4)             ! input momentum
      DOUBLE PRECISION   rr(4)             ! transformed momentum
      DOUBLE PRECISION   MQ,pQ, exe, Theta, Phi
      DOUBLE PRECISION   Lor(4,4),Bos3(4,4),Rot2(4,4),Rot3(4,4)
      INTEGER i,j
      DOUBLE PRECISION   KinLib_AngPhi
*--------------------------------
      pQ = DSQRT(QQ(1)**2 +QQ(2)**2 +QQ(3)**2)
      MQ = QQ(4)**2-pQ**2
      IF(MQ .LE. 0d0 ) GOTO 901
      MQ  = DSQRT(MQ)
      exe   = (QQ(4)+pQ)/MQ     ! exe=exp(hiper_velocity), exe>1 for q(3)>0
* Completely standard Theta, Phi
      Theta = KinLib_AngPhi(QQ(3),DSQRT(QQ(1)**2+QQ(2)**2) ) ! range (0,pi)
      Phi   = KinLib_AngPhi(QQ(1),QQ(2))                     ! range (0,2pi)
      IF(Mode .EQ. 1) THEN
         exe   = 1d0/exe
         Theta = -Theta
         Phi   = -Phi
      ENDIF
* Define matrices for elementary boosts/rotations
      CALL KinLib_DefBoost(3,     exe,Bos3) ! Calculate Boost3 matrix
      CALL KinLib_DefRotor(3,1, Theta,Rot2) ! Rotation x-z plane, around y axis
      CALL KinLib_DefRotor(1,2,   Phi,Rot3) ! Rotation x-y plane, around z axis
* Define the Entire transformation matrix (slow but secure)
      IF(Mode .EQ. -1) THEN
*     Mode =-1 from ferm_rest to LAB  (normal mode in KORALB Tralor)
         CALL KinLib_LorCopy(Bos3,     Lor) ! Lor=Bos3(exe)
         CALL KinLib_LorMult(Rot2,Lor, Lor) ! Lor=Rot2(Theta)*Bos3(exe)
         CALL KinLib_LorMult(Rot3,Lor, Lor) ! Lor=Rot3(Phi)*Rot2(Theta)*Bos3(exe)
      ELSEIF( Mode .EQ. 1) THEN
*     Mode =+1 from LAB to ferm_rest
         CALL KinLib_LorCopy(Rot3,     Lor) ! Lor=Rot3(-Phi)
         CALL KinLib_LorMult(Rot2,Lor, Lor) ! Lor=Rot2(-Theta)*Rot3(-Phi)
         CALL KinLib_LorMult(Bos3,Lor, Lor) ! Lor=Bos3(1/exe)*Rot2(-Theta)*Rot3(-Phi)
      ELSE
         GOTO 902
      ENDIF
* Transform vector
      CALL KinLib_VecTrasform(Lor,pp,rr)
      RETURN
 901  WRITE(*,*) '++++++++GPS_TraJacobWick: WRONG QQ, not timelike'
      STOP
 902  WRITE(*,*) '++++++++GPS_TraJacobWick: WRONG Mode= ', Mode
      STOP
      END


      SUBROUTINE GPS_RmatMake
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//   Translates Born spin amplitudes into double spin density matrix               //
*//                m_AmpBorn  -----> R_{ab}                                         //
*//                                                                                 //
*//   Notes:                                                                        //
*//   Polarizations for beams not included but it will be strightforward...         //
*//   m_Rmat( k, l) is realy REAL, we keep it complex to be able to xcheck it.      //
*//   One should remember to use DREAL( m_Rmat( k, l) ) in the calculations!        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    k,l
      INTEGER    i1,i2,i3,i4
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  Sum,cx

      CALL GPS_Initialize
*
* No polarization yet for initial state spins
* Loop over 4*4*2**6=16*64=1024 indices!
* The formula is eq. (2.14) in Acta. Phys. Pol. B16 (1985) 1007.
      DO k = 0,3
         DO l = 0,3
            Sum = DCMPLX(0d0,0d0)
            DO i1 = 1,2
               DO i2 = 1,2
                  DO i3 = 1,2
                     DO i4 = 1,2
                        DO j3 = 1,2
                           DO j4 = 1,2
                              Sum= Sum+
     $                                      m_AmpBorn( i1,i2, i3,i4)
     $                             *DCONJG( m_AmpBorn( i1,i2, j3,j4) )
     $                             *m_Pauli( k,j3,i3)
     $                             *m_Pauli( l,j4,i4)
                           ENDDO
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            m_Rmat( k, l) = Sum
         ENDDO
      ENDDO
      cx = m_Rmat( 0, 0)
      DO k = 0,3
         DO l = 0,3
            m_Rmat( k, l) = m_Rmat( k, l)/cx
         ENDDO
      ENDDO
      END                       !Rmat


      SUBROUTINE GPS_RmatMake2
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    !!!!!!!!!!!!!!!!!!!!!!! TEST TEST !!!!!!!!!!!!!!!!!!!!!!!                    //
*//                                                                                 //
*//             m_AmpBorn2 instead of m_AmpBorn                                     //
*//                m_AmpBorn  -----> R_{ab}                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    k,l
      INTEGER    i1,i2,i3,i4
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  Sum,cx

      CALL GPS_Initialize
*
* No polarization yet for initial state spins
* Loop over 4*4*2**6=16*64=1024 indices!
* The formula is eq. (2.14) in Acta. Phys. Pol. B16 (1985) 1007.
      DO k = 0,3
         DO l = 0,3
            Sum = DCMPLX(0d0,0d0)
            DO i1 = 1,2
               DO i2 = 1,2
                  DO i3 = 1,2
                     DO i4 = 1,2
                        DO j3 = 1,2
                           DO j4 = 1,2
                              Sum= Sum+
     $                                      m_AmpBorn2( i1,i2, i3,i4)
     $                             *DCONJG( m_AmpBorn2( i1,i2, j3,j4) )
     $                             *m_Pauli( k,j3,i3)
     $                             *m_Pauli( l,j4,i4)
                           ENDDO
                        ENDDO
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
            m_Rmat( k, l) = Sum
         ENDDO
      ENDDO
      cx = m_Rmat( 0, 0)
      DO k = 0,3
         DO l = 0,3
            m_Rmat( k, l) = m_Rmat( k, l)/cx
         ENDDO
      ENDDO
      END                       !Rmat


      SUBROUTINE GPS_RmatMult(Rot1,Rot2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Multiplies tensor m_Rmat (spin correlations joint density matrix)             //
*//   with two rotatios Rot1 and Rot2.                                              //
*//   Transposed Rot1 and Rot2 are realy employed.                                  //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      DOUBLE PRECISION      Rot1(4,4),Rot2(4,4)
      INTEGER    i,j,k,l
      DOUBLE PRECISION      sum,sum1,Rmat(0:3,0:3)
*-----------------------------------
* Save old result
      DO i=0,3
         DO j=0,3
            Rmat(i,j)=m_Rmat(i,j)
         ENDDO
      ENDDO
* Transform first index in tensor Rmat
      DO i=1,3                  !active
         DO j=0,3               !passive
            sum  = 0d0
            DO k=1,3
               sum  = sum + Rmat(k,j) *Rot1(i,k)
            ENDDO
            m_Rmat(i,j) = sum
         ENDDO
      ENDDO
* Save partial result
      DO i=0,3
         DO j=0,3
            Rmat(i,j)=m_Rmat(i,j)
         ENDDO
      ENDDO
* Transform second index in Rmat
      DO i=0,3                  !passive
         DO j=1,3               !active
            sum  = 0d0
            DO k=1,3
               sum  = sum + Rmat(i,k) *Rot2(j,k)
            ENDDO
            m_Rmat(i,j) = sum
         ENDDO
      ENDDO
      END


      SUBROUTINE GPS_Hini(KFi,KFf,PX, p1,m1,p2,m2,p3,m3,p4,m4,ph,mph, AmHarIsr)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   IR-finite part od 1-photon amplitudes for ISR                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    KFi,KFf
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),ph(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph

      DOUBLE COMPLEX  AmHarIsr(2,2,2,2,2)
      DOUBLE COMPLEX  AmpBornU(2,2,2,2)
      DOUBLE COMPLEX  AmpBornV(2,2,2,2)
      DOUBLE COMPLEX  Csum1,Csum2,U(2,2),V(2,2)
      INTEGER    j,j1,j2,j3,j4,k,Sig
      DOUBLE PRECISION      pr1,pr2,Fleps
*----------------------------------------
      Fleps =  1d-100
* ISR non-infrared two parts: (1) p1 -> photon, contracted with U-matrix
*                             (2) p2 -> photon, contracted with V-matrix
      CALL GPS_Born(KFi,KFf,PX, ph,mph,    p2,-Fleps,  p3,m3, p4,-m4, AmpBornU)
      CALL GPS_Born(KFi,KFf,PX, p1,Fleps,  ph,-mph,    p3,m3, p4,-m4, AmpBornV)
***   CALL GPS_BPrint(6,'Bo(k234)',AmpBornU)
***   CALL GPS_BPrint(6,'Bo(1k34)',AmpBornV)
* Fermion propagarotors
      pr1 = 1d0/(p1(4)*ph(4)-p1(3)*ph(3)-p1(2)*ph(2)-p1(1)*ph(1))/2d0
      pr2 =-1d0/(p2(4)*ph(4)-p2(3)*ph(3)-p2(2)*ph(2)-p2(1)*ph(1))/2d0
      DO k=1,2
         Sig = 3-2*k
         IF( m_KeyArb .EQ. 0 ) THEN
            CALL GPS_MakeU(ph,Sig,  ph,mph,  p1,m1,    U)
            CALL GPS_MakeV(ph,Sig,  p2,m2,   ph,mph,   V)
         ELSE
            CALL GPS_MakeUb(ph,Sig, ph,mph,  p1,m1,    U)
            CALL GPS_MakeVb(ph,Sig, p2,m2,   ph,mph,   V)
         ENDIF
***      WRITE(*,*) ' ///// sig = ', sig
***      CALL GPS_UPrint(6,' U(k,p1)    ',U)
***      CALL GPS_UPrint(6,' V(p2,k)    ',V)
         DO j1=1,2
            DO j2=1,2
               DO j3=1,2
                  DO j4=1,2
                     Csum1=DCMPLX(0d0,0d0)
                     Csum2=DCMPLX(0d0,0d0)
                     DO j=1,2
                        Csum1=Csum1 +U(j,j1)*pr1 *AmpBornU( j,j2,j3,j4)
                        Csum2=Csum2 +V(j2,j)*pr2 *AmpBornV(j1, j,j3,j4)
                     ENDDO
                     AmHarIsr(j1,j2,j3,j4,k) =  Csum1+Csum2
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       !!! GPS_Hini


      SUBROUTINE GPS_Hfin(KFi,KFf,PP, p1,m1,p2,m2,p3,m3,p4,m4,ph,mph, AmHarFsr)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   IR-finite part od 1-photon amplitudes for FSR                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER    KFi,KFf
      DOUBLE PRECISION      PP(4),p1(4),p2(4),p3(4),p4(4),ph(4)
      DOUBLE PRECISION      m1,m2,m3,m4,mph
*
      DOUBLE COMPLEX  AmHarFsr(2,2,2,2,2)
      DOUBLE COMPLEX  AmpBornU(2,2,2,2)
      DOUBLE COMPLEX  AmpBornV(2,2,2,2)
      DOUBLE COMPLEX  Csum1,Csum2,U(2,2),V(2,2)
      INTEGER    j,j1,j2,j3,j4,k,Sig
      DOUBLE PRECISION      pr1,pr2,Fleps
*----------------------------------------
      Fleps =  1d-100
* FSR non-infrared two parts: (1) p1 -> photon, contracted with U-matrix
*                             (2) p2 -> photon, contracted with V-matrix
      CALL GPS_Born(KFi,KFf,PP, p1,Fleps, p2,-Fleps, ph,mph, p4,-m4,   AmpBornU)
      CALL GPS_Born(KFi,KFf,PP, p1,Fleps, p2,-Fleps, p3,m3,  ph,-mph,  AmpBornV)
***   CALL GPS_BPrint(6,'Bo(12k4)',AmpBornU)
***   CALL GPS_BPrint(6,'Bo(123k)',AmpBornV)
* Fermion propagarotors
      pr1 = 1d0/(p3(4)*ph(4)-p3(3)*ph(3)-p3(2)*ph(2)-p3(1)*ph(1))/2d0
      pr2 =-1d0/(p4(4)*ph(4)-p4(3)*ph(3)-p4(2)*ph(2)-p4(1)*ph(1))/2d0
      DO k=1,2
         Sig = 3-2*k
         IF( m_KeyArb .EQ. 0 ) THEN
            CALL GPS_MakeU(ph,Sig,    p3,m3,  ph,mph,   U)
            CALL GPS_MakeV(ph,Sig,    ph,mph, p4,m4,    V)
         ELSE
            CALL GPS_MakeUb(ph,Sig,   p3,m3,  ph,mph,   U)
            CALL GPS_MakeVb(ph,Sig,   ph,mph, p4,m4,    V)
         ENDIF
***      WRITE(*,*) ' ///// sig = ', sig
***      CALL GPS_UPrint(6,' U(k,p3)    ',U)
***      CALL GPS_UPrint(6,' V(p4,k)    ',V)
         DO j1=1,2
            DO j2=1,2
               DO j3=1,2
                  DO j4=1,2
                     Csum1=DCMPLX(0d0,0d0)
                     Csum2=DCMPLX(0d0,0d0)
                     DO j=1,2
                        Csum1=Csum1 +U(j3,j)*pr1* AmpBornU(j1,j2, j,j4)
                        Csum2=Csum2 +V(j,j4)*pr2* AmpBornV(j1,j2,j3, j)
                     ENDDO
                     AmHarFsr(j1,j2,j3,j4,k) =  Csum1+Csum2
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      END                       !!! GPS_Hfin


      SUBROUTINE GPS_MakeBorn(KFi,KFf,PX,p1,p2,p3,p4,Born)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Define spin amplitudes and calculate Born x-section.                          //
*//   Resulting spin amplitudes stored in m_AmpBorn                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER    KFi,KFf
      DOUBLE PRECISION      PX(4),p1(4),p2(4),p3(4),p4(4),Born
*
      INTEGER    k,j1,j2,j3,j4
      DOUBLE PRECISION      Massf,Mbeam,Sum,Fleps
      DOUBLE PRECISION      svarX,svarQ,BetaFin
      INTEGER    NCf,BornV_GetColor
      DOUBLE PRECISION      BornV_GetMass
      DOUBLE PRECISION      PQ(4)
*----------------------------------
      CALL GPS_Initialize
      Mbeam  =  BornV_GetMass(KFi)
      Massf =  BornV_GetMass(KFf)
      Fleps = 1d-100
      Mbeam  = Fleps             ! electron mass almost zero
      DO k=1,4
         PQ(k)=p3(k)+p4(k)
      ENDDO
      svarX=PX(4)**2-PX(3)**2-PX(2)**2-PX(1)**2
      svarQ=PQ(4)**2-PQ(3)**2-PQ(2)**2-PQ(1)**2
      IF(svarX .LE. 4*Massf**2) GOTO 900

* Calculate Born spin amplitudes
      CALL GPS_Born(KFi,KFf,PX,p1,Mbeam,p2,-Mbeam,p3,Massf,p4,-Massf,m_AmpBorn)
* Calculate total x-section
      CALL GPS_BornSumSq(m_AmpBorn,Sum)
* Phase Space factor, Lorenz Beta due to final state 2-body phase space
      BetaFin = SQRT(1d0 -4d0*Massf**2/svarQ)
      Born = Sum*BetaFin
* Color factor
      NCf = BornV_GetColor(KFf)
      Born = NCf*Born
      RETURN
 900  Born = 0d0
      END


      SUBROUTINE GPS_MakeBorn2(KFi,KFf,p1,p2,p3,p4,Born)
*//////////////////////////////////////////////////////////////////////////////////
*//  !!!!!  TEST !!!!!   !!!!!  TEST !!!!! !!!!!  TEST !!!!! !!!!!  TEST !!!     //
*//                                                                              //
*//   Clasical, massive/massles, in terms of cos(theta), similar to KORALB       //
*//   We use here KORALB spin amplitudes for pure s-chanel photons               //
*//   Note that for KORALB x-axis is perpendicular to reaction plane             //
*//   while in JacobWick y-axis is perpendicular to reaction plane.              //
*//   We expect in Rmat, for pure s-chanel photon exchange,                      //
*//   strictly zero correlations between in-plane  and                           //
*//   perpendicular-to-plane components, see APP B15 (1984) p. 1154, eq. (2.6)   //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER KFi,KFf
      DOUBLE PRECISION   p1(4),p2(4),p3(4),p4(4)
      DOUBLE PRECISION   Born
*
      DOUBLE PRECISION    T3e,Qe,Ve,Ae
      DOUBLE PRECISION    T3f,Qf,Af,Vf
      INTEGER  NCf,NCe
      DOUBLE PRECISION    BetaFin
      DOUBLE PRECISION    svar,CosTheta
*-----------------------------------------------------------------------------
      DOUBLE COMPLEX  PropGam,PropZet
      DOUBLE COMPLEX  AmpGam(2,2,2,2),AmpZet(2,2,2,2)
      INTEGER    i,j,k,l
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  s31,s24,s14,s32
      DOUBLE PRECISION      CupGam,CupZet,CupZet1,CupZet2
      DOUBLE COMPLEX  HeliFactor
      DOUBLE COMPLEX  GPS_iProd1
      DOUBLE COMPLEX  GPS_iProd2
      DOUBLE PRECISION      Massf,Mbeam,Mfin,SinTheta
      DOUBLE PRECISION      Sum,Fleps
*-----------------------------------------------------------------------------
      CALL GPS_Initialize
      CALL BornV_GetParticle(KFf, Massf,Qf,T3f,NCf)
      CALL BornV_GetParticle(KFi, Mbeam, Qe,T3e,NCe)
      svar= (p1(4)+p2(4))**2 -(p1(3)+p2(3))**2 -(p1(2)+p2(2))**2 -(p1(1)+p2(1))**2
      Mfin = Massf*2d0/sqrt(svar)
      IF(svar .LE. 4*Massf**2) GOTO 900
      CosTheta= (p1(3)*p3(3)+p1(2)*p3(2)+p1(1)*p3(1))
     $     /SQRT(p1(3)*p1(3)+p1(2)*p1(2)+p1(1)*p1(1))
     $     /SQRT(p3(3)*p3(3)+p3(2)*p3(2)+p3(1)*p3(1))
      IF(abs(CosTheta) .GT. 1d0) WRITE(*,*) ' BornV: CosTheta=',CosTheta
      SinTheta = SQRT(1d0-CosTheta**2)
      WRITE(*,*) ' GPS_MakeBorn2: svar,CosTheta=',svar,CosTheta
* Couplings
      Ve    = (2*T3e -4*Qe*m_Sw2)/DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      Ae    =  2*T3e             /DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      Vf    = (2*T3f -4*Qf*m_Sw2)/DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      Af    =  2*T3f             /DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      IF(m_KeyZet .LE. 0) THEN
         Ve=0d0
         Ae=0d0
      ENDIF
      IF(m_KeyZet .EQ. 9) THEN
         Qe=0d0
         Qf=0d0
      ENDIF
*=============================================================
* Propagators
      PropGam =    DCMPLX(  1d0/svar,  0d0)
      PropZet =    1d0/DCMPLX(svar-m_MZ**2, m_GammZ*svar/m_MZ)
      IF(m_KeyZet .EQ.-1) PropZet =  1d0/DCMPLX(Svar-m_MZ**2, m_GammZ*m_MZ)
*=============================================================
* Clean
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  AmpGam( j1,j2,j3,j4) = DCMPLX(0d0,0d0)
                  AmpZet( j1,j2,j3,j4) = DCMPLX(0d0,0d0)
                  m_AmpBorn2( j1,j2,j3,j4) = DCMPLX(0d0,0d0)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
* Clasical, massles
      DO j1 = 1,2
         DO j3 = 1,2
            DO j4 = 1,2
               Hel1 = 3-2*j1
               Hel3 = 3-2*j3
               Hel4 = 3-2*j4
               Hel2 = -Hel1     ! helicity conservation
               j2  = (3-Hel2)/2
***[[ With finite Massf this part is valid strictly if you switch off Z exchange
               IF(Hel3 .EQ. -Hel4) THEN
                  HeliFactor = -DCMPLX( 0d0, Hel1*Hel3 -CosTheta )
               ELSE         
                  HeliFactor = -DCMPLX(Mfin*SinTheta, 0d0)
***  Activate line below if you want to get rid of mass effects completely!!!
***               HeliFactor = -DCMPLX(0d0, 0d0)
               ENDIF
***]]
               CupGam = Qe*Qf
               CupZet = (Ve +Hel2*Ae)*(Vf +Hel3*Af)
               AmpGam( j1,j2,j3,j4) = HeliFactor*PropGam*CupGam
               AmpZet( j1,j2,j3,j4) = HeliFactor*PropZet*CupZet
               m_AmpBorn2(j1,j2,j3,j4) =
     $              (AmpGam(j1,j2,j3,j4)+AmpZet(j1,j2,j3,j4))*svar/2
            ENDDO
         ENDDO
      ENDDO
* Spin sumation/averaging
      CALL GPS_BornSumSq(m_AmpBorn2,Sum)
      Born = Sum
* Lorenz Beta due to final state 2-body phase space
      BetaFin = SQRT(1d0 -4d0*Massf**2/svar)
      Born = Born*BetaFin
      Born = NCf*Born           ! Color factor
      RETURN
 900  Born = 0d0
      END      !GPS_MakeBorn2


      SUBROUTINE GPS_MakeBorn1(KFi,KFf,p1,p2,p3,p4,Born)
*///////////////////////////////////////////////////////////////////////////////
*//  !!!!!  TEST !!!!!   !!!!!  TEST !!!!! !!!!!  TEST !!!!! !!!!!  TEST !!!  //
*//                                                                           //
*//  Born  spin amplitudes, spinor method, massles, based on Chisholm         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER KFi,KFf
      DOUBLE PRECISION   p1(4),p2(4),p3(4),p4(4)
      DOUBLE PRECISION   Born
*
      DOUBLE PRECISION    T3e,Qe,Ve,Ae
      DOUBLE PRECISION    T3f,Qf,Af,Vf
      INTEGER  NCf,NCe
      DOUBLE PRECISION    BetaFin, svar
*-----------------------------------------------------------------------------
      DOUBLE COMPLEX  PropGam,PropZet
      DOUBLE COMPLEX  AmpGam(2,2,2,2),AmpZet(2,2,2,2)
      INTEGER    i,j,k,l
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  s31,s24,s14,s32
      DOUBLE PRECISION      CupGam,CupZet
      DOUBLE COMPLEX  HeliFactor
      DOUBLE COMPLEX  GPS_iProd1
      DOUBLE COMPLEX  GPS_iProd2
      DOUBLE PRECISION      Massf,Mbeam
      DOUBLE PRECISION      Sum
*-----------------------------------------------------------------------------
      CALL GPS_Initialize
      CALL BornV_GetParticle(KFf, Massf,Qf,T3f,NCf)
      CALL BornV_GetParticle(KFi, Mbeam, Qe,T3e,NCe)
      svar= (p1(4)+p2(4))**2 -(p1(3)+p2(3))**2 -(p1(2)+p2(2))**2 -(p1(1)+p2(1))**2
      IF(svar .LE. 4*Massf**2) GOTO 900
* Couplings
      Ve    = (2*T3e -4*Qe*m_Sw2)/DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      Ae    =  2*T3e             /DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      Vf    = (2*T3f -4*Qf*m_Sw2)/DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      Af    =  2*T3f             /DSQRT(m_Sw2*(1d0-m_Sw2))/4d0
      IF(m_KeyZet .LE. 0) THEN
         Ve=0d0
         Ae=0d0
      ENDIF
      IF(m_KeyZet .EQ. 9) THEN
         Qe=0d0
         Qf=0d0
      ENDIF
*=============================================================
* Propagators
      PropGam =    DCMPLX(  1d0/svar,  0d0)
      PropZet =    1d0/DCMPLX(svar-m_MZ**2, m_GammZ*svar/m_MZ)
      IF(m_KeyZet .EQ.-1) PropZet =  1d0/DCMPLX(Svar-m_MZ**2, m_GammZ*m_MZ)
*=============================================================
* Clean
      DO j1 = 1,2
         DO j2 = 1,2
            DO j3 = 1,2
               DO j4 = 1,2
                  AmpGam( j1,j2,j3,j4) = DCMPLX(0d0,0d0)
                  AmpZet( j1,j2,j3,j4) = DCMPLX(0d0,0d0)
                  m_AmpBorn1( j1,j2,j3,j4) = DCMPLX(0d0,0d0)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
* Spinor, massles, based on Chisholm
      DO j1 = 1,2
         DO j3 = 1,2
            Hel1 = 3-2*j1
            Hel3 = 3-2*j3
            Hel2 = -Hel1        ! helicity conservation initial state
            Hel4 = -Hel3        ! helicity conservation initial state
            j2  = (3-Hel2)/2
            j4  = (3-Hel4)/2
            CupGam = Qe*Qf
            IF( Hel1 .EQ. -Hel3 ) THEN
               s31 = GPS_iProd1( -Hel1,p3,p1)
               s24 = GPS_iProd1(  Hel1,p2,p4)
               HeliFactor = s31*s24
               CupZet = (Ve +Hel2*Ae)*(Vf +Hel1*Af)
            ELSE
               s14 = GPS_iProd1( -Hel1,p1,p4)
               s32 = GPS_iProd1(  Hel1,p3,p2)
               HeliFactor = s14*s32
               CupZet = (Ve +Hel2*Ae)*(Vf +Hel2*Af)
            ENDIF
            AmpGam( j1,j2,j3,j4) = HeliFactor*PropGam*CupGam
            AmpZet( j1,j2,j3,j4) = HeliFactor*PropZet*CupZet
            m_AmpBorn1(j1,j2,j3,j4) = AmpGam(j1,j2,j3,j4)+AmpZet(j1,j2,j3,j4)
         ENDDO
      ENDDO
* Spin sumation/averaging
      CALL GPS_BornSumSq(m_AmpBorn1,Sum)
* Lorenz Beta due to final state 2-body phase space
      BetaFin = SQRT(1d0 -4d0*Massf**2/svar)
      Born = Sum*BetaFin
      Born = NCf*Born           ! Color factor
      RETURN
 900  Born = 0d0
      END                       !!!!!!  GPS_MakeBorn1


      SUBROUTINE GPS_BornSimple(KFi,KFf,svar,costhe,Born)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Simple Born similar as in BornV                                         //
*//   Limitation: final mass terms exact for photon exchange!                 //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER  KFi,KFf
      DOUBLE PRECISION    svar,costhe
      DOUBLE PRECISION    Born
*
      DOUBLE PRECISION    ss,T3e,Qe,deno,Ve,Ae
      DOUBLE PRECISION    ye,yf,xf,rechi,xe,amx2
      DOUBLE PRECISION    thresh,ff0,ff1,chi2
      DOUBLE PRECISION    t3f,af,vf,sum,qf
      DOUBLE PRECISION    Massf,Mbeam
      DOUBLE PRECISION    BWD
      INTEGER  NCf,NCe
      DOUBLE PRECISION    BetaFin
*-----------------------------------------------------------------
      CALL GPS_Initialize
      CALL BornV_GetParticle(KFi, Mbeam, Qe,T3e,NCe)
      CALL BornV_GetParticle(KFf, Massf,Qf,T3f,NCf)

      IF(abs(costhe) .GT. 1d0) WRITE(*,*) ' BornV: costhe=',costhe

      ss = svar
      amx2=4d0*Massf**2/svar
* Z and gamma couplings to beams (electrons)
      deno  = 4d0*sqrt(m_Sw2*(1d0-m_Sw2))
      Ve    = (2*T3e -4*Qe*m_Sw2)/deno
      Ae    =  2*T3e             /deno
      Vf    = (2*T3f -4*Qf*m_Sw2)/deno
      Af    =  2*T3f             /deno

      IF(m_KeyZet .LE. 0) THEN
         Ve=0d0
         Ae=0d0
      ENDIF
      IF(m_KeyZet .EQ. 9) THEN
         Qe=0d0
         Qf=0d0
      ENDIF
      BWD = (ss-m_MZ**2)**2 + (m_GammZ*ss/m_MZ)**2
      IF(m_KeyZet .EQ.-1) BWD = (ss-m_MZ**2)**2 + (m_GammZ*m_MZ)**2
      chi2 = ss**2        /BWD
      rechi=(ss-m_MZ**2)*ss /BWD
      xe= Ve**2 +Ae**2
      xf= Vf**2 +Af**2
      ye= 2*Ve*Ae
      yf= 2*Vf*Af
      ff0= qe**2*qf**2 +2*rechi*qe*qf*Ve*Vf +chi2*xe*xf
      ff1=             +2*rechi*qe*qf*Ae*Af +chi2*ye*yf
      Born    = (1d0+ costhe**2 +amx2*(1d0-costhe**2))*ff0 +2d0*costhe*ff1
*     Colour factor
      Born = NCf*Born

      thresh = BetaFin
      IF(    svar .LE.  4d0*Massf**2) THEN
         thresh=0d0
      ELSE
         BetaFin = SQRT(1d0 -4d0*Massf**2/svar)
         thresh  = BetaFin
      ENDIF
c[[[
c      thresh=1d0
c]]]
      Born= Born*thresh
      END                       !!!! GPS_BornSimple


      SUBROUTINE GPS_MakeUW(Norm,ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// like GPS_MakeU  but with W v-a coupling included                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   U(2,2),UW0(2,2),WC(2,2),Cnor,Norm
         Cnor=Norm
         CALL GPS_MakeU(ph,Sigma,  p1,m1,   p2,m2,    UW0)
         CALL  GPS_MatrWm(WC)             ! W-e-nu couplingss
         CALL  GPS_times(Cnor,WC,UW0,U) ! we add v-a coupl
      END

      SUBROUTINE GPS_MakeVW(Norm,ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// like GPS_MakeV  but with W v-a coupling included                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   V(2,2),VW0(2,2),WC(2,2),Cnor,Norm
         Cnor=Norm
         CALL GPS_MakeV(ph,Sigma,  p1,m1,   p2,m2,    VW0)
         CALL  GPS_MatrW(WC)             ! W-e-nu couplingss
         CALL  GPS_times(Cnor,VW0,WC,V) ! we add v-a coupl
      END

 
      SUBROUTINE GPS_MakeUWb(Norm,ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// like GPS_MakeUb but with W v-a coupling included                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   U(2,2),UW0(2,2),WC(2,2),Cnor,Norm
         Cnor=Norm
         CALL GPS_MakeUb(ph,Sigma,  p1,m1,   p2,m2,    UW0)
         CALL  GPS_MatrWm(WC)             ! W-e-nu couplingss
         CALL  GPS_times(Cnor,WC,UW0,U) ! we add v-a coupl
      END

      SUBROUTINE GPS_MakeVWb(Norm,ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// like GPS_MakeVb but with W v-a coupling included                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////

      IMPLICIT NONE
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   V(2,2),VW0(2,2),WC(2,2),Cnor,Norm
         Cnor=Norm
         CALL GPS_MakeVb(ph,Sigma,  p1,m1,   p2,m2,    VW0)
         CALL  GPS_MatrW(WC)             ! W-e-nu couplingss
         CALL  GPS_times(Cnor,VW0,WC,V) ! we add v-a coupl
      END

      SUBROUTINE GPS_MakeU(ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix U, (epsilon-slash sandwiched between ubar-u spinors)        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   U(2,2)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE PRECISION       Sqr2
*-----------
      Sqr2 = DSQRT(2d0)
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         U(1,1) =  Sqr2*( GPS_XiProd(p2,ph) *GPS_iProd1(1,ph,p1))           ! (++)
         U(2,2) =  Sqr2*( GPS_XiProd(p1,ph) *GPS_iProd1(1,ph,p2))           ! (--)
         U(1,2) =  0d0                                                      ! (+-)
         U(2,1) =  Sqr2*( -m1*GPS_XiProd(p2,p1) +m2*GPS_XiProd(p1,p2))      ! (-+)
      ELSEIF(sigma. EQ. -1 ) THEN
         U(1,1) =  Sqr2*( GPS_XiProd(p1,ph) *GPS_iProd1(-1,ph,p2))          ! (++)
         U(2,2) =  Sqr2*( GPS_XiProd(p2,ph) *GPS_iProd1(-1,ph,p1))          ! (--)
         U(2,1) =  0d0                                                      ! (-+)
         U(1,2) =  Sqr2*( -m1*GPS_XiProd(p2,p1) +m2*GPS_XiProd(p1,p2))      ! (+-)
      ELSE
         Write(*,*) '++++++++ GPS_MakeU: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MakeU

      SUBROUTINE GPS_MakeV(ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix V, (epsilon-slash sandwiched between v vbar spinors)        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   V(2,2)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE PRECISION       Sqr2
*-----------
      Sqr2 = DSQRT(2d0)
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         V(2,2) =  Sqr2*( GPS_XiProd(p2,ph) *GPS_iProd1(1,ph,p1))            !(--)
         V(1,1) =  Sqr2*( GPS_XiProd(p1,ph) *GPS_iProd1(1,ph,p2))            !(++)
         V(2,1) =  0d0                                                       !(-+)
         V(1,2) =  Sqr2*( m1*GPS_XiProd(p2,p1) -m2*GPS_XiProd(p1,p2))        !(+-)
      ELSEIF(sigma. EQ. -1 ) THEN
         V(2,2) =  Sqr2*( GPS_XiProd(p1,ph) *GPS_iProd1(-1,ph,p2))           !(--)
         V(1,1) =  Sqr2*( GPS_XiProd(p2,ph) *GPS_iProd1(-1,ph,p1))           !(++)
         V(1,2) =  0d0                                                       !(+-)
         V(2,1) =  Sqr2*( m1*GPS_XiProd(p2,p1) -m2*GPS_XiProd(p1,p2))        !(-+)
      ELSE
         Write(*,*) '++++++++ GPS_MakeV: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MakeV


      SUBROUTINE GPS_MakeUb(ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix U, (epsilon-slash sandwiched between ubar-u spinors)        //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//   m_b     = auxiliary lightlike vector IS IMPLICIT INPUT                        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   U(2,2)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE COMPLEX   Cnor
*-----------
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1(-1,ph,m_b)
         U(1,1) = Cnor*(     GPS_iProd1(  1,p1, ph)*GPS_iProd1(-1,m_b,p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         U(2,2) = Cnor*(     GPS_iProd1( -1,p1,m_b)*GPS_iProd1( 1,ph, p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         U(1,2) = Cnor*(    +m1*GPS_XiProd( m_b,p1)*GPS_iProd1( 1,ph, p2)    !(+-)
     $                      +m2*GPS_XiProd( m_b,p2)*GPS_iProd1( 1,p1, ph) )
         U(2,1) = Cnor*(    +m1*GPS_XiProd(  ph,p1)*GPS_iProd1(-1,m_b,p2)    !(-+)
     $                      +m2*GPS_XiProd(  ph,p2)*GPS_iProd1(-1,p1,m_b) )
      ELSEIF(sigma. EQ. -1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1( 1,ph,m_b)
         U(1,1) = Cnor*(     GPS_iProd1(  1,p1,m_b)*GPS_iProd1(-1,ph, p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         U(2,2) = Cnor*(     GPS_iProd1( -1,p1, ph)*GPS_iProd1( 1,m_b,p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         U(2,1) = Cnor*(    +m1*GPS_XiProd( m_b,p1)*GPS_iProd1(-1,ph, p2)    !(-+)
     $                      +m2*GPS_XiProd( m_b,p2)*GPS_iProd1(-1,p1, ph) )
         U(1,2) = Cnor*(    +m1*GPS_XiProd(  ph,p1)*GPS_iProd1( 1,m_b,p2)    !(+-)
     $                      +m2*GPS_XiProd(  ph,p2)*GPS_iProd1( 1,p1,m_b) )
      ELSE
         Write(*,*) '++++++++ GPS_MakeUb: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MakeUb

      SUBROUTINE GPS_MakeVb(ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix V, (epsilon-slash sandwiched between vbar-v spinors)        //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//   m_b     = auxiliary lightlike vector IS IMPLICIT INPUT                        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   V(2,2)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE COMPLEX   Cnor
*-----------
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1(-1,ph,m_b)
         V(2,2) = Cnor*(     GPS_iProd1(  1,p1, ph)*GPS_iProd1(-1,m_b,p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         V(1,1) = Cnor*(     GPS_iProd1( -1,p1,m_b)*GPS_iProd1( 1,ph, p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         V(2,1) = Cnor*(    -m1*GPS_XiProd( m_b,p1)*GPS_iProd1( 1,ph, p2)    !(-+)
     $                      -m2*GPS_XiProd( m_b,p2)*GPS_iProd1( 1,p1, ph) )
         V(1,2) = Cnor*(    -m1*GPS_XiProd(  ph,p1)*GPS_iProd1(-1,m_b,p2)    !(+-)
     $                      -m2*GPS_XiProd(  ph,p2)*GPS_iProd1(-1,p1,m_b) )
      ELSEIF(sigma. EQ. -1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1( 1,ph,m_b)
         V(2,2) = Cnor*(     GPS_iProd1(  1,p1,m_b)*GPS_iProd1(-1,ph, p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         V(1,1) = Cnor*(     GPS_iProd1( -1,p1, ph)*GPS_iProd1( 1,m_b,p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         V(1,2) = Cnor*(    -m1*GPS_XiProd( m_b,p1)*GPS_iProd1(-1,ph, p2)    !(+-)
     $                      -m2*GPS_XiProd( m_b,p2)*GPS_iProd1(-1,p1, ph) )
         V(2,1) = Cnor*(    -m1*GPS_XiProd(  ph,p1)*GPS_iProd1( 1,m_b,p2)    !(-+)
     $                      -m2*GPS_XiProd(  ph,p2)*GPS_iProd1( 1,p1,m_b) )
      ELSE
         Write(*,*) '++++++++ GPS_MakeVb: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MakeVb




      SUBROUTINE GPS_MakeUX(Cn,ph,mh,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix U, (ph-slash sandwiched between ubar-u spinors)             //
*//   mass terms not tested   !!!                                                   //
*//   ph      = photon  4-momentum                                                  //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//   m_b     = auxiliary lightlike vector IS IMPLICIT INPUT                        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),mh,p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   U(2,2),A(2,2),B(2,2),AB(2,2),C(2,2)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE COMPLEX   Cn,Cnor
*-----------
      CALL GPS_Initialize
      Cnor=1D0
*-----------
      CALL GPS_Initialize
      CALL  GPS_MatrS(p1,m1,ph,mh,A)
      CALL  GPS_MatrWm(C)
      CALL  GPS_MatrS(ph,mh,p2,m2,B)
      CALL  GPS_times(Cnor,A,B,AB)
      CALL  GPS_times(Cn,C,AB,U)
      END                       !!!! GPS_MakeUb

      SUBROUTINE GPS_MakeVX(Cn,ph,mh,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix V, (ph-slash sandwiched between vbar-v spinors)             //
*//   mass terms not studied !!!                                                    //
*//   ph      = photon  4-momentum                                                  //
*//   mh      = photon  mass                                                        //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//   massles limit                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),mh,p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   V(2,2),A(2,2),B(2,2),C(2,2),AB(2,2)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE COMPLEX   Cn,Cnor,XX
      Cnor=1D0
*-----------
      CALL GPS_Initialize
      CALL  GPS_MatrS(p1,m1,ph,mh,A)
      CALL  GPS_MatrWm(C)
      CALL  GPS_MatrS(ph,mh,p2,m2,B)
      CALL  GPS_times(Cnor,A,B,AB)
      CALL  GPS_times(Cn,AB,C,V)
      XX=V(1,1)
      V(1,1)=V(2,2)
      V(2,2)=XX
      END                       !!!! GPS_MakeVb




      SUBROUTINE GPS_MatrU(Cfact,ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix U, (epsilon-slash sandwiched between ubar-u spinors)        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,U(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Norm
*-----------
      Norm = DSQRT(2d0)*Cfact
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         U(1,1) =  Norm*( GPS_XiProd(p2,ph) *GPS_iProd1(1,ph,p1))           ! (++)
         U(2,2) =  Norm*( GPS_XiProd(p1,ph) *GPS_iProd1(1,ph,p2))           ! (--)
         U(1,2) =  0d0                                                      ! (+-)
         U(2,1) =  Norm*( -m1*GPS_XiProd(p2,p1) +m2*GPS_XiProd(p1,p2))      ! (-+)
      ELSEIF(sigma. EQ. -1 ) THEN
         U(1,1) =  Norm*( GPS_XiProd(p1,ph) *GPS_iProd1(-1,ph,p2))          ! (++)
         U(2,2) =  Norm*( GPS_XiProd(p2,ph) *GPS_iProd1(-1,ph,p1))          ! (--)
         U(2,1) =  0d0                                                      ! (-+)
         U(1,2) =  Norm*( -m1*GPS_XiProd(p2,p1) +m2*GPS_XiProd(p1,p2))      ! (+-)
      ELSE
         Write(*,*) '++++++++ GPS_MatrU: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MatrU

      SUBROUTINE GPS_MatrV(Cfact,ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix V, (epsilon-slash sandwiched between v vbar spinors)        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,V(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Norm
*-----------
      Norm = DSQRT(2d0)*Cfact
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         V(2,2) =  Norm*( GPS_XiProd(p2,ph) *GPS_iProd1(1,ph,p1))            !(--)
         V(1,1) =  Norm*( GPS_XiProd(p1,ph) *GPS_iProd1(1,ph,p2))            !(++)
         V(2,1) =  0d0                                                       !(-+)
         V(1,2) =  Norm*( m1*GPS_XiProd(p2,p1) -m2*GPS_XiProd(p1,p2))        !(+-)
      ELSEIF(sigma. EQ. -1 ) THEN
         V(2,2) =  Norm*( GPS_XiProd(p1,ph) *GPS_iProd1(-1,ph,p2))           !(--)
         V(1,1) =  Norm*( GPS_XiProd(p2,ph) *GPS_iProd1(-1,ph,p1))           !(++)
         V(1,2) =  0d0                                                       !(+-)
         V(2,1) =  Norm*( m1*GPS_XiProd(p2,p1) -m2*GPS_XiProd(p1,p2))        !(-+)
      ELSE
         Write(*,*) '++++++++ GPS_MatrV: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MatrV


      SUBROUTINE GPS_MatrUb(Cfact,ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix U, (epsilon-slash sandwiched between ubar-u spinors)        //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//   m_b     = auxiliary lightlike vector IS IMPLICIT INPUT                        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,U(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Cnor
*-----------
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1(-1,ph,m_b)*Cfact
         U(1,1) = Cnor*(     GPS_iProd1(  1,p1, ph)*GPS_iProd1(-1,m_b,p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         U(2,2) = Cnor*(     GPS_iProd1( -1,p1,m_b)*GPS_iProd1( 1,ph, p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         U(1,2) = Cnor*(    +m1*GPS_XiProd( m_b,p1)*GPS_iProd1( 1,ph, p2)    !(+-)
     $                      +m2*GPS_XiProd( m_b,p2)*GPS_iProd1( 1,p1, ph) )
         U(2,1) = Cnor*(    +m1*GPS_XiProd(  ph,p1)*GPS_iProd1(-1,m_b,p2)    !(-+)
     $                      +m2*GPS_XiProd(  ph,p2)*GPS_iProd1(-1,p1,m_b) )
      ELSEIF(sigma. EQ. -1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1( 1,ph,m_b)*Cfact
         U(1,1) = Cnor*(     GPS_iProd1(  1,p1,m_b)*GPS_iProd1(-1,ph, p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         U(2,2) = Cnor*(     GPS_iProd1( -1,p1, ph)*GPS_iProd1( 1,m_b,p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         U(2,1) = Cnor*(    +m1*GPS_XiProd( m_b,p1)*GPS_iProd1(-1,ph, p2)    !(-+)
     $                      +m2*GPS_XiProd( m_b,p2)*GPS_iProd1(-1,p1, ph) )
         U(1,2) = Cnor*(    +m1*GPS_XiProd(  ph,p1)*GPS_iProd1( 1,m_b,p2)    !(+-)
     $                      +m2*GPS_XiProd(  ph,p2)*GPS_iProd1( 1,p1,m_b) )
      ELSE
         Write(*,*) '++++++++ GPS_MatrUb: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MatrUb

      SUBROUTINE GPS_MatrVb(Cfact,ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Transition matrix V, (epsilon-slash sandwiched between vbar-v spinors)        //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   ph      = photon  4-momentum                                                  //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//   m_b     = auxiliary lightlike vector IS IMPLICIT INPUT                        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,V(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Cnor
*-----------
      CALL GPS_Initialize
      IF(     sigma. EQ. 1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1(-1,ph,m_b)*Cfact
         V(2,2) = Cnor*(     GPS_iProd1(  1,p1, ph)*GPS_iProd1(-1,m_b,p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         V(1,1) = Cnor*(     GPS_iProd1( -1,p1,m_b)*GPS_iProd1( 1,ph, p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         V(2,1) = Cnor*(    -m1*GPS_XiProd( m_b,p1)*GPS_iProd1( 1,ph, p2)    !(-+)
     $                      -m2*GPS_XiProd( m_b,p2)*GPS_iProd1( 1,p1, ph) )
         V(1,2) = Cnor*(    -m1*GPS_XiProd(  ph,p1)*GPS_iProd1(-1,m_b,p2)    !(+-)
     $                      -m2*GPS_XiProd(  ph,p2)*GPS_iProd1(-1,p1,m_b) )
      ELSEIF(sigma. EQ. -1 ) THEN
         Cnor   = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1( 1,ph,m_b)*Cfact
         V(2,2) = Cnor*(     GPS_iProd1(  1,p1,m_b)*GPS_iProd1(-1,ph, p2)    !(--)
     $                +m1*m2*GPS_XiProd(    m_b,p2)*GPS_XiProd(   ph, p1) )
         V(1,1) = Cnor*(     GPS_iProd1( -1,p1, ph)*GPS_iProd1( 1,m_b,p2)    !(++)
     $                +m1*m2*GPS_XiProd(    m_b,p1)*GPS_XiProd(   ph, p2) )
         V(1,2) = Cnor*(    -m1*GPS_XiProd( m_b,p1)*GPS_iProd1(-1,ph, p2)    !(+-)
     $                      -m2*GPS_XiProd( m_b,p2)*GPS_iProd1(-1,p1, ph) )
         V(2,1) = Cnor*(    -m1*GPS_XiProd(  ph,p1)*GPS_iProd1( 1,m_b,p2)    !(-+)
     $                      -m2*GPS_XiProd(  ph,p2)*GPS_iProd1( 1,p1,m_b) )
      ELSE
         Write(*,*) '++++++++ GPS_MatrVb: WRONG sigma= ',sigma
      ENDIF
      END                       !!!! GPS_MatrVb

      SUBROUTINE GPS_MatrS(p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   S matrix for spinor products (matrix version of  GPS_iProd1                   //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses   (not used at the moment massless case taken        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,V(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Cnor
*-----------
      CALL GPS_Initialize
         V(1,2) =      GPS_iProd1(  1,p1, p2)   !(+-)
         V(2,1) =      GPS_iProd1( -1,p1,p2)    !(-+)
         V(2,2) = 0D0                            !(--)
         V(1,1) = 0D0                            !(++)

      END                       !!!! GPS_MatrS
      SUBROUTINE GPS_MatrW(V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   W coupling vertes in spinor indices   V-case                                  //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,V(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Cnor
*-----------
      CALL GPS_Initialize
         Cnor   = sqrt(1.d0/2.d0/m_Sw2)
         V(1,2) = Cnor*0D0 !(+-)
         V(2,1) = Cnor*0D0 !(-+)
         V(2,2) = Cnor*0D0 !(--)
         V(1,1) = Cnor*1D0 !(++)

      END                       !!!! GPS_MatrW
      SUBROUTINE GPS_MatrWm(V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   W coupling vertes in spinor indices U-case                                    //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER           sigma,l1,l2
      DOUBLE PRECISION  ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX    Cfact,V(2,2)
      DOUBLE COMPLEX    GPS_iProd1
      DOUBLE PRECISION  GPS_XiProd
      DOUBLE COMPLEX    Cnor
*-----------
      CALL GPS_Initialize
         Cnor   = sqrt(1.d0/2.d0/m_Sw2)
         V(1,2) = Cnor*0D0 !(+-)
         V(2,1) = Cnor*0D0 !(-+)
         V(2,2) = Cnor*1D0 !(--)
         V(1,1) = Cnor*0D0 !(++)

      END                       !!!! GPS_MatrWm




      SUBROUTINE GPS_MakeUE(ph,sigma,p1,m1,p2,m2,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Alternative construction of U using MultiE, essentialy for tests.             //
*//   Note that one cannot input Xi into MultiE                                     //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      INCLUDE 'GPS.h'
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   U(2,2)
      DOUBLE COMPLEX   A1(2,2),A2(2,2)
      DOUBLE PRECISION       eps
      INTEGER     i,j,k
      DOUBLE COMPLEX   Csum,Norm
      DOUBLE COMPLEX   GPS_iProd1
*-----------
      CALL GPS_Initialize
      eps = 1d-100
      CALL GPS_MultiE(      1,  p1, m1,  p1, m1,   sigma, m_b,eps,  ph,eps,   A1)
      CALL GPS_MultiE(  sigma,  ph,eps, m_b,eps,       1,  p2, m2,  p2, m2,   A2)
      Norm = DSQRT(2d0) /GPS_iProd1(-sigma,ph,m_b)
      CALL  GPS_times(Norm,A1,A2,U)
      END                       !!!!  GPS_MakeUE

      SUBROUTINE GPS_MakeVE(ph,sigma,p1,m1,p2,m2,V)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Alternative construction of V using MultiE, essentialy for tests.             //
*//   Note that one cannot input Xi into MultiE                                     //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      INCLUDE 'GPS.h'
      INTEGER     sigma,l1,l2
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   V(2,2)
      DOUBLE COMPLEX   A1(2,2),A2(2,2)
      DOUBLE PRECISION       eps
      INTEGER     i,j,k
      DOUBLE COMPLEX   Csum,Norm
      DOUBLE COMPLEX   GPS_iProd1
*-----------
      CALL GPS_Initialize
      eps = 1d-100
      CALL GPS_MultiE(      1,  p1,-m1,  p1,-m1,   sigma, m_b,eps,  ph,eps,   A1)
      CALL GPS_MultiE(  sigma,  ph,eps, m_b,eps,       1,  p2,-m2,  p2,-m2,   A2)
      Norm = DSQRT(2d0) /GPS_iProd1(-sigma,ph,m_b)
      CALL  GPS_times(Norm,A1,A2,V)
      END                       !!!!  GPS_MakeVE

      SUBROUTINE GPS_times(C,A1,A2,Result)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Matrix multiplication, Warning: Result cannot be the same as A1 or A2 !!!!    //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      DOUBLE COMPLEX   C,A1(2,2),A2(2,2),Result(2,2)
      INTEGER     i,j,k
      DOUBLE COMPLEX   Csum
*-----------
      DO i=1,2
         DO j=1,2
            Csum= DCMPLX(0d0,0d0)
            DO k=1,2
               Csum= Csum +A1(i,k)*A2(k,j)
            ENDDO
            Result(i,j)=Csum*C
         ENDDO
      ENDDO
      END                       !!!!  GPS_times

      SUBROUTINE GPS_sum(C,A1,A2,Result)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Matrix summation, Warning: C multiplies second matrix                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      DOUBLE COMPLEX  A1(2,2),A2(2,2),Result(2,2)
      INTEGER     i,j,k
      DOUBLE PRECISION   C
*-----------
      DO i=1,2
         DO j=1,2
            Result(i,j)=A1(i,j)+ A2(i,j)*C
         ENDDO
      ENDDO
      END                       !!!!  GPS_sum


      SUBROUTINE GPS_MultiE(rho, a,ma,b,mb, mu,c,mc,d,md, Matrix)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Clever routine Multi introduced by Elzbieta Richter-Was.                      //
*//   Simplified version.                                                           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     rho,mu
      DOUBLE PRECISION       a(4),ma, b(4),mb, c(4),mc, d(4),md
      DOUBLE COMPLEX   Matrix(2,2)
      DOUBLE COMPLEX   GPS_iProd2
*------------------------------
      Matrix(1,1) = GPS_iProd2(  rho,a,ma,   mu,c,mc)
      Matrix(1,2) = GPS_iProd2(  rho,a,ma,  -mu,d,md)
      Matrix(2,1) = GPS_iProd2( -rho,b,mb,   mu,c,mc)
      Matrix(2,2) = GPS_iProd2( -rho,b,mb,  -mu,d,md)
*------------------------------
      END                       !!!! GPS_MultiE



      DOUBLE COMPLEX FUNCTION GPS_Sof1(sigma,ph,pf)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Single soft photon contribution to Soft factor at amplitude level             //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   ph      = photon  4-momentum                                                  //
*//   pf      = fermion 4-momenta                                                   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER              sigma
      DOUBLE PRECISION     ph(4),pf(4)
      DOUBLE COMPLEX       GPS_iProd1
      DOUBLE PRECISION     GPS_XiProd
*
      GPS_Sof1 =  
     $     DSQRT(2d0)*GPS_iProd1(sigma,ph,pf)*GPS_XiProd(pf,ph)
     $     /(2d0*(pf(4)*ph(4)-pf(3)*ph(3)-pf(2)*ph(2)-pf(1)*ph(1))) !
      END

      DOUBLE COMPLEX FUNCTION GPS_Sof1b(sigma,ph,pf,mf)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Single soft photon contribution to Soft factor at amplitude level             //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   ph      = photon  4-momentum                                                  //
*//   pf      = fermion 4-momentum                                                  //
*//   mf      = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER           sigma
      DOUBLE PRECISION  ph(4),pf(4),mf
      DOUBLE COMPLEX    GPS_bfacb
*
      GPS_Sof1b =
     $     GPS_bfacb(sigma,ph,pf,mf)
     $     /(2*( pf(4)*ph(4)-pf(3)*ph(3)-pf(2)*ph(2)-pf(1)*ph(1) ))
      END

      DOUBLE COMPLEX FUNCTION GPS_Sof1x(sigma,ph,pf)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Single soft photon contribution to Soft factor at amplitude level             //
*//   no propagator                                                                 //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   ph      = photon  4-momentum                                                  //
*//   pf      = fermion 4-momenta                                                   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER              sigma
      DOUBLE PRECISION     ph(4),pf(4)
      DOUBLE COMPLEX       GPS_iProd1
      DOUBLE PRECISION     GPS_XiProd
*
      GPS_Sof1x =  
     $     DSQRT(2d0)*GPS_iProd1(sigma,ph,pf)*GPS_XiProd(pf,ph)
      END

      DOUBLE COMPLEX FUNCTION GPS_Sof1bx(sigma,ph,pf,mf)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Single soft photon contribution to Soft factor at amplitude level             //
*//   no propagator                                                                 //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   ph      = photon  4-momentum                                                  //
*//   pf      = fermion 4-momentum                                                  //
*//   mf      = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER           sigma
      DOUBLE PRECISION  ph(4),pf(4),mf
      DOUBLE COMPLEX    GPS_bfacb
*
      GPS_Sof1bx =
     $     GPS_bfacb(sigma,ph,pf,mf)
!     $     /(2*( pf(4)*ph(4)-pf(3)*ph(3)-pf(2)*ph(2)-pf(1)*ph(1) ))
      END




      DOUBLE COMPLEX  FUNCTION GPS_soft(sigma,ph,p1,p2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   2-fermion Soft factor at amplitude level                                      //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   ph      = photon  4-momentum                                                  //
*//   p1,p2   = fermion 4-momenta                                                   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     sigma
      DOUBLE PRECISION       ph(4),p1(4),p2(4)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE COMPLEX   Soft,bf1,bf2
      DOUBLE PRECISION       pk1,pk2
      DOUBLE PRECISION       Sqr2
*------
      Sqr2 = DSQRT(2d0)
      CALL GPS_Initialize
      pk1 = p1(4)*ph(4)-p1(3)*ph(3)-p1(2)*ph(2)-p1(1)*ph(1)
      pk2 = p2(4)*ph(4)-p2(3)*ph(3)-p2(2)*ph(2)-p2(1)*ph(1)
      bf1 =  Sqr2*GPS_iProd1(sigma,ph,p1)*GPS_XiProd(p1,ph) !!! =GPS_bfact(sigma,ph,p1)
      bf2 =  Sqr2*GPS_iProd1(sigma,ph,p2)*GPS_XiProd(p2,ph) !!! =GPS_bfact(sigma,ph,p2)
      Soft = -bf1/(2*pk1) +bf2/(2*pk2)
      GPS_soft = Soft
      END

      DOUBLE COMPLEX  FUNCTION GPS_bfact(sigma,phot,pferm)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//     diagonal element of U-matrix for massive fermion = denominator in s-factor  //
*//     sigma  = photon polarization (+1,-1)                                        //
*//     phot   = photon 4-momentum                                                  //
*//     pferm  = fermion 4-momentum                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     sigma
      DOUBLE PRECISION       phot(4), pferm(4)
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
*---------------------------
      CALL GPS_Initialize
      GPS_bfact =  DSQRT(2d0)*GPS_iProd1(sigma,phot,pferm)*GPS_XiProd(pferm,phot)
      END


      DOUBLE COMPLEX  FUNCTION GPS_softb(sigma,ph,p1,m1,p2,m2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Soft factor at amplitude level                                                //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   sigma   = photon polarization (+1,-1)                                         //
*//   ph      = photon  4-momentum                                                  //
*//   p1,p2   = fermion 4-momenta                                                   //
*//   m1,m2   = fermion masses                                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     sigma
      DOUBLE PRECISION       ph(4),p1(4),m1,p2(4),m2
      DOUBLE COMPLEX   GPS_bfacb
      DOUBLE COMPLEX   bf1,bf2
      DOUBLE PRECISION       pk1,pk2
*------
      CALL GPS_Initialize
      pk1 = p1(4)*ph(4)-p1(3)*ph(3)-p1(2)*ph(2)-p1(1)*ph(1)
      pk2 = p2(4)*ph(4)-p2(3)*ph(3)-p2(2)*ph(2)-p2(1)*ph(1)
      bf1 = GPS_bfacb(sigma,ph,p1,m1)
      bf2 = GPS_bfacb(sigma,ph,p2,m2)
      GPS_softb = -bf1/(2*pk1) +bf2/(2*pk2)
      END


      DOUBLE COMPLEX  FUNCTION GPS_bfacb(sigma,phot,pferm,mass)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   diagonal element of U-matrix for massive fermion = denominator in s-factor    //
*//   dependend of vector auxial-gauge vector b=beta=m_b !!!                        //
*//   sigma  = photon polarization (+1,-1)                                          //
*//   phot   = photon 4-momentum                                                    //
*//   pferm  = fermion 4-momentum                                                   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER     sigma
      DOUBLE PRECISION       phot(4), pferm(4), mass
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
*---------------------------
      CALL GPS_Initialize
      GPS_bfacb = DCMPLX(DSQRT(2d0),0d0)/GPS_iProd1(-sigma,phot,m_b)
     $        *(     GPS_iProd1( -sigma,m_b,pferm) *GPS_iProd1( sigma, pferm, phot)
     $          +mass**2 *GPS_XiProd(   m_b,pferm) *GPS_XiProd(        phot, pferm) )
      END

      DOUBLE COMPLEX  FUNCTION GPS_iProd1(L,p,q)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// This is basic inner s-product of spinors s_{L}(p,q)=ubar_{L}(p)*u_{-L}(q)       //
*// We exploit identity s_{-}(p,q) = -[s_{+}(p,q)]^*                                //
*// Four-vectors p,q are in principle massless, however, massive p,q                //
*// are also meaningfull. Implicit projection on xi is then assumed.                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      INTEGER     L
      DOUBLE PRECISION       p(4), q(4)
      DOUBLE COMPLEX   Prod

      IF(     L .EQ.  1 ) THEN
         Prod=
     $        -DSQRT( (p(4)-p(1)) / (q(4)-q(1)) ) *DCMPLX(q(2),q(3))
     $        +DSQRT( (q(4)-q(1)) / (p(4)-p(1)) ) *DCMPLX(p(2),p(3))
      ELSEIF( L .EQ. -1 ) THEN
         Prod=
     $        -DSQRT( (q(4)-q(1)) / (p(4)-p(1)) ) *DCMPLX(p(2),p(3))
     $        +DSQRT( (p(4)-p(1)) / (q(4)-q(1)) ) *DCMPLX(q(2),q(3))
         Prod= DCONJG(Prod)
      ELSE
         WRITE(*,*) '##### GPS_iProd1: Wrong L= ', L
      ENDIF
      GPS_iProd1 =  Prod
      END       

      DOUBLE COMPLEX  FUNCTION GPS_iProd2(Lamp,p,mp,Lamq,q,mq)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// General spinor product s_{l1,l2}(p,q) for massive spinors u and/or v            //
*// mp and mq are masses of p and q. Negative mass means antiparticle!              //
*//                                                                                 //
*// NOTES:                                                                          //
*// Antiparticle, v-spinor, is recognized according to sign of mass.                //
*// Spin sign is flipped for v-spinor.                                              //
*// This requires attention for zero-mass because zero mass particle is recognized  //
*// as u-spinor (no flip of spin).                                                  //
*// The way out is to give to antiparticle very small mass like -1e-150.            //
*//                                                                                 //
*// Note also that mp=0 or mq=0 for massive p, q, mean that p, q                    //
*// are projected on xi.                                                            //
*//                                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER     Lamp,Lamq
      DOUBLE PRECISION       p(4),q(4),mp,mq
      DOUBLE COMPLEX   GPS_iProd1
      DOUBLE PRECISION       GPS_XiProd
      DOUBLE COMPLEX   Prod
      INTEGER     Lp,Lq

      Lp = Lamp
      Lq = Lamq
* Helicity for v-spinor is fliped
      IF( mp .LT. 0d0) Lp = -Lp
      IF( mq .LT. 0d0) Lq = -Lq
* Helicity conservation and non-conservation separately
      IF(     Lp .EQ. -Lq ) THEN
         Prod = GPS_iProd1(Lp,p,q)
      ELSEIF( Lp .EQ.  Lq ) THEN
         Prod = DCMPLX( mp*GPS_XiProd(q,p) +mq*GPS_XiProd(p,q), 0d0)
      ELSE
         WRITE(*,*) '##### GPS_iProd2: Wrong Lp,Lq= ', Lp,Lq
      ENDIF
      GPS_iProd2 = Prod
      END       

      DOUBLE PRECISION  FUNCTION GPS_XiProd(p,q)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   auxiliary function called in GPS_iProd2                                       //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  p(4),q(4)

      GPS_XiProd = DSQRT( (p(4)-p(1)) / (q(4)-q(1)) )
      END

      SUBROUTINE GPS_BornPrint(nout,Mode)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Printout of Born spin amplitudes ina a nice format                      //
*//   Mode=0    prints AmpBorn, principal Born spin amplitudes                //
*//   Mode=1,2  for test printouts (to be kicked out!!!!!)                    //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER nout,Mode
*
      INTEGER j1,j2,j3,j4
*----------------------------------------
      WRITE(nout,*) ' '
      WRITE(nout,'(5a)') '++++++++++++++++++++++++++++++++++++',
     $                        ' Born spin amplitudes ',
     $                   '++++++++++++++++++++++++++++++++++++'
      DO j1=1,2
         DO j2=1,2
            WRITE(nout,'(a,4(a,4i2,a))')  '     ',
     $           (('{', 3-2*j1, 3-2*j2, 3-2*j3 , 3-2*j4 ,'}  ', j3=1,2),j4=1,2)
         ENDDO
      ENDDO
      DO j1=1,2
         DO j2=1,2
            IF(Mode .EQ. 0) THEN
               WRITE(nout,'(4(a,2f10.6,a))') 
     $              (('[',m_AmpBorn(j1,j2,j3,j4),'] ', j3=1,2),j4=1,2)
            ELSEIF(Mode .EQ. 1) THEN
               WRITE(nout,'(4(a,2f10.6,a))') 
     $              (('[',m_AmpBorn1(j1,j2,j3,j4),'] ', j3=1,2),j4=1,2)
            ELSEIF(Mode .EQ. 2) THEN
               WRITE(nout,'(4(a,2f10.6,a))') 
     $              (('[',m_AmpBorn2(j1,j2,j3,j4),'] ', j3=1,2),j4=1,2)
            ENDIF
         ENDDO
      ENDDO
      WRITE(nout,'(120i1)') (mode, j1=1,91)
      END

      SUBROUTINE GPS_BPrint(nout,word,AmpBorn)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Printout of Born 16 spin amplitudes in  a nice format                   //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER      nout
      CHARACTER*8  word
      DOUBLE COMPLEX    AmpBorn(2,2,2,2)
      DOUBLE PRECISION        Sum
*
      INTEGER     j1,j2,j3,j4
*----------------------------------------
      WRITE(nout,'(a)') ' '
      WRITE(nout,'(4a)') '+++++++++++++++++++++++++++++++++++++++++++++',
     $                             ' Born amplits: ', word,
     $                  ' +++++++++++++++++++++++++++++++++++++++++++++'
***      DO j1=1,2
***         DO j2=1,2
***            WRITE(*,'(a,4(a,4i2,a))')  '     ',
***     $           (('{', 3-2*j1, 3-2*j2, 3-2*j3 , 3-2*j4 ,'}  ', j3=1,2),j4=1,2)
***         ENDDO
***      ENDDO
      DO j1=1,2
         DO j2=1,2
*@@@@       WRITE(nout,'(4(a,2f10.6,a))') 
            WRITE(nout,'(4(a,2g14.6,a))') 
     $           (('[',AmpBorn(j1,j2,j3,j4),'] ', j3=1,2),j4=1,2)
         ENDDO
      ENDDO
      Sum=0d0
      DO j1=1,2
         DO j2=1,2
            DO j3=1,2
               DO j4=1,2
                  Sum=Sum+CDABS(AmpBorn(j1,j2,j3,j4))**2
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      WRITE(nout,'(a,g20.12)') '++++++++++ Sum= ',Sum
      END

      SUBROUTINE GPS_RmatPrint(nout,word)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Born similar as in BornV, mass terms exact for photon exchange!         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER      nout
      CHARACTER*8  word
*
      INTEGER    k,l
*----------------
      WRITE(nout,'(a)') ' '
      WRITE(nout,'(4a)') '#####################################',
     $                    ' R-matrix ', word,
     $                   ' #####################################'
      DO k=0,3
         WRITE(nout,'(4(a,2f10.6,a))')  ('[',m_Rmat(k,l),'] ', l=0,3)
      ENDDO
*----------------
      END

      SUBROUTINE GPS_UPrint(nout,word,U)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Print U-matrix in a nice form                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      DOUBLE COMPLEX  U(2,2)
      INTEGER k,l
      CHARACTER*8  word
*
      WRITE(nout,'(a)') '  '
      WRITE(nout,'(4a)') '////////////////////////////////////////',
     $                             ' T-matrix: ',word,
     $                  ' ////////////////////////////////////////'
      DO k=1,2
         WRITE(nout,'(2(a,2f20.12,a))')  ('[',U(k,l),' ]     ', l=1,2)
      ENDDO
      END


      SUBROUTINE GPS_Amp1Print(nout,word,Amp1Phot)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Print 1-photon 32 spin amplitudes in a readible format on unit nout           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      DOUBLE COMPLEX    Amp1Phot(2,2,2,2,2)
      CHARACTER*8  word
      INTEGER      j1,j2,j3,j4,k
      DOUBLE PRECISION        Sum
*
      WRITE(nout,'(a)') ' '
      WRITE(nout,'(4a)') '++++++++++++++++++++++++++++++++++',
     $                    ' 1-phot amplits: ',word,
     $                  ' ++++++++++++++++++++++++++++++++++'
      DO j1=1,2
         DO j2=1,2
            WRITE(*,'(a,4(a,4i2,a))')  '     ',
     $           (('{', 3-2*j1, 3-2*j2, 3-2*j3 , 3-2*j4 ,'}  ', j3=1,2),j4=1,2)
         ENDDO
      ENDDO
      DO k=1,2
         DO j1=1,2
            DO j2=1,2
****               WRITE(nout,'(4(a,2f14.8,a))') 
               WRITE(nout,'(4(a,2g14.6,a))') 
     $              (('[',Amp1Phot(j1,j2,j3,j4,k),'] ', j3=1,2),j4=1,2)
            ENDDO
         ENDDO
      ENDDO
      Sum=0d0
      DO k=1,2
         DO j1=1,2
            DO j2=1,2
               DO j3=1,2
                  DO j4=1,2
                     Sum=Sum+ CDABS(Amp1Phot(j1,j2,j3,j4,k))**2
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO
***      WRITE(nout,'(a,f20.12)') '++++++++++ Sum= ',Sum
      WRITE(nout,'(a,g20.12)') '++++++++++ Sum= ',Sum
      END
            
*/////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//           Setters and Getters of CLASS  GPS                                     //
*//           Setters and Getters of CLASS  GPS                                     //
*//           Setters and Getters of CLASS  GPS                                     //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE GPS_GetXi(xi,eta)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   xi is basic lightlike vector in LAB frame entering definition of all spinors  //
*//   called k0 in Kleiss Stirling papers                                           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      DOUBLE PRECISION  xi(4),eta(4)
      INTEGER k
*--------------
      DO k=1,4
         xi(k)  =  m_Xi(k)
         eta(k) =  m_Eta(k)
      ENDDO
      END

      SUBROUTINE GPS_SetKeyArb(KeyArb)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  KeyArb is for switching on/off the use of m_b,   KeyArb=0 means b=Xi           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER KeyArb
      m_KeyArb = KeyArb
      END

      SUBROUTINE GPS_GetKeyArb(KeyArb)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  KeyArb is for switching on/off the use of m_b,   KeyArb=0 means b=Xi           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER KeyArb
      KeyArb = m_KeyArb
      END

      SUBROUTINE GPS_Setb1
*/////////////////////////////////////////////////////////////////////////////////////
*//   b1-->b                                                                        //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER k
      DO k=1,4
         m_b( k) = m_b1( k)
      ENDDO
      END

      SUBROUTINE GPS_Setb2
*/////////////////////////////////////////////////////////////////////////////////////
*//   b2-->b                                                                        //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER k
      DO k=1,4
         m_b( k) = m_b2( k)
      ENDDO
      END

      SUBROUTINE GPS_Setb3
*/////////////////////////////////////////////////////////////////////////////////////
*//   b3-->b                                                                        //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER k
      DO k=1,4
         m_b( k) = m_b3( k)
      ENDDO
      END

      SUBROUTINE GPS_GetRmat(Rmat)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Export m_Rmat to outside world                                                //
*//   Note that Rmat is REAL while m_Rmat is still COMPLEX                          //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      DOUBLE PRECISION      sum,sum1,Rmat(0:3,0:3)
      INTEGER    i,j
*-----------------------------------
* Save old result
      DO i=0,3
         DO j=0,3
            Rmat(i,j)=m_Rmat(i,j)
         ENDDO
      ENDDO
      END

      SUBROUTINE GPS_GetDebg(j,y)
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      DOUBLE PRECISION  y
      INTEGER j
*---------------------------------------------------------------
      y = m_Debg(j)
      END   ! GPS_GetDebg


      SUBROUTINE GPS_SetDebg(j,y)
*////////////////////////////////////////////////////////////////////////////////
*//                                                                            //
*////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
*
      DOUBLE PRECISION  y
      INTEGER j
*---------------------------------------------------------------
      IF(j.LT.0 .OR. j.GT.200) THEN
         WRITE(*,*) ' STOP in GPS_SetDebg: j= ',j
         STOP
      ENDIF
      m_Debg(j) =y
      END   ! GPS_SetDebg


      SUBROUTINE GPS_ZeroWtSet
*/////////////////////////////////////////////////////////////////////////////////////
*//   Zeroing weght list                                                            //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER  j
*
      DO j=1,m_lenwt
         m_WtSet(j)=0d0
      ENDDO
      END   ! GPS_ZeroWtSet

      SUBROUTINE GPS_GetWtSet(WtBest,WtSet)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Export list of weights                                                        //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION    WtBest,WtSet(*)
      INCLUDE 'GPS.h'
*
      INTEGER  j
*--------------------------------------------------------------
      WtBest = m_WtBest
* collection of all weights
      DO j=1,m_lenwt
         WtSet(j)= m_WtSet(j)
      ENDDO
      END                       !!!GPS_GetWtSet!!!

      SUBROUTINE GPS_SetKeyINT(KeyINT)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Set INTerference ISR*FSR switch                                               //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER KeyINT
*
      m_KeyINT = KeyINT
      END


      SUBROUTINE GPS_SetPolBeams(PolBeam1,PolBeam2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Seting beam POLARIZATION vectors                                              //
*//   Dont forget Wigner rotation to GPS frame!!!!                                  //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER i,j,k
      DOUBLE PRECISION  PolBeam1(4),PolBeam2(4)
*------------------------------------------------------------------------------------
      DO k=1,4
         m_PolBeam1( k) = PolBeam1(k)
         m_PolBeam2( k) = PolBeam2(k)
      ENDDO
* Define spin density matriced
      DO i=1,2
         DO j=1,2
            m_SDMat1(i,j)=0D0
            m_SDMat2(i,j)=0D0
            DO k=1,4
               m_SDMat1(i,j)=m_SDMat1(i,j)+m_Pauli4( k,i,j) *PolBeam1(k)
               m_SDMat2(i,j)=m_SDMat2(i,j)+m_Pauli4( k,i,j) *PolBeam2(k)
            ENDDO
         ENDDO
      ENDDO
      END

      SUBROUTINE GPS_SetHvectors(HvecFer1,HvecFer2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Seting final fermion POLARIMETER vectors                                      //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER i,j,k
      DOUBLE PRECISION  HvecFer1(4),HvecFer2(4)
*-------------------------------------------------------------------------------------
      DO k=1,4
         m_HvecFer1( k) = HvecFer1(k)
         m_HvecFer2( k) = HvecFer2(k)
      ENDDO
* Define immediately polarimeter density matriced
      DO i=1,2
         DO j=1,2
            m_SDMat3(i,j)=0D0
            m_SDMat4(i,j)=0D0
            DO k=1,4
               m_SDMat3(i,j)=m_SDMat3(i,j)+m_Pauli4( k,i,j) *HvecFer1(k)
               m_SDMat4(i,j)=m_SDMat4(i,j)+m_Pauli4( k,i,j) *HvecFer2(k)
            ENDDO
         ENDDO
      ENDDO
      END


      SUBROUTINE GPS_GetPolBeams(PolBeam1,PolBeam2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   !!!!!!!!!!!!!!!!! TEMPORARY, no getters for these variables !!!!!!!!!!!!!!!!! //
*//   Geting beam POLARIZATION vectors                                              //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER k
      DOUBLE PRECISION  PolBeam1(4),PolBeam2(4)
*
      DO k=1,4
         PolBeam1( k) = m_PolBeam1(k)
         PolBeam2( k) = m_PolBeam2(k)
      ENDDO
      END

      SUBROUTINE GPS_GetHvectors(HvecFer1,HvecFer2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   !!!!!!!!!!!!!!!!! TEMPORARY, no getters for these variables !!!!!!!!!!!!!!!!! //
*//   Geting final fermion POLARIMETER vectors                                      //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      INTEGER k
      DOUBLE PRECISION  HvecFer1(4),HvecFer2(4)
*
      DO k=1,4
         HvecFer1( k) = m_HvecFer1(k)
         HvecFer2( k) = m_HvecFer2(k)
      ENDDO
      END



      SUBROUTINE GPS_Make_eps(ph,Sig,eps)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  photon polarization 4-vector explicitelly calculated                           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      SAVE                      !!! <-- necessary !!!
*
      DOUBLE PRECISION      k1(4),k2(4),k3(4),k4(4),ph(4)
      DOUBLE PRECISION      k1m(4),k2m(4),k3m(4),k4m(4)
*
      DOUBLE PRECISION      Fleps
*-----------------------------------------------------------------------------
      INTEGER    j,k
      INTEGER    Sig
      DOUBLE COMPLEX  GPS_Sof1x,GPS_Sof1bx
      DOUBLE COMPLEX  eps(4)
      DOUBLE COMPLEX  x1,x2,x3,x1m,x2m,x3m
*-----------------------------------------------------------------------------
        DO k=1,4
         k1(k)=0D0
         k2(k)=0D0
         k3(k)=0D0
         k4(k)=0D0
         k1m(k)=0D0
         k2m(k)=0D0
         k3m(k)=0D0
         k4m(k)=0D0
        ENDDO
         k=4
         k1(k)=sqrt(2D0)
         k2(k)=1D0
         k3(k)=1D0
         k4(k)=1D0
         k1m(k)=sqrt(2D0)
         k2m(k)=1D0
         k3m(k)=1D0
         k4m(k)=1D0

         k1(1)=1D0
         k1(2)=1D0
         k2(2)=1D0
         k3(3)=1D0
         k4(4)=1D0
         k1m(1)=-1D0
         k1m(2)= 1D0
         k2m(2)=-1D0
         k3m(3)=-1D0
         k4m(4)=-1D0

      Fleps =  1d-100

      IF( m_KeyArb  .EQ.  0 ) THEN
         x1 =   GPS_Sof1x( 1,ph,k1 )
         x1m=   GPS_Sof1x( 1,ph,k1m)
         x2 =   GPS_Sof1x( 1,ph,k2 )
         x2m=   GPS_Sof1x( 1,ph,k2m)
         x3 =   GPS_Sof1x( 1,ph,k3 )
         x3m=   GPS_Sof1x( 1,ph,k3m)
      ELSE
         x1 =   GPS_Sof1bx( 1,ph,k1 ,Fleps)
         x1m=   GPS_Sof1bx( 1,ph,k1m,Fleps)
         x2 =   GPS_Sof1bx( 1,ph,k2 ,Fleps)
         x2m=   GPS_Sof1bx( 1,ph,k2m,Fleps)
         x3 =   GPS_Sof1bx( 1,ph,k3 ,Fleps)
         x3m=   GPS_Sof1bx( 1,ph,k3m,Fleps)
      ENDIF
         eps(4)=(x3+x3m)/2D0
         eps(3)=-(x3-x3m)/2D0
         eps(2)=-(x2-x2m)/2D0
         eps(1)=-(x1-x1m)/2D0

         DO j=1,4
          eps(j)=eps(j)/2d0
         ENDDO

         IF (sig.LT.0d0) THEN
           DO j=1,4
             eps(j)=DCONJG(eps(j))
           ENDDO
         ENDIF
         RETURN

         write(*,*) 'vec  ph=',ph
         write(*,*) 'sig=',sig
         write(*,*) 'eps(i)=',eps

         write(*,*) 'consistency check; product=',
     $               ((eps(4)*ph(4)-eps(3)*ph(3)-eps(2)*ph(2)-eps(1)*ph(1)))
         write(*,*) 'consistency check; square=',
     $               eps(4)*DCONJG(eps(4))-eps(3)*DCONJG(eps(3))-eps(2)*DCONJG(eps(2))-eps(1)*DCONJG(eps(1))


      END                       !!!GPS_make_eps!!!


*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                      End of CLASS  GPS                                          //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE GPS_BornWPlusT(KFi,KFf,s,t,u,p1,m1,p2,m2,p3,m3,p4,m4,AmpBorn,AmpBornT)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//   Tests of BornW spin amplitudes using ortogonality relations:: to be removed?  //
*//                                                                                 //
*//   Input:                                                                        //
*//   KFi, Kff = beam and final fermion flavour codes (to define charges)           //
*//   PX       = s-chanel momentum for W propagator (not for spinors)               //
*//   pi,mi    are for spinors, not for W propagator                                //
*//   p1,m1    =fermion momentum and mass (beam)                                    //
*//   p2,m2    =fermion momentum and mass (beam)                                    //
*//   p3,m3    =fermion momentum and mass final state                               //
*//   p4,m4    =fermion momentum and mass final state                               //
*//                                                                                 //
*//   Output:                                                                       //
*//   AmpBornT     = amplitude                                                      //
*//                                                                                 //
*//                                                                                 //
*//   Notes:                                                                        //
*//   Electron mass neglected in spinors, this is why we may use Chisholm!          //
*//   Final fermion mass kept exactly. I doubt                                      //
*//   W in t-chanel.                                                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      SAVE                      !!! <-- necessary !!!
*
      INTEGER    Mode,KFi,KFf,Level,JakKoralZ
      DOUBLE PRECISION      p1(4),p2(4),p3(4),p4(4),k1(4),k2(4),k3(4),k4(4)
      DOUBLE PRECISION      k1m(4),k2m(4),k3m(4),k4m(4)
      DOUBLE PRECISION      m1,m2,m3,m4,Xborn
*
      DOUBLE PRECISION      BornSum
      DOUBLE PRECISION      s,t,u
      DOUBLE PRECISION      Fleps,m_MW, m_GammW
*-----------------------------------------------------------------------------
      INTEGER    i,j,k,l
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4
      DOUBLE COMPLEX  Cfac,AmpBornW
      DOUBLE COMPLEX  PropW
      DOUBLE COMPLEX  s31,s24,s14,s32
      DOUBLE COMPLEX  AmpBorn(2,2,2,2),AmpBornT(2,2,2,2)
      DOUBLE COMPLEX  Coef, IntIR
      DOUBLE COMPLEX  TT,UU
      DOUBLE COMPLEX  UWX1(2,2),UWX1m(2,2),VWX1(2,2),VWX1m(2,2)
      DOUBLE COMPLEX  UWX2(2,2),UWX2m(2,2),VWX2(2,2),VWX2m(2,2)
      DOUBLE COMPLEX  UWX3(2,2),UWX3m(2,2),VWX3(2,2),VWX3m(2,2)
      DOUBLE COMPLEX  GPS_iProd2
*-----------------------------------------------------------------------------
* Electroweak
      INTEGER      NCf,NCe
      DOUBLE PRECISION        T3e,Qe
      DOUBLE PRECISION        T3f,Qf
      DOUBLE COMPLEX    Cn,Ve,Ae,Vf,Af, VVCor, WVPi
*-----------------------------------------------------------------------------
      DOUBLE PRECISION        dummy
c[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      INTEGER    icont
c      DATA       icont /0/
c]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*-----------------------------------------------------------------------------
      IF (ABS(KFf) .NE. 12)  RETURN
        return
        DO k=1,4
         k1(k)=0D0
         k2(k)=0D0
         k3(k)=0D0
         k4(k)=0D0
         k1m(k)=0D0
         k2m(k)=0D0
         k3m(k)=0D0
         k4m(k)=0D0
        ENDDO
         k=4
         k1(k)=sqrt(2D0)
         k2(k)=1D0
         k3(k)=1D0
         k4(k)=1D0
         k1m(k)=sqrt(2D0)
         k2m(k)=1D0
         k3m(k)=1D0
         k4m(k)=1D0

         k1(1)=1D0
         k1(2)=1D0
         k2(2)=1D0
         k3(3)=1D0
         k4(4)=1D0
         k1m(1)=-1D0
         k1m(2)= 1D0
         k2m(2)=-1D0
         k3m(3)=-1D0
         k4m(4)=-1D0

      JakKoralZ=0  ! warning: defined in 2 places ! to fix a potential problem .eq.1 like KORALZ .eq.0 possiby OK
      Fleps =  1d-100
      Cn=1D0
         CALL GPS_MakeUX(Cn,k1,Fleps, p3,m3,   p1,m1,    UWX1) ! v-a inside
         CALL GPS_MakeVX(Cn,k1,Fleps, p2,m2,   p4,m4,    VWX1) ! v-a inside
         CALL GPS_MakeUX(Cn,k1m,Fleps, p3,m3,   p1,m1,    UWX1m) ! v-a inside
         CALL GPS_MakeVX(Cn,k1m,Fleps, p2,m2,   p4,m4,    VWX1m) ! v-a inside

         CALL GPS_MakeUX(Cn,k2,Fleps, p3,m3,   p1,m1,    UWX2) ! v-a inside
         CALL GPS_MakeVX(Cn,k2,Fleps, p2,m2,   p4,m4,    VWX2) ! v-a inside
         CALL GPS_MakeUX(Cn,k2m,Fleps, p3,m3,   p1,m1,    UWX2m) ! v-a inside
         CALL GPS_MakeVX(Cn,k2m,Fleps, p2,m2,   p4,m4,    VWX2m) ! v-a inside
           write(*,*) k1
           write(*,*) fleps,m3,m1
           write(*,*) p3
           write(*,*) p1
!           CALL GPS_UPrint(0,' UWX1    ',UWX2)
!           CALL GPS_UPrint(0,' UWX2    ',UWX2)


         CALL GPS_MakeUX(Cn,k3,Fleps, p3,m3,   p1,m1,    UWX3) ! v-a inside
         CALL GPS_MakeVX(Cn,k3,Fleps, p2,m2,   p4,m4,    VWX3) ! v-a inside
         CALL GPS_MakeUX(Cn,k3m,Fleps, p3,m3,   p1,m1,    UWX3m) ! v-a inside
         CALL GPS_MakeVX(Cn,k3m,Fleps, p2,m2,   p4,m4,    VWX3m) ! v-a inside

        DO j1 = 1,2
           DO j2 = 1,2
              DO j3 = 1,2
                 DO j4 = 1,2
                    AmpBornT( j1,j2,j3,j4) = 0D0
                 ENDDO                  
              ENDDO
           ENDDO
        ENDDO



*=============================================================
* Get charges, izospin, color
         CALL BornV_GetParticle(KFi, dummy, Qe,T3e,NCe)
         CALL BornV_GetParticle(KFf, dummy, Qf,T3f,NCf)
*=============================================================
      Coef  =1.d0/2.d0/m_Sw2

      IF(JakKoralZ.eq.1) then
*       W Propagator: it looks crazy in this case ....
        CALL GPS_EWFFactW(KFi,KFf,s,u,PropW,WVPi)
      ELSE
        CALL GPS_EWFFactW(KFi,KFf,s,t,PropW,WVPi)
      ENDIF
        DO j1 = 1,2
           DO j2 = 1,2
              DO j3 = 1,2
                 DO j4 = 1,2
                    AmpBornT( j1,j2,j3,j4) = PropW/4d0    *( ! factor of 1/4 is due to versor norm
     $              (UWX2(j3,j1)+UWX2m(j3,j1))*(VWX2(j2,j4)+VWX2m(j2,j4))
     $             -(UWX1(j3,j1)-UWX1m(j3,j1))*(VWX1(j2,j4)-VWX1m(j2,j4))
     $             -(UWX2(j3,j1)-UWX2m(j3,j1))*(VWX2(j2,j4)-VWX2m(j2,j4)) ! g_\mu_\nu matrix in versor repr.
     $             -(UWX3(j3,j1)-UWX3m(j3,j1))*(VWX3(j2,j4)-VWX3m(j2,j4))
     $                                                  )/(-2D0) ! arbitrary fix-up 
                 ENDDO                  
              ENDDO
           ENDDO
        ENDDO
      write(*,*) ' ========= test =========='
      write(*,*) ' we compare Born ME using:'
      write(*,*) ' '
      write(*,*) ' (1) standard BORN of KK '
      Call GPS_BPrint(0,' BORN   ',AmpBorn)
      write(*,*) ' '
      write(*,*) ' (2) BORN from orthogonality and  buiding blocks for nunu:'
      Call GPS_BPrint(0,' BORN-T   ',AmpBornT)
      stop
*////////////////////////////////////////////////////////////////////////////////////////////
*//     Primitives formfactor-type for construction of spin amplitudes                     //
*////////////////////////////////////////////////////////////////////////////////////////////
      END                       !!!GPS_BornWPlusT!!!

      SUBROUTINE GPS_MakeUT(KFf,ph,Sig, p2,m2,   p4,m4,  p3,m3,   p1,m1,UW,VW)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Test of UW,VW transition matrices using UWX, VWX and orthogonality             //
*//  also, photon polarization 4-vector explicitelly calculated                     //
*//  to be kept?                                                                               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'GPS.h'
      SAVE                      !!! <-- necessary !!!
*
      INTEGER    Mode,KFi,KFf,Level,JakKoralZ
      DOUBLE PRECISION      p1(4),p2(4),p3(4),p4(4),k1(4),k2(4),k3(4),k4(4),ph(4)
      DOUBLE PRECISION      k1m(4),k2m(4),k3m(4),k4m(4)
      DOUBLE PRECISION      m1,m2,m3,m4,Xborn
*
      DOUBLE PRECISION      BornSum
      DOUBLE PRECISION      s,t,u
      DOUBLE PRECISION      Fleps,m_MW, m_GammW
*-----------------------------------------------------------------------------
      INTEGER    i,j,k,l
      INTEGER    j1,j2,j3,j4
      INTEGER    Hel1,Hel2,Hel3,Hel4,Sig
      DOUBLE COMPLEX  Cfac,AmpBornW
      DOUBLE COMPLEX  PropW,GPS_Sof1x,GPS_Sof1bx
      DOUBLE COMPLEX  s31,s24,s14,s32
      DOUBLE COMPLEX  Cn,Coef, IntIR
      DOUBLE COMPLEX  TT,UU
      DOUBLE COMPLEX  UWX1(2,2),UWX1m(2,2),VWX1(2,2),VWX1m(2,2),UW(2,2),UWT(2,2)
      DOUBLE COMPLEX  UWX2(2,2),UWX2m(2,2),VWX2(2,2),VWX2m(2,2),VW(2,2),VWT(2,2)
      DOUBLE COMPLEX  UWX3(2,2),UWX3m(2,2),VWX3(2,2),VWX3m(2,2)
      DOUBLE COMPLEX  GPS_iProd2,eps(4)
      DOUBLE COMPLEX  x1,x2,x3,x1m,x2m,x3m,xp1
*-----------------------------------------------------------------------------
* Electroweak
      INTEGER      NCf,NCe
      DOUBLE PRECISION        T3e,Qe
      DOUBLE PRECISION        T3f,Qf
      DOUBLE COMPLEX    Ve,Ae,Vf,Af, VVCor, WVPi
*-----------------------------------------------------------------------------
      DOUBLE PRECISION        dummy
c[[[[[[[[[[!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      INTEGER    icont
c      DATA       icont /0/
c]]]]]]]]]]!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*-----------------------------------------------------------------------------
      IF (ABS(KFf) .NE. 12)  RETURN
       if (sig.lt.0d0) return
         return
        DO k=1,4
         k1(k)=0D0
         k2(k)=0D0
         k3(k)=0D0
         k4(k)=0D0
         k1m(k)=0D0
         k2m(k)=0D0
         k3m(k)=0D0
         k4m(k)=0D0
        ENDDO
         k=4
         k1(k)=sqrt(2D0)
         k2(k)=1D0
         k3(k)=1D0
         k4(k)=1D0
         k1m(k)=sqrt(2D0)
         k2m(k)=1D0
         k3m(k)=1D0
         k4m(k)=1D0

         k1(1)=1D0
         k1(2)=1D0
         k2(2)=1D0
         k3(3)=1D0
         k4(4)=1D0
         k1m(1)=-1D0
         k1m(2)= 1D0
         k2m(2)=-1D0
         k3m(3)=-1D0
         k4m(4)=-1D0

      JakKoralZ=0  ! warning: defined in 2 places ! to fix a potential problem .eq.1 like KORALZ .eq.0 possiby OK
      Fleps =  1d-100

      IF( m_KeyArb  .EQ.  0 ) THEN
         x1 =   GPS_Sof1x( 1,ph,k1 )
         x1m=   GPS_Sof1x( 1,ph,k1m)
         x2 =   GPS_Sof1x( 1,ph,k2 )
         x2m=   GPS_Sof1x( 1,ph,k2m)
         x3 =   GPS_Sof1x( 1,ph,k3 )
         x3m=   GPS_Sof1x( 1,ph,k3m)
         xp1=   GPS_Sof1x( 1,ph,p1) 
      ELSE
         x1 =   GPS_Sof1bx( 1,ph,k1 ,Fleps)
         x1m=   GPS_Sof1bx( 1,ph,k1m,Fleps)
         x2 =   GPS_Sof1bx( 1,ph,k2 ,Fleps)
         x2m=   GPS_Sof1bx( 1,ph,k2m,Fleps)
         x3 =   GPS_Sof1bx( 1,ph,k3 ,Fleps)
         x3m=   GPS_Sof1bx( 1,ph,k3m,Fleps)
         xp1=   GPS_Sof1bx( 1,ph,p1, Fleps)
      ENDIF
         eps(4)=(x3+x3m)/2D0
         eps(3)=-(x3-x3m)/2D0
         eps(2)=-(x2-x2m)/2D0
         eps(1)=-(x1-x1m)/2D0
         write(*,*) '  >>>>  TEST of UWX, VWX <<<<'
         write(*,*) 'vec  ph=',ph
         write(*,*) 'sig=',sig
         write(*,*) 'raw eps(i)=',eps

         write(*,*) 'consistency check on Sof1(b)x; ratio=',
     $               ((eps(4)*p1(4)-eps(3)*p1(3)-eps(2)*p1(2)-eps(1)*p1(1))/xp1)
         write(*,*) 'we got however from Sof1(b)x and standard othogonality game twice photon polarization 4-vector.'
         do j1=1,4
          eps(j1)=eps(j1)/2d0
         enddo

         write(*,*) '    eps(i)=',eps
         Cn=1D0
         CALL GPS_MakeUX(Cn,k1,Fleps, p3,m3,   p1,m1,    UWX1) ! v-a inside
         CALL GPS_MakeVX(Cn,k1,Fleps, p2,m2,   p4,m4,    VWX1) ! v-a inside
         CALL GPS_MakeUX(Cn,k1m,Fleps, p3,m3,   p1,m1,    UWX1m) ! v-a inside
         CALL GPS_MakeVX(Cn,k1m,Fleps, p2,m2,   p4,m4,    VWX1m) ! v-a inside

         CALL GPS_MakeUX(Cn,k2,Fleps, p3,m3,   p1,m1,    UWX2) ! v-a inside
         CALL GPS_MakeVX(Cn,k2,Fleps, p2,m2,   p4,m4,    VWX2) ! v-a inside
         CALL GPS_MakeUX(Cn,k2m,Fleps, p3,m3,   p1,m1,    UWX2m) ! v-a inside
         CALL GPS_MakeVX(Cn,k2m,Fleps, p2,m2,   p4,m4,    VWX2m) ! v-a inside

         CALL GPS_MakeUX(Cn,k3,Fleps, p3,m3,   p1,m1,    UWX3) ! v-a inside
         CALL GPS_MakeVX(Cn,k3,Fleps, p2,m2,   p4,m4,    VWX3) ! v-a inside
         CALL GPS_MakeUX(Cn,k3m,Fleps, p3,m3,   p1,m1,    UWX3m) ! v-a inside
         CALL GPS_MakeVX(Cn,k3m,Fleps, p2,m2,   p4,m4,    VWX3m) ! v-a inside

        DO j1 = 1,2
           DO j2 = 1,2
              DO j3 = 1,2
                 DO j4 = 1,2
                 ENDDO                  
              ENDDO
           ENDDO
        ENDDO

      

      Coef  =1.d0/2.d0/m_Sw2

        DO j1 = 1,2
           DO j2 = 1,2
               UWT(j1,j2)=(1+sig)/2*(
     $                     eps(4)*(uwx3(j1,j2)+uwx3m(j1,j2))/2
     $                    +eps(3)*(uwx3(j1,j2)-uwx3m(j1,j2))/2
     $                    +eps(2)*(uwx2(j1,j2)-uwx2m(j1,j2))/2
     $                    +eps(1)*(uwx1(j1,j2)-uwx1m(j1,j2))/2
     $                     ) ! not fully tested, case of sig= 1
               UWT(j1,j2)=UWT(j1,j2)+(1-sig)/2D0*( 
     $                    -dconjg(eps(4))*(uwx3(j1,j2)+uwx3m(j1,j2))/2
     $                    -dconjg(eps(3))*(uwx3(j1,j2)-uwx3m(j1,j2))/2
     $                    -dconjg(eps(2))*(uwx2(j1,j2)-uwx2m(j1,j2))/2
     $                    -dconjg(eps(1))*(uwx1(j1,j2)-uwx1m(j1,j2))/2
     $                     )    ! tested to the end case of sig= -1
               VWT(j1,j2)=(1+sig)/2*(
     $                     eps(4)*(vwx3(j1,j2)+vwx3m(j1,j2))/2
     $                    +eps(3)*(vwx3(j1,j2)-vwx3m(j1,j2))/2
     $                    +eps(2)*(vwx2(j1,j2)-vwx2m(j1,j2))/2
     $                    +eps(1)*(vwx1(j1,j2)-vwx1m(j1,j2))/2
     $                     ) ! 
               VWT(j1,j2)=VWT(j1,j2)+(1-sig)/2D0*( 
     $                    -dconjg(eps(4))*(vwx3(j1,j2)+vwx3m(j1,j2))/2
     $                    -dconjg(eps(3))*(vwx3(j1,j2)-vwx3m(j1,j2))/2
     $                    -dconjg(eps(2))*(vwx2(j1,j2)-vwx2m(j1,j2))/2
     $                    -dconjg(eps(1))*(vwx1(j1,j2)-vwx1m(j1,j2))/2
     $                     )    ! 
           ENDDO
        ENDDO

           CALL GPS_UPrint(0,' UW    ',UW)
           CALL GPS_UPrint(0,' UW-T    ',UWT)
           write(*,*) 'CONCLUSION of two above being equal:'
           write(*,*) 'MakeUX acts on a 4-vector identically as combination of MakeUb MatrWm _times'
           write(*,*) 'acts on normalized to -1 polarization vector of photon'
           CALL GPS_UPrint(0,' VW    ',VW)
           CALL GPS_UPrint(0,' VW-T    ',VWT)
           write(*,*) 'CONCLUSION of two above being equal:'
           write(*,*) 'MakeVX acts on a 4-vector identically as combination of MakeVb MatrWm _times'
           write(*,*) 'acts on normalized to -1 polarization vector of photon'

       stop
*////////////////////////////////////////////////////////////////////////////////////////////
*//     Primitives formfactor-type for construction of spin amplitudes                     //
*////////////////////////////////////////////////////////////////////////////////////////////
      END                       !!!GPS_BornWPlusT!!!







*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                     Pseudo-CLASS  HepEvt                                 //
*//                                                                          //
*//  Purpose:  keep and serve event in HEPEVT format                         //
*//                                                                          //
*//  Output of KK2f is encoded in double precission /d_hepevt/               //
*//  which is double precision version of /hepevt/                           //
*//  It was necessary to rename /hepevt/                                     //
*//  because older Jetset uses single precision version of /hepevt/          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////


      SUBROUTINE HepEvt_Fill
*//////////////////////////////////////////////////////////////////////////////////////
*//                                                                                  //
*//  Filling HEP COMMON block using HepEvt_Fil1 routine                              //
*//                                                                                  //
*//  INPUT:                                                                          //
*//     KFfin          final particle code       (KarLud)                            //
*//     pf1,pf2        beam (electron) momenta   (KarLud)                            //
*//     nphox          photon multiplicity       (KarLud)                            //
*//     xphot          photon momenta            (KarLud)                            //
*//     qf1,qf2        final momenta             (Karfin)                            //
*//     nphoy          photon multiplicity       (KarFin)                            //
*//     yphot          photon momenta            (KarFin)                            //
*//                                                                                  //
*//  !!!!!! To be disscussed with the users. !!!!!!                                  //
*//  In present version we add beamstrahlung photons to the record                   //
*//  along with the ISR photons. It can be missleading!                              //
*//  May be they should be rather listed                                             //
*//  as the initial state particles, together with beams?                            //
*//                                                                                  //
*//////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  pf1(4),pf2(4),qf1(4),qf2(4),xphot(100,4),yphot(100,4)
      DOUBLE PRECISION  aph(5), bph(5)
      DOUBLE PRECISION  amel,amfin,BornV_GetMass
      DOUBLE PRECISION  Psum(4), Etot
      DOUBLE PRECISION  WtMain,WtCrud
      INTEGER           KFfin,ip,kstat,j,i,nphoy,nphox,kfbeam
      DOUBLE PRECISION  pbos(4),xmtmp ! &&&
*------------------------------------------------------------------------------
* Actual KFcode of final fermion
      CALL KarLud_GetKFfin(KFfin)
* Beams and ISR photons
      CALL KarLud_GetPhotons(nphox,xphot)
      CALL KarLud_GetBeams(pf1,pf2)
* Final fermions and FSR photons
      CALL KarFin_GetPhotons(nphoy,yphot)
      CALL KarFin_GetFermions(qf1,qf2)
      DO j=1,4
         Psum(j)=qf1(j)+qf2(j)
      ENDDO
*
      KFbeam = 11    ! KF=11 denotes electron
      amel   =BornV_GetMass(KFbeam)
* Beams
      ip=1                      !(&&&
      CALL HepEvt_Fil1(ip,3, 11, 0,0,0,0, pf1,amel,.FALSE.)
      ip=2
      CALL HepEvt_Fil1(ip,3,-11, 0,0,0,0, pf2,amel,.FALSE.)
* Now mother of ffbar and photons added in event record 
*   1,2               - beams
*   3                 - Z/gamma*
*   4,...,4+nphot     - photons 
*   4+nphot,5+nphot   - final fermions   
      DO j=1,4
         pbos(j)=qf1(j)+qf2(j)
      ENDDO
      xmtmp=pbos(4)**2-pbos(1)**2-pbos(2)**2-pbos(3)**2
      xmtmp=DSQRT(ABS(xmtmp))
      ip=ip+1
      CALL HepEvt_Fil1(ip,2,23,1,2,0,0,pbos,xmtmp,.FALSE.)

* Radiative photons (4 ... 3+nphot) (pdg-code for gamma is 22)
* Note that both ISR and FSR photons have beams as parents.
* This is because (a) JETSET does not tolerate FSR photons attached to quarks
*                 (b) photons for CEEX model cannot be split into ISR and FSR
      CALL HepEvt_SetPhotStart(ip) ! remeber position of the 1-st photon
      IF(nphox .NE. 0) THEN
         DO i=1,nphox
            DO j=1,4
               aph(j) = xphot(i,j)
               Psum(j)= Psum(j)+xphot(i,j)
            END DO
            ip=ip+1
            CALL HepEvt_Fil1(ip,1,22, 1,2,0,0, aph,0d0,.FALSE.) ! ISR
         END DO
      END IF
      IF(nphoy .NE. 0) THEN
         DO i=1,nphoy
            DO j=1,4
               aph(j) = yphot(i,j)
               Psum(j)= Psum(j)+yphot(i,j)
            END DO
            ip=ip+1
c$$$           CALL HepEvt_Fil1(4+ip,1,22, 2,1,0,0, aph,0d0,.FALSE.) ! FSR
            CALL HepEvt_Fil1(ip,1,22, 1,2,0,0, aph,0d0,.FALSE.) ! FSR
         END DO
      END IF
* add beamstrahlung photons to the record along with ISR photons, can be missleading!!
      CALL KarLud_GetBeasts( aph, bph)
      IF( aph(4).NE.0d0) THEN
         ip=ip+1
         CALL HepEvt_Fil1(ip,1,22, 1,2,0,0, aph,0d0,.FALSE.)
         DO j=1,4
            Psum(j)= Psum(j)+aph(j)
         ENDDO
      ENDIF
      IF( bph(4).NE.0d0) THEN
         ip=ip+1
         CALL HepEvt_Fil1(ip,1,22, 1,2,0,0, bph,0d0,.FALSE.)
         DO j=1,4
            Psum(j)= Psum(j)+bph(j)
         ENDDO
      ENDIF
      CALL HepEvt_SetPhotEnd(ip) ! remeber position of the last photon

*  Final state fermions (&&& moved after photons
      IF( KFfin .EQ. 0  ) RETURN
      amfin  =BornV_GetMass(KFfin)
      KStat = 1
      ip=ip+1
      CALL HepEvt_Fil1(ip,KStat, KFfin, 3,3,0,0, qf1,amfin,.FALSE.) ! &&& parent=3
      CALL HepEvt_SetF(ip)
      ip=ip+1
      CALL HepEvt_Fil1(ip,KStat,-KFfin, 3,3,0,0, qf2,amfin,.FALSE.) ! &&& parent=3
      CALL HepEvt_SetFbar(ip)

      Etot= SQRT(ABS(Psum(4)**2 -Psum(3)**2 -Psum(2)**2 -Psum(1)**2))
* Check on total 4-momentum conservation
      IF( ABS(Etot/(pf1(4)+pf2(4)+aph(4)+bph(4))-1d0) .GT.1d-4) THEN
         WRITE(*,*) '++++ HepEvt_Fill: something wrong with Etot=',Etot
      ENDIF
      IF( ABS(Psum(4)/(pf1(4)+pf2(4)+aph(4)+bph(4))-1d0) .GT.1d-4) THEN
         WRITE(*,*) '++++ HepEvt_Fill: something wrong with Psum(4)=',Psum(4)
      ENDIF
* Finaly fill also LUND common block
      CALL pyhepc(2)
      END


      SUBROUTINE HepEvt_Hadronize(HadMin)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//          Aranging jets and hadronization                                 //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
*
      INTEGER  ijoin(2)
      DOUBLE PRECISION    HadMin
      REAL                sqrs1
      INTEGER  ih1,ih2
* ----------------------------------------------------------------------

* Quarks only, KeyHad=1 required
c$$$      ih1=3  ! fermion is here      &&&
c$$$      ih2=4  ! antifermion is here  &&&
      Call HepEvt_GetF(   ih1)  ! fermion is here
      Call HepEvt_GetFbar(ih2)  ! antifermion is here
      IF ( ABS(idhep(ih1)) .LT. 10 ) THEN 
** Explicit string arangement:
         ijoin(1) = ih1
         ijoin(2) = ih2
** q-qbar effective mass
         sqrs1=(phep(4,ih1)+phep(4,ih2))**2
     $        -(phep(3,ih1)+phep(3,ih2))**2
     $        -(phep(2,ih1)+phep(2,ih2))**2
     $        -(phep(1,ih1)+phep(1,ih2))**2
         sqrs1=sqrt(abs(sqrs1))
* Showering < HadMas cut-off value (this also deals with WT=0 events)
* see also bornv
         IF( sqrs1 .GT. HadMin**2) THEN
            CALL pyjoin(2,ijoin)
            CALL pyshow(ih1,ih2,sqrs1)
            CALL pyexec
         ENDIF
      ENDIF
      
      END


      SUBROUTINE HepEvt_Fil1( n,ist,id,jmo1,jmo2,jda1,jda2,p4,pinv,phflag)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// This subroutine fills one entry into the hepevt common                   //
*// and updates the information for affected mother entries                  //
*// WRITTEN by Martin W. Gruenewald (91/01/28)                               //
*// Re-Edited by S. Jadach, 6 july 97                                        //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
* input
      INTEGER           n,ist,id,jmo1,jmo2,jda1,jda2
      DOUBLE PRECISION  p4(4),pinv
      LOGICAL           phflag
* locals
      INTEGER           ip,i,ihep
      SAVE
* ----------------------------------------------------------------------
*
* check address mode
      IF (n .EQ. 0) THEN
* append mode
        ihep=nhep+1
      ELSE IF (n .GT. 0) THEN
* absolute position
        ihep=n
      ELSE
* relative position
        ihep=nhep+n
      END IF
*
* check on ihep
      IF ((ihep .LE. 0) .OR. (ihep .GT. nmxhep)) RETURN
*
* add entry
      nhep=ihep
      isthep(ihep)   =ist
      idhep(ihep)    =id
      jmohep(1,ihep) =jmo1
      IF(jmo1 .LT. 0)  jmohep(1,ihep)=jmohep(1,ihep)+ihep
      jmohep(2,ihep) =jmo2
      IF(jmo2 .LT. 0)  jmohep(2,ihep)=jmohep(2,ihep)+ihep
      jdahep(1,ihep) =jda1
      jdahep(2,ihep) =jda2
*
      DO i=1,4
         phep(i,ihep)=p4(i)
* KORAL-B and KORAL-Z do not provide vertex and/or lifetime informations
         vhep(i,ihep)=0d0
      END DO
      phep(5,ihep)=pinv
* flag for photos...
      qedrad(ihep)=phflag
* update process:
      DO ip=jmohep(1,ihep),jmohep(2,ihep)
         IF(ip .GT. 0)THEN
* IF there is a daughter at ihep, mother entry at ip has decayed
            IF(isthep(ip) .EQ. 1)isthep(ip)=2
* and daughter pointers of mother entry must be updated
            IF(jdahep(1,ip) .EQ. 0)THEN
               jdahep(1,ip)=ihep
               jdahep(2,ip)=ihep
            ELSE
               jdahep(2,ip)=max(ihep,jdahep(2,ip))
            END IF
         END IF
      END DO
*
      END


      SUBROUTINE HepEvt_GetKFfin(KFfin)
*//////////////////////////////////////////////////////////////////////////////
*// Purpose:  Get KFcode of final fermion out of /hepevt/                    //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER KFfin
      INTEGER ih1 ! pjh
* ----------------------------------------------------------------------
      CALL HepEvt_GetF(ih1) ! pjh
      KFfin = idhep(ih1) ! pjh
      END ! GetKFfin

      SUBROUTINE HepEvt_GetBeams(pf1,pf2)
*//////////////////////////////////////////////////////////////////////////////
*// Purpose:  Get Beam momenta out of /hepevt/                               //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  pf1(4),pf2(4)
      INTEGER k
* ----------------------------------------------------------------------
      DO k=1,4
         pf1(k) =phep(k,1)
         pf2(k) =phep(k,2)
      ENDDO
      END ! GetBeams
      SUBROUTINE HepEvt_GetParticle(Id,Istart,Iadress,pf1)
*//////////////////////////////////////////////////////////////////////////////
*// Purpose:  Get Id-partcle momenta out of /hepevt/                         //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  pf1(4)
      INTEGER k,l,Id,Istart,Iadress
* ----------------------------------------------------------------------
      Iadress=-1
      DO l=Istart,nhep
        IF(Idhep(l).eq.Id) then
          Iadress=l
          DO k=1,4
             pf1(k) =phep(k,l)
          ENDDO
          RETURN
        ENDIF
      ENDDO
      END ! GetParticle

      SUBROUTINE HepEvt_GetFfins(pf1,pf2)
*//////////////////////////////////////////////////////////////////////////////
*// Purpose:  Get final fermion momenta out of /hepevt/                      //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  pf1(4),pf2(4)
      INTEGER k
      INTEGER ih1,ih2 ! pjh
* ----------------------------------------------------------------------
      CALL HepEvt_GetF(ih1) ! pjh
      CALL HepEvt_GetFbar(ih2) ! pjh
      DO k=1,4
         pf1(k) =phep(k,ih1) ! pjh
         pf2(k) =phep(k,ih2) ! pjh
      ENDDO
      END ! GetFfins

      SUBROUTINE HepEvt_GetNPhot(nphot)
*//////////////////////////////////////////////////////////////////////////////
*// Purpose:  Get number of bremsstrahlung photons  out of /hepevt/          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER nphot,kf,j,ih
      INTEGER ih1,ifot ! pjh
*
      nphot=0
      CALL HepEvt_GetPhotStart(ifot) ! pjh
      DO j=1,100
         ih = ifot+j ! pjh
         kf = idhep(ih)
* STOP if /hepevt/ ended or non-photon found
         IF(ih .GT. nhep) GOTO 110
         IF(kf .NE. 22)   GOTO 110
* Bremsstrahlung photons have by convention 1-st parent being 1-st beam (ISR)
* at ih=1 or first final fermion at (FSR)
         CALL HepEvt_GetF(ih1) ! pjh
         IF(jmohep(1,ih).EQ.1 .OR. jmohep(1,ih).EQ.ih1) nphot=nphot+1 ! pjh
      ENDDO
      RETURN
 110  CONTINUE
      END


      SUBROUTINE HepEvt_GetPhotAll(nphot,phot)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get ALL photons out of /hepevt/                                //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  phot(100,4)
      INTEGER nphot,j,k,ih,kf
      INTEGER ih1,ifot ! pjh

* ----------------------------------------------------------------------
      nphot=0
      CALL HepEvt_GetPhotStart(ifot) ! pjh
      DO j=1,100
         ih = ifot+j ! pjh
         kf = idhep(ih)
* STOP if /hepevt/ ended or non-photon found
         IF(ih .GT. nhep) GOTO 110
         IF(kf .NE. 22)   GOTO 110
* Bremsstrahlung photons have by convention 1-st parent being 1-st beam (ISR)
* at ih=1 or first final fermion  (FSR)
         CALL HepEvt_GetF(ih1) ! pjh
         IF(jmohep(1,ih).EQ.1 .OR. jmohep(1,ih).EQ.ih1) THEN ! pjh
            nphot=nphot+1
            DO  k=1,4
               phot(nphot,k) =phep(k,ih)
            ENDDO
         ENDIF
      ENDDO
      GOTO 900
 110  CONTINUE
      RETURN
 900  WRITE(*,*) '++++ HepEvt_GetPhotAll: To many photons!!!'
      END ! HepEvt_GetPhotAll

      SUBROUTINE HepEvt_GetPhotIni(nphot,phot)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get ISR photons out of /hepevt/                                //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  phot(100,4)
      INTEGER nphot,j,k,ih,kf
      INTEGER ifot ! pjh
* ----------------------------------------------------------------------
      nphot=0
      CALL HepEvt_GetPhotStart(ifot)
      DO j=1,100
         ih = ifot+j               ! pjh
         kf = idhep(ih)
* STOP if /hepevt/ ended or non-photon found
         IF(ih .GT. nhep) GOTO 110
         IF(kf .NE. 22)   GOTO 110
* ISR photons have by convention 1-st parent being 1-st beam (convention)
         IF(jmohep(1,ih) .EQ. 1) THEN
            nphot=nphot+1
            DO  k=1,4
               phot(nphot,k) =phep(k,ih)
            ENDDO
         ENDIF
      ENDDO
      GOTO 900
 110  CONTINUE
      RETURN
 900  WRITE(*,*) '++++ HepEvt_GetPhotIni: To many photons!!!'
      END ! HepEvt_GetPhotIni

      SUBROUTINE HepEvt_GetPhotFin(nphot,phot)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get FSR photons out of /hepevt/                                //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  phot(100,4)
      INTEGER nphot,j,k,ih,kf
      INTEGER ifot ! pjh
* ----------------------------------------------------------------------
      nphot=0
      CALL HepEvt_GetPhotStart(ifot) ! pjh
      DO j=1,100
         ih = ifot+j               ! pjh
         kf = idhep(ih)
* STOP if /hepevt/ ended or non-photon found
         IF(ih .GT. nhep) GOTO 110
         IF(kf .NE. 22)   GOTO 110
* FSR photons have 1-st parent being 2-nd beam (convention)
         IF(jmohep(1,ih) .EQ. 2) THEN
            nphot=nphot+1
            DO  k=1,4
               phot(nphot,k) =phep(k,ih)
            ENDDO
         ENDIF
      ENDDO
      GOTO 900
 110  CONTINUE
      RETURN
 900  WRITE(*,*) '++++ HepEvt_GetPhotFin: To many photons!!!'
      END ! HepEvt_GetPhotFin

      SUBROUTINE HepEvt_GetPhotBst(nphot,phot)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get Beamstrahlung photons out of /hepevt/                      //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      DOUBLE PRECISION  phot(100,4)
      INTEGER nphot,j,k,ih,kf
      INTEGER ifot ! pjh
* ----------------------------------------------------------------------
      nphot=0
      CALL HepEvt_GetPhotStart(ifot) ! pjh
      DO j=1,100
         ih = ifot+j               ! pjh
         kf = idhep(ih)
* STOP if /hepevt/ ended or non-photon found
         IF(ih .GT. nhep) GOTO 110
         IF(kf .NE. 22)   GOTO 110
* Beamsstrahlung photons have pT exactly zero
         IF( (phep(1,ih)**2 +phep(2,ih)) .EQ. 0d0) THEN
            nphot=nphot+1
            DO  k=1,4
               phot(nphot,k) =phep(k,ih)
            ENDDO
         ENDIF
      ENDDO
      GOTO 900
 110  CONTINUE
      RETURN
 900  WRITE(*,*) '++++ HepEvt_GetPhotBst: To many photons!!!'
      END ! HepEvt_GetPhotBst


      SUBROUTINE HepEvt_SetPhotStart(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Set start position of photons in HepEvt common                 //
*//           First photon is at StartPosn+1                                 //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      m_PhotStart=Posn
      END

      SUBROUTINE HepEvt_GetPhotStart(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get start position of photons in HepEvt common                 //
*//           First photon is at StartPosn+1                                 //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      Posn=m_PhotStart
      END

      SUBROUTINE HepEvt_SetPhotEnd(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get end position of photons in HepEvt common                   //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      m_PhotEnd=Posn
      END

      SUBROUTINE HepEvt_GetPhotEnd(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get end position of photons in HepEvt common                   //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      Posn=m_PhotEnd
      END

      SUBROUTINE HepEvt_GetPhotNumb(Numb)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Get number of photons in HepEvt common                         //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Numb
      Numb=m_PhotEnd-m_PhotStart
      END

      SUBROUTINE HepEvt_SetF(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Set position of final state fermion in HepEvt common           //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      m_PosnF=Posn
      END

      SUBROUTINE HepEvt_GetF(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Set position of final state fermion in HepEvt common           //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      Posn=m_PosnF
      END

      SUBROUTINE HepEvt_SetFbar(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Set position of final state anti-fermion in HepEvt common      //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      m_PosnFbar=Posn
      END

      SUBROUTINE HepEvt_GetFbar(Posn)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Purpose:  Set position of final state anti-fermion in HepEvt common      //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'HepEvt.h'
      INTEGER Posn
      Posn=m_PosnFbar
      END

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  HepEvt                                //
*//////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//  =======================================================================        //
*//  =======================================================================        //
*//  =====================FERMION PAIR PRODUCTION===========================        //
*//  ===========QED INITIAL AND FINAL STATE EXPONENTIATION==================        //
*//  =======================================================================        //
*//  ==This program is the descendant of the YFS3 and KORALZ Monte Carlos===        //
*//  ===================YFS1 September 1987 ================================        //
*//  ===================YFS2 September 1988 ================================        //
*//  ===================YFS3 February  1993 ================================        //
*//  =================YFS3ff September 1997 ================================        //
*//  ===================KK2f June      1998 ================================        //
*//  ===================KK  4.00  Nov. 1998 ================================        //
*//  ===================KK  4.01  Feb. 1999 ================================        //
*//  ===================KK  4.02  Apr. 1999 ================================        //
*//  ===================KK  4.11  Sep. 1999 ================================        //
*//  ===================KK  4.12  Oct. 1999 ================================        //
*//  ===================KK  4.13  Jan. 2000 ================================        //
*//  ===================KK  4.14  Jun. 2000 ================================        //
*//  ===================KK  4.15  May. 2001 ================================        //
*//  =======================================================================        //
*//                                                                                 //
*//  AUTHORS:                                                                       //
*//                        S. Jadach                                                //
*//     Address: Institute of Nuclear Physics, Cracow, Poland                       //
*//                       B.F.L. Ward                                               //
*//     Address: University of Tennessee,  Knoxville, Tennessee                     //
*//                         Z. Was                                                  //
*//     Address: Institute of Nuclear Physics, Cracow, Poland                       //
*//                                                                                 //
*//            (C) 1998 by S. Jadach, BFL Ward, Z. Was                              //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////


*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//                       Pseudo-CLASS  KK2f                                        //
*//                                                                                 //
*//     Purpose:   KK 2-fermion generator, top level class                          //
*//                                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*

      SUBROUTINE KK2f_ReaDataX(DiskFile,iReset,imax,xpar)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   DiskFile  = input file to read                                                //
*//   imax   = maximum index in xpar                                                //
*//   iReset = 1, resets xpar to 0d0                                                //
*//   iTalk=1,     prints echo into standard input                                  //
*//                                                                                 //
*//   Single data card is:    (a1,i4,d15.0,a60)                                     //
*//   First data card: BeginX                                                       //
*//   Last  data card: EndX                                                         //
*//   First character * defines comment card!                                       //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      CHARACTER*(*)     DiskFile
      DOUBLE PRECISION  xpar(*)
      CHARACTER*6       beg6
      CHARACTER*4       end4
      CHARACTER*1       mark1
      CHARACTER*60      comm60
      CHARACTER*80      comm80
      INTEGER           imax,iReset,iTalk
      INTEGER           ninp,i,line,index
      DOUBLE PRECISION  value
*////////////////////////////////////////
*//  Clear xpar and read default Umask //
*////////////////////////////////////////
      iTalk = 1
      IF(iReset .EQ. 1 ) THEN
         iTalk = 0
         DO i=1,imax
            xpar(i)=0d0
         ENDDO
      ENDIF
      ninp = 13
      OPEN(ninp,file=DiskFile)
      IF(iTalk .EQ. 1) THEN
         WRITE(  *,*) '****************************'
         WRITE(  *,*) '*    KK2f_ReaDataX Starts  *'
         WRITE(  *,*) '****************************'
      ENDIF
* Search for 'BeginX'
      DO line =1,100000
         READ(ninp,'(a6,a)') beg6,comm60
         IF(beg6 .EQ. 'BeginX') THEN
            IF(iTalk .EQ. 1)   WRITE( *,'(a6,a)') beg6,comm60
            GOTO 200
         ENDIF
      ENDDO
 200  CONTINUE
* Read data, 'EndX' terminates data, '*' marks comment
      DO line =1,100000
         READ(ninp,'(a)') mark1
         IF(mark1 .EQ. ' ') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a1,i4,d15.0,a60)') mark1,index,value,comm60
            IF(iTalk .EQ. 1) 
     $           WRITE( *,'(a1,i4,g15.6,a60)') mark1,index,value,comm60
            IF( (index .LE. 0) .OR. (index .GE. imax)) GOTO 990
            xpar(index) = value
         ELSEIF(mark1 .EQ. 'E') THEN
            BACKSPACE(ninp)
            READ(  ninp,'(a4,a)') end4,comm60
            IF(iTalk .EQ. 1)   WRITE( *,'(a4,a)') end4,comm60
            IF(end4 .EQ. 'EndX') GOTO 300
            GOTO 991
         ELSEIF(mark1 .EQ. '*') THEN
            BACKSPACE(ninp)
            READ(  ninp,'(a)') comm80
            IF(iTalk .EQ. 1)    WRITE( *,'(a)') comm80
         ENDIF
      ENDDO
 300  CONTINUE
      IF(iTalk .EQ. 1)  THEN
         WRITE(  *,*) '**************************'
         WRITE(  *,*) '*   KK2f_ReaDataX Ends   *'
         WRITE(  *,*) '**************************'
      ENDIF
      CLOSE(ninp)
      RETURN
*-----------
 990  WRITE(    *,*) '+++ KK2f_ReaDataX: wrong index= ',index
      STOP
      RETURN
 991  WRITE(    *,*) '+++ KK2f_ReaDataX: wrong end of data '
      STOP
      END



      SUBROUTINE KK2f_Initialize(xpar)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Initialize class KK2f_                                                        //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INCLUDE 'BXformat.h'
      DOUBLE PRECISION    xpar(*)
      INTEGER             IsGenerated, KF,j,kxp
      INTEGER             Nbranch, KFferm
      DOUBLE PRECISION    BornV_Integrated, BornV_Sig0nb
      DOUBLE PRECISION    BornV_GetMass, BornV_GetAuxPar
      DOUBLE PRECISION    WtMax,  amferm, xcru, vmaxGPS
      DOUBLE PRECISION    WMlist(200), Xborn(200)  ! 200 should be class parameter!!!
      INTEGER             NBbranch,   KFlist(200), Nbin
      DOUBLE PRECISION    vvmin, pol1, pol2, PolBeam1(4), PolBeam2(4)
*--------------------------------------------------------------------------
*     Initialization of the internal histogramming package
      CALL GLK_Initialize

      DO j=1,m_jlim
         m_xpar(j)=xpar(j)
      ENDDO
*
      m_out = m_xpar(4)
*
      WRITE(m_out,'(10x,a)')
     $' ',
     $'  *************************************************************',
     $'  *  ****   ****    ****  ****    ***       ***     ******    *',
     $'  *  ****   ****    ****  ****    ****     ****   **********  *',
     $'  *  ****   ****    ****  ****    *****   *****  *****   ***  *',
     $'  *  **********     *********     *************  ****         *',
     $'  *  *******        ******        *************  ****         *',
     $'  *  **********     ********      **** *** ****  *****   ***  *',
     $'  *  ****  *****    ****  ****    ****  *  ****   **********  *',
     $'  *  ****   *****   ****   ****   ****     ****     *******   *',
     $'  *************************************************************',
     $' '
*
      m_CMSene = m_xpar( 1)
      m_DelEne = m_xpar( 2)
      m_WTmax  = m_xpar( 9)
      m_KeyWgt = m_xpar(10)
      m_npmax  = m_xpar(19)
      m_Idyfs  = m_xpar(8)
      m_KeyISR = m_xpar(20)
      m_KeyFSR = m_xpar(21)
      m_KeyINT = m_xpar(27)
      m_KeyGPS = m_xpar(28)
      m_alfinv = m_xpar(30)
      m_vcut(1)= m_xpar(41)
      m_vcut(2)= m_xpar(42)
      m_vcut(3)= m_xpar(43)
      m_KeyHad = m_xpar(50)
      m_HadMin = m_xpar(51)
      m_KFini  = m_xpar(400)
      m_MasPhot= m_xpar(510)
      vvmin =xpar(16)
      m_Emin   = m_CMSene/2d0 * vvmin
      m_Xenph  = m_xpar(40)
      IF(m_KeyINT .EQ. 0)  m_Xenph  = 1D0
*
      DO j=1,3
         m_PolBeam1(j)=m_xpar(60+j)
         m_PolBeam2(j)=m_xpar(63+j)
      ENDDO
      m_PolBeam1(4)=1d0
      m_PolBeam2(4)=1d0

*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '        KK Monte Carlo         '
      WRITE(m_out,bxl1v) 'Version ',      m_version,     m_date
      WRITE(m_out,bxl1f) m_CMSene,   'CMS energy average ','CMSene','a1'
      WRITE(m_out,bxl1f) m_DelEne,   'Beam energy spread ','DelEne','a2'
      WRITE(m_out,bxl1i) m_npmax,    'Max. photon mult.  ','npmax ','a3'
      WRITE(m_out,bxl1i) m_KeyISR,   'ISR switch         ','KeyISR','a4'
      WRITE(m_out,bxl1i) m_KeyFSR,   'FSR switch         ','KeyFSR','a5'
      WRITE(m_out,bxl1i) m_KeyINT,   'ISR/FSR interferenc','KeyINT','a6'
      WRITE(m_out,bxl1i) m_KeyGPS,   'New exponentiation ','KeyGPS','a7'
      WRITE(m_out,bxl1i) m_KeyHad,   'Hadroniz.  switch  ','KeyHad','a7'
      WRITE(m_out,bxl1f) m_HadMin,   'Hadroniz. min. mass','HadMin','a9'
      WRITE(m_out,bxl1f) m_WTmax,    'Maximum weight     ','WTmax ','a10'
      WRITE(m_out,bxl1i) m_npmax,    'Max. photon mult.  ','npmax ','a11'
      WRITE(m_out,bxl1i) m_KFini,    'Beam ident         ','KFini ','a12'
      WRITE(m_out,bxl1f) m_Emin,     'Manimum phot. ener.','Ene   ','a13'
      WRITE(m_out,bxl1g) m_MasPhot,  'Phot.mass, IR regul','MasPho','a14'
      WRITE(m_out,bxl1g) m_Xenph  ,  'Phot. mult. enhanc.','Xenph ','a15'
      WRITE(m_out,bxl1g) m_Vcut(1),  'Vcut1              ','Vcut1 ','a16'
      WRITE(m_out,bxl1g) m_Vcut(2),  'Vcut2              ','Vcut2 ','a16'
      WRITE(m_out,bxl1g) m_Vcut(3),  'Vcut3              ','Vcut2 ','a16'
      WRITE(m_out,bxl1f) m_PolBeam1(1), 'PolBeam1(1)     ','Pol1x ','a17'
      WRITE(m_out,bxl1f) m_PolBeam1(2), 'PolBeam1(2)     ','Pol1y ','a18'
      WRITE(m_out,bxl1f) m_PolBeam1(3), 'PolBeam1(3)     ','Pol1z ','a19'
      WRITE(m_out,bxl1f) m_PolBeam2(1), 'PolBeam2(1)     ','Pol2x ','a20'
      WRITE(m_out,bxl1f) m_PolBeam2(2), 'PolBeam2(2)     ','Pol2y ','a21'
      WRITE(m_out,bxl1f) m_PolBeam2(3), 'PolBeam2(3)     ','Pol2z ','a22'
      WRITE(m_out,bxclo)

* Check on polarization vectors
      pol1 = SQRT( m_PolBeam1(1)**2+m_PolBeam1(2)**2+m_PolBeam1(3)**2 )
      pol2 = SQRT( m_PolBeam2(1)**2+m_PolBeam2(2)**2+m_PolBeam2(3)**2 )
      IF(       pol1 .GT. 1d0 .OR. pol2 .GT. 1d0 ) THEN
         WRITE(m_out,'(a)') ' ##### STOP in KK2f_Initialize: WRONG SPIN VECTORS '
         WRITE(    *,'(a)') ' ##### STOP in KK2f_Initialize: WRONG SPIN VECTORS '
         STOP
      ENDIF
* Note that getter KK2f_GetIsBeamPolarized exists!
      m_IsBeamPolarized = 1
      IF( (pol1+pol2) .LT. 1d-6 ) m_IsBeamPolarized = 0

      IF(m_KeyWgt.EQ.1 .AND. m_KeyHad.EQ.1 ) THEN
         WRITE(m_out,*) '+++WARNING: for WT=0 events NO hadronization!'
         WRITE(    *,*) '+++WARNING: for WT=0 events NO hadronization!'
      ENDIF
*
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
* Identificator for this generator
      m_idgen = 6
* Important 'signature histo' which remembers crude total x-section
      CALL GLK_Mbook(m_idgen   ,'KK2f signature  $', 1,m_WTmax)
* Tests
      CALL GLK_Mbook(m_Idyfs+40, 'KK2f: Photon raw multiplicity $',10, 0.1d0*m_xpar(19))
*= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
* Basic initialization of brancher, branches are defined in BornV_Initialize
      m_IdBra = m_Idyfs+100
      CALL MBrA_Initialize(m_out,m_IdBra,50,m_WTmax, 'MBrA: KK2f main weight$')
*     Add branch for each final fermion
      DO j=1,20
         IsGenerated = m_xpar(400+j)
         IF( IsGenerated .NE. 0) THEN
            kxp = 500+10*j
            KF    = m_xpar(kxp+1)
            WtMax = m_xpar(kxp+7)
            IF(m_KeyISR .EQ. 0) WtMax = 1d0
            Nbin = 5
            CALL MBrA_AddBranch(KF,Nbin,WTmax,'MBrA: next branch$')
         ENDIF
      ENDDO
*----------------------------------------------------------------------
      CALL  BornV_Initialize( m_xpar)
      CALL KarLud_Initialize(m_xpar,xcru)
      CALL KarFin_Initialize(m_xpar)
      CALL   QED3_Initialize(m_xpar)
      IF(m_KeyGPS .NE. 0 ) THEN
         CALL GPS_Initialize
         CALL GPS_Setb2
         IF( m_IsBeamPolarized .EQ. 1) THEN
            CALL KK2f_WignerIni(m_KFini,m_CMSene,m_PolBeam1,m_PolBeam2, PolBeam1,PolBeam2) 
            WRITE(m_out,bxope)
            WRITE(m_out,bxtxt) 'KK2f: Beam polarizations Wigner rotated '
            WRITE(m_out,bxl1f) PolBeam1(1), 'PolBeam1(1)     ','Pol1x ','=='
            WRITE(m_out,bxl1f) PolBeam1(2), 'PolBeam1(2)     ','Pol1y ','=='
            WRITE(m_out,bxl1f) PolBeam1(3), 'PolBeam1(3)     ','Pol1z ','=='
            WRITE(m_out,bxl1f) PolBeam2(1), 'PolBeam2(1)     ','Pol2x ','=='
            WRITE(m_out,bxl1f) PolBeam2(2), 'PolBeam2(2)     ','Pol2y ','=='
            WRITE(m_out,bxl1f) PolBeam2(3), 'PolBeam2(3)     ','Pol2z ','=='
            WRITE(m_out,bxclo)
         ENDIF
         CALL GPS_SetPolBeams(PolBeam1,PolBeam2)
      ENDIF
*----------------------------------------------------------------------
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) 'KK2f: Initialization '
* Crude normalization in nanobarns
      m_Xcrunb = xcru * BornV_Sig0nb(m_CMSene)
      WRITE(m_out,bxl1g) m_Xcrunb,   'x-crude [nb]       ','Xcrunb','**'
* Note that m_Xborn initialized here is used for KF generation for KeyISR=0
*
* List of properties of generated channels, calculate Xborn list
      CALL MBrA_GetKFlist(Nbranch,KFList)
      CALL MBrA_GetWMList(Nbranch,WMList)
      WRITE(m_out,bxtxt) 'List of final fermions:                '
      DO j=1,Nbranch
         KF = KFList(j)
         Xborn(j)= BornV_Integrated(KF,m_CMSene**2)    !<-- Initialization for tests only
         amferm  = BornV_GetMass(KF)
         vmaxGPS = BornV_GetAuxPar(KF)
         WRITE(m_out,bxl1i) KF       ,'KF of final fermion','KFfin ','**'
         WRITE(m_out,bxl1g) amferm   ,'mass of final ferm.','amferm','**'
         WRITE(m_out,bxl1g) Xborn(j) ,'Xborn [R]          ','Xborn ','**'
         WRITE(m_out,bxl1g) WMlist(j),'WtMax sampling par.','WtMax ','**'
         WRITE(m_out,bxl1g) vmaxGPS  ,'vmax for CEEX      ','vmaxGPS','**'
      ENDDO
      WRITE(m_out,bxclo)

* set generation parameters
* This initialization is used in KeyISR=0 case !!!
      CALL MBrA_SetXSList(Xborn)
*----------------------------------------------------------------------
      CALL TauPair_Initialize(m_xpar)
*----------------------------------------------------------------------
      IF( m_KeyFSR .NE. 0) CALL pyGive("MSTJ(41)=1;")
*----------------------------------------------------------------------
      m_nevgen=0
*----------------------------------------------------------------------
***** CALL GLK_ListPrint(6)     ! debug
      END                       !!! KK2f_Initialize !!!


      SUBROUTINE KK2f_WignerIni(KFbeam,CMSene,PolBeam1,PolBeam2, Polar1,Polar2) 
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Purpose: Wigner rotation for spin polarizations vectors of beams.             //
*//                                                                                 //
*//   For the moment we assume that beam polarization vectors are defined           //
*//   in beam particle rest frames which are reached from CSM by simple             //
*//   z-boost without any rotation. Note that first beam is paralel to z-axis.      //
*//                                                                                 //
*//   Notes:                                                                        //
*//   - Initialization of class BornV and GPS required                              //
*//   - Externals from GPS and Kinlib classes                                       //
*//   - In order to facilitate external tests, KK2f.h is not included (temporarily) //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
*-------------------------------------------------------------------------------------
      INTEGER   KFbeam
      DOUBLE PRECISION     CMSene
      DOUBLE PRECISION     BornV_GetMass, Mbeam
      DOUBLE PRECISION     p1(4),p2(4)
      DOUBLE PRECISION     PolBeam1(4), PolBeam2(4), Polar1(4),Polar2(4)
      INTEGER   i,j,k
      DOUBLE PRECISION     pi,thet,phi,exe
*-------------------------------------------------------------------------------------
      WRITE(*,*) '=====================: KK2f_WignerIni: ============================'

      Mbeam   = BornV_GetMass(KFbeam)
      WRITE(*,*) 'Mbeam= ',Mbeam,CMSene

      CALL KinLib_DefPair(CMSene,Mbeam,Mbeam,p1,p2)

* Initialize GPS tralor
      CALL GPS_TralorPrepare(p1,1)
      CALL GPS_TralorPrepare(p2,2)

      WRITE(*,*) 'KK2f_WignerIni:================beam rest frame====================='
      CALL KinLib_VecPrint(6 ,'PolBeam1',PolBeam1)
      CALL KinLib_VecPrint(6 ,'PolBeam2',PolBeam2)

*/////////////////////////////////////////////////////////////////////////////////////
*//   These two transformations from beam particle frames to CMS define where       //
*//   'machine spin polarization vectors' are primarily defined.                    //
*//   In the present version the transformations are simple boosts along z-axis.    //
*//   To be changed apprioprietly, if we adopt another convention!!!!               //
*/////////////////////////////////////////////////////////////////////////////////////
      exe   = (p1(4)+p1(3))/Mbeam
      CALL KinLib_Boost(3,     exe,PolBeam1,PolBeam1) ! beam1_rest --> CMS
      CALL KinLib_Boost(3, 1d0/exe,PolBeam2,PolBeam2) ! beam2_rest --> CMS
*/////////////////////////////////////////////////////////////////////////////////////

      WRITE(*,*) 'KK2f_WignerIni:=================CMS================================='
      CALL KinLib_VecPrint(6 ,'PolBeam1',PolBeam1)
      CALL KinLib_VecPrint(6 ,'PolBeam2',PolBeam2)

*/////////////////////////////////////////////////////////////////////////////////////
*//   These transformations assures that for calculations with KS spinors we use    //
*//   beam spin polarization vectors in the proper GPS frames of the beam particles //
*/////////////////////////////////////////////////////////////////////////////////////
      CALL GPS_TralorUnDo(   1,PolBeam1,Polar1)     ! CMS --> beam1_rest GPS
      CALL GPS_TralorUnDo(   2,PolBeam2,Polar2)     ! CMS --> beam2_rest GPS

      WRITE(*,*) 'KK2f_WignerIni:=================GPS================================='
      CALL KinLib_VecPrint(6 ,'Polar1  ',Polar1)
      CALL KinLib_VecPrint(6 ,'Polar2  ',Polar2)

      Polar1(4)=1d0             ! in order to get rid of rounding errors
      Polar2(4)=1d0             ! in order to get rid of rounding errors
      END


      SUBROUTINE KK2f_Make
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//   Make one event ISR + FSR                                                      //
*//                                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INCLUDE 'BXformat.h'
*
      DOUBLE PRECISION  xxf(4)
      REAL              rvec(10)
      INTEGER     i,j,k
      INTEGER     LevPri,Ie1Pri,Ie2Pri
      INTEGER     KFfin
      INTEGER     TauIsInitialized
      DOUBLE PRECISION       wt_fsr,wt_isr,WtScaled
      DOUBLE PRECISION       CMSE, vv, svar1, vmaxGPS
      DOUBLE PRECISION       charg2,amfi1,amfi2
      DOUBLE PRECISION       BornV_GetMass,BornV_GetCharge,BornV_GetAuxPar
      DOUBLE PRECISION       BornV_Differential
      DOUBLE PRECISION       WtSetNew(200), WtBest, WtBest1, WtBest2
      DOUBLE PRECISION       rn
      DOUBLE PRECISION       SvarQ, vvQ, Exe
*-----------------------------------------------------------
      m_nevgen   = m_nevgen +1
      m_ypar( 9) = m_nevgen
  100 CONTINUE
      m_WtCrud  = 1d0
      LevPri =m_xpar( 5)  !PrintOut Level 0,1,2,3
      Ie1Pri =m_xpar( 6)  !PrintOut Start point
      Ie2Pri =m_xpar( 7)  !PrintOut End   point
* WtSet reseting to zero
      DO j=1,m_lenwt
         m_WtSet(j)  =0d0
         m_WtList(j) =0d0
      ENDDO
* =============================================
*                   ISR
* =============================================
* define p1,2, xxf, xf1,2 and photons
* note that xf1,xf2 are not used anymore
* Different Final state masses supported (for W pair production)
      CALL KarLud_Make(xxf,wt_ISR)
      m_WtCrud  = m_WtCrud*wt_ISR

* Actual KFcode of final fermion
      CALL MBrA_GetKF(KFfin)
      m_ypar(400) = KFfin  ! temporary debug

*   Control printout
      IF(LevPri .GE. 2) CALL KarLud_Print(m_nevgen,Ie1Pri,Ie2Pri)
* =============================================
*                   FSR
* =============================================
* Generate FSR photons and final fermion momenta.
* xxf defines frame (Z-frame) of final fermions + FSR photons.
* (Fermion momenta from KarLud_Make are not used, even for pure ISR)
      IF(m_WtCrud .NE. 0d0) THEN
         charg2 =BornV_GetCharge(KFfin)**2
         amfi1  =BornV_GetMass(KFfin)
         amfi2  =BornV_GetMass(KFfin)
         CALL KarFin_Make(xxf,amfi1,amfi2,charg2,wt_FSR)
         m_WtCrud  = m_WtCrud*wt_FSR
      ENDIF
*   Control printout
      IF(LevPri .GE. 2) CALL KarFin_Print(m_nevgen,Ie1Pri,Ie2Pri)
* ==============================================================================
* Merging photons in CMS frame. The common list from merging
* is used in GPS package. It is not used in QED3. Merge is BEFORE ZBoostAll.
* ==============================================================================
      IF(m_WtCrud .NE. 0d0)  CALL KK2f_Merge
      IF(m_WtCrud .NE. 0d0)  CALL KK2f_MakePhelRand         !<-- Generate photon helicities
* Control printout ISR+FSR
      IF(LevPri .GE. 1) CALL KK2f_Print(Ie1Pri,Ie2Pri)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// All four-momenta are constructed and recorded at this point.                    //
*//                                                                                 //
*// The generated distribution with the weight WtCrud represents the differential   //
*// distribution equal to phase space times all ISR S-factor times FSR S-factors    //
*// times 'Born(s*(1-v),cosheta)'                                                   //
*//                                                                                 //
*// The 'Born' angular distribution in xxf frame (in terms of Euler angles) is in   //
*// present version exactly flat (generated in KarFin).                             //
*// The distribution m_BornCru represents the above  'Born(s*(1-v),cosheta)'.       //
*// It is remodeled later on during calculation of the QED mat. elem. in QED3.      //
*//                                                                                 //
*// Memorizing m_BornCru makes sense because we  may freely manipulate in QED3      //
*// with input parameters like MZ, couplings etc. (recalculate weights for event)   //
*// This trick is not working for GPS where  BornCru is calculated internaly        //
*//                                                                                 //
*// The weight from QED3/GPS is not modifying Z position any more, see also KarLud  //
*/////////////////////////////////////////////////////////////////////////////////////
* =============================================================
*                    Model weight
* =============================================================
      m_WtMain  =m_WtCrud
      IF(m_WtCrud .NE. 0d0) THEN
         CALL BornV_GetVV(vv)
         CALL KarLud_GetXXXene(CMSE)            !<-- It is this realy OK, see above
         svar1     = CMSE**2*(1d0-vv)
         m_BornCru = 4d0/3d0*BornV_Differential(0,KFfin,svar1,0d0,0d0,0d0,0d0,0d0)
         CALL QED3_Make                         !<-- EEX
* WtSet from QED3 is filled in the range (1:200)
         CALL QED3_GetWtSet(WtBest,m_WtSet)     !<-- WtBest initialized
* New CEEX matrix element is now default for leptons and for quarks.
* Its use is controled by auxiliary parameter vmaxGPS (temporary solutions)
* CEEX is calculated twice, with ISR*FSR interference OFF and ON
         CALL  KarFin_GetSvarQ(SvarQ)
         vvQ = 1-SvarQ/CMSE**2
         vmaxGPS = BornV_GetAuxPar(KFfin)
         IF( m_KeyGPS.NE.0 .AND. vvQ.LT.vmaxGPS ) THEN
            CALL GPS_ZeroWtSet                 !<-- zeroing GPS weights
            CALL GPS_SetKeyINT( 0)             !<-- ISR*FSR interfer. OFF
            CALL GPS_Make                      !<-- CEEX    interfer. OFF
            IF( m_KeyINT .NE. 0 ) THEN
               CALL GPS_SetKeyINT( m_KeyINT)   !<-- ISR*FSR interfer. ON
               CALL GPS_Make                   !<-- CEEX    interfer. ON
            ENDIF
            CALL GPS_GetWtSet(WtBest,WtSetNew) !<-- WtBest redefined !!!
* m_WtSet appended with WtSetNew
            DO j=1,200
               m_WtSet(j+200) = WtSetNew(j)
            ENDDO
         ENDIF
         m_WtMain  = m_WtMain*WtBest
      ENDIF
*   Control printout
      IF(LevPri .GE. 2) THEN
         CALL  QED3_wtPrint(' KK2f ',m_out,m_nevgen,Ie1Pri,Ie2Pri,wt_ISR,wt_FSR,WtBest,m_WtSet) !
      ENDIF
*///////////////////////////////////////////////////////////////
*//                                                           //
*//     Optional rejection according to principal weight      //
*//                                                           //
*///////////////////////////////////////////////////////////////
      IF(m_KeyWgt .EQ. 0) THEN              !!! CONSTANT-WEIGHT events
         CALL PseuMar_MakeVec(rvec,1)
         rn = rvec(1)
         CALL GLK_Mfill(m_idgen, m_Xcrunb*m_WTmax, rn)
         CALL MBrA_Fill(m_WtMain   ,rn)
         WtScaled = m_WtMain/m_WTmax
         IF( WtScaled .GT. 1d0) THEN
            m_WtMain = WtScaled
         ELSE
            m_WtMain = 1.d0
         ENDIF
         IF(rn .GT. WtScaled) GOTO 100
         m_WtCrud=1d0
* collection of the weights for the advanced user
         DO j=1,m_lenwt
            m_WtList(j) = m_WtSet(j)/WtBest ! Division by zero impossible due to rejection
         ENDDO
      ELSE                                  !!! VARIABLE-WEIGHT events
         CALL GLK_Mfill(m_idgen   , m_Xcrunb, 0d0)
         CALL MBrA_Fill(m_WtMain,  0d0)
* collection of the weights for the advanced user
         DO j=1,m_lenwt
            m_WtList(j) = m_WtSet(j)*m_WtCrud
         ENDDO
      ENDIF
* =============================================================
* =============================================================
* Some test
      CALL GLK_Mfill(m_Idyfs+40, 1d0*m_nphot,   rn)
* =============================================================
      IF(m_KeyWgt .EQ. 0) THEN
* wt_ISR,2  weights are reset to one
         wt_ISR=1d0
         wt_FSR=1d0
      ENDIF
      m_ypar(1)=m_WtMain        ! Main Total Weight  ! debug
      m_ypar(2)=m_WtCrud        ! Total Crude weight ! debug
      m_ypar(3)=wt_ISR          ! reintroduction     ! debug
      m_ypar(4)=wt_FSR          !                      debug
*/////////////////////////////////////////////////////////////////////////////////////
*//         Fill standard HEP and/or LUND common blocks and HADRONIZE               //
*/////////////////////////////////////////////////////////////////////////////////////
      CALL KarLud_GetExe(Exe)
      CALL KK2f_ZBoostAll(  exe)
      CALL KarLud_ZBoostAll(exe)
      CALL KarFin_ZBoostAll(exe)
*
      IF( m_WtMain .NE. 0d0) CALL HepEvt_Fill
      IF( m_WtMain .NE. 0d0 .AND. m_KeyHad .EQ. 1 ) THEN
         CALL HepEvt_Hadronize(m_HadMin)
      ENDIF
* =================================================================
* Tau decays using tauola, all spin effects implemented!
      IF(m_WtCrud .NE. 0d0) THEN
         IF( ABS(KFfin) .EQ. 15) THEN
            CALL TauPair_GetIsInitialized(TauIsInitialized)
            IF( TauIsInitialized .NE. 0) THEN
               IF(m_KeyGPS .EQ. 0 ) THEN
                  WRITE(m_out,*) ' #### STOP in KK2f_Make: for tau decays GPS not activated !!!'
                  WRITE(    *,*) ' #### STOP in KK2f_Make: for tau decays GPS not activated !!!'
                  STOP
               ENDIF
               CALL TauPair_Make1        ! tau decay generation
               CALL TauPair_ImprintSpin  ! introduction of spin effects by rejection
               CALL TauPair_Make2        ! the remaining book-keeping and Photos
            ENDIF
         ENDIF
      ENDIF
      END                       !!! end of KK2f_Make !!!

      SUBROUTINE KK2f_ZBoostAll(exe)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   performs z-boost on all momenta of the event                            //
*//   this z-boost corresponds to beamstrahlung or beamspread                 //
*//   and is done at the very end of generation, after m.el. calculation      //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION  exe
      INTEGER           j,k
      DOUBLE PRECISION  ph(4)
*
      IF( exe.EQ. 1d0) RETURN
      CALL KinLib_Boost(3,exe,m_p1,m_p1)
      CALL KinLib_Boost(3,exe,m_p2,m_p2)
      CALL KinLib_Boost(3,exe,m_q1,m_q1)
      CALL KinLib_Boost(3,exe,m_q2,m_q2)
      DO j=1,m_nphot
         DO k=1,4
            ph(k) = m_sphot(j,k)
         ENDDO
         CALL KinLib_Boost(3,exe,ph,ph)
         DO k=1,4
            m_sphot(j,k) = ph(k)
         ENDDO
      ENDDO
      END

      SUBROUTINE KK2f_Finalize
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Final bookkeping and printouts                                                //
*//   Normalization available through getter KK2f_GetXsecMC(xSecPb, xErrPb)         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'KK2f.h'
*
      INTEGER     LevelPrint
      DOUBLE PRECISION       errela,averwt
      DOUBLE PRECISION       BornV_Integrated
      DOUBLE PRECISION       BornV_Sig0nb
      DOUBLE PRECISION       xkarl,error,erkarl,erabs
      DOUBLE PRECISION       xsmc,erel
      DOUBLE PRECISION       sig0pb,xBorPb
      DOUBLE PRECISION       avmlt,ermlt,upmlt
      DOUBLE PRECISION       WTsup, AvUnd, AvOve
      DOUBLE PRECISION       ROverf, RUnder
*-------------------------------------------------------------------
      LevelPrint=2
      sig0pb =  BornV_Sig0nb(m_CMSene)*1000

* Born xsec, just for orientation whwre we are...
      xBorPb =  BornV_Integrated(0,m_CMSene**2) * sig0pb

* Crude from karLud + printout
      CALL KarLud_Finalize(LevelPrint,xkarl,error)
      erkarl = 0d0

* Printout from Karfin
      CALL KarFin_Finalize(LevelPrint)

* Average of the main weight
      CALL GLK_MgetAve(m_idbra, AverWt, ErRela, WtSup)

* main X-section = crude * <WtMain>
      xsmc   =  xkarl*averwt
      erel   =  SQRT(erkarl**2+errela**2)
      erabs  =  xsmc*erel
*============================================================
* The final cross section exported to user
* through getter KK2f_GetXsecMC(xSecPb, xErrPb)
      m_xSecPb =  xsmc*sig0pb     ! MC xsection in picobarns
      m_xErrPb =  m_xSecPb*erel     ! Its error   in picobarns
*============================================================

* no printout for LevelPrint =1
      IF(LevelPrint .LE. 1) RETURN
*
* print photon multiplicity distribution
      CALL  GLK_Mprint(m_Idyfs+40)
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '  KK2f_Finalize  printouts '
      WRITE(m_out,bxl1f) m_cmsene,   'cms energy total   ','cmsene','a0'
      WRITE(m_out,bxl1i) m_nevgen,   'total no of events ','nevgen','a1'
      WRITE(m_out,bxtxt) '** principal info on x-section **'
      WRITE(m_out,bxl2f) xsmc,erabs, 'xs_tot MC R-units  ','xsmc  ','a1'
      WRITE(m_out,bxl1f) m_xSecPb,   'xs_tot    picob.   ','xSecPb','a3'
      WRITE(m_out,bxl1f) m_xErrPb,   'error     picob.   ','xErrPb','a4'
      WRITE(m_out,bxl1f) erel,       'relative error     ','erel  ','a5'
      WRITE(m_out,bxl1f) WTsup ,    'WTsup, largest WT  ','WTsup ','a10'
      WRITE(m_out,bxtxt) '** some auxiliary info **'
      WRITE(m_out,bxl1f) xBorPb,     'xs_born   picobarns','xborn','a11'
      CALL GLK_MgetAve(m_Idyfs+40, avmlt, ermlt, upmlt)
      WRITE(m_out,bxl1f) avmlt,      'Raw phot. multipl. ','     ','==='
      WRITE(m_out,bxl1f) upmlt,      'Highest phot. mult.','     ','==='
      WRITE(m_out,bxtxt) '  End of KK2f  Finalize  '
      WRITE(m_out,bxclo)

* Print more on the main weight
      CALL MBrA_Print0
* Print even more on the weight in each branch!
      CALL MBrA_Print1
*--------------------------------------------------------------------------------
      CALL TauPair_Finalize
      END



      SUBROUTINE  KK2f_DsigOverDtau(mout,Rho)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   !!! This routine is only for documentation and testing purposes !!!           //
*//                                                                                 //
*//   The distribution DsigOverDtau corresponding to WtCrud                         //
*//   Normalized with respect to dTau = Lorenz invariant phase space                //
*//                                                                                 //
*//   Photons attributed to ISR or FSR as in MC and interference ISR/FSR absent.    //
*//                                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION      Rho
      INTEGER    mout
*
      DOUBLE PRECISION     pi
      PARAMETER (pi =3.1415926535897932d0)
      INTEGER    i,j,k
      DOUBLE PRECISION      PP(4),PX(4),PQ(4),Pho(4)
      DOUBLE PRECISION      SfacIniProd, SfacIni
      DOUBLE PRECISION      SfacFinProd, SfacFin
      DOUBLE PRECISION      kp1, kp2, kq1, kq2
      DOUBLE PRECISION      alfQED, Jcur(4)
      DOUBLE PRECISION      svar, svarX, svarQ, Massf, Mas1, Mas2, Mbeam
      DOUBLE PRECISION      ChaIni, ChaFin, xBorn, betaf, fLLux
      DOUBLE PRECISION      vv, vx,vq
      INTEGER    KFfin
      DOUBLE PRECISION      BornV_GetMass, BornV_GetCharge, BornV_Simple
      DOUBLE PRECISION      YFSkonIni, YFS_IRini, YFSkonFin, YFS_IRfin, YFS_isr, YFS_fsr, Yisr, Yfsr
      DOUBLE PRECISION      BVR_SForFac
      DOUBLE PRECISION      alfpi, alfpini, alfpfin, e_QED
      DOUBLE PRECISION      SfaSpi
      DOUBLE COMPLEX  GPS_soft
      INTEGER    nout
*-----------------------------------------------
      INTEGER   Icont
      SAVE      Icont
      DATA      Icont /0/
*-----------------------------------------------
      IF(Icont .GE. 500 )  RETURN
      nout = mout
      alfQED  = 1d0/m_alfinv
      e_QED  = DSQRT( 4d0*pi*alfQED)
* Actual KFcode of final fermion
      CALL MBrA_GetKF(KFfin)
      Massf  = BornV_GetMass(KFfin)
      Mas1  = Massf
      Mas2  = Massf
      Mbeam = BornV_GetMass(m_KFini)
* Final/initial state charges, (-1 for electron)
      ChaFin = BornV_GetCharge(KFfin)
      ChaIni = BornV_GetCharge(m_KFini)
      alfpi   = alfQED/pi
      alfpini  = alfpi*ChaIni**2
      alfpfin  = alfpi*ChaFin**2
* Product of ISR factors
      DO k=1,4
         PP(k) = m_p1(k) +m_p2(k)
         PX(k) = m_p1(k) +m_p2(k)
         PQ(k) = m_q1(k) +m_q2(k)
      ENDDO
      svar  = PP(4)*PP(4) -PP(3)*PP(3) -PP(2)*PP(2) -PP(1)*PP(1)
      IF( svar .LE. (2*Massf)**2 ) GOTO 900
*   //////////////////////////////////////////
*   //            S-factors                 //
*   //////////////////////////////////////////
      SfacIniProd = 1d0
      DO i=1,m_nphot
         IF( m_isr(i) .EQ. 1 ) THEN   ! select ISR
            DO k=1,4
               Pho(k) = m_sphot(i,k)
               PX(k)  = PX(k) -Pho(k)
            ENDDO
            kp1 = m_p1(4)*Pho(4)-m_p1(3)*Pho(3)-m_p1(2)*Pho(2)-m_p1(1)*Pho(1)
            kp2 = m_p2(4)*Pho(4)-m_p2(3)*Pho(3)-m_p2(2)*Pho(2)-m_p2(1)*Pho(1)
            DO k=1,4
               Jcur(k)  = m_p1(k)/kp1 -m_p2(k)/kp2
            ENDDO
            SfacIni = -(ChaIni**2 *alfQED/(4*pi**2))*(Jcur(4)**2 -Jcur(3)**2-Jcur(2)**2-Jcur(1)**2)
            SfacIniProd = SfacIniProd *SfacIni
*///////////////////////////
***            SfaSpi = 1/2d0 *1d0/(2d0*pi)**3  *CDABS( e_QED *GPS_soft(1,Pho,m_p1,m_p2) )**2
***            SfaSpi = 2d0*SfaSpi       !! factor 2 for two +- photon helicities
***            WRITE(*,'(a,6e20.14)') '/// SfacIni,SfaSpi = ', SfacIni,SfaSpi/SfacIni
*///////////////////////////
         ENDIF
      ENDDO
* Product of FSR factors
      SfacFinProd = 1d0
      DO i=1,m_nphot
         IF( m_isr(i) .EQ. 0 ) THEN   ! select FSR
            DO k=1,4
               Pho(k) = m_sphot(i,k)
            ENDDO
            kq1 = m_q1(4)*Pho(4)-m_q1(3)*Pho(3)-m_q1(2)*Pho(2)-m_q1(1)*Pho(1)
            kq2 = m_q2(4)*Pho(4)-m_q2(3)*Pho(3)-m_q2(2)*Pho(2)-m_q2(1)*Pho(1)
            DO k=1,4
               Jcur(k)  = m_q1(k)/kq1 -m_q2(k)/kq2
            ENDDO
            SfacFin = -(ChaFin**2 *alfQED/(4*pi**2))*(Jcur(4)**2 -Jcur(3)**2-Jcur(2)**2-Jcur(1)**2)
            SfacFinProd = SfacFinProd *SfacFin
*///////////////////////////
***         SfaSpi =1/2d0 *1d0/(2d0*pi)**3 *(ChaFin*e_QED *CDABS(GPS_soft(1,Pho,m_q1,m_q2)))**2
***         SfaSpi = 2d0*SfaSpi        !! factor 2 for two +- photon helicities ?
***         WRITE(*,'(a,6e20.14)') '/// SfacFin,SfaSpi = ', SfacFin, SfaSpi/SfacFin
*///////////////////////////
         ENDIF
      ENDDO
      svarX = PX(4)*PX(4) -PX(3)*PX(3) -PX(2)*PX(2) -PX(1)*PX(1)
      svarQ = PQ(4)*PQ(4) -PQ(3)*PQ(3) -PQ(2)*PQ(2) -PQ(1)*PQ(1)
      CALL BornV_GetVV(vv)
      vx    = 1d0 -svarX/svar
      vq    = 1d0 -svarQ/svar
      IF( svarQ .LE. (2*Massf)**2 ) GOTO 900
*   //////////////////////////////////////////
*   //              Born                    //
*   //////////////////////////////////////////
      xBorn = BornV_Simple( m_KFini,KFfin,svarX, 0d0  ) *(svar/svarX) !!<- Born(svarX)*svar
      xBorn = xBorn/(4*pi)
*   //////////////////////////////////////////
*   //              FormFactors             //
*   //////////////////////////////////////////
* Finaly formfactors, provided common IR sphere in CMS
* Equivalent alternatives:  Yisr==YFS_isr  and  Yfsr==YFS_fsr
* YFSkon imported from BornV and KarFin(piatek), unused there, not included in WtCrude!
* YFS_IR is included in WtCrude (Karfin) and normalization (Karlud, BornV).
      CALL  BornV_GetYFS_IR( YFS_IRini )
      CALL  BornV_GetYFSkon( YFSkonIni )
      YFS_isr =  YFS_IRini*YFSkonIni
      CALL KarFin_GetYFS_IR( YFS_IRfin )
      CALL KarFin_GetYFSkon( YFSkonFin )
      YFS_fsr =  YFS_IRfin*YFSkonFin
      Yisr= BVR_SForFac(alfpini, m_p1,Mbeam, m_p2,Mbeam, m_Emin, m_MasPhot)
      Yfsr= BVR_SForFac(alfpfin, m_q1,Mas1,  m_q2,Mas2,  m_Emin, m_MasPhot)
*   //////////////////////////////////////////
*   //              Other                   //
*   //////////////////////////////////////////
      betaf = DSQRT( 1d0 - 4*Massf**2/svarQ )
      fLLux = (svarX/svarQ)
*   //////////////////////////////////////////
*   //              Total                   //
*   //////////////////////////////////////////
      Rho = SfacIniProd            !! product of ISR formfactors
     $     *SfacFinProd            !! product of FSR formfactors
     $     *xBorn                  !! Born x-section dependend on vv
     $     *2d0/betaf              !! 2-body phase space factor (inverse of it)
     $     *fLLux                  !! LL 'flux-factor'
     $     *Yisr                   !! YFS full formfactor, ISR part
     $     *Yfsr                   !! YFS full formfactor, FSR part
*/////////////////////////////////////////////////////////////////////////////////
*//                            X-Checks                                         //
*/////////////////////////////////////////////////////////////////////////////////
**      IF(vq  .LE. 0.3d0)  RETURN
**      IF(vq  .GE. 0.9d0)  RETURN
      WRITE(nout,'(a,5g20.12)') '////////////// KK2f_DsigOverDtau ///////////////'
      WRITE(nout,'(a,5g20.12)') '/// SfacIniProd*SfacFinProd  = ', SfacIniProd*SfacFinProd
      WRITE(nout,'(a,5g20.12)') '///   vx,vq = ',vx,vq, vv/vx, betaf
      WRITE(nout,'(a,5g20.12)') '///   Yisr  = ',Yisr,YFS_isr,Yisr/YFS_isr
      WRITE(nout,'(a,5g20.12)') '///   Yfsr  = ',Yfsr,YFS_fsr,Yfsr/YFS_fsr
      WRITE(nout,'(a,5g20.12)') '///   Rho   = ',Rho
      WRITE(nout,'(a,5g20.12)') '//////////////////////////////////////////////////'
*/////////////////////////////////////////////////////////////////////////////////
      Icont =Icont +1
      RETURN
 900  CONTINUE
      Rho = 0d0
      END                       !!! KK2f_DsigOverDtau !!!

      SUBROUTINE  KK2f_Merge
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Merging ISR and FSR photon momenta.                                            //
*//  Photons are ordered according to energy (in CMS)                               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER nphox,nphoy
      DOUBLE PRECISION   xphot(m_phmax,4),yphot(m_phmax,4),enex,eney
      INTEGER i,k,i1,i2
*-----------------------------------------------------------------------
      CALL KarLud_GetBeams(    m_p1, m_p2)
      CALL KarFin_GetFermions( m_q1, m_q2)
*
      CALL KarLud_GetPhotons(nphox,xphot)
      CALL KarFin_GetPhotons(nphoy,yphot)
*
      m_nphot  = nphox +nphoy
      i1=1
      i2=1
      DO i=1,m_nphot
         enex = 0d0
         eney = 0d0
* saveguard against Alex Read and Tiziano Camporesi effect (bug in old BHLUMI)
         IF(i1 .LE. nphox) enex = xphot(i1,4)
         IF(i2 .LE. nphoy) eney = yphot(i2,4)
         IF(enex .GT. eney) THEN
            DO k=1,4
               m_sphot( i,k) = xphot(i1,k)
            ENDDO
            m_isr(i) = 1        ! ISR origin
            i1=i1+1    
         ELSE
            DO k=1,4
               m_sphot( i,k) = yphot(i2,k) ! FSR
            ENDDO
            m_isr(i) = 0        ! FSR origin
            i2=i2+1    
         ENDIF
      ENDDO
      END


      SUBROUTINE KK2f_MakePhelRand
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Generate photon helicities randomly                                     //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER   i
      REAL                 rvec(m_phmax)
*
      IF(m_nphot .LE. 0) RETURN
      CALL PseuMar_MakeVec(rvec,m_nphot)
      DO i=1,m_nphot
         IF( rvec(i) .GT. 0.5d0 ) THEN
            m_Phel(i)=0
         ELSE
            m_Phel(i)=1
         ENDIF
      ENDDO
      END


      SUBROUTINE  KK2f_Print(ie1,ie2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Prints out four momenta of Beams and Final state particles,                    //
*//  and the serial number of event m_nevgen on unit m_out                          //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION   p1(4),p2(4),q1(4),q2(4),sphum(4)
      CHARACTER*8 txt
      DOUBLE PRECISION    sum(4),amf1,amf2,ams,amph
      INTEGER  i,k,KFfin,ie1,ie2
*-----------------------------------------------------------------
* Actual KFcode of final fermion
      CALL MBrA_GetKF(KFfin)
* Fermion momenta
      CALL KarLud_GetBeams(p1,p2)
      CALL KarFin_GetFermions(q1,q2)
*
      txt = ' KK2f '
      IF( (m_nevgen .GE. ie1) .AND. (m_nevgen .LE. ie2) ) THEN
         CALL  KK2f_Print1(m_out)
      ENDIF
      END


      SUBROUTINE  KK2f_Print1(nout)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Prints out four momenta of Beams and Final state particles,                    //
*//  and the serial number of event m_nevgen on unit nout                           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      INTEGER     nout
      CHARACTER*8 txt
      DOUBLE PRECISION    sum(4),amf1,amf2,ams,amph
      INTEGER  i,k,KFfin
*-----------------------------------------------------------------
* Actual KFcode of final fermion
      CALL MBrA_GetKF(KFfin)
*
      txt = ' KK2f '
      WRITE(nout,*) '================== ',txt,' ======================>',m_nevgen
*     
      amf1 = m_p1(4)**2-m_p1(3)**2-m_p1(2)**2-m_p1(1)**2
      amf1 = sqrt(abs(amf1))
      amf2 = m_p2(4)**2-m_p2(3)**2-m_p2(2)**2-m_p2(1)**2
      amf2 = sqrt(abs(amf2))
      WRITE(nout,3100) 'm_p1',(  m_p1(  k),k=1,4),amf1
      WRITE(nout,3100) 'm_p2',(  m_p2(  k),k=1,4),amf2
*     
      amf1 = m_q1(4)**2-m_q1(3)**2-m_q1(2)**2-m_q1(1)**2
      amf1 = sqrt(abs(amf1))
      amf2 = m_q2(4)**2-m_q2(3)**2-m_q2(2)**2-m_q2(1)**2
      amf2 = sqrt(abs(amf2))
      WRITE(nout,3100) 'm_q1',(  m_q1(  k),k=1,4),amf1,KFfin
      WRITE(nout,3100) 'm_q2',(  m_q2(  k),k=1,4),amf2,KFfin
*     
      DO i=1,m_nphot
         amph = m_sphot(i,4)**2-m_sphot(i,3)**2 -m_sphot(i,2)**2-m_sphot(i,1)**2
         amph = sqrt(abs(amph))
         WRITE(nout,3100) 'pho',(m_sphot(i,k),k=1,4),amph
      ENDDO
      DO k=1,4
         sum(k)=m_q1(k)+m_q2(k)
      ENDDO
      DO i=1,m_nphot
         DO k=1,4
            sum(k)=sum(k)+m_sphot(i,k)
         ENDDO
      ENDDO
      ams = sum(4)**2-sum(3)**2-sum(2)**2-sum(1)**2
      ams = sqrt(abs(ams))
      WRITE(nout,3100) 'sum',(  sum(  k),k=1,4),ams
 3100 FORMAT(1x,a3,1x,5f20.14,i5)
      END


      SUBROUTINE KK2f_GetOneY(j,y)
*/////////////////////////////////////////////////////////////////////////////////////
*//   obsolete                                                                      //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION  y
      INTEGER j
*---------------------------------------------------------------
      y = m_ypar(j)
      END   ! KK2f_GetOneY


      SUBROUTINE KK2f_SetOneY(j,y)
*/////////////////////////////////////////////////////////////////////////////////////
*//   obsolete                                                                      //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION  y
      INTEGER j
*---------------------------------------------------------------
      m_ypar(j) =y
      END   ! KK2f_GetOneY


      SUBROUTINE KK2f_GetWt(WtMain,WtCrud)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Main weights                                                                  //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION    WtMain,WtCrud
      WtMain = m_WtMain  ! the best total weight
      WtCrud = m_WtCrud  ! Crude weight (helps to avoid bad events)
      END ! KK2f_GetWt


      SUBROUTINE KK2f_GetWtCrud(WtCrud)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Main weights                                                                  //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION    WtCrud
      WtCrud = m_WtCrud  ! Crude weight (helps to avoid bad events)
      END ! KK2f_GetWtCrud


      SUBROUTINE KK2f_GetWtAll(WtMain,WtCrud,WtSet)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Weights ALL                                                                   //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      INTEGER  j
      DOUBLE PRECISION    WtMain,WtCrud,WtSet(*)
      DOUBLE PRECISION    WtBest
*--------------------------------------------------------------
      WtMain = m_WtMain  ! the best total weight
      WtCrud = m_WtCrud  ! Crude weight (helps to avoid bad events)
      DO j=1,m_lenwt
         WtSet(j) = m_WtSet(j)
      ENDDO
      END ! KK2f_GetWtAll

      SUBROUTINE KK2f_GetWtList(WtMain,WtList)
*/////////////////////////////////////////////////////////////////////////////////////
*//   ALL Weights, also works for weighted eevents!!!!                              //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      INTEGER  j
      DOUBLE PRECISION    WtMain,WtList(*)
*--------------------------------------------------------------
      WtMain = m_WtMain  ! the best total weight
      DO j=1,m_lenwt
         WtList(j) = m_WtList(j)
      ENDDO
      END ! KK2f_GetWtAll


      SUBROUTINE KK2f_GetPhoton1(iphot,phot)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   get i-th photon momentum                                                //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      SAVE
      INTEGER iphot
      DOUBLE PRECISION   phot(4)
      INTEGER k
*
      DO k=1,4
         phot(k) = m_sphot(iphot,k)
      ENDDO
      END

      SUBROUTINE KK2f_GetPhotAll(NphAll,PhoAll)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Get all photons, note that they are ordered in energy                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      INTEGER  NphAll
      DOUBLE PRECISION    PhoAll(m_phmax,4) ! Now we have m_phmax=100
      INTEGER  j,k
*------------------
      NphAll = m_nphot
      DO j=1,m_nphot
         DO k=1,4
            PhoAll(j,k) = m_sphot(j,k)
         ENDDO
      ENDDO
      END                       !!! KK2f_GetPhotAll !!!

      SUBROUTINE KK2f_GetNphot(Nphot)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Get total photon multiplicity                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      INTEGER  Nphot
*------------------
      Nphot = m_nphot
      END                       !!! KK2f_GetNphot !!!

      SUBROUTINE KK2f_GetIsr(isr)
*/////////////////////////////////////////////////////////////////////////////////////
*//   ISR/FSR markers of all photons                                                //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER  isr(*),j
*--------
      DO j=1,m_nphot
         isr(j) = m_isr(j)
      ENDDO
      END                       !!! KK2f_GetIsr !!!

      SUBROUTINE KK2f_GetPhel(Phel)
*/////////////////////////////////////////////////////////////////////////////////////
*//   ISR/FSR markers of all photons                                                //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER  Phel(*),j
*--------
      DO j=1,m_nphot
         Phel(j) = m_Phel(j)
      ENDDO
      END                       !!! KK2f_GetPhel !!!

      SUBROUTINE KK2f_GetIsBeamPolarized(IsBeamPolarized)
*/////////////////////////////////////////////////////////////////////////////////////
*//   ISR/FSR markers of all photons                                                //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER  IsBeamPolarized
*--------
      IsBeamPolarized = m_IsBeamPolarized
      END                       !!! KK2f_GetIsBeamPolarized !!!


      SUBROUTINE KK2f_GetBornCru(BornCru)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Memorizing m_BornCru makes sense because we  may freely manipulate in QED3      //
*// with input parameters like MZ, couplings etc. (recalculate weights for event)   //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION  BornCru
*------------------
      BornCru = m_BornCru
      END

      SUBROUTINE KK2f_GetKeyISR(KeyISR)
*/////////////////////////////////////////////////////////////////////////////////////
*//   ISR switch                                                                    //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER KeyISR
*
      KeyISR = m_KeyISR
      END

      SUBROUTINE KK2f_GetKeyFSR(KeyFSR)
*/////////////////////////////////////////////////////////////////////////////////////
*//   FSR switch                                                                    //
*//   called in BornV                                                               //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER KeyFSR
*
      KeyFSR = m_KeyFSR
      END

      SUBROUTINE KK2f_GetKeyINT(KeyINT)
*/////////////////////////////////////////////////////////////////////////////////////
*//   ISR*FSR interference switch                                                   //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER KeyINT
*
      KeyINT = m_KeyINT
      END

      SUBROUTINE KK2f_GetKeyGPS(KeyGPS)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Get CEEX level switch                                                         //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER KeyGPS
*
      KeyGPS = m_KeyGPS
      END


      SUBROUTINE KK2f_GetKeyWgt(KeyWgt)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Get CEEX level switch                                                         //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER KeyWgt
*
      KeyWgt = m_KeyWgt
      END

      SUBROUTINE KK2f_GetKFini(KFini)
*/////////////////////////////////////////////////////////////////////////////////////
*//   KF of beams                                                                   //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER KFini
*
      KFini = m_KFini
      END

      SUBROUTINE KK2f_GetIdyfs(Idyfs)
*/////////////////////////////////////////////////////////////////////////////////////
*//   pointer for histograms                                                        //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      INTEGER Idyfs
*
      Idyfs = m_Idyfs
      END

      SUBROUTINE KK2f_GetBeams(p1,p2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Four-momenta of beams                                                         //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION   p1(4),p2(4)
      INTEGER k
*
      DO k=1,4
         p1(k) = m_p1(k)
         p2(k) = m_p2(k)
      ENDDO
      END

      SUBROUTINE KK2f_GetFermions(q1,q2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   final state fermion four-momenta                                              //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION   q1(4),q2(4)
      INTEGER k
*
      DO k=1,4
         q1(k) = m_q1(k)
         q2(k) = m_q2(k)
      ENDDO
      END

      SUBROUTINE KK2f_GetVcut(Vcut)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Technical cuts for consecutive beta's                                         //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
*
      DOUBLE PRECISION   Vcut(3)
      INTEGER k
*
      DO k=1,3
         Vcut(k) = m_Vcut(k)
      ENDDO
      END

      SUBROUTINE KK2f_GetCMSene(CMSene)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Photon minimum energy in LAB system                                           //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   CMSene
*
      CMSene = m_CMSene
      END

      SUBROUTINE KK2f_GetEmin(Emin)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Photon minimum energy in LAB system                                           //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   Emin
*
      Emin = m_Emin
      END

      SUBROUTINE KK2f_GetMasPhot(MasPhot)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Photon mass for virtual corrections                                           //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   MasPhot
*
      MasPhot = m_MasPhot
      END

      SUBROUTINE KK2f_GetXenph(Xenph)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Enhancement factor in crude photon multiplicity                               //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   Xenph
*
      Xenph = m_Xenph
      END

      SUBROUTINE KK2f_GetPolBeam1(PolBeam1)
*/////////////////////////////////////////////////////////////////////////////////////
*//   FIRST beam spin polarization                                                  //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   PolBeam1(4)
      INTEGER j
*
      DO j=1,4
         PolBeam1(j) = m_PolBeam1(j)
      ENDDO
      END

      SUBROUTINE KK2f_GetPolBeam2(PolBeam2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   SECOND beam spin polarization                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   PolBeam2(4)
      INTEGER j
*
      DO j=1,4
         PolBeam2(j) = m_PolBeam2(j)
      ENDDO
      END

      SUBROUTINE KK2f_GetXsecMC(xSecPb, xErrPb)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Photon minimum energy in LAB system                                           //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   xSecPb, xErrPb
*
      xSecPb = m_xSecPb
      xErrPb = m_xErrPb
      END

      SUBROUTINE KK2f_GetVersion(Version)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Get VERSION number of the program                                             //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      DOUBLE PRECISION   Version
*
      Version = m_Version
      END

      SUBROUTINE KK2f_GetDate(Date)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Get VERSION date of the program                                             //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KK2f.h'
      CHARACTER*14   Date
*
      Date = m_Date
      END

*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//                      End of Class  KK2f                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                     Pseudo-CLASS  KarFin                                        //
*//                                                                                 //
*//   Purpose:                                                                      //
*//   Top level  Monte-Carlo event generator for FSR radiadion.                     //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE KarFin_Initialize(xpar_input)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    Initialization of input and internal variables                               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INCLUDE 'BXformat.h'
      DOUBLE PRECISION   xpar_input(*)
      DOUBLE PRECISION   vvmin,cmsene,delfac
*
      m_Nevgen = 0
      m_MarTot = 0
      m_out    = xpar_input(4)
      m_idyfs  = xpar_input(8)
      m_nmax   = xpar_input(19)/2
      
      m_KeyFSR = xpar_input(21)
      m_KeyPia = xpar_input(22)
      m_MltFSR = xpar_input(24)
      m_KeyQSR = xpar_input(29)
      m_alfinv = xpar_input(30)

      m_IsFSR  = m_KeyFSR ! initial value only

      vvmin     = xpar_input(16)
      cmsene    = xpar_input( 1)
      m_emin    = cmsene/2d0*vvmin
      m_MasPhot =1d-60 ! dummy photon mass
     
      delfac   = xpar_input(18)
      m_delta  = vvmin*delfac

      CALL KK2f_GetXenph(m_Xenph)

      m_WtMass = 1d0

      CALL GLK_Mbook(m_idyfs+60,'KarLud: phot. mult. raw $', 1, 1d0*m_nmax)
      CALL GLK_Mbook(m_idyfs+69,'KarLud: totat weight    $', 1, 2d0)

      IF(m_KeyFSR .EQ. 0) RETURN

      CALL GLK_Mbook(m_idyfs+64,'YFSfin: photon raw multiplicity$', 10, 0.2d0*m_nmax)
      CALL GLK_Mbook(m_idyfs+61,'YFSfin: wt1, kinematics $', 1, 2d0)
      CALL GLK_Mbook(m_idyfs+62,'YFSfin: wt2, jacobian   $', 1, 2d0)
      CALL GLK_Mbook(m_idyfs+63,'YFSfin: wt3, mass       $', 1, 2d0)
      CALL GLK_Mbook(m_idyfs+65,'YFSfin: piatek, wtctrl  $', 1, 2d0)
      CALL GLK_Mbook(m_idyfs+66,'YFSfin: piatek, wtrem   $', 1, 2d0)

      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) 'KarFin Initialize  START'
      WRITE(m_out,bxl1i) m_KeyFSR,  'FSR radiation on/off','KeyFSR','a1'
      WRITE(m_out,bxl1i) m_KeyQSR,  'radiation from quark','KeyQSR','a2'
      WRITE(m_out,bxl1i) m_KeyPia,  'removal    switch   ','KeyPia','a3'
      WRITE(m_out,bxl1g) delfac,    'infrared cut FACTOR ','delfac','a4'
      WRITE(m_out,bxl1g) m_delta,   'infrared cut itself ','delta ','a5'
      WRITE(m_out,bxl1g) m_emin ,   'EminCMS for removal ','[GeV] ','a6'
      WRITE(m_out,bxl1i) m_nmax,    'Max. photon mult.   ','nmax  ','a7'
      WRITE(m_out,bxtxt) 'KarFin Initialize  END  '
      WRITE(m_out,bxclo)

      END                       !!!KarFin_Initialize!!!


      SUBROUTINE KarFin_Finalize(mode)
*     ********************************
      IMPLICIT NONE
*
      INCLUDE 'BXformat.h'
      INCLUDE 'KarFin.h'
*-----------------------------------------------------------------------------
      INTEGER mode
      DOUBLE PRECISION   avmlt,ermlt,upmlt
      DOUBLE PRECISION    WTsup, AvUnd, AvOve, ROverf, AveWt, ERela
      INTEGER  Nevtot,Nevacc,Nevneg,Nevove,Nevzer
*
      DOUBLE PRECISION   awt69,dwt69
      DOUBLE PRECISION   evove,evneg,evacc,evzer,evtot,dumm2
*----------
      DOUBLE PRECISION   awt61,dwt61
      DOUBLE PRECISION   awt62,dwt62
      DOUBLE PRECISION   awt63,dwt63
      DOUBLE PRECISION   awt65,dwt65
      DOUBLE PRECISION   awt66,dwt66
      INTEGER ntot66,nzer66
*
      INTEGER Ntot, Nacc, Nneg, Nove, Nzer
*---------------------------------------------------------------------------
*
      IF(m_KeyFSR .EQ. 0) RETURN
* no printout for mode=1
      IF(mode .EQ. 1) RETURN
* no printout if there was no generation
      IF(m_Nevgen .EQ. 0) RETURN
*============================================================================
      CALL  GLK_Mprint(m_idyfs+64)
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) 'KarFin Finalize    START'
      WRITE(m_out,bxl1i) m_NevGen,   ' generated events  ','nevgen','a2'

      CALL  GLK_MgetAve(m_idyfs+61,awt61,dwt61,WtSup)
      CALL  GLK_MgetAve(m_idyfs+62,awt62,dwt62,WtSup)
      CALL  GLK_MgetAve(m_idyfs+63,awt63,dwt63,WtSup)

      WRITE(m_out,bxl2f) awt61,dwt61,' kinematics, smin  ','wt1   ','a5'
      WRITE(m_out,bxl2f) awt62,dwt62,' jacobian          ','wt2   ','a6'
      WRITE(m_out,bxl2f) awt63,dwt63,' photon ang. dist. ','wt3   ','a7'
*
* Details on MASS WEIGHT REARRANGENMENT, and PHOTON REMOVAL
*
      CALL  GLK_MgetAve(m_idyfs+64,avmlt,ermlt,upmlt)
      CALL  GLK_MgetAve(m_idyfs+65,awt65,dwt65,WtSup)

      CALL GLK_MgetAll(m_idyfs+66,
     $     awt66,dwt66, WtSup, AvUnd, AvOve,
     $     ntot66,Nacc,Nneg,Nove,nzer66)
*
      WRITE(m_out,bxtxt) '    ON MASS WEIGHTS     '
      WRITE(m_out,bxl2f) awt66,dwt66,'removal wgt wtrem  ','     ',' b1'
      WRITE(m_out,bxl1i) ntot66,     'no. of raw events  ','     ',' b2'
      WRITE(m_out,bxl1i) nzer66,     'wt6=0      events  ','     ',' b3'
      WRITE(m_out,bxl2f) awt65,dwt65,'control wgt wctrl  ','     ',' b4'
      WRITE(m_out,bxl1i) m_MarTot,   'marked photons     ','MarTot','a5'
      WRITE(m_out,bxl1g) m_emin,     'emin               ','     ',' b6'
      WRITE(m_out,bxl1g) m_delta,    'delta              ','     ',' b7'
      WRITE(m_out,bxl1f) avmlt,      'raw ph. multipl.   ','     ',' b8'
      WRITE(m_out,bxl1f) upmlt,      'Highest phot. mult.','     ',' b9'
      WRITE(m_out,bxtxt) 'YFSfin Finalize    END  '
      WRITE(m_out,bxclo)
* test histograms
*[[      CALL GLK_Print(m_idyfs+20)
*[[      CALL gopera(m_idyfs+31,'/',m_idyfs+32,m_idyfs+33,1d0,1d0)
*[[      CALL GLK_Print(m_idyfs+33)
*=========================================================================
*-----------------------------------------------------------------------
*.........................output window a...............................
      CALL GLK_MgetAve(m_idyfs+60, avmlt, ermlt, upmlt)
      CALL GLK_MgetAve(m_idyfs+69,awt69,dwt69,WtSup)


* general information on weights
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '    KarFin Finalize     '
      WRITE(m_out,bxl1i) m_NevGen,   ' generated events  ','nevgen','a2'
      WRITE(m_out,bxl2f) awt69,dwt69,' general weight    ','wt    ','a1'
      WRITE(m_out,bxl1f) avmlt,      ' aver. ph. multi.  ','avmlt ','a3'
      WRITE(m_out,bxclo)
      END                       !!!KarFin_Finalize!!!


      SUBROUTINE KarFin_Make(PX,amfi1,amfi2,CharSq,wt)
*/////////////////////////////////////////////////////////////////////////////
* INPUT:
*     amfi1,2 = final masses, may change event per event (resonances)
*     CharSq  = charge squared
*     PX     = 4-momentum of the entire FSR system (fermions+photons)
*     KFfin     = final state fermion KF code
* OUTPUT:
*     qf1,2   = final state charged particle momenta
*     nphot   = FSR photon multiplicity
*     phot    = list of FSR photons
*     phsu    = total FSR photon 4-momentum
*     wt      = Crude mc weight
*/////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INCLUDE 'BXformat.h'
      DOUBLE PRECISION   CharSq,amfi1,amfi2
      DOUBLE PRECISION   PX(4)
      DOUBLE PRECISION   wt,WtFin,wtt
      INTEGER KFfin
      INTEGER k,j
*----------------------------------------------------------------------------
*
* Initialize momenta to zero
      DO k=1,4
         m_q1(k)   =0d0
         m_q2(k)   =0d0
         m_phsu(k) =0d0
      ENDDO
      m_nphot=0
      DO j=1,m_npmx
         DO k=1,4
            m_phot(j,k) =0d0
         ENDDO
      ENDDO
*
*     Generate photons and fermions in the rest frame of ferms Q=qf1+qf2
*     and transform to LAB system, 
*     PX=qf1+qf2+phsu is total momentum of the entire FSR system,
*     as seen from the LAB system.
*
      m_IsFSR = m_KeyFSR
      CALL KarLud_GetKFfin(KFfin)
* check for quarks KeyQSR flag
      IF( ABS(KFfin) .LT. 10) m_IsFSR = m_IsFSR*m_KeyQSR
*-----------------------------------------------------------------------
      IF(m_IsFSR .EQ. 1) THEN
         m_NevGen = m_NevGen+1  ! Only bremsstrahlung counts!!!
         CALL KarFin_YFSfin( PX,  amfi1, amfi2,  CharSq,  WtFin)
      ELSE
*-----------------------------------------------------------------------
*     No final state bremss, fermion momenta defined in Z frame
         CALL KinLib_phspc2( PX,amfi1,amfi2,m_q1,m_q2,wtt)
         WtFin = 1d0
         m_nphot  = 0
      ENDIF
*-----------------------------------------------------------------------
*     main weight
      wt=WtFin

      CALL GLK_Mfill(m_idyfs+69, wt,  1d0)
      CALL GLK_Mfill(m_idyfs+60, 1d0*m_nphot,  1d0)
* =============================================
      END                       !!!KarFin_Make!!!


      SUBROUTINE KarFin_YFSfin( PX,Mas1,Mas2,CharSq,WtFin)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*//  Simulates final state bremsstrahlung out of pair of charged patricles.                 //
*//  (algorithm of 11-th March 1989, by S. Jadach)                                          //
*//  INPUT  :                                                                               //
*//      MltFSR  = normaly set to zero, otherwise enforces photon                           //
*//                multiplicity to be exactly equal MltFSR (special tests)                  //
*//      KeyPia  = photon removal switch                                                    //
*//      PX     = 4-momentum of FSR system as seen from LAB system                          //
*//      Mas1,2 = masses of the final charged pair                                          //
*//      delta   = lower energy bound (dimensionless)                                       //
*//      emin    = lower energy bound (GeV) in LAB system                                   //
*//      CharSq  = final state charge squared                                               //
*//      alfinv  = 1/alpha_QED                                                              //
*//  OUTPUT :                                                                               //
*//      qf1,2    = final fermion four momentum (GeV)                                       //
*//      nphot   = photon multiplicity                                                      //
*//      phot    = photon four momenta (GeV) in cms                                         //
*//      phsu    = sum of photon momenta                                                    //
*//      ygr,zet = Sudakov variables                                                        //
*//      WtFin   = MC weight at this stage                                                  //
*//      martot  = control variable, no of marked photons                                   //
*//  OTHER:                                                                                 //
*//      Mark    = marks on photons CLOSE to lower energy bound                             //
*//      qf1c,2c = ficticious final fermion four momenta for crude S-factor                 //
*//      wt1    = the weight - phase space limits for very hard phot.                       //
*//      wt2    = the weight - translation jacobian.                                        //
*//      wt3    = the weight - single final mass-weight                                     //
*//      wtm    = the list/matrix of mass-weights.                                          //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
*
      DOUBLE PRECISION  pi
      PARAMETER(pi=3.1415926535897932d0)
*
      DOUBLE PRECISION   PX(4)

      DOUBLE PRECISION   WtMlist(100)
      DOUBLE PRECISION   rr(100),xk(100),cgx(100),sgx(100)
      DOUBLE PRECISION   dis0(100)
      REAL               rvec(100)
      INTEGER Mark(100)
*
      INTEGER i,j,k
      DOUBLE PRECISION  phi,cg,sg,eta1,eta2,uu,ul,del1,del2
      DOUBLE PRECISION  yy,xmk2,ener,smini
      DOUBLE PRECISION  wt1,wt2,wt3,WtFin
      DOUBLE PRECISION  betc,etc1,etc2,amd1
      DOUBLE PRECISION  average,Mas1,Mas2,svar,amfin,sprim,qmsene,amcru
      DOUBLE PRECISION  alf1,dis1,amc2,CharSq,xsum
      DOUBLE PRECISION  dl1,dl2,betn,xfact,gamf2,dist1,amd2
*-----------------------------------------------------------------------
      m_NevGen = m_NevGen + 1
*
      alf1 = 1d0/pi/m_alfinv
      wt1   =1d0
      wt2   =1d0
      wt3   =1d0
*-----------------------------------------------------------------------
      svar = PX(4)**2-PX(3)**2-PX(2)**2-PX(1)**2
      amfin = MIN(Mas1,Mas2)
      amc2  = 4.d0*amfin**2/svar  ! overvalued svar for crude
      betc  = sqrt(1d0-amc2)      ! overvalued svar for crude
      DO i=1,100
         rr(i)=0.d0
         Mark(i)=0
      ENDDO
      DO k=1,4
         m_phsu(k)=0.d0
      ENDDO
      DO i=1,100
         m_yfin(i)=0
         m_zfin(i)=0
         DO k=1,4
            m_phot(i,k)=0.d0
         ENDDO
      ENDDO
*-----------------------------------------------------------------------
*  generate photon multiplicity, average = average multiplicity (crude)
*-----------------------------------------------------------------------
      gamf2 = CharSq*alf1 *(1+betc**2)/betc *dlog((1d0+betc)**2/amc2)
      average = gamf2*dlog(1/m_delta)
      average = average *m_Xenph
    5 CONTINUE
      CALL KarFin_PoissGen(average,m_nmax,m_nphot,rr)
** This is for tests of program at fixed multiplicity (advanc. users)
      IF((m_MltFSR .NE. 0) .AND. (m_nphot .NE. m_MltFSR)) GOTO 5
**
      IF(m_nphot .EQ. 0) THEN
         sprim=svar
      ELSE
*-----------------------------------------------------------------------
*     begin with photon energy
         xsum=0.d0
         DO  i=1,m_nphot
            xk(i)=m_delta**rr(i)
            IF(xk(i) .LT. sqrt(10.d0)*m_delta) Mark(i)=1
            xsum=xsum+xk(i)
         ENDDO
         IF(xsum .GE. 1.d0) GOTO 900
         xfact=1d0/(1.d0-xsum)
         DO i=1,m_nphot
            xk(i)=xk(i)*xfact
         ENDDO
         CALL PseuMar_MakeVec(rvec,m_nphot)
         DO i=1,m_nphot
*-----------------------------------------------------------------------
*     simplified photon angular distribution,
*     s'->s and m**2/(kp)**2 dropped
*     cg=cos(theta) and sg=sin(theta) memorized to avoid rounding err.
            CALL KarFin_AngBre(amc2,dl1,dl2,cg,sg,dis0(i),dis1)
*-----------------------------------------------------------------------
*     define photon momenta (in units of sqrt(s')/2 )
            phi=2.d0*pi*rvec(i)
            m_phot(i,1)=xk(i)*sg*cos(phi)
            m_phot(i,2)=xk(i)*sg*sin(phi)
            m_phot(i,3)=xk(i)*cg
            m_phot(i,4)=xk(i)
            DO k=1,4
               m_phsu(k)=m_phsu(k)+m_phot(i,k)
            ENDDO
            cgx(i)=cg
            sgx(i)=sg
         ENDDO
*-----------------------------------------------------------------------
*     determine rescaling factor and s', wt2 is dilatation jacobian
         xmk2 = m_phsu(4)**2-m_phsu(3)**2-m_phsu(2)**2-m_phsu(1)**2
         yy   = 1.d0/(1.d0 +m_phsu(4) +xmk2/4.d0 )
         wt2  = yy*(1.d0+m_phsu(4))
         sprim= svar*yy
*-----------------------------------------------------------------------
*     reject events with too hard photons
         smini= (Mas1+Mas2)**2
         IF(sprim .LT. smini) GOTO 900
*-----------------------------------------------------------------------
*     Recsale properly all photon momenta
*-----------------------------------------------------------------------
         ener = sqrt(sprim)/2.d0
         DO  k=1,4
            m_phsu(k)= m_phsu(k)*ener
            DO  i=1,m_nphot
               m_phot(i,k)=m_phot(i,k)*ener
            ENDDO
         ENDDO
      ENDIF ! m_nphot
*-----------------------------------------------------------------------
*     final fermion momenta
*-----------------------------------------------------------------------
      amcru  = amfin*SQRT(sprim/svar)
      qmsene = SQRT(sprim)
      ener   = qmsene/2d0
      CALL KinLib_givpair(qmsene,Mas1,Mas2,m_q1, m_q2 ,betn,eta1,eta2)! real
      CALL KinLib_givpair(qmsene,amcru,amcru,m_r1,m_r2,betc,etc1,etc2)! ghost
*-----------------------------------------------------------------------
*     Mass weight for theta distribution
*-----------------------------------------------------------------------
*     Mass weight compensates for s'->s and droping terms -m**2/(k.q)**2
*     Care is taken of machine rounding errors.
*     del1 and del2 RECALCULATED out of angles sgx(i),cgx(i)
*     with TRUE sprim, using EXACT formulas
      amd1 = (Mas1/ener)**2
      amd2 = (Mas2/ener)**2
      DO i=1,m_nphot
         IF( cgx(i) .GT. 0.d0 ) THEN
            del1 = amd1/(eta1+betn) +betn*sgx(i)**2/(1+cgx(i))
            del2 = eta2 +betn*cgx(i)
         ELSE
            del1 = eta1 -betn*cgx(i)
            del2 = amd2/(eta2+betn) +betn*sgx(i)**2/(1-cgx(i))
         ENDIF
         dist1=1d0/(del1*del2) 
     $        *(1d0 -(amd1+amd2)/4d0
     $                   -amd2/4d0*del1/del2 -amd1/4d0*del2/del1)
         WtMlist(i)= dist1/dis0(i)
         IF(WtMlist(i) .LT.  1.d-90) WtMlist(i)= 0.d0
***********
** dist1x below is exactly the same as dist1 but for small masses is 
** prone to severe rounding errors (in contrast to dist1 used above)
*         IF((1-sprim/svar) .gt. 0.01d0) THEN
*            qf1qf2= m_q1(4)*m_q2(4) -m_q1(3)*m_q2(3) -m_q1(2)*m_q2(2) -m_q1(1)*m_q2(1)
*            qf1k = m_q1(4)*m_phot(i,4)-m_q1(3)*m_phot(i,3) -m_q1(2)*m_phot(i,2)-m_q1(1)*m_phot(i,1)
*            qf2k = m_q2(4)*m_phot(i,4)-m_q2(3)*m_phot(i,3) -m_q2(2)*m_phot(i,2)-m_q2(1)*m_phot(i,1)
*            dist1x = 2*qf1qf2/qf1k/qf2k -Mas1**2/qf1k**2 -Mas2**2/qf2k**2
*            dist1x = dist1x/4d0*m_phot(i,4)**2
*            WRITE(*,'(a,5f20.10)') '===>: ',dist1x/dist1
*         ENDIF
***********
*     finaly define Sudakov variables
         m_yfin(i)=del1*xk(i)/2d0
         m_zfin(i)=del2*xk(i)/2d0
      ENDDO
*-----------------------------------------------------------------------
* Transform from rest frame of Q=qf1+qf2 down to CMS=Lab,
* through inetrmediate rest frame of PX=qf1+qf2+phsu.
*-----------------------------------------------------------------------
      CALL KarFin_Kinf1(PX,m_q1,m_q2,m_r1,m_r2,m_nphot,m_phot,m_phsu)
*-----------------------------------------------------------------------
* Calculate YFS formfactor (cut-off dependent part) and mass weights
* Optionally removing photons below emin from the list
      CALL KarFin_Piatek( Mas1,Mas2,CharSq,WtMlist, wt3)
*-----------------------------------------------------------------------
* Monitoring weights and other control variables,
* Non-essential for the MC generation itself.
      CALL GLK_Mfill(m_idyfs+64, 1d0*m_nphot  ,1d0)
*[[[      uu = 1d0-sprim/svar
*[[[      CALL GLK_Fil1(m_idyfs+31, uu  ,wctrl)
*[[[      CALL GLK_Fil1(m_idyfs+32, uu  ,  1d0)
* marked photons
      IF(m_nphot .GE. 1) THEN
         DO i=1,m_nphot
*[[[            ul= log10(m_phot(i,4)/m_emin)
*[[[            IF(Mark(i) .EQ. 1)   CALL GLK_Fil1(m_idyfs+20,   ul,1.d0)
            IF(Mark(i) .EQ. 1)   m_MarTot=m_MarTot+1
         ENDDO
      ENDIF
*-----------------------------------------------------------------------
* Final Monitoring weights
 1000 CONTINUE
      WtFin = wt1*wt2*wt3
      CALL GLK_Mfill(m_idyfs+61,     wt1  ,1d0)
      CALL GLK_Mfill(m_idyfs+62,     wt2  ,1d0)
      CALL GLK_Mfill(m_idyfs+63,     wt3  ,1d0)
*-----------------------------------------------------------------------
      RETURN
*-----------------------------------------------------------------------
* Event outside phase space (too hard photon)
 900  CONTINUE
      wt1   = 0.d0
      wt2   = 1.d0
      wt3   = 1.d0
      m_nphot = 0
      GOTO 1000   !!!! a litle bit clumsy jump
*
      END                       !!!KarFin_YFSfin!!!


      SUBROUTINE KarFin_Piatek( Mas1,Mas2,CharSq,WtMlist, Wt3)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// Written CERN, piatek, 22 sept. 1989  (S.J.)                                             //
*// This routine calculates YFS form-factor and optionaly                                   //
*// removes some of soft photons (below Emin in CMS), calculating compensating weight.      //
*// Note the action of this routine is not Loretnz invariant !!!!                           //
*// Input:                                                                                  //
*//     KeyPia   = 0, NO removal of photons below Emin                                      //
*//              = 1, with removal of photons below Emin                                    //
*//     Mas1,2  = fermion masses                                                           //
*//     delta    = infrared cut-off in generation (dimensionless)                           //
*//     CharSq   = FSR charge squared                                                       //
*//     alfinv   = 1/alpha_QED                                                              //
*//     qf1,2    = fermion momenta                                                          //
*//     qf1c,2c  = ghost fermion momenta in crude S-factor                                  //
*//     phsu     = sum of photon momenta                                                    //
*//     phot     = list of photon momenta                                                   //
*//     WtMlist  = list of mass-weights for all photons                                     //
*// OUTPUT:                                                                                 //
*//     WtRem    = mass-weight of removed photons, for tests                                //
*//              = 1,  for KeyPia=0                                                         //
*//     WtMas    = mass-weight for all photons, see comments below,                         //
*//                the MOST IMPORTANT OUTPUT of Piatek !!!                                  //
*//     WtCtrl    = control-weight for remowed photons, for tests                           //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// REMARK on r1, r2:                                                                       //
*// They are ghost-fermion momenta in the definition of the crude                           //
*// distribution, i.e. truncated S-factor is r1*r2/(k*r1)/(k*r2)                            //
*// as generated in the crude MC.                                                           //
*// In QMS frame 4-momenta r1, r2 have the same energy                                      //
*// and directions as q1, q2                                                                //
*// but different masses Mas1c, Mas2c and therefore longer 3-momenta.                       //
*// They are usefull because we may use the same analytical                                 //
*// expression for the exact Breal and  for 'crude Breal'                                   //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
*
      DOUBLE PRECISION    Mas1,Mas2,CharSq,WtMlist(100),Wt3
*
      DOUBLE PRECISION  pi
      PARAMETER(pi=3.1415926535897932d0)
      INTEGER  i,j,k,nph
      DOUBLE PRECISION    QQ(4),PX(4)
      DOUBLE PRECISION    amc2,Mass,Delta1,Epsi1,wtm1,wtm2
      DOUBLE PRECISION    svarX,svarQ,QQk,q1q2,betc
      DOUBLE PRECISION    DelB,DelB2,DelB2u,DelVol,DelYFS
      DOUBLE PRECISION    BVR_A
      DOUBLE PRECISION    Eq1,Eq2,EminQ,EQQ,r1r2,Mas1c,Mas2c
      DOUBLE PRECISION    BtiXcru,BtiQcru,BtiQexa,BtiXexa
      DOUBLE PRECISION    BVR_Btildc,BVR_Btilda
      DOUBLE PRECISION    WtRem,WtCtrl
      DOUBLE PRECISION    VoluMC, YFS_IR, YFSkon
      DOUBLE PRECISION    alfpi,alfch,alfCR
*-----------------------------
      INTEGER  iCont
      DATA     iCont /0/
*-----------------------------
      WtCtrl =1d0
      WtRem  =1d0
      alfpi = 1d0/pi/m_AlfInv
      alfch = alfpi*CharSq
      alfCR = alfch* m_Xenph
      Mass = MIN(Mas1,Mas2)
      DO k=1,4
         PX(k) = m_q1(k)+m_q2(k) +m_phsu(k)
         QQ(k) = m_q1(k)+m_q2(k)
      ENDDO
      svarX = PX(4)**2-PX(3)**2 -PX(2)**2 -PX(1)**2
      svarQ = QQ(4)**2-QQ(3)**2 -QQ(2)**2 -QQ(1)**2
      EQQ   = 0.5d0*SQRT(svarQ)
      q1q2  =  m_q1(4)*m_q2(4) -m_q1(3)*m_q2(3) -m_q1(2)*m_q2(2) -m_q1(1)*m_q2(1)
      r1r2  =  m_r1(4)*m_r2(4) -m_r1(3)*m_r2(3) -m_r1(2)*m_r2(2) -m_r1(1)*m_r2(1)
      QQk   =  QQ(4)*m_phsu(4) -QQ(3)*m_phsu(3) -QQ(2)*m_phsu(2) -QQ(1)*m_phsu(1)
      Eq1   = (svarQ +Mas1**2 -Mas2**2)/(2*SQRT(svarQ))
      Eq2   = (svarQ -Mas1**2 +Mas2**2)/(2*SQRT(svarQ))
* Delta1 and Epsi1 are cutoffs located in YFS formfactor
* Note that xfact=(1+2*QQk/svarQ) is EXACTLY the same as in KarFin_YFSfin
* Delta1 = 2*Emin/sqrt(s'), where Emin is in QMS
      Delta1 = m_Delta*(1+ 2*QQk/svarQ)
      Epsi1  = DSQRT(m_Emin**2/m_q1(4)/m_q2(4))
      EminQ  = EQQ*Delta1
* The total phase space integral for crude x-section and YFS formfactor cut-off dependend part
* Note that delta is a lower limit on y-z-variables in crude generation
      amc2  =  4d0*Mass**2/svarX
      betc  =  DSQRT(1d0-amc2)
*     ===========================================================================================
      VoluMC =  2d0*alfCR *(1d0+betc**2)/(2d0*betc)* DLOG((1d0+betc)**2/amc2) *DLOG(1/m_Delta)
      YFS_IR = -2d0*alfch *(     q1q2 *BVR_A( q1q2, Mas1,Mas2) -1d0  )        *DLOG(1/Delta1)
* YFSkon is delegated/exported to QED3 (unused here).
      YFSkon =  1/4d0 *2*alfch*(DLOG(svarQ/Mass**2)-1) + alfch*( -.5d0  +pi**2/3d0) ! Mass<<sqrt(s)
*     ===========================================================================================
* Corrections necessary for photon remooval scenario (now default!)
      Mas1c  = Mass*SQRT(svarQ/svarX)
      Mas2c  = Mass*SQRT(svarQ/svarX)
      BtiXcru= BVR_Btildc(alfCR, r1r2, m_r1(4),m_r2(4), Mas1c,Mas2c, m_Emin, m_MasPhot) !crude
      BtiQcru= BVR_Btildc(alfCR, r1r2, EQQ,    EQQ,     Mas1c,Mas2c, EminQ,  m_MasPhot) !crude
      BtiXexa= BVR_Btilda(alfch, q1q2, m_q1(4),m_q2(4), Mas1, Mas2,  m_Emin, m_MasPhot) !exact
      BtiQexa= BVR_Btilda(alfch, q1q2, Eq1,    Eq2,     Mas1 ,Mas2,  EminQ,  m_MasPhot) !exact
      DelVol = BtiXcru -BtiQcru   ! positive
      DelYFS = BtiXexa -BtiQexa   ! positive
      DelB2  = -DelVol+DelYFS
*------ oldies ------
* Total QMS-CMS, result should be negative ( delta<<epsilon )
      DelB  =   VoluMC +YFS_IR
* Ultrarelativistic (small mass) old version is the following:
      DelB2u = -2*alfch*(DLOG(svarX/svarQ)+1) *dlog(Epsi1/Delta1)
* The average mass-weight for removed photon = exp(DelB2)
* It can be calculated analyticaly as a  ratio of YFS formfactors
* On the other hand, it is checked by MC, see control weight WtCtrl.
*********************************************************************************************
*      IF(iCont.LE.10 ) THEN
*         IF((1-svarQ/svarX) .GT. 0.1d0) THEN
*            iCont = iCont+1
*((( Approximate version of DelB2 without contr. with A4 terms for tests (surpisingly good!!!)
*      DelB2w = -2*alfch*(  (1d0+betc**2)/(2d0*betc)* DLOG((1d0+betc)**2/amc2) 
*     $                    -( q1q2*BVR_A(q1q2,Mas1,Mas2) -1d0 )
*     $                  ) *DLOG(Epsi1/Delta1)
*         WRITE(*,'(a,5f20.10)') 'piatek: ',1-svarQ/svarX,DelB2,DelB2u/DelB2,DelB2w/DelB2
*)))
*            VoluMC2 = 
*     $            BVR_Btildc(alfch, r1r2, EQQ,EQQ,  Mas1c,Mas2c, EQQ,          m_MasPhot)
*     $           -BVR_Btildc(alfch, r1r2, EQQ,EQQ,  Mas1c,Mas2c, EQQ*m_Delta,  m_MasPhot)
*            WRITE(*,'(a,5f20.10)') '###Piatek: ',1-svarQ/svarX, VoluMC2,VoluMC,VoluMC2/VoluMC
*         ENDIF
*      ENDIF
*********************************************************************************************
*--------------------------
      wtm1=1d0
      wtm2=1d0
* mass weight below and above Emin calculated separately
      DO i=1,m_nphot
         IF(m_phot(i,4) .LT. m_Emin) THEN
            wtm1=wtm1*WtMlist(i) /m_Xenph
            IF(wtm1 .LE. 1d-90) wtm1=0d0
         ELSE
            wtm2=wtm2*WtMlist(i) /m_Xenph
            IF(wtm2 .LE. 1d-90) wtm2=0d0
         ENDIF
      ENDDO
*------------------------------------------------------------------------------
* Control weight - its average should be =1 within statist. error!
      WtCtrl =wtm1*exp(-DelB2)
      IF(m_KeyPia .EQ. 0) THEN
         IF( ABS(DelB) .GT. 100d0 ) WRITE(*,*) '#### KarFin_Piatek: DelB= ',DelB
         WtRem    = 1d0
         m_WtMass = wtm1*wtm2      !!! <--removal OFF
         m_VoluMC = EXP( VoluMC)
         m_YFS_IR = EXP( YFS_IR)
         m_YFSkon = EXP( YFSkon)   !!! <--finite part of YFS formfactor
*        =====================================
      ELSE
* Optional removal of photons below Emin from the record
* in such a case WtMas includes exp(belb2)= <wt3> for removed ph.
         nph=m_nphot
         DO j=m_nphot,1,-1
            IF(m_phot(j,4) .LT. m_Emin) THEN
               DO i=j+1,nph
                  DO k=1,4
                     m_phot(i-1,k)=m_phot(i,k)
                  ENDDO
               ENDDO
               nph=nph-1
            ENDIF
         ENDDO
* Correction of Alex Read, probably obsolete because KarFin_Merge is also corrected
         DO j=nph+1,m_nphot
            DO k=1,4
               m_phot(j,k) = 0.d0
            ENDDO
         ENDDO
         m_nphot=nph
* Important to remember: EXP(YFS_IR +YFSkon)         = full YFS formfactor in QMS
*                        EXP(YFS_IR +YFSkon +DelYFS) = full YFS formfactor in CMS
         WtRem    = wtm1
         m_WtMass = wtm2           !!! <--removal ON
         m_VoluMC = EXP(VoluMC -DelVol)
         m_YFS_IR = EXP(YFS_IR +DelYFS)
* YFSkon is delegated/exported to QED3 (unused here).
         m_YFSkon = EXP(YFSkon)    !!! <--finite part of YFS formfactor in QMS frame!!!
*        ===============================================
      ENDIF
*     **********************************
      Wt3 = m_WtMass *m_VoluMC *m_YFS_IR
*     **********************************
* Monitoring
      CALL GLK_Mfill(m_idyfs+65,       WtCtrl ,1d0)
      CALL GLK_Mfill(m_idyfs+66,       WtRem  ,1d0)
*
      END                       !!! KarFin_Piatek !!!



      SUBROUTINE KarFin_PoissGen(average,nmax,mult,rr)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//  Last corr. nov. 91                                                                     //
*//   This generates photon multipl. nphot according to poisson distr.                      //
*//   Input:  average = average multiplicity                                                //
*//           nmax  = maximum multiplicity                                                  //
*//   Output: mult = generated multiplicity                                                 //
*//           rr(1:100) list of ordered uniform random numbers,                             //
*//           a byproduct result, to be eventually used for some further                    //
*//           purpose (i.e.  generation of photon energies).                                //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE   PRECISION average,  rr(*)
      INTEGER  nmax,mult
* locals
      REAL                 rvec(10)
      DOUBLE   PRECISION   sum,y
      INTEGER              nn,it,nfail
      DATA nfail/0/
*---------------------------------------------------------------------------------------------
      IF( average.LE.0 ) GOTO 900
 50   nn=0
      sum=0d0
      DO it=1,nmax
         CALL PseuMar_MakeVec(rvec,1)
         y= log(rvec(1))
         sum=sum+y
         nn=nn+1
         rr(nn)=sum/(-average)
         IF(sum .LT. -average) GOTO 130
      ENDDO
      nfail=nfail+1
      IF(nfail .GT. 10) GOTO 900
      GOTO 50
 130  mult=nn-1
      RETURN
 900  CONTINUE
      WRITE(*,*) ' STOP in KarFin_PoissGen: nmax,average= ',nmax,average
      STOP
      END

      SUBROUTINE KarFin_AngBre(am2,del1,del2,costhg,sinthg,dist0,dist1)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//   This routine generates photon angular distribution                                    //
*//   in the rest frame of the fermion pair.                                                //
*//   The distribution is the S-factor without mass term,                                   //
*//   i.e. without terms 2p_1p_2/(kp_1)(kp_2)                                               //
*//   Fermion mass is treated exactly!                                                      //
*//   INPUT:                                                                                //
*//       am2 = 4*massf**2/s where massf is fermion mass                                    //
*//       and s is effective mass squared of the parent fermion-pair.                       //
*//   OUTPUT:                                                                               //
*//       del1= 1-beta*cos(theta)                                                           //
*//       del2= 1+beta*cos(theta)                                                           //
*//       costhg, sinthg, cos and sin of the photon                                         //
*//       angle with respect to fermions direction                                          //
*//       dist0 = distribution generated, without m**2/(kp)**2 terms                        //
*//       dist1 = distribution with m**2/(kp)**2 terms                                      //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT  NONE
      DOUBLE PRECISION am2,del1,del2,costhg,sinthg,dist0,dist1
* Locals
      DOUBLE PRECISION  a,eps,beta
      REAL              rn(10)
*---------------------------------------------------------------------------------------------
      CALL PseuMar_MakeVec(rn,2)
      beta =SQRT(1.d0-am2)
      eps  =am2/(1.d0+beta)                     != 1-beta
      del1 =(2.d0-eps)*(eps/(2.d0-eps))**rn(1)  != 1-beta*costhg
      del2 =2.d0-del1                           != 1+beta*costhg
* calculation of sin and cos theta from internal variables
      costhg=(del2-del1)/(2*beta)               ! exact
      sinthg=SQRT(del1*del2-am2*costhg**2)      ! exact
* symmetrization
      IF(rn(2) .LE. 0.5d0) THEN
        a=del1
        del1=del2
        del2=a
        costhg= -costhg
      ENDIF
      dist0=1d0/(del1*del2)*(1d0 -am2/2d0)
      dist1=1d0/(del1*del2) 
     $     *(1d0 -am2/2d0 -am2/4d0*(del1/del2+del2/del1))
* totaly equivalent formula is the following
*     dist1=1d0/(del1*del2)   *beta*sinthg**2/(del1*del2)
      END


      SUBROUTINE KarFin_Kinf1(PX,q1,q2,q1c,q2c,nphot,phot,phsu)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// Transforms final fermions and photons from QMS through Z-frame to CMS.                  //
*// Random Euler rotation is applied in the intermediate PX frame (Z frame)                 //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      DOUBLE PRECISION   PX(4),phot(100,4),phsu(4),q1(4),q2(4),q1c(4),q2c(4)
      DOUBLE PRECISION   ph(4),qqk(4)
      INTEGER i,k,nphot

* Calculate total final state four-momentum ferms+phots
      DO k=1,4
         qqk(k)=q1(k)+q2(k)+phsu(k)
      ENDDO
* Transform fermions
      CALL KarFin_BostEul(-1,qqk,PX,q1,q1) ! <-- Initialize Euler angles!!!
      CALL KarFin_BostEul( 0,qqk,PX,q2,q2)
      CALL KarFin_BostEul( 0,qqk,PX,q1c,q1c)
      CALL KarFin_BostEul( 0,qqk,PX,q2c,q2c)
      CALL KarFin_BostEul( 0,qqk,PX,phsu,phsu)
* Transform photons
      DO i=1,nphot
         DO k=1,4
            ph(k)=phot(i,k)
         ENDDO
         CALL KarFin_BostEul( 0,qqk,PX,ph,ph)
         DO k=1,4
            phot(i,k)= ph(k)
         ENDDO
      ENDDO
      END

      SUBROUTINE KarFin_BostEul(mode,qqk,PX,pvec,qvec)
*/////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                         //
*// Three transformations:                                                                  //
*// (1) Boost from final fermions rest frame to ferms+phots rest frame (Z frame).           //
*// (2) Euler rotation erasing memory of fermion directions.                                //
*// (3) Boost to laboratory system.                                                         //
*// Note that this transformation generates 'flat 2-body Born' in PX frame.                 //
*// This is very easy to implement.                                                         //
*// Of course, more sophisticated angular distribution can be implemented.                  //
*// In such a case BostEul will be replaced with some other transformation.                 //
*// Photon removal procedure with Piatek will work for arbitrary transformation.            //
*//                                                                                         //
*// Note that the procedure is encapsulated functionaly by generating Euler                 //
*// angles localy, during first call for mode=-1.                                           //
*//                                                                                         //
*/////////////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER mode
      DOUBLE PRECISION  pvec(4),qvec(4),qqk(4),PX(4)
      DOUBLE PRECISION  pi
      PARAMETER(pi=3.1415926535897932d0)
      REAL               rvec(10)
      DOUBLE PRECISION   the,phi,cth
*
      IF(mode .EQ. -1) THEN
* Angles for Euler rotation
         CALL PseuMar_MakeVec(rvec,2)
         cth= 1.d0 -2.d0*rvec(1)
         the= acos(cth)
         phi= 2.d0*pi*rvec(2)
      ENDIF
* And the transformations
      CALL KinLib_BostQ(  1,qqk,pvec,qvec) ! Boost along qqk
      CALL KinLib_Rotor(2,3,the,qvec,qvec) ! Rotation y-z
      CALL KinLib_Rotor(1,2,phi,qvec,qvec) ! Rotation x-y
      CALL KinLib_BostQ( -1,PX,qvec,qvec)  ! Boost to CMS
      END


      SUBROUTINE KarFin_ZBoostAll(exe)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   performs z-boost on all momenta of the event                            //
*//   this z-boost corresponds to beamstrahlung or beamspread                 //
*//   and is done at the very end of generation, after m.el. calculation      //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      DOUBLE PRECISION  exe
      INTEGER           j,k
      DOUBLE PRECISION  ph(4)
*
      IF( exe.EQ. 1d0) RETURN
      CALL KinLib_Boost(3,exe,m_q1,m_q1)
      CALL KinLib_Boost(3,exe,m_q2,m_q2)
      CALL KinLib_Boost(3,exe,m_phsu,m_phsu)
      DO j=1,m_nphot
         DO k=1,4
            ph(k) = m_phot(j,k)
         ENDDO
         CALL KinLib_Boost(3,exe,ph,ph)
         DO k=1,4
            m_phot(j,k) = ph(k)
         ENDDO
      ENDDO
      END


      SUBROUTINE KarFin_GetIsFSR(IsFSR)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INTEGER IsFSR
      IsFSR= m_IsFSR
      END

      SUBROUTINE KarFin_GetKeyPia(KeyPia)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INTEGER KeyPia
      KeyPia= m_KeyPia
      END

      SUBROUTINE KarFin_GetNphot(nphot)     
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get photon multiplicity                                                 //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INTEGER nphot
*
      nphot = m_nphot
      END

      SUBROUTINE KarFin_GetPhoton1(iphot,phot)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   get i-th photon momentum                                                //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INTEGER iphot
      DOUBLE PRECISION   phot(4)
      INTEGER k
*
      DO k=1,4
         phot(k) = m_phot(iphot,k)
      ENDDO
      END

      SUBROUTINE KarFin_GetSudakov1(iphot,y,z)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get sudakovs of i-th photon                                             //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INTEGER iphot
      DOUBLE PRECISION   y,z
*
      y = m_yfin(iphot)
      z = m_zfin(iphot)
      END


      SUBROUTINE KarFin_GetSudakov(nphot,yfin,zfin)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
*
      INTEGER nphot
      DOUBLE PRECISION   yfin(*),zfin(*)
      INTEGER i
*-------------
      nphot = m_nphot
      DO i=1,m_nphot
         yfin(i) = m_yfin(i)
         zfin(i) = m_zfin(i)
      ENDDO
      END

      SUBROUTINE KarFin_GetPhotons(nphot,sphot)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
*
      INTEGER nphot
      DOUBLE PRECISION   sphot(m_npmx,4)
      INTEGER j,k
*-------------
      nphot = m_nphot
      DO j=1,m_nphot
         DO k=1,4
            sphot(j,k) = m_phot(j,k)
         ENDDO
      ENDDO
      END

      SUBROUTINE KarFin_GetFermions(qf1,qf2)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
*
      DOUBLE PRECISION   qf1(4),qf2(4)
      INTEGER k
*---------------
      DO k=1,4
         qf1(k) = m_q1(k)
         qf2(k) = m_q2(k)
      ENDDO
      END

      SUBROUTINE KarFin_GetSvarQ(SvarQ)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
*
      DOUBLE PRECISION   SvarQ
*---------------
      SvarQ = ( m_q1(4)+m_q2(4) )**2 -( m_q1(3)+m_q2(3) )**2 
     $       -( m_q1(2)+m_q2(2) )**2 -( m_q1(1)+m_q2(1) )**2
      END

      SUBROUTINE KarFin_GetYFSkon(YFSkon)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get finite part of YFS form-factor                                     //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      DOUBLE PRECISION    YFSkon
*------------------
      YFSkon = m_YFSkon
      END

      SUBROUTINE KarFin_GetYFS_IR(YFS_IR)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//   Get finite part of YFS form-factor                                     //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      DOUBLE PRECISION    YFS_IR
*------------------
      YFS_IR = m_YFS_IR
      END

      SUBROUTINE KarFin_WtMass(WtMass)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      DOUBLE PRECISION   WtMass
*
      WtMass = m_WtMass
      END

      SUBROUTINE KarFin_Print(iev,ie1,ie2)
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*// Prints out four momenta of FINAL state                                   //
*// and the serial number of event iev on unit m_out                         //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarFin.h'
      INTEGER  iev,ie1,ie2
      DOUBLE PRECISION    sphum(4)
      CHARACTER*8 txt
      DOUBLE PRECISION    sum(4),ams,amph,amf1,amf2
      INTEGER  i,k
*--------------------------------------------------------
      IF( (iev .GE. ie1) .AND. (iev .LE. ie2) ) THEN
         txt = '  KarFin '
         WRITE(m_out,*) 
     $        '=========== ',txt,' ======================>',iev
         amf1 = m_q1(4)**2-m_q1(3)**2-m_q1(2)**2-m_q1(1)**2
         amf1 = sqrt(abs(amf1))
         amf2 = m_q2(4)**2-m_q2(3)**2-m_q2(2)**2-m_q2(1)**2
         amf2 = sqrt(abs(amf2))
         WRITE(m_out,3100) 'qf1',(  m_q1(  k),k=1,4),amf1
         WRITE(m_out,3100) 'qf2',(  m_q2(  k),k=1,4),amf2
         DO i=1,m_nphot
            amph = m_phot(i,4)**2-m_phot(i,3)**2 -m_phot(i,2)**2-m_phot(i,1)**2
            amph = sqrt(abs(amph))
            WRITE(m_out,3100) 'pho',(m_phot(i,k),k=1,4),amph
         ENDDO
         DO k=1,4
            sum(k)=m_q1(k)+m_q2(k)
         ENDDO
         DO i=1,m_nphot
            DO k=1,4
               sum(k)=sum(k)+m_phot(i,k)
            ENDDO
         ENDDO
         ams = sum(4)**2-sum(3)**2-sum(2)**2-sum(1)**2
         ams = sqrt(abs(ams))
         WRITE(m_out,3100) 'sum',(  sum(  k),k=1,4),ams
      ENDIF
 3100 FORMAT(1x,a3,1x,5f20.14)
      END


*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  KarFin                                //
*//////////////////////////////////////////////////////////////////////////////
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                     Pseudo-CLASS  KarLud                                 //
*//                                                                          //
*//   Purpose:                                                               //
*//   Top level  Monte-Carlo event generator for ISR radiadion.              //
*//   Administrates directly generation of v=1-s'/s                          //
*//   and optionaly of beamstrahlung variables x1 and x2.                    //
*//                                                                          //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////


      SUBROUTINE KarLud_Initialize(xpar_input,XCrude)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INCLUDE 'BXformat.h'
      DOUBLE PRECISION   xpar_input(*)
      DOUBLE PRECISION   XCrude, prec, Mathlib_Gauss
      INTEGER            ke, KFbeam, n, KeyGrid
      DOUBLE PRECISION   a,b,result,error
      DOUBLE PRECISION   BornV_Crude
      DOUBLE PRECISION   xborn,xdel
      DOUBLE PRECISION   IRCroots
      INTEGER            IRCacc,   IRCver,   IRCdate,   IRCxchat
      DOUBLE PRECISION   BornV_RhoVesko1
      EXTERNAL           BornV_RhoVesko1
* debug
      DOUBLE PRECISION   dbg_xsec,dbg_err,dbg_AveWt,dbg_RatWt,dbg_XCrude
*--------------------------------------------------------------------------------
      m_nevgen =  0
      m_nmax   =  xpar_input(19)/2
      m_idyfs  =  xpar_input(8)
*
      m_CMSene = xpar_input( 1)  ! initial value, to be preserved
      m_XXXene = m_CMSene        ! initial value, to be variable
      m_DelEne = xpar_input( 2)
      m_exe    = 1d0             ! no z-boost for default exe=1 
      m_out    = xpar_input( 4)
      m_vvmin  = xpar_input(16)
      m_vvmax  = xpar_input(17)
      m_KeyZet = xpar_input(501)
      m_KeyISR = xpar_input(20)
      m_MltISR = xpar_input(23)
      m_KeyFix = xpar_input(25)
      m_KeyWtm = xpar_input(26)
      m_alfinv = xpar_input(30)

      CALL KK2f_GetXenph(m_Xenph)

      m_WtMass = 1d0
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) 'KarLud_Initialize START'
      WRITE(m_out,bxl1f) m_CMSene,   'CMS energy average','CMSene','=='
      WRITE(m_out,bxl1f) m_DelEne,   'Beam energy spread','DelEne','=='
      WRITE(m_out,bxl1i) m_KeyISR,   'ISR on/off switch ','KeyISR','=='
      WRITE(m_out,bxl1i) m_KeyFix,   'Type of ISR       ','KeyFix','=='
      WRITE(m_out,bxl1i) m_KeyZet,   'Elect_weak switch ','KeyZet','=='
      WRITE(m_out,bxl1i) m_MltISR,   'Fixed nphot mult. ','MltISR','=='
      WRITE(m_out,bxl1i) m_nmax,     'Max. photon mult. ','nmax  ','=='
      WRITE(m_out,bxclo)
*//////////////////////////////////////////////////////////////////////////////////////
*//     Check on validity of input                                                   //
*//////////////////////////////////////////////////////////////////////////////////////
      IF(m_DelEne.GT.2d0) THEN
         WRITE(m_out,*) ' ### STOP in KarLud_Initialize: DelEne too big ', m_DelEne
         STOP
      ENDIF
      IF( (m_DelEne.NE.0d0) .AND. (m_KeyFix.EQ.2) ) THEN
         WRITE(m_out,*) ' ### STOP in KarLud_Initialize:'
         WRITE(m_out,*) ' Beamsstrahlung and Beam energy spread together not safe, not tested'
         STOP
      ENDIF
*//////////////////////////////////////////////////////////////////////////////////////
      KFbeam = 11           ! KF=11 is electron
      ke = 500+10*KFbeam
      m_amel   = xpar_input(ke+6)
*
      IF(m_KeyISR .EQ. 2) THEN
*        YFSini2 is only for very special tests, no harm if deleted
         CALL YFSini2_Initialize(m_amel, m_alfinv, m_vvmin, m_nmax, m_out, m_KeyWtm, m_MltISR)
      ENDIF
*
      xborn  = BornV_Crude(0d0)
      IF(m_KeyISR .EQ. 1) THEN
         IF(m_KeyFix .EQ. 0) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   This is normal ISR with help of Vesko1 routine, initialization                 //
*//////////////////////////////////////////////////////////////////////////////////////
            CALL Vesk1_Initialize(BornV_RhoVesko1,m_XCrude)
            XCrude    = m_XCrude
         ELSEIF(m_KeyFix .EQ. 2) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//   Initialization of Circe package of Thorsten Ohl                                //
*//   and of the beamstrahlung module Bstra                                          //
*//////////////////////////////////////////////////////////////////////////////////////
            IRCroots = xpar_input(71)
            IRCacc   = xpar_input(72)
            IRCver   = xpar_input(73)
            IRCdate  = xpar_input(74)
            IRCxchat = xpar_input(75)
            CALL IRC_circes(0d0, 0d0, IRCroots, IRCacc, IRCver, IRCdate, IRCxchat)
            KeyGrid  = xpar_input(76)
            CALL BStra_Initialize(KeyGrid,m_XCrude)         ! beamstrahlung initialization
         ELSEIF(m_KeyFix .EQ. -1) THEN
*//////////////////////////////////////////////////////////////////////////////////////
*//  The case of ISR swithed off, Born process                                       //
*//////////////////////////////////////////////////////////////////////////////////////
            m_XCrude = BornV_RhoVesko1(2d0)
            XCrude   = m_XCrude
            m_xcgaus = m_XCrude
            WRITE(m_out,bxl1f) m_XCrude,'xs_crude  BornV_Rho','xcvesk','  '
         ELSE
            WRITE(m_out,*) ' +++++ STOP in KarLud_Initialize, KeyFix = ', m_KeyFix
            STOP
         ENDIF
*     Miscelaneous x-check on x-section from vesko1
         IF(m_KeyFix .GE. 0 ) THEN
            a = 0d0
            b = 1d0
            prec = 1d-5
            m_xcgaus = Mathlib_Gauss(BornV_RhoVesko1,a,b, prec)
***         CALL Mathlib_GausJad(BornV_RhoVesko1,a,b, -prec, m_xcgaus) ! rather slow
            m_ErGaus   = m_xcgaus*prec
            xdel = m_XCrude/m_xcgaus-1
            WRITE(m_out,bxl1f) m_XCrude,'xs_crude  vesko    ','xcvesk','  '
            WRITE(m_out,bxl1f) m_xcgaus,'xs_crude  gauss    ','xcgaus','  '
            WRITE(m_out,bxl1f) xdel  ,  'xcvesk/xcgaus-1    ','      ','  '
         ENDIF
      ELSEIF( (m_KeyISR .EQ. 0) .OR. (m_KeyISR .EQ. 2) ) THEN
         XCrude    = xborn
         m_XCrude  = xborn
         WRITE(m_out,bxl1f) m_XCrude,'xs_crude  Born     ','xborn ','  '
      ELSE
         WRITE(*,*) ' ++++ KarLud: wrong KeyISR=',m_KeyISR
         STOP
      ENDIF
*
      CALL GLK_Mbook(m_idyfs+58,'KarLud, wtvesk  $', 100, 1.20d0)
      CALL GLK_Mbook(m_idyfs+59,'KarLud, wt_ISR  $', 1, 2.d0)
*
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) 'KarLud_Initialize END '
      WRITE(m_out,bxclo)
      END                       ! KarLud_Initialize


      SUBROUTINE KarLud_SmearBeams
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    Beam spread is implemented here                                              //
*//    This is correct only for very small spread < 2GeV                            //
*//    Should not be used together with beamstrahlung, lack of tests.               //
*//                                                                                 //
*//    Distribution is Gauss(X)=N*EXP( (X-CMSene/2)**2/(2*DelEne**2) )              //
*//    that is DelEne is proper dispersion in  Ebeam (not in CMSene).               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION   pi
      PARAMETER(         pi=3.1415926535897932d0)
      DOUBLE PRECISION   EBeam1,EBeam2
      DOUBLE PRECISION   R
      REAL               rvec(10)
*------------------------------------------------------------------------------------
      IF( m_DelEne.EQ.0d0 ) RETURN
      CALL PseuMar_MakeVec(rvec,2)
      R  = m_DelEne*SQRT(-2d0*LOG(rvec(1)))
      EBeam1 = m_CMSene/2 + R*cos(2*pi*rvec(2))
      EBeam2 = m_CMSene/2 + R*sin(2*pi*rvec(2))
      EBeam1 = MAX(EBeam1,0d0)
      EBeam2 = MAX(EBeam2,0d0)
* Redefine CMS energy after smearing of the beam energies!
      m_XXXene = 2*SQRT(EBeam1*EBeam2)
      m_exe    = m_exe*SQRT(EBeam1/EBeam2)
      END                       ! KarLud_SmearBeams

      SUBROUTINE KarLud_Make(PX,wt_ISR)
*/////////////////////////////////////////////////////////////////////////////////
*//                                                                             //
*// OUTPUT:                                                                     //
*//     PX       4-momentum left after photon emission (PX=q1+q2)               //
*//     m_p1,2   beams 4-momenta                                                //
*//     m_q1,2   final state 4-momenta                                          //
*//     m_nphot   photon multiplicity                                           //
*//     m_sphot   photon 4-momenta                                              //
*//     m_sphum   sum of photon 4-momenta                                       //
*//     m_yini,zini Sudakov variables from low level MC generator               //
*//                                                                             //
*/////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INCLUDE 'BXformat.h'
      DOUBLE PRECISION    PX(4)
*---------------------
      DOUBLE PRECISION    BornV_GetMass
      DOUBLE PRECISION    BornV_RhoVesko1
      EXTERNAL            BornV_RhoVesko1
      DOUBLE PRECISION    et1,et2
      DOUBLE PRECISION    amfi1,amfi2, x,y, bt1, dummy
      DOUBLE PRECISION    wtt, wt_ISR
      DOUBLE PRECISION    ph(4)
      DOUBLE PRECISION    EBeam1,EBeam2
      INTEGER             k,j 
*
      m_NevGen =  m_NevGen+1
      m_exe    = 1d0
      m_XXXene = m_CMSene
      IF(m_KeyISR .EQ. 1) THEN
*///////////////////////////////////////////////////////////////////////////////////
*//    Machine Gaussian Beam Spread  << 1GeV                                      //
*// Weight from Vesko/Vegas has Z peak at wrong v, at rescaled prosition          //
*// However, rho of Bornv is called once again by Vesko/Vegas for modified XXXene //
*// and Z position will be at the correct position in vv.                         //
*// The weight in model is not modifying Z position any more, see KK2f.           //
*///////////////////////////////////////////////////////////////////////////////////
      CALL KarLud_SmearBeams
      CALL BornV_SetCMSene(m_XXXene)
*-------------------------------------------------------------
*     Generate vv = 1-s'/s
         IF(    m_KeyFix .EQ. 0) THEN
            CALL Vesk1_Make( BornV_RhoVesko1, x,y, m_WtBasic)
         ELSEIF(m_KeyFix .EQ. 2) THEN
            CALL BStra_Make(m_vv, m_x1, m_x2, m_WtBasic)
*           Redefine CMS energy and boost
            m_XXXene = m_XXXene*SQRT((1d0-m_x1)*(1d0-m_x2))
            m_exe    = m_exe   *SQRT((1d0-m_x1)/(1d0-m_x2))
         ELSEIF(m_KeyFix .EQ.-1) THEN
            m_WtBasic=1d0
            dummy = BornV_RhoVesko1(2d0)
         ELSE          
            WRITE(*,*) ' ++++ KarLud: wrong KeyFix=',m_KeyFix
            STOP
         ENDIF
         CALL BornV_GetVV(m_vv)
*        Low-level multiphoton generator
         CALL KarLud_YFSini(m_XXXene, m_vv, PX, m_WtIni)
         wt_ISR = m_WtBasic*m_WtIni
*-------------------------------------------------------------
      ELSEIF(m_KeyISR .EQ. 2) THEN
*     This is for special tests with flat x-section
         CALL YFSini2_Make(m_XXXene, m_vv, m_p1,m_p2,
     $        m_nphot,m_sphot,m_sphum,m_yini,m_zini,PX,m_WtIni)
         wt_ISR = m_WtIni
*-------------------------------------------------------------
      ELSEIF(m_KeyISR .EQ. 0) THEN
         CALL KinLib_givpair(m_XXXene,m_amel,m_amel,m_p1,m_p2,bt1,et1,et2)
         DO k=1,4
            PX(k) = m_p1(k)+m_p2(k)
         ENDDO
         m_nphot   = 0
         wt_ISR  = 1d0
      ELSE
         WRITE(*,*) ' ++++ KarLud: wrong KeyISR=',m_KeyISR
         STOP
      ENDIF
*-------------------------------------------------------------
* Generate flavour KF and set exclusive mode
* Note that GenKF uses table of xsections m_Xborn defined in 
* the Vesk1_Make( BornV_RhoVesko1,...) or predefined during Initialization
      IF(wt_ISR .NE. 0d0)  THEN
         CALL MBrA_GenKF(m_KFfin,m_Wt_KF)
         wt_ISR = wt_ISR *m_Wt_KF
      ENDIF
      CALL KK2f_SetOneY(255,m_Wt_KF) ! Pure debug
*-------------------------------------------------------------
      IF(wt_ISR .EQ. 0d0 ) THEN
*     Set momenta to zero for WT=0 events
         DO k=1,4
            m_q1(k) =0d0
            m_q2(k) =0d0
            m_sphum(k)=0d0
         ENDDO
         m_nphot=0
         DO j=1,m_npmx
            DO k=1,4
               m_sphot(j,k)=0d0
            ENDDO
         ENDDO
         m_KFfin = 0
      ELSE
*     Define final fermion momenta (NOT used in case of FSR)
*     PX is the four-momentum of final state fermion pair in CMS
         amfi1  =BornV_GetMass(m_KFfin)
         amfi2  =BornV_GetMass(m_KFfin)
         CALL KinLib_phspc2( PX,amfi1,amfi2,m_q1,m_q2,wtt)
      ENDIF
*-------------------------------------------------------------
*     Final weight administration
      CALL GLK_Mfill(m_idyfs+58, m_WtBasic,  1d0)
      CALL GLK_Mfill(m_idyfs+59, wt_ISR, 1d0)
* store PX for further use through getter
      DO k=1,4
         m_PX(k) = PX(k)
      ENDDO
*-------------------------------------------------------------
      CALL KK2f_SetOneY(203,Wt_ISR)    ! Pure temporary debug
      CALL KK2f_SetOneY(250,m_WtBasic) ! Pure temporary debug
      CALL KK2f_SetOneY(251,m_WtIni)   ! Pure temporary debug
      END

      SUBROUTINE KarLud_Finalize(mode, XKarlud, KError)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Calculates crude xsection  XKarlud  and prints out final statistics     //
*//                                                                           //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'BXformat.h'
      INCLUDE 'KarLud.h'
      INTEGER  mode
      DOUBLE PRECISION    XKarlud,  KError
      DOUBLE PRECISION    ISRcru,  ISRerr, ISRbest
      DOUBLE PRECISION    averwt,  evtot,  evacc,  evneg, evove
      DOUBLE PRECISION    wt_ISR,  erkrl,  ErRela
      DOUBLE PRECISION    ddr,     ddv,    erkr,   xskr
      DOUBLE PRECISION    WTsup, AvUnd, AvOve, ROverf
      INTEGER  Nevtot,Nevacc,Nevneg,Nevove,Nevzer
*///////////////////////////////////////////////////////////////////////////////
*//   Normal case, ISR is ON                                                  //
*///////////////////////////////////////////////////////////////////////////////
      IF(m_KeyISR .EQ. 1) THEN
* Important part for NORMALIZATION in KK2f
         IF(     m_KeyFix .EQ. 0 ) THEN
            CALL Vesk1_Finalize(ISRbest,ErRela,ISRcru)
            ISRerr   = ISRbest*ErRela
            XKarlud  = ISRcru                      ! true crude
            KError   = 0d0
         ELSEIF( m_KeyFix .EQ. 2 ) THEN
            CALL BStra_GetXCrude(ISRcru)
            CALL BStra_Finalize(ISRbest,ErRela)
            ISRerr   = ISRbest*ErRela
            XKarlud  = ISRbest                     ! crude from internal loop
            KError   = ISRerr                      ! and its error
         ELSEIF( m_KeyFix .EQ. -1 ) THEN
            ISRcru   = m_XCrude
            ISRbest  = m_XCrude
            ISRerr   = 0d0
            XKarlud  = ISRcru                     ! artificial crude
            KError   = 0d0
         ELSE
            WRITE(*,*) ' ++++ KarLud_Finalize: wrong KeyFix=',m_KeyFix
            STOP
         ENDIF
*---------------------------------------------------------------
* The rest is miscelaneous information
* no printout for mode = 1
         IF(mode .EQ. 2) THEN
            WRITE(m_out,bxope)
            WRITE(m_out,bxtxt) '     KarLud  final  report     '
            WRITE(m_out,bxl1i) m_NevGen,         'total no of events','nevtot ','=='
            WRITE(m_out,bxl1f) ISRcru,           'ISRcru  [R]       ','ISRcru ','=='
            WRITE(m_out,bxl2f) ISRbest,ISRerr,   'ISRbest [R],ISRerr','ISRbest','=='
            WRITE(m_out,bxl1g) XKarlud,          'XKarlud [R]       ','XKarlud','=='
            WRITE(m_out,bxl1g) KError,           'KError  [R]       ','KError ','=='
            WRITE(m_out,bxclo)
*     Principal weight
            CALL GLK_MgetAll(m_idyfs+59, wt_ISR,erkrl, WtSup, AvUnd, AvOve,
     $                                Nevtot,Nevacc,Nevneg,Nevove,Nevzer)
            WRITE(m_out,bxope)
            WRITE(m_out,bxtxt) '  Report on wt_ISR of KarLud   '
            WRITE(m_out,bxl1i) nevtot,          'total no of events ','nevtot ','=='
            WRITE(m_out,bxl1i) nevneg,          'wt<0        events ','nevneg ','=='
            WRITE(m_out,bxl2f) wt_ISR,erkrl,    '<wt>               ','wt_ISR ','=='
            xskr   = XKarlud*wt_ISR
            erkr   = xskr*erkrl
            WRITE(m_out,bxl2f) xskr,erkr,       'sigma of KarLud [R]','xskarl ','=='
            WRITE(m_out,bxclo)
*     Vesko weight (miscelaneous)
            IF(     m_KeyFix .EQ. 0 ) THEN
               CALL GLK_MgetAve(m_idyfs+58,AverWt,ErRela,WtSup)
               WRITE(m_out,bxope)
               WRITE(m_out,bxl2f) averwt,errela,    'Average WT of Vesk1','AVesk1','=='
               WRITE(m_out,bxl2f) m_xcgaus,m_ErGaus,'xs_est gauss    [R]','xcgaus','=='
               ddv    = ISRbest/m_xcgaus-1d0
               ddr    = ErRela + 1d-6
               WRITE(m_out,bxl2f) ddv,ddr,          'xcve/xcgs-1        ','      ','=='
               WRITE(m_out,bxclo)
               CALL  GLK_Mprint(m_idyfs+58)
            ENDIF
         ENDIF
*///////////////////////////////////////////////////////////////////////////////
*//   Normal case, ISR is OFF, Born only                                      //
*///////////////////////////////////////////////////////////////////////////////
      ELSEIF( (m_KeyISR .EQ. 0) .OR. (m_KeyISR .EQ. 2) ) THEN
         XKarlud    = m_XCrude
         KError     = 0d0   
         IF(mode .EQ. 2) THEN
            WRITE(m_out,bxope)
            WRITE(m_out,bxl1i) m_NevGen,   'total no of events','nevtot ','a0'
            WRITE(m_out,bxl1f) XKarlud,    'xs_crude  Born     ','xborn ','  '
            WRITE(m_out,bxclo)
         ENDIF
      ELSE
         WRITE(*,*) ' ++++ KarLud: wrong KeyISR=',m_KeyISR
         STOP
      ENDIF
      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) '     KarLud_Finalize END  <<<     '
      WRITE(m_out,bxclo)
      END  ! KarLud_Finalize


      SUBROUTINE KarLud_YFSini(XXXene,vv, PX,WtIni)
*////////////////////////////////////////////////////////////////////////////////////
*//                                                                                //
*//  ======================================================================        //
*//  ======================= Y F S G E N ==================================        //
*//  ======================================================================        //
*//  The algorithm in this subprogram was described in:                            //
*//  ``Yennie-Frautschi-Suura soft photons in Monte Carlo event generators''       //
*//             Unpublished report by S. Jadach,                                   //
*//          MPI-Munchen, MPI-PAE/PTh 6/87, Jan. 1987.                             //
*//                                                                                //
*//  Later on used in YFS1,YFS2,YFS3, YFSWW, KORALZ, KORALW Monte Carlo programs   //
*//                                                                                //
*//  Purpose:  ISR photon emission, photon multiplicity and momenta                //
*//                                                                                //
*////////////////////////////////////////////////////////////////////////////////////
*//   INPUT:    XXXene,vv                                                          //
*//   OUTPUT:   PX,WtIni                                                           //
*//                                                                                //
*//   XXXene  = total cms energy                                                   //
*//   amel    = beam mass                                                          //
*//   MltISR  = flag normaly set to zero, for SPECIAL tests enforces               //
*//             photon multiplicity to be exactly equal MltISR                     //
*//   vv      = v=1-s'/s variable                                                  //
*//   vmin    = minimum v variable (infrared cutoff)                               //
*//   nmax    = maximum photon multiplicity                                        //
*//   alfinv  = 1/apha_QED                                                         //
*//   p1,2    = initial fermion momenta (along z-axix)                             //
*//   nphot   = photon multiplicity                                                //
*//   sphot   = photon four-momenta                                                //
*//   sphum   = total photon four-momentum                                         //
*//   ygr,zet = Sudakov variables                                                  //
*//   PX      = 4-mmentum left after photon emission                               //
*//   WtIni   = total weight from this sub-generator                               //
*////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
*
      DOUBLE PRECISION  pi
      PARAMETER( pi=3.1415926535897932d0)
*
      DOUBLE PRECISION    XXXene,vv
*
      DOUBLE PRECISION    xphot(100,4)    ! photon momenta before rescaling
      DOUBLE PRECISION    PX(4),xph(100),rr(100)
      DOUBLE PRECISION    pp(4),pk(4)
*
      INTEGER  i,j,k
      DOUBLE PRECISION    phi,cg,sg,xk
      DOUBLE PRECISION    dist0,dist1
      DOUBLE PRECISION    beta,eta1,eta2
      DOUBLE PRECISION    Ene,ppdpp,ppdpk,pkdpk,DilFac,DilFac0,AA
      DOUBLE PRECISION    del1,del2,am2
      DOUBLE PRECISION    DilJac0,DilJac,AvMult,WtIni
      DOUBLE PRECISION    WtDil0,WtCut0
      REAL                rvec(10)
*---------------------------------------
      Ene  = XXXene/2d0
* Define 4-momenta of the initial charged particles (emitters)
      CALL KinLib_givpair(XXXene,m_amel,m_amel,m_p1,m_p2,beta,eta1,eta2)
      DO i=1,m_nmax
         xph(i)=0d0
         m_yini(i)=0d0
         m_zini(i)=0d0
         DO j=1,4
            xphot(i,j)=0d0
            m_sphot(i,j)=0d0
         ENDDO
      ENDDO
      IF(vv .LE. m_vvmin) THEN
*///////////////////////////////////////////////////
*//    no photon above detectability threshold    //
*///////////////////////////////////////////////////
         m_WtMass  = 1d0
         m_WtDil  = 1d0
         m_WtCut  = 1d0
         WtDil0 = 1d0          !test
         WtCut0 = 1d0          !test
         m_nphot=0
      ELSE
*/////////////////////////////////////////////////////////
*// one or more photons, generate photon multiplicity   //
*// nphot = poisson(AvMult) + 1                         //
*/////////////////////////////////////////////////////////
         CALL BornV_GetAvMult(AvMult)
 100     CONTINUE
         CALL KarLud_PoissGen(AvMult,m_nmax,m_nphot,rr)
         m_nphot = m_nphot+1
* For special tests of program at fixed multiplicity (for advc. users)
         IF((m_MltISR .NE. 0) .AND. (m_nphot .NE. m_MltISR)) GOTO 100
         IF(m_nphot .EQ. 1) THEN
            xph(1)=vv
         ELSE
            xph(1)=vv
            DO i=2,m_nphot
               xph(i)=vv*(m_vvmin/vv)**rr(i-1)
            ENDDO
         ENDIF ! nphot
         m_WtMass=1d0
         DO i=1,m_nphot
            xk=xph(i)
            am2  = (m_amel/Ene)**2
            CALL KarLud_AngBre(am2,del1,del2,cg,sg,dist0,dist1)
            dist0 = dist0 *m_Xenph
            m_WtMass    =m_WtMass *(dist1/dist0)
            CALL PseuMar_MakeVec(rvec,1)
            phi=2d0*pi*rvec(1)
            xphot(i,1)=xk*sg*cos(phi)
            xphot(i,2)=xk*sg*sin(phi)
            xphot(i,3)=xk*cg
            xphot(i,4)=xk
            m_yini(i)    =xk*del1/2d0
            m_zini(i)    =xk*del2/2d0
         ENDDO
*///////////////////////////////////////////////////////////////////////////
*// Here we determine dilatation factor for rescaling 4-momenta           //
*// of all photons such that total 4-momentum is conserved (for fixed v)  //
*///////////////////////////////////////////////////////////////////////////
         IF(m_nphot .EQ. 1) THEN
            DilFac0 = 1d0
            DilFac  = 1d0
            DilJac  = 1d0
         ELSE
            DO k=1,4
               pk(k)=0d0
               pp(k)=0d0
            ENDDO
            pp(4)=2d0           ! total energy in units of ene
            DO i=1,m_nphot
               DO k=1,4
                  pk(k)=pk(k)+xphot(i,k)
               ENDDO
            ENDDO
            ppdpp = pp(4)**2-pp(3)**2-pp(2)**2-pp(1)**2
            pkdpk = pk(4)**2-pk(3)**2-pk(2)**2-pk(1)**2
            ppdpk = pp(4)*pk(4)-pp(3)*pk(3)-pp(2)*pk(2)-pp(1)*pk(1)
            AA    = ppdpp*pkdpk/(ppdpk)**2
*     Dilatation factor
            DilFac0 = 2d0*ppdpk/ppdpp/vv
            DilFac  = DilFac0*.5d0*(1d0+sqrt(1d0-vv*AA))
*     and the corresponding jacobian factor
            DilJac  = (1d0+1d0/sqrt(1d0-vv*AA))/2d0
         ENDIF
         DilJac0 = (1d0+1d0/sqrt(1d0-vv))/2d0  !!! as in crude v-dist. in BornV_RhoVesko1
         m_WtDil  = DilJac/DilJac0
         m_WtCut  = 1d0
         WtDil0   = 1d0 /DilJac0   ! test
         WtCut0   = 1d0            ! test
*     scale down photon energies and momenta
         DO i=1,m_nphot
            m_yini(i) =m_yini(i)/DilFac
            m_zini(i) =m_zini(i)/DilFac
            DO k=1,4
               m_sphot(i,k)=xphot(i,k)/DilFac
            ENDDO
         ENDDO
*     Check on lower energy cut-off
         IF(m_sphot(m_nphot,4) .LT. m_vvmin)      m_WtCut = 0d0
         IF(xphot(m_nphot,4)/DilFac0 .LT. m_vvmin ) WtCut0 =0d0 !!! test
      ENDIF ! vv
*     Photon momenta rescaled into GEV units
      DO j=1,4
         m_sphum(j)=0d0
      ENDDO
      DO  i=1,m_nphot
         DO  j=1,4
            m_sphot(i,j) = m_sphot(i,j)*Ene
            m_sphum(j)   = m_sphum(j) +m_sphot(i,j)
         ENDDO
      ENDDO
* 4-momentum left after photon emission
      DO k=1,4
         PX(k)= -m_sphum(k)
      ENDDO
      PX(4)=PX(4)+XXXene
* Total ISR weight
      IF(m_KeyWtm .EQ. 1) m_WtMass=1d0
*
      WtIni = m_WtMass *m_WtDil *m_WtCut
*     ==============================
*((((((((((((((((((((((((((((
* Testing/debug part, some variables exported up to KK2f class
* NO HARM IF OMITTED !!!!
      CALL KK2f_SetOneY(252,m_WtMass)
      CALL KK2f_SetOneY(253,m_WtDil)
      CALL KK2f_SetOneY(254,m_WtCut)
* Auxiliary weights for tests of crude v-distr.
      CALL KK2f_SetOneY(263,WtDil0)
      CALL KK2f_SetOneY(264,WtCut0)
*))))))))))))))))))))))))))))
*----------------------------
      END

      SUBROUTINE KarLud_AngBre(am2,del1,del2,costhg,sinthg,dist0,dist1)
*//////////////////////////////////////////////////////////////////////////////
*// This routine generates photon angular distribution                       //
*// in the rest frame of the fermion pair.                                   //
*// The distribution is the S-factor without mass term,                      //
*// i.e. without terms 2p_1p_2/(kp_1)(kp_2)                                  //
*// Fermion mass is treated exactly!                                         //
*// INPUT:                                                                   //
*//     am2 = 4*massf**2/s where massf is fermion mass                       //
*//     and s is effective mass squared of the parent fermion-pair.          //
*// OUTPUT:                                                                  //
*//     del1= 1-beta*cos(theta)                                              //
*//     del2= 1+beta*cos(theta)                                              //
*//     costhg, sinthg, cos and sin of the photon                            //
*//     angle with respect to fermions direction                             //
*//     dist0 = distribution generated, without m**2/(kp)**2 terms           //
*//     dist1 = distribution with m**2/(kp)**2 terms                         //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  am2,del1,del2,costhg,sinthg,dist0,dist1
* locals
      DOUBLE PRECISION  a,eps,beta
      REAL              rn(10)
*------------------------------------------------------------------------------
      CALL PseuMar_MakeVec(rn,2)
      beta =sqrt(1.d0-am2)
      eps  =am2/(1.d0+beta)                     != 1-beta
      del1 =(2.d0-eps)*(eps/(2.d0-eps))**rn(1)  != 1-beta*costhg
      del2 =2.d0-del1                           != 1+beta*costhg
* calculation of sin and cos theta from internal variables
      costhg=(del2-del1)/(2*beta)               ! exact
      sinthg=sqrt(del1*del2-am2*costhg**2)      ! exact
* symmetrization
      IF(rn(2) .LE. 0.5d0) THEN
        a=del1
        del1=del2
        del2=a
        costhg= -costhg
      ENDIF
      dist0=1d0/(del1*del2)*(1d0 -am2/2d0)
      dist1=1d0/(del1*del2) 
     $     *(1d0 -am2/2d0 -am2/4d0*(del1/del2+del2/del1))
* totaly equivalent formula is the following
*     dist1=1d0/(del1*del2)   *beta*sinthg**2/(del1*del2)
      END

      SUBROUTINE KarLud_PoissGen(average,nmax,mult,rr)
*//////////////////////////////////////////////////////////////////////////////
*// Last corr. nov. 91                                                       //
*// This generates photon multipl. nphot according to poisson distr.         //
*// Input:  average = average multiplicity                                   //
*//         nmax  = maximum multiplicity                                     //
*// Output: mult = generated multiplicity                                    //
*//         rr(1:100) list of ordered uniform random numbers,                //
*//         a byproduct result, to be eventually used for some further       //
*//         purpose (i.e.  generation of photon energies).                   //
*//////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER           nmax,mult
      DOUBLE PRECISION  rr(*),average
* locals
      DOUBLE PRECISION  sum,y
      INTEGER           it,nfail,nn
      REAL              rvec(10)
      DATA nfail/0/
*------------------------------------------------------------------------------
 50   nn=0
      sum=0d0
      DO it=1,nmax
         CALL PseuMar_MakeVec(rvec,1)
         y= log(rvec(1))
         sum=sum+y
         nn=nn+1
         rr(nn)=sum/(-average)
         IF(sum .LT. -average) GOTO 130
      ENDDO
      nfail=nfail+1
      IF(nfail .GT. 100) GOTO 900
      GOTO 50
 130  mult=nn-1
      RETURN
 900  WRITE(*,*) ' poissg: to small nmax ',nmax
      STOP
      END


      SUBROUTINE KarLud_ZBoostAll(exe)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   performs z-boost on all momenta of the event                            //
*//   this z-boost corresponds to beamstrahlung or beamspread                 //
*//   and is done at the very end of generation, after m.el. calculation      //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION  exe
      INTEGER           j,k
      DOUBLE PRECISION  ph(4)
*
      IF( exe.EQ. 1d0) RETURN
      CALL KinLib_Boost(3,exe,m_p1,m_p1)
      CALL KinLib_Boost(3,exe,m_p2,m_p2)
      CALL KinLib_Boost(3,exe,m_q1,m_q1)
      CALL KinLib_Boost(3,exe,m_q2,m_q2)
      CALL KinLib_Boost(3,exe,m_sphum,m_sphum)
      CALL KinLib_Boost(3,exe,m_PX,m_PX)
      DO j=1,m_nphot
         DO k=1,4
            ph(k) = m_sphot(j,k)
         ENDDO
         CALL KinLib_Boost(3,exe,ph,ph)
         DO k=1,4
            m_sphot(j,k) = ph(k)
         ENDDO
      ENDDO
      END


      SUBROUTINE KarLud_GetXXXene(XXXene)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION  XXXene
*
      XXXene = m_XXXene
      END

      SUBROUTINE KarLud_GetExe(Exe)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION  Exe
*
      Exe = m_Exe
      END

      SUBROUTINE KarLud_Getvvmax(vvmax)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION  vvmax
*
      vvmax = m_vvmax
      END

      SUBROUTINE KarLud_GetVVxx(vv,x1,x2)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION  vv,x1,x2
*
      vv = m_vv
      x1 = m_x1
      x2 = m_x2
      END


      SUBROUTINE KarLud_GetSudakov1(iphot,y,z)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get sudakovs of i-th photon                                             //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER iphot
      DOUBLE PRECISION   y,z
*
      y = m_yini(iphot)
      z = m_zini(iphot)
      END

      SUBROUTINE KarLud_GetNphot(nphot)     
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get photon multiplicity                                                 //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER nphot
*
      nphot = m_nphot
      END

      SUBROUTINE KarLud_GetKFfin(KFfin)     
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get photon multiplicity                                                 //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER KFfin
*
      KFfin = m_KFfin
      END

      SUBROUTINE KarLud_GetSudakov(nphot,yini,zini)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get all Sudakovs                                                        //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER nphot
      DOUBLE PRECISION   yini(*),zini(*)
      INTEGER i
*
      nphot = m_nphot
      DO i=1,m_nphot
         yini(i) = m_yini(i)
         zini(i) = m_zini(i)
      ENDDO
      END

      SUBROUTINE KarLud_GetPhoton1(iphot,phot)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   get i-th photon momentum                                                //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER iphot
      DOUBLE PRECISION   phot(4)
      INTEGER k
*
      DO k=1,4
         phot(k) = m_sphot(iphot,k)
      ENDDO
      END


      SUBROUTINE KarLud_GetPhotons(nphot,sphot)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   Get all photons                                                         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER nphot
      DOUBLE PRECISION   sphot(m_npmx,4)
      INTEGER j,k
*
      nphot = m_nphot
      DO j=1,m_nphot
         DO k=1,4
            sphot(j,k) = m_sphot(j,k)
         ENDDO
      ENDDO
      END

      SUBROUTINE KarLud_GetBeams(p1,p2)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   In the case of beamstrahlung these are beams AFTER beamstrahlung        //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION   p1(4),p2(4)
      INTEGER k
*
      DO k=1,4
         p1(k) = m_p1(k)
         p2(k) = m_p2(k)
      ENDDO
      END

      SUBROUTINE KarLud_GetBeasts(p1,p2)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   In the case of beamstrahlung these are photons of the  beamstrahlung    //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION   p1(*),p2(*)
      INTEGER k
*-----------------------------------------
      DO k=1,4
         p1(k) = 0d0
         p2(k) = 0d0
      ENDDO
      p1(4) =  0.5d0*m_CMSene*m_x1
      p1(3) =  p1(4)
      p2(4) =  0.5d0*m_CMSene*m_x2
      p2(3) = -p2(4)
      END

      SUBROUTINE KarLud_GetPX(PX)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION   PX(4)
      INTEGER k
*
      DO k=1,4
         PX(k) = m_PX(k)
      ENDDO
      END

      SUBROUTINE KarLud_WtMass(WtMass)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      DOUBLE PRECISION   WtMass
*
      WtMass = m_WtMass
      END


      SUBROUTINE KarLud_Print(iev,ie1,ie2)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//  Prints out four momenta of INITIAL state                                 //
*//  and the serial number of event iev on unit m_out                         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER  iev,ie1,ie2
      CHARACTER*8 txt
      DOUBLE PRECISION    sum(4),ams,amph,amf1,amf2
      INTEGER  i,k
*--------------------------------------------------------
      IF( (iev .GE. ie1) .AND. (iev .LE. ie2) ) THEN
         txt = '  KarLud '
         WRITE(m_out,*) 
     $        '=========== ',txt,' ======================>',iev
         amf1 = m_p1(4)**2-m_p1(3)**2-m_p1(2)**2-m_p1(1)**2
         amf1 = sqrt(abs(amf1))
         amf2 = m_p2(4)**2-m_p2(3)**2-m_p2(2)**2-m_p2(1)**2
         amf2 = sqrt(abs(amf2))
         WRITE(m_out,3100) 'p1',(  m_p1(  k),k=1,4),amf1
         WRITE(m_out,3100) 'p2',(  m_p2(  k),k=1,4),amf2
         DO i=1,m_nphot
            amph = m_sphot(i,4)**2-m_sphot(i,3)**2
     $            -m_sphot(i,2)**2-m_sphot(i,1)**2
            amph = sqrt(abs(amph))
            WRITE(m_out,3100) 'pho',(m_sphot(i,k),k=1,4),amph
         ENDDO
         DO k=1,4
            sum(k)=m_p1(k)+m_p2(k)
         ENDDO
         DO i=1,m_nphot
            DO k=1,4
               sum(k)=sum(k)-m_sphot(i,k)
            ENDDO
         ENDDO
         ams = sum(4)**2-sum(3)**2-sum(2)**2-sum(1)**2
         ams = sqrt(abs(ams))
         WRITE(m_out,3100) 'sum',(  sum(  k),k=1,4),ams
      ENDIF
 3100 FORMAT(1x,a3,1x,5f20.14)
      END

      SUBROUTINE KarLud_Print1(nout)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//  Prints out four momenta of INITIAL state                                 //
*//  and the serial number of event iev on unit m_out                         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'KarLud.h'
      INTEGER  nout
      CHARACTER*8 txt
      DOUBLE PRECISION    sum(4),ams,amph,amf1,amf2
      INTEGER  i,k
*--------------------------------------------------------
      txt = '  KarLud '
      WRITE(nout,*) '=========== ',txt,' ======================',m_NevGen
      amf1 = m_p1(4)**2-m_p1(3)**2-m_p1(2)**2-m_p1(1)**2
      amf1 = sqrt(abs(amf1))
      amf2 = m_p2(4)**2-m_p2(3)**2-m_p2(2)**2-m_p2(1)**2
      amf2 = sqrt(abs(amf2))
      WRITE(nout,3100) 'p1',(  m_p1(  k),k=1,4),amf1
      WRITE(nout,3100) 'p2',(  m_p2(  k),k=1,4),amf2
      DO i=1,m_nphot
         amph = m_sphot(i,4)**2-m_sphot(i,3)**2
     $        -m_sphot(i,2)**2-m_sphot(i,1)**2
         amph = sqrt(abs(amph))
         WRITE(nout,3100) 'pho',(m_sphot(i,k),k=1,4),amph
      ENDDO
      DO k=1,4
         sum(k)=m_p1(k)+m_p2(k)
      ENDDO
      DO i=1,m_nphot
         DO k=1,4
            sum(k)=sum(k)-m_sphot(i,k)
         ENDDO
      ENDDO
      ams = sum(4)**2-sum(3)**2-sum(2)**2-sum(1)**2
      ams = sqrt(abs(ams))
      WRITE(nout,3100) 'sum',(  sum(  k),k=1,4),ams
 3100 FORMAT(1x,a3,1x,5f20.14)
      END


*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//                       End of CLASS  KarLud                                //
*///////////////////////////////////////////////////////////////////////////////


*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                           //
*//                                       KinLib                                                              //
*//                                                                                                           //
*//                   Library of kinematics operations on Lorentz vectors and matrices                        //
*//                                                                                                           //
*//  This class is just collection of tools for Lorentz vectors and transformation                            //
*//  Probably in future will be incorporated in the corresponding classes                                     //
*//                                                                                                           //
*// KinLib_ThetaD(PX,p1,p2,q1,q2,Svar,CosTheta)  : Provides Svar and single cos(theta) among p1,p2 and q1,q2  //
*// KinLib_SvarThet(p1,p2,q1,q2,Svar,CosTheta)   : The same as ThetaD but Energy conservation required        //
*// KinLib_ThetaR(Qtot,pp1,pp2,qq1,qq2,cth11,cth12,cth21,cth22) : cos(theta) of four scattering angles        //
*// KinLib_phspc2(qq,am1,am2,q1,q2,wt)                     : Generates q1, q2 with masses am1,am2             //
*// KinLib_givpair(cmsene,am1,am2,p1,p2,beta,eta1,eta2)    : Constructs  p1,p2 along z-axis                   //
*// KinLib_DefPair(cmsene,am1,am2,p1,p2)                   : Constructs  p1,p2 along z-axis                   //
*// KinLib_RotEul(the,phi,pvec,qvec)                  : Theta-phi rotation  y-rot(the) z-rot(phi)             //
*// KinLib_RotEulInv(the,phi,pvec,qvec)               : Inverse of KinLib_RotEul                              //
*// KinLib_RotEuler(alfa,beta,gamma,pvec,qvec)        : Full Euler rotation R_3(alpha)*R_2(beta)*R_3(gamma)   //
*// KinLib_RotEulerInv(alfa,beta,gamma,pvec,qvec)     : Inverse of KinLib_RotEuler                            //
*// KinLib_BostQ(Mode,QQ,pp,r)                        : Boost to rest frame of arbitrary timelike QQ          //
*// KinLib_BostQQ(Mode,QQ,pp,r)                       : Boost to rest frame of arbitrary timelike QQ          //
*// KinLib_DefBostQQ(Mode,QQ,Lor)                     : defines Lorenz transformation Lor of BostQQ           //
*// KinLib_Rotor(k1,k2,ph1,pvec,qvec)                 : Rotation in any of the three planes k1,k2             //
*// KinLib_DefRotor(k1,k2,phi,Lor)                    : Defines rotation matrix of  KinLib_Rotor              //
*// KinLib_Boost(k,exe,pvec,qvec)           : Boost along k-th axis, exe=exp(eta), eta= hiperbolic velocity   //
*// KinLib_DefBoost(kaxis,exe,Lor)          : Defines boost matrix the same as in  KinLib_Boost               //
*// KinLib_RotTranspose(Lor)                          : Transpose spacelike part of matrix Lor                //
*// KinLib_LorCopy(Lor1,Lor2)                         : Copy Lor1 --> Lor2                                    //
*// KinLib_RotColumn(e1,e2,e3,Rot)          : Column-wise construction of rotation using 3 versors            //
*// KinLib_LorMult(Lor1,Lor2,Lor3)          : Multiply matrices Lor3=Lor1*Lor2 for Lorenz transformations     //
*// KinLib_VecTrasform(Lor,p,q)             : Multiply vector and matrix, q=Lor*p (Lorentz transform)         //
*// FUNCTION KinLib_AngPhi(x,y)             : calculates angle in (0,2*pi) range out of (x,y) vector          //
*// KinLib_VecPrint(nunit,word,pp)          : prints single momentum 'pp' on unit 'nunit' with comment 'word' //
*// KinLib_LorPrint(nunit,word,Lor)         : Print transformation matrix Lor                                 //
*//                                                                                                           //
*///////////////////////////////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE KinLib_ThetaD(PX,p1,p2,q1,q2,Svar,CosTheta)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Provides Svar and  ONE cos(theta) among p1,p2 and q1,q2                       //
*//   Energy conservation NOT required!!                                            //
*//   The angle is beteween 3-vectors (p1-p2) and (q1-q2) in PX rest frame.         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION     PX(4),p1(4),p2(4),q1(4),q2(4)
      DOUBLE PRECISION     Svar,CosTheta
      DOUBLE PRECISION     pd(4),qd(4),a,b
      INTEGER   k
*
      DO k=1,4
         pd(k) = p1(k)-p2(k)
         qd(k) = q1(k)-q2(k)
      ENDDO
      Svar = PX(4)**2-PX(3)**2-PX(2)**2-PX(1)**2
      IF( Svar .LE. 0d0 ) THEN
         WRITE(*,*) '++++++++++ KinLib_ThetaDr: PX not timelike'
         STOP
      ENDIF
      a = PX(4)*pd(4) -PX(3)*pd(3) -PX(2)*pd(2) -PX(1)*pd(1)
      b = PX(4)*qd(4) -PX(3)*qd(3) -PX(2)*qd(2) -PX(1)*qd(1)
      DO k=1,4
         pd(k) = pd(k) - a*PX(k)/Svar
         qd(k) = qd(k) - b*PX(k)/Svar
      ENDDO
      a = pd(4)**2-pd(3)**2-pd(2)**2-pd(1)**2
      b = qd(4)**2-qd(3)**2-qd(2)**2-qd(1)**2
      IF( a*b .LE. 0d0 ) THEN
         WRITE(*,*) '++++++++++ KinLib_ThetaDr: a,b=',a,b
         STOP
      ENDIF
      CosTheta = -(qd(4)*pd(4) -qd(3)*pd(3) -qd(2)*pd(2) -qd(1)*pd(1))/DSQRT(ABS(a*b))
*
      IF( ABS(CosTheta) .GT. 1d0 ) THEN
         WRITE(*,*) '++++++++++ KinLib_ThetaDr: CosTheta= ',CosTheta
      ENDIF
*
      END

      SUBROUTINE KinLib_SvarThet(p1,p2,q1,q2,Svar,CosTheta)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Provides Svar and  ONE cos(theta) among p1,p2 and q1,q2                       //
*//   Energy conservation required!!                                                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION     p1(4),p2(4),q1(4),q2(4)
      DOUBLE PRECISION     Svar,CosTheta
*
      IF( ABS((p1(4)+p2(4)) - (q1(4)+q2(4))) .GT. 1d-50 ) THEN
         WRITE(*,*) '++++++++++ KinLib_ThetSvar: not conserved energy!!!!'
         STOP
      ENDIF
*
      Svar = (p1(4)+p2(4))**2-(p1(3)+p2(3))**2-(p1(2)+p2(2))**2-(p1(1)+p2(1))**2
      CosTheta=      (p1(3)*q1(3)+p1(2)*q1(2)+p1(1)*q1(1))
     $          /SQRT(p1(3)*p1(3)+p1(2)*p1(2)+p1(1)*p1(1))
     $          /SQRT(q1(3)*q1(3)+q1(2)*q1(2)+q1(1)*q1(1))
      END

      SUBROUTINE KinLib_ThetaR(Qtot,pp1,pp2,qq1,qq2,cth11,cth12,cth21,cth22)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Provides cos(theta) for four scattering angles among p1,p2 and q1,q2          //
*//   Angles are calculated in the rest frame of Qtot                               //
*//   Called in programs calculating Born(p1,p2,q1,q2) distribution.                //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  cth11,cth12,cth21,cth22
      DOUBLE PRECISION  Qtot(*),pp1(*),pp2(*),qq1(*),qq2(*)
      DOUBLE PRECISION  p1(4),p2(4),q1(4),q2(4)
      DOUBLE PRECISION  q1d,q2d,p1d,p2d
*
* Boost to Z/gamma frame
      CALL KinLib_BostQ(1,Qtot,pp1,p1)
      CALL KinLib_BostQ(1,Qtot,pp2,p2)
      CALL KinLib_BostQ(1,Qtot,qq1,q1)
      CALL KinLib_BostQ(1,Qtot,qq2,q2)
* Calculate all four possible angles
      q1d=        sqrt(q1(1)**2 +q1(2)**2 +q1(3)**2)
      q2d=        sqrt(q2(1)**2 +q2(2)**2 +q2(3)**2)
      p1d=        sqrt(p1(1)**2 +p1(2)**2 +p1(3)**2)
      p2d=        sqrt(p2(1)**2 +p2(2)**2 +p2(3)**2)
      cth11 = (p1(1)*q1(1) +p1(2)*q1(2) +p1(3)*q1(3))/p1d/q1d
      cth12 =-(p1(1)*q2(1) +p1(2)*q2(2) +p1(3)*q2(3))/p1d/q2d
      cth21 =-(p2(1)*q1(1) +p2(2)*q1(2) +p2(3)*q1(3))/p2d/q1d
      cth22 = (p2(1)*q2(1) +p2(2)*q2(2) +p2(3)*q2(3))/p2d/q2d
c[[[
c      cth11 = (q1(1)*p1(1) +q1(2)*p1(2) +q1(3)*p1(3))/q1d/p1d
c      cth12 =-(q1(1)*p2(1) +q1(2)*p2(2) +q1(3)*p2(3))/q1d/p2d
c      cth21 =-(q2(1)*p1(1) +q2(2)*p1(2) +q2(3)*p1(3))/q2d/p1d
c      cth22 = (q2(1)*p2(1) +q2(2)*p2(2) +q2(3)*p2(3))/q2d/p2d
c]]]
      END

      SUBROUTINE KinLib_phspc2(qq,am1,am2,q1,q2,wt)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*// Generates q1, q2 with masses am1,am2, such that qq = q1+q2                   //
*// In the qq rest frame spherical density is flat                               //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  qq(*),q1(*),q2(*),am1,am2,wt
      DOUBLE PRECISION  pi
      PARAMETER( pi = 3.1415926535897932d0)
      DOUBLE PRECISION  cmsene,cth,phi,beta,eta1,eta2,the
      REAL              rvec(10)

      cmsene= sqrt(qq(4)**2-qq(3)**2-qq(2)**2-qq(1)**2)
      CALL PseuMar_MakeVec(rvec,2)
      cth= 1.d0 -2.d0*rvec(1)
      the= acos(cth)
      phi= 2.d0*pi*rvec(2)
      CALL KinLib_givpair(cmsene,am1,am2,q1,q2,beta,eta1,eta2)
      CALL KinLib_RotEul(the,phi,q1,q1)
      CALL KinLib_BostQ(  -1,qq,q1,q1)
      CALL KinLib_RotEul(the,phi,q2,q2)
      CALL KinLib_BostQ(  -1,qq,q2,q2)
      wt = beta/2d0
      END

      SUBROUTINE KinLib_givpair(cmsene,am1,am2,p1,p2,beta,eta1,eta2)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//  For CMS energy = cmsene it defines two 'decay' momenta p1,p2                //
*//  in their rest frame, along z-axix                                           //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  cmsene,am1,am2,beta,eta1,eta2
      DOUBLE PRECISION  p1(*),p2(*)
      DOUBLE PRECISION  ener,svar
*
      ener  =  cmsene/2d0
      svar  =  cmsene**2
      beta  =  sqrt((svar-(am1-am2)**2)
     $             *(svar-(am1+am2)**2))/svar
      eta1=    (svar+am1**2-am2**2)/svar
      eta2  =  (svar-am1**2+am2**2)/svar

      p1(1)  =  0d0
      p1(2)  =  0d0
      p1(3)  =  ener*beta
      p1(4)  =  ener*eta1

      p2(1)  =  0d0
      p2(2)  =  0d0
      p2(3)  = -ener*beta
      p2(4)  =  ener*eta2
      END

      SUBROUTINE KinLib_DefPair(cmsene,am1,am2,p1,p2)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//  For CMS energy = cmsene it defines two 'decay' momenta p1,p2                //
*//  in their rest frame, along z-axix                                           //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  cmsene,am1,am2,beta,eta1,eta2
      DOUBLE PRECISION  p1(*),p2(*)
      DOUBLE PRECISION  ener,svar
*
      ener  =  cmsene/2d0
      svar  =  cmsene**2
      beta  =  sqrt((svar-(am1-am2)**2)
     $             *(svar-(am1+am2)**2))/svar
      eta1=    (svar+am1**2-am2**2)/svar
      eta2  =  (svar-am1**2+am2**2)/svar

      p1(1)  =  0d0
      p1(2)  =  0d0
      p1(3)  =  ener*beta
      p1(4)  =  ener*eta1

      p2(1)  =  0d0
      p2(2)  =  0d0
      p2(3)  = -ener*beta
      p2(4)  =  ener*eta2
      END

      SUBROUTINE KinLib_RotEul(the,phi,pvec,qvec)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//      Theta-phi rotation, it turns vector r along z-axis into r(theta,phi)    //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  the,phi,pvec(4),qvec(4)

      CALL KinLib_Rotor(3,1,the,pvec,qvec) ! y-rotation
      CALL KinLib_Rotor(1,2,phi,qvec,qvec) ! z-rotation
      END

      SUBROUTINE KinLib_RotEulInv(the,phi,pvec,qvec)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//      Inverse of KinLib_RotEul                                                //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  the,phi,pvec(4),qvec(4)

      CALL KinLib_Rotor(1,2,-phi,pvec,qvec) ! z-rotation
      CALL KinLib_Rotor(3,1,-the,qvec,qvec) ! y-rotation
      END

      SUBROUTINE KinLib_RotEuler(alfa,beta,gamma,pvec,qvec)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//      Full Euler rotation R_3(alpha)*R_2(beta)*R_3(gamma)                     //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  pvec(4),qvec(4),alfa,beta,gamma
*
      CALL KinLib_Rotor(1,2, alfa,  pvec,qvec) ! z-rotation
      CALL KinLib_Rotor(3,1, beta,  qvec,qvec) ! y-rotation
      CALL KinLib_Rotor(1,2, gamma, qvec,qvec) ! z-rotation
      END

      SUBROUTINE KinLib_RotEulerInv(alfa,beta,gamma,pvec,qvec)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//      Inverse of KinLib_RotEuler                                              //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  pvec(4),qvec(4),alfa,beta,gamma
*
      CALL KinLib_Rotor(1,2, -gamma, pvec,qvec) ! z-rotation
      CALL KinLib_Rotor(3,1, -beta,  qvec,qvec) ! y-rotation
      CALL KinLib_Rotor(1,2, -alfa,  qvec,qvec) ! z-rotation
      END

      SUBROUTINE KinLib_BostQ(Mode,QQ,pp,r)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*// Boost along arbitrary axis (as implemented by Ronald Kleiss).                //
*// The method is described in textbook of Jackson on electrodynamics.           //
*// p boosted into r  from actual frame to rest frame of Q                       //
*// forth (Mode = 1) or back (Mode = -1).                                        //
*// Q must be a timelike, p may be arbitrary.                                    //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER   Mode
      DOUBLE PRECISION     QQ(*),pp(*),r(*)
      DOUBLE PRECISION     Q(4),p(4),fac,amq
      INTEGER   k
*
      DO k=1,4
         p(k)=pp(k)
         Q(k)=QQ(k)
      ENDDO
      amQ =dsqrt(Q(4)**2-Q(1)**2-Q(2)**2-Q(3)**2)
      IF    (Mode .EQ. -1) THEN
         r(4) = (p(1)*Q(1)+p(2)*Q(2)+p(3)*Q(3)+p(4)*Q(4))/amQ
         fac  = (r(4)+p(4))/(Q(4)+amQ)
      ELSEIF(Mode .EQ.  1) THEN
         r(4) =(-p(1)*Q(1)-p(2)*Q(2)-p(3)*Q(3)+p(4)*Q(4))/amQ
         fac  =-(r(4)+p(4))/(Q(4)+amQ)
      ELSE
         WRITE(*,*) ' ++++++++ Wrong Mode in KinLib_BostQ ', Mode
         STOP
      ENDIF
      r(1)=p(1)+fac*Q(1)
      r(2)=p(2)+fac*Q(2)
      r(3)=p(3)+fac*Q(3)
      END

      SUBROUTINE KinLib_DefBostQQ(Mode,QQ,Lor)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   defines Lorenz transformation Lor, the same as in BostQQ                   //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER  Mode
      DOUBLE PRECISION    QQ(4),Lor(4,4)
      INTEGER  k,l
      DOUBLE PRECISION    EQ,MQ
*---------------------------
      IF( IABS(MODE) .NE. 1 ) GOTO 900
      EQ = QQ(4)
      IF(EQ .LE. 0d0 ) GOTO 901
      MQ = QQ(4)**2-QQ(1)**2-QQ(2)**2-QQ(3)**2
      IF(MQ .LE. 0d0 ) GOTO 901
      MQ = DSQRT(MQ)
* Construct Lorenz transformation matrix
      DO k=1,3
         DO l=1,3
            Lor(k,l) = QQ(k)*QQ(l)/MQ/(MQ+EQ)
         ENDDO
      ENDDO
      DO k=1,3
         Lor(4,k) = -Mode*QQ(k)/MQ
         Lor(k,4) = -Mode*QQ(k)/MQ
         Lor(k,k) =  Lor(k,k) +1d0
      ENDDO
      Lor(4,4) = EQ/MQ
      RETURN
 900  WRITE(*,*) '++++++++ WRONG Mode in KinLib_BostQQ =',Mode
      STOP
 901  WRITE(*,*) '++++++++ WRONG QQ, not timelike'
      STOP
      END


      SUBROUTINE KinLib_BostQQ(Mode,QQ,pp,r)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*// Boost along arbitrary axis (as implemented by S.J.).                         //
*// The method is described in textbook of Jackson on electrodynamics.           //
*// p boosted into r  from actual frame to rest frame of Q                       //
*// forth (Mode = 1) or back (Mode = -1).                                        //
*// Q must be a timelike, p may be arbitrary.                                    //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INTEGER  Mode
      DOUBLE PRECISION    QQ(*),pp(*),r(*)
      DOUBLE PRECISION    Lor(4,4),p(4)
      DOUBLE PRECISION    EQ,MQ,sum
      INTEGER  i,j,k,l
*---------------------------
      IF( IABS(MODE) .NE. 1 ) GOTO 900
      EQ = QQ(4)
      IF(EQ .LE. 0d0 ) GOTO 901
      MQ = QQ(4)**2-QQ(1)**2-QQ(2)**2-QQ(3)**2
      IF(MQ .LE. 0d0 ) GOTO 901
      MQ = DSQRT(MQ)
      DO i=1,4
         p(i)=pp(i)
      ENDDO
* Construct Lorenz transformation matrix
      DO k=1,3
         DO l=1,3
            Lor(k,l) = QQ(k)*QQ(l)/MQ/(MQ+EQ)
         ENDDO
      ENDDO
      DO k=1,3
         Lor(4,k) = -Mode*QQ(k)/MQ
         Lor(k,4) = -Mode*QQ(k)/MQ
         Lor(k,k) =  Lor(k,k) +1d0
      ENDDO
      Lor(4,4) = EQ/MQ

* Transform vector p, i.e. multiply by matrix Lor
      DO k=1,4
         sum = 0d0
         DO l=1,4
            sum=sum+ Lor(k,l)*p(l)
         ENDDO
         r(k) = sum
      ENDDO
*
      RETURN
 901  WRITE(*,*) '++++++++ KinLib_BostQQ: WRONG QQ, not timelike'
      STOP
 900  WRITE(*,*) '++++++++ KinLib_BostQQ: WRONG Mode = ',Mode
      STOP
      END

      SUBROUTINE KinLib_DefRotor(k1,k2,phi,Lor)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Defines rotation matrix the same as in  KinLib_Rotor                       //
*//   (k1,k2)= (1,2), x-y plane, around z axis                                   //
*//   (k1,k2)= (2,3), y-z plane, around x axis                                   //
*//   (k1,k2)= (3,1), x-z plane, around y axis                                   //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER k1,k2
      DOUBLE PRECISION  phi,Lor(4,4)
      INTEGER i,j,k
*
      DO i=1,4
         DO j=1,4
            Lor(i,j) = 0d0
         ENDDO
         Lor(i,i) = 1d0
      ENDDO
      Lor(k1,k1) =  cos(phi)
      Lor(k2,k2) =  cos(phi)
      Lor(k1,k2) = -sin(phi)
      Lor(k2,k1) =  sin(phi)  
      END

      SUBROUTINE KinLib_Rotor(k1,k2,ph1,pvec,qvec)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//  This is for rotation in any of the three planes                             //
*//  Rotation of (k1,k2) components, with copying of other                       //
*//   (k1,k2)= (1,2), x-y plane, around z axis                                   //
*//   (k1,k2)= (2,3), y-z plane, around x axis                                   //
*//   (k1,k2)= (3,1), x-z plane, around y axis                                   //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER k1,k2
      DOUBLE PRECISION   ph1,pvec(4),qvec(4)
      INTEGER k
      DOUBLE PRECISION   cs,sn,rvec1,rvec2,phi

      phi=ph1
      cs=cos(phi)
      sn=sin(phi)
      DO k=1,4
         qvec(k)=pvec(k)
      ENDDO
      rvec1  = pvec(k1)
      rvec2  = pvec(k2)
      qvec(k1)= cs*rvec1-sn*rvec2
      qvec(k2)= sn*rvec1+cs*rvec2
      END

      SUBROUTINE KinLib_DefBoost(kaxis,exe,Lor)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Defines boost matrix the same as in  KinLib_Boost                          //
*//   boost along k-th axis, exe=exp(eta), eta= hiperbolic velocity.             //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER kaxis
      DOUBLE PRECISION   exe,Lor(4,4)
      INTEGER i,j,k
      DOUBLE PRECISION   ch,sh
*
      IF(exe .LT. 0d0 ) THEN
         WRITE(*,*) '+++++++ KinLib_DefBoost: Wrong exe= ',exe
         STOP
      ENDIF
      DO i=1,4
         DO j=1,4
            Lor(i,j) = 0d0
         ENDDO
         Lor(i,i) = 1d0
      ENDDO
      ch = (exe +1d0/exe)/2d0
      sh = (exe -1d0/exe)/2d0
      k=kaxis
      Lor(4,4) = ch
      Lor(k,k) = ch
      Lor(4,k) = sh
      Lor(k,4) = sh
      END

      SUBROUTINE KinLib_Boost(k,exe,pvec,qvec)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*// boost along k-th axis, exe=exp(eta), eta= hiperbolic velocity.               //
*// unboosted components are copied                                              //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER  k
      DOUBLE PRECISION    exe
      DOUBLE PRECISION    pvec(4),qvec(4)
      DOUBLE PRECISION    rpl,rmi,qpl,qmi
      INTEGER  i
*
      IF(exe .LT. 0d0 ) THEN
         WRITE(*,*) '+++++++ KinLib_Boost: Wrong exe= ',exe
         STOP
      ENDIF
      DO i=1,4
         qvec(i)=pvec(i)
      ENDDO
      rpl=pvec(4)+pvec(k)
      rmi=pvec(4)-pvec(k)
      qpl=rpl*exe
      qmi=rmi/exe
      qvec(k)=(qpl-qmi)/2
      qvec(4)=(qpl+qmi)/2
      END


      SUBROUTINE KinLib_RotTranspose(Lor)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Transpose spacelike part of matrix Lor                                     //
*//   Used for getting inverse of the rotation matrix                            //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  Lor(4,4),x
      INTEGER  k,l
*-------
      DO k=1,2
         DO l=k+1,3
            x= Lor(k,l)
            Lor(k,l) = Lor(l,k)
            Lor(l,k) = x
         ENDDO
      ENDDO
      END

      SUBROUTINE KinLib_LorCopy(Lor1,Lor2)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Copy Lor1 --> Lor2                                                         //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  Lor1(4,4),Lor2(4,4)
      INTEGER  k,l
*-------
      DO k=1,4
         DO l=1,4
c[[[            Lor2(k,l) = Lor1(k,l)
            Lor2(k,l) = Lor1(k,l)
         ENDDO
      ENDDO
      END

      SUBROUTINE KinLib_RotColumn(e1,e2,e3,Rot)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Column-wise construction of rotation using 3 versors                       //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  Rot(4,4)
      DOUBLE PRECISION  e1(4),e2(4),e3(4)
      INTEGER  k,l
*-------
      Rot(4,4) = 1d0
      DO k=1,3
         Rot(k,4) =0d0
         Rot(4,k) =0d0
      ENDDO
      Rot(1,1) = e1(1)
      Rot(2,1) = e1(2)
      Rot(3,1) = e1(3)
      Rot(1,2) = e2(1)
      Rot(2,2) = e2(2)
      Rot(3,2) = e2(3)
      Rot(1,3) = e3(1)
      Rot(2,3) = e3(2)
      Rot(3,3) = e3(3)
      END


      SUBROUTINE KinLib_LorMult(Lor1,Lor2,Lor3)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Multiply matrices Lor3=Lor1*Lor2 for Lorenz transformations                //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION    Lor1(4,4),Lor2(4,4),Lor3(4,4)
      DOUBLE PRECISION    Lor(4,4)
      INTEGER  i,j,k,l
      DOUBLE PRECISION    sum
*-------
      DO i=1,4
         DO j=1,4
            sum=0d0
            DO k=1,4
               sum=sum+Lor1(i,k)*Lor2(k,j)
            ENDDO
            Lor(i,j)=sum
         ENDDO
      ENDDO
      DO i=1,4
         DO j=1,4
            Lor3(i,j) = Lor(i,j)
         ENDDO
      ENDDO
      END


      SUBROUTINE KinLib_VecTrasform(Lor,p,q)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Multiply vector and matrix, q=Lor*p                                        //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION    Lor(4,4),p(4),q(4)
      INTEGER  k,l
      DOUBLE PRECISION    pp(4),sum
*-------
      DO k=1,4
         pp(k)=p(k)
      ENDDO
      DO k=1,4
         sum=0d0
         DO l=1,4
            sum=sum+Lor(k,l)*pp(l)
         ENDDO
         q(k)=sum
      ENDDO
      END


      DOUBLE PRECISION  FUNCTION KinLib_AngPhi(x,y)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//     calculates angle in (0,2*pi) range out of (x,y) vector                   //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  x,y
      DOUBLE PRECISION  pi
      PARAMETER( pi =3.1415926535897932d0)
      DOUBLE PRECISION  the

      IF(ABS(y) .LT. ABS(x)) THEN
        the = ATAN(abs(y/x))
        IF(x .LE. 0d0) the=pi-the
      ELSE
        the = ACOS(x/sqrt(x**2+y**2))
      ENDIF
      IF(y .LT. 0d0) the = 2d0*pi-the
      KinLib_AngPhi = the
      END

      SUBROUTINE KinLib_VecPrint(nunit,word,pp)
*///////////////////////////////////////////////////////////////////////////////
*//                                                                           //
*//   prints single momentum 'pp' on unit 'nunit' with comment 'word'         //
*//                                                                           //
*///////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER      nunit
      CHARACTER*8  word
      DOUBLE PRECISION        pp(4),ams
      INTEGER      i
*----
      ams = pp(4)**2 -pp(3)**2 -pp(2)**2 -pp(1)**2
      IF(ams .GT. 0.0) ams = SQRT(ams)
      WRITE(nunit,'(a8,5(1x,f20.13))') word,(pp(i),i=1,4),ams
      END

      SUBROUTINE KinLib_LorPrint(nunit,word,Lor)
*//////////////////////////////////////////////////////////////////////////////////
*//                                                                              //
*//   Print transformation matrix Lor                                            //
*//                                                                              //
*//////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER      nunit
      CHARACTER*8  word
      DOUBLE PRECISION  Lor(4,4)
      INTEGER k,j

      WRITE(nunit,'(4a)') '::::::::::::::::::::::::::::',
     $                    ' Lorenz Transformation ',word,
     $                    ' :::::::::::::::::::::::::::'
      DO k=1,4
         WRITE(nunit,'(6f20.14)') (Lor(k,j), j=1,4)
      ENDDO
      END
*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
*//                                                                                                           //
*//                          End of CLASS  KinLib                                                             //
*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////
*//                                                                         //
*// Actual version rewritten by S. Jadach, Nov 1997.                        //
*//                                                                         //
*// Universal random number generator proposed by MARSAGLIA and ZAMAN       //
*// in report FSU-SCRI-87-50                                                //
*//        modified by F. James, 1988 and 1989, to generate a vector        //
*//        of pseudorandom numbers rvec of length lenv, and to put in       //
*//        the COMMON block everything needed to specify currrent state,    //
*//        and to add input and output entry points rmarin, rmarut.         //
*// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   //
*// ++  CALLing sequences for ranmar:                                  ++   //
*// ++      CALL ranmar (rvec, len)   returns a vector rvec of len     ++   //
*// ++                   32-bit random floating point numbers between  ++   //
*// ++                   zero and one.                                 ++   //
*// ++      CALL rmarin(i1,n1,n2)   initializes the generator from one ++   //
*// ++                  32-bit integer i1, and number counts n1,n2     ++   //
*// ++                  (for initializing, set n1=n2=0, but to restart ++   //
*// ++                    a previously generated sequence, use values  ++   //
*// ++                    output by rmarut)                            ++   //
*// ++      CALL rmarut(i1,n1,n2)   outputs the value of the original  ++   //
*// ++                  seed and the two number counts, to be used     ++   //
*// ++                  for restarting by initializing to i1 and       ++   //
*// ++                  skipping n2*100000000+n1 numbers.              ++   //
*// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   //
*//                                                                         //
*//         Initializing routine for ranmar, may be called before           //
*//         generating pseudorandom numbers with ranmar. the input          //
*//         values should be in the ranges:  0<=ijklin<=900 000 000         //
*//                                          0<=ntotin<=999 999 999         //
*//                                          0<=ntot2n<<999 999 999!        //
*// to get the standard values in MARSAGLIA's paper, ijklin=54217137        //
*//                                            ntotin,ntot2n=0              //
*//                                                                         //
*/////////////////////////////////////////////////////////////////////////////


      SUBROUTINE PseuMar_Initialize(ijkl_new, ntot_new,ntot2_new)
*/////////////////////////////////////////////////////////////////////////////
*//                                                                         //
*/////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'PseuMar.h'
      INTEGER ijkl_new, ntot_new, ntot2_new
*
      REAL    t,uni,s
      INTEGER m,i24,jj,idum,loop2,now
      INTEGER i,j,ij,k,l,ii,kl

      m_iwarm = 9000009   ! special mark on initialization

      m_ijkl = ijkl_new
      m_ntot = max(ntot_new,0)
      m_ntot2= max(ntot2_new,0)
      
      ij = m_ijkl/30082
      kl = m_ijkl - 30082*ij
      i = MOD(ij/177, 177) + 2
      j = MOD(ij, 177)     + 2
      k = MOD(kl/169, 178) + 1
      l = MOD(kl, 169)
      WRITE(6,'(a,5i10)')
     $     'ranmar initialized: ij,kl,ijkl,ntot,ntot2=',
     $     ij,kl,m_ijkl,m_ntot,m_ntot2
      DO ii= 1, 97
         s = 0.
         t = .5
         DO jj= 1, 24
            m = MOD(MOD(i*j,179)*k, 179)
            i = j
            j = k
            k = m
            l = MOD(53*l+1, 169)
            IF (MOD(l*m,64)  .GE.  32)  s = s+t
            t = 0.5*t
         ENDDO
         m_U(ii) = s
      ENDDO
      m_twom24 = 1.0
      DO i24= 1, 24
         m_twom24 = 0.5*m_twom24
      ENDDO
      m_C  =   362436.*m_twom24
      m_CD =  7654321.*m_twom24
      m_CM = 16777213.*m_twom24
      m_i97 = 97
      m_j97 = 33
*       complete initialization by skipping
*            (ntot2*modcns + ntot) random numbers
      DO loop2= 1, m_ntot2+1
         now = modcns
         IF (loop2  .EQ.  m_ntot2+1)  now=m_ntot
         IF (now  .GT.  0)  THEN
            WRITE(6,'(a,i15)') ' rmarin skipping over ',now
            DO idum = 1, m_ntot
               uni = m_U(m_i97)-m_U(m_j97)
               IF (uni  .LT.  0.)  uni=uni+1.
               m_U(m_i97) = uni
               m_i97 = m_i97-1
               IF (m_i97  .EQ.  0)  m_i97=97
               m_j97 = m_j97-1
               IF (m_j97  .EQ.  0)  m_j97=97
               m_C = m_C - m_CD
               IF (m_C  .LT.  0.)   m_C=m_C+m_CM
            ENDDO
         ENDIF
      ENDDO
      END


      SUBROUTINE PseuMar_MakeVec(rvec,lenv)
*/////////////////////////////////////////////////////////////////////////////
*//                                                                         //
*/////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'PseuMar.h'
*
      REAL              rvec(*)
      INTEGER lenv
      INTEGER ijkl_new, ntot_new,ntot2_new
      REAL              zuni,uni
      INTEGER ivec
*-------------------------------------------------------------------------
      IF (m_iwarm  .NE.  9000009)  THEN
* Default initialization. User has called ranmar without rmarin.
         ijkl_new  = 54217137
         ntot_new  = 0
         ntot2_new = 0
         CALL PseuMar_Initialize(ijkl_new, ntot_new,ntot2_new)
      ENDIF

* Normal entry to generate lenv random numbers
      DO ivec= 1, lenv
         uni = m_U(m_i97)-m_U(m_j97)
         IF (uni  .LT.  0.)  uni=uni+1.
         m_U(m_i97) = uni
         m_i97 = m_i97-1
         IF (m_i97  .EQ.  0)  m_i97=97
         m_j97 = m_j97-1
         IF (m_j97  .EQ.  0)  m_j97=97
         m_C = m_C - m_CD
         IF (m_C  .LT.  0.)   m_C=m_C+m_CM
         uni = uni-m_C
         IF (uni  .LT.  0.) uni=uni+1.
         rvec(ivec) = uni
* Replace exact zeros by uniform distr. *2**-24
         IF (uni  .EQ.  0.)  THEN
            zuni = m_twom24*m_U(2)
*     an exact zero here is very unlikely, but let's be safe.
            IF (zuni  .EQ.  0.) zuni= m_twom24*m_twom24
            rvec(ivec) = zuni
         ENDIF
      ENDDO
      m_ntot  = m_ntot + lenv
      IF (m_ntot  .GE.  modcns)  THEN
         m_ntot2  =  m_ntot2 + 1
         m_ntot   =  m_ntot - modcns
      ENDIF
      END


      SUBROUTINE PseuMar_Out(ijkl_out, ntot_out, ntot2_out)
*/////////////////////////////////////////////////////////////////////////////
*//                                                                         //
*/////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'PseuMar.h'
      INTEGER ijkl_out, ntot_out, ntot2_out
*
      ijkl_out  = m_ijkl
      ntot_out  = m_ntot
      ntot2_out = m_ntot2
      END

*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                     Pseudo-CLASS  QED3                                          //
*//                                                                                 //
*//   Calculation of QED matrix element with Yennie-Frautschi-Suura exponentiation  //
*//   for s-chanel exchange-exchange fermion-anfifermion production processe.       //
*//   Order alpha^1 is complete, beyond O(alf^1) leading-log is mainly exploited.   //
*//                                                                                 //
*//   e+ e- ---> f + fbar + n gamma                                                 //
*//                                                                                 //
*//   The following contributions are included:                                     //
*//                                                                                 //
*//   ISR:  O(L^0*alf^0)                                                            //
*//         O(L^1*alf^1)  O(L^0*alf^1)                                              //
*//         O(L^2*alf^2)                                                            //
*//         O(L^3*alf^3)                                                            //
*//   FSR:  O(L^0*alf^0)                                                            //
*//         O(L^1*alf^1)  O(L^0*alf^1)                                              //
*//         O(L^2*alf^2)                                                            //
*//                                                                                 //
*//   Neglected:                                                                    //
*//      ISR*FSR interferences, spin polarization                                   //
*//      t-chanel exchanges                                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE QED3_Initialize(xpar)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Initialization directly from basic input                                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  xpar(*)
*
      INCLUDE 'QED3.h'
*
      m_IdeWgt = xpar(11)
      m_alfinv = xpar(30)
      m_KeyISR = xpar(20)
      m_vvmin  = xpar(16)
      END

      SUBROUTINE QED3_Make
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   main routine for calculation of long list of the weights                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      SAVE
      INCLUDE 'QED3.h'
*
      DOUBLE PRECISION   xx(4)
      DOUBLE PRECISION   pf1(4),pf2(4)
      DOUBLE PRECISION   qf1(4),qf2(4)
      INTEGER nphox,nphoy
      DOUBLE PRECISION   xphot(100,4),yphot(100,4)
      DOUBLE PRECISION   yini(100),zini(100),yfin(100),zfin(100)
* Elements of beta calculation, Auxiliary/temporary
      DOUBLE PRECISION   beta20,beta21,beta30
      DOUBLE PRECISION   betx12,bety12
      DOUBLE PRECISION   beti12,beti21
      DOUBLE PRECISION   betf01
* Contributions from individual photons
      DOUBLE PRECISION  
     $  betx10(npmx),              ! ISR beta1 tree     times (1+delf1), factorization!
     $  betx11(npmx),              ! ISR beta1 one-loop times (1+delf1), factorization!
     $  bety10(npmx),              ! FSR beta1 tree     times (1+deli1), factorization!
     $  bety11(npmx),              ! FSR beta1 one-loop times (1+deli1), factorization!
     $  betxx20(npmx,npmx),        ! beta2 ISR*ISR tree 
     $  betxy20(npmx,npmx),        ! beta2 ISR*FSR tree 
     $  betyy20(npmx,npmx),        ! beta2 FSR*FSR tree 
     $  beti10(npmx),              ! beta1 tree   ISR only
     $  beti11(npmx),              ! beta1 1-loop ISR only
     $  beti20(npmx,npmx),         ! beta2 tree   ISR only
     $  betf10(npmx),              ! beta1 tree   FSR only, for beta2, beta3
     $  betf11(npmx)               ! beta1 1-loop FSR only, for beta2, beta3
***********************************************************************
      DOUBLE PRECISION   qq(4),pp(4)
      DOUBLE PRECISION   ggf1,ggf2,gggi1,gggi2,gi1,gi2,ggi1,ggi2,gf1,gf2
      DOUBLE PRECISION   cth21,cth12,cth22,cth11
      DOUBLE PRECISION   DisCru
      DOUBLE PRECISION   dist20,disi11,dist10,dist11,dist12,dist30
      DOUBLE PRECISION   andi11,andi21,andi12,andi22,andis,dist21
      DOUBLE PRECISION   hfac1,hfac2,hfac,hfac3,sfacj
      DOUBLE PRECISION   zz,yy,y3,z3,z1,y1,z2,y2,uu,vv,z,y
      DOUBLE PRECISION   ForFin,ForIni,fYFS,fYFSu
      DOUBLE PRECISION   Bor1
c{{{{
      DOUBLE PRECISION   ph(4),kq1,kq2,p1p2,q1q2,tt,tt1,uu1,borc,dig1,sofc
c}}}}

      DOUBLE PRECISION   gami,gamf,delp,delq,deli3,deli2,delf2,delf1,deli1,delf3
      DOUBLE PRECISION   svar,svar1,svar2
      DOUBLE PRECISION   amel,amfin,charge,charg2

      DOUBLE PRECISION   BornV_GetMass,BornV_GetCharge,BornV_Differential

      INTEGER i,j,k,jph,ntree
      INTEGER jph1,jph2,jph3
*
      INTEGER IsFSR,KFbeam,KFfin
*
      INTEGER icont             !debug
      SAVE    icont
      DOUBLE PRECISION   wtm2,wtm0,wtm1
***********************************************************************
**                     Inline functions                              **
***********************************************************************
* Multiplicative mass-correction to truncated S-factor 
      DOUBLE PRECISION   wm0,wm1,wmd,wm7,a,b,del
      wm0(del,a,b)= 1d0 -2d0*del -del*(a/b+b/a)
*
*     O(alf1) amplitude, bremsstrahlung factor, del->0 limit
      wm1(del,a,b)=
     $  1d0 - del*(a/b+b/a)*(1d0-a)*(1d0-b)*2d0/((1d0-a)**2+(1d0-b)**2)
*
* wmd as in BHLUMI, (what about exact mass terms???)
      wmd(del,a,b) =
     $  1d0 + del*(a/b+b/a)*(a**2+b**2)/((1-a)**2+(1-b)**2)
*
* Factorized wm1, as in BHLUMI, to improve numerical stability.
*     Identity wm1=wm7=wm0*wmd, is true up to delta**4 terms
      wm7(del,a,b) = (1d0 +2d0*del -del*(a/b+b/a))
     $ *(1d0 + del*(a**2+b**2)*(a**2+b**2)/((1-a)**2+(1-b)**2)/(a*b))
*
**                 End of inline functions                           **
***********************************************************************
      DATA icont /0/
*     =================================================================
      icont=icont+1
      m_WtBest=1d0

      DO i=1,m_lenwt
         m_WtSet(i)=0d0
      ENDDO
*
      KFbeam = 11                      ! KF=11 is electron
      amel   = BornV_GetMass(KFbeam)
* Actual KFcode of final fermion
      CALL KarLud_GetKFfin(KFfin)
      amfin  = BornV_GetMass(KFfin)
* Final state charge, -1 for electron
      charge = BornV_GetCharge(KFfin)
      charg2 = charge**2

* Check dynamicaly on FSR (quarks!!!)
      CALL KarFin_GetIsFSR(IsFSR)

* ISR
      CALL KarLud_GetSudakov(nphox,yini,zini)
      CALL KarLud_GetPhotons(nphox,xphot)
      CALL KarLud_GetBeams(pf1,pf2)
* FSR
      CALL KarFin_GetSudakov(nphoy,yfin,zfin)
      CALL KarFin_GetPhotons(nphoy,yphot)
      CALL KarFin_GetFermions(qf1,qf2)

* Define 4-mometa for initial/final states and Z
      DO k=1,4
         pp(k)= pf1(k)+ pf2(k)
         qq(k)= qf1(k)+ qf2(k)
         xx(k)= qq(k)
      ENDDO
      DO j=1,nphoy
         DO k=1,4
            xx(k) = xx(k)+yphot(j,k)
         ENDDO
      ENDDO

      svar  = pp(4)**2-pp(3)**2-pp(2)**2-pp(1)**2
      svar1 = xx(4)**2-xx(3)**2-xx(2)**2-xx(1)**2
      svar2 = qq(4)**2-qq(3)**2-qq(2)**2-qq(1)**2
      vv = 1d0 -svar1/svar
      uu = 1d0 -svar2/svar1
      gami =         2d0/m_alfinv/pi*(dlog(svar/amel**2)  -1d0)
      gamf = charg2* 2d0/m_alfinv/pi*(dlog(svar2/amfin**2)-1d0)

* Crude Born distribution
      CALL KK2f_GetBornCru(DisCru)
*
      delp=  amel**2/svar
      delq= amfin**2/svar2
*
      CALL KinLib_ThetaR(xx,pf1,pf2,qf1,qf2,cth11,cth12,cth21,cth22)
      andi11= BornV_Differential(1,KFfin,svar1,cth11,0d0,0d0,0d0,0d0)
      andi12= BornV_Differential(1,KFfin,svar1,cth12,0d0,0d0,0d0,0d0)
      andi21= BornV_Differential(1,KFfin,svar1,cth21,0d0,0d0,0d0,0d0)
      andi22= BornV_Differential(1,KFfin,svar1,cth22,0d0,0d0,0d0,0d0)
*-----------------------------------------------------------
*                beta0 
*-----------------------------------------------------------
* Beta0 components
      CALL QED3_bvirt0(m_alfinv,   1d0,svar , amel,deli1,deli2,deli3)
      CALL QED3_bvirt0(m_alfinv,charg2,svar2,amfin,delf1,delf2,delf3)
*
      IF(m_KeyISR .EQ. 0)  deli1   = 0d0
      IF(m_KeyISR .EQ. 0)  deli2   = 0d0
      IF(IsFSR    .EQ. 0)  delf1   = 0d0
      IF(IsFSR    .EQ. 0)  delf2   = 0d0

* Beta0, initial+final, factorized form
      andis = (andi11 +andi12 +andi21 +andi22)/4
      m_Beta03 = andis*(1d0+deli1+deli2+deli3)*(1d0+delf1+delf2) !O(alf3)
      m_Beta02 = andis*(1d0+deli1+deli2)      *(1d0+delf1+delf2) !O(alf2)
      m_Beta01 = andis*(1d0+deli1)            *(1d0+delf1)       !O(alf1)
      m_Beta00 = andis                                           !O(alf0)
* Initial only
      m_beti03 = andis*(1d0+deli1+deli2+deli3)
      m_beti02 = andis*(1d0+deli1+deli2)
      m_beti01 = andis*(1d0+deli1)
      m_beti00 = andis
* Auxiliary
      betf01 = andis*(1d0+delf1)

c[[[[[[[
c      IF( nphox+nphoy .EQ.0) THEN
c      write(*,*) '||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||'
c      write(*,*) ' QED3: nphox+nphoy =',nphox+nphoy
c      write(*,*) ' QED3: m_Beta01,m_Beta02 = ',m_Beta01,m_Beta02
c      write(*,*) ' QED3: (1d0+deli1)*(1d0+delf1)= ',(1d0+deli1)*(1d0+delf1)
c      write(*,*) ' QED3: (1d0+deli1+deli2)*(1d0+delf1+delf2)= ',(1d0+deli1+deli2)*(1d0+delf1+delf2)
c      ENDIF
c]]]]]]]

*-----------------------------------------------------------
*                beta1 initial
*-----------------------------------------------------------
      m_xBet10 = 0d0
      m_xBet11 = 0d0
      m_xBet12 = 0d0
      m_sbti10 = 0d0
      m_sbti11 = 0d0
      m_sbti12 = 0d0
      IF(m_KeyISR .NE. 0  .AND.  vv .GT. m_vlim1) THEN
         DO jph=1,nphox
            y = yini(jph)
            z = zini(jph)
            sfacj  =  2d0/(y*z) *wm0(delp,y,z)
            m_xSfac(jph) = sfacj
            hfac = wmd(delp,y,z) *sfacj
            gf1 = 0.5d0
            gf2 = 0.5d0
            CALL QED3_Disr1(gami,yini,zini,jph, gi1,gi2,ggi1,ggi2,gggi1,gggi2)
*---- O(alf1) ----,  tree_level --------
*     The unconventional (1+delf1) in betx10 helps ISR*FSR factorization
*     in the O(alf2) semi-analytical x-check
            dist10= (  gi1*gf1*andi11   +gi1*gf2*andi12
     $                +gi2*gf1*andi21   +gi2*gf2*andi22)*hfac
            betx10(jph)=(dist10 -m_Beta00*sfacj )*(1+delf1)
            m_xBet10 = m_xBet10 +betx10(jph) /sfacj
*---- O(alf2) ----,  one_loop_level --------
            dist11= ( ggi1*gf1*andi11  +ggi1*gf2*andi12
     $               +ggi2*gf1*andi21  +ggi2*gf2*andi22)*hfac
            betx11(jph)= dist11*(1+delf1)       -m_Beta01*sfacj
            m_xBet11 = m_xBet11 +betx11(jph) /sfacj
*---- O(alf3) ----,  two_loop_level -------- !!!NEW!!!
            dist12= (gggi1*gf1*andi11 +gggi1*gf2*andi12
     $              +gggi2*gf1*andi21 +gggi2*gf2*andi22)*hfac
            betx12     = dist12*(1+delf1+delf2) -m_Beta02*sfacj
            m_xBet12 = m_xBet12 +betx12      /sfacj
***** pure ISR
            beti10(jph) =  dist10 -m_beti00*sfacj   !O(alf1)
            m_sbti10 = m_sbti10 +beti10(jph) /sfacj
            beti11(jph) =  dist11 -m_beti01*sfacj   !O(alf2)
            m_sbti11 = m_sbti11 +beti11(jph) /sfacj
            beti12      =  dist12 -m_beti02*sfacj   !O(alf3) !!!NEW
            m_sbti12 = m_sbti12 +beti12      /sfacj
         ENDDO
      ELSE
         DO jph=1,nphox
            m_xSfac(jph)  = -1d0
            betx10(jph) =  0d0
            betx11(jph) =  0d0
            beti10(jph) =  0d0
            beti11(jph) =  0d0
         ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta1 final
*-----------------------------------------------------------
      m_yBet10=0d0
      m_yBet11=0d0
      m_yBet12=0d0
      IF(IsFSR .NE. 0  .AND.  uu .GT. m_vlim1) THEN
         DO jph=1,nphoy
            yy = yfin(jph)
            zz = zfin(jph)
            y  = yy/(1 +yy+zz)
            z  = zz/(1 +yy+zz)
            sfacj  =  2d0/(yy*zz)*wm0(delq,yy,zz)
            m_ySfac(jph) = sfacj
            hfac = wmd(delq,y,z) *sfacj
            gi1 = 0.5d0
            gi2 = 0.5d0
            CALL QED3_Dfsr1(gamf,yfin,zfin,jph, gf1,gf2,ggf1,ggf2)
*---- O(alf1) ---,  tree level
*     unconventional (1+deli1) in bety10 helps ISR*FSR factorization 
*     in the O(alf2) semi-analytical x-check
            dist10= (gi1*gf1*andi11   + gi1*gf2*andi12
     $              +gi2*gf1*andi21   + gi2*gf2*andi22)*hfac
            bety10(jph) = (dist10 -m_Beta00*sfacj  )*(1d0+deli1) !!!
            m_yBet10 = m_yBet10 +bety10(jph) /sfacj
*---- O(alf2) ---, one loop level
            dist11= (gi1*ggf1*andi11 + gi1*ggf2*andi12
     $              +gi2*ggf1*andi21 + gi2*ggf2*andi22)*hfac
            bety11(jph) =  dist11*(1+deli1) -m_Beta01*sfacj
            m_yBet11 = m_yBet11 +bety11(jph) /sfacj
*---- O(alf3) ---, two loop level for ISR !!!NEW
*     Additional O(alf2) ISR virtual correction deli2 only
            bety12 = (1+deli1+deli2)*(dist11 -m_Beta00*(1+delf1)*sfacj)
            m_yBet12 = m_yBet12 +bety12 /sfacj
*****  pure FSR *****, for construction of beta2, beta3
            betf10(jph) =  dist10 -m_Beta00*sfacj
            betf11(jph) =  dist11 -betf01*sfacj
         ENDDO
      ELSE
        DO jph=1,nphoy
           m_ySfac(jph)  = -1d0
           bety10(jph) =  0d0
           bety11(jph) =  0d0
        ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta2 initial-initial
*-----------------------------------------------------------
      m_xxBet20=0d0
      m_xxBet21=0d0
      m_sbti20=0d0
      m_sbti21=0d0
      IF(m_KeyISR .NE. 0  .AND.  vv .GT. m_vlim2) THEN
         DO jph2=2,nphox
            DO jph1=1,jph2-1
               hfac1  =  m_xSfac(jph1)*wmd(delp,yini(jph1),zini(jph1))
               hfac2  =  m_xSfac(jph2)*wmd(delp,yini(jph2),zini(jph2))
*     Summation over two LL fragmentation trees for 2 ISR ohotons,
*     photon jph1 is always harder because of low level M.C. generation
               ntree = 2  ! for 2 ISR fragmentation trees
               gi1  = 0d0
               gi2  = 0d0
               ggi1 = 0d0
               ggi2 = 0d0
               CALL QED3_Disr2(gami,yini,zini,jph1, jph1,jph2,
     $                                  gi1,gi2,ggi1,ggi2) ! 1-st tree
               CALL QED3_Disr2(gami,yini,zini,jph1, jph2,jph1,
     $                                  gi1,gi2,ggi1,ggi2) ! 2-nd tree
               gf1 = 0.5d0      ! 0.5d0 for averaging over 2 choices
               gf2 = 0.5d0      ! of Born angles in final state
*---  O(alf2) ---, tree level---------
               dist20= (gi1*gf1*andi11 + gi1*gf2*andi12
     $                 +gi2*gf1*andi21 + gi2*gf2*andi22)
     $                 *hfac1*hfac2/ntree
* In beta20 we use beti10 instead of betx10,
* Reason: unusual definition of betx10, see relevant comment above
               beta20 = dist20
     $              -m_Beta00*m_xSfac(jph1)*m_xSfac(jph2)
     $              -beti10(jph1)*m_xSfac(jph2) -beti10(jph2)*m_xSfac(jph1)
               betxx20(jph1,jph2)=beta20
               m_xxBet20=m_xxBet20 +beta20 /m_xSfac(jph1)/m_xSfac(jph2)
*---  O(alf3) ---, one loop level ---------!!!!NEW!!!!
               dist21= (ggi1*gf1*andi11+ ggi1*gf2*andi12
     $                 +ggi2*gf1*andi21+ ggi2*gf2*andi22)
     $                 *hfac1*hfac2/ntree
               beta21 = dist21*(1+delf1)
     $              -m_Beta01*m_xSfac(jph1)*m_xSfac(jph2)
     $              -betx11(jph1)*m_xSfac(jph2) -betx11(jph2)*m_xSfac(jph1)
               m_xxBet21=m_xxBet21 +beta21 /m_xSfac(jph1)/m_xSfac(jph2)
***** Pure ISR *****
               m_sbti20=m_sbti20 +beta20 /m_xSfac(jph1)/m_xSfac(jph2)
               beti21 = dist21
     $              -m_beti01*m_xSfac(jph1)*m_xSfac(jph2)
     $              -beti11(jph1)*m_xSfac(jph2) -beti11(jph2)*m_xSfac(jph1)
               m_sbti21=m_sbti21 +beti21 /m_xSfac(jph1)/m_xSfac(jph2)
            ENDDO
         ENDDO
      ELSE
         DO  jph2=2,nphox
            DO  jph1=1,nphox
               betxx20(jph1,jph2)= 0d0
            ENDDO
         ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta2 final-final
*-----------------------------------------------------------
      m_yyBet20=0d0
      m_yyBet21=0d0
      IF(IsFSR .NE. 0  .AND.  uu .GT. m_vlim2) THEN
         DO  jph2=2,nphoy
            DO jph1=1,jph2-1
               y1  = yfin(jph1) /(1 +yfin(jph1) +zfin(jph1) )
               z1  = zfin(jph1) /(1 +yfin(jph1) +zfin(jph1) )
               y2  = yfin(jph2) /(1 +yfin(jph2) +zfin(jph2) )
               z2  = zfin(jph2) /(1 +yfin(jph2) +zfin(jph2) )
*     Note y1,z1<1 (yfin,zfin cant be used directly in wmd(...))
               hfac1  =  m_ySfac(jph1)*wmd(delq,y1,z1)
               hfac2  =  m_ySfac(jph2)*wmd(delq,y2,z2)
*              sum over two FSR fragmentation trees
               ntree = 2  ! for 2 FSR fragmentation trees
               gf1 = 0d0
               gf2 = 0d0
               CALL QED3_Dfsr2(yfin,zfin,jph1,jph1,jph2,gf1,gf2) ! 1-st tree
               CALL QED3_Dfsr2(yfin,zfin,jph1,jph2,jph1,gf1,gf2) ! 2-nd tree
               gi1 = 0.5d0        ! 0.5d0 for averaging over 2 choices
               gi2 = 0.5d0        ! of Born angles in initial state
*---- O(alf2) ----, tree level
               dist20 =
     $              (gi1*gf1*andi11+ gi1*gf2*andi12
     $              +gi2*gf1*andi21+ gi2*gf2*andi22)
     $              *hfac1*hfac2/ntree
               beta20 = dist20 
     $              -m_Beta00*m_ySfac(jph1)*m_ySfac(jph2)
     $              -betf10(jph1)*m_ySfac(jph2) -betf10(jph2)*m_ySfac(jph1)
               betyy20(jph1,jph2)=beta20
               m_yyBet20=m_yyBet20 +beta20 /m_ySfac(jph1)/m_ySfac(jph2)
*---- O(alf3) ----, one loop level  !!!NEW
* Primitive ISR virtual correction only
               beta21 = (1d0+deli1)*beta20
               m_yyBet21=m_yyBet21 +beta21 /m_ySfac(jph1)/m_ySfac(jph2)
            ENDDO
         ENDDO
      ELSE
         DO  jph2=2,nphoy
            DO  jph1=1,nphoy
               betyy20(jph1,jph2)= 0d0
            ENDDO
         ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta2 initial-final
*-----------------------------------------------------------
* or in other terminology   beta1_init - beta1_final
      m_xyBet20=0d0
      m_xyBet21=0d0
      IF(m_KeyISR*IsFSR .NE. 0 .AND. vv.GT.m_vlim1 .AND. uu.GT.m_vlim1) THEN
         DO jph1=1,nphox
            DO jph2=1,nphoy
               y1  = yini(jph1)
               z1  = zini(jph1)
               y2  = yfin(jph2) /(1 +yfin(jph2) +zfin(jph2) )
               z2  = zfin(jph2) /(1 +yfin(jph2) +zfin(jph2) )
               hfac1 =    m_xSfac(jph1) *wmd(delp,y1,z1)
               hfac2 =    m_ySfac(jph2) *wmd(delq,y2,z2)
               CALL QED3_Disr1(gami,yini,zini,jph1,
     $                  gi1,gi2,ggi1,ggi2,gggi1,gggi2)
               CALL QED3_Dfsr1(gamf,yfin,zfin,jph2,gf1,gf2,ggf1,ggf2)
*---- O(alf2) -----, tree level
               dist20 = 
     $              (gi1*gf1*andi11+ gi1*gf2*andi12
     $              +gi2*gf1*andi21+ gi2*gf2*andi22)*hfac1*hfac2
               beta20 = dist20 
     $              -m_Beta00*m_xSfac(jph1)*m_ySfac(jph2)
     $              -beti10(jph1)*m_ySfac(jph2) -betf10(jph2)*m_xSfac(jph1)
               betxy20(jph1,jph2)=beta20
               m_xyBet20=m_xyBet20 +beta20 /m_xSfac(jph1)/m_ySfac(jph2)
*---- O(alf3) -----, one loop level  !!!!!!!NEW
* Note that virtual correction is factorized ISR*FSR, as usual
               dist21 = 
     $              (ggi1*ggf1*andi11+ ggi1*ggf2*andi12
     $              +ggi2*ggf1*andi21+ ggi2*ggf2*andi22)*hfac1*hfac2
               beta21 = dist21 
     $              -m_Beta01*m_xSfac(jph1)*m_ySfac(jph2)
     $              -betx11(jph1)*m_ySfac(jph2) -bety11(jph2)*m_xSfac(jph1)
               m_xyBet21=m_xyBet21 +beta21 /m_xSfac(jph1)/m_ySfac(jph2)
            ENDDO
         ENDDO
      ELSE
         DO  jph1=1,nphox
            DO  jph2=1,nphoy
               betxy20(jph1,jph2)= 0d0
            ENDDO
         ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta3 initial-initial-initial
*-----------------------------------------------------------
      m_xxxBet30=0d0
      m_sbti30=0d0
      IF(m_KeyISR .NE. 0  .AND.  vv .GT. m_vlim2) THEN
         DO jph3 = 3,nphox
            DO jph2 = 2,jph3-1
               DO jph1 = 1,jph2-1
                  hfac1  =  m_xSfac(jph1)*wmd(delp,yini(jph1),zini(jph1))
                  hfac2  =  m_xSfac(jph2)*wmd(delp,yini(jph2),zini(jph2))
                  hfac3  =  m_xSfac(jph3)*wmd(delp,yini(jph3),zini(jph3))
*      Summation over 6 LL fragmentation trees for 3 ISR photons,
*      photon jph1 is always harder because of low level M.C. generation
                  ntree = 6     ! for 2 ISR fragmentation trees
                  gi1 = 0d0
                  gi2 = 0d0
                  CALL QED3_Disr3(yini,zini,jph1, jph1,jph2,jph3, gi1,gi2)
                  CALL QED3_Disr3(yini,zini,jph1, jph2,jph1,jph3, gi1,gi2)
                  CALL QED3_Disr3(yini,zini,jph1, jph1,jph3,jph2, gi1,gi2)
                  CALL QED3_Disr3(yini,zini,jph1, jph2,jph3,jph1, gi1,gi2)
                  CALL QED3_Disr3(yini,zini,jph1, jph3,jph1,jph2, gi1,gi2)
                  CALL QED3_Disr3(yini,zini,jph1, jph3,jph2,jph1, gi1,gi2)
                  gf1 = 0.5d0   ! 0.5d0 for averaging over 2 choices
                  gf2 = 0.5d0   ! of Born angles in final state
*---- O(alf3) -----, tree level  !!!!!!!NEW
                  dist30 = 
     $                 (gi1*gf1*andi11+ gi1*gf2*andi12
     $                 +gi2*gf1*andi21+ gi2*gf2*andi22)
     $                 *hfac1*hfac2*hfac3/ntree
                  beta30 = dist30
     $                 -m_Beta00 *m_xSfac(jph1) *m_xSfac(jph2) *m_xSfac(jph3)
     $                 -beti10(jph1) *m_xSfac(jph2) *m_xSfac(jph3)
     $                 -beti10(jph2) *m_xSfac(jph1) *m_xSfac(jph3)
     $                 -beti10(jph3) *m_xSfac(jph1) *m_xSfac(jph2)
     $                 -betxx20(jph1,jph2) *m_xSfac(jph3)
     $                 -betxx20(jph1,jph3) *m_xSfac(jph2)
     $                 -betxx20(jph2,jph3) *m_xSfac(jph1)
                  m_xxxBet30 = m_xxxBet30 
     $                 +beta30/m_xSfac(jph1)/m_xSfac(jph2)/m_xSfac(jph3)
* Pure ISR, simply the same
                  m_sbti30 = m_sbti30 
     $                 +beta30/m_xSfac(jph1)/m_xSfac(jph2)/m_xSfac(jph3)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta3 initial-initial-final
*-----------------------------------------------------------
      m_xxyBet30 = 0d0
      IF(  m_KeyISR .NE. 0  .AND.  vv .GT. m_vlim2 .AND.
     $     IsFSR .NE. 0  .AND.  uu .GT. m_vlim1) THEN
         DO jph2=2,nphox
            DO jph1=1,jph2-1
               DO jph3=1,nphoy
                  hfac1  =  m_xSfac(jph1)*wmd(delp,yini(jph1),zini(jph1))
                  hfac2  =  m_xSfac(jph2)*wmd(delp,yini(jph2),zini(jph2))
                  y3  = yfin(jph3) /(1 +yfin(jph3) +zfin(jph3) )
                  z3  = zfin(jph3) /(1 +yfin(jph3) +zfin(jph3) )
                  hfac3  =  m_ySfac(jph3)*wmd(delq,y3,z3)
                  ntree = 2     ! for 2 ISR fragmentation trees
                  gi1  = 0d0    ! initialization
                  gi2  = 0d0    ! initialization
                  CALL QED3_Disr2(gami,yini,zini,jph1, jph1,jph2,
     $                                    gi1,gi2,ggi1,ggi2) ! 1-st tree
                  CALL QED3_Disr2(gami,yini,zini,jph1, jph2,jph1,
     $                                    gi1,gi2,ggi1,ggi2) ! 2-nd tree
                  CALL QED3_Dfsr1(gamf,yfin,zfin,jph3,gf1,gf2,ggf1,ggf2)
*---- O(alf3) -----, tree level  !!!!!!!NEW
                  dist30= (gi1*gf1*andi11 + gi1*gf2*andi12
     $                    +gi2*gf1*andi21 + gi2*gf2*andi22)
     $                    *hfac1*hfac2*hfac3/ntree
                  beta30 = dist30 
     $                 -m_Beta00 *m_xSfac(jph1) *m_xSfac(jph2) *m_ySfac(jph3)
     $                 -beti10(jph1) *m_xSfac(jph2) *m_ySfac(jph3)
     $                 -beti10(jph2) *m_xSfac(jph1) *m_ySfac(jph3)
     $                 -betf10(jph3) *m_xSfac(jph1) *m_xSfac(jph2)
     $                 -betxx20(jph1,jph2) *m_ySfac(jph3)
     $                 -betxy20(jph1,jph3) *m_xSfac(jph2)
     $                 -betxy20(jph2,jph3) *m_xSfac(jph1)
                  m_xxyBet30 = m_xxyBet30 
     $                 +beta30/m_xSfac(jph1)/m_xSfac(jph2)/m_ySfac(jph3)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
*-----------------------------------------------------------
*                beta3 initial-final-final
*-----------------------------------------------------------
      m_xyyBet30 = 0d0
      IF(  m_KeyISR .NE. 0  .AND.  vv .GT. m_vlim2 .AND.
     $      IsFSR .NE. 0  .AND.  uu .GT. m_vlim1) THEN
         DO  jph2=2,nphoy
            DO jph1=1,jph2-1
               DO jph3=1,nphox
                  y1  = yfin(jph1) /(1 +yfin(jph1) +zfin(jph1) )
                  z1  = zfin(jph1) /(1 +yfin(jph1) +zfin(jph1) )
                  y2  = yfin(jph2) /(1 +yfin(jph2) +zfin(jph2) )
                  z2  = zfin(jph2) /(1 +yfin(jph2) +zfin(jph2) )
                  hfac1  =  m_ySfac(jph1)*wmd(delq,y1,z1)
                  hfac2  =  m_ySfac(jph2)*wmd(delq,y2,z2)
                  hfac3  =  m_xSfac(jph3)*wmd(delp,yini(jph3),zini(jph3))
*     sum over two FSR fragmentation trees
                  ntree = 2    ! for 2 FSR fragmentation trees
                  gf1 = 0d0    ! initialization
                  gf2 = 0d0    ! initialization
                  CALL QED3_Dfsr2(yfin,zfin,jph1, jph1,jph2, gf1,gf2)
                  CALL QED3_Dfsr2(yfin,zfin,jph1, jph2,jph1, gf1,gf2)
                  CALL QED3_Disr1(gami,yini,zini,jph3,
     $                  gi1,gi2,ggi1,ggi2,gggi1,gggi2)
*---- O(alf3) -----, tree level  !!!!!!!NEW
                  dist30= (gi1*gf1*andi11 + gi1*gf2*andi12
     $                    +gi2*gf1*andi21 + gi2*gf2*andi22)
     $                    *hfac1*hfac2*hfac3/ntree
                  beta30 = dist30 
     $                 -m_Beta00 *m_ySfac(jph1) *m_ySfac(jph2) *m_xSfac(jph3)
     $                 -betf10(jph1) *m_ySfac(jph2) *m_xSfac(jph3)
     $                 -betf10(jph2) *m_ySfac(jph1) *m_xSfac(jph3)
     $                 -beti10(jph3) *m_ySfac(jph1) *m_ySfac(jph2)
     $                 -betyy20(jph1,jph2) *m_xSfac(jph3)
     $                 -betxy20(jph3,jph1) *m_ySfac(jph2)
     $                 -betxy20(jph3,jph2) *m_ySfac(jph1)
                  m_xyyBet30 = m_xyyBet30 
     $                 +beta30/m_ySfac(jph1)/m_ySfac(jph2)/m_xSfac(jph3)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
*-----------------------------------------------------------------
*     Finite part of the YFS formfactor for the ISR/FSR
*-----------------------------------------------------------------
      CALL  BornV_GetYFSkon(ForIni)
      CALL KarFin_GetYFSkon(ForFin)
      IF(m_KeyISR .EQ. 0)   ForIni=1d0
      IF(   IsFSR .EQ. 0)   ForFin=1d0
      fYFS = ForIni*ForFin
*-----------------------------------------------------------------
*     and the rejection weights = (new.distr/crude.distr)
*-----------------------------------------------------------------
*     ============================================
*     ========== INITIAL + FINAL =================
*     ============================================
*
* Note that m_xyBet20 (which is genuine O(alf2)) is added to O(alf1),
* because our semianalytical programs are only able to deal 
* with factorized ini/fin, see also O(alf1) definitions of beta1's.
*
* Total's, all beta's ---------------------------------
      m_WtSet(71) =   fYFS*  m_Beta00/DisCru
      m_WtSet(72) =   fYFS*( m_Beta01 +m_xBet10 +m_yBet10 +m_xyBet20)/DisCru
      m_WtSet(73) =   fYFS*( m_Beta02 +m_xBet11 +m_yBet11 
     $                      +m_xxBet20 +m_xyBet20 +m_yyBet20 )/DisCru
* !!!NEW!!!
      m_WtSet(74) =   fYFS*( m_Beta03 +m_xBet12 +m_yBet12
     $                    +m_xxBet21 +m_xyBet21 +m_yyBet21
     $                    +m_xxxBet30 +m_xxyBet30 +m_xyyBet30 )/DisCru
* First order, individual beta's -------------
      m_WtSet(80) =   fYFS*m_Beta01/DisCru
      m_WtSet(81) =   fYFS*(m_xBet10+m_yBet10)/DisCru
      m_WtSet(82) =   fYFS*(m_xBet10)/DisCru
      m_WtSet(83) =   fYFS*(m_yBet10)/DisCru
      m_WtSet(84) =   fYFS*(m_xyBet20)/DisCru
* Second order, individual beta's ------------
      m_WtSet(90) =   fYFS*m_Beta02/DisCru
      m_WtSet(91) =   fYFS*(m_xBet11+m_yBet11)/DisCru
      m_WtSet(92) =   fYFS*(m_xxBet20+m_xyBet20+m_yyBet20)/DisCru
      m_WtSet(93) =   fYFS*(m_xBet11)/DisCru
      m_WtSet(94) =   fYFS*(m_yBet11)/DisCru
      m_WtSet(95) =   fYFS*(m_xxBet20)/DisCru
      m_WtSet(96) =   fYFS*(m_xyBet20)/DisCru
      m_WtSet(97) =   fYFS*(m_yyBet20)/DisCru
* Third order, individual beta's ------------!!!NEW!!!
      m_WtSet(100) =   fYFS*m_Beta03/DisCru
      m_WtSet(101) =   fYFS*(m_xBet12 +m_yBet12)/DisCru
      m_WtSet(102) =   fYFS*(m_xxBet21+m_xyBet21+m_yyBet21)/DisCru
      m_WtSet(103) =   fYFS*m_xBet12/DisCru
      m_WtSet(104) =   fYFS*m_yBet12/DisCru
      m_WtSet(105) =   fYFS*m_xxBet21/DisCru
      m_WtSet(106) =   fYFS*m_xyBet21/DisCru
      m_WtSet(107) =   fYFS*m_yyBet21/DisCru
      m_WtSet(108) =   fYFS*(m_xxxBet30+m_xxyBet30+m_xyyBet30)/DisCru
      m_WtSet(109) =   fYFS*m_xxxBet30/DisCru
      m_WtSet(110) =   fYFS*m_xxyBet30/DisCru
      m_WtSet(111) =   fYFS*m_xyyBet30/DisCru

*     ============================================
*     ========= INITIAL STATE ALONE ==============
*     ============================================
* Total's, all beta's ---------------------------------
      m_WtSet( 1) =   ForIni* m_beti00/DisCru
      m_WtSet( 2) =   ForIni*(m_beti01+m_sbti10)/DisCru
      m_WtSet( 3) =   ForIni*(m_beti02+m_sbti11+m_sbti20)/DisCru
!!!NEW
      m_WtSet( 4) =   ForIni*(m_beti03+m_sbti12+m_sbti21+m_sbti30)/DisCru
* First order, individual beta's -------------
      m_WtSet(10) =   ForIni*m_beti01/DisCru
      m_WtSet(11) =   ForIni*m_sbti10/DisCru
* Second order, individual beta's ------------
      m_WtSet(20) =   ForIni*m_beti02/DisCru
      m_WtSet(21) =   ForIni*m_sbti11/DisCru
      m_WtSet(22) =   ForIni*m_sbti20/DisCru
!!!NEW
* Third order, individual beta's ------------
      m_WtSet(30) =   ForIni*m_beti03/DisCru
      m_WtSet(31) =   ForIni*m_sbti12/DisCru
      m_WtSet(32) =   ForIni*m_sbti21/DisCru
      m_WtSet(33) =   ForIni*m_sbti30/DisCru
*
*//=================================================================//
*//            ISR   Non-exponentiated version                      //
*//            Not yet fully implemented......                      //
*//=================================================================//
* Entire 0,1,2-photon distributions
      fYFSu = exp( -gami*dlog(1/m_vvmin) )
      m_dis0   =0d0
      m_dis1   =0d0
      m_dis2   =0d0
      m_dig1   =0d0
      IF( (nphox+nphoy) .EQ. 0) THEN
         m_dis0 = 1
         m_dis1 = 1+gami*dlog(m_vvmin)
         m_dis2 = 1+gami*dlog(m_vvmin)+0.5*(gami*dlog(m_vvmin))**2
      ELSEIF( nphox .EQ. 1) THEN
         y = yini(1)
         z = zini(1)
         gf1 = 0.5d0
         gf2 = 0.5d0
         gi1 = ((1-y)**2)/2d0
         gi2 = ((1-z)**2)/2d0
         Bor1=  gi1*gf1*andi11   +gi1*gf2*andi12 
     $         +gi2*gf1*andi21   +gi2*gf2*andi22
         m_dis1 = Bor1  *wmd(delp,y,z)            !! S-factor divided off
* standard O(alf1) for comparisons with spinor methods
         m_dig1 = Bor1  *2d0/(y*z)*wm1(delp,y,z)*svar/svar1
         m_dis2 = m_dis1*(1 +gami*dlog(m_vvmin))
cc         m_dis1 = 1                       !!!! blank matrix elm.
cc         m_dis2 = 1 +gami*dlog(m_vvmin)   !!!! blank matrix elm.
      ELSEIF( nphoy .EQ. 1) THEN
         yy = yfin(1)
         zz = zfin(1)
         y  = yy/(1 +yy+zz)
         z  = zz/(1 +yy+zz)
         gi1 = 0.5d0
         gi2 = 0.5d0
         gf2 = ((1-y)**2 ) /2d0 !!! y,z are swapped! correct d_fsr !!!!
         gf1 = ((1-z)**2 ) /2d0
         Bor1=    gi1*gf1*andi11   +gi1*gf2*andi12 
     $           +gi2*gf1*andi21   +gi2*gf2*andi22
* standard O(alf1) for comparisons with spinor methods
         m_dig1 = Bor1  *2d0/(y*z)*wm1(delq,y,z)
         m_dis1 = Bor1  *wmd(delq,y,z)            !! S-factor divided off
c{{{
c         IF((y+z).GT.0.9d0) THEN
c            WRITE(*,'(a)') '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
c            DO k=1,4
c               ph(k) = yphot(1,k)
c            ENDDO
c            kq1 = 2*(ph(4)*qf1(4)-ph(3)*qf1(3)-ph(2)*qf1(2)-ph(1)*qf1(1))
c            kq2 = 2*(ph(4)*qf2(4)-ph(3)*qf2(3)-ph(2)*qf2(2)-ph(1)*qf2(1))
c            p1p2= 2*(pf1(4)*pf2(4)-pf1(3)*pf2(3)-pf1(2)*pf2(2)-pf1(1)*pf2(1))
c            q1q2= 2*(qf1(4)*qf2(4)-qf1(3)*qf2(3)-qf1(2)*qf2(2)-qf1(1)*qf2(1))
c            tt  = 2*(pf1(4)*qf1(4)-pf1(3)*qf1(3)-pf1(2)*qf1(2)-pf1(1)*qf1(1))
c            uu  = 2*(pf1(4)*qf2(4)-pf1(3)*qf2(3)-pf1(2)*qf2(2)-pf1(1)*qf2(1))
c            tt1 = 2*(pf2(4)*qf2(4)-pf2(3)*qf2(3)-pf2(2)*qf2(2)-pf2(1)*qf2(1))
c            uu1 = 2*(pf2(4)*qf1(4)-pf2(3)*qf1(3)-pf2(2)*qf1(2)-pf2(1)*qf1(1))
c            WRITE(*,'(a,5g20.10)') 'yy ',yy,kq1/q1q2
c            borc = (tt**2+uu**2+tt1**2+uu1**2)/p1p2**2
c            sofc = 2d0*p1p2**2/kq1/kq2
c            WRITE(*,'(a,5g20.10)') 'borc    ', borc,Bor1,borc/bor1
c            WRITE(*,'(a,5g20.10)') 'sofc    ', sofc, 2/(y*z)
c            dig1 =  2d0 *(tt**2+uu**2+tt1**2+uu1**2)/kq1/kq2
c            WRITE(*,'(a,5g20.10)')  'dig1  ', dig1, m_dig1, dig1/m_dig1
c         ENDIF
c}}}
      ELSEIF( nphox .EQ. 2) THEN
         m_dis2 = wm0(delp,yini(1),zini(1)) *wm0(delp,yini(2),zini(2))
cc         m_dis2 = 1             !!!! blank matrix elm.
      ENDIF
***
* UNEXP Total O(alf0),O(alf1),O(alf2)
      m_WtSet(160) =    m_dis0 /fYFSu
      m_WtSet(161) =    m_dis1 /fYFSu
      m_WtSet(162) =    m_dis2 /fYFSu

*|=================================================================|
*|        Model weight (the best)                                  |
*|=================================================================|
      m_WtBest = m_WtSet(m_IdeWgt)


*********[[[[[[[[[[[[*********DEBUG*********
c      wtm2= m_WtSet(73)*wtcrud
c      wtm1= m_WtSet(72)*wtcrud
c      wtm0= m_WtSet(71)*wtcrud
**      IF(wtm1  .LT.  0d0 .OR. wtm1  .GT.  5d0) THEN
c      IF(wtm2  .LT.  0d0 .OR. wtm2  .GT.  5d0) THEN
c         WRITE(6,*) '++++++++++++++++++++++++++++++++++++++++++++++++++'
c         WRITE(6,*) 'icont,kffin= ',icont,kffin
c         WRITE(6,*) 'nphox,nphoy,vv,uu=',nphox,nphoy,vv,uu
c         WRITE(6,*) 'wtm2,wtm1,wtm0= ',wtm2,wtm1,wtm0
c         WRITE(6,*) 'm_WtSet(71),wtcrud= ',m_WtSet(71),wtcrud
**         CALL dumps(6)
**         CALL dumpi(6)
**         CALL dumpf(6)
c      ENDIF
*********]]]]]]]]]]]]*********DEBUG*********
      END


      SUBROUTINE QED3_bvirt0(alfinv,charg2,svar,am,dels1,dels2,dels3)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// ISR/FSR virtual corrections to beta0                                            //
*// beta0 is equal born*(1+dels1+dels2+dels3)                                       //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION alfinv,charg2,svar,am,dels1,dels2,dels3
* locals
      DOUBLE PRECISION pi,zet2,zet3
      PARAMETER(pi=3.1415926535897932d0)
      PARAMETER(zet2= pi**2/6d0)
      PARAMETER(zet3= 1.2020569031595942854d0)
      DOUBLE PRECISION gami,bilg,alfpi,Mlog
*-------------------------------------------------------------------------------------
      DOUBLE COMPLEX     F1_1, F1_2, cL
*-------------------------------------------------------------------------------------
      alfpi =  1d0/alfinv/pi
      bilg  =  DLOG(svar/am**2)
      Mlog  =  DLOG(svar/am**2) -1d0
      gami  =  2*charg2*alfpi*(bilg-1)
      dels1 =  gami/2d0
**    dels2 =  1/2d0 *(gami/2d0)**2
* ISR with subleading terms from Berends, Burgers, Van Neerveen
* (the effect of including NLL is negligible, below 1d-4)
      dels2 =  
     $      charg2**2*alfpi**2  *0.5d0*bilg**2                 ! LL
**     $     +charg2**2*alfpi**2*(
**     $       -(13d0/16d0 +1.5d0*zet2 -3d0*zet3)*bilg           ! NLL
**     $       -16d0/5d0*zet2*zet2 +51d0/8d0*zet2 +13d0/4d0      ! NNLL
**     $       -4.5d0*zet3 -6d0*zet2*log(2d0)                    ! NNLL
**     $      )
      dels3 = 1/6d0 *(gami/2d0)**3
***/////////////////////////////////////////////////////////////////////////
*** The assignements below will get together O(alf1)CEEX and O(alf1)EEX
*** but it will spoil O(alf2)EEX because dels1 is also input for beta1
*      cL    = DCMPLX( DLOG(Svar/Am**2)-1d0, -1d0 )
*      F1_1  = Alfpi*charg2   *0.5d0*cL
*      dels1 = CDABS(1+ F1_1)**2 -1d0
*      F1_2 = F1_1
*     $     +(Alfpi*charg2)**2 *(
*     $              +cL**2/8d0 
*     $              +cL*( 3d0/32 -3d0/4*zet2 +3d0/2*zet3 ) 
*     $     )
*      dels2 = CDABS(1+ F1_2)**2 -(1d0+dels1)
***/////////////////////////////////////////////////////////////////////////
      END                       !!!QED3_bvirt0!!!

      SUBROUTINE QED3_Disr1(gami,y,z,j1,g1,g2,gg1,gg2,ggg1,ggg2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Ingredients for O(alf3)NLL ISR matrix element.                                  //
*// INPUT:                                                                          //
*//     alfinv=  QED coupling                                                       //
*//     charg2=  charge squared                                                     //
*//     y,z=     Sudakov variables                                                  //
*//     j1=      pointers to input-photon                                           //
*// OUTPUT:                                                                         //
*//     g's are set here: g=treelevel, gg=oneloop, ggg=twoloop                      //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  gami,g1,g2,gg1,gg2,ggg1,ggg2
      INTEGER           j1
      DOUBLE PRECISION  y(*),z(*)
* locals
      DOUBLE PRECISION  dels1,dels2,zz,a1,b1
*-------------------------------------------------------------------------------------
      a1 = y(j1)
      b1 = z(j1)
      zz = (1d0-a1)*(1d0-b1)
      IF(zz  .le.0d0) WRITE(*,*) '!!!! zz=',zz
      dels1 =  gami/2d0 -gami/4d0*dlog(zz)
      dels2 =  gami**2/8d0
     $        -gami**2/8d0  *dlog(zz)
     $        +gami**2/24d0 *dlog(zz)**2
* Exact O(alf1) matrix element for the hardest photon jhard
      g1   = ((1-a1)**2            )/2d0
      g2   = (            (1-b1)**2)/2d0
      gg1  = ((1-a1)**2            )/2d0 *(1+dels1)
      gg2  = (            (1-b1)**2)/2d0 *(1+dels1)
      ggg1 = ((1-a1)**2            )/2d0 *(1+dels1+dels2)
      ggg2 = (            (1-b1)**2)/2d0 *(1+dels1+dels2)
      END

      SUBROUTINE QED3_Disr2(gami,y,z,jhard,j1,j2,g1,g2,gg1,gg2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Ingredients for O(alf2)NLL ISR matrix element.                                  //
*// INPUT:                                                                          //
*//     gami  = 2*alfa/pi*(BigLog-1)                                                //
*//     y,z   = Sudakov variables                                                   //
*//     jhard = pointer of hardes photon                                            //
*//     j1,j2 = pointers of two input-photons                                       //
*// OUTPUT:                                                                         //
*//     g's, gg's are updated (have to be initialized in calling program)           //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  y(*),z(*)
      DOUBLE PRECISION  gami,g1,g2,gg1,gg2
      INTEGER           jhard,j1,j2
* locals
      DOUBLE PRECISION  a1,z1,p2,delvir1,z1z2,a2,b1,p1,b2
*-------------------------------------------------------------------------------------
      a1 = y(j1)
      b1 = z(j1)
      a2 = y(j2)/(1d0-y(j1))
      b2 = z(j2)/(1d0-z(j1))
* Exact O(alf1) matrix element for the hardest photon jhard
      IF(jhard .EQ. j1) THEN
         p1= ((1-a1)**2            ) *( (1-a2)**2 + (1-b2)**2 )/4d0
         p2= (            (1-b1)**2) *( (1-a2)**2 + (1-b2)**2 )/4d0
      ELSE
         p1= ((1-a1)**2 +(1-b1)**2 ) *( (1-a2)**2             )/4d0
         p2= ((1-a1)**2 +(1-b1)**2 ) *(             (1-b2)**2 )/4d0
      ENDIF
      g1 = g1 +p1
      g2 = g2 +p2
      z1 =  (1-y(j1))*(1-z(j1))
      z1z2= (1-y(j1)-y(j2))*(1-z(j1)-z(j2))
* soft limit to QED3_Disr1 OK, for 2 trees we get 3 terms gami/6d0*dlog(zz)
      delvir1 = gami/2d0 -gami/6d0*dlog(z1) -gami/6d0*dlog(z1z2)
      gg1=gg1 +p1*(1+delvir1)
      gg2=gg2 +p2*(1+delvir1)

      IF(z1  .le.0d0) WRITE(*,*) '!!!! z1=',z1
      IF(z1z2.le.0d0) WRITE(*,*) '!!!! z1z2=',z1z2
      END


      SUBROUTINE QED3_Disr3(y,z,jhard,j1,j2,j3,g1,g2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Ingredients for O(alf3)LL ISR matrix element.                                   //
*// INPUT:                                                                          //
*//     y,z Sudakov variables                                                       //
*//     jhard pointer of hardes photon                                              //
*//     j1,j2,j3 pointers of 3 input-photons                                        //
*// OUTPUT:                                                                         //
*//     g1,g2 are updated (have to be initialized in calling program)               //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION  y(*),z(*)
      DOUBLE PRECISION  g1,g2
      INTEGER           jhard,j1,j2,j3
* locals
      DOUBLE PRECISION  chi1,chi2,b3,a3,p2,p1,b1,a1,b2,a2,a,b
* inline functions
      chi1(a  )=  0.5d0*    (1d0-a)**2
      chi2(a,b)=  0.5d0*   ((1d0-a)**2+(1d0-b)**2)
*-------------------------------------------------------------------------------------
      a1 = y(j1)
      b1 = z(j1)
      a2 = y(j2)/(1d0-y(j1))
      b2 = z(j2)/(1d0-z(j1))
      a3 = y(j3)/(1d0-y(j2)-y(j1))
      b3 = z(j3)/(1d0-z(j2)-z(j1))
* Exact O(alf1) matrix element for the hardest photon jhard
      IF(jhard .EQ. j1) THEN
         p1= chi1(a1) *chi2(a2,b2) *chi2(a3,b3)
         p2= chi1(b1) *chi2(a2,b2) *chi2(a3,b3)
      ELSEIF(jhard .EQ. j2) THEN
         p1= chi2(a1,b1) *chi1(a2) *chi2(a3,b3)
         p2= chi2(a1,b1) *chi1(b2) *chi2(a3,b3)
      ELSE
         p1= chi2(a1,b1) *chi2(a2,b2) *chi1(a3)
         p2= chi2(a1,b1) *chi2(a2,b2) *chi1(b3)
      ENDIF
      g1=g1 +p1
      g2=g2 +p2
      END                       !!!QED3_Disr3!!!


      SUBROUTINE QED3_Dfsr1(gamf,y,z,j1,g1,g2,gg1,gg2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Ingredients for O(alf2)NLL FSR matrix element.                                  //
*// INPUT:                                                                          //
*//     y,z Sudakov variables                                                       //
*//     j1 pointer of input-photons                                                 //
*// OUTPUT:                                                                         //
*//     g's are set here                                                            //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER           j1
      DOUBLE PRECISION  gamf,g1,g2,gg1,gg2
      DOUBLE PRECISION  y(*),z(*)
* locals
      DOUBLE PRECISION  zz,dels1,a1,b1
*-------------------------------------------------------------------------------------
* normal definition as in O(alf1) single-photon case
      a1 = y(j1)/( 1d0 +y(j1) +z(j1) )
      b1 = z(j1)/( 1d0 +y(j1) +z(j1) )
      zz = (1d0-a1)*(1d0-b1)
      IF(zz  .LE. 0d0) WRITE(*,*) '!!!! zz=',zz
      dels1 =  gamf/2d0 +gamf/4d0*dlog(zz)
* Exact O(alf1) matrix element for the hardest photon jhard
      g2 = ((1-a1)**2            ) /2d0              ! corrected
      g1 = (            (1-b1)**2) /2d0              ! corrected
      gg2= ((1-a1)**2            ) /2d0 *(1+dels1)   ! corrected
      gg1= (            (1-b1)**2) /2d0 *(1+dels1)   ! corrected
      END                       !!!QED3_Dfsr1!!!

      SUBROUTINE QED3_Dfsr2(y,z,jhard,j1,j2,g1,g2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Ingredients for O(alf2)NLL FSR matrix element.                                  //
*// INPUT:                                                                          //
*//     y,z Sudakov variables                                                       //
*//     jhard pointer of hardes photon                                              //
*//     j1,j2 pointers of two input-photons                                         //
*// OUTPUT:                                                                         //
*//     g1,g2 are updated (have to be initialized in calling program)               //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER           jhard,j1,j2
      DOUBLE PRECISION  g1,g2
      DOUBLE PRECISION  y(*),z(*)
* locals
      DOUBLE PRECISION  b2,a2,p1,p2,b1,a1,zp2,yp2
*-------------------------------------------------------------------------------------
* normal definition as in O(alf1) single-photon case
      a1 = y(j1)/( 1d0 +y(j1) +z(j1) )
      b1 = z(j1)/( 1d0 +y(j1) +z(j1) )
* take into account primary photon emission
      yp2 = y(j2)/( 1d0 +y(j1) )
      zp2 = z(j2)/( 1d0 +z(j1) )
* as in O(alf1) single-photon case
      a2 = yp2/(1d0 + yp2 +zp2)
      b2 = zp2/(1d0 + yp2 +zp2)

* Exact O(alf1) matrix element for the hardest photon jhard
      IF(jhard .EQ. j1) THEN
         p2= ((1-a1)**2            ) *( (1-a2)**2 + (1-b2)**2 )/4d0 ! corrected
         p1= (            (1-b1)**2) *( (1-a2)**2 + (1-b2)**2 )/4d0 ! corrected
      ELSE
         p2= ((1-a1)**2 +(1-b1)**2 ) *( (1-a2)**2             )/4d0 ! corrected
         p1= ((1-a1)**2 +(1-b1)**2 ) *(             (1-b2)**2 )/4d0 ! corrected
      ENDIF
      g1=g1 +p1
      g2=g2 +p2
      END


      SUBROUTINE  QED3_wtPrint(txt,nout,iev,ie1,ie2,wt_ISR,wt_FSR,wtbest,wtset)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*// Prints out all weights                                                          //
*// and the serial number of event iev on unit nout                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER           nout,iev,ie1,ie2
      DOUBLE PRECISION  wt_ISR,wt_FSR,wtbest
      DOUBLE PRECISION  wtset(*)
      CHARACTER*8 txt
* locals
      INTEGER           i
      DOUBLE PRECISION  wtmain,wtcrud
*-------------------------------------------------------------------------------------
      IF( (iev .GE. ie1) .AND. (iev .LE. ie2) ) THEN
         WRITE(nout,*) 
     $        '=========== ',txt,' =======weights========>',iev
         wtcrud  = wt_ISR*wt_FSR
         wtmain  = wtcrud*wtbest
         WRITE(nout,3000) 'wtmain,wt_ISR,wt_FSR,wtbest= ',
     $                     wtmain,wt_ISR,wt_FSR,wtbest
         WRITE(nout,3100) (' (',i,')=',wtset(i)*wtcrud, i=1,150)
         WRITE(nout,*) '   '
      ENDIF
 3000 FORMAT(a,4f18.14)
 3100 FORMAT(4(a3,i3,a2,f18.14))
      END                       !!!QED3_wtPrint!!!

      SUBROUTINE QED3_GetWtSet(WtBest,WtSet)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Export list of weights                                                        //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'QED3.h'
      DOUBLE PRECISION    WtBest,WtSet(*)
* locals
      INTEGER  j
*--------------------------------------------------------------
      WtBest = m_WtBest
* collection of all weights
      DO j=1,m_lenwt
         WtSet(j)= m_WtSet(j)
      ENDDO
      END                       !!!QED3_GetWtSet!!!

      SUBROUTINE QED3_GetDig1(dig1)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Provides O(alf1) ISR matrix element for tests                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'QED3.h'
      DOUBLE PRECISION   dig1
*---------------------------------------------------------------
      dig1= m_dig1
      END                       !!!QED3_GetDig1!!!

*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//              End of Pseudo-CLASS  QED3                                          //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////


      SUBROUTINE fort_open(nout,fname)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Interface used by c++ programs                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT  NONE
      CHARACTER fname*(*)
      INTEGER   nout,nout2
*-------------------------------------------------------------------------------------
      nout2 = nout
      OPEN(nout2,file=fname)
**      WRITE(6,'(A,A20,A)')    '======>',filename,'<========='
**      WRITE(nout,'(A,A20,A)') '======>',filename,'<========='
      END

      SUBROUTINE fort_close(nout)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Interface used by c++ programs                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER  nout,nout2
*-------------------------------------------------------------------------------------
      nout2 = nout
      CLOSE(nout2)
      END


      SUBROUTINE ReaDataX(ninp,xpar,imax)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Single data card is:    (a1,i4,d15.0,a60)                                      //
*//  First character * defines comment card!                                        //
*//                                                                                 //
*//  Note that this program does not clear xpar!!!                                  //
*//  one has to do it before calling it, if necessary!!!                            //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      DOUBLE PRECISION xpar(*)
      INTEGER          ninp,imax
*-------------------------------------------------------------------------------------
      CHARACTER*6      beg6
      CHARACTER*4      end4
      CHARACTER*1      mark1
      CHARACTER*60     comm60
      CHARACTER*80     comm80
      INTEGER          line,index
      DOUBLE PRECISION value
*-------------------------------------------------------------------------------------

*      WRITE(  *,*) '***************************'
*      WRITE(  *,*) '*  Parser ReaDataX starts *'
*      WRITE(  *,*) '***************************'

* Search for 'BeginX'
      DO line =1,10000
         READ(ninp,'(a6,a)') beg6,comm60
         IF(beg6 .EQ. 'BeginX') THEN
            WRITE(  *,'(a6,a)') beg6,comm60
            GOTO 200
         ENDIF
      ENDDO
 200  CONTINUE

* Read data, 'EndX' terminates data, '*' marks comment
      DO line =1,1000
         READ(ninp,'(a)') mark1
         IF(mark1 .EQ. ' ') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a1,i4,d15.0,a60)') mark1,index,value,comm60
            WRITE(  *,'(a1,i4,g15.6,a60)') mark1,index,value,comm60
            IF( (index .LE. 0) .OR. (index .GE. imax)) GOTO 990
            xpar(index) = value
         ELSEIF(mark1 .EQ. 'E') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a4,a)') end4,comm60
            WRITE(  *,'(a4,a)') end4,comm60
            IF(end4 .EQ. 'EndX') GOTO 300
            GOTO 991
         ELSEIF(mark1 .EQ. '*') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a)') comm80
            WRITE(  *,'(a)') comm80
         ENDIF
      ENDDO
 300  CONTINUE

*      WRITE(  *,*) '***************************'
*      WRITE(  *,*) '* Parser ReaDataX ends    *'
*      WRITE(  *,*) '***************************'
      RETURN
 990  WRITE(*,*) '+++ ReaDataX: wrong index= ',index
      STOP
      RETURN
 991  WRITE(*,*) '+++ ReaDataX: wrong end of data '
      STOP
      END

      SUBROUTINE ReaDataN(ninp,npar,imax)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  Single data card is:    (a1,i4,i15,a60)                                        //
*//  First character * defines comment card!                                        //
*//                                                                                 //
*//  Note that this program does not clear xpar!!!                                  //
*//  one has to do it before calling it, if necessary!!!                            //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INTEGER          ninp,imax,npar(*)
*
      CHARACTER*6      beg6
      CHARACTER*4      end4
      CHARACTER*1      mark1
      CHARACTER*60     comm60
      CHARACTER*80     comm80   
      INTEGER          nvalue,index,line
*--------------------------------------------------------------------------------------
*      WRITE(  *,*) '***************************'
*      WRITE(  *,*) '* Parser  ReaDataN starts *'
*      WRITE(  *,*) '***************************'

* Search for 'BeginN'
      DO line =1,10000
         READ(ninp,'(a6,a)') beg6,comm60
         IF(beg6 .EQ. 'BeginN') THEN
            WRITE(  *,'(a6,a)') beg6,comm60
            GOTO 200
         ENDIF
      ENDDO
 200  CONTINUE

* Read data, 'EndN' terminates data, '*' marks comment
      DO line =1,1000
         READ(ninp,'(a)') mark1
         IF(mark1 .EQ. ' ') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a1,i4,i15,a60)') mark1,index,nvalue,comm60
            WRITE(  *,'(a1,i4,i15,a60)') mark1,index,nvalue,comm60
            IF( (index .LE. 0) .OR. (index .GE. imax)) GOTO 990
            npar(index) = nvalue
         ELSEIF(mark1 .EQ. 'E') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a4,a)') end4,comm60
            WRITE(  *,'(a4,a)') end4,comm60
            IF(end4 .EQ. 'EndN') GOTO 300
            GOTO 991
         ELSEIF(mark1 .EQ. '*') THEN
            BACKSPACE(ninp)
            READ(ninp,'(a)') comm80
            WRITE(  *,'(a)') comm80
         ENDIF
      ENDDO
 300  CONTINUE

*      WRITE(  *,*) '***************************'
*      WRITE(  *,*) '* Parser ReaDataN ends    *'
*      WRITE(  *,*) '***************************'
      RETURN
 990  WRITE(*,*) '+++ ReaDataN: wrong index= ',index
      STOP
      RETURN
 991  WRITE(*,*) '+++ ReaDataN: wrong end of data '
      STOP
      END

* 
* 
* 
* 
* 
* 
* 
* 
* 
* 

* 
* 
* 
* 
* 
* 
* 


      SUBROUTINE INIETC(ITAUXPAR,xpar)
      INCLUDE "BXformat.h"
      REAL*8 xpar(*)
      INTEGER   INUT,IOUT
      COMMON /INOUT/  
     $     INUT,         ! Input  unit  number (not used)
     $     IOUT          ! Ounput unit  number
      COMMON / IDFC  / IDFF
      COMMON / TAURAD / XK0DEC,ITDKRC
      DOUBLE PRECISION            XK0DEC
      COMMON / JAKI   /  JAK1,JAK2,JAKP,JAKM,KTOM
* Note: I dont see KeyA1=2,3 realy implemented in the code SJ. ??????
      INTEGER  KeyA1
      COMMON /TESTA1/
     $     KeyA1           ! Special switch for tests of dGamma/dQ**2 in a1 decay
* KeyA1=1 constant width of a1 and rho
* KeyA1=2 free choice of rho propagator (defined in function FPIK)
*         and free choice of a1 mass and width. function g(Q**2)
*         (see formula 3.48 in Comp. Phys. Comm. 64 (1991) 275)
*         hard coded both in Monte Carlo and in testing distribution.
* KeyA1=3 function g(Q**2) hardcoded in the Monte Carlo
*         (it is timy to calculate!), but appropriately adjusted in testing distribution.
      SAVE
       idff    = xpar(ITAUXPAR+3)        ! Lund identifier for first tau (15 for  tau-)
C XK0 for tau decays.
       xk0dec  = xpar(ITAUXPAR+5)        ! IR-cut for QED rad. in leptonic decays
C radiative correction switch in tau --> e (mu) decays !
       itdkRC  = xpar(ITAUXPAR+4)        ! QED rad. in leptonic decays
C switches of tau+ tau- decay modes !!
       Jak1            = xpar(ITAUXPAR+1)   ! Decay Mask for first tau
       Jak2            = xpar(ITAUXPAR+2)   ! Decay Mask for second tau
C output file number for TAUOLA
       IOUT    = xpar(4)
C  KeyA1 is used for formfactors actually not in use
      KeyA1   = xpar(ITAUXPAR+6)        ! Type of a1 current

        WRITE(iout,bxope)
        WRITE(iout,bxtxt) ' Parameters passed from KK  to Tauola:   '
        WRITE(iout,bxl1i) Jak1,      'dec. type 1-st tau  ','Jak1  ','t01'
        WRITE(iout,bxl1i) Jak2,      'dec. type 2-nd tau  ','Jak2  ','t02'
        WRITE(iout,bxl1i) KeyA1,     'current type a1 dec.','KeyA1 ','t03'
        WRITE(iout,bxl1i) idff,      'PDG id 1-st tau     ','idff  ','t04'
        WRITE(iout,bxl1i) itdkRC,    'R.c. switch lept dec','itdkRC','t05'
        WRITE(iout,bxl1g) xk0dec,    'IR-cut for lept r.c.','xk0dec','t06'
        WRITE(iout,bxclo)

      end

      SUBROUTINE INITDK(ITAUXPAR,xpar)
* ----------------------------------------------------------------------
*     INITIALISATION OF TAU DECAY PARAMETERS  and routines
*
*     called by : KORALZ
* ----------------------------------------------------------------------
      INCLUDE "BXformat.h"
      INTEGER   INUT,IOUT
      COMMON /INOUT/  
     $     INUT,         ! Input  unit  number (not used)
     $     IOUT          ! Ounput unit  number
      REAL*8 xpar(*)
      COMMON / DECPAR / GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      REAL*4            GFERMI,GV,GA,CCABIB,SCABIB,GAMEL
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS

      PARAMETER (NMODE=15,NM1=0,NM2=1,NM3=8,NM4=2,NM5=1,NM6=3)
      COMMON / DECOMP /IDFFIN(9,NMODE),MULPIK(NMODE)
     &                ,NAMES
      CHARACTER NAMES(NMODE)*31

      CHARACTER OLDNAMES(7)*31
      CHARACTER*80 bxINIT
      PARAMETER (
     $  bxINIT ='(1x,1h*,g17.8,            16x, a31,a4,a4, 1x,1h*)'
     $ )
      REAL*4 PI,POL1(4)
*
*
* LIST OF BRANCHING RATIOS
CAM normalised to e nu nutau channel
CAM                  enu   munu   pinu  rhonu   A1nu   Knu    K*nu   pi
CAM   DATA JLIST  /    1,     2,     3,     4,     5,     6,     7,

*AM   DATA GAMPRT /1.000,0.9730,0.6054,1.2432,0.8432,0.0432,O.O811,0.616
*AM
*AM  multipion decays
*
*    conventions of particles names
*                 K-,P-,K+,  K0,P-,KB,  K-,P0,K0
*                  3, 1,-3  , 4, 1,-4  , 3, 2, 4  ,
*                 P0,P0,K-,  K-,P-,P+,  P-,KB,P0
*                  2, 2, 3  , 3, 1,-1  , 1,-4, 2  ,
*                 ET,P-,P0   P-,P0,GM
*                  9, 1, 2  , 1, 2, 8
*

C
      DIMENSION NOPIK(6,NMODE),NPIK(NMODE)
*AM   outgoing multiplicity and flavors of multi-pion /multi-K modes    
      DATA   NPIK  /                4,                    4,  
     1                              5,                    5,
     2                              6,                    6,
     3                              3,                    3,            
     4                              3,                    3,            
     5                              3,                    3,            
     6                              3,                    3,  
     7                              2                         /         

      DATA  NOPIK / -1,-1, 1, 2, 0, 0,     2, 2, 2,-1, 0, 0,  
     1              -1,-1, 1, 2, 2, 0,    -1,-1,-1, 1, 1, 0,  
     2              -1,-1,-1, 1, 1, 2,    -1,-1, 1, 2, 2, 2, 
     3              -3,-1, 3, 0, 0, 0,    -4,-1, 4, 0, 0, 0,  
     4              -3, 2,-4, 0, 0, 0,     2, 2,-3, 0, 0, 0,  
     5              -3,-1, 1, 0, 0, 0,    -1, 4, 2, 0, 0, 0,  
     6               9,-1, 2, 0, 0, 0,    -1, 2, 8, 0, 0, 0,


     7              -3, 4, 0, 0, 0, 0                         /

* LIST OF BRANCHING RATIOS
      NCHAN = NMODE + 7
      DO 1 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I

        IF(I.EQ. 1) GAMPRT(I) = 1.0000
        IF(I.EQ. 2) GAMPRT(I) = 1.0000
        IF(I.EQ. 3) GAMPRT(I) = 1.0000
        IF(I.EQ. 4) GAMPRT(I) = 1.0000
        IF(I.EQ. 5) GAMPRT(I) = 1.0000
        IF(I.EQ. 6) GAMPRT(I) = 1.0000
        IF(I.EQ. 7) GAMPRT(I) = 1.0000
        IF(I.EQ. 8) GAMPRT(I) = 1.0000
        IF(I.EQ. 9) GAMPRT(I) = 1.0000
        IF(I.EQ.10) GAMPRT(I) = 1.0000
        IF(I.EQ.11) GAMPRT(I) = 1.0000
        IF(I.EQ.12) GAMPRT(I) = 1.0000
        IF(I.EQ.13) GAMPRT(I) = 1.0000
        IF(I.EQ.14) GAMPRT(I) = 1.0000
        IF(I.EQ.15) GAMPRT(I) = 1.0000
        IF(I.EQ.16) GAMPRT(I) = 1.0000
        IF(I.EQ.17) GAMPRT(I) = 1.0000
        IF(I.EQ.18) GAMPRT(I) = 1.0000
        IF(I.EQ.19) GAMPRT(I) = 1.0000
        IF(I.EQ.20) GAMPRT(I) = 1.0000
        IF(I.EQ.21) GAMPRT(I) = 1.0000
        IF(I.EQ.22) GAMPRT(I) = 1.0000

        IF(I.EQ. 1) OLDNAMES(I)='  TAU-  -->   E-               '
        IF(I.EQ. 2) OLDNAMES(I)='  TAU-  -->  MU-               '
        IF(I.EQ. 3) OLDNAMES(I)='  TAU-  -->  PI-               '
        IF(I.EQ. 4) OLDNAMES(I)='  TAU-  -->  PI-, PI0          '
        IF(I.EQ. 5) OLDNAMES(I)='  TAU-  -->  A1- (two subch)   '
        IF(I.EQ. 6) OLDNAMES(I)='  TAU-  -->   K-               '
        IF(I.EQ. 7) OLDNAMES(I)='  TAU-  -->  K*- (two subch)   '
        IF(I.EQ. 8) NAMES(I-7)='  TAU-  --> 2PI-,  PI0,  PI+   '
        IF(I.EQ. 9) NAMES(I-7)='  TAU-  --> 3PI0,        PI-   '
        IF(I.EQ.10) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 2PI0   '
        IF(I.EQ.11) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,        '
        IF(I.EQ.12) NAMES(I-7)='  TAU-  --> 3PI-, 2PI+,  PI0   '
        IF(I.EQ.13) NAMES(I-7)='  TAU-  --> 2PI-,  PI+, 3PI0   '
        IF(I.EQ.14) NAMES(I-7)='  TAU-  -->  K-, PI-,  K+      '
        IF(I.EQ.15) NAMES(I-7)='  TAU-  -->  K0, PI-, K0B      '

        IF(I.EQ.16) NAMES(I-7)='  TAU-  -->  K-,  K0, PI0      '

        IF(I.EQ.17) NAMES(I-7)='  TAU-  --> PI0  PI0   K-      '
        IF(I.EQ.18) NAMES(I-7)='  TAU-  -->  K-  PI-  PI+      '
        IF(I.EQ.19) NAMES(I-7)='  TAU-  --> PI-  K0B  PI0      '
        IF(I.EQ.20) NAMES(I-7)='  TAU-  --> ETA  PI-  PI0      '
        IF(I.EQ.21) NAMES(I-7)='  TAU-  --> PI-  PI0  GAM      '
        IF(I.EQ.22) NAMES(I-7)='  TAU-  -->  K-  K0            '
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
      DO I=1,NMODE
        MULPIK(I)=NPIK(I)
        DO J=1,MULPIK(I)
         IDFFIN(J,I)=NOPIK(J,I)
        ENDDO
      ENDDO
*
*
* --- COEFFICIENTS TO FIX RATIO OF:
* --- A1 3CHARGED/ A1 1CHARGED 2 NEUTRALS MATRIX ELEMENTS (MASLESS LIM.)
* --- PROBABILITY OF K0 TO BE KS
* --- PROBABILITY OF K0B TO BE KS
* --- RATIO OF COEFFICIENTS FOR K*--> K0 PI-
* --- ALL COEFFICENTS SHOULD BE IN THE RANGE (0.0,1.0)
* --- THEY MEANING IS PROBABILITY OF THE FIRST CHOICE ONLY IF ONE
* --- NEGLECTS MASS-PHASE SPACE EFFECTS
      BRA1=0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
*

      GFERMI = 1.16637E-5
      CCABIB = 0.975
      GV     = 1.0
      GA     =-1.0
      GFERMI = xpar(32)
      IF (XPAR(ITAUXPAR+100+1).GT.-1D0) THEN
C initialization form KK
        CCABIB = XPAR(ITAUXPAR+7)
        GV     = XPAR(ITAUXPAR+8)
        GA     = XPAR(ITAUXPAR+9)

        BRA1   = XPAR(ITAUXPAR+10)
        BRKS   = XPAR(ITAUXPAR+11)
        BRK0   = XPAR(ITAUXPAR+12)
        BRK0B  = XPAR(ITAUXPAR+13)
        DO K=1,NCHAN
         GAMPRT(K)=XPAR(ITAUXPAR+100+K)
        ENDDO
      ENDIF
* ZW 13.04.89 HERE WAS AN ERROR
      SCABIB = SQRT(1.-CCABIB**2)
      PI =4.*ATAN(1.)
      GAMEL  = GFERMI**2*AMTAU**5/(192*PI**3)
*
*      CALL DEXAY(-1,pol1)
*
* PRINTOUTS FOR KK version

      SUM=0
      DO K=1,NCHAN
       SUM=SUM+GAMPRT(K)
      ENDDO

      
      WRITE(iout,bxope)
      WRITE(iout,bxtxt) ' TAUOLA Initialization SUBROUTINE INITDK:    '
      WRITE(iout,bxtxt) ' Adopted to read from KK                     '
      WRITE(iout,bxtxt) '                      '
      WRITE(iout,bxtxt) ' Choice Probability      --     Decay Channel'
      DO K=1,7      
      WRITE(iout,bxINIT) GAMPRT(K)/SUM,    OLDNAMES(K),'****','***'
      ENDDO
      DO K=8,7+NMODE      
      WRITE(iout,bxINIT) GAMPRT(K)/SUM,     NAMES(K-7),'****','***'
      ENDDO
      WRITE(iout,bxtxt) ' In addition:'
      WRITE(iout,bxINIT) GV,    'Vector W-tau-nu coupl.     ','****','***'
      WRITE(iout,bxINIT) GA,    'Axial  W-tau-nu coupl.     ','****','***'
      WRITE(iout,bxINIT) GFERMI,'Fermi Coupling             ','****','***'
      WRITE(iout,bxINIT) CCABIB,'cabibo angle               ','****','***'
      WRITE(iout,bxINIT) BRA1,  'a1 br ratio (massless)     ','****','***'
      WRITE(iout,bxINIT) BRKS,  'K* br ratio (massless)     ','****','***'
      WRITE(iout,bxclo)
            
      RETURN
      END

      SUBROUTINE INIPHY(XK00)
* ----------------------------------------------------------------------
*     INITIALISATION OF PARAMETERS
*     USED IN QED and/or GSW ROUTINES
* ----------------------------------------------------------------------
      COMMON / QEDPRM /ALFINV,ALFPI,XK0
      REAL*8           ALFINV,ALFPI,XK0
      REAL*8 PI8,XK00
*
      PI8    = 4.D0*DATAN(1.D0)
      ALFINV = 137.03604D0
      ALFPI  = 1D0/(ALFINV*PI8)
      XK0=XK00
      END

      SUBROUTINE INIMAS(ITAUXPAR,xpar)
* ----------------------------------------------------------------------
*     INITIALISATION OF MASSES
*
*     called by : KORALZ
* ----------------------------------------------------------------------
      INCLUDE "BXformat.h"
      INTEGER   INUT,IOUT
      COMMON /INOUT/  
     $     INUT,         ! Input  unit  number (not used)
     $     IOUT          ! Ounput unit  number
      REAL*8 xpar(*)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      CHARACTER*80 bxINIT
      PARAMETER (
     $  bxINIT ='(1x,1h*,g17.8,            16x, a31,a4,a4, 1x,1h*)'
     $ )
*
* IN-COMING / OUT-GOING  FERMION MASSES
      AMTAU  = xpar(656)
      AMNUTA = 0.010
      AMEL   = xpar(616)
      AMNUE  = 0.0
      AMMU   = xpar(636)
      AMNUMU = 0.0
*
* MASSES USED IN TAU DECAYS

      AMPIZ  = 0.134964
      AMPI   = 0.139568
      AMRO   = 0.773
      GAMRO  = 0.145
*C    GAMRO  = 0.666
      AMA1   = 1.251
      GAMA1  = 0.599
      AMK    = 0.493667
      AMKZ   = 0.49772
      AMKST  = 0.8921
      GAMKST = 0.0513

      WRITE(iout,bxope)
      WRITE(iout,bxtxt) ' TAUOLA Initialization SUBROUTINE INIMAS:    '
      WRITE(iout,bxtxt) ' Adopted to read from KK                     '
      WRITE(iout,bxINIT) amtau, 'AMTAU tau-mass             ','****','***'
      WRITE(iout,bxINIT) amel , 'AMEL  electron-mass        ','****','***'
      WRITE(iout,bxINIT) ammu , 'AMMU  muon-mass            ','****','***'
      WRITE(iout,bxclo)

      END

      SUBROUTINE CHOICE(MNUM,RR,ICHAN,PROB1,PROB2,PROB3,
     $            AMRX,GAMRX,AMRA,GAMRA,AMRB,GAMRB)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      AMROP=1.1
      GAMROP=0.36
      AMOM=.782
      GAMOM=0.0084
*     XXXXA CORRESPOND TO S2 CHANNEL !
      IF(MNUM.EQ.0) THEN
         PROB1=0.5
         PROB2=0.5
         AMRX =AMA1
         GAMRX=GAMA1
         AMRA =AMRO
         GAMRA=GAMRO
         AMRB =AMRO
         GAMRB=GAMRO
      ELSEIF(MNUM.EQ.1) THEN
         PROB1=0.5
         PROB2=0.5
         AMRX =1.57
         GAMRX=0.9
         AMRB =AMKST
         GAMRB=GAMKST
         AMRA =AMRO
         GAMRA=GAMRO
      ELSEIF(MNUM.EQ.2) THEN
         PROB1=0.5
         PROB2=0.5
         AMRX =1.57
         GAMRX=0.9
         AMRB =AMKST
         GAMRB=GAMKST
         AMRA =AMRO
         GAMRA=GAMRO
      ELSEIF(MNUM.EQ.3) THEN
         PROB1=0.5
         PROB2=0.5
         AMRX =1.27
         GAMRX=0.3
         AMRA =AMKST
         GAMRA=GAMKST
         AMRB =AMKST
         GAMRB=GAMKST
      ELSEIF(MNUM.EQ.4) THEN
         PROB1=0.5
         PROB2=0.5
         AMRX =1.27
         GAMRX=0.3
         AMRA =AMKST
         GAMRA=GAMKST
         AMRB =AMKST
         GAMRB=GAMKST
      ELSEIF(MNUM.EQ.5) THEN
         PROB1=0.5
         PROB2=0.5
         AMRX =1.27
         GAMRX=0.3
         AMRA =AMKST
         GAMRA=GAMKST
         AMRB =AMRO
         GAMRB=GAMRO
      ELSEIF(MNUM.EQ.6) THEN
         PROB1=0.4
         PROB2=0.4
         AMRX =1.27
         GAMRX=0.3
         AMRA =AMRO
         GAMRA=GAMRO
         AMRB =AMKST
         GAMRB=GAMKST
      ELSEIF(MNUM.EQ.7) THEN
         PROB1=0.0
         PROB2=1.0
         AMRX =1.27
         GAMRX=0.9
         AMRA =AMRO
         GAMRA=GAMRO
         AMRB =AMRO
         GAMRB=GAMRO
      ELSEIF(MNUM.EQ.8) THEN
         PROB1=0.0
         PROB2=1.0
         AMRX =AMROP
         GAMRX=GAMROP
         AMRB =AMOM
         GAMRB=GAMOM
         AMRA =AMRO
         GAMRA=GAMRO
      ELSEIF(MNUM.EQ.101) THEN
         PROB1=.35
         PROB2=.35
         AMRX =1.2
         GAMRX=.46
         AMRB =AMOM
         GAMRB=GAMOM
         AMRA =AMOM
         GAMRA=GAMOM
      ELSEIF(MNUM.EQ.102) THEN
         PROB1=0.0
         PROB2=0.0
         AMRX =1.4
         GAMRX=.6
         AMRB =AMOM
         GAMRB=GAMOM
         AMRA =AMOM
         GAMRA=GAMOM
      ELSE
         PROB1=0.0
         PROB2=0.0
         AMRX =AMA1
         GAMRX=GAMA1
         AMRA =AMRO
         GAMRA=GAMRO
         AMRB =AMRO
         GAMRB=GAMRO
      ENDIF
*
      IF    (RR.LE.PROB1) THEN
         ICHAN=1
      ELSEIF(RR.LE.(PROB1+PROB2)) THEN
         ICHAN=2
         AX   =AMRA
         GX   =GAMRA
         AMRA =AMRB
         GAMRA=GAMRB
         AMRB =AX
         GAMRB=GX
         PX   =PROB1
         PROB1=PROB2
         PROB2=PX
      ELSE
         ICHAN=3
      ENDIF
*
      PROB3=1.0-PROB1-PROB2
      END

      FUNCTION DCDMAS(IDENT)
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
      IF      (IDENT.EQ. 1) THEN
        APKMAS=AMPI
      ELSEIF  (IDENT.EQ.-1) THEN
        APKMAS=AMPI
      ELSEIF  (IDENT.EQ. 2) THEN
        APKMAS=AMPIZ
      ELSEIF  (IDENT.EQ.-2) THEN
        APKMAS=AMPIZ
      ELSEIF  (IDENT.EQ. 3) THEN
        APKMAS=AMK
      ELSEIF  (IDENT.EQ.-3) THEN
        APKMAS=AMK
      ELSEIF  (IDENT.EQ. 4) THEN
        APKMAS=AMKZ
      ELSEIF  (IDENT.EQ.-4) THEN
        APKMAS=AMKZ
      ELSEIF  (IDENT.EQ. 8) THEN
        APKMAS=0.0001
      ELSEIF  (IDENT.EQ.-8) THEN
        APKMAS=0.0001
      ELSEIF  (IDENT.EQ. 9) THEN
        APKMAS=0.5488
      ELSEIF  (IDENT.EQ.-9) THEN
        APKMAS=0.5488
      ELSE
        PRINT *, 'STOP IN APKMAS, WRONG IDENT=',IDENT
        STOP
      ENDIF
      DCDMAS=APKMAS
      END
      FUNCTION LUNPIK(ID,ISGN)
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      REAL*4 XIO(1)
      IDENT=ID*ISGN

      IF      (IDENT.EQ. 1) THEN
        IPKDEF=-211
      ELSEIF  (IDENT.EQ.-1) THEN
        IPKDEF= 211
      ELSEIF  (IDENT.EQ. 2) THEN
        IPKDEF=111
      ELSEIF  (IDENT.EQ.-2) THEN
        IPKDEF=111
      ELSEIF  (IDENT.EQ. 3) THEN
        IPKDEF=-321
      ELSEIF  (IDENT.EQ.-3) THEN
        IPKDEF= 321

      ELSEIF  (IDENT.EQ. 4) THEN
*
* K0 --> K0_LONG (IS 130) / K0_SHORT (IS 310) = 1/1
        CALL RANMAR(XIO,1)
        IF (XIO(1).GT.BRK0) THEN
          IPKDEF= 130
        ELSE
          IPKDEF= 310
        ENDIF
      ELSEIF  (IDENT.EQ.-4) THEN
*
* K0B--> K0_LONG (IS 130) / K0_SHORT (IS 310) = 1/1
        CALL RANMAR(XIO,1)
        IF (XIO(1).GT.BRK0B) THEN
          IPKDEF= 130
        ELSE
          IPKDEF= 310
        ENDIF
      ELSEIF  (IDENT.EQ. 8) THEN
        IPKDEF= 22
      ELSEIF  (IDENT.EQ.-8) THEN
        IPKDEF= 22
      ELSEIF  (IDENT.EQ. 9) THEN
        IPKDEF= 221
      ELSEIF  (IDENT.EQ.-9) THEN
        IPKDEF= 221
      ELSE
        PRINT *, 'STOP IN IPKDEF, WRONG IDENT=',IDENT
        STOP
      ENDIF
      LUNPIK=IPKDEF
      END




      SUBROUTINE TAURDF(KTO)
* THIS ROUTINE CAN BE CALLED BEFORE ANY TAU+ OR TAU- EVENT IS GENERATED
* IT CAN BE USED TO GENERATE TAU+ AND TAU- SAMPLES OF DIFFERENT
* CONTENTS
      COMMON / TAUKLE / BRA1,BRK0,BRK0B,BRKS
      REAL*4            BRA1,BRK0,BRK0B,BRKS
      COMMON / TAUBRA / GAMPRT(30),JLIST(30),NCHAN
      IF (KTO.EQ.1) THEN
*     ==================
* LIST OF BRANCHING RATIOS
      NCHAN = 19
      DO 1 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) = .0000
        IF(I.EQ. 2) GAMPRT(I) = .0000
        IF(I.EQ. 3) GAMPRT(I) = .0000
        IF(I.EQ. 4) GAMPRT(I) = .0000
        IF(I.EQ. 5) GAMPRT(I) = .0000
        IF(I.EQ. 6) GAMPRT(I) = .0000
        IF(I.EQ. 7) GAMPRT(I) = .0000
        IF(I.EQ. 8) GAMPRT(I) = 1.0000
        IF(I.EQ. 9) GAMPRT(I) = 1.0000
        IF(I.EQ.10) GAMPRT(I) = 1.0000
        IF(I.EQ.11) GAMPRT(I) = 1.0000
        IF(I.EQ.12) GAMPRT(I) = 1.0000
        IF(I.EQ.13) GAMPRT(I) = 1.0000
        IF(I.EQ.14) GAMPRT(I) = 1.0000
        IF(I.EQ.15) GAMPRT(I) = 1.0000
        IF(I.EQ.16) GAMPRT(I) = 1.0000
        IF(I.EQ.17) GAMPRT(I) = 1.0000
        IF(I.EQ.18) GAMPRT(I) = 1.0000
        IF(I.EQ.19) GAMPRT(I) = 1.0000
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   1  CONTINUE
* --- COEFFICIENTS TO FIX RATIO OF:
* --- A1 3CHARGED/ A1 1CHARGED 2 NEUTRALS MATRIX ELEMENTS (MASLESS LIM.)
* --- PROBABILITY OF K0 TO BE KS
* --- PROBABILITY OF K0B TO BE KS
* --- RATIO OF COEFFICIENTS FOR K*--> K0 PI-
* --- ALL COEFFICENTS SHOULD BE IN THE RANGE (0.0,1.0)
* --- THEY MEANING IS PROBABILITY OF THE FIRST CHOICE ONLY IF ONE
* --- NEGLECTS MASS-PHASE SPACE EFFECTS
      BRA1=0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
      ELSE
*     ====
* LIST OF BRANCHING RATIOS
      NCHAN = 19
      DO 2 I = 1,30
      IF (I.LE.NCHAN) THEN
        JLIST(I) = I
        IF(I.EQ. 1) GAMPRT(I) = .0000
        IF(I.EQ. 2) GAMPRT(I) = .0000
        IF(I.EQ. 3) GAMPRT(I) = .0000
        IF(I.EQ. 4) GAMPRT(I) = .0000
        IF(I.EQ. 5) GAMPRT(I) = .0000
        IF(I.EQ. 6) GAMPRT(I) = .0000
        IF(I.EQ. 7) GAMPRT(I) = .0000
        IF(I.EQ. 8) GAMPRT(I) = 1.0000
        IF(I.EQ. 9) GAMPRT(I) = 1.0000
        IF(I.EQ.10) GAMPRT(I) = 1.0000
        IF(I.EQ.11) GAMPRT(I) = 1.0000
        IF(I.EQ.12) GAMPRT(I) = 1.0000
        IF(I.EQ.13) GAMPRT(I) = 1.0000
        IF(I.EQ.14) GAMPRT(I) = 1.0000
        IF(I.EQ.15) GAMPRT(I) = 1.0000
        IF(I.EQ.16) GAMPRT(I) = 1.0000
        IF(I.EQ.17) GAMPRT(I) = 1.0000
        IF(I.EQ.18) GAMPRT(I) = 1.0000
        IF(I.EQ.19) GAMPRT(I) = 1.0000
      ELSE
        JLIST(I) = 0
        GAMPRT(I) = 0.
      ENDIF
   2  CONTINUE
* --- COEFFICIENTS TO FIX RATIO OF:
* --- A1 3CHARGED/ A1 1CHARGED 2 NEUTRALS MATRIX ELEMENTS (MASLESS LIM.)
* --- PROBABILITY OF K0 TO BE KS
* --- PROBABILITY OF K0B TO BE KS
* --- RATIO OF COEFFICIENTS FOR K*--> K0 PI-
* --- ALL COEFFICENTS SHOULD BE IN THE RANGE (0.0,1.0)
* --- THEY MEANING IS PROBABILITY OF THE FIRST CHOICE ONLY IF ONE
* --- NEGLECTS MASS-PHASE SPACE EFFECTS
      BRA1=0.5
      BRK0=0.5
      BRK0B=0.5
      BRKS=0.6667
      ENDIF
*     =====
      END
* 
* 
* 
* 
* 
* 
* 




*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                                                                                 //
*//                         Pseudo-CLASS  TauPair                                   //
*//                                                                                 //
*//       Purpose:                                                                  //
*//       (a) Interface to Simulation of TWO tau decays                             //
*//       (b) Calculates spin weight wt using GPS_MakeRho2 and introduces           //
*//           spin effects in tau decays by means of rejection with <wt>=1          //
*//       (c) Transforms decay products to CMS frame                                //
*//       (d) Interfaces Photos to Tauola                                           //
*//                                                                                 //
*//   Notes:                                                                        //
*//   The class is initialized by KK2f_Initialize                                   //
*//   It is called from KK2f_Make                                                   //
*//   It needs GPS to be initialized in order to calculate spin weight (final)      //
*//                                                                                 //
*//                                                                                 //
*//   For the moment this file contains the interface to tauola                     //
*//   The rest of code is in tauface.f and tauola.f                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE Taupair_Initialize(xpar)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    Class initialization                                                         //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INCLUDE 'BXformat.h'
      INCLUDE 'Taupair.h'
*
      DOUBLE PRECISION  xpar(*)
      DOUBLE PRECISION  HvecDummy(4)
      DOUBLE PRECISION  xk0qed
      INTEGER Jak1,Jak2
      INTEGER ITAUXPAR
      PARAMETER ( ITAUXPAR=2000)
*=====================================================================================

      m_out   = xpar(4)
      m_idyfs = xpar(8)   ! pointer for internal histograming

      m_IsInitialized = xpar(415)  ! General mask for tau chanel

C switches of tau+ tau- decay modes !!
      m_IFPHOT        = xpar(ITAUXPAR+4)   ! QED rad. in hadronic decays (PHOTOS)
      Jak1            = xpar(ITAUXPAR+1)   ! Decay Mask for first tau
      Jak2            = xpar(ITAUXPAR+2)   ! Decay Mask for second tau
      IF( (Jak1.EQ.-1) .AND. (Jak2.EQ.-1) ) m_IsInitialized = 0

        m_KeyClone      = 1       ! dip-switch for cloning procedure, =1,2
        m_KeyClone      = 2       ! dip-switch for cloning procedure, =1,2
        WRITE(m_out,bxope)
        WRITE(m_out,bxtxt) ' KK interface of Tauola                  '
        WRITE(m_out,bxl1i) m_KeyClone,'Cloning procedure   ','KeyClone','t01'
        WRITE(m_out,bxclo)

* Initialisation of tau decay package TAUOLA; ITAUXPAR is for indirect adressing.
      CALL INIETC(ITAUXPAR,xpar)
      IF( m_IsInitialized .EQ. 0) THEN
         WRITE(m_out,bxope) 
         WRITE(m_out,bxtxt) ' !!!!! Tauola inhibited !!!!    '
         WRITE(m_out,bxclo)
      ELSE
        CALL INIMAS(ITAUXPAR,xpar)
        CALL INITDK(ITAUXPAR,xpar)
        xk0qed = 0.1D0            ! <=== It seems to be never used
        CALL INIPHY(xk0qed)
        CALL DEKAY(-1,HvecDummy)

* Initialization of PHOTOS
        IF(m_IFPHOT .EQ. 1)   CALL PHOINI

* control weight
        CALL GLK_Mbook(m_idyfs+30,'Tau Pair: wt1, Spin Imprint weight $', 40, 4d0)
      ENDIF
      END

      SUBROUTINE Taupair_Finalize
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    Printout of final statistics                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
*
      INCLUDE 'BXformat.h'
      INCLUDE 'Taupair.h'
*
      DOUBLE PRECISION  HvecDummy(4)
      DOUBLE PRECISION   awt30,dwt30,WtSup30
*-------------------------------------------------------------------------------------
      IF( m_IsInitialized .EQ. 0) RETURN

      CALL DEKAY(100,HvecDummy)

      CALL  GLK_MgetAve(m_idyfs+30,awt30,dwt30,WtSup30)

      CALL  GLK_Mprint(m_idyfs+30)

      WRITE(m_out,bxope)
      WRITE(m_out,bxtxt) 'Tau Pair Finalize         '
      WRITE(m_out,bxl2f) awt30,dwt30,'Spin Imprint <wt1> ','wt1ave','a1'
      WRITE(m_out,bxl1f) WtSup30    ,'Maximum value wt1  ','wt1max','a2'
      WRITE(m_out,bxclo)

      END


      SUBROUTINE Taupair_Make1
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Main step in tau decau generation                                             //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
*-------------------------------------------------------------------------------------
      IF( m_IsInitialized .EQ. 0) RETURN
      CALL DEKAY(1,m_HvecTau1)
      CALL DEKAY(2,m_HvecTau2)
      END

      SUBROUTINE Taupair_ImprintSpin
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Impose spin effects in tau decays with rejection method.                      //
*//   In order to save CPU time each tau decay is cloned by Euler rotation.         //
*//   This is safe and mathematicaly correct!                                       //
*//   Remember:  the average weight due to introduction of spin effects             //
*//   in tau+ tau- system has be EXACTLY 1/4.                                       //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
*
      DOUBLE PRECISION       wt, wtmax, wt0,wt1,wt2
      DOUBLE PRECISION       HvecFer1(4), HvecFer2(4)
      INTEGER     loop
      REAL                   rvec(10)
*-------------------------------------------------------------------------------------
      INTEGER icont
      DATA icont /0/
*-------------------------------------------------------------------------------------
      IF( m_IsInitialized .EQ. 0) RETURN
      icont = icont+1
      loop=0
 1099 CONTINUE
         loop=loop+1
* Cloning tau decay by Euler rotation
         CALL   Taupair_Clone
         CALL   Taupair_GetHvectors( HvecFer1,HvecFer2)
         CALL      GPS_SetHvectors( HvecFer1,HvecFer2)
*
         CALL GPS_MakeRho2(wt0,wt1,wt2)
*        --------------------------
         wt = wt1
*****    IF(icont. LE. 10 ) WRITE(16,*) ' LOOP ,wt = ', loop, wt
         wtmax = 4d0
         CALL GLK_Mfill(m_idyfs+30, wt,  wtmax)
         CALL PseuMar_MakeVec(rvec,1)
      IF (wt .LT. wtmax*rvec(1)) GOTO 1099
      END
 
      SUBROUTINE Taupair_Make2
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//    Transform tau decay products to CMS, PDG book-keeping and Photos             //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
      DOUBLE PRECISION     p3(4),p4(4)
      INTEGER ih1,ih2

      CALL KarFin_GetFermions( p3,p4)
      CALL GPS_tralorPrepare(p3,1)
      CALL GPS_tralorPrepare(p4,2)

      CALL HepEvt_GetF(   ih1)  ! fermion is here &&&
      CALL HepEvt_GetFbar(ih2)  ! antifermion is here
      CALL Taupair_SetFermPos(ih1,ih2)  ! set ffbar positions in Tauola &&&

      CALL DEKAY(1+10,m_HvecTau1)
      CALL DEKAY(2+10,m_HvecTau2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                    Photos comes last                                            //
*/////////////////////////////////////////////////////////////////////////////////////
      IF(m_IFPHOT .EQ. 1) THEN
         CALL PHOTOS(ih1)
         CALL PHOTOS(ih2)
      ENDIF
      CALL pyhepc(2)
      END

      SUBROUTINE Taupair_Clone
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   This routine is strongly interrelated with Tralor  !!!                        //
*//                                                                                 //
*//   Cloning tau decays by additional rotation tau decay products with respect     //
*//   to frames  initialy used in the decay simulation.                             //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
*
      DOUBLE PRECISION       KinLib_AngPhi
      DOUBLE PRECISION       Habs1,Habs2
      DOUBLE PRECISION       hb1(4),hb2(4)
      REAL                   rrr(10)
*-------------------------------------------------------------------------------------
      INTEGER icont
      DATA icont /0/
*-------------------------------------------------------------------------------------
*/////////////////////////////////////////////////////////////////////////////////////
*//   Generation of random two independent Euler rotations                          //
*/////////////////////////////////////////////////////////////////////////////////////
      CALL PseuMar_MakeVec(rrr,3)
      m_alfa1  = 2d0*m_pi*rrr(3)        ! azimuthal angle in (0,2*pi)
      m_beta1  = ACOS(2d0*rrr(1)-1d0)   ! polar angle     in (0,  pi)
      m_gamma1 = 2d0*m_pi*rrr(2)        ! azimuthal angle in (0,2*pi)
*------------------------------------------------
      CALL PseuMar_MakeVec(rrr,3)
      m_alfa2  = 2d0*m_pi*rrr(3)        ! azimuthal angle in (0,2*pi)
      m_beta2  = ACOS(2d0*rrr(1)-1d0)   ! polar angle     in (0,  pi)
      m_gamma2 = 2d0*m_pi*rrr(2)        ! azimuthal angle in (0,2*pi)
      IF(m_KeyClone .EQ. 1) THEN
*/////////////////////////////////////////////////////////////////////////////////////
*//   Cloning tau decay with help of  Euler rotations FIRST method                  //
*/////////////////////////////////////////////////////////////////////////////////////
         Habs1 = DSQRT( m_HvecTau1(1)**2 +m_HvecTau1(2)**2 +m_HvecTau1(3)**2 )
         Habs2 = DSQRT( m_HvecTau2(1)**2 +m_HvecTau2(2)**2 +m_HvecTau2(3)**2 )
* Standart phi, theta for polarimeter fectors, phi in (0,2*pi), theta in (0,pi)
         IF(Habs1 .GT. 1d-5) THEN
            m_phi1  = KinLib_AngPhi( m_HvecTau1(1), m_HvecTau1(2) )
            m_thet1 = KinLib_AngPhi( m_HvecTau1(3), DSQRT(m_HvecTau1(1)**2+m_HvecTau1(2)**2) )
         ELSE
            m_phi1  =0d0
            m_thet1 =0d0
         ENDIF
         IF(Habs2 .GT. 1d-5) THEN
            m_phi2  = KinLib_AngPhi( m_HvecTau2(1), m_HvecTau2(2) )
            m_thet2 = KinLib_AngPhi( m_HvecTau2(3), DSQRT(m_HvecTau2(1)**2+m_HvecTau2(2)**2) )
         ELSE
            m_phi2  =0d0
            m_thet2 =0d0
         ENDIF
*(((((((((((((
* Test of angle definition, resulting hb1,hb2 should be on z-axis
*         IF(icont .LE. 60 ) THEN
*            icont=icont+1
*            CALL  KinLib_RotEulInv( m_thet1, m_phi1, m_HvecTau1, hb1)
*            CALL  KinLib_RotEulInv( m_thet2, m_phi2, m_HvecTau2, hb2)
*            CALL KinLib_VecPrint(6,'hb1=    ',hb1)
*            CALL KinLib_VecPrint(6,'hb2=    ',hb2)
*         ENDIF
*)))))))))))))
         m_HvClone1(1) =0d0
         m_HvClone1(2) =0d0
         m_HvClone1(3) =Habs1
         m_HvClone1(4) =1d0
         m_HvClone2(1) =0d0
         m_HvClone2(2) =0d0
         m_HvClone2(3) =Habs2
         m_HvClone2(4) =1d0
         CALL  KinLib_RotEul( m_beta1, m_gamma1, m_HvClone1, m_HvClone1)
         CALL  KinLib_RotEul( m_beta2, m_gamma2, m_HvClone2, m_HvClone2)
      ELSEIF(m_KeyClone .EQ. 2) THEN
*/////////////////////////////////////////////////////////////////////////////////////
*//   Cloning tau decay with help of  Euler rotations, SECOND method                //
*/////////////////////////////////////////////////////////////////////////////////////
         CALL KinLib_RotEuler(m_alfa1, m_beta1, m_gamma1, m_HvecTau1, m_HvClone1)
         CALL KinLib_RotEuler(m_alfa2, m_beta2, m_gamma2, m_HvecTau2, m_HvClone2)
      ELSE
         WRITE(m_out,*) ' ##### STOP in Taupair_Clone: wrong KeyClone= ',m_KeyClone
         WRITE(    *,*) ' ##### STOP in Taupair_Clone: wrong KeyClone= ',m_KeyClone
         STOP
      ENDIF
      END

      SUBROUTINE Tralo4(Kto,P,Q,AM)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   This routine is strongly interrelated with Taupair_Clone!!!                    //
*//                                                                                 //
*//  SUBSITUTE OF TRALO4                                                            // 
*//  TRALO4 is called in TAUOLA /hepevt/ interface to boost from tau+-              //
*//  restframe to lab. It includes rotations in tau restframe due to spin effect    //
*//  implementation                                                                 //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
*
      INTEGER   Kto
      REAL                 P(4),Q(4),AM
* locals
      DOUBLE PRECISION     Pd(4)
      INTEGER   k
* ------------------------------------------------------------
      AM = SQRT(ABS( P(4)**2 -P(3)**2 -P(2)**2 -P(1)**2 ))
* Translation from REAL              to DOUBLE PRECISION 
      DO k=1,4
         Pd(K)=P(K)
      ENDDO
      IF(m_KeyClone .EQ. 1) THEN
         IF(   Kto .EQ. 1) THEN
            CALL  KinLib_RotEulInv( m_thet1, m_phi1,   Pd,Pd)
            CALL  KinLib_RotEul(    m_beta1, m_gamma1, Pd,Pd)
         ELSEIF( Kto .EQ. 2) THEN
            CALL  KinLib_RotEulInv( m_thet2, m_phi2,   Pd,Pd)
            CALL  KinLib_RotEul(    m_beta2, m_gamma2, Pd,Pd)
         ELSE
            GOTO 900
         ENDIF
      ELSEIF(m_KeyClone .EQ. 2) THEN
         IF(     Kto .EQ. 1) THEN
            CALL KinLib_RotEuler( m_alfa1, m_beta1, m_gamma1, Pd,Pd)
         ELSEIF( Kto .EQ. 2) THEN
            CALL KinLib_RotEuler( m_alfa2, m_beta2, m_gamma2, Pd,Pd)
         ELSE
            GOTO 900
         ENDIF
      ELSE
         GOTO 901
      ENDIF
      CALL  GPS_TralorDoIt(KTO,Pd,Pd)
* Translation from DOUBLE PRECISION  to REAL             
      DO k=1,4
         Q(K)=Pd(K)
      ENDDO
      RETURN
*----------------------------------------------
 900  CONTINUE
      WRITE(*,*) ' ###### STOP in TRALO4: Wrong Kto = ',Kto
      WRITE(*,*) ' ###### STOP in TRALO4: Wrong Kto = ',Kto
      STOP
 901  CONTINUE
      WRITE(m_out,*) ' ##### STOP in Taupair_Tralo4: wrong KeyClone= ',m_KeyClone
      WRITE(    *,*) ' ##### STOP in Taupair_Tralo4: wrong KeyClone= ',m_KeyClone
      STOP
      END


      SUBROUTINE FILHEP(N,IST,ID,JMO1,JMO2,JDA1,JDA2,P4,PINV,PHFLAG)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//  FILHEP of TAUOLA must be in single precission but double precision             // 
*//  HepEvt_Fil1 is its functional copy                                             //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      LOGICAL PHFLAG
      DOUBLE PRECISION  PINVD,P4D(4)
      REAL P4(4)
      DO k=1,4
        P4D(k)=P4(k)
      ENDDO
      PINVD=PINV
      CALL HepEvt_Fil1(N,IST,ID,JMO1,JMO2,JDA1,JDA2,P4D,PINVD,PHFLAG)
      END

*/////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//           Setters and Getters of CLASS  Tauola                                  //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
*/////////////////////////////////////////////////////////////////////////////////////

      SUBROUTINE Taupair_SetKeyClone(KeyClone)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Setter to KeyClone                                                            //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
      INTEGER KeyClone
*
      m_KeyClone = KeyClone
      IF( KeyClone .LT. 1 .OR. KeyClone .GT. 2) THEN
         WRITE(m_out,*) ' ##### STOP in Taupair_SetKeyClone: wrong KeyClone= ',KeyClone
         WRITE(    *,*) ' ##### STOP in Taupair_SetKeyClone: wrong KeyClone= ',KeyClone
         STOP
      ENDIF
      END

      SUBROUTINE Taupair_GetIsInitialized(IsInitialized)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Get to know if Tauola is active (IsInitialized=1)                             //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
      INTEGER IsInitialized
*
      IsInitialized = m_IsInitialized
      END

      SUBROUTINE Taupair_GetHvectors(HvecFer1,HvecFer2)
*/////////////////////////////////////////////////////////////////////////////////////
*//   Geting clone of the tau lepton POLARIMETER vectors                            //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
      INTEGER k
      DOUBLE PRECISION  HvecFer1(4),HvecFer2(4)
*
      DO k=1,4
         HvecFer1( k) = m_HvClone1(k)
         HvecFer2( k) = m_HvClone2(k)
      ENDDO
      END

      SUBROUTINE Taupair_SetFermPos(np1,np2)
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//   Get to know if Tauola is active (IsInitialized=1)                             //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'Taupair.h'
      INTEGER np1,np2
*
      m_np1 = np1
      m_np2 = np2
      END
*/////////////////////////////////////////////////////////////////////////////////////
*//                                                                                 //
*//                End of   Pseudo-CLASS  Taoula                                    //
*//                                                                                 //
*/////////////////////////////////////////////////////////////////////////////////////
 
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                     Pseudo-CLASS  YFSini2                                //
*//                                                                          //
*//                  !!!!! SPECIAL TESTS !!!!!!                              //
*//                                                                          //
*//   Purpose:  ISR photon emission, photon multiplicity and momenta         //
*//   ====================================================================== //
*//   Simplistic ISR generation without modulation of s', for SPECIAL TESTS. //
*//   (!!!!No dilatation !!!!)                                               //
*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//  The algorithm in this subprogram was ALSO described in:                 //
*//  ``Yennie-Frautschi-Suura soft photons in Monte Carlo event generators'' //
*//             Unpublished report by S. Jadach,                             //
*//          MPI-Munchen, MPI-PAE/PTh 6/87, Jan. 1987.                       //
*//                                                                          //
*//////////////////////////////////////////////////////////////////////////////


      SUBROUTINE YFSini2_Initialize( amel,alfinv,vvmin,nmax,out,KeyWtm,MltISR)
*
      INCLUDE 'YFSini2.h'
      SAVE
*
      DOUBLE PRECISION    amel,alfinv,vvmin
      INTEGER  nmax,out,KeyWtm,MltISR
*
      m_amel    = amel
      m_alfinv  = alfinv
      m_vvmin   = vvmin
      m_nmax    = nmax
      m_out     = out
      m_KeyWtm  = KeyWtm
      m_MltISR  = MltISR
      END


      SUBROUTINE YFSini2_Make(CMSene,                   ! Input
     $     vv,p1,p2,nphot,sphot,sphum,ygr,zet,qq,wtini) ! Output
*     *************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION  pi
      PARAMETER( pi=3.1415926535897932d0)
      DOUBLE PRECISION  CMSene,vv,sphum(4),sphot(100,4),ygr(100),zet(100)
*
      INCLUDE 'KarLud.h'
*
      DOUBLE PRECISION  xphot(100,4)    ! photon momenta before rescaling
      DOUBLE PRECISION  pp(4),pk(4)
      DOUBLE PRECISION  p1(4),p2(4),qq(4),xph(100)
      DOUBLE PRECISION  rrdummy(100)
      REAL              rr(100)
      SAVE
*
      DOUBLE PRECISION   eta1,eta2,beta
      DOUBLE PRECISION   dist0,dist1,cg,sg,sprim,gami,gami2
      DOUBLE PRECISION   wtm,xk,vmax,am2,ene,average
      DOUBLE PRECISION   phi,wt_cut,wtini,wt_mas
      DOUBLE PRECISION   alf1,del1,del2
      REAL               rvec(10)
      INTEGER i,j,k,nphot
*
      alf1 = 1d0/pi/m_alfinv
      ene  = CMSene/2d0
* Define 4-momenta of the initial charged particles (emitters)
      CALL KinLib_givpair(CMSene,m_amel,m_amel,p1,p2,beta,eta1,eta2)
* Here gami2 must be used instead of gami (mass term neglected)
      am2  = (m_amel/ene)**2
      beta = sqrt(1d0-am2)
      gami  = 2d0*alf1* (dlog((1+beta)**2/am2) - 1d0)
      gami2 = 2d0*alf1*  dlog((1+beta)**2/am2)
      IF(m_KeyWtm .EQ. 1) gami2 = gami
      wt_mas  = (1d0/m_vvmin)**(gami2-gami)
      DO i=1,m_nmax
         xph(i)=0d0
         ygr(i)=0d0
         zet(i)=0d0
         DO j=1,4
            xphot(i,j)=0d0
            sphot(i,j)=0d0
         ENDDO
      ENDDO
* generate photon multiplicity
      average = gami2*dlog(1d0/m_vvmin)
      CALL YFSini2_PoissGen(average,m_nmax,nphot,rrdummy)

* refill rr in order to avoid ordering
      IF(nphot .GT. 0) CALL PseuMar_MakeVec(rr,nphot)

      wt_cut  = 1d0
      DO i=1,nphot
         xph(i)=(m_vvmin)**rr(i)
      ENDDO
      DO i=1,nphot
         xk=xph(i)
*****    CALL YFSini2_angbre(am2,del1,del2,cg,sg,dist0,dist1)    ! standard
         CALL YFSini2_angbre2(i,am2,del1,del2,cg,sg,dist0,dist1) ! test
         wtm=dist1/dist0
         wt_mas    =wt_mas*wtm
         CALL PseuMar_MakeVec(rvec,1)
         phi=2d0*pi*rvec(1)
         xphot(i,1)=xk*sg*cos(phi)
         xphot(i,2)=xk*sg*sin(phi)
         xphot(i,3)=xk*cg
         xphot(i,4)=xk
         ygr(i)    =xk*del1/2d0
         zet(i)    =xk*del2/2d0
      ENDDO
*
* photon momenta rescaled into GEV units
      DO j=1,4
         sphum(j)=0d0
      ENDDO
      DO  i=1,nphot
         DO  j=1,4
            sphot(i,j)=xphot(i,j)*ene
            sphum(j)=sphum(j)+sphot(i,j)
         ENDDO
      ENDDO

* 4-momentum left after photon emission
      DO k=1,4
         qq(k)=-sphum(k)
      ENDDO
      qq(4)=qq(4)+CMSene
      sprim = qq(4)**2 -qq(3)**2 -qq(2)**2 -qq(1)**2
      vv = 1d0 -sprim/CMSene**2
      IF( qq(4) .LE. 0d0) wt_cut  = 0d0  !!!! important !!!!
cc      IF( vv .GT. vmax)   wt_cut  = 0d0 ???? vmax undefined !!!!

* Total ISR weight
      IF(m_KeyWtm .EQ. 1) wt_mas=1d0
      wtini = wt_mas *wt_cut   ! =ypar(251)
*------------------------------------
*  for debug
      CALL KK2f_SetOneY(252,wt_mas)
      CALL KK2f_SetOneY(254,wt_cut)
      CALL KK2f_SetOneY( 10,vv)
      CALL KK2f_SetOneY(255,vv)
*------------------------------------
      END  !yfs_ini2

      SUBROUTINE YFSini2_AngBre(am2,del1,del2,costhg,sinthg,dist0,dist1)
*     *****************************************************************
* This routine generates photon angular distribution
* in the rest frame of the fermion pair.
* The distribution is the S-factor without mass term,
* i.e. without terms 2p_1p_2/(kp_1)(kp_2)
* Fermion mass is treated exactly!
* INPUT:  
*     am2 = 4*massf**2/s where massf is fermion mass
*     and s is effective mass squared of the parent fermion-pair.
* OUTPUT:
*     del1= 1-beta*cos(theta)
*     del2= 1+beta*cos(theta)
*     costhg, sinthg, cos and sin of the photon
*     angle with respect to fermions direction
*     dist0 = distribution generated, without m**2/(kp)**2 terms
*     dist1 = distribution with m**2/(kp)**2 terms
*     ***************************************
      IMPLICIT NONE
      DOUBLE PRECISION  am2,del1,del2,costhg,sinthg,dist0,dist1
* locals
      REAL              rn(10)
      DOUBLE PRECISION  a,eps,beta
*------------------------------------------------------------------------------
      CALL PseuMar_MakeVec(rn,2)
      beta =sqrt(1.d0-am2)
      eps  =am2/(1.d0+beta)                     != 1-beta
      del1 =(2.d0-eps)*(eps/(2.d0-eps))**rn(1)  != 1-beta*costhg
      del2 =2.d0-del1                           != 1+beta*costhg
* calculation of sin and cos theta from internal variables
      costhg=(del2-del1)/(2*beta)               ! exact
      sinthg=sqrt(del1*del2-am2*costhg**2)      ! exact
* symmetrization
      IF(rn(2) .LE. 0.5d0) THEN
        a=del1
        del1=del2
        del2=a
        costhg= -costhg
      ENDIF
      dist0=1d0/(del1*del2)*(1d0 -am2/2d0)
      dist1=1d0/(del1*del2) 
     $     *(1d0 -am2/2d0 -am2/4d0*(del1/del2+del2/del1))
* totaly equivalent formula is the following
*     dist1=1d0/(del1*del2)   *beta*sinthg**2/(del1*del2)
      END

      SUBROUTINE YFSini2_angbre2(iph,am2,del1,del2,costhg,sinthg,dist0,dist1)
*     **********************************************************
* modified version of angbre, added iph argument which enforces
* forward or backward peak in photon distribution
*     ***************************************
      IMPLICIT NONE
      INTEGER           iph
      DOUBLE PRECISION  am2,del1,del2,costhg,sinthg,dist0,dist1
* locals
      REAL              rn(10)
      DOUBLE PRECISION  a,eps,beta
*---------------------------------------------------------------------------------
      CALL PseuMar_MakeVec(rn,2)

cccc[[[[ very special tests
*      IF( iph .LE. 2) rn(2)=1d0 ! both photons 1 and 2 are forward
* lub
*      IF( iph .EQ. 1) rn(2)=1d0 ! photons 1 forward
*      IF( iph .EQ. 2) rn(2)=0d0 ! photons 2 backward
cccc]]]]

      beta =sqrt(1.d0-am2)
      eps  =am2/(1.d0+beta)                     != 1-beta
      del1 =(2.d0-eps)*(eps/(2.d0-eps))**rn(1)  != 1-beta*costhg
      del2 =2.d0-del1                           != 1+beta*costhg
* calculation of sin and cos theta from internal variables
      costhg=(del2-del1)/(2*beta)               ! exact
      sinthg=sqrt(del1*del2-am2*costhg**2)      ! exact
* symmetrization
      IF(rn(2) .LE. 0.5d0) THEN
        a=del1
        del1=del2
        del2=a
        costhg= -costhg
      ENDIF
      dist0=1d0/(del1*del2)*(1d0 -am2/2d0)
      dist1=1d0/(del1*del2) 
     $     *(1d0 -am2/2d0 -am2/4d0*(del1/del2+del2/del1))
* totaly equivalent formula is the following
*     dist1=1d0/(del1*del2)   *beta*sinthg**2/(del1*del2)
      END


      SUBROUTINE YFSini2_PoissGen(average,nmax,mult,rr)
*     ************************************************
* Last corr. nov. 91
* This generates photon multipl. nphot according to poisson distr.
* Input:  average = average multiplicity
*         nmax  = maximum multiplicity
* Output: mult = generated multiplicity
*         rr(1:100) list of ordered uniform random numbers,
*         a byproduct result, to be eventually used for some further
*         purpose (i.e.  generation of photon energies).
*     ************************
      IMPLICIT NONE
      DOUBLE PRECISION  average
      DOUBLE PRECISION  rr(*)
      INTEGER           nmax,mult
* locals
      DOUBLE PRECISION  y,sum
      INTEGER           it,nfail,nn
      REAL              rvec(10)
      DATA nfail/0/
*----------------------------------------------------------------
 50   nn=0
      sum=0d0
      DO it=1,nmax
         CALL PseuMar_MakeVec(rvec,1)
         y= log(rvec(1))
         sum=sum+y
         nn=nn+1
         rr(nn)=sum/(-average)
         IF(sum .LT. -average) GOTO 130
      ENDDO
      nfail=nfail+1
      IF(nfail .GT. 100) GOTO 900
      GOTO 50
 130  mult=nn-1
      RETURN
 900  WRITE(*,*) ' YFSini2_PoissGen: to small nmax ',nmax
      STOP
      END

*//////////////////////////////////////////////////////////////////////////////
*//                                                                          //
*//                      End of CLASS  YFSini                                //
*//////////////////////////////////////////////////////////////////////////////

