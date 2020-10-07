
      SUBROUTINE AMPINI(XPAR,NPAR)
! ********************************
! Initialization of the external 4fermion matrix el. codes
! XPAR(100),NPAR(100): input parameter matrices of KORALW as explained in
!                      the manual
! For the moment only GRACE is interfaced
! ********************************
      implicit real(16) (a-h,o-z)
      implicit integer (i-n)
      DIMENSION  XPAR (100),NPAR (100) 
      DIMENSION AMAFIN(20) 

! parameters for 4-fermion amplitudes
      COMMON /AM4PAR/ INPAR(100),YXPAR(100),YAMAFIN(20),YSIN2W,YGPICOB

! additional information can be obtained with the help of routine
      CALL MASOW(SIN2W,GPICOB,AMAFIN)
!     where sin2w is a sine of weinberg angle 
!     and   AMAFIN contained masses of W decay products
!     all as printed by the KORALW
!

! filling /AM4PAR/
      do ii=1,100
        inpar(ii) = npar(ii)
        yxpar(ii) = xpar(ii)
      enddo
      do ii=1,20
        yamafin(ii) = amafin(ii)
      enddo
      ygpicob =gpicob
      ysin2w = sin2w
! end  filling /AM4PAR/

      WRITE(6,'(10X,A)')
     $         '*******************************************************'
      WRITE(6,'(10X,A)')
     $         '****** AMPINI: external library initialization ********'
      WRITE(6,'(10X,A)')
     $         '*******************************************************'
      WRITE(6,*) ' '

! here users code should come

      WRITE(6,'(10X,A)') 
     $         '*******************************************************'
      WRITE(6,'(10X,A)') 
     $         '************* AMPINI -- GRACE activation **************'
      WRITE(6,'(10X,A/)') 
     $         '*******************************************************'
      WRITE(*,'(20X,A/)') 'GRACE   Ver.   2. 0'
      WRITE(*,'(20X,A/)') 'date of interfacing: 28-05-96'
      WRITE(*,'(10X,A/)')
     .'(c)Copyright 1990-1996 Minami-Tateya Group (KEK, Japan)     '
      WRITE(6,'(10X,A)') 
     $         '*******************************************************'

!      KeyPhy = NPAR(2)
!      KeyZet = MOD(KeyPhy,1000)/100
!      KeyWu  = MOD(KeyPhy,1000000)/100000

!      if( keyzet.ne.1 ) then
!        write(6,*)'AMPINI==> Sorry, not implemented: KeyZet =',keyzet
!        stop
!      endif
!      if( keywu.ne.1 ) then
!        write(6,*)'AMPINI==> Sorry, not implemented: KeyWu =',keywu
!        stop
!      endif
      call gr_init(1,XPAR,NPAR,SIN2W,GPICOB,AMAFIN)
         
      END



! *****************************************************
      SUBROUTINE AMP4F( Q1,IFLBM1, Q2,IFLBM2
     $      ,P1,IFLAV1, P2,IFLAV2, P3,IFLAV3, P4,IFLAV4
     $      , WTMOD4F,WT4F )
! *****************************************************
! external 4-fermion matrix elements calculations
! INPUTS
!   q1,q2              - beam 4momenta (e-, e+)
!   iflbm1,iflbm2      - beam IDs
!   p1..p4             - final state fermions 4momenta
!                        p1:    fermion of W-
!                        p2:antifermion of W-
!                        p3:    fermion of W+
!                        p4:antifermion of W+
!   iflav1..iflav4     - final state fermions IDs
!   WARNING: for the moment iflav-s are dummy, and 4momenta must be
!            ordered as described above
! OUTPUTS
!   wtmod4f        - principal weight for rejection
!   wt4f(9)        - auxiliary weights wector
!      wtmod4f = elmatr_4fermions
! *****************************************************
      implicit real(16) (a-h,o-z)
      implicit integer (i-n)  

! parameters for 4fermion amplitudes
      COMMON /AM4PAR/ NPAR(100),XPAR(100),AMAFIN(20),SIN2W,GPICOB
!      common /testownis/ pp
*     PARAMETER ( MXDIM = 50 )
*     real(16)  X(MXDIM) 
      DIMENSION Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      DIMENSION RQ1(4),RQ2(4),RP1(4),RP2(4),RP3(4),RP4(4)
      DIMENSION PE(4,6),PP(6,6)
      DIMENSION WT4F(9), isw4f(9)
      DIMENSION II(4),JJ(4),KK(2)
      save
      save suma
      data suma / 0q0 / 

! isw4f entries   1  2  3  4  5  6  7  8  9
      data isw4f /1, 1, 1, 1, 1, 1, 0, 0, 0/


! dipswitch ISWITCH if set to 0 activates just CC03 of GRACE.
! it is for consistency tests. 
! 0: CC03, 
! 1: standard all graphs,
!      5/27/00 added 2,3,4,5 for channel $\mu\mu\tau\tau$
!      6/23/00 changed 4->6, added new 4
! 2: ISNS$_{\gamma+Z}$,
! 3: FSNS$_{\gamma+Z}$ $\tau\tau$ pair to $ee\to \mu\mu$,
! 4: ISNS$_\gamma$ +FSNS$_{gamma}$ $\tau\tau$ pairs.
! 5: ISNS$_\gamma$ $\tau\tau$ pairs.
! 6: FSNS$_{\gamma+Z}$ $\mu\mu$ pair to $ee\to\tau\tau$,
* Graph selection : initial state tautau from gamma
*                   + final state tautau from gamma 
! if ISWITCH is set to negative value then matrix element is 
! calculated for all values of ISWITCH greater than zero 
! that are activated in ISW4f

      ISWITCH = Npar( 8) ! i_sw4f

! anomalous couplings are suppressed in  fake 4fermion matr. el.
      keyacc_lcl=0

      DO I4F=1,9
        WT4F(I4F) = 0q0
      ENDDO

! -- NOW OFF DIAGONAL NON-GRACE CONTRIBUTIONS
     
      if     (iflav1.eq.1.and.iflav2.ne.-2
     $   .and.iflav1.ne.-iflav2.and.iflav3.ne.-iflav4) then
!--   ======
       wtmod4f =wwborn(p1,p2,p3,p4,keyacc_lcl)
      elseif (iflav1.eq.3.and.iflav2.ne.-4
     $   .and.iflav1.ne.-iflav2.and.iflav3.ne.-iflav4) then
!--   ======
       wtmod4f =wwborn(p1,p2,p3,p4,keyacc_lcl)
      elseif (iflav1.eq.5
     $   .and.iflav1.ne.-iflav2.and.iflav3.ne.-iflav4) then
!--   ======
       wtmod4f =wwborn(p1,p2,p3,p4,keyacc_lcl)
      elseif (iflav4.eq.-1.and.iflav3.ne.2
     $   .and.iflav1.ne.-iflav2.and.iflav3.ne.-iflav4) then
!--   ======
       wtmod4f =wwborn(p1,p2,p3,p4,keyacc_lcl)
      elseif (iflav4.eq.-3.and.iflav3.ne.4
     $   .and.iflav1.ne.-iflav2.and.iflav3.ne.-iflav4) then
!--   ======
       wtmod4f =wwborn(p1,p2,p3,p4,keyacc_lcl)
      elseif (iflav4.eq.-5
     $   .and.iflav1.ne.-iflav2.and.iflav3.ne.-iflav4) then
!--   ======
       wtmod4f =wwborn(p1,p2,p3,p4,keyacc_lcl)
      else
!--   ======
! -- NOW DIAGONAL GRACE MODES
! -- WE START WITH::
! definition of  translation of the final states indices as in KORALW 
! into Grace process numbers (idef). iiii denotes relative order 
! as momenta p1,p2,p3,p4 should enter into Grace calculations.
! KB1 defines if cp transformation hast to be used for particular
! final state channel.
      IF(ISWITCH.eq.0) then 
!this is udmn
c       idef = 27
c       iiii =1234
c       KB1  =1   
! switching to sccs  ms. 3/4/98, tests
! do not forget to adjust normalisation (1/3) at the very end
       idef = 62
       iiii =3412
       KB1  =1
      
      ELSEIF(iflav1.eq.-iflav2.and.iflav3.eq.-iflav4
     $       .and.iflav1*iflav4.lt.0) THEN
! new zz-channels
       IF     (iflav1.eq.11.and.iflav3.eq.11) then
        iiii =1234
        KB1  =1
        idef=7
       ELSEIF (iflav1.eq.11.and.iflav3.eq.13) then
        iiii =1234
        KB1  =1
        idef=8
       ELSEIF (iflav1.eq.13.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=8
       ELSEIF (iflav1.eq.11.and.iflav3.eq.15) then
        iiii =1234
        KB1  =1
        idef=9
       ELSEIF (iflav1.eq.15.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=9
       ELSEIF (iflav1.eq.13.and.iflav3.eq.13) then
        iiii =1234
        KB1  =1
        idef=10
       ELSEIF (iflav1.eq.15.and.iflav3.eq.15) then
        iiii =1234
        KB1  =1
        idef=11
       ELSEIF (iflav1.eq.13.and.iflav3.eq.15) then
        iiii =1234
        KB1  =1
        idef=12
       ELSEIF (iflav1.eq.15.and.iflav3.eq.13) then
        iiii =3412
        KB1  =1
        idef=12
       ELSEIF (iflav1.eq.11.and.iflav3.eq.14) then
        iiii =1234
        KB1  =1
        idef=13
       ELSEIF (iflav1.eq.14.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=13
       ELSEIF (iflav1.eq.11.and.iflav3.eq.16) then
        iiii =1234
        KB1  =1
        idef=14
       ELSEIF (iflav1.eq.16.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=14
       ELSEIF (iflav1.eq.12.and.iflav3.eq.13) then
        iiii =1234
        KB1  =1
        idef=15
       ELSEIF (iflav1.eq.13.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=15
       ELSEIF (iflav1.eq.12.and.iflav3.eq.15) then
        iiii =1234
        KB1  =1
        idef=16
       ELSEIF (iflav1.eq.15.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=16
       ELSEIF (iflav1.eq.16.and.iflav3.eq.13) then
        iiii =1234
        KB1  =1
        idef=17
       ELSEIF (iflav1.eq.13.and.iflav3.eq.16) then
        iiii =3412
        KB1  =1
        idef=17
       ELSEIF (iflav1.eq.14.and.iflav3.eq.15) then
        iiii =1234
        KB1  =1
        idef=18
       ELSEIF (iflav1.eq.15.and.iflav3.eq.14) then
        iiii =3412
        KB1  =1
        idef=18
       ELSEIF (iflav1.eq.12.and.iflav3.eq.12) then
        iiii =1234
        KB1  =1
        idef=19
       ELSEIF (iflav1.eq.12.and.iflav3.eq.14) then
        iiii =1234 
        KB1  =1
        idef=20
       ELSEIF (iflav1.eq.14.and.iflav3.eq.12) then
        iiii =3412  
        KB1  =1
        idef=20
       ELSEIF (iflav1.eq.12.and.iflav3.eq.16) then
        iiii =1234  
        KB1  =1
        idef=21
       ELSEIF (iflav1.eq.16.and.iflav3.eq.12) then
        iiii =3412  
        KB1  =1
        idef=21

       ELSEIF (iflav1.eq.14.and.iflav3.eq.14) then
        iiii =1234
        KB1  =1
        idef=22
       ELSEIF (iflav1.eq.16.and.iflav3.eq.16) then
        iiii =1234
        KB1  =1
        idef=23
       ELSEIF (iflav1.eq.14.and.iflav3.eq.16) then
        iiii =1234  ! is it OK ?????
        KB1  =1
        idef=24
       ELSEIF (iflav1.eq.16.and.iflav3.eq.14) then
        iiii =3412  ! is it OK ?????
        KB1  =1
        idef=24
! ...   end table 1 
! ...   e - quarks
       ELSEIF (iflav1.eq.11.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=31
       ELSEIF (iflav1.eq. 2.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=31
       ELSEIF (iflav1.eq.11.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=32
       ELSEIF (iflav1.eq. 4.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=32
       ELSEIF (iflav1.eq.11.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=33
       ELSEIF (iflav1.eq. 1.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=33
       ELSEIF (iflav1.eq.11.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=34
       ELSEIF (iflav1.eq. 3.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=34
       ELSEIF (iflav1.eq.11.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=35
       ELSEIF (iflav1.eq. 5.and.iflav3.eq.11) then
        iiii =3412
        KB1  =1
        idef=35
! ...   mu - quarks messed
       ELSEIF (iflav1.eq.13.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=36
       ELSEIF (iflav1.eq. 2.and.iflav3.eq.13) then
        iiii =3412
        KB1  =1
        idef=36
       ELSEIF (iflav1.eq.13.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=37
       ELSEIF (iflav1.eq. 4.and.iflav3.eq.13) then
        iiii =3412
        KB1  =1
        idef=37
       ELSEIF (iflav1.eq.15.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=38
       ELSEIF (iflav1.eq. 2.and.iflav3.eq.15) then
        iiii =3412
        KB1  =1
        idef=38
       ELSEIF (iflav1.eq.15.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=39
       ELSEIF (iflav1.eq. 4.and.iflav3.eq.15) then
        iiii =3412
        KB1  =1
        idef=39
       ELSEIF (iflav1.eq.13.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=40
       ELSEIF (iflav1.eq. 1.and.iflav3.eq.13) then
        iiii =3412
        KB1  =1
        idef=40
! ...   mu-tau - quarks
       ELSEIF (iflav1.eq.13.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=41
       ELSEIF (iflav1.eq. 3.and.iflav3.eq.13) then
        iiii =3412
        KB1  =1
        idef=41
       ELSEIF (iflav1.eq.13.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=42
       ELSEIF (iflav1.eq. 5.and.iflav3.eq.13) then
        iiii =3412
        KB1  =1
        idef=42
       ELSEIF (iflav1.eq.15.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=43
       ELSEIF (iflav1.eq. 1.and.iflav3.eq.15) then
        iiii =3412
        KB1  =1
        idef=43
       ELSEIF (iflav1.eq.15.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=44
       ELSEIF (iflav1.eq. 3.and.iflav3.eq.15) then
        iiii =3412
        KB1  =1
        idef=44
       ELSEIF (iflav1.eq.15.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=45
       ELSEIF (iflav1.eq. 5.and.iflav3.eq.15) then
        iiii =3412
        KB1  =1
        idef=45
!--
! ...   nu e - quarks
       ELSEIF (iflav1.eq.12.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=31+15
       ELSEIF (iflav1.eq. 2.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=31+15
       ELSEIF (iflav1.eq.12.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=32+15
       ELSEIF (iflav1.eq. 4.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=32+15
       ELSEIF (iflav1.eq.12.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=33+15
       ELSEIF (iflav1.eq. 1.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=33+15
       ELSEIF (iflav1.eq.12.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=34+15
       ELSEIF (iflav1.eq. 3.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=34+15
       ELSEIF (iflav1.eq.12.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=35+15
       ELSEIF (iflav1.eq. 5.and.iflav3.eq.12) then
        iiii =3412
        KB1  =1
        idef=35+15
! ...   nu mu - quarks messed
       ELSEIF (iflav1.eq.14.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=36+15
       ELSEIF (iflav1.eq. 2.and.iflav3.eq.14) then
        iiii =3412
        KB1  =1
        idef=36+15
       ELSEIF (iflav1.eq.14.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=37+15
       ELSEIF (iflav1.eq. 4.and.iflav3.eq.14) then
        iiii =3412
        KB1  =1
        idef=37+15
       ELSEIF (iflav1.eq.16.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=38+15
       ELSEIF (iflav1.eq. 2.and.iflav3.eq.16) then
        iiii =3412
        KB1  =1
        idef=38+15
       ELSEIF (iflav1.eq.16.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=39+15
       ELSEIF (iflav1.eq. 4.and.iflav3.eq.16) then
        iiii =3412
        KB1  =1
        idef=39+15
       ELSEIF (iflav1.eq.14.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=40+15
       ELSEIF (iflav1.eq. 1.and.iflav3.eq.14) then
        iiii =3412
        KB1  =1
        idef=40+15
! ...   nu mutau - quarks
       ELSEIF (iflav1.eq.14.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=41+15
       ELSEIF (iflav1.eq. 3.and.iflav3.eq.14) then
        iiii =3412
        KB1  =1
        idef=41+15
       ELSEIF (iflav1.eq.14.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=42+15
       ELSEIF (iflav1.eq. 5.and.iflav3.eq.14) then
        iiii =3412
        KB1  =1
        idef=42+15
       ELSEIF (iflav1.eq.16.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=43+15
       ELSEIF (iflav1.eq. 1.and.iflav3.eq.16) then
        iiii =3412
        KB1  =1
        idef=43+15
       ELSEIF (iflav1.eq.16.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=44+15
       ELSEIF (iflav1.eq. 3.and.iflav3.eq.16) then
        iiii =3412
        KB1  =1
        idef=44+15
       ELSEIF (iflav1.eq.16.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=45+15
       ELSEIF (iflav1.eq. 5.and.iflav3.eq.16) then
        iiii =3412
        KB1  =1
        idef=45+15
! ... hadronic processes ...
       ELSEIF (iflav1.eq. 2.and.iflav3.eq. 2) then
        iiii =1234
        KB1  =1
        idef=64
       ELSEIF (iflav1.eq. 4.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=65
       ELSEIF (iflav1.eq. 1.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=66
       ELSEIF (iflav1.eq. 3.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=67
       ELSEIF (iflav1.eq. 5.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=68
       ELSEIF (iflav1.eq. 2.and.iflav3.eq. 4) then
        iiii =1234
        KB1  =1
        idef=69
       ELSEIF (iflav1.eq. 4.and.iflav3.eq. 2) then
        iiii =3412
        KB1  =1
        idef=69
       ELSEIF (iflav1.eq. 2.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=70
       ELSEIF (iflav1.eq. 3.and.iflav3.eq. 2) then
        iiii =3412
        KB1  =1
        idef=70
       ELSEIF (iflav1.eq. 2.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=71
       ELSEIF (iflav1.eq. 5.and.iflav3.eq. 2) then
        iiii =3412
        KB1  =1
        idef=71
       ELSEIF (iflav1.eq. 4.and.iflav3.eq. 1) then
        iiii =1234
        KB1  =1
        idef=72
       ELSEIF (iflav1.eq. 1.and.iflav3.eq. 4) then
        iiii =3412
        KB1  =1
        idef=72
       ELSEIF (iflav1.eq. 4.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=73
       ELSEIF (iflav1.eq. 5.and.iflav3.eq. 4) then
        iiii =3412
        KB1  =1
        idef=73
       ELSEIF (iflav1.eq. 1.and.iflav3.eq. 3) then
        iiii =1234
        KB1  =1
        idef=74
       ELSEIF (iflav1.eq. 3.and.iflav3.eq. 1) then
        iiii =3412
        KB1  =1
        idef=74
       ELSEIF (iflav1.eq. 1.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=75
       ELSEIF (iflav1.eq. 5.and.iflav3.eq. 1) then
        iiii =3412
        KB1  =1
        idef=75
       ELSEIF (iflav1.eq. 3.and.iflav3.eq. 5) then
        iiii =1234
        KB1  =1
        idef=76
       ELSEIF (iflav1.eq. 5.and.iflav3.eq. 3) then
        iiii =3412
        KB1  =1
        idef=76

       ELSE
        write(*,*)  'AMP4F: not initialized zz mode'
        write(*,*) iflav1,iflav2,iflav3,iflav4
        stop
       ENDIF

      ELSEIF(IFLAV1.eq. 11.and.IFLAV4.eq.-11) THEN
       idef =1
       iiii =3412
       KB1  =1
      ELSEIF(IFLAV1.eq. 11.and.IFLAV4.eq.-13) THEN
       idef =2
       iiii =1243
       KB1  =1
      ELSEIF(IFLAV1.eq. 13.and.IFLAV4.eq.-11) THEN
       idef =2
       iiii =1243
       KB1  =2
      ELSEIF(IFLAV1.eq. 11.and.IFLAV4.eq.-15) THEN
       idef =3
       iiii =1243
       KB1  =1
      ELSEIF(IFLAV1.eq. 15.and.IFLAV4.eq.-11) THEN
       idef =3
       iiii =1243
       KB1  =2
      ELSEIF(IFLAV1.eq. 13.and.IFLAV4.eq.-13) THEN
       idef =4
       iiii =3412
       KB1  =1
      ELSEIF(IFLAV1.eq. 13.and.IFLAV4.eq.-15) THEN
       idef =6
       iiii =1243
       KB1  =1
      ELSEIF(IFLAV1.eq. 15.and.IFLAV4.eq.-13) THEN
       idef =6
       iiii =1243
       KB1  =2
      ELSEIF(IFLAV1.eq. 15.and.IFLAV4.eq.-15) THEN
       idef =5
       iiii =3412
       KB1  =1
      ELSEIF(IFLAV1.eq. 11.and.IFLAV4.eq.-1) THEN
       idef = 25
       iiii =1234
       KB1  =1
      ELSEIF(IFLAV1.eq.  1.and.IFLAV4.eq.-11) THEN
       idef = 25
       iiii =1234
       KB1  =2
      ELSEIF(IFLAV1.eq. 13.and.IFLAV4.eq.-1) THEN
       idef = 27
       iiii =1234
       KB1  =1
      ELSEIF(IFLAV1.eq.  1.and.IFLAV4.eq.-13) THEN
       idef = 27
       iiii =1234
       KB1  =2
      ELSEIF(IFLAV1.eq. 15.and.IFLAV4.eq.-1) THEN
       idef = 29
       iiii =1234
       KB1  =1
      ELSEIF(IFLAV1.eq.  1.and.IFLAV4.eq.-15) THEN
       idef = 29
       iiii =1234
       KB1  =2
      ELSEIF(IFLAV1.eq. 11.and.IFLAV4.eq.-3) THEN
       idef = 26
       iiii =1234
       KB1  =1
      ELSEIF(IFLAV1.eq.  3.and.IFLAV4.eq.-11) THEN
       idef = 26
       iiii =1234
       KB1  =2
      ELSEIF(IFLAV1.eq. 13.and.IFLAV4.eq.-3) THEN
       idef = 28
       iiii =1234
       KB1  =1
      ELSEIF(IFLAV1.eq.  3.and.IFLAV4.eq.-13) THEN
       idef = 28
       iiii =1234
       KB1  =2
      ELSEIF(IFLAV1.eq. 15.and.IFLAV4.eq.-3) THEN
       idef = 30
       iiii =1234
       KB1  =1
      ELSEIF(IFLAV1.eq.  3.and.IFLAV4.eq.-15) THEN
       idef = 30
       iiii =1234
       KB1  =2
      ELSEIF(IFLAV1.eq. 1.and.IFLAV4.eq.-1) THEN
       idef = 61
       iiii =3412
       KB1  =1
      ELSEIF(IFLAV1.eq. 3.and.IFLAV4.eq.-3) THEN
       idef = 62
       iiii =3412
       KB1  =1
      ELSEIF(IFLAV1.eq. 3.and.IFLAV4.eq.-1) THEN
       idef = 63
       iiii =3412
       KB1  =1
      ELSEIF(IFLAV1.eq. 1.and.IFLAV4.eq.-3) THEN
       idef = 63
       iiii =3412
       KB1  =2
      else
       WRITE(*,*) 'amp4f:     I do not know how to handle:'
       WRITE(*,*) 'IFLAV(1-4): ',IFLAV1,IFLAV2,IFLAV3,IFLAV4
       stop
      endif

!--- CP transformation now ...
        IF(KB1.EQ.1) Then
         do k=1,4
          rq1(k)=q1(k)
          rq2(k)=q2(k)
          rp1(k)=p1(k)
          rp2(k)=p2(k)
          rp3(k)=p3(k)
          rp4(k)=p4(k)
         enddo
        else
         do k=1,3
          rq1(k)=-q2(k)
          rq2(k)=-q1(k)
          rp1(k)=-p4(k)
          rp2(k)=-p3(k)
          rp3(k)=-p2(k)
          rp4(k)=-p1(k)
         enddo
          k=4
          rq1(k)=q2(k)
          rq2(k)=q1(k)
          rp1(k)=p4(k)
          rp2(k)=p3(k)
          rp3(k)=p2(k)
          rp4(k)=p1(k)
        endif
c-- translation of order index iiii into algorithm variables
!
        II(1)=    iiii      /1000
        II(2)=mod(iiii,1000)/100 
        II(3)=mod(iiii, 100)/10  
        II(4)=mod(iiii,  10)     
        DO L=1,4
         IL=II(L)
         JJ(IL)=L+2
        ENDDO
!
!        write(*,*) ii(1),ii(2),ii(3),ii(4)
!        write(*,*) jj(1),jj(2),jj(3),jj(4)
!        stop
!
!---  ======================
       DO K=1,4
*           1:  EL-  INITIAL   LPRTCL  MASS=AMEL
         PE( K,    1 ) = rq1( K)
*           2:  EL+  INITIAL   LANTIP  MASS=AMEL
         PE( K,    2 ) = rq2( K)
*           3:  MU-  FINAL     LPRTCL  MASS=AMMU
         PE( K, JJ(1)) = rp1( K)
*           4:  NM   FINAL     LANTIP  MASS=AMNM
         PE( K, JJ(2)) = rp2( K)
*           5:  UQ   FINAL     LPRTCL  MASS=AMUQ
         PE( K, JJ(3)) = rp3( K)
*           6:  DQ+  FINAL     LANTIP  MASS=AMDQ
         PE( K, JJ(4)) = rp4( K)
       ENDDO
!
*     Set table of inner products of momenta.
*     This table should be calculated from invariants.
*     When it is constructed from PE,it will not so accurate.
*
*     PP(I,J) = inner product between PE(*,I) and PE(*,J)
!      iwal=0
      do i=1,6
        do j=1,6
          pp(i,j) = pe(4,i)*pe(4,j) 
     &         -pe(1,i)*pe(1,j) -pe(2,i)*pe(2,j) -pe(3,i)*pe(3,j)
!          if(pp(i,j).lt.1d-7) iwal=1
        enddo
      enddo
!      if(iwal.eq.1) then
!          write(*,*) '=====amp4f here is problem==============='
!          write(*,*) '=====one of inv is too small============='
!       do i=1,6
!         do j=1,6
!           write(*,*) 'i=',i,' j=',j,' pp=',pp(i,j)
!         enddo
!       enddo
!      endif

!!      WTMOD4F = 1q0
!!      if(iflav1.eq.13.and.iflav3.eq.2) then  !(81)
!!      if(iflav1.eq.1.and.iflav3.eq.2) then  !(11)
!!      if(iflav1.eq.1.and.iflav3.eq.14) then  !(18)
!!      if(iflav1.eq.13.and.iflav3.eq.14) then  !(88)
*=======================================================================
*==== full 4-fermion x-section only for CKM dominant channels !

! MS. 19.06.97
!!       CALL gr_rinit(idef,XPAR,NPAR,SIN2W,GPICOB,AMAFIN)
       CALL gr_rinit(idef)
! MS. 19.06.97

! MS 31.05.00
! MS 31.05.00
       IF(iswitch.ge.0) THEN
! MS 31.05.00
! MS 31.05.00
          CALL SM_KORALW(ISWITCH,XPAR,NPAR,SIN2W,GPICOB,AMAFIN)
*         ============= coupling constants
           call amparm
*         =============
*           el_4ferm = func(x)
           el_4ferm = func(pe,pp)

 !The following re-normalisation for CC03 is necessary, because for
 !CC03 all flavour configurations use the same matrix element of
 !doubly leptonic type (in Grace).
           IF(ISWITCH.eq.0) then
              if(abs(iflav1).lt.10) then
                 wm = 3q0
              else
                 wm = 1q0
              endif
              if(abs(iflav3).lt.10) then
                 wp = 3q0
              else
                 wp = 1q0
              endif 
c     el_4ferm = el_4ferm*wp*wm/3q0  ! uden
              el_4ferm = el_4ferm*wp*wm/9q0 ! cssc
           ENDIF
! ... up to here

           WTMOD4F = el_4ferm
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! in the doubly ckm-suppresses case we add !!!
!!! in quadratures the CC03 contribution     !!!
!!!                          10/7/98 ms      !!!
! first select ZZ-type final states
           IF(iflav1.eq.-iflav2.and.iflav3.eq.-iflav4
     $          .and.iflav1*iflav4.lt.0) THEN
              icwm=iflav1
              icwp=iflav3
! then select MIX non-diag.
              IF( iflav1.EQ.2 .AND. iflav3.EQ.3   .OR.
     @            iflav1.EQ.2 .AND. iflav3.EQ.5   .OR.
     @            iflav1.EQ.4 .AND. iflav3.EQ.5   .OR.
     @            iflav1.EQ.4 .AND. iflav3.EQ.1 ) THEN
! swap ZZ to WW
! the 4momenta 1-3 must be flipped (u~us~s -> s~uu~s)
                 wtcc03 =wwborn(p3,p2,p1,p4,keyacc_lcl)
! send to interfaces also the ratio
                 wt4f9=wtmod4f/(wtcc03+wtmod4f)
                 CALL waga_ckm(-1,wt4f9)  
                 wtmod4f=wtmod4f+wtcc03 
              ELSEIF( iflav1.EQ.1 .AND. iflav3.EQ.4   .OR.
     @                iflav1.EQ.3 .AND. iflav3.EQ.2   .OR.
     @                iflav1.EQ.5 .AND. iflav3.EQ.2   .OR.
     @                iflav1.EQ.5 .AND. iflav3.EQ.4 ) THEN
! swap ZZ to WW
! the 4momenta 2-4 must be flipped (s~su~u -> s~uu~s)
                 wtcc03 =wwborn(p1,p4,p3,p2,keyacc_lcl)
! send to interfaces also the ratio
                 wt4f9=wtmod4f/(wtcc03+wtmod4f)
                 CALL waga_ckm(-1,wt4f9)  
                 wtmod4f=wtmod4f+wtcc03 
              ENDIF
           ENDIF
!!!                                          !!!
!!! in the doubly ckm-suppresses case we add !!!
!!! in quadratures the CC03 contribution     !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! MS 31.05.00
        ELSE
! MS 31.05.00
           DO is=1,9
              IF(isw4f(is).NE.0 .OR. is.EQ.1) THEN
                 CALL SM_KORALW(is,XPAR,NPAR,SIN2W,GPICOB,AMAFIN)
*               ============= coupling constants
                 call amparm
*               =============
*                el_4 = func(x)
                 el_4 = func(pe,pp)
                 wt4f(is) = el_4
              ENDIF
           ENDDO
           wtmod4f=wt4f(1)
! MS 31.05.00
! MS 31.05.00
        ENDIF
! MS 31.05.00
! MS 31.05.00

      endif
!--   ======
!--   end of the main choice menu. 
*=======================================================================

c      WT4F(1) = el_WWpair
c      WT4F(2) = el_4ferm

*=======================================================================

!!!!!!!
c      suma=suma+wtmod4f
c      WRITE(6,*)'suma==',suma
c      IF(el_4ferm.le.0q0) write(6,*)'elmatr zero,wtmod4f',wtmod4f
!!!!!!!
!      write(8,'(a,f18.11)') '88 wt4f=',wtmod4f
       IF(ISWITCH.eq.0) then
         el_WW=wwborn(p1,p2,p3,p4,0)
         WRITE(8,'(a,f18.11,a,f22.16,a,I3,I3,I3,I3)') 
     $   '88 wt4f=',wtmod4f,' ',wtmod4f/el_WW, 
     $   ' IFLAV(1-4): ',IFLAV1,IFLAV2,IFLAV3,IFLAV4
         WRITE(6,'(a,f18.11,a,f22.16,a,I3,I3,I3,I3)') 
     $   '88 wt4f=',wtmod4f,' ',wtmod4f/el_WW, 
     $   ' IFLAV(1-4): ',IFLAV1,IFLAV2,IFLAV3,IFLAV4
       ENDIF

*      write(6,*) '4ferm=',el_WWpair,el_4ferm
*      write(6,*) 'wtmod4ferm=',wtmod4f

      return
      END

* File spdetc.f generated by GRACE Ver. 2.00(35)        1996/03/24/15:33
* 
*          Fortran source code generator
*     (c)copyright 1990-1996 Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
      subroutine spdetx(ireco)
*-----------------------------------------------------------------------
*     Selection of Color State for llll,llqq,qqqq
*        output:icolst (0->ncbase-1)
*-----------------------------------------------------------------------
      implicit real(16) (a-h,o-z)
      implicit integer (i-n)
C     include 'grc4f_init/incl1.f'
C     include 'grc4f_init/inclk.f'
      include 'incl1.h'
      include 'inclk.h'
      dimension asum(0:ncbase-1),cratio(0:ncbase-1)
      DIMENSION drvec(1)

      if( kmcbas .eq. 1 ) then
          icolst = 0
      else
*-----------------------------------------------------------------------
*        Calcul amplitutes squared.
*-----------------------------------------------------------------------
      do 120 ic = 0 , ncbase-1
      asum(ic) = 0.0q0
      do 110 ih = 0, lag-1
         asum(ic) = asum(ic) + dble(agc(ih,ic))**2 + imag(agc(ih,ic))**2
  110 continue
      asum(ic) = asum(ic)*cfmtx(ic,ic)
  120 continue
*-----------------------------------------------------------------------
*        Search maximum.
*-----------------------------------------------------------------------
      allsum = 0.0q0
      do 130 ic = 0, ncbase-1
         allsum = allsum + asum(ic)
  130 continue
      tmpsum = 0.0q0
      do 140 ic = 0, ncbase-1
         tmpsum = tmpsum + asum(ic)
         cratio(ic) = tmpsum/allsum
  140 continue

!      cran = drn(idummy)
        CALL varran(drvec,1)
        cran=drvec(1)
      icolst = 0
      do 150 ic = 1, ncbase-1
! here was a bug ? zw 20.06.96 if( cratio(ic) .gt. cran ) then
         if( cratio(ic-1) .gt. cran ) then
             goto 160
         else
             icolst = ic
         endif
  150 continue
  160 continue
      endif
      ireco=icolst
      return
      end






* File setmas.f generated by GRACE Ver. 2.00(35)        1996/03/24/15:33
* 
*          Fortran source code generator
*     (c)copyright 1990-1996 Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
************************************************************************
      subroutine sm_koralw(ibackgr,xpar,npar,sin2w,gpicob,amafin)
************************************************************************
* Overwrites GRACE initialisation according to KORALW needs
* ibackgr = 0  - doubly resonant W-pairs
* ibackgr = 1  - complete 4fermion process
************************************************************************
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      dimension  xpar ( *),npar ( *)
      dimension  amafin(20)
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      include 'incl1.h'
      include 'inclk.h'
      common /chcntl/ jwidth
      character*80 BXOPE,BXTXT,BXCLO,BXTXI,BXTXF
      data init /0/
      save init
*-----------------------------------------------------------------------
* constants
      pi    = acos(- 1.0q0 )
      pi2   = pi * pi
      rad   = pi / 180.0q0
      gevpb = 0.38937966d9
      alpha = 1.0q0/128.07q0
      alpha0= 1.0q0/137.0359895q0
*KEK  alphas= 0.123q0
      alphas= 0.12q0

************* KORALW stuff ******************
      ALFWIN  = XPAR(3)
      ALPHAW  = 1q0/ ALFWIN
      XAMAZ   = XPAR(4)
      GAMMZ   = XPAR(5)
      XAMAW   = XPAR(6)
      GAMMW   = XPAR(7)
       xamh   =xpar(11)
       xagh   =xpar(12)
! missing redefinition of alphas ! m.s. 6/19/98
      alphas  =xpar(13)

! missing redefinition of alphaw ! m.s. 12/5/97
      alpha = alphaw  ! m.s. 12/5/97
************* end KORALW stuff ******************
*-----------------------------------------------------------------------
      call selgrf( ibackgr )

      jgraph = 0
*-----------------------------------------------------------------------
* mass
*
         amw = 80.23q0
         amz = 91.1888q0
***** From KORAL (begin) *****
         amw = xamaw
         amz = xamaz
***** From KORAL (e n d) *****
         ama = 0.0q0
         amg = 0.0q0
         amh = 10000.0q0
***** From KORAL (begin) *****
         amh = xamh
***** From KORAL (e n d) *****
         amx = AMW
         amy = AMZ
        amne = 0.0q0
        amnm = 0.0q0
        amnt = 0.0q0
        amel = 0.51099906Q-3
        ammu = 105.658389Q-3
        amta = 1.7771q0

***** From KORAL (begin) *****
      AMNE = AMAFIN(12)
      AMNM = AMAFIN(14)
      AMNT = AMAFIN(16)
      AMEL = AMAFIN(11)
      AMMU = AMAFIN(13)
      AMTA = AMAFIN(15)
***** From KORAL (e n d) *****

        amuq = 5.0Q-3
        amcq = 1.3q0
        amtq = 174.0q0
        amdq = 10.0Q-3
        amsq = 200.0Q-3
        ambq = 4.3q0
***** From KORAL (begin) *****
      AMUQ = AMAFIN(2)
      AMCQ = AMAFIN(4)
* 7/15/98 ms      AMTQ = 174.0q0
      AMTQ = AMAFIN(6)
      AMDQ = AMAFIN(1)
      AMSQ = AMAFIN(3)
      AMBQ = AMAFIN(5)
***** From KORAL (e n d) *****
        amcp = AMW
        amcm = AMW
        amcz = AMZ
        amca = AMA
        amcg = AMG

* set quark mass = 1.d-5
*
      if( jqmass .eq. 0 )then
*
        write(6,*) 'WARNING: sm_koralw=> quark masses set to 1q-5'
            amuq = 1.0q-5
            amcq = 1.0q-5
            amtq = 1.0q-5
            amdq = 1.0q-5
            amsq = 1.0q-5
            ambq = 1.0q-5
      endif
*
* width

         agw = 2.03367033062746q0
         agz = 2.4974q0
         agh = 0.0q0
***** From KORAL (begin) *****
      AGW  = GAMMW
      AGZ  = GAMMZ
      agh  = xagh
***** From KORAL (e n d) *****

         agx = AGW
         agy = AGZ
        agcq = 0.0q0
        agtq = 0.0q0
        agsq = 0.0q0
        agbq = 0.0q0
        agcp = AGW
        agcm = AGW
        agcz = AGZ

* Gauge parametes (default is unitary gauge)
      igauab = 0
      igauwb = 0
      igauzb = 0
      igaugb = 0
      agauge(igauab) = 1.0q0
      agauge(igauwb) = 1.0q0
      agauge(igauzb) = 1.0q0
      agauge(igaugb) = 1.0q0
      agauge(igau00) = 1.0q0

! test 6/26/00
*   Unitary gauge
c      IGAUAB = 0
c      IGAUWB = 0
c      IGAUZB = 0
c      IGAUGL = 0
c
c      AGAUGE(0) = 1.0D20
c
*   Covariant gauge
c      IGAUAB = 1
c      IGAUWB = 2
c      IGAUZB = 3
c      IGAUGL = 4
c
c      AGAUGE(IGAUAB) = 2.0q0
c      AGAUGE(IGAUWB) = 3.0q0
c      AGAUGE(IGAUZB) = 4.0q0
c      AGAUGE(IGAUGL) = 5.0q0
c
! end test 6/26/00

* Spin average
      aspin = 1.0q0

*     1: initial electron mass=amel
      jhs(1) = 0
      jhe(1) = lextrn - 1
      aspin = aspin/dble(jhe(1) - jhs(1)+1)

*     2: initial positron mass=amel
      jhs(2) = 0
      jhe(2) = lextrn - 1
      aspin = aspin/dble(jhe(2) - jhs(2)+1)

*     3: final nu-e mass=amne
      jhs(3) = 0
      jhe(3) = lextrn - 1

*     4: final positron mass=amel
      jhs(4) = 0
      jhe(4) = lextrn - 1

*     5: final electron mass=amel
      jhs(5) = 0
      jhe(5) = lextrn - 1

*     6: final nu-e-bar mass=amne
      jhs(6) = 0
      jhe(6) = lextrn - 1

* Flag of cyclic polarization
*     QED vertex with on-shell fermions.
!      jtgamm = 0
*     Anomalous coupling for 3-vector-boson.
      jano3v = 0
*     Coulomb correction.
*n    jcolmb = 0
*     QCD correction.
*n    jqcdcr = 0
*     Private flag (Internal Higgs)
      jhiggs = 1
*     Private flag (Internal Gluon)
      jgluon = 0

*     Private flag (Decay)
*n    jdecay = 0
*     Private flag (Hadronization)
*n    jhadrn = 1

*     Running width (0) or fixed width(1) in CHANEL
      jwidth = 0

*     Coulomb correction
      colmbf = 1.0q0
      if (init.eq.0) then
       init=1
       BXOPE =  '(//1X,15(5H*****)    )' 
       BXTXT =  '(1X,1H*,                  A48,25X,    1H*)'
       BXTXI =  '(1X,1H*,                  A48,I2,23X, 1H*)'
       BXTXF =  '(1X,1H*,                A48,F7.5,18X, 1H*)'
       BXCLO =  '(1X,15(5H*****)/   )' 
       NOUT=6
       WRITE(NOUT,BXOPE)
       WRITE(NOUT,BXTXT) 'Grace 2.0 initialization routine sm_koralw:'

       WRITE(NOUT,BXTXI) 'Higgs switch                 jhiggs =    '
     $                  ,jhiggs
       WRITE(NOUT,BXTXI) 'gluon switch                 jgluon =    '
     $                  ,jgluon
       WRITE(NOUT,BXTXF) 'gluon as intermediate boson: alpha_s=    '
     $                  ,alphas
       WRITE(NOUT,BXTXT) 'warning from sm_koralw:                  '
       WRITE(NOUT,BXTXT) 'please check consistency of the sin2w:   '
       WRITE(NOUT,BXTXT) 'as defined in KORALW vs required by GRACE'
       WRITE(NOUT,BXTXT) 'consult manuals of the two packages      '
       WRITE(NOUT,BXCLO)
! not needed to prt. write(*,*) 'Running width switch jwidth =',jwidth 
      endif
      return
      end


*     (c)copyright           KORAL         Group, Poland
*     (c)copyright           Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
************************************************************************
      SUBROUTINE gr_init(ibackgr,XPAR,NPAR,SIN2W,GPICOB,AMAFIN)
************************************************************************
* ibackgr = 0  - doubly resonant W-pairs
* ibackgr = 1  - complete 4fermion process
************************************************************************
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)

******************
      DIMENSION  XPAR (100),NPAR (100)
      DIMENSION  AMAFIN(20)
******************
*=======================================================================
*          parameters for amplitude calculation
*=======================================================================

*         ============= Set global parameters
           call usrprm
*         =============

*=======================================================================
*================== KORALW overrides Mass and Width ====================
*=======================================================================
*         ====================================================
           CALL SM_KORALW(ibackgr,XPAR,NPAR,SIN2W,GPICOB,AMAFIN)
*         ====================================================
*=======================================================================
*=======================================================================
*         ============= select process
           call procdb
*         =============

*         ============= select kinematics
           call kinmdb
*         =============

*         ============= coupling constants
           call amparm
*         =============
      return
      end
*     (c)copyright           KORAL         Group, Poland
*     (c)copyright           Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
************************************************************************
      SUBROUTINE gr_rinit(ib)
************************************************************************
* ib type of the final state 
************************************************************************
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)

******************
*=======================================================================
*          parameters for amplitude calculation
*=======================================================================

*         ============= Set global parameters
           call rusrprm(ib)
*         =============

*         ============= select process
           call procdb
*         =============

*         ============= select kinematics
           call kinmdb
*         =============

*         ============= coupling constants
!           call amparm
*         =============
      return
      end

* File usrprm.f generated by  "grc4f"
* 
*          Fortran source code generator
*     (c)copyright 1990-1996 Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
************************************************************************
      subroutine rusrprm(i)
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)

      include 'incl1.h'
      include 'inclk.h'
      common /chcntl/ jwidth
      common /grc4fs/ nthprc
*-----------------------------------------------------------------------
* 
* Set global parameters.
* 
*     Process No.
      nthprc = i
*     Set quark-mass = 0.(jqmass = 1 )
      jqmass = 1
*     running width (1:fixed / 0:run)
      jwidth = 0

      return
      end






* File amparm.f generated by GRACE Ver. 2.00(35)        1996/03/24/15:33
* 
*          Fortran source code generator
*     (c)copyright 1990-1996 Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
************************************************************************
      subroutine amparm
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)

      include 'incl1.h'
      include 'inclk.h'

      DIMENSION amaf(20)
*-----------------------------------------------------------------------
**    alpha     =   1.0q0/137.0359895q0
*
      complex(16) zr, zi
      save       zr, zi
      zr = dcmplx(1.0q0, 0.0q0)
      zi = dcmplx(0.0q0, 1.0q0)
*-----------------------------------------------------------------------
*     call sminit(   1,   1)

************* KORALW stuff ******************
! m.s.6/19/98      CALL KWPAR2(XAMAW,XAMAZ,XGAMMW,XGAMMZ,XSINW2)
      CALL MASOW(XSINW2,GPICB,AMAF)   ! m.s.6/19/98  
************* end KORALW stuff ******************
* coupling constants
*-----------------------------------------------------------------------
      zero      =   0.0q0
      one       =   1.0q0
      two       =   2.0q0
      three     =   3.0q0
      four      =   4.0q0
*
*  qed coupling constant
*
      r2        =   sqrt(two)
      r2i       =   one/r2
      ge2       =   four*pi*alpha
      ge        =   sqrt(ge2)
      ge2h      =   ge2/two
      cqed      =   ge
*
*  electric charges
*
      ql        = - one
      qu        =   two/three
      qd        = - one/three
*
*  electro-weak coupling constants
*
*   gauge coupling constants
*
      amw2      =   amw**2
      amh2      =   amh**2
*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*     GW    = dsqrt(1q0-xsinw2)/dsqrt(xsinw2)  ! AMWB/AMZW
*     GZ    = 1q0/dsqrt(xsinw2)                ! AMZB/AMZW
*     GZW   = 1q0/dsqrt(1q0-xsinw2)            ! AMZB/AMWB
*     GWZ   = dsqrt(1q0-xsinw2)                ! AMWB/AMZB
*     gcos      =   amw/amz
      gcos      =   sqrt(1.0q0-xsinw2)
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*-----------------------------------------------------------------------

      gcos2     =   gcos**2
      gsin      =   sqrt(one-gcos2)
      gtan      =   gsin/gcos
      gcosi     =   one/gcos
      gcos2i    =   one/gcos2
      gcosd     =   two*gcos2 - one
*
      gg        =   ge/gsin
      gg2       =   gg**2
      ggh       =   gg/two
      gg2h      =   gg2/two
*
*   sss and ssss
*     gs        =   gg *amh2/(two *amw )
*     gs2       =   gg2*amh2/(four*amw2)
      gs        =   gg *amh2/(two *amw )
      gs2       =   gg2*amh2/(four*amw2)
*
*   ffw
      gwfl      =   gg*r2i
*
*   ffa
      gal       =   ql*ge
      gau       =   qu*ge
      gad       =   qd*ge
*
*   ffz
      gza       =   ggh*gcosi
      gzc       =   gg*gtan*gsin
*
      gznl      =   gza
      gzll      = - ql*gzc - gza
      gzlr      = - ql*gzc
      gzul      = - qu*gzc + gza
      gzur      = - qu*gzc
      gzdl      = - qd*gzc - gza
      gzdr      = - qd*gzc
*
*   sff
      gx        =   gg*r2i/amw
      g3        =   ggh/amw
* qcd coupling constants
*
      cqcd      =   one
      cqcd      =   alphas
* coupling constants

        czww    = zr*( gg*gcos)
        caww    = zr*( ge)
        cggg    = zr*( cqcd)
       cwwaa    = zr*( ge2)
       cwwza    = zr*( ge*gg*gcos)
       cwwzz    = zr*( gg2*gcos2)
       cwwww    = zr*(-gg2)
       cgggg    = zr*( cqcd**2)
        cwhm    = zi*( ggh)
        cwhp    = zi*( ggh)
        cwym    = zr*( ggh)
        cwyp    = zr*(-ggh)
        czpm    = zr*(-ggh*gcosd*gcosi)
        capm    = zr*(-ge)
        czhy    = zi*( ggh*gcosi)
        chww    = zr*( gg*amw)
        chzz    = zr*( gg*amz*gcosi)
        cwzm    = zi*( ge*amz*gsin)
        cwam    = zi*(-ge*amw)
        cwzp    = zi*(-ge*amz*gsin)
        cwap    = zi*( ge*amw)
       cwwhh    = zr*( gg2h)
       czzhh    = zr*( gg2h*gcos2i)
       cwzhm    = zi*( ge2h*gcosi)
       cwzhp    = zi*(-ge2h*gcosi)
       cwahm    = zi*(-ge*ggh)
       cwahp    = zi*( ge*ggh)
       cwwyy    = zr*( gg2h)
       czzyy    = zr*( gg2h*gcos2i)
       cwzym    = zr*( ge2h*gcosi)
       cwzyp    = zr*( ge2h*gcosi)
       cwaym    = zr*(-ge*ggh)
       cwayp    = zr*(-ge*ggh)
       cwwpm    = zr*( gg2h)
       czzpm    = zr*( gg2h*(gcosd*gcosi)**2)
       caapm    = zr*( two*ge2)
       czapm    = zr*( ge*gg*gcosd*gcosi)
        chyy    = zr*(-gs)
        chhh    = zr*(-three*gs)
       chhhh    = zr*(-three*gs2)
       cyyyy    = zr*(-three*gs2)
       cpmyy    = zr*(-gs2)
       chhpm    = zr*(- gs2)
       chhyy    = zr*(-gs2)
       cpmpm    = zr*(-two*gs2)
        cwne(1) = zr*( gwfl)
        cwne(2) = zr*0
        cwnm(1) = zr*( gwfl)
        cwnm(2) = zr*0
        cwnt(1) = zr*( gwfl)
        cwnt(2) = zr*0
        cwel(1) = zr*( gwfl)
        cwel(2) = zr*0
        cwmu(1) = zr*( gwfl)
        cwmu(2) = zr*0
        cwta(1) = zr*( gwfl)
        cwta(2) = zr*0
        cwuq(1) = zr*( gwfl)
        cwuq(2) = zr*0
        cwcq(1) = zr*( gwfl)
        cwcq(2) = zr*0
        cwtq(1) = zr*( gwfl)
        cwtq(2) = zr*0
        cwdq(1) = zr*( gwfl)
        cwdq(2) = zr*0
        cwsq(1) = zr*( gwfl)
        cwsq(2) = zr*0
        cwbq(1) = zr*( gwfl)
        cwbq(2) = zr*0
        cael(1) = zr*( gal)
        cael(2) = zr*( gal)
        camu(1) = zr*( gal)
        camu(2) = zr*( gal)
        cata(1) = zr*( gal)
        cata(2) = zr*( gal)
        cauq(1) = zr*( gau)
        cauq(2) = zr*( gau)
        cacq(1) = zr*( gau)
        cacq(2) = zr*( gau)
        catq(1) = zr*(gau)
        catq(2) = zr*(gau)
        cadq(1) = zr*( gad)
        cadq(2) = zr*( gad)
        casq(1) = zr*( gad)
        casq(2) = zr*( gad)
        cabq(1) = zr*( gad)
        cabq(2) = zr*( gad)
        czne(1) = zr*( gznl)
        czne(2) = zr*0
        cznm(1) = zr*( gznl)
        cznm(2) = zr*0
        cznt(1) = zr*( gznl)
        cznt(2) = zr*0
        czel(1) = zr*( gzll)
        czel(2) = zr*( gzlr)
        czmu(1) = zr*( gzll)
        czmu(2) = zr*( gzlr)
        czta(1) = zr*( gzll)
        czta(2) = zr*( gzlr)
        czuq(1) = zr*( gzul)
        czuq(2) = zr*( gzur)
        czcq(1) = zr*( gzul)
        czcq(2) = zr*( gzur)
        cztq(1) = zr*( gzul)
        cztq(2) = zr*( gzur)
        czdq(1) = zr*( gzdl)
        czdq(2) = zr*( gzdr)
        czsq(1) = zr*( gzdl)
        czsq(2) = zr*( gzdr)
        czbq(1) = zr*( gzdl)
        czbq(2) = zr*( gzdr)
        cguq(1) = zr*( cqcd)
        cguq(2) = zr*( cqcd)
        cgdq(1) = zr*( cqcd)
        cgdq(2) = zr*( cqcd)
        cgcq(1) = zr*( cqcd)
        cgcq(2) = zr*( cqcd)
        cgsq(1) = zr*( cqcd)
        cgsq(2) = zr*( cqcd)
        cgbq(1) = zr*( cqcd)
        cgbq(2) = zr*( cqcd)
        cgtq(1) = zr*( cqcd)
        cgtq(2) = zr*( cqcd)
        cmmu(1) = zi*( ammu*gx)
        cmmu(2) = zi*0
        cmta(1) = zi*( amta*gx)
        cmta(2) = zi*0
        cpmu(1) = zi*0
        cpmu(2) = zi*(-ammu*gx)
        cpta(1) = zi*0
        cpta(2) = zi*(-amta*gx)
        cmuq(1) = zi*( amdq*gx)
        cmuq(2) = zi*(-amuq*gx)
        cmcq(1) = zi*( amsq*gx)
        cmcq(2) = zi*(-amcq*gx)
        cmtq(1) = zi*( ambq*gx)
        cmtq(2) = zi*(-amtq*gx)
        cpdq(1) = zi*( amuq*gx)
        cpdq(2) = zi*(-amdq*gx)
        cpsq(1) = zi*( amcq*gx)
        cpsq(2) = zi*(-amsq*gx)
        cpbq(1) = zi*( amtq*gx)
        cpbq(2) = zi*(-ambq*gx)
        chmu(1) = zr*(-ammu*g3)
        chmu(2) = zr*(-ammu*g3)
        chta(1) = zr*(-amta*g3)
        chta(2) = zr*(-amta*g3)
        chuq(1) = zr*(-amuq*g3)
        chuq(2) = zr*(-amuq*g3)
        chcq(1) = zr*(-amcq*g3)
        chcq(2) = zr*(-amcq*g3)
        chtq(1) = zr*(-amtq*g3)
        chtq(2) = zr*(-amtq*g3)
        chdq(1) = zr*(-amdq*g3)
        chdq(2) = zr*(-amdq*g3)
        chsq(1) = zr*(-amsq*g3)
        chsq(2) = zr*(-amsq*g3)
        chbq(1) = zr*(-ambq*g3)
        chbq(2) = zr*(-ambq*g3)
        cymu(1) = zi*(-( ammu*g3))
        cymu(2) = zi*( ammu*g3)
        cyta(1) = zi*(-( amta*g3))
        cyta(2) = zi*( amta*g3)
        cyuq(1) = zi*(-(-amuq*g3))
        cyuq(2) = zi*(-amuq*g3)
        cycq(1) = zi*(-(-amcq*g3))
        cycq(2) = zi*(-amcq*g3)
        cytq(1) = zi*(-(-amtq*g3))
        cytq(2) = zi*(-amtq*g3)
        cydq(1) = zi*(-( amdq*g3))
        cydq(2) = zi*( amdq*g3)
        cysq(1) = zi*(-( amsq*g3))
        cysq(2) = zi*( amsq*g3)
        cybq(1) = zi*(-( ambq*g3))
        cybq(2) = zi*( ambq*g3)
       cwczp    = zr*(-gg*gcos)
       cwcmz    = zr*( gg*gcos)
       cwcap    = zr*(-ge)
       cwcma    = zr*( ge)
       cwczm    = zr*( gg*gcos)
       cwcpz    = zr*(-gg*gcos)
       cwcam    = zr*( ge)
       cwcpa    = zr*(-ge)
       czcmm    = zr*(-gg*gcos)
       czcpp    = zr*( gg*gcos)
       cacmm    = zr*(-ge)
       cacpp    = zr*( ge)
       cgcgg    = zr*( cqcd)
       cpczp    = zi*(-ggh*gcosd*amz)
       cpcap    = zi*(-ge*amw)
       cpcmz    = zi*( ggh*amz)
       cmczm    = zi*( ggh*gcosd*amz)
       cmcam    = zi*( ge*amw)
       cmcpz    = zi*(-ggh*amz)
       cycmm    = zi*(-ggh*amw)
       cycpp    = zi*( ggh*amw)
       chcmm    = zr*(-ggh*amw)
       chcpp    = zr*(-ggh*amw)
       chczz    = zr*(-ggh*gcosi*amz)

      return
      end
************************************************************************
      subroutine selgrf(ibackgr)
************************************************************************
* Overwrites GRACE initialisation according to KORALW needs
* ibackgr = 0  - doubly resonant W-pairs
* ibackgr = 1  - complete 4fermion process
************************************************************************
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)
      include 'incl1.h'
      common /grc4fs/ nthprc
*-----------------------------------------------------------------------
*------------------------------------
      if( ibackgr .eq. 0 ) then
*------------------------------------
          do n1 = 1, ngraph
             jselg(n1) = 0
          enddo
* Graph selection : W-pairs
          if( nthprc .eq. 27 ) then
              jselg(15) = 1
              jselg(19) = 1
              jselg(23) = 1
          elseif( nthprc .eq. 62 ) then
              jselg(57) = 1
              jselg(61) = 1
              jselg(77) = 1
          else
              print *,'not yet selgrf',nthprc
              stop
          endif
*------------------------------------
      elseif( ibackgr .eq. 2 ) then
*------------------------------------
! NOTE that graphs 33-36 are higgs + ghosts
          do n1 = 1, ngraph
             jselg(n1) = 0
          enddo
* Graph selection : initial state tata 
* pairs in mumutata final state (12)
          if( nthprc .eq. 12 ) then
          do n1 = 37, 44
             jselg(n1) = 1
          enddo
          endif
*------------------------------------
      elseif( ibackgr .eq. 3 ) then
*------------------------------------
          do n1 = 1, ngraph
             jselg(n1) = 0
          enddo
* Graph selection : final state tata 
* pairs off mumu in mumutata final state (12)
          if( nthprc .eq. 12 ) then
          do n1 = 1, 16
             jselg(n1) = 1
          enddo
          endif
*------------------------------------
      elseif( ibackgr .eq. 4 ) then
*------------------------------------
          do n1 = 1, ngraph
             jselg(n1) = 0
          enddo
* Graph selection : initial state tautau from gamma
*                   + final state tautau from gamma 
          if( nthprc .eq. 12 ) then
! IS
             jselg(37) = 1
             jselg(39) = 1
             jselg(41) = 1
             jselg(42) = 1
! FS
             jselg( 1) = 1
             jselg( 5) = 1
             jselg( 9) = 1
             jselg(13) = 1
          elseif( nthprc .eq. 8 ) then
! this is \mu\mu\e\e channel
! IS
             jselg(39) = 1
             jselg(40) = 1
             jselg(43) = 1
             jselg(45) = 1
! FS
             jselg( 9) = 1
             jselg(11) = 1
             jselg(13) = 1
             jselg(15) = 1
          endif
*------------------------------------
      elseif( ibackgr .eq. 5 ) then
*------------------------------------
          do n1 = 1, ngraph
             jselg(n1) = 0
          enddo
* Graph selection : initial state tautau from gamma  
          if( nthprc .eq. 12 ) then
             jselg(37) = 1
             jselg(39) = 1
             jselg(41) = 1
             jselg(42) = 1
          elseif( nthprc .eq. 8 ) then
! this is \mu\mu\e\e channel
! IS
             jselg(39) = 1
             jselg(40) = 1
             jselg(43) = 1
             jselg(45) = 1
          endif
*------------------------------------
      elseif( ibackgr .eq. 6 ) then
*------------------------------------
          do n1 = 1, ngraph
             jselg(n1) = 0
          enddo
* Graph selection : final state mumu 
* pairs off tata in mumutata final state (12)
          if( nthprc .eq. 12 ) then
          do n1 = 17, 32
             jselg(n1) = 1
          enddo
          endif
*------------------------------------
      else
*------------------------------------
* Graph selection : all graphs
          do 20 n1 = 1, ngraph
             jselg(n1) = 1
   20     continue
      endif
*-----------------------------------------------------------------------
      return
      end

* File usrprm.f generated by  "grc4f"
* 
*          Fortran source code generator
*     (c)copyright 1990-1996 Minami-Tateya Group, Japan
*-----------------------------------------------------------------------
************************************************************************
      subroutine usrprm
      implicit real(16)(a-h,o-z)
      implicit integer (i-n)

      include 'incl1.h'
      include 'inclk.h'
      common /chcntl/ jwidth
      common /grc4fs/ nthprc
*-----------------------------------------------------------------------
* 
* Set global parameters.
* 
*     Process No.
      nthprc = 27
*     Set quark-mass = 0.(jqmass = 1 )
      jqmass = 1
*     running width (1:fixed / 0:run)
      jwidth = 0

      return
      end
