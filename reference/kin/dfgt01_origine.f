CDECK  ID>, AMPCXX.
      SUBROUTINE AMPCXX (GAL,GAX,GZL,GZX,GSNX,AMSN,GMSN,AMZ,GMZ,
     .                   EIN,EOT,XIN,XOT,AMP)

      IMPLICIT   REAL*4  ( A-H, O-Z )
      REAL   *4  GAL(2), GAX(2), GZL(2), GZX(2),
     .           AMSN, GMSN, AMZ, GMZ
      COMPLEX*8  GSNX(2,2)
      COMPLEX*8  EIN(6), EOT(6), XIN(6), XOT(6), AMP(0:3)
C--
      COMPLEX*8  CURR(6), SCAL(3)
C
C========< Entry Point >================================================
C
C--
C  Compute photon s-channel exchange diagram.
C--
      CALL JIOXXX(EIN,EOT,GAL,0.,0.,CURR)
      CALL IOVXXX(XIN,XOT,CURR,GAX,AMP(1))
C--
C  Compute Z s-channel exchange diagram.
C--
      CALL JIOXXX(EIN,EOT,GZL,AMZ,GMZ,CURR)
      CALL IOVXXX(XIN,XOT,CURR,GZX,AMP(2))
C--
C  Compute t-channel snu exchange diagram.
C--
      CALL HIOXXX(EIN,XOT,GSNX(1,1),AMSN,GMSN,SCAL)
      CALL IOSXXX(XIN,EOT,SCAL,GSNX(1,2),AMP(3))
      AMP(3) = - AMP(3)
C--
C  Sum three amplitudes.
C     AMP(0) = sum.
C        (1) = photon-exchange.
C        (2) = Z-exchange.
C        (3) = snu-exchange.
C--
      AMP(0) = AMP(1) + AMP(2) + AMP(3)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, FULCXD.
      SUBROUTINE FULCXD(IDP,IHEL,PV,AMP2)

      IMPLICIT   REAL*4  ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      COMMON /SSCONS/ xAM0, xAMU, xAM2, xTANB, xAMA, xRELAX
      REAL*4          xAM0, xAMU, xAM2, xTANB, xAMA, xRELAX
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      COMMON /SMCUPL/ GAL, GAU, GAD, GWF, GZN, GZL,
     .                GZU, GZD, G1, GW, GWWZ, GWWA
C--
      REAL*4          GAL(2), GAU(2), GAD(2), GWF(2), GZN(2),GZL(2),
     .                GZU(2), GZD(2), G1(2), GW, GWWZ, GWWA
C--
      COMMON /SSCUPL/ GNCW, GCCZ, GNNZ,
     .                GCESNL, GCNSEL, GCNSER,
     .                GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     .                GNNSNL, GNESEL, GNESER,
     .                GNUSUL, GNUSUR, GNDSDL, GNDSDR
C--
      COMPLEX*8  GNCW(2,4,2), GCCZ(2,2,2), GNNZ(2,4,4)
      COMPLEX*8  GCESNL(2,2), GCNSEL(2,2), GCNSER(2,2),
     .           GCUSDL(2,2), GCUSDR(2,2), GCDSUL(2,2), GCDSUR(2,2)
      COMPLEX*8  GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     .           GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)
C--

      INTEGER*4  IDP(3,*), IHEL(*)
      REAL   *4  PV(0:3,*), AMP2(0:3)
C--
      COMPLEX*8  SPINOR(6,20), WVCT(6), SCAL(3), AMP(0:3)
      COMPLEX*8  GWX(2,2), GCFSF(2,2,2), GNFSF(2,2,2)
      REAL   *4  AMSSF(2,2), GAMSF(2,2)
      DATA NCALL / 0 /
C
C========< Entry Point >================================================
C
C--
C  Prepare coupling constants.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL     = 1
         AMSW      = AMW
         GAMW      = GMWTOT
C>>>
C        QCD       =  1
         QCD       =  1 + xALFS/xPI
C>>>
         GWX(1,1)  = CONJG(GNCW(1,1,1))
         GWX(2,1)  = CONJG(GNCW(2,1,1))
         GWX(1,2)  = GNCW(1,1,1)
         GWX(2,2)  = GNCW(2,1,1)
C>>>
CTBW          GWX(1,1)  = GWF(1)
CTBW          GWX(2,1)  = GWF(2)
C>>>
      ENDIF
C--
C  Set masses and couplings depending on final states.
C--
      CALL RDOTxx(PV(0,1),PV(0,1),AMSX)
      AMSX  = SQRT(AMSX)
      CALL RDOTxx(PV(0,2),PV(0,2),AMSX0)
      AMSX0 = SQRT(AMSX0)
C--
      AM1   = AMSX
      AM2   = AMSX0
      AM3   = AMF(IDP(1,3),IDP(2,3),IDP(3,3))
      AM4   = AMF(IDP(1,4),IDP(2,4),IDP(3,4))
C--
      IF ( IDP(3,3).EQ.1 ) THEN
         AMSSF(  1,1) = SFM(1)
         GAMSF(  1,1) = GMSF(1)
         GCFSF(1,1,1) = GCESNL(1,1)
         GCFSF(2,1,1) = GCESNL(2,1)
         GNFSF(1,1,1) = CONJG(GNNSNL(2,1))
         GNFSF(2,1,1) = CONJG(GNNSNL(1,1))
         AMSSF(  2,1) = SFM(2)
         GAMSF(  2,1) = GMSF(2)
         GCFSF(1,2,1) = CONJG(GCNSEL(2,1))
         GCFSF(2,2,1) = CONJG(GCNSEL(1,1))
         GNFSF(1,2,1) = GNESEL(1,1)
         GNFSF(2,2,1) = GNESEL(2,1)
      ELSE
         AMSSF(  1,1) = SFM(4)
         GAMSF(  1,1) = GMSF(4)
         GCFSF(1,1,1) = GCDSUL(1,1)
         GCFSF(2,1,1) = GCDSUL(2,1)
         GNFSF(1,1,1) = CONJG(GNUSUL(2,1))
         GNFSF(2,1,1) = CONJG(GNUSUL(1,1))
         AMSSF(  2,1) = SFM(6)
         GAMSF(  2,1) = GMSF(6)
         GCFSF(1,2,1) = CONJG(GCUSDL(2,1))
         GCFSF(2,2,1) = CONJG(GCUSDL(1,1))
         GNFSF(1,2,1) = GNDSDL(1,1)
         GNFSF(2,2,1) = GNDSDL(2,1)
      ENDIF
C--
C  Prepare spinors for external lines.
C--
      CALL OXXXXX(PV(0,1),AM1   ,IHEL(1),-1,SPINOR(1, 1))
      CALL IXXXXX(PV(0,2),AM2   ,IHEL(2),-1,SPINOR(1, 2))
      CALL OXXXXX(PV(0,3),AM3   ,IHEL(3),+1,SPINOR(1, 3))
      CALL IXXXXX(PV(0,3),AM3   ,IHEL(3),-1,SPINOR(1,13))
      CALL IXXXXX(PV(0,4),AM4   ,IHEL(4),-1,SPINOR(1, 4))
      CALL OXXXXX(PV(0,4),AM4   ,IHEL(4),+1,SPINOR(1,14))
C--
C  Prepare spinors for X+.
C--
      CALL JIOXXX(SPINOR(1,4),SPINOR(1,3),GWF,AMSW,GAMW,WVCT)
      CALL IOVXXC(SPINOR(1,2),SPINOR(1,1),WVCT,GWX(1,1),AMP(1))
C--
      CALL HIOXXX(SPINOR(1,2),SPINOR(1, 3),GNFSF(1,1,1),
     .            AMSSF(1,1),GAMSF(1,1),SCAL)
      CALL IOSXXX(SPINOR(1, 4),SPINOR(1,1),SCAL,GCFSF(1,1,1),AMP(2))
C--
      CALL HIOXXX(SPINOR(1,2),SPINOR(1,14),GNFSF(1,2,1),
     .            AMSSF(2,1),GAMSF(2,1),SCAL)
      CALL IOSXXX(SPINOR(1,13),SPINOR(1,1),SCAL,GCFSF(1,2,1),AMP(3))
C--
      AMP(0)  = AMP(1) - AMP(2) + AMP(3)
C>>>
CTBW       AMP(0) = AMP(1)
C>>>
C--
C  Calculate amplitude squared.
C--
      FACT    = (3*QCD)**(IDP(3,3)-1)
     .          * VKM( IDP(1,3), IDP(1,4), IDP(3,3) )**2
      AMP2(0) = FACT*ABS(AMP(0))**2
      AMP2(1) = FACT*ABS(AMP(1))**2
      AMP2(2) = FACT*ABS(AMP(2))**2
      AMP2(3) = FACT*ABS(AMP(3))**2
C--
C  That's it.
C--
      RETURN
      END

CDECK  ID>, GMZ2FF.
C* (Inputs)
C*    AMZ    : (R*4) : Z mass.
C*    T3F    : (R*4) : T3 of fermion.
C*    QF     : (R*4) : fermion charge.
C*    C      : (R*4) : color factor.
C*    AM     : (R*4) : fermion mass.
C* (Output)
C*    GM     : (R*4) : partial width for Z --> f + f.
C*

      SUBROUTINE GMZ2FF(AMZ,T3F,QF,C,AM,GM)

      IMPLICIT REAL*4 ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4 AMZ, T3F, QF, C, AM, GM
C
C========< Entry Point >================================================
C
C--
C  Calculate width.
C--
      AMZ2 = AMZ*AMZ
      P1   = AMZ2/4 - AM*AM
      IF ( P1.LE.0.D0 ) THEN
         GM = 0
         RETURN
      ELSE
         P1   = SQRT(P1)
      ENDIF
      GV  =  T3F/2 - xSIN2W*QF
      GA  = -T3F/2
      TTA = 2*( (GV**2+GA**2)*( AMZ2 - 4*P1*P1/3 )
     .         - 4*GA*GA*AM*AM )
      FAC =  (xALF/xSIN2W/xCOS2W)/2
      GM  = FAC*TTA*C*P1/AMZ2
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, INSSCN.
      SUBROUTINE INSSCN(ALF,ALFS,S2W,WM,ZM,AM0,AMU,AM2,TANB,AMA,RELAX)

C 17/JUL/95   Modified to include sin2w as an input argument. T.T

      IMPLICIT    REAL*4 ( A-H, O-Z )
      COMMON /SSCONS/ xAM0, xAMU, xAM2, xTANB, xAMA, xRELAX
      REAL*4          xAM0, xAMU, xAM2, xTANB, xAMA, xRELAX
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      COMMON /SSCUPL/ GNCW, GCCZ, GNNZ,
     .                GCESNL, GCNSEL, GCNSER,
     .                GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     .                GNNSNL, GNESEL, GNESER,
     .                GNUSUL, GNUSUR, GNDSDL, GNDSDR
C--
      COMPLEX*8  GNCW(2,4,2), GCCZ(2,2,2), GNNZ(2,4,4)
      COMPLEX*8  GCESNL(2,2), GCNSEL(2,2), GCNSER(2,2),
     .           GCUSDL(2,2), GCUSDR(2,2), GCDSUL(2,2), GCDSUR(2,2)
      COMPLEX*8  GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     .           GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)
C--
      REAL*4   ALF, ALFS, WM, ZM, AM0, AMU, AM2, TANB, AMA, RELAX
      REAL*4   FM(3)
C
C========< Entry Point >================================================
C
C--
C  Initialize SUSY parameters.
C--
      xAM0   = AM0
      xAMU   = AMU
      xAM2   = AM2
      xTANB  = TANB
      xAMA   = AMA
      xRELAX = RELAX
C--
C  Initialize particle table and coupling constants.
C  This version ignors Yukawa couplings.
C--
CTT 17/Jul/95     S2W   = ( 1 - WM/ZM )*( 1 + WM/ZM )
      FM(1) = 0
      FM(2) = 0
      FM(3) = 0
C--
CTT 17/Jul/95  WM is included as an input argument fo INSUSY
      CALL INSUSY(AM0,AMU,AM2,RELAX,TANB,AMA,ALF,ALFS,S2W,ZM,WM,FM,
     .            SFM,SWM,SZM,GMSF,GMSW,GMSZ,
     .            GNCW,GCCZ,GNNZ,
     .            GCESNL,GCNSEL,GCNSER,
     .            GCUSDL,GCUSDR,GCDSUL,GCDSUR,
     .            GNNSNL,GNESEL,GNESER,
     .            GNUSUL,GNUSUR,GNDSDL,GNDSDR )
C--
      GMSH(1) = 0.5
      GMSH(2) = 0.5
      GMSH(3) = 0.5
      GMSH(4) = 0.5
C--
C  Print out modified parameters.
C--
      WRITE(*,*)'             '
      WRITE(*,*)' ****** INSSCN Modified SSCONS and SSPTAB ********'
      WRITE(*,*)'             '
      WRITE(*,*)'    AM0    = ', xAM0
      WRITE(*,*)'    AMU    = ', xAMU
      WRITE(*,*)'    AM2    = ', xAM2
      WRITE(*,*)'    TANB   = ', xTANB
      WRITE(*,*)'    AMA    = ', xAMA
      WRITE(*,*)'    RELAX  = ',xRELAX
      WRITE(*,*)'        '
      WRITE(*,*)'    AMSNL  = ', SFM(1)
      WRITE(*,*)'    AMSEL  = ', SFM(2)
      WRITE(*,*)'    AMSER  = ', SFM(3)
      WRITE(*,*)'    AMSUL  = ', SFM(4)
      WRITE(*,*)'    AMSUR  = ', SFM(5)
      WRITE(*,*)'    AMSDL  = ', SFM(6)
      WRITE(*,*)'    AMSDR  = ', SFM(7)
      WRITE(*,*)'        '
      WRITE(*,*)'    AMSW1  = ', SWM(1)
      WRITE(*,*)'    AMSW2  = ', SWM(2)
      WRITE(*,*)'        '
      WRITE(*,*)'    AMSZ1  = ', SZM(1)
      WRITE(*,*)'    AMSZ2  = ', SZM(2)
      WRITE(*,*)'    AMSZ3  = ', SZM(3)
      WRITE(*,*)'    AMSZ4  = ', SZM(4)
      WRITE(*,*)'        '
      WRITE(*,*)'    GMSNL  = ', GMSF(1)
      WRITE(*,*)'    GMSEL  = ', GMSF(2)
      WRITE(*,*)'    GMSER  = ', GMSF(3)
      WRITE(*,*)'    GMSUL  = ', GMSF(4)
      WRITE(*,*)'    GMSUR  = ', GMSF(5)
      WRITE(*,*)'    GMSDL  = ', GMSF(6)
      WRITE(*,*)'    GMSDR  = ', GMSF(7)
      WRITE(*,*)'        '
      WRITE(*,*)'    GMSW1  = ', GMSW(1)
      WRITE(*,*)'    GMSW2  = ', GMSW(2)
      WRITE(*,*)'        '
      WRITE(*,*)'    GMSZ1  = ', GMSZ(1)
      WRITE(*,*)'    GMSZ2  = ', GMSZ(2)
      WRITE(*,*)'    GMSZ3  = ', GMSZ(3)
      WRITE(*,*)'    GMSZ4  = ', GMSZ(4)
      WRITE(*,*)'        '
      WRITE(*,*)' *****************************************************'
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, SGCXXF.
CC**********************************************************************
C*
C*=====================================-----===
C* Subroutine SGCXXF(IDP,IHEL,AM,IPV,X,SG,PV)
C*=====================================-----===
C*
C* (Purpose)
C*    Calculate differential cross section for e+ + e- ---> X+ + X-
C*    with their subsequent decays.
C* (Inputs)
C*       IDP(1,i) : (I*4) : generation number.
C*          (2,i) : (I*4) : T3 + 1.5.
C*          (3,i) : (I*4) : l/q flag; (1,2) = (l,q).
C*       IHEL( i) : (I*4) : helicity combination.
C*                        : i = (1,2,3,4,5,6,7,8)
C*                        :   = (e-,e+,X0,fu,fdb,X0,fub,fd)
C*                        :            <-- X+ --><-- X- -->
C*       AM(i)    : (R*4) : mass of i.
C*       IPV(1,1) : (I*4) : a+
C*          (2,1) : (I*4) : b+
C*          (3,1) : (I*4) : c+
C*          (1,2) : (I*4) : a-
C*          (2,2) : (I*4) : b-
C*          (3,2) : (I*4) : c-
C*       X(1,1)   : (R*4) : s.
C*        (2,1)   : (R*4) : cos(theta_X-).
C*        (3,1)   : (R*4) : phi_X-.
C*       X(1,2)   : (R*4) : invariant mass squared for X+.
C*        (2,2)   : (R*4) : cos(theta_a+) in X+ rest frame.
C*        (3,2)   : (R*4) : phi_a+ in X+ rest frame.
C*       X(1,3)   : (R*4) : invariant mass squared for a+b+.
C*        (2,3)   : (R*4) : invariant mass squared for a+c+.
C*        (3,3)   : (R*4) : phi_b+ in X+ rest frame.
C*       X(1,4)   : (R*4) : invariant mass squared for X-.
C*        (2,4)   : (R*4) : cos(theta_a-) in X- rest frame.
C*        (3,4)   : (R*4) : phi_a- in X- rest frame.
C*       X(1,5)   : (R*4) : invariant mass squared for a-b-.
C*        (2,5)   : (R*4) : invariant mass squared for a-c-.
C*        (3,5)   : (R*4) : phi_b- in X- rest frame..
C* (Output)
C*       PV(*,i)  : (R*4) : 4-momentum of i-th particle, where
C*                        : numbering convention is that of IDP.
C*       SG       : (R*4) : d(sigma)/dX1dX2....dXn.
C* (Relation)
C*    Invokes UVZERO, UHSETF, UBTRAN, FULXCXC, and library routines
C*    in 'HELAS.FORT'.
C*
CC**********************************************************************

      SUBROUTINE SGCXXF(IDP,IHEL,AM,IPV,X,SG,PV)

      IMPLICIT     REAL*4  ( A-H, O-Z )

      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)

      INTEGER*4    IDP(3,*), IHEL(*), IPV(3,*)
      REAL   *4    AM(*), X(3,*), SG, PV(0:3,*)
      REAL   *4    QV(0:3,8), EC(3,3), EB(3,3), EA(3,3), AMP2(0:3)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Constants.
C  Spin average is taken only for positron.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         SPIN   = 2
         FACT   = xGV2PB*2/(x4PI**4)/SPIN
         AME    = AMF(1,2,1)
         CALL UVZERO(9,EC)
         EC(1,1) = 1
         EC(2,2) = 1
         EC(3,3) = 1
      ENDIF
C--
C  Set 4-momenta.
C--
      CALL UVZERO(32,PV)
      CALL UVZERO(32,QV)
C--
      WAT = 1
C
C-- e+ + e- --> CM.
C
      S       = X(1,1)
      RS      = SQRT(S)
      EBM     = RS/2
      PBM     = SQRT((EBM-AME)*(EBM+AME))
      BT0     = PBM/EBM
      PV(0,1) = EBM
      PV(3,1) = PBM
      CALL PMIRRx(PV(0,1),PV(0,2))
      QV(0,1) = RS
C
C-- CM --> X+ X-
C
      AM12 = X(1,4)
      AM22 = X(1,2)
      CALL UHPHS2(1,QV(0,1),AM12,AM22,EC,X(2,1),X(3,1),
     .            QV(0,4),QV(0,3),BT1,EA)
      IF ( BT1.EQ.0. )                           GO TO 9999
      WAT = WAT*BT1
      CALL UVCOPY(9,EC,EA)
      CALL UVCOPY(9,EC,EB)
C
C-- X+ --> a + b + c
C
      AM1 = AM(IPV(1,1))
      AM2 = AM(IPV(2,1))
      AM3 = AM(IPV(3,1))
      Q12 = SQRT(X(1,3))
      Q13 = SQRT(X(2,3))
C>>>
CCDBG       PRINT *, ' P_X+ = ', (QV(K,3),K=0,3)
CCDBG       PRINT *, ' AM1, AM2, AM3 = ', AM1, AM2, AM3
CCDBG       PRINT *, ' CS1, FI1, FI2 = ', X(2,2), X(3,2), X(3,3)
CCDBG       PRINT *, ' Q12 = ', Q12, ' Q13 = ', Q13
C>>>
      CALL UHPHS3(QV(0,3),AM1,AM2,AM3,X(2,2),
     .            X(3,2),X(3,3),Q12,Q13,WT,QV(0,5))
      IF ( WT.EQ.0. )                            GO TO 9999
      WAT = WAT*WT
C--
      CALL UVCOPY(4,QV(0,5),PV(0,IPV(1,1)))
      CALL UVCOPY(4,QV(0,6),PV(0,IPV(2,1)))
      CALL UVCOPY(4,QV(0,7),PV(0,IPV(3,1)))
C
C-- X- --> a + b + c
C
      AM1 = AM(IPV(1,2))
      AM2 = AM(IPV(2,2))
      AM3 = AM(IPV(3,2))
      Q12 = SQRT(X(1,5))
      Q13 = SQRT(X(2,5))
C>>>
CCDBG       PRINT *, ' P_X- = ', (QV(K,4),K=0,3)
CCDBG       PRINT *, ' AM1, AM2, AM3 = ', AM1, AM2, AM3
CCDBG       PRINT *, ' CS1, FI1, FI2 = ', X(2,4), X(3,4), X(3,5)
CCDBG       PRINT *, ' Q12 = ', Q12, ' Q13 = ', Q13
C>>>
      CALL UHPHS3(QV(0,4),AM1,AM2,AM3,X(2,4),
     .            X(3,4),X(3,5),Q12,Q13,WT,QV(0,5))
      IF ( WT.EQ.0. )                            GO TO 9999
      WAT = WAT*WT
C--
      CALL UVCOPY(4,QV(0,5),PV(0,IPV(1,2)))
      CALL UVCOPY(4,QV(0,6),PV(0,IPV(2,2)))
      CALL UVCOPY(4,QV(0,7),PV(0,IPV(3,2)))
C--
C  Calculate phase space weight.
C--
      WAT = WAT*FACT/(2*S*BT0)
C--
C  Calculate amplitude squared.
C--
CPHASE                SG  = WAT
CPHASE                RETURN
C>>>
CCDBG        PRINT *, ' PV_3 = ', (PV(K,3),K=0,3)
CCDBG        PRINT *, ' PV_4 = ', (PV(K,4),K=0,3)
CCDBG        PRINT *, ' PV_5 = ', (PV(K,5),K=0,3)
CCDBG        PRINT *, ' PV_6 = ', (PV(K,6),K=0,3)
CCDBG        PRINT *, ' PV_7 = ', (PV(K,7),K=0,3)
CCDBG        PRINT *, ' PV_8 = ', (PV(K,8),K=0,3)
C>>>
      CALL FULCXX(IDP,IHEL,PV,AMP2)
C>>>
CCDBG        PRINT *, ' WAT = ', WAT, ' AMP2 = ', AMP2
C>>>
C--
C  Differenctial cross section.
C--
      SG  = AMP2(0)*WAT
CAONLY                SG  = AMP2(1)*WAT
CZONLY                SG  = AMP2(2)*WAT
CTONLY                SG  = AMP2(3)*WAT
C--
C  That's it.
C--
      RETURN
C--
C  Kinematically forbidden.
C--
9999  SG = 0
      RETURN
      END
CDECK  ID>, UHQLIM.
CC**********************************************************************
C*
C*=========================================-----------===
C* Subroutine UHPQLIM(AMPR,AM1,AM2,AM3,Q12,Q13MN,Q13MX)
C*=========================================-----------===
C*
C* (Purpose)
C*    Calculate kinematic boundary for PR ---> 1 + 2 + 3.
C* (Inputs)
C*       AMPR    : (R*8) : parent mass.
C*       AM1     : (R*8) : 1st daughter mass.
C*       AM2     : (R*8) : 2nd daughter mass.
C*       AM3     : (R*8) : 3rd daughter mass.
C*       Q12     : (R*8) : invariant mass for pair 1-2.
C* (Output)
C*       Q13MN   : (R*8) : Q_13 minimum.
C*       Q13MX   : (R*8) : Q_13 maximum.
C* (Relation)
C*    Invokes no external subroutines or functions.
C*
CC**********************************************************************

      SUBROUTINE UHQLIM(AMPR,AM1,AM2,AM3,Q12,Q13MN,Q13MX)

      IMPLICIT   REAL*8  ( A-H, O-Z )
      REAL*8     AMPR, AM1, AM2, AM3, Q12, Q13MN, Q13MX
C
C========< Entry Point >================================================
C
C--
C  Calculate (E,P) in 1-2 rest frame.
C--
      E1 = Q12*Q12
      E1 = ( E1 + (AM1-AM2)*(AM1+AM2) )/2/Q12
      P1 = SQRT(MAX(( E1 - AM1 )*( E1 + AM1 ),0.D0))
C--
      E3 = (AMPR-Q12)*(AMPR+Q12)
      E3 = ( E3 - AM3*AM3 )/2/Q12
      P3 = SQRT(MAX(( E3 - AM3 )*( E3 + AM3 ),0.D0))
C--
C  Calculate kinematic boundary.
C--
      E1P3  = E1 + E3
      P1P3  = P1 + P3
      P1M3  = P1 - P3
      Q13MN = SQRT(MAX((E1P3-P1P3)*(E1P3+P1P3),0.D0))
      Q13MX = SQRT(MAX((E1P3-P1M3)*(E1P3+P1M3),0.D0))
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, COUPLSUB.
C======================================================================C
C subroutines to give couplings of supersymmetric particles
C   Caution !!!!
C     outputs are complex*8
C   Outputs are always those of G_couple of the following Lagrangean
C      Lagrangean = G_couple * (field_i) * (field_j)....
C   Even if field_i and field_j are the same Majorana particles,
C   factor 1/2 is also included. In other word, if L is
C      Lagrangean = (1/2) * G_couple *(Maj)*(Maj)
C   output is given as (1/2)*G_couple.
C======================================================================C
C----------------------------------------------------------------------C
      SUBROUTINE  INOGBS( ALPHE, SIN2W,
     &                    PHIL, PHIR, ER, ON, ETA,
     &                    GNCW, GCCZ, GNNZ )
C----------------------------------------------------------------------C
C Purpose: give couplings of Ino -Gauge_boson
C Input  : ALPHE  e**2/4/pi         !
C          SIN2W  weak mix. angle   !  S.M. parameters
C          PHIL, PHIR (real*4) mixing angle for chargino
C          ER         (real*4) phase for chargino
C          ON(i,j) (real*4)(i,j=1..4)diagonalizing matrix for chargino
C          ETA(i)  (complex*8) (i=1..4)    phase factor
C Output : GNCW(i,j,k) (i=1..2, j=1..4, k=1..2 )
C          GCCZ(i,j,k) (i=1..2, j=1..2, k=1..2 )
C          GNNZ(i,j,k) (i=1..2, j=1..4, k=1..4 )  ( j =/< k )
C              i=1/left-handed,  i=2/right-handed
C
C----------------------------------------------------------------------C
      REAL*4    ON(4,4)
      COMPLEX*8 ETA(4)

      COMPLEX*8 GNCW(2,4,2)
      COMPLEX*8 GCCZ(2,2,2), GV(2,2), GA(2,2)
      COMPLEX*8 GNNZ(2,4,4), CA(4,4), CV(4,4)
      COMPLEX*8 IMGI, CMPLX
      DATA IMGI /(0.,1.)/
C----------------------------------------------------------------------C
      DO 1 I=1,4
        IF( ETA(I).NE.1. .AND. ETA(I).NE.IMGI ) THEN
          WRITE(6,*) ' ETA is wrong, ETA=',ETA
          STOP
        ENDIF
1     CONTINUE

      PI = ACOS(0.)*2.
      SQRT2 = SQRT(2.)

      COS2W = 1.-SIN2W
      COSW  = SQRT(COS2W)
      SINW  = SQRT(SIN2W)
      TANW  = SINW/COSW
      TAN2W = TANW**2

      GWEAK = SQRT(ALPHE*4.*PI/SIN2W)
      GZ    = GWEAK/COSW

      SINPL = SIN(PHIL)
      SINPR = SIN(PHIR)
      COSPL = COS(PHIL)
      COSPR = COS(PHIR)
      SIN2PL = SINPL**2
      COS2PL = COSPL**2
      SIN2PR = SINPR**2
      COS2PR = COSPR**2

C---
C Neutralino-Chargino-W_boson
C---
      DO 10 I=1,4
        GNCW(1,I,1) = ETA(I)*( ON(I,2)*COSPL +ON(I,3)*SINPL/SQRT2 )
     &                *(-GWEAK)
        GNCW(1,I,2) = ETA(I)*(-ON(I,2)*SINPL +ON(I,3)*COSPL/SQRT2 )
     &                *(-GWEAK)
        GNCW(2,I,1) =
     & -GWEAK*CONJG(ETA(I))*( ON(I,2)*COSPR -ON(I,4)*SINPR/SQRT2 )
        GNCW(2,I,2) =
     & -GWEAK*CONJG(ETA(I))*ER*(-ON(I,2)*SINPR -ON(I,4)*COSPR/SQRT2 )
10    CONTINUE

C---
C Chargino-Chargino-Z_boson
C---
      GV(1,1) = 0.25*( SIN2PL+ SIN2PR ) - COS2W
      GA(1,1) = 0.25*( SIN2PL- SIN2PR )

      GV(2,2) = 0.25*( COS2PL+ COS2PR ) - COS2W
      GA(2,2) = 0.25*( COS2PL- COS2PR )

      GV(1,2) = 0.25*( COSPL*SINPL + ER*COSPR*SINPR )
      GA(1,2) = 0.25*( COSPL*SINPL - ER*COSPR*SINPR )
      GV(2,1) = GV(1,2)
      GA(2,1) = GA(1,2)

      DO 20 I=1,2
      DO 20 J=1,2
        GCCZ(1,I,J) = GV(I,J)+ GA(I,J)
        GCCZ(2,I,J) = GV(I,J)- GA(I,J)
20    CONTINUE
      DO 21 I=1,2
      DO 21 J=1,2
      DO 21 K=1,2
21    GCCZ(I,J,K) = -GCCZ(I,J,K)*GZ

C---
C Neutralino-Neutralino-Z_boson
C---
      DO 30 I=1,4
      DO 30 J=1,4
       COM = ON(I,3)*ON(J,3) - ON(I,4)*ON(J,4)
       CMPLX = ETA(I)*CONJG(ETA(J))
       IF( REAL(CMPLX) .EQ. 0 ) THEN
         CV(I,J) = 0.5*IMGI *IMAG(CMPLX) * COM
         CA(I,J) = 0.
       ELSE
         CV(I,J) = 0.
         CA(I,J) = 0.5 *REAL(CMPLX) * COM
       ENDIF
30    CONTINUE
      DO 31 I=1,4
      DO 31 J=1,4
        GNNZ(1,I,J) = CV(I,J)+CA(I,J)
        GNNZ(2,I,J) = CV(I,J)-CA(I,J)
31    CONTINUE
      DO 32 I=1,2
      DO 32 J=1,4
      DO 32 K=1,4
      IF(J.NE.K ) THEN
      GNNZ(I,J,K) = -GNNZ(I,J,K)*GZ
      ELSE
      GNNZ(I,J,K) = -GNNZ(I,J,K)*GZ/2.
      ENDIF
32    CONTINUE


      RETURN
      END
C----------------------------------------------------------------------C
      SUBROUTINE  INOFSF( ALPHE, SIN2W, WM,
     &                    EMAS, UMAS, DMAS,
     &                    TANB, PHIL, PHIR, ER, ON, ETA,
     &                    GCESNL, GCNSEL, GCNSER,
     &                    GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     &                    GNNSNL, GNESEL, GNESER,
     &                    GNUSUL, GNUSUR, GNDSDL, GNDSDR )
C----------------------------------------------------------------------C
C purpose : give couplings for Ino -fermion -Sfermion
C Input  : ALPHE  e**2/4/pi
C          SIN2W (real*4)  weak mixing angle
C          WM    (real*4)  W-boson mass
C          EMAS, UMAS, DMAS (real*4) fermion mass
C               ( charged-lepton, up-type, down-type )
C          TANB  (real*4)  SUSY parameter tan(beta)=v2/v1
C          PHIL, PHIR      chargino mix. angle
C          ER              chargino mix. phase
C          ON(i,j) (real*4)(i,j=1..4)diagonalizing matrix for chargino
C          ETA(i)  (complex*8) (i=1..4)    phase factor
C Output :
C    GCESNL(i,j) (i=1..2, j=1..2 ) / chargino(j)-electron-Sneutrino_L
C    GCNSEL(i,j) (i=1..2, j=1..2 ) / chargino(j)-neutrino-Selectron_L
C    GCNSER(i,j) (i=1..2, j=1..2 ) / chargino(j)-neutrino-Selectron_R
C    GCDSUL(i,j) (i=1..2, j=1..2 ) / chargino(j)-d_quark-Su_quark_L
C    GCDSUR(i,j) (i=1..2, j=1..2 ) / chargino(j)-d_quark-Su_quark_R
C    GCUSDL(i,j) (i=1..2, j=1..2 ) / chargino(j)-u_quark-Sd_quark_L
C    GCUSDR(i,j) (i=1..2, j=1..2 ) / chargino(j)-u_quark-Sd_quark_R
C    GNNSNL(i,j) (i=1..2, j=1..4 ) / neutralino(j)-neutrino-Sneutrino_L
C    GNESEL(i,j) (i=1..2, j=1..4 ) / neutralino(j)-electron-Selectron_L
C    GNESER(i,j) (i=1..2, j=1..4 ) / neutralino(j)-electron-Selectron_R
C    GNUSEL(i,j) (i=1..2, j=1..4 ) / neutralino(j)-u_quark-Su_quark_L
C    GNUSER(i,j) (i=1..2, j=1..4 ) / neutralino(j)-u_quark-Su_quark_R
C    GNDSEL(i,j) (i=1..2, j=1..4 ) / neutralino(j)-u_quark-Su_quark_L
C    GNDSER(i,j) (i=1..2, j=1..4 ) / neutralino(j)-u_quark-Su_quark_R
C              i=1/left-handed-coupling,  i=2/right-handed
C----------------------------------------------------------------------C
      REAL*4 ON(4,4)
      COMPLEX*8 ETA(4)
      COMPLEX*8 GCESNL(2,2), GCNSEL(2,2), GCNSER(2,2),
     &          GCUSDL(2,2), GCUSDR(2,2), GCDSUL(2,2), GCDSUR(2,2)
      COMPLEX*8 GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     &          GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)
C----------------------------------------------------------------------C
      PI = ACOS(0.)*2.
      SQRT2 = SQRT(2.)
      GWEAK = SQRT(ALPHE*4.*PI/SIN2W)
      COS2W = 1.-SIN2W
      TANW  = SQRT(SIN2W/COS2W)
C  0 < beta < pi
C     BETA  = ATAN2( ABS(TANB), SIGN(1.,TANB) )
C     BETA  = DATAN2( DABS(DBLE(TANB)), DSIGN(1.D0,DBLE(TANB)) )
C     COSB  = COS(BETA)
C     SINB  = SIN(BETA)
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSB  = SIGN( SQRT(COS2B), TANB )
      SIN2B = 1.-COS2B
      SINB  = SQRT(SIN2B)
      COSPL = COS(PHIL)
      SINPL = SIN(PHIL)
      COSPR = COS(PHIR)
      SINPR = SIN(PHIR)

      WMCOSB = WM*COSB
      WMSINB = WM*SINB
      WMSQR2 = WM*SQRT(2.)
      WSQ2CB = WMSQR2*COSB
      WSQ2SB = WMSQR2*SINB

C---
C Chargino-fermion-Sfermion
C---
C chargino-electron-Sneutrino
      GCESNL(1,1) = -GWEAK*COSPR
      GCESNL(2,1) =  GWEAK*EMAS*SINPL/WSQ2CB
      GCESNL(1,2) =  GWEAK*ER*SINPR
      GCESNL(2,2) =  GWEAK*EMAS*COSPL/WSQ2CB
C chargino-neutrino-Selectron_L
      GCNSEL(1,1) = -GWEAK*COSPL
      GCNSEL(2,1) =  0.
      GCNSEL(1,2) =  GWEAK*SINPL
      GCNSEL(2,2) =  0.
C chargino-neutrino-Selectron_R
      GCNSER(1,1) =  GWEAK*EMAS*SINPL/WSQ2CB
      GCNSER(2,1) =  0.
      GCNSER(1,2) =  GWEAK*EMAS*COSPL/WSQ2CB
      GCNSER(2,2) =  0.
C chargino-d_quark-Su_quark_L
      GCDSUL(1,1) = -GWEAK*COSPR
      GCDSUL(2,1) =  GWEAK*DMAS*SINPL/WSQ2CB
      GCDSUL(1,2) =  GWEAK*ER*SINPR
      GCDSUL(2,2) =  GWEAK*DMAS*COSPL/WSQ2CB
C chargino-d_quark-Su_quark_R
      GCDSUR(1,1) =  GWEAK*UMAS*SINPR/WSQ2SB
      GCDSUR(2,1) =  0.
      GCDSUR(1,2) =  GWEAK*UMAS*ER*COSPR/WSQ2SB
      GCDSUR(2,2) =  0.
C chargino-u_quark-Sd_quark_L
      GCUSDL(1,1) = -GWEAK*COSPL
      GCUSDL(2,1) =  GWEAK*UMAS*SINPR/WSQ2SB
      GCUSDL(1,2) =  GWEAK*SINPL
      GCUSDL(2,2) =  GWEAK*UMAS*ER*COSPR/WSQ2SB
C chargino-u_quark-Sd_quark_R
      GCUSDR(1,1) =  GWEAK*DMAS*SINPL/WSQ2CB
      GCUSDR(2,1) =  0.
      GCUSDR(1,2) =  GWEAK*DMAS*COSPL/WSQ2CB
      GCUSDR(2,2) =  0.

C---
C Neutralino-fermion-Sfermion
C---
      UCHRG= 2./3.
      DCHRG=-1./3.
      DO 10 I=1,4
        GNNSNL(1,I) = CONJG(ETA(I))*(ON(I,2)-ON(I,1)*TANW)
C       GNNSNL(1,I) = CONJG(ETA(I))*(ON(I,2))
        GNNSNL(2,I) = 0.
        GNESEL(1,I) =-CONJG(ETA(I))*(ON(I,2)+ON(I,1)*TANW)
        GNESEL(2,I) = ETA(I)*EMAS*ON(I,3)/WMCOSB
        GNESER(1,I) = CONJG(ETA(I))*EMAS*ON(I,3)/WMCOSB
        GNESER(2,I) = 2.*ETA(I)*ON(I,1)*TANW

c        GNUSUL(1,I) = CONJG(ETA(I))
c     &               *( ON(I,2)+(2.*UCHRG-1.)*ON(I,1)*TANW )
c        GNUSUL(2,I) = ETA(I)*UMAS*ON(I,3)/WMSINB
c        GNUSUR(1,I) = CONJG(ETA(I))*UMAS*ON(I,3)/WMSINB
c        GNUSUR(2,I) =-2.*ETA(I)*UCHRG*TANW*ON(I,1)

C*** new *****

        GNUSUL(1,I) = CONJG(ETA(I))
     &               *( ON(I,2)+(2.*UCHRG-1.)*ON(I,1)*TANW )
        GNUSUL(2,I) = ETA(I)*UMAS*ON(I,4)/WMSINB
        GNUSUR(1,I) = CONJG(ETA(I))*UMAS*ON(I,4)/WMSINB
        GNUSUR(2,I) =-2.*ETA(I)*UCHRG*TANW*ON(I,1)


        GNDSDL(1,I) = CONJG(ETA(I))
     &               *(-ON(I,2)+(2.*DCHRG+1.)*ON(I,1)*TANW )
        GNDSDL(2,I) = ETA(I)*DMAS*ON(I,3)/WMCOSB
        GNDSDR(1,I) = CONJG(ETA(I))*DMAS*ON(I,3)/WMCOSB
        GNDSDR(2,I) =-2.*ETA(I)*DCHRG*TANW*ON(I,1)
10    CONTINUE

      GWSQR2 = GWEAK/SQRT2
      DO 20 I=1,2
      DO 20 J=1,4
        GNNSNL(I,J) = -GWSQR2*GNNSNL(I,J)
        GNESEL(I,J) = -GWSQR2*GNESEL(I,J)
        GNESER(I,J) = -GWSQR2*GNESER(I,J)
        GNUSUL(I,J) = -GWSQR2*GNUSUL(I,J)
        GNUSUR(I,J) = -GWSQR2*GNUSUR(I,J)
        GNDSDL(I,J) = -GWSQR2*GNDSDL(I,J)
        GNDSDR(I,J) = -GWSQR2*GNDSDR(I,J)
20    CONTINUE


      RETURN
      END
C----------------------------------------------------------------------C
      SUBROUTINE  HGSGBS( ALPHE, SIN2W, ZM, WM, TANB, TANA,
     &                    GH0WW, GH0ZZ,
     &                    GH0HCW, GH0PHZ, GHHZ, GHHA,
     &                    GHHWW, GH02W2, GH0HWZ, GH0HWA,
     &                    GHHZZ, GH02Z2, GHHZA, GHHAA )
C----------------------------------------------------------------------C
C purpose : give  Higss- Gauge_boson couplings
C inputs  : ALPHE, SIN2W, WM / S.M. parameters
C           TANB / ratio of vev. ( v2/v1 )
C           TANA / tan of Higgs boson mix. angle
C outputs : GH0WW(i) (i=1..2 ) / Higgs_0(i)-W-W coupling
C           GH0ZZ(i) (i=1..2 ) / Higgs_0(i)-Z-Z coupling
C           GH0HCW(i)(i=1..3 ) / Higgs_0(i)-Higgs_(-)- W(dag.)
C                         (i=3 is for pseudoscaler Higgs )
C           GH0PHZ(i)(i=1..2 ) / Higgs_0(i)-P_Higgs_0 -Z
C           GHHZ               / Higgs_(+) -Higgs_(-) -Z
C           GHHA               / Higgs_(+) -Higgs_(-) -A
C           GHHWW              / Higgs_(+) -Higgs_(-) -W-W
C           GH02W2             / Higgs_0(i) -Higgs_0(i) -W-W
C                                ( same for all H_0(i) )
C           GH0HWZ(i)(i=1..3)  / Higgs_0(i) -Higgs_(-) -W(dag.)-Z
C           GH0HWA(i)(i=1..3)  / Higgs_0(i) -Higgs_(-) -W(dag.)-Z
C           GHHZZ              / Higgs_(+) -Higgs_(-) -Z-Z
C           GH02Z2             / Higgs_0(i) -Higgs_0(i) -Z-Z
C                                ( same for all H_0(i) )
C           GHHZA              / Higgs_(+) -Higgs_(-) -Z-A
C           GHHAA              / Higgs_(+) -Higgs_(-) -A-A
C  17/JUL/95   ZM is added as an input argument.  T.T
C  18/MAR/96   Revised. S.G.
C----------------------------------------------------------------------C
      COMPLEX*8 GH0WW(2), GH0ZZ(2), GH0HCW(3), GH0PHZ(2),
     &          GH0HWZ(3), GH0HWA(3)
      COMPLEX*8 IMGI
      DATA IMGI/(0.,1.)/
C----------------------------------------------------------------------C
      PI = ACOS(0.)*2.
      GWEAK = SQRT(ALPHE*4.*PI/SIN2W)
      EC    = SQRT(ALPHE*4.*PI)

      COS2W = 1.-SIN2W
      COSW  = SQRT(COS2W)
CTT 17/Jul/95      ZM    = WM/COSW
      GZ    = GWEAK/COSW

C     ALPHA = ATAN2( ABS(TANA), SIGN(1.,TANA) )
C     ALPHA = DATAN2( DABS(DBLE(TANA)), DSIGN(1.D0,DBLE(TANA)) )
      TAN2A = TANA**2
      COS2A = 1./(1.+TAN2A)
      COSA  = SIGN( SQRT(COS2A), TANA )
      SIN2A = 1.-COS2A
      SINA  = SQRT(SIN2A)
C     BETA  = ATAN2( ABS(TANB), SIGN(1.,TANB) )
C     BETA  = DATAN2( DABS(DBLE(TANB)), DSIGN(1.D0,DBLE(TANB)) )
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSB  = SIGN( SQRT(COS2B), TANB )
      SIN2B = 1.-COS2B
      SINB  = SQRT(SIN2B)

C     DELT = ALPHA-BETA
C     COSD = COS(DELT)
C     SIND = SIN(DELT)
      COSD = COSA*COSB + SINA*SINB

C      SIND = SINA*COSB + COSA*SINB
C***** new *****

      SIND = SINA*COSB - COSA*SINB

      GH0WW(1) = -GWEAK*SIND *WM
      GH0WW(2) =  GWEAK*COSD *WM

      GH0ZZ(1) = -0.5*GZ*SIND *ZM
      GH0ZZ(2) =  0.5*GZ*COSD *ZM

      GH0HCW(1) = 0.5*GWEAK*COSD *IMGI
      GH0HCW(2) = 0.5*GWEAK*SIND *IMGI
      GH0HCW(3) =-0.5*GWEAK

      GH0PHZ(1) =-0.5*GZ*COSD
      GH0PHZ(2) =-0.5*GZ*SIND

      GHHZ      = -(-0.5+SIN2W)*GZ *IMGI
      GHHA      = EC *IMGI

      GHHWW     = 0.5*GWEAK**2
      GH02W2    = 0.25*GWEAK**2
      GH0HWZ(1) =-0.5*GWEAK*GZ*SIN2W*COSD
      GH0HWZ(2) =-0.5*GWEAK*GZ*SIN2W*SIND
      GH0HWZ(3) =-0.5*GWEAK*GZ*SIN2W *IMGI
      GH0HWA(1) = 0.5*GWEAK*EC*COSD
      GH0HWA(2) = 0.5*GWEAK*EC*SIND
      GH0HWA(3) = 0.5*GWEAK*EC *IMGI
      GHHZZ     = GZ**2*(-0.5+SIN2W)**2
      GH02Z2    = GZ**2/8.
      GHHZA     =-2.*GZ*EC*(-0.5+SIN2W)
      GHHAA     = EC**2


      RETURN
      END
C----------------------------------------------------------------------C
      SUBROUTINE  INOHGS( ALPHE, SIN2W, TANB, TANA,
     &                    PHIL, PHIR, ER, ON, ETA,
     &                    GCCH, GCNH, GNNH )
C----------------------------------------------------------------------C
C purpose : give couplings for Ino-Higgs couplings
C inputs  : ALPHE, SIN2W     / S.M. parameters
C           TANB             / vev. ratio
C           TANA             / tan of Higgs mix. angle
C           PHIL, PHIR, ER   / chargino mix. angles & phase
C           ON(4,4), ETA(4)  / neutralino mix. angles & phase
C outputs : GCCH(i,j,k,l) (i,j,k=1..2 ) C.ino(j)-C.ino(k)-H_0(l)
C                         (   l=1..3  )
C           GCNH(i,j,k) (i,j=1..2,  k=1..4 ) C.ino(j)-N.ino(k)-H_(-)
C           GNNH(i,j,k,l)(i=1..2, j,k=1..4 ) N.ino(j)-N.ino(k)-H_0(l)
C                         (   l=1..3  )
C              i=1/left-handed,  i=2/right-handed
C----------------------------------------------------------------------C
      REAL*4    ON(4,4)
      COMPLEX*8 ETA(4)
      COMPLEX*8 GSCCH(2,2), GCCH(2,2,2,3), GPCCP(2)
      COMPLEX*8 GCNH(2,2,4), GNNH(2,4,4,3), SS(3),PP(3), SS0, PP0
      COMPLEX*8 IMGI, CMPLX
      DATA IMGI /(0.,1.)/
C----------------------------------------------------------------------C
      PI = ACOS(0.)*2.
      SQRT2 = SQRT(2.)
      GWEAK = SQRT(ALPHE*4.*PI/SIN2W)
      COS2W = 1.-SIN2W
      SINW  = SQRT(SIN2W)
      COSW  = SQRT(COS2W)
      TANW  = SINW/COSW
      GZ    = GWEAK/COSW

C  0 < beta < pi
C     BETA  = ATAN2( ABS(TANB), SIGN(1.,TANB) )
C     BETA  = DATAN2( DABS(DBLE(TANB)), DSIGN(1.D0,DBLE(TANB)) )
C     COSB  = COS(BETA)
C     SINB  = SIN(BETA)
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSB  = SIGN( SQRT(COS2B), TANB )
      SIN2B = 1.-COS2B
      SINB  = SQRT(SIN2B)
C     ALPHA = ATAN2( ABS(TANA), SIGN(1.,TANA) )
C     ALPHA = DATAN2( DABS(DBLE(TANA)), DSIGN(1.D0,DBLE(TANA)) )
C     SINA  = SIN(ALPHA)
C     COSA  = COS(ALPHA)
      TAN2A = TANA**2
      COS2A = 1./(1.+TAN2A)
      COSA  = SIGN( SQRT(COS2A), TANA )
      SIN2A = 1.-COS2A
      SINA  = SQRT(SIN2A)

      COSPL = COS(PHIL)
      SINPL = SIN(PHIL)
      COSPR = COS(PHIR)
      SINPR = SIN(PHIR)

C---
C Chargino-Chargino-Higgs
C---
C  GSCCH(i,j) : i=chargino(i), j=Higgs(j)
      GSCCH(1,1) = -SINA*SINPL*COSPR +COSA*COSPL*SINPR
      GSCCH(2,1) =  ER*(SINA*COSPL*SINPR -COSA*SINPL*COSPR)
      GSCCH(1,2) =  COSA*SINPL*COSPR +SINA*COSPL*SINPR
      GSCCH(2,2) =  ER*(-COSA*COSPL*SINPR -SINA*SINPL*COSPR)
      GCCH(1,1,1,1) = GSCCH(1,1)
      GCCH(2,1,1,1) = GSCCH(1,1)
      GCCH(1,2,2,1) = GSCCH(2,1)
      GCCH(2,2,2,1) = GSCCH(2,1)
      GCCH(1,1,1,2) = GSCCH(1,2)
      GCCH(2,1,1,2) = GSCCH(1,2)
      GCCH(1,2,2,2) = GSCCH(2,2)
      GCCH(2,2,2,2) = GSCCH(2,2)

      GPCCP(1) = SINB*SINPL*COSPR +COSB*COSPL*SINPR
      GPCCP(2) = ER*(-SINB*COSPL*SINPR -COSB*SINPL*COSPR)
      GCCH(1,1,1,3) =-GPCCP(1)
      GCCH(2,1,1,3) = GPCCP(1)
      GCCH(1,2,2,3) =-GPCCP(2)
      GCCH(2,2,2,3) = GPCCP(2)

      GCCH(1,1,2,1) = -SINA*COSPL*COSPR -COSA*SINPL*SINPR
      GCCH(2,1,2,1) = ER*(SINA*SINPL*SINPR +COSA*COSPL*COSPR)
      GCCH(1,2,1,1) = GCCH(2,1,2,1)
      GCCH(2,2,1,1) = GCCH(1,1,2,1)
      GCCH(1,1,2,2) = COSA*COSPL*COSPR -SINA*SINPL*SINPR
      GCCH(2,1,2,2) = ER*(-COSA*SINPL*SINPR +SINA*COSPL*COSPR)
      GCCH(1,2,1,2) = GCCH(2,1,2,2)
      GCCH(2,2,1,2) = GCCH(1,1,2,2)

      GCCH(1,1,2,3) = -SINB*COSPL*COSPR +COSB*SINPL*SINPR
      GCCH(2,1,2,3) = ER*(-SINB*SINPL*SINPR +COSB*COSPL*COSPR)
      GCCH(1,2,1,3) = -GCCH(2,1,2,3)
      GCCH(2,2,1,3) = -GCCH(1,1,2,3)

      DO 5 I=1,2
      DO 5 J=1,2
      DO 5 K=1,2
      DO 5 L=1,3
5     GCCH(I,J,K,L) = -GCCH(I,J,K,L) *GWEAK/SQRT2

      DO 6 I=1,2
      DO 6 J=1,2
      DO 6 K=1,2
6     GCCH(I,J,K,3) =  GCCH(I,J,K,3) *IMGI

C---
C Chargino-Neutralino-Higgs
C---
      DO 10 I=1,4
       ONI21T = (ON(I,2)+ON(I,1)*TANW)/SQRT2
       GCNH(1,1,I) = COSB*( COSPR*ON(I,4) +SINPR*ONI21T )*CONJG(ETA(I))
       GCNH(2,1,I) = SINB*( COSPL*ON(I,3) -SINPL*ONI21T ) * ETA(I)
       GCNH(1,2,I) = COSB*ER*(-SINPR*ON(I,4)
     &               +COSPR*ONI21T )*CONJG(ETA(I))
       GCNH(2,2,I) = SINB*(-SINPL*ON(I,3) -COSPL*ONI21T )*ETA(I)
10    CONTINUE

      DO 15 I=1,2
      DO 15 J=1,2
      DO 15 K=1,4
15    GCNH(I,J,K) = GCNH(I,J,K) * (-GWEAK)

C---
C Neutralino-Neutralino-Higgs
C---
      DO 30 I=1,4
       ONI21T = (ON(I,2)-ON(I,1)*TANW)
       SS0= ETA(I)**2* ONI21T*( -SINA*ON(I,3)-COSA*ON(I,4) )
       GNNH(1,I,I,1) = SS0
       GNNH(2,I,I,1) = SS0
       SS0= ETA(I)**2* ONI21T*( COSA*ON(I,3)-SINA*ON(I,4) )
       GNNH(1,I,I,2) = SS0
       GNNH(2,I,I,2) = SS0
       PP0=-ETA(I)**2* ONI21T*(-SINB*ON(I,3)+COSB*ON(I,4) )
       GNNH(1,I,I,3) =-PP0*IMGI
       GNNH(2,I,I,3) = PP0*IMGI
30    CONTINUE

      DO 40 I=1,4
      DO 40 J=1,4
       IF(I.EQ.J) GO TO 40
       CMPLX  = ETA(I)*ETA(J)
       ONI21T = (ON(I,2)-ON(I,1)*TANW)
       ONJ21T = (ON(J,2)-ON(J,1)*TANW)
       FF1=   ONI21T*(-SINA*ON(J,3)-COSA*ON(J,4))
     &       +ONJ21T*(-SINA*ON(I,3)-COSA*ON(I,4))
       FF2=   ONI21T*( COSA*ON(J,3)-SINA*ON(J,4))
     &       +ONJ21T*( COSA*ON(I,3)-SINA*ON(I,4))
       FF3=   ONI21T*(-SINB*ON(J,3)+COSB*ON(J,4))
     &       +ONJ21T*(-SINB*ON(I,3)+COSB*ON(I,4))
       IF( REAL(CMPLX) .NE.0 ) THEN
        SS(1) = REAL(CMPLX)*FF1
        PP(1) = 0.
        SS(2) = REAL(CMPLX)*FF2
        PP(2) = 0.
        SS(3) = 0.
        PP(3) =-REAL(CMPLX)*FF3
       ELSE
        SS(1) = 0.
        PP(1) = IMAG(CMPLX)*FF1
        SS(2) = 0.
        PP(2) = IMAG(CMPLX)*FF2
        SS(3) = IMAG(CMPLX)*FF3
        PP(3) = 0.
       ENDIF
       GNNH(1,I,J,1) = SS(1) -IMGI*PP(1)
       GNNH(2,I,J,1) = SS(1) +IMGI*PP(1)
       GNNH(1,I,J,2) = SS(2) -IMGI*PP(2)
       GNNH(2,I,J,2) = SS(2) +IMGI*PP(2)
       GNNH(1,I,J,3) = SS(3) -IMGI*PP(3)
       GNNH(2,I,J,3) = SS(3) +IMGI*PP(3)
40    CONTINUE

      DO 50 I=1,2
      DO 50 J=1,4
      DO 50 K=1,4
      DO 50 L=1,3
50    GNNH(I,J,K,L) = GNNH(I,J,K,L)*(-GWEAK)/2.


      RETURN
      END
CDECK  ID>, FULCXX.
CC**********************************************************************
C*
C*===============================----===
C* Subroutine FULCXX(IDP,IHEL,PV,AMP2)
C*===============================----===
C*
C* (Purpose)
C*    Calculate amplitude for e+ + e- ---> XC+ + XC-.
C* (Inputs)
C*       IDP(1,i) : (I*4) : generation number.
C*          (2,i) : (I*4) : T3 + 1.5.
C*          (3,i) : (I*4) : l/q flag; (1,2) = (l,q).
C*       IHEL( i) : (I*4) : helicity combination.
C*       PV(*, i) : (R*4) : 4-momentum of i-th particle.
C*                        : i =  1 for  e-.
C*                        :   =  2 for  e+.
C*                        :   =  3 for  LSP from XC+.
C*                        :   =  4 for  fu from XC+
C*                        :   =  5 for  fdb from XC+.
C*                        :   =  6 for  LSP from XC-.
C*                        :   =  7 for  fub  from XC-
C*                        :   =  8 for  fd from XC-.
C* (Outputs)
C*    AMP2(0)     : (R*4) : (amplitude sum)**2.
C*        (1)     : (R*4) : photon.
C*        (2)     : (R*4) : Z.
C*        (3)     : (R*4) : t-channel snu-exchange.
C* (Relation)
C*    Invokes AMPCXX and subroutines in HELAS.LOAD.
C*
CC**********************************************************************

      SUBROUTINE FULCXX(IDP,IHEL,PV,AMP2)

      IMPLICIT   REAL*4  ( A-H, O-Z )

      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      COMMON /SSCONS/ xAM0, xAMU, xAM2, xTANB, xAMA, xRELAX
      REAL*4          xAM0, xAMU, xAM2, xTANB, xAMA, xRELAX
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      COMMON /SMCUPL/ GAL, GAU, GAD, GWF, GZN, GZL,
     .                GZU, GZD, G1, GW, GWWZ, GWWA
C--
      REAL*4          GAL(2), GAU(2), GAD(2), GWF(2), GZN(2), GZL(2),
     .                GZU(2), GZD(2), G1(2), GW, GWWZ, GWWA
C--
      COMMON /SSCUPL/ GNCW, GCCZ, GNNZ,
     .                GCESNL, GCNSEL, GCNSER,
     .                GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     .                GNNSNL, GNESEL, GNESER,
     .                GNUSUL, GNUSUR, GNDSDL, GNDSDR
C--
      COMPLEX*8  GNCW(2,4,2), GCCZ(2,2,2), GNNZ(2,4,4)
      COMPLEX*8  GCESNL(2,2), GCNSEL(2,2), GCNSER(2,2),
     .           GCUSDL(2,2), GCUSDR(2,2), GCDSUL(2,2), GCDSUR(2,2)
      COMPLEX*8  GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     .           GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)
C--


      INTEGER*4  IDP(3,*), IHEL(*)
      REAL   *4  PV(0:3,*), AMP2(0:3)
C--
      COMPLEX*8  SPINOR(6,20), EIN(6), EOT(6), XIN(6), XOT(6),
     .           WVCT(6), SCAL(3), AMP(0:3)
      COMPLEX*8  GWX(2,2), GSNX(2,2), GCFSF(2,2,2), GNFSF(2,2,2)
      REAL   *4  GAX(2), GZX(2), AMSSF(2,2), GAMSF(2,2)
      DATA NCALL / 0 /
C
C========< Entry Point >================================================
C
C--
C  Prepare coupling constants.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL     = 1
         AMSE      = AMF(1,2,1)
         AMSW      = AMW
         GAMW      = GMWTOT
         AMSZ      = AMZ
         GAMZ      = GMZTOT
C>>>
         QCD       =  1 + xALFS/xPI
C        QCD       =  1
C>>>
         GWX(1,1)  = CONJG(GNCW(1,1,1))
         GWX(2,1)  = CONJG(GNCW(2,1,1))
         GWX(1,2)  = GNCW(1,1,1)
         GWX(2,2)  = GNCW(2,1,1)
         GAX(1)    = GAL(1)
         GAX(2)    = GAL(2)
         GZX(1)    = GCCZ(1,1,1)
         GZX(2)    = GCCZ(2,1,1)
         GSNX(1,1) = GCESNL(1,1)
         GSNX(2,1) = GCESNL(2,1)
         GSNX(1,2) = CONJG(GCESNL(2,1))
         GSNX(2,2) = CONJG(GCESNL(1,1))
      ENDIF
C--
C  Set masses and couplings depending on final states.
C--
      AM4 = AMF(IDP(1,4),IDP(2,4),IDP(3,4))
      AM5 = AMF(IDP(1,5),IDP(2,5),IDP(3,5))
      AM7 = AMF(IDP(1,7),IDP(2,7),IDP(3,7))
      AM8 = AMF(IDP(1,8),IDP(2,8),IDP(3,8))
C--
      IF ( IDP(3,4).EQ.1 ) THEN
         AMSSF(  1,1) = SFM(1)
         GAMSF(  1,1) = GMSF(1)
         GCFSF(1,1,1) = GCESNL(1,1)
         GCFSF(2,1,1) = GCESNL(2,1)
         GNFSF(1,1,1) = CONJG(GNNSNL(2,1))
         GNFSF(2,1,1) = CONJG(GNNSNL(1,1))
         AMSSF(  2,1) = SFM(2)
         GAMSF(  2,1) = GMSF(2)
         GCFSF(1,2,1) = CONJG(GCNSEL(2,1))
         GCFSF(2,2,1) = CONJG(GCNSEL(1,1))
         GNFSF(1,2,1) = GNESEL(1,1)
         GNFSF(2,2,1) = GNESEL(2,1)
      ELSE
         AMSSF(  1,1) = SFM(4)
         GAMSF(  1,1) = GMSF(4)
         GCFSF(1,1,1) = GCDSUL(1,1)
         GCFSF(2,1,1) = GCDSUL(2,1)
         GNFSF(1,1,1) = CONJG(GNUSUL(2,1))
         GNFSF(2,1,1) = CONJG(GNUSUL(1,1))
         AMSSF(  2,1) = SFM(6)
         GAMSF(  2,1) = GMSF(6)
         GCFSF(1,2,1) = CONJG(GCUSDL(2,1))
         GCFSF(2,2,1) = CONJG(GCUSDL(1,1))
         GNFSF(1,2,1) = GNDSDL(1,1)
         GNFSF(2,2,1) = GNDSDL(2,1)
      ENDIF
C--
      IF ( IDP(3,7).EQ.1 ) THEN
         AMSSF(  1,2) = SFM(1)
         GAMSF(  1,2) = GMSF(1)
         GCFSF(1,1,2) = CONJG(GCESNL(2,1))
         GCFSF(2,1,2) = CONJG(GCESNL(1,1))
         GNFSF(1,1,2) = GNNSNL(1,1)
         GNFSF(2,1,2) = GNNSNL(2,1)
         AMSSF(  2,2) = SFM(2)
         GAMSF(  2,2) = GMSF(2)
         GCFSF(1,2,2) = GCNSEL(1,1)
         GCFSF(2,2,2) = GCNSEL(2,1)
         GNFSF(1,2,2) = CONJG(GNESEL(2,1))
         GNFSF(2,2,2) = CONJG(GNESEL(1,1))
      ELSE
         AMSSF(  1,2) = SFM(4)
         GAMSF(  1,2) = GMSF(4)
         GCFSF(1,1,2) = CONJG(GCDSUL(2,1))
         GCFSF(2,1,2) = CONJG(GCDSUL(1,1))
         GNFSF(1,1,2) = GNUSUL(1,1)
         GNFSF(2,1,2) = GNUSUL(2,1)
         AMSSF(  2,2) = SFM(6)
         GAMSF(  2,2) = GMSF(6)
         GCFSF(1,2,2) = GCUSDL(1,1)
         GCFSF(2,2,2) = GCUSDL(2,1)
         GNFSF(1,2,2) = CONJG(GNDSDL(2,1))
         GNFSF(2,2,2) = CONJG(GNDSDL(1,1))
      ENDIF
C--
C  Prepare spinors for external lines.
C--
      CALL IXXXXX(PV(0,1),AMSE  ,IHEL(1),+1,EIN)
      CALL OXXXXX(PV(0,2),AMSE  ,IHEL(2),-1,EOT)
C--
      CALL IXXXXX(PV(0,3),SZM(1),IHEL(3),-1,SPINOR(1, 3))
      CALL OXXXXX(PV(0,4),AM4   ,IHEL(4),+1,SPINOR(1, 4))
      CALL IXXXXX(PV(0,4),AM4   ,IHEL(4),-1,SPINOR(1,14))
      CALL IXXXXX(PV(0,5),AM5   ,IHEL(5),-1,SPINOR(1, 5))
      CALL OXXXXX(PV(0,5),AM5   ,IHEL(5),+1,SPINOR(1,15))
C--
      CALL OXXXXX(PV(0,6),SZM(1),IHEL(6),+1,SPINOR(1, 6))
      CALL IXXXXX(PV(0,7),AM7   ,IHEL(7),-1,SPINOR(1, 7))
      CALL OXXXXX(PV(0,7),AM7   ,IHEL(7),+1,SPINOR(1,17))
      CALL OXXXXX(PV(0,8),AM8   ,IHEL(8),+1,SPINOR(1, 8))
      CALL IXXXXX(PV(0,8),AM8   ,IHEL(8),-1,SPINOR(1,18))
C--
C  Prepare spinors for X+.
C--
      CALL JIOXXX(SPINOR(1,5),SPINOR(1,4),GWF,AMSW,GAMW,WVCT)
      CALL FVIXXC(SPINOR(1,3),WVCT,GWX(1,1),SWM(1),GMSW(1),XIN)
CNOXCTOW       XIN(1) = 0
CNOXCTOW       XIN(2) = 0
CNOXCTOW       XIN(3) = 0
CNOXCTOW       XIN(4) = 0
C--
      CALL HIOXXX(SPINOR(1,3),SPINOR(1, 4),GNFSF(1,1,1),
     .            AMSSF(1,1),GAMSF(1,1),SCAL)
      CALL FSIXXX(SPINOR(1, 5),SCAL,GCFSF(1,1,1),
     .            SWM(1),GMSW(1),SPINOR(1, 1))
CNOUSF       SPINOR(1,1) = 0
CNOUSF       SPINOR(2,1) = 0
CNOUSF       SPINOR(3,1) = 0
CNOUSF       SPINOR(4,1) = 0
C--
      CALL HIOXXX(SPINOR(1,3),SPINOR(1,15),GNFSF(1,2,1),
     .            AMSSF(2,1),GAMSF(2,1),SCAL)
      CALL FSIXXX(SPINOR(1,14),SCAL,GCFSF(1,2,1),
     .            SWM(1),GMSW(1),SPINOR(1,11))
CNODSF       SPINOR(1,11) = 0
CNODSF       SPINOR(2,11) = 0
CNODSF       SPINOR(3,11) = 0
CNODSF       SPINOR(4,11) = 0
C--
      XIN(1) = XIN(1) - SPINOR(1,1) + SPINOR(1,11)
      XIN(2) = XIN(2) - SPINOR(2,1) + SPINOR(2,11)
      XIN(3) = XIN(3) - SPINOR(3,1) + SPINOR(3,11)
      XIN(4) = XIN(4) - SPINOR(4,1) + SPINOR(4,11)
C--
C  Prepare spinors for X-.
C--
      CALL JIOXXX(SPINOR(1,7),SPINOR(1,8),GWF,AMSW,GAMW,WVCT)
      CALL FVOXXC(SPINOR(1,6),WVCT,GWX(1,2),SWM(1),GMSW(1),XOT)
CNOXCTOW       XOT(1) = 0
CNOXCTOW       XOT(2) = 0
CNOXCTOW       XOT(3) = 0
CNOXCTOW       XOT(4) = 0
C--
      CALL HIOXXX(SPINOR(1, 7),SPINOR(1,6),GNFSF(1,1,2),
     .            AMSSF(1,2),GAMSF(1,2),SCAL)
      CALL FSOXXX(SPINOR(1, 8),SCAL,GCFSF(1,1,2),
     .            SWM(1),GMSW(1),SPINOR(1, 2))
CNOUSF       SPINOR(1,2) = 0
CNOUSF       SPINOR(2,2) = 0
CNOUSF       SPINOR(3,2) = 0
CNOUSF       SPINOR(4,2) = 0
C--
      CALL HIOXXX(SPINOR(1,18),SPINOR(1,6),GNFSF(1,2,2),
     .            AMSSF(2,2),GAMSF(2,2),SCAL)
      CALL FSOXXX(SPINOR(1,17),SCAL,GCFSF(1,2,2),
     .            SWM(1),GMSW(1),SPINOR(1,12))
CNODSF       SPINOR(1,12) = 0
CNODSF       SPINOR(2,12) = 0
CNODSF       SPINOR(3,12) = 0
CNODSF       SPINOR(4,12) = 0
C--
      XOT(1) = XOT(1) - SPINOR(1,2) + SPINOR(1,12)
      XOT(2) = XOT(2) - SPINOR(2,2) + SPINOR(2,12)
      XOT(3) = XOT(3) - SPINOR(3,2) + SPINOR(3,12)
      XOT(4) = XOT(4) - SPINOR(4,2) + SPINOR(4,12)
C--
C  Calculate amplitude**2.
C--
      CALL AMPCXX (GAL,GAX,GZL,GZX,GSNX,SFM(1),GMSF(1),AMSZ,GAMZ,
     .             EIN,EOT,XIN,XOT,AMP)
      FACT    = (3*QCD)**(IDP(3,4)+IDP(3,7)-2)
     .          * VKM( IDP(1,4), IDP(1,5), IDP(3,4) )**2
     .          * VKM( IDP(1,7), IDP(1,8), IDP(3,7) )**2
      AMP2(0) = FACT*ABS(AMP(0))**2
      AMP2(1) = FACT*ABS(AMP(1))**2
      AMP2(2) = FACT*ABS(AMP(2))**2
      AMP2(3) = FACT*ABS(AMP(3))**2
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, GTSFMS.
C*                           This version does not take into account
C*                           L-R mixings.
C*

      SUBROUTINE GTSFMS(AM0,AM2,AMU,TNB,ALF,S2W,AMZ,SFM)

      IMPLICIT    REAL*4 ( A-H, O-Z )
      REAL*4      AM0, AM2, AMU, TNB, ALF, S2W, SFM(7)
      REAL*4      ALFT(3), B(3), F(3), C(2,7), AMSF2(7)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Constants at Q**2 = MZ**2
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         AMU   = AMU
         x2PI  = 2*ACOS(-1.)
         x4PI  = 2*x2PI
         NGN   = 3
C--MSSM
         B(1)  = (10/3.)*NGN + 1
         B(2)  = 2*NGN - 6 + 1
         B(3)  = 2*NGN - 9
      ENDIF
C--
C  Constants at Q**2 = MZ**2
C--
      AMZCS = AMZ*AMZ*(TNB-1)*(TNB+1)/(TNB*TNB+1)
C--
C  MX.
C--
      TEE   = (x4PI/ALF)*(3-8*S2W)/(3*B(1)-5*B(2))
      AMX   = AMZ*EXP(TEE/2)
      ALFWI = S2W/ALF
      ALFXI = ALFWI - (B(2)/x4PI)*TEE
      ALFSI = ALFXI + (B(3)/x4PI)*TEE
C--
C     PRINT *, ' 1/ALF(MZ) = ', 1/ALF
C     PRINT *, ' SIN2W     = ', S2W
C     PRINT *, ' AMZ       = ', AMZ, ' GeV'
C     PRINT *, ' NGN       = ', NGN
C     PRINT *, ' AMX       = ', AMX
C     PRINT *, ' ALFS      = ', 1/ALFSI
C     PRINT *, ' --- '
C--
C  Alpha_tilde.
C--
      ALFT(3) = 1/ALFXI/x4PI
      ALFT(2) = ALFT(3)
      ALFT(1) = 3/5.*ALFT(2)
C--
C  f_i.
C--
      DO 10 I = 1, 3
         F(I) = 1/B(I)/ALFT(I) * ( 1 - 1/(1+B(I)*ALFT(I)*TEE)**2 )
10    CONTINUE
C--
C  M_i**2 = M_0**2 + C(1,i)*M**2 + C(2,i)*cos(2*beta)*M_Z**2.
C--
      C(1,1) = 2*( 4/3.*ALFT(3)*F(3) + 3/4.*ALFT(2)*F(2)
     .                               + 1/36.*ALFT(1)*F(1) )
      C(1,2) = C(1,1)
      C(1,3) = 2*( 4/3.*ALFT(3)*F(3) + 4/9.*ALFT(1)*F(1) )
      C(1,4) = 2*( 4/3.*ALFT(3)*F(3) + 1/9.*ALFT(1)*F(1) )
      C(1,5) = 2*( 3/4.*ALFT(2)*F(2) + 1/4.*ALFT(1)*F(1) )
      C(1,6) = C(1,5)
      C(1,7) = 2*ALFT(1)*F(1)
C--
      C1QSM  = C(1,1) + C(1,2) + C(1,3) + C(1,4)
      C1LSM  = C(1,5) + C(1,6) + C(1,7)
C--
      C(2,1) = -0.5 + 2/3.*S2W
      C(2,2) =  0.5 - 1/3.*S2W
      C(2,3) =      - 2/3.*S2W
      C(2,4) =      + 1/3.*S2W
      C(2,5) =  0.5 -      S2W
      C(2,6) = -0.5
      C(2,7) =      +      S2W
C--
C  Loop over sferminons. AMSF2 is mass**2.
C     AMSF2(I) = (SUL,SDL,SUR,SDR,SEL,SNL,SER)
C--
      AM   = ALFWI/ALFXI*AM2
      AM02 = AM0**2
      DO 20 I = 1, 7
         AMSF2(I) = AM02 + AM*AM*C(1,I) + C(2,I)*AMZCS
20    CONTINUE
C--
      SFM(1) = SQRT(AMSF2(6))
      SFM(2) = SQRT(AMSF2(5))
      SFM(3) = SQRT(AMSF2(7))
      SFM(4) = SQRT(AMSF2(1))
      SFM(5) = SQRT(AMSF2(3))
      SFM(6) = SQRT(AMSF2(2))
      SFM(7) = SQRT(AMSF2(4))
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, INSUSY.
C*     FM (1)     :(R*4): e mass.
C*        (2)     :(R*4): u mass.
C*        (3)     :(R*4): d mass.
C*     SFM(1)     :(R*4): snu_L mass.
C*        (2)     :(R*4): se_L mass.
C*        (3)     :(R*4): se_R mass.
C*        (4)     :(R*4): su_L mass.
C*        (5)     :(R*4): su_R mass.
C*        (6)     :(R*4): sd_L mass.
C*        (7)     :(R*4): sd_R mass.
C*     SWM(1-2)   :(R*4): chargino masses.
C*     SZM(1-4)   :(R*4): neutralino masses.

C 17/JUL/95 Modified to inlclude WM as an input argument. T.T
C 18/MAR/96 Revised and Updated. S.G.

      SUBROUTINE INSUSY(AM0,AMU,AM2,RELAX,TANB,AMA,ALF,ALFS,S2W,ZM,
     .                  WM,FM,
     .                  SFM,SWM,SZM,GMSF,GMSW,GMSZ,
     .                  GNCW,GCCZ,GNNZ,
     .                  GCESNL,GCNSEL,GCNSER,
     .                  GCUSDL,GCUSDR,GCDSUL,GCDSUR,
     .                  GNNSNL,GNESEL,GNESER,
     .                  GNUSUL,GNUSUR,GNDSDL,GNDSDR )

      IMPLICIT   REAL*4  ( A-H, O-Z )
      REAL   *4  AM0, AMU, AM2, TANB, AMA, ALF, ALFS, S2W, ZM, FM(3),
     .           SFM(7), SWM(2), SZM(4), GMSF(7), GMSW(2), GMSZ(4),
     .           RELAX
      COMPLEX*8  GNCW(2,4,2), GCCZ(2,2,2), GNNZ(2,4,4)
      COMPLEX*8  GCESNL(2,2), GCNSEL(2,2), GCNSER(2,2),
     .           GCUSDL(2,2), GCUSDR(2,2), GCDSUL(2,2), GCDSUR(2,2)
      COMPLEX*8  GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     .           GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)
C--
      REAL   *4  OL(2,2), OR(2,2), ER
      REAL   *4  ON(4,4)
      COMPLEX*8  ETA(4), UN(4,4)
C--
      REAL   *4  DCWSEL(6), DCWSER(6), DCWSNL(6)
      REAL   *4  BRSEL(6), BRSER(6), BRSNL(6)
      REAL   *4  DCWIN1(8), BRINO1(8)
      REAL   *4  HG0MAS(2)
C
C==================< Entry Point >======================================
C
C--
C  Set MSSM parameters.
C--
CTT 17/Jul/95      WM    = ZM*SQRT(1-S2W)
C--
      SM    = AM0
      HM1   = AMU
      G2MAS = AM2
      G3MAS = G2MAS*(ALFS/ALF)*S2W
C--
C  Calculate sfermion masses.
C     SFM(I) = (SNL,SEL,SER,SUL,SUR,SDL,SDR)
C--
      CALL GTSFMS(AM0,AM2,AMU,TANB,ALF,S2W,ZM,SFM)
C--
C  Diagonalize mass matrices for inos.
C--
Ctt 17/Jul/95 ZM is modified to be an input parameter for INOMIX
      CALL  INOMIX( ZM, WM, S2W, ALF, ALFS,
     .              HM1, G3MAS, TANB,
     .              SWM, SZM,
     .              OL, OR, ER, PHIL, PHIR,
     .              ON, UN, ETA, RELAX,
     .              IFLG  )
      write(6,*)' eta ', (eta(i),i=1,4)
C--
C  Calculate ino-ino-gauge boson couplings.
C--
      CALL  INOGBS( ALF, S2W,
     .              PHIL, PHIR, ER, ON, ETA,
     .              GNCW, GCCZ, GNNZ )
C--
C  Calculate ino-sfermion-fermion couplings.
C--
      CALL  INOFSF( ALF, S2W, WM,
     .              FM(1), FM(2), FM(3),
     .              TANB, PHIL, PHIR, ER, ON, ETA,
     .              GCESNL, GCNSEL, GCNSER,
     .              GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     .              GNNSNL, GNESEL, GNESER,
     .              GNUSUL, GNUSUR, GNDSDL, GNDSDR )
C--
C  Calculate widths of SUSY particles.
C  Very temporary version which is by no means correct.
C--
CTT 17/Jul/95  ZM is added as an input argument for SF2BR
      CALL SF2BR(  ZM, WM, S2W, ALF, ALFS,
     .             HM1, G3MAS, TANB, RELAX,
     .             FM(1), FM(2), FM(3),
     .             SFM(2), SFM(3), SFM(1), AMA,
     .             SWM, SZM, HGCMAS, HG0MAS,
     .             DCWSEL, DCWSER, DCWSNL,
     .             BRSEL, BRSER, BRSNL,
     .             DCWIN1, BRINO1 )
C--
      GMSW(1) = DCWIN1(1) + DCWIN1(2) + DCWIN1(3) + DCWIN1(4)
     .        + DCWIN1(5) + DCWIN1(6) + DCWIN1(7) + DCWIN1(8)
      IF ( GMSW(1).LE.1.E-10 ) GMSW(1) = 5.E-3
      GMSW(1) = 5.E-3
      GMSW(1) = GMSW(1)
      GMSW(2) = GMSW(1)
      GMSZ(1) = 0
      GMSZ(2) = GMSW(1)
      GMSZ(3) = GMSW(1)
      GMSZ(4) = GMSW(1)
C--
      GMSF(1) = DCWSNL(1) + DCWSNL(2) + DCWSNL(3)
     .        + DCWSNL(4) + DCWSNL(5) + DCWSNL(6)
      GMSF(2) = DCWSEL(1) + DCWSEL(2) + DCWSEL(3)
     .        + DCWSEL(4) + DCWSEL(5) + DCWSEL(6)
      GMSF(3) = DCWSER(1) + DCWSER(2) + DCWSER(3)
     .        + DCWSER(4) + DCWSER(5) + DCWSER(6)
      GMSF(4) = GMSF(2)
      GMSF(5) = GMSF(3)
      GMSF(6) = GMSF(2)
      GMSF(7) = GMSF(3)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, SPEVNT.
C*    Fill the JETSET commons. (Disabled in this version !!!)
C*


      SUBROUTINE SPEVNT(IRET)

      IMPLICIT    REAL*4 ( A-H, O-Z )
      PARAMETER        ( IxPRC = 1 )

*
* Datacards
*
       INTEGER TRIG
       COMMON /STDATA/ TRIG
*
       REAL BEAM
       COMMON /STDATB/ BEAM
*
       INTEGER IPMC
       COMMON /MCPARA/ IPMC
*
       PARAMETER (MAXUSR=6)
       REAL RUSR(MAXUSR)
       COMMON /MCUSER/ RUSR
*
       COMMON/MCONS1/DMZ
       COMMON/MCONS2/DMW
       COMMON/MCONS3/DMH
       COMMON/MCONS4/DMT
       COMMON/MCONS5/DALPE
       COMMON/MCONS6/DALPS
       COMMON/MCONS7/DSIN2THW
       COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
       SAVE /LUJETS/
       COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
       COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
       COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
************************************************************************
      INTEGER nmxhep, nevhep, nhep, isthep, idhep, jmohep, jdahep
      DOUBLE PRECISION phep, vhep
      PARAMETER (nmxhep = 2000)
      COMMON /HEPEVT/ nevhep, nhep, isthep(nmxhep), idhep(nmxhep)
     &,               jmohep(2,nmxhep), jdahep(2,nmxhep)
     &,               phep(5,nmxhep), vhep(4,nmxhep)
      SAVE /HEPEVT/
**************************************************************************
       CHARACTER*8 chaf
       COMMON /ludat4/ chaf(500)
       SAVE /LUdat4/
       INTEGER ijoin(2),ijoin1(2)

*
C
      PARAMETER       ( NEXLIN = 10 )
      COMMON /XCXCCN/  ROOTS, POLE, XPHASE(3,NEXLIN-3),
     .                 IDPTCL(3,NEXLIN), IHLCMB(NEXLIN),
     .                 PVOUT(0:3,NEXLIN), DSGMDX, DPDEBM, BMENGY(0:4),
     .                 ISRBM
      REAL   *4        ROOTS, POLE, XPHASE, PVOUT, DSGMDX, DPDEBM,
     .                 BMENGY
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)

      dimension erd(2)
      REAL*4       QMX1,QMX2

      COMMON /SSWORK/ IBUF(10000)
      REAL   *4       RBUF(10000)
      EQUIVALENCE    (IBUF,RBUF)
      REAL   *4       QV(0:3,10), QF(3,2,2)
      DATA NCALL      / 0 /
      DATA NPRINT     /10 /
      DATA NEVENT     / 0 /
      REAL*8     TAUC,TAUN
      REAL*8     HBAR
      PARAMETER( HBAR= 6.583173D-25 )

C
C========< Entry Point >================================================
C
C--
C  Reset IRET.
C--
      IRET = 0
C--
C  Initialize constants.
C--

      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         DO 10 IG = 1, 3
            QF(IG,1,1) =  0.
            QF(IG,2,1) = -1.
            QF(IG,1,2) =  2./3.
            QF(IG,2,2) = -1./3.
10       CONTINUE
      ENDIF
C--
C  TBPUT 'Spring:Header'.
C       Elm#1  IBUF(1) = Event #.
C                  (2) = Date
C                  (3) = Time
C                  (4) = not used
C                  (5) = E (e-)    MeV unit.
C                  (6) = E (e+)    MeV unit.
C                  (7) = Ebeam     MeV unit.
C                  (8) = Pol (e-)
C                  (9) = Pol (e+)
C                 (10) = not used.
C--
      NEVENT = NEVENT + 1
      IBUF(1) = NEVENT
      IBUF(2) = 0
      IBUF(3) = 0
CSG      CALL UIDATE( IBUF(2) )
CSG      CALL   TIME( IBUF(3) )
      IBUF(5) = BMENGY(3)*1.E3
      IBUF(6) = BMENGY(4)*1.E3
      IBUF(7) = BMENGY(0)*1.E3
      IBUF(8) = POLE
      IBUF(9) = 0
C--
C  TBPUT 'Spring:Header'.
C       Elm#2  IBUF( 1-24) = IDPTCL(3,8)
C                  (25-32) = IHLCMB
C              RBUF(33-47) = XPHASE(3,5)
C                  (   48) = DSGMDX
C                  (   49) = DPDEBM
C                  (   50) = nominal beam energy
C                  (   51) = Gaussian smeared E(e-)
C                  (   52) = Gaussian smeared E(e+)
C                  (   53) = E(e-) after beamstrahlung
C                  (   54) = E(e+) after beamstrahlung
C--
      CALL UVCOPY(24,IDPTCL,IBUF(1))
      CALL UVCOPY(8,IHLCMB(1),IBUF(25))
      CALL UVCOPY(15,XPHASE,RBUF(33))
      RBUF(48) = DSGMDX
      RBUF(49) = DPDEBM
      CALL UVCOPY(5,BMENGY(0),RBUF(50))
C--
C  TBPUT 'Spring:Parton_List'.
C       Elm#i  RBUF(1) = Particle serial number.
C                  (2) = Particle ID
C                  (3) = Mass (GeV)
C                  (4) = Charge
C                  (5) = Px(GeV)
C                  (6) = Py(GeV)
C                  (7) = Pz(GeV)
C                  (8) = E(GeV)
C                  (9) =    not used
C                 (10) =    not used
C                 (11) =    not used
C                 (12) = # daughter parton
C                 (13) = particle serial # of the 1st daughter
C                 (14) = particle serial # of the parent
C                 (15) =    not used
C                 (16) =    not used
C                 (17) = helicity
C                 (18) = colour single group ID
C                 (19) =    not used
C                 (20) =    not used
C--
C     PVOUT(*,i) = 4-momentum of i-th parton
C
C                               /  3 X0
C                              /
C                             /
C                            /\W+/ 4 f(up)
C      1 e-  \           X+ /  \/
C             \            /    \    _
C              \          /      \ 5 f(down)
C               \________/
C               /        \
C              /          \      / 8 f(down)
C             /            \    /
C      2 e+  /           X- \  /\    _
C                            \/W-\ 7 f(up)
C                             \
C                              \
C                               \  6 X0
C--
      CALL PSUMxx(PVOUT(0,4),PVOUT(0,5),QV(0,7))
      CALL PSUMxx(QV(0,7),PVOUT(0,3),QV(0,3))
      CALL PSUMxx(PVOUT(0,7),PVOUT(0,8),QV(0,8))
      CALL PSUMxx(QV(0,8),PVOUT(0,6),QV(0,4))
C--
      NPT = 0
      NOLD = N
      NC = N

      DO 2 ITR = 1,2
         isgn=(itr-1)*2-1
         k(itr,1) = 21
         k(itr,2) = isgn*11
         k(itr,3) = 0
         k(itr,4) = 0
         k(itr,5) = 0
         p(itr,1) = 0.
         p(itr,2) = 0.
         p(itr,3) = BEAM *float(isgn)
         p(itr,4) = BEAM
         p(itr,5) = 0.
         DO I=1,5
           V(itr,I) = 0.
         ENDDO
 2    CONTINUE

C--
C  (1) Store X+.
C--
      NC = NC + 1
      CALL RDOTxx(QV(0,3),QV(0,3),VMT2)
      CALL UVZERO( 20, RBUF )
      NPT      =   NPT + 1
      RBUF( 1) =   NPT
      RBUF( 2) =   -70
      RBUF( 3) =   SQRT(VMT2)
      RBUF( 4) =   1.
      RBUF( 5) =   QV(1,3)
      RBUF( 6) =   QV(2,3)
      RBUF( 7) =   QV(3,3)
      RBUF( 8) =   QV(0,3)
      RBUF(12) =   3
      RBUF(13) =   3
      RBUF(14) =   0
      RBUF(18) =   1
C--
      K(NC,1) = 11
      K(NC,2) = INT(RBUF(2))
      K(NC,3) = 0
      K(NC,4) = 3
      K(NC,5) = 5
      P(NC,1) = RBUF(5)
      P(NC,2) = RBUF(6)
      P(NC,3) = RBUF(7)
      P(NC,4) = RBUF(8)
      P(NC,5) = RBUF(3)
      DO I=1,5
        V(NC,I) = 0.
      ENDDO



C--
C  (2) Store X-.
C--
      NC = NC + 1
      CALL RDOTxx(QV(0,4),QV(0,4),VMT2)
      CALL UVZERO( 20, RBUF )
      NPT      =   NPT + 1
      RBUF( 1) =   NPT
CSG      RBUF( 2) =   77
      RBUF( 2) =   70
      RBUF( 3) =   SQRT(VMT2)
      RBUF( 4) =   -1.
      RBUF( 5) =   QV(1,4)
      RBUF( 6) =   QV(2,4)
      RBUF( 7) =   QV(3,4)
      RBUF( 8) =   QV(0,4)
      RBUF(12) =   3
      RBUF(13) =   6
      RBUF(14) =   0
      RBUF(18) =   1
C--
      K(NC,1) = 11
      K(NC,2) = INT(RBUF(2))
      K(NC,3) = 0
      K(NC,4) = 6
      K(NC,5) = 8
      P(NC,1) = RBUF(5)
      P(NC,2) = RBUF(6)
      P(NC,3) = RBUF(7)
      P(NC,4) = RBUF(8)
      P(NC,5) = RBUF(3)
      DO I=1,5
        V(NC,I) = 0.
      ENDDO



C--
C  (3) Store X0 from X+.
C--
      NC = NC + 1
      CALL UVZERO( 20, RBUF )
      NPT      =   NPT + 1
      RBUF( 1) =   NPT
CSG      RBUF( 2) =   71
c      RBUF( 2) =   67
      RBUF( 2) =   66
      RBUF( 3) =   SZM(1)
      RBUF( 4) =   0
      RBUF( 5) =   PVOUT(1,3)
      RBUF( 6) =   PVOUT(2,3)
      RBUF( 7) =   PVOUT(3,3)
      RBUF( 8) =   PVOUT(0,3)
      RBUF(12) =   0
      RBUF(13) =   0
      RBUF(14) =   1
      RBUF(18) =   1
C--
      K(NC,1) = 1
      K(NC,2) = INT(RBUF(2))
      K(NC,3) = NOLD + 1
      K(NC,4) = 0
      K(NC,5) = 0
      P(NC,1) = RBUF(5)
      P(NC,2) = RBUF(6)
      P(NC,3) = RBUF(7)
      P(NC,4) = RBUF(8)
      P(NC,5) = RBUF(3)
      DO I=1,5
        V(NC,I) = 0.
      ENDDO

C-
C--
C  (4-5) matter fermions from X+.
C--
      NF = -1
      DO 200 IPT = 4, 5
         NC =NC + 1
         CALL IDK2PD(IDPTCL(1,IPT),IDPDG)
         IG = IDPTCL(1,IPT)
         IT = IDPTCL(2,IPT)
         LQ = IDPTCL(3,IPT)
         NF = -NF
         CALL UVZERO( 20, RBUF )
         NPT      =   NPT + 1
         RBUF( 1) =   NPT
         RBUF( 2) =   NF*IDPDG
         RBUF( 3) =   AMF(IG,IT,LQ)
         RBUF( 4) =   NF*QF(IG,IT,LQ)
         RBUF( 5) =   PVOUT(1,IPT)
         RBUF( 6) =   PVOUT(2,IPT)
         RBUF( 7) =   PVOUT(3,IPT)
         RBUF( 8) =   PVOUT(0,IPT)
         RBUF(12) =   0
         RBUF(13) =   0
         RBUF(14) =   1
         RBUF(17) =   IHLCMB(IPT)
         RBUF(18) =   2
         RBUF(19) =   (LQ-1)*101
C--
         K(NC,1) = 1
         K(NC,2) = INT(RBUF(2))
         K(NC,3) = NOLD + 1
         K(NC,4) = 0
         K(NC,5) = 0
         P(NC,1) = RBUF(5)
         P(NC,2) = RBUF(6)
         P(NC,3) = RBUF(7)
         P(NC,4) = RBUF(8)
         P(NC,5) = RBUF(3)
         DO I=1,5
           V(NC,I) = 0.
         ENDDO
200   CONTINUE

      K1=K(NC,2)
      K2=K(NC-1,2)


      ERED=P(NC-1,4)+P(NC,4)
      ERD(1)=ERED

      QMX1=(P(NC-1,4)+P(NC,4))**2-(P(NC-1,3)+P(NC,3))**2
     &      -(P(NC-1,2)+P(NC,2))**2-(P(NC-1,1)+P(NC,1))**2
      QMX1=SIGN(1.0,QMX1)*SQRT(ABS(QMX1))

c      IF(ERD(1).LT.ULMASS(k1)+ulmass(k2))THEN
c       write(*,*)' ERD1 ',ERD(1)
c      ELSE
c       WRITE(*,*)'WARNING WRONG ERD1'
c      ENDIF

C--
C  (6) Store X0 from X-.
C--
      NC = NC + 1
      CALL UVZERO( 20, RBUF )
      NPT      =   NPT + 1
      RBUF( 1) =   NPT
CSG      RBUF( 2) =   71
      RBUF( 2) =   66
      RBUF( 3) =   SZM(1)
      RBUF( 4) =   0
      RBUF( 5) =   PVOUT(1,6)
      RBUF( 6) =   PVOUT(2,6)
      RBUF( 7) =   PVOUT(3,6)
      RBUF( 8) =   PVOUT(0,6)
      RBUF(12) =   0
      RBUF(13) =   0
      RBUF(14) =   2
      RBUF(18) =   3
C--
      K(NC,1) = 1
      K(NC,2) = INT(RBUF(2))
      K(NC,3) = NOLD + 2
      K(NC,4) = 0
      K(NC,5) = 0
      P(NC,1) = RBUF(5)
      P(NC,2) = RBUF(6)
      P(NC,3) = RBUF(7)
      P(NC,4) = RBUF(8)
      P(NC,5) = RBUF(3)
      DO I=1,5
        V(NC,I) = 0.
      ENDDO

C--
C  (7-8) matter fermions from X-.
C--
      NF = 1
      DO 300 IPT = 7, 8
         NC = NC + 1
         CALL IDK2PD(IDPTCL(1,IPT),IDPDG)
         IG = IDPTCL(1,IPT)
         IT = IDPTCL(2,IPT)
         LQ = IDPTCL(3,IPT)
         NF = -NF
         CALL UVZERO( 20, RBUF )
         NPT      =   NPT + 1
         RBUF( 1) =   NPT
         RBUF( 2) =   NF*IDPDG
         RBUF( 3) =   AMF(IG,IT,LQ)
         RBUF( 4) =   NF*QF(IG,IT,LQ)
         RBUF( 5) =   PVOUT(1,IPT)
         RBUF( 6) =   PVOUT(2,IPT)
         RBUF( 7) =   PVOUT(3,IPT)
         RBUF( 8) =   PVOUT(0,IPT)
         RBUF(12) =   0
         RBUF(13) =   0
         RBUF(14) =   2
         RBUF(17) =   IHLCMB(IPT)
         RBUF(18) =   4
         RBUF(19) =   (LQ-1)*201
C--
         K(NC,1) = 1
         K(NC,2) = INT(RBUF(2))
         K(NC,3) = NOLD + 2
         K(NC,4) = 0
         K(NC,5) = 0
         P(NC,1) = RBUF(5)
         P(NC,2) = RBUF(6)
         P(NC,3) = RBUF(7)
         P(NC,4) = RBUF(8)
         P(NC,5) = RBUF(3)
         DO I=1,5
           V(NC,I) = 0.
         ENDDO
300   CONTINUE
C
      N = NC


      K1=K(NC,2)
      K2=K(NC-1,2)
      ERED=P(NC-1,4)+P(NC,4)
      ERD(2)=ERED

      QMX2=(P(NC-1,4)+P(NC,4))**2-(P(NC-1,3)+P(NC,3))**2
     &     -(P(NC-1,2)+P(NC,2))**2-(P(NC-1,1)+P(NC,1))**2
      QMX2=SIGN(1.0,QMX2)*SQRT(ABS(QMX2))


c      IF(ERD(2).LT.ULMASS(k1)+ulmass(k2))THEN
c       write(*,*)' ERD2 ',ERD(2)
c      ELSE
c       WRITE(*,*)'WARNING WRONG ERD2'
c      ENDIF

       ijoin(1) = 4
       ijoin(2) = 5
       ijoin1(1) = 7
       ijoin1(2) = 8

      IF(iabs(K(4,2)).LT.10.AND.iabs(K(5,2)).LT.10)THEN
       k(4,1) = 2
       k(5,1) = 2
       call lujoin(2,ijoin)
       call lushow(4,5,QMX1)
      endif
      IF(iabs(K(7,2)).LT.10.AND.iabs(K(8,2)).LT.10)THEN
       k(7,1) = 2
       k(8,1) = 2
       call lujoin(2,ijoin1)
       call lushow(7,8,QMX2)
      endif
      CALL LUEXEC
c      if(NEVENT.LT.4)call lulist(1)

C
c      CALL LUHEPC (1)
      RETURN
      END
CDECK  ID>, UHSETF.
CC**********************************************************************
C*
C*========================--==
C* Subroutine UHSETF(P,EB,EA)
C*========================--==
C*
C* (Purpose)
C*    Setups a reference frame with EA(*,3) along P and EA(*,2)
C*    perpendicular to P-EB(*,3) plane.
C* (Input)
C*       P (3)   : (R*4) : direction vector of new polar axis.
C*       EB(*,i) : (R*4) : i-th axis of the old reference frame.
C* (Output)
C*       EA(*,i) : (R*4) : i-th axis of the new reference frame.
C* (Relation)
C*    Calls UNRMV3, UNCROS, UVCOPY, UVZERO, and UABSV3.
C*
CC**********************************************************************

      SUBROUTINE UHSETF(P,EB,EA)

      REAL*4     P(3), EB(3,3), EA(3,3)
      REAL*4     E(3,3), E2(3)
      DATA XMIN  / 1.E-6 /
C
C========< Entry Point >================================================
C
C--
C  Set new Z-axis.
C--
      CALL UNRMV3(P,E(1,3))
C--
C  Set new Y-axis.
C--
      CALL UCROSS(EB(1,3),E(1,3),E2)
      CSTH  = UDOT3(E(1,3),EB(1,3))
      AE2   = UABSV3(E2)
C--
      IF ( AE2.LT.XMIN ) THEN
         CALL UVCOPY(9,EB,EA)
         IF ( CSTH.LT.0. ) CALL USCLM3(-1.,EB(1,3),EA(1,3))
         RETURN
      ELSE
         CALL UNRMV3(E2,E(1,2))
      ENDIF
C--
C  Set new X-axis.
C--
      CALL UNCROS(E(1,2),E(1,3),E(1,1))
C--
C  Store E in EA.
C--
      CALL UVCOPY(9,E,EA)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DCY2BODY.
C==============
C subrouines to calculate decay widthes of 2-body decay channel
C
C      Fermion_1 ---> Fermion_2 + Vector_boson
C      Scaler    ---> Fermion_1 + Fermion_2
C      Fermion_1 ---> Fermion_2 + Scaler
C
C  << Note >>
C  If both oF fermion_1 & 2 are Majorana particles,
C  one should give appropreate GL & GR or keep in mind that
C  output is not correct because these routines
C  don't concern the case.
C
C==============
C----------------------------------------------------------------------C
      SUBROUTINE F1F2VD( F1MAS, F2MAS, VMAS, GL, GR, DCYWDT )
C----------------------------------------------------------------------C
C Purpose
C     calculate the decay widthes of
C       Fermion_1 --> Fermion_2 + Vector_boson
C
C Inputs ( note that GL,GR are complex*8 )
C     F1MAS (Real*4) :   mass of fermion_1 (GeV)
C     F2MAS (Real*4) :   mass of fermion_2 (GeV)
C     VMAS  (Real*4) :   mass of Vector_boson  (GeV)
C     GL, GR (complex*8) :  left/right-handed coupling of
C                           f_1 - f_2 - V
C Output
C     DCYWDT : decay width (GeV)
C----------------------------------------------------------------------C
      COMPLEX*8 GL, GR
C----------------------------------------------------------------------C
      IF( F1MAS .LE. F2MAS+VMAS ) THEN
        DCYWDT = 0.
        RETURN
      ENDIF

      PI = ACOS(0.)*2.
      F1MAS2 = F1MAS**2
      F2MAS2 = F2MAS**2
      VMAS2 = VMAS**2

      PMOM2 = (F1MAS2-(F2MAS+VMAS)**2)*(F1MAS2-(F2MAS-VMAS)**2)
     &         /4./F1MAS2
      PMOM = SQRT(PMOM2)

      TMAT2 = 0.5* ( ABS(GL)**2 + ABS(GR)**2 )
     &        * ( F1MAS2+F2MAS2-VMAS2
     &             + ((F1MAS2-F2MAS2)**2-VMAS2**2)/VMAS2      )
     &       - 6.*F1MAS*F2MAS*REAL( GL*CONJG(GR) )

      DCYWDT = PMOM*TMAT2/8./PI/F1MAS2


      RETURN
      END
C----------------------------------------------------------------------C
      SUBROUTINE SCF1F2( SCMAS, F1MAS, F2MAS, GL, GR, DCYWDT )
C----------------------------------------------------------------------C
C Purpose
C     calculate the decay widthes of
C       Scaler ---> fermion_1 + fermion_2
C Inputs ( note that GL,GR are complex*8 )
C     SCMAS  (Real*4) :    mass of scaler (GeV)
C     F1MAS  (Real*4) :    mass of fermion_1  (GeV)
C     F2MAS  (Real*4) :    mass of fermion_2  (GeV)
C     GL, GR (complex*8) :  left/right-handed coupling of
C                           scaler - f_1 - f_2
C Output
C     DCYWDT : decay width (GeV)
C----------------------------------------------------------------------C
      COMPLEX*8 GL, GR
C----------------------------------------------------------------------C
      IF( SCMAS .LE. F1MAS+F2MAS ) THEN
        DCYWDT = 0.
        RETURN
      ENDIF

      PI = ACOS(0.)*2.
      SCMAS2 = SCMAS**2
      F1MAS2 = F1MAS**2
      F2MAS2 = F2MAS**2

      PMOM2 = (SCMAS2-(F1MAS+F2MAS)**2)*(SCMAS2-(F1MAS-F2MAS)**2)
     &         /4./SCMAS2
      PMOM = SQRT(PMOM2)

      TMAT2 = ( ABS(GL)**2 + ABS(GR)**2 ) * ( SCMAS2-F1MAS2-F2MAS2 )
     &       - 4.*F2MAS*F1MAS*REAL( GL*CONJG(GR) )

      DCYWDT = PMOM*TMAT2/8./PI/SCMAS2


      RETURN
      END
C----------------------------------------------------------------------C
      SUBROUTINE F1F2SC( F1MAS, F2MAS, SCMAS, GL, GR, DCYWDT )
C----------------------------------------------------------------------C
C Purpose
C     calculate the decay widthes of
C       fermion_1 --> fermion_2 + scaler
C Inputs ( note that GL,GR are complex*8 )
C     F1MAS  (Real*4) :    mass of fermion_1  (GeV)
C     F2MAS  (Real*4) :    mass of fermion_2  (GeV)
C     SCMAS  (Real*4) :    mass of scaler (GeV)
C     GL, GR (complex*8) :  left/right-handed coupling of
C                           f_1 - f_2 - scaler
C Output
C     DCYWDT : decay width (GeV)
C----------------------------------------------------------------------C
      COMPLEX*8 GL, GR
C----------------------------------------------------------------------C
      IF( F1MAS .LE. F2MAS+SCMAS ) THEN
        DCYWDT = 0.
        RETURN
      ENDIF

      PI = ACOS(0.)*2.
      F1MAS2 = F1MAS**2
      F2MAS2 = F2MAS**2
      SCMAS2 = SCMAS**2

      PMOM2 = (F1MAS2-(F2MAS+SCMAS)**2)*(F1MAS2-(F2MAS-SCMAS)**2)
     &         /4./F1MAS2
      PMOM = SQRT(PMOM2)

      TMAT2 = ( ABS(GL)**2 + ABS(GR)**2 ) * ( F1MAS2+F2MAS2-SCMAS2 )
     &       + 4.*F1MAS*F2MAS*REAL( GL*CONJG(GR) )
C      spin average for initial fermion f1
      TMAT2 = TMAT2/2.

      DCYWDT = PMOM*TMAT2/8./PI/F1MAS2


      RETURN
      END
CDECK  ID>, FUNCXD.
C*    X+ --> X0 fu + fdbar.
C*
      REAL*8 FUNCTION FUNCXD(ZZ)

      IMPLICIT REAL*4 ( A-H, O-Z )
      REAL*8   ZZ(50), Z(50)
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      PARAMETER       ( NEXLIN = 10 )
      COMMON /XCXCCN/  ROOTS, POLE, XPHASE(3,NEXLIN-3),
     .                 IDPTCL(3,NEXLIN), IHLCMB(NEXLIN),
     .                 PVOUT(0:3,NEXLIN), DSGMDX, DPDEBM, BMENGY(0:4),
     .                 ISRBM
      REAL   *4        ROOTS, POLE, XPHASE, PVOUT, DSGMDX, DPDEBM,
     .                 BMENGY
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
C--
C  For no ISR and no BMEFF.
C--
      PARAMETER   ( MX_NZZ = 50 )
      COMMON /BSHUFL/  NZZ, ISHUFL(MX_NZZ)
      INTEGER*4        NZZ, ISHUFL

      PARAMETER  ( NP = 4, NHEL = 4 )
      INTEGER*4   IPV(3,2)
      REAL   *4   SG, PV(0:3,NP), AM(NP), AMR(3,3)
C--
      REAL   *4   AMSF(3,2,2), GTSF(3,2,2)
C--
      REAL   *8   WAT, WT
C--
      INTEGER*4   IHEL(NP,NHEL,2)
      DATA        (( IHEL(I,J,1), I=1,NP ), J=1,NHEL )  /
     .                   -1,   -1, -1,+1,
     .                   -1,   +1, -1,+1,
     .                   -1,   -1, +1,-1,
     .                   -1,   +1, +1,-1 /
      DATA        (( IHEL(I,J,2), I=1,NP ), J=1,NHEL )  /
     .                   +1,   -1, -1,+1,
     .                   +1,   +1, -1,+1,
     .                   +1,   -1, +1,-1,
     .                   +1,   +1, +1,-1 /        
C--
      DATA NCALL /  0 /
C
C========< Entry Point >================================================
C
C--
C  Set some variables.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         AMX    = SWM(1)
         GMX    = GMSW(1)
C--
         AMSF(1,1,1) = SFM(1)
         AMSF(1,2,1) = SFM(2)
         AMSF(1,1,2) = SFM(4)
         AMSF(1,2,2) = SFM(6)
C--
         AMSF(2,1,1) = SFM(1)
         AMSF(2,2,1) = SFM(2)
         AMSF(2,1,2) = SFM(4)
         AMSF(2,2,2) = SFM(6)
C--
         AMSF(3,1,1) = SFM(1)
         AMSF(3,2,1) = SFM(2)
         AMSF(3,1,2) = SFM(4)
         AMSF(3,2,2) = SFM(6)
C--
         GTSF(1,1,1) = GMSF(1)
         GTSF(1,2,1) = GMSF(2)
         GTSF(1,1,2) = GMSF(4)
         GTSF(1,2,2) = GMSF(6)
C--
         GTSF(2,1,1) = GMSF(1)
         GTSF(2,2,1) = GMSF(2)
         GTSF(2,1,2) = GMSF(4)
         GTSF(2,2,2) = GMSF(6)
C--
         GTSF(3,1,1) = GMSF(1)
         GTSF(3,2,1) = GMSF(2)
         GTSF(3,1,2) = GMSF(4)
         GTSF(3,2,2) = GMSF(6)
      ENDIF
C--
C  Shuffle integration variables.
C--
      DO 100 I = 1, NZZ
         Z(ISHUFL(I)) = ZZ(I)
100   CONTINUE
C--
C  Set independent variables.
C     Z( 1) : X+ decay mode
C      ( 2) : helicity combination
C      ( 3) : m(+ab)**2
C      ( 4) : m(+ac)**2
C      ( 5) : cos(theta_a)     in X+ rest frame
C      ( 6) : phi_a            in X+ rest frame
C      ( 7) : phi_b            in X+ rest frame
C--
C  Reset event weight.
C--
      WAT = 1
C--
C  First set initial states.
C--
      IDPTCL(1, 1) = 0
      IDPTCL(2, 1) = 0
      IDPTCL(3, 1) = 0
C--
C  Then select final states.
C--
      IDPTCL(1, 2) = 0
      IDPTCL(2, 2) = 0
      IDPTCL(3, 2) = 0
C-- W+ from X+.
      XW1 = Z(1)
      DO 200 IMD = 1, 12
         IF ( XW1.LT.BRVDK(IMD,1) )              GO TO 210
200   CONTINUE
C--
210   IDPTCL(1, 3) = MDVDK(1,IMD,1)
      IDPTCL(2, 3) = 1
      IDPTCL(3, 3) = MDVDK(3,IMD,1)
      IDPTCL(1, 4) = MDVDK(2,IMD,1)
      IDPTCL(2, 4) = 2
      IDPTCL(3, 4) = MDVDK(3,IMD,1)
      IMD1         = IMD
C--
      BRW1 = BRVDK(IMD,1) - BRVDK(IMD-1,1)
      WAT  = WAT/BRW1
C--
C  Select helicity combination.
C--
      HLM   = 0.5
      IF ( Z(2).LT.HLM ) THEN
         ICMB = 1
         JCMB = NHEL*Z(2)/HLM + 1
         WAT  = WAT*NHEL
      ELSE
         ICMB = 2
         JCMB = NHEL*(Z(2)-HLM)/(1-HLM) + 1
         WAT  = WAT*NHEL
      ENDIF
      JCMB = MIN(JCMB,NHEL)
      CALL UVCOPY(NP,IHEL(1,JCMB,ICMB),IHLCMB(1))
C--
C  Select kinematic variables.
C--
      AM(1)   = AMX
      AM(2)   = SZM(1)
      AM(3)   = AMF(IDPTCL(1,3),IDPTCL(2,3),IDPTCL(3,3))
      AM(4)   = AMF(IDPTCL(1,4),IDPTCL(2,4),IDPTCL(3,4))
C>>>
CTBW       AM(1)   = AMF(3,1,2)
CTBW       AM(2)   = AMF(3,2,2)
C>>>
      IF ( AM(1).LE.AM(2)+AM(3)+AM(4) ) THEN
         FUNCXD = 0
         RETURN
      ENDIF
C--
      AMR(1,1) = AMSF(IDPTCL(1,3),IDPTCL(2,3),IDPTCL(3,3))
      AMR(2,1) = GTSF(IDPTCL(1,3),IDPTCL(2,3),IDPTCL(3,3))
      AMR(3,1) = AMR(1,1) + AM(4)
      AMR(1,2) = AMSF(IDPTCL(1,4),IDPTCL(2,4),IDPTCL(3,4))
      AMR(2,2) = GTSF(IDPTCL(1,4),IDPTCL(2,4),IDPTCL(3,4))
      AMR(3,2) = AMR(1,2) + AM(3)
      AMR(1,3) = AMW
      AMR(2,3) = GMWTOT
      AMR(3,3) = AMR(1,3) + AM(2)
C--
C  Set phase space variables.
C--
      CALL GETXPHW(AM,AMR,Z(3),IPV,XPHASE,WT)
      IF ( WT.LE.0.D0 ) THEN
         FUNCXD = 0
         RETURN
      ENDIF
      WAT = WAT*WT
C--
C  Calculate differential width.
C--
      CALL GMCXDF(IDPTCL,IHLCMB,AM,IPV,XPHASE,SG,PV)
      FUNCXD   = SG*WAT
      IF ( FUNCXD.LE.0.D0 ) RETURN
C--
C  Save 4-momenta in PVOUT.
C--
      CALL UVCOPY(4*NP,PV(0,1),PVOUT(0,1))
C--
C  Fill hists. and plots.
C--
      COSLB = PV(3,4)/UABSV3(PV(1,4))
      COSBB = XPHASE(2,1)
      PHIBB = XPHASE(3,1)
      COMB  = JCMB + NHEL*(ICMB-1) + .1
      QXP2  = XPHASE(1,1)
      Q42   = XPHASE(1,2)
      Q52   = XPHASE(2,2)
C--
c      CALL XHFILL( 31, DBLE(COSBB)       ,FUNCXD )
c      CALL XHFILL( 32, DBLE(PHIBB)*xRD2DG,FUNCXD )
c      CALL XHFILL( 33, DBLE(SQRT(Q42))   ,FUNCXD )
c      CALL XHFILL( 34, DBLE(SQRT(Q52))   ,FUNCXD )
c      CALL XHFILL( 35, DBLE(COMB)        ,FUNCXD )
c      IF ( IDPTCL(3,4).EQ.1 .AND. IDPTCL(1,4).LE.2 ) THEN
c         CALL XHFILL( 36, DBLE(PV(0,4))     ,FUNCXD )
c         CALL XHFILL( 37, DBLE(COSLB)       ,FUNCXD )
c      ENDIF
      CALL XHFILL( 38, DBLE(IMD1+.1)        ,FUNCXD )
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, HELAS204.
C
C ======================================================================
C
      SUBROUTINE BOOSTX(P,Q , PBOOST)
C
C This subroutine performs the Lorentz boost of a four-momentum.  The
C momentum P is assumed to be given in the rest frame of Q.  PBOOST is
C the momentum P boosted to the frame in which Q is given.  Q must be a
C timelike momentum.
C
C INPUT:
C       real    P(0:3)         : four-momentum P in the Q rest  frame
C       real    Q(0:3)         : four-momentum Q in the boosted frame
C
C OUTPUT:
C       real    PBOOST(0:3)    : four-momentum P in the boosted frame
C
      REAL    P(0:3),Q(0:3),PBOOST(0:3),PQ,QQ,M,LF
C
      QQ=Q(1)**2+Q(2)**2+Q(3)**2
C
      IF (QQ.NE.0.) THEN
         PQ=P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3)
         M=SQRT(Q(0)**2-QQ)
         LF=((Q(0)-M)*PQ/QQ+P(0))/M
         PBOOST(0) = (P(0)*Q(0)+PQ)/M
         PBOOST(1) =  P(1)+Q(1)*LF
         PBOOST(2) =  P(2)+Q(2)*LF
         PBOOST(3) =  P(3)+Q(3)*LF
      ELSE
         PBOOST(0)=P(0)
         PBOOST(1)=P(1)
         PBOOST(2)=P(2)
         PBOOST(3)=P(3)
      ENDIF
      RETURN
      END
C
C **********************************************************************
C
      SUBROUTINE COUP1X(SW2 , GW,GWWA,GWWZ)
C
C This subroutine sets up the coupling constants of the gauge bosons in
C the STANDARD MODEL.
C
C INPUT:
C       real    SW2            : square of sine of the weak angle
C
C OUTPUT:
C       real    GW             : weak coupling constant
C       real    GWWA           : dimensionLESS coupling of W-,W+,A
C       real    GWWZ           : dimensionLESS coupling of W-,W+,Z
C
      REAL    SW2,GW,GWWA,GWWZ,ALPHA,FOURPI,EE,SW,CW
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE=SQRT(ALPHA*FOURPI)
      SW=SQRT(SW2)
      CW=SQRT(1.-SW2)
C
      GW    =  EE/SW
      GWWA  =  EE
      GWWZ  =  EE*CW/SW
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP2X(SW2 , GAL,GAU,GAD,GWF,GZN,GZL,GZU,GZD,G1)
C
C This subroutine sets up the coupling constants for the fermion-
C fermion-vector vertices in the STANDARD MODEL.  The array of the
C couplings specifies the chirality of the flowing-IN fermion.  G??(1)
C denotes a left-handed coupling, and G??(2) a right-handed coupling.
C
C INPUT:
C       real    SW2            : square of sine of the weak angle
C
C OUTPUT:
C       real    GAL(2)         : coupling with A of charged leptons
C       real    GAU(2)         : coupling with A of up-type quarks
C       real    GAD(2)         : coupling with A of down-type quarks
C       real    GWF(2)         : coupling with W-,W+ of fermions
C       real    GZN(2)         : coupling with Z of neutrinos
C       real    GZL(2)         : coupling with Z of charged leptons
C       real    GZU(2)         : coupling with Z of up-type quarks
C       real    GZD(2)         : coupling with Z of down-type quarks
C       real    G1(2)          : unit coupling of fermions
C
      REAL GAL(2),GAU(2),GAD(2),GWF(2),GZN(2),GZL(2),GZU(2),GZD(2),
     &     G1(2),SW2,ALPHA,FOURPI,EE,SW,CW,EZ,EY
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE=SQRT(ALPHA*FOURPI)
      SW=SQRT(SW2)
      CW=SQRT(1.-SW2)
      EZ=EE/(SW*CW)
      EY=EE*(SW/CW)
C
      GAL(1) =  EE
      GAL(2) =  EE
      GAU(1) = -EE*2./3.
      GAU(2) = -EE*2./3.
      GAD(1) =  EE   /3.
      GAD(2) =  EE   /3.
      GWF(1) = -EE/SQRT(2.*SW2)
      GWF(2) =  0.
      GZN(1) = -EZ*  0.5
      GZN(2) =  0.
      GZL(1) = -EZ*(-0.5+SW2)
      GZL(2) = -EY
      GZU(1) = -EZ*( 0.5-SW2*2./3.)
      GZU(2) =  EY*          2./3.
      GZD(1) = -EZ*(-0.5+SW2   /3.)
      GZD(2) = -EY             /3.
      G1(1)  =  1.
      G1(2)  =  1.
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP3X(SW2,ZMASS,HMASS ,
     &                  GWWH,GZZH,GHHH,GWWHH,GZZHH,GHHHH)
C
C This subroutine sets up the coupling constants of the gauge bosons and
C Higgs boson in the STANDARD MODEL.
C
C INPUT:
C       real    SW2            : square of sine of the weak angle
C       real    ZMASS          : mass of Z
C       real    HMASS          : mass of Higgs
C
C OUTPUT:
C       real    GWWH           : dimensionFUL  coupling of W-,W+,H
C       real    GZZH           : dimensionFUL  coupling of Z, Z, H
C       real    GHHH           : dimensionFUL  coupling of H, H, H
C       real    GWWHH          : dimensionFUL  coupling of W-,W+,H, H
C       real    GZZHH          : dimensionFUL  coupling of Z, Z, H, H
C       real    GHHHH          : dimensionLESS coupling of H, H, H, H
C
      REAL    SW2,ZMASS,HMASS,GWWH,GZZH,GHHH,GWWHH,GZZHH,GHHHH,
     &        ALPHA,FOURPI,EE2,SC2,V
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE2=ALPHA*FOURPI
      SC2=SW2*(1.0-SW2)
      V=2.0*ZMASS*SQRT(SC2)/SQRT(EE2)
C
      GWWH  =   EE2/SW2*0.5*V
      GZZH  =   EE2/SC2*0.5*V
      GHHH  =  -HMASS**2/V*3.0
      GWWHH =   EE2/SW2*0.5
      GZZHH =   EE2/SC2*0.5
      GHHHH = -(HMASS/V)**2*3.0
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE COUP4X(SW2,ZMASS,FMASS , GCHF)
C
C This subroutine sets up the coupling constant for the fermion-fermion-
C Higgs vertex in the STANDARD MODEL.  The coupling is COMPLEX and the
C array of the coupling specifies the chirality of the flowing-IN
C fermion.  GCHF(1) denotes a left-handed coupling, and GCHF(2) a right-
C handed coupling.
C
C INPUT:
C       real    SW2            : square of sine of the weak angle
C       real    ZMASS          : Z       mass
C       real    FMASS          : fermion mass
C
C OUTPUT:
C       complex GCHF(2)        : coupling of fermion and Higgs
C
      COMPLEX GCHF(2)
      REAL    SW2,ZMASS,FMASS,ALPHA,FOURPI,EZ,G
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EZ=SQRT(ALPHA*FOURPI)/SQRT(SW2*(1.-SW2))
      G=EZ*FMASS*0.5/ZMASS
C
      GCHF(1) = CMPLX( -G )
      GCHF(2) = CMPLX( -G )
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE EAIXXX(EB,EA,SHLF,CHLF,PHI,NHE,NHA , EAI)
C
C This subroutine computes an off-shell electron wavefunction after
C emitting a photon from the electron beam, with a special care for the
C small angle region.  The momenta are measured in the laboratory frame,
C where the e- beam is along the positive z axis.
C
C INPUT:
C       real    EB             : energy (GeV)    of beam  e-
C       real    EA             : energy (GeV)    of final photon
C       real    SHLF           : sin(theta/2)    of final photon
C       real    CHLF           : cos(theta/2)    of final photon
C       real    PHI            : azimuthal angle of final photon
C       integer NHE  = -1 or 1 : helicity        of beam  e-
C       integer NHA  = -1 or 1 : helicity        of final photon
C
C OUTPUT:
C       complex EAI(6)         : off-shell electron             |e',A,e>
C
      COMPLEX EAI(6),PHS
      REAL    EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER NHE,NHA,NN
C
      ME   =REAL(0.51099906D-3)
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*REAL(3.14159265D0))
C
      NN=NHA*NHE
      RNHE=REAL(NHE)
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
      D=-1./(EA*EB*(4.*SHLF**2+(ME/EB)**2*C))
      COEFF=-REAL(NN)*GAL*SQRT(EB)*D
      XNNP=X*REAL(1+NN)
      XNNM=X*REAL(1-NN)
      SNP=SIN(PHI)
      CSP=COS(PHI)
      PHS=CMPLX( CSP , RNHE*SNP )
C
      EAI((5-3*NHE)/2) = -RNHE*COEFF*ME*S*(1.+XNNP*.5)
      EAI((5-NHE)/2)   =  XNNP*COEFF*ME*CHLF**2*PHS
      EAI((5+NHE)/2)   =  RNHE*COEFF*EB*S*(-2.+XNNM)
      EAI((5+3*NHE)/2) =  XNNM*COEFF*EB*SHLF**2*PHS*2.
C
      EAI(5) =  EB*CMPLX( 1.-X , 1.-X*C )
      EAI(6) = -EB*X*S*CMPLX( CSP , SNP )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE EAOXXX(EB,EA,SHLF,CHLF,PHI,NHE,NHA , EAO)
C
C This subroutine computes an off-shell positron wavefunction after
C emitting a photon from the positron beam, with a special care for the
C small angle region.  The momenta are measured in the laboratory frame,
C where the e+ beam is along the negative z axis.
C
C INPUT:
C       real    EB             : energy (GeV)    of beam  e+
C       real    EA             : energy (GeV)    of final photon
C       real    SHLF           : sin(theta/2)    of final photon
C       real    CHLF           : cos(theta/2)    of final photon
C       real    PHI            : azimuthal angle of final photon
C       integer NHE  = -1 or 1 : helicity        of beam  e+
C       integer NHA  = -1 or 1 : helicity        of final photon
C
C OUTPUT:
C       complex EAO(6)         : off-shell positron             <e,A,e'|
C
      COMPLEX EAO(6),PHS
      REAL    EB,EA,SHLF,CHLF,PHI,ME,ALPHA,GAL,RNHE,X,C,S,D,COEFF,
     &        XNNP,XNNM,SNP,CSP
      INTEGER NHE,NHA,NN
C
      ME   =REAL(0.51099906D-3)
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*REAL(3.14159265D0))
C
      NN=NHA*NHE
      RNHE=REAL(NHE)
      X=EA/EB
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
      D=-1./(EA*EB*(4.*CHLF**2-(ME/EB)**2*C))
      COEFF=REAL(NN)*GAL*SQRT(EB)*D
      XNNP=X*REAL(1+NN)
      XNNM=X*REAL(1-NN)
      SNP=SIN(PHI)
      CSP=COS(PHI)
      PHS=CMPLX( CSP ,-RNHE*SNP )
C
      EAO((5-3*NHE)/2) =              COEFF*ME*S*(1.+XNNP*.5)
      EAO((5-NHE)/2)   = RNHE*XNNP    *COEFF*ME*SHLF**2*PHS
      EAO((5+NHE)/2)   =              COEFF*EB*S*(-2.+XNNM)
      EAO((5+3*NHE)/2) = REAL(NHA-NHE)*COEFF*EB*X*CHLF**2*PHS*2.
C
      EAO(5) = EB*CMPLX( X-1. , X*C+1. )
      EAO(6) = EB*X*S*CMPLX( CSP , SNP )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FSIXXX(FI,SC,GC,FMASS,FWIDTH , FSI)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-IN external fermion and a vector boson.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex SC(3)          : input    scalar                      S
C       complex GC(2)          : coupling constants                 GCHF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FSI(6)         : off-shell fermion             |F',S,FI>
C
      COMPLEX FI(6),SC(3),FSI(6),GC(2),SL1,SL2,SR1,SR2,DS
      REAL    PF(0:3),FMASS,FWIDTH,PF2,P0P3,P0M3
C
      FSI(5) = FI(5)-SC(2)
      FSI(6) = FI(6)-SC(3)
C
      PF(0)=REAL( FSI(5))
      PF(1)=REAL( FSI(6))
      PF(2)=AIMAG(FSI(6))
      PF(3)=AIMAG(FSI(5))
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      DS=-SC(1)/CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      P0P3=PF(0)+PF(3)
      P0M3=PF(0)-PF(3)
      SL1=GC(1)*(P0P3*FI(1)+CONJG(FSI(6))*FI(2))
      SL2=GC(1)*(P0M3*FI(2)      +FSI(6) *FI(1))
      SR1=GC(2)*(P0M3*FI(3)-CONJG(FSI(6))*FI(4))
      SR2=GC(2)*(P0P3*FI(4)      -FSI(6) *FI(3))
C
      FSI(1) = ( GC(1)*FMASS*FI(1) + SR1 )*DS
      FSI(2) = ( GC(1)*FMASS*FI(2) + SR2 )*DS
      FSI(3) = ( GC(2)*FMASS*FI(3) + SL1 )*DS
      FSI(4) = ( GC(2)*FMASS*FI(4) + SL2 )*DS
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FSOXXX(FO,SC,GC,FMASS,FWIDTH , FSO)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-OUT external fermion and a vector boson.
C
C INPUT:
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex SC(6)          : input    scalar                      S
C       complex GC(2)          : coupling constants                 GCHF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FSO(6)         : off-shell fermion             <FO,S,F'|
C
      COMPLEX FO(6),SC(6),FSO(6),GC(2),SL1,SL2,SR1,SR2,DS
      REAL    PF(0:3),FMASS,FWIDTH,PF2,P0P3,P0M3
C
      FSO(5) = FO(5)+SC(2)
      FSO(6) = FO(6)+SC(3)
C
      PF(0)=REAL( FSO(5))
      PF(1)=REAL( FSO(6))
      PF(2)=AIMAG(FSO(6))
      PF(3)=AIMAG(FSO(5))
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      DS=-SC(1)/CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      P0P3=PF(0)+PF(3)
      P0M3=PF(0)-PF(3)
      SL1=GC(2)*(P0P3*FO(3)      +FSO(6) *FO(4))
      SL2=GC(2)*(P0M3*FO(4)+CONJG(FSO(6))*FO(3))
      SR1=GC(1)*(P0M3*FO(1)      -FSO(6) *FO(2))
      SR2=GC(1)*(P0P3*FO(2)-CONJG(FSO(6))*FO(1))
C
      FSO(1) = ( GC(1)*FMASS*FO(1) + SL1 )*DS
      FSO(2) = ( GC(1)*FMASS*FO(2) + SL2 )*DS
      FSO(3) = ( GC(2)*FMASS*FO(3) + SR1 )*DS
      FSO(4) = ( GC(2)*FMASS*FO(4) + SR2 )*DS
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FVIXXX(FI,VC,G,FMASS,FWIDTH , FVI)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-IN external fermion and a vector boson.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex VC(6)          : input    vector                      V
C       real    G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FVI(6)         : off-shell fermion             |F',V,FI>
C
      COMPLEX FI(6),VC(6),FVI(6),SL1,SL2,SR1,SR2,D,CI
      REAL    G(2),PF(0:3),FMASS,FWIDTH,PF2
C
      FVI(5) = FI(5)-VC(5)
      FVI(6) = FI(6)-VC(6)
C
      PF(0)=REAL( FVI(5))
      PF(1)=REAL( FVI(6))
      PF(2)=AIMAG(FVI(6))
      PF(3)=AIMAG(FVI(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FI(1)
     &    +(VC(2)-CI*VC(3))*FI(2)
      SL2= (VC(2)+CI*VC(3))*FI(1)
     &    +(VC(1)-   VC(4))*FI(2)
C
      IF (G(2).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FI(3)
     &    -(VC(2)-CI*VC(3))*FI(4)
      SR2=-(VC(2)+CI*VC(3))*FI(3)
     &    +(VC(1)+   VC(4))*FI(4)
C
      FVI(1) = ( G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVI(2) = ( G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
      FVI(3) = ( G(2)*((PF(0)+PF(3))*SR1 +CONJG(FVI(6))*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVI(4) = ( G(2)*(       FVI(6)*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
C
      RETURN
C
  10  FVI(1) = G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)*D
      FVI(2) = G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)*D
      FVI(3) = G(1)*FMASS*SL1*D
      FVI(4) = G(1)*FMASS*SL2*D
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE FVOXXX(FO,VC,G,FMASS,FWIDTH , FVO)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-OUT external fermion and a vector boson.
C
C INPUT:
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V
C       real    G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FVO(6)         : off-shell fermion             <FO,V,F'|
C
      COMPLEX FO(6),VC(6),FVO(6),SL1,SL2,SR1,SR2,D,CI
      REAL    G(2),PF(0:3),FMASS,FWIDTH,PF2
C
      FVO(5) = FO(5)+VC(5)
      FVO(6) = FO(6)+VC(6)
C
      PF(0)=REAL( FVO(5))
      PF(1)=REAL( FVO(6))
      PF(2)=AIMAG(FVO(6))
      PF(3)=AIMAG(FVO(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FO(3)
     &    +(VC(2)+CI*VC(3))*FO(4)
      SL2= (VC(2)-CI*VC(3))*FO(3)
     &    +(VC(1)-   VC(4))*FO(4)
C
      IF (G(2).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FO(1)
     &    -(VC(2)+CI*VC(3))*FO(2)
      SR2=-(VC(2)-CI*VC(3))*FO(1)
     &    +(VC(1)+   VC(4))*FO(2)
C
      FVO(1) = ( G(2)*( (PF(0)+PF(3))*SR1        +FVO(6)*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVO(2) = ( G(2)*( CONJG(FVO(6))*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
      FVO(3) = ( G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVO(4) = ( G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
C
      RETURN
C
  10  FVO(1) = G(1)*FMASS*SL1*D
      FVO(2) = G(1)*FMASS*SL2*D
      FVO(3) = G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)*D
      FVO(4) = G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)*D
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HIOXXX(FI,FO,GC,SMASS,SWIDTH , HIO)
C
C This subroutine computes an off-shell scalar current from an external
C fermion pair.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex GC(2)          : coupling constants                 GCHF
C       real    SMASS          : mass  of OUTPUT scalar S
C       real    SWIDTH         : width of OUTPUT scalar S
C
C OUTPUT:
C       complex HIO(3)         : scalar current             J(<FI|S|FO>)
C
      COMPLEX FI(6),FO(6),HIO(3),GC(2),DN
      REAL    Q(0:3),SMASS,SWIDTH,Q2
C
      HIO(2) = FO(5)-FI(5)
      HIO(3) = FO(6)-FI(6)
C
      Q(0)=REAL( HIO(2))
      Q(1)=REAL( HIO(3))
      Q(2)=AIMAG(HIO(3))
      Q(3)=AIMAG(HIO(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DN=-CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HIO(1) = ( GC(1)*(FO(1)*FI(1)+FO(2)*FI(2))
     &          +GC(2)*(FO(3)*FI(3)+FO(4)*FI(4)) )/DN
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HSSSXX(S1,S2,S3,G,SMASS,SWIDTH , HSSS)
C
C This subroutine computes an off-shell scalar current from the four-
C scalar coupling.
C
C INPUT:
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       real    G              : coupling constant                 GHHHH
C       real    SMASS          : mass  of OUTPUT scalar S'
C       real    SWIDTH         : width of OUTPUT scalar S'
C
C OUTPUT:
C       complex HSSS(3)        : scalar current           J(S':S1,S2,S3)
C
      COMPLEX S1(3),S2(3),S3(3),HSSS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
      HSSS(2) = S1(2)+S2(2)+S3(2)
      HSSS(3) = S1(3)+S2(3)+S3(3)
C
      Q(0)=REAL( HSSS(2))
      Q(1)=REAL( HSSS(3))
      Q(2)=AIMAG(HSSS(3))
      Q(3)=AIMAG(HSSS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HSSS(1) = DG * S1(1)*S2(1)*S3(1)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HSSXXX(S1,S2,G,SMASS,SWIDTH , HSS)
C
C This subroutine computes an off-shell scalar current from the three-
C scalar coupling.
C
C INPUT:
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                  GHHH
C       real    SMASS          : mass  of OUTPUT scalar S'
C       real    SWIDTH         : width of OUTPUT scalar S'
C
C OUTPUT:
C       complex HSS(3)         : scalar current              J(S':S1,S2)
C
      COMPLEX S1(3),S2(3),HSS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
      HSS(2) = S1(2)+S2(2)
      HSS(3) = S1(3)+S2(3)
C
      Q(0)=REAL( HSS(2))
      Q(1)=REAL( HSS(3))
      Q(2)=AIMAG(HSS(3))
      Q(3)=AIMAG(HSS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HSS(1) = DG*S1(1)*S2(1)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HVSXXX(VC,SC,G,SMASS,SWIDTH , HVS)
C
C This subroutine computes an off-shell scalar current from the vector-
C scalar-scalar coupling.  The coupling is absent in the minimal SM in
C unitary gauge.
C
C INPUT:
C       complex VC(6)          : input vector                          V
C       complex SC(3)          : input scalar                          S
C       real    G              : coupling constant (S charge)
C       real    SMASS          : mass  of OUTPUT scalar S'
C       real    SWIDTH         : width of OUTPUT scalar S'
C
C Examples of the coupling constant G for SUSY particles are as follows:
C   -----------------------------------------------------------
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |
C   -----------------------------------------------------------
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |
C   -----------------------------------------------------------
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |
C   -----------------------------------------------------------
C where the SC charge is defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex HVS(3)         : scalar current                J(S':V,S)
C
      COMPLEX VC(6),SC(3),HVS(3),DG,QVV,QPV
      REAL    QV(0:3),QP(0:3),QA(0:3),G,SMASS,SWIDTH,Q2
C
      HVS(2) = VC(5)+SC(2)
      HVS(3) = VC(6)+SC(3)
C
      QV(0)=REAL(  VC(2))
      QV(1)=REAL(  VC(3))
      QV(2)=AIMAG( VC(3))
      QV(3)=AIMAG( VC(2))
      QP(0)=REAL(  SC(2))
      QP(1)=REAL(  SC(3))
      QP(2)=AIMAG( SC(3))
      QP(3)=AIMAG( SC(2))
      QA(0)=REAL( HVS(2))
      QA(1)=REAL( HVS(3))
      QA(2)=AIMAG(HVS(3))
      QA(3)=AIMAG(HVS(2))
      Q2=QA(0)**2-(QA(1)**2+QA(2)**2+QA(3)**2)
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
      QVV=QV(0)*VC(1)-QV(1)*VC(2)-QV(2)*VC(3)-QV(3)*VC(4)
      QPV=QP(0)*VC(1)-QP(1)*VC(2)-QP(2)*VC(3)-QP(3)*VC(4)
C
      HVS(1) = DG*(2.*QPV+QVV)*SC(1)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HVVSXX(V1,V2,SC,G,SMASS,SWIDTH , HVVS)
C
C This subroutine computes an off-shell scalar current of the vector-
C vector-scalar-scalar coupling.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex SC(3)          : input  scalar                        S
C       real    G              : coupling constant                 GVVHH
C       real    SMASS          : mass  of OUTPUT scalar S'
C       real    SWIDTH         : width of OUTPUT scalar S'
C
C OUTPUT:
C       complex HVVS(3)        : scalar current            J(S':V1,V2,S)
C
      COMPLEX V1(6),V2(6),SC(3),HVVS(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
      HVVS(2) = V1(5)+V2(5)+SC(2)
      HVVS(3) = V1(6)+V2(6)+SC(3)
C
      Q(0)=REAL( HVVS(2))
      Q(1)=REAL( HVVS(3))
      Q(2)=AIMAG(HVVS(3))
      Q(3)=AIMAG(HVVS(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HVVS(1) = DG * SC(1)
     &         *(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE HVVXXX(V1,V2,G,SMASS,SWIDTH , HVV)
C
C This subroutine computes an off-shell scalar current from the vector-
C vector-scalar coupling.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       real    G              : coupling constant                  GVVH
C       real    SMASS          : mass  of OUTPUT scalar S
C       real    SWIDTH         : width of OUTPUT scalar S
C
C OUTPUT:
C       complex HVV(3)         : off-shell scalar current     J(S:V1,V2)
C
      COMPLEX V1(6),V2(6),HVV(3),DG
      REAL    Q(0:3),G,SMASS,SWIDTH,Q2
C
      HVV(2) = V1(5)+V2(5)
      HVV(3) = V1(6)+V2(6)
C
      Q(0)=REAL( HVV(2))
      Q(1)=REAL( HVV(3))
      Q(2)=AIMAG(HVV(3))
      Q(3)=AIMAG(HVV(2))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
C
      DG=-G/CMPLX( Q2-SMASS**2 , MAX(SIGN( SMASS*SWIDTH ,Q2),0.) )
C
      HVV(1) = DG*(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE IOSXXX(FI,FO,SC,GC , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-scalar
C coupling.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex SC(3)          : input    scalar                      S
C       complex GC(2)          : coupling constants                 GCHF
C
C OUTPUT:
C       complex VERTEX         : amplitude                     <FO|S|FI>
C
      COMPLEX FI(6),FO(6),SC(3),GC(2),VERTEX
C
      VERTEX = SC(1)*( GC(1)*(FI(1)*FO(1)+FI(2)*FO(2))
     &                +GC(2)*(FI(3)*FO(3)+FI(4)*FO(4)) )
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE IOVXXX(FI,FO,VC,G , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-vector
C coupling.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V
C       real    G(2)           : coupling constants                  GVF
C
C OUTPUT:
C       complex VERTEX         : amplitude                     <FO|V|FI>
C
      COMPLEX FI(6),FO(6),VC(6),VERTEX
      REAL    G(2)
C
      VERTEX =  G(1)*( (FO(3)*FI(1)+FO(4)*FI(2))*VC(1)
     &                +(FO(3)*FI(2)+FO(4)*FI(1))*VC(2)
     &                -(FO(3)*FI(2)-FO(4)*FI(1))*VC(3)*CMPLX(0.,1.)
     &                +(FO(3)*FI(1)-FO(4)*FI(2))*VC(4)             )
      IF (G(2).NE.0.) VERTEX = VERTEX
     &        + G(2)*( (FO(1)*FI(3)+FO(2)*FI(4))*VC(1)
     &                -(FO(1)*FI(4)+FO(2)*FI(3))*VC(2)
     &                +(FO(1)*FI(4)-FO(2)*FI(3))*VC(3)*CMPLX(0.,1.)
     &                -(FO(1)*FI(3)-FO(2)*FI(4))*VC(4)             )
C
      RETURN
      END
C  *********************************************************************
C  *********************************************************************
C  ***                                                               ***
C  ***                                                               ***
C  ***   HHH    HH   EEEEEEEEE   LL              AA        SSSSS     ***
C  ***   HHH    HH   EEEEEEEEE   LL             AAAA      SSSSSSS    ***
C  ***   HHH    HH   EE          LL            AA  AA    SS     SS   ***
C  ***   HHH    HH   EE          LL           AA    AA   SS     SS   ***
C  ***   HHH    HH   EE          LL          AAA    AA   SS          ***
C  ***   HHH    HH   EE          LL          AAA    AA   SS          ***
C  ***   HHHHHHHHH   EEEEEEE     LL          AAA    AA     SSS       ***
C  ***   HHHHHHHHH   EEEEEEE     LL          AAA    AA       SSS     ***
C  ***   HHH    HH   EE          LL          AAA    AA          SS   ***
C  ***   HHH    HH   EE          LL          AAAAAAAAA          SS   ***
C  ***   HHH    HH   EE          LL          AAAAAAAAA   SS     SS   ***
C  ***   HHH    HH   EE          LL          AAA    AA   SS     SS   ***
C  ***   HHH    HH   EE          LL          AAA    AA   SS     SS   ***
C  ***   HHH    HH   EEEEEEEEE   LLLLLLLLL   AAA    AA    SSSSSSS    ***
C  ***   HHH    HH   EEEEEEEEE   LLLLLLLLL   AAA    AA     SSSSS     ***
C  ***                                                               ***
C  ***               coded by H. Murayama & I. Watanabe              ***
C  ***                          ver.  2.4                            ***
C  ***                       12th  Apr.  1992                        ***
C  ***                                                               ***
C  *** For the formalism and notations, see the following reference: ***
C  ***           H. Murayama, I. Watanabe and K. Hagiwara            ***
C  ***           "HELAS: HELicity Amplitude Subroutines              ***
C  ***               for Feynman diagram evaluation"                 ***
C  ***               KEK Report 91-11, December 1991                 ***
C  ***                                                               ***
C  *********************************************************************
C  *********************************************************************
C
C  The subroutines are named as follows.
C
C  External Lines:
C   | f >                     : fermion (flow-IN)            ==>  IXXXXX
C   < f |                     : fermion (flow-OUT)           ==>  OXXXXX
C   epsilon^mu , epsilon*^mu  : vector boson (initial,final) ==>  VXXXXX
C   S                         : scalar boson (initial,final) ==>  SXXXXX
C
C  Vertices:
C   < f' V f >            : amplitude        of   FFV vertex ==>  IOVXXX
C   | f' V f >            : flow-in  fermion from FFV vertex ==>  FVIXXX
C   < f V f' |            : flow-out fermion from FFV vertex ==>  FVOXXX
C   J^mu(< f' | V | f >)  : vector   current from FFV vertex ==>  JIOXXX
C                         : W3       current from FFV vertex ==>  J3XXXX
C   < f' S f >            : amplitude        of   FFS vertex ==>  IOSXXX
C   | f' S f >            : flow-in  fermion from FFS vertex ==>  FSIXXX
C   < f S f' |            : flow-out fermion from FFS vertex ==>  FSOXXX
C   J(< f' | S | f >)     : scalar   current from FFS vertex ==>  HIOXXX
C   Gamma(V1,V2,V3)         : amplitude      of   VVV vertex ==>  VVVXXX
C   J^mu(V':V1,V2)          : vector current from VVV vertex ==>  JVVXXX
C   Gamma(V1,V2,S)          : amplitude      of   VVS vertex ==>  VVSXXX
C   J^mu(V':V,S)            : vector current from VVS vertex ==>  JVSXXX
C   J(S:V1,V2)              : scalar current from VVS vertex ==>  HVVXXX
C   Gamma(V,S1,S2)          : amplitude      of   VSS vertex ==>  VSSXXX
C   J^mu(V:S1,S2)           : vector current from VSS vertex ==>  JSSXXX
C   J(S':V,S)               : scalar current from VSS vertex ==>  HVSXXX
C   Gamma(S1,S2,S3)         : amplitude      of   SSS vertex ==>  SSSXXX
C   J(S':S1,S2)             : scalar current from SSS vertex ==>  HSSXXX
C   Gamma(WM,WP,WM,WP) : amplitude of   4-point W+/W- vertex ==>  WWWWXX
C   J^mu(W':W1,W2,W3)  : W current from 4-point W+/W- vertex ==>  JWWWXX
C   Gamma(WM,W3,WP,W3) : amplitude of   4-point W/W3  vertex ==>  W3W3XX
C   J^mu(W':W1,W2,W3)  : W current from 4-point W/W3  vertex ==>  JW3WXX
C   Gamma(V1,V2,S1,S2)   : amplitude        of   VVSS vertex ==>  VVSSXX
C   J^mu(V':V,S1,S2)     : vector current   from VVSS vertex ==>  JVSSXX
C   J(S':V1,V2,S)        : scalar current   from VVSS vertex ==>  HVVSXX
C   Gamma(S1,S2,S3,S4)   : amplitude        of   SSSS vertex ==>  SSSSXX
C   J(S':S1,S2,S3)       : scalar current   from SSSS vertex ==>  HSSSXX
C
C  Special Vertices:
C   | e' A e- >               : initial electron with photon ==>  EAIXXX
C   < e+ A e' |               : initial positron with photon ==>  EAOXXX
C   J^mu(< e+ | A | e->)      : t-channel photon from e-/e+  ==>  JEEXXX
C
C  Utilities for Momentum Manipulations:
C   P^mu(energy,mass,costh,phi)          : set up 4-momentum ==>  MOMNTX
C   P1^mu & P2^mu   : set up two 4-momenta in 1 2 rest frame ==>  MOM2CX
C   P_boosted                  : Lorentz boost of 4-momentum ==>  BOOSTX
C   P_rotated                       : rotation of 4-momentum ==>  ROTXXX
C
C  Standard Model Coupling Constants:
C   for VVV,VVVV vertices                                    ==>  COUP1X
C   for FFV vertices                                         ==>  COUP2X
C   for VVS,SSS,VVSS,SSSS vertices                           ==>  COUP3X
C   for FFS vertices                                         ==>  COUP4X
C
C **********************************************************************
C
      SUBROUTINE IXXXXX(P,FMASS,NHEL,NSF , FI)
C
C This subroutine computes a fermion wavefunction with the flowing-IN
C fermion number.
C
C INPUT:
C       real    P(0:3)         : four-momentum of fermion
C       real    FMASS          : mass          of fermion
C       integer NHEL = -1 or 1 : helicity      of fermion
C       integer NSF  = -1 or 1 : +1 for particle, -1 for anti-particle
C
C OUTPUT:
C       complex FI(6)          : fermion wavefunction               |FI>
C
      COMPLEX FI(6),CHI(2)
      REAL    P(0:3),SF(2),SFOMEG(2),OMEGA(2),FMASS,PP,PP3,SQP0P3,SQM
      INTEGER NHEL,NSF,IP,IM,NH
C
      FI(5) = CMPLX(P(0),P(3))*NSF
      FI(6) = CMPLX(P(1),P(2))*NSF
C
      NH=NHEL*NSF
C
      IF (FMASS.EQ.0.) GOTO 10
C
      PP=MIN(P(0),SQRT(P(1)**2+P(2)**2+P(3)**2))
C
      IF (PP.EQ.0.) GOTO 20
C
      SF(1)=REAL(1+NSF+(1-NSF)*NH)*.5
      SF(2)=REAL(1+NSF-(1-NSF)*NH)*.5
      OMEGA(1)=SQRT(P(0)+PP)
      OMEGA(2)=FMASS/OMEGA(1)
      IP=(3+NH)/2
      IM=(3-NH)/2
      SFOMEG(1)=SF(1)*OMEGA(IP)
      SFOMEG(2)=SF(2)*OMEGA(IM)
      PP3=MAX(PP+P(3),0.)
      CHI(1)=CMPLX( SQRT(PP3*.5/PP) )
      IF (PP3.EQ.0.) THEN
         CHI(2)=CMPLX(-NH )
      ELSE
         CHI(2)=CMPLX( NH*P(1) , P(2) )/SQRT(2.*PP*PP3)
      ENDIF
C
      FI(1) = SFOMEG(1)*CHI(IM)
      FI(2) = SFOMEG(1)*CHI(IP)
      FI(3) = SFOMEG(2)*CHI(IM)
      FI(4) = SFOMEG(2)*CHI(IP)
C
      RETURN
C
  10  SQP0P3=SQRT(MAX(P(0)+P(3),0.))*NSF
      CHI(1)=CMPLX( SQP0P3 )
      IF (SQP0P3.EQ.0.) THEN
         CHI(2)=CMPLX(-NHEL )*SQRT(2.*P(0))
      ELSE
         CHI(2)=CMPLX( NH*P(1), P(2) )/SQP0P3
      ENDIF
      IF (NH.EQ.1) THEN
         FI(1) = CMPLX( 0. )
         FI(2) = CMPLX( 0. )
         FI(3) = CHI(1)
         FI(4) = CHI(2)
      ELSE
         FI(1) = CHI(2)
         FI(2) = CHI(1)
         FI(3) = CMPLX( 0. )
         FI(4) = CMPLX( 0. )
      ENDIF
      RETURN
C
  20  SQM=SQRT(FMASS)
      IP=(1+NH)/2
      IM=(1-NH)/2
C
      FI(1) = IP     * SQM
      FI(2) = IM*NSF * SQM
      FI(3) = IP*NSF * SQM
      FI(4) = IM     * SQM
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE J3XXXX(FI,FO,GAF,GZF,ZMASS,ZWIDTH , J3)
C
C This subroutine computes the sum of photon and Z currents with the
C suitable weights ( J(W3) = cos(theta_W) J(Z) + sin(theta_W) J(A) ).
C The output J3 is useful as an input of VVVXXX, JVVXXX or W3W3XX.
C The photon propagator is given in Feynman gauge, and the Z propagator
C is given in unitary gauge.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       real    GAF(2)         : FI couplings with A                 GAF
C       real    GZF(2)         : FI couplings with Z                 GZF
C       real    ZMASS          : mass  of Z
C       real    ZWIDTH         : width of Z
C
C OUTPUT:
C       complex J3(6)          : W3 current             J^mu(<FO|W3|FI>)
C
      COMPLEX FI(6),FO(6),J3(6),
     &        C0L,C1L,C2L,C3L,CSL,C0R,C1R,C2R,C3R,CSR,DZ,DDIF
      REAL    GAF(2),GZF(2),Q(0:3),ZMASS,ZWIDTH,ZM2,ZMW,Q2,DA,WW,
     &        CW,SW,GN,GZ3L,GA3L
C
      J3(5) = FO(5)-FI(5)
      J3(6) = FO(6)-FI(6)
C
      Q(0)=-REAL( J3(5))
      Q(1)=-REAL( J3(6))
      Q(2)=-AIMAG(J3(6))
      Q(3)=-AIMAG(J3(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      ZM2=ZMASS**2
      ZMW=ZMASS*ZWIDTH
C
      DA=1./Q2
      WW=MAX(SIGN( ZMW ,Q2),0.)
      DZ=1./CMPLX( Q2-ZM2 , WW )
      DDIF=CMPLX( -ZM2 , WW )*DA*DZ
C DDIF is the difference : DDIF=DA-DZ
C  For the running width, use below instead of the above WW,DZ and DDIF.
C      WW=MAX( ZWIDTH*Q2/ZMASS ,0.)
C      DZ=1./CMPLX( Q2-ZM2 , WW )
C      DDIF=CMPLX( -ZM2 , WW )*DA*DZ
C
      CW=1./SQRT(1.+(GZF(2)/GAF(2))**2)
      SW=SQRT((1.-CW)*(1.+CW))
      GN=GAF(2)*SW
      GZ3L=GZF(1)*CW
      GA3L=GAF(1)*SW
      C0L=  FO(3)*FI(1)+FO(4)*FI(2)
      C0R=  FO(1)*FI(3)+FO(2)*FI(4)
      C1L=-(FO(3)*FI(2)+FO(4)*FI(1))
      C1R=  FO(1)*FI(4)+FO(2)*FI(3)
      C2L= (FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,1.)
      C2R=(-FO(1)*FI(4)+FO(2)*FI(3))*CMPLX(0.,1.)
      C3L= -FO(3)*FI(1)+FO(4)*FI(2)
      C3R=  FO(1)*FI(3)-FO(2)*FI(4)
      CSL=(Q(0)*C0L-Q(1)*C1L-Q(2)*C2L-Q(3)*C3L)/ZM2
      CSR=(Q(0)*C0R-Q(1)*C1R-Q(2)*C2R-Q(3)*C3R)/ZM2
C
      J3(1) =  GZ3L*DZ*(C0L-CSL*Q(0))+GA3L*C0L*DA
     &       + GN*(C0R*DDIF-CSR*Q(0)*DZ)
      J3(2) =  GZ3L*DZ*(C1L-CSL*Q(1))+GA3L*C1L*DA
     &       + GN*(C1R*DDIF-CSR*Q(1)*DZ)
      J3(3) =  GZ3L*DZ*(C2L-CSL*Q(2))+GA3L*C2L*DA
     &       + GN*(C2R*DDIF-CSR*Q(2)*DZ)
      J3(4) =  GZ3L*DZ*(C3L-CSL*Q(3))+GA3L*C3L*DA
     &       + GN*(C3R*DDIF-CSR*Q(3)*DZ)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JEEXXX(EB,EF,SHLF,CHLF,PHI,NHB,NHF,NSF , JEE)
C
C This subroutine computes an off-shell photon wavefunction emitted from
C the electron or positron beam, with a special care for the small angle
C region.  The momenta are measured in the laboratory frame, where the
C e- (e+) beam is along the positive (negative) z axis.
C
C INPUT:
C       real    EB             : energy (GeV)    of beam  e-/e+
C       real    EF             : energy (GeV)    of final e-/e+
C       real    SHLF           : sin(theta/2)    of final e-/e+
C       real    CHLF           : cos(theta/2)    of final e-/e+
C       real    PHI            : azimuthal angle of final e-/e+
C       integer NHB  = -1 or 1 : helicity        of beam  e-/e+
C       integer NHF  = -1 or 1 : helicity        of final e-/e+
C       integer NSF  = -1 or 1 : +1 for electron, -1 for positron
C
C OUTPUT:
C       complex JEE(6)         : off-shell photon          J^mu(<e|A|e>)
C
      COMPLEX JEE(6),COEFF
      REAL    CS(2),EB,EF,SHLF,CHLF,PHI,ME,ALPHA,GAL,HI,SF,SFH,X,ME2,Q2,
     &        RFP,RFM,SNP,CSP,RXC,C,S
      INTEGER NHB,NHF,NSF
C
      ME   =REAL(0.51099906D-3)
      ALPHA=1./128.
      GAL  =SQRT(ALPHA*4.*REAL(3.14159265D0))
C
      HI =REAL(NHB)
      SF =REAL(NSF)
      SFH=REAL(NHB*NSF)
      CS((3+NSF)/2)=SHLF
      CS((3-NSF)/2)=CHLF
C CS(1)=CHLF and CS(2)=SHLF for electron
C CS(1)=SHLF and CS(2)=CHLF for positron
      X=EF/EB
      ME2=ME**2
      Q2=-4.*CS(2)**2*(EF*EB-ME2)
     &   +SF*(1.-X)**2/X*(SHLF+CHLF)*(SHLF-CHLF)*ME2
      RFP=REAL(1+NSF)
      RFM=REAL(1-NSF)
      SNP=SIN(PHI)
      CSP=COS(PHI)
C
      IF (NHB.EQ.NHF) THEN
         RXC=2.*X/(1.-X)*CS(1)**2
         COEFF= GAL*2.*EB*SQRT(X)*CS(2)/Q2
     &         *(CMPLX( RFP )-RFM*CMPLX( CSP ,-SNP*HI ))*.5
         JEE(1) =  CMPLX( 0. )
         JEE(2) =  COEFF*CMPLX( (1.+RXC)*CSP ,-SFH*SNP )
         JEE(3) =  COEFF*CMPLX( (1.+RXC)*SNP , SFH*CSP )
         JEE(4) =  COEFF*(-SF*RXC/CS(1)*CS(2))
      ELSE
         COEFF= GAL*ME/Q2/SQRT(X)
     &         *(CMPLX( RFP )+RFM*CMPLX( CSP , SNP*HI ))*.5*HI
         JEE(1) = -COEFF*(1.+X)*CS(2)*CMPLX( CSP , SFH*SNP )
         JEE(2) =  COEFF*(1.-X)*CS(1)
         JEE(3) =  JEE(2)*CMPLX( 0. , SFH )
         JEE(4) =  JEE(1)*SF*(1.-X)/(1.+X)
      ENDIF
C
      C=(CHLF+SHLF)*(CHLF-SHLF)
      S=2.*CHLF*SHLF
C
      JEE(5) = -EB*CMPLX( 1.-X , SF-X*C )
      JEE(6) =  EB*X*S*CMPLX( CSP , SNP )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JIOXXX(FI,FO,G,VMASS,VWIDTH , JIO)
C
C This subroutine computes an off-shell vector current from an external
C fermion pair.  The vector boson propagator is given in Feynman gauge
C for a massless vector and in unitary gauge for a massive vector.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       real    G(2)           : coupling constants                  GVF
C       real    VMASS          : mass  of OUTPUT vector V
C       real    VWIDTH         : width of OUTPUT vector V
C
C OUTPUT:
C       complex JIO(6)         : vector current          J^mu(<FO|V|FI>)
C
      COMPLEX FI(6),FO(6),JIO(6),C0,C1,C2,C3,CS,D
      REAL    G(2),Q(0:3),VMASS,VWIDTH,Q2,VM2,DD
C
      JIO(5) = FO(5)-FI(5)
      JIO(6) = FO(6)-FI(6)
C
      Q(0)=REAL( JIO(5))
      Q(1)=REAL( JIO(6))
      Q(2)=AIMAG(JIO(6))
      Q(3)=AIMAG(JIO(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 50
C
      D=1./CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above D.
C      D=1./CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      IF (G(2).EQ.0.) GOTO 10
C
      C0=  G(1)*( FO(3)*FI(1)+FO(4)*FI(2))
     &    +G(2)*( FO(1)*FI(3)+FO(2)*FI(4))
      C1= -G(1)*( FO(3)*FI(2)+FO(4)*FI(1))
     &    +G(2)*( FO(1)*FI(4)+FO(2)*FI(3))
      C2=( G(1)*( FO(3)*FI(2)-FO(4)*FI(1))
     &    +G(2)*(-FO(1)*FI(4)+FO(2)*FI(3)))*CMPLX(0.,1.)
      C3=  G(1)*(-FO(3)*FI(1)+FO(4)*FI(2))
     &    +G(2)*( FO(1)*FI(3)-FO(2)*FI(4))
      CS=(Q(0)*C0-Q(1)*C1-Q(2)*C2-Q(3)*C3)/VM2
C
      JIO(1) = (C0-CS*Q(0))*D
      JIO(2) = (C1-CS*Q(1))*D
      JIO(3) = (C2-CS*Q(2))*D
      JIO(4) = (C3-CS*Q(3))*D
C
      RETURN
C
  10  D=D*G(1)
      C0=  FO(3)*FI(1)+FO(4)*FI(2)
      C1= -FO(3)*FI(2)-FO(4)*FI(1)
      C2=( FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,1.)
      C3= -FO(3)*FI(1)+FO(4)*FI(2)
      CS=(Q(0)*C0-Q(1)*C1-Q(2)*C2-Q(3)*C3)/VM2
C
      JIO(1) = (C0-CS*Q(0))*D
      JIO(2) = (C1-CS*Q(1))*D
      JIO(3) = (C2-CS*Q(2))*D
      JIO(4) = (C3-CS*Q(3))*D
C
      RETURN
C
  50  CONTINUE
C
      DD=1./Q2
C
      IF (G(2).EQ.0.) GOTO 60
C
      JIO(1) = ( G(1)*( FO(3)*FI(1)+FO(4)*FI(2))
     &          +G(2)*( FO(1)*FI(3)+FO(2)*FI(4)) )*DD
      JIO(2) = (-G(1)*( FO(3)*FI(2)+FO(4)*FI(1))
     &          +G(2)*( FO(1)*FI(4)+FO(2)*FI(3)) )*DD
      JIO(3) = ( G(1)*( FO(3)*FI(2)-FO(4)*FI(1))
     &          +G(2)*(-FO(1)*FI(4)+FO(2)*FI(3)) )*CMPLX(0.,DD)
      JIO(4) = ( G(1)*(-FO(3)*FI(1)+FO(4)*FI(2))
     &          +G(2)*( FO(1)*FI(3)-FO(2)*FI(4)) )*DD
C
      RETURN
C
  60  DD=DD*G(1)
C
      JIO(1) =  ( FO(3)*FI(1)+FO(4)*FI(2))*DD
      JIO(2) = -( FO(3)*FI(2)+FO(4)*FI(1))*DD
      JIO(3) =  ( FO(3)*FI(2)-FO(4)*FI(1))*CMPLX(0.,DD)
      JIO(4) =  (-FO(3)*FI(1)+FO(4)*FI(2))*DD
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JSSXXX(S1,S2,G,VMASS,VWIDTH , JSS)
C
C This subroutine computes an off-shell vector current from the vector-
C scalar-scalar coupling.  The coupling is absent in the minimal SM in
C unitary gauge.  The propagator is given in Feynman gauge for a
C massless vector and in unitary gauge for a massive vector.
C
C INPUT:
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant (S1 charge)
C       real    VMASS          : mass  of OUTPUT vector V
C       real    VWIDTH         : width of OUTPUT vector V
C
C Examples of the coupling constant G for SUSY particles are as follows:
C   -----------------------------------------------------------
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |
C   -----------------------------------------------------------
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |
C   -----------------------------------------------------------
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |
C   -----------------------------------------------------------
C where the S1 charge is defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex JSS(6)         : vector current            J^mu(V:S1,S2)
C
      COMPLEX S1(3),S2(3),JSS(6),DG,ADG
      REAL    PP(0:3),PA(0:3),Q(0:3),G,VMASS,VWIDTH,Q2,VM2,MP2,MA2,M2D
C
      JSS(5) = S1(2)+S2(2)
      JSS(6) = S1(3)+S2(3)
C
      Q(0)=REAL( JSS(5))
      Q(1)=REAL( JSS(6))
      Q(2)=AIMAG(JSS(6))
      Q(3)=AIMAG(JSS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G/CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above DG.
C      DG=G/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      ADG=DG*S1(1)*S2(1)
C
      PP(0)=REAL( S1(2))
      PP(1)=REAL( S1(3))
      PP(2)=AIMAG(S1(3))
      PP(3)=AIMAG(S1(2))
      PA(0)=REAL( S2(2))
      PA(1)=REAL( S2(3))
      PA(2)=AIMAG(S2(3))
      PA(3)=AIMAG(S2(2))
      MP2=PP(0)**2-(PP(1)**2+PP(2)**2+PP(3)**2)
      MA2=PA(0)**2-(PA(1)**2+PA(2)**2+PA(3)**2)
      M2D=MP2-MA2
C
      JSS(1) = ADG*( (PP(0)-PA(0)) - Q(0)*M2D/VM2)
      JSS(2) = ADG*( (PP(1)-PA(1)) - Q(1)*M2D/VM2)
      JSS(3) = ADG*( (PP(2)-PA(2)) - Q(2)*M2D/VM2)
      JSS(4) = ADG*( (PP(3)-PA(3)) - Q(3)*M2D/VM2)
C
      RETURN
C
  10  ADG=G*S1(1)*S2(1)/Q2
C
      JSS(1) = ADG*REAL( S1(2)-S2(2))
      JSS(2) = ADG*REAL( S1(3)-S2(3))
      JSS(3) = ADG*AIMAG(S1(3)-S2(3))
      JSS(4) = ADG*AIMAG(S1(2)-S2(2))
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JVSSXX(VC,S1,S2,G,VMASS,VWIDTH , JVSS)
C
C This subroutine computes an off-shell vector current from the vector-
C vector-scalar-scalar coupling.  The vector propagator is given in
C Feynman gauge for a massless vector and in unitary gauge for a massive
C vector.
C
C INPUT:
C       complex VC(6)          : input  vector                        V
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                 GVVHH
C       real    VMASS          : mass  of OUTPUT vector V'
C       real    VWIDTH         : width of OUTPUT vector V'
C
C OUTPUT:
C       complex JVSS(6)        : vector current         J^mu(V':V,S1,S2)
C
      COMPLEX VC(6),S1(3),S2(3),JVSS(6),DG
      REAL    Q(0:3),G,VMASS,VWIDTH,Q2,VK,VM2
C
      JVSS(5) = VC(5)+S1(2)+S2(2)
      JVSS(6) = VC(6)+S1(3)+S2(3)
C
      Q(0)=REAL( JVSS(5))
      Q(1)=REAL( JVSS(6))
      Q(2)=AIMAG(JVSS(6))
      Q(3)=AIMAG(JVSS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G*S1(1)*S2(1)/CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above DG.
C      DG=G*S1(1)*S2(1)/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      VK=(Q(0)*VC(1)-Q(1)*VC(2)-Q(2)*VC(3)-Q(3)*VC(4))/VM2
C
      JVSS(1) = DG*(VC(1)-VK*Q(0))
      JVSS(2) = DG*(VC(2)-VK*Q(1))
      JVSS(3) = DG*(VC(3)-VK*Q(2))
      JVSS(4) = DG*(VC(4)-VK*Q(3))
C
      RETURN
C
  10  DG= G*S1(1)*S2(1)/Q2
C
      JVSS(1) = DG*VC(1)
      JVSS(2) = DG*VC(2)
      JVSS(3) = DG*VC(3)
      JVSS(4) = DG*VC(4)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JVSXXX(VC,SC,G,VMASS,VWIDTH , JVS)
C
C This subroutine computes an off-shell vector current from the vector-
C vector-scalar coupling.  The vector propagator is given in Feynman
C gauge for a massless vector and in unitary gauge for a massive vector.
C
C INPUT:
C       complex VC(6)          : input vector                          V
C       complex SC(3)          : input scalar                          S
C       real    G              : coupling constant                  GVVH
C       real    VMASS          : mass  of OUTPUT vector V'
C       real    VWIDTH         : width of OUTPUT vector V'
C
C OUTPUT:
C       complex JVS(6)         : vector current             J^mu(V':V,S)
C
      COMPLEX VC(6),SC(3),JVS(6),DG,VK
      REAL    Q(0:3),G,VMASS,VWIDTH,Q2,VM2
C
      JVS(5) = VC(5)+SC(2)
      JVS(6) = VC(6)+SC(3)
C
      Q(0)=REAL( JVS(5))
      Q(1)=REAL( JVS(6))
      Q(2)=AIMAG(JVS(6))
      Q(3)=AIMAG(JVS(5))
      Q2=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      IF (VMASS.EQ.0.) GOTO 10
C
      DG=G*SC(1)/CMPLX( Q2-VM2 , MAX(SIGN( VMASS*VWIDTH ,Q2),0.) )
C  For the running width, use below instead of the above DG.
C      DG=G*SC(1)/CMPLX( Q2-VM2 , MAX( VWIDTH*Q2/VMASS ,0.) )
C
      VK=(-Q(0)*VC(1)+Q(1)*VC(2)+Q(2)*VC(3)+Q(3)*VC(4))/VM2
C
      JVS(1) = DG*(Q(0)*VK+VC(1))
      JVS(2) = DG*(Q(1)*VK+VC(2))
      JVS(3) = DG*(Q(2)*VK+VC(3))
      JVS(4) = DG*(Q(3)*VK+VC(4))
C
      RETURN
C
  10  DG=G*SC(1)/Q2
C
      JVS(1) = DG*VC(1)
      JVS(2) = DG*VC(2)
      JVS(3) = DG*VC(3)
      JVS(4) = DG*VC(4)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JVVXXX(V1,V2,G,VMASS,VWIDTH , JVV)
C
C This subroutine computes an off-shell vector current from the three-
C point gauge boson coupling.  The vector propagator is given in Feynman
C gauge for a massless vector and in unitary gauge for a massive vector.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       real    G              : coupling constant (see the table below)
C       real    VMASS          : mass  of OUTPUT vector V
C       real    VWIDTH         : width of OUTPUT vector V
C
C The possible sets of the inputs are as follows:
C    ------------------------------------------------------------------
C    |   V1   |   V2   |  JVV   |      G       |   VMASS  |  VWIDTH   |
C    ------------------------------------------------------------------
C    |   W-   |   W+   |  A/Z   |  GWWA/GWWZ   | 0./ZMASS | 0./ZWIDTH |
C    | W3/A/Z |   W-   |  W+    | GW/GWWA/GWWZ |   WMASS  |  WWIDTH   |
C    |   W+   | W3/A/Z |  W-    | GW/GWWA/GWWZ |   WMASS  |  WWIDTH   |
C    ------------------------------------------------------------------
C where all the bosons are defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex JVV(6)         : vector current            J^mu(V:V1,V2)
C
      COMPLEX V1(6),V2(6),JVV(6),J12(0:3),JS,DG,
     &        SV1,SV2,S11,S12,S21,S22,V12
      REAL    P1(0:3),P2(0:3),Q(0:3),G,VMASS,VWIDTH,GS,S,VM2,M1,M2
C
      JVV(5) = V1(5)+V2(5)
      JVV(6) = V1(6)+V2(6)
C
      P1(0)=REAL( V1(5))
      P1(1)=REAL( V1(6))
      P1(2)=AIMAG(V1(6))
      P1(3)=AIMAG(V1(5))
      P2(0)=REAL( V2(5))
      P2(1)=REAL( V2(6))
      P2(2)=AIMAG(V2(6))
      P2(3)=AIMAG(V2(5))
      Q(0)=-REAL( JVV(5))
      Q(1)=-REAL( JVV(6))
      Q(2)=-AIMAG(JVV(6))
      Q(3)=-AIMAG(JVV(5))
      S=Q(0)**2-(Q(1)**2+Q(2)**2+Q(3)**2)
      VM2=VMASS**2
C
      V12=V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4)
      SV1= (P2(0)-Q(0))*V1(1) -(P2(1)-Q(1))*V1(2)
     &    -(P2(2)-Q(2))*V1(3) -(P2(3)-Q(3))*V1(4)
      SV2=-(P1(0)-Q(0))*V2(1) +(P1(1)-Q(1))*V2(2)
     &    +(P1(2)-Q(2))*V2(3) +(P1(3)-Q(3))*V2(4)
      J12(0)=(P1(0)-P2(0))*V12 +SV1*V2(1) +SV2*V1(1)
      J12(1)=(P1(1)-P2(1))*V12 +SV1*V2(2) +SV2*V1(2)
      J12(2)=(P1(2)-P2(2))*V12 +SV1*V2(3) +SV2*V1(3)
      J12(3)=(P1(3)-P2(3))*V12 +SV1*V2(4) +SV2*V1(4)
C
      IF (VMASS.EQ.0.) GOTO 10
C
      M1=P1(0)**2-(P1(1)**2+P1(2)**2+P1(3)**2)
      M2=P2(0)**2-(P2(1)**2+P2(2)**2+P2(3)**2)
      S11=P1(0)*V1(1)-P1(1)*V1(2)-P1(2)*V1(3)-P1(3)*V1(4)
      S12=P1(0)*V2(1)-P1(1)*V2(2)-P1(2)*V2(3)-P1(3)*V2(4)
      S21=P2(0)*V1(1)-P2(1)*V1(2)-P2(2)*V1(3)-P2(3)*V1(4)
      S22=P2(0)*V2(1)-P2(1)*V2(2)-P2(2)*V2(3)-P2(3)*V2(4)
      JS=(V12*(-M1+M2) +S11*S12 -S21*S22)/VM2
C
      DG=-G/CMPLX( S-VM2 , MAX(SIGN( VMASS*VWIDTH ,S),0.) )
C  For the running width, use below instead of the above DG.
C      DG=-G/CMPLX( S-VM2 , MAX( VWIDTH*S/VMASS ,0.) )
C
      JVV(1) = DG*(J12(0)-Q(0)*JS)
      JVV(2) = DG*(J12(1)-Q(1)*JS)
      JVV(3) = DG*(J12(2)-Q(2)*JS)
      JVV(4) = DG*(J12(3)-Q(3)*JS)
C
      RETURN
C
  10  GS=-G/S
C
      JVV(1) = GS*J12(0)
      JVV(2) = GS*J12(1)
      JVV(3) = GS*J12(2)
      JVV(4) = GS*J12(3)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JW3WXX(W1,W2,W3,G1,G2,WMASS,WWIDTH,VMASS,VWIDTH , JW3W)
C
C This subroutine computes an off-shell W+, W-, W3, Z or photon current
C from the four-point gauge boson coupling, including the contributions
C of W exchange diagrams.  The vector propagator is given in Feynman
C gauge for a photon and in unitary gauge for W and Z bosons.  If one
C sets WMASS=0.0, then the ggg-->g current is given (see sect 2.9.1 of
C the manual).
C
C INPUT:
C       complex W1(6)          : first  vector                        W1
C       complex W2(6)          : second vector                        W2
C       complex W3(6)          : third  vector                        W3
C       real    G1             : first  coupling constant
C       real    G2             : second coupling constant
C                                                  (see the table below)
C       real    WMASS          : mass  of internal W
C       real    WWIDTH         : width of internal W
C       real    VMASS          : mass  of OUTPUT W'
C       real    VWIDTH         : width of OUTPUT W'
C
C The possible sets of the inputs are as follows:
C   -------------------------------------------------------------------
C   |  W1  |  W2  |  W3  | G1 | G2 |WMASS|WWIDTH|VMASS|VWIDTH || JW3W |
C   -------------------------------------------------------------------
C   |  W-  |  W3  |  W+  | GW |GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   |
C   |  W-  |  W3  |  W+  | GW |GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   |
C   |  W-  |  Z   |  W+  |GWWZ|GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   |
C   |  W-  |  Z   |  W+  |GWWZ|GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   |
C   |  W-  |  A   |  W+  |GWWA|GWWZ|WMASS|WWIDTH|ZMASS|ZWIDTH ||  Z   |
C   |  W-  |  A   |  W+  |GWWA|GWWA|WMASS|WWIDTH|  0. |  0.   ||  A   |
C   -------------------------------------------------------------------
C   |  W3  |  W-  |  W3  | GW | GW |WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W3  |  W+  |  W3  | GW | GW |WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  W3  |  W-  |  Z   | GW |GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W3  |  W+  |  Z   | GW |GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  W3  |  W-  |  A   | GW |GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W3  |  W+  |  A   | GW |GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  Z   |  W-  |  Z   |GWWZ|GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  Z   |  W+  |  Z   |GWWZ|GWWZ|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  Z   |  W-  |  A   |GWWZ|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  Z   |  W+  |  A   |GWWZ|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   |  A   |  W-  |  A   |GWWA|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W+  |
C   |  A   |  W+  |  A   |GWWA|GWWA|WMASS|WWIDTH|WMASS|WWIDTH ||  W-  |
C   -------------------------------------------------------------------
C where all the bosons are defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex JW3W(6)        : W current             J^mu(W':W1,W2,W3)
C
      COMPLEX*8  W1(6),W2(6),W3(6),JW3W(6)
      COMPLEX*16 DW1(0:3),DW2(0:3),DW3(0:3),
     &           JJ(0:3),JS(0:3),JT(0:3),J4(0:3),
     &           JT12(0:3),JT32(0:3),J12(0:3),J32(0:3),
     &           DWS,DWT,DV,W12,W32,W13,P1W2,P2W1,P3W2,P2W3,
     &           JK12,JK32,JSW3,JTW1,P3JS,KSW3,P1JT,KTW1,JQ
      REAL*4     G1,G2,WMASS,WWIDTH,VMASS,VWIDTH
      REAL*8     P1(0:3),P2(0:3),P3(0:3),Q(0:3),KS(0:3),KT(0:3),
     &           DG2,DMW,DWW,DMV,DWV,MW2,MV2,MW2INV,Q2,KS2,KT2
C
      JW3W(5) = W1(5)+W2(5)+W3(5)
      JW3W(6) = W1(6)+W2(6)+W3(6)
C
      DW1(0)=DCMPLX(W1(1))
      DW1(1)=DCMPLX(W1(2))
      DW1(2)=DCMPLX(W1(3))
      DW1(3)=DCMPLX(W1(4))
      DW2(0)=DCMPLX(W2(1))
      DW2(1)=DCMPLX(W2(2))
      DW2(2)=DCMPLX(W2(3))
      DW2(3)=DCMPLX(W2(4))
      DW3(0)=DCMPLX(W3(1))
      DW3(1)=DCMPLX(W3(2))
      DW3(2)=DCMPLX(W3(3))
      DW3(3)=DCMPLX(W3(4))
      P1(0)=DBLE(      W1(5))
      P1(1)=DBLE(      W1(6))
      P1(2)=DBLE(AIMAG(W1(6)))
      P1(3)=DBLE(AIMAG(W1(5)))
      P2(0)=DBLE(      W2(5))
      P2(1)=DBLE(      W2(6))
      P2(2)=DBLE(AIMAG(W2(6)))
      P2(3)=DBLE(AIMAG(W2(5)))
      P3(0)=DBLE(      W3(5))
      P3(1)=DBLE(      W3(6))
      P3(2)=DBLE(AIMAG(W3(6)))
      P3(3)=DBLE(AIMAG(W3(5)))
      Q(0)=-(P1(0)+P2(0)+P3(0))
      Q(1)=-(P1(1)+P2(1)+P3(1))
      Q(2)=-(P1(2)+P2(2)+P3(2))
      Q(3)=-(P1(3)+P2(3)+P3(3))
      KS(0)=P1(0)+P2(0)
      KS(1)=P1(1)+P2(1)
      KS(2)=P1(2)+P2(2)
      KS(3)=P1(3)+P2(3)
      KT(0)=P2(0)+P3(0)
      KT(1)=P2(1)+P3(1)
      KT(2)=P2(2)+P3(2)
      KT(3)=P2(3)+P3(3)
      Q2 =Q(0)**2 -(Q(1)**2 +Q(2)**2 +Q(3)**2)
      KS2=KS(0)**2-(KS(1)**2+KS(2)**2+KS(3)**2)
      KT2=KT(0)**2-(KT(1)**2+KT(2)**2+KT(3)**2)
      DG2=DBLE(G1)*DBLE(G2)
      DMW=DBLE(WMASS)
      DWW=DBLE(WWIDTH)
      DMV=DBLE(VMASS)
      DWV=DBLE(VWIDTH)
      MW2=DMW**2
      MV2=DMV**2
      MW2INV=1.0D0/MW2
C
      DWS=-DG2/DCMPLX( KS2-MW2 , DMAX1(DSIGN(DMW*DWW,KS2),0.D0) )
      DWT=-DG2/DCMPLX( KT2-MW2 , DMAX1(DSIGN(DMW*DWW,KT2),0.D0) )
      IF (VMASS.EQ.0.) THEN
      DV = 1.0D0/DCMPLX( Q2 )
      ELSE
      DV = 1.0D0/DCMPLX( Q2 -MV2 , DMAX1(DSIGN(DMV*DWV,Q2 ),0.D0) )
      ENDIF
C  For the running width, use below instead of the above DV.
C      DV = 1.0D0/DCMPLX( Q2 -MV2 , DMAX1(DWV*Q2/DMV,0.D0) )
C
      W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      IF (WMASS.EQ.0.) GOTO 10
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
      P3W2= (P3(0)+KT(0))*DW2(0)-(P3(1)+KT(1))*DW2(1)
     &     -(P3(2)+KT(2))*DW2(2)-(P3(3)+KT(3))*DW2(3)
      P2W3= (P2(0)+KT(0))*DW3(0)-(P2(1)+KT(1))*DW3(1)
     &     -(P2(2)+KT(2))*DW3(2)-(P2(3)+KT(3))*DW3(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
      JT32(0)= (P3(0)-P2(0))*W32 + P2W3*DW2(0) - P3W2*DW3(0)
      JT32(1)= (P3(1)-P2(1))*W32 + P2W3*DW2(1) - P3W2*DW3(1)
      JT32(2)= (P3(2)-P2(2))*W32 + P2W3*DW2(2) - P3W2*DW3(2)
      JT32(3)= (P3(3)-P2(3))*W32 + P2W3*DW2(3) - P3W2*DW3(3)
C
      JK12=(JT12(0)*KS(0)-JT12(1)*KS(1)-JT12(2)*KS(2)-JT12(3)*KS(3))
     &     *MW2INV
      JK32=(JT32(0)*KT(0)-JT32(1)*KT(1)-JT32(2)*KT(2)-JT32(3)*KT(3))
     &     *MW2INV
C
      J12(0)=(JT12(0)-KS(0)*JK12)*DWS
      J12(1)=(JT12(1)-KS(1)*JK12)*DWS
      J12(2)=(JT12(2)-KS(2)*JK12)*DWS
      J12(3)=(JT12(3)-KS(3)*JK12)*DWS
      J32(0)=(JT32(0)-KT(0)*JK32)*DWT
      J32(1)=(JT32(1)-KT(1)*JK32)*DWT
      J32(2)=(JT32(2)-KT(2)*JK32)*DWT
      J32(3)=(JT32(3)-KT(3)*JK32)*DWT
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
      JTW1=J32(0)*DW1(0)-J32(1)*DW1(1)-J32(2)*DW1(2)-J32(3)*DW1(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
      P1JT= (P1(0)-Q(0))*J32(0)-(P1(1)-Q(1))*J32(1)
     &     -(P1(2)-Q(2))*J32(2)-(P1(3)-Q(3))*J32(3)
      KTW1= (KT(0)-Q(0))*DW1(0)-(KT(1)-Q(1))*DW1(1)
     &     -(KT(2)-Q(2))*DW1(2)-(KT(3)-Q(3))*DW1(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
      JT(0)= (KT(0)-P1(0))*JTW1 + P1JT*DW1(0) - KTW1*J32(0)
      JT(1)= (KT(1)-P1(1))*JTW1 + P1JT*DW1(1) - KTW1*J32(1)
      JT(2)= (KT(2)-P1(2))*JTW1 + P1JT*DW1(2) - KTW1*J32(2)
      JT(3)= (KT(3)-P1(3))*JTW1 + P1JT*DW1(3) - KTW1*J32(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DG2*( DW1(0)*W32 + DW3(0)*W12 - 2.D0*DW2(0)*W13 )
      J4(1)=DG2*( DW1(1)*W32 + DW3(1)*W12 - 2.D0*DW2(1)*W13 )
      J4(2)=DG2*( DW1(2)*W32 + DW3(2)*W12 - 2.D0*DW2(2)*W13 )
      J4(3)=DG2*( DW1(3)*W32 + DW3(3)*W12 - 2.D0*DW2(3)*W13 )
C
      JJ(0)=JS(0)+JT(0)+J4(0)
      JJ(1)=JS(1)+JT(1)+J4(1)
      JJ(2)=JS(2)+JT(2)+J4(2)
      JJ(3)=JS(3)+JT(3)+J4(3)
C
      IF (VMASS.EQ.0.) GOTO 20
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MV2
C
      JW3W(1) = CMPLX( (JJ(0)-JQ*Q(0))*DV )
      JW3W(2) = CMPLX( (JJ(1)-JQ*Q(1))*DV )
      JW3W(3) = CMPLX( (JJ(2)-JQ*Q(2))*DV )
      JW3W(4) = CMPLX( (JJ(3)-JQ*Q(3))*DV )
C
      RETURN
C
  10  W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
C
      J12(0)=JT12(0)*DWS
      J12(1)=JT12(1)*DWS
      J12(2)=JT12(2)*DWS
      J12(3)=JT12(3)*DWS
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DG2*( DW1(0)*W32 - DW2(0)*W13 )
      J4(1)=DG2*( DW1(1)*W32 - DW2(1)*W13 )
      J4(2)=DG2*( DW1(2)*W32 - DW2(2)*W13 )
      J4(3)=DG2*( DW1(3)*W32 - DW2(3)*W13 )
C
      JJ(0)=JS(0)+J4(0)
      JJ(1)=JS(1)+J4(1)
      JJ(2)=JS(2)+J4(2)
      JJ(3)=JS(3)+J4(3)
C
      IF (VMASS.EQ.0.) GOTO 20
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MV2
C
      JW3W(1) = CMPLX( (JJ(0)-JQ*Q(0))*DV )
      JW3W(2) = CMPLX( (JJ(1)-JQ*Q(1))*DV )
      JW3W(3) = CMPLX( (JJ(2)-JQ*Q(2))*DV )
      JW3W(4) = CMPLX( (JJ(3)-JQ*Q(3))*DV )
C
      RETURN
C
  20  JW3W(1) = CMPLX( JJ(0)*DV )
      JW3W(2) = CMPLX( JJ(1)*DV )
      JW3W(3) = CMPLX( JJ(2)*DV )
      JW3W(4) = CMPLX( JJ(3)*DV )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE JWWWXX(W1,W2,W3,GWWA,GWWZ,ZMASS,ZWIDTH,WMASS,WWIDTH ,
     &                  JWWW)
C
C This subroutine computes an off-shell W+/W- current from the four-
C point gauge boson coupling, including the contributions of photon and
C Z exchanges.  The vector propagators for the output W and the internal
C Z bosons are given in unitary gauge, and that of the internal photon
C is given in Feynman gauge.
C
C INPUT:
C       complex W1(6)          : first  vector                        W1
C       complex W2(6)          : second vector                        W2
C       complex W3(6)          : third  vector                        W3
C       real    GWWA           : coupling constant of W and A       GWWA
C       real    GWWZ           : coupling constant of W and Z       GWWZ
C       real    ZMASS          : mass  of internal Z
C       real    ZWIDTH         : width of internal Z
C       real    WMASS          : mass  of OUTPUT W
C       real    WWIDTH         : width of OUTPUT W
C
C The possible sets of the inputs are as follows:
C   -------------------------------------------------------------------
C   |  W1  |  W2  |  W3  |GWWA|GWWZ|ZMASS|ZWIDTH|WMASS|WWIDTH || JWWW |
C   -------------------------------------------------------------------
C   |  W-  |  W+  |  W-  |GWWA|GWWZ|ZMASS|ZWIDTH|WMASS|WWIDTH ||  W+  |
C   |  W+  |  W-  |  W+  |GWWA|GWWZ|ZMASS|ZWIDTH|WMASS|WWIDTH ||  W-  |
C   -------------------------------------------------------------------
C where all the bosons are defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex JWWW(6)        : W current             J^mu(W':W1,W2,W3)
C
      COMPLEX*8  W1(6),W2(6),W3(6),JWWW(6)
      COMPLEX*16 DW1(0:3),DW2(0:3),DW3(0:3),
     &           JJ(0:3),JS(0:3),JT(0:3),J4(0:3),
     &           JT12(0:3),JT32(0:3),J12(0:3),J32(0:3),
     &           DZS,DZT,DW,W12,W32,W13,P1W2,P2W1,P3W2,P2W3,
     &           JK12,JK32,JSW3,JTW1,P3JS,KSW3,P1JT,KTW1,JQ
      REAL*4     GWWA,GWWZ,ZMASS,ZWIDTH,WMASS,WWIDTH
      REAL*8     P1(0:3),P2(0:3),P3(0:3),Q(0:3),KS(0:3),KT(0:3),
     &           DGWWA2,DGWWZ2,DGW2,DMZ,DWZ,DMW,DWW,MZ2,MW2,Q2,KS2,KT2,
     &           DAS,DAT
C
      JWWW(5) = W1(5)+W2(5)+W3(5)
      JWWW(6) = W1(6)+W2(6)+W3(6)
C
      DW1(0)=DCMPLX(W1(1))
      DW1(1)=DCMPLX(W1(2))
      DW1(2)=DCMPLX(W1(3))
      DW1(3)=DCMPLX(W1(4))
      DW2(0)=DCMPLX(W2(1))
      DW2(1)=DCMPLX(W2(2))
      DW2(2)=DCMPLX(W2(3))
      DW2(3)=DCMPLX(W2(4))
      DW3(0)=DCMPLX(W3(1))
      DW3(1)=DCMPLX(W3(2))
      DW3(2)=DCMPLX(W3(3))
      DW3(3)=DCMPLX(W3(4))
      P1(0)=DBLE(      W1(5))
      P1(1)=DBLE(      W1(6))
      P1(2)=DBLE(AIMAG(W1(6)))
      P1(3)=DBLE(AIMAG(W1(5)))
      P2(0)=DBLE(      W2(5))
      P2(1)=DBLE(      W2(6))
      P2(2)=DBLE(AIMAG(W2(6)))
      P2(3)=DBLE(AIMAG(W2(5)))
      P3(0)=DBLE(      W3(5))
      P3(1)=DBLE(      W3(6))
      P3(2)=DBLE(AIMAG(W3(6)))
      P3(3)=DBLE(AIMAG(W3(5)))
      Q(0)=-(P1(0)+P2(0)+P3(0))
      Q(1)=-(P1(1)+P2(1)+P3(1))
      Q(2)=-(P1(2)+P2(2)+P3(2))
      Q(3)=-(P1(3)+P2(3)+P3(3))
      KS(0)=P1(0)+P2(0)
      KS(1)=P1(1)+P2(1)
      KS(2)=P1(2)+P2(2)
      KS(3)=P1(3)+P2(3)
      KT(0)=P2(0)+P3(0)
      KT(1)=P2(1)+P3(1)
      KT(2)=P2(2)+P3(2)
      KT(3)=P2(3)+P3(3)
      Q2 =Q(0)**2 -(Q(1)**2 +Q(2)**2 +Q(3)**2)
      KS2=KS(0)**2-(KS(1)**2+KS(2)**2+KS(3)**2)
      KT2=KT(0)**2-(KT(1)**2+KT(2)**2+KT(3)**2)
      DGWWA2=DBLE(GWWA)**2
      DGWWZ2=DBLE(GWWZ)**2
      DGW2  =DGWWA2+DGWWZ2
      DMZ=DBLE(ZMASS)
      DWZ=DBLE(ZWIDTH)
      DMW=DBLE(WMASS)
      DWW=DBLE(WWIDTH)
      MZ2=DMZ**2
      MW2=DMW**2
C
      DAS=-DGWWA2/KS2
      DAT=-DGWWA2/KT2
      DZS=-DGWWZ2/DCMPLX( KS2-MZ2 , DMAX1(DSIGN(DMZ*DWZ,KS2),0.D0) )
      DZT=-DGWWZ2/DCMPLX( KT2-MZ2 , DMAX1(DSIGN(DMZ*DWZ,KT2),0.D0) )
      DW =-1.0D0/DCMPLX( Q2 -MW2 , DMAX1(DSIGN(DMW*DWW,Q2 ),0.D0) )
C  For the running width, use below instead of the above DW.
C      DW =-1.0D0/DCMPLX( Q2 -MW2 , DMAX1(DWW*Q2/DMW,0.D0) )
C
      W12=DW1(0)*DW2(0)-DW1(1)*DW2(1)-DW1(2)*DW2(2)-DW1(3)*DW2(3)
      W32=DW3(0)*DW2(0)-DW3(1)*DW2(1)-DW3(2)*DW2(2)-DW3(3)*DW2(3)
C
      P1W2= (P1(0)+KS(0))*DW2(0)-(P1(1)+KS(1))*DW2(1)
     &     -(P1(2)+KS(2))*DW2(2)-(P1(3)+KS(3))*DW2(3)
      P2W1= (P2(0)+KS(0))*DW1(0)-(P2(1)+KS(1))*DW1(1)
     &     -(P2(2)+KS(2))*DW1(2)-(P2(3)+KS(3))*DW1(3)
      P3W2= (P3(0)+KT(0))*DW2(0)-(P3(1)+KT(1))*DW2(1)
     &     -(P3(2)+KT(2))*DW2(2)-(P3(3)+KT(3))*DW2(3)
      P2W3= (P2(0)+KT(0))*DW3(0)-(P2(1)+KT(1))*DW3(1)
     &     -(P2(2)+KT(2))*DW3(2)-(P2(3)+KT(3))*DW3(3)
C
      JT12(0)= (P1(0)-P2(0))*W12 + P2W1*DW2(0) - P1W2*DW1(0)
      JT12(1)= (P1(1)-P2(1))*W12 + P2W1*DW2(1) - P1W2*DW1(1)
      JT12(2)= (P1(2)-P2(2))*W12 + P2W1*DW2(2) - P1W2*DW1(2)
      JT12(3)= (P1(3)-P2(3))*W12 + P2W1*DW2(3) - P1W2*DW1(3)
      JT32(0)= (P3(0)-P2(0))*W32 + P2W3*DW2(0) - P3W2*DW3(0)
      JT32(1)= (P3(1)-P2(1))*W32 + P2W3*DW2(1) - P3W2*DW3(1)
      JT32(2)= (P3(2)-P2(2))*W32 + P2W3*DW2(2) - P3W2*DW3(2)
      JT32(3)= (P3(3)-P2(3))*W32 + P2W3*DW2(3) - P3W2*DW3(3)
C
      JK12=(JT12(0)*KS(0)-JT12(1)*KS(1)-JT12(2)*KS(2)-JT12(3)*KS(3))/MZ2
      JK32=(JT32(0)*KT(0)-JT32(1)*KT(1)-JT32(2)*KT(2)-JT32(3)*KT(3))/MZ2
C
      J12(0)=JT12(0)*(DAS+DZS)-KS(0)*JK12*DZS
      J12(1)=JT12(1)*(DAS+DZS)-KS(1)*JK12*DZS
      J12(2)=JT12(2)*(DAS+DZS)-KS(2)*JK12*DZS
      J12(3)=JT12(3)*(DAS+DZS)-KS(3)*JK12*DZS
      J32(0)=JT32(0)*(DAT+DZT)-KT(0)*JK32*DZT
      J32(1)=JT32(1)*(DAT+DZT)-KT(1)*JK32*DZT
      J32(2)=JT32(2)*(DAT+DZT)-KT(2)*JK32*DZT
      J32(3)=JT32(3)*(DAT+DZT)-KT(3)*JK32*DZT
C
      JSW3=J12(0)*DW3(0)-J12(1)*DW3(1)-J12(2)*DW3(2)-J12(3)*DW3(3)
      JTW1=J32(0)*DW1(0)-J32(1)*DW1(1)-J32(2)*DW1(2)-J32(3)*DW1(3)
C
      P3JS= (P3(0)-Q(0))*J12(0)-(P3(1)-Q(1))*J12(1)
     &     -(P3(2)-Q(2))*J12(2)-(P3(3)-Q(3))*J12(3)
      KSW3= (KS(0)-Q(0))*DW3(0)-(KS(1)-Q(1))*DW3(1)
     &     -(KS(2)-Q(2))*DW3(2)-(KS(3)-Q(3))*DW3(3)
      P1JT= (P1(0)-Q(0))*J32(0)-(P1(1)-Q(1))*J32(1)
     &     -(P1(2)-Q(2))*J32(2)-(P1(3)-Q(3))*J32(3)
      KTW1= (KT(0)-Q(0))*DW1(0)-(KT(1)-Q(1))*DW1(1)
     &     -(KT(2)-Q(2))*DW1(2)-(KT(3)-Q(3))*DW1(3)
C
      JS(0)= (KS(0)-P3(0))*JSW3 + P3JS*DW3(0) - KSW3*J12(0)
      JS(1)= (KS(1)-P3(1))*JSW3 + P3JS*DW3(1) - KSW3*J12(1)
      JS(2)= (KS(2)-P3(2))*JSW3 + P3JS*DW3(2) - KSW3*J12(2)
      JS(3)= (KS(3)-P3(3))*JSW3 + P3JS*DW3(3) - KSW3*J12(3)
      JT(0)= (KT(0)-P1(0))*JTW1 + P1JT*DW1(0) - KTW1*J32(0)
      JT(1)= (KT(1)-P1(1))*JTW1 + P1JT*DW1(1) - KTW1*J32(1)
      JT(2)= (KT(2)-P1(2))*JTW1 + P1JT*DW1(2) - KTW1*J32(2)
      JT(3)= (KT(3)-P1(3))*JTW1 + P1JT*DW1(3) - KTW1*J32(3)
C
      W13=DW1(0)*DW3(0)-DW1(1)*DW3(1)-DW1(2)*DW3(2)-DW1(3)*DW3(3)
C
      J4(0)=DGW2*( DW1(0)*W32 + DW3(0)*W12 - 2.D0*DW2(0)*W13 )
      J4(1)=DGW2*( DW1(1)*W32 + DW3(1)*W12 - 2.D0*DW2(1)*W13 )
      J4(2)=DGW2*( DW1(2)*W32 + DW3(2)*W12 - 2.D0*DW2(2)*W13 )
      J4(3)=DGW2*( DW1(3)*W32 + DW3(3)*W12 - 2.D0*DW2(3)*W13 )
C
      JJ(0)=JS(0)+JT(0)+J4(0)
      JJ(1)=JS(1)+JT(1)+J4(1)
      JJ(2)=JS(2)+JT(2)+J4(2)
      JJ(3)=JS(3)+JT(3)+J4(3)
C
      JQ=(JJ(0)*Q(0)-JJ(1)*Q(1)-JJ(2)*Q(2)-JJ(3)*Q(3))/MW2
C
      JWWW(1) = CMPLX( (JJ(0)-JQ*Q(0))*DW )
      JWWW(2) = CMPLX( (JJ(1)-JQ*Q(1))*DW )
      JWWW(3) = CMPLX( (JJ(2)-JQ*Q(2))*DW )
      JWWW(4) = CMPLX( (JJ(3)-JQ*Q(3))*DW )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE MOM2CX(ESUM,MASS1,MASS2,COSTH1,PHI1 , P1,P2)
C
C This subroutine sets up two four-momenta in the two particle rest
C frame.
C
C INPUT:
C       real    ESUM           : energy sum of particle 1 and 2
C       real    MASS1          : mass            of particle 1
C       real    MASS2          : mass            of particle 2
C       real    COSTH1         : cos(theta)      of particle 1
C       real    PHI1           : azimuthal angle of particle 1
C
C OUTPUT:
C       real    P1(0:3)        : four-momentum of particle 1
C       real    P2(0:3)        : four-momentum of particle 2
C
      REAL    P1(0:3),P2(0:3),
     &        ESUM,MASS1,MASS2,COSTH1,PHI1,MD2,ED,PP,SINTH1
C
      MD2=(MASS1-MASS2)*(MASS1+MASS2)
      ED=MD2/ESUM
      IF (MASS1*MASS2.EQ.0.) THEN
      PP=(ESUM-ABS(ED))*0.5
C
      ELSE
      PP=SQRT((MD2/ESUM)**2-2.0*(MASS1**2+MASS2**2)+ESUM**2)*0.5
      ENDIF
      SINTH1=SQRT((1.0-COSTH1)*(1.0+COSTH1))
C
      P1(0) = MAX((ESUM+ED)*0.5,0.)
      P1(1) = PP*SINTH1*COS(PHI1)
      P1(2) = PP*SINTH1*SIN(PHI1)
      P1(3) = PP*COSTH1
C
      P2(0) = MAX((ESUM-ED)*0.5,0.)
      P2(1) = -P1(1)
      P2(2) = -P1(2)
      P2(3) = -P1(3)
C
      RETURN
      END
C
C **********************************************************************
C
      SUBROUTINE MOMNTX(ENERGY,MASS,COSTH,PHI , P)
C
C This subroutine sets up a four-momentum from the four inputs.
C
C INPUT:
C       real    ENERGY         : energy
C       real    MASS           : mass
C       real    COSTH          : cos(theta)
C       real    PHI            : azimuthal angle
C
C OUTPUT:
C       real    P(0:3)         : four-momentum
C
      REAL    P(0:3),ENERGY,MASS,COSTH,PHI,PP,SINTH
C
      P(0) = ENERGY
      IF (ENERGY.EQ.MASS) THEN
         P(1) = 0.
         P(2) = 0.
         P(3) = 0.
      ELSE
         PP=SQRT((ENERGY-MASS)*(ENERGY+MASS))
         SINTH=SQRT((1.-COSTH)*(1.+COSTH))
         P(3) = PP*COSTH
         IF (PHI.EQ.0.) THEN
            P(1) = PP*SINTH
            P(2) = 0.
         ELSE
            P(1) = PP*SINTH*COS(PHI)
            P(2) = PP*SINTH*SIN(PHI)
         ENDIF
      ENDIF
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE OXXXXX(P,FMASS,NHEL,NSF , FO)
C
C This subroutine computes a fermion wavefunction with the flowing-OUT
C fermion number.
C
C INPUT:
C       real    P(0:3)         : four-momentum of fermion
C       real    FMASS          : mass          of fermion
C       integer NHEL = -1 or 1 : helicity      of fermion
C       integer NSF  = -1 or 1 : +1 for particle, -1 for anti-particle
C
C OUTPUT:
C       complex FO(6)          : fermion wavefunction               <FO|
C
      COMPLEX FO(6),CHI(2)
      REAL    P(0:3),SF(2),SFOMEG(2),OMEGA(2),FMASS,PP,PP3,SQP0P3,SQM
      INTEGER NHEL,NSF,NH,IP,IM
C
      FO(5) = CMPLX(P(0),P(3))*NSF
      FO(6) = CMPLX(P(1),P(2))*NSF
C
      NH=NHEL*NSF
C
      IF (FMASS.EQ.0.) GOTO 10
C
      PP=MIN(P(0),SQRT(P(1)**2+P(2)**2+P(3)**2))
C
      IF (PP.EQ.0.) GOTO 20
C
      PP=MIN(P(0),SQRT(P(1)**2+P(2)**2+P(3)**2))
      SF(1)=REAL(1+NSF+(1-NSF)*NH)*.5
      SF(2)=REAL(1+NSF-(1-NSF)*NH)*.5
      OMEGA(1)=SQRT(P(0)+PP)
      OMEGA(2)=FMASS/OMEGA(1)
      IP=(3+NH)/2
      IM=(3-NH)/2
      SFOMEG(1)=SF(1)*OMEGA(IP)
      SFOMEG(2)=SF(2)*OMEGA(IM)
      PP3=MAX(PP+P(3),0.)
      CHI(1)=CMPLX( SQRT(PP3*.5/PP) )
      IF (PP3.EQ.0.) THEN
         CHI(2)=CMPLX(-NH )
      ELSE
         CHI(2)=CMPLX( NH*P(1) , -P(2) )/SQRT(2.*PP*PP3)
      ENDIF
C
      FO(1) = SFOMEG(2)*CHI(IM)
      FO(2) = SFOMEG(2)*CHI(IP)
      FO(3) = SFOMEG(1)*CHI(IM)
      FO(4) = SFOMEG(1)*CHI(IP)
C
      RETURN
C
  10  SQP0P3=SQRT(MAX(P(0)+P(3),0.))*NSF
      CHI(1)=CMPLX( SQP0P3 )
      IF (SQP0P3.EQ.0.) THEN
         CHI(2)=CMPLX(-NHEL )*SQRT(2.*P(0))
      ELSE
         CHI(2)=CMPLX( NH*P(1), -P(2) )/SQP0P3
      ENDIF
      IF (NH.EQ.1) THEN
         FO(1) = CHI(1)
         FO(2) = CHI(2)
         FO(3) = CMPLX( 0. )
         FO(4) = CMPLX( 0. )
      ELSE
         FO(1) = CMPLX( 0. )
         FO(2) = CMPLX( 0. )
         FO(3) = CHI(2)
         FO(4) = CHI(1)
      ENDIF
      RETURN
C
  20  SQM=SQRT(FMASS)
      IP=-((1+NH)/2)
      IM=  (1-NH)/2
C
      FO(1) = IM     * SQM
      FO(2) = IP*NSF * SQM
      FO(3) = IM*NSF * SQM
      FO(4) = IP     * SQM
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE ROTXXX(P,Q , PROT)
C
C This subroutine performs the spacial rotation of a four-momentum.
C The momentum P is assumed to be given in the frame where the spacial
C component of Q points the positive z-axis.  PROT is the momentum P
C rotated to the frame where Q is given.
C
C INPUT:
C       real    P(0:3)         : four-momentum P in Q(1)=Q(2)=0 frame
C       real    Q(0:3)         : four-momentum Q in the rotated frame
C
C OUTPUT:
C       real    PROT(0:3)      : four-momentum P in the rotated frame
C
      REAL    P(0:3),Q(0:3),PROT(0:3),QT2,QT,PSGN,QQ,P1
C
      PROT(0) = P(0)
C
      QT2=Q(1)**2+Q(2)**2
C
      IF (QT2.EQ.0.) THEN
          IF (Q(3).EQ.0.) THEN
             PROT(1) = P(1)
             PROT(2) = P(2)
             PROT(3) = P(3)
          ELSE
             PSGN=SIGN(1.,Q(3))
             PROT(1) = P(1)*PSGN
             PROT(2) = P(2)*PSGN
             PROT(3) = P(3)*PSGN
          ENDIF
      ELSE
          QQ=SQRT(QT2+Q(3)**2)
          QT=SQRT(QT2)
          P1=P(1)
          PROT(1) = Q(1)*Q(3)/QQ/QT*P1 -Q(2)/QT*P(2) +Q(1)/QQ*P(3)
          PROT(2) = Q(2)*Q(3)/QQ/QT*P1 +Q(1)/QT*P(2) +Q(2)/QQ*P(3)
          PROT(3) =          -QT/QQ*P1               +Q(3)/QQ*P(3)
      ENDIF
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE SSSSXX(S1,S2,S3,S4,G , VERTEX)
C
C This subroutine computes an amplitude of the four-scalar coupling.
C
C INPUT:
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       complex S4(3)          : fourth scalar                        S4
C       real    G              : coupling constant                 GHHHH
C
C OUTPUT:
C       complex VERTEX         : amplitude            Gamma(S1,S2,S3,S4)
C
      COMPLEX S1(3),S2(3),S3(3),S4(3),VERTEX
      REAL    G
C
      VERTEX = G*S1(1)*S2(1)*S3(1)*S4(1)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE SSSXXX(S1,S2,S3,G , VERTEX)
C
C This subroutine computes an amplitude of the three-scalar coupling.
C
C INPUT:
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex S3(3)          : third  scalar                        S3
C       real    G              : coupling constant                  GHHH
C
C OUTPUT:
C       complex VERTEX         : amplitude               Gamma(S1,S2,S3)
C
      COMPLEX S1(3),S2(3),S3(3),VERTEX
      REAL    G
C
      VERTEX = G*S1(1)*S2(1)*S3(1)
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE SXXXXX(P,NSS , SC)
C
C This subroutine computes a complex SCALAR wavefunction.
C
C INPUT:
C       real    P(0:3)         : four-momentum of scalar boson
C       integer NSS  = -1 or 1 : +1 for final, -1 for initial
C
C OUTPUT:
C       complex SC(3)          : scalar wavefunction                   S
C
      COMPLEX SC(3)
      REAL    P(0:3)
      INTEGER NSS
C
      SC(1) = CMPLX( 1.0 )
      SC(2) = CMPLX(P(0),P(3))*NSS
      SC(3) = CMPLX(P(1),P(2))*NSS
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VSSXXX(VC,S1,S2,G , VERTEX)
C
C This subroutine computes an amplitude from the vector-scalar-scalar
C coupling.  The coupling is absent in the minimal SM in unitary gauge.
C
C       complex VC(6)          : input  vector                        V
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant (S1 charge)
C
C Examples of the coupling constant G for SUSY particles are as follows:
C   -----------------------------------------------------------
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |
C   -----------------------------------------------------------
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |
C   -----------------------------------------------------------
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |
C   -----------------------------------------------------------
C where the S1 charge is defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex VERTEX         : amplitude                Gamma(V,S1,S2)
C
      COMPLEX VC(6),S1(3),S2(3),VERTEX
      REAL    P(0:3),G
C
      P(0)=REAL( S1(2)-S2(2))
      P(1)=REAL( S1(3)-S2(3))
      P(2)=AIMAG(S1(3)-S2(3))
      P(3)=AIMAG(S1(2)-S2(2))
C
      VERTEX = G*S1(1)*S2(1)
     &        *(VC(1)*P(0)-VC(2)*P(1)-VC(3)*P(2)-VC(4)*P(3))
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VVSSXX(V1,V2,S1,S2,G , VERTEX)
C
C This subroutine computes an amplitude of the vector-vector-scalar-
C scalar coupling.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       real    G              : coupling constant                 GVVHH
C
C OUTPUT:
C       complex VERTEX         : amplitude            Gamma(V1,V2,S1,S2)
C
      COMPLEX V1(6),V2(6),S1(3),S2(3),VERTEX
      REAL    G
C
      VERTEX = G*S1(1)*S2(1)
     &        *(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VVSXXX(V1,V2,SC,G , VERTEX)
C
C This subroutine computes an amplitude of the vector-vector-scalar
C coupling.
C
C INPUT:
C       complex V1(6)          : first  vector                        V1
C       complex V2(6)          : second vector                        V2
C       complex SC(3)          : input  scalar                        S
C       real    G              : coupling constant                  GVVH
C
C OUTPUT:
C       complex VERTEX         : amplitude                Gamma(V1,V2,S)
C
      COMPLEX V1(6),V2(6),SC(3),VERTEX
      REAL    G
C
      VERTEX = G*SC(1)*(V1(1)*V2(1)-V1(2)*V2(2)-V1(3)*V2(3)-V1(4)*V2(4))
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE VVVXXX(WM,WP,W3,G , VERTEX)
C
C This subroutine computes an amplitude of the three-point coupling of
C the gauge bosons.
C
C INPUT:
C       complex WM(6)          : vector               flow-OUT W-
C       complex WP(6)          : vector               flow-OUT W+
C       complex W3(6)          : vector               J3 or A    or Z
C       real    G              : coupling constant    GW or GWWA or GWWZ
C
C OUTPUT:
C       complex VERTEX         : amplitude               Gamma(WM,WP,W3)
C
      COMPLEX WM(6),WP(6),W3(6),VERTEX,
     &        XV1,XV2,XV3,V12,V23,V31,P12,P13,P21,P23,P31,P32
      REAL    PWM(0:3),PWP(0:3),PW3(0:3),G
C
      PWM(0)=REAL( WM(5))
      PWM(1)=REAL( WM(6))
      PWM(2)=AIMAG(WM(6))
      PWM(3)=AIMAG(WM(5))
      PWP(0)=REAL( WP(5))
      PWP(1)=REAL( WP(6))
      PWP(2)=AIMAG(WP(6))
      PWP(3)=AIMAG(WP(5))
      PW3(0)=REAL( W3(5))
      PW3(1)=REAL( W3(6))
      PW3(2)=AIMAG(W3(6))
      PW3(3)=AIMAG(W3(5))
C
      V12=WM(1)*WP(1)-WM(2)*WP(2)-WM(3)*WP(3)-WM(4)*WP(4)
      V23=WP(1)*W3(1)-WP(2)*W3(2)-WP(3)*W3(3)-WP(4)*W3(4)
      V31=W3(1)*WM(1)-W3(2)*WM(2)-W3(3)*WM(3)-W3(4)*WM(4)
      XV1=0.
      XV2=0.
      XV3=0.
      IF (ABS(WM(1)).NE.0.) THEN
      IF (ABS(WM(1)).GE.MAX(ABS(WM(2)),ABS(WM(3)),ABS(WM(4)))*1.E-1)
     &      XV1=PWM(0)/WM(1)
      ENDIF
      IF (ABS(WP(1)).NE.0.) THEN
      IF (ABS(WP(1)).GE.MAX(ABS(WP(2)),ABS(WP(3)),ABS(WP(4)))*1.E-1)
     &      XV2=PWP(0)/WP(1)
      ENDIF
      IF (ABS(W3(1)).NE.0.) THEN
      IF (ABS(W3(1)).GE.MAX(ABS(W3(2)),ABS(W3(3)),ABS(W3(4)))*1.E-1)
     &      XV3=PW3(0)/W3(1)
      ENDIF
      P12= (PWM(0)-XV1*WM(1))*WP(1)-(PWM(1)-XV1*WM(2))*WP(2)
     &    -(PWM(2)-XV1*WM(3))*WP(3)-(PWM(3)-XV1*WM(4))*WP(4)
      P13= (PWM(0)-XV1*WM(1))*W3(1)-(PWM(1)-XV1*WM(2))*W3(2)
     &    -(PWM(2)-XV1*WM(3))*W3(3)-(PWM(3)-XV1*WM(4))*W3(4)
      P21= (PWP(0)-XV2*WP(1))*WM(1)-(PWP(1)-XV2*WP(2))*WM(2)
     &    -(PWP(2)-XV2*WP(3))*WM(3)-(PWP(3)-XV2*WP(4))*WM(4)
      P23= (PWP(0)-XV2*WP(1))*W3(1)-(PWP(1)-XV2*WP(2))*W3(2)
     &    -(PWP(2)-XV2*WP(3))*W3(3)-(PWP(3)-XV2*WP(4))*W3(4)
      P31= (PW3(0)-XV3*W3(1))*WM(1)-(PW3(1)-XV3*W3(2))*WM(2)
     &    -(PW3(2)-XV3*W3(3))*WM(3)-(PW3(3)-XV3*W3(4))*WM(4)
      P32= (PW3(0)-XV3*W3(1))*WP(1)-(PW3(1)-XV3*W3(2))*WP(2)
     &    -(PW3(2)-XV3*W3(3))*WP(3)-(PW3(3)-XV3*W3(4))*WP(4)
C
      VERTEX = -(V12*(P13-P23)+V23*(P21-P31)+V31*(P32-P12))*G
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE VXXXXX(P,VMASS,NHEL,NSV , VC)
C
C This subroutine computes a VECTOR wavefunction.
C
C INPUT:
C       real    P(0:3)         : four-momentum of vector boson
C       real    VMASS          : mass          of vector boson
C       integer NHEL = -1, 0, 1: helicity      of vector boson
C                                (0 is forbidden if VMASS=0.0)
C       integer NSV  = -1 or 1 : +1 for final, -1 for initial
C
C OUTPUT:
C       complex VC(6)          : vector wavefunction       epsilon^mu(V)
C
      COMPLEX VC(6)
      REAL    P(0:3),VMASS,HEL,HEL0,PT,PT2,PP,PZPT,EMP,SQH
      INTEGER NHEL,NSV,NSVAHL
C
      SQH=SQRT(.5)
      HEL=REAL(NHEL)
      NSVAHL=NSV*ABS(HEL)
      PT2=P(1)**2+P(2)**2
      PP=MIN(P(0),SQRT(PT2+P(3)**2))
      PT=MIN(PP,SQRT(PT2))
C
      VC(5) = CMPLX(P(0),P(3))*NSV
      VC(6) = CMPLX(P(1),P(2))*NSV
C
      IF (VMASS.EQ.0.) GOTO 10
C
      HEL0=1.0-ABS(HEL)
C
      IF (PP.EQ.0.) GOTO 20
C
      EMP=P(0)/(VMASS*PP)
        VC(1) = CMPLX( HEL0*PP/VMASS )
        VC(4) = CMPLX( HEL0*P(3)*EMP+HEL*PT/PP*SQH )
      IF (PT.NE.0.) THEN
         PZPT=P(3)/(PP*PT)*SQH*HEL
        VC(2) = CMPLX( HEL0*P(1)*EMP-P(1)*PZPT , -NSVAHL*P(2)/PT*SQH )
        VC(3) = CMPLX( HEL0*P(2)*EMP-P(2)*PZPT ,  NSVAHL*P(1)/PT*SQH )
      ELSE
        VC(2) = CMPLX(-HEL*SQH )
        VC(3) = CMPLX( 0. , NSVAHL*SIGN(SQH,P(3)) )
      ENDIF
      RETURN
C
  10  PP=P(0)
      PT=SQRT(P(1)**2+P(2)**2)
        VC(1) = CMPLX( 0. )
        VC(4) = CMPLX( HEL*PT/PP*SQH )
      IF (PT.NE.0.) THEN
         PZPT=P(3)/(PP*PT)*SQH*HEL
        VC(2) = CMPLX(-P(1)*PZPT , -NSV*P(2)/PT*SQH )
        VC(3) = CMPLX(-P(2)*PZPT ,  NSV*P(1)/PT*SQH )
      ELSE
        VC(2) = CMPLX(-HEL*SQH )
        VC(3) = CMPLX( 0. , NSV*SIGN(SQH,P(3)) )
      ENDIF
      RETURN
C
  20  VC(1) = CMPLX( 0. )
      VC(2) = CMPLX(-HEL*SQH )
      VC(3) = CMPLX( 0. , NSVAHL*SQH )
      VC(4) = CMPLX( HEL0 )
C
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE W3W3XX(WM,W31,WP,W32,G31,G32,WMASS,WWIDTH , VERTEX)
C
C This subroutine computes an amplitude of the four-point coupling of
C the W-, W+ and two W3/Z/A.  The amplitude includes the contributions
C of W exchange diagrams.  The internal W propagator is given in unitary
C gauge.  If one sets WMASS=0.0, then the gggg vertex is given (see sect
C 2.9.1 of the manual).
C
C INPUT:
C       complex WM(0:3)        : flow-OUT W-                         WM
C       complex W31(0:3)       : first    W3/Z/A                     W31
C       complex WP(0:3)        : flow-OUT W+                         WP
C       complex W32(0:3)       : second   W3/Z/A                     W32
C       real    G31            : coupling of W31 with W-/W+
C       real    G32            : coupling of W32 with W-/W+
C                                                  (see the table below)
C       real    WMASS          : mass  of W
C       real    WWIDTH         : width of W
C
C The possible sets of the inputs are as follows:
C   -------------------------------------------
C   |  WM  |  W31 |  WP  |  W32 |  G31 |  G32 |
C   -------------------------------------------
C   |  W-  |  W3  |  W+  |  W3  |  GW  |  GW  |
C   |  W-  |  W3  |  W+  |  Z   |  GW  | GWWZ |
C   |  W-  |  W3  |  W+  |  A   |  GW  | GWWA |
C   |  W-  |  Z   |  W+  |  Z   | GWWZ | GWWZ |
C   |  W-  |  Z   |  W+  |  A   | GWWZ | GWWA |
C   |  W-  |  A   |  W+  |  A   | GWWA | GWWA |
C   -------------------------------------------
C where all the bosons are defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex VERTEX         : amplitude          Gamma(WM,W31,WP,W32)
C
      COMPLEX    WM(6),W31(6),WP(6),W32(6),VERTEX
      COMPLEX*16 DV1(0:3),DV2(0:3),DV3(0:3),DV4(0:3),
     &           J12(0:3),J34(0:3),J14(0:3),J32(0:3),DVERTX,
     &           SV1,SV2,SV3,SV4,TV1,TV2,TV3,TV4,DWS,DWT,
     &           V12,V13,V14,V23,V24,V34,JS12,JS34,JS14,JS32,JS,JT
      REAL       PWM(0:3),PW31(0:3),PWP(0:3),PW32(0:3),
     &           G31,G32,WMASS,WWIDTH
      REAL*8     Q(0:3),K(0:3),DP1(0:3),DP2(0:3),DP3(0:3),DP4(0:3),
     &           DMW,DWIDTH,DM2INV,S,T
C
      PWM(0)=REAL( WM(5))
      PWM(1)=REAL( WM(6))
      PWM(2)=AIMAG(WM(6))
      PWM(3)=AIMAG(WM(5))
      PWP(0)=REAL( WP(5))
      PWP(1)=REAL( WP(6))
      PWP(2)=AIMAG(WP(6))
      PWP(3)=AIMAG(WP(5))
      PW31(0)=REAL( W31(5))
      PW31(1)=REAL( W31(6))
      PW31(2)=AIMAG(W31(6))
      PW31(3)=AIMAG(W31(5))
      PW32(0)=REAL( W32(5))
      PW32(1)=REAL( W32(6))
      PW32(2)=AIMAG(W32(6))
      PW32(3)=AIMAG(W32(5))
C
      DV1(0)=DCMPLX(WM(1))
      DV1(1)=DCMPLX(WM(2))
      DV1(2)=DCMPLX(WM(3))
      DV1(3)=DCMPLX(WM(4))
      DP1(0)=DBLE(PWM(0))
      DP1(1)=DBLE(PWM(1))
      DP1(2)=DBLE(PWM(2))
      DP1(3)=DBLE(PWM(3))
      DV2(0)=DCMPLX(W31(1))
      DV2(1)=DCMPLX(W31(2))
      DV2(2)=DCMPLX(W31(3))
      DV2(3)=DCMPLX(W31(4))
      DP2(0)=DBLE(PW31(0))
      DP2(1)=DBLE(PW31(1))
      DP2(2)=DBLE(PW31(2))
      DP2(3)=DBLE(PW31(3))
      DV3(0)=DCMPLX(WP(1))
      DV3(1)=DCMPLX(WP(2))
      DV3(2)=DCMPLX(WP(3))
      DV3(3)=DCMPLX(WP(4))
      DP3(0)=DBLE(PWP(0))
      DP3(1)=DBLE(PWP(1))
      DP3(2)=DBLE(PWP(2))
      DP3(3)=DBLE(PWP(3))
      DV4(0)=DCMPLX(W32(1))
      DV4(1)=DCMPLX(W32(2))
      DV4(2)=DCMPLX(W32(3))
      DV4(3)=DCMPLX(W32(4))
      DP4(0)=DBLE(PW32(0))
      DP4(1)=DBLE(PW32(1))
      DP4(2)=DBLE(PW32(2))
      DP4(3)=DBLE(PW32(3))
      DMW   =DBLE(WMASS)
      DWIDTH=DBLE(WWIDTH)
      DMW2  =DMW**2
C
      IF (WMASS.EQ.0.) GOTO 10
C
      DM2INV=1.0D0/DMW2
C
      V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
      K(0)=DP1(0)+DP4(0)
      K(1)=DP1(1)+DP4(1)
      K(2)=DP1(2)+DP4(2)
      K(3)=DP1(3)+DP4(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      T=K(0)**2-K(1)**2-K(2)**2-K(3)**2
      DWS=-1.D0/DCMPLX( S-DMW2 , DMAX1(DSIGN(DMW*DWIDTH,S),0.D0) )
      DWT=-1.D0/DCMPLX( T-DMW2 , DMAX1(DSIGN(DMW*DWIDTH,T),0.D0) )
C
      SV1= (DP2(0)+Q(0))*DV1(0)-(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2)-(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0)+(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2)+(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0)-(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2)-(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0)+(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2)+(DP3(3)-Q(3))*DV4(3)
C
      TV1= (DP4(0)+K(0))*DV1(0)-(DP4(1)+K(1))*DV1(1)
     &    -(DP4(2)+K(2))*DV1(2)-(DP4(3)+K(3))*DV1(3)
      TV2=-(DP3(0)-K(0))*DV2(0)+(DP3(1)-K(1))*DV2(1)
     &    +(DP3(2)-K(2))*DV2(2)+(DP3(3)-K(3))*DV2(3)
      TV3= (DP2(0)-K(0))*DV3(0)-(DP2(1)-K(1))*DV3(1)
     &    -(DP2(2)-K(2))*DV3(2)-(DP2(3)-K(3))*DV3(3)
      TV4=-(DP1(0)+K(0))*DV4(0)+(DP1(1)+K(1))*DV4(1)
     &    +(DP1(2)+K(2))*DV4(2)+(DP1(3)+K(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      J14(0)=(DP1(0)-DP4(0))*V14 +TV1*DV4(0) +TV4*DV1(0)
      J14(1)=(DP1(1)-DP4(1))*V14 +TV1*DV4(1) +TV4*DV1(1)
      J14(2)=(DP1(2)-DP4(2))*V14 +TV1*DV4(2) +TV4*DV1(2)
      J14(3)=(DP1(3)-DP4(3))*V14 +TV1*DV4(3) +TV4*DV1(3)
      J32(0)=(DP3(0)-DP2(0))*V23 +TV3*DV2(0) +TV2*DV3(0)
      J32(1)=(DP3(1)-DP2(1))*V23 +TV3*DV2(1) +TV2*DV3(1)
      J32(2)=(DP3(2)-DP2(2))*V23 +TV3*DV2(2) +TV2*DV3(2)
      J32(3)=(DP3(3)-DP2(3))*V23 +TV3*DV2(3) +TV2*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
      JS14=K(0)*J14(0)-K(1)*J14(1)-K(2)*J14(2)-K(3)*J14(3)
      JS32=K(0)*J32(0)-K(1)*J32(1)-K(2)*J32(2)-K(3)*J32(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
      JT=J14(0)*J32(0)-J14(1)*J32(1)-J14(2)*J32(2)-J14(3)*J32(3)
C
      DVERTX = V12*V34 +V14*V23 -2.D0*V13*V24
     &        +DWS*(JS -JS12*JS34*DM2INV) +DWT*(JT -JS14*JS32*DM2INV)
C
      VERTEX = CMPLX( DVERTX ) * (G31*G32)
C
      RETURN
C
  10  V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      DWS=-1.D0/DCMPLX( S )
C
      SV1= (DP2(0)+Q(0))*DV1(0)-(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2)-(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0)+(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2)+(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0)-(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2)-(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0)+(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2)+(DP3(3)-Q(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
C
      DVERTX = V14*V23 -V13*V24 +DWS*JS
C
      VERTEX = CMPLX( DVERTX ) * (G31*G32)
C
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE WWWWXX(WM1,WP1,WM2,WP2,GWWA,GWWZ,ZMASS,ZWIDTH , VERTEX)
C
C This subroutine computes an amplitude of the four-point W-/W+
C coupling, including the contributions of photon and Z exchanges.  The
C photon propagator is given in Feynman gauge and the Z propagator is
C given in unitary gauge.
C
C INPUT:
C       complex WM1(0:3)       : first  flow-OUT W-                  WM1
C       complex WP1(0:3)       : first  flow-OUT W+                  WP1
C       complex WM2(0:3)       : second flow-OUT W-                  WM2
C       complex WP2(0:3)       : second flow-OUT W+                  WP2
C       real    GWWA           : coupling constant of W and A       GWWA
C       real    GWWZ           : coupling constant of W and Z       GWWZ
C       real    ZMASS          : mass  of Z
C       real    ZWIDTH         : width of Z
C
C OUTPUT:
C       complex VERTEX         : amplitude        Gamma(WM1,WP1,WM2,WP2)
C
      COMPLEX    WM1(6),WP1(6),WM2(6),WP2(6),VERTEX
      COMPLEX*16 DV1(0:3),DV2(0:3),DV3(0:3),DV4(0:3),
     &           J12(0:3),J34(0:3),J14(0:3),J32(0:3),DVERTX,
     &           SV1,SV2,SV3,SV4,TV1,TV2,TV3,TV4,DZS,DZT,
     &           V12,V13,V14,V23,V24,V34,JS12,JS34,JS14,JS32,JS,JT
      REAL       PWM1(0:3),PWP1(0:3),PWM2(0:3),PWP2(0:3),
     &           GWWA,GWWZ,ZMASS,ZWIDTH
      REAL*8     Q(0:3),K(0:3),DP1(0:3),DP2(0:3),DP3(0:3),DP4(0:3),
     &           DGWWA2,DGWWZ2,DGW2,DMZ,DWIDTH,S,T,DAS,DAT
C
      PWM1(0)=REAL( WM1(5))
      PWM1(1)=REAL( WM1(6))
      PWM1(2)=AIMAG(WM1(6))
      PWM1(3)=AIMAG(WM1(5))
      PWP1(0)=REAL( WP1(5))
      PWP1(1)=REAL( WP1(6))
      PWP1(2)=AIMAG(WP1(6))
      PWP1(3)=AIMAG(WP1(5))
      PWM2(0)=REAL( WM2(5))
      PWM2(1)=REAL( WM2(6))
      PWM2(2)=AIMAG(WM2(6))
      PWM2(3)=AIMAG(WM2(5))
      PWP2(0)=REAL( WP2(5))
      PWP2(1)=REAL( WP2(6))
      PWP2(2)=AIMAG(WP2(6))
      PWP2(3)=AIMAG(WP2(5))
C
      DV1(0)=DCMPLX(WM1(1))
      DV1(1)=DCMPLX(WM1(2))
      DV1(2)=DCMPLX(WM1(3))
      DV1(3)=DCMPLX(WM1(4))
      DP1(0)=DBLE(PWM1(0))
      DP1(1)=DBLE(PWM1(1))
      DP1(2)=DBLE(PWM1(2))
      DP1(3)=DBLE(PWM1(3))
      DV2(0)=DCMPLX(WP1(1))
      DV2(1)=DCMPLX(WP1(2))
      DV2(2)=DCMPLX(WP1(3))
      DV2(3)=DCMPLX(WP1(4))
      DP2(0)=DBLE(PWP1(0))
      DP2(1)=DBLE(PWP1(1))
      DP2(2)=DBLE(PWP1(2))
      DP2(3)=DBLE(PWP1(3))
      DV3(0)=DCMPLX(WM2(1))
      DV3(1)=DCMPLX(WM2(2))
      DV3(2)=DCMPLX(WM2(3))
      DV3(3)=DCMPLX(WM2(4))
      DP3(0)=DBLE(PWM2(0))
      DP3(1)=DBLE(PWM2(1))
      DP3(2)=DBLE(PWM2(2))
      DP3(3)=DBLE(PWM2(3))
      DV4(0)=DCMPLX(WP2(1))
      DV4(1)=DCMPLX(WP2(2))
      DV4(2)=DCMPLX(WP2(3))
      DV4(3)=DCMPLX(WP2(4))
      DP4(0)=DBLE(PWP2(0))
      DP4(1)=DBLE(PWP2(1))
      DP4(2)=DBLE(PWP2(2))
      DP4(3)=DBLE(PWP2(3))
      DGWWA2=DBLE(GWWA)**2
      DGWWZ2=DBLE(GWWZ)**2
      DGW2  =DGWWA2+DGWWZ2
      DMZ   =DBLE(ZMASS)
      DWIDTH=DBLE(ZWIDTH)
C
      V12= DV1(0)*DV2(0)-DV1(1)*DV2(1)-DV1(2)*DV2(2)-DV1(3)*DV2(3)
      V13= DV1(0)*DV3(0)-DV1(1)*DV3(1)-DV1(2)*DV3(2)-DV1(3)*DV3(3)
      V14= DV1(0)*DV4(0)-DV1(1)*DV4(1)-DV1(2)*DV4(2)-DV1(3)*DV4(3)
      V23= DV2(0)*DV3(0)-DV2(1)*DV3(1)-DV2(2)*DV3(2)-DV2(3)*DV3(3)
      V24= DV2(0)*DV4(0)-DV2(1)*DV4(1)-DV2(2)*DV4(2)-DV2(3)*DV4(3)
      V34= DV3(0)*DV4(0)-DV3(1)*DV4(1)-DV3(2)*DV4(2)-DV3(3)*DV4(3)
C
      Q(0)=DP1(0)+DP2(0)
      Q(1)=DP1(1)+DP2(1)
      Q(2)=DP1(2)+DP2(2)
      Q(3)=DP1(3)+DP2(3)
      K(0)=DP1(0)+DP4(0)
      K(1)=DP1(1)+DP4(1)
      K(2)=DP1(2)+DP4(2)
      K(3)=DP1(3)+DP4(3)
C
      S=Q(0)**2-Q(1)**2-Q(2)**2-Q(3)**2
      T=K(0)**2-K(1)**2-K(2)**2-K(3)**2
C
      DAS=-1.D0/S
      DAT=-1.D0/T
      DZS=-1.D0/DCMPLX( S-DMZ**2 , DMAX1(DSIGN(DMZ*DWIDTH,S),0.D0) )
      DZT=-1.D0/DCMPLX( T-DMZ**2 , DMAX1(DSIGN(DMZ*DWIDTH,T),0.D0) )
C
      SV1= (DP2(0)+Q(0))*DV1(0) -(DP2(1)+Q(1))*DV1(1)
     &    -(DP2(2)+Q(2))*DV1(2) -(DP2(3)+Q(3))*DV1(3)
      SV2=-(DP1(0)+Q(0))*DV2(0) +(DP1(1)+Q(1))*DV2(1)
     &    +(DP1(2)+Q(2))*DV2(2) +(DP1(3)+Q(3))*DV2(3)
      SV3= (DP4(0)-Q(0))*DV3(0) -(DP4(1)-Q(1))*DV3(1)
     &    -(DP4(2)-Q(2))*DV3(2) -(DP4(3)-Q(3))*DV3(3)
      SV4=-(DP3(0)-Q(0))*DV4(0) +(DP3(1)-Q(1))*DV4(1)
     &    +(DP3(2)-Q(2))*DV4(2) +(DP3(3)-Q(3))*DV4(3)
C
      TV1= (DP4(0)+K(0))*DV1(0) -(DP4(1)+K(1))*DV1(1)
     &    -(DP4(2)+K(2))*DV1(2) -(DP4(3)+K(3))*DV1(3)
      TV2=-(DP3(0)-K(0))*DV2(0) +(DP3(1)-K(1))*DV2(1)
     &    +(DP3(2)-K(2))*DV2(2) +(DP3(3)-K(3))*DV2(3)
      TV3= (DP2(0)-K(0))*DV3(0) -(DP2(1)-K(1))*DV3(1)
     &    -(DP2(2)-K(2))*DV3(2) -(DP2(3)-K(3))*DV3(3)
      TV4=-(DP1(0)+K(0))*DV4(0) +(DP1(1)+K(1))*DV4(1)
     &    +(DP1(2)+K(2))*DV4(2) +(DP1(3)+K(3))*DV4(3)
C
      J12(0)=(DP1(0)-DP2(0))*V12 +SV1*DV2(0) +SV2*DV1(0)
      J12(1)=(DP1(1)-DP2(1))*V12 +SV1*DV2(1) +SV2*DV1(1)
      J12(2)=(DP1(2)-DP2(2))*V12 +SV1*DV2(2) +SV2*DV1(2)
      J12(3)=(DP1(3)-DP2(3))*V12 +SV1*DV2(3) +SV2*DV1(3)
      J34(0)=(DP3(0)-DP4(0))*V34 +SV3*DV4(0) +SV4*DV3(0)
      J34(1)=(DP3(1)-DP4(1))*V34 +SV3*DV4(1) +SV4*DV3(1)
      J34(2)=(DP3(2)-DP4(2))*V34 +SV3*DV4(2) +SV4*DV3(2)
      J34(3)=(DP3(3)-DP4(3))*V34 +SV3*DV4(3) +SV4*DV3(3)
C
      J14(0)=(DP1(0)-DP4(0))*V14 +TV1*DV4(0) +TV4*DV1(0)
      J14(1)=(DP1(1)-DP4(1))*V14 +TV1*DV4(1) +TV4*DV1(1)
      J14(2)=(DP1(2)-DP4(2))*V14 +TV1*DV4(2) +TV4*DV1(2)
      J14(3)=(DP1(3)-DP4(3))*V14 +TV1*DV4(3) +TV4*DV1(3)
      J32(0)=(DP3(0)-DP2(0))*V23 +TV3*DV2(0) +TV2*DV3(0)
      J32(1)=(DP3(1)-DP2(1))*V23 +TV3*DV2(1) +TV2*DV3(1)
      J32(2)=(DP3(2)-DP2(2))*V23 +TV3*DV2(2) +TV2*DV3(2)
      J32(3)=(DP3(3)-DP2(3))*V23 +TV3*DV2(3) +TV2*DV3(3)
C
      JS12=Q(0)*J12(0)-Q(1)*J12(1)-Q(2)*J12(2)-Q(3)*J12(3)
      JS34=Q(0)*J34(0)-Q(1)*J34(1)-Q(2)*J34(2)-Q(3)*J34(3)
      JS14=K(0)*J14(0)-K(1)*J14(1)-K(2)*J14(2)-K(3)*J14(3)
      JS32=K(0)*J32(0)-K(1)*J32(1)-K(2)*J32(2)-K(3)*J32(3)
C
      JS=J12(0)*J34(0)-J12(1)*J34(1)-J12(2)*J34(2)-J12(3)*J34(3)
      JT=J14(0)*J32(0)-J14(1)*J32(1)-J14(2)*J32(2)-J14(3)*J32(3)
C
      DVERTX = (V12*V34 +V14*V23 -2.D0*V13*V24)*DGW2
     &        +(DZS*DGWWZ2+DAS*DGWWA2)*JS -DZS*DGWWZ2*JS12*JS34/DMZ**2
     &        +(DZT*DGWWZ2+DAT*DGWWA2)*JT -DZT*DGWWZ2*JS14*JS32/DMZ**2
C
      VERTEX = -CMPLX( DVERTX )
C
      RETURN
      END
CDECK  ID>, LNBLNK.
c File : lnblnk.F
c Description : LNBLNK(Sun) Substitution
c
c Author : Ryosuke Itoh, Physics Division, KEK
c Date : 10 - Feb - 1994
c Note : This code was grabbed from CLI's LENRD
c-
       FUNCTION LNBLNK(CH)
       CHARACTER*(*) CH
C
C..... GET NON-BLANK LENGTH OF CHARACTER
C
       L = LEN(CH)
       M = L
       DO 1 I = 1,L
         IF (CH(M:M).NE.' ') GOTO 2
         M = M-1
 1     CONTINUE
 2     LNBLNK = M
       RETURN
       END
CDECK  ID>, SPINIT.
      SUBROUTINE SPINIT
      RETURN
      END
CDECK  ID>, USMASS.
C***********************************************************************
C*
C*=====================================
C* Subroutine USMASS(MODE,PNAME,PMASS)
C*==============================-----==
C*
C* (Ppurpose)
C*    This subroutine allows users to put or get particle masses
C*    in or from /SMPTAB/ without directly referring to it.
C* (Inputs)
C*      MODE     : (R*4) : (1,2)=(get,put).
C*      PNAME    : (C*4) : particle name.
C* (Input/Output)
C*      PMASS    : (R*4) : particle mass.
C* (Relation)
C*    Calls no subroutines.
C*
C***********************************************************************

      SUBROUTINE USMASS(MODE,PNAME,PMASS)

      INTEGER  *4   MODE
      CHARACTER*4   PNAME
      REAL     *4   PMASS
C--
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
C
C========< Entry Point >================================================
C
C--
C  Decide the length of the name string.
C--
      L = INDEX(PNAME,' ') - 1
C--
C  Branch on kinds of particles.
C--
      IF ( PNAME(:L).EQ.'E' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(1,2,1)
         ELSE
            AMF(1,2,1) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'MU' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(2,2,1)
         ELSE
            AMF(2,2,1) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'TAU' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(3,2,1)
         ELSE
            AMF(3,2,1) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'U' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(1,1,2)
         ELSE
            AMF(1,1,2) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'C' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(2,1,2)
         ELSE
            AMF(2,1,2) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'T' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(3,1,2)
         ELSE
            AMF(3,1,2) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'D' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(1,2,2)
         ELSE
            AMF(1,2,2) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'S' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(2,2,2)
         ELSE
            AMF(2,2,2) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'B' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMF(3,2,2)
         ELSE
            AMF(3,2,2) = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'W' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMW
         ELSE
            AMW = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'Z' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMZ
         ELSE
            AMZ = PMASS
         ENDIF
      ELSE IF ( PNAME(:L).EQ.'HSM' ) THEN
         IF ( MODE.EQ.1 ) THEN
            PMASS = AMH
         ELSE
            AMH = PMASS
         ENDIF
      ELSE
         PRINT *, '>>> ERROR IN USMASS >>>'
         PRINT *, '  Particle ', PNAME(:L), ' is not supported.'
         PRINT *, '  Stop.'
         STOP
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DEGISR.
      SUBROUTINE DEGISR(RS,Z,X,DEG)

      IMPLICIT REAL*8  ( A-H, O-Z    )
      REAL*8   RS, Z, X, DEG
      DATA NCALL /0/
      DATA DX    /1.D-8/
C
C========< Entry Point >================================================
C
C--
C  Dummy section for no ISR.
C--
CNOISR       X   = 1
CNOISR       DEG = 1
CNOISR       RETURN
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         xALF0  = 1/137.036D0
         xPI    = ACOS(-1.D0)
         AME    = 0.5110034D-3
      ENDIF
C--
C  Calculate beta_e.
C--
      S    = RS*RS
      BT   = 2*xALF0/xPI * ( LOG(S/AME**2) - 1 )
      HBT  = BT/2
      HBTI = 1/HBT
      DZ   = DX**HBT
      A    = 1 + 3*HBT/4 + HBT**2/4 * ( 9.D0/8 - xPI**2/3 )
C--
      IF ( (1-Z).LE.DZ ) THEN
         X   = 1
         DEG = DZ*A + HBT**2/8 * ( 2*DX**2*LOG(DX) - DX**2 )
         DEG = DEG/DZ
         RETURN
      ENDIF
C--
C  Calculate x = E_e/E_beam.
C--
      X   = 1 - (1-Z)**HBTI
C--
C  Calculate Jacobian.
C--
      WT  = HBTI * (1-Z)**(HBTI-1)
C--
C  Calculate D_eg.
C--
      DEG = HBT * ( 1 - X )**(HBT-1) * A
     .    - HBT/2 * ( 1 + X )
     .    + HBT**2/8 * ( - 4*(1+X)*LOG(1-X)
     .                   - ((1+3*X**2)/(1-X))*LOG(X)
     .                   - (5+X) )
      DEG = DEG*WT
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, FUNCXX.
C*   FUNC for e+ e- --> X+ X- full amplitudes.
C*   QED vtx correction ( 1+3*BTE/4 ) and
C*                      ( 1 + 2*ALF0/PI * ( PI**2/6 - 1/4 ) )
C*                      included.
C*   Modified to use new ISR function.
C*
      REAL*8 FUNCTION FUNCXX(ZZ)

      IMPLICIT REAL*4 ( A-H, O-Z )
      REAL*8   ZZ(50), Z(50)
      REAL*8   ZZ1(50), RS0,XM,XP,DEGM,DEGP
      common/histv/var
      real*4     VAR(31)

C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /DTYP/   EVTYP
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      PARAMETER       ( NEXLIN = 10 )
      COMMON /XCXCCN/  ROOTS, POLE, XPHASE(3,NEXLIN-3),
     .                 IDPTCL(3,NEXLIN), IHLCMB(NEXLIN),
     .                 PVOUT(0:3,NEXLIN), DSGMDX, DPDEBM, BMENGY(0:4),
     .                 ISRBM
      REAL   *4        ROOTS, POLE, XPHASE, PVOUT, DSGMDX, DPDEBM,
     .                 BMENGY
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5

C--
C--
C  For no ISR and no BMEFF.
C--
      PARAMETER   ( MX_NZZ = 50 )
      COMMON /BSHUFL/  NZZ, ISHUFL(MX_NZZ)
      INTEGER*4        NZZ, ISHUFL

      PARAMETER  ( NP = 8, NHEL = 16 )
      INTEGER*4   IPV(3,2)
      REAL   *4   SG, PV(0:3,NP), AM(NP), AMX(2), GMX(2), AMR(3,3,2)
C--
      REAL   *4   AMSF(3,2,2), GTSF(3,2,2)
C--
      REAL   *4   QV(0:3,20)
      REAL   *8   WAT, WT
C--
      INTEGER*4   IHEL(NP,NHEL,2)
      DATA        (( IHEL(I,J,1), I=1,NP ), J=1,NHEL )  /
     .                   -1,+1,   -1, -1,+1,   -1, +1,-1,
     .                   -1,+1,   -1, -1,+1,   +1, +1,-1,
     .                   -1,+1,   +1, -1,+1,   -1, +1,-1,
     .                   -1,+1,   +1, -1,+1,   +1, +1,-1,
     .                   -1,+1,   -1, -1,+1,   -1, -1,+1,
     .                   -1,+1,   -1, -1,+1,   +1, -1,+1,
     .                   -1,+1,   +1, -1,+1,   -1, -1,+1,
     .                   -1,+1,   +1, -1,+1,   +1, -1,+1,
     .                   -1,+1,   -1, +1,-1,   -1, +1,-1,
     .                   -1,+1,   -1, +1,-1,   +1, +1,-1,
     .                   -1,+1,   +1, +1,-1,   -1, +1,-1,
     .                   -1,+1,   +1, +1,-1,   +1, +1,-1,
     .                   -1,+1,   -1, +1,-1,   -1, -1,+1,
     .                   -1,+1,   -1, +1,-1,   +1, -1,+1,
     .                   -1,+1,   +1, +1,-1,   -1, -1,+1,
     .                   -1,+1,   +1, +1,-1,   +1, -1,+1/
      DATA        (( IHEL(I,J,2), I=1,NP ), J=1,NHEL )  /
     .                   +1,-1,   -1, -1,+1,   -1, +1,-1,
     .                   +1,-1,   -1, -1,+1,   +1, +1,-1,
     .                   +1,-1,   +1, -1,+1,   -1, +1,-1,
     .                   +1,-1,   +1, -1,+1,   +1, +1,-1,
     .                   +1,-1,   -1, -1,+1,   -1, -1,+1,
     .                   +1,-1,   -1, -1,+1,   +1, -1,+1,
     .                   +1,-1,   +1, -1,+1,   -1, -1,+1,
     .                   +1,-1,   +1, -1,+1,   +1, -1,+1,
     .                   +1,-1,   -1, +1,-1,   -1, +1,-1,
     .                   +1,-1,   -1, +1,-1,   +1, +1,-1,
     .                   +1,-1,   +1, +1,-1,   -1, +1,-1,
     .                   +1,-1,   +1, +1,-1,   +1, +1,-1,
     .                   +1,-1,   -1, +1,-1,   -1, -1,+1,
     .                   +1,-1,   -1, +1,-1,   +1, -1,+1,
     .                   +1,-1,   +1, +1,-1,   -1, -1,+1,
     .                   +1,-1,   +1, +1,-1,   +1, -1,+1/
C--
      DATA NCALL /  0 /
C
C========< Entry Point >================================================
C
C--
C  Set some variables.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         AMX(1) = SWM(1)
         GMX(1) = GMSW(1)
         AMX(2) = SWM(1)
         GMX(2) = GMSW(1)
C--
         AME   = AMF(1,2,1)
C--
         AMSF(1,1,1) = SFM(1)
         AMSF(1,2,1) = SFM(2)
         AMSF(1,1,2) = SFM(4)
         AMSF(1,2,2) = SFM(6)
C--
         AMSF(2,1,1) = SFM(1)
         AMSF(2,2,1) = SFM(2)
         AMSF(2,1,2) = SFM(4)
         AMSF(2,2,2) = SFM(6)
C--
         AMSF(3,1,1) = SFM(1)
         AMSF(3,2,1) = SFM(2)
         AMSF(3,1,2) = SFM(4)
         AMSF(3,2,2) = SFM(6)
C--
         GTSF(1,1,1) = GMSF(1)
         GTSF(1,2,1) = GMSF(2)
         GTSF(1,1,2) = GMSF(4)
         GTSF(1,2,2) = GMSF(6)
C--
         GTSF(2,1,1) = GMSF(1)
         GTSF(2,2,1) = GMSF(2)
         GTSF(2,1,2) = GMSF(4)
         GTSF(2,2,2) = GMSF(6)
C--
         GTSF(3,1,1) = GMSF(1)
         GTSF(3,2,1) = GMSF(2)
         GTSF(3,1,2) = GMSF(4)
         GTSF(3,2,2) = GMSF(6)
C--
         xALF0 = 1/137.0359895E0
      ENDIF
C--
C  Shuffle integration variables.
C--
      DO 100 I = 1, NZZ
         ZZ1(ISHUFL(I)) = ZZ(I)
100   CONTINUE
C--
C  Set independent variables.
C   ZZ1( 1) : e- beam
C      ( 2) : e+ beam
C      ( 3) : bremsstrahlng e-
C      ( 4) : bremsstrahlng e+
C followings are slided with +1
C  5    ( 4) : X+ decay mode
C  6    ( 5) : X- decay mode
C  7    ( 6) : e- helicity
C  8    ( 7) : m(+ab)**2
C  9    ( 8) : m(+ac)**2
C 10    ( 9) : m(-ab)**2
C 11    (10) : m(-ac)**2
C 12    (11) : m(X+)**2
C 13    (12) : m(X-)**2
C 14    (13) : cos(theta_X-)
C 15    (14) : phi_X-
C 16    (15) : cos(theta_a)     in X+ rest frame
C 17    (16) : phi_a            in X+ rest frame
C 18    (17) : phi_b            in X+ rest frame
C 19    (18) : cos(theta_a)     in X- rest frame
C 20    (19) : phi_a            in X- rest frame
C 21    (20) : phi_b            in X- rest frame
C 22    (21) : E_beam spread of E-
C 23    (22) : E_beam spread of E+
C
C Then shift the variables ZZ1(4) --> ZZ1(23) to make the
C same as the original Z(4)-->Z(22) plus Z(23)
      Z(1) = ZZ1(1)
      Z(2) = ZZ1(2)
      Z(3) = ZZ1(3)
      DO 101 I=4,22
        Z(I) = ZZ1(I+1)
101   CONTINUE
      Z(23) = ZZ1(4)
C-----
C     Z( 1) : e- beam
C      ( 2) : e+ beam
C      ( 3) : bremsstrahlng
C      ( 4) : X+ decay mode
C      ( 5) : X- decay mode
C      ( 6) : e- helicity
C      ( 7) : m(+ab)**2
C      ( 8) : m(+ac)**2
C      ( 9) : m(-ab)**2
C      (10) : m(-ac)**2
C      (11) : m(X+)**2
C      (12) : m(X-)**2
C      (13) : cos(theta_X-)
C      (14) : phi_X-
C      (15) : cos(theta_a)     in X+ rest frame
C      (16) : phi_a            in X+ rest frame
C      (17) : phi_b            in X+ rest frame
C      (18) : cos(theta_a)     in X- rest frame
C      (19) : phi_a            in X- rest frame
C      (20) : phi_b            in X- rest frame
C      (21) : E_beam spread of E-
C      (22) : E_beam spread of E+
C      (23) : bremsstrahlng
C--
C  Reset event weight.
C--
      WAT = 1
C--
C  Beam energy.
C--
      BMENGY(0) = ROOTS/2
      BMENGY(1) = BMENGY(0)
      BMENGY(2) = BMENGY(0)
      BMENGY(3) = BMENGY(0)
      BMENGY(4) = BMENGY(0)
      DPDEBM    = 1
      RS        = ROOTS
      EMBM      = RS/2
      EPBM      = RS/2
      QED       = 1
C--
C  Then decide reduced sqrt(s) after bremsstrahlung.
C--
      IF ( ISRBM.GE.2 ) THEN
        RS0  = RS
        CALL DEGISR(RS0,Z(3),XM,DEGM)
        CALL DEGISR(RS0,Z(23),XP,DEGP)
        WAT  = WAT*DEGM*DEGP
        IF ( WAT.EQ.0.D0 ) THEN
           FUNCXX = 0
           RETURN
        ENDIF
        RS   = RS0*SQRT(XM*XP)
        EBM0 = RS0/2
        EMBM = EBM0*XM
        EPBM = EBM0*XP
      ENDIF
C--
C  First set initial states.
C--
      IDPTCL(1, 1) = 1
      IDPTCL(2, 1) = 2
      IDPTCL(3, 1) = 1
      IDPTCL(1, 2) = 1
      IDPTCL(2, 2) = 2
      IDPTCL(3, 2) = 1
C--
C  Then select final states.
C--
C-- W+ from X+.
      XW1 = Z(4)
      DO 200 IMD = 1, 12
         IF ( XW1.LT.BRVDK(IMD,1) )              GO TO 210
200   CONTINUE
C--
210   IDPTCL(1, 4) = MDVDK(1,IMD,1)
      IDPTCL(2, 4) = 1
      IDPTCL(3, 4) = MDVDK(3,IMD,1)
      IDPTCL(1, 5) = MDVDK(2,IMD,1)
      IDPTCL(2, 5) = 2
      IDPTCL(3, 5) = MDVDK(3,IMD,1)
      IMD1         = IMD
C--
      BRW1 = BRVDK(IMD,1) - BRVDK(IMD-1,1)
      WAT  = WAT/BRW1
C
C-- W- from X-.
      XW2 = Z(5)
      DO 300 IMD = 1, 12
         IF ( XW2.LT.BRVDK(IMD,1) )              GO TO 310
300   CONTINUE
C--
310   IDPTCL(1, 7) = MDVDK(1,IMD,1)
      IDPTCL(2, 7) = 1
      IDPTCL(3, 7) = MDVDK(3,IMD,1)
      IDPTCL(1, 8) = MDVDK(2,IMD,1)
      IDPTCL(2, 8) = 2
      IDPTCL(3, 8) = MDVDK(3,IMD,1)
      IMD2         = IMD
C--
      BRW2 = BRVDK(IMD,1) - BRVDK(IMD-1,1)
      WAT  = WAT/BRW2
C--
C  Select helicity combination.
C--
      HLM   = (1-POLE)/2
      IF ( Z(6).LT.HLM ) THEN
         ICMB = 1
         JCMB = NHEL*Z(6)/HLM + 1
      ELSE
         ICMB = 2
         JCMB = NHEL*(Z(6)-HLM)/(1-HLM) + 1
      ENDIF
      WAT = WAT*NHEL
      JCMB = MIN(JCMB,NHEL)
      CALL UVCOPY(NP,IHEL(1,JCMB,ICMB),IHLCMB(1))
C--
C  Select kinematic variables.
C--
      AM(1)   = AMF(IDPTCL(1,1),IDPTCL(2,1),IDPTCL(3,1))
      AM(2)   = AMF(IDPTCL(1,2),IDPTCL(2,2),IDPTCL(3,2))
      AM(3)   = SZM(1)
      AM(4)   = AMF(IDPTCL(1,4),IDPTCL(2,4),IDPTCL(3,4))
      AM(5)   = AMF(IDPTCL(1,5),IDPTCL(2,5),IDPTCL(3,5))
      AM(6)   = SZM(1)
      AM(7)   = AMF(IDPTCL(1,7),IDPTCL(2,7),IDPTCL(3,7))
      AM(8)   = AMF(IDPTCL(1,8),IDPTCL(2,8),IDPTCL(3,8))
C--
      AMR(1,1,1) = AMSF(IDPTCL(1,4),IDPTCL(2,4),IDPTCL(3,4))
      AMR(2,1,1) = GTSF(IDPTCL(1,4),IDPTCL(2,4),IDPTCL(3,4))
      AMR(3,1,1) = AMR(1,1,1) + AM(5)
      AMR(1,2,1) = AMSF(IDPTCL(1,5),IDPTCL(2,5),IDPTCL(3,5))
      AMR(2,2,1) = GTSF(IDPTCL(1,5),IDPTCL(2,5),IDPTCL(3,5))
      AMR(3,2,1) = AMR(1,2,1) + AM(4)
      AMR(1,3,1) = AMW
      AMR(2,3,1) = GMWTOT
      AMR(3,3,1) = AMR(1,3,1) + AM(3)
      AMR(1,1,2) = AMSF(IDPTCL(1,7),IDPTCL(2,7),IDPTCL(3,7))
      AMR(2,1,2) = GTSF(IDPTCL(1,7),IDPTCL(2,7),IDPTCL(3,7))
      AMR(3,1,2) = AMR(1,1,2) + AM(8)
      AMR(1,2,2) = AMSF(IDPTCL(1,8),IDPTCL(2,8),IDPTCL(3,8))
      AMR(2,2,2) = GTSF(IDPTCL(1,8),IDPTCL(2,8),IDPTCL(3,8))
      AMR(3,2,2) = AMR(1,2,2) + AM(7)
      AMR(1,3,2) = AMW
      AMR(2,3,2) = GMWTOT
      AMR(3,3,2) = AMR(1,3,2) + AM(6)
C--
C  Set phase space variables.
C--
      CALL GETXPH(RS,AM,AMX,GMX,AMR,Z(7),IPV,XPHASE,WT)
      IF ( WT.LE.0.D0 ) THEN
         FUNCXX = 0
         RETURN
      ENDIF
      WAT = WAT*WT
C--
C  Calculate differential cross sections.
C--
      CALL SGCXXF(IDPTCL,IHLCMB,AM,IPV,XPHASE,SG,PV)
      SG     = SG*QED
      FUNCXX   = SG*WAT
      DSGMDX = SG
      IF ( FUNCXX.LE.0.D0 ) RETURN
C--
C  Boost everything to laboratroy frame.
C--
      QV(0,1) = EMBM + EPBM
      QV(1,1) = 0
      QV(2,1) = 0
      QV(3,1) = EMBM - EPBM
      IF ( ABS(QV(3,1)).GT.1.E-3 ) THEN
         DO 500 IP = 1, NP
            CALL BOOSTx(PV(0,IP),QV(0,1),PVOUT(0,IP))
500      CONTINUE
      ELSE
         CALL UVCOPY(4*NP,PV(0,1),PVOUT(0,1))
      ENDIF
C--
C  Fill hists. and plots.
C--
      CALL UVCOPY(4*NP,PVOUT(0,1),PV(0,1))
C--
      EBM   = RS/2
      S     = RS*RS
      CALL PSUMxx(PV(0,3),PV(0,6),QV(0,1))
      PTOP  = SQRT( QV(1,1)**2 + QV(2,1)**2 )
C--
      CALL PSUMxx(PV(0,4),PV(0,5),QV(0,2))
      CALL PSUMxx(QV(0,2),PV(0,3),QV(0,11))
      CALL PSUMxx(PV(0,7),PV(0,8),QV(0,3))
      CALL PSUMxx(QV(0,3),PV(0,6),QV(0,12))
      CALL PMIRRx(QV(0,11),QV(0,11))
      CALL PMIRRx(QV(0,12),QV(0,12))
      CALL BOOSTx(PV(0,5),QV(0,11),QV(0,15))
      CALL BOOSTx(PV(0,8),QV(0,12),QV(0,18))
      CS15 = UDOT3(QV(1,15),QV(1,11))/UABSV3(QV(1,15))/UABSV3(QV(1,11))
      CS18 = UDOT3(QV(1,18),QV(1,12))/UABSV3(QV(1,18))/UABSV3(QV(1,12))
C--
      CALL PSUMxx(PV(0,3),PV(0,5),QV(0,13))
      CALL PSUMxx(PV(0,6),PV(0,8),QV(0,16))
      CALL PMIRRx(QV(0,13),QV(0,13))
      CALL PMIRRx(QV(0,16),QV(0,16))
      CALL BOOSTx(PV(0,5),QV(0,13),QV(0,15))
      CALL BOOSTx(PV(0,8),QV(0,16),QV(0,18))
      CS25 = UDOT3(QV(1,15),QV(1,13))/UABSV3(QV(1,15))/UABSV3(QV(1,13))
      CS28 = UDOT3(QV(1,18),QV(1,16))/UABSV3(QV(1,18))/UABSV3(QV(1,16))
C--
      COSL  =  PV(3,8)/UABSV3(PV(1,8))
      COSLB = -PV(3,5)/UABSV3(PV(1,5))
      COSJ  =  QV(3,3)/UABSV3(QV(1,3))
      COSJB = -QV(3,2)/UABSV3(QV(1,2))
      PT45  = MAX(SQRT( QV(1,2)**2 + QV(2,2)**2 ),.1)
      PT78  = MAX(SQRT( QV(1,3)**2 + QV(2,3)**2 ),.1)
      ACOP  = ( QV(1,2)*QV(1,3) + QV(2,2)*QV(2,3) )/PT45/PT78
      IF ( ABS(ACOP).GE.1.0 ) ACOP = SIGN(1.0,ACOP)
      ACOP  = xPI - ACOS(ACOP)
      COST  = XPHASE(2,1)
      PHIT  = XPHASE(3,1)
      COSBB = XPHASE(2,2)
      PHIBB = XPHASE(3,2)
      COSB  = XPHASE(2,4)
      PHIB  = XPHASE(3,4)
      COMB  = JCMB + NHEL*(ICMB-1) + .1
      QXP2  = XPHASE(1,2)
      Q42   = XPHASE(1,3)
      Q52   = XPHASE(2,3)
      QXM2  = XPHASE(1,4)
      Q72   = XPHASE(1,5)
      Q82   = XPHASE(2,5)
C--
      var(1)=COST
      var(2)=PHIT*xRD2DG
      var(3)=COSBB      
      var(4)=PHIBB*xRD2DG
      var(5)=COSB       
      var(6)=PHIB*xRD2DG
      var(7)=SQRT(Q42)  
      var(8)=SQRT(Q52)  
      var(9)=PTOP 
      var(10)=ACOP*xRD2DG

      var(11)=SQRT(Q72)  

      var(12)=SQRT(Q82)  

      var(13)=COMB 

      var(14)=PV(0,5)
      var(15)=COSLB
      var(16)=CS25

      var(17)=PV(0,8)
      var(18)=COSL
      var(19)=CS28
      var(20)=QV(0,2)
      var(21)=COSJB  
      var(22)=QV(0,3)
      var(23)=COSJ 
      var(24)=EVTYP      
      var(25)=(IMD1+.1)  
      var(26)=(IMD2+.1)  
      var(27)=RS/ROOTS   
      var(28)=CS15       
      var(29)=CS18       
      var(30)=SQRT(QXP2)
      var(31)=SQRT(QXM2)


c      CALL HFILL(  1, COST,0.,1. )
c      CALL HFILL(  2, PHIT*xRD2DG,0.,1. )
c      CALL HFILL(  3, COSBB      ,0.,1. )
c      CALL HFILL(  4, PHIBB*xRD2DG,0.,1. )
c      CALL HFILL(  5, COSB       ,0.,1. )
c      CALL HFILL(  6, PHIB *xRD2DG,0.,1. )
c      CALL HFILL(  7, SQRT(Q42)  ,0.,1. )
c      CALL HFILL(  8, SQRT(Q52)  ,0.,1. )
c      CALL HFILL(  9, PTOP,0.,1. )
c      CALL HFILL( 10, ACOP*xRD2DG,0.,1. )
c      CALL HFILL( 11, SQRT(Q72)  ,0.,1. )
c      CALL HFILL( 12, SQRT(Q82)  ,0.,1. )
c      CALL HFILL( 13, COMB,0.,1. )
c      IF ( IDPTCL(3,5).EQ.1 .AND. IDPTCL(1,5).LE.2 ) THEN
c         CALL HFILL( 14, PV(0,5)    ,0.,1. )
c         CALL HFILL( 15, COSLB      ,0.,1. )
c         CALL HFILL( 27, CS25,0.,1. )
c      ENDIF
c      IF ( IDPTCL(3,8).EQ.1 .AND. IDPTCL(1,8).LE.2 ) THEN
c         CALL HFILL( 14, PV(0,8)    ,0.,1. )
c         CALL HFILL( 15, COSL,0.,1. )
c         CALL HFILL( 28, CS28,0.,1. )
c      ENDIF
c      IF ( IDPTCL(3,5).EQ.2 ) THEN
c         CALL HFILL( 16, QV(0,2)    ,0.,1. )
c         CALL HFILL( 17, COSJB     ,0.,1. )
c      ENDIF
c      IF ( IDPTCL(3,8).EQ.2 ) THEN
c         CALL HFILL( 16, QV(0,3)    ,0.,1. )
c         CALL HFILL( 17, COSJ,0.,1. )
c      ENDIF
c      EVTYP = IDPTCL(3,4) + IDPTCL(3,7) -0.9
c      CALL HFILL( 18, EVTYP         ,0.,1. )
c      CALL HFILL( 19, (IMD1+.1)       ,0.,1. )
c      CALL HFILL( 20, (IMD2+.1)       ,0.,1. )
c      CALL HFILL( 21, RS/ROOTS     ,0.,1. )
c      CALL HFILL( 23, CS15          ,0.,1. )
c      CALL HFILL( 24, CS18          ,0.,1. )
c      CALL HFILL( 25, SQRT(QXP2)    ,0.,1. )
c      CALL HFILL( 26, SQRT(QXM2)    ,0.,1. )
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, HELASC.
C ----------------------------------------------------------------------
C
      SUBROUTINE FVIXXC(FI,VC,G,FMASS,FWIDTH , FVI)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-IN external fermion and a vector boson.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   !FI>
C       complex VC(6)          : input    vector                      V
C       complex G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FVI(6)         : off-shell fermion             !F',V,FI>
C
      COMPLEX FI(6),VC(6),FVI(6),SL1,SL2,SR1,SR2,D,CI,G(2)
      REAL    PF(0:3),FMASS,FWIDTH,PF2
C
      FVI(5) = FI(5)-VC(5)
      FVI(6) = FI(6)-VC(6)
C
      PF(0)=REAL( FVI(5))
      PF(1)=REAL( FVI(6))
      PF(2)=AIMAG(FVI(6))
      PF(3)=AIMAG(FVI(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FI(1)
     &    +(VC(2)-CI*VC(3))*FI(2)
      SL2= (VC(2)+CI*VC(3))*FI(1)
     &    +(VC(1)-   VC(4))*FI(2)
C
      IF (ABS(G(2)).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FI(3)
     &    -(VC(2)-CI*VC(3))*FI(4)
      SR2=-(VC(2)+CI*VC(3))*FI(3)
     &    +(VC(1)+   VC(4))*FI(4)
C
      FVI(1) = ( G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVI(2) = ( G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
      FVI(3) = ( G(2)*((PF(0)+PF(3))*SR1 +CONJG(FVI(6))*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVI(4) = ( G(2)*(       FVI(6)*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
C
      RETURN
C
  10  FVI(1) = G(1)*((PF(0)-PF(3))*SL1 -CONJG(FVI(6))*SL2)*D
      FVI(2) = G(1)*(      -FVI(6)*SL1 +(PF(0)+PF(3))*SL2)*D
      FVI(3) = G(1)*FMASS*SL1*D
      FVI(4) = G(1)*FMASS*SL2*D
C
      RETURN
      END
C ----------------------------------------------------------------------
C
      SUBROUTINE FVOXXC(FO,VC,G,FMASS,FWIDTH , FVO)
C
C This subroutine computes an off-shell fermion wavefunction from a
C flowing-OUT external fermion and a vector boson.
C
C INPUT:
C       complex FO(6)          : flow-out fermion                   <FO!
C       complex VC(6)          : input    vector                      V
C       complex G(2)           : coupling constants                  GVF
C       real    FMASS          : mass  of OUTPUT fermion F'
C       real    FWIDTH         : width of OUTPUT fermion F'
C
C OUTPUT:
C       complex FVO(6)         : off-shell fermion             <FO,V,F'!
C
      COMPLEX FO(6),VC(6),FVO(6),SL1,SL2,SR1,SR2,D,CI,G(2)
      REAL    PF(0:3),FMASS,FWIDTH,PF2
C
      FVO(5) = FO(5)+VC(5)
      FVO(6) = FO(6)+VC(6)
C
      PF(0)=REAL( FVO(5))
      PF(1)=REAL( FVO(6))
      PF(2)=AIMAG(FVO(6))
      PF(3)=AIMAG(FVO(5))
      CI=CMPLX(0.,1.)
      PF2=PF(0)**2-(PF(1)**2+PF(2)**2+PF(3)**2)
C
      D=-1./CMPLX( PF2-FMASS**2 , MAX(SIGN( FMASS*FWIDTH ,PF2),0.) )
      SL1= (VC(1)+   VC(4))*FO(3)
     &    +(VC(2)+CI*VC(3))*FO(4)
      SL2= (VC(2)-CI*VC(3))*FO(3)
     &    +(VC(1)-   VC(4))*FO(4)
C
      IF (ABS(G(2)).EQ.0.) GOTO 10
C
      SR1= (VC(1)-   VC(4))*FO(1)
     &    -(VC(2)+CI*VC(3))*FO(2)
      SR2=-(VC(2)-CI*VC(3))*FO(1)
     &    +(VC(1)+   VC(4))*FO(2)
C
      FVO(1) = ( G(2)*( (PF(0)+PF(3))*SR1        +FVO(6)*SR2)
     &          +G(1)*FMASS*SL1)*D
      FVO(2) = ( G(2)*( CONJG(FVO(6))*SR1 +(PF(0)-PF(3))*SR2)
     &          +G(1)*FMASS*SL2)*D
      FVO(3) = ( G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)
     &          +G(2)*FMASS*SR1)*D
      FVO(4) = ( G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)
     &          +G(2)*FMASS*SR2)*D
C
      RETURN
C
  10  FVO(1) = G(1)*FMASS*SL1*D
      FVO(2) = G(1)*FMASS*SL2*D
      FVO(3) = G(1)*( (PF(0)-PF(3))*SL1        -FVO(6)*SL2)*D
      FVO(4) = G(1)*(-CONJG(FVO(6))*SL1 +(PF(0)+PF(3))*SL2)*D
C
      RETURN
      END

      SUBROUTINE VSSXXC(VC,S1,S2,GC , VERTEX)
C
C This subroutine computes an amplitude from the vector-scalar-scalar
C coupling.  The coupling is absent in the minimal SM in unitary gauge.
C
C       complex VC(6)          : input  vector                        V
C       complex S1(3)          : first  scalar                        S1
C       complex S2(3)          : second scalar                        S2
C       complex GC             : coupling constant (S1 charge)
C
C Examples of the coupling constant GC for SUSY particles are as follows:
C   -----------------------------------------------------------
C   |    S1    | (Q,I3) of S1  ||   V=A   |   V=Z   |   V=W   |
C   -----------------------------------------------------------
C   | nu~_L    | (  0  , +1/2) ||   ---   |  GZN(1) |  GWF(1) |
C   | e~_L     | ( -1  , -1/2) ||  GAL(1) |  GZL(1) |  GWF(1) |
C   | u~_L     | (+2/3 , +1/2) ||  GAU(1) |  GZU(1) |  GWF(1) |
C   | d~_L     | (-1/3 , -1/2) ||  GAD(1) |  GZD(1) |  GWF(1) |
C   -----------------------------------------------------------
C   | e~_R-bar | ( +1  ,  0  ) || -GAL(2) | -GZL(2) | -GWF(2) |
C   | u~_R-bar | (-2/3 ,  0  ) || -GAU(2) | -GZU(2) | -GWF(2) |
C   | d~_R-bar | (+1/3 ,  0  ) || -GAD(2) | -GZD(2) | -GWF(2) |
C   -----------------------------------------------------------
C where the S1 charge is defined by the flowing-OUT quantum number.
C
C OUTPUT:
C       complex VERTEX         : amplitude                Gamma(V,S1,S2)
C
      COMPLEX GC,VC(6),S1(3),S2(3),VERTEX
      REAL    P(0:3)
C
      P(0)=REAL( S1(2)-S2(2))
      P(1)=REAL( S1(3)-S2(3))
      P(2)=AIMAG(S1(3)-S2(3))
      P(3)=AIMAG(S1(2)-S2(2))
C
      VERTEX = GC*S1(1)*S2(1)
     &        *(VC(1)*P(0)-VC(2)*P(1)-VC(3)*P(2)-VC(4)*P(3))
C
      RETURN
      END

      SUBROUTINE IOVXXC(FI,FO,VC,G , VERTEX)
C
C This subroutine computes an amplitude of the fermion-fermion-vector
C coupling.
C
C INPUT:
C       complex FI(6)          : flow-in  fermion                   |FI>
C       complex FO(6)          : flow-out fermion                   <FO|
C       complex VC(6)          : input    vector                      V
C       real    G(2)           : coupling constants                  GVF
C
C OUTPUT:
C       complex VERTEX         : amplitude                     <FO|V|FI>
C
      COMPLEX FI(6),FO(6),VC(6),VERTEX
C>>>
C     REAL    G(2)
      COMPLEX G(2)
C>>>
C
      VERTEX =  G(1)*( (FO(3)*FI(1)+FO(4)*FI(2))*VC(1)
     &                +(FO(3)*FI(2)+FO(4)*FI(1))*VC(2)
     &                -(FO(3)*FI(2)-FO(4)*FI(1))*VC(3)*CMPLX(0.,1.)
     &                +(FO(3)*FI(1)-FO(4)*FI(2))*VC(4)             )
      IF (ABS(G(2)).NE.0.) VERTEX = VERTEX
     &        + G(2)*( (FO(1)*FI(3)+FO(2)*FI(4))*VC(1)
     &                -(FO(1)*FI(4)+FO(2)*FI(3))*VC(2)
     &                +(FO(1)*FI(4)-FO(2)*FI(3))*VC(3)*CMPLX(0.,1.)
     &                -(FO(1)*FI(3)-FO(2)*FI(4))*VC(4)             )
C
      RETURN
      END
CDECK  ID>, MAIN.
      PROGRAM MAIN
*
***********************************************************************
*
*   MAIN routine for the MC generator shell
*
***********************************************************************
*
*
* Datacards
*
       INTEGER TRIG
       COMMON /STDATA/ TRIG
*
       REAL BEAM
       COMMON /STDATB/ BEAM
*
       INTEGER IPMC
       COMMON /MCPARA/ IPMC
*
       PARAMETER (MAXUSR=6)
       REAL RUSR(MAXUSR)
       COMMON /MCUSER/ RUSR
*
       COMMON/MCONS1/DMZ
       COMMON/MCONS2/DMW
       COMMON/MCONS3/DMH
       COMMON/MCONS4/DMT
       COMMON/MCONS5/DALPE
       COMMON/MCONS6/DALPS
       COMMON/MCONS7/DSIN2THW
*
*
* *** Initialise FFREAD
*
      CALL FFINIT(0)
*
* **  Initialize DFGT
*
      CALL MCEEVT(-2)
*
* *** Define keys specific for this generator.
*
      CALL FFKEY ('TRIG',TRIG,1,'INTEGER')
      CALL FFKEY ('BEAM',BEAM,1,'REAL')
      CALL FFKEY ('IPMC',IPMC,1,'INTEGER')
      CALL FFKEY ('RUSR',RUSR(1),MAXUSR,'REAL')
      CALL FFKEY ('DMZ',DMZ,1,'REAL')
      CALL FFKEY ('DMW',DMW,1,'REAL')
      CALL FFKEY ('DMH',DMH,1,'REAL')
      CALL FFKEY ('DMT',DMT,1,'REAL')
      CALL FFKEY ('ALPH',DALPE,1,'REAL')
      CALL FFKEY ('ALPS',DALPS,1,'REAL')
      CALL FFKEY ('SINW',DSIN2THW,1,'REAL')
*
      CALL FFGO
*
      CALL MCEEVT(-1)
*
* *** Loop of generation
*
      DO K=1,TRIG
        CALL MCEEVT(0)
      ENDDO
*
* *** Termination step
*
      CALL MCEEVT(1)
*
      STOP
      END
CDECK  ID>, SWPELM.
C----------------------------------------------------------------------C
      SUBROUTINE SWPELM( ARRAY1, IORDER, ARRAY2, IDIM )
C----------------------------------------------------------------------C
C*  Swap ARRAY1(*) in decreasing order.
C*  Input  ARRAY1(*), IDIM
C*  Output IORDER(*), ARRAY2(*)
C----------------------------------------------------------------------C
      REAL*4    ARRAY1(IDIM), ARRAY2(IDIM)
      INTEGER*4 IORDER(IDIM)
C--------------------------------------
C  Reset arrays first.
      DO 1 I = 1, IDIM
        IORDER(I) = 0
1       ARRAY2(I) = 0

      DO 10 I1 = 1, IDIM
       IORDER(I1) = 1
       DO 20 I2 = 1, IDIM
        IF( ARRAY1(I1) .LT. ARRAY1(I2) ) IORDER(I1) = IORDER(I1)+1
20     CONTINUE
       ARRAY2(IORDER(I1)) = ARRAY1(I1)
10    CONTINUE

      DO 30 I = 2, IDIM
        IYES = 0
        DO 40 J = 1, IDIM
         IF( I .EQ. IORDER(J) ) IYES = 1
40      CONTINUE
      IF( IYES .EQ. 0 ) ARRAY2(I) = ARRAY2(I-1)
30    CONTINUE

C    Check IORDER(*)
      DO 50 I = 1, IDIM
       IF( IORDER(I) .LE. 0 .OR. IORDER(I) .GT. IDIM ) THEN
         WRITE(6,*) ' Error at SWPELM: Order array (IORDER)',
     &              ' may be wrong.'
         STOP
       ENDIF
50    CONTINUE

      RETURN
      END
CDECK  ID>, USORTD.
C*********************************************************************
C*
C*    ----------------------------------------======
C*    SUBROUTINE USORTD(NREC, LREC, KEY, ARY , IPNT)
C*    ----------------------------------------======
C*
C*(Function)
C*    Quick sorter of an real*4 array.
C*
C*(Input)
C*    NREC    :  Number of records.
C*    LREC    :  Record size.
C*    KEY     :  Key element pointer in a record.
C*    ARY     :  Real    array to be sortted.
C*
C*(Output)
C*    IPNT    :  Pointer containing the results.
C*
C*(Author)
C*
C*    Hirofumi Fujii    Jan 12, 1983
C*    A. Miyamoto         6-Sep-1985, modified the arguments.
C*
C*********************************************************************
C*
      SUBROUTINE USORTD(NREC, LREC, KEY, ARY, IPNT)
C
      IMPLICIT REAL*8 ( A-H, O-Z )
      INTEGER*4   IPNT(NREC)
      REAL*8      ARY(LREC, NREC)
      PARAMETER  (ISTKSZ = 32 ,
     >            ILEFT  = 1,
     >            IRIGHT = 2 )
      INTEGER     ISTACK(2, ISTKSZ)
C
C*
C*(1)  Initialize.
C*
      DO 100 I = 1, NREC
         IPNT(I) = I
100   CONTINUE
      ISP   = 1
      ISTACK(ILEFT ,ISP) = 1
      ISTACK(IRIGHT,ISP) = NREC
C*
C*(2)  Get the most left and most right position from the stack.
C*
200   CONTINUE
        NLEFT  = ISTACK(ILEFT, ISP)
        NRIGHT = ISTACK(IRIGHT,ISP)
        ISP    = ISP - 1
C*
C*(3)  Determine the range to be sortted.
C*
300     CONTINUE
          IL   = NLEFT
          IR   = NRIGHT
          IM   = (IL+IR)/2
C>>>
C           VALKEY = ARY(KEY, IPNT(IM))
          VALKEY = ABS(ARY(KEY, IPNT(IM)))
C>>>
C*
C*(4)   Swap the data with respect to the VALKEY.
C*
400       CONTINUE
C>>>
C 410         IF(ARY(KEY,IPNT(IL)).GE.VALKEY) GO TO 420
410         IF(ABS(ARY(KEY,IPNT(IL))).GE.VALKEY) GO TO 420
C>>>
              IL = IL + 1
              IF(IL.GT.NREC)
     >        PRINT *,'Warning in USORTD  IL greater than NREC...'

              GO TO 410
C>>>
C  420        IF(ARY(KEY,IPNT(IR)).LE.VALKEY) GO TO 430
420         IF(ABS(ARY(KEY,IPNT(IR))).LE.VALKEY) GO TO 430
C>>>
              IR = IR - 1
              IF(IR.LE.0)
     >        PRINT *,'Warning in USORTD  IR less than 0....'
              GO TO 420
C
430         CONTINUE
C
C    Swap data.
C
            IF(IL.LE.IR) THEN
              ISAVE = IPNT(IL)
              IPNT(IL) = IPNT(IR)
              IPNT(IR) = ISAVE
              IL = IL + 1
              IR = IR - 1
            END IF

            IF(IL.LE.IR) GO TO 400

C*
C*(5)  Find out the next region to be sorted.
C*
            IF(IR-NLEFT.LT.NRIGHT-IL) THEN
              IF(IL.LT.NRIGHT) THEN
                ISP = ISP + 1
                ISTACK(ILEFT ,ISP) = IL
                ISTACK(IRIGHT,ISP) = NRIGHT
              ENDIF
              NRIGHT = IR
            ELSE
              IF(NLEFT.LT.IR) THEN
                ISP = ISP + 1
                ISTACK(ILEFT ,ISP) = NLEFT
                ISTACK(IRIGHT,ISP) = IR
              ENDIF
              NLEFT = IL
            ENDIF

          IF(NLEFT.LT.NRIGHT) GO TO 300

        IF(ISP.GT.0) GO TO 200

      RETURN
      END
CDECK  ID>, DFGT_IN.
      SUBROUTINE DFGT_IN

      IMPLICIT REAL*8 (A-H,O-Z)
C--
      PARAMETER (MXDIM = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
C--
      PARAMETER       ( NEXLIN = 10 )
      COMMON /XCXCCN/  ROOTS, POLE, XPHASE(3,NEXLIN-3),
     .                 IDPTCL(3,NEXLIN), IHLCMB(NEXLIN),
     .                 PVOUT(0:3,NEXLIN), DSGMDX, DPDEBM, BMENGY(0:4),
     .                 ISRBM
      REAL   *4        ROOTS, POLE, XPHASE, PVOUT, DSGMDX, DPDEBM,
     .                 BMENGY
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
C* (Contents)
C*    SQRTS   : (R*4) : sqrt(s).
C*    POLEBM  : (R*4) : electron beam polarization.
C*    SGMEBM  : (R*4) : beam energy spread (fraction).
C*    ISRB    : (I*4) : ISR and BM flag.
C*

      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO
      INTEGER*4       ISRB
      COMMON /SSCONS/ AM0, AMU, AM2, TANB, AMA, RELAX
      REAL*4          AM0, AMU, AM2, TANB, AMA, RELAX
C* (Contents)
C*    ALFI  : (R*4) : 1/alpha(m_Z).
C*    ALFS  : (R*4) : alpha_s(m_Z).
C*    AMSW  : (R*4) : m_W (GeV).
C*    AMSZ  : (R*4) : m_Z (GeV).
C*    AMSH  : (R*4) : m_H (GeV).
C*    AMST  : (R*4) : m_t (GeV).
C*    AS2W  : (R*4) : sin2tw.
C*
      COMMON /USMPRM/  ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      REAL*4           ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      COMMON /BEMCNS/ SGEBM
      REAL*4          SGEBM
C--
C  For no ISR and no BMEFF.
C--
      PARAMETER   ( MX_NZZ = 50 )
      COMMON /BSHUFL/  NZZ, ISHUFL(MX_NZZ)
      INTEGER*4        NZZ, ISHUFL
      COMMON /BSITPR/  NCAL, ITM1, ITM2
      INTEGER*4        NCAL, ITM1, ITM2
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5

      CHARACTER*60 title
C--

C--
C  Local R*4 variables used in subroutine calls.
C--
      REAL*4        ALF, S2W
C
C========< Entry Point >================================================
C
C--
C  Initialize numerical constants and particle table.
C--
CSG      ALF   = 1/ALFI
CSG      CALL INSMCN(ALF,ALFS,AMSW,AMSZ,AMSH,AMST,AS2W)
CSG      S2W = xSIN2W
CSG      CALL INSSCN(ALF,ALFS,S2W,AMSW,AMSZ,AM0,AMU,AM2,TANB,AMA)
C--
      IF ( GAMSW1.GT.0.0 ) THEN
         CALL MODGAM('SW',1,GAMSW1)
      ENDIF
C--
C  Set user constants.
C--
      ROOTS  = SQRTS
      SGEBM  = SGMEBM
      POLE   = POLEBM
      ISRBM  = ISRB
      WRITE(*,*)' '
      WRITE(*,*)' ******< Job constants >*********** '
      WRITE(*,*)'  '
      WRITE(*,*)'     ROOTS  = ', ROOTS
      WRITE(*,*)'     SGEBM  = ', SGEBM
      WRITE(*,*)'     POLE   = ', POLE
      WRITE(*,*)'     GAMSW1 = ', GAMSW1
      WRITE(*,*)'     GAMINP = ', GAMWINO
      WRITE(*,*)'     ISRBM  = ', ISRBM
      WRITE(*,*)'  '
      WRITE(*,*)' ********************************** '
      WRITE(*,*)' '
C--
C  Set BASES constants.
C--
C mod. 95/07/19 by T.T
      IF ( ISRBM.EQ.1 ) THEN
         NDIM   = 17
         NWILD  = 7
         IOFF   = 3  +1
      ELSE IF ( ISRBM.EQ.2 ) THEN
         NDIM   = 18 +1
         NWILD  = 8  +1
         IOFF   = 2
      ELSE IF ( ISRBM.EQ.3 ) THEN !Not use this option !!!! (S.G.)
         NDIM   = 22 +1
         NWILD  = 10 +1
         NWILD  = 10    ! I want to use NWILD=11 but not allowed
         IOFF   = 0
      ELSE
         WRITE(*,*)' >>> USERIN : Invalid ISRBM = ', ISRBM
         WRITE(*,*)'   Will STOP immediately. '
         STOP
      ENDIF
C>>>
C      NOIG = NWILD + 1
      NOIG = 999
C>>>
C--
      NZZ = NDIM
C--
      NCALL  = NCAL
      ITMX1  = ITM1
      ITMX2  = ITM2
C--
C  Set lower and upper limits.
C--
      DO 100 IX = 1, NDIM
         XL(IX) = 0
         XU(IX) = 1
         IF ( IX.GE.NOIG ) IG(IX) = 0
         ISHUFL(IX) = IX + IOFF
100   CONTINUE
C--
C  Define hists. and plots.
C--
      PTMX = ROOTS/2
      QMX  = ROOTS/2
c      CALL XHINIT( 1, -1.0D0, 1.0D0, 50,'cos(theta_X-)      ')
c      CALL XHINIT( 2,  0.0D0,360.D0, 50,'phi_X-             ')
c      CALL XHINIT( 3, -1.0D0, 1.0D0, 50,'cos(theta_a+)      ')
c      CALL XHINIT( 4,  0.0D0,360.D0, 50,'phi_b+             ')
c      CALL XHINIT( 5, -1.0D0, 1.0D0, 50,'cos(theta_a-)      ')
c      CALL XHINIT( 6,  0.0D0,360.D0, 50,'phi_a-             ')
c      CALL XHINIT( 7,  0.0D0,   QMX, 50,'M_ab+              ')
c      CALL XHINIT( 8,  0.0D0,   QMX, 50,'M_ac+              ')
c      CALL XHINIT( 9,  0.0D0,  PTMX, 50,'Missing PT         ')
c      CALL XHINIT(10,  0.0D0,180.D0, 50,'Acop               ')
c      CALL XHINIT(11,  0.0D0,   QMX, 50,'M_ab-              ')
c      CALL XHINIT(12,  0.0D0,   QMX, 50,'M_ac-              ')
c      CALL XHINIT(13,  1.0D0, 33.D0, 32,'Hel. comb.         ')
c      CALL XHINIT(14,  0.0D0,   QMX, 50,'E_e/mu             ')
c      CALL XHINIT(15, -1.0D0, 1.0D0, 50,'cos(theta_l-)      ')
c      CALL XHINIT(16,  0.0D0,   QMX, 50,'E_qqbar            ')
c      CALL XHINIT(17, -1.0D0, 1.0D0, 50,'cos(theta_J-)      ')
c      CALL XHINIT(18,  1.0D0,  4.D0,  3,'Topology (LL,LJ,JJ)')
c      CALL XHINIT(19,  1.0D0, 13.D0, 12,'Decay mode( X+ )   ')
c      CALL XHINIT(20,  1.0D0, 13.D0, 12,'Decay mode( X- )   ')
c      CALL XHINIT(21,   .0D0,  1.D0, 50,'sqrt(rs)/sqrt(root)')
c      CALL XHINIT(23, -1.0D0, 1.0D0, 50,'cos(theta_fdbar)   ')
c      CALL XHINIT(24, -1.0D0, 1.0D0, 50,'cos(theta_fd)      ')
c      CALL XHINIT(25,  0.0D0,   QMX, 50,'M_X+               ')
c      CALL XHINIT(26,  0.0D0,   QMX, 50,'M_X-               ')
c      CALL XHINIT(27, -1.0D0, 1.0D0, 50,'cos(theta_l+)_35   ')
c      CALL XHINIT(28, -1.0D0, 1.0D0, 50,'cos(theta_l-)_68   ')

      CALL HBOOK1( 1,'cos(thetaX-)      ',50, -1.0, 1.0,0.) 
      CALL HBOOK1( 2,'phiX-             ',50,  0.0,360.,0.)
      title='cos(theta a+) (X+ in a+b+c) X+ rest frame' 
      CALL HBOOK1( 3,title,50, -1.0, 1.0,0.)
      title='phib+  (X+ in a+b+c) X+ rest frame'
      CALL HBOOK1( 4,title,50,  0.0,360.,0.)
      title='cos(theta a-) (X- in a+b+c) X- rest frame' 
      CALL HBOOK1( 5,title,50, -1.0, 1.0,0.)
      title= 'phia- (X- in a+b+c) X- rest frame'
      CALL HBOOK1( 6,title,50,0.0,360.,0.)
      CALL HBOOK1( 7,'Mab+  (X+  in a+b+c)', 50, 0.0,  ROOTS,0.)
      CALL HBOOK1( 8,'Mac+  (X+  in a+b+c)',50, 0.0,   ROOTS,0.)
      CALL HBOOK1( 9,' Missing Pt',50, 0.0, ROOTS,0.)
      CALL HBOOK1(10,'Acop', 50, 0.0,180.,0.)
      CALL HBOOK1(11,'Mab-  (X- in a+b+c)',50,  0.0,   ROOTS,0.)
      CALL HBOOK1(12,'Mac-  (X- in a+b+c)',50,  0.0,   ROOTS,0.)
      CALL HBOOK1(13,'Hel. com', 32, 1.0, 33.,0.)
      CALL HBOOK1(14,'E(e/mu)',50,  0.0,   ROOTS,0.)
      CALL HBOOK1(15,'cos(theta l-) lab frame',50, -1.0, 1.0,0.)
      CALL HBOOK1(16,'Eqqbar ',50,  0.0,   ROOTS,0.)
      CALL HBOOK1(17,'cos(theta J-) lab frame',50, -1.0, 1.0,0.)
      CALL HBOOK1(18,'Topology (LL,LJ,JJ)',50,  1.0, 4.,0.)
      CALL HBOOK1(19,'Decay mode( X+ ) (e,m,tau),',12,1.0, 13.,0.)
      CALL HBOOK1(20,'Decay mode( X- ) (e,m.tau)',12,1.0, 13.,0.)
      CALL HBOOK1(21,'sqrt(rs)/sqrt(root)',50,   .0,  1.,0.)
      CALL HBOOK1(23,'cos(theta fdbar) X+ rest frame',50,-1.0, 1.0,0.)
      CALL HBOOK1(24,'cos(theta fd) X- rest frame',50,   -1.0, 1.0,0.)
      CALL HBOOK1(25,'Mass X+ ',50,  0.0,   ROOTS,0.)
      CALL HBOOK1(26,'Mass X-',50,  0.0,  ROOTS ,0.)
      title='cos(theta l+) X0fdbar rest frame'
      CALL HBOOK1(27, title,50,-1.0,1.0,0.)
      CALL HBOOK1(28,'cos(theta l-) X0fd rest frame',50,-1.0,1.0 ,0.)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, GETXPH.
CC**********************************************************************
C*
C*=======================================-----------===
C* Subroutine GETXPH(RS,AM,AMX,GMX,AMR,Z,IPV,XPH,WAT)
C*=======================================-----------===
C*
C* (Purpose)
C*    Generate phase space variables for a given set of random numbers
C*    of this iteration.
C* (Inputs)
C*       RS         : (R*4) : sqrt(s).
C*       AM(i)      : (R*4) : mass  of i.
C*       AMX(1)     : (R*4) : mass  of X-.
C*          (2)     : (R*4) : mass  of X+.
C*       GMX(1)     : (R*4) : width of X-.
C*          (2)     : (R*4) : width of X+.
C*       AMR(1,r,1) : (R*4) : mass of resonance r
C*          (2,r,1) : (R*4) : width of resonance r           for X+.
C*          (3,r,1) : (R*4) : m_(ij) + m_k  for r <--> (ij)
C*       AMR(1,r,2) : (R*4) : mass of resonance r
C*          (2,r,2) : (R*4) : width of resonance r           for X-.
C*          (3,r,2) : (R*4) : m_(ij) + m_k  for r <--> (ij)
C*       Z(1-14)    : (R*8) : integration varialble in (0,1).
C* (Outputs)
C*       IPV(1,1) : (I*4) : a+
C*          (2,1) : (I*4) : b+
C*          (3,1) : (I*4) : c+
C*          (1,2) : (I*4) : a-
C*          (2,2) : (I*4) : b-
C*          (3,2) : (I*4) : c-
C*       XPH(1,1) : (R*4) : s.
C*          (2,1) : (R*4) : cos(theta_X-).
C*          (3,1) : (R*4) : phi_X-.
C*       XPH(1,2) : (R*4) : invariant mass squared for X+.
C*          (2,2) : (R*4) : cos(theta_a+) in X+ rest frame.
C*          (3,2) : (R*4) : phi_a+ in X+ rest frame.
C*       XPH(1,3) : (R*4) : invariant mass squared for a+b+.
C*          (2,3) : (R*4) : invariant mass squared for a+c+.
C*          (3,3) : (R*4) : phi_b+ in X+ rest frame.
C*       XPH(1,4) : (R*4) : invariant mass squared for X-.
C*          (2,4) : (R*4) : cos(theta_a-) in X- rest frame.
C*          (3,4) : (R*4) : phi_a- in X- rest frame.
C*       XPH(1,5) : (R*4) : invariant mass squared for a-b-.
C*          (2,5) : (R*4) : invariant mass squared for a-c-.
C*          (3,5) : (R*4) : phi_b- in X- rest frame..
C*       WAT      : (R*8) : Jacobian weight.
C* (Relation)
C*    Invokes UHQ2BW and UHQIJ3 in ttz_lib.
C* (Update Record)
C*   95/04/09           Restrict the Q3 and Q6 ranges to avoid
C*                      extremely off-shell charginos, since such
C*                      a case requires inclusions of diagrams
C*                      other than chargino pair productions.
C*   95/04/12           Switch to the zero-width approximation
C*                      when the width is below GMXMIN.
C*
CC**********************************************************************

      SUBROUTINE GETXPH(RS,AM,AMX,GMX,AMR,Z,IPV,XPH,WAT)

      IMPLICIT   REAL*8 ( A-H, O-Z )
C--
C  Dummy arguments.
C--
      INTEGER*4  IPV(3,2)
      REAL   *4  RS, AM(*), AMX(2), GMX(2), AMR(3,3,2), XPH(3,*)
      REAL   *8  Z(*), WAT
C--
C  Local variables.
C--
      INTEGER*4  JPV(3,3,3)
      REAL   *8  WT
      DATA EPS    / 5.D0  /
      DATA GMXMIN / 1.D-2 /
      DATA NCALL  /    0  /
C
C========< Entry Point >================================================
C
C--
C  Set some variables.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
C--
         xPI   = ACOS(-1.D0)
         x2PI  = 2*xPI
         ENGM  = 10
C--
         JPV(1,1,2) = 3
         JPV(2,1,2) = 4
         JPV(3,1,2) = 5
         JPV(1,1,3) = 4
         JPV(2,1,3) = 3
         JPV(3,1,3) = 5
         JPV(1,2,1) = 3
         JPV(2,2,1) = 5
         JPV(3,2,1) = 4
         JPV(1,2,3) = 5
         JPV(2,2,3) = 3
         JPV(3,2,3) = 4
         JPV(1,3,1) = 4
         JPV(2,3,1) = 5
         JPV(3,3,1) = 3
         JPV(1,3,2) = 5
         JPV(2,3,2) = 4
         JPV(3,3,2) = 3
      ENDIF
C--
C  Set independent variables.
C     Z( 1) : m(X+)**2
C      ( 2) : m(X-)**2
C      ( 3) : m(+ab)**2
C      ( 4) : m(+ac)**2
C      ( 5) : m(-ab)**2
C      ( 6) : m(-ac)**2
C      ( 7) : cos(theta_X-)
C      ( 8) : phi_X-
C      ( 9) : cos(theta_a)     in X+ rest frame
C      (10) : phi_a            in X+ rest frame
C      (11) : phi_b            in X+ rest frame
C      (12) : cos(theta_a)     in X- rest frame
C      (13) : phi_a            in X- rest frame
C      (14) : phi_b            in X- rest frame
C--
C  Reset event weight.
C--
      WAT = 1
C--
C  Set invariant masses squared for X+.
C--
      AMSX  = AMX(1)
      GAMX  = GMX(1)
C--
      IF ( GAMX.GT.GMXMIN ) THEN
         Q3MN  = AM(3) + AM(4) + AM(5)
         Q3MX  = RS - (AM(6)+AM(7)+AM(8))
         IF ( AMSX.GT.Q3MN .AND. AMSX.LT.Q3MX ) THEN
            Q3MN  = MAX(Q3MN,AMSX-ENGM*GAMX)
            Q3MX  = MIN(Q3MX,AMSX+ENGM*GAMX)
         ENDIF
         CALL UHQ2BW(AMSX,GAMX,Q3MN,Q3MX,Z(5),Q32,WT)
         Q3    = SQRT(Q32)
      ELSE
         Q32 = AMSX*AMSX
         Q3  = AMSX
         WT  = xPI*AMSX*GAMX
      ENDIF
      WAT   = WAT*WT
C--
C  And for X-.
C--
      AMSX  = AMX(2)
      GAMX  = GMX(2)
C--
      IF ( GAMX.GT.GMXMIN ) THEN
         Q6MN  = AM(6) + AM(7) + AM(8)
         Q6MX  = RS - Q3
         IF ( AMSX.GT.Q6MN .AND. AMSX.LT.Q6MX ) THEN
            Q6MN  = MAX(Q6MN,AMSX-ENGM*GAMX)
            Q6MX  = MIN(Q6MX,AMSX+ENGM*GAMX)
         ENDIF
         CALL UHQ2BW(AMSX,GAMX,Q6MN,Q6MX,Z(6),Q62,WT)
         Q6    = SQRT(Q62)
      ELSE
         Q62 = AMSX*AMSX
         Q6  = AMSX
         WT  = xPI*AMSX*GAMX
      ENDIF
      WAT   = WAT*WT
C--
C  Then set invariant masses squared
C  for daughter particles from X+.
C--
      CALL UHQIJ3(AM(1),JPV,AMR(1,1,1),EPS,Q3,Z(1),IPV(1,1),Q42,Q52,WT)
      WAT = WAT*WT
C--
C  And for daughter particles from X-.
C--
      CALL UHQIJ3(AM(4),JPV,AMR(1,1,2),EPS,Q6,Z(3),IPV(1,2),Q72,Q82,WT)
      WAT = WAT*WT
C--
      IPV(1,2) = IPV(1,2) + 3
      IPV(2,2) = IPV(2,2) + 3
      IPV(3,2) = IPV(3,2) + 3
C>>>
CCDBG       print *, ' IPV(*,1) = ', (IPV(K,1),K=1,3)
CCDBG       print *, '    (*,2) = ', (IPV(K,2),K=1,3)
C>>>
C--
C  Add angular variables.
C--
      XPH(1,1) = RS*RS
      XPH(2,1) = -1 + 2*Z(7)
      XPH(3,1) = x2PI*Z(8)
      XPH(1,2) = Q32
      XPH(2,2) = -1 + 2*Z(9)
      XPH(3,2) = x2PI*Z(10)
      XPH(1,3) = Q42
      XPH(2,3) = Q52
      XPH(3,3) = x2PI*Z(11)
      XPH(1,4) = Q62
      XPH(2,4) = -1 + 2*Z(12)
      XPH(3,4) = x2PI*Z(13)
      XPH(1,5) = Q72
      XPH(2,5) = Q82
      XPH(3,5) = x2PI*Z(14)
C>>>
CCDBG       PRINT *, ' Q32 = ', Q32, ' Q42 = ', Q42, ' Q52 = ', Q52
CCDBG       PRINT *, ' Q62 = ', Q62, ' Q72 = ', Q72, ' Q82 = ', Q82
C>>>
C--
C  Calculate Jacobian.
C--
      WAT = WAT*2**3*(x2PI)**5
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, HELASV1.
C
C **********************************************************************
C
      SUBROUTINE CDOTxx(V1,V2 , SCALAR)
C
C This subroutine computes the scalar product of two COMPLEX vectors.
C For the real vectors (momenta), use RDOTxx subroutine.
C
C INPUT:
C       complex V1(0:3)        : vector
C       complex V2(0:3)        : vector
C
C OUTPUT:
C       complex SCALAR         : scalar product of two vectors
C
      COMPLEX V1(0:3),V2(0:3),SCALAR
C
      SCALAR =  V1(0)*V2(0)-V1(1)*V2(1)
     &         -V1(2)*V2(2)-V1(3)*V2(3)
      RETURN
      END
C
C ======================================================================
C
      SUBROUTINE CONJGx(V , VCONJG)
C
C This subroutine computes the conjugation of a complex vector.
C
C INPUT:
C       complex V(0:3)         : complex vector
C
C OUTPUT:
C       complex VCONJG(0:3)    : conjugation V*
C
      COMPLEX V(0:3),VCONJG(0:3)
C
      VCONJG(0)=CONJG(V(0))
      VCONJG(1)=CONJG(V(1))
      VCONJG(2)=CONJG(V(2))
      VCONJG(3)=CONJG(V(3))
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE PMIRRx(P , PMIRR)
C
C This subroutine computes the real vector whose signs of the spacial
C components are reversed from the ones of the original vector.
C
C INPUT:
C       real    P(0:3)         : real vector
C
C OUTPUT:
C       real    PMIRR(0:3)     : mirror vector
C
      REAL    P(0:3),PMIRR(0:3)
C
      PMIRR(0)= P(0)
      PMIRR(1)=-P(1)
      PMIRR(2)=-P(2)
      PMIRR(3)=-P(3)
      RETURN
      END
      SUBROUTINE PSIGNx(P , PSIGN)
C
C This subroutine reverses the sign of a four-momentum.
C
C INPUT:
C       real    P(0:3)         : real vector
C
C OUTPUT:
C       real    PSIGN(0:3)     : reversed four-vector -P
C
      REAL    P(0:3),PSIGN(0:3)
C
      PSIGN(0)=-P(0)
      PSIGN(1)=-P(1)
      PSIGN(2)=-P(2)
      PSIGN(3)=-P(3)
      RETURN
      END
      SUBROUTINE PSUBxx(P1,P2 , PSUB)
C
C This subroutine computes the subtraction of two real vectors.
C
C INPUT:
C       real    P1(0:3)        : real vector
C       real    P2(0:3)        : real vector
C
C OUTPUT:
C       real    PSUB(0:3)      : subtraction P1-P2
C
      REAL    P1(0:3),P2(0:3),PSUB(0:3)
C
      PSUB(0)=P1(0)-P2(0)
      PSUB(1)=P1(1)-P2(1)
      PSUB(2)=P1(2)-P2(2)
      PSUB(3)=P1(3)-P2(3)
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE PSUMxx(P1,P2 , PSUM)
C
C This subroutine computes the sum of two real vectors.
C
C INPUT:
C       real    P1(0:3)        : real vector
C       real    P2(0:3)        : real vector
C
C OUTPUT:
C       real    PSUM(0:3)      : summation P1+P2
C
      REAL    P1(0:3),P2(0:3),PSUM(0:3)
C
      PSUM(0)=P1(0)+P2(0)
      PSUM(1)=P1(1)+P2(1)
      PSUM(2)=P1(2)+P2(2)
      PSUM(3)=P1(3)+P2(3)
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE RDOTxx(P1,P2 , SCALAR)
C
C This subroutine computes the scalar product of two REAL vectors.
C To get an inner-product of two momenta, call this subroutine.
C For the complex vectors, use CDOTxx subroutine.
C
C INPUT:
C       real    P1(0:3)        : vector
C       real    P2(0:3)        : vector
C
C OUTPUT:
C       real    SCALAR         : scalar product of two vectors
C
      REAL P1(0:3),P2(0:3)
C
      SCALAR =  P1(0)*P2(0)-P1(1)*P2(1)
     &         -P1(2)*P2(2)-P1(3)*P2(3)
      RETURN
      END
CDECK  ID>, MCEEVT.
      SUBROUTINE MCEEVT(IC)
C
C...generator steering routine
C
*
* Datacards
*
      INTEGER TRIG
      COMMON /STDATA/ TRIG
*
      REAL BEAM
      COMMON /STDATB/ BEAM
*
      INTEGER IPMC
      COMMON /MCPARA/ IPMC
*     
      PARAMETER (MAXUSR=6)
      REAL RUSR(MAXUSR)
      COMMON /MCUSER/ RUSR
*
       COMMON/MCONS1/DMZ
       COMMON/MCONS2/DMW
       COMMON/MCONS3/DMH
       COMMON/MCONS4/DMT
       COMMON/MCONS5/DALPE
       COMMON/MCONS6/DALPS
       COMMON/MCONS7/DSIN2THW
*
      REAL*4 SIGMARES,DSIGMARES
      COMMON /MYSIGM/ SIGMARES,DSIGMARES
C
      DOUBLE PRECISION QK(4),QIN(4),QOUT(4)
      DOUBLE PRECISION S,S1,EBEAM,EBEAM1,SIG,DINV
      INTEGER JOININDX(2)
C
      CHARACTER*100 SPNAME
C
      IF (IC.EQ.-2) THEN
        WRITE(*,*)
        WRITE(*,*) ' DFGT: Initialize constants, defaults.'
        WRITE(*,*)
*
* reset datacard values:
c        CALL VZERO(IPMC,MAXPMC)
        CALL VZERO(RUSR,MAXUSR)
C
C...load values of physical constants
        DMZ      = 91.188
        DMW      = 80.2
        DMH      = 300.0
        DMT      = 176.0
C        DALPE    = 1/128.
C        DALPS    = 0.12
C        DSIN2THW = 0.232
C
c	TRIG=10
c	BEAM=90.
C
C...load default settings:
C...initial state radiation on
        IPMC=1
*
	WRITE(*,*)
	WRITE(*,*) ' DFGT: Initialize Packages .........'
	WRITE(*,*)
        CALL DFGTMN(-2)
C
      ELSE IF (IC.EQ.-1) THEN
*
C...kinematics
        EBEAM=BEAM*1.0D0
        S=4.0D0*BEAM*BEAM
C
C... MC Parameter global initialaization.
C
	WRITE(*,*)
	WRITE(*,*) ' DFGT: Read datacards and Reset paramter ......'
	WRITE(*,*)
        
	CALL DFGT_PAR(0)
        write(6,*)' rusr',(rusr(i),i=1,6)
C... First we calculate the 3-body decay width of the Chargino_1 used as
C... input for the generator.
C
	WRITE(*,*)
	WRITE(*,*) ' DFGT: 3-body Width .................'
	WRITE(*,*)
        CALL DFGTW
C
C... Then start the integration step to build the probability distributions
C... used in the generation step.
C
	WRITE(*,*)
	WRITE(*,*) ' DFGT: Integration Step, and calc. X-section ...'
	WRITE(*,*)
        CALL DFGTMN(-1)
      ELSE IF (IC.EQ.0) THEN
C
C...Generate event
C
        CALL DFGTMN(0)
C
      ELSE IF (IC.EQ.1) THEN
        WRITE(*,*)
        WRITE(*,*) ' DFGT: End of EVENT generation'
        WRITE(*,*)
        CALL DFGTMN(1)
      ELSE
        WRITE(*,*)
        WRITE(*,*) ' DFGT: called with IC = ',IC
        WRITE(*,*)
      END IF
      RETURN
      END
CDECK  ID>, UHCUPL.
C***********************************************************************
C*
C*==============================----------------------------------==
C* Subroutine UHCUPL(ALPHA,SW2, GAL,GAU,GAD,GWF,GZN,GZL,GZU,GZD,G1,
C*                   GW,GWWZ,GWWA)
C*===================------------===================================
C*
C* (Ppurpose)
C*    This subroutine sets up the coupling constants in the SM.
C*    The array of couplings specifies the chirality of the INCOMING
C*    fermion. G?(1) denotes a left-handed coupling, and G?(2) a right-
C*    handed coupling.
C* (Inputs)
C*      ALPHA    : (R*4) : fine structure constant.
C*      SW2      : (R*4) : square of sine of Weinberg angle
C* (Outputs)
C*      GAL(2)   : (R*4) : coupling with A of charged leptons.
C*      GAU(2)   : (R*4) : coupling with A of up-type quarks.
C*      GAD(2)   : (R*4) : coupling with A of down-type quarks.
C*      GWF(2)   : (R*4) : coupling with W-,W+ of fermions.
C*      GZN(2)   : (R*4) : coupling with Z of neutrinos.
C*      GZL(2)   : (R*4) : coupling with Z of charged leptons.
C*      GZU(2)   : (R*4) : coupling with Z of up-type quarks.
C*      GZD(2)   : (R*4) : coupling with Z of down-type quarks.
C*      G1(2)    : (R*4) : unit coupling of fermions.
C*      GW       : (R*4) : weak coupling constant.
C*      GWWZ     : (R*4) : coupling of W-,W+,Z.
C*      GWWA     : (R*4) : coupling of W-,W+,A.
C* (Relation)
C*    Calls no subroutines.
C***********************************************************************

      SUBROUTINE UHCUPL(ALPHA,SW2, GAL,GAU,GAD,GWF,GZN,GZL,GZU,GZD,G1,
     .                  GW,GWWZ,GWWA)

      REAL*4    ALPHA,SW2,GW,GWWZ,GWWA
      REAL*4    GAL(2),GAU(2),GAD(2),GWF(2),GZN(2),GZL(2),
     .          GZU(2),GZD(2),G1(2)
C
C========< Entry Point >================================================
C
C--
C  Define some constants.
C--
      FOURPI = REAL(4.*3.14159265358979323846)
      E      = SQRT(ALPHA *FOURPI)
      SW     = SQRT(SW2)
      CW     = SQRT(1.-SW2)
      EZ     = E/(SW*CW)
C--
C  Calculate coupling constants involving fermions.
C--
      GAL(1) = E
      GAL(2) = E
      GAU(1) =-E*2./3.
      GAU(2) =-E*2./3.
      GAD(1) = E/3.
      GAD(2) = E/3.
      GWF(1) =-E/SQRT(2.*SW2)
      GWF(2) = 0.
      GZN(1) =-EZ*.5
      GZN(2) = 0.
      GZL(1) =-EZ*(-.5+SW2)
      GZL(2) =-EZ*SW2
      GZU(1) =-EZ*(.5-2./3.*SW2)
      GZU(2) = EZ*2./3.*SW2
      GZD(1) =-EZ*(-.5+1./3.*SW2)
      GZD(2) =-EZ/3.*SW2
      G1(1)  = 1.
      G1(2)  = 1.
C--
C  Calculate gauge boson self-coupling constants.
C--
      GW     = E/SW
      GWWZ   = E/SW*CW
      GWWA   = E
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, USVCKM.
C***********************************************************************
C*
C*============================---==
C* Subroutine USVCKM(MODE,I,J,VFF)
C*============================---==
C*
C* (Ppurpose)
C*    This subroutine allows users to put or get a CKM matrix element
C*    in or from /SMPTAB/ without directly referring to it.
C* (Inputs)
C*      MODE     : (R*4) : (1,2)=(get,put).
C*      I        : (I*4) : (1,2,3)=(u,c,t).
C*      J        : (I*4) : (1,2,3)=(d,s,b).
C* (Input/Output)
C*      VFF      : (R*4) : V_ij
C* (Relation)
C*    Calls no subroutines.
C*
C***********************************************************************

      SUBROUTINE USVCKM(MODE,I,J,VFF)

      INTEGER  *4   MODE, I, J
      REAL     *4   VFF
C--
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
C
C========< Entry Point >================================================
C
C--
C  Branch on MODE.
C--
      IF ( I.LT.1 .OR. I.GT.3 .OR.
     .     J.LT.1 .OR. J.GT.3 ) THEN
         PRINT *, ' >>>> ERROR in USVCKM >>>>'
         PRINT *, '   Invalid I or J: (I,J) = (', I, ',', J,')'
         PRINT *, '   STOP immediately. '
         STOP
      ELSE
         IF ( MODE.EQ.1 ) THEN
            VFF = VKM(I,J,2)
         ELSE
            VKM(I,J,2) = VFF
         ENDIF
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DFGT_PAR.
CC**********************************************************************
C*
C*=========================
C* Subroutine DFGT_PAR(IDE)
C*=========================
C*
C*   Read parameters for DFGT job.
C*
C*   Input: IDE = 0 (global          init.)
C*   Input: IDE = 1 (width      step init.)
C*   Input: IDE = 2 (generation step init.)
C* S.Giagu
CC**********************************************************************

      SUBROUTINE DFGT_PAR(IDE)
C
*
* Datacards
*
       INTEGER TRIG
       COMMON /STDATA/ TRIG
*
       REAL BEAM
       COMMON /STDATB/ BEAM
*
       INTEGER IPMC
       COMMON /MCPARA/ IPMC
*
       PARAMETER (MAXUSR=6)
       REAL RUSR(MAXUSR)
       COMMON /MCUSER/ RUSR
*
       COMMON/MCONS1/DMZ
       COMMON/MCONS2/DMW
       COMMON/MCONS3/DMH
       COMMON/MCONS4/DMT
       COMMON/MCONS5/DALPE
       COMMON/MCONS6/DALPS
       COMMON/MCONS7/DSIN2THW

      COMMON /BSITPRKIN/  NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X
      INTEGER*4    NCALW,NCALX, ITM1W,ITM1X, ITM2W,ITM2X,ITMX1,ITMX2
      
*
C
C* (Contents)
C*    SQRTS   : (R*4) : sqrt(s).
C*    POLEBM  : (R*4) : electron beam polarization.
C*    SGMEBM  : (R*4) : beam energy spread (fraction).
C*    ISRB    : (I*4) : ISR and BM flag.
C*

      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO
      INTEGER*4       ISRB
      COMMON /SSCONS/ AM0, AMU, AM2, TANB, AMA, RELAX
      REAL*4          AM0, AMU, AM2, TANB, AMA, RELAX
C* (Contents)
C*    ALFI  : (R*4) : 1/alpha(m_Z).
C*    ALFS  : (R*4) : alpha_s(m_Z).
C*    AMSW  : (R*4) : m_W (GeV).
C*    AMSZ  : (R*4) : m_Z (GeV).
C*    AMSH  : (R*4) : m_H (GeV).
C*    AMST  : (R*4) : m_t (GeV).
C*    AS2W  : (R*4) : sin2tw.
C*
      COMMON /USMPRM/  ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      REAL*4           ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      CHARACTER*128   BSINF, BSOUTF
      INTEGER*4 NGETBS, NUBS, NPUTBS, NDOSPR, NPRINF, NPRHST, LUINFO
      INTEGER*4 NBSINF, NOSPEV
      REAL*8 ESTIM, SIGMA, CTIME
      INTEGER*4 IT1, IT2, MXTRY

      COMMON /BS51PR/ ESTIM, SIGMA, CTIME, IT1, IT2, MXTRY,
     >        NGETBS, NUBS, NPUTBS, NDOSPR, NPRINF, NPRHST,
     >        LUINFO, NBSINF, NOSPEV,
     >        BSINF, BSOUTF

      COMMON /BS51HT/ LUHIST, BSHSTF
      INTEGER*4       LUHIST
      CHARACTER*128   BSHSTF
      COMMON /BSITPR/  NCAL, ITM1, ITM2
      INTEGER*4        NCAL, ITM1, ITM2

      COMMON /PLOTLU/ LU
      INTEGER*4       LU
C
C =====< Entry Point >==================================================
C
C
      IF ( IDE .EQ. 0 ) THEN
C
        WRITE(*,*) 'BASES_V5.1 - Change parameters for BASES V5.1 ',
     .          ' generator .'
C--
C  Default user parameters.
C--
        LUINFO = 6
        LU = LUINFO
C--
        AM0    = 70.
        AMU    = 400.
        AM2    = 250.
        TANB   = +2.
        AMA    = -9999.
        GAMSW1 = -999.
        GAMWINO = 0.
        RELAX   = 1.
C--
C  Set default beam parameters.
C--
        SQRTS  = 161.
        SGMEBM = 0.005
        POLEBM = 0.0
        ISRB   = 2
C--
        ALFI    = 1./DALPE
        ALFS    = DALPS
        AMSW    = DMW
        AMSZ    = DMZ
        AMSH    = DMH
        AMST    = DMT
	AS2W    = DSIN2THW
        AM0     = RUSR(1)
        AMU     = RUSR(2)
        AM2     = RUSR(3)
        TANB    = RUSR(4)
        AMA     = RUSR(5)
        RELAX   = RUSR(6)
        SQRTS   = BEAM*2.
        POLEBM  = +0.
        ISRB    = IPMC + 1
        NOSPEV  = TRIG
C
C --------------------------------------------
C  Print out the modified parameters.
C --------------------------------------------
C
900     CONTINUE
C--
C  If AMA is unspecified, set it to a default value.
C--
        IF ( AMA.EQ.-9999. ) THEN
         WRITE(*,*) ' AMA was not explicitly given.'
         WRITE(*,*) ' Going to use AMA = SQRT(AM0**2+AMU**2).'
         AMA    = SQRT(AM0**2+AMU**2)
        ENDIF
C--
C  User parameters.
C--
        WRITE(*,*)' '
        WRITE(*,*)'User Parameters'
        WRITE(*,*)' '
        WRITE(*,*)' ALFI=',ALFI,' ALFS=',ALFS
        WRITE(*,*)' AMSW=',AMSW,' AMSZ=',AMSZ,' AMSH=',AMSH
        WRITE(*,*)' AMST=',AMST,' AS2W=',AS2W
        WRITE(*,*)' '
        WRITE(*,*)' AM0=',AM0
        WRITE(*,*)' AMU=',AMU
        WRITE(*,*)' AM2=',AM2
        WRITE(*,*)' TNB=',TANB
        WRITE(*,*)' AMA=',AMA
        WRITE(*,*)' RELAX=',RELAX
        WRITE(*,*)' '
        WRITE(*,*)' ECM =',SQRTS
        WRITE(*,*)' POLE=',POLEBM
        WRITE(*,*)' SGEB=',SGMEBM
        WRITE(*,*)' ISRB=',ISRB
        WRITE(*,*)' '
      ELSEIF ( IDE .EQ. 1) THEN
        NCAL   = NCALW
        ITM1   = ITM1W
        ITM2   = ITM2W
      ELSEIF ( IDE .EQ. 2) THEN
        NCAL   = NCALX
        ITM1   = ITM1X
        ITM2   = ITM2X
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, GETXPHW.
CC**********************************************************************
C*
C*============================-----------===
C* Subroutine GETXPHW(AM,AMR,Z,IPV,XPH,WAT)
C*============================-----------===
C*
C* (Purpose)
C*    Generate phase space variables for a given set of random numbers
C*    of this iteration.
C* (Inputs)
C*       AM(i)    : (R*4) : mass  of i.
C*       AMR(1,r) : (R*4) : mass of resonance r
C*          (2,r) : (R*4) : width of resonance r
C*          (3,r) : (R*4) : m_(ij) + m_k  for r <--> (ij)
C*       Z(1-7)   : (R*8) : integration varialble in (0,1).
C* (Outputs)
C*       IPV(1)   : (I*4) : a+
C*          (2)   : (I*4) : b+
C*          (3)   : (I*4) : c+
C*       XPH(1,1) : (R*4) : invariant mass squared for X+.
C*          (2,1) : (R*4) : cos(theta_a+) in X+ rest frame.
C*          (3,1) : (R*4) : phi_a+ in X+ rest frame.
C*       XPH(1,2) : (R*4) : invariant mass squared for a+b+.
C*          (2,2) : (R*4) : invariant mass squared for a+c+.
C*          (3,2) : (R*4) : phi_b+ in X+ rest frame.
C*       WAT      : (R*8) : Jacobian weight.
C* (Relation)
C*    Invokes UHQ2BW and UHQIJ3 in ttz_lib.
C*
CC**********************************************************************

      SUBROUTINE GETXPHW(AM,AMR,Z,IPV,XPH,WAT)

      IMPLICIT   REAL*8 ( A-H, O-Z )
C--
C  Dummy arguments.
C--
      INTEGER*4  IPV(*)
      REAL   *4  AM(*), AMR(3,3), XPH(3,*)
      REAL   *8  Z(*), WAT
C--
C  Local variables.
C--
      INTEGER*4  JPV(3,3,3)
      REAL   *8  WT
      DATA EPS   / 5.D0 /
      DATA NCALL /    0 /
C
C========< Entry Point >================================================
C
C--
C  Set some variables.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
C--
         x2PI  = 2*ACOS(-1.D0)
C--
         JPV(1,1,2) = 2
         JPV(2,1,2) = 3
         JPV(3,1,2) = 4
         JPV(1,1,3) = 3
         JPV(2,1,3) = 2
         JPV(3,1,3) = 4
         JPV(1,2,1) = 2
         JPV(2,2,1) = 4
         JPV(3,2,1) = 3
         JPV(1,2,3) = 4
         JPV(2,2,3) = 2
         JPV(3,2,3) = 3
         JPV(1,3,1) = 3
         JPV(2,3,1) = 4
         JPV(3,3,1) = 2
         JPV(1,3,2) = 4
         JPV(2,3,2) = 3
         JPV(3,3,2) = 2
      ENDIF
C--
C  Set independent variables.
C     Z( 1) : m(+ab)**2
C      ( 2) : m(+ac)**2
C      ( 3) : cos(theta_a)     in X+ rest frame
C      ( 4) : phi_a            in X+ rest frame
C      ( 5) : phi_b            in X+ rest frame
C--
C  Reset event weight.
C--
      WAT = 1
C--
C  Set invariant masses squared for daughter particles from X+.
C--
      AMX = AM(1)
      CALL UHQIJ3(AM(1),JPV,AMR(1,1),EPS,AMX,Z(1),IPV(1),Q42,Q52,WT)
      WAT = WAT*WT
C>>>
CCDBG       print *, ' IPV(*) = ', (IPV(K),K=1,3)
C>>>
C--
C  Add angular variables.
C--
      XPH(1,1) = AMX*AMX
      XPH(2,1) = -1 + 2*Z(3)
      XPH(3,1) = x2PI*Z(4)
      XPH(1,2) = Q42
      XPH(2,2) = Q52
      XPH(3,2) = x2PI*Z(5)
C--
C  Calculate Jacobian.
C--
      WAT = WAT*2*(x2PI)**2
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, HGSMIX.
C----------------------------------------------------------------------C
      SUBROUTINE HGSMIX( SIN2W, ZM, WM, TANB, PSMAS,
     &			  HG0MAS, HGCMAS, TANA)
C----------------------------------------------------------------------C
C  purpose : give information about neutral/charged Higgs bosons
C    ( inputs & outputs are real*4 )
C  inputs  : SIN2W / weak mixing angle
C            WM    / W-boson mass
C            PSMAS / pseudoscalar mass
C            TANB  / ratio of vev. ( v2/v1 )
C  outputs : HG0MAS(i) i=1..2 / neutral Higgs mass (i=1:lighter )
C            HGCMAS           / charged Higgs mass( physical )
C            TANA   / tan of Higgs boson mixing angle
C  17/Jul/95  ZM is added as an input argment.   T.T
C----------------------------------------------------------------------C
      REAL*4 HG0MAS(2), HG0MS2(2)
C     DATA IIII/0/
C----------------------------------------------------------------------C
      COS2W = 1.-SIN2W
      COSW  = SQRT(COS2W)
CTT 17/Jul/95      ZM    = WM/COSW
      ZM2   = ZM**2
      PSMAS2= PSMAS**2

C     BETA  = DATAN2( DABS(DBLE(TANB)), DSIGN(1.D0,DBLE(TANB)) )
C     COSB  = COS(BETA)
C     SINB  = SIN(BETA)
C     COSDB = (COSB-SINB)*(COSB+SINB)
C     SINDB = 2.*SINB*COSB
C     SIN2B = SINB**2
C     COS2B = COSB**2
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSB  = SIGN( SQRT(COS2B), TANB )
      SIN2B = 1.-COS2B
      SINB  = SQRT(SIN2B)
      COSDB = (COSB-SINB)*(COSB+SINB)
      SINDB = 2.*SINB*COSB

      HGCMAS = SQRT( PSMAS2 + WM**2 )
      AMS    = PSMAS2 + ZM2
      DELTM  = SQRT( ((PSMAS+ZM)*(PSMAS-ZM))**2
     &                + 4.*PSMAS2 * ZM2 *SINDB**2 )

      HG0MS2(1) =  0.5*( AMS - DELTM )
      HG0MS2(2) =  0.5*( AMS + DELTM )
      DO 10 I=1,2
10    HG0MAS(I) = SQRT( HG0MS2(I) )

CCCC
C     IF( IIII.EQ.0 ) THEN
C     WRITE(6,*) 'COSB=',COSB
C     WRITE(6,*) 'SINB=',SINB
C     WRITE(6,*) 'ZM=',ZM
C     WRITE(6,*) 'PSMAS=',PSMAS
C     WRITE(6,*) 'DELTM=',DELTM
C     WRITE(6,*) 'H0MAS=',HG0MAS
C     WRITE(6,*) 'HCMAS=',HGCMAS
C     IIII=1
C     ENDIF
CCCC

      TANA = ( HG0MS2(1) -PSMAS2*COS2B -ZM2*SIN2B )
     &      / ( PSMAS2+ZM2 )/SINB/COSB
C     ALPHA = ATAN2( ABS(TANA), SIGN(1.,TANA) )
      ALPHA = DATAN2( DABS(DBLE(TANA)), DSIGN(1.D0,DBLE(TANA)) )


      RETURN
      END
CDECK  ID>, MODGAM.
      SUBROUTINE MODGAM(PNAME,I,GAM)

      IMPLICIT    REAL*4 ( A-H, O-Z )
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      CHARACTER*2 PNAME
      INTEGER  *4 I
      REAL     *4 GAM
C
C========< Entry Point >================================================
C
C--
C  Modify width.
C--
      IF ( PNAME.EQ.'SW' ) THEN
         IF ( I.GE.1 .AND. I.LE.2 ) THEN
            GMSW(I) = GAM
            PRINT *, ' MODGAM modified GMSW(',I,') to ', GAM
         ELSE
							GO TO 9999
         ENDIF
      ELSE IF ( PNAME.EQ.'SZ' ) THEN
         IF ( I.GE.1 .AND. I.LE.4 ) THEN
            GMSZ(I) = GAM
            PRINT *, ' MODGAM modified GMSZ(',I,') to ', GAM
         ELSE
							GO TO 9999
         ENDIF
      ELSE IF ( PNAME.EQ.'SF' ) THEN
         IF ( I.GE.1 .AND. I.LE.7 ) THEN
            GMSF(I) = GAM
            PRINT *, ' MODGAM modified GMSF(',I,') to ', GAM
         ELSE
							GO TO 9999
         ENDIF
      ELSE IF ( PNAME.EQ.'SH' ) THEN
         IF ( I.GE.1 .AND. I.LE.4 ) THEN
            GMSH(I) = GAM
            PRINT *, ' MODGAM modified GMSH(',I,') to ', GAM
         ELSE
							GO TO 9999
         ENDIF
      ELSE
							GO TO 9999
      ENDIF
C--
C  Normal end.
C--
      RETURN
C--
C  Error.
C--
9999  PRINT *, ' >>>> MODGAM: ERROR '
      PRINT *, '    PNAME = ', PNAME, ' I = ', I, ' is invalid.'
      STOP
      END
CDECK  ID>, UHPHS2.
CC**********************************************************************
C*
C*================================================------------==
C* Subroutine UHPHS2(MODE,Q,AM12,AM22,EB,CSTH,PHI,P1,P2,BTB,EA)
C*================================================------------==
C*
C* (Purpose)
C*    Calculates daughter 4-momenta (P1,P2) in the overall frame,
C*    when cos(theta) and phi (CSTH,PHI) of the first daughter in
C*    the helicity frame (EA) of the parent is given.
C*    EB is the helicity frame of the grandparent necessary to
C*    to specify azimuthal angle in the new frame (EA).
C*    Notice that the parent 4-momentum (Q) should be defined in
C*    the overall frame, since it will be used to define new Z
C*    axis.
C* (Inputs)
C*       MODE    : (I*4) : (1,2) = (no transf.,transf.)
C*       Q(0:3)  : (R*4) : parent 4-momentum.
C*       AM12    : (R*4) : mass**2 of 1-st daughter.
C*       AM22    : (R*4) : mass**2 of 2-nd daughter.
C*       EB(*,i) : (R*4) : old reference frame.
C*       CSTH    : (R*4) : cos(theta) of the 1-st daughter.
C*       PHI     : (R*4) : phi of the 1-st daughter.
C* (Outputs)
C*       P1(0:3) : (R*4) : 1-st daughter 4-mementum.
C*       P2(0:3) : (R*4) : 2-nd daughter 4-mementum.
C*       BTB     : (R*4) : beta_bar of the daughters in the parent
C*                       : rest frame.
C*       EA(*,i) : (R*4) : new reference frame.
C* (Relation)
C*    Calls RDOTxx, BOOSTx, UHSETF, and UBTRAN.
C*
CC**********************************************************************

      SUBROUTINE UHPHS2(MODE,Q,AM12,AM22,EB,CSTH,PHI,P1,P2,BTB,EA)

      IMPLICIT   REAL*4  ( A-H, O-Z )
      INTEGER*4  MODE
      REAL   *4  Q(0:3), AM12, AM22, EB(3,3), CSTH, PHI,
     .           P1(0:3), P2(0:3), BTB, EA(3,3)
C--
      REAL   *8  BETA2, X1, X2, AQ, AMQ, AMQ2, AP1
C--
C  Statement function.
C--
      BETA2(X1,X2) =  1 - 2*(X1+X2) + (X1-X2)**2
C
C========< Entry Point >================================================
C
C--
C  Calculate daughter 4-momenta in the parent helicity frame.
C--
      AQ   = SQRT(Q(1)**2+Q(2)**2+Q(3)**2)
      AMQ2 = (Q(0)-AQ)*(Q(0)+AQ)
      IF ( AMQ2.LE.0.D0 ) THEN
         PRINT *, ' >>>> Error in UHPHS2 >>>> '
         PRINT *, '   Q    = ', Q
         PRINT *, '   AMQ2 = ', AMQ2, ' AM12 = ', AM12, ' AM22 = ', AM22
         BTB = 0
         RETURN
      ENDIF
C--
      AMQ   = SQRT(AMQ2)
      BTB   = BETA2(AM12/AMQ2,AM22/AMQ2)
      IF ( BTB.LE.0. ) THEN
C        PRINT *, ' >>>> Error in UHPHS2 >>>> '
C        PRINT *, '   Q    = ', Q
C        PRINT *, '   AMQ2 = ', AMQ2, ' AM12 = ', AM12, ' AM22 = ', AM22
         BTB = 0
         RETURN
      ENDIF
C--
      BTB   = SQRT(BTB)
      AP1   = (AMQ/2)*BTB
      SNTH  = SQRT((1-CSTH)*(1+CSTH))
      P1(0) = SQRT(AP1*AP1+AM12)
      P1(1) = AP1*SNTH*COS(PHI)
      P1(2) = AP1*SNTH*SIN(PHI)
      P1(3) = AP1*CSTH
C--
      P2(0) = AMQ - P1(0)
      P2(1) =     - P1(1)
      P2(2) =     - P1(2)
      P2(3) =     - P1(3)
C--
C  Transform the daughter 4-momenta to the frame in which the parent
C  4-momentum (Q) is defined.
C--
      IF ( MODE.EQ.1 ) THEN
         CALL UVCOPY(9,EB,EA)
      ELSE
         CALL UHSETF(Q(1),EB,EA)
         CALL UBTRAN(P1(1),EA,P1(1))
         CALL UBTRAN(P2(1),EA,P2(1))
         CALL BOOSTx(P1,Q,P1)
         CALL BOOSTx(P2,Q,P2)
      ENDIF
C--
      IF ( P1(0).LE.0. .OR. P2(0).LE.0. ) THEN
         BTB = 0
      ELSE
         AP = SQRT(P1(1)**2+P1(2)**2+P1(3)**2)
         IF ( P1(0).LT.AP ) P1(0) = SQRT(AP*AP+AM12)
         AP = SQRT(P2(1)**2+P2(2)**2+P2(3)**2)
         IF ( P2(0).LT.AP ) P2(0) = SQRT(AP*AP+AM22)
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, XCMASS.
      SUBROUTINE XCMASS(AM2,AMU,BT,AMW,AMXC,PHIL,PHIR,EPSR)

      IMPLICIT   REAL*8 ( A-H, O-Z )
      REAL*8     AM2, AMU, BT, AMW, AMXC(2), PHIL, PHIR, EPSR
      REAL*8     AMSMAT(2,2), UL(2,2), UR(2,2)
      DATA NCALL / 0 /
C
C========< Entry Point >================================================
C
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         PI    = ACOS(-1.)
         SQ2   = SQRT(2.)
      ENDIF
C--
C  Calculate mass matrix.
C--
      CSB  = COS(BT)
      SNB  = SIN(BT)
      SN2B = 2*SNB*CSB
      CS2B = (CSB-SNB)*(CSB+SNB)
C--
      AMSMAT(1,1) = AM2
      AMSMAT(1,2) = SQ2*AMW*CSB
      AMSMAT(2,1) = SQ2*AMW*SNB
      AMSMAT(2,2) = AMU
C--
C  Calculate chargino masses.
C--
      SQDET = AM2*AMU - AMW*AMW*SN2B
      B     = AM2*AM2 + AMU*AMU + 2*AMW*AMW
      D     = (B-2*SQDET)*(B+2*SQDET)
C>>>
      IF ( D.LT.0.D0 ) THEN
         RETURN
      ENDIF
C>>>
      SQD   = SQRT( D )
      AMX1  = SQRT( ( B - SQD )/2 )
      AMX2  = SQRT( ( B + SQD )/2 )
      AMXC(1) = AMX1
      AMXC(2) = AMX2
C--
C  Calculate mixing matrix.
C--
      PHIL  = ATAN( ( (AMX1-AM2)*(AMX1+AM2)-AMW*AMW*(1-CS2B) )
     .             /( SQ2*AMW*(AM2*CSB+AMU*SNB) ) )
      PHIL  = MOD(PHIL+PI,PI)
      PHIR  = ATAN( ( (AMX1-AM2)*(AMX1+AM2)-AMW*AMW*(1+CS2B) )
     .             /( SQ2*AMW*(AM2*SNB+AMU*CSB) ) )
      PHIR  = MOD(PHIR+PI,PI)
C--
      UL(1,1) = COS(PHIL)
      UL(1,2) = SIN(PHIL)
      UL(2,1) = -UL(1,2)
      UL(2,2) = UL(1,1)
C--
      UR(1,1) = COS(PHIR)
      UR(1,2) = SIN(PHIR)
      UR(2,1) = -UR(1,2)
      UR(2,2) = UR(1,1)
C--
      AMX1 = 0
      DO 100 J = 1, 2
         DO 10 I = 1, 2
            AMX1 = AMX1 + UR(1,I)*AMSMAT(I,J)*UL(1,J)
10       CONTINUE
100   CONTINUE
      IF ( AMX1.LT.0.D0 ) PHIR = PHIR + PI
C--
C  Decide EPSR.
C--
      EPSR = SIGN(1.D0,AM2*AMU-AMW*AMW*SN2B)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DFGTBDI.
CC**********************************************************************
C*
C*=========================
C* Subroutine DFGTBDI.
C*=========================
C*
C*   Initialize DFGT COMMONs.
C*
CC**********************************************************************

      SUBROUTINE DFGTBDI
C
      COMMON /BEMCNS/ SGEBM
      REAL*4          SGEBM
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
C* (Contents)
C*    ALFI  : (R*4) : 1/alpha(m_Z).
C*    ALFS  : (R*4) : alpha_s(m_Z).
C*    AMSW  : (R*4) : m_W (GeV).
C*    AMSZ  : (R*4) : m_Z (GeV).
C*    AMSH  : (R*4) : m_H (GeV).
C*    AMST  : (R*4) : m_t (GeV).
C*    AS2W  : (R*4) : sin2tw.
C*
      COMMON /USMPRM/  ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      REAL*4           ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
C
      SGEBM = 0.007
C
      AMF(1,1,1) = 1.E-10
      AMF(2,1,1) = 1.E-10
      AMF(3,1,1) = 1.E-10
      AMF(1,2,1) = 0.5110034E-3
      AMF(2,2,1) = 105.65946E-3
      AMF(3,2,1) = 1.732E0
      AMF(1,1,2) = 0.04E0
      AMF(2,1,2) = 1.5E0
      AMF(3,1,2) = 135.0E0
      AMF(1,2,2) = 0.04E0
      AMF(2,2,2) = 0.1E0
      AMF(3,2,2) = 4.7E0
C
      GMF(1,1,1) = 1.E-10
      GMF(2,1,1) = 1.E-10
      GMF(3,1,1) = 1.E-10
      GMF(1,2,1) = 1.E-10
      GMF(2,2,1) = 1.E-10
      GMF(3,2,1) = 1.E-10
      GMF(1,1,2) = 1.E-10
      GMF(2,1,2) = 1.E-10
      GMF(3,1,2) = 1.E-10
      GMF(1,2,2) = 1.E-10
      GMF(2,2,2) = 1.E-10
      GMF(3,2,2) = 1.E-10
C
      AMZ = 91.17E0
      AMW = 80.0E0
      AMH = 300.0E0
      GMZTOT = 2.3541E0
      GMWTOT = 1.9127E0
      GMHTOT = 1.0000E0
C
      VKM(1,1,1) = 1.000E0
      VKM(2,1,1) = 0.000E0
      VKM(3,1,1) = 0.000E0
      VKM(1,2,1) = 0.000E0
      VKM(2,2,1) = 1.000E0
      VKM(3,2,1) = 0.000E0
      VKM(1,3,1) = 0.000E0
      VKM(2,3,1) = 0.000E0
      VKM(3,3,1) = 1.000E0
      VKM(1,1,2) = 0.975E0
      VKM(2,1,2) = 0.222E0
      VKM(3,1,2) = 0.010E0
      VKM(1,2,2) = 0.222E0
      VKM(2,2,2) = 0.974E0
      VKM(3,2,2) = 0.043E0
      VKM(1,3,2) = 0.010E0
      VKM(2,3,2) = 0.043E0
      VKM(3,3,2) = 0.999E0
C
      MDVDK(1,1,1) = 1
      MDVDK(2,1,1) = 1
      MDVDK(3,1,1) = 1

      MDVDK(1,2,1) = 2
      MDVDK(2,2,1) = 2
      MDVDK(3,2,1) = 1

      MDVDK(1,3,1) = 3
      MDVDK(2,3,1) = 3
      MDVDK(3,3,1) = 1

      MDVDK(1,4,1) = 1
      MDVDK(2,4,1) = 1
      MDVDK(3,4,1) = 2

      MDVDK(1,5,1) = 2
      MDVDK(2,5,1) = 1
      MDVDK(3,5,1) = 2

      MDVDK(1,6,1) = 3
      MDVDK(2,6,1) = 1
      MDVDK(3,6,1) = 2

      MDVDK(1,7,1) = 1
      MDVDK(2,7,1) = 2
      MDVDK(3,7,1) = 2

      MDVDK(1,8,1) = 2
      MDVDK(2,8,1) = 2
      MDVDK(3,8,1) = 2

      MDVDK(1,9,1) = 3
      MDVDK(2,9,1) = 2
      MDVDK(3,9,1) = 2

      MDVDK(1,10,1) = 1
      MDVDK(2,10,1) = 3
      MDVDK(3,10,1) = 2

      MDVDK(1,11,1) = 2
      MDVDK(2,11,1) = 3
      MDVDK(3,11,1) = 2

      MDVDK(1,12,1) = 3
      MDVDK(2,12,1) = 3
      MDVDK(3,12,1) = 2

      MDVDK(1,1,2) = 1
      MDVDK(2,1,2) = 1
      MDVDK(3,1,2) = 1

      MDVDK(1,2,2) = 2
      MDVDK(2,2,2) = 1
      MDVDK(3,2,2) = 1

      MDVDK(1,3,2) = 3
      MDVDK(2,3,2) = 1
      MDVDK(3,3,2) = 1

      MDVDK(1,4,2) = 1
      MDVDK(2,4,2) = 2
      MDVDK(3,4,2) = 1

      MDVDK(1,5,2) = 2
      MDVDK(2,5,2) = 2
      MDVDK(3,5,2) = 1

      MDVDK(1,6,2) = 3
      MDVDK(2,6,2) = 2
      MDVDK(3,6,2) = 1

      MDVDK(1,7,2) = 1
      MDVDK(2,7,2) = 1
      MDVDK(3,7,2) = 2

      MDVDK(1,8,2) = 2
      MDVDK(2,8,2) = 1
      MDVDK(3,8,2) = 2

      MDVDK(1,9,2) = 3
      MDVDK(2,9,2) = 1
      MDVDK(3,9,2) = 2

      MDVDK(1,10,2) = 1
      MDVDK(2,10,2) = 2
      MDVDK(3,10,2) = 2

      MDVDK(1,11,2) = 2
      MDVDK(2,11,2) = 2
      MDVDK(3,11,2) = 2

      MDVDK(1,12,2) = 3
      MDVDK(2,12,2) = 2
      MDVDK(3,12,2) = 2
C
      BRVDK(0,1) = 0.
      BRVDK(1,1) = 0.
      BRVDK(2,1) = 0.
      BRVDK(3,1) = 0.
      BRVDK(4,1) = 0.
      BRVDK(5,1) = 0.
      BRVDK(6,1) = 0.
      BRVDK(7,1) = 0.
      BRVDK(8,1) = 0.
      BRVDK(9,1) = 0.
      BRVDK(10,1) = 0.
      BRVDK(11,1) = 0.
      BRVDK(12,1) = 0.
      BRVDK(0,2) = 0.
      BRVDK(1,2) = 0.
      BRVDK(2,2) = 0.
      BRVDK(3,2) = 0.
      BRVDK(4,2) = 0.
      BRVDK(5,2) = 0.
      BRVDK(6,2) = 0.
      BRVDK(7,2) = 0.
      BRVDK(8,2) = 0.
      BRVDK(9,2) = 0.
      BRVDK(10,2) = 0.
      BRVDK(11,2) = 0.
      BRVDK(12,2) = 0.
C
      ALFI = 128.0
      ALFS = 0.120
      AMSW = 80.00
      AMSZ = 91.17
      AMSH = 300.
      AMST = 170.
      AS2W = 0.232
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, GMCXDF.
CC**********************************************************************
C*
C*=====================================-----===
C* Subroutine GMCXDF(IDP,IHEL,AM,IPV,X,SG,PV)
C*=====================================-----===
C*
C* (Purpose)
C*    Calculate decay width for X+ ---> X0 fu fdb.
C* (Inputs)
C*       IDP(1,i) : (I*4) : generation number.
C*          (2,i) : (I*4) : T3 + 1.5.
C*          (3,i) : (I*4) : l/q flag; (1,2) = (l,q).
C*       IHEL( i) : (I*4) : helicity combination.
C*                        : i = (1,2,3,4)
C*                        :   = (X+,X0,fu,fdb)
C*                        :         <-- X+ -->
C*       AM(i)    : (R*4) : mass of i.
C*       IPV(1)   : (I*4) : a+
C*          (2)   : (I*4) : b+
C*          (3)   : (I*4) : c+
C*       X(1,1)   : (R*4) : m_X+^2.
C*        (2,1)   : (R*4) : cos(theta_a+).
C*        (3,1)   : (R*4) : phi_a+.
C*       X(1,2)   : (R*4) : invariant mass squared for a+b+.
C*        (2,2)   : (R*4) : invariant mass squared for a+c+.
C*        (3,2)   : (R*4) : phi_b+.
C* (Output)
C*       PV(*,i)  : (R*4) : 4-momentum of i-th particle, where
C*                        : numbering convention is that of IDP.
C*       SG       : (R*4) : d(sigma)/dX1dX2....dXn.
C* (Relation)
C*    Invokes UHSETF, UBTRAN, FULCXD, and library routines in FORTLIB.
C*
CC**********************************************************************

      SUBROUTINE GMCXDF(IDP,IHEL,AM,IPV,X,SG,PV)

      IMPLICIT     REAL*4  ( A-H, O-Z )
      INTEGER*4    IDP(3,*), IHEL(*), IPV(3)
      REAL   *4    AM(*), X(3,*), SG, PV(0:3,*)
      REAL   *4    QV(0:3,4), AMP2(0:3)
C
C========< Entry Point >================================================
C
C--
C  Set 4-momenta.
C--
      CALL UVZERO(16,PV)
      CALL UVZERO(16,QV)
C--
      WAT = 1
C
C-- X+.
C
      AMX2    = X(1,1)
      AMX     = SQRT(AMX2)
      PV(0,1) = AMX
      QV(0,1) = AMX
C
C-- X+ --> a + b + c
C
      AM1 = AM(IPV(1))
      AM2 = AM(IPV(2))
      AM3 = AM(IPV(3))
      Q12 = SQRT(X(1,2))
      Q13 = SQRT(X(2,2))
C>>>
CCDBG       PRINT *, ' P_X+ = ', (QV(K,1),K=0,3)
CCDBG       PRINT *, ' AM1, AM2, AM3 = ', AM1, AM2, AM3
CCDBG       PRINT *, ' CS1, FI1, FI2 = ', X(2,1), X(3,1), X(3,2)
CCDBG       PRINT *, ' Q12 = ', Q12, ' Q13 = ', Q13
C>>>
      CALL UHPHS3W(QV(0,1),AM1,AM2,AM3,X(2,1),
     .            X(3,1),X(3,2),Q12,Q13,WT,QV(0,2))
      IF ( WT.EQ.0. )                            GO TO 9999
      WAT = WAT*WT
C--
      CALL UVCOPY(4,QV(0,2),PV(0,IPV(1)))
      CALL UVCOPY(4,QV(0,3),PV(0,IPV(2)))
      CALL UVCOPY(4,QV(0,4),PV(0,IPV(3)))
C--
C  Calculate phase space weight.
C--
      WAT = WAT/(2*AMX)
C--
C  Calculate amplitude squared.
C--
C>>>
CCDBG        PRINT *, ' PV_2 = ', (PV(K,2),K=0,3)
CCDBG        PRINT *, ' PV_3 = ', (PV(K,3),K=0,3)
CCDBG        PRINT *, ' PV_4 = ', (PV(K,4),K=0,3)
CCDBG        PRINT *, ' IDP  = ', ((IDP(K,L),K=1,3),L=1,4)
CCDBG        PRINT *, ' IHEL = ', (IHEL(K),K=1,4)
C>>>
      CALL FULCXD(IDP,IHEL,PV,AMP2)
C>>>
CCDBG        PRINT *, ' WAT = ', WAT, ' AMP2 = ', AMP2
C>>>
C--
C  Differenctial width.
C--
      SG  = AMP2(0)*WAT
C--
C  That's it.
C--
      RETURN
C--
C  Kinematically forbidden.
C--
9999  SG = 0
      RETURN
      END
CDECK  ID>, HIOKI.
C   WM CALCULATED FROM ZM AND GF, BY Z.HIOKI    30TH NOV. 1988
C   NEWEST VERSION
C-----------------------------------------------------------------------
C           MW5.FORT77
C
C  1-L00P CORRECTION  ( W-BOSON MASS SHIFT )
C
C ** CALCULATION OF DELTA-R **
C  MAIN PROGRAM
CRUN       IMPLICIT REAL*8(A-H,O-Z)
CRUN       COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
CRUN      *     UM, DM, CM, SM, TM, BM, PM, DELWM,
CRUN      #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM,
CRUN       COMMON/ZHCMWN/F1,F2,F3,F4
CRUN       DIMENSION J(2),WMASS(5)
CRUN C  HIGGS MASS
CRUN       DATA PM0/ 100./
CRUN C----
CRUN C     DO 1000 KKK=1,1,1
CRUN C     DO 2000 LLL=1,1,1
CRUN C     SSQZM=90D0+DFLOAT(KKK-1)
CRUN C     TM0=30D0+30D0*DFLOAT(LLL-1)
CRUN       SSQZM = 92.
CRUN       TM0   = 180.
CRUN       PM0   = 1000.
CRUN       CALL ZHWMAS( DR, WMASS, SSQZM, TM0, PM0)
CRUN       CALL DATE(J)
CRUN       FLM0 = SQRT( FLM )
CRUN       FUM0 = SQRT( FUM )
CRUN       FDM0 = SQRT( FDM )
CRUN       WRITE(6,1) J, F1, F2, F3, F4, TM0, PM0
CRUN      *,         FLM0, FUM0, FDM0, SSQZM,  SQWM, WMASS, DR
CRUN 1     FORMAT(1H //10X,2A4//2X,7HFERMI1=F3.0,2X,7HFERMI2=F3.0,2X,7HF
CRUN      #=F3.0,2X,7HFERMI4=F3.0
CRUN      #//2X,3HMT=F7.2,2X,7HMHIGGS=F7.2,2X,4HFLM=F7.2,2X,4HFUM=F7.2,2
CRUN      #DM=F7.2//3X,3HMZ=F8.4,3X,4HMW0=F8.4///2X,3HMW=F9.4,3X,4HMW2=F
CRUN      #X,4HMW3=F9.4,3X,4HMW4=F9.4,3X,'MW5=',F9.4,//5X,3HDR=,D15.7/)
CRUN 2000  CONTINUE
CRUN 1000  CONTINUE
CRUN       STOP
CRUN       END
CRUN
CRUN       IMPLICIT REAL*8(A-H,O-Z)
CRUN       DIMENSION WMASS(5)
CRUN 1     CONTINUE
CRUN       PRINT *,'Enter Z,Top,Higgs masses'
CRUN       READ(5,*) SSQZM,TM0,PM0
CRUN       IF(SSQZM.LE.0.) GO TO 9
CRUN       CALL ZHWMAS( DR, WMASS, SSQZM,  TM0,  PM0)
CRUN       PRINT *,' DR =',DR,' for Mw,Mz,Mt,Mh=',WMASS(4),SSQZM,TM0,PM0
CRUN       PRINT *,' SIN2W=',1.-( WMASS(4)/SSQZM )**2
CRUN       GO TO 1
CRUN 9     CONTINUE
CRUN       STOP
CRUN       END
CRUN
      SUBROUTINE ZHWMAS( DR, WMASS, SSQZM, TM0, PM0)
C**********************************************************************
C*  INPUT:
C*        SSQZM  : Z MASS
C*        TM0    : TOP QUARK MASS
C*        PM0    : HIGGS MASS
C*  OUTPUT:
C*        DR     : RADIATIVE CORECTION TO W MASS
C*        WMASS(1):
C*        WMASS(2):
C*        WMASS(3):
C*        WMASS(4): Like as Sirlin.
C*        WMASS(5): No corection
C*  NOTE:
C*        THIS SUBROUTINE REQUIRES BLOCK DATA ZHPRM.
C*            WRITTEN  BY Z. HIOKI,  ( MODIFIED BY T.TAUCHI )
C**********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
     *     UM, DM, CM, SM, TM, BM, PM, DELWM,
     #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM, FDM
      COMMON/ZHCMWN/F1,F2,F3,F4
      DIMENSION WMASS(5)
      EXTERNAL ACC
C-- CONSTANTS
      DATA GF/ 1.16632D-5/, WWW/ 37.281311D0/
C
      SQZM= SSQZM
      ZM  = SQZM**2
      WM  = SQZM*(  SQZM + DSQRT( SQZM**2 - 4D0*WWW**2 )  ) / 2D0
      SQWM= DSQRT( WM )
      TM  = TM0**2
      PM  = PM0**2
      SN  = 5.62D-2*F1*F2
      CS  = 1D0-SN
      WMASS(5) = SQWM
C  DEF OF ZN
      ZN=ZM/64D0/WM/(ZM-WM)*((ZM+2D0*WM)/2D0+ZM*DLOG(ZM)+2D0*WM*DLOG(WM)
     #)
C  DEF OF ZL
      ZL=ZM*(5D-1+DLOG(WM))/32D0
     #/(ZM-WM)+(ZM-2D0*WM)**2*(5D-1+DLOG(ZM))/64D0/WM/(ZM-WM)
C  DEF OF ZLM ( ZL OF MUON )
      ZLM=ZM*(5D-1+DLOG(WM))/32D0
     #/(ZM-WM)+(ZM-2D0*WM)**2*(5D-1+DLOG(ZM))/64D0/WM/(ZM-WM)
C  DEF OF DELZMF
      DELZMF=ZM*ZM*(ZM*(2D0*(F1+F2+F3+F4)*E(0D0,0D0,ZM)
     #+(1D0+(4D0*WM-3D0*ZM
     #)**2/ZM/ZM)*(E(EM,EM,ZM)*F1+E(XM,XM,ZM)*F2+E(TAM,TAM,ZM)*F3
     #+E(FLM,FLM,ZM)*F4)+3D0*(
     #1D0+(8D0*WM-5D0*ZM)**2/9D0/ZM/ZM)*(E(UM,UM,ZM)*F1+E(CM,CM,ZM)*F2+
     #E(TM,TM,ZM)*F3+E(FUM,FUM,ZM)*F4)
     #+3D0*(1D0+(4D0*WM-ZM)**2/9D0/ZM/ZM)*(E(DM,DM,ZM)*F1
     #+E(SM,SM,ZM)*F2+E(BM,BM,ZM)*F3+E(FDM,FDM,ZM)*F4))
     #-EM*E0(EM,EM,ZM)*F1-XM*E0(XM,XM,ZM)*F2-
     #TAM*E0(TAM,TAM,ZM)*F3-
     #FLM*E0(FLM,FLM,ZM)*F4-3D0*(UM*E0(UM,UM,ZM)*F1+DM*E0(DM,DM,ZM)*F1+
     #CM*E0(CM,CM,ZM)*F2+SM*E0(SM,SM,ZM)*F2+TM*E0(TM,TM,ZM)*F3+BM*E0(BM,
     #BM,ZM)*F3+FUM*E0(FUM,FUM,ZM)*F4+FDM*E0(FDM,FDM,ZM)*F4))
C  DEF OF DELZM
      DELZM=-1D0/32D0/WM/(ZM-WM)*(-ZM*(ZM*ZM-2D0*ZM*WM+4D0*WM*WM)/3D0+
     #ZM*ZM*ZM*DLOG(ZM)/2D0+WM*(ZM*ZM-4D0*ZM*WM+16D0*WM*WM)*DLOG(WM)+
     #DELZMF+ZM*(ZM*ZM-4D0*ZM*WM+24D0*WM*WM)*E(WM,WM,ZM)+
     #WM*(3D0*ZM*ZM-14D0*ZM*WM-16D0*WM*WM)*E0(WM,WM,ZM)+ZM*ZM*ZM*(2D0*E0
     #(PM,ZM,ZM)-E2(PM,ZM,ZM))-PM*ZM*ZM*(E1(ZM,PM,ZM)-DLOG(PM)/2D0))
C  DEF OF DELWMF
      DELWMF=2D0*WM*(E(0D0,EM,WM)*F1+E(0D0,XM,WM)*F2+E(0D0,TAM,WM)*F3
     #+E(0D0,FLM,WM)*F4)-
     #EM*E1(0D0,EM,WM)*F1-XM*E1(0D0,XM,WM)*F2-TAM*E1(0D0,TAM,WM)*F3
     #-FLM*E1(0D0,FLM,WM)*F4+3
     #D0*CS*(2D0*WM*E(UM,DM,WM)*F1-DM*E1(UM,DM,WM)*F1-UM*E1(DM,UM,WM)*F1
     #)+3D0*SN*(2D0*
     #WM*E(UM,SM,WM)-SM*E1(UM,SM,WM)-UM*E1(SM,UM,WM))+3D0*CS*(2D0*WM*
     #E(CM,SM,WM)*F2-SM*E1(CM,SM,WM)*F2-CM*E1(SM,CM,WM)*F2)+
     #3D0*SN*(2D0*WM*E(CM,DM,WM)-DM*E1(CM,DM,WM)-CM*E1(DM,CM,WM))+3D0*(2
     #D0*WM*E(TM,BM,WM)*F3-BM*E1(TM,BM,WM)*F3-TM*E1(BM,TM,WM)*F3)
     #+3D0*(2D0*WM*E(FUM,FDM,WM)*F4-FDM*E1(FUM,FDM,WM)*F4
     #-FUM*E1(FDM,FUM,WM)*F4)
C  DEF OF DELWM
      DELWM=-1D0/32D0/(ZM-WM)*(-WM*ZM+7D0*WM*ZM*DLOG(WM)+ZM*(6D0*WM+ZM/2
     #D0)*DLOG(ZM)+2D0*ZM*DELWMF+(ZM*ZM-20D0*ZM*WM-8D0*WM*WM)*E0(ZM,WM,W
     #M)+(ZM*ZM+16D0*ZM*WM+4D0*WM*WM)*E1(ZM,WM,WM)-WM*(ZM+20D0*WM)*E2(ZM
     #,WM,WM)+4D0*WM*(ZM-WM)*(-2D0*E0(0D0,WM,WM)+E1(0D0,WM,WM)-5D0*E2(0D
     #0,WM,WM))+PM*ZM*(DLOG(PM)/2D0-E1(WM,PM,WM))+ZM*WM*(2D0*E0(PM,WM,WM
     #)-E2(PM,WM,WM)))
C  DEF OF Y
      Y=-1D0/48D0+7D0*DLOG(WM)/32D0
     #-F1*(DLOG(WM)+DLOG(WM)/3D0+4D0*DLOG(WM)/3D0)/24D0
     #-F2*(DLOG(WM)+DLOG(WM)/3D0+4D0*DLOG(WM)/3D0)/24D0
     #-F3*(DLOG(WM)+DLOG(WM)/3D0+4D0*DLOG(WM)/3D0)/24D0
     #-F4*(DLOG(FLM)+DLOG(FDM)/3D0+4D0*DLOG(FUM)/3D0)/24D0
C  RUNNING COUPLING-CONSTANT
      SUM=F1*(DLOG(EM/WM)+DLOG(DM/WM)/3D0+4D0*DLOG(UM/WM)/3D0)
     #+F2*(DLOG(XM/WM)+DLOG(SM/WM)/3D0+4D0*DLOG(CM/WM)/3D0)
     #+F3*(DLOG(TAM/WM)+DLOG(BM/WM)/3D0)
      IF(TM.GT.WM) GO TO 10
      SUM=SUM+4D0/3D0*DLOG(TM/WM)*F3
10    ALPW=ALP/(1D0+ALP*(SUM+SUM)/6D0/PAI)
C
C  DEF OF VF0
      VF0=(ZM*DLOG(ZM)-WM*DLOG(WM))/(ZM-WM)-1D0
C  DEF OF VC2
      VC2=5D0-6D0*DLOG(WM)-2D0*WM/(ZM-WM)*(5D-1+3D0*VF0)
     #-ZM*(ZM-2D0*WM)/2D0/WM/(ZM-WM)*(5D-1+DLOG(ZM))
     #+16D0*(DELZM/ZM-(DELZM-DELWM)/(ZM-WM)+2D0*Y+ZL+ZN)
C  DEF OF VC3
      VC3=VC2-16D0*(ZL-ZLM)
C
C  ---------  START OF CALCULATION  ----------
C
      AT=-ALP*PAI*ZM/2D0/WM/(ZM-WM)
      DR=ACC(0D0)/AT
      AAA=DSQRT(PAI*ALPW/DSQRT(2D0)/GF/(1D0-DR))
      WMASS(1)=SQZM*DSQRT((1D0+DSQRT(1D0-4D0*AAA**2/ZM))/2D0)
C
      AAA=DSQRT(PAI*(ALPW+ALP*DR)/DSQRT(2D0)/GF)
      WMASS(2)=SQZM*DSQRT((1D0+DSQRT(1D0-4D0*AAA**2/ZM))/2D0)
C
      AAA=DSQRT(PAI*ALPW/DSQRT(2D0)/GF)
      SQWMLL=SQZM*DSQRT((1D0+DSQRT(1D0-4D0*AAA**2/ZM))/2D0)
      WMASS(3)=
     * SQWMLL+SQWM**3*(ZM-WM)**2*ACC(0D0)/ALP/PAI/ZM/(2D0*WM-ZM)
C  ---------------------------------------------------------------------
C  DEF OF Y
      Y=-1D0/48D0+7D0*DLOG(WM)/32D0
     #-F1*(DLOG(EM)+DLOG(DM)/3D0+4D0*DLOG(UM)/3D0)/24D0
     #-F2*(DLOG(XM)+DLOG(SM)/3D0+4D0*DLOG(CM)/3D0)/24D0
     #-F3*(DLOG(TAM)+DLOG(BM)/3D0+4D0*DLOG(TM)/3D0)/24D0
     #-F4*(DLOG(FLM)+DLOG(FDM)/3D0+4D0*DLOG(FUM)/3D0)/24D0
C  DEF OF VF0
      VF0=(ZM*DLOG(ZM)-WM*DLOG(WM))/(ZM-WM)-1D0
C  DEF OF VC2
      VC2=5D0-6D0*DLOG(WM)-2D0*WM/(ZM-WM)*(5D-1+3D0*VF0)
     #-ZM*(ZM-2D0*WM)/2D0/WM/(ZM-WM)*(5D-1+DLOG(ZM))
     #+16D0*(DELZM/ZM-(DELZM-DELWM)/(ZM-WM)+2D0*Y+ZL+ZN)
C  DEF OF VC3
      VC3=VC2-16D0*(ZL-ZLM)
C
      DR=ACC(0D0)/AT
      AAA=DSQRT(PAI*ALP/DSQRT(2D0)/GF/(1D0-DR))
      WMASS(4) = SQZM*DSQRT((1D0+DSQRT(1D0-4D0*AAA**2/ZM))/2D0)
C  ---------------------------------------------------------------------
C
      RETURN
      END
C
C
C ** ACC **
C
      DOUBLE PRECISION FUNCTION ACC(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
     *     UM, DM, CM, SM, TM, BM, PM, DELWM,
     #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM, FDM
      ACC=ALP*ALP*ZM/16D0/WM/(ZM-WM)
     #*(-VC2-VC3+2D0*ZM/WM/(ZM-WM)*AW(X)-1D0-2D0*DLOG(WM)
     #+(3D0*ZM*ZM-6D0*ZM*WM-2D0*WM*WM)/(ZM-WM)**2*DLOG(ZM/WM))
      RETURN
      END
C
C
C ** AW **
C
      DOUBLE PRECISION FUNCTION AW(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
     *     UM, DM, CM, SM, TM, BM, PM, DELWM,
     #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM, FDM
      AW=-(X-WM)/2D0+(-(PM-2D0*WM)*(E0(PM,WM,X)-E0(PM,WM,WM))
     #+(AWFL(X)+AWFQ(X))*2D0
     #+(PM-WM)*(E1(PM,WM,X)-E1(PM,WM,WM))+X*E(PM,WM,X)-WM*E(PM,WM,WM))
     #/2D0+(ZM-WM)/ZM*(-7D0*WM*((DLOG(WM)-1D0)-E0(0D0,WM,WM))
     #-5D0*(X*(DLOG(WM)-1D0)-WM*E0(0D0,WM,WM))
     #+8D0*WM*((DLOG(WM)-1.5D0)/2D0-E0(0D0,WM,WM)+E1(0D0,WM,WM))
     #+10D0*(X*(DLOG(WM)/6D0-5D0/36D0)-WM*E(0D0,WM,WM)))
     #+((4D0*ZM*ZM-10D0*ZM*WM-28D0*WM*WM)
     #*(E0(WM,ZM,X)-E0(WM,ZM,WM))
     #-20D0*WM*(X*E0(WM,ZM,X)-WM*E0(WM,ZM,WM))
     #+(-2D0*ZM*ZM-30D0*ZM*WM+32D0*WM*WM)*(E1(WM,ZM,X)-E1(WM,ZM,WM))
     #+(2D0*ZM+4D1*WM)*(X*E(WM,ZM,X)-WM*E(WM,ZM,WM)))/4D0/ZM
      RETURN
      END
C
C
C ** AWFL **
C
      DOUBLE PRECISION FUNCTION AWFL(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
     *     UM, DM, CM, SM, TM, BM, PM, DELWM,
     #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM, FDM
      COMMON/ZHCMWN/F1,F2,F3,F4
      AWFL=-EM*(E0(0D0,EM,X)-E0(0D0,EM,WM))*F1
     #+EM*(E0(0D0,EM,X)-E1(0D0,EM,X)-E0(0D0,EM,WM)+E1(0D0,EM,WM))*F1
     #+2D0*(X*E(0D0,EM,X)-WM*E(0D0,EM,WM))*F1
     #-XM*(E0(0D0,XM,X)-E0(0D0,XM,WM))*F2
     #+XM*(E0(0D0,XM,X)-E0(0D0,XM,WM)-E1(0D0,XM,X)+E1(0D0,XM,WM))*F2
     #+2D0*(X*E(0D0,XM,X)-WM*E(0D0,XM,WM))*F2
     #-TAM*(E0(0D0,TAM,X)-E0(0D0,TAM,WM))*F3
     #+TAM*(E0(0D0,TAM,X)-E0(0D0,TAM,WM)-E1(0D0,TAM,X)+E1(0D0,TAM,WM))*F
     #3
     #+2D0*(X*E(0D0,TAM,X)-WM*E(0D0,TAM,WM))*F3
     #-FLM*(E0(0D0,FLM,X)-E0(0D0,FLM,WM))*F4
     #+FLM*(E0(0D0,FLM,X)-E0(0D0,FLM,WM)-E1(0D0,FLM,X)+E1(0D0,FLM,WM))
     #*F4
     #+2D0*(X*E(0D0,FLM,X)-WM*E(0D0,FLM,WM))*F4
      RETURN
      END
C
C
C ** AWFQ **
C
      DOUBLE PRECISION FUNCTION AWFQ(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
     *     UM, DM, CM, SM, TM, BM, PM, DELWM,
     #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM, FDM
      COMMON/ZHCMWN/F1,F2,F3,F4
      AWFQ=3D0*(
     #CS*(-DM*(E0(DM,UM,X)-E0(DM,UM,WM))
     #+(DM-UM)*(E1(DM,UM,X)-E1(DM,UM,WM))
     #+2D0*(X*E(DM,UM,X)-WM*E(DM,UM,WM)))*F1
     #+SN*(-SM*(E0(SM,UM,X)-E0(SM,UM,WM))
     #+(SM-UM)*(E1(SM,UM,X)-E1(SM,UM,WM))
     #+2D0*(X*E(SM,UM,X)-WM*E(SM,UM,WM)))
     #+CS*(-SM*(E0(SM,CM,X)-E0(SM,CM,WM))
     #+(SM-CM)*(E1(SM,CM,X)-E1(SM,CM,WM))
     #+2D0*(X*E(SM,CM,X)-WM*E(SM,CM,WM)))*F2
     #+SN*(-DM*(E0(DM,CM,X)-E0(DM,CM,WM))
     #+(DM-CM)*(E1(DM,CM,X)-E1(DM,CM,WM))
     #+2D0*(X*E(DM,CM,X)-WM*E(DM,CM,WM)))
     #-BM*(E0(BM,TM,X)-E0(BM,TM,WM))*F3
     #+(BM-TM)*(E1(BM,TM,X)-E1(BM,TM,WM))*F3
     #+2D0*(X*E(BM,TM,X)-WM*E(BM,TM,WM))*F3
     #-FDM*(E0(FDM,FUM,X)-E0(FDM,FUM,WM))*F4
     #+(FDM-FUM)*(E1(FDM,FUM,X)-E1(FDM,FUM,WM))*F4
     #+2D0*(X*E(FDM,FUM,X)-WM*E(FDM,FUM,WM))*F4)
      RETURN
      END
C
C
C ** E0 **
C
      DOUBLE PRECISION FUNCTION E0(X,Y,Z)
      IMPLICIT REAL*8(A-H,O-Z)
      WD=X-Y
      IF(WD.NE.0D0) GO TO 1
      WS=X+Y
      IF(WS.NE.0D0) GO TO 2
      IF(Z.NE.0D0) GO TO 3
      E0=0D0
      RETURN
3     E0=DLOG(DABS(Z))-2D0
      RETURN
2     IF(Z) 10,20,30
10    WX=Z**2/(X**2*(DABS(DLOG(X))))-10D-4
      IF(WX.GE.0D0) GO TO 200
      E0=DLOG(X)-Z/(6D0*X)
      RETURN
200   E0=DLOG(X)-2D0+DSQRT(1D0-4D0*X/Z)*
     #DLOG((2D0*X-Z+DSQRT((-Z)*(4D0*X-Z)))/(2D0*X))
      RETURN
20    E0=DLOG(X)
      RETURN
30    WZ=Z-4D0*X
      IF(WZ.GE.0D0) GO TO 1000
      E0=DLOG(X)-2D0+2D0*DSQRT((4D0*X-Z)/Z)*
     #DATAN(DSQRT(Z/(4D0*X-Z)))
      RETURN
1000  E0=DLOG(X)-2D0+DSQRT((Z-4D0*X)/Z)*
     #DLOG((Z-2D0*X+DSQRT(Z*(Z-4D0*X)))/(2D0*X))
      RETURN
1     IF(X.NE.0D0) GO TO 6
      IF(Y.NE.Z) GO TO 35
      E0=DLOG(Z)-2D0
      RETURN
35    RYZ=DABS(Z/Y)-1D-5
      IF(RYZ.GE.0D0) GO TO 36
      E0=-1D0+DLOG(Y)-Z/2D0/Y
      RETURN
36    E0=(1D0-Y/Z)*DLOG(DABS(Z-Y))+Y*DLOG(Y)/Z-2D0
      RETURN
6     WY=Y-Z
      IF(WY.NE.0D0) GO TO 40
      WB=4D0*Z-X
      IF(WB.GT.0D0) GO TO 300
      IF(Z/X.GT.3D-5)GO TO 303
      E0=DLOG(X)
      RETURN
303   E0=X*DLOG(X)/(2D0*Z)-(X-2D0*Z)*DLOG(Z)/(2D0*Z)-2D0
     #-DSQRT(X*(X-4D0*Z))*DLOG((X+DSQRT(X*(X-4D0*Z)))**2
     #/(4D0*X*Z))/2D0/Z
      RETURN
300   E0=X*DLOG(X)/(2D0*Z)-(X-2D0*Z)*DLOG(Z)/(2D0*Z)-2D0
     #+DSQRT(X*(4D0*Z-X))*(DATAN((2D0*Z-X)/(DSQRT(X*(4D0*Z-X))))
     #+DATAN(X/(DSQRT(X*(4D0*Z-X)))))/Z
      RETURN
40    WA=X-Z
      IF(WA.NE.0D0) GO TO 50
      WB=4D0*Z-Y
      IF(WB.GT.0D0) GO TO 400
      IF(Z/Y.GT.3D-5)GO TO 404
      E0=DLOG(Y)
      RETURN
404   E0=Y*DLOG(Y)/(2D0*Z)-(Y-2D0*Z)*DLOG(Z)/(2D0*Z)-2D0
     #-DSQRT(Y*(Y-4D0*Z))*DLOG((Y+DSQRT(Y*(Y-4D0*Z)))**2
     #/(4D0*Y*Z))/2D0/Z
      RETURN
400   E0=Y*DLOG(Y)/(2D0*Z)-(Y-2D0*Z)*DLOG(Z)/(2D0*Z)-2D0
     #+DSQRT(Y*(4D0*Z-Y))*(DATAN((2D0*Z-Y)/(DSQRT(Y*(4D0*Z-Y))))
     #+DATAN(Y/(DSQRT(Y*(4D0*Z-Y)))))/Z
      RETURN
50    WW=(Z/(X-Y))**2-1D-6
      IF(WW.GE.0D0) GO TO 4000
      IF(X/Y.GT.1D-5)GO TO 123
      E0=DLOG(Y)
      RETURN
123   IF(Y/X.GT.1D-5)GO TO 234
      E0=DLOG(X)
      RETURN
234   E0=(X*DLOG(X)-Y*DLOG(Y))/(X-Y)-1D0
      RETURN
4000  QM1=Z-(DSQRT(X)-DSQRT(Y))**2
      QM2=Z-(DSQRT(X)+DSQRT(Y))**2
      IF(QM1) 5000,5000,7
7     IF(QM2) 70,5000,5000
70    E0=-2D0+DLOG(X*Y)/2D0+(Y-X)*DLOG(Y/X)/2D0/Z
     #+2D0*DSQRT((2D0*(X+Y)*Z-(X-Y)**2-Z*Z)/4D0/Z/Z)
     #*DATAN(DSQRT(2D0*(X+Y)*Z-(X-Y)**2-Z*Z)/(X+Y-Z))
      RETURN
5000  IF(Z-X-Y) 7000,6000,6000
6000  E0=((Z-X+Y)*DLOG(Y)+(Z+X-Y)*DLOG(X)-4D0*Z+DSQRT((Z+X-Y)**2
     #-4D0*X*Z)*DLOG((Z-X-Y+DSQRT((Z+X-Y)**2-4D0*X*Z))**2/
     #(4D0*X*Y)))/(2D0*Z)
      RETURN
7000  E0=((Z-X+Y)*DLOG(Y)+(Z+X-Y)*DLOG(X)-4D0*Z-DSQRT((Z+X-Y)**2
     #-4D0*X*Z)*DLOG((Z-X-Y-DSQRT((Z+X-Y)**2-4D0*X*Z))**2/
     #(4D0*X*Y)))/(2D0*Z)
      RETURN
      END
C
C
C ** E1 **
C
      DOUBLE PRECISION FUNCTION E1(X,Y,Z)
      IMPLICIT REAL*8(A-H,O-Z)
      WD=X-Y
      IF(WD.NE.0D0) GO TO 1
      WS=X+Y
      IF(WS.NE.0D0) GO TO 2
      IF(Z.NE.0D0) GO TO 3
      E1=0D0
      RETURN
3     E1=(DLOG(DABS(Z))-2D0)/2D0
      RETURN
2     IF(Z) 10,20,30
10    WX=Z**2/(X**2*(DABS(DLOG(X))))-10D-4
      IF(WX.GE.0D0) GO TO 200
      E1=(DLOG(X)-Z/(6D0*X))/2D0
      RETURN
200   E1=(DLOG(X)-2D0+DSQRT(1D0-4D0*X/Z)*
     #DLOG((2D0*X-Z+DSQRT((-Z)*(4D0*X-Z)))/(2D0*X)))/2D0
      RETURN
20    E1=(DLOG(X))/2D0
      RETURN
30    WZ=Z-4D0*X
      IF(WZ.GE.0D0) GO TO 1000
      E1=(DLOG(X)-2D0+2D0*DSQRT((4D0*X-Z)/Z)*
     #DATAN(DSQRT(Z/(4D0*X-Z))))/2D0
      RETURN
1000  E1=(DLOG(X)-2D0+DSQRT((Z-4D0*X)/Z)*
     #DLOG((Z-2D0*X+DSQRT(Z*(Z-4D0*X)))/(2D0*X)))/2D0
      RETURN
1     IF(X.NE.0D0) GO TO 6
      IF(Y.NE.Z) GO TO 35
      E1=(DLOG(Z)-1D0)*5D-1
      RETURN
35    RYZ=DABS(Z/Y)-1D-5
      IF(RYZ.GE.0D0) GO TO 36
      E1=-1D0/4D0+DLOG(Y)/2D0-Z/6D0/Y
      RETURN
36    E1=(1D0-Y/Z)**2/2D0*DLOG(DABS(Z-Y))+Y*(2D0*Z-Y)/2D0/Z/Z*DLOG(Y)-1D
     #0+Y/(2D0*Z)
      RETURN
6     WY=Y-Z
      IF(WY.NE.0D0) GO TO 40
      WB=4D0*Z-X
      IF(WB.GT.0D0) GO TO 300
      IF(Z/X.GT.3D-5)GO TO 303
      E1=DLOG(X)/2D0
      RETURN
303   E1=(X*X-2D0*X*Z)*DLOG(X)/(4D0*Z*Z)+(2D0*Z*X-X*X+2D0*Z*Z)*DLOG(Z)/(
     #4D0*Z*Z)-1D0/2D0
     #-DSQRT(X*(X-4D0*Z))*DLOG((X+DSQRT(X*(X-4D0*Z)))**2
     #/(4D0*X*Z))/4D0/Z/Z*X-X/(2D0*Z)
      RETURN
300   E1=(-2D0*X*Z+X*X)*DLOG(X)/(4D0*Z*Z)+(2D0*Z*X-X*X+2D0*Z*Z)*DLOG(Z)/
     #(4D0*Z*Z)-1D0/2D0
     #+DSQRT(X*(4D0*Z-X))*(DATAN((2D0*Z-X)/(DSQRT(X*(4D0*Z-X))))
     #+DATAN(X/(DSQRT(X*(4D0*Z-X)))))/2D0/Z/Z*X-X/2D0/Z
      RETURN
40    WA=X-Z
      IF(WA.NE.0D0) GO TO 50
      WB=4D0*Z-Y
      IF(WB.GT.0D0) GO TO 400
      IF(Z/Y.GT.3D-5)GO TO 404
      E1=DLOG(Y)/2D0
      RETURN
404   E1=Y*DLOG(Y)/(2D0*Z)-(Y-2D0*Z)*DLOG(Z)/(2D0*Z)-2D0
     #-DSQRT(Y*(Y-4D0*Z))*DLOG((Y+DSQRT(Y*(Y-4D0*Z)))**2
     #/(4D0*Y*Z))/2D0/Z
     #-(Y*Y-2D0*Y*Z)*DLOG(Y)/(4D0*Z*Z)-(2D0*Z*Y-Y*Y+2D0*Z*Z)*DLOG(Z)/(4D
     #0*Z*Z)+1D0/2D0
     #+DSQRT(Y*(Y-4D0*Z))*DLOG((Y+DSQRT(Y*(Y-4D0*Z)))**2
     #/(4D0*Y*Z))/4D0/Z/Z*Y+Y/(2D0*Z)
      RETURN
400   E1=Y*DLOG(Y)/(2D0*Z)-(Y-2D0*Z)*DLOG(Z)/(2D0*Z)-2D0
     #+DSQRT(Y*(4D0*Z-Y))*(DATAN((2D0*Z-Y)/(DSQRT(Y*(4D0*Z-Y))))
     #+DATAN(Y/(DSQRT(Y*(4D0*Z-Y)))))/Z
     #-((-2D0*Y*Z+Y*Y)*DLOG(Y)/(4D0*Z*Z)+(2D0*Z*Y-Y*Y+2D0*Z*Z)*DLOG(Z)/
     #(4D0*Z*Z)-1D0/2D0
     #+DSQRT(Y*(4D0*Z-Y))*(DATAN((2D0*Z-Y)/(DSQRT(Y*(4D0*Z-Y))))
     #+DATAN(Y/(DSQRT(Y*(4D0*Z-Y)))))/2D0/Z/Z*Y-Y/2D0/Z)
      RETURN
50    WW=(Z/(X-Y))**2-1D-6
      IF(WW.GE.0D0) GO TO 4000
      IF(X/Y.GT.1D-5)GO TO 123
      E1=DLOG(Y)/2D0
      RETURN
123   IF(Y/X.GT.1D-5)GO TO 234
      E1=DLOG(X)/2D0
      RETURN
234   E1=(DLOG(Y)-X*X/(Y-X)**2*DLOG(Y/X)-5D-1+X/(Y-X))/2D0
      RETURN
4000  QM1=Z-(DSQRT(X)-DSQRT(Y))**2
      QM2=Z-(DSQRT(X)+DSQRT(Y))**2
      IF(QM1) 5000,5000,7
7     IF(QM2) 70,5000,5000
70    E1=-X*DLOG(X)/2D0/Z+Y*DLOG(Y)/2D0/Z+(Y-X-2D0*Z)
     #/2D0/Z-(Y-X-Z)/2D0/Z*(DLOG(X*Y)/2D0+(Y-X)*DLOG(Y/X)/2D0/Z)
     #-(Y-X-Z)/Z*DSQRT((2D0*(X+Y)*Z-(X-Y)**2-Z*Z)/4D0/Z/Z)
     #*DATAN(DSQRT(2D0*(X+Y)*Z-(X-Y)**2-Z*Z)/(X+Y-Z))
      RETURN
5000  IF(Z-X-Y) 7000,6000,6000
6000  E1=(2D0*Z**2*DLOG(Y)-((Z+X-Y)**2-2D0*X*Z)*
     #DLOG(Y/X)-4D0*Z**2-2D0*Z*(X-Y)+(Z+X-Y)*DSQRT((Z+X-Y)**2
     #-4D0*X*Z)*DLOG((Z-X-Y
     #+DSQRT((Z+X-Y)**2-4D0*X*Z))**2/(4D0*X*Y)))/(4D0*Z**2)
      RETURN
7000  E1=(2D0*Z**2*DLOG(Y)-((Z+X-Y)**2-2D0*X*Z)*
     #DLOG(Y/X)-4D0*Z**2-2D0*Z*(X-Y)-(Z+X-Y)*DSQRT((Z+X-Y)**2
     #-4D0*X*Z)*DLOG((Z-X-Y
     #-DSQRT((Z+X-Y)**2-4D0*X*Z))**2/(4D0*X*Y)))/(4D0*Z**2)
      RETURN
      END
C
C
C ** E2 **
C
      DOUBLE PRECISION FUNCTION E2(X,Y,Z)
      IMPLICIT REAL*8(A-H,O-Z)
      WD=X-Y
      IF(WD.NE.0D0) GO TO 1
      WS=X+Y
      IF(WS.NE.0D0) GO TO 2
      IF(Z.NE.0D0) GO TO 3
      E2=0D0
      RETURN
3     E2=DLOG(DABS(Z))/3D0-13D0/18D0
      RETURN
2     IF(Z) 10,20,30
10    WX=Z**2/(X**2*(DABS(DLOG(X))))-10D-4
      IF(WX.GE.0D0) GO TO 200
      E2=DLOG(X)/3D0-Z/(20D0*X)
      RETURN
200   E2=DLOG(X)/3D0-13D0/18D0+DSQRT(1D0-4D0*X/Z)*
     #DLOG((2D0*X-Z+DSQRT((-Z)*(4D0*X-Z)))/(2D0*X))
     #*(1D0-X/Z)/3D0+2D0*X/(3D0*Z)
      RETURN
20    E2=DLOG(X)/3D0
      RETURN
30    WZ=Z-4D0*X
      IF(WZ.GE.0D0) GO TO 1000
      E2=DLOG(X)/3D0-13D0/18D0+2D0*(Z-X)/3D0*DSQRT((4D0*X-Z)/Z)*
     #DATAN(DSQRT(Z/(4D0*X-Z)))/Z+2D0*X/(3D0*Z)
      RETURN
1000  E2=DLOG(X)/3D0-13D0/18D0+(Z-X)/3D0*DSQRT((Z-4D0*X)/Z)*
     #DLOG((Z-2D0*X+DSQRT(Z*(Z-4D0*X)))/(2D0*X))/Z+2D0*X/(3D0*Z)
      RETURN
1     IF(X.NE.0D0) GO TO 6
      IF(Y.NE.Z) GO TO 35
      E2=(3D0*DLOG(Z)-2D0)/9D0
      RETURN
35    RYZ=DABS(Z/Y)-1D-5
      IF(RYZ.GE.0D0) GO TO 36
      E2=-1D0/9D0+DLOG(Y)/3D0-Z/12D0/Y
      RETURN
36    E2=(1D0-Y/Z)**3*DLOG(DABS(Z-Y))/3D0+Y*(3D0*Z*Z-3D0*Y*Z+Y**2)*DLOG
     #(Y)/Z**3/3D0-13D0/18D0+5D0*Y/(6D0*Z)-Y*Y/(3D0*Z*Z)
      RETURN
6     WY=Y-Z
      IF(WY.NE.0D0) GO TO 40
      WB=4D0*Z-X
      IF(WB.GT.0D0) GO TO 300
      IF(Z/X.GT.3D-5)GO TO 303
      E2=DLOG(X)/3D0
      RETURN
303   E2=DLOG(Z)/3D0+X**2*(X-3D0*Z)*DLOG(X/Z)/(6D0*Z**3)
     #-2D0/9D0
     #-DSQRT(X*(X-4D0*Z))*DLOG((X+DSQRT(X*(X-4D0*Z)))**2
     #/(4D0*X*Z))/(6D0*Z**3)*X*(X-Z)+X*(3D0*Z-2D0*X)/(6D0*Z*Z)
      RETURN
300   E2=DLOG(Z)/3D0+X**2*(X-3D0*Z)*DLOG(X/Z)/(6D0*Z**3)
     #-2D0/9D0
     #+DSQRT(X*(4D0*Z-X))*(DATAN((2D0*Z-X)/(DSQRT(X*(4D0*Z-X))))
     #+DATAN(X/(DSQRT(X*(4D0*Z-X)))))/(Z**3*3D0)*X*(X-Z)
     #+X*(3D0*Z-2D0*X)/(6D0*Z*Z)
      RETURN
40    WA=X-Z
      IF(WA.NE.0D0) GO TO 50
      WB=4D0*Z-Y
      IF(WB.GT.0D0) GO TO 400
      IF(Z/Y.GT.3D-5)GO TO 404
      E2=DLOG(Y)/3D0
      RETURN
404   E2=((9D0*Z*Z-6D0*Y*Z+Y*Y)*Y*DLOG(Y)+(2D0*Z**3-9D0*Y*Z*Z+6D0*Y*
     #Y*Z-Y**3)*DLOG(Z)-Z*(22D0*Z*Z-27D0*Y*Z+6D0*Y*Y)/3D0
     #-(3D0*Z-Y)*(Z-Y)*DSQRT(Y*(Y-4D0*Z))*
     #DLOG((Y+DSQRT(Y*(Y-4D0*Z)))**2/(4D0*Y*Z)))/(6D0*Z**3)
      RETURN
400   E2=((9D0*Z*Z-6D0*Y*Z+Y*Y)*Y*DLOG(Y)+(2D0*Z**3-9D0*Y*Z*Z+6D0*Y*
     #Y*Z-Y**3)*DLOG(Z)-Z*(22D0*Z*Z-27D0*Y*Z+6D0*Y*Y)/3D0
     #+(3D0*Z-Y)*(Z-Y)*DSQRT(Y*(4D0*Z-Y))*
     #(DATAN((2D0*Z-Y)/DSQRT(Y*(4D0*Z-Y)))
     #+DATAN(DSQRT(Y/(4D0*Z-Y))))*2D0)/(6D0*Z**3)
      RETURN
50    WW=(Z/(X-Y))**2-1D-6
      IF(WW.GE.0D0) GO TO 4000
      IF(X/Y.GT.1D-5)GO TO 123
      E2=DLOG(Y)/3D0
      RETURN
123   IF(Y/X.GT.1D-5)GO TO 234
      E2=DLOG(X)/3D0
      RETURN
234   E2=(DLOG(Y)+(X/(Y-X))**3*DLOG(Y/X)-1D0/3D0+X/(Y-X)/2D0
     #-(X/(Y-X))**2)/3D0
      RETURN
4000  QM1=Z-(DSQRT(X)-DSQRT(Y))**2
      QM2=Z-(DSQRT(X)+DSQRT(Y))**2
      IF(QM1) 5000,5000,7
7     IF(QM2) 70,5000,5000
70    E2=-13D0/18D0+(5D0*Y-X)/6D0/Z-(X-Y)**2/3D0/Z/Z
     #+((Y-X+Z)**3/24D0/Z**3-Y*(Y-X-Z)/2D0/Z**2)*DLOG(Y)
     #-((Y-X-Z)**3/24D0/Z**3-X*(Y-X-Z)/2D0/Z**2)*DLOG(X)
     #+(Y-X-Z)**2/8D0/Z**2*(DLOG(X*Y)+(Y-X)/Z*DLOG(Y/X))
     #+2D0/3D0/Z**2*(Z*Z+(X-2D0*Y)*Z+(X-Y)**2)
     #*DSQRT((2D0*(X+Y)*Z-(X-Y)**2-Z*Z)/4D0/Z**2)
     #*DATAN(DSQRT(2D0*(X+Y)*Z-(X-Y)**2-Z*Z)/(X+Y-Z))
      RETURN
5000  IF(Z-X-Y) 7000,6000,6000
6000  E2=(6D0*Z**3*DLOG(Y)-3D0*(Z+X-Y)*((Z+X-Y)**2
     #-3D0*X*Z)*DLOG(Y/X)-4D0*Z**3+12D0*X*Z**2-3D0*
     #Z*(Z+X-Y)*(3D0*Z+2D0*(X-Y))+3D0*((Z+X-Y)**2-X*Z)
     #*DSQRT((Z+X-Y)**2-4D0*X*Z)*DLOG((Z-X-Y+DSQRT
     #((Z+X-Y)**2-4D0*X*Z))**2/(4D0*X*Y)))/(18D0*Z**3)
      RETURN
7000  E2=(6D0*Z**3*DLOG(Y)-3D0*(Z+X-Y)*((Z+X-Y)**2
     #-3D0*X*Z)*DLOG(Y/X)-4D0*Z**3+12D0*X*Z**2-3D0*
     #Z*(Z+X-Y)*(3D0*Z+2D0*(X-Y))-3D0*((Z+X-Y)**2-X*Z)
     #*DSQRT((Z+X-Y)**2-4D0*X*Z)*DLOG((Z-X-Y-DSQRT
     #((Z+X-Y)**2-4D0*X*Z))**2/(4D0*X*Y)))/(18D0*Z**3)
      RETURN
      END
C
C
C ** E **
C
      DOUBLE PRECISION FUNCTION E(X,Y,Z)
      IMPLICIT REAL*8(A-H,O-Z)
      E=E1(X,Y,Z)-E2(X,Y,Z)
      RETURN
      END

      BLOCK DATA  ZHPRM
      IMPLICIT REAL*8(A-H,O-Z)
C FERMION MASSES-SQURE
C---
C    Remove option BREMS5 for adjust light quark masses to get good
C    agreement with BREMMUS5.
C---
      COMMON /ZHMASS/ ZM, SQZM, WM, SQWM, EM, XM, TAM,
     *     UM, DM, CM, SM, TM, BM, PM, DELWM,
     #     DELZM, ZL, ZLM, Y, CS, SN, ALP, PAI, VC2, VC3, FLM, FUM, FDM
      DATA  EM/ 2.611D-7/, XM/ 0.01117D0/, TAM/3.17552D0/
     *,     UM/ 0.00160D0 /, DM/ 0.004225D0/, CM/ 2.25D0/, SM/ 0.01D0/
     *,     BM/ 22.09D0   /, FLM/ 2500.D0/, FUM/ 2500.D0/, FDM/ 2500.D0/
CBREMS5       DATA  EM/ 2.611D-7/, XM/ 0.01117/, TAM/3.17552/
CBREMS5      *,     UM/ 0.00305 /, DM/ 0.00305 /, CM/ 2.25/, SM/ 0.49  /
CBREMS5      *,     BM/ 20.25   /, FLM/ 2500./, FUM/ 2500./, FDM/ 2500./
      DATA ALP/0.007297350D0/, PAI/ 3.14159D0/
      COMMON/ZHCMWN/F1,F2,F3,F4
C  F1,F2,F3,F4 : FERMION GENERATION
      DATA F1,F2,F3,F4/ 1., 1., 1., 0./
      END
CDECK  ID>, MYBASES.
* (22-Mar-1996) Mod. version of Bases5.1 by S.G.
************************************************************************
*    ====================================================              *
      SUBROUTINE BASES( FXN, S, SIGMA, CTIME, IT1, IT2 )
*    ====================================================              *
*      Subroutine BASES for the Numerical integration.                 *
*      In terms of this program Integration can be done, furthermore   *
*      a probability distribution can be made for the event generation.*
*      The event with weight one is generated by program SPRING.       *
* ((Input))                                                            *
*    from the arguement                                                *
*      FXN    : Name of function program                               *
*    from the labeled common /BASE1/                                   *
*      XL(50) : Lower limits of the integration variabels              *
*      XU(50) : upper limits of the integration variabels              *
*      NDIM   : Dimension of the integration                           *
*      NCALL  : Number of sampling points per iteration                *
*    from the lebeled common /BASE2/                                   *
*      ITMX*  : Number of iteration                                    *
*      ACC*   : Required accuracies                                    *
* ((Output))                                                           *
*      S      : Estimate of the integral                               *
*      SIGMA  : Standard deviation of the estimate                     *
*      CTIME  : Computing time required for integration                *
*      IT1    : Number of iterations for the grid defining step        *
*      IT2    : Number of iterations for the integration step          *
C*                                                                     *
C*       Coded by S.Kawabata         April '94                         *
C*                                                                     *
C***********************************************************************
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      EXTERNAL FXN
      PARAMETER (MXDIM = 50)
*
*     JFLAG =  0 : First trial for defining grids.
*     JFLAG =  1 : First trial for event accumulation.
*     JFLAG =  2 : Second or more trial for defining grids.
*     JFLAG =  3 : Second or more trial for accumulation.
*                                                                      *
      COMMON /BASE0/ JFLAG,IBASES
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF
      CHARACTER*80 ERROR
      COMMON /BWARN1/ NERROR
      COMMON /BWARN2/ ERROR(3,3)
*        INTV = ( 0 / 1 / any ) = ( Batch / Batch(Unix) / Interactive )
*        IPNT = ( 0 / any ) = ( IBM Type / Ascii printer )
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

       COMMON/NINFO/ NODEID, NUMNOD
       COMMON /BDATE/ IDATE(3),ITIME(2)
*            IDATE(1) : year        ITIME(1) : hour
*            IDATE(2) : month       ITIME(2) : minute
*            IDATE(3) : day
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)

*-------------------------------------------------
*     Check the parameters defined by user
*------------------------------------------------------

      CALL BSCHCK

* ---------------------------------------------------------------
*          Initialize timer
* ---------------------------------------------------------------

       CALL BSDATE

       JFLAG  = 0
       LU     = 6
       IF( INTV .GT. 1 ) THEN
           CALL BSPRNT( LU, 1, IDUM1, IDUM2 )
       ENDIF

C  -----------------------------------------------------
C     Defining grids
C  -----------------------------------------------------
*
       DO 100 I = 1, NWILD
          IG(I) = 1
  100  CONTINUE

       CALL BSETGU

       IF( INTV .GT. 1 ) THEN
           CALL BSPRNT( LU, 4, IDUM1, IDUM2 )
       ENDIF

       CALL BSUTIM( 0, 2 )

*     ===================
       CALL BSINTG( FXN )
*     ===================        For a parallel computer
C                                      CALL BSCAST( JFLAG, 1 )

*  ----------------------------------------------------
*     Accumulation to make probability distribution
*  ----------------------------------------------------
*     ===================
       CALL BSINTG( FXN )
*     ===================        For a parallel computer
C                                      CALL BSCAST( JFLAG, 1 )
       S     = AVGI
       SIGMA = SD
       CTIME = STIME
       IT1   = ITG
       IT2   = ITF

       CALL BSUTIM( 0, 2 )
       TIMEB2 = RTIME

       IF( NERROR .GT. 0 ) THEN
           WRITE(6,9900)
 9900      FORMAT(1X,'****************************************',
     .               '***************************************',
     .           /1X,'* (((( Warning in the integration step ',
     .               '))))                                   *',
     .           /1X,'*                                      ',
     .               '                                       *')
           DO 990 J = 1,NERROR
           DO 990 I = 1,3
              WRITE(6,9901) ERROR(I,J)
 9901         FORMAT(1X,A79)
  990      CONTINUE
           WRITE(6,9902)
 9902      FORMAT(1X,'*                                      ',
     .               '                                       *',
     .           /1X,'*(( Suggestion ))                      ',
     .               '                                       *',
     .           /1X,'* (1) Try integration again with larger ',
     .               'number of sample points than this job.*',
     .           /1X,'* or                                   ',
     .               '                                       *',
     .           /1X,'* (2) The integral variables are not sui',
     .               'ted for the function.                 *',
     .           /1X,'*     Take another integral variables !!',
     .               '                                      *',
     .           /1X,'*                                       ',
     .               '                                      *',
     .           /1X,'****************************************',
     .               '***************************************')
       ENDIF

       IF( INTV .GT. 1 ) THEN
           CALL BSPRNT( LU, 2, IDUM1, IDUM2 )
       ENDIF

       RETURN
       END
************************************************************************
*     ==========================                                       *
       SUBROUTINE BHINIT( LUN )
*     ==========================                                       *
*                                                                      *
* ((Purpose))                                                          *
*    Initialization program for  histograms and scatter plots.         *
*    This program is called by USERIN.                                 *
* ((Arguments))                                                        *
*    LUN    : logical unit number for message print                    *
* !! Caution!!                                                         *
*    When LUN is set equal to 0, the message print is suppressed.      *
* (( Common /PLOTH/ ))                                                 *
*                                                                      *
*    NW                     : Total number of words of used buffer     *
*                                                                      *
*    NHIST                  : Number of Histograms                     *
*    NSCAT                  : Number of Scat_Plots                     *
*                                                                      *
*   -----------------                                                  *
*     Hashing Table                                                    *
*   -----------------                                                  *
*                                                                      *
*     XHASH(   1,i)      : NH Number of histograms for the i-th class  *
*     XHASH(   2,i) = K  : Serial number of histograms                 *
*              :                     :                                 *
*     XHASH(NH+1,i) = K  : Serial number of histograms                 *
*                     |                                                *
*              MAPL(1,K) = ID  : Histogram ID                          *
*              MAPL(2,K) = IP1 : the 1st pointer to the K-th buffer    *
*              MAPL(3,K) = IP2 : the 2nd pointer to the K-th buffer    *
*              MAPL(4,K) = IP3 : the 3rd pointer to the K-th buffer    *
*                                                                      *
* (( Common /PLOTB/ ))                                                 *
*                                                                      *
*   --------------------                                               *
*     Histogram buffer                                                 *
*   --------------------                                               *
*                                                                      *
*    IP1  = NW + 1                                                     *
*           NW = NW + 281    : Updated NW                              *
*       BUFF( IP1 )          = Xmin                                    *
*       BUFF( IP1 + 1 )      = Xmax                                    *
*       IBUF( IP1 + 2 )      = No. of bins                             *
*       BUFF( IP1 + 3 )      = Bin width                               *
*    IP2  = IP1 + 4                                                    *
*       IBUF(   IP2       )                                            *
*          => IBUF( +  51 )  = No. of sampling points                  *
*       BUFF(   IP2 +  52)                                             *
*          => BUFF( + 103 )  = Sum of Fi for the current IT            *
*       BUFF(   IP2 + 104)                                             *
*          => BUFF( + 155 )  = Sum of Fi**2 for the current IT         *
*       BUFF(   IP2 + 156)                                             *
*          => BUFF( + 207 )  = Sum of Fi for total                     *
*       BUFF(   IP2 + 208)                                             *
*          => BUFF( + 259 )  = Sum of Fi**2 for total                  *
*    IP3  = IP1 + 264                                                  *
*       IBUF( IP3 )          = Tag for spring                          *
*       IBUF( IP3   +  1 )                                             *
*          => IBUF( + 16 )   = Title of this histogram                 *
*                                                                      *
*   --------------------                                               *
*     Scat_Plot buffer                                                 *
*   --------------------                                               *
*                                                                      *
* IP1   = NW + 1                                                       *
*         NW  = NW + 2527                                              *
*       BUFF( IP1 )          = Xmin                                    *
*       BUFF( IP1 + 1 )      = Xmax                                    *
*       IBUF( IP1 + 2 )      = No. of bins for X                       *
*       BUFF( IP1 + 3 )      = Bin width for X                         *
*       BUFF( IP1 + 4 )      = Ymin                                    *
*       BUFF( IP1 + 5 )      = Ymax                                    *
*       IBUF( IP1 + 6 )      = No. of bins for Y                       *
*       BUFF( IP1 + 7 )      = Bin width for Y                         *
* IP2   = IP1 + 8                                                      *
*       BUFF(   IP2       )  = No. of sampling points                  *
*       BUFF(   IP2 +   1 )                                            *
*          => BUFF( +2500 )  = Sum of Fi                               *
* IP3   = IP1 + 2509                                                   *
*       IBUF( IP3 )          = X-Tag for spring                        *
*       IBUF( IP3   +  1 )   = Y-Tag for spring                        *
*       IBUF( IP3   +  2 )                                             *
*          => IBUF( + 17 )   = Title of this histogram                 *
*                                                                      *
*  ((Author))                                                          *
*    S.Kawabata    June '90 at KEK                                     *
*                                                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      COMMON/PLOTLU/ LU
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
         LU   = LUN

         NW     = 0

         DO 50 I = 1, 13
           XHASH(1,I) = 0
           DHASH(1,I) = 0
   50    CONTINUE
         NHIST    = 0
         NSCAT    = 0
         DO 100 I = 1, NHS
           MAPL(1,I)= 0
  100    CONTINUE
         DO 200 I = 1, NSC
           MAPD(1,I)= 0
  200    CONTINUE
C
      RETURN
      END
************************************************************************
*     =========================                                        *
       SUBROUTINE BHPLOT( LU )
*     =========================                                        *
* ((Purpose))                                                          *
*     Interface routine to print histograms and scatter plots.         *
*     Routines XHPLOT and DHPLOT are called to print them.             *
* ((Author))                                                           *
*     S.Kawabata  June '90  at KEK                                     *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      REAL*8 AVGI,SD,CHI2A
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF
*                                                                      *
*--------------------------- Entry point ------------------------------*
*
      IF( ITF .LE. 0 ) RETURN
*    ===================                                               *
      CALL XHCHCK( LU )
*    ===================

      IF( NHIST .LE. 0 ) THEN
         WRITE(LU,9000)
 9000    FORMAT(1X,'No Histogram')
      ELSE
         DO 500 J = 1, NHIST
            IFBASE(J) = 1
*          =====================
            CALL XHPLOT(LU, 0, J )
*          =====================
  500    CONTINUE
      ENDIF

*    ===================
      CALL DHPLOT( LU )
*    ===================

      RETURN
      END
************************************************************************
*    ====================                                              *
      SUBROUTINE BHRSET
*    ====================                                              *
* ((Purpose))                                                          *
*     To reset contents of histograms and scatter plots.               *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*-------------------------- Clear Histograms --------------------------*
*                                                                      *
         DO 200 J    = 1, NHIST
           IP2       = MAPL(3,J)
           DO 100 I  = IP2,IP2+259
             IBUF(I) = 0
  100      CONTINUE
           IFBASE(J) = 0
  200    CONTINUE
*                                                                      *
*-------------------------- Clear Scat. Plots -------------------------*
*                                                                      *
         DO 500  J   = 1, NSCAT
           IP2       = MAPD(3,J)
           DO 400  I = IP2,IP2+2500
             IBUF(I) = 0.0
  400      CONTINUE
  500    CONTINUE
*                                                                      *
      RETURN
      END
************************************************************************
*     ====================                                             *
       SUBROUTINE BHSAVE
*     ====================                                             *
* ((Purpose))                                                          *
*     To save contents of temporary buffers to the histogram buffers,  *
*     in order to avoid the precision problem.                         *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
      DO 200 J = 1, NHIST
         IP2   = MAPL(3,J)
         NC    = IBUF( MAPL(2,J)+2 ) + 1
         IB1   = IP2 + 52
         IB2   = IB1 + 52
         DO 100 I = 0,NC
            I1    = I + IB1
            I2    = I1 + 104
            BUFF(I2)  = BUFF(I2) + BUFF(I1)
            BUFF(I1)  = 0.0
            I1    = I + IB2
            I2    = I1 + 104
            BUFF(I2)  = BUFF(I2) + BUFF(I1)
            BUFF(I1)  = 0.0
  100    CONTINUE
  200 CONTINUE
C
      RETURN
      END
************************************************************************
*    ===================                                               *
      SUBROUTINE BSCHCK
*    ===================                                               *
* ((Purpose))                                                          *
*     To check user's initialization parameters.                       *
*                                                                      *
*        Coded by S.Kawabata        Oct. '85                           *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER ( MXDIM = 50)
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2

      COMMON /BASE0/ JFLAG,IBASES
      COMMON /BASE1/ XLT(MXDIM),XUT(MXDIM),NDIMT,NWILDT,
     .               IGT(MXDIM),NCALLT
      COMMON /BASE2/ ACC1T,ACC2T,ITMX1T,ITMX2T
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP
      COMMON /XHCNTL/ LOCK

      LOCK  = 1

      IF( IBASES .NE.  1 ) THEN
          WRITE(6,9000)
 9000     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   BSINIT was not called before calling BASES  *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF


      IF( NDIM .LT. 1) THEN
          WRITE(6,9100)
 9100     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   NDIM was not set before calling BASES.      *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

      NDIMT = NDIM

      DO 200 I = 1,NDIM
         IF( XU(I) .LE. -1.0D37) THEN
             WRITE(6,9200) I,I
 9200        FORMAT(
     .        5X,'*************************************************',
     .       /5X,'*                                               *',
     .       /5X,'*   XL(',I6,' ).  XU(',I6,' ) were not set      *',
     .       /5X,'*    before calling BASES.                      *',
     .       /5X,'*   Process was terminated due to this error.   *',
     .       /5X,'*                                               *',
     .       /5X,'*************************************************')
             STOP
         ENDIF

         IGT(I)  = IG(I)
         XLT(I)  = XL(I)
         XUT(I)  = XU(I)

  200 CONTINUE
C
C  Change the maximum number of the wild variables
C 10 ===> 15
      IF( NWILD .LT.  0) THEN
          NWILD = MIN( NDIM, 15)
          WRITE(6,9300) NWILD
 9300     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD was not set before calling BASES.     *',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD is set equal to the value(',I6,' ).   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
      ELSE
     .IF( NWILD .GT. 15) THEN
          NWILDO = NWILD
          NWILD  = MIN( NDIM, 15)
          WRITE(6,9400) NWILDO, NWILD
 9400     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD(',I6,' ) was too large number.        *',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD is set equal to the value(',I6,' ).   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
      ENDIF

      NWILDT = NWILD
      NCALLT = NCALL

      ITMX1T = ITMX1
      ITMX2T = ITMX2
      ACC1T  = ACC1
      ACC2T  = ACC2
C
      RETURN
      END
C***********************************************************************
C*=======================                                              *
       SUBROUTINE BSDATE
C*=======================                                              *
C*((Purpose))                                                          *
C*    Changethe format of the time stamp.                              *
C*    This program should be modified according to the machine.        *
C*((Author))                                                           *
C*    S.Kawabata  Nov. '91 at KEK                                      *
C*    For HP      Jul. '92 at KEK                                      *
C***********************************************************************
       COMMON /BDATE/ IDATE(3),ITIME(2)
       COMMON /SLATE/ IS(40)
*            IDATE(1) : year        ITIME(1) : hour
*            IDATE(2) : month       ITIME(2) : minute
*            IDATE(3) : day

*      CALL UXDATE(IY,IM,ID,IHH,IMM)
       call datime(id,it)
       CALL UCOPY(IS(1),IDATE(1),5)
       IDATE(1) = MOD(IDATE(1),1900)
*      IDATE(1) = IY
*      IDATE(2) = IM
*      IDATE(3) = ID
*      ITIME(1) = IHH
*      ITIME(2) = IMM
       RETURN
       END
************************************************************************
*    ===========================================                       *
      SUBROUTINE BSDIMS( MDIM, MWILD, XLL, XUU )
*    ===========================================                       *
* ((Purpose))                                                          *
*     To set the BASES parameters.                                     *
* ((Input))                                                            *
*     MDIM   : The number of dimension of integral                     *
*     MWILD  : The number of wild variables                            *
*     XLL(i) : The lower value of the i-th integral variable           *
*     XUU(i) : The upper value of the i-th integral variable           *
* ((Output))                                                           *
*     These parameters are to be set in the labeled common /BPARM1/    *
* (Caution)                                                            *
*     The parameter IG(i) is not able to set by this routine.          *
*     If some of parameters IG(i) are required to be changed,          *
*     it is done by calling the subroutine BSGRID.                     *
*                                                                      *
*        Coded by S.Kawabata         August '94                        *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL

      DIMENSION XLL(MDIM), XUU(MDIM)

*=========================================================

      NDIM   = MDIM
      NWILD  = MWILD
      DO 100 I= 1, NDIM
         XL(I) = XLL(I)
         XU(I) = XUU(I)
  100 CONTINUE

       RETURN
       END
C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSETGU                                                *
C*========================                                             *
C*((Function))                                                         *
C*     Initialization of Bases progam                                  *
C*     This is called only when IFLAG=0.                               *
C*     ( IFLAG = 0 ; First Trial of Defining Grid step )               *
C*                                                                     *
C*    Changed by S.Kawabata    Aug. 1984 at Nagoya Univ.               *
C*    Last update              Oct. 1985 at KEK                        *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSETGU
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
     .               ND,NG,NPG,MA(MXDIM)
      COMMON /BASE6/ D(NDMX,MXDIM),
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,ITSX

      DIMENSION  XIN(NDMX)
      DATA  ONE/ 1.0D0/
C
C---------------------------------------------------------------
C           Define the number of grids and sub-regions
C---------------------------------------------------------------
C==> Determine NG : Number of grids
          NG    = (NCALL/2.)**(1./NWILD)
         IF(NG .GT. 25) NG  = 25
  100    IF(NG .LT.  2) NG  =  1
         IF(NG**NWILD .GT. LENG) THEN
            NG  = NG - 1
            GO TO 100
         ENDIF
C
C==> Determine ND : Number of sub-regions
          M     = NDMX/NG
          ND    = M*NG
C
C==> Determine NPG: Number of sampling points per subhypercube
          NSP   = NG**NWILD
          NPG   = NCALL/NSP

          XI(1,1)= ONE
          MA(1)  = 1
          DX(1)  = XU(1)-XL(1)

          IF( NDIM .GT. 1 ) THEN
              DO 130 J = 2,NDIM
                 XI(1,J)= ONE
                 DX(J)  = XU(J)-XL(J)
                 IF( J .LE. NWILD ) THEN
                    MA(J)  = NG*MA(J-1)
                 ENDIF
  130         CONTINUE
          ENDIF
C
C---------------------------------------------------------------
C           Set size of subregions uniform
C---------------------------------------------------------------
          NDM   = ND-1
          RC    = ONE/ND
          DO 155 J =1,NDIM
             K     = 0
             XN    = 0.D0
             DR    = XN
             I     = K
  140        K     = K+1
             DR    = DR+ONE
             XO    = XN
             XN    = XI(K,J)
  145       IF(RC .GT. DR) GO TO 140
             I     = I+1
             DR    = DR-RC
             XIN(I)= XN-(XN-XO)*DR
            IF(I .LT. NDM) GO TO 145
             DO 150 I  = 1,NDM
                XI(I,J)= XIN(I)
  150        CONTINUE
             XI(ND,J)  = ONE
  155     CONTINUE
********************************************* Updated Feb.08 '94
          IF( ITSX .GT. 0 ) THEN
              IPSAVE = 1
              XACC    = 1.0D37
              XTI     = 0.0D0
              XTSI    = XACC
              ITSX    = 1
              DO 200 J = 1, NDIM
              DO 200 I = 1, ND
                 XSAVE(I,J) = XI(I,J)
  200         CONTINUE
          ENDIF
C
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSETGV( IFLAG )                                       *
C*========================                                             *
C*((Function))                                                         *
C*    Refine the grid sizes                                            *
C*                                                                     *
C*    Coded   by S.Kawabata    Aug. 1984 at Nagoya Univ.               *
C*    Last update              Oct. 1985 at KEK                        *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSETGV( IFLAG )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
     .               ND,NG,NPG,MA(MXDIM)
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
      COMMON /BASE6/ D(NDMX,MXDIM),
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,ITSX
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF
*

      DIMENSION  XIN(NDMX),R(NDMX),DT(MXDIM),DDX(NDMX)
      DATA  ONE/1.0D0/,ZERO/0.0D0/,N0/0/,N1/1/
*
*========= Save the grid information for the best accuracy ===========
*
      IF( ITSX .GT. 0 ) THEN
          IF( IFLAG .EQ. 0 ) THEN
              IF( IT .GE. 5 ) THEN
                  IF( ( TI .GT. AVGI+SD) .AND. TSI .LT. XTSI ) THEN
                      DO 400 J = 1, NDIM
                      DO 400 I = 1, ND
                         XSAVE(I,J) = XI(I,J)
  400                 CONTINUE
                      XACC         = TACC
                      ITSX         = IT
                      XTI          = TI
                      XTSI         = TSI
                  ENDIF
              ENDIF
          ELSE
              IF( ( XTI .GT. TI) .AND. XTSI .LT. TSI ) THEN
                  DO 500 J = 1, NDIM
                  DO 500 I = 1, ND
                     XI(I,J) = XSAVE(I,J)
  500             CONTINUE
*                ==========
                   RETURN
*                ==========
              ENDIF
          ENDIF
      ENDIF

C======= SMOOTHING THE FUNCTION D(I,J)
C
        CLOGE   = 1.0D0/LOG(10.0D0)

        NDM     = ND-1
        DO 780 J= N1,NDIM
         IF( IG(J) .EQ. 1 ) THEN
          DDX(1)= 0.5D0*(D(1,J) + D(2,J))
          DO 710 I=2,NDM
            DDX(I)= (D(I+1,J) + D(I,J) + D(I-1,J))/3.D0
  710     CONTINUE
          DDX(ND) = 0.5D0*(D(NDM,J) + D(ND,J))
          DT(J) = 0.D0
          DO 720 I = 1, ND
             D(I,J) = DDX(I)
             DT(J)  = DT(J)+D(I,J)
  720     CONTINUE
C
C=========== REDEFINE THE GRID
C

          DTLOG   = LOG(DT(J))
          DT10    = CLOGE*DTLOG
          RC    = ZERO
          DO 730 I= N1,ND
            R(I)  = ZERO
            IF(D(I,J) .GT. ZERO) THEN
               DILOG = LOG(D(I,J))
               IF( DT10 - CLOGE*DILOG  .LE. 70.0D0 ) THEN
                   XO    = DT(J)/D(I,J)
                   R(I)  = ((XO-ONE)/(XO*(DTLOG-DILOG)))**ALPH
               ELSE
C                  XO    = DT(J)/D(I,J)
                   R(I)  = (DTLOG-DILOG)**(-ALPH)
               ENDIF
            ENDIF
            RC    = RC+R(I)
  730     CONTINUE
          RC    = RC/ND
          K     = N0
          XN    = N0
          DR    = XN
          I     = K
  740  K     = K + N1
          DR    = DR+R(K)
          XO    = XN
          XN    = XI(K,J)
  750 IF(RC.GT.DR)GO TO 740
          I     = I + N1
          DR    = DR-RC
          XIN(I)= XN-(XN-XO)*DR/R(K)
      IF(I.LT.NDM)GO TO 750
          DO 760 I= N1,NDM
            XI(I,J)= XIN(I)
  760     CONTINUE
          XI(ND,J)= ONE
         ENDIF
  780   CONTINUE
C
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSGETW( WEIGHT )                                      *
C*========================                                             *
C*((Function))                                                         *
C*    Get Weight                                                       *
C*                                                                     *
C*    Coded   by T.Ishikawa    Jun. 1995 at KEK                        *
C*    Last update              Jun. 1995 at KEK                        *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSGETW( WEIGHT )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
*
*========= Save the grid information for the best accuracy ===========
*
      WEIGHT = WGT
C
      RETURN
      END
************************************************************************
*    ==================================                                *
      SUBROUTINE BSGRID( MDIM, IGG )
*    ==================================                                *
* ((Purpose))                                                          *
*     To change the grid optimizing flag.                              *
* ((Input))                                                            *
*     MDIM   : The number of dimension of integral                     *
*     IGG(i) : The flag switches whether the grid of i-th variable     *
*              is to be optimized ( 1 ) or kept uniform ( 0 ).         *
* ((Output))                                                           *
*     These parameters are to be set in the labeled common /BPARM1/    *
*                                                                      *
*        Coded by S.Kawabata         August '94                        *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL

      DIMENSION IGG(MDIM)

*=========================================================

      NDIM   = MDIM
      DO 100 I= 1, NDIM
         IG(I) = IGG(I)
  100 CONTINUE

       RETURN
       END
************************************************************************
*    ===============================                                   *
      SUBROUTINE BSHBOK( IOFSET )
*    ===============================                                   *
* ((Purpose))                                                          *
*      To write the ID-th histogram on the unit LUNIT.                 *
* ((Input))                                                            *
*      LUNIT: Logical unit number                                      *
*      ID   : Historgram ID                                            *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata   June '90 at KEK                                   *
*                                                                      *
************************************************************************

      REAL*8         SCALLS,WGT,TI,TSI,TACC
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      CHARACTER*80 TITLE

      COMMON/PLOTLU/ LU

C
      IF( NHIST .GT. 0 ) THEN

          NTOTAL= SCALLS

          DO 500 IHIST = 1, NHIST
             ID    = MAPL(1,IHIST)
             ID    = ID + IOFSET
             IP1   = MAPL(2,IHIST)
             XMIN  = BUFF(IP1)
             XMAX  = BUFF(IP1+1)
             NXBIN = IBUF(IP1+2)
             DEV   = BUFF(IP1+3)
             IP2   = MAPL(3,IHIST)
             IP3   = MAPL(4,IHIST)

*        WRITE(LU,9200) ID,LUNIT,(BUFF(I),I=IP3+1,IP3+15),
*    .              NTOTAL,NXBIN,DEV
*9200    FORMAT(/1H1,
*    .         1X,'** Histogram ID(',I5,' ) was saved in Unit(',I2,') **',
*    .         /1X,'Title : ',15A4,
*    .         /1X,'Entries     =',I10,
*    .         /1X,'No. of bins =',I10,'  Width =',G13.4)

             WRITE( TITLE, 9500) (BUFF(I), I=IP3+1,IP3+16)
 9500        FORMAT(16A4)

             CALL HBOOK1( ID, TITLE, NXBIN, XMIN, XMAX, 0.0 )

             IPF   = IP2 + 156
             IPF2  = IPF + 52
             FACT       = 1./(NTOTAL*DEV)
             DO 400 I = 1, NXBIN
                TX     = BUFF(I+IPF)
                NX     = IBUF(I+IP2)
                VLS    = TX*FACT
*               IF( NX .GT. 1 ) THEN
*                   DEV2   =  NX*BUFF(I+IPF2)-TX*TX
*                   IF( DEV2 .LE. 0.0 ) THEN
*                       VER = 0.0
*                   ELSE
*                       VER = FACT*SQRT( DEV2/( NX-1 ))
*                   ENDIF
*               ELSEIF( NX .EQ. 1 ) THEN
*                   VER = VLS
*               ELSE
*                   VER = 0.0
*               ENDIF
                XX     = XMIN + DEV*(FLOAT(I) - 0.5)

	        CALL HFILL( ID, XX, 0.0, VLS )
  400        CONTINUE

  500     CONTINUE

      ENDIF

      IF( NSCAT .GT. 0 ) THEN
         DO 900 ISCAT = 1, NSCAT

            IP3   = MAPD(4,ISCAT)

            WRITE( TITLE, 9500) (BUFF(I), I=IP3+2,IP3+17)

            ID    = MAPD(1,ISCAT)
            ID    = ID + IOFSET + 10000

            IP1   = MAPD(2,ISCAT)
            XL    = BUFF(IP1)
            XU    = BUFF(IP1+1)
            NX    = IBUF(IP1+2)
            DX    = BUFF(IP1+3)
            YL    = BUFF(IP1+4)
            YU    = BUFF(IP1+5)
            NY    = IBUF(IP1+6)
            DY    = BUFF(IP1+7)

            CALL HBOOK2( ID, TITLE, NX, XL, XU, NY, YL, YU, 0.0 )

            IP2   = MAPD(3,ISCAT)
            NTOTAL= IBUF(IP2)
            FACT       = 1./(NTOTAL*DX*DY)

            DO 300 L = 0, NY-1
               IB     = NX*L + IP2
               DO 200 I = 1,NX
                  VLS   = BUFF( I + IB )* FACT
                  XX    = XL + DX*(FLOAT(I) - 0.5)
                  YY    = YL + DY*(FLOAT(L) - 0.5)

                  CALL HFILL( ID, XX, YY, VLS )

  200          CONTINUE
  300       CONTINUE

  900    CONTINUE
      ENDIF

      RETURN
      END
***********************************************************************
*============================                                         *
      SUBROUTINE BSINFO( LU )
*============================                                         *
*((Purpose))                                                          *
*    Print the information for                                        *
*        (1) BASES parameters                                         *
*        (2) Computer time information                                *
*        (3) Convergency behavior of the Grid optimization step       *
*        (4) Convergency behavior of the integration step             *
*(( Input ))                                                          *
*    LU  :  Logical unit number of printer                            *
*                                                                     *
*           by S.Kawabata    March 1994 at KEK
*                                                                     *
***********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF

*  Print Title

      CALL BSPRNT( LU, 1, IDUM1, IDUM2 )

*  Print Bases parameters

      CALL BSPRNT( LU, 4, IDUM1, IDUM2 )

*  Print Computing time information

      CALL BSPRNT( LU, 3, IDUM1, IDUM2 )

*  Print Convergency Behaviors

      DO 100 ISTEP = 0, 1
         ITX  = ITG
         IF( ISTEP .EQ. 1 ) ITX = ITF

      IF( ITX .GT. 0 ) THEN

         CALL BSPRNT( LU, 8, ITX, ISTEP )

      ENDIF
  100 CONTINUE

      RETURN
      END
************************************************************************
*    ===================                                               *
      SUBROUTINE BSINIT
*    ===================                                               *
* ((Purpose))                                                          *
*     Initialization of BASE50/SPRING50.                               *
*     Function of this routine is                                      *
*       (0) Set the size of histogram and scatter plot buffers         *
*       (1) Set the parameters INTV and IPNT                           *
*             INTV = ( 0 / 1 / any )                                   *
*                  = ( Batch / Batch(Unix) / Interactive )             *
*             IPNT = ( 0 / any )                                       *
*                  = ( IBM Type / Ascii printer )                      *
*       (2) Set the acceleration factor ALPHA by 1.5                   *
*            The range of this value is from 0.0 to 2.0.               *
*            ALPHA = 0.0 results in no grid-optimization.              *
*       (3) Set the grid-optimization flag IGOPT ( Default value 0 )   *
*             IGOPT = 0  :  The grid is optimized by VEGAS algorithm   *
*             IGOPT = 1  :  The grid is optimized so that the accuracy *
*                           of each iteration be minimized.            *
*       (4) Set Node-ID number NODEID and the number of nodes NUMNOD   *
*       (5) Set seed of radom number                                   *
*       (6) Set the values of BASES paremeters with default ones.      *
*       (7) Set the values of parameters with non-sense values,        *
*            which should be set again with the true values by User    *
*            before running BASES.                                     *
*                                                                      *
*        Coded by S.Kawabata         March '94                         *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2

      COMMON /BASE0/ JFLAG,IBASES
      COMMON /BASE6/ D(NDMX,MXDIM),
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,IGOPT
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP
       COMMON/NINFO/ NODEID, NUMNOD
       COMMON /BDATE/ IDATE(3),ITIME(2)
*            IDATE(1) : year        ITIME(1) : hour
*            IDATE(2) : month       ITIME(2) : minute
*            IDATE(3) : day
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)
*=========================================================
* (0) Initialization of timer and Histogram buffer
*     Timer initialization
       CALL BSTIME( TIME0, 0 )
       TIMEB1 = TIME0
       TIMINT = 0

*     Histogram buffer initialization
       LU  = 6
       CALL BHINIT( LU )

*=========================================================

* (1) Set the parameters INTV and IPNT
       INTV  = 2
       IPNT  = 1
* (2) Set the acceleration factor ALPHA by 1.5
       ALPH  = 1.5D0
* (3) Set the grid-optimization flag IGOPT
       IGOPT = 0
* (4) Set Node-ID number NODEID and the number of nodes NUMNOD
*      IF( INTV .EQ. 0 ) THEN
           NODEID = 0
           NUMNOD = 1
*      ELSE
*          NODEID = 0
*          NUMNOD = 1
*      ENDIF

C---------------------------------------------------------------
C (5)  Set initial seeds of random number generator
C---------------------------------------------------------------
c       ISEED = 12345
C
c       CALL DRNSET( ISEED )
C ---------------------------------------------------------------
C (6),(7)  Set BASES parameters equal to default values
C ---------------------------------------------------------------
C
       NDIM   = -1
       NWILD  =  1
       ITMX1  = 15
       ITMX2  = 100
       NCALL  = 1000
c       ACC1   = 0.2D0
c       ACC2   = 0.01D0
       DO 100 I = 1,MXDIM
          IG(I) = 1
          XU(I)  = -1.0D37
  100  CONTINUE

*    Initialization of computing time table of BASES
       DO 200 I = 0, 2
          TIMEBS(I) = 0.0
  200  CONTINUE

*-------------------------------------------
*      Don't change IBASES from this value
*-------------------------------------------
       IBASES =  1

       RETURN
       END
***********************************************************************
*                                                                     *
*    ==========================                                       *
      SUBROUTINE BSINTG( FXN )
*    ==========================                                       *
*((Function))                                                         *
*    Subroutine performs N-dimensional Monte Carlo integration        *
*    for four vector generation of simulated events                   *
*                                                                     *
*       JFLAG = 0 ; First Trial of Defining Grid                      *
*       JFLAG = 1 ; First Trial of Data Accumulation                  *
*       JFLAG = 2 ; Second Trial of Defining Grid                     *
*       JFLAG = 3 ; Second Trial of Data Accumulation                 *
*                                                                     *
*    Coded   by S.Kawabata    July 1980 at DESY, Hamburg              *
*    Last update              March 1994                              *
*                                                                     *
***********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)

      EXTERNAL FXN
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE0/ JFLAG,IBASES
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
     .               ND,NG,NPG,MA(MXDIM)
      PARAMETER (ITM = 50)
      REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT
      COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),
     .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),
     .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)
      COMMON /BASE6/ D(NDMX,MXDIM),
     .               ALPH,XSAVE(NDMX,MXDIM),XTI,XTSI,XACC,ITSX
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,ITG,ITF
      CHARACTER*80 ERROR
      COMMON /BWARN1/ NERROR
      COMMON /BWARN2/ ERROR(3,3)
*
*        INTV = ( 0 / 1 / any ) = ( Batch / Batch(Unix) / Interactive )
*        IPNT = ( 0 / any ) = ( IBM Type / Ascii printer )
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      REAL*8  X(MXDIM)
      INTEGER KG(MXDIM),IA(MXDIM)

      COMMON/NINFO/ NODEID, NUMNOD
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)
C     REAL*8  TX(2)
      INTEGER NCNODE(2,512),NPNODE(2,512)
C     INTEGER NEFF(2)
*
*     Parameters for checking convergency
*
      DATA ACLMT,FC / 25.0D0, 5.0D0 /


      DATA  ONE/ 1.0D0/, ZERO/0.0D0/, LU / 6/
      DATA  N0/0/, N1/1/, HUNDRT/100.0D0/

************************************************************************
*                       Initialization Part
************************************************************************
*=======================================================================
*          Determine the number of hypercubes NSP
*=======================================================================

      XND     = ND
      NSP     = NG**NWILD
      XJAC    = 1.0D0
      DO  5 I = 1, NDIM
         XJAC = XJAC*DX(I)
    5 CONTINUE
      CALLS   = NSP*NPG
      DXG     = 1.0D0/NG
      DV2G    = DXG**(2*NWILD)/NPG/NPG/(NPG-1)
      DXG     = DXG*XND

      IF( NSP .EQ. 1 ) THEN
*=======================================================================
*           Determination of the number of sampling points
*               per node in the single hypercube case
*=======================================================================
          MEX     = MOD(NPG,NUMNOD)
          NPERCP  = NPG/NUMNOD
          NPGT    = 0
          DO  12 NODEX = 1,NUMNOD
             NPGS  = NPGT + 1
             NPGT  = NPGT + NPERCP
             IF( NODEX .LE. MEX ) NPGT = NPGT + 1
             NCNODE(1,NODEX) = 1
             NCNODE(2,NODEX) = 1
             NPNODE(1,NODEX) = NPGS
             NPNODE(2,NODEX) = NPGT
   12     CONTINUE
      ELSE
*=======================================================================
*          Determination of the number of hypercubes
*              per node in many hypercubes case
*=======================================================================
          MEX     = MOD(NSP,NUMNOD)
          NPERCP  = NSP/NUMNOD
          NSPT    = 0
          DO  15 NODEX = 1,NUMNOD
             NSPS  = NSPT + 1
             NSPT  = NSPT + NPERCP
             IF( NODEX .LE. MEX ) NSPT = NSPT + 1
             NCNODE(1,NODEX) = NSPS
             NCNODE(2,NODEX) = NSPT
             NPNODE(1,NODEX) = 1
             NPNODE(2,NODEX) = NPG
   15     CONTINUE
      ENDIF
*=======================================================================
      NEND    = N0
      ATACC   = ZERO
      NERROR  = N0
      NER1    = N0
      NER2    = N0
      NER3    = N0
      SUMTI   = ZERO
      SUMTSI  = ZERO

      IF(JFLAG .EQ. N0 .OR. JFLAG .EQ. N1 ) THEN
*-----------------------------------------------------------------------
*        JFLAG = 0  : The first trial of the grid optim. step
*        JFLAG = 1  : The first trial of the integration step
*-----------------------------------------------------------------------
         DO 10 J  = N1,NSP
           DXD(J) = ZERO
           DXP(J) = ZERO
   10    CONTINUE
*       -----------------
         ISTEP   = JFLAG
*       -----------------
         IT1   = N1
         SI    = ZERO
         SI2   = ZERO
         SWGT  = ZERO
         SCHI  = ZERO
*       =============
         CALL BHRSET
*       =============
         NSU     = N0
         SCALLS= ZERO
      ELSE
*-----------------------------------------------------------------------
*        JFLAG = 2  : The continuation of the grid optim. step
*        JFLAG = 3  : The continuation of the integration step
*-----------------------------------------------------------------------
C        IF( JFLAG .EQ. 2 ) THEN
*           -------------
C            ISTEP  = N0
*           -------------
C        ELSE
C    .   IF( JFLAG .EQ. 3 ) THEN
*           -------------
C            ISTEP  = N1
*           -------------
C        ELSE
C                *****************
C                      STOP
C                *****************
C         ENDIF
C
C         IT1   = IT + 1
      ENDIF

*------- Set the expected accuracy and the max. iteration number -------

      ITMX   = ITMX1
      ACC    = ACC1*0.01D0
      IF( ISTEP .EQ. N1 ) THEN
         ITMX = ITMX2
         ACC  = ACC2*0.01D0
      ENDIF

*-------- Print the title of the convergency behavior table -----------
*                  in the interactive mode
      IF( INTV .GT. 1 ) THEN
*         -----------------------------------
           CALL BSPRNT( LU, 5, ISTEP, IDUM2 )
*         -----------------------------------
      ENDIF
      NEGFLG     = 0

*    =====================
      CALL BSUTIM( 0, 2 )
*    =====================

*********************************************************************
*               Main Integration Loop
*********************************************************************
*    ========
      DO 500  IT = IT1,ITMX
*    ========
*=======================================================================
*                 Initialization for the iteration
*=======================================================================

         SCALLS  = SCALLS + CALLS
         NGOOD   = N0
         NEGTIV  = N0
         TI      = ZERO
         TSI     = TI

         IF( ISTEP .EQ. N0 ) THEN
             DO 200 J= N1,NDIM
             DO 200 I=1,ND
                D(I,J)= TI
  200        CONTINUE
         ENDIF

         NODEX  = NODEID
         IF( NODEID .EQ. 0 )  NODEX = NUMNOD

*---------------------------------------------------------------------
*        Distributing hyper cubes to NumNode nodes
*           NCNODE(1,NODEX)   : 1st cube number for the node NODEX
*           NCNODE(2,NODEX)   : Last cube number for the node NODEX
*                    NODEX    : node number 1 => NumNode(=0)
*                    NODEX    : node number 1 => NumNode(=0)
*---------------------------------------------------------------------

         NSP1  = NCNODE(1,NODEX)
         NSP2  = NCNODE(2,NODEX)
*                                 Dummy loopfor a parallel processor
C                                 IF( NSP1 .GT. 1 ) THEN
C                                     CALL DRLOOP( NDIM*NPG*(NSP1-1) )
C                                 ENDIF

*=====================================================================
*      Loop for hypercube from NSP1 to NSP2 in the NodeX-th node
*=====================================================================
*       ========
         DO 400 NCB = NSP1, NSP2
*       ========
            FB      = 0.0
            F2B     = 0.0
            NP      = NCB - 1
            IF( NWILD .GT. 1 ) THEN
                DO 210 J = 1,NWILD-1
                   NUM   = MOD(NP,MA(J+1))
                   KG(J) = NUM/MA(J) + 1
  210           CONTINUE
            ENDIF
            KG(NWILD)     = NP/MA(NWILD) + 1

*---------------------------------------------------------------------
*       If number of hypercubes is only one,
*        Distributing sampling points to NumNode nodes
*           NPNODE(1,NODEX)   : 1st sample point for the node NODEX
*           NPNODE(2,NODEX)   : Last sample point for the node NODEX
*                    NODEX    : node number 1 => NumNode(=0)
*---------------------------------------------------------------------

            NPG1  = NPNODE(1,NODEX)
            NPG2  = NPNODE(2,NODEX)
*                                 Dummy loop for a parallel processor
C                                 IF( NPG1 .GT. 1 ) THEN
C                                     CALL DRLOOP( NDIM*(NPG1-1) )
C                                 ENDIF

*=====================================================================
*          Loop for sampling points from NPG1 to NPG2
*                in the single hypercube case
*=====================================================================
*          ========
            DO 300 NTY = NPG1,NPG2
*          ========
*---------------------------------------------------------------------
*        Determine the integration variables by random numbers
*---------------------------------------------------------------------

               WGT   = XJAC
               DO 250 J= 1,NDIM
                  IF( J .LE. NWILD ) THEN
                      XN  = (KG(J)-DRN(IDUMY))*DXG+1.D0
                  ELSE
                      XN  = ND*DRN(IDUMY)+1.D0
                  ENDIF
                  IA(J)   = XN
                  IAJ     = IA(J)
                  IF( IAJ .EQ. 1) THEN
                      XO  = XI(IAJ,J)
                      RC  = (XN-IA(J))*XO
                  ELSE
                      XO  = XI(IAJ,J)-XI(IAJ-1,J)
                      RC  = XI(IAJ-1,J)+(XN-IAJ)*XO
                  ENDIF
                  X(J)    = XL(J)+RC*DX(J)
                  WGT     = WGT*XO*XND
  250          CONTINUE
*-----------------------------------------------------------------------
*                     =======
               FXG  =  FXN(X)*WGT
*                     =======
*-----------------------------------------------------------------------
*             Check the value of the integrand
*-----------------------------------------------------------------------

               IF( FXG .NE. 0.0 ) THEN
                   NGOOD = NGOOD + 1
                   IF( ISTEP .EQ. 1 ) THEN
                       DXD(NCB) = DXD(NCB) + FXG
                       IF( FXG .GT. DXP(NCB) ) DXP(NCB) = FXG
                   ENDIF
                   IF( FXG .LT. 0.0 ) THEN
                       NEGTIV= NEGTIV+ 1
                       IF( NEGFLG .EQ. 0 ) THEN
                          WRITE(6,9200) IT,NODEID
 9200                     FORMAT(1X,
     .                       '******* WARNING FROM BASES ********',
     .                       '***********',
     .                       /1X,'*  Negative FUNCTION at IT =',I3,1X,
     .                       ', node = ',I3,1X,'*',
     .                       /1X,'***********************************',
     .                       '***********')
                          NEGFLG  = 1
                       ENDIF
                   ENDIF
               ENDIF

*-----------------------------------------------------------------------
*              Accumulation of FXG and FXG*FXG
*-----------------------------------------------------------------------

               F2    = FXG*FXG
               FB    = FB + FXG
               F2B   = F2B + F2

               IF( ISTEP .EQ. 0 ) THEN
                   DO 260  J = 1,NDIM
                      D(IA(J),J)= D(IA(J),J)+F2
  260              CONTINUE
               ENDIF
*======
  300       CONTINUE
*======
*------------------------------------------- for a parallel processor
*                                 Dummy loop for a parallel processor
C                                 IF( NPG2 .LT. NPG ) THEN
C                                     CALL DRLOOP(NDIM*(NPG-NPG1))
C                                 ENDIF
*                                 Global sum of FB and F2B
C                                 IF( NSP .EQ. 1 ) THEN
C                                     CALL BSDSUM(  FB, 1 )
C                                     CALL BSDSUM( F2B, 1 )
C                                 ENDIF
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*         Calculate the estimate and variance in the hypercube
*-----------------------------------------------------------------------

            F2B   = DSQRT(F2B*NPG)
            F2S   = (F2B-FB)*(F2B+FB)
            TI    = TI+FB
            TSI   = TSI + F2S

*======
  400    CONTINUE
*======
*------------------------------------------- for a parallel processor
*                                 Dummy loop
C                                 IF( NSP2 .LT. NSP ) THEN
C                                     CALL DRLOOP(NDIM*NPG*(NSP-NSP2))
C                                 ENDIF

*                                 Global sum of efficiency and frequency
*                                     of negative valued function
C                                 NEFF(1) = NGOOD
C                                 NEFF(2) = NEGTIV
C                                 CALL BSISUM( NEFF, 2 )

C                                 TX(1) = TI
C                                 TX(2) = TSI
C                                 IF( NSP .EQ. 1 ) THEN
C                                     CALL BSDSUM(   TX, 2 )
C                                 ENDIF

*                                 Global sum of grid information
C                                 IF( ISTEP .EQ. 0 ) THEN
C                                     NOWORK = NDMX*NDIM
C                                     CALL BSDSUM(    D, NOWORK )
C                                 ENDIF

*=====================================================================
*           Compute Result of this Iteration
*=====================================================================
*--------------------------------------------------------------------
*           Accumulate the histogram entries
*--------------------------------------------------------------------
*       -------------
         CALL BHSAVE
*       -------------
*--------------------------------------------------------------------

C        TI     = TX(1)
C        TSI    = TX(2)
C        NGOOD  = NEFF(1)
C        NEGTIV = NEFF(2)

         TI    = TI/CALLS
         TSI   = TSI*DV2G
**
         IF( TSI .LE. 1.0D-37 ) TSI = 1.0D-37
**
         TI2   = TI*TI

         IF( NGOOD .LE. 10 ) THEN
*           --------------------------------
             CALL BSPRNT( LU, 9, IDUM1, IDUM2 )
*           --------------------------------
*            *****************
                   STOP
*            *****************

         ENDIF

*--------------------------------------------------------------------
*               Calculate the cumulative result
*--------------------------------------------------------------------

         WGT   = ONE/TSI
         SI    = SI+TI*WGT
         SWGT  = SWGT+WGT
         SCHI  = SCHI+TI2*WGT
         AVGI  = SI/SWGT
         CHI2A = ZERO
         IF(IT .GT. N1 ) CHI2A = (SCHI - SI*AVGI)/(IT-.999D0)
         SD    = DSQRT(ONE/SWGT)

*---------------------------------------------------------------------
*             Save the results in the buffer
*---------------------------------------------------------------------

         TSI   = DSQRT(TSI)
         ITX         = MOD( IT, ITM)
         IF( ITX .EQ. 0 ) ITX = ITM
         ITRAT(ITX,ISTEP)  = IT
         EFF  (ITX,ISTEP)  = NGOOD/CALLS*HUNDRT
         WRONG(ITX,ISTEP)  = NEGTIV/CALLS*HUNDRT
         RESLT(ITX,ISTEP)  = AVGI
         ACSTD(ITX,ISTEP)  = SD
         TRSLT(ITX,ISTEP)  = TI
         TACC              = ABS(TSI/TI*HUNDRT)
         TSTD (ITX,ISTEP)  = TACC
         PCNT (ITX,ISTEP)  = ABS(SD/AVGI*HUNDRT)

*----------------------------------------------------------------------
*                  Check cumulative accuracy
*----------------------------------------------------------------------

         IF( NODEID .EQ. 0 ) THEN

*-------------------  Check cumulative accuracy -----------------------

             SDAV  = SD/AVGI
             IF((ABS(SDAV) .LE. ACC)) NEND = N1

             IF( ISTEP .EQ. N1 ) THEN
                 IF( TACC .GT. ACLMT ) THEN
                     IF( NER1 .EQ. 0 ) THEN
                         NERROR = NERROR + 1
                         WRITE(ERROR(1,NERROR),9900) NERROR,IT,ACLMT
 9900                    FORMAT('* (',I1,') Temp. accuracy of it-#',
     .                         I3,' is too large comparing to',
     .                         F6.2,' percent.',6X,'*')
                         WRITE(ERROR(2,NERROR),9901) TACC,ACLMT
 9901                    FORMAT('*',8X,'Temp. accuracy (',
     .                         F7.4,' % )  >>   (',
     .                         F7.4,' % )',23X,'*')
                         WRITE(ERROR(3,NERROR),9902)
 9902                    FORMAT('*',77X,'*')
                         NER1  = 1
                     ENDIF
                 ENDIF
                 IF( IT .GT. 1 ) THEN
                     IF(( TI .GT. AVTI+FDEVI ) .OR.
     .                  ( TI .LT. AVTI-FDEVI )      ) THEN
                          IF( NER2 .EQ. 0 ) THEN
                              NERROR = NERROR + 1
                              WRITE(ERROR(1,NERROR),9910) NERROR,IT,FC
 9910                         FORMAT('* (',I1,') Temp. estimate of ',
     .                        'it-#',I3,' fluctuates more than ',
     .                               F4.1,'*average-sigma.',6X,'*')
                              RE = TI
*patch TI:1995/08/25
                              ARE = ABS(RE)
*old                          CALL BSORDR( RE, FX2, ORDER, IORDR )
                              CALL BSORDR( ARE, FX2, ORDER, IORDR )
*patch end
                              RE = TI/ORDER
                              RE1 = AVTI
                              AC  = FDEVI
*patch TI:1995/08/25
                              ARE1 = ABS(AVTI)
                              AAC  = ABS(FDEVI)
                              IF( ARE1 .GE. AAC ) THEN
                                  CALL BSORDR( ARE1, FX2, ORDR1, IORDR1)
                              ELSE
                                  CALL BSORDR( AAC, FX2, ORDR1, IORDR1)
                              ENDIF
*                             IF( RE1 .GE. AC ) THEN
*                                 CALL BSORDR( RE1, FX2, ORDR1, IORDR1)
*                             ELSE
*                                 CALL BSORDR( AC, FX2, ORDR1, IORDR1)
*                             ENDIF
*patch end
                              RE1 = AVTI/ORDR1
                              AC  = AC/ORDR1
                              WRITE(ERROR(2,NERROR),9911) RE,IORDR,
     .                                          RE1,AC,IORDR1
 9911                         FORMAT('*        Temp. Estimate (',
     .                         F10.6,' E',I3,')  >  (',F10.6,'+',F8.6,
     .                         ' ) E',I3,', or',1X,'*')
                              WRITE(ERROR(3,NERROR),9912) RE,IORDR,
     .                                          RE1,AC,IORDR1
 9912                         FORMAT('*        Temp. Estimate (',
     .                         F10.6,' E',I3,')  <  (',F10.6,'-',F8.6,
     .                         ' ) E',I3,5X,'*')
                              NER2 = 1
                          ENDIF
                     ENDIF
                     IF( TSI .GT. FDEVI ) THEN
                         IF( NER3 .EQ. 0 ) THEN
                             NERROR = NERROR + 1
                             WRITE(ERROR(1,NERROR),9920) NERROR,IT,FC
 9920                        FORMAT('* (',I1,') Error of it-#',
     .                              I3,' fluctuates more than',F4.1,
     .                              '*average-sigma.',16X,'*')
                             RE1 = TSI
*patch TI:1995/08/25
                             ARE1 = ABS(TSI)
*                            CALL BSORDR( RE1, FX2, ORDER, IORDR)
                             CALL BSORDR( ARE1, FX2, ORDER, IORDR)
*patch end;
                             RE1 = TSI/ORDER
                             AC  = FDEVI
*patch TI:1995/08/25
                             AAC  = ABS(FDEVI)
*                            CALL BSORDR( AC, FX2, ORDR1, IORDR1)
                             CALL BSORDR( AAC, FX2, ORDR1, IORDR1)
*patch end;
                             AC  = AC/ORDR1
                             WRITE(ERROR(2,NERROR),9921) RE1,IORDR,
     .                                         AC,IORDR1
 9921                        FORMAT('*        Temp. Error (',
     .                         F10.6,' E',I3,')  >  (',F10.6,
     .                         ' E',I3,')',18X,'*')
                             WRITE(ERROR(3,NERROR),9902)
                             NER3  = 1
                         ENDIF
                     ENDIF
                 ENDIF
                 SUMTSI = SUMTSI + TSI
                 SUMTI  = SUMTI  + TI
                 AVTSI  = SUMTSI/FLOAT(IT)
                 AVTI   = SUMTI/FLOAT(IT)
                 FDEVI  = FC*AVTSI
             ENDIF
         ENDIF

*------------------------------------------- for a parallel processor

*                                  Broadcast
C                                  CALL BSCAST( NEND, 1 )

*----------------------------------------------------------------------
*        Smoothing the Distribution D(I,J) and refine the grids
*----------------------------------------------------------------------

         IF( ISTEP .LE. N0 ) THEN
             IF( IT .EQ. ITMX ) NEND = N1
*           ---------------------
             CALL BSETGV( NEND )
*           ---------------------
         ENDIF
*       ==========================
         CALL BSUTIM( 0, ISTEP )
*       ==========================

         TIME (ITX,ISTEP)  = TIMINT
         STIME             = TIMINT

*---- Print the convergency behavior table in the interactive mode ----
         IF( INTV .GT. 1 ) THEN
*            ---------------------------------
              CALL BSPRNT ( LU, 6, ISTEP, IDUM2 )
*            ---------------------------------
         ENDIF

         IF( NEND .EQ. N1 ) GO TO 600

*       ======================
         CALL BSUTIM( 0, 2 )
*       ======================
*======
  500 CONTINUE
*======
      IT    = IT - N1
      NEND  = N1

***********************************************************************
*                   Termination of BASES
***********************************************************************
*======
  600 CONTINUE
*======
*---------------------------------------------- For a parallel computer

*                                 Global sum of histograms
C                                 CALL BHSUM
*                                 Global sum of probabilities
C                                 CALL BSDSUM(  DXD, NSP )
*                                 Global sum of the max.value in each HC
C                                 CALL BSDSUM(  DXP, NSP )


*======================= End of the step ? ============================

      IF( NEND .EQ. N1 ) THEN
          IF( INTV .GT. 1 ) THEN
*            ---------------------------------
              CALL BSPRNT ( LU, 7, IDUM1, IDUM2 )
*            ---------------------------------
          ENDIF
          IF( ISTEP .EQ. N0) THEN
              JFLAG   = N1
              ITG     = IT
          ELSE
              JFLAG   = N0
              ITF     = IT
          ENDIF
      ENDIF
*    ======================
       CALL BSUTIM( 0, 2 )
*    ======================

      RETURN
      END
***********************************************************************
*    ===================================                              *
      SUBROUTINE BSLIST( LU, I, ISTEP )
*    ===================================                              *
* ((purpose))                                                         *
*     Print out results of each iteration and cumulative result       *
* ((Argument))                                                        *
*  (Input)                                                            *
*     LU      : Logical unit number for the printer                   *
*     I       : Address in the arrays of common /BASE5/               *
*     ISTEP   : The Set-Identifier                                    *
*               ISTEP = ( 0 / 1 ) = ( Grid opt. / Integration step )  *
*                                                                     *
*     S. Kawabata   March '94                                         *
***********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (ITM = 50)
      REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT
      COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),
     .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),
     .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)

      CALL BSTCNV( TIME(I,ISTEP), IH, MN, IS1, IS2 )

      RE  = RESLT(I,ISTEP)
      AC  = ABS(ACSTD(I,ISTEP))
      ARE = ABS(RE)
      IF( ARE .GE. AC) THEN
          CALL BSORDR( ARE, F2, ORDER, IORDR)
      ELSE
          CALL BSORDR(  AC, F2, ORDER, IORDR )
      ENDIF
      RE  = RE/ORDER
      AC  = AC/ORDER
      IEFF = EFF(I,ISTEP)
      WRITE(LU,9631) ITRAT(I,ISTEP),IEFF,WRONG(I,ISTEP),
     .              TRSLT(I,ISTEP),TSTD(I,ISTEP),
     .              RE,AC,IORDR,PCNT(I,ISTEP),IH,MN,IS1,IS2
 9631 FORMAT(I4,I4,F6.2,1P,E11.3, 0P,1X,F6.3,
     .              F10.6,'(+-',F8.6,')E',I3.2,1X,F6.3,
     .          1X,I3,':',I2,':',I2,'.',I2.2)


      RETURN
      END
C***********************************************************************
C*                                                                     *
C*=============================================                        *
C*    SUBROUTINE BSORDR( VAL, F2, ORDER, IORDR)                        *
C*=============================================                        *
C*((Function))                                                         *
C*    To resolve the real number VAL into mantester and exponent parts.*
C*  When VAL = 1230.0 is given, output are                             *
C*        F2 = 1.2  and ORDER = 4.0.                                   *
C*((Input))                                                            *
C*  VAL  : Real*8 value                                                *
C*((Output))                                                           *
C*  F2   : The upper two digits is given                               *
C*  ORDER: Order is given                                              *
C*  IORDR: Exponent is given                                           *
C*((Author))                                                           *
C*  S.Kawabata                                                         *
C*                                                                     *
C***********************************************************************

      SUBROUTINE BSORDR(VAL, F2, ORDER, IORDR)
      IMPLICIT REAL*8 (A-H,O-Z)

      IF( VAL .NE. 0.0 ) THEN
          ORDER    =  LOG10( VAL )
          IORDR    =  INT( ORDER )
          IF( ORDER .LT. 0.0D0 ) IORDR = IORDR - 1
          ORDER  = 10.D0**IORDR
          F2     = VAL/ORDER
      ELSE
          IORDR  = 0
          ORDER  = 1.0D0
          F2    = 0.0D0
      ENDIF

      RETURN
      END
************************************************************************
*    ================================================                  *
      SUBROUTINE BSPARM( MCALL, AC1, AC2, IT1, IT2 )
*    ================================================                  *
* ((Purpose))                                                          *
*     To set the BASES parameters.                                     *
* ((Input))                                                            *
*     MCALL  : The number of sample points per iteration.              *
*              This actual number is to be determined by taking the    *
*              number of dimensions into account.                      *
*     AC1 %  : The required accuracy at the grid optimization step     *
*     AC2 %  : The required accuracy at the integration step.          *
*     IT1    : The max. number of iteration at the grid opt. step.     *
*     IT2    : Thr max. number of iteration at the integration step.   *
* ((Output))                                                           *
*     These parameters are set in the labeled common /BPARM1/ and      *
*     /BPARM2/.
*                                                                      *
*        Coded by S.Kawabata         August '94                        *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2

      INTEGER MCALL, IT1, IT2
      REAL*8 AC1, AC2

      NCALL = MCALL
      ACC1  = AC1
      ACC2  = AC2
      ITMX1 = IT1
      ITMX2 = IT2

      RETURN
      END
***********************************************************************
*    =======================================                          *
      SUBROUTINE BSPRNT( LU, ID, IP1, IP2 )
*    =======================================                          *
* ((purpose))                                                         *
*     Print out routine of BASES.                                     *
*  (Argument)                                                         *
*     ID  : Identity number of printouts.                             *
*     IP1... IP2 : Integer                                            *
*  (Author)                                                           *
*     S. Kawabata   May 1992                                          *
*     Last update   March 1994                                        *
***********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE0/ JFLAG,IBASES
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
     .               ND,NG,NPG,MA(MXDIM)
      PARAMETER (ITM = 50)
      REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT
      COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),
     .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),
     .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)
      REAL*4 STIME
      COMMON /BSRSLT/AVGI,SD,CHI2A,STIME,IT1,ITF
      CHARACTER*51 ICH(0:1)
      CHARACTER*1 CN
*        INTV = ( 0 / 1 / any ) = ( Batch / Batch(Unix) / Interactive )
*        IPNT = ( 0 / any ) = ( IBM Type / Ascii printer )
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP
*
       COMMON /BDATE/ IDATE(3),ITIME(2)
*            IDATE(1) : year        ITIME(1) : hour
*            IDATE(2) : month       ITIME(2) : minute
*            IDATE(3) : day
      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)
      REAL*4 XTIME
*
       COMMON/NINFO/ NODEID, NUMNOD
*
      DATA  ICH / 'Convergency Behavior for the Grid Optimization Step',
     .            'Convergency Behavior for the Integration Step      '/

      IF( NODEID .NE. 0 ) RETURN
      CN = CHAR(12)

      GO TO ( 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ), ID
C----------------------------------------------------------- BSMAIN

  100 IF( IPNT .EQ. 0 ) THEN
          WRITE(LU,9600)
 9600     FORMAT(/1H1,/1H )
      ELSE
          WRITE(LU,9610) CN
 9610     FORMAT(A1)
      ENDIF
      WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
 9620 FORMAT(55X,'Date: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
      WRITE(LU,9050)
 9050 FORMAT(
     . 8X,'**********************************************************',
     ./8X,'*                                                        *',
     ./8X,'*     BBBBBBB     AAAA     SSSSSS   EEEEEE   SSSSSS      *',
     ./8X,'*     BB    BB   AA  AA   SS    SS  EE      SS    SS     *',
     ./8X,'*     BB    BB  AA    AA  SS        EE      SS           *',
     ./8X,'*     BBBBBBB   AAAAAAAA   SSSSSS   EEEEEE   SSSSSS      *',
     ./8X,'*     BB    BB  AA    AA        SS  EE            SS     *',
     ./8X,'*     BB    BB  AA    AA  SS    SS  EE      SS    SS     *',
     ./8X,'*     BBBB BB   AA    AA   SSSSSS   EEEEEE   SSSSSS      *',
     ./8X,'*                                                        *',
     ./8X,'*                   BASES Version 5.1                    *',
     ./8X,'*           coded by S.Kawabata KEK, March 1994          *',
     ./8X,'**********************************************************')

          RETURN
C----------------------------------------------------------- BSMAIN

  200     IF( IPNT .EQ. 0 ) THEN
              WRITE(LU,9600)
          ELSE
              WRITE(LU,9610) CN
          ENDIF
          WRITE(LU,9300)
 9300     FORMAT(20X,
     .         '****** END OF BASES *********')

C----------------------------------------------------------- BSMAIN

  300 CONTINUE
      WRITE(LU,9305)
 9305 FORMAT(
     .//5X,'<<   Computing Time Information   >>')

*     WRITE(LU,9310) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
*9310 FORMAT(/15X,'Start at: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
*     CALL BSDATE
*     WRITE(LU,9320) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
*9320 FORMAT(15X,'End   at: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
      WRITE(LU,9330)
 9330 FORMAT(/15X,'(1) For BASES              H: M:  Sec')
      CALL BSTCNV(TIMEBS(2),IH,MN,IS1,IS2)
      WRITE(LU,9340) IH, MN, IS1, IS2
 9340 FORMAT(19X,'Overhead           : ',I3,':',I2,':',I2,'.',I2.2)
      CALL BSTCNV(TIMEBS(0),IH,MN,IS1,IS2)
      WRITE(LU,9350) IH, MN, IS1, IS2
 9350 FORMAT(19X,'Grid Optim. Step   : ',I3,':',I2,':',I2,'.',I2.2)
      CALL BSTCNV(TIMEBS(1),IH,MN,IS1,IS2)
      WRITE(LU,9360) IH, MN, IS1, IS2
 9360 FORMAT(19X,'Integration Step   : ',I3,':',I2,':',I2,'.',I2.2)
      XTIME = TIMEB2 - TIMEB1
      CALL BSTCNV(XTIME,IH,MN,IS1,IS2)
      WRITE(LU,9365) IH, MN, IS1, IS2
 9365 FORMAT(19X,'Go time for all    : ',I3,':',I2,':',I2,'.',I2.2)
      EXTIM  = TIMEBS(1)*1000.0/SCALLS/0.7
      WRITE(LU,9375)
 9375 FORMAT(/15X,'(2) Expected event generation time')
      WRITE(LU,9376) EXTIM
 9376 FORMAT(19X,'Expected time for 1000 events :',F10.2,' Sec')
      RETURN

C----------------------------------------------------------- BASES

  400 NSP   = NG**NWILD
      MCALL = NSP*NPG
      WRITE(LU,9400) NDIM,NWILD,MCALL,NCALL,ND,NG,NSP
 9400 FORMAT(
     .//5X,'<<   Parameters for BASES    >>',
     .//5X,' (1) Dimensions of integration etc.',
     . /5X,'     # of dimensions :    Ndim    =',I9,3X,'( 50 at max.)',
     . /5X,'     # of Wilds      :    Nwild   =',I9,3X,'( 15 at max.)',
     . /5X,'     # of sample points : Ncall   =',I9,'(real)',
     .                                         I9,'(given)',
     . /5X,'     # of subregions    : Ng      =',I9,' / variable',
     . /5X,'     # of regions       : Nregion =',I9,' / variable',
     . /5X,'     # of Hypercubes    : Ncube   =',I9,
     .//5X,' (2) About the integration variables')
      WRITE(LU,9405)
 9405 FORMAT(10X,'------',2('+---------------'),'+-------+-------')
      WRITE(LU,9410)
 9410 FORMAT(10X,'    i       XL(i)           XU(i)     ',
     .           '  IG(i)   Wild')
      WRITE(LU,9405)
       DO 450 I = 1,NDIM
          IF( I .LE. NWILD ) THEN
          WRITE(LU,9420) I,XL(I),XU(I),IG(I)
 9420     FORMAT(10X,I5,1P,2('  ',E14.6),'  ',3X,0P,I1,3X,
     .                       '   yes')
          ELSE
          WRITE(LU,9421) I,XL(I),XU(I),IG(I)
 9421     FORMAT(10X,I5,1P,2('  ',E14.6),'  ',3X,0P,I1,3X,
     .                        '    no')
          ENDIF
  450  CONTINUE
       WRITE(LU,9405)
       WRITE(LU,9450) ITMX1,ACC1,ITMX2,ACC2
 9450  FORMAT(
     . /5X,' (3) Parameters for the grid optimization step',
     . /5X,'     Max.# of iterations: ITMX1 =',I9,
     . /5X,'     Expected accuracy  : Acc1  =',F9.4,' %',
     .//5X,' (4) Parameters for the integration step',
     . /5X,'     Max.# of iterations: ITMX2 =',I9,
     . /5X,'     Expected accuracy  : Acc2  =',F9.4,' %')

          RETURN
C----------------------------------------------------------- BASES

  500    IF( INTV .LE. 1 )    RETURN
         ISTEP  = IP1
         IF( IPNT .EQ. 0 ) THEN
             WRITE(LU,9600)
         ELSE
             WRITE(LU,9610) CN
         ENDIF
         WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
         WRITE(LU,9500) ICH(ISTEP)
 9500    FORMAT(15X,A)
         WRITE(LU,9570)
         WRITE(LU,9550)
 9550    FORMAT(1X,'<- Result of  each iteration ->',
     .          2X,'<-     Cumulative Result     ->',
     .          1X,'< CPU  time >',
     .         /1X,' IT Eff R_Neg   Estimate  Acc %',
     .          2X,'Estimate(+- Error )order  Acc %',
     .          1X,'( H: M: Sec )')
         WRITE(LU,9570)
 9570    FORMAT(1X,7('----------'),'--------')
         RETURN

C----------------------------------------------------------- BASES

  600    IF( INTV .LE. 1 ) RETURN
         ISTEP  = IP1
         ITX = MOD( IT, ITM)
         IF( ITX .EQ. 0 ) ITX = ITM

         CALL BSLIST( LU, ITX, ISTEP )

         RETURN

  700    IF( INTV .LE. 1 ) RETURN
         WRITE(LU,9570)

         RETURN
C----------------------------------------------------------- BASES

  800    ITJ    = IP1
         ISTEP  = IP2
         ITX  = MOD( ITJ, ITM )
         IF( ITX .EQ. 0 ) ITX = ITM

         IF( ITRAT(1,ISTEP) .EQ. 1 ) THEN
             NDEV   = 1
         ELSE
             NDEV   = 2
             ITFN   = ITM
             ITMN   = 10000
             DO 610 I = 1,ITM
                IF( ITRAT(I,ISTEP) .LT. ITMN ) THEN
                    ITST = I
                    ITMN = ITRAT(I,ISTEP)
                ENDIF
  610        CONTINUE
             IF( ITST .EQ. 1 ) NDEV = 1
         ENDIF

         IF( IPNT .EQ. 0 ) THEN
             WRITE(LU,9600)
         ELSE
             WRITE(LU,9610) CN
         ENDIF
         WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
         WRITE(LU,9500) ICH(ISTEP)
         WRITE(LU,9570)
         WRITE(LU,9550)
         WRITE(LU,9570)

  625    IF( NDEV .EQ. 1 ) THEN
             ITST = 1
             ITFN = ITX
         ENDIF

         DO 650 I = ITST, ITFN

            CALL BSLIST( LU, I, ISTEP )

  650    CONTINUE
         NDEV  = NDEV - 1
         IF( NDEV .GT. 0 ) GO TO 625
         WRITE(LU,9570)

      RETURN

C----------------------------------------------------------- BASES

  900 WRITE(LU,9950)
 9950 FORMAT(1X,'******** FATAL ERROR IN BASES **************',
     .      /1X,'There are no enough good points in this iteration.',
     .      /1X,'Process was terminated due to this error.')

      RETURN

C-----------------------------------------------------------------
 1000 LOOP = IP1
      IF( IP2 .NE. 0 ) THEN
          IF( IPNT .EQ. 0 ) THEN
              WRITE(LU,9600)
           ELSE
              WRITE(LU,9610) CN
           ENDIF
           WRITE(LU,9620) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
           WRITE(LU,9650)
 9650      FORMAT(
     .      20X,'Results of Integration',
     .     /10X,5('----------'),'------',
     .     /10X,' Loop#  Estimate(+- Error )order',
     .                     '  It1  It2 ( H: M: Sec )',
     .     /10X,5('----------'),'------')
      ENDIF

      RE  = AVGI
      AC  = ABS(SD)
      ARE = ABS(RE)
      IF( ARE .GE. AC) THEN
          CALL BSORDR( ARE, F2, ORDER, IORDR)
      ELSE
          CALL BSORDR(  AC, F2, ORDER, IORDR )
      ENDIF
      RE  = RE/ORDER
      AC  = AC/ORDER
      CALL BSTCNV( STIME, IH, MN, IS1, IS2)
      WRITE(LU,9660) LOOP,RE,AC,IORDR,IT1,IT,IH,MN,IS1,IS2
 9660 FORMAT(10X,I6,F10.6,'(+-',F8.6,')E',I3.2,2I5,
     .        1X,I3,':',I2,':',I2,'.',I2.2,
     .      /10X,5('----------'),'------')

      RETURN
      END
C***********************************************************************
C*                                                                     *
C*========================                                             *
C*    SUBROUTINE BSPUTW( WEIGHT )                                      *
C*========================                                             *
C*((Function))                                                         *
C*    Put Weight                                                       *
C*                                                                     *
C*    Coded   by T.Ishikawa    Jun. 1995 at KEK                        *
C*    Last update              Jun. 1995 at KEK                        *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSPUTW( WEIGHT )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
*
*========= Save the grid information for the best accuracy ===========
*
      WGT = WEIGHT
C
      RETURN
      END
************************************************************************
*                                                                      *
*    ==========================                                        *
      SUBROUTINE BSREAD( LUN )
*    ==========================                                        *
* ((Function))                                                         *
*     Read temporary result from the logocal unit LUN                  *
* ((Auther))                                                           *
*     S.Kawabata    June '90 at KEK                                    *
*                                                                      *
************************************************************************


      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ ND1(5*MXDIM+3)
*     COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
*    .               IG(MXDIM),NCALL
C     COMMON /BASE2/ ND2(6)
*     COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      COMMON /BASE3/ ND3(11)
*     COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
      COMMON /BASE4/ ND4(2*MXDIM*(NDMX+1)+4*LENG+MXDIM+3)
*     COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
*    .               ND,NG,NPG,MA(MXDIM)
      PARAMETER (ITM  = 50 )
*     COMMON /BASE5/ ND5(22*ITM)
      COMMON /BASE5/ ND5(23*ITM)
*     REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT
*     COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),
*    .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),
*    .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)
      COMMON /RANDM/ ND6(45)

      PARAMETER ( NHS = 50, NSC = 50 )
      COMMON /PLOTH/ NPH(18*(NHS+NSC)+29),NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
*     INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
*     COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
*    .              NHIST, MAPL(4,NHS),
*    .              NSCAT, MAPD(4,NSC),
*    .              NW

      COMMON/NINFO/ NODEID, NUMNOD

      IF( NODEID .NE. 0 ) RETURN

      REWIND LUN
      READ(LUN) ND1,ND3,ND4,ND5,ND6,NPH
C     READ(LUN) ND1,ND2,ND3,ND4,ND5,ND6,NPH

      READ(LUN) NW,(IBUF(I),I=1,NW)
C
      RETURN
      END
************************************************************************
*=================================================
      SUBROUTINE BSTCNV( TIME, IH, MN, IS1, IS2 )
*=================================================
* (Purpose)
*    Resolve TIME in second into IH, MN, IS1, IS2
* (Input)
*    TIME : in the unit of second
* (Output)
*    IH   : Hours
*    MN   : Minute
*    IS1  : Second
*    IS2  : 0.xx Second
* (Author)
*    S.Kawabata 1992 June 15
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 TIME
      INTEGER  HOUR
      DATA HOUR, MINUT, N100/ 360000, 6000, 100 /

      ISEC  = TIME*N100
      IH    = 0
      MN    = IH
      IF( ISEC .GE. MINUT ) THEN
          ITIME = ISEC
          IF( ISEC .GE. HOUR ) THEN
              IH    = ITIME/HOUR
              IHX   = IH*HOUR
              ITIME = ITIME - IHX
              ISEC  = ISEC - IHX
          ENDIF
          MN    = ITIME/MINUT
          ISEC  = ISEC - MN*MINUT
      ENDIF
      IS1  = ISEC/N100
      IS2  = MOD( ISEC, N100)

      RETURN
      END
*CMZ :          24/06/94  10.51.47  by  Unknown
*-- Author :
C
C***********************************************************************
C*=================================                                    *
C* SUBROUTINE BSTIME( TIME, IFLG )                                     *
C*=================================                                    *
C*((Purpose))                                                          *
C*        Interface routine to get used CPU time from FORTRAN          *
C*        Library routine CLOCK etc.                                   *
C*((Input))                                                            *
C*        IFLG  : Flag                                                 *
C*          IFLG = 0 : Initialization of clock routine.                *
C*          IFLG = 1 : Get used CPU time.                              *
C*((Output))                                                           *
C*        TIME  : Used CPU time in second.                             *
C*                                                                     *
C*       Coded by S.Kawabata        Oct. '85                           *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE BSTIME( TIME, IFLG )
C
*     save time_init
C

      IF( IFLG .NE. 0 ) THEN
C
C         iutime.c should be compiled.
C
*         TIME = uxtime() - time_init
          CALL TIMEX(TIME)
C
      ELSE

*         time_init = uxtime()
          CALL TIMEST(9999999.)
          TIME      = 0.0

      ENDIF
C
      RETURN
      END
      SUBROUTINE BSUTIM( JOB, ID )

C     COMMON/NINFO/ NODEID, NUMNOD
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)

*  Prior to call thisroutine, BSTIME( TIME0, 1 ) should be called
*  for initialize the time offset TIME0.
*
*     print *,'bsutim .. job, id ',job,id
      CALL BSTIME( RTIME, 1)
      DTIME      = RTIME - TIME0

      IF( JOB .EQ. 0 ) THEN
*       For BASES computing time
*         ID  = 0  : Grid defining step
*               1  : Integration step
*               2  : Others

          TIMEBS(ID) = TIMEBS(ID) + DTIME

          IF( ID .LE. 1 ) THEN
              TIMINT = TIMINT + DTIME
          ENDIF
      ELSE
*       For SPRING computing time
*         ID  = 0  : Event generation
*               1  : Overhead
*               2  : Others

          TIMESP(ID) = TIMESP(ID) + DTIME

      ENDIF

      TIME0      = RTIME

      RETURN
      END
************************************************************************
*                                                                      *
*    ==========================                                        *
      SUBROUTINE BSWRIT( LUN )
*    =====================                                             *
* ((Purpose))                                                          *
*     Read temporary result from disk file.                            *
* ((Auther))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ ND1(5*MXDIM+3)
*     COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
*    .               IG(MXDIM),NCALL
C     COMMON /BASE2/ ND2(6)
*     COMMON /BASE2/ ACC1,ACC2,ITMX1,ITMX2
      COMMON /BASE3/ ND3(11)
*     COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT
      COMMON /BASE4/ ND4(2*MXDIM*(NDMX+1)+4*LENG+MXDIM+3)
*     COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
*    .               ND,NG,NPG,MA(MXDIM)
      PARAMETER (ITM  = 50 )
*     COMMON /BASE5/ ND5(22*ITM)
      COMMON /BASE5/ ND5(23*ITM)
*     REAL*4 TIME, EFF, WRONG, TRSLT, TSTD, PCNT
*     COMMON /BASE5/ ITRAT(ITM,0:1),TIME(ITM,0:2),EFF(ITM,0:1),
*    .               WRONG(ITM,0:1),RESLT(ITM,0:1),ACSTD(ITM,0:1),
*    .               TRSLT(ITM,0:1),TSTD(ITM,0:1),PCNT(ITM,0:1)
      COMMON /RANDM/ ND6(45)

      PARAMETER ( NHS = 50, NSC = 50 )
      COMMON /PLOTH/ NPH(18*(NHS+NSC)+29),NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
*     INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
*     COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
*    .              NHIST, MAPL(4,NHS),
*    .              NSCAT, MAPD(4,NSC),
*    .              NW

      COMMON/NINFO/ NODEID, NUMNOD

      IF( NODEID .NE. 0 ) RETURN

      REWIND LUN
      WRITE(LUN) ND1,ND3,ND4,ND5,ND6,NPH
C     WRITE(LUN) ND1,ND2,ND3,ND4,ND5,ND6,NPH
      IF(NW .EQ. 0 ) NW = 281
      WRITE(LUN) NW,(IBUF(I),I=1,NW)
C
      RETURN
      END
************************************************************************
*    =======================================                           *
       SUBROUTINE DHFILL( ID, DX, DY, FX )
*    =======================================                           *
* ((Function))                                                         *
*     To fill scatter plot                                             *
*   This routine identifies the bin number which is to be updated      *
*   with weight FX*WGT.  Up to five points per plot are able to        *
*   be stacked before calling BHUPDT or SHUPDT.                        *
* ((Input))                                                            *
*   ID    : Histogram identification number                            *
*   DX    : Input x value                                              *
*   DY    : Input y value                                              *
*   FX    : Input value of the function                                *
* ((Author))                                                           *
*   S.Kawabata         June '90 at KEK                                 *
*                                                                      *
************************************************************************

      REAL*8 DX, DY, FX
      COMMON /BASE0/ IFLAG,IBASES
      REAL*8 SCALLS,WGT,TI,TSI,TACC
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))
C     COMMON /PLOTLU/ LU
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*======================================================================*
*               Find the scatter plot ID in the table                  *
*======================================================================*
*                                                                      *
      IF( NSCAT .GT. 0 ) THEN
          I  = IABS(MOD( ID, 13 )) + 1
          IF( DHASH(1, I) .EQ. 1 ) THEN
            IF( ID .EQ. MAPD( 1, DHASH(2,I))) THEN
                ISCAT = DHASH(2,I)
                GO TO 200
            ENDIF
          ELSEIF( DHASH(1, I) .GT. 1 ) THEN
            DO 100 K = 2, DHASH(1,I)+1
               IF( ID .EQ. MAPD( 1, DHASH(K,I))) THEN
                   ISCAT = DHASH(K,I)
                   GO TO 200
               ENDIF
  100       CONTINUE
          ENDIF
      ENDIF
C     IF( LU .GT. 0 ) THEN
C         WRITE(LU,9000) ID
C9000     FORMAT(1X,'No Scat_Plot corresponds to ID =',I5,
C    .          /1X,' This call is neglected ]]]')
C     ENDIF
      RETURN

*                                                                      *
*======================================================================*
*               Determine the bin numbers for x and y                  *
*======================================================================*
*                                                                      *
  200 X     = DX*1.0
      Y     = DY*1.0

          IP1   = MAPD(2,ISCAT)
          XMIN  = BUFF(IP1)
          XMAX  = BUFF(IP1+1)
          MXBIN = IBUF(IP1+2)
          DEV   = BUFF(IP1+3)
          IX    =   0
          IY    =   0
          IF( X .GE. XMIN .AND. X .LE. XMAX ) THEN
              IX   = INT( (X - XMIN)/DEV+ 1.0 )
              IF( IX .GT. MXBIN ) IX =   0
          ENDIF
C
          IF( IX .GT. 0 ) THEN
              YMIN  = BUFF(IP1+4)
              YMAX  = BUFF(IP1+5)
              MYBIN = IBUF(IP1+6)
              DEV   = BUFF(IP1+7)
              IF( Y .GE. YMIN .AND. Y .LE. YMAX ) THEN
                  IY   = INT((Y - YMIN)/DEV + 1.0)
                 IF( IY .GT. MYBIN ) THEN
                     IX  =  0
                     IY  =  0
                 ENDIF
              ENDIF
          ENDIF
*                                                                      *
*======================================================================*
*               Fill the scatter plot ID                               *
*======================================================================*
*----------------------------------------------------------------------*
*               For BASES                                              *
*----------------------------------------------------------------------*
*                                                                      *
      IF( IBASES .EQ. 1 ) THEN
          IF( IY .GT. 0 ) THEN

              IP2       = MAPD(3,ISCAT)
              IBUF(IP2) = SCALLS
              IP2       = IX + MXBIN*(IY - 1) + IP2
              BUFF(IP2) = BUFF(IP2) + FX*WGT

          ENDIF

*----------------------------------------------------------------------*
*               For SPRING                                             *
*----------------------------------------------------------------------*
*                                                                      *
      ELSE

          IP3         = MAPD(4,ISCAT)
          IBUF(IP3)   = IX
          IBUF(IP3+1) = IY

      ENDIF

      RETURN
      END
************************************************************************
*  =================================================================== *
      SUBROUTINE DHINIT(ID,DXMIN,DXMAX,NXBIN,DYMIN,DYMAX,NYBIN,TNAME)
*  =================================================================== *
* ((Function))                                                         *
*     To define a scatter plot                                         *
* ((Input))                                                            *
*    ID   : scatter plot identification number                         *
*    DXMIN: Lower limit of X for the scatter plot                      *
*    DXMAX: Upper limit of X for the scatter plot                      *
*    NXBIN: Number of bins of X for the plot (Max. is 50 )             *
*    DYMIN: Lower limit of Y for the scatter plot                      *
*    DYMAX: Upper limit of Y for the scatter plot                      *
*    NYBIN: Number of bins of Y for the plot (Max. is 50 )             *
*    TNAME: Title of the plot in the character string (upto 64         *
*            characters)                                               *
* ((Author))                                                           *
*    S.Kawabata     June '90 at KEK                                    *
*                                                                      *
************************************************************************

      REAL*8 DXMIN,DXMAX,DYMIN,DYMAX
      CHARACTER*(*) TNAME
      CHARACTER*64 NAME
      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))
*     COMMON/XHCNTL/ LOCK
      COMMON/PLOTLU/ LU
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*======================================================================*
*               Find the scatter plot ID in the table                  *
*======================================================================*
*                                                                      *
      IF( NSCAT .GE. NSC ) THEN
*         IF( LOCK .NE. 0 ) RETURN
          IF( LU .GT. 0 ) THEN
            WRITE(LU,9000) NSCAT,ID
 9000       FORMAT(1X,'Numberof Scat_plots exceeds ',I3,' at ID = ',I3,
     .            /1X,'This call is neglected.')
          ENDIF
          RETURN
      ENDIF

      IEXIST = 0
      I  = IABS(MOD( ID, 13 )) + 1
      NS     = DHASH(1, I)

      IF( NS .EQ. 1 ) THEN
            IF( ID .EQ. MAPD( 1, DHASH(2,I))) THEN
*               IF( LOCK .NE. 0 ) RETURN
                IEXIST = DHASH(2,I)
            ENDIF
      ELSEIF( NS .GT. 1 ) THEN
          DO 100 K = 2, DHASH(1,I)+1
            IF( ID .EQ. MAPD( 1, DHASH(K,I))) THEN
*               IF( LOCK .NE. 0 ) RETURN
                IEXIST = DHASH(K,I)
                GO TO 110
            ENDIF
  100    CONTINUE
  110    CONTINUE
      ENDIF
      XMIN  = DXMIN*1.0
      XMAX  = DXMAX*1.0
      YMIN  = DYMIN*1.0
      YMAX  = DYMAX*1.0

      IF( IEXIST .GT. 0 ) THEN
          IF( LU .GT. 0 ) THEN
            WRITE(LU,9100) ID
          ENDIF
 9100     FORMAT(1X,'Scat_Plot ID (',I3,' ) exists already.')
          IP1    =  MAPD(2,IEXIST)
          IF(( XMIN .EQ. BUFF(IP1))   .AND.
     .       ( XMAX .EQ. BUFF(IP1+1)) .AND.
     .       ( NXBIN .EQ. IBUF(IP1+2)) )    THEN
             IF(( YMIN .EQ. BUFF(IP1+4))   .AND.
     .          ( YMAX .EQ. BUFF(IP1+5)) .AND.
     .          ( NYBIN .EQ. IBUF(IP1+6)) )    THEN
                  IF( LU .GT. 0 ) THEN
                      WRITE(LU,9110)
                  ENDIF
 9110             FORMAT(1X,' This call is neglected.')
                  RETURN
             ENDIF
          ENDIF
          IF( LU .GT. 0 ) THEN
              WRITE(LU,9120) ID,XMIN,XMAX,NXBIN,YMIN,YMAX,NYBIN
          ENDIF
 9120     FORMAT(1X,'Scat_Plot ( ID =',I3,' ) parameters are replaced',
     .          /1X,'by the following new parameters :',
     .          /1X,' XMIN(',E12.5,')  XMAX(',E12.5,' )  XBIN(',I4,' )',
     .          /1X,' YMIN(',E12.5,')  YMAX(',E12.5,' )  YBIN(',I4,' )')
      ENDIF
      IF(NXBIN .GT. 50 .OR. NYBIN .GT. 50 ) THEN
         IF( LU .GT. 0 ) THEN
             WRITE(LU,9300) NXBIN,NYBIN,ID
         ENDIF
 9300    FORMAT(1X,'Bin size (',2I3,' )  exceeds 50 at ID =',I5,
     .         /1X,' This call is neglected .')
         RETURN
      ELSEIF((XMIN .GE. XMAX) .OR. (YMIN .GE. YMAX)) THEN
         IF( LU .GT. 0 ) THEN
             WRITE(LU,9400) ID,XMIN,XMAX,YMIN,YMAX
         ENDIF
 9400    FORMAT(1X,'Lower limit is larger than upper at SC_PL ID =',I5,
     .         /1X,' This call is neglected .',
     .         /1X,' XMIN =',G13.4,' XMAX =',G13.4,
     .         /1X,' YMIN =',G13.4,' YMAX =',G13.4)
         RETURN
      ENDIF
      IF(DHASH(1,I) .GE. NSC ) THEN
         IF( LU .GT. 0 ) THEN
             WRITE(LU,9500) I
         ENDIF
 9500    FORMAT(1X,I5,'-th Hash table overflow',
     .         /1X,' This call is neglected.')
         RETURN
      ENDIF

      IF( IEXIST .GT. 0 ) THEN
          NSCT     = IEXIST
      ELSE
          NSCAT        = NSCAT + 1
          DHASH(1,I)   = DHASH(1,I) + 1
          K            = DHASH(1,I) + 1
          DHASH(K,I)   = NSCAT
          NSCT         = NSCAT
          IP1    = NW + 1
          NW  = NW + 2527
          MAPD(1,NSCT)  = ID
          MAPD(2,NSCT)  = IP1
      ENDIF

         BUFF(IP1     ) = XMIN
         BUFF(IP1 +  1) = XMAX
         IBUF(IP1 +  2) = NXBIN
         DEV            = XMAX - XMIN
         BUFF(IP1 +  3) = DEV/NXBIN
         BUFF(IP1 +  4) = YMIN
         BUFF(IP1 +  5) = YMAX
         IBUF(IP1 +  6) = NYBIN
         DEV            = YMAX - YMIN
         BUFF(IP1 +  7) = DEV/NYBIN
      IP2   = IP1 + 8
         MAPD(3,NSCT)  = IP2
         IBUF(IP2     ) = 0
      IP3   = IP1 + 2509
         MAPD(4,NSCT)  = IP3
         IBUF(IP3     ) =  0
         IBUF(IP3 +  1) =  0

         I1   = IP3 + 2
         I2   = I1 + 15
         NAME = TNAME
         READ(NAME,9800) (BUFF(I),I=I1,I2)
 9800    FORMAT(16A4)

      RETURN
      END
************************************************************************
*     =========================                                        *
       SUBROUTINE DHPLOT( LU )
*     =========================                                        *
* ((Purpose))                                                          *
*      To print scatter plots for BASES and SPRING                     *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    June '90                                         *
*                                                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      CHARACTER*1  PLUS,MINUS,BLNK,STAR,NUM(0:9),NEG(0:9),SHARP,PNT
      REAL*4       X(50)
      CHARACTER*1 CHARR(50), CN
      CHARACTER*80 FORM1,FORM
      DATA  PLUS /'+'/, MINUS /'-'/, BLNK /' '/, STAR /'*'/
      DATA  SHARP /'#'/,  PNT /'.'/
      DATA  NUM  / '0','1','2','3','4','5','6','7','8','9'/
      DATA  NEG  / '-','a','b','c','d','e','f','g','h','i'/
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
      CN   = CHAR(12)

      IF( NSCAT .GT. 0 ) THEN
         DO 900 ISCAT = 1, NSCAT
            IP3   = MAPD(4,ISCAT)
            IF( IPNT .EQ. 0 ) THEN
                WRITE(LU,9010)
            ELSE
                WRITE(LU,9020) CN
            ENDIF
 9010       FORMAT(/1H1)
 9020       FORMAT(A1)
            WRITE(LU,9100) MAPD(1,ISCAT),(BUFF(I), I=IP3+2,IP3+17)
 9100       FORMAT(/5X,'Scat_Plot (ID =',I3,' ) for ',16A4,/)

            IP1   = MAPD(2,ISCAT)
            XL    = BUFF(IP1)
            XU    = BUFF(IP1+1)
            NX    = IBUF(IP1+2)
            DX    = BUFF(IP1+3)
            XM    = ABS(XU)
            XX    = ABS(XL)
            IF( XX .GT. XM ) XM = XX
            CALL XHORDR( XU, FX, XORD, IXORD)
            YL    = BUFF(IP1+4)
            YU    = BUFF(IP1+5)
            NY    = IBUF(IP1+6)
            DY    = BUFF(IP1+7)
            MIDY  = NY/2
            IF( MIDY .EQ. 0 ) MIDY = 1
            YM    = ABS(YU)
            YY    = ABS(YL)
            IF( YY .GT. YM ) YM = YY
            CALL XHORDR( YM, FY, YORD, IYORD)
            IP2   = MAPD(3,ISCAT)
            NTOTAL= IBUF(IP2)
            VMAX  = BUFF(IP2+1)
            VMIN  = VMAX
            DO 100 J = 0,NY-1
               IB    = NX*J + IP2
               DO 100 I = 1,NX
                  VLS    = BUFF( I + IB )
                  IF( VLS .GT. VMAX ) VMAX = VLS
                  IF( VLS .LT. VMIN ) VMIN = VLS
  100       CONTINUE
***
            IF( VMAX .EQ. 0.0 .AND. VMIN .EQ. 0.0 ) THEN
                VMAX  = 10.0
                VMIN  = 0.0
            ENDIF
***
            IF( VMAX .GT. -VMIN ) THEN
                UNIT = ABS(VMAX)/11.0
            ELSE
                UNIT = ABS(VMIN)/11.0
            ENDIF
            WRITE(FORM1,9200) NX
*9200       FORMAT('(7X,''E'',I3,3X,''+'',',I2,'(''--''),''-+'')')
 9200       FORMAT('(7X,''E'',I3,3X,''+'',',I2,'(''-''),''+'')')
            WRITE(LU,FORM1) IYORD
            DO 300 L = NY-1,0,-1
               IB     = NX*L + IP2
               DO 200 I = 1,NX
                 XNUM   = BUFF( I + IB )/UNIT
                 IF( XNUM .LT. 0 0 ) THEN
                     NUMB   = XNUM - 1.0
                     IF(     NUMB .GE. -1 )THEN
                             CHARR(I) = MINUS
                     ELSEIF( NUMB .GE. -10 ) THEN
                            CHARR(I) = NEG(-NUMB-1)
                     ELSE
                            CHARR(I) = SHARP
                     ENDIF
                 ELSE
                     NUMB   = XNUM + 1.0
                     IF(     XNUM .EQ. 0.0 ) THEN
                             CHARR(I) = BLNK
                     ELSEIF( NUMB .LE.  1 ) THEN
                             CHARR(I) = PLUS
                             IF( VMIN .GE. 0.0 ) CHARR(I) = PNT
                     ELSEIF( NUMB .LE. 10 ) THEN
                             CHARR(I) = NUM(NUMB-1)
                     ELSE
                             CHARR(I) = STAR
                     ENDIF
                 ENDIF
  200          CONTINUE

               Y   = (L*DY + YL)/YORD
               IF( L .EQ. MIDY ) THEN
                   WRITE(FORM,9300) NX
*9300              FORMAT('(5X,F6.3,'' Y I'',',I2,'(1X,A1),'' I'')')
 9300              FORMAT('(5X,F6.3,'' Y I'',',I2,'A1,''I'')')
               ELSE
                   WRITE(FORM,9310) NX
*9310              FORMAT('(5X,F6.3,''   I'',',I2,'(1X,A1),'' I'')')
 9310              FORMAT('(5X,F6.3,''   I'',',I2,'A1,''I'')')
               ENDIF
               WRITE(LU,FORM) Y,(CHARR(M),M=1,NX)

  300       CONTINUE

            WRITE(LU,FORM1) IYORD

            NXH   = NX/2
            IF( NXH .EQ. 0 ) NXH = 1
            WRITE(FORM,9400) NXH

*           WRITE(FORM,9400) NX
 9400       FORMAT('(6X,''Low-'',5X,',I2,'X,''X'')')
            WRITE(LU,FORM)

            XORD     = XORD*10.
            DO 400 I = 1, NX
               X(I)  = ((I-1)*DX + XL)/XORD
               IF( X(I) .LT. 0.0 ) THEN
                   CHARR(I)  = MINUS
                   X(I)      = -X(I)
               ELSE
                   CHARR(I)  = BLNK
               ENDIF
  400       CONTINUE
            WRITE(FORM1,9500) NX
*9500       FORMAT('(6X,''Edge'',5X,',I2,'(1X,A1))')
 9500       FORMAT('(6X,''Edge'',5X,',I2,'A1)')
            WRITE(LU,FORM1) (CHARR(M),M=1,NX)

            XORD      = 1.0
            DO 600 I  = 1,5
               IF( I .EQ. 2 ) THEN
                   WRITE(FORM,9602) NX
 9602              FORMAT('(7X,''E'',I3,4X',I2,
     .                    '(''.''))')
                   WRITE(LU,FORM) IXORD
               ELSE
                   DO 500 J = 1, NX
                      XX        = X(J)*10.0
                      NUMB      = XX
                      CHARR(J)  = NUM(NUMB)
                      X(J)      = XX - FLOAT(NUMB)
  500              CONTINUE
                   IF(     I .EQ. 4 ) THEN
                           WRITE(FORM,9604) NX
 9604                      FORMAT('(7X,''Low-'',4X,',I2,
     .                            'A1)')
                   ELSEIF( I .EQ. 5 ) THEN
                           WRITE(FORM,9605) NX
 9605                      FORMAT('(7X,''Edge'',4X,',I2,
     .                            'A1)')
                   ELSE
                           WRITE(FORM,9601) NX
 9601                      FORMAT('(15X,',I2,
     .                            'A1)')
                   ENDIF
                   WRITE(LU,FORM) (CHARR(M),M=1,NX)
               ENDIF
  600       CONTINUE

  900    CONTINUE
      ENDIF
C
      RETURN
      END
C**********************************************************************
C*======================                                              *
C* FUNCTION DRN( ISEED)                                               *
C*======================                                              *
C*  Machine-independent Random number generator                       *
C*     General purpose Version,  OK as long as >= 32 bits             *
C*((Arguement))                                                       *
C*  ISEED: Seed                                                       *
C*                                                                    *
C* 21-Mar-96 Mod. by S. Giagu to use RANMAR in the L3 MC Generator    *
C*           Framework.                                               *
C*                                                                    *
C**********************************************************************

      DOUBLE PRECISION FUNCTION DRN(ISEED)
      REAL XVECX(1)
      CALL RANMAR(XVECX,1)
      DRN = XVECX(1)
      RETURN
      END
C**********************************************************************
C*============================                                        *
C* Subroutine DRNSET( ISEED )                                         *
C*============================                                        *
C*((Purpose))                                                         *
C*  Initialization routine of                                         *
C*         Machine-independent Random number generator                *
C*         General purpose Version,  OK as long as >= 32 bits         *
C*((Arguement))                                                       *
C*  ISEED: SEED                                                       *
C*                                                                    *
C* 21-Mar-96 Mod. by S. Giagu to use RANMAR.
C*                                                                    *
C**********************************************************************

      SUBROUTINE DRNSET( ISEED )
      NTOT1=0
      NTOT2=0
      CALL RMARIN(ISEED,NTOT1,NTOT2)
      RETURN
      END
************************************************************************
*    ===================                                               *
      SUBROUTINE SHCLER
*    ===================                                               *
* ((FUNCTION))                                                         *
*     To cancel the update of histograms and scatter plots in case     *
*   of the trial was rejected.                                         *
* ((Author))                                                           *
*     S.Kawabata June '90 at KEK                                       *
*                                                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      IF( NHIST .GT. 0 ) THEN
         DO 200  J   = 1, NHIST
           IP3       = MAPL(3,J)
           IBUF(IP3) = -1
  200    CONTINUE
      ENDIF
C
      IF( NSCAT .GT. 0 ) THEN
         DO 500   K    = 1, NSCAT
           IP3         = MAPD(4,K)
           IBUF(IP3)   =  0
           IBUF(IP3+1) =  0
  500    CONTINUE
      ENDIF
C
      RETURN
      END
************************************************************************
*    ===========================                                       *
      SUBROUTINE SHFILL( NTRY )
*    ===========================                                       *
* ((Function))                                                         *
*     To fill the number of trials for a event generation              *
* ((Input))                                                            *
*    NTYR : the number of trials for the current event                 *
* ((Author))                                                           *
*    S.Kawabata    April 1994                                          *
*                                                                      *
************************************************************************

      PARAMETER ( MXBIN = 51 )
      COMMON/PLOTSP/ NBIN,IBUFSP( MXBIN )

      IF( NTRY .LE. NBIN ) THEN
          IBUFSP( NTRY ) = IBUFSP( NTRY ) + 1
      ELSE
          IBUFSP( NBIN+1 ) = IBUFSP( NBIN+1 ) + 1
      ENDIF

      RETURN
      END
************************************************************************
*    ============================                                      *
      SUBROUTINE SHINIT( MXTRY )
*    ============================                                      *
* ((Function))                                                         *
*     To clear the histogram buffer for generation efficiency          *
* ((Input))                                                            *
*    MXTRY: Maximum number of trials for one event generation          *
* ((Author))                                                           *
*    S.Kawabata    April 1994                                          *
*                                                                      *
************************************************************************

      INTEGER MXTRY
      PARAMETER ( MXBIN = 51 )
      COMMON/PLOTSP/ NBIN,IBUFSP( MXBIN )

      IF( MXTRY .GT. 50 ) THEN
          NBIN  = 50
      ELSE
          NBIN  = MXTRY
      ENDIF

      DO 100 I = 1,NBIN+1
         IBUFSP(I) = 0
  100 CONTINUE

      RETURN
      END
************************************************************************
*    =========================                                         *
      SUBROUTINE SHPLOT( LU )
*    =========================                                         *
C*((Function))                                                         *
C*    To print histograms and scatter plots defined by XHINIT and      *
C*  DHINIT.                                                            *
C*    For the original histograms, a special histograms are printd     *
C*  by this routine. For the additional histograms and scatter plots   *
C*  routines XHPLOT and DHPLOT are called.                             *
C*((Author))                                                           *
C*    S.Kawabata   June '90 at KEK                                     *
C*                                                                     *
C***********************************************************************

      REAL*8         SCALLS,WGT,TI,TSI,TACC
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      CHARACTER*50 CHARR,CHR1
      CHARACTER*52 SCALE
      REAL  VAL(0:51),VLOG(0:51)
      REAL  VERR(0:51)
      CHARACTER*1  BLNK,STAR,CROS,AI,CN
      DATA  YMAX / 50/, BLNK /' '/, STAR /'*'/, CROS /'O'/
      DATA  AI /'I'/

      CN  = CHAR(12)

      CALL XHCHCK( LU )

      IF( NHIST .GT. 0 ) THEN
*                 add March 1994
         CALL SHUPDT
*
C        NTOTAL= SCALLS
         DO 500 IHIST = 1, NHIST
          IF(IFBASE(IHIST) .EQ. 1 ) THEN
            IP3  = MAPL(4,IHIST)
            IF( IPNT .EQ. 0 ) THEN
                WRITE(LU,9010)
            ELSE
                WRITE(LU,9020) CN
            ENDIF
 9010       FORMAT(/1H1)
 9020       FORMAT(A1)
            WRITE(LU,9050) MAPL(1,IHIST),(BUFF(I), I=IP3+1,IP3+15)
 9050       FORMAT(1X,'Original Histogram (ID =',I3,' ) for ',15A4)

            IP1   = MAPL(2,IHIST)
            XMIN  = BUFF(IP1)
            XMAX  = BUFF(IP1+1)
            NXBIN = IBUF(IP1+2) + 1
            DEV   = BUFF(IP1+3)
            VMAX  = 0.0
            VORG  = 0.0
            VEVT  = 0.0
C           FACT       = 1./(NTOTAL*DEV)
            FACT       = 1./(SCALLS*DEV)
            IP2   = MAPL(3,IHIST)
            IPX   = IP2 + 52
            IPF   = IP2 + 156
            IPF2  = IPF + 52
C           VAL(0)     = BUFF(IPF)/NTOTAL
            VAL(0)     = BUFF(IPF)/SCALLS
C           VAL(NXBIN) = BUFF(IPF+NXBIN)/NTOTAL
            VAL(NXBIN) = BUFF(IPF+NXBIN)/SCALLS
            VEVT1 = BUFF(IPX) + BUFF(IPX+NXBIN)
            DO  50 I   = 1,NXBIN-1
                TX     = BUFF(I+IPF)
                NX     = IBUF(I+IP2)
                VLS    = TX*FACT
                IF( VMAX .LT. VLS ) VMAX = VLS
                VAL(I) = VLS
                IF( NX .GT. 1 ) THEN
                  DEV2   =  NX*BUFF(I+IPF2)-TX*TX
                  IF( DEV2 .LE. 0.0 ) THEN
                      VERR(I)= 0.0
                  ELSE
                      VERR(I)= FACT*SQRT( DEV2/( NX-1 ))
                  ENDIF
*TI             ELSEIF( NX .EQ. 1 ) THEN
*TI               VERR(I)= VLS
                ELSE
                  VERR(I)= 0.0
                ENDIF
                VORG   = VLS + VORG
                VEVT   = BUFF(I+IPX) + VEVT
   50       CONTINUE
            NTOT   = INT(VEVT+VEVT1)
            IF( VMAX .LE. 0.0 .AND. VEVT .GT. 0.0 ) THEN
                  WRITE(LU,9060) MAPL(1,IHIST)
 9060             FORMAT(/5X,'***************************************',
     .                   /5X,'* Since BASES has no entry            *',
     .                   /5X,'*     in the histogram ID(',I6,' ),   *',
     .                   /5X,'*  an additional hist. is given       *',
     .                   /5X,'*     in the next page in stead.      *',
     .                   /5X,'***************************************')
C
                  CALL XHPLOT( LU, 1, IHIST )
C
                  GO TO 500
            ELSEIF( VEVT .LE. 0) THEN
                  WRITE(LU,9070) IHIST
 9070             FORMAT(/5X,'***************************************',
     .                   /5X,'*    SPRING has no entry              *',
     .                   /5X,'*     in the histogram ID(',I6,' )    *',
     .                   /5X,'***************************************')
                  GO TO 500
            ENDIF
            VNORM = VORG/VEVT
            XNORM = VNORM*DEV
            VLMAX = ALOG10(VMAX)
            VLMIN = VLMAX
            DO  60 I = 0,NXBIN
              IF( VAL(I) .GT. 0.0 ) THEN
                  VLS   = ALOG10( VAL(I) )
                 IF( I .GT. 0 .AND. I .LT. NXBIN ) THEN
                    IF( VLS .LT. VLMIN ) VLMIN = VLS
                 ENDIF
                 VLOG(I)  = VLS
              ELSE
                 VLOG(I)  = 0.0
              ENDIF
   60       CONTINUE
C
             VXMAX = VLMAX
             IF( VLMIN .LT. 0.0) THEN
                VXMIN = IFIX(VLMIN) - 1.0
             ELSE
                VXMIN = IFIX(VLMIN)
             ENDIF
             CALL XHRNGE( 1, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP)
             UNITL = (VLMAX-VLMIN)/YMAX
C
             CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHR1)
C
C
             WRITE(LU,9150) NTOT
 9150        FORMAT(1X,'Total =',I10,' events',
     .              3X,'"*" : Orig. Dist. in Log Scale.')
             VXMIN = 10.0**VLMIN
             WRITE(LU,9200) SCALE
 9200        FORMAT(1X,'   x      d(Sig/dx)  dN/dx',A52)
             WRITE(LU,9250) CHR1
 9250        FORMAT(1X,
     .             '+-------+----------+-------+',
     .       A50 )
C

            VX    = ABS(XMAX)
            XM    = ABS(XMIN)
            IF( XM .GT. VX ) VX = XM

            CALL XHORDR( VX, F2, ORD, IORD )

            DO 200 I = 0,NXBIN
              RNORM = VNORM
              IF( I .EQ. 0 .OR. I .EQ. NXBIN ) RNORM = XNORM
              VX    = VAL(I)
              XL     = BUFF( I + IPX )
              NX     = XL
              IF( VX .GT. 0.0 ) THEN
                 NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0
              ELSE
                 NUMBL  = 0
              ENDIF
              IF( NX .GT. 0 ) THEN
                 NUMB   = ( LOG10( XL*RNORM ) - VLMIN)/UNITL + 1.0
                 ERL    = SQRT(XL)
                 DERL   = (XL + ERL)*RNORM
                 NERUP  = ( LOG10( DERL ) - VLMIN)/UNITL + 1.0
                 DERL   = (XL - ERL)*RNORM
                 IF( DERL .GT. 0.0 ) THEN
                     NERLW  = ( LOG10( DERL ) - VLMIN)/UNITL + 1.0
                 ELSE
                     NERLW  = 0
                 ENDIF
              ELSE
                 NUMB   = 0
                 NERUP  = 0
                 NERLW  = 0
              ENDIF
              IF( NUMB  .GT. 50 ) NUMB = 50
              IF( NUMBL .GT. 50 ) NUMBL= 50
              DO 100 K = 1,50
                IF( K .LE. NUMBL) THEN
                  CHARR(K:K) = STAR
                ELSE
                  IF( K .EQ. 50 ) THEN
                    CHARR(K:K) = AI
                  ELSE
                    CHARR(K:K) = BLNK
                  ENDIF
                ENDIF
C
                IF(     K .EQ. NUMB ) THEN
                        CHARR(K:K) = CROS
                        IF( K .EQ. NERUP .AND. K .EQ. NERLW ) GO TO 100
                ENDIF
                IF(     K .EQ. NERUP ) THEN
                        CHARR(K:K) = '>'
                ELSEIF( K .EQ. NERLW ) THEN
                        CHARR(K:K) = '<'
                ENDIF

  100         CONTINUE

              CALL XHORDR( VX, F2, ORDER, IORDR )

             IF( I .EQ. 0 .OR. I .EQ. NXBIN ) THEN
                 WRITE(LU,9300) IORD,F2,IORDR,NX,CHARR
 9300            FORMAT(1X,'I  E',I3,' I',F6.3,'E',I3,'I',
     .                                            I7,'I',A50)
             ELSE
                   XM    = (XMIN + DEV*(I-1))/ORD
                   WRITE(LU,9340) XM,F2,IORDR,NX,CHARR
 9340              FORMAT(1X,'I',F6.3,' I',F6.3,'E',I3,'I',
     .                                        I7,'I',A50)
             ENDIF
  200       CONTINUE
             WRITE(LU,9250) CHR1
             WRITE(LU,9260)
 9260    FORMAT(1X,
     .       '   x      d(Sig/dx)  dN/dx',4X,
     .       '"O" : Generated Events.',
     .       '( Arbitrary unit in Log )')
C
           ELSE
C
              CALL XHPLOT( LU, 1, IHIST )
C
           ENDIF
  500    CONTINUE
      ENDIF
C
      CALL DHPLOT( LU )
C
      RETURN
      END
************************************************************************
*    ====================                                              *
      SUBROUTINE SHRSET
*    ====================                                              *
* ((Function))                                                         *
*     To reset the content of histograms and scatter plots.            *
* ((Author))                                                           *
*     S.Kawabata   June '90 at KEK                                     *
*                                                                      *
* **********************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      IF( NHIST .GT. 0 ) THEN
         DO 100 IHIST = 1, NHIST
            IP2       = MAPL(3,IHIST) + 52
            IP3       = MAPL(4,IHIST)
            IBUF(IP3) = -1
            DO 100 I = 0,51
               BUFF(I+IP2) = 0.0
  100      CONTINUE
      ENDIF
C
      IF( NSCAT .GT. 0 ) THEN
         DO 400   ISCAT = 1, NSCAT
            IP3         = MAPD(4,ISCAT)
            IBUF(IP3)   = 0
            IBUF(IP3+1) = 0
            IP2         = MAPD(3,ISCAT)
            IBUF(IP2)   = 0
            DO 400   I  = IP2+1,IP2+2500
               BUFF(I)  = 0.0
  400      CONTINUE
      ENDIF
C
      RETURN
      END
************************************************************************
*    ====================                                              *
      SUBROUTINE SHUPDT
*    ====================                                              *
* ((Function))                                                         *
*     To update histograms and scatter plots with unit weight.         *
*   The bin number to be updated is marked by XHFILL and DHFILL.       *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
*                                                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      IF( NHIST .GT. 0 ) THEN
         DO 150   IHIST   = 1, NHIST
            IP3       = MAPL(4,IHIST)
            IX        = IBUF(IP3)
            IF( IX .GE. 0 ) THEN
                IP       = IX + MAPL(3,IHIST) + 52
                BUFF(IP) = BUFF(IP) + 1.

                IBUF(IP3)  = -1
            ENDIF
  150    CONTINUE
      ENDIF
C
      IF( NSCAT .GT. 0 ) THEN
         DO 250   ISCAT   = 1, NSCAT
            IP3         = MAPD(4,ISCAT)
            IX          = IBUF(IP3)
            IF( IX .GT. 0 ) THEN
                IP1   = MAPD(2,ISCAT)
                MXBIN = IBUF(IP1+2)
                MYBIN = IBUF(IP1+6)
                IP2       = MAPD(3,ISCAT)
                IBUF(IP2) = IBUF(IP2) + 1
                IY        = IBUF(IP3+1)
                IF( IX .GT. 0 .AND. IX .LE. MXBIN .AND.
     .              IY .GT. 0 .AND. IY .LE. MYBIN ) THEN
                    IP       = IX + MXBIN*(IY-1) + IP2
                    BUFF(IP) = BUFF(IP) + 1.0
                ENDIF
                IBUF(IP3)   =  0
                IBUF(IP3+1) =  0
           ENDIF
C
  250    CONTINUE
      ENDIF
C
      RETURN
      END
************************************************************************
*    ===================                                               *
      SUBROUTINE SPCHCK
*    ===================                                               *
* ((Purpose))                                                          *
*     To check user's initialization parameters.                       *
*                                                                      *
*        Coded by S.Kawabata      April  '94                           *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER ( MXDIM = 50)
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2

      COMMON /BASE0/ JFLAG,IBASES
      COMMON /BASE1/ XLT(MXDIM),XUT(MXDIM),NDIMT,NWILDT,
     .               IGT(MXDIM),NCALLT
      COMMON /BASE2/ ACC1T,ACC2T,ITMX1T,ITMX2T
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      IF( NDIM .NE. NDIMT ) THEN
          WRITE(6,9100) NDIM,NDIMT
 9100     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given NDIM(',I6,' ) does not match          *',
     .    /5X,'*      to NDIM(',I6,' ) in BASES.               *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

      IF( NWILD .NE. NWILDT ) THEN
          WRITE(6,9110) NWILD,NWILDT
 9110     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given NWILD(',I6,' ) does not match         *',
     .    /5X,'*      to NWILD(',I6,' ) in BASES.              *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

      DO 200 I = 1,NDIM
         IF( XL(I) .NE. XLT(I) ) THEN
             WRITE(6,9200) I,XL(I),I,XLT(I)
 9200        FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given XL(',I3,' ) = ',D15.8,'            *',
     .    /5X,'*      does not match to                        *',
     .    /5X,'*      to XL(',I3,' ) = ',D15.8,' in BASES   *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
             STOP
         ENDIF
         IF( XU(I) .NE. XUT(I) ) THEN
             WRITE(6,9210) I,XU(I),I,XUT(I)
 9210        FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given XU(',I3,' ) = ',D15.8,'            *',
     .    /5X,'*      does not match to                        *',
     .    /5X,'*      to XU(',I3,' ) = ',D15.8,' in BASES   *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
             STOP
         ENDIF
  200 CONTINUE

      RETURN
      END
************************************************************************
*    ===============================                                   *
      SUBROUTINE SPHBOK( IOFSET )
*    ===============================                                   *
* ((Purpose))                                                          *
*      To write the ID-th histogram on the unit LUNIT.                 *
* ((Input))                                                            *
*      LUNIT: Logical unit number                                      *
*      ID   : Historgram ID                                            *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata   June '90 at KEK                                   *
*                                                                      *
************************************************************************

      COMMON /SPRNG2/ MXTRY,NEVENT, NTRIAL, MISS

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      CHARACTER*80 TITLE

      COMMON/PLOTLU/ LU

      REAL*4 COUNT(NHS), ECOUNT(NHS)

C
      IF( NHIST .GT. 0 ) THEN


          DO 500 IHIST = 1, NHIST
C                             ID1 : ID number of IHIST-th histogram
C                             BMAX : Maximum content of Hist ID1 from BASES
             IFLH  = IFBASE(IHIST)
             IF( IFLH .EQ. 1 ) THEN
                 ID1   = MAPL(1,IHIST)
                 BSUM  = HSUM( ID1 )
             ELSE
                 BSUM  = 0.0
             ENDIF

             ID    = MAPL(1,IHIST)
             ID    = ID + IOFSET + 20000
             IP1   = MAPL(2,IHIST)
             XMIN  = BUFF(IP1)
             XMAX  = BUFF(IP1+1)
             NXBIN = IBUF(IP1+2)
             DEV   = BUFF(IP1+3)
             IP2   = MAPL(3,IHIST)
             IP3   = MAPL(4,IHIST)


             WRITE( TITLE, 9500) (BUFF(I), I=IP3+1,IP3+16)
 9500        FORMAT(16A4)

             CALL HBOOK1( ID, TITLE, NXBIN, XMIN, XMAX, 0.0 )

             IPX   = IP2 + 52
C               To obtain the maximum content of Spring hist ID1
             SSUM  = NEVENT
C                        FACT : Normalization factor
             FACT  = BSUM/SSUM
             DO  400 I   = 0, NXBIN

                 XX     = XMIN + DEV*(FLOAT(I) - 0.5)

	        CALL HFILL( ID, XX, 0.0, BUFF( I + IPX ) )

                COUNT(I)  = BUFF( I+IPX )
                ECOUNT(I) = SQRT( BUFF( I+IPX ) )
                IF( IFLH .EQ. 1 ) THEN
                    COUNT(I) = COUNT(I) * FACT
                    ECOUNT(I) = ECOUNT(I) * FACT
                ENDIF
  400        CONTINUE

             CALL HPAK(  ID,  COUNT )
             CALL HPAKE( ID, ECOUNT )

  500     CONTINUE

      ENDIF

      IF( NSCAT .GT. 0 ) THEN
         DO 900 ISCAT = 1, NSCAT

            IP3   = MAPD(4,ISCAT)

            WRITE( TITLE, 9500) (BUFF(I), I=IP3+2,IP3+17)

            ID    = MAPD(1,ISCAT)
            ID    = ID + IOFSET + 30000

            IP1   = MAPD(2,ISCAT)
            XL    = BUFF(IP1)
            XU    = BUFF(IP1+1)
            NX    = IBUF(IP1+2)
            DX    = BUFF(IP1+3)
            YL    = BUFF(IP1+4)
            YU    = BUFF(IP1+5)
            NY    = IBUF(IP1+6)
            DY    = BUFF(IP1+7)

            CALL HBOOK2( ID, TITLE, NX, XL, XU, NY, YL, YU, 0.0 )

            IP2   = MAPD(3,ISCAT)

            DO 700 L = 0, NY-1
               IB     = NX*L + IP2
               DO 600 I = 1,NX

                  XX    = XL + DX*(FLOAT(I) - 0.5)
                  YY    = YL + DY*(FLOAT(L) - 0.5)

                  CALL HFILL( ID, XX, YY, BUFF( I + IB ) )

  600          CONTINUE
  700       CONTINUE

  900    CONTINUE
      ENDIF

      RETURN
      END
************************************************************************
*    =========================                                         *
      SUBROUTINE SPHIST( LU )
*    =========================                                         *
* ((Purpose))                                                          *
*      To print the histogram for event generation                     *
* ((Input))                                                            *
*      LU   : logical unit number for the printer to be printed        *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    April 1994                                       *
*                                                                      *
************************************************************************

      PARAMETER ( MXBIN = 51 )
      COMMON/PLOTSP/ NBIN,IBUFSP( MXBIN )
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      REAL  VAL(MXBIN),VLOG(MXBIN)
      CHARACTER*50 CHARR,CHAR1
      CHARACTER*52 SCALE
      CHARACTER*1  BLNK,STAR,OO,AI,CN
      DATA  YMAX / 50/
      DATA  BLNK /' '/, STAR /'*'/, OO /'O'/, AI /'I'/

      CN    = CHAR(12)
      IF( IPNT .EQ. 0 ) THEN
          WRITE(LU,9000)
 9000     FORMAT(/1H1,/1H )
      ELSE
          WRITE(LU,9005) CN
 9005     FORMAT(A1)
      ENDIF
          WRITE(LU,9102)
 9102     FORMAT(5X,
     .  '************* Number of trials to get an event *************')

      XMIN  = 1.0
      XMAX  = NBIN
      DEV   = 1.0
      NBIN1 = NBIN + 1

          NTOTAL     = IBUFSP(NBIN1)
          VAL(NBIN1) = FLOAT(NTOTAL)
          VMIN       = 0.0
          VMAX       = VMIN
          DO  55 I   = 1,NBIN
              NTR    = IBUFSP(I)
              VLS    = FLOAT( NTR )
              NTOTAL = NTR + NTOTAL
              IF( VMAX .LT. VLS ) VMAX = VLS
              VAL(I) = VLS
   55     CONTINUE

          VLMAX = LOG10(VMAX)
          VLMIN = VLMAX

           DO  60 I = 1,NBIN1
               IF( VAL(I) .GT. 0.0 ) THEN
                   VLS   = LOG10( VAL(I) )
                   IF( I .LE. NBIN ) THEN
                       IF( VLS .LT. VLMIN ) VLMIN = VLS
                   ENDIF
                   VLOG(I)  = VLS
               ENDIF
   60      CONTINUE

           IF( VLMIN .LT. 0.0) THEN
               VXMIN = IFIX(VLMIN) - 1.0
           ELSE
               VXMIN = IFIX(VLMIN)
           ENDIF
           VXMAX = VLMAX
           IFLG  = 1
           CALL XHRNGE( IFLG, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP )
           UNITL = (VLMAX-VLMIN)/YMAX

       IFLG   = 0
           IF( VMIN .GE. 0.0 ) THEN
               VXMAX  = 1.2*VMAX
               VXMIN  = 0.0
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )
           ELSE
               VXMAX  = 1.1*VMAX
               VXMIN  = 1.1*VMIN
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )
           ENDIF

       UNIT  = (VMAX-VMIN)/YMAX

       CALL XHSCLE( IFLG, VMIN, VMAX, VSTP, UNIT, SCALE, CHAR1 )

             WRITE(LU,9210) NTOTAL
 9210        FORMAT(1X,'Total =',I10,' events',
     .        3X,'"*" : No. of events in Linear scale.')
             WRITE(LU,9205) SCALE
 9205        FORMAT(1X,'   x      Lg(dN/dx)  dN/dx',A52)
             WRITE(LU,9251) CHAR1
 9251        FORMAT(1X,
     .             '+-------+----------+-------+',
     .       A50 )

       VX    = ABS(XMAX)
       XM    = ABS(XMIN)
       IF( XM .GT. VX ) VX = XM

       CALL XHORDR( VX, F2, ORD, IORD )

       IF( VMIN .LT. 0.0 ) THEN
           V1    = VMIN
           NUMBL = 1
           DO 150 I = 1, 80
              V2    = V1 + UNIT
              IF( V1 .LE. 0.0 .AND. V2 .GE. 0.0 ) THEN
                  NUMBL  = I
                  GO TO 180
              ENDIF
              V1    = V2
  150      CONTINUE
       ENDIF

  180  DO 300 I = 1,NBIN1
          VX   = VAL(I)
          IF( VMIN .GE. 0.0 ) THEN
              IF( VX .GT. 0.0 ) THEN
                  NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0
                  NUMB   = VX/UNIT + 1.0
              ELSE
                  NUMBL  = 0
                  NUMB   = 0
              ENDIF
              IF( NUMB .GT. 50 ) NUMB = 50
              IF( NUMBL.GT. 50 ) NUMBL= 50
              DO 200 K = 1,50
                 IF(     ( K .GT. NUMBL) .AND. (K .GT. NUMB ) ) THEN
                           IF( K .EQ. 50 ) THEN
                               CHARR(K:K) = AI
                           ELSE
                               CHARR(K:K) = BLNK
                           ENDIF
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .GT. NUMB )) THEN
                           CHARR(K:K) = OO
                 ELSEIF( ( K .GT. NUMBL) .AND. (K .LE. NUMB )) THEN
                           CHARR(K:K) = STAR
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .LE. NUMB)) THEN
                           IF( NUMB .GE. NUMBL ) THEN
                               CHARR(K:K) = OO
                           ELSE
                               CHARR(K:K) = STAR
                           ENDIF
                 ENDIF
  200         CONTINUE
          ELSE

              V1          = VMIN
              NHIG        = 1
              DO 220  J = 1, 50
                 V2     = V1 + UNIT
                 IF( VX .GE. V1 .AND. VX .LT. V2 ) THEN
                     NHIG   = J
                     GO TO 240
                 ENDIF
                 V1    = V2
  220         CONTINUE
  240         NLOW   = NUMBL
              IF( NHIG .LT. NLOW) THEN
                  NX    = NHIG
                  NHIG  = NLOW
                  NLOW  = NX
              ENDIF

              DO 250 K = 1, 49
                 IF(     K .EQ. NUMBL ) THEN
                         CHARR(K:K) = AI
                 ELSEIF( K .GT. NHIG ) THEN
                         CHARR(K:K) = BLNK
                 ELSEIF( K .LT. NLOW ) THEN
                         CHARR(K:K) = BLNK
                 ELSE
                     IF( K .EQ. NHIG .AND. K .EQ. NLOW) THEN
                         CHARR(K:K) = AI
                     ELSE
                         CHARR(K:K) = STAR
                     ENDIF
                 ENDIF
  250         CONTINUE
              CHARR(50:50) = AI
          ENDIF

             NX  = VAL(I)
             VX     = VAL(I)
             VX1    = VX
             IF( VX .LT. 0.0 ) VX1 = -VX
             CALL XHORDR( VX1, F2, ORDER, IORDR )
             F2     = VX/ORDER
             IF( I .EQ. NBIN1 ) THEN
                 WRITE(LU,9400) IORD,F2,IORDR,NX,CHARR
 9400            FORMAT(1X,'I  E',I3,' I',F6.3,'E',I3,'I',
     .                                            I7,'I',A50)
             ELSE
                   XM  = (XMIN + DEV*(I - 1))/ORD
                   WRITE(LU,9440) XM,F2,IORDR,NX,CHARR
 9440              FORMAT(1X,'I',F6.3,' I',F6.3,'E',I3,'I',
     .                                        I7,'I',A50)
             ENDIF

  300  CONTINUE

       IF( VMIN .GE. 0.0 ) THEN
           CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHAR1)
           VXMIN  = 10**VLMIN
       ENDIF

           WRITE(LU,9251) CHAR1
           WRITE(LU,9205) SCALE
           WRITE(LU,9360)
 9360      FORMAT(30X,'"O" : No. of Events in Log. scale.')

C

      RETURN
      END
***********************************************************************
*============================                                         *
      SUBROUTINE SPINFO( LU )
*============================                                         *
*((Purpose))                                                          *
*    Print the information for                                        *
*        (1) BASES parameters                                         *
*        (2) Computer time information                                *
*        (3) Convergency behavior of the Grid optimization step       *
*        (4) Convergency behavior of the integration step             *
*(( Input ))                                                          *
*    LU  :  Logical unit number of printer                            *
*                                                                     *
*           by S.Kawabata    March 1994 at KEK
*                                                                     *
***********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON /BDATE/ IDATE(3),ITIME(2)
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      COMMON /SPRNG2/ MXTRY,NEVENT, NTRIAL, MISS

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
*     COMMON/PLOTH/ XHASH(ILH,13),DHASH(IDH,14),IFBASE(ILH),
*    .              MAXL, NHIST, MAPL(4,ILH),
*    .              MAXD, NSCAT, MAPD(4,IDH),
*    .              NW
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW

      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)
      REAL*4 XTIME

      CHARACTER*1 CN

       IF( IPNT .EQ. 0 ) THEN
           WRITE(LU,9300)
       ELSE
           CN     = CHAR(12)
           WRITE(LU,9350) CN
       ENDIF
 9300  FORMAT(/1H1,////1H )
 9350  FORMAT(A1,////1X)
       WRITE(LU,9360) (IDATE(I),I=1,3),(ITIME(J),J=1,2)
 9360  FORMAT(55X,'Date: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
       WRITE(LU,9400)
 9400 FORMAT(
     . 8X,'**********************************************************',
     ./8X,'*                                                        *',
     ./8X,'*    SSSSS   PPPPPP   RRRRRR   IIIII  N    NN   GGGGG    *',
     ./8X,'*   SS   SS  PP   PP  RR   RR   III   NN   NN  GG   GG   *',
     ./8X,'*   SS       PP   PP  RR   RR   III   NNN  NN  GG        *',
     ./8X,'*    SSSSS   PPPPPP   RRRRR     III   NNNN NN  GG  GGGG  *',
     ./8X,'*        SS  PP       RR  RR    III   NN NNNN  GG   GG   *',
     ./8X,'*   SS   SS  PP       RR   RR   III   NN  NNN  GG   GG   *',
     ./8X,'*    SSSSS   PP       RR    RR IIIII  NN   NN   GGGGG    *',
     ./8X,'*                                                        *',
     ./8X,'*                  SPRING Version 5.1                    *',
     ./8X,'*           coded by S.Kawabata KEK, March 1994          *',
     ./8X,'**********************************************************')
*                                                                      *
          EFF   = FLOAT(NEVENT)/FLOAT(NTRIAL)*100.D0
          CALL BSTIME( RTIME, 1 )
          XTIME = RTIME - TIMES1
          WRITE(LU,9500) NEVENT,EFF,(TIMESP(I),I=0,2),XTIME,MXTRY,MISS
 9500     FORMAT(/5X,'Number of generated events    =',I10,
     .         /5X,'Generation efficiency         =',F10.3,' Percent',
     .         /5X,'Computing time for generation =',F10.3,' Seconds',
     .         /5X,'               for Overhead   =',F10.3,' Seconds',
     .         /5X,'               for Others     =',F10.3,' Seconds',
     .         /5X,'GO time for event generation  =',F10.3,' Seconds',
     .         /5X,'Max. number of trials MXTRY   =',I10,' per event',
     .         /5X,'Number of miss-generation     =',I10,' times')

      CALL SPHIST( LU )

      RETURN
      END
C***********************************************************************
C*====================================                                 *
C* SUBROUTINE SPRGEN( F, MXTRY, NTRY )                                 *
C*====================================                                 *
C*                                                                     *
C*     Generation of events according to the probability density       *
C*     which is stored in a disk file.                                 *
C*                                                                     *
C*    Coded   by S.Kawabata   at July,1980                             *
C*    Update     S.Kawabata   September '84                            *
C*                                                                     *
C***********************************************************************
C
       SUBROUTINE SPRGEN(F,MXTRY,NTRY)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      EXTERNAL F
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
     .               ND,NG,NPG,MA(MXDIM)

      COMMON /SPRNG1/ XND, DXG, XJAC, DXMAX, NSP

      DIMENSION Y(MXDIM),KG(MXDIM)
      DATA ONE/1.0D0/
C
C
      RX    = DRN(DUMY)*DXMAX
C
C  -------------- Binary Search  --------------------------------
C
      IPMIN = 1
      IPMAX = NSP
C
 300  IC    = (IPMIN+IPMAX)/2
        IF(RX .LT. DXD(IC)) THEN
          IPMAX = IC
        ELSE
          IPMIN = IC
        ENDIF
      IF(IPMAX-IPMIN .GT.  2) GO TO 300
C
      IC    = IPMIN-1
 350  IC    = IC+1
      IF(DXD(IC) .LT. RX) GO TO 350
C
C --------------------------------------------------------------------
C      Identify the hypecube number from sequential number IC
C --------------------------------------------------------------------
C
       FMAX  = DXP(IC)
C
       IX    = IC-1

       KG(NWILD) = IX/MA(NWILD) + 1
       IF( NWILD .GT. 1 ) THEN
           DO 400 J = 1,NWILD-1
              NUM   = MOD(IX,MA(J+1))
              KG(J) = NUM/MA(J) + 1
  400      CONTINUE
       ENDIF
C
C  ------------------------------------------------------------------
C                     Sample and test a event
C  ------------------------------------------------------------------
C
      DO 600 NTRY = 1,MXTRY
        WGT   = XJAC
        DO 550 J=1,NDIM
          IF( J .LE. NWILD) THEN
             XN    = (KG(J)-DRN(DUMY))*DXG+ONE
          ELSE
             XN    = ND*DRN(DUMY) + ONE
          ENDIF
          IAJ   = XN
          IF(IAJ .EQ. 1) THEN
            XO    = XI(IAJ,J)
            RC    = (XN-IAJ)*XO
          ELSE
            XO    = XI(IAJ,J)-XI(IAJ-1,J)
            RC    = XI(IAJ-1,J)+(XN-IAJ)*XO
          ENDIF
          Y(J)  = XL(J) + RC*DX(J)
          WGT   = WGT*XO*XND
  550   CONTINUE
C
*       FX    = F(Y)*WGT
        FF    = F(Y)
        FX    = FF*WGT
        FUNCT = FX/FMAX
C
        IF( FX .GT. 0.0D0 ) THEN
*           IF( DRN(DUMY) .LE. FUNCT ) GO TO 700
            XJ = DRN(DUMY)
            IF( XJ .LE. FUNCT ) GO TO 700
*           IF( XJ .LE. FUNCT ) THEN
*               WRITE(6,9999) NTRY,IC,FF,WGT,XJ,FUNCT
*9999           FORMAT(1X,'NTRY,IC,FF,WGT,XJ,FUNCT = ',2I5,4E12.4)
*               GO TO 700
*           ENDIF
        ELSE
     .  IF( FX .LT. 0.0D0 ) THEN
            WRITE(6,9100) IC
 9100       FORMAT(
     .      /5X,'********** FATAL ERROR IN SPRING **********',
     .      /5X,'* A negative value of function was found  *',
     .      /5X,'*        in the ',I6,'-th Hypercube.      *',
     .      /5X,'*******************************************')
            WRITE(6,9405)
 9405       FORMAT(5X,'------',3('+---------------'),'+')
            WRITE(6,9410)
 9410       FORMAT(5X,'    i       XL(i)             X       ',
     .                '     XU(i)')
            WRITE(6,9405)
            DO 450 I = 1,NDIM
                WRITE(6,9420) I,XL(I),Y(I),XU(I)
 9420           FORMAT(5X,I5,1P,3('  ',E14.6))
  450       CONTINUE
            WRITE(6,9405)
            STOP
        ENDIF
C
        CALL SHCLER
C
  600 CONTINUE

      NTRY  = MXTRY + 1

  700 RETURN
      END
************************************************************************
*    ==================================                                *
      SUBROUTINE SPRING(FUNC, MXTRY )
*    ==================================                                *
*         Main Program for the Event generation program SPRING.        *
*                                                                      *
*        Coded by S.Kawabata        September '84                      *
*                                                                      *
************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      EXTERNAL FUNC
      COMMON /BASE0/ NDUM,IBASES
      PARAMETER (MXDIM = 50, NDMX = 50, LENG = 32768)
      COMMON /BASE1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,
     .               IG(MXDIM),NCALL
      COMMON /BASE4/ XI(NDMX,MXDIM),DX(MXDIM),DXD(LENG),DXP(LENG),
     .               ND,NG,NPG,MA(MXDIM)
      COMMON /BDATE/ IDATE(3),ITIME(2)
      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      COMMON /SPRNG1/ XND, DXG, XJAC, DXMAX, NSP
      COMMON /SPRNG2/ MXTRYP,NEVENT, NTRIAL,MISS

      REAL*4 TIMEBS,TIMINT,TIMESP,TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME1/ TIME0,RTIME,TIMEB1,TIMEB2,TIMES1
      COMMON /BTIME2/ TIMEBS(0:2),TIMINT,TIMESP(0:2)
*                                                                      *
*----------------------------- Entry point ----------------------------*
*                                                                      *
*======================================================================*
*                  Initialization of the program                       *
*======================================================================*
*----------------------------------------------------------------------*
*                     initialize timer etc.                            *
*----------------------------------------------------------------------*
*                                                                      *
       IF( IBASES .GT. 0 ) THEN

           CALL SPCHCK

           CALL BSTIME( TIME0, 0 )
           TIMES1 = TIME0

           MXTRYP = MXTRY
           INTV   = 0
           IBASES = 0
           MISFLG = 0

           CALL BSDATE

           DO 10 I = 0,2
              TIMESP(I) = 0.0
   10      CONTINUE
*                                                                      *
            IF( MXTRY .LT. 10 ) MXTRY = 50
            NBIN    = MXTRY
            IF( MXTRY .GT. 50) NBIN = 50
            MXTRY1  = MXTRY + 1
            MISS    = 0
            NEVENT  = 0
            NTRIAL  = 0

            CALL SHINIT( MXTRY1 )

*           -------------
             CALL SHRSET
*            -------------
*----------------------------------------------------------------------*
*             Make the cumulative probability distribution             *
*----------------------------------------------------------------------*
*                                                                      *
            XND     = ND
            DXG     = XND/NG
            NSP     = NG**NWILD

*///// DEBUG
*       MCALL   = NSP*NPG
*       CALL BSPRNT( 4, MCALL, IDUM2, IDUM3, IDUM4 )
*
            XJAC    = 1.0
            DO 50 I = 1, NDIM
               XJAC = XJAC*DX(I)
   50       CONTINUE
            DXMAX   = 0.0D0
            DO 100  I = 1,NSP
               IF( DXD( I ) .LT. 0.0D0 ) THEN
                   WRITE(6,9100) I
 9100              FORMAT(
     .             /5X,'********** FATAL ERROR IN SPRING **********',
     .             /5X,'*     Negative probability was found      *',
     .             /5X,'*        in the ',I6,'-th Hypercube.      *',
     .             /5X,'*******************************************')
                   STOP
               ENDIF

               DXMAX    = DXMAX + DXD( I )
               DXD(I)   = DXMAX
  100       CONTINUE
*        =====================
          CALL BSUTIM( 1, 1 )
*        =====================
      ENDIF
*     =====================
       CALL BSUTIM( 1, 2 )
*     =====================
      IF( IBASES .EQ. 1 ) THEN
          WRITE(6,9000)
 9000     FORMAT(
     .      1X,'**************************************************',
     .     /1X,'*    Flag IBASES was not equal to "0".           *',
     .     /1X,'*                                                *',
     .     /1X,'*   Process was terminated by this error.        *',
     .     /1X,'*   Call S.Kawabata.                             *',
     .     /1X,'**************************************************')
           STOP
       ENDIF
*                                                                      *
*======================================================================*
*                       Event generation                               *
*======================================================================*
*     =====================
  500  CALL BSUTIM( 1, 1 )
*     =====================

*     ==================================
        CALL SPRGEN( FUNC, MXTRY, IRET)
*     ==================================

*     =====================
       CALL BSUTIM( 1, 0 )
*     =====================

      CALL SHFILL( IRET )

      IF( IRET .LE. MXTRY ) THEN
          NTRIAL =NTRIAL + IRET
          NEVENT = NEVENT + 1
          CALL SHUPDT
      ELSE
          NTRIAL =NTRIAL + IRET - 1
          MISS = MISS + 1
          IF( MISFLG .EQ. 0 .AND. MISS .GT. MXTRY ) THEN
              WRITE(6,9600) MXTRY
 9600         FORMAT(1X,'****************************************',
     .                  '****************************************',
     .              /1X,'* (((( Warning ))))                     ',
     .                  '                                       *',
     .              /1X,'*                                       ',
     .                  '                                       *',
     .              /1X,'*  The number of mis-generations is foun',
     .                  'd more than',I3,' times.                  *')
              WRITE(6,9610)
 9610         FORMAT(1X,'*                                       ',
     .                  '                                       *',
     .              /1X,'*(( Suggestion ))                       ',
     .                  '                                       *',
     .              /1X,'* (1) Try integration again with larger ',
     .                  'number of sample points than this job. *',
     .              /1X,'* or                                    ',
     .                  '                                       *',
     .              /1X,'* (2) The integral variables are not sui',
     .                  'ted for the function.                  *',
     .              /1X,'*     Take another integral variables !!',
     .                  '                                       *',
     .              /1X,'*                                       ',
     .                  '                                       *',
     .              /1X,'****************************************',
     .                  '****************************************')
            MISFLG = 1
          ENDIF
          GO TO 500
      ENDIF
*     =====================
  600  CALL BSUTIM( 1, 1 )
*     =====================

      RETURN
      END
************************************************************************
*    =======================                                           *
      SUBROUTINE XHCHCK(LU)
*    =======================                                           *
* ((Purpose))                                                          *
*      To check the contents of the histogram table                    *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    June '90                                         *
*                                                                      *
************************************************************************

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP
      CHARACTER*1 CN

      CN  = CHAR(12)

      IF( IPNT .EQ. 0 ) THEN
          WRITE(LU,9000)
      ELSE
          WRITE(LU,9010) CN
      ENDIF
 9000 FORMAT(/1H1)
 9010 FORMAT(A1)

      WRITE(LU,9050) NW
 9050 FORMAT(
     . //5X,'*********  Contents of the histogram Header *********',
     .     //1X,'(1) Actual Buffer size     = ',I6,' Words')
      WRITE(LU,9100) NHS,NHIST
 9100 FORMAT(1X,'(2) Contents of Histograms ',
     .      /1X,'    Max. No. of Histograms = ',I6,
     .      /1X,'    Number   of Histograms = ',I6)

      IF( NHIST .GT. 0 ) THEN
          WRITE(LU,9200)
 9200     FORMAT(1X,'   ID     X_min        X_max    X_bin',
     .              ' Hash Hst#')
          DO 200 I = 1, 13
             NT    = XHASH(1,I)
             IF( NT .GT. 0 ) THEN
                 DO 100 J = 2, NT+1
                    K     = XHASH(J,I)
                    IP1   = MAPL(2,K)
                    IP3   = MAPL(4,K)
                    XMIN  = BUFF(IP1)
                    XMAX  = BUFF(IP1+1)
                    NBIN  = IBUF(IP1+2)
                    WRITE(LU,9300) MAPL(1,K),XMIN,XMAX,NBIN,I,NT,K
 9300               FORMAT(1X,I5,1X,1PE12.4,1X,E12.4,I5,2I3,I5)
  100            CONTINUE
             ENDIF
  200     CONTINUE
      ENDIF

      WRITE(LU,9400) NSC,NSCAT
 9400 FORMAT(1X,'(3) Contents of Scatter Plots',
     .      /1X,'    Max. No. of Scat_Plots = ',I6,
     .      /1X,'    Number   of Scat_Plots = ',I6)

      IF( NSCAT .GT. 0 ) THEN
          WRITE(LU,9500)
 9500     FORMAT(1X,'   ID      X_min   ',
     .              '     X_max   X-Bin    Y_min   ',
     .              '     Y_max   Y_Bin Hash Hst#')
          DO 400 I = 1, 13
             NT    = DHASH(1,I)
             IF( NT .GT. 0 ) THEN
                 DO 300 J = 2, NT+1
                    K     = DHASH(J,I)
                    IP1   = MAPD(2,K)
                    IP3   = MAPD(4,K)
                    XMIN  = BUFF(IP1)
                    XMAX  = BUFF(IP1+1)
                    NXBN  = IBUF(IP1+2)
                    YMIN  = BUFF(IP1+4)
                    YMAX  = BUFF(IP1+5)
                    NYBN  = IBUF(IP1+6)
                    WRITE(LU,9600) MAPD(1,K),XMIN,XMAX,NXBN,
     .                            YMIN,YMAX,NYBN,I,NT,K
 9600               FORMAT(1X,I5,1X,1PE12.4,1X,E12.4,I5,
     .                                 E12.4,1X,E12.4,I5,2I3,I5)
  300            CONTINUE
             ENDIF
  400     CONTINUE
      ENDIF
      RETURN
      END
************************************************************************
*    ==============================                                    *
      SUBROUTINE XHFILL(ID, DX, FX )
*    ==============================                                    *
* ((Function))                                                         *
*     To fill histograms.                                              *
*   This routine identifies the bin number which is to be updated      *
*   with weight FX*WGT.  Up to five points per histogram are able      *
*   to be stacked before calling BHUPDT or SHUPDT.                     *
* ((Input))                                                            *
*   ID    : Histogram identification number                            *
*   DX    : Input value                                                *
*   FX    : Input value of the function                                *
* ((Author))                                                           *
*   S.Kawabata         June '90 at KEK                                 *
*                                                                      *
************************************************************************

      REAL*8 DX,FX
      COMMON /BASE0/ IFLAG,IBASES
      REAL*8         SCALLS,WGT,TI,TSI,TACC
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

C     COMMON/PLOTLU/ LU
C
      IF( NHIST .GT. 0 ) THEN
C
          I  = IABS(MOD( ID, 13 )) + 1
          IF( XHASH(1, I) .EQ. 1 ) THEN
            IF( ID .EQ. MAPL( 1, XHASH(2,I))) THEN
                IHIST = XHASH(2,I)
                GO TO 200
            ENDIF
          ELSEIF( XHASH(1, I) .GT. 1 ) THEN
            DO 100 K = 2, XHASH(1,I)+1
               IF( ID .EQ. MAPL( 1, XHASH(K,I))) THEN
                   IHIST = XHASH(K,I)
                   GO TO 200
               ENDIF
  100       CONTINUE
          ENDIF
      ENDIF
C     IF( LU .GT. 0 ) THEN
C         WRITE(LU,9000) ID
C     ENDIF
C9000 FORMAT(1X,'No Histogram corresponds to ID =',I5,
C    .      /1X,' This call is neglected.')
      RETURN
C

  200 X     = DX*1.0

          IX    = -1
          IP1   = MAPL(2,IHIST)
          XMIN  = BUFF(IP1)
          XMAX  = BUFF(IP1+1)
          NXBIN = IBUF(IP1+2)
          DEV   = BUFF(IP1+3)
          IF(     X .LT. XMIN ) THEN
                  IX   = 0
          ELSEIF( X .GT. XMAX ) THEN
                 IX   = NXBIN + 1
          ELSE
                 IX   = INT((X - XMIN)/DEV + 1.0)
                 IF( IX .GT. NXBIN ) IX = NXBIN
          ENDIF
C        PRINT*,'ID, IHIST, IFBASE =',ID,IHIST,(IFBASE(I),I=1,NHIST)

      IF( IBASES .EQ. 1 ) THEN

          IP2       = MAPL(3,IHIST) + IX
          IBUF(IP2) = IBUF(IP2) + 1
          FXWGT     = FX*WGT
          IP2       = IP2 + 52
          BUFF(IP2) = BUFF(IP2) + FXWGT
          IP2       = IP2 + 52
          BUFF(IP2) = BUFF(IP2) + FXWGT*FXWGT
*   Add March 1994
          IFBASE(IHIST) = 1

      ELSE
C        PRINT*,'ID, IHIST, IFBASE =',ID,IHIST,(IFBASE(I),I=1,NHIST)

         IP3        =  MAPL(4,IHIST)
         IBUF(IP3)  = IX

      ENDIF

C
      RETURN
      END
************************************************************************
*    ============================================                      *
      SUBROUTINE XHINIT(ID,DXMIN,DXMAX,NBIN,TNAME)
*    ============================================                      *
* ((Function))                                                         *
*     To define a histogram.                                           *
* ((Input))                                                            *
*    ID   : Histogram identification number                            *
*    DXMIN: Lower limit of the histogram                               *
*    DXMAX: Upper limit of the histogram                               *
*    NBIN : Number of bins for the histogram (Max. is 50 )             *
*    TNAME: Title of the histogram in the character string (upto 64    *
*            characters)                                               *
* ((Author))                                                           *
*    S.Kawabata    June '90                                            *
*                                                                      *
************************************************************************

      REAL*8 DXMIN, DXMAX
      CHARACTER*(*) TNAME
      CHARACTER*68  NAME

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

*     COMMON/XHCNTL/ LOCK
      COMMON/PLOTLU/ LU

      IF( NHIST .GE. NHS ) THEN
*         IF( LOCK .NE. 0 ) RETURN
          IF( LU .GT. 0 ) THEN
              WRITE(LU,9000) NHIST,ID
          ENDIF
 9000     FORMAT(1X,'Number of Histograms exceeds ',I3,' at ID = ',I3,
     .            /1X,'This call is neglected.')
          RETURN
      ENDIF

      IEXIST = 0
      I  = IABS(MOD( ID, 13 )) + 1
      NH = XHASH(1,I)

      IF( NH .EQ. 1 ) THEN
            IF( ID .EQ. MAPL( 1, XHASH(2,I))) THEN
*               IF( LOCK .NE. 0 ) RETURN
                IEXIST = XHASH(2,I)
            ENDIF
      ELSEIF( NH .GT. 1 ) THEN
          DO 100 K = 2, NH+1
            IF( ID .EQ. MAPL( 1, XHASH(K,I))) THEN
*               IF( LOCK .NE. 0 ) RETURN
                IEXIST = XHASH(K,I)
                GO TO 110
            ENDIF
  100    CONTINUE
  110    CONTINUE
      ENDIF
      XMIN  = DXMIN*1.0
      XMAX  = DXMAX*1.0

      IF( IEXIST .GT. 0 ) THEN
          IF( LU .GT. 0 ) THEN
              WRITE(LU,9100) ID
          ENDIF
 9100     FORMAT(1X,'Histogram ID (',I3,' ) exists already.')
          IP1    =  MAPL(2,IEXIST)
          IF(( XMIN .EQ. BUFF(IP1))   .AND.
     .       ( XMAX .EQ. BUFF(IP1+1)) .AND.
     .       ( NBIN .EQ. IBUF(IP1+2)) )    THEN
               IF( LU .GT. 0 ) THEN
                   WRITE(LU,9110)
               ENDIF
 9110          FORMAT(1X,' This call is neglected.')
               RETURN
          ENDIF
          IF( LU .GT. 0 ) THEN
              WRITE(LU,9120) ID,XMIN,XMAX,NBIN
          ENDIF
 9120     FORMAT(1X,'Histogram ( ID =',I3,' ) parameters are replaced',
     .          /1X,'by the following new parameters :',
     .          /1X,' XMIN(',E12.5,')  XMAX(',E12.5,' )  NBIN(',I4,' )')
      ENDIF

      IF((NHIST .GE. NHS) .AND. (ID .GT. 0) ) THEN
*         IF( LOCK .NE. 0 ) RETURN
          IF( LU .GT. 0 ) THEN
              WRITE(LU,9000) NHS,ID
          ENDIF
         RETURN
      ENDIF

      IF(NBIN  .GT. 50 ) THEN
         IF( LU .GT. 0 ) THEN
             WRITE(LU,9200) NBIN,ID
         ENDIF
 9200    FORMAT(1X,'Bin size (',I3,' )  exceeds 50 at ID =',I5,
     .         /1X,' This call is neglected.')
         RETURN
      ENDIF
      IF(XMIN  .GE. XMAX ) THEN
         IF( LU .GT. 0 ) THEN
             WRITE(LU,9300) ID
         ENDIF
 9300    FORMAT(1X,'Lower limit is larger than upper at ID =',I5,
     .         /1X,' This call is neglected.')
         RETURN
      ENDIF
      IF(XHASH(1,I) .GE. NHS) THEN
         IF( LU .GT. 0 ) THEN
             WRITE(LU,9400) I
         ENDIF
 9400    FORMAT(1X,I5,'-th Hash table overflow',
     .         /1X,' This call is neglected.')
         RETURN
      ENDIF

      IF( IEXIST .GT. 0 ) THEN
          NHST     = IEXIST
      ELSE
          NHIST        = NHIST + 1
          XHASH(1,I)   = XHASH(1,I) + 1
          K            = XHASH(1,I) + 1
          XHASH(K,I)   = NHIST
          NHST         = NHIST
          IP1    = NW + 1
          NW  = NW + 281
          MAPL(1,NHST)  = ID
          MAPL(2,NHST)  = IP1
      ENDIF
         BUFF(IP1     ) = XMIN
         BUFF(IP1 +  1) = XMAX
         IBUF(IP1 +  2) = NBIN
         DEV            = XMAX - XMIN
         BUFF(IP1 +  3) = DEV/NBIN
      IP2   = IP1 + 4
         MAPL(3,NHST)  = IP2
      IP3   = IP1 + 264
         MAPL(4,NHST)  = IP3
         IBUF(IP3)     = -1

         I1   = IP3 + 1
         I2   = I1 + 15
         NAME = TNAME
         READ(NAME,9800) (BUFF(I),I=I1,I2)
 9800    FORMAT(16A4)

C
 1000 CONTINUE
      RETURN
      END
C***********************************************************************
C*                                                                     *
C*=============================================                        *
C*    SUBROUTINE XHORDR( VAL, F2, ORDER, IORDR)                        *
C*=============================================                        *
C*((Function))                                                         *
C*    To resolve the real number VAL into mantester and exponent parts.*
C*  When VAL = 1230.0 is given, output are                             *
C*        F2 = 1.2  and ORDER = 4.0.                                   *
C*((Input))                                                            *
C*  VAL  : Real*4 value                                                *
C*((Output))                                                           *
C*  F2   : The upper two digits is given                               *
C*  ORDER: Order is given                                              *
C*  IORDR: Exponent is given                                           *
C*((Author))                                                           *
C*  S.Kawabata                                                         *
C*                                                                     *
C***********************************************************************

      SUBROUTINE XHORDR(VAL, F2, ORDER, IORDR)

      IF( VAL .NE. 0.0 ) THEN
          ORDER    =  LOG10( VAL )
          IORDR    =  INT( ORDER )
          IF( ORDER .LT. 0.0 ) IORDR = IORDR - 1
          ORDER  = 10.0**IORDR
          F2     = VAL/ORDER
      ELSE
          IORDR  = 0
          ORDER  = 1.0
          F2    = 0.0
      ENDIF

      RETURN
      END
************************************************************************
*    =====================================                             *
      SUBROUTINE XHPLOT( LU, IFG, IHIST )
*    =====================================                             *
* ((Purpose))                                                          *
*      To print histograms for BASES and SPRING.                       *
* ((Input))                                                            *
*      IFG  : Flag which indicats whether this is called by BASES      *
*             or SPRING.  IFG = ( 0 / anyother) = ( By BASES/ SPRING)  *
*      IHIST: Serial number of the histogram                           *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata    June '90 at KEK                                  *
*     Last update     March '94                                        *
*                                                                      *
************************************************************************

      REAL*8         SCALLS,WGT,TI,TSI,TACC
      COMMON /BASE3/ SCALLS,WGT,TI,TSI,TACC,IT

      PARAMETER ( NHS = 50, NSC = 50 )
      INTEGER*4 XHASH,DHASH,NHIST,MAPL,IFBASE,NSCAT,MAPD
      COMMON/PLOTH/ XHASH(NHS+1,13),DHASH(NSC+1,14),IFBASE(NHS),
     .              NHIST, MAPL(4,NHS),
     .              NSCAT, MAPD(4,NSC),
     .              NW
      COMMON /PLOTB/ IBUF( 281*NHS + 2527*NSC )
      REAL*4         BUFF( 281*NHS + 2527*NSC )
      EQUIVALENCE (IBUF(1),BUFF(1))

      COMMON /BSCNTL/ INTV, IPNT, NLOOP, MLOOP

      REAL  VAL(0:51),VLOG(0:51),VERR(0:51)
      CHARACTER*50 CHARR,CHAR1
      CHARACTER*52 SCALE
      CHARACTER*1  BLNK,STAR,OO,AI,CN
      DATA  YMAX / 50/
      DATA  BLNK /' '/, STAR /'*'/, OO /'O'/, AI /'I'/

      CN    = CHAR(12)
      IP3   = MAPL(4,IHIST)
      IF(     IFG .EQ. 0 ) THEN
            IF( IPNT .EQ. 0 ) THEN
                WRITE(LU,9000)
            ELSE
                WRITE(LU,9005) CN
            ENDIF
 9000       FORMAT(/1H1)
 9005       FORMAT(A1)
            WRITE(LU,9100) MAPL(1,IHIST),(BUFF(I),I=IP3+1,IP3+16)
 9100       FORMAT(1X,'Histogram (ID =',I3,' ) for ',16A4)
      ELSEIF( IFG .EQ. -10 ) THEN
            IF( IPNT .EQ. 0 ) THEN
                WRITE(LU,9000)
            ELSE
                WRITE(LU,9005) CN
            ENDIF
            WRITE(LU,9102) (BUFF(I),I=IP3+1,IP3+16)
 9102       FORMAT(5X,16A4)
      ELSE
            IF( IPNT .EQ. 0 ) THEN
                WRITE(LU,9000)
            ELSE
                WRITE(LU,9005) CN
            ENDIF
            WRITE(LU,9105) MAPL(1,IHIST),(BUFF(I),I=IP3+1,IP3+16)
 9105       FORMAT(
     .      1X,'Additional Histogram (ID =',I3,' ) for ',16A4)
      ENDIF

      IP1   = MAPL(2,IHIST)
      XMIN  = BUFF(IP1)
      XMAX  = BUFF(IP1+1)
      NXBIN = IBUF(IP1+2) + 1
      DEV   = BUFF(IP1+3)
      IP2   = MAPL(3,IHIST)

      IF( IFG .EQ. 0 ) THEN
C         NTOTAL     = SCALLS
          FACT       = 1./(SCALLS*DEV)
          IPF   = IP2 + 156
          IPF2  = IPF + 52
          VAL(0)     = BUFF(IPF)/SCALLS
          VAL(NXBIN) = BUFF(IPF+NXBIN)/SCALLS
          VMAX       = FACT*BUFF(IPF+1)
          VMIN       = VMAX
          DO  50 I   = 1,NXBIN-1
              TX     = BUFF(I+IPF)
              NX     = IBUF(I+IP2)
              VLS    = TX*FACT
              IF( VMAX .LT. VLS ) VMAX = VLS
              IF( VMIN .GT. VLS ) VMIN = VLS
              VAL(I) = VLS
              IF( NX .GT. 1 ) THEN
                  DEV2   =  NX*BUFF(I+IPF2)-TX*TX
                  IF( DEV2 .LE. 0.0 ) THEN
                      VERR(I)= 0.0
                  ELSE
                      VERR(I)= FACT*SQRT( DEV2/( NX-1 ))
                  ENDIF
*TI           ELSEIF( NX .EQ. 1 ) THEN
*TI               VERR(I)= VLS
              ELSE
                  VERR(I)= 0.0
              ENDIF

   50     CONTINUE
      ELSE
          IPX   = IP2 + 52
          VAL(0)     = BUFF(IPX)
          VAL(NXBIN) = BUFF(IPX+NXBIN)
          NTOTAL     = INT(VAL(0)) + INT(VAL(NXBIN))
          VMIN       = 0.0
          VMAX       = VMIN
          DO  55 I   = 1,NXBIN-1
              VLS    = BUFF(I+IPX)
              NTOTAL = INT(VLS) + NTOTAL
              IF( VMAX .LT. VLS ) VMAX = VLS
              VAL(I) = VLS
              IF( VLS .GT. 0.0 ) THEN
                  VERR(I) = SQRT(VLS)
              ELSE
                  VERR(I) = 0.0
              ENDIF
   55     CONTINUE
       ENDIF
***
       IF( VMAX .EQ. 0.0 .AND. VMIN .EQ. 0.0) THEN
           V0 = VAL(0)
           VM = VAL(NXBIN)
           IF( V0 .GE. 0.0 .AND. VM .GE. 0.0 ) THEN
               VMIN  = 0.0
               IF( V0 .GT. VM  ) THEN
                   VMAX = V0
               ELSE
                   VMAX = VM
               ENDIF
           ELSEIF( V0 .LT. 0.0 .AND. VM .LT. 0.0 ) THEN
               VMAX  = 0.0
               IF( V0 .LT. VM ) THEN
                   VMIN  = V0
               ELSE
                   VMIN  = VM
               ENDIF
           ELSEIF( V0 .GT. VM ) THEN
               VMAX  = V0
               VMIN  = VM
           ELSE
               VMAX  = VM
               VMIN  = V0
           ENDIF
       ENDIF
***
       IF( VMIN .GE. 0.0 ) THEN
C//VV
           IF( VMAX .GT. 0.0 ) THEN
               VLMAX = LOG10(VMAX)
           ELSE
               VLMAX = 2.0
           ENDIF
C//
           VLMIN = VLMAX
           DO  60 I = 0,NXBIN
               IF( VAL(I) .GT. 0.0 ) THEN
                   VLS   = LOG10( VAL(I) )
                   IF( I .GT. 0 .AND. I .LT. NXBIN ) THEN
                       IF( VLS .LT. VLMIN ) VLMIN = VLS
                   ENDIF
                   VLOG(I)  = VLS
C//VV
C              ELSE
C                  VLOG(I)  = 0.0
               ENDIF
   60      CONTINUE

           IF( VLMIN .LT. 0.0) THEN
               VXMIN = IFIX(VLMIN) - 1.0
           ELSE
               VXMIN = IFIX(VLMIN)
           ENDIF
           VXMAX = VLMAX
           IFLG  = 1
           CALL XHRNGE( IFLG, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP )
           UNITL = (VLMAX-VLMIN)/YMAX

       ENDIF

       IFLG   = 0
       IF( VMAX .GT. 0.0 ) THEN
           IF( VMIN .GE. 0.0 ) THEN
               VXMAX  = 1.2*VMAX
               VXMIN  = 0.0
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )
           ELSE
               VXMAX  = 1.1*VMAX
               VXMIN  = 1.1*VMIN
               CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )
           ENDIF
       ELSE
          VXMAX  = 0.0
          VXMIN  = 1.1*VMIN
          CALL XHRNGE( IFLG, VXMIN, VXMAX, VMIN, VMAX, VSTP )
       ENDIF

       UNIT  = (VMAX-VMIN)/YMAX

       CALL XHSCLE( IFLG, VMIN, VMAX, VSTP, UNIT, SCALE, CHAR1 )
C
C
       IF( IFG .EQ. 0 ) THEN
           WRITE(LU,9150)
 9150      FORMAT(30X,'Linear Scale indicated by "*"')
           WRITE(LU,9200) SCALE
 9200      FORMAT(1X,'    x      d(Sigma)/dx    ',A52)
           WRITE(LU,9250) CHAR1
 9250      FORMAT(1X,
     .                '+-------+------------------+',
     .           A50 )
       ELSE
             WRITE(LU,9210) NTOTAL
 9210        FORMAT(1X,'Total =',I10,' events',
     .        3X,'"*" : No. of events in Linear scale.')
             WRITE(LU,9205) SCALE
 9205        FORMAT(1X,'   x      Lg(dN/dx)  dN/dx',A52)
             WRITE(LU,9251) CHAR1
 9251        FORMAT(1X,
     .             '+-------+----------+-------+',
     .       A50 )
       ENDIF

       VX    = ABS(XMAX)
       XM    = ABS(XMIN)
       IF( XM .GT. VX ) VX = XM

       CALL XHORDR( VX, F2, ORD, IORD )

       IF( VMIN .LT. 0.0 ) THEN
           V1    = VMIN
           NUMBL = 1
           DO 150 I = 1, 80
              V2    = V1 + UNIT
              IF( V1 .LE. 0.0 .AND. V2 .GE. 0.0 ) THEN
                  NUMBL  = I
                  GO TO 180
              ENDIF
              V1    = V2
  150      CONTINUE
       ENDIF

  180  DO 300 I = 0,NXBIN
          VX   = VAL(I)
          IF( VMIN .GE. 0.0 ) THEN
              IF( VX .GT. 0.0 ) THEN
                  NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0
                  NUMB   = VX/UNIT + 1.0
              ELSE
                  NUMBL  = 0
                  NUMB   = 0
              ENDIF
              IF( NUMB .GT. 50 ) NUMB = 50
              IF( NUMBL.GT. 50 ) NUMBL= 50
              DO 200 K = 1,50
                 IF(     ( K .GT. NUMBL) .AND. (K .GT. NUMB ) ) THEN
                           IF( K .EQ. 50 ) THEN
                               CHARR(K:K) = AI
                           ELSE
                               CHARR(K:K) = BLNK
                           ENDIF
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .GT. NUMB )) THEN
                           CHARR(K:K) = OO
                 ELSEIF( ( K .GT. NUMBL) .AND. (K .LE. NUMB )) THEN
                           CHARR(K:K) = STAR
                 ELSEIF( ( K .LE. NUMBL) .AND. (K .LE. NUMB)) THEN
                           IF( NUMB .GE. NUMBL ) THEN
                               CHARR(K:K) = OO
                           ELSE
                               CHARR(K:K) = STAR
                           ENDIF
                 ENDIF
  200         CONTINUE
          ELSE

              V1          = VMIN
              NHIG        = 1
              DO 220  J = 1, 50
                 V2     = V1 + UNIT
                 IF( VX .GE. V1 .AND. VX .LT. V2 ) THEN
                     NHIG   = J
                     GO TO 240
                 ENDIF
                 V1    = V2
  220         CONTINUE
  240         NLOW   = NUMBL
              IF( NHIG .LT. NLOW) THEN
                  NX    = NHIG
                  NHIG  = NLOW
                  NLOW  = NX
              ENDIF

              DO 250 K = 1, 49
                 IF(     K .EQ. NUMBL ) THEN
                         CHARR(K:K) = AI
                 ELSEIF( K .GT. NHIG ) THEN
                         CHARR(K:K) = BLNK
                 ELSEIF( K .LT. NLOW ) THEN
                         CHARR(K:K) = BLNK
                 ELSE
                     IF( K .EQ. NHIG .AND. K .EQ. NLOW) THEN
                         CHARR(K:K) = AI
                     ELSE
                         CHARR(K:K) = STAR
                     ENDIF
                 ENDIF
  250         CONTINUE
              CHARR(50:50) = AI
          ENDIF

          IF( IFG .EQ. 0 ) THEN

              NX     = IBUF(I+IP2)
              VX     = VAL(I)
              VX1    = VX
              IF( VX .LT. 0.0 ) VX1 = -VX


              IF( I .EQ. 0 .OR. I. EQ. NXBIN ) THEN
                  CALL XHORDR( VX1, F2, ORDER, IORDR )
                  F2     = VX/ORDER
                  WRITE(LU,9300) IORD,F2,IORDR,CHARR
 9300             FORMAT(1X,'I  E',I3,' I',F6.3,8X,'E',I3,
     .                                     'I',A50)
              ELSE
                  XM    = (XMIN + DEV*(I-1))/ORD
                  VE     = VERR(I)
                  IF( VE .GT. VX1 ) THEN
                      CALL XHORDR(  VE, F2, ORDER, IORDR )
                  ELSE
                      CALL XHORDR( VX1, F2, ORDER, IORDR )
                  ENDIF
                  F2   = VX/ORDER
                  VE   = VE/ORDER
                  WRITE(LU,9340) XM,F2,VE,IORDR,CHARR
 9340             FORMAT(1X,'I', F6.3,' I',F6.3,'+-',F5.3,' E',I3,
     .                                    'I',A50)
             ENDIF
          ELSE
             NX  = VAL(I)
             VX     = VAL(I)
             VX1    = VX
             IF( VX .LT. 0.0 ) VX1 = -VX
             CALL XHORDR( VX1, F2, ORDER, IORDR )
             F2     = VX/ORDER
             IF( I .EQ. 0 .OR. I .EQ. NXBIN ) THEN
                 WRITE(LU,9400) IORD,F2,IORDR,NX,CHARR
 9400            FORMAT(1X,'I  E',I3,' I',F6.3,'E',I3,'I',
     .                                            I7,'I',A50)
             ELSE
                   XM  = (XMIN + DEV*(I - 1))/ORD
                   WRITE(LU,9440) XM,F2,IORDR,NX,CHARR
 9440              FORMAT(1X,'I',F6.3,' I',F6.3,'E',I3,'I',
     .                                        I7,'I',A50)
             ENDIF
          ENDIF
  300  CONTINUE

       IF( VMIN .GE. 0.0 ) THEN
           CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHAR1)
           VXMIN  = 10**VLMIN
       ENDIF

       IF( IFG .EQ. 0 ) THEN
           WRITE(LU,9250) CHAR1
           IF( VMIN .GE. 0.0 ) THEN
               WRITE(LU,9200) SCALE
               WRITE(LU,9260)
 9260          FORMAT(30X,'Logarithmic Scale indicated by "O"')
           ELSE
               WRITE(LU,9200) SCALE
           ENDIF
       ELSE
           WRITE(LU,9251) CHAR1
           WRITE(LU,9205) SCALE
           WRITE(LU,9360)
 9360      FORMAT(30X,'"O" : No. of Events in Log. scale.')
       ENDIF

C
  500  CONTINUE

      RETURN
      END
C***********************************************************************
C*                                                                     *
C*============================================================         *
C*  SUBROUTINE XHRNGE( IFLG, VMIN, VMAX, VTMIN, VTMAX, STEP)           *
C*============================================================         *
C*((Function))                                                         *
C*    Determine the vertical range of the histogram.                   *
C*((Input))                                                            *
C*    IFLG   : Flag which indicates whether logarithmic or linear      *
C*             scale.  IFLG = ( 1 / any other ) = ( log / linear )     *
C*    VMIN,VMAX : Minimum and maximum values of vertical window.       *
C*((Output))                                                           *
C*    VTMIN,VTMAX : Minimum and maxmum values of optimized vertical    *
C*                  window.                                            *
C*    STEP   : step of scale for the optimized vertical window         *
C*((Author))                                                           *
C*    S.Kawabata    Oct '85  at KEK                                    *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE XHRNGE( IFLG, VMIN, VMAX, VTMIN, VTMAX, STEP)
C
C     IFLG =    1 : Log scale
C            other: Linear scale
C
      PARAMETER ( NBIN  = 25 )
      REAL    WIND(NBIN),STP1(NBIN),STP2(NBIN)
C
      DATA WIND/
     .   1.00, 1.10, 1.20, 1.30, 1.40, 1.50, 1.60, 1.80, 2.00,  2.20,
     .   2.50, 2.70, 3.00, 3.30, 3.60, 4.00, 4.50, 5.00, 5.50,  6.00,
     .   6.50, 7.00, 8.00, 9.00, 10.0/
*     DATA STP1/
*    .   0.20, 0.22, 0.30, 0.26, 0.28, 0.30, 0.32, 0.36, 0.40,  0.44,
*    .   0.50, 0.54, 0.60, 0.66, 0.60, 0.80, 0.90, 1.00, 1.10,  1.00,
*    .   1.30, 1.00, 1.60, 1.80, 2.00/
      DATA STP1/
     .   0.250,0.275,0.300,0.325,0.350,0.375,0.400,0.450,0.500,0.550,
     .   0.625,0.675,0.750,0.825,0.900,1.000,1.125,1.250,1.375,1.500,
     .   1.625,1.750,2.000,2.250,2.500/
      DATA STP2/
     .   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  1.00,
     .   1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,  2.00,
     .   2.00, 2.00, 2.00, 2.00, 2.00/
C
          XMAX   = VMAX
          XMIN   = VMIN
          IFLAG  = IFLG
          IF( IFLG .NE. 1 .AND. VMIN .LT. 0.0 ) THEN
              IF( VMAX .LE. 0.0 )THEN
                  IFLAG  = 2
                  XMAX  = - VMIN
                  XMIN  =  0.0
              ELSE
                  AVMIN  = - VMIN
                  XMIN  =0.0
                  IF( VMAX .GE. AVMIN ) THEN
                      IFLAG  = 3
                      XMAX  = VMAX
                      XMIN1 = AVMIN
                  ELSE
                      IFLAG  = 4
                      XMAX  = AVMIN
                      XMIN1 = VMAX
                  ENDIF
              ENDIF
          ENDIF
          DSCALE = XMAX - XMIN
          CALL XHORDR( DSCALE, DSF2, DSORDR, IORD)

          DO 100 I = 2, 25
             IF( DSF2 .GE. WIND(I-1) .AND.
     .           DSF2 .LE. WIND( I )       ) GO TO 200
 100      CONTINUE
          I = 25
C
 200      CONTINUE

          XMAX = WIND(I)*DSORDR + XMIN
          IF(     DSORDR .GE. 10.0 .OR. IFLG .NE. 1 ) THEN
                  STEP1  = STP1(I)
                  STEP   = STEP1*DSORDR
          ELSE
                  STEP1  = STP2(I)
                  STEP   = STEP1
          ENDIF

          IF(     IFLAG .LE. 1 ) THEN
                  VTMAX  = XMAX
                  VTMIN  = XMIN
          ELSEIF( IFLAG .EQ. 2 ) THEN
                  VTMAX  = XMIN
                  VTMIN  = -XMAX
          ELSE

                  XPLUS   = 0.0
                  DO 300 J = 1, 10
                     XPLUS = XPLUS + STEP
                     IF( XPLUS .GT. XMIN1 ) GO TO 400
 300              CONTINUE
 400              XMIN = XPLUS
                  XMAX = XMAX
                  IF( IFIX((WIND(I)+0.1)/STEP1)+J .GT. 7 ) THEN
                      STEP = 2.0*STEP
                  ENDIF
                  IF( IFLAG .EQ. 3 ) THEN
                      VTMAX  = XMAX
                      VTMIN  = -XMIN
                  ELSE
                      VTMAX  = XMIN
                      VTMIN  = -XMAX
                  ENDIF
          ENDIF
C
      RETURN
      END
************************************************************************
*    =========================================================         *
      SUBROUTINE XHSCLE( IFLG,VMIN,VMAX,VSTP,UNIT,SCALE,CHAR)
*    =========================================================         *
* ((Function))                                                         *
*     Determine the vertical scale and make it's format                *
* ((Input))                                                            *
*     IFLG   : Flag which indicates whether logarithmic or linear      *
*              scale.  IFLG = ( 1 / any other ) = ( log / linear )     *
*     VMIN,VMAX : Minimum and maximum values of vertical window.       *
*     VSTEP  : Step of unit scale                                      *
*     UNIT   : Unit of one mark *(or o)                                *
* ((Output))                                                           *
*     NSCL   : Number of scale mark                                    *
*     NBLK   : Number of blanks between scale marks                    *
*     CHAR   : Format of scale                                         *
* ((Author))                                                           *
*     S.Kawabata    Oct '85  at KEK                                    *
*                                                                      *
************************************************************************

      CHARACTER*50 CHAR
      CHARACTER*52 SCALE
      CHARACTER*1 PLUS,MINUS
      DATA PLUS /'+'/, MINUS /'-'/

C     IFLG =    1 : Log scale
C            other: Linear scale
      WRITE(SCALE,9000)
 9000 FORMAT(5('          '))
      IF( IFLG .EQ. 1 ) THEN
          SC  = 10.**VMIN
      ELSE
          SC  = VMIN
      ENDIF

      WRITE(SCALE(1:8),9100) SC
 9100 FORMAT(1P,E8.1)
      I2    = 8
      STV   = VSTP + VMIN
      STV1  = STV
      VAL1  = VMIN
      CHAR(50:50) = PLUS
      DO  100   I = 1, 49
          VAL2    = VAL1 + UNIT
          IF( STV .GE. VAL1 .AND. STV .LT. VAL2 ) THEN
              CHAR(I:I)  = PLUS
              NSCL       = NSCL + 1
              IF( IFLG .EQ. 1 ) THEN
                 SC          = 10.0**STV
              ELSE
                 IF(     STV1 .EQ. 0.0 ) THEN
                         SC           = STV
                 ELSEIF( ABS(STV/STV1) .LT. 1.E-2 ) THEN
                         SC           = 0.0
                 ELSE
                         SC          = STV
                 ENDIF
                 STV1       = STV
              ENDIF
              STV  = STV + VSTP
              IF( I2 .LT. I-1 ) THEN
                  I2   = I + 8
                  IF( I2 .LE. 52 ) THEN
                      WRITE(SCALE(I+1:I2),9100) SC
                  ENDIF
              ENDIF
          ELSE
              CHAR(I:I) = MINUS
          ENDIF
          VAL1      = VAL2
  100 CONTINUE
C
      IF( NSCL .EQ. 0 ) THEN
          IF( IFLG .EQ. 1 ) THEN
             SC       = 10.0**VMAX
          ELSE
             SC       = VMAX
          ENDIF
          WRITE(SCALE(44:52),9100) SC
      ENDIF
C
      RETURN
      END
CDECK  ID>, UHPHS3.
CC**********************************************************************
C*
C*=======================================================------===
C* Subroutine UHPHS3(QPR,AM1,AM2,AM3,CS1,FI1,FI2,Q12,Q13,WAT,PV)
C*=======================================================------===
C*
C* (Purpose)
C*    Set 4-momenta for QPR ---> AM1 + AM2 + AM3.
C* (Inputs)
C*       QPR(*)  : (R*4) : parent 4-momentum.
C*       AM1     : (R*4) : 1st daughter mass.
C*       AM2     : (R*4) : 2nd daughter mass.
C*       AM3     : (R*4) : 3rd daughter mass.
C*       CS1     : (R*4) : cos(theta_1).
C*       FI1     : (R*4) : phi_1.
C*       FI2     : (R*4) : phi_2 measured from PR-3 plane.
C*       Q12     : (R*4) : invariant mass for 1-2 pair.
C*       Q13     : (R*4) : invariant mass for 1-3 pair.
C* (Output)
C*       WAT     : (R*4) : phase space weight.
C*       PV(*,1) : (R*4) : 1st daughter 4-momentum.
C*         (*,2) : (R*4) : 2nd daughter 4-momentum.
C*         (*,3) : (R*4) : 3rd daughter 4-momentum.
C* (Relation)
C*    Invokes UVZERO, UHSETF, UBTRAN, and routines in helas_v1.
C*
CC**********************************************************************

      SUBROUTINE UHPHS3(QPR,AM1,AM2,AM3,CS1,FI1,FI2,Q12,Q13,WAT,PV)

      IMPLICIT REAL*8  ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4    QPR(0:3), AM1, AM2, AM3, CS1, FI1, FI2, Q12, Q13,
     .          WAT, PV(0:3,3)
      REAL*4    QV(0:3,3), EC(3,3), EB(3,3), EA(3,3)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Constants.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL   = 1
         CALL UVZERO(9,EC)
         EC(1,1) = 1
         EC(2,2) = 1
         EC(3,3) = 1
         FACT    = 1/(32*x2PI**5)
      ENDIF
C--
C  Zap 4-vectors.
C--
      CALL UVZERO(12,PV)
      CALL UVZERO(12,QV)
C--
C  Check if the parent mass is large enough.
C--
      QQ123 = AM1*AM1 + AM2*AM2 + AM3*AM3
      Q122  = Q12*Q12
      Q132  = Q13*Q13
C--
      AMPR2 = QPR(0)*QPR(0)
      AMPR2 = AMPR2 - QPR(1)*QPR(1) - QPR(2)*QPR(2) - QPR(3)*QPR(3)
      IF ( AMPR2+QQ123.LE.Q122+Q132 ) 			GO TO 9999
C--
C  Set 4-momenta in the parent rest frame.
C     PV(*,1) = 4-momentum of 1st daughter
C       (*,2) = 4-momentum of 2nd daughter
C       (*,3) = 4-momentum of 3rd daughter
C--
      AMPR = SQRT(AMPR2)
      E2 = ( AMPR2 + AM2*AM2 - Q13*Q13 )/2/AMPR
      E3 = ( AMPR2 + AM3*AM3 - Q12*Q12 )/2/AMPR
      E1 = AMPR - E2 - E3
      P2 = ( E2 - AM2 )*( E2 + AM2 )
      IF ( P2.LT.0.D0 ) 				GO TO 9999
      P2 = SQRT(P2)
      P3 = ( E3 - AM3 )*( E3 + AM3 )
      IF ( P3.LT.0.D0 ) 				GO TO 9999
      P3 = SQRT(P3)
      P1 = ( E1 - AM1 )*( E1 + AM1 )
      IF ( P1.LT.0.D0 ) 				GO TO 9999
      P1 = SQRT(P1)
C--
      IF ( P1.GT.0.D0 ) THEN
         IF ( P2.GT.0.D0 ) THEN
            CSTH2 = -(P1*P1+P2*P2-P3*P3)/(2*P1*P2)
            IF ( ABS(CSTH2).GT.1.D0 ) CSTH2 = SIGN(1.D0,CSTH2)
         ELSE
            CSTH2 = 0
         ENDIF
         IF ( P3.GT.0.D0 ) THEN
            CSTH3 = -(P1*P1+P3*P3-P2*P2)/(2*P1*P3)
            IF ( ABS(CSTH3).GT.1.D0 ) CSTH3 = SIGN(1.D0,CSTH3)
         ELSE
            CSTH3 = 0
         ENDIF
      ELSE
         CSTH2 =  1
         CSTH3 = -1
      ENDIF
      SNTH2 = SQRT((1-CSTH2)*(1+CSTH2))
      SNTH3 = SQRT((1-CSTH3)*(1+CSTH3))
C--
      PV(0,2) = E2
      PV(1,2) = P2*SNTH2*COS(FI2)
      PV(2,2) = P2*SNTH2*SIN(FI2)
      PV(3,2) = P2*CSTH2
C--
      PV(0,3) = E3
      PV(1,3) = -PV(1,2)
      PV(2,3) = -PV(2,2)
      PV(3,3) = -(PV(3,2) + P1)
C--
C  Set reference frame.
C--
      IF ( ABS(CS1).LT.1.D0 ) THEN
         SN1 = SQRT((1-CS1)*(1+CS1))
      ELSE
         SN1 = 0
      ENDIF
      PV(1,1) = SN1*COS(FI1)
      PV(2,1) = SN1*SIN(FI1)
      PV(3,1) = CS1
C--
      CALL UHSETF(QPR(1),EC,EB)
      CALL UHSETF(PV(1,1),EB,EA)
C--
C  Transform to the parent-moving frame.
C--
      CALL UBTRAN(PV(1,2),EA,PV(1,2))
      CALL UBTRAN(PV(1,3),EA,PV(1,3))
C--
      CALL BOOSTx(PV(0,2),QPR(0),PV(0,2))
      CALL BOOSTx(PV(0,3),QPR(0),PV(0,3))
      CALL PSUBxx(QPR(0),PV(0,2),PV(0,1))
      CALL PSUBxx(PV(0,1),PV(0,3),PV(0,1))
C--
C  Calculate phase space weight.
C--
      WAT = FACT/AMPR2
      RETURN
C--
9999  WAT = 0
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, XNMASS.
      SUBROUTINE XNMASS(AM2,AMU,BT,SN2W,AMZ,AMXN,ON,ETA)

      IMPLICIT   REAL*8 ( A-H, O-Z )
      REAL   *8  AM2, AMU, BT, SN2W, AMZ, AMXN(4), ON(4,4)
      COMPLEX*16 ETA(4)
      REAL   *8  AMSMAT(10), AMX(4), EV(4,4), WORK(8)
      INTEGER*4  IP(4)
      DATA NCALL / 0 /
C
C========< Entry Point >================================================
C
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         PI    = ACOS(-1.)
         SQ2   = SQRT(2.)
      ENDIF
C--
C  Calculate mass matrix.
C--
      CS2W = 1 - SN2W
      TN2W = SN2W/CS2W
      SNW  = SQRT(SN2W)
      CSW  = SQRT(CS2W)
      AM1  = (5./3)*AM2*TN2W
      CSB  = COS(BT)
      SNB  = SIN(BT)
C--
      AMSMAT( 1) =  AM1
      AMSMAT( 2) =  0
      AMSMAT( 3) =  AM2
      AMSMAT( 4) = -AMZ*SNW*CSB
      AMSMAT( 5) =  AMZ*CSW*CSB
      AMSMAT( 6) =  0
      AMSMAT( 7) =  AMZ*SNW*SNB
      AMSMAT( 8) = -AMZ*CSW*SNB
      AMSMAT( 9) = -AMU
      AMSMAT(10) =  0
C--
C  Diagonalize mass matrix.
C--
      CALL DSEIG1(AMSMAT,4,AMX,EV,4,ME,WORK,IRT)
      IF ( IRT.NE.0 ) THEN
         PRINT *, ' >>>>> Error in XNMASS: IRT = ', IRT
      ENDIF
C--
C  Sort out mass eigen values.
C--
C>>>
C     IF ( NCALL.EQ.1 ) THEN
C        NCALL = 2
         CALL USORTD(4,1,1,AMX,IP)
C     ENDIF
C>>>
      DO 100 I = 1, 4
         AMXN(I) = ABS(AMX(IP(I)))
         IF ( AMX(IP(I)).GE.0.D0 ) THEN
            ETA(I) = 1
         ELSE
            ETA(I) = (0.D0,1.D0)
         ENDIF
         DO 10 J = 1, 4
            ON(I,J) = EV(J,IP(I))
10       CONTINUE
100   CONTINUE
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DFGTMN.
      SUBROUTINE DFGTMN(IFLG)
C===
C Main program for Chargino pair.
C C.Dionisi,K.Fujii.S.Giagu.T.Tsukamoto
C===
      IMPLICIT REAL*8 (A-H,O-Z)

      CHARACTER*128   BSINF, BSOUTF
      INTEGER*4 NGETBS, NUBS, NPUTBS, NDOSPR, NPRINF, NPRHST, LUINFO
      INTEGER*4 NBSINF, NOSPEV
      REAL*8 ESTIM, SIGMA, CTIME
      INTEGER*4 IT1, IT2, MXTRY

      COMMON /BS51PR/ ESTIM, SIGMA, CTIME, IT1, IT2, MXTRY,
     >        NGETBS, NUBS, NPUTBS, NDOSPR, NPRINF, NPRHST,
     >        LUINFO, NBSINF, NOSPEV,
     >        BSINF, BSOUTF

      COMMON /BS51HT/ LUHIST, BSHSTF
      INTEGER*4       LUHIST
      CHARACTER*128   BSHSTF
C* (Contents)
C*    SQRTS   : (R*4) : sqrt(s).
C*    POLEBM  : (R*4) : electron beam polarization.
C*    SGMEBM  : (R*4) : beam energy spread (fraction).
C*    ISRB    : (I*4) : ISR and BM flag.
C*

      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO
      INTEGER*4       ISRB
      REAL*4 SIGMARES,DSIGMARES
      COMMON /MYSIGM/ SIGMARES,DSIGMARES

      EXTERNAL FUNCXX
      IF ( IFLG .EQ. -2 ) THEN

	CALL DFGTBDI
3232    CONTINUE

      ELSE IF ( IFLG .EQ. -1 ) THEN

C--
C Initialization : set default parameters for BASES/SPRING
C--

        CALL DFGT_PAR(2)
        CALL BSINIT
        CALL DFGT_IN
        CALL SPINIT

C--
C  BASES Integration
C--
        CALL BASES( FUNCXX, ESTIM, SIGMA, CTIME, IT1, IT2 )
        XMYESTIM = ESTIM*(5.E-3/GAMWINO)**2.
        XMYSIGMA = SIGMA*(5.E-3/GAMWINO)**2.
        SIGMARES = XMYESTIM/1000.
        DSIGMARES = XMYSIGMA/1000.
	WRITE(*,*)' '
        WRITE(*,*)'**********************************************'
        WRITE(*,*)'    XSECTION (pb) =',XMYESTIM,' +- ', XMYSIGMA
        WRITE(*,*)'**********************************************'
	WRITE(*,*)' '
CSG        CALL BSINFO(LUINFO)
        CALL BHPLOT(LUINFO)

C========
C Event generation
C========

      ELSE IF (IFLG .EQ. 0 ) THEN
            MXTRY = 50
        CALL SPRING( FUNCXX, MXTRY )
        CALL SPEVNT( IRET )
        CALL FILLHI        
C--
C     Output statiscal information, hists. and plots of generation
C--

      ELSE IF ( IFLG .GT. 0 ) THEN
        CALL SPINFO(LUINFO)
        CALL SHPLOT(LUINFO)

      ELSE
        WRITE(*,*)'ERROR::DFGTMN Called w. Wrong IFLG =',IFLG
        CALL ABEND
      ENDIF

      RETURN
      END
CDECK  ID>, GMH2FF.
C* (Inputs)
C*    AMH    : (R*4) : Higss mass.
C*    AMF    : (R*4) : fermion mass.
C*    AMZ    : (R*4) : Z mass.
C* (Output)
C*    GM     : (R*4) : (1/1)*( Higss partical width ) for F = L.
C*                   : (1/3)*( Higss partical width ) for F = Q.
C*

      SUBROUTINE GMH2FF(AMH,AMF,AMZ,GM)

      IMPLICIT REAL*4 ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4 AMH, AMF, AMZ, GM
C
C========< Entry Point >================================================
C
C--
C  Initialize constants.
C--
      GF     = xPI*xALF/xSIN2W/xCOS2W/xSQRT2/AMZ**2
      FACT   = GF/4/xSQRT2/xPI
C--
C  Calculate width.
C--
      EF = AMH/2
      BT = (EF-AMF)*(EF+AMF)
      IF ( BT.LE.0. ) THEN
         GM = 0
         RETURN
      ELSE
         BT   = SQRT(BT)/EF
      ENDIF
      GM = FACT*AMF*AMF*AMH*BT**3
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, IDK2PD.
      SUBROUTINE IDK2PD(IDKF,IDPD)

      INTEGER*4  IDKF(3), IDPD
      INTEGER*4  IDPDG(3,2,2)
      DATA IDPDG / 12, 14, 16,    11, 13, 15,
     .              2,  4,  6,     1,  3,  5 /

      IG = IDKF(1)
      IT = IDKF(2)
      LQ = IDKF(3)
      IDPD = IDPDG(IG,IT,LQ)
      RETURN
      END
CDECK  ID>, PROBEB.
      SUBROUTINE PROBEB(X1,X2,E0,EBM,EBM0,PROB)
      IMPLICIT REAL*8 (Z)
      COMMON /BEMCNS/ SGEBM
      REAL*4          SGEBM
      REAL*4     X1, X2, E0, EBM, EBM0, PROB
C S300
C     DATA ITYP  / 31 /
C X300
      DATA ITYP  / 33 /
C>>>
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Initialization.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         FACT  = 1/SQRT(2*ACOS(-1.))
      ENDIF
C--
C  Decide flatly smeared beam energy.
C--
      EBM0 = E0*( 1 + SGEBM*2*(X1-0.5) )
      PROB = 1
C--
C  Convolute beamstrahlung.
C--
      Z    = X2
      CALL EPROBX(Z,ITYP,ZB)
      EBM  = EBM0*ZB
C--
C  Calculate corresponding probability density.
C--
C     PROB = PROB*EPROB(ZB,ZE)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, UHPHS3W.
CC**********************************************************************
C*
C*=======================================================------===
C* Subroutin UHPHS3W(QPR,AM1,AM2,AM3,CS1,FI1,FI2,Q12,Q13,WAT,PV)
C*=======================================================------===
C*
C* (Purpose)
C*    Set 4-momenta for QPR ---> AM1 + AM2 + AM3.
C* (Inputs)
C*       QPR(*)  : (R*4) : parent 4-momentum.
C*       AM1     : (R*4) : 1st daughter mass.
C*       AM2     : (R*4) : 2nd daughter mass.
C*       AM3     : (R*4) : 3rd daughter mass.
C*       CS1     : (R*4) : cos(theta_1).
C*       FI1     : (R*4) : phi_1.
C*       FI2     : (R*4) : phi_2 measured from PR-3 plane.
C*       Q12     : (R*4) : invariant mass for 1-2 pair.
C*       Q13     : (R*4) : invariant mass for 1-3 pair.
C* (Output)
C*       WAT     : (R*4) : phase space weight.
C*       PV(*,1) : (R*4) : 1st daughter 4-momentum.
C*         (*,2) : (R*4) : 2nd daughter 4-momentum.
C*         (*,3) : (R*4) : 3rd daughter 4-momentum.
C* (Relation)
C*    Invokes UVZERO, UHSETF, UBTRAN, and routines in helas_v1.
C*
CC**********************************************************************

      SUBROUTINE UHPHS3W(QPR,AM1,AM2,AM3,CS1,FI1,FI2,Q12,Q13,WAT,PV)

      IMPLICIT REAL*8  ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--

      REAL*4    QPR(0:3), AM1, AM2, AM3, CS1, FI1, FI2, Q12, Q13,
     .          WAT, PV(0:3,3)
      REAL*4    QV(0:3,3), EC(3,3), EB(3,3), EA(3,3)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Constants.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL   = 1
         CALL UVZERO(9,EC)
         EC(1,1) = 1
         EC(2,2) = 1
         EC(3,3) = 1
         FACT    = 1/(32*x2PI**5)
      ENDIF
C--
C  Zap 4-vectors.
C--
      CALL UVZERO(12,PV)
      CALL UVZERO(12,QV)
C--
C  Check if the parent mass is large enough.
C--
      QQ123 = AM1*AM1 + AM2*AM2 + AM3*AM3
      Q122  = Q12*Q12
      Q132  = Q13*Q13
C--
      AMPR2 = QPR(0)*QPR(0)
      AMPR2 = AMPR2 - QPR(1)*QPR(1) - QPR(2)*QPR(2) - QPR(3)*QPR(3)
      IF ( AMPR2+QQ123.LE.Q122+Q132 ) 			GO TO 9999
C--
C  Set 4-momenta in the parent rest frame.
C     PV(*,1) = 4-momentum of 1st daughter
C       (*,2) = 4-momentum of 2nd daughter
C       (*,3) = 4-momentum of 3rd daughter
C--
      AMPR = SQRT(AMPR2)
      E2 = ( AMPR2 + AM2*AM2 - Q13*Q13 )/2/AMPR
      E3 = ( AMPR2 + AM3*AM3 - Q12*Q12 )/2/AMPR
      E1 = AMPR - E2 - E3
      P2 = ( E2 - AM2 )*( E2 + AM2 )
      IF ( P2.LT.0.D0 ) 				GO TO 9999
      P2 = SQRT(P2)
      P3 = ( E3 - AM3 )*( E3 + AM3 )
      IF ( P3.LT.0.D0 ) 				GO TO 9999
      P3 = SQRT(P3)
      P1 = ( E1 - AM1 )*( E1 + AM1 )
      IF ( P1.LT.0.D0 ) 				GO TO 9999
      P1 = SQRT(P1)
C--
      IF ( P1.GT.0.D0 ) THEN
         IF ( P2.GT.0.D0 ) THEN
            CSTH2 = -(P1*P1+P2*P2-P3*P3)/(2*P1*P2)
            IF ( ABS(CSTH2).GT.1.D0 ) CSTH2 = SIGN(1.D0,CSTH2)
         ELSE
            CSTH2 = 0
         ENDIF
         IF ( P3.GT.0.D0 ) THEN
            CSTH3 = -(P1*P1+P3*P3-P2*P2)/(2*P1*P3)
            IF ( ABS(CSTH3).GT.1.D0 ) CSTH3 = SIGN(1.D0,CSTH3)
         ELSE
            CSTH3 = 0
         ENDIF
      ELSE
         CSTH2 =  1
         CSTH3 = -1
      ENDIF
      SNTH2 = SQRT((1-CSTH2)*(1+CSTH2))
      SNTH3 = SQRT((1-CSTH3)*(1+CSTH3))
C--
      PV(0,2) = E2
      PV(1,2) = P2*SNTH2*COS(FI2)
      PV(2,2) = P2*SNTH2*SIN(FI2)
      PV(3,2) = P2*CSTH2
C--
      PV(0,3) = E3
      PV(1,3) = -PV(1,2)
      PV(2,3) = -PV(2,2)
      PV(3,3) = -(PV(3,2) + P1)
C--
C  Set reference frame.
C--
      IF ( ABS(CS1).LT.1.D0 ) THEN
         SN1 = SQRT((1-CS1)*(1+CS1))
      ELSE
         SN1 = 0
      ENDIF
      PV(1,1) = SN1*COS(FI1)
      PV(2,1) = SN1*SIN(FI1)
      PV(3,1) = CS1
C--
      P_PR = SQRT( QPR(1)*QPR(1) +  QPR(2)*QPR(2) + QPR(3)*QPR(3) )
      IF ( P_PR/AMPR.LE.1.D-4 ) THEN
         CALL UVCOPY(9,EC,EB)
      ELSE
         CALL UHSETF(QPR(1),EC,EB)
      ENDIF
      CALL UHSETF(PV(1,1),EB,EA)
C--
C  Transform to the parent-moving frame.
C--
      CALL UBTRAN(PV(1,2),EA,PV(1,2))
      CALL UBTRAN(PV(1,3),EA,PV(1,3))
C--
      IF ( P_PR/AMPR.GT.1.D-4 ) THEN
         CALL BOOSTx(PV(0,2),QPR(0),PV(0,2))
         CALL BOOSTx(PV(0,3),QPR(0),PV(0,3))
      ENDIF
      CALL PSUBxx(QPR(0),PV(0,2),PV(0,1))
      CALL PSUBxx(PV(0,1),PV(0,3),PV(0,1))
C--
C  Calculate phase space weight.
C--
      WAT = FACT/AMPR2
      RETURN
C--
9999  WAT = 0
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DFGTUTIL.
C
C ======================================================================
C
      SUBROUTINE BOOST0 (P,Q , PBOOST)
C
C This subroutine computes the Lorentz boost of the real vector.
C The vector P is assumed to be given in the rest frame of Q, which must
C be a timelike vector.  PBOOST is the vector P boosted to the frame in
C which Q is given.
C
C INPUT:
C       real    P(0:3)         : real vector which will be boosted
C       real    Q(0:3)         : vector of new origin in the old
C                                coordinates
C
C OUTPUT:
C       real    PBOOST(0:3)    : real vector boosted
C
      IMPLICIT REAL (A-H,L,M,O-Z)
      REAL    P(0:3),Q(0:3),PBOOST(0:3)
C
      PQ=P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3)
      QQ=Q(1)**2+Q(2)**2+Q(3)**2
C
      M=SQRT(Q(0)**2-QQ)
      LF=((Q(0)-M)*PQ/QQ+P(0))/M
      PBOOST(0)=(P(0)*Q(0)+PQ)/M
      PBOOST(1)=P(1)+Q(1)*LF
      PBOOST(2)=P(2)+Q(2)*LF
      PBOOST(3)=P(3)+Q(3)*LF
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE PMIRR0 (P , PMIRR)
C
C This subroutine computes the real vector whose signs of the spacial
C components are reversed from the ones of the original vector.
C
C INPUT:
C       real    P(0:3)         : real vector
C
C OUTPUT:
C       real    PMIRR(0:3)     : mirror vector
C
      REAL    P(0:3),PMIRR(0:3)
C
      PMIRR(0)= P(0)
      PMIRR(1)=-P(1)
      PMIRR(2)=-P(2)
      PMIRR(3)=-P(3)
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE PSUM00 (P1,P2 , PSUM)
C
C This subroutine computes the sum of two real vectors.
C
C INPUT:
C       real    P1(0:3)        : real vector
C       real    P2(0:3)        : real vector
C
C OUTPUT:
C       real    PSUM(0:3)      : summation P1+P2
C
      REAL    P1(0:3),P2(0:3),PSUM(0:3)
C
      PSUM(0)=P1(0)+P2(0)
      PSUM(1)=P1(1)+P2(1)
      PSUM(2)=P1(2)+P2(2)
      PSUM(3)=P1(3)+P2(3)
      RETURN
      END
C
C ----------------------------------------------------------------------
C
      SUBROUTINE RDOT00 (P1,P2 , SCALAR)
C
C This subroutine computes the scalar product of two REAL vectors.
C To get an inner-product of two momenta, call this subroutine.
C For the complex vectors, use CDOT00  subroutine.
C
C INPUT:
C       real    P1(0:3)        : vector
C       real    P2(0:3)        : vector
C
C OUTPUT:
C       real    SCALAR         : scalar product of two vectors
C
      REAL P1(0:3),P2(0:3)
C
      SCALAR =  P1(0)*P2(0)-P1(1)*P2(1)
     &         -P1(2)*P2(2)-P1(3)*P2(3)
      RETURN
      END
      FUNCTION UABSV3(X)
C =============------===
C
      DIMENSION X(3)
C
      UABSV3 = UDOT3(X,X)
      UABSV3 = SQRT(UABSV3)
C
      RETURN
      END
      SUBROUTINE UBTRAN(PB,E,PA)
C* ==========================--==
C
      DIMENSION PB(3),PA(3),P(3),E(3,3)
C
      DO 10 I=1,3
        P(I) = 0.0
      DO 10 II=1,3
        P(I) = P(I)+PB(II)*E(I,II)
 10   CONTINUE
C
      CALL UCOPV3(P,PA)
C
      RETURN
      END
      SUBROUTINE UCOPV3(X,Y)
C =======================---==
C
      DIMENSION X(3),Y(3)
C
      DO 10 I=1,3
        Y(I) = X(I)
 10   CONTINUE
C
      RETURN
      END
      SUBROUTINE UCROSS(X,Y,Z)
C =========================---===
C
      DIMENSION X(3),Y(3),Z(3),A(3)
C
      A(1) = X(2)*Y(3)-X(3)*Y(2)
      A(2) = X(3)*Y(1)-X(1)*Y(3)
      A(3) = X(1)*Y(2)-X(2)*Y(1)
C
      CALL UCOPV3(A,Z)
C
      RETURN
      END
      FUNCTION UDOT3(X,Y)
C* ============-----=======
C
      DIMENSION X(3),Y(3)
C
      UDOT3 = 0
C
      DO 10 I=1,3
        UDOT3 = UDOT3+X(I)*Y(I)
 10   CONTINUE
C
      RETURN
      END
C***********************************************************************
C*
C*  -------------------------------------------------====
C*  Subroutine UDSRCH( NREC, LREC, KEY, ARAY, XDATA, NPNT )
C*  -------------------------------------------------====
C*(Function)
C*   Make binary search.    Double presision version.
C*   Find NPNT which satisfies
C*        ARAY(KEY,NPNT) < or = XDATA < ARAY(KEY,NPNT+1)
C*   ARAY should be sorted from smaller data to larger data.
C*
C*(Input)
C*   NREC  ; # of records
C*   LREC  ; Record size  (ARAY(LREC,NREC))
C*   KEY   ; KEy element pointer in a record.
C*   ARAY  ; Target data aray
C*   XDATA ; Reference data.
C*(Output)
C*   NPNT  ; Pointer which satisfies the conditions.
C*           NPNT = 0 if XDATA < ARAY(KEY,1)
C*           NPNT = NREC if XDATA =ARAY(KEY,NREC) or
C*                                > ARAY(KEY,NREC)
C*(Author)
C*   A. Miyamoto   16-Oct-1990  Original version.
C*
C***********************************************************************
C
      SUBROUTINE UDSRCH(NREC,LREC,KEY,ARAY,XDATA,NPNT)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8   ARAY(LREC,NREC)
C
C ====< Entry Point > ==================================================
C
C ------------------------------------
C (1) Omit Extream case first.
C ------------------------------------
C
      IF( XDATA .LT. ARAY(KEY,1) ) THEN
        NPNT = 0
        RETURN
      ELSEIF( XDATA .GE. ARAY(KEY,NREC) ) THEN
        NPNT = NREC
        RETURN
      ENDIF
C
C
C ------------------------------------
C (2) Binary search
C ------------------------------------
C
C
      I1 = 1
      I2 = NREC
200   CONTINUE
      IMDL = (I2+I1)/2
      IF( XDATA.GE.ARAY(KEY,IMDL) ) THEN
          I1 = IMDL
      ELSE
          I2 = IMDL
      ENDIF
C
      IF( I2-I1 .GT. 1 ) GO TO 200
      NPNT = I1
      RETURN
      END
      SUBROUTINE UNCROS(A,B,C)
C =========================---=
C
      DIMENSION A(3),B(3),C(3)
C-----
C  Calculate vector product.
C-----
      AMAG = UABSV3(A)
      BMAG = UABSV3(B)
      CALL UCROSS(A,B,C)
      CMAG = UABSV3(C)
      IF (CMAG.LT.(1.0E-6*AMAG*BMAG)) GOTO 5
      CALL USCLM3(1./CMAG,C,C)
      GO TO 10
C-----
C  Error return.
C-----
 5    CONTINUE
      WRITE(6,7)A,B
 7    FORMAT(' ***UNCROS Error...input vectors parallel...input',
     +   ' dumped ***',/' ***',6E12.4,' ***')
 10   RETURN
      END
      SUBROUTINE UNRMV3(X,UX)
C* ======================---===
C
      DIMENSION X(3),UX(3)
C
      AX   = UABSV3(X)
      AX   = 1./AX
      CALL USCLM3(AX,X,UX)
C
      RETURN
      END
      SUBROUTINE USCLM3(A,X,AX)
C* =========================--==
C
      DIMENSION X(3),AX(3)
C
      DO 10 I=1,3
        AX(I) = A*X(I)
 10   CONTINUE
C
      RETURN
      END
      FUNCTION VDOTN(A,B,N)
CSG      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(0:3),B(0:3)
      HA=0.0
      HB=0.0
      VDOTN=0.0
      DO 1 J=1,N
      VDOTN=VDOTN+A(J)*B(J)
      HA=HA+A(J)**2.
   1  HB=HB+B(J)**2.
      IF(HA*HB.EQ.0.0) THEN
        VDOTN=1.0
      ELSE
        VDOTN=VDOTN/SQRT(HA*HB)
      END IF
      END
      SUBROUTINE DATE(HIZUKE)
      CHARACTER*8 HIZUKE
      INTEGER*4   N, TIME, TA(9)

      N = TIME()
      CALL LTIME(N, TA)
      WRITE(HIZUKE,110) TA(6), TA(5)+1, TA(4)
 110  FORMAT(I2.2,'-',I2.2,'-',I2.2)
      RETURN
      END
      SUBROUTINE DFGTEND
**
*  Subr. DFGTEND
*
*   Close standard I/O file
*
*   S.G. (9-03-1995)
**
      CLOSE(1)
      CLOSE(5)
      CLOSE(6)
      CLOSE(23)
*
      RETURN
      END
      SUBROUTINE DFGTIN
**
*  Subr. DFGTIN
*
*   Open standard I/O file
*
*   S.G. (9-03-1995)
**
* *** Open Histograms output file.
*
      OPEN(UNIT=1,FORM='UNFORMATTED',
     +     RECL=4096,ACCESS='DIRECT',STATUS='UNKNOWN')
*
* *** Open datacards.
*
      OPEN(UNIT=5,STATUS='UNKNOWN')
*
* *** Open output file.
*
      OPEN(UNIT=6,STATUS='UNKNOWN')
*
* *** Open Probab. output file
*
      OPEN(UNIT=23,FORM='UNFORMATTED',STATUS='UNKNOWN')
*
      RETURN
      END
      SUBROUTINE DJACOBI(A,N,NP,D,V,NROT)
C#######################################################################
C
C   Subr. DJACOBI
C
C   DJACOBI compute all eigenvalues and eigenvectors of a symmetric
C   matrix.
C
C   INPUT:  A  double precision --> Symmetric matrix of dimension NxN.
C           N  integer          --> Dimension of A.
C           NP integer          --> NP == N (!?!).
C   OUTPUT: D  double precision --> Vector of dimension N, which contain
C                                   the eigenvalues of A.
C           V  double precision --> Matrix (NxN), the columns of this
C                                   matrix are the eigenvectors of A
C                                   associated to the eigenvalues in D.
C           NROT integer        --> Number of jacobi's rotations used for
C                                   diagonalization of matrix.
C
C  N.B. After calling DJACOBI the elements over the diagonal of A are
C       destroyed !!
C
C                             Algorytm from Numerical Recipies (1992)
C                             Mod. by S.Giagu (24/1/1995)
C
C#######################################################################
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (NMAX=100)
      DIMENSION A(NP,NP),D(NP),V(NP,NP),B(NMAX),Z(NMAX)
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          TRESH=0.2*SM/N**2.
        ELSE
          TRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=100.*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+G.EQ.ABS(D(IP)))
     *         .AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=0.5*H/A(IP,IQ)
                T=1./(ABS(THETA)+SQRT(1.+THETA**2.))
                IF(THETA.LT.0.)T=-T
              ENDIF
              C=1./SQRT(1+T**2.)
              S=T*C
              TAU=S/(1.+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=0.
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE
      PAUSE '50 iterations should never happen'
      RETURN
      END
C*********************************************************************
C*
C*    ------------------======
C*    Subroutine UIDATE(IDATE)
C*    ------------------======
C*
C*(Function)
C*    Converts character format Date (Date obtained by Facom standard
C*   date routine with format yy-mm-dd ) into TOPAZ standard date
C*   format, yymmdd in INTEGER*4 format.
C*
C*(Output)
C*    IDATE   :  Integer*4 variable of Date with format yymmdd.
C*
C*(Author)
C*    A. Miyamoto        19-Dec-1985  Original version.
C*
C*********************************************************************
C*
      SUBROUTINE UIDATE(IDATE)
C
      CHARACTER*8 HIZUKE
C
      CALL DATE(HIZUKE)
      READ(HIZUKE(1:2),'(I2)') IY
      READ(HIZUKE(4:5),'(I2)') IM
      READ(HIZUKE(7:8),'(I2)') ID
      IDATE = ID + 100*IM + 10000*IY
      RETURN
      END
C*********************************************************************
C*
C*    ----------------------------------------======
C*    SUBROUTINE USORTR(NREC, LREC, KEY, ARY , IPNT)
C*    ----------------------------------------======
C*
C*(Function)
C*    Quick sorter of an real*4 array.
C*
C*(Input)
C*    NREC    :  Number of records.
C*    LREC    :  Record size.
C*    KEY     :  Key element pointer in a record.
C*    ARY     :  Real    array to be sortted.
C*
C*(Output)
C*    IPNT    :  Pointer containing the results.
C*
C*(Author)
C*
C*    Hirofumi Fujii    Jan 12, 1983
C*    A. Miyamoto         6-Sep-1985, modified the arguments.
C*
C*********************************************************************
C*
      SUBROUTINE USORTR(NREC, LREC, KEY, ARY, IPNT)
C
      INTEGER*4   IPNT(NREC)
      REAL*4      ARY(LREC, NREC)
      PARAMETER  (ISTKSZ = 32 ,
     >            ILEFT  = 1,
     >            IRIGHT = 2 )
      INTEGER     ISTACK(2, ISTKSZ)
C
C*
C*(1)  Initialize.
C*
      DO 100 I = 1, NREC
         IPNT(I) = I
100   CONTINUE
      ISP   = 1
      ISTACK(ILEFT ,ISP) = 1
      ISTACK(IRIGHT,ISP) = NREC
C*
C*(2)  Get the most left and most right position from the stack.
C*
200   CONTINUE
        NLEFT  = ISTACK(ILEFT, ISP)
        NRIGHT = ISTACK(IRIGHT,ISP)
        ISP    = ISP - 1
C*
C*(3)  Determine the range to be sortted.
C*
300     CONTINUE
          IL   = NLEFT
          IR   = NRIGHT
          IM   = (IL+IR)/2
          VALKEY = ARY(KEY, IPNT(IM))
C*
C*(4)   Swap the data with respect to the VALKEY.
C*
400       CONTINUE
410         IF(ARY(KEY,IPNT(IL)).GE.VALKEY) GO TO 420
              IL = IL + 1
              IF(IL.GT.NREC)
     >        PRINT *,'Warning in USORTR  IL greater than NREC...'

              GO TO 410
420         IF(ARY(KEY,IPNT(IR)).LE.VALKEY) GO TO 430
              IR = IR - 1
              IF(IR.LE.0)
     >        PRINT *,'Warning in USORTR  IR less than 0....'
              GO TO 420
C
430         CONTINUE
C
C    Swap data.
C
            IF(IL.LE.IR) THEN
              ISAVE = IPNT(IL)
              IPNT(IL) = IPNT(IR)
              IPNT(IR) = ISAVE
              IL = IL + 1
              IR = IR - 1
            END IF

            IF(IL.LE.IR) GO TO 400

C*
C*(5)  Find out the next region to be sorted.
C*
            IF(IR-NLEFT.LT.NRIGHT-IL) THEN
              IF(IL.LT.NRIGHT) THEN
                ISP = ISP + 1
                ISTACK(ILEFT ,ISP) = IL
                ISTACK(IRIGHT,ISP) = NRIGHT
              ENDIF
              NRIGHT = IR
            ELSE
              IF(NLEFT.LT.IR) THEN
                ISP = ISP + 1
                ISTACK(ILEFT ,ISP) = NLEFT
                ISTACK(IRIGHT,ISP) = IR
              ENDIF
              NLEFT = IL
            ENDIF

          IF(NLEFT.LT.NRIGHT) GO TO 300

        IF(ISP.GT.0) GO TO 200

      RETURN
      END
C*UVCOPY    ECSECT
C***************************************************************
C*                                                             *
C* SUBROUTINE UVCOPY( NWORD, FROM, TO )                        *
C*                                                             *
C*  (Purpose)                                                  *
C*       Copy Array                                            *
C*  (Input)                                                    *
C*       NWORD : # of words ( size of ARRAY. in I*4 unit.)     *
C*       FROM  : Source array.                                 *
C*       TO    : Destination array.                            *
C*  (Author)                                                   *
C*       A. Miyamoto    18-Jun-1986                            *
C*       A. Miyamoto     8-Feb-1992 Prepared FORTRAN version   *
C*				    for use on HP	       *
C***************************************************************
	SUBROUTINE UVCOPY( NWORD, FROM, TO )
	INTEGER*4  NWORD, FROM(NWORD), TO(NWORD)
 	DO 100 I = 1, NWORD
 	  TO(I) = FROM(I)
 100	CONTINUE
CSG         call vec_$copy(from, to, nword)
	RETURN
	END
C  UVZERO    ECSECT
C***************************************************************
C*                                                             *
C* SUBROUTINE UVZERO( NWORD, ARRAY )                           *
C*                                                             *
C*  (Purpose)                                                  *
C*       Zero clear array.    .                                *
C*  (Input)                                                    *
C*       ARRAY : Target array.                                 *
C*       NWORD : # of words ( size of ARRAY.)                  *
C*  (Author)                                                   *
C*       A. Miyamoto    18-Jun-1986                            *
C*       S. Giagu       30-Aug-1995                            *
C*                                                             *
C***************************************************************
C23456
      SUBROUTINE UVZERO( NWORD, IARRAY)
      INTEGER*4 IARRAY(NWORD)
      CALL VZERO(IARRAY,NWORD)
CSG      call vec_$init(IARRAY, NWORD, 0 )
      RETURN
      END
CDECK  ID>, GMH2VV.
C* (Inputs)
C*    AMH    : (R*4) : Higss mass.
C*    AMV    : (R*4) : vector boson mass.
C*    AMZ    : (R*4) : Z mass.
C* (Output)
C*    GM     : (R*4) : (1/1)*( Higss partical width ) for V = Z.
C*                   : (1/2)*( Higss partical width ) for V = W.
C*

      SUBROUTINE GMH2VV(AMH,AMV,AMZ,GM)

      IMPLICIT REAL*4 ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4 AMH, AMV, AMZ, GM
C
C========< Entry Point >================================================
C
C--
C  Initialize constants.
C--
      GF     = xPI*xALF/xSIN2W/xCOS2W/xSQRT2/AMZ**2
      FACT   = GF/16/xSQRT2/xPI
C--
C  Calculate width.
C--
      EW = AMH/2
      BT = (EW-AMV)*(EW+AMV)
      IF ( BT.LE.0. ) THEN
         GM = 0
         RETURN
      ELSE
         BT   = SQRT(BT)/EW
      ENDIF
      GM = FACT*AMH**3*BT*( 1 - 4*(AMV/AMH)**2 + 12*(AMV/AMH)**4 )
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, INODCY.
C calculate chargino/neutralino decay widthes/BR
C----------------------------------------------------------------------C
      SUBROUTINE INODCY( ZM, WM, SIN2W, ALPHE, ALPHS,
     &                   HM1, G3MAS, TANB, RELAX, 
     &                   EMAS, UMAS, DMAS,
     &                   SFMSL, SFMSR, SNEUM, PSHMAS,
     &                   SWM, SZM, HGCMAS, HG0MAS,
     &                   DCCINO, BRCINO, DCNINO, BRNINO )
C----------------------------------------------------------------------C
C 17/Jul/95 modified to include ZM as an input argument
C
C Purpose: calculate decay widthes & brancing ratios for
C          chargino/neutralino decays
C Inputs:  WM, SIN2W, ALPHE, ALPHS / parms. for SM.
C          HM1, G3MAS, TANB        / parms. for MSSM.
C          EMAS, UMAS, DMAS        / masses for charged lepton,
C                                    u-type & d-type quarks
C          SFMSL, SFMSR, SNEUM     / masses for sleptons
C          PSHMAS                  / pseudo-scaler higgs mass
C          RELAX                   / to relax the GUT relantion
C Output: SWM, SZM                 / chargino,neutralino masses
C         DCCINO, DCNINO
C         BRCINO, BRNINO
C----------------------------------------------------------------------C
      REAL*4  OL(2,2), OR(2,2), ER
      REAL*4  ON(4,4)
      COMPLEX*8 ETA(4), UN(4,4), GL, GR
      COMPLEX*8 G1L,G1R, G2L,G2R
      REAL*4  SWM(2), SZM(4)
      REAL*4  DCCINO(40,4), DCNINO(40,2)
      REAL*4  BRCINO(40,4), BRNINO(40,2)
      REAL*4  RELAX

      COMPLEX*8 GCESNL(2,4), GCNSEL(2,4), GCNSER(2,4),
     &          GCUSDL(2,4), GCUSDR(2,4), GCDSUL(2,4), GCDSUR(2,4)
      COMPLEX*8 GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     &          GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)

      COMPLEX*8 GNCW(2,4,2)
      COMPLEX*8 GCCZ(2,2,2)
      COMPLEX*8 GNNZ(2,4,4)

      REAL*4 HG0MAS(2)
      COMPLEX*8 GCCH(2,2,2,3)
      COMPLEX*8 GCNH(2,2,4), GNNH(2,4,4,3)
C------------------------------------------------
      WRITE(6,*)'INODCY CALLED '
C    No printout diagonalization results
      IFLG = 1
      CALL       INOMIX( ZM, WM, SIN2W, ALPHE, ALPHS,
     &                   HM1, G3MAS, TANB,
     &                   SWM, SZM,
     &                   OL, OR, ER, PHIL, PHIR,
     &                   ON, UN, ETA, RELAX,
     &                   IFLG  )

C     require that chargino(1) is heavier than neutralino(1)
      IF( SWM(1).LT.SZM(1) ) RETURN

      DO 1 I=1,40
       DO 2 J=1,4
        DCNINO(I,J)=0.
        BRNINO(I,J)=0.
2      CONTINUE
       DO 3 J=1,2
        DCCINO(I,J)=0.
        BRCINO(I,J)=0.
3      CONTINUE
1     CONTINUE

C   calculate higgs mixing
      CALL  HGSMIX( SIN2W, ZM, WM, TANB, PSHMAS, HG0MAS, HGCMAS, TANA)

C  determine Sfermion-fermion-chargino/neutralino couplings
      CALL        INOFSF( ALPHE, SIN2W, WM,
     &                    EMAS, UMAS, DMAS,
     &                    TANB, PHIL, PHIR, ER, ON, ETA,
     &                    GCESNL, GCNSEL, GCNSER,
     &                    GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     &                    GNNSNL, GNESEL, GNESER,
     &                    GNUSUL, GNUSUR, GNDSDL, GNDSDR )

C       determine ino-ino-gauge_boson  couplings
      CALL        INOGBS( ALPHE, SIN2W,
     &                    PHIL, PHIR, ER, ON, ETA,
     &                    GNCW, GCCZ, GNNZ )

C       determine ino-ino-Higgs couplings
      CALL       INOHGS( ALPHE, SIN2W, TANB, TANA,
     &                    PHIL, PHIR, ER, ON, ETA,
     &                    GCCH, GCNH, GNNH )

C       set SM couplings etc.
      GAMW = 2.25
      GAMZ = 2.53
      PI  = ACOS(0.)*2.
      EC2 = 4.*PI*ALPHE
      GWEAK = SQRT(EC2/SIN2W)
      COS2W = 1.-SIN2W
      GZ    = SQRT(EC2/COS2W/SIN2W)
C   f-f_bar-Z
      GZNL = -GZ*( 0.5 )
      GZNR = 0.
      GZEL = -GZ*(-0.5+SIN2W)
      GZER = -GZ*(     SIN2W)
      GZUL = -GZ*( 0.5-2./3.*SIN2W)
      GZUR = -GZ*(    -2./3.*SIN2W)
      GZDL = -GZ*(-0.5+1./3.*SIN2W)
      GZDR = -GZ*(     1./3.*SIN2W)
C   f1-f2_bar-W
      GWFL = -GWEAK/SQRT(2.)
      GWFR = 0.

C=============================
C neutralino decay
C=============================
C loop over neutralino_J
      DO 1000 J=1,4

C----
C 2-bodies decays
C----
C      calculate decay widthes of neutralino_4 2-body decays
C  neutralino_J --> chargino_i(-) + W(+)  + h.c.
      N = 0
      DO 10 I=1,2
      F1MAS = SZM(J)
      F2MAS = SWM(I)
      VMAS  = WM
      GL = GNCW(1,J,I)
      GR = GNCW(2,J,I)
      CALL F1F2VD( F1MAS, F2MAS, VMAS, GL, GR, DCYWDT )
      N = N+1
      DCNINO(N,J) = DCYWDT *2.
10    CONTINUE
C N is 2

C  neutralino_J --> chargino_i(-) + Higgs(+)  + h.c.
      DO 11 I=1,2
      F1MAS = SZM(J)
      F2MAS = SWM(I)
      SCMAS = HGCMAS
      GL = GCNH(1,I,J)
      GR = GCNH(2,I,J)
      CALL F1F2SC( F1MAS, F2MAS, SCMAS, GL, GR, DCYWDT )
      N = N+1
      DCNINO(N,J) = DCYWDT *2.
11    CONTINUE
C N is 4

C  neutralino_J --> neutralino_i + Higgs0_k
      DO 12 I=1,4
      DO 12 K=1,3
      F1MAS = SZM(J)
      F2MAS = SZM(I)
      IF( K.LE.2 ) THEN
        SCMAS = HG0MAS(K)
      ELSE
        SCMAS = PSHMAS
      ENDIF
      GL = GNNH(1,I,J,K)
      GR = GNNH(2,I,J,K)
      CALL F1F2SC( F1MAS, F2MAS, SCMAS, GL, GR, DCYWDT )
      N = N+1
      DCNINO(N,J) = DCYWDT
12    CONTINUE
C N is 16

C  neutralino_J --> neutralino_i + Z0
      DO 13 I=1,4
      F1MAS = SZM(J)
      F2MAS = SZM(I)
      VMAS = ZM
      GL = GNNZ(1,I,J)
      GR = GNNZ(2,I,J)
      CALL F1F2VD( F1MAS, F2MAS, VMAS, GL, GR, DCYWDT )
      N = N+1
      DCNINO(N,J) = DCYWDT
13    CONTINUE
C N is 20

C----
C 3-bodies decays
C----
C  neutralino_J --> neutralino_I + f f_bar
C    by virtual Z exchange, mass(f) is neglected
      AM1 = SZM(J)
      AMV = ZM
      GAMV= GAMZ

      DO 14 I=1,4
       AM2 = SZM(I)
       G1L = GNNZ(1,I,J)
       G1R = GNNZ(2,I,J)
C   1. (f f_bar) = (nu nu_bar) type
       G2L = GZNL
       G2R = GZNR
       NF = 3
       NCOL=1
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCNINO(N,J) = DCYWDT*NF*NCOL
C   2. (f f_bar) = (e e_bar)   type
       G2L = GZEL
       G2R = GZER
       NF = 3
       NCOL=1
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCNINO(N,J) = DCYWDT*NF*NCOL
C   3. (f f_bar) = (u u_bar)   type
       G2L = GZUL
       G2R = GZUR
       NF = 2
       NCOL=3
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCNINO(N,J) = DCYWDT*NF*NCOL
C   4. (f f_bar) = (d d_bar)   type
       G2L = GZDL
       G2R = GZDR
       NF = 3
       NCOL=3
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCNINO(N,J) = DCYWDT*NF*NCOL
14    CONTINUE
C N is 36

C  neutralino_J --> chargino_I + f1 f2_bar
C     by virtual W exchange, mass(f) is neglected
      AM1 = SZM(J)
      AMV = WM
      GAMV= GAMW
      G2L = GWFL
      G2R = GWFR
      DO 15 I=1,2
       AM2 = SWM(I)
C       be careful to give couplings
       G1L = AIMAG(GNCW(1,J,I))
       G1R = AIMAG(GNCW(2,J,I))
C   1.(f1 f2_bar) = (e nu_bar)  type
       NF = 3
       NCOL=1
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCNINO(N,J) = DCYWDT*NF*NCOL
C   2.(f1 f2_bar) = (u d_bar)   type
       NF = 2
       NCOL=3
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCNINO(N,J) = DCYWDT*NF*NCOL
15    CONTINUE
C N is 40

1000  CONTINUE
C end of neutralino loop

C=============================
C chargino decay
C=============================
C loop over chargino_K
      DO 1100 K=1,2
C----
C 2-bodies decays
C----
C      calculate decay widthes of chargino 2-body decays
C  chargino_K --> chargino_1 + Z0
      N=0
      IF( K.EQ.2 ) THEN
        F1MAS = SWM(2)
        F2MAS = SWM(1)
        VMAS = ZM
        GL = GCCZ(1,1,2)
        GR = GCCZ(2,1,2)
        CALL F1F2VD( F1MAS, F2MAS, VMAS, GL, GR, DCYWDT )
        N = N+1
        DCCINO(K,N) = DCYWDT
      ELSE
        N = N+1
        DCCINO(K,N) = 0
      ENDIF
C N is 1

C  chargino_K --> chargino_1 + H0_j
      IF( K.EQ.2 ) THEN
      F1MAS = SWM(2)
      F2MAS = SWM(1)
      DO 20 J = 1,3
        IF( J.LE.2 ) THEN
          SCMAS = HG0MAS(J)
        ELSE
          SCMAS = PSHMAS
        ENDIF
        GL = GCCH(1,2,1,J)
        GR = GCCH(2,2,1,J)
        CALL F1F2SC( F1MAS, F2MAS, SCMAS, GL, GR, DCYWDT )
        N = N+1
        DCCINO(K,N) = DCYWDT
20    CONTINUE
      ELSE
      DO 21 J=1,3
        N = N+1
        DCCINO(K,N) = 0
21    CONTINUE
      ENDIF
C N is 4

C  chargino_K --> neutralino_j + W(-)
      F1MAS = SWM(K)
      DO 22 J = 1,4
        F2MAS = SZM(J)
        VMAS = WM
        GL = GNCW(1,J,K)
        GR = GNCW(2,J,K)
        CALL F1F2VD( F1MAS, F2MAS, VMAS, GL, GR, DCYWDT )
        N = N+1
        DCCINO(K,N) = DCYWDT
22    CONTINUE
C N is 8

C  chargino_K --> neutralino_j + H(-)
      F1MAS = SWM(K)
      DO 23 J = 1,4
        F2MAS = SZM(J)
        SCMAS = HGCMAS
        GL = CONJG(GCNH(2,K,J))
        GR = CONJG(GCNH(1,K,J))
        CALL F1F2SC( F1MAS, F2MAS, SCMAS, GL, GR, DCYWDT )
        N = N+1
        DCCINO(K,N) = DCYWDT
23    CONTINUE
C N is 12

C----
C 3-bodies decays
C----
C  chargino_2 --> chargino_1 + f f_bar
C    by virtual Z exchange, mass(f) is neglected
      IF( K.EQ.2 ) THEN
       AM1 = SWM(2)
       AM2 = SWM(1)
       AMV = ZM
       GAMV= GAMZ
       G1L = GCCZ(1,1,2)
       G1R = GCCZ(2,1,2)
C   1. (f f_bar) = (nu nu_bar) type
       G2L = GZNL
       G2R = GZNR
       NF = 3
       NCOL=1
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCCINO(K,N) = DCYWDT*NF*NCOL
C   2. (f f_bar) = (e e_bar)   type
       G2L = GZEL
       G2R = GZER
       NF = 3
       NCOL=1
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCCINO(K,N) = DCYWDT*NF*NCOL
C   3. (f f_bar) = (u u_bar)   type
       G2L = GZUL
       G2R = GZUR
       NF = 2
       NCOL=3
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCCINO(K,N) = DCYWDT*NF*NCOL
C   4. (f f_bar) = (d d_bar)   type
       G2L = GZDL
       G2R = GZDR
       NF = 3
       NCOL=3
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCCINO(K,N) = DCYWDT*NF*NCOL
      ELSE
       DO 24 J=1,4
         N = N+1
         DCCINO(K,N) = 0
24     CONTINUE
      ENDIF
C N is 16

C  chargino_K --> neutralino_j + f1 f2_bar
C     by virtual W exchange, mass(f) is neglected
      AM1 = SWM(K)
      AMV = WM
      GAMV= GAMW
      G2L = GWFL
      G2R = GWFR
      DO 25 J=1,4
       AM2 = SZM(J)
       G1L = GNCW(1,J,K)
       G1R = GNCW(2,J,K)
C    (f1 f2_bar) = (e nu_bar)  type
       NF = 3
       NCOL=1
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       N = N+1
       DCCINO(K,N) = DCYWDT*NF*NCOL
C    (f1 f2_bar) = (u d_bar)   type
       NF = 2
       NCOL=3
       CALL FR3DCY( G1L,G1R, G2L,G2R, AM1,AM2, AMV,GAMV,  DCYWDT )
       L = L+1
       N = N+1
       DCCINO(K,N) = DCYWDT*NF*NCOL
25    CONTINUE
C N is 24

1100  CONTINUE
C end of chargino loop




      RETURN
      END
CDECK  ID>, SF2BR.
C----------------------------------------------------------------------C
      SUBROUTINE SF2BR(  ZM, WM, SIN2W, ALPHE, ALPHS,
     &                   HM1, G3MAS, TANB, RELAX,
     &                   EMAS, UMAS, DMAS,
     &                   SFMSL, SFMSR, SNEUM, PSMAS,
     &                   SWM, SZM, HGCMAS, HG0MAS,
     &                   DCWSEL, DCWSER, DCWSNL,
     &                   BRSEL, BRSER, BRSNL,
     &                   DCWIN1, BRINO1 )
C----------------------------------------------------------------------C
C 17/Jul/95 modified to include ZM as an input argument
C Purpose: calculate decay widthes & brancing ratios for
C          2-body-sferiom decays such as
C            1.) charged slepton --> neutrino + chargino
C            2.) charged slepton --> charged lepton + neutralino
C            3.) sneutrino       --> charged lepton + chargino
C            4.) sneutrino       --> neutrino + neutralino
C Inputs:  WM, SIN2W, ALPHE, ALPHS / parms. for SM.
C          HM1, G3MAS, TANB        / parms. for MSSM.
C          EMAS, UMAS, DMAS        / masses for charged lepton,
C                                    u-type & d-type quarks
C          SFMSL, SFMSR, SNEUM     / masses for sleptons
C          PSMAS                   / pseudo-scaler higgs mass
C          RELAX                   / to relax the GUT condition
C Output: SWM, SZM                 / chargino,neutralino masses
C         DCWSEL, DCWSER, DCWSNL   / decay widthes
C         BRSEL, BRSER, BRSNL      / branching ratios
C         DCWIN1, BRINO1           / 2-body decay widthes & branching
C                                    ratios of chargino(1)-->neutralino
C----------------------------------------------------------------------C
      REAL*4  OL(2,2), OR(2,2), ER
      REAL*4  ON(4,4)
      COMPLEX*8 ETA(4), UN(4,4), GL, GR
      REAL*4  SWM(2), SZM(4), INOMS
      REAL*4  RELAX

C bug fixed by T.T  '95/Jul/8
Cbug      COMPLEX*8 GCESNL(2,4), GCNSEL(2,4), GCNSER(2,4),
Cbug     &          GCUSDL(2,4), GCUSDR(2,4), GCDSUL(2,4), GCDSUR(2,4)
      COMPLEX*8 GCESNL(2,2), GCNSEL(2,2), GCNSER(2,2),
     &          GCUSDL(2,2), GCUSDR(2,2), GCDSUL(2,2), GCDSUR(2,2)
      COMPLEX*8 GNNSNL(2,4), GNESEL(2,4), GNESER(2,4),
     &          GNUSUL(2,4), GNUSUR(2,4), GNDSDL(2,4), GNDSDR(2,4)

      COMPLEX*8 GNCW(2,4,2)
      COMPLEX*8 GCCZ(2,2,2)
      COMPLEX*8 GNNZ(2,4,4)

      COMPLEX*8 GCCH(2,2,2,3)
      COMPLEX*8 GCNH(2,2,4), GNNH(2,4,4,3)

      REAL*4  DCWSEL(6), DCWSER(6), DCWSNL(6)
      REAL*4  BRSEL(6), BRSER(6), BRSNL(6)

      REAL*4 DCWIN1(8), BRINO1(8)

      REAL*4 HG0MAS(2)
C------------------------------------------------
      WRITE(6,*)'SF2BR CALLED'
C    No printout diagonalization results
      IFLG = 1
      CALL       INOMIX( ZM, WM, SIN2W, ALPHE, ALPHS,
     &                   HM1, G3MAS, TANB,
     &                   SWM, SZM,
     &                   OL, OR, ER, PHIL, PHIR,
     &                   ON, UN, ETA, RELAX, 
     &                   IFLG  )

C   calculate higgs mixing
      CALL  HGSMIX( SIN2W, ZM, WM, TANB, PSMAS, HG0MAS, HGCMAS, TANA)

C  determine Sfermion-fermion-chargino/neutralino couplings
      CALL        INOFSF( ALPHE, SIN2W, WM,
     &                    EMAS, UMAS, DMAS,
     &                    TANB, PHIL, PHIR, ER, ON, ETA,
     &                    GCESNL, GCNSEL, GCNSER,
     &                    GCUSDL, GCUSDR, GCDSUL, GCDSUR,
     &                    GNNSNL, GNESEL, GNESER,
     &                    GNUSUL, GNUSUR, GNDSDL, GNDSDR )

C       determine ino-ino-gauge_boson  couplings
      CALL        INOGBS( ALPHE, SIN2W,
     &                    PHIL, PHIR, ER, ON, ETA,
     &                    GNCW, GCCZ, GNNZ )

C       determine ino-ino-Higgs couplings
      CALL       INOHGS( ALPHE, SIN2W, TANB, TANA,
     &                    PHIL, PHIR, ER, ON, ETA,
     &                    GCCH, GCNH, GNNH )

      DO 5 I=1,8
        DCWIN1(I) = 0
        BRINO1(I) = 0
5     CONTINUE

      DO 11 I=1,6
        DCWSEL(I) = 0
        DCWSER(I) = 0
        DCWSNL(I) = 0
        BRSEL(I) = 0
        BRSER(I) = 0
        BRSNL(I) = 0
11    CONTINUE

C
C---
C  calculate decay widthes for 2-body-sfermion decay
C---
C left-handed Selectron decay
C   Se_L --> nu_e + chargino(i)
      FMAS  = 0.
      DO 10 I=1,2
        INOMS = SWM(I)
C      note the convention
        GR = GCNSEL(1,I)
        GL = GCNSEL(2,I)
        CALL SCF1F2( SFMSL, FMAS, INOMS, GL, GR, DCYWDT )
        DCWSEL(I) = DCYWDT
10    CONTINUE
C   Se_L --> e + neutralino(i)
      FMAS  = EMAS
      DO 20 I=1,4
        INOMS = SZM(I)
C      note the convention
        GR = GNESEL(1,I)
        GL = GNESEL(2,I)
        CALL SCF1F2( SFMSL, FMAS, INOMS, GL, GR, DCYWDT )
        DCWSEL(I+2) = DCYWDT
20    CONTINUE

C Right-handed Selectron decay
C   Se_R --> nu_e + chargino(i)
      FMAS  = 0.
      DO 30 I=1,2
        INOMS = SWM(I)
C      note the convention
        GR = GCNSER(1,I)
        GL = GCNSER(2,I)
        CALL SCF1F2( SFMSR, FMAS, INOMS, GL, GR, DCYWDT )
        DCWSER(I) = DCYWDT
30    CONTINUE
C   Se_R --> e + neutralino(i)
      FMAS  = EMAS
      DO 40 I=1,4
        INOMS = SZM(I)
C      note the convention
        GR = GNESER(1,I)
        GL = GNESER(2,I)
        CALL SCF1F2( SFMSR, FMAS, INOMS, GL, GR, DCYWDT )
        DCWSER(I+2) = DCYWDT
40    CONTINUE

C Sneutrino decay
C  snu_e --> electron + chargino(i)
      FMAS  = EMAS
      DO 60 I=1,2
        INOMS = SWM(I)
C      note the convention
        GR = GCESNL(1,I)
        GL = GCESNL(2,I)
        CALL SCF1F2( SNEUM, FMAS, INOMS, GL, GR, DCYWDT )
        DCWSNL(I) = DCYWDT
60    CONTINUE
C  snu_e --> neutrino + neutralino(i)
      FMAS  = 0.
      DO 65 I=1,4
        INOMS = SZM(I)
C      note the convention
        GR = GNNSNL(1,I)
        GL = GNNSNL(2,I)
        CALL SCF1F2( SNEUM, FMAS, INOMS, GL, GR, DCYWDT )
        DCWSNL(I+2) = DCYWDT
65    CONTINUE

C---
C  calculate branching ratios
C---

      TOTDL = 0
      TOTDR = 0
      TOTDN = 0
      DO 50 I=1,6
       TOTDL = TOTDL+DCWSEL(I)
       TOTDR = TOTDR+DCWSER(I)
       TOTDN = TOTDN+DCWSNL(I)
50    CONTINUE

      IF( TOTDL.GT.0 ) THEN
        DO 55 I=1,6
         BRSEL(I) = DCWSEL(I)/TOTDL
55      CONTINUE
      ENDIF

      IF( TOTDR.GT.0 ) THEN
        DO 56 I=1,6
         BRSER(I) = DCWSER(I)/TOTDR
56      CONTINUE
      ENDIF

      IF( TOTDN.GT.0 ) THEN
        DO 57 I=1,6
         BRSNL(I) = DCWSNL(I)/TOTDN
57      CONTINUE
      ENDIF

C--- calculate chargino_1 2-body decay widthes ( to neutralino )
C  chargino_1 --> neutralino_j + W(-)
      F1MAS = SWM(1)
      DO 70 J = 1,4
        F2MAS = SZM(J)
        VMAS = WM
        GL = GNCW(1,J,1)
        GR = GNCW(2,J,1)
        CALL F1F2VD( F1MAS, F2MAS, VMAS, GL, GR, DCYWDT )
        DCWIN1(J) = DCYWDT
70    CONTINUE

C  chargino_1 --> neutralino_j + H(-)
      F1MAS = SWM(1)
      DO 72 J = 1,4
        F2MAS = SZM(J)
        SCMAS = HGCMAS
C bug fixed by T.T  '95/Jul/5
Cbug        GL = CONJG(GCNH(2,2,J))
Cbug        GR = CONJG(GCNH(1,2,J))
        GL = CONJG(GCNH(2,1,J))
        GR = CONJG(GCNH(1,1,J))
        CALL F1F2SC( F1MAS, F2MAS, SCMAS, GL, GR, DCYWDT )
        DCWIN1(J+4) = DCYWDT
72    CONTINUE


      TOTD = 0
      DO 75 I=1,8
      IF( DCWIN1(I).LT.0 ) THEN
       WRITE(6,*) ' I=',I,' G3M=',G3MAS
      ENDIF
75    TOTD = TOTD+DCWIN1(I)
      IF( TOTD.GT.0 ) THEN
        DO 80 I=1,8
80      BRINO1(I) = DCWIN1(I)/TOTD
      ENDIF



      RETURN
      END
CDECK  ID>, UHPHSN.
C     NP = # external particles.
C     IB = 1, ..., NP-3 : branch sequence number.
C
C   Branch sequence is controlled by
C
C     IPBR(0,IB)  = JB for parent branch.
C         (1,IB)  = JB for 1-st daughter branch.
C         (2,IB)  = JB for 2-nd daughter branch.
C   where
C
C     JB = -1,...,-NP+4 : subsequent branches.
C        = 0            : the first branch from CM to 2 branches.
C        = 1, 2         : intial state particles.
C        = 3, ..., NP   : final state particles.
C
C   which point to
C
C     XMASS(JB)   = q**2 for branch JB if JB =0 or <0.
C          (JB)   = m**2 for particle JB if JB > 0.
C     XANGL(1,JB) = cos(theta) for branch JB( JB =0 or <0 ).
C          (2,JB) = phi        for branch JB( JB =0 or <0 ).
C
CC**********************************************************************
C*
C*========================================------===
C* Subroutine UHPHSN(NP,IPBR,XMASS2,XANGL,WAT,PV)
C*========================================------===
C*
C* (Purpose)
C*    Calculate (NP-2)-body phase space.
C* (Inputs)
C*       NP          : (I*4) : # external particles.
C*       IPBR  (0,i) : (I*4) : parent # for i-th branch.
C*             (1,i) : (I*4) : 1-st daughter # for i-th branch.
C*             (2,i) : (I*4) : 2-nd daughter # for i-th branch.
C*       XMASS2(  j) : (R*4) : invariant mass squared for branch #j.
C*       XANGLE(1,j) : (R*4) : cos(theta) for branch #j.
C*             (2,j) : (R*4) : phi for branch #j.
C* (Output)
C*       WAT      : (R*4) : (flux factor)*(phase space weight).
C*       PV(*,i)  : (R*4) : 4-momentum of i-th particle, where i=1,2
C*                        : should be initial state particles.
C*
CC**********************************************************************

      SUBROUTINE UHPHSN(NP,IPBR,XMASS2,XANGL,WAT,PV)

      IMPLICIT     REAL*4  ( A-H, O-Z    )
      INTEGER*4    NP, IPBR(0:2,NP-3)
      REAL   *4    XMASS2(-NP+4:NP), XANGL(2,-NP+4:0), WAT, PV(0:3,NP)
C--
C  MXxNP is the maximum number of external particles.
C--
      PARAMETER    ( MXxNP = 20 )
      REAL   *4    QV(0:3,-MXxNP:MXxNP), EE(3,3,-MXxNP:MXxNP)
      DATA NCALL /0/
C--
C  Statement function.
C--
      BETA(X1,X2) = SQRT(MAX(0.,1-2*(X1+X2)+(X1-X2)**2))
C
C========< Entry Point >================================================
C
C--
C  Initialize CM frame axes.
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL  = 1
         CALL UVZERO(9,EE(1,1,0))
         EE(1,1,0) = 1
         EE(2,2,0) = 1
         EE(3,3,0) = 1
         xPI  = ACOS(-1.)
         x2PI = 2*xPI
         x4PI = 4*xPI
      ENDIF
C--
C  Clear 4-momenta.
C--
      CALL UVZERO(4*NP,PV)
      CALL UVZERO(4*(2*MXxNP+1),QV)
C--
C  Prepare initial state momenta.
C     p1 + p2 ---> CM
C--
      S       = XMASS2(0)
      AM12    = XMASS2(1)
      AM22    = XMASS2(2)
      BT0     = BETA(AM12/S,AM22/S)
      RS      = SQRT(S)
      PBM     = (RS/2)*BT0
      EBM1    = SQRT(AM12+PBM**2)
      EBM2    = SQRT(AM22+PBM**2)
      QV(0,1) = EBM1
      QV(3,1) = PBM
      QV(0,2) = EBM2
      QV(3,2) = -PBM
      QV(0,0) = RS
C--
C  Loop over final state branches.
C--
      NBR   = NP - 3
      WAT   = x2PI/(x4PI**(3*NBR))/(2*S*BT0)
      MODE  = 1
      DO 1000 IBR = 1, NBR
         IPR  = IPBR(0,IBR)
         IDT1 = IPBR(1,IBR)
         IDT2 = IPBR(2,IBR)
         AM12 = XMASS2(IDT1)
         AM22 = XMASS2(IDT2)
         CALL UHPHS2(MODE,QV(0,IPR),AM12,AM22,EE(1,1,IPR),
     .               XANGL(1,IPR),XANGL(2,IPR),QV(0,IDT1),QV(0,IDT2),
     .               BT,EE(1,1,IDT1))
         IF ( BT.EQ.0. )                         GO TO 9999
         CALL UVCOPY(9,EE(1,1,IDT1),EE(1,1,IDT2))
         WAT  = WAT*BT
         MODE = 2
1000  CONTINUE
C--
C  Store final state particle momenta in PV.
C--
      CALL UVCOPY(4*NP,QV(0,1),PV(0,1))
C--
C  That's it.
C--
      RETURN
C--
C  Kinematically forbidden.
C--
9999  WAT = 0
      RETURN
      END
CDECK  ID>, DFGTW.
C===
C Main program for the chargino width calculation.
C C.Dionisi,K.Fujii,S.Giagu,T.Tsukamoto
C===
      SUBROUTINE DFGTW
      IMPLICIT REAL*8 (A-H,O-Z)

      CHARACTER*128   BSINF, BSOUTF
      INTEGER*4 NGETBS, NUBS, NPUTBS, NDOSPR, NPRINF, NPRHST, LUINFO
      INTEGER*4 NBSINF, NOSPEV
      REAL*8 ESTIM, SIGMA, CTIME
      INTEGER*4 IT1, IT2, MXTRY

      COMMON /BS51PR/ ESTIM, SIGMA, CTIME, IT1, IT2, MXTRY,
     >        NGETBS, NUBS, NPUTBS, NDOSPR, NPRINF, NPRHST,
     >        LUINFO, NBSINF, NOSPEV,
     >        BSINF, BSOUTF

      COMMON /BS51HT/ LUHIST, BSHSTF
      INTEGER*4       LUHIST
      CHARACTER*128   BSHSTF
C* (Contents)
C*    SQRTS   : (R*4) : sqrt(s).
C*    POLEBM  : (R*4) : electron beam polarization.
C*    SGMEBM  : (R*4) : beam energy spread (fraction).
C*    ISRB    : (I*4) : ISR and BM flag.
C*

      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO
      INTEGER*4       ISRB
      EXTERNAL FUNCXD

C--
C Initialization : set default parameters for BASES/SPRING
C--
      CALL DFGT_PAR(1)

      CALL BSINIT

      CALL DFGTW_IN

C--- NEW BECAUSE LUJET
C -- CHARGINI
C

C--
C  BASES Integration
C--
      CALL BASES( FUNCXD, ESTIM, SIGMA, CTIME, IT1, IT2 )
      GAMWINO = ESTIM
      WRITE(*,*)' '
      WRITE(*,*)'********************************************** '
      WRITE(*,*)'Chargino 3-body width=',ESTIM,' +- ', SIGMA
      WRITE(*,*)'********************************************** '
      WRITE(*,*)'CTIME =',CTIME
      WRITE(*,*)'IT1   =',IT1,' IT2=',IT2
      WRITE(*,*)' '
CSG      CALL BSINFO(LUINFO)
      CALL BHPLOT(LUINFO)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, GMT2QW.
C*
C* (Inputs)
C*    AMT    : (R*4) : top mass.
C*    AMD    : (R*4) : down type quark mass.
C*    AMW    : (R*4) : W mass.
C*    VFF    : (R*4) : KM matrix element.
C* (Output)
C*    GM     : (R*4) : partial width for t --> D + W.
C*

      SUBROUTINE GMT2QW(AMT,AMD,AMW,VFF,GM)

      IMPLICIT REAL*4 ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4 AMT, AMD, AMW, VFF,  GM
C--
C  Statement function.
C--
      BETA(X1,X2) = SQRT( 1 - 2*(X1+X2) + (X1-X2)**2 )
C
C========< Entry Point >================================================
C
C--
C  Calculate width.
C--
      IF ( AMD+AMW.GE.AMT ) THEN
         GM = 0
         RETURN
      ENDIF
      X1  = (AMD/AMT)**2
      X2  = (AMW/AMT)**2
      FAC = (xALF/xSIN2W)/16
      GM  = FAC*(AMT**3/AMW**2)*(VFF**2)
     .         *BETA(X1,X2)*( (1-X1)**2 + X2*(1+X1) - 2*X2*X2 )
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, INOMIX.
C----------------------------------------------------------------------C
      SUBROUTINE INOMIX( ZM, WM, SIN2W, ALPHE, ALPHS,
     &                   HM1, G3MAS, TANB,
     &                   SWM, SZM,
     &                   OL, OR, ER, PHIL, PHIR,
     &                   ON, UN, ETA, RELAX, 
     &                   IFLG  )
C----------------------------------------------------------------------C
C=======================================================================
C   Diagonalize mass matrices of charged(neutral) gaugino & higgsino
C   and then calculate some constants such as couplings etc.
C  Ref.   Hikasa-san's note
C  Mod. in diagonalization routine by Stefano Giagu 24/1/1995
C  17/Jul/95  ZM is modified to be an input parameter.  T.T
C
C <<< Inputs & Output >>>
C    In:
C     IFLG = 0 : print out diagonalization results
C          .ne.0   no prints
C    Out:
C     IFLG < 0 : error
C     IFLG > 0 : OK
C
C <<< Inputs >>>  ---> all inputs are real*4
C ( standard model constants )
C     1) WM    : mass of a W-boson
C     2) SIN2W : weak mixing angle
C     3) ALPHE : fine structure constant
C     4) ALPHS : strong coupling constant
C ( free parameters )
C     1) HM1   : the higgsino mass term  > 0
C     2) G3MAS : gaugino mass for SU(3)  > 0
C     3) TANB=V2/V1 : ratio of the v.e.v. of two higgs doublets
C     ( gaugino mass relation at the weak scale )
C        G2MAS = (ALPHE/ALPHS/SIN2W) * G3MAS
C        G1MAS = (5/3)*TAN2W * G2MAS * RELAX
C        RELAX = to relax the GUT condition
C
C <<< outputs >>>
C REAL*4  ZM                  Z boson mass
C REAL*4  SWM(2), SZM(4)      masses of charginos & neutralinos ( > 0 )
C REAL*4  OL(2,2), OR(2,2), ER      matrices etc. for diagonalization
C REAL*4  PHIL, PHIR                mixing angles
C REL*4   ON(4,4)                   matrices etc. for diagonalization
C COMPLEX*8  UN(4,4), ETA(4)
C=============
C REAL*4  MIXCHG(2,2), MIXNEU(4,4)  mixing matrices
C=======================================================================

      PARAMETER ( PI = 3.1415927 )

      REAL*4  MIXCHG(2,2), MIXNEU(4,4)
      REAL*4  OL(2,2), OR(2,2), ER
      REAL*4  ON(4,4)
      COMPLEX*8 ETA(4), UN(4,4)
      REAL*4  SWM(2), SWM2(2)
      REAL*4  SZM(4), SZM1(4)
      REAL*4  RELAX
C
C CCC SG
C  for DJACOBI
      REAL*8  AJJ(4,4)
C  for SSL-2
      REAL*8  WRKARY(10), EV(4,4), EI(4), VW(10), EV1(4,4), EI1(4)
Cxxx      REAL*4  WRKARY(10), EV(4,4), EI(4), VW(10), EV1(4,4), EI1(4)
      INTEGER*4 IORDER(4)

      REAL*4 TSTMAT(4,4)
C-----------------------------------------------------------------------
C check
      write(6,*)'G3MAS',G3MAS,' HM1 ',HM1
C      IF( G3MAS.LT.0 .OR. HM1.LT.0 ) THEN
      IF( G3MAS.LT.0. ) THEN
       IFLG =-1
       RETURN
      ENDIF

      SQRT2 = SQRT(2.)

      COS2W = 1.-SIN2W
      TAN2W = SIN2W/COS2W
      SINW  = SQRT(SIN2W)
      COSW  = SQRT(COS2W)
      TANW  = SQRT(TAN2W)
CTT 17/Jul/95       ZM    = WM/COSW
      WM2   = WM**2
      ZM2   = ZM**2
      HM12  = HM1**2

C  0 < beta < pi
C     BETA  = ATAN2( ABS(TANB), SIGN(1.,TANB) )
C     COSB  = COS(BETA)
C     SINB  = SIN(BETA)
C     COSDB = (COSB-SINB)*(COSB+SINB)
C     SINDB = 2.*SINB*COSB
C     SIN2B = SINB**2
C     COS2B = COSB**2
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSB  = SIGN( SQRT(COS2B), TANB )
      SIN2B = 1.-COS2B
      SINB  = SQRT(SIN2B)
      COSDB = (COSB-SINB)*(COSB+SINB)
      SINDB = 2.*SINB*COSB

C  constants calculated by using of the given values
C    gaugino masses
      G2MAS = G3MAS/ALPHS * ALPHE/SIN2W
      G1MAS = G2MAS*SIN2W/(1.-SIN2W)*(5./3.) * RELAX

      G2MAS2 = G2MAS**2
      G1MAS2 = G1MAS**2

C  chargino mass matrix ( 2*2 )
      MIXCHG(1,1) = G2MAS
      MIXCHG(2,1) = SQRT2*WM*SINB
      MIXCHG(1,2) = SQRT2*WM*COSB
      MIXCHG(2,2) = HM1

C  neutralino mass matrix ( 4*4 )
      MIXNEU(1,1) = G1MAS
      MIXNEU(2,1) = 0.
      MIXNEU(2,2) = G2MAS
      MIXNEU(3,1) = -ZM* SINW*COSB
      MIXNEU(3,2) =  ZM* COSW*COSB
      MIXNEU(3,3) = 0.
      MIXNEU(4,1) =  ZM* SINW*SINB
      MIXNEU(4,2) = -ZM* COSW*SINB
      MIXNEU(4,3) = -HM1
      MIXNEU(4,4) = 0.

      MIXNEU(1,2) = MIXNEU(2,1)
      MIXNEU(1,3) = MIXNEU(3,1)
      MIXNEU(1,4) = MIXNEU(4,1)
      MIXNEU(2,3) = MIXNEU(3,2)
      MIXNEU(2,4) = MIXNEU(4,2)
      MIXNEU(3,4) = MIXNEU(4,3)

C  calculation for chargino
C   mass eigenvalues
      DELTC2 = (G2MAS2+HM12 +2*WM2 )**2
     &        -4.*( G2MAS*HM1 -WM2*SINDB)**2
CCCC
      IF( DELTC2 .LT.0 ) THEN
        WRITE(6,*) 'G3MAS=',G3MAS,' HM1=',HM1, ' TANB=',TANB
        WRITE(6,*) 'G2MAS=',G2MAS,' WM=',WM, ' SINDB=',SINDB
        IFLG = -1
        RETURN
      ENDIF
      DELTC  = SQRT(DELTC2)
      SWM2(1) = 0.5 *(G2MAS2 +HM12 + 2.*WM2-DELTC)
C     SWM2(2) = 0.5 *(G2MAS2 +HM12 + 2.*WM2+DELTC)
      SWM2(2) = SWM2(1) + DELTC
      SWM(1) = SQRT(SWM2(1))
      SWM(2) = SQRT(SWM2(2))
      TANDPL = 2.*SQRT2*WM*(G2MAS*COSB+HM1*SINB)
     &         /(G2MAS2-HM12-2.*WM2*COSDB)
      TANDPR = 2.*SQRT2*WM*(G2MAS*SINB+HM1*COSB)
     &         /(G2MAS2-HM12+2.*WM2*COSDB)
      TANPL = ( SWM2(1) -G2MAS2 -2.*WM2*SIN2B)
     &         /SQRT2/WM/(G2MAS*COSB+HM1*SINB)
      TANPR = ( SWM2(1) -G2MAS2 -2.*WM2*COS2B)
     &         /SQRT2/WM/(G2MAS*SINB+HM1*COSB)

C  0 < phi_L < pi
      PHIL  = ATAN2( ABS(TANPL), SIGN(1.,TANPL) )
      COSPL = COS(PHIL)
      SINPL = SIN(PHIL)
      OL(1,1) =  COSPL
      OL(2,1) = -SINPL
      OL(1,2) =  SINPL
      OL(2,2) =  COSPL

C  take 0 < phi_R < pi and check the lighter mass eigen value
      PHIR  = ATAN2( ABS(TANPR), SIGN(1.,TANPR) )
      COSPR = COS(PHIR)
      SINPR = SIN(PHIR)
      OR(1,1) =  COSPR
      OR(2,1) = -SINPR
      OR(1,2) =  SINPR
      OR(2,2) =  COSPR
C diagonalize mass matrix with OL & OR and check the 1st eigen value
C and then re-define phi_R if the 1st eigen value is less than 0.
C ( the 1st eigen value corresponds to the lighter chargino )
      AM1=0.
      DO 5  I=1,2
      DO 5  J=1,2
5     AM1 = AM1 + OR(1,I)*MIXCHG(I,J)*OL(1,J)

      IF( AM1.LT.0 ) THEN
        SINPR = -SINPR
        COSPR = -COSPR
      ENDIF
      PHIR  = ATAN2( SINPR, COSPR )
      ER = SIGN(1., G2MAS*HM1-WM2*SINDB)
      OR(1,1) =  COSPR
      OR(2,1) = -SINPR*ER
      OR(1,2) =  SINPR
      OR(2,2) =  COSPR*ER

      IF( IFLG .EQ.0 ) THEN
        WRITE(6,*) '%%%%% diagonalization results for chargino'
        WRITE(6,*) '   mixing matrix'
        WRITE(6,102) '  M_chargino=', (MIXCHG(1,I),I=1,2)
        WRITE(6,102) '             ', (MIXCHG(2,I),I=1,2)
        WRITE(6,101) ' SWM=',SWM
        WRITE(6,103) ' TANPL=',TANPL, ' TANPR=',TANPR,'  ER=',ER
        WRITE(6,103) ' TANDPL=',TANDPL, ' TANDPR=',TANDPR
        WRITE(6,103) ' PHIL =',PHIL/PI*180.,
     &               ' PHIR =',PHIR/PI*180.
        WRITE(6,102) '  OL=', ( OL(1,I),I=1,2)
     &              ,'  OR=', ( OR(1,I),I=1,2)
        WRITE(6,102) '     ', ( OL(2,I),I=1,2)
     &              ,'     ', ( OR(2,I),I=1,2)
        DO 1001 I=1,2
        DO 1002 J=1,2
        TSTMAT(I,J)=0
        DO 1003 K=1,2
        DO 1004 L=1,2
        TSTMAT(I,J)=TSTMAT(I,J)+ OR(I,K)*MIXCHG(K,L)*OL(J,L)
1004    CONTINUE
1003    CONTINUE
1002    CONTINUE
1001    CONTINUE
        WRITE(6,*) ' TSTMAT(I,J)= OR(I,K)*MIXCHG(K,L)*OL(J,L)'
        WRITE(6,201) '     ', (TSTMAT(1,J),J=1,2)
        WRITE(6,201) '     ', (TSTMAT(2,J),J=1,2)
      ENDIF
101   FORMAT( 1H ,    A, 2(F14.7,1X ) )
102   FORMAT( 1H ,    A, 2(F14.7,1X ),
     &           4X , A, 2(F14.7,1X )  )
103   FORMAT( 1H ,    4( A,F14.7 ) )

C  diagonalize neutralino mass matrix
      AJJ(1,1) = MIXNEU(1,1)
      AJJ(2,1) = MIXNEU(2,1)
      AJJ(2,2) = MIXNEU(2,2)
      AJJ(3,1) = MIXNEU(3,1)
      AJJ(3,2) = MIXNEU(3,2)
      AJJ(3,3) = MIXNEU(3,3)
      AJJ(4,1) = MIXNEU(4,1)
      AJJ(4,2) = MIXNEU(4,2)
      AJJ(4,3) = MIXNEU(4,3)
      AJJ(4,4) = MIXNEU(4,4)
      AJJ(1,2) = MIXNEU(1,2)
      AJJ(1,3) = MIXNEU(1,3)
      AJJ(1,4) = MIXNEU(1,4)
      AJJ(2,3) = MIXNEU(2,3)
      AJJ(2,4) = MIXNEU(2,4)
      AJJ(3,4) = MIXNEU(3,4)
CSG      WRKARY(1) = MIXNEU(1,1)
CSG      WRKARY(2) = MIXNEU(2,1)
CSG      WRKARY(3) = MIXNEU(2,2)
CSG      WRKARY(4) = MIXNEU(3,1)
CSG      WRKARY(5) = MIXNEU(3,2)
CSG      WRKARY(6) = MIXNEU(3,3)
CSG      WRKARY(7) = MIXNEU(4,1)
CSG      WRKARY(8) = MIXNEU(4,2)
CSG      WRKARY(9) = MIXNEU(4,3)
CSG      WRKARY(10)= MIXNEU(4,4)
C
C CCC Diagonalization of neutralino mass matrix by JACOBY metods.
C CCC Stefano Giagu 24/1/1995
C
      NJ = 4
      CALL DJACOBI(AJJ,NJ,NJ,EI1,EV1,NROTT)
      NRO_MAX = 10 * NJ * NJ
      IF ( NJ .EQ. 1) THEN
        ICON = 10000
      ELSE IF ( NJ .LT. 1 ) THEN
        ICON = 30000
      ELSE IF ( NROTT .GT. NRO_MAX ) THEN
        WRITE(6,*) ' Warning in JACOBI : NROTT/NRO_MAX= ',NROTT,NRO_MAX
      ELSE
        ICON = 0
      ENDIF
C       ---- SSL-2 subroutine ----
Cxxx      CALL DSEIG1( WRKARY, 4, EI1, EV1, 4, MEIG, VW, ICON )
Cxxx      CALL SEIG1( WRKARY, 4, EI1, EV1, 4, MEIG, VW, ICON )
C       EI1(i)   : eigen values
C       EV1(*,i) : eigen vectors corresponding to EI1(i)
      IF( ICON .GT. 0 ) THEN
C      WRITE(6,*) ' Error in MIXMAT : SSL-2 Error (SEIG1)',
C    &            ' Condition code=', ICON
C      STOP
       IFLG = -ICON
       RETURN
      ENDIF
C       re-arrange eigen-values in increasing (absolute value) order
      DO 10 I=1,4
       SZM(I) = ABS(EI1(I))
10    CONTINUE
      CALL SWPELM( SZM, IORDER, SZM1, 4 )
      IF( IFLG .EQ.-1) THEN
        WRITE(6,*) '%%%%% diagonalization results for neutralino'
        WRITE(6,*) '   mixing matrix'
        WRITE(6,201) '  M_neutralino=', (MIXNEU(1,I),I=1,4)
        WRITE(6,201) '               ', (MIXNEU(2,I),I=1,4)
        WRITE(6,201) '               ', (MIXNEU(3,I),I=1,4)
        WRITE(6,201) '               ', (MIXNEU(4,I),I=1,4)
        WRITE(6,201) '     SZM   =', SZM
        WRITE(6,201) '     SZM1  =', SZM1
        WRITE(6,201) '     EI1   =', EI1
        WRITE(6,202) '     IORDER=', IORDER
        WRITE(6,201) '     EV1(1,*)=',(EV1(1,I),I=1,4)
        WRITE(6,201) '     EV1(2,*)=',(EV1(2,I),I=1,4)
        WRITE(6,201) '     EV1(3,*)=',(EV1(3,I),I=1,4)
        WRITE(6,201) '     EV1(4,*)=',(EV1(4,I),I=1,4)
201     FORMAT(1H , A, 4(F14.7,2X) )
202     FORMAT(1H , A, 4(I14  ,2X) )
      ENDIF
C       treat the degenerate case
14    CONTINUE
      DO 11 I=1,4
       IOK = 0
       DO 12 J=1,4
         IF( IORDER(J) .EQ. I ) IOK=1
12     CONTINUE
       IF( IOK .NE. 1 ) THEN
         DO 13 K=1,4
           IF( IORDER(K)+1 .EQ. I ) THEN
             IORDER(K) = I
             GO TO 14
           ENDIF
13       CONTINUE
       ENDIF
11    CONTINUE

      DO 15 I=1,4
C        J : order index in increasing (abs.value) order.
        J = 5-IORDER(I)
        EI(J) = EI1(I)
        SZM(J)= EI1(I)
        IF( SZM(J).GT.0. ) THEN
          ETA(J) = (1., 0.)
        ELSE
          ETA(J) = (0., 1.)
        ENDIF
        DO 16 K=1,4
          EV(K,J) = EV1(K,I)
          ON(J,K) = EV1(K,I)
16      CONTINUE
15    CONTINUE

      DO 17 I=1,4
      DO 17 J=1,4
17    UN(I,J)=ON(I,J)*ETA(I)

      DO 51 I=1,2
51    SWM(I) = ABS(SWM(I))
      DO 52 I=1,4
52    SZM(I) = ABS(SZM(I))

      IF( IFLG.EQ.0 ) THEN
        WRITE(6,*) '%%%%% diagonalization results for neutralino'
        WRITE(6,*) '   mixing matrix'
        WRITE(6,301) '  M_neutralino=', (MIXNEU(1,I),I=1,4)
        WRITE(6,301) '               ', (MIXNEU(2,I),I=1,4)
        WRITE(6,301) '               ', (MIXNEU(3,I),I=1,4)
        WRITE(6,301) '               ', (MIXNEU(4,I),I=1,4)
        WRITE(6,301) '     EI1   =', EI1
        WRITE(6,202) '     IORDER=', IORDER
        WRITE(6,301) '     SZM   =', SZM
        WRITE(6,*  ) '                    j--->'
        WRITE(6,301) '    EV1(i,j)=',(EV1(1,I),I=1,4)
        WRITE(6,301) '             ',(EV1(2,I),I=1,4)
        WRITE(6,301) '       i     ',(EV1(3,I),I=1,4)
        WRITE(6,301) '             ',(EV1(4,I),I=1,4)
        WRITE(6,*  ) ' '
        WRITE(6,302) '     ON(i,j)=',(ON(1,I),I=1,4),
     &  '    ETA=', ETA(1)
        WRITE(6,302) '             ',(ON(2,I),I=1,4),
     &  '    ETA=', ETA(2)
        WRITE(6,302) '       i     ',(ON(3,I),I=1,4),
     &  '    ETA=', ETA(3)
        WRITE(6,302) '             ',(ON(4,I),I=1,4),
     &  '    ETA=', ETA(4)
        DO 2001 I=1,4
        DO 2002 J=1,4
        TSTMAT(I,J)=0
        DO 2003 K=1,4
        DO 2003 L=1,4
        TSTMAT(I,J) = TSTMAT(I,J) +ON(I,K)*MIXNEU(K,L)*ON(J,L)
2003    CONTINUE
2002    CONTINUE
2001    CONTINUE
          WRITE(6,*) ' TSTMAT(I,J)= ON(I,K)*MIXNEU(K,L)*ON(J,L)'
          WRITE(6,201) '     ', (TSTMAT(1,J),J=1,4)
          WRITE(6,201) '     ', (TSTMAT(2,J),J=1,4)
          WRITE(6,201) '     ', (TSTMAT(3,J),J=1,4)
          WRITE(6,201) '     ', (TSTMAT(4,J),J=1,4)
        ENDIF
301       FORMAT(1H , A,  4(F14.7,2X) )
302       FORMAT(1H , A,  4(F14.7,2X), A, 2F3.0 )
CCCCCCCCCCCC
CCCCCCCCCCCC
      IFLG=1

      RETURN
      END
CDECK  ID>, SFMASS.
C*                           This version does not take into account
C*                           L-R mixings.
C*

      SUBROUTINE SFMASS(AM0,AM2,AMU,BETA,ALF,S2W,AMZ,AMSF2)

      IMPLICIT    REAL*8 ( A-H, O-Z )
      REAL*8      AM0, AM2, AMU, BETA, ALF, S2W, AMSF2(7)
      REAL*8      ALFT(3), B(3), F(3), C(2,7)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Constants at Q**2 = MZ**2
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         x2PI  = 2*ACOS(-1.)
         x4PI  = 2*x2PI
         NGN   = 3
C--MSSM
         B(1)  = (10/3.)*NGN + 1
         B(2)  = 2*NGN - 6 + 1
         B(3)  = 2*NGN - 9
      ENDIF
C--
C  Constants at Q**2 = MZ**2
C--
      TNB   = TAN(BETA)
      AMZCS = AMZ*AMZ*(TNB-1)*(TNB+1)/(TNB*TNB+1)
C--
C  MX.
C--
      TEE   = (x4PI/ALF)*(3-8*S2W)/(3*B(1)-5*B(2))
      AMX   = AMZ*EXP(TEE/2)
      ALFWI = S2W/ALF
      ALFXI = ALFWI - (B(2)/x4PI)*TEE
      ALFSI = ALFXI + (B(3)/x4PI)*TEE
C--
C     PRINT *, ' 1/ALF(MZ) = ', 1/ALF
C     PRINT *, ' SIN2W     = ', S2W
C     PRINT *, ' AMZ       = ', AMZ, ' GeV'
C     PRINT *, ' NGN       = ', NGN
C     PRINT *, ' AMX       = ', AMX
C     PRINT *, ' ALFS      = ', 1/ALFSI
C     PRINT *, ' --- '
C--
C  Alpha_tilde.
C--
      ALFT(3) = 1/ALFXI/x4PI
      ALFT(2) = ALFT(3)
      ALFT(1) = 3/5.*ALFT(2)
C--
C  f_i.
C--
      DO 10 I = 1, 3
         F(I) = 1/B(I)/ALFT(I) * ( 1 - 1/(1+B(I)*ALFT(I)*TEE)**2 )
10    CONTINUE
C--
C  M_i**2 = M_0**2 + C(1,i)*M**2 + C(2,i)*cos(2*beta)*M_Z**2.
C--
      C(1,1) = 2*( 4/3.*ALFT(3)*F(3) + 3/4.*ALFT(2)*F(2)
     .                               + 1/36.*ALFT(1)*F(1) )
      C(1,2) = C(1,1)
      C(1,3) = 2*( 4/3.*ALFT(3)*F(3) + 4/9.*ALFT(1)*F(1) )
      C(1,4) = 2*( 4/3.*ALFT(3)*F(3) + 1/9.*ALFT(1)*F(1) )
      C(1,5) = 2*( 3/4.*ALFT(2)*F(2) + 1/4.*ALFT(1)*F(1) )
      C(1,6) = C(1,5)
      C(1,7) = 2*ALFT(1)*F(1)
C--
      C1QSM  = C(1,1) + C(1,2) + C(1,3) + C(1,4)
      C1LSM  = C(1,5) + C(1,6) + C(1,7)
C--
      C(2,1) = -0.5 + 2/3.*S2W
      C(2,2) =  0.5 - 1/3.*S2W
      C(2,3) =      - 2/3.*S2W
      C(2,4) =      + 1/3.*S2W
      C(2,5) =  0.5 -      S2W
      C(2,6) = -0.5
      C(2,7) =      +      S2W
C--
C  Loop over sferminons. AMSF2 is mass**2.
C     AMSF2(I) = (SUL,SDL,SUR,SDR,SEL,SNL,SER)
C--
      AM   = ALFWI/ALFXI*AM2
      AM02 = AM0**2
      DO 20 I = 1, 7
         AMSF2(I) = AM02 + AM*AM*C(1,I) + C(2,I)*AMZCS
20    CONTINUE
C--
C  That's it.
C--
      RETURN
      END
C=====================================================================C
      SUBROUTINE SFLMAS( COMSCM, G2MAS, TANB, SIN2W, ZM,
     &                   SFMSL, SFMSR, SNEUM,
     &                   SFMSL2, SFMSR2, SNEUM2, IRET )
C=====================================================================C
      IRET=1
C fix sfermion masses
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSDB = 2.*COS2B-1.
      GMX2  = G2MAS**2 /0.679
      SFMSL2 = COMSCM**2 + 0.52*GMX2 - (0.5-SIN2W)* ZM**2 *COSDB
      SFMSR2 = COMSCM**2 + 0.15*GMX2 - SIN2W* ZM**2 *COSDB
      SNEUM2 = COMSCM**2 + 0.52*GMX2 +  0.5       * ZM**2 *COSDB

      IF( SFMSL2.LT.0 .OR. SFMSR2.LT.0 .OR. SNEUM2.LT.0 ) THEN
C       WRITE(6,*) ' G2MAS=',G2MAS, ' SFMSL2=',SFMSL2,
C    &             ' SFMSR2=',SFMSR2, ' SNEUM2=',SNEUM2
        SFMSL2= AMAX1( SFMSL2, 0.)
        SFMSR2= AMAX1( SFMSR2, 0.)
        SNEUM2= AMAX1( SNEUM2, 0.)
        IRET=-1
C       RETURN
      ENDIF

      SFMSL = SQRT( SFMSL2 )
      SFMSR = SQRT( SFMSR2 )
      SNEUM = SQRT( SNEUM2 )

      RETURN
      END
C=====================================================================C
      SUBROUTINE SFQMAS( COMSCM, G3MAS, TANB, SIN2W, ZM,
     &                   SFDML, SFDMR, SFUML, SFUMR,
     &                   SFDML2, SFDMR2, SFUML2, SFUMR2, IRET )
C=====================================================================C
      IRET=1
C fix sfermion masses
      TAN2B = TANB**2
      COS2B = 1./(1.+TAN2B)
      COSDB = 2.*COS2B-1.
      G3M2  = G3MAS**2
      SFDML2 = COMSCM**2 + 0.837*G3M2 -(0.5-1./3.*SIN2W)*ZM**2 *COSDB
      SFDMR2 = COMSCM**2 + 0.783*G3M2 - 1./3.* SIN2W    *ZM**2 *COSDB
      SFUML2 = COMSCM**2 + 0.837*G3M2 +(0.5-2./3.*SIN2W)*ZM**2 *COSDB
      SFUMR2 = COMSCM**2 + 0.783*G3M2 + 2./3.* SIN2W    *ZM**2 *COSDB

      IF( SFDML2.LT.0 .OR. SFDMR2.LT.0   .OR.
     &    SFUML2.LT.0 .OR. SFUMR2.LT.0  ) THEN
C       WRITE(6,*) ' G3MAS=',G3MAS, ' SFDML2=',SFDML2,
C    &             ' SFDMR2=',SFDMR2, ' SFUML2=',SFUML2,
C    &             ' SFUMR2=',SFUMR2
        SFDML2= AMAX1( SFDML2, 0.)
        SFDMR2= AMAX1( SFDMR2, 0.)
        SFUML2= AMAX1( SFUML2, 0.)
        SFUMR2= AMAX1( SFUMR2, 0.)
        IRET=-1
C       RETURN
      ENDIF

      SFDML = SQRT( SFDML2 )
      SFDMR = SQRT( SFDMR2 )
      SFUML = SQRT( SFUML2 )
      SFUMR = SQRT( SFUMR2 )

      RETURN
      END
CDECK  ID>, UHQ2BW.
CC**********************************************************************
C*
C*===================================------===
C* Subroutine UHQ2BW(AM,GM,QMN,QMX,Z,Q2,WAT)
C*===================================------===
C*
C* (Purpose)
C*    Reparametrizes a Breit-Wigner resonance for less singular
C*    behavior in numerical integration.
C* (Inputs)
C*       AM     : (R*8) : mass.
C*       GM     : (R*8) : width.
C*       QMN    : (R*8) : Q_min.
C*       QMX    : (R*8) : Q_max.
C*       Z      : (R*8) : integration variable in (0,1).
C* (Output)
C*       Q2     : (R*8) : Q^2.
C*       WAT    : (R*8) : Jacobian weight.
C* (Relation)
C*    Invokes no external routines.
C*
CC**********************************************************************

      SUBROUTINE UHQ2BW(AM,GM,QMN,QMX,Z,Q2,WAT)

      IMPLICIT   REAL*8 ( A-H, O-Z )
C--
C  Dummy arguments.
C--
      REAL   *8  AM, GM, QMN, QMX, Z, Q2, WAT
C
C========< Entry Point >================================================
C
C--
C  Define some variables.
C--
      AMGM  = AM*GM
      AM2   = AM*AM
      AMGM2 = AMGM*AMGM
C--
C  Calculate Q2.
C--
      THMN = ATAN((QMN-AM)*(QMN+AM)/AMGM)
      THMX = ATAN((QMX-AM)*(QMX+AM)/AMGM)
      TH   = THMN + (THMX-THMN)*Z
      Q2   = AMGM*TAN(TH) + AM2
C--
C  Then calculate Jacobian.
C--
      WAT  = (THMX-THMN)*( (Q2-AM2)**2 + AMGM2 )/AMGM
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, DFGTW_IN.
C*    X+ decay.
C*

      SUBROUTINE DFGTW_IN

      IMPLICIT REAL*8 (A-H,O-Z)
C--
      PARAMETER (MXDIM = 50 )
      COMMON /BPARM1/ XL(MXDIM),XU(MXDIM),NDIM,NWILD,IG(MXDIM),NCALL
      COMMON /BPARM2/ ACC1,ACC2,ITMX1,ITMX2
C--
      PARAMETER       ( NEXLIN = 10 )
      COMMON /XCXCCN/  ROOTS, POLE, XPHASE(3,NEXLIN-3),
     .                 IDPTCL(3,NEXLIN), IHLCMB(NEXLIN),
     .                 PVOUT(0:3,NEXLIN), DSGMDX, DPDEBM, BMENGY(0:4),
     .                 ISRBM
      REAL   *4        ROOTS, POLE, XPHASE, PVOUT, DSGMDX, DPDEBM,
     .                 BMENGY
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
C* (Contents)
C*    SQRTS   : (R*4) : sqrt(s).
C*    POLEBM  : (R*4) : electron beam polarization.
C*    SGMEBM  : (R*4) : beam energy spread (fraction).
C*    ISRB    : (I*4) : ISR and BM flag.
C*

      COMMON /USRPRM/ SQRTS, POLEBM, SGMEBM, GAMSW1, ISRB, GAMWINO
      REAL   *4       SQRTS, POLEBM, SGMEBM, GAMSW1, GAMWINO
      INTEGER*4       ISRB
      COMMON /SSCONS/ AM0, AMU, AM2, TANB, AMA, RELAX
      REAL*4          AM0, AMU, AM2, TANB, AMA, RELAX
C* (Contents)
C*    ALFI  : (R*4) : 1/alpha(m_Z).
C*    ALFS  : (R*4) : alpha_s(m_Z).
C*    AMSW  : (R*4) : m_W (GeV).
C*    AMSZ  : (R*4) : m_Z (GeV).
C*    AMSH  : (R*4) : m_H (GeV).
C*    AMST  : (R*4) : m_t (GeV).
C*    AS2W  : (R*4) : sin2tw.
C*
      COMMON /USMPRM/  ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      REAL*4           ALFI, ALFS, AMSW, AMSZ, AMSH, AMST,AS2W
      COMMON /BEMCNS/ SGEBM
      REAL*4          SGEBM
C--
C  For no ISR and no BMEFF.
C--
      PARAMETER   ( MX_NZZ = 50 )
      COMMON /BSHUFL/  NZZ, ISHUFL(MX_NZZ)
      INTEGER*4        NZZ, ISHUFL
      COMMON /BSITPR/  NCAL, ITM1, ITM2
      INTEGER*4        NCAL, ITM1, ITM2
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--

C--
C  Local R*4 variables used in subroutine calls.
C--
      REAL*4        ALF, S2W
C
C========< Entry Point >================================================
C
C--
C  Initialize numerical constants and particle table.
C--
      ALF   = 1/ALFI
      CALL INSMCN(ALF,ALFS,AMSW,AMSZ,AMSH,AMST,AS2W)
      S2W = xSIN2W
      CALL INSSCN(ALF,ALFS,S2W,AMSW,AMSZ,AM0,AMU,AM2,TANB,AMA,RELAX)
C--
C  Set BASES constants.
C--

      NDIM   = 7
      NWILD  = 4
      IOFF   = 0
C>>>
C      NOIG = NWILD + 1
      NOIG = 999
C>>>
C--
      NZZ = NDIM
C--
      NCALL  = NCAL
      ITMX1  = ITM1
      ITMX2  = ITM2
C--
C  Set lower and upper limits.
C--
      DO 100 IX = 1, NDIM
         XL(IX) = 0
         XU(IX) = 1
         IF ( IX.GE.NOIG ) IG(IX) = 0
         ISHUFL(IX) = IX + IOFF
100   CONTINUE
C--
C  Define hists. and plots.
C--
      QMX  = 250
c      CALL XHINIT(31, -1.0D0, 1.0D0, 50,'cos(theta_a+)      ')
c      CALL XHINIT(32,  0.0D0,360.D0, 50,'phi_b+             ')
c      CALL XHINIT(33,  0.0D0,   QMX, 50,'M_ab+              ')
c      CALL XHINIT(34,  0.0D0,   QMX, 50,'M_ac+              ')
c      CALL XHINIT(35,  1.0D0,  9.D0,  8,'Hel. comb.         ')
c      CALL XHINIT(36,  0.0D0,   QMX, 50,'E_e/mu             ')
c      CALL XHINIT(37, -1.0D0, 1.0D0, 50,'cos(theta_l-)      ')
c      CALL XHINIT(38,  1.0D0, 13.D0, 12,'Decay mode( X+ )   ')
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, GMW2FF.
C* (Inputs)
C*    AMW    : (R*4) : W mass.
C*    AMU    : (R*4) : up type fermion mass.
C*    AMD    : (R*4) : down type fermion mass.
C*    VFF    : (R*4) : KM matrix element.
C*    C      : (R*4) : color factor.
C* (Output)
C*    GM     : (R*4) : partial width for W --> fu + fd.
C*

      SUBROUTINE GMW2FF(AMW,AMU,AMD,VFF,C,GM)

      IMPLICIT REAL*4 ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4 AMW, AMU, AMD, VFF, C, GM
C--
C  Statement function.
C--
      PSTR(A,B,C) = SQRT( (A-(B+C))*(A+(B+C))*(A-(B-C))*(A+(B-C)) )/2/A
C
C========< Entry Point >================================================
C
C--
C  Calculate width.
C--
      IF ( AMU+AMD.GE.AMW ) THEN
         GM = 0
         RETURN
      ELSE
         AMU2 = AMU*AMU
         AMD2 = AMD*AMD
         AMW2 = AMW*AMW
         P1P2 = ( AMW2 - AMU2 - AMD2 )/2
         P1   = PSTR(AMW,AMU,AMD)
      ENDIF
      TTA = ( P1P2 + 2*(AMU2+P1P2)*(AMD2+P1P2)/AMW2 )/3
      FAC = (xALF/xSIN2W)/2
      GM  = FAC*TTA*C*VFF*VFF*P1/AMW2
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, INSMCN.
      SUBROUTINE INSMCN(ALF,ALFS,AMSW,AMSZ,AMSH,AMST,AS2W)

      IMPLICIT    REAL*4 ( A-H, O-Z )
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK,ISLB
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SMCUPL/ GAL, GAU, GAD, GWF, GZN, GZL,
     .                GZU, GZD, G1, GW, GWWZ, GWWA
      INTEGER    JSEL,NMDL,NMDQ
      COMMON /HCDEFF/JSEL
      INTEGER    ISTAR
C--
      REAL*4          GAL(2), GAU(2), GAD(2), GWF(2), GZN(2), GZL(2),
     .                GZU(2), GZD(2), G1(2), GW, GWWZ, GWWA
C--
      REAL*4   ALF, ALFS, AMSW, AMSZ, AMSH, AMST, AS2W
      REAL*4   BRT(12)
      DIMENSION IVECL(0:5)
      DIMENSION IVECQ(0:5)
      DATA IVECL/0,1,0,1,0,3/
      DATA IVECQ/0,3,3,0,0,0/
      REAL*4   BRL,BRQ
CSG      EXTERNAL BLKDAT
C
C========< Entry Point >================================================
C
C--
C  Initialize numerical constants.
C--
      xPI    = ACOS(-1.)
      x2PI   = 2*xPI
      x4PI   = 4*xPI
      xHPI   = xPI/2
      xSQRT2 = SQRT(2.0)
      xGV2PB = 3.8937966E8
      xRD2DG = 180/xPI
      xDG2RD = xPI/180
C--
C  Initialize coupling constants.
C--
      xALF   = ALF
CTT 17/Jul/95      xSIN2W = (1-AMSW/AMSZ)*(1+AMSW/AMSZ)
CSG 22/Mar/96      xSIN2W = 0.232
      xSIN2W = AS2W
CSG      write(*,*) '============ modification ==========='
CSG      write(*,*) 'sin2w is ',xSIN2W,
CSG     & ' : not calculated from W & Z masses'
CSG      write(*,*) '====================================='
      xCOS2W = 1 - xSIN2W
      xALFS  = ALFS
C--
C  Initialize particle table.
C--
      AMW        = AMSW
      AMZ        = AMSZ
      AMH        = AMSH
      AMF(3,1,2) = AMST
C--
C  Print out modified parameters.
C--
      WRITE(*,*)' '
      WRITE(*,*)' ****** INSMCN Modified SMCONS and SMPTAB ********'
      WRITE(*,*)'             '
      WRITE(*,*)'    ALF    = ', xALF
      WRITE(*,*)'    ALFS   = ', xALFS
      WRITE(*,*)'    S2W    = ', xSIN2W
      WRITE(*,*)'    AMW    = ', AMW
      WRITE(*,*)'    AMZ    = ', AMZ
      WRITE(*,*)'    AMH    = ', AMH
      WRITE(*,*)'    AMT    = ', AMF(3,1,2)
      WRITE(*,*)' '
C--
C  Initialize W width.
C--
      COLOR  = 3
      QCD    = 1 + xALFS/xPI
      NMD    = 0
      GMWTOT = 0
C--Leptons.

      ISLB = JSEL
      C   = 1
      VFF = 1

C-- 
      if(islb.eq.4) then
       istar=3
       NMD =2
      else
       istar=1
      endif
      DO 70 IG = istar, 3-IVECL(ISLB)
         CALL GMW2FF(AMW,AMF(IG,1,1),AMF(IG,2,1),VFF,C,GM)
         GMWTOT   = GMWTOT + GM
         NMD      = NMD + 1
         BRT(NMD) = GMWTOT
70    CONTINUE
      NMDL=NMD
      write(*,*)'NMDL ISLB GMWTOT', NMDL,ISLB,GMWTOT
      WRITE(*,*)'BRT 1 3',BRT(1),BRT(2),BRT(3)
C--Quarks.
      NMD=3
      C = COLOR*QCD
      DO 80 IGD = 1, 3-IVECQ(ISLB)
         DO 15 IGU = 1, 3-IVECQ(ISLB) 
            VFF = VKM(IGU,IGD,2)
            CALL GMW2FF(AMW,AMF(IGU,1,2),AMF(IGD,2,2),VFF,C,GM)
            GMWTOT   = GMWTOT + GM
            NMD      = NMD + 1
            BRT(NMD) = GMWTOT
 15         CONTINUE
80    CONTINUE
      NMDQ = NMD
      write(*,*)'NMDQ ISLB GMWTOT', NMDQ,ISLB,GMWTOT
      WRITE(*,*)'BRT 3 12',(BRT(I),I=1,12)
C--
C  Set branching fractions.
C--

      DO 25 IMD = 1, NMD
         BRVDK(IMD,1) = BRT(IMD)/GMWTOT
25    CONTINUE
      WRITE(*,*)'BRVDK ',(BRVDK(I,1),I=1,NMD)
C--
      if(islb.eq.5) then
        brl=0.
        brq=BRT(NMDQ)/GMWTOT 
      elseif (islb.lt.3.and.islb.gt.0) then
         brq=0.
         brl=BRT(NMDL)/GMWTOT
      else
         brl=BRT(NMDL)/GMWTOT
         brq=(BRT(NMDQ)-BRT(NMDL))/GMWTOT 
      endif

      WRITE(*,*)' '
      WRITE(*,*)'    GMWTOT = ', GMWTOT
      WRITE(*,*)'    BR(lp) = ', brl
      WRITE(*,*)'    BR(qk) = ', brq
      WRITE(*,*)' '
C--
C  Initialize Z width.
C--
      NMD    = 0
      GMZTOT = 0
C--Leptons.
      C = 1
      DO 30 IT = 1, 2
         T3F = 1.5 - IT
         QF  = 1.0 - IT
         DO 3 IG = 1, 3
            CALL GMZ2FF(AMZ,T3F,QF,C,AMF(IG,IT,1),GM)
            GMZTOT   = GMZTOT + GM
            NMD      = NMD + 1
            BRT(NMD) = GMZTOT
3        CONTINUE
30    CONTINUE
C--Quarks.
      C = COLOR*QCD
      DO 40 IT = 1, 2
         T3F = 1.5   - IT
         QF  = 5./3. - IT
         DO 4 IG = 1, 3
            CALL GMZ2FF(AMZ,T3F,QF,C,AMF(IG,IT,2),GM)
            GMZTOT   = GMZTOT + GM
            NMD      = NMD + 1
            BRT(NMD) = GMZTOT
4        CONTINUE
40    CONTINUE
C--
C  Set branching fractions.
C--
      DO 45 IMD = 1, NMD
         BRVDK(IMD,2) = BRT(IMD)/GMZTOT
45    CONTINUE
C--
      WRITE(*,*)' '
      WRITE(*,*)'    GMZTOT = ', GMZTOT
      WRITE(*,*)'    BR(nu) = ', BRT(3)/GMZTOT
      WRITE(*,*)'    BR(lp) = ', (BRT( 6)-BRT( 3))/GMZTOT
      WRITE(*,*)'    BR(qu) = ', (BRT( 9)-BRT( 6))/GMZTOT
      WRITE(*,*)'    BR(qd) = ', (BRT(12)-BRT( 9))/GMZTOT
      WRITE(*,*)' '
C--
C  Initialize Higgs width.
C--
      AMSTU = AMF(3,2,1)
      AMSC  = AMF(2,1,2)
      AMSB  = AMF(3,2,2)
      CALL GMH2FF(AMSH,AMSTU,AMSZ,GMH2TU)
      CALL GMH2FF(AMSH,AMSC,AMSZ,GMH2C)
      CALL GMH2FF(AMSH,AMSB,AMSZ,GMH2B)
      CALL GMH2FF(AMSH,AMST,AMSZ,GMH2T)
      CALL GMH2VV(AMSH,AMSW,AMSZ,GMH2W)
      CALL GMH2VV(AMSH,AMSZ,AMSZ,GMH2Z)
      C      = COLOR
      GMH2C  = C*GMH2C
      GMH2B  = C*GMH2B
      GMH2T  = C*GMH2T
      GMH2W  = 2*GMH2W
      GMHTOT = GMH2TU + GMH2C + GMH2B + GMH2T + GMH2W + GMH2Z
C--
      WRITE(*,*)' '
      WRITE(*,*)'    GMHTOT = ', GMHTOT
      IF ( GMHTOT.LE.0. ) GMHTOT = 1.E-10
      WRITE(*,*)'    BR(tu) = ', GMH2TU/GMHTOT
      WRITE(*,*)'    BR(c)  = ', GMH2C/GMHTOT
      WRITE(*,*)'    BR(b)  = ', GMH2B/GMHTOT
      WRITE(*,*)'    BR(t)  = ', GMH2T/GMHTOT
      WRITE(*,*)'    BR(W)  = ', GMH2W/GMHTOT
      WRITE(*,*)'    BR(Z)  = ', GMH2Z/GMHTOT
      WRITE(*,*)' '
C--
C  Initialize top width.
C--
      VFF = (VKM(3,3,2))**2
      CALL GMT2QW(AMST,AMSB,AMSW,VFF,GM)
      GMF(3,1,2) = GM
C--
      WRITE(*,*)'        '
      WRITE(*,*)'    GMTTOT = ', GMF(3,1,2)
      WRITE(*,*)'        '
      WRITE(*,*)' *****************************************************'
C--
C  Initialize coupling constants.
C--
      CALL UHCUPL(ALF,xSIN2W, GAL,GAU,GAD,GWF,GZN,GZL,GZU,GZD,G1,
     .            GW,GWWZ,GWWA)
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, SFMMIX.
C----------------------------------------------------------------------C
      SUBROUTINE SFMMIX( ZM, WM, SIN2W,
     &                   HM1, TANB, ATERM,
     &                   SFTMSL, SFTMSR, FRMMAS, TYPFRM,
     &                   SFMAS, PHIF, THTF,
     &                   IRET  )
C----------------------------------------------------------------------C
C=======================================================================
C   Diagonalize mass matrices of sfermions
C   and then calculate some constants such as couplings etc.
C  Ref.   Hikasa-san's note
C
C
C <<< Inputs >>>  R*4 if not specified
C ( standard model constants )
C     1) WM   :    W-boson mass
C     2) SIN2W: weak mixing angle
C ( free parameters for SUSY )
C     1) HM1    :  the higgsino mass term
C     2) TANB   : the ratio of the v.e.v. of two higgs doublets
C       (  =V2/V1 )
C     3) ATERM(complex*8)  :  trilinear term A
C     4) SFTMSL,SFTMSR : soft scaler mass term of Left/Right-handed
C     5) FRMMAS : fermion mass
C     6) TYPFRM(I*4) : fermion type ( 0:neutrino, 1:charged lepton,
C                                  2:up-type quark, 3:down-type quark )
C
C    IPRNT ; print out flag for debug etc.
C
C <<< outputs >>>
C REAL*4  SFMAS(i) mass eigen values (i=1:lighter)
C REAL*4  PHIF     phase to make mass matrix real
C REAL*4  THTF     mixing angle
C     IRET < 0 : error
C     IRET > 0 : OK
C=============
C 17/Jul/95 modified to include ZM as an input argument
C=======================================================================

      PARAMETER ( PI = 3.1415927 )
      INTEGER*4 TYPFRM
      REAL*4    SFMAS(2), SFMAS2(2)
      COMPLEX*8 ATERM, AM2LR

C-----------------------------------------------------------------------
C check input (TYPFRM) & set T3L,charge
      IF( TYPFRM.EQ.0 ) THEN
C        neutrino
        T3L = 0.5
        QCHRG = 0.
      ELSEIF( TYPFRM.EQ.1 ) THEN
C        charged lepton
        T3L = -0.5
        QCHRG = -1.
      ELSEIF( TYPFRM.EQ.2 ) THEN
C        up-type quark
        T3L = 0.5
        QCHRG = 2./3.
      ELSEIF( TYPFRM.EQ.3 ) THEN
C        down-type quark
        T3L = -0.5
        QCHRG = -1./3.
      ELSE
C       WRITE(6,*) ' Error(SFMMIX):TYPFRM=',TYPFRM
        IRET = -1
        RETURN
      ENDIF

      COS2W = 1.-SIN2W
      TAN2W = SIN2W/COS2W
      SINW  = SQRT(SIN2W)
      COSW  = SQRT(COS2W)
      TANW  = SQRT(TAN2W)
CTT 17/Jul/95      ZM    = WM/COSW
      WM2   = WM**2
      ZM2   = ZM**2

C  0 < beta < pi
      COSB  = SQRT( 1./(TANB**2+1.) )
      IF( TANB .LT. 0 ) COSB = -COSB
      SINB  = SQRT((1.-COSB)*(1.+COSB))
      COSDB = (COSB-SINB)*(COSB+SINB)
      SINDB = 2.*SINB*COSB
      SIN2B = SINB**2
      COS2B = COSB**2

C off-diagonal element
      IF( MOD(TYPFRM,2).EQ.0 ) THEN
        AM2LR = -FRMMAS*( CONJG(ATERM) +HM1/TANB )
      ELSE
        AM2LR = -FRMMAS*( CONJG(ATERM) +HM1*TANB )
      ENDIF

      SFTML2 = SFTMSL**2
      SFTMR2 = SFTMSR**2
      FRMMS2 = FRMMAS**2

      SM2LR = ABS(AM2LR)**2
      ASUM1 = SFTML2 + FRMMS2
      SM2FL = ASUM1 + ZM2*COSDB*( T3L-QCHRG*SIN2W)
      SM2FR = ASUM1 + ZM2*COSDB*QCHRG*SIN2W

      CM2LR = REAL(AM2LR)
      SM2LR = IMAG(AM2LR)
      PHIF  = ATAN2( SM2LR, CM2LR )

      ASUM1  = SFTML2 +SFTMR2
      ASQRT  = SQRT( (SFTML2-SFTMR2)**2 + 4*SM2LR**2 )
      SFMAS2(1) = ( ASUM1 - ASQRT )/2.
      SFMAS2(2) = ( ASUM1 + ASQRT )/2.
      DO 10 I=1,2
10    SFMAS(I) = SQRT(SFMAS(I))

      TANTHF = SM2LR/(SFMAS2(1) - SFTMR2 )
      THTF   = ATAN2( TANTHF, 1. )

      IRET = 1

      RETURN
      END
CDECK  ID>, UHQIJ3.
CC**********************************************************************
C*
C*=========================================-----------------===
C* Subroutine UHQIJ3(AM,JPV,AMR,EPSL,QPR,Z,IPV,QIJ2,QIK2,WAT)
C*=========================================-----------------===
C*
C* (Purpose)
C*    Makes an appropriate choice of (ij) and (ik) for Dalitz's
C*    variables of 3-body phase space and return the generated
C*    values together with the Jacobian.
C* (Inputs)
C*       AM(i)          : (R*4) : mass of i
C*       JPV(1,r_1,r_2) : (I*4) : i
C*          (2,r_1,r_2) : (I*4) : j   r_1 <--> (ij)
C*          (3,r_1,r_2) : (I*4) : k   r_2 <--> (ik)
C*       AMR(1,r)       : (R*4) : mass of resonance r
C*          (2,r)       : (R*4) : width of resonance r
C*          (3,r)       : (R*4) : m_(ij) + m_k  for r <--> (ij)
C*       EPSL           : (R*8) : width of gray area near poles
C*       QPR            : (R*8) : parent mass.
C*       Z(1-2)         : (R*8) : integration variables in (0,1)
C* (Output)
C*       IPV(1)   : (I*4) : i
C*          (2)   : (I*4) : j
C*          (3)   : (I*4) : k
C*       QIJ2     : (R*8) : (Q_ij)^2.
C*       QIK2     : (R*8) : (Q_ik)^2.
C*       WAT      : (R*8) : Jacobian weight.
C* (Relation)
C*    Invokes USORTR in FORTLIB and UHQLIM and UHQ2BW in ttz_lib.
C*
CC**********************************************************************

      SUBROUTINE UHQIJ3(AM,JPV,AMR,EPSL,QPR,Z,IPV,QIJ2,QIK2,WAT)

      IMPLICIT   REAL*8 ( A-H, O-Z )
C--
C  Dummy arguments.
C--
      INTEGER*4  JPV(3,3,3), IPV(3)
      REAL   *4  AM(*), AMR(3,3)
      REAL   *8  EPSL, QPR, Z(2), QIJ2, QIK2, WAT
C--
C  Local variables.
C--
      INTEGER*4  ISHF(3)
C--
      DATA NCALL /  0 /
C
C========< Entry Point >================================================
C
C--
C  Reset event weight.
C--
      WAT = 1
C--
C  Sort possible resonances.
C     AMR(1,r') = AM_ij
C        (2,r') = GM_ij
C        (3,r') = AM_ij + AM_k
C  where r = 1, 2, 3 correspond to 3 combinations of
C  daughter particles (i,j,k). Notice that i, j, and k point
C  to AM and PV and not necessarily congiguous.
C     ISHF(r) = r' for the r-th resonance from the lightest.
C--
      CALL USORTR(3,3,3,AMR(1,1),ISHF(1))
C--
C  Check if there are more than 2 poles.
C--
      IF ( AMR(3,ISHF(3)).GT.QPR+EPSL ) THEN
C--
C  -----------------
C  Less than 3 poles
C  -----------------
C  Choose the pairs corresponding to the lightest two as
C  independent variables.
C     JPV(1,r'1,r'2) = i
C        (2,r'1,r'2) = j
C        (3,r'1,r'2) = k
C  where r'1 is a (ij) resonance and r'2 is a (ik) resonance.
C--
         IPV(1) = JPV(1,ISHF(1),ISHF(2))
         IPV(2) = JPV(2,ISHF(1),ISHF(2))
         IPV(3) = JPV(3,ISHF(1),ISHF(2))
C--
         AM_A = AM(IPV(1))
         AM_B = AM(IPV(2))
         AM_C = AM(IPV(3))
C-- ab.
         QIJMN = AM_A + AM_B
         QIJMX = QPR - AM_C
         CALL UHQ2BW(DBLE(AMR(1,ISHF(1))),DBLE(AMR(2,ISHF(1))),
     .                                QIJMN,QIJMX,Z(1),QIJ2,WT)
         QIJ   = SQRT(QIJ2)
         WAT   = WAT*WT
C-- ac.
         CALL UHQLIM(QPR,AM_A,AM_B,AM_C,QIJ,QIKMN,QIKMX)
         CALL UHQ2BW(DBLE(AMR(1,ISHF(2))),DBLE(AMR(2,ISHF(2))),
     .                                QIKMN,QIKMX,Z(2),QIK2,WT)
         WAT   = WAT*WT
C--
      ELSE
C--
C  -------
C  3 poles
C  -------
C  Always choose the pair giving the lightest (bc) as an in dependent
C  variables, while the other should be selected from (ab) or (ac),
C  depending on the region in the phase space.
C--
         XR  = AMR(1,ISHF(2)) - EPSL
         YR  = AMR(1,ISHF(3)) - EPSL
C--
         IA  = JPV(1,ISHF(2),ISHF(3))
         IB  = JPV(2,ISHF(2),ISHF(3))
         IC  = JPV(3,ISHF(2),ISHF(3))
C--
         AM_A = AM(IA)
         AM_B = AM(IB)
         AM_C = AM(IC)
C--
C  Do not allow the double resonance of the heaviest two.
C  This is a restriction to the parameter space for simplicity of
C  the program (and is due to my laziness).
C--
         CALL UHQLIM(QPR,AM_A,AM_B,AM_C,XR,YRMN,YRMX)
         CALL UHQLIM(QPR,AM_A,AM_C,AM_B,YR,XRMN,XRMX)
C--
         IF ( YR.LE.YRMX .OR. XR.LE.XRMX ) THEN
            PRINT *, '>>>> GETXPH: CAUTION '
            PRINT *, '  Kinematics inappropriate for this set of ',
     .                 'parameters.'
            PRINT *, '     QPR = ', QPR
            PRINT *, '     AM  = ', AM_A, AM_B, AM_C
            PRINT *, '     AMR = ', AMR(1,ISHF(1)),
     .                              AMR(1,ISHF(2)),
     .                              AMR(1,ISHF(3))
            PRINT *, '  Ignore this point.'
            PRINT *, '  When you got this message too often, ',
     .                 'the result might be wrong.'
            WAT = 0
            RETURN
         ENDIF
C--
C  The point accepted.
C--
         XMN   = AM_A + AM_B
         XMX   = QPR - AM_C
         XCT   = (XR+XRMX)/2
C--
C  Branch on kinematical regions.
C--
         IF ( XMN+Z(1)*(XMX-XMN).LT.XCT ) THEN
C--
C  Region I: choose (ac) and (bc) as independent.
C--
            Z1  = Z(1)*(XMX-XMN)/(XCT-XMN)
            WAT = WAT*(XMX-XMN)/(XCT-XMN)
C--
            IPV(1) = IC
            IPV(2) = IA
            IPV(3) = IB
C--
            YMN = AM_A + AM_C
            YMX = QPR - AM_B
            CALL UHQLIM(QPR,AM_A,AM_C,AM_B,YMN,XCMN,XCMX)
            XC  = XCMN
            YC  = YMN
C-- ac.
            IF ( XC.LE.XCT ) THEN
               QIJMN = YMN
            ELSE
               CALL UHQLIM(QPR,AM_A,AM_B,AM_C,XCT,YCTMN,YCTMX)
               QIJMN = YCTMN
            ENDIF
            QIJMX = YMX
            CALL UHQ2BW(DBLE(AMR(1,ISHF(3))),DBLE(AMR(2,ISHF(3))),
     .                                     QIJMN,QIJMX,Z1,QIJ2,WT)
            QIJ   = SQRT(QIJ2)
            WAT   = WAT*WT
C-- bc.
            CALL UHQLIM(QPR,AM_C,AM_A,AM_B,QIJ,QIKMN,QIKMX)
            IF ( QIJ.GT.YCTMN .AND. QIJ.LT.YCTMX ) THEN
               QIKMN = AM_A**2 + AM_B**2 + AM_C**2
     .                + (QPR-XCT)*(QPR+XCT) - QIJ**2
               QIKMN = SQRT(MAX(QIKMN,0.D0))
            ENDIF
            CALL UHQ2BW(DBLE(AMR(1,ISHF(1))),DBLE(AMR(2,ISHF(1))),
     .                                   QIKMN,QIKMX,Z(2),QIK2,WT)
            WAT   = WAT*WT
C--
         ELSE
C--
C  Region II: choose (ab) and (bc) as independent.
C--
            Z1  = ((XMX-XMN)*Z(1)-(XCT-XMN))/(XMX-XCT)
            WAT = WAT*(XMX-XMN)/(XMX-XCT)
C--
            IPV(1) = IB
            IPV(2) = IA
            IPV(3) = IC
C-- ab.
            QIJMN  = XCT
            QIJMX  = XMX
            CALL UHQ2BW(DBLE(AMR(1,ISHF(2))),DBLE(AMR(2,ISHF(2))),
     .                                     QIJMN,QIJMX,Z1,QIJ2,WT)
            QIJ   = SQRT(QIJ2)
            WAT   = WAT*WT
C-- bc.
            CALL UHQLIM(QPR,AM_B,AM_A,AM_C,QIJ,QIKMN,QIKMX)
            CALL UHQ2BW(DBLE(AMR(1,ISHF(1))),DBLE(AMR(2,ISHF(1))),
     .                                   QIKMN,QIKMX,Z(2),QIK2,WT)
            WAT   = WAT*WT
         ENDIF
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
CDECK  ID>, FILLHI.
      SUBROUTINE FILLHI
      IMPLICIT REAL*4 ( A-H, O-Z )
      common/histv/var
      real*4     VAR(31)
C*
C*==================
C* Common /SMPTAB/
C*==================
C*
C*    This common contains parton properties such as masses and
C*    widths of elementary fermions and gauge bosons.
C*    Is possible to modify these costants by the datacards IPMC
C*    and RPMC in the usual L3 framework.
C*
C*
      COMMON /DTYP/   EVTYP
      COMMON /SMPTAB/ AMF(3,2,2), GMF(3,2,2), AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT, MDVDK(3,12,2),
     .                BRVDK(0:12,2), VKM(3,3,2)
      INTEGER*4       MDVDK
      REAL   *4       AMF, GMF, AMW, AMZ, AMH,
     .                GMZTOT, GMWTOT, GMHTOT,
     .                BRVDK, VKM
      COMMON /SSPTAB/ SWM, SZM, SFM, SHM, GMSW, GMSZ, GMSF, GMSH
      REAL*4          SWM(2), SZM(4), SFM(7), SHM(4),
     .                GMSW(2), GMSZ(4), GMSF(7), GMSH(4)
      PARAMETER       ( NEXLIN = 10 )
      COMMON /XCXCCN/  ROOTS, POLE, XPHASE(3,NEXLIN-3),
     .                 IDPTCL(3,NEXLIN), IHLCMB(NEXLIN),
     .                 PVOUT(0:3,NEXLIN), DSGMDX, DPDEBM, BMENGY(0:4),
     .                 ISRBM
      REAL   *4        ROOTS, POLE, XPHASE, PVOUT, DSGMDX, DPDEBM,
     .                 BMENGY
      INTEGER*4        IDPTCL, IHLCMB, ISRBM
      COMMON /SMCONS/ xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5
C--
      REAL*4          xPI, x2PI, x4PI, xHPI, xSQRT2, xRD2DG, xDG2RD,
     .                xSIN2W, xCOS2W, xALF, xALFS, xGV2PB, xLAM5


      CALL HFILL(  1, VAR(1),0.,1. )
      CALL HFILL(  2, VAR(2),0.,1. )
      CALL HFILL(  3, VAR(3)     ,0.,1. )
      CALL HFILL(  4, VAR(4),0.,1. )
      CALL HFILL(  5, VAR(5)     ,0.,1. )
      CALL HFILL(  6, VAR(6),0.,1. )
      CALL HFILL(  7, VAR(7),0.,1. )
      CALL HFILL(  8, VAR(8)  ,0.,1. )
      CALL HFILL(  9, VAR(9),0.,1. )
      CALL HFILL( 10, VAR(10),0.,1. )
      CALL HFILL( 11, VAR(11)  ,0.,1. )
      CALL HFILL( 12, VAR(12),0.,1. )
      CALL HFILL( 13, VAR(13),0.,1. )
      IF ( IDPTCL(3,5).EQ.1 .AND. IDPTCL(1,5).LE.2 ) THEN
         CALL HFILL( 14,VAR(14)    ,0.,1. )
         CALL HFILL( 15,VAR(15)      ,0.,1. )
         CALL HFILL( 27,VAR(16),0.,1. )
      ENDIF
      IF ( IDPTCL(3,8).EQ.1 .AND. IDPTCL(1,8).LE.2 ) THEN
         CALL HFILL( 14, VAR(17)    ,0.,1. )
         CALL HFILL( 15, VAR(18),0.,1. )
         CALL HFILL( 28, VAR(19),0.,1. )
      ENDIF
      IF ( IDPTCL(3,5).EQ.2 ) THEN
         CALL HFILL( 16, VAR(20)    ,0.,1. )
         CALL HFILL( 17, VAR(21)    ,0.,1. )
      ENDIF
      IF ( IDPTCL(3,8).EQ.2 ) THEN
         CALL HFILL( 16, VAR(22)    ,0.,1. )
         CALL HFILL( 17, VAR(23),0.,1. )
      ENDIF
      CALL HFILL( 18, VAR(24) ,0.,1. )
      CALL HFILL( 19, VAR(25) ,0.,1. )
      CALL HFILL( 20, VAR(26) ,0.,1. )
      CALL HFILL( 21, VAR(27) ,0.,1. )
      CALL HFILL( 23, VAR(28) ,0.,1. )
      CALL HFILL( 24, VAR(29) ,0.,1. )
      CALL HFILL( 25, VAR(30) ,0.,1. )
      CALL HFILL( 26, VAR(31) ,0.,1. )
C--
C  That's it.
C--
      RETURN
      END