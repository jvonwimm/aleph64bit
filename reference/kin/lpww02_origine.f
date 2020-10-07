C======================================================================
C
C
C                         ----------------
C                         |     LEPWW    |
C                         ----------------
C
C
C
C A Monte Carlo Generator for
C         e+ e-  --->  f1 f2  f3 f4      (four fermions)
C via W+W- and/or ZZ  bosons.
C
C
C based on a program written by Ronald Kleiss.
C
C includes:
C   * ISR collinear with the beams,
C   * effective couplings based on GF,
C   * Coulomb singularity,
C   * mass-dependent width in the boson propagator
C   * alpha_S correction to hadronic partial widths
C   * FSR for prompt electrons and muons using PHOTOS
C   * parton showers and fragmentation using JETSET 7.3
C   * interface to KINGAL
C
C Ramon MIQUEL (CERN)  and  Michael SCHMITT (WISCONSIN)
C======================================================================
      SUBROUTINE LEPWWD
C=======================================================================
C Set default values for run-time options.
C Zero counters and arrays.
C-----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................

C
C logical unit number for messages
C
      LUOUTP = 6
C
C set default values
C
      XMZ = 91.190D0
      XMW = 80.250D0
      XMT = 175.D0
      ALFA = 1.D0/137.035989D0
      ALFAS = 0.123D0
      GF = 1.16639D-5
      SIN2W = 0.2317D0
      ECM = 176.D0
      WWUSER = 2.088D0
      ZWUSER = 2.507D0
      IRFLAG = 1
      CSFLAG = 1
      BWFLAG = 1
      ASFLAG = 1
      HIFLAG = 1
      FRFLAG = 1
      IZFLAG = 1
      ILFLAG = 1
      UWFLAG = 0
C
      SDVRT(1) = 0.0185
      SDVRT(2) = 0.0008
      SDVRT(3) = 1.02
C
C initialize counters
C
      NLEPWW = 0
      NUPDAT = 0
      W0 = 0.D0
      W1 = 0.D0
      W2 = 0.D0
      WW0SUM = 0.0D0
      WW1SUM = 0.0D0
      WW2SUM = 0.0D0
      ZZ0SUM = 0.0D0
      ZZ1SUM = 0.0D0
      ZZ2SUM = 0.0D0
      CALL VZERO(BRSTZ,12)
      CALL VZERO(BRSTW, 9)
      NWW = 0
      NZZ = 0
      WLARGE = -1.D0
C
      RETURN
      END
      SUBROUTINE LPWWIN
C=======================================================================
C Initialize the generator
C-----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
CC      INCLUDE 'LUN7COM.INC'
C............................................................
C LUND common block
C
      INTEGER JN7LU,K7LU,LJNPAR
      REAL    P7LU,V7LU
      PARAMETER( LJNPAR= 4000)
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     *                V7LU(LJNPAR,5)
C
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C............................................................
C
      DOUBLE PRECISION SCM,SMWID
      INTEGER I,IJS,IJSCOD,IKLCOD,LUCOMP
      INTEGER KKT,KKB,KKW,KKZ
      PARAMETER(KKT=6,KKB=5,KKW=24,KKZ=23)
      REAL    ULMASS,MASSUM
      EXTERNAL IJSCOD,IKLCOD,ULMASS,LUCOMP
C
C banner
C
      WRITE(LUOUTP,10)
C
C report constants
C
      WRITE(LUOUTP,20) ECM,XMZ,XMW,XMT,1.D0/ALFA,ALFAS,GF*1.D5,SIN2W
C
C Calculate the on-shell production cross sections for W+ W- and Z0 Z0
C
      SCM = ECM*ECM
      IF (ECM.GT.2.D0*XMW) THEN
        CALL BORNOX(SCM,XMZ,XMW,SIN2W,SIGWW,SIGZZ)
        IF (SIGWW.LE.1.0D-16) THEN
          WRITE(LUOUTP,22) SIGWW
          STOP
        ENDIF
      ENDIF
C
C Calculate the couplings and widths
C
      CALL KOPPEL
C
C User values for W and Z width, if desired.
C
      IF (UWFLAG.LT.1) THEN
        WRITE(6,31) SNGL(XMG(3)/XMW), SNGL(XMG(2)/XMZ)
      ELSE
        WRITE(6,31) SNGL(XMG(3)/XMW), SNGL(XMG(2)/XMZ)
        SMWID = XMG(3)/XMW
        XMG(3) = XMG(3)*WWUSER/SMWID
        XMG(4) = XMG(3)
        SMWID = XMG(2)/XMZ
        XMG(2) = XMG(2)*ZWUSER/SMWID
        WRITE(6,32) SNGL(XMG(3)/XMW), SNGL(XMG(2)/XMZ)
      ENDIF
C
C Initialize BK, which define ingoing and outgoing fermions
C
      DO 2 I=1,2
    2 BK(I)=-1
      DO 3 I=3,6
    3 BK(I)=+1
C
C update W, Z, and top masses in /LUDAT2/
C
      IF ((XM2(2).LE.0.D0).OR.(XM2(3).LE.0.D0).OR.(XM2(4).LE.0.D0).OR.
     *    (XMG(2).LE.0.D0).OR.(XMG(3).LE.0.D0).OR.(XMG(4).LE.0.D0))THEN
        WRITE(LUOUTP,27) XM2,XMG
        STOP
      ENDIF
      PMAS(LUCOMP(KKZ),1) = SNGL(XMZ)
      PMAS(LUCOMP(KKW),1) = SNGL(XMW)
      PMAS(LUCOMP(KKT),1) = SNGL(XMT)
      PMAS(LUCOMP(KKZ),2) = SNGL(XMG(2)/XMZ)
      PMAS(LUCOMP(KKW),2) = SNGL(XMG(3)/XMW)
C
C set mass values in FLMASS array
C FLMASS is used in the Kleiss code for taking (tiny) mass effects
C into account.  ULMASS is a function in JETSET.
C
      WRITE(LUOUTP,930)
      DO 9 I = 1, 12
        IJS = IJSCOD(I)
        FLMASS(I) = ULMASS(IJS)
        WRITE(LUOUTP,935) I,IJS,IKLCOD(IJS),FLMASS(I)
    9 CONTINUE
C
C check whether decays to top are allowed
C
      MASSUM = PMAS(LUCOMP(KKT),1) + PMAS(LUCOMP(KKB),1)
      NOTOPQ = (MASSUM .GT. PMAS(LUCOMP(KKW),1))
      IF (NOTOPQ) THEN
        WRITE(LUOUTP,48)
      ELSE
        WRITE(LUOUTP,49)
      ENDIF
C
C simulate initial-state radiation?
C
      IF (IRFLAG.EQ.0) THEN
        WRITE(LUOUTP,41)
      ELSE
        WRITE(LUOUTP,42)
      ENDIF
C
C simulate final-state radiation?
C
      IF (FRFLAG.EQ.0) THEN
        WRITE(LUOUTP,51)
      ELSE
        WRITE(LUOUTP,52)
      ENDIF
C
C correction for the coulomb singularity?
C
      IF (CSFLAG.EQ.0) THEN
        WRITE(LUOUTP,45)
      ELSE
        WRITE(LUOUTP,46)
      ENDIF
C
C correction for a Breit-Wigner with mass-dependent width?
C
      IF (BWFLAG.EQ.0) THEN
        WRITE(LUOUTP,65)
      ELSE
        WRITE(LUOUTP,66)
      ENDIF
C
C Make alpha_strong correction for boson width?
C
      IF (ASFLAG.EQ.0) THEN
        WRITE(LUOUTP,67) 66.7*SNGL(ALFAS)/3.1415926
      ELSE
        WRITE(LUOUTP,68) 66.7*SNGL(ALFAS)/3.1415926
      ENDIF
C
C Include ZZ diagrams?
C
      IF (IZFLAG.EQ.0) THEN
        WRITE(LUOUTP,71)
      ELSE
        WRITE(LUOUTP,72)
      ENDIF
C
C Run LUND?
C
      IF (ILFLAG.EQ.0) THEN
        WRITE(LUOUTP,73)
      ELSE
        WRITE(LUOUTP,74)
      ENDIF
C
C Histograms?
C
      IF (HIFLAG.EQ.0) THEN
        WRITE(LUOUTP,69)
      ELSE
        WRITE(LUOUTP,70)
      ENDIF
C
C set arbitrary parameters ALF(1) and ALF(2)
C These control the sampling of WW diagrams and ZZ diagrams
C
      ALF(1) = 1.0D0
      ALF(2) = 0.1D0
C
C estimate maximum weight for generator, and cross section.
C
      CALL WZCROS
C
C initialize PHOTOS for final-state radiation
C
      IF (FRFLAG.GT.0) CALL PHOINI
C
C Book diagnostic histograms
C
      IF (HIFLAG.NE.0) CALL WWHISI
C
      RETURN
   10 FORMAT(///,
     * 10X,'===================================================',/,
     * 10X,'= =                                             = =',/,
     * 10X,'= =              L E P W W                      = =',/,
     * 10X,'= =                                             = =',/,
     * 10X,'===================================================',//)
   20 FORMAT(//,' IMPORTANT CONSTANTS: ',/,
     * 10X,'ECM (center of mass energy)                : ',F12.1,/,
     * 10X,'XMZ (Z mass)                               : ',F12.5,/,
     * 10X,'XMW (W mass)                               : ',F12.5,/,
     * 10X,'XMT (top quark mass)                       : ',F12.5,/,
     * 10X,'1/ALFA  (alpha_QED)                        : ',F12.5,/,
     * 10X,'ALFAS (alpha_Strong)                       : ',F12.5,/,
     * 10X,'GF (Fermi constant x 10**-5)               : ',F12.8,/,
     * 10X,'SIN2W (Sin-squared ELW mixing angle)       : ',F12.6,/)
   22 FORMAT(//,' =LPWWIN= Cross section is zero ! ',4X,D12.4,/
     * 10X,'(Check that ECM is above threshold.)',/)
   27 FORMAT(/,' =LPWWIN= ERROR !!!',4X,
     * 'The mass and/or widths are zero!',/,
     * 10X,'array XM2: ',5F8.3,/,10X,'array XMG: ',5F8.3,/)
   31 FORMAT(/,' =LPWWIN= SM values for the total widths:',/,
     * 20X,'Gamma_W =',F8.3,6X,'Gamma_Z =',F8.3,'  GeV')
   32 FORMAT(10X,'Replace these by values supplied by the USER:',/,
     * 20X,'Gamma_W =',F8.3,6X,'Gamma_Z =',F8.3,'  GeV')
   41 FORMAT(/,' =LPWWIN= do NOT simulate initial-state radiation.')
   42 FORMAT(/,' =LPWWIN= simulate initial-state radiation.')
   45 FORMAT(/,' =LPWWIN= do NOT correct for the Coulomb Singularity.')
   46 FORMAT(/,' =LPWWIN= correct for the Coulomb Singularity.')
   48 FORMAT(/,' =LPWWIN= Z will not decay to top quarks, and',/,
     * 10X,'W will not decay to bottom (or top) quarks.')
   49 FORMAT(/,' =LPWWIN= W and Z will decay to top quarks.')
   51 FORMAT(/,' =LPWWIN= do NOT simulate final-state radiation.')
   52 FORMAT(/,' =LPWWIN= simulate final-state radiation using PHOTOS.')
   65 FORMAT(/,' =LPWWIN= Breit-Wigner generated using ',
     * 'a constant width.')
   66 FORMAT(/,' =LPWWIN= Breit-Wigner generated using ',
     * 'a mass-dependent width.')
   67 FORMAT(/,' =LPWWIN= Do NOT apply alpha_S correction for ',
     * 'boson widths.',/,
     * 10X,'(Would be about a ',F6.1,' % change.)')
   68 FORMAT(/,' =LPWWIN= Apply alpha_S correction for ',
     * 'boson widths.',/,
     * 10X,'(Will be about a ',F6.1,' % change.)')
   69 FORMAT(/,' =LPWWIN= Do NOT book diagnostic histograms.')
   70 FORMAT(/,' =LPWWIN= Book diagnostic histograms.')
   71 FORMAT(/,' =LPWWIN= Contributions from ZZ diagrams turned OFF.')
   72 FORMAT(/,' =LPWWIN= Include contributions from ZZ diagrams.')
   73 FORMAT(/,' =LPWWIN= Do not call LUND routines --> no hadrons')
   74 FORMAT(/,' =LPWWIN= Call LUND routines and produce hadrons.')
  930 FORMAT(/,' =LPWWIN= Summary of Fermion Species',/,
     * ' number    Jetset code      Kleiss code      mass (GeV)')
  935 FORMAT(1X,I4,9X,I4,14X,I4,11X,F10.6)
      END
      SUBROUTINE LEPWWG
C=======================================================================
C Obtain one unweighted event using the Rejection algorithm.
C Call PHOTOS for FSR from prompt electrons and muons.
C Fill /LUJETS/ and call LUND for showering and fragmentation.
C-----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
CC      INCLUDE 'LUN7COM.INC'
C............................................................
C LUND common block
C
      INTEGER JN7LU,K7LU,LJNPAR
      REAL    P7LU,V7LU
      PARAMETER( LJNPAR= 4000)
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     *                V7LU(LJNPAR,5)
C
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C............................................................
CC      INCLUDE 'HEPEVT.INC'
C............................................................
C Standard HEPEVT common block
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     * JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C............................................................
CC      INCLUDE 'PHOQED.INC'
C
C special interface for marking particles not allowed to radiate
C
      LOGICAL QEDRAD
      COMMON /PHOQED/ QEDRAD(NMXHEP)
C............................................................
      DOUBLE PRECISION PW1(4),PW2(4),QW1(4),QW2(4),QW3(4),QW4(4)
      DOUBLE PRECISION PZ1(4),PZ2(4),QZ1(4),QZ2(4),QZ3(4),QZ4(4)
      DOUBLE PRECISION QF1(5),QF2(5),QF3(5),QF4(5),QB1(5),QB2(5)
      DOUBLE PRECISION XM,W,WWW,ZZW,SCM,RTEST,SPR,F1,F2
      DOUBLE PRECISION QBOOST(4)
      INTEGER I1,I2,J1,J2
      LOGICAL ZZEVNT
      INTEGER I,J,NTRMAX
      PARAMETER(NTRMAX= 10000) ! maximum number of trials for gen event
      REAL    RNDM,RX1,RX2,RX3
      INTEGER IJSINV
      INTEGER IJOIN1(2),IJOIN2(2)
      INTEGER KKZ,KKW
      PARAMETER(KKZ=23,KKW=24)
      REAL    MA1,MA2
      INTEGER KF1,KF2,KFF1,KFF2,KFF3,KFF4
      REAL    ULMASS
      INTEGER IMOTH,N,IPB1,IPB2
      EXTERNAL RNDM,IJSINV,ULMASS,XM
C
      SCM = ECM*ECM
C
C Keep track of what has been generated so far.
C
      NLEPWW = NLEPWW + 1
      IF ((NLEPWW.LT.50).OR.(MOD(NLEPWW,1000).EQ.0)) THEN
        WRITE(LUOUTP,601) NLEPWW
      ENDIF
C
C======================= GENERATE ONE EVENT =============================
C
C  choose a primary vertex
C
      CALL RANNOR(RX1,RX2)
      CALL RANNOR(RX2,RX3)
      VERTEX(1) = RX1*SDVRT(1)
      VERTEX(2) = RX2*SDVRT(2)
      VERTEX(3) = RX3*SDVRT(3)
      VERTEX(4) = 0.0
C
C generate beam energies
C
      NTRIAL = 0
    5 CONTINUE
C
      IF (IRFLAG.GT.0) THEN
        CALL EBLOSS(SCM,XISR1,XISR2,F1,F2)
      ELSE
        XISR1 = 1.0D0
        XISR2 = 1.0D0
        F1 = 1.0D0
        F2 = 1.0D0
      ENDIF
      SPR = SCM*XISR1*XISR2
C
C choose flavors for final-state fermions (s/r CHOOSE)
C set up flavor and interference flags (s/r FLAVS)
C
    7 CONTINUE
      CALL CHOOSE(I1,J1,I2,J2)
      CALL FLAVS(I1,J1,I2,J2)
C
C calculate the exact cross section at a phase space point
C
      W = 0.0D0
      WWW = 0.0D0
      ZZW = 0.0D0
      IF (WWPOSS) THEN
        ZINTFR = ZZPOSS
        CALL WWS(SPR,WWW,PW1,PW2,QW1,QW2,QW3,QW4)
        WWW = WWW*F1*F2
        W = WWW
        IROUTN = 1
      ELSEIF (ZZPOSS) THEN
        ZINTFR = (I1.EQ.I2)
        CALL ZZS(SPR,ZZW,PZ1,PZ2,QZ1,QZ2,QZ3,QZ4)
        ZZW = ZZW*F1*F2
        W = ZZW
        IROUTN = 2
      ENDIF
      WW0SUM = WW0SUM + 1.0D0
      WW1SUM = WW1SUM + WWW
      WW2SUM = WW2SUM + WWW**2
      ZZ0SUM = ZZ0SUM + 1.0D0
      ZZ1SUM = ZZ1SUM + ZZW
      ZZ2SUM = ZZ2SUM + ZZW**2
      W0 = W0 + 1.0D0
      W1 = W1 + W
      W2 = W2 + W**2
      IF (HIFLAG.GT.0) THEN
        CALL HF1(IDOF+3,SNGL(WWW),1.)
        CALL HF1(IDOF+4,SNGL(ZZW),1.)
        CALL HF1(IDOF+5,SNGL(W),1.)
      ENDIF
C
C keep track of largest weight encountered
C
      IF (W.GT.WLARGE) WLARGE = W
C
C check whether current weight is greater than expected maximum
C
      IF (W.GT.WMAX) THEN
        NUPDAT = NUPDAT + 1
        WRITE(LUOUTP,9100) W,WMAX
        WRITE(LUOUTP,9110) WWPOSS,ZZPOSS,WINTFR,ZINTFR
        IF (WWPOSS) THEN
          WRITE(LUOUTP,9140) '1',I1,QW1
          WRITE(LUOUTP,9140) '2',J1,QW2
          WRITE(LUOUTP,9140) '3',I2,QW3
          WRITE(LUOUTP,9140) '4',J2,QW4
          WRITE(LUOUTP,9142) XM(QW1,QW2),XM(QW3,QW4)
        ELSE
          WRITE(LUOUTP,9141) '1',I1,QZ1
          WRITE(LUOUTP,9141) '2',J1,QZ2
          WRITE(LUOUTP,9141) '3',I2,QZ3
          WRITE(LUOUTP,9141) '4',J2,QZ4
          WRITE(LUOUTP,9142) XM(QZ1,QZ2),XM(QZ3,QZ4)
        ENDIF
        WMAX = 1.05D0 * W
      ENDIF
      NTRIAL = NTRIAL + 1
      IF (NTRIAL.GT.NTRMAX) THEN
        WRITE(LUOUTP,9000) NTRIAL,WMAX
        STOP
      ENDIF
C
C rejection method test
C
      RX1 = RNDM(NTRIAL)
      RTEST = WMAX * DBLE(RX1)
      IF (W.LT.RTEST) GOTO 5
C
C update arrays for branching ratio statistics
C
      ZZEVNT = (.NOT.WWPOSS)
      IF (ZZEVNT) THEN
        NZZ = NZZ + 1
        I = IJSINV(IFL1(1))
        BRSTZ(I) = BRSTZ(I)+1
        I = IJSINV(IFL2(1))
        BRSTZ(I) = BRSTZ(I)+1
      ELSE
        NWW = NWW + 1
        I = (IJSINV(IFL1(1))+1)/2
        IF (I.LE.3) THEN
          BRSTW(I) = BRSTW(I)+1
        ELSE
          J = (IJSINV(IFL1(2))+1)/2
          IF (J.EQ.I) THEN
            BRSTW(I) = BRSTW(I)+1
          ELSE
            BRSTW(I+3) = BRSTW(I+3)+1
          ENDIF
        ENDIF
        I = (IJSINV(IFL2(2))+1)/2
        IF (I.LE.3) THEN
          BRSTW(I) = BRSTW(I)+1
        ELSE
          J = (IJSINV(IFL2(1))+1)/2
          IF (J.EQ.I) THEN
            BRSTW(I) = BRSTW(I)+1
          ELSE
            BRSTW(I+3) = BRSTW(I+3)+1
          ENDIF
        ENDIF
      ENDIF
C======================================================================
C Prepare interface to LUND, etc.
C Put four vectors in more convenient arrays
C
C - fermion flavors correspond to ZZ but not to WW
      IF (ZZEVNT) THEN
        KF1 = 23
        KF2 = 23
        DO I = 1, 4
          QF1(I) = QZ1(I)
          QF2(I) = QZ2(I)
          QF3(I) = QZ3(I)
          QF4(I) = QZ4(I)
          QB1(I) = QF1(I) + QF2(I)
          QB2(I) = QF3(I) + QF4(I)
        ENDDO
        QF1(5) = REALP(5,1)
        QF2(5) = REALP(5,2)
        QF3(5) = REALP(5,3)
        QF4(5) = REALP(5,4)
        KFF1 = IFL1(1)
        KFF2 = IFL1(2)
        KFF3 = IFL2(1)
        KFF4 = IFL2(2)
      ELSE
C - fermion flavors correspond to WW only (IPEAK=0), or for
C   both WW and ZZ (IPEAK=1), and
C   the invariant masses peak for two W's.
        IF (IPEAK.LE.1) THEN
          KF1 =  24
          KF2 = -24
          DO I = 1, 4
            QF1(I) = QW1(I)
            QF2(I) = QW2(I)
            QF3(I) = QW3(I)
            QF4(I) = QW4(I)
            QB1(I) = QF1(I) + QF2(I)
            QB2(I) = QF3(I) + QF4(I)
          ENDDO
          QF1(5) = REALP(5,1)
          QF2(5) = REALP(5,2)
          QF3(5) = REALP(5,3)
          QF4(5) = REALP(5,4)
          KFF1 = IFL1(1)
          KFF2 = IFL1(2)
          KFF3 = IFL2(1)
          KFF4 = IFL2(2)
        ELSE
C   the invariant masses peak for two Z's.
          KF1 =  23
          KF2 =  23
          DO I = 1, 4
            QF1(I) = QW1(I)
            QF2(I) = QW4(I)
            QF3(I) = QW3(I)
            QF4(I) = QW2(I)
            QB1(I) = QF1(I) + QF2(I)
            QB2(I) = QF3(I) + QF4(I)
          ENDDO
          QF1(5) = REALP(5,1)
          QF2(5) = REALP(5,4)
          QF3(5) = REALP(5,3)
          QF4(5) = REALP(5,2)
          KFF1 = IFL1(1)
          KFF2 = IFL2(2)
          KFF3 = IFL2(1)
          KFF4 = IFL1(2)
        ENDIF
      ENDIF
      QB1(5) = QB1(4)**2-QB1(1)**2-QB1(2)**2-QB1(3)**2
      QB1(5) = DSQRT(QB1(5))
      QB2(5) = QB2(4)**2-QB2(1)**2-QB2(2)**2-QB2(3)**2
      QB2(5) = DSQRT(QB2(5))
C
C Lorentz boost back to lab frame (needed if ISR was generated)
C
      IF ((XISR1.NE.1.0D0).OR.(XISR2.NE.1.0D0)) THEN
        QBOOST(1) = 0.0D0
        QBOOST(2) = 0.0D0
        QBOOST(3) = (XISR1-XISR2)*0.5D0*ECM
        QBOOST(4) = (XISR1+XISR2)*0.5D0*ECM
        CALL BOOST(QBOOST,QB1)
        CALL BOOST(QBOOST,QB2)
        CALL BOOST(QBOOST,QF1)
        CALL BOOST(QBOOST,QF2)
        CALL BOOST(QBOOST,QF3)
        CALL BOOST(QBOOST,QF4)
      ENDIF
C
C====================== INTERFACE TO PHOTOS =============================
C
C First, copy particles into /HEPEVT/ common block.
C
      NHEP = 0
C
C first boson, with two daughters
C
      NHEP = NHEP + 1
      IMOTH = NHEP
      ISTHEP(NHEP) = 2
      IDHEP(NHEP) = KF1
      JMOHEP(1,NHEP) = 0
      JMOHEP(2,NHEP) = 0
      JDAHEP(1,NHEP) = NHEP+1
      JDAHEP(2,NHEP) = NHEP+2
      DO I = 1, 5
        PHEP(I,NHEP) = SNGL(QB1(I))
      ENDDO
      CALL UCOPY(VERTEX,VHEP(1,NHEP),4)
C
      NHEP = NHEP + 1
      ISTHEP(NHEP) = 1
      IDHEP(NHEP) = KFF1
      JMOHEP(1,NHEP) = IMOTH
      JMOHEP(2,NHEP) = 0
      JDAHEP(1,NHEP) = 0
      JDAHEP(2,NHEP) = 0
      DO I = 1, 5
        PHEP(I,NHEP) = SNGL(QF1(I))
      ENDDO
      CALL UCOPY(VERTEX,VHEP(1,NHEP),4)
C
      NHEP = NHEP + 1
      ISTHEP(NHEP) = 1
      IDHEP(NHEP) = KFF2
      JMOHEP(1,NHEP) = IMOTH
      JMOHEP(2,NHEP) = 0
      JDAHEP(1,NHEP) = 0
      JDAHEP(2,NHEP) = 0
      DO I = 1, 5
        PHEP(I,NHEP) = SNGL(QF2(I))
      ENDDO
      CALL UCOPY(VERTEX,VHEP(1,NHEP),4)
C
C second boson, with two daughters
C
      NHEP = NHEP + 1
      IMOTH = NHEP
      ISTHEP(NHEP) = 2
      IDHEP(NHEP) = KF2
      JMOHEP(1,NHEP) = 0
      JMOHEP(2,NHEP) = 0
      JDAHEP(1,NHEP) = NHEP+1
      JDAHEP(2,NHEP) = NHEP+2
      DO I = 1, 5
        PHEP(I,NHEP) = SNGL(QB2(I))
      ENDDO
      CALL UCOPY(VERTEX,VHEP(1,NHEP),4)
C
      NHEP = NHEP + 1
      ISTHEP(NHEP) = 1
      IDHEP(NHEP) = KFF3
      JMOHEP(1,NHEP) = IMOTH
      JMOHEP(2,NHEP) = 0
      JDAHEP(1,NHEP) = 0
      JDAHEP(2,NHEP) = 0
      DO I = 1, 5
        PHEP(I,NHEP) = SNGL(QF3(I))
      ENDDO
      CALL UCOPY(VERTEX,VHEP(1,NHEP),4)
C
      NHEP = NHEP + 1
      ISTHEP(NHEP) = 1
      IDHEP(NHEP) = KFF4
      JMOHEP(1,NHEP) = IMOTH
      JMOHEP(2,NHEP) = 0
      JDAHEP(1,NHEP) = 0
      JDAHEP(2,NHEP) = 0
      DO I = 1, 5
        PHEP(I,NHEP) = SNGL(QF4(I))
      ENDDO
      CALL UCOPY(VERTEX,VHEP(1,NHEP),4)
C
C Mask off everything except e and mu
C
      DO I = 1, NHEP
        KF1 = IABS(IDHEP(I))
        QEDRAD(I) = ((KF1.EQ.11).OR.(KF1.EQ.13))
      ENDDO
C
C generate FSR
C
      IF (FRFLAG.GT.0) CALL PHOTOS(1)
C
C===================== INTERFACE TO LUND ================================
C
C translate from /HEPEVT/ to /LUJETS/ common blocks
C
      CALL LUHEPC(2)
      IF (ILFLAG.LE.0) GOTO 850
C
C find bosons and corresponding fermion pairs
C
      N = 0
      DO I = 1, NHEP
        KF1 = IABS(IDHEP(I))
        IF ((KF1.EQ.KKZ).OR.(KF1.EQ.KKW)) THEN
          N = N + 1
          IF (N.EQ.1) IPB1 = I
          IF (N.EQ.2) IPB2 = I
        ENDIF
      ENDDO
      IJOIN1(1) = K7LU(IPB1,4)
      IJOIN1(2) = IJOIN1(1)+1
      IJOIN2(1) = K7LU(IPB2,4)
      IJOIN2(2) = IJOIN2(1)+1
C
C fragment the event
C
C first, disable particle decays.
C
      MSTJ(21) = 0
C
C Mask off the second pair of fermions, by offsetting
C the JETSET status code by 10.  Then, if the first pair are
C quarks, then join then as a color string.  Call LUEXEC to
C perform the parton shower, and fragmentation.
C
      K7LU(IJOIN2(1),1) = K7LU(IJOIN2(1),1)+10
      K7LU(IJOIN2(2),1) = K7LU(IJOIN2(2),1)+10
      IF ((IABS(IFL1(1)).LE.6).AND.(IABS(IFL1(2)).LE.6)) THEN
        CALL LUJOIN(2,IJOIN1)
        MA1 = SNGL(BOS1(5))
        CALL LUSHOW(IJOIN1(1),IJOIN1(2),MA1)
      ENDIF
      CALL LUEXEC
C
C Now, return the second pair of fermions to their original status.
C If they are quarks, then join them.  Call LUEXEC to perform the
C parton shower and fragmentation.
C
      K7LU(IJOIN2(1),1) = K7LU(IJOIN2(1),1)-10
      K7LU(IJOIN2(2),1) = K7LU(IJOIN2(2),1)-10
      IF ((IABS(IFL2(1)).LE.6).AND.(IABS(IFL2(2)).LE.6)) THEN
        CALL LUJOIN(2,IJOIN2)
        MA2 = SNGL(BOS2(5))
        CALL LUSHOW(IJOIN2(1),IJOIN2(2),MA2)
      ENDIF
      CALL LUEXEC
C
C Finally, re-enable particle decays, and let LUND decay them.
C
      MSTJ(21) = 2
      CALL LUEXEC
C
C LUEDIT(14) will remove decayed particles, BUT it also will
C destroy the history information linking the fermions to the
C parent bosons
C
C NO NO NO !     CALL LUEDIT(14)
C
C debug some events
C
  850 CONTINUE
      IF (NLEPWW.LE.10) CALL LULIST(1)
      IF (MSTU(24).GT.0) THEN
        WRITE(LUOUTP,9002) NLEPWW
        CALL LULIST(1)
      ENDIF
C
C fill some histograms
C
      IF (HIFLAG.NE.0) CALL WWHISA(QF1,QF2,QF3,QF4,QB1,QB2)
C
      RETURN
  601 FORMAT(' === LEPWWG === start event ',I8)
 9000 FORMAT(' =LEPWWG= too many attempts to generate event:',I12,/,
     * 10X,'maximum weight used : ',E12.5)
 9002 FORMAT(/,' =LEPWWG= Problem in LUDECY at event #',I10,/)
 9100 FORMAT(/,' =LEPWWG= event weight is larger than maximum:',/,
     * 10X,'current weight:',E12.4,8X,'current maximum:',E12.4,/,
     * 10X,'==> update the maximum!',/)
 9110 FORMAT(10X,'WWPOSS= ',L1,4X,'ZZPOSS= ',L1,4X,
     * 'WINTFR= ',L1,4X,'ZINTFR= ',L1)
 9140 FORMAT(10X,'W fermion ',A1,', flavor:',I3,4X,'momentum:',4E12.5)
 9141 FORMAT(10X,'Z fermion ',A1,', flavor:',I3,4X,'momentum:',4E12.5)
 9142 FORMAT(10X,'fermion-pair masses (12):',E12.5,4X,'(34):',E12.5)
      END
      SUBROUTINE LPWWF
C======================================================================
C
C Print statistics for LEPWW generator.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
C
      REAL    ZZFRAC
      INTEGER I
      REAL    BR,BRSUM,BRSIG
      CHARACTER*12 NAMDKW(9),NAMDKZ(12)
      DATA NAMDKW / 'W -> e   nu ' ,
     *              'W -> mu  nu ' ,
     *              'W -> tau nu ' ,
     *              'W -> u   d  ' ,
     *              'W -> c   s  ' ,
     *              'W -> t   b  ' ,
     *              'W -> u   s  ' ,
     *              'W -> c   d  ' ,
     *              'W -> ?   ?  '  /
      DATA NAMDKZ / 'Z -> e   e  ' ,
     *              'Z -> nue nue' ,
     *              'Z -> mu  mu ' ,
     *              'Z -> num num' ,
     *              'Z -> tau tau' ,
     *              'Z -> nut nut' ,
     *              'Z -> d   d  ' ,
     *              'Z -> u   u  ' ,
     *              'Z -> s   s  ' ,
     *              'Z -> c   c  ' ,
     *              'Z -> b   b  ' ,
     *              'Z -> t   t  ' /
C
C integrated cross sections
C
      WRITE(LUOUTP,900) WW0SUM,ZZ0SUM
      IF (WW0SUM.GT.1.0D0) THEN
        WW1SUM = WW1SUM / WW0SUM
        WW2SUM = WW2SUM / WW0SUM
        WW2SUM = DSQRT(WW2SUM-WW1SUM*WW1SUM)/DSQRT(WW0SUM)
      ENDIF
      IF (ZZ0SUM.GT.1.0D0) THEN
        ZZ1SUM = ZZ1SUM / ZZ0SUM
        ZZ2SUM = ZZ2SUM / ZZ0SUM
        ZZ2SUM = DSQRT(ZZ2SUM-ZZ1SUM*ZZ1SUM)/DSQRT(ZZ0SUM)
      ENDIF
      IF (W0.GT.1.0D0) THEN
        W1 = W1 / W0
        W2 = W2 / W0
        W2 = DSQRT(W2-W1*W1)/DSQRT(W0)
      ENDIF
      WRITE(LUOUTP,910) WW1SUM,WW2SUM,ZZ1SUM,ZZ2SUM,W1,W2
      WRITE(LUOUTP,911) WMAX,NUPDAT,WLARGE
C
C efficiency
C
      WRITE(LUOUTP,912) 100.*FLOAT(NWW+NZZ)/SNGL(W0)
C
C how many were there ?
C
      WRITE(LUOUTP,914) NWW,NZZ
      IF (NWW+NZZ.GT.0) THEN
        ZZFRAC = FLOAT(NZZ)/FLOAT(NWW+NZZ)
        WRITE(LUOUTP,915) ZZFRAC
      ENDIF
C
C calculate effective branching ratios
C
      WRITE(LUOUTP,950)
      WRITE(LUOUTP,951)
      BRSUM = 0.0
      DO 209 I = 1, 9
        BRSUM = BRSUM + FLOAT(BRSTW(I))
  209 CONTINUE
      DO 210 I = 1, 9
        IF (BRSUM.GT.1.0) THEN
          BR = FLOAT(BRSTW(I))/BRSUM
          BRSIG = SQRT(BR*(1.-BR)/BRSUM)
        ELSE
          BR = 0.0
          BRSIG = 0.0
        ENDIF
        WRITE(LUOUTP,955) NAMDKW(I),BR,BRSIG,BRSTW(I)
  210 CONTINUE
C
      WRITE(LUOUTP,951)
      BRSUM = 0.0
      DO 219 I = 1, 12
        BRSUM = BRSUM + FLOAT(BRSTZ(I))
  219 CONTINUE
      DO 220 I = 1, 12
        IF (BRSUM.GT.0.0D0) THEN
          BR = FLOAT(BRSTZ(I))/BRSUM
          BRSIG = SQRT(BR*(1.-BR)/BRSUM)
        ELSE
          BR = 0.0
          BRSIG = 0.0
        ENDIF
        WRITE(LUOUTP,955) NAMDKZ(I),BR,BRSIG,BRSTZ(I)
  220 CONTINUE
      WRITE(LUOUTP,951)
C
C PHOTOS summary
C
      IF (FRFLAG.GT.0) CALL PHOREP
C
C Finish diagnostic histograms
C
      IF (HIFLAG.NE.0) CALL WWHISF
C
      RETURN
  900 FORMAT(///,' ===================== LEPWW SUMMARY ',20('='),//,
     * 10X,'number of WW trials :',F12.0,/,
     * 10X,'number of ZZ trials :',F12.0)
  910 FORMAT(/,10X,'cross section for WW  events:',F10.4,
     * ' +/-',F10.4,' pb',/,
     * 10X,'cross section for ZZ  events:',F10.4,
     * ' +/-',F10.4,' pb',/,
     * 10X,'cross section for all events:',F10.4,
     * ' +/-',F10.4,' pb',//,
     * 10X,'(Note: "ZZ" refers to final states which cannot involve',
     * ' two W bosons.)')
  911 FORMAT(/,10X,'maximum weight: ',F10.3,/,
     * 10X,'number of updates of max weight: ',I4,/,
     * 10X,'largest weight encountered: ',F10.3,/)
  912 FORMAT(10X,'efficiency : ',F8.3,' %',/)
  914 FORMAT(/,10X,'number of "WW" events : ',I6,/,
     * 10X,'number of "ZZ" events : ',I6)
  915 FORMAT(10X,'fraction which were ZZ :',F8.4)
  950 FORMAT(//,10X,'effective branching ratios:')
  951 FORMAT(2X,64('-'))
  955 FORMAT(6X,A12,3X,F8.4,' +-',F8.4,3X,'N decays:',I6)
      END
      SUBROUTINE RNDM8(R,DUMMY)
C----------------------------------------------------------------------
C return eight random numbers, double precision
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION R(8)
      INTEGER I,DUMMY
      REAL    RX1,RNDM
      EXTERNAL RNDM
      DO 10 I = 1, 8
        RX1  = RNDM(I)
        R(I) = DBLE(RX1)
   10 CONTINUE
      DUMMY = I
      RETURN
      END
      SUBROUTINE EBLOSS(SCM,X1,X2,F1,F2)
C======================================================================
C
C Generate a pair of beam energies after ISR (in units of the
C nominal beam energy).  input: SCM = nominal cm-energy-squared.
C
C Method based on Monte Carlo simulation of nu-nubar events written
C by Ramon MIQUEL.  An approximation to the exact structure function
C is generated, and a correction factor giving the ratio of the
C exact to the approximate functions is calculated.  The factor is
C used later in the rejection test based on the cross section.
C (called from subroutine ASKUSE and WZCROS.)
C
C M.Schmitt & R.Miquel 23-July-1993.
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      REAL*8 SCM,ME,ME2,BETA,PI,BETA0,R,RMIN,X1,X2,F1,F2,DERATI
      PARAMETER( PI= 3.14159265359D0)
      PARAMETER( ME= 0.51099906D-3, ME2=ME**2)
      PARAMETER( RMIN=  0.001D0) ! avoid numerical errors
      REAL    RNDM
      EXTERNAL RNDM,DERATI
      SAVE BETA0
      DATA BETA0 /0.D0/
C
      X1 = 1.0D0
      X2 = 1.0D0
      F1 = 1.0D0
      F2 = 1.0D0
      IF (BETA0.EQ.0.0D0) BETA0 = (2.0D0*ALFA/PI)
      IF (SCM.LE.ME2) THEN
        WRITE(LUOUTP,100) SCM
        F1 = 0.0D0
        F2 = 0.0D0
      ENDIF
      BETA = BETA0*(DLOG(SCM/ME2)-1.0D0)
C
C first X value
C
      R = DBLE(RNDM(SCM))
      IF (R.GT.RMIN) THEN
        X1 = 1.0D0 - R**(2.0D0/BETA)
      ELSE
        X1 = 1.0D0
      ENDIF
      F1 = DERATI(SCM,X1)
C
C second X value
C
      R = DBLE(RNDM(SCM))
      IF (R.GT.RMIN) THEN
        X2 = 1.0D0 - R**(2.0D0/BETA)
      ELSE
        X2 = 1.0D0
      ENDIF
      F2 = DERATI(SCM,X2)
C
      IF (X1.GT.1.0D0) X1 = 1.0D0
      IF (X2.GT.1.0D0) X2 = 1.0D0
C
  100 FORMAT(/,' =EBLOSS= SCM=',F8.3,'  ==> reject.',/)
      END
      FUNCTION DERATI(S,X)
C======================================================================
C This function evaluates the ratio of the exact structure function
C for the radiating initial-state electron to the approximate
C expression   0.5*BETA*(1-X)**(BETA/2-1).
C This comes from Ramon MIQUEL.
C
C inputs:  S= cm-energy-squared
C          X= energy of electron after radiation, in units of Ebeam
C  DELP is Delta-prime, given in eq. 2.272 of his thesis.
C  The true structure function is given in eq. 2.273.
C The maximum value of this ratio is about 1.05 (for Ecm=176).
C called from EBLOSS.
C M.Schmitt 23-Jul-1993,  taken from Ramon MIQUEL's code.
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      REAL*8 DERATI,S,X,Z2,Z3,ME,ME2,L,DELP,BETA,PI,BETA0
      PARAMETER( Z2= 1.6449340668D0)
      PARAMETER( Z3= 1.202056903159D0)
      PARAMETER( PI= 3.14159265359D0)
      PARAMETER( ME= 0.51099906D-3, ME2=ME**2)
      INTEGER INIT
      SAVE INIT,DELP,BETA0
      DATA INIT/0/
C
      DERATI = 1.0D0
      IF (S.LT.ME2) RETURN
C
      IF (INIT.EQ.0) THEN
        INIT = 1
        L= DLOG(S/ME2)
        BETA0 = (2.0D0*ALFA/PI)
        BETA = BETA0*(L-1.0D0)
        DELP= 1.D0+BETA**2*PI**2/24.D0+ALFA/PI
     .       *(1.5D0*L+PI**2/3.D0-2.D0)
     .       +ALFA**2.D0/PI**2.D0*((9.D0/8.D0-2.D0*Z2)*L**2.D0
     .       +(-45.D0/16.D0+
     .       11.D0/2.D0*Z2+3.D0*Z3)*L-6.D0/5.D0*Z2**2.D0-9.D0/2.D0*Z3-
     .       6.D0*Z2*DLOG(2.D0)+3.D0/8.D0*Z2+57.D0/12.D0)
        DELP= DSQRT(DELP)
      ENDIF
C
      IF (S.LT.ME2) RETURN
      BETA = BETA0*(DLOG(S/ME2)-1.0D0)
      IF (X.EQ.1.D0) THEN
        DERATI = DELP
      ELSE
        DERATI = DELP - 0.5D0*(1.D0+X)*(1.D0-X)**(1.D0-BETA/2.D0)
     .     +BETA/16.D0*
     .     ((X+1.D0)*(-4.D0*DLOG(1.D0-X)+3.D0*DLOG(X))-4.D0/(1.D0-X)
     .     *DLOG(X)-5.D0-X)*(1.D0-X)**(1.D0-BETA/2.D0)
      ENDIF
C
      END
      SUBROUTINE BOOST(QM,QP)
C-----------------------------------------------------------------------
C Trivial interface to s/r BOOST0
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION QM(4),QI(4),QP(4)
      INTEGER I
      DO I = 1, 4
        QI(I) = QP(I)
      ENDDO
      CALL BOOST0(QM,QI,QP)
      END
      SUBROUTINE BOOST0(QM,QI,QO)
C................................................
C  GENERAL DOUBLE PRECISION BOOSTING ROUTINE
C
C  QM = TOTAL 4-VECTOR OF SYSTEM "SP"
C                      IN SYSTEM "S"
C  QI = INPUT : 4-VECTOR IN SYSTEM SP
C  QO = OUTPUT: 4-VECTOR IN SYSTEM S
C................................................
      IMPLICIT NONE
      DOUBLE PRECISION QM(4),QI(4),QO(4)
      DOUBLE PRECISION EM,PM2,PM,M,EI,PIP,EO,POP,C
      INTEGER I
C..
      EM = QM(4)
      PM2 = QM(1)**2+QM(2)**2+QM(3)**2
      IF(PM2.NE.0.D0) GO TO 15
        DO 7 I=1,4
7       QO(I) = QI(I)
        RETURN
15    PM = DSQRT(PM2)
      M = DSQRT(EM*EM-PM2)
      IF(M.NE.0.D0) GO TO 16
        WRITE(6,100)
100     FORMAT('  BOOST AT BETA=1 REQUIRED !. STOP AT SUBROUTINE BOOST')
        STOP
C..
16    EI = QI(4)
      PIP = 0.D0
      DO 10 I=1,3
10    PIP = PIP + QI(I)*QM(I)/PM
C..
      EO  = ( EM*EI + PM*PIP )/M
      POP = ( PM*EI + EM*PIP )/M
C..
      QO(4) = EO
      C = (POP-PIP)/PM
      DO 20 I=1,3
20    QO(I) = C*QM(I) + QI(I)
      RETURN
      END
      SUBROUTINE CHOOSE(I1,J1,I2,J2)
C======================================================================
C Choose a set of four fermion flavors, consistent with a pair
C of intermediate W or Z bosons.
C The approximate branching ratios for W and Z decay to fermions
C are used to optimize the generation of unweighted events; an
C overall weight which takes this into account is stored in PIJ.
C called by s/r ASKUSE.
C
C Note: Decays to top quarks are excluded by construction.
C
C input:  none
C output: I1,J1 : pair of fermions for first W or Z
C         I2,J2 : pair of fermions for second W or Z
C         PIJ : weight for this selection (passed in common)
C
C R.Miquel Dec-93.
C M.Schmitt & R.Miquel 26-Apr-94.
C M.Schmitt  1-Feb-95 Cabibbo mixing
C M.Schmitt 14-Feb-95 implement IZFLAG
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LUN7COM.INC'
C............................................................
C LUND common block
C
      INTEGER JN7LU,K7LU,LJNPAR
      REAL    P7LU,V7LU
      PARAMETER( LJNPAR= 4000)
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     *                V7LU(LJNPAR,5)
C
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C............................................................
      INTEGER I1,I2,J1,J2,D_ISO,ISO_LO
      DOUBLE PRECISION GWLEP,GWQUA,GWSUM
      DOUBLE PRECISION GZCHL,GZNUL,GZUPQ,GZDNQ,GZSUM
      DOUBLE PRECISION WE,WMU,WTAU,WUD,WCS,WTB
      DOUBLE PRECISION ZE,ZNUE,ZMU,ZNUM,ZTAU,ZNUT,ZU,ZD,ZS,ZC,ZB,ZT
      INTEGER IE,INUE,IMU,INUM,ITAU,INUT,IU,ID,IS,IC,IB,IT
      DOUBLE PRECISION PW,PZ,B1,B2,R,RW1,RW2,RZ1,RZ2
      REAL    STCAB2,RMIX,SUM
      REAL     RNDM
      EXTERNAL RNDM
C codes for fermion flavors
      PARAMETER( IE   = 1, INUE = 2)
      PARAMETER( IMU  = 3, INUM = 4)
      PARAMETER( ITAU = 5, INUT = 6)
      PARAMETER( ID   = 7, IU   = 8)
      PARAMETER( IS   = 9, IC   =10)
      PARAMETER( IB   =11, IT   =12)
C build up a priori probs for W decays
      PARAMETER( GWLEP = 1.0D0/9.0D0,  GWQUA = 3.0D0*GWLEP)
      PARAMETER( GWSUM = 3.0D0*GWLEP + 2.0D0*GWQUA)
      PARAMETER( WE    = 0.0D0 + GWLEP/GWSUM)
      PARAMETER( WMU   = WE    + GWLEP/GWSUM)
      PARAMETER( WTAU  = WMU   + GWLEP/GWSUM)
      PARAMETER( WUD   = WTAU  + GWQUA/GWSUM)
      PARAMETER( WCS   = WUD   + GWQUA/GWSUM)
      PARAMETER( WTB   = WCS   + 0.0D0/GWSUM)
C build up a priori probs for Z decays
      PARAMETER( GZCHL = 0.03357D0,    GZNUL = 0.06691D0)
      PARAMETER( GZDNQ = 0.15413D0,    GZUPQ = 0.11947D0)
      PARAMETER( GZSUM = 3.0D0*GZCHL + 3.0D0*GZNUL +
     *                   3.0D0*GZDNQ + 2.0D0*GZUPQ    )
      PARAMETER( ZE    = 0.0D0 + GZCHL/GZSUM)
      PARAMETER( ZNUE  = ZE    + GZNUL/GZSUM)
      PARAMETER( ZMU   = ZNUE  + GZCHL/GZSUM)
      PARAMETER( ZNUM  = ZMU   + GZNUL/GZSUM)
      PARAMETER( ZTAU  = ZNUM  + GZCHL/GZSUM)
      PARAMETER( ZNUT  = ZTAU  + GZNUL/GZSUM)
      PARAMETER( ZD    = ZNUT  + GZDNQ/GZSUM)
      PARAMETER( ZU    = ZD    + GZUPQ/GZSUM)
      PARAMETER( ZS    = ZU    + GZDNQ/GZSUM)
      PARAMETER( ZC    = ZS    + GZUPQ/GZSUM)
      PARAMETER( ZB    = ZC    + GZDNQ/GZSUM)
      PARAMETER( ZT    = ZB    + 0.0D0/GZSUM)
C
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      LOGICAL FIRST
      SAVE FIRST
      SAVE STCAB2
      DATA FIRST /.TRUE./
C
C Choose whether to generate four fermions based on Z decay
C probabilities or on W decay probabilities.  This choice is made
C based on the relative sizes of the "WW" and "ZZ" cross sections.
C (This does not exclude the chance of WW/ZZ interference.)
C The Z decay possibilities are defeated if IZFLAG = 0.
C
      IF (IZFLAG.GT.0) THEN
        IF ((SIGWW+SIGZZ).GT.0.D0) THEN
          PZ = MAX( SIGZZ/(SIGWW+SIGZZ), 0.01D0 )
        ELSE
          PZ = 0.01D0
        ENDIF
        PW = 1.D0-PZ
        R = DBLE(RNDM(SIGWW))
      ELSE
        R = 0.D0
        PW = 1.D0
        PZ = 0.D0
      ENDIF
      IF (FIRST) THEN
        WRITE(6,110) SIGWW,SIGZZ,PZ,PW
        WRITE(6,120) GWLEP,GWQUA,GWSUM
        WRITE(6,130) GZCHL,GZNUL,GZDNQ,GZUPQ,GZSUM
        WRITE(6,140) WE,WMU,WTAU,WUD,WCS,WTB
        WRITE(6,150) ZE,ZNUE,ZMU,ZNUM,ZTAU,ZNUT,ZD,ZU,ZS,ZC,ZB,ZT
      ENDIF
C
C calculate cabibbo-mixing probability  (VCKM comes from LUND commons)
C
      IF (FIRST) THEN
        SUM = VCKM(1,1) + VCKM(2,2) + VCKM(1,2) + VCKM(2,1)
        IF (SUM.GT.0.0) THEN
          STCAB2 = (VCKM(1,2)+VCKM(2,1))/SUM
          WRITE(6,43) STCAB2
        ELSE
          STCAB2 = 0.
          WRITE(6,44) STCAB2
        ENDIF
        FIRST = .FALSE.
      ENDIF
C-----------------------------------------------------------------------
C use W decay probabilities
C
      IF (R.LT.PW) THEN
        RW1 = DBLE(RNDM(PW))
        IF     (RW1.LT.WE)   THEN
             I1 = IE
             B1 = WE
        ELSEIF (RW1.LT.WMU)  THEN
             I1 = IMU
             B1 = WMU-WE
        ELSEIF (RW1.LT.WTAU)  THEN
             I1 = ITAU
             B1 = WTAU-WMU
        ELSEIF (RW1.LT.WUD)  THEN
             I1 = ID
             B1 = WUD-WTAU
        ELSEIF (RW1.LT.WCS)  THEN
             I1 = IS
             B1 = WCS-WUD
        ELSE
             I1 = IB
             B1 = WTB-WCS
        ENDIF
        RW2 = DBLE(RNDM(PW))
        IF     (RW2.LT.WE)   THEN
             I2 = IE
             B2 = WE
        ELSEIF (RW2.LT.WMU)  THEN
             I2 = IMU
             B2 = WMU-WE
        ELSEIF (RW2.LT.WTAU)  THEN
             I2 = ITAU
             B2 = WTAU-WMU
        ELSEIF (RW2.LT.WUD)  THEN
             I2 = ID
             B2 = WUD-WTAU
        ELSEIF (RW2.LT.WCS)  THEN
             I2 = IS
             B2 = WCS-WUD
        ELSE
             I2 = IB
             B2 = WTB-WCS
        ENDIF
        J1 = I1+1
        J2 = I2+1
C
C simulate cabibbo-mixing
C   We randomly chande d into s and s into d according to STCAB2.
C   Since for this routine WUD=WCS, we do not have to modify the
C   weights; we only have to be careful to exclude ZZ contrbution
C   if only one W has mixed.
C
C d->s for W boson 1
        IF (I1.EQ.ID) THEN
          RMIX = RNDM(I1)
          IF (RMIX.LT.STCAB2) I1 = IS
C s->d for W boson 1
        ELSEIF (I1.EQ.IS) THEN
          RMIX = RNDM(I1)
          IF (RMIX.LT.STCAB2) I1 = ID
        ENDIF
C d->s for W boson 2
        IF (I2.EQ.ID) THEN
          RMIX = RNDM(I2)
          IF (RMIX.LT.STCAB2) I2 = IS
C s->d for W boson 2
        ELSEIF (I2.EQ.IS) THEN
          RMIX = RNDM(I2)
          IF (RMIX.LT.STCAB2) I2 = ID
        ENDIF
C
C calculate weight for these specific flavors,
C taking into account the possibility of WW/ZZ interference
C
        IF ((I1.NE.I2).OR.(J1.NE.J2)) THEN
          PIJ = PW*B1*B2
        ELSE
          IF (I1.LT.ID) THEN
            PIJ = PW*B1*B2 + 2.D0*PZ*GZCHL*GZNUL/GZSUM**2
          ELSE
            PIJ = PW*B1*B2 + 2.D0*PZ*GZDNQ*GZUPQ/GZSUM**2
          ENDIF
        ENDIF
C-----------------------------------------------------------------------
C use Z decay probabilities
C
      ELSE
        RZ1 = DBLE(RNDM(PZ))
        IF     (RZ1.LT.ZE)   THEN
             I1 = IE
             B1 = ZE
        ELSEIF (RZ1.LT.ZNUE)  THEN
             I1 = INUE
             B1 = ZNUE-ZE
        ELSEIF (RZ1.LT.ZMU)  THEN
             I1 = IMU
             B1 = ZMU-ZNUE
        ELSEIF (RZ1.LT.ZNUM)  THEN
             I1 = INUM
             B1 = ZNUM-ZMU
        ELSEIF (RZ1.LT.ZTAU)  THEN
             I1 = ITAU
             B1 = ZTAU-ZNUM
        ELSEIF (RZ1.LT.ZNUT)  THEN
             I1 = INUT
             B1 = ZNUT-ZTAU
        ELSEIF (RZ1.LT.ZD)  THEN
             I1 = ID
             B1 = ZD-ZNUT
        ELSEIF (RZ1.LT.ZU)  THEN
             I1 = IU
             B1 = ZU-ZD
        ELSEIF (RZ1.LT.ZS)  THEN
             I1 = IS
             B1 = ZS-ZU
        ELSEIF (RZ1.LT.ZC)  THEN
             I1 = IC
             B1 = ZC-ZS
        ELSEIF (RZ1.LT.ZB)  THEN
             I1 = IB
             B1 = ZB-ZC
        ELSEIF (RZ1.LT.ZT)  THEN
             I1 = IT
             B1 = ZT-ZB
        ENDIF
        RZ2 = DBLE(RNDM(PZ))
        IF     (RZ2.LT.ZE)   THEN
             I2 = IE
             B2 = ZE
        ELSEIF (RZ2.LT.ZNUE)  THEN
             I2 = INUE
             B2 = ZNUE-ZE
        ELSEIF (RZ2.LT.ZMU)  THEN
             I2 = IMU
             B2 = ZMU-ZNUE
        ELSEIF (RZ2.LT.ZNUM)  THEN
             I2 = INUM
             B2 = ZNUM-ZMU
        ELSEIF (RZ2.LT.ZTAU)  THEN
             I2 = ITAU
             B2 = ZTAU-ZNUM
        ELSEIF (RZ2.LT.ZNUT)  THEN
             I2 = INUT
             B2 = ZNUT-ZTAU
        ELSEIF (RZ2.LT.ZD)  THEN
             I2 = ID
             B2 = ZD-ZNUT
        ELSEIF (RZ2.LT.ZU)  THEN
             I2 = IU
             B2 = ZU-ZD
        ELSEIF (RZ2.LT.ZS)  THEN
             I2 = IS
             B2 = ZS-ZU
        ELSEIF (RZ2.LT.ZC)  THEN
             I2 = IC
             B2 = ZC-ZS
        ELSEIF (RZ2.LT.ZB)  THEN
             I2 = IB
             B2 = ZB-ZC
        ELSEIF (RZ2.LT.ZT)  THEN
             I2 = IT
             B2 = ZT-ZB
        ENDIF
        J1 = I1
        J2 = I2
C
C calculate weight for these specific flavors,
C taking into account the possibility of WW/ZZ interference
C
        IF (I1.EQ.I2) THEN
          PIJ = PZ*B1*B2
        ELSE
          D_ISO = IABS(I1-I2)
          ISO_LO = MIN(I1,I2)
          IF ((D_ISO.EQ.1).AND.(MOD(ISO_LO,2).EQ.1)) THEN
            IF (I1.LT.ID) THEN
              PIJ = 2.D0*PZ*B1*B2 + PW*GWLEP*GWLEP/GWSUM**2
            ELSE
              PIJ = 2.D0*PZ*B1*B2 + PW*GWQUA*GWQUA/GWSUM**2
            ENDIF
          ELSE
            PIJ = 2.D0*PZ*B1*B2
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
   43 FORMAT(/,' =CHOOSE= calculate ',
     * 'sin_theta_Cabibbo-squared =',F8.5,' from JETSET parameters.')
   44 FORMAT(/,' =CHOOSE= JETSET CKM matrix is singular.',3X,
     * 'No cabibbo mixing.')
  110 FORMAT(/,/,' Constants used by subroutine CHOOSE:',/,
     * 4X,'(from BORNOX:) SIGWW,SIGZZ =',2E12.5,/,
     * 4X,'PZ = ',F12.5,6X,'PW=',F12.5)
  120 FORMAT(4X,'GWLEP=',F8.5,4X,'GWQUA=',F8.5,/,4X,'GWSUM=',F8.5)
  130 FORMAT(4X,'GZCHL=',F8.5,4X,'GZNUL=',F8.5,/,
     *       4X,'GZDNQ=',F8.5,4X,'GZUPQ=',F8.5,/,4X,'GZSUM=',F8.5)
  140 FORMAT(4X,'W interval:',6F8.5)
  150 FORMAT(4X,'Z interval:',6F8.5,/,15X,6F8.5)
      END
      SUBROUTINE FLAVS(I1,J1,I2,J2)
C======================================================================
C
C Set flags and arrays for the flavor combination (I1,J1,I2,J2).
C     I1 = "down"-type fermion for boson 1
C     J1 = "up"-type fermion for boson 1
C     I2 = "down"-type fermion for boson 2
C     J2 = "up"-type fermion for boson 2
C
C Warning:  the values for J1 and J2 may be exchanged.
C
C Note: The order for Kleiss' code is:
C     boson 1, fermion 1 : up
C     boson 1, fermion 2 : anti-down
C     boson 2, fermion 1 : down
C     boson 2, fermion 2 : anti-up
C
C Also, set WW/ZZ interference flags.
C  "JS" refers to "JetSet" scheme
C  "KL" refers to "Kleiss" scheme
C
C M.Schmitt 26-Jul-1993.
C M.Schmitt 10-May-1994.  Correct fermion order for boson 1.
C M.Schmitt 24-May-1994.  enforce order of flavors
C M.Schmitt  1-Feb-1995.  watch out for cabibbo-mixing
C M.Schmitt 15-Feb-1995.  set ZZPOSS=False if IZFLAG<1.
C M.Schmitt  7-May-1995.  include (1+alfas/pi) color factor
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      INTEGER I1,J1,I2,J2,IJSCOD,IKLCOD,DIJ,DII,ILO,ITEMP
      DOUBLE PRECISION CFACT1,CFACT2,PI
      EXTERNAL IJSCOD,IKLCOD
      SAVE PI
      DATA PI / 0.D0 /
C
      IF (PI.LT.3.D0) PI = DACOS(-1.D0)
C
C enforce order of flavors:  (required by WWS)
C  if WW is possible, then the order must be
C   i1   down   (anti-particle)
C   j1   up     (particle)
C   i2   down   (particle)
C   j2   up     (anti-particle)
C
      DIJ = IABS(I1-J1)
      IF (DIJ.EQ.0) THEN
        DII = IABS(I1-I2)
        ILO = MIN(I1,I2)
        IF ((DII.EQ.1).AND.(MOD(ILO,2).EQ.1)) THEN
          IF (I1.LT.I2) THEN
            ITEMP = J1
            J1 = I2
            I2 = ITEMP
          ELSE
            ITEMP = I1
            I1 = J2
            J2 = ITEMP
          ENDIF
        ENDIF
      ENDIF
C
C beams
C
      IFK(1) = 2
      IFK(2) = 2
C
C boson 1
C
      IFL1(1) =  IJSCOD(J1)
      IFL1(2) = -IJSCOD(I1)
      IFK(3) = IKLCOD(IFL1(1))
      IFK(4) = IKLCOD(IFL1(2))
      IF (IFK(3).GE.3) THEN
        CFACT1 = 3.D0
        IF (ASFLAG.NE.0) CFACT1 = CFACT1 * (1.D0+ALFAS/PI)
      ELSE
        CFACT1 = 1.0D0
      ENDIF
      FLMAS1(1) = FLMASS(J1)
      FLMAS1(2) = FLMASS(I1)
C
C boson 2
C
      IFL2(1) =  IJSCOD(I2)
      IFL2(2) = -IJSCOD(J2)
      IFK(5) = IKLCOD(IFL2(1))
      IFK(6) = IKLCOD(IFL2(2))
      IF (IFK(5).GE.3) THEN
        CFACT2 = 3.D0
        IF (ASFLAG.NE.0) CFACT2 = CFACT2 * (1.D0+ALFAS/PI)
      ELSE
        CFACT2 = 1.0D0
      ENDIF
      FLMAS2(1) = FLMASS(I2)
      FLMAS2(2) = FLMASS(J2)
C
C net color factor
C
      CFACT = CFACT1*CFACT2
C
C set flags for WW/ZZ possibilities
C
      WWPOSS = .FALSE.
      ZZPOSS = .FALSE.
      IF ((J1.EQ.12).OR.(J2.EQ.12)) THEN
        WWPOSS = .FALSE.
      ELSEIF (IABS(J1-I1).EQ.1) THEN
        WWPOSS = .TRUE.
      ELSEIF (IABS(J2-I2).EQ.1) THEN
        WWPOSS = .TRUE.
      ELSEIF (IABS(J1-I1).EQ.3) THEN
        WWPOSS = .TRUE.
      ELSEIF (IABS(J2-I2).EQ.3) THEN
        WWPOSS = .TRUE.
      ELSEIF ((MOD(I1,2).EQ.1).AND.(J2-I1.EQ.1)) THEN
        WWPOSS = .TRUE.
      ELSEIF ((MOD(I1,2).EQ.0).AND.(J2-I1.EQ.-1)) THEN
        WWPOSS = .TRUE.
      ENDIF
      IF (IZFLAG.LT.1) THEN
        ZZPOSS = .FALSE.
      ELSEIF ((J1.EQ.12).OR.(J2.EQ.12)) THEN
        ZZPOSS = .FALSE.
      ELSEIF ((J1-I1.EQ.0).AND.(J2-I2.EQ.0)) THEN
        ZZPOSS = .TRUE.
      ELSEIF ((I2-I1.EQ.0).AND.(J2-J1.EQ.0)) THEN
        ZZPOSS = .TRUE.
      ENDIF
      WINTFR = .FALSE.
      ZINTFR = .FALSE.
      RETURN
      END
      FUNCTION IJSCOD(I)
C----------------------------------------------------------------------
C return Jetset 7.3 code according to I:
C   I =  1     electron
C        2     electron neutrino
C        3     muon
C        4     muon neutrino
C        5     tau
C        6     tau neutrino
C        7     d quark
C        8     u quark
C        9     s quark
C       10     c quark
C       11     b quark
C       12     t quark
C This map is inverted by IJSINV.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IJSCOD,I,IDATA(12)
      SAVE IDATA
      DATA IDATA / 11,12,13,14,15,16, 1,2,3,4,5,6 /
      IF ((I.GE.1).AND.(I.LE.12)) THEN
        IJSCOD = IDATA(I)
      ELSE
        IJSCOD = 0
      ENDIF
      RETURN
      END
      FUNCTION IJSINV(IJS)
C----------------------------------------------------------------------
C invert IJSCOD
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IJSINV,IJS,J,IJSCOD
      EXTERNAL IJSCOD
      IJSINV= 0
      DO 10 J = 1, 12
        IF (IJSCOD(J).EQ.IABS(IJS)) THEN
          IJSINV = J
          RETURN
        ENDIF
   10 CONTINUE
      END
      FUNCTION IKLCOD(IJS)
C----------------------------------------------------------------------
C return KLEISS' code according to the jetset code IJS: (see IJSCOD)
C (see s/r KOPPEL for definition of the Kleiss codes.)
C IJS = 11     electron              IKLCOD = 2
C       12     electron neutrino              1
C       13     muon                           2
C       14     muon neutrino                  1
C       15     tau                            2
C       16     tau neutrino                   1
C        1     d quark                        4
C        2     u quark                        3
C        3     s quark                        4
C        4     c quark                        3
C        5     b quark                        4
C        6     t quark                        3
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IKLCOD,IJS,I
      I = IABS(IJS)
      IF ((I.GE.1).AND.(I.LE.6)) THEN
        IKLCOD = 3+MOD(I,2)
      ELSEIF ((I.GE.11).AND.(I.LE.16)) THEN
        IKLCOD = 1+MOD(I,2)
      ELSE
        IKLCOD = 0
      ENDIF
      RETURN
      END
      SUBROUTINE WZCROS
C======================================================================
C
C Calculate the cross section for WW and ZZ production, and
C the maximum as well.
C
C The input quantities (XMW,XMT,ALFA,etc.) must be defined before
C calling this routine.
C
C This routine is called from ASKUSI.
C
C M.Schmitt
C M.Schmitt 25Jul93.  Add initial-state radiation.
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      INTEGER I,I1,J1,I2,J2
      DOUBLE PRECISION W,WWW,ZZW,TOT0,TOT1,TOT2,WW1,WW2,ZZ1,ZZ2,ZZFRAC
      DOUBLE PRECISION SCM,SPR,X1,X2,F1,F2
      DOUBLE PRECISION PW1(4),PW2(4),QW1(4),QW2(4),QW3(4),QW4(4)
      DOUBLE PRECISION SCALUP
      PARAMETER( SCALUP = 2.0D0 )
C
C initialize constants
C
      NTRIAL = 10000
      SCM = ECM*ECM
      TOT0 = 0.D0
      TOT1 = 0.D0
      TOT2 = 0.D0
      WW1 = 0.D0
      WW2 = 0.D0
      ZZ1 = 0.D0
      ZZ2 = 0.D0
      WWWMAX = -1.0D0
      ZZWMAX = -1.0D0
      X1 = 1.0D0
      X2 = 1.0D0
      F1 = 1.0D0
      F2 = 1.0D0
C
C integrate cross section
C
      WRITE(6,905) NTRIAL
      DO 11 I = 1, NTRIAL
        IF (IRFLAG.NE.0) CALL EBLOSS(SCM,X1,X2,F1,F2)
        W = 0.D0
        WWW = 0.D0
        ZZW = 0.D0
        SPR = SCM*X1*X2
        CALL CHOOSE(I1,J1,I2,J2)
        CALL FLAVS(I1,J1,I2,J2)
        IF (WWPOSS) THEN
          ZINTFR = ZZPOSS
          CALL WWS(SPR,WWW,PW1,PW2,QW1,QW2,QW3,QW4)
          WWW = WWW*F1*F2
          W = WWW
        ELSEIF (ZZPOSS) THEN
          ZINTFR = (I1.EQ.I2)
          CALL ZZS(SPR,ZZW,PW1,PW2,QW1,QW2,QW3,QW4)
          ZZW = ZZW*F1*F2
          W = ZZW
        ENDIF
        TOT0 = TOT0 + 1.D0
        TOT1 = TOT1 + W
        TOT2 = TOT2 + W*W
        WW1 = WW1 + WWW
        WW2 = WW2 + WWW*WWW
        ZZ1 = ZZ1 + ZZW
        ZZ2 = ZZ2 + ZZW*ZZW
        IF (WWW.GT.WWWMAX) WWWMAX = WWW
        IF (ZZW.GT.ZZWMAX) ZZWMAX = ZZW
   11 CONTINUE
C
      IF (TOT0.GT.0.0D0) THEN
        WW1 = WW1/TOT0
        WW2 = WW2/TOT0
        WW2 = DSQRT(WW2-WW1*WW1)/DSQRT(TOT0)
      ELSE
        WRITE(LUOUTP,901) 'WW'
        WW1 = 0.0D0
        WW2 = 1.0D0
      ENDIF
C
      IF (TOT0.GT.0.0D0) THEN
        ZZ1 = ZZ1/TOT0
        ZZ2 = ZZ2/TOT0
        ZZ2 = DSQRT(ZZ2-ZZ1*ZZ1)/DSQRT(TOT0)
      ELSE
        WRITE(LUOUTP,901) 'ZZ'
        ZZ1 = 0.0D0
        ZZ2 = 1.0D0
      ENDIF
C
      IF (TOT0.GT.0.D0) THEN
        TOT1 = TOT1/TOT0
        TOT2 = TOT2/TOT0
        TOT2 = DSQRT(TOT2-TOT1*TOT1)/DSQRT(TOT0)
      ENDIF
C
      WRITE(LUOUTP,911) ECM,WW1,WW2,WWWMAX,
     * ZZ1,ZZ2,ZZWMAX,TOT1,TOT2
C
      IF (TOT1.GT.0.0D0) THEN
        ZZFRAC = ZZ1/TOT1
      ELSE
        WRITE(LUOUTP,961)
        STOP
      ENDIF
      WRITE(LUOUTP,970) ZZFRAC
C
C maximum
C
      WMAX = SCALUP * DMAX1(WWWMAX,ZZWMAX)
C
C store total cross section for function XKSECT
C
      XSECT = SNGL(TOT1)
C
      RETURN
  901 FORMAT(' =WZCROS= Cross section is zero for ee -> ',A)
  905 FORMAT(/,' =WZCROS= integrate cross section using',I7,' points.')
  911 FORMAT(/,' =WZCROS= estimated cross sections at ECM=',
     * F6.1,' GeV:',/,
     * 10X,'WW sigma = ',F8.3,' +/-',F7.3,' pb',4X,
     * 'maximum:',F10.2,/,
     * 10X,'ZZ sigma = ',F8.3,' +/-',F7.3,' pb',4X,
     * 'maximum:',F10.2,/,
     * 10X,'total    = ',F8.3,' +/-',F7.3,' pb',/,/,
     * 10X,'(These numbers are only estimates.  See the summary at',/,
     * 10X,'the end of the job for more accurate results.)')
  961 FORMAT(' =WZCROS= Total cross section is zero!')
  970 FORMAT(/,' =WZCROS= approximate fraction which will be ',
     * 'ZZ events:',F8.5,/)
      END
      SUBROUTINE BORNOX(SVAL,XMZ,XMW,S2W,SIGWW,SIGZZ)
C----------------------------------------------------------------------
C THE LOWEST-ORDER TOTAL CROSS SECTIONS FOR
C E+ E-  ---> W+ W-
C       AND
C E+ E-  ---> ZO ZO
C IN THE STANDARD MODEL, ASSUMING ZERO WIDTHS.
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C
C THE CROSS SECTION FOR E+ E-  --->  ZO ZO
C SEE R.BROWN & K.MIKAELIAN, PHYS.REV.D19(1979)972
      EL=DSQRT(4.*3.1415926536/137.036D0)
      SW=DSQRT(S2W)
      CW=DSQRT(1.-S2W)
      GA=-EL/4./SW/CW
      GV=GA*(1.-4.*S2W)
      SIGZZ=0.
      Z=XMZ**2/SVAL
      IF(Z.GT..25D0) GOTO 2000
      BZ=DSQRT(1.-4.*Z)
      GG=GV**4+6.*GV**2*GA**2+GA**4
      SIGZZ=3.8937D+08 * GG /(4.*3.1415927D0*XMZ**2)
     . *( (1.+4.*Z**2)/(1.-2.*Z)*DLOG((1.+BZ)/(1.-BZ))
     .    -BZ )  * Z
 2000 CONTINUE
C
C THE CROSS SECTION FOR E+ E-  --->  W+ W-
C SEE R.PHILIPPE, PHYS.REV.D26(1982)1588
      S=SVAL/XMW**2
      IF(S.LT.4.D0) GOTO 3000
      BW=DSQRT(1.-4./S)
      BT=DLOG((1.+BW)/(1.-BW))/BW
      R=XMZ**2/XMW**2
      S2=S**2
      S3=S2*S
      G1=2.*S/3.*(S3+16.*S2-68.*S-48.)
      G2=(S2+20.*S-48.)/12.
     .  +4.*(1.-2./S)*BT
      G3=-(S3+18.*S2-28.*S-24.)/3.
     .   +32.*(1.+1./(2.*S))*BT
      SAA=S2W**2/S2*G1
      V=(S2W-1./4.)/CW
      A=-1./4./CW
      SZZ=CW**2*(V**2+A**2)/(-S+R)**2*G1
      SNN=G2
      SAZ=2.*V*CW*S2W/S/(-S+R)*G1
      SAN=S2W/S*G3
      SZN=(V+A)*CW/(-S+R)*G3
      G=EL/SW
      SIGWW=3.8937D+08 * BW /(128.*3.1415927D0*XMW**2*S)
     . *G**4*(SAA+SZZ+SNN+SAZ+SAN+SZN)
C
C PRINT OUT THE RESULTS
 3000 ROOTS=DSQRT(SVAL)
      PRINT 4000,ROOTS,XMZ,XMW,S2W,SIGZZ,SIGWW
 4000 FORMAT(' TOTAL LOWEST-ORDER CROSS SECTIONS FOR',
     . ' ZERO BOSON WIDTHS:'/,
     . ' INVARIANT MASS    =',F10.3,' GEV'/,
     . ' Z0 MASS           =',F10.3,' GEV'/,
     . ' W  MASS           =',F10.3,' GEV'/,
     . ' SIN**2 WEAK ANGLE =',F10.3/,
     . ' E+ E- ---> Z0 Z0  :',F15.6,' PB'/,
     . ' E+ E- ---> W+ W-  :',F15.6,' PB'/,
     . ' ',40(1H-))
      RETURN
      END
      SUBROUTINE KOPPEL
C----------------------------------------------------------------------
C CALCULATE THE FERMION-FERMION-BOSON AND THREE-BOSON COUPLINGS
C IN THE STANDARD MODEL, WITHOUT COLOUR FACTORS.
C ALSO CALCULATE MASS**2 AND MASS*WIDTH OF THE BOSONS.
C V =  FFB VECTOR COUPLING
C A =  FFB AXIAL-VECTOR COUPLING
C B =  BBB COUPLING
C THE FERMION INDICES ARE:1: NEUTRINO OF ELECTRON (MUON,TAU)
C                         2: ELECTRON (MUON,TAU)
C                         3: UP QUARK (CHARM,TOP)
C                         4: DOWN QUARK (STRANGE,BOTTOM)
C THE BOSON INDICES ARE   1: PHOTON
C                         2: Z0 BOSON
C                         3: W+ BOSON
C                         4: W- BOSON
C                         5: GLUON
C NO KOBAYASHI-MASKAWA MIXING IS IMPLEMENTED HERE.
C THE W+- ARE IDENTIFIED AS OUTGIONG FROM THE BBB VERTEX.
C XM2= BOSON MASS**2
C XMG= BOSON MASS*WIDTH
C
C THE FUNDAMENTAL PARAMETERS:
C ALFA = THE Q.E.D. FINE STRUCTURE CONSTANT
C SIN2 = SIN**2 OF THE WEAK MIXING ANGLE
C ALFAS = THE QCD FINE STRUCTURE CONSTANT
C XMZ = MASS OF THE Z0 BOSON IN GEV
C XGZ = TOTAL WIDTH OF THE Z0 BOSON IN GEV
C XMW = MASS OF THE W+- BOSONS IN GEV
C XGW = WIDTH OF THE W+- BOSONS IN GEV
C----------------------------------------------------------------------
C DECW and DECZ modified by a factor to take EW corrections
C into account.  2Aug93.  R.Miquel.
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      INTEGER I,J,K,INFO
      DOUBLE PRECISION PI,E,G,S,C,Z,W,Q,GW,XGW,XGZ
      DOUBLE PRECISION DECW,GWNE,GWUD,GWTB,BWNE,BWUD,BWTB
      DOUBLE PRECISION DECZ,GZNN,GZEE,GZUU,GZDD,GZTT
      DOUBLE PRECISION BZNN,BZEE,BZUU,BZDD,BZTT
      DOUBLE PRECISION HEAVY
      EXTERNAL HEAVY
      SAVE INFO
      DATA INFO/0/
C
C Kleiss's original code
C
CXX      PI = 4.D0*DATAN(1.D0)
CXX      E = DSQRT(4.D0*PI*ALFA )
CXX      G = DSQRT(4.D0*PI*ALFAS)
CXX      S = DSQRT(SIN2W)
CXX      C = DSQRT(1.D0-SIN2W)
CXX      Z = E/4.D0/C/S
CXX      W = E/S/DSQRT(8.D0)
CXX      Q = E*C/S
C
C new way to calculate couplings.  Favor GF over ALPHA
C
      PI = 4.D0*DATAN(1.D0)
      S = DSQRT(SIN2W)
      C = DSQRT(1.D0-SIN2W)
      GW = DSQRT(2.D0)*GF
      GW = 2.D0*XMW*DSQRT(GW)
      E = GW*S
      Z = GW/C/4.D0
      W = GW/2.D0/DSQRT(2.D0)
      Q = GW*C
      G = DSQRT(4.D0*PI*ALFAS)
C
C THE FFB VECTOR COUPLING
      V(1,1) = 0.D0
      V(2,1) = -E
      V(3,1) = 2.D0*E/3.D0
      V(4,1) = -E/3.D0
      V(1,2) =  Z
      V(2,2) = -Z*(1.D0-4.D0*SIN2W)
      V(3,2) =  Z*(1.D0-8.D0/3.D0*SIN2W)
      V(4,2) = -Z*(1.D0-4.D0/3.D0*SIN2W)
      DO 1 J=3,4
      DO 1 K=1,4
    1 V(K,J) = W
      V(1,5) = 0.D0
      V(2,5) = 0.D0
      V(3,5) = G
      V(4,5) = G
C
C THE FFB AXIAL-VECTOR COUPLING
      DO 2 J=1,5,4
      DO 2 K=1,4
    2 A(K,J) = 0.D0
      A(1,2) =  Z
      A(2,2) = -Z
      A(3,2) =  Z
      A(4,2) = -Z
      DO 3 J=3,4
      DO 3 K=1,4
    3 A(K,J) = W
C
C THE BBB COUPLINGS
      DO 4 I=1,5
      DO 4 J=1,5
      DO 4 K=1,5
    4 B(I,J,K) = 0.D0
      B(3,4,1) = -E
      B(4,1,3) = -E
      B(1,3,4) = -E
      B(1,4,3) =  E
      B(4,3,1) =  E
      B(3,1,4) =  E
      B(3,4,2) = -Q
      B(4,2,3) = -Q
      B(2,3,4) = -Q
      B(2,4,3) =  Q
      B(4,3,2) =  Q
      B(3,2,4) =  Q
      B(5,5,5) = G
C
C CALCULATE THE TOTAL DECAY WIDTHS OF THE
C HEAVY BOSONS AND THE BRANCHING RATIOS
C Include ALFAS correction depending on ASFLAG
      DECW=XMW/12.D0/PI
      GWNE=DECW*( V(1,3)**2 + A(1,3)**2 )
      GWUD=DECW*( V(3,3)**2 + A(3,3)**2 ) *3.D0
      IF (ASFLAG.NE.0) GWUD = GWUD * (1.D0+ALFAS/PI)
      GWTB=0.D0
      IF(XMT.GT.XMW) GOTO 31
      GWTB=GWUD*HEAVY( V(3,3)/A(3,3) , XMT/XMW , 0.D0)
   31 XGW= 3.D0*GWNE + 2.D0*GWUD + GWTB
      BWNE=GWNE/XGW *100.D0
      BWUD=GWUD/XGW *100.D0
      BWTB=GWTB/XGW *100.D0
      DECZ=XMZ/12.D0/PI
      GZNN=DECZ*( V(1,2)**2 + A(1,2)**2 )
      GZEE=DECZ*( V(2,2)**2 + A(2,2)**2 )
      GZUU=DECZ*( V(3,2)**2 + A(3,2)**2 ) *3.D0
      GZDD=DECZ*( V(4,2)**2 + A(4,2)**2 ) *3.D0
      IF (ASFLAG.NE.0) THEN
        GZUU = GZUU * (1.D0+ALFAS/PI)
        GZDD = GZDD * (1.D0+ALFAS/PI)
      ENDIF
      GZTT=0.D0
      IF(XMT.GT.XMZ/2.D0) GOTO 32
      GZTT=GZUU*HEAVY( V(3,2)/A(3,2) , XMT/XMZ , XMT/XMZ )
   32 XGZ= 3.D0*GZNN + 3.D0*GZEE + 2.D0*GZUU + 3.D0*GZDD + GZTT
      BZNN=GZNN/XGZ *100.D0
      BZEE=GZEE/XGZ *100.D0
      BZUU=GZUU/XGZ *100.D0
      BZDD=GZDD/XGZ *100.D0
      BZTT=GZTT/XGZ *100.D0
C
C THE MASS AND WIDTH INFORMATION
      XM2(1) = 0.D0
      XM2(2) = XMZ**2
      XM2(3) = XMW**2
      XM2(4) = XM2(3)
      XM2(5) = 0.D0
      XMG(1) = 0.D0
      XMG(2) = XMZ*XGZ
      XMG(3) = XMW*XGW
      XMG(4) = XMG(3)
      XMG(5) = 0.D0
C
C PRINT OUT THE COUPLING CONSTANTS
      IF(INFO.GE.1) RETURN
      INFO=INFO+1
      PRINT 10
   10 FORMAT('0',20(1H-),' START OF KOPPEL INFO ',20(1H-))
      PRINT 11,ALFA,SIN2W,ALFAS,XMZ,XMW,XMT
   11 FORMAT('0'/,
     . ' I CALCULATE THE COUPLINGS IN THE STANDARD MODEL.'/,
     . ' THE INPUT PARAMETERS ARE :'/,
     . ' Q.E.D. FINE STRUCTURE CONSTANT  :',F8.5/,
     . ' SIN**2 OF THE WEAK MIXING ANGLE :',F8.5/,
     . ' Q.C.D. FINE STRUCTURE CONSTANT  :',F8.5/,
     . ' MASS OF THE Z0 BOSON            :',F8.3,' GEV'/,
     . ' MASS OF THE W+/- BOSONS         :',F8.3,' GEV'/,
     . ' MASS OF THE TOP QUARK           :',F8.3,' GEV'/,
     . ' (ALL OTHER FERMIONS ARE HAVE ZERO MASS)' )
      PRINT 12,C,S,Z,W,Q
   12 FORMAT('0',/' C,S,Z,W,Q =',5D10.3)
      PRINT 13,((V(J,K),K=1,5),J=1,4)
   13 FORMAT('0'/,
     . ' FFB VECTOR COUPLING ( ROW=F , COL= B )'/,
     . 4(5D10.3/))
      PRINT 14,((A(J,K),K=1,5),J=1,4)
   14 FORMAT('0'/,
     . ' FFB AXIAL-VECTOR COUPLING ( ROW=F , COL= B )'/,
     . 4(5D10.3/))
      PRINT 15,(((B(I,J,K),K=1,5),J=1,5),I=1,5)
   15 FORMAT('0'/,
     . ' BBB NON-ABELIAN COUPLINGS'/,
     . 5( 5(5D10.3/)/) )
      PRINT 16,XGW,BWNE,BWUD,BWTB,XGZ,BZNN,BZEE,BZUU,BZDD,BZTT
   16 FORMAT('0THE WIDTHS OF THE HEAVY BOSONS, AND THE WAY',
     . ' THEY ARE BUILT UP:'/,
     . ' TOTAL WIDTH OF W+ OR W-       =',F7.3,' GEV'/,
     . ' BR FOR LEPTON DECAY           =',F7.3,' %'/,
     . ' BR FOR MASSLESS QUARK DECAY   =',F7.3,' %'/,
     . ' BR FOR TOP-BOTTOM DECAY       =',F7.3,' %'/,
     . ' TOTAL WIDTH OF Z0             =',F7.3,' GEV'/,
     . ' BR FOR NEUTRINO DECAY         =',F7.3,' %'/,
     . ' BR FOR LEPTON DECAY           =',F7.3,' %'/,
     . ' BR FOR LIGHT UP DECAY         =',F7.3,' %'/,
     . ' BR FOR DOWN DECAY             =',F7.3,' %'/,
     . ' BR FOR TOP DECAY              =',F7.3,' %')
      PRINT 17,(XM2(J),XMG(J),J=1,5)
   17 FORMAT('0'/,
     . ' MASS**2 AND MASS*WIDTH VALUES'/,5(2D10.3/))
      PRINT 18
   18 FORMAT('0',20(1H-),' END OF KOPPEL INFO   ',20(1H-))
      RETURN
      END
      SUBROUTINE BACUS(N)
C----------------------------------------------------------------------
C CALCULATE THE PRODUCT MATRICES S,D AND F
C S(L,J,K) = U.BAR(L,J) *  U(-L,K) , WHERE L IS THE
C HELICITY OF THE SPINOR, AND J,K ARE THE MOMENTA;
C D(J,K) = 2.*DOT(J,K) , WHERE DOT IS THE MINKOWSKI PRODUCT;
C F(J,K) = BK(J)*BK(K)*D(J,K) , WHERE THE BK'S ARE THE KINE-
C MATICAL INDICES OF THE MOMENTA :
C BK(P) = 1 IF THE MOMENTUM P IS OUTGOING;
C       -1 IF THE MOMENTUM P IS INCOMING;
C        0 IF THE MOMENTUM P IS AN AUXILIARY DUMMY.
C THE COMPONENTS ARE DEFINED AS (X,Y,Z,0).
C N = THE NUMBER OF MOMENTA ACTUALLY TO BE CONSIDERED.
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      COMPLEX*16 S
      DOUBLE PRECISION P3(4),P4(4),P5(4),P6(4)
      EQUIVALENCE (P3(1),Q1(1))
      EQUIVALENCE (P4(1),Q2(1))
      EQUIVALENCE (P5(1),Q3(1))
      EQUIVALENCE (P6(1),Q4(1))
      COMMON / PRODUX / S(2,10,10),D(10,10),FF(10,10)
      DIMENSION Q(4,10),E(10)
      EQUIVALENCE (Q(1,1),P1(1))
      INTEGER J,K,N,INFO
      DATA INFO/10/
      DO 1 J=1,10
      E(J)=0.
      DO 1 K=1,10
      S(1,J,K)=0.
      S(2,J,K)=0.
      D(J,K)=0.
    1 FF(J,K)=0.
      DO 2 J=1,N
    2 E(J)=DSQRT(Q(4,J)-Q(1,J))
      DO 3 J=1,N
      DO 3 K=J,N
      S(1,J,K)=DCMPLX(Q(2,J),Q(3,J))*E(K)/E(J)
     .        -DCMPLX(Q(2,K),Q(3,K))*E(J)/E(K)
      S(1,K,J)=-S(1,J,K)
      S(2,J,K)=DCONJG(S(1,K,J))
      S(2,K,J)=DCONJG(S(1,J,K))
      D(J,K)=S(1,J,K)*S(2,K,J)
      FF(J,K)=BK(J)*BK(K)*D(J,K)
      D(K,J)=D(J,K)
    3 FF(K,J)=FF(J,K)
      IF(INFO.GE.1) RETURN
      INFO=INFO+1
      PRINT 11
   11 FORMAT('0',20(1H-),' START OF BACUS INFO  ',20(1H-))
      PRINT  12
   12 FORMAT('0'/,' INPUT FOUR-VECTORS')
      DO  13 J=1,4
   13 PRINT 14,(Q(J,K),K=1,N)
   14 FORMAT(' ',10G12.4)
      PRINT 15,(E(J),J=1,N)
   15 FORMAT('0'/,10G12.4///)
      DO 16 J=1,N
      DO 16 K=J,N
   16 PRINT 17,J,K,S(1,J,K),J,K,S(2,J,K),
     .         J,K,D(J,K),J,K,FF(J,K)
   17 FORMAT('  S(1,',I2,',',I2,') =',2G10.3,
     .       '  S(2,',I2,',',I2,') =',2G10.3,
     .       '  D(',I2,',',I2,') =',G10.3,
     .       '  F(',I2,',',I2,') =',G10.3)
      SUMMOM=0.
      DO 18 J=1,N
      DO 18 K=1,N
   18 SUMMOM=SUMMOM+FF(J,K)
      PRINT 19,SUMMOM
   19 FORMAT('0'/,' MOMENTUM CONSERVATION CHECK :',G15.6)
      PRINT 20
   20 FORMAT('0',20(1H-),' END OF BACUS INFO    ',20(1H-))
      RETURN
      END
      SUBROUTINE GRTYP1(IP1,IP2,B1,
     .                  IP3,IP4,B2,
     .                  IP5,IP6,AMP,KILL)
C----------------------------------------------------------------------
C
C        -------------------------------
C       I                               I
C       I       TYPE  1  DIAGRAM        I
C       I                               I
C        -------------------------------
C
C CALCULATE THE FEYNMAN GRAPH NO.1.    (t-channel)
C THE ENTRIES ARE AS FOLLOWS:
C IP'S = THE FERMION MOMENTUM
C L'S  = THE FERMION HELICITY ( 1=POSITVE, 2=NEGATIVE)
C B'S  = THE BOSON FLAVOUR
C AMP=THE RESULTING AMPLITUDE
C KILL=1 IF THE AMPLITUDE IS GENERALLY NONZERO;
C     =0 IF THE AMPLITUDE IS IDENTICALLY ZERO FOR HELICITY
C        REASONS OR COUPLING CONSTANT COMBINATIONS.
C----------------------------------------------------------------------
C
C                                B1        /---------  IP1
C    IP3 ----------------------------------
C                     |                    \---------  IP2
C                     |
C                     |
C                     |                    /---------  IP5
C    IP4 ----------------------------------
C                                B2        \---------  IP6
C
C
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      COMPLEX*16 PR,S,SP,AMP
      INTEGER IP1,IP2,IP3,IP4,IP5,IP6,B1,B2,IQ1,R1,IQ2,R2
      INTEGER KILL,L1,L3,L5,M3,IH
      COMMON / PRODUX / S(2,10,10),D(10,10),FF(10,10)
      COMMON / HELICI / IH(10)
C
C CHECK ON AMPLITUDE BEING IDENTICALLY ZERO
      AMP=0.
      KILL=0
      L1=IH(IP1)
      L3=IH(IP3)
      L5=IH(IP5)
      IF(L1.NE.IH(IP2)) RETURN
      IF(L3.NE.IH(IP4)) RETURN
      IF(L5.NE.IH(IP6)) RETURN
      COIP1=V(IFK(IP1),B1)+(-1)**L1*A(IFK(IP1),B1)
      IF(COIP1.EQ.0.D0) RETURN
      COIP2=V(IFK(IP3),B1)+(-1)**L3*A(IFK(IP3),B1)
      IF(COIP2.EQ.0.D0) RETURN
      COIP3=V(IFK(IP3),B2)+(-1)**L3*A(IFK(IP3),B2)
      IF(COIP3.EQ.0.D0) RETURN
      COIP4=V(IFK(IP5),B2)+(-1)**L5*A(IFK(IP5),B2)
      IF(COIP4.EQ.0.D0) RETURN
C
C AMPLITUDE ACCEPTED AS NONZERO NUMBER: CALCULATE PROPAGATORS
      KILL=1
      PR=1.
      IF(BK(IP1).EQ.0.OR.BK(IP2).EQ.0) GOTO 1
      PR=1./DCMPLX(FF(IP1,IP2)-XM2(B1),XMG(B1))
    1 CONTINUE
      IF(BK(IP5).EQ.0.OR.BK(IP6).EQ.0) GOTO 2
      PR=PR/DCMPLX( FF(IP5,IP6)-XM2(B2) , XMG(B2) )
    2 CONTINUE
      PR=PR/( FF(IP1,IP2) + FF(IP1,IP3) + FF(IP2,IP3) )
     . * COIP1 * COIP2 * COIP3 * COIP4
C
C CALCULATE SPINORIAL PART OF THE AMPLITUDE
      IF(L1.NE.L3) GOTO 11
      IQ1=IP1
      R1=IP2
      GOTO 12
   11 IQ1=IP2
      R1=IP1
   12 CONTINUE
      IF(L5.NE.L3) GOTO 13
      IQ2=IP5
      R2=IP6
      GOTO 14
   13 IQ2=IP6
      R2=IP5
   14 CONTINUE
      M3=3-L3
      SP=4.*S(L3,IP3,IQ1)
     .     *( BK(IQ1)*S(M3,R1,IQ1)*S(L3,IQ1,IQ2)
     .      + BK(IP3)*S(M3,R1,IP3)*S(L3,IP3,IQ2) )
     .     *S(M3,R2,IP4)
C
C FINAL RESULT
      AMP=(0.D0,-1.D0)*PR*SP
      RETURN
      END
      SUBROUTINE GRTYP2(IP1,IP2,B1,
     .                  IP3,IP4,B2,
     .                  IP5,IP6,B3,AMP,KILL)
C----------------------------------------------------------------------
C
C        -------------------------------
C       I                               I
C       I       TYPE  2  DIAGRAM        I
C       I                               I
C        -------------------------------
C
C CALCULATE THE FEYNMAN GRAPH NO.2.    (s-channel)
C THE ENTRIES ARE AS FOLLOWS:
C P'S = THE FERMION MOMENTUM
C L'S = THE FERMION HELICITY ( 1=POSITVE, 2=NEGATIVE)
C B'S = THE BOSON FLAVOUR
C AMP=THE RESULTING AMPLITUDE
C KILL=1 IF THE AMPLITUDE IS GENERALLY NONZERO;
C     =0 IF THE AMPLITUDE IS IDENTICALLY ZERO FOR HELICITY
C        REASONS OR COUPLING CONSTANT COMBINATIONS.
C----------------------------------------------------------------------
C
C                                               ---------- IP1
C                                              |
C                                              /---------- IP2
C        IP3   -------\                     B1/
C                      \     B2              /
C                        -------------------
C                      /                     \
C        IP4   -------/                     B3\
C                                              \---------- IP5
C                                              |
C                                               ---------- IP6
C
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      COMPLEX*16 PR1,PR2,PR3,PR,S
      COMPLEX*16 SIP1,SIP2,SIP3,SIP4,SIP5,SIP6,SP,AMP
      INTEGER IP1,IP2,IP3,IP4,IP5,IP6,B1,B2,B3
      INTEGER KILL,L1,L3,L5,M1,M3,M5,IH
      COMMON / PRODUX / S(2,10,10),D(10,10),FF(10,10)
      COMMON / HELICI / IH(10)
C
C CHECK ON AMPLITUDE BEING IDENTICALLY ZERO
      AMP=0.
      KILL=0
      L1=IH(IP1)
      L3=IH(IP3)
      L5=IH(IP5)
      IF(L1.NE.IH(IP2)) RETURN
      IF(L3.NE.IH(IP4)) RETURN
      IF(L5.NE.IH(IP6)) RETURN
      BC=B(B1,B2,B3)
      IF(BC.EQ.0.D0) RETURN
      COIP1=V(IFK(IP1),B1)+(-1)**L1*A(IFK(IP1),B1)
      IF(COIP1.EQ.0.D0) RETURN
      COIP2=V(IFK(IP3),B2)+(-1)**L3*A(IFK(IP3),B2)
      IF(COIP2.EQ.0.D0) RETURN
      COIP3=V(IFK(IP5),B3)+(-1)**L5*A(IFK(IP5),B3)
      IF(COIP3.EQ.0.D0) RETURN
C
C AMPLITUDE ACCEPTED AS NONZERO NUMBER: CALCULATE PROPAGATORS
      KILL=1
      PR1=1.
      IF(BK(IP1).EQ.0.OR.BK(IP2).EQ.0) GOTO 1
      PR1=1./DCMPLX( FF(IP1,IP2)-XM2(B1) , XMG(B1))
    1 CONTINUE
      PR2=1.
      IF(BK(IP3).EQ.0.OR.BK(IP4).EQ.0) GOTO 2
      PR2=1./DCMPLX( FF(IP3,IP4)-XM2(B2) , XMG(B2) )
    2 CONTINUE
      PR3=1.
      IF(BK(IP5).EQ.0.OR.BK(IP6).EQ.0) GOTO 3
      PR3=1./DCMPLX( FF(IP5,IP6)-XM2(B3) , XMG(B3) )
    3 CONTINUE
      PR=COIP1*PR1 * COIP2*PR2 * COIP3*PR3 * BC
C
C CALCULATE THE SPINORIAL PART OF THE AMPLITUDE
      M1=3-L1
      M3=3-L3
      M5=3-L5
      IF(L3.NE.L1) GOTO 11
      SIP1 = S(L1,IP1,IP3)*S(M1,IP4,IP2)
      GOTO 12
   11 SIP1 = S(L1,IP1,IP4)*S(M1,IP3,IP2)
   12 CONTINUE
      IF(L5.NE.L3) GOTO 13
      SIP2 = S(L3,IP3,IP5)*S(M3,IP6,IP4)
      GOTO 14
   13 SIP2 = S(L3,IP3,IP6)*S(M3,IP5,IP4)
   14 CONTINUE
      IF(L1.NE.L5) GOTO 15
      SIP3 = S(L5,IP5,IP1)*S(M5,IP2,IP6)
      GOTO 16
   15 SIP3 = S(L5,IP5,IP2)*S(M5,IP1,IP6)
   16 CONTINUE
      SIP4 = BK(IP1)*S(L5,IP5,IP1)*S(M5,IP1,IP6)
     .    + BK(IP2)*S(L5,IP5,IP2)*S(M5,IP2,IP6)
      SIP5 = BK(IP3)*S(L1,IP1,IP3)*S(M1,IP3,IP2)
     .    + BK(IP4)*S(L1,IP1,IP4)*S(M1,IP4,IP2)
      SIP6 = BK(IP5)*S(L3,IP3,IP5)*S(M3,IP5,IP4)
     .    + BK(IP6)*S(L3,IP3,IP6)*S(M3,IP6,IP4)
      SP=4.*( SIP1*SIP4 + SIP2*SIP5 + SIP3*SIP6 )
C
C FINAL RESULT
      AMP=(0.D0,1.D0)*PR*SP
      RETURN
      END
      SUBROUTINE APPROX(S,RM2,RMG,IP,ISWAP,MAPP)
C======================================================================
C Calculate an approximation to the cross section appropriate to
C the generation of two Breit-Wigners.  The output MAPP will be
C used as part of the total event weight.
C called by WWS and ZZS.
C
C input:  S = ECM**2, after ISR
C         RM2 = (M_boson)**2
C         RMG = M_boson * Gamma_boson
C         IP = angular dependence parameter.  (same as s/r TWORES)
C            = 1 for production peaked in forward direction (WW)
C            = 2 for     "        "    in both directions   (ZZ)
C         ISWAP = flag to temporarily exchange fermions 2 and 3
C               = 0 do not swap them
C               > 0 swap them
C output: MAPP = approximation
C
C R.Miquel Dec-93.
C R.Miquel & M.Schmitt 27-Apr-1994.  Add ISWAP.
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION S,RM2,RMG,MAPP
      DOUBLE PRECISION BBB(4),AM12,AM22,D,XL,C,G,RMG2
      INTEGER IP,ISWAP,I
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
C
      MAPP = 0.0D0
C
C swap fermions 2 and 4
C
      IF (ISWAP.GT.0) THEN
        DO I=1,4
          BBB(I)=Q4(I)
          Q4(I)=Q2(I)
          Q2(I)=BBB(I)
        ENDDO
      ENDIF
C
C CALCULATE THE APPPROX. MATRIX ELEMENT (2 POSSIBILITIES)
C
      AM12=(Q1(4)+Q2(4))**2
     .    -(Q1(1)+Q2(1))**2-(Q1(2)+Q2(2))**2-(Q1(3)+Q2(3))**2
      AM22=(Q3(4)+Q4(4))**2
     .    -(Q3(1)+Q4(1))**2-(Q3(2)+Q4(2))**2-(Q3(3)+Q4(3))**2
      D=S-AM12-AM22
      XL=DSQRT(D**2-4.D0*AM12*AM22)
      D=D/XL
      C=(Q1(3)+Q2(3))/SQRT((Q1(4)+Q2(4))**2-AM12)
C
C We have to map C ==> -C (see subroutine TWORES).
C
CCC   IF(IP.EQ.1) G=(D-C)*DLOG((D+1.D0)/(D-1.D0))
      IF(IP.EQ.1) G=(D+C)*DLOG((D+1.D0)/(D-1.D0))
      IF(IP.EQ.2) G=(D*D-C*C)/D*DLOG((D+1.D0)/(D-1.D0))
      RMG2=RMG**2
      G=G
     . *((AM12-RM2)**2+RMG2)
     . *((AM22-RM2)**2+RMG2)
     . *XL/S
      IF (G.NE.0.D0) THEN
        MAPP=ALF(IP)/G
      ELSE
        MAPP = 0.D0
      ENDIF
C
C Put fermions 2 and 4 back in original places
C
      IF (ISWAP.GT.0) THEN
        DO I=1,4
          BBB(I)=Q4(I)
          Q4(I)=Q2(I)
          Q2(I)=BBB(I)
        ENDDO
      ENDIF
C
      RETURN
      END
      SUBROUTINE TWORES(R,S,RM2,RMG,IP,G)
C----------------------------------------------------------------------
C
C PHASE SPACE GENERATOR FOR THE PRODUCTION OF A PAIR
C OF RESONANCES DECAYING INTO 4 PARTICLES.
C INPUTS:
C   R(8): RANDOM NUMBER ARRAY (BETWEEN 0 AND 1)
C   S: TOTAL INVARIANT MASS SQUARED
C   RM: RESONANCE MASS (NOMINAL)
C   RG: RESONANCE TOTAL WIDTH
C   IP: ANGULAR DEPENDENCE PARAMETER:
C     =1: PRODUCTION PEAKED IN FORWARD DIRECTION    (WW)
C     =2:  "           "     "  " & BACKWARD  "     (ZZ)
C OUTPUTS:
C   G: WEIGHT OF THE PRODUCED PHASE SPACE POINT (LE.0)
C   P1,P2: THE MOMENTA OF THE INCOMING PARTICLES
C   Q1,..,Q4:      "   "   "  OUTGOING    "
C
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      DOUBLE PRECISION R(8),S,RM2,RMG,G
      DOUBLE PRECISION AM12,AM22,AM1,AM2
      DOUBLE PRECISION PI,RS,D,XL,C,E,F,F1,F2,C1,C2,S1,S2,SC,R1,R3
      DOUBLE PRECISION Q(4),QV,H(4),PABS
      INTEGER IP,I,ICOM
      SAVE PI
      DATA PI/0.0D0/
      IF (PI.EQ.0.0D0) PI=DACOS(-1.0D0)
C
C BEAM FOUR-VECTORS
C We define the electron (P2) to go in the +Z direction.
C Note:  This forces C ==> -C below.
C
      RS=SQRT(S)
      P1(4)=RS/2.
      P1(3)=-P1(4)
      P1(2)=0.D0
      P1(1)=0.D0
      P2(4)=P1(4)
      P2(3)=-P1(3)
      P2(2)=0.D0
      P2(1)=0.D0
C
C THE DEFAULT EVENT WEIGHT IS ZERO
      G=0.D0
C
C GENERATE THE RESONATING PAIR MASSES
      AM12=RM2+RMG*DTAN((R(1)-.5)*PI)
      AM22=RM2+RMG*DTAN((R(2)-.5)*PI)
      BOS1(5)=DSIGN(DSQRT(DABS(AM12)),AM12)
      BOS2(5)=DSIGN(DSQRT(DABS(AM22)),AM22)
C
C CHECK ON THE PHASE SPACE RESTRICTIONS
      IF(AM12.LT.0.D0) RETURN
      IF(AM22.LT.0.D0) RETURN
      AM1=DSQRT(AM12)
      AM2=DSQRT(AM22)
      IF((AM1+AM2).GT.RS) RETURN
C
C LOAD FERMION MASSES. CHECK PHASE SPACE HERE TO SAVE TIME
      REALP(5,1) = FLMAS1(1)
      REALP(5,2) = FLMAS1(2)
      REALP(5,3) = FLMAS2(1)
      REALP(5,4) = FLMAS2(2)
      IF ( (REALP(5,1)+REALP(5,2) ).GE.AM1.OR.
     *     (REALP(5,3)+REALP(5,4) ).GE.AM2) RETURN
C
C GENERATE THE NONTRIVIAL POLAR ANGLE (2 POSSIBILITIES)
C We have to map C ==> -C because of the definition of the Z axis above.
      D=S-AM12-AM22
      XL=DSQRT(D**2-4.D0*AM12*AM22)
      D=D/XL
      IF(IP.EQ.2) GOTO 3
      C=D-(D+1.D0)*((D-1.D0)/(D+1.D0))**R(3)
      C=-C
      GOTO 4
    3 E=((D+1.D0)/(D-1.D0))**(2.D0*R(3)-1.D0)
      C=D*((E-1.D0)/(E+1.D0))
      C=-C
C
C GENERATE THE REMAINING ANGULAR VARIABLES
    4 F=2.D0*PI*R(4)
      F1=2.D0*PI*R(5)
      F2=2.D0*PI*R(6)
      C1=-1.D0+2.D0*R(7)
      C2=-1.D0+2.D0*R(8)
      S1=DSQRT(1.D0-C1*C1)
      S2=DSQRT(1.D0-C2*C2)
      SC=DSQRT(1.D0-C*C)
C
C CONSTRUCT MOMENTUM OF THE FIRST RESONANCE
      SC=DSQRT(1.-C*C)
      Q(4)=(S+AM12-AM22)/(2.*RS)
      QV=DSQRT(Q(4)**2-AM12)
      Q(3)=QV*C
      Q(2)=QV*SC*DCOS(F)
      Q(1)=QV*SC*DSIN(F)
      DO 120 ICOM=1,4
        BOS1(ICOM)=Q(ICOM)
  120 CONTINUE
C
C        MASSLESS 4-MOMENTA OF FINAL FERMIONS FOR THE SPINORS:
C CONSTRUCT Q1 IN REST FRAME OF FIRST RESONANCE
      H(4)=AM1/2.D0
      H(3)=H(4)*C1
      H(2)=H(4)*S1*DCOS(F1)
      H(1)=H(4)*S1*DSIN(F1)
C
C BOOST VECTOR H TO ITS LAB FRMANE VALUE Q1
      Q1(4)=(Q(4)*H(4)+Q(3)*H(3)+Q(2)*H(2)+Q(1)*H(1))/AM1
      R1=(Q1(4)+H(4))/(Q(4)+AM1)
      DO 5 I=1,3
    5 Q1(I)=H(I)+R1*Q(I)
C
C CONSTRUCT Q2 FROM MOMENTUM CONSERVATION
      DO 6 I=1,4
    6 Q2(I)=Q(I)-Q1(I)
C
C MASSIVE 4-MOMENTA OF FINAL FERMIONS FOR THE OUTPUT:
C
      H(4) = (AM12 + REALP(5,1)**2 - REALP(5,2)**2)/2./AM1
      PABS=SQRT((AM12-REALP(5,1)**2-REALP(5,2)**2)**2-4*REALP(5,1)**2
     >                *REALP(5,2)**2)/2./AM1
      H(3)=PABS*C1
      H(2)=PABS*S1*DCOS(F1)
      H(1)=PABS*S1*DSIN(F1)
C BOOST VECTOR H TO ITS LAB FRAME VALUE REALP
      REALP(4,1)=(Q(4)*H(4)+Q(3)*H(3)+Q(2)*H(2)+Q(1)*H(1))/AM1
      R1=(REALP(4,1)+H(4))/(Q(4)+AM1)
      DO 15 I=1,3
15    REALP(I,1)=H(I)+R1*Q(I)
C CONSTRUCT REALP(I,2) FROM MOMENTUM CONSERVATION
      DO 16 I=1,4
16    REALP(I,2)=Q(I)-REALP(I,1)
C
C
C ++++++++  CONSTRUCT THE MOMENTUM OF THE SECOND RESONANCE ++++++++++
C
      Q(4)=RS-Q(4)
      DO 7 I=1,3
    7 Q(I)=-Q(I)
      DO 130 ICOM=1,4
        BOS2(ICOM)=Q(ICOM)
130   CONTINUE
C
C        MASSLESS 4-MOMENTA OF FINAL FERMIONS FOR THE SPINORS:
C CONSTRUCT Q3 IN REST FRAME OF SECOND RESONANCE
      H(4)=AM2/2.D0
      S2=DSQRT(1.-C2*C2)
      H(3)=H(4)*C2
      H(2)=H(4)*S2*DCOS(F2)
      H(1)=H(4)*S2*DSIN(F2)
C
C BOOST H TO ITS LAB FRAME VALUE Q3
      Q3(4)=(Q(4)*H(4)+Q(3)*H(3)+Q(2)*H(2)+Q(1)*H(1))/AM2
      R3=(Q3(4)+H(4))/(Q(4)+AM2)
      DO 8 I=1,3
    8 Q3(I)=H(I)+R3*Q(I)
C
C CONSTRUCT Q4 FROM MOMENTUM CONSERVATION
      DO 9 I=1,4
    9 Q4(I)=Q(I)-Q3(I)
C
C        MASSIVE 4-MOMENTA OF FINAL FERMIONS FOR THE OUTPUT:
C
      H(4) = (AM22 + REALP(5,3)**2 - REALP(5,4)**2)/2./AM2
      PABS=SQRT((AM22-REALP(5,3)**2-REALP(5,4)**2)**2-4*REALP(5,3)**2
     >                *REALP(5,4)**2)/2./AM2
      H(3)=PABS*C2
      H(2)=PABS*S2*DCOS(F2)
      H(1)=PABS*S2*DSIN(F2)
C
C BOOST H TO ITS LAB FRAME VALUE REALP
      REALP(4,3)=(Q(4)*H(4)+Q(3)*H(3)+Q(2)*H(2)+Q(1)*H(1))/AM2
      R3=(REALP(4,3)+H(4))/(Q(4)+AM2)
      DO 18 I=1,3
18    REALP(I,3)=H(I)+R3*Q(I)
C
C CONSTRUCT REALP(I,4) FROM MOMENTUM CONSERVATION
      DO 19 I=1,4
19    REALP(I,4)=Q(I)-REALP(I,3)
C
      G = 1.D0
      END
      FUNCTION COULMB(S)
C======================================================================
C This function calculates an approximate value for the correction
C to W+W- production due to the Coulomb singularity.
C Latest theoretical formula: V.S.Fadin, V.A.Khoze, A.D.Martin,
C A.Chapovsky, in DTP/94/116 (Dec,1994) which differs from previous
C calculation in setting DELTA = 0.
C Previous calculation is given in D.Bardin,W.Beenakker,A.Denner
C CERN-TH.6953/93
C original version based on D.Bardin, DESY 93-035
C
C input:   S = center of mass energy squared (after ISR)
C output:  COULMB = correction:   sigma --> sigma * (1 + COULMB)
C          where sigma is the cross section.
C
C 11Aug93  M.Schmitt & R.Miquel  first version using Bardin formula
C 16Aug94  M.Schmitt update to D.Bardin,W.Beenakker,A.Denner formula
C 12Feb95  M.Schmitt & R.Miquel  update to Fadin et.al.
C-----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      DOUBLE PRECISION COULMB,S,S1,S2,MW,GW,CONST,PI
      DOUBLE PRECISION M1,M2,BETA,DELTA,IMBETM,ANG,AT,B2
      COMPLEX*8 BETM,BD,MSQ
      SAVE CONST,MW,GW,PI,MSQ
      DATA CONST / 0.D0 /
C
C initialize constant
C
      IF (CONST.EQ.0D0) THEN
        PI = 4.D0*DATAN(1.D0)
        MW = DSQRT(XM2(3))
        GW = XMG(3)/MW
        CONST = 0.5D0*PI*ALFA
        MSQ = DCMPLX(XM2(3),XMG(3))
      ENDIF
C
C calculate the correction
C
      M1 = BOS1(5)
      M2 = BOS2(5)
      IF ((S.EQ.0D0).OR.(M1.EQ.0D0).OR.(M2.EQ.0D0)) THEN
        WRITE(6,100) S,M1,M2
        COULMB = 0.D0
        RETURN
      ENDIF
      BETA = (S - (M1-M2)**2) * (S - (M1+M2)**2)
      IF (BETA.LE.0.D0) THEN
        WRITE(6,110) S,M1,M2,BETA
        COULMB = 0.D0
        RETURN
      ENDIF
      BETA = DSQRT(BETA)/S
C-OLD DELTA = DABS(M1**2-M2**2)/S
      DELTA = 0.D0
      BETM = (1.0D0-4.0D0*MSQ/S)
      BETM = CSQRT(BETM)
      IMBETM = AIMAG(BETM)
      BD = BETM+DCMPLX(DELTA)
      B2 = CABS(BD)**2
      ANG = 0.5D0*(B2-BETA**2)/(BETA*IMBETM)
      AT = DATAN(ANG)
      IF (AT.GT.0.5D0*PI) AT = AT-PI
      COULMB = CONST*(1.D0-2.D0*AT/PI)/BETA
      RETURN
C
  100 FORMAT(' =COULMB= zero masses: S,M1,M2=',3E12.5)
  110 FORMAT(' =COULMB= neg sqrt for BETA zero masses: S,M1,M2=',4E12.5)
      END
      SUBROUTINE BWMODF(I1,I2,WF1,WF2)
C======================================================================
C Calculate weights for the boson resonances which take into account
C the dependence of the boson width on its mass.
C (S/r TWORES generates a simple Breit-Wigner shape for each boson.
C The weights calculated here are the ratio of a Breit-Wigner with
C mass-dependent width to that with a constant width.)
C
C input:  I1,I2 are the boson "flavors"  (Kleiss convention)
C output: WF1,WF2 are the corresponding weights.
C
C 13Aug93 M.Schmitt & R.Miquel
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      DOUBLE PRECISION S1,S2,A1,A2,U1,U2,WF1,WF2
      INTEGER I1,I2
C
C don't touch photons or gluons
C
      IF ((I1.EQ.1).OR.(I1.EQ.5).OR.(I2.EQ.1).OR.(I2.EQ.5)) THEN
        WF1 = 1.D0
        WF2 = 1.D0
        RETURN
      ENDIF
C
C get boson masses
C
      S1 = BOS1(5)**2
      S2 = BOS2(5)**2
      A1 = S1/XM2(I1)
      A2 = S2/XM2(I2)
      U1 = (S1-XM2(I1))**2
      U2 = (S2-XM2(I2))**2
C
C calculate ratio of breit-wigner resonance terms
C
      WF1 = (U1+XMG(I1)**2) / (U1+A1**2*XMG(I1)**2)
      WF2 = (U2+XMG(I2)**2) / (U2+A2**2*XMG(I2)**2)
C
      RETURN
      END
      SUBROUTINE WWS(S,W,H1,H2,H3,H4,H5,H6)
C----------------------------------------------------------------------
C
C calculate weight for a e+e- --> W+W- event
C
C input:    S     = center-of-mass energy squared
C outputs:  W     = event weight
C           H1,H2 = beam electron four-vectors
C           H3,H4 = four-vectors for first fermion pair
C           H5,H6 = four-vectors for second fermion pair
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      DOUBLE PRECISION S,W,H1(4),H2(4),H3(4),H4(4),H5(4),H6(4)
      DOUBLE PRECISION SIGAPW,SIGAPZ,PW,R(8),RMAPW,RMAPZ
      REAL    RW,RNDM
      EXTERNAL RNDM
      INTEGER NMANY,I,J,L1,L2,L3,L4,L5,L6,LREST
      INTEGER KIL1,KIL2,KIL3,KIL4,KIL5
      COMMON / HELICI / L1,L2,L3,L4,L5,L6,LREST(4)
      COMPLEX*16 AM(20)
      DOUBLE PRECISION X1,X2,X3,XREST(9)
      EQUIVALENCE (X1,XXS(1))
      EQUIVALENCE (X2,XXS(2))
      EQUIVALENCE (X3,XXS(3))
      EQUIVALENCE (XREST(1),XXS(4))
      DOUBLE PRECISION PI,HC2
      PARAMETER (PI=3.141592654D0,HC2=0.38937966D+09)
      DOUBLE PRECISION HEV1,HEV2,HEV3,HEV4,BW1,BW2
      DOUBLE PRECISION COULMB
      EXTERNAL COULMB
C
C INITIALIZE THE OUTPUT MOMENTA TO ZERO
C
      DO 81 J=1,4
        H1(J)=0.
        H2(J)=0.
        H3(J)=0.
        H4(J)=0.
        H5(J)=0.
        H6(J)=0.
   81 CONTINUE
C
C overall constants (which depend on S )
C
      W = 0.D0
      SIGAPW=1.D0/(2.D0*S)
     .      *1.D0/(2.D0*PI)**8
     .      *1.D0/(8.D0)**3
     .      *4.D0
     .      *(2.D0*PI)**3
     .      *(PI/XMG(3))**2
     .      *HC2
     .      *ALF(1)
      SIGAPZ=1.D0/(2.D0*S)
     .      *1.D0/(2.D0*PI)**8
     .      *1.D0/(8.D0)**3
     .      *4.D0
     .      *(2.D0*PI)**3
     .      *(PI/XMG(2))**2
     .      *HC2
     .      *ALF(2)
      PW = SIGAPW/(SIGAPW+SIGAPZ)
      IF (PW.GT.0.99D0) PW = 0.99D0
C
C GENERATE A PHASE SPACE EVENT
C
C If final state can go through both WW and ZZ intermediate states,
C then randomly choose (according to PW), whether to favor peaking
C in the (uu/dd) or in the (ud/ud) channels.  In either case, calculate
C the total weight, by calling APPROX for both combinations.
C
      CALL RNDM8(R,NMANY)
      IF (.NOT.ZINTFR) THEN
        CALL TWORES(R,S,XM2(3),XMG(3),1,W)
        IPEAK = 0
        IF (W.EQ.0.D0) RETURN
        CALL APPROX(S,XM2(3),XMG(3),1,0,RMAPW)
      ELSE
        RW = RNDM(S)
        IF (RW.LT.SNGL(PW)) THEN
C generate four-vectors (including kinematic adjustments for masses)
          CALL TWORES(R,S,XM2(3),XMG(3),1,W)
          IPEAK = 1
          IF (W.EQ.0.D0) RETURN
          CALL APPROX(S,XM2(3),XMG(3),1,0,RMAPW)
          CALL APPROX(S,XM2(2),XMG(2),2,1,RMAPZ)
        ELSE
C must interchange fermion masses to correspond to ZZ --> f1f2 f3f4
          H1(1) = FLMAS2(2)
          FLMAS2(2) = FLMAS1(2)
          FLMAS1(2) = H1(1)
C generate four-vectors (including kinematic adjustments for masses)
          CALL TWORES(R,S,XM2(2),XMG(2),2,W)
          IPEAK = 2
          IF (W.EQ.0.D0) RETURN
          CALL APPROX(S,XM2(2),XMG(2),2,0,RMAPZ)
          CALL APPROX(S,XM2(3),XMG(3),1,1,RMAPW)
C interchange four-vectors to conform to "WW" ordering.
          DO I = 1, 4
            H1(I) = Q2(I)
            Q2(I) = Q4(I)
            Q4(I) = H1(I)
            H1(I) = REALP(I,2)
            REALP(I,2) = REALP(I,4)
            REALP(I,4) = H1(I)
          ENDDO
          H1(1) = REALP(5,2)
          REALP(5,2) = REALP(5,4)
          REALP(5,4) = H1(1)
          H1(1) = FLMAS2(2)
          FLMAS2(2) = FLMAS1(2)
          FLMAS1(2) = H1(1)
        ENDIF
      ENDIF
C
C CALCULATE THE EXACT CROSS SECTION
C   BACUS sets up the spin tensor for pairs of fermions
C
      CALL BACUS(6)
C
C sum amplitudes over fermion helicities
C  L1,L2 = beam helicities
C  L3,L4,L5,L6 = final state fermion helicities
C
      X1=0.D0
      X2=0.D0
      X3=0.D0
      DO 100 L1=1,2
        L2=L1
        DO 100 L3=1,2
          DO 100 L4=1,2
            DO 100 L5=1,2
              DO 100 L6=1,2
C
C one t-channel and two s-channel diagrams.
C
                CALL GRTYP1(3,4,3, 1,2,   4,5,6, AM(1),KIL1)
                CALL GRTYP2(3,4,3, 1,2,1, 5,6,4, AM(2),KIL2)
                CALL GRTYP2(3,4,3, 1,2,2, 5,6,4, AM(3),KIL3)
                X2=X2+CDABS(AM(1)+AM(2)+AM(3))**2
C
C amplitudes with Zs, if appropriate.  (t-channel)
C
                IF (ZINTFR) THEN
                  CALL GRTYP1(3,6,2, 1,2, 2,5,4,AM(4),KIL4)
                  CALL GRTYP1(5,4,2, 1,2, 2,3,6,AM(5),KIL5)
                  X1=X1+CDABS(AM(4)+AM(5))**2
                  X3=X3+DREAL(DCONJG(AM(1)+AM(2)+AM(3))*(AM(4)+AM(5)))
                ENDIF
C
  100 CONTINUE
C
C get phase space and matrix element mass factors
C works on realp vectors 1-4, the outgoing massive femion 4-momenta
C
      IF (.NOT.ZINTFR) THEN
        CALL REDUC(1,2,3,1,HEV1)
        CALL REDUC(3,4,3,2,HEV2)
      ELSE
        IF (IPEAK.LT.2) THEN
          CALL REDUC(1,2,3,1,HEV1)
          CALL REDUC(3,4,3,2,HEV2)
          CALL REDUC(1,4,2,0,HEV3)
          CALL REDUC(2,3,2,0,HEV4)
        ELSE
          CALL REDUC(1,4,3,1,HEV1)
          CALL REDUC(2,3,3,2,HEV2)
          CALL REDUC(1,2,2,0,HEV3)
          CALL REDUC(3,4,2,0,HEV4)
        ENDIF
      ENDIF
C
C correction for coulomb singularity
C
      IF (CSFLAG.EQ.0) THEN
        COULF = 1.D0
      ELSE
        COULF = 1.D0 + COULMB(S)
      ENDIF
C
C correction for Breit-Wigner with mass-dependent width
C
      IF (BWFLAG.EQ.0) THEN
        BW1 = 1.D0
        BW2 = 1.D0
      ELSE
        CALL BWMODF(3,4,BW1,BW2)
      ENDIF
C
C put all factors together
C (modified to include interference term X3 exactly)
C
      X2=X2*CFACT*COULF*HEV1*HEV2/4.D0
      IF (ZINTFR) THEN
        X1=X1*CFACT*HEV3*HEV4/4.D0
        X3= -X3*DSQRT(HEV1*HEV2*HEV3*HEV4)*DSQRT(CFACT)/2.D0
        W=X2+X1+X3
        W=W*(SIGAPW+SIGAPZ)/(RMAPW+RMAPZ)
      ELSE
        W=X2
        W=W*SIGAPW/RMAPW
      ENDIF
      W=W*BW1*BW2/PIJ
C
C DEFINE THE OUTPUT MOMENTA
C
      DO 92 J=1,4
        H1(J)=P1(J)
        H2(J)=P2(J)
C       H3(J)=Q1(J)
C       H4(J)=Q2(J)
C       H5(J)=Q3(J)
C       H6(J)=Q4(J)
        H3(J) = REALP(J,1)
        H4(J) = REALP(J,2)
        H5(J) = REALP(J,3)
        H6(J) = REALP(J,4)
   92 CONTINUE
C
      RETURN
      END
      SUBROUTINE ZZS(S,W,H1,H2,H3,H4,H5,H6)
C----------------------------------------------------------------------
C
C calculate weight for a e+e- --> ZZ event
C
C input:    S     = center-of-mass energy squared
C outputs:  W     = event weight
C           H1,H2 = beam electron four-vectors
C           H3,H4 = four-vectors for first fermion pair
C           H5,H6 = four-vectors for second fermion pair
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      DOUBLE PRECISION S,W,H1(4),H2(4),H3(4),H4(4),H5(4),H6(4)
      DOUBLE PRECISION SIGAPZ,R(8),RMAPZ,RMAPZ2,QTEMP(5)
      REAL RX,RNDM
      EXTERNAL RNDM
      INTEGER NMANY,J,L1,L2,L3,L4,L5,L6,LREST
      INTEGER KIL1,KIL2,KIL12,KIL13
      COMMON / HELICI / L1,L2,L3,L4,L5,L6,LREST(4)
      COMPLEX*16 AM(20)
      DOUBLE PRECISION X1,X2,X3,X4,X5,X6,X7,X8,X9,XREST(3)
      EQUIVALENCE (XREST(1),XXS(1))
      EQUIVALENCE (X1,XXS(4))
      EQUIVALENCE (X2,XXS(5))
      EQUIVALENCE (X3,XXS(6))
      EQUIVALENCE (X4,XXS(7))
      EQUIVALENCE (X5,XXS(8))
      EQUIVALENCE (X6,XXS(9))
      EQUIVALENCE (X7,XXS(10))
      EQUIVALENCE (X8,XXS(11))
      EQUIVALENCE (X9,XXS(12))
      DOUBLE PRECISION PI,HC2
      PARAMETER (PI=3.141592654D0,HC2=0.38937966D+09)
      DOUBLE PRECISION HEV1,HEV2,HEV3,HEV4,BW1,BW2
C
C INITIALIZE THE OUTPUT MOMENTA TO ZERO
C
      DO 81 J=1,4
        H1(J)=0.
        H2(J)=0.
        H3(J)=0.
        H4(J)=0.
        H5(J)=0.
        H6(J)=0.
   81 CONTINUE
C
      W = 0.D0
      SIGAPZ=1.D0/(2.D0*S)
     .      *1.D0/(2.D0*PI)**8
     .      *1.D0/(8.D0)**3
     .      *4.D0
     .      *(2.D0*PI)**3
     .      *(PI/XMG(2))**2
     .      *HC2
     .      *ALF(2)
C
C GENERATE A PHASE SPACE EVENT
C
      CALL RNDM8(R,NMANY)
      CALL TWORES(R,S,XM2(2),XMG(2),2,W)
      IPEAK = 3
      IF (W.EQ.0.D0) RETURN
C
C Calculate approximate cross section, used in calculating
C the weight for this "event"
C
      CALL APPROX(S,XM2(2),XMG(2),2,0,RMAPZ)
C
C if all four fermions have the same flavor, then we have to take
C into account the "other" combination leading to resonanting Z's
C
      IF (ZINTFR) THEN
        DO J = 1, 5
          IF (J.LT.5) THEN
            QTEMP(J) = Q2(J)
            Q2(J) = Q4(J)
            Q4(J) = QTEMP(J)
          ENDIF
          QTEMP(J)   = REALP(J,2)
          REALP(J,2) = REALP(J,4)
          REALP(J,4) = QTEMP(J)
        ENDDO
        CALL APPROX(S,XM2(2),XMG(2),2,0,RMAPZ2)
        RX = RNDM(S)
        IF (RX.LT.0.5) THEN
          DO J = 1, 5
            IF (J.LT.5) THEN
              QTEMP(J) = Q2(J)
              Q2(J) = Q4(J)
              Q4(J) = QTEMP(J)
            ENDIF
            QTEMP(J) = REALP(J,2)
            REALP(J,2) = REALP(J,4)
            REALP(J,4) = QTEMP(J)
          ENDDO
        ENDIF
        RMAPZ = 0.5*(RMAPZ+RMAPZ2)
      ENDIF
C
C CALCULATE THE EXACT CROSS SECTION
C
      CALL BACUS(6)
C
      X1=0.D0
      X2=0.D0
      X3=0.D0
      DO 100 L1=1,2
        L2=L1
        DO 100 L3=1,2
          DO 100 L4=1,2
            DO 100 L5=1,2
              DO 100 L6=1,2
C
C   THE DIAGRAMS FOR ALL ZZ FINAL STATES
C                           f f B  e e  B f f
                CALL GRTYP1(3,4,2, 1,2, 2,5,6,AM(1),KIL1)
                CALL GRTYP1(5,6,2, 1,2, 2,3,4,AM(2),KIL2)
                X1=X1+CDABS(AM(1)+AM(2))**2
C
C   INTERFERE WITH W DIAGRAMS IS NOT CALCULATED HERE.
C   Any diagram which may come from W's is calculated in WWS,
C   according to the flags set in s/r FLAVS.  This is an
C   arbitrary but consistently implemented choice.
C
CCC                IF (WINTFR) THEN
CCC                  CALL GRTYP1(3,6,3,1,2,4,5,4,AM(3),KIL3)
CCC                  CALL GRTYP2(3,6,3,1,2,1,5,4,4,AM(4),KIL4)
CCC                  CALL GRTYP2(3,6,3,1,2,2,5,4,4,AM(5),KIL5)
CCC                  X2=X2+CDABS(AM(3)+AM(4)+AM(5))**2
CCC                  X3=X3+DREAL(DCONJG(AM(3)+AM(4)+AM(5))*(AM(1)+AM(2)))
CCC                ENDIF
C
C   INTERFERE WITH ADDITIONAL CROSSED Z DIAGRAMS
C
                IF (ZINTFR) THEN
C                             f f B  e e  B f f
                  CALL GRTYP1(3,6,2, 1,2, 2,5,4, AM(3),KIL12)
                  CALL GRTYP1(5,4,2, 1,2, 2,3,6, AM(4),KIL13)
                  X2=X2+CDABS(AM(3)+AM(4))**2
                  X3=X3+DREAL(DCONJG(AM(3)+AM(4))*(AM(1)+AM(2)))
                ENDIF
C
  100 CONTINUE
C
C get phase space and martrix element mass factors
C
      CALL REDUC(1,2,2,1,HEV1)
      CALL REDUC(3,4,2,2,HEV2)
C
CCC      IF (WINTFR)  THEN
CCC        CALL REDUC(1,4,3,0,HEV3)
CCC        CALL REDUC(2,3,3,0,HEV4)
CCC      ENDIF
      IF (ZINTFR) THEN
        CALL REDUC(1,4,2,0,HEV3)
        CALL REDUC(2,3,2,0,HEV4)
      ENDIF
C
C correction for Breit-Wigner with mass-dependent width
C
      IF (BWFLAG.EQ.0) THEN
        BW1 = 1.D0
        BW2 = 1.D0
      ELSE
        CALL BWMODF(2,2,BW1,BW2)
      ENDIF
C
C ADD UP THE CONTRIBUTIONS
C
      X1=X1*HEV1*HEV2*CFACT/4.D0
CCC      IF (WINTFR.OR.ZINTFR) THEN
      IF (ZINTFR) THEN
        X2=X2*HEV3*HEV4*CFACT/4.D0
        X3=-X3*DSQRT(HEV1*HEV2*HEV3*HEV4*CFACT)/2.D0
        W=(X1+X2+X3)/4.D0
      ELSE
        W=X1
      ENDIF
      W=W*SIGAPZ/RMAPZ
      W = W*BW1*BW2/PIJ
C
C DEFINE THE OUTPUT MOMENTA
C
      DO 92 J=1,4
        H1(J)=P1(J)
        H2(J)=P2(J)
C       H3(J)=Q1(J)
C       H4(J)=Q2(J)
C       H5(J)=Q3(J)
C       H6(J)=Q4(J)
        H3(J) = REALP(J,1)
        H4(J) = REALP(J,2)
        H5(J) = REALP(J,3)
        H6(J) = REALP(J,4)
   92 CONTINUE
C
      RETURN
      END
      FUNCTION XM(A,B)
C----------------------------------------------------------------------
C calculate the invariant mass
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION XM,A(*),B(*)
      XM=A(4)*B(4)-A(3)*B(3)-A(2)*B(2)-A(1)*B(1)
      IF (XM.GT.1.0E-11) THEN
        XM=DSQRT(2.D0*XM)
      ELSE
        XM = 0.0D0
      ENDIF
      RETURN
      END
      SUBROUTINE REDUC(IP1,IP2,IB,IFLAG,HEV)
C----------------------------------------------------------------------
C
C        CALULATE THE PHASE SPACE AND MATRIX ELEMENT REDUCTION FACTOR
C        FOR BOSON-->F FBAR DECAY.
C
C        IP1,IP2: PARTICLE ORDER NO. (1-4)
C        IB: BOSON ID NO.
C        IFLAG: 0--FIND RESONANCE MASS FROM INVARIANT MASS OF FERMION
C                   4-VECTORS
C               1,2--TAKE RES. MASS FROM XM1 OR XM2 RESPECTIVELY.
C----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
      DOUBLE PRECISION RESM,HEV,RAT,HEAVY
      EXTERNAL HEAVY
      INTEGER IP1,IP2,IFLAG,IB
C
      HEV = 0.D0
      RAT = V(IFK(IP1+2),IB)/A(IFK(IP1+2),IB)
      IF (IFLAG .EQ. 1)  THEN
        RESM = BOS1(5)
      ELSEIF (IFLAG .EQ. 2) THEN
        RESM = BOS2(5)
      ELSE
        RESM = DSQRT( ABS( REALP(5,IP1)**2 + REALP(5,IP2)**2 +
     *   2.D0*(
     *     REALP(4,IP1)*REALP(4,IP2)
     *    -REALP(1,IP1)*REALP(1,IP2)
     *    -REALP(2,IP1)*REALP(2,IP2)
     *    -REALP(3,IP1)*REALP(3,IP2) ) ) )
        IF ( (REALP(5,IP1)+REALP(5,IP2)) .GE. RESM) RETURN
      ENDIF
C
      HEV = HEAVY(RAT,REALP(5,IP1)/RESM,REALP(5,IP2)/RESM)
C
      RETURN
      END
      FUNCTION HEAVY(X,Y1,Y2)
C----------------------------------------------------------------------
C
C PHASE SPACE REDUCTION FACTOR FOR HEAVY FERMIONIC DECAYS
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION HEAVY,X,Y1,Y2
      HEAVY = ( 1.D0 -0.5D0*(Y1**2+Y2**2)
     . -0.5D0*(Y1**2-Y2**2)**2 + 3.D0*Y1*Y2*((X**2-1.D0)/(X**2+1.D0)))
      HEAVY = HEAVY*SQRT( (1.D0-Y1**2-Y2**2)**2 - 4.D0*Y1**2*Y2**2 )
C
      RETURN
      END
      SUBROUTINE WWHISI
C=======================================================================
C
C Book diagnostic histograms for LEPWW MC event generator.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
C
C global histogram offset
C
      IDOF = 5000
C
C book the histograms
C
      CALL HBOOK1(IDOF+1,'number of trials per accepted event',
     *            100,0.,500.,0.)
      CALL HBOOK1(IDOF+2,'routine (WWS=1,ZZS=2)',5,0.,5.,0.)
      CALL HBOOK1(IDOF+3,'weights from WWS',150,0.,300.,0.)
      CALL HBOOK1(IDOF+4,'weights from ZZS',150,0.,300.,0.)
      CALL HBOOK1(IDOF+5,'all weights',150,0.,300.,0.)
      CALL HBOOK1(IDOF+6,'WWS interference with ZZ',4,0.,4.,0.)
      CALL HBOOK1(IDOF+7,'ZZS interference with ZZ',4,0.,4.,0.)
      CALL HBOOK1(IDOF+8,'kinematic PEAK flag',6,0.,6.,0.)
C
      CALL HBOOK1(IDOF+11,'center-of-mass energy GeV',110,100.,210.,0.)
      CALL HBOOK1(IDOF+12,'ISR per beam (GeV)',100,0.,50.,0.)
      CALL HBOOK1(IDOF+13,'effective Egamma (GENTLE-style) (GeV)',
     *  100,0.,50.,0.)
      CALL HBOOK1(IDOF+14,'net PZ (GeV)',100,0.,40.,0.)
      CALL HBOOK1(IDOF+15,'vertex X',40,-0.1,0.1,0.)
      CALL HBOOK1(IDOF+16,'vertex Y',40,-0.004,0.004,0.)
      CALL HBOOK1(IDOF+17,'vertex Z',40,-4.0,4.0,0.)
C
      CALL HBOOK1(IDOF+21,'WW event, resonating mass',60,50.,110.,0.)
      CALL HBOOK1(IDOF+22,'WW event, NONresonating mass',50,0.,200.,0.)
      CALL HBOOK1(IDOF+23,'ZZ event, resonating mass',60,50.,110.,0.)
      CALL HBOOK1(IDOF+24,'ZZ event, NONresonating mass',50,0.,200.,0.)
      CALL HBOOK2(IDOF+25,'WW, one resonating mass against the other',
     *     40,60.,100.,40,60.,100.,0.)
      CALL HBOOK2(IDOF+26,'ZZ, one resonating mass against the other',
     *     40,60.,100.,40,60.,100.,0.)
C
      CALL HBOOK1(IDOF+31,'W+ cos[q] wrt beam',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+32,'W- cos[q] wrt beam',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+33,'W+ momentum (GeV)',40,0.,80.,0.)
      CALL HBOOK1(IDOF+34,'Z1 cos[q] wrt beam',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+35,'Z2 cos[q] wrt beam',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+36,'Z1 momentum (GeV)',40,0.,80.,0.)
C
      CALL HBOOK1(IDOF+41,'f1 cos[a] in W+ rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+42,'f2 cos[a] in W+ rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+43,'f3 cos[a] in W- rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+44,'f4 cos[a] in W- rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+45,'f1 cos[a] in Z1 rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+46,'f2 cos[a] in Z1 rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+47,'f3 cos[a] in Z2 rest frame',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+48,'f4 cos[a] in Z2 rest frame',40,-1.,1.,0.)
      CALL HBPROF(IDOF+49,
     *  'mean cos[a]^2! (f1 in W+) vs. cos[q] (W+ wrt beam)',
     *  20,-1.,1.,-1.,1.,' ')
C
      CALL HBOOK1(IDOF+51,'f1 momentum (GeV)',50,0.,100.,0.)
      CALL HBOOK1(IDOF+52,'f2 momentum (GeV)',50,0.,100.,0.)
      CALL HBOOK1(IDOF+53,'f1 cos[q] wrt beam',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+54,'f2 cos[q] wrt beam',40,-1.,1.,0.)
      CALL HBOOK1(IDOF+55,'angle between f1,f2 (deg)',40,0.,200.,0.)
      CALL HBOOK1(IDOF+56,'ratio of momenta (f2/f1)',40,0.,4.,0.)
C
      CALL HBOOK1(IDOF+61,'N FSR photons per event',5,-0.5,4.5,0.)
      CALL HBOOK1(IDOF+62,'E each FSR photon (GeV)',20,0.,10.,0.)
      CALL HBOOK1(IDOF+63,'FSR origin (e=1,[m]=2,other=3)',
     *  5,-0.5,4.5,0.)
C
      CALL HBOOK1(IDOF+71,'N electrons (P"G#3)',6,-0.5,5.5,0.)
      CALL HBOOK1(IDOF+72,'N muons (P"G#3)',6,-0.5,5.5,0.)
      CALL HBOOK1(IDOF+73,'N neutrinos',11,-0.5,11.5,0.)
      CALL HBOOK1(IDOF+74,'N charged stable particles (P?PERP!"G#0.5)',
     *            60,0.,60.,0.)
      CALL HBOOK1(IDOF+75,'N photons (E"G#0.5)',60,0.,60.,0.)
      CALL HBOOK1(IDOF+76,'E all neutrinos',40,0.,200.,0.)
      CALL HBOOK1(IDOF+77,'E all detectable photons',40,0.,160.,0.)
      CALL HBOOK1(IDOF+78,'E all detectable charged particles',
     *            50,0.,200.,0.)
      CALL HBOOK1(IDOF+79,
     *  'E all detectable photons + charged particles',
     *  50,0.,200.,0.)
C
      CALL HLDIR('//PAWC',' ')
      RETURN
      END
      SUBROUTINE WWHISA(QF1,QF2,QF3,QF4,QB1,QB2)
C=======================================================================
C
C WW analysis: fill diagnostic histograms
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
CC      INCLUDE 'LEPWW.INC'
C........................................
C
C LEPWW.INC
C
      INTEGER LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK
      INTEGER IFL1(2),IFL2(2),IFK(10),BK(10)
      LOGICAL NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      REAL    JSCOD0,JSCOD1
      REAL    SDVRT,VERTEX,XSECT
      DOUBLE PRECISION ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW
      DOUBLE PRECISION SIGWW,WWWSUM,WWWERR,WWWMAX
      DOUBLE PRECISION SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,WMAX,WLARGE
      DOUBLE PRECISION TOTSUM,TOTERR
      DOUBLE PRECISION XISR1,XISR2
      DOUBLE PRECISION CFACT,COULF
      DOUBLE PRECISION FLMAS1,FLMAS2,FLMASS
      DOUBLE PRECISION V,A,B,XM2,XMG
      DOUBLE PRECISION XXS
      DOUBLE PRECISION P1,P2,Q1,Q2,Q3,Q4,REALP,BOS1,BOS2
      DOUBLE PRECISION WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM
      DOUBLE PRECISION W0,W1,W2
      DOUBLE PRECISION ALF(2),PIJ
      INTEGER BRSTW(9),BRSTZ(12),NWW,NZZ
      INTEGER IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG
      INTEGER UWFLAG
      DOUBLE PRECISION WWUSER,ZWUSER
C
      COMMON / LEPWWI / LUOUTP,IDOF,NLEPWW,NTRIAL,NUPDAT,IROUTN,IPEAK,
     * IFL1,IFL2,
     * IRFLAG,CSFLAG,BWFLAG,ASFLAG,HIFLAG,FRFLAG,IZFLAG,ILFLAG,UWFLAG,
     * NOTOPQ,WINTFR,ZINTFR,WWPOSS,ZZPOSS
      COMMON / LEPWWF / JSCOD0,JSCOD1,SDVRT(3),VERTEX(4),XSECT
      COMMON / LEPWIN / WW0SUM,WW1SUM,WW2SUM,ZZ0SUM,ZZ1SUM,ZZ2SUM,
     * W0,W1,W2
      COMMON / WZBRS  / BRSTW,BRSTZ,NWW,NZZ
C
      COMMON / KOPLIN / ALFA,SIN2W,ALFAS,GF,XMZ,XMW,XMT,ECM,AZ,AW,
     * SIGWW,WWWSUM,WWWERR,WWWMAX,SIGZZ,ZZWSUM,ZZWERR,ZZWMAX,
     * TOTSUM,TOTERR,CFACT,FLMASS(12),
     * FLMAS1(2),FLMAS2(2),WMAX,WLARGE,COULF,
     * ALF,PIJ,
     * XISR1,XISR2,
     * WWUSER,ZWUSER
      COMMON / KOPLUT / V(4,5),A(4,5),B(5,5,5),XM2(5),XMG(5)
      COMMON / FLAKIN / IFK,BK
      COMMON / PARTIS / XXS(12)
      COMMON / MOMENZ / P1(4),P2(4),Q1(4),Q2(4),Q3(4),Q4(4),REALP(5,4),
     * BOS1(5),BOS2(5)
C........................................
CC      INCLUDE 'LUN7COM.INC'
C............................................................
C LUND common block
C
      INTEGER JN7LU,K7LU,LJNPAR
      REAL    P7LU,V7LU
      PARAMETER( LJNPAR= 4000)
      COMMON /LUJETS/ JN7LU,K7LU(LJNPAR,5),P7LU(LJNPAR,5),
     *                V7LU(LJNPAR,5)
C
      INTEGER MSTJ,MSTU
      REAL    PARJ,PARU
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C
      INTEGER KCHG
      REAL    PMAS,PARF,VCKM
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C............................................................
CC      INCLUDE 'HEPEVT.INC'
C............................................................
C Standard HEPEVT common block
C
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      INTEGER NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL    PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     * JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C............................................................
C
      REAL    ROOTS,EPHO,PZ
      DOUBLE PRECISION QF1(5),QF2(5),QF3(5),QF4(5),QB1(5),QB2(5)
      DOUBLE PRECISION XM,MX1,MX2
      REAL    CTHB1,CTHB2
      DOUBLE PRECISION PB1,PB2,GAM,BET,E01,E02,P01,P02,PL1,PL2
      DOUBLE PRECISION CTHF1,CTHF2,PF1,PF2,CTH,RAT,ANG,PI
      INTEGER I,NPH,KF1,KF2
      INTEGER NGAM,NCH,NEL,NMU,NNU,KS,KF
      REAL    EGAM,ENU,ECH,PPER,P,CT
      EXTERNAL XM
      SAVE PI
      DATA PI / 0.D0 /
C
      IF (PI.LT.3.D0) PI = DACOS(-1.D0)
C
C generator performance
C
      CALL HF1(IDOF+1,FLOAT(NTRIAL),1.)
      CALL HF1(IDOF+2,FLOAT(IROUTN),1.)
      IF (IROUTN.EQ.1) THEN
        IF (ZINTFR) THEN
          CALL HF1(IDOF+6,1.,1.)
        ELSE
          CALL HF1(IDOF+6,2.,1.)
        ENDIF
      ELSEIF (IROUTN.EQ.2) THEN
        IF (ZINTFR) THEN
          CALL HF1(IDOF+7,1.,1.)
        ELSE
          CALL HF1(IDOF+7,2.,1.)
        ENDIF
      ENDIF
      CALL HF1(IDOF+8,FLOAT(IPEAK),1.)
C
C ISR
C
      ROOTS = ECM*SNGL(DSQRT(XISR1*XISR2))
      CALL HF1(IDOF+11,ROOTS,1.)
      EPHO = 0.5*SNGL(ECM*(1.D0-XISR1))
      CALL HF1(IDOF+12,EPHO,1.)
      EPHO = 0.5*SNGL(ECM*(1.D0-XISR2))
      CALL HF1(IDOF+12,EPHO,1.)
      EPHO = 0.5*SNGL(ECM*(1.D0-XISR1*XISR2))
      CALL HF1(IDOF+13,EPHO,1.)
      PZ = 0.5*SNGL(ECM*(XISR1-XISR2))
      CALL HF1(IDOF+14,ABS(PZ),1.)
C
C primary vertex
C
      CALL HF1(IDOF+15,VERTEX(1),1.)
      CALL HF1(IDOF+16,VERTEX(2),1.)
      CALL HF1(IDOF+17,VERTEX(3),1.)
C
C boson masses
C
      IF (IROUTN.EQ.1) THEN
        CALL HF1(IDOF+21,SNGL(QB1(5)),1.)
        CALL HF1(IDOF+21,SNGL(QB2(5)),1.)
        MX1 = XM(QF1,QF3)
        MX2 = XM(QF2,QF4)
        CALL HF1(IDOF+22,SNGL(MX1),1.)
        CALL HF1(IDOF+22,SNGL(MX2),1.)
        CALL HF2(IDOF+25,SNGL(QB2(5)),SNGL(QB1(5)),1.)
      ELSEIF (IROUTN.EQ.2) THEN
        CALL HF1(IDOF+23,SNGL(QB1(5)),1.)
        CALL HF1(IDOF+23,SNGL(QB2(5)),1.)
        MX1 = XM(QF1,QF3)
        MX2 = XM(QF2,QF4)
        CALL HF1(IDOF+24,SNGL(MX1),1.)
        CALL HF1(IDOF+24,SNGL(MX2),1.)
        CALL HF2(IDOF+26,SNGL(QB2(5)),SNGL(QB1(5)),1.)
      ENDIF
C
C boson angular distributions
C
      PB1 = DSQRT(QB1(1)**2+QB1(2)**2+QB1(3)**2)
      PZ = SNGL(QB1(3))
      IF (PB1.GT.0.D0) THEN
        CTHB1 = PZ/SNGL(PB1)
      ELSE
        CTHB1 = -2.0
      ENDIF
      PB2 = DSQRT(QB2(1)**2+QB2(2)**2+QB2(3)**2)
      PZ = SNGL(QB2(3))
      IF (PB2.GT.0.D0) THEN
        CTHB2 = PZ/SNGL(PB2)
      ELSE
        CTHB2 = -2.0
      ENDIF
      IF (IROUTN.EQ.1) THEN
        CALL HF1(IDOF+31,CTHB1,1.)
        CALL HF1(IDOF+32,CTHB2,1.)
        CALL HF1(IDOF+33,SNGL(PB1),1.)
      ELSEIF (IROUTN.EQ.2) THEN
        CALL HF1(IDOF+34,CTHB1,1.)
        CALL HF1(IDOF+35,CTHB2,1.)
        CALL HF1(IDOF+36,SNGL(PB1),1.)
      ENDIF
C
C fermion angle in boson rest frame
C
      GAM = QB1(4)/QB1(5)
      BET = DSQRT((GAM+1.D0)*(GAM-1.D0))/GAM
      E01 = 0.5D0*(QB1(5)**2+QF1(5)**2-QF2(5)**2)/QB1(5)
      E02 = 0.5D0*(QB1(5)**2+QF2(5)**2-QF1(5)**2)/QB1(5)
      P01 = DSQRT(E01**2-QF1(5)**2)
      P02 = DSQRT(E02**2-QF2(5)**2)
      PL1 = (QF1(1)*QB1(1)+QF1(2)*QB1(2)+QF1(3)*QB1(3))/PB1
      PL2 = (QF2(1)*QB1(1)+QF2(2)*QB1(2)+QF2(3)*QB1(3))/PB1
      CTHF1 = ((PL1/GAM)-BET*E01)/P01
      CTHF2 = ((PL2/GAM)-BET*E02)/P02
      IF (IROUTN.EQ.1) THEN
        CALL HF1(IDOF+41,SNGL(CTHF1),1.)
        CALL HF1(IDOF+42,SNGL(CTHF2),1.)
        CALL HFILL(IDOF+49,CTHB1,SNGL(CTHF1**2),1.)
      ELSEIF (IROUTN.EQ.2) THEN
        CALL HF1(IDOF+45,SNGL(CTHF1),1.)
        CALL HF1(IDOF+46,SNGL(CTHF2),1.)
      ENDIF
C
      GAM = QB2(4)/QB2(5)
      BET = DSQRT((GAM+1.D0)*(GAM-1.D0))/GAM
      E01 = 0.5D0*(QB2(5)**2+QF3(5)**2-QF4(5)**2)/QB2(5)
      E02 = 0.5D0*(QB2(5)**2+QF4(5)**2-QF3(5)**2)/QB2(5)
      P01 = DSQRT(E01**2-QF3(5)**2)
      P02 = DSQRT(E02**2-QF4(5)**2)
      PL1 = (QF3(1)*QB2(1)+QF3(2)*QB2(2)+QF3(3)*QB2(3))/PB2
      PL2 = (QF4(1)*QB2(1)+QF4(2)*QB2(2)+QF4(3)*QB2(3))/PB2
      CTHF1 = ((PL1/GAM)-BET*E01)/P01
      CTHF2 = ((PL2/GAM)-BET*E02)/P02
      IF (IROUTN.EQ.1) THEN
        CALL HF1(IDOF+43,SNGL(CTHF1),1.)
        CALL HF1(IDOF+44,SNGL(CTHF2),1.)
      ELSEIF (IROUTN.EQ.2) THEN
        CALL HF1(IDOF+47,SNGL(CTHF1),1.)
        CALL HF1(IDOF+48,SNGL(CTHF2),1.)
      ENDIF
C
C fermions in the lab frame
C
      PF1 = DSQRT(QF1(1)**2+QF1(2)**2+QF1(3)**2)
      PF2 = DSQRT(QF2(1)**2+QF2(2)**2+QF2(3)**2)
      IF ((PF1.GT.0.D0).AND.(PF2.GT.0.D0)) THEN
        CTH = (QF1(1)*QF2(1)+QF1(2)*QF2(2)+QF1(3)*QF2(3))/(PF1*PF2)
        CTHF1 = QF1(3)/PF1
        CTHF2 = QF2(3)/PF2
        RAT = PF2/PF1
      ELSE
        CTH = -2.D0
        CTHF1 = -2.D0
        CTHF2 = -2.D0
        RAT = -2.D0
      ENDIF
      IF (DABS(CTH).LT.1.D0) THEN
        ANG = DACOS(CTH)*180.0D0/PI
      ELSE
        ANG = -10.D0
      ENDIF
      CALL HF1(IDOF+51,SNGL(PF1),1.)
      CALL HF1(IDOF+52,SNGL(PF2),1.)
      CALL HF1(IDOF+53,SNGL(CTHF1),1.)
      CALL HF1(IDOF+54,SNGL(CTHF2),1.)
      CALL HF1(IDOF+55,SNGL(ANG),1.)
      CALL HF1(IDOF+56,SNGL(RAT),1.)
C
CC      PF1 = DSQRT(QF3(1)**2+QF3(2)**2+QF3(3)**2)
CC      PF2 = DSQRT(QF4(1)**2+QF4(2)**2+QF4(3)**2)
CC      IF ((PF1.GT.0.D0).AND.(PF2.GT.0.D0)) THEN
CC        CTH = (QF3(1)*QF4(1)+QF3(2)*QF4(2)+QF3(3)*QF4(3))/(PF1*PF2)
CC        CTHF1 = QF3(3)/PF1
CC        CTHF2 = QF4(3)/PF2
CC        RAT = PF2/PF1
CC      ELSE
CC        CTH = -2.D0
CC        CTHF1 = -2.D0
CC        CTHF2 = -2.D0
CC        RAT = -2.D0
CC      ENDIF
CC      IF (DABS(CTH).LT.1.D0) THEN
CC        ANG = DACOS(CTH)*180.0D0/PI
CC      ELSE
CC        ANG = -10.D0
CC      ENDIF
CC      CALL HF1(IDOF+51,SNGL(PF1),1.)
CC      CALL HF1(IDOF+52,SNGL(PF2),1.)
CC      CALL HF1(IDOF+53,SNGL(CTHF1),1.)
CC      CALL HF1(IDOF+54,SNGL(CTHF2),1.)
CC      CALL HF1(IDOF+55,SNGL(ANG),1.)
CC      CALL HF1(IDOF+56,SNGL(RAT),1.)
C
C FSR photons
C
      NPH = 0
      DO I = 2, NHEP
        KF1 = IABS(IDHEP(I))
        IF (KF1.EQ.22) THEN
          NPH = NPH + 1
          EPHO = PHEP(4,I)
          CALL HF1(IDOF+62,EPHO,1.)
          KF2 = IABS(IDHEP(I-1))
          IF ((KF2.EQ.11).OR.(KF2.EQ.12)) THEN
            CALL HF1(IDOF+63,1.,1.)
          ELSEIF ((KF2.EQ.13).OR.(KF2.EQ.14)) THEN
            CALL HF1(IDOF+63,2.,1.)
          ELSE
            CALL HF1(IDOF+63,3.,1.)
          ENDIF
        ENDIF
      ENDDO
      CALL HF1(IDOF+61,FLOAT(NPH),1.)
C
C final-state particles
C
      NGAM = 0
      NCH = 0
      NEL = 0
      NMU = 0
      NNU = 0
      EGAM = 0.
      ENU = 0.
      ECH = 0.
      DO I = 1, JN7LU
        KS = K7LU(I,1)
        IF ((KS.GT.0).AND.(KS.LT.10)) THEN
          KF = IABS(K7LU(I,2))
          PPER = SQRT(P7LU(I,1)**2+P7LU(I,2)**2)
          P = SQRT(PPER**2+P7LU(I,3)**2)
          IF (P.GT.0.001) THEN
            CT = ABS(P7LU(I,3)/P)
          ELSE
            CT = -2.
          ENDIF
          IF (KF.EQ.11) THEN
            IF ((P.GT.3.0).AND.(CT.LT.0.9)) NEL = NEL + 1
          ELSEIF (KF.EQ.13) THEN
            IF ((P.GT.3.0).AND.(CT.LT.0.9)) NMU = NMU + 1
          ELSEIF (KF.EQ.22) THEN
            IF ((P.GT.0.25).AND.(CT.LT.0.95)) THEN
              NGAM = NGAM + 1
              EGAM = EGAM + P
            ENDIF
          ELSEIF ((KF.EQ.12).OR.(KF.EQ.14).OR.(KF.EQ.16)) THEN
            NNU = NNU + 1
            ENU = ENU + P
          ENDIF
          IF ((PPER.GT.0.5).AND.(CT.LT.0.9)) THEN
            IF ((KF.EQ.11).OR.(KF.EQ.12).OR.
     *          (KF.EQ.211).OR.(KF.EQ.321).OR.(KF.EQ.2212)) THEN
              NCH = NCH + 1
              ECH = ECH + P7LU(I,4)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      CALL HF1(IDOF+71,FLOAT(NEL),1.)
      CALL HF1(IDOF+72,FLOAT(NMU),1.)
      CALL HF1(IDOF+73,FLOAT(NNU),1.)
      CALL HF1(IDOF+74,FLOAT(NCH),1.)
      CALL HF1(IDOF+75,FLOAT(NGAM),1.)
      IF (NNU.GT.0)  CALL HF1(IDOF+76,ENU,1.)
      IF (NGAM.GT.0) CALL HF1(IDOF+77,EGAM,1.)
      IF (NCH.GT.0)  CALL HF1(IDOF+78,ECH,1.)
      IF (NGAM+NCH.GT.0) CALL HF1(IDOF+79,EGAM+ECH,1.)
C
      RETURN
      END
      SUBROUTINE WWHISF
C=======================================================================
C
C Manipulate diagnostic histograms for LEPWW MC event generator.
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      RETURN
      END
