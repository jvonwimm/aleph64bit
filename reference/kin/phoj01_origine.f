C
C Modifications made by AJ FINCH to original
C
C renamed SUBROUTINE D106BD to PHD106
c         SUBROUTINE GSET   to PHGSET
c         SUBROUTINE MINSQ  to PHMINSQ
c         SUBROUTINE FCN1   to PHFCN1
c         SUBROUTINE VD01A  to PHVD01A 
c         SUBROUTINE BESJ0  to PHBESJ0
C
C added....
C
c      double precision function drndm(dummy)
c      DOUBLE PRECISION DUMMY
c      drndm = sngl(rndm(sngl(dummy)))
c      return
c      end
c    BBL : this looks very strange .....
C    I change it to 
c      double precision function drndm(dummy)
c      double PRECISION DUMMY
c      real*4 rndm
c      drndm = dble(rndm(sngl(dummy)))
c      return
C      end
c removed...
c
c      SUBROUTINE CFILL
c      SUBROUTINE CLTOU 
c      SUBROUTINE UFILL
c      SUBROUTINE UHTOC
c      FUNCTION GAUSS
c      REAL FUNCTION GAMMA
c      FUNCTION FINT
c      DOUBLE PRECISION FUNCTION DGAMMA
c
c


      SUBROUTINE PODIFF(IDIF1,IDIF2,IMOTH1,IMOTH2,MSOFT,MHARD,
     &                  IMODE,IREJ)
C**********************************************************************
C
C     preparation of common HEPEVS for diffractive chains
C
C     input:   IDIF1  0   quasi elastic scattering of particle 1
C                     1   diffractive dissociation of particle 1
C
C              IDIF2  0   quasi elastic scattering of particle 2
C                     1   diffractive dissociation of particle 2
C
C              IMOTH1/2   index of mother particles in COMMON HEPEVS
C
C              IMODE   0  sampling of diffractive cut
C                      1  sampling of enhanced cut
C                      2  sampling of diffractive cut without
C                         scattering (needed for double-pomeron)
C                     -1  initialization
C                     -2  output of statistics
C
C     output:   MSOFT     number of generated soft chains
C               MHARD     number of generated hard chains
C               IDIF1/2  diffraction label for particle 1/2
C                      0   quasi elastic scattering
C                      1   low-mass diffractive dissociation
C                      2   soft high-mass diffractive dissociation
C                      3   hard resolved diffractive dissociation
C                      4   hard direct diffractive dissociation
C               IREJ   0  successful generation of partons
C                      1  generation failure
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(EPS  = 1.D-7,
     &          IONE = 1,
     &          ONE  = 1.D0,
     &          TWO  = 2.D0,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          DEPS = 1.D-10,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C  CM system of diffraction
      COMMON /DIFCMS/ ECMD,PCMD,PMASSD(2),PVIRTD(2),GAMBED(4),
     &                SIDD,CODD,SIFD,COFD,PDCMS(4,2),NPOSD(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (IEETAB=10)
      COMMON /XSETAB/ SIGTAB(4,70,IEETAB),SIGECM(4,IEETAB),ISIMAX
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DIMENSION P1(7),P2(7),XMASS(2),AMP(2),PP(7,2)
      DIMENSION IDPDG(2),IDBAM(2),IPAR(2),IPOSP(2,2),IRPDG(2),IVEC(2),
     &  IRBAM(2),IFL1P(2),IFL2P(2),ISAM(2),JSAM(2),KSAM(2),LSAM(2),
     &  MSAM(2),IDIR(2)
*     CHARACTER*79 TITLE
*     DIMENSION XLIMB(2)
C
C  initialization/ output of statistics
      IF(IMODE.EQ.-1) THEN
*       CALL NEWHIS(0.D0,15.D0,0.D0,XLIMB,49,IM1)
*       CALL NEWHIS(0.D0,15.D0,0.D0,XLIMB,49,IM2)
        RETURN
      ELSE IF(IMODE.EQ.-2) THEN
*       TITLE = 'M1*DN/DM1  MASS DISTRIBUTION PART.1 DIFFRACTION'
*       CALL OUTHIS(IM1,1,TITLE,1.D0,1)
*       TITLE = 'M2*DN/DM2  MASS DISTRIBUTION PART.2 DIFFRACTION'
*       CALL OUTHIS(IM2,1,TITLE,1.D0,1)
        RETURN
      ENDIF
      IREJ = 0
C  critical/supercritical Pomeron assumed
      ISCP = ISWMDL(22)
C  mass cuts
      PIMASS  = 0.140D0
C  debug output
      IF(IDEB(45).GE.10) THEN
        WRITE(6,'(1X,A,/5X,7I4)')
     &   'PODIFF:DEBUG(1):IDIF1,IDIF2,IMOTH1,IMOTH2,MSOFT,MHARD,IMODE',
     &    IDIF1,IDIF2,IMOTH1,IMOTH2,MSOFT,MHARD,IMODE
      ENDIF
      IPAR(1) = IDIF1
      IPAR(2) = IDIF2
C  range of T
      TMIN = -1D-08
      TMAX = -MAX(ONE/PIMASS**2,6.D0)
C  save chain numbers
      MSOFT = 0
      MHARD = 0
      KHPOMS = KHPOM
      KSPOMS = KSPOM
      KSREGS = KSREG
      KHDIRS = KHDIR
      JDA11 = JDAHEP(1,IMOTH1)
      JDA21 = JDAHEP(2,IMOTH1)
      JDA12 = JDAHEP(1,IMOTH2)
      JDA22 = JDAHEP(2,IMOTH2)
      ISTH1 = ISTHEP(IMOTH1)
      ISTH2 = ISTHEP(IMOTH2)
C  get mother data
      NPOSD(1) = IMOTH1
      NPOSD(2) = IMOTH2
      DO 20 I=1,2
        IDPDG(I) = IDHEP(NPOSD(I))
        IDBAM(I) = IMPART(NPOSD(I))
        AMP(I) = POPAMA(IDBAM(I),0)
        IF(IDPDG(I).EQ.22) THEN
          PMASSD(I) = 0.765D0
          PVIRTD(I) = PHEP(5,NPOSD(I))**2
        ELSE
          PMASSD(I) = POPAMA(IDBAM(I),0)
          PVIRTD(I) = ZERO
        ENDIF
 20   CONTINUE
C  get CM system
      P1(1) = PHEP(1,IMOTH1)+PHEP(1,IMOTH2)
      P1(2) = PHEP(2,IMOTH1)+PHEP(2,IMOTH2)
      P1(3) = PHEP(3,IMOTH1)+PHEP(3,IMOTH2)
      P1(4) = PHEP(4,IMOTH1)+PHEP(4,IMOTH2)
      SS = P1(4)**2-P1(1)**2-P1(2)**2-P1(3)**2
      ECMD = SQRT(SS)
      IF(IDEB(45).GE.15) WRITE(6,'(1X,A,E12.4)')
     &  'PODIFF:DEBUG:AVAILABE ENERGY',ECMD
C  check total available energy
      IF((AMP(1)+AMP(2)+0.1D0).GE.ECMD) THEN
        IFAIL(7) = IFAIL(7)+1
        IREJ = 1
        RETURN
      ENDIF
C  boost into CMS
      DO 10 I=1,4
        GAMBED(I) = P1(I)/ECMD
 10   CONTINUE
      CALL ALTRA(GAMBED(4),-GAMBED(1),-GAMBED(2),-GAMBED(3),
     &           PHEP(1,IMOTH1),PHEP(2,IMOTH1),PHEP(3,IMOTH1),
     &           PHEP(4,IMOTH1),PTOT1,P1(1),P1(2),P1(3),P1(4))
C  rotation angles
      CODD = P1(3)/PTOT1
      SIDD = SQRT((ONE-CODD)*(ONE+CODD))
      COFD = ONE
      SIFD = ZERO
      IF(PTOT1*SIDD.GT.DEPS) THEN
        COFD = P1(1)/(SIDD*PTOT1)
        SIFD = P1(2)/(SIDD*PTOT1)
        ANORF= SQRT(COFD*COFD+SIFD*SIFD)
        COFD = COFD/ANORF
        SIFD = SIFD/ANORF
      ENDIF
C  initial particles in CMS
      PDCMS(1,1) = ZERO
      PDCMS(2,1) = ZERO
      PDCMS(3,1) = PTOT1
      PDCMS(4,1) = P1(4)
      PDCMS(1,2) = ZERO
      PDCMS(2,2) = ZERO
      PDCMS(3,2) = -PTOT1
      PDCMS(4,2) = ECMD-P1(4)
C  get new CM momentum
      AM12 = PMASSD(1)**2
      AM22 = PMASSD(2)**2
      PCMD = XLAMB(SS,AM12,AM22)/(2.D0*ECMD)
C
C  coherence constraint (max diffractive mass allowed)
      IF(IMODE.EQ.2) THEN
        THRM1 = PARMDL(71)/SQRT(1-PARMDL(72))
        THRM1 = MAX(THRM1,PARMDL(70)*PARMDL(71))
        THRM2 = SQRT(1-PARMDL(72))*ECMD
        THRM2 = MIN(THRM2,ECMD/PARMDL(70))
      ELSE
        THRM1 = PARMDL(46)
        THRM2 = PARMDL(45)*ECMD
C  check kinematic limits
        IF(THRM2.LE.(4.D0*CUTMU(2))) THEN
          IPAR(1) = MIN(IPAR(1),1)
          IPAR(2) = MIN(IPAR(2),1)
        ENDIF
      ENDIF
C  check energy vs. coherence constraints
      IF(MAX(CUTMU(2),PMASSD(1)+THRM1).GE.THRM2) IPAR(1) = 0
      IF(MAX(CUTMU(3),PMASSD(2)+THRM1).GE.THRM2) IPAR(2) = 0
      IF(IPAR(1)+IPAR(2).EQ.0) THEN
        IFAIL(7) = IFAIL(7)+1
        IREJ = 1
        RETURN
      ENDIF
C
      ITRY = 0
      ITRYM = 10
      NSLP = 0
      NHEPS = NHEP
      IPARS1 = IPAR(1)
      IPARS2 = IPAR(2)
C  main rejection loop
C -------------------------------
 50   CONTINUE
      ITRY = ITRY+1
      IF(ITRY.GT.1) THEN
        IFAIL(13) = IFAIL(13)+1
        IF(ITRY.GE.ITRYM) THEN
          IFAIL(7) = IFAIL(7)+1
          IREJ = 1
          RETURN
        ENDIF
      ENDIF
      KSPOM = KSPOMS
      KHPOM = KHPOMS
      KHDIR = KHDIRS
      KSREG = KSREGS
      IPAR(1) = IPARS1
      IPAR(2) = IPARS2
C  reset mother-daugther relations
      NHEP = NHEPS
      JDAHEP(1,IMOTH1) = JDA11
      JDAHEP(2,IMOTH1) = JDA21
      JDAHEP(1,IMOTH2) = JDA12
      JDAHEP(2,IMOTH2) = JDA22
      ISTHEP(IMOTH1) = ISTH1
      ISTHEP(IMOTH2) = ISTH2
C
C  main loop
      DO 100 I=1,2
C  sampling of new masses
        IRPDG(I) = 0
        IRBAM(I) = 0
        IFL1P(I) = IDPDG(I)
        IFL2P(I) = IDBAM(I)
        IF(IPAR(I).EQ.0) THEN
C  vector meson dominance assumed
          XMASS(I) = AMP(I)
          CALL VECRES(IVEC(I),XMASS(I),IFL1P(I),IFL2P(I))
C  diffractive dissociation
        ELSE IF(IPAR(I).LE.4) THEN
          XMMIN = MAX(CUTMU(I+1),PMASSD(I)+THRM1)
          PREF2 = PMASSD(I)**2+PVIRTD(I)
          XMASS(I) = XDMASS(XMMIN,THRM2,PREF2,ISCP)
          IF(IMODE.EQ.2) GOTO 130
          IP = I+1
C  get cross sections
          IF(XMASS(I).LE.SIGECM(IP,1)) THEN
            IN1 = 1
            IN2 = 1
          ELSE IF(XMASS(I).LT.SIGECM(IP,ISIMAX)) THEN
            DO 55 II=2,ISIMAX
              IF(XMASS(I).LE.SIGECM(IP,II)) GOTO 210
 55         CONTINUE
 210        CONTINUE
            IN1 = II-1
            IN2 = II
          ELSE
            WRITE(6,'(/1X,A,2E12.3)')
     &      'PODIFF:WARNING:TOO HIGH XM',XMASS(I),SIGECM(IP,ISIMAX)
            IN1 = ISIMAX
            IN2 = ISIMAX
          ENDIF
          FAC2=0.D0
          IF(IN1.NE.IN2) FAC2=LOG(XMASS(I)/SIGECM(IP,IN1))
     &                        /LOG(SIGECM(IP,IN2)/SIGECM(IP,IN1))
          FAC1=1.D0-FAC2
          STOT = FAC2*SIGTAB(IP,1,IN2)+FAC1*SIGTAB(IP,1,IN1)
          SELA = FAC2*SIGTAB(IP,2,IN2)+FAC1*SIGTAB(IP,2,IN1)
          SINE = STOT-SELA
          SDIR = FAC2*SIGTAB(IP,29,IN2)+FAC1*SIGTAB(IP,29,IN1)
          SDIR = MAX(ZERO,SDIR)/SINE
C  sample interaction
          IF((DRNDM(FAC1).LT.SDIR).AND.(IPRON(8+I).NE.0)) THEN
            IDIR(I) = 1
            ISAM(I) = 0
            JSAM(I) = 0
            KSAM(I) = 0
            LSAM(I) = 0
            MSAM(I) = 0
          ELSE
            CALL SAMPRB(XMASS(I),IP,ISAM(I),JSAM(I),KSAM(I),LSAM(I),
     &        MSAM(I))
          ENDIF
C  suppress hard diffraction
          IF(IPRON(8+I).EQ.0) KSAM(I) = 0
C  suppress multiple interaction
          IF(ISWMDL(16).EQ.0) THEN
            KSAM(I) = 0
            IDIR(I) = 0
            IF(ISAM(I)+KSAM(I).GT.0) THEN
              ISAM(I) = 1
              JSAM(I) = 0
            ELSE
              JSAM(I) = 1
            ENDIF
          ELSE IF(ISWMDL(16).EQ.1) THEN
            IF(IDIR(I).GT.0) THEN
            ELSE IF(KSAM(I).GT.0) THEN
              KSAM(I) = 1
              ISAM(I) = 0
              JSAM(I) = 0
            ELSE IF(ISAM(I).GT.0) THEN
              ISAM(I) = 1
              JSAM(I) = 0
            ELSE
              JSAM(I) = 1
            ENDIF
          ELSE IF(ISWMDL(16).EQ.2) THEN
            KSAM(I) = MIN(KSAM(I),1)
          ELSE IF(ISWMDL(16).EQ.3) THEN
            IF(ISAM(I).GT.0) THEN
              ISAM(I) = 1
              JSAM(I) = 0
            ELSE
              JSAM(I) = 1
            ENDIF
          ENDIF
          IF(ISAM(I)+JSAM(I)+KSAM(I)+IDIR(I).EQ.0) JSAM(I) = 1
          IF(IDIR(I).GT.0) THEN
            IPAR(I) = 4
          ELSE IF(KSAM(I).GT.0) THEN
            IPAR(I) = 3
          ELSE IF(ISAM(I).GT.0) THEN
            IPAR(I) = 2
          ELSE
            IPAR(I) = 1
          ENDIF
C  mass fine correction
          IF(IPAR(I).EQ.1) THEN
            CALL MASSAD(IDPDG(I),IFL1P(I),IFL2P(I),PMASSD(1),
     &        XMASS(I),XMNEW,IRPDG(I),IRBAM(I))
            XMASS(I) = XMNEW
          ENDIF
C  debug output
          IF(IDEB(45).GE.15) WRITE(6,'(1X,A,/5X,I3,E12.4,4I5)')
     &      'PODIFF:DEBUG:IP,XMASS,ISAM,JSAM,KSAM,IDIR',
     &      IP,XMASS(I),ISAM(I),JSAM(I),KSAM(I),IDIR(I)
 130      CONTINUE
        ELSE
          WRITE(6,'(/1X,A,I3)') 'PODIFF:ERROR:INVALID IPAR',IPAR(I)
          CALL POABRT
        ENDIF
 100  CONTINUE
C
C  sampling of momentum transfer
      TT =  XSLOPE(TMIN,TMAX,XMASS(1),XMASS(2),IREJ)
      IF(IREJ.NE.0) THEN
        NSLP=NSLP+1
        IF(NSLP.LT.10) GOTO 50
        WRITE(6,'(1X,A,/10X,2I3,2E12.3)')
     &   'PODIFF:WARNING:TOO MANY SLOPE REJECTIONS:IPAR1,IPAR2,M1,M2',
     &   IPAR,XMASS
        CALL POABRT
      ENDIF
C  calculate new momenta in CMS
      CALL DIFKIN(XMASS(1),XMASS(2),TT,P1,P2,IREJ)
      IF(IREJ.NE.0) GOTO 50
      DO 120 I=1,4
        PP(I,1) = P1(I)
        PP(I,2) = P2(I)
 120  CONTINUE
C  debug output
      IF(IDEB(45).GE.5) THEN
        WRITE(6,'(1X,A,/5X,2I3,3E12.3)')
     &    'PODIFF:DEBUG:IPAR1,IPAR2,XMASS1,XMASS2,TT',IPAR,XMASS,TT
      ENDIF
C  actualize debug common
      IF(IMODE.EQ.1) THEN
        IDIFR1 = IPAR(1)
        IDIFR2 = IPAR(2)
      ENDIF
C  comment line for diffraction
      CALL REGPAR(30,IPROCE,IMODE,NPOSD(1),NPOSD(2),XMASS(1),XMASS(2),
     &   TT,ECMD,IPAR(1),IPAR(2),IDPDG(1),IDPDG(2),ICPOS,1)
C  write diffractive chains/particles
      DO 200 I=1,2
        I1 = I
        I2 = 3-I1
        PP(6,I1) = SIGN(PHEP(5,NPOSD(I1))**2,PHEP(5,NPOSD(I1)))
        PP(7,I1) = TT
        CALL PODREG(NPOSD(I1),NPOSD(I2),IFL1P(I1),IFL2P(I1),IPAR(I1),
     &    PP(1,I1),PP(1,I2),IPOSP(1,I1),IMODE,IREJ)
        IF(IREJ.NE.0) THEN
          IFAIL(7+I) = IFAIL(7+I)+1
          IF(IDEB(45).GE.3) WRITE(6,'(1X,A,2I3,E11.3)')
     &      'PODIFF:WARNING:REJECTION PODREG (I,IPAR,XM)',
     &      I,IPAR(I),XMASS(I)
          GOTO 50
        ENDIF
        ICOLOR(I1,ICPOS) = IPOSP(1,I1)
 200  CONTINUE
C  double-pomeron scattering?
      IF(IMODE.EQ.2) GOTO 150
C
C  diffractive final states
      DO 300 I=1,2
 110    CONTINUE
        IF(IPAR(I).EQ.0) THEN
C  vector meson production
          IF(IDPDG(I).EQ.22) THEN
            ISP = IPAMDL(3)
            IF(ISWMDL(21).GE.1) ISP = IPAMDL(4)
            CALL SDECAY(IPOSP(1,I),ISP,2)
          ENDIF
        ELSE
          IF(ISAM(I)+JSAM(I)+KSAM(I).EQ.0) JSAM(I) = 1
          IF(IDIR(I).GT.0) THEN
            IPAR(I) = 4
          ELSE IF(KSAM(I).GT.0) THEN
            IPAR(I) = 3
          ELSE IF(ISAM(I).GT.0) THEN
            IPAR(I) = 2
          ELSE
            IPAR(I) = 1
          ENDIF
          IPHIST(I,ICPOS) = IPAR(I)
C  update debug common
          KSPOM = ISAM(I)
          KSREG = JSAM(I)
          KHPOM = KSAM(I)
          KHDIR = IDIR(I)
          IDIFR1 = IPAR(1)
          IDIFR2 = IPAR(2)
          IF((IRPDG(I).NE.0).AND.(ISWMDL(23).GT.0)) THEN
C  resonance decay, pi+pi- background
            P1(1) = PHEP(1,IPOSP(1,I))+PHEP(1,IPOSP(2,I))
            P1(2) = PHEP(2,IPOSP(1,I))+PHEP(2,IPOSP(2,I))
            P1(3) = PHEP(3,IPOSP(1,I))+PHEP(3,IPOSP(2,I))
            P1(4) = PHEP(4,IPOSP(1,I))+PHEP(4,IPOSP(2,I))
            CALL REGPAR(1,IRPDG(I),IRBAM(I),IPOSP(1,I),IPOSP(2,I),
     &        P1(1),P1(2),P1(3),P1(4),0,0,0,0,IPOS,1)
            ISP = IPAMDL(3)
            IF((IDPDG(I).EQ.22).AND.(ISWMDL(21).EQ.2)) ISP = IPAMDL(4)
            CALL SDECAY(IPOS,ISP,2)
            IREJ = 0
          ELSE
C  particle-pomeron scattering
            NHEPS = NHEP
            CALL STDPAR(IPOSP(1,I),IPOSP(2,I),ISAM(I),JSAM(I),
     &        KSAM(I),IDIR(I),IREJ)
          ENDIF
        ENDIF
        IF(IREJ.NE.0) THEN
          IFAIL(20+I) = IFAIL(20+I)+1
          IF(IPAR(I).GT.1) THEN
            IF(IPAR(I).EQ.3) IFAIL(7+2*I) = IFAIL(7+2*I)+1
            IF(IPAR(I).EQ.4) IFAIL(8+2*I) = IFAIL(8+2*I)+1
            IF(IDIR(I).GT.0) THEN
              IDIR(I) = 0
            ELSE IF(KSAM(I).GT.0) THEN
              KSAM(I) = KSAM(I)-1
            ELSE IF(ISAM(I).GT.0) THEN
              ISAM(I) = ISAM(I)-1
            ENDIF
            NHEP = NHEPS
            JDAHEP(1,IPOSP(1,I)) = 0
            JDAHEP(2,IPOSP(1,I)) = 0
            JDAHEP(1,IPOSP(2,I)) = 0
            JDAHEP(2,IPOSP(2,I)) = 0
            GOTO 110
          ELSE
            IF(IDEB(45).GE.2) WRITE(6,'(1X,A,2I3,E11.3)')
     &        'PODIFF:WARNING:REJECTION STDPAR (I,IPAR,XM)',
     &        I,IPAR(I),XMASS(I)
            GOTO 50
          ENDIF
        ENDIF
 300  CONTINUE
C
*     IF(IPAR(1).GT.0) THEN
*       CALL FILHIS(XMASS(1),XMASS(1),IM1)
*       CALL ADDHIS(IM1)
*     ENDIF
*     IF(IPAR(2).GT.0) THEN
*       CALL FILHIS(XMASS(2),XMASS(2),IM2)
*       CALL ADDHIS(IM2)
*     ENDIF
      IDIF1 = IPAR(1)
      IDIF2 = IPAR(2)
C  update debug common
      KSPOM = KSPOMS+ISAM(1)+ISAM(2)
      KSREG = KSREGS+JSAM(1)+JSAM(2)
      KHPOM = KHPOMS+KSAM(1)+KSAM(2)
      KHDIR = KHDIRS+IDIR(1)+IDIR(2)
C
 150  CONTINUE
C  debug output
      IF(IDEB(45).GE.10) THEN
        WRITE(6,'(1X,A,/5X,7I4)')
     &   'PODIFF:DEBUG(2):IPAR1,IPAR2,IMOTH1,IMOTH2,MSOFT,MHARD,IMODE',
     &    IPAR,NPOSD,MSOFT,MHARD,IMODE
      ENDIF
      IF(IDEB(45).GE.15) THEN
        WRITE(6,'(2(/1X,A))') 'PODIFF:DEBUG:OUTPUT OF COMMON HEPEVS',
     &                        '===================================='
        CALL POPREV(0)
      ENDIF
      END
C
C
      SUBROUTINE PODREG(IMOTH1,IMOTH2,IFL1,IFL2,IPAR,P1,P2,IPOSH,IMODE,
     &                  IREJ)
C*********************************************************************
C
C     perform chain construction of diffractive dissociation
C
C     input:     IMOTH1,2     index of mother particles in HEPEVS
C                IFL1,IFL2    particle numbers
C                             (IDPDG,IDBAM for quasi-elas. hadron)
C                IPAR         0  quasi-elasic scattering
C                             1  single chain configuration
C                             2  two chain configuration
C                P1           massive 4 momentum of first
C                P1(6)        virtuality/squ.mass of particle (GeV**2)
C                P1(7)        virtuality of Pomeron (neg, GeV**2)
C                P2           massive 4 momentum of second particle
C                IMODE        1   diffraction dissociation
C                             2   double-pomeron scattering
C
C     output:    IPOSH(1-2)   index of the particles in COMMON HEPEVS
C                IREJ         0  successful chain construction
C                             1  no chain construction possible
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION IPOSH(2)
C
      PARAMETER(EPS  = 1.D-7,
     &          IONE = 1,
     &          ONE  = 1.D0,
     &          TWO  = 2.D0,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          DEPS = 1.D-10,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C  CM system of diffraction
      COMMON /DIFCMS/ ECMD,PCMD,PMASSD(2),PVIRTD(2),GAMBED(4),
     &                SIDD,CODD,SIFD,COFD,PDCMS(4,2),NPOSD(2)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DIMENSION P1(7),P2(7),PCH1(2,4)
C
      IREJ = 0
      ILTR1 = NHEP+1
C  elastic part
      IF(IPAR.EQ.0) THEN
        IF(IFL1.EQ.6666) THEN
C  pi+/pi- isotropic background
          CALL REGPAR(1,MPDGHA(IFL2),IFL2,IMOTH1,IMOTH2,P1(1),P1(2),
     &      P1(3),P1(4),0,0,0,0,IPOSH(1),1)
          CALL SDECAY(IPOSH(1),0,-2)
        ELSE
C  registration of resonance
          CALL REGPAR(1,IFL1,IFL2,IMOTH1,IMOTH2,P1(1),P1(2),P1(3),
     &      P1(4),0,0,0,0,IPOSH(1),1)
        ENDIF
C
C  diffractive dissociation
      ELSE IF((IPAR.GE.1).AND.(IPAR.LE.4)) THEN
C  calculation of resulting particle momenta
        IF(IMOTH1.EQ.NPOSD(1)) THEN
          K = 2
        ELSE
          K = 1
        ENDIF
        DO 100 I=1,4
          PCH1(2,I) = PDCMS(I,K)-P2(I)
          PCH1(1,I) = P1(I)-PCH1(2,I)
 100    CONTINUE
C  registration
        IF(IMODE.LT.2) CALL REGPAR(1,IFL1,IFL2,IMOTH1,IMOTH2,PCH1(1,1),
     &    PCH1(1,2),PCH1(1,3),PCH1(1,4),-1,0,ICA1,ICA2,IPOSH(1),1)
        CALL REGPAR(1,45,0,IMOTH2,IMOTH1,PCH1(2,1),PCH1(2,2),
     &              PCH1(2,3),PCH1(2,4),-1,0,ICB1,ICB2,IPOSH(2),1)
C  invalid IPAR
      ELSE
        WRITE(6,'(/1X,A,I6)') 'PODREG:ERROR:INVALID IPAR:',IPAR
        CALL POABRT
      ENDIF
C  back transformation
      CALL LTRHEP(ILTR1,NHEP,CODD,SIDD,COFD,SIFD,GAMBED(4),GAMBED(1),
     &            GAMBED(2),GAMBED(3))
      END
C
C
      SUBROUTINE QELAST(IPROC,JM1,JM2,IREJ)
C**********************************************************************
C
C     sampling of quasi elastic processes
C
C     input:   IPROC  2   purely elastic scattering
C              IPROC  3   q-ela. omega/omega/phi/pi+pi- production
C              IPROC  4   double pomeron scattering
C              IPROC  -1  initialization
C              IPROC  -2  output of statistics
C              JM1        index of initial particle 1
C              JM2        index of initial particle 2
C
C     output:  initial and final particles in COMMON HEPEVS involving
C              polarized resonances in COMMON HEPEVS and decay
C              products
C
C              IREJ    0  successful
C                      1  failure
C                     50  user rejection
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(NTAB = 20,
     &          EPS  = 1.D-10,
     &          IONE = 1,
     &          ONE  = 1.D0,
     &          PIMASS = 0.13D0,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          DEPS = 1.D-10,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  CM system of diffraction
      COMMON /DIFCMS/ ECMD,PCMD,PMASSD(2),PVIRTD(2),GAMBED(4),
     &                SIDD,CODD,SIFD,COFD,PDCMS(4,2),NPOSD(2)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  cross sections
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DIMENSION   P1(5),P(5,2),PMI(2),RMASS(2),IFL(2),IDPRO(4)
      CHARACTER*8 VMESA(0:4),VMESB(0:4),PANAME
      DIMENSION   ISAMVM(4,4)
      DATA IDPRO / 113,223,333,6666 /
      DATA VMESA / 'VMESON  ','RHO     ','OMEGA0  ','PHI     ',
     &             'PI+PI-  ' /
      DATA VMESB / 'VMESON  ','RHO     ','OMEGA0  ','PHI     ',
     &             'PI+PI-' /
C
C  sampling of quasi elastic processes
      IF((IPROC.EQ.2).OR.(IPROC.EQ.3)) THEN
        IREJ = 0
        NPOSD(1) = JM1
        NPOSD(2) = JM2
        DO 55 I=1,2
          PMI(I) = PHEP(5,NPOSD(I))
          IF(PMI(I).LT.0.1D0) PMI(I) = 0.765D0
 55     CONTINUE
C  get CM system
        P1(1) = PHEP(1,JM1)+PHEP(1,JM2)
        P1(2) = PHEP(2,JM1)+PHEP(2,JM2)
        P1(3) = PHEP(3,JM1)+PHEP(3,JM2)
        P1(4) = PHEP(4,JM1)+PHEP(4,JM2)
        SS = P1(4)**2-P1(1)**2-P1(2)**2-P1(3)**2
        ECMD = SQRT(SS)
C
        IF(ECMD.LE.PMI(1)+PMI(2)) THEN
          IF(IDEB(34).GE.3) WRITE(6,'(1X,A,I12,3E12.4)')
     &      'QELAST:WARNING:TOO SMALL MASS (EV,ECM,M1,M2)',KEVENT,
     &      ECMD,PMI
          IREJ = 5
          RETURN
        ENDIF
C
        DO 60 I=1,4
          GAMBED(I) = P1(I)/ECMD
 60     CONTINUE
        CALL ALTRA(GAMBED(4),-GAMBED(1),-GAMBED(2),-GAMBED(3),
     &           PHEP(1,NPOSD(1)),PHEP(2,NPOSD(1)),PHEP(3,NPOSD(1)),
     &           PHEP(4,NPOSD(1)),PTOT1,P1(1),P1(2),P1(3),P1(4))
C  rotation angles
        CODD = P1(3)/PTOT1
        SIDD = SQRT((ONE-CODD)*(ONE+CODD))
        COFD = ONE
        SIFD = ZERO
        IF(PTOT1*SIDD.GT.DEPS) THEN
          COFD = P1(1)/(SIDD*PTOT1)
          SIFD = P1(2)/(SIDD*PTOT1)
          ANORF= SQRT(COFD*COFD+SIFD*SIFD)
          COFD = COFD/ANORF
          SIFD = SIFD/ANORF
        ENDIF
C  get CM momentum
        AM12 = PMI(1)**2
        AM22 = PMI(2)**2
        PCMD = XLAMB(SS,AM12,AM22)/(2.D0*ECMD)
C
        ICALL = ICALL + 1
C  main rejection label
 50     CONTINUE
C  determine process and final particles
        IFL(1) = IDHEP(NPOSD(1))
        IFL(2) = IDHEP(NPOSD(2))
        IF(IPROC.EQ.3) THEN
          ITRY = 0
 100      CONTINUE
          ITRY = ITRY+1
          IF(ITRY.GT.50) THEN
            IF(IDEB(34).GE.3) WRITE(6,'(1X,A,I12,I5,E12.4)')
     &        'QELAST:WARNING:MASS REJECTION (EV,ITRY,ECM)',KEVENT,
     &        ITRY,ECMD
            IREJ = 5
            RETURN
          ENDIF
          XI = DRNDM(PCMD)*SIGVM(0,0)-DEPS
          DO 110 I=1,4
            DO 120 J=1,4
              XI = XI-SIGVM(I,J)
              IF(XI.LE.ZERO) GOTO 130
 120        CONTINUE
 110      CONTINUE
 130      CONTINUE
          IF(IFL(1).EQ.22) IFL(1) = IDPRO(I)
          IF(IFL(2).EQ.22) IFL(2) = IDPRO(J)
          ISAMVM(I,J) = ISAMVM(I,J)+1
          ISAMQE = ISAMQE+1
C  sample new masses
          CALL SAMASS(IFL(1),RMASS(1))
          CALL SAMASS(IFL(2),RMASS(2))
          IF(RMASS(1)+RMASS(2).GE.ECMD) GOTO 100
        ELSE IF(IPROC.EQ.2) THEN
          I = 0
          J = 0
          ISAMEL = ISAMEL+1
          RMASS(1) = POPAMA(NPOSD(1),2)
          RMASS(2) = POPAMA(NPOSD(2),2)
        ELSE
          WRITE(6,'(/1X,A,I6)') 'QELAST:ERROR:INVALID IPROC',IPROC
          CALL POABRT
        ENDIF
C  sample momentum transfer
        CALL SADSDT(I,J,TT)
        IF(IDEB(34).GE.5) WRITE(6,'(1X,A,2I6,I3,3E11.3)')
     &    'QELAST:DEBUG:IF1,2,T,RM1,RM2',IFL,IPROC,TT,RMASS
C  calculate new momenta
        CALL DIFKIN(RMASS(1),RMASS(2),TT,P(1,1),P(1,2),IREJ)
        IF(IREJ.NE.0) GOTO 50
C  comment line for elastic/quasi-elastic scattering
        CALL REGPAR(35,IPROC,0,NPOSD(1),NPOSD(2),RMASS(1),RMASS(2),TT,
     &    ECMD,IFL(1),IFL(2),IDHEP(NPOSD(1)),IDHEP(NPOSD(2)),ICPOS,1)
C
        I1 = NHEP+1
C  fill COMMON HEPEVS
        DO 200 I=1,2
          K = 3-I
          IF(IFL(I).EQ.6666) THEN
C  pi+/pi- isotropic background
            CALL REGPAR(1,113,33,NPOSD(I),NPOSD(K),P(1,I),P(2,I),
     &        P(3,I),P(4,I),0,0,0,0,IPOS,1)
            ICOLOR(I,ICPOS) = IPOS
            CALL SDECAY(IPOS,0,-2)
          ELSE
C  registration
            CALL REGPAR(1,IFL(I),0,NPOSD(I),NPOSD(K),P(1,I),P(2,I),
     &        P(3,I),P(4,I),0,0,0,0,IPOS,1)
            ICOLOR(I,ICPOS) = IPOS
          ENDIF
 200    CONTINUE
        I2 = NHEP
C  search for vector-mesons
        DO 300 I=I1,I2
C  decay according to polarization
          IF(IDHEP(JMOHEP(1,I)).EQ.22) THEN
            ISP = IPAMDL(3)
            IF(ISWMDL(21).GE.1) ISP = IPAMDL(4)
            CALL SDECAY(I,ISP,2)
          ENDIF
 300    CONTINUE
        I2 = NHEP
C  back transformation
        CALL LTRHEP(I1,I2,CODD,SIDD,COFD,SIFD,GAMBED(4),GAMBED(1),
     &              GAMBED(2),GAMBED(3))
C
C  initialization of tables
      ELSE IF(IPROC.EQ.-1) THEN
        WRITE(6,'(/,1X,A,/,1X,A)')
     &       'INITIALIZATION OF ELASTIC/QUASI-ELASTIC PROCESSES:',
     &       '=================================================='
        DO 10 I=1,4
          DO 20 J=1,4
            ISAMVM(I,J) = 0
 20       CONTINUE
 10     CONTINUE
        ISAMEL = 0
        ISAMQE = 0
        IF(IDHEP(JM1).NE.22) THEN
          VMESA(1) = PANAME(IMPART(JM1),0)
        ELSE
          VMESA(1) = 'RHO     '
        ENDIF
        IF(IDHEP(JM2).NE.22) THEN
          VMESB(1) = PANAME(IMPART(JM2),0)
        ELSE
          VMESB(1) = 'RHO     '
        ENDIF
        CALL SAMASS(-1,RMASS(1))
        CALL SADSDT(-1,0,TT)
        ICALL = 0
C
C  output of statistics
      ELSE IF(IPROC.EQ.-2) THEN
        WRITE(6,'(/,1X,A,I10/,1X,A)')
     &       'STATISTICS OF ELASTIC/QUASI-ELASTIC PROCESSES:',ICALL,
     &       '=============================================='
        IF(ICALL.LT.10) RETURN
        WRITE(6,'(1X,A,I10)')
     &    'SAMPLED ELASTIC PROCESSES:',ISAMEL
        WRITE(6,'(1X,A,I10)')
     &    'SAMPLED QUASI-ELASTIC VECTORMESON PRODUCTION:',ISAMQE
        WRITE(6,'(15X,4(4X,A8))') (VMESB(I),I=1,4)
        DO 30 I=1,4
          WRITE(6,'(5X,A8,4I12)') VMESA(I),(ISAMVM(I,K),K=1,4)
 30     CONTINUE
        CALL SAMASS(-2,RMASS(1))
        CALL SADSDT(-2,0,TT)
      ELSE
        WRITE(6,'(/1X,A,I3)') 'QELAST:ERROR:UNKNOWN PROCESS ',IPROC
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE PODBLE(IMOTH1,IMOTH2,MSOFT,MHARD,IMODE,IREJ)
C**********************************************************************
C
C     preparation of common HEPEVS for double-pomeron scattering
C
C     input:   IMOTH1/2   index of mother particles in COMMON HEPEVS
C
C              IMODE   1  sampling of pomeron-pomeron scattering
C                     -1  initialization
C                     -2  output of statistics
C
C     output:   MSOFT     number of generated soft chains
C               MHARD     number of generated hard chains
C               IREJ      0  accepted
C                         1  rejected
C                        50  user rejection
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER(NTAB = 20,
     &          EPS  = 1.D-10,
     &          IONE = 1,
     &          ONE  = 1.D0,
     &          PIMASS = 0.13D0,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          DEPS = 1.D-10,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (IEETAB=10)
      COMMON /XSETAB/ SIGTAB(4,70,IEETAB),SIGECM(4,IEETAB),ISIMAX
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DIMENSION PD(4)
C
      IREJ = 0
      IP = 4
C  select first diffraction
      IF(DRNDM(XI).GT.0.5D0) THEN
        IPAR1 = 1
        IPAR2 = 0
      ELSE
        IPAR1 = 0
        IPAR2 = 1
      ENDIF
      NHEPS = NHEP
      ITRY2 = 0
      ITRYM = 1000
C  generate diffractive pomeron scatterings
 60   CONTINUE
      ITRY2 = ITRY2+1
      IF(ITRY2.GT.1) THEN
        IFAIL(39) = IFAIL(39)+1
        IF(ITRY2.GE.ITRYM) GOTO 50
      ENDIF
      I1 = IPAR1
      I2 = IPAR2
      JDAHEP(1,IMOTH1) = 0
      JDAHEP(2,IMOTH1) = 0
      JDAHEP(1,IMOTH2) = 0
      JDAHEP(2,IMOTH2) = 0
      ISTHEP(IMOTH1) = 1
      ISTHEP(IMOTH2) = 1
      NHEP = NHEPS
      CALL PODIFF(I1,I2,IMOTH1,IMOTH2,MSOFT,MHARD,2,IREJ)
      IF(IREJ.NE.0) GOTO 50
      CALL PODIFF(I2,I1,IMOTH1,IMOTH2,MSOFT,MHARD,2,IREJ)
      IF(IREJ.NE.0) GOTO 50
      DO 100 I2 = NHEP,1,-1
        IF(IDHEP(I2).EQ.45) GOTO 110
 100  CONTINUE
 110  CONTINUE
      DO 120 I1 = I2-1,1,-1
        IF(IDHEP(I1).EQ.45) GOTO 130
 120  CONTINUE
 130  CONTINUE
C  mass of pomeron-pomeron system
      DO 140 I=1,4
        PD(I) = PHEP(I,I1)+PHEP(I,I2)
 140  CONTINUE
      XMASS = PD(4)**2-PD(1)**2-PD(2)**2-PD(3)**2
      IF(IDEB(59).GE.20) WRITE(6,'(1X,A,2I3,E12.4)')
     &  'PODBLE:DEBUG:IPOM1,IPOM2,MASS**2',I1,I2,XMASS
      IF(XMASS.LT.0.1D0) GOTO 60
      XMASS = SQRT(XMASS)
      IF(XMASS.LT.PARMDL(71)) GOTO 60
C  get cross sections
      IF(XMASS.LE.SIGECM(IP,1)) THEN
        IN1 = 1
        IN2 = 1
      ELSE IF(XMASS.LT.SIGECM(IP,ISIMAX)) THEN
        DO 55 I=2,ISIMAX
          IF(XMASS.LE.SIGECM(IP,I)) GOTO 210
 55     CONTINUE
 210    CONTINUE
        IN1 = I-1
        IN2 = I
      ELSE
        WRITE(6,'(/1X,A,2E12.3)')
     &    'PODBLE:WARNING:TOO HIGH XM',XMASS,SIGECM(IP,ISIMAX)
        IN1 = ISIMAX
        IN2 = ISIMAX
      ENDIF
      FAC2=0.D0
      IF(IN1.NE.IN2) FAC2=LOG(XMASS/SIGECM(IP,IN1))
     &                    /LOG(SIGECM(IP,IN2)/SIGECM(IP,IN1))
      FAC1=1.D0-FAC2
      STOT = FAC2*SIGTAB(IP,1,IN2)+FAC1*SIGTAB(IP,1,IN1)
      SELA = FAC2*SIGTAB(IP,2,IN2)+FAC1*SIGTAB(IP,2,IN1)
      SINE = STOT-SELA
      SDIR = FAC2*SIGTAB(IP,29,IN2)+FAC1*SIGTAB(IP,29,IN1)
      SDIR = MAX(ZERO,SDIR)/SINE
C  sample interaction
      IF((DRNDM(FAC1).LT.SDIR).AND.(IPRON(11).NE.0)) THEN
        IDIR = 1
        ISAM = 0
        JSAM = 0
        KSAM = 0
        LSAM = 0
        MSAM = 0
      ELSE
        CALL SAMPRB(XMASS,IP,ISAM,JSAM,KSAM,LSAM,MSAM)
      ENDIF
C  suppress hard scattering
      IF(IPRON(11).EQ.0) KSAM = 0
C  suppress multiple interaction
      IF(ISWMDL(25).EQ.0) THEN
        KSAM = 0
        IDIR = 0
        IF(ISAM+KSAM.GT.0) THEN
          ISAM = 1
          JSAM = 0
        ELSE
          JSAM = 1
        ENDIF
      ELSE IF(ISWMDL(25).EQ.1) THEN
        IF(IDIR.GT.0) THEN
        ELSE IF(KSAM.GT.0) THEN
          KSAM = 1
          ISAM = 0
          JSAM = 0
        ELSE IF(ISAM.GT.0) THEN
          ISAM = 1
          JSAM = 0
        ELSE
          JSAM = 1
        ENDIF
      ELSE IF(ISWMDL(25).EQ.2) THEN
        KSAM = MIN(KSAM,1)
      ELSE IF(ISWMDL(25).EQ.3) THEN
        IF(ISAM.GT.0) THEN
          ISAM = 1
          JSAM = 0
        ELSE
          JSAM = 1
        ENDIF
      ENDIF
C  rejection loop
 200  CONTINUE
      IF(ISAM+JSAM+KSAM+IDIR.EQ.0) JSAM = 1
C  debug output
      IF(IDEB(59).GE.15) WRITE(6,'(1X,A,/5X,I3,E12.4,4I5)')
     &  'PODBLE:DEBUG:IP,XMASS,ISAM,JSAM,KSAM,IDIR,',
     &  IP,XMASS,ISAM,JSAM,KSAM,IDIR
C  pomeron-pomeron scattering
      IF(IDIR.GT.0) THEN
        IPAR = 4
      ELSE IF(KSAM.GT.0) THEN
        IPAR = 3
      ELSE IF(ISAM.GT.0) THEN
        IPAR = 2
      ELSE
        IPAR = 1
      ENDIF
      IDDPOM = IPAR
C  debug information
      IF(ISAM+JSAM.GT.0) KSDPO = 1
      IF(KSAM+IDIR.GT.0) KHDPO = 1
      KSPOM = ISAM
      KSREG = JSAM
      KHPOM = KSAM
      KHDIR = IDIR
      KSTRG = 0
      KSLOO = 0
      CALL STDPAR(I1,I2,ISAM,JSAM,KSAM,IDIR,IREJ)
      IF(IREJ.NE.0) THEN
        IFAIL(21) = IFAIL(21)+1
        IF(IPAR.GT.1) THEN
          IF(IPAR.EQ.3) IFAIL(9) = IFAIL(9)+1
          IF(IDIR.GT.0) THEN
            IFAIL(10) = IFAIL(10)+1
            IDIR = 0
          ELSE IF(KSAM.GT.0) THEN
            KSAM = KSAM-1
          ELSE IF(ISAM.GT.0) THEN
            ISAM = ISAM-1
          ENDIF
          GOTO 200
        ELSE
          IF(IDEB(59).GE.2) WRITE(6,'(1X,A,2I3,E11.3)')
     &      'PODBLE:WARNING:REJECTION STDPAR (I,IPAR,XM)',
     &      I,IPAR,XMASS
          GOTO 50
        ENDIF
      ENDIF
C  comment line for central diffraction
      CALL REGPAR(40,4,IPAR,IMOTH1,IMOTH2,PD(1),PD(2),PD(3),PD(4),
     &            I1,I2,IDHEP(IMOTH1),IDHEP(IMOTH2),IPOS,1)
      PHEP(5,IPOS) = XMASS
C  debug output
      IF(IDEB(59).GE.15) THEN
        WRITE(6,'(2(/1X,A))') 'PODBLE:DEBUG OUTPUT:',
     &    '===================='
        CALL POPREV(0)
      ENDIF
      RETURN
C  rejection
 50   CONTINUE
      IREJ = 1
      IFAIL(40) = IFAIL(40)+1
      IF(IDEB(59).GE.3) THEN
        WRITE(6,'(1X,A)')
     &    'PODBLE:WARNING:REJECTION (ITRY,ITRYM)',ITRY2,ITRYM
        IF(IDEB(59).GE.10) THEN
          CALL POPREV(0)
        ELSE
          CALL POPREV(-1)
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE SADSDT(IELA1,IELA2,TT)
C**********************************************************************
C
C     t sampling of elastic/quasi elastic processes
C
C     input:   IELA1,2   0   purely elastic scattering
C                        1   rho production
C                        2   omega production
C                        3   phi production
C                        4   pi+/pi- background
C              IELA1     -1  initialization
C                        -2  output of statistics
C
C     output:  TT     t   Mandelstam variable (negative)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(NTAB = 20,
     &          EPS  = 1.D-10,
     &          IONE = 1,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
      COMMON /QEVECM/ VMAS(4),GAMM(4),RMIN(4),RMAX(4),VMSL(4),VMFA(4),
     &                TMIN(4),TMAX(4)
C
      IF(((IELA1.GE.0).AND.(IELA1.LE.4))
     &   .AND.((IELA2.GE.0).AND.(IELA2.LE.4))) THEN
C  determine slope
        IF(ISWMDL(13).EQ.0) THEN
C  external slope values
          print *,' SADSDT:ERROR:not installed !!!!!!!'
          CALL POABRT
        ELSE IF(ISWMDL(13).EQ.1) THEN
C  model slopes
          IF(IELA1*IELA2.EQ.0) THEN
            SLOPE = SLOEL
            TTMIN = TMIN(1)
            TTMAX = TMAX(1)
          ELSE
            SLOPE = SLOVM(IELA1,IELA2)
            I = MAX(IELA1,IELA2)
            TTMIN = TMIN(I)
            TTMAX = TMAX(I)
          ENDIF
        ELSE
          WRITE(6,'(/1X,A,I5)') 'SASDSDT:ERROR:INVALID ISWMDL(13)',
     &      ISWMDL(13)
          CALL POABRT
        ENDIF
C  sampling of t value
        XI0   = EXP(-SLOPE*ABS(TTMIN))
        XIDEL = EXP(-SLOPE*ABS(TTMAX))-XI0
        TT = LOG(XIDEL*DRNDM(XI)+XI0)/SLOPE
C  debug output
        IF(IDEB(33).GE.15) WRITE(6,'(1X,A,2I3,2E12.3)')
     &    'SADSDT:DEBUG:IELA1,IELA2,SLOPE,T',IELA1,IELA2,SLOPE,TT
C  initialization of tables
      ELSE IF(IELA1.EQ.-1) THEN
C  output of statistics
      ELSE IF(IELA1.EQ.-2) THEN
C  unknown mode IELA1
      ELSE
        WRITE(6,'(/1X,A,2I5)') 'SADSDT:ERROR:UNSUPPORTED IELA1,IELA2',
     &    IELA1,IELA2
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE SAMASS(IFLA,RMASS)
C**********************************************************************
C
C     resonance mass sampling of quasi elastic processes
C
C     input:   IFLA       PDG number of particle
C              IFLA   -1  initialization
C              IFLA   -2  output of statistics
C
C     output:  RMASS      particle mass (in GeV)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(EPS  = 1.D-10,
     &          IONE = 1,
     &          ONE  = 1.D0,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /QEVECM/ VMAS(4),GAMM(4),RMIN(4),RMAX(4),VMSL(4),VMFA(4),
     &                TMIN(4),TMAX(4)
C
      CHARACTER*79  TITLE
      DIMENSION     IHIS(4)
      PARAMETER(NTABM=50)
      DIMENSION XMA(4,NTABM),XMC(4,NTABM),RMA(4,NTABM)
      DIMENSION SUM(4),ICALL(4)
      DIMENSION XLIMB(2)
C
C*****************************************************************
C  initialization of tables
      IF(IFLA.EQ.-1) THEN
C
        NSTEP = NTABM
        DO 102 I=1,4
          ICALL(I) = 0
          IHIS(I)  = 0
C          IF(IDEB(35).GE.1)
C     &      CALL NEWHIS(RMIN(I),RMAX(I),ZERO,XLIMB,30,IHIS(I))
          DELTAM=(RMAX(I)-RMIN(I))/DBLE(NSTEP-1)
          DO 105 K=1,NSTEP
            RMA(I,K)=RMIN(I)+DELTAM*DBLE(K-1)
 105      CONTINUE
 102    CONTINUE
C  calculate table of dsig/dm
        CALL DSIGDM(RMA,XMA,NSTEP)
C  output of table
        IF(IDEB(35).GE.1) THEN
          WRITE(6,'(/5X,A)') 'TABLE:   MASS (GeV)  DSIG/DM (mub/GeV)'
          WRITE(6,'(1X,A,/1X,A)')
     &      '  (M,  RHO,     M,  OMEGA,      M,   PHI,    M,  PI+PI-)',
     &      ' -------------------------------------------------------'
          DO 106 K=1,NSTEP
            WRITE(6,'(1X,8E12.3)') RMA(1,K),XMA(1,K),
     &        RMA(2,K),XMA(2,K),RMA(3,K),XMA(3,K),RMA(4,K),XMA(4,K)
 106      CONTINUE
        ENDIF
C  make second table for sampling
        DO 109 I=1,4
          SUM(I) = ZERO
          DO 108 K=2,NSTEP
            SUM(I) = SUM(I) + (XMA(I,K-1)+XMA(I,K))/2.D0
            XMC(I,K) = SUM(I)
 108      CONTINUE
 109    CONTINUE
C  normalization
        DO 118 K=1,NSTEP
          DO 119 I=1,4
            XMC(I,K) = XMC(I,K)/XMC(I,NSTEP)
 119      CONTINUE
 118    CONTINUE
        IF(IDEB(35).GE.10) THEN
          WRITE(6,'(/5X,A)') 'DSIGDM:DEBUG:NORMALIZED SUMMED TABLE:'
          WRITE(6,'(1X,A,/1X,A)')
     &      '  (M,  RHO,     M,  OMEGA,      M,   PHI,    M,  PI+PI-)',
     &      ' -------------------------------------------------------'
          DO 120 K=1,NSTEP
            WRITE(6,'(1X,8E12.3)') RMA(1,K),XMC(1,K),
     &        RMA(2,K),XMC(2,K),RMA(3,K),XMC(3,K),RMA(4,K),XMC(4,K)
 120      CONTINUE
        ENDIF
C
C**************************************************
C  output of statistics
      ELSE IF(IFLA.EQ.-2) THEN
        WRITE(6,'(2(/1X,A))') 'SAMASS:DEBUG:STATISTICS',
     &                        '========================'
        WRITE(6,'(4(/8X,A,I10))') 'RHO:   ',ICALL(1),
     &    'OMEGA: ',ICALL(2),'PHI:   ',ICALL(3),'PI+PI-:',ICALL(4)
c        IF(IHIS(1).NE.0) THEN
c          TITLE='SAMASS: RHO0 MASS DISTRIBUTION (QUASI-ELASTIC)'
c          CALL OUTHIS(IHIS(1),0,TITLE,ONE,0)
c        ENDIF
c        IF(IHIS(2).NE.0) THEN
c          TITLE='SAMASS: OMEGA(768) MASS DISTRIBUTION (QUASI-ELASTIC)'
c          CALL OUTHIS(IHIS(2),0,TITLE,ONE,0)
c        ENDIF
c        IF(IHIS(3).NE.0) THEN
c          TITLE='SAMASS: PHI MASS DISTRIBUTION (QUASI-ELASTIC)'
c          CALL OUTHIS(IHIS(3),0,TITLE,ONE,0)
c        ENDIF
c        IF(IHIS(4).NE.0) THEN
c          TITLE='SAMASS: PI+/PI- MASS DISTRIBUTION (BACKGROUND)'
c          CALL OUTHIS(IHIS(4),0,TITLE,ONE,0)
c        ENDIF
C
C********************************************************
C  sampling of RMASS
      ELSE
        IF(IFLA.EQ.113) THEN
          KP = 1
        ELSE IF(IFLA.EQ.223) THEN
          KP = 2
        ELSE IF(IFLA.EQ.333) THEN
          KP = 3
        ELSE IF(IFLA.EQ.6666) THEN
          KP = 4
        ELSE
          RMASS = POPAMA(IFLA,1)
          IF(IDEB(35).GE.20) WRITE(6,'(1X,A,I7,E12.3)')
     &      'SAMASS:DEBUG:IFLA,MASS',IFLA,RMASS
          RETURN
        ENDIF
        XI = DRNDM(RMASS) + EPS
C  binary search
        IF((XMC(KP,1).LE.XI).AND.(XMC(KP,NSTEP).GE.XI)) THEN
          KMIN=1
          KMAX=NSTEP
 300      CONTINUE
          IF((KMAX-KMIN).EQ.IONE) GOTO 400
          KK=(KMAX+KMIN)/2
          IF(XI.LE.XMC(KP,KK)) THEN
            KMAX=KK
          ELSE
            KMIN=KK
          ENDIF
          GOTO 300
 400      CONTINUE
        ELSE
          WRITE(6,'(1X,A)') 'SAMASS:ERROR:XI OUT OF RANGE'
          WRITE(6,'(5X,A,I7,I6,E12.4)') 'EVENT,IFLA,XI',
     &      KEVENT,IFLA,XI
          CALL POABRT
        ENDIF
C  fine interpolation
        RMASS = RMA(KP,KMIN)+
     &          (RMA(KP,KMAX)-RMA(KP,KMIN))/
     &          (XMC(KP,KMAX)-XMC(KP,KMIN))
     &          *(XI-XMC(KP,KMIN))
        IF(IDEB(35).GE.20) THEN
          IF(IDEB(35).GE.25) WRITE(6,'(1X,A,3E15.3)')
     &      'SAMASS:DEBUG:MLEFT,MRIGHT,RMASS',
     &      RMA(KP,KMIN),RMA(KP,KMAX),RMASS
          WRITE(6,'(1X,A,I7,E12.3)') 'SAMASS:DEBUG:IFLA,MASS',
     &      IFLA,RMASS
        ENDIF
C  histogramming
        ICALL(KP) = ICALL(KP)+1
        IF(IHIS(KP).NE.0) THEN
C          CALL FILHIS(RMASS,ONE,IHIS(KP))
C          CALL ADDHIS(IHIS(KP))
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE DSIGDM(RMA,XMA,NSTEP)
C**********************************************************************
C
C     differential cross section DSIG/DM of low mass enhancement
C
C     input:   RMA(4,NTABM)   mass values
C     output:  XMA(4,NTABM)   DSIG/DM of resonances
C                  1          rho production
C                  2          omega production
C                  3          phi production
C                  4          pi-pi continuum
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(NTAB = 20,
     &          EPS  = 1.D-10,
     &          IONE = 1,
     &          ZERO = 0.D0)
C
      PARAMETER(NTABM=50)
      DIMENSION XMA(4,NTABM),RMA(4,NTABM)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /QEVECM/ VMAS(4),GAMM(4),RMIN(4),RMAX(4),VMSL(4),VMFA(4),
     &                TMIN(4),TMAX(4)
C
      PIMASS = 0.135
C  rho meson shape (mass dependent width)
      QRES = SQRT(VMAS(1)**2 - 4.D0*PIMASS**2)
      DO 100 I=1,NSTEP
        XMASS = RMA(1,I)
        QQ = SQRT(XMASS**2 - 4.D0*PIMASS**2)
        GAMMA = GAMM(1)*(QQ/QRES)**3
        XMA(1,I) = XMASS*GAMMA*(XMASS/VMAS(1))**PARMDL(49)
     &             /((VMAS(1)**2-XMASS**2)**2+VMAS(1)**2*GAMMA**2)
 100  CONTINUE
C  omega/phi meson (constant width)
      DO 200 K=2,3
        DO 300 I=1,NSTEP
          XMASS = RMA(K,I)
          XMA(K,I) = XMASS*GAMM(K)
     &               /((VMAS(K)**2-XMASS**2)**2+VMAS(K)**2*GAMM(K)**2)
 300    CONTINUE
 200  CONTINUE
C  pi-pi continuum
      DO 400 I=1,NSTEP
        XMASS = RMA(4,I)
        XMA(4,I) = (XMASS-0.29D0)**2/XMASS
 400  CONTINUE
      END
C
C
      SUBROUTINE SDECAY(NPOS,ISP,ILEV)
C**********************************************************************
C
C     decay of single resonance of COMMON HEPEVS:
C       decay in helicity frame according to polarization, isotropic 
C       decay and decay with limited transverse phase space possible
C
C     ATTENTION:
C     reference to particle number of BAMJET has to existc
C
C     input:   NPOS    position in /HEPEVS/
C              ISP     0  decay according to phase space
C                      1  decay according to transversal polarization
C                      2  decay according to longitudinal polarization
C                      3  decay with limited phase space
C              ILEV    decay mode to use
C                      1 strong + weak decays
C                      2 only strong decays
C                      3 strong decays, weak decays for charmed part.
C                        and tau leptons
C                      negative: remove mother resonance after decay
C
C     output:  COMMON HEPEVS/E final particles according to decay mode
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(NTAB = 20,
     &          EPS  = 1.D-15,
     &          ONE  = 1.D0,
     &          OHALF= 0.5D0,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          DEPS = 1.D-10,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C  particle properties
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     &IBAR(187),K1(187),K2(187)
C  decay channels
      COMMON/DECAYC/ ZKNAME(550),NZK(550,3),WT(550)
C  final particles
      COMMON /DECPHS/ ECM(3),PCM(3),COD(3),COF(3),SIF(3)
C
      DIMENSION WGHD(20),KCH(20),ID(3),IFL(3)
C
      IDBAM = IMPART(NPOS)
      IMODE = ABS(ILEV)
      IF(IDEB(36).GE.15) WRITE(6,'(1X,A,3I5)') 
     &  'SDECAY:DEBUG:NPOS,ISP,ILEV',NPOS,ISP,ILEV
C  particle stable?
      IF(IMODE.EQ.1) THEN
        IF((IDBAM.EQ.135).OR. (IDBAM.EQ.136)) GOTO 400
        IF((IDBAM.GE.  1).AND.(IDBAM.LE.  7)) GOTO 400
      ELSE IF(IMODE.EQ.2) THEN
        IF((IDBAM.GE.  1).AND.(IDBAM.LE. 25)) GOTO 400
        IF((IDBAM.GE. 97).AND.(IDBAM.LE.103)) GOTO 400
        IF((IDBAM.GE.115).AND.(IDBAM.LE.122)) GOTO 400
        IF((IDBAM.GE.131).AND.(IDBAM.LE.136)) GOTO 400
        IF(IDBAM.EQ.109)                      GOTO 400
        IF((IDBAM.GE.137).AND.(IDBAM.LE.160)) GOTO 400
      ELSE IF(IMODE.EQ.3) THEN
        IF((IDBAM.GE.  1).AND.(IDBAM.LE. 23)) GOTO 400
        IF((IDBAM.GE. 97).AND.(IDBAM.LE.103)) GOTO 400
        IF((IDBAM.EQ.109).AND.(IDBAM.EQ.115)) GOTO 400
        IF((IDBAM.GE.133).AND.(IDBAM.LE.136)) GOTO 400
      ELSE
        WRITE(6,'(/1X,A,I5)') 'SDECAY:ERROR:INVALID MODE',IMODE
        CALL POABRT
      ENDIF
C  decay products
      K = 0
      WGSUM = ZERO
      AMIST = PHEP(5,NPOS)
      DO 100 I=K1(IDBAM),K2(IDBAM)
        AMSUM = ZERO
        DO 200 L=1,3
          ID(L) = NZK(I,L)
          IF(ID(L).NE.0) THEN
            AMSUM = AMSUM+AM(ID(L))
          ENDIF
 200    CONTINUE
        IF(AMSUM.LT.AMIST) THEN
          K = K+1
          WGHD(K) = WT(I)
          KCH(K) = I
        ENDIF
 100  CONTINUE
      IF(K.EQ.0)THEN
        WRITE(6,'(/1X,A,I6,3E12.4)')
     &    'SDECAY:WARNING:TOO SMALL PARTICLE MASS (NPOS,MA,DCYM)',
     &    NPOS,AMIST,AMSUM
        CALL POPREV(0)
        RETURN
      ENDIF
C  sample new decay channel
      XI = (DRNDM(AMSUM)-EPS)*WGSUM
      K = 0
      WGSUM = ZERO
 500  CONTINUE
        K = K+1
        WGSUM = WGSUM+WGHD(K)
      IF(XI.GT.WGSUM) GOTO 500
      IK = KCH(K)
      ID(1) = NZK(IK,1)
      ID(2) = NZK(IK,2)
      ID(3) = NZK(IK,3)
C  decay possible?
      IF(ID(2).LT.1) RETURN
C  rotation
      PTOT = SQRT(PHEP(1,NPOS)**2+PHEP(2,NPOS)**2+PHEP(3,NPOS)**2)
      CXS = PHEP(1,NPOS)/PTOT
      CYS = PHEP(2,NPOS)/PTOT
      CZS = PHEP(3,NPOS)/PTOT
C  boost
      GBET = PTOT/AMIST
      GAM = PHEP(4,NPOS)/AMIST
C
      IF(ID(3).EQ.0) THEN
C  two particle decay
        CALL SDECY2(AMIST,AM(ID(1)),AM(ID(2)),ISP)
      ELSE
C  three particle decay
        CALL SDECY3(AMIST,AM(ID(1)),AM(ID(2)),AM(ID(3)),ISP)
      ENDIF
C
      IF(ILEV.LT.0) THEN
        IF(NHEP.NE.NPOS) THEN
          WRITE(6,'(/1X,A,2I5)')
     &    'SDECAY:ERROR:CANNOT REMOVE RESONANCE (NPOS,NHEP)',NPOS,NHEP
          CALL POABRT
        ENDIF
        IMO1 = JMOHEP(1,NPOS)
        IMO2 = JMOHEP(2,NPOS)
        NHEP = NHEP-1
      ELSE
        IMO1 = NPOS
        IMO2 = 0
      ENDIF
C  back transformation and registration
      DO 300 I=1,3
        IF(ID(I).GT.0) THEN
          CALL TRAFO(GAM,GBET,CXS,CYS,CZS,COD(I),COF(I),SIF(I),
     &      PCM(I),ECM(I),PTOT,CX,CY,CZ,EE)
          XX = PTOT*CX
          YY = PTOT*CY
          ZZ = PTOT*CZ
          CALL REGPAR(1,IFL(I),ID(I),IMO1,IMO2,XX,YY,ZZ,EE,0,0,0,0,
     &                IPOS,1)
        ENDIF
 300  CONTINUE
C
 400  CONTINUE
C  debug output
      IF(IDEB(36).GE.20) THEN
        WRITE(6,'(2(/1X,A))') 'SDECAY:DEBUG:COMMON HEPEVS',
     &                        '=========================='
        CALL POPREV(0)
      ENDIF
      END
C
C
      SUBROUTINE SDECY2(UMO,AM1,AM2,ISP)
C**********************************************************************
C
C     isotropic/anisotropic two particle decay in CM system,
C     (transversally polarized boson into two pseudo-scalar mesons)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   = 0.D0,
     &           ONE    = 1.D0,
     &           TWO    = 2.D0 )
C
      COMMON /DECPHS/ ECM(3),PCM(3),COD(3),COF(3),SIF(3)
C
      UMO2=UMO*UMO
      AM11=AM1*AM1
      AM22=AM2*AM2
      ECM(1)=(UMO2+AM11-AM22)/(TWO*UMO)
      ECM(2)=UMO-ECM(1)
      WAU=ECM(1)*ECM(1)-AM11
      IF(WAU.LT.ZERO) THEN
        WRITE(6,'(/1X,A,E12.4)') 'SDECY2:ERROR:TOO SMALL MASS',WAU
        CALL POABRT
      ENDIF
      PCM(1)=SQRT(WAU)
      PCM(2)=PCM(1)
C
      CALL SFECFE(SIF(1),COF(1))
      IF(ISP.EQ.0) THEN
C  no polarization
        COD(1)  = TWO*DRNDM(UMO)-ONE
      ELSE IF(ISP.EQ.1) THEN
C  transverse polarization
 400    CONTINUE
          COD(1)  = TWO*DRNDM(SID12)-ONE
          SID12 = ONE-COD(1)*COD(1)
        IF(SID12.LT.DRNDM(AM1)) GOTO 400
      ELSE IF(ISP.EQ.2) THEN
C  longitudinal polarization
 500    CONTINUE
          COD(1)  = TWO*DRNDM(AM2)-ONE
          COD12 = COD(1)*COD(1)
        IF(COD12.LT.DRNDM(AM11)) GOTO 500
      ELSE
        WRITE(6,'(/1X,A,I3)') 'SDECY2:ERROR:INVALID POLARIZATION',ISP
        CALL POABRT
      ENDIF
C
      COD(2) = -COD(1)
      COF(2) = -COF(1)
      SIF(2) = -SIF(1)
      END
C
C
      SUBROUTINE SDECY3(UMO,AM1,AM2,AM3,ISP)
C**********************************************************************
C
C     isotropic/anisotropic three particle decay in CM system,
C     (transversally polarized boson into three pseudo-scalar mesons)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   = 0.D0,
     &           OHALF  = 0.5D0,
     &           ONE    = 1.D0,
     &           TWO    = 2.D0,
     &           DEPS   = 1.D-30,
     &           EPS    = 1.D-15 )
C
      COMMON /DECPHS/ ECM(3),PCM(3),COD(3),COF(3),SIF(3)
      DIMENSION F(5),XX(5)
C
C  calculation of maximum of S2 phase space weight
      UMOO=UMO+UMO
      GU=(AM2+AM3)**2
      GO=(UMO-AM1)**2
      UFAK=1.0000000000001D0
      IF (GU.GT.GO) UFAK=0.99999999999999D0
      OFAK=TWO-UFAK
      GU=GU*UFAK
      GO=GO*OFAK
      DS2=(GO-GU)/99.D0
      AM11=AM1*AM1
      AM22=AM2*AM2
      AM33=AM3*AM3
      UMO2=UMO*UMO
      RHO2=ZERO
      S22=GU
      DO 124 I=1,100
        S21=S22
        S22=GU+(I-1.D0)*DS2
        RHO1=RHO2
        RHO2=XLAMB(S22,UMO2,AM11)*XLAMB(S22,AM22,AM33)/(S22+EPS)
        IF(RHO2.LT.RHO1) GOTO 125
  124 CONTINUE
C
  125 CONTINUE
      S2SUP=(S22-S21)/2.D0+S21
      SUPRHO=XLAMB(S2SUP,UMO2,AM11)*XLAMB(S2SUP,AM22,AM33)
     &       /(S2SUP+EPS)
      SUPRHO=SUPRHO*1.05D0
      XO=S21-DS2
      IF(GU.LT.GO.AND.XO.LT.GU) XO=GU
      IF(GU.GT.GO.AND.XO.GT.GU) XO=GU
      XX(1)=XO
      XX(3)=S22
      X1=(XO+S22)*OHALF
      XX(2)=X1
      F(3)=RHO2
      F(1)=XLAMB(XO,UMO2,AM11)*XLAMB(XO,AM22,AM33)/(XO+EPS)
      F(2)=XLAMB(X1,UMO2,AM11)*XLAMB(X1,AM22,AM33)/(X1+EPS)
      DO 126 I=1,16
        X4=(XX(1)+XX(2))*OHALF
        X5=(XX(2)+XX(3))*OHALF
        F(4)=XLAMB(X4,UMO2,AM11)*XLAMB(X4,AM22,AM33)/(X4+EPS)
        F(5)=XLAMB(X5,UMO2,AM11)*XLAMB(X5,AM22,AM33)/(X5+EPS)
        XX(4)=X4
        XX(5)=X5
        DO 128 II=1,5
          IA=II
          DO 131 III=IA,5
            IF(F(II).LT.F(III)) THEN
              FH=F(II)
              F(II)=F(III)
              F(III)=FH
              FH=XX(II)
              XX(II)=XX(III)
              XX(III)=FH
            ENDIF
 131      CONTINUE
 128    CONTINUE
        SUPRHO=F(1)
        S2SUP=XX(1)
        DO 129 II=1,3
          IA=II
          DO 130 III=IA,3
            IF (XX(II).LT.XX(III)) THEN
              FH=F(II)
              F(II)=F(III)
              F(III)=FH
              FH=XX(II)
              XX(II)=XX(III)
              XX(III)=FH
            ENDIF
 130      CONTINUE
 129    CONTINUE
 126  CONTINUE
C
      AM23=(AM2+AM3)**2
C
C  selection of S1
      ITH=0
 200  CONTINUE
        ITH=ITH+1
        IF(ITH.GT.200) THEN
          WRITE(6,'(/1X,A,I10)') 'SDECY3:ERROR:TOO MANY ITERATIONS',
     &      ITH
          CALL POABRT
        ENDIF
        S2=AM23+DRNDM(AM2)*((UMO-AM1)**2-AM23)
        Y=DRNDM(AM23)*SUPRHO
        RHO=XLAMB(S2,UMO2,AM11)*XLAMB(S2,AM22,AM33)/S2
      IF(Y.GT.RHO) GOTO 200
C
C  selection of S2
      S1=DRNDM(AM2)*RHO+AM11+AM22-(S2-UMO2+AM11)*(S2+AM22-AM33)
     &   /(2.D0*S2)-RHO/TWO
      S3=UMO2+AM11+AM22+AM33-S1-S2
      ECM(1)=(UMO2+AM11-S2)/UMOO
      ECM(2)=(UMO2+AM22-S3)/UMOO
      ECM(3)=(UMO2+AM33-S1)/UMOO
      PCM(1)=SQRT((ECM(1)+AM1)*(ECM(1)-AM1))
      PCM(2)=SQRT((ECM(2)+AM2)*(ECM(2)-AM2))
      PCM(3)=SQRT((ECM(3)+AM3)*(ECM(3)-AM3))
C
C  calculation of angles: TH between p1,p2; TH1 p3,p1; TH2 p3,p2
      IF((PCM(1).LE.EPS).OR.(PCM(2).LE.EPS)) THEN
        COSTH=(DRNDM(S1)-OHALF)*TWO
      ELSE
        COSTH=(ECM(1)*ECM(2)+OHALF*(AM11+AM22-S1))/(PCM(1)*PCM(2))
      ENDIF
*     IF(ABS(COSTH).GT.0.999999999999D0)
*    &  COSTH=SIGN(0.99999999999D0,COSTH)
      COSTH2=(PCM(3)*PCM(3)+PCM(2)*PCM(2)-PCM(1)*PCM(1))
     &        /(TWO*PCM(2)*PCM(3))
*     IF(ABS(COSTH2).GT.0.99999999999D0)
*    &  COSTH2=SIGN(0.9999999999D0,COSTH2)
      SINTH2=SQRT(ONE-COSTH2*COSTH2)
      SINTH1=COSTH2*SQRT(ONE-COSTH*COSTH)-COSTH*SINTH2
      COSTH1=COSTH*COSTH2+SINTH2*SQRT(ONE-COSTH*COSTH)
C
C  selection of the sperical coordinates of particle 3
      CALL SFECFE(SIF(3),COF(3))
      IF(ISP.EQ.0) THEN
C  no polarization
        COD(3)  = TWO*DRNDM(S2)-ONE
      ELSE IF(ISP.EQ.1) THEN
C  transverse polarization
 400    CONTINUE
          COD(3)  = TWO*DRNDM(S1)-ONE
          SID32 = ONE-COD(3)*COD(3)
        IF(SID32.LT.DRNDM(COSTH)) GOTO 400
      ELSE IF(ISP.EQ.2) THEN
C  longitudinal polarization
 500    CONTINUE
          COD(3)  = TWO*DRNDM(COSTH2)-ONE
          COD32 = COD(3)*COD(3)
        IF(COD32.LT.DRNDM(SINTH1)) GOTO 500
      ELSE
        WRITE(6,'(/1X,A,I3)') 'SDECY3:ERROR:INVALID POLARIZATION',ISP
        CALL POABRT
      ENDIF
C
C  selection of the rotation angle of p1-p2 plane along p3
      IF(ISP.EQ.0) THEN
        CALL SFECFE(SFE,CFE)
      ELSE
        SFE = ZERO
        CFE = ONE
      ENDIF
      CX11=-COSTH1
      CY11=SINTH1*CFE
      CZ11=SINTH1*SFE
      CX22=-COSTH2
      CY22=-SINTH2*CFE
      CZ22=-SINTH2*SFE
C
      SID3 = SQRT((ONE+COD(3))*(ONE-COD(3)))
      COD(1)=CX11*COD(3)+CZ11*SID3
      IF((ONE-COD(1)*COD(1)).LT.DEPS) THEN
        WRITE(6,'(1X,A,5E12.4)') 'SDECY3:WARNING:COS(TH1) > 1',
     &    COD(1),COF(3),SID3,CX11,CZ11
        CALL POPREV(-1)
      ENDIF
      SID1=SQRT((ONE+COD(1))*(ONE-COD(1)))
      COF(1)=(CX11*SID3*COF(3)-CY11*SIF(3)-CZ11*COD(3)*COF(3))/SID1
      SIF(1)=(CX11*SID3*SIF(3)+CY11*COF(3)-CZ11*COD(3)*SIF(3))/SID1
      COD(2)=CX22*COD(3)+CZ22*SID3
      SID2=SQRT((ONE+COD(2))*(ONE-COD(2)))
      COF(2)=(CX22*SID3*COF(3)-CY22*SIF(3)-CZ22*COD(3)*COF(3))/SID2
      SIF(2)=(CX22*SID3*SIF(3)+CY22*COF(3)-CZ22*COD(3)*SIF(3))/SID2
      END
C
C
      DOUBLE PRECISION FUNCTION XDMASS(XMIN,XMAX,PREF2,IMODE)
C**********************************************************************
C
C     sampling of Mx diffractive mass distribution within
C              limits XMIN, XMAX
C
C     input:    XMIN,XMAX     limitations (GeV)
C               PREF2         original particle mass/ reference mass
C                             (squared, GeV**2)
C               IMODE         1 critical Pomeron
C                             2 supercritical Pomeron
C
C     output:   diffractive mass (GeV)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER(EPS  = 1.D-10,
     &          IONE = 1,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          ZERO = 0.D0)
C
      IF((XMIN.GE.XMAX).OR.(XMIN.LE.0.3D0)) THEN
        WRITE(6,'(/1X,A,3E12.4)') 'XDMASS:ERROR:INVALID MASS LIMITS',
     &    XMIN,XMAX,PREF2
        CALL POPREV(-1)
        XDMASS = 0.135D0
        RETURN
      ENDIF
C
      IF(IMODE.GT.0) THEN
        PM2 = ZERO
      ELSE
        PM2 = PREF2
      ENDIF
C
C  critical Pomeron
      IF(ABS(IMODE).EQ.1) THEN
        XMIN2 = LOG(XMIN**2-PM2)
        XMAX2 = LOG(XMAX**2-PM2)
        XI = (XMAX2-XMIN2)*DRNDM(PM2)+XMIN2
        XMA2 = EXP(XI)+PM2
C
C  supercritical Pomeron
      ELSE IF(ABS(IMODE).EQ.2) THEN
        DDELTA = 1.D0-PARMDL(48)
        XMIN2 = (XMIN**2-PM2)**DDELTA
        XMAX2 = (XMAX**2-PM2)**DDELTA
        XI = (XMAX2-XMIN2)*DRNDM(PM2)+XMIN2
        XMA2 = XI**(1.D0/DDELTA)+PM2
      ELSE
        WRITE(6,'(/,1X,A,I3)') 'XMASS2:ERROR:UNSUPPORTED MODE IMODE ',
     &       IMODE
        CALL POABRT
      ENDIF
C
      XDMASS = SQRT(XMA2)
C  debug output
      IF(IDEB(43).GE.5) THEN
        WRITE(6,'(1X,A,4E12.3)') 'XDMASS:XMIN,XMAX,PREF2,XDMASS',
     &    XMIN,XMAX,PREF2,XDMASS
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION XSLOPE(TMIN,TMAX,XM1,XM2,IREJ)
C**********************************************************************
C
C     sampling of T  (Mandelstam variable) distribution within
C              limits TMIN, TMAX
C
C     input:    TMIN,TMAX ( < 0 )   limitations (GeV**2)
C               XM1                 mass of diffractive system 1
C               XM2                 mass of diffractive system 2
C
C     output:   T value    ( < 0 )  (GeV**2)
C               IREJ       0  successful sampling
C                          1  masses too big for given T range
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C  CM system of diffraction
      COMMON /DIFCMS/ ECMD,PCMD,PMASSD(2),PVIRTD(2),GAMBED(4),
     &                SIDD,CODD,SIFD,COFD,PDCMS(4,2),NPOSD(2)
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
      PARAMETER(EPS  = 1.D-10,
     &          IONE = 1,
     &          TWO  = 2.D0,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          ZERO = 0.D0)
C
      IREJ = 0
      XMP12 = XM1**2+PVIRTD(1)
      XMP22 = XM2**2+PVIRTD(2)
      XMX1 = SQRT(XMP12)
      XMX2 = SQRT(XMP22)
      SS = ECMD**2
C
C  calculation of slope (mass dependent)
      CALL SCALES(PMASSD(1),PMASSD(2),XMX1,XMX2,SC1,SC2,SB1,SB2)
      FAC = 4.D0*(PMASSD(1)*PMASSD(2))**2
      SLOPE = B0PPP+TWO*(B0POM(1)*SB1+B0POM(2)*SB2+ALPOMP*LOG(SS*FAC
     &  /((PMASSD(1)**2+XMP12)*(PMASSD(2)**2+XMP22))+PARMDL(47)))
      SLOPE = MAX(SLOPE,2.D0)
C
C  determine min. ABS(T) necessary to produce masses
      PCM2 = PCMD**2
      PCMP2 = XLAMB(SS,XMP12,XMP22)**2/(4.D0*SS)
      IF(PCMP2.LE.ZERO) THEN
        IREJ = 1
        XSLOPE = 0.D0
        RETURN
      ENDIF
      TMINP = PMASSD(1)**2+XMP12+TWO*PCMD*SQRT(PCMP2)
     &        -TWO*SQRT((PMASSD(1)**2+PCM2)*(XMP12+PCMP2))
C
      IF(TMINP.LT.TMAX) THEN
        IF(IDEB(44).GE.3) THEN
          WRITE(6,'(1X,A,/5X,5E12.3)')
     &      'XSLOPE:REJECTION:TMIN,TMAX,TMINP,XM1,XM2',
     &      TMIN,TMAX,TMINP,XM1,XM2
        ENDIF
        IFAIL(32) = IFAIL(32)+1
        IREJ = 1
        XSLOPE = 0.D0
        RETURN
      ENDIF
      TMINA = MIN(TMIN,TMINP)
C
C  sampling from corrected range of T
      TMINE = EXP(SLOPE*TMINA)
      TMAXE = EXP(SLOPE*TMAX)
      XI = (TMINE-TMAXE)*DRNDM(SLOPE)+TMAXE
      TT = LOG(XI)/SLOPE
C  debug output
      IF(IDEB(44).GE.15) THEN
        WRITE(6,'(1X,A,/5X,7E10.3)')
     &    'XSLOPE:DEBUG:TMIN,TMINP,TMAX,SLOPE,T,XM1,XM2',
     &    TMIN,TMINP,TMAX,SLOPE,TT,XM1,XM2
      ENDIF
C
      XSLOPE = TT
      END
C
C
      SUBROUTINE DIFKIN(XMP1,XMP2,TT,PMOM1,PMOM2,IREJ)
C**********************************************************************
C
C     calculation of diffractive kinematics
C
C     input:    XMP1         mass of outgoing particle system 1 (GeV)
C               XMP2         mass of outgoing particle system 2 (GeV)
C               TT           momentum transfer    (GeV**2, negative)
C
C     output:   PMOM1(4)     four momentum of outgoing system 1
C               PMOM2(4)     four momentum of outgoing system 2
C               IREJ         0    kinematics consistent
C                            1    kinematics inconsistent
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION PMOM1(5),PMOM2(5)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  CM system of diffraction
      COMMON /DIFCMS/ ECMD,PCMD,PMASSD(2),PVIRTD(2),GAMBED(4),
     &                SIDD,CODD,SIFD,COFD,PDCMS(4,2),NPOSD(2)
C
      PARAMETER(EPS  = 1.D-10,
     &          IONE = 1,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          DEPS = 0.001,
     &          ZERO = 0.D0)
C
C  debug output
      IF(IDEB(49).GT.10) THEN
        WRITE(6,'(1X,A,/5X,5E12.4)')
     &    'DIFKIN:DEBUG:ECMD,PCMD,MINI-1,MINI-2,TT:',
     &    ECMD,PCMD,XMP1,XMP2,TT
      ENDIF
C
C  kinematic constraints
      IREJ = 1
      IF(ECMD.LE.1.1D0*(XMP1+XMP2)) RETURN
C  new squared cms momentum
      XMP12 = XMP1**2
      XMP22 = XMP2**2
      SS = ECMD**2
      PCM2 = PCMD**2
      PCMP2 = XLAMB(SS,XMP12,XMP22)**2/(4.D0*SS)
C
C  new longitudinal momentum
      E1I = SQRT(PCM2+PMASSD(1)**2)
      E1F = SQRT(PCMP2+XMP12)
      E2F = SQRT(PCMP2+XMP22)
C
      PLONG = (TT+PCM2+PCMP2-(E1I-E1F)**2)/(2.D0*PCMD)
      PTRAN = PCMP2-PLONG**2
      IF(PTRAN.LT.ZERO) THEN
        IF(IDEB(49).GE.1) THEN
        WRITE(6,'(1X,A)') 'DIFKIN:WARNING:INCONSISTENT KINEMATICS:'
        CALL POPREV(-1)
        WRITE(6,'(1X,A)') 'DIFKIN:DEBUG:INITIAL KINEMATICS:'
        WRITE(6,'(1X,A,/5X,6E12.3)') 'XMP1,XMP2,TT,PCMP,PLONG,PTRANS',
     &    XMP1,XMP2,TT,SQRT(PCMP2),PLONG,SIGN(SQRT(ABS(PTRAN)),PTRAN)
        ENDIF
        PLONG = SQRT(PCMP2)
        PTRAN = ZERO
        TT = 2.D0*PCMD*PLONG+(E1I-E1F)**2-PCM2-PCMP2
        IF(IDEB(49).GE.1) THEN
        WRITE(6,'(1X,A)') 'DIFKIN:DEBUG:ADJUSTED KINEMATICS:'
        WRITE(6,'(1X,A,/5X,6E12.3)') 'XMP1,XMP2,TT,PCMP,PLONG,PTRANS',
     &    XMP1,XMP2,TT,SQRT(PCMP2),PLONG,SQRT(PTRAN)
        ENDIF
        IF(TT.GT.ZERO) THEN
c          print *,' DIFKIN:WARNING:TT > 0 ',TT
          IREJ = 1
          RETURN
        ENDIF
      ELSE
        PTRAN = SQRT(PTRAN)
      ENDIF
      XI = PI2*DRNDM(PTRAN)
C
C  outgoing momenta in cms system
      PMOM1(4) = E1F
      PMOM1(1) = PTRAN*COS(XI)
      PMOM1(2) = PTRAN*SIN(XI)
      PMOM1(3) = PLONG
      PMOM1(5) = XMP1
C
      PMOM2(4) = E2F
      PMOM2(1) = -PMOM1(1)
      PMOM2(2) = -PMOM1(2)
      PMOM2(3) = -PLONG
      PMOM2(5) = XMP2
      IREJ = 0
C
C  debug output / consistency check
      IF(IDEB(49).GE.0) THEN
C  check kinematics
        XM1 = PMOM1(4)**2-PMOM1(1)**2-PMOM1(2)**2-PMOM1(3)**2
        XM1 = SIGN(SQRT(ABS(XM1)),XM1)
        XM2 = PMOM2(4)**2-PMOM2(1)**2-PMOM2(2)**2-PMOM2(3)**2
        XM2 = SIGN(SQRT(ABS(XM2)),XM2)
        IF((ABS(XM1-XMP1).GT.DEPS).OR.(ABS(XM1-XMP1).GT.DEPS)) THEN
          WRITE(6,'(1X,A,/5X,4E11.4)')
     &      'DIFKIN:WARNING:INCON.MASSES:MINI-1,MOUT-1,MINI-2,MOUT-2',
     &      XMP1,XM1,XMP2,XM2
          CALL POPREV(-1)
        ENDIF
C  output
        IF(IDEB(49).GT.10) THEN
          WRITE(6,'(1X,A,5E11.3,/1X,A,5E11.3)') 
     &      'DIFKIN:DEBUG:P1',PMOM1,'             P2',PMOM2
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE VECRES(IVEC,RMASS,IDPDG,IDBAM)
C**********************************************************************
C
C     sampling of vector meson resonance in diffractive processes
C
C     input:   common QEVECM   VDMFAC factors
C
C     output:  IVEC         1   incoming hadron
C                           2   rho 0
C                           3   omega
C                           4   phi
C                           5   pi+/pi- background
C              RMASS        mass of vector meson (GeV)
C              IDPDG        particle ID according to PDG
C              IDBAM        particle ID according to BAMJET
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(EPS  = 1.D-10,
     &          IONE = 1,
     &          PI   = 3.14159265359D0,
     &          PI2  = 6.28318530718D0,
     &          ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
      COMMON /QEVECM/ VMAS(4),GAMM(4),RMIN(4),RMAX(4),VMSL(4),VMFA(4),
     &                TMIN(4),TMAX(4)
C  particle code translation
      DIMENSION ITRANS(4),IPOS(4)
C                  rho0,omega,phi,pi+/pi-
      DATA ITRANS /113, 223, 333, 6666 /
      DATA IPOS   / 33,  35,  96,   33 /
C
      IDPDO = IDPDG
      IF(IDPDG.EQ.22) THEN
        XI = DRNDM(RMASS)*(VMFA(1)+VMFA(2)+VMFA(3)+VMFA(4))
        SUM = ZERO
        DO 55 K=1,4
          SUM = SUM + VMFA(K)
          IF(XI.LE.SUM) GOTO 65
 55     CONTINUE
 65     CONTINUE
C
        IDPDG = ITRANS(K)
        IDBAM = IPOS(K)
        IVEC  = K+1
C  sample mass of vector meson
        CALL SAMASS(IDPDG,RMASS)
      ELSE IF(IDPDG.EQ.777) THEN
        IF(IHFLD(1,1).EQ.0) THEN
          CALL SEAFLA(1,IFL1,IFL2,RMASS)
          CALL HACODE(IFL1,IFL2,IDBA1,IDBA2)
        ELSE
          CALL HACODE(IHFLD(1,1),IHFLD(1,2),IDBA1,IDBA2)
        ENDIF
        RMAS1 = POPAMA(IDBA1,0)
        RMAS2 = POPAMA(IDBA2,0)
        IF((IDBA2.NE.0).AND.
     &    (DRNDM(RMAS1).LT.(RMAS1/(RMAS1+RMAS2)))) THEN
          IDBAM = IDBA2
          RMASS = RMAS2
        ELSE
          IDBAM = IDBA1
          RMASS = RMAS1
        ENDIF
        IDPDG = MPDGHA(IDBAM)
        IVEC = 1
      ELSE IF(IDPDG.EQ.888) THEN
        IF(IHFLD(2,1).EQ.0) THEN
          CALL SEAFLA(2,IFL1,IFL2,RMASS)
          CALL HACODE(IFL1,IFL2,IDBA1,IDBA2)
        ELSE
          CALL HACODE(IHFLD(2,1),IHFLD(2,2),IDBA1,IDBA2)
        ENDIF
        RMAS1 = POPAMA(IDBA1,0)
        RMAS2 = POPAMA(IDBA2,0)
        IF((IDBA2.NE.0).AND.
     &    (DRNDM(RMAS1).LT.(RMAS1/(RMAS1+RMAS2)))) THEN
          IDBAM = IDBA2
          RMASS = RMAS2
        ELSE
          IDBAM = IDBA1
          RMASS = RMAS1
        ENDIF
        IDPDG = MPDGHA(IDBAM)
        IVEC = 1
      ENDIF
C  debug output
      IF(IDEB(47).GE.5) THEN
        WRITE(6,'(1X,A,/10X,3I7,E12.4)')
     &    'VECRES:DEBUG:IDPDG-OLD,IDPDG,IDBAM,MASS',
     &    IDPDO,IDPDG,IDBAM,RMASS
      ENDIF
      END
C
C
      SUBROUTINE DIFRES(IDMOTH,IVAL1,IVAL2,
     &                  IDPDG,IDBAM,RMASS,RGAM,RWG,LISTL)
C**********************************************************************
C
C     list of resonance states for low mass resonances
C
C     input:   IDMOTH       PDG ID of mother particle
C              IVAL1,2      quarks (photon only)
C
C     output:  IDPDG        list of PDG IDs for possible resonances
C              IDBAM        list of corresponding CPC IDs
C              RMASS        mass
C              RGAMS        decay width
C              RMASS        additional weight factor
C              LISTL        entries in current list
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION  IDPDG(10),IDBAM(10),RMASS(10),RGAM(10),RWG(10)
C
      PARAMETER (EPS    =  1.D-10,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-40)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     &            IBAR(187),K1(187),K2(187)
C
      DIMENSION RWGHT(20),IRPDG(20),IRBAM(20)
      DATA IRPDG /113, 223, 333, 50223, 40113, 60223, 10333, 30113,
     &            12212, 42212, -12212, -42212,
     &            8*0 /
      DATA IRBAM /33, 35, 96, 26, 27, 28, 29, 30,
     &            61, 65, 77, 79,
     &            8*0 /
      DATA RWGHT /1.D0, 0.11D0, 0.1D0, 0.11D0, 1.D0, 0.11D0, 0.1D0,
     &            1.D0, 1.D0, 1.D0, 1.D0, 1.D0,
     &            8*1.D0 /
C
      LISTL = 0
      IF(IDMOTH.EQ.22) THEN
        I1 = 4
        I2 = 8
      ELSE IF(IDMOTH.EQ.2212) THEN
        I1 = 9
        I2 = 10
      ELSE IF(IDMOTH.EQ.-2212) THEN
        I1 = 11
        I2 = 12
      ELSE
        RETURN
      ENDIF
C
      DO 100 I=I1,I2
        LISTL = LISTL+1
        IDBAM(LISTL) = IRBAM(I)
        IDPDG(LISTL) = IRPDG(I)
        RMASS(LISTL) = AM(IDBAM(LISTL))
        RGAM(LISTL)  = GA(IDBAM(LISTL))
        RWG(LISTL)   = RWGHT(I)
 100  CONTINUE
      IF(IDEB(85).GE.20) THEN
        WRITE(6,'(1X,A,3I7)') 'DIFRES:DEBUG:MOTHER,QUARKS',IDMOTH,
     &    IVAL1,IVAL2
        DO 200 I=1,LISTL
          WRITE(6,'(1X,I3,2I7,E12.4)') I,IDBAM(I),IDPDG(I),RMASS(I)
 200    CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE MASSAD(IFLMO,IFL1,IFL2,PMASS,XMCON,XMOUT,IDPDG,IDBAM)
C***********************************************************************
C
C    fine correction of low mass strings to mass of corresponding
C    resonance or two particle threshold
C
C    input:     IFLMO         PDG ID of mother particle
C               IFL1,2        requested parton flavours
C                             (not used at the moment)
C               PMASS         reference mass (mass of mother particle)
C               XMCON         conjecture of mass
C
C    output:    XMOUT         output mass (adjusted input mass)
C                             moved ot nearest mass possible
C               IDPDG         PDG resonance ID
C               IDBAM         BAMJET resonance ID
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-8)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  particle properties
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     &IBAR(187),K1(187),K2(187)
C  decay channels
      COMMON/DECAYC/ ZKNAME(550),NZK(550,3),WT(550)
C
      DIMENSION XWG(10),RMA(10),RGA(10),RWG(10),IRPDG(10),IRBAM(10)
C
      XMINP = XMCON
      IDPDG = 0
      IDBAM = 0
      XMOUT = XMINP
C  resonance treatment activated?
      IF(ISWMDL(23).EQ.0) RETURN
C
      CALL DIFRES(IFLMO,IFL1,IFL2,IRPDG,IRBAM,RMA,RGA,RWG,LISTL)
      IF(LISTL.LT.1) THEN
        IF(IDEB(7).GE.2) WRITE(6,'(1X,A,3I7)')
     &    'MASSAD:WARNING:NO RESONANCES (IFMO,IF1,IF2)',
     &    IFLMO,IFL1,IFL2
        GOTO 50
      ENDIF
C  mass small?
      PMASSL = (PMASS+0.15D0)**2
      XMINP2 = XMINP**2
C  determine resonance probability
      DM2 = 1.1D0
      RPROB = (PMASSL+DM2)*(XMINP2-PMASSL)/(DM2*XMINP2)
      IF(RPROB.LT.DRNDM(PMASSL)) THEN
C  sample new resonance
        XWGSUM = ZERO
        DO 100 I=1,LISTL
          XWG(I) = RWG(I)/RMA(I)**2
          XWGSUM = XWGSUM+XWG(I)
 100    CONTINUE
C
        ITER = 0
 150    CONTINUE
        ITER = ITER+1
        IF(ITER.GE.5) THEN
          IDBAM = 0
          IDPDG = 0
          XMOUT = XMINP
          GOTO 50
        ENDIF
C
        I = 0
        XI = XWGSUM*DRNDM(XMOUT)
 200    CONTINUE
          I = I+1
          XWGSUM = XWGSUM-XWG(I)
        IF((XI.LT.XWGSUM).AND.(I.LT.LISTL)) GOTO 200
        IDPDG = IRPDG(I)
        IDBAM = IRBAM(I)
        GARES = RGA(I)
        XMRES = RMA(I)
        XMRES2 = XMRES**2
C  sample new mass (relativistic Breit-Wigner shape)
        ALO = ATAN((PMASSL+DM2-XMRES2)/(XMRES*GARES))
        AHI = ATAN((5.D0-XMRES2)/(XMRES*GARES))
        XI = (AHI-ALO)*DRNDM(XMRES)+ALO
        XMOUT = XMRES*GARES*TAN(XI)+XMRES2
        XMOUT = SQRT(XMOUT)
C  check mass for decay
        AMDCY = TWO*XMRES
        DO 250 IK=K1(IDBAM),K2(IDBAM)
          AMSUM = ZERO
          DO 275 I=1,3
            IF(NZK(IK,I).NE.0) AMSUM = AMSUM+AM(NZK(IK,I))
 275      CONTINUE
          AMDCY = MIN(AMDCY,AMSUM)
 250    CONTINUE
        IF(AMDCY.GE.XMOUT) GOTO 150
C  debug output
        IF(IDEB(7).GE.10)
     &    WRITE(6,'(1X,A,/1X,3I6,2E10.3,2I7,2E10.3)')
     &    'MASSAD:DEBUG:IFMO,IF1,IF2,XMCON,XMOUT,IDPDG,IDBAM,RMA,RGA',
     &    IFLMO,IFL1,IFL2,XMCON,XMOUT,IDPDG,IDBAM,RMA(I),RGA(I)
        RETURN
      ENDIF
C
 50   CONTINUE
C  debug output
      IF(IDEB(7).GE.15)
     &  WRITE(6,'(1X,A,/1X,3I6,2E10.3)')
     &    'MASSAD:DEBUG:CHAIN SAMPLED: IFMO,IF1,IF2,XMCON,XMOUT',
     &    IFLMO,IFL1,IFL2,XMCON,XMOUT
      END
c      SUBROUTINE FITPAR(IOUTP)
C**********************************************************************
C
C     read input parameters according to PDFs
C
C**********************************************************************
c      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c      SAVE
C
c      PARAMETER(ZERO=0.D0,
c     &           ONE=1.D0,
c     &          DEFA=-99999.D0,
c     &          DEFB=-100000.D0,
c     &         THOUS=1.D3)
C
c      PARAMETER (NMAXD=100)
c      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
c     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
c      CHARACTER*8 MDLNA
c      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
c      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
c      CHARACTER*8 PDFNAM
c      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
c     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
c      COMMON /INPUTS/ PARINP(30)
c      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
c     &                ALREG,ALREGP,GR(2),B0REG(2),
c     &                GPPP,GPPR,B0PPP,B0PPR,
c     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
c      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
c      DIMENSION   INUM(3),GPS(2),GRS(2),IFPAS(2)
c      CHARACTER*8 CNAME8,PDFNA1,PDFNA2
c      CHARACTER*10 CNAM10
c      DATA CNAME8 /'        '/
c      DATA CNAM10 /'          '/
c      DATA INIT / 0 /
c      DATA IFPAS / 0, 0 /
C
c      IF((INIT.EQ.1).AND.
c     &   (IFPAP(1).EQ.IFPAS(1)).AND.
c     &   (IFPAP(2).EQ.IFPAS(2))) GOTO 1300
c      INIT=1
c      IFPAS(1) = IFPAP(1)
c      IFPAS(2) = IFPAP(2)
C  parton distribution functions
c      CALL ACTPDF(IFPAP(1),PVIRT(1),1)
c      CALL GETPDF(1,PDFNA1,ALAM,Q2MIN,Q2MAX,XMIN,XMAX)
c      CALL ACTPDF(IFPAP(2),PVIRT(2),2)
c      CALL GETPDF(2,PDFNA2,ALAM,Q2MIN,Q2MAX,XMIN,XMAX)
c      IF(IDEB(54).GE.0) THEN
c        WRITE(6,'(/1X,A,I7,2X,A,3I7)') 'FITPAR:DEBUG: LOOKING FOR PDF',
c     &    IFPAP(1),PDFNA1,IGRP(1),ISET(1),IEXT(1)
c        WRITE(6,'(1X,A,I7,2X,A,3I7)') 'FITPAR:DEBUG: LOOKING FOR PDF',
c     &    IFPAP(2),PDFNA2,IGRP(2),ISET(2),IEXT(2)
c      ENDIF
c*     GOTO 1010
C  get parameters of soft cross sections from parameter table
c      OPEN(12,FILE='fitpar.dat',ERR=1010,STATUS='OLD')
c 100  CONTINUE
c        READ(12,'(A8)',ERR=1020,END=1010) CNAME8
c        IF(CNAME8.EQ.'STOP') GOTO 1010
c        IF(CNAME8.EQ.'NEXTDATA') THEN
c          READ(12,'(I8,2X,A8,3I6)',ERR=1020,END=1010) IDPA1,CNAME8,INUM
c          IF((IDPA1.EQ.IFPAP(1)).AND.(CNAME8.EQ.PDFNA1)
c     &       .AND.(INUM(1).EQ.IGRP(1)).AND.(INUM(2).EQ.ISET(1))) THEN
c            READ(12,'(I8,2X,A8,3I6)',ERR=1020,END=1010)IDPA2,CNAME8,INUM
c            IF((IDPA2.EQ.IFPAP(2)).AND.(CNAME8.EQ.PDFNA2)
c     &         .AND.(INUM(1).EQ.IGRP(2)).AND.(INUM(2).EQ.ISET(2))) THEN
c              WRITE(6,'(/1X,A)') 'FITPAR:DEBUG: PARAMETER SET FOUND'
c              READ(12,*) ALPOM,ALPOMP,GP,B0POM
c              READ(12,*) ALREG,ALREGP,GR,B0REG
c              READ(12,*) GPPP,B0PPP,GPPR,B0PPR
c              READ(12,*) VDMFAC(1),VDMFAC(2),VDMFAC(3),VDMFAC(4)
c              READ(12,*) B0HAR
c              READ(12,*) AKFAC
c              READ(12,*) PHISUP
c              READ(12,*) RMASS1,RMASS2,VAR
c              GOTO 1100
c            ENDIF
c          ENDIF
c        ENDIF
c      GOTO 100
c 1020 CONTINUE
c        WRITE(6,'(A)') ' FITPAR:ERROR: cannot read file fitpar.dat'
c        WRITE(6,'(A,A10,A8)') ' last data card: ',CNAM10,CNAME8
c 1010 CONTINUE
c        WRITE(6,'(A)') ' FITPAR:WARNING: cannot find parameters'
c        WRITE(6,'(A,A8,2X,A8)') ' PDF sets: ',PDFNA1,PDFNA2
c        WRITE(6,'(A)') ' (initialization with default parameter values)'
C
C  general parameters
c        ALPOM     = 1.097
c        ALPOMP    = .250
c        ALREG     = .490
c        ALREGP    = 1.000
c        AKFAC     = 1.000
c        VAR       = 3.000
c        GPPP      = .170
c        B0PPP     = .500
c        GPPR      = PARMDL(7)*GPPP
c        B0PPR     = .300
C
C  gamma-gamma scattering
c        IF((IFPAP(1).EQ.22).AND.(IFPAP(2).EQ.22)) THEN
c          GP(1)     = 2.745
c          GP(2)     = GP(1)
c          B0POM(1)  = 1.225
c          B0POM(2)  = B0POM(1)
c          B0HAR     = 3.200
c          GR(1)     = 4.740
c          GR(2)     = GR(1)
c          B0REG(1)  = 0.462 
c          B0REG(2)  = B0REG(1)
c          VDMFAC(1) = .00431
c          VDMFAC(2) = .00008
c          VDMFAC(3) = VDMFAC(1)
c          VDMFAC(4) = VDMFAC(2)
c          PHISUP(1) = .7
c          PHISUP(2) = PHISUP(1)
c          RMASS1    = 1.000
c          RMASS2    = RMASS1
C
C  p-gamma scattering
c        ELSE IF((ABS(IFPAP(1)).EQ.2212)
c     &     .AND.(IFPAP(2).EQ.22)) THEN
c          GP(1)     = 6.827
c          GP(2)     = 2.745
c          B0HAR     = 3.200
c          B0POM(1)  = 1.136
c          B0POM(2)  = 1.225
c          GR(1)     = 11.740
c          GR(2)     = 4.721
c          B0REG(1)  =  .428
c          B0REG(2)  =  .462
c          VDMFAC(1) = 1.0000
c          VDMFAC(2) = .00000
c          VDMFAC(3) = .00431
c          VDMFAC(4) = .00008
c          PHISUP(1) = .600
c          PHISUP(2) = .700
c          RMASS1    = 1.100
c          RMASS2    = 1.000
C
C  gamma-p scattering
c        ELSE IF((IFPAP(1).EQ.22)
c     &          .AND.(ABS(IFPAP(2)).EQ.2212)) THEN
c          GP(1)     = 2.745
c          GP(2)     = 6.827
c          B0HAR     = 3.200
c          B0POM(1)  = 1.225
c          B0POM(2)  = 1.136
c          GR(1)     = 4.721 
c          GR(2)     = 11.740
c          B0REG(1)  = .462
c          B0REG(2)  = .428
c          VDMFAC(1) = .00431
c          VDMFAC(2) = .00008
c          VDMFAC(3) = 1.0000
c          VDMFAC(4) = .00000
c          PHISUP(1) = .700
c          PHISUP(2) = .600
c          RMASS1    = 1.000
c          RMASS2    = 1.100
C
C  p-p scattering
c        ELSE IF((ABS(IFPAP(1)).EQ.2212)
c     &          .AND.(ABS(IFPAP(2)).EQ.2212)) THEN
c          GP(1)     = 6.827
c          GP(2)     = GP(1)
c          B0HAR     = 3.2000
c          B0POM(1)  = 1.136
c          B0POM(2)  = B0POM(1)
c          GR(1)     = 13.098
c          GR(2)     = GR(1)
c          B0REG(1)  = 1.746
c          B0REG(2)  = B0REG(1)
c          VDMFAC(1) = 1.0000
c          VDMFAC(2) = .00000
c          VDMFAC(3) = VDMFAC(1)
c          VDMFAC(4) = VDMFAC(2)
c          PHISUP(1) = .600
c          PHISUP(2) = PHISUP(1)
c          RMASS1    = 1.100
c          RMASS2    = RMASS1
C
C  p-pi scattering
c        ELSE IF((ABS(IFPAP(2)).EQ.211)
c     &          .AND.(ABS(IFPAP(1)).EQ.2212)) THEN
c          GP(1)     = 6.827
c          B0HAR     = 3.200
c          B0POM(1)  = 1.136
c          GR(1)     = 13.098
c          B0REG(1)  = 1.746
c          PHISUP(1) = .600
c          PHISUP(2) = .700
c          RMASS1    = 1.100
c          RMASS2    = 0.500
c          GP(2) = GP(1)*2.D0/3.D0
c          GR(2) = GR(1)*2.D0/3.D0
c          B0REG(2) = B0REG(1)*0.8D0
c          B0POM(2) = B0POM(1)*0.8D0
c          VDMFAC(1) = 1.0000
c          VDMFAC(2) = .00000
c          VDMFAC(3) = VDMFAC(1)
c          VDMFAC(4) = VDMFAC(2)
C
C  pi-p scattering
c        ELSE IF((ABS(IFPAP(2)).EQ.2212)
c     &          .AND.(ABS(IFPAP(1)).EQ.211)) THEN
c          GP(2)     = 6.827
c          B0HAR     = 3.200
c          B0POM(2)  = 1.136
c          GR(2)     = 13.098
c          B0REG(2)  = 1.746
c          PHISUP(1) = .700
c          PHISUP(2) = .600
c          RMASS1    = 0.5
c          RMASS2    = 1.100
c          GP(1) = GP(2)*2.D0/3.D0
c          GR(1) = GR(2)*2.D0/3.D0
c          B0REG(1) = B0REG(2)*0.8D0
c          B0POM(1) = B0POM(2)*0.8D0
c          VDMFAC(1) = 1.0000
c          VDMFAC(2) = .00000
c          VDMFAC(3) = VDMFAC(1)
c          VDMFAC(4) = VDMFAC(2)
C
C  pomeron-pomeron scattering
c        ELSE IF((IFPAP(1).EQ.45).AND.(IFPAP(2).EQ.45)) THEN
c          GP(1)     = 0.12
c          GP(2)     = GP(1)
c          B0POM(1)  = 0.500
c          B0POM(2)  = B0POM(1)
c          B0HAR     = 0.100
c          GR(1)     = 0.25
c          GR(2)     = GR(1)
c          B0REG(1)  = 0.500
c          B0REG(2)  = B0REG(1)
c          VDMFAC(1) = 1.0000
c          VDMFAC(2) = .00000
c          VDMFAC(3) = VDMFAC(1)
c          VDMFAC(4) = VDMFAC(2)
c          PHISUP(1) = .662
c          PHISUP(2) = PHISUP(1)
c          RMASS1    = 1.100
c          RMASS2    = RMASS1
C
C  unknown particles
c        ELSE
c          WRITE(6,'(//1X,A,2I8)')
c     &      'FITPAR:ERROR:UNSUPPORTED PARTICLE COMBINATION',
c     &      IFPAP(1),IFPAP(2)
c          STOP
c        ENDIF
c 1100 CONTINUE
c      CLOSE(12)
c      GPS(1) = GP(1)
c      GPS(2) = GP(2)
c      GRS(1) = GR(1)
c      GRS(2) = GR(2)
c 1300 CONTINUE
C
C  overwrite parameters with input data cards
c      IF(PARINP(1).GT.DEFA) THEN
c        ALPOM     = PARINP(1)
c        PARINP(1) = DEFB
c      ENDIF
c      IF(PARINP(2).GT.DEFA) THEN
c        ALPOMP    = PARINP(2)
c        PARINP(2) = DEFB
c      ENDIF
c      IF(PARINP(3).GT.DEFA) THEN
c        GP(1)     = PARINP(3)
c        PARINP(3) = DEFB
c      ENDIF
c      IF(PARINP(4).GT.DEFA) THEN
c        GP(2)     = PARINP(4)
c        PARINP(4) = DEFB
c      ENDIF
c      IF(PARINP(5).GT.DEFA) THEN
c        B0POM(1)  = PARINP(5)
c        PARINP(5) = DEFB
c      ENDIF
c      IF(PARINP(6).GT.DEFA) THEN
c        B0POM(2)  = PARINP(6)
c        PARINP(6) = DEFB
c      ENDIF
c      IF(PARINP(7).GT.DEFA) THEN
c        ALREG     = PARINP(7)
c        PARINP(7) = DEFB
c      ENDIF
c      IF(PARINP(8).GT.DEFA) THEN
c        ALREGP    = PARINP(8)
c        PARINP(8) = DEFB
c      ENDIF
c      IF(PARINP(9).GT.DEFA) THEN
c        GR(1)     = PARINP(9)
c        PARINP(9) = DEFB
c      ENDIF
c      IF(PARINP(10).GT.DEFA) THEN
c        GR(2)      = PARINP(10)
c        PARINP(10) = DEFB
c      ENDIF
c      IF(PARINP(11).GT.DEFA) THEN
c        B0REG(1)  = PARINP(11)
c        PARINP(11) = DEFB
c      ENDIF
c      IF(PARINP(12).GT.DEFA) THEN
c        B0REG(2)  = PARINP(12)
c        PARINP(12) = DEFB
c      ENDIF
c      IF(PARINP(13).GT.DEFA) THEN
c        GPPP      = PARINP(13)
c        PARINP(13) = DEFB
c      ENDIF
c      IF(PARINP(14).GT.DEFA) THEN
c        B0PPP     = PARINP(14)
c        PARINP(14)= DEFB
c      ENDIF
c      IF(PARINP(15).GT.DEFA) THEN
c        VDMFAC(1) = PARINP(15)
c        PARINP(15)= DEFB
c      ENDIF
c      IF(PARINP(16).GT.DEFA) THEN
c        VDMFAC(2) = PARINP(16)
c        PARINP(16)= DEFB
c      ENDIF
c      IF(PARINP(17).GT.DEFA) THEN
c        VDMFAC(3) = PARINP(17)
c        PARINP(17)= DEFB
c      ENDIF
c      IF(PARINP(18).GT.DEFA) THEN
c        VDMFAC(4) = PARINP(18)
c        PARINP(18)= DEFB
c      ENDIF
c      IF(PARINP(19).GT.DEFA) THEN
c        B0HAR     = PARINP(19)
c        PARINP(19)= DEFB
c      ENDIF
c      IF(PARINP(20).GT.DEFA) THEN
c        AKFAC     = PARINP(20)
c        PARINP(20)= DEFB
c      ENDIF
c      IF(PARINP(21).GT.DEFA) THEN
c        PHISUP(1) = PARINP(21)
c        PARINP(21)= DEFB
c      ENDIF
c      IF(PARINP(22).GT.DEFA) THEN
c        PHISUP(2) = PARINP(22)
c        PARINP(22)= DEFB
c      ENDIF
c      IF(PARINP(23).GT.DEFA) THEN
c        RMASS1    = PARINP(23)
c        PARINP(23)= DEFB
c      ENDIF
c      IF(PARINP(24).GT.DEFA) THEN
c        RMASS2    = PARINP(24)
c        PARINP(24)= DEFB
c      ENDIF
c      IF(PARINP(25).GT.DEFA) THEN
c        VAR       = PARINP(25)
c        PARINP(25)= DEFB
c      ENDIF
c      IF(PARINP(27).GT.DEFA) THEN
c        GPPR      = PARINP(27)
c        PARINP(27)= DEFB
c      ENDIF
c      IF(PARINP(28).GT.DEFA) THEN
c        B0PPR     = PARINP(28)
c        PARINP(28)= DEFB
c      ENDIF
C
C  VDM couplings
c      RHOM2 = 0.6D0
c*     ALPHA = 1.1D0
c      ALPHA = 1.D0
c      IF(IFPAP(1).EQ.22) THEN
c        VDMQ2F(1) = RHOM2/(RHOM2+PVIRT(1))*VDMFAC(1)
c        VDMQ2F(2) = RMASS1**2/(RMASS1**2+PVIRT(1))*VDMFAC(2)
c*       GP(1) = GPS(1)*(RHOM2/(RHOM2+PVIRT(1)))**ALPHA
c*       GR(1) = GRS(1)*(RHOM2/(RHOM2+PVIRT(1)))**ALPHA
c      ELSE
c        VDMQ2F(1) = VDMFAC(1)
c        VDMQ2F(2) = VDMFAC(2)
c      ENDIF
c      IF(IFPAP(2).EQ.22) THEN
c        VDMQ2F(3) = RHOM2/(RHOM2+PVIRT(2))*VDMFAC(3)
c        VDMQ2F(4) = RMASS2**2/(RMASS2**2+PVIRT(2))*VDMFAC(4)
c*       GP(2) = GPS(2)*(RHOM2/(RHOM2+PVIRT(2)))**ALPHA
c*       GR(2) = GRS(2)*(RHOM2/(RHOM2+PVIRT(2)))**ALPHA
c      ELSE
c        VDMQ2F(3) = VDMFAC(3)
c        VDMQ2F(4) = VDMFAC(4)
c      ENDIF
c        VDMQ2F(1) = VDMFAC(1)
c        VDMQ2F(2) = VDMFAC(2)
c        VDMQ2F(3) = VDMFAC(3)
c        VDMQ2F(4) = VDMFAC(4)
cC  output of parameter set
c      IF((IDEB(54).GE.5).OR.(IOUTP.GT.0)) THEN
c        WRITE(6,'(/,A,/,A)') ' FITPAR:DEBUG:SOFT PARAMETER SET:',
c     &                       ' ==============================='
c        WRITE(6,'(2(A,F7.3),2(A,2F9.3))')
c     &  '  ALPOM:',ALPOM,' ALPOMP:',ALPOMP,' GP:',GP,' B0POM:',
c     &  B0POM
c        WRITE(6,'(2(A,F7.3),2(A,2F9.3))')
c     &  '  ALREG:',ALREG,' ALREGP:',ALREGP,' GR:',GR,' B0REG:',
c     &  B0REG
c        WRITE(6,'(4(A,F7.3))')
c     &  '  GPPP :',GPPP,' B0PPP:',B0PPP,' GPPR :',GPPR,' B0PPR:',B0PPR
c        WRITE(6,'(A,4F10.5)') ' VDMFAC:',VDMFAC
c        WRITE(6,'(A,4F10.5)') ' VDMQ2F:',VDMQ2F
c        WRITE(6,'(A,F8.3)')  '  B0HAR:',B0HAR
c        WRITE(6,'(A,F8.3)')  '  AKFAC:',AKFAC
c        WRITE(6,'(A,2F8.3)') ' PHISUP:',PHISUP
c        WRITE(6,'(A,3F8.3)') '  RMASS:',RMASS1,RMASS2,VAR
c      ENDIF
c      CALL POHINI(1,IFPAP(1),IFPAP(2),PVIRT(1),PVIRT(2),6,IOUTP)
c      END
cC
C
      SUBROUTINE FITPAR(IOUTP)
C**********************************************************************
C
C     read input parameters according to PDFs
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(ZERO=0.D0,
     &           ONE=1.D0,
     &          DEFA=-99999.D0,
     &          DEFB=-100000.D0,
     &         THOUS=1.D3)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      COMMON /INPUTS/ PARINP(30)
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
      DIMENSION   INUM(3),GPS(2),GRS(2),IFPAS(2)
      CHARACTER*8 CNAME8,PDFNA1,PDFNA2
      CHARACTER*10 CNAM10

      INTEGER XIDPA1(10),XINUM1(10,3),XIDPA2(10),XINUM2(10,3)
      CHARACTER*8 XCNAM81(10),XCNAM82(10)
      PARAMETER(NSET=10)
      REAL ARRAY(10,27)

      DATA CNAME8 /'        '/
      DATA CNAM10 /'          '/
      DATA INIT / 0 /
      DATA IFPAS / 0, 0 /
C
C CREATED FROM FITPAR.DAT USING CREATE_FITPAR_CODE.FOR
C BY A.J.FINCH 29TH MAY 1996
C
      XIDPA1( 1) =       2212
      XCNAM81( 1) = 'GRV94 LO'
      XINUM1( 1,1) =          5
      XINUM1( 1,2) =          6
      XINUM1( 1,3) =          0
      XIDPA2( 1) =       2212
      XCNAM82( 1) = 'GRV94 LO'
      XINUM2( 1,1) =          5
      XINUM2( 1,2) =          6
      XINUM2( 1,3) =          0
      ARRAY( 1, 1) =   1.09700
      ARRAY( 1, 2) =   0.25000
      ARRAY( 1, 3) =   6.82700
      ARRAY( 1, 4) =   6.82700
      ARRAY( 1, 5) =   1.13600
      ARRAY( 1, 6) =   1.13600
      ARRAY( 1, 7) =   0.49000
      ARRAY( 1, 8) =   1.00000
      ARRAY( 1, 9) =  10.09800
      ARRAY( 1,10) =  10.09800
      ARRAY( 1,11) =   1.74600
      ARRAY( 1,12) =   1.74600
      ARRAY( 1,13) =   0.17000
      ARRAY( 1,14) =   0.50000
      ARRAY( 1,15) =   0.57600
      ARRAY( 1,16) =   0.30000
      ARRAY( 1,17) =   1.00000
      ARRAY( 1,18) =   0.00000
      ARRAY( 1,19) =   1.00000
      ARRAY( 1,20) =   0.00000
      ARRAY( 1,21) =   3.20000
      ARRAY( 1,22) =   1.00000
      ARRAY( 1,23) =   0.60000
      ARRAY( 1,24) =   0.60000
      ARRAY( 1,25) =   1.10000
      ARRAY( 1,26) =   1.10000
      ARRAY( 1,27) =   3.00000
      XIDPA1( 2) =      -2212
      XCNAM81( 2) = 'GRV94 LO'
      XINUM1( 2,1) =          5
      XINUM1( 2,2) =          6
      XINUM1( 2,3) =          0
      XIDPA2( 2) =       2212
      XCNAM82( 2) = 'GRV94 LO'
      XINUM2( 2,1) =          5
      XINUM2( 2,2) =          6
      XINUM2( 2,3) =          0
      ARRAY( 2, 1) =   1.09700
      ARRAY( 2, 2) =   0.25000
      ARRAY( 2, 3) =   6.82700
      ARRAY( 2, 4) =   6.82700
      ARRAY( 2, 5) =   1.13600
      ARRAY( 2, 6) =   1.13600
      ARRAY( 2, 7) =   0.49000
      ARRAY( 2, 8) =   1.00000
      ARRAY( 2, 9) =  13.09800
      ARRAY( 2,10) =  13.09800
      ARRAY( 2,11) =   1.74600
      ARRAY( 2,12) =   1.74600
      ARRAY( 2,13) =   0.17000
      ARRAY( 2,14) =   0.50000
      ARRAY( 2,15) =   0.57600
      ARRAY( 2,16) =   0.30000
      ARRAY( 2,17) =   1.00000
      ARRAY( 2,18) =   0.00000
      ARRAY( 2,19) =   1.00000
      ARRAY( 2,20) =   0.00000
      ARRAY( 2,21) =   3.20000
      ARRAY( 2,22) =   1.00000
      ARRAY( 2,23) =   0.60000
      ARRAY( 2,24) =   0.60000
      ARRAY( 2,25) =   1.10000
      ARRAY( 2,26) =   1.10000
      ARRAY( 2,27) =   3.00000
      XIDPA1( 3) =       2212
      XCNAM81( 3) = 'GRV94 LO'
      XINUM1( 3,1) =          5
      XINUM1( 3,2) =          6
      XINUM1( 3,3) =          0
      XIDPA2( 3) =      -2212
      XCNAM82( 3) = 'GRV94 LO'
      XINUM2( 3,1) =          5
      XINUM2( 3,2) =          6
      XINUM2( 3,3) =          0
      ARRAY( 3, 1) =   1.09700
      ARRAY( 3, 2) =   0.25000
      ARRAY( 3, 3) =   6.82700
      ARRAY( 3, 4) =   6.82700
      ARRAY( 3, 5) =   1.13600
      ARRAY( 3, 6) =   1.13600
      ARRAY( 3, 7) =   0.49000
      ARRAY( 3, 8) =   1.00000
      ARRAY( 3, 9) =  13.09800
      ARRAY( 3,10) =  13.09800
      ARRAY( 3,11) =   1.74600
      ARRAY( 3,12) =   1.74600
      ARRAY( 3,13) =   0.17000
      ARRAY( 3,14) =   0.50000
      ARRAY( 3,15) =   0.57600
      ARRAY( 3,16) =   0.30000
      ARRAY( 3,17) =   1.00000
      ARRAY( 3,18) =   0.00000
      ARRAY( 3,19) =   1.00000
      ARRAY( 3,20) =   0.00000
      ARRAY( 3,21) =   3.20000
      ARRAY( 3,22) =   1.00000
      ARRAY( 3,23) =   0.60000
      ARRAY( 3,24) =   0.60000
      ARRAY( 3,25) =   1.10000
      ARRAY( 3,26) =   1.10000
      ARRAY( 3,27) =   3.00000
      XIDPA1( 4) =         22
      XCNAM81( 4) = 'GRV-G LO'
      XINUM1( 4,1) =          5
      XINUM1( 4,2) =          3
      XINUM1( 4,3) =          0
      XIDPA2( 4) =       2212
      XCNAM82( 4) = 'GRV94 LO'
      XINUM2( 4,1) =          5
      XINUM2( 4,2) =          6
      XINUM2( 4,3) =          0
      ARRAY( 4, 1) =   1.09700
      ARRAY( 4, 2) =   0.25000
      ARRAY( 4, 3) =   2.74500
      ARRAY( 4, 4) =   6.82700
      ARRAY( 4, 5) =   1.22500
      ARRAY( 4, 6) =   1.13600
      ARRAY( 4, 7) =   0.50000
      ARRAY( 4, 8) =   1.00000
      ARRAY( 4, 9) =   4.72100
      ARRAY( 4,10) =  11.74000
      ARRAY( 4,11) =   0.46200
      ARRAY( 4,12) =   0.42800
      ARRAY( 4,13) =   0.17000
      ARRAY( 4,14) =   0.50000
      ARRAY( 4,15) =   0.67700
      ARRAY( 4,16) =   0.30000
      ARRAY( 4,17) =   0.00431
      ARRAY( 4,18) =   0.00008
      ARRAY( 4,19) =   1.00000
      ARRAY( 4,20) =   0.00000
      ARRAY( 4,21) =   3.20000
      ARRAY( 4,22) =   1.00000
      ARRAY( 4,23) =   0.70000
      ARRAY( 4,24) =   0.60000
      ARRAY( 4,25) =   1.00000
      ARRAY( 4,26) =   1.10000
      ARRAY( 4,27) =   3.00000
      XIDPA1( 5) =       2212
      XCNAM81( 5) = 'GRV94 LO'
      XINUM1( 5,1) =          5
      XINUM1( 5,2) =          6
      XINUM1( 5,3) =          0
      XIDPA2( 5) =         22
      XCNAM82( 5) = 'GRV-G LO'
      XINUM2( 5,1) =          5
      XINUM2( 5,2) =          3
      XINUM2( 5,3) =          0
      ARRAY( 5, 1) =   1.09700
      ARRAY( 5, 2) =   0.25000
      ARRAY( 5, 3) =   6.82700
      ARRAY( 5, 4) =   2.74500
      ARRAY( 5, 5) =   1.13600
      ARRAY( 5, 6) =   1.22500
      ARRAY( 5, 7) =   0.50000
      ARRAY( 5, 8) =   1.00000
      ARRAY( 5, 9) =  11.74000
      ARRAY( 5,10) =   4.72100
      ARRAY( 5,11) =   0.42800
      ARRAY( 5,12) =   0.46200
      ARRAY( 5,13) =   0.17000
      ARRAY( 5,14) =   0.50000
      ARRAY( 5,15) =   0.67700
      ARRAY( 5,16) =   0.30000
      ARRAY( 5,17) =   1.00000
      ARRAY( 5,18) =   0.00000
      ARRAY( 5,19) =   0.00431
      ARRAY( 5,20) =   0.00008
      ARRAY( 5,21) =   3.20000
      ARRAY( 5,22) =   1.00000
      ARRAY( 5,23) =   0.60000
      ARRAY( 5,24) =   0.70000
      ARRAY( 5,25) =   1.10000
      ARRAY( 5,26) =   1.00000
      ARRAY( 5,27) =   3.00000
      XIDPA1( 6) =         22
      XCNAM81( 6) = 'GRV-G LO'
      XINUM1( 6,1) =          5
      XINUM1( 6,2) =          3
      XINUM1( 6,3) =          0
      XIDPA2( 6) =         22
      XCNAM82( 6) = 'GRV-G LO'
      XINUM2( 6,1) =          5
      XINUM2( 6,2) =          3
      XINUM2( 6,3) =          0
      ARRAY( 6, 1) =   1.09700
      ARRAY( 6, 2) =   0.25000
      ARRAY( 6, 3) =   2.74500
      ARRAY( 6, 4) =   2.74500
      ARRAY( 6, 5) =   1.22500
      ARRAY( 6, 6) =   1.22500
      ARRAY( 6, 7) =   0.50000
      ARRAY( 6, 8) =   1.00000
      ARRAY( 6, 9) =   4.74000
      ARRAY( 6,10) =   4.72100
      ARRAY( 6,11) =   0.46200
      ARRAY( 6,12) =   0.46200
      ARRAY( 6,13) =   0.17000
      ARRAY( 6,14) =   0.50000
      ARRAY( 6,15) =   0.67700
      ARRAY( 6,16) =   0.30000
      ARRAY( 6,17) =   0.00431
      ARRAY( 6,18) =   0.00008
      ARRAY( 6,19) =   0.00431
      ARRAY( 6,20) =   0.00008
      ARRAY( 6,21) =   3.20000
      ARRAY( 6,22) =   1.00000
      ARRAY( 6,23) =   0.70000
      ARRAY( 6,24) =   0.70000
      ARRAY( 6,25) =   1.00000
      ARRAY( 6,26) =   1.00000
      ARRAY( 6,27) =   3.00000
      XIDPA1( 7) =         22
      XCNAM81( 7) = 'SaS-1D  '
      XINUM1( 7,1) =          1
      XINUM1( 7,2) =          1
      XINUM1( 7,3) =          4
      XIDPA2( 7) =         22
      XCNAM82( 7) = 'SaS-1D  '
      XINUM2( 7,1) =          1
      XINUM2( 7,2) =          1
      XINUM2( 7,3) =          4
      ARRAY( 7, 1) =   1.09700
      ARRAY( 7, 2) =   0.25000
      ARRAY( 7, 3) =   3.11700
      ARRAY( 7, 4) =   3.11700
      ARRAY( 7, 5) =   1.34500
      ARRAY( 7, 6) =   1.34500
      ARRAY( 7, 7) =   0.30200
      ARRAY( 7, 8) =   1.00000
      ARRAY( 7, 9) =   6.60500
      ARRAY( 7,10) =   6.60500
      ARRAY( 7,11) =   0.17500
      ARRAY( 7,12) =   0.17500
      ARRAY( 7,13) =   0.17000
      ARRAY( 7,14) =   0.50000
      ARRAY( 7,15) =   0.61200
      ARRAY( 7,16) =   0.30000
      ARRAY( 7,17) =   0.00409
      ARRAY( 7,18) =   0.00009
      ARRAY( 7,19) =   0.00409
      ARRAY( 7,20) =   0.00009
      ARRAY( 7,21) =   3.20000
      ARRAY( 7,22) =   1.00000
      ARRAY( 7,23) =   0.70000
      ARRAY( 7,24) =   0.70000
      ARRAY( 7,25) =   1.00000
      ARRAY( 7,26) =   1.00000
      ARRAY( 7,27) =   3.00000
      XIDPA1( 8) =         22
      XCNAM81( 8) = 'SaS-1M  '
      XINUM1( 8,1) =          1
      XINUM1( 8,2) =          2
      XINUM1( 8,3) =          4
      XIDPA2( 8) =         22
      XCNAM82( 8) = 'SaS-1M  '
      XINUM2( 8,1) =          1
      XINUM2( 8,2) =          2
      XINUM2( 8,3) =          4
      ARRAY( 8, 1) =   1.09700
      ARRAY( 8, 2) =   0.25000
      ARRAY( 8, 3) =   2.55400
      ARRAY( 8, 4) =   2.55400
      ARRAY( 8, 5) =   1.09100
      ARRAY( 8, 6) =   1.09100
      ARRAY( 8, 7) =   0.50000
      ARRAY( 8, 8) =   1.00000
      ARRAY( 8, 9) =   4.25800
      ARRAY( 8,10) =   4.25800
      ARRAY( 8,11) =   0.49000
      ARRAY( 8,12) =   0.49000
      ARRAY( 8,13) =   0.17000
      ARRAY( 8,14) =   0.50000
      ARRAY( 8,15) =   0.61200
      ARRAY( 8,16) =   0.30000
      ARRAY( 8,17) =   0.00457
      ARRAY( 8,18) =   0.00010
      ARRAY( 8,19) =   0.00457
      ARRAY( 8,20) =   0.00010
      ARRAY( 8,21) =   3.20000
      ARRAY( 8,22) =   1.00000
      ARRAY( 8,23) =   0.70000
      ARRAY( 8,24) =   0.70000
      ARRAY( 8,25) =   1.00000
      ARRAY( 8,26) =   1.00000
      ARRAY( 8,27) =   3.00000
      XIDPA1( 9) =         22
      XCNAM81( 9) = 'SaS-2D  '
      XINUM1( 9,1) =          1
      XINUM1( 9,2) =          3
      XINUM1( 9,3) =          4
      XIDPA2( 9) =         22
      XCNAM82( 9) = 'SaS-2D  '
      XINUM2( 9,1) =          1
      XINUM2( 9,2) =          3
      XINUM2( 9,3) =          4
      ARRAY( 9, 1) =   1.09700
      ARRAY( 9, 2) =   0.25000
      ARRAY( 9, 3) =   2.53300
      ARRAY( 9, 4) =   2.53300
      ARRAY( 9, 5) =   1.13400
      ARRAY( 9, 6) =   1.13400
      ARRAY( 9, 7) =   0.50100
      ARRAY( 9, 8) =   1.00000
      ARRAY( 9, 9) =   4.23000
      ARRAY( 9,10) =   4.23000
      ARRAY( 9,11) =   0.49300
      ARRAY( 9,12) =   0.49300
      ARRAY( 9,13) =   0.17000
      ARRAY( 9,14) =   0.50000
      ARRAY( 9,15) =   0.61200
      ARRAY( 9,16) =   0.30000
      ARRAY( 9,17) =   0.00459
      ARRAY( 9,18) =   0.00010
      ARRAY( 9,19) =   0.00459
      ARRAY( 9,20) =   0.00010
      ARRAY( 9,21) =   3.20000
      ARRAY( 9,22) =   1.00000
      ARRAY( 9,23) =   0.70000
      ARRAY( 9,24) =   0.70000
      ARRAY( 9,25) =   1.00000
      ARRAY( 9,26) =   1.00000
      ARRAY( 9,27) =   3.00000
      XIDPA1(10) =         22
      XCNAM81(10) = 'SaS-2M  '
      XINUM1(10,1) =          1
      XINUM1(10,2) =          4
      XINUM1(10,3) =          4
      XIDPA2(10) =         22
      XCNAM82(10) = 'SaS-2M  '
      XINUM2(10,1) =          1
      XINUM2(10,2) =          4
      XINUM2(10,3) =          4
      ARRAY(10, 1) =   1.09700
      ARRAY(10, 2) =   0.25000
      ARRAY(10, 3) =   2.82200
      ARRAY(10, 4) =   2.82200
      ARRAY(10, 5) =   1.09100
      ARRAY(10, 6) =   1.09100
      ARRAY(10, 7) =   0.49100
      ARRAY(10, 8) =   1.00000
      ARRAY(10, 9) =   4.68700
      ARRAY(10,10) =   4.68700
      ARRAY(10,11) =   0.45800
      ARRAY(10,12) =   0.45800
      ARRAY(10,13) =   0.17000
      ARRAY(10,14) =   0.50000
      ARRAY(10,15) =   0.61200
      ARRAY(10,16) =   0.30000
      ARRAY(10,17) =   0.00466
      ARRAY(10,18) =   0.00003
      ARRAY(10,19) =   0.00466
      ARRAY(10,20) =   0.00003
      ARRAY(10,21) =   3.20000
      ARRAY(10,22) =   1.00000
      ARRAY(10,23) =   0.70000
      ARRAY(10,24) =   0.70000
      ARRAY(10,25) =   1.00000
      ARRAY(10,26) =   1.00000
      ARRAY(10,27) =   3.00000

C
      IF((INIT.EQ.1).AND.
     &   (IFPAP(1).EQ.IFPAS(1)).AND.
     &   (IFPAP(2).EQ.IFPAS(2))) GOTO 1300
      INIT=1
      IFPAS(1) = IFPAP(1)
      IFPAS(2) = IFPAP(2)
C  parton distribution functions
      CALL ACTPDF(IFPAP(1),PVIRT(1),1)
      CALL GETPDF(1,PDFNA1,ALAM,Q2MIN,Q2MAX,XMIN,XMAX)
      CALL ACTPDF(IFPAP(2),PVIRT(2),2)
      CALL GETPDF(2,PDFNA2,ALAM,Q2MIN,Q2MAX,XMIN,XMAX)
      IF(IDEB(54).GE.0) THEN
        WRITE(6,'(/1X,A,I7,2X,A,3I7)') 'FITPAR:DEBUG: LOOKING FOR PDF',
     &    IFPAP(1),PDFNA1,IGRP(1),ISET(1),IEXT(1)
        WRITE(6,'(1X,A,I7,2X,A,3I7)') 'FITPAR:DEBUG: LOOKING FOR PDF',
     &    IFPAP(2),PDFNA2,IGRP(2),ISET(2),IEXT(2)
      ENDIF
*     GOTO 1010
C  get parameters of soft cross sections from parameter table
C      OPEN(12,FILE='fitpar.dat',ERR=1010,STATUS='OLD')
      DO ILOOP = 1,NSET
 100  CONTINUE
C        READ(12,'(A8)',ERR=1020,END=1010) CNAME8         
C        IF(CNAME8.EQ.'STOP') GOTO 1010
C        IF(CNAME8.EQ.'NEXTDATA') THEN
C          READ(12,'(I8,2X,A8,3I6)',ERR=1020,END=1010) IDPA1,CNAME8,INUM
          IDPA1  = XIDPA1(ILOOP) 
          CNAME8 = XCNAM81(ILOOP)
          INUM(1)   = XINUM1(ILOOP,1)
          INUM(2)   = XINUM1(ILOOP,2)
          INUM(3)   = XINUM1(ILOOP,3)
          IF((IDPA1.EQ.IFPAP(1)).AND.(CNAME8.EQ.PDFNA1)
     &       .AND.(INUM(1).EQ.IGRP(1)).AND.(INUM(2).EQ.ISET(1))) THEN
C            READ(12,'(I8,2X,A8,3I6)',ERR=1020,END=1010)IDPA2,CNAME8,INUM
          IDPA2  = XIDPA2(ILOOP) 
          CNAME8 = XCNAM82(ILOOP)
          INUM(1)   = XINUM2(ILOOP,1)
          INUM(2)   = XINUM2(ILOOP,2)
          INUM(3)   = XINUM2(ILOOP,3)
            IF((IDPA2.EQ.IFPAP(2)).AND.(CNAME8.EQ.PDFNA2)
     &         .AND.(INUM(1).EQ.IGRP(2)).AND.(INUM(2).EQ.ISET(2))) THEN
              WRITE(6,'(/1X,A)') 'FITPAR:DEBUG: PARAMETER SET FOUND'
C              READ(12,*) ALPOM,ALPOMP,GP,B0POM
               ALPOM     = ARRAY(ILOOP,1)
               ALPOMP    = ARRAY(ILOOP,2)
               GP(1)     = ARRAY(ILOOP,3)
               GP(2)     = ARRAY(ILOOP,4)
               B0POM(1)  = ARRAY(ILOOP,5)
               B0POM(2)  = ARRAY(ILOOP,6)
C              READ(12,*) ALREG,ALREGP,GR,B0REG
               ALREG     = ARRAY(ILOOP,7)
               ALREGP    = ARRAY(ILOOP,8)
               GR(1)     = ARRAY(ILOOP,9)
               GR(2)     = ARRAY(ILOOP,10)
               B0REG(1)  = ARRAY(ILOOP,11)
               B0REG(2)  = ARRAY(ILOOP,12)
C              READ(12,*) GPPP,B0PPP,GPPR,B0PPR
               GPPP      = ARRAY(ILOOP,13)
               B0PPP     = ARRAY(ILOOP,14)
               GPPR      = ARRAY(ILOOP,15)
               B0PPR     = ARRAY(ILOOP,16)

C              READ(12,*) VDMFAC(1),VDMFAC(2),VDMFAC(3),VDMFAC(4)
               DO IVDM = 1,4
                VDMFAC(IVDM) = ARRAY(ILOOP, 16+IVDM)
               ENDDO
C              READ(12,*) B0HAR
               B0HAR     = ARRAY(ILOOP,21)
C              READ(12,*) AKFAC
               AKFAC     = ARRAY(ILOOP,22)
C              READ(12,*) PHISUP
               PHISUP(1)    = ARRAY(ILOOP,23)
               PHISUP(2)    = ARRAY(ILOOP,24)
C              READ(12,*) RMASS1,RMASS2,VAR
               RMASS1    = ARRAY(ILOOP,25)
               RMASS2    = ARRAY(ILOOP,26)
               VAR       = ARRAY(ILOOP,27)
              GOTO 1100
            ENDIF
          ENDIF
C        ENDIF
      enddo
c      GOTO 100
c 1020 CONTINUE
c        WRITE(6,'(A)') ' FITPAR:ERROR: cannot read file fitpar.dat'
c        WRITE(6,'(A,A10,A8)') ' last data card: ',CNAM10,CNAME8
 1010 CONTINUE
        WRITE(6,'(A)') ' FITPAR:WARNING: cannot find parameters'
        WRITE(6,'(A,A8,2X,A8)') ' PDF sets: ',PDFNA1,PDFNA2
        WRITE(6,'(A)') ' (initialization with default parameter values)'
C
C  general parameters
        ALPOM     = 1.097
        ALPOMP    = .250
        ALREG     = .490
        ALREGP    = 1.000
        AKFAC     = 1.000
        VAR       = 3.000
        GPPP      = .170
        B0PPP     = .500
        GPPR      = PARMDL(7)*GPPP
        B0PPR     = .300
C
C  gamma-gamma scattering
        IF((IFPAP(1).EQ.22).AND.(IFPAP(2).EQ.22)) THEN
          GP(1)     = 2.745
          GP(2)     = GP(1)
          B0POM(1)  = 1.225
          B0POM(2)  = B0POM(1)
          B0HAR     = 3.200
          GR(1)     = 4.740
          GR(2)     = GR(1)
          B0REG(1)  = 0.462 
          B0REG(2)  = B0REG(1)
          VDMFAC(1) = .00431
          VDMFAC(2) = .00008
          VDMFAC(3) = VDMFAC(1)
          VDMFAC(4) = VDMFAC(2)
          PHISUP(1) = .7
          PHISUP(2) = PHISUP(1)
          RMASS1    = 1.000
          RMASS2    = RMASS1
C
C  p-gamma scattering
        ELSE IF((ABS(IFPAP(1)).EQ.2212)
     &     .AND.(IFPAP(2).EQ.22)) THEN
          GP(1)     = 6.827
          GP(2)     = 2.745
          B0HAR     = 3.200
          B0POM(1)  = 1.136
          B0POM(2)  = 1.225
          GR(1)     = 11.740
          GR(2)     = 4.721
          B0REG(1)  =  .428
          B0REG(2)  =  .462
          VDMFAC(1) = 1.0000
          VDMFAC(2) = .00000
          VDMFAC(3) = .00431
          VDMFAC(4) = .00008
          PHISUP(1) = .600
          PHISUP(2) = .700
          RMASS1    = 1.100
          RMASS2    = 1.000
C
C  gamma-p scattering
        ELSE IF((IFPAP(1).EQ.22)
     &          .AND.(ABS(IFPAP(2)).EQ.2212)) THEN
          GP(1)     = 2.745
          GP(2)     = 6.827
          B0HAR     = 3.200
          B0POM(1)  = 1.225
          B0POM(2)  = 1.136
          GR(1)     = 4.721 
          GR(2)     = 11.740
          B0REG(1)  = .462
          B0REG(2)  = .428
          VDMFAC(1) = .00431
          VDMFAC(2) = .00008
          VDMFAC(3) = 1.0000
          VDMFAC(4) = .00000
          PHISUP(1) = .700
          PHISUP(2) = .600
          RMASS1    = 1.000
          RMASS2    = 1.100
C
C  p-p scattering
        ELSE IF((ABS(IFPAP(1)).EQ.2212)
     &          .AND.(ABS(IFPAP(2)).EQ.2212)) THEN
          GP(1)     = 6.827
          GP(2)     = GP(1)
          B0HAR     = 3.2000
          B0POM(1)  = 1.136
          B0POM(2)  = B0POM(1)
          GR(1)     = 13.098
          GR(2)     = GR(1)
          B0REG(1)  = 1.746
          B0REG(2)  = B0REG(1)
          VDMFAC(1) = 1.0000
          VDMFAC(2) = .00000
          VDMFAC(3) = VDMFAC(1)
          VDMFAC(4) = VDMFAC(2)
          PHISUP(1) = .600
          PHISUP(2) = PHISUP(1)
          RMASS1    = 1.100
          RMASS2    = RMASS1
C
C  p-pi scattering
        ELSE IF((ABS(IFPAP(2)).EQ.211)
     &          .AND.(ABS(IFPAP(1)).EQ.2212)) THEN
          GP(1)     = 6.827
          B0HAR     = 3.200
          B0POM(1)  = 1.136
          GR(1)     = 13.098
          B0REG(1)  = 1.746
          PHISUP(1) = .600
          PHISUP(2) = .700
          RMASS1    = 1.100
          RMASS2    = 0.500
          GP(2) = GP(1)*2.D0/3.D0
          GR(2) = GR(1)*2.D0/3.D0
          B0REG(2) = B0REG(1)*0.8D0
          B0POM(2) = B0POM(1)*0.8D0
          VDMFAC(1) = 1.0000
          VDMFAC(2) = .00000
          VDMFAC(3) = VDMFAC(1)
          VDMFAC(4) = VDMFAC(2)
C
C  pi-p scattering
        ELSE IF((ABS(IFPAP(2)).EQ.2212)
     &          .AND.(ABS(IFPAP(1)).EQ.211)) THEN
          GP(2)     = 6.827
          B0HAR     = 3.200
          B0POM(2)  = 1.136
          GR(2)     = 13.098
          B0REG(2)  = 1.746
          PHISUP(1) = .700
          PHISUP(2) = .600
          RMASS1    = 0.5
          RMASS2    = 1.100
          GP(1) = GP(2)*2.D0/3.D0
          GR(1) = GR(2)*2.D0/3.D0
          B0REG(1) = B0REG(2)*0.8D0
          B0POM(1) = B0POM(2)*0.8D0
          VDMFAC(1) = 1.0000
          VDMFAC(2) = .00000
          VDMFAC(3) = VDMFAC(1)
          VDMFAC(4) = VDMFAC(2)
C
C  pomeron-pomeron scattering
        ELSE IF((IFPAP(1).EQ.45).AND.(IFPAP(2).EQ.45)) THEN
          GP(1)     = 0.12
          GP(2)     = GP(1)
          B0POM(1)  = 0.500
          B0POM(2)  = B0POM(1)
          B0HAR     = 0.100
          GR(1)     = 0.25
          GR(2)     = GR(1)
          B0REG(1)  = 0.500
          B0REG(2)  = B0REG(1)
          VDMFAC(1) = 1.0000
          VDMFAC(2) = .00000
          VDMFAC(3) = VDMFAC(1)
          VDMFAC(4) = VDMFAC(2)
          PHISUP(1) = .662
          PHISUP(2) = PHISUP(1)
          RMASS1    = 1.100
          RMASS2    = RMASS1
C
C  unknown particles
        ELSE
          WRITE(6,'(//1X,A,2I8)')
     &      'FITPAR:ERROR:UNSUPPORTED PARTICLE COMBINATION',
     &      IFPAP(1),IFPAP(2)
          STOP
        ENDIF
 1100 CONTINUE
c      CLOSE(12)
      GPS(1) = GP(1)
      GPS(2) = GP(2)
      GRS(1) = GR(1)
      GRS(2) = GR(2)
 1300 CONTINUE
C
C  overwrite parameters with input data cards
      IF(PARINP(1).GT.DEFA) THEN
        ALPOM     = PARINP(1)
        PARINP(1) = DEFB
      ENDIF
      IF(PARINP(2).GT.DEFA) THEN
        ALPOMP    = PARINP(2)
        PARINP(2) = DEFB
      ENDIF
      IF(PARINP(3).GT.DEFA) THEN
        GP(1)     = PARINP(3)
        PARINP(3) = DEFB
      ENDIF
      IF(PARINP(4).GT.DEFA) THEN
        GP(2)     = PARINP(4)
        PARINP(4) = DEFB
      ENDIF
      IF(PARINP(5).GT.DEFA) THEN
        B0POM(1)  = PARINP(5)
        PARINP(5) = DEFB
      ENDIF
      IF(PARINP(6).GT.DEFA) THEN
        B0POM(2)  = PARINP(6)
        PARINP(6) = DEFB
      ENDIF
      IF(PARINP(7).GT.DEFA) THEN
        ALREG     = PARINP(7)
        PARINP(7) = DEFB
      ENDIF
      IF(PARINP(8).GT.DEFA) THEN
        ALREGP    = PARINP(8)
        PARINP(8) = DEFB
      ENDIF
      IF(PARINP(9).GT.DEFA) THEN
        GR(1)     = PARINP(9)
        PARINP(9) = DEFB
      ENDIF
      IF(PARINP(10).GT.DEFA) THEN
        GR(2)      = PARINP(10)
        PARINP(10) = DEFB
      ENDIF
      IF(PARINP(11).GT.DEFA) THEN
        B0REG(1)  = PARINP(11)
        PARINP(11) = DEFB
      ENDIF
      IF(PARINP(12).GT.DEFA) THEN
        B0REG(2)  = PARINP(12)
        PARINP(12) = DEFB
      ENDIF
      IF(PARINP(13).GT.DEFA) THEN
        GPPP      = PARINP(13)
        PARINP(13) = DEFB
      ENDIF
      IF(PARINP(14).GT.DEFA) THEN
        B0PPP     = PARINP(14)
        PARINP(14)= DEFB
      ENDIF
      IF(PARINP(15).GT.DEFA) THEN
        VDMFAC(1) = PARINP(15)
        PARINP(15)= DEFB
      ENDIF
      IF(PARINP(16).GT.DEFA) THEN
        VDMFAC(2) = PARINP(16)
        PARINP(16)= DEFB
      ENDIF
      IF(PARINP(17).GT.DEFA) THEN
        VDMFAC(3) = PARINP(17)
        PARINP(17)= DEFB
      ENDIF
      IF(PARINP(18).GT.DEFA) THEN
        VDMFAC(4) = PARINP(18)
        PARINP(18)= DEFB
      ENDIF
      IF(PARINP(19).GT.DEFA) THEN
        B0HAR     = PARINP(19)
        PARINP(19)= DEFB
      ENDIF
      IF(PARINP(20).GT.DEFA) THEN
        AKFAC     = PARINP(20)
        PARINP(20)= DEFB
      ENDIF
      IF(PARINP(21).GT.DEFA) THEN
        PHISUP(1) = PARINP(21)
        PARINP(21)= DEFB
      ENDIF
      IF(PARINP(22).GT.DEFA) THEN
        PHISUP(2) = PARINP(22)
        PARINP(22)= DEFB
      ENDIF
      IF(PARINP(23).GT.DEFA) THEN
        RMASS1    = PARINP(23)
        PARINP(23)= DEFB
      ENDIF
      IF(PARINP(24).GT.DEFA) THEN
        RMASS2    = PARINP(24)
        PARINP(24)= DEFB
      ENDIF
      IF(PARINP(25).GT.DEFA) THEN
        VAR       = PARINP(25)
        PARINP(25)= DEFB
      ENDIF
      IF(PARINP(27).GT.DEFA) THEN
        GPPR      = PARINP(27)
        PARINP(27)= DEFB
      ENDIF
      IF(PARINP(28).GT.DEFA) THEN
        B0PPR     = PARINP(28)
        PARINP(28)= DEFB
      ENDIF
C
C  VDM couplings
      RHOM2 = 0.6D0
*     ALPHA = 1.1D0
      ALPHA = 1.D0
      IF(IFPAP(1).EQ.22) THEN
        VDMQ2F(1) = RHOM2/(RHOM2+PVIRT(1))*VDMFAC(1)
        VDMQ2F(2) = RMASS1**2/(RMASS1**2+PVIRT(1))*VDMFAC(2)
*       GP(1) = GPS(1)*(RHOM2/(RHOM2+PVIRT(1)))**ALPHA
*       GR(1) = GRS(1)*(RHOM2/(RHOM2+PVIRT(1)))**ALPHA
      ELSE
        VDMQ2F(1) = VDMFAC(1)
        VDMQ2F(2) = VDMFAC(2)
      ENDIF
      IF(IFPAP(2).EQ.22) THEN
        VDMQ2F(3) = RHOM2/(RHOM2+PVIRT(2))*VDMFAC(3)
        VDMQ2F(4) = RMASS2**2/(RMASS2**2+PVIRT(2))*VDMFAC(4)
*       GP(2) = GPS(2)*(RHOM2/(RHOM2+PVIRT(2)))**ALPHA
*       GR(2) = GRS(2)*(RHOM2/(RHOM2+PVIRT(2)))**ALPHA
      ELSE
        VDMQ2F(3) = VDMFAC(3)
        VDMQ2F(4) = VDMFAC(4)
      ENDIF
        VDMQ2F(1) = VDMFAC(1)
        VDMQ2F(2) = VDMFAC(2)
        VDMQ2F(3) = VDMFAC(3)
        VDMQ2F(4) = VDMFAC(4)
C  output of parameter set
      IF((IDEB(54).GE.5).OR.(IOUTP.GT.0)) THEN
        WRITE(6,'(/,A,/,A)') ' FITPAR:DEBUG:SOFT PARAMETER SET:',
     &                       ' ==============================='
        WRITE(6,'(2(A,F7.3),2(A,2F9.3))')
     &  '  ALPOM:',ALPOM,' ALPOMP:',ALPOMP,' GP:',GP,' B0POM:',
     &  B0POM
        WRITE(6,'(2(A,F7.3),2(A,2F9.3))')
     &  '  ALREG:',ALREG,' ALREGP:',ALREGP,' GR:',GR,' B0REG:',
     &  B0REG
        WRITE(6,'(4(A,F7.3))')
     &  '  GPPP :',GPPP,' B0PPP:',B0PPP,' GPPR :',GPPR,' B0PPR:',B0PPR
        WRITE(6,'(A,4F10.5)') ' VDMFAC:',VDMFAC
        WRITE(6,'(A,4F10.5)') ' VDMQ2F:',VDMQ2F
        WRITE(6,'(A,F8.3)')  '  B0HAR:',B0HAR
        WRITE(6,'(A,F8.3)')  '  AKFAC:',AKFAC
        WRITE(6,'(A,2F8.3)') ' PHISUP:',PHISUP
        WRITE(6,'(A,3F8.3)') '  RMASS:',RMASS1,RMASS2,VAR
      ENDIF
      CALL POHINI(1,IFPAP(1),IFPAP(2),PVIRT(1),PVIRT(2),6,IOUTP)
      END
C
C     

      SUBROUTINE INPCS(IP,XM1,XM2,XM3,XM4)
C*********************************************************************
C
C     calculation of scaled input cross sections and slopes
C
C     input: IP               particle combination
C            XM1,XM2,XM3,XM4  masses of external lines
C            COMMON /GLOCMS/  energy and PT cut-off
C                   /REGGE/   soft and hard parameters
C                   /SIGMAS/  input cross sections
C                   /ZIGMAS/  scaled input values
C                    IFHARD   0  calculate hard input cross sections
C                             1  assume hard input cross sections exist
C
C     output: ZPOM            scaled pomeron cross section
C             ZIGR            scaled reggeon cross section
C             ZIGHR           scaled hard resolved cross section
C             ZIGHD           scaled hard direct cross section
C             ZIGT1           scaled triple-Pomeron cross section
C             ZIGT2           scaled triple-Pomeron cross section
C             ZIGL            scaled loop-Pomeron cross section
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER ( MAXPRO = 16 )
      CHARACTER*18 PROC
      COMMON /PEPROC/ PROC(0:MAXPRO)
      COMPLEX*16      SIGP,SIGR,SIGHD,SIGHR,SIGT1,SIGT2,SIGL,SIGDP,
     &                SIGD1,SIGD2,DSIGH
      COMMON /SIGMAS/ SIGP,SIGR,SIGHD,SIGHR,SIGT1(2),SIGT2(2),SIGL,
     &                SIGDP(4),SIGD1(2),SIGD2(2),DSIGH(0:MAXPRO)
      COMPLEX*16      ZIGP,ZIGR,ZIGHD,ZIGHR,ZIGT1,ZIGT2,ZIGL,ZIGDP,
     &                ZIGD1,ZIGD2,
     &                BPOM,BREG,BHAR,BHAD,BTR1,BTR2,BLOO,BDP,BD1,BD2
      COMMON /ZIGMAS/ ZIGP,ZIGR,ZIGHD,ZIGHR,ZIGT1(2),ZIGT2(2),ZIGL,
     &                ZIGDP(4),ZIGD1(2),ZIGD2(2),
     &                BPOM,BREG,BHAR,BHAD,BTR1(2),BTR2(2),BLOO,BDP(4),
     &                BD1(2),BD2(2)
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      COMMON /FITMOD/ IFMOD,IFHARD
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
C
      COMPLEX*16      CZERO,BP4,BR4,BHR4,BHD4,BT14,BT24,BD4,SP,SR,SS,
     &                BPOM1,BPOM2,BREG1,BREG2,B0HARD
      DIMENSION       SCB1(4),SCB2(4),SCG1(4),SCG2(4)
      DIMENSION       BT14(2),BT24(2),BD4(4)
      DIMENSION       DSPT(0:MAXPRO)
C
      PARAMETER(ZERO=0.D0,
     &         IZERO=0,
     &          IONE=1,
     &          ITWO=2,
     &        ITHREE=3,
     &         IFOUR=4,
     &         IFIVE=5,
     &         OHALF=0.5D0,
     &           ONE=1.D0,
     &           TWO=2.D0,
     &         THREE=3.D0,
     &          FOUR=4.D0,
     &          FIVE=5.D0,
     &         THOUS=1.D3,
     &           EPS=0.01D0,
     &          DEPS=1.D-30)
C
      DATA  INIT /0/
C
      CDABS(SS) = ABS(SS)
      DCMPLX(X,Y) = CMPLX(X,Y)
C
      IF(INIT.EQ.IZERO) THEN
        CZERO=DCMPLX(ZERO,ZERO)
        INIT=IONE
      ENDIF
C  debug output
      IF(IDEB(48).GE.10) THEN
        WRITE(6,'(/1X,A,I3,4E12.3,I3)')
     &    'INPCS:DEBUG:IP,M1..M4,IFHARD',IP,XM1,XM2,XM3,XM4,IFHARD
      ENDIF
      CALL FITPAR(0)
C  scales
      CALL SCALES(XM1,XM2,XM3,XM4,SCALE1,SCALE2,SCALB1,SCALB2)
C
C  calculate hard input cross sections (output in mb)
      IF(IFHARD.NE.1) THEN
        IF((IFHARD.EQ.0).AND.(ECMSH(IP,1).GT.1.D0)) THEN
C  double-log interpolation
          CALL POHINT(IP,ECMP,0,MAXPRO,3,4,1)
          DO 60 M=0,15
            DSIGH(M) = XSECT(3,M)
            DSPT(M)  = XSECT(4,M)
 60       CONTINUE
        ELSE
C  new calculation
          CALL POHINT(IP,ECMP,0,-2,0,0,1)
          CALL POHXTO(ECMP,PTCUT(IP),PTCUT(IP),DSIGH,DSPT)
        ENDIF
C
C  save values to calculate soft pt distribution
        VFAC = (VDMQ2F(1)+VDMQ2F(2))*(VDMQ2F(3)+VDMQ2F(4))
        DSIGHP = DSPT(9)/VFAC
        SIGH   = DSIGH(9)/VFAC
C  extract real part
        IF(IPAMDL(1).EQ.IZERO) THEN
          DO 50 I=0,MAXPRO
            DSIGH(I)=DCMPLX(DREAL(DSIGH(I)),ZERO)
 50       CONTINUE
        ENDIF
C  write out results
        IF(IDEB(48).GE.15) THEN
          WRITE(6,'(/1X,A,E12.3)')
     &       'INPCS:DEBUG:QCD-PM CROSS SECTIONS (mb)',ECMP
          DO 200 I=0,MAXPRO
            WRITE(6,'(10X,A,2E14.4)') PROC(I),DSIGH(I)
 200      CONTINUE
        ENDIF
      ENDIF
C
      SCALE = CDABS(DSIGH(15))
      IF(SCALE.LT.DEPS) THEN
        SIGHD=CZERO
      ELSE
        SIGHD=DSIGH(15)
      ENDIF
      SCALE = CDABS(DSIGH(9))
      IF(SCALE.LT.DEPS) THEN
        SIGHR=CZERO
      ELSE
        SIGHR=DSIGH(9)*SCALE1*SCALE2/VFAC
      ENDIF
C
C  calculate soft input cross sections (output in mb)
      SS=DCMPLX(ECMP**2-PMASSP(1)**2-PMASSP(2)**2+0.01D0,ZERO)
      IF(IPAMDL(1).EQ.1) THEN
C  pomeron signature
        SP=SS*DCMPLX(ZERO,-ONE)
C  reggeon signature
        SR=SS*DCMPLX(ZERO,ONE)
      ELSE
        SP=SS
        SR=SS
      ENDIF
C  coupling constants (mb**1/2)
C  particle dependent slopes (GeV**-2)
      IF(IP.EQ.1) THEN
        GP1 = GP(1)
        GP2 = GP(2)
        GR1 = GR(1)
        GR2 = GR(2)
        B0POM1 = B0POM(1)
        B0POM2 = B0POM(2)
        B0REG1 = B0REG(1)
        B0REG2 = B0REG(2)
        B0HARD = B0HAR
      ELSE IF(IP.EQ.2) THEN
        GP1 = GP(1)
        GP2 = GPPP
        GR1 = GR(1)
        GR2 = GPPR
        B0POM1 = B0POM(1)
        B0POM2 = B0PPP
        B0REG1 = B0REG(1)
        B0REG2 = B0PPR
        B0HARD = B0POM1+B0POM2
      ELSE IF(IP.EQ.3) THEN
        GP1 = GPPP
        GP2 = GP(2)
        GR1 = GPPR
        GR2 = GR(2)
        B0POM1 = B0PPP
        B0POM2 = B0POM(2)
        B0REG1 = B0PPR
        B0REG2 = B0REG(2)
        B0HARD = B0POM1+B0POM2
      ELSE IF(IP.EQ.4) THEN
        GP1 = GPPP
        GP2 = GPPP
        GR1 = GPPR
        GR2 = GPPP
        B0POM1 = B0PPP
        B0POM2 = B0PPP
        B0REG1 = B0PPR
        B0REG2 = B0PPR
        B0HARD = B0POM1+B0POM2
      ELSE
        WRITE(6,'(/1X,A,I7)') 'INPCS:ERROR:INVALID IP',IP
        CALL POABRT
      ENDIF
      GP1 = GP1*SCALE1
      GP2 = GP2*SCALE2
      GR1 = GR1*SCALE1
      GR2 = GR2*SCALE2
C  input slope parameters (GeV**-2)
      BPOM1 = B0POM1*SCALB1
      BPOM2 = B0POM2*SCALB2
      BREG1 = B0REG1*SCALB1
      BREG2 = B0REG2*SCALB2
C  effective slopes
      SCALE = SS/((XM1**2+XM3**2)*(XM2**2+XM4**2))+TWO
      BPOM = BPOM1 + BPOM2 + ALPOMP*LOG(SCALE)
      BREG = BREG1 + BREG2 + ALREGP*LOG(SCALE)
      IF(IPAMDL(9).EQ.0) THEN
        BHAR = B0HARD
        BHAD = B0HARD
      ELSE IF(IPAMDL(9).EQ.1) THEN
        BHAR = B0HARD*(SCALB1+SCALB2)/TWO
        BHAD = BHAR
      ELSE IF(IPAMDL(9).EQ.2) THEN
        BHAR = BPOM1+BPOM2
        BHAD = BHAR
      ELSE
        BHAR = BPOM
        BHAD = BPOM
      ENDIF
C  input cross section pomeron
      SIGP=GP1*GP2*EXP((ALPOM-ONE)*LOG(SP))
      SIGR=GR1*GR2*EXP((ALREG-ONE)*LOG(SR))
C  save value to calculate soft pt distribution
      SIGS = (SIGR+SIGP)/(SCALE1*SCALE2)
C
C  higher order graphs
      VIRT1 = PVIRTP(1)
      VIRT2 = PVIRTP(2)
C  bare/renormalized intercept for enhanced graphs
      IF(IPAMDL(8).EQ.0) THEN
        DELTAP = ALPOM-ONE
      ELSE
        DELTAP = PARMDL(48)-ONE
      ENDIF
      SD = ECMP**2
      BP1 = 2.D0*BPOM1
      BP2 = 2.D0*BPOM2
C  input cross section high mass double diffraction
      CALL LOOREG(SD,GP1,BP1,GP2,BP2,
     &            DELTAP,ALPOMP,GPPP,B0PPP,VIRT1,VIRT2,SIGTR,BTR)
      SIGL = DCMPLX(SIGTR,ZERO)
      BLOO = DCMPLX(BTR,ZERO)
C
C  input cross section high mass diffraction particle 1
C  first possibility
      CALL SCALES(XM1,XM2,XM3,PMASSP(2),SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(XM1,PMASSP(2),XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      SCALB1 = (SCB1(1)+SCB1(2))/TWO
      SCALB2 = (SCB2(1)+SCB2(2))/TWO
      BP1 = 2.D0*BPOM1*SCALB1
      BP2 = 2.D0*BPOM2*SCALB2
C  input cross section high mass diffraction
      CALL TRIREG(SD,GP1,BP1,GP2,BP2,
     &            DELTAP,ALPOMP,GPPP,B0PPP,VIRT1,SIGTR,BTR)
      SIGT1(1) = SCG1(1)*SCG2(1)*SCG2(2)*DCMPLX(SIGTR,ZERO)
      BTR1(1)  = DCMPLX(BTR,ZERO)
C  second possibility:  high-low mass double diffraction
      CALL SCALES(XM1,XM2,XM3,RMASS2,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(XM1,RMASS2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      SCALB1 = (SCB1(1)+SCB1(2))/TWO
      SCALB2 = (SCB2(1)+SCB2(2))/TWO
      BP1 = 2.D0*BPOM1*SCALB1
      BP2 = 2.D0*BPOM2*SCALB2
C  input cross section high mass diffraction
      CALL TRIREG(SD,GP1,BP1,GP2,BP2,
     &            DELTAP,ALPOMP,GPPP,B0PPP,VIRT1,SIGTR,BTR)
      SIGT1(2) = SCG1(1)*SCG2(1)*SCG2(2)*DCMPLX(SIGTR,ZERO)
      BTR1(2)  = DCMPLX(BTR,ZERO)
C
C  input cross section high mass diffraction particle 2
C  first possibility
      CALL SCALES(XM1,XM2,PMASSP(1),XM4,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(PMASSP(1),XM2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      SCALB1 = (SCB1(1)+SCB1(2))/TWO
      SCALB2 = (SCB2(1)+SCB2(2))/TWO
      BP1 = 2.D0*BPOM1*SCALB1
      BP2 = 2.D0*BPOM2*SCALB2
C  input cross section high mass diffraction
      CALL TRIREG(SD,GP2,BP2,GP1,BP1,
     &            DELTAP,ALPOMP,GPPP,B0PPP,VIRT2,SIGTR,BTR)
      SIGT2(1) = SCG1(1)*SCG1(2)*SCG2(1)*DCMPLX(SIGTR,ZERO)
      BTR2(1)  = DCMPLX(BTR,ZERO)
C  second possibility:  high-low mass double diffraction
      CALL SCALES(XM1,XM2,RMASS1,XM4,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(RMASS1,XM2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      SCALB1 = (SCB1(1)+SCB1(2))/TWO
      SCALB2 = (SCB2(1)+SCB2(2))/TWO
      BP1 = 2.D0*BPOM1*SCALB1
      BP2 = 2.D0*BPOM2*SCALB2
C  input cross section high mass diffraction
      CALL TRIREG(SD,GP2,BP2,GP1,BP1,
     &            DELTAP,ALPOMP,GPPP,B0PPP,VIRT2,SIGTR,BTR)
      SIGT2(2) = SCG1(1)*SCG1(2)*SCG2(1)*DCMPLX(SIGTR,ZERO)
      BTR2(2)  = DCMPLX(BTR,ZERO)
C
C  input cross section for X-iterated triple-Pomeron
C  first possibility
      CALL SCALES(XM1,XM2,PMASSP(1),XM4,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(PMASSP(1),XM2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      CALL SCALES(XM1,XM2,XM3,PMASSP(2),SCG1(3),SCG2(3),SCB1(3),SCB2(3))
      CALL SCALES(XM1,PMASSP(2),XM3,XM4,SCG1(4),SCG2(4),SCB1(4),SCB2(4))
      SCALB1 = (SCB1(1)+SCB1(2)+SCB1(3)+SCB1(4))/FOUR
      SCALB2 = (SCB2(1)+SCB2(2)+SCB2(3)+SCB2(4))/FOUR
      BP1 = BPOM1*SCALB1
      BP2 = BPOM2*SCALB2
      CALL TRXPOM(SD,GP2,BP2,GP1,BP1,DELTAP,ALPOMP,GPPP,B0PPP,SIGTX,BTX)
      SIGDP(1) = SCG1(1)*SCG1(2)*SCG2(3)*SCG2(4)*DCMPLX(SIGTX,ZERO)
      BDP(1)   = DCMPLX(BTX,ZERO)
C  second possibility
      CALL SCALES(XM1,XM2,RMASS1,XM4,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(RMASS1,XM2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      CALL SCALES(XM1,XM2,XM3,PMASSP(2),SCG1(3),SCG2(3),SCB1(3),SCB2(3))
      CALL SCALES(XM1,PMASSP(2),XM3,XM4,SCG1(4),SCG2(4),SCB1(4),SCB2(4))
      SCALB1 = (SCB1(1)+SCB1(2)+SCB1(3)+SCB1(4))/FOUR
      SCALB2 = (SCB2(1)+SCB2(2)+SCB2(3)+SCB2(4))/FOUR
      BP1 = BPOM1*SCALB1
      BP2 = BPOM2*SCALB2
      CALL TRXPOM(SD,GP2,BP2,GP1,BP1,DELTAP,ALPOMP,GPPP,B0PPP,SIGTX,BTX)
      SIGDP(2) = SCG1(1)*SCG1(2)*SCG2(3)*SCG2(4)*DCMPLX(SIGTX,ZERO)
      BDP(2)   = DCMPLX(BTX,ZERO)
C  third possibility
      CALL SCALES(XM1,XM2,PMASSP(1),XM4,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(PMASSP(1),XM2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      CALL SCALES(XM1,XM2,XM3,RMASS2,SCG1(3),SCG2(3),SCB1(3),SCB2(3))
      CALL SCALES(XM1,RMASS2,XM3,XM4,SCG1(4),SCG2(4),SCB1(4),SCB2(4))
      SCALB1 = (SCB1(1)+SCB1(2)+SCB1(3)+SCB1(4))/FOUR
      SCALB2 = (SCB2(1)+SCB2(2)+SCB2(3)+SCB2(4))/FOUR
      BP1 = BPOM1*SCALB1
      BP2 = BPOM2*SCALB2
      CALL TRXPOM(SD,GP2,BP2,GP1,BP1,DELTAP,ALPOMP,GPPP,B0PPP,SIGTX,BTX)
      SIGDP(3) = SCG1(1)*SCG1(2)*SCG2(3)*SCG2(4)*DCMPLX(SIGTX,ZERO)
      BDP(3)   = DCMPLX(BTX,ZERO)
C  fourth possibility
      CALL SCALES(XM1,XM2,RMASS1,XM4,SCG1(1),SCG2(1),SCB1(1),SCB2(1))
      CALL SCALES(RMASS1,XM2,XM3,XM4,SCG1(2),SCG2(2),SCB1(2),SCB2(2))
      CALL SCALES(XM1,XM2,XM3,RMASS2,SCG1(3),SCG2(3),SCB1(3),SCB2(3))
      CALL SCALES(XM1,RMASS2,XM3,XM4,SCG1(4),SCG2(4),SCB1(4),SCB2(4))
      SCALB1 = (SCB1(1)+SCB1(2)+SCB1(3)+SCB1(4))/FOUR
      SCALB2 = (SCB2(1)+SCB2(2)+SCB2(3)+SCB2(4))/FOUR
      BP1 = BPOM1*SCALB1
      BP2 = BPOM2*SCALB2
      CALL TRXPOM(SD,GP2,BP2,GP1,BP1,DELTAP,ALPOMP,GPPP,B0PPP,SIGTX,BTX)
      SIGDP(4) = SCG1(1)*SCG1(2)*SCG2(3)*SCG2(4)*DCMPLX(SIGTX,ZERO)
      BDP(4)   = DCMPLX(BTX,ZERO)
C
C  input cross section for YY-iterated triple-Pomeron
C     .....
C
C  write out input cross sections
      IF(IDEB(48).GE.5) THEN
        WRITE(6,'(2(/1X,A))') 'INPUT CROSS SECTIONS AND SLOPES',
     &                        '==============================='
        WRITE(6,'(1X,A,3E12.3)') 'ENERGY                  ',ECMP,PVIRTP
        WRITE(6,'(1X,A,4E12.3)') 'EXTERNAL MASSES 1,2,3,4 ',
     &       XM1,XM2,XM3,XM4
        WRITE(6,'(A)') ' INPUT CROSS SECTIONS (millibarn):'
        WRITE(6,'(A,2E12.3)') '           SIGR     ',SIGR
        WRITE(6,'(A,2E12.3)') ' (soft)    SIGP     ',SIGP
        WRITE(6,'(A,2E12.3)') ' (hard)    SIGHR    ',SIGHR
        WRITE(6,'(A,2E12.3)') '           SIGHD    ',SIGHD
        WRITE(6,'(A,4E12.3)') '           SIGT1    ',SIGT1
        WRITE(6,'(A,4E12.3)') '           SIGT2    ',SIGT2
        WRITE(6,'(A,2E12.3)') '           SIGL     ',SIGL
        WRITE(6,'(A,4E12.3)') '         SIGDP(1-2) ',SIGDP(1),SIGDP(2)
        WRITE(6,'(A,4E12.3)') '         SIGDP(3-4) ',SIGDP(3),SIGDP(4)
        WRITE(6,'(A)') ' INPUT SLOPES (GeV**-2)'
        WRITE(6,'(A,2E12.3)') '           BREG     ',BREG
        WRITE(6,'(A,2E12.3)') '            BREG1   ',BREG1
        WRITE(6,'(A,2E12.3)') '            BREG2   ',BREG2
        WRITE(6,'(A,2E12.3)') '           BPOM     ',BPOM
        WRITE(6,'(A,2E12.3)') '            BPOM1   ',BPOM1
        WRITE(6,'(A,2E12.3)') '            BPOM2   ',BPOM2
        WRITE(6,'(A,2E12.3)') '           BHAR     ',BHAR
        WRITE(6,'(A,2E12.3)') '           BHAD     ',BHAD
        WRITE(6,'(A,E12.3)')  '           B0PPP    ',B0PPP
        WRITE(6,'(A,4E12.3)') '           BTR1     ',BTR1
        WRITE(6,'(A,4E12.3)') '           BTR2     ',BTR2
        WRITE(6,'(A,2E12.3)') '           BLOO     ',BLOO
        WRITE(6,'(A,4E12.3)') '           BDP(1-2) ',BDP(1),BDP(2)
        WRITE(6,'(A,4E12.3)') '           BDP(3-4) ',BDP(3),BDP(4)
      ENDIF
C
      BPOM  = BPOM*GEV2MB
      BREG  = BREG*GEV2MB
      BHAR  = BHAR*GEV2MB
      BHAD  = BHAD*GEV2MB
      BTR1(1)  = BTR1(1)*GEV2MB
      BTR1(2)  = BTR1(2)*GEV2MB
      BTR2(1)  = BTR2(1)*GEV2MB
      BTR2(2)  = BTR2(2)*GEV2MB
      BLOO  = BLOO*GEV2MB
C
      BP4 =BPOM*FOUR
      BR4 =BREG*FOUR
      BHR4=BHAR*FOUR
      BHD4=BHAD*FOUR
      BT14(1)=BTR1(1)*FOUR
      BT14(2)=BTR1(2)*FOUR
      BT24(1)=BTR2(1)*FOUR
      BT24(2)=BTR2(2)*FOUR
      BL4 =BLOO*FOUR
C
      ZIGP     = SIGP/(PI2*BP4)
      ZIGR     = SIGR/(PI2*BR4)
      ZIGHR    = SIGHR/(PI2*BHR4)*AKFAC
      ZIGHD    = SIGHD/(PI2*BHD4)*AKFAC
      ZIGT1(1) = SIGT1(1)/(PI2*BT14(1))
      ZIGT1(2) = SIGT1(2)/(PI2*BT14(2))
      ZIGT2(1) = SIGT2(1)/(PI2*BT24(1))
      ZIGT2(2) = SIGT2(2)/(PI2*BT24(2))
      ZIGL = SIGL/(PI2*BL4)
      DO 20 I=1,4
        BDP(I) = BDP(I)*GEV2MB
        BD4(I) = BDP(I)*FOUR
        ZIGDP(I) = SIGDP(I)/(PI2*BD4(I))
 20   CONTINUE
C
      IF(IDEB(48).GE.10) THEN
        WRITE(6,'(A)') ' NORMALIZED INPUT VALUES:'
        WRITE(6,'(A,2E12.3)') '           ZIGR ',ZIGR
        WRITE(6,'(A,2E12.3)') '           BREG ',BREG
        WRITE(6,'(A,2E12.3)') '           ZIGP ',ZIGP
        WRITE(6,'(A,2E12.3)') '           BPOM ',BPOM
        WRITE(6,'(A,2E12.3)') '          ZIGHR ',ZIGHR
        WRITE(6,'(A,2E12.3)') '           BHAR ',BHAR
        WRITE(6,'(A,2E12.3)') '          ZIGHD ',ZIGHD
        WRITE(6,'(A,2E12.3)') '           BHAD ',BHAD
        WRITE(6,'(A,4E12.3)') '          ZIGT1 ',ZIGT1
        WRITE(6,'(A,4E12.3)') '          ZIGT2 ',ZIGT2
        WRITE(6,'(A,2E12.3)') '           ZIGL ',ZIGL
        WRITE(6,'(A,4E12.3)') '     ZIGDP(1-2) ',ZIGDP(1),ZIGDP(2)
        WRITE(6,'(A,4E12.3)') '     ZIGDP(3-4) ',ZIGDP(3),ZIGDP(4)
      ENDIF
      END
C
C
      SUBROUTINE SCALES(XM1,XM2,XM3,XM4,SCG1,SCG2,SCB1,SCB2)
C**********************************************************************
C
C     calculation of scale factors
C              (mass dependent couplings and slopes)
C
C     input:   XM1..XM4     external masses
C
C     output:  SCG1,SCG2    scales of coupling constants
C              SCB1,SCB2    scales of coupling slope parameter
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0,
     &           ONE  = 1.D0,
     &           EPS  = 1.D-3)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
C  scale factors for couplings
      ECMMIN = 2.D0
*     ECMTP = 6.D0
      ECMTP = 1.D0
      IF(ABS(XM1-XM3).GT.EPS) THEN
        IF(ECMP.LT.ECMTP) THEN
          SCG1 = PHISUP(1)*LOG(ECMP**2/ECMMIN)/LOG(ECMTP**2/ECMMIN)
        ELSE
          SCG1 = PHISUP(1)
        ENDIF
      ELSE
        SCG1 = ONE
      ENDIF
      IF(ABS(XM2-XM4).GT.EPS) THEN
        IF(ECMP.LT.ECMTP) THEN
          SCG2 = PHISUP(2)*LOG(ECMP**2/ECMMIN)/LOG(ECMTP**2/ECMMIN)
        ELSE
          SCG2 = PHISUP(2)
        ENDIF
      ELSE
        SCG2 = ONE
      ENDIF
C
C  scale factors for slope parameters
      IF((ISWMDL(1).LT.2).OR.(IPAMDL(10).EQ.1)) THEN
        SCB1 = ONE
        SCB2 = ONE
      ELSE IF(ISWMDL(1).EQ.2) THEN
C  rational
        SCB1 = 2.D0*PMASSP(1)**2/(XM1**2+XM3**2)
        SCB2 = 2.D0*PMASSP(2)**2/(XM2**2+XM4**2)
      ELSE IF(ISWMDL(1).GE.3) THEN
C  symmetric gaussian
        SCB1 = VAR*(XM1-XM3)**2
        IF(SCB1.LT.25.D0) THEN
          SCB1 = EXP(-SCB1)
        ELSE
          SCB1 = ZERO
        ENDIF
        SCB2 = VAR*(XM2-XM4)**2
        IF(SCB2.LT.25.D0) THEN
          SCB2 = EXP(-SCB2)
        ELSE
          SCB2 = ZERO
        ENDIF
      ELSE
        WRITE(6,'(/,1X,A,I4)') 'SCALES:ERROR:INVALID ISWMDL(1)',
     &    ISWMDL(1)
        CALL POABRT
      ENDIF
C  debug output
      IF(IDEB(65).GE.10) THEN
        WRITE(6,'(1X,A,4E11.3)') 'SCALES:DEBUG: M1..M4 ',
     &       XM1,XM2,XM3,XM4
        WRITE(6,'(5X,A,4E11.3)') 'SCB1,SCB2,SCG1,SCG2',
     &       SCB1,SCB2,SCG1,SCG2
      ENDIF
      END
C
C
      SUBROUTINE EIKON(IP,B)
C*********************************************************************
C
C     calculation of unitarized amplitudes
C
C     input: IP               particle combination
C            B                impact parameter (mb**(1/2))
C            COMMON /SIGMAS/  input cross sections
C                   /GLOCMS/  cm energy
C                   /REGGE/   soft and hard parameters
C
C     output: COMMON /AMPLIT/
C             AMPEL           purely elastic amplitude
C             AMPVM           quasi-elastically vectormeson prod.
C             AMLMSD(2)       amplitudes of low mass sing. diffr.
C             AMHMSD(2)       amplitudes of high mass sing. diffr.
C             AMLMDD          amplitude of low mass double diffr.
C             AMHMDD          amplitude of high mass double diffr.
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  output common block
      COMPLEX*16      AMPEL,AMPVM,AMPSOF,AMPHAR,AMLMSD,AMHMSD,AMLMDD,
     &                AMHMDD,AMPDP,AMPD1,AMPD2
      COMMON /AMPLIT/ AMPEL,AMPVM(4,4),AMPSOF,AMPHAR,AMLMSD(2),
     &                AMHMSD(2),AMLMDD,AMHMDD,AMPDP,AMPD1,AMPD2
C
      PARAMETER ( MAXPRO = 16 )
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
      COMPLEX*16      SIGP,SIGR,SIGHD,SIGHR,SIGT1,SIGT2,SIGL,SIGDP,
     &                SIGD1,SIGD2,DSIGH
      COMMON /SIGMAS/ SIGP,SIGR,SIGHD,SIGHR,SIGT1(2),SIGT2(2),SIGL,
     &                SIGDP(4),SIGD1(2),SIGD2(2),DSIGH(0:MAXPRO)
      COMPLEX*16      ZIGP,ZIGR,ZIGHD,ZIGHR,ZIGT1,ZIGT2,ZIGL,ZIGDP,
     &                ZIGD1,ZIGD2,
     &                BPOM,BREG,BHAR,BHAD,BTR1,BTR2,BLOO,BDP,BD1,BD2
      COMMON /ZIGMAS/ ZIGP,ZIGR,ZIGHD,ZIGHR,ZIGT1(2),ZIGT2(2),ZIGL,
     &                ZIGDP(4),ZIGD1(2),ZIGD2(2),
     &                BPOM,BREG,BHAR,BHAD,BTR1(2),BTR2(2),BLOO,BDP(4),
     &                BD1(2),BD2(2)
C  global CM system
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /FITMOD/ IFMOD,IFHARD
C
      COMPLEX*16      CZERO,CONE,B24,AUXP,AUXR,AUXH,AUXD,AUXT1,AUXT2,
     &                AUXL,AMPR,AMPO,AMPP,AMPQ
C
      PARAMETER(ZERO=0.D0,
     &         IZERO=0,
     &          IONE=1,
     &          ITWO=2,
     &        ITHREE=3,
     &         IFOUR=4,
     &         IFIVE=5,
     &          ISIX=6,
     &         OHALF=0.5D0,
     &           ONE=1.D0,
     &           TWO=2.D0,
     &         THREE=3.D0,
     &          FOUR=4.D0,
     &          FIVE=5.D0,
     &         THOUS=1.D3,
     &        EXPMAX=70.D0,
     &          DEPS=1.D-20)
C
      PARAMETER(MDIM=5)
      COMMON /ZXEIKS/ XM(MDIM,MDIM),XOUT(MDIM,MDIM),
     &                XIN(MDIM),XOU(MDIM),
     &                ZXP(MDIM,MDIM),BXP(MDIM,MDIM),
     &                ZXR(MDIM,MDIM),BXR(MDIM,MDIM),
     &                ZXH(MDIM,MDIM),BXH(MDIM,MDIM),
     &                ZXD(MDIM,MDIM),BXD(MDIM,MDIM),
     &                ZXT1A(MDIM,MDIM),BXT1A(MDIM,MDIM),
     &                ZXT1B(MDIM,MDIM),BXT1B(MDIM,MDIM),
     &                ZXT2A(MDIM,MDIM),BXT2A(MDIM,MDIM),
     &                ZXT2B(MDIM,MDIM),BXT2B(MDIM,MDIM),
     &                ZXL(MDIM,MDIM),BXL(MDIM,MDIM),
     &                ZXDP(MDIM,MDIM),BXDP(MDIM,MDIM),
     &                ZXD1A(MDIM,MDIM),BXD1A(MDIM,MDIM),
     &                ZXD1B(MDIM,MDIM),BXD1B(MDIM,MDIM),
     &                ZXD2A(MDIM,MDIM),BXD2A(MDIM,MDIM),
     &                ZXD2B(MDIM,MDIM),BXD2B(MDIM,MDIM)
C
      DIMENSION PVOLD(2)
      DATA  ELAST / 0.D0 /
      DATA  IPOLD / -1 /
      DATA  PVOLD / -1.D0, -1.D0 /
      DATA  XMPOM / 0.766D0 /
      DATA  XMVDM / 0.766D0 /
C
      DCMPLX(X,Y) = CMPLX(X,Y)
C
C  calculation of ZIGmas and slopes
C
C  test for redundant calculation
      IF((ECM.NE.ELAST).OR.(PVIRT(1).NE.PVOLD(1))
     &   .OR.(PVIRT(2).NE.PVOLD(2)).OR.(IP.NE.IPOLD)) THEN
C  effective particle masses, VDM assumption
        XMASS1 = PMASS(1)
        XMASS2 = PMASS(2)
        IF(IFPAP(1).EQ.22) THEN
          XMASS1 = XMVDM
        ELSE IF(IFPAP(1).EQ.45) THEN
          XMASS1 = XMPOM
        ENDIF
        IF(IFPAP(2).EQ.22) THEN
          XMASS2 = XMVDM
        ELSE IF(IFPAP(2).EQ.45) THEN
          XMASS2 = XMPOM
        ENDIF
        IF(IP.EQ.2) THEN
          XMASS2 = XMPOM 
        ELSE IF(IP.EQ.3) THEN
          XMASS2 = XMPOM
        ELSE
          XMASS1 = XMPOM
          XMASS2 = XMPOM
        ENDIF
        PMASSP(1) = XMASS1
        PMASSP(2) = XMASS2
        ECMP = ECM
C
        CZERO    = DCMPLX(ZERO,ZERO)
        CONE     = DCMPLX(ONE,ZERO)
        ELAST    = ECM
        PVOLD(1) = PVIRT(1)
        PVOLD(2) = PVIRT(2)
        IPOLD    = IP
C
C  pure elastic scattering
        CALL INPCS(IP,XMASS1,XMASS2,XMASS1,XMASS2)
          ZXP(1,1) = ZIGP
          BXP(1,1) = BPOM
          ZXR(1,1) = ZIGR
          BXR(1,1) = BREG
          ZXH(1,1) = ZIGHR
          BXH(1,1) = BHAR
          ZXD(1,1) = ZIGHD
          BXD(1,1) = BHAD
          ZXT1A(1,1) = ZIGT1(1)
          BXT1A(1,1) = BTR1(1)
          ZXT1B(1,1) = ZIGT1(2)
          BXT1B(1,1) = BTR1(2)
          ZXT2A(1,1) = ZIGT2(1)
          BXT2A(1,1) = BTR2(1)
          ZXT2B(1,1) = ZIGT2(2)
          BXT2B(1,1) = BTR2(2)
          ZXL(1,1) = ZIGL
          BXL(1,1) = BLOO
          ZXDP(1,1) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
          BXDP(1,1) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C  Born-graph cross sections
          SIGPOM = SIGP
          SIGREG = SIGR
          SIGTR1 = SIGT1(1)+SIGT1(2)
          SIGTR2 = SIGT2(1)+SIGT2(2)
          SIGLOO = SIGL
          SIGDPO = SIGDP(1)+SIGDP(2)+SIGDP(3)+SIGDP(4)
C
C  avoid further hard cross section calculation
        ITMP = IFHARD
        IFHARD = 1
C
        IF(ISWMDL(1).EQ.2) THEN
C
C  pure elastic scattering of low mass resonance 1
          CALL INPCS(IP,RMASS1,XMASS2,RMASS1,XMASS2)
            ZXP(2,2) = ZIGP
            BXP(2,2) = BPOM
            ZXR(2,2) = ZIGR
            BXR(2,2) = BREG
            ZXH(2,2) = ZIGHR
            BXH(2,2) = BHAR
            ZXD(2,2) = ZIGHD
            BXD(2,2) = BHAD
            ZXT1A(2,2) = ZIGT1(1)
            BXT1A(2,2) = BTR1(1)
            ZXT1B(2,2) = ZIGT1(2)
            BXT1B(2,2) = BTR1(2)
            ZXT2A(2,2) = ZIGT2(1)
            BXT2A(2,2) = BTR2(1)
            ZXT2B(2,2) = ZIGT2(2)
            BXT2B(2,2) = BTR2(2)
            ZXL(2,2) = ZIGL
            BXL(2,2) = BLOO
            ZXDP(2,2) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
            BXDP(2,2) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
C  pure elastic scattering of low mass resonance 2
          CALL INPCS(IP,XMASS1,RMASS2,XMASS1,RMASS2)
            ZXP(3,3) = ZIGP
            BXP(3,3) = BPOM
            ZXR(3,3) = ZIGR
            BXR(3,3) = BREG
            ZXH(3,3) = ZIGHR
            BXH(3,3) = BHAR
            ZXD(3,3) = ZIGHD
            BXD(3,3) = BHAD
            ZXT1A(3,3) = ZIGT1(1)
            BXT1A(3,3) = BTR1(1)
            ZXT1B(3,3) = ZIGT1(2)
            BXT1B(3,3) = BTR1(2)
            ZXT2A(3,3) = ZIGT2(1)
            BXT2A(3,3) = BTR2(1)
            ZXT2B(3,3) = ZIGT2(2)
            BXT2B(3,3) = BTR2(2)
            ZXL(3,3) = ZIGL
            BXL(3,3) = BLOO
            ZXDP(3,3) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
            BXDP(3,3) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
C  pure elastic scattering of both of the low mass resonances
          CALL INPCS(IP,RMASS1,RMASS2,RMASS1,RMASS2)
            ZXP(4,4) = ZIGP
            BXP(4,4) = BPOM
            ZXR(4,4) = ZIGR
            BXR(4,4) = BREG
            ZXH(4,4) = ZIGHR
            BXH(4,4) = BHAR
            ZXD(4,4) = ZIGHD
            BXD(4,4) = BHAD
            ZXT1A(4,4) = ZIGT1(1)
            BXT1A(4,4) = BTR1(1)
            ZXT1B(4,4) = ZIGT1(2)
            BXT1B(4,4) = BTR1(2)
            ZXT2A(4,4) = ZIGT2(1)
            BXT2A(4,4) = BTR2(1)
            ZXT2B(4,4) = ZIGT2(2)
            BXT2B(4,4) = BTR2(2)
            ZXL(4,4) = ZIGL
            BXL(4,4) = BLOO
            ZXDP(4,4) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
            BXDP(4,4) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
        ENDIF
C
C  low mass single diffractive scattering 1
        CALL INPCS(IP,XMASS1,XMASS2,RMASS1,XMASS2)
          ZXP(1,2) = ZIGP
          BXP(1,2) = BPOM
          ZXR(1,2) = ZIGR
          BXR(1,2) = BREG
          ZXH(1,2) = ZIGHR
          BXH(1,2) = BHAR
          ZXD(1,2) = ZIGHD
          BXD(1,2) = BHAD
          ZXT1A(1,2) = ZIGT1(1)
          BXT1A(1,2) = BTR1(1)
          ZXT1B(1,2) = ZIGT1(2)
          BXT1B(1,2) = BTR1(2)
          ZXT2A(1,2) = ZIGT2(1)
          BXT2A(1,2) = BTR2(1)
          ZXT2B(1,2) = ZIGT2(2)
          BXT2B(1,2) = BTR2(2)
          ZXL(1,2) = ZIGL
          BXL(1,2) = BLOO
          ZXDP(1,2) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
          BXDP(1,2) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
C  low mass single diffractive scattering 2
        CALL INPCS(IP,XMASS1,XMASS2,XMASS1,RMASS2)
          ZXP(1,3) = ZIGP
          BXP(1,3) = BPOM
          ZXR(1,3) = ZIGR
          BXR(1,3) = BREG
          ZXH(1,3) = ZIGHR
          BXH(1,3) = BHAR
          ZXD(1,3) = ZIGHD
          BXD(1,3) = BHAD
          ZXT1A(1,3) = ZIGT1(1)
          BXT1A(1,3) = BTR1(1)
          ZXT1B(1,3) = ZIGT1(2)
          BXT1B(1,3) = BTR1(2)
          ZXT2A(1,3) = ZIGT2(1)
          BXT2A(1,3) = BTR2(1)
          ZXT2B(1,3) = ZIGT2(2)
          BXT2B(1,3) = BTR2(2)
          ZXL(1,3) = ZIGL
          BXL(1,3) = BLOO
          ZXDP(1,3) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
          BXDP(1,3) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
C  low mass double diffractive scattering
        CALL INPCS(IP,XMASS1,XMASS2,RMASS1,RMASS2)
          ZXP(1,4) = ZIGP
          BXP(1,4) = BPOM
          ZXR(1,4) = ZIGR
          BXR(1,4) = BREG
          ZXH(1,4) = ZIGHR
          BXH(1,4) = BHAR
          ZXD(1,4) = ZIGHD
          BXD(1,4) = BHAD
          ZXT1A(1,4) = ZIGT1(1)
          BXT1A(1,4) = BTR1(1)
          ZXT1B(1,4) = ZIGT1(2)
          BXT1B(1,4) = BTR1(2)
          ZXT2A(1,4) = ZIGT2(1)
          BXT2A(1,4) = BTR2(1)
          ZXT2B(1,4) = ZIGT2(2)
          BXT2B(1,4) = BTR2(2)
          ZXL(1,4) = ZIGL
          BXL(1,4) = BLOO
          ZXDP(1,4) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
          BXDP(1,4) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
       IF(ISWMDL(1).EQ.2) THEN
C
C  low mass mixed single diffractive scattering
          CALL INPCS(IP,RMASS1,XMASS2,XMASS1,RMASS2)
            ZXP(2,3) = ZIGP
            BXP(2,3) = BPOM
            ZXR(2,3) = ZIGR
            BXR(2,3) = BREG
            ZXH(2,3) = ZIGHR
            BXH(2,3) = BHAR
            ZXD(2,3) = ZIGHD
            BXD(2,3) = BHAD
            ZXT1A(2,3) = ZIGT1(1)
            BXT1A(2,3) = BTR1(1)
            ZXT1B(2,3) = ZIGT1(2)
            BXT1B(2,3) = BTR1(2)
            ZXT2A(2,3) = ZIGT2(1)
            BXT2A(2,3) = BTR2(1)
            ZXT2B(2,3) = ZIGT2(2)
            BXT2B(2,3) = BTR2(2)
            ZXL(2,3) = ZIGL
            BXL(2,3) = BLOO
            ZXDP(2,3) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
            BXDP(2,3) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
C  low mass single to double diffractive scattering 1
          CALL INPCS(IP,RMASS1,XMASS2,RMASS1,RMASS2)
            ZXP(2,4) = ZIGP
            BXP(2,4) = BPOM
            ZXR(2,4) = ZIGR
            BXR(2,4) = BREG
            ZXH(2,4) = ZIGHR
            BXH(2,4) = BHAR
            ZXD(2,4) = ZIGHD
            BXD(2,4) = BHAD
            ZXT1A(2,4) = ZIGT1(1)
            BXT1A(2,4) = BTR1(1)
            ZXT1B(2,4) = ZIGT1(2)
            BXT1B(2,4) = BTR1(2)
            ZXT2A(2,4) = ZIGT2(1)
            BXT2A(2,4) = BTR2(1)
            ZXT2B(2,4) = ZIGT2(2)
            BXT2B(2,4) = BTR2(2)
            ZXL(2,4) = ZIGL
            BXL(2,4) = BLOO
            ZXDP(2,4) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
            BXDP(2,4) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
C
C  low mass single to double diffractive scattering 2
          CALL INPCS(IP,XMASS1,RMASS2,RMASS1,RMASS2)
            ZXP(3,4) = ZIGP
            BXP(3,4) = BPOM
            ZXR(3,4) = ZIGR
            BXR(3,4) = BREG
            ZXH(3,4) = ZIGHR
            BXH(3,4) = BHAR
            ZXD(3,4) = ZIGHD
            BXD(3,4) = BHAD
            ZXT1A(3,4) = ZIGT1(1)
            BXT1A(3,4) = BTR1(1)
            ZXT1B(3,4) = ZIGT1(2)
            BXT1B(3,4) = BTR1(2)
            ZXT2A(3,4) = ZIGT2(1)
            BXT2A(3,4) = BTR2(1)
            ZXT2B(3,4) = ZIGT2(2)
            BXT2B(3,4) = BTR2(2)
            ZXL(3,4) = ZIGL
            BXL(3,4) = BLOO
            ZXDP(3,4) = ZIGDP(1)+ZIGDP(2)+ZIGDP(3)+ZIGDP(4)
            BXDP(3,4) = (BDP(1)+BDP(2)+BDP(3)+BDP(4))/FOUR
        ENDIF
C
C  restore IFHARD value
        IFHARD = ITMP
      ENDIF
C
      B24=DCMPLX(B**2,ZERO)/FOUR
C
      AMPEL      = CZERO
      AMPR      = CZERO
      AMPO      = CZERO
      AMPP      = CZERO
      AMPQ      = CZERO
      AMLMSD(1) = CZERO
      AMLMSD(2) = CZERO
      AMHMSD(1) = CZERO
      AMHMSD(2) = CZERO
      AMLMDD    = CZERO
      AMHMDD    = CZERO
C
C  different models
C
      IF(ISWMDL(1).LT.3) THEN
C  pomeron
        AUXP  = ZXP(1,1)*EXP(-B24/BXP(1,1))
C  reggeon
        AUXR  = ZXR(1,1)*EXP(-B24/BXR(1,1))
C  hard resolved processes
        AUXH  = ZXH(1,1)*EXP(-B24/BXH(1,1))
C  hard direct processes
        AUXD  = ZXD(1,1)*EXP(-B24/BXD(1,1))
C  triple-Pomeron: baryon high mass diffraction
        AUXT1 = ZXT1A(1,1)*EXP(-B24/BXT1A(1,1))
     &        + ZXT1B(1,1)*EXP(-B24/BXT1B(1,1))
C  triple-Pomeron: photon/meson high mass diffraction
        AUXT2 = ZXT2A(1,1)*EXP(-B24/BXT2A(1,1))
     &        + ZXT2B(1,1)*EXP(-B24/BXT2B(1,1))
C  loop-Pomeron
        AUXL  = ZXL(1,1)*EXP(-B24/BXL(1,1))
      ENDIF
C
C
      IF(ISWMDL(1).EQ.IZERO) THEN
        AMPEL = OHALF*((VDMQ2F(1)+VDMQ2F(2)+VDMQ2F(3)+VDMQ2F(4))
     &                 *(CONE-EXP(-AUXR-AUXP-AUXH+AUXT1+AUXT2+AUXL))
     &        +(CONE-(VDMQ2F(1)-VDMQ2F(2)-VDMQ2F(3)-VDMQ2F(4)))*AUXD
     &               )
        AMPR = OHALF*SQRT(VDMQ2F(1))*(CONE-EXP(-AUXR-AUXP-AUXH
     &                                      +AUXT1+AUXT2+AUXL))
        AMPO = OHALF*SQRT(VDMQ2F(2))*(CONE-EXP(-AUXR-AUXP-AUXH
     &                                      +AUXT1+AUXT2+AUXL))
        AMPP = OHALF*SQRT(VDMQ2F(3))*(CONE-EXP(-AUXR-AUXP-AUXH
     &                                      +AUXT1+AUXT2+AUXL))
        AMPQ = OHALF*SQRT(VDMQ2F(4))*(CONE-EXP(-AUXR-AUXP-AUXH
     &                                      +AUXT1+AUXT2+AUXL))
C
C
      ELSE IF(ISWMDL(1).EQ.1) THEN
        AMPR = OHALF*SQRT(VDMQ2F(1))*
     &         ( CONE-EXP(-AUXR-AUXP-AUXH*VDMQ2F(1)) )
        AMPO = OHALF*SQRT(VDMQ2F(2))*
     &         ( CONE-EXP(-3.D0*AUXR-AUXP-AUXH*VDMQ2F(2)) )
        AMPP = OHALF*SQRT(VDMQ2F(3))*
     &         ( CONE-EXP(-AUXP-AUXH*VDMQ2F(3)) )
        AMPQ = OHALF*SQRT(VDMQ2F(4))*
     &         ( CONE-EXP(-AUXR-AUXP-AUXH*VDMQ2F(4)) )
        AMPEL = SQRT(VDMQ2F(1))*AMPR
     &         + SQRT(VDMQ2F(2))*AMPO
     &         + SQRT(VDMQ2F(3))*AMPP
     &         + SQRT(VDMQ2F(4))*AMPQ
     &         + AUXD/2.D0
C
C  simple analytic two channel model (version A)
      ELSE IF(ISWMDL(1).EQ.3) THEN
        CALL CHAN2A(B)
C
      ELSE
        WRITE(6,'(1X,A,I2)')
     &       'EIKON: ERROR: unsupported model ISWMDL(1) ',ISWMDL(1)
        STOP
      ENDIF
C
      END
C
C
      SUBROUTINE DSIGDT(EE,XTA,NFILL)
C*********************************************************************
C
C     calculation of unitarized amplitude
C                    and differential cross section
C
C     input:   EE       cm energy (GeV)
C              XTA(1,*) t values (GeV**2)
C              NFILL    entries in t table
C
C     output:  XTA(2,*)  DSIG/DT  g p --> g p (mub/GeV**2)
C              XTA(3,*)  DSIG/DT  g p --> rho0 p
C              XTA(4,*)  DSIG/DT  g p --> omega0 p
C              XTA(5,*)  DSIG/DT  g p --> phi p
C              XTA(6,*)  DSIG/DT  g p --> pi+ pi- p (continuum)
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION XTA(6,NFILL)
C
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      COMPLEX*16      AMPEL,AMPVM,AMPSOF,AMPHAR,AMLMSD,AMHMSD,AMLMDD,
     &                AMHMDD,AMPDP,AMPD1,AMPD2
      COMMON /AMPLIT/ AMPEL,AMPVM(4,4),AMPSOF,AMPHAR,AMLMSD(2),
     &                AMHMSD(2),AMLMDD,AMHMDD,AMPDP,AMPD1,AMPD2
C
      PARAMETER(ZERO=0.D0,
     &         IZERO=0,
     &          IONE=1,
     &          ITWO=2,
     &        ITHREE=3,
     &         OHALF=0.5D0,
     &           ONE=1.D0,
     &           TWO=2.D0,
     &         THREE=3.D0,
     &          FOUR=4.D0,
     &         THOUS=1.D3,
     &          DEPS=1.D-20)
C
      COMPLEX*16   XT,AMP,CZERO
      DIMENSION    AMP(5),XPNT(96),WGHT(96),XT(5,100)
      CHARACTER*12 FNA
C
      CDABS(AMPEL) = ABS(AMPEL)
      DCMPLX(X,Y) = CMPLX(X,Y)
C
C  re-initialization of EIKON
      ETMP = ECM
      ECM = 2.5555D0
      CALL EIKON(1,ZERO)
      ECM = EE
C
      CZERO=DCMPLX(ZERO,ZERO)
      IF(NFILL.GT.100) THEN
        WRITE(6,'(1X,A,I4)')
     &       ' DSIGDT: ERROR: too many entries in table',NFILL
        STOP
      ENDIF
C
      DO 100 K=1,NFILL
        DO 150 L=1,5
          XT(L,K)=CZERO
 150    CONTINUE
 100  CONTINUE
C
C  impact parameter integration
C     BMAX=12.D0*SQRT(MAX(BPOM,BREG))
      BMAX=10.D0
      CALL PHGSET(ZERO,BMAX,NGAUSO,XPNT,WGHT)
      IAMP = 5
      IF((IFPAP(1).EQ.22).AND.(IFPAP(2).NE.22)) THEN
        I1 = 1
        I2 = 0
      ELSE IF((IFPAP(1).NE.22).AND.(IFPAP(2).EQ.22)) THEN
        I1 = 0
        I2 = 1
      ELSE IF((IFPAP(1).EQ.22).AND.(IFPAP(2).EQ.22)) THEN
        I1 = 1
        I2 = 1
      ELSE
        I1 = 0
        I2 = 0
        IAMP = 1
      ENDIF
      J1 = I1*2
      K1 = I1*3
      L1 = I1*4
      J2 = I2*2
      K2 = I2*3
      L2 = I2*4
C
      DO 200 I=1,NGAUSO
        WG=WGHT(I)*XPNT(I)
C  calculate amplitudes
        CALL EIKON(1,XPNT(I))
        AMP(1) = AMPEL
        AMP(2) = AMPVM(I1,I2)
        AMP(3) = AMPVM(J1,J2)
        AMP(4) = AMPVM(K1,K2)
        AMP(5) = AMPVM(L1,L2)
C
        DO 400 J=1,NFILL
          XX=XPNT(I)*SQRT(XTA(1,J)/GEV2MB)
          FAC = PHBESJ0(XX)*WG
          DO 500 K=1,IAMP
            XT(1,J)=XT(1,J)+AMP(K)*FAC
 500      CONTINUE
 400    CONTINUE
 200  CONTINUE
C
C  change units to mb/GeV**2
      FAC = 4.D0*PI/GEV2MB
      FNA = '(mb/GeV**2) '
      IF(I1+I2.EQ.1) THEN
        FAC = FAC*THOUS
        FNA = '(mub/GeV**2)'
      ELSE IF(I1+I2.EQ.2) THEN
        FAC = FAC*THOUS*THOUS
        FNA = '(nb/GeV**2) '
      ENDIF
      IF(IDEB(56).GE.5) THEN
        WRITE(6,'(1X,A,A12,/1X,A)') 'TABLE:  -T (GeV**2)   DSIG/DT ',
     &    FNA,'=========================================='
      ENDIF
      DO 600 J=1,NFILL
        DO 700 K=1,IAMP
          XTA(K+1,J)=CDABS(XT(K,J))**2*FAC
 700    CONTINUE
        IF(IDEB(56).GE.5) THEN
          WRITE(6,'(5X,6E12.3)') (XTA(I,J),I=1,IAMP+1)
        ENDIF
 600  CONTINUE
C
      ECM = ETMP
      END
C
C
      SUBROUTINE POXSEC(IP,EE)
C*********************************************************************
C
C     calculation of physical  cross sections
C
C     input:   IP      particle combination
C              EE      cms energy (GeV)
C
C     output:  COMMON /SIGMAS/  input cross sections
C              COMMON /ZIGMAS/  scaled input cross values
C              COMMON /XSECTP/  physical cross sections and slopes
C
C              slopes in GeV**-2, cross sections in mb
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16      SIGP,SIGR,SIGHD,SIGHR,SIGT1,SIGT2,SIGL,SIGDP,
     &                SIGD1,SIGD2,DSIGH
      COMMON /SIGMAS/ SIGP,SIGR,SIGHD,SIGHR,SIGT1(2),SIGT2(2),SIGL,
     &                SIGDP(4),SIGD1(2),SIGD2(2),DSIGH(0:MAXPRO)
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER(ZERO=0.D0,
     &         OHALF=0.5D0,
     &           ONE=1.D0,
     &          ONEM=-1.D0,
     &           TWO=2.D0,
     &          FOUR=4.D0,
     &         THOUS=1.D3,
     &          DEPS=1.D-20)
C
      CHARACTER*8     PANAME
      COMPLEX*16      AMPEL,AMPVM,AMPSOF,AMPHAR,AMLMSD,AMHMSD,AMLMDD,
     &                AMHMDD,AMPDP,AMPD1,AMPD2
      COMMON /AMPLIT/ AMPEL,AMPVM(4,4),AMPSOF,AMPHAR,AMLMSD(2),
     &                AMHMSD(2),AMLMDD,AMHMDD,AMPDP,AMPD1,AMPD2
C
      DIMENSION XPNT(96),WGHT(96),SLVM1(4,4),SLVM2(4,4)
      CHARACTER*8 VMESA(0:4),VMESB(0:4)
      DATA VMESA / 'VMESON  ','RHO     ','OMEGA0  ','PHI     ',
     &             'PI+PI-  ' /
      DATA VMESB / 'VMESON  ','RHO     ','OMEGA0  ','PHI     ',
     &             'PI+PI-' /
C
      CDABS(AMPEL) = ABS(AMPEL)
C
      ETMP = ECM
      IF(EE.LT.ZERO) GOTO 500
      ECM = EE
C  impact parameter integration
C     BMAX=12.D0*SQRT(MAX(BPOM,BREG))
      BMAX=10.D0
      CALL PHGSET(ZERO,BMAX,NGAUSO,XPNT,WGHT)
      SIGTOT    = ZERO
      SIGINE    = ZERO
      SIGELA    = ZERO
      SIGLSD(1) = ZERO
      SIGLSD(2) = ZERO
      SIGLDD    = ZERO
      SIGHSD(1) = ZERO
      SIGHSD(2) = ZERO
      SIGHDD    = ZERO
      SIGCDF    = ZERO
      SIG1So    = ZERO
      SIG1HA    = ZERO
      SLEL1 = ZERO
      SLEL2 = ZERO
      DO 50 I=1,4
        DO 55 K=1,4
          SIGVM(I,K) = ZERO
          SLVM1(I,K) = ZERO
          SLVM2(I,K) = ZERO
 55     CONTINUE
 50   CONTINUE
C
      DO 100 I=1,NGAUSO
        B2  = XPNT(I)**2
        WG  = WGHT(I)*XPNT(I)
        WGB = B2*WG
C
C  calculate impact parameter amplitude, results in /AMPLIT/
        CALL EIKON(IP,XPNT(I))
C
        SIGTOT    = SIGTOT + DREAL(AMPEL)*WG
        SIGELA    = SIGELA + CDABS(AMPEL)**2*WG
        SLEL1     = SLEL1  + AMPEL*WGB
        SLEL2     = SLEL2  + AMPEL*WG
C
        DO 110 J=1,4
          DO 120 K=1,4
            SIGVM(J,K) = SIGVM(J,K) + CDABS(AMPVM(J,K))**2*WG
            SLVM1(J,K) = SLVM1(J,K) + AMPVM(J,K)*WGB
            SLVM2(J,K) = SLVM2(J,K) + AMPVM(J,K)*WG
 120      CONTINUE
 110    CONTINUE
C
        SIGLSD(1) = SIGLSD(1) + CDABS(AMLMSD(1))**2*WG
        SIGLSD(2) = SIGLSD(2) + CDABS(AMLMSD(2))**2*WG
        SIGLDD    = SIGLDD    + CDABS(AMLMDD)**2*WG
        SIG1SO    = SIG1SO    + DREAL(AMPSOF)*WG
        SIG1HA    = SIG1HA    + DREAL(AMPHAR)*WG
        SIGHSD(1) = SIGHSD(1) + DREAL(AMHMSD(1))*WG
        SIGHSD(2) = SIGHSD(2) + DREAL(AMHMSD(2))*WG
        SIGHDD    = SIGHDD    + DREAL(AMHMDD)*WG
        SIGCDF    = SIGCDF    + DREAL(AMPDP)*WG
C
 100  CONTINUE
C
      SIGDIR = DREAL(SIGHD)
      FAC    = FOUR*PI2
      SIGTOT = SIGTOT*FAC
      SIGELA = SIGELA*FAC
      FACSL  = OHALF/GEV2MB
      SLOEL  = SLEL1/MAX(DEPS,SLEL2)*FACSL
      DO 130 I=1,4
        DO 140 J=1,4
          SIGVM(I,J) = SIGVM(I,J)*FAC
          SLOVM(I,J) = SLVM1(I,J)/MAX(DEPS,SLVM2(I,J))*FACSL
 140    CONTINUE
 130  CONTINUE
C
      SIGVM(0,0) = ZERO
      DO 150 I=1,4
        SIGVM(0,I) = ZERO
        SIGVM(I,0) = ZERO
        DO 160 J=1,4
          SIGVM(0,I) = SIGVM(0,I) + SIGVM(J,I)
          SIGVM(I,0) = SIGVM(I,0) + SIGVM(I,J)
 160    CONTINUE
        SIGVM(0,0) = SIGVM(0,0) + SIGVM(I,0)
 150  CONTINUE
C
      SIGLSD(1) = SIGLSD(1)*FAC
      SIGLSD(2) = SIGLSD(2)*FAC
      SIGLDD    = SIGLDD   *FAC
      SIGHSD(1) = SIGHSD(1)*FAC
      SIGHSD(2) = SIGHSD(2)*FAC
      SIGHDD    = SIGHDD   *FAC
      SIGCDF    = SIGCDF   *FAC
      SIG1SO    = SIG1SO   *FAC
      SIG1HA    = SIG1HA   *FAC
C
      SIGINE    = SIGTOT - SIGELA
C
 500  CONTINUE
      IF(IFPAP(1).NE.22) THEN
        VMESA(1) = PANAME(IFPAB(1),0)
      ELSE
        VMESA(1) = 'RHO     '
      ENDIF
      IF(IFPAP(2).NE.22) THEN
        VMESB(1) = PANAME(IFPAB(2),0)
      ELSE
        VMESB(1) = 'RHO     '
      ENDIF
C
C  write out physical cross sections
      IF(IDEB(57).GE.5) THEN
        WRITE(6,'(/1X,A,I3,/1X,A)')
     &    'POXSEC:DEBUG: CROSS SECTIONS (mb) FOR COMBINATION:',IP,
     &    '=================================================='
        WRITE(6,'(5X,A,E12.3,2E11.3)')'ENERGY,VIRTUALITIES',ECM,PVIRT
        WRITE(6,'(5X,A,E12.3)') '             TOTAL ',SIGTOT
        WRITE(6,'(5X,A,E12.3)') '    PURELY ELASTIC ',SIGELA
        WRITE(6,'(5X,A,E12.3)') '         INELASTIC ',SIGINE
        WRITE(6,'(5X,A,E12.3)') ' S-DIFF.PARTICLE 1 ',
     &    SIGLSD(1)+SIGHSD(1)
        IF(IDEB(57).GE.5) THEN
          WRITE(6,'(5X,A,E12.3)') '     LOW-MASS PART ',SIGLSD(1)
          WRITE(6,'(5X,A,E12.3)') '    HIGH-MASS PART ',SIGHSD(1)
        ENDIF
        WRITE(6,'(5X,A,E12.3)') ' S-DIFF.PARTICLE 2 ',
     &    SIGLSD(2)+SIGHSD(2)
        IF(IDEB(57).GE.5) THEN
          WRITE(6,'(5X,A,E12.3)') '     LOW-MASS PART ',SIGLSD(2)
          WRITE(6,'(5X,A,E12.3)') '    HIGH-MASS PART ',SIGHSD(2)
        ENDIF
        WRITE(6,'(5X,A,E12.3)') '       DOUBLE DIFF ',SIGLDD+SIGHDD
        WRITE(6,'(5X,A,E12.3)') '     LOW-MASS PART ',SIGLDD
        WRITE(6,'(5X,A,E12.3)') '    HIGH-MASS PART ',SIGHDD
        WRITE(6,'(5X,A,E12.3)') '    DOUBLE POMERON ',SIGCDF
        WRITE(6,'(5X,A,E12.3)') '     ELASTIC SLOPE ',SLOEL
        DO 200 I=1,4
          DO 210 J=1,4
            IF(SIGVM(I,J).GT.DEPS) THEN
              WRITE(6,'(1X,A,A8,A8)') 'Q-ELASTIC PRODUCTION OF ',
     &          VMESA(I),VMESB(J)
              WRITE(6,'(10X,A,E12.3)') 'CROSS SECTION ',SIGVM(I,J)
              IF((I.NE.0).AND.(J.NE.0))
     &          WRITE(6,'(18X,A,E12.3)') 'SLOPE ',SLOVM(I,J)
            ENDIF
 210      CONTINUE
 200    CONTINUE
        WRITE(6,'(5X,A,E12.3)') ' VMESON PRODUCTION ',SIGVM(0,0)
        WRITE(6,'(5X,A,E12.3)') '  ONE-POMERON SOFT ',SIG1SO
        WRITE(6,'(5X,A,E12.3)') '  ONE-POMERON HARD ',SIG1HA
        WRITE(6,'(5X,A,E12.3)') '  POMERON EXCHANGE ',SIGPOM
        WRITE(6,'(5X,A,E12.3)') '  REGGEON EXCHANGE ',SIGREG
        WRITE(6,'(5X,A,E12.3)') ' HARD RESOLVED QCD ',DREAL(DSIGH(9))
        WRITE(6,'(5X,A,E12.3/)')'   HARD DIRECT QCD ',DREAL(DSIGH(15))
      ENDIF
C
      ECM = ETMP
      END
C
C
      SUBROUTINE IMPAMP(EE,BMIN,BMAX,NSTEP)
C*********************************************************************
C
C     calculation of physical  impact parameter amplitude
C
C     input:   EE      cm energy (GeV)
C              BMIN    lower bound in B
C              BMAX    upper bound in B
C              NSTEP   number of values (linear)
C
C     output:  values written to output unit
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER(ZERO=0.D0,
     &         OHALF=0.5D0,
     &           ONE=1.D0,
     &          ONEM=-1.D0,
     &           TWO=2.D0,
     &          FOUR=4.D0,
     &         THOUS=1.D3,
     &          DEPS=1.D-20)
C
      COMPLEX*16      AMPEL,AMPVM,AMPSOF,AMPHAR,AMLMSD,AMHMSD,AMLMDD,
     &                AMHMDD,AMPDP,AMPD1,AMPD2
      COMMON /AMPLIT/ AMPEL,AMPVM(4,4),AMPSOF,AMPHAR,AMLMSD(2),
     &                AMHMSD(2),AMLMDD,AMHMDD,AMPDP,AMPD1,AMPD2
C
      ECM=EE
      BSTEP = (BMAX-BMIN)/DBLE(NSTEP-1)
C
      WRITE(6,'(3(/,1X,A))')
     &  'IMPACT PARAMETER AMPLITUDES:',
     &  '  B  AMP-EL  AMP-LMSD(1,2)  AMP-HMSD(1,2)  AMP-LMDD  AMP-HMDD',
     &  '============================================================='
C
      BB = BMIN
      DO 100 I=1,NSTEP
C  calculate impact parameter amplitudes
        CALL EIKON(1,BB)
        WRITE(6,'(1X,8E12.4)') BB,DREAL(AMPEL),
     &    DREAL(AMLMSD(1)),DREAL(AMLMSD(2)),
     &    DREAL(AMHMSD(1)),DREAL(AMHMSD(2)),DREAL(AMLMDD),DREAL(AMHMDD)
        BB = BB+BSTEP
 100  CONTINUE
C
      END
C
C
      SUBROUTINE PRBDIS(IP,ECM,IE)
C*********************************************************************
C
C     calculation of multi interactions probabilities
C
C     input:  IP        particle combination to scatter
C             ECM       CMS energy
C             IE        index for weight storing
C             COMMON /PROBAB/
C             IMAX      max. number of soft pomeron interactions
C             KMAX      max. number of hard pomeron interactions
C             LMAX      max. number of triple/loop-Pomeron interactions
C             MMAX      max. number of double-Pomeron interactions
C
C     output: COMMON /PROBAB/
C             PROB      field of probabilities
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(ZERO=0.D0,
     &         IZERO=0,
     &          IONE=1,
     &          ITWO=2,
     &        ITHREE=3,
     &         IFOUR=4,
     &         OHALF=0.5D0,
     &           ONE=1.D0,
     &           TWO=2.D0,
     &         THREE=3.D0,
     &          FOUR=4.D0,
     &         THOUS=1.D3,
     &         DDEPS=1.D-40,
     &          DEPS=1.D-20,
     &           EPS=1.D-10)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16      SIGP,SIGR,SIGHD,SIGHR,SIGT1,SIGT2,SIGL,SIGDP,
     &                SIGD1,SIGD2,DSIGH
      COMMON /SIGMAS/ SIGP,SIGR,SIGHD,SIGHR,SIGT1(2),SIGT2(2),SIGL,
     &                SIGDP(4),SIGD1(2),SIGD2(2),DSIGH(0:MAXPRO)
C
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER(IEETAB=10,IIMAX=30,KKMAX=10,LLMAX=2,MMMAX=1)
      REAL PROB
      COMMON /PROBAB/ PROB(4,IEETAB,0:IIMAX,0:KKMAX),EPTAB(4,IEETAB),
     &                IEEMAX,IMAX,KMAX
      COMMON /XSETAB/ SIGTAB(4,70,IEETAB),SIGECM(4,IEETAB),ISIMAX
      COMMON /AVKLMN/ AVERI,AVERK,AVERL,AVERM,AVERN
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER(MDIM=5)
      COMMON /ZXEIKS/ XM(MDIM,MDIM),XOUT(MDIM,MDIM),
     &                XIN(MDIM),XOU(MDIM),
     &                ZXP(MDIM,MDIM),BXP(MDIM,MDIM),
     &                ZXR(MDIM,MDIM),BXR(MDIM,MDIM),
     &                ZXH(MDIM,MDIM),BXH(MDIM,MDIM),
     &                ZXD(MDIM,MDIM),BXD(MDIM,MDIM),
     &                ZXT1A(MDIM,MDIM),BXT1A(MDIM,MDIM),
     &                ZXT1B(MDIM,MDIM),BXT1B(MDIM,MDIM),
     &                ZXT2A(MDIM,MDIM),BXT2A(MDIM,MDIM),
     &                ZXT2B(MDIM,MDIM),BXT2B(MDIM,MDIM),
     &                ZXL(MDIM,MDIM),BXL(MDIM,MDIM),
     &                ZXDP(MDIM,MDIM),BXDP(MDIM,MDIM),
     &                ZXD1A(MDIM,MDIM),BXD1A(MDIM,MDIM),
     &                ZXD1B(MDIM,MDIM),BXD1B(MDIM,MDIM),
     &                ZXD2A(MDIM,MDIM),BXD2A(MDIM,MDIM),
     &                ZXD2B(MDIM,MDIM),BXD2B(MDIM,MDIM)
C  local variables
      DIMENSION  AB(4,4),CHI2(4),ABSUM2(4,4),ABSTMP(4),CHITMP(4)
      PARAMETER (ICHMAX=40)
      DIMENSION  CHIFAC(4,4),ELAFAC(4),PCHAIN(2,ICHMAX)
C
      DIMENSION AMPFAC(4),XPNT(96),WGHT(96)
      DIMENSION FACLOG(0:30),PSOFT(0:30),PHARD(0:20)
C  combinatorical factors
      DATA      CHIFAC / 1.D0, 1.D0,-1.D0,-1.D0,
     &                   1.D0,-1.D0, 1.D0,-1.D0,
     &                   1.D0,-1.D0,-1.D0, 1.D0,
     &                   1.D0, 1.D0, 1.D0, 1.D0 /
      DATA FACLOG /           .000000000000000D+00,
     &  .000000000000000D+00, .693147180559945D+00,
     &  .109861228866811D+01, .138629436111989D+01,
     &  .160943791243410D+01, .179175946922805D+01,
     &  .194591014905531D+01, .207944154167984D+01,
     &  .219722457733622D+01, .230258509299405D+01,
     &  .239789527279837D+01, .248490664978800D+01,
     &  .256494935746154D+01, .263905732961526D+01,
     &  .270805020110221D+01, .277258872223978D+01,
     &  .283321334405622D+01, .289037175789616D+01,
     &  .294443897916644D+01, .299573227355399D+01,
     &  .304452243772342D+01, .309104245335832D+01,
     &  .313549421592915D+01, .317805383034795D+01,
     &  .321887582486820D+01, .325809653802148D+01,
     &  .329583686600433D+01, .333220451017520D+01,
     &  .336729582998647D+01, .340119738166216D+01 /
C
      DATA  ELAST / 0.D0 /
C
C  test for redundant calculation: skip cs calculation
      IF(ECM.NE.ELAST) THEN
        ELAST = ECM
        CALL POXSEC(IP,ELAST)
        ISIMAX = IE
        SIGECM(IP,IE) = ECM
        SIGTAB(IP,1,IE) = SIGTOT
        SIGTAB(IP,2,IE) = SIGELA
        J = 2
        DO 5 I=0,4
          DO 6 K=0,4
            J = J+1
            SIGTAB(IP,J,IE) = SIGVM(I,K)
 6        CONTINUE
 5      CONTINUE
        SIGTAB(IP,28,IE) = SIGINE
        SIGTAB(IP,29,IE) = SIGDIR
        SIGTAB(IP,30,IE) = SIGLSD(1)
        SIGTAB(IP,31,IE) = SIGLSD(2)
        SIGTAB(IP,32,IE) = SIGHSD(1)
        SIGTAB(IP,33,IE) = SIGHSD(2)
        SIGTAB(IP,34,IE) = SIGLDD
        SIGTAB(IP,35,IE) = SIGHDD
        SIGTAB(IP,36,IE) = SIGCDF
        SIGTAB(IP,37,IE) = SIG1SO
        SIGTAB(IP,38,IE) = SIG1HA
        SIGTAB(IP,39,IE) = SLOEL
        J = 39
        DO 7 I=1,4
          DO 8 K=1,4
            J = J+1
            SIGTAB(IP,J,IE) = SLOVM(I,K)
 8        CONTINUE
 7      CONTINUE
        FAC = (VDMQ2F(1)+VDMQ2F(2))*(VDMQ2F(3)+VDMQ2F(4))
        SIGTAB(IP,56,IE) = SIGPOM*FAC
        SIGTAB(IP,57,IE) = SIGREG*FAC
        SIGTAB(IP,58,IE) = DREAL(DSIGH(9))
        SIGTAB(IP,59,IE) = DREAL(DSIGH(15))
        SIGTAB(IP,60,IE) = SIGTR1
        SIGTAB(IP,61,IE) = SIGTR2
        SIGTAB(IP,62,IE) = SIGLOO
        SIGTAB(IP,63,IE) = SIGDPO
      ENDIF
C
*     BMAX=5.D0*SQRT(DBLE(BPOM))
      BMAX=10.D0
      EPTAB(IP,IE) = ECM
C
      CALL PHGSET(ZERO,BMAX,NGAUSO,XPNT,WGHT)
      DO 160 K=0,KMAX
        DO 170 I=0,IMAX
          PROB(IP,IE,I,K) = ZERO
 170    CONTINUE
 160  CONTINUE
      DO 120 I=1,ICHMAX
        PCHAIN(1,I) = ZERO
        PCHAIN(2,I) = ZERO
 120  CONTINUE
C
      SIGRES = SIGTOT-SIGELA-SIGVM(0,0)-SIGCDF-SIGDIR
     &       - SIGLSD(1)-SIGLSD(2)-SIGLDD
      AUXFAC = PI2/SIGRES
      IF(ISWMDL(1).EQ.3) THEN
        AMPFAC(1) = SQRT(VDMQ2F(1)*VDMQ2F(3))
        AMPFAC(2) = SQRT(VDMQ2F(2)*VDMQ2F(3))
        AMPFAC(3) = SQRT(VDMQ2F(1)*VDMQ2F(4))
        AMPFAC(4) = SQRT(VDMQ2F(2)*VDMQ2F(4))
        ELAFAC(1) = (VDMQ2F(1)*VDMQ2F(3)+VDMQ2F(2)*VDMQ2F(3)
     &              +VDMQ2F(1)*VDMQ2F(4)+VDMQ2F(2)*VDMQ2F(4))/FOUR
        ELAFAC(2) = (AMPFAC(1)*AMPFAC(2)+AMPFAC(3)*AMPFAC(4))/TWO
        ELAFAC(3) = (AMPFAC(1)*AMPFAC(3)+AMPFAC(2)*AMPFAC(4))/TWO
        ELAFAC(4) = AMPFAC(1)*AMPFAC(4)
        DO 133 I=1,4
          AMPFAC(I) = ZERO
          DO 135 K=1,4
            AMPFAC(I) = AMPFAC(I) + ELAFAC(K)*CHIFAC(K,I)
 135      CONTINUE
          AMPFAC(I) = AMPFAC(I)*AUXFAC
 133    CONTINUE
      ENDIF
C
C  main cross section loop
C**********************************************************
      DO 5000 IB=1,NGAUSO
        B24=XPNT(IB)**2/FOUR
        FAC = XPNT(IB)*WGHT(IB)
C
        IF((ISWMDL(1).EQ.3).OR.(ISWMDL(1).EQ.4)) THEN
C
C  amplitude construction
          DO 525 I=1,4
            AB(1,I)=ZXP(1,I)*EXP(-B24/BXP(1,I))
     &              +ZXR(1,I)*EXP(-B24/BXR(1,I))
            AB(2,I)=ZXH(1,I)*EXP(-B24/BXH(1,I))
            AB(3,I)=-ZXT1A(1,I)*EXP(-B24/BXT1A(1,I))
     &              -ZXT1B(1,I)*EXP(-B24/BXT1B(1,I))
     &              -ZXT2A(1,I)*EXP(-B24/BXT2A(1,I))
     &              -ZXT2B(1,I)*EXP(-B24/BXT2B(1,I))
     &              -ZXL(1,I)*EXP(-B24/BXL(1,I))
            AB(4,I)=ZXDP(1,I)*EXP(-B24/BXDP(1,I))
*
*           CHKSUM = AB(2,I)/(AB(1,I)+AB(2,I))
            CHKSUM = 0.D0
            AB(1,I) = AB(1,I)+(AB(3,I)+AB(4,I))*(1.D0-CHKSUM)
            AB(2,I) = AB(2,I)+(AB(3,I)+AB(4,I))*CHKSUM
            AB(3,I) = ZERO
            AB(4,I) = ZERO
*
 525      CONTINUE
C
          DO 460 I=1,4
            DO 500 K=1,4
              ABSUM2(I,K) = ZERO
              DO 550 L=1,4
                ABSUM2(I,K) = ABSUM2(I,K) + CHIFAC(L,K)*AB(I,L)
 550          CONTINUE
              ABSUM2(I,K) = TWO*ABSUM2(I,K)
 500        CONTINUE
 460      CONTINUE
          DO 600 I=1,4
            CHI2(I) = ZERO
            DO 650 K=1,4
              CHI2(I) = CHI2(I) + ABSUM2(K,I)
 650        CONTINUE
 600      CONTINUE
C  sums instead of products
          DO 660 I=1,4
            DO 670 KD=1,4
              DTMP = ABS(ABSUM2(I,KD))
              IF(DTMP.LT.1.D-30) THEN
                ABSUM2(I,KD) = -50.D0
              ELSE
                ABSUM2(I,KD) = LOG(DTMP)
              ENDIF
 670        CONTINUE
 660      CONTINUE
C
          DO 700 KD=1,4
            DO 750 I=1,4
              ABSTMP(I) = ABSUM2(I,KD)
 750        CONTINUE
C  recursive sum
            CHITMP(1) = -ABSUM2(1,KD)
            DO 800 I=0,IMAX
              CHITMP(1) = CHITMP(1)+ABSTMP(1)-FACLOG(I)
              CHITMP(2) = -ABSTMP(2)
              DO 810 K=0,KMAX
                CHITMP(2) = CHITMP(2)+ABSTMP(2)-FACLOG(K)
C  calculation of elastic part
                DTMP = -CHI2(KD)+CHITMP(1)+CHITMP(2)
                IF(DTMP.LT.-30.D0) THEN
                  DTMP = ZERO
                ELSE
                  DTMP = EXP(DTMP)*FAC*AMPFAC(KD)
                ENDIF
                PROB(IP,IE,I,K) = PROB(IP,IE,I,K) + DTMP
 810          CONTINUE
 800        CONTINUE
 700      CONTINUE
          PROB(IP,IE,0,0) = ZERO
C
C**********************************************************
        ELSE
          WRITE(6,'(1X,A,I2,A)') 'PRBDIS: ERROR: model no ',ISWMDL(1),
     &                       ' not supported'
          STOP
        ENDIF
 5000 CONTINUE
C
C  debug output
      IF(IDEB(55).GE.15) THEN
        WRITE(6,'(/,1X,A,I3,E11.4)')
     &    'PRBDIS:DEBUG:LIST OF PROBABILITIES (UNCORRECTED),IP,ECM:',
     &    IP,ECM
        DO 905 I=0,MIN(IMAX,5)
          DO 915 K=0,MIN(KMAX,5)
            IF(ABS(PROB(IP,IE,I,K)).GT.1.D-10)
     &        WRITE(6,'(10X,2I3,5X,E12.3)') I,K,PROB(IP,IE,I,K)
 915      CONTINUE
 905    CONTINUE
      ENDIF
C  chain probability (uncorrected)
      IF(IDEB(55).GE.5) THEN
        DO 955 I=0,IMAX
          DO 965 K=0,KMAX
            INDX = 2*I+2*K
            IF((INDX.LE.ICHMAX).AND.(INDX.GT.0)) THEN
              PCHAIN(1,INDX) = PCHAIN(1,INDX) + PROB(IP,IE,I,K)
            ENDIF
 965      CONTINUE
 955    CONTINUE
        WRITE(6,'(/1X,A,E11.4)')
     &  'PRBDIS:DEBUG:LIST OF SELECTED PROBABILITIES (UNCORR),ECM',ECM
        WRITE(6,'(10X,A)') 'I,   0HPOM,   1HPOM,   2HPOM'
        DO 183 I=0,IIMAX
          IF(ABS(PROB(IP,IE,I,0)).GT.1.D-10)
     &      WRITE(6,'(5X,I4,3E12.4)') I,PROB(IP,IE,I,0),
     &      PROB(IP,IE,I,1),PROB(IP,IE,I,2)
 183    CONTINUE
      ENDIF
C
C  probability check
      CHKSUM = ZERO
      PRONEG = ZERO
      AVERI =  ZERO
      AVERK =  ZERO
      AVERL =  ZERO
      AVERM =  ZERO
      AVERN =  ZERO
      SIGMI =  ZERO
      SIGMK =  ZERO
      SIGML =  ZERO
      SIGMM =  ZERO
      DO 1001 I=0,IMAX
        PSOFT(I) = 0.D0
 1001 CONTINUE
      DO 1002 K=0,KMAX
        PHARD(K) = 0.D0
 1002 CONTINUE
      DO 1000 K=0,KMAX
        DO 1010 I=0,IMAX
          TMP = PROB(IP,IE,I,K)
          IF(TMP.LT.ZERO) THEN
            IF((IDEB(55).GE.0).AND.(TMP.LT.-EPS)) THEN
              WRITE(6,'(1X,A,4I4,E14.4)')
     &          'PRBDIS:WARNING: NEG.PROBABILITY ',
     &              IP,IE,I,K,PROB(IP,IE,I,K)
            ENDIF
            PRONEG = PRONEG+TMP
            TMP = ZERO
          ENDIF
          CHKSUM = CHKSUM+TMP
          AVERI = AVERI+DBLE(I)*TMP
          AVERK = AVERK+DBLE(K)*TMP
          SIGMI = SIGMI+DBLE(I**2)*TMP
          SIGMK = SIGMK+DBLE(K**2)*TMP
          PSOFT(I) = PSOFT(I)+PROB(IP,IE,I,K)
          PHARD(K) = PHARD(K)+PROB(IP,IE,I,K)
          PROB(IP,IE,I,K) = CHKSUM
 1010   CONTINUE
 1000 CONTINUE
C
      IF(IDEB(55).GE.1) WRITE(6,'(/,1X,A,2E15.6)')
     &  'PRBDIS:DEBUG:FIRST SUM OF PROBABILITIES',CHKSUM,PRONEG
C  chain probabilites output
      IF(IDEB(55).GE.5) THEN
        WRITE(6,'(/1X,A)') 'LIST OF CHAIN PROBABILITIES (UNCORR/CORR)'
        DO 185 I=1,ICHMAX
          IF(ABS(PCHAIN(1,I)).GT.1.D-10)
     &      WRITE(6,'(5X,I4,2E12.3)') I,PCHAIN(1,I),PCHAIN(1,I)/CHKSUM
 185    CONTINUE
      ENDIF
C  rescaling necessary
      IF(ABS(CHKSUM-ONE).GT.1.D-15) THEN
        FAC = ONE/CHKSUM
        IF(IDEB(55).GE.1) WRITE(6,'(/,1X,A,E15.6)')
     &    'PRBDIS:DEBUG:RESCALING OF PROBABILITIES WITH FACTOR',FAC
        DO 40 K=0,KMAX
          DO 50 I=0,IMAX
            PROB(IP,IE,I,K) = PROB(IP,IE,I,K)*FAC
  50      CONTINUE
  40    CONTINUE
        AVERI = AVERI*FAC
        AVERK = AVERK*FAC
        AVERL = AVERL*FAC
        AVERM = AVERM*FAC
        SIGMI = SIGMI*FAC**2
        SIGMK = SIGMK*FAC**2
        SIGML = SIGML*FAC**2
        SIGMM = SIGMM*FAC**2
      ENDIF
C
C  probability to find Reggeon/Pomeron
      PROB(IP,IE,0,0) = -SIGREG/(SIGPOM+SIGREG)
      AVERJ = -PROB(IP,IE,0,0)*AVERI
      AVERII = AVERI-AVERJ
C
      SIGTAB(IP,64,IE) = AVERII
      SIGTAB(IP,65,IE) = AVERK
      SIGTAB(IP,66,IE) = AVERJ
      SIGTAB(IP,67,IE) = AVERL
      SIGTAB(IP,68,IE) = AVERM
C
      SIGTAB(IP,69,IE) = PROB(IP,IE,IMAX,0)*SIGRES
      SIGTAB(IP,70,IE) = SIGRES-SIGTAB(IP,69,IE)
C
      IF(IDEB(55).GE.1) THEN
        WRITE(6,'(/1X,A,/1X,A)')
     &    'PRBDIS:DEBUG:EXPECTED INTERACTION STATISTICS:',
     &    '============================================='
        WRITE(6,'(1X,A,E12.4,2I3)')
     &    'ENERGY,IP,TABLE INDEX:',EPTAB(IP,IE),IP,IE
        WRITE(6,'(1X,A,2I4)') 'CURRENT LIMITATIONS (soft,hard):',
     &    IMAX,KMAX
        WRITE(6,'(1X,A,/,4X,A,/,1X,6E11.3)')
     &    'AVERAGED NUMBER OF CUTS PER EVENT:',
     &    ' (Pom / Pom-h / Reg / enh-tri-loop / enh-dble / sum):',
     &    AVERII,AVERK,AVERJ,AVERL,AVERM,
     &    AVERI+AVERK+AVERL+AVERM
        WRITE(6,'(1X,A,/,4X,A,/,1X,4E11.3)')
     &    'STANDARD DEVIATION ( SQRT(SIGMA) ):',
     &    ' (Pomeron / Pomeron-h / enh-tri-loop / enh-dble):',
     &    SQRT(ABS(SIGMI-AVERI**2)),SQRT(ABS(SIGMK-AVERK**2)),
     &    SQRT(ABS(SIGML-AVERL**2)),SQRT(ABS(SIGMM-AVERM**2))
        WRITE(6,'(1X,A)') 'CROSS SECTION / PROBABILITY  SOFT, HARD'
        DO 1003 I=0,MIN(IMAX,KMAX)
          WRITE(6,'(I5,2E12.4,3X,2E12.4)') 
     &      I,PSOFT(I)*SIGRES,PSOFT(I),PHARD(I)*SIGRES,PHARD(I)
 1003   CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE SAMPRO(IPROC)
C********************************************************************
C
C     routine to sample kind of process
C
C     input:   IPROC
C                 -2     output of statistics
C                 -1     initialization
C                  0     sampling of process
C     output:  IPROC
C                  1     nondiffractive resolved process
C                  2     elastic scattering
C                  3     quasi elastic rho/omega/phi production
C                  4     central diffraction
C                  5     single diffraction according to IDIFF1
C                  6     single diffraction according to IDIFF2
C                  7     double diffraction
C                  8     direct processes
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  1.D-6,
     &           TINY   =  1.D-10)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
C
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      DIMENSION PRO(8),PAC(8),XPROB(8),SIGSDI(2)
C
C  initialization of statistics
      IF(IPROC.GE.0) THEN
        CALLS      = CALLS + ONE
C  probabilities
        IF(ISWMDL(1).EQ.3) THEN
          IF(ISWMDL(2).EQ.1) THEN
            SIGSDI(1) = SIGLSD(1)+SIGHSD(1)
            SIGSDI(2) = SIGLSD(2)+SIGHSD(2)
            SIGDDI    = SIGLDD+SIGHDD
            SIGNDR    = SIGINE-SIGVM(0,0)-SIGCDF-SIGDIR
     &                - SIGSDI(1)-SIGSDI(2)-SIGDDI
            XPROB(1)  = SIGNDR/SIGTOT
            XPROB(2)  = SIGELA/SIGTOT
            XPROB(3)  = SIGVM(0,0)/SIGTOT
            XPROB(4)  = SIGCDF/SIGTOT
            XPROB(5)  = SIGSDI(1)/SIGTOT
            XPROB(6)  = SIGSDI(2)/SIGTOT
            XPROB(7)  = SIGDDI/SIGTOT
            XPROB(8)  = SIGDIR/SIGTOT
          ELSE
            SIGHR = ZERO
            IF(IPRON(1).EQ.1) SIGHR = SIGHAR
            SIGHD = ZERO
            IF(IPRON(8).EQ.1) SIGHD = SIGDIR
            XPROB(1) = SIGHR/(SIGHR+SIGHD)
            XPROB(2) = ZERO
            XPROB(3) = ZERO
            XPROB(4) = ZERO
            XPROB(5) = ZERO
            XPROB(6) = ZERO
            XPROB(7) = ZERO
            XPROB(8) = SIGHD/(SIGHR+SIGHD)
          ENDIF
        ELSE
          WRITE(6,'(/,1X,A,I4)') 'SAMPRO:ERROR:UNSUPPORTED MODEL',
     &      ISWMDL(1)
          CALL POABRT
        ENDIF
C  culumnative values
        DO 230 I=2,8
          XPROB(I) = XPROB(I-1)+XPROB(I)
 230    CONTINUE
        IF(IDEB(11).GE.10) THEN
          WRITE(6,'(/,1X,A)') 'SAMPRO:DEBUG:SUMMED PROB.DISTRIBUTION'
          DO 240 I=1,8
            WRITE(6,'(10X,I3,5X,E12.3)') I,XPROB(I)
 240      CONTINUE
        ENDIF
        IF((ABS(XPROB(8)-ONE).GT.1.D-2).AND.(IDEB(11).GE.0)) THEN
          FAC = ONE/XPROB(8)
          WRITE(6,'(/1X,A,E12.3)')
     &      'SAMPRO:WARNING:INCONSISTENT CROSS SECTIONS, RESCALING',FAC
        ENDIF
C
C  sample process
 50     CONTINUE
          XI = DRNDM(SIGSUM)*XPROB(8)
          DO 100 I=1,8
            IF(XI.LE.XPROB(I)) GOTO 110
 100      CONTINUE
 110      CONTINUE
          IPROC = MIN(I,8)
C
          PITER      = PITER + ONE
          PRO(IPROC) = PRO(IPROC) + ONE
          IF(ISWMDL(2).EQ.1) THEN
            SIGSUM = SIGSUM+SIGTOT
          ELSE
            SIGSUM = SIGSUM+SIGGEN(3)
          ENDIF
C  reject certain processes
        IF(IPRON(IPROC).EQ.0) GOTO 50
        PAC(IPROC) = PAC(IPROC) + ONE
        ECMSUM = ECMSUM + ECM
C  debug output
        IF(IDEB(11).GT.0) 
     &    WRITE(6,'(1X,A,I4)') 'SAMPRO:DEBUG:PROCESS-ID',IPROC
C
C  statistics initialization
      ELSE IF(IPROC.EQ.-1) THEN
        DO 250 I=1,8
          PRO(I) = ZERO
          PAC(I) = ZERO
 250    CONTINUE
        CALLS = ZERO
        PITER = ZERO
        SIGSUM = ZERO
        ECMSUM = ZERO
C
C  write out statistics
      ELSE IF(IPROC.EQ.-2) THEN
        SIGSUM = SIGSUM/PITER**2
        ECMSUM = ECMSUM/CALLS
        IF(IDEB(11).GE.0) THEN
          WRITE(6,'(/,1X,A,F12.0,E12.4,/,1X,A)')
     &      'SAMPRO:PROCESS STATISTICS (CALLS,ENERGY)',CALLS,ECMSUM,
     &      '========================='
          WRITE(6,'(8X,A)')
     &          'PROCESS      TRIED     CROSS SECTION   ACCEPTED'
          IF(ISWMDL(2).EQ.1) THEn
            WRITE(6,'(9(/5X,A,F12.0,5X,E12.4,5X,F12.0))')
     &        '    ALL PROCESSES',PITER,PITER*SIGSUM,CALLS,
     &        ' NONDIF.INELASTIC',PRO(1),PRO(1)*SIGSUM,PAC(1),
     &        '          ELASTIC',PRO(2),PRO(2)*SIGSUM,PAC(2),
     &        'VMESON PRODUCTION',PRO(3),PRO(3)*SIGSUM,PAC(3),
     &        '   DOUBLE POMERON',PRO(4),PRO(4)*SIGSUM,PAC(4),
     &        ' SINGLE DIFFR.(1)',PRO(5),PRO(5)*SIGSUM,PAC(5),
     &        ' SINGLE DIFFR.(2)',PRO(6),PRO(6)*SIGSUM,PAC(6),
     &        ' DOUBLE DIFFRACT.',PRO(7),PRO(7)*SIGSUM,PAC(7),
     &        ' DIRECT PROCESSES',PRO(8),PRO(8)*SIGSUM,PAC(8)
          ELSE
            WRITE(6,'(3(/5X,A,F12.0,5X,E12.4,5X,F12.0))')
     &        '    ALL PROCESSES',PITER,PITER*SIGSUM,CALLS,
     &        '  DOUBLE RESOLVED',PRO(1),PRO(1)*SIGSUM,PAC(1),
     &        ' SINGLE RES + DIR',PRO(8),PRO(8)*SIGSUM,PAC(8)
            ENDIF
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE SAMPRB(ECMI,IP,ISAM,JSAM,KSAM,LSAM,MSAM)
C********************************************************************
C
C     routine to sample number of cut graphs of different kind
C
C     input:  IP      scattering particle combination
C             ECMI    CMS energy
C             IP      -1         initialization
C                     -2         output of statistics
C                     others     sampling of cuts
C
C     output: ISAM    number of soft Pomerons cut
C             JSAM    number of soft Reggeons cut
C             KSAM    number of hard Pomerons cut
C             LSAM    number of triple/loop-Pomerons diffractively cut
C             MSAM    number of double-Pomerons cut
C
C     PRBDIS has to be called before
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
C
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      PARAMETER(IEETAB=10,IIMAX=30,KKMAX=10,LLMAX=2,MMMAX=1)
      REAL PROB
      COMMON /PROBAB/ PROB(4,IEETAB,0:IIMAX,0:KKMAX),EPTAB(4,IEETAB),
     &                IEEMAX,IMAX,KMAX
C  global energy
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C  global cross sections
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      DIMENSION ECMSUM(4),CALLS(4),
     &          AVERI(4),AVERJ(4),AVERK(4),AVERL(4),AVERM(4),
     &          DEVI(4),DEVK(4),DEVL(4),DEVM(4),AVERB(0:2),AVERC(0:2)
C
*     DIMENSION IHSPOM(4),IHHPOM(4),IHAPOM(4)
*     CHARACTER*72 TITLE
C
C  sample number of interactions
      IF(IP.GE.0) THEN
        KLAST = 0
        ITER = 0
        ECMX = ECMI
        KLIM = 1
        IF((IPAMDL(13).GT.0).AND.(IPROCE.EQ.1)) THEN
	  ECMX = SECM
          KLIM = 0
        ENDIF
C  sample up to kinematic limits only
        IMAX1 = MIN(IMAX,INT(0.4D0*ECMX/CUTMU(1)))
        IF(IMAX1.LT.1) THEN
          ISAM = 0
          JSAM = 1
          KSAM = 0
          LSAM = 0
          MSAM = 0
          GOTO 150
        ENDIF
C  find interpolation factors
        IF(ECMX.LE.EPTAB(IP,1)) THEN
          I1 = 1
          I2 = 1
        ELSE IF(ECMX.LT.EPTAB(IP,IEEMAX)) THEN
          DO 50 I=2,IEEMAX
            IF(ECMX.LE.EPTAB(IP,I)) GOTO 200
 50       CONTINUE
 200      CONTINUE
          I1 = I-1
          I2 = I
        ELSE
          WRITE(6,'(/1X,A,2E12.3)')
     &      'SAMPRB:WARNING:TOO HIGH ENERGY',ECMX,EPTAB(IP,IEEMAX)
          CALL POPREV(-1)
          I1 = IEEMAX
          I2 = IEEMAX
        ENDIF
        FAC2 = 0.D0
        IF(I1.NE.I2)
     &    FAC2=LOG(ECMX/EPTAB(IP,I1))/LOG(EPTAB(IP,I2)/EPTAB(IP,I1))
        FAC1=1.D0-FAC2
        PREG = -PROB(IP,I1,0,0)*FAC1-PROB(IP,I2,0,0)*FAC2
C
 10     CONTINUE
        ITER = ITER+1
        PRO0 = PROB(IP,I1,0,KLAST)*FAC1+PROB(IP,I2,0,KLAST)*FAC2
        XI = DRNDM(FAC2)*(ONE-PRO0)+PRO0
        DO 260 KSAM=KLAST,KMAX
          DO 270 ISAM=0,IMAX
            PRO = PROB(IP,I1,ISAM,KSAM)*FAC1
     &           +PROB(IP,I2,ISAM,KSAM)*FAC2
            IF(PRO.GT.XI) GOTO 100
 270      CONTINUE
 260    CONTINUE
        ISAM = MIN(IMAX,ISAM)
        KSAM = MIN(KMAX,KSAM)
        KSAM = 0
        LSAM = 0
        MSAM = 0
C
 100    CONTINUE
*       IF(ITER.EQ.1) KLAST = MIN(1,KSAM)
        IF((IP.EQ.1).AND.(ITER.EQ.1)) THEN
          AVERB(0) = AVERB(0)+1.D0
          AVERB(1) = AVERB(1)+DBLE(ISAM)
          AVERB(2) = AVERB(2)+DBLE(KSAM)
        ENDIF
        IF(ITER.GT.100) THEN
          ISAM = 0
          JSAM = 1
          KSAM = 0
          LSAM = 0
          MSAM = 0
          IF(IDEB(12).GE.3) WRITE(6,'(1X,A,I10,E11.3,I6)')
     &      'SAMPRB:WARNING:REJECTION (EV,ECM,ITER)',KEVENT,ECMX,ITER
        ELSE
C  reggeon contribution
          JSAM = 0
          IF(IPAMDL(2).EQ.1) THEN
            DO 90 I=1,ISAM
              IF(DRNDM(PRO).LT.PREG) JSAM = JSAM+1
 90         CONTINUE
            ISAM = ISAM-JSAM
          ENDIF
C  limitation given by field dimensions
          IF((2*ISAM+JSAM+3*KSAM).GT.50) GOTO 10
          IF(ISWMDL(14).EQ.0) THEN
            IF(LSAM+MSAM.NE.0) GOTO 10
          ENDIF
 120      CONTINUE
C  reweight according to virtualities
            WGX = 1
            IF(IP.EQ.1) THEN
              FSUPP1 = FSUP(1)*FSUP(2)
              I = MAX(0,ISAM+JSAM)
              WGX = FSUPP1**I
              IF(WGX.LT.DRNDM(ECMX)) GOTO 10
            ENDIF
C  phase space limitation
            IF(ISAM+JSAM+KSAM.EQ.0) ISAM = 1
            XM = DBLE(2*ISAM+JSAM)*CUTMU(1)
     &          +DBLE(2*LSAM+2*MSAM)*CUTMU(2)
     &          +DBLE(2*KSAM)*PTCUT(IP)
            PACC = WGX*EXP(PARMDL(9)*(CUTMU(1)-XM)/ECMX)
            IF(DRNDM(XM).GT.PACC) THEN
              IF(ISAM+JSAM+KSAM.GT.1) THEN
                IF(JSAM.GT.0) THEN
                  JSAM = JSAM-1
                  GOTO 120
                ELSE IF(ISAM.GT.0) THEN
                  ISAM = ISAM-1
                  GOTO 120
                ELSE IF(KSAM.GT.KLIM) THEN
                  KSAM = KSAM-1
                  GOTO 120
                ENDIF
              ENDIF
            ENDIF
        ENDIF
C
        ISAM = ISAM+JSAM/2
        JSAM = MOD(JSAM,2)
 150    CONTINUE
        ECMSUM(IP) = ECMSUM(IP)+ECMX
        AVERI(IP) = AVERI(IP) + DBLE(ISAM)
        AVERJ(IP) = AVERJ(IP) + DBLE(JSAM)
        AVERK(IP) = AVERK(IP) + DBLE(KSAM)
        AVERL(IP) = AVERL(IP) + DBLE(LSAM)
        AVERM(IP) = AVERM(IP) + DBLE(MSAM)
        DEVI(IP)  = DEVI(IP)  + DBLE((ISAM+JSAM)**2)
        DEVK(IP)  = DEVK(IP)  + DBLE(KSAM**2)
        DEVL(IP)  = DEVL(IP)  + DBLE(LSAM**2)
        DEVM(IP)  = DEVM(IP)  + DBLE(MSAM**2)
        CALLS(IP) = CALLS(IP) + 1.D0
*       CALL FILHIS(DBLE(ISAM),1.D0,IHSPOM(IP))
*       CALL ADDHIS(IHSPOM(IP))
*       CALL FILHIS(DBLE(KSAM),1.D0,IHHPOM(IP))
*       CALL ADDHIS(IHHPOM(IP))
*       CALL FILHIS(DBLE(ISAM+KSAM),1.D0,IHAPOM(IP))
*       CALL ADDHIS(IHAPOM(IP))
        IF(IP.EQ.1) THEN
          AVERC(0) = AVERC(0)+1.D0
          AVERC(1) = AVERC(1)+DBLE(ISAM)
          AVERC(2) = AVERC(2)+DBLE(KSAM)
        ENDIF
C
        IF(IDEB(12).GE.10) THEN
          WRITE(6,'(1X,A,2E11.4,5I3)')'SAMPRB:DEBUG:(ECM,I,J,K,L,M)',
     &      ECM,ECMX,ISAM,JSAM,KSAM,LSAM,MSAM
        ENDIF
C
C  initialize statistics
      ELSE IF(IP.EQ.-1) THEN
        DO 60 I=1,4
          ECMSUM(I) = ZERO
          AVERI(I) = ZERO
          AVERJ(I) = ZERO
          AVERK(I) = ZERO
          AVERL(I) = ZERO
          AVERM(I) = ZERO
          DEVI(I)  = ZERO
          DEVK(I)  = ZERO
          DEVL(I)  = ZERO
          DEVM(I)  = ZERO
          CALLS(I) = ZERO
*         CALL NEWHIS(0.5D0,15.5D0,ZERO,CALLS,15,IHSPOM(I))
*         CALL NEWHIS(0.5D0,15.5D0,ZERO,CALLS,15,IHHPOM(I))
*         CALL NEWHIS(0.5D0,20.5D0,ZERO,CALLS,20,IHAPOM(I))
 60     CONTINUE
        DO 65 I=0,2
          AVERB(I) = 0.D0
          AVERC(I) = 0.D0
 65     CONTINUE
        RETURN
C
C  write out statistics
      ELSE IF(IP.EQ.-2) THEN
        DO 70 I=1,4
          IF(CALLS(I).LT.2.D0) GOTO 75
          CALLS(I) = MAX(CALLS(I),ONE)
          WRITE(6,'(/,1X,A,I3,E13.4,F12.0,/,1X,A)')
     &      'SAMPRB:INTERACTION STATISTICS (IP,<ECM>,CALLS)',
     &      I,ECMSUM(I)/CALLS(I),CALLS(I),
     &      '============================='
          AVERI(I) = AVERI(I)/CALLS(I)
          AVERJ(I) = AVERJ(I)/CALLS(I)
          AVERK(I) = AVERK(I)/CALLS(I)
          AVERL(I) = AVERL(I)/CALLS(I)
          AVERM(I) = AVERM(I)/CALLS(I)
          DEVI(I)  = SQRT(DEVI(I)/CALLS(I)-(AVERI(I)+AVERJ(I))**2)
          DEVK(I)  = SQRT(DEVK(I)/CALLS(I)-AVERK(I)**2)
          DEVL(I)  = SQRT(DEVL(I)/CALLS(I)-AVERL(I)**2)
          DEVM(I)  = SQRT(DEVM(I)/CALLS(I)-AVERM(I)**2)
C
          WRITE(6,'(1X,A,I3/,4X,A,/,1X,6E11.3)')
     &    'AVERAGED NUMBER OF INTERACTIONS:',I,
     &' (Pom / Pom-h / Reg / enh-tri-loop / enh-dble / sum):',
     &     AVERI(I),AVERK(I),AVERJ(I),AVERL(I),AVERM(I),
     &     AVERI(I)+AVERK(I)+AVERJ(I)+AVERL(I)+AVERM(I)
          WRITE(6,'(1X,A,/,4X,A,/,1X,4E11.3)')
     &     'STANDARD DEVIATION ( SQRT(SIGMA) ):',
     &' (Pomeron / Pomeron-h / enh-tri-loop / enh-dble):',
     &     DEVI(I),DEVK(I),DEVL(I),DEVM(I)
C
          IF(I.EQ.1) THEN
            WRITE(6,'(1X,A)') 'AVERAGE I,K (bare)'
            WRITE(6,'(1X,F12.0,2E12.4)') AVERB(0),AVERB(1)/AVERB(0),
     &        AVERB(2)/AVERB(0)
            WRITE(6,'(1X,A)') 'AVERAGE I,K (with energy correction)'
            WRITE(6,'(1X,F12.0,2E12.4)') AVERC(0),AVERC(1)/AVERC(0),
     &        AVERC(2)/AVERC(0)
          ENDIF
C
*         TITLE = 'DISTRIBUTION OF SAMPLED SOFT CUT POMERONS'
*         CALL OUTHIS(IHSPOM(I),1,TITLE,ONE,0)
*         TITLE = 'DISTRIBUTION OF SAMPLED HARD CUT POMERONS'
*         CALL OUTHIS(IHHPOM(I),1,TITLE,ONE,0)
*         TITLE = 'DISTRIBUTION OF SAMPLED CUT POMERONS'
*         CALL OUTHIS(IHAPOM(I),1,TITLE,ONE,0)
 75       CONTINUE
 70     CONTINUE
        RETURN
      ENDIF
      END
C
C
      SUBROUTINE TRIREG(S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP,VIR2A,
     &                  SIGTR,BTR)
C**********************************************************************
C
C     calculation of triple-Pomeron total cross section
C     according to Gribovs Regge theory
C
C     input:        S        squared cms energy
C                   GA       coupling constant to diffractive line
C                   AA       slope related to GA (GeV**-2)
C                   GB       coupling constant to elastic line
C                   BB       slope related to GB (GeV**-2)
C                   DELTA    effective pomeron delta (intercept-1)
C                   ALPHAP   slope of pomeron trajectory (GeV**-2)
C                   GPPP     triple-Pomeron coupling
C                   BPPP     slope related to B0PPP (GeV**-2)
C                   VIR2A    virtuality of particle a (GeV**2)
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGTR    total triple-Pomeron cross section
C                   BTR      effective triple-Pomeron slope
C                            (differs from diffractive slope!)
C
C     uses E_i (Exponential-Integral function)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO=0.D0,
     &           EPS =0.0001D0)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
C  integration cut-off Sigma_U ( see Nucl.Phys.B97(1975)493 )
      SIGU = 2.5
C  integration cut-off Sigma_L (min. squared mass of diff. blob)
      SIGL = 5.+VIR2A
C  debug output
      IF(IDEB(50).GE.10) THEN
        WRITE(6,'(/1X,A,/1X,9E10.3)')
     &       'TRIREG:DEBUG: S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP ',
     &       S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP
      ENDIF
C
      IF(S.LT.5.D0) THEN
        SIGTR = ZERO
        BTR = BPPP+BB
        RETURN
      ENDIF
C  change units of ALPHAP to mb
      ALSCA  = ALPHAP*GEV2MB
C
C  cross section
      PART1=GA*GB**2*GPPP/(16.*PI*2.*ALSCA)*S**DELTA*
     &        EXP(-(BB+BPPP)/(2.*ALPHAP)*DELTA)
      PART2=DEXPI(((BB+BPPP)/(2.*ALPHAP)+LOG(S/SIGL))*DELTA)
      PART3=DEXPI(((BB+BPPP)/(2.*ALPHAP)+LOG(SIGU))*DELTA)
C
      SIGTR=PART1*(PART2-PART3)
C
C  slope
      PART1 = (BB+BPPP+2.*ALPHAP*LOG(S/SIGL))/
     &        (BB+BPPP+2.*ALPHAP*LOG(SIGU))
      PART2 = LOG(PART1)
      BTR = (AA+BB)/2.+BPPP+ALPHAP*LOG(S)
     &      -0.5*ALPHAP*LOG(1.D0+S/(SIGU*SIGL))/PART2
C
      IF(SIGTR.LT.EPS) SIGTR = ZERO
      IF(BTR.LT.BB)  BTR = BB
      IF(IDEB(50).GE.7) THEN
        WRITE(6,'(/1X,A,3E12.3)') 'TRIREG:DEBUG: ENERGY,SIGTR,BTR ',
     &       SQRT(S),SIGTR,BTR
      ENDIF
      END
C
C
      SUBROUTINE LOOREG(S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP,
     &                  VIR2A,VIR2B,SIGLO,BLO)
C**********************************************************************
C
C     calculation of loop-Pomeron total cross section
C     according to Gribovs Regge theory
C
C     input:        S        squared cms energy
C                   GA       coupling constant to diffractive line
C                   AA       slope related to GA (GeV**-2)
C                   GB       coupling constant to elastic line
C                   BB       slope related to GB (GeV**-2)
C                   DELTA    effective pomeron delta (intercept-1)
C                   ALPHAP   slope of pomeron trajectory (GeV**-2)
C                   GPPP     triple-Pomeron coupling
C                   BPPP     slope related to B0PPP (GeV**-2)
C                   VIR2A    virtuality of particle a (GeV**2)
C                   VIR2B    virtuality of particle b (GeV**2)
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGLO    total loop-Pomeron cross section
C                   BLO      effective loop-Pomeron slope
C                            (differs from double diffractive slope!)
C
C     uses E_i (Exponential-Integral function)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO=0.D0,
     &           EPS =0.0001D0)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
C  integration cut-off Sigma_U ( see Nucl.Phys.B97(1975)493 )
      SIGU = 2.5
C  integration cut-off Sigma_L (min. squared mass of diff. blob)
      SIGL = 5.+VIR2A+VIR2B
C  debug output
      IF(IDEB(51).GE.10) THEN
        WRITE(6,'(/1X,A,/1X,9E10.3)')
     &       'LOOREG:DEBUG: S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP ',
     &       S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP
      ENDIF
C
      IF(S.LT.5.D0) THEN
        SIGLO = ZERO
        BLO = 2.D0*BPPP
        RETURN
      ENDIF
        
C
C  change units of ALPHAP to mb
      ALSCA  = ALPHAP*GEV2MB
C
C  cross section
      PART1=GA*GB*GPPP**2/(16.*PI*2.*ALSCA)*S**DELTA*
     &        EXP(-DELTA*BPPP/ALPHAP)
      PARTA=BPPP/ALPHAP+LOG(S/SIGL**2)
      PARTB=BPPP/ALPHAP+LOG(SIGU)
      SIGLO=PART1*( PARTA*(DEXPI(PARTA*DELTA)-DEXPI(PARTB*DELTA))
     &             +EXP(PARTA*DELTA)/DELTA-EXP(PARTB*DELTA)/DELTA
     &            )
C
C  slope
      PART1 = LOG(ABS(PARTA/PARTB))
     &       *(PARTA-LOG(1.D0+S/(SIGL**2*SIGU)))
      BLO = (AA+BB)/2.+2.*BPPP+ALPHAP*LOG(S)
     &      -0.25*ALPHAP*LOG(1.D0+S/(SIGU*SIGL))**2/PART1
C
      IF(SIGLO.LT.EPS) SIGLO = ZERO
      IF(BLO.LT.2.D0*BPPP) BLO = 2.D0*BPPP
      IF(IDEB(51).GE.7) THEN
        WRITE(6,'(/1X,A,3E12.3)') 'LOOREG:DEBUG: ENERGY,SIGLO,BLO ',
     &       SQRT(S),SIGLO,BLO
      ENDIF
      END
C
C
      SUBROUTINE TRXREG(S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP,SIGDP,BDP)
C**********************************************************************
C
C     calculation of total cross section of two tripe-Pomeron
C     graphs in X configuration according to Gribovs Reggeon field theory
C
C     input:        S        squared cms energy
C                   GA       coupling constant to diffractive line
C                   AA       slope related to GA (GeV**-2)
C                   GB       coupling constant to elastic line
C                   BB       slope related to GB (GeV**-2)
C                   DELTA    effective pomeron delta (intercept-1)
C                   ALPHAP   slope of pomeron trajectory (GeV**-2)
C                   BPPP     triple-Pomeron coupling
C                   BTR      slope related to B0PPP (GeV**-2)
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGDP    total cross section for double-Pomeron
C                            scattering
C                   BDP      effective double-Pomeron slope
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO=0.D0,
     &           EPS =0.0001D0)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
      DIMENSION XWGH1(96),XWGH2(96),XPOS1(96),XPOS2(96)
C
C  integration cut-off Sigma_U
      SIGU = 2.5
C  integration cut-off Sigma_L
      SIGL = 2.
C  integration precision
      NGAUS1 = 8
      NGAUS2 = 8
C
C  debug output
      IF(IDEB(52).GE.10) THEN
        WRITE(6,'(/1X,A,/1X,9E10.3)')
     &       'TRXREG:DEBUG: S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP ',
     &       S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP
      ENDIF
C
      IF(S.LT.5.D0) THEN
        SIGDP = ZERO
        BDP = AA+BB
        RETURN
      ENDIF
C
C  cross section
      XIL = LOG(SIGL)
      XIU = LOG(SIGU)
      XI = LOG(S)
      FAC = GPPP**2*GA**2*GB**2/(256.D0*PI2)*EXP(DELTA*XI)
      XSUM = 0.D0
      IF(XI-XIL-XIU.GT.ZERO) THEN
        CALL PHGSET(XIU,XI-XIL-XIU,NGAUS1,XPOS1,XWGH1)
        DO 100 I1=1,NGAUS1
          XI1 = XPOS1(I1)
          BDP1 = BPPP+AA+2.D0*ALPHAP*XI1
          SUM2 = 0.D0
          IF(XI-XIL-XI1.GT.0.D0) THEN
            CALL PHGSET(XIU,XI-XIL-XI1,NGAUS2,XPOS2,XWGH2)
            DO 200 I2=1,NGAUS2
              XI2 = XPOS2(I2)
              BDP2 = BPPP+BB+2.D0*ALPHAP*XI2
              SUM2 = SUM2+EXP(DELTA*(XI1+XI2))/BDP2*XWGH2(I2)
 200        CONTINUE
          ENDIF
          XSUM = XSUM + SUM2/BDP1*XWGH1(I1)
 100    CONTINUE
        XSUM = XSUM*FAC/GEV2MB**2
      ENDIF
      SIGDP = XSUM
C
C  slope
      BDP = 0.5D0*(AA+BB+2.D0*ALPHAP*(XI+XIU)*0.5D0)
C
      IF(IDEB(52).GE.7) THEN
        WRITE(6,'(/1X,A,3E12.3)') 'TRXREG:DEBUG: ENERGY,SIGDP,BDP ',
     &       SQRT(S),SIGDP,BDP
      ENDIF
      END
C
C
      SUBROUTINE TRXPOM(S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP,SIGDP,BDP)
C**********************************************************************
C
C     calculation of total cross section of two tripe-Pomeron
C     graphs in X configuration according to Gribovs Reggeon field theory
C
C     input:        S        squared cms energy
C                   GA       coupling constant to elastic line 1
C                   AA       slope related to GA (GeV**-2)
C                   GB       coupling constant to elastic line 2
C                   BB       slope related to GB (GeV**-2)
C                   DELTA    effective pomeron delta (intercept-1)
C                   ALPHAP   slope of pomeron trajectory (GeV**-2)
C                   BPPP     triple-Pomeron coupling
C                   BTR      slope related to B0PPP (GeV**-2)
C                   note: units of all coupling constants are mb**1/2
C
C     output:       SIGDP    total cross section for double-Pomeron
C                            scattering
C                   BDP      effective double-Pomeron slope
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO=0.D0,
     &           EPS =0.0001D0)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
      DIMENSION XWGH1(96),XPOS1(96)
C
C  lower integration cut-off Sigma_L
      SIGL = PARMDL(71)**2
C  upper integration cut-off Sigma_U
      C = 1.D0-1.D0/PARMDL(70)**2
      C = MAX(PARMDL(72),C)
      SIGU = (1.D0-C)**2*S
C  integration precision
      NGAUS1=16
C
C  debug output
      IF(IDEB(52).GE.10) THEN
        WRITE(6,'(/1X,A,/1X,9E10.3)')
     &       'TRXPOM:DEBUG:S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP',
     &       S,GA,AA,GB,BB,DELTA,ALPHAP,GPPP,BPPP
      ENDIF
C
      IF(SIGU.LE.SIGL) THEN
        SIGDP = ZERO
        BDP = AA+BB
        RETURN
      ENDIF
C
C  cross section
C
      XIL = LOG(SIGL)
      XIU = LOG(SIGU)
      XI = LOG(S)
      FAC = (GPPP*GA*GB)**2/(256.D0*PI2)/ALPHAP/GEV2MB**2
      ALPHA2 = 2.D0*ALPHAP
      ALOC = LOG(1.D0/(1.D0-C))
      CALL PHGSET(XIL,XIU,NGAUS1,XPOS1,XWGH1)
      XSUM = 0.D0
      DO 100 I1=1,NGAUS1
        AMXSQ  = EXP(XPOS1(I1))
        ALOSMX = LOG(S/AMXSQ)
        ALCSMX = LOG((1.D0-C)*S/AMXSQ)
        W = LOG((AA+BPPP+ALPHA2*ALCSMX)/(BB+BPPP+ALPHA2*ALOC))
        W = MAX(0.D0,W)
        WN=(AA+BB+2.D0*BPPP+ALPHA2*ALOSMX)
C  supercritical part
        WSC = AMXSQ**DELTA*(S/AMXSQ)**(2.D0*DELTA)
        XSUM = XSUM + W*XWGH1(I1)/WN*WSC
 100  CONTINUE
      SIGDP = XSUM*FAC
C
C  slope
      BDP = 0.5*(AA+BB+BPPP+ALPHAP*XI)
C
      IF(IDEB(52).GE.7) THEN
        WRITE(6,'(/1X,A,3E12.3)') 'TRXPOM:DEBUG:ENERGY,SIGDP,BDP',
     &       SQRT(S),SIGDP,BDP
      ENDIF
      END
C
C
      SUBROUTINE CHAN2A(BB)
C***********************************************************************
C
C     simple two channel model to realize low mass diffraction
C     (version A, iteration of triple- and loop-Pomeron)
C
C     input:     BB      impact parameter (mb**1/2)
C
C     output:    common /AMPLIT/
C                AMPEL      elastic amplitude
C                AMPVM(4,4) q-elastic VM production
C                AMLMSD(2)  low mass single diffraction amplitude
C                AMHMSD(2)  high mass single diffraction amplitude
C                AMLMDD     low mass double diffraction amplitude
C                AMHMDD     high mass double diffraction amplitude
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (GEV2MB=0.38935)
      PARAMETER (PI=3.141592654,
     &           DEPS=1.D-5,
     &           ZERO=0.D0,
     &           TWO=2.D0,
     &           FOUR=4.D0,
     &           EIGHT=8.D0,
     &           ONE=1.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
C  eikonal functions and slopes
      PARAMETER(MDIM=5)
      COMMON /ZXEIKS/ XM(MDIM,MDIM),XOUT(MDIM,MDIM),
     &                XIN(MDIM),XOU(MDIM),
     &                ZXP(MDIM,MDIM),BXP(MDIM,MDIM),
     &                ZXR(MDIM,MDIM),BXR(MDIM,MDIM),
     &                ZXH(MDIM,MDIM),BXH(MDIM,MDIM),
     &                ZXD(MDIM,MDIM),BXD(MDIM,MDIM),
     &                ZXT1A(MDIM,MDIM),BXT1A(MDIM,MDIM),
     &                ZXT1B(MDIM,MDIM),BXT1B(MDIM,MDIM),
     &                ZXT2A(MDIM,MDIM),BXT2A(MDIM,MDIM),
     &                ZXT2B(MDIM,MDIM),BXT2B(MDIM,MDIM),
     &                ZXL(MDIM,MDIM),BXL(MDIM,MDIM),
     &                ZXDP(MDIM,MDIM),BXDP(MDIM,MDIM),
     &                ZXD1A(MDIM,MDIM),BXD1A(MDIM,MDIM),
     &                ZXD1B(MDIM,MDIM),BXD1B(MDIM,MDIM),
     &                ZXD2A(MDIM,MDIM),BXD2A(MDIM,MDIM),
     &                ZXD2B(MDIM,MDIM),BXD2B(MDIM,MDIM)
C
C  output common
      COMPLEX*16      AMPEL,AMPVM,AMPSOF,AMPHAR,AMLMSD,AMHMSD,AMLMDD,
     &                AMHMDD,AMPDP,AMPD1,AMPD2
      COMMON /AMPLIT/ AMPEL,AMPVM(4,4),AMPSOF,AMPHAR,AMLMSD(2),
     &                AMHMSD(2),AMLMDD,AMHMDD,AMPDP,AMPD1,AMPD2
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
C  local variables
      DIMENSION  AB(6,4),CHI(4),CHDS(4),CHDH(4),CHDA(4),CHDB(4),
     &           CHDD(4),CHDP(4),
     &           AMPCHA(4),EX1CHI(4),EX2CHI(4),ABSUM(4),AMPELA(4,7),
     &           ELAFAC(4),AMPFAC(4)
      DIMENSION CHIFAC(4,4),EXPFAC(4,4),IELTAB(4,4)
C  combinatorical factors
      DATA      CHIFAC / 1.D0, 1.D0,-1.D0,-1.D0,
     &                   1.D0,-1.D0, 1.D0,-1.D0,
     &                   1.D0,-1.D0,-1.D0, 1.D0,
     &                   1.D0, 1.D0, 1.D0, 1.D0 /
      DATA      EXPFAC / 1.D0, 1.D0, 1.D0, 1.D0,
     &                   1.D0,-1.D0,-1.D0, 1.D0,
     &                  -1.D0, 1.D0,-1.D0, 1.D0,
     &                  -1.D0,-1.D0, 1.D0, 1.D0 /
      DATA      IELTAB / 1, 2, 3, 4,
     &                   2, 1, 4, 3,
     &                   3, 4, 1, 2,
     &                   4, 3, 2, 1 /
C
      IF(IDEB(86).GE.20) THEN
        WRITE(6,'(1X,A,E12.3)') 'CHAN2A:DEBUG:BB ',BB
      ENDIF
      B24 = BB**2/FOUR
C  VDM factors
      AMPFAC(1) = SQRT(VDMQ2F(1)*VDMQ2F(3))
      AMPFAC(2) = SQRT(VDMQ2F(2)*VDMQ2F(3))
      AMPFAC(3) = SQRT(VDMQ2F(1)*VDMQ2F(4))
      AMPFAC(4) = SQRT(VDMQ2F(2)*VDMQ2F(4))
      ELAFAC(1) = (VDMQ2F(1)*VDMQ2F(3)+VDMQ2F(2)*VDMQ2F(3)
     &             +VDMQ2F(1)*VDMQ2F(4)+VDMQ2F(2)*VDMQ2F(4))/EIGHT
      ELAFAC(2) = (AMPFAC(1)*AMPFAC(2)+AMPFAC(3)*AMPFAC(4))/FOUR
      ELAFAC(3) = (AMPFAC(1)*AMPFAC(3)+AMPFAC(2)*AMPFAC(4))/FOUR
      ELAFAC(4) = AMPFAC(1)*AMPFAC(4)/TWO
C
      DO 25 I=1,4
        AB(1,I)=ZXP(1,I)*EXP(-B24/BXP(1,I))
     &          +ZXR(1,I)*EXP(-B24/BXR(1,I))
        AB(2,I)=ZXH(1,I)*EXP(-B24/BXH(1,I))
        AB(3,I)=-ZXT1A(1,I)*EXP(-B24/BXT1A(1,I))
        AB(4,I)=-ZXT2A(1,I)*EXP(-B24/BXT2A(1,I))
        AB(5,I)=-ZXL(1,I)*EXP(-B24/BXL(1,I))
     &          -ZXT1B(1,I)*EXP(-B24/BXT1B(1,I))
     &          -ZXT2B(1,I)*EXP(-B24/BXT2B(1,I))
        AB(6,I)=ZXDP(1,I)*EXP(-B24/BXDP(1,I))
 25   CONTINUE
C
      DO 50 I=1,4
        ABSUM(I)  = ZERO
        DO 75 II=6,1,-1
          ABSUM(I) = ABSUM(I) + AB(II,I)
 75     CONTINUE
 50   CONTINUE
      IF(IDEB(86).GE.20) THEN
        WRITE(6,'(1X,A,4E12.3)') 'CHAN2A:DEBUG:ABSUM',ABSUM
      ENDIF
      DO 100 I=1,4
        CHI(I)  = ZERO
        CHDS(I) = ZERO
        CHDH(I) = ZERO
        CHDA(I) = ZERO
        CHDB(I) = ZERO
        CHDD(I) = ZERO
        CHDP(I) = ZERO
        AMPELA(I,5) = ZERO
        AMPELA(I,6) = ZERO
        AMPELA(I,7) = ZERO
        DO 200 K=1,4
          AMPELA(I,K) = ZERO
          AMPVM(I,K)  = ZERO
          CHI(I)  = CHI(I)  + CHIFAC(K,I)*ABSUM(K)
          CHDS(I) = CHDS(I) + CHIFAC(K,I)*AB(1,K)
          CHDH(I) = CHDH(I) + CHIFAC(K,I)*AB(2,K)
          CHDA(I) = CHDA(I) + CHIFAC(K,I)*AB(3,K)
          CHDB(I) = CHDB(I) + CHIFAC(K,I)*AB(4,K)
          CHDD(I) = CHDD(I) + CHIFAC(K,I)*AB(5,K)
          CHDP(I) = CHDP(I) + CHIFAC(K,I)*AB(6,K)
 200    CONTINUE
        IF(CHI(I).LT.-DEPS) THEN
          IF(IDEB(86).GE.0) THEN
            WRITE(6,'(1X,A,I3,2E12.3)')
     &        'CHAN2A:WARNING: NEG.EIGENVALUE:(I,B,CHI)',
     &        I,BB,CHI(I)
            WRITE(6,'(5X,A,5E12.3)') 'E,CHIs:',ECM,(ABSUM(K),K=1,4)
          ENDIF
        ENDIF
        IF(ABS(CHI(I)).GT.200.D0) THEN
          EX1CHI(I) = ZERO
          EX2CHI(I) = ZERO
        ELSE
          TMP       = EXP(-CHI(I))
          EX1CHI(I) = TMP
          EX2CHI(I) = TMP*TMP
        ENDIF
 100  CONTINUE
      IF(IDEB(86).GE.20) THEN
        WRITE(6,'(1X,A,4E12.3)') 'CHAN2A:DEBUG:EX1CHI',EX1CHI
      ENDIF
C
      AMPELA(1,1) = FOUR
      DO 300 K=1,4
        DO 400 J=1,4
          AMPELA(K,1) = AMPELA(K,1) - EXPFAC(J,K)*EX1CHI(J)
          AMPELA(K,2) = AMPELA(K,2)
     &      + EXPFAC(J,K)*TWO*CHDS(J)*EX2CHI(J)
          AMPELA(K,3) = AMPELA(K,3)
     &      + EXPFAC(J,K)*TWO*CHDH(J)*EX2CHI(J)
          AMPELA(K,4) = AMPELA(K,4)
     &      + EXPFAC(J,K)*(-TWO*(CHDA(J)+2.D0*CHDP(J)))*EX2CHI(J)
          AMPELA(K,5) = AMPELA(K,5)
     &      + EXPFAC(J,K)*(-TWO*(CHDB(J)+2.D0*CHDP(J)))*EX2CHI(J)
          AMPELA(K,6) = AMPELA(K,6)
     &      + EXPFAC(J,K)*(-TWO*CHDD(J))*EX2CHI(J)
          AMPELA(K,7) = AMPELA(K,7)
     &      + EXPFAC(J,K)*TWO*CHDP(J)*EX2CHI(J)
 400    CONTINUE
 300  CONTINUE
C
      IF(IDEB(86).GE.25) THEN
        DO 305 I=1,7
          WRITE(6,'(1X,A,I3,4E10.3)') 'CHAN2A:DEBUG:AMPELA(1-4,I)',I,
     &      (AMPELA(K,1),K=1,4)
 305    CONTINUE
      ENDIF
C
C  VDM factors --> amplitudes
C  low mass excitations
      DO 500 I=1,4
        AMPCHA(I) = ZERO
        DO 600 K=1,4
          AMPCHA(I) = AMPCHA(I) + AMPFAC(K)*AMPELA(IELTAB(K,I),1)
 600    CONTINUE
 500  CONTINUE
      AMPVME    = AMPCHA(1)/EIGHT
      AMLMSD(1) = AMPCHA(2)/EIGHT
      AMLMSD(2) = AMPCHA(3)/EIGHT
      AMLMDD    = AMPCHA(4)/EIGHT
C  elastic part, high mass diffraction
      AMPEL = 0.5D0*ZXD(1,1)*EXP(-B24/BXD(1,1))
      AMPSOF    = ZERO
      AMPHAR    = ZERO
      AMHMSD(1) = ZERO
      AMHMSD(2) = ZERO
      AMHMDD    = ZERO
      AMPDP     = ZERO
      DO 450 I=1,4
        AMPEL     = AMPEL     + ELAFAC(I)*AMPELA(I,1)
        AMPSOF    = AMPSOF    + ELAFAC(I)*AMPELA(I,2)
        AMPHAR    = AMPHAR    + ELAFAC(I)*AMPELA(I,3)
        AMHMSD(1) = AMHMSD(1) + ELAFAC(I)*AMPELA(I,4)
        AMHMSD(2) = AMHMSD(2) + ELAFAC(I)*AMPELA(I,5)
        AMHMDD    = AMHMDD    + ELAFAC(I)*AMPELA(I,6)
        AMPDP     = AMPDP     + ELAFAC(I)*AMPELA(I,7)
 450  CONTINUE
      AMPSOF    = AMPSOF/TWO
      AMPHAR    = AMPHAR/TWO
      AMHMSD(1) = AMHMSD(1)/TWO
      IF(DREAL(AMHMSD(1)).LE.ZERO) AMHMSD(1) = ZERO
      AMHMSD(2) = AMHMSD(2)/TWO
      IF(DREAL(AMHMSD(2)).LE.ZERO) AMHMSD(2) = ZERO
      AMHMDD    = AMHMDD/TWO
      IF(DREAL(AMHMDD).LE.ZERO) AMHMDD = ZERO
      AMPDP     = AMPDP/TWO
      IF(DREAL(AMPDP).LE.ZERO) AMPDP = ZERO
C  vector-meson production, weight factors
      IF((IFPAP(1).EQ.22).OR.(IFPAP(2).EQ.22)) THEN
        IF(IFPAP(1).EQ.22) THEN
          IF(IFPAP(2).EQ.22) THEN
            DO 10 I=1,4
              DO 15 J=1,4
                AMPVM(I,J) = PARMDL(9+I)*PARMDL(9+J)*AMPVME
 15           CONTINUE
 10         CONTINUE
          ELSE
            AMPVM(1,1) = PARMDL(10)*AMPVME
            AMPVM(2,1) = PARMDL(11)*AMPVME
            AMPVM(3,1) = PARMDL(12)*AMPVME
            AMPVM(4,1) = PARMDL(13)*AMPVME
          ENDIF
        ELSE IF(IFPAP(2).EQ.22) THEN
          AMPVM(1,1) = PARMDL(10)*AMPVME
          AMPVM(1,2) = PARMDL(11)*AMPVME
          AMPVM(1,3) = PARMDL(12)*AMPVME
          AMPVM(1,4) = PARMDL(13)*AMPVME
        ENDIF
      ENDIF
C  debug output
      IF(IDEB(86).GE.5) THEN
        WRITE(6,'(/,1X,A)') 'CHAN2A:DEBUG:IMPACT PARAMETER AMPLITUDES:'
        WRITE(6,'(1X,A,2E12.3)') '       AMPEL',AMPEL
        WRITE(6,'(1X,A,8E10.3)') 'AMPVM(1,1-4)',(AMPVM(1,K),K=1,4)
        WRITE(6,'(1X,A,8E10.3)') 'AMPVM(2,1-4)',(AMPVM(2,K),K=1,4)
        WRITE(6,'(1X,A,8E10.3)') 'AMPVM(3,1-4)',(AMPVM(3,K),K=1,4)
        WRITE(6,'(1X,A,8E10.3)') 'AMPVM(4,1-4)',(AMPVM(4,K),K=1,4)
        WRITE(6,'(1X,A,4E12.3)') '  AMPSOF/HAR',AMPSOF,AMPHAR
        WRITE(6,'(1X,A,4E12.3)') '      AMLMSD',AMLMSD
        WRITE(6,'(1X,A,4E12.3)') '      AMHMSD',AMHMSD
        WRITE(6,'(1X,A,2E12.3)') '      AMLMDD',AMLMDD
        WRITE(6,'(1X,A,2E12.3)') '      AMHMDD',AMHMDD
        WRITE(6,'(1X,A,2E12.3)') '       AMPDP',AMPDP
      ENDIF
      END
      SUBROUTINE PHOFIT
C**********************************************************************
C
C     fit program to adjust free parameters of the model
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16      SIGP,SIGR,SIGHD,SIGHR,SIGT1,SIGT2,SIGL,SIGDP,
     &                SIGD1,SIGD2,DSIGH
      COMMON /SIGMAS/ SIGP,SIGR,SIGHD,SIGHR,SIGT1(2),SIGT2(2),SIGL,
     &                SIGDP(4),SIGD1(2),SIGD2(2),DSIGH(0:MAXPRO)
C
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER(NECM=200,NPAR=30)
      CHARACTER*6 HINT
      COMMON /FITMOD/ IFMOD,IFHARD
      COMMON /FITTYP/IFIT,NIST,ECMS(NECM),XTOT(NECM),XTOTE(NECM),
     &               XELA(NECM),XELAE(NECM),XINE(NECM),
     &               XINEE(NECM),XSDI(NECM),XSDIE(NECM),
     &               SLO(NECM),SLOE(NECM),XPRO(3,NECM),XPROE(3,NECM),
     &               XI(NPAR),XINC(NPAR),NFITP,HINT(NPAR),IFITF(NPAR)
      COMMON /CONTRL/XSTEP,NFUN
C
      PARAMETER(THOUS=1000,
     &          IZERO=0,
     &          ZERO =0.D0,
     &          ONE  =1.D0,
     &          IONE =1)
      PARAMETER(NDUM=50000)
      DIMENSION XVAR(NPAR),DXVAR(NPAR)
      DIMENSION WDUMMY(NDUM),YI(1000),SIGSDI(2)
C
C  external chi**2 determination (FCN)
      EXTERNAL AMPFIT,TESTF
C
      CHARACTER*10 CNAME
      CHARACTER*70 NUMBER
      CHARACTER*12 FILENA
C
      DATA ICALLF /0/
C  control output
      DATA IOPEN /0/
C  control output
      DATA IPRINT /1/
C  print covariance matrix
      DATA COV /1/
C  minimal increment
      DATA EPS /1.D-5/
C  minimization strategy
      DATA ISTRAT /1/
C
 14   FORMAT(A10,A70)
 15   FORMAT(A12)
      WRITE(6,*) ' ******** START OF PHOJET PARAMETER FIT *********'
      IF(ICALLF.EQ.0) CALL FITDAT
      ICALLF = ICALLF+1
      CALL FITPAR(0)
C
 1200 CONTINUE
        READ(5,14) CNAME,NUMBER
        IF(CNAME.EQ.'ENDFIT    ') GOTO 1300
        IF(CNAME.EQ.'SUSPENDFIT') GOTO 1400
        IF(CNAME.EQ.'COMMENT   ') THEN
          WRITE(6,*) ' COMMENT  ',NUMBER
        ELSE IF(CNAME.EQ.'INPUTFILE ') THEN
          READ(NUMBER,15) FILENA
          WRITE(6,*) '  Input File: ',FILENA
C  read cross section data cards
          CALL XINP(FILENA)
        ELSE IF(CNAME.EQ.'SUMMARY   ') THEN
          READ(NUMBER,15) FILENA
          WRITE(6,*) '  Output File: ',FILENA
C  open output file for summary
          IF(IOPEN.EQ.1) THEN
            CLOSE(10)
            IOPEN=0
          ENDIF
          OPEN(10,FILE=FILENA,STATUS='UNKNOWN',ERR=900)
          IOPEN=1
          WRITE(10,*) ' --------------- summary file ----------------'
        ELSE IF(CNAME.EQ.'NITER     ') THEN
          READ(NUMBER,*) NFUN
          WRITE(6,*) '  NFUN (NITER)   ',NFUN
        ELSE IF(CNAME.EQ.'IPRINT    ') THEN
          READ(NUMBER,*) IPRINT
          WRITE(6,*) '  IPRINT         ',IPRINT
        ELSE IF(CNAME.EQ.'FITMODE   ') THEN
          READ(NUMBER,*) IFMOD
          WRITE(6,*) '  FITMODE        ',IFMOD
        ELSE IF(CNAME.EQ.'STRATEGY  ') THEN
          READ(NUMBER,*) ISTRAT
          WRITE(6,*) '  STRATEGY       ',ISTRAT
        ELSE IF(CNAME.EQ.'NFITP     ') THEN
          READ(NUMBER,*) NFITP
          WRITE(6,*) '  NFITP (number of fit parameters) = ',NFITP
        ELSE IF(CNAME.EQ.'XINCREMENT') THEN
          READ(NUMBER,*) (XINC(IFITF(I)),I=1,NFITP)
          DO 110 I=1,NFITP
             WRITE(6,*) '  XINC(',IFITF(I),')=',XINC(IFITF(I))
             IF(XINC(IFITF(I)).LT.EPS) THEN
               WRITE(6,*) ' *** Error reading Xincrement ***'
     &                   ,' (value to small)'
               STOP
             ENDIF
 110      CONTINUE
        ELSE IF(CNAME.EQ.'XSTEP     ') THEN
          READ(NUMBER,*) XSTEP
          WRITE(6,*) '  XSTEP = ',XSTEP
        ELSE IF(CNAME.EQ.'PARAMETER') THEN
          WRITE(6,*) '  selected fit parameters:'
          READ(NUMBER,*) (IFITF(I),I=1,NFITP)
          DO 100 I=1,NFITP
             WRITE(6,*) '  IFITF(',I,')=',IFITF(I),
     &                  '   --> ',HINT(IFITF(I))
             IF((IFITF(I).LT.1).OR.(IFITF(I).GT.NPAR)) THEN
               WRITE(6,*)
     &          ' *** Error reading selected fit parameter ***'
               STOP
             ENDIF
 100      CONTINUE
C
C  perform simple fit using PHMINSQ of CERN library
        ELSE IF(CNAME.EQ.'AMPFIT    ') THEN
          WRITE(6,*)
     &    ' **** start of AMPFIT program ****'
          WRITE(10,*)
     &    ' **** start of AMPFIT program ****'
          IF(NFITP.LE.0) THEN
            WRITE(6,*) '  *** Error: select first fit parameter ***'
            STOP
          ENDIF
C  initial parameter settings
          CALL FITPAR(1)
          XI(1)=ALPOM
          XI(2)=ALPOMP
          XI(3)=GP(1)
          XI(4)=GP(2)
          XI(5)=B0POM(1)
          XI(6)=B0POM(2)
          XI(7)=ALREG
          XI(8)=ALREGP
          XI(9)=GR(1)
          XI(10)=B0REG(1)
          XI(11)=GPPP
          XI(12)=B0PPP
          XI(13)=VDMFAC(1)
          XI(14)=VDMFAC(2)
          XI(15)=VDMFAC(3)
          XI(16)=VDMFAC(4)
          XI(17)=B0HAR
          XI(18)=AKFAC
          XI(19)=PHISUP(1)
          XI(20)=PHISUP(2)
          XI(21)=RMASS1
          XI(22)=RMASS2
          XI(23)=GPPR
          XI(24)=B0PPR
          GR(2)    = GP(2)/GP(1)*GR(1)
          B0REG(2) = B0POM(2)/B0POM(1)*B0REG(1)
C  control length of auxiliary field
          NW=NFITP+IFIT*(NFITP+1)+(3*NFITP*(NFITP+1))/2+1
          IF(NW.GT.NDUM) THEN
            WRITE(6,*)
     &       ' PHOFIT: ERROR: size of aux. field WDUMMY to small'
            STOP
          ELSE
            NW=NDUM
          ENDIF
C  prepare parameter input field
          DO 120 I=1,NFITP
             XVAR(I)=XI(IFITF(I))
             DXVAR(I)=XINC(IFITF(I))
 120      CONTINUE
C  call to CERN fit subroutine
          WRITE(6,*) ' **** start of fit program ****'
          WRITE(10,*) ' **** start of fit program ****'
          CALL DATINP(10,1)
          CALL DATINP(6,1)
          CALL PHMINSQ(IFIT,NFITP,YI,XVAR,DXVAR,IPRINT,NFUN,NW,
     &                WDUMMY,COV,XSTEP,AMPFIT)
          IFHARD = 0
          CALL DATINP(10,0)
          CALL DATINP(6,0)
          WRITE(6,*) ' **** end of fit program ****'
          WRITE(10,*) ' **** end of fit program ****'
C
C  output of graphic tables, energy dependence
        ELSE IF(CNAME.EQ.'ECM-TABLE ') THEN
          READ(NUMBER,*) ELOWER,EUPPER,NTAB
          WRITE(6,*)
     &    ' **** begin of energy table ****'
          WRITE(10,*)
     &    ' **** begin of energy table ****'
          WRITE(6,*) '  E_min = ',ELOWER
          WRITE(6,*) '  E_max = ',EUPPER
          WRITE(6,*) '  Steps = ',NTAB
          CALL FITPAR(1)
          DELTA=(LOG(EUPPER)-LOG(ELOWER))/DBLE(NTAB)
          ETAB=LOG(ELOWER)-DELTA
          F = 1.D0
          IF(IFPAP(1).EQ.22) F = F*1000.D0
          IF(IFPAP(2).EQ.22) F = F*1000.D0
C  write out data
          WRITE(6,*)
     &' ECMS / W / SIGTOT/SIGEL/SIGIN / SIGSD,SIGSD(1,2),SIGDD / SLOPE'
          WRITE(10,*)
     &' ECMS / W / SIGTOT/SIGEL/SIGIN / SIGSD,SIGSD(1,2),SIGDD / SLOPE'
 18       FORMAT(10E11.4)
          DO 2000 I=1,NTAB+1
             ETAB=ETAB+DELTA
             EE=EXP(ETAB)
             ECM = EE
             CALL POXSEC(1,EE)
             SIGSDI(1) = SIGLSD(1)+SIGHSD(1)
             SIGSDI(2) = SIGLSD(2)+SIGHSD(2)
             SIGDDI = SIGLDD+SIGHDD
             WRITE(10,18) EE,EE*EE,SIGTOT*F,SIGELA*F,
     &         SIGINE*F,(SIGSDI(1)+SIGSDI(2))*F,
     &         SIGSDI(1)*F,SIGSDI(2)*F,SIGDDI*F,SLOEL
             WRITE(6,18) EE,EE*EE,SIGTOT*F,SIGELA*F,
     &         SIGINE*F,(SIGSDI(1)+SIGSDI(2))*F,
     &         SIGSDI(1)*F,SIGSDI(2)*F,SIGDDI*F,SLOEL
 2000     CONTINUE
          ETAB=LOG(ELOWER)-DELTA
          WRITE(6,*)
     &' ECMS / W / SIGEL / RHO / OMEGA / PHI / SIGCDD / LMSD / HMSD '
          WRITE(10,*)
     &' ECMS / W / SIGEL / RHO / OMEGA / PHI / SIGCDD / LMSD / HMSD '
 19       FORMAT(11E11.4)
          DO 2005 I=1,NTAB+1
             ETAB=ETAB+DELTA
             EE=EXP(ETAB)
             ECM = EE
             CALL POXSEC(1,EE)
             WRITE(10,19) EE,EE*EE,SIGELA*F,(SIGVM(K,0)*F,K=1,3),
     &         SIGCDF*F,SIGLSD(1)*F,SIGLSD(2)*F,SIGHSD(1)*F,
     &         SIGHSD(2)*F
             IF(IFPAP(1).EQ.22) THEN
               WRITE(6,19)  EXP(ETAB),SIGELA*F,(SIGVM(K,0)*F,K=1,3),
     &           SIGCDF*F,SIGLSD(1)*F,SIGLSD(2)*F,SIGHSD(1)*F,
     &           SIGHSD(2)*F
             ELSE IF(IFPAP(2).EQ.22) THEN
               WRITE(6,19) EE,EE*EE,SIGELA*F,(SIGVM(0,K)*F,K=1,3),
     &           SIGCDF*F,SIGLSD(1)*F,SIGLSD(2)*F,SIGHSD(1)*F,
     &           SIGHSD(2)*F
             ENDIF
 2005     CONTINUE
          WRITE(6,*) ' **** end of energy table ****'
          WRITE(10,*) ' **** end of energy table ****'
C
C  output of graphic tables, virtuality dependence
        ELSE IF(CNAME.EQ.'VIR-TABLE ') THEN
          READ(NUMBER,*) EE,ISIDE,VLOWER,VUPPER,NTAB
          VLOWER = ABS(VLOWER)
          VUPPER = ABS(VUPPER)
          WRITE(6,*)
     &    ' **** begin of virtuality table ****'
          WRITE(10,*)
     &    ' **** begin of virtuality table ****'
          WRITE(6,*) '  Energy   = ',EE
          WRITE(6,*) '  Side     = ',ISIDE
          WRITE(6,*) '  P**2_min = ',VLOWER
          WRITE(6,*) '  P**2_max = ',VUPPER
          WRITE(6,*) '  Steps    = ',NTAB
          WRITE(10,*) '  Energy   = ',ECM
          WRITE(10,*) '  Side     = ',ISIDE
          CALL FITPAR(1)
          DELTA=(LOG(VUPPER)-LOG(VLOWER))/DBLE(NTAB)
          VTAB=LOG(VLOWER)-DELTA
          ECM = EE
          F = 1.D0
          IF(IFPAP(1).EQ.22) F = F*1000.D0
          IF(IFPAP(2).EQ.22) F = F*1000.D0
C  write out data
          WRITE(6,*)
     &' P**2 / SIGTOT / SIGEL / SIGIN / SIGSD,SIGSD(1,2),SIGDD / SLOPE'
          WRITE(10,*)
     &' P**2 / SIGTOT / SIGEL / SIGIN / SIGSD,SIGSD(1,2),SIGDD / SLOPE'
          DO 2010 I=1,NTAB+1
             VTAB=VTAB+DELTA
             PVIRT(ISIDE)=SQRT(EXP(VTAB))
             CALL POXSEC(1,EE)
             SIGSDI(1) = SIGLSD(1)+SIGHSD(1)
             SIGSDI(2) = SIGLSD(2)+SIGHSD(2)
             SIGDDI = SIGLDD+SIGHDD
             WRITE(10,18) EXP(VTAB),SIGTOT*F,SIGELA*F,
     &         SIGINE*F,(SIGSDI(1)+SIGSDI(2))*F,
     &         SIGSDI(1)*F,SIGSDI(2)*F,SIGDDI*F,SLOEL
             WRITE(6,18)  EXP(VTAB),SIGTOT*F,SIGELA*F,
     &         SIGINE*F,(SIGSDI(1)+SIGSDI(2))*F,
     &         SIGSDI(1)*F,SIGSDI(2)*F,SIGDDI*F,SLOEL
 2010     CONTINUE
          VTAB=LOG(VLOWER)-DELTA
          WRITE(6,*)
     &' Table: P**2 / SIGEL / RHO / OMEGA / PHI / LMSD / HMSD '
          WRITE(10,*)
     &' Table: P**2 / SIGEL / RHO / OMEGA / PHI / LMSD1,2 / HMSD1,2 '
          DO 2015 I=1,NTAB+1
             VTAB=VTAB+DELTA
             PVIRT(ISIDE)=SQRT(EXP(VTAB))
             CALL POXSEC(1,EE)
             WRITE(10,19) EXP(VTAB),SIGELA*F,(SIGVM(K,0)*F,K=1,3),
     &         SIGLSD(1)*F,SIGLSD(2)*F,SIGHSD(1)*F,SIGHSD(2)*F
             IF(IFPAP(1).EQ.22) THEN
               WRITE(6,19)  EXP(VTAB),SIGELA*F,(SIGVM(K,0)*F,K=1,3),
     &           SIGLSD(1)*F,SIGLSD(2)*F,SIGHSD(1)*F,SIGHSD(2)*F
             ELSE IF(IFPAP(2).EQ.22) THEN
               WRITE(6,19)  EXP(VTAB),SIGELA*F,(SIGVM(0,K)*F,K=1,3),
     &           SIGLSD(1)*F,SIGLSD(2)*F,SIGHSD(1)*F,SIGHSD(2)*F
             ENDIF
 2015     CONTINUE
          WRITE(6,*) ' **** end of virtuality table ****'
          WRITE(10,*) ' **** end of virtuality table ****'
C
C  test run to produce output table of input cross sections
        ELSE IF(CNAME.EQ.'INP-TABLE ') THEN
 20       FORMAT(10E11.4)
          READ(NUMBER,*) ELOWER,EUPPER,NTAB
          WRITE(6,*)
     &    ' **** begin of input cross section table ****'
          WRITE(10,*)
     &    ' **** begin of input cross section table ****'
          CALL FITPAR(1)
          WRITE(6,*) '  E_min = ',ELOWER
          WRITE(6,*) '  E_max = ',EUPPER
          WRITE(6,*) '  Steps = ',NTAB
          DELTA=(LOG(EUPPER)-LOG(ELOWER))/DBLE(NTAB)
          ETAB=LOG(ELOWER)-DELTA
          FAC = (VDMQ2F(1)+VDMQ2F(2))*(VDMQ2F(3)+VDMQ2F(4))
          F = 1.D0
          IF(IFPAP(1).EQ.22) F = F*1000.D0
          IF(IFPAP(2).EQ.22) F = F*1000.D0
          FF = F*FAC
          PMASS1 = PMASS(1)
          IF(IFPAP(1).EQ.22) PMASS1 = 0.766D0
          PMASS2 = PMASS(2)
          IF(IFPAP(2).EQ.22) PMASS2 = 0.766D0
          WRITE(6,*)
     &      ' E, SIGP, SIGR, SIGHD, SIGHR, SIGT1, SIGT2, SIGL'
          WRITE(10,*)
     &      ' E, SIGP, SIGR, SIGHD, SIGHR, SIGT1, SIGT2, SIGL'
          DO 3000 I=1,NTAB+1
             ETAB = ETAB+DELTA
             ECM = EXP(ETAB)
             CALL INPCS(1,PMASS1,PMASS2,PMASS1,PMASS2)
             SIGHD=DSIGH(15)
             SIGHR=DSIGH(9)
             WRITE(6,20)
     &         EXP(ETAB),DREAL(SIGP)*FF,DREAL(SIGR)*FF,DREAL(SIGHD)*F,
     &         DREAL(SIGHR)*F,DREAL(SIGT1(1))*FF,DREAL(SIGT1(2))*FF,
     &         DREAL(SIGT2(1))*FF,DREAL(SIGT2(2))*FF,
     &         DREAL(SIGL)*FF
             WRITE(10,20)
     &         EXP(ETAB),DREAL(SIGP)*FF,DREAL(SIGR)*FF,DREAL(SIGHD)*F,
     &         DREAL(SIGHR)*F,DREAL(SIGT1(1))*FF,DREAL(SIGT1(2))*FF,
     &         DREAL(SIGT2(1))*FF,DREAL(SIGT2(2))*FF,
     &         DREAL(SIGL)*FF
 3000     CONTINUE
          WRITE(6,*)
     &    ' **** end of input cross section table ****'
          WRITE(10,*)
     &    ' **** end of input cross section table ****'
C  write out parameter table for file fitpar.dat
        ELSE IF(CNAME.EQ.'PAR-TABLE ') THEN
          CALL PARTAB(10)
          CALL PARTAB(6)
C
C  unknown data card:
        ELSE
          WRITE(6,*) ' PHOFIT: unknown data card, ENDFIT assumed'
          WRITE(6,*) '         last input: ',CNAME,NUMBER
          GOTO 1300
        ENDIF
      GOTO 1200
  900 CONTINUE
        WRITE(6,*) ' PHOFIT: ERROR: opening file ',FILENA,' ***'
        STOP
 1300 CONTINUE
      IF(IOPEN.EQ.1) THEN
C       CALL INPOUT(10)
        CLOSE(10)
      ENDIF
 1400 CONTINUE
      WRITE(6,*) ' ******** END OF PHOJET PARAMETER FIT *********'
      END
C
C
      SUBROUTINE XINP(FILENA)
C*********************************************************************
C
C     read cross section data to perform fit
C
C     input parameter: FILENA name of input file
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      CHARACTER*(*) FILENA
C
      PARAMETER(NECM=200,NPAR=30)
      CHARACTER*6 HINT
      COMMON /FITTYP/IFIT,NIST,ECMS(NECM),XTOT(NECM),XTOTE(NECM),
     &               XELA(NECM),XELAE(NECM),XINE(NECM),
     &               XINEE(NECM),XSDI(NECM),XSDIE(NECM),
     &               SLO(NECM),SLOE(NECM),XPRO(3,NECM),XPROE(3,NECM),
     &               XI(NPAR),XINC(NPAR),NFITP,HINT(NPAR),IFITF(NPAR)
      PARAMETER(MAXKK=20,MAXDS=20)
      COMMON /DSIGEL/ DSDT(MAXKK,3,MAXDS),DSIGI(3,MAXKK)
      PARAMETER(ZERO=0.D0,
     &          ONEM=-1.D0,
     &          IZERO=0,
     &          ONE  =1.D0,
     &          TWO  =2.D0,
     &          THREE=3.D0,
     &          FOUR =4.D0)
C
      CHARACTER*6 CNAME
      CHARACTER*76 NUMBER
C  initialization
      DO 200 I=1,NECM
         XTOT(I) = ONEM
         XELA(I) = ONEM
         XINE(I) = ONEM
         XSDI(I) = ONEM
         SLO(I)  = ONEM
         XPRO(1,I) = ONEM
         XPRO(2,I) = ONEM
         XPRO(3,I) = ONEM
 200  CONTINUE
      DO 205 I=1,MAXKK
         DSIGI(2,I)=ZERO
 205  CONTINUE
C  read input data cards
      OPEN(2,FILE=FILENA,STATUS='OLD',ERR=900)
 10   FORMAT(A6,A74)
C12   FORMAT(1X,A4,2X,9F10.3)
 12   FORMAT(1X,A6,2X,3F10.4)
      I  = IZERO
      K  = IZERO
      KK = IZERO
 100  CONTINUE
        READ(2,10) CNAME,NUMBER
        IF(CNAME.EQ.'STOP  ') GOTO 1000
        IF(CNAME.EQ.'ECMS  ') THEN
          IF(I.EQ.NECM) THEN
            WRITE(6,*)
     &      ' *** ERROR: input X sections excceed array size ***'
            WRITE(6,*)' following data cards ignored'
            GOTO 1000
          ENDIF
          I=I+1
          READ(NUMBER,*) ECMS(I)
          WRITE(6,*) 'ECMS  ',ECMS(I)
        ELSE IF(CNAME.EQ.'XTOT  ') THEN
          K=K+1
          READ(NUMBER,*) XTOT(I),XTOTE(I)
          WRITE(6,'(1X,A,2E15.3)') 'XTOT  ',XTOT(I),XTOTE(I)
        ELSE IF(CNAME.EQ.'XELA  ') THEN
          K=K+1
          READ(NUMBER,*) XELA(I),XELAE(I)
          WRITE(6,'(1X,A,2E15.3)') 'XELA  ',XELA(I),XELAE(I)
        ELSE IF(CNAME.EQ.'XINELA') THEN
          K=K+1
          READ(NUMBER,*) XINE(I),XINEE(I)
          WRITE(6,'(1X,A,2E15.3)') 'XINELA',XINE(I),XINEE(I)
        ELSE IF(CNAME.EQ.'XSDIFF') THEN
          K=K+1
          READ(NUMBER,*) XSDI(I),XSDIE(I)
          WRITE(6,'(1X,A,2E15.3)') 'XSDIFF',XSDI(I),XSDIE(I)
        ELSE IF(CNAME.EQ.'SLOPE ') THEN
          K=K+1
          READ(NUMBER,*) SLO(I),SLOE(I)
          WRITE(6,'(1X,A,2E15.3)') 'SLOPE ',SLO(I),SLOE(I)
        ELSE IF(CNAME.EQ.'DSDTGA') THEN
          KK=KK+1
          READ(NUMBER,*) EE,NDUM
          DSIGI(2,KK)=DBLE(NDUM)
          DSIGI(1,KK)=EE
          DSIGI(3,KK)=ONE
          WRITE(6,'(1X,A,E15.3,3X,I3)') 'DSDTGA ',EE,NDUM
          DO 105 II=1,NDUM
            READ(2,*) DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
            WRITE(6,'(6X,3E15.3)')
     &            DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
 105      CONTINUE
          K=K+NDUM
        ELSE IF(CNAME.EQ.'DSDTRH') THEN
          KK=KK+1
          READ(NUMBER,*) EE,NDUM
          DSIGI(2,KK)=DBLE(NDUM)
          DSIGI(1,KK)=EE
          DSIGI(3,KK)=TWO
          WRITE(6,'(1X,A,E15.3,3X,I3)') 'DSDTRH ',EE,NDUM
          DO 106 II=1,NDUM
            READ(2,*) DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
            WRITE(6,'(6X,3E15.3)')
     &            DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
 106      CONTINUE
          K=K+NDUM
        ELSE IF(CNAME.EQ.'DSDTOM') THEN
          KK=KK+1
          READ(NUMBER,*) EE,NDUM
          DSIGI(2,KK)=DBLE(NDUM)
          DSIGI(1,KK)=EE
          DSIGI(3,KK)=THREE
          WRITE(6,'(1X,A,E15.3,3X,I3)') 'DSDTOM ',EE,NDUM
          DO 107 II=1,NDUM
            READ(2,*) DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
            WRITE(6,'(6X,3E15.3)')
     &            DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
 107      CONTINUE
          K=K+NDUM
        ELSE IF(CNAME.EQ.'DSDTPH') THEN
          KK=KK+1
          READ(NUMBER,*) EE,NDUM
          DSIGI(2,KK)=DBLE(NDUM)
          DSIGI(1,KK)=EE
          DSIGI(3,KK)=FOUR
          WRITE(6,'(1X,A,E15.3,3X,I3)') 'DSDTPH ',EE,NDUM
          DO 108 II=1,NDUM
            READ(2,*) DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
            WRITE(6,'(6X,3E15.3)')
     &            DSDT(KK,1,II),DSDT(KK,2,II),DSDT(KK,3,II)
 108      CONTINUE
          K=K+NDUM
        ELSE IF(CNAME.EQ.'RHOPRO') THEN
          K=K+1
          READ(NUMBER,*) XPRO(1,I),XPROE(1,I)
          WRITE(6,'(1X,A,2E15.3)') 'RHOPRO',XPRO(1,I),XPROE(1,I)
        ELSE IF(CNAME.EQ.'OMEPRO') THEN
          K=K+1
          READ(NUMBER,*) XPRO(2,I),XPROE(2,I)
          WRITE(6,'(1X,A,2E15.3)') 'OMEPRO',XPRO(2,I),XPROE(2,I)
        ELSE IF(CNAME.EQ.'PHIPRO') THEN
          K=K+1
          READ(NUMBER,*) XPRO(3,I),XPROE(3,I)
          WRITE(6,'(1X,A,2E15.3)') 'PHIPRO',XPRO(3,I),XPROE(3,I)
        ELSE IF(CNAME.EQ.'COMM  ') THEN
          WRITE(6,'(1X,A6,A)') 'COMM  ',NUMBER
        ELSE
          WRITE(6,*) ' XINP: ERROR: unknown data card : ',CNAME
          STOP
        ENDIF
        GOTO 100
 1000 CONTINUE
C  set number of energy data items
      NIST=I
C  set number of total data items
      IFIT=K
      WRITE(6,'(2X,A)') ' ---------------------------------'
      WRITE(6,'(2X,A,I4)') ' Total number of entries to fit ',
     &                     IFIT
      CLOSE(2)
C  control data consistence
      IF(NIST.LE.0) THEN
        PRINT *,' *** Error: unexpected STOP, to few data cards ***'
        STOP
      ENDIF
      IF(IFIT.LT.NIST) THEN
        PRINT *,' *** Error: more energy then X section items ***'
        WRITE(6,*) ' energy items: ',NIST,
     &             ' cross sections: ',IFIT
        STOP
      ENDIF
      RETURN
C
 900  CONTINUE
      WRITE(6,*) ' XINP: ERROR: cannot open inputfile: ',
     &           FILENA,' ***'
C
      END
C
C
      SUBROUTINE DATINP(NOUT,IFLAG)
C*********************************************************************
C
C     print out all important data values
C
C     input parameter: NOUT   number of output unit
C                      IFLAG  flag to select output of energy table
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
      PARAMETER (NECM=200,NPAR=30)
      CHARACTER*6 HINT
      COMMON /FITTYP/IFIT,NIST,ECMS(NECM),XTOT(NECM),XTOTE(NECM),
     &               XELA(NECM),XELAE(NECM),XINE(NECM),
     &               XINEE(NECM),XSDI(NECM),XSDIE(NECM),
     &               SLO(NECM),SLOE(NECM),XPRO(3,NECM),XPROE(3,NECM),
     &               XI(NPAR),XINC(NPAR),NFITP,HINT(NPAR),IFITF(NPAR)
      PARAMETER(MAXKK=20,MAXDS=20)
      COMMON /DSIGEL/ DSDT(MAXKK,3,MAXDS),DSIGI(3,MAXKK)
      PARAMETER(IZERO =0,
     &          IONE  =1,
     &          ITWO  =2,
     &          ITHREE=3,
     &          IFOUR =4)
C
C  write out initialization
      WRITE(NOUT,*)
     &  ' -------------------------------------------------'
      IF(IFLAG.EQ.1) THEN
        WRITE(NOUT,*) ' Energy data entries :',NIST
        WRITE(NOUT,*) ' Total fit data items:',IFIT
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV ---- SIGMAtot (mub) -------- Error (mub) '
        DO 10 I=1,NIST
          IF(XTOT(I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XTOT(I),' +- ',XTOTE(I)
          ENDIF
 10     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV ---- SIGMAela (mub) --------- Error (mub) '
        DO 11 I=1,NIST
          IF(XELA(I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XELA(I),' +- ',XELAE(I)
          ENDIF
 11     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV --- SIGMAinela (mub) --------- Error (mub) '
        DO 12 I=1,NIST
          IF(XINE(I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XINE(I),' +- ',XINEE(I)
          ENDIF
 12     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV --- SIGMAsdiff (mub) --------- Error (mub) '
        DO 13 I=1,NIST
          IF(XSDI(I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XSDI(I),' +- ',XSDIE(I)
          ENDIF
 13     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV --- SLOPE (GeV**-2) ---------- Error (GeV**-2)'
        DO 16 I=1,NIST
          IF(SLO(I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),SLO(I),' +- ',SLOE(I)
          ENDIF
 16     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV --- SIGMA (RHO production) (mub) --- Error ---'
        DO 17 I=1,NIST
          IF(XPRO(1,I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XPRO(1,I),' +- ',XPROE(1,I)
          ENDIF
 17     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV -- SIGMA (OMEGA production) (mub) -- Error ---'
        DO 18 I=1,NIST
          IF(XPRO(2,I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XPRO(2,I),' +- ',XPROE(2,I)
          ENDIF
 18     CONTINUE
        WRITE(NOUT,*)
     &  ' --- Ecms/GeV --- SIGMA (PHI production) (mub) --- Error ---'
        DO 19 I=1,NIST
          IF(XPRO(3,I).GT.0.0) THEN
            WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            ECMS(I),XPRO(3,I),' +- ',XPROE(3,I)
          ENDIF
 19     CONTINUE
        DO 20 KK=1,MAXKK
          IF(INT(DSIGI(2,KK)).GT.IZERO) THEN
            IF(INT(DSIGI(3,KK)).EQ.IONE) THEN
              WRITE(NOUT,'(1X,A)') 'PHOTON PROTON ELASTIC SCATTERING'
            ELSE IF(INT(DSIGI(3,KK)).EQ.ITWO) THEN
              WRITE(NOUT,'(1X,A)') 'DIFFRACTIVE RHO PRODUCTION'
            ELSE IF(INT(DSIGI(3,KK)).EQ.ITHREE) THEN
              WRITE(NOUT,'(1X,A)') 'DIFFRACTIVE OMEGA PRODUCTION'
            ELSE IF(INT(DSIGI(3,KK)).EQ.IFOUR) THEN
              WRITE(NOUT,'(1X,A)') 'DIFFRACTIVE PHI PRODUCTION'
            ELSE
              WRITE(NOUT,'(1X,A)') 'DATINP:ERROR: unknown particle'
              STOP
            ENDIF
            WRITE(NOUT,'(3X,A,E15.3)') ' DSIG/DT  energy (GeV) ',
     &            DSIGI(1,KK)
            WRITE(NOUT,*)
     &  ' --- T (GeV**2)-- DSIGDT (mub/GeV**2) --- Error (mub/GeV**-2)'
            DO 21 II=1,INT(DSIGI(2,KK))
              WRITE(NOUT,'(2X,2E15.3,A,E15.3)')
     &            DSDT(KK,1,II),DSDT(KK,2,II),' +- ',DSDT(KK,3,II)
 21         CONTINUE
          ENDIF
 20     CONTINUE
      ENDIF
C
      WRITE(NOUT,'(1X,A)') 'DATINP:DEBUG:PARAMETER SET:'
      WRITE(NOUT,'(2(A,F7.3),2(A,2F9.3))')
     &  '  ALPOM:',ALPOM,' ALPOMP:',ALPOMP,' GP:',GP,' B0POM:',
     &  B0POM
      WRITE(NOUT,'(2(A,F7.3),2(A,2F9.3))')
     &  '  ALREG:',ALREG,' ALREGP:',ALREGP,' GR:',GR,' B0REG:',
     &  B0REG
      WRITE(NOUT,'(4(A,F8.3))')
     &  '  GPPP :',GPPP,' B0PPP:',B0PPP,' GPPR :',GPPR,' B0PPR:',B0PPR
      WRITE(NOUT,'(5X,A,4F8.5)') '  VDMFAC:',VDMFAC(1),VDMFAC(2),
     &                                         VDMFAC(3),VDMFAC(4)
      WRITE(NOUT,'(5X,A,F8.3)')  '   B0HAR:',B0HAR
      WRITE(NOUT,'(5X,A,F8.3)')  '   AKFAC:',AKFAC
      WRITE(NOUT,'(5X,A,2F8.3)')  '  PHISUP:',PHISUP
      WRITE(NOUT,'(5X,A,3F8.3)') '   RMASS:',RMASS1,RMASS2,VAR
C
      IF(IFLAG.EQ.1) THEN
        WRITE(NOUT,'(A)') 'FIT PARAMETERS: '
        DO 15 I=1,NFITP
           WRITE(NOUT,*) '  IFITF(',I,')=',IFITF(I),
     &                  '   --> ',HINT(IFITF(I))
 15     CONTINUE
        DO 25 I=1,NFITP
          WRITE(NOUT,*)
     &    '  XI(',IFITF(I),') = ',XI(IFITF(I)),
     &    '  XINC(',IFITF(I),') = ',XINC(IFITF(I))
 25     CONTINUE
      ENDIF
      WRITE(NOUT,*)
     &  ' -------------------------------------------------'
C
      END
C
C
      SUBROUTINE PARTAB(NOUT)
C*********************************************************************
C
C     print out all important data values
C
C     input parameter: NOUT   number of output unit
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      WRITE(NOUT,'(A)') '  **** PARAMETER TABLE ****'
      WRITE(NOUT,'(1X,A)') 'NEXTDATA'
      WRITE(NOUT,'(1X,I8,2X,A8,3I6)')
     &  IFPAP(1),PDFNAM(1),IGRP(1),ISET(1),IEXT(1)
      WRITE(NOUT,'(1X,I8,2X,A8,3I6)')
     &  IFPAP(2),PDFNAM(2),IGRP(2),ISET(2),IEXT(2)
      WRITE(NOUT,'(5X,6F8.3)') ALPOM,ALPOMP,GP,B0POM
      WRITE(NOUT,'(5X,6F8.3)') ALREG,ALREGP,GR,B0REG
      WRITE(NOUT,'(5X,4F8.3)') GPPP,B0PPP,GPPR,B0PPR
      WRITE(NOUT,'(5X,4F8.5)') VDMFAC
      WRITE(NOUT,'(5X,F8.3)') B0HAR
      WRITE(NOUT,'(5X,F8.3)') AKFAC
      WRITE(NOUT,'(5X,2F8.3)') PHISUP
      WRITE(NOUT,'(5X,3F8.3)') RMASS1,RMASS2,VAR
      END
C
C
      SUBROUTINE FCCN(N,GRAD,CHI2,X,IFLAG,FUTIL)
C*********************************************************************
C
C     calculate chi**2 function ( called by MINUIT )
C
C     input parameter: GRAD   optional gradient vector, not used
C                      N      dimension of argument vector
C                      X      field of argument values
C                      CHI2   function value
C                      IFLAG  flag to indicate status of call
C                             ( see CERNLIB D507 )
C
C     output parameter:
C                      CHI2   function value: chi**2
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      DIMENSION GRAD(*),X(*)
      EXTERNAL FUTIL
C
      PARAMETER (NECM=200,NPAR=30)
      PARAMETER (ZERO=0.D0,
     &           IZERO=0,
     &           IONE =1,
     &           ITWO =2,
     &          ITHREE=3,
     &           IFOUR=4)
      CHARACTER*6 HINT
      COMMON /FITMOD/ IFMOD,IFHARD
      COMMON /FITTYP/IFIT,NIST,ECMS(NECM),XTOT(NECM),XTOTE(NECM),
     &               XELA(NECM),XELAE(NECM),XINE(NECM),
     &               XINEE(NECM),XSDI(NECM),XSDIE(NECM),
     &               SLO(NECM),SLOE(NECM),XPRO(3,NECM),XPROE(3,NECM),
     &               XI(NPAR),XINC(NPAR),NFITP,HINT(NPAR),IFITF(NPAR)
C
      DIMENSION F(1000)
C
      CALL FUTIL(IFIT,N,F,X,IFLAG)
C
      CHI2 = 0.D0
      DO 100 I=1,IFIT
        CHI2 = CHI2 + F(I)**2
 100  CONTINUE
C
      END
C
C
      SUBROUTINE AMPFIT(M,N,F,X,IFLAG)
C*********************************************************************
C
C     calculate deviations ( called by PHMINSQ, CERNLIB )
C
C     input parameter: M      number of function values in field F
C                      N      dimension of argument vector
C                      F      field of function values
C                      X      field of argument values
C                      IFLAG  flag to indicate status of PHMINSQ
C                             ( see CERNLIB D507 )
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      DIMENSION F(M),X(N)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16      SIGP,SIGR,SIGHD,SIGHR,SIGT1,SIGT2,SIGL,SIGDP,
     &                SIGD1,SIGD2,DSIGH
      COMMON /SIGMAS/ SIGP,SIGR,SIGHD,SIGHR,SIGT1(2),SIGT2(2),SIGL,
     &                SIGDP(4),SIGD1(2),SIGD2(2),DSIGH(0:MAXPRO)
C
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER (NECM=200,NPAR=30)
      PARAMETER (ZERO=0.D0,
     &           IZERO=0,
     &           IONE =1,
     &           ITWO =2,
     &          ITHREE=3,
     &           IFOUR=4)
      CHARACTER*6 HINT
      COMMON /FITMOD/ IFMOD,IFHARD
      COMMON /FITTYP/IFIT,NIST,ECMS(NECM),XTOT(NECM),XTOTE(NECM),
     &               XELA(NECM),XELAE(NECM),XINE(NECM),
     &               XINEE(NECM),XSDI(NECM),XSDIE(NECM),
     &               SLO(NECM),SLOE(NECM),XPRO(3,NECM),XPROE(3,NECM),
     &               XI(NPAR),XINC(NPAR),NFITP,HINT(NPAR),IFITF(NPAR)
      PARAMETER(MAXKK=20,MAXDS=20)
      COMMON /DSIGEL/ DSDT(MAXKK,3,MAXDS),DSIGI(3,MAXKK)
C
      DIMENSION DSIGMC(0:MAXPRO,NECM),XTA(6,MAXDS),SIGSDI(2)
C
      IF(IDEB(62).GE.10)
     &  WRITE(6,*) ' ************* call to AMPFIT *****************'
C
C***  update common blocks ( copy fit parameter )
      DO 100 I=1,NFITP
         XI(IFITF(I))=X(I)
 100  CONTINUE
      ALPOM  =      XI(1)
      ALPOMP =      XI(2)
      GP(1)  =      XI(3)
      GP(2)  =      XI(4)
      B0POM(1)  =   ABS(XI(5))
      B0POM(2)  =   ABS(XI(6))
      ALREG  =      XI(7)
      ALREGP =      XI(8)
      GR(1)  =      XI(9)
      B0REG(1) =    ABS(XI(10))
      GPPP   =      ABS(XI(11))
      B0PPP  =      ABS(XI(12))
      VDMFAC(1) =   ABS(XI(13))
      VDMFAC(2) =   ABS(XI(14))
      VDMFAC(3) =   ABS(XI(15))
      VDMFAC(4) =   ABS(XI(16))
      B0HAR  =      ABS(XI(17))
      AKFAC  =      XI(18)
      PHISUP(1) =   XI(19)
      GR(2)     =   GP(2)/GP(1)*GR(1)
      B0REG(2)  =   B0POM(2)/B0POM(1)*B0REG(1)
C  FMSR
*     PHISUP(2) =   PHISUP(1)*SQRT(GP(1)/GP(2))
      PHISUP(2) =   XI(20)
      RMASS1 =      XI(21)
      RMASS2 =      XI(22)
      GPPR   =      ABS(XI(23))
      B0PPR  =      ABS(XI(24))
C  scattering of identical particles
      IF(ABS(IFPAP(1)).EQ.ABS(IFPAP(2))) THEN
        B0POM(2)  = B0POM(1)
        B0REG(2)  = B0REG(1)
        GP(2)     = GP(1)
        GR(2)     = GR(1)
        PHISUP(2) = PHISUP(1)
        RMASS2    = RMASS1
      ENDIF
      IF(IPAMDL(9).EQ.2) B0HAR = B0POM(1)+B0POM(2)
C  VDM couplings
      RHOM2 = 0.59D0
      IF(IFPAP(1).EQ.22) THEN
        VDMQ2F(1) = RHOM2/(RHOM2+PVIRT(1)**2)*VDMFAC(1)
        VDMQ2F(2) = RMASS1**2/(RMASS1**2+PVIRT(1)**2)*VDMFAC(2)
      ELSE
        VDMQ2F(1) = VDMFAC(1)
        VDMQ2F(2) = VDMFAC(2)
      ENDIF
      IF(IFPAP(2).EQ.22) THEN
        VDMQ2F(3) = RHOM2/(RHOM2+PVIRT(2)**2)*VDMFAC(3)
        VDMQ2F(4) = RMASS2**2/(RMASS2**2+PVIRT(2)**2)*VDMFAC(4)
      ELSE
        VDMQ2F(3) = VDMFAC(3)
        VDMQ2F(4) = VDMFAC(4)
      ENDIF
C
      IF(IDEB(62).GE.5) THEN
        WRITE(6,'(1X,A)') 'AMPFIT:DEBUG:PARAMETER SET:'
        WRITE(6,'(2(A,F7.3),2(A,2F9.3))')
     &    '  ALPOM:',ALPOM,' ALPOMP:',ALPOMP,' GP:',GP,'  B0POM:',
     &    B0POM
        WRITE(6,'(2(A,F7.3),2(A,2F9.3))')
     &    '  ALREG:',ALREG,' ALREGP:',ALREGP,' GR:',GR,'  B0REG:',
     &    B0REG
        WRITE(6,'(2(A,F8.3))')
     &    '  GPPP :',GPPP,' B0PPP:',B0PPP,' GPPR :',GPPR,' B0PPR:',B0PPR
        WRITE(6,'(A,4F8.5)') '  VDMFAC:',VDMFAC
        WRITE(6,'(A,4F8.5)') '  VDMQ2F:',VDMQ2F
        WRITE(6,'(5X,A,F8.3)')  '   B0HAR:',B0HAR
        WRITE(6,'(5X,A,F8.3)')  '   AKFAC:',AKFAC
        WRITE(6,'(5X,A,2F8.3)')  '  PHISUP:',PHISUP
        WRITE(6,'(5X,A,3F8.3)') '   RMASS:',RMASS1,RMASS2,VAR
      ENDIF
C
C  loop over all energies
      K=1
      DO 110 I=1,NIST
         ECM = ECMS(I)
         IF(IFMOD.EQ.0) THEN
C  calculate hard input cross section each time
           IFHARD = -1
           CALL POXSEC(1,ECMS(I))
         ELSE IF(IFMOD.EQ.IONE) THEN
C  calculate hard input cross section first time only
           IF(IFLAG.EQ.1) THEN
             IFHARD = 0
             CALL POXSEC(1,ECMS(I))
             DO 105 II=0,MAXPRO
               DSIGMC(II,I)=DSIGH(II)
 105         CONTINUE
           ELSE
             DO 120 II=0,MAXPRO
               DSIGH(II)=DSIGMC(II,I)
 120         CONTINUE
             IFHARD = 1
             CALL POXSEC(1,ECMS(I))
           ENDIF
         ELSE IF(IFMOD.EQ.2) THEN
           CALL TESTF
         ELSE
           WRITE(6,*) ' FCN: ERROR: unsupported fit mode ',IFMOD
           STOP
         ENDIF
         SIGSDI(1) = SIGLSD(1)+SIGHSD(1)
         SIGSDI(2) = SIGLSD(2)+SIGHSD(2)
C  total cross section
         IF(XTOT(I).GT.ZERO) THEN
           F(K)=(SIGTOT-XTOT(I))/XTOTE(I)
           K=K+1
         ENDIF
C  total elastic cross section
         IF(XELA(I).GT.ZERO) THEN
           F(K)=(SIGELA-XELA(I))/XELAE(I)
           K=K+1
         ENDIF
C  inelastic cross section
         IF(XINE(I).GT.ZERO) THEN
           F(K)=(SIGINE-XINE(I))/XINEE(I)
           K=K+1
         ENDIF
C  single diffractive cross section
         IF(XSDI(I).GT.ZERO) THEN
           F(K)=(SIGSDI(1)+SIGSDI(2)-XSDI(I))/XSDIE(I)
           K=K+1
         ENDIF
C  slope parameter
         IF(SLO(I).GT.ZERO) THEN
           F(K)=(SLOEL-SLO(I))/SLOE(I)
           K=K+1
         ENDIF
C  vector meson production cross section
         DO 10 KK=1,3
           IF(XPRO(KK,I).GT.ZERO) THEN
             F(K)=(SIGVM(KK,0)-XPRO(KK,I))/XPROE(KK,I)
             K=K+1
           ENDIF
 10      CONTINUE
C
C  last iteration / debug output: write out all results
       IF((IFLAG.EQ.3).OR.(IFLAG.EQ.1).OR.(IDEB(62).GE.20)) THEN
         II = 10
         IF(IDEB(62).GE.20) II = 6
         WRITE(II,*) ' --------------------------------------------'
         WRITE(II,'(1X,A,F10.2)') 'AMPFIT:DEBUG:ENERGY',ECMS(I)
C  total cross section
         IF(XTOT(I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'SIGTOT',XTOT(I),XTOTE(I),SIGTOT,
     &          ((XTOT(I)-SIGTOT)/XTOTE(I))**2
         ENDIF
C  total elastic cross section
         IF(XELA(I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'SIGEL ',XELA(I),XELAE(I),SIGELA,
     &          ((XELA(I)-SIGELA)/XELAE(I))**2
         ENDIF
C  inelastic cross section
         IF(XINE(I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'SIGIN ',XINE(I),XINEE(I),SIGTOT-SIGELA,
     &          ((XINE(I)-SIGTOT+SIGELA)/XINEE(I))**2
         ENDIF
C  single diffractive cross section
         IF(XSDI(I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,5E12.4)')
     &          'SIGSD ',XSDI(I),XSDIE(I),SIGSDI,
     &          ((XSDI(I)-(SIGSDI(1)+SIGSDI(2)))/XSDIE(I))**2
         ENDIF
C  nuclear slope
         IF(SLO(I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'SLOPE ',SLO(I),SLOE(I),SLOEL,
     &          ((SLO(I)-SLOEL)/SLOE(I))**2
         ENDIF
C  RHO production cross section
         IF(XPRO(1,I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'RHOPRO',XPRO(1,I),XPROE(1,I),SIGVM(1,0),
     &          ((XPRO(1,I)-SIGVM(1,0))/XPROE(1,I))**2
         ENDIF
C  OMEGA production cross section
         IF(XPRO(2,I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'OMEPRO',XPRO(2,I),XPROE(2,I),SIGVM(2,0),
     &          ((XPRO(2,I)-SIGVM(2,0))/XPROE(2,I))**2
         ENDIF
C  PHI production cross section
         IF(XPRO(3,I).GT.0.0) THEN
           WRITE(II,'(1X,A,2X,4E12.4)')
     &          'PHIPRO',XPRO(3,I),XPROE(3,I),SIGVM(3,0),
     &          ((XPRO(3,I)-SIGVM(3,0))/XPROE(3,I))**2
         ENDIF
       ENDIF
 110  CONTINUE
C  differential elastic cross section
      DO 130 I=1,MAXKK
         IF(INT(DSIGI(2,I)).GT.IZERO) THEN
           DO 140 KK=1,INT(DSIGI(2,I))
             XTA(1,KK)=DSDT(I,1,KK)
 140       CONTINUE
           CALL DSIGDT(DSIGI(1,I),XTA,INT(DSIGI(2,I)))
           DO 150 KK=1,INT(DSIGI(2,I))
             F(K+KK-1)=
     &       (XTA(INT(DSIGI(3,I))+1,KK)-DSDT(I,2,KK))/DSDT(I,3,KK)
 150       CONTINUE
C  last iteration: write out all results
           IF((IFLAG.EQ.3).OR.(IFLAG.EQ.1)) THEN
             WRITE(10,*)
     &              ' --------------------------------------------'
             IF(INT(DSIGI(3,I)).EQ.IONE) THEN
               WRITE(10,'(1X,A)') 'PHOTON PROTON ELASTIC SCATTERING'
             ELSE IF(INT(DSIGI(3,I)).EQ.ITWO) THEN
               WRITE(10,'(1X,A)') 'DIFFRACTIVE RHO PRODUCTION'
             ELSE IF(INT(DSIGI(3,I)).EQ.ITHREE) THEN
               WRITE(10,'(1X,A)') 'DIFFRACTIVE OMEGA PRODUCTION'
             ELSE IF(INT(DSIGI(3,I)).EQ.IFOUR) THEN
               WRITE(10,'(1X,A)') 'DIFFRACTIVE PHI PRODUCTION'
             ELSE
               WRITE(10,'(1X,A)') 'DATINP:ERROR: unknown particle'
               STOP
             ENDIF
             WRITE(10,'(1X,A,E12.3,A,E12.3)')
     &            'DSIG/DT  ',DSIGI(1,I),'   ',DSIGI(2,I)
             DO 155 KK=1,INT(DSIGI(2,I))
               WRITE(10,'(2X,3E12.3,2X,2E12.3)')
     &            DSDT(I,1,KK),DSDT(I,2,KK),DSDT(I,3,KK),
     &            XTA(INT(DSIGI(3,I))+1,KK),
     &            ((XTA(INT(DSIGI(3,I))+1,KK)-DSDT(I,2,KK))
     &             /DSDT(I,3,KK))**2
 155         CONTINUE
           ENDIF
           K=K+INT(DSIGI(2,I))
         ENDIF
 130  CONTINUE
      K=K-1
C  cross check
      IF(K.NE.IFIT) THEN
        WRITE(6,*) ' AMPFIT: ERROR: fit items do not conform with IFIT'
        WRITE(6,*) ' Items: ',K,'    IFIT: ',IFIT
        STOP
      ENDIF
C  debug output
      IF(IDEB(62).GE.5) THEN
        XCHI = 0.D0
        DO 115 KK=1,K
          XCHI = XCHI+F(KK)**2
 115    CONTINUE
        WRITE(6,'(1X,A,E12.4)') 'AMPFIT:DEBUG:CHI**2',XCHI
        IF(IDEB(62).GE.10)
     &    WRITE(6,*) ' ************** end of AMPFIT *****************'
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE TESTF
C*********************************************************************
C
C     test function to control fit program
C
C     input parameter: ECM   energy ( cm system )
C
C*********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER(ONE = 1.D0)
C
C  Landshoff like fit
      SS=ECM**2
      SIGTOT = GP(1)*SS**(ALPOM-ONE) + GR(1)*SS**(ALREG-ONE)
      SIGELA = GP(1)*SS**(ALPOM-ONE) + GPPP*SS**(ALREG-ONE)
C
      RETURN
      END
C
C
      SUBROUTINE PHMINSQ (M,N,F,X,E,IPRINT,NFUN ,NW,W,COV,XSTEP,FCNAME)
C*********************************************************************
C
C     subroutine to find minimum of functions
C     ( see CERNLIB, Program Library D507 )
C
C     changed to work with different FCN subroutines  (r.e.)
C
C*********************************************************************
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      DIMENSION F(M),X(N),E(N),W(NW)
      EXTERNAL FCNAME
      IFLAG=1
      ESCALE=1.
      MAXFUN=NFUN
      MPLUSN=M+N
      KST=N+MPLUSN
      NPLUS=N+1
      KINV=NPLUS*(MPLUSN+1)
      KSTORE=KINV-MPLUSN-1
      CALL PHFCN1(M,N,F,X,IFLAG,NW,W,KEND,KEND2N,FCNAME)
      IFLAG=4
      NN=N+N
      K=NN
      DO 1 I=1,M
      K=K+1
      W(K)=F(I)
    1 CONTINUE
      IINV=2
      K=KST
      I=1
    2 X(I)=X(I)+E(I)
      CALL PHFCN1(M,N,F,X,IFLAG,NW,W,KEND,KEND2N,FCNAME)
      X(I)=X(I)-E(I)
      DO 3 J=1,N
      K=K+1
      W(K)=0.
      W(J)=0.
    3 CONTINUE
      SUM=0.
      KK=NN
      DO 4 J=1,M
      KK=KK+1
      F(J)=F(J)-W(KK)
      SUM=SUM+F(J)*F(J)
    4 CONTINUE
      IF (SUM) 5,5,6
    5 WRITE(6,7)I
    7 FORMAT(5X,'E(',I3,') UNREASONABLY SMALL')
      RETURN
    6 SUM=1./SQRT (SUM)
      J=K-N+I
      W(J)=E(I)*SUM
      DO 9 J=1,M
      K=K+1
      W(K)=F(J)*SUM
      KK=NN+J
      DO 11 II=1,I
      KK=KK+MPLUSN
      W(II)=W(II)+W(KK)*W(K)
   11 CONTINUE
    9 CONTINUE
      ILESS=I-1
      IGAMAX=N+I-1
      INCINV=N-ILESS
      INCINP=INCINV+1
      IF (ILESS) 13,13,14
   13 W(KINV)=1.
      GO TO 15
   14 B=1.
      DO 16 J=NPLUS,IGAMAX
      W(J)=0.
   16 CONTINUE
      KK=KINV
      DO 17 II=1,ILESS
      IIP=II+N
      W(IIP)=W(IIP)+W(KK)*W(II)
      JL=II+1
      IF (JL-ILESS) 18,18,19
   18 DO 20 JJ=JL,ILESS
      KK=KK+1
      JJP=JJ+N
      W(IIP)=W(IIP)+W(KK)*W(JJ)
      W(JJP)=W(JJP)+W(KK)*W(II)
   20 CONTINUE
   19 B=B-W(II)*W(IIP)
      KK=KK+INCINP
   17 CONTINUE
      B=1./B
      KK=KINV
      DO 21 II=NPLUS,IGAMAX
      BB=-B*W(II)
      DO 22 JJ=II,IGAMAX
      W(KK)=W(KK)-BB*W(JJ)
      KK=KK+1
   22 CONTINUE
      W(KK)=BB
      KK=KK+INCINV
   21 CONTINUE
      W(KK)=B
   15 GO TO (27,24),IINV
   24 I=I+1
      IF (I-N) 2,2,25
   25 IINV=1
      FF=0.
      KL=NN
      DO 26 I=1,M
      KL=KL+1
      F(I)=W(KL)
      FF=FF+F(I)*F(I)
   26 CONTINUE
      ICONT = 1
      ISS=1
      MC=N+1
      IPP=IPRINT*(IPRINT-1)
      ITC=0
      IPS=1
      IPC=0
   27 IPC=IPC-IPRINT
      IF (IPC) 28,29,29
   28 WRITE(6,30)ITC,MC,FF
      WRITE(10,30)ITC,MC,FF
   30 FORMAT(//5X,'ITERATION',I4,I9,' CALLS OF FCN   ',5X,'F=',E24.14)
      WRITE(6,31)(X(I),I=1,N)
      WRITE(10,31) (X(I),I=1,N)
   31 FORMAT(5X,'VARIABLES',/(5E24.14))
      WRITE(6,32)(F(I),I=1,M)
      WRITE(10,32)(F(I),I=1,M)
   32 FORMAT(5X,'FUNCTIONS',/(5E24.14))
      IPC=IPP
      GO TO (29,33),IPS
   29 GO TO (34,35),ICONT
   35 IF (CHANGE-1.D0) 10,10,36
   10 CONTINUE
   37 WRITE(6,38)
      WRITE(10,38)
   38 FORMAT(//5X,'      FINAL VALUES OF FUNCTIONS AND VARIABLES')
      IPS=2
      GO TO 28
   33 IFLAG=3
      CALL PHFCN1(M,N,F,X,IFLAG,NW,W,KEND,KEND2N,FCNAME)
      IF(COV.EQ.0.) RETURN
      IWC1 = 2*N+N*(N+M)
      IWC=IWC1
      DO 91 I=1,N
      DO 91 J=1,I
      IWC=IWC+1
      W(IWC)=0.
      DO 90 MR=1,N
      JCOV=N+J+MR*(N+M)
      ICOV=N+I+MR*(N+M)
   90 W(IWC)=W(IWC)+W(ICOV)*W(JCOV)
   91 CONTINUE
 1000 FORMAT(///40X,'VARIANCE-COVARIANCE MATRIX'///)
      WRITE(6,1000)
      WRITE(10,1000)
      IB=IWC1
      DO 92 I=1,N
      IA=IB+1
      IB=IB+I
      WRITE(6,1001)(W(J),J=IA,IB)
      WRITE(10,1001)(W(J),J=IA,IB)
   92 CONTINUE
 1001 FORMAT(10X,5G20.6//)
      RETURN
   36 ICONT=1
   34 ITC=ITC+1
      K=N
      KK=KST
      DO 39 I=1,N
      K=K+1
      W(K)=0.
      KK=KK+N
      W(I)=0.
      DO 40 J=1,M
      KK=KK+1
      W(I)=W(I)+W(KK)*F(J)
   40 CONTINUE
   39 CONTINUE
      DM=0.
      K=KINV
      DO 41 II=1,N
      IIP=II+N
      W(IIP)=W(IIP)+W(K)*W(II)
      JL=II+1
      IF (JL-N) 42,42,43
   42 DO 44 JJ=JL,N
      JJP=JJ+N
      K=K+1
      W(IIP)=W(IIP)+W(K)*W(JJ)
      W(JJP)=W(JJP)+W(K)*W(II)
   44 CONTINUE
      K=K+1
   43 IF (DM-ABS (W(II)*W(IIP))) 45,41,41
   45 DM=ABS (W(II)*W(IIP))
      KL=II
   41 CONTINUE
      II=N+MPLUSN*KL
      CHANGE=0.
      DO 46 I=1,N
      JL=N+I
      W(I)=0.
      DO 47 J=NPLUS,NN
      JL=JL+MPLUSN
      W(I)=W(I)+W(J)*W(JL)
   47 CONTINUE
      II=II+1
      W(II)=W(JL)
      W(JL)=X(I)
      IF (ABS (E(I)*CHANGE)-ABS (W(I))) 48,48,46
   48 CHANGE=ABS (W(I)/E(I))
   46 CONTINUE
      DO 49 I=1,M
      II=II+1
      JL=JL+1
      W(II)=W(JL)
      W(JL)=F(I)
   49 CONTINUE
      FC=FF
      ACC=0.1/CHANGE
      IT=3
      XC=0.
      XL=0.
      IS=3
      IF (CHANGE-1.) 50,50,51
   50 ICONT=2
   51 CALL PHVD01A (IT,XC,FC,20,ACC,0.1D0,XSTEP)
      GO TO (52,53,53,53),IT
   52 MC=MC+1
      IF (MC-MAXFUN) 54,54,55
   55 WRITE(6,56)MAXFUN
      WRITE(10,56)MAXFUN
   56 FORMAT(5X,I6,' CALLS OF FCN')
      ISS=2
      GO TO 53
   54 XL=XC-XL
      DO 57 J=1,N
      X(J)=X(J)+XL*W(J)
   57 CONTINUE
      XL=XC
      CALL PHFCN1(M,N,F,X,IFLAG,NW,W,KEND,KEND2N,FCNAME)
      FC=0.
      DO 58 J=1,M
      FC=FC+F(J)*F(J)
   58 CONTINUE
      GO TO (59,59,60),IS
   60 K=N
      IF (FC-FF) 61,51,62
   61 IS=2
      FMIN=FC
      FSEC=FF
      GO TO 63
   62 IS=1
      FMIN=FF
      FSEC=FC
      GO TO 63
   59 IF (FC-FSEC) 64,51,51
   64 K=KSTORE
      GO TO (75,74),IS
   75 K=N
   74 IF (FC-FMIN) 65,51,66
   66 FSEC=FC
      GO TO 63
   65 IS=3-IS
      FSEC=FMIN
      FMIN=FC
   63 DO 67 J=1,N
      K=K+1
      W(K)=X(J)
   67 CONTINUE
      DO 68 J=1,M
      K=K+1
      W(K)=F(J)
   68 CONTINUE
      GO TO 51
   53 K=KSTORE
      KK=N
      GO TO (69,70,69),IS
   70 K=N
      KK=KSTORE
   69 SUM=0.
      DM=0.
      JJ=KSTORE
      DO 71 J=1,N
      K=K+1
      KK=KK+1
      JJ=JJ+1
      X(J)=W(K)
      W(JJ)=W(K)-W(KK)
   71 CONTINUE
      DO 72 J=1,M
      K=K+1
      KK=KK+1
      JJ=JJ+1
      F(J)=W(K)
      W(JJ)=W(K)-W(KK)
      SUM=SUM+W(JJ)*W(JJ)
      DM=DM+F(J)*W(JJ)
   72 CONTINUE
      GO TO (73,10),ISS
   73 J=KINV
      KK=NPLUS-KL
      DO 76 I=1,KL
      K=J+KL-I
      J=K+KK
      W(I)=W(K)
      W(K)=W(J-1)
   76 CONTINUE
      IF (KL-N) 77,78,78
   77 KL=KL+1
      JJ=K
      DO 79 I=KL,N
      K=K+1
      J=J+NPLUS-I
      W(I)=W(K)
      W(K)=W(J-1)
   79 CONTINUE
      W(JJ)=W(K)
      B=1./W(KL-1)
      W(KL-1)=W(N)
      GO TO 88
   78 B=1./W(N)
   88 K=KINV
      DO 80 I=1,ILESS
      BB=B*W(I)
      DO 81 J=I,ILESS
      W(K)=W(K)-BB*W(J)
      K=K+1
   81 CONTINUE
      K=K+1
   80 CONTINUE
      IF (FMIN-FF) 82,83,83
   83 CHANGE=0.
      GO TO 84
   82 FF=FMIN
      CHANGE=ABS (XC)*CHANGE
   84 XL=-DM/FMIN
      SUM=1./SQRT (SUM+DM*XL)
      K=KSTORE
      DO 85 I=1,N
      K=K+1
      W(K)=SUM*W(K)
      W(I)=0.
   85 CONTINUE
      DO 86 I=1,M
      K=K+1
      W(K)=SUM*(W(K)+XL*F(I))
      KK=NN+I
      DO 87 J=1,N
      KK=KK+MPLUSN
      W(J)=W(J)+W(KK)*W(K)
   87 CONTINUE
   86 CONTINUE
      GO TO 14
      END
C
      SUBROUTINE PHVD01A (ITEST,X,F,MAXFUN,ABSACC,RELACC,XSTEP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      GO TO (1,2,2),ITEST
    2 IS=6-ITEST
      ITEST=1
      IINC=1
      XINC=XSTEP+XSTEP
      MC=IS-3
      IF (MC) 4,4,15
    3 MC=MC+1
      IF (MAXFUN-MC) 12,15,15
   12 ITEST=4
   43 X=DB
      F=FB
      IF (FB-FC) 15,15,44
   44 X=DC
      F=FC
   15 RETURN
    1 GO TO (5,6,7,8),IS
    8 IS=3
    4 DC=X
      FC=F
      X=X+XSTEP
      GO TO 3
    7 IF (FC-F) 9,10,11
   10 X=X+XINC
      XINC=XINC+XINC
      GO TO 3
    9 DB=X
      FB=F
      XINC=-XINC
      GO TO 13
   11 DB=DC
      FB=FC
      DC=X
      FC=F
   13 X=DC+DC-DB
      IS=2
      GO TO 3
    6 DA=DB
      DB=DC
      FA=FB
      FB=FC
   32 DC=X
      FC=F
      GO TO 14
    5 IF (FB-FC) 16,17,17
   17 IF (F-FB) 18,32,32
   18 FA=FB
      DA=DB
   19 FB=F
      DB=X
      GO TO 14
   16 IF (FA-FC) 21,21,20
   20 XINC=FA
      FA=FC
      FC=XINC
      XINC=DA
      DA=DC
      DC=XINC
   21 XINC=DC
      IF ((D-DB)*(D-DC)) 32,22,22
   22 IF (F-FA) 23,24,24
   23 FC=FB
      DC=DB
      GO TO 19
   24 FA=F
      DA=X
   14 IF (FB-FC) 25,25,29
   25 IINC=2
      XINC=DC
      IF (FB-FC) 29,45,29
   29 D=(FA-FB)/(DA-DB)-(FA-FC)/(DA-DC)
      IF (D*(DB-DC)) 33,33,37
   37 D=0.5*(DB+DC-(FB-FC)/D)
      IF (ABS (D-X)-ABS (ABSACC)) 34,34,35
   35 IF (ABS (D-X)-ABS (D*RELACC)) 34,34,36
   34 ITEST=2
      GO TO 43
   36 IS=1
      X=D
      IF ((DA-DC)*(DC-D)) 3,26,38
   38 IS=2
      GO TO (39,40),IINC
   39 IF (ABS (XINC)-ABS (DC-D)) 41,3,3
   33 IS=2
      GO TO (41,42),IINC
   41 X=DC
      GO TO 10
   40 IF (ABS (XINC-X)-ABS (X-DC)) 42,42,3
   42 X=0.5*(XINC+DC)
      IF ((XINC-X)*(X-DC)) 26,26,3
   45 X=0.5*(DB+DC)
      IF ((DB-X)*(X-DC)) 26,26,3
   26 ITEST=3
      GO TO 43
      END
C
C
      SUBROUTINE PHFCN1(M,N,F,X,IFLAG,NW,W,KEND,KEND2N,FCNAME)
C*********************************************************************
C
C     THIS SUBROUTINE WILL BE MODIFIED AND USED FOR CONDITIONED MINIMIZ.
C     ( called by PHMINSQ, CERNLIB )
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      DIMENSION F(M),X(N),W(NW)
      EXTERNAL FCNAME
      CALL FCNAME(M,N,F,X,IFLAG)
      RETURN
      END
C
C
      SUBROUTINE FITDAT
C**********************************************************************
C
C     initialization of parameters of fit program
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NECM=200,NPAR=30)
      CHARACTER*6 HINT
      COMMON /FITMOD/ IFMOD,IFHARD
      COMMON /FITTYP/IFIT,NIST,ECMS(NECM),XTOT(NECM),XTOTE(NECM),
     &               XELA(NECM),XELAE(NECM),XINE(NECM),
     &               XINEE(NECM),XSDI(NECM),XSDIE(NECM),
     &               SLO(NECM),SLOE(NECM),XPRO(3,NECM),XPROE(3,NECM),
     &               XI(NPAR),XINC(NPAR),NFITP,HINT(NPAR),IFITF(NPAR)
      COMMON /CONTRL/XSTEP,NFUN
C
      HINT(1)  =  'ALPOM '
      HINT(2)  =  'ALPOMP'
      HINT(3)  =  'GP1   '
      HINT(4)  =  'GP2   '
      HINT(5)  =  'B0POM1'
      HINT(6)  =  'B0POM2'
      HINT(7)  =  'ALREG '
      HINT(8)  =  'ALREGP'
      HINT(9)  =  'GR1   '
      HINT(10) =  'B0REG1'
      HINT(11) =  'GPPP  '
      HINT(12) =  'B0PPP '
      HINT(13) =  'VDMFA1'
      HINT(14) =  'VDMFA2'
      HINT(15) =  'VDMFA3'
      HINT(16) =  'VDMFA4'
      HINT(17) =  'B0HAR '
      HINT(18) =  'AKFAC '
      HINT(19) =  'PHISU1'
      HINT(20) =  'PHISU2'
      HINT(21) =  'RMASS1'
      HINT(22) =  'RMASS2'
      HINT(23) =  'GPPR  '
      HINT(24) =  'B0PPR '
      HINT(25) =  'ERROR '
      HINT(26) =  'ERROR '
      HINT(27) =  'ERROR '
      HINT(28) =  'ERROR '
      HINT(29) =  'ERROR '
      HINT(30) =  'CUTMU '
C
      NFUN  = 400
      XSTEP = 0.05D0
      DO 10 I=1,30
        XINC(I)  = 0.02D0
 10   CONTINUE
      IFMOD = 0
      IFHARD = 0
C
      END
      SUBROUTINE REGPAR(ISTH,IDPDG,IDBAM,JM1,JM2,P1,P2,P3,P4,
     &                  IPHIS1,IPHIS2,IC1,IC2,IPOS,IMODE)
C**********************************************************************
C
C     registration of particle in COMMONs HEPEVS and HEPEVE
C
C     input:    ISTH             status code of particle
C                                 -2     initial parton hard scattering
C                                 -1     parton
C                                  0     chain
C                                  1     visible particle (no color)
C                                  2     decayed particle
C               IDPDG            PDG particle ID code
C               IDBAM            BAMJET particle ID code
C               JM1,JM2          first and second mother index
C               P1..P4           four momentum
C               IPHIS1           extended history information
C                                  IPHIS1<100: JM1 from particle 1
C                                  IPHIS1>100: JM1 from particle 2
C                                  1    valence quark
C                                  2    valence diquark
C                                  3    sea quark
C                                  4    sea diquark
C                                  (neg. for antipartons)
C               IPHIS2           extended history information
C                                  positive: JM2 from particle 1
C                                  negative: JM2 from particle 2
C                                  (see IPHIS1)
C               IC1,IC2          color labels for partons
C               IMODE            1  register given parton
C                                0  reset HEPEVS and HEPEVE
C                                2  return data of entry IPOS
C
C               IPOS             position of particle in HEPEVS
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (DEPS = 1.D-20)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      IF(IMODE.EQ.1) THEN
        IF(IDEB(76).GE.26) THEN
          WRITE(6,'(/1X,A,/1X,A,/2X,I3,I6,3I4,4E10.3)')
     &      'REGPAR:DEBUG:','ISTH,IDPDG,IDBAM,JM1,JM2,P1,P2,P3,P4',
     &      ISTH,IDPDG,IDBAM,JM1,JM2,P1,P2,P3,P4
          WRITE(6,'(1X,A,/2X,6I6)')
     &      '   IPHIS1,IPHIS2,IC1,IC2,IPOS,IMODE',
     &      IPHIS1,IPHIS2,IC1,IC2,IPOS,IMODE
        ENDIF
        IF(NHEP.EQ.NMXHEP) THEN
          WRITE(6,'(/1X,A,I6/)')
     &      'REGPAR:ERROR: NO MORE SPACE FOR PARTICLE REGISTRATION',
     &      NHEP
          CALL POABRT
        ENDIF
        NHEP = NHEP+1
        IDBAMI = IDBAM
        IDPDGI = IDPDG
        IF(ISTH.EQ.1) THEN
          IF((IDBAM.NE.0).AND.(IDPDG.EQ.0)) THEN
            IDPDGI = MPDGHA(IDBAM)
          ELSE IF((IDBAM.EQ.0).AND.(IDPDG.NE.0)) THEN
            IDBAMI = MCIHAD(IDPDG)
          ENDIF
        ENDIF
C  standard data
        ISTHEP(NHEP) = ISTH
        IDHEP(NHEP)  = IDPDGI
        JMOHEP(1,NHEP) = JM1
        JMOHEP(2,NHEP) = JM2
C  update of mother-daugther relations
        IF(ABS(ISTH).LE.1) THEN
          IF(JM1.GT.0) THEN
            IF(JDAHEP(1,JM1).EQ.0) THEN
              JDAHEP(1,JM1) = NHEP
              ISTHEP(JM1) = 2
            ENDIF
            JDAHEP(2,JM1) = NHEP
          ENDIF
          IF((JM2.NE.JM1).AND.(JM2.GT.0)) THEN
            IF(JDAHEP(1,JM2).EQ.0) THEN
              JDAHEP(1,JM2) = NHEP
              ISTHEP(JM2) = 2
            ENDIF
            JDAHEP(2,JM2) = NHEP
          ELSE IF(JM2.LT.0) THEN
            DO 100 II=JM1+1,-JM2
              IF(JDAHEP(1,II).EQ.0) THEN
                JDAHEP(1,II) = NHEP
                ISTHEP(II) = 2
              ENDIF
              JDAHEP(2,II) = NHEP
100         CONTINUE
          ENDIF
        ENDIF
        PHEP(1,NHEP) = P1
        PHEP(2,NHEP) = P2
        PHEP(3,NHEP) = P3
        PHEP(4,NHEP) = P4
        IF(ABS(ISTH).LE.3) THEN
          TMP=P4**2-P1**2-P2**2-P3**2
          PHEP(5,NHEP) = SIGN(SQRT(ABS(TMP)),TMP)
        ELSE
          PHEP(5,NHEP) = 0.D0
        ENDIF
        JDAHEP(1,NHEP) = 0
        JDAHEP(2,NHEP) = 0
C  extended information
        IMPART(NHEP) = IDBAMI
C  extended history information
        IPHIST(1,NHEP) = IPHIS1
        IPHIST(2,NHEP) = IPHIS2
C  charge/baryon number or colors
        IF(ISTH.EQ.1) THEN
          ICOLOR(1,NHEP) = IPOCH3(NHEP,2)
          ICOLOR(2,NHEP) = IPOBA3(NHEP,2)
        ELSE
          ICOLOR(1,NHEP) = IC1
          ICOLOR(2,NHEP) = IC2
        ENDIF
C
        IPOS = NHEP
        IF(IDEB(76).GE.26) THEN
          WRITE(6,'(1X,A,2I4,2X,2I4,E12.3,I5)')
     &      'REGPAR:OUTPUT:IPHIST1/2,IC1/2,MASS,IPOS',IPHIST(1,NHEP),
     &      IPHIST(2,NHEP),ICOLOR(1,NHEP),ICOLOR(2,NHEP),
     &      PHEP(5,NHEP),IPOS
        ENDIF
      ELSE IF(IMODE.EQ.0) THEN
        NHEP   = 0
        NEVHEP = 0
      ELSE IF(IMODE.EQ.2) THEN
        IF((IPOS.LT.1).OR.(IPOS.GT.NHEP)) THEN
          WRITE(6,'(1X,A,2I8)')
     &      'REGPAR:WARNING: INDEX OUT OF BOUNDS (NHEP,IPOS)',NHEP,IPOS
          RETURN
        ENDIF
        ISTH  = ISTHEP(IPOS)
        IDPDG = IDHEP(IPOS)
        IDBAM = IMPART(IPOS)
        JM1   = JMOHEP(1,IPOS)
        JM2   = JMOHEP(2,IPOS)
        P1    = PHEP(1,IPOS)
        P2    = PHEP(2,IPOS)
        P3    = PHEP(3,IPOS)
        P4    = PHEP(4,IPOS)
        IPHIS1= IPHIST(1,IPOS)
        IPHIS2= IPHIST(2,IPOS)
        IC1   = ICOLOR(1,IPOS)
        IC2   = ICOLOR(2,IPOS)
      ELSE
        WRITE(6,'(1X,A,I8)') 'REGPAR:WARNING: UNSUPPORTED MODE ',IMODE
      ENDIF
      END
C
C
      SUBROUTINE INDEX2(KA,KB,IND)
C**********************************************************************
C
C     determination of index in baryon table
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      KP=KA*KB
      KS=KA+KB
      IF(KP.EQ.1) THEN
        IND=1
      ELSE IF(KP.EQ.2) THEN
        IND=2
      ELSE IF(KP.EQ.3) THEN
        IND=3
      ELSE IF(KP.EQ.4.AND.KS.EQ.5) THEN
        IND=4
      ELSE IF(KP.EQ.5) THEN
        IND=5
      ELSE IF(KP.EQ.6.AND.KS.EQ.7) THEN
        IND=6
      ELSE IF(KP.EQ.4.AND.KS.EQ.4) THEN
        IND=7
      ELSE IF(KP.EQ.6.AND.KS.EQ.5) THEN
        IND=8
      ELSE IF(KP.EQ.8) THEN
        IND=9
      ELSE IF(KP.EQ.10) THEN
        IND=10
      ELSE IF(KP.EQ.12.AND.KS.EQ.8) THEN
        IND=11
      ELSE IF(KP.EQ.9) THEN
        IND=12
      ELSE IF(KP.EQ.12.AND.KS.EQ.7) THEN
        IND=13
      ELSE IF(KP.EQ.15) THEN
        IND=14
      ELSE IF(KP.EQ.18) THEN
        IND=15
      ELSE IF(KP.EQ.16) THEN
        IND=16
      ELSE IF(KP.EQ.20) THEN
        IND=17
      ELSE IF(KP.EQ.24) THEN
        IND=18
      ELSE IF(KP.EQ.25) THEN
        IND=19
      ELSE IF(KP.EQ.30) THEN
        IND=20
      ELSE IF(KP.EQ.36) THEN
        IND=21
      ELSE
        WRITE(6,'(1X,A,I3,I3)') 'ERROR:INDEX2: ',KA,KB
        CALL POABRT
      ENDIF
      RETURN
      END
C
C
      INTEGER FUNCTION NDIQU(K1,K2)
C**********************************************************************
C
C     determine PDG diquark number from quark numbers K1, K2
C
C     K1, K2:  1   d  (anti --> neg.)
C              2   u
C              3   s
C              4   c
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      DIMENSION NDIQ(4,4)
      DATA NDIQ /
     &   1103, 2101, 3101, 4101,
     &   2103, 2203, 3201, 4201,
     &   3103, 3203, 3303, 4301,
     &   4103, 4203, 4303, 4403 /
C
      K = K1*K2
      IF((K.GE.1).AND.(K.LE.16)) THEN
        NDIQU = SIGN(NDIQ(ABS(K1),ABS(K2)),K1)
      ELSE
        WRITE(6,'(/1X,A,2I8)') 'NDIQU:WARNING: QUARK-ID OUT OF RANGE',
     &    K1,K2
        NDIQU = 0
        CALL POABRT
      ENDIF
      END
C
C
      INTEGER FUNCTION IPDGQU(K,IDBAM)
C**********************************************************************
C
C     quark contents according to PDG conventions
C     (random selection in case of quark mixing)
C
C     input:   IDBAM  BAMJET particle code
C              K      1..3   quark number
C
C     output:  1   d  (anti --> neg.)
C              2   u
C              3   s
C              4   c
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      IQ = IBAMQU(K,IDBAM,0)
C  quark-antiquark
      IF((IQ.GT.6).AND.(IQ.LT.13)) THEN
        IQ = 6 - IQ
      ENDIF
C  exchange of up and down
      IF(ABS(IQ).EQ.1) THEN
        IQ = SIGN(2,IQ)
      ELSE IF(ABS(IQ).EQ.2) THEN
        IQ = SIGN(1,IQ)
      ENDIF
      IPDGQU = IQ
      END
C
C
      INTEGER FUNCTION IBAMQU(K,IDBAM,INEW)
C**********************************************************************
C
C     quark contents according to BAMJET conventions
C     (random selection in case of quark mixing)
C
C     input:   IDBAM  BAMJET particle code
C              K      1..3   quark number
C
C     output:  1   u      7   u bar
C              2   d      8   d bar
C              3   s      9   s bar
C              4   c     10   c bar
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      DIMENSION ITAB(3,210)
      DATA ((ITAB(I,K),I=1,3),K=1,30) /
     &    1,  1,  2,   7,  7,  8,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   1,  2,  2,   7,  8,  8,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    1,  8,  0,   2,  7,  0,   1,  9,  0,
     &    3,  7,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   2,  2,  3,   1,  1,  3,
     &    1,  2,  3, 201,202,  0,   2,  9,  0,
     &    3,  8,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0 /
      DATA ((ITAB(I,K),I=1,3),K=31,60) /
     &    3,  9,  0,   1,  8,  0, 203,204,  0,
     &    2,  7,  0,   0,  0,  0,   1,  9,  0,
     &    2,  9,  0,   3,  7,  0,   3,  8,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   1,  1,  1,   1,  1,  2,
     &    1,  2,  2,   2,  2,  2,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0 /
      DATA ((ITAB(I,K),I=1,3),K=61,90) /
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    7,  7,  7,   7,  7,  8,   7,  8,  8,
     &    8,  8,  8,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0 /
      DATA ((ITAB(I,K),I=1,3),K=91,120) /
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   3,  9,  0,
     &    1,  3,  3,   2,  3,  3,   7,  7,  9,
     &    7,  8,  9,   8,  8,  9,   7,  9,  9,
     &    8,  9,  9,   1,  1,  3,   1,  2,  3,
     &    2,  2,  3,   1,  3,  3,   2,  3,  3,
     &    3,  3,  3,   7,  7,  9,   7,  8,  9,
     &    8,  8,  9,   7,  9,  9,   8,  9,  9,
     &    9,  9,  9,   4,  7,  0,   4,  8,  0,
     &    2, 10,  0,   1, 10,  0,   4,  9,  0 /
      DATA ((ITAB(I,K),I=1,3),K=121,150) /
     &    3, 10,  0,   4, 10,  0,   4,  7,  0,
     &    4,  8,  0,   2, 10,  0,   1, 10,  0,
     &    4,  9,  0,   3, 10,  0,   4, 10,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   1,  2,  4,   1,  3,  4,
     &    2,  3,  4,   1,  1,  4,   0,  0,  0,
     &    2,  2,  4,   0,  0,  0,   0,  0,  0,
     &    3,  3,  4,   1,  4,  4,   2,  4,  4,
     &    3,  4,  4,   7,  8, 10,   7,  9, 10 /
      DATA ((ITAB(I,K),I=1,3),K=151,180) /
     &    8,  9, 10,   7,  7, 10,   0,  0,  0,
     &    8,  8, 10,   0,  0,  0,   0,  0,  0,
     &    9,  9, 10,   7, 10, 10,   8, 10, 10,
     &    9, 10, 10,   1,  1,  4,   1,  2,  4,
     &    2,  2,  4,   1,  3,  4,   2,  3,  4,
     &    3,  3,  4,   1,  4,  4,   2,  4,  4,
     &    3,  4,  4,   4,  4,  4,   7,  7, 10,
     &    7,  8, 10,   8,  8, 10,   7,  9, 10,
     &    8,  9, 10,   9,  9, 10,   7, 10, 10,
     &    8, 10, 10,   9, 10, 10,  10, 10, 10 /
      DATA ((ITAB(I,K),I=1,3),K=181,210) /
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   1,  7,  0,
     &    2,  8,  0,   1,  7,  0,   2,  8,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0,
     &    0,  0,  0,   0,  0,  0,   0,  0,  0 /
      DATA IDOLD /0/
C
      IF(K.GT.0) THEN
        IF((ITAB(1,IDBAM).LE.200).OR.(ABS(ITAB(1,IDBAM)).GT.500)) THEN
          ID = ITAB(K,IDBAM)
        ELSE
          IF(IDOLD.NE.IDBAM) THEN
            IT=AINT((ITAB(2,IDBAM)-ITAB(1,IDBAM)
     &         +0.999999999999999D0)*DRNDM(X)+ITAB(1,IDBAM))
          ELSE
            IDOLD = 0
          ENDIF
          ID = ITAB(K,IT)
        ENDIF
        IDOLD = IDBAM
        IBAMQU = ID
      ELSE
        ITAB(-K,IDBAM) = INEW
        IBAMQU = INEW
      ENDIF
      END
C
C
      INTEGER FUNCTION ICONV1(IPART)
C*********************************************************************
C
C     conversion of quark numbering scheme to PARTICLE DATA GROUP
C                                             convention
C
C     input:   old internal particle code of hard scattering
C                    0   gluon
C                    1   d
C                    2   u
C                    3   s
C                    4   c
C     valence quarks changed to standard numbering
C
C     output:  standard particle codes
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      II = ABS(IPART)
C  change gluon number
      IF(II.EQ.0) THEN
        ICONV1 = 21
C  change valence quark
      ELSE IF((II.GT.6).AND.(II.LT.13)) THEN
        ICONV1 = SIGN(II-6,IPART)
      ELSE
        ICONV1 = IPART
      ENDIF
      END
C
C
      SUBROUTINE HACODE(ID1,ID2,IDBAM1,IDBAM2)
C*********************************************************************
C
C     determination of hadron index from quarks
C
C     input:   ID1,ID2   parton code according to PDG conventions
C
C     output:  IDBAM1,2  BAMJET particle code
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON/INPDAT/IMPS(6,6),IMVE(6,6),IB08(6,21),IB10(6,21),
     *IA08(6,21),IA10(6,21),A1,B1,B2,B3,LT,LB,ISU,IYQ,BET,AS,B8,AME,DIQ
C
      IDBAM1 = 0
      IF(ABS(ID1).GT.6) THEN
C  diquark-antidiquark
        IF(ABS(ID2).GT.6) THEN
          IDBAM1 = 23
          IDBAM2 = 33
          RETURN
        ENDIF
        I = ICONV2(ID1,1)
        J = ICONV2(ID1,2)
        K = ICONV2(ID2,0)
        IBAR = 1
      ELSE IF(ABS(ID2).GT.6) THEN
        I = ICONV2(ID2,1)
        J = ICONV2(ID2,2)
        K = ICONV2(ID1,0)
        IBAR = 1
      ELSE IF((ID1*ID2).LT.0) THEN
        I = ICONV2(ID1,0)
        J = ICONV2(ID2,0)
        IBAR = 0
      ELSE
        WRITE(6,'(/1X,A,2I6)') 'HACODE:ERROR: INVALID FLAVORS ',ID1,ID2
        CALL POPREV(-1)
      ENDIF
C  baryon
      IF(IBAR.EQ.1) THEN
        IF(I.GT.6) THEN
          II = I -6
          JJ = J -6
          KK = K -6
          IABAR = 1
        ELSE
          II = I
          JJ = J
          KK = K
          IABAR = 0
        ENDIF
        IF((II*JJ*KK).LE.0) THEN
          WRITE(6,'(/1X,A,2I6)')
     &      'HACODE:ERROR: INCONSISTENT QUARK FLAVOURS ',ID1,ID2
          CALL POABRT
        ENDIF
        CALL INDEX2(II,JJ,IND)
        IF(IABAR.EQ.1) THEN
C  antibaryon
          IDBAM1 = IA08(KK,IND)
          IDBAM2 = IA10(KK,IND)
        ELSE
C  baryon
          IDBAM1 = IB08(KK,IND)
          IDBAM2 = IB10(KK,IND)
        ENDIF
C  meson
      ELSE
        IF(I.GT.6) THEN
          II = I -6
          JJ = J
        ELSE IF(J.GT.6) THEN
          II = J -6
          JJ = I
        ELSE
          WRITE(6,'(/1X,A,2I6)')
     &      'HACODE:ERROR: INCONSISTENT QUARK FLAVOURS ',ID1,ID2
          CALL POABRT
        ENDIF
        IDBAM1 = IMPS(II,JJ)
        IDBAM2 = IMVE(II,JJ)
      ENDIF
      IF(IDBAM1.EQ.0) IDBAM1 = IDBAM2
      IF(IDBAM2.EQ.0) IDBAM2 = IDBAM1
      END
C
C
      SUBROUTINE ID2BAM(ID1,ID2,NOBAM,IBAM1,IBAM2,IBAM3,IBAM4)
C*********************************************************************
C
C     conversion of quark numbering scheme
C
C     input:   standard particle codes:
C                       ID1
C                       ID2
C
C     output:  NOBAM    BAMJET string code
C              BAMJET particle codes:
C                       IBAM1
C                       IBAM2
C                       IBAM3
C                       IBAM4
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      IDA1 = ABS(ID1)
      IDA2 = ABS(ID2)
C  quark antiquark jet
      IF((IDA1.LE.6).AND.(IDA2.LE.6)) THEN
        IF((ID1*ID2).GE.0) THEN
          WRITE(6,'(/1X,A,2I5)') 'ID2BAM:ERROR: NO CHAIN POSSIBLE ',
     &         ID1,ID2
          CALL POABRT
        ENDIF
        IBAM1 = ICONV2(ID1,0)
        IBAM2 = ICONV2(ID2,0)
        IBAM3 = 0
        IBAM4 = 0
        NOBAM = 3
C  quark diquark jet
      ELSE IF((IDA2.GT.6).AND.(IDA1.LE.6)) THEN
        IF((ID1*ID2).LE.0) THEN
          WRITE(6,'(/1X,A,2I5)') 'ID2BAM:ERROR: NO CHAIN POSSIBLE ',
     &         ID1,ID2
          CALL POABRT
        ENDIF
        IBAM1 = ICONV2(ID1,0)
        IBAM2 = ICONV2(ID2,1)
        IBAM3 = ICONV2(ID2,2)
        IBAM4 = 0
        NOBAM = 4
C  diquark quark jet
      ELSE IF((IDA1.GT.6).AND.(IDA2.LE.6)) THEN
        IF((ID1*ID2).LE.0) THEN
          WRITE(6,'(/1X,A,2I5)') 'ID2BAM:ERROR: NO CHAIN POSSIBLE ',
     &         ID1,ID2
          CALL POABRT
        ENDIF
        IBAM1 = ICONV2(ID1,1)
        IBAM2 = ICONV2(ID1,2)
        IBAM3 = ICONV2(ID2,0)
        IBAM4 = 0
        NOBAM = 6
C  diquark antidiquark jet
      ELSE IF((IDA1.GT.6).AND.(IDA2.GT.6)) THEN
        IF((ID1*ID2).GE.0) THEN
          WRITE(6,'(/1X,A,2I5)') 'ID2BAM:ERROR: NO CHAIN POSSIBLE ',
     &         ID1,ID2
          CALL POABRT
        ENDIF
        IBAM1 = ICONV2(ID1,1)
        IBAM2 = ICONV2(ID1,2)
        IBAM3 = ICONV2(ID2,1)
        IBAM4 = ICONV2(ID2,2)
        NOBAM = 5
      ENDIF
      END
C
C
      INTEGER FUNCTION ICONV2(ID,NN)
C*********************************************************************
C
C     conversion of quark numbering scheme
C
C     input:   PDG parton numbering
C              for diquarks:  NN number of the constituent quark
C                             (e.g. ID=2301,NN=1 -> ICONV2=1)
C
C     output:  BAMJET particle codes
C              1 u     7 a-u
C              2 d     8 a-d
C              3 s     9 a-s
C              4 c    10 a-c
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      IDA = ABS(ID)
C  diquarks
      IF(IDA.GT.6) THEN
        IDA = IDA/(10**(4-NN))
        IDA = MOD(IDA,10)
      ENDIF
C  exchange up and dn quarks
      IF(IDA.EQ.1) THEN
        IDA = 2
      ELSE IF(IDA.EQ.2) THEN
        IDA = 1
      ENDIF
C  antiquarks
      IF(ID.LT.0) IDA = IDA+6
      ICONV2 = IDA
      END
C
C
      INTEGER FUNCTION ICONV3(ID)
C*********************************************************************
C
C     conversion of quark numbering scheme: u <-> d
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      IDA = ABS(ID)
C  exchange up and dn quarks
      IF(IDA.EQ.1) THEN
        IDA = 2
      ELSE IF(IDA.EQ.2) THEN
        IDA = 1
      ENDIF
C  antiquarks
      IF(ID.LT.0) IDA = -IDA
      ICONV3 = IDA
      END
C
C
      SUBROUTINE MKSLTR(P1,P2,GAM,GAMB)
C********************************************************************
C
C     calculate successive Lorentz boots for arbitrary Lorentz trans.
C
C     input:   P1                initial 4 vector
C              GAM(3),GAMB(3)    Lorentz boost parameters
C
C     output:  P2                final  4 vector
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      DIMENSION P1(4),P2(4),GAM(3),GAMB(3)
C
      P2(4) = P1(4)
      DO 150 I=1,3
        P2(I)=GAM(I)*P1(I)+GAMB(I)*P2(4)
        P2(4)=GAM(I)*P2(4)+GAMB(I)*P1(I)
 150  CONTINUE
      END
C
C
      SUBROUTINE GETLTR(P1,P2,GAM,GAMB,DELE,IREJ)
C********************************************************************
C
C     calculate Lorentz boots for arbitrary Lorentz transformation
C
C     input:   P1    initial 4 vector
C              P2    final 4 vector
C
C     output:  GAM(3),GAMB(3)
C              DELE   energy deviation
C              IREJ   0 success
C                     1 failure
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER ( ONE = 1.D0,
     &            ZERO = 0.D0,
     &            DREL = 0.001D0 )
      DIMENSION P1(4),P2(4),GAM(3),GAMB(3),PA(4),PP(4)
C
      IREJ = 1
      DO 50 K=1,4
        PA(K) = P1(K)
        PP(K) = P1(K)
 50   CONTINUE
      PM1 = P1(4)**2-P1(1)**2-P1(2)**2-P1(3)**2
      DO 100 I=1,3
        PP(I) = P2(I)
        PP(4) = PM1+PP(1)**2+PP(2)**2+PP(3)**2
        IF(PP(4).LE.ZERO) RETURN
        PP(4) = SQRT(PP(4))
        GAMB(I) = (SQRT(PA(4)**2-PA(I)**2+PP(I)**2)*PP(I)
     &             -PA(4)*PA(I))/(PA(4)**2+PP(I)**2)
        GAM(I) = ONE/SQRT(ONE-GAMB(I)**2)
        GAMB(I) = GAMB(I)*GAM(I)
        DO 150 K=1,4
          PA(K) = PP(K)
 150    CONTINUE
 100  CONTINUE
      DELE = P2(4)-PP(4)
      IREJ = 0
C  consistency check
*     IF(ABS(P2(4)-PP(4))/MAX(P2(4),PP(4)).GT.DREL) THEN
*       PM2 = P2(4)**2-P2(1)**2-P2(2)**2-P2(3)**2
*       WRITE(6,'(/1X,A,2E12.5)')
*    &    'GETLTR:WARNING: INCONSISTENT ENERGIES',P2(4),PP(4)
*       WRITE(6,'(1X,A,2E12.4)') 'INPUT MASSES',PM1,PM2
*       WRITE(6,'(1X,A,4E12.4)') 'INPUT ',P1
*       WRITE(6,'(1X,A,4E12.4)') 'OUTPUT',P2
*       WRITE(6,'(1X,A,4E12.4)') 'INTERN',PP
*     ENDIF
      END
C
C
      SUBROUTINE ALTRA(GA,BGX,BGY,BGZ,PCX,PCY,PCZ,EC,P,PX,PY,PZ,E)
C*********************************************************************
C
C    ARBITRARY LORENTZ TRANSFORM
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      EP=PCX*BGX+PCY*BGY+PCZ*BGZ
      PE=EP/(GA+1.D0)+EC
      PX=PCX+BGX*PE
      PY=PCY+BGY*PE
      PZ=PCZ+BGZ*PE
      P=SQRT(PX*PX+PY*PY+PZ*PZ)
      E=GA*EC+EP
      END
C
C
      SUBROUTINE TRAFO(GAM,BGAM,CX,CY,CZ,COD,COF,SIF,P,ECM,
     &                 PL,CXL,CYL,CZL,EL)
C**********************************************************************
C
C     LORENTZ TRANSFORMATION INTO THE LAB - SYSTEM
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      SID=SQRT(1.D0-COD*COD)
      PLX=P*SID*COF
      PLY=P*SID*SIF
      PCMZ=P*COD
      PLZ=GAM*PCMZ+BGAM*ECM
      PL=SQRT(PLX*PLX+PLY*PLY+PLZ*PLZ)
      EL=GAM*ECM+BGAM*PCMZ
C     ROTATION INTO THE ORIGINAL DIRECTION
      COZ=PLZ/PL
      SIZ=SQRT(MAX(1.D0-COZ**2,0.D0))
      CALL DTRANS(CX,CY,CZ,COZ,SIZ,COF,SIF,CXL,CYL,CZL)
      END
C
C
      SUBROUTINE DTRANS(XO,YO,ZO,CDE,SDE,CFE,SFE,X,Y,Z)
C***********************************************************************
C
C     actualized version of Oct. 1992
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER(TINY=1.D-08,TINY2=1.D-30,ONE=1.D0)
C
      AX=ABS(XO)
      AY=ABS(YO)
      IF(AX.LT.AY) THEN
        AMAX=AY
        AMIN=AX
      ELSE
        AMAX=AX
        AMIN=AY
      ENDIF
      IF (ABS(XO)-TINY) 1,1,2
    1 IF (ABS(YO)-TINY) 3,3,2
C
    3 CONTINUE
C     WRITE(6,*)' DTRANS XO YO ZO =',XO,YO,ZO
      X=SDE*CFE
      Y=SDE*SFE
      Z=CDE*ZO
C     WRITE(6,*)' DTRANS X=SDE*CFE Y=SDE*SFE Z=CDE'
C     WRITE(6,*) X,Y,Z
      RETURN
C
    2 CONTINUE
      IF(AMAX.GT.TINY2) THEN
        AR=AMIN/AMAX
        AR=AR*AR
        A=AMAX*SQRT(ONE+AR)
      ELSE
C       WRITE(6,*)' DTRANS AMAX LE TINY2 '
        GOTO 3
      ENDIF
      XI=SDE*CFE
      YI=SDE*SFE
      ZI=CDE
      X=-YO*XI/A-ZO*XO*YI/A+XO*ZI
      Y=XO*XI/A-ZO*YO*YI/A+YO*ZI
      Z=A*YI+ZO*ZI
      END
C
C
      SUBROUTINE TRANI(XO,YO,ZO,CDE,SDE,CFE,SFE,X,Y,Z)
C**********************************************************************
C
C  rotation of coordinate frame (1) -fe rotation around z axis
C                               (2) -de rotation around y axis
C  (inverse rotation to TRANS)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      X= CDE*CFE*XO+CDE*SFE*YO-SDE*ZO
      Y=-SFE    *XO+CFE*    YO
      Z= SDE*CFE*XO+SDE*SFE*YO+CDE*ZO
      END
C
C
      SUBROUTINE TRANS(XO,YO,ZO,CDE,SDE,CFE,SFE,X,Y,Z)
C**********************************************************************
C
C  rotation of coordinate frame (1) de rotation around y axis
C                               (2) fe rotation around z axis
C  (inverse rotation to TRANI)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      X= CDE*CFE*XO-SFE*YO+SDE*CFE*ZO
      Y= CDE*SFE*XO+CFE*YO+SDE*SFE*ZO
      Z=-SDE    *XO       +CDE    *ZO
      END
C
C
      SUBROUTINE DATESS
C**********************************************************************
C
C     output of all particle and decay constants
C
C     ATTENTION: changed to work with new decay data (01/95)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      CHARACTER*8 ZKNAME,ANAME,Z
      COMMON/DECAYC/ ZKNAME(550),NZK(550,3),WT(550)
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     *,K1(187),K2(187)
      COMMON /HAMCIN/IAMCIN(187)
      DIMENSION ICHAR(187)
      EQUIVALENCE (ICH(1),ICHAR(1))
      DIMENSION Z(3)
C
      PRINT 102
  102 FORMAT(///' TABLE OF USED PARTICLES AND RESONANCES (I)',//
     *' I = NUMBER OF PARTICLE OR RESONANCE',/
     *' IAMCIN = PARTICLE DATA GROUP INDEX OF PARTICLE OR RESONANCE',/
     *' ANAME = NAME OF I'/,
     *' AM = MASS OF I  (GEV)',/
     *' GA = WIDTH OF I  (GEV)',/
     *' TAU = LIFE TIME OF I  (SEC.)',/
     *' ICH = ELECTRIC CHARGE OF I, IBAR = BARYONIC CHARGE OF I',/' ', '
     *K1 = FIRST DECAY CHANNEL NUMBER, K2 = LAST DECAY CHANNEL NUMBER OF
     *I')
      PRINT 101
  101 FORMAT(///'   I  PDG-I  ANAME     AM         GA         TAU
     &ICH IBAR  K1  K2'/)
      JOO=180
      DO 41 I=1,JOO
      PRINT 100,I,IAMCIN(I),ANAME(I),AM(I),GA(I),TAU(I),ICH(I),
     *            IBAR(I),K1(I),K2(I)
  100 FORMAT (1I4,I7,2X,A8,3E11.4,4I4)
   41 CONTINUE
      PRINT 92
   92 FORMAT(///' DECAY CHANNELS OF PARTICLES AND RESONANCES',//)
      PRINT 93
   93 FORMAT(' ANAME = PARTICLE AND RESONANCE NAME'/,
     *' DNAME = DECAY CHANNEL NAME'/,
     *' J = DECAY CHANNEL NUMBER'/,
     *' I = NUMBER OF DECAYING PARTICLE'/,
     *' WT = SUM OF DECAY CHANNEL WEIGHTS FROM K1(I) UP TO J'/,
     *' NZK = PROGRAM INTERNAL NUMBERS OF DECAY PRODUCTS')
      PRINT 94
   94 FORMAT(///'   I     J      ANAME       DNAME                DECAY
     *PRODUCTS            WT       NZK'/)
      DO 2 I=1,JOO
        IK1=K1(I)
        IK2=K2(I)
        IF (IK1.LE.0) GO TO 2
        WTSUM = 0.D0
        DO 3 IK=IK1,IK2
          WTSUM  = WTSUM+WT(IK)
          I1=NZK(IK,1)
          I2=NZK(IK,2)
          I3=NZK(IK,3)
          AMTEST=AM(I)
          IBTEST=IBAR(I)
          ICTEST=ICHAR(I)
          Z(1) = 'BLANC   '
          Z(2) = 'BLANC   '
          Z(3) = 'BLANC   '
          IF (I1.GT.0) THEN
            Z(1)=ANAME(I1)
            AMTEST = AMTEST-AM(I1)
            IBTEST = IBTEST-IBAR(I1)
            ICTEST = ICTEST-ICH(I1)
          ENDIF
          IF (I2.GT.0) THEN
            Z(2)=ANAME(I2)
            AMTEST = AMTEST-AM(I2)
            IBTEST = IBTEST-IBAR(I2)
            ICTEST = ICTEST-ICH(I2)
          ENDIF
          IF (I3.GT.0) THEN
            Z(3)=ANAME(I3)
            AMTEST = AMTEST-AM(I3)
            IBTEST = IBTEST-IBAR(I3)
            ICTEST = ICTEST-ICH(I3)
          ENDIF
      PRINT 91,I,IK,ANAME(I),ZKNAME(IK),(Z(J),J=1,3),WT(IK),I1,I2,I3
   91 FORMAT(2I5,' DECAY OF ',1A6,' (CHANNEL: ',1A6,' ) TO ',3(1A6,2X),
     *1F8.4,3I5)
      IF (AMTEST) 51,52,52
   51 MTEST=1
      GO TO 53
   52 MTEST=0
   53 CONTINUE
      IF (MTEST+IBTEST**2+ICTEST**2.NE.0) PRINT 909,AMTEST,IBTEST,ICTEST
  909 FORMAT (' DATESS:WARNING: DECAY VALUES (MA,BA,CH)',F10.5,2I6)
    3 CONTINUE
      IF(ABS(WTSUM-1.D0).GT.1.D-10)
     &  WRITE(6,'(1X,A,F10.7)') 'DATESS:ERROR: WEIGHT SUM',WTSUM
    2 CONTINUE
      RETURN
      END
C
C
      FUNCTION MCIHAD(MCIND)
C**********************************************************************
C
C    CALCULATION OF THE PARTICLE INDEX ACCORDING TO THE PDG PROPOSAL.
C    MCIHAD PARTICLE NUMBER AS IN DECAY, BAMJET, HADEVT, FLUKA ETC.
C    MCIND  PDG PARTICLE NUMBER
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HAMCIN/ IAMCIN(187)
C
C  search for hadron
      DO 10 I=1,187
        IH=I
        IF (IAMCIN(I).EQ.MCIND) GOTO 20
 10   CONTINUE
C  check parton?
      MCIHAD = 0
      MI = ABS(MCIND)
      IF((MI.LE.6).OR.(MI.EQ.21).OR.(MI.EQ.9)) RETURN
      IF((MI.EQ.1103).OR.(MI.EQ.2101).OR.(MI.EQ.2103)
     &   .OR.(MI.EQ.2203)) RETURN
      IF((MI.EQ.3101).OR.(MI.EQ.3103).OR.(MI.EQ.3201)
     &   .OR.(MI.EQ.3203).OR.(MI.EQ.3303)) RETURN
C  unknown particle
      IF(IDEB(71).GE.0) THEN
        WRITE(6,'(/1X,A,I8,/)')
     &    'MCIHAD:WARNING:NO HADRON FOUND FOR PARTICLE CODE: ',
     &    MCIND
      ENDIF
      IH = 99999
 20   CONTINUE
      MCIHAD=IH
C
      RETURN
      END
C
C
      FUNCTION MPDGHA(MCIND)
C**********************************************************************
C
C    CALCULATION OF THE PARTICLE INDEX ACCORDING TO THE PDG PROPOSAL.
C    MCIND  PARTICLE NUMBER AS IN DECAY, BAMJET, HADEVT, FLUKA ETC.
C    MPDGHA PDG PARTICLE NUMBER
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      COMMON /HAMCIN/ IAMCIN(187)
C
      MPDGHA=IAMCIN(MCIND)
      RETURN
      END
C
C
      FUNCTION ILU2PD(LUKF)
C**********************************************************************
C
C    conversion of JETSET KF code to PDG code
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PARAMETER (NTAB=10)
      DIMENSION LU2PD(2,NTAB)
      DATA LU2PD / 4232, 4322,
     &             4322, 4232,
     &             3212, 3122,
     &             3122, 3212,
     &            30553, 20553,
     &            30443, 20443,
     &            20443, 10443,
     &            10443, 0,
     &            511,   0,
     &            10551, 551 /
C
      DO 100 I=1,NTAB
        IF(LU2PD(1,I).EQ.LUKF) THEN
          ILU2PD=LU2PD(2,I)
          RETURN
        ENDIF
 100  CONTINUE
      ILU2PD=LUKF
      RETURN
      END
C
C
      FUNCTION IPD2LU(IPDG)
C**********************************************************************
C
C    conversion of PDG code to JETSET code
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PARAMETER (NTAB=8)
      DIMENSION LU2PD(2,NTAB)
      DATA LU2PD / 4232, 4322,
     &             4322, 4232,
     &             3212, 3122,
     &             3122, 3212,
     &            30553, 20553,
     &            30443, 20443,
     &            20443, 10443,
     &            10551, 551 /
C
      DO 100 I=1,NTAB
        IF(LU2PD(2,I).EQ.IPDG) THEN
          IPD2LU=LU2PD(1,I)
          RETURN
        ENDIF
 100  CONTINUE
      IPD2LU=IPDG
      RETURN
      END
C
C
      CHARACTER*8 FUNCTION PANAME(I,MODE)
C**********************************************************************
C
C     output of particel name
C
C     input:  MODE    0:   I treated as compress particle code
C                     1:   I treated as PDG number
C                     2:   I location in /HEPEVS/
C
C     index:  particle index (location) in common HEPEVS
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OTHREE =  ONE/THREE,
     &           TWOTHR =  TWO/THREE,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-05)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     &,K1(187),K2(187)
C
      CHARACTER*8 PNAME
      CHARACTER*1 QUANAM(6)
      DATA        QUANAM / 'd','u','s','c','b','t' /
C
      IF(MODE.EQ.0) THEN
        PANAME = ANAME(I)
      ELSE IF(MODE.EQ.1) THEN
        IDBAM = MCIHAD(I)
        IF(IDBAM.EQ.99999) THEN
          WRITE(6,'(1X,A,I7)') 'PANAME:WARNING:INVALID PARTICLE',I
          PANAME = '????????'
        ELSE
          PANAME = ANAME(IDBAM)
        ENDIF
      ELSE IF(MODE.EQ.2) THEN
        ID = IDHEP(I)
C  comment line?
        IF(ISTHEP(I).GE.11) GOTO 300
C  code in BAMJET?
        IF((IMPART(I).GT.0).AND.(IMPART(I).LE.187)) THEN
          PANAME = ANAME(IMPART(I))
        ELSE
          IF((ID.GE.-6).AND.(ID.LE.6).AND.(ID.NE.0)) THEN
            PNAME = QUANAM(ABS(ID))
          ELSE IF(ID.EQ.6666) THEN
            PNAME = 'resonan.'
          ELSE IF(ID.EQ.7777) THEN
            PNAME = 'mo.chain'
          ELSE IF(ID.EQ.8888) THEN
            PNAME = 'chain'
          ELSE IF((ID.EQ.9).OR.(ID.EQ.21)) THEN
            PNAME = 'gluon'
          ELSE IF(ABS(ID).EQ.1103) THEN
            PNAME = 'dd(1)'
          ELSE IF(ABS(ID).EQ.2101) THEN
            PNAME = 'ud(0)'
          ELSE IF(ABS(ID).EQ.2103) THEN
            PNAME = 'ud(1)'
          ELSE IF(ABS(ID).EQ.2203) THEN
            PNAME = 'uu(1)'
          ELSE IF(ABS(ID).EQ.3101) THEN
            PNAME = 'ds(0)'
          ELSE IF(ABS(ID).EQ.3103) THEN
            PNAME = 'ds(1)'
          ELSE IF(ABS(ID).EQ.3201) THEN
            PNAME = 'us(0)'
          ELSE IF(ABS(ID).EQ.3203) THEN
            PNAME = 'us(1)'
          ELSE IF(ABS(ID).EQ.3303) THEN
            PNAME = 'ss(1)'
          ELSE
            PNAME = '????'
          ENDIF
          IF(ID.LT.0) THEN
            PANAME = 'a-'//PNAME
          ELSE
            PANAME = PNAME
          ENDIF
        ENDIF
        RETURN
C  comment lines
 300    CONTINUE
        IF(ISTHEP(I).EQ.20) THEN
          PANAME = 'ini part'
        ELSE IF(ISTHEP(I).EQ.25) THEN
          PANAME = 'hard sca'
        ELSE IF(ISTHEP(I).EQ.30) THEN
          PANAME = 'diffract'
        ELSE IF(ISTHEP(I).EQ.35) THEN
          PANAME = 'qua-elas'
        ELSE IF(ISTHEP(I).EQ.40) THEN
          PANAME = 'cen.diff'
        ELSE
          PANAME = 'comment'
        ENDIF
      ELSE
        WRITE(6,'(1X,A,2I7)') 'PANAME:WARNING:INVALID MODE',I,MODE
        PANAME = '????????'
      ENDIF
      END
C
C
      INTEGER FUNCTION IPOBA3(INDEX,MODE)
C**********************************************************************
C
C     output of three times the baryon charge
C
C     index:  MODE
C             0   INDEX gives CPC particle number
C             1   INDEX gives PDG particle number
C             2   INDEX gives position of particle in HEPEVS
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OTHREE =  ONE/THREE,
     &           TWOTHR =  TWO/THREE,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-05)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     &,K1(187),K2(187)
C
      IPOBA3 = 0
C
      IF(MODE.EQ.0) THEN
        IDBAM = INDEX
        IDPDG = MPDGHA(INDEX)
        ILINE = 0
      ELSE IF(MODE.EQ.1) THEN
        IDPDG = INDEX
        IDBAM = 0
        ILINE = 0
      ELSE IF(MODE.EQ.2) THEN
        IDPDG = IDHEP(INDEX)
        IDBAM = IMPART(INDEX)
        ILINE = INDEX
      ELSE
        WRITE(6,'(1X,A,2I5)') 
     &    'IPOBA3:ERROR:INVALID ARGUMENTS',INDEX,MODE
        RETURN
      ENDIF
C
C  CPC particle
      IF((IDBAM.GT.0).AND.(IDBAM.LE.187)) THEN
        IPOBA3 = 3*IBAR(IDBAM)
C  parton
      ELSE IF((IDPDG.GE.-6).AND.(IDPDG.LE.6)) THEN
        IPOBA3 = SIGN(1,IDPDG)
      ELSE IF((IDPDG.EQ.21).OR.(IDPDG.EQ.9)) THEN
        IPOBA3 = ZERO
      ELSE IF((ABS(IDPDG).EQ.1103).OR.
     &        (ABS(IDPDG).EQ.2101).OR.
     &        (ABS(IDPDG).EQ.2103).OR.
     &        (ABS(IDPDG).EQ.2203).OR.
     &        (ABS(IDPDG).EQ.3101).OR.
     &        (ABS(IDPDG).EQ.3103).OR.
     &        (ABS(IDPDG).EQ.3201).OR.
     &        (ABS(IDPDG).EQ.3203).OR.
     &        (ABS(IDPDG).EQ.3303)) THEN
        IPOBA3 = SIGN(2,IDPDG)
      ELSE IF(ILINE.GT.0) THEN
C  comment line
        IF(ISTHEP(ILINE).GE.11) THEN
          RETURN
C  chain system
        ELSE IF((IDPDG.EQ.6666)
     &      .OR.(IDPDG.EQ.7777)
     &      .OR.(IDPDG.EQ.8888)) THEN
          IPOBA3 = ICOLOR(2,ILINE)
        ENDIF
      ELSE
        IDBAM = MCIHAD(IDPDG)
        IF(IDBAM.EQ.99999) THEN
          WRITE(6,'(/,1X,A,I6)') 'IPOBA3:WARNING:UNKNOWN PARTICLE',
     &      IDPDG
        ELSE
          IPOBA3 = 3*IBAR(IDBAM)
        ENDIF
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION POPAMA(INDEX,MODE)
C**********************************************************************
C
C     output of average particle mass
C
C     input:  MODE  -1   initialization
C                    0   INDEX gives CPC particle number
C                    1   INDEX gives PDG particle number
C                    2   INDEX gives position of particle in HEPEVS
C
C     output: average particle mass
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           TWOTHR =  TWO/THREE,
     &           OTHR   =  ONE/THREE,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-05)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     &,K1(187),K2(187)
      DIMENSION QUMASS(0:6)
**sr 29.04. commented to use dp-JETSET
      REAL ULMASS
**
C
      POPAMA = ZERO
      IF(MODE.EQ.0) THEN
        POPAMA = AM(INDEX)
      ELSE IF(MODE.EQ.1) THEN
        IF(ABS(INDEX).LE.6) THEN
          POPAMA = QUMASS(ABS(INDEX))
        ELSE
          POPAMA = DBLE(ULMASS(INDEX))
        ENDIF
      ELSE IF(MODE.EQ.2) THEN
        IF(ISTHEP(INDEX).GE.11) RETURN
        IF(IMPART(INDEX).EQ.0) THEN
          POPAMA = DBLE(ULMASS(IDHEP(INDEX)))
        ELSE
          POPAMA = AM(IMPART(INDEX))
        ENDIF
      ELSE IF(MODE.EQ.-1) THEN
        DO 100 I=1,6
          QUMASS(I) = DBLE(ULMASS(I))
 100    CONTINUE
        QUMASS(0) = ZERO
      ELSE
        WRITE(6,'(1X,A,2I5)') 
     &    'POPAMA:ERROR:INVALID ARGUMENTS',INDEX,MODE
        RETURN
      ENDIF
      END
C
C
      INTEGER FUNCTION IPOCH3(INDEX,MODE)
C**********************************************************************
C
C     output of three times the electric charge
C
C     index:  MODE
C             0   INDEX gives CPC particle number
C             1   INDEX gives PDG particle number
C             2   INDEX gives position of particle in HEPEVS

C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           TWOTHR =  TWO/THREE,
     &           OTHR   =  ONE/THREE,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-05)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     &,K1(187),K2(187)
C
      IPOCH3 = 0
C
      IF(MODE.EQ.0) THEN
        IDBAM = INDEX
        IDPDG = MPDGHA(INDEX)
        ILINE = 0
      ELSE IF(MODE.EQ.1) THEN
        IDPDG = INDEX
        IDBAM = 0
        ILINE = 0
      ELSE IF(MODE.EQ.2) THEN
        IDPDG = IDHEP(INDEX)
        IDBAM = IMPART(INDEX)
        ILINE = INDEX
      ELSE
        WRITE(6,'(1X,A,2I5)') 
     &    'IPOCH3:ERROR:INVALID ARGUMENTS',INDEX,MODE
        RETURN
      ENDIF
C
C  CPC particle
      IF((IDBAM.GT.0).AND.(IDBAM.LE.187)) THEN
        IPOCH3 = 3*ICH(IDBAM)
C  parton
      ELSE IF((IDPDG.GE.-6).AND.(IDPDG.LE.6)) THEN
        ICH1 = - MOD(ABS(IDPDG),2)
     &       + MOD(ABS(IDPDG)+1,2)*2
        IF(IDPDG.LT.0) ICH1 = -ICH1
        IPOCH3 = ICH1
      ELSE IF((IDPDG.EQ.21).OR.(IDPDG.EQ.9)) THEN
        IPOCH3 = ZERO
      ELSE IF((ABS(IDPDG).EQ.1103).OR.
     &        (ABS(IDPDG).EQ.3101).OR.
     &        (ABS(IDPDG).EQ.3103).OR.
     &        (ABS(IDPDG).EQ.3303)) THEN
        IPOCH3 = -SIGN(2,IDPDG)
      ELSE IF((ABS(IDPDG).EQ.2101).OR.
     &        (ABS(IDPDG).EQ.2103).OR.
     &        (ABS(IDPDG).EQ.3201).OR.
     &        (ABS(IDPDG).EQ.3203)) THEN
        IPOCH3 = SIGN(1,IDPDG)
      ELSE IF(ABS(IDPDG).EQ.2203) THEN
        IPOCH3 =  SIGN(4,IDPDG)
      ELSE IF(ILINE.GT.0) THEN
C  comment line
        IF(ISTHEP(ILINE).GE.11) THEN
          RETURN
C  chain system
        ELSE IF((IDPDG.EQ.6666)
     &      .OR.(IDPDG.EQ.7777)
     &      .OR.(IDPDG.EQ.8888)) THEN
            IPOCH3 = ICOLOR(1,ILINE)
        ENDIF
      ELSE
        IDBAM = MCIHAD(IDPDG)
        IF(IDBAM.EQ.99999) THEN
          WRITE(6,'(/,1X,A,I6)') 'IPOCH3:WARNING:UNKNOWN PARTICLE',
     &      IDPDG
        ELSE
          IPOCH3 = 3*ICH(IDBAM)
        ENDIF
      ENDIF
      END
C
C

      SUBROUTINE PODIMA(I,J,K,L,AM8,AM82A,AM82B,AM10,I8A,I8B)
C**********************************************************************
C
C     determine minimal masses corresponding to the input flavours
C     (diquark a-diquark chain system)
C
C     input: I,J,K,L   1,2,3,4 -->  u/d/s/c
C                      7,8,9,10     au/ad/as/ac
C                               (other flavours are not supported)
C
C     output: AM8      mass of two octett baryons
C             AM82A,B  mass of one octett and one decuplett baryon
C             AM10     mass of two decuplett baryon
C             I8A,I8B  BAMJET baryon index for AM8 mass configuration
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     *IBAR(187),K1(187),K2(187)
      COMMON/INPDAT/IMPS(6,6),IMVE(6,6),IB08(6,21),IB10(6,21),
     *IA08(6,21),IA10(6,21),A1,B1,B2,B3,LT,LB,ISU,IYQ,BET,AS,B8,AME,DIQ
C
      IF(I.GT.6) THEN
C  antibaryon
        II = I -6
        JJ = J -6
        KK = K
        LL = L
        IABAR = 1
      ELSE
C  baryon
        II = I
        JJ = J
        KK = K -6
        LL = L -6
        IABAR = 0
      ENDIF
C
C  find index
      CALL INDEX2(II,JJ,INDA)
      CALL INDEX2(KK,LL,INDB)
C
C  find possible two particle configuration (insert q qbar system)
      AM8   = 1000.D0
      AM82A = 1000.D0
      AM82B = 1000.D0
      AM10  = 1000.D0
      DO 100 IQ=1,4
        IF(IABAR.EQ.1) THEN
C  antibaryon A
          K8A  = IA08(IQ,INDA)
          K10A = IA10(IQ,INDA)
          K8B  = IB08(IQ,INDB)
          K10B = IB10(IQ,INDB)
        ELSE
C  baryon A
          K8A  = IB08(IQ,INDA)
          K10A = IB10(IQ,INDA)
          K8B  = IA08(IQ,INDB)
          K10B = IA10(IQ,INDB)
        ENDIF
        IF(K8A.LE.0) K8A=K10A
        IF(K8B.LE.0) K8B=K10B
        IF(AM8.GT.AM(K8A)+AM(K8B)) THEN
          I8A = K8A
          I8B = K8B
          AM8 = AM(K8A)+AM(K8B)
        ENDIF
        AM82A = MIN(AM82A,AM(K8A)+AM(K10B))
        AM82B = MIN(AM82B,AM(K8B)+AM(K10A))
        AM10  = MIN(AM10,AM(K10A)+AM(K10B))
 100  CONTINUE
      END
C
C
      SUBROUTINE POBAMA(I,J,K,AM8,AM82,AM10,AM102,I8,I10)
C**********************************************************************
C
C     determine baryon masses corresponding to the input flavours
C
C     input: I,J,K     1,2,3,4 -->  u/d/s/c
C                      7,8,9,10     au/ad/as/ac
C                               (other flavours are not supported)
C            attention: diquark will be built up from first two flavours
C
C
C     output: AM8      octett baryon mass
C             AM82     next possible two particle configuration
C                      (octett baryon and meson)
C             AM10     decuplett baryon mass
C             AM102    next possible two particle configuration
C                      (decuplett baryon and meson)
C             I8,I10   baryon numbers in BAMJET
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     *IBAR(187),K1(187),K2(187)
      COMMON/INPDAT/IMPS(6,6),IMVE(6,6),IB08(6,21),IB10(6,21),
     *IA08(6,21),IA10(6,21),A1,B1,B2,B3,LT,LB,ISU,IYQ,BET,AS,B8,AME,DIQ
C
      IF(I.GT.6) THEN
C  antibaryon
        II = I -6
        JJ = J -6
        KK = K -6
        IABAR = 1
      ELSE
C  baryon
        II = I
        JJ = J
        KK = K
        IABAR = 0
      ENDIF
C
C  find index
      CALL INDEX2(II,JJ,IND)
      IF(IABAR.EQ.1) THEN
C  antibaryon
        I8  = IA08(KK,IND)
        I10 = IA10(KK,IND)
      ELSE
C  baryon
        I8  = IB08(KK,IND)
        I10 = IB10(KK,IND)
      ENDIF
C  masses (if combination possible)
      IF (I8.LE.0) I8=I10
      AM8  = AM(I8)
      AM10 = AM(I10)
C
C  find next possible two particle configuration (insert q qbar system)
      AM82  = 1000.D0
      AM102 = 1000.D0
      DO 100 L=1,4
        IF(IABAR.EQ.1) THEN
C  antibaryon
          K8  = IA08(L,IND)
          K10 = IA10(L,IND)
          AMM = MIN(AM(IMPS(KK,L)),AM(IMVE(KK,L)))
        ELSE
C  baryon
          K8  = IB08(L,IND)
          K10 = IB10(L,IND)
          AMM = MIN(AM(IMPS(L,KK)),AM(IMVE(L,KK)))
        ENDIF
        IF (K8.LE.0) K8=K10
        AM82  = MIN(AM82, AM(K8) + AMM)
        AM102 = MIN(AM102, AM(K10) + AMM)
 100  CONTINUE
C  add phase space
      AM82  = AM82*1.5D0
      AM102 = AM102*1.1D0
      END
C
C
      SUBROUTINE POMEMA(I,J,AMPS,AMPS2,AMVE,AMVE2,IPS,IVE)
C**********************************************************************
C
C     determine meson masses corresponding to the input flavours
C
C     input: I,J     1,2,3,4 -->  u/d/s/c
C                    7,8,9,10     au/ad/as/ac
C                               (other flavours are not supported)
C
C     output: AMPS     pseudo scalar meson mass
C             AMPS2    next possible two particle configuration
C                      (two pseudo scalar  mesons)
C             AMVE     vector meson mass
C             AMVE2    next possible two particle configuration
C                      (two vector mesons)
C             IPS,IVE  meson numbers in BAMJET
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     *IBAR(187),K1(187),K2(187)
      COMMON/INPDAT/IMPS(6,6),IMVE(6,6),IB08(6,21),IB10(6,21),
     *IA08(6,21),IA10(6,21),A1,B1,B2,B3,LT,LB,ISU,IYQ,BET,AS,B8,AME,DIQ
C
      IF(I.GT.6) THEN
        II = I -6
        JJ = J
      ELSE IF(J.GT.6) THEN
        II = J -6
        JJ = I
      ELSE
        WRITE(6,'(/1X,A,2I4)')
     &    'POMEMA:ERROR: INCONSISTENT QUARK FLAVOURS ',I,J
        CALL POABRT
      ENDIF
C
C  find index
      IPS = IMPS(II,JJ)
      IVE = IMVE(II,JJ)
C  masses
      AMPS = AM(IPS)
      AMVE = AM(IVE)
C
C  find next possible two particle configuration with lowest mass
C  (insert q qbar system)
      AM2P = 1000.D0
      AM2V = 1000.D0
      DO 100 K=1,4
        AM2P = MIN(AM2P,AM(IMPS(II,K))+AM(IMPS(K,JJ)))
        AM2V = MIN(AM2V,AM(IMVE(II,K))+AM(IMVE(K,JJ)))
 100  CONTINUE
C  add phase space
      AMPS2 = AM2P*1.5D0
      AMVE2 = AM2V*1.1D0
      END
C
C
      SUBROUTINE PCODES
C**********************************************************************
C
C     debug particle codes of BAMJET and PDG,
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      CHARACTER*8 ANAME
      COMMON /PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     *IBAR(187),K1(187),K2(187)
C
      CHARACTER*16 JETNAM
      DIMENSION IETAB(10)
      DATA IETAB /2,-1,-1,2,0,0,-2,1,1,-2/
C
      WRITE(6,'(/1X,A)') 'PCODES:PARTICLE NUMBER DEBUG/CHECK'
C
C  determine quark contents
C  check meson table
      WRITE(6,'(3(/1X,A)/)') 'QUARK INDEX TABLE BAMJET',
     &                       '========================',
     &  '1 - u, 2 - d, 3 - s, c - 4, 7 - au, 8 - ad, 9 - as, 10 - ac'
      WRITE(6,'(/1X,A/)')
     &  '  BAM-ID  NAME      Q1  Q2  Q3    MASS    PDG-ID CHARGE'
      DO 100 I=1,4
        DO 200 K=7,10
          II = I
          KK = K
          ICHARG = (IETAB(II)+IETAB(KK))/3
          CALL POMEMA(II,KK,AMPS,AMPS2,AMVE,AMVE2,IPS,IVE)
          WRITE(6,'(1X,I6,3X,A8,3I4,E12.3,I6,I3)')
     &      IPS,ANAME(IPS),II,KK,0,AMPS,MPDGHA(IPS),ICH(IPS)
          IF(ICHARG.NE.ICH(IPS)) THEN
            WRITE(6,'(1X,A,/,1X,I4,A,2I5)')
     &        'PCODES:ERROR: INCONSISTENT CHARGE',IPS,ANAME(IPS),
     &        ICHARG,ICH(IPS)
          ENDIF
          WRITE(6,'(1X,I6,3X,A8,3I4,E12.3,I6,I3)')
     &      IVE,ANAME(IVE),II,KK,0,AMVE,MPDGHA(IVE),ICH(IVE)
          IF(ICHARG.NE.ICH(IVE)) THEN
            WRITE(6,'(1X,A,/,1X,I4,A,2I5)')
     &        'PCODES:ERROR: INCONSISTENT CHARGE',IVE,ANAME(IVE),
     &        ICHARG,ICH(IVE)
          ENDIF
 200    CONTINUE
 100  CONTINUE
C  check baryon table
      DO 300 I=1,4
        DO 400 J=I,4
          DO 500 K=J,4
            ICHARG = (IETAB(I)+IETAB(J)+IETAB(K))/3
            CALL POBAMA(I,J,K,AM8,AM82,AM10,AM102,I8,I10)
            WRITE(6,'(1X,I6,3X,A8,3I4,E12.3,I6,I3)')
     &        I8,ANAME(I8),I,J,K,AM8,MPDGHA(I8),ICH(I8)
            IF(ICHARG.NE.ICH(I8)) THEN
              WRITE(6,'(1X,A,/,1X,I4,A,2I5)')
     &          'PCODES:ERROR: INCONSISTENT CHARGE',I8,ANAME(I8),
     &          ICHARG,ICH(I8)
            ENDIF
            WRITE(6,'(1X,I6,3X,A8,3I4,E12.3,I6,I3)')
     &        I10,ANAME(I10),I,J,K,AM10,MPDGHA(I10),ICH(I10)
            IF(ICHARG.NE.ICH(I10)) THEN
              WRITE(6,'(1X,A,/,1X,I4,A,2I5)')
     &          'PCODES:ERROR: INCONSISTENT CHARGE',I10,ANAME(I10),
     &          ICHARG,ICH(I10)
            ENDIF
            II = I + 6
            JJ = J + 6
            KK = K + 6
            ICHARG = (IETAB(II)+IETAB(JJ)+IETAB(KK))/3
            CALL POBAMA(II,JJ,KK,AM8,AM82,AM10,AM102,I8,I10)
            WRITE(6,'(1X,I6,3X,A8,3I4,E12.3,I6,I3)')
     &        I8,ANAME(I8),II,JJ,KK,AM8,MPDGHA(I8),ICH(I8)
            IF(ICHARG.NE.ICH(I8)) THEN
              WRITE(6,'(1X,A,/,1X,I4,A,2I5)')
     &          'PCODES:ERROR: INCONSISTENT CHARGE',I8,ANAME(I8),
     &          ICHARG,ICH(I8)
            ENDIF
            WRITE(6,'(1X,I6,3X,A8,3I4,E12.3,I6,I3)')
     &        I10,ANAME(I10),II,JJ,KK,AM10,MPDGHA(I10),ICH(I10)
            IF(ICHARG.NE.ICH(I10)) THEN
              WRITE(6,'(1X,A,/,1X,I4,A,2I5)')
     &          'PCODES:ERROR: INCONSISTENT CHARGE',I10,ANAME(I10),
     &          ICHARG,ICH(I10)
            ENDIF
 500      CONTINUE
 400    CONTINUE
 300  CONTINUE
C
      WRITE(6,'(2(/1X,A)/)') 'PARTICLE TRANSLATION TABLE',
     &                       '=========================='
      WRITE(6,'(1X,A)')
     &  'QUARKS: 1 - d,  2 - u,  3 - s,  4 - c,  5 - b,  6 - t'
      WRITE(6,'(1X,A)')
     &  'PDG-ID   NAME  JETSET-NAME  BAM-ID  CHARGE   QUARKS'
C
      DO 600 I=1,187
        IPDG = MPDGHA(I)
        JETNAM = '              '
        IF(IPDG.NE.99999) CALL LUNAME(IPDG,JETNAM)
        WRITE(6,'(1X,I8,2X,A,2X,A16,I5,I5,3X,3I3)')
     &    IPDG,ANAME(I),JETNAM,I,ICH(I),(IPDGQU(K,I),K=1,3)
*       WRITE(6,'(1X,I5,2X,A,I8,E12.3,I4,3X,3I3)')
*    &    I,ANAME(I),MPDGHA(I),AM(I),ICH(I),(IPDGQU(K,I),K=1,3)
 600  CONTINUE
      WRITE(6,'(/)')
C
      END
C
C
      SUBROUTINE PRIBLO(IMODE)
C**********************************************************************
C
C     input:  IMODE     0  output for BLOCK DATA construction
C                       1  read file bblo.txt
C                          (overwrite BLOCK DATA statements)
C                       2  write file bblo.txt from current settings
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      CHARACTER*8 ANAME,ZKNAM
      COMMON /PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     *IBAR(187),K1(187),K2(187)
      COMMON /DECAYC/ZKNAM(550),NZK(550,3),WT(550)
**sr 29.04. commented to use dp-JETSET
      REAL  PMAS,PARF,VCKM
**
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C  LUND function
*     REAL ULMASS
*     CHARACTER*16 PDGNAM
C
C  read file bblo.txt
      IF(IMODE.EQ.1) THEN
        OPEN(3,STATUS='OLD',FILE='bblo.txt')
        DO 1 I=1,187
          READ(3,2)
     &      ANAME(I),AM(I),GA(I),TAU(I),ICH(I),IBAR(I),K1(I),K2(I)
    2     FORMAT (A8,3E15.7,4I6)
C
*         IPDG=MPDGHA(I)
*         LUKF=IPD2LU(IPDG)
*         LUKF=IPDG
*         IJET=LUCOMP(LUKF)
*         PDGNAM=' '
*         AML1 = 0.D0
*         AML2 = 0.D0
*         IF((IPDG.NE.99999).AND.(IPDG.NE.0)) THEN
*           CALL LUNAME(LUKF,PDGNAM)
*           IF(IJET.NE.0) THEN
*             AML1 =INT(1000*ULMASS(LUKF))/1000.
*             AML2 = PMAS(IJET,1)
*             AM(I) = AML2
*           ENDIF
*         ENDIF
*         WRITE(6,'(2X,I3,3X,I6,2X,A8,E10.4,2X,A16,2X,2E11.4)')
*    &      I,IPDG,ANAME(I),AM(I),PDGNAM,AML1,AML2
*         WRITE(6,2)
*    &      ANAME(I),AM(I),GA(I),TAU(I),ICH(I),IBAR(I),K1(I),K2(I)
C
    1   CONTINUE
        DO 3 I=1,550
          READ(3,4) ZKNAM(I),NZK(I,1),NZK(I,2),NZK(I,3),WT(I)
    4     FORMAT (A8,3I6,F15.8)
    3   CONTINUE
        CLOSE(3)
C
C  write file bblo.txt to read decay channels
      ELSE IF(IMODE.EQ.2) THEN
        OPEN(3,STATUS='UNKNOWN',FILE='bblo.txt')
        DO 10 I=1,187
          WRITE(3,2)
     &      ANAME(I),AM(I),GA(I),TAU(I),ICH(I),IBAR(I),K1(I),K2(I)
 10     CONTINUE
        DO 11 I=1,550
          WRITE(3,4) ZKNAM(I),NZK(I,1),NZK(I,2),NZK(I,3),WT(I)
 11     CONTINUE
        CLOSE(3)
C
C  write DATA statements
      ELSE IF(IMODE.EQ.0) THEN
        NMAX=185
        NSTEP=5
        NIST=187
        IK=18
        K0=1
        DO 100 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (BNAME(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,16A)') '&  ',
     &      ('''',ANAME(I),''',',I=II,II+NSTEP-1)
          IK=IK+1
 100    CONTINUE
        WRITE(6,'(5X,16A)') '&  ',
     &    ('''',ANAME(I),''',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=185
        NSTEP=5
        NIST=187
        IK=18
        K0=1
        DO 150 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (BM(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(E10.4,A))') '&  ',
     &      (AM(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 150    CONTINUE
        WRITE(6,'(5X,A,10(E10.4,A))') '&  ',
     &    (AM(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=185
        NSTEP=5
        NIST=187
        IK=18
        K0=1
        DO 200 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (BA(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(E10.4,A))') '&  ',
     &      (GA(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 200    CONTINUE
        WRITE(6,'(5X,A,10(E10.4,A))') '&  ',
     &    (GA(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=185
        NSTEP=5
        NIST=187
        IK=18
        K0=1
        DO 250 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (BAU(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(E10.4,A))') '&  ',
     &      (TAU(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 250    CONTINUE
        WRITE(6,'(5X,A,10(E10.4,A))') '&  ',
     &    (TAU(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=180
        NSTEP=10
        NIST=187
        IK=18
        K0=1
        DO 300 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (IBCH(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I3,A))') '&  ',
     &      (ICH(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 300    CONTINUE
        WRITE(6,'(5X,A,10(I3,A))') '&  ',
     &    (ICH(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=180
        NSTEP=10
        NIST=187
        IK=18
        K0=1
        DO 350 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (IBBAR(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I3,A))') '&  ',
     &      (IBAR(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 350    CONTINUE
        WRITE(6,'(5X,A,10(I3,A))') '&  ',
     &    (IBAR(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=180
        NSTEP=10
        NIST=187
        IK=18
        K0=1
        DO 400 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (KB1(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I4,A))') '&  ',
     &      (K1(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 400    CONTINUE
        WRITE(6,'(5X,A,10(I4,A))') '&  ',
     &    (K1(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=180
        NSTEP=10
        NIST=187
        IK=18
        K0=1
        DO 450 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (KB2(K),K=',K0,',',
     &        MIN(NIST,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I4,A))') '&  ',
     &      (K2(I),',',I=II,II+NSTEP-1)
          IK=IK+1
 450    CONTINUE
        WRITE(6,'(5X,A,10(I4,A))') '&  ',
     &    (K2(I),',',I=NMAX+1,NIST)
        WRITE(6,'(5X,A)') '&           /'
C
        NMAX=550
        NSTEP=5
        IK=18
        K0=1
        DO 500 II=1,NMAX,5
          IF(IK.EQ.18) THEN
            IF(II.NE.1) WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (ZBNAM(K),K=',K0,',',
     &        MIN(NMAX,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,16A)') '&  ',('''',ZKNAM(I),''',',I=II,II+4)
          IK=IK+1
 500    CONTINUE
        NSTEP=10
        IK=18
        K0=1
        DO 550 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (NBK1(K),K=',K0,',',
     &        MIN(NMAX,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I4,A))') '&  ',(NZK(I,1),',',I=II,II+9)
          IK=IK+1
 550    CONTINUE
        IK=18
        K0=1
        DO 600 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (NBK2(K),K=',K0,',',
     &        MIN(NMAX,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I4,A))') '&  ',(NZK(I,2),',',I=II,II+9)
          IK=IK+1
 600    CONTINUE
        IK=18
        K0=1
        DO 650 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (NBK3(K),K=',K0,',',
     &        MIN(NMAX,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(I4,A))') '&  ',(NZK(I,3),',',I=II,II+9)
          IK=IK+1
 650    CONTINUE
        NSTEP=5
        IK=18
        K0=1
        DO 700 II=1,NMAX,NSTEP
          IF(IK.EQ.18) THEN
            WRITE(6,'(5X,A)') '&           /'
            WRITE(6,'(5X,A,I3,A,I3,A)') ' DATA (BT(K),K=',K0,',',
     &        MIN(NMAX,K0-1+17*NSTEP),') /'
            K0=K0+17*NSTEP
            IK=1
          ENDIF
          WRITE(6,'(5X,A,10(E10.4,A))') '&  ',(WT(I),',',I=II,II+4)
          IK=IK+1
 700    CONTINUE
        WRITE(6,'(5X,A)') '&        /'
      ELSE
        WRITE(6,'(1X,A,I5)') 'PRIBLO:WARNING:UNSUPPORTED IMODE',IMODE
      ENDIF
      END
C
C
      SUBROUTINE POQCHK(MODE,IDEV)
C**********************************************************************
C
C     check quantum numbers of entries in COMMON HEPEVS
C           ( energy, momentum, charge, baryon number conservation)
C
C     input:    MODE    -1  check overall momentum conservation
C                           and perform detailed check in case of
C                           deviations
C                        1  test all branchings, mother-daughter
C                           relations
C
C     output:   IDEV     0  no deviations
C                        1  deviations found
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0)
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  global CMS energy
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
C  count number of errors to avoid disk overflow
      DATA IERR / 0 /
C
      IDEV = 0
C  conservation check suppressed
      IF((IPAMDL(15).EQ.0).OR.(IDEB(20).LE.-10)) RETURN
C  dtunuc call
      IF(IPAMDL(13).GT.0) RETURN
C
      DDREL = PARMDL(75)
      DDABS = PARMDL(76)
      IF(MODE.EQ.-1) GOTO 500
C
 50   CONTINUE
C
      I = 1
 100  CONTINUE
C  recognize only decayed particles as mothers
        IF(ISTHEP(I).EQ.2) THEN
C  search for other mother particles
          K = JDAHEP(1,I)
          K1 = JMOHEP(1,K)
          K2 = JMOHEP(2,K)
C  sum over mother particles
          ICH1 = IPOCH3(K1,2)
          IBA1 = IPOBA3(K1,2)
          EE1 = PHEP(4,K1)
          PX1 = PHEP(1,K1)
          PY1 = PHEP(2,K1)
          PZ1 = PHEP(3,K1)
          IF(K2.LT.0) THEN
            K2 = -K2
            IF((K1.GT.I).OR.(K2.LT.I)) THEN
              WRITE(6,'(/,1X,A,3I4)')
     &        'POQCHK:WARNING:INCONSISTENT MOTHER/DAUGHTER RELATION ',
     &        I,K1,K2
              CALL POPREV(-1)
            ENDIF
            DO 400 II=K1+1,K2
              IF(ABS(ISTHEP(II)).LE.2) THEN
                ICH1 = ICH1 + IPOCH3(II,2)
                IBA1 = IBA1 + IPOBA3(II,2)
                EE1 = EE1 + PHEP(4,II)
                PX1 = PX1 + PHEP(1,II)
                PY1 = PY1 + PHEP(2,II)
                PZ1 = PZ1 + PHEP(3,II)
              ENDIF
 400        CONTINUE
          ELSE IF((K2.GT.0).AND.(K2.NE.K1)) THEN
            ICH1 = ICH1 + IPOCH3(K2,2)
            IBA1 = IBA1 + IPOBA3(K2,2)
            EE1 = EE1 + PHEP(4,K2)
            PX1 = PX1 + PHEP(1,K2)
            PY1 = PY1 + PHEP(2,K2)
            PZ1 = PZ1 + PHEP(3,K2)
          ENDIF
C  sum over daughter particles
          ICH2 = ZERO
          IBA2 = ZERO
          EE2 = ZERO
          PX2 = ZERO
          PY2 = ZERO
          PZ2 = ZERO
          DO 200 II=JDAHEP(1,I),JDAHEP(2,I)
            IF(ABS(ISTHEP(II)).LE.2) THEN
              ICH2 = ICH2 + IPOCH3(II,2)
              IBA2 = IBA2 + IPOBA3(II,2)
              EE2 = EE2 + PHEP(4,II)
              PX2 = PX2 + PHEP(1,II)
              PY2 = PY2 + PHEP(2,II)
              PZ2 = PZ2 + PHEP(3,II)
            ENDIF
 200      CONTINUE
C  conservation check
          ESC = MAX(MAX(EE1,EE2)*DDREL,DDABS)
          IF(ABS(EE1-EE2).GT.ESC) THEN
            WRITE(6,'(1X,A,I3,2X,2E12.4)')
     &        'POQCHK:WARNING:ENERGY TEST PARTICLE NO ',I,EE1,EE2
            IDEV = 1
          ENDIF
          ESC = MAX(MAX(ABS(PX1),ABS(PX2))*DDREL,DDABS)
          IF(ABS(PX1-PX2).GT.ESC) THEN
            WRITE(6,'(1X,A,I3,2X,2E12.4)')
     &        'POQCHK:WARNING:MOMENTUM X TEST PARTICLE NO ',I,PX1,PX2
            IDEV = 1
          ENDIF
          ESC = MAX(MAX(ABS(PY1),ABS(PY2))*DDREL,DDABS)
          IF(ABS(PY1-PY2).GT.ESC) THEN
            WRITE(6,'(1X,A,I3,2X,2E12.4)')
     &        'POQCHK:WARNING:MOMENTUM Y TEST PARTICLE NO ',I,
     &        PY1,PY2
            IDEV = 1
          ENDIF
          ESC = MAX(MAX(ABS(PZ1),ABS(PZ2))*DDREL,DDABS)
          IF(ABS(PZ1-PZ2).GT.ESC) THEN
            WRITE(6,'(1X,A,I3,2X,2E12.3)')
     &        'POQCHK:WARNING:MOMENTUM Z TEST PARTICLE NO ',I,
     &        PZ1,PZ2
            IDEV = 1
          ENDIF
          IF(ICH1.NE.ICH2) THEN
            WRITE(6,'(1X,A,I3,2X,2I5)')
     &      'POQCHK:WARNING:ELE. CHARGE CONSERVATION PARTICLE NO ',I,
     &      ICH1,ICH2
            IDEV = 1
          ENDIF
          IF(IBA1.NE.IBA2) THEN
            WRITE(6,'(1X,A,I3,2X,2I5)')
     &      'POQCHK:WARNING:BAR. CHARGE CONSERVATION PARTICLE NO ',I,
     &      IBA1,IBA2
            IDEV = 1
          ENDIF
          IF(IDEB(20).GE.35) THEN
            WRITE(6,
     &        '(/,1X,A,A,2(2X,I4,A,I4),2(/,5X,A,4E13.4),/5X,A,4I5)')
     &      'POQCHK DIAGNOSTICS:',
     &      '(1.MOTHER/L.MOTHER,1.DAUGHTER/L.DAUGHTER):',
     &      K1,'/',K2,JDAHEP(1,I),'/',JDAHEP(2,I),
     &      'MOTHER MOMENTA   ',PX1,PY1,PZ1,EE1,
     &      'DAUGHTER MOMENTA ',PX2,PY2,PZ2,EE2,
     &      'CHARGE,BARYON NO ',ICH1,ICH2,IBA1,IBA2
          ENDIF
        ENDIF
        I = I+1
      IF(I.LE.NHEP) GOTO 100
C
      IERR = IERR+IDEV
C
C  write complete event in case of deviations
      IF((IDEB(20).GE.0).AND.(IDEV.NE.0)) THEN
        CALL POPREV(0)
        IF(ISTR.GT.0) THEN
          CALL POPRST
          IF(ISWMDL(6).GE.0) CALL LULIST(1)
        ENDIF
      ENDIF
C
C  stop after too many errors
      IF(IERR.GT.IPAMDL(179)) THEN
        WRITE(6,'(/1X,A,I6)')
     &    'POQCHK:ERROR:TOO MANY INCONSISTENCIES',IERR
        CALL POABRT
      ENDIF
C
      RETURN
C
C  overall check only (less time consuming)
C
 500  CONTINUE
      ICH1 = IPOCH3(1,2) + IPOCH3(2,2)
      IBA1 = IPOBA3(1,2) + IPOBA3(2,2)
      EE1 = PHEP(4,1) + PHEP(4,2)
      PX1 = PHEP(1,1) + PHEP(1,2)
      PY1 = PHEP(2,1) + PHEP(2,2)
      PZ1 = PHEP(3,1) + PHEP(3,2)
      ICH2 = ZERO
      IBA2 = ZERO
      EE2 = ZERO
      PX2 = ZERO
      PY2 = ZERO
      PZ2 = ZERO
C
      DO 300 K=3,NHEP
C  recognize only existing particles as possible daughters
        IF(ABS(ISTHEP(K)).EQ.1) THEN
          ICH2 = ICH2 + IPOCH3(K,2)
          IBA2 = IBA2 + IPOBA3(K,2)
          EE2 = EE2 + PHEP(4,K)
          PX2 = PX2 + PHEP(1,K)
          PY2 = PY2 + PHEP(2,K)
          PZ2 = PZ2 + PHEP(3,K)
        ENDIF
 300  CONTINUE
C  comparison
      ESC = ECM*DDREL
      IF(ABS(EE1-EE2).GT.ESC) THEN
        WRITE(6,'(1X,A,2X,2E15.5)')
     &    'POQCHK:WARNING: TOTAL ENERGY TEST',EE1,EE2
        IDEV = 1
      ENDIF
      IF(ABS(PX1-PX2).GT.ESC) THEN
        WRITE(6,'(1X,A,2X,2E15.5)')
     &    'POQCHK:WARNING: TOTAL MOMENTUM X TEST',PX1,PX2
        IDEV = 1
      ENDIF
      IF(ABS(PY1-PY2).GT.ESC) THEN
        WRITE(6,'(1X,A,2X,2E15.5)')
     &    'POQCHK:WARNING: TOTAL MOMENTUM Y TEST',PY1,PY2
        IDEV = 1
      ENDIF
      IF(ABS(PZ1-PZ2).GT.ESC) THEN
        WRITE(6,'(1X,A,2X,2E15.5)')
     &    'POQCHK:WARNING: TOTAL MOMENTUM Z TEST',PZ1,PZ2
        IDEV = 1
      ENDIF
      IF(ICH1.NE.ICH2) THEN
        WRITE(6,'(1X,A,2X,2I5)')
     &  'POQCHK:WARNING: ELE. CHARGE CONSERVATION',ICH1,ICH2
        IDEV = 1
      ENDIF
      IF(IBA1.NE.IBA2) THEN
        WRITE(6,'(1X,A,2X,2I5)')
     &  'POQCHK:WARNING: BAR. CHARGE CONSERVATION ',IBA1,IBA2
        IDEV = 1
      ENDIF
C
C  write complete event in case of deviations
      IF((IDEB(20).GE.0).AND.(IDEV.NE.0)) THEN
        DDREL = DDREL/2.D0
        DDABS = DDABS/2.D0
        WRITE(6,'(/1X,A,2E12.4)')
     &    'POQCHK:DEBUG:INCREASING PRECISION OF TESTS TO',DDREL,DDABS
        GOTO 50
      ENDIF
      END
C
C
      SUBROUTINE POABRT
C**********************************************************************
C
C     abort of MC event generation due to fatal error:
C     print all information of event generation and history
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( ZERO = 0.D0 )
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      COMMON /ABRHRD/ XH1(50),XH2(50),IJHI1(50),IJHI2(50),
     &                IJHF1(50),IJHF2(50),PHARD1(4,50),PHARD2(4,50),IPTM
      COMMON /ABRSOF/ XS1(50),XS2(50),IJSI1(50),IJSI2(50),
     &                PSOFT1(4,50),PSOFT2(4,50)
C
      PARAMETER ( MSCAHD = 50 )
      COMMON /HARSLT/ LSCAHD,LSC1HD,
     &                ETAHD(MSCAHD,2) ,PTHD(MSCAHD), Q2SCA(MSCAHD,2),
     &                XHD(MSCAHD,2)   ,VHD(MSCAHD) ,X0HD(MSCAHD,2),
     &                NINHD(MSCAHD,2) ,NOUTHD(MSCAHD,2),
     &                N0INHD(MSCAHD,2),NBRAHD(MSCAHD,2),NPROHD(MSCAHD)
C
      WRITE(6,'(/,/,1X,A,/,1X,A)') 'POABRT: STOP OF PROGRAM EXECUTION',
     &                             '================================='
      WRITE(6,'(/,1X,A,/,1X,A)') 'LISTING OF AVAILABLE DATA FOLLOWS:'
C
      CALL SETMDL(0,0,-2)
      CALL POPREV(-1)
      CALL ACTPDF(0,ZERO,-2)
C  print selected parton flavours
      WRITE(6,'(1X,A,I4)') 'SELECTED SOFT FLAVOURS: ',KSOFT
      DO 700 I=1,KSOFT
        WRITE(6,'(10X,2I5)') IJSI1(I),IJSI2(I)
 700  CONTINUE
      WRITE(6,'(1X,A,I4)') 'SELECTED HARD FLAVOURS: ',KHARD
      DO 750 I=1,KHARD
        WRITE(6,'(10X,A,I5)') 'PROCESS:',NPROHD(I)
        WRITE(6,'(10X,A,2I5,7X,A,2I5)') 'INITIAL:',IJHI1(I),IJHI2(I),
     &                                  'FINAL:',IJHF1(I),IJHF2(I)
 750  CONTINUE
C  print selected parton momenta
      WRITE(6,'(1X,A,I4)') 'SELECTED SOFT MOMENTA: ',KSOFT
      DO 300 I=1,KSOFT
        WRITE(6,'(10X,A,4E12.3)') 'PAR.1',(PSOFT1(II,I),II=1,4)
        WRITE(6,'(10X,A,4E12.3)') 'PAR.2',(PSOFT2(II,I),II=1,4)
 300  CONTINUE
      WRITE(6,'(1X,A,I4)') 'SELECTED HARD MOMENTA: ',KHARD
      DO 350 I=1,KHARD
        WRITE(6,'(10X,A,4E12.3)') 'PAR.1',(PHARD1(II,I),II=1,4)
        WRITE(6,'(10X,A,4E12.3)') 'PAR.2',(PHARD2(II,I),II=1,4)
 350  CONTINUE
C  print COMMON HEPEVS
      CALL POPREV(0)
C  fragmentation process
      IF(ISTR.GT.0) THEN
C  print COMMON STRING
        CALL POPRST
        IF(ISWMDL(6).GE.0) CALL LULIST(1)
      ENDIF
C  last message
      WRITE(6,'(//10X,A,//1X,A)')
     &  'POABRT:WARNING: EXECUTION TERMINATED DUE TO FATAL ERROR',
     &'*** SIMULATING DIVISION BY ZERO TO GET TRACEBACK INFROMATION ***'
      ISTR = 100/IPAMDL(100)
      END
C
C
      SUBROUTINE TRACE(ISTART,ISWI,LEVEL)
C**********************************************************************
C
C     trace program subroutines according to level,
C                          original output levels will be saved
C
C     input:   ISTART      first event to trace
C              ISWI        number of events to trace
C                                0   loop call, use old values
C                               -1   restore original output levels
C                                1   take level and wait for event
C              LEVEL       desired output level
C                                0   standard output
C                                5   cross sections, slopes etc.
C                               10   parameter of subroutines and
C                                    results
C                               20   maximal possible output
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      DIMENSION IMEM(NMAXD)
C
C  protect ISWI
      ISW = ISWI
 10   CONTINUE
      IF(ISW.EQ.0) THEN
        IF(KEVENT.LT.ION) THEN
          RETURN
        ELSE IF(KEVENT.EQ.ION) THEN
          WRITE(6,'(/,/,1X,A,/,1X,A,/,/)') 'TRACE SWITCHED ON',
     &                                   '================='
          DO 100 I=1,NMAXD
            IMEM(I) = IDEB(I)
            IDEB(I) = MAX(ILEVEL,IMEM(I))
 100      CONTINUE
        ELSE IF(KEVENT.EQ.IOFF) THEN
          WRITE(6,'(/,/,1X,A,/,1X,A,/,/)') 'TRACE SWITCHED OFF',
     &                                   '=================='
          DO 200 I=1,NMAXD
            IDEB(I) = IMEM(I)
 200      CONTINUE
        ENDIF
      ELSE IF(ISW.EQ.-1) THEN
        DO 300 I=1,NMAXD
          IDEB(I) = IMEM(I)
 300    CONTINUE
      ELSE
C  save information
        ION = ISTART
        IOFF = ISTART+ISW
        ILEVEL = LEVEL
      ENDIF
C  check coincidence
      IF(ISW.GT.0) THEN
        ISW=0
        ILEVEL = LEVEL
        GOTO 10
      ENDIF
      END
C
C
      SUBROUTINE POPRST
C**********************************************************************
C
C     print information of COMMON STRING
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      WRITE(6,'(/,1X,A,I5)') 'CHAINS: SOFT+HARD: ',ISTR
      WRITE(6,'(1X,A)') '=================='
      WRITE(6,'(5X,A,I10)') 'EVENT: ',KEVENT
C
      WRITE(6,'(/,1X,A/,1X,A)') 'COMMON STRING:',
     &  ' NOBAM  ID1  ID2  ID3  ID4  NPO1/2/3  MASS  BETX  BETY  BETZ'
      WRITE(6,'(1X,A)')
     &  ' ==========================================================='
      DO 800 I=1,ISTR
        WRITE(6,'(1X,8I4,4E11.3)')
     &         NCODE(I),IPAR1(I),IPAR2(I),IPAR3(I),IPAR4(I),NPOS(1,I),
     &         NPOS(2,I),NPOS(3,I),PHEP(5,NPOS(1,I)),
     &         GAMBET(1,I),GAMBET(2,I),GAMBET(3,I)
 800    CONTINUE
      END
C
C
      SUBROUTINE POPREV(NPART)
C**********************************************************************
C
C     print all information of event generation and history
C
C     input:        NPART  -1   minimal output: process IDs
C                           0   output of common HEPEVS
C                          >0   call   LULIST(NPART)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0)
C  global energy
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      CHARACTER*8 PANAME
C
      IF(NPART.GE.0) WRITE(6,'(/)')
      WRITE(6,'(1X,A,E12.3)')
     &  'POPREV:DEBUG:EVENT CHARACTERISTICS,CM-ENERGY',ECM
      CALL SETPAR(-2,IH,NPART,ZERO)
      WRITE(6,'(6X,A,A,/1X,I10,10I6)')
     &  'EV-CALL,ISPOM,IHPOM,ISREG,IHDIR,KSTRG,',
     &  'KHTRG,KSLOO,KHLOO,KSDPO,KHDPO',
     &  KEVENT,KSPOM,KHPOM,KSREG,KHDIR,KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,
     &  KHDPO
      WRITE(6,'(6X,A,I4,4I3)')
     &  'PROCESS-ID,IDNODF,IDIFF1,IDIFF2,IDDPOM',IPROCE,IDNODF,IDIFR1,
     &  IDIFR2,IDDPOM
C
      IF(NPART.LT.0) RETURN
C
      IF(NPART.GE.1) CALL POPRST
C
      WRITE(6,'(2(/1X,A))') 'COMMON HEPEVS',
     &                      '============='
      ICHAS  = 0
      IBARFS = 0
      WRITE(6,'(/1X,A,A,/,1X,A,A)')
     &  '   NO  IST  PDGID   NAME   MO-1 MO-2 DA-1 DA-2  CHA  BAR',
     &  '  IH1  IH2  CO1  CO2',
     &  '--------------------------------------------------------',
     &  '--------------------'
      DO 20 IH=1,NHEP
        CH = DBLE(IPOCH3(IH,2)/3.D0)
        BA = DBLE(IPOBA3(IH,2)/3.D0)
        WRITE(6,'(1X,2I5,I7,1X,A8,4I5,2F5.1,2I5,2I5)')
     &    IH,ISTHEP(IH),IDHEP(IH),PANAME(IH,2),
     &    JMOHEP(1,IH),JMOHEP(2,IH),JDAHEP(1,IH),JDAHEP(2,IH),
     &    CH,BA,IPHIST(1,IH),IPHIST(2,IH),
     &    ICOLOR(1,IH),ICOLOR(2,IH)
        IF(ABS(ISTHEP(IH)).EQ.1) THEN
          ICHAS  = ICHAS  + IPOCH3(IH,2)
          IBARFS = IBARFS + IPOBA3(IH,2)
        ENDIF
   20 CONTINUE
      WRITE(6,'(1X,2(A,I6))') '     SUMMED CHARGE:',ICHAS/3,
     &                        '     SUMMED BARYON:',IBARFS/3
C
      WRITE(6,7)
      PXS    = ZERO
      PYS    = ZERO
      PZS    = ZERO
      P0S    = ZERO
      DO 30 IN=1,NHEP
        WRITE(6,8) IN,ISTHEP(IN),IMPART(IN),PANAME(IN,2),
     &    (PHEP(J,IN),J=1,5),SQRT(PHEP(1,IN)**2+PHEP(2,IN)**2)
        IF(ABS(ISTHEP(IN)).EQ.1) THEN
          PXS = PXS + PHEP(1,IN)
          PYS = PYS + PHEP(2,IN)
          PZS = PZS + PHEP(3,IN)
          P0S = P0S + PHEP(4,IN)
        ENDIF
   30 CONTINUE
      AMFS = P0S**2-PXS**2-PYS**2-PZS**2
      AMFS = SIGN(SQRT(ABS(AMFS)),AMFS)
      WRITE(6,10) ' SUM:   ',PXS,PYS,PZS,P0S,AMFS
      WRITE(6,'(//)')
C
    5 FORMAT(2X,8H NUMBER ,8H STATUS ,8H IDENT. ,
     &  8H 1.MOTH.,8H 2.MOTH.,8H 1.DAUG.,8H L.DAUG.,
     &  8H CHARGE ,8H BARYON ,/)
    6 FORMAT(7I8,2F8.3)
    7 FORMAT(/,2X,' NR STAT CODE  NAME  X-MOMENTA',
     &  ' Y-MOMENTA Z-MOMENTA  ENERGY    MASS     PT',/,
     &         2X,'-------------------------------',
     &  '--------------------------------------------')
    8 FORMAT(I5,I4,I5,2X,A8,2F8.3,3F10.3,F8.3)
    9 FORMAT(I10,14X,5F10.3)
   10 FORMAT(15X,A8,1X,2F8.3,3F10.3)
C
      IF(NPART.GE.2) CALL LULIST(1)
      END
C
C
      SUBROUTINE LTRHEP(I1,I2,COD,SID,COF,SIF,GAM,BGX,BGY,BGZ)
C*******************************************************************
C
C     Lorentz transformation of entries I1 to I2 in
C     COMMON /HEPEVS/, comment entries not changed
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0,
     &           ONE  = 1.D0,
     &           DIFF = 0.001D0,
     &           EPS  = 1.D-5)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
      DO 100 I=I1,MIN(I2,NHEP)
        IF(ABS(ISTHEP(I)).GT.10) GOTO 90
        CALL TRANS(PHEP(1,I),PHEP(2,I),PHEP(3,I),COD,SID,COF,SIF,
     &    XX,YY,ZZ)
        EE=PHEP(4,I)
        CALL ALTRA(GAM,BGX,BGY,BGZ,XX,YY,ZZ,EE,PTOT,
     &    PHEP(1,I),PHEP(2,I),PHEP(3,I),PHEP(4,I))
 90     CONTINUE
 100  CONTINUE
C
C  debug precision
      IF(IDEB(70).LT.1) RETURN
      DO 200 I=I1,MIN(NHEP,I2)
        IF(ABS(ISTHEP(I)).GT.10) GOTO 190
        PMASS = PHEP(4,I)**2-PHEP(1,I)**2-PHEP(2,I)**2-PHEP(3,I)**2
        PMASS = SIGN(SQRT(ABS(PMASS)),PMASS)
        IF((ABS(PMASS-PHEP(5,I))/MAX(PHEP(5,I),ONE)).GT.DIFF) THEN
          WRITE(6,'(1X,A,I5,2E13.4)')
     &      'LTRHEP:WARNING: INCONSISTENT MASSES:',I,PMASS,PHEP(5,I)
        ENDIF
 190    CONTINUE
 200  CONTINUE
      END
C
C
      SUBROUTINE PECMS(ID,PMASS1,PMASS2,ECM,PP,EE)
C*******************************************************************
C
C     calculation of cms momentum and energy of massive particle
C     (ID=  1 using PMASS1,  2 using PMASS2)
C
C     output:  PP    cms momentum
C              EE    energy in CMS of particle ID
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0,
     &           TWO  = 2.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      S=ECM**2
      PM1 = SIGN(PMASS1**2,PMASS1)
      PM2 = SIGN(PMASS2**2,PMASS2)
      PP = SQRT(S**2 - TWO*PM1*S - TWO*PM2*S - TWO*PM1*PM2
     &          + PM1**2 + PM2**2)/(TWO*ECM)
C
      IF(ID.EQ.1) THEN
        EE = SQRT( PM1 + PP**2 )
      ELSE IF(ID.EQ.2) THEN
        EE = SQRT( PM2 + PP**2 )
      ELSE
        WRITE(6,'(/1X,A,I3,/)') 'PECMS:WARNING:UNSUPPORTED ID NUMBER:',
     &    ID
        EE = PP
      ENDIF
      END
C
C
      SUBROUTINE POFRIN
C********************************************************************
C
C     initialization of fragmentation packages BAMJET and DECAY
C     or JETSET 7.3
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
C  write block datas
*     CALL PRIBLO(0)
C  read file bblo.txt
*     CALL PRIBLO(1)
C
C  check particle codes and tables
      IF(IDEB(10).GE.15) CALL PCODES
C  write all data tables and decay modes
      IF(IDEB(10).GE.25) CALL DATESS
C
C  LUND JETSET 7.3 fragmentation
      CALL POLUIN(ISWMDL(6))
C  write parameter settings
      IF(IDEB(10).GE.5) CALL LULIST(13)
C  write all data tables and decay modes
      IF(IDEB(10).GE.25) CALL LULIST(12)
      END
C
C
      SUBROUTINE POLUIN(IDEFAU)
C***********************************************************************
C
C     initialization for JETSET call in DTUNUC 1.04 (J.R. 6/93)
C                      changed to work in PHOJET   (R.E. 1/94)
C
C     input:  IDEFAU    0  no hadronization at all
C                       1  do not touch any parameter of JETSET
C                       2  default parameters kept, decay length 10mm to
C                          define stable particles
C                       3  load tuned parameters for JETSET 7.3
C             neg. value:  prevent strange/charm hadrons from decaying
C
C     ATTENTION: single precision interface to JETSET
C                may be JETSET version dependent
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (EPS=1.D-10)
C
**sr 29.04. commented to use dp-JETSET
      REAL  P,V
**sr 29.04. LUJETS array-dimensions increased
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
C     COMMON/LUJETS/N,K(8000,5),P(8000,5),V(8000,5)
**sr 29.04. commented to use dp-JETSET
      REAL  PARU,PARJ
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
**sr 29.04. commented to use dp-JETSET
      REAL  PMAS,PARF,VCKM
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
**sr 29.04. commented to use dp-JETSET
      REAL  BRAT
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
C
      IDEFAB = ABS(IDEFAU)
C
      IF(IDEFAB.EQ.0) THEN
        WRITE(6,'(/1X,A)') 'POLUIN:DEBUG:HADRONIZATION SWITCHED OFF'
        RETURN
      ENDIF
C
C  declare stable particles
      IF(IDEFAB.GE.2) MSTJ(22) = 2
C
C  load optimized parameters
      IF(IDEFAB.GE.3) THEN
C  switch popcorn off
**sr comment this
        MSTJ(12) = 1
**
*       PARJ(19)=0.19
C  Lund a-parameter
C  (default=0.3)
        PARJ(41)=0.3
C  Lund b-parameter
C  (default=1.0)
        PARJ(42)=1.0
C  Lund sigma parameter in pt distribution
C  (default=0.36)
        PARJ(21)=0.36
      ENDIF
C
C  prevent particles decaying
      IF(IDEFAU.LT.0) THEN
C                 KOS
        KC=LUCOMP(310)
        MDCY(KC,1)=0
C                 PIO
        KC=LUCOMP(111)
        MDCY(KC,1)=0
C                 LAMBDA
        KC=LUCOMP(3122)
        MDCY(KC,1)=0
C                 ALAMBDA
        KC=LUCOMP(-3122)
        MDCY(KC,1)=0
C                 SIG+
        KC=LUCOMP(3222)
        MDCY(KC,1)=0
C                 ASIG+
        KC=LUCOMP(-3222)
        MDCY(KC,1)=0
C                 SIG-
        KC=LUCOMP(3112)
        MDCY(KC,1)=0
C                 ASIG-
        KC=LUCOMP(-3112)
        MDCY(KC,1)=0
C                 SIG0
        KC=LUCOMP(3212)
        MDCY(KC,1)=0
C                 ASIG0
        KC=LUCOMP(-3212)
        MDCY(KC,1)=0
C                 TET0
        KC=LUCOMP(3322)
        MDCY(KC,1)=0
C                 ATET0
        KC=LUCOMP(-3322)
        MDCY(KC,1)=0
C                 TET-
        KC=LUCOMP(3312)
        MDCY(KC,1)=0
C                 ATET-
        KC=LUCOMP(-3312)
        MDCY(KC,1)=0
C                 OMEGA-
        KC=LUCOMP(3334)
        MDCY(KC,1)=0
C                 AOMEGA-
        KC=LUCOMP(-3334)
        MDCY(KC,1)=0
C                 D+
        KC=LUCOMP(411)
        MDCY(KC,1)=0
C                 D-
        KC=LUCOMP(-411)
        MDCY(KC,1)=0
C                 D0
        KC=LUCOMP(421)
        MDCY(KC,1)=0
C                 A-D0
        KC=LUCOMP(-421)
        MDCY(KC,1)=0
C                 DS+
        KC=LUCOMP(431)
        MDCY(KC,1)=0
C                 A-DS+
        KC=LUCOMP(-431)
        MDCY(KC,1)=0
C                ETAC
        KC=LUCOMP(441)
        MDCY(KC,1)=0
C                LAMBDAC+
        KC=LUCOMP(4122)
        MDCY(KC,1)=0
C                A-LAMBDAC+
        KC=LUCOMP(-4122)
        MDCY(KC,1)=0
C                SIGMAC++
        KC=LUCOMP(4222)
        MDCY(KC,1)=0
C                SIGMAC+
        KC=LUCOMP(4212)
        MDCY(KC,1)=0
C                SIGMAC0
        KC=LUCOMP(4112)
        MDCY(KC,1)=0
C                A-SIGMAC++
        KC=LUCOMP(-4222)
        MDCY(KC,1)=0
C                A-SIGMAC+
        KC=LUCOMP(-4212)
        MDCY(KC,1)=0
C                A-SIGMAC0
        KC=LUCOMP(-4112)
        MDCY(KC,1)=0
C                KSIC+
        KC=LUCOMP(4232)
        MDCY(KC,1)=0
C                KSIC0
        KC=LUCOMP(4132)
        MDCY(KC,1)=0
C                A-KSIC+
        KC=LUCOMP(-4232)
        MDCY(KC,1)=0
C                A-KSIC0
        KC=LUCOMP(-4132)
        MDCY(KC,1)=0
      ENDIF
C
      WRITE(6,2355)
     &  IDEFAU,PARJ(2),MSTJ(12),PARJ(19),PARJ(41),PARJ(42),PARJ(21)
 2355 FORMAT( /' POLUIN:DEBUG:JETSET INITIALIZATION',I3/,
     &         ' ==================================',/,
     & '     PARJ( 2) strangeness suppression (0.3)  : ',F10.3,/,
     & '     MSTJ(12) popcorn (2)                    : ',I10,/,
     & '     PARJ(19) popcorn (1.0)                  : ',F10.3,/,
     & '     PARJ(41) Lund a (0.5)                   : ',F10.3,/,
     & '     PARJ(42) Lund b (0.9)                   : ',F10.3,/,
     & '     PARJ(21) sigma in pt distribution (0.35): ',F10.3,/)
*     DO 100 I=1,13
*       WRITE(6,'(1X,A,I3,E12.3)') 'PARU: ',I,PARU(I)
*100  CONTINUE
      END
C
C
      SUBROUTINE SETPAR(ISIDE,IDPDG,IDBAM,PVIR)
C**********************************************************************
C
C     assign a particle to side 1 or 2
C     (also special treatment for remnants)
C
C     input:    ISIDE      1,2  side selected for the particle
C                          -2   output of current settings
C               IDPDG      PDG number
C               IDBAM      CPC number
C                          0     CPC determination in subroutine
C                          -1    special particle remnant, IDPDG
C                                is the particle number the remnant
C                                corresponds to (see /HADVAL/)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (DEPS = 1.D-20,
     &           ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  global energy
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
C
      CHARACTER*8 ANAME
      COMMON /PART/ ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     &              IBAR(187),K1(187),K2(187)
C
      IF((ISIDE.EQ.1).OR.(ISIDE.EQ.2)) THEN
        IDBAMN = IDBAM
C  remnant?
        IF(IDBAM.EQ.-1) THEN
          IF(ISIDE.EQ.1) THEN
            IDBAMR = 186
            IDPDGR = 777
          ELSE
            IDBAMR = 187
            IDPDGR = 888
          ENDIF
          IDEQB(ISIDE) = MCIHAD(IDPDG)
          IDEQP(ISIDE) = IDPDG
          IDB = IDEQB(ISIDE)
          AM(IDBAMR)  = AM(IDB)
          GA(IDBAMR)  = GA(IDB)
          TAU(IDBAMR) = TAU(IDB)
          IF(IHFLS(ISIDE).EQ.1) THEN
            ICH(IDBAMR)  = ICH(IDB)
            IBAR(IDBAMR) = IBAR(IDB)
            K1(IDBAMR) = K1(IDB)
            K2(IDBAMR) = K2(IDB)
          ELSE
            ICH(IDBAMR)  = 0
            IBAR(IDBAMR) = 0
            K1(IDBAMR) = K1(111)
            K2(IDBAMR) = K2(111)
          ENDIF
          IFL1 = IHFLD(ISIDE,1)
          IFL2 = IHFLD(ISIDE,2)
          IFL3 = 0
          IF(IHFLS(ISIDE).EQ.1) THEN
            IF(ABS(IHFLD(ISIDE,1)).GT.1000) THEN
              IFL1 = IHFLD(ISIDE,1)/1000
              IFL2 = MOD(IHFLD(ISIDE,1)/100,10)
              IFL3 = IHFLD(ISIDE,2)
            ELSE IF(ABS(IHFLD(ISIDE,2)).GT.1000) THEN
              IFL1 = IHFLD(ISIDE,1)
              IFL2 = IHFLD(ISIDE,2)/1000
              IFL3 = MOD(IHFLD(ISIDE,2)/100,10)
            ENDIF
          ENDIF
          IFL1 = ICONV3(IFL1)
          IFL2 = ICONV3(IFL2)
          IFL3 = ICONV3(IFL3)
          IDUM = IBAMQU(-1,IDBAMR,IFL1)
          IDUM = IBAMQU(-2,IDBAMR,IFL2)
          IDUM = IBAMQU(-3,IDBAMR,IFL3)
          IDBAMN = IDBAMR
          IDPDGN = IDPDGR
          IF(IDEB(87).GE.5) THEN
            WRITE(6,'(1X,A,I2,/5X,A,I7,4I6)')
     &        'SETPAR:DEBUG:REMNANT ASSIGNMENT SIDE',ISIDE,
     &        'IDPDG,IFL1,2,3,IVAL',IDPDGN,IFL1,IFL2,IFL3,IHFLS(ISIDE)
          ENDIF
        ELSE IF(IDBAM.EQ.0) THEN
          IHFLS(ISIDE) = 1
          IHFLD(ISIDE,1) = 0
          IHFLD(ISIDE,2) = 0
          IDBAMN = MCIHAD(IDPDG)
          IDPDGN = IDPDG
        ENDIF
C initialize COMMON /GLOCMS/
        IFPAP(ISIDE) = IDPDGN
        IFPAB(ISIDE) = IDBAMN
        PMASS(ISIDE) = POPAMA(IDBAMN,0)
        IF(IFPAP(ISIDE).EQ.22) THEN
          PVIRT(ISIDE) = ABS(PVIR)
        ELSE
          PVIRT(ISIDE) = ZERO
        ENDIF
      ELSE IF(ISIDE.EQ.-2) THEN
C  output of current settings
        DO 100 I=1,2
          WRITE(6,'(1X,A,I2,2X,A,I7,I4,2E11.4)') 'SETPAR:DEBUG:SIDE',
     &      I,'IDPDG,IDBAM,PMASS,PVIRT',IFPAP(I),IFPAB(I),PMASS(I),
     &      PVIRT(I)
          IF((IFPAP(I).EQ.777).OR.(IFPAP(I).EQ.888)) THEN
            WRITE(6,'(5X,A,I7,I4,I2,3I5)')
     &        'REMNANT:IDPDG,IDBAM,IVAL,IFLA1,2',IDEQP(I),IDEQB(I),
     &        IHFLS(I),IHFLD(I,1),IHFLD(I,2)
          ENDIF
 100    CONTINUE
      ELSE
        WRITE(6,'(/1X,A,I8)') 'SETPAR:WARNING:INVALID ISIDE',ISIDE
      ENDIF
      END
      SUBROUTINE STDPAR(IJM1,IJM2,MSPOM,MSREG,MHPOM,MHDIR,IREJ)
C**********************************************************************
C
C     select the initial parton x-fractions and flavors and
C     the final parton momenta and flavours
C     for standard Pomeron/Reggeon cuts
C
C     input:   IJM1   index of mother particle 1 in HEPEVS
C              IJM2   index of mother particle 2 in HEPEVS
C              MSPOM  soft cut Pomerons
C              MHPOM  hard or semihard cut Pomerons
C              MSREG  soft cut Reggeons
C              MHDIR  direct hard processes
C
C              IJM1   -1    initialization of statistics
C                     -2    output of statistics
C
C     output:  partons are directly written to HEPEVS,HEPEVE
C
C          structure of COMMON /ABRHRD/
C               XH1(I),XH2(I):     x-values of initial partons
C               IJHI1(I),IJHI2(I): flavor of initial parton
C                                  0            gluon
C                                  1,2,3,4      sea quarks
C                                  7,8,9,10     valence quarks
C                                  negative     anti s or v quarks
C               IJHF1(I),IJHF2(I): flavor of final state partons
C               PHARD1(I,J),PHARD2(I,J): final part. momentum and energy
C                                J=1   PX
C                                 =2   PY
C                                 =3   PZ
C                                 =4   ENERGY
C
C          structure of COMMON /ABRSOF/
C               XS1(I),XS2(I):     x-values of initial partons
C               IJSI1(I),IJSI2(I): flavor of initial parton
C                                  0            gluon
C                                  1,2,3,4      sea quarks
C                                  7,8,9,10     valence quarks
C                                  negative     anti s or v quarks
C               IJSF1(I),IJSF2(I): flavor of final state partons
C               PSOFT1(I,J),PSOFT2(I,J): final part. momentum and energy
C                                J=1   PX
C                                 =2   PY
C                                 =3   PZ
C                                 =4   ENERGY
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
C  global CM system
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      COMMON /ABRHRD/ XH1(50),XH2(50),IJHI1(50),IJHI2(50),
     &                IJHF1(50),IJHF2(50),PHARD1(4,50),PHARD2(4,50),IPTM
      COMMON /ABRISR/ IFLISR(2,150),PHISR(2,4,150),IPOISR(2,2,50)
      COMMON /ABRSOF/ XS1(50),XS2(50),IJSI1(50),IJSI2(50),
     &                PSOFT1(4,50),PSOFT2(4,50)
      COMMON /HAEVNT/ PT1,PT2,NHARD,NTRY,IHARD,ITRY,IREJEV
      PARAMETER ( MAXPRO = 16 )
      PARAMETER ( MLINE = 400 , MSCAHD = 50 )
      COMMON /HAEVTR/ LINE,LIN,LREC1(MLINE),LREC2(MLINE),PREC(4,MLINE)
      COMMON /HARSLT/ LSCAHD,LSC1HD,
     &                ETAHD(MSCAHD,2) ,PTHD(MSCAHD), Q2SCA(MSCAHD,2),
     &                XHD(MSCAHD,2)   ,VHD(MSCAHD) ,X0HD(MSCAHD,2),
     &                NINHD(MSCAHD,2) ,NOUTHD(MSCAHD,2),
     &                N0INHD(MSCAHD,2),NBRAHD(MSCAHD,2),NPROHD(MSCAHD)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      COMMON /KINLIM/ ETAMI(2,15),ETAMA(2,15),XXMI(2,15),XXMA(2,15)
C
      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           RHOMAS =  0.766D0,
     &           DEPS   =  1.D-10,
     &           TINY   =  1.D-10)
C
*     CHARACTER*72 TITLE
      DIMENSION PC(4),IFLA(2),ICI(2,2)
*     DATA INIT / 0 /
C
      IF(IJM1.EQ.-1) THEN
        DO 116 I=1,15
          ETAMI(1,I) = 1.D10
          ETAMA(1,I) = -1.D10
          ETAMI(2,I) = 1.D10
          ETAMA(2,I) = -1.D10
          XXMI(1,I) = 1.D0
          XXMA(1,I) = 0.D0
          XXMI(2,I) = 1.D0
          XXMA(2,I) = 0.D0
 116    CONTINUE
        CALL POHSCA(IJM1,1)
        CALL POHCOL(IJM1,ZERO,0,0,0,0,0,0,0,0,0,0,0,0)
*       IF(INIT.EQ.0) THEN
*         CALL NEWHIS(ZERO,ONE,ZERO,PC,30,IHBA1)
*         CALL NEWHIS(ZERO,ONE,ZERO,PC,30,IHBA2)
*         CALL NEWHIS(ZERO,ONE,ZERO,PC,30,IHBB1)
*         CALL NEWHIS(ZERO,ONE,ZERO,PC,30,IHBB2)
*         INIT = 1
*       ENDIF

        RETURN
      ELSE IF(IJM1.EQ.-2) THEN
        IF(IDEB(23).GE.1) THEN
          WRITE(6,'(/1X,A)') 
     &      'KINEMATIC LIMITS PARTICLE C (ETAMIN,ETAMAX,XMIN,XMAX)'
          DO 117 I=1,15
            WRITE(6,'(5X,I3,4E13.5)') 
     &        I,ETAMI(1,I),ETAMA(1,I),XXMI(1,I),XXMA(1,I)
 117      CONTINUE
          WRITE(6,'(1X,A)') 
     &      'KINEMATIC LIMITS PARTICLE D (ETAMIN,ETAMAX,XMIN,XMAX)'
          DO 118 I=1,15
            WRITE(6,'(5X,I3,4E13.5)') 
     &        I,ETAMI(2,I),ETAMA(2,I),XXMI(2,I),XXMA(2,I)
 118      CONTINUE
        ENDIF
        CALL POHSCA(IJM1,1)
        CALL POHCOL(IJM1,ZERO,0,0,0,0,0,0,0,0,0,0,0,0)
*       TITLE = 'X DISTRIBUTION OF PARTON 1 FROM PARTICLE 1'
*       CALL OUTHIS(IHBA1,0,TITLE,ONE,0)
*       TITLE = 'X DISTRIBUTION OF PARTON 2 FROM PARTICLE 1'
*       CALL OUTHIS(IHBA2,0,TITLE,ONE,0)
*       TITLE = 'X DISTRIBUTION OF PARTON 1 FROM PARTICLE 2'
*       CALL OUTHIS(IHBB1,0,TITLE,ONE,0)
*       TITLE = 'X DISTRIBUTION OF PARTON 2 FROM PARTICLE 2'
*       CALL OUTHIS(IHBB2,0,TITLE,ONE,0)
        RETURN
      ENDIF
C  debug output
      IF(IDEB(23).GT.5) WRITE(6,221)
     &  IJM1,IJM2,MSPOM,MHPOM,MSREG,MHDIR
  221 FORMAT (' STDPAR:DEBUG:JM1,JM2,MSPOM,MHPOM,MSREG,MHDIR ',6I5)
C
C  get mother data (exchange if first particle is Pomeron)
      IF(IDHEP(IJM1).EQ.45) THEN
        JM1 = IJM2
        JM2 = IJM1
      ELSE
        JM1 = IJM1
        JM2 = IJM2
      ENDIF
      NPOSP(1) = JM1
      NPOSP(2) = JM2
      IDPDG1 = IDHEP(JM1)
      IDBAM1 = IMPART(JM1)
      IDPDG2 = IDHEP(JM2)
      IDBAM2 = IMPART(JM2)
C  VDM assumption
      DELMAS = ZERO
      IF(IDHEP(JM1).EQ.22) THEN
        PMASSP(1) = RHOMAS+DELMAS
        PVIRTP(1) = PHEP(5,JM1)**2
      ELSE
        PMASSP(1) = POPAMA(IDBAM1,0)+DELMAS
        PVIRTP(1) = ZERO
      ENDIF
      IF(IDHEP(JM2).EQ.22) THEN
        PMASSP(2) = RHOMAS+DELMAS
        PVIRTP(2) = PHEP(5,JM2)**2
      ELSE
        PMASSP(2) = POPAMA(IDBAM2,0)+DELMAS
        PVIRTP(2) = ZERO
      ENDIF
C  get Pomeron/Reggeon CM system
      PC(1) = PHEP(1,JM1)+PHEP(1,JM2)
      PC(2) = PHEP(2,JM1)+PHEP(2,JM2)
      PC(3) = PHEP(3,JM1)+PHEP(3,JM2)
      PC(4) = PHEP(4,JM1)+PHEP(4,JM2)
      SS = PC(4)**2-PC(1)**2-PC(2)**2-PC(3)**2
C
      IF(SS.LT.DEPS) THEN
        CALL POPREV(1)
        IREJ = 5
        GOTO 150
      ENDIF
C
      ECMP = SQRT(SS)
      IF(IDEB(23).GE.5) THEN
        WRITE(6,'(1X,A,2I7,E12.4)')
     &  'STDPAR:DEBUG:PARTICLES,AVAILABLE ENERGY:',
     &  IDHEP(JM1),IDHEP(JM2),ECMP
        IF(IDEB(23).GE.25) CALL POPREV(0)
      ENDIF
      DO 10 I=1,4
        GAMBEP(I) = PC(I)/ECMP
 10   CONTINUE
      CALL ALTRA(GAMBEP(4),-GAMBEP(1),-GAMBEP(2),-GAMBEP(3),
     &           PHEP(1,JM1),PHEP(2,JM1),PHEP(3,JM1),
     &           PHEP(4,JM1),PTOT1,PC(1),PC(2),PC(3),PC(4))
C  rotation angle: particle 1 moves along +z
      CODP= PC(3)/PTOT1
c
c correct for bug in dec alpha compiler
c that cant recognise zero when it sees it
c
      aleft = ONE-CODP
      aright = ONE+CODP
      if(aleft.lt.0)aleft=0
      if(aright.lt.0)aright=0
      SIDP= SQRT((aleft)*(aright))
      COFP=ONE
      SIFP=ZERO
      IF(PTOT1*SIDP.GT.DEPS) THEN
        COFP=PC(1)/(SIDP*PTOT1)
        SIFP=PC(2)/(SIDP*PTOT1)
        ANORF=SQRT(COFP*COFP+SIFP*SIFP)
        COFP=COFP/ANORF
        SIFP=SIFP/ANORF
      ENDIF
C  get CM momentum
      XM12 = PMASSP(1)**2
      XM22 = PMASSP(2)**2
      PCMP = XLAMB(SS,XM12,XM22)/(2.D0*ECMP)
      II = 0
C  particle combination
      IF(IDPDG2.EQ.IFPAP(2)) THEN
        IF(IDPDG1.EQ.IFPAP(1)) II = 1
      ELSE IF(IDPDG2.EQ.45) THEN
        IF(IDPDG1.EQ.IFPAP(1)) THEN
          II = 2
        ELSE IF(IDPDG1.EQ.IFPAP(2)) THEN
          II = 3
        ELSE IF(IDPDG1.EQ.45) THEN
          II = 4
        ENDIF
      ENDIF
      IF(II.EQ.0) THEN
        IF(ISWMDL(14).GT.0) THEN
          II = 1
        ELSE
          WRITE(6,'(/1X,A,2I8)')
     & 'STDPAR:ERROR:UNSUPPORTED PARTICLE COMBINATION:',IDPDG1,IDPDG2
          CALL POABRT
        ENDIF
      ENDIF
C  parton distribution functions
      IF((MHPOM+MHDIR).GT.0) THEN
        CALL ACTPDF(IDPDG1,PVIRTP(1),1)
        CALL ACTPDF(IDPDG2,PVIRTP(2),2)
        Q0SQR  = MAX(PDFQ2M(1),PDFQ2M(2))
      ENDIF
      CALL POHINT(II,ECMP,-1,MAXPRO,1,4,MSPOM+MHPOM+MHDIR)
C
      NTRY   = 10
      IREJ   = 0
C  first particle added to /HEPEVE/
      NLOR1 = NHEP+1
C
C  ---------------- direct processes -----------------
C
      IF(MHDIR.EQ.1) THEN
        CALL POHDIR(II,IVAL1,IVAL2,MSPAR1,MSPAR2,MHPAR1,MHPAR2,IREJ)
        IF(IREJ.EQ.50) RETURN
        IF(IREJ.NE.0) GOTO 150
        ICA1  = 0
        ICA2  = 0
        ICB1  = 0
        ICB2  = 0
        IPDF1 = 0
        IPDF2 = 0
C
C  QCD compton scattering
C ------------------------------
        IF(NPROHD(1).EQ.10) THEN
C  register hadron remnant
          CALL POHREM(JM2,JM1,-1,IVAL2,1,ICB1,ICB2,IUSED,IREJ)
          IPDF2 = 1000*IGRP(2)+ISET(2)
        ELSE IF(NPROHD(1).EQ.12) THEN
C  register hadron remnant
          CALL POHREM(JM1,JM2,1,IVAL1,1,ICA1,ICA2,IUSED,IREJ)
          IPDF1 = 1000*IGRP(1)+ISET(1)
C
C  photon gluon fusion
C ---------------------------
        ELSE IF(NPROHD(1).EQ.11) THEN
C  register hadron remnant
          CALL POHREM(JM2,JM1,-1,IVAL2,1,ICB1,ICB2,IUSED,IREJ)
          IPDF2 = 1000*IGRP(2)+ISET(2)
        ELSE IF(NPROHD(1).EQ.13) THEN
C  register hadron remnant
          CALL POHREM(JM1,JM2,1,IVAL1,1,ICA1,ICA2,IUSED,IREJ)
          IPDF1 = 1000*IGRP(1)+ISET(1)
C
C  double direct process (no remnant)
C ----------------------------
        ELSE IF(NPROHD(1).EQ.14) THEN
        ENDIF
C  write comments to HEPEVS
        CALL REGPAR(25,II,NPROHD(1),IDPDG1,IDPDG2,X0HD(1,1),X0HD(1,2),
     &    PTHD(1),VHD(1),N0INHD(1,1),N0INHD(1,2),ICONV1(NOUTHD(1,1)),
     &    ICONV1(NOUTHD(1,2)),IPOS,1)
        CALL REGPAR(20,ICONV1(N0INHD(1,1)),IPDF1,JM1,JM2,PREC(1,1),
     &    PREC(2,1),PREC(3,1),Q2SCA(1,1),100,NBRAHD(1,1),ICA1,ICA2,
     &    IPOS,1)
        CALL REGPAR(20,ICONV1(N0INHD(1,2)),IPDF2,JM1,JM2,PREC(1,2),
     &    PREC(2,2),PREC(3,2),Q2SCA(1,2),100,NBRAHD(1,2),ICA1,ICA2,
     &    IPOS2,1)
C  write final partons to HEPEVS
        IF((ISWMDL(8).GE.2).AND.(NPROHD(1).NE.14)) THEN
          ICI(1,1) = ICA1
          ICI(1,2) = ICA2
          ICI(2,1) = ICB1
          ICI(2,2) = ICB2
          I = 1
          IFLA(1) = IJHI1(I)
          IFLA(2) = IJHI2(I)
C  initial state radiation
          DO 130 K=1,2
            DO 135 IPA=IPOISR(K,2,I),IPOISR(K,1,I)+1,-1
              IFLB = IFLISR(K,IPA)
              IF(IFLA(K).EQ.0) THEN
                IF(IFLB.EQ.0) THEN
                  CALL SELCOL(ICI(K,1),ICI(K,2),IC1,IC2,
     &              ICI(K,1),ICI(K,2),3)
                ELSE IF(IFLB.GT.0) THEN
                  CALL SELCOL(ICI(K,1),ICI(K,2),IC1,IC2,
     &              ICI(K,1),ICI(K,2),4)
                ELSE
                  CALL SELCOL(ICI(K,1),ICI(K,2),ICI(K,1),ICI(K,2),
     &              IC1,IC2,4)
                ENDIF
              ELSE
                IF(IFLB.EQ.0) THEN
                  CALL SELCOL(ICI(K,1),ICI(K,2),ICI(K,1),ICI(K,2),
     &              IC1,IC2,2)
                ELSE
                  CALL SELCOL(ICI(K,1),ICI(K,2),IC1,IC2,
     &              ICI(K,1),ICI(K,2),2)
                ENDIF
              ENDIF
              IIFL = ICONV1(IFLB)
              CALL REGPAR(-1,IIFL,0,JM1,JM2,PHISR(K,1,IPA),
     &          PHISR(K,2,IPA),PHISR(K,3,IPA),PHISR(K,4,IPA),I*100+K,
     &          0,IC1,IC2,IPOS,1)
              CALL QUASTA(IIFL,IDHEP(JM1),5)
              IFLA(K) = IFLA(K)-IFLB
 135        CONTINUE
 130      CONTINUE
          CALL POHCOL(NPROHD(I),VHD(I),IFLA(1),ICI(1,1),ICI(1,2),
     &      IFLA(2),ICI(2,1),ICI(2,2),IJHF1(I),ICI(1,1),ICI(1,2),
     &      IJHF2(I),ICI(2,1),ICI(2,2))
          DO 140 K=1,2
            IPA = IPOISR(K,1,I)
            CALL REGPAR(-1,ICONV1(IFLISR(K,IPA)),0,JM1,JM2,
     &        PHISR(K,1,IPA),PHISR(K,2,IPA),PHISR(K,3,IPA),
     &        PHISR(K,4,IPA),-I*100,0,ICI(K,1),ICI(K,2),IPOS,1)
 140      CONTINUE
        ELSE
          CALL POHCOL(NPROHD(1),VHD(1),IJHI1(1),ICA1,ICA2,IJHI2(1),
     &      ICB1,ICB2,IJHF1(1),ICA1,ICA2,IJHF2(1),ICB1,ICB2)
          I = -1
          IF(ABS(IJHF1(1)).GT.12) I = 1
          CALL REGPAR(I,ICONV1(IJHF1(1)),0,JM1,JM2,PHARD1(1,1),
     &    PHARD1(2,1),PHARD1(3,1),PHARD1(4,1),-100,0,ICA1,ICA2,IPOS,1)
          CALL REGPAR(I,ICONV1(IJHF2(1)),0,JM1,JM2,PHARD2(1,1),
     &    PHARD2(2,1),PHARD2(3,1),PHARD2(4,1),-100,0,ICB1,ICB2,IPOS,1)
        ENDIF
C
        IF(ISWMDL(18).EQ.0) THEN
          IPOS2 = IPOS2-1
          CALL PARTPT(0,NLOR1,IPOS2,PTCUT(II),IREJ)
          IF(IREJ.NE.0) THEN
            IFAIL(26) = IFAIL(26) + 1
            GOTO 150
          ENDIF
        ENDIF
C
C  ----------------- resolved processes -------------------
C
C  single Reggeon exchange
C ----------------------------
      ELSE IF((MSREG.EQ.1).AND.(MHPOM+MSPOM.EQ.0)) THEN
C  flavours
        CALL REGFLA(JM1,JM2,IFL1,IFL2,IREJ)
        IF(IREJ.NE.0) THEN
          IFAIL(24) = IFAIL(24)+1
          GOTO 150
        ENDIF
        CALL QUASTA(IFL1,IDHEP(JM1),2)
        CALL QUASTA(IFL2,IDHEP(JM2),2)
C  cutoff / pt initialization
        CALL POMSCA(II,MSPOM,MHPOM,MSREG,IVAL1,IVAL2,MSPAR1,MSPAR2,
     &              MHPAR1,MHPAR2,IREJ)
C  colors
        CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
        IF(((ABS(IFL1).GT.6).AND.(IFL1.GT.0))
     &     .OR.((ABS(IFL1).LE.6).AND.(IFL1.LT.0))) THEN
          CALL SWAPI(ICA1,ICB1)
        ENDIF
        ECMH = ECMP/TWO
C  registration
	IF(IPAMDL(13).GT.0) THEN
          CALL REGPAR(-1,IFL1,0,JM1,JM2,ZERO,ZERO,ECMH*XPSUB,
     &               ECMH*XPSUB,-1,0,ICA1,0,IPOS1,1)
          CALL REGPAR(-1,IFL2,0,JM1,JM2,ZERO,ZERO,-ECMH*XTSUB,
     &               ECMH*XTSUB,-1,0,ICB1,0,IPOS2,1)
        ELSE
          CALL REGPAR(-1,IFL1,0,JM1,JM2,ZERO,ZERO,ECMH,ECMH,
     &      -1,0,ICA1,1,IPOS1,1)
          CALL REGPAR(-1,IFL2,0,JM1,JM2,ZERO,ZERO,-ECMH,ECMH,
     &      -1,0,ICB1,1,IPOS2,1)
        ENDIF
C  soft pt assignment
        IF(ISWMDL(18).EQ.0) THEN
          CALL PARTPT(0,IPOS1,IPOS2,PTCUT(II),IREJ)
          IF(IREJ.NE.0) THEN
            IFAIL(26) = IFAIL(26) + 1
            GOTO 150
          ENDIF
        ENDIF
C
C  multi Reggeon / Pomeron exchange
C----------------------------------------
      ELSE
C  parton configuration
        CALL POMSCA(II,MSPOM,MHPOM,MSREG,IVAL1,IVAL2,MSPAR1,MSPAR2,
     &              MHPAR1,MHPAR2,IREJ)
        IF(IREJ.EQ.50) RETURN
        IF(IREJ.NE.0) GOTO 150
C
C  register particles
C
C  register soft partons
        IF(IVAL1.NE.0) THEN
          IF(IVAL1.LT.0) THEN
            IND1 = 3
            IVAL1=-IVAL1
          ELSE
            IND1 = 2
          ENDIF
        ELSE IF(MSPOM.EQ.0) THEN
          IND1 = MSPAR1+1-MSREG
        ELSE
          IND1 = 1
        ENDIF
        IF(IVAL2.NE.0) THEN
          IF(IVAL2.LT.0) THEN
            IND2 = 3
            IVAL2=-IVAL2
          ELSE
            IND2 = 2
          ENDIF
        ELSE IF(MSPOM.EQ.0) THEN
          IND2 = MSPAR2+1-MSREG
        ELSE
          IND2 = 1
        ENDIF
C
C  soft Pomeron final states
C -----------------------------------
        K = MSPOM+MHPOM+MSREG
        DO 50 I=1,MSPOM
C
          CALL POSPOM(II,IND1,IND2,K,ISWAP,IREJ)
          IF(IREJ.NE.0) THEN
            IFAIL(26) = IFAIL(26) + 1
            GOTO 150
          ENDIF
C
 50     CONTINUE
C
C  soft Reggeon final states
C -----------------------------------------
        DO 75 I=1,MSREG
C  flavours
          CMASS1 = SQRT(XS1(IND1)*XS2(IND2)*ECMP)
          IF(DRNDM(CMASS1).LT.OHALF) THEN
            CALL SEAFLA(JM1,IFLA1,IFLB1,CMASS1)
          ELSE
            CALL SEAFLA(JM2,IFLA1,IFLB1,CMASS1)
          ENDIF
          CALL QUASTA(IFLA1,IDHEP(JM1),2)
          CALL QUASTA(IFLB1,IDHEP(JM2),2)
C  colors
          CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
          IF(((ABS(IFLA1).GT.6).AND.(IFLA1.GT.0))
     &      .OR.((ABS(IFLA1).LE.6).AND.(IFLA1.LT.0)))
     &      CALL SWAPI(ICA1,ICB1)
C  registration
          CALL REGPAR(-1,IFLA1,0,JM1,JM2,PSOFT1(1,IND1),PSOFT1(2,IND1),
     &      PSOFT1(3,IND1),PSOFT1(4,IND1),I,0,ICA1,ICA2,IPOS1,1)
          IND1 = IND1+1
          CALL REGPAR(-1,IFLB1,0,JM2,JM1,PSOFT2(1,IND2),PSOFT2(2,IND2),
     &      PSOFT2(3,IND2),PSOFT2(4,IND2),I,0,ICB1,ICB2,IPOS2,1)
          IND2 = IND2+1
C  soft pt assignment
          IF(ISWMDL(18).EQ.0) THEN
            CALL PARTPT(0,IPOS1,IPOS2,PTCUT(II),IREJ)
            IF(IREJ.NE.0) THEN
              IFAIL(26) = IFAIL(26) + 1
              GOTO 150
            ENDIF
          ENDIF
 75     CONTINUE
C
C  hard Pomeron final states
C ------------------------------------
        IND1 = MSPAR1
        IND2 = MSPAR2
C
        DO 100 I=1,MHPOM
          IF(IVAL1.EQ.I) THEN
            IVQ = 1
            IND = 1
          ELSE IF((MSPOM.EQ.0).AND.(I.EQ.1).AND.(IVAL1.EQ.0)) THEN
            IVQ = 0
            IND = 1
          ELSE
            IVQ = -1
            IND = IND1
          ENDIF
          CALL POHREM(JM1,JM2,I,IVQ,IND,ICA1,ICA2,IUSED,IREJ)
          IF(IVQ.LT.0) IND1 = IND1-IUSED
          IF(IVAL2.EQ.I) THEN
            IVQ = 1
            IND = 1
          ELSE IF((MSPOM.EQ.0).AND.(I.EQ.1).AND.(IVAL2.EQ.0)) THEN
            IVQ = 0
            IND = 1
          ELSE
            IVQ = -1
            IND = IND2
          ENDIF
          CALL POHREM(JM2,JM1,-I,IVQ,IND,ICB1,ICB2,IUSED,IREJ)
          IF(IVQ.LT.0) IND2 = IND2-IUSED
C
C  write comments to HEPEVS
          IFLI1 = ICONV1(N0INHD(I,1))
          IFLI2 = ICONV1(N0INHD(I,2))
          IFLO1 = ICONV1(NOUTHD(I,1))
          IFLO2 = ICONV1(NOUTHD(I,2))
          CALL QUASTA(IFLI1,IDPDG1,4)
          CALL QUASTA(IFLI2,IDPDG2,4)
          CALL QUASTA(IFLO1,0,6)
          CALL QUASTA(IFLO2,0,6)
          CALL REGPAR(25,II,NPROHD(I),IDPDG1,IDPDG2,X0HD(I,1),
     &      X0HD(I,2),PTHD(I),VHD(I),N0INHD(I,1),N0INHD(I,2),
     &      IFLO1,IFLO2,IPOS,1)
          I1 = 4*I-3
          IPDF = 1000*IGRP(1)+ISET(1)
          CALL REGPAR(20,IFLI1,IPDF,JM1,JM2,PREC(1,I1),
     &      PREC(2,I1),PREC(3,I1),Q2SCA(I,1),I*100,NBRAHD(I,1),
     &      ICA1,ICA2,IPOS,1)
          I1 = I1+1
          IPDF = 1000*IGRP(2)+ISET(2)
          CALL REGPAR(20,IFLI2,IPDF,JM2,JM1,PREC(1,I1),
     &      PREC(2,I1),PREC(3,I1),Q2SCA(I,2),I*100,NBRAHD(I,2),
     &      ICB1,ICB2,IPOS,1)
C  register hard scattered partons
          IF((I.EQ.1).AND.(ISWMDL(8).GE.2)) THEN
            ICI(1,1) = ICA1
            ICI(1,2) = ICA2
            ICI(2,1) = ICB1
            ICI(2,2) = ICB2
            IFLA(1) = IJHI1(I)
            IFLA(2) = IJHI2(I)
C  initial state radiation
            DO 230 K=1,2
              DO 235 IPA=IPOISR(K,2,I),IPOISR(K,1,I)+1,-1
                IFLB = IFLISR(K,IPA)
                IF(IFLA(K).EQ.0) THEN
                  IF(IFLB.EQ.0) THEN
                    CALL SELCOL(ICI(K,1),ICI(K,2),IC1,IC2,
     &                ICI(K,1),ICI(K,2),3)
                  ELSE IF(IFLB.GT.0) THEN
                    CALL SELCOL(ICI(K,1),ICI(K,2),IC1,IC2,
     &                ICI(K,1),ICI(K,2),4)
                  ELSE
                    CALL SELCOL(ICI(K,1),ICI(K,2),ICI(K,1),ICI(K,2),
     &                IC1,IC2,4)
                  ENDIF
                ELSE
                  IF(IFLB.EQ.0) THEN
                    CALL SELCOL(ICI(K,1),ICI(K,2),ICI(K,1),ICI(K,2),
     &                IC1,IC2,2)
                  ELSE
                    CALL SELCOL(ICI(K,1),ICI(K,2),IC1,IC2,
     &                ICI(K,1),ICI(K,2),2)
                  ENDIF
                ENDIF
                IIFL = ICONV1(IFLB)
                CALL REGPAR(-1,IIFL,0,JM1,JM2,PHISR(K,1,IPA),
     &            PHISR(K,2,IPA),PHISR(K,3,IPA),PHISR(K,4,IPA),I*100+K,
     &            0,IC1,IC2,IPOS,1)
                CALL QUASTA(IIFL,IDHEP(JM1),5)
                IFLA(K) = IFLA(K)-IFLB
 235          CONTINUE
 230        CONTINUE
            CALL POHCOL(NPROHD(I),VHD(I),IFLA(1),ICI(1,1),ICI(1,2),
     &        IFLA(2),ICI(2,1),ICI(2,2),IJHF1(I),ICI(1,1),ICI(1,2),
     &        IJHF2(I),ICI(2,1),ICI(2,2))
            DO 240 K=1,2
              IPA = IPOISR(K,1,I)
              CALL REGPAR(-1,ICONV1(IFLISR(K,IPA)),0,JM1,JM2,
     &          PHISR(K,1,IPA),PHISR(K,2,IPA),PHISR(K,3,IPA),
     &          PHISR(K,4,IPA),-I*100,0,ICI(K,1),ICI(K,2),IPOS,1)
 240        CONTINUE
          ELSE
            CALL POHCOL(NPROHD(I),VHD(I),IJHI1(I),ICA1,ICA2,IJHI2(I),
     &             ICB1,ICB2,IJHF1(I),ICA1,ICA2,IJHF2(I),ICB1,ICB2)
            CALL REGPAR(-1,ICONV1(IJHF1(I)),0,JM1,JM2,PHARD1(1,I),
     &        PHARD1(2,I),PHARD1(3,I),PHARD1(4,I),-I*100,0,ICA1,ICA2,
     &        IPOS,1)
            CALL REGPAR(-1,ICONV1(IJHF2(I)),0,JM1,JM2,PHARD2(1,I),
     &        PHARD2(2,I),PHARD2(3,I),PHARD2(4,I),-I*100,0,ICB1,ICB2,
     &        IPOS,1)
          ENDIF
 100    CONTINUE
C  end of resolved parton registration
      ENDIF
C
C  assign "soft" pt for soft (spectator) partons in hard Pomerons
      IF((MHDIR+MHPOM.GT.0).AND.(ISWMDL(24).GE.0)) THEN
        CALL PARTPT(1,NLOR1,NHEP,PTCUT(II),IREJ)
        IF(IREJ.NE.0) THEN
          IFAIL(25) = IFAIL(25) + 1
          GOTO 150
        ENDIF
      ENDIF
C
C  assign "soft" pt for partons in soft Pomerons
      IF((MHDIR.EQ.0).AND.(ISWMDL(18).EQ.1)) THEN
        CALL PARTPT(0,NLOR1,NHEP,PTCUT(II),IREJ)
        IF(IREJ.NE.0) THEN
          IFAIL(26) = IFAIL(26) + 1
          GOTO 150
        ENDIF
      ENDIF
C  boost back to lab frame
      CALL LTRHEP(NLOR1,NHEP,CODP,SIDP,COFP,SIFP,GAMBEP(4),GAMBEP(1),
     &            GAMBEP(2),GAMBEP(3))
      RETURN
C
C  rejection treatment
 150  CONTINUE
      IFAIL(3) = IFAIL(3)+1
      IF(IDEB(23).GT.2) WRITE(6,'(/1X,A,4I6)')
     &  'STDPAR:WARNING:REJECTION (MSPOM,MHPOM,MSREG,MHDIR)',
     &  MSPOM,MHPOM,MSREG,MHDIR
      RETURN
      END
C
C
      SUBROUTINE POHCOL(MSPR,V,IP1,ICA1,ICA2,IP2,ICB1,ICB2,
     &                  IP3,ICC1,ICC2,IP4,ICD1,ICD2)
C*********************************************************************
C
C     get color flow for hard resolved process
C
C     input:    IP1..4  flavour of partons (PDG convention)
C               V       parton subprocess Mandelstam variable  V = t/s
C                       (lightcone momenta assumed)
C               ICA,ICB color labels
C               MSPR    process number
C                       -1   initialization of statistics
C                       -2   output of statistics
C
C     output:   ICC,ICD color label of final partons
C
C     (it is possible to use the same variables for in and output)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER ( OHALF = 0.5D0,
     &            ZERO  = 0.D0,
     &            ONE   = 1.D0,
     &            TWO   = 2.D0,
     &            THREE = 3.D0 )
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      PARAMETER ( MAXPRO = 16 )
      CHARACTER*18 PROC
      COMMON /PEPROC/ PROC(0:MAXPRO)
C
      DIMENSION PC(3),ICONF(8,5)
C
C  initialization
      IF(MSPR.EQ.-1) THEN
        DO 200 I=1,8
          DO 210 K=1,5
            ICONF(I,K) = 0
 210      CONTINUE
 200    CONTINUE
        RETURN
C  output of statistics
      ELSE IF(MSPR.EQ.-2) THEN
        IF(IDEB(26).LT.1) RETURN
        WRITE(6,'(/1X,A,/1X,A)')
     &    'POHCOL:DEBUG:SAMPLED COLOR CONFIGURATIONS',
     &    '========================================='
        WRITE(6,'(6X,A)')
     &    'DIAGRAM                  COLOR CONFIGURATIONS (1-4)      SUM'
        DO 300 I=1,8
          DO 310 K=1,4
            ICONF(I,5) = ICONF(I,5) + ICONF(I,K)
 310      CONTINUE
          WRITE(6,'(2X,A,4I11,I12)') PROC(I),(ICONF(I,K),K=1,5)
 300    CONTINUE
        RETURN
      ENDIF
C
C  gluons:  first color positive
      IF((IP1.EQ.0).AND.(ICA1.LT.0)) THEN
        I = ICA2
        ICA2 = ICA1
        ICA1 = I
      ENDIF
      IF((IP2.EQ.0).AND.(ICB1.LT.0)) THEN
        I = ICB2
        ICB2 = ICB1
        ICB1 = I
      ENDIF
C
C  large Nc limit of all graphs
C  ------------------------------------------------------------
      IF((ISWMDL(11).EQ.1).AND.(MSPR.LT.10)) THEN
C  g g --> g g
        IF(MSPR.EQ.1) THEN
          IF(DRNDM(V).GT.OHALF) THEN
            IC1 = ICA1
            IC2 = ICB2
            IC3 = ICB1
            IC4 = ICA2
          ELSE
            IC1 = ICB1
            IC2 = ICA2
            IC3 = ICA1
            IC4 = ICB2
          ENDIF
          ICONF(MSPR,3) = ICONF(MSPR,3)+1
C  q q --> g g
        ELSE IF(MSPR.EQ.2) THEN
          CALL SELCOL(0,0,I1,K1,I2,K2,1)
          IF(ICA1.LT.0) THEN
            IC1 = I1
            IC2 = ICA1
            IC3 = ICB1
            IC4 = I2
            ICONF(MSPR,2) = ICONF(MSPR,2)+1
          ELSE
            IC1 = ICA1
            IC2 = I2
            IC3 = I1
            IC4 = ICB1
            ICONF(MSPR,1) = ICONF(MSPR,1)+1
          ENDIF
C  q g --> q g
        ELSE IF(MSPR.EQ.3) THEN
          IF(IP1.NE.0) THEN
            K = ICA1
            ICA1 = ICB1
            ICA2 = ICB2
            ICB1 = K
            ICB2 = 0
          ENDIF
          IF(ICB1*ICA1.GT.0) THEN
            K = ICA1
            ICA1 = ICA2
            ICA2 = K
          ENDIF
          IF(IP1.NE.0) THEN
            IC1 = ICA2
            IC2 = 0
            IC3 = ICA1
            IC4 = ICB1
            ICONF(MSPR,1) = ICONF(MSPR,1)+1
          ELSE
            IC1 = ICA1
            IC2 = ICB1
            IC3 = ICA2
            IC4 = 0
            ICONF(MSPR,2) = ICONF(MSPR,2)+1
          ENDIF
C  g g --> q q
        ELSE IF(MSPR.EQ.4) THEN
C  one closed color line necessary
          IC1 = ICA2
          IC2 = 0
          IC3 = ICB1
          IC4 = 0
          CALL POHCOR(-ICB2,ICA1)
          IF(IP3*IC1.LT.0) THEN
            I = IC1
            IC1 = IC3
            IC3 = I
          ENDIF
          ICONF(MSPR,2) = ICONF(MSPR,2)+1
C  q q --> q q
        ELSE IF((MSPR.EQ.5).OR.(MSPR.EQ.7).OR.(MSPR.EQ.8)) THEN
          IF(IP3*ICA1.LT.0) THEN
            IC1 = ICB1
            IC3 = ICA1
          ELSE
            IC1 = ICA1
            IC3 = ICB1
          ENDIF
          IC2 = 0
          IC4 = 0
          ICONF(MSPR,2) = ICONF(MSPR,2)+1
C  q q --> q q
        ELSE IF(MSPR.EQ.6) THEN
          IF(IP3*ICA1.LT.0) THEN
            IC1 = ICB1
            IC3 = ICA1
            ICONF(MSPR,1) = ICONF(MSPR,1)+1
          ELSE
            IC1 = ICA1
            IC3 = ICB1
            ICONF(MSPR,2) = ICONF(MSPR,2)+1
          ENDIF
          IC2 = 0
          IC4 = 0
C  unknown process
        ELSE
          WRITE(6,'(/1X,A,I3)')
     &      'POHCOL:ERROR:UNSUPPORTED PROCESS',MSPR
          CALL POABRT
        ENDIF
C
C  closer to real QCD
C  -----------------------------------------------------
      ELSE
        U = -(ONE+V)
C  g g --> g g
        IF(MSPR.EQ.1) THEN
          PC(1) = 1/V**2  +TWO/V    +THREE  +TWO*V    +V**2
          PC(2) = 1/U**2  +TWO/U    +THREE  +TWO*U    +U**2
          PC(3) = (V/U)**2+TWO*(V/U)+THREE  +TWO*(U/V)+(U/V)**2
          XI = (PC(1)+PC(2)+PC(3))*DRNDM(U)
          PCS = ZERO
          DO 100 I=1,3
            PCS = PCS+PC(I)
            IF(XI.LT.PCS) GOTO 110
 100      CONTINUE
 110      CONTINUE
          IF(I.EQ.1) THEN
            CALL SELCOL(0,0,I1,K1,I2,K2,1)
            IF(DRNDM(V).GT.OHALF) THEN
              IC1 = I1
              IC2 = ICA2
              IC3 = ICB1
              IC4 = I2
              CALL POHCOR(-ICB2,ICA1)
            ELSE
              IC1 = ICA1
              IC2 = I2
              IC3 = I1
              IC4 = ICB2
              CALL POHCOR(-ICB1,ICA2)
            ENDIF
          ELSE IF(I.EQ.2) THEN
            CALL SELCOL(0,0,I1,K1,I2,K2,1)
            IF(DRNDM(U).GT.OHALF) THEN
              IC1 = ICB1
              IC2 = I2
              IC3 = I1
              IC4 = ICA2
              CALL POHCOR(-ICB2,ICA1)
            ELSE
              IC1 = I1
              IC2 = ICB2
              IC3 = ICA1
              IC4 = I2
              CALL POHCOR(-ICB1,ICA2)
            ENDIF
          ELSE
            IF(DRNDM(V).GT.OHALF) THEN
              IC1 = ICB1
              IC2 = ICA2
              IC3 = ICA1
              IC4 = ICB2
            ELSE
              IC1 = ICA1
              IC2 = ICB2
              IC3 = ICB1
              IC4 = ICA2
            ENDIF
          ENDIF
          ICONF(MSPR,I) = ICONF(MSPR,I)+1
C  q q --> g g
        ELSE IF(MSPR.EQ.2) THEN
          PC(1) = U/V-TWO*U**2
          PC(2) = V/U-TWO*V**2
          CALL SELCOL(0,0,I1,K1,I2,K2,1)
          XI = (PC(1)+PC(2))*DRNDM(U)
          IF(XI.LT.PC(1)) THEN
            IF(ICA1.GT.0) THEN
              IC1 = ICA1
              IC2 = I2
              IC3 = I1
              IC4 = ICB1
              ICONF(MSPR,1) = ICONF(MSPR,1)+1
            ELSE
              IC1 = I1
              IC2 = ICA1
              IC3 = ICB1
              IC4 = I2
              ICONF(MSPR,2) = ICONF(MSPR,2)+1
            ENDIF
          ELSE
            IF(ICA1.GT.0) THEN
              IC1 = I1
              IC2 = ICB1
              IC3 = ICA1
              IC4 = I2
              ICONF(MSPR,3) = ICONF(MSPR,3)+1
            ELSE
              IC1 = ICB1
              IC2 = I2
              IC3 = I1
              IC4 = ICA1
              ICONF(MSPR,4) = ICONF(MSPR,4)+1
            ENDIF
          ENDIF
C  q g --> q g
        ELSE IF(MSPR.EQ.3) THEN
          IF(IP1.EQ.0) THEN
            K = ICB1
            ICB1 = ICA1
            ICB2 = ICA2
            ICA1 = K
            ICA2 = 0
          ENDIF
          PC(1) = TWO*(U/V)**2-U
          PC(2) = TWO/V**2-ONE/U
          XI = (PC(1)+PC(2))*DRNDM(V)
          IF(XI.LT.PC(1)) THEN
            CALL SELCOL(0,0,I1,K1,I2,K2,1)
            IF(ICA1.GT.0) THEN
              IC1 = I1
              IC2 = 0
              IC3 = ICB1
              IC4 = I2
              CALL POHCOR(-ICB2,ICA1)
              ICONF(MSPR,1) = ICONF(MSPR,1)+1
            ELSE
              IC1 = I2
              IC2 = 0
              IC3 = I1
              IC4 = ICB2
              CALL POHCOR(-ICB1,ICA1)
              ICONF(MSPR,2) = ICONF(MSPR,2)+1
            ENDIF
          ELSE
            IF(ICA1.GT.0) THEN
              IC1 = ICB1
              IC2 = 0
              IC3 = ICA1
              IC4 = ICB2
              ICONF(MSPR,3) = ICONF(MSPR,3)+1
            ELSE
              IC1 = ICB2
              IC2 = 0
              IC3 = ICB1
              IC4 = ICA1
              ICONF(MSPR,4) = ICONF(MSPR,4)+1
            ENDIF
          ENDIF
          IF(IP1.EQ.0) THEN
            I1 = IC1
            I2 = IC2
            IC1 = IC3
            IC2 = IC4
            IC3 = I1
            IC4 = I2
          ENDIF
C  g g --> q q
        ELSE IF(MSPR.EQ.4) THEN
          IC2 = 0
          IC4 = 0
C  one closed color line necessary
          PC(1) = U/V-TWO*U**2
          PC(2) = V/U-TWO*V**2
          XI = (PC(1)+PC(2))*DRNDM(U)
          IF(XI.LT.PC(1)) THEN
            IF(IP3.GT.0) THEN
              CALL POHCOR(-ICB1,ICA2)
              IC1 = ICA1
              IC3 = ICB2
              ICONF(MSPR,1) = ICONF(MSPR,1)+1
            ELSE
              CALL POHCOR(-ICB2,ICA1)
              IC1 = ICA2
              IC3 = ICB1
              ICONF(MSPR,2) = ICONF(MSPR,2)+1
            ENDIF
          ELSE
            IF(IP3.GT.0) THEN
              CALL POHCOR(-ICB2,ICA1)
              IC1 = ICB1
              IC3 = ICA2
              ICONF(MSPR,3) = ICONF(MSPR,3)+1
            ELSE
              CALL POHCOR(-ICB1,ICA2)
              IC1 = ICB2
              IC3 = ICA1
              ICONF(MSPR,4) = ICONF(MSPR,4)+1
            ENDIF
          ENDIF
C  q q --> q q
        ELSE IF((MSPR.EQ.5).OR.(MSPR.EQ.6).OR.
     &          ((MSPR.EQ.8).AND.(IP1*IP2.LT.0))) THEN
          IC2 = 0
          IC4 = 0
          PC(1) = (ONE+U**2)/V**2
          PC(2) = (V**2+U**2)
          XI = (PC(1)+PC(2))*DRNDM(V)
          IF(XI.LT.PC(1)) THEN
            CALL POHCOR(-ICB1,ICA1)
            CALL SELCOL(0,0,I1,K1,I2,K2,1)
            IF(IP3.GT.0) THEN
              IC1 = I1
              IC3 = I2
              ICONF(MSPR,1) = ICONF(MSPR,1)+1
            ELSE
              IC1 = I2
              IC3 = I1
              ICONF(MSPR,2) = ICONF(MSPR,2)+1
            ENDIF
          ELSE
            IF(IP3.GT.0) THEN
              IC1 = MAX(ICA1,ICB1)
              IC3 = MIN(ICA1,ICB1)
              ICONF(MSPR,3) = ICONF(MSPR,3)+1
            ELSE
              IC1 = MIN(ICA1,ICB1)
              IC3 = MAX(ICA1,ICB1)
              ICONF(MSPR,4) = ICONF(MSPR,4)+1
            ENDIF
          ENDIF
C  q q --> q q
        ELSE IF((MSPR.EQ.7).OR.(MSPR.EQ.8)) THEN
          IC2 = 0
          IC4 = 0
          PC(1) = (ONE+U**2)/V**2
          PC(2) = (ONE+V**2)/U**2
          XI = (PC(1)+PC(2))*DRNDM(U)
          IF(XI.LT.PC(1)) THEN
            IC1 = ICB1
            IC3 = ICA1
            ICONF(MSPR,1) = ICONF(MSPR,1)+1
          ELSE
            IC1 = ICA1
            IC3 = ICB1
            ICONF(MSPR,2) = ICONF(MSPR,2)+1
          ENDIF
C  gam q --> q g
        ELSE IF(MSPR.EQ.10) THEN
          CALL SELCOL(ICB1,ICB2,IC1,IC2,IC3,IC4,2)
          IF(IP3.EQ.0) THEN
            CALL SWAPI(IC1,IC3)
            CALL SWAPI(IC2,IC4)
          ENDIF
C  gam g --> q q
        ELSE IF(MSPR.EQ.11) THEN
          IC1 = ICB1
          IC2 = 0
          IC3 = ICB2
          IC4 = 0
          IF(IP3.LT.0) CALL SWAPI(IC1,IC3)
C  q gam --> q g
        ELSE IF(MSPR.EQ.12) THEN
          CALL SELCOL(ICA1,ICA2,IC1,IC2,IC3,IC4,2)
          IF(IP3.EQ.0) THEN
            CALL SWAPI(IC1,IC3)
            CALL SWAPI(IC2,IC4)
          ENDIF
C  g gam --> q q
        ELSE IF(MSPR.EQ.13) THEN
          IC1 = ICA1
          IC2 = 0
          IC3 = ICA2
          IC4 = 0
          IF(IP3.LT.0) CALL SWAPI(IC1,IC3)
        ELSE IF(MSPR.EQ.14) THEN
          IF(ABS(IP3).GT.12) THEN
            IC1 = 0
            IC2 = 0
            IC3 = 0
            IC4 = 0
          ELSE
            CALL SELCOL(ICA1,ICA2,IC1,IC2,IC3,IC4,1)
            IF(IP3.LT.0) CALL SWAPI(IC1,IC3)
          ENDIF
C  unknown process
        ELSE
          WRITE(6,'(/1X,A,I3)')
     &      'POHCOL:ERROR:UNSUPPORTED PROCESS',MSPR
          CALL POABRT
        ENDIF
      ENDIF
C
C  debug output
      IF(IDEB(26).GE.10) THEN
        WRITE(6,'(1X,A,I4,/,5X,A,3I5,2X,3I5)') 'POHCOL:DEBUG:PROCESS',
     &    MSPR,'INITIAL PARTONS AND COLORS',IP1,ICA1,ICA2,IP2,ICB1,ICB2
        WRITE(6,'(5X,A,3I5,2X,3I5)')
     &    'FINAL PARTONS AND COLORS',IP3,IC1,IC2,IP4,IC3,IC4
      ENDIF
      ICC1 = IC1
      ICC2 = IC2
      ICD1 = IC3
      ICD2 = IC4
      END
C
C
      SUBROUTINE POHCOR(ICOLD,ICNEW)
C*********************************************************************
C
C     correct color in HEPEVE to work in real QCD
C
C     input:    ICOLD   color to find
C               ICNEW   new color
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DO 100 I=NHEP,3,-1
        IF(ISTHEP(I).NE.-1) GOTO 99
        IF(ICOLOR(1,I).EQ.ICOLD) THEN
          ICOLOR(1,I) = ICNEW
          GOTO 200
        ELSE IF((ICOLOR(2,I).EQ.ICOLD).AND.(IDHEP(I).EQ.21)) THEN
          ICOLOR(2,I) = ICNEW
          GOTO 200
        ENDIF
 99     CONTINUE
 100  CONTINUE
        WRITE(6,'(1X,A,I3)') 'POHCOR:ERROR:MISSING COLOR',ICOLD
        CALL POABRT
 200  CONTINUE
      END
C
C
      SUBROUTINE POHREM(JM1,JM2,INDXH,IVAL,INDXS,IC1,IC2,IUSED,IREJ)
C*********************************************************************
C
C     get color structure for initial quark of hard scattering
C     and register hadron remnant
C
C     input:    JM1,2   index of mother particle in HEPEVS
C               INDXH   index of hard parton (hard pomeron number)
C                       positive for labels 1
C                       negative for labels 2
C               IVAL     1  hard valence parton
C                        0  hard sea parton connected by color flow with
C                           valence quarks
C                       -1  hard sea parton independent on valence quarks
C               INDXS   index of soft partons needed
C
C     output:   IC1,IC2 color label of initial parton
C               IUSED   number of soft X values used
C               IREJ
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      COMMON /ABRHRD/ XH1(50),XH2(50),IJHI1(50),IJHI2(50),
     &                IJHF1(50),IJHF2(50),PHARD1(4,50),PHARD2(4,50),IPTM
      COMMON /ABRSOF/ XS1(50),XS2(50),IJSI1(50),IJSI2(50),
     &                PSOFT1(4,50),PSOFT2(4,50)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
C
      IREJ = 0
      IF(INDXH.GT.0) THEN
        IJH = ICONV1(IJHI1(INDXH))
      ELSE
        IJH = ICONV1(IJHI2(-INDXH))
      ENDIF
C  direct process
      IUSED = 0
      IC1   = 0
      IC2   = 0
      IF((IJH.EQ.22).OR.(IJH.EQ.45)) RETURN
C
      IHP = 100*ABS(INDXH)
      IVSW = 1
      IF((IDHEP(JM1).EQ.22).OR.(IDHEP(JM1).EQ.45)) IVSW = 0
C
C  quark
        IF(IJH.NE.21) THEN
          IF(IVAL.EQ.1) THEN
C  valence quark engaged
            CALL PARTRE(JM1,IJH,IREM,IREJ)
            IF(IREJ.NE.0) THEN
              WRITE(6,'(/1X,A,2I6)')
     &          'POHREM:ERROR:INVALID VALENCE FLAVOURS JM,IFLA',JM1,IJH
              CALL POABRT
            ENDIF
            CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
            IF(((ABS(IREM).GT.6).AND.(IREM.GT.0))
     &         .OR.((ABS(IREM).LE.6).AND.(IREM.LT.0))) THEN
              I = ICA1
              ICA1 = ICB1
              ICB1 = I
            ENDIF
C  remainder of hadron
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS)
              P2 = PSOFT1(2,INDXS)
              P3 = PSOFT1(3,INDXS)
              P4 = PSOFT1(4,INDXS)
              IJSI1(INDXS) = IREM
            ELSE
              P1 = PSOFT2(1,INDXS)
              P2 = PSOFT2(2,INDXS)
              P3 = PSOFT2(3,INDXS)
              P4 = PSOFT2(4,INDXS)
              IJSI2(INDXS) = IREM
            ENDIF
C  registration
            CALL REGPAR(-1,IREM,0,JM1,JM2,P1,P2,P3,P4,
     &                    IHP,0,ICA1,IVSW,IPOS,1)
            CALL QUASTA(IREM,IDHEP(JM1),5)
            IUSED = 1
          ELSE IF(IVAL.EQ.0) THEN
C  sea quark and valence quarks engaged
            CALL VALFLA(JM1,IVFL1,IVFL2,PHEP(5,JM1))
            CALL QUASTA(IVFL1,IDHEP(JM1),1)
            CALL QUASTA(IVFL2,IDHEP(JM1),1)
            CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
            IF(DRNDM(P1).LT.OHALF) THEN
              CALL SELCOL(ICB1,ICB2,ICB1,ICB2,ICC1,ICC2,2)
            ELSE
              CALL SELCOL(ICA1,ICA2,ICA1,ICA2,ICC1,ICC2,2)
            ENDIF
            IF(((ABS(IVFL1).GT.6).AND.(IVFL1.GT.0))
     &         .OR.((ABS(IVFL1).LE.6).AND.(IVFL1.LT.0))) THEN
              I = ICA1
              ICA1 = ICB1
              ICB1 = I
            ENDIF
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS)
              P2 = PSOFT1(2,INDXS)
              P3 = PSOFT1(3,INDXS)
              P4 = PSOFT1(4,INDXS)
              IJSI1(INDXS) = IVFL1
            ELSE
              P1 = PSOFT2(1,INDXS)
              P2 = PSOFT2(2,INDXS)
              P3 = PSOFT2(3,INDXS)
              P4 = PSOFT2(4,INDXS)
              IJSI2(INDXS) = IVFL1
            ENDIF
C  registration
            CALL REGPAR(-1,IVFL1,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICA1,IVSW,IPOS,1)
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS+1)
              P2 = PSOFT1(2,INDXS+1)
              P3 = PSOFT1(3,INDXS+1)
              P4 = PSOFT1(4,INDXS+1)
              IJSI1(INDXS+1) = IVFL2
            ELSE
              P1 = PSOFT2(1,INDXS+1)
              P2 = PSOFT2(2,INDXS+1)
              P3 = PSOFT2(3,INDXS+1)
              P4 = PSOFT2(4,INDXS+1)
              IJSI2(INDXS+1) = IVFL2
            ENDIF
C  registration
            CALL REGPAR(-1,IVFL2,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICB1,IVSW,IPOS,1)
            IF(IJH.LT.0) THEN
              ICB1 = ICC2
              ICA1 = ICC1
            ELSE
              ICB1 = ICC1
              ICA1 = ICC2
            ENDIF
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS+2)
              P2 = PSOFT1(2,INDXS+2)
              P3 = PSOFT1(3,INDXS+2)
              P4 = PSOFT1(4,INDXS+2)
              IJSI1(INDXS+2) = -IJH
            ELSE
              P1 = PSOFT2(1,INDXS+2)
              P2 = PSOFT2(2,INDXS+2)
              P3 = PSOFT2(3,INDXS+2)
              P4 = PSOFT2(4,INDXS+2)
              IJSI2(INDXS+2) = -IJH
            ENDIF
C  registration
            CALL REGPAR(-1,-IJH,0,JM1,JM2,P1,P2,P3,P4,
     &                    IHP,0,ICA1,0,IPOS,1)
            IUSED = 3
          ELSE IF(IVAL.EQ.-1) THEN
C  only sea quark engaged
            CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
            IF(IJH.GT.0) THEN
              ICC1 = ICB1
              ICB1 = ICA1
              ICA1 = ICC1
            ENDIF
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS)
              P2 = PSOFT1(2,INDXS)
              P3 = PSOFT1(3,INDXS)
              P4 = PSOFT1(4,INDXS)
              IJSI1(INDXS) = -IJH
            ELSE
              P1 = PSOFT2(1,INDXS)
              P2 = PSOFT2(2,INDXS)
              P3 = PSOFT2(3,INDXS)
              P4 = PSOFT2(4,INDXS)
              IJSI2(INDXS) = -IJH
            ENDIF
C  registration
            CALL REGPAR(-1,-IJH,0,JM1,JM2,P1,P2,P3,P4,
     &                    IHP,0,ICA1,0,IPOS,1)
            CALL QUASTA(-IJH,IDHEP(JM1),5)
            IUSED = 1
          ELSE
            WRITE(6,'(1X,A,2I5)')
     &        'POHREM:ERROR:UNSUPPORTED COMBINATION OF IVAL,IJH',
     &        IVAL,IJH
            CALL POABRT
          ENDIF
C
          IC1 = ICB1
          IC2 = 0
C
C  gluon
        ELSE
          IF(IVAL.EQ.1) THEN
C  gluon from valence quarks
            CALL VALFLA(JM1,IFL1,IFL2,PHEP(5,JM1))
            CALL QUASTA(IFL1,IDHEP(JM1),1)
            CALL QUASTA(IFL2,IDHEP(JM1),1)
            CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
            IF(((ABS(IFL1).LE.6).AND.(IFL1.LT.0))
     &         .OR.((ABS(IFL1).GT.6).AND.(IFL1.GT.0))) THEN
              I = ICA1
              ICA1 = ICB1
              ICB1 = I
            ENDIF
            IF(DRNDM(P2).LT.OHALF) THEN
              CALL SELCOL(ICA1,ICA2,ICA1,ICA2,ICC1,ICC2,2)
            ELSE
              CALL SELCOL(ICB1,ICB2,ICB1,ICB2,ICC1,ICC2,2)
            ENDIF
C  remainder of hadron
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS)
              P2 = PSOFT1(2,INDXS)
              P3 = PSOFT1(3,INDXS)
              P4 = PSOFT1(4,INDXS)
              IJSI1(INDXS) = IFL1
            ELSE
              P1 = PSOFT2(1,INDXS)
              P2 = PSOFT2(2,INDXS)
              P3 = PSOFT2(3,INDXS)
              P4 = PSOFT2(4,INDXS)
              IJSI2(INDXS) = IFL1
            ENDIF
C  registration
            CALL REGPAR(-1,IFL1,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICA1,IVSW,IPOS,1)
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS+1)
              P2 = PSOFT1(2,INDXS+1)
              P3 = PSOFT1(3,INDXS+1)
              P4 = PSOFT1(4,INDXS+1)
              IJSI1(INDXS+1) = IFL2
            ELSE
              P1 = PSOFT2(1,INDXS+1)
              P2 = PSOFT2(2,INDXS+1)
              P3 = PSOFT2(3,INDXS+1)
              P4 = PSOFT2(4,INDXS+1)
              IJSI2(INDXS+1) = IFL2
            ENDIF
C  registration
            CALL REGPAR(-1,IFL2,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICB1,IVSW,IPOS,1)
C
            IUSED = 2
          ELSE IF(IVAL.EQ.0) THEN
C  gluon from sea quarks connected with valence quarks
            CALL VALFLA(JM1,IFL1,IFL2,PHEP(5,JM1))
            CALL QUASTA(IFL1,IDHEP(JM1),1)
            CALL QUASTA(IFL2,IDHEP(JM1),1)
            CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
            IF(((ABS(IFL1).LE.6).AND.(IFL1.LT.0))
     &         .OR.((ABS(IFL1).GT.6).AND.(IFL1.GT.0))) THEN
              I = ICA1
              ICA1 = ICB1
              ICB1 = I
            ENDIF
            IF(DRNDM(P3).LT.OHALF) THEN
              CALL SELCOL(ICA1,ICA2,ICA1,ICA2,ICC1,ICC2,2)
            ELSE
              CALL SELCOL(ICB1,ICB2,ICB1,ICB2,ICC1,ICC2,2)
            ENDIF
C  remainder of hadron
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS)
              P2 = PSOFT1(2,INDXS)
              P3 = PSOFT1(3,INDXS)
              P4 = PSOFT1(4,INDXS)
              IJSI1(INDXS) = IFL1
            ELSE
              P1 = PSOFT2(1,INDXS)
              P2 = PSOFT2(2,INDXS)
              P3 = PSOFT2(3,INDXS)
              P4 = PSOFT2(4,INDXS)
              IJSI2(INDXS) = IFL1
            ENDIF
C  registration
            CALL REGPAR(-1,IFL1,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICA1,IVSW,IPOS,1)
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS+1)
              P2 = PSOFT1(2,INDXS+1)
              P3 = PSOFT1(3,INDXS+1)
              P4 = PSOFT1(4,INDXS+1)
              IJSI1(INDXS+1) = IFL2
            ELSE
              P1 = PSOFT2(1,INDXS+1)
              P2 = PSOFT2(2,INDXS+1)
              P3 = PSOFT2(3,INDXS+1)
              P4 = PSOFT2(4,INDXS+1)
              IJSI2(INDXS+1) = IFL2
            ENDIF
C  registration
            CALL REGPAR(-1,IFL2,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICB1,IVSW,IPOS,1)
C  sea quark pair
            CALL SEAFLA(JM1,IFL1,IFL2,CUTMU(1))
            CALL QUASTA(IFL1,IDHEP(JM1),3)
            CALL QUASTA(IFL2,IDHEP(JM2),3)
            IF(ICC1.GT.0) THEN
              IFL1 = ABS(IFL1)
              IFL2 = -IFL1
            ELSE
              IFL1 = -ABS(IFL1)
              IFL2 = -IFL1
            ENDIF
            IF(DRNDM(P4).LT.OHALF) THEN
              ICB1 = ICC2
              CALL SELCOL(ICC1,0,ICA1,ICA2,ICC1,ICC2,2)
            ELSE
              ICA1 = ICC1
              CALL SELCOL(ICC2,0,ICB1,ICB2,ICC1,ICC2,2)
            ENDIF
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS+2)
              P2 = PSOFT1(2,INDXS+2)
              P3 = PSOFT1(3,INDXS+2)
              P4 = PSOFT1(4,INDXS+2)
              IJSI1(INDXS+2) = IFL1
            ELSE
              P1 = PSOFT2(1,INDXS+2)
              P2 = PSOFT2(2,INDXS+2)
              P3 = PSOFT2(3,INDXS+2)
              P4 = PSOFT2(4,INDXS+2)
              IJSI2(INDXS+2) = IFL1
            ENDIF
C  registration
            CALL REGPAR(-1,IFL1,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICA1,0,IPOS,1)
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS+3)
              P2 = PSOFT1(2,INDXS+3)
              P3 = PSOFT1(3,INDXS+3)
              P4 = PSOFT1(4,INDXS+3)
              IJSI1(INDXS+3) = IFL2
            ELSE
              P1 = PSOFT2(1,INDXS+3)
              P2 = PSOFT2(2,INDXS+3)
              P3 = PSOFT2(3,INDXS+3)
              P4 = PSOFT2(4,INDXS+3)
              IJSI2(INDXS+3) = IFL2
            ENDIF
C  registration
            CALL REGPAR(-1,IFL2,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICB1,0,IPOS,1)
C
            IUSED = 4
          ELSE IF(IVAL.EQ.-1) THEN
C  gluon from sea quarks
            CALL SEAFLA(JM1,IFL1,IFL2,CUTMU(1))
            CALL QUASTA(IFL1,IDHEP(JM1),3)
            CALL QUASTA(IFL2,IDHEP(JM2),3)
            CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
            IF(((ABS(IFL1).LE.6).AND.(IFL1.LT.0))
     &         .OR.((ABS(IFL1).GT.6).AND.(IFL1.GT.0))) THEN
              I = ICA1
              ICA1 = ICB1
              ICB1 = I
            ENDIF
            IF(DRNDM(P1).LT.OHALF) THEN
              CALL SELCOL(ICA1,ICA2,ICA1,ICA2,ICC1,ICC2,2)
            ELSE
              CALL SELCOL(ICB1,ICB2,ICB1,ICB2,ICC1,ICC2,2)
            ENDIF
C  remainder of hadron
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS)
              P2 = PSOFT1(2,INDXS)
              P3 = PSOFT1(3,INDXS)
              P4 = PSOFT1(4,INDXS)
              IJSI1(INDXS) = IFL1
            ELSE
              P1 = PSOFT2(1,INDXS)
              P2 = PSOFT2(2,INDXS)
              P3 = PSOFT2(3,INDXS)
              P4 = PSOFT2(4,INDXS)
              IJSI2(INDXS) = IFL1
            ENDIF
C  registration
            CALL REGPAR(-1,IFL1,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICA1,ICA2,IPOS,1)
C  remainder of sea
            IF(INDXH.GT.0) THEN
              P1 = PSOFT1(1,INDXS-1)
              P2 = PSOFT1(2,INDXS-1)
              P3 = PSOFT1(3,INDXS-1)
              P4 = PSOFT1(4,INDXS-1)
              IJSI1(INDXS-1) = IFL2
            ELSE
              P1 = PSOFT2(1,INDXS-1)
              P2 = PSOFT2(2,INDXS-1)
              P3 = PSOFT2(3,INDXS-1)
              P4 = PSOFT2(4,INDXS-1)
              IJSI2(INDXS-1) = IFL2
            ENDIF
C  registration
            CALL REGPAR(-1,IFL2,0,JM1,JM2,P1,P2,P3,P4,
     &                  IHP,0,ICB1,ICB2,IPOS,1)
C
            IUSED = 2
          ELSE
            WRITE(6,'(1X,A,2I5)')
     &        'POHREM:ERROR:UNSUPPORTED COMBINATION OF IVAL,IJH',
     &        IVAL,IJH
            CALL POABRT
          ENDIF
          IC1 = ICC1
          IC2 = ICC2
        ENDIF
      END
C
C
      SUBROUTINE POHDIR(II,IVAL1,IVAL2,MSPAR1,MSPAR2,MHPAR1,MHPAR2,
     &                  IREJ)
C**********************************************************************
C
C     parton orientated formulation of direct scattering processes
C
C     input:
C
C     output:   II        particle combination (1..4)
C               IVAL1,2   0 no valence quarks engaged
C                         1 valence quarks engaged
C               MSPAR1,2  number of realized soft partons
C               MHPAR1,2  number of realized hard partons
C               IREJ      1 failure
C                         0 success
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /HAEVNT/ PT1,PT2,NHARD,NTRY,IHARD,ITRY,IREJEV
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /HAEVTR/ LINE,LIN,LREC1(MLINE),LREC2(MLINE),PREC(4,MLINE)
      COMMON /DGLAPP/ Q2MISR(2),PMISR(2),ZMISR(2),AL2ISR(2),NFSISR
      COMMON /ABRHRD/ XH1(50),XH2(50),IJHI1(50),IJHI2(50),
     &                IJHF1(50),IJHF2(50),PHARD1(4,50),PHARD2(4,50),IPTM
      COMMON /ABRSOF/ XS1(50),XS2(50),IJSI1(50),IJSI2(50),
     &                PSOFT1(4,50),PSOFT2(4,50)
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      COMMON /HARSLT/ LSCAHD,LSC1HD,
     &                ETAHD(MSCAHD,2) ,PTHD(MSCAHD), Q2SCA(MSCAHD,2),
     &                XHD(MSCAHD,2)   ,VHD(MSCAHD) ,X0HD(MSCAHD,2),
     &                NINHD(MSCAHD,2) ,NOUTHD(MSCAHD,2),
     &                N0INHD(MSCAHD,2),NBRAHD(MSCAHD,2),NPROHD(MSCAHD)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      DIMENSION P1(4),P2(4)
C
      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
      ITYPE(L) = MOD(LREC1(L),100)-50
C
      ITRY = 0
      LSC1HD = 0
      MSOFT = 2
      MHARD = 1
      PT1 = PTCUT(II)
C
      IF(ECMP.LT.(2.D0*PT1+2.1*CUTMU(II))) THEN
        IFAIL(18) = IFAIL(18)+1
        IREJ = 5
        GOTO 450
      ENDIF
      AS     = (CUTMU(II)/ECMP)**2
      AH     = (2.D0*PT1/ECMP)**2
      ALNS   = LOG(AS)
      ALNH   = LOG(AH)
C
C  main loop to select hard and soft parton kinematics
C -----------------------------------------------------
 120  CONTINUE
        IREJ = 0
        ITRY   = ITRY+1
        LSC1HD = LSC1HD+1
        IF(ITRY.GT.1) THEN
          IFAIL(17) = IFAIL(17)+1
          IF(ITRY.GE.NTRY) THEN
            IREJ = 1
            GOTO 450
          ENDIF
        ENDIF
        LINE   = 0
        LSCAHD = 0
        XSS1   = ZERO
        XSS2   = ZERO
        MSPAR1 = 0
        MSPAR2 = 0
        Z1MAX    = LOG(MAX(TINY,ONE-MSOFT*AS))
        Z1DIF    = Z1MAX-ALNH
        XMAXX    = ONE - MSOFT*AS
C
C  select hard V,X
        CALL POHSCA(1,II)
        XSS1   = XSS1+X1
        XSS2   = XSS2+X2
        XH1(1) = X1
        XH2(1) = X2
C  debug output
        IF(IDEB(25).GE.20) THEN
          WRITE(6,'(1X,A,2E12.4,2I5)')
     &      'POHDIR:DEBUG:AS,XMAX,PROCESS NUMBER,ITRY',
     &      AS,XMAXX,MSPR,ITRY
          WRITE(6,'(1X,A,4E12.4)') 'HARD X1,2  SUM X1,2',
     &      X1,X2,XSS1,XSS2
        ENDIF
C
      IF(MSPR.LE.11) THEN
        IF((XSS2.GT.XMAXX).OR.((ONE-XSS2).LT.AS)) GOTO 120
      ELSE IF(MSPR.LE.13) THEN
        IF((XSS1.GT.XMAXX).OR.((ONE-XSS1).LT.AS)) GOTO 120
      ENDIF
C
C  fill COMMON HARSLT
      LSCAHD     = 1
      XHD(1,1)   = X1
      XHD(1,2)   = X2
      VHD(1)     = V
      ETAHD(1,1) = ETAC
      ETAHD(1,2) = ETAD
      PTHD(1)    = PT
      Q2SCA(1,1) = QQPD
      Q2SCA(1,2) = QQPD
      NPROHD(1)  = MSPR
      NBRAHD(1,1)= IDPDG1
      NBRAHD(1,2)= IDPDG2
C  valence quarks?
      IF(MSPR.LE.11) THEN
        NINHD(1,1) = IDPDG1
        NINHD(1,2) = ITYPE(2)
        IF(ABS(NINHD(1,2)).GT.6) THEN
          IVAL2 = 1
        ELSE
          IVAL2 = 0
        ENDIF
        KHDIR = 1
      ELSE IF(MSPR.LE.13) THEN
        NINHD(1,1) = ITYPE(1)
        IF(ABS(NINHD(1,1)).GT.6) THEN
          IVAL1 = 1
        ELSE
          IVAL1 = 0
        ENDIF
        NINHD(1,2) = IDPDG2
        KHDIR = 2
      ELSE
        NINHD(1,1) = IDPDG1
        NINHD(1,2) = IDPDG2
        KHDIR = 3
      ENDIF
C  reweight according to photon virtuality
      IF((IPAMDL(115).EQ.1).AND.(MSPR.NE.14)) THEN
        WGX = 1.D0
        IF((MSPR.LE.11).AND.(IDPDG2.EQ.22)) THEN
          WGX = LOG(QQPD/(PVIRTP(2)+PARMDL(31)))/LOG(QQPD/PARMDL(31))
          IF(NINHD(1,2).EQ.0) WGX = WGX*WGX
        ELSE IF((MSPR.LE.13).AND.(IDPDG1.EQ.22)) THEN
          WGX = LOG(QQPD/(PVIRTP(1)+PARMDL(31)))/LOG(QQPD/PARMDL(31))
          IF(NINHD(1,1).EQ.0) WGX = WGX*WGX
        ENDIF
        IF(WGX.LT.DRNDM(WGX)) THEN
          IREJ = 50
          RETURN
        ENDIF
      ENDIF
C  copy for parton showering
      DO 111 K=1,2
        NOUTHD(1,K) = ITYPE(K+2)
        N0INHD(1,K) = NINHD(1,K)
        X0HD(1,K)   = XHD(1,K)
111   CONTINUE
C  generate ISR
      IF((MSPR.NE.14).AND.(ISWMDL(8).GE.2)) THEN
C  ISR cutoffs
        Q2MISR(1) = MIN(PT1**2,PARMDL(125+II))
        Q2MISR(2) = Q2MISR(1)
        Q2H = PARMDL(93)*PT**2
*       Q2H = -PARMDL(93)*VHD(1)*XHD(1,1)*XHD(1,2)*ECMP*ECMP
        XHMAX1 =  ONE - XSS1 - MSOFT*AS + XHD(1,1)
        XHMAX2 =  ONE - XSS2 - MSOFT*AS + XHD(1,2)
        DO 42 J=1,4
          P1(J) = PREC(J,3)
          P2(J) = PREC(J,4)
 42     CONTINUE
        CALL POHISR(1,P1,P2,NOUTHD(1,1),NOUTHD(1,2),NINHD(1,1),
     &    NINHD(1,2),Q2H,XHD(1,1),XHD(1,2),XHMAX1,XHMAX2,IFL1,IFL2,
     &    XISR1,XISR2,IREJ)
        XSS1 = XSS1+XISR1-XHD(1,1)
        XSS2 = XSS2+XISR2-XHD(1,2)
        NINHD(1,1) = IFL1
        NINHD(1,2) = IFL2
        XHD(1,1) = XISR1
        XHD(1,2) = XISR2
      ELSE
        IFL1 = NINHD(1,1)
        IFL2 = NINHD(1,2)
      ENDIF
C  add photon/hadron remnant
      Z1MAX = LOG(MAX(TINY,ONE-MHARD*AH))
      Z2MAX = LOG(MAX(TINY,ONE-MHARD*AH))
      Z1DIF = Z1MAX+Z2MAX-ALNS
      Z2DIF = Z1DIF
C
C  incoming gluon
      IF(IFL2.EQ.0) THEN
        XMAXX    = ONE - XSS2 - AS
        XMAXH    = MIN(XMAXX,PARMDL(44))
        CALL HADSP2(IDBAM2,XSS2,XMAXH,XS2,IREJ)
        IVAL2 = 1
        MSPAR1 = 0
        MSPAR2 = 2
        MHPAR1 = 1
        MHPAR2 = 1
      ELSE IF(IFL1.EQ.0) THEN
        XMAXX    = ONE - XSS1 - AS
        XMAXH    = MIN(XMAXX,PARMDL(44))
        CALL HADSP2(IDBAM1,XSS1,XMAXH,XS1,IREJ)
        IVAL1 = 1
        MSPAR1 = 2
        MSPAR2 = 0
        MHPAR1 = 1
        MHPAR2 = 1
C
C  incoming quark
      ELSE IF(ABS(IFL2).LE.12) THEN
        IF(IVAL2.EQ.1) THEN
          XS2(1) = ONE - XSS2
          MSPAR1 = 0
          MSPAR2 = 1
          MHPAR1 = 1
          MHPAR2 = 1
        ELSE
          XMAXX    = ONE - XSS2 - AS
          XMAXH    = MIN(XMAXX,PARMDL(44))
          CALL HADSP3(IDBAM2,XSS2,XMAXH,XS2,IREJ)
          MSPAR1 = 0
          MSPAR2 = 3
          MHPAR1 = 1
          MHPAR2 = 1
        ENDIF
      ELSE IF(ABS(IFL1).LE.12) THEN
        IF(IVAL1.EQ.1) THEN
          XS1(1) = ONE - XSS1
          MSPAR1 = 1
          MSPAR2 = 0
          MHPAR1 = 1
          MHPAR2 = 1
        ELSE
          XMAXX    = ONE - XSS1 - AS
          XMAXH    = MIN(XMAXX,PARMDL(44))
          CALL HADSP3(IDBAM1,XSS1,XMAXH,XS1,IREJ)
          MSPAR1 = 3
          MSPAR2 = 0
          MHPAR1 = 1
          MHPAR2 = 1
        ENDIF
C
C  double direct process
      ELSE IF(MSPR.EQ.14) THEN
        MSPAR1 = 0
        MSPAR2 = 0
        MHPAR1 = 1
        MHPAR2 = 1
C
C  unknown process
      ELSE
        WRITE(6,'(/1X,A,I3/)') 'POHDIR:ERROR:UNSUPPORTED MSPR',MSPR
        CALL POABRT
      ENDIF
      IF(IREJ.NE.0) THEN
        IF(IDEB(25).GE.3) THEN
          WRITE(6,'(1X,A,3I5)')
     &      'POHDIR:WARNING:INT.REJ.(MSPR,ITRY,NTRY)',MSPR,ITRY,NTRY
        ENDIF
        GOTO 120
      ENDIF
C
C  select information from event-record
C
C  flavors
      IF(ABS(NINHD(1,1)).GT.6)
     &  NINHD(1,1)=SIGN(ABS(NINHD(1,1))-6,NINHD(1,1))
      IJHI1(1) = NINHD(1,1)
      IF(ABS(NINHD(1,2)).GT.6)
     &  NINHD(1,2)=SIGN(ABS(NINHD(1,2))-6,NINHD(1,2))
      IJHI2(1) = NINHD(1,2)
C  final partons
C  flavors
      IJHF1(1) = NOUTHD(1,1)
      IJHF2(1) = NOUTHD(1,2)
C  four momenta
      DO 140 J=1,4
        PHARD1(J,1) = PREC(J,3)
        PHARD2(J,1) = PREC(J,4)
140   CONTINUE
C
C  soft particle momenta
      IF(MSPAR1.GT.0) THEN
        DO 50 I=1,MSPAR1
          PSOFT1(1,I) = ZERO
          PSOFT1(2,I) = ZERO
          PSOFT1(3,I) = XS1(I)*ECMP/TWO
          PSOFT1(4,I) = XS1(I)*ECMP/TWO
 50     CONTINUE
      ENDIF
      IF(MSPAR2.GT.0) THEN
        DO 55 I=1,MSPAR2
          PSOFT2(1,I) = ZERO
          PSOFT2(2,I) = ZERO
          PSOFT2(3,I) = -XS2(I)*ECMP/TWO
          PSOFT2(4,I) = XS2(I)*ECMP/TWO
 55     CONTINUE
      ENDIF
C  process counting
      MXSECT(3,MSPR,II) = MXSECT(3,MSPR,II)+1
      KSOFT = MAX(MSPAR1,MSPAR2)
      KHARD = MAX(MHPAR1,MHPAR2)
C  debug output
      IF(IDEB(25).GE.10) THEN
        WRITE(6,'(/1X,A,2I3,3I5)')
     &    'POHDIR:DEBUG:(ACCEPTED) IVAL1,IVAL2,MSPR,ITRY,NTRY',
     &     IVAL1,IVAL2,MSPR,ITRY,NTRY
        IF(MSPAR1.GT.0) THEN
          WRITE(6,'(5X,A,I4)') 'SOFT X PARTICLE1:',MSPAR1
          DO 105 I=1,MSPAR1
            WRITE(6,'(10X,I3,E12.3)') I,XS1(I)
 105      CONTINUE
        ENDIF
        IF(MSPAR2.GT.0) THEN
          WRITE(6,'(5X,A,I4)') 'SOFT X PARTICLE2:',MSPAR2
          DO 106 I=1,MSPAR2
            WRITE(6,'(10X,I3,E12.3)') I,XS2(I)
 106      CONTINUE
        ENDIF
        IF(MHPAR1.GT.0) THEN
          WRITE(6,'(5X,A,I4)') 'INI.HARD X/FLAVOR PARTICLE1:',MHPAR1
          DO 107 I=1,MHPAR1
            WRITE(6,'(10X,I3,E12.3,I5)') I,XH1(I),IJHI1(I)
 107      CONTINUE
          WRITE(6,'(5X,A,I4)') 'FIN.HARD MOMENTA  PARTICLE1:',MHPAR1
          DO 108 I=1,MHPAR1
            WRITE(6,'(10X,I3,4E12.3,I5)') I,(PHARD1(K,I),K=1,4),
     &        IJHF1(I)
 108      CONTINUE
        ENDIF
        IF(MHPAR2.GT.0) THEN
          WRITE(6,'(5X,A,I4)') 'INI.HARD X/FLAVOUR PARTICLE2:',MHPAR2
          DO 109 I=1,MHPAR2
            WRITE(6,'(10X,I3,E12.3,I5)') I,XH2(I),IJHI2(I)
 109      CONTINUE
          WRITE(6,'(5X,A,I4)') 'FIN.HARD MOMENTA  PARTICLE2:',MHPAR2
          DO 110 I=1,MHPAR2
            WRITE(6,'(10X,I3,4E12.3,I5)') I,(PHARD2(K,I),K=1,4),
     &        IJHF2(I)
 110      CONTINUE
        ENDIF
      ENDIF
      RETURN
C
 450  CONTINUE
      IFAIL(16) = IFAIL(16)+1
      IF(IDEB(25).GE.2) THEN
        WRITE(6,'(1X,A,3I5)')
     &    'POHDIR:WARNING:REJECTION (ITRY,NTRY,IREJ)',ITRY,NTRY,IREJ
       WRITE(6,'(5X,A,E12.4)') 'AVAILABLE ENERGY:',ECMP
       IF(IDEB(25).GE.5) THEN
         CALL POPREV(0)
       ELSE
         CALL POPREV(-1)
       ENDIF
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE POMSCA(II,MSPOM,MHPOM,MSREG,IVAL1,IVAL2,MSPAR1,MSPAR2,
     &                  MHPAR1,MHPAR2,IREJ)
C**********************************************************************
C
C     parton orientated formulation of
C               soft and semihard inelastic events
C
C
C     input:    II        particle combiantion (1..4)
C               MSPOM     number of soft pomerons
C               MHPOM     number of semihard pomerons
C               MSREG     number of soft reggeons
C
C     output:   IVAL1,2   0 no valence quark engaged
C                         otherwise:  position of valence quark engaged
C                         neg.number: gluon connected to valence quark
C                                     by color flow
C               MSPAR1,2  number of realized soft partons
C               MHPAR1,2  number of realized hard partons
C               IREJ      1 failure
C                         0 success
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      COMMON /DGLAPP/ Q2MISR(2),PMISR(2),ZMISR(2),AL2ISR(2),NFSISR
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      COMMON /HAEVNT/ PT1,PT2,NHARD,NTRY,IHARD,ITRY,IREJEV
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /HAEVTR/ LINE,LIN,LREC1(MLINE),LREC2(MLINE),PREC(4,MLINE)
      COMMON /ABRHRD/ XH1(50),XH2(50),IJHI1(50),IJHI2(50),
     &                IJHF1(50),IJHF2(50),PHARD1(4,50),PHARD2(4,50),IPTM
      COMMON /ABRSOF/ XS1(50),XS2(50),IJSI1(50),IJSI2(50),
     &                PSOFT1(4,50),PSOFT2(4,50)
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      COMMON /HARSLT/ LSCAHD,LSC1HD,
     &                ETAHD(MSCAHD,2) ,PTHD(MSCAHD), Q2SCA(MSCAHD,2),
     &                XHD(MSCAHD,2)   ,VHD(MSCAHD) ,X0HD(MSCAHD,2),
     &                NINHD(MSCAHD,2) ,NOUTHD(MSCAHD,2),
     &                N0INHD(MSCAHD,2),NBRAHD(MSCAHD,2),NPROHD(MSCAHD)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           TINY   =  1.D-30,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           FOUR   =  4.D0,
     &           OHALF  =  0.5D0)
      DIMENSION P1(4),P2(4),PD1(-6:6)
      ITYPE(L)      = MOD(LREC1(L),100)-50
C
      IF(IDEB(24).GT.20) THEN
        WRITE(6,'(1X,A,3I5)')
     &    'POMSCA:DEBUG:MSPOM,MHPOM,MSREG',MSPOM,MHPOM,MSREG
      ENDIF
C
      ITRY  = 0
      IREJ  = 0
      INMAX = 10
      MHARD = MHPOM
      PT1 = PTCUT(II)
C  ISR cutoffs
      Q2MISR(1) = MIN(PT1**2,PARMDL(125+II))
      Q2MISR(2) = Q2MISR(1)
C  phase space limitation
      IF(MHPOM.GT.0) THEN
        IF(ECMP.LT.(2.D0*PT1+2.1D0*CUTMU(II))) THEN
          IREJ = 5
          IFAIL(6) = IFAIL(6) + 1
          GOTO 450
        ENDIF
      ENDIF
      AS     = (CUTMU(II)/ECMP)**2
      AH     = (2.D0*PT1/ECMP)**2
C
C  main loop to select hard and soft parton kinematics
C -----------------------------------------------------
 20   CONTINUE
        IREJ  = 0
        IHARD = 0
        LSC1HD = 0
        ITRY  = ITRY+1
        IF(ITRY.GT.1) IFAIL(5) = IFAIL(5)+1
        IF(ITRY.GE.NTRY) THEN
          IREJ = 1
          GOTO 450
        ENDIF
        LINE   = 0
        LSCAHD = 0
        IF((IPAMDL(13).GT.0).AND.(IPROCE.EQ.1)) THEN
          XSS1   = MAX(0.D0,1.D0-XPSUB)
          XSS2   = MAX(0.D0,1.D0-XTSUB)
        ELSE
          XSS1   = ZERO
          XSS2   = ZERO
        ENDIF
        IVAL1  = 0
        IVAL2  = 0
        IF(MSPOM.EQ.0) THEN
          MSPAR1 = 2+2*MHPOM+MSREG
        ELSE
          MSPAR1 = 2*MSPOM+MSREG+2*MHPOM
        ENDIF
        MSPAR2 = MSPAR1
        MHPAR1 = MHPOM
        MHPAR2 = MHPOM
C
        MSCHA = MSPAR1
        MHCHA = 2*MHPOM
        KSOFT = MSCHA
        KHARD = MHCHA
C  check kinematic limitations
        XX = AS*DBLE(4*MSCHA)+AH*DBLE(MHCHA)
        IF(XX.GE.ONE) THEN
          IF(IDEB(24).GE.1) THEN
            WRITE(6,'(1X,A,2I2,3E10.3)')
     &        'POMSCA:WARNING:KIN.REJECTION(MS,MH,AS,AH,XX)',
     &        MSCHA,MHCHA,AS,AH,XX
          ENDIF
          IREJ = 2
          IFAIL(6) = IFAIL(6) + 1
          GOTO 450
        ENDIF
        IF(MSCHA.GT.1) THEN
          AS = ONE/(TWO*(MSCHA-1))*(ONE-SQRT(ONE-FOUR*AS*(MSCHA-1)))
        ENDIF
        AS = MAX(AS,PSOMIN/PCMP)
C
        ALNS  = LOG(AS)
        ALNH  = LOG(AH)
        Z1MAX = LOG(MAX(TINY,ONE-MSCHA*AS-MAX(MHCHA-1,0)*AH))
        Z2MAX = LOG(MAX(TINY,ONE-MSCHA*AS-MAX(MHCHA-1,0)*AH))
        Z1DIF = Z1MAX+Z2MAX-ALNH
        Z2DIF = Z1DIF
        XMAXX = ONE - MSCHA*AS - MAX(MHCHA-1,0)*AH
        PTMAX = 0.D0
        IPTM  = 0
C
C  select hard parton momenta
C ------------------- begin of inner loop -------------------
        DO 10 NN=1,MHARD
C
          INTRY = 0
          IVOLD1 = IVAL1
          IVOLD2 = IVAL2
C
 15       CONTINUE
          IVAL1 = IVOLD1
          IVAL2 = IVOLD2
          INTRY = INTRY+1
          IFAIL(31) = IFAIL(31)+1
C  generate one hard scattering
          CALL POHSCA(2,II)
          XSSS1   = XSS1+X1
          XSSS2   = XSS2+X2
          XH1(NN) = X1
          XH2(NN) = X2
C  debug output
          IF(IDEB(24).GE.10) WRITE(6,'(1X,A,I3,I4,I5,2E12.4)')
     &      'POMSCA:DEBUG:MSPR,IHARD,ITRY,X1,X2',MSPR,NN,ITRY,X1,X2
C
          IF(    (XSSS1.GT.XMAXX)
     &       .OR.(XSSS2.GT.XMAXX)
     &       .OR.((ONE-XSSS1)*(ONE-XSSS2).LT.AS) ) THEN
            IF(INTRY.GT.INMAX) GOTO 20
            LINE = LINE-4
C*****************************
            IF(IHARD.EQ.0) THEN
              IF(ISWMDL(2).NE.1) GOTO 15
              MHPOM = 0
              MSPOM = 1
            ENDIF
            GOTO 199
*           GOTO 15
C*****************************
          ENDIF
C  valence quarks
          DO 116 K=1,2
            IND = 4*(NN-1)+K
            NINHD(NN,K)  = ITYPE(IND)
            NOUTHD(NN,K) = ITYPE(IND+2)
 116      CONTINUE
C  anomalous/resolved contribution
          IF(ISWMDL(27).GT.0) THEN
            IF((IDPDG1.EQ.22).AND.(NINHD(NN,1).NE.0)) THEN
              WGDIR = ZERO
              CALL POPDFB(NINHD(NN,1),X1,Q2SCA(NN,1),PVIRTP(1),WGDIR)
              XI = DRNDM(X1)*PD1(NINHD(NN,1))
              IF(WGDIR.GT.XI) THEN
                NINHD(NN,1) = SIGN(ABS(NINHD(NN,1))+6,NINHD(NN,1))
              ENDIF
            ENDIF
            IF((IDPDG2.EQ.22).AND.(NINHD(NN,2).NE.0)) THEN
              WGDIR = ZERO
              CALL POPDFB(NINHD(NN,2),X2,Q2SCA(NN,2),PVIRTP(2),WGDIR)
              XI = DRNDM(X2)*PD1(NINHD(NN,2))
              IF(WGDIR.GT.XI) THEN
                NINHD(NN,2) = SIGN(ABS(NINHD(NN,2))+6,NINHD(NN,2))
              ENDIF
            ENDIF
          ENDIF
C
          IF((IVAL1.EQ.0).AND.(NINHD(NN,1).NE.0)
     &       .AND.(ABS(NINHD(NN,1)).LE.6)) THEN
            IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)) THEN
              NINHD(NN,1) = SIGN(ABS(NINHD(NN,1))+6,NINHD(NN,1))
            ENDIF
          ENDIF
          IF((IVAL2.EQ.0).AND.(NINHD(NN,2).NE.0)
     &       .AND.(ABS(NINHD(NN,2)).LE.6)) THEN
            IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
              NINHD(NN,2) = SIGN(ABS(NINHD(NN,2))+6,NINHD(NN,2))
            ENDIF
          ENDIF
C  particle 1
          IF(ABS(NINHD(NN,1)).GT.6) THEN
            IF(IVAL1.NE.0) THEN
              IF(INTRY.GT.INMAX) GOTO 20
              LINE = LINE-4
              GOTO 15
            ELSE
              IVAL1 = NN
            ENDIF
          ENDIF
C  particle 2
          IND = IND+1
          IF(ABS(NINHD(NN,2)).GT.6) THEN
            IF(IVAL2.NE.0) THEN
              IF(INTRY.GT.INMAX) GOTO 20
              LINE = LINE-4
              GOTO 15
            ELSE
              IVAL2 = NN
            ENDIF
          ENDIF
C
          IHARD = IHARD+1
          XSS1 = XSSS1
          XSS2 = XSSS2
C  fill COMMON HARSLT
          LSCAHD         = IHARD
          XHD(IHARD,1)   = X1
          XHD(IHARD,2)   = X2
          VHD(IHARD)     = V
          ETAHD(IHARD,1) = ETAC
          ETAHD(IHARD,2) = ETAD
          PTHD(IHARD)    = PT
          NPROHD(IHARD)  = MSPR
          Q2SCA(IHARD,1) = QQPD
          Q2SCA(IHARD,2) = QQPD
C
          X0HD(NN,1)      = X1
          X0HD(NN,2)      = X2
          N0INHD(NN,1)    = NINHD(NN,1)
          N0INHD(NN,2)    = NINHD(NN,2)
          NBRAHD(NN,1)    = IDPDG1
          NBRAHD(NN,2)    = IDPDG2
          IF(PT.GT.PTMAX) IPTM = IHARD
C  reweight according to photon virtuality
          IF((IHARD.EQ.1).AND.(IPAMDL(115).EQ.1)) THEN
            WGX = 1.D0
            IF(IDPDG1.EQ.22) THEN
              WG1 = LOG(QQPD/(PVIRTP(1)+PARMDL(31)))
     &             /LOG(QQPD/PARMDL(31))
              IF(NINHD(IHARD,1).EQ.0) WG1 = WG1*WG1
              WGX = WG1
            ENDIF
            IF(IDPDG2.EQ.22) THEN
              WG1 = LOG(QQPD/(PVIRTP(2)+PARMDL(31)))
     &             /LOG(QQPD/PARMDL(31))
              IF(NINHD(IHARD,2).EQ.0) WG1 = WG1*WG1
              WGX = WGX*WG1
            ENDIF
            IF(WGX.LT.DRNDM(WGX)) THEN
              IREJ = 50
              RETURN
            ENDIF
          ENDIF
C
C  generate ISR
          IF((IHARD.EQ.1).AND.(ISWMDL(8).GE.2)) THEN
            Q2H = PARMDL(93)*PT**2
*           Q2H = -PARMDL(93)*VHD(IPTM)
*    &            *XHD(IPTM,1)*XHD(IPTM,2)*ECMP*ECMP
            IND = 4*(IPTM-1)+1
            XHMAX1 =  ONE - XSS1 - MSCHA*AS + XHD(IPTM,1)
            XHMAX2 =  ONE - XSS2 - MSCHA*AS + XHD(IPTM,2)
            I3     = 4*IPTM-1
            I4     = 4*IPTM
            DO 42 J=1,4
              P1(J) = PREC(J,I3)
              P2(J) = PREC(J,I4)
 42         CONTINUE
            IF(IDEB(24).GE.10)
     &        WRITE(6,'(1X,A,2I6)') 'POMSCA:ISR FOR IPTM,IND',IPTM,IND
            CALL POHISR(IPTM,P1,P2,NOUTHD(IPTM,1),NOUTHD(IPTM,2),
     &        NINHD(IPTM,1),NINHD(IPTM,2),Q2H,XHD(IPTM,1),XHD(IPTM,2),
     &        XHMAX1,XHMAX2,IFL1,IFL2,XISR1,XISR2,IREJ)
            XSS1 = XSS1+XISR1-XHD(IPTM,1)
            XSS2 = XSS2+XISR2-XHD(IPTM,2)
            NINHD(IPTM,1) = IFL1
            NINHD(IPTM,2) = IFL2
            XHD(IPTM,1) = XISR1
            XHD(IPTM,2) = XISR2
          ENDIF
C
 10     CONTINUE
 199    CONTINUE
        MHPOM =  IHARD
        MHPAR1 = IHARD
        MHPAR2 = IHARD
        IF(MSPOM.EQ.0) THEN
          MSPAR1 = 2+2*MHPOM+MSREG
        ELSE
          MSPAR1 = 2*MSPOM+MSREG+2*MHPOM
        ENDIF
        MSPAR2 = MSPAR1
C**************************
C ------------------- end of inner (hard) loop -------------------
C
C  count quarks involved in hard scattering
        IQUA1 = 0
        IQUA2 = 0
        IVGLU1 = 0
        IVGLU2 = 0
        DO 115 I=1,IHARD
          IF(NINHD(I,1).NE.0) THEN
            IQUA1 = IQUA1+1
          ELSE IF(IVGLU1.EQ.0) THEN
            IVGLU1 = I
          ENDIF
          IF(NINHD(I,2).NE.0) THEN
            IQUA2 = IQUA2+1
          ELSE IF(IVGLU2.EQ.0) THEN
            IVGLU2 = I
          ENDIF
115     CONTINUE
C  gluons emitted by valence quarks
        IF(II.EQ.1) THEN
          VALPRO = VALPRG(1)
        ELSE
          VALPRO = 1.D0
        ENDIF
        IV1 = 1
        IF(IVAL1.EQ.0) THEN
          IF((IVGLU1.NE.0).AND.(DRNDM(XSS1).LT.VALPRO)) THEN
            IVAL1 = -IVGLU1
          ELSE
            IV1 = 0
          ENDIF
        ENDIF
        IF(II.EQ.1) THEN
          VALPRO = VALPRG(2)
        ELSE
          VALPRO = 1.D0
        ENDIF
        IV2 = 1
        IF(IVAL2.EQ.0) THEN
          IF((IVGLU2.NE.0).AND.(DRNDM(XSS2).LT.VALPRO)) THEN
            IVAL2 = -IVGLU2
          ELSE
            IV2 = 0
          ENDIF
        ENDIF
C  debug output
        IF(IDEB(24).GE.5) THEN
          WRITE(6,'(1X,A,6I4)')
     &      'POMSCA:DEBUG:IVAL1,2  IQUA1,2  IVGLU1,2',
     &      IVAL1,IVAL2,IQUA1,IQUA2,IVGLU1,IVGLU2
        ENDIF
C  correct soft quark numbers
        IF(MSPOM.EQ.0) THEN
          MSPAR1 = MSPAR1-IQUA1-2*IV1
          MSPAR2 = MSPAR2-IQUA2-2*IV2
        ELSE
          MSPAR1 = MSPAR1-IQUA1
          MSPAR2 = MSPAR2-IQUA2
        ENDIF
C
C  select soft X
 25     CONTINUE
        XMAX1  = ONE - MAX(MSPAR1-1,0)*AS - XSS1
        XMAX2  = ONE - MAX(MSPAR2-1,0)*AS - XSS2
        I1 = IV1
        I2 = IV2
        IF(IVAL1.LE.0) I1 = 0
        IF(IVAL2.LE.0) I2 = 0
        IF(IV1*IV2.NE.0) THEN
          MSDIFF = 2*MAX(0,MSPOM)
        ELSE
          MSDIFF = 2*MAX(0,MSPOM-1)
        ENDIF
        MSG1 = MSPAR1
        MSG2 = MSPAR2
        MSM1 = MSPAR1-MSDIFF
        MSM2 = MSPAR2-MSDIFF
        XMAXH1 = MIN(XMAX1,PARMDL(44))
        XMAXH2 = MIN(XMAX2,PARMDL(44))
        CALL SOFTXX(NPOSP(1),NPOSP(2),MSG1,MSG2,I1,I2,MSM1,MSM2,
     &              XSS1,XSS2,XMAXH1,XMAXH2,XS1,XS2,IREJ)
        MSPOM  = MSPOM-(MSPAR1-MSG1)/2
        MSPAR1 = MSG1
        MSPAR2 = MSG2
C  correct for high pt tail
        IF(IREJ.NE.0) THEN
          IREJ = 4
          GOTO 450
        ENDIF
C  ------------ kinematics sampled ---------------
C  debug output
        IF(IDEB(24).GE.10) THEN
          WRITE(6,'(1X,A,I3)') 'POMSCA:DEBUG:SOFT X VALUES,ITRY',ITRY
          DO 104 I=2,MAX(MSPAR1,MSPAR2)
            WRITE(6,'(10X,I3,2E12.3)') I,XS1(I),XS2(I)
 104      CONTINUE
        ENDIF
      IF((ONE-XSS1)*(ONE-XSS2).LT.AS) GOTO 20
C
C  end of loop
      XS1(1) = ONE - XSS1
      XS2(1) = ONE - XSS2
C*****************************
*     IF( IHARD.LT.MHARD .AND. ITRY.GE.NTRY ) THEN
      IF( ITRY.GE.NTRY ) THEN
C*****************************
        IREJ = 6
        GOTO 450
      ENDIF
C
C  select information from event-record
C
C  initial partons
      DO 45 N=1,IHARD
C  flavors
        IF(ABS(NINHD(N,1)).GT.6)
     &    NINHD(N,1)=SIGN(ABS(NINHD(N,1))-6,NINHD(N,1))
        IJHI1(N) = NINHD(N,1)
        IF(ABS(NINHD(N,2)).GT.6)
     &    NINHD(N,2)=SIGN(ABS(NINHD(N,2))-6,NINHD(N,2))
        IJHI2(N) = NINHD(N,2)
 45   CONTINUE
C  final partons
      DO 30 N=1,LSCAHD
        I3     = 4*N-1
        I4     = 4*N
C  flavors
        IJHF1(N) = NOUTHD(N,1)
        IJHF2(N) = NOUTHD(N,2)
C  four momentum
        DO 40 J=1,4
          PHARD1(J,N) = PREC(J,I3)
          PHARD2(J,N) = PREC(J,I4)
40      CONTINUE
C  process counting
        MXSECT(3,NPROHD(N),II) = MXSECT(3,NPROHD(N),II)+1
30    CONTINUE
C
C  soft particle momenta
      DO 50 I=1,MSPAR1
        PSOFT1(1,I) = ZERO
        PSOFT1(2,I) = ZERO
        PSOFT1(3,I) = XS1(I)*ECMP/TWO
        PSOFT1(4,I) = XS1(I)*ECMP/TWO
 50   CONTINUE
      DO 55 I=1,MSPAR2
        PSOFT2(1,I) = ZERO
        PSOFT2(2,I) = ZERO
        PSOFT2(3,I) = -XS2(I)*ECMP/TWO
        PSOFT2(4,I) = XS2(I)*ECMP/TWO
 55   CONTINUE
      KSOFT = MAX(MSPAR1,MSPAR2)
      KHARD = MAX(MHPAR1,MHPAR2)
C  debug output
      IF(IDEB(24).GE.10) THEN
        WRITE(6,'(/1X,A,2I3,2I5)')
     &    'POMSCA:DEBUG:(ACCEPTED) IVAL1,IVAL2,ITRY,NTRY',
     &     IVAL1,IVAL2,ITRY,NTRY
        IF(MSPAR1+MSPAR2.GT.0) THEN
          WRITE(6,'(5X,A)') 'SOFT X PARTICLE1   PARTICLE2:'
          XTMP1 = ZERO
          XTMP2 = ZERO
          DO 105 I=1,MAX(MSPAR1,MSPAR2)
            IF(I.LE.MIN(MSPAR1,MSPAR2)) THEN
              WRITE(6,'(10X,I3,2E13.4)') I,XS1(I),XS2(I)
              XTMP1 = XTMP1+XS1(I)
              XTMP2 = XTMP2+XS2(I)
            ELSE IF(I.LE.MSPAR1) THEN
              WRITE(6,'(10X,I3,2E13.4)') I,XS1(I),ZERO
              XTMP1 = XTMP1+XS1(I)
            ELSE IF(I.LE.MSPAR2) THEN
              WRITE(6,'(10X,I3,2E13.4)') I,ZERO,XS2(I)
              XTMP2 = XTMP2+XS2(I)
            ENDIF
 105      CONTINUE
          WRITE(6,'(1X,A,2E13.4)') 'SUM X1/2 (soft):',XTMP1,XTMP2
        ENDIF
        IF(MHPAR1.GT.0) THEN
          WRITE(6,'(5X,A)') 
     &      'NR  MSPR HARD X / HARD X ISR / FLAVOR PARTICLE1,2:'
          DO 107 I=1,MHPAR1
            WRITE(6,'(10X,2I3,4E12.3,2I3)')
     &        I,NPROHD(I),XH1(I),XH2(I),XHD(I,1),XHD(I,2),
     &        IJHI1(I),IJHI2(I)
              XTMP1 = XTMP1+XHD(I,1)
              XTMP2 = XTMP2+XHD(I,2)
 107      CONTINUE
          WRITE(6,'(1X,A,2E13.4)') 'SUM X1/2 (soft+hard):',XTMP1,XTMP2
          WRITE(6,'(5X,A)') 'HARD MOMENTA  PARTICLE1:'
          DO 108 I=1,MHPAR1
            WRITE(6,'(10X,I3,4E12.3,I5)') I,(PHARD1(K,I),K=1,4),
     &        IJHF2(I)
 108      CONTINUE
          WRITE(6,'(5X,A)') 'HARD MOMENTA  PARTICLE2:'
          DO 110 I=1,MHPAR2
            WRITE(6,'(10X,I3,4E12.3,I5)') I,(PHARD2(K,I),K=1,4),
     &        IJHF1(I)
 110      CONTINUE
        ENDIF
      ENDIF
      IFAIL(31) = IFAIL(31)-IHARD
      RETURN
C
 450  CONTINUE
      IFAIL(4) = IFAIL(4)+1
      IF(IDEB(24).GE.2) THEN
        WRITE(6,'(1X,A,/10X,7I5)')
     &  'POMSCA:REJECTION:(MSPOM,MHPOM,IHARD,MHARD,ITRY,NTRY,IREJ)',
     &  MSPOM,MHPOM,IHARD,MHARD,ITRY,NTRY,IREJ
        WRITE(6,'(5X,A,I4,2E12.4)') 'IP,ENERGY,PTCUT:',II,ECMP,PT1
        IF(IDEB(24).GE.5) THEN
          CALL POPREV(0)
        ELSE
          CALL POPREV(-1)
        ENDIF
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE POHX12
C**********************************************************************
C
C     selection of x1 and x2 according to 1/x1*1/x2
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      PARAMETER ( TINY= 1.D-30, ONE=1.D0 ,TINY6=1.D-06)

10      Z1 = Z1MAX-DRNDM(X1)*Z1DIF
        Z2 = Z2MAX-DRNDM(X2)*Z2DIF
        IF ( (Z1+Z2).LT.ALNH ) GOTO 10
      X1   = EXP(Z1)
      X2   = EXP(Z2)
      AXX  = AH/(X1*X2)
      W    = SQRT(MAX(TINY,ONE-AXX))
      W1   = AXX/(ONE+W)
      END
C
C
      SUBROUTINE POHDX1
C**********************************************************************
C
C     selection of x1 according to 1/x1
C     ( x2 = 1 )
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      PARAMETER ( TINY= 1.D-30, ONE=1.D0 ,TINY6=1.D-06)
C
      Z1 = Z1MAX-DRNDM(X1)*Z1DIF
      X2   = 1.D0
      X1   = EXP(Z1)
      AXX  = AH/X1
      W    = SQRT(MAX(TINY,ONE-AXX))
      W1   = AXX/(ONE+W)
      END
C
C
      SUBROUTINE POHKIN(IREJ)
C***********************************************************************
C
C     selection of kinematic variables
C     (resolved and direct processes)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      PARAMETER ( TINY= 1.D-30, ONE=1.D0 ,TINYP=1.D-14 )
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
C
      COMMON /KINLIM/ ETAMI(2,15),ETAMA(2,15),XXMI(2,15),XXMA(2,15)
C
      DIMENSION RM(-1:MAXPRO)
      DATA RM / 3.31D0, 0.0D0,
     &          7.60D0, 0.65D0, 4.00D0, 0.65D0, 0.89D0, 
     &          0.45D0, 0.89D0, 0.89D0, 0.0D0,  4.776D0, 
     &          0.615D0,4.776D0,0.615D0,1.0D0,  0.0D0, 
     &          1.0D0 /
C
      IREJ = 0
      M    = MSPR
C------------- resolved processes -----------
      IF     ( M.EQ.1 ) THEN
10      CALL POHX12
        V  =-0.5D0*W1/(W1+DRNDM(X1)*W)
        U  =-1.D0-V
        R  = (1.D0+W)*2.25D0*(V*V*(3.D0-U*V-V/(U*U))-U)
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(1)*DRNDM(X2) ) GOTO 10
        IF ( DRNDM(V).LE.0.5D0 ) V = U
      ELSEIF ( M.EQ.2 .OR. M.EQ.4 ) THEN
20      CALL POHX12
        WL = LOG(W1)
        V  =-EXP(-0.6931472D0+DRNDM(X1)*WL)
        U  =-1.D0-V
        R  = (U*U+V*V)*((16.D0/27.D0)/U-(4.D0/3.D0)*V)*(WL/W)*AXX
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(M)*DRNDM(X2) ) GOTO 20
        IF ( DRNDM(V).LE.0.5D0 ) V = U
      ELSEIF ( M.EQ.3 ) THEN
30      CALL POHX12
        V  =-0.5D0*W1/(W1+DRNDM(X1)*W)
        U  =-1.D0-V
        R  = (1.D0+W)*(1.D0+U*U)*(1.D0-(4.D0/9.D0)*V*V/U)
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(3)*DRNDM(X2) ) GOTO 30
      ELSEIF ( M.EQ.5 ) THEN
50      CALL POHX12
        V  =-0.5D0*AXX/(W1+2.D0*DRNDM(X1)*W)
        U  =-1.D0-V
        R  = (4.D0/9.D0)*(1.D0+U*U+V*V*(U*U+V*V))-(8.D0/27.D0)*U*U*V
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(5)*DRNDM(X2) ) GOTO 50
      ELSEIF ( M.EQ.6 ) THEN
60      CALL POHX12
        V  =-0.5D0*(1.D0+W)+DRNDM(X1)*W
        U  =-1.D0-V
        R  = (4.D0/9.D0)*(U*U+V*V)*AXX
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(6)*DRNDM(V) ) GOTO 60
      ELSEIF ( M.EQ.7 ) THEN
70      CALL POHX12
        V  =-0.5D0*W1/(W1+DRNDM(X1)*W)
        U  =-1.D0-V
        R  = (1.D0+W)*((2.D0/9.D0)*(1.D0+U*U+(1.D0+V*V)*V*V/(U*U))
     &       -(4.D0/27.D0)*V/U)
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(7)*DRNDM(X2) ) GOTO 70
        IF ( DRNDM(V).LE.0.5D0 ) V = U
      ELSEIF ( M.EQ.8 ) THEN
80      CALL POHX12
        V  =-0.5D0*AXX/(W1+2.D0*DRNDM(X1)*W)
        U  =-1.D0-V
        R  = (4.D0/9.D0)*(1.D0+U*U)
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(8)*DRNDM(X2) ) GOTO 80
      ELSEIF ( M.EQ.-1 ) THEN
90      CALL POHX12
        WL = LOG(W1)
        V  =-EXP(-0.6931472D0+DRNDM(X1)*WL)
        U  =-1.D0-V
        R  = (1.D0+V*V)*(V/(U*U)-(4.D0/9.D0))*(WL/W)*AXX
        IF(R*W.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R*W.LT.RM(-1)*DRNDM(X2) ) GOTO 90
C------------- direct processes -----------
      ELSEIF ( M.EQ.10 ) THEN
100     CALL POHDX1
        WL = LOG(AXX/(1.D0+W)**2)
        V  =-(1.D0+W)/2.D0*EXP(DRNDM(X1)*WL)
        U  =-1.D0-V
        R  = -(8.D0/3.D0)*(V*V+1.D0)*WL*AXX
        IF(R.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R.LT.RM(10)*DRNDM(V) ) GOTO 100
        X2 = X1
        X1 = 1.D0
      ELSEIF ( M.EQ.11) THEN
110     CALL POHDX1
        WL = LOG(W1)
        U  =-EXP(-0.6931472D0+DRNDM(X1)*WL)
        V  =-1.D0-U
        R  = (U*U+V*V)/V*WL*AXX
        IF(R.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R.LT.RM(11)*DRNDM(X2) ) GOTO 110
        IF ( DRNDM(V).LE.0.5D0 ) V = U
        X2 = X1
        X1 = 1.D0
      ELSEIF ( M.EQ.12 ) THEN
120     CALL POHDX1
        WL = LOG(AXX/(1.D0+W)**2)
        U  =-(1.D0+W)/2.D0*EXP(DRNDM(X1)*WL)
        V  =-1.D0-U
        R  = -(8.D0/3.D0)*(U*U+1.D0)*WL*AXX
        IF(R.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R.LT.RM(12)*DRNDM(X2) ) GOTO 120
      ELSEIF ( M.EQ.13) THEN
130     CALL POHDX1
        WL = LOG(W1)
        V  =-EXP(-0.6931472D0+DRNDM(X1)*WL)
        U  =-1.D0-V
        R  = (U*U+V*V)/U*WL*AXX
        IF(R.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R.LT.RM(13)*DRNDM(X2) ) GOTO 130
        IF ( DRNDM(V).LE.0.5D0 ) V = U
C------------- double direct process -----------
      ELSEIF ((M.EQ.14).OR.(M.EQ.16)) THEN
        X1 = 1.D0
        X2 = 1.D0
        AXX= AH
        W  = SQRT(MAX(TINY,ONE-AXX))
        W1 = AXX/(1.D0+W)
        WL = LOG(W1)
 140    V  =-EXP(-0.6931472D0+DRNDM(X1)*WL)
        U  =-1.D0-V
        R  = -(U*U+V*V)/U
        IF(R.GT.RM(M)) WRITE(6,'(1X,A,I3)') 'POHKIN:WEIGHT ERROR',M
        IF ( R.LT.RM(14)*DRNDM(X2) ) GOTO 140
        IF ( DRNDM(V).LE.0.5D0 ) V = U
C---------------------------------------------
      ELSE
        WRITE(6,'(/1X,A,I3)')
     &    'POHKIN:ERROR:UNSUPPORTED PROCESS (MSPR)',MSPR
        CALL POABRT
      ENDIF
C
      V    = MAX(MIN(V,-TINYP ),-1.D0+TINYP)
      U    = -1.D0-V
      U    = MAX(MIN(U,-TINYP ),-1.D0+TINYP)
      PT   = SQRT(U*V*X1*X2)*ECMP
      ETAC = 0.5D0*LOG((U*X1)/(V*X2))
      ETAD = 0.5D0*LOG((V*X1)/(U*X2))
C
      MM = M
      IF(M.EQ.-1) MM = 3
      ETAMI(1,MM) = MIN(ETAMI(1,MM),ETAC)
      ETAMA(1,MM) = MAX(ETAMA(1,MM),ETAC)
      ETAMI(2,MM) = MIN(ETAMI(2,MM),ETAD)
      ETAMA(2,MM) = MAX(ETAMA(2,MM),ETAD)
      XXMI(1,MM) = MIN(XXMI(1,MM),X1)
      XXMA(1,MM) = MAX(XXMA(1,MM),X1)
      XXMI(2,MM) = MIN(XXMI(2,MM),X2)
      XXMA(2,MM) = MAX(XXMA(2,MM),X2)
C
      IF(IDEB(81).GE.25) WRITE(6,'(1X,A,/5X,6E12.3)')
     &  'POHKIN:DEBUG:V,PT,ETAC,ETAD,X1,X2',V,PT,ETAC,ETAD,X1,X2
      END
C
C
      SUBROUTINE POHWGH(PDS,PDA,PDB,FDISTR)
C***********************************************************************
C
C     calculate product of PDFs and coupling constants
C     according to selected MSPR (process type)
C
C     input:    COMMON /HASCA /
C
C     output:   PDS     resulting from PDFs alone
C               FDISTR  complete weight function
C               PDA,PDB fields containing the PDFs
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      PARAMETER ( TINY= 1.D-30, ONE=1.D0 ,TINY6=1.D-06)
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      DIMENSION PDA(-6:6),PDB(-6:6)
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
**sr 29.04. commented to use dp-JETSET
      REAL ULALEM,SSR,QQALR
**
C
      FDISTR = 0.D0
C  set hard scale  QQ  for alpha and partondistr.
      IF     ( NQQAL.EQ.1 ) THEN
        QQAL = AQQAL*PT*PT
      ELSEIF ( NQQAL.EQ.2 ) THEN
        QQAL = AQQAL*X1*X2*ECMP*ECMP*U*V/(1.D0+V*V+U*U)
      ELSEIF ( NQQAL.EQ.3 ) THEN
        QQAL = AQQAL*X1*X2*ECMP*ECMP
      ELSEIF ( NQQAL.EQ.4 ) THEN
        QQAL = AQQAL*X1*X2*ECMP*ECMP*(U*V)**(1.D0/3.D0)
      ENDIF
      IF     ( NQQPD.EQ.1 ) THEN
        QQPD = AQQPD*PT*PT
      ELSEIF ( NQQPD.EQ.2 ) THEN
        QQPD = AQQPD*X1*X2*ECMP*ECMP*U*V/(1.D0+V*V+U*U)
      ELSEIF ( NQQPD.EQ.3 ) THEN
        QQPD = AQQPD*X1*X2*ECMP*ECMP
      ELSEIF ( NQQPD.EQ.4 ) THEN
        QQPD = AQQPD*X1*X2*ECMP*ECMP*(U*V)**(1.D0/3.D0)
      ENDIF
C  coupling constants, PDFs
      IF(MSPR.LT.9) THEN
        ALPHA1 = BQCD/LOG(MAX(QQAL/(PDFLAM(1)*PDFLAM(2)),1.1D0))
        ALPHA2 = ALPHA1
        CALL PDIS(X1,QQPD,1,PDA)
        CALL PDIS(X2,QQPD,2,PDB)
        IF ( MSPR.EQ.1  .OR.  MSPR.EQ.4 ) THEN
          PDS   = PDA(0)*PDB(0)
        ELSE
          S2    = 0.D0
          S3    = 0.D0
          S4    = 0.D0
          S5    = 0.D0
          DO 10 I=1,NF
            S2  = S2+PDA(I)*PDB(-I)+PDA(-I)*PDB( I)
            S3  = S3+PDA(I)*PDB( I)+PDA(-I)*PDB(-I)
            S4  = S4+PDA(I)+PDA(-I)
            S5  = S5+PDB(I)+PDB(-I)
 10       CONTINUE
          IF ((MSPR.EQ.2).OR.(MSPR.EQ.5).OR.(MSPR.EQ.6)) THEN
            PDS = S2
          ELSE IF((MSPR.EQ.3).OR.(MSPR.EQ.-1)) THEN
            PDS = PDA(0)*S5+PDB(0)*S4
          ELSE IF(MSPR.EQ.7) THEN
            PDS = S3
          ELSE IF(MSPR.EQ.8) THEN
            PDS = S4*S5-(S2+S3)
          ENDIF
        ENDIF
      ELSE IF(MSPR.LT.12) THEN
        ALPHA2 = BQCD/LOG(MAX(QQAL/PDFLAM(2)**2,1.1D0))
        IF(IDPDG1.EQ.22) THEN
          QQALR = QQAL
          ALPHA1 = DBLE(ULALEM(QQALR))
*         ALPHA1 = 1.D0/137.D0
        ELSE IF(IDPDG1.EQ.45) THEN
          ALPHA1 = PARMDL(74)
        ENDIF
        CALL PDIS(X2,QQPD,2,PDB)
        S4    = 0.D0
        S6    = 0.D0
        DO 15 I=1,NF
          S4  = S4+PDB(I)+PDB(-I)
C  charge counting
          IF(MOD(I,2).EQ.0) THEN
            S6  = S6+(PDB(I)+PDB(-I))*4.D0/9.D0
          ELSE
            S6  = S6+(PDB(I)+PDB(-I))*1.D0/9.D0
          ENDIF
 15     CONTINUE
        IF(MSPR.EQ.10) THEN
          IF(IDPDG1.EQ.45) THEN
            PDS = S4
          ELSE
            PDS = S6
          ENDIF
        ELSE
          PDS = PDB(0)
        ENDIF
      ELSE IF(MSPR.LT.14) THEN
        ALPHA1 = BQCD/LOG(MAX(QQAL/PDFLAM(1)**2,1.1D0))
        IF(IDPDG2.EQ.22) THEN
          QQALR = QQAL
          ALPHA2 = DBLE(ULALEM(QQALR))
*         ALPHA2 = 1.D0/137.D0
        ELSE IF(IDPDG2.EQ.45) THEN
          ALPHA2 = PARMDL(74)
        ENDIF
        CALL PDIS(X1,QQPD,1,PDA)
        S4    = 0.D0
        S6    = 0.D0
        DO 20 I=1,NF
          S4  = S4+PDA(I)+PDA(-I)
C  charge counting
          IF(MOD(I,2).EQ.0) THEN
            S6  = S6+(PDA(I)+PDA(-I))*4.D0/9.D0
          ELSE
            S6  = S6+(PDA(I)+PDA(-I))*1.D0/9.D0
          ENDIF
 20     CONTINUE
        IF(MSPR.EQ.12) THEN
          IF(IDPDG2.EQ.45) THEN
            PDS = S4
          ELSE
            PDS = S6
          ENDIF
        ELSE
          PDS = PDA(0)
        ENDIF
      ELSE IF(MSPR.EQ.14) THEN
        SSR = X1*X2*ECMP*ECMP
        IF(IDPDG1.EQ.22) THEN
          ALPHA1 = DBLE(ULALEM(SSR))
*         ALPHA1 = 1.D0/137.D0
        ELSE IF(IDPDG1.EQ.45) THEN
          ALPHA1 = PARMDL(74)
        ENDIF
        IF(IDPDG2.EQ.22) THEN
          ALPHA2 = DBLE(ULALEM(SSR))
*         ALPHA2 = 1.D0/137.D0
        ELSE IF(IDPDG2.EQ.45) THEN
          ALPHA2 = PARMDL(74)
        ENDIF
        PDS = 1.D0
      ELSE
        WRITE(6,'(/1X,A,I4)') 'POHWGH:ERROR:INVALID HARD PROCESS',MSPR
        CALL POABRT
      ENDIF
C  complete weight
      FDISTR  = XSECT(1,MSPR)*ALPHA1*ALPHA2*PDS
C  debug output
      IF(IDEB(15).GE.20) WRITE(6,'(1X,A,/5X,I3,2I6,4E10.3)')
     &    'POHWGH:DEBUG:MSPR,ID1,ID2,AL1,AL2,PDS,FDIS',
     &    MSPR,IDPDG1,IDPDG2,ALPHA1,ALPHA2,PDS,FDISTR
      END
C
C
      SUBROUTINE POHSCA(IMODE,IP)
C**********************************************************************
C
C     POHSCA determines the type of hard subprocess, the partons taking
C     part in subprocess and the kinematic variables
C
C     input:  IMODE   1   direct processes
C                     2   resolved processes
C                     -1  initialization
C                     -2  output of statistics
C             IP      1-4 level of hard interaction (hadron/photon)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER( ZERO = 0.D0,
     &           ONE  = 1.D0,
     &           EPS  = 1.D-10,
     &           DEPS = 1.D-30,
     &           TWO  = 2.D0 )
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      CHARACTER*18 PROC
      COMMON /PEPROC/ PROC(0:MAXPRO)
      COMMON /HAEVNT/ PT1,PT2,NHARD,NTRY,IHARD,ITRY,IREJEV
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      DIMENSION PDA(-6:6),PDB(-6:6)
      COMMON /HAEVTR/ LINE,LIN,LREC1(MLINE),LREC2(MLINE),PREC(4,MLINE)
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
C
 111  CONTINUE
C  resolved processes
      IF(IMODE.EQ.2) THEN
        MXSECT(0,0,IP) = 0
        XSECT(2,9)  = 0.D0
        DO 15 M=-1,8
          IF(MXSECT(0,M,IP).EQ.1) XSECT(2,9) = XSECT(2,9)+XSECT(2,M)
 15     CONTINUE
        IF(XSECT(2,9).LT.DEPS) THEN
          WRITE(6,'(/1X,A,I4)') 'POHSCA:ERROR:NO RES.PROCESS(IP)',IP
          CALL POABRT
        ENDIF
C
C ----------------------------------------------I
C  begin of iteration loop (resolved processes) I
C                                               I
        IREJSC = 0
 10     CONTINUE
        IREJSC = IREJSC+1
        IREJEV = IREJEV+1
C  find subprocess
        B      = DRNDM(X1)*XSECT(2,9)
        MSPR   =-2
        SUM    = 0.D0
 20     MSPR   = MSPR+1
        IF ( MXSECT(0,MSPR,IP).EQ.1 ) SUM = SUM+XSECT(2,MSPR)
        IF ( SUM.LT.B  .AND. MSPR.LT.8 ) GOTO 20
C  find kin. variables X1,X2 and V
        CALL POHKIN(IREJ)
        IF(IREJ.NE.0) THEN
          IFAIL(29) = IFAIL(29)+1
          IF(IREJSC.GT.100) THEN
            WRITE(6,'(/1X,A,I10)')
     &        'POHSCA:ERROR:TOO MANY REJECTIONS (resolved)',IREJSC
              CALL POABRT
          ENDIF
          GOTO 10
        ENDIF
C  calculate remaining distribution
        CALL POHWGH(PDS,PDA,PDB,F)
C  actualize counter for cross-section calculation
        IF( F .LE. 1.D-40 ) F=0.D0
        XSECT(5,MSPR) = XSECT(5,MSPR)+F
        XSECT(6,MSPR) = XSECT(6,MSPR)+F*F
        MXSECT(1,MSPR,IP) = MXSECT(1,MSPR,IP)+1
C  check F against FMAX
        WEIGHT = F/(XSECT(2,MSPR)+DEPS)
        IF ( WEIGHT.LT.DRNDM(X2) ) GOTO 10
C-------------------------------------------------------------------
         IF(WEIGHT.GT.1.D0) THEN
           WRITE(6,1234) MSPR,IP,IDPDG1,IDPDG2,F,XSECT(2,MSPR),WEIGHT
 1234      FORMAT(/,' POHSCA:WARNING:(resolved) W>1 (MSPR,IP,ID1,2)',
     &       2I3,2I7,/' F,XSECT(2,MSPR),W',3E12.4)
           WRITE(6,'(1X,A,5E11.3)') 'ECM,PTWANT,AS,AH,PT',
     &       ECMP,PTWANT,AS,AH,PT
           WRITE(6,'(1X,A,5E11.3)') 'ETAC,ETAD,X1,X2,V',
     &       ETAC,ETAD,X1,X2,V
           CALL POPREV(-1)
         ENDIF
C-------------------------------------------------------------------
C                                             I
C  end of iteration loop (resolved processes) I
C --------------------------------------------I
C
C*********************************************************************
C
C  direct processes
      ELSE IF(IMODE.EQ.1) THEN
        XSECT(2,15)  = 0.D0
        DO 25 M= 10,14
          IF(MXSECT(0,M,IP).EQ.1) XSECT(2,15)=XSECT(2,15)+XSECT(2,M)
 25     CONTINUE
        IF(XSECT(2,15).LT.DEPS) THEN
          WRITE(6,'(/1X,A,I4)') 'POHSCA:ERROR:NO DIR.PROCESS(IP)',IP
          CALL POABRT
        ENDIF
C
C ----------------------------------------------I
C  begin of iteration loop (direct processes)   I
C                                               I
        IREJSC = 0
 100    CONTINUE
        IREJSC = IREJSC+1
        IREJEV = IREJEV+1
C  find subprocess
        B      = DRNDM(X1)*XSECT(2,15)
        MSPR   = 9
        SUM    = 0.D0
 200    MSPR   = MSPR+1
        IF(MXSECT(0,MSPR,IP).EQ.1) SUM = SUM+XSECT(2,MSPR)
        IF ( SUM.LT.B  .AND. MSPR.LT.14 ) GOTO 200
C  find kin. variables X1,X2 and V
        CALL POHKIN(IREJ)
        IF(IREJ.NE.0) THEN
          IFAIL(28) = IFAIL(28)+1
          IF(IREJSC.GT.1000) THEN
            WRITE(6,'(/1X,A,I10)')
     &        'POHSCA:ERROR:TOO MANY REJECTIONS (direct)',IREJSC
              CALL POABRT
          ENDIF
          GOTO 100
        ENDIF
C  calculate remaining distribution
        CALL POHWGH(PDS,PDA,PDB,F)
C  actualize counter for cross-section calculation
        IF( F .LE. 1.D-15 ) F=0.D0
        XSECT (5,MSPR) = XSECT (5,MSPR)+F
        XSECT (6,MSPR) = XSECT (6,MSPR)+F*F
        MXSECT(1,MSPR,IP) = MXSECT(1,MSPR,IP)+1
C  check F against FMAX
        WEIGHT = F/(XSECT(2,MSPR)+DEPS)
        IF ( WEIGHT.LT.DRNDM(X2) ) GOTO 100
C-------------------------------------------------------------------
         IF(WEIGHT.GT.1.D0) THEN
           WRITE(6,1235) MSPR,IP,IDPDG1,IDPDG2,F,XSECT(2,MSPR),WEIGHT
 1235      FORMAT(/,' POHSCA:WARNING:(direct) W>1 (MSPR,IP,ID1,2)',2I3,
     &       2I7,/' F,XSECT(2,MSPR),W',3E12.4)
           WRITE(6,'(1X,A,5E11.3)') 'ECM,PTWANT,AS,AH,PT',
     &       ECMP,PTWANT,AS,AH,PT
           WRITE(6,'(1X,A,5E11.3)') 'ETAC,ETAD,X1,X2,V',
     &       ETAC,ETAD,X1,X2,V
           CALL POPREV(-1)
         ENDIF
C-------------------------------------------------------------------
C                                             I
C  end of iteration loop (direct processes)   I
C --------------------------------------------I
C
C  initialization
      ELSE IF(IMODE.EQ.-1) THEN
C  initialize cross section calculations
        DO 40 M=-1,MAXPRO
          DO 30 I=5,6
            XSECT(I,M) = ZERO
 30       CONTINUE
          DO 35 J=1,4
C  reset counters
            MXSECT(1,M,J) = 0
            MXSECT(2,M,J) = 0
 35       CONTINUE
 40     CONTINUE
        IF(IDEB(78).GE.0) THEN
          WRITE(6,'(/1X,A,/1X,A)')
     &      'POHSCA:DEBUG:ACTIVATED HARD PROCESSES',
     &      '====================================='
          WRITE(6,'(5X,A)') 'PROCESS,    IP= 1 ... 4 (on/off)'
          DO 42 M=1,MAXPRO
            WRITE(6,'(1X,I3,5X,A,4I3)') M,PROC(M),(MXSECT(0,M,J),J=1,4)
 42       CONTINUE
        ENDIF
        RETURN
      ELSE IF(IMODE.EQ.-2) THEN
        DO 43 K=1,4
          DO 44 J=1,4
            MXSECT(J,0,K)  = 0
            MXSECT(J,9,K)  = 0
            MXSECT(J,15,K) = 0
            MXSECT(J,3,K) = MXSECT(J,3,K)+MXSECT(J,-1,K)
            DO 45 M=1,8
              MXSECT(J,9,K) = MXSECT(J,9,K)+MXSECT(J,M,K)
 45         CONTINUE
            DO 46 M=10,14
              MXSECT(J,15,K) = MXSECT(J,15,K)+MXSECT(J,M,K)
 46         CONTINUE
            MXSECT(J,0,K) = MXSECT(J,9,K)+MXSECT(J,15,K)
 44       CONTINUE
 43     CONTINUE
        IF(IDEB(78).GE.1) THEN
          WRITE(6,'(/1X,A,/1X,A)')
     &      'POHSCA:DEBUG:INTERNAL REJECTION STATISTICS',
     &      '=========================================='
          DO 48 K=1,4
            IF(MXSECT(1,0,K).GT.0) THEN
              WRITE(6,'(5X,A,I3)')
     &          'PROCESS (SAMPLED/ACCEPTED) FOR IP:',K
              DO 47 M=0,MAXPRO
                WRITE(6,'(1X,I3,1X,A,2X,2I12,3F9.3)') M,PROC(M),
     &            MXSECT(1,M,K),MXSECT(2,M,K),(DBLE(MXSECT(J,M,K))
     &            /DBLE(MAX(1,MXSECT(J-1,M,K))),J=2,4)
 47           CONTINUE
            ENDIF
 48       CONTINUE
        ENDIF
        RETURN
      ELSE
        WRITE(6,'(/1X,A,I10)') 'POHSCA:ERROR:UNSUPPORTED MODE',IMODE
        CALL POABRT
      ENDIF
C
C  the event is accepted now
C  actualize counter for accepted events
      MXSECT(2,MSPR,IP) = MXSECT(2,MSPR,IP)+1
      IF(MSPR.EQ.-1) MSPR = 3
C
C  find initial partons
      SUM    = 0.D0
      SCHECK = DRNDM(SUM)*PDS-EPS
      IF     ( MSPR.EQ.1  .OR.  MSPR.EQ.4 ) THEN
        IA = 0
        IB = 0
      ELSEIF ( MSPR.EQ.2  .OR.  MSPR.EQ.5  .OR.  MSPR.EQ.6 ) THEN
        DO 610 IA=-NF,NF
          IF ( IA.EQ.0 ) GOTO 610
          SUM  = SUM+PDA(IA)*PDB(-IA)
          IF ( SUM.GE.SCHECK ) GOTO 620
 610      CONTINUE
 620    IB =-IA
      ELSEIF ( MSPR.EQ.3 ) THEN
        IB     = 0
        DO 630 IA=-NF,NF
          IF ( IA.EQ.0 ) GOTO 630
          SUM  = SUM+PDA(0)*PDB(IA)
          IF ( SUM.GE.SCHECK ) GOTO 640
          SUM  = SUM+PDA(IA)*PDB(0)
          IF ( SUM.GE.SCHECK ) GOTO 650
 630    CONTINUE
 640    IB     = IA
        IA     = 0
 650    CONTINUE
      ELSEIF ( MSPR.EQ.7 ) THEN
        DO 660 IA=-NF,NF
          IF ( IA.EQ.0 ) GOTO 660
          SUM  = SUM+PDA(IA)*PDB(IA)
          IF ( SUM.GE.SCHECK ) GOTO 670
 660      CONTINUE
 670    IB     = IA
      ELSEIF ( MSPR.EQ.8 ) THEN
        DO 690 IA=-NF,NF
          IF ( IA.EQ.0 ) GOTO 690
          DO 680 IB=-NF,NF
            IF ( ABS(IB).EQ.ABS(IA)  .OR.  IB.EQ.0 ) GOTO 680
            SUM = SUM+PDA(IA)*PDB(IB)
            IF ( SUM.GE.SCHECK ) GOTO 700
 680        CONTINUE
 690      CONTINUE
 700    CONTINUE
      ELSEIF ( MSPR.EQ.10 ) THEN
        IA     = 0
        DO 710 IB=-NF,NF
          IF ( IB.NE.0 ) THEN
            IF(IDPDG1.EQ.22) THEN
              IF(MOD(ABS(IB),2).EQ.0) THEN
                SUM = SUM+PDB(IB)*4.D0/9.D0
              ELSE
                SUM = SUM+PDB(IB)*1.D0/9.D0
              ENDIF
            ELSE
              SUM = SUM+PDB(IB)
            ENDIF
            IF ( SUM.GE.SCHECK ) GOTO 720
          ENDIF
 710    CONTINUE
 720    CONTINUE
      ELSEIF ( MSPR.EQ.12 ) THEN
        IB     = 0
        DO 810 IA=-NF,NF
          IF ( IA.NE.0 ) THEN
            IF(IDPDG2.EQ.22) THEN
              IF(MOD(ABS(IA),2).EQ.0) THEN
                SUM = SUM+PDA(IA)*4.D0/9.D0
              ELSE
                SUM = SUM+PDA(IA)*1.D0/9.D0
              ENDIF
            ELSE
              SUM = SUM+PDA(IA)
            ENDIF
            IF ( SUM.GE.SCHECK ) GOTO 820
          ENDIF
 810    CONTINUE
 820    CONTINUE
      ELSEIF ((MSPR.EQ.11).OR.(MSPR.EQ.13).OR.(MSPR.EQ.14)) THEN
        IA     = 0
        IB     = 0
      ENDIF
C  final check
      IF((ABS(IA).GT.NF).OR.(ABS(IB).GT.NF)) THEN
        print *,' final check IA,IB',IA,IB
        GOTO 111
      ENDIF
C
C  find final partons
      IC = IA
      ID = IB
      IF     ( MSPR.EQ.2 ) THEN
        IC = 0
        ID = 0
      ELSEIF ( MSPR.EQ.4 ) THEN
        IC = INT(FLOAT(NF+NF)*DRNDM(SUM))+1
        IF ( IC.GT.NF ) IC = NF-IC
        ID =-IC
      ELSEIF ( MSPR.EQ.6 ) THEN
        IC = INT(FLOAT(NF+NF-2)*DRNDM(SUM))+1
        IF ( IC.GT.NF-1 ) IC = NF-1-IC
        IF ( ABS(IC).EQ.ABS(IA) ) IC = SIGN(NF,IC)
        ID =-IC
      ELSEIF ( MSPR.EQ.11) THEN
        SUM = 0.D0
        DO 730 IC=-NF,NF
          IF ( IC.NE.0 ) THEN
            IF(IDPDG1.EQ.22) THEN
              IF(MOD(ABS(IC),2).EQ.0) THEN
                SUM = SUM + 4.D0
              ELSE
                SUM = SUM + 1.D0
              ENDIF
            ELSE
              SUM = SUM + 1.D0
            ENDIF
          ENDIF
 730    CONTINUE
        SCHECK = DRNDM(SUM)*SUM-EPS
        SUM = 0.D0
        DO 740 IC=-NF,NF
          IF ( IC.NE.0 ) THEN
            IF(IDPDG1.EQ.22) THEN
              IF(MOD(ABS(IC),2).EQ.0) THEN
                SUM = SUM + 4.D0
              ELSE
                SUM = SUM + 1.D0
              ENDIF
            ELSE
              SUM = SUM + 1.D0
            ENDIF
            IF ( SUM.GE.SCHECK ) GOTO 750
          ENDIF
 740    CONTINUE
 750    CONTINUE
        ID = -IC
      ELSEIF ( MSPR.EQ.13) THEN
        SUM = 0.D0
        DO 830 IC=-NF,NF
          IF ( IC.NE.0 ) THEN
            IF(IDPDG2.EQ.22) THEN
              IF(MOD(ABS(IC),2).EQ.0) THEN
                SUM = SUM + 4.D0
              ELSE
                SUM = SUM + 1.D0
              ENDIF
            ELSE
              SUM = SUM + 1.D0
            ENDIF
          ENDIF
 830    CONTINUE
        SCHECK = DRNDM(SUM)*SUM-EPS
        SUM = 0.D0
        DO 840 IC=-NF,NF
          IF ( IC.NE.0 ) THEN
            IF(IDPDG2.EQ.22) THEN
              IF(MOD(ABS(IC),2).EQ.0) THEN
                SUM = SUM + 4.D0
              ELSE
                SUM = SUM + 1.D0
              ENDIF
            ELSE
              SUM = SUM + 1.D0
            ENDIF
            IF ( SUM.GE.SCHECK ) GOTO 850
          ENDIF
 840    CONTINUE
 850    CONTINUE
        ID = -IC
      ELSEIF ( MSPR.EQ.14) THEN
        SUM = 0.D0
        DO 930 IC=1,NF
          FAC1 = 1.D0
          FAC2 = 1.D0
          IF(MOD(ABS(IC),2).EQ.0) THEN
            IF(IDPDG1.EQ.22) FAC1 = 4.D0
            IF(IDPDG2.EQ.22) FAC2 = 4.D0
          ENDIF
          SUM = SUM + FAC1*FAC2
 930    CONTINUE
        IF(IPAMDL(64).NE.0) THEN
          IF((IDPDG1.EQ.22).AND.(IDPDG2.EQ.22)) SUM = SUM + 81.D0
        ENDIF
        SCHECK = DRNDM(SUM)*SUM-EPS
        SUM = 0.D0
        DO 940 IC=1,NF
          FAC1 = 1.D0
          FAC2 = 1.D0
          IF(MOD(ABS(IC),2).EQ.0) THEN
            IF(IDPDG1.EQ.22) FAC1 = 4.D0
            IF(IDPDG2.EQ.22) FAC2 = 4.D0
          ENDIF
          SUM = SUM + FAC1*FAC2
          IF ( SUM.GE.SCHECK ) GOTO 950
 940    CONTINUE
        IC = 15
 950    CONTINUE
        ID = -IC
        IF(DRNDM(FAC1).GT.0.5D0) CALL SWAPI(IC,ID)
      ENDIF
      XM3 = POPAMA(IC,1)
      XM4 = POPAMA(ID,1)
      IF(ABS(IC).EQ.15) GOTO 955
C
C  valence quarks involved?
      IF(IA.NE.0) THEN
        IF(IDPDG1.EQ.22) THEN
          CALL POPDFB(IA,X1,QQPD,PVIRTP(1),FXP)
          IF(DRNDM(XMS3)*PDA(IA).GT.PDA(IA)-FXP) IA=SIGN(ABS(IA)+6,IA)
        ELSE
          IF(DRNDM(XMS3)*PDA(IA).GT.PDA(-IA)) IA = SIGN(ABS(IA)+6,IA)
        ENDIF
      ENDIF
      IF(IB.NE.0) THEN
        IF(IDPDG2.EQ.22) THEN
          CALL POPDFB(IB,X2,QQPD,PVIRTP(2),FXP)
          IF(DRNDM(XMS3)*PDB(IB).GT.PDB(IB)-FXP) IB=SIGN(ABS(IB)+6,IB)
        ELSE
          IF(DRNDM(XMS4)*PDB(IB).GT.PDB(-IB)) IB = SIGN(ABS(IB)+6,IB)
        ENDIF
      ENDIF
C  fill event record
 955  CONTINUE
      CALL SFECFE(SINPHI,COSPHI)
      ECM2 = ECMP/2.D0
C
      LINE          = LINE+1
      PREC(1,LINE)  = 0.D0
      PREC(2,LINE)  = 0.D0
      PREC(3,LINE)  = ECM2*X1
      PREC(4,LINE)  = PREC(3,LINE)
      LREC1(LINE)   = IA+50+100*MSPR
      LREC2(LINE)   = 01000
      LINE          = LINE+1
      PREC(1,LINE)  = 0.D0
      PREC(2,LINE)  = 0.D0
      PREC(3,LINE)  =-ECM2*X2
      PREC(4,LINE)  =-PREC(3,LINE)
      LREC1(LINE)   = IB+50
      LREC2(LINE)   = 01000
      LINE          = LINE+1
      PREC(1,LINE)  = PT*COSPHI
      PREC(2,LINE)  = PT*SINPHI
      PREC(3,LINE)  = -ECM2*(U*X1-V*X2)
      PREC(4,LINE)  = -ECM2*(U*X1+V*X2)
      LREC1(LINE)   = IC+50
      LREC2(LINE)   = 11000
      LINE          = LINE+1
      PREC(1,LINE)  = -PREC(1,LINE-1)
      PREC(2,LINE)  = -PREC(2,LINE-1)
      PREC(3,LINE)  = -ECM2*(V*X1-U*X2)
      PREC(4,LINE)  = -ECM2*(V*X1+U*X2)
      LREC1(LINE)   = ID+50
      LREC2(LINE)   = 11000
C  convert to mass shell
      CALL MSHELL(PREC(1,LINE-1),PREC(1,LINE),XM3,XM4,
     &  PREC(1,LINE-1),PREC(1,LINE),IREJ)
      IF(IREJ.NE.0) THEN
        IF(IDEB(78).GE.5) WRITE(6,'(1X,A,3E12.4)')
     &    'POHSCA:WARNING:MSHELL REJECTION (PT,M1,M2)',PT,XM3,XM4
        LINE = LINE-4
        GOTO 111
      ENDIF
C  debug output
      IF(IDEB(78).GE.20) THEN
        SHAT = X1*X2*ECMP*ECMP
        WRITE(6,'(1X,A,4I4)') 'POHSCA:DEBUG:IA,IB,IC,ID',IA,IB,IC,ID
        WRITE(6,'(5X,A,I4,2E11.3)') 'LINE, INITIAL X ',LINE,X1,X2
        WRITE(6,'(5X,A,4E11.3)') 'U,V,PT,SHAT ',U,V,PT,SHAT
      ENDIF
      END
C
C
      SUBROUTINE POHFAC(PTCUT,ECMI)
C*********************************************************************
C
C     initialization: find scaling factors and maxima of remaining
C                     weights
C
C     input:   PTCUT  transverse momentum cutoff
C              ECMI   cms energy
C
C     output:  XSECT  field for sampling hard processes
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( MAXPRO = 16 )
      PARAMETER ( MXABWT = 96 )
      PARAMETER ( ZERO = 0.D0,
     &            ONE  = 1.D0 )
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      DIMENSION       ABSZ(MXABWT),WEIG(MXABWT)
      DIMENSION S(-1:MAXPRO),S1(-1:MAXPRO),S2(-1:MAXPRO),
     &          F124(-1:MAXPRO)
      DATA F124 / 1.D0,0.D0,
     &            4.D0,2.D0,2.D0,2.D0,4.D0,1.D0,4.D0,4.D0,0.D0,1.D0,
     &            2.D0,1.D0,2.D0,1.D0,0.D0,1.D0 /
C
      SS     = ECMI*ECMI
      AH     = (2.D0*PTCUT/ECMI)**2
      ALN    = LOG(AH)
      HLN    = LOG(0.5D0)
      NPOINT = NGAUIN
      CALL PHGSET(ZERO,ONE,NPOINT,ABSZ,WEIG)
      DO 10 M=-1,MAXPRO
        S1(M) = ZERO
10    CONTINUE
C
C  resolved processes
      DO 80 I1=1,NPOINT
        Z1   = ABSZ(I1)
        X1   = EXP(ALN*Z1)
        DO 20 M=-1,9
          S2(M) = ZERO
20      CONTINUE
C
        DO 60 I2=1,NPOINT
          Z2    = (1.D0-Z1)*ABSZ(I2)
          X2    = EXP(ALN*Z2)
          FAXX  = AH/(X1*X2)
          W     = SQRT(1.D0-FAXX)
          W1    = FAXX/(1.+W)
          WLOG  = LOG(W1)
          FWW   = FAXX*WLOG/W
          DO 30 M=-1,9
            S(M) = 0.D0
30        CONTINUE
C
          DO 40 I=1,NPOINT
            Z   = ABSZ(I)
            VA  =-0.5D0*W1/(W1+Z*W)
            UA  =-1.D0-VA
            VB  =-0.5D0*FAXX/(W1+2.D0*W*Z)
            UB  =-1.D0-VB
            VC  =-EXP(HLN+Z*WLOG)
            UC  =-1.D0-VC
            VE  =-0.5D0*(1.D0+W)+Z*W
            UE  =-1.D0-VE
            S(1)  = S(1)+(1.+W)*2.25*(VA*VA*(3.-UA*VA-VA/(UA*UA))-UA)*
     &           WEIG(I)
            S(2)  = S(2)+(VC*VC+UC*UC)*((16./27.)/UC-(4./3.)*VC)*FWW*
     &            WEIG(I)
            S(3)  = S(3)+(1.+W)*(1.+UA*UA)*(1.-(4./9.)*VA*VA/UA)*WEIG(I)
            S(5)  = S(5)+((4./9.)*(1.+UB*UB+(UB*UB+VB*VB)*VB*VB)-
     &            (8./27.)*UA*UA*VA)*WEIG(I)
            S(6)  = S(6)+(4./9.)*(UE*UE+VE*VE)*FAXX*WEIG(I)
            S(7)  = S(7)+(1.+W)*((2./9.)*(1.+UA*UA+(1.+VA*VA)*VA*VA/
     &            (UA*UA))-(4./27.)*VA/UA)*WEIG(I)
            S(8)  = S(8)+(4./9.)*(1.+UB*UB)*WEIG(I)
            S(-1) = S(-1)+(1.+VC*VC)*(VC/(UC*UC)-(4./9.))*FWW*WEIG(I)
40        CONTINUE
          S(4)    = S(2)*(9./32.)
          DO 50 M=-1,8
            S2(M) = S2(M)+S(M)*WEIG(I2)*W
50        CONTINUE
60      CONTINUE
        DO 70 M=-1,8
          S1(M) = S1(M)+S2(M)*(1.D0-Z1)*WEIG(I1)
70      CONTINUE
80    CONTINUE
      S1(4) = S1(4)*NF
      S1(6) = S1(6)*MAX(0,NF-1)
C
C  direct processes
      IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)
     &   .OR.(IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
        DO 180 I1=1,NPOINT
          Z2   = ABSZ(I1)
          X2   = EXP(ALN*Z2)
          FAXX  = AH/X2
          W     = SQRT(1.D0-FAXX)
          W1    = FAXX/(1.D0+W)
          WLOG  = LOG(W1)
          WL = LOG(FAXX/(1.D0+W)**2)
          FWW1  = FAXX*WL/ALN
          FWW2  = FAXX*WLOG/ALN
          DO 130 M=10,12
            S(M) = ZERO
 130      CONTINUE
C
          DO 140 I=1,NPOINT
            Z   = ABSZ(I)
            UA  =-(1.D0+W)/2.D0*EXP(Z*WL)
            VA  =-1.D0-UA
            VB  =-EXP(HLN+Z*WLOG)
            UB  =-1.D0-VB
            S(10)  = S(10)+(8.D0/3.D0)*(1.D0+UA*UA)*WEIG(I)*FWW1
            S(11) = S(11)-(VB*VB+UB*UB)/UB*WEIG(I)*FWW2
 140      CONTINUE
          DO 170 M=10,11
            S1(M) = S1(M)+S(M)*WEIG(I1)
 170      CONTINUE
 180    CONTINUE
        S1(12) = S1(10)
        S1(13) = S1(11)
C  quark charges fractions
        IF(IDPDG1.EQ.22) THEN
          CHRNF = 0.D0
          CH1 = 4.D0/9.D0
          CH2 = 3.D0/9.D0
          DO 100 I=1,NF
            CHRNF = CHRNF + CH1-CH2*MOD(I,2)
 100      CONTINUE
          S1(11) = S1(11)*CHRNF
        ELSE IF(IDPDG1.EQ.45) THEN
          S1(11) = S1(11)*NF
        ELSE
          S1(11) = 0.D0
        ENDIF
        IF(IDPDG2.EQ.22) THEN
          CHRNF = 0.D0
          CH1 = 4.D0/9.D0
          CH2 = 3.D0/9.D0
          DO 200 I=1,NF
            CHRNF = CHRNF + CH1-CH2*MOD(I,2)
 200      CONTINUE
          S1(13) = S1(13)*CHRNF
        ELSE IF(IDPDG2.EQ.45) THEN
          S1(13) = S1(13)*NF
        ELSE
          S1(13) = 0.D0
        ENDIF
      ENDIF
C
C  global factors
      FFF    = PI*GEV2MB*ALN*ALN/(AH*SS)
      DO 90 M=-1,MAXPRO
        XSECT(1,M) = MAX(FFF*F124(M)*S1(M),ZERO)
90    CONTINUE
C
C  double direct process
      IF(((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45))
     &   .AND.((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45))) THEN
        FAC = 0.D0
        DO 300 I=1,NF
          IF(IDPDG1.EQ.22) THEN
            F1 = (4.D0-3.D0*MOD(I,2))/9.D0
          ELSE
            F1 = 1.D0
          ENDIF
          IF(IDPDG2.EQ.22) THEN
            F2 = (4.D0-3.D0*MOD(I,2))/9.D0
          ELSE
            F2 = 1.D0
          ENDIF
          FAC = FAC+F1*F2*3.D0
 300    CONTINUE
        ZZ = SQRT(1.D0-4.D0*PTCUT*PTCUT/SS)
        XSECT(1,14) = 4.D0*PI/SS*(LOG((1.D0+ZZ)/(1.D0-ZZ))-ZZ)
     &               *GEV2MB*FAC
      ENDIF
      END
C
C
      SUBROUTINE POHWGX(PTCUT,ECM)
C**********************************************************************
C
C     find maximum of remaining weight for MC sampling
C
C     input:   PTCUT  transverse momentum cutoff
C              ECM    cms energy
C
C     output:  XSECT  field for sampling hard processes
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER ( MAXPRO = 16 , MLINE = 400 , MSCAHD = 50 )
      PARAMETER ( NKM = 10 )
      PARAMETER ( TINY = 1.D-20,
     &            ZERO = 0.D0 )
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      DIMENSION Z(3),D(3),FF(0:NKM),PDA(-6:6),PDB(-6:6),XM1(NKM),
     &  XM2(NKM),PTM(NKM),ZMX(3,NKM),DMX(3,NKM),IMX(NKM),IPO(NKM)
      DIMENSION IFTAB(-1:MAXPRO)
      DATA IFTAB  / 4,0,1,2,4,1,2,2,3,5,0,6,7,8,9,10,0,10 /
C
C  initial settings
      AH    = (2.D0*PTCUT/ECM)**2
      ALNH  = LOG(AH)
      FF(0) = ZERO
      DO 22 I=1,NKM
        FF(I) = ZERO
        XM1(I) = ZERO
        XM2(I) = ZERO
        PTM(I) = ZERO
        ZMX(1,I) = ZERO
        ZMX(2,I) = ZERO
        ZMX(3,I) = ZERO
        DMX(1,I) = ZERO
        DMX(2,I) = ZERO
        DMX(3,I) = ZERO
        IMX(I) = 0
        IPO(I) = 0
 22   CONTINUE
C
      NKML = 10
      DO 40 NKON=1,NKML
C
        DO 50 IST=1,3
C  start configuration
        IF(IST.EQ.1) THEN
          Z(1) = MIN(0.999D0,LOG(0.5D0)/LOG(AH))
          Z(2) = 0.5
          Z(3) = 0.1
          D(1) =-0.5
          D(2) = 0.5
          D(3) = 0.5
        ELSE IF(IST.EQ.2) THEN
          Z(1) = 0.999D0
          Z(2) = 0.5
          Z(3) = 0.0
          D(1) =-0.5
          D(2) = 0.5
          D(3) = 0.5
        ELSE IF(IST.EQ.3) THEN
          Z(1) = MIN(0.999D0,LOG(0.5D0)/LOG(AH))
          Z(2) = 0.1
          Z(3) = 0.1
          D(1) =-0.5
          D(2) = 0.5
          D(3) = 0.5
        ELSE IF(IST.EQ.4) THEN
          Z(1) = MIN(0.999D0,LOG(0.5D0)/LOG(AH))
          Z(2) = 0.9
          Z(3) = 0.1
          D(1) =-0.5
          D(2) = 0.5
          D(3) = 0.5
        ENDIF
        IT   = 0
        CALL POHWGI(ECM,PTCUT,NKON,Z,F2)
C  process possible?
        IF(F2.LE.ZERO) GOTO 35
C
 10     CONTINUE
          IT   = IT+1
          FOLD = F2
          DO 30 I=1,3
            D(I) = D(I)/5.D0
            Z(I)   = Z(I)+D(I)
            CALL POHWGI(ECM,PTCUT,NKON,Z,F3)
            IF ( F2.GT.F3 ) Z(I) = Z(I)-D(I)
            IF ( F2.GT.F3 ) D(I) =-D(I)
 20         CONTINUE
              F1   = MIN(F2,F3)
              F2   = MAX(F2,F3)
              Z(I) = Z(I)+D(I)
              CALL POHWGI(ECM,PTCUT,NKON,Z,F3)
            IF ( F3.GT.F2 ) GOTO 20
            ZZ     = Z(I)-D(I)
            Z(I)   = ZZ+0.5*D(I)*(F3-F1)/MAX(TINY,F2+F2-F1-F3)
            IF ( ABS(ZZ-Z(I)).GT.D(I)*0.1D0 )
     &        CALL POHWGI(ECM,PTCUT,NKON,Z,F1)
            IF ( F1.LE.F2 ) Z(I) = ZZ
            F2     = MAX(F1,F2)
 30       CONTINUE
        IF((ABS(FOLD-F2)/MAX(TINY,F2).GT.0.002D0).OR.(IT.LT.3)) GOTO 10
C
        IF(F2.GT.FF(NKON)) THEN
          FF(NKON)  = MAX(F2,ZERO)
          XM1(NKON) = X1
          XM2(NKON) = X2
          PTM(NKON) = PT
          ZMX(1,NKON) = Z(1)
          ZMX(2,NKON) = Z(2)
          ZMX(3,NKON) = Z(3)
          DMX(1,NKON) = D(1)
          DMX(2,NKON) = D(2)
          DMX(3,NKON) = D(3)
          IMX(NKON) = IT
          IPO(NKON) = IST
        ENDIF
C
 50     CONTINUE
 35     CONTINUE
 40   CONTINUE
C  debug output
      IF(IDEB(38).GE.5) THEN
        WRITE(6,'(/1X,A)')
     &    'POHWGX:DEBUG:MAXIMUM OF WEIGHT (I,IT,IS,FF,Z(1-3),D(1-3))'
        DO 60 I=1,NKM
          IF(IMX(I).NE.0) WRITE(6,'(1X,I2,I3,I2,7E10.3)') I,IMX(I),
     &      IPO(I),FF(I),ZMX(1,I),ZMX(2,I),ZMX(3,I),DMX(1,I),
     &      DMX(2,I),DMX(3,I)
 60     CONTINUE
      ENDIF
C
      DO 70 I=-1,MAXPRO
        XSECT(2,I)  = MAX(FF(IFTAB(I))*XSECT(1,I),ZERO)
 70   CONTINUE
C  debug output
      IF(IDEB(38).GE.5) THEN
        WRITE(6,'(/1X,A)') 'POHWGX:DEBUG:TOTAL WEIGHTS'
        WRITE(6,'(5X,A)') 'I    X1   X2   PT   XSECT(2,I)  FDIS'
        DO 80 I=-1,MAXPRO
          IF((IFTAB(I).NE.0).AND.(XSECT(2,I).GT.ZERO)) THEN
            MSPR = I
            X1 = MIN(XM1(IFTAB(I)),0.9999999999D0)
            X2 = MIN(XM2(IFTAB(I)),0.9999999999D0)
            PT = PTM(IFTAB(I))
            CALL POHWGH(PDS,PDA,PDB,FDIS)
            WRITE(6,'(1X,I3,5E12.3)') I,X1,X2,PT,XSECT(2,I),FDIS
          ENDIF
 80     CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE POHWGI(ECMX,PTCUT,NKON,Z,FDIS)
C**********************************************************************
C
C     auxiliary subroutine to find maximum of remaining weight
C
C     input:  ECMX   current CMS energy
C             PTCUT  current pt cutoff
C             NKON   process label  1..5  resolved
C                                   6..7  direct particle 1
C                                   8..9  direct particle 2
C                                   10    double direct
C             Z(3)   transformed variable
C
C     output: remaining weight
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION Z(3)
C
      PARAMETER ( NKM   = 10 )
      PARAMETER ( TINY  = 1.D-30,
     &            ONE   = 1.D0,
     &            TINY6 = 1.D-06,
     &            ZERO  = 0.D0 )
      PARAMETER (NMAXD=100)
C
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      DIMENSION F(NKM),PDA(-6:6),PDB(-6:6)
**sr 29.04. commented to use dp-JETSET
      REAL ULALEM,SSR,QQALR
**
C
      FDIS = ZERO
C
      IF(IDEB(64).GE.25) WRITE(6,'(1X,A,/5X,5E12.3,I5)')
     &  'POHWGI:DEBUG:ECM,PT,Z,NK',ECMX,PTCUT,Z,NKON
C  check input values
      IF ( Z(1).LT.0.D0  .OR.  Z(1).GT.1.D0 ) RETURN
      IF ( Z(2).LT.0.D0  .OR.  Z(2).GT.1.D0 ) RETURN
      IF ( Z(3).LT.0.D0  .OR.  Z(3).GT.1.D0 ) RETURN
C  transformations
      Y1    = EXP(ALNH*Z(1))
      IF(NKON.LE.5) THEN
C  resolved kinematic
        Y2  =-(1.D0-Y1)+2.D0*(1.D0-Y1)*Z(2)
        X1  = 0.5D0*(Y2+SQRT(Y2*Y2+4.D0*Y1))
        X2  = X1-Y2
        X1 = MIN(X1,0.999999999999D0)
        X2 = MIN(X2,0.999999999999D0)
      ELSE IF(NKON.LE.7) THEN
C  direct kinematic 1
        X1 = ONE
        X2 = MIN(Y1,0.999999999999D0)
      ELSE IF(NKON.LE.9) THEN
C  direct kinematic 2
        X1 = MIN(Y1,0.999999999999D0)
        X2 = ONE
      ELSE
C  double direct kinematic
        X1 = ONE
        X2 = ONE
      ENDIF
      W   = SQRT(MAX(TINY,1.D0-AH/Y1))
      V   =-0.5D0+W*(Z(3)-0.5D0)
      U   =-(1.D0+V)
      PT  = MAX(SQRT(U*V*Y1*ECMX*ECMX),PTCUT)
C
C  set hard scale  QQ  for alpha and partondistr.
      IF     ( NQQAL.EQ.1 ) THEN
        QQAL = AQQAL*PT*PT
      ELSEIF ( NQQAL.EQ.2 ) THEN
        QQAL = AQQAL*Y1*ECMX*ECMX*U*V/(1.+V*V+U*U)
      ELSEIF ( NQQAL.EQ.3 ) THEN
        QQAL = AQQAL*Y1*ECMX*ECMX
      ELSEIF ( NQQAL.EQ.4 ) THEN
        QQAL = AQQAL*Y1*ECMX*ECMX*(U*V)**(1./3.)
      ENDIF
      IF     ( NQQPD.EQ.1 ) THEN
        QQPD = AQQPD*PT*PT
      ELSEIF ( NQQPD.EQ.2 ) THEN
        QQPD = AQQPD*Y1*ECMX*ECMX*U*V/(1.+V*V+U*U)
      ELSEIF ( NQQPD.EQ.3 ) THEN
        QQPD = AQQPD*Y1*ECMX*ECMX
      ELSEIF ( NQQPD.EQ.4 ) THEN
        QQPD = AQQPD*Y1*ECMX*ECMX*(U*V)**(1./3.)
      ENDIF
C
      IF(NKON.LE.5) THEN
        DO 10 N=1,5
          F(N) = ZERO
 10     CONTINUE
C  resolved processes
        ALPHA1 = (BQCD/LOG(MAX(QQAL/(PDFLAM(1)*PDFLAM(2)),1.1D0)))
        ALPHA2 = ALPHA1
        CALL PDIS(X1,QQPD,1,PDA)
        CALL PDIS(X2,QQPD,2,PDB)
C  calculate full distribution FDIS
        DO 20 I=1,NF
          F(2) = F(2)+PDA(I)*PDB(-I)+PDA(-I)*PDB( I)
          F(3) = F(3)+PDA(I)*PDB( I)+PDA(-I)*PDB(-I)
          F(4) = F(4)+PDA(I)+PDA(-I)
          F(5) = F(5)+PDB(I)+PDB(-I)
20      CONTINUE
        F(1)   = PDA(0)*PDB(0)
        T      = PDA(0)*F(5)+PDB(0)*F(4)
        F(5)   = F(4)*F(5)-(F(2)+F(3))
        F(4)   = T
      ELSE IF(NKON.LE.7) THEN
C  direct processes particle 1
        IF(IDPDG1.EQ.22) THEN
          QQALR = QQAL
          ALPHA1 = DBLE(ULALEM(QQALR))
*         ALPHA1 = 1.D0/137.D0
          CH1 = 4.D0/9.D0
          CH2 = 3.D0/9.D0
        ELSE IF(IDPDG1.EQ.45) THEN
          ALPHA1 = PARMDL(74)
          CH1 = ONE
          CH2 = ZERO
        ELSE
          FDIS = -1.D0
          RETURN
        ENDIF
        ALPHA2 = BQCD/LOG(MAX(QQAL/PDFLAM(2)**2,1.1D0))
        CALL PDIS(X2,QQPD,2,PDB)
        F(6) = ZERO
        DO 30 I=1,NF
          F(6) = F(6)+(PDB(I)+PDB(-I))*(CH1-CH2*MOD(I,2))
 30     CONTINUE
        F(7)   = PDB(0)
      ELSE IF(NKON.LE.9) THEN
C  direct processes particle 2
        ALPHA1 = BQCD/LOG(MAX(QQAL/PDFLAM(1)**2,1.1*ONE))
        IF(IDPDG2.EQ.22) THEN
          QQALR = QQAL
          ALPHA2 = DBLE(ULALEM(QQALR))
*         ALPHA2 = 1.D0/137.D0
          CH1 = 4.D0/9.D0
          CH2 = 3.D0/9.D0
        ELSE IF(IDPDG2.EQ.45) THEN
          ALPHA2 = PARMDL(74)
          CH1 = ONE
          CH2 = ZERO
        ELSE
          FDIS = -1.D0
          RETURN
        ENDIF
        CALL PDIS(X1,QQPD,1,PDA)
        F(8) = ZERO
        DO 40 I=1,NF
          F(8) = F(8)+(PDA(I)+PDA(-I))*(CH1-CH2*MOD(I,2))
 40     CONTINUE
        F(9)   = PDA(0)
      ELSE
C  double direct process
        SSR = ECMX*ECMX
        IF(IDPDG1.EQ.22) THEN
          ALPHA1 = DBLE(ULALEM(SSR))
*         ALPHA1 = 1.D0/137.D0
        ELSE IF(IDPDG1.EQ.45) THEN
          ALPHA1 = PARMDL(74)
        ELSE
          FDIS = -1.D0
          RETURN
        ENDIF
        IF(IDPDG2.EQ.22) THEN
          ALPHA1 = DBLE(ULALEM(SSR))
*         ALPHA2 = 1.D0/137.D0
        ELSE IF(IDPDG2.EQ.45) THEN
          ALPHA2 = PARMDL(74)
        ELSE
          FDIS = -1.D0
          RETURN
        ENDIF
        F(10) = ONE
      ENDIF
C
      FDIS   = MAX(ZERO,F(NKON)*ALPHA1*ALPHA2)
C  debug output
      IF(IDEB(64).GE.20) WRITE(6,'(1X,A,/2X,I3,2I6,7E11.3)')
     &  'POHWGI:DEBUG:NKON,ID1,ID2,AL1,AL2,X1,X2,PT,F(NKON),FDIS',
     &  NKON,IDPDG1,IDPDG2,ALPHA1,ALPHA2,X1,X2,PT,F(NKON),FDIS
      END
C
C
      SUBROUTINE POHINI(IP,IDP1,IDP2,PV1,PV2,NOUT,MODE)
C**********************************************************************
C
C     initialize calculation of hard cross section
C
C     must not be called during MC generation
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( ZERO   = 0.D0,
     &            DEPS   = 1.D-10,
     &            ONE    = 1.D0 )
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
      PARAMETER ( MAXPRO = 16 )
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
C  initialize COMMON /HAPARA/
      BQCD   = PI4/(11.D0-(2.D0/3.D0)*NF)
C
C  set local Pomeron common
      IDPDG1    = IDP1
      IDPDG2    = IDP2
      PVIRTP(1) = PV1
      PVIRTP(2) = PV2
C  initialize PDFs
      CALL ACTPDF(IDPDG1,PVIRTP(1),1)
      CALL ACTPDF(IDPDG2,PVIRTP(2),2)
      Q0SQR  = MAX(PDFQ2M(1),PDFQ2M(2))
C  initialize scales with defaults
      IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)) THEN
        IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
          AQQAL  = PARMDL(83)
          AQQALI = PARMDL(86)
          AQQALF = PARMDL(89)
          AQQPD  = PARMDL(92)
          NQQAL  = IPAMDL(83)
          NQQALI = IPAMDL(86)
          NQQALF = IPAMDL(89)
          NQQPD  = IPAMDL(92)
        ELSE
          AQQAL  = PARMDL(82)
          AQQALI = PARMDL(85)
          AQQALF = PARMDL(88)
          AQQPD  = PARMDL(91)
          NQQAL  = IPAMDL(82)
          NQQALI = IPAMDL(85)
          NQQALF = IPAMDL(88)
          NQQPD  = IPAMDL(91)
        ENDIF
      ELSE IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
        AQQAL  = PARMDL(82)
        AQQALI = PARMDL(85)
        AQQALF = PARMDL(88)
        AQQPD  = PARMDL(91)
        NQQAL  = IPAMDL(82)
        NQQALI = IPAMDL(85)
        NQQALF = IPAMDL(88)
        NQQPD  = IPAMDL(91)
      ELSE
        AQQAL  = PARMDL(81)
        AQQALI = PARMDL(84)
        AQQALF = PARMDL(87)
        AQQPD  = PARMDL(90)
        NQQAL  = IPAMDL(81)
        NQQALI = IPAMDL(84)
        NQQALF = IPAMDL(87)
        NQQPD  = IPAMDL(90)
      ENDIF
      IF(PARMDL(109+IP).LT.DEPS) PARMDL(109+IP) = AQQAL
      IF(PARMDL(113+IP).LT.DEPS) PARMDL(113+IP) = AQQALI
      IF(PARMDL(117+IP).LT.DEPS) PARMDL(117+IP) = AQQALF
      IF(PARMDL(121+IP).LT.DEPS) PARMDL(121+IP) = AQQPD
      IF(IPAMDL(64+IP).LT.0) IPAMDL(64+IP) = NQQAL
      IF(IPAMDL(68+IP).LT.0) IPAMDL(68+IP) = NQQALI
      IF(IPAMDL(72+IP).LT.0) IPAMDL(72+IP) = NQQALF
      IF(IPAMDL(76+IP).LT.0) IPAMDL(76+IP) = NQQPD
      AQQAL  = PARMDL(109+IP)
      AQQALI = PARMDL(113+IP)
      AQQALF = PARMDL(117+IP)
      AQQPD  = PARMDL(121+IP)
      NQQAL  = IPAMDL(64+IP)
      NQQALI = IPAMDL(68+IP)
      NQQALF = IPAMDL(72+IP)
      NQQPD  = IPAMDL(76+IP)
      PTCUT(1) = PARMDL(36)
      PTCUT(2) = PARMDL(37)
      PTCUT(3) = PARMDL(38)
      PTCUT(4) = PARMDL(39)
C  write out all settings
      IF((IDEB(66).GE.15).OR.(MODE.GT.0)) THEN
        WRITE(NOUT,1050) IP,IDPDG1,IDPDG2,PTCUT,
     &    PDFNAM(1),IGRP(1),ISET(1),IEXT(1),
     &    PDFNAM(2),IGRP(2),ISET(2),IEXT(2),
     &    PDFLAM,Q0SQR,NF,NQQAL,AQQAL,NQQPD,AQQPD
1050    FORMAT(/,
     &    ' POHINI:DEBUG:HARD SCATTERING PARAMETERS:',/,
     &    ' =======================================',/,
     &    5X,'IP, PARTICLE1 / PARTICLE2:',I3,2I8,/,
     &    5X,'MIN. PT   :',4F8.1,/,
     &    5X,'PDF SIDE 1:',2X,A8,' IGRP/ISET/IEXT ',3I4,/,
     &    5X,'PDF SIDE 2:',2X,A8,' IGRP/ISET/IEXT ',3I4,/,
     &    5X,'LAMBDA1,2 :',2F15.3,/,
     &    5X,'Q0**2     :',F15.3,/,
     &    5X,'NF        :',I15,/,
     &    5X,'NQQAL     :',I15,/,
     &    5X,'AQQAL     :',F15.3,/,
     &    5X,'NQQPD     :',I15,/,
     &    5X,'AQQPD     :',F15.3,/)
      ENDIF
      END
C
C
      SUBROUTINE POHINT(IP,ECM,I1,I2,K1,K2,MSPOM)
C**********************************************************************
C
C     interpolate cross sections and weights for hard scattering
C
C     input:  IP     particle combination
C             ECM    CMS energy
C             I1     first subprocess to calculate
C             I2     last subprocess to calculate
C                    <-1  only scales and cutoffs calculated
C             K1     first variable to calculate
C             K2     last variable to calculate
C             MSPOM  cross sections to use for pt distribution
C                    0  reggeon
C                    >0 pomeron
C
C             for K1 < 3 the soft pt distribution is also calculated
C
C     output: interpolated values in XSECT(6,-1:MAXPRO)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (OHALF  = 0.5D0,
     &           ZERO   = 0.D0,
     &           ONE    = 1.D0,
     &           TWO    = 2.D0,
     &           DEPS   = 1.D-15,
     &           DEPS2  = 2.D-15,
     &           PLARGE = 1.D20 )
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /REGGE/  ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC
      COMMON /TWOCHA/ PHISUP(2),RMASS1,RMASS2,VAR
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
C  hard scales
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
C
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      PARAMETER ( MAXPRO = 16 )
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /HAEVNT/ PT1,PT2,NHARD,NTRY,IHARD,ITRY,IREJEV
C
      DIMENSION XP(2)
C
C  running cutoff
      IF(IPAMDL(7).EQ.1) THEN
        PTCUT(IP) = PARMDL(35+IP)
     &             +MAX(ZERO,0.12D0*(LOG10(ECM/50.D0))**3)
      ELSE
        PTCUT(IP) = PARMDL(35+IP)
      ENDIF
C  cutoffs
      PT1    = PTCUT(IP)
      PTWANT = PTCUT(IP)
C  scales for hard scattering
      AQQAL  = PARMDL(109+IP)
      AQQALI = PARMDL(113+IP)
      AQQALF = PARMDL(117+IP)
      AQQPD  = PARMDL(121+IP)
      NQQAL  = IPAMDL(64+IP)
      NQQALI = IPAMDL(68+IP)
      NQQALF = IPAMDL(72+IP)
      NQQPD  = IPAMDL(76+IP)
      IF(IDEB(58).GE.15) THEN
        WRITE(6,'(1X,A,4I3,4E10.3)') 'POHINT:DEBUG:SCALES',
     &    NQQAL,NQQALI,NQQALF,NQQPD,AQQAL,AQQALI,AQQALF,AQQPD
      ENDIF
      IF(I2.LT.-1) RETURN
C
C  double-log interpolation
      IF(ECM.LT.2.1D0*PTCUT(IP)) THEN
        DO 50 M=I1,I2
          DO 60 K=K1,K2
            XSECT(K,M) = ZERO
 60       CONTINUE
 50     CONTINUE
      ELSE
        I=1
 310    CONTINUE
          I = I+1
        IF((ECM.GT.ECMSH(IP,I)).AND.(I.LT.ISTTAB)) GOTO 310
        FAC = (LOG(ECM)-LOG(ECMSH(IP,I-1)))
     &        /(LOG(ECMSH(IP,I))-LOG(ECMSH(IP,I-1)))
        DO 100 M=I1,I2
          DO 200 K=K1,K2
            XX = LOG(XSECTA(K,M,IP,I-1)+DEPS)
     &      +LOG((XSECTA(K,M,IP,I)+DEPS)/(XSECTA(K,M,IP,I-1)+DEPS))*FAC
            XX = EXP(XX)
            IF(XX.LT.DEPS2) XX = ZERO
            IF(K.EQ.2) XX = XX*1.2D0
            XSECT(K,M) = XX
 200      CONTINUE
 100    CONTINUE
      ENDIF
C
      IF((K1.LT.3).AND.(K2.GE.3)) THEN
C  calculate slope of soft pt distribution
        IF(IP.EQ.1) THEN
          SIGS = GR(1)*GR(2)*ECM**(TWO*(ALREG-ONE))
     &          +GP(1)*GP(2)*ECM**(TWO*(ALPOM-ONE))
*         SIGH   = (XSECT(3,15)+XSECT(3,9))
          SIGH   = XSECT(3,9)
     &             /(VDMQ2F(1)+VDMQ2F(2))/(VDMQ2F(3)+VDMQ2F(4))
*         DSIGHP = (XSECT(4,15)+XSECT(4,9))
          DSIGHP = XSECT(4,9)
     &             /(VDMQ2F(1)+VDMQ2F(2))/(VDMQ2F(3)+VDMQ2F(4))
        ELSE IF(IP.EQ.2) THEN
          SIGS = GP(1)*GPPR*ECM**(TWO*(ALREG-ONE))
     &          +GP(1)*GPPP*ECM**(TWO*(ALPOM-ONE))
*         SIGH   = (XSECT(3,15)+XSECT(3,9))/(VDMQ2F(1)+VDMQ2F(2))
          SIGH   = XSECT(3,9)/(VDMQ2F(1)+VDMQ2F(2))
*         DSIGHP = (XSECT(4,15)+XSECT(4,9))/(VDMQ2F(1)+VDMQ2F(2))
          DSIGHP = XSECT(4,9)/(VDMQ2F(1)+VDMQ2F(2))
        ELSE IF(IP.EQ.3) THEN
          SIGS = GP(2)*GPPR*ECM**(TWO*(ALREG-ONE))
     &          +GP(2)*GPPP*ECM**(TWO*(ALPOM-ONE))
*         SIGH   = (XSECT(3,15)+XSECT(3,9))/(VDMQ2F(3)+VDMQ2F(4))
          SIGH   = XSECT(3,9)/(VDMQ2F(3)+VDMQ2F(4))
*         DSIGHP = (XSECT(4,15)+XSECT(4,9))/(VDMQ2F(3)+VDMQ2F(4))
          DSIGHP = XSECT(4,9)/(VDMQ2F(3)+VDMQ2F(4))
        ELSE
          SIGS = GPPR**2*ECM**(TWO*(ALREG-ONE))
     &          +GPPP**2*ECM**(TWO*(ALPOM-ONE))
*         SIGH   = XSECT(3,15)+XSECT(3,9)
          SIGH   = XSECT(3,9)
*         DSIGHP = XSECT(4,15)+XSECT(4,9)
          DSIGHP = XSECT(4,9)
        ENDIF
        FS = FPS(IP)
        FH = FPH(IP)
        SIGS   = MAX(SIGS,ZERO)
        SIGH   = MAX(SIGH,ZERO)
        DSIGHP = MAX(DSIGHP,ZERO)
        CALL SOFTPT(-1,PT1,PT1,XP,IV,PTS)
      ENDIF
C
 300  CONTINUE
C  debug output
      IF(IDEB(58).GE.15) THEN
        WRITE(6,'(1X,A,I10,3I2,2E10.3)')
     &    'POHINT:DEBUG:WEIGHTS EV,IP,K1,K2,ECM,PTC',
     &    KEVENT,IP,K1,K2,ECM,PTCUT(IP)
        DO 162 M=I1,I2
          WRITE(6,'(5X,2I3,4E12.3)')
     &      M,MXSECT(0,M,IP),(XSECT(K,M),K=K1,K2)
 162    CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE POHMCI(EMAXF)
C**********************************************************************
C
C     initialize MC sampling and calculate hard cross section
C
C     input:  EMAXF(4) maximal CMS energies to appear in
C                      interpolation table in reference to PTCUT(1..4)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION EMAXF(4)
C
      PARAMETER (OHALF  = 0.5D0,
     &           ZERO   = 0.D0,
     &           DEPS   = 1.D-10,
     &           PLARGE = 1.D20 )
C
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  global CM system
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
C
      PARAMETER ( MAXPRO = 16 )
      CHARACTER*18 PROC
      COMMON /PEPROC/ PROC(0:MAXPRO)
      PARAMETER ( MLINE = 400 , MSCAHD = 50 )
      COMMON /HAEVTR/ LINE,LIN,LREC1(MLINE),LREC2(MLINE),PREC(4,MLINE)
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
C
      COMPLEX*16 DSIG
      DIMENSION DSIG(0:MAXPRO),DSPT(0:MAXPRO)
C
C  initialize COMMON /HAEVTR/
      LINE       = 0
      LIN        = 0
      DO 20 L = 1,MLINE
        LREC1(L) = 0
        LREC2(L) = 0
        DO 10 I=1,4
          PREC(I,L) = ZERO
 10     CONTINUE
 20   CONTINUE
C  initializaion for all pt cutoffs
      I0 = 4
      IF(ISWMDL(2).EQ.0) I0 = 1
      DO 110 I=I0,1,-1
C  skip unassigned PTCUT
        IF(PTCUT(I).LT.0.5D0) GOTO 90
C
        ELLOW = LOG(2.2*PTCUT(I))
        DELTA = (LOG(EMAXF(I))-ELLOW)/DBLE(ISTTAB-1)
        IF(DELTA.LE.ZERO) THEN
          DO 60 IE=1,ISTTAB
            ECMSH(I,IE) = EXP(ELLOW+DELTA*(IE-1))
            XSECTA(1,0,I,IE)  = PTCUT(I)
            XSECTA(1,-1,I,IE) = ZERO
            XSECTA(2,-1,I,IE) = ZERO
            DO 65 M=0,MAXPRO
              XSECTA(1,M,I,IE) = ZERO
              XSECTA(2,M,I,IE) = ZERO
              XSECTA(3,M,I,IE) = ZERO
              XSECTA(4,M,I,IE) = ZERO
  65        CONTINUE
 60       CONTINUE
          GOTO 90
        ENDIF
C  switch between external particles and Pomeron
        IF(I.EQ.1) THEN
          IDP1 = IFPAP(1)
          PV1  = PVIRT(1)
          IDP2 = IFPAP(2)
          PV2  = PVIRT(2)
        ELSE IF(I.EQ.2) THEN
          IDP1 = IFPAP(1)
          PV1  = PVIRT(1)
          IDP2 = 45
          PV2  = ZERO
        ELSE IF(I.EQ.3) THEN
          IDP1 = IFPAP(2)
          PV1  = PVIRT(2)
          IDP2 = 45
          PV2  = ZERO
        ELSE IF(I.EQ.4) THEN
          IDP1 = 45
          PV1  = ZERO
          IDP2 = 45
          PV2  = ZERO
        ENDIF
C  initialize PT scales
        IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)) THEN
          IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
            FPS(I) = PARMDL(105)
            FPH(I) = PARMDL(106)
          ELSE
            FPS(I) = PARMDL(103)
            FPH(I) = PARMDL(104)
          ENDIF
        ELSE IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
          FPS(I) = PARMDL(103)
          FPH(I) = PARMDL(104)
        ELSE
          FPS(I) = PARMDL(101)
          FPH(I) = PARMDL(102)
        ENDIF
C  initialize hard scattering
        CALL POHINI(I,IDP1,IDP2,PV1,PV2,6,IDEB(8))
C  initialization for several energies
        DO 100 IE=1,ISTTAB
          ECMSH(I,IE) = EXP(ELLOW+DELTA*(IE-1))
          CALL POHINT(I,ECMSH(I,IE),0,-2,0,0,1)
          IF(IDEB(8).GE.5) THEN
            WRITE(6,'(1X,A,2E10.3,2I7)')
     &        'POHMCI:DEBUG:CALC. OF (PT,ECM,ID1,ID2)',
     &        PTCUT(I),ECMSH(I,IE),IDPDG1,IDPDG2
          ENDIF
          CALL POHFAC(PTCUT(I),ECMSH(I,IE))
          CALL POHWGX(PTCUT(I),ECMSH(I,IE))
          CALL POHXTO(ECMSH(I,IE),PTCUT(I),PTCUT(I),DSIG,DSPT)
          IF(IDEB(8).GE.10) THEN
            WRITE(6,'(1X,A,/,1X,A)')
     &        'HARD CROSS SECTIONS SIGH(mb),DSIG/DPT(mb/GeV**2)',
     &        '================================================'
            DO 205 M=0,MAXPRO
              WRITE(6,'(10X,A,3E14.4)') PROC(M),DSIG(M),DSPT(M)
 205        CONTINUE
          ENDIF
          XSECTA(1,0,I,IE)  = PTCUT(I)
          XSECTA(1,-1,I,IE) = XSECT(1,-1)
          XSECTA(2,-1,I,IE) = XSECT(2,-1)
          DO 105 M=0,MAXPRO
            XSECTA(1,M,I,IE) = XSECT(1,M)
            XSECTA(2,M,I,IE) = XSECT(2,M)
            XSECTA(3,M,I,IE) = DREAL(DSIG(M))*MXSECT(0,M,I)
            XSECTA(4,M,I,IE) = DSPT(M)*MXSECT(0,M,I)
 105      CONTINUE
C  summed quantities
          XSECTA(3,9,I,IE) = ZERO
          XSECTA(4,9,I,IE) = ZERO
          DO 106 M=1,8
            IF(MXSECT(0,M,I).GT.0) THEN
              XSECTA(3,9,I,IE) = XSECTA(3,9,I,IE)+XSECTA(3,M,I,IE)
              XSECTA(4,9,I,IE) = XSECTA(4,9,I,IE)+XSECTA(4,M,I,IE)
            ENDIF
 106      CONTINUE
          XSECTA(3,15,I,IE) = ZERO
          XSECTA(4,15,I,IE) = ZERO
          DO 107 M=10,14
            IF(MXSECT(0,M,I).GT.0) THEN
              XSECTA(3,15,I,IE) = XSECTA(3,15,I,IE)+XSECTA(3,M,I,IE)
              XSECTA(4,15,I,IE) = XSECTA(4,15,I,IE)+XSECTA(4,M,I,IE)
            ENDIF
 107      CONTINUE
          XSECTA(3,0,I,IE) = XSECTA(3,9,I,IE)+XSECTA(3,15,I,IE)
          XSECTA(4,0,I,IE) = XSECTA(4,9,I,IE)+XSECTA(4,15,I,IE)
 100    CONTINUE
C  debug output of weights
        IF(IDEB(8).GE.5) THEN
          WRITE(6,'(/1X,A,5X,2I7,I3,F5.2,/1X,A)')
     &      'POHMCI:DEBUG:WEIGHTS,MAXIMA (ID1,ID2,IP,PTCUT)',
     &      IDPDG1,IDPDG2,I,PTCUT(I),
     &      '=============================================='
          DO 515 M=-1,MAXPRO
            IF((M.EQ.0).OR.(M.EQ.9).OR.(M.EQ.15)) GOTO 512
            WRITE(6,'(2X,A,I3,2I7)')
     &        'POHMCI:DEBUG:ECM XSECT(1-4) FOR PROCESS,ID1,ID2',
     &        M,IDPDG1,IDPDG2
            DO 510 K=1,ISTTAB
              WRITE(6,'(3X,5E12.4)') ECMSH(I,K),XSECTA(1,M,I,K),
     &        XSECTA(2,M,I,K),XSECTA(3,M,I,K),XSECTA(4,M,I,K)
 510        CONTINUE
 512        CONTINUE
 515      CONTINUE
        ENDIF
 90     CONTINUE
 110  CONTINUE
      END
C
C
      SUBROUTINE POHXR3(ECMH,PT,ETAC,ETAD,DSIGMC)
C**********************************************************************
C
C     differential cross section DSIG/(DETAC*DETAD*DPT)
C
C     input:  ECMH     CMS energy
C             PT       parton PT
C             ETAC     pseudorapidity of parton C
C             ETAD     pseudorapidity of parton D
C
C     output: DSIGMC(0:15) QCD-PM cross sections dsigma/dpt/detac/detad
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16 DSIGMC
      DIMENSION DSIGMC(0:MAXPRO)
      DIMENSION DSIGM(0:MAXPRO)
C
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      PARAMETER ( TINY= 1.D-30, ONEP1=1.1, TINY6=1.D-06, EPS=1.D-20)
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
      DIMENSION PDA(-6:6),PDB(-6:6)
      PARAMETER (ZERO = 0.D0,
     &           IONE = 1,
     &           ITWO = 2 )
C
      DO 10 I=1,9
        DSIGMC(I) = CMPLX(0.D0,0.D0)
        DSIGM(I)  = 0.D0
10    CONTINUE
C
      EC     = EXP(ETAC)
      ED     = EXP(ETAD)
C  kinematic conversions
      XA     = PT*(EC+ED)/ECMH
      XB     = XA/(EC*ED)
      IF((XA.GE.1.D0).OR.(XB.GE.1.D0)) THEN
        WRITE(6,'(/1X,A,2E12.4)') 'POHXR3:ERROR:X1 OR X2 > 1',XA,XB
        RETURN
      ENDIF
      SP     = XA*XB*ECMH*ECMH
      UP     =-ECMH*PT*EC*XB
      UP     = UP/SP
      TP     =-(1.D0+UP)
      UU     = UP*UP
      TT     = TP*TP
C  set hard scale  QQ  for alpha and partondistr.
      IF     ( NQQAL.EQ.1 ) THEN
        QQAL = AQQAL*PT*PT
      ELSEIF ( NQQAL.EQ.2 ) THEN
        QQAL = AQQAL*SP*UP*TP/(1.D0+TT+UU)
      ELSEIF ( NQQAL.EQ.3 ) THEN
        QQAL = AQQAL*SP
      ELSEIF ( NQQAL.EQ.4 ) THEN
        QQAL = AQQAL*SP*(UP*TP)**(1.D0/3.D0)
      ENDIF
      IF     ( NQQPD.EQ.1 ) THEN
        QQPD = AQQPD*PT*PT
      ELSEIF ( NQQPD.EQ.2 ) THEN
        QQPD = AQQPD*SP*UP*TP/(1.D0+TT+UU)
      ELSEIF ( NQQPD.EQ.3 ) THEN
        QQPD = AQQPD*SP
      ELSEIF ( NQQPD.EQ.4 ) THEN
        QQPD = AQQPD*SP*(UP*TP)**(1.D0/3.D0)
      ENDIF
C
      ALPHA  = BQCD/LOG(MAX(QQAL/(PDFLAM(1)*PDFLAM(2)),ONEP1))
      FACTOR = PI2*GEV2MB*PT*(ALPHA/SP)**2
C  parton distributions (times x)
      CALL PDIS(XA,QQPD,1,PDA)
      CALL PDIS(XB,QQPD,2,PDB)
      S1    = PDA(0)*PDB(0)
      S2    = 0.D0
      S3    = 0.D0
      S4    = 0.D0
      S5    = 0.D0
      DO 20 I=1,NF
        S2  = S2+PDA(I)*PDB(-I)+PDA(-I)*PDB( I)
        S3  = S3+PDA(I)*PDB( I)+PDA(-I)*PDB(-I)
        S4  = S4+PDA(I)+PDA(-I)
        S5  = S5+PDB(I)+PDB(-I)
20    CONTINUE
C  partial cross sections (including color and symmetry factors)
C  resolved photon matrix elements (light quarks)
      DSIGM(1) = 2.25D0*(3.-((UP*TP)+UP/TT+TP/UU))
      DSIGM(6) = (4.D0/9.D0)*(UU+TT)
      DSIGM(8) = (4.D0/9.D0)*(1.D0+UU)/TT
      DSIGM(2) = (16.D0/27.D0)*(UU+TT)/(UP*TP)-3.D0*DSIGM(6)
      DSIGM(3) = ((1.D0+UU)/TT)-(4.D0/9.D0)*(1.D0+UU)/UP
      DSIGM(4) = (9.D0/32.D0)*DSIGM(2)
      DSIGM(5) = DSIGM(6)+DSIGM(8)-(8.D0/27.D0)*UU/TP
      DSIGM(7) = 0.5D0*(DSIGM(8)+(4.D0/9.D0)*(1.D0+TT)/UU-
     &           (8.D0/27.D0)/(UP*TP))
C
      DSIGM(1) = FACTOR*DSIGM(1)*S1
      DSIGM(2) = FACTOR*DSIGM(2)*S2
      DSIGM(3) = FACTOR*DSIGM(3)*(PDA(0)*S5+PDB(0)*S4)
      DSIGM(4) = FACTOR*DSIGM(4)*S1*NF
      DSIGM(5) = FACTOR*DSIGM(5)*S2
      DSIGM(6) = FACTOR*DSIGM(6)*S2*MAX(0,(NF-1))
      DSIGM(7) = FACTOR*DSIGM(7)*S3
      DSIGM(8) = FACTOR*DSIGM(8)*(S4*S5-(S2+S3))
C  complex part
      X=ABS(TP-UP)
      FAC2 = -LOG((X+2.D0)/(X+1.D-30))/PI
C
      DO 50 I=1,8
        IF(DSIGM(I).LT.EPS) DSIGM(I) = ZERO
        DSIGMC(I) = CMPLX(DSIGM(I),DSIGM(I)*FAC2)
        DSIGMC(9) = DSIGMC(9)+DSIGMC(I)
 50   CONTINUE
      END
C
C
      SUBROUTINE POHXR2(ECMH,PT,ETAC,DSIGMC)
C**********************************************************************
C
C     differential cross section DSIG/(DETAC*DPT)
C
C     input:  ECMH     CMS energy
C             PT       parton PT
C             ETAC     pseudorapidity of parton C
C
C     output: DSIGMC(0:15) QCD-PM cross sections dsigma/dpt/detac
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16 DSIGMC
      DIMENSION DSIGMC(0:MAXPRO)
      PARAMETER ( TINY= 1.D-20 )
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      COMPLEX*16 DSIG1
      DIMENSION DSIG1(0:MAXPRO)
      DIMENSION ABSZ(32),WEIG(32)
C
      DO 10 M=1,9
        DSIGMC(M) = CMPLX(0.D0,0.D0)
        DSIG1(M)  = 0.D0
10    CONTINUE
C
      EC  = EXP(ETAC)
      ARG = ECMH/PT
      IF  ( ARG.LE.EC .OR. ARG.LE.1.D0/EC ) RETURN
      EDU = LOG(ARG-EC)
      EDL =-LOG(ARG-1.D0/EC)
      NPOINT = NGAUET
      CALL PHGSET(EDL,EDU,NPOINT,ABSZ,WEIG)
      DO 30 I=1,NPOINT
        CALL POHXR3(ECMH,PT,ETAC,ABSZ(I),DSIG1)
        DO 20 M=1,9
          PCTRL= DREAL(DSIG1(M))/TINY
          IF( PCTRL.GE.1.D0 ) THEN
            DSIGMC(M) = DSIGMC(M)+WEIG(I)*DSIG1(M)
          ENDIF
20      CONTINUE
30    CONTINUE
      END
C
C
      SUBROUTINE POHXD2(ECMH,PT,ETAC,DSIGMC)
C**********************************************************************
C
C     differential cross section DSIG/(DETAC*DPT) for direct processes
C
C     input:  ECMH     CMS energy of scattering system
C             PT       parton PT
C             ETAC     pseudorapidity of parton C
C
C     output: DSIGMC(0:15) QCD-PM cross sections dsigma/dpt/detac
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16 DSIGMC
      DIMENSION DSIGMC(0:MAXPRO)
      PARAMETER ( TINY= 1.D-30, ONEP1=1.1, TINY6=1.D-06, EPS=1.D-25)
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      DIMENSION PDA(-6:6),PDB(-6:6),DSIGM(0:MAXPRO)
      PARAMETER ( ZERO = 0.D0,
     &             ONE = 1.D0 )
**sr 29.04. commented to use dp-JETSET
      REAL QQALR,ULALEM
**
C
      ONE32=1.D0/9.D0
      TWO32=4.D0/9.D0
      DO 10 I=10,13
        DSIGMC(I) = CMPLX(0.D0,0.D0)
        DSIGM(I) = 0.D0
 10   CONTINUE
      DSIGMC(15) = CMPLX(0.D0,0.D0)
      DSIGM(15) = 0.D0
C
C  direct particle 1
      IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)) THEN
        EC     = EXP(ETAC)
        ED     = ECMH/PT-EC
C  kinematic conversions
        XA     = 1.D0
        XB     = 1.D0/(EC*ED)
        IF ( XB.GE.1.D0 ) THEN
          WRITE(6,'(/1X,A,2E12.4)') 'POHXD2:ERROR:XB>1 (XA,XB)',XA,XB
          RETURN
        ENDIF
        SP     = XA*XB*ECMH*ECMH
        UP     =-ECMH*PT*EC*XB
        UP     = UP/SP
        TP     =-(1.D0+UP)
        UU     = UP*UP
        TT     = TP*TP
C  set hard scale  QQ  for alpha and partondistr.
        IF     ( NQQAL.EQ.1 ) THEN
          QQAL = AQQAL*PT*PT
        ELSEIF ( NQQAL.EQ.2 ) THEN
          QQAL = AQQAL*SP*UP*TP/(1.D0+TT+UU)
        ELSEIF ( NQQAL.EQ.3 ) THEN
          QQAL = AQQAL*SP
        ELSEIF ( NQQAL.EQ.4 ) THEN
          QQAL = AQQAL*SP*(UP*TP)**(1.D0/3.D0)
        ENDIF
        IF     ( NQQPD.EQ.1 ) THEN
          QQPD = AQQPD*PT*PT
        ELSEIF ( NQQPD.EQ.2 ) THEN
          QQPD = AQQPD*SP*UP*TP/(1.D0+TT+UU)
        ELSEIF ( NQQPD.EQ.3 ) THEN
          QQPD = AQQPD*SP
        ELSEIF ( NQQPD.EQ.4 ) THEN
          QQPD = AQQPD*SP*(UP*TP)**(1.D0/3.D0)
        ENDIF
C
        ALPHA2 = BQCD/LOG(MAX(QQAL/PDFLAM(2)**2,ONEP1))
        IF(IDPDG1.EQ.22) THEN
          QQALR = QQAL
          ALPHA1 = DBLE(ULALEM(QQALR))
        ELSE IF(IDPDG1.EQ.45) THEN
          ALPHA1 = PARMDL(74)
        ENDIF
        FACTOR = -PI2*GEV2MB*UP/PT*ALPHA1*ALPHA2/SP
C  parton distribution (times x)
        CALL PDIS(XB,QQPD,2,PDB)
        S1    = PDB(0)
C  charge counting
        S2    = 0.D0
        S3    = 0.D0
        IF(IDPDG1.EQ.22) THEN
          DO 20 I=1,NF
            IF(MOD(I,2).EQ.0) THEN
              S2 = S2 + (PDB(I)+PDB(-I))*TWO32
              S3 = S3 + TWO32
            ELSE
              S2 = S2 + (PDB(I)+PDB(-I))*ONE32
              S3 = S3 + ONE32
            ENDIF
 20       CONTINUE
        ELSE IF(IDPDG1.EQ.45) THEN
          DO 25 I=1,NF
            S2 = S2 + PDB(I)+PDB(-I)
 25       CONTINUE
          S3 = NF
        ENDIF
C  partial cross sections (including color and symmetry factors)
C  direct photon matrix elements
        DSIGM(10) = -8.D0/3.D0*(UU+1.D0)/UP
        DSIGM(11) = (UU+TT)/(UP*TP)
C
        DSIGM(10) = FACTOR*DSIGM(10)*S2
        DSIGM(11) = FACTOR*DSIGM(11)*S1*S3
C  complex part
        X=ABS(TP-UP)
        FAC2 = -LOG((X+2.D0)/(X+1.D-30))/PI
C
        DO 50 I=10,11
          IF(DSIGM(I).LT.ZERO) THEN
            print *,'POHXD2: negative ',I,DSIGM(I),ECMH
            DSIGM(I) = ZERO
          ENDIF
          DSIGMC(I) = CMPLX(DSIGM(I),DSIGM(I)*FAC2)
          DSIGMC(15) = DSIGMC(15)+DSIGMC(I)
 50     CONTINUE
      ENDIF
C
C  direct particle 2
      IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
        EC     = EXP(ETAC)
        ED     = 1.D0/(ECMH/PT-1.D0/EC)
C  kinematic conversions
        XA     = PT*(EC+ED)/ECMH
        XB     = 1.D0
        IF ( XA.GE.1.D0 ) THEN
          WRITE(6,'(/1X,A,2E12.4)') 'POHXD2:ERROR:XA>1 (XA,XB)',XA,XB
          RETURN
        ENDIF
        SP     = XA*XB*ECMH*ECMH
        UP     =-ECMH*PT*EC*XB
        UP     = UP/SP
        TP     =-(1.D0+UP)
        UU     = UP*UP
        TT     = TP*TP
C  set hard scale  QQ  for alpha and partondistr.
        IF     ( NQQAL.EQ.1 ) THEN
          QQAL = AQQAL*PT*PT
        ELSEIF ( NQQAL.EQ.2 ) THEN
          QQAL = AQQAL*SP*UP*TP/(1.D0+TT+UU)
        ELSEIF ( NQQAL.EQ.3 ) THEN
          QQAL = AQQAL*SP
        ELSEIF ( NQQAL.EQ.4 ) THEN
          QQAL = AQQAL*SP*(UP*TP)**(1.D0/3.D0)
        ENDIF
        IF     ( NQQPD.EQ.1 ) THEN
          QQPD = AQQPD*PT*PT
        ELSEIF ( NQQPD.EQ.2 ) THEN
          QQPD = AQQPD*SP*UP*TP/(1.D0+TT+UU)
        ELSEIF ( NQQPD.EQ.3 ) THEN
          QQPD = AQQPD*SP
        ELSEIF ( NQQPD.EQ.4 ) THEN
          QQPD = AQQPD*SP*(UP*TP)**(1.D0/3.D0)
        ENDIF
C
        ALPHA1 = BQCD/LOG(MAX(QQAL/PDFLAM(1)**2,ONEP1))
        IF(IDPDG2.EQ.22) THEN
          QQALR  = QQAL
          ALPHA2 = DBLE(ULALEM(QQALR))
        ELSE IF(IDPDG2.EQ.45) THEN
          ALPHA2 = PARMDL(74)
        ENDIF
        FACTOR = -PI2*GEV2MB*TP/PT*ALPHA1*ALPHA2/SP
C  parton distribution (times x)
        CALL PDIS(XA,QQPD,1,PDA)
        S1    = PDA(0)
C  charge counting
        S2    = 0.D0
        S3    = 0.D0
        IF(IDPDG2.EQ.22) THEN
          DO 70 I=1,NF
            IF(MOD(I,2).EQ.0) THEN
              S2 = S2 + (PDA(I)+PDA(-I))*TWO32
              S3 = S3 + TWO32
            ELSE
              S2 = S2 + (PDA(I)+PDA(-I))*ONE32
              S3 = S3 + ONE32
            ENDIF
 70       CONTINUE
        ELSE IF(IDPDG2.EQ.45) THEN
          DO 75 I=1,NF
            S2 = S2 + PDA(I)+PDA(-I)
 75       CONTINUE
          S3 = NF
        ENDIF
C  partial cross sections (including color and symmetry factors)
C  direct photon matrix elements
        DSIGM(12) = -8.D0/3.D0*(UU+1.D0)/UP
        DSIGM(13) = (UU+TT)/(UP*TP)
C
        DSIGM(12) = FACTOR*DSIGM(12)*S2
        DSIGM(13) = FACTOR*DSIGM(13)*S3*S1
C  complex part
        X=ABS(TP-UP)
        FAC2 = -LOG((X+2.D0)/(X+1.D-30))/PI
C
        DO 80 I=12,13
          IF(DSIGM(I).LT.ZERO) THEN
            print *,'POHXD2: negative ',I,DSIGM(I),ECMH
            DSIGM(I) = ZERO
          ENDIF
          DSIGMC(I) = CMPLX(DSIGM(I),DSIGM(I)*FAC2)
          DSIGMC(15) = DSIGMC(15)+DSIGMC(I)
 80     CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE POHXPT(ECMH,PT,IPRO,DSIGMC)
C**********************************************************************
C
C     differential cross section DSIG/DPT
C
C     input:  ECMH     CMS energy of scattering system
C             PT       parton PT
C             IPRO     1  resolved processes
C                      2  direct processes
C                      3  resolved and direct processes
C
C     output: DSIGMC(0:12) QCD-PM cross sections dsigma/dpt
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16 DSIGMC
      DIMENSION  DSIGMC(0:MAXPRO)
      PARAMETER ( TINY= 1.D-10, ONEP1=1.1, EPS=1.D-25)
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      COMPLEX*16 DSIG1
      DIMENSION  DSIG1(0:MAXPRO)
      DIMENSION ABSZ(32),WEIG(32)
**sr 29.04. commented to use dp-JETSET
      REAL ULALEM,SSR
**
C
      DO 10 M=0,MAXPRO
        DSIGMC(M) = CMPLX(0.D0,0.D0)
        DSIG1(M)  = CMPLX(0.D0,0.D0)
 10   CONTINUE
C
C  resolved and direct processes
      AMT = 2.D0*PT/ECMH
      IF ( AMT.GE.1.D0 ) RETURN
      ECU = LOG((SQRT(1.D0-AMT*AMT)+1.D0)/AMT)
      ECL =-ECU
      NPOINT = NGAUET
      CALL PHGSET(ECL,ECU,NPOINT,ABSZ,WEIG)
      DO 30 I=1,NPOINT
        DSIG1(9)  = CMPLX(0.D0,0.D0)
        DSIG1(15) = CMPLX(0.D0,0.D0)
        IF(IPRO.EQ.1) THEN
          CALL POHXR2(ECMH,PT,ABSZ(I),DSIG1)
        ELSE IF(IPRO.EQ.2) THEN
          CALL POHXD2(ECMH,PT,ABSZ(I),DSIG1)
        ELSE
          CALL POHXR2(ECMH,PT,ABSZ(I),DSIG1)
          CALL POHXD2(ECMH,PT,ABSZ(I),DSIG1)
        ENDIF
        DO 20 M=1,MAXPRO
          DSIGMC(M) = DSIGMC(M)+WEIG(I)*DSIG1(M)
 20     CONTINUE
 30   CONTINUE
C
C  double direct processes
      IF(((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45))
     &   .AND.((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45))) THEN
        FAC = 0.D0
        SS = ECMH*ECMH
        SSR = REAL(SS)
        ALPHAE = DBLE(ULALEM(SSR))
*       ALPHAE = 1.D0/137.D0
        DO 300 I=1,NF
          IF(IDPDG1.EQ.22) THEN
            F1 = (4.D0-3.D0*MOD(I,2))/9.D0*ALPHAE
          ELSE
            F1 = PARMDL(74)
          ENDIF
          IF(IDPDG2.EQ.22) THEN
            F2 = (4.D0-3.D0*MOD(I,2))/9.D0*ALPHAE
          ELSE
            F2 = PARMDL(74)
          ENDIF
          FAC = FAC+F1*F2*3.D0
 300    CONTINUE
C  direct cross sections
        ZZ = SQRT(1.D0-4.D0*PT*PT/SS+TINY)
        T1 = -SS/2.D0*(1.D0+ZZ)
        T2 = -SS/2.D0*(1.D0-ZZ)
        XM = -2.D0*PT/ZZ*((SS+T1)/T1+T1/(SS+T1)+(SS+T2)/T2+T2/(SS+T2))
C  hadronic part
        DSIGMC(14) = GEV2MB*2.D0*PI*FAC/(SS*SS)*XM
C  leptonic part (e, mu, tau)
        DSIGMC(16) = 0.D0
        IF((IDPDG1.EQ.22).AND.(IDPDG2.EQ.22)) THEN
          DSIGMC(16) = DSIGMC(14)/FAC*3.D0*ALPHAE**2
C  simulation of tau together with quarks
          IF(IPAMDL(64).NE.0) DSIGMC(14) = DSIGMC(14)+DSIGMC(16)/3.D0
        ENDIF
      ENDIF
C
      DSIGMC(15) = DSIGMC(15)+DSIGMC(14)
      DSIGMC(0)  = DSIGMC(9)+DSIGMC(15)
      END
C
C
      SUBROUTINE POHXTO(ECMH,PTCUTR,PTCUTD,DSIGMC,DSDPTC)
C**********************************************************************
C
C     total hard cross section (perturbative QCD, Parton Model)
C
C     input:  ECMH     CMS energy of scattering system
C             PTCUTR   PT cutoff for resolved processes
C             PTCUTD   PT cutoff for direct processes (photon, Pomeron)
C
C     output: DSIGMC(0:12) QCD PM cross sections for given cutoff
C             DSDPTC(0:12) QCD PM differential cross sections at cutoff
C
C     note:  COMPLEX*16          DSIGMC
C            DOUBLE PRECISION    DSDPTC
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER ( MAXPRO = 16 )
      COMPLEX*16 DSIGMC
      DIMENSION DSIGMC(0:MAXPRO),DSDPTC(0:MAXPRO)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
      COMPLEX*16 DSIG1
      DIMENSION DSIG1(0:MAXPRO)
      DIMENSION ABSZ(32),WEIG(32)
**sr 29.04. commented to use dp-JETSET
      REAL ULALEM,SSR
**
      DATA FAC / 3.0D0 /
C
      DO 10 M=0,MAXPRO
        DSIGMC(M)= CMPLX(0.D0,0.D0)
 10   CONTINUE
      EEC=ECMH/2.D0
C
      IF ( PTCUTR.GE.EEC ) GOTO 100
C
C  integration for resolved processes
      PTMIN  = PTCUTR
      PTMAX  = MIN(FAC*PTMIN,EEC)
      NPOINT = NGAUP1
      CALL POHXPT(ECMH,PTMIN,1,DSIG1)
      DO 60 M=1,9
        DSDPTC(M) = DREAL(DSIG1(M))
 60   CONTINUE
      DSIGH   = DREAL(DSIG1(9))
      DSIGHP  = DSIGH
      PTMXX  = 0.95D0*PTMAX
      CALL POHXPT(ECMH,PTMXX,1,DSIG1)
      DSIGL  = DREAL(DSIG1(9))
      EX     = LOG(DSIGH/(DSIGL+1.D-30))/LOG(FAC)
      EX1    = 1.0D0-EX
      DO 50 K=1,2
        IF ( PTMIN.GE.PTMAX ) GOTO 40
        RL   = PTMIN**EX1
        RU   = PTMAX**EX1
        CALL PHGSET(RL,RU,NPOINT,ABSZ,WEIG)
        DO 30 I=1,NPOINT
          R  = ABSZ(I)
          PT = R**(1.0D0/EX1)
          CALL POHXPT(ECMH,PT,1,DSIG1)
          F  = WEIG(I)*PT/(R*EX1)
          DO 20 M=1,9
            DSIGMC(M) = DSIGMC(M)+F*DSIG1(M)
 20       CONTINUE
 30     CONTINUE
 40     PTMIN  = PTMAX
        PTMAX  = EEC
        NPOINT = NGAUP2
 50   CONTINUE
      SIGH = DREAL(DSIGMC(9))
 100  CONTINUE
C
C  integration for direct processes
      IF(PTCUTD.GE.ECMH/2.D0) RETURN
C
      IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)
     &   .OR.(IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
        PTMIN  = PTCUTD
        PTMAX  = MIN(FAC*PTMIN,EEC)
        NPOINT = NGAUP1
        CALL POHXPT(ECMH,PTMIN,2,DSIG1)
        DO 160 M=10,16
          DSDPTC(M) = DREAL(DSIG1(M))
 160    CONTINUE
        DSIGH   = DREAL(DSIG1(15)-DSIG1(14))
        PTMXX  = 0.95D0*PTMAX
        CALL POHXPT(ECMH,PTMXX,2,DSIG1)
        DSIGL  = DREAL(DSIG1(15)-DSIG1(14))
        IF(DSIGH.EQ.DSIGL) RETURN
        EX     = LOG(DSIGH/(DSIGL+1.D-30))/LOG(FAC)
        EX1    = 1.0D0-EX
        DO 150 K=1,2
          IF ( PTMIN.GE.PTMAX ) GOTO 140
          RL   = PTMIN**EX1
          RU   = PTMAX**EX1
          CALL PHGSET(RL,RU,NPOINT,ABSZ,WEIG)
          DO 130 I=1,NPOINT
            R  = ABSZ(I)
            PT = R**(1.0D0/EX1)
            CALL POHXPT(ECMH,PT,2,DSIG1)
            F  = WEIG(I)*PT/(R*EX1)
            DO 120 M=10,15
              DSIGMC(M) = DSIGMC(M)+F*DSIG1(M)
 120        CONTINUE
 130      CONTINUE
 140      PTMIN  = PTMAX
          PTMAX  = EEC
          NPOINT = NGAUP2
 150    CONTINUE
      ENDIF
C
C  double direct process
      IF(((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45))
     &   .AND.((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45))) THEN
        FACC = 0.D0
        SS = ECMH*ECMH
        SSR = REAL(SS)
        ALPHAE = DBLE(ULALEM(SSR))
*       ALPHAE = 1.D0/137.D0
        DO 300 I=1,NF
          IF(IDPDG1.EQ.22) THEN
            F1 = (4.D0-3.D0*MOD(I,2))/9.D0*ALPHAE
          ELSE
            F1 = PARMDL(74)
          ENDIF
          IF(IDPDG2.EQ.22) THEN
            F2 = (4.D0-3.D0*MOD(I,2))/9.D0*ALPHAE
          ELSE
            F2 = PARMDL(74)
          ENDIF
          FACC = FACC + F1*F2*3.D0
 300    CONTINUE
C
        ZZ = SQRT(1.D0-4.D0*PTCUTD*PTCUTD/SS)
        R  = 4.D0*PI/SS*(LOG((1.D0+ZZ)/(1.D0-ZZ))-ZZ)*GEV2MB
C  hadronic cross section
        DSIGMC(14) = R*FACC
C************************** 1.04 bug? ****************
*       SS = ECMH*ECMH
*       ZZ = 4.D0*PTCUTD*PTCUTD/SS
*       ZZ = 6.D0*GEV2MB*FACC*PI/SS/3.D0/(ALPHAE*137.D0)**2
*    &     *(4.D0*LOG((1.D0+SQRT(1.D0-ZZ))/SQRT(ZZ))-2.D0*SQRT(2.D0-ZZ))
*       ZZ = MAX(ZZ,0.D0)
*       write(6,'(1x,a,3e12.4)')
*    &    'ecm,old/new',ECMH,ZZ,DREAL(DSIGMC(14))
C******************************************************
C  leptonic cross section
        IF((IDPDG1.EQ.22).AND.(IDPDG2.EQ.22)) THEN
          DSIGMC(16) = R*3.D0*ALPHAE**2
C  simulation of tau together with quarks
          IF(IPAMDL(64).NE.0) DSIGMC(14) = DSIGMC(14)+DSIGMC(16)/3.D0
          DSIGMC(16) = DSIGMC(16)*2.D0/3.D0
        ELSE
          DSIGMC(16) = CMPLX(0.D0,0.D0)
        ENDIF
C  sum of direct part
        DSIGMC(15) = CMPLX(0.D0,0.D0)
        DO 400 I=10,14
          DSIGMC(15) = DSIGMC(15) + DSIGMC(I)
 400    CONTINUE
      ENDIF
C
      DSIGMC(0) = DSIGMC(9) + DSIGMC(15)
      DSDPTC(0) = DSDPTC(9) + DSDPTC(15)
      END
C
C
      SUBROUTINE POHISR(IHARD,P1,P2,IPF1,IPF2,IPA1,IPA2,Q2H,XH1,XH2,
     &                  XHMAX1,XHMAX2,IPB1,IPB2,XISR1,XISR2,IREJ)
C********************************************************************
C
C     initial state radiation according to DGLAP evolution equations
C     (backward evolution, no spin effects)
C
C     input:    IHARD     index of hard Pomeron
C               P1,P2     4 momenta of hard scattered final partons
C                         (in CMS of hard scattering)
C               IPF1,2    flavours of final partons
C               IPA1,2    flavours of initial partons
C               Q2H       momentum transfer (squared, positive)
C               XH1,XH2   x values of initial partons
C               XHMAX1,2  max. x values allowed
C
C     output:   all emitted partons in COMMON /ABRISR/, final state
C               partons are the first two entries
C               shower evolution traced in COMMON /DGLAPI/
C               IPB1,2    flavours of new initial partons
C               XISR1,2   x values of new initial partons
C
C     attention: quark numbering according to PDG convention, but
C                0 for gluons
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      DIMENSION P1(4),P2(4)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
      COMMON /DGLAPP/ Q2MISR(2),PMISR(2),ZMISR(2),AL2ISR(2),NFSISR
      COMMON /DGLAPI/ Q2SH(2,50),PT2SH(2,50),XPSH(2,50),ZPSH(2,50),
     &                THSH(2,50),SHAT(50),IFL1(2,50),IFL2(2,50),
     &                IBRA(2,100),IFANO(2),ISH(2),NACC
      COMMON /ABRISR/ IFLISR(2,150),PHISR(2,4,150),IPOISR(2,2,50)
C
      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           RHOMAS =  0.766D0,
     &           PI     =  3.14159265359D0,
     &           DEPS   =  1.D-10,
     &           TINY   =  1.D-10)
C
      DIMENSION XHMA(2),NEXT(2),PD1(-6:6),PD2(-6:6),WGGAP(-6:6),
     &          WGPDF(-6:6),XHMI(2),GB(4),PM(4),PN(4),PC(2,4),Q2(2),
     &          IVAL(2),IPAL(2),IL(2),IFSUM(2),IDMO(2)
C
      IREJ = 0
      NTRY = 1000
      NITER = 0
C  debug output
      IF(IDEB(79).GE.10) THEN
        WRITE(6,'(1X,A,/1X,I10,3I3,5E11.3,2(/5X,4E12.3))')
     &    'POHISR:DEBUG:KEV,IHARD,IP1,IP2,Q2H,XH1,XH2,XHM1,XHM2:',
     &    KEVENT,IHARD,IPA1,IPA2,Q2H,XH1,XH2,XHMAX1,XHMAX2,P1,P2
      ENDIF
      IF(IHARD.EQ.0) RETURN
C
 10   CONTINUE
      NACC = 0
      IDMO(1) = IDPDG1
      IDMO(2) = IDPDG2
C
C  copy final state partons to local fields
      DO 50 K=1,2
        IF(IHARD.EQ.1) THEN
          IPOISR(K,1,1) = 1
        ELSE
          IPOISR(K,1,IHARD) = IPOISR(K,2,IHARD-1)+1
        ENDIF
        IPAL(K) = IPOISR(K,1,IHARD)
 50   CONTINUE
      DO 55 I=1,4
        PHISR(1,I,IPAL(1)) = P1(I)
        PHISR(2,I,IPAL(2)) = P2(I)
 55   CONTINUE
      IFLISR(1,IPAL(1)) = IPF1
      IFLISR(2,IPAL(2)) = IPF2
C
C  check limitations, initialize COMMON /DGLAPI/
      IF((Q2H.GT.Q2MISR(1)).AND.(XH1.LT.XHMAX1)) THEN
        NEXT(1) = 1
        Q2SH(1,1) = Q2H
      ELSE
        NEXT(1) = 0
        Q2SH(1,1) = ZERO
      ENDIF
      IF((Q2H.GT.Q2MISR(2)).AND.(XH2.LT.XHMAX2)) THEN
        NEXT(2) = 1
        Q2SH(2,1) = Q2H
      ELSE
        NEXT(2) = 0
        Q2SH(2,1) = ZERO
      ENDIF
C
      ISH(1) = 1
      ISH(2) = 1
      XPSH(1,1) = XH1
      XPSH(2,1) = XH2
C
      IFL1(1,1) = IPA1
      IVAL(1) = 0
      IF((IPA1.EQ.22).OR.(IPA1.EQ.45)) THEN
        NEXT(1) = 0
      ELSE IF(ABS(IPA1).GT.6) THEN
        IFL1(1,1) = SIGN(ABS(IPA1)-6,IPA1)
        IVAL(1) = 1
      ENDIF
      IFL1(2,1) = IPA2
      IVAL(2) = 0
      IF((IPA2.EQ.22).OR.(IPA2.EQ.45)) THEN
        NEXT(2) = 0
      ELSE IF(ABS(IPA2).GT.6) THEN
        IFL1(2,1) = SIGN(ABS(IPA2)-6,IPA2)
        IVAL(2) = 1
      ENDIF
C
      IF(IDEB(79).GE.17) WRITE(6,'(1X,A,/5X,2I2,3E12.3)')
     &  'POHISR:INITIAL TESTS (NEXT1,2 Q2H Q21,2)',NEXT,Q2H,Q2MISR
      IF(NEXT(1)+NEXT(2).EQ.0) GOTO 800
C
C  initialize parton shower loop
      B0QCD = (33.D0-TWO*NFSISR)/6.D0
      AL2ISR(1) = PDFLAM(1)
      AL2ISR(2) = PDFLAM(2)
      XHMA(1) = XHMAX1
      XHMA(2) = XHMAX2
      XHMI(1) = PMISR(1)/PCMP
      XHMI(2) = PMISR(2)/PCMP
      ZPSH(1,1) = ONE
      ZPSH(2,1) = ONE
      THSH(1,1) = 1.D10
      THSH(2,1) = 1.D10
      SHAT1 = XH1*XH2*ECMP**2
      PT2SH(1,1) = Q2H*(ONE-Q2H/SHAT1)
      PT2SH(2,1) = PT2SH(1,1)
      IF(PT2SH(1,1).LT.Q2MISR(1)) NEXT(1) = 0
      IF(PT2SH(2,1).LT.Q2MISR(2)) NEXT(2) = 0
      IFANO(1) = 0
      IFANO(2) = 0
      ZZ = ONE
      IF(IREJ.NE.0) GOTO 800
C
C  main generation loop
C -------------------------------------------------
 100  CONTINUE
C  choose parton side to become solved
        IF((NEXT(1)+NEXT(2)).EQ.2) THEN
          IF(Q2SH(1,ISH(1)).GT.Q2SH(2,ISH(2))) THEN
            IP = 1
          ELSE IF(Q2SH(2,ISH(2)).GT.Q2SH(1,ISH(1))) THEN
            IP = 2
          ELSE
            IP = MAX(INT(DRNDM(SHAT1)*2.D0+0.999999D0),1)
          ENDIF
        ELSE IF(NEXT(1).EQ.1) THEN
          IP = 1
        ELSE IF(NEXT(2).EQ.1) THEN
          IP = 2
        ELSE
          GOTO 800
        ENDIF
        INDX = ISH(IP)
C  INDX now parton position of parton to become solved
C  IP   now side to be treated
        XP = XPSH(IP,INDX)
        Q2P = Q2SH(IP,INDX)
        PT2 = PT2SH(IP,INDX)
        IFLB = IFL1(IP,INDX)
C  check available x
        XMIP = XHMI(IP)
C  cutoff by x limitation: no further development
        IF((XHMA(IP)-XP).LT.XMIP*TWO) THEN
          NEXT(IP) = 0
          Q2SH(IP,INDX) = ZERO
          IF(IDEB(79).GE.17) THEN
            WRITE(6,'(1X,A,/5X,3E12.4,2I3)')
     &        'POHISR:DEBUG:EVOLUTION X-STOP (XP,XMIP,XHMA,IP,INDX)',
     &        XP,XMIP,XHMA(IP),IP,INDX
          ENDIF
          GOTO 100
        ENDIF
C  initial value of evolution variable t
        TT = LOG(AQQALI*Q2P/AL2ISR(IP))
        DO 110 I=-NFSISR,NFSISR
          WGGAP(I) = ZERO
          WGPDF(I) = ZERO
 110    CONTINUE
C  DGLAP weights
        ZMIN = XP/XHMA(IP)
        ZMAX = XP/(XP+XMIP)
        CF = 4./3.
C  q --> q g, g --> g g
        IF(IFLB.EQ.0) THEN
          WGGAP(0) = CF*((ZMAX**2-ZMIN**2)/TWO-TWO*(ZMAX-ZMIN)
     &      +TWO*LOG(ZMAX/ZMIN))
          DO 120 I=1,NFSISR
            WGGAP(I)  = WGGAP(0)
            WGGAP(-I) = WGGAP(0)
 120      CONTINUE
          WGGAP(0) = 6.D0*((ZMIN**3-ZMAX**3)/3.D0+(ZMAX**2-ZMIN**2)/TWO
     &      -TWO*(ZMAX-ZMIN)+LOG(ZMAX/ZMIN*(ONE-ZMIN)/(ONE-ZMAX)))
C  q --> g q, g --> q qb
        ELSE IF(ABS(IFLB).LE.6) THEN
          WGGAP(IFLB) = CF*((ZMIN**2-ZMAX**2)/TWO-ZMAX+ZMIN
     &      -TWO*LOG((ONE-ZMAX)/(ONE-ZMIN)))
          IF(IVAL(IP).EQ.0) WGGAP(0) = 0.5D0*(2./3.*(ZMAX**3-ZMIN**3)
     &      -ZMAX**2+ZMIN**2+ZMAX-ZMIN)
        ELSE
          WRITE(6,'(/1X,A,I7)') 'POHISR:ERROR:UNSUPPORTED PART.ID',IFLB
          CALL POABRT
        ENDIF
C
C  rejection loop for z,t sampling
C ------------------------------------
 200    CONTINUE
          NITER = NITER+1
          IF(NITER.GE.NTRY) THEN
            WRITE(6,'(1X,A,2I6)') 'POHISR:WARNING:TOO MANY REJECTIONS',
     &        NITER,NTRY
            CALL POPREV(-1)
C  clean up event
            IREJ = 1
            GOTO 10
          ENDIF
C  PDF weights
          IF(NQQALI.EQ.1) THEN
            SCALE2 = PT2*AQQPD
          ELSE
            SCALE2 = Q2P*AQQPD
          ENDIF
          CALL PDIS(XP,SCALE2,IP,PD1)
C  anomalous/resolved evolution
          IF(IPAMDL(110).GE.1) THEN
            IF((IDMO(IP).EQ.22).AND.(IFLB.NE.0)
     &         .AND.(IFLB.NE.21)) THEN
              WGDIR = ZERO
              CALL POPDFB(IFLB,XP,SCALE2,PVIRTP(IP),WGDIR)
              XI = DRNDM(XP)*PD1(IFLB)
              IF(WGDIR.GT.XI) THEN
C  debug output
                IF(IDEB(79).GE.17) WRITE(6,'(1X,A,/5X,4E12.5,I2,I3)')
     &       'POHISR:DEBUG:ANOM.TERM (WGDIR,WGPDF,X,SCALE2,IP,IFLB)',
     &            WGDIR,PD1(IFLB),XP,SCALE2,IP,IFLB
                Q2SH(IP,INDX) = ZERO
                NEXT(IP) = 0
                IFANO(IP) = INDX
                GOTO 100
              ENDIF
            ENDIF
          ENDIF
C
          WGTOT = ZERO
          DO 210 I=-NFSISR,NFSISR
            WGPDF(I) = PD1(I)/PD1(IFLB)*5.D0
            WGTOT = WGTOT+WGPDF(I)*WGGAP(I)
 210      CONTINUE
C
 215      CONTINUE
C  sample new t value
          TT = TT*EXP(MAX(-10.D0,LOG(DRNDM(SHAT1))*B0QCD/WGTOT))
          Q2NEW = AL2ISR(IP)*EXP(TT)/AQQALI
C  debug output
          IF(IDEB(79).GE.20) WRITE(6,'(1X,A,E12.5)')
     &      'POHISR:DEBUG:PRE-SELECTED Q2:',Q2NEW
C  compare to limits
          IF(Q2NEW.LT.Q2MISR(IP)) THEN
            Q2SH(IP,INDX) = ZERO
            NEXT(IP) = 0
            IF(IDEB(79).GE.17) WRITE(6,'(1X,A,2E10.3,2I3)')
     &        'POHISR:DEBUG:EVOLUTION Q2-STOP (Q2,Q2MIN,IP,INDX):',
     &        Q2NEW,Q2MISR(IP),IP,INDX
            GOTO 100
          ENDIF
          Q2SH(IP,INDX) = Q2NEW
          TT = LOG(AQQALI*Q2NEW/AL2ISR(IP))
C  selection of flavours
          XI = WGTOT*DRNDM(TT)
          IFLA = -NFSISR-1
 220      CONTINUE
            IFLA = IFLA+1
            XI = XI-WGPDF(IFLA)*WGGAP(IFLA)
          IF((XI.GT.ZERO).AND.(IFLA.LT.NFSISR)) GOTO 220
C  debug output
          IF(IDEB(79).GE.20) WRITE(6,'(1X,A,2I3)')
     &      'POHISR:DEBUG:PRE-SELECTED IFLA (IFLA,IFLB):',IFLA,IFLB
C  selection of z
          CALL POHZSP(IFLA,IFLB,NFSISR,ZMIN,ZMAX,ZZ)
C  debug output
          IF(IDEB(79).GE.20) WRITE(6,'(1X,A,E12.3)')
     &      'POHISR:DEBUG:PRE-SELECTED ZZ',ZZ
C  angular ordering
          THETA = 4.D0*ZZ**2*Q2NEW/((ECMP*XP)**2*(ONE-ZZ))
          IF(THETA.GT.THSH(IP,INDX)) THEN
            IF(IDEB(79).GE.20) WRITE(6,'(1X,A,2E12.3)')
     &        'POHISR:DEBUG:REJECT BY ANGLE (NEW/OLD)',
     &        THETA,THSH(IP,INDX)
            GOTO 215
          ENDIF
C  rejection weight given by new PDFs
          XNEW = XP/ZZ
          PT2NEW = Q2NEW*(ONE-ZZ)
          IF(NQQALI.EQ.1) THEN
            SCALE2 = PT2NEW*AQQPD
          ELSE
            SCALE2 = Q2NEW*AQQPD
          ENDIF
          IF(SCALE2.LT.Q2MISR(IP)) THEN
            Q2SH(IP,INDX) = ZERO
            NEXT(IP) = 0
            IF(IDEB(79).GE.17) WRITE(6,'(1X,A,2E10.3,2I3)')
     &        'POHISR:DEBUG:EVOL.Q2-STOP (SCALE2,Q2MIN,IP,INDX):',
     &        Q2NEW,Q2MISR(IP),IP,INDX
            GOTO 100
          ENDIF
          CALL PDIS(XNEW,SCALE2,IP,PD2)
          IF(PD2(IFLA).LT.1.D-10) GOTO 200
          CALL PDIS(XP,SCALE2,IP,PD1)
          PD1(IFLB) = MAX(PD1(IFLB),1.D-10)
          WGF = PD2(IFLA)/PD1(IFLB)/WGPDF(IFLA)
          IF(NQQALI.EQ.1) WGF = WGF*LOG(Q2NEW*AQQALI/AL2ISR(IP))
     &      /LOG(PT2NEW*AQQALI/AL2ISR(IP))
          IF((WGF.GT.ONE).AND.(IDEB(79).GE.2)) THEN
            WRITE(6,'(1X,A,E12.3)') 'POHISR:WARNING:FINAL WEIGHT:',WGF
            WRITE(6,'(6X,A,I7,2I3,3E11.3)')
     &      'EV,IFLA,IFLB,Q2,PT2,Z:',KEVENT,IFLA,IFLB,Q2NEW,PT2NEW,ZZ
          ENDIF
        IF(WGF.LT.DRNDM(XNEW)) GOTO 200
C
        IF(IDEB(79).GE.15) THEN
          WRITE(6,'(1X,A,/3X,3I3,3E11.3)')
     &      'POHISR:DEBUG:ACCEPTED (IP,IFLA,IFLB,PT2,Q2,Z)',
     &      IP,IFLA,IFLB,PT2NEW,Q2NEW,ZZ
        ENDIF
C
C  branching accepted, registration
        Q2SH(IP,INDX) = Q2NEW
        PT2SH(IP,INDX) = PT2NEW
        ZPSH(IP,INDX) = ZZ
        IFL2(IP,INDX) = IFLA-IFLB
        Q2SH(IP,INDX+1) = Q2NEW
        PT2SH(IP,INDX+1) = PT2SH(IP,INDX)
        XPSH(IP,INDX+1) = XNEW
        THSH(IP,INDX+1) = THETA
        IFL1(IP,INDX+1) = IFLA
        ISH(IP) = ISH(IP)+1
        NACC = NACC+1
        SHAT(NACC) = SHAT1
        IBRA(1,NACC) = IP
        IBRA(2,NACC) = INDX
        SHAT1 = SHAT1/ZZ
C
C  generation of next branching
      IF(NEXT(1)+NEXT(2).NE.0) GOTO 100
C
 800  CONTINUE
      IPB1 = SIGN(ABS(IFL1(1,ISH(1)))+6*IVAL(1),IFL1(1,ISH(1)))
      IPB2 = SIGN(ABS(IFL1(2,ISH(2)))+6*IVAL(2),IFL1(2,ISH(2)))
      XISR1 = XPSH(1,ISH(1))
      XISR2 = XPSH(2,ISH(2))
C  parton kinematics
      IF(NACC.GT.0) THEN
C  final partons in CMS
        PM(3) = (XH1-XH2)*ECMP/TWO
        PM(4) = (XH1+XH2)*ECMP/TWO
        SH = XH1*XH2*ECMP**2
        SSH = SQRT(SH)
        GB(3) = PM(3)/SSH
        GB(4) = PM(4)/SSH
        CALL ALTRA(GB(4),ZERO,ZERO,-GB(3),P1(1),P1(2),P1(3),
     &    P1(4),PTOT1,PHISR(1,1,IPAL(1)),PHISR(1,2,IPAL(1)),
     &    PHISR(1,3,IPAL(1)),PHISR(1,4,IPAL(1)))
        CALL ALTRA(GB(4),ZERO,ZERO,-GB(3),P2(1),P2(2),P2(3),
     &    P2(4),PTOT1,PHISR(2,1,IPAL(2)),PHISR(2,2,IPAL(2)),
     &    PHISR(2,3,IPAL(2)),PHISR(2,4,IPAL(2)))
        IL(1) = 1
        IL(2) = 1
        DO 900 I=1,NACC
          IPA = IBRA(1,I)
          IPB = 3-IPA
          IL(IPA) = IBRA(2,I)
C  initial partons in CMS
          SH = SHAT(I)
          SSH = SQRT(SH)
          SHZ = SH/ZPSH(IPA,IL(IPA))
          SSHZ = SQRT(SHZ)
          Q2(1) = Q2SH(1,IL(1))
          Q2(2) = Q2SH(2,IL(2))
          PC(1,1) = ZERO
          PC(1,2) = ZERO
          PC(1,3) = SQRT((SH+Q2(1)+Q2(2))**2-4.D0*Q2(1)*Q2(2))/(TWO*SSH)
          PC(1,4) = (SH-Q2(1)+Q2(2))/(TWO*SSH)
          PC(2,1) = ZERO
          PC(2,2) = ZERO
          PC(2,3) = -PC(1,3)
          PC(2,4) = SSH-PC(1,4)
C  emitted parton (on mass shell)
          XMS4 = POPAMA(IFL2(IPA,IL(IPA)),1)**2
          EE3 = (SHZ-Q2(IPA)+Q2(IPB)-XMS4)/(TWO*SSH)
          PZ3 = (TWO*PC(IPA,4)*EE3+Q2(IPA)+Q2SH(IPA,IL(IPA)+1)+XMS4)
     &          /(TWO*PC(IPA,3))
          PT3 = EE3**2-PZ3**2+Q2SH(IPA,IL(IPA)+1)
          IF(PT3.LT.ZERO) THEN
            IF(IDEB(79).GE.5) WRITE(6,'(1X,A,E12.3)')
     &        'POHISR:WARNING:REJECTION BY PT3',PT3
            GOTO 10
          ENDIF
          PT3 = SQRT(PT3)
          CALL SFECFE(SFE,CFE)
          PX3 = CFE*PT3
          PY3 = SFE*PT3
          IPAL(IPA) = IPAL(IPA)+1
          PHISR(IPA,1,IPAL(IPA)) = PX3
          PHISR(IPA,2,IPAL(IPA)) = PY3
          PHISR(IPA,3,IPAL(IPA)) = PZ3-PC(IPA,3)
          PHISR(IPA,4,IPAL(IPA)) = EE3-PC(IPA,4)
          IFLISR(IPA,IPAL(IPA)) = IFL2(IPA,IL(IPA))
          PC(IPA,1) = PX3
          PC(IPA,2) = PY3
          PC(IPA,3) = PZ3
          PC(IPA,4) = EE3
C  boost into new CMS
          DO 842 K=1,4
            GB(K) = (PC(1,K)+PC(2,K))/SSHZ
 842      CONTINUE
          CALL ALTRA(GB(4),-GB(1),-GB(2),-GB(3),PC(1,1),PC(1,2),
     &      PC(1,3),PC(1,4),PTOT1,PM(1),PM(2),PM(3),PM(4))
          COG= PM(3)/PTOT1
          SIG= SQRT((ONE-COG)*(ONE+COG))
          COH=ONE
          SIH=ZERO
          IF(PTOT1*SIG.GT.DEPS) THEN
            COH=PM(1)/(SIG*PTOT1)
            SIH=PM(2)/(SIG*PTOT1)
            ANORF=SQRT(COH*COH+SIH*SIH)
            COH=COH/ANORF
            SIH=SIH/ANORF
          ENDIF
          PX3 = ZERO
          PY3 = ZERO
          PZ3 = ZERO
          DO 845 K=1,2
            DO 844 L=IPOISR(K,1,IHARD),IPAL(K)
              CALL ALTRA(GB(4),-GB(1),-GB(2),-GB(3),
     &          PHISR(K,1,L),PHISR(K,2,L),PHISR(K,3,L),PHISR(K,4,L),
     &          PTOT1,PM(1),PM(2),PM(3),PM(4))
              CALL TRANI(PM(1),PM(2),PM(3),COG,SIG,COH,SIH,PN(1),
     &          PN(2),PN(3))
              CALL TRANS(PN(1),PN(2),PN(3),ONE,ZERO,COH,SIH,
     &          PHISR(K,1,L),PHISR(K,2,L),PHISR(K,3,L))
              PHISR(K,4,L) = PM(4)
              PX3 = PX3+PHISR(K,1,L)
              PY3 = PY3+PHISR(K,2,L)
              PZ3 = PZ3+PHISR(K,3,L)
 844        CONTINUE
 845      CONTINUE
 900    CONTINUE
C  boost to global CMS
        PM(3) = (XISR1-XISR2)/TWO
        PM(4) = (XISR1+XISR2)/TWO
        SSH = SQRT(XISR1*XISR2)
        GB(3) = PM(3)/SSH
        GB(4) = PM(4)/SSH
        DO 945 K=1,2
          DO 944 L=IPOISR(K,1,IHARD),IPAL(K)
            CALL ALTRA(GB(4),ZERO,ZERO,GB(3),PHISR(K,1,L),PHISR(K,2,L),
     &        PHISR(K,3,L),PHISR(K,4,L),PTOT1,PM(1),PM(2),PM(3),PM(4))
            PHISR(K,1,L) = PM(1)
            PHISR(K,2,L) = PM(2)
            PHISR(K,3,L) = PM(3)
            PHISR(K,4,L) = PM(4)
 944      CONTINUE
 945    CONTINUE
      ENDIF
      IPOISR(1,2,IHARD) = IPAL(1)
      IPOISR(2,2,IHARD) = IPAL(2)
C  debug output
      IF(IDEB(79).GE.10) THEN
        WRITE(6,'(1X,A,2I5,/6X,A)')
     &    'POHISR:DEBUG:ISR CONFIGURATION(NITER,NACC)',NITER,NACC,
     &    ' SIDE   NO.   IFLB IFLC     Q2SH    PT2SH     XH         ZZ'
        DO 600 II=1,NACC
          K = IBRA(1,II)
          I = IBRA(2,II)
          WRITE(6,'(5X,4I5,4E11.3)')
     &      K,I,IFL1(K,I),IFL2(K,I),Q2SH(K,I),PT2SH(K,I),XPSH(K,I),
     &      ZPSH(K,I)
 600    CONTINUE
        WRITE(6,'(1X,A,2I10/,6X,A,2E12.3,2I5)') 'NUMBER OF EMISSIONS',
     &    ISH(1)-1,ISH(2)-1,'NEW X1,X2,IFL1,ILF2',XISR1,XISR2,IPB1,IPB2
C  check of final configuration
        PX3 = ZERO
        PY3 = ZERO
        PZ3 = ZERO
        EE3 = ZERO
        IFSUM(1) = 0
        IFSUM(2) = 0
        WRITE(6,'(1X,A)') 'POHISR:DEBUG:OUTGOING PARTONS'
        DO 745 K=1,2
          DO 744 L=IPOISR(K,1,IHARD),IPOISR(K,2,IHARD)
            WRITE(6,'(6X,2I4,I6,4E11.3)') K,L,IFLISR(K,L),
     &        PHISR(K,1,L),PHISR(K,2,L),PHISR(K,3,L),PHISR(K,4,L)
            IFSUM(K) = IFSUM(K)+ IFLISR(K,L)
            PX3 = PX3 + PHISR(K,1,L)
            PY3 = PY3 + PHISR(K,2,L)
            PZ3 = PZ3 + PHISR(K,3,L)
            EE3 = EE3 + PHISR(K,4,L)
 744      CONTINUE
 745    CONTINUE
        IFSUM(1) = IFSUM(1)-IPB1
        IFSUM(2) = IFSUM(2)-IPB2
        PZ3 = PZ3 -(XISR1-XISR2)*ECMP/TWO
        EE3 = EE3 -(XISR1+XISR2)*ECMP/TWO
        WRITE(6,'(1X,A,2I4,4E11.3)') 'CHECK:IFL1,2 PCM(1-4)',
     &    IFSUM,PX3,PY3,PZ3,EE3
      ENDIF
      END
C
C
      SUBROUTINE POHZSP(IFLA,IFLB,NFSH,ZMIN,ZMAX,ZZ)
C*********************************************************************
C
C     sampling of z values from DGLAP kernels
C
C     input:  IFLA,IFLB      parton flavours
C             NFSH           flavours involved in hard processes
C             ZMIN           minimal ZZ allowed
C             ZMAX           maximal ZZ allowed
C
C     output: ZZ             z value
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-10)
C
      IF(ZMAX.LE.ZMIN) THEN
        WRITE(6,'(1X,A,2E12.3)')
     &    'POHZSP:WARNING:ZMAX<ZMIN (ZMAX,ZMIN)',ZMAX,ZMIN
        CALL POPREV(-1)
        ZZ = ZERO
        RETURN
      ENDIF
C
      IF(IFLB.EQ.0) THEN
        IF(IFLA.EQ.0) THEN
C  g --> g g
          C1 = ZMAX/ZMIN*(ONE-ZMIN)/(ONE-ZMAX)
          C2 = (ONE-ZMIN)/ZMIN
 100      CONTINUE
            ZZ = ONE/(ONE+C2/C1**DRNDM(ZMIN))
          IF((ONE-ZZ*(ONE-ZZ))**2.LT.DRNDM(ZMAX)) GOTO 100
        ELSE IF(ABS(IFLA).LE.NFSH) THEN
C  q --> q g
          C1 = ZMAX/ZMIN
 200      CONTINUE
            ZZ = ZMIN*C1**DRNDM(ZMIN)
          IF(OHALF*(ONE+(ONE-ZZ)**2).LT.DRNDM(ZMAX)) GOTO 200
        ELSE
          GOTO 900
        ENDIF
      ELSE IF(ABS(IFLB).LE.NFSH) THEN
        IF(IFLA.EQ.0) THEN
C  g --> q qb
          C1 = ZMAX-ZMIN
 300      CONTINUE
            ZZ = ZMIN+C1*DRNDM(ZMIN)
          IF((TWO*ZZ*(ZZ-ONE)+ONE).LT.DRNDM(ZMAX)) GOTO 300
        ELSE IF(ABS(IFLA).LE.NFSH) THEN
C  q --> g q
          C1 = (ONE-ZMAX)/(ONE-ZMIN)
          C2 = ONE-ZMIN
 400      CONTINUE
            ZZ = ONE-C2*C1**DRNDM(ZMIN)
          IF(OHALF*(ONE+ZZ**2).LT.DRNDM(ZMAX)) GOTO 400
        ELSE
          GOTO 900
        ENDIF
      ELSE
        GOTO 900
      ENDIF
C  debug output
      IF(IDEB(80).GE.20) WRITE(6,'(1X,A,2I3,3E11.3)')
     &  'POHZSP:DEBUG:IFLA,IFLB,ZZ,ZMIN,ZMAX',
     &  IFLA,IFLB,ZZ,ZMIN,ZMAX
      RETURN
C
 900  CONTINUE
      WRITE(6,'(/1X,A,2I7)') 'POHZSP:ERROR:INVALID FLAVORS A,B',
     &  IFLA,IFLB
      CALL POABRT
      END
      SUBROUTINE EVENT(NEV,P1,P2,FAC,IREJ)
C********************************************************************
C
C     main subroutine to manage simulation processes
C
C     input: NEV       -1   initialization
C                       1   generation of events
C                       2   generation of events without rejection
C                           due to energy dependent cross section
C                       3   generation of events without rejection
C                           due to initialization energy
C                      -2   output of event generation statistics
C            P1(4)     momentum of particle 1 (internal TARGET)
C            P2(4)     momentum of particle 2 (internal PROJECTILE)
C            FAC       used for initialization:
C                      contains cross section the events corresponds to
C                      during generation: current cross section
C
C     output: IREJ     0: event accepted
C                      1: event rejected
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           IZERO  =  0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
C
      DIMENSION P1(4),P2(4)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C  global CMS system
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER ( MAXPRO = 16 )
      CHARACTER*18 PROC
      COMMON /PEPROC/ PROC(0:MAXPRO)
      INTEGER MXSECT
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
C
      DIMENSION IPRSAM(10),IPRACC(10),IDNS(4),IDNA(4)
C
      IREJ = 0
C  initializations
      IF(NEV.EQ.-1) THEN
        WRITE(6,'(/,1X,A,/,1X,A)') 'INITIALIZATION OF EVENT GENERATION',
     &                             '=================================='
        CALL TIMDAT
        CALL SETMDL(0,0,-2)
C  amplitude parameters
        CALL FITPAR(1)
C        CALL NEWHIS(ZERO,ZERO,ZERO,ZERO,-1,IREJ)
        CALL POHEPI(1,P1,P2,JM1,JM2)
        CALL REJECT(-1)
C  initialize MC package
        CALL POMCIN(JM1,JM2)
        CALL SAMPRO(-1)
        CALL PARTON(-1,0,0,P1,P2,IREJ)
C  initialize quark statistics
        CALL QUASTA(0,0,-1)
C  cross section
        FAC = SIGGEN(4)
        DO 20 I=1,10
          IPRSAM(I) = 0
          IPRACC(I) = 0
 20     CONTINUE
        ISPS = 0
        ISPA = 0
        ISRS = 0
        ISRA = 0
        IHPS = 0
        IHPA = 0
        ISTS = 0
        ISTA = 0
        ISLS = 0
        ISLA = 0
        IDIS = 0
        IDIA = 0
        IDPS = 0
        IDPA = 0
        IDNS(1) = 0
        IDNS(2) = 0
        IDNS(3) = 0
        IDNS(4) = 0
        IDNA(1) = 0
        IDNA(2) = 0
        IDNA(3) = 0
        IDNA(4) = 0
        KACCEP = 0
        KEVENT = 0
        KEVGEN = 0
        ECMSUM = ZERO
        CALL TIMDAT
      ELSE IF(NEV.GT.0) THEN
C
C  -------------- begin event generation ---------------
C
        IPAMDL(13) = 0
        IF(NEV.EQ.3) IPAMDL(13) = 1
        KEVENT = KEVENT+1
C  enable debugging
        CALL TRACE(0,0,0)
        IF(IDEB(68).GE.1) THEN
          IF((MOD(I,50).EQ.0).OR.(IDEB(68).GE.2))
     &      WRITE(6,'(1X,A,2I12)') 'CALL TO EVENT NO',KEVENT,KACCEP
        ENDIF
        CALL POHEPI(0,P1,P2,JM1,JM2)
C  cross section calculation
        FAC = SIGGEN(3)
        IF(NEV.EQ.1) THEN
          IF(DRNDM(FAC).GT.(SIGGEN(3)/SIGGEN(4))) THEN
            IREJ = 1
            IF(IDEB(68).GE.3) WRITE(6,'(1X,A,I10)')
     &        'EVENT:DEBUG:REJECTION DUE TO CROSS SECTION',KEVENT
            RETURN
          ENDIF
        ENDIF
        KEVGEN = KEVGEN+1
        SIGGEN(1) = SIGGEN(4)*DBLE(KEVGEN)/DBLE(KEVENT)
C
        ITRY1 = 0
 50     CONTINUE
          ITRY1 = ITRY1+1
          IF(ITRY1.GT.1) CALL POHEPI(1,P1,P2,JM1,JM2)
C  sample process
          IPROCE = 0
          CALL SAMPRO(IPROCE)
C  sampling statistics
          IPRSAM(IPROCE) = IPRSAM(IPROCE)+1
C
          ITRY2 = 0
 60       CONTINUE
            ITRY2 = ITRY2+1
            IF(ITRY2.GT.1) CALL POHEPI(1,P1,P2,JM1,JM2)
C  sample number of cut graphs according to IPROCE and
C  generate parton configurations+chains
            CALL PARTON(IPROCE,JM1,JM2,P1,P2,IREJ)
C  collect statistics
            ISPS = ISPS+KSPOM
            IHPS = IHPS+KHPOM
            ISRS = ISRS+KSREG
            ISTS = ISTS+KSTRG+KHTRG
            ISLS = ISLS+KSLOO+KHLOO
            IDIS = IDIS+MIN(KHDIR,1)
            IDPS = IDPS+KHDPO+KSDPO
            IF((IDIFR1+IDIFR2+IDDPOM.EQ.0).AND.(KHDIR.GT.0))
     &        IDNS(KHDIR) = IDNS(KHDIR)+1
C  rejection?
          IF(IREJ.NE.0) THEN
            IF(IREJ.EQ.50) THEN
              IF(NEV.EQ.1) RETURN
              IREJ = 2
            ENDIF
            IFAIL(1) = IFAIL(1)+1
            IF(IDEB(68).GE.2) THEN
              WRITE(6,'(/1X,A,2I5)')
     &          'EVENT:WARNING:REJECTION BY PARTON',ITRY2,IREJ
              CALL POPREV(-1)
            ENDIF
            IF(ITRY1.GT.5) RETURN
            IF(IREJ.GE.5) THEN
              IF(ISWMDL(2).EQ.0) RETURN
              GOTO 50
            ENDIF
            IF(ITRY2.LT.5) GOTO 60
            GOTO 50
          ENDIF
C  fragmentation of chains
          CALL POFRAG(IREJ)
C  rejection?
          IF(IREJ.NE.0) THEN
            IFAIL(23) = IFAIL(23)+1
            IF(IDEB(68).GE.2)  THEN
              WRITE(6,'(/1X,A,2I5)')
     &          'EVENT:WARNING:REJECTION BY POFRAG',ITRY2,IREJ
              CALL POPREV(-1)
            ENDIF
            GOTO 50
          ENDIF
C  event now completely processed and accepted
C  acceptance statistics
          IPRACC(IPROCE) = IPRACC(IPROCE)+1
          ISPA = ISPA+KSPOM
          IHPA = IHPA+KHPOM
          ISRA = ISRA+KSREG
          ISTA = ISTA+(KSTRG+KHTRG)
          ISLA = ISLA+(KSLOO+KHLOO)
          IDIA = IDIA+MIN(KHDIR,1)
          IDPA = IDPA+KHDPO+KSDPO
          IF((IDIFR1+IDIFR2.EQ.0).AND.(KHDIR.GT.0))
     &      IDNA(KHDIR) = IDNA(KHDIR)+1
C  check of conservation of quantum numbers
          IF(IDEB(68).GE.-5) THEN
            CALL POQCHK(-1,IREJ)
            IF(IREJ.NE.0) GOTO 50
          ENDIF
C  debug output
          IF(IDEB(68).GE.20) THEN
            IF(IDEB(68).GE.25) THEN
              CALL POPREV(1)
            ELSE
              CALL POPREV(0)
            ENDIF
          ENDIF
C  effective cross section
          KACCEP = KACCEP+1
          SIGGEN(2) = SIGGEN(4)*DBLE(KACCEP)/DBLE(KEVENT)
          ECMSUM = ECMSUM+ECM
      ELSE IF(NEV.EQ.-2) THEN
C
C  ---------------- end of event generation ----------------------
C
        WRITE(6,'(/,1X,A,/,1X,A,/1X,A,3I12,/1X,A,F12.1)')
     &    'STATISTICS OF EVENT GENERATION',
     &    '==============================',
     &    'CALLED,GENERATED,ACCEPTED EVENTS:',KEVENT,KEVGEN,KACCEP,
     &    'AVERAGE CMS ENERGY:',ECMSUM/DBLE(KACCEP)
        CALL TIMDAT
C  write out statistics
        IF(KEVENT.GT.50) THEN
C
          FAC1 = SIGGEN(4)/DBLE(KEVENT)
          FAC2 = FAC/DBLE(KACCEP)
          WRITE(6,'(/1X,A,/1X,A)')
     &      'EVENT:DEBUG:GENERATED AND ACCEPTED EVENTS:',
     &      '========================================='
          WRITE(6,'(3X,A)')
     &   'PROCESS, SAMPLED, ACCEPTED, CROSS SECTION (INTERNAL/EXTERNAL)'
          WRITE(6,'(3X,A,2I12,2E14.4)') 'NON.DIFF.',IPRSAM(1),IPRACC(1),
     &      DBLE(IPRACC(1))*FAC1,DBLE(IPRACC(1))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'ELAS SCA.',IPRSAM(2),IPRACC(2),
     &      DBLE(IPRACC(2))*FAC1,DBLE(IPRACC(2))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'QELA SCA.',IPRSAM(3),IPRACC(3),
     &      DBLE(IPRACC(3))*FAC1,DBLE(IPRACC(3))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DOUB.POM.',IPRSAM(4),IPRACC(4),
     &      DBLE(IPRACC(4))*FAC1,DBLE(IPRACC(4))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DIFF.PAR1',IPRSAM(5),IPRACC(5),
     &      DBLE(IPRACC(5))*FAC1,DBLE(IPRACC(5))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DIFF.PAR2',IPRSAM(6),IPRACC(6),
     &      DBLE(IPRACC(6))*FAC1,DBLE(IPRACC(6))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DOUB.DIF.',IPRSAM(7),IPRACC(7),
     &      DBLE(IPRACC(7))*FAC1,DBLE(IPRACC(7))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DIR ALL  ',IPRSAM(8),IPRACC(8),
     &      DBLE(IPRACC(8))*FAC1,DBLE(IPRACC(8))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DIR x RES',IDNS(1),IDNA(1),
     &      DBLE(IDNA(1))*FAC1,DBLE(IDNA(1))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'RES x DIR',IDNS(2),IDNA(2),
     &      DBLE(IDNA(2))*FAC1,DBLE(IDNA(2))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DIR x DIR',IDNS(3),IDNA(3),
     &      DBLE(IDNA(3))*FAC1,DBLE(IDNA(3))*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'SOFT POM.',ISPS,ISPA,
     &      DBLE(ISPA)*FAC1,DBLE(ISPA)*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'HARD POM.',IHPS,IHPA,
     &      DBLE(IHPA)*FAC1,DBLE(IHPA)*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'SOFT REG.',ISRS,ISRA,
     &      DBLE(ISRA)*FAC1,DBLE(ISRA)*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'ENH. TRG.',ISTS,ISTA,
     &      DBLE(ISTA)*FAC1,DBLE(ISTA)*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'ENH. LOG.',ISLS,ISLA,
     &      DBLE(ISLA)*FAC1,DBLE(ISLA)*FAC2
          WRITE(6,'(3X,A,2I12,2E14.4)') 'DOUB.POM.',IDPS,IDPA,
     &      DBLE(IDPA)*FAC1,DBLE(IDPA)*FAC2
          WRITE(6,'(2(/1X,A,E12.4)/)') ' SAMPLED CROSS SECTION (mb)',
     &      SIGGEN(1),'ACCEPTED CROSS SECTION (mb)',SIGGEN(2)
C
          CALL REJECT(-2)
          CALL SAMPRO(-2)
          CALL PARTON(-2,0,0,P1,P2,IREJ)
C  statistics of hard scattering processes
          WRITE(6,'(2(/1X,A))') 
     &      'EVENT:DEBUG:STATISTICS OF HARD SCATTERING PROCESSES',
     &      '==================================================='
          DO 43 K=1,4
            IF(MXSECT(1,0,K).GT.0) THEN
              WRITE(6,'(5X,A,I3)')
     &      'PROCESS (ACCEPTED,X-SECTION INTERNAL/EXTERNAL) FOR IP:',K
              DO 47 M=0,MAXPRO
                WRITE(6,'(1X,I3,1X,A,2X,2I12,2E14.4)') M,PROC(M),
     &            MXSECT(1,M,K),MXSECT(2,M,K),DBLE(MXSECT(2,M,K))*FAC1,
     &            DBLE(MXSECT(2,M,K))*FAC2
 47           CONTINUE
            ENDIF
 43       CONTINUE
C
          IF(IDEB(68).GE.0) CALL QUASTA(0,0,-2)
          IF(IDEB(68).GE.2) CALL PARTPT(-2,0,0,0.D0,0)
        ELSE
          WRITE(6,'(/1X,A,I4,/)') 'NO OUTPUT OF STATISTICS',KEVENT
        ENDIF
      ELSE
        WRITE(6,'(/1X,A,I7)') 'EVENT:ERROR:UNSUPPORTED NEV',NEV
      ENDIF
      END
C
C
      SUBROUTINE PARTON(IPROC,JM1,JM2,P1,P2,IREJ)
C********************************************************************
C
C     calculation of complete parton configuration
C
C     input:  IPROC   process ID  1 nondiffractive
C                                 2 elastic
C                                 3 quasi-ela. rho,omega,phi prod.
C                                 4 double Pomeron
C                                 5 single diff 1
C                                 6 single diff 2
C                                 7 double diff diss.
C                                 8 direct photon
C             JM1,2   index of mother particles in /HEPEVS/
C
C
C     output: complete parton configuration in HEPEVS,HEPEVE
C             IREJ                1 failure
C                                 0 success
C                                50 rejection due to user cutoffs
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION P1(4),P2(4)
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
C
      IREJ = 0
C  clear event statistics
      KSPOM = 0
      KHPOM = 0
      KSREG = 0
      KHDIR = 0
      KSTRG = 0
      KHTRG = 0
      KSLOO = 0
      KHLOO = 0
      KHARD = 0
      KSOFT = 0
      KSDPO = 0
      KHDPO = 0
C**********************************************************
C  nondiffractive resolved processes
      IF(IPROC.EQ.1) THEN
C  sample number of interactions
 555    CONTINUE
        IINT = 0
        IP   = 1
C  generate only hard events
        IF(ISWMDL(2).EQ.0) THEN
          MHPOM = 1
          MSPOM = 0
          MSREG = 0
          MHDIR = 0
        ELSE
C  minimum bias events
          CALL SAMPRB(ECM,IP,IINT,JINT,KINT,LINT,MINT)
          MINT = 0
          MHDIR = 0
C
C  resolved soft processes: pomeron and reggeon
          MSPOM = IINT
          MSREG = JINT
C  resolved hard process: hard pomeron
          MHPOM = KINT
C  resolved absorptive corrections
          MPTRI = LINT
          MPLOO = MINT
C  restrictions given by user
C  ----------------------------
          IF(ISWMDL(15).EQ.-1) THEN
            MHPOM = 0
            IF(MSREG.GT.0) THEN
              MSPOM = 0
              MSREG = 1
            ELSE
              MSPOM = 1
              MSREG = 0
            ENDIF
          ELSE IF(ISWMDL(15).EQ.0) THEN
            IF(MHPOM.GT.0) THEN
              MHPOM = 1
              MSPOM = 0
              MSREG = 0
            ELSE IF(MSPOM.GT.0) THEN
              MSPOM = 1
              MSREG = 0
            ELSE
              MSREG = 1
            ENDIF
          ELSE IF(ISWMDL(15).EQ.1) THEN
            MHPOM = MIN(1,MHPOM)
          ELSE IF(ISWMDL(15).EQ.2) THEN
            IF(MSPOM.GT.0) THEN
              MSPOM = 1
              MSREG = 0
            ELSE
              MSREG = 1
            ENDIF
          ENDIF
        ENDIF
C  ----------------------------
C
        KSPOM = MSPOM
        KSREG = MSREG
        KHPOM = MHPOM
        KHDIR = MHDIR
        KSTRG = MPTRI
        KSLOO = MPLOO
C  statistics
        ISPS = ISPS+KSPOM
        IHPS = IHPS+KHPOM
        ISRS = ISRS+KSREG
        ISTS = ISTS+KSTRG
        ISLS = ISLS+KSLOO
C  user limitations
C        CALL PRESEL(10,IREJ)
        IF(IREJ.NE.0) RETURN
C
        IF(IDEB(3).GE.5) THEN
          WRITE(6,'(1X,A,I10,I7,6I4)')
     &      'PARTON:DEBUG:EV,SP,SR,HP,HD,ET,EL',
     &      KEVENT,MSPOM,MSREG,MHPOM,MHDIR,MPTRI,MPLOO
        ENDIF
        ITRY2 = 0
 50     CONTINUE
        ITRY2 = ITRY2+1
        IF(ITRY2.GT.1) CALL POHEPI(1,P1,P2,JM1,JM2)
        CALL STDPAR(JM1,JM2,MSPOM,MSREG,MHPOM,MHDIR,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(2) = IFAIL(2) + 1
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY STDPAR ',ITRY2
            CALL POPREV(-1)
          ENDIF
          RETURN
        ENDIF
        IF(MHPOM.GT.0) THEN
          IDNODF = 3
        ELSE IF(MSPOM.GT.0) THEN
          IDNODF = 2
        ELSE
          IDNODF = 1
        ENDIF
C  check of quantum numbers of parton configurations
        IF(IDEB(3).GE.0) THEN
          CALL POQCHK(1,IREJ)
          IF(IREJ.NE.0) GOTO 50
        ENDIF
C  sample chains to prepare fragmentation
        CALL CHAINS(1,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(30) = IFAIL(30)+1
          IF(IDEB(3).GE.2)  THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY CHAINS',ITRY2
            CALL POPREV(-1)
          ENDIF
          IF(ITRY2.LT.20) GOTO 50
          IF(IDEB(3).GE.1) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION',ITRY2
            CALL POPREV(-1)
          ENDIF
          RETURN
        ENDIF
C  statistics
        ISPA = ISPA+KSPOM
        IHPA = IHPA+KHPOM
        ISRA = ISRA+KSREG
        ISTA = ISTA+KSTRG
        ISLA = ISLA+KSLOO
C
C**********************************************************
C  elastic scattering / quasi-elastic rho/omega/phi production
      ELSE IF((IPROC.EQ.2).OR.(IPROC.EQ.3)) THEN
        IF(IDEB(3).GE.5) THEN
          WRITE(6,'(1X,A,I10,I4)') 'PARTON:DEBUG:EV,QELAST. VMP',
     &      KEVENT,IPROC
        ENDIF
        CALL QELAST(IPROC,JM1,JM2,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY QELAST',IREJ
            CALL POPREV(-1)
          ENDIF
          RETURN
        ENDIF
C  prepare possible decays
        CALL CHAINS(1,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(30) = IFAIL(30)+1
          RETURN
        ENDIF
C
C**********************************************************
C  double Pomeron scattering
      ELSE IF(IPROC.EQ.4) THEN
        MSOFT = 0
        MHARD = 0
        IF(IDEB(3).GE.5) WRITE(6,'(1X,A,I10)')
     &      'PARTON:DEBUG:EV,DOUBLE-POMERON SCATTERING',KEVENT
        IDPS = IDPS+1
        ITRY2 = 0
 60     CONTINUE
        ITRY2 = ITRY2+1
C  clear and write initial particles into HEPEVS
        IF(ITRY2.GT.1) CALL POHEPI(1,P1,P2,JM1,JM2)
        CALL PODBLE(JM1,JM2,MSOFT,MHARD,1,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY PODBLE',IREJ
            CALL POPREV(-1)
          ENDIF
          RETURN
        ENDIF
C  check of quantum numbers of parton configurations
        IF(IDEB(3).GE.0) THEN
          CALL POQCHK(1,IREJ)
          IF(IREJ.NE.0) GOTO 60
        ENDIF
C  sample chains to prepare fragmentation
        CALL CHAINS(1,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(30) = IFAIL(30)+1
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY CHAINS',ITRY2
            CALL POPREV(-1)
          ENDIF
          IF(ITRY2.LT.10) GOTO 60
          WRITE(6,'(/1X,A,I5)') 'PARTON:WARNING:REJECTION',ITRY2
          CALL POPREV(-1)
          RETURN
        ENDIF
        IDPA = IDPA+1
C
C**********************************************************
C  single / double diffraction
      ELSE IF((IPROC.GE.5).AND.(IPROC.LE.7)) THEN
        MSOFT = 0
        MHARD = 0
        IF(IDEB(3).GE.5) THEN
          WRITE(6,'(1X,A,I10,2I4)')
     &      'PARTON:DEBUG:EV,DIFFRACTION',
     &      KEVENT,IPAR1,IPAR2
        ENDIF
        IF(IPROC.EQ.5) ID1S = ID1S+1
        IF(IPROC.EQ.6) ID2S = ID2S+1
        IF(IPROC.EQ.7) ID3S = ID3S+1
        ITRY2 = 0
 70     CONTINUE
        ITRY2 = ITRY2+1
        IPAR1 = 1
        IPAR2 = 1
        IF(IPROC.EQ.5) IPAR2 = 0
        IF(IPROC.EQ.6) IPAR1 = 0
C  clear and write initial particles into HEPEVS
        IF(ITRY2.GT.1) CALL POHEPI(1,P1,P2,JM1,JM2)
        CALL PODIFF(IPAR1,IPAR2,JM1,JM2,MSOFT,MHARD,0,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY PODIFF',IREJ
            CALL POPREV(-1)
          ENDIF
          RETURN
        ENDIF
C  check of quantum numbers of parton configurations
        IF(IDEB(3).GE.0) THEN
          CALL POQCHK(1,IREJ)
          IF(IREJ.NE.0) GOTO 70
        ENDIF
C  sample chains to prepare fragmentation
        CALL CHAINS(1,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(30) = IFAIL(30)+1
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY CHAINS',ITRY2
            CALL POPREV(-1)
          ENDIF
          IF(ITRY2.LT.10) GOTO 70
          WRITE(6,'(/1X,A,I5)')
     &      'PARTON:WARNING:REJECTION',ITRY2
          CALL POPREV(-1)
          RETURN
        ENDIF
        IF(IPROC.EQ.5) ID1A = ID1A+1
        IF(IPROC.EQ.6) ID2A = ID2A+1
        IF(IPROC.EQ.7) ID3A = ID3A+1
C
C**********************************************************
C  single / double direct processes
      ELSE IF(IPROC.EQ.8) THEN
        MSREG = 0
        MSPOM = 0
        MHPOM = 0
        MHDIR = 1
        IF(IDEB(3).GE.5) THEN
          WRITE(6,'(1X,A,I10)') 'PARTON:DEBUG:EV,DIRECT PROC',KEVENT
        ENDIF
        KSPOM = MSPOM
        KSREG = MSREG
        KHPOM = MHPOM
        KHDIR = 4
        IDIS = IDIS+MHDIR
        ITRY2 = 0
 80     CONTINUE
        ITRY2 = ITRY2+1
C  clear and write initial particles into HEPEVS
        IF(ITRY2.GT.1) CALL POHEPI(1,P1,P2,JM1,JM2)
        CALL STDPAR(JM1,JM2,MSPOM,MSREG,MHPOM,MHDIR,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(2) = IFAIL(2) + 1
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)')
     &        'PARTON:WARNING:REJECTION BY STDPAR',ITRY2
            CALL POPREV(-1)
          ENDIF
          RETURN
        ENDIF
        IDNODF = 4
C  check of quantum numbers of parton configurations
        IF(IDEB(3).GE.0) THEN
          CALL POQCHK(1,IREJ)
          IF(IREJ.NE.0) GOTO 80
        ENDIF
C  sample chains to prepare fragmentation
        CALL CHAINS(1,IREJ)
        IF(IREJ.NE.0) THEN
          IF(IREJ.EQ.50) RETURN
          IFAIL(30) = IFAIL(30)+1
          IF(IDEB(3).GE.2) THEN
            WRITE(6,'(/1X,A,I5)') 
     &        'PARTON:WARNING:REJECTION BY CHAINS',ITRY2
            CALL POPREV(-1)
          ENDIF
          IF(ITRY2.LT.10) GOTO 80
          WRITE(6,'(/1X,A,I5)') 'PARTON:WARNING:REJECTION',ITRY2
          CALL POPREV(-1)
          RETURN
        ENDIF
        IF(IPROC.EQ.5) ID1A = ID1A+1
        IF(IPROC.EQ.6) ID2A = ID2A+1
        IF(IPROC.EQ.7) ID3A = ID3A+1
        IDIA = IDIA+MHDIR
C
C**********************************************************
C  initialize control statistics
      ELSE IF(IPROC.EQ.-1) THEN
        CALL SAMPRB(ECM,-1,0,0,0,0,0)
        CALL STDPAR(-1,0,0,0,0,0,IREJ)
        CALL SEAFLA(-1,0,0,DUMMY)
        IF((IFPAP(1).EQ.22).OR.(IFPAP(2).EQ.22)) CALL QELAST(-1,1,2,0)
        ISPS = 0
        ISPA = 0
        ISRS = 0
        ISRA = 0
        IHPS = 0
        IHPA = 0
        ISTS = 0
        ISTA = 0
        ISLS = 0
        ISLA = 0
        ID1S = 0
        ID1A = 0
        ID2S = 0
        ID2A = 0
        ID3S = 0
        ID3A = 0
        IDPS = 0
        IDPA = 0
        IDIS = 0
        IDIA = 0
        CALL CHAINS(-1,IREJ)
        CALL PODIFF(0,0,0,0,0,0,-1,IREJ)
        RETURN
C
C  produce statistics summary
      ELSE IF(IPROC.EQ.-2) THEN
        IF(ISWMDL(2).NE.0) CALL SAMPRB(ECM,-2,0,0,0,0,0)
        IF(IDEB(3).GE.0) THEN
          WRITE(6,'(/1X,A,/1X,A)')
     &      'PARTON:DEBUG:ACCEPTED PARTON CONFIGURATIONS:',
     &      '============================================'
          WRITE(6,'(10X,A)') 'PROCESS          SAMPLED      ACCEPTED'
          WRITE(6,'(10X,A,2I12)') 'SOFT POM.',ISPS,ISPA
          WRITE(6,'(10X,A,2I12)') 'HARD POM.',IHPS,IHPA
          WRITE(6,'(10X,A,2I12)') 'SOFT REG.',ISRS,ISRA
          WRITE(6,'(10X,A,2I12)') 'ENH. TRI.',ISTS,ISTA
          WRITE(6,'(10X,A,2I12)') 'ENH. LOO.',ISLS,ISLA
          WRITE(6,'(10X,A,2I12)') 'DIFF.PA1.',ID1S,ID1A
          WRITE(6,'(10X,A,2I12)') 'DIFF.PA2.',ID2S,ID2A
          WRITE(6,'(10X,A,2I12)') 'DOUB.DIF.',ID3S,ID3A
          WRITE(6,'(10X,A,2I12)') 'DOUB.POM.',IDPS,IDPA
          WRITE(6,'(10X,A,2I12/)') 'DIR.PHOT.',IDIS,IDIA
        ENDIF
        CALL STDPAR(-2,0,0,0,0,0,IREJ)
        IF((IFPAP(1).EQ.22).OR.(IFPAP(2).EQ.22)) CALL QELAST(-2,1,2,0)
        CALL CHAINS(-2,IREJ)
        CALL PODIFF(0,0,0,0,0,0,-2,IREJ)
        CALL SEAFLA(-2,0,0,DUMMY)
        RETURN
      ELSE
        WRITE(6,'(1X,A,I2)')
     &    'PARTON:ERROR:UNKNOWN PROCESS ID ',IPROC
        STOP
      ENDIF
      END
C
C
      SUBROUTINE POMCIN(JM1,JM2)
C********************************************************************
C
C     initialization of MC event generation
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           PIMASS =  0.13D0,
     &           TINY   =  1.D-10)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
      PARAMETER (MAXPRO = 16, MAXTAB = 20)
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      PARAMETER (IEETAB=10,IIMAX=30,KKMAX=10)
      REAL PROB
      COMMON /PROBAB/ PROB(4,IEETAB,0:IIMAX,0:KKMAX),EPTAB(4,IEETAB),
     &                IEEMAX,IMAX,KMAX
      COMMON /XSETAB/ SIGTAB(4,70,IEETAB),SIGECM(4,IEETAB),ISIMAX
C
      CHARACTER*8 PANAME
      DIMENSION ECMF(4)
C
C  initialize fragmentation
      CALL POFRIN
C
C  reset interpolation tables
      DO 50 I=1,4
        DO 60 J=1,10
          DO 70 K=1,70
            SIGTAB(I,K,J) = ZERO
 70       CONTINUE
          SIGECM(I,J) = ZERO
 60     CONTINUE
 50   CONTINUE
C  max number of allowed colors (large N expansion)
      IC1 = 0
      IC2 = 10000
      CALL SELCOL(IC1,IC2,0,0,0,0,-1)
C  lower energy limit of initialization
      ETABLO = PARMDL(19)
      IF(ECM.LE.5.D0) ETABLO = MIN(2.D0,ETABLO)
C
      WRITE(6,'(/,1X,A,2F12.1)')
     &  'POMCIN:DEBUG:SELECTED ENERGY RANGE (SQRT(S))',ETABLO,ECM
      WRITE(6,'(5X,A,A,F7.3,E15.4)')
     &  'PARTICLE 1 (NAME,MASS,VIRTUALITY): ',PANAME(JM1,2),
     &  PMASS(1),PVIRT(1)
      WRITE(6,'(5X,A,A,F7.3,E15.4)')
     &  'PARTICLE 2 (NAME,MASS,VIRTUALITY): ',PANAME(JM2,2),
     &  PMASS(2),PVIRT(2)
C  cuts on probabilities of multiple interactions
      IMAX = MIN(IPAMDL(32),IIMAX)
      KMAX = MIN(IPAMDL(33),KKMAX)
      AH = 2.D0*PTCUT(1)/ECM
      IMAX = MAX(5,MIN(IMAX,INT(ECM/2.0D0)))
      KMAX = MIN(KMAX,1+INT(0.9*ONE/AH))
C  hard interpolation table
      ISTTAB = MIN(IPAMDL(30),MAXTAB)
      IF(ECM.LT.100.D0) ISTTAB = MIN(ISTTAB,15)
      IF(ECM.LT.50.D0)  ISTTAB = MIN(ISTTAB,7)
      IF(ECM.LT.10.D0)  ISTTAB = MIN(ISTTAB,3)
      ECMF(1) = ECM
      ECMF(2) = 0.9D0*ECMF(1)
      ECMF(3) = ECMF(2)
      ECMF(4) = ECMF(2)
      CALL POHMCI(ECMF)
C  dimension of interpolation table of cut probabilities
      IEEMAX = MIN(IPAMDL(31),IEETAB)
      IF(ECM.LT.100.D0) IEEMAX = MIN(IEEMAX,5)
      IF(ECM.LT.10.D0)  IEEMAX = MIN(IEEMAX,3)
      ISIMAX = IEEMAX
C  calculate probability distribution
      I0 = 4
      IF(ISWMDL(2).EQ.0) I0 = 1
      DO 150 IP=I0,1,-1
      ECMPRO = ECMF(IP)*1.001D0
      IF(IEEMAX.GT.1) THEN
        ELMIN = LOG(ETABLO)
        EDELTA = (LOG(ECMPRO)-ELMIN)/DBLE(MAX(1,IEEMAX-1))
        DO 100 I=1,IEEMAX
          ECMPRO = EXP(ELMIN+DBLE(I-1)*EDELTA)
          CALL PRBDIS(IP,ECMPRO,I)
 100    CONTINUE
      ELSE
        CALL PRBDIS(IP,ECMPRO,1)
      ENDIF
      IF(((IDEB(62).GE.0).AND.(IP.EQ.1)).OR.(IDEB(62).GE.1)) THEN
      WRITE(6,'(/1X,A,I3/1X,A,/1X,A)')
     &'POMCIN:TOTAL CROSS SECTIONS (mb) FOR PARTICLE COMBINATION',IP,
     &' ECM    SIGTOT  SIGELA  SIGINE  SIGQEL  SIGSD1  SIGSD2  SIGDD',
     &'============================================================='
      DO 200 I=1,IEEMAX
        WRITE(6,'(1X,8(1PE9.2))') SIGECM(IP,I),SIGTAB(IP,1,I),
     &    SIGTAB(IP,2,I),SIGTAB(IP,28,I),SIGTAB(IP,3,I),
     &    SIGTAB(IP,30,I)+SIGTAB(IP,32,I),
     &    SIGTAB(IP,31,I)+SIGTAB(IP,33,I),
     &    SIGTAB(IP,34,I)+SIGTAB(IP,35,I)
 200  CONTINUE
      ENDIF
      IF(IDEB(62).GE.1) THEN
      WRITE(6,'(/1X,A,I3/1X,A,/1X,A)')
     &'POMCIN:PARTIAL CROSS SECTIONS (mb) FOR PARTICLE COMBINATION',IP,
     &' ECM    SIGSD1L SIGSD1H SIGSD2L SIGSD2H SIGDDL  SIGDDH  SIGCDF',
     &'=============================================================='
      DO 205 I=1,IEEMAX
        WRITE(6,'(1X,8(1PE9.2))') SIGECM(IP,I),SIGTAB(IP,30,I),
     &    SIGTAB(IP,32,I),SIGTAB(IP,31,I),SIGTAB(IP,33,I),
     &    SIGTAB(IP,34,I),SIGTAB(IP,35,I),SIGTAB(IP,36,I)
 205  CONTINUE
      ENDIF
      IF(IDEB(62).GE.2) THEN
      WRITE(6,'(/1X,A,I3/1X,A,/1X,A)')
     &'POMCIN:BORN GRAPH X-SECTIONS (mb) FOR PARTICLE COMBINATION',IP,
     &' ECM    SIGSVDM SIGHRES SIGHDIR SIGTR1  SIGTR2  SIGLOO SIGDPO',
     &'============================================================='
      DO 210 I=1,IEEMAX
        WRITE(6,'(1X,8(1PE9.2))') SIGECM(IP,I),
     &    SIGTAB(IP,56,I)+SIGTAB(IP,57,I),SIGTAB(IP,58,I),
     &    SIGTAB(IP,59,I),SIGTAB(IP,60,I),SIGTAB(IP,61,I),
     &    SIGTAB(IP,62,I),SIGTAB(IP,63,I)
 210  CONTINUE
      WRITE(6,'(/1X,A,I3/1X,A,/1X,A)')
     &'POMCIN:UNITARIZED X-SECTIONS (mb) FOR PARTICLE COMBINATION',IP,
     &' ECM    SIGSVDM SIGHVDM  SIGTR1  SIGTR2  SIGLOO SIGDPO  SLOPE',
     &'============================================================='
      DO 215 I=1,IEEMAX
        WRITE(6,'(1X,8(1PE9.2))') SIGECM(IP,I),SIGTAB(IP,69,I),
     &    SIGTAB(IP,70,I),SIGTAB(IP,32,I),SIGTAB(IP,33,I),
     &    SIGTAB(IP,35,I),SIGTAB(IP,36,I),SIGTAB(IP,39,I)
 215  CONTINUE
      ENDIF
      IF(IDEB(62).GE.1) THEN
      WRITE(6,'(/1X,A,/1X,A,/1X,A)')
     &'POMCIN:EXPECTED AVERAGE NUMBER OF CUTS IN NON-DIFF EVENTS:',
     &' ECM  POM-S / POM-H / REG / ENH-TRL / ENH-DBLE',
     &'=============================================='
      DO 220 I=1,IEEMAX
        WRITE(6,'(1X,6(1PE9.2))') SIGECM(IP,I),SIGTAB(IP,64,I),
     &    SIGTAB(IP,65,I),SIGTAB(IP,66,I),SIGTAB(IP,67,I),
     &    SIGTAB(IP,68,I)
 220  CONTINUE
      ENDIF
 150  CONTINUE
C  simulate only hard scatterings
      IF(ISWMDL(2).EQ.0) THEN
        WRITE(6,'(2(/1X,A))') 
     &    'WARNING: GENERATION OF HARD SCATTERINGS ONLY!',
     &    '============================================='
        DO 155 I=2,7
          IPRON(I) = 0
 155    CONTINUE
        SIGGEN(4) = 0.D0
        DO 160 I=1,IEEMAX
          SIGMAX = 0.D0
          IF(IPRON(1).EQ.1) SIGMAX = SIGTAB(1,58,I)
          IF(IPRON(8).EQ.1) SIGMAX = SIGMAX+SIGTAB(1,59,I)
          IF(SIGMAX.GT.SIGGEN(4)) THEN
            ISIGM = I
            SIGGEN(4) = SIGMAX
          ENDIF
 160    CONTINUE
      ELSE
      WRITE(6,'(2(/1X,A))') 'ACTIVATED PROCESSES, CROSS SECTION:',
     &                      '==================================='
      WRITE(6,'(5X,A,I3)') '  NONDIFFR. RESOLVED PROCESSES',IPRON(1)
      WRITE(6,'(5X,A,I3)')  '            ELASTIC SCATTERING',IPRON(2)
      WRITE(6,'(5X,A,I3)')  'QELAST. VECTORMESON PRODUCTION',IPRON(3)
      WRITE(6,'(5X,A,I3)')  '      DOUBLE POMERON PROCESSES',IPRON(4)
      WRITE(6,'(5X,A,I3)')  ' SINGLE DIFFRACT. PARTICLE (1)',IPRON(5)
      WRITE(6,'(5X,A,I3)')  ' SINGLE DIFFRACT. PARTICLE (2)',IPRON(6)
      WRITE(6,'(5X,A,I3)')  '    DOUBLE DIFFRACT. PROCESSES',IPRON(7)
      WRITE(6,'(5X,A,I3)')  '       DIRECT PHOTON PROCESSES',IPRON(8)
      WRITE(6,'(5X,A,I3)')  '  HARD DIFF-DISS. PARTICLE (1)',IPRON(9)
      WRITE(6,'(5X,A,I3)')  '  HARD DIFF-DISS. PARTICLE (2)',IPRON(10)
      WRITE(6,'(5X,A,I3)')  '  HARD DOUBLE POMERON SCATTER.',IPRON(11)
C  calculate effective cross section
      SIGGEN(4) = 0.D0
      DO 165 I=1,IEEMAX
        SIGMAX = 0.D0
        SIGTOT     =  SIGTAB(1,1,I)
        SIGELA     =  SIGTAB(1,2,I)
        SIGVM(0,0) = SIGTAB(1,3,I)
        SIGDIR     = SIGTAB(1,29,I)
        SIGLSD(1)  = SIGTAB(1,30,I)
        SIGLSD(2)  = SIGTAB(1,31,I)
        SIGHSD(1)  = SIGTAB(1,32,I)
        SIGHSD(2)  = SIGTAB(1,33,I)
        SIGLDD     = SIGTAB(1,34,I)
        SIGHDD     = SIGTAB(1,35,I)
        SIGCDF     = SIGTAB(1,36,I)
        IF(IPRON(1).EQ.1) SIGMAX = SIGTOT-SIGELA-SIGVM(0,0)-SIGCDF
     &  -SIGLSD(1)-SIGHSD(1)-SIGLSD(2)-SIGHSD(2)-SIGLDD-SIGHDD-SIGDIR
        IF(IPRON(2).EQ.1) SIGMAX = SIGMAX+SIGELA
        IF(IPRON(3).EQ.1) SIGMAX = SIGMAX+SIGVM(0,0)
        IF(IPRON(4).EQ.1) SIGMAX = SIGMAX+SIGCDF
        IF(IPRON(5).EQ.1) SIGMAX = SIGMAX+SIGLSD(1)+SIGHSD(1)
        IF(IPRON(6).EQ.1) SIGMAX = SIGMAX+SIGLSD(2)+SIGHSD(2)
        IF(IPRON(7).EQ.1) SIGMAX = SIGMAX+SIGLDD+SIGHDD
        IF(IPRON(8).EQ.1) SIGMAX = SIGMAX+SIGDIR
        IF(SIGMAX.GT.SIGGEN(4)) THEN
          ISIGM = I
          SIGGEN(4) = SIGMAX
        ENDIF
 165  CONTINUE
      ENDIF
      IF(SIGGEN(4).LT.1.D-20) THEN
        WRITE(6,'(//1X,A)') 
     &  'POMCIN:ERROR:SELECTED PROCESSES HAVE VANISHING X-SECTION'
        STOP
      ENDIF
      WRITE(6,'(3X,A,3E11.4)') 
     &  'MAXIMUM SEARCH (EMIN/EMAX/EPEAK)',SIGECM(1,1),
     &  SIGECM(1,IEEMAX),SIGECM(1,ISIGM)
      WRITE(6,'(11X,A,E12.4,/)') 'MAX. CROSS SECTION (mb)',SIGGEN(4)
      END
C
C
      SUBROUTINE REJECT(IMODE)
C********************************************************************
C
C     MC rejection counting
C
C     input IMODE    -1   initialization
C                    -2   output of statistics
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           TINY   =  1.D-10)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
C  initialization
      IF(IMODE.EQ.-1) THEN
        DO 100 I=1,NMXJ
          IFAIL(I) = 0
 100    CONTINUE
C
        REJTIT(1)  = 'PARTON ALL'
        REJTIT(2)  = 'ENH GRAPHS'
        REJTIT(3)  = 'STDPAR ALL'
        REJTIT(4)  = 'POMSCA ALL'
        REJTIT(5)  = 'POMSCA INT'
        REJTIT(6)  = 'POMSCA KIN'
        REJTIT(7)  = 'PODIFF ALL'
        REJTIT(8)  = 'REGDIF ALL'
        REJTIT(9)  = 'HRES.DIF.1'
        REJTIT(10) = 'HDIR.DIF.1'
        REJTIT(11) = 'HRES.DIF.2'
        REJTIT(12) = 'HDIR.DIF.2'
        REJTIT(13) = 'PODIFF INT'
        REJTIT(14) = 'HADRON SP2'
        REJTIT(15) = 'HADRON SP3'
        REJTIT(16) = 'POHDIR ALL'
        REJTIT(17) = 'POHDIR INT'
        REJTIT(18) = 'POHDIR KIN'
        REJTIT(19) = 'MCHECK BAR'
        REJTIT(20) = 'MCHECK MES'
        REJTIT(21) = 'DIF.DISS.1'
        REJTIT(22) = 'DIF.DISS.2'
        REJTIT(23) = 'CHAIN FRAG'
        REJTIT(24) = 'MSHELL CHA'
        REJTIT(25) = 'PARTPT HAR'
        REJTIT(26) = 'PARTPT SOF'
        REJTIT(27) = 'POMCOR ALL'
        REJTIT(28) = 'HACHEK DIR'
        REJTIT(29) = 'HACHEK RES'
        REJTIT(30) = 'CHAINS ALL'
        REJTIT(31) = 'POMSCA HAR'
        REJTIT(32) = 'DIFF SLOPE'
        REJTIT(33) = 'GLU2QU ALL'
        REJTIT(34) = 'MASCOR ALL'
        REJTIT(35) = 'PARCOR ALL'
        REJTIT(36) = 'MSHELL PAR'
        REJTIT(37) = 'MSHELL ALL'
        REJTIT(38) = 'SOFTXX ALL'
        REJTIT(39) = 'DB-POM INT'
        REJTIT(40) = 'DB-POM ALL'
C
C  write output
      ELSE IF(IMODE.EQ.-2) THEN
        WRITE(6,'(/,1X,A,/,1X,A)') 'REJECTION STATISTICS:',
     &                             '====================='
        DO 300 I=1,NMXJ
          IF(IFAIL(I).GT.0)
     &      WRITE(6,'(1X,I3,1X,A,5X,I15)') I,REJTIT(I),IFAIL(I)
 300    CONTINUE
      ELSE
        WRITE(6,'(1X,A,I3)') 'WARNING:REJECT:UNSUPPORTED MODE ',IMODE
      ENDIF
      END
C
C
      SUBROUTINE POSPOM(IP,IND1,IND2,KCUT,ISWAP,IREJ)
C***********************************************************************
C
C     registration of one cut pomeron (soft/semihard)
C
C     input:   IP      particle combination the pomeron belongs to
C              IND1,2  position of X values in COMMON /ABRSOF/
C                      1 corresponds to a valence-pomeron
C              KCUT    number of cut pomerons and reggeons 
C
C     output:  ISWAP   exchange of x values
C              IREJ    success/failure
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-8)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      PARAMETER (IEETAB=10)
      COMMON /XSETAB/ SIGTAB(4,70,IEETAB),SIGECM(4,IEETAB),ISIMAX
C
      COMMON /ABRSOF/ XS1(50),XS2(50),IJSI1(50),IJSI2(50),
     &                PSOFT1(4,50),PSOFT2(4,50)
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      COMMON/POMRES/IPOPOS(2,100),IPORES(100),IPOIX1,IPOIX2
C
      DIMENSION P1(4),P2(4),WGXHSD(2),WGX(6)
C
      IREJ = 0
      ISWAP = 0
      JM1 = NPOSP(1)
      JM2 = NPOSP(2)
      CMASS1 = SQRT(XS1(IND1)*XS2(IND2)*ECMP)
      CMASS2 = SQRT(XS1(IND1+1)*XS2(IND2+1)*ECMP)
      CMASS1 = MIN(CMASS1,CMASS2)
      CMASS2 = MAX(CMASS1,CUTMU(1))
C  flavours
      IF(IND1.EQ.1) THEN
        CALL VALFLA(JM1,IFLA1,IFLA2,CMASS2)
        CALL QUASTA(IFLA1,IDHEP(JM1),1)
        CALL QUASTA(IFLA2,IDHEP(JM1),1)
      ELSE
        CALL SEAFLA(JM1,IFLA1,IFLA2,CMASS2)
        CALL QUASTA(IFLA1,IDHEP(JM1),3)
        CALL QUASTA(IFLA2,IDHEP(JM1),3)
      ENDIF
      IF(IND2.EQ.1) THEN
        CALL VALFLA(JM2,IFLB1,IFLB2,CMASS2)
        CALL QUASTA(IFLB1,IDHEP(JM2),1)
        CALL QUASTA(IFLB2,IDHEP(JM2),1)
      ELSE
        CALL SEAFLA(JM2,IFLB1,IFLB2,CMASS2)
        CALL QUASTA(IFLB1,IDHEP(JM2),3)
        CALL QUASTA(IFLB2,IDHEP(JM2),3)
      ENDIF
      DO 75 I=1,4
        P1(I) = PSOFT1(I,IND1)+PSOFT1(I,IND1+1)
        P2(I) = PSOFT2(I,IND2)+PSOFT2(I,IND2+1)
 75   CONTINUE
C*************************
C  pomeron resolved?
      IF((ISWMDL(14).GT.0).AND.(IPOIX1.GT.0)) THEN
        SSUB = (P1(4)+P2(4))**2-(P1(1)+P2(1))**2
     &        -(P1(2)+P2(2))**2-(P1(3)+P2(3))**2
        ESUB = SQRT(SSUB)
C  load cross sections from interpolation table
        IF(ESUB.LE.SIGECM(IP,1)) THEN
          I1 = 1
          I2 = 1
        ELSE IF(ESUB.LT.SIGECM(IP,ISIMAX)) THEN
          DO 50 I=2,ISIMAX
            IF(ESUB.LE.SIGECM(IP,I)) GOTO 200
 50       CONTINUE
 200      CONTINUE
          I1 = I-1
          I2 = I
        ELSE
          WRITE(6,'(/1X,A,2E12.3)')
     &      'POSPOM:WARNING:TOO HIGH ENERGY',ESUB,SIGECM(IP,ISIMAX)
          CALL POPREV(-1)
          I1 = ISIMAX
          I2 = ISIMAX
        ENDIF
        FAC2=0.D0
        IF(I1.NE.I2) FAC2=LOG(ESUB/SIGECM(IP,I1))
     &                    /LOG(SIGECM(IP,I2)/SIGECM(IP,I1))
        FAC1=1.D0-FAC2
C  calculate weights
        WGXPOM = FAC2*SIGTAB(IP,69,I2)+FAC1*SIGTAB(IP,69,I1)
        WGXHSD(1) = FAC2*SIGTAB(IP,32,I2)+FAC1*SIGTAB(IP,32,I1)
        WGXHSD(2) = FAC2*SIGTAB(IP,33,I2)+FAC1*SIGTAB(IP,33,I1)
        WGXHDD = FAC2*SIGTAB(IP,35,I2)+FAC1*SIGTAB(IP,35,I1)
        WGXCDF = FAC2*SIGTAB(IP,36,I2)+FAC1*SIGTAB(IP,36,I1)
        WGX(1) = WGXPOM-3.D0*(WGXHSD(1)+WGXHSD(2)+WGXHDD)+15.D0*WGXCDF
        WGX(2) = WGXCDF
        WGX(3) = WGXHSD(1)
        WGX(4) = WGXHSD(2)
        WGX(5) = WGXHDD
        WGX(6) = 2.D0*(WGXHSD(1)+WGXHSD(2)+WGXHDD)
        SUM  = WGX(1)+WGX(2)+WGX(3)+WGX(4)+WGX(5)+WGX(6)
 205    CONTINUE
        XI = DRNDM(SUM)*SUM
        I = 0
        SUM = 0.D0
 210    CONTINUE
          I = I+1
          SUM = SUM+WGX(I)
        IF((XI.GT.SUM).AND.(I.LT.6)) GOTO 210
C***************
          IF(I.NE.1) THEN
            ISAM = 4
            IF(I.EQ.6) ISAM = 8
            PACC = EXP(-PARMDL(8)*DBLE(ISAM*CUTMU(1))/ESUB)
            IF(DRNDM(XM).GT.PACC) I=1
          ENDIF
C***************
*       print *,'pospom: jm1,jm2',JM1,JM2
*       print *,'esub,kcut,event,i',ESUB,KCUT,KEVENT,I
*       write(6,'(1x,6e11.3)') WGX
C***************
C  do not generate diffraction for events with only one cut
        IF((KCUT.EQ.1).AND.(I.LT.6)) I = 1
C  second scattering needed
        IF(I.GT.1) THEN
          CALL HACODE(IFLA1,IFLA2,IDHA1,IDUM)
          CALL HACODE(IFLB1,IFLB2,IDHA2,IDUM)
          IDPD1 = MPDGHA(IDHA1)
          IDPD2 = MPDGHA(IDHA2)
          CALL REGPAR(1,IDPD1,IDHA1,JM1,JM2,P1(1),P1(2),P1(3),P1(4),
     &      I,0,0,0,IPOS1,1)
          CALL REGPAR(1,IDPD2,IDHA2,JM2,JM1,P2(1),P2(2),P2(3),P2(4),
     &      I,0,0,0,IPOS1,1)
          IND1 = IND1+2
          IND2 = IND2+2
C  update index common
          IPOIX2 = IPOIX2+1
          IPORES(IPOIX2) = I+2
          IPOPOS(1,IPOIX2) = IPOS1-1
          IPOPOS(2,IPOIX2) = IPOS1
          RETURN
        ENDIF
      ENDIF
C*************************
C
 100  CONTINUE
      IF(ISWMDL(12).EQ.0) THEN
C  sample colors
        CALL SELCOL(0,0,ICA1,ICA2,ICB1,ICB2,1)
        CALL SELCOL(0,0,ICC1,ICC2,ICD1,ICD2,1)
C  valence quark labels
        IF(IND1.EQ.1) THEN
          ICA2 = 1
          ICD2 = 1
        ENDIF
        IF(IND2.EQ.1) THEN
          ICB2 = 1
          ICC2 = 1
        ENDIF
C  color connection
        IF(((ABS(IFLA1).GT.6).AND.(IFLA1.GT.0))
     &    .OR.((ABS(IFLA1).LE.6).AND.(IFLA1.LT.0)))
     &    CALL SWAPI(ICA1,ICD1)
        IF(((ABS(IFLB1).GT.6).AND.(IFLB1.LT.0))
     &    .OR.((ABS(IFLB1).LE.6).AND.(IFLB1.GT.0)))
     &    CALL SWAPI(ICB1,ICC1)
        ISWAP = 0
        IF(ICA1*ICB1.GT.0) THEN
          IF((IND1.NE.1).AND.(IND2.NE.1)) THEN
            IF(DRNDM(CMASS1).GT.OHALF) THEN
              CALL SWAPI(IFLA1,IFLA2)
              CALL SWAPI(ICA1,ICD1)
            ELSE
              CALL SWAPI(IFLB1,IFLB2)
              CALL SWAPI(ICB1,ICC1)
            ENDIF
          ELSE IF(IND1.NE.1) THEN
            CALL SWAPI(IFLA1,IFLA2)
            CALL SWAPI(ICA1,ICD1)
          ELSE IF(IND2.NE.1) THEN
            CALL SWAPI(IFLB1,IFLB2)
            CALL SWAPI(ICB1,ICC1)
          ELSE IF((IFLA1.EQ.-IFLA2).AND.(IFLB1.EQ.-IFLB2)) THEN
            IF(DRNDM(CMASS1).GT.OHALF) THEN
              CALL SWAPI(IFLA1,IFLA2)
              CALL SWAPI(ICA1,ICD1)
            ELSE
              CALL SWAPI(IFLB1,IFLB2)
              CALL SWAPI(ICB1,ICC1)
            ENDIF
          ELSE IF(IFLA1.EQ.-IFLA2) THEN
            CALL SWAPI(IFLA1,IFLA2)
            CALL SWAPI(ICA1,ICD1)
          ELSE IF(IFLB1.EQ.-IFLB2) THEN
            CALL SWAPI(IFLB1,IFLB2)
            CALL SWAPI(ICB1,ICC1)
          ELSE
            ISWAP = 1
*           WRITE(6,'(1X,A)') 'POSPOM:WARNING:CHAIN SWAP'
*           WRITE(6,'(5X,A,4I7)') 'FLAVORS',IFLA1,IFLA2,IFLB1,IFLB2
*           WRITE(6,'(5X,A,4I7)') 'COLORS ',ICA1,ICD1,ICB1,ICC1
*           CALL POPREV(-1)
          ENDIF
        ENDIF
C  registration
        CALL REGPAR(-1,IFLA1,0,JM1,JM2,PSOFT1(1,IND1),PSOFT1(2,IND1),
     &    PSOFT1(3,IND1),PSOFT1(4,IND1),I,0,ICA1,ICA2,IPOS1,1)
        IND1 = IND1+1
        CALL REGPAR(-1,IFLA2,0,JM1,JM2,PSOFT1(1,IND1),PSOFT1(2,IND1),
     &      PSOFT1(3,IND1),PSOFT1(4,IND1),I,0,ICD1,ICD2,IPOS,1)
        IND1 = IND1+1
        CALL REGPAR(-1,IFLB1,0,JM2,JM1,PSOFT2(1,IND2),PSOFT2(2,IND2),
     &      PSOFT2(3,IND2),PSOFT2(4,IND2),I,0,ICB1,ICB2,IPOS,1)
        IND2 = IND2+1
        CALL REGPAR(-1,IFLB2,0,JM2,JM1,PSOFT2(1,IND2),PSOFT2(2,IND2),
     &      PSOFT2(3,IND2),PSOFT2(4,IND2),I,0,ICC1,ICC2,IPOS2,1)
        IND2 = IND2+1
C  soft pt assignment
        IF(ISWMDL(18).EQ.0) THEN
          CALL PARTPT(0,IPOS1,IPOS2,PTCUT(IP),IREJ)
          IF(IREJ.NE.0) RETURN
        ENDIF
      ELSE
*       CALL POBFKL(P1,P2,IPART,IREJ)
*       IF(IREJ.NE.0) RETURN
      ENDIF
      END
C
C
      SUBROUTINE HADSP2(IFLB,XS1,XMAX,XSOFT1,IREJ)
C***********************************************************************
C
C     split hadron momentum XMAX into two partons using
C     lower cut-off: AS
C
C     input:   IFLB    compressed particle code of particle to split
C              XS1     sum of x values already selected
C              XMAX    maximal x possible
C
C     output:  XS1     new sum of x values (without first one)
C              XSOFT1  field of selected x values
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-8)
C
      DIMENSION XSOFT1(50)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     *,K1(187),K2(187)
C  model exponents
      DATA PVMES1 /-0.5D0/
      DATA PVMES2 /-0.5D0/
      DATA PVBAR1 / 1.5D0/
      DATA PVBAR2 /-0.5D0/
C
      IREJ = 0
      ITMAX = 100
C
C  mesonic particle
      IF(IBAR(IFLB).EQ.0) THEN
        XPOT1 = PVMES1+ONE
        XPOT2 = PVMES2+ONE
C  baryonic particle
      ELSE
        XPOT1 = PVBAR1+ONE
        XPOT2 = PVBAR2+ONE
      ENDIF
      ITER = 0
      XREST= ONE-XS1
C  selection loop
 100  CONTINUE
        ITER = ITER+1
        IF(ITER.GE.ITMAX) THEN
          IF(IDEB(39).GE.3) THEN
            WRITE(6,'(1X,A,I8)') 'HADSP2:WARNING:REJECTION (ITER)',ITER
            WRITE(6,'(5X,A,3E12.3)') 'XS1,XMAX,AS:',XS1,XMAX,AS
          ENDIF
          IFAIL(14) = IFAIL(14)+1
          IREJ = 1
          RETURN
        ENDIF
        ZZ = XREST*BETARN(XPOT2,XPOT1)
      IF((ZZ.GT.XMAX).OR.(ZZ.LT.AS)) GOTO 100
      XSS1 = XS1 + ZZ
      IF((ONE-XSS1).LT.AS) GOTO 100
C
      XS1 = XSS1
      XSOFT1(1) = ONE-XSS1
      XSOFT1(2) = ZZ
C  debug output
      IF(IDEB(39).GE.10) THEN
        WRITE(6,'(1X,A,2I8)') 'HADSP2:DEBUG:(ITMAX,ITER)',ITMAX,ITER
        WRITE(6,'(5X,A,3E10.3,5X,2E11.4)') 'XS1,XMAX,AS  X1,X2:',
     &    XS1,XMAX,AS,XSOFT1(1),XSOFT1(2)
      ENDIF
      END
C
C
      SUBROUTINE HADSP3(IFLB,XS1,XMAX,XSOFT1,IREJ)
C***********************************************************************
C
C     split hadron momentum XMAX into diquark & quark pair
C     using lower cut-off: AS
C
C     input:   IFLB    compressed particle code of particle to split
C              XS1     sum of x values already selected
C              XMAX    maximal x possible
C
C     output:  XS1     new sum of x values
C              XSOFT1  field of selected x values
C
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-8)
C
      DIMENSION XSOFT1(50),XSOFT2(50)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),IBAR(187)
     *,K1(187),K2(187)
C
      DIMENSION XPOT1(3),XPOT2(3)
C
C  model exponents
      DATA PVMES1 /-0.5D0/
      DATA PVMES2 /-0.5D0/
      DATA PSMES  /-0.99D0/
      DATA PVBAR1 / 1.5D0/
      DATA PVBAR2 /-0.5D0/
      DATA PSBAR  /-0.99D0/
C
      IREJ = 0
C
C  determine exponents
C  particle 1
C
C  mesonic particle
      IF(IBAR(IFLB).EQ.0) THEN
        XPOT1(1) = PVMES1
        XPOT1(2) = PVMES2
        XPOT1(3) = PSMES
C  baryonic particle
      ELSE
        XPOT1(1) = PVBAR1
        XPOT1(2) = PVBAR2
        XPOT1(3) = PSBAR
      ENDIF
C  particle 2
C  mesonic particle
      XPOT2(1) = PVMES1
      XPOT2(2) = PVMES2
      XPOT2(3) = PSMES
C
      XDUM1 = 0.01D0
      XDUM2 = 0.99D0
*     CALL SELSXR(3,XPOT1,XPOT2,XS1,XDUM1,XMAX,XDUM2,
*    &            XSOFT1,XSOFT2,IREJ)
      CALL SELSXS(3,3,XPOT1,XPOT2,XS1,XDUM1,XMAX,XDUM2,
     &            XSOFT1,XSOFT2,IREJ)
C  rejection?
      IF(IREJ.NE.0) THEN
        IF(IDEB(74).GE.3) WRITE(6,'(1X,A,I6,2E12.4)')
     &    'HADSP3:WARNING:REJECTION (IFLB,XS1,XMAX)',IFLB,XS1,XMAX
        IFAIL(15) = IFAIL(15)+1
        IREJ = 1
        RETURN
      ENDIF
C  debug output
      IF(IDEB(74).GE.10) THEN
        WRITE(6,'(1X,A,I6,2E12.4)')
     &    'HADSP3:DEBUG:IFLB,XS1,XMAX',IFLB,XS1,XMAX
        DO 100 I=1,3
          WRITE(6,'(10X,I4,2E12.4)') I,XSOFT1(I),XSOFT2(I)
 100    CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE SOFTXX(JM1,JM2,MSPAR1,MSPAR2,IVAL1,IVAL2,MSM1,MSM2,
     &                  XSUM1,XSUM2,XMAX1,XMAX2,XS1,XS2,IREJ)
C***********************************************************************
C
C    select soft x values
C
C    input:   JM1,JM2    mother particle index in HEPEVS
C                        (0  flavour not known before)
C             MSPAR1,2   number of x values to select
C             IVAL1,2    number valence quarks involved (0,1,2)
C             MSM1,2     minimum number of soft x to get sampled
C             XSUM1,2    sum of all x values samples up this call
C             XMAX1,2    max. x value
C
C    output   XSUM1,2    new sum of x-values sampled
C             XS1,2      filed containing sampled x values
C
C    x values of valence partons are first given
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
      CHARACTER*8 ANAME
      COMMON/PART/ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     &            IBAR(187),K1(187),K2(187)
C
      DIMENSION XS1(50),XS2(50),XPOT1(50),XPOT2(50)
C
      IREJ = 0
      MSMAX = MAX(MSPAR1,MSPAR2)
      MSMIN = MAX(MSM1,MSM2)
C  determine exponents
      IBAR1 = ABS(IBAR(IMPART(JM1)))
      IBAR2 = ABS(IBAR(IMPART(JM2)))
C  meson-baryon scattering (asymmetric sea)
      IF((IBAR1+IBAR2).EQ.1) THEN
        PSBAR = PARMDL(53)
        PSMES = PARMDL(57)
      ELSE
        PSBAR = PARMDL(52)
        PSMES = PARMDL(56)
      ENDIF
C  particle 1
C  mesonic particle
      IF(IBAR1.EQ.0) THEN
        IF(IHFLS(1).EQ.0) THEN
          XPOT1(1) = PARMDL(62)
          XPOT1(2) = PARMDL(63)
        ELSE
          XPOT1(1) = PARMDL(54)
          XPOT1(2) = PARMDL(55)
        ENDIF
        DO 100 I=3-IVAL1,MSMAX
          XPOT1(I) = PSMES
 100    CONTINUE
C  baryonic particle
      ELSE
        IF(IHFLS(1).EQ.0) THEN
          XPOT1(1) = PARMDL(62)
          XPOT1(2) = PARMDL(63)
        ELSE
          XPOT1(1) = PARMDL(50)
          XPOT1(2) = PARMDL(51)
        ENDIF
        DO 200 I=3-IVAL1,MSMAX
          XPOT1(I) = PSBAR
 200    CONTINUE
      ENDIF
C  particle 2
C  mesonic particle
      IF(IBAR2.EQ.0) THEN
        IF(IHFLS(2).EQ.0) THEN
          XPOT2(1) = PARMDL(62)
          XPOT2(2) = PARMDL(63)
        ELSE
          XPOT2(1) = PARMDL(54)
          XPOT2(2) = PARMDL(55)
        ENDIF
        DO 300 I=3-IVAL2,MSMAX
          XPOT2(I) = PSMES
 300    CONTINUE
C  baryonic particle
      ELSE
        IF(IHFLS(2).EQ.0) THEN
          XPOT2(1) = PARMDL(62)
          XPOT2(2) = PARMDL(63)
        ELSE
          XPOT2(1) = PARMDL(50)
          XPOT2(2) = PARMDL(51)
        ENDIF
        DO 400 I=3-IVAL2,MSMAX
          XPOT2(I) = PSBAR
 400    CONTINUE
      ENDIF
C
      XSS1 = XSUM1
      XSS2 = XSUM2
      MSOFT = MSMAX
C  sample distribution
      IF(IPAMDL(14).EQ.0) THEN
        IF(MSOFT.EQ.2) THEN
          CALL SELSX2(XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ELSE IF(MSOFT.LT.5) THEN
          CALL SELSXR(MSOFT,MSMIN,XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ELSE
          CALL SELSXS(MSOFT,MSMIN,XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ENDIF
      ELSE IF(IPAMDL(14).EQ.1) THEN
        IF(MSOFT.EQ.2) THEN
          CALL SELSX2(XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ELSE
          CALL SELSXS(MSOFT,MSMIN,XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ENDIF
      ELSE IF(IPAMDL(14).EQ.2) THEN
        CALL SELSXS(MSOFT,MSMIN,XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &              XS1,XS2,IREJ)
      ELSE IF(IPAMDL(14).EQ.3) THEN
        IF(MSOFT.EQ.2) THEN
          CALL SELSX2(XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ELSE IF(IVAL1+IVAL2.EQ.4) THEN
          CALL SELSXI(MSOFT,MSMIN,XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ELSE
          CALL SELSXS(MSOFT,MSMIN,XPOT1,XPOT2,XSS1,XSS2,XMAX1,XMAX2,
     &                XS1,XS2,IREJ)
        ENDIF
      ELSE IF(IPAMDL(14).EQ.2) THEN
      ELSE
        WRITE(6,'(/,1X,A,I3)')
     &    'SOFTXX:ERROR:UNSUPPORTED IPAMDL(14)',IPAMDL(14)
        STOP
      ENDIF
      IF(IREJ.NE.0) THEN
        IFAIL(38) = IFAIL(38)+1
        IF(IDEB(48).GE.2) WRITE(6,'(1X,A,I3)')
     &    'SOFTXX:REJECTION:IREJ',IREJ
        RETURN
      ENDIF
      IF(MSOFT.NE.MSMAX) THEN
        MSDIFF = MSMAX-MSOFT
        MSPAR1 = MSPAR1-MSDIFF
        MSPAR2 = MSPAR2-MSDIFF
      ENDIF
C  correct for different MSPAR numbers
      IF(MSOFT.NE.MSPAR1) THEN
        IF(MSPAR1.GT.1) THEN
          XDEL = ZERO
          DO 500 I=MSPAR1+1,MSOFT
            XDEL = XDEL+XS1(I)
 500      CONTINUE
          XFAC = (ONE-XSUM1)/(ONE-XDEL-XSUM1)
          DO 550 I=2,MSPAR1
            XS1(I) = XS1(I)*XFAC
 550      CONTINUE
          XSS1 = (XSS1-XDEL-XSUM1)*XFAC+XSUM1
        ELSE
          XSS1 = XSUM1
        ENDIF
      ENDIF
      IF(MSOFT.NE.MSPAR2) THEN
        IF(MSPAR2.GT.1) THEN
          XDEL = ZERO
          DO 600 I=MSPAR2+1,MSOFT
            XDEL = XDEL+XS2(I)
 600      CONTINUE
          XFAC = (ONE-XSUM2)/(ONE-XDEL-XSUM2)
          DO 650 I=2,MSPAR2
            XS2(I) = XS2(I)*XFAC
 650      CONTINUE
          XSS2 = (XSS2-XDEL-XSUM2)*XFAC+XSUM2
        ELSE
          XSS2 = XSUM2
        ENDIF
      ENDIF
C  first x entry
      XS1(1) = ONE - XSS1
      XS2(1) = ONE - XSS2
      XSUM1 = XSS1
      XSUM2 = XSS2
      END
C
C
      SUBROUTINE SELSXR(MSOFT,MSMIN,XPOT1,XPOT2,XS1,XS2,XMAX1,XMAX2,
     &                  XSOFT1,XSOFT2,IREJ)
C***********************************************************************
C
C    select x values of soft chain ends (rejection method)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION XPOT1(50),XPOT2(50),XSOFT1(50),XSOFT2(50)
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      DIMENSION XLDIF(2,50),XLMIN(2,50),REVP(2,50),POT(2,50)
C
      IF(IDEB(13).GE.10) THEN
        WRITE(6,'(1X,A)') 'SELSXR:DEBUG:'
        WRITE(6,'(5X,A,I4,5E11.3)') 'MSOFT,XS1,XS2,XMAX1,2',
     &    MSOFT,XS1,XS2,XMAX1,XMAX2
        DO 40 I=1,MSOFT
          WRITE(6,'(5X,A,I4,2E12.3)') 'EXPONENTS',I,XPOT1(I),XPOT2(I)
 40     CONTINUE
      ENDIF
C
      IREJ = 0
      XMIN  = MAX(PSOMIN/ECM*TWO,XSOMIN)
      XMIN1 = MAX(AS/XMAX1,XMIN)
      XMIN2 = MAX(AS/XMAX2,XMIN)
      IF((XMIN1.GE.XMAX1).OR.(XMIN2.GE.XMAX2)) THEN
        IF(IDEB(13).GE.1) THEN
          WRITE (6,'(1X,A,4E11.3)')
     &      'SELSXR:WARNING:XMIN>XMAX (XMIN1,2/XMAX1,2)',
     &      XMIN1,XMIN2,XMAX1,XMAX2
          CALL POPREV(-1)
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
C
      IF(MSOFT.EQ.1) THEN
        XSOFT1(2) = ZERO
        XSOFT2(2) = ZERO
        RETURN
      ENDIF
      XWMAX = MAX(XMAX1**XPOT1(1),XMIN1**XPOT1(1))
     &        *MAX(XMAX2**XPOT2(1),XMIN2**XPOT2(1))
C
 10   CONTINUE
C
      DO 50 I=2,MSOFT
        POT(1,I) = XPOT1(I)+ONE
        POT(2,I) = XPOT2(I)+ONE
        REVP(1,I) = ONE/POT(1,I)
        REVP(2,I) = ONE/POT(2,I)
        XLMIN(1,I) = XMIN1**POT(1,I)
        XLMAX = XMAX1**POT(1,I)
        XLDIF(1,I) = XLMAX-XLMIN(1,I)
        XLMIN(2,I) = XMIN2**POT(2,I)
        XLMAX = XMAX2**POT(2,I)
        XLDIF(2,I) = XLMAX-XLMIN(2,I)
 50   CONTINUE
C
      IITRY = 0
 5    CONTINUE
      IITRY = IITRY + 1
      IF(IITRY.GE.500) THEN
        IF(MSOFT-MSMIN.GE.4) THEN
          MSOFT = MSMIN
          GOTO 10
        ENDIF
        IF(IDEB(13).GT.0) THEN
          WRITE(6,'(1X,A,I4)') 'SELSXR:WARNING:REJECTION',IITRY
          WRITE(6,'(5X,A,3E12.3)') 'XMAX1,2,AS:',XMAX1,XMAX2,AS
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
      XREST1 = ONE-XS1
      XREST2 = ONE-XS2
      DO 100 I=2,MSOFT
 20     CONTINUE
        Z1 = XLDIF(1,I)*DRNDM(XS1)+XLMIN(1,I)
        Z2 = XLDIF(2,I)*DRNDM(XS2)+XLMIN(2,I)
        XSOFT1(I) = Z1**REVP(1,I)
        XSOFT2(I) = Z2**REVP(2,I)
        IF( (XSOFT1(I)*XSOFT2(I)).LT.AS ) GOTO 20
        XREST1 = XREST1-XSOFT1(I)
        IF(XREST1.LT.XMIN1) GOTO 5
        XREST2 = XREST2-XSOFT2(I)
        IF(XREST2.LT.XMIN2) GOTO 5
        IF(XREST1*XREST2.LT.AS) GOTO 5
 100  CONTINUE
      XSOFT1(1) = XREST1
      XSOFT2(1) = XREST2
      IREJ=0
*     XX = ONE
*     DO 200 I=2,MSOFT
*       XX = XX*XSOFT1(I)**XPOT1(I)*XSOFT2(I)**XPOT2(I)
*200  CONTINUE
      XX = XSOFT1(1)**XPOT1(1)*XSOFT2(1)**XPOT2(1)
      IF((XX-DRNDM(XX)*XWMAX).LT.ZERO) GOTO 5
C
      XS1 = ONE-XREST1
      XS2 = ONE-XREST2
C  debug output
      IF(IDEB(13).GE.10) THEN
        WRITE(6,'(1X,A,2E12.4)') 'XSUM 1,2:',XS1,XS2
        DO 30 I=1,MSOFT
          WRITE(6,'(5X,A,I4,2E12.3)') 'XSOFT',I,XSOFT1(I),XSOFT2(I)
 30     CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE SELSX2(XPOT1,XPOT2,XSUM1,XSUM2,XMAX1,XMAX2,
     &                  XS1,XS2,IREJ)
C***********************************************************************
C
C    select x values of soft chain ends using BETARN
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION XPOT1(50),XPOT2(50),XS1(50),XS2(50)
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      IF(IDEB(32).GE.10) THEN
        WRITE(6,'(1X,A)') 'SELSX2:DEBUG:'
        WRITE(6,'(5X,A,5E11.3)') 'AS,XSUM1,2,XMAX1,2',
     &    AS,XSUM1,XSUM2,XMAX1,XMAX2
        DO 30 I=1,2
          WRITE(6,'(5X,A,I4,2E12.3)') 'EXPONENTS',I,XPOT1(I),XPOT2(I)
 30     CONTINUE
      ENDIF
C
      XMIN  = MAX(PSOMIN/ECM*TWO,XSOMIN)
      XMIN1 = MAX(AS/XMAX2,XMIN)
      XMIN2 = MAX(AS/XMAX1,XMIN)
      IF((XMIN1.GE.XMAX1).OR.(XMIN2.GE.XMAX2)) THEN
        IF(IDEB(32).GE.1) THEN
          WRITE (6,'(1X,A,4E12.3)')
     &      'SELSX2:WARNING:XMIN>XMAX (XMIN1,2/XMAX1,2)',
     &      XMIN1,XMIN2,XMAX1,XMAX2
          CALL POPREV(-1)
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
C
      IREJ = 0
      FAC1 = ONE-XSUM1
      FAC2 = ONE-XSUM2
      FAC = FAC1*FAC2
      GAM1 = XPOT1(1)+ONE
      GAM2 = XPOT2(1)+ONE
      BET1 = XPOT1(2)+ONE
      BET2 = XPOT2(2)+ONE
C
      DO 100 I=1,100
 10     CONTINUE
        X1 = BETARN(GAM1,BET1)
        X2 = BETARN(GAM2,BET2)
        X3 = ONE - X1
        X4 = ONE - X2
        IF(X1*X2*FAC.GT.AS) THEN
          IF(X3*X4*FAC.GT.AS) THEN
            XS1(1) = X1*FAC1
            XS1(2) = X3*FAC1
            XS2(1) = X2*FAC2
            XS2(2) = X4*FAC2
            XSUM1 = XSUM1+XS1(2)
            XSUM2 = XSUM2+XS2(2)
            GOTO 300
          ENDIF
        ENDIF
 100  CONTINUE
      IREJ = 1
      IF(IDEB(32).GE.2)
     &  WRITE(6,'(1X,A,I9)') 'SELSX2:REJECTION:EVENT',KEVENT
 300  CONTINUE
C  debug output
      IF(IDEB(32).GE.10) THEN
        WRITE(6,'(1X,A,2E12.4)') 'SELSX2:DEBUG:XSUM1,2:',XSUM1,XSUM2
        DO 20 I=1,2
          WRITE(6,'(5X,A,I4,2E12.3)') '     XSOFT',I,XS1(I),XS2(I)
 20     CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE SELSXS(MSOFT,MSMIN,XPOT1,XPOT2,XS1,XS2,XMAX1,XMAX2,
     &                  XSOFT1,XSOFT2,IREJ)
C***********************************************************************
C
C    select x values of soft chain ends (rescaling method)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION XPOT1(50),XPOT2(50),XSOFT1(50),XSOFT2(50)
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      DIMENSION XLDIF(2,50),XLMIN(2,50),REVP(2,50),POT(2,50)
C
      IREJ = 0
C
      IF(IDEB(14).GE.10) THEN
        WRITE(6,'(1X,A)') 'SELSXS:DEBUG:'
        WRITE(6,'(5X,A,I4,4E12.3)') 'MSOFT,XS1,XS2,XMAX1,2',
     &    MSOFT,XS1,XS2,XMAX1,XMAX2
        DO 40 I=1,MSOFT
          WRITE(6,'(5X,A,I4,2E12.3)') 'EXPONENTS',I,XPOT1(I),XPOT2(I)
 40     CONTINUE
      ENDIF
C
 10   CONTINUE
C
      IF(MSOFT.EQ.1) THEN
        XSOFT1(1) = ONE-XS1
        XSOFT1(2) = ZERO
        XSOFT2(1) = ONE-XS2
        XSOFT2(2) = ZERO
        RETURN
      ENDIF
      XMIN = MAX(PSOMIN/ECM*TWO,XSOMIN)
      XMIN1 = MAX(AS/XMAX2,XMIN)
      XMIN2 = MAX(AS/XMAX1,XMIN)
      IF((XMIN1.GE.XMAX1).OR.(XMIN2.GE.XMAX2)) THEN
        IF(IDEB(14).GE.1) THEN
          WRITE (6,'(1X,A,4E10.3)')
     &      'SELSXS:WARNING:XMIN>XMAX (XMIN1,2/XMAX1,2)',
     &      XMIN1,XMIN2,XMAX1,XMAX2
          CALL POPREV(-1)
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
C
      DO 50 I=1,MSOFT
        POT(1,I) = XPOT1(I)+ONE
        POT(2,I) = XPOT2(I)+ONE
        REVP(1,I) = ONE/POT(1,I)
        REVP(2,I) = ONE/POT(2,I)
        XLMIN(1,I) = XMIN1**POT(1,I)
        XLMAX = XMAX1**POT(1,I)
        XLDIF(1,I) = XLMAX-XLMIN(1,I)
        XLMIN(2,I) = XMIN2**POT(2,I)
        XLMAX = XMAX2**POT(2,I)
        XLDIF(2,I) = XLMAX-XLMIN(2,I)
 50   CONTINUE
C
      IITRY = 0
 5    CONTINUE
      IITRY = IITRY + 1
      IF(IITRY.GE.200) THEN
        IF(MSOFT-MSMIN.GE.4) THEN
          MSOFT= MSMIN
          GOTO 10
        ENDIF
        IF(IDEB(14).GT.2) THEN
          WRITE(6,'(1X,A,I4)') 'SELSXS:WARNING:REJECTION',IITRY
          WRITE(6,'(1X,A,I4,5E10.3)') 'MS,XMIN1/2,XMAX1/2,AS:',
     &      MSOFT,XMIN1,XMIN2,XMAX1,XMAX2,AS
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
      XSUM1 = ZERO
      XSUM2 = ZERO
      DO 100 I=1,MSOFT
 20     CONTINUE
        Z1 = XLDIF(1,I)*DRNDM(XS1)+XLMIN(1,I)
        Z2 = XLDIF(2,I)*DRNDM(XS2)+XLMIN(2,I)
        XSOFT1(I) = Z1**REVP(1,I)
        XSOFT2(I) = Z2**REVP(2,I)
        IF( (XSOFT1(I)*XSOFT2(I)).LT.AS ) GOTO 20
        XSUM1 = XSUM1+XSOFT1(I)
        XSUM2 = XSUM2+XSOFT2(I)
 100  CONTINUE
      FAC1 = (ONE-XS1)/XSUM1
      FAC2 = (ONE-XS2)/XSUM2
      DO 200 I=1,MSOFT
        XSOFT1(I) = XSOFT1(I)*FAC1
        XSOFT2(I) = XSOFT2(I)*FAC2
        IF(XSOFT1(I)*XSOFT2(I).LT.AS) GOTO 5
 200  CONTINUE
C
      XS1 = ONE-XSOFT1(1)
      XS2 = ONE-XSOFT2(1)
C  debug output
      IF(IDEB(14).GE.10) THEN
        WRITE(6,'(1X,A,2E12.4)') 'XSUM 1,2:',XS1,XS2
        DO 30 I=1,MSOFT
          WRITE(6,'(5X,A,I4,2E12.3)') 'XSOFT',I,XSOFT1(I),XSOFT2(I)
 30     CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE SELSXI(MSOFT,MSMIN,XPOT1,XPOT2,XS1,XS2,XMAX1,XMAX2,
     &                  XSOFT1,XSOFT2,IREJ)
C***********************************************************************
C
C    select x values of soft chain ends (sea independent from valence)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION XPOT1(50),XPOT2(50),XSOFT1(50),XSOFT2(50)
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
C
      DIMENSION XLDIF(2,50),XLMIN(2,50),REVP(2,50),POT(2,50)
C
      IREJ = 0
C
      XMIN = MAX(PSOMIN/ECM*TWO,XSOMIN)
      XMIN1 = MAX(AS/XMAX2,XMIN)
      XMIN2 = MAX(AS/XMAX1,XMIN)
      IF((XMIN1.GE.XMAX1).OR.(XMIN2.GE.XMAX2)) THEN
        IF(IDEB(31).GE.1) THEN
          WRITE (6,'(1X,A,4E12.3)')
     &      'SELSXI:WARNING:XMIN>XMAX (XMIN1,2/XMAX1,2)',
     &      XMIN1,XMIN2,XMAX1,XMAX2
          CALL POPREV(-1)
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
C
 10   CONTINUE
C
      DO 50 I=1,MSOFT
        POT(1,I) = XPOT1(I)+ONE
        POT(2,I) = XPOT2(I)+ONE
        REVP(1,I) = ONE/POT(1,I)
        REVP(2,I) = ONE/POT(2,I)
        XLMIN(1,I) = XMIN1**POT(1,I)
        XLMAX = XMAX1**POT(1,I)
        XLDIF(1,I) = XLMAX-XLMIN(1,I)
        XLMIN(2,I) = XMIN2**POT(2,I)
        XLMAX = XMAX2**POT(2,I)
        XLDIF(2,I) = XLMAX-XLMIN(2,I)
 50   CONTINUE
C
C  selection of sea
      IITRY = 0
 5    CONTINUE
      IITRY = IITRY + 1
      IF(IITRY.GE.50) THEN
        IF(MSOFT-MSMIN.GE.4) THEN
          MSOFT = MSMIN
          GOTO 10
        ENDIF
        IF(IDEB(31).GT.2) THEN
          WRITE(6,'(1X,A,I4)') 'SELSXI:TOO MANY ITERATIONS:',IITRY
          WRITE(6,'(5X,A,I4,3E11.3)') 'MSOFT,XMAX1,2,AS:',
     &      MSOFT,XMAX1,XMAX2,AS
        ENDIF
        IREJ = 1
        RETURN
      ENDIF
      XSUM1 = XS1
      XSUM2 = XS2
      DO 100 I=3,MSOFT
 20     CONTINUE
        Z1 = XLDIF(1,I)*DRNDM(XS1)+XLMIN(1,I)
        Z2 = XLDIF(2,I)*DRNDM(XS2)+XLMIN(2,I)
        XSOFT1(I) = Z1**REVP(1,I)
        XSOFT2(I) = Z2**REVP(2,I)
        IF( (XSOFT1(I)*XSOFT2(I)).LT.AS ) GOTO 20
        XSUM1 = XSUM1+XSOFT1(I)
        XSUM2 = XSUM2+XSOFT2(I)
 100  CONTINUE
      IF(XSUM1.GT.XMAX1) GOTO 5
      IF(XSUM2.GT.XMAX2) GOTO 5
C
C  selection of valence
      CALL SELSX2(XPOT1,XPOT2,XSUM1,XSUM2,XMAX1,XMAX2,
     &  XSOFT1,XSOFT2,IREJ)
      IF(IREJ.NE.0) THEN
        IF(MSOFT-MSMIN.GE.4) THEN
          MSOFT = MSMIN
          GOTO 10
        ENDIF
        IF(IDEB(31).GE.1) THEN
          WRITE(6,'(1X,A)') 'SELSXI:WARNING:REJECTION BY SELSX2'
          CALL POPREV(-1)
        ENDIF
        RETURN
      ENDIF
C
      XS1 = ONE-XSOFT1(1)
      XS2 = ONE-XSOFT2(1)
C  debug output
      IF(IDEB(31).GE.10) THEN
        WRITE(6,'(1X,A,2E12.4)') 'SELSXI:DEBUG:XSUM 1,2:',XS1,XS2
        DO 30 I=1,MSOFT
          WRITE(6,'(5X,A,I4,2E12.3)') 'XSOFT',I,XSOFT1(I),XSOFT2(I)
 30     CONTINUE
      ENDIF
      END
C
C
      SUBROUTINE SOFTPT(ISOFT,PTCUT,PTMAX,XV,IV,PTSOF)
C***********************************************************************
C
C    select pt of soft chain ends
C
C    input:    ISOFT          number of soft partons
C                    -1       initialization
C                    >=0      sampling of p_t
C                    -2       output of statistics
C              PTCUT          cutoff for soft chains
C              PTMAX          maximal allowed PT
C              XV             field of x values
C              IV             0    sea quark
C                             1    valence quark
C
C    output:   COMMON XSECPT  containing parameters AAS,BETAS
C              PTSOF          filed with soft pt values
C
C    note:     ISWMDL(3/4) = 0  dNs/dP_t = P_t ASS * exp(-BETA*P_t**2)
C              ISWMDL(3/4) = 1  dNs/dP_t = P_t ASS * exp(-BETA*P_t)
C              ISWMDL(3/4) = 2  photon wave function
C              ISWMDL(3/4) = 10 no soft P_t assignment
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-40)
C
      DIMENSION PTSOF(2,100),XV(100),IV(100)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
C
      COMMON /HASCA / PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,ETAC,ETAD,X1,X2,V,U,QQPD,QQAL,
     &                W,W1,AXX,WEIGHT,MSPR,IREJSC
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
C
      CHARACTER*72 TITLE
      DIMENSION BETAB(100)
      DATA IPTSA / 0 /
C
C  selection of pt
      IF(ISOFT.GE.0) THEN
        CALLS = CALLS + ONE
C  sample according to model ISWMDL(3-6)
        IF(ISOFT.GT.1) THEN
 210      CONTINUE
          PTXS = ZERO
          PTYS = ZERO
          DO 300 I=2,ISOFT
            IMODE = ISWMDL(3)
C  valence partons
            IF(IV(I).EQ.1) THEN
              BETA = BETAS(1)
C  photon/Pomeron valence part
              IF(IPAMDL(5).EQ.1) THEN
                IF(XV(I).GE.ZERO) THEN
                  IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)) THEN
                    IMODE = ISWMDL(4)
                    BETA = BETAS(3)
                  ENDIF
                ELSE
                  IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
                    IMODE = ISWMDL(4)
                    BETA = BETAS(3)
                  ENDIF
                ENDIF
              ELSE IF(IPAMDL(5).EQ.2) THEN
                BETA = PARMDL(20)
              ELSE IF(IPAMDL(5).EQ.3) THEN
                BETA = BETAS(3)
              ENDIF
C  sea partons
            ELSE IF(IV(I).EQ.0) THEN
              BETA = BETAS(3)
C  hard scattering remnant
            ELSE
              IF(IPAMDL(6).EQ.0) THEN
                BETA = BETAS(1)
              ELSE IF(IPAMDL(6).EQ.1) THEN
                BETA = BETAS(3)
              ELSE
                BETA = PARMDL(20)
              ENDIF
            ENDIF
            BETA = MAX(BETA,0.01D0)
*     print *,'ivn,beta1,beta3,beta',IV(I),BETAS(1),BETAS(3),BETA
            CALL SELPT(XV(I),ZERO,PTCUT,PTS,BETA,IMODE)
            PTS = MIN(PTMAX,PTS)
            CALL SFECFE(SIG,COG)
            PTSOF(1,I) = COG*PTS
            PTSOF(2,I) = SIG*PTS
            PTXS = PTXS+PTSOF(1,I)
            PTYS = PTYS+PTSOF(2,I)
            BETAB(I) = BETA
 300      CONTINUE
C  balancing of momenta
          PTS = SQRT(PTXS**2+PTYS**2)
          IF(PTS.GE.PTMAX) GOTO 210
          PTSOF(1,1) = -PTXS
          PTSOF(2,1) = -PTYS
          BETAB(1) = 0.D0
C
*400      CONTINUE
C
C  single parton only
        ELSE
          IMODE = ISWMDL(3)
C  valence partons
          IF(IV(1).EQ.1) THEN
            BETA = BETAS(1)
C  photon/Pomeron valence part
            IF(IPAMDL(5).EQ.1) THEN
              IF(XV(1).GE.ZERO) THEN
                IF((IDPDG1.EQ.22).OR.(IDPDG1.EQ.45)) THEN
                  IMODE = ISWMDL(4)
                  BETA = BETAS(3)
                ENDIF
              ELSE
                IF((IDPDG2.EQ.22).OR.(IDPDG2.EQ.45)) THEN
                  IMODE = ISWMDL(4)
                  BETA = BETAS(3)
                ENDIF
              ENDIF
            ELSE IF(IPAMDL(5).EQ.2) THEN
              BETA = PARMDL(20)
            ELSE IF(IPAMDL(5).EQ.3) THEN
              BETA = BETAS(3)
            ENDIF
C  sea partons
          ELSE IF(IV(1).EQ.0) THEN
            BETA = BETAS(3)
C  hard scattering remnant
          ELSE
            IF(IPAMDL(6).EQ.1) THEN
              BETA = BETAS(3)
            ELSE
              BETA = PARMDL(20)
            ENDIF
          ENDIF
          BETA = MAX(BETA,0.01D0)
*     print *,'iv1,beta1,beta3,beta',IV(1),BETAS(1),BETAS(3),BETA
          CALL SELPT(XV(1),ZERO,PTCUT,PTS,BETA,IMODE)
          PTS = MIN(PTMAX,PTS)
          CALL SFECFE(SIG,COG)
          PTSOF(1,1) = COG*PTS
          PTSOF(2,1) = SIG*PTS
          BETAB(1) = BETA
        ENDIF
C  histogramming
        IF(IPTSA.NE.0) THEN
          DO 500 I=1,ISOFT
            PT = SQRT(PTSOF(1,I)**2+PTSOF(2,I)**2)
C            CALL FILHIS(PT,ONE,IPTSA)
 500      CONTINUE
C          CALL ADDHIS(IPTSA)
        ENDIF
C  debug output
        IF(IDEB(29).GE.10) THEN
          WRITE(6,'(1X,A,I4)') 'SOFTPT:DEBUG:ISOFT',ISOFT
          WRITE(6,'(6X,A)') 'TABLE OF  I, IV, XV, PT-X, PT-Y, BETA'
          DO 105 I=1,ISOFT
            WRITE(6,'(10X,2I3,4E12.3)') I,IV(I),XV(I),PTSOF(1,I),
     &        PTSOF(2,I),BETAB(I)
 105      CONTINUE
        ENDIF
C
C  initialization of statistics and parameters
C
      ELSE IF(ISOFT.EQ.-1) THEN
        PTSMIN = ZERO
        PTSMAX = PTCUT
C  histogram of pt distribution
C        IF((IDEB(29).GE.1).AND.(IPTSA.EQ.0))
C     &    CALL NEWHIS(ZERO,5.D0,ZERO,ZERO,30,IPTSA)
        IMODE = -100+ISWMDL(3)
        CALL SELPT(ECMP,PTSMIN,PTSMAX,PTS,BETAS(3),IMODE)
C
C  output of statistics
C
      ELSE IF(ISOFT.EQ.-2) THEN
        IF(IPTSA.NE.0) THEN
C          TITLE = 'SOFTPT:DEBUG:PRIMARY SOFT PT DISTRIBUTION '
C          CALL OUTHIS(IPTSA,1,TITLE,ONE,1)
          IPTSA = 0
        ENDIF
      ELSE
        WRITE(6,'(1X,A,I2)') 'SOFTPT:ERROR:UNSUPPORTED ISOFT ',ISOFT
        STOP
      ENDIF
      END
C
C
      SUBROUTINE SELPT(EE,PTLOW,PTHIGH,PTS,BETA,IMODE)
C***********************************************************************
C
C    select pt from different distributions
C
C    input:    EE            energy (for initialization only)
C                            otherwise x value of corresponding parton
C              PTLOW         lower pt limit
C              PTHIGH        upper pt limit
C                            (PTHIGH > 20 will cause DEXP underflows)
C
C              IMODE = 0     dNs/dP_t = P_t * ASS * exp(-BETA*P_t**2)
C              IMODE = 1     dNs/dP_t = P_t * ASS * exp(-BETA*P_t)
C              IMODE = 2     dNs/dP_t according photon wave function
C              IMODE = 10    no sampling
C
C              IMODE = -100+IMODE    initialization according to
C                                    given limitations
C
C    output:   PTS           sampled pt value
C    initialization:
C              BETA          soft pt slope in central region
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           PI2    =  6.28318530718D0,
     &           OHALF  =  0.5D0,
     &           AMIN   =  1.D-2,
     &           EPS    =  1.D-7,
     &           DEPS   =  1.D-30)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  CM system of Pomeron/Reggeon exchange
      COMMON /POMCMS/ ECMP,PCMP,PMASSP(2),PVIRTP(2),GAMBEP(4),
     &                SIDP,CODP,SIFP,COFP,NPOSP(2),
     &                IDPDG1,IDBAM1,IDPDG2,IDBAM2
C
      COMMON /AVKLMN/ AVERI,AVERK,AVERL,AVERM,AVERN
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
      DOUBLE PRECISION CONN0,CONN1
      EXTERNAL CONN0,CONN1
C
C  initialization
      IF(IMODE.LT.0) GOTO 100
C
C  sampling of pt values according to IMODE
      PX = PTHIGH
      PTS = ZERO
      IF(PX.LT.AMIN) RETURN
C
      IF((PX-PTLOW).LT.0.01) THEN
        IF(IDEB(5).GE.3) WRITE(6,'(1X,A,2E12.3,I3)')
     &    'SELPT:WARNING:PTLOW,PTHIGH,IMODE ',PTLOW,PTHIGH,IMODE
        RETURN
      ENDIF
      IF(IMODE.EQ.0) THEN
 25     CONTINUE
          XI1 = DRNDM(PX)*(ONE-EXP(-BETA*PX**2)) + EXP(-BETA*PX**2)
          PTS = SQRT(-ONE/BETA*LOG(XI1))
        IF((PTS.GT.PTHIGH).OR.(PTS.LT.PTLOW)) GOTO 25
      ELSE IF(IMODE.EQ.1) THEN
        XIMIN = EXP(-BETA*PTHIGH)
        XIDEL = ONE-XIMIN
 50     CONTINUE
          PTS = -LOG((XIDEL*DRNDM(XIDEL)+XIMIN)
     &              *(XIDEL*DRNDM(XIMIN)+XIMIN)+DEPS)/BETA
        IF(PTS.LT.XMT) GOTO 50
        PTS = SQRT(PTS**2-XMT2)
        IF((PTS.GT.PTHIGH).OR.(PTS.LT.PTLOW)) GOTO 50
      ELSE IF(IMODE.EQ.2) THEN
        IF(EE.GE.ZERO) THEN
          P2 = PVIRTP(1)
        ELSE
          P2 = PVIRTP(2)
        ENDIF
        XV = ABS(EE)
        AA = (ONE-XV)*XV*P2+PARMDL(25)
 75     CONTINUE
          PTS = SQRT(AA/(DRNDM(PX)+EPS)-AA)
        IF((PTS.GT.PTHIGH).OR.(PTS.LT.PTLOW)) GOTO 75
      ELSE IF(IMODE.NE.10) THEN
        WRITE(6,'(/1X,A,I4)') 'SELPT:ERROR:UNSUPPORTED DISTRIBUTION',
     &    IMODE
        CALL POABRT
      ENDIF
C  debug output
      IF(IDEB(5).GE.20) THEN
        WRITE(6,'(1X,A,I3,4E10.3)')
     &    'SELPT:DEBUG:MODE,BET,PTMI,PTMA,PT',
     &    IMODE,BETA,PTLOW,PTHIGH,PTS
      ENDIF
      RETURN
C
C  initialization
 100  CONTINUE
        PTSMIN = PTLOW
        PTSMAX = PTHIGH
        PTCON = PTHIGH
C  calculation of parameters
        INIT = IMODE+100
        AAS = ZERO
        IF(INIT.EQ.0) THEN
          BETAS(1) = PARMDL(23)+0.15D0*LOG(EE)*(PARMDL(24)-PARMDL(23))
          IF(SIGH/SIGS.LT.0.0001D0) THEN
            AAS = ZERO
            BETA = BETAS(1)
          ELSE
            XTOL = 1.D-4
            METHOD = 1
            MAXF = 500
            BETUP = 2.5D0
            BETLO = -2.D0
            BETA = 0.D0
            BETA = DZEROX(BETLO,BETUP,XTOL,MAXF,CONN0,METHOD)
            IF(BETA.LT.EPS) THEN
              BETA = 0.01
            ENDIF
          ENDIF
        ELSE IF(INIT.EQ.1) THEN
          XMT = PARMDL(43)
          XMT2 = XMT*XMT
          BETAS(1) = PARMDL(21)+0.15D0*LOG(EE)*(PARMDL(22)-PARMDL(21))
          IF(SIGH/SIGS.LT.0.0001D0) THEN
            AAS = ZERO
            BETA = BETAS(1)
          ELSE
            XTOL = 1.D-4
            METHOD = 1
            MAXF = 500
            BETUP = 7.D0
            BETLO = -3.D0
            BETA = 0.D0
            BETA = DZEROX(BETLO,BETUP,XTOL,MAXF,CONN1,METHOD)
            IF(BETA.LT.EPS) THEN
              BETA = 0.01
            ENDIF
          ENDIF
        ELSE IF(INIT.EQ.10) THEN
          IF(IDEB(5).GT.10)
     &      WRITE(6,'(/1X,A)') 'SELPT:WARNING:NO SOFT PT SAMPLING'
          RETURN
        ELSE
          WRITE(6,'(1X,A,I4)') 'SELPT:ERROR:UNSUPPORTED DISTRIBUTION',
     &      INIT
          CALL POABRT
        ENDIF
        BETA = MIN(BETA,BETAS(1))
C
        IF(BETA.LE.ZERO) THEN
          WRITE(6,'(1X,A,2E12.3)')
     &      'SELPT:WARNING:PARAMETER BETA,AAS NEG.',BETA,AAS
          WRITE(6,'(1X,A,4E12.3)') ' SIGS,DSIGHP,SIGH,PTCON:',
     &      SIGS,DSIGHP,SIGH,PTCON
          CALL POPREV(-1)
        ENDIF
C  output of initialization parameters
        IF(IDEB(5).GE.10) THEN
          WRITE(6,'(1X,A,I3)') 'SELPT:DEBUG:INITIALIZATION,MODEL',INIT
          WRITE(6,'(5X,A,2E15.3)') 'BETA, AAS       ',BETA,AAS
          WRITE(6,'(5X,A,3E15.3)') 'ECM,PTMIN,PTMAX ',EE,PTSMIN,PTSMAX
          WRITE(6,'(5X,A,3E15.3)') 'SIGS,DSIGHP,SIGH',SIGS,DSIGHP,SIGH
        ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION CONN0(BETA)
C***********************************************************************
C
C    auxiliary function to determine parameters of soft
C    pt distribution  dNs/dP_t = P_t * AAS * EXP(-BETA*P_t**2)
C
C    internal factors: FS  number of soft partons in soft Pomeron
C                      FH  number of soft partons in hard Pomeron
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (ZERO = 0.D0,
     &           ONE  = 1.D0,
     &           TWO  = 2.D0)
      COMMON /AVKLMN/ AVERI,AVERK,AVERL,AVERM,AVERN
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
C
      XX = BETA*PTCON**2
      AAS = DSIGHP*EXP(XX)/PTCON
      IF(ABS(XX).LT.1.D-3) THEN
        FF = TWO*PTCON*(FS*SIGS+FH*SIGH)/DSIGHP-PTCON
      ELSE
        FF = TWO*PTCON*(FS*SIGS+FH*SIGH)/DSIGHP-(EXP(XX)-ONE)/BETA
      ENDIF
      CONN0 = FF
C
*     WRITE(6,'(1X,A,3E12.3)') 'CONN0:BETA,AAS,FF',BETA,AAS,FF
*     WRITE(6,'(1X,A,3E12.3)') 'CONN0:SIGS,SIGH,DSIGH',SIGS,SIGH,DSIGHP
      END
C
C
      DOUBLE PRECISION FUNCTION CONN1(BETA)
C***********************************************************************
C
C    auxiliary function to determine parameters of soft
C    pt distribution  dNs/dP_t = P_t * AAS * EXP(-BETA*P_t)
C
C    internal factors: FS  number of soft partons in soft Pomeron
C                      FH  number of soft partons in hard Pomeron
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
      PARAMETER (ZERO =  0.D0,
     &           ONE  =  1.D0,
     &           TWO  =  2.D0)
      COMMON /AVKLMN/ AVERI,AVERK,AVERL,AVERM,AVERN
      COMMON /XSECPT/ SIGS,DSIGHP,SIGH,FS,FH,BETAS(3),AAS,PTCON
C
      XX = BETA*PTCON
      AAS = DSIGHP*EXP(XX)/PTCON
      IF(ABS(XX).LT.1.D-3) THEN
        FF = AAS/TWO*PTCON**2
      ELSE
        FF = FS*SIGS+FH*SIGH
     &       -AAS/BETA**2*(ONE-(ONE+BETA*PTCON)*EXP(-XX))
      ENDIF
      CONN1 = FF
C
*     WRITE(6,'(1X,A,3E12.3)') 'CONN1:BETA,AAS,FF',BETA,AAS,FF
*     WRITE(6,'(1X,A,3E12.3)') 'CONN1:SIGS,SIGH,DSIGH',SIGS,SIGH,DSIGHP
      END
C
C
      SUBROUTINE SELCOL(ICO1,ICO2,ICOA1,ICOA2,ICOB1,ICOB2,IMODE)
C********************************************************************
C
C    selection of color combinatorics
C
C    input:         ICO1,2   colors of incoming particle
C                   IMODE    -2  output of initialization status
C                            -1  initialization
C                                   ICINP(1) selection mode
C                                            0   QCD
C                                            1   large N_c expansion
C                                   ICINP(2) max. allowed color
C                            0   clear internal color counter
C                            1   hadron into two colored objects
C                            2   quark into quark gluon
C                            3   gluon into gluon gluon
C                            4   gluon into quark antiquark
C
C    output:        ICOA1,2  colors of first outgoing particle
C                   ICOB1,2  colors of second outgoing particle
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (OHALF = 0.5D0)
      DATA METHOD /0/, II /0/
C
      ICI1 = ICO1
      ICI2 = ICO2
      IF(METHOD.EQ.0) THEN
C
        IF(IMODE.EQ.1) THEN
          II = II+1
          IF(II.GT.MAXCOL)
     &      II = MIN(DRNDM(XI)*MAXCOL+1.00001,DBLE(MAXCOL))
          ICOA1 = II
          ICOA2 = 0
          ICOB1 = -II
          ICOB2 = 0
        ELSE IF(IMODE.EQ.2) THEN
          II = II+1
          IF(II.GT.MAXCOL)
     &      II = MIN(DRNDM(XI)*MAXCOL+1.00001,DBLE(MAXCOL))
          ICOA2 = 0
          IF(ICI1.GT.0) THEN
            ICOA1 = II
            ICOB1 = ICI1
            ICOB2 = -II
          ELSE
            ICOA1 = -II
            ICOB1 = II
            ICOB2 = ICI1
          ENDIF
        ELSE IF(IMODE.EQ.3) THEN
          II = II+1
          IF(II.GT.MAXCOL)
     &      II = MIN(DRNDM(XI)*MAXCOL+1.00001,DBLE(MAXCOL))
          IF(DRNDM(YI).GT.OHALF) THEN
            ICOA1 = ICI1
            ICOA2 = -II
            ICOB1 = II
            ICOB2 = ICI2
          ELSE
            ICOB1 = ICI1
            ICOB2 = -II
            ICOA1 = II
            ICOA2 = ICI2
          ENDIF
        ELSE IF(IMODE.EQ.4) THEN
          ICOA1 = ICI1
          ICOA2 = 0
          ICOB1 = ICI2
          ICOB2 = 0
        ELSE IF(IMODE.EQ.0) THEN
          II = 0
        ELSE IF(IMODE.EQ.-1) THEN
          METHOD = ICI1
          MAXCOL = ICI2
        ELSE IF(IMODE.EQ.-2) THEN
          WRITE(6,'(1X,A,2I5)') 'SELCOL:PARAMETERS:METHOD,MAXCOL ',
     &      METHOD,MAXCOL
        ELSE
          WRITE(6,'(1X,A,I5)') 'SELCOL:ERROR:UNSUPPORTED MODE ',
     &      IMODE
          CALL POABRT
        ENDIF
C
      ELSE
        WRITE(6,'(1X,A,I5)')
     &    'SELCOL:ERROR:UNSUPPORTED METHOD SELECTED ',METHOD
        CALL POABRT
      ENDIF
C
      II = ABS(II)
      IF(IDEB(75).GE.10) THEN
        WRITE(6,'(1X,A,I5,I12,I5)') 'SELCOL:DEBUG:IMODE,MAXCOL,II ',
     &    IMODE,MAXCOL,II
        WRITE(6,'(10X,A,2I5)') 'INPUT  COLORS ',ICI1,ICI2
        WRITE(6,'(10X,A,4I5)') 'OUTPUT COLORS ',
     &    ICOA1,ICOA2,ICOB1,ICOB2
      ENDIF
      END
C
C
      SUBROUTINE PARTRE(INDX,IOUT,IREM,IREJ)
C**********************************************************************
C
C     selection of parton remainder (quark or diquark)
C
C     input:    INDX   index of particle in COMMON HEPEVS
C               IOUT   parton already used
C
C     output:   IREM   remainder according to valence flavours
C               IREJ   0  flavour combination possible
C                      1  flavour combination impossible
C
C     all particle ID are given according to PDG conventions
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  0.1D0,
     &           DEPS   =  1.D-40)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DIMENSION IQUA(3)
C
      ID1 = IDHEP(INDX)
      ID2 = IMPART(INDX)
      IREJ = 0
C
      IF(ID2.EQ.0) THEN
        WRITE(6,'(1X,A,I6)') 'PARTRE:ERROR:NO BAMJET ID ',INDX
        CALL POABRT
      ENDIF
C  special particle with strong flavour mixing
      IQUA(1) = IPDGQU(1,ID2)
      IF(IQUA(1).EQ.0) THEN
        IF(ABS(IOUT).LE.4) THEN
          IREM = -IOUT
          GOTO 100
        ELSE
          GOTO 150
        ENDIF
      ENDIF
C  ordinary hadron
      IQUA(2) = IPDGQU(2,ID2)
      IQUA(3) = IPDGQU(3,ID2)
C  compare to flavour content
      IF(ABS(IOUT).LT.1000) THEN
C  single quark requested
        IF(IQUA(1).EQ.IOUT) THEN
          K1 = 2
          K2 = 3
        ELSE IF(IQUA(2).EQ.IOUT) THEN
          K1 = 1
          K2 = 3
        ELSE IF(IQUA(3).EQ.IOUT) THEN
          K1 = 1
          K2 = 2
        ELSE
          GOTO 150
        ENDIF
        IF(IQUA(3).EQ.0) THEN
          IREM = IQUA(K1)
        ELSE
          K3   = NDIQU(ABS(IQUA(K1)),ABS(IQUA(K2)))
          IREM = SIGN(K3,ID1)
        ENDIF
      ELSE IF(IQUA(3).NE.0) THEN
C  diquark requested
        K3   = NDIQU(ABS(IQUA(1)),ABS(IQUA(2)))
        IDQ1 = SIGN(K3,ID1)
        K3   = NDIQU(ABS(IQUA(2)),ABS(IQUA(1)))
        IDQ2 = SIGN(K3,ID1)
        K3   = NDIQU(ABS(IQUA(2)),ABS(IQUA(3)))
        IDQ3 = SIGN(K3,ID1)
        K3   = NDIQU(ABS(IQUA(3)),ABS(IQUA(2)))
        IDQ4 = SIGN(K3,ID1)
        K3   = NDIQU(ABS(IQUA(1)),ABS(IQUA(3)))
        IDQ5 = SIGN(K3,ID1)
        K3   = NDIQU(ABS(IQUA(3)),ABS(IQUA(1)))
        IDQ6 = SIGN(K3,ID1)
        IF((IDQ1.EQ.IOUT).OR.(IDQ2.EQ.IOUT)) THEN
          IREM = IQUA(3)
        ELSE IF((IDQ3.EQ.IOUT).OR.(IDQ4.EQ.IOUT)) THEN
          IREM = IQUA(1)
        ELSE IF((IDQ5.EQ.IOUT).OR.(IDQ6.EQ.IOUT)) THEN
          IREM = IQUA(2)
        ELSE
          GOTO 150
        ENDIF
      ELSE
        GOTO 150
      ENDIF
C
 100  CONTINUE
C  debug output
      IF(IDEB(72).GE.10) THEN
        WRITE(6,'(1X,A,5I6)')
     &    'PARTRE:DEBUG:INDX,ID-PDG,ID-BAM,IOUT,IREM ',
     &    INDX,ID1,ID2,IOUT,IREM
      ENDIF
      RETURN
C  rejection
 150  CONTINUE
      IREJ = 1
      IF(IDEB(72).GE.2)
     &  WRITE(6,'(1X,A,5I7)')
     &  'PARTRE:REJECTION IDPDG,Q1-3,IOUT',IDHEP(INDX),IQUA,IOUT
      END
C
C
      SUBROUTINE VALFLA(IPAR,IFL1,IFL2,XMASS)
C**********************************************************************
C
C     selection of valence falvour decomposition of particle IPAR
C
C     input:    IPAR   particle index in common HEPEVS
C                      -1   initialization
C                      -2   output of statistics
C               XMASS  mass of particle
C                      (essential for Pomeron:
C                       mass dependent flavour sampling)
C
C     output:   IFL1,IFL2
C               baryon: IFL1  diquark flavour
C               (valence flavours according to PDG conventions)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  0.1D0,
     &           DEPS   =  1.D-40)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      IF(IPAR.GT.0) THEN
        K = IPAR
C  select particle code
        ID1 = IDHEP(K)
        ID2 = IMPART(K)
        IF(IPOBA3(K,2).EQ.0) THEN
C  photon
          IF(ID1.EQ.22) THEN
C  charge dependent flavour sampling
            K = INT(DRNDM(XMASS)*6.D0+1.D0)
            IF(K.LE.4) THEN
              IFL1 = 2
              IFL2 = -2
            ELSE IF(K.EQ.5) THEN
              IFL1 = 1
              IFL2 = -1
            ELSE
              IFL1 = 3
              IFL2 = -3
            ENDIF
            IF(DRNDM(XI).LT.0.5D0) THEN
              K = IFL1
              IFL1 = IFL2
              IFL2 = K
            ENDIF
C  Pomeron
          ELSE IF(ID1.EQ.45) THEN
            IF(ISWMDL(19).EQ.0) THEN
C  SU(3) symmetric valences
              K = INT(DRNDM(XMASS)*3.D0+1.D0)
              IF(DRNDM(XI).LT.OHALF) THEN
                IFL1 = K
              ELSE
                IFL1 = -K
              ENDIF
              IFL2 = -IFL1
            ELSE IF(ISWMDL(19).EQ.1) THEN
C  mass dependent flavour sampling
              CALL SEAFLA(IPAR,IFL1,IFL2,XMASS)
            ELSE
              WRITE(6,'(/1X,A,I5)') 'VALFLA:ERROR:INVALID ISWMDL(19)',
     &          ISWMDL(19)
              CALL POABRT
            ENDIF
C  meson
          ELSE
            K = INT(2.D0*DRNDM(XMASS)+1.D0)
            K = MIN(K,2)
            IFL1 = IPDGQU(K,ID2)
            K = MOD(K,2) + 1
            IFL2 = IPDGQU(K,ID2)
            IF(IFL1.EQ.0) CALL SEAFLA(IPAR,IFL1,IFL2,XMASS)
          ENDIF
C  baryon
        ELSE
          K = INT(3.D0*DRNDM(XMASS)+1.000001D0)
          K = MIN(K,3)
          K1 = MOD(K,3)+1
          K2 = MOD(K1,3)+1
          K3 = NDIQU(ABS(IPDGQU(K1,ID2)),ABS(IPDGQU(K2,ID2)))
          IFL1 = SIGN(K3,ID1)
          IFL2 = IPDGQU(K,ID2)
        ENDIF
C  debug output
        IF(IDEB(46).GE.10) THEN
        WRITE(6,'(1X,A,I5,E12.3,2I7)') 'VALFLA:DEBUG:IPAR,MASS,FL1,2',
     &    IPAR,XMASS,IFL1,IFL2
        ENDIF
      ELSE IF(IPAR.EQ.-1) THEN
C  initialization
      ELSE IF(IPAR.EQ.-2) THEN
C  output of final statistics
      ELSE
        WRITE(6,'(1X,A,I10)') 'VALFLA:ERROR:INVALID IPAR',IPAR
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE REGFLA(JM1,JM2,IFLR1,IFLR2,IREJ)
C**********************************************************************
C
C     selection of reggeon flavours
C
C     input:    JM1,JM2      position index of mother hadrons
C
C     output:   IFLR1,IFLR2  valence flavours according to
C                            PDG conventions and JM1,JM2
C               IREJ         0  reggeon possible
C                            1  reggeon impossible
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  0.1D0,
     &           DEPS   =  1.D-40)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      IF(JM1.GT.0) THEN
        IREJ = 0
        ITER = 0
C  invariant mass
        CMASS = SQRT((PHEP(4,JM1)+PHEP(4,JM2))**2
     &              -(PHEP(1,JM1)+PHEP(1,JM2))**2
     &              -(PHEP(2,JM1)+PHEP(2,JM2))**2
     &              -(PHEP(3,JM1)+PHEP(3,JM2))**2)
 50     CONTINUE
        ITER = ITER+1
        IF(ITER.GT.10) THEN
          IREJ = 1
C  debug output
          IF(IDEB(41).GE.2) WRITE(6,'(/1X,A,2I5)')
     &      'REGFLA:WARNING:REJECTION JM1,JM2',JM1,JM2
          RETURN
        ENDIF
C  selection of particle 1
        IF(DRNDM(CMASS).GT.OHALF) THEN
          IM1 = JM1
          IM2 = JM2
        ELSE
          IM1 = JM2
          IM2 = JM1
        ENDIF
        CALL VALFLA(IM1,IFL1,IFL2,CMASS)
C  check if remainder of particle 2 exists
        CALL PARTRE(IM2,-IFL1,IREM1,IREJ1)
        CALL PARTRE(IM2,-IFL2,IREM2,IREJ2)
C  reggeon exchange possible?
        IF(IREJ1*IREJ2.NE.0) THEN
          IV1 = 1
          IV2 = 1
          IF(IDHEP(JM1).EQ.777) THEN
            IV1 = IHFLS(1)
          ELSE IF(IDHEP(JM1).EQ.888) THEN
            IV1 = IHFLS(2)
          ENDIF
          IF(IDHEP(JM2).EQ.777) THEN
            IV2 = IHFLS(1)
          ELSE IF(IDHEP(JM2).EQ.888) THEN
            IV2 = IHFLS(2)
          ENDIF
C  particle remnant
          IF(IV1.EQ.0) THEN
            IF(IV2.EQ.0) THEN
              CALL SEAFLA(JM1,IFL1,IFL2,CMASS)
              IFLR1 = IFL1
              IFLR2 = -IFL1
            ELSE
              CALL VALFLA(JM2,IFL1,IFL2,CMASS)
              IFLR1 = IFL1
              IFLR2 = IFL2
            ENDIF
          ELSE IF(IV2.EQ.0) THEN
            CALL VALFLA(JM1,IFL1,IFL2,CMASS)
            IFLR1 = IFL2
            IFLR2 = IFL1
          ELSE
C  debug output
            IF(IDEB(41).GE.15) WRITE(6,'(/1X,A,3I4)')
     &        'REGFLA:WARNING:INT.REJECTION JM1,JM2,ITRY',JM1,JM2,ITER
            GOTO 50
          ENDIF
        ELSE
C  reggeon exchange
          IF(IREJ1+IREJ2.EQ.0) THEN
            IF(DRNDM(CMASS).GT.OHALF) THEN
              IFLR1 = IFL2
              IFLR2 = IREM1
            ELSE
              IFLR1 = IFL1
              IFLR2 = IREM2
            ENDIF
          ELSE IF(IREJ1.EQ.0) THEN
            IFLR1 = IFL2
            IFLR2 = IREM1
          ELSE
            IFLR1 = IFL1
            IFLR2 = IREM2
          ENDIF
        ENDIF
C  debug output
        IF(IDEB(41).GE.10) THEN
          WRITE(6,'(1X,A,4I5,E12.4)') 'REGFLA:JM1,JM2,FLR1,FLR2,MASS',
     &      JM1,JM2,IFLR1,IFLR2,CMASS
        ENDIF
      ELSE IF(JM1.EQ.-1) THEN
C  initialization
      ELSE IF(JM1.EQ.-2) THEN
C  output of statistics
      ELSE
        WRITE(6,'(1X,A,I10)') 'REGFLA:ERROR:INVALID JM1',JM1
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE SEAFLA(IPAR,IFL1,IFL2,CHMASS)
C**********************************************************************
C
C     selection of sea flavour content of particle IPAR
C
C     input:    IPAR    particle index in common HEPEVS
C               CHMASS  available invariant chain mass
C                       positive mass --> use BAMJET method
C                       negative mass --> SU(3) symmetric sea according
C                       to values given in PARMDL(1-6)
C               IPAR    -1 initialization
C                       -2 output of statistics
C
C     output:   sea flavours according to PDG conventions
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  0.1D0,
     &           DEPS   =  1.D-40)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
C
      IF(IPAR.GT.0) THEN
        IF((ISWMDL(20).EQ.1).OR.(CHMASS.LT.ZERO)) THEN
C  constant weights for sea
          SUM = ZERO
          DO 40 K=1,NFSEA
            SUM = SUM + PARMDL(K)
 40       CONTINUE
          XI = DRNDM(SUM)*SUM
          SUM = ZERO
          DO 50 K=1,NFSEA
            SUM = SUM + PARMDL(K)
            IF(XI.LE.SUM) GOTO 55
 50       CONTINUE
 55       CONTINUE
          K = MIN(K,NFSEA)
        ELSE
C  mass dependent flavour sampling
 10       CONTINUE
            CALL FLAUX(CHMASS,K)
          IF(K.GT.NFSEA) GOTO 10
        ENDIF
        IF(DRNDM(CHMASS).GT.OHALF) K = -K
        IFL1 = K
        IFL2 = -K
        IF(IDEB(46).GE.10) THEN
          WRITE(6,'(1X,A,3I5,E12.4)') 'SEAFLA:IPAR,IFL1,IFL2,MASS',
     &      IPAR,IFL1,IFL2,CHMASS
        ENDIF
      ELSE IF(IPAR.EQ.-1) THEN
C  initialization
        NFSEA = NFS
      ELSE IF(IPAR.EQ.-2) THEN
C  output of statistics
      ELSE
        WRITE(6,'(1X,A,I10)') 'SEAFLA:ERROR:INVALID IPAR',IPAR
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE FLAUX(EQUARK,K)
C***********************************************************************
C
C    auxiliary subroutine to select flavours
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-14)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
C
      DIMENSION WGHT(9)
C
C  calculate weights for given energy
      IF(EQUARK.LT.QMASS(1)) THEN
        IF(IDEB(16).GE.5)
     &    WRITE(6,'(1X,A,E12.3)') 'FLAUX:WARNING:VERY SMALL MASS',
     &      EQUARK
        WGHT(1) = 0.5D0
        WGHT(2) = 0.5D0
        WGHT(3) = ZERO
        WGHT(4) = ZERO
        SUM = 1.D0
      ELSE
        SUM = ZERO
        DO 305 K=1,NFS
          IF(EQUARK.GT.QMASS(K)) THEN
            WGHT(K) = BETAF(EQUARK,QMASS(K),BET)
          ELSE
            WGHT(K) = ZERO
          ENDIF
          SUM = SUM + WGHT(K)
 305    CONTINUE
      ENDIF
C  sample flavours
      XI = SUM*(DRNDM(SUM)-DEPS)
      K = 0
      SUM = ZERO
 400  CONTINUE
        K = K+1
        SUM = SUM + WGHT(K)
      IF(XI.GT.SUM) GOTO 400
C  debug output
      IF(IDEB(16).GE.20) THEN
        WRITE(6,'(1X,A,I5)') 'FLAUX:DEBUG:SELECTED FLAVOUR',K
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION BETAF(X1,X2,BET)
C********************************************************************
C
C     weights of different quark flavours
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      AX=0.D0
      BETX1=BET*X1
      IF(BETX1.LT.70.D0) AX=-1.D0/BET**2*(BETX1+1.D0)*EXP(-BETX1)
      AY=1.D0/BET**2*(BET*X2+1.D0)*EXP(-BET*X2)
      BETAF=AX+AY
      RETURN
      END
C
C
      SUBROUTINE MCHECK(J1,IREJ)
C********************************************************************
C
C    check parton momenta for fragmentation
C
C    input:      J1      first  chain number
C                COMMON  HEPEVS
C                COMMON  STRING
C
C    output:     COMMON  HEPEVS
C                COMMON  STRING
C                IREJ    0  successful
C                        1  failure
C
C    in case of very small chain mass:
C                NNCH    mass label of chain
C                        0  string
C                       -1  octett baryon / pseudo scalar meson
C                        1  decuplett baryon / vector meson
C                IBHAD   hadron number according to CPC,
C                        chain will be treated as resonance
C                        (sometimes far off mass shell)
C
C    constant WIDTH ( 0.01GeV ) determines range of acceptance
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           WIDTH  =  0.01D0,
     &           DEPS   =  1.D-40)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      IREJ = 0
C  quark antiquark jet
      STRM = PHEP(5,NPOS(1,J1))
      IF(NCODE(J1).EQ.3) THEN
        CALL POMEMA(IPAR1(J1),IPAR2(J1),AMPS,AMPS2,AMVE,AMVE2,IPS,IVE)
        IF(IDEB(18).GE.5)
     &    WRITE(6,'(1X,A,/3X,I3,5E12.3)')
     &            'MCHECK:1.CHAIN NO, CHMASS,AMPS,AMPS2,AMVE,AMVE2 ',
     &            J1,STRM,AMPS,AMPS2,AMVE,AMVE2
        IF(STRM.LT.AMPS) THEN
          IREJ = 1
          IFAIL(20) = IFAIL(20) + 1
          RETURN
        ELSE IF(STRM.LT.AMPS2) THEN
          IF(STRM.LT.(AMVE-WIDTH)) THEN
            NNCH(J1) = -1
            IBHAD(J1) = IPS
          ELSE
            NNCH(J1) = 1
            IBHAD(J1) = IVE
          ENDIF
        ELSE
          NNCH(J1) = 0
          IBHAD(J1) = 0
        ENDIF
C  quark diquark or v.s. jet
      ELSE IF((NCODE(J1).EQ.4).OR.(NCODE(J1).EQ.6)) THEN
        CALL POBAMA(IPAR1(J1),IPAR2(J1),IPAR3(J1),
     &              AM8,AM82,AM10,AM102,I8,I10)
        IF(IDEB(18).GE.5)
     &    WRITE(6,'(1X,A,/5X,I3,5E12.3)')
     &            'MCHECK:1.CHAIN NO, CHMASS,AM8,AM82,AM10,AM102 ',
     &            J1,STRM,AM8,AM82,AM10,AM102
        IF(STRM.LT.AM8) THEN
          IREJ = 1
          IFAIL(19) = IFAIL(19) + 1
          RETURN
        ELSE IF(STRM.LT.AM82) THEN
          IF(STRM.LT.(AM10-WIDTH)) THEN
            NNCH(J1) = -1
            IBHAD(J1) = I8
          ELSE
            NNCH(J1) = 1
            IBHAD(J1) = I10
          ENDIF
        ELSE
          NNCH(J1) = 0
          IBHAD(J1) = 0
        ENDIF
C  diquark a-diquark chain
      ELSE IF(NCODE(J1).EQ.5) THEN
        CALL PODIMA(IPAR1(J1),IPAR2(J1),IPAR3(J1),IPAR4(J1),
     &              AM8,AM8A,AM8B,AM10,I8A,I8B)
        IF(IDEB(18).GE.5)
     &    WRITE(6,'(1X,A,/5X,I3,5E12.3)')
     &            'MCHECK:1.CHAIN NO, CHMASS,AM8,AM8A,AM8B,AM10 ',
     &            J1,STRM,AM8,AM8A,AM8B,AM10
        IF(STRM.LT.AM8) THEN
          IREJ = 1
          IFAIL(19) = IFAIL(19) + 1
          RETURN
        ELSE
          NNCH(J1) = 0
          IBHAD(J1) = 0
        ENDIF
      ELSE IF(NCODE(J1).LT.0) THEN
        RETURN
      ELSE
        WRITE(6,'(/,1X,A,2I8)') 'ERROR:MCHECK:CHAIN NO,NCODE ',
     &                          J1,NCODE(J1)
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE POMCOR(IREJ)
C********************************************************************
C
C    join quarks to gluons in case of too small masses
C
C    input:      COMMON  HEPEVS
C                COMMON  STRING
C                IREJ    -1          initialization
C                        -2          output of statistics
C
C    output:     COMMON  HEPEVS
C                COMMON  STRING
C                IREJ    0  successful
C                        1  failure
C
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  1.D-10,
     &           EMIN   =  0.3D0,
     &           DEPS   =  1.D-40)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      DIMENSION PJ(4)
C
      IF(IREJ.EQ.-1) THEN
        ICTOT = 0
        ICCOR = 0
        RETURN
      ELSE IF(IREJ.EQ.-2) THEN
        WRITE(6,'(/1X,A,2I8/)')
     &    'POMCOR:DEBUG:TOTAL/JOINED CHAINS',ICTOT,ICCOR
        RETURN
      ENDIF
C
      IREJ = 0
C
      NITER = 100
      ITER = 0
      ICTOT = ICTOT+ISTR
      IF(ISWMDL(17).LE.0) RETURN
C  debug chain common
      IF(IDEB(83).GE.25) CALL POPRST
C
 50   CONTINUE
      ITER = ITER+1
      IF(ITER.GE.NITER) THEN
        IREJ = 1
        IF(IDEB(83).GE.2) THEN
          WRITE(6,'(1X,A,2I5)') 'POMCOR:WARNING:REJECTION',ITER,NITER
          IF(IDEB(83).GE.10) CALL POPREV(0)
        ENDIF
        RETURN
      ENDIF
C
C  check mass limits
      ISTRO = ISTR
      DO 100 I=1,ISTRO
        IF(NCODE(I).LT.0) GOTO 99
        J1 = NPOS(1,I)
        NRPOM = IPHIST(2,J1)
        IF(NRPOM.GE.100) GOTO 99
        CMASS0 = PHEP(5,J1)
C  get masses
        IF(NCODE(I).EQ.3) THEN
          CALL POMEMA(IPAR1(I),IPAR2(I),AM1,AM2,AM3,AM4,IP1,IP2)
        ELSE IF((NCODE(I).EQ.4).OR.(NCODE(I).EQ.6)) THEN
          CALL POBAMA(IPAR1(I),IPAR2(I),IPAR3(I),
     &                AM1,AM2,AM3,AM4,IP1,IP2)
        ELSE IF(NCODE(I).EQ.5) THEN
          CALL PODIMA(IPAR1(I),IPAR2(I),IPAR3(I),IPAR4(I),
     &                AM1,AM2,AM3,AM4,IP1,IP2)
        ELSE IF(NCODE(I).LT.0) THEN
          GOTO 99
        ELSE
          WRITE(6,'(/,1X,A,2I5)') 'ERROR:POMCOR:CHAIN NO,NCODE ',
     &                            J1,NCODE(I)
          CALL POABRT
        ENDIF
        IF(IDEB(83).GE.5)
     &    WRITE(6,'(1X,A,/3X,2I4,5E11.3,2I5)')
     &      'POMCOR:DEBUG:CHAIN,POM,CHMASS,AM1,AM2,AM3,AM4,IP1,IP2 ',
     &      I,NRPOM,CMASS0,AM1,AM2,AM3,AM4,IP1,IP2
C  select masses to correct
        IF(CMASS0.LT.MAX(AM2,AM4)) THEN
          DO 200 K=1,ISTRO
            IF((K.NE.I).AND.(NCODE(K).GE.0)) THEN
              J2 = NPOS(1,K)
C  join quarks to gluon
              IF(NRPOM.EQ.IPHIST(2,J2)) THEN
C  flavour check
                IFL1 = 0
                IFL2 = 0
                PROB1 = ZERO
                PROB2 = ZERO
                KK1 = NPOS(2,I)
                KK2 = NPOS(2,K)
                IF(IDHEP(KK1)+IDHEP(KK2).EQ.0) THEN
                  CMASS = (PHEP(4,KK1)+PHEP(4,KK2))**2
     &                   -(PHEP(1,KK1)+PHEP(1,KK2))**2
     &                   -(PHEP(2,KK1)+PHEP(2,KK2))**2
     &                   -(PHEP(2,KK1)+PHEP(2,KK2))**2
                  IFL1 = ABS(IDHEP(KK1))
                  IF(IFL1.GT.2) THEN
                    PROB1 = 0.1D0/MAX(CMASS,EPS)
                  ELSE
                    PROB1 = 0.9D0/MAX(CMASS,EPS)
                  ENDIF
                ENDIF
                KK1 = ABS(NPOS(3,I))
                KK2 = ABS(NPOS(3,K))
                IF(IDHEP(KK1)+IDHEP(KK2).EQ.0) THEN
                  CMASS = (PHEP(4,KK1)+PHEP(4,KK2))**2
     &                   -(PHEP(1,KK1)+PHEP(1,KK2))**2
     &                   -(PHEP(2,KK1)+PHEP(2,KK2))**2
     &                   -(PHEP(2,KK1)+PHEP(2,KK2))**2
                  IFL2 = ABS(IDHEP(KK1))
                  IF(IFL2.GT.2) THEN
                    PROB2 = 0.1D0/MAX(CMASS,EPS)
                  ELSE
                    PROB2 = 0.9D0/MAX(CMASS,EPS)
                  ENDIF
                ENDIF
                IF(IFL1+IFL2.EQ.0) GOTO 99
C  fusion possible
                ICCOR = ICCOR+1
                IF((DRNDM(CMASS)*(PROB1+PROB2)).LT.PROB1) THEN
                  JJ = 2
                  JE = 3
                ELSE
                  JJ = 3
                  JE = 2
                ENDIF
                KK1 = ABS(NPOS(JJ,I))
                KK2 = ABS(NPOS(JJ,K))
                I1 = ABS(NPOS(JE,I))
                I2 = KK1
                IS = SIGN(1,I2-I1)
                I2 = I2 - IS
                K1 = KK2
                K2 = ABS(NPOS(JE,K))
                KS = SIGN(1,K2-K1)
                K1 = K1 + KS
                IP1 = NHEP+1
C  copy mother partons of chain I
                DO 300 II=I1,I2,IS
                  CALL REGPAR(-1,IDHEP(II),0,J1,J2,PHEP(1,II),
     &              PHEP(2,II),PHEP(3,II),PHEP(4,II),I,IPHIST(2,II),
     &              ICOLOR(1,II),ICOLOR(2,II),IPOS,1)
 300            CONTINUE
C  register gluon
                DO 350 II=1,4
                  PJ(II) = PHEP(II,KK1)+PHEP(II,KK2)
 350            CONTINUE
                CALL REGPAR(-1,21,0,J1,J2,PJ(1),PJ(2),PJ(3),PJ(4),
     &            I,IPHIST(2,KK2),ICOLOR(1,KK1),ICOLOR(1,KK2),IPOS,1)
C  copy mother partons of chain K
                DO 400 II=K1,K2,KS
                  CALL REGPAR(-1,IDHEP(II),0,J1,J2,PHEP(1,II),
     &              PHEP(2,II),PHEP(3,II),PHEP(4,II),I,IPHIST(2,II),
     &              ICOLOR(1,II),ICOLOR(2,II),IPOS,1)
 400            CONTINUE
C  create new chain entry
                DO 450 II=1,4
                  PJ(II) = PHEP(II,J1)+PHEP(II,J2)
 450            CONTINUE
                IP2 = IPOS
                CALL REGPAR(-1,8888,0,IP1,-IP2,PJ(1),PJ(2),PJ(3),PJ(4),
     &            I,IPHIST(2,J1),ICOLOR(1,J1)+ICOLOR(1,J2),
     &            ICOLOR(2,J1)+ICOLOR(2,J2),IPOS,1)
C  delete chain K in COMMON /STRING/
                NCODE(K) = -999
C  update chain I in COMMON /STRING/
                NPOS(1,I) = IPOS
                NPOS(2,I) = IP1
                NPOS(3,I) = -IP2
C  calculate new BAMJET chain codes
                CALL ID2BAM(IDHEP(IP1),IDHEP(IP2),NCODE(I),IPAR1(I),
     &            IPAR2(I),IPAR3(I),IPAR4(I))
                GOTO 99
              ENDIF
            ENDIF
 200      CONTINUE
        ENDIF
 99     CONTINUE
 100  CONTINUE
      IF(IDEB(83).GE.20) THEN
        WRITE(6,'(1X,A)') 'POMCOR:DEBUG:AFTER CHAIN FUSION'
        IF(IDEB(83).GE.22) THEN
          CALL POPRST
          CALL POPREV(0)
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE MASCOR(IREJ)
C********************************************************************
C
C    check and adjust parton momenta for fragmentation
C
C    input:      COMMON  HEPEVS
C                COMMON  STRING
C                IREJ    -1          initialization
C                        -2          output of statistics
C
C    output:     COMMON  HEPEVS
C                COMMON  STRING
C                IREJ    0  successful
C                        1  failure
C
C    in case of very small chain mass:
C       - direct manipulation of COMMON HEPEVS and HEPEVE
C       - chain will be deleted from COMMON STRING (label -99)
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  1.D-10,
     &           EMIN   =  0.3D0,
     &           DEPS   =  1.D-40)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      DIMENSION PC1(4),PC2(4),P1(4),PTR(4),GAM(3),GAMB(3)
C
      IF(IREJ.EQ.-1) THEN
        ICTOT = 0
        ICCOR = 0
        RETURN
      ELSE IF(IREJ.EQ.-2) THEN
        WRITE(6,'(/1X,A,2I8/)')
     &    'MASCOR:DEBUG:TOTAL/CONVERTED CHAINS',ICTOT,ICCOR
        RETURN
      ENDIF

      IREJ = 0
      NITER = 100
      ITER = 0
      ICTOT = ICTOT+ISTR
      IF(ISWMDL(7).EQ.-1) RETURN
C  debug chain common
      IF(IDEB(42).GE.25) CALL POPRST
C
      ITOUCH = 0
 50   CONTINUE
      ITER = ITER+1
      IF(ITER.GE.NITER) THEN
        IREJ = 1
        IF(IDEB(42).GE.2) THEN
          WRITE(6,'(1X,A,2I5)') 'MASCOR:WARNING:REJECTION',ITER,NITER
          IF(IDEB(42).GE.10) CALL POPREV(0)
        ENDIF
        RETURN
      ENDIF
C
C  check mass limits
      IF(DRNDM(CMASS0).LT.0.5D0) THEN
        IM1 = 1
        IM2 = ISTR
        IST = 1
      ELSE
        IM1 = ISTR
        IM2 = 1
        IST = -1
      ENDIF
      DO 100 I=IM1,IM2,IST
        J1 = NPOS(1,I)
        CMASS0 = PHEP(5,J1)
C  get masses
        IF(NCODE(I).EQ.3) THEN
          CALL POMEMA(IPAR1(I),IPAR2(I),AM1,AM2,AM3,AM4,IP1,IP2)
        ELSE IF((NCODE(I).EQ.4).OR.(NCODE(I).EQ.6)) THEN
          CALL POBAMA(IPAR1(I),IPAR2(I),IPAR3(I),
     &                AM1,AM2,AM3,AM4,IP1,IP2)
        ELSE IF(NCODE(I).EQ.5) THEN
          CALL PODIMA(IPAR1(I),IPAR2(I),IPAR3(I),IPAR4(I),
     &              AM1,AM2,AM3,AM4,IP1,IP2)
        ELSE IF(NCODE(I).LT.0) THEN
          GOTO 90
        ELSE
          WRITE(6,'(/,1X,A,2I5)') 'MASCOR:ERROR:CHAIN NO,NCODE ',
     &                            J1,NCODE(I)
          CALL POABRT
        ENDIF
        IF(IDEB(42).GE.20)
     &    WRITE(6,'(1X,A,/3X,I3,5E11.3,2I5)')
     &      'MASCOR:DEBUG:CHAIN NO, CHMASS,AM1,AM2,AM3,AM4,IP1,IP2 ',
     &      I,CMASS0,AM1,AM2,AM3,AM4,IP1,IP2
C  select masses to correct
        IBHAD(I) = 0
        NNCH(I) = 0
C  correction needed?
C  no resonances for diquark-anti-diquark chains
        IF(NCODE(I).EQ.5) THEN
          IF(CMASS0.LT.1.1D0*AM1) THEN
            IF(ISWMDL(7).LE.2) THEN
              IBHAD(I) = 8888
              NNCH(I)  = -1
              CHMASS   = AM1*1.3D0
            ELSE
              IREJ = 1
              RETURN
            ENDIF
          ENDIF
        ELSE
          INEED = 0
C  resonances possible
          IF(ISWMDL(7).EQ.0) THEN
            IF(CMASS0.LT.AM1*0.99D0) THEN
              IBHAD(I) = IP1
              NNCH(I)  = -1
              CHMASS   = AM1
              INEED = 1
            ELSE IF(CMASS0.LT.MIN(AM2,AM4)*1.2D0) THEN
              DELM1 = ONE/((CMASS0-AM1)**2+EPS)
              DELM2 = ONE/((CMASS0-AM3)**2+EPS)
              IF(DRNDM(DELM1).LT.DELM1/(DELM1+DELM2)) THEN
                IBHAD(I) = IP1
                NNCH(I)  = -1
                CHMASS   = AM1
              ELSE
                IBHAD(I) = IP2
                NNCH(I)  = 1
                CHMASS   = AM3
              ENDIF
            ENDIF
          ELSE IF((ISWMDL(7).EQ.1).OR.(ISWMDL(7).EQ.2)) THEN
            IF(CMASS0.LT.AM1*0.99) THEN
              IBHAD(I) = IP1
              NNCH(I) = -1
              CHMASS = AM1
              INEED = 1
            ENDIF
          ELSE IF(ISWMDL(7).EQ.3) THEN
            IF(CMASS0.LT.AM1) THEN
              IREJ = 1
              RETURN
            ENDIF
          ELSE
            WRITE(6,'(/1X,A,I5)')
     &        'MASCOR:ERROR:UNSUPPORTED ISWMDL(7)',ISWMDL(7)
            CALL POABRT
          ENDIF
        ENDIF
C
C  correction necessary?
        IF(IBHAD(I).NE.0) THEN
C  find largest invar. mass
          IPOS = 0
          CMASS1 = -1.D0
          DO 200 J2=NHEP,3,-1
            IF((ABS(ISTHEP(J2)).EQ.1)
     &         .AND.(NCODE(IPHIST(1,J2)).GT.0)) THEN
              CMASS2= (PHEP(4,J1)+PHEP(4,J2))**2
     &               -(PHEP(1,J1)+PHEP(1,J2))**2
     &               -(PHEP(2,J1)+PHEP(2,J2))**2
     &               -(PHEP(3,J1)+PHEP(3,J2))**2
              IF(CMASS2.GT.CMASS1) THEN
                IPOS=J2
                CMASS1=CMASS2
              ENDIF
            ENDIF
 200      CONTINUE
          J2 = IPOS
          IF((J1.EQ.J2).OR.(CMASS1.LE.EMIN)) THEN
            IF(INEED.EQ.1) THEN
              IREJ = 1
              RETURN
            ELSE
              IBHAD(I) = 0
              NNCH(I) = 0
              GOTO 90
            ENDIF
          ENDIF
          ISTA = ISTHEP(J1)
          ISTB = ISTHEP(J2)
          CMASS1 = SQRT(CMASS1)
          CMASS2 = PHEP(5,J2)
          IF(CMASS1.LT.(CMASS2+CHMASS)) CMASS2 = CMASS1-1.1D0*CHMASS
          IREJ = 1
          IF(CMASS2.GT.0.D0) CALL MSHELL(PHEP(1,J1),PHEP(1,J2),
     &      CHMASS,CMASS2,PC1,PC2,IREJ)
          IF(IREJ.NE.0) THEN
            IFAIL(24) = IFAIL(24)+1
            IF(IDEB(42).GE.2) THEN
              WRITE(6,'(1X,A,2I4)')
     &          'MASCOR:WARNING:MSHELL REJECTION:J1,J2',J1,J2
              IF(IDEB(42).GE.10) CALL POPREV(0)
            ENDIF
            IREJ = 1
            RETURN
          ENDIF
C  momentum transfer
          DO 210 II=1,4
            PTR(II) = PHEP(II,J2)-PC2(II)
 210      CONTINUE
          IF(IDEB(42).GE.10) WRITE(6,'(1X,A,/5X,2I3,4E12.3)')
     &      'MASCOR:DEBUG:J1,J2,TRANSFER',J1,J2,PTR
C  copy parents of chains
C  register partons belonging to first chain
          IF(IDHEP(J1).EQ.8888) THEN
            K1 = JMOHEP(1,J1)
            K2 = MAX(JMOHEP(1,J1),-JMOHEP(2,J1))
            ESUM = ZERO
            DO 500 II=K1,K2
              ESUM = ESUM+PHEP(4,II)
 500        CONTINUE
            IF(JMOHEP(2,J1).GT.0) ESUM = ESUM+PHEP(4,JMOHEP(2,J1))
            DO 600 II=K1,K2
              FAC = PHEP(4,II)/ESUM
              DO 650 K=1,4
                P1(K) = PHEP(K,II)+FAC*PTR(K)
 650          CONTINUE
              CALL REGPAR(-1,IDHEP(II),0,J1,J2,P1(1),P1(2),P1(3),P1(4),
     &                    IPHIST(1,II),IPHIST(2,II),ICOLOR(1,II),
     &                    ICOLOR(2,II),IPOS,1)
 600        CONTINUE
            K1A = IPOS+K1-K2
            IF(JMOHEP(2,J1).GT.0) THEN
              II = JMOHEP(2,J1)
              FAC = PHEP(4,II)/ESUM
              DO 675 K=1,4
                P1(K) = PHEP(K,II)+FAC*PTR(K)
 675          CONTINUE
              CALL REGPAR(-1,IDHEP(II),0,J1,J2,P1(1),P1(2),P1(3),P1(4),
     &                    IPHIST(1,II),IPHIST(2,II),ICOLOR(1,II),
     &                    ICOLOR(2,II),IPOS,1)
            ENDIF
            K2A = -IPOS
          ELSE
            K1A = J1
            K2A = J2
          ENDIF
C  register partons belonging to second chain
          IF(IDHEP(J2).EQ.8888) THEN
            CALL GETLTR(PHEP(1,J2),PC2,GAM,GAMB,DELE,IREJL)
            K1 = JMOHEP(1,J2)
            K2 = MAX(JMOHEP(1,J2),-JMOHEP(2,J2))
            ESUM = ZERO
            DO 300 II=K1,K2
              ESUM = ESUM+PHEP(4,II)
 300        CONTINUE
            IF(JMOHEP(2,J2).GT.0) ESUM = ESUM+PHEP(4,JMOHEP(2,J2))
            DO 400 II=K1,K2
              FAC = PHEP(4,II)/ESUM
              IF(IREJL.EQ.0) THEN
                CALL MKSLTR(PHEP(1,II),P1,GAM,GAMB)
                P1(4) = P1(4)+FAC*DELE
              ELSE
                DO 450 K=1,4
                  P1(K) = PHEP(K,II)-FAC*PTR(K)
 450            CONTINUE
              ENDIF
              CALL REGPAR(-1,IDHEP(II),0,J1,J2,P1(1),P1(2),P1(3),P1(4),
     &                    IPHIST(1,II),IPHIST(2,II),ICOLOR(1,II),
     &                    ICOLOR(2,II),IPOS,1)
 400        CONTINUE
            K1B = IPOS+K1-K2
            IF(JMOHEP(2,J2).GT.0) THEN
              II = JMOHEP(2,J2)
              FAC = PHEP(4,II)/ESUM
              IF(IREJL.EQ.0) THEN
                CALL MKSLTR(PHEP(1,II),P1,GAM,GAMB)
                P1(4) = P1(4)+FAC*DELE
              ELSE
                DO 475 K=1,4
                  P1(K) = PHEP(K,II)-FAC*PTR(K)
 475            CONTINUE
              ENDIF
              CALL REGPAR(-1,IDHEP(II),0,J1,J2,P1(1),P1(2),P1(3),P1(4),
     &                    IPHIST(1,II),IPHIST(2,II),ICOLOR(1,II),
     &                    ICOLOR(2,II),IPOS,1)
            ENDIF
            K2B = -IPOS
          ELSE
            K1B = J1
            K2B = J2
          ENDIF
C  register first chain/collapsed to hadron
          IF((ISWMDL(7).EQ.0).OR.(ISWMDL(7).EQ.1)) THEN
            IF(NCODE(I).NE.5) THEN
              CALL REGPAR(1,0,IBHAD(I),K1A,K2A,PC1(1),PC1(2),PC1(3),
     &          PC1(4),IPHIST(1,J1),IPHIST(2,J1),0,0,IPOS,1)
C  label chain as collapsed to hadron/resonance
              NCODE(I)  = -99
              IDHEP(J1) = 6666
            ELSE
              CALL REGPAR(-1,8888,0,K1A,K2A,PC1(1),PC1(2),PC1(3),
     &          PC1(4),IPHIST(1,J1),IPHIST(2,J1),0,0,IPOS,1)
              IDHEP(J1) = 7777
            ENDIF
            NPOS(1,I) = IPOS
            NPOS(2,I) = K1A
            NPOS(3,I) = K2A
          ELSE
            CALL REGPAR(ISTA,IDHEP(J1),IMPART(J1),K1A,K2A,PC1(1),
     &        PC1(2),PC1(3),PC1(4),IPHIST(1,J1),IPHIST(2,J1),
     &        ICOLOR(1,J1),ICOLOR(2,J1),IPOS,1)
            IF(IDHEP(J1).EQ.8888) THEN
              NPOS(1,IPHIST(1,J1)) = IPOS
              NPOS(2,IPHIST(1,J1)) = K1A
              NPOS(3,IPHIST(1,J1)) = K2A
C  label chain as collapsed to resonance-chain
              IDHEP(J1) = 7777
            ELSE IF((IPHIST(1,J1).GE.1).AND.(IPHIST(1,J1).LE.ISTR)) THEN
              IF(NPOS(1,IPHIST(1,J1)).EQ.J1) NPOS(1,IPHIST(1,J1))=IPOS
            ENDIF
          ENDIF
C  register second chain/hadron/parton
          CALL REGPAR(ISTB,IDHEP(J2),IMPART(J2),K1B,K2B,PC2(1),PC2(2),
     &      PC2(3),PC2(4),IPHIST(1,J2),IPHIST(2,J2),ICOLOR(1,J2),
     &      ICOLOR(2,J2),IPOS,1)
          IF(IDHEP(J2).EQ.8888) THEN
            NPOS(1,IPHIST(1,J2))=IPOS
            NPOS(2,IPHIST(1,J2))=K1B
            NPOS(3,IPHIST(1,J2))=K2B
C  label chain touched by momentum transfer
            IDHEP(J2) = 7777
          ELSE IF((IPHIST(1,J2).GE.1).AND.(IPHIST(1,J2).LE.ISTR)) THEN
            IF(NPOS(1,IPHIST(1,J2)).EQ.J2) NPOS(1,IPHIST(1,J2))=IPOS
          ENDIF
          ICCOR = ICCOR+1
          ITOUCH = ITOUCH+1
C  consistency checks
          IF(IDEB(42).GE.5) THEN
            CALL POQCHK(-1,IDEV)
            IF(IDEB(42).GE.25) CALL POPREV(0)
          ENDIF
C  jump to next iteration
          GOTO 50
        ENDIF
 90     CONTINUE
 100  CONTINUE
C  debug output
      IF(IDEB(42).GE.15) THEN
        IF((ITOUCH.GT.0).OR.(IDEB(42).GE.25)) THEN
          WRITE(6,'(1X,A,I5)') 'MASCOR:DEBUG:ITERATIONS:',ITER
          CALL POPREV(1)
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE PARCOR(MODE,IREJ)
C********************************************************************
C
C    conversion of chain partons to JETSET masses
C
C    input:      MODE    >0 position index of corresponding chain
C                        -1 initialization
C                        -2 output of statistics
C
C    output:     COMMON  STRING
C                IREJ    1 combination of chains impossible
C                        0 successful combination
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DELM   =  0.005D0,
     &           DEPS   =  1.D-40,
     &           EPS    =  1.D-5)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      DIMENSION PP1(4),PP2(4),PB1(4),PB2(4),GAM(3),GAMB(3),
     &          PL(4,20),XMP(20),XML(20)
**sr 29.04. commented to use dp-JETSET
      REAL ULMASS
**
C
      IREJ = 0
      IMODE = MODE
C
      IF(IMODE.GT.0) THEN
        ICH = 0
        I1 = JMOHEP(1,IMODE)
        I2 = ABS(JMOHEP(2,IMODE))
        IS = 1
C  copy to local field
        L = 0
        DO 100 I=I1,I2,IS
          L = L+1
          DO 200 K=1,4
            PL(K,L) = PHEP(K,I)
 200      CONTINUE
          XMP(L) = PHEP(5,I)
          XML(L) = DBLE(ULMASS(IDHEP(I)))
 100    CONTINUE
        IPAR = L
        XMC = PHEP(5,IMODE)
        IF(IDEB(82).GE.20) THEN
          WRITE(6,'(1X,A,I7,2I4)')
     &      'PARCOR:DEBUG:INI.MOMENTA,MASSES(C/L),(EV,ICH,L)',
     &      KEVENT,IMODE,L
          DO 150 I=1,L
            WRITE(6,'(1X,4E12.4,2X,2E12.4)') (PL(K,I),K=1,4),
     &       XMP(I),XML(I)
 150      CONTINUE
        ENDIF
C
C  two parton configurations
C  -----------------------------------------
        IF(IPAR.EQ.2) THEN
          XM1 = XML(1)
          XM2 = XML(2)
          IF((XM1+XM2).GE.XMC) THEN
            IF(IDEB(82).GE.6) WRITE(6,'(1X,A,/,5X,I3,3E12.4)')
     &        'PARCOR:WARNING:REJECTION I,XM1,XM2,XMC',
     &        IMODE,XM1,XM2,XMC
            GOTO 990
          ENDIF
C  conversion possible
          CALL MSHELL(PL(1,1),PL(1,2),XM1,XM2,PP1,PP2,IREJ)
          IF(IREJ.NE.0) THEN
            IFAIL(36) = IFAIL(36)+1
            IF(IDEB(82).GE.6) WRITE(6,'(1X,A,I8,I4,E12.4)')
     &      'PARCOR:WARNING:REJECTION BY MSHELL EV,CHAIN,MASS',
     &        KEVENT,IMODE,XMC
            GOTO 990
          ENDIF
          ICH = 1
          DO 115 K=1,4
            PL(K,1) = PP1(K)
            PL(K,2) = PP2(K)
            XMP(1) = XM1
            XMP(2) = XM2
 115      CONTINUE
C
C  multi parton configurations
C  ---------------------------------
        ELSE
C
C  random selection of chain side to start with
          IF(DRNDM(XMC).LT.0.5D0) THEN
            K1 = 1
            K2 = IPAR
            KS = 1
          ELSE
            K1 = IPAR
            K2 = 1
            KS = -1
          ENDIF
          ITER = 0
C
 300      CONTINUE
          IF(ITER.LT.4) THEN
            KK = K1
            K1 = K2
            K2 = KK
            KS = -1*KS
          ELSE
            GOTO 990
          ENDIF
          ITER = ITER+1
C  select method
          IF(ITER.GT.2) GOTO 230
C
C  conversion according to color flow method
          IFAI = 0
          DO 210 II=K1,K2-KS,KS
            DO 215 IK=II+KS,K2,KS
              XM1 = XML(II)
              XM2 = XML(IK)
*             IF(IDEB(82).GE.10) WRITE(6,'(1X,A,2I3,4E12.4)')
*    &          'PARCOR:I,K,XM(1-4)',II,IK,XM1,XMP(II),XM2,XMP(IK)
              IF((ABS(XM1-XMP(II)).GT.DELM)
     &           .OR.(ABS(XM2-XMP(IK)).GT.DELM)) THEN
                CALL MSHELL(PL(1,II),PL(1,IK),XM1,XM2,PP1,PP2,IREJ)
                IF(IREJ.NE.0) THEN
                  IFAIL(36) = IFAIL(36)+1
                  IF(IDEB(82).GE.6) WRITE(6,'(1X,A,I8,3I4)')
     &              'PARCOR:WARNING:INT.REJ. BY MSHELL EV,IC,I1,I2',
     &              KEVENT,IMODE,II,IK
                  IREJ = 0
                ELSE
                  ICH = ICH+1
                  DO 220 KK=1,4
                    PL(KK,II) = PP1(KK)
                    PL(KK,IK) = PP2(KK)
 220              CONTINUE
                  XMP(II) = XM1
                  XMP(IK) = XM2
                  GOTO 219
                ENDIF
              ELSE
                GOTO 219
              ENDIF
 215        CONTINUE
            IFAI = II
 219        CONTINUE
 210      CONTINUE
          IF(IFAI.NE.0) GOTO 300
          GOTO 950
C
 230      CONTINUE
C
C  conversion according to remainder method
          DO 350 I=K1,K2,KS
            XM1 = XML(I)
            IF(ABS(XM1-XMP(I)).GT.DELM) THEN
              ICH = ICH+1
              IFAI = I
C  conversion necessary
              DO 400 K=1,4
                PB1(K) = PL(K,I)
                PB2(K) = PHEP(K,IMODE)-PB1(K)
 400          CONTINUE
              XM2 = PB2(4)**2-PB2(1)**2-PB2(2)**2-PB2(3)**2
              IF(XM2.LT.ZERO) THEN
                IF(IDEB(82).GE.10) WRITE(6,'(1X,A,/,5X,3I3,4E12.4)')
     &       'PARCOR:WARNING:INT.REJ. I,IPA,ICH,XML,XMP,XM2**2,MCHAIN',
     &            I,IPAR,IMODE,XM1,XMP(I),XM2,XMC
                GOTO 300
              ENDIF
              XM2 = SQRT(XM2)
              IF((XM1+XM2).GE.XMC) THEN
                IF(IDEB(82).GE.10) WRITE(6,'(1X,A,/,5X,3I3,4E12.4)')
     &            'PARCOR:WARNING:INT.REJ. I,IPA,ICH,XML,XMP,XM2,XMC',
     &            I,IPAR,IMODE,XM1,XMP(I),XM2,XMC
                GOTO 300
              ENDIF
C  conversion possible
              CALL MSHELL(PB1,PB2,XM1,XM2,PP1,PP2,IREJ)
              IF(IREJ.NE.0) THEN
                IFAIL(36) = IFAIL(36)+1
                IF(IDEB(82).GE.6) WRITE(6,'(1X,A,I8,3I4)')
     &            'PARCOR:WARNING:MSHELL REJ. ITER,CHAIN,PARTON',
     &            ITER,IMODE,I
                GOTO 300
              ENDIF
C  calculate Lorentz transformation
              CALL GETLTR(PB2,PP2,GAM,GAMB,DELE,IREJ)
              IF(IREJ.NE.0) THEN
                IF(IDEB(82).GE.6) WRITE(6,'(1X,A,I8,3I4)')
     &            'PARCOR:WARNING:GETLTR REJ. ITER,CHAIN,PARTON',
     &            ITER,IMODE,I
                GOTO 300
              ENDIF
              IFAI = 0
C  transform remaining partons
              DO 450 L=K1,K2,KS
                IF(L.NE.I) THEN
                  CALL MKSLTR(PL(1,L),PP2,GAM,GAMB)
                  DO 500 K=1,4
                    PL(K,L) = PP2(K)
 500              CONTINUE
                ELSE
                  DO 550 K=1,4
                    PL(K,L) = PP1(K)
 550              CONTINUE
                ENDIF
 450          CONTINUE
              XMP(I) = XM1
            ENDIF
 350      CONTINUE
        ENDIF
C
C  register transformed partons
 950      CONTINUE
          IREJ = 0
          IF(ICH.NE.0) THEN
            IP1 = NHEP+1
            L = 0
            DO 700 I=I1,I2,IS
              L= L+1
              CALL REGPAR(-1,IDHEP(I),0,IMODE,0,PL(1,L),PL(2,L),PL(3,L),
     &          PL(4,L),IPHIST(1,I),IPHIST(2,I),ICOLOR(1,I),ICOLOR(2,I),
     &          IPOS,1)
 700        CONTINUE
            IP2 = IPOS
C  register chain
            CALL REGPAR(-1,8888,0,IP1,-IP2,PHEP(1,IMODE),PHEP(2,IMODE),
     &        PHEP(3,IMODE),PHEP(4,IMODE),IPHIST(1,IMODE),
     &        IPHIST(2,IMODE),ICOLOR(1,IMODE),ICOLOR(2,IMODE),IPOS,1)
C  update COMMON /STRING/
            I = IPHIST(1,IMODE)
            NPOS(1,I) = IPOS
            NPOS(2,I) = IP1
            NPOS(3,I) = -IP2
          ENDIF
C  debug output
          IF(IDEB(82).GE.20) THEN
            WRITE(6,'(1X,A,I7,2I4)')
     &        'PARCOR:DEBUG:FIN.MOMENTA,MASSES(C/L),(EV,ICH,L)',
     &        KEVENT,IMODE,L
            DO 850 I=1,L
              WRITE(6,'(1X,4E12.4,2X,2E12.4)') (PL(K,I),K=1,4),
     &         XMP(I),XML(I)
 850        CONTINUE
            WRITE(6,'(1X,A,2I5)')
     &        'PARCOR:DEBUG:CONVERSION DONE (OLD/NEW ICH)',IMODE,IPOS
          ENDIF
          RETURN
C  rejection
 990      CONTINUE
          IREJ = 1
          IF(IDEB(82).GE.3) THEN
            WRITE(6,'(/1X,A,/,5X,3I5,E12.4)')
     &        'PARCOR:WARNING:REJECTION I,IPAR,ICHAIN,MCHAIN',
     &         IFAI,IPAR,IMODE,XMC
            IF(IDEB(82).GE.5) THEN
              WRITE(6,'(1X,A,I7,2I4)')
     &          'PARCOR:DEBUG:MOMENTA,MASSES(C/L),(EV,ICH,L)',
     &          KEVENT,IMODE,IPAR
              DO 155 I=1,IPAR
                WRITE(6,'(1X,4E12.4,2X,2E12.4)') (PL(K,I),K=1,4),
     &           XMP(I),XML(I)
 155          CONTINUE
            ENDIF
          ENDIF
          RETURN
C
C
      ELSE IF(IMODE.EQ.-1) THEN
C  initialization
        RETURN
C
      ELSE IF(IMODE.EQ.-2) THEN
C  final output
        RETURN
      ENDIF
      END
C
C
      SUBROUTINE CHAINS(IMODE,IREJ)
C********************************************************************
C
C    calculation of chain combinatorics, Lorentz boosts and
C                   particle codes
C
C                - splitting of gluons
C                - chains will be built up from pairs of partons
C                  according to their color labels
C                  with IDHEP(..) = -1
C                - there can be other particles between to chain partons
C                  (these will be unchanged by chain construction)
C
C    input:      IMODE    1  complete chain processing
C                        -1 initialization
C                        -2 output of statistics
C
C    output:     COMMON  STRING
C                     50   rejection due to user cutoffs
C                IREJ    1 combination of chains impossible
C                        0 successful combination
C                       50 rejection due to user cutoffs
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-40,
     &           EPS    =  1.D-5)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      INTEGER MXSECT
      PARAMETER ( MAXPRO = 16, MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      COMMON/POMRES/IPOPOS(2,100),IPORES(100),IPOIX1,IPOIX2
C
*     CHARACTER*72 TITLE
C
      IREJ = 0
      IF(IMODE.EQ.-1) THEN
        CALL POMCOR(-1)
        CALL MASCOR(-1)
        CALL PARCOR(-1,IREJ)
C  pt distribution of partons soft
*       CALL NEWHIS(ZERO,5.D0,ZERO,ZERO,30,IPTSA)
C  pt distribution of partons hard (inside chains)
*       CALL NEWHIS(ZERO,10.D0,ZERO,ZERO,30,IPTHI)
C  pt distribution of partons hard (chain ends)
*       CALL NEWHIS(ZERO,5.D0,ZERO,ZERO,30,IPTHO)
C  pt distribution of all partons
*       CALL NEWHIS(ZERO,10.D0,ZERO,ZERO,40,IPTA)
        RETURN
      ELSE IF(IMODE.EQ.-2) THEN
        CALL POMCOR(-2)
        CALL MASCOR(-2)
        CALL PARCOR(-2,IREJ)
C  pt distribution of partons
*       TITLE = 'PRIMARY PARTON PT DISTRIBUTION (SOFT CHAINS)'
*       CALL OUTHIS(IPTSA,1,TITLE,ONE,1)
C  pt distribution of partons
*       TITLE = 'PRIMARY PARTON PT DISTRIBUTION (HARD CHAINS INSIDE)'
*       CALL OUTHIS(IPTHI,1,TITLE,ONE,1)
C  pt distribution of partons
*       TITLE = 'PRIMARY PARTON PT DISTRIBUTION (HARD CHAIN ENDS)'
*       CALL OUTHIS(IPTHO,1,TITLE,ONE,1)
C  pt distribution of all partons
*       TITLE = 'PRIMARY PARTON PT DISTRIBUTION (ALL)'
*       CALL OUTHIS(IPTA,1,TITLE,ONE,1)
        RETURN
      ENDIF
C
C  generate enhanced graphs
      IF(IPOIX2.GT.0) THEN
 200    CONTINUE
        I1 = IPOIX1
        I2 = IPOIX2
        IF(ISWMDL(14).EQ.1) IPOIX1 = 0
        KSPOMS = KSPOM
        KSREGS = KSREG
        KHPOMS = KHPOM
        KHDIRS = KHDIR
        DO 110 I=I1,I2
          KSPOM = 0
          KSREG = 0
          KHPOM = 0
          KHDIR = 0
          IF(IPORES(I).EQ.8) THEN
            KSPOM = 2
            LSPOM = 2
            LHPOM = 0
            LSREG = 0
            LHDIR = 0
            CALL STDPAR(IPOPOS(1,I),IPOPOS(2,I),LSPOM,LSREG,LHPOM,
     &        LHDIR,IREJ)
            IF(IREJ.NE.0) THEN
              IF(IDEB(4).GE.2) THEN
                WRITE(6,'(/1X,A,I5)')
     &            'CHAINS:WARNING:SEC.REJECTION BY STDPAR',IREJ
                CALL POPREV(-1)
              ENDIF
              RETURN
            ENDIF
            KSPOM = KSPOMS+LSPOM
            KSREG = KSREGS+LSREG
            KHPOM = KHPOMS+LHPOM
            KHDIR = KHDIRS+LHDIR
          ELSE IF(IPORES(I).EQ.4) THEN
            ITEMP = ISWMDL(25)
            ISWMDL(25) = 0
            CALL PODBLE(IPOPOS(1,I),IPOPOS(2,I),MSOFT,MHARD,1,IREJ)
            ISWMDL(25) = ITEMP
            IF(IREJ.NE.0) THEN
              IF(IDEB(4).GE.2) THEN
                WRITE(6,'(/1X,A,I5)')
     &            'CHAINS:WARNING:SEC.REJECTION BY PODBLE',IREJ
                CALL POPREV(-1)
              ENDIF
              RETURN
            ENDIF
            KSDPO = KSDPO+1
            KSPOM = KSPOMS+KSPOM
            KSREG = KSREGS+KSREG
            KHPOM = KHPOMS+KHPOM
            KHDIR = KHDIRS+KHDIR
          ELSE
            IDIF1 = 1
            IDIF2 = 1
            IF(IPORES(I).EQ.5) THEN
              IDIF2 = 0
              KSTRG = KSTRG+1
            ELSE IF(IPORES(I).EQ.6) THEN
              IDIF1 = 0
              KSTRG = KSTRG+1
            ELSE
              KSLOO = KSLOO+1
            ENDIF
            ITEMP = ISWMDL(16)
            ISWMDL(16) = 0
            CALL PODIFF(IDIF1,IDIF2,IPOPOS(1,I),IPOPOS(2,I),
     &        MSOFT,MHARD,0,IREJ)
            ISWMDL(16) = ITEMP
            IF(IREJ.NE.0) THEN
              IF(IDEB(4).GE.2) THEN
                WRITE(6,'(/1X,A,I5)')
     &            'CHAINS:WARNING:SEC.REJECTION BY PODIFF',IREJ
                CALL POPREV(-1)
              ENDIF
              RETURN
            ENDIF
            KSPOM = KSPOMS+KSPOM
            KSREG = KSREGS+KSREG
            KHPOM = KHPOMS+KHPOM
            KHDIR = KHDIRS+KHDIR
          ENDIF
 110    CONTINUE
        IF(IPOIX2.GT.I2) THEN
          IPOIX1 = I2+1
          GOTO 200
        ENDIF
      ENDIF
C
C  split gluons to q qbar pairs
      IF(ISWMDL(9).GT.0) THEN
        NHEPO = NHEP
        DO 30 I=3,NHEPO
          IF((ISTHEP(I).EQ.-1).AND.(IDHEP(I).EQ.21)) THEN
            ICG1=ICOLOR(1,I)
            ICG2=ICOLOR(2,I)
            IQ1 = 0
            IQ2 = 0
            DO 40 K=3,NHEPO
              IF(ICOLOR(1,K).EQ.-ICG1) THEN
                IQ1 = K
                IF(IQ1*IQ2.NE.0) GOTO 45
              ELSE IF(ICOLOR(1,K).EQ.-ICG2) THEN
                IQ2 = K
                IF(IQ1*IQ2.NE.0) GOTO 45
              ENDIF
 40         CONTINUE
            WRITE(6,'(/1X,A,3I6)')
     &        'CHAINS:ERROR:NO MATCHING COLOR FOUND (IG,ICG1,ICG2)',
     &        I,ICG1,ICG2
            CALL POABRT
 45         CONTINUE
            CALL GLU2QU(I,IQ1,IQ2,IREJ)
            IF(IREJ.NE.0) THEN
              IF(IDEB(19).GE.5) THEN
                WRITE(6,'(/,1X,A)')
     &            'CHAINS:WARNING:NO GLUON SPLITTING POSSIBLE'
                CALL POPREV(0)
              ENDIF
              RETURN
            ENDIF
          ENDIF
 30     CONTINUE
      ENDIF
C
C  construct chains and write entries sorted by chains
      ISTR = ISTR+1
      NHEPO = NHEP
      DO 50 I=3,NHEPO
        IF(ISTHEP(I).EQ.1) THEN
C  hadrons
          NPOS(1,ISTR) = I
          NPOS(2,ISTR) = 0
          NPOS(3,ISTR) = 0
          NCODE(ISTR) = -99
          IPHIST(1,I) = ISTR
          ISTR = ISTR+1
        ELSE IF((ISTHEP(I).EQ.-1).AND.(IDHEP(I).NE.21)) THEN
C  partons
          ICOL1 = -ICOLOR(1,I)
          P1 = PHEP(1,I)
          P2 = PHEP(2,I)
          P3 = PHEP(3,I)
          P4 = PHEP(4,I)
          ICH1 = IPOCH3(I,2)
          IBA1 = IPOBA3(I,2)
          CALL REGPAR(-1,IDHEP(I),IMPART(I),I,0,
     &                P1,P2,P3,P4,IPHIST(1,I),IPHIST(2,I),
     &                ICOLOR(1,I),ICOLOR(2,I),IPOS,1)
          JM1 = IPOS
C
 65       CONTINUE
          NRPOM = 0
          DO 55 K=1,NHEPO
            IF(ISTHEP(K).EQ.-1)THEN
              IF(IDHEP(K).EQ.21) THEN
                IF(ICOLOR(1,K).EQ.ICOL1) THEN
                  ICOL1 = -ICOLOR(2,K)
                  GOTO 60
                ELSE IF(ICOLOR(2,K).EQ.ICOL1) THEN
                  ICOL1 = -ICOLOR(1,K)
                  GOTO 60
                ENDIF
              ELSE IF(ICOLOR(1,K).EQ.ICOL1) THEN
                ICOL1 = 0
                GOTO 60
              ENDIF
            ENDIF
 55       CONTINUE
          WRITE(6,'(/1X,A,I5)')
     &      'CHAINS:ERROR:NO MATCHING PART. FOUND FOR COLOR',-ICOL1
          CALL POABRT
 60       CONTINUE
          P1 = P1+PHEP(1,K)
          P2 = P2+PHEP(2,K)
          P3 = P3+PHEP(3,K)
          P4 = P4+PHEP(4,K)
          NRPOM = MAX(NRPOM,IPHIST(1,K))
          ICH1 = ICH1+IPOCH3(K,2)
          IBA1 = IBA1+IPOBA3(K,2)
          CALL REGPAR(-1,IDHEP(K),IMPART(K),K,0,
     &      PHEP(1,K),PHEP(2,K),PHEP(3,K),PHEP(4,K),
     &      IPHIST(1,K),IPHIST(2,K),ICOLOR(1,K),ICOLOR(2,K),IPOS,1)
C  further parton involved?
          IF(ICOL1.NE.0) GOTO 65
          JM2 = IPOS
C  register chain
          IF(NRPOM.GT.100) NRPOM = NRPOM-MOD(NRPOM,100)
          CALL REGPAR(-1,8888,0,JM1,-JM2,P1,P2,P3,P4,
     &                ISTR,NRPOM,ICH1,IBA1,IPOS,1)
C  store additional chain information
          NPOS(1,ISTR) = IPOS
          NPOS(2,ISTR) = JM1
          NPOS(3,ISTR) = -JM2
C  calculate BAMJET chain codes
          CALL ID2BAM(IDHEP(JM1),IDHEP(JM2),NCODE(ISTR),
     &                IPAR1(ISTR),IPAR2(ISTR),IPAR3(ISTR),IPAR4(ISTR))
          ISTR = ISTR+1
        ENDIF
 50   CONTINUE
      ISTR = ISTR-1
C
C  statistics of partons before mass correction
*     DO 53 K=NHEPO+1,NHEP
*        IF(IDHEP(K).EQ.8888) GOTO 54
*        PT = SQRT(PHEP(1,K)**2+PHEP(2,K)**2)
C  pt distribution of soft partons
*        IF(ABS(IPHIST(1,K)).LT.100) CALL FILHIS(PT,ONE,IPTSA)
C  pt distribution of hard partons inside of chains
*        IF(IPHIST(1,K).LE.-100) CALL FILHIS(PT,ONE,IPTHI)
C  pt distribution of hard partons at the chain ends
*        IF(IPHIST(1,K).GE.100) CALL FILHIS(PT,ONE,IPTHO)
C  pt distribution of all partons
*        CALL FILHIS(PT,ONE,IPTA)
*54      CONTINUE
*53   CONTINUE
*     CALL ADDHIS(IPTSA)
*     CALL ADDHIS(IPTHI)
*     CALL ADDHIS(IPTHO)
*     CALL ADDHIS(IPTA)
C
      IF(IDEB(19).GE.17) THEN
        WRITE(6,'(1X,A)') 'CHAINS:DEBUG:AFTER CHAIN CONSTRUCTION'
        CALL POPREV(0)
      ENDIF
C
C  Pomeron corrections
      CALL POMCOR(IREJ)
      IF(IREJ.NE.0) THEN
        IFAIL(27) = IFAIL(27)+1
        IF(IDEB(19).GE.3) THEN
          WRITE(6,'(1X,A,I6)')
     &      'CHAINS:WARNING:POMCOR REJECTION (IREJ)',IREJ
          CALL POPREV(-1)
        ENDIF
        RETURN
      ENDIF
C
C  chain mass corrections
      CALL MASCOR(IREJ)
      IF(IREJ.NE.0) THEN
        IFAIL(34) = IFAIL(34)+1
        IF(IDEB(19).GE.3) THEN
          WRITE(6,'(1X,A,I6)')
     &      'CHAINS:WARNING:MASCOR REJECTION (IREJ)',IREJ
          CALL POPREV(-1)
        ENDIF
        RETURN
      ENDIF
C
C  parton mass corrections
      DO 100 I=1,ISTR
        IF(NCODE(I).GE.0) THEN
          CALL PARCOR(NPOS(1,I),IREJ)
          IF(IREJ.NE.0) THEN
            IFAIL(35) = IFAIL(35)+1
            IF(IDEB(19).GE.3) THEN
              WRITE(6,'(1X,A,I6)')
     &          'CHAINS:WARNING:PARCOR REJECTION (IREJ)',IREJ
              CALL POPREV(-1)
            ENDIF
            RETURN
          ENDIF
        ENDIF
 100  CONTINUE
C
C  statistics of hard processes
      DO 550 I=3,NHEP
        IF(ISTHEP(I).EQ.25) THEN
          K  = IMPART(I)
          II = IDHEP(I)
          MXSECT(4,K,II) = MXSECT(4,K,II)+1
        ENDIF
 550  CONTINUE
C  debug: write out chains
      IF(IDEB(19).GE.5) THEN
        IF(IDEB(19).GE.10)
     &    CALL POQCHK(1,IDEV)
        IF(IDEB(19).GE.15) THEN
          CALL POPREV(0)
        ELSE
          CALL POPRST
        ENDIF
      ENDIF
      END
C
C
      SUBROUTINE POFRAG(IREJ)
C********************************************************************
C
C     do all fragmentation of chains
C
C     output:  IREJ    0   successful
C                      1   rejection
C                     50   rejection due to user cutoffs
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           PI     =  3.14159265359D0,
     &           EPS    =  1.D-30,
     &           TINY   =  1.D-6)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
C  single precision interface
**sr 29.04. commented to use dp-JETSET
      REAL PLU,RQLUN
C  LUND particle data
**sr 29.04. commented to use dp-JETSET
      REAL PARU,PARJ
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
**sr 29.04. commented to use dp-JETSET
      REAL P,V
**sr 29.04.  LUJETS array-dimensions increased
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
C     COMMON/LUJETS/N,K(8000,5),P(8000,5),V(8000,5)
**
C
      DIMENSION IJOIN(100)
C
      IREJ = 0
      IF(ABS(ISWMDL(6)).GT.3) THEN
        WRITE(6,'(/1X,A,I3)') 'POFRAG:ERROR:ISWMDL(6)',ISWMDL(6)
        CALL POABRT
      ELSE
C  jetset fragmentation (involving gluons)
        IP = 0
        DO 300 I=1,ISTR
C  write final particles/resonances to jetset
          IF(NCODE(I).EQ.-99) THEN
            II = NPOS(1,I)
            IP = IP+1
            P(IP,1) = PHEP(1,II)
            P(IP,2) = PHEP(2,II)
            P(IP,3) = PHEP(3,II)
            P(IP,4) = PHEP(4,II)
            P(IP,5) = PHEP(5,II)
            K(IP,1) = 1
            K(IP,2) = IDHEP(II)
            K(IP,3) = 0
            K(IP,4) = 0
            K(IP,5) = 0
            IPHIST(2,II) = IP
C  write partons to jetset
          ELSE IF(NCODE(I).GE.0) THEN
            K1 = JMOHEP(1,NPOS(1,I))
            K2 = MAX(JMOHEP(1,NPOS(1,I)),-JMOHEP(2,NPOS(1,I)))
            IJ = 0
            DO 400 II=K1,K2
              IP = IP+1
              P(IP,1) = PHEP(1,II)
              P(IP,2) = PHEP(2,II)
              P(IP,3) = PHEP(3,II)
              P(IP,4) = PHEP(4,II)
              P(IP,5) = PHEP(5,II)
              K(IP,1) = 1
              K(IP,2) = IDHEP(II)
              K(IP,3) = 0
              K(IP,4) = 0
              K(IP,5) = 0
              IPHIST(2,II) = IP
              IJ = IJ+1
              IJOIN(IJ) = IP
              CALL QUASTA(IDHEP(II),IFPAP(1),7)
 400        CONTINUE
            II = JMOHEP(2,NPOS(1,I))
            IF((II.GT.0).AND.(II.NE.K1)) THEN
              IP = IP+1
              P(IP,1) = PHEP(1,II)
              P(IP,2) = PHEP(2,II)
              P(IP,3) = PHEP(3,II)
              P(IP,4) = PHEP(4,II)
              P(IP,5) = PHEP(5,II)
              K(IP,1) = 1
              K(IP,2) = IDHEP(II)
              K(IP,3) = 0
              K(IP,4) = 0
              K(IP,5) = 0
              IPHIST(2,II) = IP
              IJ = IJ+1
              IJOIN(IJ) = IP
              CALL QUASTA(IDHEP(II),IFPAP(1),7)
            ENDIF
            N = IP
C  connect partons to chains
            CALL LUJOIN(IJ,IJOIN)
          ENDIF
 300    CONTINUE
C  set Lund counter
        N = IP
C
        IF(IDEB(22).GE.25) THEN
          WRITE(6,'(//1X,A)')
     &      'POFRAG:DEBUG:CHAIN SYSTEM BEFORE FRAGMENTATION'
          CALL POPREV(2)
        ENDIF
C  fragment complete parton configuration
        IF(IP.GT.0) THEN
C  final state evolution
          IF((ISWMDL(8).EQ.1).OR.(ISWMDL(8).EQ.3)) THEN
            ISH = 0
            DO 125 I=3,NHEP
              IF((IPHIST(2,I).GT.0)
     &          .AND.(IPHIST(2,I).LE.IP)
     &          .AND.(IPHIST(1,I).LE.-100)) THEN
                ISH = ISH+1
                IJOIN(ISH) = I
              ENDIF
 125        CONTINUE
            IF(ISH.GE.2) THEN
              DO 130 K1=1,ISH
                IF(IJOIN(K1).EQ.0) GOTO 130
                I = IJOIN(K1)
                DO 135 K2=K1+1,ISH
                  IF(IJOIN(K2).EQ.0) GOTO 135
                  II = IJOIN(K2)
                  IF(IPHIST(1,I).EQ.IPHIST(1,II)) THEN
                    PT1 = SQRT(PHEP(1,II)**2+PHEP(2,II)**2)
                    PT2 = SQRT(PHEP(1,I)**2+PHEP(2,I)**2)
                    RQLUN = MIN(PT1,PT2)
                    IF(IDEB(22).GE.10) WRITE(6,'(1X,A,2I5,E12.4)')
     &                'POFRAG:DEBUG:LUSHOW CALLED',I,II,RQLUN
                    CALL LUSHOW(IPHIST(2,I),IPHIST(2,II),RQLUN)
                    IJOIN(K1) = 0
                    IJOIN(K2) = 0
                    GOTO 130
                  ENDIF
 135            CONTINUE
 130          CONTINUE
            ENDIF
          ENDIF
C  hadronization and decay
          IF(ISWMDL(6).NE.0) THEN
            II = MSTU(21)
            MSTU(21) = 1
            CALL LUEXEC
            MSTU(21) = II
C  event accepted?
            IF(MSTU(24).NE.0) THEN
              IF(IDEB(22).GE.2) THEN
                WRITE(6,'(1X,A,I12,I3)') 
     &            'POFRAG:WARNING:REJECTION BY JETSET (EV/IERR)',
     &            KEVENT,MSTU(24)
              ENDIF
              IREJ = 1
              RETURN
            ENDIF
          ENDIF
C
          NEVHEP = NHEP+1
C  copy hadrons back to HEPEVE (sorted by chains)
          NLINES = KLU(0,1)
          DO 155 II=1,ISTR
            IF(NCODE(II).GE.0) THEN
              K1 = IPHIST(2,NPOS(2,II))
              K2 = IPHIST(2,-NPOS(3,II))
            ELSE IF(NCODE(II).EQ.-99) THEN
              K1 = IPHIST(2,NPOS(1,II))
              K2 = K1
            ELSE
              GOTO 149
            ENDIF
            DO 160 J=1,NLINES
              IF(KLU(J,7).EQ.1) THEN
                IPMOTH = KLU(J,15)
                IF((IPMOTH.GE.K1).AND.(IPMOTH.LE.K2)) THEN
                  IBAM = MCIHAD(KLU(J,8))
                  IF(IBAM.EQ.99999) THEN
                    IF(IDEB(22).GE.2) THEN
                      WRITE(6,'(/1X,A)')
     &                  'POFRAG:WARNING:LUND ERROR:REJECTION'
C ajf commented out!    CALL POPREV(-1)
                    ENDIF
                    IREJ = 1
                    RETURN
                  ENDIF
                  PX = PLU(J,1)
                  PY = PLU(J,2)
                  PZ = PLU(J,3)
                  HE = PLU(J,4)
C  check mass, do fine corrections
                  XMB = PLU(J,5)**2
                  PABS = PX**2+PY**2+PZ**2
                  XMA = HE**2-PABS
                  PABS = SQRT(PABS)
                  DELT = 0.5D0*(XMB-XMA)/(HE+PABS)
                  HE = HE+DELT
                  DELT = (1.D0-DELT/PABS)
                  PX = PX*DELT
                  PY = PY*DELT
                  PZ = PZ*DELT
C
                  IS = 1
                  IF(IBAM.EQ.0) THEN
                    IF(ISWMDL(6).EQ.0) THEN
                      IS = -1
                    ELSE
                      IF(IDEB(22).GE.2) THEN
                        WRITE(6,'(/1X,A)')
     &                    'POFRAG:WARNING:LUND ERROR:REJECTION'
                        CALL POPREV(-1)
                      ENDIF
                      IREJ = 1
                      RETURN
                    ENDIF
                  ENDIF
                  CALL REGPAR(IS,KLU(J,8),IBAM,NPOS(1,II),0,PX,PY,PZ,
     &              HE,J,0,0,0,IPOS,1)
                  ISTHEP(IPOS) = 1
                ENDIF
              ENDIF
 160        CONTINUE
 149        CONTINUE
 155      CONTINUE
        ENDIF
      ENDIF
C  debug event status
      IF(IDEB(22).GE.15) THEN
        WRITE(6,'(//1X,A)')
     &    'POFRAG:DEBUG:PARTICLES AFTER FRAGMENTATION'
        CALL POPREV(2)
      ENDIF
      END
C
C
      SUBROUTINE POHEPI(IMODE,P1,P2,IP1,IP2)
C********************************************************************
C
C     prepare COMMON HEPEVS for new event
C
C     first subroutine called for each event
C
C     input:   P1(4)  particle 1
C              P2(4)  particle 2
C              IMODE  0    general initialization
C                     1    initialization after internal rejection
C
C     output:  IP1,IP2  index of interacting particles
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION P1(4),P2(4)
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           EPS    =  1.D-5,
     &           DEPS   =  1.D-40)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C  lepton/hadron-photon kinematics
      COMMON /PHOSRC/ PINI(5,2),PFIN(5,2),PGAM(5,2),
     &                GYY(2),GQ2(2),GGECM,GAIMP(2),PFTHE(2),PFPHI(2),
     &                IDPSRC(2),IDBSRC(2),RADSRC(2),AMSRC(2),GAMSRC(2)
C  global energy
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER (IEETAB=10)
      COMMON /XSETAB/ SIGTAB(4,70,IEETAB),SIGECM(4,IEETAB),ISIMAX
      COMMON /XSECTP/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF,
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,SIGDPO,
     &                SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGGEN(4),
     &                FSUP(2)
C
      PARAMETER (MSTR=100)
      COMMON /STRING/ GAMBET(4,MSTR),NPOS(3,MSTR),NCODE(MSTR),
     &     IPAR1(MSTR),IPAR2(MSTR),IPAR3(MSTR),IPAR4(MSTR),
     &     NNCH(MSTR),IBHAD(MSTR),ISTR
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      COMMON/POMRES/IPOPOS(2,100),IPORES(100),IPOIX1,IPOIX2
C
      DIMENSION IM(2)
C
C  counter reset
      KSPOM  = 0
      KHPOM  = 0
      KSREG  = 0
      KHDIR  = 0
      KSTRG  = 0
      KHTRG  = 0
      KSLOO  = 0
      KHLOO  = 0
      KSDPO  = 0
      KSOFT  = 0
      KHARD  = 0
C
      IDNODF = 0
      IDIFR1 = 0
      IDIFR2 = 0
      IDDPOM = 0
      ISTR   = 0
      IPOIX1 = 0
      IF(ISWMDL(14).GT.0) IPOIX1 = 1
      IPOIX2 = 0
      CALL REGPAR(0,0,0,0,0,ZERO,ZERO,ZERO,ZERO,
     &            0,0,0,0,IPOS,0)
      CALL SELCOL(0,0,0,0,0,0,0)
C
C  lepton-photon/hadron-photon vertex and initial particles
      IM(1) = 0
      IM(2) = 0
      IF((IPAMDL(11).GT.0).AND.(IDPSRC(1).NE.0)) THEN
        CALL REGPAR(1,IDPSRC(1),IDBSRC(1),0,0,PINI(1,1),PINI(2,1),
     &    PINI(3,1),PINI(4,1),0,0,0,0,IM(1),1)
      ELSE
        CALL REGPAR(1,IFPAP(1),IFPAB(1),IM(1),0,P1(1),P1(2),P1(3),
     &    P1(4),0,0,0,0,IP1,1)
      ENDIF
      IF((IPAMDL(12).GT.0).AND.(IDPSRC(2).NE.0)) THEN
        CALL REGPAR(1,IDPSRC(2),IDBSRC(2),0,0,PINI(1,2),PINI(2,2),
     &    PINI(3,2),PINI(4,2),0,0,0,0,IM(2),1)
      ELSE
        CALL REGPAR(1,IFPAP(2),IFPAB(2),IM(2),0,P2(1),P2(2),P2(3),
     &    P2(4),0,0,0,0,IP2,1)
      ENDIF
      IF((IPAMDL(11).GT.0).AND.(IDPSRC(1).NE.0)) THEN
        CALL REGPAR(1,IDPSRC(1),IDBSRC(1),IM(1),0,PFIN(1,1),
     &    PFIN(2,1),PFIN(3,1),PFIN(4,1),0,0,0,0,IPOS,1)
        CALL REGPAR(1,IFPAP(1),IFPAB(1),IM(1),0,P1(1),P1(2),P1(3),
     &    P1(4),0,0,0,0,IP1,1)
      ENDIF
      IF((IPAMDL(12).GT.0).AND.(IDPSRC(2).NE.0)) THEN
        CALL REGPAR(1,IDPSRC(2),IDBSRC(2),IM(2),0,PFIN(1,2),
     &    PFIN(2,2),PFIN(3,2),PFIN(4,2),0,0,0,0,IPOS,1)
        CALL REGPAR(1,IFPAP(2),IFPAB(2),IM(2),0,P2(1),P2(2),P2(3),
     &    P2(4),0,0,0,0,IP2,1)
      ENDIF
      PMASS(1) = PHEP(5,IP1)
      PVIRT(1) = ZERO
      IF(IFPAP(1).EQ.22) PVIRT(1) = PMASS(1)**2
      PMASS(2) = PHEP(5,IP2)
      PVIRT(2) = ZERO
      IF(IFPAP(2).EQ.22) PVIRT(2) = PMASS(2)**2
C
C  full initialization
C  CMS energy
      ECM = SQRT((P1(4)+P2(4))**2-(P1(1)+P2(1))**2-(P1(2)+P2(2))**2
     &           -(P1(3)+P2(3))**2)
      CALL PECMS(1,PMASS(1),PMASS(2),ECM,PCM,EE)
      NEVHEP = KEVENT
C
C  reset of particle record only?
      IF(IMODE.EQ.0) THEN
C  load cross sections from interpolation table
        IP = 1
        IF(ECM.LE.SIGECM(IP,1)) THEN
          I1 = 1
          I2 = 1
        ELSE IF(ECM.LT.SIGECM(IP,ISIMAX)) THEN
          DO 50 I=2,ISIMAX
            IF(ECM.LE.SIGECM(IP,I)) GOTO 200
 50       CONTINUE
 200      CONTINUE
          I1 = I-1
          I2 = I
        ELSE
          WRITE(6,'(/1X,A,2E12.3)')
     &      'POHEPI:WARNING:TOO HIGH ENERGY',ECM,SIGECM(IP,ISIMAX)
          CALL POPREV(-1)
          I1 = ISIMAX
          I2 = ISIMAX
        ENDIF
        FAC2=0.D0
        IF(I1.NE.I2) FAC2=LOG(ECM/SIGECM(IP,I1))
     &                    /LOG(SIGECM(IP,I2)/SIGECM(IP,I1))
        FAC1=1.D0-FAC2
C  cross section dependence on photon virtualities
        DO 140 K=1,2
          FSUP(K) = 1.D0
          IF((IFPAP(K).EQ.22).AND.(ISWMDL(10).EQ.1)) THEN
            FSUP(K) = 0.D0
            DO  150 I=1,3
              FSUP(K) = FSUP(K)
     &               +PARMDL(26+I)*(1.D0+PVIRT(K)/(4.D0*PARMDL(30+I)))
     &               /(1.D0+PVIRT(K)/PARMDL(30+I))**2
 150        CONTINUE
            FSUP(K) = FSUP(K)+PARMDL(30)/(1.D0+PVIRT(K)/PARMDL(34))
            FAC1 = FAC1*FSUP(K)
            FAC2 = FAC2*FSUP(K)
          ENDIF
 140    CONTINUE
C
        SIGTOT =  FAC2*SIGTAB(IP,1,I2)+FAC1*SIGTAB(IP,1,I1)
        SIGELA =  FAC2*SIGTAB(IP,2,I2)+FAC1*SIGTAB(IP,2,I1)
        J = 2
        DO 5 I=0,4
          DO 6 K=0,4
            J = J+1
            SIGVM(I,K) = FAC2*SIGTAB(IP,J,I2)+FAC1*SIGTAB(IP,J,I1)
 6        CONTINUE
 5      CONTINUE
        SIGINE = FAC2*SIGTAB(IP,28,I2)+FAC1*SIGTAB(IP,28,I1)
        SIGDIR = FAC2*SIGTAB(IP,29,I2)+FAC1*SIGTAB(IP,29,I1)
        SIGLSD(1) = FAC2*SIGTAB(IP,30,I2)+FAC1*SIGTAB(IP,30,I1)
        SIGLSD(2) = FAC2*SIGTAB(IP,31,I2)+FAC1*SIGTAB(IP,31,I1)
        SIGHSD(1) = FAC2*SIGTAB(IP,32,I2)+FAC1*SIGTAB(IP,32,I1)
        SIGHSD(2) = FAC2*SIGTAB(IP,33,I2)+FAC1*SIGTAB(IP,33,I1)
        SIGLDD = FAC2*SIGTAB(IP,34,I2)+FAC1*SIGTAB(IP,34,I1)
        SIGHDD = FAC2*SIGTAB(IP,35,I2)+FAC1*SIGTAB(IP,35,I1)
        SIGCDF = FAC2*SIGTAB(IP,36,I2)+FAC1*SIGTAB(IP,36,I1)
C  corrections due to photon virtuality
        IF(IPAMDL(115).GT.0) THEN
          SIGHAR = FAC2*SIGTAB(IP,70,I2)+FAC1*SIGTAB(IP,70,I1)
        ELSE
          SIGHAR = 0.D0
        ENDIF
        SIGINE = SIGINE-SIGDIR-SIGHAR
        SIGTOT = SIGTOT-SIGDIR-SIGHAR
        SIGDIR = SIGDIR/(FSUP(1)*FSUP(2))
        SIGHAR = SIGHAR/(FSUP(1)*FSUP(2))
        SIGINE = SIGINE+SIGDIR+SIGHAR
        SIGTOT = SIGTOT+SIGDIR+SIGHAR
        SIGLSD(1) = SIGLSD(1)*FSUP(2)
        SIGLSD(2) = SIGLSD(2)*FSUP(1)
        SIGHSD(1) = SIGHSD(1)*FSUP(2)
        SIGHSD(2) = SIGHSD(2)*FSUP(1)
        SIGLDD    = SIGLDD*FSUP(1)*FSUP(2)
        SIGCDF    = SIGCDF*FSUP(1)*FSUP(2)
        DO 7 I=0,4
          DO 8 K=0,4
            SIGVM(I,K) = SIGVM(I,K)*FSUP(1)*FSUP(2)
 8        CONTINUE
 7      CONTINUE
C
        SIG1SO = FAC2*SIGTAB(IP,37,I2)+FAC1*SIGTAB(IP,37,I1)
        SIG1HA = FAC2*SIGTAB(IP,38,I2)+FAC1*SIGTAB(IP,38,I1)
        SLOEL = FAC2*SIGTAB(IP,39,I2)+FAC1*SIGTAB(IP,39,I1)
        J = 39
        DO 9 I=1,4
          DO 10 K=1,4
            J = J+1
            SLOVM(I,K) = FAC2*SIGTAB(IP,J,I2)+FAC1*SIGTAB(IP,J,I1)
 10       CONTINUE
 9      CONTINUE
        SIGPOM = FAC2*SIGTAB(IP,56,I2)+FAC1*SIGTAB(IP,56,I1)
        SIGREG = FAC2*SIGTAB(IP,57,I2)+FAC1*SIGTAB(IP,57,I1)
        SIGHAR = FAC2*SIGTAB(IP,58,I2)+FAC1*SIGTAB(IP,58,I1)
C  effective cross section
        SIGGEN(3) = 0.D0
        IF(ISWMDL(2).NE.0) THEN
          IF(IPRON(1).EQ.1) SIGGEN(3) = SIGTOT-SIGELA-SIGVM(0,0)
     &      -SIGCDF-SIGLSD(1)-SIGHSD(1)-SIGLSD(2)-SIGHSD(2)-SIGLDD
     &      -SIGHDD-SIGDIR
          IF(IPRON(2).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGELA
          IF(IPRON(3).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGVM(0,0)
          IF(IPRON(4).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGCDF
          IF(IPRON(5).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGLSD(1)+SIGHSD(1)
          IF(IPRON(6).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGLSD(2)+SIGHSD(2)
          IF(IPRON(7).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGLDD+SIGHDD
          IF(IPRON(8).EQ.1) SIGGEN(3) = SIGGEN(3)+SIGDIR
C  simulate only hard scatterings
        ELSE
          IF(IPRON(8).EQ.1) THEN
            SIGGEN(3) = FAC2*SIGTAB(IP,59,I2)+FAC1*SIGTAB(IP,59,I1)
            SIGGEN(3) = SIGGEN(3)/(FSUP(1)*FSUP(2))
          ENDIF
          IF(IPRON(1).EQ.1) SIGGEN(3) = SIGGEN(3)
     &      +(FAC2*SIGTAB(IP,58,I2)+FAC1*SIGTAB(IP,58,I1))
     &      /(FSUP(1)*FSUP(2))
        ENDIF
      ENDIF
C  debug output
      IF(IDEB(63).GE.15) THEN
        WRITE(6,'(1X,A,2I5)') 'POHEPI:DEBUG:HEPEVS INITIALIZED:'
        ONEM = -1.D0
        CALL POXSEC(1,ONEM)
        CALL POPREV(0)
      ENDIF
      END
C
C
      SUBROUTINE PARTPT(IMODE,IF,IL,PTCUT,IREJ)
C********************************************************************
C
C    assign to parton level particles soft/hard pt
C    and append these partons on COMMON /HEPEVS/
C
C    input:  IMODE   -2   output of statistics
C                    -1   initialization
C                     0   sampling of pt for soft partons belonging to
C                         soft Pomerons
C                     1   sampling of pt for soft partons belonging to
C                         hard Pomerons
C            IF           first entry in /HEPEVE/ to check
C            IL           last entry in /HEPEVE/ to check
C            PTCUT        current value of PTCUT between soft and hard
C
C    output: IREJ     0   success
C                     1   failure
C
C    (soft pt is sampled by call to SOFTPT)
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           THREE  =  3.D0,
     &           OHALF  =  0.5D0,
     &           PI2    =  6.28318530718D0,
     &           DEPS   =  1.D-40)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      DIMENSION PTS(2,100),XP(100),MODIFY(100),P(2,2),IV(100)
C
C  debug output
      IF(IDEB(6).GE.10) WRITE(6,'(1X,A,3I4)')
     &  'PARTPT:DEBUG:MODE,FIRST,LAST',IMODE,IF,IL
C
      IF(IMODE.LT.0) GOTO 1000
C
      IREJ = 0
      IF((ISWMDL(3).EQ.10).AND.(ISWMDL(4).EQ.10)) RETURN
C
C  find first parton
      DO 100 ISTART=IF,IL
        IF(ISTHEP(ISTART).EQ.-1) GOTO 200
 100  CONTINUE
 200  CONTINUE
C  count entries to modify
      IENTRY = 0
      PTCUT2 = PTCUT**2
      EMIN = 1.D20
      IPEAK = 1
C  soft Pomerons
      IF(IMODE.EQ.0) THEN
        DO 300 I=ISTART,IL
          IF((ISTHEP(I).EQ.-1).AND.(ABS(IPHIST(1,I)).LT.100)) THEN
            IF((PHEP(1,I)**2+PHEP(2,I)**2).LT.PTCUT2) THEN
              IENTRY = IENTRY+1
              MODIFY(IENTRY) = I
              XP(IENTRY) = SIGN(PHEP(4,I)/ECM,PHEP(3,I))
              IV(IENTRY) = ICOLOR(2,I)
              IF(PHEP(4,I).LT.EMIN) THEN
                EMIN = PHEP(4,I)
                IPEAK = IENTRY
              ENDIF
            ENDIF
          ENDIF
 300    CONTINUE
C  hard Pomerons
      ELSE IF(IMODE.EQ.1) THEN
        DO 350 I=ISTART,IL
          IF((ISTHEP(I).EQ.-1).AND.(IPHIST(1,I).GE.100)) THEN
            IF((PHEP(1,I)**2+PHEP(2,I)**2).LT.PTCUT2) THEN
              IENTRY = IENTRY+1
              MODIFY(IENTRY) = I
              XP(IENTRY) = SIGN(PHEP(4,I)/ECM,PHEP(3,I))
              IF(ISWMDL(24).EQ.0) THEN
                IV(IENTRY) = ICOLOR(2,I)
              ELSE IF(ISWMDL(24).EQ.1) THEN
                IV(IENTRY) = -1
              ELSE
                IV(IENTRY) = 1
              ENDIF
              IF(PHEP(4,I).LT.EMIN) THEN
                EMIN = PHEP(4,I)
                IPEAK = IENTRY
              ENDIF
            ENDIF
          ENDIF
 350    CONTINUE
      ELSE
        WRITE(6,'(/1X,A,I5)') 'PARTPT:ERROR:INVALID MODE',IMODE
        CALL POABRT
      ENDIF
C  debug output
      IF(IDEB(6).GE.5) THEN
        WRITE(6,'(1X,A,3I4)')
     &    'PARTPT:DEBUG:NUMBER OF PARTONS,IPEAK,MODE',IENTRY,IPEAK,IMODE
        IF(IDEB(6).GE.20) CALL POPREV(0)
      ENDIF
      IF(IENTRY.LE.1) RETURN
C
C  sample pt of soft partons
      IF(ISWMDL(5).LE.1) THEN
C  energy limited sampling
        CALL SWAPI(MODIFY(IPEAK),MODIFY(1))
        ITER = 0
 400    CONTINUE
          PSUMX = ZERO
          PSUMY = ZERO
          ITER = ITER+1
          IF(ITER.GE.1000) THEN
            WRITE(6,'(/1X,A,2I5)') 'PARTPT:WARNING:REJECT IMOD,ITER',
     &        IENTRY,ITER
            IREJ = 1
            RETURN
          ENDIF
          DO 410 I=2,IENTRY
            II = MODIFY(I)
            PTMX = MIN(PHEP(4,II),PTCUT)
            IF(ISWMDL(5).EQ.0) THEN
              CALL SOFTPT(1,PTCUT,PTMX,XP(II),IV(II),P)
            ELSE
              CALL SOFTPT(1,PTMX,PTMX,XP(II),IV(II),P)
            ENDIF
            PTS(1,I) = P(1,1)
            PTS(2,I) = P(2,1)
            PSUMX = PSUMX+P(1,1)
            PSUMY = PSUMY+P(2,1)
 410      CONTINUE
          PTREM = SQRT(PSUMX**2+PSUMY**2)
        IF(PTREM.GT.MIN(PHEP(4,I),PTCUT)) GOTO 400
        PTS(1,1) = -PSUMX
        PTS(2,1) = -PSUMY
      ELSE IF(ISWMDL(5).EQ.2) THEN
C  unlimited sampling
        IPEAK = DRNDM(PSUMX)*IENTRY+1
        CALL SWAPI(MODIFY(IPEAK),MODIFY(1))
        CALL SWAPD(XP(IPEAK),XP(1))
        CALL SWAPI(IV(IPEAK),IV(1))
        CALL SOFTPT(IENTRY,PTCUT,PTCUT,XP,IV,PTS)
      ELSE
        WRITE(6,'(/1X,A,I4)') 'PARTPT:UNSUPPORTED ISWMDL(5):',ISWMDL(5)
        CALL POABRT
      ENDIF
C  change parton in COMMON HEPEVS
      DO 500 II=1,IENTRY
        I = MODIFY(II)
        PHEP(1,I) = PHEP(1,I)+PTS(1,II)
        PHEP(2,I) = PHEP(2,I)+PTS(2,II)
        AMSQR = PHEP(4,I)**2
     &           -PHEP(1,I)**2-PHEP(2,I)**2-PHEP(3,I)**2
        PHEP(5,I) = SIGN(SQRT(ABS(AMSQR)),AMSQR)
 500  CONTINUE
C  debug output
      IF(IDEB(6).GE.15) THEN
        WRITE(6,'(1X,A,/1X,A)') 'PARTPT:DEBUG OUTPUT:',
     &                        '===================='
        CALL POPREV(0)
      ENDIF
      RETURN
C
C  initialization / output of statistics
 1000 CONTINUE
      CALL SOFTPT(IMODE,PTM,PTM,XP,IV,PTS)
      END
C
C
      SUBROUTINE MSHELL(PA1,PA2,XM1,XM2,P1,P2,IREJ)
C********************************************************************
C
C    rescaling of momenta of two partons to put both
C                                       on mass shell
C
C    input:       PA1,PA2   input momentum vectors
C                 XM1,2     desired masses of particles afterwards
C                 P1,P2     changed momentum vectors
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-20)
C
      DIMENSION PA1(4),PA2(4),P1(4),P2(4)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
C  debug output
      IREJ = 0
      IF(IDEB(40).GE.10) THEN
        WRITE(6,'(1X,A)') 'MSHELL:DEBUG:INPUT MOMENTA:'
        WRITE(6,'(5X,4E12.5)') (PA1(K),K=1,4)
        WRITE(6,'(5X,4E12.5)') (PA2(K),K=1,4)
        WRITE(6,'(5X,A,2E12.3)') 'NEW MASSES:',XM1,XM2
      ENDIF
C  Lorentz transformation into system CMS
      PX = PA1(1)+PA2(1)
      PY = PA1(2)+PA2(2)
      PZ = PA1(3)+PA2(3)
      EE = PA1(4)+PA2(4)
      XMS = EE**2-PX**2-PY**2-PZ**2
      IF(XMS.LT.(XM1+XM2)**2) THEN
        IREJ = 1
        IDEV = 5
        IFAIL(37) = IFAIL(37)+1
        IF(IDEB(40).GT.0) THEN
          WRITE(6,'(/1X,A)') 'MSHELL:REJECT:TOO SMALL CHAIN MASS'
          WRITE(6,'(5X,A,3E12.4)') 'SM,MASSES:',
     &      SIGN(SQRT(ABS(XMS)),XMS),XM1,XM2
          WRITE(6,'(5X,A,4E11.4)') 'PX,PY,PZ,EE:',PX,PY,PZ,EE
          IF(IDEB(40).GE.3) GOTO 55
        ENDIF
        RETURN
      ENDIF
      XMS = SQRT(XMS)
      BGX = PX/XMS
      BGY = PY/XMS
      BGZ = PZ/XMS
      GAM = EE/XMS
      CALL ALTRA(GAM,-BGX,-BGY,-BGZ,PA1(1),PA1(2),PA1(3),
     &           PA1(4),PTOT1,P1(1),P1(2),P1(3),P1(4))
C  rotation angles
      PTOT1 = MAX(DEPS,PTOT1)
      COD= P1(3)/PTOT1
      SID= SQRT((ONE-COD)*(ONE+COD))
      COF=ONE
      SIF=ZERO
      IF(PTOT1*SID.GT.DEPS) THEN
        COF=P1(1)/(SID*PTOT1)
        SIF=P1(2)/(SID*PTOT1)
        ANORF=SQRT(COF*COF+SIF*SIF)
        COF=COF/ANORF
        SIF=SIF/ANORF
      ENDIF
C  new CM momentum and energies (for masses XM1,XM2)
      XM12 = XM1**2
      XM22 = XM2**2
      SS   = XMS**2
      PCMP = XLAMB(SS,XM12,XM22)/(2.D0*XMS)
      EE1  = SQRT(XM12+PCMP**2)
      EE2  = XMS-EE1
C  back rotation
      CALL TRANS(ZERO,ZERO,PCMP,COD,SID,COF,SIF,XX,YY,ZZ)
      CALL ALTRA(GAM,BGX,BGY,BGZ,XX,YY,ZZ,EE1,
     &           PTOT1,P1(1),P1(2),P1(3),P1(4))
      CALL ALTRA(GAM,BGX,BGY,BGZ,-XX,-YY,-ZZ,EE2,
     &           PTOT2,P2(1),P2(2),P2(3),P2(4))
C  check consistency
      DEL = XMS*0.0001
      IF(ABS(PX-P1(1)-P2(1)).GT.DEL) THEN
        IDEV = 1
      ELSE IF(ABS(PY-P1(2)-P2(2)).GT.DEL) THEN
        IDEV = 2
      ELSE IF(ABS(PZ-P1(3)-P2(3)).GT.DEL) THEN
        IDEV = 3
      ELSE IF(ABS(EE-P1(4)-P2(4)).GT.DEL) THEN
        IDEV = 4
      ELSE
        IDEV = 0
      ENDIF
 55   CONTINUE
C  debug output
      IF(IDEV.NE.0) THEN
        WRITE(6,'(/1X,A,I3)') 'MSHELL:WARNING:INCONSISTENT TRAFO',IDEV
        WRITE(6,'(1X,A)') 'MSHELL:INPUT MOMENTA:'
        WRITE(6,'(5X,4E12.5)') (PA1(K),K=1,4)
        WRITE(6,'(5X,4E12.5)') (PA2(K),K=1,4)
        WRITE(6,'(5X,A,3E12.3)') 'AVA.MASS,MASSES:',XMS,XM1,XM2
        WRITE(6,'(1X,A)') 'MSHELL:OUTPUT MOMENTA:'
        WRITE(6,'(5X,4E12.5)') (P1(K),K=1,4)
        WRITE(6,'(5X,4E12.5)') (P2(K),K=1,4)
      ELSE IF(IDEB(40).GE.10) THEN
        WRITE(6,'(1X,A)') 'MSHELL:DEBUG:OUTPUT MOMENTA:'
        WRITE(6,'(5X,4E12.5)') (P1(K),K=1,4)
        WRITE(6,'(5X,4E12.5)') (P2(K),K=1,4)
      ENDIF
      END
C
C
      SUBROUTINE GLU2QU(IG,IQ1,IQ2,IREJ)
C********************************************************************
C
C    split gluon with index I in HEPEVS
C          (massless gluon assumed)
C
C    input:      COMMON  HEPEVS
C                IG      gluon index
C                IQ1     first quark index
C                IQ2     second quark index
C
C    output:     new quarks in COMMON HEPEVS
C                IREJ    1 splitting impossible
C                        0 splitting successful
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO   =  0.D0,
     &           ONE    =  1.D0,
     &           TWO    =  2.D0,
     &           OHALF  =  0.5D0,
     &           DEPS   =  1.D-40,
     &           EPS    =  1.D-5)
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVS/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      COMMON/HEPEVE/IMPART(NMXHEP),IPHIST(2,NMXHEP),ICOLOR(2,NMXHEP)
C
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
C
      DIMENSION P1(4),P2(4)
      DATA CUTM  /0.02D0/
C
      IREJ = 0
C
C  calculate chain masses max possible
      IF(ISWMDL(9).EQ.1) THEN
        CMASS1=2.D0*(PHEP(4,IG)*PHEP(4,IQ1)-PHEP(1,IG)*PHEP(1,IQ1)
     &     -PHEP(2,IG)*PHEP(2,IQ1)-PHEP(3,IG)*PHEP(3,IQ1))
        IF(CMASS1.LT.CUTM) THEN
          IF(IDEB(73).GE.5) THEN
            WRITE(6,'(1X,A,3I4,4E10.3)')
     &        'GLU2QU:REJECTION:IG,IQ1,IQ2,CMASS1',IG,IQ1,IQ2,CMASS1
          ENDIF
          IFAIL(33) = IFAIL(33) + 1
          IREJ = 1
          RETURN
        ENDIF
        CMASS2=2.D0*(PHEP(4,IG)*PHEP(4,IQ2)-PHEP(1,IG)*PHEP(1,IQ2)
     &     -PHEP(2,IG)*PHEP(2,IQ2)-PHEP(3,IG)*PHEP(3,IQ2))
        IF(CMASS2.LT.CUTM) THEN
          IF(IDEB(73).GE.5) THEN
            WRITE(6,'(1X,A,3I4,4E10.3)')
     &        'GLU2QU:REJECTION:IG,IQ1,IQ2,CMASS2',IG,IQ1,IQ2,CMASS2
          ENDIF
          IFAIL(33) = IFAIL(33) + 1
          IREJ = 1
          RETURN
        ENDIF
C
C  calculate minimal z
        ZMIN1 = (CUTM-SIGN(PHEP(5,IQ1)**2,PHEP(5,IQ1)))/CMASS1+EPS
        ZMIN2 = (CUTM-SIGN(PHEP(5,IQ2)**2,PHEP(5,IQ2)))/CMASS2+EPS
        ZMIN = MIN(ZMIN1,ZMIN2)
        IF(MAX(ZMIN1,ZMIN2).GE.0.45D0) THEN
          IF(IDEB(73).GE.5) THEN
            WRITE(6,'(1X,A,3I3,4E10.3)')
     &        'GLU2QU:REJECTION:IG,IQ1,IQ2,ZMIN1,ZMIN2,P1*PG,P2*PG',
     &        IG,IQ1,IQ2,ZMIN1,ZMIN2,CMASS1,CMASS2
          ENDIF
          IFAIL(33) = IFAIL(33) + 1
          IREJ = 1
          RETURN
        ENDIF
      ELSE
        ZMIN = MIN(0.1D0,0.5D0/PHEP(4,IG))
      ENDIF
C
      ZFRAC = GLUSPL(ZMIN)
      IF((ZFRAC.LT.ZMIN1).OR.((ONE-ZFRAC).LT.ZMIN2)) THEN
        ZFRAC = ONE-ZFRAC
      ENDIF
      DO 200 I=1,4
        P1(I) = PHEP(I,IG)*ZFRAC
        P2(I) = PHEP(I,IG)*(ONE-ZFRAC)
 200  CONTINUE
C  quark flavours
      CMASS1 = SQRT(ZFRAC*CMASS1+SIGN(PHEP(5,IQ1)**2,PHEP(5,IQ1)))
      CMASS2 = SQRT((ONE-ZFRAC)*CMASS2+SIGN(PHEP(5,IQ2)**2,PHEP(5,IQ2)))
      CALL SEAFLA(IG,K,I,MIN(CMASS1,CMASS2))
      CALL QUASTA(K,21,3)
      CALL QUASTA(I,21,3)
      IF(ABS(IDHEP(IQ1)).GT.6) THEN
        K = SIGN(ABS(K),IDHEP(IQ1))
      ELSE
        K = -SIGN(ABS(K),IDHEP(IQ1))
      ENDIF
C  colors
      IF(K.GT.0) THEN
        IC1 = MAX(ICOLOR(1,IG),ICOLOR(2,IG))
        IC2 = MIN(ICOLOR(1,IG),ICOLOR(2,IG))
      ELSE
        IC1 = MIN(ICOLOR(1,IG),ICOLOR(2,IG))
        IC2 = MAX(ICOLOR(1,IG),ICOLOR(2,IG))
      ENDIF
C  register new partons
      CALL REGPAR(-1,K,0,IG,0,P1(1),P1(2),P1(3),P1(4),
     &            IPHIST(1,IG),0,IC1,0,IPOS,1)
      CALL REGPAR(-1,-K,0,IG,0,P2(1),P2(2),P2(3),P2(4),
     &            IPHIST(1,IG),0,IC2,0,IPOS,1)
C  debug output
      IF(IDEB(73).GE.20) THEN
          WRITE(6,'(1X,A,/1X,A,3I3,5E10.3)')
     &      'GLU2QU:DEBUG:','   IG,IQ1,IQ2,ZMIN1,2,Z,P1*PG,P2*PG',
     &      IG,IQ1,IQ2,ZMIN1,ZMIN2,ZFRAC,CMASS1,CMASS2
        WRITE(6,'(1X,A,4I5)') '   FLAVOURS, COLORS  ',
     &    K,-K,IC1,IC2
      ENDIF
      END
C
C
      DOUBLE PRECISION FUNCTION GLUSPL(ZMIN)
C*********************************************************************
C
C     calculate quark - antiquark light cone momentum fractions
C     according to Altarelli-Parisi g->q aq splitting function
C     (symmetric z interval assumed)
C
C     input: ZMIN    minimal Z value allowed,
C                    1-ZMIN maximal Z value allowed
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0,
     &           ALEXP= 0.3333333333D0,
     &           OHALF= 0.5D0,
     &           DEPS = 1.D-10,
     &           ONE  = 1.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      IF(ZMIN.GE.OHALF) THEN
        IF(IDEB(69).GT.2) THEN
          WRITE(6,'(1X,A,E12.4)') 'GLUSPL:WARNING:ZMIN>=0.5',ZMIN
        ENDIF
        ZZ=ZERO
        GOTO 1000
      ELSE IF(ZMIN.LE.ZERO) THEN
        IF(IDEB(69).GT.2) THEN
          WRITE(6,'(1X,A,E12.4)') 'GLUSPL:WARNING:ZMIN<=0',ZMIN
        ENDIF
        ZMINL = DEPS
      ELSE
        ZMINL = ZMIN
      ENDIF
C
      ZMAX = ONE-ZMINL
      XI   = DRNDM(ZMAX)
      ZZ   = ((ONE-XI)*ZMINL**3+XI*ZMAX**3)**ALEXP
      IF(DRNDM(ZZ).LT.OHALF) ZZ = ONE-ZZ
C
 1000 CONTINUE
      IF(IDEB(69).GE.10) THEN
        WRITE(6,'(1X,A,2E12.4)') 'GLUSPL:DEBUG:ZMIN,Z ',ZMIN,ZZ
      ENDIF
      GLUSPL = ZZ
      END

      SUBROUTINE SETMDL(INDX,IVAL,IMODE)
C**********************************************************************
C
C     set model switches
C
C     input:  INDX       model parameter number
C                        (positive: ISWMDL, negative: IPAMDL)
C             IVAL       new value
C             IMODE      -1  print value of parameter INDX
C                        1   set new value
C                        -2  print current settings
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      IF(IMODE.EQ.-2) THEN
        WRITE(6,'(/1X,A,/1X,A,/)') 'SETMDL:DEBUG: CURRENT SETTINGS:',
     &    '==============================='
        DO 100 I=1,48,3
          IF(ISWMDL(I).EQ.-9999) GOTO 200
          IF(ISWMDL(I+1).EQ.-9999) THEN
            WRITE(6,'(5X,I3,A1,A,I6)') I,':',MDLNA(I),ISWMDL(I)
            GOTO 200
          ELSE IF(ISWMDL(I+2).EQ.-9999) THEN
            WRITE(6,'(2(5X,I3,A1,A,I6))') I,':',MDLNA(I),ISWMDL(I),
     &        I+1,':',MDLNA(I+1),ISWMDL(I+1)
            GOTO 200
          ELSE
            WRITE(6,'(3(5X,I3,A1,A,I6))')
     &        (I+K,':',MDLNA(I+K),ISWMDL(I+K),K=0,2)
          ENDIF
 100    CONTINUE
 200    CONTINUE
      ELSE IF(IMODE.EQ.-1) THEN
        WRITE(6,'(1X,A,1X,A,I6)') 'SETMDL:',MDLNA(INDX),ISWMDL(INDX)
      ELSE IF(IMODE.EQ.1) THEN
        IF(INDX.GT.0) THEN
          IF(ISWMDL(INDX).NE.IVAL) THEN
            WRITE(6,'(1X,A,I4,1X,A,2I6)') 'SETMDL:ISWMDL(OLD/NEW):',
     &        INDX,MDLNA(INDX),ISWMDL(INDX),IVAL
            ISWMDL(INDX) = IVAL
          ENDIF
        ELSE IF(INDX.LT.0) THEN
          IF(IPAMDL(-INDX).NE.IVAL) THEN
            WRITE(6,'(1X,A,I4,1X,2I6)') 'SETMDL:IPAMDL(OLD/NEW):',
     &        -INDX,IPAMDL(-INDX),IVAL
            IPAMDL(-INDX) = IVAL
          ENDIF
        ENDIF
      ELSE
        WRITE(6,'(/1X,A,I6)') 'SETMDL:ERROR: UNSUPPORTED MODE',IMODE
      ENDIF
      END
C
C
      SUBROUTINE PHODAT
C*********************************************************************
C
C     initialization of variables and switches
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      COMMON /HAQQAP/ AQQAL,AQQALI,AQQALF,AQQPD,
     &                NQQAL,NQQALI,NQQALF,NQQPD
      COMMON /HAGAUP/ NGAUP1,NGAUP2,NGAUET,NGAUIN,NGAUSO
      COMMON /HAPARA/ ALPHAS,Q0SQR,ALASQR,BQCD,NPD,NF,NHA,NHB
      PARAMETER(IEETAB=10,IIMAX=30,KKMAX=10)
      REAL PROB
      COMMON /PROBAB/ PROB(4,IEETAB,0:IIMAX,0:KKMAX),EPTAB(4,IEETAB),
     &                IEEMAX,IMAX,KMAX
      COMMON /LEPCUT/ ECMIN,ECMAX,EEMIN1,EEMIN2,
     &                YMIN1,YMAX1,YMIN2,YMAX2,
     &                Q2MIN1,Q2MAX1,Q2MIN2,Q2MAX2,
     &                THMIN1,THMAX1,THMIN2,THMAX2
      COMMON /CUTOFF/ PTCUT(4),CUTMU(4),FPS(4),FPH(4),PSOMIN,XSOMIN
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
      COMMON /HADRON/ QMASS(6),BET,PCOUDI,PNORM,VALPRG(2),NFS
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
      COMMON /INPUTS/ PARINP(30)
      COMMON /PROCES/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15)
      COMMON /QEVECM/ VMAS(4),GAMM(4),RMIN(4),RMAX(4),VMSL(4),VMFA(4),
     &                TMIN(4),TMAX(4)
      COMMON /DGLAPP/ Q2MISR(2),PMISR(2),ZMISR(2),AL2ISR(2),NFSISR
      COMMON /ABRISR/ IFLISR(2,150),PHISR(2,4,150),IPOISR(2,2,50)
C
      PARAMETER ( MAXPRO = 16 )
      CHARACTER*18 PROC
      COMMON /PEPROC/ PROC(0:MAXPRO)
      PARAMETER ( MAXTAB = 20 )
      COMMON /HAXSEC/ XSECTA(4,-1:MAXPRO,4,MAXTAB),XSECT(6,-1:MAXPRO),
     &                MXSECT(0:4,-1:MAXPRO,4),ECMSH(4,MAXTAB),ISTTAB
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
      COMMON /FITMOD/ IFMOD,IFHARD
C
C  initialize COMMON /GLOCMS/
      ECM    = 200.D0
      PMASS(1) = 0.938D0
      PVIRT(1) = 0.D0
      PMASS(2) = 0.D0
      PVIRT(2) = 0.D0
      IFPAP(1) = 2212
      IFPAB(1) = 1
      IFPAP(2) = 22
      IFPAB(2) = 7
C  initialize COMMON /HADVAL/
      IHFLD(1,1) = 0
      IHFLD(1,2) = 0
      IHFLD(2,1) = 0
      IHFLD(2,2) = 0
      IHFLS(1) = 1
      IHFLS(2) = 1
C  initialize COMMON /MODELS/
      ISWMDL(1)  = 3
      MDLNA(1)  = 'AMPL MOD'
      ISWMDL(2)  = 1
      MDLNA(2)  = 'MIN-BIAS'
      ISWMDL(3)  = 1
      MDLNA(3)  = 'PTS DISH'
      ISWMDL(4)  = 1
      MDLNA(4)  = 'PTS DISP'
      ISWMDL(5)  = 2
      MDLNA(5)  = 'PTS ASSI'
      ISWMDL(6)  = 2
      MDLNA(6)  = 'HADRONIZ'
      ISWMDL(7)  = 2
      MDLNA(7)  = 'MASS COR'
      ISWMDL(8)  = 3
      MDLNA(8)  = 'PAR SHOW'
      ISWMDL(9)  = 0
      MDLNA(9)  = 'GLU SPLI'
      ISWMDL(10) = 1
      MDLNA(10) = 'VIRT PHO'
      ISWMDL(11) = 0
      MDLNA(11) = 'LARGE NC'
      ISWMDL(12) = 0
      MDLNA(12) = 'LIPA POM'
      ISWMDL(13) = 1
      MDLNA(13) = 'QELAS VM'
      ISWMDL(14) = 1
      MDLNA(14) = 'ENHA GRA'
      ISWMDL(15) = 3
      MDLNA(15) = 'MULT SCA'
      ISWMDL(16) = 4
      MDLNA(16) = 'MULT DIF'
      ISWMDL(17) = 0
      MDLNA(17) = 'POM CORR'
      ISWMDL(18) = 0
      MDLNA(18) = 'BALAN PT'
      ISWMDL(19) = 0
      MDLNA(19) = 'POMV FLA'
      ISWMDL(20) = 0
      MDLNA(20) = 'SEA  FLA'
      ISWMDL(21) = 2
      MDLNA(21) = 'SPIN DEC'
      ISWMDL(22) = 1
      MDLNA(22) = 'DIF.MASS'
      ISWMDL(23) = 1
      MDLNA(23) = 'DIFF RES'
      ISWMDL(24) = 0
      MDLNA(24) = 'PTS HPOM'
      ISWMDL(25) = 4
      MDLNA(25) = 'MULT CDF'
      ISWMDL(26) = 1
      MDLNA(26) = 'OVERLAP '
      ISWMDL(27) = 0
      MDLNA(27) = 'MUL R/AN'
      ISWMDL(28) = -9999
C  mass-independent sea flavour ratios
      PARMDL(1)  = 0.425D0
      PARMDL(2)  = 0.425D0
      PARMDL(3)  = 0.15D0
      PARMDL(4)  = 0.D0
      PARMDL(5)  = 0.D0
      PARMDL(6)  = 0.D0
C  relative size of GPPR to GPPP
      PARMDL(7)  = 3.6D0
C  suppression by energy momentum conservation
      PARMDL(8)  = 7.D0
      PARMDL(9)  = 7.D0
C  VDM factors
      PARMDL(10) = 0.856D0
      PARMDL(11) = 0.288D0
      PARMDL(12) = 0.288D0
      PARMDL(13) = 0.316D0
      PARMDL(14) = 0.866D0
      PARMDL(15) = 0.288D0
      PARMDL(16) = 0.288D0
      PARMDL(17) = 0.288D0
      PARMDL(18) = 0.D0
C  lower energy limit for initialization
      PARMDL(19) = 5.D0
C  intrinsic pt for hard scattering
      PARMDL(20) = 5.D0
C  low energy beta of soft pt distribution 1
      PARMDL(21) = 6.0D0
C  high energy beta of soft pt distribution 1
      PARMDL(22) = 4.0D0
C  low energy beta of soft pt distribution 0
      PARMDL(23) = 2.5D0
C  high energy beta of soft pt distribution 0
      PARMDL(24) = 0.4D0
C  effective quark mass in photon wave function
      PARMDL(25) = 0.2D0
C  normalization of unevolved Pomeron PDFs
      PARMDL(26) = 0.3D0
C  effective VDM parameters for Q**2 dependence of cross section
      PARMDL(27) = 0.65D0
      PARMDL(28) = 0.08D0
      PARMDL(29) = 0.05D0
      PARMDL(30) = 0.22D0
      PARMDL(31) = 0.589824D0
      PARMDL(32) = 0.609961D0
      PARMDL(33) = 1.038361D0
      PARMDL(34) = 1.96D0
C  pt cutoff defaults
      PARMDL(36) = 2.5D0
      PARMDL(37) = 2.5D0
      PARMDL(38) = 2.5D0
      PARMDL(39) = 2.5D0
C  mass in soft pt distribution
      PARMDL(43) = 0.D0
C  maximum of x allowed for leading particle
      PARMDL(44) = 0.9D0
C  coherence constraint in diffraction
      PARMDL(45) = 0.46D0
C  mass threshold in diffraction (2pi mass)
      PARMDL(46) = 0.3D0
C  regularization of slope parameter in diffraction
      PARMDL(47) = 4.D0
C  renormalized intercept for enhanced graphs
      PARMDL(48) = 1.08D0
C  Ross-Stodolsky exponent
      PARMDL(49) = 4.2D0
C  exponents of x distributions
C  baryon
      PARMDL(50) = 1.5D0
      PARMDL(51) = -0.5D0
      PARMDL(52) = -0.99D0
      PARMDL(53) = -0.9D0
C  meson (non-strangeness part)
      PARMDL(54) = -0.5D0
      PARMDL(55) = -0.5D0
      PARMDL(56) = -0.99D0
      PARMDL(57) = -0.99D0
C  meson (strangeness part)
      PARMDL(58) = -0.2D0
      PARMDL(59) = -0.2D0
      PARMDL(60) = -0.99D0
      PARMDL(61) = -0.99D0
C  particle remnant (no valence quarks)
      PARMDL(62) = -0.5D0
      PARMDL(63) = -0.5D0
      PARMDL(64) = -0.99D0
      PARMDL(65) = -0.99D0
C  minimal mass for elastic pomerons in central diffraction
      PARMDL(70) = 2.D0
C  minimal mass of diffractive blob in central diffraction
      PARMDL(71) = 2.D0
C  minimal Feynman x cut in central diffraction
      PARMDL(72) = 0.D0
C  direct pomeron coupling
      PARMDL(74) = 0.D0
C  relative deviation allowed for energy-momentum conservation
C  energy-momentum relative deviation
      PARMDL(75) = 0.01D0
C  transverse momentum deviation
      PARMDL(76) = 0.01D0
C  scales to calculate alpha-s of matrix element
      PARMDL(81) = 0.075D0
      PARMDL(82) = 1.D0
      PARMDL(83) = 1.D0
C  scales to calculate alpha-s of initial state radiation
      PARMDL(84) = 1.D0
      PARMDL(85) = 1.D0
      PARMDL(86) = 1.D0
C  scales to calculate alpha-s of final state radiation
      PARMDL(87) = 1.D0
      PARMDL(88) = 1.D0
      PARMDL(89) = 1.D0
C  scales to calculate PDFs
      PARMDL(90) = 1.D0
      PARMDL(91) = 1.D0
      PARMDL(92) = 1.D0
C  scale for ISR starting virtuality
      PARMDL(93) = 1.D0
C  weight factors for pt-distribution
      PARMDL(101) = 2.D0
      PARMDL(102) = 2.D0
      PARMDL(103) = 4.D0
      PARMDL(104) = 2.D0
      PARMDL(105) = 6.D0
      PARMDL(106) = 4.D0
C
*     PARMDL(110-125)  reserved for hard scattering
C  currently chosen scales for hard scattering
      DO 10 I=1,16
        PARMDL(109+I) = 0.D0
 10   CONTINUE
C  virtuality cutoff in initial state evolution
      PARMDL(126) = PARMDL(36)**2
      PARMDL(127) = PARMDL(36)**2
      PARMDL(128) = PARMDL(36)**2
      PARMDL(129) = PARMDL(36)**2
C  complex amplitudes, eikonal functions
      IPAMDL(1)  = 0
C  allow for Reggeon cuts
      IPAMDL(2)  = 1
C  decay of hadron resonances in diffraction (0 iso, 1 trans, 2 long)
      IPAMDL(3)  = 0
C  polarization of photon resonances (0 none, 1 trans, 2 long)
      IPAMDL(4)  = 1
C  pt of valence partons
      IPAMDL(5)  = 1
C  pt of hard scattering remnant
      IPAMDL(6)  = 0
C  running cutoff for hard scattering
      IPAMDL(7)  = 1
C  intercept used for the calculation of enhanced graphs
      IPAMDL(8)  = 1
C  effective slope of hard scattering amplitde
      IPAMDL(9)  = 0
C  mass dependence of slope parameters
      IPAMDL(10) = 0
C  lepton-photon vertex 1
      IPAMDL(11) = 0
C  lepton-photon vertex 2
      IPAMDL(12) = 0
C  call by DTUNUC
      IPAMDL(13) = 0
C  method to sample x distributions
      IPAMDL(14) = 3
C  energy-momentum check
      IPAMDL(15) = 1
C  resummation of triple- and loop-Pomeron
      IPAMDL(24) = 1
C  resummation of X iterated triple-Pomeron
      IPAMDL(25) = 1
C  dimension of interpolation table for weights in hard scattering
      IPAMDL(30) = MAXTAB
C  dimension of interpolation table for pomeron cut distribution
      IPAMDL(31) = IEETAB
C  number of cut soft pomerons (restriction by field dimension)
      IPAMDL(32) = IIMAX
C  number of cut hard pomerons (restriction by field dimension)
      IPAMDL(33) = KKMAX
C  tau pair production in direct photon-photon collisions
      IPAMDL(64) = 0
C  currently chosen scales for hard scattering
*     IPAMDL(65-80)  reserved for hard scattering
      DO 15 I=1,16
        IPAMDL(64+I) = -99999
 15   CONTINUE
C  scales to calculate alpha-s of matrix element
      IPAMDL(81) = 1
      IPAMDL(82) = 1
      IPAMDL(83) = 1
C  scales to calculate alpha-s of initial state radiation
      IPAMDL(84) = 1
      IPAMDL(85) = 1
      IPAMDL(86) = 1
C  scales to calculate alpha-s of final state radiation
      IPAMDL(87) = 1
      IPAMDL(88) = 1
      IPAMDL(89) = 1
C  scales to calculate PDFs
      IPAMDL(90) = 1
      IPAMDL(91) = 1
      IPAMDL(92) = 1
C  program abort for fatal errors (simulation of division by zero)
      IPAMDL(100) = 0
C  qqbar-gamma coupling in initial state showers
      IPAMDL(110) = 1
C  reweighting / use photon virtuality in photon PDF calculations
      IPAMDL(115) = 1
C  max. number of conservation law violations allowed in one run
      IPAMDL(179) = 20
C  initialize COMMON /PROBAB/
      IEEMAX = IEETAB
      IMAX   = IIMAX
      KMAX   = KKMAX
C  initialize COMMON /INPUTS/
      DO 20 I=1,30
        PARINP(I) = -100000.D0
 20   CONTINUE
C  initialize COMMON /HADRON/
      QMASS(1) =  0.3D0
      QMASS(2) =  0.3D0
      QMASS(3) =  0.5D0
      QMASS(4) =  1.6D0
      QMASS(5) =  5.D0
      QMASS(6) =  174.D0
      BET    = 8.D0
      PCOUDI = 0.D0
      VALPRG(1) = 1.D0
      VALPRG(2) = 1.D0
      NFS    = 4
C  initialize COMMON /HAXSEC/
      ISTTAB = 20
C  initialize COMMON /CUTOFF/
      PTCUT(1) = PARMDL(36)
      PTCUT(2) = PARMDL(37)
      PTCUT(3) = PARMDL(38)
      PTCUT(4) = PARMDL(39)
      CUTMU(1) = 1.D0
      CUTMU(2) = 1.D0
      CUTMU(3) = 1.D0
      CUTMU(4) = 1.D0
      PSOMIN = 0.D0
      XSOMIN = 0.D0
C  initialize COMMON /HAPARA/
      NF       = 4
C  initialize COMMON /HAGAUP/
      NGAUP1 = 12
      NGAUP2 = 12
      NGAUET = 16
      NGAUIN = 12
      NGAUSO = 96
C  initialize COMMON /DEBUG/
      DO 30 I=1,100
        IDEB(I) = 0
 30   CONTINUE
C  initialize COMMON /PROCES/
      IPRON(1) = 1
      IPRON(2) = 1
      IPRON(3) = 1
      IPRON(4) = 1
      IPRON(5) = 1
      IPRON(6) = 1
      IPRON(7) = 1
      IPRON(8) = 1
      IPRON(9) = 1
      IPRON(10) = 1
      IPRON(11) = 1
C  initialize COMMON /QEVECM/
      TWOPIM = 0.28D0
      RMIN(1) = 0.285D0
      RMIN(2) = 0.45D0
      RMIN(3) = 1.D0
      RMIN(4) = TWOPIM
      VMAS(1) = 0.770D0
      VMAS(2) = 0.787D0
      VMAS(3) = 1.02D0
      VMAS(4) = TWOPIM
      GAMM(1) = 0.155D0
      GAMM(2) = 0.01D0
      GAMM(3) = 0.0045D0
      GAMM(4) = 1.D0
      RMAX(1) = VMAS(1)+TWOPIM
      RMAX(2) = VMAS(2)+TWOPIM
      RMAX(3) = VMAS(3)+TWOPIM
      RMAX(4) = VMAS(1)+TWOPIM
      VMSL(1) = 11.D0
      VMSL(2) = 10.D0
      VMSL(3) = 6.D0
      VMSL(4) = 4.D0
      VMFA(1) = 0.0033D0
      VMFA(2) = 0.00036D0
      VMFA(3) = 0.0002D0
      VMFA(4) = 0.0002D0
      TMIN(1) = -1.D-5
      TMIN(2) = -1.D-5
      TMIN(3) = -1.D-5
      TMIN(4) = -1.D-5
      TMAX(1) = -1.5
      TMAX(2) = -1.5
      TMAX(3) = -1.5
      TMAX(4) = -1.5
C  initialize COMMON /DGLAPP/
      Q2MISR(1) = PARMDL(36)**2
      Q2MISR(2) = PARMDL(36)**2
      PMISR(1) = 1.D0
      PMISR(2) = 1.D0
      ZMISR(1) = 0.001D0
      ZMISR(2) = 0.001D0
      AL2ISR(1) = 0.046D0
      AL2ISR(2) = 0.046D0
      NFSISR  = 4
C  initialize COMMON /ABRISR/
      DO 40 I=1,50
        IPOISR(1,2,I) = 0
        IPOISR(2,2,I) = 0
 40   CONTINUE
C  initialize COMMON /PEPROC/
      PROC(0) = 'sum over processes'
      PROC(1) = 'G  +G  --> G  +G  '
      PROC(2) = 'Q  +QB --> G  +G  '
      PROC(3) = 'G  +Q  --> G  +Q  '
      PROC(4) = 'G  +G  --> Q  +QB '
      PROC(5) = 'Q  +QB --> Q  +QB '
      PROC(6) = 'Q  +QB --> QP +QBP'
      PROC(7) = 'Q  +Q  --> Q  +Q  '
      PROC(8) = 'Q  +QP --> Q  +QP '
      PROC(9) = 'resolved processes'
      PROC(10) = 'gam+Q  --> G  +Q  '
      PROC(11) = 'gam+G  --> Q  +QB '
      PROC(12) = 'Q  +gam--> G  +Q  '
      PROC(13) = 'G  +gam--> Q  +QB '
      PROC(14) = 'gam+gam--> Q  +QB '
      PROC(15) = 'direct processes  '
      PROC(16) = 'gam+gam--> l+ +l- '
C  initialize COMMON /HAXSEC/
      DO 60 I=1,4
        DO 50 M=-1,15
          XSECT(I,M) = 0.D0
C  switch all hard subprocesses on
          MXSECT(0,M,I)  = 1
 50     CONTINUE
        MXSECT(0,16,I)  = 0
        ECMSH(I,1) = 0.D0
 60   CONTINUE
C  initialize COMMON /HACONS/
      PI = 3.14159265359D0
      PI2 = 2.D0*PI
      PI4 = 2.D0*PI2
C  GeV**-2 --> millibarn
      GEV2MB = 0.389365D0
C  initialize COMMON /LEPCUT/
      ECMIN = 5.D0
      ECMAX = 1.D+30
      EEMIN1 = 1.D0
      EEMIN2 = 1.D0
      YMAX1 = -1.D0
      YMAX2 = -1.D0
      THMIN1 = 0.D0
      THMAX1 = PI
      THMIN2 = 0.D0
      THMAX2 = PI
C  initialize COMMON /FITMOD/
      IFMOD = 0
      IFHARD = 0
      END
C
C
      SUBROUTINE HADDAT
C******************************************************************
C
C     BAMJET/DECAY particle data
C     (update of particle masses, new resonances Apr.94, Jan.95)
C
C******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      CHARACTER*8 ANAME,ZKNAM
      COMMON /PART/ ANAME(187),AM(187),GA(187),TAU(187),ICH(187),
     &              IBAR(187),K1(187),K2(187)
      COMMON /DECAYC/ZKNAM(550),NZK(550,3),WT(550)
C  fragmentation data and parameters
      COMMON/INPDAT/IMPS(6,6),IMVE(6,6),IB08(6,21),IB10(6,21),
     &IA08(6,21),IA10(6,21),A1,B1,B2,B3,LT,LB,ISU,IYQ,BET,AS,B8,AME,DIQ
C  translation table
      COMMON /HAMCIN/ IAMCIN(187)
C  index tables
      DIMENSION IV(36),IP(36),IB(126),IBB(126),IA(126),IAA(126)
C
      CHARACTER*8 BNAME,ZBNAM
      DIMENSION BNAME(187),BM(187),BA(187),BAU(187),IBCH(187),
     &          IBBAR(187),KB1(187),KB2(187)
      DIMENSION ZBNAM(550),NBK1(550),NBK2(550),NBK3(550),BT(550)
      DIMENSION IBMCIN(187)
C
      DATA (BNAME(K),K=  1, 85) /
     &  'P       ','AP      ','E-      ','E+      ','NUE     ',
     &  'ANUE    ','GAM     ','NEU     ','ANEU    ','MUE+    ',
     &  'MUE-    ','K0L     ','PI+     ','PI-     ','K+      ',
     &  'K-      ','LAM     ','ALAM    ','K0S     ','SIGM-   ',
     &  'SIGM+   ','SIGM0   ','PI0     ','K0      ','AK0     ',
     &  'OME1420 ','RHO1450 ','OME1600 ','PHI1680 ','RHO1700 ',
     &  'ETA550  ','RHO+77  ','RHO077  ','RHO-77  ','OM0783  ',
     &  'K*+892  ','K*0892  ','K*-892  ','AK*089  ','KA+125  ',
     &  'KA0125  ','KA-125  ','AKA012  ','K*+142  ','K*0142  ',
     &  'K*-142  ','AK*014  ','S+1385  ','S01385  ','S-1385  ',
     &  'L01820  ','L02030  ','N*++12  ','N*+ 12  ','N*012   ',
     &  'N*-12   ','N*++16  ','N*+16   ','N*016   ','N*-16   ',
     &  'N*+1440 ','N*01440 ','N*+15   ','N*015   ','N*+1710 ',
     &  'N*01710 ','AN--12  ','AN*-12  ','AN*012  ','AN*+12  ',
     &  'AN--16  ','AN*-16  ','AN*016  ','AN*+16  ','AN*-15  ',
     &  'AN*015  ','AN*-1440','AN*01440','AN*-1710','AN*01710',
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','BLANC   '
     &           /
      DATA (BNAME(K),K= 86,170) /
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','BLANC   ',
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','ETA*    ',
     &  'PHI     ','TETA0   ','TETA-   ','ASIG-   ','ASIG0   ',
     &  'ASIG+   ','ATETA0  ','ATETA+  ','SIG*+   ','SIG*0   ',
     &  'SIG*-   ','TETA*0  ','TETA*   ','OMEGA-  ','ASIG*-  ',
     &  'ASIG*0  ','ASIG*+  ','ATET*0  ','ATET*+  ','OMEGA+  ',
     &  'D0      ','D+      ','D-      ','AD0     ','F+      ',
     &  'F-      ','ETAC    ','D*0     ','D*+     ','D*-     ',
     &  'AD*0    ','F*+     ','F*-     ','PSI     ','JPSI    ',
     &  'TAU+    ','TAU-    ','NUET    ','ANUET   ','NUEM    ',
     &  'ANUEM   ','C0+     ','A+      ','A0      ','C1++    ',
     &  'C1+     ','C10     ','S+      ','S0      ','T0      ',
     &  'XU++    ','XD+     ','XS+     ','AC0-    ','AA-     ',
     &  'AA0     ','AC1--   ','AC1-    ','AC10    ','AS-     ',
     &  'AS0     ','AT0     ','AXU--   ','AXD-    ','AXS     ',
     &  'C1*++   ','C1*+    ','C1*0    ','S*+     ','S*0     ',
     &  'T*0     ','XU*++   ','XD*+    ','XS*+    ','TETA++  '
     &           /
      DATA (BNAME(K),K=171,187) /
     &  'AC1*--  ','AC1*-   ','AC1*0   ','AS*-    ','AS*0    ',
     &  'AT*0    ','AXU*--  ','AXD*-   ','AXS*-   ','ATET--  ',
     &  'RO      ','R+      ','R-      ','Pomeron ','Reggeon ',
     &  'Remnant1','Remnant2'
     &           /
      DATA (BM(K),K=  1, 85) /
     &  0.9383D+00,0.9383D+00,0.5100D-03,0.5100D-03,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.9396D+00,0.9396D+00,0.1057D+00,
     &  0.1057D+00,0.4977D+00,0.1396D+00,0.1396D+00,0.4936D+00,
     &  0.4936D+00,0.1116D+01,0.1116D+01,0.4977D+00,0.1197D+01,
     &  0.1189D+01,0.1193D+01,0.1350D+00,0.4977D+00,0.4977D+00,
     &  0.1420D+01,0.1465D+01,0.1622D+01,0.1680D+01,0.1720D+01,
     &  0.5474D+00,0.7669D+00,0.7700D+00,0.7669D+00,0.7820D+00,
     &  0.8921D+00,0.8962D+00,0.8921D+00,0.8962D+00,0.1300D+01,
     &  0.1300D+01,0.1300D+01,0.1300D+01,0.1421D+01,0.1421D+01,
     &  0.1421D+01,0.1421D+01,0.1383D+01,0.1384D+01,0.1387D+01,
     &  0.1820D+01,0.2030D+01,0.1231D+01,0.1232D+01,0.1233D+01,
     &  0.1234D+01,0.1675D+01,0.1675D+01,0.1675D+01,0.1675D+01,
     &  0.1450D+01,0.1450D+01,0.1515D+01,0.1515D+01,0.1775D+01,
     &  0.1775D+01,0.1231D+01,0.1232D+01,0.1233D+01,0.1234D+01,
     &  0.1675D+01,0.1675D+01,0.1675D+01,0.1675D+01,0.1515D+01,
     &  0.1515D+01,0.1450D+01,0.1450D+01,0.1775D+01,0.1775D+01,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00
     &           /
      DATA (BM(K),K= 86,170) /
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.2030D+01,0.9575D+00,
     &  0.1019D+01,0.1315D+01,0.1321D+01,0.1189D+01,0.1193D+01,
     &  0.1197D+01,0.1315D+01,0.1321D+01,0.1383D+01,0.1384D+01,
     &  0.1387D+01,0.1532D+01,0.1535D+01,0.1672D+01,0.1383D+01,
     &  0.1384D+01,0.1387D+01,0.1532D+01,0.1535D+01,0.1672D+01,
     &  0.1865D+01,0.1869D+01,0.1869D+01,0.1865D+01,0.1969D+01,
     &  0.1969D+01,0.2980D+01,0.2007D+01,0.2010D+01,0.2010D+01,
     &  0.2007D+01,0.2113D+01,0.2113D+01,0.3686D+01,0.3097D+01,
     &  0.1784D+01,0.1784D+01,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.2285D+01,0.2460D+01,0.2460D+01,0.2452D+01,
     &  0.2453D+01,0.2454D+01,0.2550D+01,0.2550D+01,0.2730D+01,
     &  0.3610D+01,0.3610D+01,0.3790D+01,0.2285D+01,0.2460D+01,
     &  0.2460D+01,0.2452D+01,0.2453D+01,0.2454D+01,0.2550D+01,
     &  0.2550D+01,0.2730D+01,0.3610D+01,0.3610D+01,0.3790D+01,
     &  0.2500D+01,0.2500D+01,0.2500D+01,0.2630D+01,0.2630D+01,
     &  0.2800D+01,0.3670D+01,0.3670D+01,0.3850D+01,0.4890D+01
     &           /
      DATA (BM(K),K=171,187) /
     &  0.2500D+01,0.2500D+01,0.2500D+01,0.2630D+01,0.2630D+01,
     &  0.2800D+01,0.3670D+01,0.3670D+01,0.3850D+01,0.4890D+01,
     &  0.1250D+01,0.1250D+01,0.1250D+01,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.8000D+00
     &           /
      DATA (BA(K),K=  1, 85) /
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.1740D+00,0.3100D+00,0.2800D+00,0.1500D+00,0.2400D+00,
     &  0.8500D-06,0.1520D+00,0.1520D+00,0.1520D+00,0.1000D-01,
     &  0.7900D-01,0.7900D-01,0.7900D-01,0.7900D-01,0.4500D+00,
     &  0.4500D+00,0.4500D+00,0.4500D+00,0.1080D+00,0.1080D+00,
     &  0.1080D+00,0.1080D+00,0.5000D-01,0.5000D-01,0.5000D-01,
     &  0.8500D-01,0.1800D+00,0.1150D+00,0.1150D+00,0.1150D+00,
     &  0.1150D+00,0.2000D+00,0.2000D+00,0.2000D+00,0.2000D+00,
     &  0.3500D+00,0.3500D+00,0.1000D+00,0.1000D+00,0.2000D+00,
     &  0.2000D+00,0.1150D+00,0.1150D+00,0.1150D+00,0.1150D+00,
     &  0.2000D+00,0.2000D+00,0.2000D+00,0.2000D+00,0.1000D+00,
     &  0.1000D+00,0.3500D+00,0.3500D+00,0.2000D+00,0.2000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00
     &           /
      DATA (BA(K),K= 86,170) /
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.2000D-02,
     &  0.4000D-02,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.3400D-01,0.3400D-01,
     &  0.3600D-01,0.9000D-02,0.9000D-02,0.0000D+00,0.3400D-01,
     &  0.3400D-01,0.3600D-01,0.9000D-02,0.9000D-02,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.5000D-02,0.2000D-02,0.2000D-02,
     &  0.5000D-02,0.2000D-02,0.2000D-02,0.2000D-03,0.7000D-03,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00
     &           /
      DATA (BA(K),K=171,187) /
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.3000D+00,0.3000D+00,0.3000D+00,0.3000D+00,0.3000D+00,
     &  0.3000D+00,0.3000D+00
     &           /
      DATA (BAU(K),K=  1, 85) /
     &  0.1000D+19,0.1000D+19,0.1000D+19,0.1000D+19,0.1000D+19,
     &  0.1000D+19,0.1000D+19,0.9180D+03,0.9180D+03,0.2200D-05,
     &  0.2200D-05,0.5200D-07,0.2600D-07,0.2600D-07,0.1200D-07,
     &  0.1200D-07,0.2600D-09,0.2600D-09,0.9000D-10,0.1500D-09,
     &  0.8000D-10,0.5000D-14,0.8000D-16,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00
     &           /
      DATA (BAU(K),K= 86,170) /
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.3000D-09,0.1700D-09,0.8000D-10,0.1000D-13,
     &  0.1500D-09,0.3000D-09,0.1700D-09,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.1000D-09,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.1000D-09,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.9000D-11,0.9000D-11,0.9000D-11,0.9000D-11,0.1000D+19,
     &  0.1000D+19,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00
     &           /
      DATA (BAU(K),K=171,187) /
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00
     &           /
      DATA (IBCH(K),K=  1,170) /
     &    1, -1, -1,  1,  0,  0,  0,  0,  0,  1,
     &   -1,  0,  1, -1,  1, -1,  0,  0,  0, -1,
     &    1,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &    0,  1,  0, -1,  0,  1,  0, -1,  0,  1,
     &    0, -1,  0,  1,  0, -1,  0,  1,  0, -1,
     &    0,  0,  2,  1,  0, -1,  2,  1,  0, -1,
     &    1,  0,  1,  0,  1,  0, -2, -1,  0,  1,
     &   -2, -1,  0,  1, -1,  0, -1,  0, -1,  0,
     &    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &    0,  0,  0,  0,  0,  0,  0, -1, -1,  0,
     &    1,  0,  1,  1,  0, -1,  0, -1, -1, -1,
     &    0,  1,  0,  1,  1,  0,  1, -1,  0,  1,
     &   -1,  0,  0,  1, -1,  0,  1, -1,  0,  0,
     &    1, -1,  0,  0,  0,  0,  1,  1,  0,  2,
     &    1,  0,  1,  0,  0,  2,  1,  1, -1, -1,
     &    0, -2, -1,  0, -1,  0,  0, -2, -1, -1,
     &    2,  1,  0,  1,  0,  0,  2,  1,  1,  2
     &           /
      DATA (IBCH(K),K=171,187) /
     &   -2, -1,  0, -1,  0,  0, -2, -1, -1, -2,
     &    0,  1, -1,  0,  0,  0,  0
     &           /
      DATA (IBBAR(K),K=  1,170) /
     &    1, -1,  0,  0,  0,  0,  0,  1, -1,  0,
     &    0,  0,  0,  0,  0,  0,  1, -1,  0,  1,
     &    1,  1,  0,  0,  0,  0,  0,  0,  0,  0,
     &    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &    0,  0,  0,  0,  0,  0,  0,  1,  1,  1,
     &    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &    1,  1,  1,  1,  1,  1, -1, -1, -1, -1,
     &   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     &    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &    0,  0,  0,  0,  0,  0,  1,  1, -1, -1,
     &   -1, -1, -1,  1,  1,  1,  1,  1,  1, -1,
     &   -1, -1, -1, -1, -1,  0,  0,  0,  0,  0,
     &    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &    0,  0,  0,  0,  0,  0,  1,  1,  1,  1,
     &    1,  1,  1,  1,  1,  1,  1,  1, -1, -1,
     &   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     &    1,  1,  1,  1,  1,  1,  1,  1,  1,  1
     &           /
      DATA (IBBAR(K),K=171,187) /
     &   -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     &    0,  0,  0,  0,  0,  0,  0
     &           /
      DATA (KB1(K),K=  1,170) /
     &     1,   2,   3,   4,   5,   6,   7,   8,   9,  10,
     &    11,  12,  16,  17,  18,  24,  30,  34,  38,  40,
     &    41,  43,  44, 136, 138, 308, 311, 313, 316, 318,
     &    46,  51,  52,  54,  55,  58,  60,  62,  64,  66,
     &    68,  70,  72,  74,  82,  90,  98, 106, 109, 112,
     &   114, 123, 140, 141, 143, 145, 146, 150, 157, 164,
     &   168, 319, 180, 187, 194, 202, 210, 211, 213, 215,
     &   216, 220, 227, 234, 238, 245, 252, 262, 272, 280,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0, 331, 335, 340, 341, 342, 344,
     &   345, 346, 347, 348, 351, 354, 357, 359, 361, 364,
     &   367, 370, 373, 375, 377, 380, 384, 386, 388, 392,
     &   395, 398, 401, 403, 406, 409, 411, 413, 415, 418,
     &   421, 426, 431, 432, 433, 434, 435, 449, 453, 458,
     &   459, 460, 461, 462, 463, 467, 469, 471, 473, 487,
     &   491, 496, 497, 498, 499, 500, 501, 505, 507, 509,
     &   511, 512, 513, 514, 515, 516, 517, 518, 519, 520
     &           /
      DATA (KB1(K),K=171,187) /
     &   523, 524, 525, 526, 527, 528, 529, 530, 531, 532,
     &   535, 538, 540,  52,  52,  51,  54
     &           /
      DATA (KB2(K),K=  1,170) /
     &     1,   2,   3,   4,   5,   6,   7,   8,   9,  10,
     &    11,  15,  16,  17,  23,  29,  31,  35,  39,  40,
     &    42,  43,  45, 137, 139, 310, 312, 315, 317, 318,
     &    50,  51,  53,  54,  57,  59,  61,  63,  65,  67,
     &    69,  71,  73,  81,  89,  97, 105, 108, 111, 113,
     &   122, 135, 140, 142, 144, 145, 149, 156, 163, 167,
     &   177, 328, 186, 193, 201, 209, 210, 212, 214, 215,
     &   219, 226, 233, 237, 244, 251, 261, 271, 279, 287,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0, 334, 339, 340, 341, 343, 344,
     &   345, 346, 347, 350, 353, 356, 358, 360, 363, 366,
     &   369, 372, 374, 376, 379, 383, 385, 387, 391, 394,
     &   397, 400, 402, 405, 408, 410, 412, 414, 417, 420,
     &   425, 430, 431, 432, 433, 434, 448, 452, 457, 458,
     &   459, 460, 461, 462, 466, 468, 470, 472, 486, 490,
     &   495, 496, 497, 498, 499, 500, 504, 506, 508, 510,
     &   511, 512, 513, 514, 515, 516, 517, 518, 519, 522
     &           /
      DATA (KB2(K),K=171,187) /
     &   523, 524, 525, 526, 527, 528, 529, 530, 531, 534,
     &   537, 539, 541,  53,  53,  51,  54
     &           /
      DATA (ZBNAM(K),K=  1, 85) /
     &  'P       ','AP      ','E-      ','E+      ','NUE     ',
     &  'ANUE    ','GAM     ','PE-NUE  ','APEANU  ','EANUNU  ',
     &  'E-NUAN  ','3PI0    ','PI+-0   ','PIMUNU  ','PIE-NU  ',
     &  'MU+NUE  ','MU-NUE  ','MU+NUE  ','PI+PI0  ','PI++-   ',
     &  'PI+00   ','M+P0NU  ','E+P0NU  ','MU-NU   ','PI-0    ',
     &  'PI+--   ','PI-00   ','M-P0NU  ','E-P0NU  ','PPI-    ',
     &  'NPI0    ','PD-NUE  ','PM-NUE  ','APPI+   ','ANPI0   ',
     &  'APE+NU  ','APM+NU  ','PI+PI-  ','PI0PI0  ','NPI-    ',
     &  'PPI0    ','NPI+    ','LAGA    ','GAGA    ','GAE+E-  ',
     &  'GAGA    ','GAGAP0  ','PI000   ','PI+-0   ','PI+-GA  ',
     &  'PI+0    ','PI+-    ','PI00    ','PI-0    ','PI+-0   ',
     &  'PI+-    ','PI0GA   ','K+PI0   ','K0PI+   ','KOPI0   ',
     &  'K+PI-   ','K-PI0   ','AK0PI-  ','AK0PI0  ','K-PI+   ',
     &  'K+PI0   ','K0PI+   ','K0PI0   ','K+PI-   ','K-PI0   ',
     &  'K0PI-   ','AK0PI0  ','K-PI+   ','K+PI0   ','K0PI+   ',
     &  'K+89P0  ','K08PI+  ','K+RO77  ','K0RO+7  ','K+OM07  ',
     &  'K+E055  ','K0PI0   ','K+PI+   ','K089P0  ','K+8PI-  '
     &           /
      DATA (ZBNAM(K),K= 86,170) /
     &  'K0R077  ','K+R-77  ','K+R-77  ','K0OM07  ','K0E055  ',
     &  'K-PI0   ','K0PI-   ','K-89P0  ','AK08P-  ','K-R077  ',
     &  'AK0R-7  ','K-OM07  ','K-E055  ','AK0PI0  ','K-PI+   ',
     &  'AK08P0  ','K-8PI+  ','AK0R07  ','AK0OM7  ','AK0E05  ',
     &  'LA0PI+  ','SI0PI+  ','SI+PI0  ','LA0PI0  ','SI+PI-  ',
     &  'SI-PI+  ','LA0PI-  ','SI0PI-  ','NEUAK0  ','PK-     ',
     &  'SI+PI-  ','SI0PI0  ','SI-PI+  ','LA0ET0  ','S+1PI-  ',
     &  'S-1PI+  ','SO1PI0  ','NEUAK0  ','PK-     ','LA0PI0  ',
     &  'LA0OM0  ','LA0RO0  ','SI+RO-  ','SI-RO+  ','SI0RO0  ',
     &  'LA0ET0  ','SI0ET0  ','SI+PI-  ','SI-PI+  ','SI0PI0  ',
     &  'K0S     ','K0L     ','K0S     ','K0L     ','P PI+   ',
     &  'P PI0   ','N PI+   ','P PI-   ','N PI0   ','N PI-   ',
     &  'P PI+   ','N*#PI0  ','N*+PI+  ','PRHO+   ','P PI0   ',
     &  'N PI+   ','N*#PI-  ','N*+PI0  ','N*0PI+  ','PRHO0   ',
     &  'NRHO+   ','P PI-   ','N PI0   ','N*+PI-  ','N*0PI0  ',
     &  'N*-PI+  ','PRHO-   ','NRHO0   ','N PI-   ','N*0PI-  ',
     &  'N*-PI0  ','NRHO-   ','P PI0   ','N PI+   ','P2PI0   '
     &           /
      DATA (ZBNAM(K),K=171,255) /
     &  'PPI+-   ','N*#PI-  ','N*+PI0  ','N*0PI+  ','PRHO0   ',
     &  'NRHO+   ','PETA    ','BLANC   ','BLANC   ','P PI0   ',
     &  'N PI+   ','N*#PI-  ','N*+PI0  ','N*0PI+  ','PRHO0   ',
     &  'NRHO+   ','P PI-   ','N PI0   ','N*+PI-  ','N*0PI0  ',
     &  'N*-PI+  ','PRHO-   ','NRHO0   ','P PI0   ','N PI+   ',
     &  'PRHO0   ','NRHO+   ','LAMK+   ','S+ K0   ','S0 K+   ',
     &  'PETA0   ','P PI-   ','N PI0   ','PRHO-   ','NRHO0   ',
     &  'LAMK0   ','S0 K0   ','S- K+   ','NETA/   ','APPI-   ',
     &  'APPI0   ','ANPI-   ','APPI+   ','ANPI0   ','ANPI+   ',
     &  'APPI-   ','AN*=P0  ','AN*-P-  ','APRHO-  ','APPI0   ',
     &  'ANPI-   ','AN*=P+  ','AN*-P0  ','AN*0P-  ','APRHO0  ',
     &  'ANRHO-  ','APPI+   ','ANPI0   ','AN*-P+  ','AN*0P0  ',
     &  'AN*+P-  ','APRHO+  ','ANRHO0  ','ANPI+   ','AN*0P+  ',
     &  'AN*+P0  ','ANRHO+  ','APPI0   ','ANPI-   ','AN*=P+  ',
     &  'AN*-P0  ','AN*0P-  ','APRHO0  ','ANRHO-  ','APPI+,  ',
     &  'ANPI0   ','AN*-P+  ','AN*0P0  ','AN*+P-  ','APRHO+  ',
     &  'ANRHO0  ','AP PI0  ','AN PI-  ','AP2PI0  ','APPI+-  '
     &           /
      DATA (ZBNAM(K),K=256,340) /
     &  'AN*#PI+ ','AN*+PI0 ','AN*0PI- ','APRHO0  ','ANRHO+  ',
     &  'APETA   ','AN PI0  ','AP PI+  ','AN2PI0  ','ANPI+-  ',
     &  'AN*+PI+ ','AN*0PI0 ','AN*-PI+ ','ANRHO0  ','APRHO+  ',
     &  'ANETA   ','AP PI0  ','AN PI-  ','APRHO0  ','ANRHO-  ',
     &  'ALAMK-  ','AS-AK0  ','AS0K-   ','APETA0  ','AP PI+  ',
     &  'AN PI0  ','APRHO+  ','ANRHO0  ','ALAMK0  ','AS0AK0  ',
     &  'AS+ K-  ','ANETA/  ','BLANC   ','BLANC   ','BLANC   ',
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','BLANC   ',
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','BLANC   ',
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','BLANC   ',
     &  'BLANC   ','BLANC   ','OMOP0   ','OMOP+   ','OMOP-   ',
     &  'RPI+-   ','RPI00   ','OMOP0   ','OMOP+   ','OMOP-   ',
     &  'PHKS1   ','PHKS2   ','ROMPI   ','N PI0   ','P PI-   ',
     &  'N2PI0   ','NPI+-   ','N*-PI+  ','N*0PI0  ','N*+PI-  ',
     &  'NRHO0   ','PRHO-   ','NETA    ','BLANC   ','BLANC   ',
     &  'EPI+-   ','EPI00   ','GAPI+-  ','GAGA*   ','K+-     ',
     &  'KLKS    ','RHOPI   ','PI+-0   ','EGA     ','LPI0    '
     &           /
      DATA (ZBNAM(K),K=341,425) /
     &  'LPI     ','APPI0   ','ANPI-   ','ALAGA   ','ANPI    ',
     &  'ALPI0   ','ALPI+   ','LAPI+   ','SI+PI0  ','SI0PI+  ',
     &  'LAPI0   ','SI+PI-  ','SI-PI+  ','LAPI-   ','SI-PI0  ',
     &  'SI0PI-  ','TE0PI0  ','TE-PI+  ','TE0PI-  ','TE-PI0  ',
     &  'TE0PI   ','TE-PI   ','LAK-    ','ALPI-   ','AS-PI0  ',
     &  'AS0PI-  ','ALPI0   ','AS+PI-  ','AS-PI+  ','ALPI+   ',
     &  'AS+PI0  ','AS0PI+  ','AT0PI0  ','AT+PI-  ','AT0PI+  ',
     &  'AT+PI0  ','AT0PI   ','AT+PI   ','ALK+    ','K-PI+   ',
     &  'K-PI+0  ','K0PI+-  ','K0PI0   ','K-PI++  ','AK0PI+  ',
     &  'K+PI--  ','K0PI-   ','K+PI-   ','K+PI-0  ','AKPI-+  ',
     &  'AK0PI0  ','ETAPIF  ','K++-    ','K+AK0   ','ETAPI-  ',
     &  'K--+    ','K-K0    ','PI00    ','PI+-    ','GAGA    ',
     &  'D0PI0   ','D0GA    ','D0PI+   ','D+PI0   ','DFGA    ',
     &  'AD0PI-  ','D-PI0   ','D-GA    ','AD0PI0  ','AD0GA   ',
     &  'F+GA    ','F+GA    ','F-GA    ','F-GA    ','PSPI+-  ',
     &  'PSPI00  ','PSETA   ','E+E-    ','MUE+-   ','PI+-0   ',
     &  'M+NN    ','E+NN    ','RHO+NT  ','PI+ANT  ','K*+ANT  '
     &           /
      DATA (ZBNAM(K),K=426,510) /
     &  'M-NN    ','E-NN    ','RHO-NT  ','PI-NT   ','K*-NT   ',
     &  'NUET    ','ANUET   ','NUEM    ','ANUEM   ','SI+ETA  ',
     &  'SI+ET*  ','PAK0    ','TET0K+  ','SI*+ET  ','N*+AK0  ',
     &  'N*++K-  ','LAMRO+  ','SI0RO+  ','SI+RO0  ','SI+OME  ',
     &  'PAK*0   ','N*+AK*  ','N*++K*  ','SI+AK0  ','TET0PI  ',
     &  'SI+AK*  ','TET0RO  ','SI0AK*  ','SI+K*-  ','TET0OM  ',
     &  'TET-RO  ','SI*0AK  ','C0+PI+  ','C0+PI0  ','C0+PI-  ',
     &  'A+GAM   ','A0GAM   ','TET0AK  ','TET0K*  ','OM-RO+  ',
     &  'OM-PI+  ','C1++AK  ','A+PI+   ','C0+AK0  ','A0PI+   ',
     &  'A+AK0   ','T0PI+   ','ASI-ET  ','ASI-E*  ','APK0    ',
     &  'ATET0K  ','ASI*-E  ','AN*-K0  ','AN*--K  ','ALAMRO  ',
     &  'ASI0RO  ','ASI-RO  ','ASI-OM  ','APK*0   ','AN*-K*  ',
     &  'AN*--K  ','ASI-K0  ','ATETPI  ','ASI-K*  ','ATETRO  ',
     &  'ASI0K*  ','ASI-K*  ','ATE0OM  ','ATE+RO  ','ASI*0K  ',
     &  'AC-PI-  ','AC-PI0  ','AC-PI+  ','AA-GAM  ','AA0GAM  ',
     &  'ATET0K  ','ATE0K*  ','AOM+RO  ','AOM+PI  ','AC1--K  ',
     &  'AA-PI-  ','AC0-K0  ','AA0PI-  ','AA-K0   ','AT0PI-  '
     &           /
      DATA (ZBNAM(K),K=511,550) /
     &  'C1++GA  ','C1++GA  ','C10GAM  ','S+GAM   ','S0GAM   ',
     &  'T0GAM   ','XU++GA  ','XD+GAM  ','XS+GAM  ','A+AKPI  ',
     &  'T02PI+  ','C1++2K  ','AC1--G  ','AC1-GA  ','AC10GA  ',
     &  'AS-GAM  ','AS0GAM  ','AT0GAM  ','AXU--G  ','AXD-GA  ',
     &  'AXS-GA  ','AA-KPI  ','AT02PI  ','AC1--K  ','RH-PI+  ',
     &  'RH+PI-  ','RH3PI0  ','RH0PI+  ','RH+PI0  ','RH0PI-  ',
     &  'RH-PI0  ','BLANC   ','BLANC   ','BLANC   ','BLANC   ',
     &  'BLANC   ','BLANC   ','BLANC   ','BLANC   ','BLANC   '
     &           /
      DATA (NBK1(K),K=  1,170) /
     &     1,   2,   3,   4,   5,   6,   7,   1,   2,   4,
     &     3,  23,  13,  13,  13,  10,  11,  10,  13,  13,
     &    13,  10,   4,  11,  14,  14,  14,  11,   3,   1,
     &     8,   1,   1,   2,   9,   2,   2,  13,  23,   8,
     &     1,   8,  17,   7,   7,   7,  23,  23,  13,  13,
     &    13,  13,  23,  14,  13,  13,  23,  15,  24,  24,
     &    15,  16,  25,  25,  16,  15,  24,  24,  15,  16,
     &    24,  25,  16,  15,  24,  36,  37,  15,  24,  15,
     &    15,  24,  15,  37,  36,  24,  15,  24,  24,  16,
     &    24,  38,  39,  16,  25,  16,  16,  25,  16,  39,
     &    38,  25,  16,  25,  25,  17,  22,  21,  17,  21,
     &    20,  17,  22,   8,   1,  21,  22,  20,  17,  48,
     &    50,  49,   8,   1,  17,  17,  17,  21,  20,  22,
     &    17,  22,  21,  20,  22,  19,  12,  19,  12,   1,
     &     1,   8,   1,   8,   8,   1,  53,  54,   1,   1,
     &     8,  53,  54,  55,   1,   8,   1,   8,  54,  55,
     &    56,   1,   8,   8,  55,  56,   8,   1,   8,   1
     &           /
      DATA (NBK1(K),K=171,340) /
     &     1,  53,  54,  55,   1,   8,   1,   0,   0,   1,
     &     8,  53,  54,  55,   1,   8,   1,   8,  54,  55,
     &    56,   1,   8,   1,   8,   1,   8,  17,  21,  22,
     &     1,   1,   8,   1,   8,  17,  22,  20,   8,   2,
     &     2,   9,   2,   9,   9,   2,  67,  68,   2,   2,
     &     9,  67,  68,  69,   2,   9,   2,   9,  68,  69,
     &    70,   2,   9,   9,  69,  70,   9,   2,   9,  67,
     &    68,  69,   2,   9,   2,   9,  68,  69,  70,   2,
     &     9,   2,   9,   2,   2,  67,  68,  69,   2,   9,
     &     2,   9,   2,   9,   9,  70,  69,  68,   9,   2,
     &     9,   2,   9,   2,   9,  18,  99, 100,   2,   2,
     &     9,   2,   9,  18, 100, 101,   9,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,  33,  34,  32,
     &    13,  23,  33,  34,  32,  24,  25,  33,   8,   1,
     &     8,   8,  56,  55,  54,   8,   1,   8,   0,   0,
     &    31,  31,  13,   7,  15,  12,  33,  13,  31,  17
     &           /
      DATA (NBK1(K),K=341,510) /
     &    17,   2,   9,  18,   9,  18,  18,  17,  21,  22,
     &    17,  21,  20,  17,  20,  22,  97,  98,  97,  98,
     &    97,  98,  17,  18,  99, 100,  18, 101,  99,  18,
     &   101, 100, 102, 103, 102, 103, 102, 103,  18,  16,
     &    16,  24,  24,  16,  25,  15,  24,  15,  15,  25,
     &    25,  31,  15,  15,  31,  16,  16,  23,  13,   7,
     &   116, 116, 116, 117, 117, 119, 118, 118, 119, 119,
     &   120, 120, 121, 121, 130, 130, 130,   4,  10,  13,
     &    10,   4,  32,  13,  36,  11,   3,  34,  14,  38,
     &   133, 134, 135, 136,  21,  21,   1,  97, 104,  54,
     &    53,  17,  22,  21,  21,   1,  54,  53,  21,  97,
     &    21,  97,  22,  21,  97,  98, 105, 137, 137, 137,
     &   138, 139,  97,  97, 109, 109, 140, 138, 137, 139,
     &   138, 145,  99,  99,   2, 102, 110,  68,  67,  18,
     &   100,  99,  99,   2,  68,  67,  99, 102,  99, 102,
     &   100,  99, 102, 103, 111, 149, 149, 149, 150, 151,
     &   113, 113, 115, 115, 152, 150, 149, 151, 150, 157
     &           /
      DATA (NBK1(K),K=511,550) /
     &   140, 141, 142, 143, 144, 145, 146, 147, 148, 138,
     &   145, 140, 152, 153, 154, 155, 156, 157, 158, 159,
     &   160, 150, 157, 152,  34,  32,  33,  33,  32,  33,
     &    34,   0,   0,   0,   0,   0,   0,   0,   0,   0
     &           /
      DATA (NBK2(K),K=  1,170) /
     &     0,   0,   0,   0,   0,   0,   0,   3,   4,   6,
     &     5,  23,  14,  11,   3,   5,   5,   5,  23,  13,
     &    23,  23,  23,   5,  23,  13,  23,  23,  23,  14,
     &    23,   3,  11,  13,  23,   4,  10,  14,  23,  14,
     &    23,  13,   7,   7,   4,   7,   7,  23,  14,  14,
     &    23,  14,  23,  23,  14,  14,   7,  23,  13,  23,
     &    14,  23,  14,  23,  13,  23,  13,  23,  14,  23,
     &    14,  23,  13,  23,  13,  23,  13,  33,  32,  35,
     &    31,  23,  14,  23,  14,  33,  34,  35,  31,  23,
     &    14,  23,  14,  33,  34,  35,  31,  23,  13,  23,
     &    13,  33,  32,  35,  31,  13,  13,  23,  23,  14,
     &    13,  14,  14,  25,  16,  14,  23,  13,  31,  14,
     &    13,  23,  25,  16,  23,  35,  33,  34,  32,  33,
     &    31,  31,  14,  13,  23,   0,   0,   0,   0,  13,
     &    23,  13,  14,  23,  14,  13,  23,  13,  32,  23,
     &    13,  14,  23,  13,  33,  32,  14,  23,  14,  23,
     &    13,  34,  33,  14,  14,  23,  34,  23,  13,  23
     &           /
      DATA (NBK2(K),K=171,340) /
     &    13,  14,  23,  13,  33,  32,  31,   0,   0,  23,
     &    13,  14,  23,  13,  33,  32,  14,  23,  14,  23,
     &    13,  34,  33,  23,  13,  33,  32,  15,  24,  15,
     &    31,  14,  23,  34,  33,  24,  24,  15,  31,  14,
     &    23,  14,  13,  23,  13,  14,  23,  14,  34,  23,
     &    14,  13,  23,  14,  33,  34,  13,  23,  13,  23,
     &    14,  32,  33,  13,  13,  23,  32,  23,  14,  13,
     &    23,  14,  33,  34,  13,  23,  13,  23,  14,  32,
     &    33,  23,  14,  23,  13,  13,  23,  14,  33,  34,
     &    31,  23,  13,  23,  13,  14,  23,  13,  33,  32,
     &    31,  23,  14,  33,  34,  16,  25,  16,  31,  13,
     &    23,  32,  33,  24,  25,  16,  31,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,  23,  13,  14,
     &    14,  23,  23,  13,  14,  39,  37,  13,  23,  14,
     &    23,  13,  13,  23,  14,  33,  34,  31,   0,   0,
     &    13,  23,  14,   7,  16,  19,  23,  14,   7,  23
     &           /
      DATA (NBK2(K),K=341,510) /
     &    14,  23,  14,   7,  13,  23,  13,  13,  23,  13,
     &    23,  14,  13,  14,  23,  14,  23,  13,  14,  23,
     &    14,  23,  16,  14,  23,  14,  23,  14,  13,  13,
     &    23,  13,  23,  14,  13,  23,  13,  23,  15,  13,
     &    13,  13,  23,  13,  13,  14,  14,  14,  14,  14,
     &    23,  13,  16,  25,  14,  15,  24,  23,  14,   7,
     &    23,   7,  13,  23,   7,  14,  23,   7,  23,   7,
     &     7,   7,   7,   7,  13,  23,  31,   3,  11,  14,
     &   135,   5, 134, 134, 134, 136,   6, 133, 133, 133,
     &     0,   0,   0,   0,  31,  95,  25,  15,  31,  95,
     &    16,  32,  32,  33,  35,  39,  39,  38,  25,  13,
     &    39,  32,  39,  38,  35,  32,  39,  13,  23,  14,
     &     7,   7,  25,  37,  32,  13,  25,  13,  25,  13,
     &    25,  13,  31,  95,  24,  16,  31,  24,  15,  34,
     &    34,  33,  35,  37,  37,  36,  24,  14,  37,  34,
     &    37,  36,  35,  34,  37,  14,  23,  13,   7,   7,
     &    24,  39,  34,  14,  24,  14,  24,  14,  24,  14
     &           /
      DATA (NBK2(K),K=511,550) /
     &     7,   7,   7,   7,   7,   7,   7,   7,   7,  25,
     &    13,  25,   7,   7,   7,   7,   7,   7,   7,   7,
     &     7,  24,  14,  24,  13,  14,  23,  13,  23,  14,
     &    23,   0,   0,   0,   0,   0,   0,   0,   0,   0
     &           /
      DATA (NBK3(K),K=  1,170) /
     &     0,   0,   0,   0,   0,   0,   0,   5,   6,   5,
     &     6,  23,  23,   5,   5,   0,   0,   0,   0,  14,
     &    23,   5,   5,   0,   0,  14,  23,   5,   5,   0,
     &     0,   5,   5,   0,   0,   5,   5,   0,   0,   0,
     &     0,   0,   0,   0,   3,   0,   7,  23,  23,   7,
     &     0,   0,   0,   0,  23,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,  23
     &           /
      DATA (NBK3(K),K=171,340) /
     &    14,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,  23,  14,   0,   0,   0,   0,   0,
     &     0,   0,   0,  23,  14,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,  14,   0,   0,
     &    23,  14,   0,   0,   0,   0,   0,   0,   0,   0,
     &    14,  23,   7,   0,   0,   0,   0,  23,   0,   0
     &           /
      DATA (NBK3(K),K=341,510) /
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &    23,  14,   0,  13,   0,  14,   0,   0,  23,  13,
     &     0,   0,  15,   0,   0,  16,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,  14,  23,   0,   0,   0,  23,
     &   134, 134,   0,   0,   0, 133, 133,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0
     &           /
      DATA (NBK3(K),K=511,550) /
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,  13,
     &    13,  25,   0,   0,   0,   0,   0,   0,   0,   0,
     &     0,  14,  14,  24,   0,   0,   0,   0,   0,   0,
     &     0,   0,   0,   0,   0,   0,   0,   0,   0,   0
     &           /
      DATA (BT(K),K=  1, 85) /
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.2100D+00,0.1200D+00,0.2700D+00,0.4000D+00,
     &  0.1000D+01,0.1000D+01,0.6400D+00,0.2100D+00,0.6000D-01,
     &  0.2000D-01,0.3000D-01,0.4000D-01,0.6400D+00,0.2100D+00,
     &  0.6000D-01,0.2000D-01,0.3000D-01,0.4000D-01,0.6400D+00,
     &  0.3600D+00,0.0000D+00,0.0000D+00,0.6400D+00,0.3600D+00,
     &  0.0000D+00,0.0000D+00,0.6900D+00,0.3100D+00,0.1000D+01,
     &  0.5200D+00,0.4800D+00,0.1000D+01,0.9900D+00,0.1000D-01,
     &  0.3800D+00,0.3000D-01,0.3000D+00,0.2400D+00,0.5000D-01,
     &  0.1000D+01,0.1000D+01,0.0000D+00,0.1000D+01,0.8880D+00,
     &  0.2500D-01,0.8700D-01,0.3300D+00,0.6700D+00,0.3300D+00,
     &  0.6700D+00,0.3300D+00,0.6700D+00,0.3300D+00,0.6700D+00,
     &  0.3300D+00,0.6700D+00,0.3300D+00,0.6700D+00,0.3300D+00,
     &  0.6700D+00,0.3300D+00,0.6700D+00,0.1900D+00,0.3800D+00,
     &  0.9000D-01,0.2000D+00,0.3000D-01,0.4000D-01,0.5000D-01,
     &  0.2000D-01,0.1900D+00,0.3800D+00,0.9000D-01,0.2000D+00
     &           /
      DATA (BT(K),K= 86,170) /
     &  0.3000D-01,0.4000D-01,0.5000D-01,0.2000D-01,0.1900D+00,
     &  0.3800D+00,0.9000D-01,0.2000D+00,0.3000D-01,0.4000D-01,
     &  0.5000D-01,0.2000D-01,0.1900D+00,0.3800D+00,0.9000D-01,
     &  0.2000D+00,0.3000D-01,0.4000D-01,0.5000D-01,0.2000D-01,
     &  0.8800D+00,0.6000D-01,0.6000D-01,0.8800D+00,0.6000D-01,
     &  0.6000D-01,0.8800D+00,0.1200D+00,0.1900D+00,0.1900D+00,
     &  0.1600D+00,0.1600D+00,0.1700D+00,0.3000D-01,0.3000D-01,
     &  0.3000D-01,0.4000D-01,0.1000D+00,0.1000D+00,0.2000D+00,
     &  0.1200D+00,0.1000D+00,0.4000D-01,0.4000D-01,0.5000D-01,
     &  0.7500D-01,0.7500D-01,0.3000D-01,0.3000D-01,0.4000D-01,
     &  0.5000D+00,0.5000D+00,0.5000D+00,0.5000D+00,0.1000D+01,
     &  0.6700D+00,0.3300D+00,0.3300D+00,0.6700D+00,0.1000D+01,
     &  0.2500D+00,0.2700D+00,0.1800D+00,0.3000D+00,0.1700D+00,
     &  0.8000D-01,0.1800D+00,0.3000D-01,0.2400D+00,0.2000D+00,
     &  0.1000D+00,0.8000D-01,0.1700D+00,0.2400D+00,0.3000D-01,
     &  0.1800D+00,0.1000D+00,0.2000D+00,0.2500D+00,0.1800D+00,
     &  0.2700D+00,0.3000D+00,0.1500D+00,0.3500D+00,0.7000D-01
     &           /
      DATA (BT(K),K=171,255) /
     &  0.1800D+00,0.1100D+00,0.6000D-01,0.3000D-01,0.1000D-01,
     &  0.3000D-01,0.1000D-01,0.1000D+01,0.1000D+01,0.1800D+00,
     &  0.3700D+00,0.1300D+00,0.8000D-01,0.4000D-01,0.7000D-01,
     &  0.1300D+00,0.3700D+00,0.1800D+00,0.4000D-01,0.8000D-01,
     &  0.1300D+00,0.1300D+00,0.7000D-01,0.7000D-01,0.1300D+00,
     &  0.2300D+00,0.4700D+00,0.5000D-01,0.2000D-01,0.1000D-01,
     &  0.2000D-01,0.1300D+00,0.7000D-01,0.4700D+00,0.2300D+00,
     &  0.5000D-01,0.1000D-01,0.2000D-01,0.2000D-01,0.1000D+01,
     &  0.6700D+00,0.3300D+00,0.3300D+00,0.6700D+00,0.1000D+01,
     &  0.2500D+00,0.2700D+00,0.1800D+00,0.3000D+00,0.1700D+00,
     &  0.8000D-01,0.1800D+00,0.3000D-01,0.2400D+00,0.2000D+00,
     &  0.1000D+00,0.8000D-01,0.1700D+00,0.2400D+00,0.3000D-01,
     &  0.1800D+00,0.1000D+00,0.2000D+00,0.2500D+00,0.1800D+00,
     &  0.2700D+00,0.3000D+00,0.1800D+00,0.3700D+00,0.1300D+00,
     &  0.8000D-01,0.4000D-01,0.7000D-01,0.1300D+00,0.3700D+00,
     &  0.1800D+00,0.4000D-01,0.8000D-01,0.1300D+00,0.1300D+00,
     &  0.7000D-01,0.1500D+00,0.3500D+00,0.7000D-01,0.1800D+00
     &           /
      DATA (BT(K),K=256,340) /
     &  0.1100D+00,0.6000D-01,0.3000D-01,0.1000D-01,0.3000D-01,
     &  0.1000D-01,0.1500D+00,0.3500D+00,0.7000D-01,0.1800D+00,
     &  0.1100D+00,0.6000D-01,0.3000D-01,0.1000D-01,0.3000D-01,
     &  0.1000D-01,0.7000D-01,0.1300D+00,0.2300D+00,0.4700D+00,
     &  0.5000D-01,0.2000D-01,0.1000D-01,0.2000D-01,0.1300D+00,
     &  0.7000D-01,0.4700D+00,0.2300D+00,0.5000D-01,0.1000D-01,
     &  0.2000D-01,0.2000D-01,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,0.0000D+00,
     &  0.0000D+00,0.0000D+00,0.4000D+00,0.3000D+00,0.3000D+00,
     &  0.6000D+00,0.4000D+00,0.4000D+00,0.3000D+00,0.3000D+00,
     &  0.5000D+00,0.5000D+00,0.1000D+01,0.1500D+00,0.3500D+00,
     &  0.7000D-01,0.1800D+00,0.1100D+00,0.6000D-01,0.3000D-01,
     &  0.1000D-01,0.3000D-01,0.1000D-01,0.1000D+01,0.1000D+01,
     &  0.4800D+00,0.2400D+00,0.2600D+00,0.2000D-01,0.4910D+00,
     &  0.3440D+00,0.1290D+00,0.2400D-01,0.1200D-01,0.1000D+01
     &           /
      DATA (BT(K),K=341,425) /
     &  0.1000D+01,0.5200D+00,0.4800D+00,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.1000D+01,0.9000D+00,0.5000D-01,0.5000D-01,
     &  0.9000D+00,0.5000D-01,0.5000D-01,0.9000D+00,0.5000D-01,
     &  0.5000D-01,0.3300D+00,0.6700D+00,0.6700D+00,0.3300D+00,
     &  0.2500D+00,0.2500D+00,0.5000D+00,0.9000D+00,0.5000D-01,
     &  0.5000D-01,0.9000D+00,0.5000D-01,0.5000D-01,0.9000D+00,
     &  0.5000D-01,0.5000D-01,0.3300D+00,0.6700D+00,0.6700D+00,
     &  0.3300D+00,0.2500D+00,0.2500D+00,0.5000D+00,0.1000D+00,
     &  0.5000D+00,0.1600D+00,0.2400D+00,0.7000D+00,0.3000D+00,
     &  0.7000D+00,0.3000D+00,0.1000D+00,0.5000D+00,0.1600D+00,
     &  0.2400D+00,0.3000D+00,0.4000D+00,0.3000D+00,0.3000D+00,
     &  0.4000D+00,0.3000D+00,0.4900D+00,0.4900D+00,0.2000D-01,
     &  0.5500D+00,0.4500D+00,0.6800D+00,0.3000D+00,0.2000D-01,
     &  0.6800D+00,0.3000D+00,0.2000D-01,0.5500D+00,0.4500D+00,
     &  0.9000D+00,0.1000D+00,0.9000D+00,0.1000D+00,0.6000D+00,
     &  0.3000D+00,0.1000D+00,0.1000D+00,0.1000D+00,0.8000D+00,
     &  0.2800D+00,0.2800D+00,0.3500D+00,0.7000D-01,0.2000D-01
     &           /
      DATA (BT(K),K=426,510) /
     &  0.2800D+00,0.2800D+00,0.3500D+00,0.7000D-01,0.2000D-01,
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.2000D-01,
     &  0.3000D-01,0.7000D-01,0.2000D-01,0.2000D-01,0.4000D-01,
     &  0.1300D+00,0.7000D-01,0.6000D-01,0.6000D-01,0.2000D+00,
     &  0.1400D+00,0.4000D-01,0.1000D+00,0.2500D+00,0.3000D-01,
     &  0.3000D+00,0.4200D+00,0.2200D+00,0.3500D+00,0.1900D+00,
     &  0.1600D+00,0.8000D-01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.1000D+01,0.3700D+00,0.2000D+00,0.3600D+00,
     &  0.7000D-01,0.5000D+00,0.5000D+00,0.5000D+00,0.5000D+00,
     &  0.5000D+00,0.5000D+00,0.2000D-01,0.3000D-01,0.7000D-01,
     &  0.2000D-01,0.2000D-01,0.4000D-01,0.1300D+00,0.7000D-01,
     &  0.6000D-01,0.6000D-01,0.2000D+00,0.1400D+00,0.4000D-01,
     &  0.1000D+00,0.2500D+00,0.3000D-01,0.3000D+00,0.4200D+00,
     &  0.2200D+00,0.3500D+00,0.1900D+00,0.1600D+00,0.8000D-01,
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.3700D+00,0.2000D+00,0.3600D+00,0.7000D-01,0.5000D+00,
     &  0.5000D+00,0.5000D+00,0.5000D+00,0.5000D+00,0.5000D+00
     &           /
      DATA (BT(K),K=511,550) /
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.3000D+00,
     &  0.3000D+00,0.4000D+00,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.3000D+00,0.3000D+00,0.4000D+00,0.3300D+00,
     &  0.3300D+00,0.3400D+00,0.5000D+00,0.5000D+00,0.5000D+00,
     &  0.5000D+00,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,
     &  0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01,0.1000D+01
     &        /
C
C  translation table
      DATA (IBMCIN(K),K=1,100) /
     & 2212,-2212,11,-11,12,              -12,22,2112,-2112,-13,
     & 13,130,211,-211,321,               -321,3122,-3122,310,3112,
     & 3222,3212,111,311,-311,            50223,40113,60223,10333,30113,
     & 221,213,113,-213,223,              323,313,-323,-313,10323,
     & 10313,-10323,-10313,30323,30313,   -30323,-30313,3224,3214,3114,
     & 3216,3218,2224,2214,2114,          1114,12224,12214,12114,11114,
     & 12212,12112,22212,22112,42212,     42112,-2224,-2214,-2114,-1114,
     & -12224,-12214,-12114,-11114,-2124,
     & -1214,-12212,-12112,-42212,-42112,
     & 5*99999,                           5*99999,
     & 4*99999,331,                       333,3322,3312,-3222,-3212 /
      DATA (IBMCIN(K),K=101,187) /
     & -3112,-3322,-3312,3224,3214,       3114,3324,3314,3334,-3224,
     & -3214,-3114,-3324,-3314,-3334,     421,411,-411,-421,431,
     & -431,441,423,413,-413,             -423,433,-433,20443,443,
     & -15,15,16,-16,14,                  -14,4122,4232,4132,4222,
     & 4212,4112,4322,4312,4332,          4422,4412,4432,-4122,-4232,
     & -4132,-4222,-4212,-4112,-4322,     -4312,-4332,-4422,-4412,-4432,
     & 4224,4214,4114,4324,4314,          4334,4424,4414,4434,4444,
     & -4224,-4214,-4114,-4324,-4314,     -4334,-4424,-4414,-4434,-4444,
     & 3*99999,45,46,                     777,888 /
C  index tables
C
C     DEFINE THE FIELDS FOR PARTICLE CLASSIFICATION
C     IMPS=PSEUDO SCALAR MESONS (SPIN=0)
C     IMVE=VECTOR MESONS (SPIN=1)
C     IB08(IA08)=BARYONS (ANTIBARYONS) (SPIN=1/2)
C     IB10(IA10)=BARYONS (ANTIBARYONS) (SPIN=3/2)
C
      DATA IP/
     *23,14,16,116,0,0,13,23,25,117,0,0,15,24,31,120,0,0,119,118,121,
     *122,14*0/
      DATA IV/
     *33,34,38,123,0,0,32,33,39,124,0,0,36,37,96,127,0,0,126,125,128,
     *129,14*0/
      DATA IB/
     *0,1,21,140,0,0,8,22,137,0,0,97,138,0,0,146,5*0,
     *1,8,22,137,0,0,0,20,142,0,0,98,139,0,0,147,5*0,
     *21,22,97,138,0,0,20,98,139,0,0,0,145,0,0,148,5*0,
     *140,137,138,146,0,0,142,139,147,0,0,145,148,50*0/
      DATA IBB/
     *53,54,104,161,0,0,55,105,162,0,0,107,164,0,0,167,5*0,
     *54,55,105,162,0,0,56,106,163,0,0,108,165,0,0,168,5*0,
     *104,105,107,164,0,0,106,108,165,0,0,109,166,0,0,169,5*0,
     *161,162,164,167,0,0,163,165,168,0,0,166,169,0,0,170,47*0/
      DATA IA/
     *0,2,99,152,0,0,9,100,149,0,0,102,150,0,0,158,5*0,
     *2,9,100,149,0,0,0,101,154,0,0,103,151,0,0,159,5*0,
     *99,100,102,150,0,0,101,103,151,0,0,0,157,0,0,160,5*0,
     *152,149,150,158,0,0,154,151,159,0,0,157,160,50*0/
      DATA IAA/
     *67,68,110,171,0,0,69,111,172,0,0,113,174,0,0,177,5*0,
     *68,69,111,172,0,0,70,112,173,0,0,114,175,0,0,178,5*0,
     *110,111,113,174,0,0,112,114,175,0,0,115,176,0,0,179,5*0,
     *171,172,174,177,0,0,173,175,178,0,0,176,179,0,0,180,47*0/
C
C  copy to COMMONs
      DO 100 I=1,187
        ANAME(I) = BNAME(I)
        AM(I)    = BM(I)
        GA(I)    = BA(I)
        TAU(I)   = BAU(I)
        ICH(I)   = IBCH(I)
        IBAR(I)  = IBBAR(I)
        K1(I)    = KB1(I)
        K2(I)    = KB2(I)
        IAMCIN(I)= IBMCIN(I)
 100  CONTINUE
      DO 200 I=1,550
        ZKNAM(I) = ZBNAM(I)
        NZK(I,1)  = NBK1(I)
        NZK(I,2)  = NBK2(I)
        NZK(I,3)  = NBK3(I)
        WT(I)    = BT(I)
 200  CONTINUE
C  initialize index tables
      L=0
      DO 1 I=1,6
        DO 2 J=1,6
          L=L+1
          IMPS(I,J)=IP(L)
    2   CONTINUE
    1 CONTINUE
      L=0
      DO 3 I=1,6
        DO 4 J=1,6
          L=L+1
          IMVE(I,J)=IV(L)
    4   CONTINUE
    3 CONTINUE
      L=0
      DO 5 I=1,6
        DO 6 J=1,21
          L=L+1
          IB08(I,J)=IB(L)
    6   CONTINUE
    5 CONTINUE
      L=0
      DO 7 I=1,6
        DO 8 J=1,21
          L=L+1
          IB10(I,J)=IBB(L)
    8   CONTINUE
    7 CONTINUE
      L=0
      DO 9 I=1,6
        DO 10 J=1,21
          L=L+1
          IA08(I,J)=IA(L)
   10   CONTINUE
    9 CONTINUE
      L=0
      DO 11 I=1,6
        DO 12 J=1,21
          L=L+1
          IA10(I,J)=IAA(L)
   12   CONTINUE
   11 CONTINUE
C
C  BAMJET standard parameters
C  fragmentation function
      A1     = 0.88D0
C  lower energy cut-off (jet)
      B1     = 8.D0
      B2     = 8.D0
C  pt distribution
      B3     = 8.D0
C  octett/decuplett baryons
      B8     = 0.50
C  flavours
      BET    = 9.D0
C  pseudo scalar/vector mesons
      AS     = 0.50
C  meson/baryon vertex
      AME    = 0.93D0
      DIQ    = 0.375D0
C  debug
      LT     = 0
      LB     = 0
C  flavour number
      ISU    = 4
      END
      DOUBLE PRECISION FUNCTION XLAMB(X,Y,Z)
C**********************************************************************
C
C     auxiliary function for two/three particle decay mode
C     (standard LAMBDA**(1/2) function)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      YZ=Y-Z
      XLAM=X*X-2.D0*X*(Y+Z)+YZ*YZ
      IF(XLAM.LT.0.D0) XLAM=-XLAM
      XLAMB=SQRT(XLAM)
      END
C
C
      DOUBLE PRECISION FUNCTION PHBESJ0(DX)
C**********************************************************************
C
C     CERN (KERN) LIB function C312
C
C     modified by R. Engel (03/02/93)
C
C**********************************************************************
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION X,Y,V,H,ALFA,ZERO,ONE,TWO,EIGHT,D
      DOUBLE PRECISION PI1,PI2,C1(0:14),C2(0:9),C3(0:10),B0,B1,B2,P,Q,R
      SAVE

      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, EIGHT /8.0D0/
      DATA PI1 /0.79788 45608 0287D0/, PI2 /0.78539 81633 9745D0/

      DATA C1( 0) /+0.15772 79714 7489D0/
      DATA C1( 1) /-0.00872 34423 5285D0/
      DATA C1( 2) /+0.26517 86132 0334D0/
      DATA C1( 3) /-0.37009 49938 7265D0/
      DATA C1( 4) /+0.15806 71023 3210D0/
      DATA C1( 5) /-0.03489 37694 1141D0/
      DATA C1( 6) /+0.00481 91800 6947D0/
      DATA C1( 7) /-0.00046 06261 6621D0/
      DATA C1( 8) /+0.00003 24603 2882D0/
      DATA C1( 9) /-0.00000 17619 4691D0/
      DATA C1(10) /+0.00000 00760 8164D0/
      DATA C1(11) /-0.00000 00026 7925D0/
      DATA C1(12) /+0.00000 00000 7849D0/
      DATA C1(13) /-0.00000 00000 0194D0/
      DATA C1(14) /+0.00000 00000 0004D0/

      DATA C2( 0) /+0.99946 03493 4752D0/
      DATA C2( 1) /-0.00053 65220 4681D0/
      DATA C2( 2) /+0.00000 30751 8479D0/
      DATA C2( 3) /-0.00000 00517 0595D0/
      DATA C2( 4) /+0.00000 00016 3065D0/
      DATA C2( 5) /-0.00000 00000 7864D0/
      DATA C2( 6) /+0.00000 00000 0517D0/
      DATA C2( 7) /-0.00000 00000 0043D0/
      DATA C2( 8) /+0.00000 00000 0004D0/
      DATA C2( 9) /-0.00000 00000 0001D0/

      DATA C3( 0) /-0.01555 58546 05337D0/
      DATA C3( 1) /+0.00006 83851 99426D0/
      DATA C3( 2) /-0.00000 07414 49841D0/
      DATA C3( 3) /+0.00000 00179 72457D0/
      DATA C3( 4) /-0.00000 00007 27192D0/
      DATA C3( 5) /+0.00000 00000 42201D0/
      DATA C3( 6) /-0.00000 00000 03207D0/
      DATA C3( 7) /+0.00000 00000 00301D0/
      DATA C3( 8) /-0.00000 00000 00033D0/
      DATA C3( 9) /+0.00000 00000 00004D0/
      DATA C3(10) /-0.00000 00000 00001D0/
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))

      X=DX
      V=ABS(X)
      IF(V .LT. EIGHT) THEN
       Y=V/EIGHT
       H=TWO*Y**2-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 1 I = 14,0,-1
       B0=C1(I)-ALFA*B1-B2
       B2=B1
    1  B1=B0
       B1=B0-H*B2
      ELSE
       R=ONE/V
       Y=EIGHT*R
       H=TWO*Y**2-ONE
       ALFA=-TWO*H
       B1=ZERO
       B2=ZERO
       DO 2 I = 9,0,-1
       B0=C2(I)-ALFA*B1-B2
       B2=B1
    2  B1=B0
       P=B0-H*B2
       B1=ZERO
       B2=ZERO
       DO 3 I = 10,0,-1
       B0=C3(I)-ALFA*B1-B2
       B2=B1
    3  B1=B0
       Q=Y*(B0-H*B2)
       B0=V-PI2
       B1=PI1*SQRT(R)*(P*COS(B0)-Q*SIN(B0))
      ENDIF
      PHBESJ0=B1
      RETURN
      END
C
C
       DOUBLE PRECISION FUNCTION PLBEI0(X)
C**********************************************************************
C
C      Bessel Function I0
C
C**********************************************************************
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
       AX = ABS(X)
       IF (AX .LT. 3.75D0) THEN
         Y = (X/3.75D0)**2
         PLBEI0 =
     &     1.0D0+Y*(3.5156229D0+Y*(3.0899424D0+Y*(1.2067492D0
     &     +Y*(0.2659732D0+Y*(0.360768D-1+Y*0.45813D-2)))))
       ELSE
         Y = 3.75D0/AX
         PLBEI0 =
     &     (EXP(AX)/SQRT(AX))*(0.39894228D0+Y*(0.1328592D-1
     &     +Y*(0.225319D-2+Y*(-0.157565D-2+Y*(0.916281D-2
     &     +Y*(-0.2057706D-1+Y*(0.2635537D-1+Y*(-0.1647633D-1
     &     +Y*0.392377D-2))))))))
       ENDIF
       END
C
C
       DOUBLE PRECISION FUNCTION PLBEI1(X)
C**********************************************************************
C
C      Bessel Function I1
C
C**********************************************************************
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
C
       AX = ABS(X)
C
       IF (AX .LT. 3.75D0) THEN
         Y = (X/3.75D0)**2
         BESLI1 =
     &     AX*(0.5D0+Y*(0.87890594D0+Y*(0.51498869D0+Y*(0.15084934D0
     &     +Y*(0.2658733D-1+Y*(0.301532D-2+Y*0.32411D-3))))))
       ELSE
         Y = 3.75D0/AX
         BESLI1 =
     &     0.2282967D-1+Y*(-0.2895312D-1+Y*(0.1787654D-1
     &     -Y*0.420059D-2))
         BESLI1 =
     &     0.39894228D0+Y*(-0.3988024D-1+Y*(-0.362018D-2
     &     +Y*(0.163801D-2+Y*(-0.1031555D-1+Y*BESLI1))))
         BESLI1 = BESLI1 * EXP(AX)/SQRT(AX)
       ENDIF
       IF (X .LT. 0.D0) BESLI1 = -BESLI1
C
       PLBEI1 = BESLI1
       END
C
C
       DOUBLE PRECISION FUNCTION PLBEK0(X)
C**********************************************************************
C
C      Modified Bessel Function K0
C
C**********************************************************************
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
C
       IF (X .LT. 2.D0) THEN
         Y = X**2/4.D0
         PLBEK0 =
     &     (-LOG(X/2.D0)*PLBEI0(X))+(-.57721566D0+Y*(0.42278420D0
     &     +Y*(0.23069756D0+Y*(0.3488590D-1+Y*(0.262698D-2
     &     +Y*(0.10750D-3+Y*0.740D-5))))))
       ELSE
         Y = 2.D0/X
         PLBEK0 =
     &     (EXP(-X)/SQRT(X))*(1.25331414D0+Y*(-0.7832358D-1
     &     +Y*(0.2189568D-1+Y*(-0.1062446D-1+Y*(0.587872D-2
     &     +Y*(-0.251540D-2+Y*0.53208D-3))))))
       ENDIF
       END
C
C
       DOUBLE PRECISION FUNCTION PLBEK1(X)
C**********************************************************************
C
C      Modified Bessel Function K1
C
C**********************************************************************
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)       
C      
       IF (X .LT. 2.D0) THEN
         Y = X**2/4.D0
         PLBEK1 = 
     &     (LOG(X/2.D0)*PLBEI1(X))+(1.D0/X)*(1.D0+Y*(0.15443144D0
     &     +Y*(-0.67278579D0+Y*(-0.18156897D0+Y*(-0.1919402D-1
     &     +Y*(-0.110404D-2+Y*(-0.4686D-4)))))))
       ELSE
         Y=2.D0/X
         PLBEK1 = 
     &     (EXP(-X)/SQRT(X))*(1.25331414D0+Y*(0.23498619D0
     &     +Y*(-0.3655620D-1+Y*(0.1504268D-1+Y*(-0.780353D-2
     &     +Y*(0.325614D-2+Y*(-0.68245D-3)))))))
       ENDIF
       END
C
C
      SUBROUTINE PHGSET(AX,BX,NX,Z,W)
C********************************************************************
C
C     N-point gauss zeros and weights for the interval (AX,BX) are
c           stored in  arrays Z and W respectively.
c
C     N-point center interval integral for numbers not suitable for
C           Gauss integration
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      SAVE
      COMMON /GQCOM/A(273),X(273),KTAB(96)
      DIMENSION Z(NX),W(NX)
      DATA INIT/0/
C
      ALPHA=0.5*(BX+AX)
      BETA=0.5*(BX-AX)
      N=NX
*
*  the N=1 case:
      IF(N.NE.1) GO TO 1
      Z(1)=ALPHA
      W(1)=BX-AX
      RETURN
*
*  the Gauss cases:
    1 IF((N.LE.16).AND.(N.GT.1)) GO TO 2
      IF(N.EQ.20) GO TO 2
      IF(N.EQ.24) GO TO 2
      IF(N.EQ.32) GO TO 2
      IF(N.EQ.40) GO TO 2
      IF(N.EQ.48) GO TO 2
      IF(N.EQ.64) GO TO 2
      IF(N.EQ.80) GO TO 2
      IF(N.EQ.96) GO TO 2
*
*  the extended Gauss cases:
      IF((N/96)*96.EQ.N) GO TO 3
*
C  jump to center of intervall intrgration:
      GO TO 100
*
C  get Gauss point array
*
    2 CALL PHD106
C     -print out message
*     IF(INIT.LE.20)THEN
*       INIT=init+1
*       WRITE (6,*) ' initialization of Gauss int. N=',N
*     ENDIF
C  extract real points
      K=KTAB(N)
      M=N/2
      DO 21 J=1,M
C       extract values from big array
        JTAB=K-1+J
        WTEMP=BETA*A(JTAB)
        DELTA=BETA*X(JTAB)
C       store them backward
        Z(J)=ALPHA-DELTA
        W(J)=WTEMP
C       store them forward
        JP=N+1-J
        Z(JP)=ALPHA+DELTA
        W(JP)=WTEMP
   21 CONTINUE
C     store central point (odd N)
      IF((N-M-M).EQ.0) RETURN
      Z(M+1)=ALPHA
      JMID=K+M
      W(M+1)=BETA*A(JMID)
      RETURN
*
C  get ND96 times chained 96 Gauss point array
*
    3 CALL PHD106
C  print out message
      IF(INIT.LE.20)THEN
        INIT=init+1
        WRITE (6,*) ' initialization of extended Gauss int. N=',N
      ENDIF
C     -extract real points
      K=KTAB(96)
      ND96=N/96
      DO 31 J=1,48
C       extract values from big array
        JTAB=K-1+J
        WTEMP=BETA*A(JTAB)
        DELTA=BETA*X(JTAB)
        WTeMP=WTEMP/ND96
        DeLTA=DELTA/ND96
        DO 32 JD96=0,ND96-1
          ZCNTR= (ALPHA-BETA)+ BETA*FLOAT(2*JD96+1)/FLOAT(ND96)
C         store them backward
          Z(J+JD96*96)=ZCNTR-DELTA
          W(J+JD96*96)=WTEMP
C         store them forward
          JP=96+1-J
          Z(JP+JD96*96)=ZCNTR+DELTA
          W(JP+JD96*96)=WTEMP
   32   CONTINUE
   31 CONTINUE
      RETURN
*
C  the center of intervall cases:
  100 CONTINUE
C  print out message
      IF(INIT.LE.20)THEN
        INIT=init+1
        WRITE (6,*) ' init. of center of intervall int. N=',N
      ENDIF
C  put in constant weight and equally spaced central points
      N=IABS(N)
      DO 111 IN=1,N
        WIN=(BX-AX)/FLOAT(N)
        Z(IN)=AX  + (FLOAT(IN)-.5)*WIN
  111 W(IN)=WIN
      RETURN
      END
C
C
      SUBROUTINE PHD106
C*********************************************************************
C
C     store big arrays needed for Gauss integral
C     (no usefull work, full arrays A,X,ITAB copied on B,Y,LTAB)
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      SAVE
      COMMON /GQCOM/ B(273),Y(273),LTAB(96)
      DIMENSION      A(273),X(273),KTAB(96)
C
C-----TABLE OF INITIAL SUBSCRIPTS FOR N=2(1)16(4)96
      DATA KTAB(2)/1/
      DATA KTAB(3)/2/
      DATA KTAB(4)/4/
      DATA KTAB(5)/6/
      DATA KTAB(6)/9/
      DATA KTAB(7)/12/
      DATA KTAB(8)/16/
      DATA KTAB(9)/20/
      DATA KTAB(10)/25/
      DATA KTAB(11)/30/
      DATA KTAB(12)/36/
      DATA KTAB(13)/42/
      DATA KTAB(14)/49/
      DATA KTAB(15)/56/
      DATA KTAB(16)/64/
      DATA KTAB(20)/72/
      DATA KTAB(24)/82/
      DATA KTAB(28)/82/
      DATA KTAB(32)/94/
      DATA KTAB(36)/94/
      DATA KTAB(40)/110/
      DATA KTAB(44)/110/
      DATA KTAB(48)/130/
      DATA KTAB(52)/130/
      DATA KTAB(56)/130/
      DATA KTAB(60)/130/
      DATA KTAB(64)/154/
      DATA KTAB(68)/154/
      DATA KTAB(72)/154/
      DATA KTAB(76)/154/
      DATA KTAB(80)/186/
      DATA KTAB(84)/186/
      DATA KTAB(88)/186/
      DATA KTAB(92)/186/
      DATA KTAB(96)/226/
C
C-----TABLE OF ABSCISSAE (X) AND WEIGHTS (A) FOR INTERVAL (-1,+1).
C
C-----N=2
      DATA X(1)/0.577350269189626D0  /, A(1)/1.000000000000000D0  /
C-----N=3
      DATA X(2)/0.774596669241483D0  /, A(2)/0.555555555555556D0  /
      DATA X(3)/0.000000000000000D0  /, A(3)/0.888888888888889D0  /
C-----N=4
      DATA X(4)/0.861136311594053D0  /, A(4)/0.347854845137454D0  /
      DATA X(5)/0.339981043584856D0  /, A(5)/0.652145154862546D0  /
C-----N=5
      DATA X(6)/0.906179845938664D0  /, A(6)/0.236926885056189D0  /
      DATA X(7)/0.538469310105683D0  /, A(7)/0.478628670499366D0  /
      DATA X(8)/0.000000000000000D0  /, A(8)/0.568888888888889D0  /
C-----N=6
      DATA X(9)/0.932469514203152D0  /, A(9)/0.171324492379170D0  /
      DATA X(10)/0.661209386466265D0 /, A(10)/0.360761573048139D0 /
      DATA X(11)/0.238619186083197D0 /, A(11)/0.467913934572691D0 /
C-----N=7
      DATA X(12)/0.949107912342759D0 /, A(12)/0.129484966168870D0 /
      DATA X(13)/0.741531185599394D0 /, A(13)/0.279705391489277D0 /
      DATA X(14)/0.405845151377397D0 /, A(14)/0.381830050505119D0 /
      DATA X(15)/0.000000000000000D0 /, A(15)/0.417959183673469D0 /
C-----N=8
      DATA X(16)/0.960289856497536D0 /, A(16)/0.101228536290376D0 /
      DATA X(17)/0.796666477413627D0 /, A(17)/0.222381034453374D0 /
      DATA X(18)/0.525532409916329D0 /, A(18)/0.313706645877887D0 /
      DATA X(19)/0.183434642495650D0 /, A(19)/0.362683783378362D0 /
C-----N=9
      DATA X(20)/0.968160239507626D0 /, A(20)/0.081274388361574D0 /
      DATA X(21)/0.836031107326636D0 /, A(21)/0.180648160694857D0 /
      DATA X(22)/0.613371432700590D0 /, A(22)/0.260610696402935D0 /
      DATA X(23)/0.324253423403809D0 /, A(23)/0.312347077040003D0 /
      DATA X(24)/0.000000000000000D0 /, A(24)/0.330239355001260D0 /
C-----N=10
      DATA X(25)/0.973906528517172D0 /, A(25)/0.066671344308688D0 /
      DATA X(26)/0.865063366688985D0 /, A(26)/0.149451349150581D0 /
      DATA X(27)/0.679409568299024D0 /, A(27)/0.219086362515982D0 /
      DATA X(28)/0.433395394129247D0 /, A(28)/0.269266719309996D0 /
      DATA X(29)/0.148874338981631D0 /, A(29)/0.295524224714753D0 /
C-----N=11
      DATA X(30)/0.978228658146057D0 /, A(30)/0.055668567116174D0 /
      DATA X(31)/0.887062599768095D0 /, A(31)/0.125580369464905D0 /
      DATA X(32)/0.730152005574049D0 /, A(32)/0.186290210927734D0 /
      DATA X(33)/0.519096129206812D0 /, A(33)/0.233193764591990D0 /
      DATA X(34)/0.269543155952345D0 /, A(34)/0.262804544510247D0 /
      DATA X(35)/0.000000000000000D0 /, A(35)/0.272925086777901D0 /
C-----N=12
      DATA X(36)/0.981560634246719D0 /, A(36)/0.047175336386512D0 /
      DATA X(37)/0.904117256370475D0 /, A(37)/0.106939325995318D0 /
      DATA X(38)/0.769902674194305D0 /, A(38)/0.160078328543346D0 /
      DATA X(39)/0.587317954286617D0 /, A(39)/0.203167426723066D0 /
      DATA X(40)/0.367831498998180D0 /, A(40)/0.233492536538355D0 /
      DATA X(41)/0.125233408511469D0 /, A(41)/0.249147045813403D0 /
C-----N=13
      DATA X(42)/0.984183054718588D0 /, A(42)/0.040484004765316D0 /
      DATA X(43)/0.917598399222978D0 /, A(43)/0.092121499837728D0 /
      DATA X(44)/0.801578090733310D0 /, A(44)/0.138873510219787D0 /
      DATA X(45)/0.642349339440340D0 /, A(45)/0.178145980761946D0 /
      DATA X(46)/0.448492751036447D0 /, A(46)/0.207816047536889D0 /
      DATA X(47)/0.230458315955135D0 /, A(47)/0.226283180262897D0 /
      DATA X(48)/0.000000000000000D0 /, A(48)/0.232551553230874D0 /
C-----N=14
      DATA X(49)/0.986283808696812D0 /, A(49)/0.035119460331752D0 /
      DATA X(50)/0.928434883663574D0 /, A(50)/0.080158087159760D0 /
      DATA X(51)/0.827201315069765D0 /, A(51)/0.121518570687903D0 /
      DATA X(52)/0.687292904811685D0 /, A(52)/0.157203167158194D0 /
      DATA X(53)/0.515248636358154D0 /, A(53)/0.185538397477938D0 /
      DATA X(54)/0.319112368927890D0 /, A(54)/0.205198463721296D0 /
      DATA X(55)/0.108054948707344D0 /, A(55)/0.215263853463158D0 /
C-----N=15
      DATA X(56)/0.987992518020485D0 /, A(56)/0.030753241996117D0 /
      DATA X(57)/0.937273392400706D0 /, A(57)/0.070366047488108D0 /
      DATA X(58)/0.848206583410427D0 /, A(58)/0.107159220467172D0 /
      DATA X(59)/0.724417731360170D0 /, A(59)/0.139570677926154D0 /
      DATA X(60)/0.570972172608539D0 /, A(60)/0.166269205816994D0 /
      DATA X(61)/0.394151347077563D0 /, A(61)/0.186161000015562D0 /
      DATA X(62)/0.201194093997435D0 /, A(62)/0.198431485327111D0 /
      DATA X(63)/0.000000000000000D0 /, A(63)/0.202578241925561D0 /
C-----N=16
      DATA X(64)/0.989400934991650D0 /, A(64)/0.027152459411754D0 /
      DATA X(65)/0.944575023073233D0 /, A(65)/0.062253523938648D0 /
      DATA X(66)/0.865631202387832D0 /, A(66)/0.095158511682493D0 /
      DATA X(67)/0.755404408355003D0 /, A(67)/0.124628971255534D0 /
      DATA X(68)/0.617876244402644D0 /, A(68)/0.149595988816577D0 /
      DATA X(69)/0.458016777657227D0 /, A(69)/0.169156519395003D0 /
      DATA X(70)/0.281603550779259D0 /, A(70)/0.182603415044924D0 /
      DATA X(71)/0.095012509837637D0 /, A(71)/0.189450610455069D0 /
C-----N=20
      DATA X(72)/0.993128599185094D0 /, A(72)/0.017614007139152D0 /
      DATA X(73)/0.963971927277913D0 /, A(73)/0.040601429800386D0 /
      DATA X(74)/0.912234428251325D0 /, A(74)/0.062672048334109D0 /
      DATA X(75)/0.839116971822218D0 /, A(75)/0.083276741576704D0 /
      DATA X(76)/0.746331906460150D0 /, A(76)/0.101930119817240D0 /
      DATA X(77)/0.636053680726515D0 /, A(77)/0.118194531961518D0 /
      DATA X(78)/0.510867001950827D0 /, A(78)/0.131688638449176D0 /
      DATA X(79)/0.373706088715419D0 /, A(79)/0.142096109318382D0 /
      DATA X(80)/0.227785851141645D0 /, A(80)/0.149172986472603D0 /
      DATA X(81)/0.076526521133497D0 /, A(81)/0.152753387130725D0 /
C-----N=24
      DATA X(82)/0.995187219997021D0 /, A(82)/0.012341229799987D0 /
      DATA X(83)/0.974728555971309D0 /, A(83)/0.028531388628933D0 /
      DATA X(84)/0.938274552002732D0 /, A(84)/0.044277438817419D0 /
      DATA X(85)/0.886415527004401D0 /, A(85)/0.059298584915436D0 /
      DATA X(86)/0.820001985973902D0 /, A(86)/0.073346481411080D0 /
      DATA X(87)/0.740124191578554D0 /, A(87)/0.086190161531953D0 /
      DATA X(88)/0.648093651936975D0 /, A(88)/0.097618652104113D0 /
      DATA X(89)/0.545421471388839D0 /, A(89)/0.107444270115965D0 /
      DATA X(90)/0.433793507626045D0 /, A(90)/0.115505668053725D0 /
      DATA X(91)/0.315042679696163D0 /, A(91)/0.121670472927803D0 /
      DATA X(92)/0.191118867473616D0 /, A(92)/0.125837456346828D0 /
      DATA X(93)/0.064056892862605D0 /, A(93)/0.127938195346752D0 /
C-----N=32
      DATA X(94)/0.997263861849481D0 /, A(94)/0.007018610009470D0 /
      DATA X(95)/0.985611511545268D0 /, A(95)/0.016274394730905D0 /
      DATA X(96)/0.964762255587506D0 /, A(96)/0.025392065309262D0 /
      DATA X(97)/0.934906075937739D0 /, A(97)/0.034273862913021D0 /
      DATA X(98)/0.896321155766052D0 /, A(98)/0.042835898022226D0 /
      DATA X(99)/0.849367613732569D0 /, A(99)/0.050998059262376D0 /
      DATA X(100)/0.794483795967942D0/, A(100)/0.058684093478535D0/
      DATA X(101)/0.732182118740289D0/, A(101)/0.065822222776361D0/
      DATA X(102)/0.663044266930215D0/, A(102)/0.072345794108848D0/
      DATA X(103)/0.587715757240762D0/, A(103)/0.078193895787070D0/
      DATA X(104)/0.506899908932229D0/, A(104)/0.083311924226946D0/
      DATA X(105)/0.421351276130635D0/, A(105)/0.087652093004403D0/
      DATA X(106)/0.331868602282127D0/, A(106)/0.091173878695763D0/
      DATA X(107)/0.239287362252137D0/, A(107)/0.093844399080804D0/
      DATA X(108)/0.144471961582796D0/, A(108)/0.095638720079274D0/
      DATA X(109)/0.048307665687738D0/, A(109)/0.096540088514727D0/
C-----N=40
      DATA X(110)/0.998237709710559D0/, A(110)/0.004521277098533D0/
      DATA X(111)/0.990726238699457D0/, A(111)/0.010498284531152D0/
      DATA X(112)/0.977259949983774D0/, A(112)/0.016421058381907D0/
      DATA X(113)/0.957916819213791D0/, A(113)/0.022245849194166D0/
      DATA X(114)/0.932812808278676D0/, A(114)/0.027937006980023D0/
      DATA X(115)/0.902098806968874D0/, A(115)/0.033460195282547D0/
      DATA X(116)/0.865959503212259D0/, A(116)/0.038782167974472D0/
      DATA X(117)/0.824612230833311D0/, A(117)/0.043870908185673D0/
      DATA X(118)/0.778305651426519D0/, A(118)/0.048695807635072D0/
      DATA X(119)/0.727318255189927D0/, A(119)/0.053227846983936D0/
      DATA X(120)/0.671956684614179D0/, A(120)/0.057439769099391D0/
      DATA X(121)/0.612553889667980D0/, A(121)/0.061306242492928D0/
      DATA X(122)/0.549467125095128D0/, A(122)/0.064804013456601D0/
      DATA X(123)/0.483075801686178D0/, A(123)/0.067912045815233D0/
      DATA X(124)/0.413779204371605D0/, A(124)/0.070611647391286D0/
      DATA X(125)/0.341994090825758D0/, A(125)/0.072886582395804D0/
      DATA X(126)/0.268152185007253D0/, A(126)/0.074723169057968D0/
      DATA X(127)/0.192697580701371D0/, A(127)/0.076110361900626D0/
      DATA X(128)/0.116084070675255D0/, A(128)/0.077039818164247D0/
      DATA X(129)/0.038772417506050D0/, A(129)/0.077505947978424D0/
C-----N=48
      DATA X(130)/0.998771007252426D0/, A(130)/0.003153346052305D0/
      DATA X(131)/0.993530172266350D0/, A(131)/0.007327553901276D0/
      DATA X(132)/0.984124583722826D0/, A(132)/0.011477234579234D0/
      DATA X(133)/0.970591592546247D0/, A(133)/0.015579315722943D0/
      DATA X(134)/0.952987703160430D0/, A(134)/0.019616160457355D0/
      DATA X(135)/0.931386690706554D0/, A(135)/0.023570760839324D0/
      DATA X(136)/0.905879136715569D0/, A(136)/0.027426509708356D0/
      DATA X(137)/0.876572020274247D0/, A(137)/0.031167227832798D0/
      DATA X(138)/0.843588261624393D0/, A(138)/0.034777222564770D0/
      DATA X(139)/0.807066204029442D0/, A(139)/0.038241351065830D0/
      DATA X(140)/0.767159032515740D0/, A(140)/0.041545082943464D0/
      DATA X(141)/0.724034130923814D0/, A(141)/0.044674560856694D0/
      DATA X(142)/0.677872379632663D0/, A(142)/0.047616658492490D0/
      DATA X(143)/0.628867396776513D0/, A(143)/0.050359035553854D0/
      DATA X(144)/0.577224726083972D0/, A(144)/0.052890189485193D0/
      DATA X(145)/0.523160974722233D0/, A(145)/0.055199503699984D0/
      DATA X(146)/0.466902904750958D0/, A(146)/0.057277292100403D0/
      DATA X(147)/0.408686481990716D0/, A(147)/0.059114839698395D0/
      DATA X(148)/0.348755886292160D0/, A(148)/0.060704439165893D0/
      DATA X(149)/0.287362487355455D0/, A(149)/0.062039423159892D0/
      DATA X(150)/0.224763790394689D0/, A(150)/0.063114192286254D0/
      DATA X(151)/0.161222356068891D0/, A(151)/0.063924238584648D0/
      DATA X(152)/0.097004699209462D0/, A(152)/0.064466164435950D0/
      DATA X(153)/0.032380170962869D0/, A(153)/0.064737696812683D0/
C-----N=64
      DATA X(154)/0.999305041735772D0/, A(154)/0.001783280721696D0/
      DATA X(155)/0.996340116771955D0/, A(155)/0.004147033260562D0/
      DATA X(156)/0.991013371476744D0/, A(156)/0.006504457968978D0/
      DATA X(157)/0.983336253884625D0/, A(157)/0.008846759826363D0/
      DATA X(158)/0.973326827789910D0/, A(158)/0.011168139460131D0/
      DATA X(159)/0.961008799652053D0/, A(159)/0.013463047896718D0/
      DATA X(160)/0.946411374858402D0/, A(160)/0.015726030476024D0/
      DATA X(161)/0.929569172131939D0/, A(161)/0.017951715775697D0/
      DATA X(162)/0.910522137078502D0/, A(162)/0.020134823153530D0/
      DATA X(163)/0.889315445995114D0/, A(163)/0.022270173808383D0/
      DATA X(164)/0.865999398154092D0/, A(164)/0.024352702568710D0/
      DATA X(165)/0.840629296252580D0/, A(165)/0.026377469715054D0/
      DATA X(166)/0.813265315122797D0/, A(166)/0.028339672614259D0/
      DATA X(167)/0.783972358943341D0/, A(167)/0.030234657072402D0/
      DATA X(168)/0.752819907260531D0/, A(168)/0.032057928354851D0/
      DATA X(169)/0.719881850171610D0/, A(169)/0.033805161837141D0/
      DATA X(170)/0.685236313054233D0/, A(170)/0.035472213256882D0/
      DATA X(171)/0.648965471254657D0/, A(171)/0.037055128540240D0/
      DATA X(172)/0.611155355172393D0/, A(172)/0.038550153178615D0/
      DATA X(173)/0.571895646202634D0/, A(173)/0.039953741132720D0/
      DATA X(174)/0.531279464019894D0/, A(174)/0.041262563242623D0/
      DATA X(175)/0.489403145707052D0/, A(175)/0.042473515123653D0/
      DATA X(176)/0.446366017253464D0/, A(176)/0.043583724529323D0/
      DATA X(177)/0.402270157963991D0/, A(177)/0.044590558163756D0/
      DATA X(178)/0.357220158337668D0/, A(178)/0.045491627927418D0/
      DATA X(179)/0.311322871990210D0/, A(179)/0.046284796581314D0/
      DATA X(180)/0.264687162208767D0/, A(180)/0.046968182816210D0/
      DATA X(181)/0.217423643740007D0/, A(181)/0.047540165714830D0/
      DATA X(182)/0.169644420423992D0/, A(182)/0.047999388596458D0/
      DATA X(183)/0.121462819296120D0/, A(183)/0.048344762234802D0/
      DATA X(184)/0.072993121787799D0/, A(184)/0.048575467441503D0/
      DATA X(185)/0.024350292663424D0/, A(185)/0.048690957009139D0/
C-----N=80
      DATA X(186)/0.999553822651630D0/, A(186)/0.001144950003186D0/
      DATA X(187)/0.997649864398237D0/, A(187)/0.002663533589512D0/
      DATA X(188)/0.994227540965688D0/, A(188)/0.004180313124694D0/
      DATA X(189)/0.989291302499755D0/, A(189)/0.005690922451403D0/
      DATA X(190)/0.982848572738629D0/, A(190)/0.007192904768117D0/
      DATA X(191)/0.974909140585727D0/, A(191)/0.008683945269260D0/
      DATA X(192)/0.965485089043799D0/, A(192)/0.010161766041103D0/
      DATA X(193)/0.954590766343634D0/, A(193)/0.011624114120797D0/
      DATA X(194)/0.942242761309872D0/, A(194)/0.013068761592401D0/
      DATA X(195)/0.928459877172445D0/, A(195)/0.014493508040509D0/
      DATA X(196)/0.913263102571757D0/, A(196)/0.015896183583725D0/
      DATA X(197)/0.896675579438770D0/, A(197)/0.017274652056269D0/
      DATA X(198)/0.878722567678213D0/, A(198)/0.018626814208299D0/
      DATA X(199)/0.859431406663111D0/, A(199)/0.019950610878141D0/
      DATA X(200)/0.838831473580255D0/, A(200)/0.021244026115782D0/
      DATA X(201)/0.816954138681463D0/, A(201)/0.022505090246332D0/
      DATA X(202)/0.793832717504605D0/, A(202)/0.023731882865930D0/
      DATA X(203)/0.769502420135041D0/, A(203)/0.024922535764115D0/
      DATA X(204)/0.744000297583597D0/, A(204)/0.026075235767565D0/
      DATA X(205)/0.717365185362099D0/, A(205)/0.027188227500486D0/
      DATA X(206)/0.689637644342027D0/, A(206)/0.028259816057276D0/
      DATA X(207)/0.660859898986119D0/, A(207)/0.029288369583267D0/
      DATA X(208)/0.631075773046871D0/, A(208)/0.030272321759557D0/
      DATA X(209)/0.600330622829751D0/, A(209)/0.031210174188114D0/
      DATA X(210)/0.568671268122709D0/, A(210)/0.032100498673487D0/
      DATA X(211)/0.536145920897131D0/, A(211)/0.032941939397645D0/
      DATA X(212)/0.502804111888784D0/, A(212)/0.033733214984611D0/
      DATA X(213)/0.468696615170544D0/, A(213)/0.034473120451753D0/
      DATA X(214)/0.433875370831756D0/, A(214)/0.035160529044747D0/
      DATA X(215)/0.398393405881969D0/, A(215)/0.035794393953416D0/
      DATA X(216)/0.362304753499487D0/, A(216)/0.036373749905835D0/
      DATA X(217)/0.325664370747701D0/, A(217)/0.036897714638276D0/
      DATA X(218)/0.288528054884511D0/, A(218)/0.037365490238730D0/
      DATA X(219)/0.250952358392272D0/, A(219)/0.037776364362001D0/
      DATA X(220)/0.212994502857666D0/, A(220)/0.038129711314477D0/
      DATA X(221)/0.174712291832646D0/, A(221)/0.038424993006959D0/
      DATA X(222)/0.136164022809143D0/, A(222)/0.038661759774076D0/
      DATA X(223)/0.097408398441584D0/, A(223)/0.038839651059051D0/
      DATA X(224)/0.058504437152420D0/, A(224)/0.038958395962769D0/
      DATA X(225)/0.019511383256793D0/, A(225)/0.039017813656306D0/
C-----N=96
      DATA X(226)/0.999689503883230D0/, A(226)/0.000796792065552D0/
      DATA X(227)/0.998364375863181D0/, A(227)/0.001853960788946D0/
      DATA X(228)/0.995981842987209D0/, A(228)/0.002910731817934D0/
      DATA X(229)/0.992543900323762D0/, A(229)/0.003964554338444D0/
      DATA X(230)/0.988054126329623D0/, A(230)/0.005014202742927D0/
      DATA X(231)/0.982517263563014D0/, A(231)/0.006058545504235D0/
      DATA X(232)/0.975939174585136D0/, A(232)/0.007096470791153D0/
      DATA X(233)/0.968326828463264D0/, A(233)/0.008126876925698D0/
      DATA X(234)/0.959688291448742D0/, A(234)/0.009148671230783D0/
      DATA X(235)/0.950032717784437D0/, A(235)/0.010160770535008D0/
      DATA X(236)/0.939370339752755D0/, A(236)/0.011162102099838D0/
      DATA X(237)/0.927712456722308D0/, A(237)/0.012151604671088D0/
      DATA X(238)/0.915071423120898D0/, A(238)/0.013128229566961D0/
      DATA X(239)/0.901460635315852D0/, A(239)/0.014090941772314D0/
      DATA X(240)/0.886894517402420D0/, A(240)/0.015038721026994D0/
      DATA X(241)/0.871388505909296D0/, A(241)/0.015970562902562D0/
      DATA X(242)/0.854959033434601D0/, A(242)/0.016885479864245D0/
      DATA X(243)/0.837623511228187D0/, A(243)/0.017782502316045D0/
      DATA X(244)/0.819400310737931D0/, A(244)/0.018660679627411D0/
      DATA X(245)/0.800308744139140D0/, A(245)/0.019519081140145D0/
      DATA X(246)/0.780369043867433D0/, A(246)/0.020356797154333D0/
      DATA X(247)/0.759602341176647D0/, A(247)/0.021172939892191D0/
      DATA X(248)/0.738030643744400D0/, A(248)/0.021966644438744D0/
      DATA X(249)/0.715676812348967D0/, A(249)/0.022737069658329D0/
      DATA X(250)/0.692564536642171D0/, A(250)/0.023483399085926D0/
      DATA X(251)/0.668718310043916D0/, A(251)/0.024204841792364D0/
      DATA X(252)/0.644163403784967D0/, A(252)/0.024900633222483D0/
      DATA X(253)/0.618925840125468D0/, A(253)/0.025570036005349D0/
      DATA X(254)/0.593032364777572D0/, A(254)/0.026212340735672D0/
      DATA X(255)/0.566510418561397D0/, A(255)/0.026826866725591D0/
      DATA X(256)/0.539388108324357D0/, A(256)/0.027412962726029D0/
      DATA X(257)/0.511694177154667D0/, A(257)/0.027970007616848D0/
      DATA X(258)/0.483457973920596D0/, A(258)/0.028497411065085D0/
      DATA X(259)/0.454709422167743D0/, A(259)/0.028994614150555D0/
      DATA X(260)/0.425478988407300D0/, A(260)/0.029461089958167D0/
      DATA X(261)/0.395797649828908D0/, A(261)/0.029896344136328D0/
      DATA X(262)/0.365696861472313D0/, A(262)/0.030299915420827D0/
      DATA X(263)/0.335208522892625D0/, A(263)/0.030671376123669D0/
      DATA X(264)/0.304364944354496D0/, A(264)/0.031010332586313D0/
      DATA X(265)/0.273198812591049D0/, A(265)/0.031316425596861D0/
      DATA X(266)/0.241743156163840D0/, A(266)/0.031589330770727D0/
      DATA X(267)/0.210031310460567D0/, A(267)/0.031828758894411D0/
      DATA X(268)/0.178096882367618D0/, A(268)/0.032034456231992D0/
      DATA X(269)/0.145973714654896D0/, A(269)/0.032206204794030D0/
      DATA X(270)/0.113695850110665D0/, A(270)/0.032343822568575D0/
      DATA X(271)/0.081297495464425D0/, A(271)/0.032447163714064D0/
      DATA X(272)/0.048812985136049D0/, A(272)/0.032516118713868D0/
      DATA X(273)/0.016276744849602D0/, A(273)/0.032550614492363D0/
      DATA IBD/0/
      IF(IBD.NE.0) RETURN
      IBD=1
      DO 10 I=1,273
        B(I) = A(I)
10      Y(I) = X(I)
      DO 20 I=1,96
20      LTAB(I) = KTAB(I)
      RETURN
      END
C
C
      DOUBLE PRECISION FUNCTION DZEROX(A0,B0,EPS,MAXF,F,MODE)
C**********************************************************************
C     Based on
C
C        J.C.P. Bus and T.J. Dekker, Two Efficient Algorithms with
C        Guaranteed Convergence for Finding a Zero of a Function,
C        ACM Trans. Math. Software 1 (1975) 330-345.
C
C        (MODE = 1: Algorithm M;    MODE = 2: Algorithm R)
C
C        CERNLIB C200
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      CHARACTER NAME*(*)
      PARAMETER (NAME = 'DZEROX')
      LOGICAL LMT
      DIMENSION IM1(2),IM2(2),LMT(2)
      EXTERNAL F

      PARAMETER (Z1 = 1, HALF = Z1/2)

      DATA IM1 /2,3/, IM2 /-1,3/

      IF(MODE .NE. 1 .AND. MODE .NE. 2) THEN
       C=0
       WRITE(6,100) NAME,MODE
       GO TO 99
      ENDIF
      FA=F(B0)
      FB=F(A0)
      IF(FA*FB .GT. 0) THEN
       C=0
c       WRITE(6,101) NAME
c       CALL POPREV(-1)
       GO TO 99
      ENDIF
      ATL=ABS(EPS)
      B=A0
      A=B0
      LMT(2)=.TRUE.
      MF=2
    1 C=A
      FC=FA
    2 IE=0
    3 IF(ABS(FC) .LT. ABS(FB)) THEN
       IF(C .NE. A) THEN
        D=A
        FD=FA
       END IF
       A=B
       B=C
       C=A
       FA=FB
       FB=FC
       FC=FA
      END IF
      TOL=ATL*(1+ABS(C))
      H=HALF*(C+B)
      HB=H-B
      IF(ABS(HB) .GT. TOL) THEN
       IF(IE .GT. IM1(MODE)) THEN
        W=HB
       ELSE
        TOL=TOL*SIGN(Z1,HB)
        P=(B-A)*FB
        LMT(1)=IE .LE. 1
        IF(LMT(MODE)) THEN
         Q=FA-FB
         LMT(2)=.FALSE.
        ELSE
         FDB=(FD-FB)/(D-B)
         FDA=(FD-FA)/(D-A)
         P=FDA*P
         Q=FDB*FA-FDA*FB
        END IF
        IF(P .LT. 0) THEN
         P=-P
         Q=-Q
        END IF
        IF(IE .EQ. IM2(MODE)) P=P+P
        IF(P .EQ. 0 .OR. P .LE. Q*TOL) THEN
         W=TOL
        ELSEIF(P .LT. HB*Q) THEN
         W=P/Q
        ELSE
         W=HB
        END IF
       END IF
       D=A
       A=B
       FD=FA
       FA=FB
       B=B+W
       MF=MF+1
       IF(MF .GT. MAXF) THEN
        WRITE(6,102) NAME
        GO TO 99
       ENDIF
       FB=F(B)
       IF(FB .EQ. 0 .OR. SIGN(Z1,FC) .EQ. SIGN(Z1,FB)) GO TO 1
       IF(W .EQ. HB) GO TO 2
       IE=IE+1
       GO TO 3
      END IF
   99 CONTINUE
      DZEROX=C
      RETURN
  100 FORMAT(1X,A,':WARNING:MODE = ',I3,' ILLEGAL')
  101 FORMAT(1X,A,':WARNING:F(A) AND F(B) HAVE THE SAME SIGN')
  102 FORMAT(1X,A,':WARNING:TOO MANY FUNCTION CALLS')
      END
C
C
      SUBROUTINE TIMDAT
C*********************************************************************
C
C     there are two versions, one commented
C
C     IBM-VM/CMS VERSION OF TIMDAT
C
C**********************************************************************
C     CALL GETDAT(IYEAR,IMONTH,IDAY)
C     CALL GETTIM(IHOUR,IMINUTE,ISECOND,IHUNDREDSECOND)
C     WRITE(6,10) IDAY,IMONTH,IYEAR,IHOUR,IMINUTE,ISECOND,IHUNDREDSECOND
C10    FORMAT(' ***** Date ***** ',I3,'.',I2,'.',I4,' ***** Time *****',
C    &       I4,':',I2,':',I2,'.',I2,' ',10('*'))
C**********************************************************************
C
C     SGI/HP/LAHEY VERSION OF TIMDAT
C
C**********************************************************************
*     CHARACTER*9 D
*     CHARACTER*8 T
*     CALL DATE(D)
*     CALL TIME(T)
*     WRITE(6,'(/1X,2A,/9X,2A)') 'TIMDAT: Date: ',D,'Time: ',T
C*********************************************************************
C
C     RISC VERSION OF TIMDAT NOT YET INSTALLED
C
C*********************************************************************
C     WRITE(6,*) '  **** TIMDAT ON THE RISC NOT INSTALLED ****'
C*********************************************************************
C
C     Salford/NAG-F90  VERSION OF TIMDAT
C
C*********************************************************************
C     CHARACTER*8 DATE@
C     CHARACTER*8 TIME@
C     WRITE(6,'(/1X,2A,/9X,2A)') 'TIMDAT: Date: ',DATE@(),
C    &                           'Time: ',TIME@()
C
      END
C
C
      DOUBLE PRECISION FUNCTION DEXPI(RXM)
C**********************************************************************
C
C     function to calculate  E_i(x) = -E_1(-x)
C
C     CERN LIB C337   (changed by R.Engel 10/1993)
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DOUBLE PRECISION P1(5),Q1(5),P2(7),Q2(7),P3(6),Q3(6),P4(8),Q4(8)
      DOUBLE PRECISION A1(8),B1(8),A2(8),B2(8),A3(6),B3(6),XL(6)
      DOUBLE PRECISION X,Y,ZERO,ONE,TWO,THREE,AP,BP,DP,AQ,BQ,DQ,X0,V,D

      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, THREE /3.0D0/
      DATA  X0 /0.37250 74107 8137D0/
      DATA XL /-24.0D0,-12.0D0,-6.0D0,0.0D0,1.0D0,4.0D0/
      DATA P1
     1/+4.29312 52343 210D+0, +3.98941 53870 321D+1,
     2 +2.92525 18866 921D+2, +4.25696 82638 592D+2,
     3 -4.34981 43832 952D+2/
      DATA Q1
     1/+1.00000 00000 000D+0, +1.88992 88395 003D+1,
     2 +1.50950 38744 251D+2, +5.68052 52718 987D+2,
     3 +7.53585 64359 843D+2/
      DATA P2
     1/+4.30967 83946 939D-1, +6.90522 52278 444D+0,
     2 +2.30192 55939 133D+1, +2.43784 08879 132D+1,
     3 +9.04161 55694 633D+0, +9.99979 57705 159D-1,
     4 +4.65627 10797 510D-7/
      DATA Q2
     1/+1.03400 13040 487D-1, +3.31909 21359 330D+0,
     2 +2.04494 78501 379D+1, +4.12807 84189 142D+1,
     3 +3.24264 21069 514D+1, +1.00411 64382 905D+1,
     4 +1.00000 00000 000D+0/
      DATA P3
     1/-2.39099 64453 136D+0, -1.47982 19500 504D+2,
     2 -2.54376 33976 890D+2, -1.19557 61038 372D+2,
     3 -1.96304 08535 939D+1, -9.99999 99990 360D-1/
      DATA Q3
     1/+1.77600 70940 351D+2, +5.30685 09610 812D+2,
     2 +4.62230 27156 148D+2, +1.56818 43364 539D+2,
     3 +2.16304 08494 238D+1, +1.00000 00000 000D+0/
      DATA P4
     1/-8.66937 33995 107D+0, -5.49142 26552 109D+2,
     2 -4.21001 61535 707D+3, -2.49301 39345 865D+5,
     3 -1.19623 66934 925D+5, -2.21744 62775 885D+7,
     4 +3.89280 42131 120D+6, -3.91546 07380 910D+8/
      DATA Q4
     1/+3.41718 75000 000D+1, -1.60708 92658 722D+3,
     2 +3.57300 29805 851D+4, -4.83547 43616 216D+5,
     3 +4.28559 62461 175D+6, -2.49033 37574 054D+7,
     4 +8.91925 76757 561D+7, -1.65254 29972 521D+8/
      DATA A1
     1/-2.18086 38152 072D+0, -2.19010 23385 488D+1,
     2 +9.30816 38566 217D+0, +2.50762 81129 356D+1,
     3 -3.31842 53199 722D+1, +6.01217 99083 008D+1,
     4 -4.32531 13287 813D+1, +1.00443 10922 808D+0/
      DATA B1
     1/+0.00000 00000 000D+0, +3.93707 70185 272D+0,
     2 +3.00892 64837 292D+2, -6.25041 16167 188D+0,
     3 +1.00367 43951 673D+3, +1.43256 73812 194D+1,
     4 +2.73624 11988 933D+3, +5.27468 85196 291D-1/
      DATA A2
     1/-3.48334 65360 285D+0, -1.86545 45488 340D+1,
     2 -8.28561 99414 064D+0, -3.23467 33030 540D+1,
     3 +1.79601 68876 925D+1, +1.75656 31546 961D+0,
     4 -1.95022 32128 966D+0, +9.99994 29607 471D-1/
      DATA B2
     1/+0.00000 00000 000D+0, +6.95000 65588 743D+1,
     2 +5.72837 19383 732D+1, +2.57776 38423 844D+1,
     3 +7.60761 14800 773D+2, +2.89516 72792 514D+1,
     4 -3.43942 26689 987D+0, +1.00083 86740 264D+0/
      DATA A3
     1/-2.77809 28934 438D+1, -1.01047 90815 760D+1,
     2 -9.14830 08216 736D+0, -5.02233 17461 851D+0,
     3 -3.00000 77799 358D+0, +1.00000 00000 704D+0/
      DATA B3
     1/+0.00000 00000 000D+0, +1.22399 93926 823D+2,
     2 +2.72761 00778 779D+0, -7.18975 18395 045D+0,
     3 -2.99901 18065 262D+0, +1.99999 99428 260D+0/
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
C
C  conversion to E_i function
      X = -RXM
C
      IF(X .LE. XL(1)) THEN
       AP=A3(1)-X
       DO 1 I = 2,5
    1  AP=A3(I)-X+B3(I)/AP
       Y=(EXP(-X)/X)*(ONE-(A3(6)+B3(6)/AP)/X)
      ELSEIF(X .LE. XL(2)) THEN
       AP=A2(1)-X
       DO 2 I = 2,7
    2     AP=A2(I)-X+B2(I)/AP
       Y=(EXP(-X)/X)*(A2(8)+B2(8)/AP)
      ELSEIF(X .LE. XL(3)) THEN
       AP=A1(1)-X
       DO 3 I = 2,7
    3     AP=A1(I)-X+B1(I)/AP
       Y=(EXP(-X)/X)*(A1(8)+B1(8)/AP)
      ELSEIF(X .LT. XL(4)) THEN
       V=-TWO*(X/THREE+ONE)
       BP=ZERO
       DP=P4(1)
       DO 4 I = 2,8
          AP=BP
          BP=DP
    4     DP=P4(I)-AP+V*BP
       BQ=ZERO
       DQ=Q4(1)
       DO 14 I = 2,8
          AQ=BQ
          BQ=DQ
   14     DQ=Q4(I)-AQ+V*BQ
       Y=-LOG(-X/X0)+(X+X0)*(DP-AP)/(DQ-AQ)
      ELSEIF(X .EQ. XL(4)) THEN
*      CALL KERMTR('C337.1',LGFILE,MFLAG,RFLAG)
*      IF(MFLAG) THEN
*       IF(LGFILE .EQ. 0) THEN
*        WRITE(*,100) ENAME
*       ELSE
*        WRITE(LGFILE,100) ENAME
*       ENDIF
*      ENDIF
*      IF(.NOT.RFLAG) CALL ABEND
       DEXPI=ZERO
       RETURN
      ELSEIF(X .LT. XL(5)) THEN
       AP=P1(1)
       AQ=Q1(1)
       DO 5 I = 2,5
          AP=P1(I)+X*AP
    5     AQ=Q1(I)+X*AQ
       Y=-LOG(X)+AP/AQ
      ELSEIF(X .LE. XL(6)) THEN
       Y=ONE/X
       AP=P2(1)
       AQ=Q2(1)
       DO 6 I = 2,7
          AP=P2(I)+Y*AP
    6     AQ=Q2(I)+Y*AQ
       Y=EXP(-X)*AP/AQ
      ELSE
       Y=ONE/X
       AP=P3(1)
       AQ=Q3(1)
       DO 7 I = 2,6
          AP=P3(I)+Y*AP
    7     AQ=Q3(I)+Y*AQ
       Y=EXP(-X)*Y*(ONE+Y*AP/AQ)
      ENDIF
C  sign conversion to E_i
      DEXPI=-Y
      RETURN
C
      END
C
C
      DOUBLE PRECISION FUNCTION BETARN(GAM,ETA)
C********************************************************************
C
C     RANDOM NUMBER GENERATION FROM BETA
C     DISTRIBUTION IN REGION  0.LE.X.LE.1.
C     F(X) = X**(GAM-1.)*(1.-X)**(ETA-1)*GAMM(ETA+GAM) / (GAMM(GAM
C                                                        *GAMM(ETA))
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PARAMETER (ONE=1.D0)
C
      Y = GAMRN(ONE,GAM)
      Z = GAMRN(ONE,ETA)
      BETARN = Y/(Y+Z)
      RETURN
      END
C
C
      DOUBLE PRECISION FUNCTION GAMRN(ALAM,ETA)
C********************************************************************
C
C     RANDOM NUMBER SELECTION FROM GAMMA DISTRIBUTION
C     F(X) = ALAM**ETA*X**(ETA-1)*EXP(-ALAM*X) / GAM(ETA)
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      NCOU=0
      N = ETA
      F = ETA - N
      IF(F.EQ.0.D0) GOTO 20
   10 R = DRNDM(ETA)
      NCOU=NCOU+1
      IF (NCOU.GE.11) GOTO 20
      IF(R.LT.F/(F+2.71828D0)) GOTO 30
      YYY=LOG(DRNDM(F)+1.0D-9)/F
      IF(ABS(YYY).GT.50.D0) GOTO 20
      Y = EXP(YYY)
      IF(LOG(DRNDM(Y)+1.0D-9).GT.-Y) GOTO 10
      GOTO 40
   20 Y = 0.D0
      GOTO 50
   30 Y = 1.D0-LOG(DRNDM(R)+1.0D-9)
      IF(DRNDM(Y).GT.Y**(F-1.D0)) GOTO 10
   40 IF(N.EQ.0) GOTO 70
   50 Z = 1.D0
      DO 60 I = 1,N
   60 Z = Z*DRNDM(Y)
      Y = Y-LOG(Z+1.0D-9)
   70 GAMRN = Y/ALAM
      RETURN
      END
C
C
      SUBROUTINE SFECFE(SFE,CFE)
C**********************************************************************
C
C     fast random SIN(X) COS(X) selection
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
    1 CONTINUE
        X=DRNDM(XX)
        Y=DRNDM(YY)
        XX=X*X
        YY=Y*Y
        XY=XX+YY
      IF(XY.GT.1.D0) GOTO 1
      CFE=(XX-YY)/XY
      SFE=2.D0*X*Y/XY
      IF(DRNDM(XY).LT.0.5D0) THEN
        SFE=-SFE
      ENDIF
      END
C
C
      SUBROUTINE SWAPD(D1,D2)
C********************************************************************
C
C     exchange of argument values (double precision)
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      D = D1
      D1 = D2
      D2 = D
      END
C
C
      SUBROUTINE SWAPI(I1,I2)
C********************************************************************
C
C     exchange of argument values (integer)
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      K = I1
      I1 = I2
      I2 = K
      END
C
C
      DOUBLE PRECISION FUNCTION DREAL(CA)
C********************************************************************
C
C
C********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMPLEX*16 CA
      DREAL = DBLE(CA)
      END
      SUBROUTINE PDIS(X,SCALE2,NPAR,PD)
C***************************************************************
C
C     call different PDF sets for different particle types
C
C     input:      X        momentum fraction
C                 SCALE2   squared scale (GeV**2)
C                 NPAR     1     IGRP(1),ISET(1)
C                          2     IGRP(2),ISET(2)
C
C     output      PD(-6:6) field containing the x*PDF fractions
C
C***************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      DIMENSION PD(-6:6)
C
      PARAMETER ( ONE = 1.D0,
     &           ZERO = 0.D0 )
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      CHARACTER*8 MDLNA
      COMMON /MODELS/ MDLNA(50),ISWMDL(50),PARMDL(200),IPAMDL(200)
C
      DIMENSION PARAM(20),VALUE(20)
      CHARACTER*20 PARAM
C
      REAL XR,P2R,Q2R,F2GM,XPDFGM
      DIMENSION XPDFGM(-6:6)
C
      XI = X
      IF(X.GE.1.D0) THEN
        IF(IDEB(37).GE.0) THEN
          WRITE(6,'(/,1X,A,E15.8/)') 'PDIS:WARNING: X >= 1 ',X
          CALL POPREV(-1)
        ENDIF
        XI = 0.99999999999D0
      ELSE IF(X.LE.0.D0) THEN
        IF(IDEB(37).GE.0) THEN
          WRITE(6,'(/,1X,A,E15.8/)') 'PDIS:WARNING: X <= 0 ',X
          CALL POPREV(-1)
        ENDIF
        XI = 0.0001D0
      ENDIF
C
      DO 100 I=-6,6
        PD(I) = ZERO
 100  CONTINUE
      IRET = 1
C
      IF((NPAR.EQ.1).OR.(NPAR.EQ.2)) THEN
C
C  internal PDFs
C
        IF(IEXT(NPAR).EQ.0) THEN
          IF(ITYPE(NPAR).EQ.1) THEN
C  proton PDFs
            IF(IGRP(NPAR).EQ.5) THEN
              IF(ISET(NPAR).EQ.3) THEN
                CALL DOR92HO(XI,SCALE2, UDV, DV, GL, UDB, SB, CB, BB)
                UV = UDV-DV
                UDB = 2.D0*UDB
                DEL = 0.D0
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.4) THEN
                CALL DOR92LO(XI,SCALE2, UDV, DV, GL, UDB, SB, CB, BB)
                UV = UDV-DV
                UDB = 2.D0*UDB
                DEL = 0.D0
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.5) THEN
                CALL DOR94HO(XI,SCALE2,UV, DV, DEL, UDB, SB, GL)
                CB = 0.D0
                BB = 0.D0
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.6) THEN
                CALL DOR94LO(XI,SCALE2,UV, DV, DEL, UDB, SB, GL)
                CB = 0.D0
                BB = 0.D0
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.7) THEN
                CALL DOR94DI(XI,SCALE2,UV, DV, DEL, UDB, SB, GL)
                CB = 0.D0
                BB = 0.D0
                IRET = 0
              ENDIF
              PD(-5) = BB
              PD(-4) = CB
              PD(-3) = SB
              PD(-2) = 0.5D0*(UDB+DEL)
              PD(-1) = 0.5D0*(UDB-DEL)
              PD(0)  = GL
              PD(1)  = DV+PD(-1)
              PD(2)  = UV+PD(-2)
              PD(3)  = PD(-3)
              PD(4)  = PD(-4)
              PD(5)  = PD(-5)
            ENDIF
          ELSE IF(ITYPE(NPAR).EQ.2) THEN
C  pion PDFs
            IF(IGRP(NPAR).EQ.5) THEN
              IF(ISET(NPAR).EQ.1) THEN
                CALL DORPHO (XI,SCALE2, VA, GL, QB, CB, BB)
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.2) THEN
                CALL DORPLO (XI,SCALE2, VA, GL, QB, CB, BB)
                IRET = 0
              ENDIF
              PD(-5) = BB
              PD(-4) = CB
              PD(-3) = QB
              PD(-2) = QB
              PD(-1) = QB
              PD(0)  = GL
              PD(1)  = VA+PD(-1)
              PD(2)  = VA+PD(-2)
              PD(3)  = PD(-3)
              PD(4)  = PD(-4)
              PD(5)  = PD(-5)
            ENDIF
          ELSE IF(ITYPE(NPAR).EQ.3) THEN
C  photon PDFs
            IF(IGRP(NPAR).EQ.5) THEN
              IF(ISET(NPAR).EQ.1) THEN
                CALL DORGH0 (XI,SCALE2, UB, DB, SB, CB, BB, GL)
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.2) THEN
                CALL DORGHO (XI,SCALE2, UB, DB, SB, CB, BB, GL)
                IRET = 0
              ELSE IF(ISET(NPAR).EQ.3) THEN
                CALL DORGLO (XI,SCALE2, UB, DB, SB, CB, BB, GL)
                IRET = 0
              ENDIF
              PD(-5) = BB/137.D0
              PD(-4) = CB/137.D0
              PD(-3) = SB/137.D0
              PD(-2) = UB/137.D0
              PD(-1) = DB/137.D0
              PD(0)  = GL/137.D0
              PD(1)  = PD(-1)
              PD(2)  = PD(-2)
              PD(3)  = PD(-3)
              PD(4)  = PD(-4)
              PD(5)  = PD(-5)
            ENDIF
          ELSE IF(ITYPE(NPAR).EQ.20) THEN
C  Pomeron pdfs
            MODE = IGRP(NPAR)
            IF(MODE.EQ.1) THEN
              PD(0) = 6.D0*(ONE-XI)**5*PARMDL(26)
              IRET = 0
            ELSE IF(MODE.EQ.2) THEN
              PD(0) = 6.D0*XI*(ONE-XI)*PARMDL(26)
              IRET = 0
            ELSE IF(MODE.EQ.3) THEN
              PD(0) = (0.18D0/XI+5.46D0)*(ONE-XI)*PARMDL(26)
              IRET = 0
            ELSE IF(MODE.EQ.4) THEN
              CALL CKMTPD(45,XI,SCALE2,PD)
              IRET = 0
            ENDIF
          ENDIF
C
C  external PDFs
C
        ELSE IF(IEXT(NPAR).EQ.1) THEN
C  PDFLIB call: old numbering
          IF(NPAR.NE.NPAOLD) THEN
            PARAM(1) = 'MODE'
            PARAM(2) = ' '
            VALUE(1) = ABS(IGRP(NPAR))
            CALL PDFSET(PARAM,VALUE)
          ENDIF
          SCALE = SQRT(SCALE2)
          CALL STRUCTM(XI,SCALE,PD(2),PD(1),PD(-2),PD(-1),PD(-3),
     &      PD(-4),PD(-5),PD(-6),PD(0))
          DO 110 I=3,6
            PD(I) = PD(-I)
 110      CONTINUE
          IF(ABS(IGRP(NPAR)).LT.200) THEN
            PD(1) = PD(1)+PD(-1)
            PD(2) = PD(2)+PD(-2)
          ENDIF
          IRET = 0
        ELSE IF(IEXT(NPAR).EQ.2) THEN
C  PDFLIB call: new numbering
          IF(NPAR.NE.NPAOLD) THEN
            PARAM(1) = 'NPTYPE'
            PARAM(2) = 'NGROUP'
            PARAM(3) = 'NSET'
            PARAM(4) = ' '
            VALUE(1) = ITYPE(NPAR)
            VALUE(2) = ABS(IGRP(NPAR))
            VALUE(3) = ISET(NPAR)
            CALL PDFSET(PARAM,VALUE)
          ENDIF
          SCALE = SQRT(SCALE2)
          CALL PFTOPDG(XI,SCALE,PD)
          IRET = 0
        ELSE IF(IEXT(NPAR).EQ.3) THEN
C  PHOLIB call: version 2.0
          CALL PHVAL(IGRP(NPAR),ISET(NPAR),XI,SCALE2,PD,IRET)
          IF(IRET.LT.0) THEN
            WRITE(6,'(/1X,A,I2)') 'PDIS:ERROR:PHVAL RETURN CODE',IRET
            CALL POABRT
          ENDIF
          IRET = 0
C
C  photon PDFs depending on photon virtuality
C
        ELSE IF(IEXT(NPAR).EQ.4) THEN
          IF(IGRP(NPAR).EQ.1) THEN
C  Schuler/Sjostrand PDF (interface to single precision)
            XR = XI
            Q2R = SCALE2
            P2R = 0.
            IF(IPAMDL(115).GE.2) P2R = PARVI(NPAR)
            CALL SASGAM(ISET(NPAR),XR,Q2R,P2R,F2GM,XPDFGM)
            DO 120 I=-6,6
              PD(I) = DBLE(XPDFGM(I))
 120        CONTINUE
            IRET = 0
          ENDIF
        ENDIF
        IF(IRET.NE.0) THEN
          WRITE(6,'(/1X,A,/10X,5I6)')
     &      'PDIS:ERROR:UNSUPPORTED PDF(NPAR,IEXT,ITYPE,IGRP,ISET)',
     &      NPAR,IEXT(NPAR),ITYPE(NPAR),IGRP(NPAR),ISET(NPAR)
          CALL POABRT
        ENDIF
C  error in NPAR
      ELSE
        WRITE(6,'(/1X,A,I5)') 'PDIS:ERROR:INVALID NPAR(1,2) ',NPAR
        CALL POABRT
      ENDIF
      NPAOLD = NPAR
C
C  valence quark treatment
C
      IF(ITYPE(NPAR).EQ.3) THEN
C  photon conventions (correct PDFLIB bug)
        PD(1) = PD(-1)
        PD(2) = PD(-2)
      ELSE IF(ITYPE(NPAR).EQ.2) THEN
C  pion conventions
        IF(IPARID(NPAR).EQ.111) THEN
          DUMMY  = PD(1)-PD(-1)
          PD(-1) = PD(-1)+DUMMY/2.D0
          PD(1)  = PD(-1)
          DUMMY  = PD(2)-PD(-2)
          PD(-2) = PD(-2)+DUMMY/2.D0
          PD(2)  = PD(-2)
        ELSE
          DUMMY  = PD(-1)
          PD(-1) = PD(1)
          PD(1)  = DUMMY
        ENDIF
      ELSE IF(ITYPE(NPAR).EQ.1) THEN
C  nucleon conventions
        IF(ABS(IPARID(NPAR)).EQ.2112) THEN
          DUMMY = PD(1)
          PD(1) = PD(2)
          PD(2) = DUMMY
        ENDIF
      ENDIF
C  antiparticle
      IF(IPARID(NPAR).LT.0) THEN
        DO 140 I=1,6
          DUMMY=PD(I)
          PD(I)=PD(-I)
          PD(-I)=DUMMY
 140    CONTINUE
      ENDIF
C  remove valence quarks
      IF(IPAVA(NPAR).EQ.0) THEN
        DO 200 I=1,4
          PD(I) = MIN(PD(-I),PD(I))
          PD(-I) = PD(I)
 200    CONTINUE
      ENDIF
C  debug information
      IF(IDEB(37).GE.25) THEN
        WRITE(6,'(1X,A,2E12.4,I4/,2X,A,6E10.3,/2X,A,E10.3,
     &    /2X,A,6E10.3)') 'PHOPDF:DEBUG:X,SCALE**2,NPAR',X,SCALE2,NPAR,
     &    'PD(-6..-1)',(PD(I),I=-6,-1),
     &    'PD(0)     ',PD(0),
     &    'PD(1..6)  ',(PD(I),I=1,6)
      ENDIF
      END
C
C
      SUBROUTINE POPDFB(IQ,X,SCALE2,PVIRT,FXP)
C***************************************************************
C
C     contribution of the photon PDF due to box graph
C
C     input:      IQ       quark flavour 
C                 SCALE2   scale (GeV**2)
C                 X        parton momentum fraction
C                 PVIRT    photon virtuality (GeV**2, positive)
C                 FXP      x*f(x,Q**2), x times parton denisty
C
C***************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0)
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      PARAMETER (NMXJ=40)
      CHARACTER*10 REJTIT
      COMMON /REJSTA/ IFAIL(NMXJ),REJTIT(NMXJ)
      COMMON /HACONS/ PI,PI2,PI4,GEV2MB
C
      DIMENSION QM(6)
      DATA QM / 0.2D0,0.25D0,0.5D0,1.6D0,5.D0,174.D0 /
C
      FXP = 0.D0
      I = ABS(IQ)
      QM2 = QM(I)**2
      BBE = (1.D0-X)*SCALE2
      IF(BBE.LE.ZERO) THEN
        IF(IDEB(27).GE.5) WRITE(6,'(1X,A,4E10.3)')
     &    'POPDFP:WARNING:over mass limit (X,Q2,P2,QM)',X,SCALE2,
     &    PVIRT,QM(I)
        RETURN
      ENDIF
C
      FXP = X*(4.D0-3.D0*MOD(I,2))/9.D0*3.D0/(2.D0*137.D0*PI)
     &  *((X**2+(1.D0-X)**2)*LOG(BBE/(QM2*X))+8.D0*X*(1.D0-X)-1.D0)
C
      IF(IDEB(27).GE.20) WRITE(6,'(1X,A,I3,5E10.3)')
     &  'POPDFP:DEBUG:X,Q2,P2,QM',I,X,SCALE2,PVIRT,QM(I),FXP
      END
C
C
      SUBROUTINE SETPDF(IDPDG,ITYP,IPAR,ISET,IEXT,IPAVAL,MODE)
C***************************************************************
C
C     assigns  PDF numbers to particles
C
C     input:      IDPDG    PDG number of particle
C                 ITYP     particle type
C                 IPAR     PDF paramertization
C                 ISET     number of set
C                 IEXT     library number for PDF calculation
C                 IPAVAL   1 PDF with valence quarks
C                          0 PDF without valence quarks
C                 MODE     -1   add entry to table
C                           1   read from table
C                           2   output of table
C
C***************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /HADVAL/ ECMN,PCMN,SECM,SPCM,XPSUB,XTSUB,
     &                IDEQP(2),IDEQB(2),IHFLD(2,2),IHFLS(2)
C
      DIMENSION IPDFS(5,20)
      DATA IENTRY / 0 /
C
      IF(MODE.EQ.1) THEN
        I = 1
        IF(IDPDG.EQ.777) THEN
          IDCMP = IDEQP(1)
          IPAVAL = IHFLS(1)
        ELSE IF(IDPDG.EQ.888) THEN
          IDCMP = IDEQP(2)
          IPAVAL = IHFLS(2)
        ELSE
          IDCMP = IDPDG
          IPAVAL = 1
        ENDIF
200     CONTINUE
          IF(IDCMP.EQ.IPDFS(1,I)) THEN
            ITYP = IPDFS(2,I)
            IPAR = IPDFS(3,I)
            ISET = IPDFS(4,I)
            IEXT = IPDFS(5,I)
            IF(IDEB(80).GE.15) WRITE(6,'(1X,A,I7,5X,3I4)')
     &        'SETPDF:DEBUG:ID,IPAR,ISET,IEXT',IDCMP,IPAR,ISET,IEXT
            RETURN
          ENDIF
          I = I+1
          IF(I.GT.IENTRY) THEN
            WRITE(6,'(/1X,A,I7)') 'SETPDF:WARNING: NO PDF ASSIGNED',
     &        IDCMP
            CALL POABRT
          ENDIF
        GOTO 200
      ELSE IF(MODE.EQ.-1) THEN
        DO 50 I=1,IENTRY
          IF(IDPDG.EQ.IPDFS(1,I)) THEN
            WRITE(6,'(/1X,A,5I6)')
     &        'SETPDF:WARNING: OVERWRITE OLD PARTICLE PDF',
     &        IDPDG,IPDFS(2,I),IPDFS(3,I),IPDFS(4,I),IPDFS(5,I)
            GOTO 100
          ENDIF
 50     CONTINUE
        I = IENTRY+1
        IENTRY = I
 100    CONTINUE
        IPDFS(1,I) = IDPDG
        IF(IDPDG.EQ.45) THEN
          ITYP1 = 20
        ELSE IF(IDPDG.EQ.22) THEN
          ITYP1 = 3
        ELSE IF(ABS(IDPDG).LT.1000) THEN
          ITYP1 = 2
        ELSE
          ITYP1 = 1
        ENDIF
        IPDFS(2,I) = ITYP1
        IPDFS(3,I) = IPAR
        IPDFS(4,I) = ISET
        IPDFS(5,I) = IEXT
      ELSE IF(MODE.EQ.-2) THEN
        WRITE(6,'(/1X,A)') 'SETPDF:PDFS ASSIGNED BY USER:'
        DO 150 I=1,IENTRY
          WRITE(6,'(5X,I4,A,I7,A,4I5)') I,'  PARTICLE:',IPDFS(1,I),
     &      '   PDF-SET  ',IPDFS(2,I),IPDFS(3,I),IPDFS(4,I),IPDFS(5,I)
 150    CONTINUE
      ELSE
        WRITE(6,'(/1X,A,I5)') 'SETPDF:ERROR: UNSUPPORTED MODE ',MODE
      ENDIF
      END
C
C
      SUBROUTINE GETPDF(NPAR,PDFNA,ALA,Q2MI,Q2MA,XMI,XMA)
C***************************************************************
C
C     get PDF information
C
C     input:      NPAR     1  first PDF in COMMON /PARPDF/
C                          2  second PDF in COMMON /PARPDF/
C
C     output:     PDFNA    name of PDf parametrization
C                 ALA      QCD LAMBDA (4 flavours if possible)
C                 Q2MI     minimal Q2
C                 Q2MA     maximal Q2
C                 XMI      minimal X
C                 XMA      maximal X
C
C***************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      CHARACTER*8 PDFNA
C
C  PHOLIB 4.15 common
      COMMON /W50512/ QCDL4,QCDL5
      COMMON /W50513/ XMIN,XMAX,Q2MIN,Q2MAX
C  PHOPDF version 2.0 common
      PARAMETER (MAXS=6,MAXP=10)
      CHARACTER*4 CHPAR
      COMMON/PHCOM1/ XLIM(MAXP,0:MAXS,2), Q2LIM(MAXP,0:MAXS,2),
     & NSET(MAXP,2),NFL(MAXP)
      COMMON/PHCOM2/ ALM(MAXP,0:MAXS),CHPAR(MAXP),IORD(MAXP,-MAXS:MAXS)
C
C  global PDF numbers
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      DIMENSION PARAM(20),VALUE(20)
      CHARACTER*20 PARAM
C
      IF((NPAR.NE.1).AND.(NPAR.NE.2)) THEN
        WRITE(6,'(/1X,A,I6)') 'GETPDF:ERROR: INVALID PDF-NUMBER (1,2)',
     &    NPAR
        CALL POABRT
      ENDIF
      ALA = 0.D0
C
      IF(IEXT(NPAR).EQ.0) THEN
C  internal parametrizations
        IF(ITYPE(NPAR).EQ.1) THEN
C  proton PDFs
          IF(IGRP(NPAR).EQ.5) THEN
            IF(ISET(NPAR).EQ.3) THEN
              ALA    = 0.2D0
              Q2MI   = 0.3D0
              PDFNA  = 'GRV92 HO'
            ELSE IF(ISET(NPAR).EQ.4) THEN
              ALA    = 0.2D0
              Q2MI   = 0.25D0
              PDFNA  = 'GRV92 LO'
            ELSE IF(ISET(NPAR).EQ.5) THEN
              ALA    = 0.2D0
              Q2MI   = 0.4D0
              PDFNA  = 'GRV94 HO'
            ELSE IF(ISET(NPAR).EQ.6) THEN
              ALA    = 0.2D0
              Q2MI   = 0.4D0
              PDFNA  = 'GRV94 LO'
            ELSE IF(ISET(NPAR).EQ.7) THEN
              ALA    = 0.2D0
              Q2MI   = 0.4D0
              PDFNA  = 'GRV94 DI'
            ENDIF
          ENDIF
        ELSE IF(ITYPE(NPAR).EQ.2) THEN
C  pion PDFs
          IF(IGRP(NPAR).EQ.5) THEN
            IF(ISET(NPAR).EQ.1) THEN
              ALA    = 0.2D0
              Q2MI   = 0.3D0
              PDFNA  = 'GRV-P HO'
            ELSE IF(ISET(NPAR).EQ.2) THEN
              ALA    = 0.2D0
              Q2MI   = 0.25D0
              PDFNA  = 'GRV-P LO'
            ENDIF
          ENDIF
        ELSE IF(ITYPE(NPAR).EQ.3) THEN
C  photon PDFs
          IF(IGRP(NPAR).EQ.5) THEN
            IF(ISET(NPAR).EQ.1) THEN
              ALA    = 0.2D0
              Q2MI   = 0.3D0
              PDFNA  = 'GRV-G LH'
            ELSE IF(ISET(NPAR).EQ.2) THEN
              ALA    = 0.2D0
              Q2MI   = 0.3D0
              PDFNA  = 'GRV-G HO'
            ELSE IF(ISET(NPAR).EQ.3) THEN
              ALA    = 0.2D0
              Q2MI   = 0.25D0
              PDFNA  = 'GRV-G LO'
            ENDIF
          ENDIF
        ELSE IF(ITYPE(NPAR).EQ.20) THEN
C  pomeron PDFs
          IF(IGRP(NPAR).EQ.4) THEN
            CALL CKMTPA(45,XMI,XMA,ALA,Q2MI,Q2MA,PDFNA)
          ELSE
            ALA    = 0.3D0
            Q2MI   = 2.D0
            PDFNA  = 'POM-PDF1'
          ENDIF
        ENDIF
      ELSE IF(IEXT(NPAR).EQ.1) THEN
C  PDFLIB call: old numbering
        PARAM(1) = 'MODE'
        PARAM(2) = ' '
        VALUE(1) = IGRP(NPAR)
        CALL PDFSET(PARAM,VALUE)
        Q2MI = Q2MIN
        Q2MA = Q2MAX
        XMI  = XMIN
        XMA  = XMAX
        ALA  = QCDL4
        PDFNA = 'PDFLIB1'
      ELSE IF(IEXT(NPAR).EQ.2) THEN
C  PDFLIB call: new numbering
        PARAM(1) = 'NPTYPE'
        PARAM(2) = 'NGROUP'
        PARAM(3) = 'NSET'
        PARAM(4) = ' '
        VALUE(1) = ITYPE(NPAR)
        VALUE(2) = IGRP(NPAR)
        VALUE(3) = ISET(NPAR)
        CALL PDFSET(PARAM,VALUE)
        Q2MI = Q2MIN
        Q2MA = Q2MAX
        XMI  = XMIN
        XMA  = XMAX
        ALA  = QCDL4
        PDFNA = 'PDFLIB2'
      ELSE IF(IEXT(NPAR).EQ.3) THEN
C  PHOLIB interface
        ALA  = ALM(IGRP(NPAR),ISET(NPAR))
        Q2MI = 2.D0
        PDFNA = CHPAR(IGRP(NPAR))
      ELSE IF(IEXT(NPAR).EQ.4) THEN
C  photon PDFs depending on virtualities
        IF(IGRP(NPAR).EQ.1) THEN
C  Schuler/Sjostrand parametrization
          ALA = 0.2D0
          IF(ISET(NPAR).EQ.1) THEN
            Q2MI = 0.2D0
            PDFNA = 'SaS-1D  '
          ELSE IF(ISET(NPAR).EQ.2) THEN
            Q2MI = 0.2D0
            PDFNA = 'SaS-1M  '
          ELSE IF(ISET(NPAR).EQ.3) THEN
            Q2MI = 2.D0
            PDFNA = 'SaS-2D  '
          ELSE IF(ISET(NPAR).EQ.4) THEN
            Q2MI = 2.D0
            PDFNA = 'SaS-2M  '
          ENDIF
        ENDIF
      ENDIF
      IF(ALA.LT.0.01D0) THEN
        WRITE(6,'(/1X,A,/10X,5I6)')
     &    'GETPDF:ERROR:UNSUPPORTED PDF(NPAR,IEXT,ITYPE,IGRP,ISET)',
     &    NPAR,IEXT(NPAR),ITYPE(NPAR),IGRP(NPAR),ISET(NPAR)
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE ACTPDF(IDPDG,PXMASS,K)
C***************************************************************
C
C     activate PDF for QCD calculations
C
C     input:      IDPDG    PDG particle number
C                 PXMASS   particle mass (photon: virtuality (GeV**2))
C                 K        1  first PDF in COMMON /PARPDF/
C                          2  second PDF in COMMON /PARPDF/
C                         -2  write current settings
C
C     output:     COMMON /PARPDF/
C
C***************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C
      PARAMETER (NMAXD=100)
      COMMON /DEBUG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,
     &  KSTRG,KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      IF(K.GT.0) THEN
        CALL SETPDF(IDPDG,ITYPE(K),IGRP(K),ISET(K),IEXT(K),IPAVA(K),1)
        IPARID(K) = IDPDG
        PARVI(K) = PXMASS
        CALL GETPDF(K,PDFNAM(K),PDFLAM(K),PDFQ2M(K),Q2MA,XMI,XMA)
        IF(IDEB(2).GE.20) THEN
          WRITE(6,'(1X,A)')
     &      'ACTPDF:DEBUG:LAMBDA,Q2MIN,NAME,ITYPE,IPAR,ISET,IEXT,PAR'
          WRITE(6,'(1X,A,I2,2E12.3,2X,A8,4I4,I7,E11.3)') 'SIDE',K,
     &      PDFLAM(K),PDFQ2M(K),PDFNAM(K),ITYPE(K),IGRP(K),ISET(K),
     &      IEXT(K),IPARID(K),PARVI(K)
        ENDIF
        NPAOLD = K
      ELSE IF(K.EQ.-2) THEN
        WRITE(6,'(1X,A)')
     &    'ACTPDF:DEBUG:LAMBDA,Q2MIN,NAME,ITYPE,IPAR,ISET,IEXT,PAR'
        WRITE(6,'(1X,A,2E12.3,2X,A8,4I4,I7,E11.3)') 'SIDE 1:',PDFLAM(1),
     &    PDFQ2M(1),PDFNAM(1),ITYPE(1),IGRP(1),ISET(1),IEXT(1),
     &    IPARID(1),PARVI(1)
        WRITE(6,'(1X,A,2E12.3,2X,A8,4I4,I7,E11.3)') 'SIDE 2:',PDFLAM(2),
     &    PDFQ2M(2),PDFNAM(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),
     &    IPARID(2),PARVI(2)
      ELSE
        WRITE(6,'(/1X,A,2I4)') 'ACTPDF:ERROR:INVALID ARGUMENTS',
     &    IDPDG,K
        CALL POABRT
      ENDIF
      END
C
C
      SUBROUTINE POSFTU(IDPDG,SCALE2,PMASS)
C*********************************************************************
C
C     structure function test utility
C
C     input:    IDPDG    PDG ID of particle
C               SCALE2   scale (squared)
C               PMASS    mass of particle (neg. possible for photons)
C
C     output:   tables of PDF, sum rule checking, table of F2
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
C
      PARAMETER (ZERO = 0.D0)
C
      CHARACTER*8 PDFNAM
      COMMON /PARPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),PARVI(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD
C
      PARAMETER (IZERO=0)
      DIMENSION PD(-6:6),PDSUM(-6:6),PDAVE(-6:6),FXP(4)
      CHARACTER*8 PDFNA
C
      CALL ACTPDF(IDPDG,PMASS,1)
      CALL GETPDF(1,PDFNA,ALA,Q2MI,Q2MA,XMI,XMA)
C
      WRITE(6,'(/,A)') ' *** Structure Function Test Utility ***'
      WRITE(6,'(A)') ' ======================================='
C
      WRITE(6,'(/,A,3I10)')
     &  ' used structure function:',ITYPE(1),IGRP(1),ISET(1)
      WRITE(6,'(A,A)')     ' corresponds to ',PDFNA
      WRITE(6,'(A,E12.3)') '  used squared scale:',SCALE2
      WRITE(6,'(A,E12.3)') ' particle virtuality:',PMASS
      WRITE(6,'(/1X,A)') 'x times parton densities'
      WRITE(6,'(1X,A)') '    X         PD(-4 - 4)'
      WRITE(6,'(1X,A)')
     &   ' ============================================================'
 10   FORMAT(1X,10E11.4)
C  logarithmic loop over x values
C  upper bound
      XUPPER=0.9999D0
C  lower bound
      XLOWER=1.D-4
C  number of steps
      NSTEP=50
C
      XFIRST=LOG(XLOWER)
      XDELTA=LOG(XUPPER/XLOWER)/DBLE(NSTEP-1)
      DO 100 I=1,NSTEP
        X=EXP(XFIRST)
        XCONTR=X
        CALL PDIS(X,SCALE2,1,PD)
        IF(X.NE.XCONTR) THEN
          WRITE(6,*) ' x changed! old: ',XCONTR,' new: ',X
        ENDIF
        WRITE(6,10) XCONTR,(PD(K),K=-4,4)
        XFIRST=XFIRST+XDELTA
 100  CONTINUE
C
      IF(IDPDG.EQ.22) THEN
        WRITE(6,'(/1X,A)') 
     &   'comparison PDF to contribution due to box diagram'
        WRITE(6,'(1X,A)') '    X   PD(1),PB(1), .... ,PD(4),PB(4)'
        WRITE(6,'(1X,A)')
     &   ' ============================================================'
        XFIRST=LOG(XLOWER)
        XDELTA=LOG(XUPPER/XLOWER)/DBLE(NSTEP-1)
        DO 110 I=1,NSTEP
          X=EXP(XFIRST)
          CALL PDIS(X,SCALE2,1,PD)
          DO 120 K=1,4
            CALL POPDFB(K,X,SCALE2,PMASS,FXP(K))
 120      CONTINUE
          WRITE(6,'(1X,9E11.4)') X,(PD(K),FXP(K),K=1,4)
          XFIRST=XFIRST+XDELTA
 110    CONTINUE
      ENDIF
C
C  check momentum sum rule
      WRITE(6,'(/1X,A)') 'POSFTU: ESTIMATE OF QUARK SUM RULES'
      DO 199 I=-6,6
        PDSUM(I) = ZERO
        PDAVE(I) = ZERO
 199  CONTINUE
      ITER=5000
      DO 200 I=1,ITER
        XX=DBLE(I)/DBLE(ITER)
        IF(XX.EQ.1.D0) XX = 0.999999D0
        CALL PDIS(XX,SCALE2,1,PD)
        DO 202 K=-6,6
          PDSUM(K) = PDSUM(K)+PD(K)/XX
          PDAVE(K) = PDAVE(K)+PD(K)
 202    CONTINUE
 200  CONTINUE
      WRITE(6,'(1X,A)') 
     &  'POSFTU:PARTON-ID,DX-INTEGRAL: Q(X,Q**2), X*Q(X,Q**2)'
      XSUM = ZERO
      DO 204 I=-6,6
        PDSUM(I) = PDSUM(I)/DBLE(ITER)
        PDAVE(I) = PDAVE(I)/DBLE(ITER)
        XSUM = XSUM+PDAVE(I)
        WRITE(6,'(9X,I3,3X,2E15.4)') I,PDSUM(I),PDAVE(I)
 204  CONTINUE
      WRITE(6,'(1X,A)') 'NUMBER OF VALENCE FLAVOURS'
      DO 205 I=1,6
        WRITE(6,'(9X,I3,E12.4)') I,PDSUM(I)-PDSUM(-I)
 205  CONTINUE
      WRITE(6,'(1X,A,E12.4)') 'MOMENTUM SUM RULE',XSUM
      WRITE(6,'(A/)') ' ============================================='
C
C  table of F2
      WRITE(6,'(/1X,A,E12.4,/1X,A)')
     &  'POSFTU: TABLE OF X, F2(X,Q**2) FOR Q**2',SCALE2,
     &  '-----------------------------------------------------'
      ITER=100
      DO 300 I=1,ITER
        XX=DBLE(I)/DBLE(ITER)
        IF(XX.EQ.1.D0) XX = 0.9999D0
        CALL PDIS(XX,SCALE2,1,PD)
        F2 = ZERO
        DO 302 K=-6,6
          IF(K.NE.0) F2 = F2 + (4.D0-3.D0*MOD(ABS(K),2))*PD(K)
 302    CONTINUE
        WRITE(6,'(5X,2E14.5)') XX,F2/9.D0
 300  CONTINUE
      WRITE(6,'(A/)') ' ============================================='
      END
C
C
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                 *
*    G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S     *
*                                                                 *
*                         1994 UPDATE                             *
*                                                                 *
*                 FOR A DETAILED EXPLANATION SEE                  *
*                   M. GLUECK, E.REYA, A.VOGT :                   *
*                   DO-TH 94/24  =  DESY 94-206                   *
*                    (TO APPEAR IN Z. PHYS. C)                    *
*                                                                 *
*   THE PARAMETRIZATIONS ARE FITTED TO THE EVOLVED PARTONS FOR    *
*        Q**2 / GEV**2  BETWEEN   0.4   AND  1.E6                 *
*             X         BETWEEN  1.E-5  AND   1.                  *
*   LARGE-X REGIONS, WHERE THE DISTRIBUTION UNDER CONSIDERATION   *
*   IS NEGLIGIBLY SMALL, WERE EXCLUDED FROM THE FIT.              *
*                                                                 *
*   HEAVY QUARK THRESHOLDS  Q(H) = M(H)  IN THE BETA FUNCTION :   *
*                   M(C)  =  1.5,  M(B)  =  4.5                   *
*   CORRESPONDING LAMBDA(F) VALUES IN GEV FOR  Q**2 > M(H)**2 :   *
*      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.153,                                *
*      NLO :  LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.131.                                *
*   THE NUMBER OF ACTIVE QUARK FLAVOURS IS  NF = 3  EVERYWHERE    *
*   EXCEPT IN THE BETA FUNCTION, I.E. THE HEAVY QUARKS C,B,...    *
*   ARE NOT PRESENT AS PARTONS IN THE Q2-EVOLUTION.               *
*   IF NEEDED, HEAVY QUARK DENSITIES CAN BE TAKEN FROM THE 1991   *
*   GRV PARAMETRIZATION.                                          *
*                                                                 *
*   NLO DISTRIBUTIONS ARE GIVEN IN MS-BAR FACTORIZATION SCHEME    *
*   (SUBROUTINE GRV94HO) AS WELL AS IN THE DIS SCHEME (GRV94DI),  *
*   THE LEADING ORDER PARAMETRIZATION IS PROVIDED BY "GRV94LO".   *
*                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*...INPUT PARAMETERS :
*
*    X   = MOMENTUM FRACTION
*    Q2  = SCALE Q**2 IN GEV**2
*
*...OUTPUT (ALWAYS X TIMES THE DISTRIBUTION) :
*
*    UV  = U(VAL) = U - U(BAR)
*    DV  = D(VAL) = D - D(BAR)
*    DEL = D(BAR) - U(BAR)
*    UDB = U(BAR) + D(BAR)
*    SB  = S = S(BAR)
*    GL  = GLUON
*
*...LO PARAMETRIZATION :
*
       SUBROUTINE DOR94LO (X, Q2, UV, DV, DEL, UDB, SB, GL)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.23
       LAM2 = 0.2322 * 0.2322
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
       S3 = S2 * S
*...UV :
       NU  =  2.284 + 0.802 * S + 0.055 * S2
       AKU =  0.590 - 0.024 * S
       BKU =  0.131 + 0.063 * S
       AU  = -0.449 - 0.138 * S - 0.076 * S2
       BU  =  0.213 + 2.669 * S - 0.728 * S2
       CU  =  8.854 - 9.135 * S + 1.979 * S2
       DU  =  2.997 + 0.753 * S - 0.076 * S2
       UV  = DOR94FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...DV :
       ND  =  0.371 + 0.083 * S + 0.039 * S2
       AKD =  0.376
       BKD =  0.486 + 0.062 * S
       AD  = -0.509 + 3.310 * S - 1.248 * S2
       BD  =  12.41 - 10.52 * S + 2.267 * S2
       CD  =  6.373 - 6.208 * S + 1.418 * S2
       DD  =  3.691 + 0.799 * S - 0.071 * S2
       DV  = DOR94FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...DEL :
       NE  =  0.082 + 0.014 * S + 0.008 * S2
       AKE =  0.409 - 0.005 * S
       BKE =  0.799 + 0.071 * S
       AE  = -38.07 + 36.13 * S - 0.656 * S2
       BE  =  90.31 - 74.15 * S + 7.645 * S2
       CE  =  0.0
       DE  =  7.486 + 1.217 * S - 0.159 * S2
       DEL = DOR94FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...UDB :
       ALX =  1.451
       BEX =  0.271
       AKX =  0.410 - 0.232 * S
       BKX =  0.534 - 0.457 * S
       AGX =  0.890 - 0.140 * S
       BGX = -0.981
       CX  =  0.320 + 0.683 * S
       DX  =  4.752 + 1.164 * S + 0.286 * S2
       EX  =  4.119 + 1.713 * S
       ESX =  0.682 + 2.978 * S
       UDB=DOR94FW(X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...SB :
       ALS =  0.914
       BES =  0.577
       AKS =  1.798 - 0.596 * S
       AS  = -5.548 + 3.669 * DS - 0.616 * S
       BS  =  18.92 - 16.73 * DS + 5.168 * S
       DST =  6.379 - 0.350 * S  + 0.142 * S2
       EST =  3.981 + 1.638 * S
       ESS =  6.402
       SB  = DOR94FS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...GL :
       ALG =  0.524
       BEG =  1.088
       AKG =  1.742 - 0.930 * S
       BKG =        - 0.399 * S2
       AG  =  7.486 - 2.185 * S
       BG  =  16.69 - 22.74 * S  + 5.779 * S2
       CG  = -25.59 + 29.71 * S  - 7.296 * S2
       DG  =  2.792 + 2.215 * S  + 0.422 * S2 - 0.104 * S3
       EG  =  0.807 + 2.005 * S
       ESG =  3.841 + 0.316 * S
       GL =DOR94FW(X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
       RETURN
       END
*
*...NLO PARAMETRIZATION (MS(BAR)) :
*
       SUBROUTINE DOR94HO (X, Q2, UV, DV, DEL, UDB, SB, GL)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.34
       LAM2 = 0.248 * 0.248
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
       S3 = S2 * S
*...UV :
       NU  =  1.304 + 0.863 * S
       AKU =  0.558 - 0.020 * S
       BKU =          0.183 * S
       AU  = -0.113 + 0.283 * S - 0.321 * S2
       BU  =  6.843 - 5.089 * S + 2.647 * S2 - 0.527 * S3
       CU  =  7.771 - 10.09 * S + 2.630 * S2
       DU  =  3.315 + 1.145 * S - 0.583 * S2 + 0.154 * S3
       UV  = DOR94FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...DV :
       ND  =  0.102 - 0.017 * S + 0.005 * S2
       AKD =  0.270 - 0.019 * S
       BKD =  0.260
       AD  =  2.393 + 6.228 * S - 0.881 * S2
       BD  =  46.06 + 4.673 * S - 14.98 * S2 + 1.331 * S3
       CD  =  17.83 - 53.47 * S + 21.24 * S2
       DD  =  4.081 + 0.976 * S - 0.485 * S2 + 0.152 * S3
       DV  = DOR94FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...DEL :
       NE  =  0.070 + 0.042 * S - 0.011 * S2 + 0.004 * S3
       AKE =  0.409 - 0.007 * S
       BKE =  0.782 + 0.082 * S
       AE  = -29.65 + 26.49 * S + 5.429 * S2
       BE  =  90.20 - 74.97 * S + 4.526 * S2
       CE  =  0.0
       DE  =  8.122 + 2.120 * S - 1.088 * S2 + 0.231 * S3
       DEL = DOR94FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...UDB :
       ALX =  0.877
       BEX =  0.561
       AKX =  0.275
       BKX =  0.0
       AGX =  0.997
       BGX =  3.210 - 1.866 * S
       CX  =  7.300
       DX  =  9.010 + 0.896 * DS + 0.222 * S2
       EX  =  3.077 + 1.446 * S
       ESX =  3.173 - 2.445 * DS + 2.207 * S
       UDB=DOR94FW(X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...SB :
       ALS =  0.756
       BES =  0.216
       AKS =  1.690 + 0.650 * DS - 0.922 * S
       AS  = -4.329 + 1.131 * S
       BS  =  9.568 - 1.744 * S
       DST =  9.377 + 1.088 * DS - 1.320 * S + 0.130 * S2
       EST =  3.031 + 1.639 * S
       ESS =  5.837 + 0.815 * S
       SB  = DOR94FS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...GL :
       ALG =  1.014
       BEG =  1.738
       AKG =  1.724 + 0.157 * S
       BKG =  0.800 + 1.016 * S
       AG  =  7.517 - 2.547 * S
       BG  =  34.09 - 52.21 * DS + 17.47 * S
       CG  =  4.039 + 1.491 * S
       DG  =  3.404 + 0.830 * S
       EG  = -1.112 + 3.438 * S  - 0.302 * S2
       ESG =  3.256 - 0.436 * S
       GL =DOR94FW(X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
       RETURN
       END
*
*...NLO PARAMETRIZATION (DIS) :
*
       SUBROUTINE DOR94DI (X, Q2, UV, DV, DEL, UDB, SB, GL)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.34
       LAM2 = 0.248 * 0.248
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
       S3 = S2 * S
*...UV :
       NU  =  2.484 + 0.116 * S + 0.093 * S2
       AKU =  0.563 - 0.025 * S
       BKU =  0.054 + 0.154 * S
       AU  = -0.326 - 0.058 * S - 0.135 * S2
       BU  = -3.322 + 8.259 * S - 3.119 * S2 + 0.291 * S3
       CU  =  11.52 - 12.99 * S + 3.161 * S2
       DU  =  2.808 + 1.400 * S - 0.557 * S2 + 0.119 * S3
       UV  = DOR94FV (X, NU, AKU, BKU, AU, BU, CU, DU)
*...DV :
       ND  =  0.156 - 0.017 * S
       AKD =  0.299 - 0.022 * S
       BKD =  0.259 - 0.015 * S
       AD  =  3.445 + 1.278 * S + 0.326 * S2
       BD  = -6.934 + 37.45 * S - 18.95 * S2 + 1.463 * S3
       CD  =  55.45 - 69.92 * S + 20.78 * S2
       DD  =  3.577 + 1.441 * S - 0.683 * S2 + 0.179 * S3
       DV  = DOR94FV (X, ND, AKD, BKD, AD, BD, CD, DD)
*...DEL :
       NE  =  0.099 + 0.019 * S + 0.002 * S2
       AKE =  0.419 - 0.013 * S
       BKE =  1.064 - 0.038 * S
       AE  = -44.00 + 98.70 * S - 14.79 * S2
       BE  =  28.59 - 40.94 * S - 13.66 * S2 + 2.523 * S3
       CE  =  84.57 - 108.8 * S + 31.52 * S2
       DE  =  7.469 + 2.480 * S - 0.866 * S2
       DEL = DOR94FV (X, NE, AKE, BKE, AE, BE, CE, DE)
*...UDB :
       ALX =  1.215
       BEX =  0.466
       AKX =  0.326 + 0.150 * S
       BKX =  0.956 + 0.405 * S
       AGX =  0.272
       BGX =  3.794 - 2.359 * DS
       CX  =  2.014
       DX  =  7.941 + 0.534 * DS - 0.940 * S + 0.410 * S2
       EX  =  3.049 + 1.597 * S
       ESX =  4.396 - 4.594 * DS + 3.268 * S
       UDB=DOR94FW(X, S, ALX, BEX, AKX, BKX, AGX, BGX, CX, DX, EX, ESX)
*...SB :
       ALS =  0.175
       BES =  0.344
       AKS =  1.415 - 0.641 * DS
       AS  =  0.580 - 9.763 * DS + 6.795 * S  - 0.558 * S2
       BS  =  5.617 + 5.709 * DS - 3.972 * S
       DST =  13.78 - 9.581 * S  + 5.370 * S2 - 0.996 * S3
       EST =  4.546 + 0.372 * S2
       ESS =  5.053 - 1.070 * S  + 0.805 * S2
       SB  = DOR94FS (X, S, ALS, BES, AKS, AS, BS, DST, EST, ESS)
*...GL :
       ALG =  1.258
       BEG =  1.846
       AKG =  2.423
       BKG =  2.427 + 1.311 * S  - 0.153 * S2
       AG  =  25.09 - 7.935 * S
       BG  = -14.84 - 124.3 * DS + 72.18 * S
       CG  =  590.3 - 173.8 * S
       DG  =  5.196 + 1.857 * S
       EG  = -1.648 + 3.988 * S  - 0.432 * S2
       ESG =  3.232 - 0.542 * S
       GL = DOR94FW(X, S, ALG, BEG, AKG, BKG, AG, BG, CG, DG, EG, ESG)
       RETURN
       END
*
*...FUNCTIONAL FORMS OF THE PARAMETRIZATIONS :
*
       FUNCTION DOR94FV (X, N, AK, BK, A, B, C, D)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       DOR94FV=N * X**AK * (1.+ A*X**BK + X * (B + C*DX)) * (1.- X)**D
       RETURN
       END
*
       FUNCTION DOR94FW (X, S, AL, BE, AK, BK, A, B, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       LX = LOG (1./X)
       DOR94FW = (X**AK * (A + X * (B + X*C)) * LX**BK + S**AL
     1      * DEXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
*
       FUNCTION DOR94FS (X, S, AL, BE, AK, AG, B, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       LX = LOG (1./X)
       DOR94FS = S**AL / LX**AK * (1.+ AG*DX + B*X) * (1.- X)**D
     1       * DEXP (-E + SQRT (ES * S**BE * LX))
       RETURN
       END
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                 *
*    G R V  -  P R O T O N  - P A R A M E T R I Z A T I O N S     *
*                                                                 *
*                 FOR A DETAILED EXPLANATION SEE :                *
*              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/07             *
*                                                                 *
*   THE PARAMETRIZATIONS ARE FITTED TO THE PARTON DISTRIBUTIONS   *
*   FOR Q ** 2 BETWEEN MU ** 2 (=  0.25 / 0.30  GEV ** 2  IN LO   *
*   / HO) AND  1.E8 GEV ** 2  AND FOR X BETWEEN  1.E-5  AND  1.   *
*   REGIONS, WHERE THE DISTRIBUTION UNDER CONSIDERATION IS NEG-   *
*   LIGIBLE, I.E. BELOW ABOUT 1.E-4, WERE EXCLUDED FROM THE FIT.  *
*                                                                 *
*              HEAVY QUARK THRESHOLDS  Q(H) = M(H) :              *
*         M(C)  =  1.5,  M(B)  =  4.5,  M(T)  =  100  GEV         *
*                                                                 *
*      CORRESPONDING LAMBDA(F) VALUES FOR F ACTIVE FLAVOURS :     *
*      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.153,   LAMBDA(6)  =  0.082  GEV     *
*      HO :   LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.131,   LAMBDA(6)  =  0.053  GEV     *
*                                                                 *
*   HO DISTRIBUTION REFER TO THE MS-BAR SCHEME OF BARDEEN ET AL.  *
*                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
       SUBROUTINE DOR92LO (X, Q2, UDV, DV, GL, UDB, SB, CB, BB)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.25
       LAM2 = 0.232 * 0.232
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       S2 = S * S
       S3 = S2 * S
C...X * (UV + DV) :
       NUD  = 0.663 + 0.191 * S - 0.041 * S2 + 0.031 * S3
       AKUD = 0.326
       AGUD = -1.97 +  6.74 * S -  1.96 * S2
       BUD  =  24.4 -  20.7 * S +  4.08 * S2
       DUD  =  2.86 +  0.70 * S -  0.02 * S2
       UDV  = DOR92FV (X, NUD, AKUD, AGUD, BUD, DUD)
C...X * DV :
       ND  = 0.579 + 0.283 * S + 0.047 * S2
       AKD = 0.523 - 0.015 * S
       AGD =  2.22 -  0.59 * S -  0.27 * S2
       BD  =  5.95 -  6.19 * S +  1.55 * S2
       DD  =  3.57 +  0.94 * S -  0.16 * S2
       DV  = DOR92FV (X, ND, AKD, AGD, BD, DD)
C...X * G :
       ALG =  0.558
       BEG =  1.218
       AKG =   1.00 -  0.17 * S
       BKG =   0.0
       AGG =   0.0  + 4.879 * S - 1.383 * S2
       BGG =  25.92 - 28.97 * S + 5.596 * S2
       CG  = -25.69 + 23.68 * S - 1.975 * S2
       DG  =  2.537 + 1.718 * S + 0.353 * S2
       EG  =  0.595 + 2.138 * S
       ESG =  4.066
       GL =DOR92FW(X, S, ALG, BEG, AKG, BKG, AGG, BGG, CG, DG, EG, ESG)
C...X * UBAR = X * DBAR :
       ALU =  1.396
       BEU =  1.331
       AKU =  0.412 - 0.171 * S
       BKU =  0.566 - 0.496 * S
       AGU =  0.363
       BGU = -1.196
       CU  =  1.029 + 1.785 * S - 0.459 * S2
       DU  =  4.696 + 2.109 * S
       EU  =  3.838 + 1.944 * S
       ESU =  2.845
       UDB=DOR92FW(X, S, ALU, BEU, AKU, BKU, AGU, BGU, CU, DU, EU, ESU)
C...X * SBAR = X * S :
       SS  =   0.0
       ALS =  0.803
       BES =  0.563
       AKS =  2.082 - 0.577 * S
       AGS = -3.055 + 1.024 * S **  0.67
       BS  =   27.4 -  20.0 * S ** 0.154
       DS  =   6.22
       EST =   4.33 + 1.408 * S
       ESS =   8.27 - 0.437 * S
       SB =DOR92FS (X, S, SS, ALS, BES, AKS, AGS, BS, DS, EST, ESS)
C...X * CBAR = X * C :
       SC  =  0.888
       ALC =   1.01
       BEC =   0.37
       AKC =   0.0
       AGC =   0.0
       BC  =   4.24 - 0.804 * S
       DC  =   3.46 + 1.076 * S
       EC  =   4.61 + 1.490 * S
       ESC =  2.555 + 1.961 * S
       CB  = DOR92FS (X, S, SC, ALC, BEC, AKC, AGC, BC, DC, EC, ESC)
C...X * BBAR = X * B :
       SBO =  1.351
       ALB =   1.00
       BEB =   0.51
       AKB =   0.0
       AGB =   0.0
       BBO =  1.848
       DB  =  2.929 + 1.396 * S
       EB  =   4.71 + 1.514 * S
       ESB =   4.02 + 1.239 * S
       BB  = DOR92FS (X, S, SBO, ALB, BEB, AKB, AGB, BBO, DB, EB, ESB)
       RETURN
       END
C
       SUBROUTINE DOR92HO (X, Q2, UDV, DV, GL, UDB, SB, CB, BB)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.3
       LAM2 = 0.248 * 0.248
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
       S3 = S2 * S
C...X * (UV + DV) :
       NUD  = 0.330 + 0.151 * S - 0.059 * S2 + 0.027 * S3
       AKUD = 0.285
       AGUD = -2.28 + 15.73 * S -  4.58 * S2
       BUD  =  56.7 -  53.6 * S + 11.21 * S2
       DUD  =  3.17 +  1.17 * S -  0.47 * S2 +  0.09 * S3
       UDV  = DOR92FV (X, NUD, AKUD, AGUD, BUD, DUD)
C...X * DV :
       ND  = 0.459 + 0.315 * DS + 0.515 * S
       AKD = 0.624              - 0.031 * S
       AGD =  8.13 -  6.77 * DS +  0.46 * S
       BD  =  6.59 - 12.83 * DS +  5.65 * S
       DD  =  3.98              +  1.04 * S  -  0.34 * S2
       DV  = DOR92FV (X, ND, AKD, AGD, BD, DD)
C...X * G :
       ALG =  1.128
       BEG =  1.575
       AKG =  0.323 + 1.653 * S
       BKG =  0.811 + 2.044 * S
       AGG =   0.0  + 1.963 * S - 0.519 * S2
       BGG =  0.078 +  6.24 * S
       CG  =  30.77 - 24.19 * S
       DG  =  3.188 + 0.720 * S
       EG  = -0.881 + 2.687 * S
       ESG =  2.466
       GL=DOR92FW(X, S, ALG, BEG, AKG, BKG, AGG, BGG, CG, DG, EG, ESG)
C...X * UBAR = X * DBAR :
       ALU =  0.594
       BEU =  0.614
       AKU =  0.636 - 0.084 * S
       BKU =   0.0
       AGU =  1.121 - 0.193 * S
       BGU =  0.751 - 0.785 * S
       CU  =   8.57 - 1.763 * S
       DU  =  10.22 + 0.668 * S
       EU  =  3.784 + 1.280 * S
       ESU =  1.808 + 0.980 * S
       UDB=DOR92FW(X, S, ALU, BEU, AKU, BKU, AGU, BGU, CU, DU, EU, ESU)
C...X * SBAR = X * S :
       SS  =   0.0
       ALS =  0.756
       BES =  0.101
       AKS =  2.942 - 1.016 * S
       AGS =  -4.60 + 1.167 * S
       BS  =   9.31 - 1.324 * S
       DS  =  11.49 - 1.198 * S + 0.053 * S2
       EST =  2.630 + 1.729 * S
       ESS =   8.12
       SB  = DOR92FS (X, S, SS, ALS, BES, AKS, AGS, BS, DS, EST, ESS)
C...X * CBAR = X * C :
       SC  =  0.820
       ALC =   0.98
       BEC =   0.0
       AKC = -0.625 - 0.523 * S
       AGC =   0.0
       BC  =  1.896 + 1.616 * S
       DC  =   4.12 + 0.683 * S
       EC  =   4.36 + 1.328 * S
       ESC =  0.677 + 0.679 * S
       CB  = DOR92FS (X, S, SC, ALC, BEC, AKC, AGC, BC, DC, EC, ESC)
C...X * BBAR = X * B :
       SBO =  1.297
       ALB =   0.99
       BEB =   0.0
       AKB =   0.0  - 0.193 * S
       AGB =   0.0
       BBO =   0.0
       DB  =  3.447 + 0.927 * S
       EB  =   4.68 + 1.259 * S
       ESB =  1.892 + 2.199 * S
       BB  = DOR92FS (X, S, SBO, ALB, BEB, AKB, AGB, BBO, DB, EB, ESB)
       RETURN
       END
C
       FUNCTION DOR92FV (X, N, AK, AG, B, D)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       DOR92FV = N * X**AK * (1.+ AG*DX + B*X) * (1.- X)**D
       RETURN
       END
C
       FUNCTION DOR92FW (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       LX = LOG (1./X)
       DOR92FW = (X**AK * (AG + X * (BG + X*C)) * LX**BK + S**AL
     1      * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
C
       FUNCTION DOR92FS (X, S, ST, AL, BE, AK, AG, B, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       LX = LOG (1./X)
       IF (S .LE. ST) THEN
         DOR92FS = 0.0
       ELSE
         DOR92FS = (S-ST)**AL / LX**AK * (1.+ AG*DX + B*X) * (1.- X)**D
     1          * EXP (-E + SQRT (ES * S**BE * LX))
       END IF
       RETURN
       END
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                 *
*         G R V - P I O N - P A R A M E T R I Z A T I O N S       *
*                                                                 *
*                 FOR A DETAILED EXPLANATION SEE :                *
*              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/16             *
*                                                                 *
*   THE PARAMETRIZATIONS ARE FITTED TO THE PARTON DISTRIBUTIONS   *
*   FOR Q ** 2 BETWEEN MU ** 2 (=  0.25 / 0.30  GEV ** 2  IN LO   *
*   / HO) AND  1.E8 GEV ** 2  AND FOR X BETWEEN  1.E-5  AND  1.   *
*   REGIONS, WHERE THE DISTRIBUTION UNDER CONSIDERATION IS NEG-   *
*   LIGIBLE, I.E. BELOW ABOUT 1.E-4, WERE EXCLUDED FROM THE FIT.  *
*                                                                 *
*              HEAVY QUARK THRESHOLDS  Q(H) = M(H) :              *
*         M(C)  =  1.5,  M(B)  =  4.5,  M(T)  =  100  GEV         *
*                                                                 *
*      CORRESPONDING LAMBDA(F) VALUES FOR F ACTIVE FLAVOURS :     *
*      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.153,   LAMBDA(6)  =  0.082  GEV     *
*      HO :   LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.131,   LAMBDA(6)  =  0.053  GEV     *
*                                                                 *
*   HO DISTRIBUTION REFER TO THE MS-BAR SCHEME OF BARDEEN ET AL.  *
*                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
       SUBROUTINE DORPLO (X, Q2, VAP, GLP, QBP, CBP, BBP)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.25
       LAM2 = 0.232 * 0.232
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
C...X * VALENCE :
       NV  =  0.519 + 0.180 * S - 0.011 * S2
       AKV =  0.499 - 0.027 * S
       AGV =  0.381 - 0.419 * S
       DV  =  0.367 + 0.563 * S
       VAP =  DORFVP (X, NV, AKV, AGV, DV)
C...X * GLUON :
       ALG =  0.599
       BEG =  1.263
       AKG =  0.482 + 0.341 * DS
       BKG =   0.0
       AGG =  0.678 + 0.877 * S  - 0.175 * S2
       BGG =  0.338 - 1.597 * S
       CG  =   0.0  - 0.233 * S  + 0.406 * S2
       DG  =  0.390 + 1.053 * S
       EG  =  0.618 + 2.070 * S
       ESG =  3.676
       GLP =DORFGP(X, S, ALG, BEG, AKG, BKG, AGG, BGG, CG, DG, EG, ESG)
C...X * QBAR (SU(3)-SYMMETRIC SEA) :
       SL  =   0.0
       ALS =   0.55
       BES =   0.56
       AKS =  2.538 - 0.763 * S
       AGS = -0.748
       BS  =  0.313 + 0.935 * S
       DS  =  3.359
       EST =  4.433 + 1.301 * S
       ESS =   9.30 - 0.887 * S
       QBP =  DORFQP (X, S, SL, ALS, BES, AKS, AGS, BS, DS, EST, ESS)
C...X * CBAR = X * C :
       SC  =  0.888
       ALC =   1.02
       BEC =   0.39
       AKC =   0.0
       AGC =   0.0
       BC  =  1.008
       DC  =  1.208 + 0.771 * S
       EC  =   4.40 + 1.493 * S
       ESC =  2.032 + 1.901 * S
       CBP =  DORFQP (X, S, SC, ALC, BEC, AKC, AGC, BC, DC, EC, ESC)
C...X * BBAR = X * B :
       SBO =  1.351
       ALB =   1.03
       BEB =   0.39
       AKB =   0.0
       AGB =   0.0
       BBO =   0.0
       DB  =  0.697 + 0.855 * S
       EB  =   4.51 + 1.490 * S
       ESB =  3.056 + 1.694 * S
       BBP =  DORFQP (X, S, SBO, ALB, BEB, AKB, AGB, BBO, DB, EB, ESB)
       RETURN
       END
C
C
       SUBROUTINE DORPHO (X, Q2, VAP, GLP, QBP, CBP, BBP)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.3
       LAM2 = 0.248 * 0.248
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
C...X * VALENCE :
       NV  =  0.456 + 0.150 * DS + 0.112 * S - 0.019 * S2
       AKV =  0.505 - 0.033 * S
       AGV =  0.748 - 0.669 * DS - 0.133 * S
       DV  =  0.365 + 0.197 * DS + 0.394 * S
       VAP =  DORFVP (X, NV, AKV, AGV, DV)
C...X * GLUON :
       ALG =  1.096
       BEG =  1.371
       AKG =  0.437 - 0.689 * DS
       BKG = -0.631
       AGG =  1.324 - 0.441 * DS - 0.130 * S
       BGG = -0.955 + 0.259 * S
       CG  =  1.075 - 0.302 * S
       DG  =  1.158 + 1.229 * S
       EG  =   0.0  + 2.510 * S
       ESG =  2.604 + 0.165 * S
       GLP =DORFGP(X, S, ALG, BEG, AKG, BKG, AGG, BGG, CG, DG, EG, ESG)
C...X * QBAR (SU(3)-SYMMETRIC SEA) :
       SL  =   0.0
       ALS =   0.85
       BES =   0.96
       AKS = -0.350 + 0.806 * S
       AGS = -1.663
       BS  =  3.148
       DS  =  2.273 + 1.438 * S
       EST =  3.214 + 1.545 * S
       ESS =  1.341 + 1.938 * S
       QBP =  DORFQP (X, S, SL, ALS, BES, AKS, AGS, BS, DS, EST, ESS)
C...X * CBAR = X * C :
       SC  =  0.820
       ALC =   0.98
       BEC =   0.0
       AKC =   0.0  - 0.457 * S
       AGC =   0.0
       BC  =  -1.00 +  1.40 * S
       DC  =  1.318 + 0.584 * S
       EC  =   4.45 + 1.235 * S
       ESC =  1.496 + 1.010 * S
       CBP =  DORFQP (X, S, SC, ALC, BEC, AKC, AGC, BC, DC, EC, ESC)
C...X * BBAR = X * B :
       SBO =  1.297
       ALB =   0.99
       BEB =   0.0
       AKB =   0.0  - 0.172 * S
       AGB =   0.0
       BBO =   0.0
       DB  =  1.447 + 0.485 * S
       EB  =   4.79 + 1.164 * S
       ESB =  1.724 + 2.121 * S
       BBP =  DORFQP (X, S, SBO, ALB, BEB, AKB, AGB, BBO, DB, EB, ESB)
       RETURN
       END
C
       FUNCTION DORFVP (X, N, AK, AG, D)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       DORFVP = N * X**AK * (1.+ AG*DX) * (1.- X)**D
       RETURN
       END
C
       FUNCTION DORFGP (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       LX = LOG (1./X)
       DORFGP = (X**AK * (AG + BG*DX + C*X) * LX**BK + S**AL
     1       * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
C
       FUNCTION DORFQP (X, S, ST, AL, BE, AK, AG, B, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       DX = SQRT (X)
       LX = LOG (1./X)
       IF (S .LE. ST) THEN
          DORFQP = 0.0
       ELSE
          DORFQP = (S-ST)**AL / LX**AK * (1.+ AG*DX + B*X) * (1.- X)**D
     1           * EXP (-E + SQRT (ES * S**BE * LX))
       END IF
       RETURN
       END
C
C
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                 *
*      G R V - P H O T O N - P A R A M E T R I Z A T I O N S      *
*                                                                 *
*                 FOR A DETAILED EXPLANATION SEE :                *
*              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/31             *
*                                                                 *
*    THE OUTPUT IS ALWAYS   1./ ALPHA(EM) * X * PARTON DENSITY    *
*                                                                 *
*   THE PARAMETRIZATIONS ARE FITTED TO THE PARTON DISTRIBUTIONS   *
*   FOR Q ** 2 BETWEEN MU ** 2 (=  0.25 / 0.30  GEV ** 2  IN LO   *
*   / HO) AND  1.E6 GEV ** 2  AND FOR X BETWEEN  1.E-5  AND  1.   *
*                                                                 *
*              HEAVY QUARK THRESHOLDS  Q(H) = M(H) :              *
*         M(C)  =  1.5,  M(B)  =  4.5,  M(T)  =  100  GEV         *
*                                                                 *
*      CORRESPONDING LAMBDA(F) VALUES FOR F ACTIVE FLAVOURS :     *
*      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.153,   LAMBDA(6)  =  0.082  GEV     *
*      HO :   LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.131,   LAMBDA(6)  =  0.053  GEV     *
*                                                                 *
*      HO DISTRIBUTIONS REFER TO THE DIS(GAMMA) SCHEME, SEE :     *
*              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/26             *
*                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
       SUBROUTINE DORGLO (X, Q2, UL, DL, SL, CL, BL, GL)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.25
       LAM2 = 0.232 * 0.232
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       SS = SQRT (S)
       S2 = S * S
C...X * U = X * UBAR :
       AL =  1.717
       BE =  0.641
       AK =  0.500 - 0.176 * S
       BK = 15.00  - 5.687 * SS - 0.552 * S2
       AG =  0.235 + 0.046 * SS
       BG =  0.082 - 0.051 * S  + 0.168 * S2
       C  =   0.0  + 0.459 * S
       D  =  0.354 - 0.061 * S
       E  =  4.899 + 1.678 * S
       ES =  2.046 + 1.389 * S
       UL =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * D = X * DBAR :
       AL =  1.549
       BE =  0.782
       AK =  0.496 + 0.026 * S
       BK =  0.685 - 0.580 * SS + 0.608 * S2
       AG =  0.233 + 0.302 * S
       BG =   0.0  - 0.818 * S  + 0.198 * S2
       C  =  0.114 + 0.154 * S
       D  =  0.405 - 0.195 * S  + 0.046 * S2
       E  =  4.807 + 1.226 * S
       ES =  2.166 + 0.664 * S
       DL  =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * G :
       AL =  0.676
       BE =  1.089
       AK =  0.462 - 0.524 * SS
       BK =  5.451              - 0.804 * S2
       AG =  0.535 - 0.504 * SS + 0.288 * S2
       BG =  0.364 - 0.520 * S
       C  = -0.323              + 0.115 * S2
       D  =  0.233 + 0.790 * S  - 0.139 * S2
       E  =  0.893 + 1.968 * S
       ES =  3.432 + 0.392 * S
       GL =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * S = X * SBAR :
       SF =   0.0
       AL =  1.609
       BE =  0.962
       AK =  0.470              - 0.099 * S2
       BK =  3.246
       AG =  0.121 - 0.068 * SS
       BG = -0.090 + 0.074 * S
       C  =  0.062 + 0.034 * S
       D  =   0.0  + 0.226 * S  - 0.060 * S2
       E  =  4.288 + 1.707 * S
       ES =  2.122 + 0.656 * S
       SL =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * C = X * CBAR :
       SF =  0.888
       AL =  0.970
       BE =  0.545
       AK =  1.254 - 0.251 * S
       BK =  3.932              - 0.327 * S2
       AG =  0.658 + 0.202 * S
       BG = -0.699
       C  =  0.965
       D  =   0.0  + 0.141 * S  - 0.027 * S2
       E  =  4.911 + 0.969 * S
       ES =  2.796 + 0.952 * S
       CL =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * B = X * BBAR :
       SF =  1.351
       AL =  1.016
       BE =  0.338
       AK =  1.961 - 0.370 * S
       BK =  0.923 + 0.119 * S
       AG =  0.815 + 0.207 * S
       BG = -2.275
       C  =  1.480
       D  = -0.223 + 0.173 * S
       E  =  5.426 + 0.623 * S
       ES =  3.819 + 0.901 * S
       BL =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       RETURN
       END
C
C
       SUBROUTINE DORGHO (X, Q2, UH, DH, SH, CH, BH, GH)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.3
       LAM2 = 0.248 * 0.248
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       SS = SQRT (S)
       S2 = S * S
C...X * U = X * UBAR :
       AL =  0.583
       BE =  0.688
       AK =  0.449 - 0.025 * S  - 0.071 * S2
       BK =  5.060 - 1.116 * SS
       AG =  0.103
       BG =  0.319 + 0.422 * S
       C  =  1.508 + 4.792 * S  - 1.963 * S2
       D  =  1.075 + 0.222 * SS - 0.193 * S2
       E  =  4.147 + 1.131 * S
       ES =  1.661 + 0.874 * S
       UH =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * D = X * DBAR :
       AL =  0.591
       BE =  0.698
       AK =  0.442 - 0.132 * S  - 0.058 * S2
       BK =  5.437 - 1.916 * SS
       AG =  0.099
       BG =  0.311 - 0.059 * S
       C  =  0.800 + 0.078 * S  - 0.100 * S2
       D  =  0.862 + 0.294 * SS - 0.184 * S2
       E  =  4.202 + 1.352 * S
       ES =  1.841 + 0.990 * S
       DH  =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * G :
       AL =  1.161
       BE =  1.591
       AK =  0.530 - 0.742 * SS + 0.025 * S2
       BK =  5.662
       AG =  0.533 - 0.281 * SS + 0.218 * S2
       BG =  0.025 - 0.518 * S  + 0.156 * S2
       C  = -0.282              + 0.209 * S2
       D  =  0.107 + 1.058 * S  - 0.218 * S2
       E  =   0.0  + 2.704 * S
       ES =  3.071 - 0.378 * S
       GH =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * S = X * SBAR :
       SF =   0.0
       AL =  0.635
       BE =  0.456
       AK =  1.770 - 0.735 * SS - 0.079 * S2
       BK =  3.832
       AG =  0.084 - 0.023 * S
       BG =  0.136
       C  =  2.119 - 0.942 * S  + 0.063 * S2
       D  =  1.271 + 0.076 * S  - 0.190 * S2
       E  =  4.604 + 0.737 * S
       ES =  1.641 + 0.976 * S
       SH =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * C = X * CBAR :
       SF =  0.820
       AL =  0.926
       BE =  0.152
       AK =  1.142 - 0.175 * S
       BK =  3.276
       AG =  0.504 + 0.317 * S
       BG = -0.433
       C  =  3.334
       D  =  0.398 + 0.326 * S  - 0.107 * S2
       E  =  5.493 + 0.408 * S
       ES =  2.426 + 1.277 * S
       CH =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * B = X * BBAR :
       SF =  1.297
       AL =  0.969
       BE =  0.266
       AK =  1.953 - 0.391 * S
       BK =  1.657 - 0.161 * S
       AG =  1.076 + 0.034 * S
       BG = -2.015
       C  =  1.662
       D  =  0.353 + 0.016 * S
       E  =  5.713 + 0.249 * S
       ES =  3.456 + 0.673 * S
       BH =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       RETURN
       END
C
C
       SUBROUTINE DORGH0 (X, Q2, U0, D0, S0, C0, B0, G0)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.3
       LAM2 = 0.248 * 0.248
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       SS = SQRT (S)
       S2 = S * S
C...X * U = X * UBAR :
       AL =  1.447
       BE =  0.848
       AK =  0.527 + 0.200 * S  - 0.107 * S2
       BK =  7.106 - 0.310 * SS - 0.786 * S2
       AG =  0.197 + 0.533 * S
       BG =  0.062 - 0.398 * S  + 0.109 * S2
       C  =          0.755 * S  - 0.112 * S2
       D  =  0.318 - 0.059 * S
       E  =  4.225 + 1.708 * S
       ES =  1.752 + 0.866 * S
       U0 =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * D = X * DBAR :
       AL =  1.424
       BE =  0.770
       AK =  0.500 + 0.067 * SS - 0.055 * S2
       BK =  0.376 - 0.453 * SS + 0.405 * S2
       AG =  0.156 + 0.184 * S
       BG =   0.0  - 0.528 * S  + 0.146 * S2
       C  =  0.121 + 0.092 * S
       D  =  0.379 - 0.301 * S  + 0.081 * S2
       E  =  4.346 + 1.638 * S
       ES =  1.645 + 1.016 * S
       D0  =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * G :
       AL =  0.661
       BE =  0.793
       AK =  0.537 - 0.600 * SS
       BK =  6.389              - 0.953 * S2
       AG =  0.558 - 0.383 * SS + 0.261 * S2
       BG =   0.0  - 0.305 * S
       C  = -0.222              + 0.078 * S2
       D  =  0.153 + 0.978 * S  - 0.209 * S2
       E  =  1.429 + 1.772 * S
       ES =  3.331 + 0.806 * S
       G0 =  DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * S = X * SBAR :
       SF =   0.0
       AL =  1.578
       BE =  0.863
       AK =  0.622 + 0.332 * S  - 0.300 * S2
       BK =  2.469
       AG =  0.211 - 0.064 * SS - 0.018 * S2
       BG = -0.215 + 0.122 * S
       C  =  0.153
       D  =   0.0  + 0.253 * S  - 0.081 * S2
       E  =  3.990 + 2.014 * S
       ES =  1.720 + 0.986 * S
       S0 =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * C = X * CBAR :
       SF =  0.820
       AL =  0.929
       BE =  0.381
       AK =  1.228 - 0.231 * S
       BK =  3.806             - 0.337 * S2
       AG =  0.932 + 0.150 * S
       BG = -0.906
       C  =  1.133
       D  =   0.0  + 0.138 * S  - 0.028 * S2
       E  =  5.588 + 0.628 * S
       ES =  2.665 + 1.054 * S
       C0 =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
C...X * B = X * BBAR :
       SF =  1.297
       AL =  0.970
       BE =  0.207
       AK =  1.719 - 0.292 * S
       BK =  0.928 + 0.096 * S
       AG =  0.845 + 0.178 * S
       BG = -2.310
       C  =  1.558
       D  = -0.191 + 0.151 * S
       E  =  6.089 + 0.282 * S
       ES =  3.379 + 1.062 * S
       B0 =  DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       RETURN
       END
C
C
       FUNCTION DORGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       SX = SQRT (X)
       LX = LOG (1./X)
       DORGF  = (X**AK * (AG + BG * SX + C * X**BK)  +  S**AL
     1       * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
C
       FUNCTION DORGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT DOUBLE PRECISION (A - Z)
       IF (S .LE. SF) THEN
          DORGFS = 0.0
       ELSE
          SX = SQRT (X)
          LX = LOG (1./X)
          DS = S - SF
          DORGFS = (DS * X**AK * (AG + BG * SX + C * X**BK) + DS**AL
     1         * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       END IF
       RETURN
       END
C
C
      SUBROUTINE CKMTPA(IPA,XMI,XMA,ALA,Q2MI,Q2MA,PDFNA)
C**********************************************************************
C
C     PDF based on Regge theory, evolved with .... by ....
C
C     input: IPAR     2212   proton (not installed)
C                       45   Pomeron
C
C     output: parameters of parametrization
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      CHARACTER*8 PDFNA
C
      REAL PROP(40),POMP(40)
      DATA PROP /
     & .230000E+00, .200000E+01, .150200E+00, .120000E+01, .263100E+00,
     & .645200E+00, .354890E+01, .111700E+01, .415000E+00, .768400E-01,
     & .100000E+00, .330000E-01, .352102E-01, .200000E+01, .200000E+01,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .100000E+00, .200000E+01, .100000E+09/
      DATA POMP /
     & .230000E+00, .500000E+01, .150200E+00, .120000E+01, .263100E+00,
     & .645200E+00, .354890E+01, .111700E+01, .415000E+00, .768400E-01,
     & .700000E-01, .700000E-01, .137161E+00, .300000E+01, .200000E+01,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .100000E+00, .500000E+01, .100000E+09/
C
      IF(IPA.EQ.2212) THEN
        ALA  =PROP(1)
        Q2MI = PROP(39)
        Q2MA = PROP(40)
        PDFNA = 'CKMT-PRO'
      ELSE IF(IPA.EQ.45) THEN
        ALA  = POMP(1)
        Q2MI = POMP(39)
        Q2MA = POMP(40)
        PDFNA = 'CKMT-POM'
      ELSE
        WRITE(6,'(1X,A,I7)') 'CKMTPA:ERROR:INVALID PARTICLE CODE',IPA
        STOP
      ENDIF
      XMI = 1.D-4
      XMA = 1.D0
      END
C
C
      SUBROUTINE CKMTPD(IPAR,X,SCALE2,PD)
C**********************************************************************
C
C     PDF based on Regge theory, evolved with .... by ....
C
C     input: IPAR     2212   proton (not installed)
C                       45   Pomeron
C
C     output: PD(-6:6) x*f(x)  parton distribution functions
C            (PDFLIB convention: d = PD(1), u = PD(2) )
C
C**********************************************************************
      DOUBLE PRECISION  X,SCALE2,PD(-6:6),CDN,CUP
      DIMENSION QQ(7)
C
      COMMON/CKMT10/OWLAM,OWLAM2,RLAM,RLAM2,Q02
      COMMON/CKMT11/Q1S
C
      Q2=SNGL(SCALE2)
      Q1S=Q2
      XX=SNGL(X)
C  QCD lambda for evolution
      OWLAM = 0.23D0
      OWLAM2=OWLAM**2
C  Q0**2 for evolution
      Q02 = 2.D0
C
C
C  the conventions are : q(1)=x*u, q(2)=x*d, q(3)=q(4)=x*sbar=x*ubar=...
C                        q(6)=x*charm, q(7)=x*gluon
C
      SB=0.
      IF(Q2-Q02) 1,1,2
    2 SB=LOG(LOG(Q2/OWLAM2)/LOG(Q02/OWLAM2))
    1 CONTINUE
      IF(IPAR.EQ.2212) THEN
*       CALL CKMTPR(XX,SB,QQ
        WRITE(6,'(/1X,A,I6)') 'CKMTPD:ERROR:INVALID PARTICLE',IPAR
        CALL POABRT
      ELSE
        CALL CKMTPO(XX,SB,QQ)
      ENDIF
C
      PD(-6) = 0.D0
      PD(-5) = 0.D0
      PD(-4) = DBLE(QQ(6))
      PD(-3) = DBLE(QQ(3))
      PD(-2) = DBLE(QQ(4))
      PD(-1) = DBLE(QQ(5))
      PD(0)  = DBLE(QQ(7))
      PD(1)  = DBLE(QQ(2))
      PD(2)  = DBLE(QQ(1))
      PD(3)  = DBLE(QQ(3))
      PD(4)  = DBLE(QQ(6))
      PD(5)  = 0.D0
      PD(6)  = 0.D0
      IF(IPAR.EQ.45) THEN
        CDN = (PD(1)-PD(-1))/2.D0
        CUP = (PD(2)-PD(-2))/2.D0
        PD(-1) = PD(-1) + CDN
        PD(-2) = PD(-2) + CUP
        PD(1) = PD(-1)
        PD(2) = PD(-2)
      ENDIF
      END
C
C
      SUBROUTINE CKMTPO(X,S,QQ)
C**********************************************************************
C
C    calculation partons in Pomeron
C
C**********************************************************************
      DIMENSION QQ(7)
C
      DIMENSION F1(25),F2(25),GF(8,20,25),DL(4000)
      EQUIVALENCE (GF(1,1,1),DL(1))
      DATA DELTA/.10/
C
C  RNG=  -.5
C  DEU.NORM. QUARKS,GLUONS,NEW NORM   .6223E+00   .2754E+00   .1372E+01
C  POM.NORM. QUARKS,GLUONS,ALL    .132E+00    .275E+00    .407E+00
      DATA (DL(K),K=    1,   85) /
     & .324159E-01, .324159E-01, .298895E-01, .298895E-01, .298895E-01,
     & .298895E-01, .486150E+00,-.867362E-18, .362035E-01, .362035E-01,
     & .335142E-01, .335151E-01, .335151E-01, .335142E-01, .745381E+00,
     & .399157E-02, .417146E-01, .417146E-01, .388545E-01, .388564E-01,
     & .388564E-01, .388545E-01, .107588E+01, .969559E-02, .493208E-01,
     & .493208E-01, .462819E-01, .462849E-01, .462849E-01, .462819E-01,
     & .148168E+01, .174837E-01, .593251E-01, .593251E-01, .560991E-01,
     & .561035E-01, .561035E-01, .560991E-01, .196422E+01, .276588E-01,
     & .720220E-01, .720220E-01, .686007E-01, .686065E-01, .686065E-01,
     & .686007E-01, .252331E+01, .405154E-01, .876695E-01, .876695E-01,
     & .840445E-01, .840520E-01, .840520E-01, .840445E-01, .315730E+01,
     & .563115E-01, .106489E+00, .106489E+00, .102652E+00, .102662E+00,
     & .102662E+00, .102652E+00, .386313E+01, .752690E-01, .128662E+00,
     & .128662E+00, .124605E+00, .124616E+00, .124616E+00, .124605E+00,
     & .463661E+01, .975686E-01, .154326E+00, .154326E+00, .150039E+00,
     & .150053E+00, .150053E+00, .150039E+00, .547247E+01, .123348E+00,
     & .183571E+00, .183571E+00, .179048E+00, .179063E+00, .179063E+00/
      DATA (DL(K),K=   86,  170) /
     & .179048E+00, .636464E+01, .152698E+00, .216445E+00, .216445E+00,
     & .211676E+00, .211694E+00, .211694E+00, .211676E+00, .730631E+01,
     & .185666E+00, .252948E+00, .252948E+00, .247925E+00, .247946E+00,
     & .247946E+00, .247925E+00, .829017E+01, .222252E+00, .293037E+00,
     & .293037E+00, .287752E+00, .287776E+00, .287776E+00, .287752E+00,
     & .930850E+01, .262414E+00, .336625E+00, .336625E+00, .331070E+00,
     & .331097E+00, .331097E+00, .331070E+00, .103534E+02, .306065E+00,
     & .383587E+00, .383587E+00, .377754E+00, .377785E+00, .377785E+00,
     & .377754E+00, .114166E+02, .353079E+00, .433760E+00, .433760E+00,
     & .427641E+00, .427675E+00, .427675E+00, .427641E+00, .124903E+02,
     & .403294E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .296507E-01, .296507E-01, .258624E-01, .258624E-01, .258624E-01,
     & .258624E-01, .422709E+00,-.173472E-17, .330029E-01, .330029E-01/
      DATA (DL(K),K=  171,  255) /
     & .289773E-01, .289787E-01, .289787E-01, .289773E-01, .642996E+00,
     & .344499E-02, .377610E-01, .377610E-01, .334880E-01, .334910E-01,
     & .334910E-01, .334880E-01, .914159E+00, .828363E-02, .441590E-01,
     & .441590E-01, .396285E-01, .396333E-01, .396333E-01, .396285E-01,
     & .123635E+01, .147501E-01, .523710E-01, .523710E-01, .475730E-01,
     & .475798E-01, .475798E-01, .475730E-01, .160820E+01, .230185E-01,
     & .625514E-01, .625514E-01, .574758E-01, .574848E-01, .574848E-01,
     & .574758E-01, .202705E+01, .332433E-01, .748195E-01, .748195E-01,
     & .694563E-01, .694678E-01, .694678E-01, .694563E-01, .248945E+01,
     & .455440E-01, .892611E-01, .892611E-01, .836006E-01, .836147E-01,
     & .836147E-01, .836006E-01, .299114E+01, .600067E-01, .105928E+00,
     & .105928E+00, .999607E-01, .999776E-01, .999776E-01, .999607E-01,
     & .352735E+01, .766833E-01, .124839E+00, .124839E+00, .118555E+00,
     & .118575E+00, .118575E+00, .118555E+00, .409288E+01, .955921E-01,
     & .145978E+00, .145978E+00, .139368E+00, .139391E+00, .139391E+00,
     & .139368E+00, .468226E+01, .116719E+00, .169300E+00, .169300E+00,
     & .162355E+00, .162382E+00, .162382E+00, .162355E+00, .528987E+01/
      DATA (DL(K),K=  256,  340) /
     & .140017E+00, .194730E+00, .194730E+00, .187441E+00, .187471E+00,
     & .187471E+00, .187441E+00, .591007E+01, .165413E+00, .222167E+00,
     & .222167E+00, .214525E+00, .214559E+00, .214559E+00, .214525E+00,
     & .653724E+01, .192806E+00, .251486E+00, .251486E+00, .243482E+00,
     & .243521E+00, .243521E+00, .243482E+00, .716591E+01, .222070E+00,
     & .282539E+00, .282539E+00, .274165E+00, .274208E+00, .274208E+00,
     & .274165E+00, .779082E+01, .253058E+00, .315161E+00, .315161E+00,
     & .306410E+00, .306458E+00, .306458E+00, .306410E+00, .840695E+01,
     & .285608E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .279581E-01, .279581E-01, .222797E-01, .222797E-01, .222797E-01,
     & .222797E-01, .367732E+00, .867362E-18, .309604E-01, .309604E-01,
     & .249419E-01, .249441E-01, .249441E-01, .249419E-01, .552053E+00,
     & .296633E-02, .350831E-01, .350831E-01, .287126E-01, .287173E-01/
      DATA (DL(K),K=  341,  425) /
     & .287173E-01, .287126E-01, .770476E+00, .704001E-02, .404554E-01,
     & .404554E-01, .337212E-01, .337286E-01, .337286E-01, .337212E-01,
     & .102096E+01, .123504E-01, .471588E-01, .471588E-01, .400495E-01,
     & .400599E-01, .400599E-01, .400495E-01, .130079E+01, .189795E-01,
     & .552518E-01, .552518E-01, .477564E-01, .477700E-01, .477700E-01,
     & .477564E-01, .160637E+01, .269860E-01, .647649E-01, .647649E-01,
     & .568725E-01, .568897E-01, .568897E-01, .568725E-01, .193388E+01,
     & .364007E-01, .757021E-01, .757021E-01, .674022E-01, .674232E-01,
     & .674232E-01, .674022E-01, .227916E+01, .472280E-01, .880430E-01,
     & .880430E-01, .793257E-01, .793507E-01, .793507E-01, .793257E-01,
     & .263802E+01, .594481E-01, .101745E+00, .101745E+00, .926005E-01,
     & .926297E-01, .926297E-01, .926005E-01, .300628E+01, .730184E-01,
     & .116745E+00, .116745E+00, .107164E+00, .107198E+00, .107198E+00,
     & .107164E+00, .337982E+01, .878765E-01, .132961E+00, .132961E+00,
     & .122936E+00, .122974E+00, .122974E+00, .122936E+00, .375469E+01,
     & .103942E+00, .150298E+00, .150298E+00, .139820E+00, .139863E+00,
     & .139863E+00, .139820E+00, .412714E+01, .121118E+00, .168645E+00/
      DATA (DL(K),K=  426,  510) /
     & .168645E+00, .157706E+00, .157754E+00, .157754E+00, .157706E+00,
     & .449366E+01, .139296E+00, .187883E+00, .187883E+00, .176476E+00,
     & .176529E+00, .176529E+00, .176476E+00, .485100E+01, .158356E+00,
     & .207882E+00, .207882E+00, .196000E+00, .196059E+00, .196059E+00,
     & .196000E+00, .519622E+01, .178170E+00, .228506E+00, .228506E+00,
     & .216145E+00, .216209E+00, .216209E+00, .216145E+00, .552665E+01,
     & .198603E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .275294E-01, .275294E-01, .190245E-01, .190245E-01, .190245E-01,
     & .190245E-01, .320228E+00, .000000E+00, .302671E-01, .302671E-01,
     & .212851E-01, .212884E-01, .212884E-01, .212851E-01, .470861E+00,
     & .255059E-02, .338703E-01, .338703E-01, .243988E-01, .244059E-01,
     & .244059E-01, .243988E-01, .642452E+00, .595399E-02, .383922E-01,
     & .383922E-01, .284195E-01, .284305E-01, .284305E-01, .284195E-01/
      DATA (DL(K),K=  511,  595) /
     & .831913E+00, .102638E-01, .438519E-01, .438519E-01, .333669E-01,
     & .333821E-01, .333821E-01, .333669E-01, .103618E+01, .155000E-01,
     & .502475E-01, .502475E-01, .392399E-01, .392595E-01, .392595E-01,
     & .392399E-01, .125172E+01, .216612E-01, .575580E-01, .575580E-01,
     & .460181E-01, .460425E-01, .460425E-01, .460181E-01, .147519E+01,
     & .287272E-01, .657445E-01, .657445E-01, .536635E-01, .536929E-01,
     & .536929E-01, .536635E-01, .170330E+01, .366597E-01, .747539E-01,
     & .747539E-01, .621238E-01, .621582E-01, .621582E-01, .621238E-01,
     & .193297E+01, .454066E-01, .845205E-01, .845205E-01, .713340E-01,
     & .713738E-01, .713738E-01, .713340E-01, .216133E+01, .549027E-01,
     & .949687E-01, .949687E-01, .812194E-01, .812646E-01, .812646E-01,
     & .812194E-01, .238578E+01, .650733E-01, .106015E+00, .106015E+00,
     & .916972E-01, .917480E-01, .917480E-01, .916972E-01, .260395E+01,
     & .758355E-01, .117569E+00, .117569E+00, .102678E+00, .102735E+00,
     & .102735E+00, .102678E+00, .281373E+01, .871004E-01, .129537E+00,
     & .129537E+00, .114070E+00, .114133E+00, .114133E+00, .114070E+00,
     & .301327E+01, .987750E-01, .141824E+00, .141824E+00, .125777E+00/
      DATA (DL(K),K=  596,  680) /
     & .125846E+00, .125846E+00, .125777E+00, .320098E+01, .110764E+00,
     & .154331E+00, .154331E+00, .137703E+00, .137778E+00, .137778E+00,
     & .137703E+00, .337553E+01, .122970E+00, .166962E+00, .166962E+00,
     & .149753E+00, .149833E+00, .149833E+00, .149753E+00, .353582E+01,
     & .135299E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .286766E-01, .286766E-01, .159579E-01, .159579E-01, .159579E-01,
     & .159579E-01, .279430E+00,-.867362E-18, .312327E-01, .312327E-01,
     & .178644E-01, .178691E-01, .178691E-01, .178644E-01, .399186E+00,
     & .219459E-02, .344289E-01, .344289E-01, .204015E-01, .204111E-01,
     & .204111E-01, .204015E-01, .529978E+00, .501953E-02, .382657E-01,
     & .382657E-01, .235713E-01, .235860E-01, .235860E-01, .235713E-01,
     & .668515E+00, .847682E-02, .427243E-01, .427243E-01, .273559E-01,
     & .273758E-01, .273758E-01, .273559E-01, .812075E+00, .125486E-01/
      DATA (DL(K),K=  681,  765) /
     & .477691E-01, .477691E-01, .317212E-01, .317465E-01, .317465E-01,
     & .317212E-01, .957801E+00, .172006E-01, .533547E-01, .533547E-01,
     & .366231E-01, .366539E-01, .366539E-01, .366231E-01, .110327E+01,
     & .223886E-01, .594259E-01, .594259E-01, .420076E-01, .420441E-01,
     & .420441E-01, .420076E-01, .124628E+01, .280584E-01, .659213E-01,
     & .659213E-01, .478149E-01, .478570E-01, .478570E-01, .478149E-01,
     & .138496E+01, .341502E-01, .727749E-01, .727749E-01, .539803E-01,
     & .540280E-01, .540280E-01, .539803E-01, .151767E+01, .405990E-01,
     & .799178E-01, .799178E-01, .604361E-01, .604895E-01, .604895E-01,
     & .604361E-01, .164304E+01, .473372E-01, .872796E-01, .872796E-01,
     & .671134E-01, .671724E-01, .671724E-01, .671134E-01, .175992E+01,
     & .542955E-01, .947896E-01, .947896E-01, .739429E-01, .740075E-01,
     & .740075E-01, .739429E-01, .186739E+01, .614047E-01, .102378E+00,
     & .102378E+00, .808565E-01, .809266E-01, .809266E-01, .808565E-01,
     & .196473E+01, .685965E-01, .109978E+00, .109978E+00, .877881E-01,
     & .878637E-01, .878637E-01, .877881E-01, .205141E+01, .758045E-01,
     & .117525E+00, .117525E+00, .946745E-01, .947553E-01, .947553E-01/
      DATA (DL(K),K=  766,  850) /
     & .946745E-01, .212709E+01, .829655E-01, .124958E+00, .124958E+00,
     & .101456E+00, .101542E+00, .101542E+00, .101456E+00, .219159E+01,
     & .900196E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .318691E-01, .318691E-01, .129081E-01, .129081E-01, .129081E-01,
     & .129081E-01, .244842E+00,-.867362E-18, .343104E-01, .343104E-01,
     & .145076E-01, .145124E-01, .145124E-01, .145076E-01, .337040E+00,
     & .189443E-02, .371900E-01, .371900E-01, .165461E-01, .165557E-01,
     & .165557E-01, .165461E-01, .433081E+00, .422691E-02, .404763E-01,
     & .404763E-01, .189941E-01, .190085E-01, .190085E-01, .189941E-01,
     & .530109E+00, .696769E-02, .441304E-01, .441304E-01, .218150E-01,
     & .218342E-01, .218342E-01, .218150E-01, .626129E+00, .100799E-01,
     & .481031E-01, .481031E-01, .249615E-01, .249853E-01, .249853E-01,
     & .249615E-01, .719221E+00, .135159E-01, .523426E-01, .523426E-01/
      DATA (DL(K),K=  851,  935) /
     & .283837E-01, .284122E-01, .284122E-01, .283837E-01, .807951E+00,
     & .172259E-01, .567940E-01, .567940E-01, .320288E-01, .320619E-01,
     & .320619E-01, .320288E-01, .891154E+00, .211568E-01, .614022E-01,
     & .614022E-01, .358436E-01, .358811E-01, .358811E-01, .358436E-01,
     & .967928E+00, .252549E-01, .661122E-01, .661122E-01, .397750E-01,
     & .398169E-01, .398169E-01, .397750E-01, .103759E+01, .294673E-01,
     & .708708E-01, .708708E-01, .437716E-01, .438176E-01, .438176E-01,
     & .437716E-01, .109966E+01, .337422E-01, .756269E-01, .756269E-01,
     & .477840E-01, .478342E-01, .478342E-01, .477840E-01, .115380E+01,
     & .380302E-01, .803322E-01, .803322E-01, .517659E-01, .518200E-01,
     & .518200E-01, .517659E-01, .119986E+01, .422846E-01, .849423E-01,
     & .849423E-01, .556743E-01, .557322E-01, .557322E-01, .556743E-01,
     & .123782E+01, .464624E-01, .894164E-01, .894164E-01, .594701E-01,
     & .595315E-01, .595315E-01, .594701E-01, .126777E+01, .505242E-01,
     & .937178E-01, .937178E-01, .631181E-01, .631829E-01, .631829E-01,
     & .631181E-01, .128993E+01, .544348E-01, .978144E-01, .978144E-01,
     & .665876E-01, .666556E-01, .666556E-01, .665876E-01, .130457E+01/
      DATA (DL(K),K=  936, 1020) /
     & .581632E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .377668E-01, .377668E-01, .968304E-02, .968304E-02, .968304E-02,
     & .968304E-02, .216392E+00,-.130104E-17, .401066E-01, .401066E-01,
     & .110266E-01, .110291E-01, .110291E-01, .110266E-01, .284113E+00,
     & .164283E-02, .426983E-01, .426983E-01, .126461E-01, .126510E-01,
     & .126510E-01, .126461E-01, .350879E+00, .355790E-02, .454940E-01,
     & .454940E-01, .144965E-01, .145039E-01, .145039E-01, .144965E-01,
     & .414611E+00, .570002E-02, .484493E-01, .484493E-01, .165364E-01,
     & .165462E-01, .165462E-01, .165364E-01, .474149E+00, .802739E-02,
     & .515153E-01, .515153E-01, .187191E-01, .187313E-01, .187313E-01,
     & .187191E-01, .528511E+00, .104932E-01, .546458E-01, .546458E-01,
     & .210009E-01, .210154E-01, .210154E-01, .210009E-01, .577107E+00,
     & .130535E-01, .577962E-01, .577962E-01, .233395E-01, .233563E-01/
      DATA (DL(K),K= 1021, 1105) /
     & .233563E-01, .233395E-01, .619574E+00, .156658E-01, .609249E-01,
     & .609249E-01, .256954E-01, .257143E-01, .257143E-01, .256954E-01,
     & .655725E+00, .182905E-01, .639938E-01, .639938E-01, .280322E-01,
     & .280532E-01, .280532E-01, .280322E-01, .685523E+00, .208909E-01,
     & .669681E-01, .669681E-01, .303170E-01, .303399E-01, .303399E-01,
     & .303170E-01, .709053E+00, .234341E-01, .698172E-01, .698172E-01,
     & .325206E-01, .325454E-01, .325454E-01, .325206E-01, .726501E+00,
     & .258907E-01, .725141E-01, .725141E-01, .346176E-01, .346442E-01,
     & .346442E-01, .346176E-01, .738139E+00, .282352E-01, .750364E-01,
     & .750364E-01, .365866E-01, .366148E-01, .366148E-01, .365866E-01,
     & .744304E+00, .304461E-01, .773653E-01, .773653E-01, .384099E-01,
     & .384396E-01, .384396E-01, .384099E-01, .745388E+00, .325056E-01,
     & .794860E-01, .794860E-01, .400736E-01, .401046E-01, .401046E-01,
     & .400736E-01, .741819E+00, .343996E-01, .813873E-01, .813873E-01,
     & .415670E-01, .415993E-01, .415993E-01, .415670E-01, .734051E+00,
     & .361177E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 1106, 1190) /
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .433773E-01, .433773E-01, .745094E-02, .745094E-02, .745094E-02,
     & .745094E-02, .201612E+00,-.130104E-17, .455998E-01, .455998E-01,
     & .866239E-02, .866443E-02, .866443E-02, .866239E-02, .255046E+00,
     & .149977E-02, .479569E-01, .479569E-01, .100584E-01, .100624E-01,
     & .100624E-01, .100584E-01, .305549E+00, .317779E-02, .503976E-01,
     & .503976E-01, .115911E-01, .115970E-01, .115970E-01, .115911E-01,
     & .351606E+00, .498612E-02, .528804E-01, .528804E-01, .132216E-01,
     & .132293E-01, .132293E-01, .132216E-01, .392560E+00, .688553E-02,
     & .553621E-01, .553621E-01, .149087E-01, .149181E-01, .149181E-01,
     & .149087E-01, .427948E+00, .883486E-02, .578049E-01, .578049E-01,
     & .166165E-01, .166276E-01, .166276E-01, .166165E-01, .457612E+00,
     & .107980E-01, .601739E-01, .601739E-01, .183120E-01, .183246E-01,
     & .183246E-01, .183120E-01, .481565E+00, .127419E-01, .624390E-01,
     & .624390E-01, .199661E-01, .199801E-01, .199801E-01, .199661E-01/
      DATA (DL(K),K= 1191, 1275) /
     & .499943E+00, .146375E-01, .645736E-01, .645736E-01, .215535E-01,
     & .215688E-01, .215688E-01, .215535E-01, .512983E+00, .164593E-01,
     & .665556E-01, .665556E-01, .230528E-01, .230693E-01, .230693E-01,
     & .230528E-01, .520995E+00, .181859E-01, .683669E-01, .683669E-01,
     & .244463E-01, .244639E-01, .244639E-01, .244463E-01, .524347E+00,
     & .197998E-01, .699932E-01, .699932E-01, .257201E-01, .257387E-01,
     & .257387E-01, .257201E-01, .523447E+00, .212869E-01, .714240E-01,
     & .714240E-01, .268637E-01, .268832E-01, .268832E-01, .268637E-01,
     & .518729E+00, .226367E-01, .726523E-01, .726523E-01, .278697E-01,
     & .278900E-01, .278900E-01, .278697E-01, .510641E+00, .238420E-01,
     & .736741E-01, .736741E-01, .287338E-01, .287547E-01, .287547E-01,
     & .287338E-01, .499630E+00, .248984E-01, .744886E-01, .744886E-01,
     & .294542E-01, .294757E-01, .294757E-01, .294542E-01, .486140E+00,
     & .258043E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 1276, 1360) /
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .499162E-01, .499162E-01, .534441E-02, .534441E-02, .534441E-02,
     & .534441E-02, .191108E+00,-.151788E-17, .519197E-01, .519197E-01,
     & .646285E-02, .646402E-02, .646402E-02, .646285E-02, .232539E+00,
     & .137669E-02, .539504E-01, .539504E-01, .769150E-02, .769377E-02,
     & .769377E-02, .769150E-02, .269907E+00, .285489E-02, .559598E-01,
     & .559598E-01, .898389E-02, .898721E-02, .898721E-02, .898389E-02,
     & .302186E+00, .438814E-02, .579130E-01, .579130E-01, .103061E-01,
     & .103104E-01, .103104E-01, .103061E-01, .329124E+00, .594258E-02,
     & .597754E-01, .597754E-01, .116245E-01, .116297E-01, .116297E-01,
     & .116245E-01, .350643E+00, .748452E-02, .615191E-01, .615191E-01,
     & .129113E-01, .129174E-01, .129174E-01, .129113E-01, .366890E+00,
     & .898645E-02, .631204E-01, .631204E-01, .141428E-01, .141497E-01,
     & .141497E-01, .141428E-01, .378134E+00, .104247E-01, .645601E-01,
     & .645601E-01, .152995E-01, .153071E-01, .153071E-01, .152995E-01,
     & .384719E+00, .117798E-01, .658236E-01, .658236E-01, .163657E-01,
     & .163739E-01, .163739E-01, .163657E-01, .387045E+00, .130362E-01/
      DATA (DL(K),K= 1361, 1445) /
     & .669000E-01, .669000E-01, .173294E-01, .173381E-01, .173381E-01,
     & .173294E-01, .385547E+00, .141821E-01, .677824E-01, .677824E-01,
     & .181820E-01, .181912E-01, .181912E-01, .181820E-01, .380677E+00,
     & .152091E-01, .684672E-01, .684672E-01, .189180E-01, .189277E-01,
     & .189277E-01, .189180E-01, .372894E+00, .161119E-01, .689539E-01,
     & .689539E-01, .195349E-01, .195449E-01, .195449E-01, .195349E-01,
     & .362650E+00, .168880E-01, .692447E-01, .692447E-01, .200324E-01,
     & .200427E-01, .200427E-01, .200324E-01, .350383E+00, .175374E-01,
     & .693442E-01, .693442E-01, .204123E-01, .204229E-01, .204229E-01,
     & .204123E-01, .336505E+00, .180622E-01, .692590E-01, .692590E-01,
     & .206783E-01, .206891E-01, .206891E-01, .206783E-01, .321403E+00,
     & .184661E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .554152E-01, .554152E-01, .386683E-02, .386683E-02, .386683E-02/
      DATA (DL(K),K= 1446, 1530) /
     & .386683E-02, .185844E+00,-.151788E-17, .571372E-01, .571372E-01,
     & .493623E-02, .493704E-02, .493704E-02, .493623E-02, .219342E+00,
     & .129037E-02, .588098E-01, .588098E-01, .606768E-02, .606924E-02,
     & .606924E-02, .606768E-02, .248288E+00, .263296E-02, .603896E-01,
     & .603896E-01, .721747E-02, .721973E-02, .721973E-02, .721747E-02,
     & .271974E+00, .398431E-02, .618484E-01, .618484E-01, .835658E-02,
     & .835949E-02, .835949E-02, .835658E-02, .290397E+00, .531560E-02,
     & .631600E-01, .631600E-01, .945726E-02, .946074E-02, .946074E-02,
     & .945726E-02, .303702E+00, .659930E-02, .643051E-01, .643051E-01,
     & .104983E-01, .105023E-01, .105023E-01, .104983E-01, .312209E+00,
     & .781443E-02, .652691E-01, .652691E-01, .114624E-01, .114669E-01,
     & .114669E-01, .114624E-01, .316328E+00, .894407E-02, .660416E-01,
     & .660416E-01, .123367E-01, .123416E-01, .123416E-01, .123367E-01,
     & .316509E+00, .997546E-02, .666169E-01, .666169E-01, .131119E-01,
     & .131171E-01, .131171E-01, .131119E-01, .313229E+00, .108996E-01,
     & .669925E-01, .669925E-01, .137818E-01, .137874E-01, .137874E-01,
     & .137818E-01, .306974E+00, .117107E-01, .671695E-01, .671695E-01/
      DATA (DL(K),K= 1531, 1615) /
     & .143437E-01, .143495E-01, .143495E-01, .143437E-01, .298224E+00,
     & .124061E-01, .671517E-01, .671517E-01, .147970E-01, .148031E-01,
     & .148031E-01, .147970E-01, .287441E+00, .129858E-01, .669454E-01,
     & .669454E-01, .151437E-01, .151499E-01, .151499E-01, .151437E-01,
     & .275064E+00, .134517E-01, .665590E-01, .665590E-01, .153872E-01,
     & .153935E-01, .153935E-01, .153872E-01, .261497E+00, .138078E-01,
     & .660023E-01, .660023E-01, .155327E-01, .155391E-01, .155391E-01,
     & .155327E-01, .247105E+00, .140595E-01, .652865E-01, .652865E-01,
     & .155864E-01, .155929E-01, .155929E-01, .155864E-01, .232218E+00,
     & .142131E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .601098E-01, .601098E-01, .278641E-02, .278641E-02, .278641E-02,
     & .278641E-02, .183460E+00, .130104E-17, .614949E-01, .614949E-01,
     & .382710E-02, .382771E-02, .382771E-02, .382710E-02, .211150E+00/
      DATA (DL(K),K= 1616, 1700) /
     & .122320E-02, .627722E-01, .627722E-01, .489465E-02, .489580E-02,
     & .489580E-02, .489465E-02, .234040E+00, .246333E-02, .639042E-01,
     & .639042E-01, .594825E-02, .594990E-02, .594990E-02, .594825E-02,
     & .251649E+00, .367998E-02, .648697E-01, .648697E-01, .696315E-02,
     & .696526E-02, .696526E-02, .696315E-02, .264143E+00, .484875E-02,
     & .656502E-01, .656502E-01, .791658E-02, .791907E-02, .791907E-02,
     & .791658E-02, .271822E+00, .594722E-02, .662343E-01, .662343E-01,
     & .879236E-02, .879520E-02, .879520E-02, .879236E-02, .275124E+00,
     & .695957E-02, .666152E-01, .666152E-01, .957846E-02, .958160E-02,
     & .958160E-02, .957846E-02, .274549E+00, .787413E-02, .667905E-01,
     & .667905E-01, .102668E-01, .102702E-01, .102702E-01, .102668E-01,
     & .270615E+00, .868318E-02, .667616E-01, .667616E-01, .108528E-01,
     & .108564E-01, .108564E-01, .108528E-01, .263847E+00, .938250E-02,
     & .665331E-01, .665331E-01, .113349E-01, .113387E-01, .113387E-01,
     & .113349E-01, .254756E+00, .997082E-02, .661123E-01, .661123E-01,
     & .117139E-01, .117179E-01, .117179E-01, .117139E-01, .243828E+00,
     & .104494E-01, .655090E-01, .655090E-01, .119931E-01, .119971E-01/
      DATA (DL(K),K= 1701, 1785) /
     & .119971E-01, .119931E-01, .231518E+00, .108217E-01, .647345E-01,
     & .647345E-01, .121770E-01, .121811E-01, .121811E-01, .121770E-01,
     & .218237E+00, .110927E-01, .638017E-01, .638017E-01, .122717E-01,
     & .122759E-01, .122759E-01, .122717E-01, .204353E+00, .112689E-01,
     & .627241E-01, .627241E-01, .122842E-01, .122884E-01, .122884E-01,
     & .122842E-01, .190187E+00, .113573E-01, .615161E-01, .615161E-01,
     & .122221E-01, .122262E-01, .122262E-01, .122221E-01, .176012E+00,
     & .113659E-01, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .641221E-01, .641221E-01, .198544E-02, .198544E-02, .198544E-02,
     & .198544E-02, .183018E+00, .184314E-17, .651208E-01, .651208E-01,
     & .300778E-02, .300823E-02, .300823E-02, .300778E-02, .206180E+00,
     & .116844E-02, .659664E-01, .659664E-01, .402903E-02, .402989E-02,
     & .402989E-02, .402903E-02, .224390E+00, .232648E-02, .666277E-01/
      DATA (DL(K),K= 1786, 1870) /
     & .666277E-01, .501117E-02, .501238E-02, .501238E-02, .501117E-02,
     & .237332E+00, .343657E-02, .670904E-01, .670904E-01, .593321E-02,
     & .593473E-02, .593473E-02, .593321E-02, .245310E+00, .447818E-02,
     & .673435E-01, .673435E-01, .677663E-02, .677843E-02, .677843E-02,
     & .677663E-02, .248743E+00, .543320E-02, .673832E-01, .673832E-01,
     & .752958E-02, .753161E-02, .753161E-02, .752958E-02, .248162E+00,
     & .629021E-02, .672099E-01, .672099E-01, .818432E-02, .818655E-02,
     & .818655E-02, .818432E-02, .244140E+00, .704188E-02, .668281E-01,
     & .668281E-01, .873688E-02, .873927E-02, .873927E-02, .873688E-02,
     & .237247E+00, .768463E-02, .662457E-01, .662457E-01, .918650E-02,
     & .918903E-02, .918903E-02, .918650E-02, .228043E+00, .821808E-02,
     & .654734E-01, .654734E-01, .953502E-02, .953766E-02, .953766E-02,
     & .953502E-02, .217054E+00, .864446E-02, .645238E-01, .645238E-01,
     & .978645E-02, .978915E-02, .978915E-02, .978645E-02, .204766E+00,
     & .896811E-02, .634114E-01, .634114E-01, .994639E-02, .994915E-02,
     & .994915E-02, .994639E-02, .191614E+00, .919500E-02, .621515E-01,
     & .621515E-01, .100217E-01, .100245E-01, .100245E-01, .100217E-01/
      DATA (DL(K),K= 1871, 1955) /
     & .177983E+00, .933229E-02, .607602E-01, .607602E-01, .100200E-01,
     & .100228E-01, .100228E-01, .100200E-01, .164201E+00, .938793E-02,
     & .592539E-01, .592539E-01, .994938E-02, .995217E-02, .995217E-02,
     & .994938E-02, .150544E+00, .937032E-02, .576488E-01, .576488E-01,
     & .981814E-02, .982091E-02, .982091E-02, .981814E-02, .137234E+00,
     & .928803E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .675167E-01, .675167E-01, .139130E-02, .139130E-02, .139130E-02,
     & .139130E-02, .184090E+00, .113841E-17, .680840E-01, .680840E-01,
     & .240061E-02, .240097E-02, .240097E-02, .240061E-02, .203559E+00,
     & .112278E-02, .684634E-01, .684634E-01, .338513E-02, .338580E-02,
     & .338580E-02, .338513E-02, .217944E+00, .221249E-02, .686304E-01,
     & .686304E-01, .430938E-02, .431032E-02, .431032E-02, .430938E-02,
     & .227068E+00, .323420E-02, .685779E-01, .685779E-01, .515589E-02/
      DATA (DL(K),K= 1956, 2040) /
     & .515707E-02, .515707E-02, .515589E-02, .231353E+00, .417091E-02,
     & .683023E-01, .683023E-01, .591002E-02, .591140E-02, .591140E-02,
     & .591002E-02, .231327E+00, .500843E-02, .678068E-01, .678068E-01,
     & .656383E-02, .656537E-02, .656537E-02, .656383E-02, .227606E+00,
     & .573925E-02, .670989E-01, .670989E-01, .711344E-02, .711513E-02,
     & .711513E-02, .711344E-02, .220833E+00, .635992E-02, .661895E-01,
     & .661895E-01, .755852E-02, .756031E-02, .756031E-02, .755852E-02,
     & .211624E+00, .687048E-02, .650923E-01, .650923E-01, .790162E-02,
     & .790350E-02, .790350E-02, .790162E-02, .200567E+00, .727387E-02,
     & .638232E-01, .638232E-01, .814753E-02, .814946E-02, .814946E-02,
     & .814753E-02, .188197E+00, .757524E-02, .623994E-01, .623994E-01,
     & .830271E-02, .830469E-02, .830469E-02, .830271E-02, .174994E+00,
     & .778139E-02, .608390E-01, .608390E-01, .837482E-02, .837682E-02,
     & .837682E-02, .837482E-02, .161373E+00, .790029E-02, .591605E-01,
     & .591605E-01, .837226E-02, .837426E-02, .837426E-02, .837226E-02,
     & .147685E+00, .794065E-02, .573824E-01, .573824E-01, .830376E-02,
     & .830575E-02, .830575E-02, .830376E-02, .134218E+00, .791148E-02/
      DATA (DL(K),K= 2041, 2125) /
     & .555224E-01, .555224E-01, .817811E-02, .818008E-02, .818008E-02,
     & .817811E-02, .121200E+00, .782185E-02, .535980E-01, .535980E-01,
     & .800390E-02, .800584E-02, .800584E-02, .800390E-02, .108803E+00,
     & .768059E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .703249E-01, .703249E-01, .953926E-03, .953926E-03, .953926E-03,
     & .953926E-03, .186497E+00, .108420E-18, .704188E-01, .704188E-01,
     & .195267E-02, .195297E-02, .195297E-02, .195267E-02, .202831E+00,
     & .108414E-02, .702995E-01, .702995E-01, .290528E-02, .290582E-02,
     & .290582E-02, .290528E-02, .213933E+00, .211514E-02, .699499E-01,
     & .699499E-01, .377873E-02, .377950E-02, .377950E-02, .377873E-02,
     & .219748E+00, .306054E-02, .693699E-01, .693699E-01, .455903E-02,
     & .455997E-02, .455997E-02, .455903E-02, .220821E+00, .390679E-02,
     & .685634E-01, .685634E-01, .523531E-02, .523640E-02, .523640E-02/
      DATA (DL(K),K= 2126, 2210) /
     & .523531E-02, .217787E+00, .464347E-02, .675406E-01, .675406E-01,
     & .580340E-02, .580462E-02, .580462E-02, .580340E-02, .211353E+00,
     & .526681E-02, .663155E-01, .663155E-01, .626315E-02, .626446E-02,
     & .626446E-02, .626315E-02, .202230E+00, .577705E-02, .649052E-01,
     & .649052E-01, .661759E-02, .661897E-02, .661897E-02, .661759E-02,
     & .191081E+00, .617758E-02, .633285E-01, .633285E-01, .687230E-02,
     & .687373E-02, .687373E-02, .687230E-02, .178518E+00, .647434E-02,
     & .616058E-01, .616058E-01, .703464E-02, .703611E-02, .703611E-02,
     & .703464E-02, .165082E+00, .667499E-02, .597580E-01, .597580E-01,
     & .711320E-02, .711468E-02, .711468E-02, .711320E-02, .151241E+00,
     & .678842E-02, .578059E-01, .578059E-01, .711723E-02, .711872E-02,
     & .711872E-02, .711723E-02, .137382E+00, .682417E-02, .557702E-01,
     & .557702E-01, .705628E-02, .705776E-02, .705776E-02, .705628E-02,
     & .123821E+00, .679205E-02, .536704E-01, .536704E-01, .693979E-02,
     & .694125E-02, .694125E-02, .693979E-02, .110798E+00, .670173E-02,
     & .515252E-01, .515252E-01, .677689E-02, .677832E-02, .677832E-02,
     & .677689E-02, .984933E-01, .656256E-02, .493519E-01, .493519E-01/
      DATA (DL(K),K= 2211, 2295) /
     & .657614E-02, .657753E-02, .657753E-02, .657614E-02, .870270E-01,
     & .638332E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .725555E-01, .725555E-01, .636424E-03, .636424E-03, .636424E-03,
     & .636424E-03, .190200E+00,-.271051E-17, .721364E-01, .721364E-01,
     & .162522E-02, .162546E-02, .162546E-02, .162522E-02, .203770E+00,
     & .105090E-02, .714881E-01, .714881E-01, .254763E-02, .254807E-02,
     & .254807E-02, .254763E-02, .211914E+00, .202976E-02, .706012E-01,
     & .706012E-01, .337323E-02, .337384E-02, .337384E-02, .337323E-02,
     & .214704E+00, .290668E-02, .694832E-01, .694832E-01, .409164E-02,
     & .409240E-02, .409240E-02, .409164E-02, .212817E+00, .367169E-02,
     & .681454E-01, .681454E-01, .469593E-02, .469679E-02, .469679E-02,
     & .469593E-02, .207013E+00, .431823E-02, .666048E-01, .666048E-01,
     & .518578E-02, .518673E-02, .518673E-02, .518578E-02, .198095E+00/
      DATA (DL(K),K= 2296, 2380) /
     & .484637E-02, .648819E-01, .648819E-01, .556473E-02, .556575E-02,
     & .556575E-02, .556473E-02, .186850E+00, .525997E-02, .629989E-01,
     & .629989E-01, .583908E-02, .584014E-02, .584014E-02, .583908E-02,
     & .173986E+00, .556566E-02, .609795E-01, .609795E-01, .601725E-02,
     & .601834E-02, .601834E-02, .601725E-02, .160140E+00, .577215E-02,
     & .588474E-01, .588474E-01, .610889E-02, .611000E-02, .611000E-02,
     & .610889E-02, .145850E+00, .588934E-02, .566261E-01, .566261E-01,
     & .612435E-02, .612547E-02, .612547E-02, .612435E-02, .131564E+00,
     & .592785E-02, .543385E-01, .543385E-01, .607415E-02, .607526E-02,
     & .607526E-02, .607415E-02, .117636E+00, .589841E-02, .520060E-01,
     & .520060E-01, .596861E-02, .596970E-02, .596970E-02, .596861E-02,
     & .104336E+00, .581156E-02, .496485E-01, .496485E-01, .581753E-02,
     & .581860E-02, .581860E-02, .581753E-02, .918563E-01, .567728E-02,
     & .472842E-01, .472842E-01, .563002E-02, .563105E-02, .563105E-02,
     & .563002E-02, .803205E-01, .550487E-02, .449295E-01, .449295E-01,
     & .541435E-02, .541535E-02, .541535E-02, .541435E-02, .697975E-01,
     & .530276E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 2381, 2465) /
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .742000E-01, .742000E-01, .410482E-03, .410482E-03, .410482E-03,
     & .410482E-03, .195273E+00,-.143657E-17, .732296E-01, .732296E-01,
     & .138854E-02, .138874E-02, .138874E-02, .138854E-02, .206298E+00,
     & .102151E-02, .720241E-01, .720241E-01, .228017E-02, .228054E-02,
     & .228054E-02, .228017E-02, .211639E+00, .195225E-02, .705820E-01,
     & .705820E-01, .305794E-02, .305844E-02, .305844E-02, .305794E-02,
     & .211509E+00, .276522E-02, .689187E-01, .689187E-01, .371549E-02,
     & .371609E-02, .371609E-02, .371549E-02, .206745E+00, .345441E-02,
     & .670527E-01, .670527E-01, .425016E-02, .425084E-02, .425084E-02,
     & .425016E-02, .198254E+00, .401749E-02, .650080E-01, .650080E-01,
     & .466572E-02, .466647E-02, .466647E-02, .466572E-02, .186950E+00,
     & .445856E-02, .628107E-01, .628107E-01, .496948E-02, .497027E-02,
     & .497027E-02, .496948E-02, .173701E+00, .478517E-02, .604878E-01/
      DATA (DL(K),K= 2466, 2550) /
     & .604878E-01, .517094E-02, .517175E-02, .517175E-02, .517094E-02,
     & .159263E+00, .500709E-02, .580665E-01, .580665E-01, .528116E-02,
     & .528199E-02, .528199E-02, .528116E-02, .144287E+00, .513562E-02,
     & .555730E-01, .555730E-01, .531181E-02, .531265E-02, .531265E-02,
     & .531181E-02, .129304E+00, .518264E-02, .530325E-01, .530325E-01,
     & .527468E-02, .527550E-02, .527550E-02, .527468E-02, .114731E+00,
     & .516012E-02, .504682E-01, .504682E-01, .518116E-02, .518198E-02,
     & .518198E-02, .518116E-02, .100877E+00, .507964E-02, .479014E-01,
     & .479014E-01, .504198E-02, .504278E-02, .504278E-02, .504198E-02,
     & .879578E-01, .495209E-02, .453511E-01, .453511E-01, .486695E-02,
     & .486772E-02, .486772E-02, .486695E-02, .761077E-01, .478741E-02,
     & .428340E-01, .428340E-01, .466486E-02, .466560E-02, .466560E-02,
     & .466486E-02, .653932E-01, .459453E-02, .403645E-01, .403645E-01,
     & .444342E-02, .444413E-02, .444413E-02, .444342E-02, .558281E-01,
     & .438128E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 2551, 2635) /
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .752337E-01, .752337E-01, .253875E-03, .253875E-03, .253875E-03,
     & .253875E-03, .201902E+00,-.159920E-17, .736752E-01, .736752E-01,
     & .121909E-02, .121925E-02, .121925E-02, .121909E-02, .210465E+00,
     & .994282E-03, .718865E-01, .718865E-01, .207747E-02, .207776E-02,
     & .207776E-02, .207747E-02, .212997E+00, .187854E-02, .698746E-01,
     & .698746E-01, .280521E-02, .280560E-02, .280560E-02, .280521E-02,
     & .209895E+00, .262933E-02, .676629E-01, .676629E-01, .340063E-02,
     & .340109E-02, .340109E-02, .340063E-02, .202191E+00, .324527E-02,
     & .652775E-01, .652775E-01, .386588E-02, .386641E-02, .386641E-02,
     & .386588E-02, .190971E+00, .372876E-02, .627483E-01, .627483E-01,
     & .420910E-02, .420967E-02, .420967E-02, .420910E-02, .177278E+00,
     & .408818E-02, .601066E-01, .601066E-01, .444148E-02, .444208E-02,
     & .444208E-02, .444148E-02, .162071E+00, .433493E-02, .573831E-01,
     & .573831E-01, .457564E-02, .457625E-02, .457625E-02, .457564E-02,
     & .146148E+00, .448183E-02, .546072E-01, .546072E-01, .462506E-02/
      DATA (DL(K),K= 2636, 2720) /
     & .462567E-02, .462567E-02, .462506E-02, .130166E+00, .454252E-02,
     & .518065E-01, .518065E-01, .460307E-02, .460368E-02, .460368E-02,
     & .460307E-02, .114632E+00, .453052E-02, .490062E-01, .490062E-01,
     & .452252E-02, .452312E-02, .452312E-02, .452252E-02, .999175E-01,
     & .445880E-02, .462287E-01, .462287E-01, .439529E-02, .439588E-02,
     & .439588E-02, .439529E-02, .862750E-01, .433936E-02, .434937E-01,
     & .434937E-01, .423211E-02, .423268E-02, .423268E-02, .423211E-02,
     & .738542E-01, .418306E-02, .408180E-01, .408180E-01, .404245E-02,
     & .404299E-02, .404299E-02, .404245E-02, .627228E-01, .399946E-02,
     & .382157E-01, .382157E-01, .383446E-02, .383498E-02, .383498E-02,
     & .383446E-02, .528847E-01, .379682E-02, .356980E-01, .356980E-01,
     & .361508E-02, .361557E-02, .361557E-02, .361508E-02, .442963E-01,
     & .358213E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 2721, 2805) /
     & .756149E-01, .756149E-01, .148956E-03, .148956E-03, .148956E-03,
     & .148956E-03, .210410E+00,-.149078E-18, .734321E-01, .734321E-01,
     & .109779E-02, .109791E-02, .109791E-02, .109779E-02, .216444E+00,
     & .967243E-03, .710369E-01, .710369E-01, .191860E-02, .191882E-02,
     & .191882E-02, .191860E-02, .215992E+00, .180426E-02, .684452E-01,
     & .684452E-01, .259230E-02, .259259E-02, .259259E-02, .259230E-02,
     & .209697E+00, .249224E-02, .656884E-01, .656884E-01, .312270E-02,
     & .312305E-02, .312305E-02, .312270E-02, .198844E+00, .303522E-02,
     & .627994E-01, .627994E-01, .351747E-02, .351786E-02, .351786E-02,
     & .351747E-02, .184740E+00, .344105E-02, .598138E-01, .598138E-01,
     & .378940E-02, .378981E-02, .378981E-02, .378940E-02, .168578E+00,
     & .372269E-02, .567666E-01, .567666E-01, .395362E-02, .395405E-02,
     & .395405E-02, .395362E-02, .151409E+00, .389544E-02, .536907E-01,
     & .536907E-01, .402569E-02, .402613E-02, .402613E-02, .402569E-02,
     & .134065E+00, .397499E-02, .506163E-01, .506163E-01, .402117E-02,
     & .402161E-02, .402161E-02, .402117E-02, .117191E+00, .397702E-02,
     & .475706E-01, .475706E-01, .395467E-02, .395511E-02, .395511E-02/
      DATA (DL(K),K= 2806, 2890) /
     & .395467E-02, .101250E+00, .391626E-02, .445771E-01, .445771E-01,
     & .383961E-02, .384003E-02, .384003E-02, .383961E-02, .865500E-01,
     & .380621E-02, .416559E-01, .416559E-01, .368789E-02, .368830E-02,
     & .368830E-02, .368789E-02, .732657E-01, .365888E-02, .388235E-01,
     & .388235E-01, .350985E-02, .351025E-02, .351025E-02, .350985E-02,
     & .614686E-01, .348466E-02, .360931E-01, .360931E-01, .331425E-02,
     & .331462E-02, .331462E-02, .331425E-02, .511511E-01, .329240E-02,
     & .334751E-01, .334751E-01, .310835E-02, .310870E-02, .310870E-02,
     & .310835E-02, .422485E-01, .308941E-02, .309768E-01, .309768E-01,
     & .289805E-02, .289838E-02, .289838E-02, .289805E-02, .346590E-01,
     & .288164E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .752796E-01, .752796E-01, .816906E-04, .816906E-04, .816906E-04,
     & .816906E-04, .221322E+00, .298156E-18, .724376E-01, .724376E-01/
      DATA (DL(K),K= 2891, 2975) /
     & .100884E-02, .100894E-02, .100894E-02, .100884E-02, .224569E+00,
     & .938046E-03, .694166E-01, .694166E-01, .178592E-02, .178609E-02,
     & .178609E-02, .178592E-02, .220745E+00, .172460E-02, .662414E-01,
     & .662414E-01, .240001E-02, .240024E-02, .240024E-02, .240001E-02,
     & .210839E+00, .234694E-02, .629511E-01, .629511E-01, .286144E-02,
     & .286171E-02, .286171E-02, .286144E-02, .196463E+00, .281556E-02,
     & .595849E-01, .595849E-01, .318412E-02, .318441E-02, .318441E-02,
     & .318412E-02, .179204E+00, .314448E-02, .561822E-01, .561822E-01,
     & .338573E-02, .338605E-02, .338605E-02, .338573E-02, .160420E+00,
     & .335151E-02, .527801E-01, .527801E-01, .348530E-02, .348562E-02,
     & .348562E-02, .348530E-02, .141254E+00, .345578E-02, .494117E-01,
     & .494117E-01, .350098E-02, .350131E-02, .350131E-02, .350098E-02,
     & .122547E+00, .347555E-02, .461061E-01, .461061E-01, .344994E-02,
     & .345026E-02, .345026E-02, .344994E-02, .104908E+00, .342804E-02,
     & .428876E-01, .428876E-01, .334753E-02, .334784E-02, .334784E-02,
     & .334753E-02, .887264E-01, .332868E-02, .397764E-01, .397764E-01,
     & .320718E-02, .320748E-02, .320748E-02, .320718E-02, .742160E-01/
      DATA (DL(K),K= 2976, 3060) /
     & .319097E-02, .367882E-01, .367882E-01, .304033E-02, .304062E-02,
     & .304062E-02, .304033E-02, .614556E-01, .302641E-02, .339348E-01,
     & .339348E-01, .285650E-02, .285677E-02, .285677E-02, .285650E-02,
     & .504214E-01, .284454E-02, .312247E-01, .312247E-01, .266337E-02,
     & .266363E-02, .266363E-02, .266337E-02, .410205E-01, .265311E-02,
     & .286629E-01, .286629E-01, .246705E-02, .246730E-02, .246730E-02,
     & .246705E-02, .331166E-01, .245826E-02, .262521E-01, .262521E-01,
     & .227226E-02, .227248E-02, .227248E-02, .227226E-02, .265498E-01,
     & .226473E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .741335E-01, .741335E-01, .409835E-04, .409835E-04, .409835E-04,
     & .409835E-04, .235495E+00,-.158395E-17, .705990E-01, .705990E-01,
     & .938808E-03, .938883E-03, .938883E-03, .938808E-03, .235413E+00,
     & .903728E-03, .669383E-01, .669383E-01, .166382E-02, .166395E-02/
      DATA (DL(K),K= 3061, 3145) /
     & .166395E-02, .166382E-02, .227523E+00, .163381E-02, .631853E-01,
     & .631853E-01, .221128E-02, .221145E-02, .221145E-02, .221128E-02,
     & .213316E+00, .218562E-02, .593855E-01, .593855E-01, .259911E-02,
     & .259931E-02, .259931E-02, .259911E-02, .194833E+00, .257720E-02,
     & .555825E-01, .555825E-01, .284819E-02, .284841E-02, .284841E-02,
     & .284819E-02, .174012E+00, .282950E-02, .518174E-01, .518174E-01,
     & .298117E-02, .298140E-02, .298140E-02, .298117E-02, .152385E+00,
     & .296523E-02, .481268E-01, .481268E-01, .302067E-02, .302090E-02,
     & .302090E-02, .302067E-02, .131168E+00, .300708E-02, .445413E-01,
     & .445413E-01, .298689E-02, .298712E-02, .298712E-02, .298689E-02,
     & .111177E+00, .297532E-02, .410859E-01, .410859E-01, .289793E-02,
     & .289815E-02, .289815E-02, .289793E-02, .929352E-01, .288809E-02,
     & .377798E-01, .377798E-01, .276920E-02, .276941E-02, .276941E-02,
     & .276920E-02, .767178E-01, .276084E-02, .346372E-01, .346372E-01,
     & .261353E-02, .261373E-02, .261373E-02, .261353E-02, .626102E-01,
     & .260643E-02, .316676E-01, .316676E-01, .244134E-02, .244154E-02,
     & .244154E-02, .244134E-02, .505665E-01, .243531E-02, .288765E-01/
      DATA (DL(K),K= 3146, 3230) /
     & .288765E-01, .226087E-02, .226105E-02, .226105E-02, .226087E-02,
     & .404527E-01, .225576E-02, .262660E-01, .262660E-01, .207845E-02,
     & .207862E-02, .207862E-02, .207845E-02, .320820E-01, .207412E-02,
     & .238351E-01, .238351E-01, .189881E-02, .189897E-02, .189897E-02,
     & .189881E-02, .252422E-01, .189514E-02, .215808E-01, .215808E-01,
     & .172536E-02, .172551E-02, .172551E-02, .172536E-02, .197186E-01,
     & .172225E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .720339E-01, .720339E-01, .181946E-04, .181946E-04, .181946E-04,
     & .181946E-04, .254393E+00, .469256E-18, .677768E-01, .677768E-01,
     & .875835E-03, .875888E-03, .875888E-03, .875835E-03, .249966E+00,
     & .860480E-03, .634725E-01, .634725E-01, .153792E-02, .153801E-02,
     & .153801E-02, .153792E-02, .236824E+00, .152496E-02, .591619E-01,
     & .591619E-01, .201036E-02, .201048E-02, .201048E-02, .201036E-02/
      DATA (DL(K),K= 3231, 3315) /
     & .217211E+00, .199944E-02, .548948E-01, .548948E-01, .231978E-02,
     & .231992E-02, .231992E-02, .231978E-02, .193739E+00, .231058E-02,
     & .507162E-01, .507162E-01, .249460E-02, .249475E-02, .249475E-02,
     & .249460E-02, .168773E+00, .248686E-02, .466653E-01, .466653E-01,
     & .256217E-02, .256232E-02, .256232E-02, .256217E-02, .144012E+00,
     & .255566E-02, .427744E-01, .427744E-01, .254804E-02, .254820E-02,
     & .254820E-02, .254804E-02, .120695E+00, .254258E-02, .390676E-01,
     & .390676E-01, .247365E-02, .247380E-02, .247380E-02, .247365E-02,
     & .995452E-01, .246906E-02, .355626E-01, .355626E-01, .235710E-02,
     & .235725E-02, .235725E-02, .235710E-02, .809281E-01, .235325E-02,
     & .322703E-01, .322703E-01, .221303E-02, .221317E-02, .221317E-02,
     & .221303E-02, .649429E-01, .220980E-02, .291963E-01, .291963E-01,
     & .205294E-02, .205307E-02, .205307E-02, .205294E-02, .515039E-01,
     & .205024E-02, .263419E-01, .263419E-01, .188569E-02, .188581E-02,
     & .188581E-02, .188569E-02, .404102E-01, .188343E-02, .237044E-01,
     & .237044E-01, .171783E-02, .171795E-02, .171795E-02, .171783E-02,
     & .313959E-01, .171594E-02, .212782E-01, .212782E-01, .155409E-02/
      DATA (DL(K),K= 3316, 3400) /
     & .155419E-02, .155419E-02, .155409E-02, .241750E-01, .155251E-02,
     & .190555E-01, .190555E-01, .139767E-02, .139777E-02, .139777E-02,
     & .139767E-02, .184646E-01, .139635E-02, .170270E-01, .170270E-01,
     & .125065E-02, .125074E-02, .125074E-02, .125065E-02, .139996E-01,
     & .124955E-02, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .687547E-01, .687547E-01, .676181E-05, .676181E-05, .676181E-05,
     & .676181E-05, .280728E+00,-.145838E-17, .637529E-01, .637529E-01,
     & .808781E-03, .808818E-03, .808818E-03, .808781E-03, .270033E+00,
     & .803169E-03, .588180E-01, .588180E-01, .139388E-02, .139394E-02,
     & .139394E-02, .139388E-02, .249568E+00, .138922E-02, .539945E-01,
     & .539945E-01, .178168E-02, .178176E-02, .178176E-02, .178168E-02,
     & .222743E+00, .177782E-02, .493317E-01, .493317E-01, .200838E-02,
     & .200848E-02, .200848E-02, .200838E-02, .192919E+00, .200519E-02/
      DATA (DL(K),K= 3401, 3485) /
     & .448709E-01, .448709E-01, .211009E-02, .211019E-02, .211019E-02,
     & .211009E-02, .162975E+00, .210745E-02, .406433E-01, .406433E-01,
     & .211805E-02, .211815E-02, .211815E-02, .211805E-02, .134716E+00,
     & .211586E-02, .366716E-01, .366716E-01, .205957E-02, .205968E-02,
     & .205968E-02, .205957E-02, .109289E+00, .205776E-02, .329687E-01,
     & .329687E-01, .195606E-02, .195616E-02, .195616E-02, .195606E-02,
     & .871955E-01, .195457E-02, .295400E-01, .295400E-01, .182447E-02,
     & .182456E-02, .182456E-02, .182447E-02, .685399E-01, .182323E-02,
     & .263849E-01, .263849E-01, .167765E-02, .167774E-02, .167774E-02,
     & .167765E-02, .531615E-01, .167663E-02, .234975E-01, .234975E-01,
     & .152505E-02, .152514E-02, .152514E-02, .152505E-02, .407334E-01,
     & .152421E-02, .208683E-01, .208683E-01, .137342E-02, .137350E-02,
     & .137350E-02, .137342E-02, .308674E-01, .137273E-02, .184852E-01,
     & .184852E-01, .122732E-02, .122739E-02, .122739E-02, .122732E-02,
     & .231578E-01, .122675E-02, .163340E-01, .163340E-01, .108968E-02,
     & .108975E-02, .108975E-02, .108968E-02, .172149E-01, .108921E-02,
     & .143996E-01, .143996E-01, .962198E-03, .962260E-03, .962260E-03/
      DATA (DL(K),K= 3486, 3570) /
     & .962198E-03, .126908E-01, .961815E-03, .126661E-01, .126661E-01,
     & .845675E-03, .845732E-03, .845732E-03, .845675E-03, .928555E-02,
     & .845361E-03, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .639050E-01, .639050E-01, .189600E-05, .189600E-05, .189600E-05,
     & .189600E-05, .320203E+00, .545701E-18, .581555E-01, .581555E-01,
     & .725861E-03, .725886E-03, .725886E-03, .725861E-03, .299305E+00,
     & .724320E-03, .526376E-01, .526376E-01, .121568E-02, .121572E-02,
     & .121572E-02, .121568E-02, .267591E+00, .121442E-02, .473911E-01,
     & .473911E-01, .150820E-02, .150825E-02, .150825E-02, .150820E-02,
     & .230364E+00, .150718E-02, .424558E-01, .424558E-01, .164949E-02,
     & .164955E-02, .164955E-02, .164949E-02, .191972E+00, .164867E-02,
     & .378600E-01, .378600E-01, .168256E-02, .168262E-02, .168262E-02,
     & .168256E-02, .155818E+00, .168189E-02, .336181E-01, .336181E-01/
      DATA (DL(K),K= 3571, 3655) /
     & .164080E-02, .164086E-02, .164086E-02, .164080E-02, .123609E+00,
     & .164026E-02, .297349E-01, .297349E-01, .155136E-02, .155142E-02,
     & .155142E-02, .155136E-02, .961680E-01, .155092E-02, .262052E-01,
     & .262052E-01, .143382E-02, .143388E-02, .143388E-02, .143382E-02,
     & .735522E-01, .143346E-02, .230171E-01, .230171E-01, .130247E-02,
     & .130253E-02, .130253E-02, .130247E-02, .553916E-01, .130218E-02,
     & .201539E-01, .201539E-01, .116733E-02, .116739E-02, .116739E-02,
     & .116733E-02, .411453E-01, .116710E-02, .175955E-01, .175955E-01,
     & .103505E-02, .103510E-02, .103510E-02, .103505E-02, .301858E-01,
     & .103486E-02, .153199E-01, .153199E-01, .909828E-03, .909880E-03,
     & .909880E-03, .909828E-03, .218957E-01, .909677E-03, .133043E-01,
     & .133043E-01, .794097E-03, .794146E-03, .794146E-03, .794097E-03,
     & .157204E-01, .793976E-03, .115259E-01, .115259E-01, .689012E-03,
     & .689057E-03, .689057E-03, .689012E-03, .111816E-01, .688913E-03,
     & .996208E-02, .996208E-02, .594880E-03, .594922E-03, .594922E-03,
     & .594880E-03, .788559E-02, .594801E-03, .859151E-02, .859151E-02,
     & .511455E-03, .511493E-03, .511493E-03, .511455E-03, .551865E-02/
      DATA (DL(K),K= 3656, 3740) /
     & .511392E-03, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .567030E-01, .567030E-01, .317692E-06, .317692E-06, .317692E-06,
     & .317692E-06, .387655E+00,-.196551E-18, .502560E-01, .502560E-01,
     & .611827E-03, .611838E-03, .611838E-03, .611827E-03, .346975E+00,
     & .611576E-03, .442838E-01, .442838E-01, .981907E-03, .981929E-03,
     & .981929E-03, .981907E-03, .295290E+00, .981708E-03, .388018E-01,
     & .388018E-01, .116826E-02, .116829E-02, .116829E-02, .116826E-02,
     & .241157E+00, .116810E-02, .338227E-01, .338227E-01, .122537E-02,
     & .122541E-02, .122541E-02, .122537E-02, .190062E+00, .122525E-02,
     & .293442E-01, .293442E-01, .120047E-02, .120052E-02, .120052E-02,
     & .120047E-02, .145706E+00, .120037E-02, .253494E-01, .253494E-01,
     & .112580E-02, .112585E-02, .112585E-02, .112580E-02, .109110E+00,
     & .112572E-02, .218132E-01, .218132E-01, .102498E-02, .102503E-02/
      DATA (DL(K),K= 3741, 3825) /
     & .102503E-02, .102498E-02, .800661E-01, .102492E-02, .187030E-01,
     & .187030E-01, .913395E-03, .913450E-03, .913450E-03, .913395E-03,
     & .577342E-01, .913348E-03, .159833E-01, .159833E-01, .800935E-03,
     & .800990E-03, .800990E-03, .800935E-03, .409782E-01, .800898E-03,
     & .136172E-01, .136172E-01, .693698E-03, .693751E-03, .693751E-03,
     & .693698E-03, .286780E-01, .693669E-03, .115681E-01, .115681E-01,
     & .595013E-03, .595064E-03, .595064E-03, .595013E-03, .198197E-01,
     & .594990E-03, .980105E-02, .980105E-02, .506423E-03, .506471E-03,
     & .506471E-03, .506423E-03, .135410E-01, .506405E-03, .828286E-02,
     & .828286E-02, .428323E-03, .428368E-03, .428368E-03, .428323E-03,
     & .915498E-02, .428309E-03, .698303E-02, .698303E-02, .360397E-03,
     & .360439E-03, .360439E-03, .360397E-03, .613133E-02, .360386E-03,
     & .587373E-02, .587373E-02, .301934E-03, .301973E-03, .301973E-03,
     & .301934E-03, .407092E-02, .301925E-03, .492985E-02, .492985E-02,
     & .252029E-03, .252064E-03, .252064E-03, .252029E-03, .268179E-02,
     & .252022E-03, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 3826, 3910) /
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .450567E-01, .450567E-01, .151200E-07, .151200E-07, .151200E-07,
     & .151200E-07, .542258E+00,-.767310E-18, .381624E-01, .381624E-01,
     & .438685E-03, .438751E-03, .438751E-03, .438685E-03, .447480E+00,
     & .438674E-03, .321250E-01, .321250E-01, .653465E-03, .653574E-03,
     & .653574E-03, .653465E-03, .347952E+00, .653456E-03, .268827E-01,
     & .268827E-01, .724735E-03, .724868E-03, .724868E-03, .724735E-03,
     & .258636E+00, .724728E-03, .223751E-01, .223751E-01, .709421E-03,
     & .709567E-03, .709567E-03, .709421E-03, .185088E+00, .709417E-03,
     & .185359E-01, .185359E-01, .650478E-03, .650626E-03, .650626E-03,
     & .650478E-03, .128686E+00, .650474E-03, .152906E-01, .152906E-01,
     & .572423E-03, .572567E-03, .572567E-03, .572423E-03, .873875E-01,
     & .572420E-03, .125654E-01, .125654E-01, .490165E-03, .490302E-03,
     & .490302E-03, .490165E-03, .581141E-01, .490163E-03, .102901E-01,
     & .102901E-01, .411740E-03, .411866E-03, .411866E-03, .411740E-03/
      DATA (DL(K),K= 3911, 3995) /
     & .379596E-01, .411738E-03, .839975E-02, .839975E-02, .340986E-03,
     & .341101E-03, .341101E-03, .340986E-03, .244073E-01, .340985E-03,
     & .683634E-02, .683634E-02, .279417E-03, .279520E-03, .279520E-03,
     & .279417E-03, .154717E-01, .279416E-03, .554846E-02, .554846E-02,
     & .227114E-03, .227204E-03, .227204E-03, .227114E-03, .968450E-02,
     & .227113E-03, .449143E-02, .449143E-02, .183425E-03, .183504E-03,
     & .183504E-03, .183425E-03, .599306E-02, .183425E-03, .362676E-02,
     & .362676E-02, .147387E-03, .147455E-03, .147455E-03, .147387E-03,
     & .366977E-02, .147387E-03, .292164E-02, .292164E-02, .117936E-03,
     & .117995E-03, .117995E-03, .117936E-03, .222583E-02, .117936E-03,
     & .234830E-02, .234830E-02, .940414E-04, .940914E-04, .940914E-04,
     & .940414E-04, .133844E-02, .940412E-04, .188339E-02, .188339E-02,
     & .747651E-04, .748074E-04, .748074E-04, .747651E-04, .798451E-03,
     & .747649E-04, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00,
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
      DATA (DL(K),K= 3996, 4000) /
     & .000000E+00, .000000E+00, .000000E+00, .000000E+00, .000000E+00/
C
      DO 10 I=1,7
        QQ(I) = 0.
 10   CONTINUE
      IF(X.GT.0.9985) RETURN
C
      IS=S/DELTA+1
      IS1=IS+1
      DO 20 I=1,7
        IF(I.EQ.3.AND.X.GT.0.95) GOTO 19
        IF(I.EQ.8.AND.X.GT.0.95) GOTO 19
        DO 30 L=1,25
          F1(L)=GF(I,IS,L)
          F2(L)=GF(I,IS1,L)
 30     CONTINUE
        S1=(IS-1)*DELTA
        S2=S1+DELTA
        A1 = CKMTFV(X,F1)
        A2 = CKMTFV(X,F2)
        QQ(I)=A1*(S2-S)/DELTA+A2*(S-S1)/DELTA
 19     CONTINUE
 20   CONTINUE
      END
C
C
      FUNCTION CKMTFV(X,FVL)
C**********************************************************************
C
C     LOGARITHMIC INTERPOLATOR - WATCH OUT FOR NEGATIVE
C     FUNCTIONS AND/OR X VALUES OUTSIDE THE RANGE 0 TO 1.
C     NOTE: DIMENSION OF FVL IS OVERWRITTEN BY VALUE USED
C     IN MAIN ROUTINE.
C
C**********************************************************************
      DIMENSION FVL(25),XGRID(25)
      DATA NX,XGRID/25,.001,.002,.004,.008,.016,.032,.064,.1,.15,
     *.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,.95/
C
      CKMTFV=0.
      DO 1 I=1,NX
      IF(X.LT.XGRID(I)) GO TO 2
    1 CONTINUE
    2 I=I-1
      IF(I.EQ.0) THEN
         I=I+1
      ELSE IF(I.GT.23) THEN
         I=23
      ENDIF
      J=I+1
      K=J+1
      AXI=LOG(XGRID(I))
      BXI=LOG(1.-XGRID(I))
      AXJ=LOG(XGRID(J))
      BXJ=LOG(1.-XGRID(J))
      AXK=LOG(XGRID(K))
      BXK=LOG(1.-XGRID(K))
      FI=LOG(ABS(FVL(I)) +1.E-15)
      FJ=LOG(ABS(FVL(J)) +1.E-16)
      FK=LOG(ABS(FVL(K)) +1.E-17)
      DET=AXI*(BXJ-BXK)+AXJ*(BXK-BXI)+AXK*(BXI-BXJ)
      ALOGA=(FI*(AXJ*BXK-AXK*BXJ)+FJ*(AXK*BXI-AXI*BXK)+FK*(AXI*BXJ-AXJ*
     $ BXI))/DET
      ALPHA=(FI*(BXJ-BXK)+FJ*(BXK-BXI)+FK*(BXI-BXJ))/DET
      BETA=(FI*(AXK-AXJ)+FJ*(AXI-AXK)+FK*(AXJ-AXI))/DET
      IF(ABS(ALPHA).GT.99..OR.ABS(BETA).GT.99..OR.ABS(ALOGA).GT.99.)
     1RETURN
C      IF(ALPHA.GT.50..OR.BETA.GT.50.) THEN
C         WRITE(6,2001) X,FVL
C 2001    FORMAT(8E12.4)
C         WRITE(6,2001) ALPHA,BETA,ALOGA,DET
C      ENDIF
      CKMTFV=EXP(ALOGA)*X**ALPHA*(1.-X)**BETA
      RETURN
      END
C
C
      SUBROUTINE SASGAM(ISET,X,Q2,P2,F2GM,XPDFGM)
C**********************************************************************
C
C...Purpose: to construct the F2 and parton distributions of the photon
C...by summing homogeneous (VMD) and inhomogeneous (anomalous) terms.
C...For F2, c and b are included by the Bethe-Heitler formula;
C...in the 'MSbar' scheme additionally a Cgamma term is added.
C
C**********************************************************************
      DIMENSION XPDFGM(-6:6)
      COMMON/SASCOM/XPVMD(-6:6),XPANL(-6:6),XPANH(-6:6),XPBEH(-6:6),
     &XPDIR(-6:6)
      SAVE /SASCOM/

C...Temporary array.
      DIMENSION XPGA(-6:6)
C...Charm and bottom masses (low to compensate for J/psi etc.).
      DATA PMC/1.3/, PMB/4.6/
C...alpha_em and alpha_em/(2*pi).
      DATA AEM/0.007297/, AEM2PI/0.0011614/
C...Lambda value for 4 flavours.
      DATA ALAM/0.20/
C...Mixture u/(u+d), = 0.5 for incoherent and = 0.8 for coherent sum.
      DATA FRACU/0.8/
C...VMD couplings f_V**2/(4*pi).
      DATA FRHO/2.20/, FOMEGA/23.6/, FPHI/18.4/
C...Masses for rho (=omega) and phi.
      DATA PMRHO/0.770/, PMPHI/1.020/

C...Reset output.
      F2GM=0.
      DO 100 KFL=-6,6
      XPDFGM(KFL)=0.
      XPVMD(KFL)=0.
      XPANL(KFL)=0.
      XPANH(KFL)=0.
      XPBEH(KFL)=0.
      XPDIR(KFL)=0.
  100 CONTINUE

C...Check that input sensible.
      IF(ISET.LE.0.OR.ISET.GE.5) THEN
        WRITE(*,*) ' FATAL ERROR: SaSgam called for unknown set'
        WRITE(*,*) ' ISET = ',ISET
        STOP
      ENDIF
      IF(X.LE.0..OR.X.GT.1.) THEN
        WRITE(*,*) ' FATAL ERROR: SaSgam called for unphysical x'
        WRITE(*,*) ' X = ',X
        STOP
      ENDIF

C...Set k0 cut-off parameter as function of set used.
      IF(ISET.LE.2) THEN
        AK0=0.6
      ELSE
        AK0=2.
      ENDIF

C...Call VMD parametrization for d quark and use to give rho, omega, phi.
C...Note scale choice and dipole dampening for off-shell photon.
      P2MX=MAX(P2,AK0**2)
      CALL SASVMD(ISET,1,X,Q2,P2MX,ALAM,XPGA)
      XFVAL=XPGA(1)-XPGA(2)
      XPGA(1)=XPGA(2)
      XPGA(-1)=XPGA(-2)
      FACUD=AEM*(1./FRHO+1./FOMEGA)*(PMRHO**2/(PMRHO**2+P2))**2
      FACS=AEM*(1./FPHI)*(PMPHI**2/(PMPHI**2+P2))**2
      DO 110 KFL=-5,5
      XPVMD(KFL)=(FACUD+FACS)*XPGA(KFL)
  110 CONTINUE
      XPVMD(1)=XPVMD(1)+(1.-FRACU)*FACUD*XFVAL
      XPVMD(2)=XPVMD(2)+FRACU*FACUD*XFVAL
      XPVMD(3)=XPVMD(3)+FACS*XFVAL
      XPVMD(-1)=XPVMD(-1)+(1.-FRACU)*FACUD*XFVAL
      XPVMD(-2)=XPVMD(-2)+FRACU*FACUD*XFVAL
      XPVMD(-3)=XPVMD(-3)+FACS*XFVAL

C...Call anomalous parametrization for d + u + s.
      CALL SASANO(-3,X,Q2,P2MX,ALAM,XPGA)
      DO 120 KFL=-5,5
      XPANL(KFL)=XPGA(KFL)
  120 CONTINUE

C...Call anomalous parametrization for c and b.
      CALL SASANO(4,X,Q2,P2MX,ALAM,XPGA)
      DO 130 KFL=-5,5
      XPANH(KFL)=XPGA(KFL)
  130 CONTINUE
      CALL SASANO(5,X,Q2,P2MX,ALAM,XPGA)
      DO 140 KFL=-5,5
      XPANH(KFL)=XPANH(KFL)+XPGA(KFL)
  140 CONTINUE

C...Call Bethe-Heitler term expression for charm and bottom.
      CALL SASBEH(4,X,Q2,P2,PMC**2,XPBH)
      XPBEH(4)=XPBH
      XPBEH(-4)=XPBH
      CALL SASBEH(5,X,Q2,P2,PMB**2,XPBH)
      XPBEH(5)=XPBH
      XPBEH(-5)=XPBH

C...For MSbar subtraction call C^gamma term expression for d, u, s.
      IF(ISET.EQ.2.OR.ISET.EQ.4) THEN
        CALL SASDIR(X,Q2,P2,AK0,XPGA)
        DO 150 KFL=-5,5
        XPDIR(KFL)=XPGA(KFL)
  150   CONTINUE
      ENDIF

C...Store result in output array.
      DO 160 KFL=-5,5
      CHSQ=1./9.
      IF(IABS(KFL).EQ.2.OR.IABS(KFL).EQ.4) CHSQ=4./9.
      XPF2=XPVMD(KFL)+XPANL(KFL)+XPBEH(KFL)+XPDIR(KFL)
      IF(KFL.NE.0) F2GM=F2GM+CHSQ*XPF2
      XPDFGM(KFL)=XPVMD(KFL)+XPANL(KFL)+XPANH(KFL)
  160 CONTINUE

      RETURN
      END

C*********************************************************************

      SUBROUTINE SASVMD(ISET,KF,X,Q2,P2,ALAM,XPGA)
C...Purpose: to evaluate the VMD parton distributions of a photon,
C...evolved homogeneously from an initial scale P2 to Q2.
C...Does not include dipole suppression factor.
C...ISET is parton distribution set, see above;
C...additionally ISET=0 is used for the evolution of an anomalous photon
C...which branched at a scale P2 and then evolved homogeneously to Q2.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      DIMENSION XPGA(-6:6)
      DATA PMC/1.3/, PMB/4.6/, AEM/0.007297/, AEM2PI/0.0011614/

C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
      KFA=IABS(KF)

C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      ALAM3=ALAM*(PMC/ALAM)**(2./27.)
      ALAM5=ALAM*(ALAM/PMB)**(2./23.)
      P2EFF=MAX(P2,1.2*ALAM3**2)
      IF(KFA.EQ.4) P2EFF=MAX(P2EFF,PMC**2)
      IF(KFA.EQ.5) P2EFF=MAX(P2EFF,PMB**2)
      Q2EFF=MAX(Q2,P2EFF)

C...Find number of flavours at lower and upper scale.
      NFP=4
      IF(P2EFF.LT.PMC**2) NFP=3
      IF(P2EFF.GT.PMB**2) NFP=5
      NFQ=4
      IF(Q2EFF.LT.PMC**2) NFQ=3
      IF(Q2EFF.GT.PMB**2) NFQ=5

C...Find s as sum of 3-, 4- and 5-flavour parts.
      S=0.
      IF(NFP.EQ.3) THEN
        Q2DIV=PMC**2
        IF(NFQ.EQ.3) Q2DIV=Q2EFF
        S=S+(6./27.)*LOG(LOG(Q2DIV/ALAM3**2)/LOG(P2EFF/ALAM3**2))
      ENDIF
      IF(NFP.LE.4.AND.NFQ.GE.4) THEN
        P2DIV=P2EFF
        IF(NFP.EQ.3) P2DIV=PMC**2
        Q2DIV=Q2EFF
        IF(NFQ.EQ.5) Q2DIV=PMB**2
        S=S+(6./25.)*LOG(LOG(Q2DIV/ALAM**2)/LOG(P2DIV/ALAM**2))
      ENDIF
      IF(NFQ.EQ.5) THEN
        P2DIV=PMB**2
        IF(NFP.EQ.5) P2DIV=P2EFF
        S=S+(6./23.)*LOG(LOG(Q2EFF/ALAM5**2)/LOG(P2DIV/ALAM5**2))
      ENDIF

C...Calculate frequent combinations of x and s.
      X1=1.-X
      XL=-LOG(X)
      S2=S**2
      S3=S**3
      S4=S**4

C...Evaluate homogeneous anomalous parton distributions below or
C...above threshold.
      IF(ISET.EQ.0) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = X * 1.5 * (X**2+X1**2)
        XGLU = 0.
        XSEA = 0.
      ELSE
        XVAL = (1.5/(1.-0.197*S+4.33*S2)*X**2 + (1.5+2.10*S)/
     &  (1.+3.29*S)*X1**2 + 5.23*S/(1.+1.17*S+19.9*S3)*X*X1) *
     &  X**(1./(1.+1.5*S)) * (1.-X**2)**(2.667*S)
        XGLU = 4.*S/(1.+4.76*S+15.2*S2+29.3*S4) *
     &  X**(-2.03*S/(1.+2.44*S)) * (X1*XL)**(1.333*S) *
     &  ((4.*X**2+7.*X+4.)*X1/3. - 2.*X*(1.+X)*XL)
        XSEA = S2/(1.+4.54*S+8.19*S2+8.05*S3) *
     &  X**(-1.54*S/(1.+1.29*S)) * X1**(2.667*S) *
     &  ((8.-73.*X+62.*X**2)*X1/9. + (3.-8.*X**2/3.)*X*XL +
     &  (2.*X-1.)*X*XL**2)
      ENDIF

C...Evaluate set 1D parton distributions below or above threshold.
      ELSEIF(ISET.EQ.1) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = 1.294 * X**0.80 * X1**0.76
        XGLU = 1.273 * X**0.40 * X1**1.76
        XSEA = 0.100 * X1**3.76
      ELSE
        XVAL = 1.294/(1.+0.252*S+3.079*S2) * X**(0.80-0.13*S) *
     &  X1**(0.76+0.667*S) * XL**(2.*S)
        XGLU = 7.90*S/(1.+5.50*S) * EXP(-5.16*S) *
     &  X**(-1.90*S/(1.+3.60*S)) * X1**1.30 * XL**(0.50+3.*S) +
     &  1.273 * EXP(-10.*S) * X**0.40 * X1**(1.76+3.*S)
        XSEA = (0.1-0.397*S2+1.121*S3)/(1.+5.61*S2+5.26*S3) *
     &  X**(-7.32*S2/(1.+10.3*S2)) *
     &  X1**((3.76+15.*S+12.*S2)/(1.+4.*S))
        XSEA0 = 0.100 * X1**3.76
      ENDIF

C...Evaluate set 1M parton distributions below or above threshold.
      ELSEIF(ISET.EQ.2) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = 0.8477 * X**0.51 * X1**1.37
        XGLU = 3.42 * X**0.255 * X1**2.37
        XSEA = 0.
      ELSE
        XVAL = 0.8477/(1.+1.37*S+2.18*S2+3.73*S3) * X**(0.51+0.21*S)
     &  * X1**1.37 * XL**(2.667*S)
        XGLU = 24.*S/(1.+9.6*S+0.92*S2+14.34*S3) * EXP(-5.94*S) *
     &  X**((-0.013-1.80*S)/(1.+3.14*S)) * X1**(2.37+0.4*S) *
     &  XL**(0.32+3.6*S) + 3.42 * EXP(-12.*S) * X**0.255 *
     &  X1**(2.37+3.*S)
        XSEA = 0.842*S/(1.+21.3*S-33.2*S2+229.*S3) *
     &  X**((0.13-2.90*S)/(1.+5.44*S)) * X1**(3.45+0.5*S) *
     &  XL**(2.8*S)
        XSEA0 = 0.
      ENDIF

C...Evaluate set 2D parton distributions below or above threshold.
      ELSEIF(ISET.EQ.3) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = X**0.46 * X1**0.64 + 0.76 * X
        XGLU = 1.925 * X1**2
        XSEA = 0.242 * X1**4
      ELSE
        XVAL = (1.+0.186*S)/(1.-0.209*S+1.495*S2) * X**(0.46+0.25*S)
     &  * X1**((0.64+0.14*S+5.*S2)/(1.+S)) * XL**(1.9*S) +
     &  (0.76+0.4*S) * X * X1**(2.667*S)
        XGLU = (1.925+5.55*S+147.*S2)/(1.-3.59*S+3.32*S2) *
     &  EXP(-18.67*S) * X**((-5.81*S-5.34*S2)/(1.+29.*S-4.26*S2))
     &  * X1**((2.-5.9*S)/(1.+1.7*S)) * XL**(9.3*S/(1.+1.7*S))
        XSEA = (0.242-0.252*S+1.19*S2)/(1.-0.607*S+21.95*S2) *
     &  X**(-12.1*S2/(1.+2.62*S+16.7*S2)) * X1**4 * XL**S
        XSEA0 = 0.242 * X1**4
      ENDIF

C...Evaluate set 2M parton distributions below or above threshold.
      ELSEIF(ISET.EQ.4) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = 1.168 * X**0.50 * X1**2.60 + 0.965 * X
        XGLU = 1.808 * X1**2
        XSEA = 0.209 * X1**4
      ELSE
        XVAL = (1.168+1.771*S+29.35*S2) * EXP(-5.776*S) *
     &  X**((0.5+0.208*S)/(1.-0.794*S+1.516*S2)) *
     &  X1**((2.6+7.6*S)/(1.+5.*S)) * XL**(5.15*S/(1.+2.*S)) +
     &  (0.965+22.35*S)/(1.+18.4*S) * X * X1**(2.667*S)
        XGLU = (1.808+29.9*S)/(1.+26.4*S) * EXP(-5.28*S) *
     &  X**((-5.35*S-10.11*S2)/(1.+31.71*S)) *
     &  X1**((2.-7.3*S+4.*S2)/(1.+2.5*S)) *
     &  XL**(10.9*S/(1.+2.5*S))
        XSEA = (0.209+0.644*S2)/(1.+0.319*S+17.6*S2) *
     &  X**((-0.373*S-7.71*S2)/(1.+0.815*S+11.0*S2)) *
     &  X1**(4.+S) * XL**(0.45*S)
        XSEA0 = 0.209 * X1**4
      ENDIF
      ENDIF

C...Threshold factors for c and b sea.
      SLL=LOG(LOG(Q2EFF/ALAM**2)/LOG(P2EFF/ALAM**2))
      XCHM=0.
      IF(Q2.GT.PMC**2.AND.Q2.GT.1.001*P2EFF) THEN
        SCH=MAX(0.,LOG(LOG(PMC**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
        IF(ISET.EQ.0) THEN
          XCHM=XSEA*(1.-(SCH/SLL)**2)
        ELSE
          XCHM=MAX(0.,XSEA-XSEA0*X1**(2.667*S))*(1.-SCH/SLL)
        ENDIF
      ENDIF
      XBOT=0.
      IF(Q2.GT.PMB**2.AND.Q2.GT.1.001*P2EFF) THEN
        SBT=MAX(0.,LOG(LOG(PMB**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
        IF(ISET.EQ.0) THEN
          XBOT=XSEA*(1.-(SBT/SLL)**2)
        ELSE
          XBOT=MAX(0.,XSEA-XSEA0*X1**(2.667*S))*(1.-SBT/SLL)
        ENDIF
      ENDIF

C...Fill parton distributions.
      XPGA(0)=XGLU
      XPGA(1)=XSEA
      XPGA(2)=XSEA
      XPGA(3)=XSEA
      XPGA(4)=XCHM
      XPGA(5)=XBOT
      XPGA(KFA)=XPGA(KFA)+XVAL
      DO 110 KFL=1,5
      XPGA(-KFL)=XPGA(KFL)
  110 CONTINUE

      RETURN
      END

C*********************************************************************

      SUBROUTINE SASANO(KF,X,Q2,P2,ALAM,XPGA)
C...Purpose: to evaluate the parton distributions of the anomalous
C...photon, inhomogeneously evolved from a scale P2 (where it vanishes)
C...to Q2.
C...KF=0 gives the sum over (up to) 5 flavours,
C...KF<0 limits to flavours up to abs(KF),
C...KF>0 is for flavour KF only.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      DIMENSION XPGA(-6:6),ALAMSQ(3:5)
      DATA PMC/1.3/, PMB/4.6/, AEM/0.007297/, AEM2PI/0.0011614/

C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
      IF(Q2.LE.P2) RETURN
      KFA=IABS(KF)

C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      ALAMSQ(3)=(ALAM*(PMC/ALAM)**(2./27.))**2
      ALAMSQ(4)=ALAM**2
      ALAMSQ(5)=(ALAM*(ALAM/PMB)**(2./23.))**2
      P2EFF=MAX(P2,1.2*ALAMSQ(3))
      IF(KF.EQ.4) P2EFF=MAX(P2EFF,PMC**2)
      IF(KF.EQ.5) P2EFF=MAX(P2EFF,PMB**2)
      Q2EFF=MAX(Q2,P2EFF)
      XL=-LOG(X)

C...Find number of flavours at lower and upper scale.
      NFP=4
      IF(P2EFF.LT.PMC**2) NFP=3
      IF(P2EFF.GT.PMB**2) NFP=5
      NFQ=4
      IF(Q2EFF.LT.PMC**2) NFQ=3
      IF(Q2EFF.GT.PMB**2) NFQ=5

C...Define range of flavour loop.
      IF(KF.EQ.0) THEN
        KFLMN=1
        KFLMX=5
      ELSEIF(KF.LT.0) THEN
        KFLMN=1
        KFLMX=KFA
      ELSE
        KFLMN=KFA
        KFLMX=KFA
      ENDIF

C...Loop over flavours the photon can branch into.
      DO 110 KFL=KFLMN,KFLMX

C...Light flavours: calculate t range and (approximate) s range.
      IF(KFL.LE.3.AND.(KFL.EQ.1.OR.KFL.EQ.KF)) THEN
        TDIFF=LOG(Q2EFF/P2EFF)
        S=(6./(33.-2.*NFQ))*LOG(LOG(Q2EFF/ALAMSQ(NFQ))/
     &  LOG(P2EFF/ALAMSQ(NFQ)))
        IF(NFQ.GT.NFP) THEN
          Q2DIV=PMB**2
          IF(NFQ.EQ.4) Q2DIV=PMC**2
          SNFQ=(6./(33.-2.*NFQ))*LOG(LOG(Q2DIV/ALAMSQ(NFQ))/
     &    LOG(P2EFF/ALAMSQ(NFQ)))
          SNFP=(6./(33.-2.*(NFQ-1)))*LOG(LOG(Q2DIV/ALAMSQ(NFQ-1))/
     &    LOG(P2EFF/ALAMSQ(NFQ-1)))
          S=S+(LOG(Q2DIV/P2EFF)/LOG(Q2EFF/P2EFF))*(SNFP-SNFQ)
        ENDIF
        IF(NFQ.EQ.5.AND.NFP.EQ.3) THEN
          Q2DIV=PMC**2
          SNF4=(6./(33.-2.*4))*LOG(LOG(Q2DIV/ALAMSQ(4))/
     &    LOG(P2EFF/ALAMSQ(4)))
          SNF3=(6./(33.-2.*3))*LOG(LOG(Q2DIV/ALAMSQ(3))/
     &    LOG(P2EFF/ALAMSQ(3)))
          S=S+(LOG(Q2DIV/P2EFF)/LOG(Q2EFF/P2EFF))*(SNF3-SNF4)
        ENDIF

C...u and s quark do not need a separate treatment when d has been done.
      ELSEIF(KFL.EQ.2.OR.KFL.EQ.3) THEN

C...Charm: as above, but only include range above c threshold.
      ELSEIF(KFL.EQ.4) THEN
        IF(Q2.LE.PMC**2) GOTO 110
        P2EFF=MAX(P2EFF,PMC**2)
        Q2EFF=MAX(Q2EFF,P2EFF)
        TDIFF=LOG(Q2EFF/P2EFF)
        S=(6./(33.-2.*NFQ))*LOG(LOG(Q2EFF/ALAMSQ(NFQ))/
     &  LOG(P2EFF/ALAMSQ(NFQ)))
        IF(NFQ.EQ.5.AND.NFP.EQ.4) THEN
          Q2DIV=PMB**2
          SNFQ=(6./(33.-2.*NFQ))*LOG(LOG(Q2DIV/ALAMSQ(NFQ))/
     &    LOG(P2EFF/ALAMSQ(NFQ)))
          SNFP=(6./(33.-2.*(NFQ-1)))*LOG(LOG(Q2DIV/ALAMSQ(NFQ-1))/
     &    LOG(P2EFF/ALAMSQ(NFQ-1)))
          S=S+(LOG(Q2DIV/P2EFF)/LOG(Q2EFF/P2EFF))*(SNFP-SNFQ)
        ENDIF

C...Bottom: as above, but only include range above b threshold.
      ELSEIF(KFL.EQ.5) THEN
        IF(Q2.LE.PMB**2) GOTO 110
        P2EFF=MAX(P2EFF,PMB**2)
        Q2EFF=MAX(Q2,P2EFF)
        TDIFF=LOG(Q2EFF/P2EFF)
        S=(6./(33.-2.*NFQ))*LOG(LOG(Q2EFF/ALAMSQ(NFQ))/
     &  LOG(P2EFF/ALAMSQ(NFQ)))
      ENDIF

C...Evaluate flavour-dependent prefactor (charge^2 etc.).
      CHSQ=1./9.
      IF(KFL.EQ.2.OR.KFL.EQ.4) CHSQ=4./9.
      FAC=AEM2PI*2.*CHSQ*TDIFF

C...Evaluate parton distributions (normalized to unit momentum sum).
      IF(KFL.EQ.1.OR.KFL.EQ.4.OR.KFL.EQ.5.OR.KFL.EQ.KF) THEN
        XVAL= ((1.5+2.49*S+26.9*S**2)/(1.+32.3*S**2)*X**2 +
     &  (1.5-0.49*S+7.83*S**2)/(1.+7.68*S**2)*(1.-X)**2 +
     &  1.5*S/(1.-3.2*S+7.*S**2)*X*(1.-X)) *
     &  X**(1./(1.+0.58*S)) * (1.-X**2)**(2.5*S/(1.+10.*S))
        XGLU= 2.*S/(1.+4.*S+7.*S**2) *
     &  X**(-1.67*S/(1.+2.*S)) * (1.-X**2)**(1.2*S) *
     &  ((4.*X**2+7.*X+4.)*(1.-X)/3. - 2.*X*(1.+X)*XL)
        XSEA= 0.333*S**2/(1.+4.90*S+4.69*S**2+21.4*S**3) *
     &  X**(-1.18*S/(1.+1.22*S)) * (1.-X)**(1.2*S) *
     &  ((8.-73.*X+62.*X**2)*(1.-X)/9. + (3.-8.*X**2/3.)*X*XL +
     &  (2.*X-1.)*X*XL**2)

C...Threshold factors for c and b sea.
        SLL=LOG(LOG(Q2EFF/ALAM**2)/LOG(P2EFF/ALAM**2))
        XCHM=0.
        IF(Q2.GT.PMC**2.AND.Q2.GT.1.001*P2EFF) THEN
          SCH=MAX(0.,LOG(LOG(PMC**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
          XCHM=XSEA*(1.-(SCH/SLL)**3)
        ENDIF
        XBOT=0.
        IF(Q2.GT.PMB**2.AND.Q2.GT.1.001*P2EFF) THEN
          SBT=MAX(0.,LOG(LOG(PMB**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
          XBOT=XSEA*(1.-(SBT/SLL)**3)
        ENDIF
      ENDIF

C...Add contribution of each valence flavour.
      XPGA(0)=XPGA(0)+FAC*XGLU
      XPGA(1)=XPGA(1)+FAC*XSEA
      XPGA(2)=XPGA(2)+FAC*XSEA
      XPGA(3)=XPGA(3)+FAC*XSEA
      XPGA(4)=XPGA(4)+FAC*XCHM
      XPGA(5)=XPGA(5)+FAC*XBOT
      XPGA(KFL)=XPGA(KFL)+FAC*XVAL
  110 CONTINUE
      DO 120 KFL=1,5
      XPGA(-KFL)=XPGA(KFL)
  120 CONTINUE

      RETURN
      END

C*********************************************************************

      SUBROUTINE SASBEH(KF,X,Q2,P2,PM2,XPBH)
C...Purpose: to evaluate the Bethe-Heitler cross section for
C...heavy flavour production.
      DATA AEM2PI/0.0011614/

C...Reset output.
      XPBH=0.
      SIGBH=0.

C...Check kinematics limits.
      IF(X.GE.Q2/(4.*PM2+Q2+P2)) RETURN
      W2=Q2*(1.-X)/X-P2
      BETA2=1.-4.*PM2/W2
      IF(BETA2.LT.1E-10) RETURN
      RMQ=4.*PM2/Q2

C...Simple case: P2 = 0.
      IF(P2.LT.1E-4) THEN
        BETA=SQRT(BETA2)
        IF(BETA.LT.0.99) THEN
          XBL=LOG((1.+BETA)/(1.-BETA))
        ELSE
          XBL=LOG((1.+BETA)**2*W2/(4.*PM2))
        ENDIF
        SIGBH=BETA*(8.*X*(1.-X)-1.-RMQ*X*(1.-X))+
     &  XBL*(X**2+(1.-X)**2+RMQ*X*(1.-3.*X)-0.5*RMQ**2*X**2)

C...Complicated case: P2 > 0, based on approximation of
C...C.T. Hill and G.G. Ross, Nucl. Phys. B148 (1979) 373
      ELSE
        RPQ=1.-4.*X**2*P2/Q2
        IF(RPQ.GT.1E-10) THEN
          RPBE=SQRT(RPQ*BETA2)
          IF(RPBE.LT.0.99) THEN
            XBL=LOG((1.+RPBE)/(1.-RPBE))
            XBI=2.*RPBE/(1.-RPBE**2)
          ELSE
            RPBESN=4.*PM2/W2+(4.*X**2*P2/Q2)*BETA2
            XBL=LOG((1.+RPBE)**2/RPBESN)
            XBI=2.*RPBE/RPBESN
          ENDIF
          SIGBH=BETA*(6.*X*(1.-X)-1.)+
     &    XBL*(X**2+(1.-X)**2+RMQ*X*(1.-3.*X)-0.5*RMQ**2*X**2)+
     &    XBI*(2.*X/Q2)*(PM2*X*(2.-RMQ)-P2*X)
        ENDIF
      ENDIF

C...Multiply by charge-squared etc. to get parton distribution.
      CHSQ=1./9.
      IF(IABS(KF).EQ.2.OR.IABS(KF).EQ.4) CHSQ=4./9.
      XPBH=3.*CHSQ*AEM2PI*X*SIGBH

      RETURN
      END

C*********************************************************************

       SUBROUTINE SASDIR(X,Q2,P2,AK0,XPGA)
C...Purpose: to evaluate the direct contribution, i.e. the C^gamma term,
C...as needed in MSbar parametrizations.
      DIMENSION XPGA(-6:6)
      DATA PMC/1.3/, PMB/4.6/, AEM2PI/0.0011614/

C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE

C...Evaluate common x-dependent expression.
      XTMP = (X**2+(1.-X)**2) * (-LOG(X)) - 1.
      CGAM = 3.*AEM2PI*X * (XTMP*(1.+P2/(P2+AK0**2)) + 6.*X*(1.-X))

C...d, u, s part by simple charge factor.
      XPGA(1)=(1./9.)*CGAM
      XPGA(2)=(4./9.)*CGAM
      XPGA(3)=(1./9.)*CGAM

C...Also fill for antiquarks.
      DO 110 KF=1,5
      XPGA(-KF)=XPGA(KF)
  110 CONTINUE

      RETURN
      END
C
C
C**********************************************************************
C
C   dummy subroutine, remove to link PHOLIB
C
C**********************************************************************
      SUBROUTINE PHVAL(IGRP,ISET,XI,SCALE2,PD,IRET)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PD(-6:6)
      END
C
      SUBROUTINE QUASTA(IFL,IDPDG,IMODE)
C**********************************************************************
C
C     collect quark statistics
C     input:    IFL    parton flavour (PDG number)
C               IDPDG  mother particle (PDG number)
C               IMODE  1 .. 6   collect statistics
C                               (1,2,3,4 = valence,reggeon,sea,hard)
C                      -1       initialization
C                      -2       output
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      SAVE
C  global energy
      COMMON /GLOCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)
C
      CHARACTER*7 PDGNAM
      CHARACTER*8 HADNAM,PANAME
      CHARACTER*16 SPLNAM
      DIMENSION ICPART(-23:23,22),HADNAM(4),SPLNAM(7),PDGNAM(-23:23)
      DATA PDGNAM / '-others','-(cc)_3', ' (cs)_1',' (cs)_0',
     &              ' (cu)_1',' (cu)_0', ' (cd)_1',' (cd)_0',
     &              ' (ss)_3',
     &              '-(su)_1','-(su)_0', '-(sd)_1','-(sd)_0',
     &              '-(uu)_1','-(ud)_1', '-(ud)_0','-(dd)_1',
     &              '  -t   ','  -b   ', '  -c   ','  -s   ',
     &              '  -u   ','  -d   ', '   g   ',
     &              '   d   ','   u   ', '   s   ','   c   ',
     &              '   b   ','   t   ', ' (dd)_1',' (ud)_0',
     &              ' (ud)_1',' (uu)_1', ' (sd)_0',' (sd)_1',
     &              ' (su)_0',' (su)_1', ' (ss)_3',
     &              ' (cd)_0',' (cd)_1', ' (cu)_0',' (cu)_1',
     &              ' (cs)_0',' (cs)_1', ' (cc)_3',' others' /
      DATA SPLNAM / 'VALENCE  PARTONS',
     &              'REGGEON  PARTONS',
     &              'SEA      PARTONS',
     &              'HARD INI PARTONS',
     &              'SPECTATORS / ISR',
     &              'HARD FIN PARTONS',
     &              'FINAL    PARTONS' /
C
      IF((IMODE.GT.0).AND.(IMODE.LT.8)) THEN
        IF(IMODE.LT.6) THEN
          IF(IDPDG.EQ.IFPAP(1)) THEN
            K = 1
          ELSE IF(IDPDG.EQ.IFPAP(2)) THEN
            K = 2
          ELSE IF(IDPDG.EQ.45) THEN
            K = 3
          ELSE
            K = 4
          ENDIF
          K = 4*(IMODE-1)+K
        ELSE
          K = 15+IMODE
        ENDIF
        IFLA = ABS(IFL)
        IF(IFLA.LE.6) THEN
          ICPART(IFL,K) = ICPART(IFL,K)+1
        ELSE IF((IFL.EQ.21).OR.(IFL.EQ.9)) THEN
          ICPART(0,K) = ICPART(0,K)+1
        ELSE
          INDX = 23
          IF(IFLA.EQ.1103) THEN
            INDX = 7
          ELSE IF(IFLA.EQ.2101) THEN
            INDX = 8
          ELSE IF(IFLA.EQ.2103) THEN
            INDX = 9
          ELSE IF(IFLA.EQ.2203) THEN
            INDX = 10
          ELSE IF(IFLA.EQ.3101) THEN
            INDX = 11
          ELSE IF(IFLA.EQ.3103) THEN
            INDX = 12
          ELSE IF(IFLA.EQ.3201) THEN
            INDX = 13
          ELSE IF(IFLA.EQ.3203) THEN
            INDX = 14
          ELSE IF(IFLA.EQ.3303) THEN
            INDX = 15
          ELSE IF(IFLA.EQ.4101) THEN
            INDX = 16
          ELSE IF(IFLA.EQ.4103) THEN
            INDX = 17
          ELSE IF(IFLA.EQ.4201) THEN
            INDX = 18
          ELSE IF(IFLA.EQ.4203) THEN
            INDX = 19
          ELSE IF(IFLA.EQ.4303) THEN
            INDX = 20
          ELSE IF(IFLA.EQ.4301) THEN
            INDX = 21
          ELSE IF(IFLA.EQ.4403) THEN
            INDX = 22
          ENDIF
          INDX = SIGN(INDX,IFL)
          ICPART(INDX,K) = ICPART(INDX,K)+1
        ENDIF
C  initialization
      ELSE IF(IMODE.EQ.-1) THEN
        DO 100 I=1,22
          DO 110 K=-23,23
            ICPART(K,I) = 0
 110      CONTINUE
 100    CONTINUE
        HADNAM(1) = PANAME(IFPAB(1),0)
        HADNAM(2) = PANAME(IFPAB(2),0)
        HADNAM(3) = PANAME(184,0)
        HADNAM(4) = 'OTHERS  '
C  output of statistics
      ELSE IF(IMODE.EQ.-2) THEN
        WRITE(6,'(2(/,1X,A))') 'QUASTA:QUARK STATISTICS:',
     &                         '======================='
        DO 200 I=1,5
          I1 = 4*(I-1)+1
          I2 = 4*(I-1)+4
          WRITE(6,'(1X,A16,8X,4(2X,A8))') SPLNAM(I),HADNAM
          DO 250 K=-23,23
            ISUM = 0
            DO 260 L=I1,I2
              ISUM = ISUM+ICPART(K,L)
 260        CONTINUE
            IF(ISUM.GT.0) WRITE(6,'(10X,A,2X,4I12)') PDGNAM(K),
     &        (ICPART(K,L),L=I1,I2)
 250      CONTINUE
 200    CONTINUE
        DO 300 I=6,7
          WRITE(6,'(1X,A16)') SPLNAM(I)
          L = 15+I
          DO 350 K=-23,23
            IF(ICPART(K,L).GT.0) WRITE(6,'(10X,A,2X,I12)') PDGNAM(K),
     &        ICPART(K,L)
 350      CONTINUE
 300    CONTINUE
      ELSE
        WRITE(6,'(/,1X,A,I3)') 'QUASTA:ERROR: UNSUPPORTED IMODE',IMODE
        CALL POABRT
      ENDIF
      END
C
C
C**********************************************************************
C
C   dummy subroutines, remove to link PDFLIB
C
C**********************************************************************
      SUBROUTINE PDFSET(PARAM,VALUE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARAM(20),VALUE(20)
      CHARACTER*20 PARAM
      END
      SUBROUTINE STRUCTM(XI,SCALE,UV,DV,US,DS,SS,CS,BS,TS,GL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      END
      SUBROUTINE PFTOPDG(XI,SCALE,PD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PD(-6:6)
      END
      double precision function drndm(dummy)
      DOUBLE PRECISION DUMMY
      Real*4 rndm
      drndm = dble(rndm(sngl(dummy)))
      return
      end
