cFrom BLOCH@alws.cern.ch Fri Feb 13 15:48:50 2004
cDate: Fri, 13 Feb 2004 15:47:50 +0100
cFrom: BLOCH@alws.cern.ch
cTo: BLOCH@alws.cern.ch

C*HE 02/08/91 11:08:18 C
C*DK ASKUSE
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
      COMMON /KGCOMM/ IST,NTR,IDP,ECM,WEI
      LENTRY = 2
      CALL BHAL01(LENTRY)
      IDPR = IDP
      NTRK = NTR
      NVRT = 1
      ISTA = IST
      ECMS = ECM
      WEIT = WEI
      RETURN
      END
C*DK ASKUSI
      SUBROUTINE ASKUSI(IGCOD)
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
C*CA BCS
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA UNICOM
      COMMON / UNICOM / IIN,IUT
C*CC UNICOM
      PARAMETER ( IGCO = 2008)
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      IUT = IW(6)
      WRITE(IUT,101) IGCOD
 101  FORMAT(/,10X,
     &       'BHAL01 - CODE NUMBER =',I4,' Last mod. February 8,1991',
     & /,10X,'***********************************************',//)
C
      LENTRY = 1
C
C  CORRECTION TO BE DONE TO TAKE STANDARD ALEPH MASSES DEFINITIONS
C
      CALL BHAL01(LENTRY)
C  Print PART and KLIN banks
C
      CALL PRPART
C
      CALL PRTABL('RLEP',0)
      CALL PRTABL('KPAR',0)
C
      RETURN
      END
C*DK BHAL01
      SUBROUTINE BHAL01(LENTRY)
C***************************************************************
C
C     MONTE CARLO EVENT GENERATOR FOR THE PROCESSES
C
C         E+(PP) E-(PM)   ---->  E+(QP) E-(QM)
C
C     AND
C
C         E+(PP) E-(PM)   ----> E+(QP) E-(QM) PHOTON(QK)
C
C
C  AUTHORS: R. KLEISS, CERN
C           F.A. BERENDS, LEIDEN UNIVERSITY, LEIDEN, HOLLAND
C           W. HOLLIK, HAMBURG UNIVERSITY, HAMBURG, GERMANY
C
C
C  THE INPUT QUANTITIES ARE
C  EB    = BEAM ENERGY, IN GEV;
C  XMZ   = MASS OF THE Z0 BOSON, IN GEV;
C  XMH   = MASS OF THE HIGGS BOSON, IN GEV;
C  XMT   = MASS OF THE TOP QUARK, IN GEV;
C  THMIN = MINIMUM POLAR SCATTERING ANGLE OF THE E+,E-, IN
C          DEGREES: IT MUST BE LARGER THAN 0 DEGREES BUT
C          SMALLER THAN THMAX;
C  THMAX = MAXIMUM POLAR SCATTERING ANGLE OF THE E+,E-, IN
C          DEGREES: IT MUST BE SMALLER THAN 180 DEGREES BUT
C          LARGER THAN THMIN;
C  XKMAX = MAXIMUM ENERGY OF THE BREMSSTRAHLUNG PHOTON, AS A
C          FRACTION OF THE BEAM ENERGY E: IT MAY BE SET
C          TO 1, IN WHICH CASE THE PROGRAM CHANGES ITS VALUE
C          TO THE ACTUAL KINEMATIC LIMIT.
C
C
C  IN THE PRESENT PROGRAM THE W MASS AND THE ELECTROWEAK MIXING ANGLE
C  ARE CALCULATED FROM THE (AS YET) MORE ACCURATELY KNOWN VALUE
C  OF THEMUON LIFETIME. THIS IS DONE BY ROUTINE FINDMW
C
C***************************************************************
C  THIS IS THE MAIN PROGRAM, CONSISTING OF:
C 1) INITIALIZATION OF THE GENERATOR;
C 2) GENERATION OF AN EVENT SAMPLE,
C    AND SUBSEQUENT ANALYSIS OF THE EVENTS;
C 3) EVALUATION OF THE TOTAL GENERATED CROSS SECTION
C
C
C*CA BCS
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA UNICOM
      COMMON / UNICOM / IIN,IUT
C*CC UNICOM
C
      COMMON /KGCOMM/ ISTA,NTR,IDPR,ECMS,WEIT
      REAL*8 EB,XMZ,XMT,XMH,THMIN,THMAX,XKMAX,W,S2W
      REAL*8 SIGTOT,ERRTOT
      REAL*8 PP(4),PM(4),QP(4),QM(4),QK(4)
      DIMENSION SDVRT(3),VRTEX(4),TABL(12),TABK(4),ITAB(3)
      DIMENSION NEVENT(10),NEVPHO(2)
      COMMON / INPUT1 / EB
      COMMON / INPUT2 / XMZ,S2W,XMH,XMT
      COMMON / INPUT3 / THMIN,THMAX,XKMAX
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL ,ALRLEP
C
C  INITIALIZATION            *********************
C
      IF(LENTRY.EQ.1) THEN
C
C THE SETUP PHASE: ASK FOR THE INPUT PARAMETERS
C
       IIN=5
       EB      =   46.1D0
       XMZ     =   92.0D0
       XMT     =   60.0D0
       XMH     =  100.0D0
       THMIN   =   10.0D0
       THMAX   =  170.0D0
       XKMAX   =    1.0D0
       WEIOPT  =    1.
       WTMAX   =    1.7
C
C  The default values can be changed by the DATA CARD GENE
       JGENE = NLINK('GENE',0)
       IF(JGENE.NE.0) THEN
        EB = RW(JGENE+1)
        XMZ = RW(JGENE+2)
        XMH = RW(JGENE+3)
        XMT = RW(JGENE+4)
        THMIN = RW(JGENE+5)
        THMAX = RW(JGENE+6)
        XKMAX = RW(JGENE+7)
        WEIOPT  = RW(JGENE+8)
        WTMAX   = RW(JGENE+9)
       ENDIF
       TABL(1) = EB
       TABL(2) = XMZ
       TABL(3) = XMT
       TABL(4) = XMH
       TABL(5) = THMIN
       TABL(6) = THMAX
       TABL(7) = XKMAX
       TABL(8) = WEIOPT
       TABL(9) = WTMAX
C
C  Main vertex generation
       SDVRT(1) = 0.035
       SDVRT(2) = 0.0012
       SDVRT(3) = 1.28
       JSVRT = NLINK('SVRT',0)
       IF(JSVRT.NE.0) THEN
        SDVRT(1) = RW(JSVRT+1)
        SDVRT(2) = RW(JSVRT+2)
        SDVRT(3) = RW(JSVRT+3)
       ENDIF
       TABL(10)= SDVRT(1)
       TABL(11)= SDVRT(2)
       TABL(12)= SDVRT(3)
C
C  Fill the KPAR bank with the generator parameters
       NCOL = 12
       NROW = 1
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
C  Fill RLEP bank
       IEBEAM = NINT(EB *1000  )
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
C
C  Initialize events counters
       DO 10 I = 1,10
   10  NEVENT(I) = 0
       DO 11 I = 1,2
   11  NEVPHO(I) = 0
C
C Booking of some standard histogrammes
       CALL HBOOK1(10001,'Energy distribution : final e+$',30,0.,60.,0.)
       CALL HIDOPT(10001,'LOGY')
       CALL HBOOK1(10002,'Energy distribution : final e-$',30,0.,60.,0.)
       CALL HIDOPT(10002,'LOGY')
       CALL HBOOK1(10003,'Energy distribution : gamma$',30,0.,60.,0.)
       CALL HIDOPT(10003,'LOGY')
       CALL HBOOK1(10004,'Energy distribution : gamma < 1. GeV$',
     &                                                     40,0.,1.,0.)
       CALL HIDOPT(10004,'LOGY')
       CALL HBOOK1(10005,'Polar angle distribution : final e+$',
     &                                                     40,-1.,1.,0.)
       CALL HIDOPT(10005,'LOGY')
       CALL HBOOK1(10006,'Polar angle distribution : final e-$',
     &                                                     40,-1.,1.,0.)
       CALL HIDOPT(10006,'LOGY')
       CALL HBOOK1(10007,'Weight distribution for hard events$',
     &                                                     40,0.,2.,0.)
       CALL HBOOK2(10008,'Hard events : weight distribution    versus
     &photon energy$',30,0.,60.,40,0.,2.,0.)
C
C THE INITIALIZATION STEP OF THE PROGRAM
       CALL SETBAB(EB,XMZ,XMH,XMT,THMIN,THMAX,XKMAX)
C GB   CALL OUTCRY('GENBAB')
       RETURN
      ENDIF
C
C  EVENT GENERATION          *********************
C
      IF(LENTRY.EQ.2) THEN
C
C  EVENT STATUS (0 = O.K.)
       ISTA = 0
C
C  Initialize the track number (radiated photon included)
       NTR = 3
C
C  Generate primary vertex
       CALL RANNOR (RN1,RN2)
       CALL RANNOR (RN3,DUM)
       VRTEX(1) = RN1*SDVRT(1)
       VRTEX(2) = RN2*SDVRT(2)
       VRTEX(3) = RN3*SDVRT(3)
       VRTEX(4) = 0.
C
C GB   CALL TELLER(K,1000,'EVENT LOOP')
 1     NEVENT(1) = NEVENT(1) + 1
       CALL GENBAB(PP,PM,QP,QM,QK,W,ICON)
       CALL CANCUT(QP,QM,QK,W)
       IDPR = ICON
       WEIT = W
       ECMS = 2.*EB
       WTMAX = MAX(WEIT,WTMAX)
       IF (WEIOPT.EQ.1.) THEN
          IF (WEIT/WTMAX .LT. RNDM(DUM)) THEN
            GO TO 1
          ENDIF
          WEIT = 1.
       ENDIF
C
C  Now fill 'KINE' and 'VERT' banks
C
       IVMAI = 1
       JVERT = KBVERT(IVMAI,VRTEX,0)
       IF(JVERT.EQ.0) THEN
        ISTA = 1
        NEVENT(2) = NEVENT(2) + 1
        GO TO 97
       ENDIF
C  book 'KINE' for beam electrons (-1 and -2)
       DO 90 I = 1,3
   90   TABK(I) = (-1.**I)*PP(I)
       TABK(4) = 0.
       JKINE = KBKINE(-1,TABK,2,0)
       IF(JKINE.EQ.0) THEN
        ISTA = 2
        NEVENT(3) = NEVENT(3) + 1
        GO TO 97
       ENDIF
       DO 91 I = 1,3
   91   TABK(I) = (-1.**I)*PM(I)
       TABK(4) = 0.
       JKINE = KBKINE(-2,TABK,3,0)
       IF(JKINE.EQ.0) THEN
        ISTA = 3
        NEVENT(4) = NEVENT(4) + 1
        GO TO 97
       ENDIF
C  book 'KINE' for final state particles
       DO 92 I = 1,3
   92   TABK(I) = (-1.**I)*QP(I)
       TABK(4) = 0.
       JKINE = KBKINE(1,TABK,2,IVMAI)
       IF(JKINE.EQ.0) THEN
        ISTA = 4
        NEVENT(5) = NEVENT(5) + 1
        GO TO 97
       ENDIF
       DO 93 I = 1,3
   93   TABK(I) = (-1.**I)*QM(I)
       TABK(4) = 0.
       JKINE = KBKINE(2,TABK,3,IVMAI)
       IF(JKINE.EQ.0) THEN
        ISTA = 5
        NEVENT(6) = NEVENT(6) + 1
        GO TO 97
       ENDIF
C We did not book the radiated photon if the energy is equal to zero
       IF(QK(4).LT.1.E-06) THEN
        NTR = 2
        GO TO 95
       ENDIF
       DO 94 I = 1,3
   94   TABK(I) = (-1.**I)*QK(I)
       TABK(4) = 0.
       JKINE = KBKINE(3,TABK,1,IVMAI)
       IF(JKINE.EQ.0) THEN
        ISTA = 6
        NEVENT(7) = NEVENT(7) + 1
        GO TO 97
       ENDIF
C
C  Fill history with 'KHIS' bank
   95  DO 96 I = 1,NTR
   96  ITAB(I) = 0
       JKHIS = ALTABL('KHIS',1,NTR,ITAB,'I','E')
       IF(JKHIS.EQ.0) THEN
        ISTA = 7
        NEVENT(8) = NEVENT(8) + 1
       ENDIF
C
   97  IF(ISTA.NE.0) NEVENT(9) = NEVENT(9) + 1
       IF(ISTA.EQ.0) THEN
        NEVENT(10) = NEVENT(10) + 1
        IF(NTR.EQ.2) NEVPHO(1) = NEVPHO(1) +1
        IF(NTR.EQ.3) NEVPHO(2) = NEVPHO(2) +1
        CALL HFILL(10001,QP(4),0.,WEIT)
        CALL HFILL(10002,QM(4),0.,WEIT)
        CALL HFILL(10003,QK(4),0.,WEIT)
        IF(QK(4).LT.1.0D0) CALL HFILL(10004,QK(4),0.,WEIT)
C        ISIGNE = +1
C        IF(QP(3).GE.0.D0) ISIGNE=-1
        QPTHET = -1.*QP(3)/SQRT(QP(1)**2+QP(2)**2+QP(3)**2)
        CALL HFILL(10005,QPTHET,0.,WEIT)
        QMTHET = -1.*QM(3)/SQRT(QM(1)**2+QM(2)**2+QM(3)**2)
        CALL HFILL(10006,QMTHET,0.,WEIT)
        IF(QK(4).NE.0.0D0) THEN
         CALL HFILL(10007,WEIT,0.,1.)
         CALL HFILL(10008,QK(4),WEIT,1.)
        ENDIF
       ENDIF
C
       RETURN
      ENDIF
C
C  END OF GENERATION         *********************
C
      IF(LENTRY.EQ.3) THEN
C
C EVALUATION OF THE GENERATED CROSS SECTION
       CALL ENDBAB(SIGTOT,ERRTOT)
       CALL EFFCIT
       CALL ENDCUT(SIGTOT)
       WRITE(IUT,'(''   ********MAXIMUM WEIGHT REACHED '',F12.4)')WTMAX
       WRITE(IUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITHOUT PHOTON = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITH PHOTON    = ',I10,
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)
       WRITE(IUT,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(IUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6),
     &               NEVENT(7),NEVENT(8)
  104  FORMAT(/10X,'ISTA = 1 BOS ERROR VERT     # OF REJECT = ',I10,
     &        /10X,'ISTA = 2 BOS ERROR KINE e+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 3 BOS ERROR KINE e-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 4 BOS ERROR KINE f+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 5 BOS ERROR KINE f-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 6 BOS ERROR KINE gam # OF REJECT = ',I10,
     &        /10X,'ISTA = 7 BOS ERROR KHIS     # OF REJECT = ',I10)
      ENDIF
C
      RETURN
      END
C*DK USCJOB
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C
C --------------------------------------------------------------------
      LENTRY = 3
      CALL BHAL01(LENTRY)
      RETURN
      END
