      SUBROUTINE ASKUSI(IGCOD)
C --------------------------------------------------------------------
C G. Bonneaud October 1988.
C  B Bloch November 95 : fix argument number in HBOOK calls
C            write out code and date
C            create RLEP bank
C --------------------------------------------------------------------
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON / DTMILL / SDVRT(3),TABL(12),NEVENT(10),NEVPHO(2)
      COMMON / INPOUT / IOUT
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP
      PARAMETER ( IGCO = 1006)
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      IOUT = IW(6)
      WRITE(IOUT,101) IGCOD
 101  FORMAT(/,10X,
     &       'GGGB01 - CODE NUMBER =',I4,' Last mod. January ,2000',
     & /,10X,'***********************************************',//)
C
C   Input parameters (see description in GGGB01)
C
       EB    =    46.1
       RHO   =     2.0
       I3    =     0
       IODD  = 1234567
       ICONF =     0
       X1    =     0.02
       A1MIN =    10.0
       A1MAX =   170.0
       AVETO =    2.5
C
C  The default values can be changed by the DATA CARD GENE
C
       JGENE = NLINK('GENE',0)
       IF(JGENE.NE.0) THEN
        EB     = RW(JGENE+1)
        RHO    = RW(JGENE+2)
        I3     = IW(JGENE+3)
        IODD   = IW(JGENE+4)
        ICONF  = IW(JGENE+5)
        X1     = RW(JGENE+6)
        A1MIN  = RW(JGENE+7)
        A1MAX  = RW(JGENE+8)
        AVETO  = RW(JGENE+9)
       ENDIF
       TABL(1) = EB
       TABL(2) = RHO
       TABL(3) = I3
       TABL(4) = IODD
       TABL(5) = ICONF
       TABL(6) = X1
       TABL(7) = A1MIN
       TABL(8) = A1MAX
       TABL(9) = AVETO
C
C  Main vertex generation
C
       SDVRT(1) = 0.035
       SDVRT(2) = 0.0012
       SDVRT(3) = 1.28
       JSVRT = NLINK('SVRT',0)
       IF(JSVRT.NE.0) THEN
        SDVRT(1) = RW(JSVRT+1)
        SDVRT(2) = RW(JSVRT+2)
        SDVRT(3) = RW(JSVRT+3)
       ENDIF
       TABL(10) = SDVRT(1)
       TABL(11) = SDVRT(2)
       TABL(12) = SDVRT(3)
C
C  Fill the KPAR bank with the generator parameters
C
       NCOL = 12
       NROW = 1
       JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
C
C  Initialize events counters
C
       DO 10 I = 1,10
   10  NEVENT(I) = 0
       DO 11 I = 1,2
   11  NEVPHO(I) = 0
C
C Booking of some standard histogrammes
C
      CALL HBOOK1(10001,'WEIGHT(EXACT/APPR.) DIST. ',40,0.,RHO,0.)
      CALL HBOOK1(10002,' COS (THETA) G1 ',40,-1.,1.,0.)
      CALL HIDOPT(10002,'LOGY')
      CALL HBOOK1(10003,' ENERGY G1 ',40,0.,1.,0.)
      CALL HIDOPT(10003,'LOGY')
      CALL HBOOK1(10004,' COS (THETA) G2 ',40,-1.,1.,0.)
      CALL HIDOPT(10004,'LOGY')
      CALL HBOOK1(10005,' ENERGY G2 ',40,0.,1.,0.)
      CALL HIDOPT(10005,'LOGY')
      CALL HBOOK1(10006,' COS (THETA) G3 ',40,-1.,1.,0.)
      CALL HIDOPT(10006,'LOGY')
      CALL HBOOK1(10007,' ENERGY G3 ',40,0.,1.,0.)
      CALL HIDOPT(10007,'LOGY')
      CALL HBOOK2(10008,' X->CT G1/ Y->CT G2 ',40,-1.,1.,40,-1.,1.,0.)
      CALL HBOOK2(10009,' X->EN G1/ Y->EN G2 ',40,0.,1.,40,0.,1.,0.)
      CALL HBOOK1(10010,' ACOLINEARITY G1 G2 ',40,0.,180.,0.)
      CALL HIDOPT(10010,'LOGY')
      CALL HBOOK1(10011,' ACOPLANARITY G1 G2 ',40,0.,180.,0.)
      CALL HIDOPT(10011,'LOGY')
C
C  Generator initialization
C
      LENTRY = 1
      CALL GGGB01(LENTRY)
C
C  Print PART and KLIN banks
C
      CALL PRPART
      CALL PRTABL('KPAR',0)
      IEBEAM = NINT(EB*1000)
      JRLEP = ALRLEP(IEBEAM,'   ',0,0,0)
      CALL PRTABL('RLEP',0)
C
      RETURN
      END
      SUBROUTINE ASKUSE (IDP,IST,NTR,NVR,ECM,WEI)
C --------------------------------------------------------------------
C G. Bonneaud October 1988.
C --------------------------------------------------------------------
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON / DTMILL / SDVRT(3),TABL(12),NEVENT(10),NEVPHO(2)
      COMMON /KGCOMM/ ISTA,NTRK,IDPR,ECMS,WEIT
      COMMON / QUADRI / Q1(4),Q2(4),Q3(4)
      DIMENSION VRTEX(4),TABK(4),ITAB(3)
      INTEGER ALTABL
      EXTERNAL ALTABL
C
C  Generate primary vertex
C
      CALL RANNOR (RN1,RN2)
      CALL RANNOR (RN3,DUM)
      VRTEX(1) = RN1*SDVRT(1)
      VRTEX(2) = RN2*SDVRT(2)
      VRTEX(3) = RN3*SDVRT(3)
      VRTEX(4) = 0.
C
C  Fill 'VERT' bank
C
      IVMAI = 1
      JVERT = KBVERT(IVMAI,VRTEX,0)
      IF(JVERT.EQ.0) THEN
       ISTA = 1
       NEVENT(2) = NEVENT(2) + 1
       GO TO 97
      ENDIF
C
C  Event generation
C
      LENTRY = 2
      NEVENT(1) = NEVENT(1) + 1
      CALL GGGB01(LENTRY)
      NVR = 1
      IDP = IDPR
      ECM = ECMS
      WEI = WEIT
C
C  book 'KINE' for beam electrons (-1 and -2)
C
      TABK(1) = 0.
      TABK(2) = 0.
      TABK(3) = -TABL(1)
      TABK(4) = 0.
      JKINE = KBKINE(-1,TABK,2,0)
      IF(JKINE.EQ.0) THEN
       ISTA = 2
       NEVENT(3) = NEVENT(3) + 1
       GO TO 97
      ENDIF
      TABK(3) = TABL(1)
      TABK(4) = 0.
      JKINE = KBKINE(-2,TABK,3,0)
      IF(JKINE.EQ.0) THEN
       ISTA = 3
       NEVENT(4) = NEVENT(4) + 1
       GO TO 97
      ENDIF
C  book 'KINE' for final state particles
      IBOOKT = 0
C We did not book the photon if the energy is equal to zero
      IF(Q1(4).LT.1.E-06) THEN
       NTRK = NTRK -1
       GO TO 93
      ENDIF
      IBOOKT = IBOOKT + 1
      TABK(4) = 0.
      DO 92 I = 1,3
       TABK(I) = Q1(I)
   92 CONTINUE
      JKINE = KBKINE(IBOOKT,TABK,1,IVMAI)
      IF(JKINE.EQ.0) THEN
       ISTA = 4
       NEVENT(5) = NEVENT(5) + 1
       GO TO 99
      ENDIF
C We did not book the photon if the energy is equal to zero
   93 IF(Q2(4).LT.1.E-06) THEN
       NTRK = NTRK -1
       GO TO 95
      ENDIF
      IBOOKT = IBOOKT + 1
      TABK(4) = 0.
      DO 94 I = 1,3
       TABK(I) = Q2(I)
   94 CONTINUE
      JKINE = KBKINE(IBOOKT,TABK,1,IVMAI)
      IF(JKINE.EQ.0) THEN
       ISTA = 5
       NEVENT(6) = NEVENT(6) + 1
       GO TO 99
      ENDIF
C We did not book the photon if the energy is equal to zero
   95 IF(Q3(4).LT.1.E-06) THEN
       NTRK = NTRK -1
       GO TO 97
      ENDIF
      IBOOKT = IBOOKT + 1
      TABK(4) = 0.
      DO 96 I = 1,3
       TABK(I) = Q3(I)
   96 CONTINUE
      JKINE = KBKINE(IBOOKT,TABK,1,IVMAI)
      IF(JKINE.EQ.0) THEN
       ISTA = 6
       NEVENT(7) = NEVENT(7) + 1
       GO TO 99
      ENDIF
C
C  Fill history with 'KHIS' bank
C
   97 DO 98 I = 1,NTRK
   98 ITAB(I) = 0
      JKHIS = ALTABL('KHIS',1,NTRK,ITAB,'I','E')
      IF(JKHIS.EQ.0) THEN
       ISTA = 7
       NEVENT(8) = NEVENT(8) + 1
      ENDIF
C
   99 IF(ISTA.NE.0) NEVENT(9) = NEVENT(9) + 1
      IF(ISTA.EQ.0) THEN
       NEVENT(10) = NEVENT(10) + 1
       IF(NTRK.EQ.2) NEVPHO(1) = NEVPHO(1) +1
       IF(NTRK.EQ.3) NEVPHO(2) = NEVPHO(2) +1
      ENDIF
C
      NTR = NTRK
      IST = ISTA
      IDP = IDP + NTR
C
      RETURN
      END
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C G. Bonneaud October 1988.
C --------------------------------------------------------------------
      COMMON / DTMILL / SDVRT(3),TABL(12),NEVENT(10),NEVPHO(2)
      COMMON / INPOUT / IOUT
C
      LENTRY = 3
      CALL GGGB01(LENTRY)
C
      CALL UGTSEC
       WRITE(IOUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(IOUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITH 2 PHOTONS = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITH 3 PHOTONS = ',I10,
     &        /5X,'# OF REJECTED  EVENTS                = ',I10)
       WRITE(IOUT,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(IOUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6),
     &               NEVENT(7),NEVENT(8)
  104  FORMAT(/10X,'ISTA = 1 BOS ERROR VERT     # OF REJECT = ',I10,
     &        /10X,'ISTA = 2 BOS ERROR KINE e+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 3 BOS ERROR KINE e-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 4 BOS ERROR KINE gam1# OF REJECT = ',I10,
     &        /10X,'ISTA = 5 BOS ERROR KINE gam2# OF REJECT = ',I10,
     &        /10X,'ISTA = 6 BOS ERROR KINE gam3# OF REJECT = ',I10,
     &        /10X,'ISTA = 7 BOS ERROR KHIS     # OF REJECT = ',I10)
C
      CALL HPRINT(0)
      CALL HDELET(0)
C
      RETURN
      END
      SUBROUTINE GGGB01(LENTRY)
C --------------------------------------------------------------------
C G. Bonneaud October 1988.
C --------------------------------------------------------------------
C **********************************************************************
C   GAMMA GAMMA (GAMMA) GENERATOR DRIVER
C     (FROM BERENDS & KLEISS WITH SOME MODIFICATIONS FOR
C                 CUTTING EVENTS )
C
C                                   M.MARTINEZ    DESY-1985
C **********************************************************************
      COMMON / DTMILL / SDVRT(3),TABL(12),NEVENT(10),NEVPHO(2)
      COMMON / INPOUT / IOUT
      COMMON /KGCOMM/ ISTA,NTRK,IDPR,ECMS,WEIT
      COMMON / QUADRI / Q1(4),Q2(4),Q3(4)
      COMMON / GGDATA / EB,RHO,X1,A1MIN,A1MAX,AVETO,I3,IODD,ICONF
      COMMON / OUTVEC / QK1(4),QK2(4),QK3(4),EBE
      COMMON / CON2 / K0,RHOO,THRSLD
      COMMON / TRIGAM / I33
      REAL*8 EB,EBE,RHO,RHOO,K0,THRSLD,X1,A1MIN,A1MAX,AVETO
      REAL*8 Z,QK1,QK2,QK3
C
      DATA CONV/57.2958/
C
C  INITIALIZATION            *********************
C
      IF(LENTRY.EQ.1) THEN
C
C  Parameters initialization
C
       EB    = TABL(1)
       RHO   = TABL(2)
       I3    = TABL(3)
       IODD  = TABL(4)
       ICONF = TABL(5)
       X1    = TABL(6)
       A1MIN = TABL(7)
       A1MAX = TABL(8)
       AVETO = TABL(9)
       WRITE(IOUT,5)
  5   FORMAT(///10X,' E+ E- ---->  GAMMA  GAMMA (GAMMA) '///)
C
       EBE  = EB
       RHOO = RHO
       I33  = I3
       CALL START(EB,IODD)
C
       RETURN
      ENDIF
C
C  EVENT GENERATION          *********************
C
      IF(LENTRY.EQ.2) THEN
C
C  EVENT STATUS (0 = O.K.)
C
       ISTA = 0
C
C  Initialize the track number (radiated photon included)
C
       NTRK = 3
C
       CALL EVENT
C
       DO 10 I = 1,4
        Q1(I) = QK1(I)*EB
        Q2(I) = QK2(I)*EB
        Q3(I) = QK3(I)*EB
 10    CONTINUE
C
       CG1=0.
       IF(QK1(4).EQ.0.)GO TO 20
       CG1=REAL(QK1(3)/QK1(4)   )
       CALL HFILL(10002,CG1,0.,1.)
       CALL HFILL(10003,REAL(QK1(4)),0.,1.)
 20    CG2=0.
       IF(QK2(4).EQ.0.)GO TO 30
       CG2=REAL(QK2(3)/QK2(4))
       CALL HFILL(10004,CG2,0.,1.)
       CALL HFILL(10005,REAL(QK2(4)),0.,1.)
 30    CG3=0.
       IF(QK3(4).EQ.0.)GO TO 40
       CG3=REAL(QK3(3)/QK3(4))
       CALL HFILL(10006,CG3,0.,1.)
       CALL HFILL(10007,REAL(QK3(4)),0.,1.)
 40    IF(QK1(4).EQ.0..OR.QK2(4).EQ.0.)GO TO 50
       CALL HFILL(10008,CG1,CG2,1.)
       CALL HFILL(10009,REAL(QK1(4)),REAL(QK2(4)),1.)
 50    IF(QK1(4).EQ.0.OR.QK2(4).EQ.0.)GO TO 60
       Z=2.*(1.-QK3(4))/(QK1(4)*QK2(4))-1.
       IF(Z.LT.-1.)Z=-1.
       IF(Z.GT.1.)Z=1.
       Z=CONV*DACOS(Z)
       CALL HFILL(10010,REAL(Z),0.,1.)
       Z=-(QK1(1)*QK2(1)+QK1(2)*QK2(2))/
     . DSQRT((QK1(1)**2+QK1(2)**2)*(QK2(1)**2+QK2(2)**2))
       IF(Z.LT.-1.)Z=-1.
       IF(Z.GT.1.)Z=1.
       Z=CONV*DACOS(Z)
       CALL HFILL(10011,REAL(Z),0.,1.)
  60   CONTINUE
C
       IDPR = 100*I3 + 10*ICONF
       WEIT = 1.
       ECMS = 2.*EB
C
       RETURN
      ENDIF
C
C  END OF GENERATION         *********************
C
      IF(LENTRY.EQ.3) THEN
C
       CALL FINISH
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE UGTSEC
C------------------------
C     B. Bloch create the X-section bank KSEC   Jan 2000
C--------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      real*4 XTOT,XACC,RTOT,RACC
      COMMON / TRY / TTOT,IEV,ITRY,TTOT2,IPS
      COMMON / SIGTOT / SIGCOL
      PARAMETER ( IGCO = 1006)
C
      STESM=TTOT/ITRY
      ERRTES=DSQRT((TTOT2/ITRY-STESM**2)/ITRY)
      SIGOBS=STESM *SIGCOL
      ERRSIG=ERRTES *SIGCOL
C
      IS = 1
      IDC = IGCO
      IVER = 100
      NTOT = IEV
      NACC = NTOT
      XTOT = SIGOBS
      XACC = XTOT
      RTOT = ERRSIG
      RACC = RTOT
      ISEC =  KSECBK(IS,IDC,IVER,NTOT,NACC,XTOT,RTOT,XACC,RACC)
      call prtabl('KSEC',0)
      return
      end
