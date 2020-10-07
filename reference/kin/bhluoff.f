C*HE 05/27/92 10:42:58 C
C*DK ASKUSI
      SUBROUTINE ASKUSI(IGCOD)
C --------------------------------------------------------------------
C Initialization for BHLU01                G. Bonneaud November 1988.
C Modified for BHLU02                   Bolek Pietrzyk January  1992.
C --------------------------------------------------------------------
C*CA BCS
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA CDGENE
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
C*CC CDGENE
C*CA INOUT
      COMMON / INOUT  / NINP,NOUT,NOUT2
C*CC INOUT
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL ,ALRLEP
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT
      REAL *8 CMSENE,THMIN,THMAX,EPSCM
      DIMENSION TABL(15)
      PARAMETER ( IGCO = 2009)
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      NOUT  = IW(6)
      WRITE(NOUT,101) IGCOD
 101  FORMAT(/,10X,
     &       'BHLU02 - CODE NUMBER =',I4,' Last mod. September 18,1992',
     & /,10X,'**************************************************',//)
C
C   Input parameters (see description in BHLU02)
C
      CMSENE = 92.0
      THMIN  = 2.177
      THMAX  = 10.31
      EPSCM  = 0.0001
      KEYRAD = 1
      KEYOPT = 3001
      WTMAX = 2.8
C
C  The default values can be changed by the DATA CARD GBHL
C
      JGENE = NLINK('GBHL',0)
      IF(JGENE.NE.0) THEN
       CMSENE = RW(JGENE+1)
       THMIN  = RW(JGENE+2)
       THMAX  = RW(JGENE+3)
       EPSCM  = RW(JGENE+4)
       KEYRAD = IW(JGENE+5)
       KEYOPT = IW(JGENE+6)
       IF ( IW(JGENE).GT.6)  WTMAX  = RW(JGENE+7)
      ENDIF
      TABL(1) = CMSENE
      TABL(2) = THMIN
      TABL(3) = THMAX
      TABL(4) = EPSCM
      TABL(5) = KEYRAD
      TABL(6) = KEYOPT
C
C  Main vertex generation
C
      SDVRT(1) = 0.0180
      SDVRT(2) = 0.0010
      SDVRT(3) = 1.00
      VPOS(1)  = 0.
      VPOS(2)  = 0.
      VPOS(3)  = 0.
      VANG(1)  = 0.
      VANG(2)  = 0.
      JSVRT = NLINK('SVRT',0)
      IF(JSVRT.NE.0) THEN
       SDVRT(1) = RW(JSVRT+1)
       SDVRT(2) = RW(JSVRT+2)
       SDVRT(3) = RW(JSVRT+3)
      ENDIF
      JXVRT = NLINK('XVRT',0)
      IF(JXVRT.NE.0) THEN
       VPOS(1)  = RW(JXVRT+1)
       VPOS(2)  = RW(JXVRT+2)
       VPOS(3)  = RW(JXVRT+3)
      ENDIF
      JAVRT = NLINK('AVRT',0)
      IF(JAVRT.NE.0) THEN
       VANG(1)  = RW(JAVRT+1)
       VANG(2)  = RW(JAVRT+2)
      ENDIF
      TABL(7) = SDVRT(1)
      TABL(8) = SDVRT(2)
      TABL(9) = SDVRT(3)
      TABL(10) = VPOS(1)
      TABL(11)= VPOS(2)
      TABL(12)= VPOS(3)
      TABL(13)= WTMAX
      TABL(14)= VANG(1)
      TABL(15)= VANG(2)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL = 15
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
C  Fill RLEP bank
       EB = 0.5*CMSENE
       IEBEAM = NINT(EB *1000  )
       JRLEP = ALRLEP(IEBEAM,'    ',0,0,0)
C
C  Initialize events counters
      DO 10 I = 1,12
   10 NEVENT(I) = 0
      DO 11 I = 1,2
   11 NEVPHO(I) = 0
C
C Booking of some standard histogrammes
C
      CALL HBOOK1(10001,'Energy distribution : final e+$',
     &                                            30,0.,60.,0.)
      CALL HIDOPT(10001,'LOGY')
      CALL HBOOK1(10002,'Polar angle : final e+ (Degrees)$',
     &                                                   40,0.,180.,0.)
      CALL HIDOPT(10002,'LOGY')
      CALL HBOOK1(10003,'Energy distribution : final e-$',
     &                                            30,0.,60.,0.)
      CALL HIDOPT(10003,'LOGY')
      CALL HBOOK1(10004,'Polar angle : final e- (Degrees)$',
     &                                                   40,0.,180.,0.)
      CALL HIDOPT(10004,'LOGY')
      CALL HBOOK1(10005,'Photons multiplicity$',40,0.,40.,0.)
      CALL HBOOK1(10006,'Energy distribution : photons$',30,0.,60.,0.)
      CALL HIDOPT(10006,'LOGY')
      CALL HBOOK1(10007,'Average energy distribution : photons$',
     &                                                     30,0.,60.,0.)
      CALL HIDOPT(10007,'LOGY')
      CALL HBOOK1(10008,'Accolinearity between e+ and e-$',
     &                                                   40,0.,10.,0.)
      CALL HIDOPT(10008,'LOGY')
      CALL HBOOK1(10009,'Accoplanarity between e+ and e-$',
     &                                                   40,0.,40.,0.)
      CALL HIDOPT(10009,'LOGY')
      CALL HBOOK1(10010,'Evisible (Ee+ + Ee-) (GeV)$',40,0.,100.,0.)
      CALL HIDOPT(10010,'LOGY')
      CALL HBOOK1(10053,'Theta each particle (GeV)$',50,0.,10.,0.)
      CALL HIDOPT(10053,'LOGY')
      CALL HBOOK1(10054,'Hemisphere energy (GeV)$',50,0.,50.,0.)
      CALL HIDOPT(10054,'LOGY')
      CALL HBOOK2(10055,'EA vs EB (GeV)$',
     &  50,0.,50.,50,0.,50.,0.)
C
C  Generator initialization
C
      LENTRY = 1
      CALL BHLU02(LENTRY)
C
C  Print PART and KLIN banks
C
CC-RM      CALL PRPART
      CALL PRTABL('KPAR',0)
      CALL PRTABL('RLEP',0)
C
      RETURN
      END
C*DK ASKUSE
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C --------------------------------------------------------------------
C Generation for BHLU01                    G. Bonneaud November 1988.
C Modified for BHLU02                   Bolek Pietrzyk January  1992.
C --------------------------------------------------------------------
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      REAL*8 P1,P2,Q1,Q2,PHOT
      REAL*8 QAUX(4)
      DIMENSION VRTEX(4),TABK(4),ITAB(3)
      INTEGER ALTABL
      LOGICAL REJFL,ACCFL
      EXTERNAL ALTABL
C
C  Generate primary vertex
C
      CALL RANNOR (RN1,RN2)
      CALL RANNOR (RN3,DUM)
      VRTEX(1) = RN1*SDVRT(1) + VPOS(1)
      VRTEX(2) = RN2*SDVRT(2) + VPOS(2)
      VRTEX(3) = RN3*SDVRT(3) + VPOS(3)
      VRTEX(4) = 0.
C
C  Fill 'VERT' bank
C
      IVMAI = 1
      JVERT = KBVERT(IVMAI,VRTEX,0)
      IF(JVERT.EQ.0) THEN
        ISTA = 1
        NEVENT(2) = NEVENT(2) + 1
        GO TO 98
      ENDIF
C
C  Event generation
C
      LENTRY = 2
    1 NEVENT(1) = NEVENT(1) + 1
      CALL BHLU02(LENTRY)
CC-RM
C Perform rotation from beam to Aleph (or rather SiCAL) system,
C that is, transform coordinates from the first system to the second
C
      CALL RBTOA (P1)
      CALL RBTOA (Q1)
      CALL RBTOA (P2)
      CALL RBTOA (Q2)
      IF ( NPHOT.GT.0) THEN
      DO 44 I=1,NPHOT
        DO 45 J=1,4
 45     QAUX(J) = PHOT(I,J)
        CALL RBTOA(QAUX)
        DO 46 J=1,4
 46     PHOT(I,J) = QAUX(J)
 44   CONTINUE
      ENDIF
CC-RM
C
C  Reject events manifestly outside acceptance
C
      CALL REJEVT(P2,Q2,PHOT,NPHOT,VRTEX,REJFL)
      IF(REJFL) GO TO 1
C
      NVRT = 1
      ISTA = IST
      IDPR = IDP
      ECMS = ECM
      WEIT = WEI
C
C  book 'KINE' for beam electrons (-1 and -2)
C
      TABK(1) = Q1(1)
      TABK(2) = Q1(2)
      TABK(3) = Q1(3)
      TABK(4) = 0.
      JKINE = KBKINE(-1,TABK,2,0)
      IF(JKINE.EQ.0) THEN
        ISTA = 2
        NEVENT(3) = NEVENT(3) + 1
        GO TO 98
      ENDIF
      TABK(1) = P1(1)
      TABK(2) = P1(2)
      TABK(3) = P1(3)
      TABK(4) = 0.
      JKINE = KBKINE(-2,TABK,3,0)
      IF(JKINE.EQ.0) THEN
        ISTA = 3
        NEVENT(4) = NEVENT(4) + 1
        GO TO 98
      ENDIF
C  book 'KINE' for final state particles
      TABK(4) = 0.
      DO 92 I = 1,3
        TABK(I) = Q2(I)
   92 CONTINUE
      JKINE = KBKINE(1,TABK,2,IVMAI)
      IF(JKINE.EQ.0) THEN
        ISTA = 4
        NEVENT(5) = NEVENT(5) + 1
        GO TO 98
      ENDIF
      TABK(4) = 0.
      DO 93 I = 1,3
        TABK(I) = P2(I)
   93 CONTINUE
      JKINE = KBKINE(2,TABK,3,IVMAI)
      IF(JKINE.EQ.0) THEN
        ISTA = 5
        NEVENT(6) = NEVENT(6) + 1
        GO TO 98
      ENDIF
C
C Now we book the photons (if any...)
C
      NTR = 2 + NPHOT
      IF(NPHOT.EQ.0) GO TO 96
      JJ = 0
      DO 95 J = 1,NPHOT
C We did not book the radiated photon if the energy is equal to zero
        IF(PHOT(J,4).LT.1.E-06) THEN
          NTR = NTR - 1
          GO TO 95
        ENDIF
        TABK(4) = 0.
        DO 94 I = 1,3
          TABK(I) = PHOT(J,I)
   94   CONTINUE
        JJ = JJ + 1
        JKINE = KBKINE(2+JJ,TABK,1,IVMAI)
        IF(JKINE.EQ.0) THEN
          ISTA = 6
          NEVENT(7) = NEVENT(7) + 1
          GO TO 98
        ENDIF
   95 CONTINUE
C
C  Fill history with 'KHIS' bank
C
   96 DO 97 I = 1,NTR
   97 ITAB(I) = 0
      JKHIS = ALTABL('KHIS',1,NTR,ITAB,'I','E')
      IF(JKHIS.EQ.0) THEN
        ISTA = 7
        NEVENT(8) = NEVENT(8) + 1
      ENDIF
C
   98 IF(ISTA.NE.0) NEVENT(9) = NEVENT(9) + 1
      IF(ISTA.EQ.0) THEN
        NEVENT(10) = NEVENT(10) + 1
        IF(NTR.EQ.2) NEVPHO(1) = NEVPHO(1) +1
        IF(NTR.NE.2) NEVPHO(2) = NEVPHO(2) +1
        CALL HFILL(10001,REAL(Q2(4)),0.,WEIT)
        QPTHET = Q2(3)/DSQRT(Q2(1)**2+Q2(2)**2+Q2(3)**2)
        QPTHET = ACOS(QPTHET)*180./3.14159
        CALL HFILL(10002,QPTHET,0.,WEIT)
        CALL HFILL(10003,REAL(P2(4)),0.,WEIT)
        QMTHET = P2(3)/DSQRT(P2(1)**2+P2(2)**2+P2(3)**2)
        QMTHET = ACOS(QMTHET)*180./3.14159
        CALL HFILL(10004,QMTHET,0.,WEIT)
        XNPHOT = NPHOT
        CALL HFILL(10005,XNPHOT,0.,WEIT)
        IF(NPHOT.NE.0) THEN
          EAVE = 0.
          DO 99 I = 1,NPHOT
            EGAM = PHOT(I,4)
            EAVE = EAVE + EGAM
   99     CALL HFILL(10006,EGAM,0.,WEIT)
          EAVE = EAVE / NPHOT
          CALL HFILL(10007,EAVE,0.,WEIT)
        ENDIF
        ACCOL = P2(1)*Q2(1)+P2(2)*Q2(2)+P2(3)*Q2(3)
        ACCOL = ACCOL / (DSQRT(P2(1)**2+P2(2)**2+P2(3)**2) *
     &                  DSQRT(Q2(1)**2+Q2(2)**2+Q2(3)**2))
        ACCOL = ACOS(ACCOL)*180./3.14159
        ACCOL = 180. - ACCOL
        CALL HFILL(10008,ACCOL,0.,WEIT)
        ACCOP = P2(1)*Q2(1)+P2(2)*Q2(2)
        ACCOP = ACCOP / (DSQRT(P2(1)**2+P2(2)**2) *
     &                  DSQRT(Q2(1)**2+Q2(2)**2))
        ACCOP = ACOS(ACCOP)*180./3.14159
        ACCOP = 180. - ACCOP
        CALL HFILL(10009,ACCOP,0.,WEIT)
        EVISEE = P2(4) + Q2(4)
        CALL HFILL(10010,EVISEE,0.,WEIT)
      ENDIF
C
      NTRK = NTR
C
      RETURN
      END
C*DK USCJOB
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C End of generation for BHLU01             G. Bonneaud November 1988.
C Modified for BHLU02                   Bolek Pietrzyk January  1992.
C --------------------------------------------------------------------
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      COMMON / INOUT  / NINP,NOUT,NOUT2
C
      LENTRY = 3
      CALL BHLU02(LENTRY)
C
      WRITE(NOUT,101)
  101 FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
      WRITE(NOUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)
  102 FORMAT(/5X,'# OF GENERATED UNWEIGHTED EVENTS     = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS LOOSE ACCEPT.  = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITHOUT PHOTON = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITH PHOTON(S) = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (BOS ERROR)    = ',I10)
      WRITE(NOUT,103)
  103 FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
      WRITE(NOUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6),
     &               NEVENT(7),NEVENT(8)
  104 FORMAT(/10X,'ISTA = 1 BOS ERROR VERT     # OF REJECT = ',I10,
     &        /10X,'ISTA = 2 BOS ERROR KINE e+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 3 BOS ERROR KINE e-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 4 BOS ERROR KINE f+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 5 BOS ERROR KINE f-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 6 BOS ERROR KINE gam # OF REJECT = ',I10,
     &        /10X,'ISTA = 7 BOS ERROR KHIS     # OF REJECT = ',I10)
C
      RETURN
      END
C*DK BHLU02
      SUBROUTINE BHLU02(LENTRY)
C --------------------------------------------------------------------
C Interface for BHLU01                     G. Bonneaud November 1988.
C Modified for BHLU02                   Bolek Pietrzyk January  1992.
C --------------------------------------------------------------------
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI
      PARAMETER( PI = 3.1415926535897932D0 )
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      REAL *8 P1,P2,Q1,Q2,PHOT
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT
      REAL *8 CMSENE,THMIN,THMAX,EPSCM
      REAL *8 TRMIN,TRMAX
      REAL *8  XPAR(100)
      INTEGER NPAR(100)
C
C  INITIALIZATION            *********************
C
      IF(LENTRY.EQ.1) THEN
C
C  Parameters initialization
C
C
        TRMIN = CMSENE**2*(1D0-COS(THMIN*PI/180D0))/2D0
        TRMAX = CMSENE**2*(1D0-COS(THMAX*PI/180D0))/2D0
        XPAR(1)=CMSENE
        XPAR(2)=TRMIN
        XPAR(3)=TRMAX
        XPAR(4)=EPSCM
        XPAR(7)=WTMAX
        NPAR(1)=KEYOPT
        NPAR(2)=KEYRAD
C
        CALL BHLUMI(-1,XPAR,NPAR)
        CALL BOKER8(-1)
C
C
C  EVENT GENERATION          *********************
C
      ELSE IF(LENTRY.EQ.2) THEN
C
C  Event status (0 = O.K.)
C
        IST = 0
C
        CALL BHLUMI(0,XPAR,NPAR)
        CALL BOKER8(0)
C
        IDP  = NPHOT
        WEI  = 1.
        ECM  = CMSENE
C
C  END OF GENERATION         *********************
C
      ELSE IF(LENTRY.EQ.3) THEN
C
        CALL BHLUMI(2,XPAR,NPAR)
        CALL BOKER8(1)
C
      ENDIF
C
      RETURN
      END
C*DK REJEVT
      SUBROUTINE REJEVT(QM,QP,QK,NPHOT,VRTEX,REJFL)
C----------------------------------------------------------------------
C!  - Make a preselection on generator level
C!
C!
C!   Inputs: QM,QP,QK fourvectors of electron, positron and photon
C!           NPHOT    number of photons
C!           VRTEX(4) vertex
C!   Output: REJFL = .TRUE. if rejected
C!                 = .FALSE. if accepted
C!   DESCRIPTION
C!   ===========
C?   The purpose of this is to avoid waisting GALEPH time on
C?   events that do not have particles in the acceptance.
C?
C?   Check whether a particle on each side has hit the active
C?   region defined as theta = THMIBK (the last plane)
C?   extended by a margin of THXTRA. If so REJFL = .FALSE.
C?
C!======================================================================
      PARAMETER (PI = 3.141592654)
C theta of last plane
      PARAMETER (THMIBK = 1.33)
C theta of first tungsten
      PARAMETER (THMAFR = 3.356)
C safety margin
      PARAMETER (THXTRA = 0.13)
C theta min for enlarged acceptance
      PARAMETER (THMIAC = (THMIBK - THXTRA) * PI /180.)
C theta max for enlarged acceptance
      PARAMETER (THMAAC = (THMAFR + THXTRA) * PI /180.)
C zero suppression
      PARAMETER (EZERO = 0.050)
C energy cut
      PARAMETER (ECUT  = 0.500)
C depth at which the fiducial cut is done (cm)
cc      PARAMETER (Z0 = 252.78)
      PARAMETER (Z0 = 252.83)
C
C
      REAL*8 QM(4),QP(4),QK(100,4)
      REAL*4 QA(4,102),VRTEX(4)
      LOGICAL REJFL
C
C-----------------------------------------------------------------------
      REJFL = .FALSE.
C
C electron, positron and gamma
      DO 10 I = 1,4
        QA(I,1)=QM(I)
        QA(I,2)=QP(I)
        DO 10 J = 1,NPHOT
          QA(I,J+2)=QK(J,I)
   10 CONTINUE
C
C loop over all partons
      EA = 0.
      EB = 0.
      NPART = 2 + NPHOT
      DO 100 J = 1,NPART
        PT2   = QA(1,J)*QA(1,J)+QA(2,J)*QA(2,J)
        PTOT  = SQRT(PT2+QA(3,J)*QA(3,J))
        PT    = SQRT(PT2)
        THETA = ASIN(PT/PTOT)
CC-RM
CC        PHI = ATAN2(QA(2,J),QA(1,J))
CC        IF(PHI.LT.0.) PHI=PHI+2.*PI
CC        THETA = THETA*(1.-VRTEX(3)/Z0) + VRTEX(1)/Z0*COS(PHI)
CC     .                                 + VRTEX(2)/Z0*SIN(PHI)
CC        IF ( QA(3,J) .LT. 0.) THETA = THETA + 2.*VRTEX(3)*Z0
CC-RM
        IF ( QA(3,J) .LT. 0.) THETA = PI-THETA
        IF (THETA .LT. 1.) CALL HF1(10053,THETA*180./PI,QA(4,J))
C
        IF ( QA(4,J) .LT. EZERO ) GOTO 100
C
        IF     ( (THETA .GT. THMIAC)
     &                  .AND.
     &           (THETA .LT. THMAAC) )      THEN
          EA = EA + QA(4,J)
        ELSEIF ( (THETA .LT. (PI-THMIAC))
     &                  .AND.
     &           (THETA .GT. (PI-THMAAC)) ) THEN
          EB = EB + QA(4,J)
        ENDIF
C
  100 CONTINUE
cc-rm      IF ( (EA .LT. ECUT) .AND. (EB .LT. ECUT) )  REJFL = .TRUE.
      IF ( (EA .LT. ECUT) .OR. (EB .LT. ECUT) )  REJFL = .TRUE.
      CALL HF1(10054,EA,1.)
      CALL HF2(10055,EA,EB,1.)
C
  999 CONTINUE
      END
C*DK BOKER8
      SUBROUTINE BOKER8(MODE)
C*****************************************************************
C Calulate cross-section with UNWEIGHTED events within geometrical
C acceptance of ALEPH
C                                  Bolek Pietrzyk January 1992
C*****************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      CHARACTER*80      BXOPE,BXCLO,BXTXT,BXL1I,BXL1F,BXL2F,BXL1G,BXL2G
      PARAMETER(
     $BXOPE =  '(//1X,15(5H=====)    )',
     $BXTXT =  '(1X,1H=,                  A48,25X,    1H=)',
     $BXL1I =  '(1X,1H=,I17,                 16X, A20,A12,A7, 1X,1H=)',
     $BXL1F =  '(1X,1H=,F17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2F =  '(1X,1H=,F17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXL1G =  '(1X,1H=,G17.8,               16X, A20,A12,A7, 1X,1H=)',
     $BXL2G =  '(1X,1H=,G17.8, 4H  +-, F11.8, 1X, A20,A12,A7, 1X,1H=)',
     $BXCLO =  '(1X,15(5H=====)/   )'    )
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(100)
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
C THIS IS COMMON BLOCK FROM INSIDE GENERATOR !!!!!!!
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
C !!!!!!!
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
      DIMENSION NACC(4)
cc-rm
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      parameter (conv=pi/180.)
cc-rm
      IF(MODE.EQ.-1) THEN
*     *******************
cc-rm
        nback  = 0
        nbackp = 0
        nbackt = 0
        trmist = 91.284d0**2 * (1.d0 - cos(0.6d0*pi/180.d0)) / 2.d0
        trmast = 91.284d0**2 * (1.d0 - cos(4.1d0*pi/180.d0)) / 2.d0
cc-rm
 
        XMIN= 0.1
C.....initialize scalers
        DO 10 K = 1,4
   10   NACC(K) = 0
        NEVGEN  = 0
      ELSEIF(MODE.EQ.0) THEN
*     **********************
        NEVGEN=NEVGEN+1
        LWIDE  = .FALSE.
        LNARR  = .FALSE.
        LMIX1  = .FALSE.
        LMIX2  = .FALSE.
C Three triggers TRIGA2 = ALEPH
        CALL TRIGA2(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
        LWIDE= XWIDE.GT.XMIN
        LNARR= XNARR.GT.XMIN
        LMIX1= XMIX1.GT.XMIN
        LMIX2= XMIX2.GT.XMIN
C BHLUM2 total... with vac_pol and Z
        IF(LWIDE) NACC(1) = NACC(1) + 1
        IF(LNARR) NACC(2) = NACC(2) + 1
        IF(LMIX1) NACC(3) = NACC(3) + 1
        IF(LMIX2) NACC(4) = NACC(4) + 1
cc-rm
        if (   (lmix1.or.lmix2) .and.
     .      ( (abs(p2(3)/p2(4)).lt.cos(conv*4.1)) .or.
     .        (abs(p2(3)/p2(4)).gt.cos(conv*0.6)) .or.
     .        (abs(q2(3)/q2(4)).lt.cos(conv*4.1)) .or.
     .        (abs(q2(3)/q2(4)).gt.cos(conv*0.6)) ) ) nback = nback + 1
        if (   (lmix1.or.lmix2) .and.
     .      (tran.lt.trmist .or. tran.gt.trmast)  ) nbackp = nbackp + 1
        if  (tran.lt.trmist .or. tran.gt.trmast)  nbackt = nbackt + 1
cc-rm
      ELSEIF(MODE.EQ.1) THEN
*     ***********************
cc-rm
        write (6,*) 'nback = ', nback
        write (6,*) 'nbackp= ', nbackp
        write (6,*) 'nbackt= ', nbackt
cc-rm
        WRITE(NOUT,BXOPE)
        WRITE(NOUT,BXTXT) '============ BOKER8 ============='
        WRITE(NOUT,BXTXT) '      BHLUM2 related tests       '
        CALL BHLUMI(   1,XPAR,NPAR)
        NEVT  = NPAR(20)
        XCRU  = XPAR(20)
        WRITE(NOUT,BXTXT) '*********************************'
        WRITE(NOUT,BXTXT) 'Integrated x-sect. total best    '
        WRITE(NOUT,BXTXT) '(with vac., with Z, with s-chan.)'
        WRITE(NOUT,BXTXT) 'UNWEIGHTED EVENTS                '
        WRITE(NOUT,BXTXT) '*********************************'
        WRITE(NOUT,BXL1I) NEVGEN,     'generated events   ','NEVGEN',
     &    '  '
        WRITE(NOUT,BXL1I) NACC(1),    'wide-wide accepted ','NACC1 ',
     &    '  '
        WRITE(NOUT,BXL1I) NACC(2),    'narrow-narrow acc. ','NACC2 ',
     &    '  '
        WRITE(NOUT,BXL1I) NACC(3),    'narrow-wide accept.','NACC3 ',
     &    '  '
        WRITE(NOUT,BXL1I) NACC(4),    'wide-narrow accept.','NACC4 ',
     &    '  '
        XSMCNB = XPAR(20)
        ERMC   = XSMCNB*XPAR(21)
        FACC1 = FLOAT(NACC(1))
        FACC2 = FLOAT(NACC(2))
        FACC3 = FLOAT(NACC(3))
        FACC4 = FLOAT(NACC(4))
        FACCNW= (FACC3+FACC4)/2D0
        FEVGEN= FLOAT(NEVGEN)
        XWW    = XSMCNB*FACC1/FEVGEN
        XNN    = XSMCNB*FACC2/FEVGEN
        XNW    = XSMCNB*FACCNW/FEVGEN
cc        DXWW   = SQRT((FACC1/FEVGEN)**2*ERMC**2+
cc     &    (XSMCNB/FEVGEN)**2*FACC1+(XSMCNB*FACC1/FEVGEN**2)**2*FEVGEN)
cc        DXNN   = SQRT((FACC2/FEVGEN)**2*ERMC**2+
cc     &    (XSMCNB/FEVGEN)**2*FACC2+(XSMCNB*FACC2/FEVGEN**2)**2*FEVGEN)
cc        DXNW   = SQRT((FACCNW/FEVGEN)**2*ERMC**2+
cc     &    (XSMCNB/FEVGEN)**2*FACCNW/2D0+
cc     &    (XSMCNB*FACCNW/FEVGEN**2)**2*FEVGEN)
cc-rm
        DXWW   = SQRT((FACC1/FEVGEN)**2*ERMC**2+
     &    (XSMCNB/FEVGEN)**2*FACC1-(XSMCNB*FACC1/FEVGEN**2)**2*FEVGEN)
        DXNN   = SQRT((FACC2/FEVGEN)**2*ERMC**2+
     &    (XSMCNB/FEVGEN)**2*FACC2-(XSMCNB*FACC2/FEVGEN**2)**2*FEVGEN)
        DXNW   = SQRT((FACCNW/FEVGEN)**2*ERMC**2+
     &    (XSMCNB/FEVGEN)**2*FACCNW -
     &    (XSMCNB*FACCNW/FEVGEN**2)**2*FEVGEN)
cc-rm
        WRITE(NOUT,BXL2G) XWW,DXWW,   'wide-wide accepted ','XWW   ',
     &    '  '
        WRITE(NOUT,BXL2G) XNN,DXNN,   'narrow-narrow acc. ','XNN   ',
     &    '  '
        WRITE(NOUT,BXL2G) XNW,DXNW,   'narrow-wide accept.','XNW   ',
     &    '  '
C  type values of rundom generator on the end of gereration
        CALL RMARUT(I1OUT,N1OUT,N2OUT)
 1234   FORMAT(1X,'RANMAR at the end of generation',3I12)
        WRITE(NOUT,1234) I1OUT,N1OUT,N2OUT
      ENDIF
*     *****
      END
C*DK TRIGA2
      SUBROUTINE TRIGA2 (TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,
     $                  XMIX1,XMIX2)
C     **********************************************************
C MODIFIED TRIGA1 FOR ALEPH GEOMETRICAL CUTS
C Idealized exper. CALORIMETRIC trigger on dressed final electrons.
C Electrons and photons not distinguished!
C     ******************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
C Depth at which the fiducial cut is done (mm)
cc-This is the number that was used for the tests 
cc-   PARAMETER (Z0 = 2527.8)
      PARAMETER (Z0 = 2528.31)
C Cuts in energy in one side and sum of both sides
      PARAMETER (E1CUT = 0.44, E2CUT = 0.60)
      LOGICAL KEEPN,KEEPW
      LOGICAL LANGW,LANGN
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      DIMENSION PC(100,4)
      DIMENSION PCW1(4),PCN1(4)
      DIMENSION PCW2(4),PCN2(4)
      DATA ICONT /0/
C Beam energy
      ENE = P1(4)
C Final electrons and photons not distinguished
      DO K=1,4
        PC(1,K)=P2(K)
        PC(2,K)=Q2(K)
      ENDDO
      DO I=1,NPHOT
        DO K=1,4
          PC(2+I,K)=PHOT(I,K)
        ENDDO
      ENDDO
C
      DO K=1,4
        PCW1(K)=0D0
        PCN1(K)=0D0
        PCW2(K)=0D0
        PCN2(K)=0D0
      ENDDO
C
C Collecting energies in calorimeter sectors
C
      NP = NPHOT+2
      DO 100 I= 1,NP
CCC        THETA = ANGFI(ABS(PC(I,3)),DSQRT(PC(I,1)**2+PC(I,2)**2))
CCC        PHI   = ANGFI(PC(I,1),PC(I,2))
        X     = Z0*PC(I,1)/ABS(PC(I,3))
        Y     = Z0*PC(I,2)/ABS(PC(I,3))
        LANGN = .FALSE.
        LANGW = .FALSE.
        METHOD= 2
        IF( KEEPN(X,Y,METHOD) ) LANGN = .TRUE.
        IF( KEEPW(X,Y,METHOD) ) LANGW = .TRUE.
C
C wide/narrow sectors forward
        IF(PC(I,3) .GT. 0D0) THEN
          IF(LANGW) THEN
            DO 50 K=1,4
   50       PCW1(K)=PCW1(K)+ PC(I,K)
          ENDIF
          IF(LANGN) THEN
            DO 51 K=1,4
   51       PCN1(K)=PCN1(K)+ PC(I,K)
          ENDIF
        ELSE
C wide/narrow sectors backward
          IF(LANGW) THEN
            DO 70 K=1,4
   70       PCW2(K)=PCW2(K)+ PC(I,K)
          ENDIF
          IF(LANGN) THEN
            DO 71 K=1,4
   71       PCN2(K)=PCN2(K)+ PC(I,K)
          ENDIF
        ENDIF
  100 CONTINUE
C at least one coincidence in a pair of opposite calorimetric blocks
      XWIDE= 0D0
      XNARR= 0D0
      XMIX1= 0D0
      XMIX2= 0D0
      IF(PCW1(4)/ENE .GT. E1CUT  .AND.
     &   PCW2(4)/ENE .GT. E1CUT  .AND.
     &   (PCW1(4)+PCW2(4))/(2D0*ENE) .GT. E2CUT) XWIDE = PCW1(4)+PCW2(4)
      IF(PCN1(4)/ENE .GT. E1CUT  .AND.
     &   PCN2(4)/ENE .GT. E1CUT  .AND.
     &   (PCN1(4)+PCN2(4))/(2D0*ENE) .GT. E2CUT) XNARR = PCN1(4)+PCN2(4)
      IF(PCW1(4)/ENE .GT. E1CUT  .AND.
     &   PCN2(4)/ENE .GT. E1CUT  .AND.
     &   (PCW1(4)+PCN2(4))/(2D0*ENE) .GT. E2CUT) XMIX1 = PCW1(4)+PCN2(4)
      IF(PCN1(4)/ENE .GT. E1CUT  .AND.
     &   PCW2(4)/ENE .GT. E1CUT  .AND.
     &   (PCN1(4)+PCW2(4))/(2D0*ENE) .GT. E2CUT) XMIX2 = PCN1(4)+PCW2(4)
      END
C*DK KEEPN
      LOGICAL FUNCTION KEEPN(X,Y,METH)
C----------------------------------------------------------------------
C!  -
C!
C!   Inputs:Method number:  METH = (1,2) for the moment
C!          X,Y of particle at Z of fiducial cut
C!        -
C!
C?
      IMPLICIT REAL*8(A-H,O-Z)
C inner radius, radius of one pad, correction for asymmetry (mm)
      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)
C!======================================================================
      KEEPN = .FALSE.
C
      R   = SQRT(X**2+Y**2)
C
C min, max pad for fiducial cut
      IF     (METH.EQ.1) THEN
        PMIN = 2.
        PMAX = 12.
      ELSEIF (METH.EQ.2) THEN
        PMIN = 3.
        PMAX = 12.
      ENDIF
C min, max radius for fiducial cut
      RMIN = R0 + PMIN*RPAD - CORR
      RMAX = R0 + PMAX*RPAD - CORR
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN
        KEEPN = .TRUE.
      ENDIF
  999 RETURN
      END
C*DK KEEPW
      LOGICAL FUNCTION KEEPW(X,Y,METH)
C----------------------------------------------------------------------
C!  -
C!
C!   Inputs:Method number:  METH = (1,2) for the moment
C!          X,Y of particle at Z of fiducial cut
C!        -
C!
C?
      IMPLICIT REAL*8(A-H,O-Z)
C inner radius, radius of one pad
      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)
C!======================================================================
      KEEPW = .FALSE.
C
      R   = SQRT(X**2+Y**2)
C
C min, max pad for non-fiducial cut
      IF ( (METH.EQ.1) .OR. (METH.EQ.2) ) THEN
        PMIN = 1.
        PMAX = 15.
      ENDIF
C min, max radius for non-fiducial cut
      RMIN = R0 + PMIN*RPAD - CORR
      RMAX = R0 + PMAX*RPAD - CORR
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN
        KEEPW = .TRUE.
      ENDIF
  999 RETURN
      END
      SUBROUTINE RBTOA(P)
      REAL*8 P(4),Q(3)
C 92  PARAMETER(AX=-0.59E-3,AY=+0.20E-3)
C 93  PARAMETER(AX= 0.131E-3,AY=-0.266E-3)
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      AX = VANG(1)
      AY = VANG(2)
C
      DO 111 I = 1, 3
 111  Q(I) = P(I)
C
      P(1) =     Q(1)            + AX*Q(3)
      P(2) =               Q(2)  + AY*Q(3)
      P(3) = -AX*Q(1)  -AY*Q(2)  +    Q(3)
      END
CC-RM
