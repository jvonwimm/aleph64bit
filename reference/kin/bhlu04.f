      SUBROUTINE ASKUSI(IGCOD)
C --------------------------------------------------------------------
C Initialization for BHLUMI
C  BHLU03             December      1995.   B.Bloch
C --------------------------------------------------------------------
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL ,ALRLEP
      PARAMETER( PI = 3.1415926535897932D0 )
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT
      REAL *8 CMSENE,THMIN,THMAX,EPSCM
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      COMMON/ CDECUT /E1CUT,E2CUT
      real*4 E1CUT,E2CUT
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA,IPRI
      DIMENSION TABL(20)
      PARAMETER ( IGCO = 2013)
      REAL *8  XPAR(100)
      INTEGER NPAR(100)
C
C   Return the generator code as defined in the KINGAL library
C
      IGCOD = IGCO
      NOUT  = IW(6)
      WRITE(NOUT,101) IGCOD
 101  FORMAT(/,10X,
     &   'BHLU04 - CODE NUMBER =',I4,' Last mod. January 20, 2000',
     & /,10X,'**************************************************',//)
C
C   Input parameters (see description in BHLU03)
C
      CMSENE = 92.0D0
      THMIN  = 2.177D0
      THMAX  = 10.31D0
      EPSCM  = 0.0001D0
      KEYRAD = 1023
      KEYOPT = 3001
      WTMAX = 3.0
C
C  The default values can be changed by the DATA CARD GBHL
C
      JGBGO = NLINK('GBGO',0)
      IF(JGBGO.NE.0) THEN
       R0A = RW(JGBGO+1)
       R0B  = RW(JGBGO+2)
       CORMIN  = RW(JGBGO+3)
       CORMAX = RW(JGBGO+4)
       Z0A = RW(JGBGO+5)
       Z0B = RW(JGBGO+6)
       E1CUT  = RW(JGBGO+7)
       E2CUT  = RW(JGBGO+8)
      ENDIF
      ISICA = 0
      IPRI = -1
      JGENE = NLINK('GBHL',0)
      IF(JGENE.NE.0) THEN
       CMSENE = RW(JGENE+1)
       THMIN  = RW(JGENE+2)
       THMAX  = RW(JGENE+3)
       EPSCM  = RW(JGENE+4)
       KEYOPT = IW(JGENE+5)
       KEYRAD = IW(JGENE+6)
       IF ( IW(JGENE).GT.6)  WTMAX  = RW(JGENE+7)
       IF ( IW(JGENE).GT.7)  ISICA  = IW(JGENE+8)
       IF ( IW(JGENE).GT.8)  IPRI   = IW(JGENE+9)
      ENDIF
      TABL(1) = CMSENE
      TABL(2) = THMIN
      TABL(3) = THMAX
      TABL(4) = EPSCM
      TABL(5) = KEYRAD
      TABL(6) = KEYOPT
      TABL(7) = WTMAX
      TABL(8) = ISICA
      ECM = CMSENE
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
      TABL(9) = SDVRT(1)
      TABL(10) = SDVRT(2)
      TABL(11) = SDVRT(3)
      TABL(12) = VPOS(1)
      TABL(13)= VPOS(2)
      TABL(14)= VPOS(3)
      TABL(15)= VANG(1)
      TABL(16)= VANG(2)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL = 16
      NROW = 1
      JKPAR = ALTABL('KBH4',NCOL,NROW,TABL,'2I,(F)','C')
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
     &                50,0.,1.1*EB,0.)
      CALL HIDOPT(10001,'LOGY')
      CALL HBOOK1(10002,'Polar angle : final e+ (Degrees)$',
     &                                                   40,0.,180.,0.)
      CALL HIDOPT(10002,'LOGY')
      CALL HBOOK1(10003,'Energy distribution : final e-$',
     &                50,0.,1.1*EB,0.)
      CALL HIDOPT(10003,'LOGY')
      CALL HBOOK1(10004,'Polar angle : final e- (Degrees)$',
     &                                                   40,0.,180.,0.)
      CALL HIDOPT(10004,'LOGY')
      CALL HBOOK1(10005,'Photons multiplicity$',40,0.,40.,0.)
      CALL HBOOK1(10006,'Energy dist. of photons',50,0.,1.1*EB,0.)
      CALL HIDOPT(10006,'LOGY')
      CALL HBOOK1(10007,'Average energy distribution : photons$',
     &                50,0.,EB,0.)
      CALL HIDOPT(10007,'LOGY')
      CALL HBOOK1(10008,'Accolinearity between e+ and e-$',
     &                                                   40,0.,10.,0.)
      CALL HIDOPT(10008,'LOGY')
      CALL HBOOK1(10009,'Accoplanarity between e+ and e-$',
     &                                                   40,0.,40.,0.)
      CALL HIDOPT(10009,'LOGY')
      CALL HBOOK1(10010,'Evisible(Ee+ + Ee-) GeV',50,0.,2.2*EB,0.)
      CALL HIDOPT(10010,'LOGY')
      IF (ISICA.LE.0) THEN
        CALL HBOOK1(10051,'Dist - M8 except nonfid-cut$',50,-1.,4.,0.)
        CALL HBOOK2(10052,'E2 vs E1 - M8 except energy-cuts$',
     &  50,0.,50.,50,0.,50.,0.)
        CALL HBOOK1(10053,'Dphi - M8 except dphi-cut$',90,0.,180.,0.)
        CALL HBOOK1(10054,'Xmin - M8 except energy-cuts$',50,0.,1.,0.)
        CALL HBOOK1(10055,'Xsum - M8 except esum-cut$',50,0.,1.,0.)
        CALL HBOOK1(10056,'Theta - M8$',80,45.,125.,0.)
        CALL HBOOK1(10057,'Theta - M9$',80,45.,125.,0.)
        CALL HIDOPT(10053,'LOGY')
        CALL HIDOPT(10054,'LOGY')
        CALL HIDOPT(10055,'LOGY')
      ELSE
        CALL HBOOK1(10053,'Theta each particle (GeV)',50,0.,10.,0.)
        CALL HIDOPT(10053,'LOGY')
        CALL HBOOK1(10054,'Hemisphere energy (GeV)',50,0.,1.1*EB,0.)
        CALL HIDOPT(10054,'LOGY')
        CALL HBOOK2(10055,'EA vs EB (GeV)',
     &  50,0.,EB+5.,50,0.,EB+5.,0.)
      ENDIF
C
C  Generator initialization
C
      XPAR(1) = CMSENE
      XPAR(2) = CMSENE**2*(1D0-COS(THMIN*PI/180.D0))/2.D0
      XPAR(3) = CMSENE**2*(1D0-COS(THMAX*PI/180.D0))/2.D0
      XPAR(4) = EPSCM
      XPAR(7) = WTMAX
      NPAR(1) = KEYOPT
      NPAR(2) = KEYRAD
      call glimit(10000)
      CALL BHLUMI(-1,XPAR,NPAR)
      IF ( ISICA.LE.0) THEN
         CALL BOKER8(-1)
      ELSE
         CALL BOKERS(-1)
      ENDIF
C
C  Print PART and KLIN banks
C
CC-RM      CALL PRPART
      CALL PRTABL('KBH4',0)
      CALL PRTABL('RLEP',0)
      ncol = 8
      TABL(1) = R0A
      TABL(2) = R0B
      TABL(3) = CORMIN
      TABL(4) = CORMAX
      TABL(5) = Z0A
      TABL(6) = Z0B
      TABL(7) = E1CUT
      TABL(8) = E2CUT
      JKBGO = ALTABL('KBGO',NCOL,1,TABL,'2I,(F)','C')
      CALL PRTABL('KBGO',0)
C
      RETURN
      END
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C --------------------------------------------------------------------
C Generation for BHLU03
C  B. Bloch December 1995.
C --------------------------------------------------------------------
      PARAMETER (LBCS=1000,LCHAR=4)
      COMMON/BCS/ IW(LBCS)
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA,IPRI
      REAL *8  XPAR(100)
      INTEGER NPAR(100)
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      REAL*8 P1,P2,Q1,Q2,PHOT
C where P1 and Q1 are four-momenta of positron and electron beams.
C P2 and Q2 are four-momenta of outgoing positron and electron.
C The list PHOT(100,4) four-momenta contains
C NPHOT four-momenta of real the photons, all in GeV units.
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
    1 NEVENT(1) = NEVENT(1) + 1
      CALL BHLUMI(0,XPAR,NPAR)
      IF ( ISICA.LE.0) THEN
         CALL BOKER8(0)
      ELSE
         CALL BOKERS(0)
      ENDIF
      IDP  = NPHOT
      WEI  = 1.
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
      IF ( ISICA.LE.0) THEN
         CALL REJEVT(P2,Q2,PHOT,NPHOT,VRTEX,REJFL,ACCFL)
      ELSE
         CALL REJSEV(P2,Q2,PHOT,NPHOT,VRTEX,REJFL)
      ENDIF
      IF(REJFL) GO TO 1
C
      IF ( ISICA.LE.0) THEN
C  Count Method 8 events on parton level by REJECT routine
C
         IF(ACCFL) NEVENT(11) = NEVENT(11) + 1
      ENDIF
      NVRT = 1
      ISTA = IST
      IDPR = IDP
      ECMS = ECM
      WEIT = 1.
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
   94  CONTINUE
       JJ = JJ + 1
       JKINE = KBKINE(2+JJ,TABK,1,IVMAI)
       IF(JKINE.EQ.0) THEN
        ISTA = 6
        NEVENT(7) = NEVENT(7) + 1
        GO TO 98
       ENDIF
   95  CONTINUE
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
   99    CALL HFILL(10006,EGAM,0.,WEIT)
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
      if (ipri.gt.0) then
        IF ( ISICA.LE.0) THEN
          if ( mod(nevent(10),ipri).eq.0) call boker8(1)
        ELSE
          if ( mod(nevent(10),ipri).eq.0) call bokerS(1)
        ENDIF
      endif
      NTRK = NTR
C
      RETURN
      END
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C End of generation for BHLU03    B. Bloch December 1995
C                                 Modified Jan 200 call ugtsec
C
C --------------------------------------------------------------------
      COMMON  / CDGENE /SDVRT(3),VPOS(3),NEVENT(12),NEVPHO(2),VANG(2)
      COMMON / INOUT  / NINP,NOUT,NOUT2
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA,IPRI
      REAL *8  XPAR(100)
      INTEGER NPAR(100)
C
      CALL BHLUMI(2,XPAR,NPAR)
      IF ( ISICA.LE.0) THEN
        CALL BOKER8(2)
      ELSE
        CALL BOKERS(2)
      ENDIF
C
       WRITE(NOUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
      IF ( ISICA.LE.0) THEN
       WRITE(NOUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)
     &               ,NEVENT(11)
      ELSE
       WRITE(NOUT,102)NEVENT(1),NEVENT(10),NEVPHO(1),NEVPHO(2),NEVENT(9)
      ENDIF
  102  FORMAT(/5X,'# OF GENERATED UNWEIGHTED EVENTS     = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS LOOSE ACCEPT.  = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITHOUT PHOTON = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS WITH PHOTON(S) = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (BOS ERROR)    = ',I10,
     &        /5X,'# OF M8 ACCEPTED EVENTS              = ',I10)
       WRITE(NOUT,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(NOUT,104)NEVENT(2),NEVENT(3),NEVENT(4),NEVENT(5),NEVENT(6),
     &               NEVENT(7),NEVENT(8)
  104  FORMAT(/10X,'ISTA = 1 BOS ERROR VERT     # OF REJECT = ',I10,
     &        /10X,'ISTA = 2 BOS ERROR KINE e+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 3 BOS ERROR KINE e-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 4 BOS ERROR KINE f+  # OF REJECT = ',I10,
     &        /10X,'ISTA = 5 BOS ERROR KINE f-  # OF REJECT = ',I10,
     &        /10X,'ISTA = 6 BOS ERROR KINE gam # OF REJECT = ',I10,
     &        /10X,'ISTA = 7 BOS ERROR KHIS     # OF REJECT = ',I10)
C
      call ugtsec
      RETURN
      END
      subroutine ugtsec
C -----------------------------------------------------
C    create the xsection bank KSEC
C                                 B.Bloch jan 2000
C ------------------------------------------------------
      COMMON / KGCOMM / IST,NTR,IDP,ECM,WEI,ISICA,IPRI
      COMMON / BOKRL / NACCL(4)
      COMMON / BOKRS / NACCS(4)
      REAL*8 xpar(100),FACC3,FACC4,FEVGEN,FACCNW,XSMCNB,ERMC, XNW,DXNW
      INTEGER NPAR(100)
      PARAMETER ( IGCO = 2013,IVER= 404)

      call bhlumi(1,XPAR,NPAR)

C  NACC(3),    'narrow-wide accept.
C  NACC(4),    'wide-narrow accept.'
      IF ( ISICA.le.0) then
         FACC3 = FLOAT(NACCL(3))
         FACC4 = FLOAT(NACCL(4))
      ELSE
         FACC3 = FLOAT(NACCS(3))
         FACC4 = FLOAT(NACCS(4))
      endif
C!  NPAR(10)(20)  NEVGEN  Number of generated MC events
      NEVGEN = NPAR(10)
      FEVGEN = FLOAT(NEVGEN)
C!  XPAR(10)(20)   XMCNB  Total x-section [nb]
C!  XPAR(11)(21)    EREL  The relative error of XPAR(10)
      FACCNW = (FACC3+FACC4)/2D0
      XSMCNB = XPAR(10)
      ERMC   = XSMCNB*XPAR(11)
C     XNW,DXNW,   'narrow-wide accept.
      XNW    = XSMCNB*FACCNW/FEVGEN
      DXNW   = SQRT((FACCNW/FEVGEN)**2*ERMC**2+
     &    (XSMCNB/FEVGEN)**2*FACCNW -
     &    (XSMCNB*FACCNW/FEVGEN**2)**2*FEVGEN)

      IDC = IGCO
      IVERS = IVER
      IS = 1
      NTOT = NEVGEN
      NACC = FACCNW
      XTOT = XSMCNB
      RTOT = ERMC
      XACC = XNW
      RACC = DXNW
      ISEC =  KSECBK(IS,IDC,IVERS,NTOT,NACC,XTOT,RTOT,XACC,RACC)
      CALL PRTABL('KSEC',0)
      return
      end
      SUBROUTINE REJEVT(QM,QP,QK,NPHOT,VRTEX,REJFL,ACCFL)
C----------------------------------------------------------------------
C!  - Make a preselection on generator level
C!
C!   Author   :- Peter H. Hansen       27-NOV-1990  for BABAMC
C!   Modified :- Bolek Pietrzyk and Peter H. Hansen for BHLU02
C                                      17-JAN-1992
C              - B. Bloch fix some vartiable type in HF1
C!
C!   Inputs: QM,QP,QK fourvectors of electron, positron and photon
C!           NPHOT    number of photons
C!           VRTEX(4) vertex
C!   Output: REJFL = .TRUE. if rejected
C!                 = .FALSE. if accepted
C!           ACCFL = .TRUE. if Meth 8 accepted
C!                 = .FALSE. if Meth 8 rejected
C!   DESCRIPTION
C!   ===========
C?   The purpose of this is to avoid waisting GALEPH time on
C?   events that do not have particles in the acceptance.
C?
C?   Check whether a particle on each side has hit the active
C?   region defined as the edge at z=ZBACK (the last plane)
C?   extended by a margin of XEXTRA. If so REJFL = .FALSE.
C?   Here the alignment, vertex and B-field is taken into account.
C?
C?   Furthermore check whether the event passes Meth8.
C?   This is usefull for various studies of the acceptance at
C?   parton level.
C?
C!======================================================================
C Acceptance in horizontal plane (cm)
      PARAMETER (XEDG   = 11.9)
C y of "corner"
      PARAMETER (YEDG   = 8.4+1.2)
C z of last plane
      PARAMETER (ZBACK  = 302.0)
C z of shower max
      PARAMETER (ZAVER  = 280.0)
C z of origin of local coordinate system
      PARAMETER (ZORIG  = 262.5)
C margin allowing for transverse shower smear
      PARAMETER (XEXTRA = -.5)
      PARAMETER (YEXTRA = -.5)
C maximum theta of acceptance
      PARAMETER (TMAX = 0.170)
C separation when two clusters are formed
      PARAMETER (CLSEP = 15.0)
C phi rotation of 45.5 GeV particle in 1.5 T field
      PARAMETER (PHROT = 0.014)
C zero suppression
      PARAMETER (EZERO = 0.050)
C M8 effective dist cut on non-fiducial side
      PARAMETER (DICUT = 0.8)
C M8 maximal theta cut
      PARAMETER (THCUT = 0.125)
C M8 Dphi cut
      PARAMETER (DPCUT = 170.)
C M8 cluster energy cut
      PARAMETER (ECCUT = 0.22)
C M8 energy sum cut
      PARAMETER (ESCUT = 0.6)
C
C
      COMMON /INTBHL/CMSENE,THMIN,THMAX,EPSCM,WTMAX,KEYRAD,KEYOPT
      REAL *8 CMSENE,THMIN,THMAX,EPSCM
      DIMENSION DXN(4),DYN(4),DZN(4),OMXN(4),OMYN(4),OMZN(4)
      REAL*8 QM(4),QP(4),QK(100,4)
      REAL*4 QA(4,102),VRTEX(4)
      REAL *8 DXA,DXB,DYA,DYB
      DIMENSION DIST(2)
      LOGICAL REJFL,ACCFL,AFLAG,BFLAG,KEEPIT
C
      DATA PI/3.1415927/
      DATA IEVT/0/
C 1990 Alignment constants
C      DATA DXN/ -0.045, -0.045,  0.195,  0.195/
C      DATA DYN/ -0.093, -0.093, -0.047, -0.047/
C      DATA DZN/ -0.141, -0.170,  0.596,  0.387/
C      DATA OMXN/  0.00363, 0.00355,  0.00170, -0.00146/
C      DATA OMYN/ -0.00440, 0.00275,  0.00302,  0.00232/
C      DATA OMZN/  0.00235, 0.00235, -0.00272, -0.00272/
C 1991 Alignment constants
       DATA DXN/ -0.096, -0.118,  0.039,  0.027/
       DATA DYN/ -0.310, -0.012, -0.026, -0.100/
       DATA DZN/  0.134,  0.164,  0.317,  0.320/
       DATA OMXN/ -0.00019, 0.00007, -0.00309, -0.00353/
       DATA OMYN/ -0.00119, -0.00043, -0.00030, -0.00260/
       DATA OMZN/  0.01248, 0.01248, -0.00148, -0.00148/
C-----------------------------------------------------------------------
      REJFL = .FALSE.
      ACCFL = .FALSE.
      AFLAG = .FALSE.
      BFLAG = .FALSE.
      EA = 0.
      EB = 0.
      XA = 100.
      YA = 100.
      XB = 100.
      YB = 100.
C
C alternate kflg between 0 and 1 at each event
      IEVT = IEVT+1
      KFLG = MOD(IEVT,2)
C
C electron, positron and gamma
      DO 10 I=1,4
        QA(I,1)=QM(I)
        QA(I,2)=QP(I)
      DO 10 J = 1,NPHOT
        QA(I,J+2)=QK(J,I)
   10 CONTINUE
C
C loop over the 3 partons
      NPART = 2+NPHOT
      DO 100 J=1,NPART
        IF(QA(4,J).LT.EZERO) GOTO 100
        PT = QA(1,J)*QA(1,J)+QA(2,J)*QA(2,J)
        PTOT = SQRT(PT+QA(3,J)*QA(3,J))
        IF(PTOT.LT.EZERO) GOTO 100
        PT = SQRT(PT)
        THETA = ASIN(PT/PTOT)
        IF(QA(3,J).LT.0.) THETA = PI-THETA
        PHI = ATAN2(QA(2,J),QA(1,J))
        IF(PHI.LT.0.) PHI=PHI+2.*PI
C
C Sign z and module number
        IF(THETA.GT.1.) THEN
          SIG  =-1.
          MODU = 2
        ELSE
          SIG  = 1.
          MODU = 4
        ENDIF
        IF(COS(PHI).LT.0.) MODU=MODU-1
C
C Turn phi for electron and positron
        IF(J.LE.2) PHI = PHI - SIG*PHROT*CMSENE/(2.*PTOT)
C
C X and Y in global system
C
        X = VRTEX(1) + SIG*(ZAVER-SIG*VRTEX(3))*COS(PHI)*TAN(THETA)
        Y = VRTEX(2) + SIG*(ZAVER-SIG*VRTEX(3))*SIN(PHI)*TAN(THETA)
C
C Local system
        ZL = SIG*(ZAVER-ZORIG)
        XL = X - DXN(MODU) - OMZN(MODU)*Y + OMYN(MODU)*ZL
        YL = Y - DYN(MODU) - OMXN(MODU)*ZL + OMZN(MODU)*X
C
C Fold into first quadrant and extrapolate to last plane
        XLOC = ABS(XL)
        YLOC = ABS(YL)
C
C Find distance to edges AT THE LAST PLANE in Lcal
C (similarly to LCLUTW)
        XBACK = XLOC*ZBACK/ZAVER
        YBACK = YLOC*ZBACK/ZAVER
        DIST(1) = XBACK - 1.9
        DIST(2) = 24.5
        IF(YBACK.GT.YEDG) THEN
          DIS = XBACK-(17.5-YBACK)/0.75
          IF(DIS.LT.DIST(1)) DIST(1)=DIS
          IF(XBACK.LT.XEDG) DIST(2) = YBACK-(17.5-XBACK*0.75)
        ELSE
          DIST(1) = XBACK-XEDG
        ENDIF
C
C Sum up the energy in the active region (defined at last plane)
C find maximum energy clusters EA and EB
C two particles form separate clusters if more than 15cm apart.
        TH = THETA
        IF(THETA.GT.1.) TH = PI-THETA
        IF(DIST(1).GT.XEXTRA.AND.DIST(2).GT.YEXTRA.AND.
     &     TH.LT.TMAX.AND.QA(4,J).GT.EZERO) THEN
C
C energy deposit decreases linearly to zero when dist goes
C from 1.2 cm to zero
          EJ = QA(4,J)*AMIN1(DIST(1),1.2)/1.2
          EJ = AMAX1(EJ,0.)
C
C raise flag if any side is hit by a particle
C A side
          IF(SIG.GT.0.) THEN
            AFLAG = .TRUE.
            IF(ABS(X-XA).GT.CLSEP.OR.ABS(Y-YA).GT.CLSEP) THEN
              IF(EJ.GT.EA) THEN
                EA=EJ
                XA = XL
                YA = YL
              ENDIF
            ELSE
              XA = XA*EA+X*EJ
              YA = YA*EA+Y*EJ
              EA= EA+EJ
              XA = XA/EA
              YA = YA/EA
            ENDIF
C B side
          ELSE
            BFLAG = .TRUE.
            IF(ABS(X-XB).GT.CLSEP.OR.ABS(Y-YB).GT.CLSEP) THEN
              IF(EJ.GT.EB) THEN
                EB=EJ
                XB = XL
                YB = YL
              ENDIF
            ELSE
              XB = XB*EB+X*EJ
              YB = YB*EB+Y*EJ
              EB= EB+EJ
              XB = XB/EB
              YB = YB/EB
            ENDIF
          ENDIF
        ENDIF
  100 CONTINUE
C
C Make a trigger requirement: active region hit by particle
C
      IF(.NOT.AFLAG.OR..NOT.BFLAG) THEN
         REJFL = .TRUE.
         GOTO 999
      ENDIF
C
C Global system
        ZL = SIG*(ZAVER-ZORIG)
        XGA = XA + DXN(MODU) + OMZN(MODU)*YA - OMYN(MODU)*ZL
        YGA = YA + DYN(MODU) + OMXN(MODU)*ZL - OMZN(MODU)*XA
        XGB = XB + DXN(MODU) + OMZN(MODU)*YB - OMYN(MODU)*ZL
        YGB = YB + DYN(MODU) + OMXN(MODU)*ZL - OMZN(MODU)*XB
C
C Fiducial cut
      M8OK = 0
      M9OK = 0
      IF(KFLG.EQ.0) THEN
        DXB = XB*10.
        DYB = YB*10.
        IF(KEEPIT(DXB,DYB,5)) M8OK = M8OK+1
        IF(KEEPIT(DXB,DYB,6)) M9OK = M9OK+1
        XLOC = ABS(XA)
        YLOC = ABS(YA)
        XG  = XGB
        YG  = YGB
      ELSE
        DXA = XA*10.
        DYA = YA*10.
        IF(KEEPIT(DXA,DYA,5)) M8OK = M8OK+1
        IF(KEEPIT(DXA,DYA,6)) M9OK = M9OK+1
        XLOC = ABS(XB)
        YLOC = ABS(YB)
        XG = XGA
        YG = YGA
      ENDIF
      TH = ATAN(SQRT(XG**2+YG**2)/ZAVER)
C
C Non fiducial cut (again with hardwired constants)
        DIST(1) = XLOC - 1.9
        DIST(2) = 24.5
        IF(YLOC.GT.YEDG) THEN
          DIS = XLOC-(17.5-YLOC)/0.75
          IF(DIS.LT.DIST(1)) DIST(1)=DIS
          IF(XLOC.LT.XEDG) DIST(2) = YLOC-(17.5-XLOC*0.75)
        ELSE
          DIST(1) = XLOC-XEDG
        ENDIF
C
C it is effectively 0.8cm (instead of 1cm) because of lateral leakage.
        IF(DIST(1).GT.DICUT.AND.TH.LT.THCUT) M8OK = M8OK + 2
        IF(DIST(1).GT.1.5.AND.TH.LT.THCUT) M9OK = M9OK + 2
C
C Phi cut
        PHIA = ATAN2(YGA,XGA)
        IF(PHIA.LT.0.) PHIA=PHIA+2.*PI
        PHIB = ATAN2(YGB,XGB)
        IF(PHIB.LT.0.) PHIB=PHIB+2.*PI
        DPHI = ABS(PHIA-PHIB)*180./PI
        DPHI = AMIN1(DPHI,360.-DPHI)
        IF(DPHI.GT.DPCUT) M8OK = M8OK + 4
C
C Energy cut
      EMIN = AMIN1(EA,EB)
      IF(EA/CMSENE.GT.ECCUT.AND.EB/CMSENE.GT.ECCUT.AND.
     &   (EA+EB)/CMSENE.GT.ESCUT) M8OK = M8OK + 8
C
C Now histogram the various cut quantities for Meth 8 events
      IF(M8OK.EQ.15) ACCFL = .TRUE.
      IF(ACCFL)  CALL HF1(10056,TH*1000.,1.)
      IF(ACCFL.AND.M9OK.EQ.3)  CALL HF1(10057,TH*1000.,1.)
      IF(ACCFL.OR.M8OK.EQ.13)  CALL HF1(10051,DIST(1),1.)
      IF(ACCFL.OR.M8OK.EQ.11)  CALL HF1(10053,DPHI,1.)
      IF(ACCFL.OR.M8OK.EQ.7)   CALL HF2(10052,EA,EB,1.)
      xe = EMIN/CMSENE
      IF(ACCFL.OR.M8OK.EQ.7)   CALL HF1(10054,xe ,1.)
      xetot = (EA+EB)/CMSENE
      IF((ACCFL.OR.M8OK.EQ.7).AND.EMIN/CMSENE.GT.ECCUT)
     &      CALL HF1(10055,xetot,1.)
C
  999 CONTINUE
      END
      SUBROUTINE REJSEV(QM,QP,QK,NPHOT,VRTEX,REJFL)
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
C?                           RM 1992
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
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      COMMON/ CDECUT /E1CUT,E2CUT
      real*4 E1CUT,E2CUT
cc      PARAMETER (Z0 = 252.83)
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
      SUBROUTINE BOKER8(MODE)
C*****************************************************************
C Calulate cross-section with UNWEIGHTED events within geometrical
C acceptance of ALEPH
C                                  Bolek Pietrzyk January 1992
C  Modified B.Bloch Jan 2000 : introduce common /BOKRL/
C                              fix error calculation as for Sical
C                              by R. Miquel
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
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
C THIS IS COMMON BLOCK FROM INSIDE GENERATOR !!!!!!!
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
C !!!!!!!
      COMMON / BOKRL / NACC(4)
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
C      DIMENSION NACC(4)
      IF(MODE.EQ.-1) THEN
*     *******************
      XMIN= 0.1
C.....initialize scalers
      DO 10 K = 1,4
   10 NACC(K) = 0
      NEVGEN  = 0
      ELSEIF(MODE.EQ.0) THEN
*     **********************
      NEVGEN=NEVGEN+1
      LWIDE  = .FALSE.
      LNARR  = .FALSE.
      LMIX1  = .FALSE.
      LMIX2  = .FALSE.
c Three triggers TRIGA2 = ALEPH
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
      ELSEIF(MODE.EQ.1) THEN
*     ***********************
      WRITE(NOUT,BXOPE)
      WRITE(NOUT,BXTXT) '============ BOKER8 ============='
      WRITE(NOUT,BXTXT) '      BHLUM4 related tests       '
      CALL BHLUMI(   1,XPAR,NPAR)
      NEVT  = NPAR(20)
      XCRU  = XPAR(20)
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXTXT) 'Integrated x-sect. total best    '
      WRITE(NOUT,BXTXT) '(with vac., with Z, with s-chan.)'
      WRITE(NOUT,BXTXT) 'UNWEIGHTED EVENTS                '
      WRITE(NOUT,BXTXT) '*********************************'
      WRITE(NOUT,BXL1I) NEVGEN,     'generated events   ','NEVGEN','  '
      WRITE(NOUT,BXL1I) NACC(1),    'wide-wide accepted ','NACC1 ','  '
      WRITE(NOUT,BXL1I) NACC(2),    'narrow-narrow acc. ','NACC2 ','  '
      WRITE(NOUT,BXL1I) NACC(3),    'narrow-wide accept.','NACC3 ','  '
      WRITE(NOUT,BXL1I) NACC(4),    'wide-narrow accept.','NACC4 ','  '
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
      WRITE(NOUT,BXL2G) XWW,DXWW,   'wide-wide accepted ','XWW   ','  '
      WRITE(NOUT,BXL2G) XNN,DXNN,   'narrow-narrow acc. ','XNN   ','  '
      WRITE(NOUT,BXL2G) XNW,DXNW,   'narrow-wide accept.','XNW   ','  '
C  type values of rundom generator on the end of gereration
      CALL RMARUT(I1OUT,N1OUT,N2OUT)
 1234 FORMAT(1X,'RANMAR at the end of generation',3I12)
      WRITE(NOUT,1234) I1OUT,N1OUT,N2OUT
      ENDIF
*     *****
      END
      SUBROUTINE BOKERS(MODE)
C*****************************************************************
C Calulate cross-section with UNWEIGHTED events within geometrical
C acceptance of ALEPH
C                                  Ramon Miquel   novemb 1992
C  Modified B.Bloch Jan 2000 : introduce common /BOKRS/
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
      COMMON / WGTALL / WTMOD,WTCRU1,WTCRU2,WTSET(300)
      COMMON / PARGEN / CMSENE,TMING,RAXIG,VMAXG,XK0,KEYOPT,KEYRAD
      COMMON / PAROBL / TMINW,RAXIW,TMINN,RAXIN,VMAXE,KEYTRI
C THIS IS COMMON BLOCK FROM INSIDE GENERATOR !!!!!!!
      COMMON / TRANSR / TRAN,TRMIN,TRMAX
C !!!!!!!
      LOGICAL LWIDE,LNARR,LMIX1,LMIX2
      DIMENSION NPAR(100),XPAR(100)
      COMMON / BOKRS / NACC(4)
C      DIMENSION NACC(4)
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
C Three triggers TRIGAS = ALEPH    SICAL
        CALL TRIGAS(TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,XMIX1,XMIX2)
C       WRITE (6,*) ' X wide narr,mix1 mix2 ' , XWIDE,XNARR,XMIX1,XMIX2
        LWIDE= XWIDE.GT.XMIN
        LNARR= XNARR.GT.XMIN
        LMIX1= XMIX1.GT.XMIN
        LMIX2= XMIX2.GT.XMIN
C BHLUM4 total... with vac_pol and Z
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
      ELSEIF(MODE.GE.1) THEN
*     ***********************
cc-rm
        write (6,*) 'nback = ', nback
        write (6,*) 'nbackp= ', nbackp
        write (6,*) 'nbackt= ', nbackt
cc-rm
        WRITE(NOUT,BXOPE)
        WRITE(NOUT,BXTXT) '============ BOKERS ============='
        WRITE(NOUT,BXTXT) '      BHLUM4 related tests       '
        CALL BHLUMI(MODE,XPAR,NPAR)
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
      SUBROUTINE TRIGA2 (TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,
     $                  XMIX1,XMIX2)
C     **********************************************************
C MODIFIED TRIGA1 FOR ALEPH GEOMETRICAL CUTS
C Idealized exper. CALORIMETRIC trigger on dressed final electrons.
C Electrons and photons not distinguished!
C     ******************************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER( PI = 3.1415926535897932D0 )
      LOGICAL LANGW,LANGN,LPHI
      LOGICAL LNARROW,LWIDE,KEEPIT
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      REAL *4 X4,Y4
      DIMENSION PC(100,4)
      DIMENSION PCW1(4),PCN1(4)
      DIMENSION PCW2(4),PCN2(4)
      DATA ICONT /0/
C Beam energy
      ENE = P1(4)
C Final electrons and photons not distinguished
      DO 10 K=1,4
      PC(1,K)=P2(K)
   10 PC(2,K)=Q2(K)
      DO 20 I=1,NPHOT
      DO 20 K=1,4
   20 PC(2+I,K)=PHOT(I,K)
      NP = NPHOT+2
      DO 40 K=1,4
      PCW1(K)=0D0
      PCN1(K)=0D0
      PCW2(K)=0D0
   40 PCN2(K)=0D0
C
C Collecting energies in calorimeter sectors
C
      Z0 = 2800.D0
      DO 100 I=1,NP
C wide/narrow sectors forward
C Staszek's angels
      THETA=ANGFI(ABS(PC(I,3)),DSQRT(PC(I,1)**2+PC(I,2)**2))
      PHI  =ANGFI(PC(I,1),PC(I,2))
C Bolek's angels
c****      THETA = ACOS(ABS(PC(I,3))/PC(I,4))
C***      PHIA  = ATG( PC(I,1),PC(I,2)) - PI/2.
      X     = Z0*PC(I,1)/ABS(PC(I,3))
      Y     = Z0*PC(I,2)/ABS(PC(I,3))
      LNARROW = .FALSE.
      LWIDE   = .TRUE.
      IF( KEEPIT(X,Y,5) ) LNARROW = .TRUE.
      CALL LOOSEC(THETA,PHI,LWIDE)
      LANGW = .NOT. LWIDE
      LANGN = LNARROW
C**      X4 = X
C**      Y4 = Y
C**      IF(LANGN) CALL HF2(100,X4,Y4,1.)
C**      IF(LANGW) CALL HF2(200,X4,Y4,1.)
C**      TYPE *,I,PC(I,1),PC(I,2),THETA,THET,PHIA,PHI,X,Y,
C**     &       LNARROW,LWIDE,langw,LANGN
      IF(PC(I,3) .GT. 0D0) THEN
      IF(LANGW) THEN
        DO 50 K=1,4
   50   PCW1(K)=PCW1(K)+ PC(I,K)
      ENDIF
      IF(LANGN) THEN
        DO 51 K=1,4
   51   PCN1(K)=PCN1(K)+ PC(I,K)
      ENDIF
      ELSE
C wide/narrow sectors backward
      IF(LANGW) THEN
        DO 70 K=1,4
   70   PCW2(K)=PCW2(K)+ PC(I,K)
      ENDIF
      IF(LANGN) THEN
        DO 71 K=1,4
   71   PCN2(K)=PCN2(K)+ PC(I,K)
      ENDIF
      ENDIF
C**      TYPE *,PCW1(4),PCW2(4),PCN1(4),PCN2(4)
  100 CONTINUE
C at least one coincidences in a pair of opposite calorimetric blocks
      XWIDE= 0D0
      XNARR= 0D0
      XMIX1= 0D0
      XMIX2= 0D0
      IF(PCW1(4)/ENE .GT. 0.44D0  .AND.
     &   PCW2(4)/ENE .GT. 0.44D0  .AND.
     &  (PCW1(4)+PCW2(4))/(2D0*ENE) .GT. 0.6D0) XWIDE = PCW1(4)+PCW2(4)
      IF(PCN1(4)/ENE .GT. 0.44D0  .AND.
     &   PCN2(4)/ENE .GT. 0.44D0  .AND.
     &  (PCN1(4)+PCN2(4))/(2D0*ENE) .GT. 0.6D0) XNARR = PCN1(4)+PCN2(4)
      IF(PCW1(4)/ENE .GT. 0.44D0  .AND.
     &   PCN2(4)/ENE .GT. 0.44D0  .AND.
     &  (PCW1(4)+PCN2(4))/(2D0*ENE) .GT. 0.6D0) XMIX1 = PCW1(4)+PCN2(4)
      IF(PCN1(4)/ENE .GT. 0.44D0  .AND.
     &   PCW2(4)/ENE .GT. 0.44D0  .AND.
     &  (PCN1(4)+PCW2(4))/(2D0*ENE) .GT. 0.6D0) XMIX2 = PCN1(4)+PCW2(4)
      END
      SUBROUTINE TRIGAS (TH1N,TH2N,TH1W,TH2W,NPHI,XWIDE,XNARR,
     $                  XMIX1,XMIX2)
C     **********************************************************
C MODIFIED TRIGA1 FOR ALEPH GEOMETRICAL CUTS
C Idealized exper. CALORIMETRIC trigger on dressed final electrons.
C Electrons and photons not distinguished!
C     ******************************************
      IMPLICIT REAL*8(A-H,O-Z)
      real*4 xxf,yyf,xxb,yyb,atg
      external atg
      PARAMETER( PI = 3.1415 26535897932D0 )
C Depth at which the fiducial cut is done (mm)
cc-This is the number that was used for the tests
cc-   PARAMETER (Z0 = 2527.8)
cc      PARAMETER (Z0 = 2528.31)
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      COMMON/ CDECUT /E1CUT,E2CUT
      real*4 E1CUT,E2CUT
C Cuts in energy in one side and sum of both sides
C     PARAMETER (E1CUT = 0.44, E2CUT = 0.60)
C     PARAMETER (E1CUT = 0.44, E2CUT = 0.713)
C Delta phi cut
      PARAMETER (PHICUT = PI/6.)
      LOGICAL KEEPN,KEEPW,KEEPS
      LOGICAL LANGW,LANGN,LANGS
      LOGICAL LANGFW,LANGFN,LANGBN,LANGBW
      COMMON / MOMSET / P1(4),Q1(4),P2(4),Q2(4),PHOT(100,4),NPHOT
      DIMENSION PC(100,4)
      DIMENSION PCW1(4),PCN1(4)
      DIMENSION PCW2(4),PCN2(4)
      DIMENSION PCB(4),PCF(4)
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
        PCB(K)=0D0
        PCF(K)=0D0
      ENDDO
C
C Collecting energies in calorimeter sectors
C
      NP = NPHOT+2
      DO 100 I= 1,NP
CCC        THETA = ANGFI(ABS(PC(I,3)),DSQRT(PC(I,1)**2+PC(I,2)**2))
CCC        PHI   = ANGFI(PC(I,1),PC(I,2))
        z    = pc(i,3)
        if ( z.gt.0.) z0=z0a
        if ( z.lt.0.) z0=z0b
        X     = Z0*PC(I,1)/ABS(PC(I,3))
        Y     = Z0*PC(I,2)/ABS(PC(I,3))
C       WRITE ( 6,*) ' I  , X Y Z0' , I,X,Y,Z0
        LANGS = .FALSE.
        IF( KEEPS(X,Y,z) ) LANGS = .TRUE.
C
        if (.not.langs) go to 100
C forward
        IF(PC(I,3) .GT. 0D0) THEN
          IM = 1
          IF(LANGS) THEN
            DO 50 K=1,4
   50       PCF(K)=PCF(K)+ PC(I,K)
          ENDIF
        ELSE
C backward
          IM = 2
          IF(LANGS) THEN
            DO 70 K=1,4
   70       PCB(K)=PCB(K)+ PC(I,K)
          ENDIF
        ENDIF
  100 CONTINUE
        xf= 0.
        yf = 0.
        xb = 0.
        yb = 0.
C     WRITE ( 6,* ) ' PCF ' , PCF
C     WRITE ( 6,* ) ' PCB ' , PCB
      if (pcf(3).ne.0.) then
        XF    = Z0A*PCF(1)/ABS(PCF(3))
        YF    = Z0A*PCF(2)/ABS(PCF(3))
C     WRITE ( 6,* ) ' XF,YF ', XF,YF
      endif
      if (pcb(3).ne.0.) then
        XB    = Z0B*PCB(1)/ABS(PCB(3))
        YB    = Z0B*PCB(2)/ABS(PCB(3))
C     WRITE ( 6,* ) ' XB,YB ', XB,YB
      endif
C at least one coincidence in a pair of opposite calorimetric blocks
      XWIDE= 0D0
      XNARR= 0D0
      XMIX1= 0D0
      XMIX2= 0D0
      LANGFN = .FALSE.
      LANGFW = .FALSE.
      LANGBN = .FALSE.
      LANGBW = .FALSE.
      METHOD= 2
      IF( KEEPN(XF,YF,IM,METHOD) ) LANGFN = .TRUE.
      IF( KEEPW(XF,YF,IM,METHOD) ) LANGFW = .TRUE.
      IF( KEEPN(XB,YB,IM,METHOD) ) LANGBN = .TRUE.
      IF( KEEPW(XB,YB,IM,METHOD) ) LANGBW = .TRUE.
      xxf = xf
      xxb = xb
      yyf = yf
      yyb = yb
      if (yf.ne.0.)  PHIF = ATG(YYF,XXF)
      if (yb.ne.0.)  PHIB = ATG(YYB,XXB)
C     write ( 6,*)  'phif phib ' , PHIF , PHIB
C     write ( 6,*)  'pcf pcb(4)' , PCF(4),PCB(4)
      IF(LANGFW .AND. LANGBW    .AND.
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.
     &   PCF(4)/ENE .GT. E1CUT  .AND.
     &   PCB(4)/ENE .GT. E1CUT  .AND.
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XWIDE = PCF(4)+PCB(4)
      IF(LANGFN .AND. LANGBN    .AND.
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.
     &   PCF(4)/ENE .GT. E1CUT  .AND.
     &   PCB(4)/ENE .GT. E1CUT  .AND.
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XNARR = PCF(4)+PCB(4)
      IF(LANGFW .AND. LANGBN    .AND.
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.
     &   PCF(4)/ENE .GT. E1CUT  .AND.
     &   PCB(4)/ENE .GT. E1CUT  .AND.
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XMIX1 = PCF(4)+PCB(4)
      IF(LANGFN .AND. LANGBW    .AND.
     &   ABS(ABS(PHIF-PHIB)-PI) .LT. PHICUT  .AND.
     &   PCF(4)/ENE .GT. E1CUT  .AND.
     &   PCB(4)/ENE .GT. E1CUT  .AND.
     &   (PCF(4)+PCB(4))/(2D0*ENE) .GT. E2CUT) XMIX2 = PCF(4)+PCB(4)
 99   return
      END
      LOGICAL FUNCTION KEEPIT(X,Y,METH)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- John Renner Hansen    23-JAN-1990
C!
C!   Inputs:Method number:  METHOD = (5,6)
C!          X,Y of particle
C!        -
C!
C!   Outputs: KEEPIT
C!        -
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!
C?
      IMPLICIT REAL*8(A-H,O-Z)
      REAL *8 PADS
      REAL *8 X,Y,XP,YP
      REAL *8 LXP5(9)
      REAL *8 HXP(9)
      REAL *8 LYP(10)
      REAL *8 LXP6(9)
      DATA PADS/29.75D0/
      DATA LXP5/4.5D0,4.5D0,3.5D0,2.5D0,5*1.5D0/
      DATA HXP/10.5D0,9.5D0,9.5D0,9.5D0,8.5D0,7.5D0
     &              ,6.5D0,5.5D0,3.5D0/
      DATA LYP/0D0,3.D0,4.D0,5.D0,6.D0,7.D0
     &               ,8.D0,9.D0,10.D0,11.D0/
      DATA LXP6/5.5D0,5.5D0,4.5D0,2.5D0,5*1.5D0/
      INTEGER*4 METH
      LOGICAL LOG1,LOG2,LOG3,LOG4
C!======================================================================
      KEEPIT = .FALSE.
C
C     DEFINE ACTIVE REGION
      XP  =  ABS(X/PADS)
      YP  =  ABS(Y/PADS)
C
      IF(METH.EQ.5) THEN
      DO I = 1,9
        LOG1 = XP.GT.LXP5(I)
        LOG2 = XP.LT.HXP(I)
        LOG3 = YP.GE.LYP(I)
        LOG4 = YP.LE.LYP(I+1)
        IF(LOG1.AND.LOG2.AND.LOG3.AND.LOG4) THEN
          KEEPIT = .TRUE.
          GOTO 999
        ENDIF
      ENDDO
      ELSE
      DO I = 1,9
        LOG1 = XP.GT.LXP6(I)
        LOG2 = XP.LT.HXP(I)
        LOG3 = YP.GT.LYP(I)
        LOG4 = YP.LT.LYP(I+1)
        IF(LOG1.AND.LOG2.AND.LOG3.AND.LOG4) THEN
          KEEPIT = .TRUE.
          GOTO 999
        ENDIF
      ENDDO
      ENDIF
  999 RETURN
      END
      LOGICAL FUNCTION KEEPN(X,Y,IM,METH)
C----------------------------------------------------------------------
C!  -
C!
C!   Inputs:Method number:  METH = (1,2) for the moment
C!          X,Y of particle at Z of fiducial cut
C!        -
C!     R. MIQUEL  1992
C?
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C inner radius, radius of one pad, correction for asymmetry (mm)
C      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)
      PARAMETER ( RPAD = 5.225)
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      COMMON/ CDECUT /E1CUT,E2CUT
      real*4 E1CUT,E2CUT
C
C!======================================================================
      KEEPN = .FALSE.
C
      R   = SQRT(X**2+Y**2)
      IF ( IM.EQ.1) R0=R0A
      IF ( IM.EQ.2) R0=R0B
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
      RMIN = R0 + PMIN*RPAD - CORMIN
      RMAX = R0 + PMAX*RPAD - CORMAX
C     WRITE (6,*) 'R,R0,RMIN,RMAX' ,  R,R0,RMIN,RMAX
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN
        KEEPN = .TRUE.
      ENDIF
C     WRITE (6,* ) 'KEEPN ' , KEEPN
  999 RETURN
      END
      LOGICAL FUNCTION KEEPW(X,Y,im,METH)
C----------------------------------------------------------------------
C!  -
C!
C!   Inputs:Method number:  METH = (1,2) for the moment
C!          X,Y of particle at Z of fiducial cut
C!        -
C!    R. MIQUEL 1992
C?
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C inner radius, radius of one pad
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      COMMON/ CDECUT /E1CUT,E2CUT
      real*4 E1CUT,E2CUT
C      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)
      PARAMETER ( RPAD = 5.225 )
C!======================================================================
      KEEPW = .FALSE.
C
      R   = SQRT(X**2+Y**2)
      IF ( IM.EQ.1) R0=R0A
      IF ( IM.EQ.2) R0=R0B
C
C min, max pad for non-fiducial cut
      IF ( (METH.EQ.1) .OR. (METH.EQ.2) ) THEN
        PMIN = 1.
        PMAX = 15.
      ENDIF
C min, max radius for non-fiducial cut
      RMIN = R0 + PMIN*RPAD - CORMIN
      RMAX = R0 + PMAX*RPAD - CORMAX
C     WRITE (6,*) 'R,R0,RMIN,RMAX' ,  R,R0,RMIN,RMAX
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN
        KEEPW = .TRUE.
      ENDIF
C     WRITE (6,* ) 'KEEPW ' , KEEPW
  999 RETURN
      END
      LOGICAL FUNCTION KEEPS(X,Y,Z)
C----------------------------------------------------------------------
C!  -
C!
C!   Inputs: X,Y of particle at Z of fiducial cut
C!
C?      R. MIQUEL 1992
C----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C inner radius, radius of one pad
C      PARAMETER (R0   = 60.93, RPAD = 5.225)
      COMMON / CDBHGO / R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      real*4 R0A,R0B,CORMIN,CORMAX,Z0A,Z0B
      COMMON/ CDECUT /E1CUT,E2CUT
      real*4 E1CUT,E2CUT
C      PARAMETER (R0   = 60.93, RPAD = 5.225, CORR = 0.028)
      PARAMETER ( RPAD = 5.225 )
C!======================================================================
      KEEPS = .FALSE.
C
      R   = SQRT(X**2+Y**2)
      if(z.gt.0.) r0 = r0a
      if(z.lt.0.) r0 = r0b
C
C min, max pad for being inside SiCAL
      PMIN = 0.
      PMAX = 16.
C min, max radius for non-fiducial cut
      RMIN = R0 + PMIN*RPAD
      RMAX = R0 + PMAX*RPAD
C     WRITE (6,*) 'R,R0,RMIN,RMAX' ,  R,R0,RMIN,RMAX
      IF ( (R .GT. RMIN) .AND. (R .LT. RMAX) ) THEN
        KEEPS = .TRUE.
      ENDIF
C     WRITE (6,* ) 'KEEPS ' , KEEPS
  999 RETURN
      END
      SUBROUTINE LOOSEC(THETA,PHI,REJFL)
C----------------------------------------------------------------------
C!  - Bhabha selection method 5 on loose side
C!
C!   Author   :- Peter H. Hansen       27-NOV-1990
C!
C!   Inputs: THETA,PHI of cluster centroid in global system
C!   Output: REJFL = .TRUE. if rejected
C!                 = .FALSE. if accepted
C?
C!======================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DXN(4),DYN(4),DZN(4),OMXN(4),OMYN(4),OMZN(4)
      DIMENSION DIST(2)
      LOGICAL REJFL
C 1991 Alignment constants
       DATA DXN/ -0.096, -0.118,  0.039,  0.027/
       DATA DYN/ -0.310, -0.012, -0.026, -0.100/
       DATA DZN/  0.134,  0.164,  0.317,  0.320/
       DATA OMXN/ -0.00019, 0.00007, -0.00309, -0.00353/
       DATA OMYN/ -0.00119, -0.00043, -0.00030, -0.00260/
       DATA OMZN/  0.01248, 0.01248, -0.00148, -0.00148/
C-----------------------------------------------------------------------

      REJFL = .TRUE.
C
C Sign z and module number
      IF(THETA.GT.1.D0) THEN
        SIG  =-1.D0
        MODU = 2
      ELSE
        SIG  = 1.D0
        MODU = 4
      ENDIF
      IF(COS(PHI).LT.0.D0) MODU=MODU-1
C
C X and Y in global system
C
      X = SIG*280.D0*COS(PHI)*TAN(THETA)
      Y = SIG*280.D0*SIN(PHI)*TAN(THETA)
C
C Local system
      ZL = SIG*17.5D0
      XLOC = X - DXN(MODU) - OMZN(MODU)*Y + OMYN(MODU)*ZL
      YLOC = Y - DYN(MODU) - OMXN(MODU)*ZL + OMZN(MODU)*X
C
C Fold into first quadrant
      XLOC = ABS(XLOC)
      YLOC = ABS(YLOC)
C
C Find distance to edges (as in LCLUTW, but database hardwired)
      DIST(1) = XLOC - 1.9D0
      DIST(2) = 24.5D0
      IF(YLOC.GT.8.4D0+1.2D0) THEN
        DIS = XLOC-(17.5D0-YLOC)/0.75D0
        IF(DIS.LT.DIST(1)) DIST(1)=DIS
        DIST(2) = YLOC-(17.5D0-XLOC*0.75D0)
      ELSE
        DIST(1) = XLOC-11.9D0
      ENDIF
C
C Now make the cut on the inner boundary
C and on the outer boundary
      IF(THETA.GT.1.D0) THETA = 3.14159D0-THETA
      IF(DIST(1).GT.1.D0  .AND.
     &   DIST(2).GT.0.75D0  .AND.
     &   THETA.LT.0.125D0)  REJFL = .FALSE.
C**      TYPE *,DIST(1),DIST(2),THETA,REJFL
C
  999 CONTINUE
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
