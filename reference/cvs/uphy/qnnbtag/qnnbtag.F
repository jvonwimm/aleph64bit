      SUBROUTINE QNNBTAG(IOPVAR,ISMEAR,ICB,PHB1,PHB2,IERNNT)
      LOGICAL ERR
      CHARACTER*13 EIGNAM
#include "qcde.h"
      LOGICAL  CHTSIM
      EXTERNAL CHTSIM IFANNEE
      DIMENSION LHEMI1(10),LHEMI2(10),PHB1(4),PHB2(4)
#include "maxtrk.h"
#include "btagraw.h"
#include "pubraw.h"
#include "btpar.h"
#include "comlpt.h"
#include "comnnt.h"
#include "jsp1co.h"
      DIMENSION QT(KCQVEC,1),KT(KCQVEC,1),QV(KCQVRT,1),KV(KCQVEC,1)
#include "btaginl.h"
#include "qmacro.h"
      IERNNT = 0
      IOPVA1 = IOPVAR
      IF (.NOT.IDEB) THEN
C---- Initialize the Neural Net. ----
      CALL TIMED(T)
      CALL MLP_INI
      ENDIF
      IDEB = .TRUE.
C
      NKCOUT = NKCOUT + 1
      IF (XMCEV) THEN
      CALL Z0VERI
      IF (ITQUA.EQ.1) NB1   = NB1   + 1
      IF (ITQUA.EQ.2) NC1   = NC1   + 1
      IF (ITQUA.EQ.3) NUDS1 = NUDS1 + 1
      ELSE
                      NDAT1 = NDAT1 + 1
      ITQUA = 0
      ENDIF
C---- Test of detector status for data ----
      IF (.NOT.XMCEV.AND.KRUN.GE.6822) THEN
      IF ((.NOT.XVECAL).OR.(.NOT.XVTPCD).OR.(.NOT.XVITC)
     >.OR.(.NOT.XVHCAL).OR.(KEVEES.NE.1).OR.(.NOT.XVDEOK())) THEN
      NERR(1,ITQUA) = NERR(1,ITQUA) + 1
      GOTO 99
      ENDIF
      ENDIF
C---- CLASS 16 simulation for M.C. ----
      IF (XMCEV) THEN
      CALL SELECT
      IF (NEVTYPE.NE.1) THEN
      NERR(2,ITQUA) = NERR(2,ITQUA) + 1
      GOTO 99
      ENDIF
      ENDIF
      NKPRIS = NKPRIS + 1
C ---- Definition of the two hemispheres ----
      CALL QJOPTR('EF',' ')
      IG  = KFEFT
      ID  = KLEFT
C---- Definition of the Thrust axis ----
      CALL TRLOCK
      CALL QJTHRU (THRU,'truv',KRECO)
      ITHRU = KPDIR ('truv',KRECO)
      IF (ABS(QCT(ITHRU)).GT.0.866) THEN
      NERR(9,ITQUA) = NERR(9,ITQUA) + 1
      GOTO 99
      ENDIF
      CALL QJHEMI ('SAME','OPPO',KRECO,ITHRU,0.)
      IVEC1  = KPDIR ('SAME',KRECO)
      IVEC2  = KPDIR ('OPPO',KRECO)
      IF (IVEC1.EQ.0.OR.IVEC2.EQ.0) GOTO 99
C---- Hemisphere-track association ----
C     ihemi(itk) = 1 (2) for hemisphere ivec1 (ivec2)
C     ihemi(itk) = 999 : bad track, not used in the analysis
      NLEP = 0
      DO ILEP = 1 , 10
      IHLEP (ILEP) = 0
      ENDDO
      DO 13 ITK = IG , ID
      IHEMI (ITK) = 0
      IF (XSAME(ITK,IVEC1).AND.(.NOT.XSAME(ITK,IVEC2)) )    THEN
      IHEMI (ITK) = 1
      ELSEIF(XSAME(ITK,IVEC2).AND.(.NOT.XSAME(ITK,IVEC1)) ) THEN
      IHEMI (ITK) = 2
      ELSE
      IHEMI (ITK) = 999
      ENDIF
  13  CONTINUE
C ... Lepton identification
      DO 18 ITK = KFCHT , KLCHT
      IF (.NOT.XLEPTG(ITK)) GOTO 18
      IF (QP(ITK).LT.3.)    GOTO 18
C ...Use tighter cuts for Electron candidates (add dE/dX cuts)
      IF (KLEPPA(ITK).EQ.2.OR.KLEPPA(ITK).EQ.3.OR.KLEPPA(ITK).EQ.12.
     >.OR.KLEPPA(ITK).EQ.13.OR.KLEPPA(ITK).EQ.22.OR.KLEPPA(ITK).EQ.23)
     >THEN
      IF(CHTSIM(IVERS)) THEN
      CALL QDEDX  (ITK,1,.511E-3,1.,RI,NS,TL,RIEXP,SIGM,JER)
      ELSE
      CALL QDEDXM (ITK,1,.511E-3,1.,RI,NS,TL,RIEXP,SIGM,JER)
      ENDIF
C ...
      IF (JER.LE.0) THEN
      REST = (RI-RIEXP)/SIGM
      ELSE
      REST = 1000.
      ENDIF
C
      IF (QEIDRI(ITK,2).LT.-1.6.OR.QEIDRI(ITK,2).GT.999.) GOTO 19
      IF (QEIDRI(ITK,3).LT.-1.8.OR.QEIDRI(ITK,3).GT.3.)   GOTO 19
      IF (JER.GT.0.OR.NS.LT.50.OR.(REST.LT.-2.5.AND.REST.GE.1000.))
     >    GOTO 19
C ...End for electron id
      NLEP = NLEP + 1
      IHLEP (NLEP) = 2
      IF (XSAME(ITK,IVEC1)) IHLEP(NLEP) = 1
  19  CONTINUE
      ENDIF
c ...Standard muon Id
      IF (KLEPPA(ITK).EQ.5.OR.KLEPPA(ITK).EQ.6.OR.KLEPPA(ITK).EQ.15
     >.OR.KLEPPA(ITK).EQ.16) THEN
      NLEP = NLEP + 1
      IHLEP (NLEP) = 2
      IF (XSAME(ITK,IVEC1)) IHLEP(NLEP) = 1
      ENDIF
  18  CONTINUE
C
      NKTRUS = NKTRUS + 1
      IF5    = 0
      CALL QIPBTAG (IRET,NTRACK,NJET4,TRKJET,FRF2TRK,
     >              PROBTRK,PROBJET,PROBHEMI,PROBEVT)
C
      IF (IRET.NE.0) THEN
      NERR(7,ITQUA) = NERR(7,ITQUA) + 1
      IF5 = 1
      ENDIF
C --- Smearing of the impact parameter resolution (Lorenzo MONETA) ---
      IF (XMCEV.AND.NTRACK.GT.0.AND.IRET.EQ.0.AND.ISMEAR.EQ.1) THEN
C
C  smear the track
C
          DO 10 IT =1,NTRACK
             ITKTY = TTYPE(TFLAG(IT))
             CALL MYSMEAR(1,ITKTY,DS)
             DMIN(IT)  =  DMIN(IT) + SIGN(DS*S_DMIN(IT),DMIN(IT) )
  10      CONTINUE
C
C  recalculate the probability
C
        IF (NTRACK.GT.0) THEN
          CALL BTAGTR(FITP,NTRACK,DMIN,S_DMIN,TFLAG,PROBTRK)
          CALL BTAGEV(NTRACK,NJET,JJET,JHEMI,PROBTRK,NEGPROB,
     &                PROBJET,PROBHEMI,PROBEVT)
        ELSE
          PROBHEMI(1) = 1.1
          PROBHEMI(2) = 1.1
        ENDIF

      ENDIF
C---  Correct the charm hadron decay and b and c-lifetimes
      IF (XMCEV.AND.ICB.EQ.1) THEN
      CALL W_CHPH2 (HAXIS,LHEMI1,LHEMI2,POIDS1,POIDS2)
      ELSE
      POIDS1 = 1.
      POIDS2 = 1.
      ENDIF
C---- Correspondance hemisphere alpha --> hemisphere qipbtag ----
C                               IVEC1 --> PROBHEMI(IQ1)
C                               IVEC2 --> PROBHEMI(IQ2)
        IQ1  = 1
        IQ2  = 2
        N_OK = 0
        DO 15 NJW = 1 , NTRACK
        IJCH = FRF2TRK (NJW) + KFCHT - 1  ! Numero trace alpha
        DO 14 ITK = KFCHT , KLCHT
        IF (ITK.NE.IJCH) GOTO 14
        IF (XSAME(ITK,IVEC1)) THEN
          N_OK = 1
          IQ1 = JHEMI(NJW)
          IF (IQ1.EQ.2) IQ2 = 1
        ELSE
          N_OK = 2
          IQ2 = JHEMI(NJW)
          IF (IQ2.EQ.1) IQ1 = 2
        ENDIF
        IF (N_OK.NE.0) GOTO 17
 14     CONTINUE
 15     CONTINUE
 17     CONTINUE
      CALL TRUNLOCK
C
C---- Compute the jet shape variables for each Hemisphere ---
C
      CALL QLOCK (IVEC1)
      DO 73 IH = 1 , 2
      CALL QLREV (KRECO)
      NLEPH = 0
C
      IF (IH.EQ.1)     THEN
      IF (PROBHEMI(IQ1).GT.0.)  THEN
      PROBH   = LOG10(PROBHEMI(IQ1))
      ELSE
      PROBH   = 1.
      ENDIF
      ENDIF
C
      IF (IH.EQ.2)     THEN
      IF (PROBHEMI(IQ2).GT.0.)  THEN
      PROBH   = LOG10(PROBHEMI(IQ2))
      ELSE
      PROBH   = 1.
      ENDIF
      ENDIF
C
      IF1 = 0
      IF2 = 0
      IF3 = 0
      IF4 = 0
      CALL TRLOCK
      ENERC = 0.
      PNERC = 0.
      EMAX  = -1.
      ITKMAX = -1
      DO ITK = IG , ID
      CALL POLTRK (ITK,ITHRU,1)
      IF (.NOT.XLOCK(ITK)) THEN
      IF (QE(ITK).GT.EMAX) THEN
      ITKMAX = ITK
      EMAX   = QE(ITK)
      ENDIF
      ENERC = ENERC + QE(ITK)
      PNERC = PNERC + QP(ITK)
      ENDIF
      ENDDO
      FRACIT = EMAX / ENERC
      IF (ENERC.LT.1.0) THEN
       IF1 = 1
       GOTO 98
      ENDIF
      CALL QJOPTR ('EF',' ')
      YCUT = (6.0 / ENERC)**2
      CALL QGJMMC (NJET,CNAM,KRECO,YCUT,EVIS,SCHEME,VERSN)
      CALL CLASSJET (KRECO)
      IF (ENERC.LT.5.0) IF1 = 1
C ---compute the nucleated mass (P. ROSNET) ---
      KTRX = 50     !  maximal number of tracks used
      XCUT = 2.1    !  maximal mass for the algorithm
      CALL NUCLEAT (ITKMAX,XCUT,KTRX,IVEC1,IVEC2,ENUCL)
C
C---- Compute the number of leptons per Hemisphere ----
C
      DO IL = 1 , NLEP
      IF (IHLEP(IL).EQ.IH) NLEPH = NLEPH + 1
      ENDDO
C
C---- Compute the Missing Energy of each Hemisphere ----
C
      IF (IH.EQ.1) THEN
      IHEM = IVEC1
      POIDS = POIDS1
      ELSE
      IHEM = IVEC2
      POIDS = POIDS2
      ENDIF
      IF((QE(IVEC1)**2 - QP(IVEC1)**2).GE.0.) THEN
        XMH1 = SQRT ( QE(IVEC1)**2 - QP(IVEC1)**2 )
      ELSE
        XMH1 = 0.
      ENDIF
      IF((QE(IVEC2)**2 - QP(IVEC2)**2).GE.0.) THEN
        XMH2 = SQRT ( QE(IVEC2)**2 - QP(IVEC2)**2 )
      ELSE
        XMH2 = 0.
      ENDIF
      DELTA = ( XMH1**2 - XMH2**2 ) / 2. / QELEP
      IF (IH.EQ.2) DELTA = - DELTA
      ENU  = QELEP / 2. + DELTA - QE(IHEM)
C---- Compute the Boosted Sphericity (C. BOWDWRY) ----
      BETA1  = 0.965
      BETA2  = 0.965
      CALL JETSPH (KRECO,BETA1,BETA2,SJ1,SJ2,DSPH,ERR)
      IF (ERR) THEN
      NERR(5,ITQUA) = NERR(5,ITQUA) + 1
      IF2 = 1
      ENDIF
C---- Compute the other variables -----
      CALL TAGVAR1
      CALL TAGVAR2
      CALL TAGVAR3
      CALL TAGVAR4
      CALL TAGVAR5
C-----------------------------------------
      IF (IFLAG4.EQ.1) THEN
      NERR(6,ITQUA) = NERR(6,ITQUA) + 1
      IF3 = 1
      ENDIF
      IF (IFLAG5.EQ.1) THEN
      NERR(8,ITQUA) = NERR(8,ITQUA) + 1
      IF4 = 1
      ENDIF
C---- Test of the errors ----
  98  CONTINUE
      IF (IF1.EQ.1.OR.IF2.EQ.1.OR.IF3.EQ.1.OR.IF4.EQ.1.OR.
     >    IF5.EQ.1) GOTO 99
      IF (IH.EQ.2) THEN
                      NGEN  = NGEN  + 1
      IF (ITQUA.EQ.1) NB2   = NB2   + 1
      IF (ITQUA.EQ.2) NC2   = NC2   + 1
      IF (ITQUA.EQ.3) NUDS2 = NUDS2 + 1
      IF (ITQUA.EQ.0) NDAT2 = NDAT2 + 1
      ENDIF
C---- Loop on the different versions
      DO 75 ILOOP = 1 , 4
      ILOOP1 = ILOOP
c option 1
      IF (ILOOP.EQ.1) THEN
      VTOT(1)  = PCL(1)
      VTOT(2)  = HJ5
      VTOT(3)  = YMT
      VTOT(4)  = XMINV(2)
      VTOT(5)  = XJFM
      VTOT(6)  = PCT(2)
      VTOT(7)  = PCT(1)
      VTOT(8)  = FLOAT(KK1)
      VTOT(9)  = XMKK3
      VTOT(10) = XMKKC
      VTOT(11) = SPTC
      VTOT(12) = FRACC
      VTOT(13) = FRACIT
      VTOT(14) = XMKKI
      VTOT(15) = FRACI
      VTOT(16) = SDIR(2)
      VTOT(17) = SDIR(3)
      VTOT(18) = SDIR(7)
      VTOT(19) = SDIR(8)
      VTOT(20) = XMINV(7)
      VTOT(21) = EMINV(2)
      VTOT(22) = EMINV(3)
      VTOT(23) = EMINV(6)
      VTOT(24) = EMINV(8)
      VTOT(25) = ENUCL
      ENDIF
C option 2
      IF (ILOOP.EQ.2) THEN
      VTOT(1)  = XJTLJET
      VTOT(2)  = SJ1
      VTOT(3)  = PTJETS
      VTOT(4)  = SPT
      VTOT(5)  = PTLJET
      VTOT(6)  = PCL(1)
      VTOT(7)  = PCL(2)
      VTOT(8)  = PCT(1)
      VTOT(9)  = YMT
      VTOT(10) = REVIS
      VTOT(11) = SDIR(1)
      VTOT(12) = XJFM
      VTOT(13) = PCT(2)
      VTOT(14) = YMJET
      VTOT(15) = SMJETS
      VTOT(16) = PROBH
      VTOT(17) = FLOAT(KK)
      VTOT(18) = FLOAT(KK1)
      VTOT(19) = XMKK1
      VTOT(20) = ENU
      ENDIF
C option 3
      IF (ILOOP.EQ.3) THEN
      VTOT(1)  = PTJETS
      VTOT(2)  = SPT
      VTOT(3)  = PTLJET
      VTOT(4)  = PCL(1)
      VTOT(5)  = PCL(2)
      VTOT(6)  = PCT(1)
      VTOT(7)  = YMT
      VTOT(8)  = REVIS
      VTOT(9)  = SDIR(1)
      VTOT(10) = PCL(3)
      VTOT(11) = XJFM
      VTOT(12) = YMJET
      VTOT(13) = SMJETS
      VTOT(14) = PROBH
      VTOT(15) = FLOAT(KK2)
      VTOT(16) = FLOAT(KK3)
      VTOT(17) = FLOAT(KK)
      VTOT(18) = FLOAT(KK1)
      VTOT(19) = XMKK1
      VTOT(20) = NLEPH
      ENDIF
C option 4
      IF (ILOOP.EQ.4) THEN
      VTOT(1)  = HJ5
      VTOT(2)  = PCL(1)
      VTOT(3)  = PCL(2)
      VTOT(4)  = PCT(1)
      VTOT(5)  = SDIR(8)
      VTOT(6)  = YMT
      VTOT(7)  = XMINV(2)
      VTOT(8)  = XMKKI
      VTOT(9)  = SDIR(1)
      VTOT(10) = FRACI
      VTOT(11) = XJFM
      VTOT(12) = PCT(2)
      VTOT(13) = FLOAT(KK3)
      VTOT(14) = FLOAT(KK1)
      VTOT(15) = XMKK3
      VTOT(16) = PCT(3)
      VTOT(17) = XMKKC
      VTOT(18) = SPTC
      VTOT(19) = FRACC
      VTOT(20) = FRACIT
      ENDIF
      IF (IOPVAR.EQ.1) CALL MLP_VAR
      CALL MLP_ANA
      IF (IH.EQ.1) THEN
      PHB1 (ILOOP) = RESULT
      ELSE
      PHB2 (ILOOP) = RESULT
      ENDIF
  75  CONTINUE
C---- Unlock the tracks ----
      CALL TRUNLOCK
      DO ICH = IG , ID
      CALL POLTRK(ICH,ITHRU,2)
      ENDDO
  73  CONTINUE
C
      CALL TRUNLOCK
      CALL QLZER (KRECO)
      GOTO 999
 99   CONTINUE
      IERNNT = 1
  999 CONTINUE
      CALL TIMED(TT)
      TTT = TTT + TT
      END
C
C---- Subroutines ----
C
