      SUBROUTINE QUINIT
C
C-----------------------------------------------------------------------
C user initialisation
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      COMMON/JBEMU/BCTHMX,ACOC,NHTPMN,D0CMN,Z0CMN,D0CMX,Z0CMX,
     &NCHC,NCHCMN,ENCHMX,EC0MX,TPCUT,PHGCUT,THGCUT,PHECUT,THECUT
      COMMON/EMUPA/CSTAR,ACOL,ELEP,IDTK(2),PTK(2),DPTK(2),D0(2),Z0(2),
     &PHITK(2),THETK(2),IDFM(2),EASS(2),ENHES(2),EGASS,EGAH
      COMMON/ECOUNT/NTOTEV,NCL15,NEGH
C-----------------------------------------------------------------------
C
C
      NTOTEV=0
      NCL15=0
      NEGH=0
C
      CALL HBOOK1( 2000, 'KNCHT ', 20,0.,19., 0.)
      CALL HBOOK1(5001,'DPHI',80,-1.,1.)
      CALL HBOOK1(5002,'DTHE',80,-1.,1.)
      CALL HBOOK2( 11001,'ESUM VS PMES (1SIG)',60,0.,1.2,60,0.,1.2,0.)
      CALL HBOOK1( 11002, 'ESUM (1SIG) ', 60,0.,1.2, 0.)
      CALL HBOOK1( 11003, 'PSUM (1SIG) ', 60,0.,1.2, 0.)
      CALL HBOOK1( 11004, 'ESUM + PSUM (1SIG) ', 60,0.,2.4, 0.)
      CALL HBOOK2( 11010,'ESUM VS PMES',60,0.,1.2,60,0.,1.2,0.)
      CALL HBPROX( 11001)
      CALL HBPROY( 11001)
      CALL HBPROX( 11010)
      CALL HBPROY( 11010)
      CALL HBOOK1( 100, 'cos(theta*) of GENERATED BHABHAS',
     & 20, -1., 1., 0.)
      CALL HBOOK1( 101, 'cos(theta*) of generated Bhabhas (ACOL CUT)',
     & 20, -1., 1., 0.)
      CALL HBOOK1( 102, 'COS(THETA*) OF BHABHAS (CLASS 15)',
     & 20, -1., 1., 0.)
      CALL HBOOK1( 103, 'COS(THETA*) OF BHABHAS (ACOL CUT)',
     & 20, -1., 1., 0.)
C
C
      END
      SUBROUTINE QUNEWR(IROLD,IRNEW)
C
C-----------------------------------------------------------------------
C user initialisation
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
C
      CALL HINIRU
      CALL MINIJO
      CALL MGINIG(KUCONS,IROLD,IERR,IRNEW)
C
      END
      SUBROUTINE QUEVNT (QT,KT,QV,KV)
C
C-----------------------------------------------------------------------
C process one event
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      COMMON/JBEMU/BCTHMX,ACOC,NHTPMN,D0CMN,Z0CMN,D0CMX,Z0CMX,
     &NCHC,NCHCMN,ENCHMX,EC0MX,TPCUT,PHGCUT,THGCUT,PHECUT,THECUT
      COMMON/EMUPA/CSTAR,ACOL,ELEP,IDTK(2),PTK(2),DPTK(2),D0(2),Z0(2),
     &PHITK(2),THETK(2),IDFM(2),EASS(2),ENHES(2),EGASS,EGAH
      COMMON/ECOUNT/NTOTEV,NCL15,NEGH
      LOGICAL LFLACC, LFLC15, LFLBHA
      INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
C
      IF(.NOT.XLUMOK(DUMMY)) GO TO 999
C select E+E-/MU+MU-
      CALL BHASEL(1,1,1,1,LFLACC,LFLC15,LFLBHA)
      IF(LFLACC) NTOTEV=NTOTEV+1
      IF(LFLC15) NCL15=NCL15+1
      IF(LFLBHA) NEGH=NEGH+1
 999  CONTINUE
C
      END
      SUBROUTINE QUTERM
C
C-----------------------------------------------------------------------
C user termination
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      COMMON/JBEMU/BCTHMX,ACOC,NHTPMN,D0CMN,Z0CMN,D0CMX,Z0CMX,
     &NCHC,NCHCMN,ENCHMX,EC0MX,TPCUT,PHGCUT,THGCUT,PHECUT,THECUT
      COMMON/EMUPA/CSTAR,ACOL,ELEP,IDTK(2),PTK(2),DPTK(2),D0(2),Z0(2),
     &PHITK(2),THETK(2),IDFM(2),EASS(2),ENHES(2),EGASS,EGAH
      COMMON/ECOUNT/NTOTEV,NCL15,NEGH
      INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
C
C
      WRITE(6,*)'**********************************************'
      WRITE(6,*)'   RESULT OF SELECTION    '
      PRINT *,'*** NTOTEV = ',NTOTEV,'   NCL15 = ',NCL15,' ***'
      PRINT *,'*** NEGH = ',NEGH,' ***'
      WRITE(6,*)'**********************************************'
C
      END
      SUBROUTINE BHASEL(IDATA,IPRINT,IWRITE,IHISTO,LFLACC,LFLC15,LFLBHA)
C
C -------------------------------------------------------------------
C
C   Author         E. Locci     89/08/04  revisited 91/05/07
C
C   Input argument :
C                   IDATA  = 0 for MC, 1 for real data
C                   IPRINT = 0, 1 print flag
C                   IWRITE = 0, 1 write flag
C                   IHISTO = 0, 1 histo flag
C
C   Output argument :
C                   LFLACC = .TRUE. for events in acceptance
C                   LFLC15 = .TRUE. for events class 15 (FALSE for data)
C                   LFLBHA = .TRUE. for a Bhabha candidate
C
C   Description
C   ===========
C
C   The selected large angle Bhabhas fulfill the following conditions:
C        CLASS 15 EVENTS
C        NHITTPC > 4 & Z0 < 5 CM
C        AT LEAST 1 TRACK  WITH D0 < 2 CM
C        AT LEAST 2 TRACKS WITH D0 < 5 CM
C        AT MOST  6 TRACKS WITH D0 < 5 CM
C        COS THESTAR     = 0.90
C        ACOL            = 20 DEGREES
C        DPHI & DTHETA   = 20 DEGREES
C        P1 + P2         > 0.05 SQRT(S)
C        E1 + E2         > 0.20 SQRT(S)
C        SUM(P) + SUM(E) > 1.20 SQRT(S)
C   More details can be found in a ALEPHnote PHYSIC 90-107
C -------------------------------------------------------------------
C
      INCLUDE '/aleph/phy/qcde.inc'
      COMMON/JBEMU/BCTHMX,ACOC,NHTPMN,D0CMN,Z0CMN,D0CMX,Z0CMX,
     &NCHC,NCHCMN,ENCHMX,EC0MX,TPCUT,PHGCUT,THGCUT,PHECUT,THECUT
      COMMON/EMUPA/CSTAR,ACOL,ELEP,IDTK(2),PTK(2),DPTK(2),D0(2),Z0(2),
     &PHITK(2),THETK(2),IDFM(2),EASS(2),ENHES(2),EGASS,EGAH
      DIMENSION BCTH(3),PHIM(2)
      LOGICAL LFLACC, LFLC15, LFLBHA
      LOGICAL EVTFLG(30)
      INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
C
      LFLACC=.FALSE.
      LFLC15=.FALSE.
      LFLBHA=.FALSE.
C
      ELEP=QELEP
      IF (ELEP.LE.0.) ELEP = 91.2
C Cut definition
C
      BCTHMX=0.90
      ACOC=20.
      NHTPMN=4
      D0CMN=2.0
      Z0CMN=5.0
      D0CMX=5.0
      Z0CMX=5.0
      NCHC=6
      NCHCMN=2
      ENCHMX=0.001
      ECOMX=0.3
      TPCUT=20.*QQPI/180.
      PHGCUT=0.200
      THGCUT=0.050
      PHECUT=0.100
      THECUT=0.025
C
      CALL BHACUT
C
C Define angular range for MC. FOR MC ONLY
      IF(IDATA.EQ.0) THEN
        CALL CTKIN(BCTH,CSTARM,ACOLM,PHIM)
        IF(ABS(CSTARM).GT.BCTHMX) GO TO 500
        CALL HFILL( 100, CSTARM, 0., 1.)
        IF(ABS(ACOLM).GT.ACOC) GO TO 500
        CALL HFILL( 101, CSTARM, 0., 1.)
      ENDIF
C
      LFLACC=.TRUE.
C
C Ask for CLASS 15. FOR MC ONLY
      IF(IDATA.EQ.0) THEN
        CALL SELEVT(EVTFLG)
        IF(.NOT.EVTFLG(20)) PRINT *,'CLASS 20 = .FALSE.   ',KRUN,KEVT
        IF(.NOT.EVTFLG(15)) PRINT *,'CLASS 15 = .FALSE.   ',KRUN,KEVT
        IF(.NOT.EVTFLG(15)) GO TO 500
        LFLC15=.TRUE.
      ENDIF
C
      IF(IHISTO.NE.0) CALL HFILL(102,CSTAR,0.,1.)
C
C Define experimental angular acceptance
       IF(ABS(CSTAR).LE.BCTHMX.AND.ABS(ACOL).LE.ACOC) THEN
         IF((ABS(PTK(1))-0.5*ELEP).GT.3.*DPTK(1))
     &   PTK(1)=0.5*ELEP*PTK(1)/ABS(PTK(1))
         IF((ABS(PTK(2))-0.5*ELEP).GT.3.*DPTK(2))
     &   PTK(2)=0.5*ELEP*PTK(2)/ABS(PTK(2))
         PMES=(ABS(PTK(1))+ABS(PTK(2)))/ELEP
         ALPHA=0.00078
C Correct for saturation. FOR REAL DATA ONLY (IDATA=1)
         IF(IDATA.NE.0) THEN
           EASS(1)=EASS(1)/(1.-ALPHA*EASS(1))
           EASS(2)=EASS(2)/(1.-ALPHA*EASS(2))
           EGASS=EGASS/(1.-ALPHA*EGASS)
         ENDIF
         ESUM=(EASS(1)+EASS(2))/ELEP
         ESUME=EGASS/ELEP
         ESUMH=(EGAH+ENHES(1)+ENHES(2))/ELEP
         IF(IHISTO.NE.0) THEN
           CALL HFILL(11001,PMES,(ESUM+ESUME+ESUMH),1.)
           CALL HFILL(11002,(ESUM+ESUME+ESUMH),0.,1.)
           CALL HFILL(11003,PMES,0.,1.)
           CALL HFILL(11004,(PMES+ESUM+ESUME+ESUMH),0.,1.)
           CALL HFILL(11010,PMES,ESUM,1.)
         ENDIF
C Define a Bhabha candidate
         IF((PMES+ESUM+ESUME+ESUMH).GT.1.20.AND.ESUM.GT.0.2.AND.
     &   PMES.GT.0.05) THEN
           LFLBHA=.TRUE.
           IF(IHISTO.NE.0) THEN
             CALL HFILL(103,CSTAR,0.,1.)
             CALL HFILL(2000, FLOAT(KNCHT), 0., 1.)
           ENDIF
           IF(IPRINT.NE.0) THEN
             PRINT *,'*** BHABHAS : RUN,EVT ***',KRUN,KEVT
             PRINT 6789,KRUN,KEVT,ELEP,PTK(1),DPTK(1),D0(1),Z0(1),
     &       IDFM(1),EASS(1),ENHES(1),PHITK(1),THETK(1),
     &       PTK(2),DPTK(2),D0(2),Z0(2),IDFM(2),EASS(2),ENHES(2),       S(2),
     &       PHITK(2),THETK(2),EGASS,EGAH,CSTAR
           ENDIF
           IF(IWRITE.NE.0) THEN
             WRITE(12,8765) KRUN,KEVT
             WRITE(10,6789) KRUN,KEVT,ELEP,PTK(1),DPTK(1),D0(1),Z0(1),
     &       IDFM(1),EASS(1),ENHES(1),PHITK(1),THETK(1),
     &       PTK(2),DPTK(2),D0(2),Z0(2),IDFM(2),EASS(2),ENHES(2),       S(2),
     &       PHITK(2),THETK(2),EGASS,EGAH,CSTAR
           ENDIF
C
         ELSE
           IF(IWRITE.NE.0) THEN
             WRITE(14,6789) KRUN,KEVT,ELEP,PTK(1),DPTK(1),D0(1),Z0(1),
     &       IDFM(1),EASS(1),ENHES(1),PHITK(1),THETK(1),
     &       PTK(2),DPTK(2),D0(2),Z0(2),IDFM(2),EASS(2),ENHES(2),       S(2),
     &       PHITK(2),THETK(2),EGASS,EGAH,CSTAR
           ENDIF
C
         ENDIF
C
       ENDIF
CC
 500   CONTINUE
C
 6789 FORMAT(2I6,/,F8.3,/,4F8.3,I5,4F8.3,/,4F8.3,I5,4F8.3,/,2F8.3,F12.7)
 8765 FORMAT(2I6)
C
      RETURN
      END
      SUBROUTINE BHACUT
C
C-----------------------------------------------------------------------
C Electron analysis on reconstructed quantities
C
C   Author         E. Locci     89/08/04  revisited 91/05/07
C
C   Called from BHASEL
C
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      COMMON/JBEMU/BCTHMX,ACOC,NHTPMN,D0CMN,Z0CMN,D0CMX,Z0CMX,
     &NCHC,NCHCMN,ENCHMX,EC0MX,TPCUT,PHGCUT,THGCUT,PHECUT,THECUT
      COMMON/EMUPA/CSTAR,ACOL,ELEP,IDTK(2),PTK(2),DPTK(2),D0(2),Z0(2),
     &PHITK(2),THETK(2),IDFM(2),EASS(2),ENHES(2),EGASS,EGAH
      DIMENSION IDTRCK(100),ENTRCK(100),IARSTK(100)
      DIMENSION IDEAS(100),ENEAS(100),IARSAS(100)
      DIMENSION IDECO(100),ENECO(100),IARSEC(100)
      DIMENSION ENHE(2),PHHE(2),THHE(2),IDGA(2),EGAS(2),PHIG(2),THEG(2)
      DIMENSION IOPT1(6),IFOT1(19),PHOT1(20),CORE1(20),PHIS(2)
      DIMENSION KODE(4),NREG(3)
      DIMENSION P(3),V(3),PIMPAC(3,5),XINE(3)
      INTEGER EQEDGE
      INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
C
C Initialization
      CSTAR=1.
      ACOL=180.
      CALL VZERO(PIMPAC,15)
      CALL VZERO(IDTRCK,100)
      CALL VZERO(ENTRCK,100)
      CALL VZERO(IARSTK,100)
      CALL VZERO(IDTK,2)
      CALL VZERO(PTK,2)
      CALL VZERO(DPTK,2)
      CALL VZERO(D0,2)
      CALL VZERO(Z0,2)
      CALL VZERO(PHITK,2)
      CALL VZERO(THETK,2)
      CALL VZERO(IDFM,2)
      CALL VZERO(IDEAS,100)
      CALL VZERO(ENEAS,100)
      CALL VZERO(IARSAS,100)
      CALL VZERO(EASS,2)
      CALL VZERO(ENHE,2)
      CALL VZERO(PHHE,2)
      CALL VZERO(THHE,2)
      CALL VZERO(ENHES,2)
      CALL VZERO(IDECO,100)
      CALL VZERO(ENECO,100)
      CALL VZERO(IARSEC,100)
      CALL VZERO(IDGA,2)
      CALL VZERO(EGAS,2)
      CALL VZERO(PHIG,2)
      CALL VZERO(THEG,2)
      EGASS=0.
      ENHG=0.
      PHHG=0.
      THHG=0.
      EGAH=0.
C Find the 2 most energetic charged tracks
      NCHMN=0
      NCHMX=0
      NCH5=0
C No charged track found
      IF(KNCHT.LE.0) GO TO 999
C
      DO 10 ITK=KFCHT,KLCHT
C       Cut on NUMBER OF TPC HITS
        IF(KFRTNT(ITK).LT.NHTPMN) GO TO 10
        IF(QE(ITK).LT.ENCHMX) GO TO 10
C       Cut on D0,Z0
        IF(ABS(QDB(ITK)).LT.D0CMN.AND.ABS(QZB(ITK)).LT.Z0CMN) THEN
          NCHMN=NCHMN+1
          IF(QE(ITK).GE.1.) NCH5=NCH5+1
        ENDIF
        IF(ABS(QDB(ITK)).LT.D0CMX.AND.ABS(QZB(ITK)).LT.Z0CMX) THEN
          NCHMX=NCHMX+1
          IDTRCK(NCHMX)=ITK
          ENTRCK(NCHMX)=QE(ITK)
        ENDIF
 10   CONTINUE
C
C Cut on NUMBER OF TRACKS
      IF(NCHMX.LT.NCHCMN.OR.NCHMX.GT.NCHC.OR.NCHMN.LT.1) GO TO 999
C
      CALL SORTZV(ENTRCK,IARSTK,NCHMX,1.,1.,0)
      DO 101 IJK=1,2
        IDTK(IJK)=IDTRCK(IARSTK(IJK))
        PTK(IJK)=ENTRCK(IARSTK(IJK))*QCH(IDTK(IJK))
        DPTK(IJK)=QSIGP(IDTK(IJK))
        D0(IJK)=QDB(IDTK(IJK))
        Z0(IJK)=QZB(IDTK(IJK))
        PHITK(IJK)=QPH(IDTK(IJK))
        THETK(IJK)=ACOS(QCT(IDTK(IJK)))
C Could it be a MUON
        IOBJ=IDTK(IJK)
        IF(IOBJ.NE.0)  THEN
          CALL QMUIDO(IOBJ,IRUN,IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,
     &    XMULT,RAPP,ANG,ISHAD,SUDNT,IDF,IMCF,IER)
          IDFM(IJK)=IDF
        ENDIF
C Look for ECAL associated energy
        IF(KNECAL(IOBJ).NE.0) THEN
          DO 11 JNE=1,KNECAL(IOBJ)
            IEC=KECAL(IOBJ,JNE)
            IDEAS(JNE)=IEC
            ENEAS(JNE)=QE(IEC)
            EASS(IJK)=EASS(IJK)+QE(IEC)
 11       CONTINUE
          NEAS=KNECAL(IOBJ)
          CALL SORTZV(ENEAS,IARSAS,NEAS,1.,1.,0)
          IAS=IDEAS(IARSAS(1))
        ENDIF
C Look for HCAL associated energy
C       IF(IAS.EQ.0) GO TO 101
        IF(KNHCAL(IOBJ).EQ.0) GO TO 101
C       KTN1=KTN(IAS)
C       CALL EBINIT(IRET)
C       CALL EBNEUT(KTN1,IOPT1,IFOT1,PHOT1,CORE1,IER1)
C       IF( (IFOT1(7).EQ.1) .OR. (IFOT1(8).EQ.1.OR.IFOT1(8).EQ.2.OR.
C    &  IFOT1(8).EQ.3.OR.IFOT1(8).EQ.4) ) THEN
          DO 21 IHAD=1,KNHCAL(IOBJ)
            ENHE(IJK)=ENHE(IJK)+QE(KHCAL(IOBJ,IHAD))
            PHHE(IJK)=PHHE(IJK)
     &               +QPH(KHCAL(IOBJ,IHAD))*QE(KHCAL(IOBJ,IHAD))
            THHE(IJK)=THHE(IJK)
     &               +ACOS(QCT(KHCAL(IOBJ,IHAD)))*QE(KHCAL(IOBJ,IHAD))
 21       CONTINUE
          IF(ENHE(IJK).LE.0) ENHE(IJK)=0.001
          PHHE(IJK)=PHHE(IJK)/ENHE(IJK)
          THHE(IJK)=THHE(IJK)/ENHE(IJK)
C Is this hadronic energy associated with the track?
          DPHHE=PHITK(IJK)-PHHE(IJK)
          DTHHE=THETK(IJK)-THHE(IJK)
          V(1)=QX(IOBJ)/QP(IOBJ)
          V(2)=QY(IOBJ)/QP(IOBJ)
          V(3)=QZ(IOBJ)/QP(IOBJ)
C         PRINT *,'V',V(1),V(2),V(3)
          P(1)=QVX(1)
          P(2)=QVY(1)
          P(3)=QVZ(1)
C         PRINT *,'P',P(1),P(2),P(3)
          CALL ECYLND(200.,270.,P,V,PIMPAC)
C         PRINT *,'***PIMPAC***',PIMPAC(1,1),PIMPAC(2,1),PIMPAC(3,1)
          ICRACK=0
          XINE(1)=PIMPAC(1,1)
          XINE(2)=PIMPAC(2,1)
          XINE(3)=PIMPAC(3,1)
          ICRACK=EQEDGE(XINE,DISTAN,PHIEDG)
C         PRINT *,'***ICRACK***',ICRACK,EQEDGE(XINE,DISTAN,PHIEDG)
C         IF(ABS(DPHHE).LE.PHECUT.AND.ABS(DTHHE).LE.THECUT)
C    &    ENHES(IJK)=ENHE(IJK)
          IF(ICRACK.EQ.1)
     &    ENHES(IJK)=ENHE(IJK)
C       ENDIF
 101  CONTINUE
C
C
C Find the most energetic ecalobject NON ASSOCIATED WITH A CHARGED TRACK
      NECO=0
      IECO=KPDIR('ECAL',KRECO)
C     PRINT *,'***IECO***',IECO
 31   IF(IECO.EQ.0) GO TO 41
C     PRINT *,'***KNCHGD***',KNCHGD(IECO)
      IF(KNCHGD(IECO).EQ.0) THEN
C       PRINT *,'***QE***',QE(IECO),ECOMX
        IF(QE(IECO).GE.ECOMX) THEN
          NECO=NECO+1
          IDECO(NECO)=IECO
          ENECO(NECO)=QE(IECO)
        ENDIF
      ENDIF
      IECO=KFOLLO(IECO)
      GO TO 31
 41   CONTINUE
C     PRINT *,'***NECO***',NECO
      IF(NECO.GT.0) THEN
        CALL SORTZV(ENECO,IARSEC,NECO,1.,1.,0)
        DO 51 IJC=1,1
          IDGA(IJC)=IDECO(IARSEC(IJC))
          EGAS(IJC)=ENECO(IARSEC(IJC))
          PHIG(IJC)=QPH(IDGA(IJC))
          THEG(IJC)=ACOS(QCT(IDGA(IJC)))
 51     CONTINUE
C Is this "PHOTON" accepted?
        IF(PHITK(1).GT.QQPI) PHIE=PHITK(1)-QQPI
        IF(PHITK(1).LT.QQPI) PHIE=PHITK(1)+QQPI
C       PRINT *,'***PHITK,PHIG***',PHITK(1),PHIE,PHIG(1)
C       PRINT *,'***THETK,THEG***',THETK(2),THEG(1)
        DPHI=PHIE-PHIG(1)
        DTHE=THETK(2)-THEG(1)
C       PRINT *,'***DPHI,DTHE***',DPHI,DTHE,TPCUT
        IF(ABS(DPHI).LE.TPCUT.AND.ABS(DTHE).LE.TPCUT) THEN
          EGASS=EGAS(1)
          IF(NCH5.EQ.2.AND.KNHCAL(IDGA(1)).NE.0) THEN
C           KTN1=KTN(IDGA(1))
C           CALL EBINIT(IRET)
C           CALL EBNEUT(KTN1,IOPT1,IFOT1,PHOT1,CORE1,IER1)
C           IF( (IFOT1(7).EQ.1).OR.(IFOT1(8).EQ.1.OR.IFOT1(8).EQ.2.
C    &      OR.IFOT1(8).EQ.3.OR.IFOT1(8).EQ.4) ) THEN
              DO 61 IHADG=1,KNHCAL(IDGA(1))
                IHADU=0
                IF(ENHES(2).GT.0.) THEN
                  DO 60 IH=1,KNHCAL(IDTK(2))
                    IF(KHCAL(IDTK(2),IH).EQ.KHCAL(IDGA(1),IHADG))
     &              IHADU=1
 60               CONTINUE
                ENDIF
                IF(IHADU.NE.0) GO TO 61
                ENHG=ENHG+QE(KHCAL(IDGA(1),IHADG))
                PHHG=PHHG
     &              +QPH(KHCAL(IDGA(1),IHADG))*QE(KHCAL(IDGA(1),IHADG))
                THHG=THHG
     &         +ACOS(QCT(KHCAL(IDGA(1),IHADG)))*QE(KHCAL(IDGA(1),IHADG))
 61           CONTINUE
              IF(ENHG.LE.0) ENHG=0.001
              PHHG=PHHG/ENHG
              THHG=THHG/ENHG
C Is this hadronic energy associated with the photon?
              DPHHG=PHIG(1)-PHHG
              DPTHG=THEG(1)-THHG
              V(1)=QX(IDGA(1))/QP(IDGA(1))
              V(2)=QY(IDGA(1))/QP(IDGA(1))
              V(3)=QZ(IDGA(1))/QP(IDGA(1))
C             PRINT *,'V',V(1),V(2),V(3)
              P(1)=QVX(1)
              P(2)=QVY(1)
              P(3)=QVZ(1)
C             PRINT *,'P',P(1),P(2),P(3)
              CALL ECYLND(200.,270.,P,V,PIMPAC)
C             PRINT *,'***PIMPAC***',PIMPAC(1,1),PIMPAC(2,1),PIMPAC(3,1)
              ICRACK=0
              XINE(1)=PIMPAC(1,1)
              XINE(2)=PIMPAC(2,1)
              XINE(3)=PIMPAC(3,1)
              ICRACK=EQEDGE(XINE,DISTAN,PHIEDG)
C             PRINT *,'***ICRACK***',ICRACK,EQEDGE(XINE,DISTAN,PHIEDG)
              IF(ICRACK.EQ.1)
     &        EGAH=ENHG
C             IF(ABS(DPHHG).LE.PHGCUT.AND.ABS(DTHHG).LE.THGCUT)
C    &        EGAH=ENHG
C           ENDIF
          ENDIF
        ENDIF
      ENDIF
C Acolinearity of the 2 most energetic tracks
      ACOL=(QX(IDTK(1))*QX(IDTK(2))+QY(IDTK(1))*QY(IDTK(2))
     &+QZ(IDTK(1))*QZ(IDTK(2)))/(QE(IDTK(1))*QE(IDTK(2)))
      IF(ACOL.LT.-.999999) THEN
        ACOL=0.
      ELSEIF(ACOL.GT..999999) THEN
        ACOL=QQPI
      ELSE
        ACOL=QQPI-ACOS(ACOL)
      ENDIF
      ACOL=ACOL*180./QQPI
C C.M.S angle
      IDTK1=IDTK(1)
      IDTK2=IDTK(2)
      IF(ABS(PTK(1)).GT.ELEP.OR.DPTK(1).GT.0.5*ELEP.OR.
     &DPTK(1)/ABS(PTK(1)).GT.0.5) THEN
        IDTK1=IDTK(2)
        IDTK2=IDTK(1)
        IF(ABS(PTK(2)).GT.ELEP.OR.DPTK(2).GT..5*ELEP.OR.
     &  DPTK(2)/ABS(PTK(2)).GT..5) THEN
         IDTK1=IDTK(1)
         IDTK2=IDTK(2)
        ENDIF
      ENDIF
      THTK1 = ( ACOS( QCT(IDTK1)) * 180.) / QQPI
      THTK2 = ( ACOS( QCT(IDTK2)) * 180.) / QQPI
      IF( QCH( IDTK1) .GT. 0. .AND. QCH( IDTK2) .LT. 0.) THEN
        CSTAR  = COS( 0.5 * (QQPI/180.) * (THTK2+180.-THTK1) )
     &         / cos( 0.5 * (qqpi/180.) * (thtk2-180.+thtk1)  )
      ELSEIF( QCH(IDTK1) .LT. 0. .AND. QCH(IDTK2) .GT. 0.) THEN
        CSTAR  = COS( 0.5 * (QQPI/180.) * (THTK1+180.-THTK2) )
     &         / COS( 0.5 * (QQPI/180.) * (THTK1-180.+THTK2) )
      ELSE
c   if same sign tracks, keep the sign of the most energetic one
        IF( QCH( IDTK1) .GT. 0.) THEN
          CSTAR  = COS( 0.5 * (QQPI/180.) * (THTK2+180.-THTK1) )
     &           / COS( 0.5 * (QQPI/180.) * (THTK2-180.+THTK1) )
        ELSE
          CSTAR  = COS( 0.5 * (QQPI/180.) * (THTK1+180.-THTK2) )
     &           / COS( 0.5 * (QQPI/180.) * (THTK1-180.+THTK2) )
        ENDIF
      ENDIF
C
 999  CONTINUE
C
      RETURN
      END
      SUBROUTINE CTKIN(BCTH,CSTARM,ACOLM,PHIM)
C
C-----------------------------------------------------------------------
C Cos(theta) of Electron FOR MC DATA
C
C   Author         E. Locci     89/09/15
C
C   Called from BHASEL
C
C   Input argument :
C
C   Output argument : BCTH    cos(theta) of l+/l-
C                     CSTARM  cos(theta) in c.m.
C                     ACOLM   acollinearity
C                     PHIM    phi of l+/l-
C
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      COMMON/JBEMU/BCTHMX,ACOC,NHTPMN,D0CMN,Z0CMN,D0CMX,Z0CMX,
     &NCHC,NCHCMN,ENCHMX,EC0MX,TPCUT,PHGCUT,THGCUT,PHECUT,THECUT
      COMMON/EMUPA/CSTAR,ACOL,ELEP,IDTK(2),PTK(2),DPTK(2),D0(2),Z0(2),
     &PHITK(2),THETK(2),IDFM(2),EASS(2),ENHES(2),EGASS,EGAH
      DIMENSION ITYP(3),PVEC(4,4),BCTH(3),X(16),PHIM(2)
      INCLUDE '/aleph/phy/qmacro.inc'
C-----------------------------------------------------------------------
C
      CSTARM=1.
      ACOLM=180.
C
C Initialization
      DO 2 I=1,4
        DO 1 J=1,4
 1      PVEC(I,J)=0.
 2    CONTINUE
      CALL VZERO(BCTH,3)
      CALL VZERO(ITYP,3)
C
C Find L+ and L- from bank KINE

      NFKIN=NAMIND('FKIN')
      IF(NFKIN.LE.0) GO TO 999
      JFKIN=IW(NFKIN)
      IF(JFKIN.LE.0)   GO TO 999
      NUMK=LROWS(JFKIN)
C            loop on MC tracks
      DO 10 ITK=1,2
      KFKIN=KROW(JFKIN,ITK)
      ITYP(ITK)=IW(KFKIN+5)
C             4-vectors
      PVEC(ITK,1)=RW(KFKIN+1)
      PVEC(ITK,2)=RW(KFKIN+2)
      PVEC(ITK,3)=RW(KFKIN+3)
      PVEC(ITK,4)=RW(KFKIN+4)
C             cos theta
      FNORM=SQRT(PVEC(ITK,1)**2+PVEC(ITK,2)**2+PVEC(ITK,3)**2)
      BCTH(ITK)= PVEC(ITK,3)/FNORM
      APHIM=ATAN2(PVEC(ITK,2),PVEC(ITK,1))*180./QQPI
      PHIM(ITK)=APHIM
      IF(APHIM.LT.0.)  PHIM(ITK)=APHIM+360.
 10   CONTINUE
          THTK1 = ( ACOS( BCTH(1)) * 180.) / QQPI
          THTK2 = ( ACOS( BCTH(2)) * 180.) / QQPI
          IF( ITYP(1) .EQ. 2. .AND. ITYP(2) .EQ. 3.) THEN
            CSTARM  = COS( 0.5 *(THTK2+180.-THTK1) * QQPI/180. )
     &             / ABS( COS( 0.5 *(THTK2-180.+THTK1) * QQPI/180. ) )
          ELSEIF( ITYP(1) .EQ. 3. .AND. ITYP(2) .EQ. 2.) THEN
            CSTARM  = COS( 0.5 *(THTK1+180.-THTK2) * QQPI/180. )
     &             / ABS( COS( 0.5 *(THTK1-180.+THTK2) *QQPI/180. ) )
          ELSE
c   if same sgn tracks, keep the sgn of the most energetic one
            IF( ITYP(1) .EQ. 2.) THEN
            CSTARM  = COS( 0.5 *(THTK2+180.-THTK1) * QQPI/180. )
     &             / ABS( COS( 0.5 *(THTK2-180.+THTK1) * QQPI/180. ) )
            ELSE
              CSTARM  = COS( 0.5 *(THTK1+180.-THTK2) *QQPI/180. )
     &               / ABS( COS( 0.5 *(THTK1-180.+THTK2) * QQPI/180. ) )
            ENDIF
          ENDIF
C
      E1=SQRT(PVEC(1,1)**2+PVEC(1,2)**2+PVEC(1,3)**2+PVEC(1,4)**2)
      E2=SQRT(PVEC(2,1)**2+PVEC(2,2)**2+PVEC(2,3)**2+PVEC(2,4)**2)
      ACOL=(PVEC(1,1)*PVEC(2,1)+PVEC(1,2)*PVEC(2,2)+
     &PVEC(1,3)*PVEC(2,3))/(E1*E2)
      IF(ACOL.LT.-.999999) THEN
      ACOL=0.
      ELSEIF(ACOL.GT..999999) THEN
      ACOL=QQPI
      ELSE
      ACOL=QQPI-ACOS(ACOL)
      ENDIF
C Cut on acolinearity
      ACOL=ACOL*QQRADP
      ACOLM=ACOL
C
 999  CONTINUE
C
      RETURN
      END
