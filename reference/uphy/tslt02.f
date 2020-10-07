      SUBROUTINE TSLT02( TAUEVT , BITPATTERN , DEBUGLEVEL , RETURNCODE)
C----------------------------------------------------------------------
C
C    Author   :- Laurent Duflot         5-FEB-1992
C
C                             Modified  1-Nov-1992  for ENFLW114
C                                                       test KEEVES
C
C    Major modification :
C              - G.Ganis, M.Girone, L.Rolandi    12-JUL-1993
C                                       New criteria for Bhabha and
C                                       dimuon rejection
C              - G.Ganis, M.Girone, A.Gregorio, M.P. Casado
C                                                22-JUL-1994
C                                       Introduction of energy thresholds
C                                       Possibility of further cuts
C                                       against dimuons and cosmics
C              - G.Ganis, M.Girone, A.Gregorio
C                                                30-JAN-1995
C                                       Possibility of further cuts
C                                       against hadronic events
C
C    Inputs:   DebugLevel : if > 0 fill common /DATACUT/ with statistics
C                           on cut effects; to print it, add the followi
C                           line in your QUTERM
C                                                CALL TSLPRS( LUN )
C                           where LUN is the unit for the output printou
C
C    Parameters are passed through the data card TSLT
C
C    *      c_the   phot    had   max    acol    D_pt   E_min  m_inv
C    *     ch trk   thres  thres  n_ch   cut     cut     cut   cut
C    TSLT   0.95     1.0    1.5     8.   160.    .066    .35   0.8
C    *      n_ob   op_an   e_lead  E_max_1 E_max_2  D_min  max_mu
C    *      cut     cut     cut      cut     cut     cut     cut
C            40.   0.25     1.6      1.4     1.6      6.     0.9
C
C    The card 'OLDC' will switch to the old energy cut against dimuon
C    ( in TSLT max_mu cut has to be put to 1.8 ) and switch off the
C    new cut against cosmics.
C
C    Outputs: tauevt : LOGICAL set to TRUE if the event is selected as a
C                       tau+ tau- event
C
C             BitPattern : INTEGER variable. bit is set to one if the ev
C                           pass the corresponding cut
C
C             ReturnCode : 0     if OK
C                          100   if not accepted by LUMOK
C                          200   if no EFT section filled
C         -
C
C    Libraries required: CERNLIB , ALPHAxxx
C-----------------------------------------------------------------------------

      PARAMETER( NBCUTDATA = 19 )

      LOGICAL   TAUEVT, FIRSTWARNING, FIRSTENTRY, HVOK
      SAVE      FIRSTENTRY, FIRSTWARNING
      LOGICAL   PRESERVEHAD, PRES_BHA, PRES_MUO
      LOGICAL   BLIKE, MLIKE
      LOGICAL   NEWCUTS
      INTEGER   RETURNCODE, DEBUGLEVEL, TRIGGERREJECT
      INTEGER   RUNQUA, MYKEEVES
      INTEGER*4 BITPATTERN,ITAUEVT

      REAL    CUTVALUE(NBCUTDATA)
      SAVE    CUTVALUE
      REAL    MAXCHTKCT, MINPHOTENE, MINHADENE, IMSACUT, IMOPCUT
      REAL    MAXNBTRACK, ACOLINCUT , DELTAPTCUT
      REAL    EDELTAPTCUT, NBNBCUT , OPACUT, IMNNCUT, IM1NCUT
      REAL    ELEADCUT, ETOTBHA1CUT, ETOTBHA2CUT
      REAL    DGAMCRKCUT, XMAXMUCUT
      REAL    NITCUT

      EQUIVALENCE (CUTVALUE(1),MAXCHTKCT)
      EQUIVALENCE (CUTVALUE(2),MINPHOTENE)
      EQUIVALENCE (CUTVALUE(3),MINHADENE)
      EQUIVALENCE (CUTVALUE(4),MAXNBTRACK)
      EQUIVALENCE (CUTVALUE(5),ACOLINCUT)
      EQUIVALENCE (CUTVALUE(6),DELTAPTCUT)
      EQUIVALENCE (CUTVALUE(7),EDELTAPTCUT)
      EQUIVALENCE (CUTVALUE(8),IMSACUT)
      EQUIVALENCE (CUTVALUE(9),IMOPCUT)
      EQUIVALENCE (CUTVALUE(10),NBNBCUT)
      EQUIVALENCE (CUTVALUE(11),OPACUT)
      EQUIVALENCE (CUTVALUE(12),IMNNCUT)
      EQUIVALENCE (CUTVALUE(13),IM1NCUT)
      EQUIVALENCE (CUTVALUE(14),ELEADCUT)
      EQUIVALENCE (CUTVALUE(15),ETOTBHA1CUT)
      EQUIVALENCE (CUTVALUE(16),ETOTBHA2CUT)
      EQUIVALENCE (CUTVALUE(17),DGAMCRKCUT)
      EQUIVALENCE (CUTVALUE(18),XMAXMUCUT)
      EQUIVALENCE (CUTVALUE(19),NITCUT)
C

      INCLUDE '/aleph/phy/qcde.inc'

      DATA FIRSTWARNING / .TRUE. /
      DATA FIRSTENTRY / .TRUE. /
      DATA CUTVALUE /0.95,1.0,1.5,8.,160.,.066,.35,0.8,3.0,
     +               50.,0.3,1.8,3.0,1.6,
     +               1.4, 1.6, 6., 0.9, 5. /

      INCLUDE '/aleph/phy/qmacro.inc'

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IF( FIRSTENTRY ) THEN

C - New or old cuts ( default new )
        NEWCUTS= .TRUE.
        IOLDC= NLINK('OLDC',0)
        IF( IOLDC.NE.0 ) NEWCUTS= .FALSE.

C.. allow to change some cut by DATA card

        FIRSTENTRY = .FALSE.
        ITSLT = NLINK('TSLT',0)
        IF ( ITSLT .GT. 0  ) THEN
          WRITE(6,*) '            '
          WRITE (6,*)
     &      'TSLT02 : cut values taken from DATA cards if > 0 '
          WRITE(6,*) '            '
          DO I  = 1  , NBCUTDATA
            IF ( RW(ITSLT+I) .GT. 0. ) THEN
              CUTVALUE(I) = RW(ITSLT+I)
            ENDIF
          ENDDO
        ENDIF
        WRITE(6,*) '         '
        WRITE(6,*)
     &    '==========================='//
     &    '====================================='
        WRITE(6,*)
     &    '=========               TSLT02'//
     &    '                        =========='
        WRITE(6,*)
     &    '============================='//
     &    '==================================='
        WRITE(6,10000) MAXCHTKCT
        WRITE(6,10010) MINPHOTENE
        WRITE(6,10020) MINHADENE
        WRITE(6,10030) MAXNBTRACK
        WRITE(6,10040) ACOLINCUT
        WRITE(6,10050) DELTAPTCUT
        WRITE(6,10060) EDELTAPTCUT
        WRITE(6,10070) IMSACUT, IMOPCUT
        WRITE(6,10080) NBNBCUT, OPACUT, IM1NCUT, IMNNCUT
        WRITE(6,10100) ELEADCUT
        WRITE(6,10110) ETOTBHA2CUT, DGAMCRKCUT, ETOTBHA1CUT
        WRITE(6,10120) XMAXMUCUT
        WRITE(6,10130) NITCUT
        WRITE(6,*)
     &    '=============================='//
     &    '=================================='
        WRITE(6,*) '         '
10000   FORMAT(1X,'=== charged track cos theta < ',T45,E10.4)
10010   FORMAT(1X,'=== ecal objects energy > ',T45,E10.4)
10020   FORMAT(1X,'=== hcal objects energy > ',T45,E10.4)
10030   FORMAT(1X,'=== max # tracks ',T45,E10.4)
10040   FORMAT(1X,'=== acol cut at ',T45,E10.4,' (degre) ')
10050   FORMAT(1X,'=== delta Pt cut at ',T45,E10.4,' (*Ebeam) ')
10060   FORMAT(1X,'=== if tot energy <',T45,E10.4,' (*Ebeam) ')
10070   FORMAT(1X,'=== Inv mass cut for had preserv ',/,
     &         1X,'===         same:',T45,E10.4,'(GeV/c**2)',/,
     &         1X,'===         oppo:',T45,E10.4,'(GeV/c**2)')
10080   FORMAT(1X,'=== Hadron like : ',/,
     &         1X,'=== number of object : nob1*nob2 < ',T45,E10.4,/,
     &         1X,'=== sum of opening angle <',T45,E10.4,' (rad)',/,
     &         1X,'=== min(m1,m2)<',T45,E10.4,'(GeV/c**2)',/,
     &         1X,'=== if min(nc)>1 ',/,
     &         1X,'===    min(m1,m2)<',T45,E10.4,'(GeV/c**2)')
10100   FORMAT(1X,'=== sum of energy of leading tracks < ',T45,
     &    E10.4,' (*Ebeam) ')
10110   FORMAT(1X,'=== Bhabha like : ',/,
     &         1X,'=== total energy < ',T45,E10.4,' (*Ebeam);',/,
     &         1X,'=== if Dist_gamma_cracks < ',T45,E10.4,' (cm)',/,
     &         1X,'=== total energy < ',T45,E10.4,' (*Ebeam).')
10120   FORMAT(1X,'=== Dimuon like : ',/,
     &         1X,'=== max(emx/2,pl1,pl2) < ',T45,E10.4,' (*Ebeam);')
10130   FORMAT(1X,'=== Cosmics : ',/,
     &         1X,'=== n_itc < ',T45,E10.4)
      ENDIF


      TAUEVT     = .FALSE.
      BITPATTERN = 0
      RETURNCODE = 0
C
C  energy-flow
      IEFLW= NLINK('EFLW', 0)
      IEFLJ= NLINK('EFLJ', 0)
      IF( IEFLW.EQ.0 .AND. IEFLJ.EQ.0 ) THEN
        IF( FIRSTWARNING ) THEN
          WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(6,*) '!!       ********  TAUSLT  **********       !!'
          WRITE(6,*) '!!       **  no EFLW or EFLJ cards **       !!'
          WRITE(6,*) '!!       ** EFT section NOT filled **       !!'
          WRITE(6,*) '!!       **     Action RETURN      **       !!'
          WRITE(6,*) '!!       ****************************       !!'
          WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          FIRSTWARNING= .FALSE.
        ENDIF
        RETURNCODE= 200
        GOTO 999
      ENDIF

C
C  Check HV
      IF( (.NOT.XHVTRG) .AND. (KRUN.GT.4000) ) THEN
        RETURNCODE= 100
      ENDIF
C
C --- Create two tracks for the leadings
      INW1= KVNEW( DUMMY )
      ILD1= KVSAVC( INW1, 'LEAD1', KRECO )
      CALL QSTFLI( ILD1, 1, -1)
C
      INW2= KVNEW( DUMMY )
      ILD2= KVSAVC( INW2, 'LEAD2', KRECO )
      CALL QSTFLI( ILD2, 1, -1)
C
C  Lock unwanted particles
      CALL TSLOCK
C
C  Count Total number of objects and charged tracks
C
      NOBTOT = 0
      NCHTOT = 0
      DO ICHT = KFEFT, KLEFT
        IF( .NOT.XLOCK(ICHT) ) THEN
          NOBTOT = NOBTOT + 1
          IF( KEFOTY(ICHT).LT.3 ) NCHTOT = NCHTOT + 1
        END IF
      ENDDO
C
C  Return if no reconstructed object
C
      IF ( NOBTOT .EQ. 0 ) THEN
        CALL SBIT1(BITPATTERN,32)
        GOTO 999
      ENDIF
C
C Define two hemisphere according to the thrust axis
C
      CALL QJOPTR('EF',' ')
C -- Thrust
      CALL QJTHRU(THRUST,'THRUST',KRECO)
      ITHRUS = KPDIR('THRUST',KRECO)
C -- Two Hemispheres
      CALL QJHEMI('SAME','OPPO',KRECO,ITHRUS,0.)
      ISAME = KPDIR('SAME',KRECO)
      IOPPO = KPDIR('OPPO',KRECO)
C
C find the number of charged tracks , the number of neutral objects
C and the neutral energy in each hemisphere
C
C nchsame : number of charged tracks in hemisphere same
C NNeutSame : number of neutral object  " " "
C eneutsame : neutral energy          "  "          "
C related variables for hemisphere oppo
C
C
C EmaxTrSame : energy of the leading particle in hemisphere same
C ItkMaxTrSame : index of the leading
C EmaxTrOppo : energy of the leading particle in hemisphere oppo
C ItkMaxTrOppo : index of the leading
C
      NCHSAME= 0
      NCHOPPO= 0
      CHASAME= 0.
      CHAOPPO= 0.
      NNEUTSAME= 0
      NNEUTOPPO= 0
      ENEUTSAME= 0.
      ENEUTOPPO= 0.
      ITKMAXTRSAME= 0
      ITKMAXTROPPO= 0
      EMAXTRSAME= 0.
      EMAXTROPPO= 0.
      DO ITK = KFEFT, KLEFT
        IF( .NOT.XLOCK(ITK) ) THEN
          IF( QDOT3(ITHRUS,ITK).GT.0. ) THEN
            IF( KEFOTY(ITK).GT.3 ) THEN
              NNEUTSAME = NNEUTSAME + 1
              ENEUTSAME = ENEUTSAME + QE(ITK)
            ELSE IF( KEFOTY(ITK).LE.2 ) THEN
              NCHSAME = NCHSAME  + 1
              CHASAME = CHASAME  + QCH(ITK)
              IF( QE(ITK).GT.EMAXTRSAME ) THEN
                EMAXTRSAME = QE(ITK)
                ITKMAXTRSAME = ITK
              END IF
            ENDIF
          ELSE
            IF( KEFOTY(ITK).GT.3 ) THEN
              NNEUTOPPO = NNEUTOPPO + 1
              ENEUTOPPO = ENEUTOPPO + QE(ITK)
            ELSE IF( KEFOTY(ITK).LE.2 ) THEN
              NCHOPPO = NCHOPPO + 1
              CHAOPPO = CHAOPPO + QCH(ITK)
              IF ( QE(ITK).GT.EMAXTROPPO ) THEN
                EMAXTROPPO = QE(ITK)
                ITKMAXTROPPO = ITK
              END IF
            ENDIF
          END IF
        END IF
      ENDDO
C
C   Calculate cos(theta)* and acolinearity
C
      IF( ISAME.GT.0 .AND. IOPPO.GT.0 ) THEN
        IF( QP(ISAME).GT.0 .AND. QP(IOPPO).GT.0 ) THEN
          CTHE1= MAX( MIN( QCT(ISAME),.9999999 ),-.9999999 )
          CTHE2= MAX( MIN( QCT(IOPPO),.9999999 ),-.9999999 )
          THET1 = ACOS( CTHE1 )
          THET2 = ACOS( CTHE2 )
          IF( THET1.LT.0. )   THET1= -THET1
          IF( THET1.GT.QQPI ) THET1= 2.*QQPI - THET1
          IF( THET2.LT.0. )   THET2= -THET2
          IF( THET2.GT.QQPI ) THET2= 2.*QQPI - THET2
          THEM= THET1
          THEP= THET2
          IF( CHAOPPO.LT.0. ) THEN
            THEM= THET2
            THEP= THET1
          END IF
C
          CTEST = COS(.5*(THEM+QQPI-THEP))/COS(.5*(THEM-QQPI+THEP))
C
          ACOLIN = QCOSA(ISAME,IOPPO)
          ACOLIN=  MAX( MIN( ACOLIN,.9999999 ),-.9999999 )
          ACOLIN = ACOS( ACOLIN )
          IF( ACOLIN.LT.0. )   ACOLIN= -ACOLIN
          IF( ACOLIN.GT.QQPI ) ACOLIN= 2.*QQPI - ACOLIN
          ACOLIN = ACOLIN*180./QQPI
        END IF
      ELSE
        ACOLIN= 0.
        CTEST= 1.
        IF( IOPPO.NE.0 ) THEN
          IF( QP(IOPPO).NE.0 ) CTEST= QCT(IOPPO)
        END IF
        IF( ISAME.NE.0 ) THEN
          IF( QP(ISAME).NE.0 ) CTEST= QCT(ISAME)
        END IF
      END IF
C
      ESAME = 0.
      EOPPO = 0.
      IF( ISAME.GT.0 ) ESAME = QE(ISAME)
      IF( IOPPO.GT.0 ) EOPPO = QE(IOPPO)
C
      EBEAM = QELEP * 0.5
C
C  Cos(theta)* cut
C
      IF ( ABS(CTEST) .GT. 0.9 ) THEN
        CALL SBIT1(BITPATTERN,4)
        GOTO 999
      END IF
C
C  Acolinerity cut
C
      IF ( ACOLIN .LT. ACOLINCUT ) THEN
        CALL SBIT1(BITPATTERN,5)
        GOTO 999
      END IF
C
C  Require at least 1 charged track per hemisphere
C
      IF ( MIN(NCHSAME,NCHOPPO).EQ.0 ) THEN
        CALL SBIT1( BITPATTERN,3)
        GOTO 999
      ENDIF
C
C  Require at most 8 charged tracks
C
      IF ( NCHTOT .LT. 2 .OR. NCHTOT .GT. MAXNBTRACK ) THEN
        CALL SBIT1(BITPATTERN,1)
        GOTO 999
      ENDIF
C
C  Identification ....
C
C  Flag bhabhalike ...
      CALL BHALIKE( BLIKE )
C  ... and dimuonlike events
      CALL MUOLIKE( ILD1, ILD2, ITKMAXTRSAME, ITKMAXTROPPO, MLIKE )
C
C  Calculate opening angle
C
C  OpanMaxSame : opening angle in hemisphere isame
C  OpanMaxOppo : idem for hemisphere oppo
C
      OPANMAXSAME = 0.
      OPANMAXOPPO = 0.
      DO ITK = KFEFT, KLEFT
        IF( .NOT.XLOCK(ITK) ) THEN
          IF( KEFOTY( ITK ).LE.2 ) THEN
            IF ( QDOT3(ITHRUS,ITK).GT.0. ) THEN
              DO ITKS = KFEFT, KLEFT
                IF( .NOT.XLOCK(ITKS) ) THEN
                  IF( KEFOTY( ITKS ).LE.2 ) THEN
                    IF ( QDOT3(ITKS,ITHRUS).GT.0. .AND. ITK.NE.ITKS )
     &                THEN
                      IF ( ABS(P_ACOS(QCOSA(ITK,ITKS))).GT.OPANMAXSAME )
     &                  THEN
                        OPANMAXSAME = ABS(P_ACOS(QCOSA(ITK,ITKS)))
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ELSE
              DO ITKO = KFEFT, KLEFT
                IF( .NOT.XLOCK(ITKO) ) THEN
                  IF( KEFOTY( ITKO ).LE.2 ) THEN
                    IF ( QDOT3(ITKO,ITHRUS).LE.0. .AND. ITK.NE.ITKO )
     &                THEN
                      IF ( ABS(P_ACOS(QCOSA(ITK,ITKO))).GT.OPANMAXOPPO )
     &                  THEN
                        OPANMAXOPPO = ABS(P_ACOS(QCOSA(ITK,ITKO)))
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C  Against gamma gamma events
C
      IF( ABS(QPT(ISAME)-QPT(IOPPO)).LT.DELTAPTCUT * EBEAM .AND.
     &   (QE(ISAME)+QE(IOPPO)) .LT. EDELTAPTCUT * EBEAM )
     &   CALL SBIT1(BITPATTERN,6)
C
C  Against hadronic events
C
      QMSA=QM(ISAME)
      QMOP=QM(IOPPO)
      PRESERVEHAD =
     &  (NCHSAME.EQ.1.AND.QMSA.LT.IMSACUT.AND.QMOP.LT.IMOPCUT) .OR.
     &  (NCHOPPO.EQ.1.AND.QMOP.LT.IMSACUT.AND.QMSA.LT.IMOPCUT)
C
      IF ( (NCHSAME+NNEUTSAME)*(NCHOPPO+NNEUTOPPO).GE.NBNBCUT  ) CALL
     &    SBIT1( BITPATTERN,7)
C
      IF ( OPANMAXSAME+OPANMAXOPPO.GT.OPACUT)
     &    CALL SBIT1( BITPATTERN,8)
C
      IF( MIN(NCHSAME,NCHOPPO).GT.1 ) THEN
        IF( MIN(QMSA,QMOP).GT. IMNNCUT ) CALL SBIT1( BITPATTERN,15)
      ELSE IF( MIN(NCHSAME,NCHOPPO).LE.1 ) THEN
        IF( MIN(QMSA,QMOP).GT. IM1NCUT ) CALL SBIT1( BITPATTERN,15)
      END IF
C
C  Now dileptons
C
      IF ( EMAXTRSAME+EMAXTROPPO.GT. ELEADCUT * EBEAM ) CALL
     &  SBIT1( BITPATTERN,9)
C
C  Min. dist of possible gammas from ECAL cracks
C
      CALL DECAL( ITKMAXTRSAME, 1, DC1, DC2, JERR )
      DGCRKSAME= MIN( ABS(DC1), ABS(DC2) )
      CALL DECAL( ITKMAXTROPPO, 1, DC1, DC2, JERR )
      DGCRKOPPO= MIN( ABS(DC1), ABS(DC2) )
C
      DGCRKMIN= MIN( DGCRKSAME, DGCRKOPPO )
      IF( DGCRKMIN.LT.DGAMCRKCUT ) CALL SBIT1( BITPATTERN,10)
C
      PRES_BHA =  .NOT.BLIKE
      PRES_MUO =  .NOT.MLIKE
C
      CALL ENEMAX( EREC, ECP1, ECP2, XMAXMU )
C
C  Energy cuts against Bhabha's
C
      EMAX= MAX( EREC, ECP1+ECP2 )
      IF ( EMAX .GT. ETOTBHA1CUT ) CALL SBIT1( BITPATTERN,11)
      IF ( EMAX .GT. ETOTBHA2CUT ) CALL SBIT1( BITPATTERN,12)
C
C  Energy cut against Dimuons
C
      IF( NEWCUTS ) THEN
        IF ( XMAXMU .GT. XMAXMUCUT ) CALL SBIT1( BITPATTERN,13)
      ELSE
        IF ( EREC .GT. XMAXMUCUT  ) CALL SBIT1( BITPATTERN,13)
      END IF
C
C  Last, for cosmic rejection require at least one track
C  with |d0|<1 and |z0|<5, and n_itc > 4 if 1-1 and ...
      IC1= 0
      DO ITK= KFEFT, KLEFT
        IF( .NOT.XLOCK(ITK) ) THEN
          IF (KEFOTY(ITK) .LE. 2) THEN
            IF ( ABS(QDB(ITK)) .LT. 1 .AND. ABS(QZB(ITK)) .LT. 5  ) THEN
              IC1= 1
              GOTO 100
            ENDIF
          ENDIF
        ENDIF
      ENDDO
  100 CONTINUE
      IC2= 1
      IF( NEWCUTS ) THEN
        IF( NCHSAME.EQ.1 .AND. NCHOPPO.EQ.1 ) THEN
          DP= (QP(ITKMAXTRSAME)-QP(ITKMAXTROPPO))/
     .        (QP(ITKMAXTRSAME)+QP(ITKMAXTROPPO))
          IF( DP .LT. 0.15 ) THEN
            IF( ECP1+ECP2 .LT. 0.6 ) THEN
              ITKS= KEFOLT( ITKMAXTRSAME ) + KFCHT - 1
              ITKO= KEFOLT( ITKMAXTROPPO ) + KFCHT - 1
              NITC= KFRTNI( ITKS ) + KFRTNI( ITKO )
              IF( NITC .LT. NITCUT ) IC2= 0
            END IF
          END IF
        END IF
      END IF
      IF ( IC1*IC2 .EQ. 0 ) CALL SBIT1(BITPATTERN,14)
C
C  set the bit pattern taking into account the preservations
C  and determine if the event is to be declared tau-like
C
      ITAUEVT = BITPATTERN
C
      IF ( PRESERVEHAD ) THEN
        CALL SBIT0(ITAUEVT,7)
        CALL SBIT0(ITAUEVT,8)
        CALL SBIT0(ITAUEVT,15)
      ELSE
        CALL SBIT1(BITPATTERN,20)
      ENDIF
C
      IF ( PRES_BHA ) THEN
        CALL SBIT0(ITAUEVT,11)
        CALL SBIT0(ITAUEVT,12)
      ELSE
        CALL SBIT1(BITPATTERN,21)
        IF( JBIT( ITAUEVT,10).EQ.0 ) CALL SBIT0(ITAUEVT,11)
        IF( JBIT( ITAUEVT,10).NE.0 ) CALL SBIT0(ITAUEVT,12)
      ENDIF
      CALL SBIT0(ITAUEVT,10)
C
      IF ( PRES_MUO ) THEN
        CALL SBIT0(ITAUEVT,13)
      ELSE
        CALL SBIT1(BITPATTERN,22)
      ENDIF
C
      IF ( ITAUEVT .EQ. 0 )   TAUEVT = .TRUE.
C
  999 CONTINUE
C  Decode the bit pattern to fill some looses
      IF ( DEBUGLEVEL .GT. 0 .AND. RETURNCODE .LT. 200 ) CALL
     &  DECODE_BITPATTERN(BITPATTERN)

C Unlock locked objects
      DO I=KFEFT, KLEFT
        IF( XLOCK(I) ) CALL QLUTRK( I )
      ENDDO

      RETURN
      END

      REAL FUNCTION P_ACOS(COSANG)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                 C
C  ACOS but with protection against argument > 1  C
C   or < -1 to get rid of errors messages         C
C                                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      PARAMETER( QQPI=3.141593 )
      REAL COSANG
C
      IF ( COSANG .GT. 1. ) THEN
        P_ACOS = 0.
C        WRITE(6,*) 'cosin was greater than 1 ',cosang,krun,kevt
      ELSEIF ( COSANG .LT. -1. ) THEN
        P_ACOS = QQPI
C        WRITE(6,*) 'cosin was lower than -1 ',cosang,krun,kevt
      ELSE
        P_ACOS = ACOS(COSANG)
      ENDIF
      END

C#######################################################################
      SUBROUTINE DECODE_BITPATTERN(BITPATTERN)
C-----------------------------------------
C
C   Author   :- Laurent Duflot         9-FEB-1992
C   Modified :- Gerardo Ganis         21-JAN-1994
C
C=========================================
C
C   Purpose   : from the bit pattern fill looses with the history of
C               each event
C     (mod)   : Now fills the common /DATACUT/ with cut statistics
C   Inputs    : bit pattern
C   Outputs   : none
C
C=========================================
C +
C Declarations.
C -
      IMPLICIT NONE
      INTEGER      CUTDATA(24)
      COMMON /DATACUT/ CUTDATA
      INTEGER*4  BITPATTERN
      INTEGER IFI
      INTEGER JBIT
      EXTERNAL JBIT
      DATA IFI /1/

C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF( IFI.EQ.1 ) THEN
        CALL VZERO( CUTDATA, 24)
        IFI= 0
      END IF

      CUTDATA(23)=CUTDATA(23)+1

      IF ( JBIT(BITPATTERN,32) .EQ. 0  ) THEN
      ELSE
C - Events with no reconstructed objects
        CUTDATA(22)=CUTDATA(22)+1
        GOTO 999
      ENDIF

      IF ( JBIT(BITPATTERN,4) .EQ. 0  ) THEN
C - Evts passing cos_theta_* cut
        CUTDATA(1)=CUTDATA(1)+1
      ELSE
        GOTO 999
      ENDIF

      IF ( JBIT(BITPATTERN,5) .EQ. 0  ) THEN
C - Evts passing acolinearity cut
        CUTDATA(2)=CUTDATA(2)+1
      ELSE
        GOTO 999
      ENDIF

      IF ( JBIT(BITPATTERN,3) .EQ. 0  ) THEN
C - Evts having at least 1 charged track per hemi
        CUTDATA(3)=CUTDATA(3)+1
      ELSE
        GOTO 999
      ENDIF

      IF ( JBIT(BITPATTERN,1) .EQ. 0  ) THEN
C - Evts having 2<= N_charged_tot <=8
        CUTDATA(4)=CUTDATA(4)+1
      ELSE
        GOTO 999
      ENDIF

      IF ( JBIT(BITPATTERN,6) .EQ. 0  ) THEN
C - Evts passing E_tot and Delta_P_t cuts against gammagamma's
        CUTDATA(5)=CUTDATA(5)+1
      ELSE
        GOTO 999
      ENDIF

      IF ( JBIT(BITPATTERN,20) .EQ. 0  ) THEN
C - Events preserved from hadronic cuts
        CUTDATA(6)=CUTDATA(6)+1
      ELSE
        CUTDATA(11)=CUTDATA(11)+1
        IF ( JBIT(BITPATTERN,7) .EQ. 0  ) THEN
C - Evts passing cut on nobj1*nobj2
          CUTDATA(12)=CUTDATA(12)+1
          IF ( JBIT(BITPATTERN,8) .EQ. 0  ) THEN
C - Evts passing cut on opa1+opa2
            CUTDATA(13)=CUTDATA(13)+1
            IF ( JBIT(BITPATTERN,15) .EQ. 0  ) THEN
C - Evts passing cut on min(m1,m2)
              CUTDATA(24)=CUTDATA(24)+1
              CUTDATA(6)=CUTDATA(6)+1
            ELSE
              GOTO 999
            ENDIF
          ELSE
            GOTO 999
          ENDIF
        ELSE
          GOTO 999
        ENDIF
      ENDIF

      IF ( JBIT(BITPATTERN,9) .EQ. 0  ) THEN
C - Evts passing cut on sum of leading momenta
        CUTDATA(7)=CUTDATA(7)+1
      ELSE
        GOTO 999
      ENDIF
      IF ( JBIT(BITPATTERN,21) .EQ. 0  ) THEN
C - Evts preserved from cuts against Bhabha's
        CUTDATA(8)=CUTDATA(8)+1
      ELSE
        CUTDATA(14)=CUTDATA(14)+1
        IF ( JBIT(BITPATTERN,10) .EQ. 0  ) THEN
C - Evts having the min distance from cracks > 6 cm
          CUTDATA(15)=CUTDATA(15)+1
          IF ( JBIT(BITPATTERN,12) .EQ. 0  ) THEN
C - Evts passing the cut at 1.6*E_beam
            CUTDATA(8)=CUTDATA(8)+1
            CUTDATA(16)=CUTDATA(16)+1
          ELSE
            GOTO 999
          ENDIF
        ELSE
C - Evts having the min distance from cracks < 6 cm
          CUTDATA(17)=CUTDATA(17)+1
          IF ( JBIT(BITPATTERN,11) .EQ. 0  ) THEN
C - Evts passing the cut at 1.4*E_beam
            CUTDATA(8)=CUTDATA(8)+1
            CUTDATA(18)=CUTDATA(18)+1
          ELSE
            GOTO 999
          ENDIF
        ENDIF
      ENDIF

      IF ( JBIT(BITPATTERN,22) .EQ. 0  ) THEN
C - Evts preserved from cuts against dimuon's
        CUTDATA(9)=CUTDATA(9)+1
      ELSE
        CUTDATA(19)=CUTDATA(19)+1
        IF ( JBIT(BITPATTERN,13) .EQ. 0  ) THEN
C - Evts passing the cut at 1.8*E_beam
          CUTDATA(9)=CUTDATA(9)+1
          CUTDATA(20)=CUTDATA(20)+1
        ELSE
          GOTO 999
        ENDIF
      ENDIF

      IF ( JBIT(BITPATTERN,14) .EQ. 0 ) THEN
C - Evts passing the cut against cosmics
        CUTDATA(10)=CUTDATA(10)+1
      ELSE
        GOTO 999
      ENDIF

      CUTDATA(21)=CUTDATA(21)+1

  999 RETURN
      END

      SUBROUTINE MUOLIKE( ILDS, ILDO, ITKMAXS, ITKMAXO, MUO )
C--------------------------------------------------------------------
C! Check if the event is dimuon-like
C  Note: QMUIDO ID flag is stored for later use in common /TSLIDM/
C
C  Input :  itkmaxs    ENFLW number of leading in hemi 'SAME'
C           itkmaxo    ENFLW number of leading in hemi 'OPPO'
C
C  Output:   muo    logical, .TRUE. if the event is dimuon-like
C--------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      LOGICAL  MUO
      INCLUDE '/aleph/phy/qmacro.inc'
C
C --- Initialize
      MUO= .FALSE.
      EBEAM= QELEP/2.
      IF( EBEAM.LT.40. ) EBEAM= 45.625
C
C --- Get muo_id variables: ALPHA index ...
      ITKS= KEFOLT( ITKMAXS ) + KFCHT - 1
      ITKO= KEFOLT( ITKMAXO ) + KFCHT - 1
C
C --- Create two tracks for the leadings
      CALL QVCOPY( ILDS, ITKS )
      CALL QVCOPY( ILDO, ITKO )
C
C - Muon ID for the leading
      CALL QMUIDO( ITKS,KR,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,
     .            XMU,RAP,ANG,ISHAD,SUD,IDFS,IMC,IER)
      CALL SETFLG( ILDS,ITKS,ITKMAXS,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,
     .             XMU,RAP,ANG,ISHAD,SUD,IDFS,IMC)
C --- ... and for 'OPPO'
      CALL QMUIDO( ITKO,KR,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,
     .            XMU,RAP,ANG,ISHAD,SUD,IDFO,IMC,IER)
      CALL SETFLG( ILDO,ITKO,ITKMAXO,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,
     .             XMU,RAP,ANG,ISHAD,SUD,IDFO,IMC)
C
C --- Track index for the hemispheres
      ISAME= KPDIR('SAME', KRECO)
      IOPPO= KPDIR('OPPO', KRECO)
C
C --- Require 2 loose muons ( |idf|>0 ) ...
      IF( ABS(IDFS).GT.0.5 .AND. ABS(IDFO).GT.0.5 ) THEN
        MUO= .TRUE.
C --- ... or 1 loose muon and ene>.9*Ebeam in the opposite side.
      ELSE IF( ABS(IDFS).GT.0.5 .AND. ABS(IDFO).LT. 0.5 ) THEN
        IF( QE( IOPPO ).GT. (0.9*EBEAM) ) MUO= .TRUE.
      ELSE IF( ABS(IDFS).LT.0.5 .AND. ABS(IDFO).GT. 0.5 ) THEN
        IF( QE( ISAME ).GT. (0.9*EBEAM) ) MUO= .TRUE.
      END IF
C
      RETURN
      END

      SUBROUTINE SETFLG( ILD,ITK,ITKEF,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,
     .                       XMU,RAP,ANG,ISHAD,SUD,IDF,IMC)
C--------------------------------------------------------------------
C! Set flags with QMUIDO information
C
C  Input :  QMUIDO outputs
C  Output:  none
C--------------------------------------------------------------------
      INTEGER ILD, ITK, ITKEF, IBE, IBT, IM1, IM2, NEX, NFI, N10, N03,
     .        ISHAD, IDF, IMC
      REAL    XMU, RAP, ANG, SUD
C
      CALL QSTFLI( ILD, 1, ITK)
      CALL QSTFLI( ILD, 2, ITKEF)
      CALL QSTFLI( ILD, 3, IBE)
      CALL QSTFLI( ILD, 4, IBT)
      CALL QSTFLI( ILD, 5, IM1)
      CALL QSTFLI( ILD, 6, IM2)
      CALL QSTFLI( ILD, 7, NEX)
      CALL QSTFLI( ILD, 8, NFI)
      CALL QSTFLI( ILD, 9, N10)
      CALL QSTFLI( ILD,10, N03)
      CALL QSTFLI( ILD,11, ISHAD)
      CALL QSTFLI( ILD,12, IDF)
      CALL QSTFLI( ILD,13, IMC)
C
      CALL QSTFLR( ILD,14, XMU)
      CALL QSTFLR( ILD,15, RAP)
      CALL QSTFLR( ILD,16, ANG)
      CALL QSTFLR( ILD,17, SUD)
C
      RETURN
      END

      SUBROUTINE BHALIKE( BHA )
C--------------------------------------------------------------------
C! Check if the event is Bhabha-like
C
C  Input :  none
C  Output:   bha    logical, .TRUE. if the event is Bhabha-like
C--------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      LOGICAL    BHA
      INCLUDE '/aleph/phy/qmacro.inc'
C
C --- Initialize
      BHA= .FALSE.
      NCHS= 0
      NCHO= 0
      NELS= 0
      NELO= 0
C
C --- Track index for 'SAME'
      ISAME= KPDIR('SAME', KRECO)
C
C --- Count charged tracks and loose electrons
      DO ITK= KFEFT, KLEFT
        IF( .NOT.XLOCK(ITK) ) THEN
          IF( KEFOTY(ITK).LE.2 ) THEN
            CALL ELOOSE( ITK, IDE )
            IF( QDOT3( ITK, ISAME ).GT.0. ) THEN
              NCHS= NCHS+1
              IF( IDE.EQ.1 ) NELS= NELS+1
            ELSE
              NCHO= NCHO+1
              IF( IDE.EQ.1 ) NELO= NELO+1
            END IF
          END IF
        END IF
      END DO
C
C --- Require all good charged tracks be loose electrons
      IF( NCHS.EQ.NELS .AND. NCHO.EQ.NELO ) BHA= .TRUE.
C
      RETURN
      END
      SUBROUTINE ELOOSE(ITK,IFLAG)
C-----------------------------------------------------------------------
C! Electron id receipe
C
C  Input  : ITK     ENFLW track number ( section EF )
C  Output : IFLAG   1 if electron-like, 0 otherwise
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      DIMENSION RMASS(1),QQ(1),RIEXP(1),SIGMA(1)
      DATA RMASS /0.0005/,QQ/1./
      LOGICAL  CHTSIM
      EXTERNAL CHTSIM
      INCLUDE '/aleph/phy/qmacro.inc'
C
      IFLAG= 1

C
C --- Get the ALPHA track index
      ITA = KEFOLT(ITK) + (KFCHT - 1)
C
C --- Criteria depend on the momentum ...
      IF( QP(ITA).GT.5. ) THEN
C ----- Extrapolate to ECAL
        CALL DECAL(ITA,0,DIST1,DIST2,JERR)
C ----- Minimum distance from ECAL cracks
        DA = MIN(ABS(DIST1),ABS(DIST2))
C ----- Transverse estimator
        RT = QEIDRI(ITA,2)
C ----- Longitudinal estimator
        RL = QEIDRI(ITA,3)
        IF( DA.GT.3. .AND. RT.LT.-5. ) THEN
          IF( QP(ITA).LT.41. .OR. ABS(RL).GT.2.5 ) IFLAG= 0
        END IF
      ELSE
C ----- Get dE/dx
        IF( XMCEV .AND. .NOT.CHTSIM(IVERS ) ) THEN
          CALL QDEDXM(ITA,1,RMASS,QQ,RI,NS,TL,RIEXP,SIGMA,IER)
        ELSE
          CALL QDEDX(ITA,1,RMASS,QQ,RI,NS,TL,RIEXP,SIGMA,IER)
        END IF
C ----- Normalize it
        RD= 10000.
        IF(IER.EQ.0.AND.SIGMA(1).NE.0) RD = (RI - RIEXP(1))/SIGMA(1)
        IF( RD .LT. -3. ) IFLAG = 0
      END IF
C
C --- 'Muon' veto
      IF(KEFOTY(ITK) .EQ. 2) IFLAG = 0
C
      END

      SUBROUTINE DECAL(ITK,MODE,DIST1,DIST2,JERR)
C-----------------------------------------------------------------------
C! Computes the distance of the track to ecal module edge
C
C  Input  : ITK        Alpha number of the track
C           MODE       0 charged track
C                      1 photon-like forward ( tangent at i.p. )
C                     -1 photon-like backward (       "        )
C
C  Output : DIST1      distance from ECAL active edge at entrance point
C           DIST2      min. distance from ECAL active edge inside ECAL
C                      ( 5 cm steps )
C           JERR       Error flag ( 0 means OK )
C-----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      PARAMETER( RBARL0=192.8, ZEDCP0=255.4 )
      REAL            PAR(7), POINT(6)
      INTEGER EQEDGE
C
      INCLUDE '/aleph/phy/qmacro.inc'
C
C - Check momentum
      IF( ITK.EQ.0 ) THEN
        WRITE(*,*) ' DECAL - Track not defined -RETURN',KRUN,KEVT
        JERR = 5
        RETURN
      END IF
      IF( QP(ITK).EQ.0. ) THEN
        WRITE(*,*) ' DECAL - Track not defined -RETURN',KRUN,KEVT
        JERR = 5
        RETURN
      END IF
C
C - Computes the entrance position in ecal
      CHTP= 0.
      PXTP= QX(  ITK )
      PYTP= QY(  ITK )
      PZTP= QZ(  ITK )
      IF( MODE.EQ.0 )THEN
        CHTP= QCH( ITK )
        PHI = QFRFP0(ITK)
        X0TP= QDB(ITK)*SIN(PHI)
        Y0TP=-QDB(ITK)*COS(PHI)
        Z0TP= QZB(ITK)
      ELSE
        X0TP= QVXNOM
        Y0TP= QVYNOM
        Z0TP= QVZNOM
      ENDIF
      PTTP= QP(ITK)
C
      C0TP= 1.
      C1TP= 1.
      C2TP= 1.
      IF( PTTP.GT.0. ) THEN
        C0TP= PXTP/PTTP
        C1TP= PYTP/PTTP
        C2TP= PZTP/PTTP
      ELSE
        WRITE(*,*) ' DECAL - Track momenta is zero - RETURN'
        JERR = 4
        RETURN
      END IF
C
C - Photon-like forward: to get it we give to it a momentum of 1 TeV
      IF( MODE.NE.0 ) THEN
        XFACT=1000./PTTP
        PTTP=PTTP*XFACT
        PXTP=PXTP*XFACT
        PYTP=PYTP*XFACT
        PZTP=PZTP*XFACT
C
C - Photon-like backward: change direction
        IF(MODE.EQ.-1) THEN
          PXTP=-PXTP
          PYTP=-PYTP
          PZTP=-PZTP
          C0TP=-C0TP
          C1TP=-C1TP
          C2TP=-C2TP
        ENDIF
      ENDIF
      IF( C0TP.EQ.0. .AND. C1TP.EQ.0. ) THEN
        WRITE(*,*) 'E4XYZ1 - Vanishing P_T - RETURN', KRUN, KEVT
        JERR = 3
        RETURN
      END IF
C
C - Extrapolate to ECAL entrance ( Modified Julia routine; see below )
      DIST1=-10000
      DIST2=-10000
      IF( ABS(CHTP).LT..5 ) CHTP= 1.
      IERR=0
C - Fill inputs
      RP1   = RBARL0
      ZP1   = ZEDCP0
      PAR(1)= X0TP
      PAR(2)= Y0TP
      PAR(3)= Z0TP
      PAR(4)= C0TP
      PAR(5)= C1TP
      PAR(6)= C2TP
      PAR(7)= PTTP
      FIELRC= QMFLD             ! magnetique field picked-up in ALPHA
      IF( FIELRC.EQ.0. ) THEN
        WRITE(*,*) ' DECAL - vanishing magnetic field: set to 15. KG'
        JERR = 2
        FIELRC= 15.
      END IF
C - Extrapolate ( AlephLIB routine )
      CALL AUHCYL(RP1,ZP1,FIELRC,CHTP,PAR,POINT,IERR)
      IF( IERR.EQ.0 ) THEN
        JERR = 1
        RETURN
      ENDIF
C
C - Sometime the point is out of the module: move it 1 cm onward
      POINT(1)=POINT(1)+POINT(4)
      POINT(2)=POINT(2)+POINT(5)
      POINT(3)=POINT(3)+POINT(6)
C
C - Calculate the distance from ECAL active volume ( Alephlib routine )
      ICRACK=EQEDGE(POINT,DIS,IPHIDG)
      DIST1= DIS
C
C - Onward steps ( 5 cm each ) to get the min. dist. inside
      DIST2= -10000.
      DO I=1,10
        XL= (I-1)*5. + 5.
        POINT(1)=POINT(1)+XL*POINT(4)
        POINT(2)=POINT(2)+XL*POINT(5)
        POINT(3)=POINT(3)+XL*POINT(6)
        ICRACK=EQEDGE(POINT,DIS,IPHIDG)
        IF( ABS(DIS).LT.ABS(DIST2) ) DIST2= DIS
      END DO
C
      RETURN
      END

      SUBROUTINE ENEMAX( EMAX, ECP1, ECP2, XMAXMU )
C----------------------------------------------------------------------
C Get energy variable
C
C Output: emax = e1+e2+erad
C         ecpi = ecal pads energy in hemi i
C         xmaxmu= max( emax/2, pl1, pl2 )
C----------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'
      INCLUDE '/aleph/phy/qmacro.inc'
C
C --- Initialize
      EBEAM= QELEP/2.
      IF( EBEAM.LT.40. ) EBEAM= 45.625
      ECP1= 0.
      ECP2= 0.
C
      JNSCL = NLINK('NSCL',0)
C
C --- Index for 'SAME' and 'OPPO'
      ISAME= KPDIR('SAME', KRECO)
      IOPPO= KPDIR('OPPO', KRECO)
C
C --- Reconstructed ECAL energy per emisphere
      IOBJ= KPDIR('ECAL',KRECO)
   10 IF(IOBJ .EQ. 0) GOTO 11
C --- xc: correction factor for ecal saturation ( only data )
      XC= 1.
      IF( KRUN.GT.4000 ) XC= 1. + .00078*QPECEC(IOBJ)
      THETA = QPECTH(IOBJ)
      IF( THETA.GT.0.05770 .AND. THETA.LT.3.08389 .AND. JNSCL.GT.0 )THEN
        IF( QDOT3( IOBJ, ISAME ).GT.0. ) THEN
          ECP1= ECP1 + XC*QPECEC(IOBJ)
        ELSE
          ECP2= ECP2 + XC*QPECEC(IOBJ)
        ENDIF
      ENDIF
      IOBJ= KFOLLO(IOBJ)
      GOTO 10
   11 CONTINUE
      ECP1= ECP1/EBEAM
      ECP2= ECP2/EBEAM
C
C --- Angular variables
      CX1= QCT(ISAME)
      CX2= QCT(IOPPO)
      CX1= MAX( MIN( CX1,.999999 ),-.999999 )
      CX2= MAX( MIN( CX2,.999999 ),-.999999 )
      SX1= SQRT(1.-CX1*CX1)
      SX2= SQRT(1.-CX2*CX2)
      SX12= SX1*CX2+SX2*CX1
C
C --- Energy in the beam pipe
      ERAD= ABS(2*SX12/(SX1+SX2+ABS(SX12)))
C
C --- Recalculated energies
      EAN1= 2*SX2/(SX1+SX2+ABS(SX12))
      EAN2= 2*SX1/(SX1+SX2+ABS(SX12))
C
C --- ENFLW energies
      ENE1= QE(ISAME)
      ENE2= QE(IOPPO)
C
C --- Variables
      EREC= (ENE1+ENE2)/EBEAM
C
C --- Output
      EMAX= EREC + ERAD
C
      ILD1= KPDIR('LEAD1',KRECO)
      ILD2= KPDIR('LEAD2',KRECO)
      IF( ILD1.NE.0 .AND. ILD2.NE.0 ) THEN
        ITK1= KRDFL( ILD1, 1)
        ITK2= KRDFL( ILD2, 1)
        PL1= QP(ITK1)/EBEAM
        PL2= QP(ITK2)/EBEAM
        XMAXMU= MAX( (EREC+ERAD)/2., PL1, PL2 )
      ELSE
        WRITE(*,*) ' ENEMAX - Leading not defined '
      END IF
C
      RETURN
      END

      SUBROUTINE TSLPRS( LUN )
C-----------------------------------------------------------------------
C Routine to print out the content of /DATACUT/ common
C
C-----------------------------------------------------------------------
      INTEGER CUTDATA(24)
      COMMON /DATACUT/ CUTDATA
      CHARACTER*30 CUTNAME(25)
      DATA CUTNAME / ' | cos theta* | < .9          ',
     .               '  acol>160                    ',
     .               ' min(nc1,nc2) > 0             ',
     .               ' 1 < # charged tracks < 9     ',
     .               ' delta Pt cut & tot energy    ',
     .               ' Hadronic cuts                ',
     .               ' Sum of leading energy        ',
     .               ' Bhabha cuts                  ',
     .               ' Dimuon cuts                  ',
     .               ' Cosmics cut                  ',
     .               ' undergoing hadronic cuts     ',
     .               ' nob1*nob2 < cut              ',
     .               ' opa1+opa2 < cut              ',
     .               ' undergoing Bhabha cuts       ',
     .               ' D_gck > cut                  ',
     .               ' tot energy < cut             ',
     .               ' D_gck < cut                  ',
     .               ' tot energy < cut             ',
     .               ' undergoing dimuon cuts       ',
     .               ' max_mu < cut                 ',
     .               ' events have been selected    ',
     .               ' events without rec objects   ',
     .               ' events have XLUMOK=.TRUE.    ',
     .               ' events with >0 rec objects   ',
     .               ' min(m1,m2) < cut             '/

      WRITE(LUN,99)
   99 FORMAT(/,1X,'*-------------------------------------------------*',
     .       /,1X,'*        General statistics from TSLT02           *',
     .       /,1X,'*                                                 *',
     .       /,1X,'*        # evts      Cut description              *')

      WRITE(LUN, 100 ) CUTDATA(23),CUTNAME(23)
      WRITE(LUN, 100 ) CUTDATA(23)-CUTDATA(22),CUTNAME(24)
      DO I=1,10
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)
      END DO

      WRITE(LUN,101)
  101 FORMAT(/,1X,'*-------------------------------------------------*',
     .       /,1X,'*                    Hadronic cuts                *',
     .       /,1X,'*                                                 *',
     .       /,1X,'*        # evts      Cut description              *')

      DO I=11,13
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)
      END DO
      WRITE(LUN, 100 ) CUTDATA(24),CUTNAME(25)

      WRITE(LUN,102)
  102 FORMAT(/,1X,'*-------------------------------------------------*',
     .       /,1X,'*                    Bhabha cuts                  *',
     .       /,1X,'*                                                 *',
     .       /,1X,'*        # evts      Cut description              *')

      DO I=14,18
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)
      END DO

      WRITE(LUN,103)
  103 FORMAT(/,1X,'*-------------------------------------------------*',
     .       /,1X,'*                    Dimuon cuts                  *',
     .       /,1X,'*                                                 *',
     .       /,1X,'*        # evts      Cut description              *')

      DO I=19,20
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)
      END DO

      WRITE(LUN,104)
  104 FORMAT(/,1X,'*-------------------------------------------------*',
     .       /,1X,'*      Final statistics from TSLT02               *',
     .       /,1X,'*                                                 *')

      WRITE(LUN, 100 ) CUTDATA(23),CUTNAME(23)
      WRITE(LUN, 100 ) CUTDATA(21),CUTNAME(21)
      WRITE(LUN, 100 ) CUTDATA(22),CUTNAME(22)

      WRITE(LUN,105)
  105 FORMAT(1X,'*-------------------------------------------------*',/)


  100 FORMAT(2X,I13,5X,30A)

      RETURN
      END

      SUBROUTINE TSLOCK
C------------------------------------------------------------------------
C  Lock unwanted objects ...
C------------------------------------------------------------------------
      INCLUDE '/aleph/phy/qcde.inc'

      REAL    MAXCHTKCT, MINPHOTENE, MINHADENE
      SAVE    MAXCHTKCT, MINPHOTENE, MINHADENE
      LOGICAL FIRSTENTRY
      DATA    MAXCHTKCT  /0.95/
      DATA    MINPHOTENE /1.0/
      DATA    MINHADENE  /1.5/
      DATA    FIRSTENTRY /.TRUE./

      INCLUDE '/aleph/phy/qmacro.inc'

      IF ( FIRSTENTRY ) THEN
C.. allow to change some cut by DATA card
        FIRSTENTRY = .FALSE.
        ITSLT = NLINK('TSLT',0)
        IF( ITSLT .GT. 0  ) THEN
          IF( RW(ITSLT+1) .GT. 0. ) MAXCHTKCT= RW(ITSLT+1)
          IF( RW(ITSLT+2) .GT. 0. ) MINPHOTENE= RW(ITSLT+2)
          IF( RW(ITSLT+3) .GT. 0. ) MINHADENE= RW(ITSLT+3)
        END IF
      END IF

C --- To exclude SiCal Objects
      JNSCL = NLINK('NSCL',0)

      DO I=KFEFT,KLEFT
        IF( KEFOTY(I) .LE. 2) THEN
C --- Charged tracks at low angle
          IF (ABS(QCT(I)) .GT. MAXCHTKCT ) CALL QLTRK(I)
        ENDIF
        IF( KEFOTY(I) .EQ. 4  .OR.
     +      KEFOTY(I) .EQ. 7  .OR.
     +      KEFOTY(I) .EQ. 8 ) THEN
C --- Ecal, Lcal, SiCal reconstructed objects below threshold
          IF ( QE(I) .LT. MINPHOTENE ) CALL QLTRK(I)
        ENDIF
        IF( KEFOTY(I) .EQ. 5 ) THEN
C --- Hcal reconstructed objects below threshold
          IF ( QE(I) .LT. MINHADENE ) CALL QLTRK(I)
        ENDIF
        IF( KEFOTY(I) .EQ. 8 ) THEN
C --- Sical objects ( if requested )
          IF ( JNSCL .GT. 0 ) CALL QLTRK(I)
        ENDIF
      ENDDO

      RETURN
      END
