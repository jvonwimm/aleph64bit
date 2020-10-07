      SUBROUTINE TSLT01( TAUEVT , BITPATTERN , DEBUGLEVEL , RETURNCODE) TSLT01 2
C---------------------------------------------------------------------- TSLT01 3
C!  -                                                                   TSLT01 4
C!                                                                      TSLT01 5
C!   Author   :- Laurent Duflot         5-FEB-1992                      TSLT01 6
C!                                                                      TSLT01 7
C!                            Modified  1-Nov-1992  for ENFLW114        TSLT01 8
C!                                                      test KEEVES     TSLT01 9
C!                                                                      TSLT0110
C!   Major modification :                                               TSLT0111
C!             - G.Ganis, M.Girone, L.Rolandi    12-JUL-1993            TSLT0112
C!                                      New criteria for Bhabha and     TSLT0113
C!                                      dimuon rejection                TSLT0114
C!             - G.Ganis, M.Girone, L.Rolandi    XX-JAN-1994            TSLT0115
C!                                                                      TSLT0116
C!   Inputs:   DebugLevel : if > 0 fill common /DATACUT/ with statisticsTSLT0117
C!                          on cut effects; to print it, add the followiTSLT0118
C!                          line in your QUTERM                         TSLT0119
C!                                               CALL TSLPRS( LUN )     TSLT0120
C!                          where LUN is the unit for the output printouTSLT0121
C!                                                                      TSLT0122
C!   Outputs: tauevt : LOGICAL set to TRUE if the event is selected as aTSLT0123
C!                      tau+ tau- event                                 TSLT0124
C!                                                                      TSLT0125
C!            BitPattern : INTEGER variable. bit is set to one if the evTSLT0126
C!                          pass the corresponding cut                  TSLT0127
C!                                                                      TSLT0128
C!            ReturnCode : 0     if OK                                  TSLT0129
C!                         100   if not accepted by LUMOK               TSLT0130
C!                         200   if no EFT section filled               TSLT0131
C!        -                                                             TSLT0132
C!                                                                      TSLT0133
C!   Libraries required: CERNLIB , ALPHAxxx                             TSLT0134
C!                                                                      TSLT0135
                                                                        TSLT0136
      PARAMETER( NBCUTDATA = 11 )                                       TSLT0137
                                                                        TSLT0138
      LOGICAL   TAUEVT, FIRSTWARNING, FIRSTENTRY, HVOK, COSMIC          TSLT0139
      SAVE      FIRSTENTRY, FIRSTWARNING                                TSLT0140
      LOGICAL   PRESERVEHAD, PRES_BHA, PRES_MUO                         TSLT0141
      LOGICAL   BLIKE, MLIKE                                            TSLT0142
      INTEGER   RETURNCODE, DEBUGLEVEL, TRIGGERREJECT                   TSLT0143
      INTEGER   RUNQUA, MYKEEVES                                        TSLT0144
      INTEGER*4 BITPATTERN,ITAUEVT                                      TSLT0145
                                                                        TSLT0146
      REAL    CUTVALUE(NBCUTDATA)                                       TSLT0147
      SAVE    CUTVALUE                                                  TSLT0148
      REAL    MAXNBTRACK, ACOLINCUT , DELTAPTCUT                        TSLT0149
      REAL    EDELTAPTCUT, NBNBCUT , OPACUT                             TSLT0150
      REAL    ELEADCUT, ETOTBHA1CUT, ETOTBHA2CUT                        TSLT0151
      REAL    DGAMCRKCUT, ETOTMUOCUT                                    TSLT0152
                                                                        TSLT0153
      EQUIVALENCE (CUTVALUE(1),MAXNBTRACK)                              TSLT0154
      EQUIVALENCE (CUTVALUE(2),ACOLINCUT)                               TSLT0155
      EQUIVALENCE (CUTVALUE(3),DELTAPTCUT)                              TSLT0156
      EQUIVALENCE (CUTVALUE(4),EDELTAPTCUT)                             TSLT0157
      EQUIVALENCE (CUTVALUE(5),NBNBCUT)                                 TSLT0158
      EQUIVALENCE (CUTVALUE(6),OPACUT)                                  TSLT0159
      EQUIVALENCE (CUTVALUE(7),ELEADCUT)                                TSLT0160
      EQUIVALENCE (CUTVALUE(8),ETOTBHA1CUT)                             TSLT0161
      EQUIVALENCE (CUTVALUE(9),ETOTBHA2CUT)                             TSLT0162
      EQUIVALENCE (CUTVALUE(10),DGAMCRKCUT)                             TSLT0163
      EQUIVALENCE (CUTVALUE(11),ETOTMUOCUT)                             TSLT0164
C                                                                       TSLT0165
                                                                        TSLT0166
                                                                        TSLT0167
      INCLUDE '/aleph/phy/qcde.inc'
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,  TSLT0169
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,         TSLT0170
     +          JEVEES=12,JEVETE=13,LEVEHA=13)                          TSLT0171
                                                                        TSLT0172
                                                                        TSLT0173
      DATA FIRSTWARNING / .TRUE. /                                      TSLT0174
      DATA FIRSTENTRY / .TRUE. /                                        TSLT0175
      DATA CUTVALUE /8.,160.,.066,.35,75.,0.25,1.6,                     TSLT0176
     +               1.4, 1.6, 6., 1.8 /                                TSLT0177
                                                                        TSLT0178
      INCLUDE '/aleph/phy/qmacro.inc'
                                                                        TSLT0180
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ TSLT0181
                                                                        TSLT0182
      IF ( FIRSTENTRY ) THEN                                            TSLT0183
                                                                        TSLT0184
C.. allow to change some cut by DATA card                               TSLT0185
                                                                        TSLT0186
        FIRSTENTRY = .FALSE.                                            TSLT0187
        ITSLT = NLINK('TSLT',0)                                         TSLT0188
        IF ( ITSLT .GT. 0  ) THEN                                       TSLT0189
          WRITE(6,*) '            '                                     TSLT0190
          WRITE (6,*)                                                   TSLT0191
     &      'TAUSLT : cut values taken from DATA cards if > 0 '         TSLT0192
          WRITE(6,*) '            '                                     TSLT0193
          DO I  = 1  , NBCUTDATA                                        TSLT0194
            IF ( RW(ITSLT+I) .GT. 0. ) THEN                             TSLT0195
              CUTVALUE(I) = RW(ITSLT+I)                                 TSLT0196
            ENDIF                                                       TSLT0197
          ENDDO                                                         TSLT0198
        ENDIF                                                           TSLT0199
        WRITE(6,*) '         '                                          TSLT0100
        WRITE(6,*)                                                      TSLT0101
     &    '==========================='//                               TSLT0102
     &    '====================================='                       TSLT0103
        WRITE(6,*)                                                      TSLT0104
     &    '=========               TAUSLT'//                            TSLT0105
     &    '                        =========='                          TSLT0106
        WRITE(6,*)                                                      TSLT0107
     &    '============================='//                             TSLT0108
     &    '==================================='                         TSLT0109
        WRITE(6,10000) MAXNBTRACK                                       TSLT0110
        WRITE(6,10010) ACOLINCUT                                        TSLT0111
        WRITE(6,10020) DELTAPTCUT                                       TSLT0112
        WRITE(6,10030) EDELTAPTCUT                                      TSLT0113
        WRITE(6,10040) NBNBCUT                                          TSLT0114
        WRITE(6,10050) OPACUT                                           TSLT0115
        WRITE(6,10060) ELEADCUT                                         TSLT0116
        WRITE(6,10070) ETOTBHA2CUT, DGAMCRKCUT, ETOTBHA1CUT             TSLT0117
        WRITE(6,10080) ETOTMUOCUT                                       TSLT0118
        WRITE(6,*)                                                      TSLT0119
     &    '=============================='//                            TSLT0120
     &    '=================================='                          TSLT0121
        WRITE(6,*) '         '                                          TSLT0122
10000   FORMAT(1X,'=== max # tracks ',T45,E10.4)                        TSLT0123
10010   FORMAT(1X,'=== acol cut at ',T45,E10.4,' (degre) ')             TSLT0124
10020   FORMAT(1X,'=== deta Pt cut at ',T45,E10.4,' (*Ebeam) ')         TSLT0125
10030   FORMAT(1X,'=== if tot energy <',T45,E10.4,' (*Ebeam) ')         TSLT0126
10040   FORMAT(1X,'=== number of object : nob1*nob2 < ',T45,E10.4)      TSLT0127
10050   FORMAT(1X,'=== sum of opening angle <',T45,E10.4,' (rad)')      TSLT0128
10060   FORMAT(1X,'=== sum of energy of leading tracks < ',T45,         TSLT0129
     &    E10.4,' (*Ebeam) ')                                           TSLT0130
10070   FORMAT(1X,'=== Bhabha like : ',/,                               TSLT0131
     &         1X,'=== total energy < ',T45,E10.4,' (*Ebeam);',/,       TSLT0132
     &         1X,'=== if Dist_gamma_cracks < ',T45,E10.4,' (cm)',      TSLT0133
     &         1X,'=== total energy < ',T45,E10.4,' (*Ebeam).')         TSLT0134
10080   FORMAT(1X,'=== Dimuon like : ',/,                               TSLT0135
     &         1X,'=== total energy < ',T45,E10.4,' (*Ebeam);')         TSLT0136
      ENDIF                                                             TSLT0137
                                                                        TSLT0138
                                                                        TSLT0139
      TAUEVT     = .FALSE.                                              TSLT0140
      BITPATTERN = 0                                                    TSLT0141
      RETURNCODE = 0                                                    TSLT0142
C                                                                       TSLT0143
C  energy-flow                                                          TSLT0144
      IEFLW= NLINK('EFLW', 0)                                           TSLT0145
      IEFLJ= NLINK('EFLJ', 0)                                           TSLT0146
      IF( IEFLW.EQ.0 .AND. IEFLJ.EQ.0 ) THEN                            TSLT0147
        IF( FIRSTWARNING ) THEN                                         TSLT0148
          WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'   TSLT0149
          WRITE(6,*) '!!       ********  TAUSLT  **********       !!'   TSLT0150
          WRITE(6,*) '!!       **  no EFLW or EFLJ cards **       !!'   TSLT0151
          WRITE(6,*) '!!       ** EFT section NOT filled **       !!'   TSLT0152
          WRITE(6,*) '!!       **     Action RETURN      **       !!'   TSLT0153
          WRITE(6,*) '!!       ****************************       !!'   TSLT0154
          WRITE(6,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'   TSLT0155
          FIRSTWARNING= .FALSE.                                         TSLT0156
        ENDIF                                                           TSLT0157
        RETURNCODE= 200                                                 TSLT0158
        RETURN                                                          TSLT0159
      ENDIF                                                             TSLT0160
                                                                        TSLT0161
C                                                                       TSLT0162
C  Check HV                                                             TSLT0163
      IF( (.NOT.XHVTRG) .AND. (KRUN.GT.4000) ) THEN                     TSLT0164
        RETURNCODE= 100                                                 TSLT0165
      ENDIF                                                             TSLT0166
                                                                        TSLT0167
C  Lock all SiCAL objects if the NSCL card is present:                  TSLT0168
C  (Reading MINI's, the SiCAL objects are not listed;                   TSLT0169
C  we can only cut on theta.)                                           TSLT0170
      JNSCL = IW(NAMIND('NSCL'))                                        TSLT0171
      IF (JNSCL .GT. 0) THEN                                            TSLT0172
        DO I=KFEFT,KLEFT                                                TSLT0173
          IF (KEFOTY(I) .EQ. 7) THEN                                    TSLT0174
            IF (ABS(QCT(I)) .GT. 0.99834) CALL QLTRK(I)                 TSLT0175
          ENDIF                                                         TSLT0176
        ENDDO                                                           TSLT0177
      ENDIF                                                             TSLT0178
C                                                                       TSLT0179
C  Count Total number of objects and charged tracks                     TSLT0180
C                                                                       TSLT0181
      NOBTOT = 0                                                        TSLT0182
      NCHTOT = 0                                                        TSLT0183
      DO ICHT = KFEFT, KLEFT                                            TSLT0184
        IF( .NOT.XLOCK(ICHT) ) NOBTOT = NOBTOT + 1                      TSLT0185
        IF( KEFOTY(ICHT).LT.3 ) THEN                                    TSLT0186
          IF ( ABS(QCT(ICHT)) .LT. .95 ) THEN                           TSLT0187
            NCHTOT = NCHTOT + 1                                         TSLT0188
          ENDIF                                                         TSLT0189
        END IF                                                          TSLT0190
      ENDDO                                                             TSLT0191
C                                                                       TSLT0192
C  Return if no reconstructed object                                    TSLT0193
C                                                                       TSLT0194
      IF ( NOBTOT .EQ. 0 ) THEN                                         TSLT0195
        CALL SBIT1(BITPATTERN,32)                                       TSLT0196
        GOTO 999                                                        TSLT0197
      ENDIF                                                             TSLT0198
C                                                                       TSLT0199
C Define two hemisphere according to the thrust axis                    TSLT0200
C                                                                       TSLT0201
      CALL QJOPTR('EF',' ')                                             TSLT0202
C -- Thrust                                                             TSLT0203
      CALL QJTHRU(THRUST,'THRUST',KRECO)                                TSLT0204
      ITHRUS = KPDIR('THRUST',KRECO)                                    TSLT0205
C -- Two Hemispheres                                                    TSLT0206
      CALL QJHEMI('SAME','OPPO',KRECO,ITHRUS,0.)                        TSLT0207
      ISAME = KPDIR('SAME',KRECO)                                       TSLT0208
      IOPPO = KPDIR('OPPO',KRECO)                                       TSLT0209
C                                                                       TSLT0210
C find the number of charged tracks , the number of neutral objects     TSLT0211
C and the neutral energy in each hemisphere                             TSLT0212
C                                                                       TSLT0213
C nchsame : number of charged tracks in hemisphere same                 TSLT0214
C NNeutSame : number of neutral object  " " "                           TSLT0215
C eneutsame : neutral energy          "  "          "                   TSLT0216
C related variables for hemisphere oppo                                 TSLT0217
C                                                                       TSLT0218
C                                                                       TSLT0219
C EmaxTrSame : energy of the leading particle in hemisphere same        TSLT0220
C ItkMaxTrSame : index of the leading                                   TSLT0221
C EmaxTrOppo : energy of the leading particle in hemisphere oppo        TSLT0222
C ItkMaxTrOppo : index of the leading                                   TSLT0223
C                                                                       TSLT0224
      NCHSAME = 0                                                       TSLT0225
      NCHOPPO = 0                                                       TSLT0226
      CHASAME = 0.                                                      TSLT0227
      CHAOPPO = 0.                                                      TSLT0228
      NNEUTSAME = 0                                                     TSLT0229
      NNEUTOPPO = 0                                                     TSLT0230
      ENEUTSAME = 0.                                                    TSLT0231
      ENEUTOPPO = 0.                                                    TSLT0232
      ITKMAXTRSAME = 0                                                  TSLT0233
      ITKMAXTROPPO = 0                                                  TSLT0234
      EMAXTRSAME= 0.                                                    TSLT0235
      EMAXTROPPO = 0.                                                   TSLT0236
      DO ITK = KFEFT, KLEFT                                             TSLT0237
        IF( ISAME.GT.0 ) THEN                                           TSLT0238
         IF( QP(ISAME).GT.0 ) THEN                                      TSLT0239
          IF ( QDOT3(ISAME,ITK).GT.0. ) THEN                            TSLT0240
            IF ( KEFOTY(ITK).GT.3 ) THEN                                TSLT0241
              NNEUTSAME = NNEUTSAME + 1                                 TSLT0242
              ENEUTSAME = ENEUTSAME + QE(ITK)                           TSLT0243
            ELSE IF( KEFOTY(ITK).LE.2 ) THEN                            TSLT0244
              IF( ABS(QCT(ITK)) .LT. .95  ) THEN                        TSLT0245
                NCHSAME = NCHSAME  + 1                                  TSLT0246
                CHASAME = CHASAME  + QCH(ITK)                           TSLT0247
                IF ( QE(ITK) .GT. EMAXTRSAME) THEN                      TSLT0248
                  EMAXTRSAME = QE(ITK)                                  TSLT0249
                  ITKMAXTRSAME = ITK                                    TSLT0250
                END IF                                                  TSLT0251
              ENDIF                                                     TSLT0252
            ENDIF                                                       TSLT0253
           END IF                                                       TSLT0254
          END IF                                                        TSLT0255
        END IF                                                          TSLT0256
        IF( IOPPO.GT.0 ) THEN                                           TSLT0257
         IF( QP(IOPPO).GT.0 ) THEN                                      TSLT0258
          IF ( QDOT3(IOPPO,ITK).GT.0. ) THEN                            TSLT0259
            IF ( KEFOTY(ITK).GT.3 ) THEN                                TSLT0260
              NNEUTOPPO = NNEUTOPPO + 1                                 TSLT0261
              ENEUTOPPO = ENEUTOPPO + QE(ITK)                           TSLT0262
            ELSE IF( KEFOTY(ITK).LE.2 ) THEN                            TSLT0263
              IF( ABS(QCT(ITK)) .LT. .95  ) THEN                        TSLT0264
                NCHOPPO = NCHOPPO  + 1                                  TSLT0265
                CHAOPPO = CHAOPPO  + QCH(ITK)                           TSLT0266
                IF ( QE(ITK) .GT. EMAXTROPPO ) THEN                     TSLT0267
                  EMAXTROPPO = QE(ITK)                                  TSLT0268
                  ITKMAXTROPPO = ITK                                    TSLT0269
                END IF                                                  TSLT0270
              ENDIF                                                     TSLT0271
            ENDIF                                                       TSLT0272
          ENDIF                                                         TSLT0273
         END IF                                                         TSLT0274
        END IF                                                          TSLT0275
      ENDDO                                                             TSLT0276
C                                                                       TSLT0277
C   Calculate cos(theta)* and acolinearity                              TSLT0278
C                                                                       TSLT0279
      IF( ISAME.GT.0 .AND. IOPPO.GT.0 ) THEN                            TSLT0280
       IF( QP(ISAME).GT.0 .AND. QP(IOPPO).GT.0 ) THEN                   TSLT0281
        CTHE1= MAX( MIN( QCT(ISAME),.999999 ),-.999999 )                TSLT0282
        CTHE2= MAX( MIN( QCT(IOPPO),.999999 ),-.999999 )                TSLT0283
        THET1 = ACOS( CTHE1 )                                           TSLT0284
        THET2 = ACOS( CTHE2 )                                           TSLT0285
        IF( THET1.LT.0. )   THET1= -THET1                               TSLT0286
        IF( THET1.GT.QQPI ) THET1= 2.*QQPI - THET1                      TSLT0287
        IF( THET2.LT.0. )   THET2= -THET2                               TSLT0288
        IF( THET2.GT.QQPI ) THET2= 2.*QQPI - THET2                      TSLT0289
        THEM= THET1                                                     TSLT0290
        THEP= THET2                                                     TSLT0291
        IF( CHAOPPO.LT.0. ) THEN                                        TSLT0292
          THEM= THET2                                                   TSLT0293
          THEP= THET1                                                   TSLT0294
        END IF                                                          TSLT0295
C                                                                       TSLT0296
        CTEST = COS(.5*(THEM+QQPI-THEP))/COS(.5*(THEM-QQPI+THEP))       TSLT0297
C                                                                       TSLT0298
        ACOLIN = QCOSA(ISAME,IOPPO)                                     TSLT0299
        ACOLIN=  MAX( MIN( ACOLIN,.999999 ),-.999999 )                  TSLT0300
        ACOLIN = ACOS( ACOLIN )                                         TSLT0301
        IF( ACOLIN.LT.0. )   ACOLIN= -ACOLIN                            TSLT0302
        IF( ACOLIN.GT.QQPI ) ACOLIN= 2.*QQPI - ACOLIN                   TSLT0303
        ACOLIN = ACOLIN*180./QQPI                                       TSLT0304
       END IF                                                           TSLT0305
      ELSE                                                              TSLT0306
        ACOLIN= 0.                                                      TSLT0307
        CTEST= 1.                                                       TSLT0308
        IF( IOPPO.NE.0 ) THEN                                           TSLT0309
         IF( QP(IOPPO).NE.0 ) CTEST= QCT(IOPPO)                         TSLT0310
        END IF                                                          TSLT0311
        IF( ISAME.NE.0 ) THEN                                           TSLT0312
         IF( QP(ISAME).NE.0 ) CTEST= QCT(ISAME)                         TSLT0313
        END IF                                                          TSLT0314
      END IF                                                            TSLT0315
C                                                                       TSLT0316
      ESAME = 0.                                                        TSLT0317
      EOPPO = 0.                                                        TSLT0318
      IF( ISAME.GT.0 ) ESAME = QE(ISAME)                                TSLT0319
      IF( IOPPO.GT.0 ) EOPPO = QE(IOPPO)                                TSLT0320
C                                                                       TSLT0321
      EBEAM = QELEP * 0.5                                               TSLT0322
C                                                                       TSLT0323
C  Cos(theta)* cut                                                      TSLT0324
C                                                                       TSLT0325
      IF ( ABS(CTEST) .GT. 0.9 ) THEN                                   TSLT0326
        CALL SBIT1(BITPATTERN,4)                                        TSLT0327
        GOTO 999                                                        TSLT0328
      END IF                                                            TSLT0329
C                                                                       TSLT0330
C  Acolinerity cut                                                      TSLT0331
C                                                                       TSLT0332
      IF ( ACOLIN .LT. ACOLINCUT ) THEN                                 TSLT0333
        CALL SBIT1(BITPATTERN,5)                                        TSLT0334
        GOTO 999                                                        TSLT0335
      END IF                                                            TSLT0336
C                                                                       TSLT0337
C  Require at least 1 charged track per hemisphere                      TSLT0338
C                                                                       TSLT0339
      IF ( MIN(NCHSAME,NCHOPPO).EQ.0 ) THEN                             TSLT0340
        CALL SBIT1( BITPATTERN,3)                                       TSLT0341
        GOTO 999                                                        TSLT0342
      ENDIF                                                             TSLT0343
C                                                                       TSLT0344
C  Require at most 8 charged tracks                                     TSLT0345
C                                                                       TSLT0346
      IF ( NCHTOT .LT. 2 .OR. NCHTOT .GT. MAXNBTRACK ) THEN             TSLT0347
        CALL SBIT1(BITPATTERN,1)                                        TSLT0348
        GOTO 999                                                        TSLT0349
      ENDIF                                                             TSLT0350
C                                                                       TSLT0351
C  Identification ....                                                  TSLT0352
C                                                                       TSLT0353
C  Flag bhabhalike ...                                                  TSLT0354
      CALL BHALIKE( BLIKE )                                             TSLT0355
C  ... and dimuonlike events                                            TSLT0356
      ITKSAMEALPHA= KEFOLT( ITKMAXTRSAME ) + KFCHT - 1                  TSLT0357
      ITKOPPOALPHA= KEFOLT( ITKMAXTROPPO ) + KFCHT - 1                  TSLT0358
      CALL MUOLIKE( ITKSAMEALPHA, ITKOPPOALPHA, MLIKE )                 TSLT0359
C                                                                       TSLT0360
C  Calculate opening angle                                              TSLT0361
C                                                                       TSLT0362
C  OpanMaxSame : opening angle in hemisphere isame                      TSLT0363
C  OpanMaxOppo : idem for hemisphere oppo                               TSLT0364
C                                                                       TSLT0365
      OPANMAXSAME = 0.                                                  TSLT0366
      OPANMAXOPPO = 0.                                                  TSLT0367
      DO ITK = KFEFT, KLEFT                                             TSLT0368
        IF( KEFOTY( ITK ).LE.2 ) THEN                                   TSLT0369
          IF ( QDOT3(ISAME,ITK).GT.0. ) THEN                            TSLT0370
            DO ITKS = KFEFT, KLEFT                                      TSLT0371
              IF( KEFOTY( ITKS ).LE.2 ) THEN                            TSLT0372
                IF ( QDOT3(ITKS,ISAME).GT.0. .AND. ITK.NE.ITKS ) THEN   TSLT0373
                  IF ( ABS(P_ACOS(QCOSA(ITK,ITKS))).GT.OPANMAXSAME )    TSLT0374
     &              THEN                                                TSLT0375
                    OPANMAXSAME = ABS(P_ACOS(QCOSA(ITK,ITKS)))          TSLT0376
                  ENDIF                                                 TSLT0377
                ENDIF                                                   TSLT0378
              ENDIF                                                     TSLT0379
            ENDDO                                                       TSLT0380
          ELSE                                                          TSLT0381
            DO ITKO = KFEFT, KLEFT                                      TSLT0382
              IF( KEFOTY( ITKO ).LE.2 ) THEN                            TSLT0383
                IF ( QDOT3(ITKO,IOPPO).GT.0. .AND. ITK.NE.ITKO ) THEN   TSLT0384
                  IF ( ABS(P_ACOS(QCOSA(ITK,ITKO))).GT.OPANMAXOPPO )    TSLT0385
     &              THEN                                                TSLT0386
                    OPANMAXOPPO = ABS(P_ACOS(QCOSA(ITK,ITKO)))          TSLT0387
                  ENDIF                                                 TSLT0388
                ENDIF                                                   TSLT0389
              ENDIF                                                     TSLT0390
            ENDDO                                                       TSLT0391
          ENDIF                                                         TSLT0392
        ENDIF                                                           TSLT0393
      ENDDO                                                             TSLT0394
C                                                                       TSLT0395
C  Against gamma gamma events                                           TSLT0396
C                                                                       TSLT0397
      IF( ABS(QPT(ISAME)-QPT(IOPPO)).LT.DELTAPTCUT * EBEAM .AND.        TSLT0398
     &   (QE(ISAME)+QE(IOPPO)) .LT. EDELTAPTCUT * EBEAM )               TSLT0399
     &    CALL SBIT1(BITPATTERN,6)                                      TSLT0400
C                                                                       TSLT0401
C  Against hadronic events                                              TSLT0402
C                                                                       TSLT0403
      PRESERVEHAD =                                                     TSLT0404
     &  (  ((NCHSAME.EQ.1).AND.(QM(ISAME).LT.1.)) .OR.                  TSLT0405
     &  ((NCHOPPO.EQ.1).AND.(QM(IOPPO).LT.1.))  )                       TSLT0406
C                                                                       TSLT0407
      IF ( (NCHSAME+NNEUTSAME)*(NCHOPPO+NNEUTOPPO).GE.NBNBCUT  ) CALL   TSLT0408
     &    SBIT1( BITPATTERN,7)                                          TSLT0409
C                                                                       TSLT0410
      IF ( OPANMAXSAME+OPANMAXOPPO.GT.OPACUT)                           TSLT0411
     &    CALL SBIT1( BITPATTERN,8)                                     TSLT0412
C                                                                       TSLT0413
C  Now dileptons                                                        TSLT0414
C                                                                       TSLT0415
      IF ( EMAXTRSAME+EMAXTROPPO.GT. ELEADCUT * EBEAM ) CALL            TSLT0416
     &  SBIT1( BITPATTERN,9)                                            TSLT0417
C                                                                       TSLT0418
C  Min. dist of possible gammas from ECAL cracks                        TSLT0419
C                                                                       TSLT0420
      CALL DECAL( ITKMAXTRSAME, 1, DC1, DC2, JERR )                     TSLT0421
      DGCRKSAME= MIN( ABS(DC1), ABS(DC2) )                              TSLT0422
      CALL DECAL( ITKMAXTROPPO, 1, DC1, DC2, JERR )                     TSLT0423
      DGCRKOPPO= MIN( ABS(DC1), ABS(DC2) )                              TSLT0424
C                                                                       TSLT0425
      DGCRKMIN= MIN( DGCRKSAME, DGCRKOPPO )                             TSLT0426
      IF( DGCRKMIN.LT.DGAMCRKCUT ) CALL SBIT1( BITPATTERN,10)           TSLT0427
C                                                                       TSLT0428
      PRES_BHA =  .NOT.BLIKE                                            TSLT0429
      PRES_MUO =  .NOT.MLIKE                                            TSLT0430
C                                                                       TSLT0431
      CALL ENEMAX( 1, EMAX )                                            TSLT0432
C                                                                       TSLT0433
C  Energy cuts against Bhabha's                                         TSLT0434
C                                                                       TSLT0435
      IF ( EMAX .GT. ETOTBHA1CUT ) CALL SBIT1( BITPATTERN,11)           TSLT0436
      IF ( EMAX .GT. ETOTBHA2CUT ) CALL SBIT1( BITPATTERN,12)           TSLT0437
C                                                                       TSLT0438
      CALL ENEMAX( 2, EMAX )                                            TSLT0439
C                                                                       TSLT0440
C  Energy cut against Dimuons                                           TSLT0441
C                                                                       TSLT0442
      IF ( EMAX .GT. ETOTMUOCUT  ) CALL SBIT1( BITPATTERN,13)           TSLT0443
C                                                                       TSLT0444
C  Last, for cosmic rejection require at least one track                TSLT0445
C  with |d0|<1 and |z0|<5                                               TSLT0446
C                                                                       TSLT0447
      COSMIC = .TRUE.                                                   TSLT0448
      DO ITK= KFEFT, KLEFT                                              TSLT0449
        IF (KEFOTY(ITK) .LE. 3) THEN                                    TSLT0450
          IF ( ABS(QDB(ITK)) .LT. 1 .AND. ABS(QZB(ITK)) .LT. 5  ) THEN  TSLT0451
            COSMIC = .FALSE.                                            TSLT0452
            GOTO 100                                                    TSLT0453
          ENDIF                                                         TSLT0454
        ENDIF                                                           TSLT0455
      ENDDO                                                             TSLT0456
  100 IF ( COSMIC ) CALL SBIT1(BITPATTERN,14)                           TSLT0457
C                                                                       TSLT0458
C  set the bit pattern taking into account the preservations            TSLT0459
C  and determine if the event is to be declared tau-like                TSLT0460
C                                                                       TSLT0461
      ITAUEVT = BITPATTERN                                              TSLT0462
C                                                                       TSLT0463
      IF ( PRESERVEHAD ) THEN                                           TSLT0464
        CALL SBIT0(ITAUEVT,7)                                           TSLT0465
        CALL SBIT0(ITAUEVT,8)                                           TSLT0466
      ELSE                                                              TSLT0467
        CALL SBIT1(BITPATTERN,20)                                       TSLT0468
      ENDIF                                                             TSLT0469
C                                                                       TSLT0470
      IF ( PRES_BHA ) THEN                                              TSLT0471
        CALL SBIT0(ITAUEVT,11)                                          TSLT0472
        CALL SBIT0(ITAUEVT,12)                                          TSLT0473
      ELSE                                                              TSLT0474
        CALL SBIT1(BITPATTERN,21)                                       TSLT0475
        IF( JBIT( ITAUEVT,10).EQ.0 ) CALL SBIT0(ITAUEVT,11)             TSLT0476
        IF( JBIT( ITAUEVT,10).NE.0 ) CALL SBIT0(ITAUEVT,12)             TSLT0477
      ENDIF                                                             TSLT0478
      CALL SBIT0(ITAUEVT,10)                                            TSLT0479
C                                                                       TSLT0480
      IF ( PRES_MUO ) THEN                                              TSLT0481
        CALL SBIT0(ITAUEVT,13)                                          TSLT0482
      ELSE                                                              TSLT0483
        CALL SBIT1(BITPATTERN,22)                                       TSLT0484
      ENDIF                                                             TSLT0485
C                                                                       TSLT0486
      IF ( ITAUEVT .EQ. 0 )   TAUEVT = .TRUE.                           TSLT0487
C                                                                       TSLT0488
  999 CONTINUE                                                          TSLT0489
C  Decode the bit pattern to fill some looses                           TSLT0490
      IF ( DEBUGLEVEL .GT. 0 .AND. RETURNCODE .LT. 200 ) CALL           TSLT0491
     &  DECODE_BITPATTERN(BITPATTERN)                                   TSLT0492
                                                                        TSLT0493
      RETURN                                                            TSLT0494
      END                                                               TSLT0495
                                                                        TSLT0496
      REAL FUNCTION P_ACOS(COSANG)                                      TSLT0497
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                     TSLT0498
C                                                 C                     TSLT0499
C  ACOS but with protection against argument > 1  C                     TSLT0500
C   or < -1 to get rid of errors messages         C                     TSLT0501
C                                                 C                     TSLT0502
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                     TSLT0503
      PARAMETER( QQPI=3.141593 )                                        TSLT0504
      REAL COSANG                                                       TSLT0505
C                                                                       TSLT0506
      IF ( COSANG .GT. 1. ) THEN                                        TSLT0507
        P_ACOS = 0.                                                     TSLT0508
C        WRITE(6,*) 'cosin was greater than 1 ',cosang,krun,kevt        TSLT0509
      ELSEIF ( COSANG .LT. -1. ) THEN                                   TSLT0510
        P_ACOS = QQPI                                                   TSLT0511
C        WRITE(6,*) 'cosin was lower than -1 ',cosang,krun,kevt         TSLT0512
      ELSE                                                              TSLT0513
        P_ACOS = ACOS(COSANG)                                           TSLT0514
      ENDIF                                                             TSLT0515
      END                                                               TSLT0516
                                                                        TSLT0517
C#######################################################################TSLT0518
      SUBROUTINE DECODE_BITPATTERN(BITPATTERN)                          TSLT0519
C-----------------------------------------                              TSLT0520
C                                                                       TSLT0521
C   Author   :- Laurent Duflot         9-FEB-1992                       TSLT0522
C   Modified :- Gerardo Ganis         21-JAN-1994                       TSLT0523
C                                                                       TSLT0524
C=========================================                              TSLT0525
C                                                                       TSLT0526
C   Purpose   : from the bit pattern fill looses with the history of    TSLT0527
C               each event                                              TSLT0528
C     (mod)   : Now fills the common /DATACUT/ with cut statistics      TSLT0529
C   Inputs    : bit pattern                                             TSLT0530
C   Outputs   : none                                                    TSLT0531
C                                                                       TSLT0532
C=========================================                              TSLT0533
C +                                                                     TSLT0534
C Declarations.                                                         TSLT0535
C -                                                                     TSLT0536
      IMPLICIT NONE                                                     TSLT0537
      INTEGER      CUTDATA(23)                                          TSLT0538
      COMMON /DATACUT/ CUTDATA                                          TSLT0539
      INTEGER*4  BITPATTERN                                             TSLT0540
      INTEGER IFI                                                       TSLT0541
      INTEGER JBIT                                                      TSLT0542
      EXTERNAL JBIT                                                     TSLT0543
      DATA IFI /1/                                                      TSLT0544
                                                                        TSLT0545
C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + TSLT0546
C Entry Point.                                                          TSLT0547
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - TSLT0548
      IF( IFI.EQ.1 ) THEN                                               TSLT0549
        CALL VZERO( CUTDATA, 23)                                        TSLT0550
        IFI= 0                                                          TSLT0551
      END IF                                                            TSLT0552
                                                                        TSLT0553
      CUTDATA(23)=CUTDATA(23)+1                                         TSLT0554
                                                                        TSLT0555
      IF ( JBIT(BITPATTERN,32) .EQ. 0  ) THEN                           TSLT0556
      ELSE                                                              TSLT0557
C - Events with no reconstructed objects                                TSLT0558
        CUTDATA(22)=CUTDATA(22)+1                                       TSLT0559
        GOTO 999                                                        TSLT0560
      ENDIF                                                             TSLT0561
                                                                        TSLT0562
      IF ( JBIT(BITPATTERN,4) .EQ. 0  ) THEN                            TSLT0563
C - Evts passing cos_theta_* cut                                        TSLT0564
        CUTDATA(1)=CUTDATA(1)+1                                         TSLT0565
      ELSE                                                              TSLT0566
        GOTO 999                                                        TSLT0567
      ENDIF                                                             TSLT0568
                                                                        TSLT0569
      IF ( JBIT(BITPATTERN,5) .EQ. 0  ) THEN                            TSLT0570
C - Evts passing acolinearity cut                                       TSLT0571
        CUTDATA(2)=CUTDATA(2)+1                                         TSLT0572
      ELSE                                                              TSLT0573
        GOTO 999                                                        TSLT0574
      ENDIF                                                             TSLT0575
                                                                        TSLT0576
      IF ( JBIT(BITPATTERN,3) .EQ. 0  ) THEN                            TSLT0577
C - Evts having at least 1 charged track per hemi                       TSLT0578
        CUTDATA(3)=CUTDATA(3)+1                                         TSLT0579
      ELSE                                                              TSLT0580
        GOTO 999                                                        TSLT0581
      ENDIF                                                             TSLT0582
                                                                        TSLT0583
      IF ( JBIT(BITPATTERN,1) .EQ. 0  ) THEN                            TSLT0584
C - Evts having 2<= N_charged_tot <=8                                   TSLT0585
        CUTDATA(4)=CUTDATA(4)+1                                         TSLT0586
      ELSE                                                              TSLT0587
        GOTO 999                                                        TSLT0588
      ENDIF                                                             TSLT0589
                                                                        TSLT0590
      IF ( JBIT(BITPATTERN,6) .EQ. 0  ) THEN                            TSLT0591
C - Evts passing E_tot and Delta_P_t cuts against gammagamma's          TSLT0592
        CUTDATA(5)=CUTDATA(5)+1                                         TSLT0593
      ELSE                                                              TSLT0594
        GOTO 999                                                        TSLT0595
      ENDIF                                                             TSLT0596
                                                                        TSLT0597
      IF ( JBIT(BITPATTERN,20) .EQ. 0  ) THEN                           TSLT0598
C - Events preserved from hadronic cuts                                 TSLT0599
        CUTDATA(6)=CUTDATA(6)+1                                         TSLT0600
      ELSE                                                              TSLT0601
        CUTDATA(11)=CUTDATA(11)+1                                       TSLT0602
        IF ( JBIT(BITPATTERN,7) .EQ. 0  ) THEN                          TSLT0603
C - Evts passing cut on nobj1*nobj2                                     TSLT0604
          CUTDATA(12)=CUTDATA(12)+1                                     TSLT0605
          IF ( JBIT(BITPATTERN,8) .EQ. 0  ) THEN                        TSLT0606
C - Evts passing cut on opa1+opa2                                       TSLT0607
            CUTDATA(13)=CUTDATA(13)+1                                   TSLT0608
            CUTDATA(6)=CUTDATA(6)+1                                     TSLT0609
          ELSE                                                          TSLT0610
            GOTO 999                                                    TSLT0611
          ENDIF                                                         TSLT0612
        ELSE                                                            TSLT0613
          GOTO 999                                                      TSLT0614
        ENDIF                                                           TSLT0615
      ENDIF                                                             TSLT0616
                                                                        TSLT0617
      IF ( JBIT(BITPATTERN,9) .EQ. 0  ) THEN                            TSLT0618
C - Evts passing cut on sum of leading momenta                          TSLT0619
        CUTDATA(7)=CUTDATA(7)+1                                         TSLT0620
      ELSE                                                              TSLT0621
        GOTO 999                                                        TSLT0622
      ENDIF                                                             TSLT0623
      IF ( JBIT(BITPATTERN,21) .EQ. 0  ) THEN                           TSLT0624
C - Evts preserved from cuts against Bhabha's                           TSLT0625
        CUTDATA(8)=CUTDATA(8)+1                                         TSLT0626
      ELSE                                                              TSLT0627
        CUTDATA(14)=CUTDATA(14)+1                                       TSLT0628
        IF ( JBIT(BITPATTERN,10) .EQ. 0  ) THEN                         TSLT0629
C - Evts having the min distance from cracks > 6 cm                     TSLT0630
          CUTDATA(15)=CUTDATA(15)+1                                     TSLT0631
          IF ( JBIT(BITPATTERN,12) .EQ. 0  ) THEN                       TSLT0632
C - Evts passing the cut at 1.6*E_beam                                  TSLT0633
            CUTDATA(8)=CUTDATA(8)+1                                     TSLT0634
            CUTDATA(16)=CUTDATA(16)+1                                   TSLT0635
          ELSE                                                          TSLT0636
            GOTO 999                                                    TSLT0637
          ENDIF                                                         TSLT0638
        ELSE                                                            TSLT0639
C - Evts having the min distance from cracks < 6 cm                     TSLT0640
          CUTDATA(17)=CUTDATA(17)+1                                     TSLT0641
          IF ( JBIT(BITPATTERN,11) .EQ. 0  ) THEN                       TSLT0642
C - Evts passing the cut at 1.4*E_beam                                  TSLT0643
            CUTDATA(8)=CUTDATA(8)+1                                     TSLT0644
            CUTDATA(18)=CUTDATA(18)+1                                   TSLT0645
          ELSE                                                          TSLT0646
            GOTO 999                                                    TSLT0647
          ENDIF                                                         TSLT0648
        ENDIF                                                           TSLT0649
      ENDIF                                                             TSLT0650
                                                                        TSLT0651
      IF ( JBIT(BITPATTERN,22) .EQ. 0  ) THEN                           TSLT0652
C - Evts preserved from cuts against dimuon's                           TSLT0653
        CUTDATA(9)=CUTDATA(9)+1                                         TSLT0654
      ELSE                                                              TSLT0655
        CUTDATA(19)=CUTDATA(19)+1                                       TSLT0656
        IF ( JBIT(BITPATTERN,13) .EQ. 0  ) THEN                         TSLT0657
C - Evts passing the cut at 1.8*E_beam                                  TSLT0658
          CUTDATA(9)=CUTDATA(9)+1                                       TSLT0659
          CUTDATA(20)=CUTDATA(20)+1                                     TSLT0660
        ELSE                                                            TSLT0661
          GOTO 999                                                      TSLT0662
        ENDIF                                                           TSLT0663
      ENDIF                                                             TSLT0664
                                                                        TSLT0665
      IF ( JBIT(BITPATTERN,14) .EQ. 0 ) THEN                            TSLT0666
C - Evts passing the cut against cosmics                                TSLT0667
        CUTDATA(10)=CUTDATA(10)+1                                       TSLT0668
      ELSE                                                              TSLT0669
        GOTO 999                                                        TSLT0670
      ENDIF                                                             TSLT0671
                                                                        TSLT0672
      CUTDATA(21)=CUTDATA(21)+1                                         TSLT0673
                                                                        TSLT0674
  999 RETURN                                                            TSLT0675
      END                                                               TSLT0676
                                                                        TSLT0677
      SUBROUTINE MUOLIKE( ITKS, ITKO, MUO )                             TSLT0678
C--------------------------------------------------------------------   TSLT0679
C! Check if the event is dimuon-like                                    TSLT0680
C                                                                       TSLT0681
C  Input :  itks    alpha number of leading in hemi 'SAME'              TSLT0682
C           itko    alpha number of leading in hemi 'OPPO'              TSLT0683
C                                                                       TSLT0684
C  Output:   muo    logical, .TRUE. if the event is dimuon-like         TSLT0685
C--------------------------------------------------------------------   TSLT0686
      INCLUDE '/aleph/phy/qcde.inc'
      LOGICAL    MUO                                                    TSLT0688
      INCLUDE '/aleph/phy/qmacro.inc'
C                                                                       TSLT0690
C --- Initialize                                                        TSLT0691
      CALL LOOSES('lepLIKE ', 6)                                        TSLT0692
      MUO= .FALSE.                                                      TSLT0693
      EBEAM= QELEP/2.                                                   TSLT0694
      IF( EBEAM.LT.40. ) EBEAM= 45.625                                  TSLT0695
C                                                                       TSLT0696
C --- Get muo_id variables: for 'SAME' ...                              TSLT0697
C                                                                       TSLT0698
C - Muon ID for the leading                                             TSLT0699
      CALL QMUIDO( ITKS,KR,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,             TSLT0700
     .            XMU,RAP,ANG,ISHAD,SUD,IDFS,IMC,IER)                   TSLT0701
C --- ... and for 'OPPO'                                                TSLT0702
      CALL QMUIDO( ITKO,KR,IBE,IBT,IM1,IM2,NEX,NFI,N10,N03,             TSLT0703
     .            XMU,RAP,ANG,ISHAD,SUD,IDFO,IMC,IER)                   TSLT0704
C                                                                       TSLT0705
C --- Track index for the hemispheres                                   TSLT0706
      ISAME= KPDIR('SAME', KRECO)                                       TSLT0707
      IOPPO= KPDIR('OPPO', KRECO)                                       TSLT0708
C                                                                       TSLT0709
C --- Require 2 loose muons ( |idf|>0 ) ...                             TSLT0710
      IF( ABS(IDFS).GT.0.5 .AND. ABS(IDFO).GT.0.5 ) THEN                TSLT0711
        MUO= .TRUE.                                                     TSLT0712
C --- ... or 1 loose muon and ene>.9*Ebeam in the opposite side.        TSLT0713
      ELSE IF( ABS(IDFS).GT.0.5 .AND. ABS(IDFO).LT. 0.5 ) THEN          TSLT0714
        IF( QE( IOPPO ).GT. (0.9*EBEAM) ) MUO= .TRUE.                   TSLT0715
      ELSE IF( ABS(IDFS).LT.0.5 .AND. ABS(IDFO).GT. 0.5 ) THEN          TSLT0716
        IF( QE( ISAME ).GT. (0.9*EBEAM) ) MUO= .TRUE.                   TSLT0717
      END IF                                                            TSLT0718
C                                                                       TSLT0719
      IF( MUO ) CALL LOOSES('lepLIKE ', 7)                              TSLT0720
C                                                                       TSLT0721
      RETURN                                                            TSLT0722
      END                                                               TSLT0723
                                                                        TSLT0724
      SUBROUTINE BHALIKE( BHA )                                         TSLT0725
C--------------------------------------------------------------------   TSLT0726
C! Check if the event is Bhabha-like                                    TSLT0727
C                                                                       TSLT0728
C  Input :  none                                                        TSLT0729
C  Output:   bha    logical, .TRUE. if the event is Bhabha-like         TSLT0730
C--------------------------------------------------------------------   TSLT0731
      INCLUDE '/aleph/phy/qcde.inc'
      LOGICAL    BHA                                                    TSLT0733
      INCLUDE '/aleph/phy/qmacro.inc'
C                                                                       TSLT0735
C --- Initialize                                                        TSLT0736
      BHA= .FALSE.                                                      TSLT0737
      NCHS= 0                                                           TSLT0738
      NCHO= 0                                                           TSLT0739
      NELS= 0                                                           TSLT0740
      NELO= 0                                                           TSLT0741
      CALL LOOSES('lepLIKE ', 1)                                        TSLT0742
C                                                                       TSLT0743
C --- Track index for 'SAME'                                            TSLT0744
      ISAME= KPDIR('SAME', KRECO)                                       TSLT0745
C                                                                       TSLT0746
C --- Count charged tracks and loose electrons                          TSLT0747
      DO ITK= KFEFT, KLEFT                                              TSLT0748
        IF( KEFOTY(ITK).LE.2 ) THEN                                     TSLT0749
          IF( ABS(QCT(ITK)).LT..95 ) THEN                               TSLT0750
            CALL ELOOSE( ITK, IDE )                                     TSLT0751
            IF( QDOT3( ITK, ISAME ).GT.0. ) THEN                        TSLT0752
              NCHS= NCHS+1                                              TSLT0753
              IF( IDE.EQ.1 ) NELS= NELS+1                               TSLT0754
            ELSE                                                        TSLT0755
              NCHO= NCHO+1                                              TSLT0756
              IF( IDE.EQ.1 ) NELO= NELO+1                               TSLT0757
            END IF                                                      TSLT0758
          END IF                                                        TSLT0759
        END IF                                                          TSLT0760
      END DO                                                            TSLT0761
C                                                                       TSLT0762
C --- Require all good charged tracks be loose electrons                TSLT0763
      IF( NCHS.EQ.NELS .AND. NCHO.EQ.NELO ) BHA= .TRUE.                 TSLT0764
C                                        `                              TSLT0765
      IF( BHA ) CALL LOOSES('lepLIKE ', 2)                              TSLT0766
C                                                                       TSLT0767
      RETURN                                                            TSLT0768
      END                                                               TSLT0769
      SUBROUTINE ELOOSE(ITK,IFLAG)                                      TSLT0770
C-----------------------------------------------------------------------TSLT0771
C! Electron id receipe                                                  TSLT0772
C                                                                       TSLT0773
C  Input  : ITK     ENFLW track number ( section EF )                   TSLT0774
C  Output : IFLAG   1 if electron-like, 0 otherwise                     TSLT0775
C-----------------------------------------------------------------------TSLT0776
      INCLUDE '/aleph/phy/qcde.inc'
      DIMENSION RMASS(1),QQ(1),RIEXP(1),SIGMA(1)                        TSLT0778
      DATA RMASS /0.0005/,QQ/1./                                        TSLT0779
      INCLUDE '/aleph/phy/qmacro.inc'
C                                                                       TSLT0781
      IFLAG= 1                                                          TSLT0782
                                                                        TSLT0783
C                                                                       TSLT0784
C --- Statistical purposes                                              TSLT0785
      CALL LOOSES('ELOOSE  ', 1)                                        TSLT0786
                                                                        TSLT0787
C                                                                       TSLT0788
C --- Get the ALPHA track index                                         TSLT0789
      ITA = KEFOLT(ITK) + (KFCHT - 1)                                   TSLT0790
C                                                                       TSLT0791
C --- Criteria depend on the momentum ...                               TSLT0792
      IF( QP(ITA).GT.5. ) THEN                                          TSLT0793
C ----- Extrapolate to ECAL                                             TSLT0794
        CALL DECAL(ITK,0,DIST1,DIST2,JERR)                              TSLT0795
C ----- Minimum distance from ECAL cracks                               TSLT0796
        DA = MIN(ABS(DIST1),ABS(DIST2))                                 TSLT0797
C ----- Transverse estimator                                            TSLT0798
        RT = QEIDRI(ITK,2)                                              TSLT0799
C ----- Longitudinal estimator                                          TSLT0800
        RL = QEIDRI(ITK,3)                                              TSLT0801
        IF( DA.GT.3. .AND. RT.LT.-5. ) THEN                             TSLT0802
          IF( QP(ITA).LT.41. .OR. ABS(RL).GT.2.5 ) IFLAG= 0             TSLT0803
        END IF                                                          TSLT0804
      ELSE                                                              TSLT0805
C ----- Get dE/dx                                                       TSLT0806
        CALL QDEDXM(ITA,1,RMASS,QQ,RI,NS,TL,RIEXP,SIGMA,IER)            TSLT0807
C ----- Normalize it                                                    TSLT0808
        RD= 10000.                                                      TSLT0809
        IF(IER.EQ.0.AND.SIGMA(1).NE.0) RD = (RI - RIEXP(1))/SIGMA(1)    TSLT0810
        IF( RD .LT. -3. ) IFLAG = 0                                     TSLT0811
      END IF                                                            TSLT0812
C                                                                       TSLT0813
C --- 'Muon' veto                                                       TSLT0814
      IF(KEFOTY(ITK) .EQ. 2) IFLAG = 0                                  TSLT0815
C                                                                       TSLT0816
      END                                                               TSLT0817
                                                                        TSLT0818
      SUBROUTINE DECAL(ITK,MODE,DIST1,DIST2,JERR)                       TSLT0819
C-----------------------------------------------------------------------TSLT0820
C! Computes the distance of the track to ecal module edge               TSLT0821
C                                                                       TSLT0822
C  Input  : ITK        Alpha number of the track                        TSLT0823
C           MODE       0 charged track                                  TSLT0824
C                      1 photon-like forward ( tangent at i.p. )        TSLT0825
C                     -1 photon-like backward (       "        )        TSLT0826
C                                                                       TSLT0827
C  Output : DIST1      distance from ECAL active edge at entrance point TSLT0828
C           DIST2      min. distance from ECAL active edge inside ECAL  TSLT0829
C                      ( 5 cm steps )                                   TSLT0830
C           JERR       Error flag ( 0 means OK )                        TSLT0831
C-----------------------------------------------------------------------TSLT0832
      INCLUDE '/aleph/phy/qcde.inc'
C                                                                       TSLT0834
C - Private commons                                                     TSLT0835
      COMMON/ETPINF/CHTP,PXTP,PYTP,                                     TSLT0836
     &PZTP,X0TP,Y0TP,Z0TP,C0TP,                                         TSLT0837
     &C1TP,C2TP,PTTP                                                    TSLT0838
C                                                                       TSLT0839
      REAL POINT(6)                                                     TSLT0840
      INTEGER EQEDGE                                                    TSLT0841
      INCLUDE '/aleph/phy/qmacro.inc'
                                                                        TSLT0843
C                                                                       TSLT0844
C - Computes the entrance position in ecal                              TSLT0845
      CHTP=QCH(ITK)                                                     TSLT0846
      PXTP=QX(ITK)                                                      TSLT0847
      PYTP=QY(ITK)                                                      TSLT0848
      PZTP=QZ(ITK)                                                      TSLT0849
      PHI=QFRFP0(ITK)                                                   TSLT0850
      IF(ABS(CHTP).GT.0.) THEN                                          TSLT0851
        X0TP= QDB(ITK)*SIN(PHI)                                         TSLT0852
        Y0TP=-QDB(ITK)*COS(PHI)                                         TSLT0853
        Z0TP= QZB(ITK)                                                  TSLT0854
      ELSE                                                              TSLT0855
        X0TP=QVXNOM                                                     TSLT0856
        Y0TP=QVYNOM                                                     TSLT0857
        Z0TP=QVZNOM                                                     TSLT0858
      ENDIF                                                             TSLT0859
      PTTP=QP(ITK)                                                      TSLT0860
      C0TP=PXTP/PTTP                                                    TSLT0861
      C1TP=PYTP/PTTP                                                    TSLT0862
      C2TP=PZTP/PTTP                                                    TSLT0863
                                                                        TSLT0864
C                                                                       TSLT0865
C - Photon-like forward: to get it we give to it a momentum of 1 TeV    TSLT0866
      IF(MODE.NE.0) THEN                                                TSLT0867
        XFACT=1000./PTTP                                                TSLT0868
        PTTP=PTTP*XFACT                                                 TSLT0869
        PXTP=PXTP*XFACT                                                 TSLT0870
        PYTP=PYTP*XFACT                                                 TSLT0871
        PZTP=PZTP*XFACT                                                 TSLT0872
                                                                        TSLT0873
C                                                                       TSLT0874
C - Photon-like backward: change direction                              TSLT0875
        IF(MODE.EQ.-1) THEN                                             TSLT0876
          PXTP=-PXTP                                                    TSLT0877
          PYTP=-PYTP                                                    TSLT0878
          PZTP=-PZTP                                                    TSLT0879
          C0TP=-C0TP                                                    TSLT0880
          C1TP=-C1TP                                                    TSLT0881
          C2TP=-C2TP                                                    TSLT0882
        ENDIF                                                           TSLT0883
      ENDIF                                                             TSLT0884
                                                                        TSLT0885
C                                                                       TSLT0886
C - Extrapolate to ECAL entrance ( Modified Julia routine; see below )  TSLT0887
      DIST1=-10000                                                      TSLT0888
      DIST2=-10000                                                      TSLT0889
      IF( ABS(QCH(ITK)).LT..5 ) CHTP= 1.                                TSLT0890
      CALL E4XYZ0(POINT,IERR)                                           TSLT0891
      IF(IERR.NE.0) THEN                                                TSLT0892
        JERR=IERR                                                       TSLT0893
        DIST1= DIST1*IERR                                               TSLT0894
        DIST2= DIST2*IERR                                               TSLT0895
        RETURN                                                          TSLT0896
      ENDIF                                                             TSLT0897
                                                                        TSLT0898
C                                                                       TSLT0899
C - Sometime the point is out of the module: move it 1 cm onward        TSLT0900
      POINT(1)=POINT(1)+POINT(4)                                        TSLT0901
      POINT(2)=POINT(2)+POINT(5)                                        TSLT0902
      POINT(3)=POINT(3)+POINT(6)                                        TSLT0903
                                                                        TSLT0904
C                                                                       TSLT0905
C - Calculate the distance from ECAL active volume ( Alephlib routine ) TSLT0906
      ICRACK=EQEDGE(POINT,DIS,IPHIDG)                                   TSLT0907
      DIST1= DIS                                                        TSLT0908
                                                                        TSLT0909
C                                                                       TSLT0910
C - Onward steps ( 5 cm each ) to get the min. dist. inside             TSLT0911
      DIST2= -10000.                                                    TSLT0912
      DO I=1,10                                                         TSLT0913
        XL= (I-1)*5. + 5.                                               TSLT0914
        POINT(1)=POINT(1)+XL*POINT(4)                                   TSLT0915
        POINT(2)=POINT(2)+XL*POINT(5)                                   TSLT0916
        POINT(3)=POINT(3)+XL*POINT(6)                                   TSLT0917
        ICRACK=EQEDGE(POINT,DIS,IPHIDG)                                 TSLT0918
        IF( ABS(DIS).LT.ABS(DIST2) ) DIST2= DIS                         TSLT0919
      END DO                                                            TSLT0920
                                                                        TSLT0921
      RETURN                                                            TSLT0922
      END                                                               TSLT0923
                                                                        TSLT0924
      SUBROUTINE E4XYZ0(PAREX,IERR)                                     TSLT0925
C---------------------------------------------------------------------- TSLT0926
C!  - R2 ESTIMATOR : GIVE X Y Z EXTRAPOLATION ON THE FIRST PLANE OF ECALTSLT0927
C!                                                                      TSLT0928
C!    AUTHOR  : D. PALLIN                                               TSLT0929
C!    MODIFIED: L. ROLANDI for application to ditau selection           TSLT0930
C!                                                                      TSLT0931
C?                                                                      TSLT0932
C!======================================================================TSLT0933
      COMMON/EIFLAG/TETIFL,PHIIFL                                       TSLT0934
      COMMON/ETPINF/CHTP,PXTP,PYTP,                                     TSLT0935
     &PZTP,X0TP,Y0TP,Z0TP,C0TP,                                         TSLT0936
     &C1TP,C2TP,PTTP                                                    TSLT0937
      COMMON/E4COM0/ICLU,ITRCL,JTRAK                                    TSLT0938
     &,KSINT,IMOD,ISUB,ISUB2,INDI,SK                                    TSLT0939
     &,A(3),B(3),C(3),D(3),XI,YI,ZI                                     TSLT0940
      INTEGER JULINP,JULOUT,NWRTMX,NATOUT,EPIOUT,NOOUTP,JULMON          TSLT0941
      PARAMETER (JULINP=20,JULOUT=50,NWRTMX=3,NATOUT=1,EPIOUT=2,        TSLT0942
     +  NOOUTP=3,JULMON=18)                                             TSLT0943
      INTEGER MAXFPO                                                    TSLT0944
      PARAMETER (MAXFPO=150)                                            TSLT0945
      COMMON /RLUNIT/LINPRL,LOUTRL,LDEBRL,LCOMRL,LPOTRL,LHSTRL,         TSLT0946
     +               LRCONS,LRGEOM,LUNIRL(NWRTMX),                      TSLT0947
     +               JOUTRL(NWRTMX),ANYORL,LMONRL,LSUMRL,LPAS0L         TSLT0948
      INTEGER LINPRL,LOUTRL,LDEBRL,LCOMRL,LPOTRL,LHSTRL,LRCONS,         TSLT0949
     +               LRGEOM,LUNIRL,JOUTRL,LMONRL,LSUMRL,LPAS0L          TSLT0950
      LOGICAL ANYORL                                                    TSLT0951
      COMMON /RLUNIC/OUTLIS,FORMRL,FTYPRL,OTYPRL,SUMLIS                 TSLT0952
      CHARACTER*600 OUTLIS(NWRTMX),TEMLIS                               TSLT0953
      CHARACTER*100 SUMLIS                                              TSLT0954
      CHARACTER*4 FORMRL(3),FTYPRL(3),OTYPRL(NWRTMX)                    TSLT0955
      INTEGER JDBDRF,JHISRF,JPRSRF,JPRERF,MDET                          TSLT0956
      LOGICAL FDEBRF,FDETRF                                             TSLT0957
      PARAMETER (MDET=17)                                               TSLT0958
      COMMON /RFLAGS/FDEBRF,FDETRF(MDET),JDBDRF(MDET),JHISRF(MDET),     TSLT0959
     +        JPRSRF(MDET),JPRERF(MDET),JCMORF,JEBIRF(2),JFBIRF(2)      TSLT0960
      PARAMETER (JULVD=1,JULIT=2,JULTP=3,JULEC=4,JULLC=5)               TSLT0961
      PARAMETER (JULSA=6,JULHC=7,JULMU=8,JULCA=9,JULSK=10)              TSLT0962
      PARAMETER (JULYR=11,JULYT=12,JULEF=13,JULBC=14,JULFA=15           TSLT0963
     +           ,JULBO=16,JULSI=17)                                    TSLT0964
      PARAMETER (MODEND=0,MODREA=1,MODPRV=2,MODPRI=3,MODPRT=4)          TSLT0965
      PARAMETER (MODPRE=5,MODPRL=6,MODPRS=7,MODPRH=8,MODPRM=9)          TSLT0966
      PARAMETER (MODTPT=10,MODTPW=11,MODDEX=12,MODITC=13 ,MODVDE=14)    TSLT0967
      PARAMETER (MODFIT=15,MODFTR=16,MODECL=17,MODMIP=18,MODEID=19)     TSLT0968
      PARAMETER (MODFEO=20,MODFEP=21,MODHCL=22,MODHDP=23,MODEHG=24)     TSLT0969
      PARAMETER (MODMUF=25,MODMUA=26,MODLCA=27,MODSAT=28,MODLTR=29)     TSLT0970
      PARAMETER (MODPID=30,MODYMA=31,MODYTO=32,MODYV0=33,MODHIS=34)     TSLT0971
      PARAMETER (MODPOT=35,MODMIN=36,MODWRI=37)                         TSLT0972
      PARAMETER (MODCOM=38,MODBCA=39,MODBOO=40,MODMON=41,MODENF=42)     TSLT0973
      PARAMETER (MODGAM=43,MODEDI=44,MODGMX=45)                         TSLT0974
      PARAMETER (MODSUM=46,MODBOM=47)                                   TSLT0975
      PARAMETER (MODPSI=48,MODSIC=49,MODGMP=50)                         TSLT0976
      PARAMETER (LASTMO=50)                                             TSLT0977
      PARAMETER(ICOMIN=1,ICOMAX=228)                                    TSLT0978
      PARAMETER(JECMIN=1)                                               TSLT0979
      PARAMETER(JBAMAX=384)                                             TSLT0980
      PARAMETER(JR4MAX=384)                                             TSLT0981
      PARAMETER(JR3MAX=288)                                             TSLT0982
      PARAMETER(JR2MAX=192)                                             TSLT0983
      PARAMETER(JR1MAX=96)                                              TSLT0984
      PARAMETER(ICOV11=45,ICOV12=50,ICOV13=51,ICOV14=56)                TSLT0985
      PARAMETER(COOV10=45.5,COOV11=50.5,COOV12=51.,COOV13=51.5)         TSLT0986
      PARAMETER(COOV14=56.5)                                            TSLT0987
      PARAMETER(ICOV21=173,ICOV22=178,ICOV23=179,ICOV24=184)            TSLT0988
      PARAMETER(COOV20=173.5,COOV21=178.5,COOV22=179.,COOV23=179.5)     TSLT0989
      PARAMETER(COOV24=184.5)                                           TSLT0990
      PARAMETER(ICOVTW=5)                                               TSLT0991
      PARAMETER(ICR411=41,COR411=40.5,COR412=41.,COR413=41.5)           TSLT0992
      PARAMETER(ICR421=188,COR421=188.5,COR422=189.,COR423=189.5)       TSLT0993
      PARAMETER(ICR311=25,COR311=24.5,COR312=25.,COR313=25.5)           TSLT0994
      PARAMETER(ICR321=204,COR321=204.5,COR322=205.,COR323=205.5)       TSLT0995
      PARAMETER(ICR211=9,COR211=8.5,COR212=9.,COR213=9.5)               TSLT0996
      PARAMETER(ICR221=220,COR221=220.5,COR222=221.,COR223=221.5)       TSLT0997
      PARAMETER(RBARL0=192.8,ZEDCP0=255.4)                              TSLT0998
      PARAMETER(ZLIMOD=231.4,RLIMEC=225.)                               TSLT0999
      PARAMETER(RBARL1=250.)                                            TSLT1000
C                              magnetique field picked-up in ALPHA      TSLT1001
      COMMON/RCONDS/FIELRC                                              TSLT1002
C                                                                       TSLT1003
      PARAMETER(EPSILO=0.03)                                            TSLT1004
      CHARACTER*23 WRONG                                                TSLT1005
      DIMENSION PAR(7),PAREX(6),DX(3)                                   TSLT1006
      IBOUC=0                                                           TSLT1007
      IERR=0                                                            TSLT1008
      RP1=RBARL0                                                        TSLT1009
      ZP1=ZEDCP0                                                        TSLT1010
      PAR(1)=X0TP                                                       TSLT1011
      PAR(2)=Y0TP                                                       TSLT1012
      PAR(3)=Z0TP                                                       TSLT1013
      PAR(4)=C0TP                                                       TSLT1014
      PAR(5)=C1TP                                                       TSLT1015
      PAR(6)=C2TP                                                       TSLT1016
      PAR(7)=PTTP                                                       TSLT1017
      IF(JDBDRF(JULEC).GT.8)                                            TSLT1018
     &WRITE(LDEBRL,*)'PX PY PZ ',PXTP,PYTP,PZTP                         TSLT1019
CXX DETERMINATION X0 Y0 Z0 ET COS DIRECTEUR DE LA DTRE AU PLAN 1 DU     TSLT1020
CXX MODULE D ENTREE                                                     TSLT1021
    1 CALL AUHCYL(RP1,ZP1,FIELRC,CHTP,PAR,PAREX,ICODE)                  TSLT1022
      IF ( ICODE.EQ.0) THEN                                             TSLT1023
        IERR = 1                                                        TSLT1024
        GO TO 999                                                       TSLT1025
      ENDIF                                                             TSLT1026
      IF(JDBDRF(JULEC).GT.8)                                            TSLT1027
     &WRITE (LDEBRL,122)PAREX,ICODE,RP1,ZP1                             TSLT1028
  122 FORMAT(1X,'PAREX',6(F8.2,1X),'ICODE',I2,'R Z',2F8.3)              TSLT1029
      IBOUC=IBOUC+1                                                     TSLT1030
      DX(1)=PAREX(1)                                                    TSLT1031
      DX(2)=PAREX(2)                                                    TSLT1032
      DX(3)=PAREX(3)                                                    TSLT1033
      ROWJ=0                                                            TSLT1034
      COLI=0                                                            TSLT1035
      STK=0                                                             TSLT1036
      CALL EFNCRS(DX,ROWJ,COLI,STK,WRONG)                               TSLT1037
      TETIFL=COLI                                                       TSLT1038
      PHIIFL=ROWJ                                                       TSLT1039
      IF(JDBDRF(JULEC).GT.8)                                            TSLT1040
     &WRITE(LDEBRL,100)COLI,ROWJ,STK,WRONG                              TSLT1041
  100 FORMAT(/,1X,                                                      TSLT1042
     &'E4XYZ0 : COLI ROWJ STK',3(1X,F8.3),4X,A13)                       TSLT1043
C                                                                       TSLT1044
CXX    SI STK.GT.1.01  ON BOUCLE EN REDEFINISSANT RP1 ZP1               TSLT1045
C                                                                       TSLT1046
      IF(STK.EQ.0)THEN                                                  TSLT1047
        RP1=RBARL1                                                      TSLT1048
        ZP1=ZEDCP0                                                      TSLT1049
        IF(IBOUC.GE.5)THEN                                              TSLT1050
          IERR = 2                                                      TSLT1051
C         WRITE(LDEBRL,*)'E4XYZ0 >...IBOUC'                             TSLT1052
          GO TO 999                                                     TSLT1053
        ENDIF                                                           TSLT1054
        GOTO 1                                                          TSLT1055
      ENDIF                                                             TSLT1056
      IF(STK.GT.1.01.OR.STK.LT.0.99)THEN                                TSLT1057
        IF(IBOUC.GE.5)THEN                                              TSLT1058
          IERR = 3                                                      TSLT1059
C         WRITE(LDEBRL,*)'E4XYZ0 >...IBOUC'                             TSLT1060
          GO TO 999                                                     TSLT1061
        ENDIF                                                           TSLT1062
        ST=1.005                                                        TSLT1063
        CALL ESRPT('ALEPH',COLI,ROWJ,ST,DX)                             TSLT1064
        IF(COLI.LE.COOV12.OR.COLI.GE.COOV22)THEN                        TSLT1065
          ZP1=ABS(DX(3))+EPSILO                                         TSLT1066
          RP1=RBARL1                                                    TSLT1067
        ELSE                                                            TSLT1068
          RP1=SQRT(DX(1)**2+DX(2)**2)                                   TSLT1069
        ENDIF                                                           TSLT1070
        GOTO 1                                                          TSLT1071
      ENDIF                                                             TSLT1072
      RETURN                                                            TSLT1073
  999 CALL RERROR ( ' E4XYZ0 ' , 1 , ' NO EXTRAPOLATION DONE ')         TSLT1074
      RETURN                                                            TSLT1075
      END                                                               TSLT1076
                                                                        TSLT1077
      SUBROUTINE ENEMAX( IOPT, EMAX )                                   TSLT1078
C---------------------------------------------------------------------- TSLT1079
C---------------------------------------------------------------------- TSLT1080
      INCLUDE '/aleph/phy/qcde.inc'
      INCLUDE '/aleph/phy/qmacro.inc'
C                                                                       TSLT1083
C --- Initialize                                                        TSLT1084
      EBEAM= QELEP/2.                                                   TSLT1085
      IF( EBEAM.LT.40. ) EBEAM= 45.625                                  TSLT1086
      ECP1= 0.                                                          TSLT1087
      ECP2= 0.                                                          TSLT1088
C
      JNSCL = NLINK('NSCL',0)
C                                                                       TSLT1089
C --- Index for 'SAME' and 'OPPO'                                       TSLT1090
      ISAME= KPDIR('SAME', KRECO)                                       TSLT1091
      IOPPO= KPDIR('OPPO', KRECO)                                       TSLT1092
C                                                                       TSLT1093
C --- Reconstructed ECAL energy per emisphere                           TSLT1094
      IOBJ= KPDIR('ECAL',KRECO)                                         TSLT1095
   10 IF(IOBJ .EQ. 0) GOTO 11                                           TSLT1096
C --- xc: correction factor for ecal saturation ( only data )           TSLT1097
      XC= 1.                                                            TSLT1098
      IF( KRUN.GT.4000 ) XC= 1. + .00078*QPECEC(IOBJ)                   TSLT1099
      THETA = QPECTH(IOBJ)
      IF( THETA.GT.0.05770 .AND. THETA.LT.3.08389 .AND. JNSCL.GT.0 )THEN
        IF( QDOT3( IOBJ, ISAME ).GT.0. ) THEN                           TSLT1100
          ECP1= ECP1 + XC*QPECEC(IOBJ)                                  TSLT1101
        ELSE                                                            TSLT1102
          ECP2= ECP2 + XC*QPECEC(IOBJ)                                  TSLT1103
        ENDIF                                                           TSLT1104
      ENDIF                                                             TSLT1104
      IOBJ= KFOLLO(IOBJ)                                                TSLT1105
      GOTO 10                                                           TSLT1106
   11 CONTINUE                                                          TSLT1107
C                                                                       TSLT1108
C --- Angular variables                                                 TSLT1109
      CX1= QCT(ISAME)                                                   TSLT1110
      CX2= QCT(IOPPO)                                                   TSLT1111
      CX1= MAX( MIN( CX1,.999999 ),-.999999 )                           TSLT1112
      CX2= MAX( MIN( CX2,.999999 ),-.999999 )                           TSLT1113
      SX1= SQRT(1.-CX1*CX1)                                             TSLT1114
      SX2= SQRT(1.-CX2*CX2)                                             TSLT1115
      SX12= SX1*CX2+SX2*CX1                                             TSLT1116
C                                                                       TSLT1117
C --- Energy in the beam pipe                                           TSLT1118
      ERAD= ABS(2*SX12/(SX1+SX2+ABS(SX12)))                             TSLT1119
C                                                                       TSLT1120
C --- Recalculated energies                                             TSLT1121
      EAN1= 2*SX2/(SX1+SX2+ABS(SX12))                                   TSLT1122
      EAN2= 2*SX1/(SX1+SX2+ABS(SX12))                                   TSLT1123
C                                                                       TSLT1124
C --- ENFLW energies                                                    TSLT1125
      ENE1= QE(ISAME)                                                   TSLT1126
      ENE2= QE(IOPPO)                                                   TSLT1127
C                                                                       TSLT1128
C --- Variables                                                         TSLT1129
      EREC= (ENE1+ENE2)/EBEAM                                           TSLT1130
      EECP= (ECP1+ECP2)/EBEAM                                           TSLT1131
C                                                                       TSLT1132
C --- Output                                                            TSLT1133
      IF( IOPT.EQ.1 ) THEN                                              TSLT1134
        EMAX= MAX( EREC, EECP ) + ERAD                                  TSLT1135
      ELSE IF( IOPT.EQ.2 ) THEN                                         TSLT1136
        EMAX=        EREC         + ERAD                                TSLT1137
      ENDIF                                                             TSLT1138
C                                                                       TSLT1139
      RETURN                                                            TSLT1140
      END                                                               TSLT1141
                                                                        TSLT1142
      SUBROUTINE TSLPRS( LUN )                                          TSLT1143
C-----------------------------------------------------------------------TSLT1144
C Routine to print out the content of /DATACUT/ common                  TSLT1145
C                                                                       TSLT1146
C-----------------------------------------------------------------------TSLT1147
      INTEGER CUTDATA(23)                                               TSLT1148
      COMMON /DATACUT/ CUTDATA                                          TSLT1149
      CHARACTER*30 CUTNAME(24)                                          TSLT1150
      DATA CUTNAME / ' | cos theta* | < .9          ',                  TSLT1151
     .               '  acol>160                    ',                  TSLT1152
     .               ' min(nc1,nc2) > 0             ',                  TSLT1153
     .               ' 1 < # charged tracks < 9     ',                  TSLT1154
     .               ' delta Pt cut & tot energy    ',                  TSLT1155
     .               ' Hadronic cuts                ',                  TSLT1156
     .               ' Sum of leading energy        ',                  TSLT1157
     .               ' Bhabha cuts                  ',                  TSLT1158
     .               ' Dimuon cuts                  ',                  TSLT1159
     .               ' Cosmics cut                  ',                  TSLT1160
     .               ' undergoing hadronic cuts     ',                  TSLT1161
     .               ' nob1*nob2 < 75               ',                  TSLT1162
     .               ' opa1+opa2 < .25              ',                  TSLT1163
     .               ' undergoing Bhabha cuts       ',                  TSLT1164
     .               ' D_gck > 6cm                  ',                  TSLT1165
     .               ' tot energy < 1.6             ',                  TSLT1166
     .               ' D_gck < 6cm                  ',                  TSLT1167
     .               ' tot energy < 1.4             ',                  TSLT1168
     .               ' undergoing dimuon cuts       ',                  TSLT1169
     .               ' tot energy < 1.8             ',                  TSLT1170
     .               ' events have been selected    ',                  TSLT1171
     .               ' events without rec objects   ',                  TSLT1172
     .               ' events have XLUMOK=.TRUE.    ',                  TSLT1173
     .               ' events with >0 rec objects   '/                  TSLT1174
                                                                        TSLT1175
      WRITE(LUN,99)                                                     TSLT1176
 99   FORMAT(/,1X,'*-------------------------------------------------*',TSLT1177
     .       /,1X,'*        General statistics from TSLT01           *',TSLT1178
     .       /,1X,'*                                                 *',TSLT1179
     .       /,1X,'*        # evts      Cut description              *')TSLT1180
                                                                        TSLT1181
      WRITE(LUN, 100 ) CUTDATA(23),CUTNAME(23)                          TSLT1182
      WRITE(LUN, 100 ) CUTDATA(23)-CUTDATA(22),CUTNAME(24)              TSLT1183
      DO I=1,10                                                         TSLT1184
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)                          TSLT1185
      END DO                                                            TSLT1186
                                                                        TSLT1187
      WRITE(LUN,101)                                                    TSLT1188
101   FORMAT(/,1X,'*-------------------------------------------------*',TSLT1189
     .       /,1X,'*                    Hadronic cuts                *',TSLT1190
     .       /,1X,'*                                                 *',TSLT1191
     .       /,1X,'*        # evts      Cut description              *')TSLT1192
                                                                        TSLT1193
      DO I=11,13                                                        TSLT1194
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)                          TSLT1195
      END DO                                                            TSLT1196
                                                                        TSLT1197
      WRITE(LUN,102)                                                    TSLT1198
102   FORMAT(/,1X,'*-------------------------------------------------*',TSLT1199
     .       /,1X,'*                    Bhabha cuts                  *',TSLT1200
     .       /,1X,'*                                                 *',TSLT1201
     .       /,1X,'*        # evts      Cut description              *')TSLT1202
                                                                        TSLT1203
      DO I=14,18                                                        TSLT1204
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)                          TSLT1205
      END DO                                                            TSLT1206
                                                                        TSLT1207
      WRITE(LUN,103)                                                    TSLT1208
103   FORMAT(/,1X,'*-------------------------------------------------*',TSLT1209
     .       /,1X,'*                    Dimuon cuts                  *',TSLT1210
     .       /,1X,'*                                                 *',TSLT1211
     .       /,1X,'*        # evts      Cut description              *')TSLT1212
                                                                        TSLT1213
      DO I=19,20                                                        TSLT1214
        WRITE(LUN, 100 ) CUTDATA(I),CUTNAME(I)                          TSLT1215
      END DO                                                            TSLT1216
                                                                        TSLT1217
      WRITE(LUN,104)                                                    TSLT1218
104   FORMAT(/,1X,'*-------------------------------------------------*',TSLT1219
     .       /,1X,'*      Final statistics from TSLT01               *',TSLT1220
     .       /,1X,'*                                                 *')TSLT1221
                                                                        TSLT1222
      WRITE(LUN, 100 ) CUTDATA(23),CUTNAME(23)                          TSLT1223
      WRITE(LUN, 100 ) CUTDATA(21),CUTNAME(21)                          TSLT1224
      WRITE(LUN, 100 ) CUTDATA(22),CUTNAME(22)                          TSLT1225
                                                                        TSLT1226
      WRITE(LUN,105)                                                    TSLT1227
105   FORMAT(1X,'*-------------------------------------------------*',/)TSLT1228
                                                                        TSLT1229
                                                                        TSLT1230
100   FORMAT(2X,I13,5X,30A)                                             TSLT1231
                                                                        TSLT1232
      RETURN                                                            TSLT1233
      END                                                               TSLT1234
