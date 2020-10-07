C-------------------- /QMACRO/ --- statement functions ---------------- QMACRO 2
C - # of words/row in bank with index KI                                BMACRO 2
      LCOLS(KI) = IW(KI+LMHCOL)                                         BMACRO 3
C - # of rows in bank with index KI                                     BMACRO 4
      LROWS(KI) = IW(KI+LMHROW)                                         BMACRO 5
C - index of next row in the bank with index KI                         BMACRO 6
      KNEXT(KI) = KI + LMHLEN + IW(KI+1)*IW(KI+2)                       BMACRO 7
C - index of row # KI1 in the bank with index KI                        BMACRO 8
      KROW(KI,KI1) = KI + LMHLEN + IW(KI+1)*(KI1-1)                     BMACRO 9
C - # of free words in the bank with index KI                           BMACRO10
      LFRWRD(KI) = KI + IW(KI) - KNEXT(KI)                              BMACRO11
C - # of free rows in the bank with index KI                            BMACRO12
      LFRROW(KI) = LFRWRD(KI) / LCOLS(KI)                               BMACRO13
C - KI2th integer element of the KI1th row of the bank with index KI    BMACRO14
      ITABL(KI,KI1,KI2) = IW(KI+LMHLEN+(KI1-1)*IW(KI+1)+KI2)            BMACRO15
C - KI2th real element of the KI1th row of the bank with index KI       BMACRO16
      RTABL(KI,KI1,KI2) = RW(KI+LMHLEN+(KI1-1)*IW(KI+1)+KI2)            BMACRO17
C-------------------- /QCFUNC/ --- statement functions for ALPHA banks  QMACRO 4
      KJQDET(KI)=IW(KOQVEC+KI*KCQVEC+JQVEQD)                            QMACRO 5
      QSQT(QF)=SIGN(SQRT(ABS(QF)),QF)                                   QMACRO 6
      QP(KI)=RW(KOQVEC+KI*KCQVEC+JQVEQP)                                QMACRO 7
      QX(KI)=RW(KOQVEC+KI*KCQVEC+JQVEQX)                                QMACRO 8
      QY(KI)=RW(KOQVEC+KI*KCQVEC+JQVEQY)                                QMACRO 9
      QZ(KI)=RW(KOQVEC+KI*KCQVEC+JQVEQZ)                                QMACRO10
      QE(KI)=RW(KOQVEC+KI*KCQVEC+JQVEQE)                                QMACRO11
      QM(KI)=RW(KOQVEC+KI*KCQVEC+JQVEQM)                                QMACRO12
      QCH(KI)=RW(KOQVEC+KI*KCQVEC+JQVECH)                               QMACRO13
      KCH(KI)=NINT (QCH(KI))                                            QMACRO14
      QCT(KI)=QZ(KI)/QP(KI)                                             QMACRO15
      QDB(KI)=RW(KOQVEC+KI*KCQVEC+JQVEDB)                               QMACRO16
      QZB(KI)=RW(KOQVEC+KI*KCQVEC+JQVEZB)                               QMACRO17
      QDBS2(KI)=RW(KOQVEC+KI*KCQVEC+JQVESD)                             QMACRO18
      QZBS2(KI)=RW(KOQVEC+KI*KCQVEC+JQVESZ)                             QMACRO19
      QBC2(KI)=RW(KOQVEC+KI*KCQVEC+JQVECB)                              QMACRO20
      QSIG(KI,KI1,KI2)=RW(KOQVEC+KI*KCQVEC+KMATIX(KI1,KI2)+JQVEEM)      QMACRO21
      XSIG(KI)=RW(KOQVEC+KI*KCQVEC+JQVEEM).GE.0.                        QMACRO22
      QSMAT(KI,KI1)=RW(KOQVEC+KI*KCQVEC+KI1+JQVEEM)                     QMACRO23
      QSIGX(KI)=QSQT(QSMAT(KI,0))                                       QMACRO24
      QSIGY(KI)=QSQT(QSMAT(KI,2))                                       QMACRO25
      QSIGZ(KI)=QSQT(QSMAT(KI,5))                                       QMACRO26
      QSIGEE(KI)=RW(KOQVEC+KI*KCQVEC+JQVEEM+9)                          QMACRO27
      QSIGE(KI)=QSQT(QSIGEE(KI))                                        QMACRO28
      QSIGPP(KI)=(QX(KI)**2*QSMAT(KI,0)+QY(KI)**2*QSMAT(KI,2)+          QMACRO29
     & QZ(KI)**2*QSMAT(KI,5)+2.*(QX(KI)*(QY(KI)*QSMAT(KI,1)+            QMACRO30
     & QZ(KI)*QSMAT(KI,3))+QY(KI)*QZ(KI)*QSMAT(KI,4)))/QP(KI)**2        QMACRO31
      QSIGP(KI)=QSQT(QSIGPP(KI))                                        QMACRO32
      QSIGMM(KI)=QM(KI)*(QE(KI)**2*QSMAT(KI,9)+QX(KI)**2*QSMAT(KI,0)+   QMACRO33
     & QY(KI)**2*QSMAT(KI,2)+QZ(KI)**2*QSMAT(KI,5)+2.*(QX(KI)*(QY(KI)*  QMACRO34
     & QSMAT(KI,1)+QZ(KI)*QSMAT(KI,3))+QY(KI)*QZ(KI)*QSMAT(KI,4)-       QMACRO35
     & QE(KI)*(QX(KI)*QSMAT(KI,6)+QY(KI)*QSMAT(KI,7)+                   QMACRO36
     & QZ(KI)*QSMAT(KI,8))))/AMAX1(QM(KI)**3,1.E-16)                    QMACRO37
      QSIGM(KI)=QSQT(QSIGMM(KI))                                        QMACRO38
      QMCHIF(KI)=RW(KOQVEC+KI*KCQVEC+JQVECF)                            QMACRO39
      QPH(KI)=ATG(QY(KI),QX(KI))                                        QMACRO40
      QPT(KI)=SQRT(QX(KI)**2+QY(KI)**2)                                 QMACRO41
      KTN(KI)=IW(KOQVEC+KI*KCQVEC+JQVETN)                               QMACRO42
      KMC(KI)=KCLARM(IW(KOQVEC+KI*KCQVEC+JQVECL))                       QMACRO43
      KBMASK(KI,KI1)=IW(KOQVEC+KI*KCQVEC+KI1+JQVEBM-1)                  QMACRO44
      KCALFL(KI)=IW(KJQDET(KI)+JQDECF)                                  QMACRO45
      KCHGD(KI,KI1)=IW(KOQLIN+KI1+IW(KJQDET(KI)+JQDEFL))                QMACRO46
      KCLASS(KI)=KCLACO(IW(KOQVEC+KI*KCQVEC+JQVECL))                    QMACRO47
      KDAU(KI,KI1)=IW(KOQLIN+KI1+IW(KOQVEC+KI*KCQVEC+JQVEDL))           QMACRO48
      KECAL(KI,KI1)=IW(KOQLIN+KI1+IW(KJQDET(KI)+JQDEEL))                QMACRO49
      KENDV(KI)=IW(KOQVEC+KI*KCQVEC+JQVEEV)                             QMACRO50
      KFOLLO(KI)=IW(KOQVEC+KI*KCQVEC+JQVENP)                            QMACRO51
C                                                                       QMACRO52
      KHCAL(KI,KI1)=IW(KOQLIN+KI1+IW(KJQDET(KI)+JQDEHL))                QMACRO53
      KLUNDS(KI)=IW(KOQVEC+KI*KCQVEC+JQVEKS)                            QMACRO54
      KNCHGD(KI)=IW(KJQDET(KI)+JQDENF)                                  QMACRO55
      KNECAL(KI)=IW(KJQDET(KI)+JQDENE)                                  QMACRO56
      KNHCAL(KI)=IW(KJQDET(KI)+JQDENH)                                  QMACRO57
      KNMOTH(KI)=IW(KOQVEC+KI*KCQVEC+JQVENO)                            QMACRO58
      KMOTH(KI,KI1)=IW(KOQLIN+KI1+IW(KOQVEC+KI*KCQVEC+JQVEOL))          QMACRO59
      KMTCH(KI,KI1)=IW(KOQMTL+KI1+IW(KOQVEC+KI*KCQVEC+JQVEML))          QMACRO60
      KNDAU(KI)=IW(KOQVEC+KI*KCQVEC+JQVEND)                             QMACRO61
      KNMTCH(KI)=IW(KOQVEC+KI*KCQVEC+JQVENM)                            QMACRO62
      KORIV(KI)=IW(KOQVEC+KI*KCQVEC+JQVEOV)                             QMACRO63
      KSAME(KI)=IW(KOQVEC+KI*KCQVEC+JQVESP)                             QMACRO64
      KSMTCH(KI,KI1)=IW(KOQMTS+KI1+IW(KOQVEC+KI*KCQVEC+JQVEML))         QMACRO65
      KSTABC(KI)=IW(KOQVEC+KI*KCQVEC+JQVESC)                            QMACRO66
      KTPCOD(KI)=IW(KOQVEC+KI*KCQVEC+JQVEPA)                            QMACRO67
      XLOCKN(KI,KI1)=IAND(KBMASK(KI,KI1),KLOCK0(KI1,KMC(KI))).NE.0      QMACRO68
      XLOCK(KI)=IW(KOQVEC+KI*KCQVEC+JQVELK).NE.0.OR.XLOCKN(KI,1).OR.    QMACRO69
     & XLOCKN(KI,2).OR.XLOCKN(KI,3).OR.XLOCKN(KI,4).OR.                 QMACRO70
     & XLOCKN(KI,5).OR.XLOCKN(KI,6).OR.XLOCKN(KI,7).OR.                 QMACRO71
     & XLOCKN(KI,8).OR.XLOCKN(KI,9).OR.XLOCKN(KI,10).OR.                QMACRO72
     & XLOCKN(KI,11).OR.XLOCKN(KI,12).OR.XLOCKN(KI,13).OR.              QMACRO73
     & XLOCKN(KI,14)                                                    QMACRO74
      XMC(KI)=KCLARM(IW(KOQVEC+KI*KCQVEC+JQVECL)).NE.1                  QMACRO75
      QRDFL(KI,KI1)=RW(KOQVEC+KI*KCQVEC+JQVEUS+KI1-1)                   QMACRO76
      KRDFL(KI,KI1)=IW(KOQVEC+KI*KCQVEC+JQVEUS+KI1-1)                   QMACRO77
      XCAL(KI)=IW(KJQDET(KI)+JQDECF).NE.0                               QMACRO78
      XECAL(KI)=IABS(IW(KJQDET(KI)+JQDECF)).EQ.1                        QMACRO79
      XHCAL(KI)=IABS(IW(KJQDET(KI)+JQDECF)).EQ.2                        QMACRO80
C                                                                       QMACRO81
      QVX(KI)=RW(KOQVRT+KI*KCQVRT+JQVRVX)                               QMACRO82
      QVY(KI)=RW(KOQVRT+KI*KCQVRT+JQVRVY)                               QMACRO83
      QVZ(KI)=RW(KOQVRT+KI*KCQVRT+JQVRVZ)                               QMACRO84
      KVN(KI)=IW(KOQVRT+KI*KCQVRT+JQVRVN)                               QMACRO85
      KVTYPE(KI)=IW(KOQVRT+KI*KCQVRT+JQVRTY)                            QMACRO86
      KVINCP(KI)=IW(KOQVRT+KI*KCQVRT+JQVRIP)                            QMACRO87
      KVNDAU(KI)=IW(KOQVRT+KI*KCQVRT+JQVRND)                            QMACRO88
      KVYV0V(KI)=IW(KOQVRT+KI*KCQVRT+JQVRAY)                            QMACRO89
      KVFVER(KI)=IW(KOQVRT+KI*KCQVRT+JQVRAF)                            QMACRO90
      KVDAU(KI,KI1)=IW(KOQLIN+KI1+IW(KOQVRT+KI*KCQVRT+JQVRDL))          QMACRO91
      QVEM(KI,KI1,KI2)=RW(KOQVRT+KI*KCQVRT+KMATIX(KI1,KI2)+JQVREM)      QMACRO92
      QVCHIF(KI)=RW(KOQVRT+KI*KCQVRT+JQVRCF)                            QMACRO93
      QVDIF2(KI1,KI2)=SQRT((QVX(KI1)-QVX(KI2))**2+(QVY(KI1)-            QMACRO94
     &QVY(KI2))**2)                                                     QMACRO95
      QVDIF3(KI1,KI2)=SQRT((QVX(KI1)-QVX(KI2))**2+(QVY(KI1)-            QMACRO96
     &QVY(KI2))**2+ (QVZ(KI1)-QVZ(KI2))**2)                             QMACRO97
C                                                                       QMACRO98
      QMSQ2(KI1,KI2)=(QE(KI1)+QE(KI2))**2-(QX(KI1)+QX(KI2))**2-         QMACRO99
     & (QY(KI1)+QY(KI2))**2-(QZ(KI1)+QZ(KI2))**2                        QMACR100
      QMSQ3(KI1,KI2,KI3)=(QE(KI1)+QE(KI2)+QE(KI3))**2-                  QMACR101
     & (QX(KI1)+QX(KI2)+QX(KI3))**2-(QY(KI1)+QY(KI2)+QY(KI3))**2-       QMACR102
     & (QZ(KI1)+QZ(KI2)+QZ(KI3))**2                                     QMACR103
      QMSQ4(KI1,KI2,KI3,KI4)=(QE(KI1)+QE(KI2)+QE(KI3)+QE(KI4))**2-      QMACR104
     & (QX(KI1)+QX(KI2)+QX(KI3)+QX(KI4))**2-(QY(KI1)+QY(KI2)+           QMACR105
     & QY(KI3)+QY(KI4))**2-(QZ(KI1)+QZ(KI2)+QZ(KI3)+QZ(KI4))**2         QMACR106
      QM2(KI1,KI2)=QSQT(QMSQ2(KI1,KI2))                                 QMACR107
      QM3(KI1,KI2,KI3)=QSQT(QMSQ3(KI1,KI2,KI3))                         QMACR108
      QM4(KI1,KI2,KI3,KI4)=QSQT(QMSQ4(KI1,KI2,KI3,KI4))                 QMACR109
      QDMSQ(KI1,KI2)=(QE(KI1)-QE(KI2))**2-(QX(KI1)-QX(KI2))**2-         QMACR110
     & (QY(KI1)-QY(KI2))**2-(QZ(KI1)-QZ(KI2))**2                        QMACR111
      QBETA(KI)=QP(KI)/QE(KI)                                           QMACR112
      QGAMMA(KI)=1./SQRT((1.-QBETA(KI))*(1.+QBETA(KI)))                 QMACR113
      QDOT3(KI1,KI2)=QX(KI1)*QX(KI2)+QY(KI1)*QY(KI2)+QZ(KI1)*QZ(KI2)    QMACR114
      QDOT4(KI1,KI2)=QE(KI1)*QE(KI2)-QDOT3(KI1,KI2)                     QMACR115
      QCOSA(KI1,KI2)=QDOT3(KI1,KI2)/(QP(KI1)*QP(KI2))                   QMACR116
      QPPAR(KI1,KI2)=QDOT3(KI1,KI2)/QP(KI2)                             QMACR117
      QPPER(KI1,KI2)=SQRT((QY(KI1)*QZ(KI2)-QZ(KI1)*QY(KI2))**2+         QMACR118
     & (QZ(KI1)*QX(KI2)-QX(KI1)*QZ(KI2))**2+                            QMACR119
     & (QX(KI1)*QY(KI2)-QY(KI1)*QX(KI2))**2)/QP(KI2)                    QMACR120
      XSAME(KI1,KI2)=IAND(KBMASK(KI1,1),KBMASK(KI2,1)).NE.0.OR.         QMACR121
     & IAND(KBMASK(KI1,2),KBMASK(KI2,2)).NE.0.OR.                       QMACR122
     & IAND(KBMASK(KI1,3),KBMASK(KI2,3)).NE.0.OR.                       QMACR123
     & IAND(KBMASK(KI1,4),KBMASK(KI2,4)).NE.0.OR.                       QMACR124
     & IAND(KBMASK(KI1,5),KBMASK(KI2,5)).NE.0.OR.                       QMACR125
     & IAND(KBMASK(KI1,6),KBMASK(KI2,6)).NE.0.OR.                       QMACR126
     & IAND(KBMASK(KI1,7),KBMASK(KI2,7)).NE.0.OR.                       QMACR127
     & IAND(KBMASK(KI1,8),KBMASK(KI2,8)).NE.0.OR.                       QMACR128
     & IAND(KBMASK(KI1,9),KBMASK(KI2,9)).NE.0.OR.                       QMACR129
     & IAND(KBMASK(KI1,10),KBMASK(KI2,10)).NE.0                         QMACR130
C                                                                       QMACR131
      QCMASS(KI)=RW(KOQPAR+KI*KCQPAR+JQPAMA)                            QMACR132
      QCCHAR(KI)=RW(KOQPAR+KI*KCQPAR+JQPACH)                            QMACR133
      QCLIFE(KI)=RW(KOQPAR+KI*KCQPAR+JQPALT)                            QMACR134
      QCWIDT(KI)=RW(KOQPAR+KI*KCQPAR+JQPAWI)                            QMACR135
      XCEQOR(KI,KI1)=KTPCOD(KI).EQ.KI1.OR.                              QMACR136
     & KTPCOD(KI).EQ.IW(KOQPAR+KI1*KCQPAR+JQPAAN)                       QMACR137
      XCEQU(KI,KI1)=KTPCOD(KI).EQ.KI1                                   QMACR138
C    QCFDET                                                             QMACR139
      XFRF(KI)=IW(KJQDET(KI)+JQDEAF).NE.KQZER                           QMACR140
      QFRFIR(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFIR)                       QMACR141
      QFRFTL(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFTL)                       QMACR142
      QFRFP0(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFP0)                       QMACR143
      QFRFD0(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFD0)                       QMACR144
      QFRFZ0(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFZ0)                       QMACR145
      QFRFAL(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFAL)                       QMACR146
      QFRFEM(KI,KI1,KI2)=RW(IW(KJQDET(KI)+JQDEAF)+                      QMACR147
     & KMATIX(KI1,KI2)+JFRFEM)                                          QMACR148
      QFRFC2(KI)=RW(IW(KJQDET(KI)+JQDEAF)+JFRFC2)                       QMACR149
      KFRFDF(KI)=IW(IW(KJQDET(KI)+JQDEAF)+JFRFDF)                       QMACR150
      KFRFNO(KI)=IW(IW(KJQDET(KI)+JQDEAF)+JFRFNO)                       QMACR151
C                                                                       QMACR152
      KFRTNV(KI)=IW(IW(KJQDET(KI)+JQDEAL)+JFRTNV)                       QMACR153
      KFRTNI(KI)=IW(IW(KJQDET(KI)+JQDEAL)+JFRTNI)                       QMACR154
      KFRTNE(KI)=IW(IW(KJQDET(KI)+JQDEAL)+JFRTNE)                       QMACR155
      KFRTNT(KI)=IW(IW(KJQDET(KI)+JQDEAL)+JFRTNT)                       QMACR156
      KFRTNR(KI)=IW(IW(KJQDET(KI)+JQDEAL)+JFRTNR)                       QMACR157
C                                                                       QMACR158
      KFRIBP(KI)=IW(IW(KJQDET(KI)+JQDEFI)+JFRIBP)                       QMACR159
      KFRIDZ(KI)=IW(IW(KJQDET(KI)+JQDEFI)+JFRIDZ)                       QMACR160
      KFRIBC(KI)=IW(IW(KJQDET(KI)+JQDEFI)+JFRIBC)                       QMACR161
      KFRIDC(KI)=IW(IW(KJQDET(KI)+JQDEFI)+JFRIDC)                       QMACR162
      QFRIPE(KI)=RW(IW(KJQDET(KI)+JQDEFI)+JFRIPE)                       QMACR163
      QFRIPM(KI)=RW(IW(KJQDET(KI)+JQDEFI)+JFRIPM)                       QMACR164
      QFRIPI(KI)=RW(IW(KJQDET(KI)+JQDEFI)+JFRIPI)                       QMACR165
      QFRIPK(KI)=RW(IW(KJQDET(KI)+JQDEFI)+JFRIPK)                       QMACR166
      QFRIPP(KI)=RW(IW(KJQDET(KI)+JQDEFI)+JFRIPP)                       QMACR167
      QFRINK(KI)=RW(IW(KJQDET(KI)+JQDEFI)+JFRINK)                       QMACR168
      KFRIQF(KI)=IW(IW(KJQDET(KI)+JQDEFI)+JFRIQF)                       QMACR169
      XFRIQF(KI)=KFRIQF(KI).EQ.1.OR.KFRIQF(KI).EQ.3                     QMACR170
C                                                                       QMACR171
      KNTEX(KI)=IW(KJQDET(KI)+JQDENT)                                   QMACR172
      XTEX(KI)=IW(KJQDET(KI)+JQDENT).NE.0                               QMACR173
      KTEXSI(KI,KI1)=IW(IW(KJQDET(KI)+KI1+JQDENT)+JTEXSI)               QMACR174
      QTEXTM(KI,KI1)=RW(IW(KJQDET(KI)+KI1+JQDENT)+JTEXTM)               QMACR175
      QTEXTL(KI,KI1)=RW(IW(KJQDET(KI)+KI1+JQDENT)+JTEXTL)               QMACR176
      KTEXNS(KI,KI1)=IW(IW(KJQDET(KI)+KI1+JQDENT)+JTEXNS)               QMACR177
      QTEXAD(KI,KI1)=RW(IW(KJQDET(KI)+KI1+JQDENT)+JTEXAD)               QMACR178
C                                                                       QMACR179
      XEID(KI)=IW(KJQDET(KI)+JQDEAE).NE.KQZER                           QMACR180
      KEIDIF(KI)=IW(IW(KJQDET(KI)+JQDEAE)+JEIDIF)                       QMACR181
      QEIDRI(KI,KI1)=RW(IW(KJQDET(KI)+JQDEAE)+KI1+JEIDR1-1)             QMACR182
      QEIDEC(KI)=RW(IW(KJQDET(KI)+JQDEAE)+JEIDEC)                       QMACR183
      KEIDIP(KI)=IW(IW(KJQDET(KI)+JQDEAE)+JEIDIP)                       QMACR184
      QEIDEI(KI,KI1)=RW(IW(KJQDET(KI)+JQDEAE)+KI1+JEIDE1-1)             QMACR185
C                                                                       QMACR186
      XHMA(KI)=IW(KJQDET(KI)+JQDEAH).NE.KQZER                           QMACR187
      KHMANF(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMANF)                       QMACR188
      KHMANE(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMANE)                       QMACR189
      KHMANL(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMANL)                       QMACR190
      KHMAMH(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMAMH)                       QMACR191
      KHMAIG(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMAIG)                       QMACR192
      QHMAED(KI)=RW(IW(KJQDET(KI)+JQDEAH)+JHMAED)                       QMACR193
      QHMACS(KI)=RW(IW(KJQDET(KI)+JQDEAH)+JHMACS)                       QMACR194
      KHMAND(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMAND)                       QMACR195
      KHMAIE(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMAIE)                       QMACR196
      KHMAIT(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMAIT)                       QMACR197
      KHMAIF(KI)=IW(IW(KJQDET(KI)+JQDEAH)+JHMAIF)                       QMACR198
C                                                                       QMACR199
      XMCA(KI)=IW(KJQDET(KI)+JQDEAM).NE.KQZER                           QMACR200
      KMCANH(KI,KI1)=IW(IW(KJQDET(KI)+JQDEAM)+KI1+JMCANH-1)             QMACR201
      QMCADH(KI,KI1)=RW(IW(KJQDET(KI)+JQDEAM)+KI1+JMCADH-1)             QMACR202
      QMCADC(KI,KI1)=RW(IW(KJQDET(KI)+JQDEAM)+KI1+JMCADC-1)             QMACR203
      QMCAAM(KI)=RW(IW(KJQDET(KI)+JQDEAM)+JMCAAM)                       QMACR204
      QMCAAC(KI)=RW(IW(KJQDET(KI)+JQDEAM)+JMCAAC)                       QMACR205
C                                                                       QMACR206
      XMUI(KI)=IW(KJQDET(KI)+JQDEMU).NE.KQZER                           QMACR207
      KMUIIF(KI)=IW(IW(KJQDET(KI)+JQDEMU)+JMUIIF)                       QMACR208
      QMUISR(KI)=RW(IW(KJQDET(KI)+JQDEMU)+JMUISR)                       QMACR209
      QMUIDM(KI)=RW(IW(KJQDET(KI)+JQDEMU)+JMUIDM)                       QMACR210
      KMUIST(KI)=IW(IW(KJQDET(KI)+JQDEMU)+JMUIST)                       QMACR211
      KMUITN(KI)=IW(IW(KJQDET(KI)+JQDEMU)+JMUITN)                       QMACR212
C                                                                       QMACR213
      XPEC(KI)=IW(KJQDET(KI)+JQDEEC).NE.KQZER                           QMACR214
      QPECER(KI)=RW(IW(KJQDET(KI)+JQDEEC)+JPECER)                       QMACR215
      QPECE1(KI)=RW(IW(KJQDET(KI)+JQDEEC)+JPECE1)                       QMACR216
      QPECE2(KI)=RW(IW(KJQDET(KI)+JQDEEC)+JPECE2)                       QMACR217
      QPECTH(KI)=RW(IW(KJQDET(KI)+JQDEEC)+JPECTH)                       QMACR218
      QPECPH(KI)=RW(IW(KJQDET(KI)+JQDEEC)+JPECPH)                       QMACR219
      QPECEC(KI)=RW(IW(KJQDET(KI)+JQDEEC)+JPECEC)                       QMACR220
      KPECKD(KI)=IW(IW(KJQDET(KI)+JQDEEC)+JPECKD)                       QMACR221
      KPECCC(KI)=IW(IW(KJQDET(KI)+JQDEEC)+JPECCC)                       QMACR222
      KPECRB(KI)=IW(IW(KJQDET(KI)+JQDEEC)+JPECRB)                       QMACR223
      KPECPC(KI)=IW(IW(KJQDET(KI)+JQDEEC)+JPECPC)                       QMACR224
C                                                                       QMACR225
      XPEP(KI)=IW(KJQDET(KI)+JQDEET).NE.KQZER                           QMACR226
      QPEPT1(KI)=RW(IW(KJQDET(KI)+JQDEET)+JPEPT1)                       QMACR227
      QPEPP1(KI)=RW(IW(KJQDET(KI)+JQDEET)+JPEPP1)                       QMACR228
      QPEPT3(KI)=RW(IW(KJQDET(KI)+JQDEET)+JPEPT3)                       QMACR229
      QPEPP3(KI)=RW(IW(KJQDET(KI)+JQDEET)+JPEPP3)                       QMACR230
C                                                                       QMACR231
      XPHC(KI)=IW(KJQDET(KI)+JQDEHC).NE.KQZER                           QMACR232
      QPHCER(KI)=RW(IW(KJQDET(KI)+JQDEHC)+JPHCER)                       QMACR233
      QPHCTH(KI)=RW(IW(KJQDET(KI)+JQDEHC)+JPHCTH)                       QMACR234
      QPHCPH(KI)=RW(IW(KJQDET(KI)+JQDEHC)+JPHCPH)                       QMACR235
      QPHCEC(KI)=RW(IW(KJQDET(KI)+JQDEHC)+JPHCEC)                       QMACR236
      KPHCKD(KI)=IW(IW(KJQDET(KI)+JQDEHC)+JPHCKD)                       QMACR237
      KPHCCC(KI)=IW(IW(KJQDET(KI)+JQDEHC)+JPHCCC)                       QMACR238
      KPHCRB(KI)=IW(IW(KJQDET(KI)+JQDEHC)+JPHCRB)                       QMACR239
      KPHCPC(KI)=IW(IW(KJQDET(KI)+JQDEHC)+JPHCPC)                       QMACR240
C                                                                       QMACR241
      XEFO(KI)=IW(KJQDET(KI)+JQDEEF).NE.KQZER                           QMACR242
      QEFOWE(KI)=RW(IW(KJQDET(KI)+JQDEEF)+JEFOWE)                       QMACR243
      KEFOTY(KI)=IW(IW(KJQDET(KI)+JQDEEF)+JEFOTY)                       QMACR244
      KEFOLE(KI)=IW(IW(KJQDET(KI)+JQDEEF)+JEFOLE)                       QMACR245
      KEFOLT(KI)=IW(IW(KJQDET(KI)+JQDEEF)+JEFOLT)                       QMACR246
      KEFOLH(KI)=IW(IW(KJQDET(KI)+JQDEEF)+JEFOLH)                       QMACR247
      KEFOLC(KI)=IW(IW(KJQDET(KI)+JQDEEF)+JEFOLC)                       QMACR248
      KEFOLJ(KI)=IW(IW(KJQDET(KI)+JQDEEF)+JEFOLJ)                       QMACR249
C                                                                       QMACR250
      XPCQ(KI)=IW(KJQDET(KI)+JQDEPC).NE.KQZER                           QMACR251
      KPCQNA(KI)=IW(IW(KJQDET(KI)+JQDEPC)+JPCQNA)                       QMACR252
C                                                                       QMACR253
      XEGP(KI)=IW(KJQDET(KI)+JQDEEG).NE.KQZER                           QMACR254
      QEGPR1(KI)=RW(IW(KJQDET(KI)+JQDEEG)+JEGPR1)                       QMACR255
      QEGPR2(KI)=RW(IW(KJQDET(KI)+JQDEEG)+JEGPR2)                       QMACR256
      QEGPF4(KI)=RW(IW(KJQDET(KI)+JQDEEG)+JEGPF4)                       QMACR257
      QEGPDM(KI)=RW(IW(KJQDET(KI)+JQDEEG)+JEGPDM)                       QMACR258
      KEGPST(KI)=IW(IW(KJQDET(KI)+JQDEEG)+JEGPST)                       QMACR259
      KEGPQU(KI)=IW(IW(KJQDET(KI)+JQDEEG)+JEGPQU)                       QMACR260
      KEGPPE(KI)=IW(IW(KJQDET(KI)+JQDEEG)+JEGPPE)                       QMACR261
C                                                                       QMACR262
      XPGP(KI)=IW(KJQDET(KI)+JQDEPG).NE.KQZER.AND.IW(NAPGPC).GT.0       JB941120
      QPGPR1(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPR1)                       QMACR264
      QPGPR2(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPR2)                       QMACR265
      QPGPF4(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPF4)                       QMACR266
      QPGPDM(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPDM)                       QMACR267
      QPGPST(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPST)                       QMACR268
      QPGPQ1(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPQ1)                       QMACR269
      QPGPQ2(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPQ2)                       QMACR270
      QPGPM1(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPM1)                       QMACR271
      QPGPM2(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPM2)                       QMACR272
      QPGPMA(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPMA)                       QMACR273
      QPGPER(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPER)                       QMACR274
      QPGPTR(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPTR)                       QMACR275
      QPGPPR(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGPPR)                       QMACR276
      KPGPQU(KI)=IW(IW(KJQDET(KI)+JQDEPG)+JPGPQU)                       QMACR277
      KPGPPE(KI)=IW(IW(KJQDET(KI)+JQDEPG)+JPGPPE)                       QMACR278
      XPGAC(KI)=IW(KJQDET(KI)+JQDEPG).NE.KQZER.AND.IW(NAPGAC).GT.0      JB941121
      KPGAST(KI)=IW(IW(KJQDET(KI)+JQDEPG)+JPGAST)                       JB941122
      QPGAEF(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGAEF)                       JB941123
      QPGAGC(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGAGC)                       JB941124
      QPGAZS(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGAZS)                       JB941125
      QPGAPL(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGAPL)                       JB941126
      QPGAPH(KI)=RW(IW(KJQDET(KI)+JQDEPG)+JPGAPH)                       JB941127
      KPGAPN(KI)=IW(IW(KJQDET(KI)+JQDEPG)+JPGAPN)                       JB941128
      KPGAFA(KI)=IW(IW(KJQDET(KI)+JQDEPG)+JPGAFA)                       JB941129
      KPGAPE(KI)=IW(IW(KJQDET(KI)+JQDEPG)+JPGAPE)                       JB941130
C                                                                       JB941131
C                                                                       QMACR279
      XYV0(KI)=KVYV0V(KENDV(KI)).NE.0                                   QMACR280
      KYV0K1(KI)=IW(KVYV0V(KENDV(KI))+JYV0K1)                           QMACR281
      KYV0K2(KI)=IW(KVYV0V(KENDV(KI))+JYV0K2)                           QMACR282
      QYV0VX(KI)=RW(KVYV0V(KENDV(KI))+JYV0VX)                           QMACR283
      QYV0VY(KI)=RW(KVYV0V(KENDV(KI))+JYV0VY)                           QMACR284
      QYV0VZ(KI)=RW(KVYV0V(KENDV(KI))+JYV0VZ)                           QMACR285
      QYV0X1(KI)=RW(KVYV0V(KENDV(KI))+JYV0X1)                           QMACR286
      QYV0X2(KI)=RW(KVYV0V(KENDV(KI))+JYV0X2)                           QMACR287
      QYV0C2(KI)=RW(KVYV0V(KENDV(KI))+JYV0C2)                           QMACR288
      KYV0IC(KI)=IW(KVYV0V(KENDV(KI))+JYV0IC)                           QMACR289
      QYV0DM(KI)=RW(KVYV0V(KENDV(KI))+JYV0DM)                           QMACR290
      QYV0S1(KI)=RW(KVYV0V(KENDV(KI))+JYV0S1)                           QMACR291
      QYV0S2(KI)=RW(KVYV0V(KENDV(KI))+JYV0S2)                           QMACR292
C                                                                       JB941132
      XLEPTG(KI)=IW(KJQDET(KI)+JQDEPD).NE.KQZER                         JB941133
      KLEPPA(KI)=IW(IW(KJQDET(KI)+JQDEPD)+JPDLPA)                       JB941134
      KLEPJT(KI)=IW(IW(KJQDET(KI)+JQDEPD)+JPDLJT)+KLFJET-1              JB941135
      QLEPPI(KI)=RW(IW(KJQDET(KI)+JQDEPD)+JPDLPI)                       JB941136
      QLEPPE(KI)=RW(IW(KJQDET(KI)+JQDEPD)+JPDLPE)                       JB941137
      KLEPVP(KI)=IW(IW(KJQDET(KI)+JQDEPD)+JPDLVP)                       JB941138
C                                                                       JB941139
      XLEPTH(KI)=IW(KJQDET(KI)+JQDEPM).NE.KQZER                         JB941140
      KLEPFL(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLFL)                       JB941141
      KLEPPO(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLPO)                       JB941142
      KLEPCH(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLCH)                       JB941143
      KLEPSP(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLSP)                       JB941144
      KLEPLE(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLLE)                       JB941145
      KLEPME(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLME)                       JB941146
      KLEPKT(KI)=IW(IW(KJQDET(KI)+JQDEPM)+JPMLKT)                       JB941147
C---------------------  end of QMACRO --------------------------------- QMACR293
