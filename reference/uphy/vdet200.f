C======================================================================
C UPDATES FOR
C
C        1. JULIA277:            Updates:
C           ROUTINES:  RFORMA    define format for VZMR
C                      VDXYZT    call to VDXPBZ
C                      VDXPBZ    expand banks in z-view to every wafer
C                      VBKZWF    book bank for each wafer in z-view
C                      VDMULT    calculate position of multiplexed partner
C                      VDISMR    don't do smearing for VDET200
C                      VDMCEF    don't simulate dead strips for VDET200
C
C
C
C
C
C        2. ALEPHLIB205
C           ROUTINES:  VRMWF    wafer number => readout mod. number
C                      VTCLLD
C                      VTFILL   tracking
C                      VTLINK   tracking
C                      VGTMPP
C======================================================================

      SUBROUTINE RFORMA                                                 RFORMA 2
C********************************************************************   RFORMA 3
C! Define formats for BOS banks in JULIA                                FLR27212
C                                                                       FLR27213
C                                                                       FLR27214
C    Author: J.Knobloch  15.3.90                                        FLR27215
C    Modified by: F.Ranjard 25.4.94                                     FLR27216
C                 remove XSGE, XSHI, XSSC banks                         FLR27217
C?                                                                      RFORMA 8
C!======================================================================RFORMA 9
      CALL BKFMT('ALPB',' 2I,(5I)')                                     RFORMA11
      CALL BKFMT('BHAB',' 2I,(5I,17F,I,2F)')                            RFORMA12
      CALL BKFMT('BOMB',' 2I,(2F,I)')                                   RFORMA13
      CALL BKFMT('BOME',' 2I,(4F,I,6F,I)')                              RFORMA14
      CALL BKFMT('BOMP','(I)')                                          RFORMA15
      CALL BKFMT('BOMR','(I)')                                          RFORMA16
      CALL BKFMT('CALO','2I,(F)')                                       RFORMA17
      CALL BKFMT('CCT1','(I)')                                          RFORMA18
      CALL BKFMT('CEXT','2I,(I)')                                       RFORMA19
      CALL BKFMT('CHPR','I')                                            RFORMA20
      CALL BKFMT('CHRL','2I,(I)')                                       RFORMA21
      CALL BKFMT('CHYP','I')                                            RFORMA22
      CALL BKFMT('COCR','(I)')                                          RFORMA23
      CALL BKFMT('CPAR','2I,(I,5F,I)')                                  RFORMA24
      CALL BKFMT('CRL3','2I,(F,5I)')                                    RFORMA25
      CALL BKFMT('CTC2','(I)')                                          RFORMA26
      CALL BKFMT('DHEA','2I,(6I,14F,2I)')                               RFORMA27
      CALL BKFMT('D4CD','I')                                            GTY273 6
      CALL BKFMT('EAGC','2I,(2F)')                                      MNM27432
      CALL BKFMT('E4DE','(I)')                                          RFORMA28
      CALL BKFMT('EBOK','2I,(2I)')                                      RFORMA29
      CALL BKFMT('EBOS','2I,(F,3I)')                                    RFORMA30
      CALL BKFMT('ECHE','2I,(2F)')                                      RFORMA31
      CALL BKFMT('ECLU','2I,(I,19F,I)')                                 RFORMA32
      CALL BKFMT('ECOB','2I,(4F,I,5F,6I)')                              RFORMA33
      CALL BKFMT('ECRQ','2I,(I)')                                       RFORMA34
      CALL BKFMT('ECST','2I,(I,1F,3I)')                                 RFORMA35
      CALL BKFMT('ECT1','(I)')                                          RFORMA36
      CALL BKFMT('ECTE','2I,(I,F)')                                     RFORMA37
      CALL BKFMT('ECTY','2I,(I)')                                       RFORMA38
      CALL BKFMT('EDDB','(I)')                                          RFORMA39
      CALL BKFMT('EDST',' 2I,(3I,F,6I)')                                RFORMA40
      CALL BKFMT('EFET','2I,(8F,I)')                                    RFORMA41
      CALL BKFMT('EFOL','2I,(5F,6I)')                                   RFORMA42
      CALL BKFMT('EJET','2I,(4F)')                                      RFORMA43
      CALL BKFMT('EGRP','2I,(8F)')                                      RFORMA44
      CALL BKFMT('EGPR','2I,(2I)')                                      RFORMA45
      CALL BKFMT('ECIL','2I,(I,2F)')                                    RFORMA46
      CALL BKFMT('ELZE','2I,(6F)')                                      RFORMA47
      CALL BKFMT('EGMA','2I,(3F,I)')                                    RFORMA48
      CALL BKFMT('EHYP','2I,(I,4F,3I)')                                 RFORMA49
      CALL BKFMT('EIBR','2I,(6F,2I)')                                   RFORMA50
      CALL BKFMT('EIDT','2I,(I,8F,I,3F,2I)')                            RFORMA51
      CALL BKFMT('EIGA','2I,(3F,I)')                                    RFORMA52
      CALL BKFMT('EMIP','(I)')                                          RFORMA53
      CALL BKFMT('EMSK','2I,(30F)')                                     RFORMA54
      CALL BKFMT('EPAR','2I,(I,7F,2I)')                                 RFORMA55
      CALL BKFMT('EPRS','(I)')                                          RFORMA56
      CALL BKFMT('ERL3','(I)')                                          RFORMA57
      CALL BKFMT('ERRF',' 2I,(11I)')                                    RFORMA58
      CALL BKFMT('ESDA',' 2I,(3I,F,7I)')                                RFORMA59
      CALL BKFMT('ESTO','2I,(33F,3I)')                                  RFORMA60
      CALL BKFMT('ETC2','(I)')                                          RFORMA61
      CALL BKFMT('ETCK','(I)')                                          RFORMA62
      CALL BKFMT('ETKC','2I,(5I)')                                      RFORMA63
      CALL BKFMT('ETCO','2I,(2F)')                                      RFORMA64
      CALL BKFMT('ETP1','2I,(8F,I)')                                    RFORMA65
      CALL BKFMT('EZTH','2I,(I,3F)')                                    RFORMA66
      CALL BKFMT('EVEH','(I)')                                          RFORMA67
      CALL BKFMT('FICL',' 2I,(I)')                                      RFORMA68
      CALL BKFMT('FRFT',' 2I,(28F,2I)')                                 RFORMA69
      CALL BKFMT('FRID',' 2I,(4I,6F,I)')                                RFORMA70
      CALL BKFMT('FRTL',' 2I,(8I)')                                     RFORMA71
      CALL BKFMT('FTCL',' 2I,(I)')                                      RFORMA72
      CALL BKFMT('FVCL',' 2I,(I)')                                      RFORMA73
      CALL BKFMT('H1EC','2I,(4F)')                                      RFORMA74
      CALL BKFMT('H2EC','2I,(8F)')                                      RFORMA75
      CALL BKFMT('H3EC','2I,(16F)')                                     RFORMA76
      CALL BKFMT('H4EC','2I,(4F)')                                      RFORMA77
      CALL BKFMT('HCLU','2I,(I,15F,I,F,2I)')                            RFORMA78
      CALL BKFMT('HCTB','2I,(4F,2I)')                                   RFORMA79
      CALL BKFMT('HCTE','2I,(I,F)')                                     RFORMA80
      CALL BKFMT('HLTU','2I,(F,I)')                                     COR27154
      CALL BKFMT('HMAD','2I,(5I,2F,5I)')                                RFORMA81
      CALL BKFMT('HNGR','2I,(I)')                                       RFORMA82
      CALL BKFMT('HPCO','2I,(I,3F,4I,4F,3I)')                           RFORMA83
      CALL BKFMT('HPDS','2I,(I,2F)')                                    RFORMA84
      CALL BKFMT('HROA','2I,(2F,2I)')                                   RFORMA85
      CALL BKFMT('HSDA','2I,(3I,F,5I)')                                 RFORMA86
      CALL BKFMT('HSPE','2I,(I)')                                       RFORMA87
      CALL BKFMT('HSPH','2I,(I)')                                       RFORMA88
      CALL BKFMT('HSTO','2I,(33F,3I)')                                  RFORMA89
      CALL BKFMT('HTUB','2I,(3I,2F,2I)')                                RFORMA90
      CALL BKFMT('ICCO','2I,(5F)')                                      RFORMA91
      CALL BKFMT('ICCO','2I,(5F)')                                      RFORMA92
      CALL BKFMT('ICTR','2I,(I,F,I)')                                   RFORMA93
      CALL BKFMT('IDCR','I')                                            RFORMA94
      CALL BKFMT('IDIG','I')                                            RFORMA95
      CALL BKFMT('IGCL','I')                                            RFORMA96
      CALL BKFMT('IGTL','I')                                            RFORMA97
      CALL BKFMT('IPJT','I')                                            RFORMA98
      CALL BKFMT('IRJT','I')                                            RFORMA99
      CALL BKFMT('ITCO','2I,(I,7F)')                                    RFORM100
      CALL BKFMT('ITDI','B16')                                          RFORM101
      CALL BKFMT('ITFT','2I,(28F,2I)')                                  RFORM102
      CALL BKFMT('IZDV','I')                                            RFORM103
      CALL BKFMT('JBER','2I,(I)')                                       RFORM104
      CALL BKFMT('JCAR','A')                                            RFORM105
      CALL BKFMT('JCON','2I,(9F,2I,37F)')                               FLR271B1
      CALL BKFMT('JCON','2I,(9F,2I,2F)')                                COR27155
      CALL BKFMT('JEDS','I')                                            RFORM107
      CALL BKFMT('JEST','2I,(7I)')                                      RFORM108
      CALL BKFMT('JPAS','2I,(3I,9F,4I)')                                RFORM109
      CALL BKFMT('JSUM','2I,(10I,13F,3I,22F,2I)')                       RFORM110
      CALL BKFMT('JTDX','2I,(37F)')                                     COR27205
      CALL BKFMT('JTRE','I')                                            COR27206
      CALL BKFMT('LACC',' 2I,(9I)')                                     RFORM111
      CALL BKFMT('LALI',' 2I,(3I,11F,I)')                               RFORM112
      CALL BKFMT('LBAK',' 2I,(26F)')                                    RFORM113
      CALL BKFMT('LCAL',' 2I,(3I,A,I,24F)')                             RFORM114
      CALL BKFMT('LCLU',' 2I,(23F,3I)')                                 RFORM115
      CALL BKFMT('LCRE',' 2I,(3I,11F,8I,4F,I,6F)')                      RFORM116
      CALL BKFMT('LDST',' 2I,(5I)')                                     RFORM117
      CALL BKFMT('LDWP',' 2I,(3I)')                                     RFORM118
      CALL BKFMT('LECA',' 2I,(3I,2F)')                                  RFORM119
      CALL BKFMT('LEHI',' 2I,(34F)')                                    RFORM120
      CALL BKFMT('LIDT',' 2I,(I,15F,4I)')                               RFORM121
      CALL BKFMT('LIFL',' 2I,(4I)')                                     RFORM122
      CALL BKFMT('LLAY',' 2I,(5I,F,I)')                                 RFORM123
      CALL BKFMT('LMAP',' 2I,(I)')                                      RFORM124
      CALL BKFMT('LMTY',' 2I,(3I,A,9I,17F)')                            RFORM125
      CALL BKFMT('LOBJ',' 2I,(I,3F,2I,4F,I)')                           RFORM126
      CALL BKFMT('LOCL',' 2I,(3F)')                                     RFORM127
      CALL BKFMT('LOLE','2I,(I)')                                       RFORM128
      CALL BKFMT('LONL',' 2I,(8F)')                                     RFORM129
      CALL BKFMT('LPDA',' 2I,(I,39F)')                                  RFORM130
      CALL BKFMT('LPMO',' 2I,(3I,A,2I)')                                RFORM131
      CALL BKFMT('LRWG',' 2I,(23I)')                                    RFORM132
      CALL BKFMT('LSCO',' 2I,(3I,A,I,6F,2I)')                           RFORM133
      CALL BKFMT('LSDA',' 2I,(I,3F,2I)')                                RFORM134
      CALL BKFMT('LSIN',' 2I,(4I,3F,2I)')                               RFORM135
      CALL BKFMT('LSLO',' 2I,(3I,A,11F,2I)')                            RFORM136
      CALL BKFMT('LSTA',' 2I,10F')                                      RFORM137
      CALL BKFMT('LTRK',' 2I,(3F,2I)')                                  RFORM138
      CALL BKFMT('LUMI',' 2I,(20F)')                                    RFORM139
      CALL BKFMT('LUPA','2I,(8I,31F,4I,18F,2I,6F,2I)')                  RFORM140
      CALL BKFMT('LVHI',' 2I,(62F)')                                    RFORM141
      CALL BKFMT('LWRG',' 2I,(4I,5F,I)')                                RFORM142
      CALL BKFMT('MCAD','2I,(2I,6F,I)')                                 RFORM143
      CALL BKFMT('MHIT','2I,(3I,5F)')                                   RFORM144
      CALL BKFMT('MONE','2I,(31I,39F,5I,2F,4I,3F)')                     RFORM145
      CALL BKFMT('MTHR','2I,(I,F,I)')                                   RFORM146
      CALL BKFMT('MUEX','2I,(9F)')                                      RFORM147
      CALL BKFMT('MUID','2I,(I,2F,2I)')                                 RFORM148
      CALL BKFMT('PASL','I')                                            RFORM149
      CALL BKFMT('PCHA',' 2I,(11F)')                                    RFORM150
      CALL BKFMT('PCHY','2I,(I)')                                       RFORM151
      CALL BKFMT('PCOB','2I,(I)')                                       RFORM152
      CALL BKFMT('PCOI','I')                                            RFORM153
      CALL BKFMT('PCPA','2I,(I,5F,I)')                                  RFORM154
      CALL BKFMT('PCRL','2I,(I)')                                       RFORM155
      CALL BKFMT('PECO','2I,(6F,4I)')                                   RFORM156
      CALL BKFMT('PEHY','2I,(I)')                                       RFORM157
      CALL BKFMT('PEID','2I,(12I)')                                     RFORM158
      CALL BKFMT('PEOB','2I,(4F,I,2F)')                                 RFORM159
      CALL BKFMT('PEOT','2I,(9F,I)')                                    RFORM160
      CALL BKFMT('PEPT','2I,(4F)')                                      RFORM161
      CALL BKFMT('PEST','2I,(I,F,3I)')                                  RFORM162
      CALL BKFMT('PEWI','(I)')                                          RFORM163
      CALL BKFMT('PWEI','(I)')                                          PCI27624
      CALL BKFMT('PFRF',' 2I,(12F,17I)')                                RFORM164
      CALL BKFMT('PFRT',' 2I,(5I)')                                     RFORM165
      CALL BKFMT('PHCO','2I,(4F,5I)')                                   RFORM166
      CALL BKFMT('PHHY','2I,(3I)')                                      RFORM167
      CALL BKFMT('PHOB','2I,(3F,I,2F)')                                 RFORM168
      CALL BKFMT('PHST','2I,(2I,F,I)')                                  RFORM169
      CALL BKFMT('PHTO','2I,(6F,I)')                                    RFORM170
      CALL BKFMT('PIDI','I')                                            RFORM171
      CALL BKFMT('PITM','I')                                            RFORM172
      CALL BKFMT('PGAC','2I,(7F,2I,13F,3I)')                            MNM27433
      CALL BKFMT('PGID','2I,(I,8F,I)')                                  RFORM173
      CALL BKFMT('PGPC','2I,(8F,I,8F,I)')                               RFORM174
      CALL BKFMT('PKST','2I,(I,F,I)')                                   RFORM175
      CALL BKFMT('PLPD',' 2I,(2I)')                                     RFORM176
      CALL BKFMT('PLSD',' 2I,(5I)')                                     RFORM177
      CALL BKFMT('PMSK','2I,(I,9F,I)')                                  RFORM178
      CALL BKFMT('PPDS','2I,(I,2F,I)')                                  RFORM179
      CALL BKFMT('PPHY','2I,(3I)')                                      RFORM180
      CALL BKFMT('PPOB','2I,(I,3F,3I,2F)')                              RFORM181
      CALL BKFMT('PPRL','2I,(I)')                                       RFORM182
      CALL BKFMT('PRPW','2I,(3I,3F)')                                   RFORM183
      CALL BKFMT('PRTM','2I,(2I )')                                     RFORM184
      CALL BKFMT('PSCO','I')                                            RFORM185
      CALL BKFMT('PSPO','I')                                            RFORM186
      CALL BKFMT('PSTR','I')                                            RFORM187
      CALL BKFMT('PT2X','I')                                            RFORM188
      CALL BKFMT('PTBC','I')                                            RFORM189
      CALL BKFMT('PTCO','I')                                            RFORM190
      CALL BKFMT('PTEX','I')                                            RFORM191
      CALL BKFMT('PTMA','I')                                            RFORM192
      CALL BKFMT('PTML','I')                                            RFORM193
      CALL BKFMT('PTNC','I')                                            RFORM194
      CALL BKFMT('PTST','I')                                            RFORM195
      CALL BKFMT('PTUN','2I,(3I,11F)')                                  RFORM196
      CALL BKFMT('PVCO','2I,(7I)')                                      RFORM197
      CALL BKFMT('PYCH',' 2I,(2I)')                                     RFORM198
      CALL BKFMT('PYER',' 2I,(I,10F,I)')                                RFORM199
      CALL BKFMT('PYFR',' 2I,(2I)')                                     RFORM200
      CALL BKFMT('REVH','2I,(6I,1F,3I)')                                RFORM201
      CALL BKFMT('RTLS','2I,(I)')                                       RFORM202
      CALL BKFMT('SACO','2I,(F)')                                       RFORM203
      CALL BKFMT('SADI','I')                                            RFORM204
      CALL BKFMT('SCCP','I')                                            RFORM205
      CALL BKFMT('SCLU','2I,(4I,2F)')                                   RFORM206
      CALL BKFMT('SCOO','2I,(3I,2F)')                                   RFORM207
      CALL BKFMT('SCRP','I')                                            RFORM208
      CALL BKFMT('SFTR','2I,(I,F,I,14F,I)')                             RFORM209
      CALL BKFMT('SHOT','I')                                            RFORM210
      CALL BKFMT('SKAN','2I,(2I,6F)')                                   RFORM211
      CALL BKFMT('SKCP','I')                                            RFORM212
      CALL BKFMT('SPAT','2I,(3I,16F)')                                  RFORM213
      CALL BKFMT('SPCP','I')                                            RFORM214
      CALL BKFMT('SRAD','I')                                            RFORM215
      CALL BKFMT('SRTD','I')                                            RFORM216
      CALL BKFMT('SSBP','I')                                            RFORM217
      CALL BKFMT('SSCP','I')                                            RFORM218
      CALL BKFMT('SSKP','I')                                            RFORM219
      CALL BKFMT('SSPP','I')                                            RFORM220
      CALL BKFMT('SSTP','I')                                            RFORM221
      CALL BKFMT('SSUP','I')                                            RFORM222
      CALL BKFMT('STCP','I')                                            RFORM223
      CALL BKFMT('STRK','2I,(I,6F,2I)')                                 RFORM224
      CALL BKFMT('SUCP','I')                                            RFORM225
      CALL BKFMT('SUPA','2I,(I,6F)')                                    RFORM226
      CALL BKFMT('SWPA','I')                                            RFORM227
      CALL BKFMT('SWTU','2I,(I)')                                       RFORM228
C   Sical related banks                                                 RFORM229
      CALL BKFMT('SILH','2I,(I)')                                       RFORM230
      CALL BKFMT('SLUM','2I,(F)')                                       RFORM231
      CALL BKFMT('SILU','2I,(I,23F,2I,4F,2I,49F,I)')                    RFORM232
      CALL BKFMT('SCLS','2I,(11F,3I,15F,2I)')                           RFORM233
      CALL BKFMT('SMAP','2I,(I)')                                       RFORM234
      CALL BKFMT('SMPD','2I,(I)')                                       RFORM235
      CALL BKFMT('SPDA','2I,(F,I,F,4I)')                                RFORM236
      CALL BKFMT('T1FT','2I,(21F,3I)')                                  RFORM237
      CALL BKFMT('T1TL','I')                                            RFORM238
      CALL BKFMT('T1CL','I')                                            RFORM239
      CALL BKFMT('T2XS','2I,(I,2F,I,F,2I)')                             RFORM240
      CALL BKFMT('TARC','2I,(12F,5I)')                                  RFORM241
      CALL BKFMT('TATC','I')                                            RFORM242
      CALL BKFMT('TBCO','2I,(I,3F,3I)')                                 RFORM243
      CALL BKFMT('TCAL','(I)')                                          RFORM244
      CALL BKFMT('TCHA','2I,(23F,3I)')                                  RFORM245
      CALL BKFMT('TCLU','I')                                            RFORM246
      CALL BKFMT('TCRL','I')                                            RFORM247
      CALL BKFMT('TCTC','(I)')                                          RFORM248
      CALL BKFMT('TCTC','2I,(I)')                                       RFORM249
      CALL BKFMT('TDPV','2I,(4F)')                                      RFORM250
      CALL BKFMT('TDFV','2I,(2I,6F)')                                   RFORM251
      CALL BKFMT('TDVV','2I,(3I,8F,I,2F)')                              RFORM252
      CALL BKFMT('TVXY','2I,(4F)')                                      RFORM253
      CALL BKFMT('TELS','I')                                            RFORM254
      CALL BKFMT('TEXS','2I,(I,2F,I,F,I)')                              RFORM255
      CALL BKFMT('TGCL','(I)')                                          RFORM256
      CALL BKFMT('TGFT','2I,(21F,2I)')                                  RFORM257
      CALL BKFMT('TGMA','I')                                            RFORM258
      CALL BKFMT('TGTL','(I)')                                          RFORM259
      CALL BKFMT('THPL','I')                                            RFORM260
      CALL BKFMT('THRP','I')                                            RFORM261
      CALL BKFMT('TISL','B32')                                          RFORM262
      CALL BKFMT('TLAS','2I,(I,7F,I,F)')                                RFORM263
      CALL BKFMT('TLNK','(I)')                                          RFORM264
      CALL BKFMT('TLRL','I')                                            RFORM265
      CALL BKFMT('TMCL','I')                                            RFORM266
      CALL BKFMT('TMTL','2I,(2I,F)')                                    RFORM267
      CALL BKFMT('TPCH','I')                                            RFORM268
      CALL BKFMT('TPCO','2I,(I,5F,4I,2F)')                              RFORM269
      CALL BKFMT('TPRL','I')                                            RFORM270
      CALL BKFMT('TPUL','I')                                            RFORM271
      CALL BKFMT('TRCL','I')                                            RFORM272
      CALL BKFMT('TREX','2I,(7F,2I)')                                   RFORM273
      CALL BKFMT('TSCL','2I,(8I,3F,I)')                                 RFORM274
      CALL BKFMT('TSPU','2I,(I,5F,3I)')                                 RFORM275
      CALL BKFMT('TSRL','I')                                            RFORM276
      CALL BKFMT('TTCC','(I)')                                          RFORM277
      CALL BKFMT('TWAT','I')                                            RFORM278
      CALL BKFMT('TWIT','I')                                            RFORM279
      CALL BKFMT('TWOL','I')                                            RFORM280
      CALL BKFMT('TWPU','2I,(5I,1F,I)')                                 RFORM281
      CALL BKFMT('TWRR','B32')                                          RFORM282
      CALL BKFMT('TWTA','2I,(I,5F,I)')                                  RFORM283
      CALL BKFMT('TWTB','2I,(I,4F)')                                    RFORM284
      CALL BKFMT('VCML','2I')                                           RFORM285
      CALL BKFMT('VCMN','2I')                                           RFORM286
      CALL BKFMT('VCOM','2I,(I)')                                       RFORM287
      CALL BKFMT('VCPL','2I,(4I,F)')                                    RFORM288
      CALL BKFMT('VCSG','2I,(3I,F,4I)')                                 ALB276 2
      CALL BKFMT('VDCO','2I,(I,5F,2I)')                                 RFORM289
      CALL BKFMT('VDFK','2I,(2F,4I)')                                   MTH275 2
      CALL BKFMT('VDXY','2I,(7F,5I)')                                   RFORM290
      CALL BKFMT('VDZT','2I,(5F,5I)')                                   RFORM291
      CALL BKFMT('VFHL','2I,(I)')                                       RFORM292
      CALL BKFMT('VFLG','2I,(I)')                                       RFORM293
      CALL BKFMT('VFPH','2I,(I)')                                       RFORM294
      CALL BKFMT('VGAN','2I,(2I,F)')                                    ALB276 3
      CALL BKFMT('VGCL','I')                                            RFORM295
      CALL BKFMT('VGFT','2I,(21F,I)')                                   RFORM296
      CALL BKFMT('VGTL','I')                                            RFORM297
      CALL BKFMT('VHOT','2I,(I)')                                       RFORM298
      CALL BKFMT('VLST','I')                                            RFORM299
      CALL BKFMT('VMGN','2I,(4I,F)')                                    ALB276 4
      CALL BKFMT('VMPC','I')                                            ALB276 5
      CALL BKFMT('VMRE','I')                                            ALB276 6
      CALL BKFMT('VPEC','I')                                            ALB276 7
      CALL BKFMT('VPER','2I,(F)')                                       ALB276 8
      CALL BKFMT('VPRT','2I,(I,8F)')                                    RFORM301
      CALL BKFMT('VPUL','2I,(I,F)')                                     RFORM302
      CALL BKFMT('VZMR','2I,(2I)')                                      VLCPOS 1
      CALL BKFMT('VREG','I')                                            ALB276 9
      CALL BKFMT('VUFK','2I,(3I,F,2I)')                                 MTH275 3
      CALL BKFMT('X1AD','2I,(A,36I)')                                   RFORM303
      CALL BKFMT('X1CA','2I,(3I,A,72F)')                                RFORM304
      CALL BKFMT('X1SC','2I,(A,31I)')                                   RFORM305
      CALL BKFMT('XTBN','2I,(5I,17A,2I)')                               RFORM309
      CALL BKFMT('XTCN','2I,(2I,3I,I,10I)')                             RFORM310
      CALL BKFMT('XTEB','2I,(65I)')                                     RFORM311
      CALL BKFMT('XTRB','2I,(3I,A)')                                    RFORM312
      CALL BKFMT('XTHH','2I,(3I,3A,74I)')                               RFORM313
      CALL BKFMT('XTLF','2I,(4I,2A,2I,A,I,A,I,A,I,A,4I)')               RFORM314
      CALL BKFMT('XTMS','2I,(4I,A,3I)')                                 RFORM315
      CALL BKFMT('XTOP','2I,(3I,2A,11I)')                               RFORM316
      CALL BKFMT('XTSW','2I,(3I,3A,11I)')                               RFORM317
      CALL BKFMT('XTTL','2I,(4I,2A,I,9A)')                              RFORM318
      CALL BKFMT('XTTP','2I,(3I,11A,32I)')                              RFORM319
      CALL BKFMT('YTRL','2I,(11F,2I)')                                  RFORM320
      CALL BKFMT('YV0V','2I,(2I,24F,I,30F)')                            RFORM321
      CALL BKFMT('YV1C','2I,(4I,12F,3I,6F)')                            RFORM322
      CALL BKFMT('YVXL','2I,(I,10F,3I)')                                RFORM323
      CALL BKFMT('ZPFR',' 2I,(2F)')                                     RFORM324
      END                                                               RFORM325
      SUBROUTINE VDXYZT                                                 VDXYZT 2
C---------------------------------------------------------------------- VDXYZT 3
C!  - creates the banks VDXY,VDZT from the banks VFPH,VFHL,VFLG         VDXYZT 4
C!   Author   :- D. Brown           18-SEP-1991                         VDXYZT 5
C!                                                                      VDXYZT 6
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade                 VDXYZT 7
C!  Modified March 1995 A. Bonissent, M. Thulasidas                     VDXYZT 8
C!                 reorganise and debug                                 VDXYZT 9
C?                                                                      VDXYZT10
C!======================================================================VDXYZT11
C                                                                       VDXYZT13
C  Global includes                                                      VDXYZT14
C                                                                       VDXYZT15
C                                                                       VDFLGS 2
C!  Bit flag definitions for VDET data banks                            VDFLGS 3
C                                                                       VDFLGS 4
      INTEGER VB100U,VB050U,VBUNBD,VBUNMP,VBMULT                        VDFLGS 5
      INTEGER VBNOIS,VBDEAD,VBUNUS,VBLIN2                               VDFLGS 6
      INTEGER VBSUPP,VBOVER,VBHOTC,VBZERO                               VDFLGS 7
C                                                                       VDFLGS 8
      PARAMETER (VBSUPP = 64)                                           VDFLGS 9
      PARAMETER (VBOVER = 128)                                          VDFLGS10
      PARAMETER (VBMULT = 256)                                          VDFLGS11
      PARAMETER (VBZERO = 512)                                          VDFLGS12
      PARAMETER (VBLIN2 = 32768)                                        VDFLGS13
      PARAMETER (VB100U = 65536)                                        VDFLGS14
      PARAMETER (VB050U = 131072)                                       VDFLGS15
      PARAMETER (VBUNMP = 262144)                                       VDFLGS16
      PARAMETER (VBUNBD = 524288)                                       VDFLGS17
      PARAMETER (VBNOIS = 1048576)                                      VDFLGS18
      PARAMETER (VBDEAD = 2097152)                                      VDFLGS19
      PARAMETER (VBUNUS = 67108864)                                     VDFLGS20
      PARAMETER (VBHOTC = 16777216)                                     VDFLGS21
C                                                                       VDFLGS22
C  Online parameters; definition of bit fields in VPLH bank             VDFLGS23
C                                                                       VDFLGS24
      INTEGER OBOVER,OBSUPP,OBPULH,OBFBIT,OBEROR                        VDFLGS25
      PARAMETER (OBOVER = 16384, OBSUPP = 32768, OBEROR = 8192)         VDFLGS26
      PARAMETER (OBPULH = 16383, OBFBIT = 49152)                        VDFLGS27
C                                                                       VDQFLG 2
C ! Quality word bit flags for VDET data banks                          VDQFLG 3
C                                                                       VDQFLG 4
C                                                                       VDQFLG 5
C  First, VDXY,VDZY quality flag bits                                   VDQFLG 6
C                                                                       VDQFLG 7
      INTEGER ISEPBT,IVETOC,IMCNEF                                      VDQFLG 8
      PARAMETER (ISEPBT = 1)                                            VDQFLG 9
      PARAMETER (IVETOC = 2)                                            VDQFLG10
      PARAMETER (IMCNEF = 536870912)                                    VDQFLG11
C                                                                       VDQFLG12
C  Then, VDCO quality flag bits                                         VDQFLG13
C                                                                       VDQFLG14
                                                                        VDQFLG15
      INTEGER IVPHIT,IVZHIT                                             VDQFLG16
      INTEGER IVPAMB,IVZAMB                                             VDQFLG17
      PARAMETER (IVPHIT = 1)                                            VDQFLG18
      PARAMETER (IVZHIT = 2)                                            VDQFLG19
      PARAMETER (IVPAMB = 4)                                            VDQFLG20
      PARAMETER (IVZAMB = 8)                                            VDQFLG21
      INTEGER JVCSRP,JVCSMP,JVCSRF,JVCSSG,JVCSSA,JVCSFF,                VCSGJJ 2
     +        JVCSMF,JVCSCM,LVCSGA                                      VCSGJJ 3
      PARAMETER(JVCSRP=1,JVCSMP=2,JVCSRF=3,JVCSSG=4,JVCSSA=5,JVCSFF=6,  VCSGJJ 4
     +          JVCSMF=7,JVCSCM=8,LVCSGA=8)                             VCSGJJ 5
C                                                                       VRECON 2
C  Reconstruction parameters                                            VRECON 3
C                                                                       VRECON 4
      COMMON/VRECON/MCEVNT,                                             VRECON 5
     &     CHNOFF,IPHOFF,NRCHAN,IDCLIN,NZEXT,                           VRECON 6
     &     MNPULS,MXUNBD,MXSIGM,FNLSUM,                                 VRECON 7
     &     MKVDXY,MKVFPH,MKVHOT,                                        VRECON 8
     &     MINCMD,HCUTCM,LCUTCM,NHOTMN,                                 VRECON 9
     &     MXOCUP,DECFAC,SNOISE,                                        VRECON10
     &     SEPSIG,POSSIG,ERRNOM,                                        VRECON11
     &     CONFLAG,CONPAR                                               VRECON12
C                                                                       VRECON13
      INTEGER MNPULS,NRCHAN,NHOTMN,IDCLIN,NZEXT                         VRECON14
      INTEGER CHNOFF,MINCMD,MXUNBD,MXSIGM,FNLSUM,IPHOFF                 VRECON15
      INTEGER CONFLAG(10)                                               VRECON16
      REAL CONPAR(10)                                                   VRECON17
      REAL HCUTCM,LCUTCM,MXOCUP(2),DECFAC,SNOISE(2)                     VRECON18
      REAL SEPSIG,POSSIG,ERRNOM(2)                                      VRECON19
      LOGICAL MCEVNT,MKVDXY,MKVFPH,MKVHOT,LDECAY(2)                     VRECON20
C                                                                       VRECON21
      INTEGER NEVNTS, NRAWHT, NFNLHT, NMATHT, NNOSHT                    VPRTNC 2
      INTEGER HISOFF, PRNTLV, HISTLV,NHOTEV                             VPRTNC 3
      COMMON /VPRTNC/ NEVNTS, NRAWHT(2), NFNLHT(2), NMATHT(2),          VPRTNC 4
     &     NNOSHT(2), PRNTLV, HISTLV, HISOFF,NHOTEV                     VPRTNC 5
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,  VDXYJJ 2
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,         VDXYJJ 3
     +          JVDXIH=12,LVDXYA=12)                                    VDXYJJ 4
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,  VDZTJJ 2
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)         VDZTJJ 3
C                                                                       VDXYZT23
C These are here only because we access the banks. But the hac          VDXYZT24
c parameters                                                            VDXYZT25
C are not used in this routine                                          VDXYZT26
C                                                                       VDXYZT27
      PARAMETER(JVFHHA=1,LVFHLA=1)                                      VFHLJJ 2
      PARAMETER(JVFPPH=1,LVFPHA=1)                                      VFPHJJ 2
      PARAMETER(JVFLFG=1,LVFLGA=1)                                      VFLGJJ 2
      INTEGER VDRSPS,VDPSLC,VABCVU,VVUWXY, VSPXYZ                       VDXYZT31
C                                                                       VDXYZT32
C  Local variables                                                      VDXYZT33
C                                                                       VDXYZT34
      INTEGER KVCSG                                                     VDXYZT35
      INTEGER NAVFPH,NAVCSG                                             VDXYZT36
      INTEGER IGARB,KLINE,NCOL                                          VDXYZT37
      INTEGER KVFHL,KVFPH,KVFLG                                         VDXYZT38
      INTEGER VNELRM                                                    VDXYZT39
      INTEGER KBANK,IMPPH,IMPFL,INDEX                                   VDXYZT40
      INTEGER NROMD,NRONX,IBNUM,NRXYZ,NNXYZ                             VDXYZT41
      INTEGER IADDR,ILAY,IWFF,IFAC,IVIEW,IVWN                           VDXYZT42
      INTEGER JLAY,JWFF,JWNX,JFAC,JVIEW                                 VDXYZT43
      INTEGER JWAF                                                      VDXYZT44
      INTEGER NSTRP,ISTRP,JSTRP,KSTRP,FSTRP,MSTRP,NSTNX                 VDXYZT45
      INTEGER ISTRT,INWRM                                               VDXYZT46
      INTEGER NVFHL,IVFHL,NHIT,NVFPH                                    VDXYZT47
      INTEGER NSEP,ISEP,MXSEP,NCHMX                                     VDXYZT48
      INTEGER NAMIND                                                    VDXYZT49
      INTEGER NDATA                                                     VDXYZT50
      INTEGER NPOS,IFLAG                                                VDXYZT51
      INTEGER NC,NE,HID                                                 VDXYZT52
      INTEGER IRC                                                       VDXYZT53
      INTEGER BCOL(2)/LVDZTA,LVDXYA/                                    VDXYZT54
      INTEGER VDYEAR                                                    VLCPOS 2
      REAL PULSE,PULSO,PULSN                                            VDXYZT55
      REAL PSUM,SPOS                                                    VDXYZT56
      REAL SCUT,PSCUT                                                   VDXYZT57
      REAL VUW(3),XYZ(3),ABC(3),PSTRP                                   VDXYZT58
      REAL ESUM,CSUM,EFRST,CFIRST,ETA                                   VDXYZT59
      CHARACTER*4 BNAME(2)/'VDZT','VDXY'/                               VDXYZT60
      LOGICAL FIRST                                                     VDXYZT61
      DATA FIRST /.TRUE./                                               VDXYZT62
      PARAMETER (MXSEP=50)                                              VDXYZT63
      INTEGER IBEG(MXSEP),NSCLU(MXSEP)                                  VDXYZT64
C                                                                       VDXYZT65
C  BOS functions                                                        VDXYZT66
C                                                                       VDXYZT67
      INTEGER NLINK,NBANK                                               VDXYZT68
C                                                                       VDXYZT69
C  Variables for inline functions                                       VDXYZT70
C                                                                       VDXYZT71
      LOGICAL IHIS1,IHIS8                                               VDXYZT72
      INTEGER ILINE                                                     VDXYZT73
      INTEGER IFLG                                                      VDXYZT74
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C - # of words/row in bank with index ID                                BMACRO 2
      LCOLS(ID) = IW(ID+1)                                              BMACRO 3
C - # of rows in bank with index ID                                     BMACRO 4
      LROWS(ID) = IW(ID+2)                                              BMACRO 5
C - index of next row in the bank with index ID                         BMACRO 6
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 7
C - index of row # NRBOS in the bank with index ID                      BMACRO 8
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO 9
C - # of free words in the bank with index ID                           BMACRO10
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO11
C - # of free rows in the bank with index ID                            BMACRO12
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO13
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO14
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO15
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO16
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO17
C                                                                       BMACRO18
C                                                                       VDXYZT77
C  Inline functions                                                     VDXYZT78
C                                                                       VDXYZT79
      IHIS1(IFLG) = IAND(IFLG,1) .EQ. 1                                 VDXYZT80
      IHIS8(IFLG) = IAND(IFLG,8) .EQ. 8                                 VDXYZT81
      ILINE(IFLG) = IAND(IFLG,VBLIN2)/VBLIN2                            VDXYZT82
      IF(FIRST)THEN                                                     VDXYZT83
        FIRST=.FALSE.                                                   VDXYZT84
        NCHMX = MAX(VNELRM(1),VNELRM(2))                                VDXYZT85
        NAVFPH=NAMIND('VFPH')                                           VDXYZT86
        NAVCSG=NAMIND('VCSG')                                           VDXYZT87
      ENDIF                                                             VDXYZT88
C                                                                       VDXYZT89
C  Drop old banks                                                       VDXYZT90
C                                                                       VDXYZT91
      CALL BDROP(IW,'VDXYVDZT')                                         VDXYZT92
C                                                                       VDXYZT93
C  get the pointer of the first VFPH                                    VDXYZT94
C                                                                       VDXYZT95
      KVFPH = IW(NAVFPH)                                                VDXYZT96
C                                                                       VDXYZT97
C  If any exist, make the bank for the temporary storage                VDXYZT98
C                                                                       VDXYZT99
      IF(KVFPH.GT.0)THEN                                                VDXYZ100
        NDATA = NCHMX*LVCSGA+LMHLEN                                     VDXYZ101
        CALL AUBOS('VCSG',0,NDATA,KVCSG,IGARB)                          VDXYZ102
        IF(IGARB.EQ.2)THEN                                              VDXYZ103
          CALL RERROR('VDXYZT',1,'Out of BOS space; event rejected')    VDXYZ104
          GOTO 999                                                      VDXYZ105
        END IF                                                          VDXYZ106
        IW(KVCSG+LMHROW)=NCHMX                                          VDXYZ107
        IW(KVCSG+LMHCOL)=LVCSGA                                         VDXYZ108
      END IF                                                            VDXYZ109
C                                                                       VDXYZ110
C Say that no VDXY or VDZT bank has yet been open                       VDXYZ111
C                                                                       VDXYZ112
      INWRM=1                                                           VDXYZ113
C                                                                       VDXYZ114
C  Loop over all VFPH banks                                             VDXYZ115
C                                                                       VDXYZ116
      DO WHILE(KVFPH.GT.0)                                              VDXYZ117
C                                                                       VDXYZ118
C  Get the VFHL,VFLG bank indices                                       VDXYZ119
C                                                                       VDXYZ120
        IBNUM = IW(KVFPH-2)                                             VDXYZ121
        KVFHL = NLINK('VFHL',IBNUM)                                     VDXYZ122
        KVFLG = NLINK('VFLG',IBNUM)                                     VDXYZ123
        IF(KVFHL .LE. 0 .OR. KVFLG .LE. 0)THEN                          VDXYZ124
          CALL RERROR('VDXYZT',1,'VFHL,VFLG banks missing')             VDXYZ125
          GOTO 999                                                      VDXYZ126
        END IF                                                          VDXYZ127
C                                                                       VDXYZ128
C  loop over the clusters in this VFHL bank                             VDXYZ129
C                                                                       VDXYZ130
        NVFHL = LROWS(KVFHL)                                            VDXYZ131
        NVFPH = LROWS(KVFPH)                                            VDXYZ132
C                                                                       VDXYZ133
C  Unpack the first cluster address                                     VDXYZ134
C                                                                       VDXYZ135
        IF(NVFHL.NE.0)THEN                                              VDXYZ136
          IADDR = ITABL(KVFHL,1,JVFHHA)                                 VDXYZ137
          CALL VADDUN(IADDR,NSTRP,ILAY,IWFF,IFAC,IVIEW,FSTRP)           VDXYZ138
          CALL VAENWA(NROMD,ILAY,IWFF,IFAC,IVIEW)                       VDXYZ139
C                                                                       VDXYZ140
C Special encoding for VDXY/VDZT                                        VDXYZ141
C                                                                       VDXYZ142
          CALL VAENWA(NRXYZ,ILAY,IWFF,IFAC,1)                           VDXYZ143
        ENDIF                                                           VDXYZ144
        ISTRT=0                                                         VDXYZ145
        DO IVFHL = 1,NVFHL                                              VDXYZ146
          SCUT = SEPSIG*SNOISE(IVIEW)                                   VDXYZ147
          PSCUT = POSSIG*SNOISE(IVIEW)                                  VDXYZ148
C                                                                       VDXYZ149
C  Copy the pulseheights and flags into local bank                      VDXYZ150
C                                                                       VDXYZ151
          PULSO = 0.0                                                   VDXYZ152
          DO ISTRP=1,NSTRP                                              VDXYZ153
            JSTRP = ISTRT+ISTRP                                         VDXYZ154
            PULSE = ITABL(KVFPH,JSTRP,1)                                VDXYZ155
            IFLAG = ITABL(KVFLG,JSTRP,1)                                VDXYZ156
C                                                                       VDXYZ157
C  If needed, correct for the decay factor                              VDXYZ158
C                                                                       VDXYZ159
            PULSN = PULSE                                               VDXYZ160
            IF(IVIEW.EQ.2.AND.IDCLIN.EQ.ILINE(IFLAG))THEN               VDXYZ161
              PULSN = PULSE + DECFAC*(PULSE-PULSO)                      VDXYZ162
            END IF                                                      VDXYZ163
            KLINE = KROW(KVCSG,ISTRP+FSTRP-1)                           VDXYZ164
            RW(KLINE+JVCSMP) = PULSN                                    VDXYZ165
            IW(KLINE+JVCSMF) = IFLAG                                    VDXYZ166
C                                                                       VDXYZ167
C  Save uncorrected pulseheight in case we are correcting the next one  VDXYZ168
C                                                                       VDXYZ169
            PULSO = PULSE                                               VDXYZ170
          END DO                                                        VDXYZ171
C                                                                       VDXYZ172
C  Test histograms for charge division                                  VDXYZ173
C                                                                       VDXYZ174
          IF(IHIS8(HISTLV))THEN                                         VDXYZ175
C                                                                       VDXYZ176
C  Check for appropriate clusters                                       VDXYZ177
C                                                                       VDXYZ178
            NE = 0                                                      VDXYZ179
            ESUM = 0.0                                                  VDXYZ180
            DO ISTRP=1,NSTRP                                            VDXYZ181
              JSTRP = FSTRP+ISTRP-1                                     VDXYZ182
              PULSE = RTABL(KVCSG,ISTRP,JVCSMP)                         VDXYZ183
              IFLAG = ITABL(KVCSG,ISTRP,JVCSMF)                         VDXYZ184
              IF(PULSE.GE.PSCUT)THEN                                    VDXYZ185
                NE = NE + 1                                             VDXYZ186
                IF(NE .EQ. 1)EFRST = PULSE                              VDXYZ187
                ESUM = ESUM + PULSE                                     VDXYZ188
              END IF                                                    VDXYZ189
            END DO                                                      VDXYZ190
            IF(NE.EQ.2)THEN                                             VDXYZ191
              ETA = EFRST/ESUM                                          VDXYZ192
              IF(IVIEW.EQ.2)THEN                                        VDXYZ193
                HID = 100+10*ILINE(IFLAG)+1                             VDXYZ194
                CALL HFILL(HID,ETA,0.0,1.0)                             VDXYZ195
              ELSE                                                      VDXYZ196
                CALL HFILL(130,ETA,0.0,1.0)                             VDXYZ197
              END IF                                                    VDXYZ198
            END IF                                                      VDXYZ199
          END IF                                                        VDXYZ200
C                                                                       VDXYZ201
C  Hit separation                                                       VDXYZ202
C                                                                       VDXYZ203
          CALL VSEPAR(SCUT,FSTRP,NSTRP,NSEP,IBEG,NSCLU)                 VDXYZ204
C                                                                       VDXYZ205
C  IBEG is the beginning of each subcluster w/r FSTRP, the begin        VDXYZ206
C  of the original cluster.                                             VDXYZ207
C  We now need to translate                                             VDXYZ208
C                                                                       VDXYZ209
          DO ISEP=1,NSEP                                                VDXYZ210
            IBEG(ISEP)=IBEG(ISEP)+FSTRP-1                               VDXYZ211
          ENDDO                                                         VDXYZ212
C                                                                       VDXYZ213
C  Loop over the separated clusters                                     VDXYZ214
C                                                                       VDXYZ215
          DO 22 ISEP=1,NSEP                                             VDXYZ216
C                                                                       VDXYZ217
C  calculate the strip coordinate cluster position                      VDXYZ218
C                                                                       VDXYZ219
C Here, we use scut, while poscut should probably be used               VDXYZ220
C                                                                       VDXYZ221
            CALL VLCPOS(IBEG(ISEP),NSCLU(ISEP),SCUT,SPOS,PSUM,NPOS)     VDXYZ222
            IF(NPOS .LE. 0)THEN                                         VDXYZ223
              CALL RERROR('VDXYZT',6,'Position calculation error')      VDXYZ224
              GOTO 22                                                   VDXYZ225
            END IF                                                      VDXYZ226
C                                                                       VDXYZ227
C  Histogram the hit width                                              VDXYZ228
C                                                                       VDXYZ229
            IF(IHIS1(HISTLV))THEN                                       VDXYZ230
              CALL HFILL(HISOFF+30+IVIEW,FLOAT(NPOS),0.0,1.0)           VDXYZ231
              CALL HFILL(HISOFF+20+IVIEW,PSUM,0.0,1.0)                  VDXYZ232
            END IF                                                      VDXYZ233
C                                                                       VDXYZ234
C  Re-make final pulseheight cut                                        VDXYZ235
C                                                                       VDXYZ236
            IF(PSUM .GE. FNLSUM)THEN                                    VDXYZ237
C                                                                       VDXYZ238
C  Translate strip position to local and global position                VDXYZ239
C                                                                       VDXYZ240
              SPOS = SPOS+IBEG(ISEP)-1                                  VDXYZ241
              IRC = VSPXYZ                                              VDXYZ242
     >           (SPOS,IVIEW,ILAY,IFAC,IWFF,NROMD,NRXYZ,VUW,XYZ)        VDXYZ243
              IF (IRC.NE.1) GOTO 22                                     VDXYZ244
C                                                                       VDXYZ245
C  Tag separated clusters                                               VDXYZ246
C                                                                       VDXYZ247
                IF(NSEP .GT. 1)THEN                                     VDXYZ248
                  IFLAG = ISEPBT                                        VDXYZ249
                ELSE                                                    VDXYZ250
                  IFLAG = 0                                             VDXYZ251
                END IF                                                  VDXYZ252
C                                                                       VDXYZ253
C  Add cluster information to VDXY/VDZT bank.                           VDXYZ254
C                                                                       VDXYZ255
C  If we are processing a readout module for the first time, create     VDXYZ256
C  the bank                                                             VDXYZ257
                IF(INWRM.EQ.1)THEN                                      VDXYZ258
                  IF(IVIEW.EQ.1)THEN                                    VDXYZ259
                    NCOL=LVDZTA                                         VDXYZ260
                  ELSE                                                  VDXYZ261
                    NCOL=LVDXYA                                         VDXYZ262
                  ENDIF                                                 VDXYZ263
                  NDATA=LMHLEN+NCOL*NVFPH                               VDXYZ264
                  CALL AUBOS(BNAME(IVIEW),NRXYZ,NDATA,KBANK,IGARB)      VDXYZ265
                  IF(IGARB.EQ.2)THEN                                    VDXYZ266
                    CALL RERROR('VDXYZT',2,'Error making BOS bank')     VDXYZ267
                    GOTO 999                                            VDXYZ268
                  ENDIF                                                 VDXYZ269
                  IF(IGARB.EQ.1)THEN                                    VDXYZ270
                    KVFHL = NLINK('VFHL',IBNUM)                         VDXYZ271
                    KVFLG = NLINK('VFLG',IBNUM)                         VDXYZ272
                    KVFPH = NLINK('VFPH',IBNUM)                         VDXYZ273
                    KVCSG = IW(NAVCSG)                                  VDXYZ274
                  ENDIF                                                 VDXYZ275
                  IW(KBANK+LMHCOL)=NCOL                                 VDXYZ276
                  IW(KBANK+LMHROW)=0                                    VDXYZ277
                  INWRM=0                                               VDXYZ278
                ENDIF                                                   VDXYZ279
                KLINE = KNEXT(KBANK)                                    VDXYZ280
                IF(IVIEW .EQ. 1)THEN                                    VDXYZ281
                  RW(KLINE+JVDZZC) = XYZ(3)                             VDXYZ282
                  RW(KLINE+JVDZWC) = VUW(3)                             VDXYZ283
                  RW(KLINE+JVDZSZ) = ERRNOM(IVIEW)                      VDXYZ284
                  RW(KLINE+JVDZSW) = ERRNOM(IVIEW)                      VDXYZ285
                  RW(KLINE+JVDZPH) = PSUM                               VDXYZ286
                  IW(KLINE+JVDZIH) = IVFHL                              VDXYZ287
                  IW(KLINE+JVDZIW) = IBNUM                              VDXYZ288
                  IW(KLINE+JVDZQF) = IFLAG                              VDXYZ289
                ELSE                                                    VDXYZ290
                  RW(KLINE+JVDXXC) = XYZ(1)                             VDXYZ291
                  RW(KLINE+JVDXYC) = XYZ(2)                             VDXYZ292
                  RW(KLINE+JVDXUC) = VUW(2)                             VDXYZ293
                  RW(KLINE+JVDXSX) = ERRNOM(IVIEW)                      VDXYZ294
                  RW(KLINE+JVDXSY) = ERRNOM(IVIEW)                      VDXYZ295
                  RW(KLINE+JVDXSU) = ERRNOM(IVIEW)                      VDXYZ296
                  RW(KLINE+JVDXPH) = PSUM                               VDXYZ297
                  IW(KLINE+JVDXIH) = IVFHL                              VDXYZ298
                  IW(KLINE+JVDXIW) = IBNUM                              VDXYZ299
                  IW(KLINE+JVDXQF) = IFLAG                              VDXYZ300
                END IF                                                  VDXYZ301
                IW(KBANK+LMHROW) = IW(KBANK+LMHROW) + 1                 VDXYZ302
            END IF                                                      VDXYZ303
 22       CONTINUE                                                      VDXYZ304
          IF(IVFHL.LT.NVFHL)THEN                                        VDXYZ305
C                                                                       VDXYZ306
C  Uncode the wafer address of the next cluster                         VDXYZ307
C                                                                       VDXYZ308
            IADDR = ITABL(KVFHL,IVFHL+1,JVFHHA)                         VDXYZ309
            CALL VADDUN(IADDR,NSTNX,ILAY,IWFF,IFAC,IVWN,FSTRP)          VDXYZ310
            CALL VAENWA(NRONX,ILAY,IWFF,IFAC,IVWN)                      VDXYZ311
C                                                                       VDXYZ312
C Make the special encoding for VDXY/VDZT banks : view=1 !              VDXYZ313
C                                                                       VDXYZ314
            CALL VAENWA(NNXYZ,ILAY,IWFF,IFAC,1)                         VDXYZ315
          ENDIF                                                         VDXYZ316
C                                                                       VDXYZ317
C                                                                       VDXYZ318
C  If next readout module is different, or end of bank,                 VDXYZ319
C  one readout module has been finished. Close the bank                 VDXYZ320
C  But only if it exists                                                VDXYZ321
C                                                                       VDXYZ322
          IF(NRONX.NE.NROMD.OR.IVFHL.EQ.NVFHL)THEN                      VDXYZ323
            IF(INWRM.EQ.0)THEN                                          VDXYZ324
              NHIT = LROWS(KBANK)                                       VDXYZ325
              NDATA=LMHLEN+NCOL*NHIT                                    VDXYZ326
              CALL AUBOS(BNAME(IVIEW),NRXYZ,NDATA,KBANK,IGARB)          VDXYZ327
            ENDIF                                                       VDXYZ328
C                                                                       VDXYZ329
C Say that no VDXY or VDZT bank is anymore opened                       VDXYZ330
C                                                                       VDXYZ331
            INWRM=1                                                     VDXYZ332
C                                                                       VDXYZ333
C Reset VCSG temporary bank                                             VDXYZ334
C                                                                       VDXYZ335
            CALL VZERO(IW(KVCSG+LMHLEN+1),IW(KVCSG)-LMHLEN)             VDXYZ336
C                                                                       VDXYZ337
            NRXYZ=NNXYZ                                                 VDXYZ338
            NROMD=NRONX                                                 VDXYZ339
            IVIEW=IVWN                                                  VDXYZ340
          ENDIF                                                         VDXYZ341
C                                                                       VDXYZ342
C  Advance pointer in VFPH                                              VDXYZ343
C                                                                       VDXYZ344
          ISTRT = ISTRT + NSTRP                                         VDXYZ345
          NSTRP = NSTNX                                                 VDXYZ346
        END DO                                                          VDXYZ347
C                                                                       VDXYZ348
C  Move to next VFPH index                                              VDXYZ349
C                                                                       VDXYZ350
        KVFPH = IW(KVFPH-1)                                             VDXYZ351
      END DO                                                            VDXYZ352
C                                                                       VDXYZ353
C  Drop work bank                                                       VDXYZ354
C                                                                       VDXYZ355
      CALL BDROP(IW,'VCSG')                                             VDXYZ356
C                                                                       VLCPOS 3
C     expand VDZT banks to have one per wafer                           VLCPOS 4
C                                                                       VLCPOS 5
      IF(VDYEAR().EQ.95)CALL VDXPBZ                                     VLCPOS 6
                                                                        VLCPOS 7
 999  RETURN                                                            VDXYZ357
      END                                                               VDXYZ358
      SUBROUTINE VDXPBZ                                                 VDXPBZ 2
C---------------------------------------------------------------------- VDXPBZ 3
C!   Description                                                        VDXPBZ 4
C!   ===========                                                        VDXPBZ 5
C!   EXPANDS BANKS VDZT TO 3 WAFERS AND ADDS CLUSTERS FOR MULTIPLEXED   VDXPBZ 6
C!   Z-REGION                                                           VDXPBZ 7
C!   OLD BANKS ARE DELETED AFTERWARDS                                   VDXPBZ 8
C!                                                                      VDXPBZ 9
C!   INPUT : NONE                                                       VDXPBZ10
C!   OUTPUT: NONE                                                       VDXPBZ11
C!                                                                      VDXPBZ12
C!   Author   :- Armin Wagner          15-MAY-1995                      VDXPBZ13
C!======================================================================VDXPBZ14
C     DOESN'T WORK DUE TO UNDEFINED VARIABLES IN JULIA COMMON DECKS     VDXPBZ15
C     IMPLICIT NONE                                                     VDXPBZ16
                                                                        VDXPBZ17
      INTEGER NAVOLD /0/                                                VDXPBZ18
      INTEGER KVOLD,NVOLD,NROLD,NRNEW(6)                                VDXPBZ19
      INTEGER KVDZT(6),KVZMR(6)                                         VDXPBZ20
      INTEGER INRW(6),IORW,IND1,IND2                                    VDXPBZ21
      INTEGER ILAY,IWAFF,IFAC,IVIEW,IWFF1,IWFF2                         VDXPBZ22
      INTEGER IR,IFF,NDATA,I                                            VDXPBZ23
      REAL    ZGC1,ZGC2                                                 VDXPBZ24
      REAL    ZLC1,ZLC2                                                 VDXPBZ25
                                                                        VDXPBZ26
C FUNCTIONS                                                             VDXPBZ27
      INTEGER NSWAP,NAMIND,NLINK                                        VDXPBZ28
                                                                        VDXPBZ29
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,  VDZTJJ 2
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)         VDZTJJ 3
      INTEGER JVZMNR,JVZMRO,LVZMRA                                      VZMRJJ 2
      PARAMETER(JVZMNR=1,JVZMRO=2,LVZMRA=2)                             VZMRJJ 3
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C - # of words/row in bank with index ID                                BMACRO 2
      LCOLS(ID) = IW(ID+1)                                              BMACRO 3
C - # of rows in bank with index ID                                     BMACRO 4
      LROWS(ID) = IW(ID+2)                                              BMACRO 5
C - index of next row in the bank with index ID                         BMACRO 6
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 7
C - index of row # NRBOS in the bank with index ID                      BMACRO 8
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO 9
C - # of free words in the bank with index ID                           BMACRO10
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO11
C - # of free rows in the bank with index ID                            BMACRO12
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO13
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO14
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO15
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO16
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO17
C                                                                       BMACRO18
                                                                        VDXPBZ34
                                                                        VDXPBZ35
C                                                                       VDXPBZ36
C --  Switch name of VDZT banks to VOLD // will be dropped later        VDXPBZ37
C                                                                       VDXPBZ38
      CALL BSWAP(IW,'VDZT','VOLD')                                      VDXPBZ39
                                                                        VDXPBZ40
C                                                                       VDXPBZ41
C --  get pointer of first VOLD bank                                    VDXPBZ42
C                                                                       VDXPBZ43
      IF (NAVOLD.EQ.0) NAVOLD=NAMIND('VOLD')                            VDXPBZ44
                                                                        VDXPBZ45
                                                                        VDXPBZ46
C --  LOOP over all VOLD banks                                          VDXPBZ47
                                                                        VDXPBZ48
      KVOLD=NAVOLD+1                                                    VDXPBZ49
10    KVOLD=IW(KVOLD-1)                                                 VDXPBZ50
      IF(KVOLD.GT.0)THEN                                                VDXPBZ51
                                                                        VDXPBZ52
C       ! get parameters from VOLD                                      VDXPBZ53
        NVOLD=LROWS(KVOLD)                                              VDXPBZ54
        NROLD=IW(KVOLD-2)                                               VDXPBZ55
C                                                                       VDXPBZ56
C       ! book VDZT-banks for 3 wafers in module                        VDXPBZ57
C                                                                       VDXPBZ58
        CALL VBKZWF(NROLD,NVOLD,NRNEW)                                  VDXPBZ59
C                                                                       VDXPBZ60
C       ! GET bank-id's // only 3 used                                  VDXPBZ61
C                                                                       VDXPBZ62
        CALL VZERO(KVDZT,6)                                             VDXPBZ63
        CALL VZERO(KVZMR,6)                                             VDXPBZ64
        DO I=1,6                                                        VDXPBZ65
          IF (NRNEW(I).GE.0) KVDZT(I)=NLINK('VDZT',NRNEW(I))            VDXPBZ66
          IF (NRNEW(I).GE.0) KVZMR(I)=NLINK('VZMR',NRNEW(I))            VDXPBZ67
        ENDDO                                                           VDXPBZ68
C                                                                       VDXPBZ69
C       ! Loop over Clusters in old BANK, get multiplexed cluster       VDXPBZ70
C       ! from VDMULT and fill original and multiplexed cluster in      VDXPBZ71
C       ! NEW BANKs for the coresponding wafers                         VDXPBZ72
C                                                                       VDXPBZ73
        CALL VZERO(INRW,6)                                              VDXPBZ74
        DO IORW=1,NVOLD                                                 VDXPBZ75
          ZGC1=RTABL(KVOLD,IORW,JVDZZC)                                 VDXPBZ76
          CALL VDMULT(NROLD,ZGC1,ZLC1,IWFF1,ZGC2,ZLC2,IWFF2)            VDXPBZ77
C                                                                       VDXPBZ78
C         ! keep track how many rows already filled in new banks        VDXPBZ79
C                                                                       VDXPBZ80
          INRW(IWFF1)=INRW(IWFF1)+1                                     VDXPBZ81
          INRW(IWFF2)=INRW(IWFF2)+1                                     VDXPBZ82
C                                                                       VDXPBZ83
C         ! fill new VDZT banks                                         VDXPBZ84
C                                                                       VDXPBZ85
          IF (KVDZT(IWFF1).EQ.0 .OR.                                    VDXPBZ86
     >        KVDZT(IWFF2).EQ.0) GOTO 998                               VDXPBZ87
                                                                        VDXPBZ88
          IND1=KROW(KVDZT(IWFF1),INRW(IWFF1))                           VDXPBZ89
          IND2=KROW(KVDZT(IWFF2),INRW(IWFF2))                           VDXPBZ90
                                                                        VDXPBZ91
          IW(KVDZT(IWFF1)+LMHROW)=IW(KVDZT(IWFF1)+LMHROW)+1             VDXPBZ92
          RW(IND1+JVDZZC)=ZGC1                                          VDXPBZ93
          RW(IND1+JVDZWC)=RTABL(KVOLD,IORW,JVDZWC)                      VDXPBZ94
          RW(IND1+JVDZSZ)=RTABL(KVOLD,IORW,JVDZSZ)                      VDXPBZ95
          RW(IND1+JVDZSW)=RTABL(KVOLD,IORW,JVDZSW)                      VDXPBZ96
          RW(IND1+JVDZPH)=RTABL(KVOLD,IORW,JVDZPH)                      VDXPBZ97
          IW(IND1+JVDZIH)=ITABL(KVOLD,IORW,JVDZIH)                      VDXPBZ98
          IW(IND1+JVDZIW)=ITABL(KVOLD,IORW,JVDZIW)                      VDXPBZ99
          IW(IND1+JVDZQF)=ITABL(KVOLD,IORW,JVDZQF)                      VDXPB100
                                                                        VDXPB101
          IW(KVDZT(IWFF2)+LMHROW)=IW(KVDZT(IWFF2)+LMHROW)+1             VDXPB102
          RW(IND2+JVDZZC)=ZGC2                                          VDXPB103
          RW(IND2+JVDZWC)=ZLC2                                          VDXPB104
          RW(IND2+JVDZSZ)=RTABL(KVOLD,IORW,JVDZSZ)                      VDXPB105
          RW(IND2+JVDZSW)=RTABL(KVOLD,IORW,JVDZSW)                      VDXPB106
          RW(IND2+JVDZPH)=RTABL(KVOLD,IORW,JVDZPH)                      VDXPB107
          IW(IND2+JVDZIH)=ITABL(KVOLD,IORW,JVDZIH)                      VDXPB108
          IW(IND2+JVDZIW)=ITABL(KVOLD,IORW,JVDZIW)                      VDXPB109
          IW(IND2+JVDZQF)=ITABL(KVOLD,IORW,JVDZQF)                      VDXPB110
C                                                                       VDXPB111
C         ! fill Multiplexing Relation banks                            VDXPB112
C                                                                       VDXPB113
          IF (KVZMR(IWFF1).EQ.0 .OR.                                    VDXPB114
     >        KVZMR(IWFF2).EQ.0) GOTO 998                               VDXPB115
                                                                        VDXPB116
          IND1=KROW(KVZMR(IWFF1),INRW(IWFF1))                           VDXPB117
          IND2=KROW(KVZMR(IWFF2),INRW(IWFF2))                           VDXPB118
                                                                        VDXPB119
          IW(KVZMR(IWFF1)+LMHROW) = IW(KVZMR(IWFF1)+LMHROW)+1           VDXPB120
          IW(IND1+JVZMNR) = NRNEW(IWFF2)                                VDXPB121
          IW(IND1+JVZMRO) = INRW(IWFF2)                                 VDXPB122
                                                                        VDXPB123
          IW(KVZMR(IWFF2)+LMHROW) = IW(KVZMR(IWFF2)+LMHROW)+1           VDXPB124
          IW(IND2+JVZMNR) = NRNEW(IWFF1)                                VDXPB125
          IW(IND2+JVZMRO) = INRW(IWFF1)                                 VDXPB126
                                                                        VDXPB127
        ENDDO                                                           VDXPB128
                                                                        VDXPB129
        GOTO 10                                                         VDXPB130
      ENDIF                                                             VDXPB131
                                                                        VDXPB132
C                                                                       VDXPB133
C     DROP 'VOLD' // COMPRESS VDZT,VZMR to actual size                  VDXPB134
C                                                                       VDXPB135
                                                                        VDXPB136
      CALL BDROP(IW,'VOLD')                                             VDXPB137
      CALL AUBPRS('VDZTVZMR')                                           VDXPB138
      GOTO 999                                                          VDXPB139
                                                                        VDXPB140
998   CALL RERROR('VDXPBZ',1,'K-IDS FOR VDZT/VZMR ZERO, NOT CURED')     VDXPB141
                                                                        VDXPB142
999   RETURN                                                            VDXPB143
      END                                                               VDXPB144
                                                                        VDXPB145
                                                                        VDXPB146
      SUBROUTINE VBKZWF(NRIN,IROWS,NROUT)                               VBKZWF 2
C===============================================================        VBKZWF 3
C!   Description                                                        VBKZWF 4
C!   ===========                                                        VBKZWF 5
C! CREATES 3 NEW VDZT BANKS AND RETURNS BANK-NR'S IN ARRAY NROUT        VBKZWF 6
C! THE NUMBER OF ROWS IN THE NEW BANKS IS THE SAME AS FOR THE INPUT     VBKZWF 7
C! BANK (LARGER THAN NEEDED, WILL BE SHRINKED LATER)                    VBKZWF 8
C!                                                                      VBKZWF 9
C! TO SIMPLIFY THINGS: DO INDEXING FOR WHOLE FACE (WAFERS 1-6)          VBKZWF10
C!                                                                      VBKZWF11
C! INPUT:  NRIN           NR of old VDZT Bank (for module)              VBKZWF12
C!         IROWS          Rows   -"-                                    VBKZWF13
C! OUTPUT: NROUT          Array(6) of new bank NR's for VDZT/VZMR       VBKZWF14
C!                        ONLY 3 USED (for module), others set to       VBKZWF15
C!                        -999                                          VBKZWF16
C!                                                                      VBKZWF17
C!   Author   :- Armin Wagner          15-MAY-1995                      VBKZWF18
C===============================================================        VBKZWF19
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,  VDZTJJ 2
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)         VDZTJJ 3
      INTEGER JVZMNR,JVZMRO,LVZMRA                                      VZMRJJ 2
      PARAMETER(JVZMNR=1,JVZMRO=2,LVZMRA=2)                             VZMRJJ 3
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
                                                                        VBKZWF23
      INTEGER NRIN,IROWS,NROUT(6)                                       VBKZWF24
      INTEGER ILAY,IWAFF,IFAC,IVIEW                                     VBKZWF25
      INTEGER NDATA,KBANK,IGARB,I,IFF                                   VBKZWF26
                                                                        VBKZWF27
      DO I=1,6                                                          VBKZWF28
         NROUT(I)=-999                                                  VBKZWF29
      ENDDO                                                             VBKZWF30
      CALL VADEWA(NRIN,ILAY,IWAFF,IFAC,IVIEW)                           VBKZWF31
                                                                        VBKZWF32
C                                                                       VBKZWF33
C     IWAFF should only be 3 or 4 since VDZT Banks in VDXYZT only       VBKZWF34
C     defined per module // and IWAFF=3 => left  module                 VBKZWF35
C                               IWAFF=4 => right module                 VBKZWF36
C                           as defined in VSPXY                         VBKZWF37
C                                                                       VBKZWF38
      IF (.NOT.(IWAFF.EQ.3 .OR. IWAFF.EQ.4)) GOTO 998                   VBKZWF39
                                                                        VBKZWF40
                                                                        VBKZWF41
      DO I=1,3                                                          VBKZWF42
                                                                        VBKZWF43
C --    MODULE WITH WAFERS 1-3                                          VBKZWF44
        IF (IWAFF.EQ.3) IFF=I                                           VBKZWF45
C --    MODULE WITH WAFERS 4-6                                          VBKZWF46
        IF (IWAFF.EQ.4) IFF=I+3                                         VBKZWF47
                                                                        VBKZWF48
        CALL VAENWA(NROUT(IFF),ILAY,IFF,IFAC,IVIEW)                     VBKZWF49
                                                                        VBKZWF50
        NDATA=LMHLEN+IROWS*LVDZTA                                       VBKZWF51
        CALL AUBOS ('VDZT',NROUT(IFF),NDATA,KBANK,IGARB)                VBKZWF52
        IW(KBANK+LMHCOL)=LVDZTA                                         VBKZWF53
                                                                        VBKZWF54
        NDATA=LMHLEN+IROWS*LVZMRA                                       VBKZWF55
        CALL AUBOS ('VZMR',NROUT(IFF),NDATA,KBANK,IGARB)                VBKZWF56
        IW(KBANK+LMHCOL)=LVZMRA                                         VBKZWF57
                                                                        VBKZWF58
      ENDDO                                                             VBKZWF59
      GOTO 999                                                          VBKZWF60
                                                                        VBKZWF61
998   CALL RERROR('VBKZWF',1,'WRONG BANK-NR OF VDZT ! -> STOP')         VBKZWF62
      STOP                                                              VBKZWF63
                                                                        VBKZWF64
999   RETURN                                                            VBKZWF65
      END                                                               VBKZWF66
                                                                        VBKZWF67
      SUBROUTINE VDMULT(NR,ZGC1,ZLC1,IWFF1,ZGC2,ZLC2,IWFF2)             VDMULT 2
C=======================================================================VDMULT 3
C!   Description                                                        VDMULT 4
C!   ===========                                                        VDMULT 5
C!   For multiplexed z-view only: Z-position of strip/cluster           VDMULT 6
C!                            -> Z-position of multiplexed partner      VDMULT 7
C!   Algorithm:                                                         VDMULT 8
C!     zcoor         -> local coord (=a "in z") // using (xyz)->(abc)   VDMULT 9
C!     local coord   -> readout strip           // VDLCRS               VDMULT10
C!     readout strip -> multiplexed partner     // VZRSRS               VDMULT11
C!     ...                                                              VDMULT12
C!     do the reverse to get to zcoor again                             VDMULT13
C!                                                                      VDMULT14
C!   INPUT : NR                   BANK NUMBER OF OLD VDZT -> ILAY/IFAC  VDMULT15
C!           ZGC1                 Z-COORD. (GLOBAL)                     VDMULT16
C!   OUTPUT: ZLC1                 Z-COORD. (LOCAL/VUW(3))               VDMULT17
C!           IWFF1    LOCAL WAFER IN FACE FOR ZCOR1                     VDMULT18
C!           ZGC2     MULTIPLEXED Z-COORD. (GLOBAL)                     VDMULT19
C!           ZLC2                          (LOCAL/VUW(3))               VDMULT20
C!           IWFF2    LOCAL WAFER IN FACE FOR MULTIPLEXED Z-COORD.      VDMULT21
C!                                                                      VDMULT22
C!   *BE CAREFUL*: GEOMETRY PACKAGE USES ABC(3) AS LOCAL COORDINATES !! VDMULT23
C!   Author   :- Alain Bonissent                                        VDMULT24
C!   Modified :- Armin Wagner            15-MAY-1995                    VDMULT25
C=======================================================================VDMULT26
                                                                        VDMULT27
      IMPLICIT NONE                                                     VDMULT28
                                                                        VDMULT29
      REAL ABC(3),XYZ(3),VUW(3)                                         VDMULT30
      REAL ZGC1,ZGC2,ZLC1,ZLC2,RSTRP,PSINT,DELT,RSTR2                   VDMULT31
      INTEGER IWFF1,IWFF2,IWAF1,IWAF2,IROS1,IROS2                       VDMULT32
      INTEGER IMOD,IRET,JWAF,IER,IOF                                    VDMULT33
      INTEGER NR,ILAY,IDUM,IFAC,JFAC                                    VDMULT34
                                                                        VDMULT35
C FUNCTION_DEFS                                                         VDMULT36
      INTEGER VWFFND,VJWAFF,VABCXY,VXYZAB,VDLCRS,VDRSLC,VFWAFF,VZRSRS   VDMULT37
      INTEGER VIWFFW,VXYZVU,VABCVU,VJFACI                               VDMULT38
                                                                        VDMULT39
C ==                                                                    VDMULT40
C I. get local coordinate in z-direction                                VDMULT41
C ==                                                                    VDMULT42
                                                                        VDMULT43
C Get Layer and Local Face from NR of old VDZT Bank                     VDMULT44
      CALL VADEWA(NR,ILAY,IDUM,IFAC,IDUM)                               VDMULT45
                                                                        VDMULT46
C Get global Face Number JFAC                                           VDMULT47
      IRET = VJFACI(ILAY,IFAC,JFAC)                                     VDMULT48
                                                                        VDMULT49
C Local Wafer in face /    since 1024 electronics readout channels      VDMULT50
C correspond to first 1 1/2 wafers on either side => between 2 - 5      VDMULT51
      IWFF1 = VWFFND(ZGC1)                                              VDMULT52
                                                                        VDMULT53
C JFAC, IWFF1 -> JWAF (in fact: JWAF should be IWFF1 for Layer 1)       VDMULT54
      IRET = VJWAFF(JFAC,IWFF1,JWAF)                                    VDMULT55
                                                                        VDMULT56
C get global coor. of wafer-center                                      VDMULT57
      CALL VZERO(ABC,3)                                                 VDMULT58
      IRET = VABCXY(ABC,JWAF,XYZ)                                       VDMULT59
                                                                        VDMULT60
C Assign the proper glocal Z coord.                                     VDMULT61
      XYZ(3) = ZGC1                                                     VDMULT62
                                                                        VDMULT63
C Transform into local coordinates                                      VDMULT64
C     VUW for output                                                    VDMULT65
C     ABC for GP-routines                                               VDMULT66
                                                                        VDMULT67
      IRET = VXYZVU(XYZ,JWAF,VUW)                                       VDMULT68
      ZLC1 = VUW(3)                                                     VDMULT69
      IRET = VXYZAB(XYZ,JWAF,ABC)                                       VDMULT70
C ==                                                                    VDMULT71
C II. local coord abc  -> readout strip                                 VDMULT72
C ==                                                                    VDMULT73
                                                                        VDMULT74
      IRET = VDLCRS(ABC(1),1,RSTRP)                                     VDMULT75
                                                                        VDMULT76
C readout strip can only be integer and must be at least 1. (VDRSPS)    VDMULT77
C => get integer readout strip and store difference between local       VDMULT78
C coord. of readout strip and local coordinate //                       VDMULT79
C add later to multiplexed partner as well     // prevent being zero    VDMULT80
C                                                                       VDMULT81
      IF(RSTRP .LT.1.)THEN                                              VDMULT82
         IOF=1                                                          VDMULT83
         IROS1=1                                                        VDMULT84
      ELSE                                                              VDMULT85
         IOF=0                                                          VDMULT86
         IROS1=RSTRP                                                    VDMULT87
      ENDIF                                                             VDMULT88
                                                                        VDMULT89
C Compute local coordinate of readout strip                             VDMULT90
      IRET = VDRSLC(FLOAT(IROS1),1,PSINT)                               VDMULT91
C distance between Zcoor and strip = correction for later               VDMULT92
      DELT = ABC(1)-PSINT                                               VDMULT93
                                                                        VDMULT94
C VZRSRS need Wafer_in_Module: convert IWFF1 (1-6) to IWAF (1-3)        VDMULT95
      IRET = VFWAFF(IWFF1,IMOD,IWAF1)                                   VDMULT96
                                                                        VDMULT97
C ==                                                                    VDMULT98
C III.                                                                  VDMULT99
C   Main Call: Find the multiplexed strip position and wafer            VDMUL100
C   IWAF1,IROS1 -> IWAF2,IROS2 for multiplexed z-view                   VDMUL101
C ==                                                                    VDMUL102
                                                                        VDMUL103
      IER = VZRSRS(IWAF1,IROS1,IWAF2,IROS2)                             VDMUL104
                                                                        VDMUL105
C ==                                                                    VDMUL106
C IV. Go back to local/global coordinates                               VDMUL107
C ==                                                                    VDMUL108
                                                                        VDMUL109
C readout strip -> local coordinate                                     VDMUL110
      IROS2=IROS2-IOF                                                   VDMUL111
      IRET = VDRSLC(FLOAT(IROS2),1,PSINT)                               VDMUL112
C Add the residual and make it real                                     VDMUL113
      RSTR2 = PSINT+DELT                                                VDMUL114
      ABC(1) = RSTR2                                                    VDMUL115
C Get Local Coordinate for output                                       VDMUL116
      IRET   = VABCVU(ABC,JWAF,VUW)                                     VDMUL117
      ZLC2   = VUW(3)                                                   VDMUL118
C Go back to global coordinates // local indices -> Wafer in face       VDMUL119
      IRET = VIWFFW(IMOD,IWAF2,IWFF2)                                   VDMUL120
C   Make the new index JWAF                                             VDMUL121
      IRET = VJWAFF(JFAC,IWFF2,JWAF)                                    VDMUL122
C Transform to global coordinate                                        VDMUL123
      IRET = VABCXY(ABC,JWAF,XYZ)                                       VDMUL124
C Finished -> z-coord. of multiplexed partner                           VDMUL125
                                                                        VDMUL126
      ZGC2 = XYZ(3)                                                     VDMUL127
                                                                        VDMUL128
                                                                        VDMUL129
      RETURN                                                            VDMUL130
      END                                                               VDMUL131
                                                                        VDMUL132

      SUBROUTINE VDISMR
C----------------------------------------------------------------------
C
C DUMMY ROUTINE // DON'T DO ANY SMEARING FOR VDET95
C
      RETURN
      END


      SUBROUTINE VDMCEF(IER)
C----------------------------------------------------------------------
C
C  Don't use any efficiency map (map from old VDET still in JULIA)
C  => use dummy-routine which does nothing i.e. 100% efficiency
C
      IER=0
      RETURN
      END



      SUBROUTINE VRMWF(IWAF,IV,IROM)                                    VRMWF  2
C---------------------------------------------------------------------- VRMWF  3
C!  Transform wafer number to readout module number                     VRMWF  4
C   Get relevant readout module for a given wafer/view                  VRMWF  5
CKEY VDET TRACK                                                         VRMWF  6
C!                                                                      VRMWF  7
C!   Author   :- A. Bonissent March 1995                                VRMWF  8
C!   Inputs:                                                            VRMWF  9
C!                                                                      VRMWF 10
C!        IWAF       - encoded wafer number (see sbank VDZT/VDXY)       VRMWF 11
C!        IV         - view                                             VRMWF 12
C!                                                                      VRMWF 13
C!   Outputs:                                                           VRMWF 14
C!                                                                      VRMWF 15
C!        IROM       - encoded readout moduule number                   VRMWF 16
C!                                                                      VRMWF 17
C!   What it does :                                                     VRMWF 18
C!                                                                      VRMWF 19
C!    (IWAF has)       (IROM contains)                                  VRMWF 20
C!                -------------------------                             VRMWF 21
C!      WAF#        VDET91           VDET95                             VJWABR 1
C!                IV=1   IV=2        IV=1   IV=2                        VJWABR 2
C!        1        1      1            1     3                          VJWABR 3
C!        2        2      1            2     3                          VJWABR 4
C!        3        3      4            3     3                          VJWABR 5
C!        4        4      4            4     4                          VJWABR 6
C!        5        -      -            5     4                          VJWABR 7
C!        6        -      -            6     4                          VJWABR 8
C?                                                                      VRMWF 30
C!======================================================================VRMWF 31
      INTEGER VDYEAR                                                    VRMWF 33
      IZ = MOD(IWAF/1000,10)                                            VRMWF 34
      IF(VDYEAR().EQ.95)THEN                                            VRMWF 35
        IROM=IWAF                                                       VJWABR 9
        IF(IV.EQ.2)THEN                                                 VJWABR10
          IF(IZ.LE.2)IROM=IWAF-1000*IZ+2000                             VJWABR11
          IF(IZ.GT.2)IROM=IWAF-1000*IZ+3000                             VJWABR12
        ENDIF                                                           VJWABR13
                                                                        VJWABR14
      ELSE                                                              VRMWF 41
        IROM=IWAF                                                       VRMWF 42
        IF(IV.EQ.2)THEN                                                 VRMWF 43
          IF(IZ.EQ.2)IROM=IWAF+1000                                     VRMWF 44
          IF(IZ.EQ.1)IROM=IWAF-1000                                     VRMWF 45
        ENDIF                                                           VRMWF 46
      ENDIF                                                             VRMWF 47
      RETURN                                                            VRMWF 48
      END                                                               VRMWF 49
      SUBROUTINE  VTCLLD(ITK,IL,SCUT,IUORW,NCL)                         VTCLLD 2
C-------------------------------------------------------------          VTCLLD 3
C!Return VDET cluster whitin an given area.                             VTCLLD 4
CKEY VDET TRACK                                                         VTCLLD 5
C                                                                       VTCLLD 6
C  Author     :  B. Mours - 901001                                      VTCLLD 7
C  modified by : B. Mours - 910918                                      VTCLLD 8
C    look for cluster in adjacent wafers                                VTCLLD 9
C                H.G. Moser - 910918                                    VTCLLD10
C    normelise pulseheight before cut, select good clusters             VTCLLD11
C                                                                       VTCLLD12
C                A. Bonissent March 1995                                ABO20437
C          use Vdet year sensitive routime VRMWF                        ABO20438
C          to go from wafer to readout module                           ABO20439
C  We neglect the error comming from the cluster.                       VTCLLD13
C  This routine give at less a dummy cluster (residual=0, sigma=0)      VTCLLD14
C                                                                       VTCLLD15
C    ITK   (in)  Track number                                           VTCLLD16
C    IL    (in)  Layer number                                           VTCLLD17
C    SCUT  (in)  Size of the search area                                VTCLLD18
C    IUORW (in)  = 0 if in xy plan; 1 if in z                           VTCLLD19
C    NCL   (out) Number of found clusters + 1 (dummy cluster)           VTCLLD20
C-------------------------------------------------------------          VTCLLD21
      SAVE                                                              VTCLLD23
C                                                                       VTCLLD24
      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL        VTKREC 2
     +                ,NARCVD                                           VTKREC 3
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD          VTKREC 4
      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,        VTRPAR 2
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX         VTRPAR 3
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB  VTRPAR 4
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX                               VTRPAR 5
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER                    VTRPAR 6
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB             VTRPAR 7
C                                                                       ABO20440
      INTEGER NVTUC,NVTWC,KVTXT,KVDXZ,KVTXC,ICL,NASS,JVTXC,IWAF         VTCLLD27
      REAL    RES,PULSE                                                 VTCLLD28
      REAL VUW(2),XYZ(3),VEC(3),RERRP(10)                               VTCLLD29
      INTEGER IERR,IWAF0                                                VTCLLD30
      INTEGER IROM,IVIEW                                                ABO20441
      INTEGER VNRWAF, VNRMOD                                            VJWABR15
      LOGICAL FIRSTW                                                    VTCLLD31
C - bit 1 (IVETOC=2) is set in VDXY and VDZT quality flag to indicate   VTCLLD32
C   a generic veto hit.                                                 VTCLLD33
C   bit 30 (IMCNEF=536870912) is set in VDXY and VDZT quality flag      VTCLLD34
C   to indicate a MC inefficiency rejected hit.                         VTCLLD35
      INTEGER IVETOC,IMCNEF                                             VTCLLD36
      PARAMETER (IVETOC=2, IMCNEF=536870912)                            VTCLLD37
      LOGICAL FIRST                                                     VTCLLD38
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,  VDXYJJ 2
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,         VDXYJJ 3
     +          JVDXIH=12,LVDXYA=12)                                    VDXYJJ 4
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,  VDZTJJ 2
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)         VDZTJJ 3
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,  FRFTJJ 2
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)       FRFTJJ 3
      PARAMETER(JVTUWI=1,JVTUCI=5,JVTUUC=9,JVTUSU=13,JVTURC=17,         VTUCJJ 2
     +          JVTUPH=21,JVTURE=25,LVTUCA=28)                          VTUCJJ 3
      PARAMETER(JVTWWI=1,JVTWCI=5,JVTWWC=9,JVTWSW=13,JVTWZC=17,         VTWCJJ 2
     +          JVTWRE=21,LVTWCA=24)                                    VTWCJJ 3
      INTEGER JVTXWI,JVTXHF,JVTXUC,JVTXWC,JVTXSU,JVTXSW,                VTXTJJ 2
     +          JVTXUW,JVTXXC,JVTXYC,JVTXZC,JVTXPV,                     VTXTJJ 3
     +          JVTXPU,JVTXPW,JVTXUR,JVTXUT,JVTXUP,                     VTXTJJ 4
     +          JVTXUD,JVTXUZ,JVTXWR,JVTXWT,JVTXWP,                     VTXTJJ 5
     +          JVTXWD,JVTXWZ,LVTXTA                                    VTXTJJ 6
      PARAMETER(JVTXWI=1,JVTXHF=2,JVTXUC=3,JVTXWC=4,JVTXSU=5,JVTXSW=6,  VTXTJJ 7
     +          JVTXUW=7,JVTXXC=8,JVTXYC=9,JVTXZC=10,JVTXPV=11,         VTXTJJ 8
     +          JVTXPU=12,JVTXPW=13,JVTXUR=14,JVTXUT=15,JVTXUP=16,      VTXTJJ 9
     +          JVTXUD=17,JVTXUZ=18,JVTXWR=19,JVTXWT=20,JVTXWP=21,      VTXTJJ10
     +          JVTXWD=22,JVTXWZ=23,LVTXTA=23)                          VTXTJJ11
C!    local common to store work bank indices                           VTBOS  2
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1                             VTBOS  3
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1                  VTBOS  4
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
C!    set of intrinsic functions to handle BOS banks                    BMACRO 2
C - # of words/row in bank with index ID                                BMACRO 3
      LCOLS(ID) = IW(ID+1)                                              BMACRO 4
C - # of rows in bank with index ID                                     BMACRO 5
      LROWS(ID) = IW(ID+2)                                              BMACRO 6
C - index of next row in the bank with index ID                         BMACRO 7
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 8
C - index of row # NRBOS in the bank with index ID                      BMACRO 9
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO10
C - # of free words in the bank with index ID                           BMACRO11
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO12
C - # of free rows in the bank with index ID                            BMACRO13
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO14
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO15
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO16
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO17
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO18
C                                                                       BMACRO19
C                                                                       VTCLLD48
      KVTXT = NLINK('VTXT',ITK)                                         VTCLLD49
      IF(KVTXT.EQ.0) GO TO 999                                          VTCLLD50
C                                                                       VTCLLD51
      NCL = 1                                                           VTCLLD52
      IWAF = ITABL(KVTXT,IL,JVTXWI)                                     VTCLLD53
      FIRSTW = .TRUE.                                                   VTCLLD54
      IMASK = IVETOC+IMCNEF                                             VTCLLD55
      VUW(1) = RTABL(KVTXT,IL,JVTXUC)                                   VTCLLD56
      VUW(2) = RTABL(KVTXT,IL,JVTXWC)                                   VTCLLD57
C                                                                       VTCLLD58
C Get position of middle and last wafer                                 VJWABR16
C                                                                       VJWABR17
      NWFM = VNRWAF()                                                   VJWABR18
      NMD = VNRMOD()                                                    VJWABR19
C                                                                       VJWABR20
C  These numbers are used to decide if we look in adjacent wafer        VJWABR21
C  They should be :                                                     VJWABR22
C                                                                       VJWABR23
C              ILST1         ZMID                                       VJWABR24
C  Old VDET     3            1.5                                        VJWABR25
C  New Vdet     5            2.5                                        VJWABR26
C                                                                       VJWABR27
      ILST1 = NWFM*NMD - 1                                              VJWABR28
      ZMID =  ILST1 * 0.5                                               VJWABR29
C                                                                       VJWABR30
   50 CONTINUE                                                          VTCLLD59
C                                                                       VTCLLD60
      IVIEW=2-IUORW                                                     ABO20442
      CALL VRMWF(IWAF,IVIEW,IROM)                                       ABO20443
      IWAF=IROM                                                         ABO20444
      IF(IUORW.EQ.0) THEN                                               ABO20445
        KVDXZ = NLINK('VDXY',IWAF)                                      ABO20446
        KVTXC = KVTUC                                                   VTCLLD72
      ELSE                                                              VTCLLD73
        KVDXZ = NLINK('VDZT',IWAF)                                      VTCLLD74
        KVTXC = KVTWC                                                   VTCLLD75
      ENDIF                                                             VTCLLD76
      IF(KVDXZ.EQ.0) GO TO 900                                          VTCLLD77
C                                                                       VTCLLD78
C     pulseheight correction factor for inclined tracks                 VTCLLD79
C                                                                       VTCLLD80
      PTOT = SQRT( RTABL(KVTXT,IL,JVTXPV)**2 +                          VTCLLD81
     +             RTABL(KVTXT,IL,JVTXPU)**2 +                          VTCLLD82
     +             RTABL(KVTXT,IL,JVTXPW)**2 )                          VTCLLD83
      PCOR = ABS(RTABL(KVTXT,IL,JVTXPV)) / PTOT                         VTCLLD84
C                                                                       VTCLLD85
C-- Loop over all cluster, keep only the close one                      VTCLLD86
C                                                                       VTCLLD87
      DO 100 ICL = 1,LROWS(KVDXZ)                                       VTCLLD88
        JVTXC = KROW(KVTXC,NCL)                                         VTCLLD89
        IF(IUORW.EQ.0) THEN                                             VTCLLD90
          RES   = RTABL(KVDXZ,ICL,JVDXUC) - VUW(1)                      VTCLLD91
          NASS  = ITABL(KVDXZ,ICL,JVDXNA)                               VTCLLD92
          PULSE = RTABL(KVDXZ,ICL,JVDXPH) * PCOR                        VTCLLD93
          IQFL  = ITABL(KVDXZ,ICL,JVDXQF)                               VTCLLD94
          RW(JVTXC+JVTURE+IL-1) = RES                                   VTCLLD95
        ELSE                                                            VTCLLD96
          RES   = RTABL(KVDXZ,ICL,JVDZWC) - VUW(2)                      VTCLLD97
          NASS  = ITABL(KVDXZ,ICL,JVDZNA)                               VTCLLD98
          PULSE = RTABL(KVDXZ,ICL,JVDZPH) * PCOR                        VTCLLD99
          IQFL  = ITABL(KVDXZ,ICL,JVDZQF)                               VTCLL100
          RW(JVTXC+JVTWRE+IL-1) = RES                                   VTCLL101
        ENDIF                                                           VTCLL102
        IF(IAND(IQFL,IMASK).NE.0)       GO TO 100                       VTCLL103
        IF(ABS(RES).GT.SCUT)            GO TO 100                       VTCLL104
        IF(PULSE.LT.FLOAT(NASS)*PULMIN) GO TO 100                       VTCLL105
C                                                                       VTCLL106
        IF(NCL.GE.MAXCLS) GO TO 999                                     VTCLL107
        IW(JVTXC+JVTUWI+IL-1) = IWAF                                    VTCLL108
        IW(JVTXC+JVTUCI+IL-1) = ICL                                     VTCLL109
        IF(IUORW.EQ.0) THEN                                             VTCLL110
          RW(JVTXC+JVTURC+IL-1) = SQRT( RTABL(KVDXZ,ICL,JVDXXC)**2+     VTCLL111
     +                                  RTABL(KVDXZ,ICL,JVDXYC)**2)     VTCLL112
          RW(JVTXC+JVTUPH+IL-1) = ATAN2(RTABL(KVDXZ,ICL,JVDXYC),        VTCLL113
     +                                  RTABL(KVDXZ,ICL,JVDXXC))        VTCLL114
          RW(JVTXC+JVTUUC+IL-1) =       RTABL(KVDXZ,ICL,JVDXUC)         VTCLL115
          RW(JVTXC+JVTUSU+IL-1) =       RTABL(KVDXZ,ICL,JVDXSU)**2      VTCLL116
        ELSE                                                            VTCLL117
          RW(JVTXC+JVTWZC+IL-1) = RTABL(KVDXZ,ICL,JVDZZC)               VTCLL118
          RW(JVTXC+JVTWWC+IL-1) = RTABL(KVDXZ,ICL,JVDZWC)               VTCLL119
          RW(JVTXC+JVTWSW+IL-1) = RTABL(KVDXZ,ICL,JVDZSW)**2            VTCLL120
        ENDIF                                                           VTCLL121
        NCL = NCL+1                                                     VTCLL122
  100 CONTINUE                                                          VTCLL123
C                                                                       VTCLL124
  900 CONTINUE                                                          VTCLL125
C                                                                       VTCLL126
C-- look for cluster in the adjacent z wafer (only if xy)               VTCLL127
C                                                                       VTCLL128
      IF(FIRSTW) IWAF0 = IWAF                                           VTCLL129
      IF(ITABL(KVTXT,IL,JVTXHF).NE.0 .AND. FIRSTW) THEN                 VTCLL130
        FIRSTW = .FALSE.                                                VTCLL131
        KFRFT = NLINK('FRFT',0)                                         VTCLL132
        IWAFO = ITABL(KVTXT,IL,JVTXWI)                                  VTCLL133
        IF(VUW(2).LT.0.) THEN                                           VTCLL134
          IWAF = IWAFO-1000                                             VTCLL135
        ELSE                                                            VTCLL136
          IWAF = IWAFO+1000                                             VTCLL137
        ENDIF                                                           VTCLL138
        IZED  = MOD(IWAF/1000,10)                                       VTCLL139
        IZEDO = MOD(IWAFO/1000,10)                                      VTCLL140
C                                                                       VJWABR31
C   Look in the neighbouring wafer if :                                 VJWABR32
C   Neighbouring wafer number is a reasonable one                       VJWABR33
C                    (IZ is wafer nb-1)                                 VJWABR34
C       AND                                                             VJWABR35
C   ( View is Z) or                                                     VJWABR36
C   (one of the wafers is in Z>0 and the other is in Z<0)               VJWABR37
C                                                                       VJWABR38
        IF(IWAF.GE.0 .AND. IZED.LE.ILST1 .AND.                          VJWABR39
     +    (IUORW.NE.0 .OR. (IZED-ZMID)*(IZEDO-ZMID).LT.0.)) THEN        VJWABR40
                                                                        VJWABR41
                                                                        VJWABR42
          CALL VTXNWT(IWAF,RW(KROW(KFRFT,ITK)+JFRFIR),                  VTCLL143
     +                     RW(KROW(KVTXT,IL)+JVTXXC),                   VTCLL144
     +                VUW,XYZ,VEC,RERRP,IERR)                           VTCLL145
          IF(IERR.EQ.0) GO TO 50                                        VTCLL146
        ENDIF                                                           VTCLL147
      ENDIF                                                             VTCLL148
      IWAF = IWAF0                                                      VTCLL149
C                                                                       VTCLL150
C  Fill dummy cluster                                                   VTCLL151
C                                                                       VTCLL152
      JVTXC = KROW(KVTXC,NCL)                                           VTCLL153
      IW(JVTXC+JVTUWI+IL-1) = IWAF                                      VTCLL154
      IW(JVTXC+JVTUCI+IL-1) = 0                                         VTCLL155
      IF(IUORW.EQ.0) THEN                                               VTCLL156
        RW(JVTXC+JVTURE+IL-1) = 0.                                      VTCLL157
        RW(JVTXC+JVTURC+IL-1) = SQRT( RTABL(KVTXT,IL,JVTXXC)**2+        VTCLL158
     +                                RTABL(KVTXT,IL,JVTXYC)**2)        VTCLL159
        RW(JVTXC+JVTUPH+IL-1) = ATAN2(RTABL(KVTXT,IL,JVTXYC),           VTCLL160
     +                                RTABL(KVTXT,IL,JVTXXC))           VTCLL161
        RW(JVTXC+JVTUUC+IL-1) =       RTABL(KVTXT,IL,JVTXUC)            VTCLL162
        RW(JVTXC+JVTUSU+IL-1) = BIGERR                                  VTCLL163
      ELSE                                                              VTCLL164
        RW(JVTXC+JVTWRE+IL-1) = 0.                                      VTCLL165
        RW(JVTXC+JVTWZC+IL-1) = RTABL(KVTXT,IL,JVTXZC)                  VTCLL166
        RW(JVTXC+JVTWWC+IL-1) = RTABL(KVTXT,IL,JVTXWC)                  VTCLL167
        RW(JVTXC+JVTWSW+IL-1) = BIGERR                                  VTCLL168
      ENDIF                                                             VTCLL169
C                                                                       VTCLL170
  999 CONTINUE                                                          VTCLL171
      IW(KVTXC+LMHROW) = MAX(NCL,IW(KVTXC+LMHROW))                      VTCLL172
C                                                                       VTCLL173
      RETURN                                                            VTCLL174
      END                                                               VTCLL175
      SUBROUTINE VTFILL (ITK,ICOMB)                                     VTFILL 2
C---------------------------------------------------------------------- VTFILL 3
C! Fill VDCO and VCPL  banks.                                           VTFILL 4
CKEY VDET TRACK                                                         VTFILL 5
C                                                                       VTFILL 6
C  Author      : B. Mours   901001                                      VTFILL 7
C  modified by : TSM        910918                                      VTFILL 8
C     track ambiguity                                                   VTFILL 9
C  modified by : B. Mours   911023                                      VTFILL10
C     dont store in VCPL rejected hit (large error)                     VTFILL11
C                                                                       VTFILL12
C  input : ITK   = track number                                         VTFILL13
C          ICOMB = combinaison number in VTMA bank                      VTFILL14
C---------------------------------------------------------------------- VTFILL15
      SAVE                                                              VTFILL17
C                                                                       VTFILL18
      PARAMETER(JVDCWI=1,JVDCR0=2,JVDCPH=3,JVDCZ0=4,JVDCSR=5,JVDCSZ=6,  VDCOJJ 2
     +          JVDCQF=7,JVDCTN=8,LVDCOA=8)                             VDCOJJ 3
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,  VDXYJJ 2
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,         VDXYJJ 3
     +          JVDXIH=12,LVDXYA=12)                                    VDXYJJ 4
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,  VDZTJJ 2
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)         VDZTJJ 3
      PARAMETER(JVTMNL=1,JVTMNU=2,JVTMNW=3,JVTMC2=4,JVTMIT=5,JVTMFR=6,  VTMAJJ 2
     +          JVTMUW=7,JVTMWW=11,JVTMIU=15,JVTMIW=19,JVTMWI=23,       VTMAJJ 3
     +          JVTMR0=27,JVTMPH=31,JVTMZ0=35,JVTMUC=39,JVTMWC=43,      VTMAJJ 4
     +          JVTMSU=47,JVTMSW=51,JVTMCO=55,LVTMAA=58)                VTMAJJ 5
      PARAMETER(JVCPXB=1,JVCPNX=2,JVCPZB=3,JVCPNZ=4,JVCPC2=5,LVCPLA=5)  VCPLJJ 2
C!    local common to store work bank indices                           VTBOS  2
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1                             VTBOS  3
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1                  VTBOS  4
      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL        VTKREC 2
     +                ,NARCVD                                           VTKREC 3
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD          VTKREC 4
      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,        VTRPAR 2
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX         VTRPAR 3
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB  VTRPAR 4
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX                               VTRPAR 5
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER                    VTRPAR 6
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB             VTRPAR 7
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      INTEGER NFRFT,NVDCO,KFRFT,KVDCO,NVTMA,KVTMA,                      VTFILL28
     +        JVTMA,IL,IVDCO,JVDCO,JVCPL,KVDXY,ICL,KVDZT                VTFILL29
      REAL    SIGMU,SIGMZ                                               VTFILL30
      LOGICAL FIRST                                                     VTFILL31
C-- Local variables for ambiguous pattern search                        VTFILL32
      LOGICAL WAMBIG,UAMBIG,WFOUND,UFOUND                               VTFILL33
      INTEGER NRMUL,KVDZM                                               VJWABR43
      INTEGER JL,IWFW,IWFU,ICLW,ICLU                                    VTFILL34
      REAL CBAUG,CPAUG                                                  VTFILL35
C -- bit 2 (IVPAMB=4) is set in the VDCO quality flag to indicate       VTFILL36
C    a R-Phi ambiguous hit.                                             VTFILL37
C    bit 3 (IVZAMB=8) is set in the VDCO quality flag to indicate       VTFILL38
C    a Z ambiguous hit.                                                 VTFILL39
      INTEGER IVPAMB,IVZAMB                                             VTFILL40
      PARAMETER (IVPAMB=4, IVZAMB=8)                                    VTFILL41
      DATA FIRST/.TRUE./                                                VTFILL42
C                                                                       VTFILL43
C!    set of intrinsic functions to handle BOS banks                    BMACRO 2
C - # of words/row in bank with index ID                                BMACRO 3
      LCOLS(ID) = IW(ID+1)                                              BMACRO 4
C - # of rows in bank with index ID                                     BMACRO 5
      LROWS(ID) = IW(ID+2)                                              BMACRO 6
C - index of next row in the bank with index ID                         BMACRO 7
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 8
C - index of row # NRBOS in the bank with index ID                      BMACRO 9
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO10
C - # of free words in the bank with index ID                           BMACRO11
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO12
C - # of free rows in the bank with index ID                            BMACRO13
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO14
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO15
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO16
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO17
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO18
C                                                                       BMACRO19
C                                                                       VTFILL45
      IF(FIRST) THEN                                                    VTFILL46
        FIRST = .FALSE.                                                 VTFILL47
        NVDCO = NAMIND('VDCO')                                          VTFILL48
        NVTMA = NAMIND('VTMA')                                          VTFILL49
      ENDIF                                                             VTFILL50
C                                                                       VTFILL51
      KVTMA = IW(NVTMA)                                                 VTFILL52
      IF(KVTMA.EQ.0)        GO TO 999                                   VTFILL53
      IF(LROWS(KVTMA).EQ.0) GO TO 999                                   VTFILL54
C                                                                       VTFILL55
      KVCPL = NLINK('VCPL',ITK)                                         VTFILL56
      KVDCO = IW(NVDCO)                                                 VTFILL57
C                                                                       VTFILL58
C-- loop over all clusters found for this track                         VTFILL59
C                                                                       VTFILL60
      JVTMA = KROW(KVTMA,ICOMB)                                         VTFILL61
      IF(IW(JVTMA+JVTMNL).GT.NLYRMX) THEN                               VTFILL62
          IERVTR = 12                                                   VTFILL63
          GO TO 999                                                     VTFILL64
      ENDIF                                                             VTFILL65
C                                                                       VTFILL66
C-- Check other VTMA patterns for ambiguity                             VTFILL67
C   Ambiguity means the AUGMENTED chisquare of another pattern          VTFILL68
C   (which includes a penalty for using fewer hits)                     VTFILL69
C   is not significantly more than the AUGMENTED chisquare              VTFILL70
C   of the "best" pattern                                               VTFILL71
C   (significantly means more than CH2AMB units larger)                 VTFILL72
C                                                                       VTFILL73
C-- Initialize flags                                                    VTFILL74
      WAMBIG=.FALSE.                                                    VTFILL75
      UAMBIG=.FALSE.                                                    VTFILL76
C-- Get "best" augmented chisquare                                      VTFILL77
      CBAUG=RW(KROW(KVTMA,ICOMB)+JVTMFR)                                VTFILL78
C-- Loop over patterns                                                  VTFILL79
      DO 70 JCOMB = 1, LROWS(KVTMA)                                     VTFILL80
C-- Save time                                                           VTFILL81
        IF (WAMBIG .AND. UAMBIG) GO TO 70                               VTFILL82
C-- Don't compare pattern to itself                                     VTFILL83
        IF (JCOMB .EQ. ICOMB) GO TO 70                                  VTFILL84
C-- Get augmented chisquare of this pattern                             VTFILL85
        CPAUG=RW(KROW(KVTMA,JCOMB)+JVTMFR)                              VTFILL86
C-- Compare                                                             VTFILL87
        IF (CPAUG .GT. CBAUG+CH2AMB) GO TO 70                           VTFILL88
C-- We have found an ambiguous pattern                                  VTFILL89
C-- The following stuff is only necessary to determine if the ambiguity VTFILL90
C--   is in phi, z, or both                                             VTFILL91
C-- Loop over layers in ICOMB                                           VTFILL92
        DO 60 IL = 1, IW(JVTMA+JVTMNL)                                  VTFILL93
C-- Save time                                                           VTFILL94
          IF (WAMBIG .AND. UAMBIG) GO TO 60                             VTFILL95
C-- Find wafer and cluster numbers                                      VTFILL96
          IWFW=IW(JVTMA+IL-1+JVTMWW)                                    VTFILL97
          ICLW=IW(JVTMA+IL-1+JVTMIW)                                    VTFILL98
          IWFU=IW(JVTMA+IL-1+JVTMUW)                                    VTFILL99
          ICLU=IW(JVTMA+IL-1+JVTMIU)                                    VTFIL100
C-- Initialize flags for layer hit search                               VTFIL101
          WFOUND=.FALSE.                                                VTFIL102
          UFOUND=.FALSE.                                                VTFIL103
C-- Loop over layers in JCOMB                                           VTFIL104
          DO 50 JL = 1, IW(KROW(KVTMA,JCOMB)+JVTMNL)                    VTFIL105
C-- Save time                                                           VTFIL106
            IF (.NOT. WFOUND) THEN                                      VTFIL107
C-- Check if same W wafer                                               VTFIL108
              IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMWW) .EQ. IWFW) THEN     VTFIL109
C-- Set flag if cluster number matches or alternate is null hit         VTFIL110
                IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMIW) .EQ. ICLW .OR.    VTFIL111
     >                 RW(KROW(KVTMA,JCOMB)+JL-1+JVTMSW).GT.HBIGER) THENVTFIL112
                  WFOUND=.TRUE.                                         VTFIL113
                ENDIF                                                   VTFIL114
              ENDIF                                                     VTFIL115
            ENDIF                                                       VTFIL116
C-- Same for U hit                                                      VTFIL117
            IF (.NOT. UFOUND) THEN                                      VTFIL118
              IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMUW) .EQ. IWFU) THEN     VTFIL119
                IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMIU) .EQ. ICLU .OR.    VTFIL120
     >                 RW(KROW(KVTMA,JCOMB)+JL-1+JVTMSU).GT.HBIGER) THENVTFIL121
                  UFOUND=.TRUE.                                         VTFIL122
                ENDIF                                                   VTFIL123
              ENDIF                                                     VTFIL124
            ENDIF                                                       VTFIL125
C-- End loop over JCOMB layers                                          VTFIL126
   50     CONTINUE                                                      VTFIL127
C-- Update ambiguity flags                                              VTFIL128
          UAMBIG=UAMBIG .OR. .NOT. UFOUND                               VTFIL129
          WAMBIG=WAMBIG .OR. .NOT. WFOUND                               VTFIL130
C       (i.e. Ambiguous if the hit on the best track was NOT            VTFIL131
C               present on the 2nd best track)                          VTFIL132
C                                                                       VTFIL133
C-- End loop over ICOMB layers                                          VTFIL134
   60   CONTINUE                                                        VTFIL135
C-- End loop over JCOMB=other VTMA patterns                             VTFIL136
   70 CONTINUE                                                          VTFIL137
C-- WAMBIG and UAMBIG now contain the ambiguity information             VTFIL138
      NARCVD = 0                                                        VTFIL139
C                                                                       VTFIL140
      DO 100 IL=1,IW(JVTMA+JVTMNL)                                      VTFIL141
        SIGMU = RW(JVTMA+JVTMSU+IL-1)                                   VTFIL142
        SIGMZ = RW(JVTMA+JVTMSW+IL-1)                                   VTFIL143
        IF(SIGMU.GT.HBIGER .AND. SIGMZ.GT.HBIGER)  GO TO 100            VTFIL144
        NARCVD = NARCVD + 1                                             VTFIL145
        IF(NARCVD.EQ.1) IOFVCL = LROWS(KVDCO)                           VTFIL146
C                                                                       VTFIL147
C-- fill VDCO bank                                                      VTFIL148
C                                                                       VTFIL149
        JVDCO = KNEXT(KVDCO)                                            VTFIL150
        IW(JVDCO+JVDCWI) = IW(JVTMA+JVTMWW+IL-1)                        VTFIL151
        RW(JVDCO+JVDCR0) = RW(JVTMA+JVTMR0+IL-1)                        VTFIL152
        RW(JVDCO+JVDCPH) = RW(JVTMA+JVTMPH+IL-1)                        VTFIL153
        RW(JVDCO+JVDCZ0) = RW(JVTMA+JVTMZ0+IL-1)                        VTFIL154
        RW(JVDCO+JVDCSR) = SIGMU                                        VTFIL155
        RW(JVDCO+JVDCSZ) = SIGMZ                                        VTFIL156
        IW(JVDCO+JVDCQF) = 3                                            VTFIL157
        IF(SIGMU.GT.HBIGER) IW(JVDCO+JVDCQF) = IW(JVDCO+JVDCQF) - 1     VTFIL158
        IF(SIGMZ.GT.HBIGER) IW(JVDCO+JVDCQF) = IW(JVDCO+JVDCQF) - 2     VTFIL159
C-- Set ambiguity bits                                                  VTFIL160
        IF (UAMBIG) IW(JVDCO+JVDCQF)=IOR(IW(JVDCO+JVDCQF),IVPAMB)       VTFIL161
        IF (WAMBIG) IW(JVDCO+JVDCQF)=IOR(IW(JVDCO+JVDCQF),IVZAMB)       VTFIL162
        IW(JVDCO+JVDCTN) = ITK                                          VTFIL163
        IW(KVDCO+LMHROW) = LROWS(KVDCO) + 1                             VTFIL164
C                                                                       VTFIL165
C-- fill the VCPL bank                                                  VTFIL166
C                                                                       VTFIL167
        JVCPL = KNEXT(KVCPL)                                            VTFIL168
        IW(JVCPL+JVCPXB) = IW(JVTMA+JVTMUW+IL-1)                        VTFIL169
        IW(JVCPL+JVCPZB) = IW(JVTMA+JVTMWW+IL-1)                        VTFIL170
        IF(SIGMU.LT.HBIGER) IW(JVCPL+JVCPNX) = IW(JVTMA+JVTMIU+IL-1)    VTFIL171
        IF(SIGMZ.LT.HBIGER) IW(JVCPL+JVCPNZ) = IW(JVTMA+JVTMIW+IL-1)    VTFIL172
        RW(JVCPL+JVCPC2) = 0.                                           VTFIL173
        IW(KVCPL+LMHROW) = LROWS(KVCPL) + 1                             VTFIL174
C                                                                       VTFIL175
C-- update Nass in VDXT                                                 VJWABR44
C                                                                       VTFIL177
        KVDXY = NLINK('VDXY',IW(JVCPL+JVCPXB))                          VTFIL178
        IF(KVDXY.NE.0) THEN                                             VTFIL179
          ICL = IW(JVCPL+JVCPNX)                                        VTFIL180
          IF(ICL.NE.0) IW(KROW(KVDXY,ICL)+JVDXNA) =                     VTFIL181
     +                 IW(KROW(KVDXY,ICL)+JVDXNA) + 1                   VTFIL182
        ENDIF                                                           VTFIL183
C                                                                       VJWABR45
C-- update Nass in VDZT                                                 VJWABR46
C                                                                       VJWABR47
C-- for VDET95: increment also NASS in bank from multiplexed partner    VJWABR48
C   -> from the two clusters ("original" and "multiplexed") only one    VJWABR49
C      get assigned to a track.                                         VJWABR50
C      Since the tracks are sorted according to their error-ellipse,    VJWABR51
C      the most probable solution is taken automatically. Clusters      VJWABR52
C      with PH>900, set in VTCLLD, may be assigned to > 1 track.        VJWABR53
C                                                                       VJWABR54
        KVDZT = NLINK('VDZT',IW(JVCPL+JVCPZB))                          VTFIL184
        IF(KVDZT.NE.0) THEN                                             VTFIL185
          ICL = IW(JVCPL+JVCPNZ)                                        VTFIL186
          IF(ICL.NE.0) IW(KROW(KVDZT,ICL)+JVDZNA) =                     VTFIL187
     +                 IW(KROW(KVDZT,ICL)+JVDZNA) + 1                   VTFIL188
                                                                        VJWABR55
          CALL VGTMPP(IW(JVCPL+JVCPZB),ICL,NRMUL,ICLM)                  VJWABR56
          KVDZM = NLINK('VDZT',NRMUL)                                   VJWABR57
          IF(KVDZM.NE.0.AND.ICLM.NE.0)                                  VJWABR58
     +                 IW(KROW(KVDZM,ICLM)+JVDZNA) =                    VJWABR59
     +                 IW(KROW(KVDZM,ICLM)+JVDZNA) + 1                  VJWABR60
                                                                        VJWABR61
                                                                        VJWABR62
        ENDIF                                                           VTFIL189
C                                                                       VTFIL190
  100   CONTINUE                                                        VTFIL191
C                                                                       VTFIL192
  999 RETURN                                                            VTFIL193
      END                                                               VTFIL194
      SUBROUTINE  VTLINK(ITK)                                           VTLINK 2
C-------------------------------------------------------------          VTLINK 3
C!Track link with vertex detector clusters                              VTLINK 4
CKEY VDET TRACK                                                         VTLINK 5
C                                                                       VTLINK 6
C  Author      : B. Mours     901001                                    VTLINK 7
C  modified by : B, Mours     910918                                    VTLINK 8
C     adjust error for large pulses or double pulses                    VTLINK 9
C  modified by : B, Mours     911023                                    VTLINK10
C     do not associate half VDET hit to track without ITC hit           VTLINK11
C  modified by : B, Mours     920213                                    VTLINK12
C     change the definition of large pulses for increased errors        VTLINK13
C  modified by : D Brown,   920927                                      VTLINK14
C     Small change in logic for large pulseheight error assignment      VTLINK15
C  input is the track number . (in FRFT bank)                           VTLINK16
C                                                                       VTLINK17
C   Use extrapolated TPC+ITC tracks to the VDET. Do Track cluster       VTLINK18
C   association and fill the VTMA bank.                                 VTLINK19
C                                                                       VTLINK20
C-------------------------------------------------------------          VTLINK21
      SAVE                                                              VTLINK23
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL        VTKREC 2
     +                ,NARCVD                                           VTKREC 3
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD          VTKREC 4
      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,        VTRPAR 2
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX         VTRPAR 3
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB  VTRPAR 4
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX                               VTRPAR 5
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER                    VTRPAR 6
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB             VTRPAR 7
      PARAMETER(JVTMNL=1,JVTMNU=2,JVTMNW=3,JVTMC2=4,JVTMIT=5,JVTMFR=6,  VTMAJJ 2
     +          JVTMUW=7,JVTMWW=11,JVTMIU=15,JVTMIW=19,JVTMWI=23,       VTMAJJ 3
     +          JVTMR0=27,JVTMPH=31,JVTMZ0=35,JVTMUC=39,JVTMWC=43,      VTMAJJ 4
     +          JVTMSU=47,JVTMSW=51,JVTMCO=55,LVTMAA=58)                VTMAJJ 5
      PARAMETER(JVTSC2=1,JVTSCI=2,LVTSCA=5)                             VTSCJJ 2
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,  VDXYJJ 2
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,         VDXYJJ 3
     +          JVDXIH=12,LVDXYA=12)                                    VDXYJJ 4
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,  VDZTJJ 2
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)         VDZTJJ 3
      PARAMETER(JVTUWI=1,JVTUCI=5,JVTUUC=9,JVTUSU=13,JVTURC=17,         VTUCJJ 2
     +          JVTUPH=21,JVTURE=25,LVTUCA=28)                          VTUCJJ 3
      PARAMETER(JVTWWI=1,JVTWCI=5,JVTWWC=9,JVTWSW=13,JVTWZC=17,         VTWCJJ 2
     +          JVTWRE=21,LVTWCA=24)                                    VTWCJJ 3
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,  FRTLJJ 2
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)                             FRTLJJ 3
      INTEGER JVTXWI,JVTXHF,JVTXUC,JVTXWC,JVTXSU,JVTXSW,                VTXTJJ 2
     +          JVTXUW,JVTXXC,JVTXYC,JVTXZC,JVTXPV,                     VTXTJJ 3
     +          JVTXPU,JVTXPW,JVTXUR,JVTXUT,JVTXUP,                     VTXTJJ 4
     +          JVTXUD,JVTXUZ,JVTXWR,JVTXWT,JVTXWP,                     VTXTJJ 5
     +          JVTXWD,JVTXWZ,LVTXTA                                    VTXTJJ 6
      PARAMETER(JVTXWI=1,JVTXHF=2,JVTXUC=3,JVTXWC=4,JVTXSU=5,JVTXSW=6,  VTXTJJ 7
     +          JVTXUW=7,JVTXXC=8,JVTXYC=9,JVTXZC=10,JVTXPV=11,         VTXTJJ 8
     +          JVTXPU=12,JVTXPW=13,JVTXUR=14,JVTXUT=15,JVTXUP=16,      VTXTJJ 9
     +          JVTXUD=17,JVTXUZ=18,JVTXWR=19,JVTXWT=20,JVTXWP=21,      VTXTJJ10
     +          JVTXWD=22,JVTXWZ=23,LVTXTA=23)                          VTXTJJ11
C                                                                       VTLINK35
C!    local common to store work bank indices                           VTBOS  2
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1                             VTBOS  3
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1                  VTBOS  4
C                                                                       VTLINK37
C  Local variables                                                      VTLINK38
C                                                                       VTLINK39
      INTEGER NVTUC,NVTWC,KVTXT,JVTUC,JVTWC,JVTXT,JVTMA,KVTMA,NVTMA     VTLINK40
      INTEGER IL,ICOMB,NXY,NZT,NCOMB,IZT,ICLSU,ICLSW,IDWFU,IDWW,NWAF    VTLINK41
     +       ,NU,NW,NUW                                                 VTLINK42
      REAL    SIGMU,SIGMW,VUW(3),XYZ(3),SU(4),SW(4)                     VTLINK43
      REAL PCOR(4)                                                      VTLINK44
C - bit 0 (ISEPBT=1) is set in the VDXY and VDZT quality flag           VTLINK45
C   to indicate a separated hit                                         VTLINK46
      INTEGER ISEPBT                                                    VTLINK47
      PARAMETER (ISEPBT=1)                                              VTLINK48
      LOGICAL FIRST                                                     VTLINK49
      DATA FIRST/.TRUE./                                                VTLINK50
C                                                                       VTLINK51
C  Include explicitly the JULIA *CD BOSEXT 5-2-91 D.BROWN               VTLINK52
C                                                                       VTLINK53
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP                   VTLINK54
      CHARACTER*4 CHAINT                                                VTLINK55
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP                           VTLINK56
C!    set of intrinsic functions to handle BOS banks                    BMACRO 2
C - # of words/row in bank with index ID                                BMACRO 3
      LCOLS(ID) = IW(ID+1)                                              BMACRO 4
C - # of rows in bank with index ID                                     BMACRO 5
      LROWS(ID) = IW(ID+2)                                              BMACRO 6
C - index of next row in the bank with index ID                         BMACRO 7
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 8
C - index of row # NRBOS in the bank with index ID                      BMACRO 9
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO10
C - # of free words in the bank with index ID                           BMACRO11
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO12
C - # of free rows in the bank with index ID                            BMACRO13
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO14
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO15
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO16
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO17
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO18
C                                                                       BMACRO19
C                                                                       VTLINK58
C - 1st entry                                                           VTLINK59
C                                                                       VTLINK60
      IF (FIRST) THEN                                                   VTLINK61
         NVTMA = NAMIND('VTMA')                                         VTLINK62
         NFRTL = NAMIND('FRTL')                                         VTLINK63
         FIRST = .FALSE.                                                VTLINK64
      ENDIF                                                             VTLINK65
C                                                                       VTLINK66
C - reset the number og good clusters to 0                              VTLINK67
C                                                                       VTLINK68
      KVTMA = IW(NVTMA)                                                 VTLINK69
      IW(KVTMA+LMHROW) = 0                                              VTLINK70
C                                                                       VTLINK71
C-- check if Extrapolation bank there for this track                    VTLINK72
C                                                                       VTLINK73
      IF(NLINK('VTXT',ITK).EQ.0) GO TO 999                              VTLINK74
      IF(NLINK('VTER',ITK).EQ.0) GO TO 999                              VTLINK75
C                                                                       VTLINK76
C                                                                       VTLINK77
      CALL VZERO (IW(KVTUC+LMHCOL+1),IW(KVTUC)-LMHCOL)                  VTLINK78
      CALL VZERO (IW(KVTWC+LMHCOL+1),IW(KVTWC)-LMHCOL)                  VTLINK79
      CALL VZERO (IW(KVTS0+LMHCOL+1),IW(KVTS0)-LMHCOL)                  VTLINK80
      CALL VZERO (IW(KVTS1+LMHCOL+1),IW(KVTS1)-LMHCOL)                  VTLINK81
C                                                                       VTLINK82
C-- do cluster association in U and W plan separetly                    VTLINK83
C                                                                       VTLINK84
      CALL VTCLAS(ITK,0,NXY)                                            VTLINK85
      CALL VTCLAS(ITK,1,NZT)                                            VTLINK86
C                                                                       VTLINK87
C-- do the space association: Loop over all possible U/W combinaisons,  VTLINK88
C   compute the overall chisquare and store the good one in VTMA.       VTLINK89
C   (remember: the last combinaison IXY=NXY & IZT=NZT is empty,         VTLINK90
C   so we don't load it)                                                VTLINK91
C                                                                       VTLINK92
      KFRTL = IW(NFRTL)                                                 VTLINK93
      NITC  = ITABL(KFRTL,ITK,JFRTNI)                                   VTLINK94
      KVTMA = IW(NVTMA)                                                 VTLINK95
      NCOMB = NXY*NZT-1                                                 VTLINK96
      IW(KVTMA+LMHROW) = 0                                              VTLINK97
      IF(NCOMB.LE.0)             GO TO 999                              VTLINK98
      IF(NCOMB.GT.MAXCOM*MAXCOM) GO TO 999                              VTLINK99
      CALL VZERO (IW(KVTMA+LMHLEN+1),NCOMB*LVTMAA)                      VTLIN100
C                                                                       VTLIN101
      KVTXT = NLINK('VTXT',ITK)                                         VTLIN102
      DO 50 IL=1,NLAYER                                                 VTLIN103
         JVTXT = KROW(KVTXT,IL)                                         VTLIN104
         CALL VDHTER(IW(JVTXT+JVTXWI),RW(JVTXT+JVTXPV),                 VTLIN105
     +               RW(JVTXT+JVTXPU),RW(JVTXT+JVTXPW),                 VTLIN106
     +               RW(JVTXT+JVTXUC),RW(JVTXT+JVTXWC),                 VTLIN107
     +               USNOIS,WSNOIS,SU(IL),SW(IL))                       VTLIN108
         PTOT = SQRT(RW(JVTXT+JVTXPV)**2 +                              VTLIN109
     +               RW(JVTXT+JVTXPU)**2 +                              VTLIN110
     +               RW(JVTXT+JVTXPW)**2 )                              VTLIN111
         PCOR(IL) =  ABS(RW(JVTXT+JVTXPV)) / PTOT                       VTLIN112
         JVTXT = JVTXT + LCOLS(KVTXT)                                   VTLIN113
  50  CONTINUE                                                          VTLIN114
C                                                                       VTLIN115
      DO 200 IXY = 1,NXY                                                VTLIN116
        DO 200 IZT = 1,NZT                                              VTLIN117
          IF(IXY.EQ.NXY .AND. IZT.EQ.NZT) GO TO 200                     VTLIN118
          DO 100 IL = 1,NLAYER                                          VTLIN123
            ICLSU = IW(KROW(KVTS0,IXY)+JVTSCI+IL-1)                     VTLIN124
            ICLSW = IW(KROW(KVTS1,IZT)+JVTSCI+IL-1)                     VTLIN125
            IF(RW(KROW(KVTUC,ICLSU)+JVTUSU+IL-1).GT.HBIGER) GO TO 100   VTLIN126
            IF(RW(KROW(KVTWC,ICLSW)+JVTWSW+IL-1).GT.HBIGER) GO TO 100   VTLIN127
            IDWFU = IW(KROW(KVTUC,ICLSU)+JVTUWI+IL-1)                   VTLIN128
            IDWFW = IW(KROW(KVTWC,ICLSW)+JVTWWI+IL-1)                   VTLIN129
C                                                                       VJWABR63
C Check that u- and w-cluster are in the same module,                   VJWABR64
C VRMWF transforms the wafer identifier into a u module identifier.     VJWABR65
C                                                                       VJWABR66
            CALL VRMWF(IDWFW,2,IROM)                                    VJWABR67
            IF(IDWFU.NE.IROM) GO TO 200                                 VJWABR68
  100     CONTINUE                                                      VTLIN136
C                                                                       VTLIN137
C                                                                       VTLIN138
C-- remove track with only half a VDET cluster to reduce missaciation   VTLIN139
C   (we cut a harder for track without ITC hits to remove conversion)   VTLIN140
C                                                                       VTLIN141
          IF(NLAYER.GE.2) THEN                                          VTLIN142
            NU  = 0                                                     VTLIN143
            NW  = 0                                                     VTLIN144
            NUW = 0                                                     VTLIN145
            DO 130 IL = 1,NLAYER                                        VTLIN146
              ICLSU = IW(KROW(KVTS0,IXY)+JVTSCI+IL-1)                   VTLIN147
              ICLSW = IW(KROW(KVTS1,IZT)+JVTSCI+IL-1)                   VTLIN148
              SIGMU = RW(KROW(KVTUC,ICLSU)+JVTUSU+IL-1)                 VTLIN149
              SIGMW = RW(KROW(KVTWC,ICLSW)+JVTWSW+IL-1)                 VTLIN150
              IF(SIGMU.LT.HBIGER) NU = NU+1                             VTLIN151
              IF(SIGMW.LT.HBIGER) NW = NW+1                             VTLIN152
              IF(SIGMU.LT.HBIGER .AND.                                  VTLIN153
     +           SIGMW.LT.HBIGER) NUW = NUW+1                           VTLIN154
  130       CONTINUE                                                    VTLIN155
            IF(NU+NW.LE.1) GO TO 200                                    VTLIN156
            IF(NITC.EQ.0 .AND. NUW.LE.0 .AND. MAX(NU,NW).LE.1) GO TO 200VTLIN157
          ENDIF                                                         VTLIN158
C                                                                       VTLIN159
C-- load cluster values into VTMA bank                                  VTLIN160
C                                                                       VTLIN161
          ICOMB = LROWS(KVTMA) + 1                                      VTLIN162
          IW(KVTMA+LMHROW) = ICOMB                                      VTLIN163
          JVTMA = KROW(KVTMA,ICOMB)                                     VTLIN164
          IW(JVTMA+JVTMNL) = NLAYER                                     VTLIN165
          IW(JVTMA+JVTMNU) = 0                                          VTLIN166
          IW(JVTMA+JVTMNW) = 0                                          VTLIN167
          RW(JVTMA+JVTMC2) = 0.                                         VTLIN168
          IW(JVTMA+JVTMIT) = ITK                                        VTLIN169
          DO 150 IL=1,NLAYER                                            VTLIN170
            ICLSU = IW(KROW(KVTS0,IXY)+JVTSCI+IL-1)                     VTLIN171
            ICLSW = IW(KROW(KVTS1,IZT)+JVTSCI+IL-1)                     VTLIN172
            JVTUC = KROW(KVTUC,ICLSU)                                   VTLIN173
            JVTWC = KROW(KVTWC,ICLSW)                                   VTLIN174
            IW(JVTMA+JVTMUW+IL-1) = IW(JVTUC+JVTUWI+IL-1)               VTLIN175
            IW(JVTMA+JVTMWW+IL-1) = IW(JVTWC+JVTWWI+IL-1)               VTLIN176
            IW(JVTMA+JVTMIU+IL-1) = IW(JVTUC+JVTWCI+IL-1)               VTLIN177
            IW(JVTMA+JVTMIW+IL-1) = IW(JVTWC+JVTUCI+IL-1)               VTLIN178
C                                                                       VTLIN179
C-- convert U,W to x,y,z using alignement                               VTLIN180
C                                                                       VTLIN181
            NWAF = IW(JVTMA+JVTMWW+IL-1)                                VTLIN182
            VUW(1) = 0.                                                 VTLIN183
            VUW(2) = RW(JVTUC+JVTUUC+IL-1)                              VTLIN184
            VUW(3) = RW(JVTWC+JVTWWC+IL-1)                              VTLIN185
            CALL VGWFXY(IW(JVTMA+JVTMWW+IL-1),VUW,XYZ)                  VTLIN186
            RW(JVTMA+JVTMUC+IL-1) = VUW(2)                              VTLIN187
            RW(JVTMA+JVTMWC+IL-1) = VUW(3)                              VTLIN188
            RW(JVTMA+JVTMR0+IL-1) = SQRT(XYZ(1)**2+XYZ(2)**2)           VTLIN189
            RW(JVTMA+JVTMPH+IL-1) = ATAN2(XYZ(2),XYZ(1))                VTLIN190
            RW(JVTMA+JVTMZ0+IL-1) = XYZ(3)                              VTLIN191
C                                                                       VTLIN192
            SIGMU = RW(JVTUC+JVTUSU+IL-1)                               VTLIN193
            SIGMW = RW(JVTWC+JVTWSW+IL-1)                               VTLIN194
            IF(SIGMU.LT.HBIGER) SIGMU = SU(IL)                          VTLIN195
            IF(SIGMW.LT.HBIGER) SIGMW = SW(IL)                          VTLIN196
C                                                                       VTLIN197
C           Check if we should increase U error                         VTLIN198
C           (for double pulse or too big pulses)                        VTLIN199
C                                                                       VTLIN200
            PULSU = 0.                                                  VTLIN201
            IWAF = IW(JVTUC+JVTUWI+IL-1)                                VTLIN202
            ICL  = IW(JVTUC+JVTUCI+IL-1)                                VTLIN203
            KVDXY = NLINK('VDXY',IWAF)                                  VTLIN204
            IF(KVDXY.NE.0 .AND. ICL.GT.0) THEN                          VTLIN205
              IQFLG = ITABL(KVDXY,ICL,JVDXQF)                           VTLIN206
              IF(IAND(IQFLG,ISEPBT).NE.0) SIGMU = ESPLP2                VTLIN207
              PULSU = RTABL(KVDXY,ICL,JVDXPH)*PCOR(IL)                  VTLIN208
            ENDIF                                                       VTLIN209
C                                                                       VTLIN210
C          Check if we should increase W error                          VTLIN211
C                                                                       VTLIN212
            PULSW = 0.                                                  VTLIN213
            IWAF = IW(JVTWC+JVTWWI+IL-1)                                VTLIN214
            ICL  = IW(JVTWC+JVTWCI+IL-1)                                VTLIN215
            KVDZT = NLINK('VDZT',IWAF)                                  VTLIN216
            IF(KVDZT.NE.0 .AND. ICL.GT.0) THEN                          VTLIN217
              IQFLG = ITABL(KVDZT,ICL,JVDZQF)                           VTLIN218
              IF(IAND(IQFLG,ISEPBT).NE.0) SIGMW = ESPLP2                VTLIN219
              PULSW = RTABL(KVDZT,ICL,JVDZPH)*PCOR(IL)                  VTLIN220
            ENDIF                                                       VTLIN221
C                                                                       VTLIN222
            IF(ABS(PULSU-PULSW).GT..35*PULMIN .AND.                     VTLIN223
     +         MIN(PULSU,PULSW).GT.1..AND.                              VTLIN224
     +         MAX(PULSU,PULSW).GT.PULMIN) THEN                         VTLIN225
C                                                                       VTLIN226
C  Pulseheights don't match, and at least 1 is large;  Increase         VTLIN227
C  the error for the larger pulseheight hit as for a double hit.        VTLIN228
C                                                                       VTLIN229
              IF(PULSU.GT.PULSW)THEN                                    VTLIN230
                SIGMU = ELARP2                                          VTLIN231
              ELSE                                                      VTLIN232
                SIGMW = ELARP2                                          VTLIN233
              END IF                                                    VTLIN234
            ENDIF                                                       VTLIN235
C                                                                       VTLIN236
            RW(JVTMA+JVTMSU+IL-1) = SIGMU                               VTLIN237
            RW(JVTMA+JVTMSW+IL-1) = SIGMW                               VTLIN238
            IF(SIGMU.LT.HBIGER) IW(JVTMA+JVTMNU) = IW(JVTMA+JVTMNU)+1   VTLIN239
            IF(SIGMW.LT.HBIGER) IW(JVTMA+JVTMNW) = IW(JVTMA+JVTMNW)+1   VTLIN240
            RW(JVTMA+JVTMCO+IL-1) = 0.                                  VTLIN241
 150      CONTINUE                                                      VTLIN242
C                                                                       VTLIN243
 200    CONTINUE                                                        VTLIN244
C                                                                       VTLIN245
C                                                                       VTLIN246
 999  CONTINUE                                                          VTLIN247
      RETURN                                                            VTLIN248
C                                                                       VTLIN249
      END                                                               VTLIN250
      SUBROUTINE VGTMPP(NRVDZ,IROW,NRMUL,IROWM)                         VGTMPP 2
C---------------------------------------------------------------------- VGTMPP 3
C!   Description                                                        VGTMPP 4
C!   ===========                                                        VGTMPP 5
C!   RETURNS K-IDENTIFIER AND IROW FOR THE MULTIPLEXED PARTNER OF       VGTMPP 6
C!   A CLUSTER IN THE VDZT-BANK                                         VGTMPP 7
C!   KVDZM = 0 FOR NON VDET95-DATA                                      VGTMPP 8
C!                                                                      VGTMPP 9
C!   INPUT : NRVDZ   NR  of            VDZT Bank                        VGTMPP10
C!           IROW    Row of Cluster in VDZT Bank                        VGTMPP11
C!   OUTPUT: NRMUL   NR  of            Multiplexed VDZT Bank            VGTMPP12
C!           IROWM   Row of Cluster in Multiplexed VDZT Bank            VGTMPP13
C!                                                                      VGTMPP14
C!   Author   :- Armin Wagner          15-MAY-1995                      VGTMPP15
C!======================================================================VGTMPP16
      INTEGER JVZMNR,JVZMRO,LVZMRA                                      VZMRJJ 2
      PARAMETER(JVZMNR=1,JVZMRO=2,LVZMRA=2)                             VZMRJJ 3
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON /BCS/   IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
                                                                        VGTMPP19
      INTEGER NRVDZ,IROW                                                VGTMPP20
      INTEGER NRMUL,KVDZM,IROWM,KLINE                                   VGTMPP21
                                                                        VGTMPP22
C!    set of intrinsic functions to handle BOS banks                    BMACRO 2
C - # of words/row in bank with index ID                                BMACRO 3
      LCOLS(ID) = IW(ID+1)                                              BMACRO 4
C - # of rows in bank with index ID                                     BMACRO 5
      LROWS(ID) = IW(ID+2)                                              BMACRO 6
C - index of next row in the bank with index ID                         BMACRO 7
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)                       BMACRO 8
C - index of row # NRBOS in the bank with index ID                      BMACRO 9
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)                 BMACRO10
C - # of free words in the bank with index ID                           BMACRO11
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)                              BMACRO12
C - # of free rows in the bank with index ID                            BMACRO13
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)                               BMACRO14
C - Lth integer element of the NRBOSth row of the bank with index ID    BMACRO15
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO16
C - Lth real element of the NRBOSth row of the bank with index ID       BMACRO17
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)            BMACRO18
C                                                                       BMACRO19
C-----------------------------------------                              VGTMPP24
                                                                        VGTMPP25
C                                                                       VGTMPP26
C ACCESS PARALLEL RELATION BANK                                         VGTMPP27
C                                                                       VGTMPP28
      KVZMR = NLINK('VZMR',NRVDZ)                                       VGTMPP29
      IF (KVZMR.EQ.0) GOTO 999                                          VGTMPP30
      KLINE = KROW (KVZMR,IROW)                                         VGTMPP31
                                                                        VGTMPP32
C                                                                       VGTMPP33
C GET NR AND ROW OF Z-CLUSTER FOR MULTIPLEXED CLUSTER                   VGTMPP34
C                                                                       VGTMPP35
      NRMUL = IW(KLINE+JVZMNR)                                          VGTMPP36
      IROWM = IW(KLINE+JVZMRO)                                          VGTMPP37
                                                                        VGTMPP38
999   RETURN                                                            VGTMPP39
      END                                                               VGTMPP40
