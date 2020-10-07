CDECK  ID>, ARRIVE.
      SUBROUTINE ARRIVE

C-    CONTROL ROUTINE FOR ALL INPUT, EXCEPT FILE-NAME READ BY DOSPGO

      COMMON /MQCMOV/NQSYSS
      COMMON /MQCM/         NQSYSR,NQSYSL,NQLINK,LQWORG,LQWORK,LQTOL
     +,              LQSTA,LQEND,LQFIX,NQMAX, NQRESV,NQMEM,LQADR,LQADR2
      COMMON /QUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
     +,              IQDLUN,IQFLUN,IQHLUN,IQCLUN,  NQUSED
      COMMON /ARRCOM/LUNPAM,NCHKD,NWKD,NCARDP,NAREOF,NSKIPR,KDHOLD(20)
     +,              NTRUNC,IPROMU,IPROMI
      COMMON /ARRVRQ/MAFIL,MAPAT,MADEC,MAREAL,MAFLAT,MASKIP,MADEL
     +,              MACHEK,MADRVS,MADRVI,MAPRE,  MSELF
      COMMON /CCPARA/NCHCCD,NCHCCT,KARDCC(84),   JCCTYP,JCCPRE,JCCEND
     +,              MCCPAR(120),NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
      COMMON /CCTYPE/MCCQUI,MCCPAM,MCCTIT,MCCPAT,MCCDEC,MCCDEF,MCCEOD
     +,              MCCASM,MCCOPT,MCCUSE
      COMMON /CONST/ MPAK2(2),MPAK5(2),MPAK9(2),MPAK15(2),DAYTIM(3)
     +,              NWNAME,NWSENM,NWSEN1,LARGE
      COMMON /IOFCOM/IOTALL,IOTOFF,IOTON,IOSPEC,IOPARF(5),IOMODE(12)
      PARAMETER      (KDNWT=20, KDNWT1=19,KDNCHW=4, KDBITS=8)
      PARAMETER      (KDPOST=25,KDBLIN=32,KDMARK=0, KDSUB=63,JPOSIG=1)
      COMMON /KDPKCM/KDBLAN,KDEOD(2)
      PARAMETER      (IQBDRO=25, IQBMAR=26, IQBCRI=27, IQBSYS=31)
      COMMON /QBITS/ IQDROP,IQMARK,IQCRIT,IQZIM,IQZIP,IQSYS
                         DIMENSION    IQUEST(30)
                         DIMENSION                 LQ(99), IQ(99), Q(99)
                         EQUIVALENCE (QUEST,IQUEST),    (LQUSER,LQ,IQ,Q)
      COMMON //      QUEST(30),LQUSER(7),LQMAIN,LQSYS(24),LQPRIV(7)
     +,              LQ1,LQ2,LQ3,LQ4,LQ5,LQ6,LQ7,LQSV,LQAN,LQDW,LQUP
     +, KADRV(9), LEXD,LEXH,LEXP,LPAM,LDECO, LADRV(14)
     +, NVOPER(6),MOPTIO(31),JANSW,JCARD,NDECKR,NVUSEB(14),MEXDEC(6)
     +, NVINC(6),NVUTY(16),NVIMAT(6),NVACT(6),NVGARB(6),NVWARN(6)
     +, JASK,JCWAIT,JCWDEL,LARMAT,LAREND,NCARR
     +, NVARR(10), IDARRV(8),JPROPD,MODPAM, NVARRI(9),LARX,LARXE,LINBUF
     +, NVCCP(7),JARDO,JARWT,JARLEV
     +, NVDEP(14),LDPMAT,JDPMAT,LDPACT,JDPACT,NDPLEV,MDEPAR, NVDEPL(6)
     +, MWK(80),MWKX(80)
      EQUIVALENCE(LSASM,LADRV(8)),  (LRBIG,LADRV(13)), (LRPAM,LADRV(14))
C--------------    END CDE                             -----------------  ------
      DIMENSION    JCV(3)
      EQUIVALENCE (JCV(1),JCCPP)

C---------------   PACKED PAM-FILE             -------------------------

   90 IF (JASK)               91,161,151
   91 MUST = MAREAL - JASK
      IF (MUST)              130,121,92
   92 IF (MUST.NE.4)         GO TO 111

C--                JASK = MAFILE-1, READ FIRST TITLE RECORD, PRINT TITLE

      IF (MODPAM.NE.0)       GO TO 407
      IF (JPROPD.NE.0)       GO TO 122
      JARDO = 7
      CALL ARBIN
      IF (NVARRI(1).NE.3)    GO TO 108
      NDECKR = NDECKR - 1

      LDPMAT   = IABS(IQ(LARX))
      IQ(LPAM+12) = 80
      CALL UCOPY (IQ(LDPMAT),IQ(LPAM+13),KDNWT)
      NVDEP(11)= IABS(IQ(LARX+2)) - LDPMAT
      NVDEP(12)= IQ(LARX+1) - 1
      NVDEP(13)= 1
      MDEPAR = 5
      CALL DEPART
      GO TO 173

C--                EOI  OR  POSITIONING FAULT

  108 WRITE (IQPRNT,9108)
      CALL PABEND


C-------           JASK = MAFIL,MAPAT,MADEC  GET NEXT FILE/PATCH/DECK

  111 IF (MODPAM.NE.0)       GO TO 411
      JARDO = -7
  112 IF (NVARRI(3).GE.MUST) GO TO 173
      CALL ARBIN
      GO TO 112

C--------          JASK= MAREAL   DECODE CARD  +PATCH  +DECK

  121 CONTINUE
C     IF (JPROPD.GE.0)       CALL QFATAL                                DEBUG
      JARDO = 7
      CALL ARBIN

C--                DECODE PATCH/DECK HEADER CARD

  122 JPROPD = NVARRI(1)
      L      = IQ(LARX)
      JCCTYP = MCCDEF - JPROPD
      IF (NVOPER(3).NE.0)    GO TO 125
      IF (IQ(LARX+1).NE.1)   GO TO 123
      LARX = LARX + 2
      NWKD = -IQ(LARX) - L
      GO TO 124

  123 NWKD    = KDSTEP (IQ(L),1)
      IQ(LARX)= L + NWKD
  124 NCHKD   = KDNCHW*NWKD - 1
      JCARD = 1
      CALL CCKRAK (IQ(L))
      IF (NCCPIF.NE.0)  CALL CCPROC
      RETURN

C--                UPDATE MODE

  125 NWKD  = KDSTEP (IQ(L),1)
      NCHKD = KDNCHW*NWKD - 1
      CALL CCKRAK (IQ(L))
      RETURN


C-------           JASK = MAPRE   PRE-READ NEXT CARD

  127 IF (MODPAM.NE.0)       GO TO 471
      IF (LARX.LT.LARXE)     GO TO 128
      IF (NVARRI(3).NE.0)    GO TO 171
      JARDO = 7
      CALL ARBIN
  128 JCCPRE = 7
      GO TO 181

C-------           JASK = MAFLAT, MASKIP, MADEL, MACHEK   IGNORE CARDS

  130 JARLEV= -JASK
      IF (JASK+2)            133,132,131

C--                -1  JASK = MACHECK
  131 JARWT = JCWAIT
      GO TO 135

C--                -2  JASK = MADEL
  132 JARWT = JCWDEL
      GO TO 135

C--                -3  JASK = MASKIP
C--                -4  JASK = MAFLAT

  133 JARWT = LARGE
      JARLEV= JARLEV - 1

  135 IF (MODPAM.NE.0)       GO TO 431
      JCCPRE= 0


C----              POSITION TO CARD 'JARWT' OR NEXT C/C OF WANTED LEVEL

  136 IF (JCARD.EQ.JARWT)    GO TO 182
      IF (LARX.LT.LARXE)     GO TO 141
  137 IF (NVARRI(3).NE.0)    GO TO 171
      IF (NVARRI(4).GE.JARLEV)     GO TO 139
      IF (JCARD+520 .GE.JARWT)     GO TO 139

C--                SKIP NEXT RECORD, UP-DATE  JCARD

      JARDO = 0
      CALL ARBIN
      JCARD = IQ(LARXE)
      GO TO 137

C--                READ NEXT RECORD

  139 JARDO = 7
      CALL ARBIN

C--                CHECK CARD 'JCARD' IS C/C

  141 L = IQ(LARX)
      IF (L.GE.0)            GO TO 142
      L = -L
      JCCTYP= JARTYP (IQ(L))
      IF  (JCCTYP.GE.0)      GO TO 186
      IF (-JCCTYP.GE.JARLEV) GO TO 186

C--                CHECK CARD 'JARWT' IS WITHIN CURRENT GROUP

  142 IF (JARWT.LT.IQ(LARX+1))     GO TO 143
      JCARD = IQ(LARX+1)
      LARX  = LARX + 2
      GO TO 136

C--                POSITION TO CARD  JARWT

  143 IQ(LARX) = L + KDSTEP (IQ(L),JARWT-JCARD)
      JCARD = JARWT
      GO TO 182


C-------           JASK = MADRVI   DRIVE INDIRECT (FOREIGN) MATERIAL

  151 IF (JASK.NE.1)         GO TO 127
      NCARR = 0
      IF (MODPAM.NE.0)       GO TO 451
      JCCPRE= 0
C     IF (JCARD.EQ.JCWAIT)   CALL QFATAL                                DEBUG
      IF (LARX.GE.LARXE)     GO TO 160
  152 NVDEP(13)= JCARD
      LDPMAT   = IQ(LARX)
      IF (LDPMAT.LT.0)       GO TO 184
      LCARD= IQ(LARX+1)
      IF (JCWAIT.GE.LCARD)   GO TO 155
      NVDEP(12)= JCWAIT - JCARD
      NVDEP(11)= KDSTEP (IQ(LDPMAT),NVDEP(12))
      IQ(LARX) = LDPMAT + NVDEP(11)
      JCARD    = JCWAIT
      GO TO 158

  155 LARX  = LARX + 2
      JCARD = LCARD
      NVDEP(11)= -IQ(LARX) - LDPMAT
      NVDEP(12)= LCARD - NVDEP(13)

C--                DRIVE INDIRECT MATERIAL TO MEMORY, AND POSSIBLY LIST

  158 CALL UCOPY (IQ(LDPMAT),IQ(LARMAT),NVDEP(11))
      NCARR = NCARR  + NVDEP(12)
      LARMAT= LARMAT + NVDEP(11)
      IF (NVUSEB(9).EQ.0)    GO TO 159
      MDEPAR = -2
      CALL DEPART
  159 IF (JCARD.EQ.JCWAIT)   GO TO 182
      IF (LARX.GE.LARXE)     GO TO 160
      L = -IQ(LARX)
      GO TO 185

C--                LOAD NEXT RECORD

  160 IF (NVARRI(3).NE.0)    GO TO 171
      JARDO = 7
      CALL ARBIN
      GO TO 152


C-------           JASK = MADRVS   DRIVE SELF MATERIAL TO DEPART

  161 NVDEP(14)= 0
      IF (MODPAM.NE.0)       GO TO 461
      JCCPRE= 0
C     IF (JCARD.EQ.JCWAIT)   CALL QFATAL                                DEBUG
      IF (LARX.GE.LARXE)     GO TO 170
  162 NVDEP(13)= JCARD
      LDPMAT   = IQ(LARX)
      IF (LDPMAT.LT.0)       GO TO 184
      LCARD= IQ(LARX+1)
      IF (JCWAIT.GE.LCARD)   GO TO 165
      NVDEP(12)= JCWAIT - JCARD
      NVDEP(11)= KDSTEP (IQ(LDPMAT),NVDEP(12))
      IQ(LARX) = LDPMAT + NVDEP(11)
      JCARD    = JCWAIT
      GO TO 168

  165 LARX  = LARX + 2
      JCARD = LCARD
      NVDEP(11)= -IQ(LARX) - LDPMAT
      NVDEP(12)= LCARD - NVDEP(13)

C--                DRIVE SELF-MATERIAL TO DEPART

  168 MDEPAR = 0
      CALL DEPART
      IF (JCARD.EQ.JCWAIT)   GO TO 182
      IF (LARX.GE.LARXE)     GO TO 169
      L = -IQ(LARX)
      GO TO 185

C--                LOAD NEXT RECORD

  169 NVDEP(14)= 7
  170 IF (NVARRI(3).NE.0)    GO TO 171
      IF (JASK.NE.0)         GO TO 90
      JARDO = 7
      CALL ARBIN
      GO TO 162

C-----             END OF DECK

  171 JANSW = 7
      JCCTYP= MCCEOD
  173 JPROPD   =-NVARRI(3)
      IDARRV(7)= NVARRI(5)
      IDARRV(8)= NVARRI(6)                                              -A8M
      NVARRI(3)= 0
      LARX = LARXE
      RETURN


C----------        END OF PROCESSING, DECODE NEXT C-C

  181 L = -IQ(LARX)
      IF (L.GE.0)            GO TO 185
  182 JCCTYP= 0
      RETURN

  184 L = -LDPMAT
  185 JCCTYP= JARTYP (IQ(L))
  186 JCARD = JCARD + 1
      LARXSV= LARX
      IF (JCARD.NE.IQ(LARX+1))   GO TO 188
      LARX = LARX + 2
      NWKD = -IQ(LARX) - L
      GO TO 189

  188 NWKD = KDSTEP (IQ(L),1)
      IQ(LARX)= L + NWKD
  189 NCHKD= KDNCHW*NWKD - 1
      CALL CCKRAK (IQ(L))
      IF (JCCBAD.NE.0)       GO TO 196
      IF (NCCPIF.NE.0)  CALL CCPROC
      RETURN

C--                HANDLE FAULTY C/C

  196 MDEPAR = 2
      CALL DEPART
      LARX = LARXSV
      IQ(LARX) = L
      JCARD= JCARD - 1
      IF (JASK.EQ.0)         GO TO 162
      IF (JASK.EQ.1)         GO TO 152
      IF (JASK.GE.2)         GO TO 182
      IF (JASK.GE.-4)        GO TO 136
      RETURN


C---------------   PAM-FILE ON CARDS           -------------------------

C--                JASK = MAFILE-1, PRINT TITLE

  407 IF (NVDEP(12).EQ.0)    GO TO 481
      IQ(LPAM+12) = NCHKD
      CALL UCOPY (IQ(LDPMAT),IQ(LPAM+13),NWKD)
      NVDEP(11)= NWKD
      NVDEP(12)= 1
      MDEPAR = 5
      CALL DEPART
      RETURN

C----              JASK = MAFIL,MAPAT,MADEC  GET NEXT FILE/PATCH/DECK
C--                ACCEPTED  +PAM, +QUIT ON THE CRADLE ALSO TERMINATE

  411 JARWT = -7
      JARDO = -2

  412 JPROPD= 0
      CALL ARBCD
      IF (JPROPD.GE.MUST)    RETURN
      JPROPD = MCCDEF - JCCTYP
      IF (JPROPD.LT.MUST)    GO TO 412
      IF (JCCTYP.GE.MCCTIT)  GO TO 482
      IF (NVARR(6).EQ.0)     GO TO 412
      IF (JCCBAD.NE.0)       GO TO 496
      IF (NCCPIF.EQ.0)       GO TO 487
      CALL CCPROC
      IF (JCCIFV.EQ.0)       GO TO 487
      GO TO 412

C----              JASK = MAFLAT, MADEL, MASKIP, MACHECK   IGNORE CARDS

  431 JARDO = -1
      CALL ARBCD
      GO TO 481


C----              JASK = MADRVI   DRIVE INDIRECT (FOREIGN) MATERIAL

  451 LAREND= LQTOL
      JARWT = JCWAIT
      JARDO = 0
  453 CALL ARBCD
      NCARR = NCARR + NVDEP(12)
      IF (NVUSEB(9).EQ.0)    GO TO 456
      IF (NVDEP(12).EQ.0)    GO TO 456
      MDEPAR = -2
      CALL DEPART
  456 IF (JCCTYP.NE.0)       GO TO 482
      IF (JCARD.EQ.JARWT)    RETURN
      CALL NOMEM

C----              JASK = MADRVS   DRIVE SELF MATERIAL TO DEPART

  461 NVDEP(14)= 0
      JARWT  = JCWAIT
      JARDO  = 0
  463 LARMAT = LINBUF
      LAREND = LINBUF + IQ(LINBUF-2)
      IF (MEXDEC(5).NE.0)    GO TO 466
  464 CALL ARBCD

      IF (NVDEP(12).EQ.0)    GO TO 481
      MDEPAR= 0
      CALL DEPART
      IF (JCCTYP.NE.0)       GO TO 482
      IF (JCARD.EQ.JARWT)    RETURN
      IF (JASK.NE.0)         GO TO 90
      NVDEP(14)= 7
      GO TO 463

  466 LAREND = LINBUF + 24
      GO TO 464

C----              JASK = MAPRE   PRE-READ NEXT CARD

  471 JARDO = 1
      CALL ARBCD

C-------------     CHECK END-OF-DECK  AND RETURN

  481 IF (JCCTYP.EQ.0)       RETURN
  482 IF (JCCBAD.NE.0)       GO TO 496
      IF (NCCPIF.NE.0)  CALL CCPROC
      IF (JCCTYP-MCCEOD)     485,487,483
  483 RETURN


C----              DECK TERMINATION BY  +PATCH, +DECK

  485 IF (JCCTYP-MCCTIT)     491,488,486
  486 JPROPD = MCCDEF - JCCTYP
      J = JCV(3-JPROPD)
      IDARRV(7)= MCCPAR(J+1)
      IDARRV(8)= MCCPAR(J+2)                                            -A8M
  487 JANSW = 7
      RETURN

C--                +TITLE,  IGNORE IF VERY FIRST CARD ON FILE

  488 IF (NCARDP.EQ.1)       GO TO 489
      JPROPD = 3
      JANSW  = 7
      RETURN

  489 NCARDP = 0
      JCARD  = 0
      GO TO 90

C--                +PAM, +QUIT IN THE CRADLE

  491 IF (NVARR(6).EQ.0)     RETURN
      IF (JCCIFV.NE.0)       RETURN
      MDEPAR = 4
      CALL DEPART
      JPROPD = 3
      JANSW  = 7
      RETURN

C------            FAULTY C/C

  496 MDEPAR = 2
      CALL DEPART
      IF (JASK.LT.0)         GO TO 498
      JCARD  = JCARD - 1
      JCCPRE = 7
      JCCTYP = 0
      IF (JASK.EQ.2)         RETURN
      CALL UCOPY (IQ(LARMAT),KDHOLD(1),NWKD)
      IF (JASK.EQ.0)         GO TO 463
      GO TO 453

  498 IF (JASK.GE.-4)        GO TO 431
      IF (JASK.GE.MAPAT)     GO TO 412
      RETURN

 9108 FORMAT ('0***  PAM IS NOT PACKED BINARY OR IS BADLY POSITIONED.')
      END