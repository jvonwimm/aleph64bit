      PROGRAM EPTODA                                                    EPTODA 2
C.-------------------------------------------------------------------   EPTODA 3
C  M.Rumpf  (RUMPF at FRCPN11)                                          EPTODA 4
C  Read Bos/Epio file on unit 20 written by EPIOFMT program.            EPDA0421
C  Write newdaf on unit 21                                              EPDA0422
C  reorder TADB and TPRO in alphabetic order                            EPDA0423
C                                                                       EPDA0424
C  the following data cards are mandatory                               EPDA0425
C    * open/read EPIO data base file                                    EPDA0426
C    FEPI   'DBASE:ADBS140.EPIO | EPIO'                      ! vax      EPDA0427
C           'ADBS140 EPIO | EPIO| GIME PUBXU 210 LREC 3600 ' ! ibm      EPDA0428
C           'ADBS140.EPIO | EPIO| FETCH PUBXU 210 LREC 3600' ! cray     EPDA0429
C    * open/write DAF file                                              EPDA0410
C    FDAF   'ADBS140.DAF | DAF'                              ! vax      EPDA0411
C           'ADBS140 DAF * | DAF'                            ! ibm      EPDA0412
C           'ADBS140.CRAYDAF | DAF | DISPOSE '               ! cray     EPDA0413
C    * create the DAF with so many blocks [and directory blocks]        CDAF0148
C    DAFB    370   7                                                    CDAF0149
C    ENDQ                                                               EPDA0416
C                                                                       EPDA0417
C  BOS77 and ALEPHLIB  libraries needed                                 EPDA0418
C.-------------------------------------------------------------------   EPDA0419
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C - NAMAX = number of ALEPH banks + number of data cards                DBS04047
C   do not forget to update NBNK in SBKPAR                              DBS04048
      PARAMETER (LBOS=700000,NAMAX=1320)                                FL080297
      COMMON /BCS/   IW(LBOS)                                           BCS    6
      INTEGER IW                                                        BCS    7
      REAL RW(1000)                                                     BCS    8
      EQUIVALENCE (RW(1),IW(1))                                         BCS    9
C                                                                       BCS   10
      PARAMETER(JTTANA=1,JTTARC=2,LTTABA=2)                             TTABJJ 2
C                                                                       EPDA0423
      CHARACTER*8 SUBR                                                  CDAF0150
      CHARACTER*4 NLIST,NAME,CHAINT                                     EPDA0424
      CHARACTER*40 TFILE,TFIL                                           EPDA0425
      CHARACTER*120 MSG,MSGE,MSGB                                       EPDA0426
      CHARACTER*6  TATYP                                                EPDA0427
      CHARACTER*68 TDEVI                                                EPDA0428
C                                                                       EPDA0429
C -----------------------------------------------------------------     EPDA0430
      SUBR  = ' EPTODA '                                                CDAF0151
      LUNEP = 20                                                        EPDA0431
      LUDAFN = 21                                                       EPDA0432
C                                                                       EPDA0433
C  Init BOS and read data cards                                         EPDA0434
C                                                                       EPDA0435
      IRECL = 1024                                                      EPDA0436
C                                                                       EPDA0437
      CALL BNAMES(NAMAX)                                                EPDA0438
      CALL BOS (IW,LBOS)                                                EPDA0439
      LUNPRT = IW(6)                                                    EPDA0440
      IW(7) = 100000                                                    EPDA0441
      IW(5) = 7                                                         EPDA0442
      CALL GETENVF ('EPTODACARDS',TFIL)                                 DBS10166
      IF (TFIL.NE.' ') CALL AOPEN(IW(5),TFIL,'CARD','DISK',IER)         DBS10167
      CALL BREADC                                                       EPDA0443
C                                                                       EPDA0444
C Open EPIO file onto unit LUNEP                                        EPDA0445
C It has been written by the EPIOFMT program with LREC=3600             EPDA0446
C                                                                       EPTODA46
       NDUM = NALREC(3600)                                              EPTODA47
       CALL AGTFIL ('FEPI','READ',LUNEP,IER)                            EPTODA48
       IF (IER.NE.0) THEN                                               EPTODA49
          CALL AWERRC (IW(6),'AGTFIL','  ',IER)                         EPTODA50
          STOP                                                          EPTODA51
       ENDIF                                                            EPTODA52
C                                                                       EPTODA53
C Get DAF file name (becareful DIRECT logical becomes TRUE in AOPEN)    EPTODA54
C                                                                       EPTODA55
      CALL ACDARG ('FDAF','DAF','*',TFILE,TATYP,TDEVI,IRETC)            EPTODA56
      IF (IRETC.NE.0) THEN                                              EPTODA57
         CALL AWERRC (IW(6),'ACDARG',TFILE,IRETC)                       EPTODA58
         STOP                                                           EPTODA59
      ENDIF                                                             EPTODA60
      LE = LNBLNK(TFILE)                                                EPDA0447
C                                                                       EPTODA62
C decode DAFB or OUTB data card to get number of records of the DAF     CDAF0153
C! decode DAFB or OUTB data card to get NBREC, NBDIR and NREC           GTNREC 2
      JDAFB = NLINK ('DAFB',0)                                          GTNREC 3
      IF (JDAFB.EQ.0) THEN                                              GTNREC 4
         JDAFB = NLINK ('OUTB',0)                                       GTNREC 5
         IF (JDAFB.EQ.0) THEN                                           GTNREC 6
            WRITE(IW(6),*) SUBR, ' : no DAFB/OUTB card - STOP'          GTNREC 7
            STOP                                                        GTNREC 8
         ENDIF                                                          GTNREC 9
      ENDIF                                                             GTNREC10
      NBREC = IW(JDAFB+1)                                               GTNREC11
      NBDIR = 0                                                         GTNREC12
      IF (IW(JDAFB).GT.1) NBDIR = IW(JDAFB+2)                           GTNREC13
      NREC = NBREC + NBDIR*10000                                        GTNREC14
C                                                                       CDAF0155
C  Open new DAF                                                         CDAF0156
C                                                                       CDAF0157
      IF (LUDAFN .NE. 0) THEN                                           CDAF0158
         LUN = LUDAFN                                                   CDAF0159
         TFIL = TFILE(1:LE)                                             EPDA0448
C     open the DAF in write mode                                        OWDAF 25
       OPEN(LUN,FILE=TFIL,STATUS='NEW',ACCESS='DIRECT',                 OWDAF 26
     &      ORGANIZATION='RELATIVE',RECL=IRECL,FORM='UNFORMATTED',      OWDAF 27
     &      ERR=90)                                                     OWDAF 28
       IW(1) = 2                                                        OWDAF 29
       CALL INITDU (LUN,IRECL,NREC,'ADAM')                              OWDAF 30
       IW(1) = 0                                                        OWDAF 31
       CALL BDABF (LUN,IRECL,TFIL,'ADAM')                               OWDAF 32
      ENDIF                                                             EP050325
C                                                                       EPTOD106
C Read BOS/EP file in IW array (old DAF in EPIO format) and write       EPDA0449
C the bank onto the new DAF                                             EPDA0450
C                                                                       EPDA0451
      NBKW  = 0                                                         EPDA0452
      NWRDS = 0                                                         EPDA0453
   10 CONTINUE                                                          EPDA0454
         CALL BEPRD(IW,LUNEP,'E',*101,*102)                             EPDA0455
         INDFIR = IW(3)                                                 EPDA0456
         NAME = CHAINT(IW(INDFIR-3))                                    EPDA0457
         NR   = IW(INDFIR-2)                                            EPDA0458
C                                                                       EPDA0459
C     sort TADB and TPRO in alphabetic order                            EPDA0460
         IF (NAME.EQ.'TADB' .OR. NAME.EQ.'TPRO') THEN                   EPDA0461
            LC = IW(INDFIR+LMHCOL)                                      EPDA0462
            LR = IW(INDFIR+LMHROW)                                      EPDA0463
            CALL SORTIQ (IW(INDFIR+LMHLEN+1),LC,LR,JTTANA)              EPDA0464
         ENDIF                                                          EPDA0465
C                                                                       EPDA0466
C     copy the bank from EPIO to DAF                                    EPDA0467
         CALL BDAWR (IW,LUDAFN,NAME,NR)                                 EPDA0468
         NWRDS = NWRDS + IW(INDFIR)                                     EPDA0469
         NBKW = NBKW + 1                                                EPDA0470
C                                                                       EPDA0471
      GO TO 10                                                          EPDA0472
C                                                                       EPDA0473
C  End of data set reached                                              EPDA0474
C                                                                       EPDA0475
  102 WRITE(LUNPRT,1102)                                                EPDA0476
C                                                                       EPTOD174
      WRITE(LUNPRT,1103) NBKW , NWRDS                                   EPTOD175
      CALL BOSFM                                                        EPTOD176
C                                                                       EPTOD177
      CALL BDAPR(LUDAFN,'    ')                                         EPTOD178
      CALL BOSTA                                                        EPTOD179
      CALL ACLOSE (0,IER)                                               FLR03282
C                                                                       EPTOD181
      STOP                                                              EPTOD182
  101 WRITE(LUNPRT,1101)                                                EPTOD183
      STOP                                                              EPTOD184
C                                                                       EPTOD185
 90   WRITE (LUNPRT,1090) IOP                                           EPTOD186
      STOP                                                              EPTOD187
C                                                                       EPTOD188
 1090 FORMAT(1X,' error when opening the DAF  - IOSTAT= ',I10)          EPTOD189
 1100 FORMAT(1X,' Bank ',A4,4I9,' ADDED on DAF from EPIO')              EPTOD190
 1101 FORMAT(1X,'Error reading EPIO file')                              EPTOD191
 1102 FORMAT(1X,'End of data set on EPIO file')                         EPTOD192
 1103 FORMAT(1X,I8,' banks written on DAF with a total of',I7,' words') EPTOD193
 1105 FORMAT(1X,' Bank ',A4,4I9,' ADDED on DAF from data cards')        EPTOD195
 1106 FORMAT (1X, ' Bank ',A4,4I9,4X,'READ from EPIO      ')            EPTOD196
      END                                                               EPTOD197
      SUBROUTINE TIMEL(T)                                               EPTOD198
C.--------------------------------------------------------------------  EPTOD199
C Dummy routine                                                         EPTOD200
C Necessary for DARES from BOS to run interactively                     EPTOD201
C.--------------------------------------------------------------------  EPTOD202
      T = 10.                                                           EPTOD203
      RETURN                                                            EPTOD204
      END                                                               EPTOD205
      SUBROUTINE DAFRDS(IUN,IREC,ARR,NDIM,*)                            DAFRDS 2
      INTEGER ARR(NDIM)                                                 DAFRDS 3
      READ(UNIT=IUN,REC=IREC,ERR=200) ARR                               DAFRDS 4
      RETURN                                                            DAFRDS 5
  200 RETURN 1                                                          DAFRDS 6
      END                                                               DAFRDS 7
