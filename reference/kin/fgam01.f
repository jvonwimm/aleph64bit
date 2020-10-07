      SUBROUTINE askusi(icode)                                          ASKUSI 2
C-----------------------------------------------------------------------ASKUSI 3
C! Initialization routine                                               ASKUSI 4
C                                                                       ASKUSI 5
C-----------------------------------------------------------------------ASKUSI 6
C                                                                       ASKUSI 8
      PARAMETER(lpdec=48)                                               ASKUSI 9
      PARAMETER(igcod=1003)                                             ASKUSI10
      INTEGER nodec(lpdec)                                              ASKUSI11
      INTEGER altabl,namind,alrlep                                      ASKUSI12
      EXTERNAL altabl,namind,alrlep                                     ASKUSI13
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / miscl / loutbe,ecm,idb1,idb2,sdvrt(3),vrtx(4),           MISCL  2
     &                 nevent(10),tabl(9)                               MISCL  3
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  4
      REAL*4 ecm,sdvrt,vrtx,tabl                                        MISCL  5
C                                                                       MISCL  6
      REAL*8 x,gggg                                                     MYQUANT2
      REAL*8 ph,weight,wmax,wgh                                         MYQUANT3
      REAL*8 ebeam,xmin,tetmin,tetmax,grtmin,grtmax,delmin              MYQUANT4
      INTEGER itot,ibef,icut,igen,iana,iok                              MYQUANT5
      INTEGER iprint                                                    MYQUANT6
      COMMON / photon / ph(4,4),weight,wmax                             MYQUANT7
      COMMON / detect / ebeam,xmin,tetmin,tetmax,grtmin,grtmax,delmin   MYQUANT8
      COMMON / events / itot,ibef,icut,igen,iana,iok                    MYQUANT9
      COMMON / random / x(10)                                           MYQUAN10
      COMMON / print / iprint                                           MYQUAN11
C                                                                       ASKUSI17
      loutbe = IW(6)                                                    ASKUSI18
C...Set generator code                                                  ASKUSI19
      icode = igcod                                                     ASKUSI20
C                                                                       ASKUSI21
C Welcome !                                                             ASKUSI22
C                                                                       ASKUSI23
      WRITE(loutbe,1001) icode                                          ASKUSI24
 1001 FORMAT(//15X,'+---------------------------------------------+'/   ASKUSI25
     .      15X,'|                                             |'/      ASKUSI26
     .      15X,'|     W E L C O M E   T O    F G A M 0 1      |'/      ASKUSI27
     .      15X,'|     **********************************      |'/      ASKUSI28
     .      15X,'|     The  e+e- --> four gamma generator      |'/      ASKUSI29
     .      15X,'|     **********************************      |'/      ASKUSI30
     .      15X,'|             Code is # ',I6,'                |'/      ASKUSI31
     .      15X,'|                                             |'/      ASKUSI32
     .      15X,'|                                             |'/      ASKUSI33
     .      15x,'|     last modification : 6 April    1998     |'/      BBL001 1
     .      15X,'|     Patrick Janot  --  CERN/PPE             |'/      ASKUSI35
     .      15X,'+---------------------------------------------+'//)    ASKUSI36
C                                                                       ASKUSI37
C Read the generator parameters                                         ASKUSI38
C                                                                       ASKUSI39
      ngene = NAMIND('GENE')                                            ASKUSI40
      jgene = IW(ngene)                                                 ASKUSI41
      IF(jgene.NE.0) THEN                                               ASKUSI42
        ecm     =  RW(jgene+1)                                          ASKUSI43
        xmin    =  RW(jgene+2)                                          ASKUSI44
        tetmin  =  RW(jgene+3)                                          ASKUSI45
        grtmin  =  RW(jgene+4)                                          ASKUSI46
        delmin  =  RW(jgene+5)                                          ASKUSI47
        wmax    =  RW(jgene+6)                                          ASKUSI48
        iprint  =  IW(jgene+7)                                          ASKUSI49
      ELSE                                                              ASKUSI50
        ecm     =  130.                                                 ASKUSI51
        xmin    =  3D-3                                                 ASKUSI52
        tetmin  =  12D0                                                 ASKUSI53
        grtmin  =  12D0                                                 ASKUSI54
        delmin  =  0D00                                                 ASKUSI55
        wmax    =  0D0                                                  ASKUSI56
        iprint  =  0                                                    ASKUSI57
      ENDIF                                                             ASKUSI58
C                                                                       ASKUSI59
      ebeam  = ecm/2.                                                   ASKUSI60
      IF ( grtmin .LT. tetmin ) grtmin = tetmin                         ASKUSI61
      tetmax = 180.d0 - tetmin                                          ASKUSI62
      grtmax = 180.d0 - grtmin                                          ASKUSI63
C                                                                       ASKUSI64
      tabl(1) = ecm                                                     ASKUSI65
      tabl(2) = xmin                                                    ASKUSI66
      tabl(3) = tetmin                                                  ASKUSI67
      tabl(4) = grtmin                                                  ASKUSI68
      tabl(5) = delmin                                                  ASKUSI69
      tabl(6) = wmax                                                    ASKUSI70
C                                                                       ASKUSI71
C Do not use VEGAS within KINGAL !                                      ASKUSI72
C                                                                       ASKUSI73
      iveg   = 0                                                        ASKUSI74
      itst   = 0                                                        ASKUSI75
      iitt   = 10                                                       ASKUSI76
      ipoint = 40000                                                    ASKUSI77
C                                                                       ASKUSI78
C FGMA01 Initialization                                                 ASKUSI79
C                                                                       ASKUSI80
      CALL begin(iveg,itst)                                             ASKUSI81
      CALL hbook1(10000,'Log(weight)',50  ,-10.,5.,0.)                  ASKUSI82
C                                                                       ASKUSI83
C...Vertex smearing                                                     ASKUSI84
      sdvrt(1) = 0.035                                                  ASKUSI85
      sdvrt(2) = 0.0012                                                 ASKUSI86
      sdvrt(3) = 1.28                                                   ASKUSI87
      jsvrt = NLINK('SVRT',0)                                           ASKUSI88
      IF ( jsvrt .NE. 0 ) THEN                                          ASKUSI89
        sdvrt(1) = RW(jsvrt+1)                                          ASKUSI90
        sdvrt(2) = RW(jsvrt+2)                                          ASKUSI91
        sdvrt(3) = RW(jsvrt+3)                                          ASKUSI92
      ENDIF                                                             ASKUSI93
      tabl(7) = sdvrt(1)                                                ASKUSI94
      tabl(8) = sdvrt(2)                                                ASKUSI95
      tabl(9) = sdvrt(3)                                                ASKUSI96
C                                                                       ASKUSI97
C  Fill the KPAR bank with the generator parameters                     ASKUSI98
C                                                                       ASKUSI99
      jkpar = altabl('KPAR',9,1,tabl,'2I,(F)','C')                      ASKUS100
C  Fill RLEP bank                                                       ASKUS101
      iebeam = NINT(ecm*500)                                            ASKUS102
      jrlep = alrlep(iebeam,'    ',0,0,0)                               ASKUS103
C...Debug flags                                                         ASKUS104
      jdebu = IW(NAMIND('DEBU'))                                        ASKUS105
      IF ( jdebu .GT. 0 ) THEN                                          ASKUS106
        idb1 = IW(jdebu+1)                                              ASKUS107
        idb2 = IW(jdebu+2)                                              ASKUS108
      ENDIF                                                             ASKUS109
C                                                                       ASKUS110
C  Initialize events counters                                           ASKUS111
C                                                                       ASKUS112
       DO 11 i = 1,10                                                   ASKUS113
   11  nevent(i) = 0                                                    ASKUS114
C                                                                       ASKUS115
      CALL PRTABL('KPAR',0)                                             ASKUS116
      CALL PRTABL('RLEP',0)                                             ASKUS117
C                                                                       ASKUS118
      RETURN                                                            ASKUS119
      END                                                               ASKUS120
      SUBROUTINE ASKUSE(IDPR,ISTA,NITR,NIVX,ECMS,WEIT)                  ASKUSE 2
C-----------------------------------------------------------------------ASKUSE 3
C! Event generation                                                     ASKUSE 4
C-----------------------------------------------------------------------ASKUSE 5
      DIMENSION ptrak(4,2), qg(4),itab(4)                               ASKUSE 6
      integer ALTABL                                                    ASKUSE 7
      EXTERNAL ALTABL                                                   ASKUSE 8
      INTEGER LMHLEN, LMHCOL, LMHROW                                    BCS    2
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)                          BCS    3
C                                                                       BCS    4
      COMMON / BCS / IW(1000)                                           BCS    5
      INTEGER IW                                                        BCS    6
      REAL RW(1000)                                                     BCS    7
      EQUIVALENCE (RW(1),IW(1))                                         BCS    8
C                                                                       BCS    9
      COMMON / miscl / loutbe,ecm,idb1,idb2,sdvrt(3),vrtx(4),           MISCL  2
     &                 nevent(10),tabl(9)                               MISCL  3
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  4
      REAL*4 ecm,sdvrt,vrtx,tabl                                        MISCL  5
C                                                                       MISCL  6
      REAL*8 x,gggg                                                     MYQUANT2
      REAL*8 ph,weight,wmax,wgh                                         MYQUANT3
      REAL*8 ebeam,xmin,tetmin,tetmax,grtmin,grtmax,delmin              MYQUANT4
      INTEGER itot,ibef,icut,igen,iana,iok                              MYQUANT5
      INTEGER iprint                                                    MYQUANT6
      COMMON / photon / ph(4,4),weight,wmax                             MYQUANT7
      COMMON / detect / ebeam,xmin,tetmin,tetmax,grtmin,grtmax,delmin   MYQUANT8
      COMMON / events / itot,ibef,icut,igen,iana,iok                    MYQUANT9
      COMMON / random / x(10)                                           MYQUAN10
      COMMON / print / iprint                                           MYQUAN11
C                                                                       ASKUSE12
C  Initialization ASKUSE's arguments                                    ASKUSE13
C                                                                       ASKUSE14
      ista   = 0                                                        ASKUSE15
      nitr   = 4                                                        ASKUSE16
      nivx   = 1                                                        ASKUSE17
      ecms   = ecm                                                      ASKUSE18
      weit   = 1.                                                       ASKUSE19
      sbeam  = ecm**2                                                   ASKUSE20
      idpr   = 0                                                        ASKUSE21
C                                                                       ASKUSE22
C  Store beam particles also in bos banks                               ASKUSE23
C                                                                       ASKUSE24
      ipart = 2                                                         ASKUSE25
      DO 10 itr = 1,2                                                   ASKUSE26
         DO 20 i=1,4                                                    ASKUSE27
   20    ptrak(i,itr) = 0.                                              ASKUSE28
         ptrak(3,itr) = 0.5*ecm                                         ASKUSE29
         IF ( itr .EQ. 2 ) THEN                                         ASKUSE30
           ptrak(3,itr) =- 0.5*ecm                                      ASKUSE31
         ENDIF                                                          ASKUSE32
         ist=kbkine(-itr,ptrak(1,itr),ipart,0)                          ASKUSE33
         ipart = ipart+1                                                ASKUSE34
         IF ( ist .LE. 0 ) THEN                                         ASKUSE35
            ista = -2                                                   ASKUSE36
            GO TO 998                                                   ASKUSE37
         ENDIF                                                          ASKUSE38
  10  CONTINUE                                                          ASKUSE39
C                                                                       ASKUSE40
C  Generate one event                                                   ASKUSE41
C                                                                       ASKUSE42
  100 CONTINUE                                                          ASKUSE43
      DO 1 IX=1,7                                                       ASKUSE44
    1 x(ix) = RNDM(ix)                                                  ASKUSE45
      weight = gggg(x)                                                  ASKUSE46
      IF ( iok .EQ. 0 ) GOTO 100  
C                                                                       ASKUSE48
C Go to unweighted events                                               ASKUSE49
C                                                                       ASKUSE50
      wgh    = weight                                                   ASKUSE51
      CALL wghone(weight,wgh,wmax)                                      ASKUSE52
C                                                                       ASKUSE53
      WEITR = WEIGHT                                                    ASKUSE54
      CALL HFILL(10000,ALOG10(weitr),0.,1.)                             ASKUSE55
CBB      IF ( wgh .LE. 0.D0 ) GOTO 100  apply after cross section calc 
C                                                                       ASKUSE57
C Update the cross-section                                              ASKUSE58
C                                                                       ASKUSE59
      CALL cross(weight,wgh,wmax)                                       ASKUSE60
      IF ( wgh .LE. 0.D0 ) GOTO 100 
      IF ( iok .EQ. 0) GOTO 100                                         ASKUSE61
      IF ( WMAX.GT.0.) then                                             ASKUSE62
         WEIT = 1.                                                      ASKUSE63
      ELSE                                                              ASKUSE64
         weit = weight                                                  ASKUSE65
      endif                                                             ASKUSE66
C                                                                       ASKUSE67
C Update the event counter                                              ASKUSE68
C                                                                       ASKUSE69
      nevent(1) = nevent(1) + 1                                         ASKUSE70
C                                                                       ASKUSE71
C  Smear vertex position                                                ASKUSE72
C                                                                       ASKUSE73
      CALL rannor (rx,ry)                                               ASKUSE74
      CALL rannor (rz,dum)                                              ASKUSE75
      vrtx(1) = rx*sdvrt(1)                                             ASKUSE76
      vrtx(2) = ry*sdvrt(2)                                             ASKUSE77
      vrtx(3) = rz*sdvrt(3)                                             ASKUSE78
      vrtx(4) = 0.                                                      ASKUSE79
      IVMAI = 1                                                         ASKUSE80
      JVERT = KBVERT(IVMAI,VRTX,0)                                      ASKUSE81
      IF(JVERT.EQ.0) THEN                                               ASKUSE82
         ISTA = -1                                                      ASKUSE83
         GO TO 998                                                      ASKUSE84
      endif                                                             ASKUSE85
C                                                                       ASKUSE86
C  Fill the four photons                                                ASKUSE87
C                                                                       ASKUSE88
      ipart = 1                                                         ASKUSE89
      DO iph = 1, 4                                                     ASKUSE90
        qg(4) = 0.                                                      BBL001 2
        DO ivec = 1, 3                                                  BBL001 3
          qg(ivec) = ph(ivec,iph)                                       ASKUSE92
        ENDDO                                                           ASKUSE93
C                                                                       ASKUSE94
C Store parton/particle in K and P vectors.                             ASKUSE95
C                                                                       ASKUSE96
        ist=kbkine(iph,qg,ipart,1)                                      ASKUSE97
        IF ( ist .LE. 0 ) THEN                                          ASKUSE98
           ista = -2                                                    ASKUSE99
           GO TO 998                                                    ASKUS100
        ENDIF                                                           ASKUS101
C                                                                       ASKUS102
      ENDDO                                                             ASKUS103
      DO 98 I = 1,4                                                     ASKUS104
 98   ITAB(I) = 0                                                       ASKUS105
      JKHIS = ALTABL('KHIS',1,NITR,ITAB,'I','E')                        ASKUS106
      IF(JKHIS.EQ.0) THEN                                               ASKUS107
        ISTA = 7                                                        ASKUS108
        go to 998                                                       ASKUS109
      ENDIF                                                             ASKUS110
      ist = 0                                                           ASKUS111
C                                                                       ASKUS112
C  Event counters                                                       ASKUS113
C                                                                       ASKUS114
  998 ista = ist                                                        ASKUS115
      IF ( ist .EQ. 0 ) nevent(2) = nevent(2) + 1                       ASKUS116
      IF ( ist .GT. 0) THEN                                             ASKUS117
        nevent(3) = nevent(3) + 1                                       ASKUS118
        nevent(4) = nevent(4) + 1                                       ASKUS119
        WRITE(6,*) 'Evt ',nevent(1),' ist = ',ist                       ASKUS120
      ELSEIF ( ist .LT. 0) THEN                                         ASKUS121
        nevent(3) = nevent(3) + 1                                       ASKUS122
        nevent(4-ist) = nevent(4-ist) + 1                               ASKUS123
      ENDIF                                                             ASKUS124
C                                                                       ASKUS125
      RETURN                                                            ASKUS126
      END                                                               ASKUS127
      SUBROUTINE USCJOB                                                 USCJOB 2
C-----------------------------------------------------------------------USCJOB 3
C! Routine for printout at the end of a run                             USCJOB 4
C                                                                       USCJOB 5
C-----------------------------------------------------------------------USCJOB 6
      COMMON / miscl / loutbe,ecm,idb1,idb2,sdvrt(3),vrtx(4),           MISCL  2
     &                 nevent(10),tabl(9)                               MISCL  3
      INTEGER loutbe,idb1,idb2,nevent                                   MISCL  4
      REAL*4 ecm,sdvrt,vrtx,tabl                                        MISCL  5
C                                                                       MISCL  6
      REAL DUMMY(4)                                                     USCJOB 8
C                                                                       USCJOB 9
      CALL FINISH(0)                                                    USCJOB10
C                                                                       USCJOB11
C Print event counters                                                  USCJOB12
C                                                                       USCJOB13
       WRITE(LOUTBE,101)                                                USCJOB14
  101  FORMAT(//20X,'EVENTS STATISTICS',                                USCJOB15
     &         /20X,'*****************')                                USCJOB16
       WRITE(LOUTBE,102) NEVENT(1),NEVENT(2),NEVENT(3)                  USCJOB17
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,  USCJOB18
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,  USCJOB19
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)  USCJOB20
       WRITE(LOUTBE,103)                                                USCJOB21
  103  FORMAT(//20X,'ERRORS STATISTICS',                                USCJOB22
     &         /20X,'*****************')                                USCJOB23
       WRITE(LOUTBE,104) (NEVENT(I),I=4,8)                              USCJOB24
  104  FORMAT(/10X,'IR= 1 no space for KHIS         # OF REJECT = ',I10,USCJOB25
     &        /10X,'IR= 2 no space for VERT         # OF REJECT = ',I10,USCJOB26
     &        /10X,'IR= 3 no space for KINE         # OF REJECT = ',I10,USCJOB27
     &        /10X,'IR= 4 free for user             # OF REJECT = ',I10,USCJOB28
     &        /10X,'IR= 5 free for user             # OF REJECT = ',I10)USCJOB29
C                                                                       USCJOB30
      RETURN                                                            USCJOB31
      END                                                               USCJOB32
