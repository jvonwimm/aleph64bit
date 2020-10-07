      SUBROUTINE LCBKPR
C--------------------------------------------------------------
C$ Print LCal banks
C. - J.D.Hansen & P.Hansen - 860604
C.                           modified by F.Ranjard - 890317
C. - Prints banks 'LWHT' 'LWDI' 'LTHT' 'LTDI' 'LTTR'
C. -              'LWTR'  and 'LTTD'
C. - Called by LCDIGI                            from this .HLB
C ------------------------------------------------
      SAVE
      CHARACTER*4 NAME
      DIMENSION JESUM(4)
      DIMENSION IXMIN(15),IXMAX(15)
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
C         parameters in the LCal code
        PARAMETER (LWDIR = 4, LTTRR = 24, LTTRC = 3)
        PARAMETER (LWTRR = 4, LWTRC = 2 , LTDIC = 4)
      COMMON /LCNAMC/ NALTHT, NALTDI, NALWHT, NALWDI, NALTTR, NALWTR,
     *                NALWHI, NALSHI, NALCWS, NALCAL, NALLAY, NALMTY,
     *                NALALI, NALSCO, NALSLO, NALWRG, NALCEL, NALCSH,
     *                NALDRE, NALCCA, NALCPG, NALSHO
      COMMON /LCCOMC/ ADCOLC,    COHNLC,    DPR1LC,    DPR2LC,
     *                DPR3LC,    DPR4LC,    DPR5LC,    ECRTLC,
     *                ECUTLC,    EELALC,    GVARLC,    LCADCO,
     *                LCBHTR,    LCHBOK,    LCNLAY(3), LCNWPL,
     *                LCMATE(2), LCPRNT,    LCSTRH(3), CHTOE(3),
     *                PAR1LC,    PAR2LC,    PAR3LC,    PAR4LC,
     *                PAR5LC,    PAR6LC,    PAR7LC,    PAR8LC,
     *                RADLLC(2), SNOILC(3), SCONLC,    SSAMLC,
     *                SSTPLC(3), TNOILC(3), WNOILC(3),
     *                ZMATLC(2), ZREFLC(3), ZSTPLC(3), Z123LC(3),
     *                XYZOLC(3,2),DWIRLC
      COMMON /JDLLOC/   JDLIST
      DATA IXMIN/3,9*1,2,3,3*4/
      DATA IXMAX/8,10,11,12,2*13,14,3*15,5*16/
      DATA IETHR/500/
      DATA IFI /0/
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C         macro to decode pad-address
        LCMOD(IXXXX) = (IXXXX-1)/512 + 1
        LCROW(IXXXX) = MOD(IXXXX-1,512)/16 + 1
        LCCOL(IXXXX) = MOD(IXXXX-1,16) + 1
C
C -------------------------------------------------------------
      IF (IFI.EQ.0) THEN
         IFI = 1
         JDLIST = 0
      ENDIF
      WRITE(LOUTIO,4000)
C
C - Calculate total energy/module
C - (Printing suppressed if Etot(MODU) < IETHR (MeV))
      DO 5 I=1,4
         JESUM(I) = 0
    5 CONTINUE
      NAME = 'LWDI'
      KINDX = IW(NALWDI)
      IF(KINDX .LE. 0)                               GOTO 998
      DO 20 M = 1,LROWS(KINDX)
        MODU = ITABL(KINDX,M,1)
        DO 10 IPL= 2,LCOLS(KINDX)
   10   JESUM(MODU) = JESUM(MODU) + ITABL(KINDX,MODU,IPL)
   20 CONTINUE
C
C - Print bank LWHT
      IF (LCPRNT .LT. 2)                             GOTO 120
      NAME = 'LWHT'
      KINDX = IW(NALWHT)
      IF(KINDX .LE. 0)                               GOTO 998
      DO 115 M = 1,LROWS(KINDX)
        MODU = ITABL(KINDX,M,1)
        IF(JESUM(MODU) .LT. IETHR) GOTO 115
        KR = KROW(KINDX,M)
        WRITE (LOUTIO,4100) NAME
        WRITE (LOUTIO,5000) MODU
        LAY = 0
        DO 110 ISTOR = 1,3
          LAYMX = LCNLAY(ISTOR)
          KWLAY = KR + 1 + LAY
          WRITE (LOUTIO,5100) ISTOR,(IW(KWLAY+J),J=1,LAYMX)
          LAY    = LAY + LAYMX
  110   CONTINUE
  115 CONTINUE
C
C - Print bank LTHT
  120   IF (LCPRNT .LT. 2)                              GOTO 140
        NAME = 'LTHT'
        KINDX = IW(NALTHT)
        IF(KINDX .LE. 0)                                GOTO 998
        IF (LROWS(KINDX) .EQ. 0)                        GOTO 140
        WRITE (LOUTIO,4100) NAME
        WRITE (LOUTIO,5200)
        DO 130 J = 1,LROWS(KINDX)
          KR = KROW(KINDX,J)
          MODU=LCMOD(IW(KR+1))
          IF(JESUM(MODU) .LT. IETHR) GOTO 130
          IROW=LCROW(IW(KR+1))
          ICOL=LCCOL(IW(KR+1))
          WRITE(LOUTIO,5300) MODU,ICOL,IROW,(IW(KR+K),K=2,4)
  130   CONTINUE
C
C - Print bank LWDI
  140 IF (LCPRNT .LT. 1)                             GOTO 320
      NAME = 'LWDI'
      KINDX = IW(NALWDI)
      IF(KINDX .LE. 0)                               GOTO 998
      WRITE (LOUTIO,4100) NAME
      DO 300 M = 1,LROWS(KINDX)
        MODU = ITABL(KINDX,M,1)
        IF(JESUM(MODU) .LE. IETHR) GOTO 300
        KR = KROW(KINDX,M)
        WRITE (LOUTIO,5400) MODU
        LAY = 0
        DO 310 ISTOR = 1,3
          LAYMX = LCNLAY(ISTOR)
          KWLAY = KR + 1 + LAY
          WRITE (LOUTIO,5100) ISTOR,(IW(KWLAY+J)/10,J=1,LAYMX)
          LAY    = LAY + LAYMX
  310   CONTINUE
  300   CONTINUE
C
C - Print the LTDI bank
  320   IF (LCPRNT .LT. 1)                               GOTO 400
        NAME = 'LTDI'
        KINDX = IW(NALTDI)
        IF(KINDX .LE. 0)                                 GOTO 998
        IF (LROWS(KINDX) .EQ. 0)                         GOTO 400
        WRITE (LOUTIO,4100) NAME
        CALL WBANK(IW,JDLIST,2016,*370)
        DO 330 J = 1,LROWS(KINDX)
          KR = KROW(KINDX,J)
          IPAD = IW(KR+1)
          IW(JDLIST+IPAD) = (IW(KR+2)+IW(KR+3)+IW(KR+4))/10
  330   CONTINUE
        DO 360 MODU = 1,4
          IF(JESUM(MODU).LT.IETHR) GOTO 360
          WRITE(LOUTIO,5500) MODU
          WRITE(LOUTIO,5502)
           DO 350 IY = 1,30
            IROW = 31 - IY
            KADR = JDLIST + (MODU-1)*512 + (IROW-1)*16
            IYR = MIN0(IY,IROW)
            IXMN = IXMIN(IYR)
            IXMX = IXMAX(IYR)
            IF(IYR.EQ.1.OR.IYR.EQ.12) THEN
               WRITE(LOUTIO,5601) IROW,(IW(KADR+IC),IC=IXMN,IXMX)
            ELSEIF(IYR.GE.2.AND.IYR.LE.10) THEN
               WRITE(LOUTIO,5602) IROW,(IW(KADR+IC),IC=IXMN,IXMX)
            ELSEIF(IYR.EQ.11) THEN
               WRITE(LOUTIO,5603) IROW,(IW(KADR+IC),IC=IXMN,IXMX)
            ELSEIF(IYR.GE.13.AND.IYR.LE.15) THEN
               WRITE(LOUTIO,5604) IROW,(IW(KADR+IC),IC=IXMN,IXMX)
            ENDIF
  350     CONTINUE
  360   CONTINUE
        CALL WDROP(IW,JDLIST)
        GOTO 400
  370   WRITE (LOUTIO,*) '   not enough space to unpack this bank'
C
C - Print bank LTTR
  400 IF(LCPRNT .LT. 1)                              GOTO 600
      NAME = 'LTTR'
      KINDX = IW(NALTTR)
      IF(KINDX .LE. 0)                               GOTO 998
      WRITE (LOUTIO,4100) NAME
      WRITE (LOUTIO,5700)
      LC = LCOLS(KINDX)
      DO 530 ISEG = 1,LROWS(KINDX)
        KR = KROW(KINDX,ISEG)
        JSUM = 0
        DO 510 J = 1,LC
          JSUM = JSUM + IW(KR+J)/10
  510   CONTINUE
        IF(JSUM .LT. IETHR/10) GOTO 530
        IRING=(ISEG-1)/12 + 1
        WRITE (LOUTIO,5800) IRING,ISEG,(IW(KR+I)/10,I=1,LC),JSUM
  530 CONTINUE
      NAME = 'LWTR'
      KINDX = IW(NALWTR)
      IF(KINDX .LE. 0)                              GOTO 998
      WRITE (LOUTIO,4100) NAME
      DO 550 MODU = 1,LROWS(KINDX)
        WRITE (LOUTIO,5900) MODU
        KR = KROW(KINDX,MODU)
        WRITE (LOUTIO,6000) IW(KR+1)/10,IW(KR+2)/10
  550 CONTINUE
  600 CONTINUE
      RETURN
C
  998 WRITE(LOUTIO,7100) NAME
      RETURN
C
 4000 FORMAT(/,' +++ LCBKPR +++ final banks from LCalorimeter')
 4100 FORMAT(//,' Dump of bank  ',A4)
 5000 FORMAT(/,' Wire map of module',I2,'  unit = Ecritical')
 5100 FORMAT(1X,'Storey',I2,20I4)
 5200 FORMAT(' Tower hits ','  unit = Ecritical',/,
     *       3X,'MODULE',4X,'ICOL',4X,'IROW','       Hits')
 5300 FORMAT(6I8)
 5400 FORMAT(/,' Wire map of module',I2,'   unit = 10 MeV')
 5500 FORMAT(' Tower digitalisations (unit = 10MeV) in module',I4)
 5502 FORMAT(' COL',4X,'1    2    3    4    5    6    7    8    9',
     *       3X,'10   11   12   13   14   15   16',/,' ROW')
 5601 FORMAT(1X,I2,11X,14I5)
 5602 FORMAT(1X,I2,1X,16I5)
 5603 FORMAT(1X,I2,6X,15I5)
 5604 FORMAT(1X,I2,16X,13I5)
 5700 FORMAT(/,' Trigger tower sums -  unit = 10 MeV',/,
     *         ' Ring Segm   Stor1   Stor2   Stor3   Total')
 5800 FORMAT(2I5,4I8)
 5900 FORMAT(/,' Trigger wire sums in module',I2,' -   unit = 10 MeV')
 6000 FORMAT(' odd planes',I6,'   even planes',I6)
 6100 FORMAT(/,'  Towers --> primary track ',I3,' -  unit = 10 MeV',/,
     *       2X,'Modul Icol Irow   Stor1   Stor2   Stor3   Total')
 6200 FORMAT('  -- No signals in LCal due to this track')
 6300 FORMAT(2X,3I5,4I8)
 7100 FORMAT(' LCal bank ',A4,' does not exist')
C
      END
