      SUBROUTINE LCROC
C--------------------------------------------------------------
C! Simulate Read Out Controller
C. - J.Dines Hansen & P.Hansen - 860604
C. - Zero suppression and range saturation
C. - Modify banks 'LTDI'
C. - Called by  LCDIGI                           from this .HLB
C -----------------------------------------------
      SAVE
      LOGICAL ACCPT
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
C -------------------------------------------------------------
C - Process bank LTDI
        KINDX = IW(NALTDI)
        NOLD  = LROWS(KINDX)
        IF (KINDX .LE. 0 .OR. NOLD .LE. 0)             GOTO 999
        NEW = 0
C - Loop over stored digitalizations
        DO 110 IDIG = 1,NOLD
          KADR = KROW(KINDX,IDIG)
          ACCPT = .FALSE.
C         Loop over storeys
          DO 120 ISTOR = 1,3
C
C           Zero suppression in ADC
            IF (IW(KADR+1+ISTOR) .GE. LCSTRH(ISTOR))
     &         ACCPT = .TRUE.
C
C           Saturate signal at 12+3 bits
            MAXCO = NINT(ADCOLC*32767)
            IW(KADR+1+ISTOR) = MIN0(MAXCO,IW(KADR+1+ISTOR))
  120     CONTINUE
          IF (ACCPT) THEN
            NEW = NEW + 1
            KADRA  = KROW(KINDX,NEW)
            DO 130 ICOL = 1,LCOLS(KINDX)
  130       IW(KADRA+ICOL) = IW(KADR+ICOL)
          ENDIF
  110   CONTINUE
        IW(KINDX+2) = NEW
C
  999 RETURN
      END
