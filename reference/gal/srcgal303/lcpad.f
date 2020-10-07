      SUBROUTINE LCPAD(NAMI,ISTOR,IPAD,NHIT)
C--------------------------------------------------------------
C! Store energies in towers
C. - J.Dines Hansen & P.Hansen - 860417
C. - Store energy deposition on pads in banks LTHT or LTDI
C. - Input : NAMI   = name-index of bank
C.           ISTOR  = storey number
C.           IPAD   = tower address
C.           NHIT   = number of hits to be stored
C. - Called by  LCSHOW, LCTRAK                   from this .HLB
C -----------------------------------------------
      SAVE
      CHARACTER*4 CHAINT, NAME
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
C
      KINDX = IW(NAMI)
      IF(KINDX .LE. 0)                        GOTO 999
      NROW = LROWS(KINDX)
      NCOL = LCOLS(KINDX)
C
C - Loop over towers starting from bottom of table
      KLL = KROW(KINDX,NROW)
      DO 200 I = 1,NROW
        IF (IPAD .EQ. IW(KLL+1))              GOTO 300
        KLL = KLL - NCOL
  200 CONTINUE
C
C - If address IPAD was not found then expand the table
  100 NROW = NROW + 1
      IF(LFRWRD(KINDX) .LE. NCOL) THEN
         LEN = (NROW+20)*NCOL + LMHLEN
         NAME = CHAINT (IW(KINDX-3))
         CALL ALBOS (NAME,0,LEN,KINDX,IGARB)
      ENDIF
      IW(KINDX+2) = NROW
      KLL = KROW(KINDX,NROW)
      IW(KLL+1) = IPAD
C
C - Deposit here the number of hits
  300 IW(KLL+1+ISTOR) = IW(KLL+1+ISTOR)+NHIT
  999 RETURN
      END
