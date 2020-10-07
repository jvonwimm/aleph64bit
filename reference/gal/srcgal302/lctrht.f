      SUBROUTINE LCTRHT
C--------------------------------------------------------------
C! Sum up tower hits for triggering
C. - J.Dines Hansen & P.Hansen - 860417
C                                modified by F.Ranjard - 890210
C. - Fills bank LTTR
C. - Called by  LCDIGI                           from this .HLB
C. - Calls      LCSAMP                           from this .HLB
C.              BLIST                            from BOS
C.              VADD                             from CERNLIB
C -----------------------------------------------
      SAVE
      EXTERNAL LCSAMP
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
C         parameters in the LCal code
        PARAMETER (LWDIR = 4, LTTRR = 24, LTTRC = 3)
        PARAMETER (LWTRR = 4, LWTRC = 2 , LTDIC = 4)
C         macro to decode pad-address
        LCMOD(IXXXX) = (IXXXX-1)/512 + 1
        LCROW(IXXXX) = MOD(IXXXX-1,512)/16 + 1
        LCCOL(IXXXX) = MOD(IXXXX-1,16) + 1
C
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
C ------------------------------------------------------------
C
C - Book Tower Trigger bank
      LEN = LTTRR * LTTRC + LMHLEN
      CALL ALBOS ('LTTR',0,LEN,KL1TR,IGARB)
      IW(KL1TR+1) = LTTRC
      IW(KL1TR+2) = LTTRR
      CALL BLIST(IW,'E+','LTTR')
C
C - Sum up tower hits in the appropiate trigger amplifier
      KL1TH = IW(NALTHT)
      DO 200 ITOW = 1,LROWS(KL1TH)
         KTH = KROW(KL1TH,ITOW)
         IPAD= IW(KTH+1)
         IAMP= LCSAMP(IPAD)
         IF (IAMP .LE. 0 .OR. IAMP .GT. LTTRR)  GOTO 997
         KTR = KROW(KL1TR,IAMP)
         DO 220 ISTOR = 1,3
            IW(KTR+ISTOR) = IW(KTR+ISTOR) + IW(KTH+ISTOR+1)
  220    CONTINUE
  200 CONTINUE
      GOTO 999
C
 997  WRITE (LOUTIO,'(1X,''+++LCTRHT+++ wrong data in row# '',9I9)')
     &        ITOW,(IW(KTH+M),M=1,LCOLS(KL1TH)),IAMP
C
  999 RETURN
      END
