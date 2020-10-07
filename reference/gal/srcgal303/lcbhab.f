      SUBROUTINE LCBHAB(LCBHA,EFORW,EBACK)
C--------------------------------------------------------------
C! Simulates the Bhabha trigger
C! Author   : J.D.Hansen & P.H.Hansen - 860604
C! Modified : P.H.Hansen - 871001
C!
C!   Input : 'LTTR'
C!   Output: LCBHA  = 0 ... No trigger
C!                  = 1 ... Trigger
C!           EFORW  = trigger energy in subdetector A
C!           EBACK  = trigger energy in subdetector B
C!           update 'LCWS'
C!
C! - Called by LCTRIG                            from this .HLB
C ------------------------------------------------
      SAVE
      DIMENSION JTRIG(12,2)
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
      COMMON /LCNAMC/ NALTHT, NALTDI, NALWHT, NALWDI, NALTTR, NALWTR,
     *                NALWHI, NALSHI, NALCWS, NALCAL, NALLAY, NALMTY,
     *                NALALI, NALSCO, NALSLO, NALWRG, NALCEL, NALCSH,
     *                NALDRE, NALCCA, NALCPG, NALSHO
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
      LCBHA = 0
      EFORW = 0.
      EBACK = 0.
      KINDX = IW(NALTTR)
      IF(KINDX .LE. 0)                               GOTO 999
      LEL = LCOLS(KINDX)
C
C? Form super segments of adjecent pairs in the inner ring
      DO 100 IDET = 1,2
         DO 110 I = 1,12
           ISEG = (IDET-1)*12 + I
           KSEG = KROW(KINDX,ISEG)
           JTRIG(I,IDET) = 0
           DO 120 ISTOR = 1,3
             KL = KSEG + ISTOR
             IF (I .NE. 12) THEN
               JTRIG(I,IDET) = JTRIG(I,IDET) + IW(KL) + IW(KL+LEL)
             ELSE
               JTRIG(I,IDET) = JTRIG(I,IDET) + IW(KL) + IW(KL-11*LEL)
             ENDIF
  120      CONTINUE
  110   CONTINUE
        IF (LCPRNT .LT. 4)                                   GOTO 100
        WRITE(LOUTIO,5000) IDET,(JTRIG(J,IDET),J=1,12)
  100 CONTINUE
C
C? Loop over trigger segments and apply the trigger theshold
C? on both sides
      DO 200 I = 1,12
        J = I + 6
        IF (J .GT. 12) J = J - 12
        IF (JTRIG(I,1).GT.LCBHTR.AND.JTRIG(J,2).GT.LCBHTR) GOTO 210
  200 CONTINUE
C - Failure
      GOTO 999
C - Success
  210 LCBHA = 1
C
C? Energy in the trigger segments
      EFORW = FLOAT(JTRIG(I,1))*.001
      EBACK = FLOAT(JTRIG(J,2))*.001
C
C? Update the summary bank
      KINDX = IW(NALCWS)
      IF(KINDX .LE. 0)                                  GOTO 999
      IW(KINDX+LMHLEN+2) = IW(KINDX+LMHLEN+2) + 1
      IW(KINDX+LMHLEN+3) = IW(KINDX+LMHLEN+3) + JTRIG(I,1)
      IW(KINDX+LMHLEN+4) = IW(KINDX+LMHLEN+4) + JTRIG(J,2)
  999 CONTINUE
C
 5000 FORMAT(' +++ LCBHAB +++ component',I10,/,6I10,/,6I10)
      END
