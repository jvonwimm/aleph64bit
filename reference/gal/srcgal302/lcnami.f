      SUBROUTINE LCNAMI
C--------------------------------------------------------------
C! Prepare BOS banks
C! Author     : P.Hansen & J. Dines Hansen 860801
C! Modified   : P.Hansen 870920
C! Desciption :
C! ============
C! Set name-indices for LCal BOS banks
C! Set printing formats
C!
C! - called from LCIRUN                          from this .HLB
C! - calls       NAMIND,BKFMT                    from BOS
C!
C -------------------------------------------------
      EXTERNAL NAMIND
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
C -------------------------------------------------------------
C
      NALALI = NAMIND('LALI')
      NALCAL = NAMIND('LCAL')
      NALLAY = NAMIND('LLAY')
      NALMTY = NAMIND('LMTY')
      NALSCO = NAMIND('LSCO')
      NALSLO = NAMIND('LSLO')
      NALWRG = NAMIND('LWRG')
      NALDRE = NAMIND('LDRE')
      NALCCA = NAMIND('LCCA')
      NALCPG = NAMIND('LCPG')
      NALCEL = NAMIND('LCEL')
      NALSHO = NAMIND('LSHO')
C
      NALSHI = NAMIND('LSHI')
      NALWHI = NAMIND('LWHI')
      CALL BKFMT('LSHI','I')
      CALL BKFMT('LWHI','I')
C
      NALWHT = NAMIND('LWHT')
      CALL BKFMT('LWHT','(I)')
C
      NALWDI = NAMIND('LWDI')
      CALL BKFMT('LWDI','(I)')
C
      NALTHT = NAMIND('LTHT')
      CALL BKFMT('LTHT','(I)')
C
      NALTDI = NAMIND('LTDI')
      CALL BKFMT('LTDI','(I)')
C
      NALTTR = NAMIND('LTTR')
      CALL BKFMT('LTTR','(I)')
C
      NALWTR = NAMIND('LWTR')
      CALL BKFMT('LWTR','(I)')
C
      RETURN
C
      END
