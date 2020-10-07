      SUBROUTINE VDHIT
C-----------------------------------------------------------------------
C! Compute hits for VDET
CKEY VDET DIGITIZE
C!
C!
C!  Author         F.Forti        12/6/86
C!  Modified       G.Triggiani    02/02/88
C!                 F.Ranjard      30/05/90
C!                 A. Bonissent   15/02/94
C!                                Suppress usage of work bank
C!
C!  Description
C!  ===========
C!  When control is given to this routine, data from the
C!  common block TRKCOM are use to accumulate hit information in
C!  VDHT bank
C-----------------------------------------------------------------------
C
      SAVE NAVDHT
C
      PARAMETER (NVDHT = 1200)
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER JVDHTN, JVDHLN, JVDHPN, JVDHXE, JVDHYE, JVDHZE,
     $   JVDHXL, JVDHYL, JVDHZL, JVDHER, LVDHTA
      PARAMETER(JVDHTN=1,JVDHLN=2,JVDHPN=3,JVDHXE=4,JVDHYE=5,JVDHZE=6,
     $   JVDHXL=7,JVDHYL=8,JVDHZL=9,JVDHER=10,LVDHTA=10)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER VIFACI
      EXTERNAL CHAINT
      CHARACTER*4 CHAINT
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDHT = NAMIND('VDHT')
      ENDIF
C
C? If it is a new event drop VDHT and create new one
C
      IF (FBEGJO(1)) THEN
        FBEGJO(1) = .FALSE.
C           Drop old hits bank
        CALL BDROP(IW,'VDHT')
C           Create bank
        NDATA = LMHLEN + LVDHTA*NVDHT
        CALL ALBOS('VDHT',0,NDATA,KVDHT,IGARB)
C
C?       Add 'VDHT' bank to the 'E' list
C
      CALL BLIST(IW,'E+','VDHT')
        IW(KVDHT+LMHCOL) = LVDHTA
        IW(KVDHT+LMHROW) = 0
        IF (FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,8000)
 8000   FORMAT(/' +++VDHIT +++ New event. VDHT bank created')
      ENDIF
C
C? Process track element.
C
C        Check for first,intermediate and last steps inside volume.
C
C? If enter volume for the first time
C           Check that there is enough space in bank
C           If not increase bank size
C
      KVDHT = IW(NAVDHT)
      IF (ITRKEL(8).EQ.1) THEN
        IF (LFRROW(KVDHT).LT.1) THEN
          NDATA = IW(KVDHT) + NVDHT*LVDHTA
          CALL ALBOS('VDHT',0,NDATA,KVDHT,IGARB)
        ENDIF
C
        ENER = 0.
C
C           Calculate layer number, phi coord, zed coord.
C
        ISLOT = ITRKEL(10)
        IBID = VIFACI(ISLOT,NLAY,NPHI)
C
C              Store ENTRY point
C
        KLINE = KNEXT(KVDHT)
        IW(KLINE + JVDHTN) = ITRKEL(1)
        IW(KLINE + JVDHLN) = NLAY
        IW(KLINE + JVDHPN) = NPHI
        RW(KLINE + JVDHXE) = TRKELE(1)
        RW(KLINE + JVDHYE) = TRKELE(2)
        RW(KLINE + JVDHZE) = TRKELE(3)
        IF(FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,810) LROWS(KVDHT)+1
  810   FORMAT(' +++VDHIT +++ First step. Hit number= ',I5 )
C
C? IF intermediate step accumulate energy release
C
      ELSEIF (ITRKEL(8).EQ.0 .AND. ITRKEL(9).EQ.0) THEN
        ENER = ENER + TRKELE(12)
        IF (FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,8100) ENER
 8100   FORMAT(' +++VDHIT +++ Intermediate step. ENERGY= ',G8.3)
C
C? IF enter volume for the last time store the exit point
C
      ELSEIF (ITRKEL(8).EQ.2 .OR. ITRKEL(9).GT.0) THEN
        KLINE = KNEXT(KVDHT)
        RW(KLINE + JVDHXL) = TRKNXT(1)
        RW(KLINE + JVDHYL) = TRKNXT(2)
        RW(KLINE + JVDHZL) = TRKNXT(3)
        RW(KLINE + JVDHER) = ENER + TRKELE(12)
        IF (RW(KLINE+JVDHER).GT.0.) IW(KVDHT+LMHROW) = LROWS(KVDHT) + 1
        IF(FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,8120) RW(KVDHT+10)
 8120   FORMAT(' +++VDHIT +++ Last step in volume. ENER= ',G8.3)
      ENDIF
      RETURN
      END
