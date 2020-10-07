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
C!                 G.Taylor       1/08/95
C!                                use GEANT to treat landau
C!                                fluctuations inside the VDET
C!
C!  Description
C!  ===========
C!  When control is given to this routine, data from the
C!  common block TRKCOM are use to accumulate hit information in
C!  VDSS bank
C-----------------------------------------------------------------------
C
      SAVE NAVDSS
      SAVE ENER
C
      PARAMETER (NVDSS = 1200)
C
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
      INTEGER JVDSTN, JVDSLN, JVDSPN, JVDSXE, JVDSYE, JVDSZE,
     $   JVDSXL, JVDSYL, JVDSZL, JVDSER, JVDSRN, JVDSES, LVDSSA
      PARAMETER(JVDSTN=1,JVDSLN=2,JVDSPN=3,JVDSXE=4,JVDSYE=5,JVDSZE=6,
     $   JVDSXL=7,JVDSYL=8,JVDSZL=9,JVDSER=10,JVDSRN=11,JVDSES=12,
     $   LVDSSA=12)
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
        NAVDSS = NAMIND('VDSS')
      ENDIF
C
C? If it is a new event drop VDSS and create new one
C
      IF (FBEGJO(1)) THEN
        FBEGJO(1) = .FALSE.
C           Create bank
        NDATA = LMHLEN + LVDSSA*NVDSS
        CALL ALBOS('VDSS',0,NDATA,KVDSS,IGARB)
        IW(KVDSS+LMHCOL) = LVDSSA
        IW(KVDSS+LMHROW) = 0
C add bank to T list so that it is removed between events
        CALL BLIST(IW,'T+','VDSS')
        IF (FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,8000)
 8000   FORMAT(/' +++VDHIT +++ New event. VDSS bank created')
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
      KVDSS = IW(NAVDSS)
      IF (ITRKEL(8).EQ.1) THEN
        IF (LFRROW(KVDSS).LT.1) THEN
          NDATA = IW(KVDSS) + NVDSS*LVDSSA
          CALL ALBOS('VDSS',0,NDATA,KVDSS,IGARB)
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
        KLINE = KNEXT(KVDSS)
        IW(KLINE + JVDSTN) = ITRKEL(1)
C
C this particle is a delta ray or other non primary track
C store its track number as 0 to indicate this
C
        IF(ITRKEL(2).NE.0) IW(KLINE + JVDSTN) = 0
        IW(KLINE + JVDSLN) = NLAY
        IW(KLINE + JVDSPN) = NPHI
        IW(KLINE + JVDSRN) = 0
        IW(KLINE + JVDSES) = 0
        RW(KLINE + JVDSXE) = TRKELE(1)
        RW(KLINE + JVDSYE) = TRKELE(2)
        RW(KLINE + JVDSZE) = TRKELE(3)
        IF(FDEBJO.AND.ICVDJO(6).NE.0) WRITE(LOUTIO,810) LROWS(KVDSS)+1
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
        KLINE = KNEXT(KVDSS)
        RW(KLINE + JVDSXL) = TRKNXT(1)
        RW(KLINE + JVDSYL) = TRKNXT(2)
        RW(KLINE + JVDSZL) = TRKNXT(3)
        RW(KLINE + JVDSER) = ENER + TRKELE(12)
C
C if the track ends in this volume set the end segment flag
C
        IF(ITRKEL(9).GT.0) IW(KLINE + JVDSES) = 1
        IF (RW(KLINE+JVDSER).GT.0.) IW(KVDSS+LMHROW) = LROWS(KVDSS) + 1
        IF(FDEBJO.AND.ICVDJO(6).NE.0)
     &    WRITE(LOUTIO,8120) RW(KVDSS+JVDSER)
 8120   FORMAT(' +++VDHIT +++ Last step in volume. ENER= ',G8.3)
      ENDIF
      RETURN
      END
