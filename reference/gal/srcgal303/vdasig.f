      SUBROUTINE VDASIG
C-----------------------------------------------------------------------
C! Compute analog signals
CKEY VDET DIGITIZING
C!
C!  Author         F.Forti        11/6/86
C!  Modified       A. Bonissent   15/02/94
C!                                Suppress usage of work bank
C!
C!  Description
C!  ===========
C!  This routine does very little now that VDHT bank is created
C!  directly in VDHIT routine.
C!
C!  No noise is added here, as Minivertex noise must be added strip by
C!  strip in the digitization part.
C!  In fact, all the processing of Vdet is done later, by routine VDDIGI
C-----------------------------------------------------------------------
C
      INTEGER JVDHTN, JVDHLN, JVDHPN, JVDHXE, JVDHYE, JVDHZE,
     $   JVDHXL, JVDHYL, JVDHZL, JVDHER, LVDHTA
      PARAMETER(JVDHTN=1,JVDHLN=2,JVDHPN=3,JVDHXE=4,JVDHYE=5,JVDHZE=6,
     $   JVDHXL=7,JVDHYL=8,JVDHZL=9,JVDHER=10,LVDHTA=10)
      INTEGER JVDSTN, JVDSLN, JVDSPN, JVDSXE, JVDSYE, JVDSZE,
     $   JVDSXL, JVDSYL, JVDSZL, JVDSER, JVDSRN, JVDSES, LVDSSA
      PARAMETER(JVDSTN=1,JVDSLN=2,JVDSPN=3,JVDSXE=4,JVDSYE=5,JVDSZE=6,
     $   JVDSXL=7,JVDSYL=8,JVDSZL=9,JVDSER=10,JVDSRN=11,JVDSES=12,
     $   LVDSSA=12)
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
C
      DATA NAVDSS, NAVDHT /2*0/
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
      IF (NAVDSS.EQ.0) THEN
         NAVDSS = NAMIND('VDSS')
         NAVDHT = NAMIND('VDHT')
      ENDIF
C
C drop old VDHT bank
C
      CALL BDROP(IW,'VDHT')
C
C Compress VDSS bank
C
      CALL AUBPRS('VDSS')
      KVDSS = IW(NAVDSS)
      IF(KVDSS.LE.0) RETURN
      NVDSS=LROWS(KVDSS)
      IF(NVDSS.LE.0) RETURN
      NDATA = LMHLEN + LVDHTA*NVDSS
      CALL ALBOS('VDHT',0,NDATA,KVDHT,IGARB)
      IF(IGARB.EQ.1) KVDSS = IW(NAVDSS)
C
C?       Add 'VDHT' bank to the 'E' list
C
      CALL BLIST(IW,'E+','VDHT')
      IW(KVDHT+LMHCOL) = LVDHTA
      IW(KVDHT+LMHROW) = 0
C
      DO I=1,LROWS(KVDSS)-1
       IF((ITABL(KVDSS,I,JVDSTN).NE.ITABL(KVDSS,I+1,JVDSTN)).OR.
     &    (ITABL(KVDSS,I,JVDSLN).NE.ITABL(KVDSS,I+1,JVDSLN)).OR.
     &    (ITABL(KVDSS,I,JVDSPN).NE.ITABL(KVDSS,I+1,JVDSPN))) THEN
        IF(IW(KROW(KVDSS,I)+JVDSES).NE.1) THEN
          CALL ALTELL('VDASIG: Error in track segment', 0, 'RETURN')
          IW(KROW(KVDSS,I)+JVDSES)=1
        ENDIF
       ENDIF
      ENDDO
C
      I=1
      DO WHILE(I.LE.NVDSS)
       IF(ITABL(KVDSS,I,JVDSTN).NE.0) THEN
        KLINE = KNEXT(KVDHT)
        IW(KVDHT+LMHROW) = LROWS(KVDHT) + 1
        IW(KLINE + JVDHTN) = ITABL(KVDSS,I,JVDSTN)
        IW(KLINE + JVDHLN) = ITABL(KVDSS,I,JVDSLN)
        IW(KLINE + JVDHPN) = ITABL(KVDSS,I,JVDSPN)
        RW(KLINE + JVDHXE) = RTABL(KVDSS,I,JVDSXE)
        RW(KLINE + JVDHYE) = RTABL(KVDSS,I,JVDSYE)
        RW(KLINE + JVDHZE) = RTABL(KVDSS,I,JVDSZE)
        RW(KLINE + JVDHER) = 0.
        J=0
        DO WHILE(ITABL(KVDSS,I+J,JVDSES).EQ.0.AND.I+J.LT.NVDSS)
         RW(KLINE + JVDHER) = RW(KLINE + JVDHER)+RTABL(KVDSS,I+J,JVDSER)
         IW(KROW(KVDSS,I+J)+JVDSRN)=IW(KVDHT+LMHROW)
         J=J+1
        ENDDO
        RW(KLINE + JVDHER) = RW(KLINE + JVDHER)+RTABL(KVDSS,I+J,JVDSER)
        IW(KROW(KVDSS,I+J)+JVDSRN)=IW(KVDHT+LMHROW)
        RW(KLINE + JVDHXL) = RTABL(KVDSS,I+J,JVDSXL)
        RW(KLINE + JVDHYL) = RTABL(KVDSS,I+J,JVDSYL)
        RW(KLINE + JVDHZL) = RTABL(KVDSS,I+J,JVDSZL)
        IF(ITABL(KVDSS,I,JVDSTN).NE.ITABL(KVDSS,I+J,JVDSTN).OR.
     &     ITABL(KVDSS,I,JVDSPN).NE.ITABL(KVDSS,I+J,JVDSPN).OR.
     &     ITABL(KVDSS,I,JVDSLN).NE.ITABL(KVDSS,I+J,JVDSLN)) THEN
          CALL ALTELL('VDASIG: Error in track segment', 0, 'RETURN')
        ENDIF
        I=I+J+1
       ELSE
        I=I+1
       ENDIF
      ENDDO
C
C Compress VDHT bank
C
      CALL AUBPRS('VDHT')
C -----------------------------------------------------------------
C
C  Check run conditions to see if long debug activated
C
      IF (FDEBJO.AND.ICVDJO(6).NE.0) THEN
        KVDHT = IW(NAVDHT)
        ND = IW(KVDHT)
        WRITE(LOUTIO,8000) ND
 8000   FORMAT(/' +++VDASIG+++ Named bank VDHT exists.',
     &      '  Length of bank is:',I5)
      ENDIF
C
C    Normal debug
C
      IF (FDEBJO.AND.IPRIJO(1).NE.0) CALL VDPRHT
C
      RETURN
      END
