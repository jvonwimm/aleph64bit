      SUBROUTINE VDFILL(TYPE,IARG)
C!----------------------------------------------------------------------
C! Fill vdet histograms
CKEY VDET HISTO
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!  Fill Vdet histograms
C!  and accumulate statistics
C!
C! Input :   TYPE : type of operations to be done. Can be :
C!                  'INIT'
C!                  'WAFER'
C!                  'MODULE'
C!                  'EVENT'
C!                  'SUMMARY'
C!
C!           IARG : Operation dependent input arguments
C!
C-----------------------------------------------------------------------
C
      SAVE NAVTRS,NAVDHT,NAVDLH,NAVWCX
      SAVE NFIRS,NVDHE,IVDHE,NVDHW,IVDHW,NVDHM,IVDHM
      SAVE NVDSZ,IVDSZ,NVDSP,IVDSP
      SAVE VDLGV,VDLCO
C
      DIMENSION NBCLN(2),NVDCR(2),VDSLP(2),VDPAL(2),VDELC(2)
      DIMENSION MXCNO(2),MXCSI(2),VELGV(2),VDLCO(2),IOFSET(2),NBITSH(2)
      DIMENSION THRES(2),NAVWCX(2),NFIRV(2),NSTRH(2),QSTRH(2)
      DIMENSION XI(3),XJ(3),ERPH(2)
      DIMENSION IARG(*)
      CHARACTER*(*)TYPE
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JVWCCC=1,JVWCSN=2,JVWCVT=3,LVWC1A=3)
      PARAMETER(JVTRAD=1,JVTRCH=2,JVTRHT=3,JVTRVT=4,LVTRSA=4)
      INTEGER JVDHTN, JVDHLN, JVDHPN, JVDHXE, JVDHYE, JVDHZE,
     $   JVDHXL, JVDHYL, JVDHZL, JVDHER, LVDHTA
      PARAMETER(JVDHTN=1,JVDHLN=2,JVDHPN=3,JVDHXE=4,JVDHYE=5,JVDHZE=6,
     $   JVDHXL=7,JVDHYL=8,JVDHZL=9,JVDHER=10,LVDHTA=10)
      PARAMETER(JVDLXI=1,JVDLXO=4,JVDLMO=7,JVDLWA=8,JVDLCO=9,JVDLER=10,
     +          JVDLTR=11,JVDLHT=12,LVDLHA=12)
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
        NAVTRS = NAMIND('VTRS')
        NAVDHT = NAMIND('VDHT')
        NAVDLH = NAMIND('VDLH')
        NAVWCX(1)=NAMIND('VWC1')
        NAVWCX(2)=NAMIND('VWC2')
        CALL VFNDEL
     $   (1,NBCLN,NVDCR,VDSLP,VDPAL,VDELC,
     $    MXCSI,MXCNO,VELGV,VDLCO,IOFSET,NBITSH)
      ENDIF
      KVDHT=IW(NAVDHT)
      IF(TYPE.EQ.'INIT')THEN
         NFIRS = 0
         NVDHE=0
         IVDHE=0
         NVDHW=0
         IVDHW=0
         NVDHM=0
         IVDHM=0
         NVDSZ=0
         IVDSZ=0
         NVDSP=0
         IVDSP=0
      ELSEIF(TYPE.EQ.'WAFER')THEN
         KVDLH=IW(NAVDLH)
         IF(KVDLH.NE.0)THEN
            IMIN=IARG(2)
            IMAX=IARG(3)
            IVDHW=IVDHW+IMAX-IMIN+1
            NVDHW=NVDHW+1
            DO 30 IVDLH=IMIN,IMAX-1
            DMIN=1000.
            DO 31 IDIM=1,3
   31       XI(IDIM)=(RTABL(KVDLH,IVDLH,JVDLXI+IDIM-1)+
     >                RTABL(KVDLH,IVDLH,JVDLXO+IDIM-1))/2.
            DO 32 JVDLH=IVDLH+1,IMAX
            DO 33 IDIM=1,3
   33       XJ(IDIM)=(RTABL(KVDLH,JVDLH,JVDLXI+IDIM-1)+
     >                RTABL(KVDLH,JVDLH,JVDLXO+IDIM-1))/2.
            DIST=VDIST(XI,XJ,3)
            DMIN=AMIN1(DIST,DMIN)
   32       CONTINUE
            IF(FHISJO(1))CALL HF1(105,DMIN,1.)
   30       CONTINUE
         ENDIF
      ELSEIF(TYPE.EQ.'MODULE')THEN
         IVDHM=IVDHM+IVDHW
         NVDHM=NVDHM+1
         IF(FHISJO(1))CALL HF1(102,FLOAT(IVDHW),1.)
         IVDHW=0
         IMOD=IARG(1)
         CALL VFNDTH(IMOD,THRES)
         DO 10 IV=1,2
         KVWCX=IW(NAVWCX(IV))
         NFIRV(IV)=0
         IF(KVWCX.NE.0)THEN
            NVWCX=LROWS(KVWCX)
            DO 11 IVWCX=1,NVWCX
               PULS = RTABL(KVWCX,IVWCX,JVWCCC)
               IF(PULS.NE.0)THEN
                  NFIRS=NFIRS+1
                  NFIRV(IV)=NFIRV(IV)+1
               ENDIF
               IF(PULS.GT.THRES(IV))THEN
                  IF(FHISJO(1))CALL HF1(106,PULS,1.)
               ENDIF
   11       CONTINUE
            IF(FHISJO(1))THEN
               IF(NFIRV(1).NE.0)CALL HF1(104,FLOAT(NFIRV(1)),1.)
               IF(NFIRV(2).NE.0)CALL HF1(110,FLOAT(NFIRV(2)),1.)
            ENDIF
         ENDIF
   10    CONTINUE
      ELSEIF(TYPE.EQ.'EVENT')THEN
         KVDHT=IW(NAVDHT)
         IF(KVDHT.NE.0)THEN
            NVDHT=LROWS(KVDHT)
            DO 20 IVDHT=1,NVDHT
               PULSH=RTABL(KVDHT,IVDHT,JVDHER)
               IF(FHISJO(1))CALL HF1(107,PULSH,1.)
   20       CONTINUE
            NVDHE =NVDHE+1
            IVDHE = IVDHE+NVDHT
            IF(FHISJO(1))THEN
               CALL HF1(101,FLOAT(NVDHT),1.)
               CALL HF1(103,FLOAT(NFIRS),1.)
            ENDIF
C Reset Nb fired strips in the event
            NFIRS = 0
         ENDIF
         KVTRS = IW(NAVTRS)
         IF(KVDHT.NE.0.AND.KVTRS.NE.0)THEN
C
C Compute number of fired strips per hit in each view
C and fraction of charge collected
C
            NVDHT=LROWS(KVDHT)
            NVTRS=LROWS(KVTRS)
            DO 40 IVDHT=1,NVDHT
            EREL=(RTABL(KVDHT,IVDHT,JVDHER))
C
C Convert into pulse height
C
            DO IV=1,2
               QSTRH(IV)=0.
               NSTRH(IV)=0
               ERPH(IV) = EREL*NBITSH(IV)/(VELGV(IV)*VDLCO(IV))
            ENDDO
            DO 41 IVTRS=1,NVTRS
            IF(IVDHT.EQ.ITABL(KVTRS,IVTRS,JVTRHT))THEN
               ICODE=ITABL(KVTRS,IVTRS,JVTRAD)
               CALL VUNADD(ICODE,IBID,IL,IZ,IP,IV,ISTR)

               NSTRH(IV) = NSTRH(IV)+1
               QSTRH(IV) = QSTRH(IV)+RTABL(KVTRS,IVTRS,JVTRCH)
            ENDIF
   41       CONTINUE
            NVDSZ=NVDSZ+1
            NVDSP=NVDSP+1
            IVDSZ=IVDSZ+NSTRH(1)
            IVDSP=IVDSP+NSTRH(2)
            IF(FHISJO(1))THEN
               IF(NSTRH(1).NE.0)THEN
                  CALL HF1(108,FLOAT(NSTRH(1)),1.)
                  CALL HF1(111,QSTRH(1)/ERPH(1),1.)
               ENDIF
               IF(NSTRH(2).NE.0)THEN
                  CALL HF1(109,FLOAT(NSTRH(2)),1.)
                  CALL HF1(111,QSTRH(2)/ERPH(2),1.)
               ENDIF
            ENDIF
   40       CONTINUE
         ENDIF
         IF (FDEBJO.AND.IPRIJO(1).NE.0) CALL VDPRDI
      ELSEIF(TYPE.EQ.'SUMMARY')THEN
         IARG(1)=NVDHE
         IARG(2)=IVDHE
         IARG(3)=NVDHM
         IARG(4)=IVDHM
         IARG(5)=NVDSZ
         IARG(6)=IVDSZ
         IARG(7)=NVDSP
         IARG(8)=IVDSP
      ENDIF
      RETURN
      END
