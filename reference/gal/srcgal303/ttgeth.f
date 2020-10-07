      SUBROUTINE TTGETH(ITE)
C ----------------------------------------------------------------------
C. - M.MERMIKIDES - 851206
C. - Compute intersections(hits) of charged track element ITE
C.   with trigger pad rows
C. - Banks created:
C                 TTHT  Intersections with padrows
C                 TTHE  References to track elements
C.
C. -Called from   TTHIT
C. -Calls         ALBOS
C.                BKFMT,BLIST
C----------------------------------------------------------------------
      SAVE
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
      COMMON /TPGEOT/ NTPTRW(LTSTYP),NTPTPR(LTTSRW,LTSTYP),
     &                TPTRBG(LTSTYP),TPTRST(LTSTYP),TPTRHG(LTSTYP),
     &                TPTPHC(LTTPAD,LTTSRW,LTSTYP),
     &                TPTPHW(LTTPAD,LTTSRW,LTSTYP),
     &                ITPADG(LTTPAD,LTTSRW,LTSTYP)
C
      PARAMETER (LTPTE=12, LBTE=1000*LTPTE, LBTEX=LBTE/2)
      PARAMETER (NWPHT=6 , LBHT=1000*NWPHT, LBHTX=LBHT/2)
      PARAMETER (NWPHE=1 , LBHE=1000*NWPHE, LBHEX=LBHE/2)
      COMMON /TPNAMC/ NATPTE,NATPHT,NATPHE,NATTHT,NATTHE,NATPCO,NATPCH,
     &                 NATCRL
     &               ,JDTWRK,JDSORT,JDWORK
      DIMENSION XINT(6),PZINT(4)
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
C
C ------------------------------------------------------------
C  Create TTHT and TTHE banks on entry
C
      ITTHT = IW(NATTHT)
      IF(ITTHT.EQ.0) THEN
         CALL ALBOS ('TTHT',0,LBHT,ITTHT,IGARB)
         CALL BLIST(IW,'E+','TTHT')
         IW(ITTHT + 1) = NWPHT
         IW(ITTHT + 2) = 0
      ENDIF
C
      IF (ICTPJO(4).GT.0) THEN
         ITTHE = IW(NATTHE)
         IF(ITTHE.EQ.0) THEN
            CALL ALBOS ('TTHE',0,LBHE,ITTHE,IGARB)
C  If garbage collection occured, redefine index for 'TTHT'
            IF (IGARB.EQ.1)  ITTHT = IW(NATTHT)
            CALL BLIST(IW,'E+','TTHE')
            IW(ITTHE + 1) = NWPHE
            IW(ITTHE + 2) = 0
         ENDIF
      ENDIF
C  Get intersections with wire (cylindrical) layers
      R1 = SQRT(TRKELE(1)**2 + TRKELE(2)**2)
      R2 = SQRT(TRKNXT(1)**2 + TRKNXT(2)**2)
      IF(R1.GT.R2) THEN
         RS = R2
         R2 = R1
         R1 = RS
      ENDIF
C
      DO 205 IROW = 1,LTTROW
C
         IF(TPTROW(IROW).LT.R1) GO TO 205
         IF(TPTROW(IROW).GT.R2) GO TO 205
C
C  Determine sector types and endplate according to radius
C  and volume number
C
         IF (IROW.LE.NTPTRW(1)) THEN
C  K-type
            IST = 0
            IF (ITRKEL(10).NE.2) IST = LTSECT/2
            ISL1 = IST + 1
            ISL2 = IST + LTSLOT/2
            IRS = IROW
         ELSE
C  M,W-types
            IST = LTSLOT/2
            IF (ITRKEL(10).NE.2) IST = IST + LTSECT/2
            ISL1 = IST + 1
            ISL2 = IST + LTSLOT
            IRS = IROW - NTPTRW(1)
         ENDIF
C
C   Use GEANT3 routine with cubic model
C
         CALL GICYL(TPTROW(IROW),TRKELE(1),TRKNXT(1),
     *              0., TRKELE(11),3,XINT,SINT,PZINT,IFLAG)
         IF (IFLAG.NE.1) GO TO 205
         PHIGL = PZINT(1)
         IF(PHIGL.LT.0.) PHIGL = PHIGL + TWOPI
C
         DO 19 ISLOT = ISL1,ISL2
            DPHI = TPHDIF(TPPHI0(ISLOT),PHIGL)
            ITYP = ITPTYP(ISLOT)
            PHI1 = TPTPHC(1,IRS,ITYP)-TPTPHW(1,IRS,ITYP)
            NP = NTPTPR(IRS,ITYP)
            PHI2 = TPTPHC(NP,IRS,ITYP)+TPTPHW(NP,IRS,ITYP)
            IF (DPHI.LT.PHI1.OR.DPHI.GT.PHI2) GO TO 19
            DO 119 IPAD = 1,NP
               DP = TPHDIF(DPHI,TPTPHC(IPAD,IRS,ITYP))
               IF(ABS(DP).GT.TPTPHW(IPAD,IRS,ITYP)) GO TO 119
               GO TO 300
119         CONTINUE
19       CONTINUE
         GO TO 205
C
C  Enter hit in BOS bank
C
300      CONTINUE
C  If no room to store next hit, increase size of bank
         IF(LFRWRD(ITTHT) .LT. LCOLS(ITTHT)) THEN
            NDATA = IW(ITTHT) + LBHTX
            CALL ALBOS ('TTHT',0,NDATA,ITTHT,IGARB)
            IF (IGARB.EQ.1) ITTHE = IW(NATTHE)
         ENDIF
C   KHT1 = Address of next free row
         KHT1 = KNEXT(ITTHT)
         IW(KHT1 + 1) = ITRKEL(1)
         IW(KHT1 + 2) = IROW*100000 + ISLOT*1000 + IPAD
         RW(KHT1 + 3) = PHIGL
         RW(KHT1 + 4) = PZINT(2)
         RW(KHT1 + 5) = PZINT(3)
         RW(KHT1 + 6) = PZINT(4)
         IW(ITTHT + 2) = IW(ITTHT + 2) + 1
C
C  Update TTHE bank if track element ref. is given
C
         IF (ITE.EQ.0) GO TO 205
C  If no room to store next hit, increase size of bank
         IF(LFRWRD(ITTHE) .LT. LCOLS(ITTHE)) THEN
            NDATA = IW(ITTHE) + LBHEX
            CALL ALBOS ('TTHE',0,NDATA,ITTHE,IGARB)
            IF (IGARB.EQ.1) ITTHT = IW(NATTHT)
         ENDIF
C   KHE1 = Address of next free row
         KHE1 = KNEXT(ITTHE)
         IW(KHE1 + 1) = ITE
         IW(ITTHE + 2) = IW(ITTHE + 2) + 1
205   CONTINUE
C
      GO TO 999
999   RETURN
      END
