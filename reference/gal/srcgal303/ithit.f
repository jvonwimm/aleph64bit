      SUBROUTINE ITHIT
C.
C...ITHIT  2.00  900423  17:23                         R.Beuselinck.
C.
C!  Compute the hits data for the ITC.
C.
C.  Called by: GUSTEP                                   from this .HLB
C.      Calls: ITNOIS, ITALIG, ITCROS                   from this .HLB
C.             AUARCM                                   from ALEPHLIB
C.             WBANK                                    from BOS77
C.             SORTZV                                   from CERNLIB
C.
C.  Named banks used:
C.  IHIT    - created on 1st call for each event. Filled on each call.
C.
C-----------------------------------------------------------------------
      SAVE
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/ITWIRC/RWIRIT(8),NWIRIT(8),IWIRIT(8),PHWRIT(8),CELHIT(8),
     +              CELWIT(8),WZMXIT
C
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      COMMON/ITROTC/EULRIT(3),DXYZIT(3),ROTITC(3,3),ITSHFT,WSAGIT
      REAL EULRIT,DXYZIT,ROTITC,WSAGIT
      LOGICAL ITSHFT
C
      COMMON/ITSUMC/NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,DIGHIT,
     +      NTSMIT(10),NHSMIT(10),NDSMIT(10),NDHSIT(10)
      INTEGER NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,
     +      NTSMIT,NHSMIT,NDSMIT,NDHSIT
      REAL DIGHIT
C
      EXTERNAL AUARCM
C
      PARAMETER (SGTL2=1E-6, AATOL=0.001, BBTOL=0.001, STPMN=0.001)
      REAL SS(50), VEC1(6), VEC2(6), VA(3), VB(3), VP(3)
      INTEGER NNW(50), LLAY(50), INDX(50)
      INTEGER IFLAG
C
      LOGICAL ORD
      DATA NLAY /8/
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
C--  Declare assignment statements.
C--
      ORD(A,B,C) = (A.LT.B .AND. B.LT.C)
C
C --------------------------------------------------------------
C-- Is it a new event ?
C--
      IF (FBEGJO(2)) THEN
        FBEGJO(2) = .FALSE.
C       if output banks exist drop them
        CALL BDROP (IW,'IHIT')
C       create  bank IHIT
        CALL ALBOS ('IHIT',0,LMHLEN+LIHIT*MIHIT,JIHIT,IGARB)
        IW(JIHIT+1) = LIHIT
C
C--     Create noise hits with track id=0.
        CALL ITNOIS
C
C--     Initialise summary counters for current event.
        NTCUIT = 0
      ENDIF
C
C--  Ensure we only deal with finite length charged track steps.
C--
      IF (ITRKEL(8).NE.0 .AND. TRKELE(11).LT.STPMN) GO TO 9999
      IF (NINT(TRKELE(14)).EQ.0) GO TO 9999
      SLENG = TRKNXT(9) - TRKELE(9)
      IF (SLENG.LT.STPMN) GO TO 9999
C
C--  Transform coordinates in TRKELE,TRKNXT to local ITC coordinates
C--  for misaligned chamber.
C--
      NTCUIT = NTCUIT + 1
      CALL ITALIG(TRKELE,VEC1)
      CALL ITALIG(TRKNXT,VEC2)
      IF (FDEBJO.AND.ICITJO(4).EQ.1) THEN
        WRITE(LOUTIO,'(A,I5,A,6F15.7)') ' Trk. ',ITRKEL(1),' TRKELE=',
     +  (TRKELE(I),I=1,6)
        WRITE(LOUTIO,'(A,I5,A,6F15.7)') ' Trk. ',ITRKEL(1),' TRKNXT=',
     +  (TRKNXT(I),I=1,6)
        WRITE(LOUTIO,'(A,I5,A,6F15.7)') ' Trk. ',ITRKEL(1),'   VEC1=',
     +  (VEC1(I),I=1,6)
        WRITE(LOUTIO,'(A,I5,A,6F15.7)') ' Trk. ',ITRKEL(1),'   VEC2=',
     +  (VEC2(I),I=1,6)
      ENDIF
C
C-- Find mid point of track element using cubic spline formula and
C-- compute parabola coefficients for R(u).
C--
      CALL ISPLIN(0.5, VEC1, VEC2, SLENG, VP)
      R0 = SQRT(VEC1(1)**2 + VEC1(2)**2)
      R1 = SQRT(VEC2(1)**2 + VEC2(2)**2)
      RP = SQRT(VP(1)**2 + VP(2)**2)
      AA = 2.*(R0+R1) - 4.*RP
      BB = 4.*RP - 3.*R0 - R1
      CC = R0
C
C--  Compute all intersections of the track element with cell boundaries
C--  by solving the quadratic approximation for R(u).
C--
      IL = 0
      DO 100 LAY=1,NLAY
        CH2 = 0.5*CELHIT(LAY)
        RL1 = RWIRIT(LAY) - CH2
        RL2 = RWIRIT(LAY) + CH2
C
C--     Check if track element starts or ends within a wire layer.
C--
        IF (R0.GE.RL1 .AND. R0.LE.RL2) THEN
          IL = IL + 1
          SS(IL) = 0.
          LLAY(IL) = LAY
        ENDIF
        IF (R1.GE.RL1 .AND. R1.LE.RL2) THEN
          IL = IL + 1
          SS(IL) = 1.
          LLAY(IL) = LAY
        ENDIF
C
C--     Solve R(u). Ensure solution is not degenerate.
C--
        DO 50 J=-1,1,2
          RWANT = RWIRIT(LAY) + J*CH2
          CR = CC - RWANT
          IF (ABS(AA).LT.AATOL) THEN
C
C--         Straight line degeneracy.
C--
            IF (ABS(BB).GT.BBTOL) THEN
              U = -CR/BB
              DRDU = 2.*AA*U + BB
              IF (ORD(0.,U,1.)) THEN
                CALL IRUCHK(RWANT, U, DRDU)
                IL = IL + 1
                SS(IL) = U
                LLAY(IL) = J*LAY
              ENDIF
            ELSE
C
C--         R(u) degenerates to a point: already dealt with.
C--
              GO TO 100
            ENDIF
          ELSE
            ARG = BB**2 - 4.*AA*CR
            IF (ARG.GT.0.) THEN
              U = (-BB + SQRT(ARG))/(2.*AA)
              DRDU = 2.*AA*U + BB
              IF (ORD(0.,U,1.)) THEN
                CALL IRUCHK(RWANT, U, DRDU)
                IL = IL + 1
                SS(IL) = U
                LLAY(IL) = J*LAY
              ENDIF
              U = (-BB - SQRT(ARG))/(2.*AA)
              DRDU = 2.*AA*U + BB
              IF (ORD(0.,U,1.)) THEN
                CALL IRUCHK(RWANT, U, DRDU)
                IL = IL + 1
                SS(IL) = U
                LLAY(IL) = J*LAY
              ENDIF
            ELSE IF (ARG.EQ.0.) THEN
              U = -BB/(2.*AA)
              DRDU = 2.*AA*U + BB
              IF (ORD(0.,U,1.)) THEN
                CALL IRUCHK(RWANT, U, DRDU)
                IL = IL + 1
                SS(IL) = U
                LLAY(IL) = J*LAY
              ENDIF
            ENDIF
          ENDIF
   50   CONTINUE
  100 CONTINUE
C
C--  Order the intersections by increasing path length along track.
C--
      INDX(1) = 1
      IF (IL.GT.1) CALL SORTZV(SS,INDX,IL,1,0,0)
C
C--  Loop over all intersections and compute hits in each layer.
C--
      DO 130 K=1,IL-1
        I = INDX(K)
        I1 = INDX(K+1)
        LAY = ABS(LLAY(I))
        IF (ABS(LLAY(I)).NE.ABS(LLAY(I1))) GO TO 130
C
C--     Test whether track element lies inside layer boundaries using
C--     the mid-point.
C--
        U = 0.5*(SS(I)+SS(I1))
        CALL ISPLIO(U, VP)
        RP = SQRT(VP(1)**2 + VP(2)**2)
        CH2 = 0.5*CELHIT(LAY)
        CW2 = 0.5*CELWIT(LAY)
        IF ((LLAY(I)+LLAY(I1).NE.0) .AND.
     +      (ABS(RP-RWIRIT(LAY)).GT.CH2)) GO TO 130
C
C--     Find coordinates of start and end of segment crossing layer.
C--
        CALL ISPLIO(SS(I), VA)
        CALL ISPLIO(SS(I1), VB)
C
C--     Keep halving the segment length until it is within tolerance
C--     for treating as a series of straight sections.
C--
        NS = 1
  110   SAG2 = (0.5*(VA(1)+VB(1))-VP(1))**2 +
     +         (0.5*(VA(2)+VB(2))-VP(2))**2 +
     +         (0.5*(VA(3)+VB(3))-VP(3))**2
        IF (SAG2.GT.SGTL2) THEN
          VB(1) = VP(1)
          VB(2) = VP(2)
          VB(3) = VP(3)
          U = 0.5*(SS(I)+U)
          NS = 2*NS
          CALL ISPLIO(U, VP)
          GO TO 110
        ENDIF
        USTEP = (SS(I1)-SS(I))/NS
        IF (USTEP.LT.STPMN) GO TO 130
C
C--     Loop over straight sections within layer to find hits.
C--
        DO 120 J=1,NS
          PHI1 = ATAN2(VA(2),VA(1))
          IF (PHI1.LT.0.) PHI1 = PHI1 + TWOPI
          U = SS(I) + J*USTEP
          CALL ISPLIO(U, VB)
C
C--       Compute phi range of segment and number of wires spanned.
C--       Ensure at least two wires are tested adjacent to hit position.
C--
          PHI2 = ATAN2(VB(2),VB(1))
          IF (PHI2.LT.0.) PHI2 = PHI2 + TWOPI
          DELFI = AUARCM(PHI2-PHI1)
          NWR = NWIRIT(LAY)*ABS(DELFI)/TWOPI + 1
          IF (DELFI.LT.0.) NWR = -NWR
          PHI = PHI1 - PHWRIT(LAY)
          NW = NINT(PHI*NWIRIT(LAY)/TWOPI + 0.5)
          IF (NWR.LT.0) NW = NW + 1
          IF (NW.GT.NWIRIT(LAY)) NW = 1
          IF (NW.LT.1)           NW = NWIRIT(LAY)
C
C--       Loop over all candidate wires which may be hit by segment.
C--       For each wire find closest approach to track, allowing for
C--       wire sag. Calculate drift distance and angle within cell.
C--
          DO 115 IWI=NW,NW+NWR,SIGN(1,NWR)
            PHIW = (IWI - 1)*TWOPI/NWIRIT(LAY) + PHWRIT(LAY)
            XWI = RWIRIT(LAY)*COS(PHIW)
            YWI = RWIRIT(LAY)*SIN(PHIW)
            CALL IAPROX(XWI, YWI, VA, VB, VP)
            IF (WSAGIT.GT.0.0) THEN
              YWI = YWI - WSAGIT*(1.- (VP(3)/WZMXIT)**2)
              CALL IAPROX(XWI, YWI, VA, VB, VP)
            ENDIF
            PHIW = ATAN2(YWI, XWI)
            IF (PHIW.LT.0.) PHIW = PHIW + TWOPI
            DRIF = SQRT((VP(1)-XWI)**2 + (VP(2)-YWI)**2)
C
C--         Compute angle of normal to track from sense wire in
C--         standard orientation of cell.  There is an ambiguity of PI
C--         if the track passes through the sense wire.
C--
            IF (DRIF.LT.STPMN) THEN
              DANG = ATAN2(VB(2)-VA(2), VB(1)-VA(1))
              IF (DANG.LT.0.) DANG = DANG + TWOPI
              DANG = DANG - PHIW
            ELSE
              DANG = ATAN2(VP(2)-YWI, VP(1)-XWI)
              IF (DANG.LT.0.) DANG = DANG + TWOPI
              DANG = DANG + PIBY2 - PHIW
            ENDIF
            IF (DANG.LT.0.) DANG = DANG + TWOPI
C
C--         Is this hit within the cell boundaries ?
C--
            IF (ABS(VP(3)).GT.WZMXIT) GO TO 115
            IF (ABS(DRIF*COS(DANG)).GT.CW2) GO TO 115
            IF (ABS(DRIF*SIN(DANG)).GT.CH2) GO TO 115
            NWA = IWI
            IF (NWA.LT.1)           NWA = NWA + NWIRIT(LAY)
            IF (NWA.GT.NWIRIT(LAY)) NWA = NWA - NWIRIT(LAY)
            NWA = NWA + IWIRIT(LAY)
C
C--         Interpolate exact value of U, hence get TOF at hit.
C--
            DPB = (VP(1)-VB(1))**2 + (VP(2)-VB(2))**2 + (VP(3)-VB(3))**2
            DAB = (VA(1)-VB(1))**2 + (VA(2)-VB(2))**2 + (VA(3)-VB(3))**2
            UHIT = U - SQRT(DPB/DAB)*USTEP
            TOF1 = TRKELE(10) * 1.E9
            TOF2 = TRKNXT(10) * 1.E9
            TOF  = (TOF2 - TOF1)*UHIT + TOF1
            CTHET = (VB(3) - VA(3))/SQRT(DAB)
            IF (ABS(CTHET).GT.1.) CTHET = SIGN(1.,CTHET)
            THETA = ACOS(CTHET)
            ITNO = ITRKEL(1)
            IF (ITRKEL(2).NE.0) ITNO = -ITNO
C
C--         Store hit info, expanding bank if necessary.
C--
            JIHIT = IW(NAIHIT)
            IF (LFRWRD(JIHIT).LT.LIHIT) THEN
              ND = IW(JIHIT) + IW(JIHIT)/2
              CALL ALBOS('IHIT',0,ND,JIHIT,IGARB)
            ENDIF
C
C--         Check whether last hit was on the same wire for the same
C--         track.  If so, overwrite the old hit if the new one is
C--         closer to the wire, overwise discard the new one.
C--
            LAST = LROWS(JIHIT)
            IF (LAST.GT.0) THEN
              JLAST = KROW(JIHIT,LAST)
              ITL  = IW(JLAST+1)
              NWL  = IW(JLAST+3)
              DRFL = RW(JLAST+4)
            ELSE
              ITL = 0
              NWL = 0
            ENDIF
            IF (NWA.NE.NWL .OR. ITNO.NE.ITL) THEN
              JNEXT = KNEXT(JIHIT)
              IW(JIHIT+LMHROW) = IW(JIHIT+LMHROW) + 1
            ELSE IF (DRIF.LT.DRFL) THEN
              JNEXT = JLAST
            ELSE
              GO TO 115
            ENDIF
C
C--         Copy the data for this hit into the bank.
C--
            IW(JNEXT+1) = ITNO
            IW(JNEXT+2) = LAY
            IW(JNEXT+3) = NWA
            RW(JNEXT+4) = DRIF
            RW(JNEXT+5) = DANG
            RW(JNEXT+6) = VP(3)
            RW(JNEXT+7) = THETA
            RW(JNEXT+8) = TOF
  115     CONTINUE
          VA(1) = VB(1)
          VA(2) = VB(2)
          VA(3) = VB(3)
  120   CONTINUE
  130 CONTINUE
C
 9999 CONTINUE
      END
