      SUBROUTINE ASKTRK
C ----------------------------------------------------------------------
C - B.Bloch-Devaux - 850315                       F.Ranjard - 860929
C! Store new tracks:
C    - in the kinematic banks if Pmod > 1.Gev  or Pmod > 0.1Gev in the
C      inner part of the apparatus.
C    - other tracks are stored in the stack.
C. - called from    GUSTEP                            from this .HLB
C. - calls          GSVERT, GSKINE, GSKING            from GEANT3 pam
C.                  IUCOMP                            from KERNLIB
C.
C -----------------------------------------------------
      SAVE
      COMMON/GCTMED/NGUMED,NGATME(5),IGSVOL,IGFIEL,GFIELD,GTMXFD
     +      ,GDMXMS,GDEEMX,GEPSIL,GSTMIN,GCFIEL,GCMUL
     +      ,IGUPD,IGSTPA,NGUMOL
C
      COMMON/GCKINE/IGKINE,GPKINE(10),IGTRA,IGSTAK,IGVERT,IGPART,IGTRTY
     +              ,NGAPAR(5),GMASS,GCHARG,GTLIFE,GVERT(3),GPVERT(4)
     +              ,IGPAOL
C
      COMMON/GCTRAK/GVECT(7),GETOT,GEKIN,GVOUT(7),NGMEC,LGMEC(30)
     &,NGAMEC(30),NGSTEP,MGXNST,GDESTP,GDESTL,GSAFET,GSLENG,GSTEP,GSNEXT
     &,GSFIEL,GTOFG,GEKRAT,GUPWGH,IGNEXT,IGNWVO,IGSTOP,IGAUTO,IGEKBI
     &,IGLOSL,IGMULL,IGNGOT,NGLDOW,NGLVIN,NGLVSA,IGSTRY
C
      PARAMETER (LGKINE=100)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON /GCVOLU/ NGLEVE,NGAMES(15),NGUMBR(15),
     &  LGVOLU(15),LGINDX(15),IGNFRO,NGLVMX,NGLDEV(15),LGINMX(15),
     &  GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      PARAMETER (NOMOR=3)
      REAL GADD(3)
      LOGICAL  UPTPC,UPECA,EMPA,HADPA,MUPA,PRIMA,TINO,BREMP
      EXTERNAL IUCOMP
      CHARACTER*4 TCASE,CHAINT,CHAHOL
C - define central region and beam tube region
      DATA KCDET,KBTUB/4HCDET,4HBTUB/
C - define ITC endplate, TPC endplate and TPC outer wall
      DATA KITND,KTPEP,KTPWO /4HITND,4HTPEP,4HTPWO/
C - define TPC region
      DATA KTPCR / 4HTPCR/
C - particles w/ momentum below 40.Mev up to the TPC, and 100Mev
C   in the TPC are kept in STAK bank
C - brem or dray which take more than 15% of the incident particle
C   provoke the stopping of the incident particle and the creation
C   of a new one.
C - muons which are daughters of primaries are kept as primaries.
      DATA PERC /0.15/, CUTPC /0.1/, CUTIT /0.04/
C - Statment functions
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
C                      to define e.m. particles, had. part. or muon part
      TINO(I) = INT(GKIN(5,I)).GE.LTYPEL .AND. INT(GKIN(5,I)).LE.LTYPGA
      EMPA(I) = INT(GKIN(5,I)).LE.3
      MUPA(I) = INT(GKIN(5,I)).EQ.5 .OR. INT(GKIN(5,I)).EQ.6
      HADPA(I)= INT(GKIN(5,I)).EQ.4 .OR. INT(GKIN(5,I)).GT.6
C - define region up to TPC
      UPTPC   = (NGAMES(2).EQ.KBTUB) .OR.
     &          (NGAMES(2).EQ.KCDET .AND. NGAMES(3).NE.KTPCR .AND.
     &          NGAMES(4).NE.KITND)
C - define region up to ECAL
      UPECA   = UPTPC .OR. (NGAMES(3).EQ.KTPCR .AND. NGAMES(4).NE.KTPEP
     &                .AND. NGAMES(4).NE.KTPWO)
C ----------------------------------------------------------------------
C
C   IF there are secondaries THEN
C      IF a primary has created secondaries w/o loosing its identity
C         (BREM or DRAY) and the secondaries take more than 15% of
C         the enery of the primary THEN
C            stop the current particle
C            create a new particle w/ the residual energy
C      ENDIF
C      store the secondaries:
C      a particle is a primary IF its mother is a primary
C                             AND its momentum is above 40Mev up to TPC
C                              OR its momentum is above 100Mev up to ECA
C                             AND its origin vertex is up to the outer
C                                 TPC wall but the TPC and ITC endplate
C      store primaries in KINE and VERT banks and others in the STAK
C   ENDIF
C
      NVTX = 0
      IF (NGKINE .GT. 0) THEN
        TCASE = CHAHOL (KGCASE)
        BREMP = IGSTAK.EQ.0 .AND. UPECA. AND. GVECT(7).GT.CUTPC
     &           .AND.(TCASE.EQ.'BREM' .OR. TCASE.EQ.'DRAY')
     &           .AND.NGKINE.LT.LGKINE
        IF (BREMP) THEN
          PSUM = 0.
          DO 10 I=1,NGKINE
            PSUM = PSUM+SQRT(GKIN(1,I)**2+GKIN(2,I)**2+GKIN(3,I)**2)
   10     CONTINUE
          IF (PSUM .GT. PERC*GVECT(7)) THEN
            IGSTOP = NOMOR
            NGKINE = NGKINE+1
            DO 11 IK=1,3
              GKIN(IK,NGKINE) = GVECT(3+IK)*GVECT(7)
   11       CONTINUE
            GKIN(4,NGKINE) = SQRT(GMASS**2+GKIN(1,NGKINE)**2+
     &                            GKIN(2,NGKINE)**2+GKIN(3,NGKINE)**2)
            GKIN(5,NGKINE) = REAL(IGPART)
          ENDIF
        ENDIF
        DO 30 I=1,NGKINE
C        store particle# I either in the STACK or in KINE banks
          PMOD  = SQRT (GKIN(1,I)**2+GKIN(2,I)**2+GKIN(3,I)**2)
          PRIMA = ((IGSTAK.EQ.0).AND.( (UPTPC.AND.(PMOD.GT.CUTIT))
     &                 .OR.(UPECA.AND.(PMOD.GT.CUTPC)).OR.MUPA(I)) )
     &                 .OR. TINO(I)
          IF (PRIMA) THEN
C           primary daughter AND
C               ( muon OR
C                 part. with Pmod>0.04Gev up to TPC  OR
C                 part. with Pmod>0.1Gev up to ECAL )
C           OR geantino
C           are stored in KINE and VERT banks
            IGFLGK(I) = 1
            NVTX = 1
          ELSE
C           other particles are in stack
            IGFLGK(I) = 0
          ENDIF
   30   CONTINUE
        CALL GSKING (0)
      ENDIF
C
C   IF  a charged primary stopped and no vertex was stored yet THEN
C      store an end vertex ==> NVTX>0
C   ENDIF
C
      IF ((GCHARG.NE.0..AND.IGSTOP.NE.0.AND.IGSTAK.EQ.0)
     &     .AND. NVTX.EQ.0) THEN
        CALL GSVERT(GVECT,IGTRA,0,0,0, NVTX)
        IF(NVTX.LE.0) GO TO 999
      ENDIF
C
C   IF a new vertex has been stored THEN
C      store vertex volume name in KVOL bank
C   ENDIF
C
      IF (NVTX .GT.0) THEN
        JKVOL = IW(NAKVOL)
        IF (LFRROW(JKVOL).LT.1) THEN
          CALL ALBOS ('KVOL',0,IW(JKVOL)*2,JKVOL,IGARB)
        ENDIF
        IW(KNEXT(JKVOL)+JKVOVN) = INTCHA(TRKVOL)
        IW(KNEXT(JKVOL)+JKVOVM) = INTCHA(TCASE)
        IW(JKVOL+LMHROW) = LROWS(JKVOL) + 1
      ENDIF
C
C - end
  999 CONTINUE
      RETURN
      END
