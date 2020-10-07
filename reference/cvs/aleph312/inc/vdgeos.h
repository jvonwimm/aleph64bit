C! read-out geometry constants for VDET.
      INTEGER NSLOM,NWAFN,NWAFM,NGEOM,LVDCC,LVDNC,LVDL,NSLOI
C
      INTEGER NDIVZV,NDIVPV,NCERVZ
      INTEGER NSTPVD,NSTZVD,NVDPPI,NVDZPI,IOPZVD
      INTEGER NSLOCO,NSLOGM,NSLOME,NSLOWA,NSLOEL,NGEOWA
      INTEGER NVDLAY,NVDSLG,NVDMEC,NVDWAF,IPSIGN,IZSIGN
      INTEGER NSLOAD
C
      REAL VDWLEN,VDPPIT,VDZPIT,VDDIPP,VDDIPZ,VDDIZP,VDDIZZ
      REAL VDCRHO,ACTPVD,ACTZVD,VDTHCK,VDPSDM,VDBXTH,VDZOFF
      REAL VDPOFF,VDCPHI,VDTILT,ZWAFVD,VDAPPL,VDDEPL,VDLESP,VDLESZ
      REAL VDSTPH,VDSTZE,VDCEZO,VDCCCP,VDCCCZ,VDNCCP,VDNCCZ
C
      PARAMETER (LVDL=2, LVDCC=7, LVDNC=3)
      PARAMETER (NSLOM=15, NSLOI=12, NWAFN=4,NWAFM=4, NGEOM=4)
      COMMON /VDGEOS/ NDIVZV(NGEOM) , NDIVPV(LVDL) , NCERVZ(NGEOM),
     &                VDWLEN(2,NWAFM), VDPPIT(NWAFM) , VDZPIT(NWAFM),
     &                VDDIPP(NWAFM) , VDDIPZ(NWAFM) , VDDIZP(NWAFM),
     &                VDDIZZ(NWAFM), VDCRHO(NSLOM,LVDL), NSTPVD(NWAFM),
     &                NSTZVD(NWAFM) , ACTPVD(NWAFM) , ACTZVD(NWAFM) ,
     &                VDTHCK(NWAFM) , VDPSDM(NGEOM) ,  VDBXTH(NGEOM) ,
     &                VDZOFF(NWAFN,NGEOM) , VDPOFF(NSLOM,LVDL) ,
     &                VDCPHI(NSLOM,LVDL) , VDTILT(NSLOM,LVDL) ,
     &                ZWAFVD(NWAFN,NSLOM,LVDL), VDAPPL(NWAFM),
     &                VDDEPL(NWAFM) , VDLESP(NWAFM) , VDLESZ (NWAFM),
     &                VDSTPH(NWAFM) , VDSTZE(NWAFM) , NVDPPI(NWAFM),
     &                NVDZPI(NWAFM), VDCEZO(NWAFM,NGEOM), IOPZVD(LVDL),
     &                VDCCCP(NWAFM,0:LVDCC), VDCCCZ(NWAFM,0:LVDCC),
     &                VDNCCP(NWAFM,0:LVDNC), VDNCCZ(NWAFM,0:LVDNC),
     &                NSLOCO(NSLOM,LVDL) , NSLOGM(NSLOM,LVDL) ,
     &                NSLOME(NSLOM,LVDL) , NSLOWA(NSLOM,LVDL) ,
     &                NSLOEL(NSLOM,LVDL) , NGEOWA(NGEOM) ,
     &                NVDLAY, NVDSLG, NVDMEC, NVDWAF ,
     &                IPSIGN(4,2) , IZSIGN(4,2) , NSLOAD(NSLOM,LVDL)
C
#if defined(DOC)
    LVDL = Number of layers in Minivertex (2)

    As a rule, the letter P in the name means that the r-phi strips
    are concerned. The letter Z refer instead to zed strips.
    For the following variables there may be different values for the
    two VDET layers

    Variables essential to Geant description of VDET
    ================================================
    VDCRHO(I)  : Distance between the origin and a side center
    NDIVPV (I) : Number of sides along phi for I-th layer
    VDTHCK (I) : Thickness of a Silicon Crystal
    VDTILT(I,J): Tilt angle for i-th face of j-th layer
    VDTILT (I,J) : Angle between the normal to the I side of the J layer
                 with the line going through its centre, 0 being defined
                 as the position in which the plane of the side is
                 normal to VDAPLN direction
    VDPSDM (I) : r-phi dimension of a side
    VDZSDM (I) : zed dimension of a side
    VDPOFF (I,J) : phi angle of of the normal to the I-th side of
                   the J-th layer
    VDCPHI(I,J): Phi angle of vdcrho for I-th side of J-th layer
    VDCEZO(I,J): Z offset of I-th ceramic in a face of J-th layer
    IOPZVD (I) : Orientation of the i-th layer
    NDIVZV (I) : Number of crystals along Z in one side
    NCERVZ(I)  : Number of ceramics along z in a face
    VDWLEN(2,I): Wafer lengths along the phi and z edges
    GAPLNV (I) : Gap along Z between crystal edges of same side
    VDZOFF (I,J) : Z offset of I-th crystal centre in a side
                   of the J-th layer geometry

    Variables needed by the digitization phase
    ==========================================
    VDPPIT (I) : Strip read-out pitch in r-phi direction
    VDPWID (I) : Strip width in r-phi direction
    VDZPIT (I) : Strip read-out pitch in Z direction
    VDZWID (I) : Strip width in Z direction
    NSTPVD (I) : Number of r-phi strips in a crystal
    NSTZVD (I) : Number of Z strips in a crystal
    VDDIPP (I) : Distance of r-phi active zone to crystal edge in r-phi
    VDDIPZ (I) : Distance of r-phi active zone to crystal edge in Z
    VDDIZP (I) : Distance of Z active zone to crystal edge in r-phi
    VDDIZZ (I) : Distance of Z active zone to crystal edge in Z
    ACTPVD (I) : r-phi active length of wafer
    ACTZVD (I) : Z     active length of wafer
    VDAPPL (I) : Applied bias voltage
    VDDEPL (I) : Depletion bias voltage
    VDLESP (I) : Strip length on the r-phi side
    VDLESZ (I) : Strip length on the z side
    VDSTPH (I) : Strip phisical pitch in r-phi direction
    VDSTZE (I) : Strip phisical pitch in in z direction
    NVDPPI (I) : Strip read out frequency in r-phi direction
    NVDZPI (I) : Strip read out frequency in z direction
    VDCCCP(I,J): J-th capacitive coupling constants on the phi side
    VDCCCZ(I,J): J-th capacitive coupling constants on the zed side
    VDNCCP(I,J): J-th noise coupling constants on the phi side
    VDNCCZ(I,J): J-th noise coupling constants on the zed side
    NSLOCO (I) : Component number to which the i-th slot refers
    NSLOGM (I) : Geometry type number to which the i-th slot refers
    NSLOME (I) : Mechanical type number to which the i-th slot refers
    NSLOWA (I) : Wafer type number to which the i-th slot refers
    NSLOEL (I) : Electronics type number to which the i-th slot refers
    NGEOWA (I) : Wafer number corresponding to the i-th geometry
    NSLOAD (I) : Phi slot number in galeph referring to the i-th address
    ZWAFVD (I,J,K) : Minimum Z coordinate of the active region of the
                   I-th wafer of the J-th side in the K-th layer

    All variables are either directly taken or calculated from
    geometry banks as read from Data Base D/A file
#endif
