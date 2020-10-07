CD ECGN
C   Modification 14/03/90
      INTEGER ECALID,ECALLS,ECALLP,ECALSC,ECALSY,ECALMD,ECALPL,ECALST
      INTEGER ECALCL,ECALRG,ECALSS,ECALRW,ECALXW,ECALEC,ECALXG,ECALNP
      INTEGER ECALPR,ECALAL,ECALC1,ECALC2,ECALM1,ECALM2
      PARAMETER (ECALLS=4,  ECALLP=2,  ECALSC=3, ECALSY=2,ECALMD=12)
      PARAMETER (ECALPL=45, ECALST=3,  ECALCL=84,ECALRG=4,ECALSS=2 )
      PARAMETER (ECALRW=218,ECALXW=228,ECALEC=2, ECALXG=7,ECALNP=8 )
      REAL ECALBL,ECALBO,ECALEI,ECALEW,ECALEL,ECALGP,ECALWS,ECALAP
      REAL ECALDM,ECALTI
      COMMON/ECALCC/ ECALID,ECALPR,ECALBL,ECALBO,ECALEI,ECALEW,ECALEL,
     & ECALGP,ECALWS,ECALAP,ECALAL,ECALDM(ECALLS,ECALLS),ECALTI,
     & ECALC1,ECALC2,ECALM1,ECALM2
#if defined(DOC)
 The electromagnetic parameters and general constants.
   ECALID   Id of the electromagnetic calorimeter
   ECALLS   Dimension of projective space
   ECALLP   Dimension of plane pencil
   ECALSC   Number of subcomponents
   ECALSY   Number of subcomponent types
   ECALMD   Number of modules in a subcomponent
   ECALPL   Number of planes in a module
   ECALST   Number of stacks
   ECALCL   Size of the column table
   ECALRG   Number of regions
   ECALSS   Number of sectors in a petal
   ECALRW   Number of rows
   ECALXW   Number of row addresses
   ECALEC   Number of end caps
   ECALXG   Number of extended regions
   ECALNP   Maximum number of planes limiting a module
   ECALPR   Number of petal rows not overlapping with the barrel
   ECALBL   Length of the sensitive region in the barrel
   ECALBO   Distance of the first barrel plane to the origin
   ECALEI   Inner radius (on the flat) of the end-cap
   ECALEW   Mean distance of first 2 wire planes of an end cap to the
            origin.
   ECALEL   Distance of the 44th wire plane of an endcap to the origin
   ECALGP   Size of the gap
   ECALWS   Wire spacing
   ECALAP   Angular pitch of the modules
   ECALAL   The electromagnetic calorimeter is referenced in Aleph
   ECALDM   The displacement matrix corresponding to the tilt.
   ECALTI   Tilt of the calorimeter against Aleph
   ECALC1   The first reference plane for column definition
   ECALC2   The second reference plane for column definition
   ECALM1   The first reference plane for module definition
   ECALM2   The second reference plane for module definition
#endif
CD EALI
      INTEGER NEALI
      PARAMETER (NEALI=ECALMD*ECALSC)
      INTEGER EALIID
      REAL EALIAM,EALITD,EALIMP,EALIPL,EALISE,EALICL,EALIRW,EALILP
      COMMON/EALICC/EALIID(ECALMD,ECALSC),
     &              EALIAM(ECALLS,ECALLS,ECALMD,ECALSC),
     &              EALITD(ECALLS,ECALLS,ECALMD,ECALSC),
     &              EALIMP(ECALLS-1,   4,ECALMD,ECALSC),
     &              EALIPL(ECALLS,ECALLP,ECALMD,ECALSC),
     &              EALISE(ECALLS,ECALMD,ECALSC),
     &              EALICL(ECALLS,ECALLP,ECALMD,ECALSC),
     &              EALIRW(ECALLS,ECALLP,ECALSS,ECALMD,ECALSC),
     &              EALILP(ECALLS,ECALNP,ECALMD,ECALSC)
#if defined(DOC)
   Alignment and total displacement for each module in its slot.
    EALIID Identification
    EALIAM Alignment matrix built from relpos and relangle
    EALITD Product of the displacement matrices to go from the
           module types to the actual modules in their slots.
    EALIMP Position of the mires for each slot
    EALIPL References for the planes and stacks
    EALISE Planes separating the sectors.
    EALICL References for the columns.
    EALIRW reference planes for the module rows
    EALILP Planes limiting the sensitive zone.
#endif
CD EBPL
C   Modification 14/03/90
      INTEGER NEBPL
      PARAMETER (NEBPL=7)
      INTEGER EBPLID,EBPLVL
      REAL EBPLLF
      COMMON/EBPLCC/EBPLID(NEBPL),EBPLVL(2,NEBPL),EBPLLF(ECALLS,NEBPL)
#if defined(DOC)
  Base planes for the definition of the pencils in the module.
  We have NEBPL such entities with the following
   EBPLID  Identification
   EBPLVL  Validity limits
   EBPLLF  Base planes ( linear forms)
#endif
CD ECMT
      INTEGER NECMT
      PARAMETER (NECMT=161)
      REAL ECMTMP
      INTEGER ECMTID,ECMTCP,ECMTEL
      COMMON/ECMTCC/ECMTID(NECMT),ECMTMP(NECMT),ECMTCP(NECMT),
     &             ECMTEL(NECMT)
#if defined(DOC)
  Entified relationship .
  The relationship volume to the matter it is made of.
  We have NECMT such entities with the following
    ECMTID Identification of the relationship
    ECMTMP The proportion of this matter entering in this volume
    ECMTCP Relation to the compound material (EMAT)
    ECMTEL Relation to EMAT. A compound material is made of
                     different elements
#endif
CD ECNC
      INTEGER NECNC
      PARAMETER (NECNC=40)
      INTEGER ECNCID,ECNCEC,ECNCEQ,ECNCET
      COMMON/ECNCCC/ECNCID(NECNC),ECNCEC(NECNC),ECNCEQ(NECNC),
     &              ECNCET(NECNC)
#if defined(DOC)
  Relationship contour of volume.
  We have NECNC such entities with the following
   ECNCID   Identification
   ECNCEC   The corresponding corner
   ECNCEQ   The point of the contour
   ECNCET   Type of the volume
#endif
CD ECOL
      INTEGER ECOLID,ECOLER
      REAL ECOLPC
      COMMON/ECOLCC/ECOLID(ECALCL),ECOLPC(ECALLP,ECALCL),ECOLER(ECALCL)
#if defined(DOC)
  the table defining the positions of a module column'
  We have ECALCL such entities with the following
   ECOLID   Identification
   ECOLPC   the pencil coordinates of columns (built)
   ECOLER   the elements of the column table associated to a region
#endif
CD ECRP
      INTEGER NECRP
      PARAMETER (NECRP=20)
      INTEGER ECRPID,ECRPEC,ECRPET,ECRPP1,ECRPP2,ECRPP3
      COMMON/ECRPCC/ECRPID(NECRP),ECRPEC(NECRP),ECRPET(NECRP),
     &             ECRPP1(NECRP),ECRPP2(NECRP),ECRPP3(NECRP)
#if defined(DOC)
 The relationship planes to corners per volume type
  We have NECRP such entities with the following
    ECRPID Identification of the relationship
    ECRPEC Relation to corner
    ECRPET Relation to volume type
    ECRPP1 Relation to first plane
    ECRPP2 Relation to second plane
    ECRPP3 Relation to third plane
#endif
CD EECB
      INTEGER EECBID
      REAL EECBDM
      COMMON/EECBCC/EECBID,EECBDM(ECALLS,ECALLS)
#if defined(DOC)
   This table contains the symmetry around the Y axis transforming
    the end cap A module type in End cap B.
  EECBID  Identification
  EECBDM  matrix(4,4)
#endif
CD EFAC
      INTEGER NEFAC
      PARAMETER (NEFAC=60)
      INTEGER EFACID,EFACEA,EFACEC,EFACEF,EFACET
      COMMON/EFACCC/EFACID(NEFAC),EFACEA(NEFAC),EFACEC(NEFAC),
     &              EFACEF(NEFAC),EFACET(NEFAC)
#if defined(DOC)
  Entity faces of a volume.
  We have NEFAC such entities with the follow
   EFACID   Identification
   EFACEA   The plane of the face.
   EFACEC   Relationship to corners. The corners defining the face.
   EFACEF   Relationship to the GKS list
   EFACET   the type of the volume .
#endif
CD ELNF
      INTEGER NELNF
      PARAMETER (NELNF=93)
      INTEGER ELNFID
      CHARACTER*16 ELNFNM
      REAL ELNFLF
      COMMON/ELNFCC/ELNFID(NELNF),ELNFLF(ECALLS,NELNF)
      COMMON/ELNFCH/ELNFNM(NELNF)
#if defined(DOC)
  Entity linear forms of Ecal.
  The list of planes used to describe the geometry
  We have NELNF such entities with the following information
    ELNFID Identification of the linear form
    ELNFNM Name of the linear form
    ELNFLF Linear form coefficients
#endif
CD ELOC
       INTEGER ELOCID,ELOCEM,ELOCES
       REAL ELOCPL,ELOCCL,ELOCRW,ELOCLP,ELOCSE
       COMMON/ELOCCC/ELOCID,ELOCPL(ECALLS,ECALLP),
     & ELOCCL(ECALLS,ECALLP),ELOCRW(ECALLS,ECALLP,ECALSS),
     & ELOCLP(ECALLS,ECALNP),ELOCSE(ECALLS),ELOCEM,ELOCES
#if defined(DOC)
   The local system    (built)
  ELOCID  Identification
  ELOCPL  reference for the planes and stack
  ELOCCL  reference for the columns
  ELOCRW  reference for the rows
  ELOCLP  the limiting planes of the volume in the local system
  ELOCSE  reference for the sectors
  ELOCEM  the local module
  ELOCES  the local subcomponent
#endif
CD ELTY
      INTEGER NELTY
      PARAMETER (NELTY=14)
      INTEGER ELTYID,ELTYEA,ELTYEF,ELTYET
      COMMON/ELTYCC/ELTYID(NELTY),ELTYEA(NELTY),ELTYEF(NELTY),
     &              ELTYET(NELTY)
#if defined(DOC)
   The structure of the Fill Area Set
  We have NELTY such entities with the following
     ELTYID  Identification of the type
     ELTYEA  The face in the FAS
     ELTYEF  Beginning of the face in the FAS
     ELTYET  The type of the volume.
#endif
CD EMAT
      INTEGER NEMAT
      PARAMETER (NEMAT=86)
      INTEGER EMATID,EMATMI
      CHARACTER*16 EMATMN
      REAL EMATAW,EMATAN,EMATDE,EMATRL,EMATAL
      COMMON/EMATCC/EMATID(NEMAT),EMATMI(NEMAT),EMATAW(NEMAT),
     &   EMATAN(NEMAT),EMATDE(NEMAT),EMATRL(NEMAT),EMATAL(NEMAT)
      COMMON/EMATCH/EMATMN(NEMAT)
#if defined(DOC)
   The materials used in Aleph. They may be elements or compounds.
The compounds may be chemicals or averages of heterogeneous matters.
They are then described by the proportion of their elements
accessible by the ECMT entity.
  We have NEMAT such entities with the following
     EMATID  Identification
     EMATMI  Material index
     EMATMN  Material Name
     EMATAW  Atomic weight
     EMATAN  Atomic number
     EMATDE  Density
     EMATRL  Radiation length
     EMATAL  Absorption length
#endif
CD EMOD
      INTEGER EMODID
      REAL EMODDM,EMODPC,EMODRW
      COMMON/EMODCC/EMODID(ECALMD),EMODDM(ECALLS,ECALLS,ECALMD),
     &      EMODPC(ECALLP,ECALMD)
#if defined(DOC)
     The modules in a subcomponent
     EMODID   Identification
     EMODDM   The rotation to place a module in a slot(built)
     EMODPC   The pencil coordinates of modules (built)
#endif
CD EPHY
      INTEGER NEPHY,LEPHY
      PARAMETER (NEPHY=38)
      INTEGER EPHYID,EPHYEQ,EPHYES,EPHYVL
      CHARACTER*16 EPHYNM
      REAL EPHYLE,EPHYAN
      COMMON/EPHYCC/LEPHY,EPHYID(NEPHY),EPHYEQ(NEPHY),EPHYES(NEPHY),
     &              EPHYVL(2,NEPHY),EPHYLE(ECALLS-1,NEPHY),
     &              EPHYAN(ECALLS-1,NEPHY)
      COMMON/EPHYCH/EPHYNM(NEPHY)
#if defined(DOC)
   The physical modules.
  We can have NEPHY such entities (actually LEPHY) with the following
     EPHYID  Identification
     EPHYNM  The name of the physical module
     EPHYEQ  The module type
     EPHYES  The corresponding slot
     EPHYVL  The validity limit
     EPHYLE  The relative position of the module with respect to its
               theoretical position.
     EPHYAN  The relative angles of the module with respect to its
               theoretical position.
#endif
CD EPLN
      INTEGER EPLNID,EPLNES,EPLNPI,EPLNPS
      COMMON/EPLNCC/EPLNID(ECALPL),EPLNES(ECALPL),EPLNPI(ECALPL),
     & EPLNPS(ECALPL)
#if defined(DOC)
   The planes of a module.
   We have ECALPL such entities with the following
     EPLNID  Identification of the plane
     EPLNES  There are 10,23,12 planes in the different stacks
     EPLNPI  The pathological row close to subcomponent 1 in that plane
     EPLNPS  The pathological row close to subcomponent 3 in that plane
#endif
CD EPSC
      INTEGER EPSCID,EPSCEP,EPSCES
      REAL EPSCPC
      COMMON/EPSCCC/EPSCID(ECALPL+1,ECALSC),
     &              EPSCPC(ECALLP,ECALPL+1,ECALSC),
     &              EPSCEP(ECALPL+1,ECALSC),EPSCES(ECALPL+1,ECALSC)
#if defined(DOC)
   The planes per subcomponent
   EPSCID   Identification
   EPSCPC   Local (pencil)coordinates (built)
   EPSCEP   each plane may belong to the 3 subcomponents
   EPSCES   are the local coordinates different in the different SCs?
#endif
CD EPTY
      INTEGER EPTYID,EPTYNB,EPTYEP,EPTYEQ
      REAL EPTYFS
      COMMON/EPTCC/EPTYID(ECALPL,ECALSY),EPTYNB(ECALPL,ECALSY),
     &EPTYFS(ECALPL,ECALSY),EPTYEP(ECALPL,ECALSY),EPTYEQ(ECALPL,ECALSY)
#if defined(DOC)
      The planes per subcomponent type. Tube (wire) information
      EPTYID Identification
      EPTYNB The number of tubes per plane in a given SC type.
      EPTYFS the offset of tubes relative to a reference plane
      EPTYEP each plane has two types
      EPTYEQ the tubes are different in the different module types
#endif
CD EQTY
      INTEGER NEQTY
      PARAMETER (NEQTY=2)
      INTEGER EQTYID,EQTYMN
      CHARACTER*16 EQTYNM
      REAL EQTYTP
      COMMON/EQTYCC/EQTYID(NEQTY),EQTYMN(NEQTY),EQTYTP(ECALLS-1,4,NEQTY)
      COMMON/EQTYCH/EQTYNM(NEQTY)
#if defined(DOC)
   The different types of modules
   We have NEQTY such entities with the following
     EQTYID  Identification of the type
     EQTYNM  Name of the type (Barrel or End-Cap)
     EQTYMN  Number of mires on a module for the alignment
     EQTYTP  theoretical positions of the mires.
#endif
CD EREG
      INTEGER EREGID,EREGCN,EREGFR,EREGLS
      COMMON/EREGCC/EREGID(ECALRG),EREGCN(ECALRG),EREGFR(ECALRG),
     &             EREGLS(ECALRG)
#if defined(DOC)
   the regions where the number of columns is constant
     EREGID  Identification
     EREGCN  Number of columns per region
     EREGFR  the first column associated to a region
     EREGLS  the last column associated to a region
#endif
CD EROW
C   Modification 14/03/90
      INTEGER EROWID,EROWEX
      COMMON/EROWCC/EROWID(ECALRW+1),EROWEX(ECALRW+1)
#if defined(DOC)
   the table defining the physical rows
    EROWID Identification
    EROWEX the rows in an extended (geographical) region
#endif
CD ESCO
C   Modification 14/03/90
      INTEGER ESCOID,ESCOCF,ESCORF,ESCORL,ESCOEC,ESCOEQ,ESCOET
      INTEGER ESCOFR,ESCOLS,ESCOS1,ESCOS2,ESCORR
      CHARACTER*16 ESCONM
      REAL ESCOLI,ESCOMD
      COMMON/ESCOCC/ESCOID(ECALSC),ESCOLI(ECALLS,ECALSC),
     &ESCOMD(ECALLS,ECALLP,ECALSC),ESCOCF(ECALSC),ESCORF(ECALSC),
     &ESCORL(ECALSC),ESCOEC(ECALSC),ESCOEQ(ECALSC),ESCOET(ECALSC),
     &ESCOFR(ECALSC),ESCOLS(ECALSC),ESCOS1(ECALSC),ESCOS2(ECALSC),
     &ESCORR(ECALLP,ECALSC)
      COMMON/ESCOCH/ESCONM(ECALSC)
#if defined(DOC)
  The three subcomponents:endcap A, barrel, endcap B
  We have ECALSC such entities with the following
     ESCOID  Identification of the subcomponent
     ESCONM  Name of the subcomponent (Barrel , End-Cap A and B)
     ESCOLI  Planes delimiting the subcomponents
     ESCOMD  Reference planes for modules
     ESCOCF  Column offset
     ESCORF  Row offset
     ESCORL  Row overlap
     ESCOEC  There are three subcomponents in the electrmgn calrmtr
     ESCOEQ  Each subcomponent has a module-type
     ESCOET  Each subcomponent is barrel or end-cap
     ESCOFR  the first row of a given subcomponent in EXROPC
     ESCOLS  the last row of a given subcomponent in EXROPC
     ESCOS1  The first reference plane for stack/plane definition
     ESCOS2  The second reference plane for stack/plane definition
     ESCORR  The reference planes used for row definition.
#endif
CD ESEC
      INTEGER ESECID,ESECSR,ESECEQ
      REAL ESECDM
      COMMON/ESECCC/ESECID(ECALSS),ESECSR(ECALSS),ESECEQ(ECALSS),
     &              ESECDM(ECALLS,ECALLS,ECALSS)
#if defined(DOC)
   the two sectors of an endcap petal
  We have ECALSS such entities with the following
     ESECID  Identification
     ESECSR  Sign of the rotation associated to the sector
     ESECEQ  There are two sectors in the petals, they are defined by
     ESECDM   a rotation respective to the module frame.
#endif
CD ESLO
      INTEGER ESLOID,ESLOEA,ESLOEM,ESLOEQ,ESLOES
      REAL ESLOPS,ESLOAG
      CHARACTER*16 ESLONM
      COMMON/ESLOCC/ESLOID(ECALMD,ECALSC),
     &              ESLOPS(ECALLS-1,ECALMD,ECALSC),
     &              ESLOAG(ECALLS-1,ECALMD,ECALSC),
     &              ESLOEA(ECALMD,ECALSC),ESLOEM(ECALMD,ECALSC),
     &              ESLOEQ(ECALMD,ECALSC),ESLOES(ECALMD,ECALSC)
      COMMON/ESLOCH/ESLONM(ECALMD,ECALSC)
#if defined(DOC)
     The slot of a calorimeter module
  We have ECALMD*ECALSC such entities with the following
     ESLOID  Identification
     ESLONM  name of the slot
     ESLOPS  Position of the barycentre of the slot
                relative to the theoretical position
     ESLOAG  angles of the actual module with
                respect to its theoretical position
     ESLOEA  we attach to a slot an alignment matrix (bijection)
     ESLOEM  a slot is among the 12 of a subcomponent
     ESLOEQ  the slots have a module type
     ESLOES  the slots are in different subcomponents
#endif
CD ESSC
      INTEGER ESSCID,ESSCES,ESSCST
      REAL ESSCPP,ESSCPC,ESSCPS
      COMMON/ESSCCC/ESSCID(ECALST,ECALSC),ESSCPP(ECALLS,ECALST,ECALSC),
     & ESSCPC(ECALLP,ECALST,ECALSC),ESSCPS(ECALLP,ECALST,ECALSC),
     & ESSCES(ECALST,ECALSC),ESSCST(ECALST,ECALSC)
#if defined(DOC)
   the stacks per subcomponent
   We have ECALST*ECALSC such entities with the following
     ESSCID  Identification of the stack
     ESSCPP  plane pitches (built)
     ESSCPC  pencil coord for stacks first planes (built)
     ESSCPS  pencil coord for stacks last planes (built)
     ESSCES  each subcomponent contains 3 stacks
     ESSCST  each stack can be in 3 subcomponents
#endif
CD ESTK
      INTEGER ESTKID,ESTKFR,ESTKLS
      COMMON/ESTKCC/ESTKID(ECALST),ESTKFR(ECALST),ESTKLS(ECALST)
#if defined(DOC)
   The 3 stacks of a module
   We have ECALST such entities with the following
     ESTKID  Identification of the stack
     ESTKFR  The number of the first plane of a stack. (built)
     ESTKLS  The number of the last plane of a stack. (built)
#endif
CD ESTY
      INTEGER ESTYID,ESTYEQ,ESTYES
      REAL ESTYWF,ESTYPF
      COMMON/ESTYCC/ESTYID(ECALST,NEQTY),
     & ESTYWF(ECALLS,ECALST,NEQTY),ESTYPF(ECALLS,ECALST,NEQTY),
     & ESTYEQ(ECALST,NEQTY),ESTYES(ECALST,NEQTY)
#if defined(DOC)
   The 3 stacks of a module
   We have ECALST such entities with the following
     ESTYID  Identification of the stack
     ESTYWF  wire planes offset
     ESTYPF  planes offset against the stacks
     ESTYEQ  the stacks in the two module types are different
     ESTYES  each stack has two types according to the module type
#endif
CD ETSC
      INTEGER ETSCID,ETSCEB,ETSCEC
      REAL    ETSCDM,ETSCAF
      CHARACTER*16 ETSCNM
      COMMON/ETSCCC/ETSCID(ECALSY),ETSCEB(ECALSY),ETSCEC(ECALSY),
     & ETSCDM(ECALLS,ECALLS,ECALSY),ETSCAF(ECALSY)
      COMMON/ETSCCH/ETSCNM(ECALSY)
#if defined(DOC)
     the types of subcomponents: barrel or end-cap
   We have ECALSY such entities with the following
    ETSCID  Identification
    ETSCNM  Name
    ETSCEB  The separation plane between two sectors in the EC
    ETSCEC  There are two subcomponent types in the calorimeter,
    ETSCDM  Displacement matrix
    ETSCAF  They have a certain position relative to ECAL
#endif
CD ETYV
      INTEGER NETYV
      PARAMETER (NETYV=2)
      INTEGER ETYVID,ETYVFC,ETYVCR,ETYVNG,ETYVFT,ETYVFR,ETYVFF,
     &        ETYVLT,ETYVLR,ETYVLF
      COMMON/ETYVCC/ETYVID(NETYV),ETYVFC(NETYV),ETYVCR(NETYV),
     &              ETYVNG(NETYV),ETYVFT(NETYV),ETYVFR(NETYV),
     &ETYVFF(NETYV),ETYVLT(NETYV),ETYVLR(NETYV),ETYVLF(NETYV)
#if defined(DOC)
   The different types of convex volumes we use
   We have NETYV such entities with the following
     ETYVID  Identification of the type
     ETYVFC  Number of faces
     ETYVCR  Number of corners
     ETYVNG  Number of edges
     ETYVFT  gives the beginning of the relationship for the type
             inverted from ECNC -> ETYV
     ETYVFR  gives the beginning of the relationship for the type
             inverted from ECRP -> ETYV
     ETYVFF  gives the beginning of the relationship for the type
             inverted from EFAC -> ETYV
     ETYVLT  gives the end of the relationship for the type
             inverted from ECNC -> ETYV
     ETYVLR  gives the end of the relationship for the type
             inverted from ECRP -> ETYV
     ETYVLF  gives the end of the relationship for the type
             inverted from EFAC -> ETYV
#endif
CD EVLF
      INTEGER NEVLF
      PARAMETER (NEVLF=292)
      REAL EVLFSG
      INTEGER EVLFID,EVLFEA,EVLFEL,EVLFEV
      COMMON/EVLFCC/EVLFID(NEVLF),EVLFSG(NEVLF),EVLFEA(NEVLF),
     &             EVLFEL(NEVLF),EVLFEV(NEVLF)
#if defined(DOC)
  Entified relationship of Ecal.
  The relationship volume to its limiting planes
  We have NEVLF such entities with the following
    EVLFID Identification of the relationship
    EVLFSG Sign to be applied to the linear form to get the interior
           of the volume positive.
    EVLFEA the order of the planes
    EVLFEL Relation to ELNF
    EVLFEV Relation to EVOL
#endif
CD EVLS
      INTEGER EVLSID,EVLSVL,EVLSZN
      CHARACTER*16 EVLSNM
      COMMON/EVLSCC/EVLSID(ECALNP,ECALSC),
     &             EVLSVL(ECALNP,ECALSC),EVLSZN(ECALNP,ECALSC)
      COMMON/EVLSCH/EVLSNM(ECALNP,ECALSC)
#if defined(DOC)
  Describes the space external to a sensitive volume
  We have ECALNP such entities per subcomponent with the following
    EVLFID Identification
    EVLFNM name of the zone
    EVLFVL zone sign
    EVLFZN zone number
#endif
CD EVOL
      INTEGER NEVOL
      PARAMETER (NEVOL=43)
      INTEGER EVOLID,EVOLEM,EVOLEQ,EVOLET,EVOLFR,EVOLLS
      CHARACTER*16 EVOLNM
      COMMON/EVOLCC/EVOLID(NEVOL),EVOLEM(NEVOL),EVOLEQ(NEVOL),
     &              EVOLET(NEVOL),EVOLFR(NEVOL),EVOLLS(NEVOL)
      COMMON/EVOLCH/EVOLNM(NEVOL)
#if defined(DOC)
  Entity volume of Ecal.  The different tracking volumes
  We have NEVOL such entities with the following
   EVOLID   Identification
   EVOLNM   Name of the volume
   EVOLEM   The material associated to the volume
   EVOLEQ   The type of the module
   EVOLET   Type of the volume
   EVOLFR   The first plane associated to the volume
   EVOLLS   The last plane associated to the volume
#endif
CD EXRG
C   Modification 14/03/90
      INTEGER EXRGID,EXRGER,EXRGFR,EXRGLS
      REAL    EXRGPC
      COMMON/EXRGCC/EXRGID(ECALXG+1),EXRGPC(ECALLP,ECALXG+1),
     & EXRGER(ECALXG+1),EXRGFR(ECALXG+1),EXRGLS(ECALXG+1)
#if defined(DOC)
 Extended regions to take into account the symetric regions.
  We have ECALXG such entities with the following
   EXRGID  Identification
   EXRGPC  the pencil coordinates for extended regions (built)
   EXRGER  the extended (geographical) region to the region
   EXRGFR  the first row of an extended (geographical) region
                     inverted from EXRO -> EXRG
   EXRGLS  the last row of an extended (geographical) region
                     inverted from EXRO -> EXRG
#endif
CD EXRO
C   Modification 14/03/90
      INTEGER EXROID,EXROER,EXROES
      REAL EXROPC
      COMMON/EXROCC/EXROID(ECALXW),EXROER(ECALXW),EXROES(ECALXW),
     & EXROPC(ECALLP,ECALXW+3)
#if defined(DOC)
   the table defining the software rows
  We have ECALXW such entities with the following
    EXROID   Identification
    EXROER   correspondance between the software numbering of rows
                        and the physical one
    EXROES   the row indices belonging to a given subcomponent
    EXROPC   the pencil coordinates of the rows (built)
#endif
