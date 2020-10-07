C**************************************************************************
C
C     the input variable LDBAS indicating the logical unit of the database
C     to be accessed has to be initialized by the JUNIDB function from
C     ALEPHLIB and is passed on to the geometry read-in subroutines (which
C     require that argument for the calls of ERDDAF and ALGTDB) via the
C     following common block Dali_Graphics_Data_Base_Logical_Unit
C
      COMMON / DGDBLU / LDBSDB,IRUNDB
C#######################################################################
C
C
C
C GENERAL PARAMETERS
C
      PARAMETER ( PI = 3.141592653589 )
      PARAMETER ( RADDEG = 180./PI , DEGRAD = PI/180. )
 
C
C
C     This is the common declaration for passing the geometry data
C     from the read-in routines to the drawing rountines
C

C#######################################################################
C
C  VDET
C
C----------------------------------------------------------------------
C
C   IMPORTANT: there is a further include file ( DGCOMN_VRDDAF.INC )
C              that contains the COMMON-blocks needed for accessing
C              the results computed by VRDDAF!
C              Should these commons be changed, the COMMON-DECK
C              VDGEOS has to be extracted from ALEPHLIB anew.
C
C------------------------------------------------------------------------
C
C  VDET-geometry common block Dali_VDet_GEOmetry
C
C  VDET has 2 layers
C
C  CRILDV   corner-coordinates of inner layer ( wafer considered as flat !)
C  CROLDV   corner-coordinates of outer layer
C  NSLIDV   number of slots of inner layer
C  NSLODV   number of slots of outer layer
C  NWAFDV   number of silicon-chip-wafers per slot ( 4 ! )
C  TIILDV   tilts of inner-layer modules ( degree ! )
C  TIOLDV   tilts of outer-layer modules ( degree ! )
C  RAILDV   radius of inner-layer
C  RAOLDV   radius of outer-layer
C  DFILDV   displacement in phi for inner-layer-modules
C  DFOLDV   displacement in phi for outer-layer-modules
C  PALWDV   r-phi active length (=strip-length) of wafer ( type 1)
C  ZALWDV   z active length (=strip-length) of wafer ( type 1)
C  ZOFWDV   z-offset of wafers ( 4 wafer placed in a module )
C  NWTSDV   wafer type in slot ( 0 = not occupied, 1 = Munich, 2-4 = Pisa)
C  RMNIDV   minimum radius of inner-layer active-area wafer corner
C  RMXIDV
C  RMNODV
c  RMXODV
C  DELXDV,DELYDV,DELZDV,DELPDV .. alignment parameters
C
      COMMON /DVDGEO/ NSLIDV,NSLODV,PALWDV,ZALWDV,
     &                TIILDV(15),TIOLDV(15),
     &                RMNIDV,RMXIDV,RMNODV,RMXODV,
     &                DELXDV,DELYDV,DELZDV,DELPDV,
     &                ZOFWDV(4), NWTSDV(15,2),
     &                RAILDV,RAOLDV,DFILDV(12),DFOLDV(15),
     &                CRILDV(2,2,15),CROLDV(2,2,15)
C#######################################################################
C
C  ITC
C
C----------------------------------------------------------------------
C
C  ITC-geometry common block Dali_ITc_GEOmetry
C
C  RMAXDI   outer radius
C  RMINDI   innner radius
C  ZMAXDI   half of z-length
C
      COMMON /DITGEO/ RMAXDI,RMINDI,ZMAXDI
C#######################################################################
C
C  ECAL
C
C----------------------------------------------------------------------
C
C  ECAL-geometry common block Dali_ECal_GEOmetry
C
C  CORADE   corner-coordinates of endcap z>0
C  CORBDE   corner-coordinates of barrel
C  CORCDE   corner-coordinates of endcap z<0
C  ETILDE   tilt of ECAL
C  POLADE   polar-angles phi&theta of endcap(z>0)corners
C  POLBDE   polar-angles phi&theta of barrel corners
C  POLCDE   polar-angles phi&theta of endcap(z<0)corners
C  RMNBDE   radius-minimum of barrel
C  RMXBDE   radius-maximum of barrel
C  RICBDE   radius of inner corner of barrel modules
C  ROCBDE   radius of outer corner of barrel modules
C  NEBAMD   number of barrel moduls
C  NEECMD   number of end-cap moduls
C
      PARAMETER (NEBAMD=12,NEECMD=12)
      COMMON /DECGEO/ CORADE(3,12,NEECMD),CORBDE(3,8,NEBAMD),
     &                CORCDE(3,12,NEECMD),ETILDE ,
     &                POLADE(2,12,NEECMD),POLBDE(2,8,NEBAMD),
     &                POLCDE(2,12,NEECMD),
     &                RMNBDE ,RMXBDE ,RICBDE ,ROCBDE,
     &                RMNEDE ,RMXEDE ,RICEDE ,RMCEDE ,ROCEDE   
C#######################################################################
C
C  LCAL
C
C-----------------------------------------------------------------------
C
C  LCAL-geometry common block Dali_LCal_GEOmetry
C
C  ZDISDL   z-distance from 0
C  RINNDL   inner radius of LCAL
C  ROUTDL   outer radius of LCAL
C  ZLENDL   z-length of LCAL
C
      COMMON /DLCGEO/ ZDISDL,RINNDL,ROUTDL,ZLENDL
C#######################################################################
C
C  SATR
C
C-----------------------------------------------------------------------
C
C  SATR-GEOMETRY common block Dali_SAtr_GEOmetry
C
C  ZDISDS   z-distance from 0 ( explanation see DGRSAT )
C  RINNDS   inner radius of SATR (sensitive !)
C  ROUTDS   outer radius of SATR (sensitive !)
C  ZLENDS   z-length of SATR
C
      COMMON /DSAGEO/ ZDISDS,RINNDS,ROUTDS,ZLENDS
C#######################################################################
C
C  TPC  
C
C-----------------------------------------------------------------------
C
C  TPC-geometry common block     Dali_TPc_GEOmetry
C
C  CORNDT   coordinates of cornerpoints of of all sectors of 
C           the 3 sector-types
C  POLCDT   polar coordinates (radius,phi) of the 3 sector-types
C           ( these are the same for all sectors of one type ! )
C  FISTDT   starting-value for phi for the 3 sector-types
C  RMAXDT   maximum radius of TPC sensitive volume
C  RMINDT   minimum radius of TPC
C  ZMAXDT   maximum z-length of TPC
C
C  NTYPDT   max number of types (3)
C  NSLTDT   number of slots for each type (6)
C  NCORDT   number of corners for each of the types
C
C  remember:
C    max number of corners    : 10 (2*LTCORN - 2)
C    max number of slots      :  6 (LTSLOT/2 )  ( 1 dummy slot )
C    max number of slot-types :  3 ( M, W, K )
C
      COMMON /DTPGEO/ FISTDT(3),
     &       CORNDT (2,10,6,3), POLCDT ( 2,10,3 ),
     &       CRNGDT (2,10,6,3),
     &       NTYPDT,NCORDT(3),NSLTDT,
     &       RMAXDT , RMINDT , ZMAXDT
C#######################################################################
C
C  HCAL
C
C-----------------------------------------------------------------------
C
C  HCAL-geometry common block Dali_HCal_GEOmetry
C
C  CORADH   corner-coordinates of endcap z>0 
C  CORBDH   corner-coordinates of barrel 
C  CORCDH   corner-coordinates of endcap z<0
C
C  NHBAMD   number of barrel moduls
C  NHECMD   number of endcap moduls
C
      PARAMETER (NHBAMD=12,NHECMD=6)
      COMMON /DHCGEO/ CORADH(3,16,NHECMD),CORBDH(3,8,NHBAMD),
     &                CORCDH(3,16,NHECMD), 
     &                POLADH(2,16,NHECMD),POLBDH(2,8,NHBAMD),
     &                POLCDH(2,16,NHECMD),
     &                RMNBDH,RMXBDH,RICBDH,ROCBDH,
     &                RMNEDH,RMXEDH,RMXIDH,RICEDH,RMCEDH,ROCEDH
C######################################################################
C
C  MUON-DEDECTOR-geometry common blocks
C
C==============================================
C
C  MUON-BARREL-geometry common block Dali_Barrel_Muon_GEOmetry
C
C       NSLTDB   number of slots in barrel
C       NSLIDB   number of slots in inner layer
C       NSLODB   number of slots in outer layer
C       NMTPDB   number of different barrel-module types
C       NMTIDB   number of different inner layer barrel-module types
C       NMTODB   number of different outer layer barrel-module types
C       NCMXDB   maximum number of corners of those types
C
      PARAMETER ( NSLTDB = 34 , NSLIDB = 17 , NSLODB = 17 )
      PARAMETER ( NMTPDB = 10 , NMTIDB = 4  , NMTODB = 6  )
      PARAMETER ( NCMXDB = 16 )
C
C       ITYPDB    module TYPe in given slot
C       NCORDB    number of corners for given type
C       CORNDB    xyz-corner coordinates for modules in slots 
C       RMNIDB    Radius MiNimum of Inner barrel layer
C       RMXIDB    Radius MaXimum of Inner barrel layer
C       RMNODB    Radius MiNimum of Outer barrel layer
C       RMXODB    Radius MaXimum of Outer barrel layer
C

      COMMON /DBMGEO/  ITYPDB ( NSLTDB ) , NCORDB ( NMTPDB ) ,
     &                 CORNDB ( 3, NCMXDB, NSLTDB ),
     &                 RMNIDB, RMXIDB, RMNODB, RMXODB
C===============================================
C
C  MUON-MIDDEL-ANGLE-geometry common block Dali_Middle-angle_Muon_GEOmetry
C
C  ATTENTION : NCMXDM has to be changed due to notch in mod-type 4
C              which is not yet accounted for !
C
C       NSLTDM  number of slots in middle-angle
C       NSLHDM  number of slots of subcomp. ( z>0 or z<0 )
C       NSLIDM  number of slots in inner layer
C       NSLODM  number of slots in outer layer
C       NMTPDM  number of module types in middle-angle
C       NCMXDM  maximum number of corners of those types
C
      PARAMETER ( NSLTDM = 38 )
      PARAMETER ( NSLHDM = 19 )
      PARAMETER ( NSLIDM = 10 , NSLODM = 9 )
      PARAMETER ( NMTPDM = 9  )
      PARAMETER ( NCMXDM = 8  )
C
C       ITYPDM    module TYPe in given slot
C       NCORDM    number of corners for given type
C       CORNDM    xyz-corner coordinates for modules in slots 
C       RMNIDM    Radius MiNimum of Inner middle angle layer
C       RMXIDM    Radius MaXimum of Inner middle angle layer
C       RMNODM    Radius MiNimum of Outer middle angle layer
C       RMXODM    Radius MaXimum of Outer middle angle layer
C

      COMMON /DMMGEO/  ITYPDM ( NSLTDM ) , NCORDM ( NMTPDM ) ,
     &                 CORNDM ( 3, NCMXDM , NSLTDM ),
     &                 RMNIDM, RMXIDM, RMNODM, RMXODM
C================================================
C
C  MUON-END-CAP-geometry common block Dali_End-cap_Muon_GEOmetry
C
C  ATTENTION : the muon end-caps are here considered to be rectangles!
C              Later on, the irregular shapes of the end-caps should be
C              taken into account!
C
C       NSLTDE  number of slots in end-caps
C       NSLHDE  number of slots of subcomp. ( z>0 or z<0 )
C       NSLIDE  number of slots in inner layer
C       NSLODE  number of slots in outer layer
C       NMTPDE  number of module types in end-cap
C       NCMXDE  maximum number of corners of those types
C
      PARAMETER ( NSLTDE = 16 )
      PARAMETER ( NSLHDE =  8 )
      PARAMETER ( NSLIDE =  4 , NSLODE =  4 )
      PARAMETER ( NMTPDE =  8 )
      PARAMETER ( NCMXDE =  8 )
C
C       ITYPDE    module TYPe in given slot
C       NCORDE    number of corners for given type
C       CORNDE    xyz-corner coordinates for modules in slots 
C       RMNIDE    Radius MiNimum of Inner end cap layer
C       RMXIDE    Radius MaXimum of Inner end cap layer
C       RMNODE    Radius MiNimum of Outer end cap layer
C       RMXODE    Radius MaXimum of Outer end cap layer
C      
      COMMON /DEMGEO/  ITYPDE ( NSLTDE ) , NCORDE ( NMTPDE ) ,
     &                 CORNDE ( 3, NCMXDE , NSLTDE ),
     &                 RMNIDE, RMXIDE, RMNODE, RMXODE

