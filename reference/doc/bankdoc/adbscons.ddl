 SUBSCHEMA ADBSGEOM
 :'Contains the definitions of the entity sets and relations which
   are general for the whole ALEPH Detector Description System (ADDS)'
 
 
 AUTHOR   'A. Putzer,F.Ranjard'
 REVIEWER 'F.Loverre'
 VERSION  '3.0'
 DATE     '13/04/99'
 
 DEFINE ESET
 
 ADBL
      :      'The ALEPH Long Term database.NR=0 (LTC)\
              Number of Columns\
              Number of rows=1'
        STATIC
 
        SIZE 1,1
 
      = (NumboftheVers   = INTE [100,10000]      : 'Database Version Number',
         ChangeofDate    = INTE [880000,991231]  : 'Date of last change')
      ;

 ADBN
      :      'Data base file name. NR=0 (LTC)\
              Number of words/file\
              Number of files'
        STATIC
     
      = (FirstVersion   = INTE [1,99999]        : 'First database version number',
         LastVersion    = INTE [1,99999]        : 'Last database version number',
         filename       = CHA8                  : 'data base filename')
      ;
 
 ADBR
      :      'first run of a period. NR=0 (LTC)\
              Number of columns\
              Number of periods'
        STATIC
 
      = (PeriodNumber    = INTE [8900,9912]      : 'Period number is yy*100+mm',
         FirstRun        = INTE [2001,999999]    : 'first run number of the period',
         BPipsetup       = INTE [1,20]           : 'BPIP setup number',
         VDetsetup       = INTE [1,20]           : 'VDET setup number',
         ITcsetup        = INTE [1,20]           : 'ITC setup number',  
         TPcsetup        = INTE [1,20]           : 'TPC  setup number',
         ECalsetup       = INTE [1,20]           : 'ECAL setup number',
         LCalsetup       = INTE [1,20]           : 'LCAL setup number',
         SAtrsetup       = INTE [1,20]           : 'SATR setup number',
         HCalsetup       = INTE [1,20]           : 'HCAL setup number',
         MUonsetup       = INTE [1,20]           : 'MUON setup number',
         TRiggersetup    = INTE [1,20]           : 'Trigger setup number',
         GEneralsetup    = INTE [1,20]           : 'General setup number',
         DBasesetup      = INTE [1,20]           : 'Dbase setup number',
         SIcalsetup      = INTE [1,20]           : 'SICAL setup number',
         BEamperiod      = INTE [1,20]           : 'Beam position and size period')
      ;
 
 ADBS
      :      'The ALEPH database system. NR=0 (STC)\
              Number of Columns\
              Number of rows=1'
        STATIC
 
        SIZE 1,1
 
      = (VersNumber   = INTE [100,10000]      : 'Database Version Number',
         DateofChange = INTE [880000,991231]  : 'Date of last change')
      ;
 
 
 ALPH
      :      'The ALEPH detector
              Constants characterizing the position of the
              ALEPH Reference System (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         NPos(3)      = Length [-30.,30.0]    : 'Aleph nominal position',
         NRot(3)      = Angle                 : 'Aleph nominal rotation')
      ;
 
 
 
 CTYP
      :      'A Detector Component
              Generalized entity set to control the branch
              to the different detector components (LTC)'
 
        SIZE 2,2
 
      = (ValRng(2))
      ;
 
 ATYP
      :      'A Detector Component
              Generalized entity set to control the branch
              to the different active detector components (LTC)'
 
        SIZE 8,8
 
      = (ValRng(2))
      ;
 
 
 
 END ESET
 
 DEFINE RSET

 
 (CTYP [1,1] -> [1,*] ALPH)
               INVERTED
             : 'ALEPH is composed of components'
             ;
 
 
 (CTYP [1,1] -> [1,1] ATYP|
             -> [1,1] PTYP
                      BY CmpTyp)
                INVERTED
             : 'The components are of different types'
             ;
 
 (ATYP [1,1] -> [1,1] ECGN|
             -> [1,1] HCAL|
             -> [1,1] ITCC|
             -> [1,1] LCAL|
             -> [1,1] MUOG|
             -> [1,1] SATR|
             -> [1,1] TCGD|
             -> [1,1] VCOD
                      BY CmpTyp)
             : 'The active components are of different types'
             ;
 
 
 END RSET
 
 
 END SUBSCHEMA
 

SUBSCHEMA ECALGEOM
:'Description of the geometry of the read-out of the electromagnetic
calorimeter'
 
AUTHOR 'H.Videau'
REVIEWER ' '
VERSION '3'
DATE     '12/07/89'
 
DEFINE ATTRIBUTE
 
   Card         = INTE [1,999999]
                : 'cardinality of a set.'
                ;
 
   DisplMatElem = REAL
                : 'element of a displacement matrix in the projective space'
                ;
 
 
   LinFormComp  = REAL [-2000.000000,2000.]
                : 'component of a linear form on the affine space.'
                ;
 
   Name16       = CH16
                : 'a name of 16 characters max'
                ;
 
   PenCoor      = REAL
                : 'pencil coordinate for a plane'
                ;
 
   Sign         = INTE -1|1
                : ' a sign'
                ;
 
   Vector       = REAL [-2000.000000,2000.]
                : 'components of a vector of the projective space.'
                ;
 
END ATTRIBUTE
 
DEFINE AGGREGATE
 
   RelativePosition =  (Length(3))
        : ' relative position of two objects'
        ;
 
   RelativeRotation = (Angle(3))
        : ' relative rotation of two objects. Euler angles.'
        ;
 
   DisplacementMat = (DisplMatElem(16))
        : ' Displacement matrix in projective space.'
        ;
 
END AGGREGATE
 
DEFINE ESET
 
   EALI
        : 'equivalent to ESLO. Contains all the built quantities when ESLO\
           contains the DB information (LTC) '
 
        SIZE 36,36
 
        =  (AlignMatrix(4,4)  =  DisplMatElem : 'The alignment matrix',
            TotalDispl(4,4)   =  DisplMatElem : 'The overall displacement\
                           from module type to actual module in its slot',
            MirePosit(3,4)    =  Vector       :'The position of the mires\
                              for each slot',
            PL(4,2)     = LinFormComp : 'reference planes for the module planes\
                                         and stacks structure ',
            SE(4)       = LinFormComp : 'separating planes for the sectors ',
            CL(4,2)     = LinFormComp : 'reference planes for the module columns
',
            RW(4,2,2)   = LinFormComp : 'reference planes for the module rows',
            LimitPlanes(4,8)  = LinFormComp : 'planes delimiting the\
                              sensitive zone in the modules')
        ;
 
   EBPL
        : 'the base planes for the definition of the pencils in the module\
                        type reference (LTC)'
        = (ValRng(2),
           LF(4) = LinFormComp : ' base plane')
        ;
 
   ECGN
        :  ' the electromagnetic calorimeter (LTC)'
        SIZE    1,1
        = (LS   = Card 4        : 'Dimension of projective space',
           LP   = Card 2        : 'Dimension of plane pencils',
           SC   = Card 3        : 'Number of subcomponents',
           SY   = Card 2        : 'Number of SC types',
           MD   = Card 12       : 'Number of modules in a subcomponent',
           PL   = Card 45       : 'Number of planes in a module',
           ST   = Card 3        : 'Number of stacks',
           CL   = Card 84       : 'Size of the column table',
           RG   = Card 4        : 'Number of region ',
           SS   = Card 2        : 'Number of sectors in a petal',
           RW   = Card 218      : 'Number of rows',
           XW   = Card 228      : 'Number of row adresses',
           EC   = Card 2        : 'Number of end caps',
           XG   = Card 7        : 'Number of extended regions',
           NP   = Card 8        : 'maximum number of planes limiting a module',
           PR   = Card 45       : 'Number of petal rows not overlapping\
                                        with the barrel',
           BL   = Length [460.,466.00] : 'Length of the sensitive region in \
                                        the barrel',
           BO   = Length [185.,190.00] : 'Distance of the first barrel plane \
                                        to the origin',
           EI   = Length [56.,57.00]   : 'Inner radius (on the flat) of the \
                                        end-cap',
           EW   = Length [254.,256.00] : ' Mean distance of first 2 wire \
                                        planes of an end cap to the origin.',
           EL   = Length [293.,296.00] : 'Distance of the 44th wire plane of \
                                        an endcap to the origin.',
           GP   = Length [.31,.3300]   : 'Size of the gap',
           WS   = Length [.48,.5200]   : ' wire spacing',
           AP   = Angle  [.50,.5500]   : ' angular pitch of the modules',
           AL   = Card  1              : ' for backward compatibility',
           DM(4,4) =  DisplMatElem : 'The position of Ecal in Aleph',
           TI   =Angle [-.033,-.0320]  : 'tilt of the calorimeter')
        ;
 
   ECOL
        : ' the table defining the positions of a module column.
             Has to be ordered by region (LTC)'
        SIZE    84,84
        = (PC(2) = PenCoor      : 'the pencil coordinates of columns (built)')
        ;
 
   EECB
        : ' The second end cap (LTC)'
        SIZE    1,1
        =  DisplacementMat
        ;
 
   ELOC
        : ' The local system (LTC)'
        SIZE    1,1
        =(PLanes (4,2)  =  LinFormComp :'reference for the planes and stacks',
          CoLumns(4,2)  =  LinFormComp :'reference for the columns',
          RoWs   (4,2,2)=  LinFormComp :'reference for the rows',
          SEctors(4)    =  LinFormComp :'reference for the sectors')
        ;
 
   EMIR
        : ' the survey targets (STC)'
        SIZE 100,200
        =(ValRng(2),
          NaMe                =CHA8           : 'name of the target',
          TheoricPosit(3)     =Length [-300.,300.00] : 'theoretical position\
                               ,computed or measured, of the target',
          TheoricError(3)     =Length [-300.,300.00] : 'error on the theoretical\
                                  position',
          TheoricFlag         =INTE   [0,8]          :'quality flag for the\
                                 theoretical position',
          ActualPosit(3)      =Length [-300.,300.00] :' surveyed position',
          ActualError(3)      =Length [-300.,300.00] :' error on the surveyed\
                                                        position',
          ActualFlag          =INTE   [0,8]          :'quality flag for the\
                                 surveyed position',
          TargetNumber        =INTE          : 'Number of the target')
        ;
 
   EMOD
        :  ' the modules in a subcomponent; contains the rotation to place \
                a module in a slot (built) (LTC)'
        SIZE    12,12
        =  DisplacementMat,
          (PC(2)     = PenCoor : 'the p+encil coordinates of modules (built)')
        ;
 
   EPHY
        :  'the physical calorimeter modules (LTC)'
        = (ValRng(2),
           NM = Name16)
        ;
 
   EPLN
        : ' the planes of a module (LTC)'
        SIZE    45,45
        ;
 
   EPSC
        : ' the planes per subcomponent (LTC)'
        = (PC(2)   = PenCoor    : 'the pencil coordinates for planes (built)')
        SIZE    135,135
        ;
 
   EPTY
        : ' The planes per subcomponent types. Tube (wire) information (LTC)'
        SIZE    90,90
        =( NB = INTE    : 'the number of tubes per plane in a given SCOM',
           FS = Length  : 'the offset of tubes relative to a reference plane')
        ;
 
   EQTY
        :  ' the types of modules (LTC)'
        SIZE    2,2
        = (NM          = Name16,
           MireNumber  = Card [3,4] : 'the number of mires',
           TheoPosit(3,4) = Length : 'the theoretical positions of
                                            the mires')
        ;
 
   EREG
        : ' the regions where the number of columns is constant (LTC)'
        SIZE    4,4
        = (CN    = INTE         : 'number of columns per region \
                                  inverted from ECOL -> EREG')
        ;
 
   EROW
        : ' the table defining the physical rows.
            Has to be ordered by extended region (LTC)'
        SIZE    218,219
        = (PC(2) = PenCoor      : 'the pencil coordinates of the rows (built)')
        ;
 
   ESCO
        :  ' the three subcomponents:endcap A, barrel, endcap B (LTC)'
        SIZE    3,3
        = (NM      = Name16,
           LI(4)   = LinFormComp  : 'planes delimiting the subcomponents',
           MD(4,2) = LinFormComp  : 'reference planes for modules',
           CF      = INTE         : ' column offset',
           RF      = INTE         : 'row offset',
           RL      = INTE         : 'row overlap')
        ;
 
   ESEC
        :  ' the two sectors of an endcap petal (LTC)'
        SIZE    2,2
        = (SR   = Sign : ' Sign of the rotation associated to the
                                sector.')
        ;
 
   ESLO
        :  'the slot of a calorimeter module (LTC)'
        SIZE    36,36
        = ( NM          = Name16  : 'name of the slot')
        ;
 
   ESSC
        : ' the stacks per subcomponent (LTC)'
        SIZE    9,9
        = (PP(4)        =Vector         : ' plane pitches',
           PC(2)   =PenCoor: ' pencil coord for stacks first planes (built)',
           PS(2)   =PenCoor: ' pencil coord for stacks last planes (built)')
        ;
 
   ESTK
        : 'the 3 stacks of a module (LTC)'
        SIZE    3,3
        ;
 
   ESTY
        : ' the stacks per module type (LTC)'
        SIZE    6,6
        = (WF(4)        =Vector         : ' wire planes offset',
           PF(4)        =Vector         : ' planes offset against the stacks??')
        ;
 
   ETSC
        : ' the types of subcomponents: barrel or end-cap (LTC)'
        SIZE    2,2
        = (NM   =Name16)
        ;
 
   EXRG
        : ' extended region to take into account the symetric regions (LTC)'
        SIZE    7,7
        = (PC(2) = PenCoor      : 'the pencil coordinates for extended regions\
                                   (built)')
        ;
 
   EXRO
        : ' the table defining the software rows (LTC)'
        SIZE    228,228
        ;
 
   END ESET
 
   DEFINE RSET
 
        (ESCO [1,1] -> [3,3] ECGN)
                : ' there are three subcomponents in the electrmgn calrmtr'
                ;
 
        (EMIR [0,1] -> [3,4] EPHY)
                : ' a physical module has three or four survey targets'
                ;
 
        (EPHY [0,1] -> [0,1] ESLO)
                : 'a physical module is put in a given slot, its real position\
                        is known as relative to its theoretical one'
                =  RelativePosition,
                   RelativeRotation
                ;
 
        (ETSC [1,1] -> [1,2] ECGN)
                : 'There are two subcomponent types in the calorimeter,\
                        the barrel and the end-caps. They have a certain\
                        position relative to electromagnetic calorimeter'
                =  DisplacementMat,
                  (AF   =Angle  :' angular offset of the subcomponent-types')
                ;
 
        (ETSC [1,1] -> [0,1] EBPL )
                : 'the separation plane between sectors of the end caps'
                ;
 
        (ELOC [1,1] -> [1,1] EMOD )
                : 'the local module '
                ;
 
        (ELOC [1,1] -> [1,1] ESCO )
                : 'the local subcomponent'
                ;
 
        (ECGN [1,1] -> [1,1] EBPL BY M1)
                : 'the first reference plane for module definition'
                ;
 
        (ECGN [1,1] -> [1,1] EBPL BY M2)
                : 'the second reference plane for module definition'
                ;
 
        (ECGN [1,1] -> [1,1] EBPL BY C1)
                : 'the first reference plane for column definition'
                ;
 
        (ECGN [1,1] -> [1,1] EBPL BY C2)
                : 'the second reference plane for column definition'
                ;
 
        (ECGN [1,1] -> [1,1] EBPL BY R1)
                : 'the first reference plane for row definition'
                ;
 
        (ECGN [1,1] -> [1,1] EBPL BY R2)
                : 'the second reference plane for row definition'
                ;
 
        (ESCO [1,1] -> [1,1] EBPL BY S1P)
                : 'the first reference plane for stack/plane definition'
                ;
 
        (ESCO [1,1] -> [1,1] EBPL BY S2P)
                : 'the second reference plane for stack/plane definition'
                ;
 
        (ESLO [1,1] -> [12,12] ESCO)
                : 'the slots are in different subcomponents'
                ;
 
        (ESEC [0,1] -> [1,2] EQTY)
                =  DisplacementMat
                : 'There are two sectors in the petals, they are defined by\
                a rotation respective to the module frame.'
                ;
 
        (ESCO [1,1] -> [1,2] EQTY)
                :  'each subcomponent has a module-type '
                ;
 
        (EPHY [1,1] -> [13,24] EQTY)
                : ' there are 13 or 24 physical modules of each type'
                ;
 
        (ESCO [1,1] -> [1,2] ETSC)
                : 'each subcomponent is barrel or end-cap'
                ;
 
        (ESTY [1,1] -> [3,3] EQTY)
                : 'the stacks in the two module types are different'
                ;
 
        (ESTY [1,1] -> [2,2] ESTK)
                : 'each stack has two types according to the module type'
                ;
 
 
        (ESSC [1,1] -> [3,3] ESTK BY STack)
                : 'each stack can be in 3 subcomponents'
                ;
 
        (ESSC [1,1] -> [3,3] ESCO)
                : 'each subcomponent contains 3 stacks'
                ;
 
        (ESLO [1,1] -> [12,24] EQTY)
                : 'the slots have a module type'
                ;
 
        (EPLN [1,1] -> [10,23] ESTK)
                : 'there are 10,23,12 planes in the different stacks'
                ;
 
        (ESTK [1,1] -> [1,1] EPLN BY FiRst)
                : 'The number of the first plane of a stack. (built\
                    as inverted from EPLN -> ESTK)'
                ;
 
        (ESTK [1,1] -> [1,1] EPLN BY LaSt)
                : 'The number of the last plane of a stack. (built
                    as inverted from EPLN -> ESTK)'
                ;
 
        (EPSC [1,1] -> [3,3] EPLN)
                : 'each plane may belong to the 3 subcomponents\
                    (implicit)'
                ;
 
        (EPSC [1,1] -> [45,45] ESCO)
                : 'are the local coordinates different in \
                   the different subcomponents? (implicit)'
                ;
 
        (EPTY [1,1] -> [2,2] EPLN)
                : 'each plane has two types'
                ;
 
        (EPTY [1,1] -> [45,45] EQTY)
                : 'the tubes are different in the different module types'
                ;
 
        (EXRO [1,1] -> [1,2] EROW)
                : 'correspondance between the software numbering of rows
                        and the physical one'
                ;
 
        (EPLN [1,1] -> [1,1] EXRO BY PathoInf)
                : 'The pathological row close to subcomponent 1 in that plane'
                ;
 
        (EPLN [1,1] -> [1,1] EXRO BY PathoSup)
                : 'The pathological row close to subcomponent 3 in that plane'
                ;
 
        (EXRO [1,1] -> [45,128] ESCO)
                : 'the row indices belonging to a given subcomponent'
                ;
 
        (ESCO [1,1] -> [1,1] EXRO BY FiRst)
                : 'the first row of a given subcomponent \
                   inverted from EXRO -> ESCO'
                ;
 
        (ESCO [1,1] -> [1,1] EXRO BY LaSt)
                : 'the last row of a given subcomponent \
                   inverted from EXRO -> ESCO'
                ;
 
        (ECOL [1,1] -> [9,33] EREG)
                : 'the elements of the column table associated to a region'
                ;
 
        (EREG [1,1] -> [1,1] ECOL BY FiRst)
                : 'the first column associated to a region\
                    inverted from ECOL -> EREG'
                ;
 
        (EREG [1,1] -> [1,1] ECOL BY LaSt)
                : 'the last column associated to a region\
                    inverted from ECOL -> EREG'
                ;
 
        (EXRG [1,1] -> [1,2] EREG)
                : 'the extended (geographical) region to the region '
                ;
 
        (EROW [1,1] -> [4,132] EXRG)
                : 'the rows in an extended (geographical) region'
                ;
 
        (EXRG [1,1] -> [1,1] EROW BY FiRst)
                : 'the first row of an extended (geographical) region\
                      inverted from EROW -> EXRG'
                ;
 
        (EXRG [1,1] -> [1,1] EROW BY LaSt)
                : 'the last row of an extended (geographical) region\
                      inverted from EROW -> EXRG'
                ;
 
        (ESLO [1,1] -> [3,3] EMOD)
                : ' a slot is among the 12 of a subcomponent'
                ;
 
        (ESLO [1,1] -> [1,1] EALI)
                :' we attach to a slot an alignment matrix'
                ;
 
END RSET
 
END SUBSCHEMA
 
 
SUBSCHEMA ECALVOLUMES
:' Description of the volumes used in the tracking geometry of the
electromagnetic calorimeter.
 Description of the structure of the volume types. The relationships
   can be created at initialisation in an automatic way'
 
AUTHOR 'H.Videau'
VERSION '1'
 
DEFINE ATTRIBUTE
 
   Type         = INTE [1,32]
                : 'type of an object.'
                ;
 
END ATTRIBUTE
 
DEFINE ESET
 
   EVOL
       : ' The different volumes (LTC)'
       =( NM         = Name16)
       ;
 
   ETYV
       : 'The different types of convex volumes (LTC)'
       =( FC         = INTE [4,16]       : 'Number of faces',
          CR         = INTE [4,16]       : 'Number of corners',
          NG         = INTE [6,32]       : 'Number of edges')
       ;
 
   EMAT
       :      'Material constants (LTC) '
       = (MaterialIndex= INTE [1,99]        : 'user material number',
          MatName      = Name16             : 'Material Name',
          AtomWeight   = REAL [0.,300.000]  : 'atomic weight',
          AtomNumber   = REAL [0.,110.000]  : 'atomic number',
          Density      = REAL [0.,20.000000]: 'density [gr/cm**3]',
          RadLength    = REAL [0.000,*]     : 'radiation length [cm]',
          AbsLength    = REAL [0.000,*]     : 'absorption length [cm]')
       ;
 
   ECMT
       :  ' The relationship compound to elemental matter
           should be in the relationship to ELements (LTC)'
       = (MatterProportion = REAL [0.0000,1.]: 'proportion of the element\
                                                in the compound')
       ;
 
   ELNF
       :  ' The list of planes used to described the geometry (LTC)'
       =( NM         = Name16,
          LF(4)      = LinFormComp : 'the 4 components of the plane')
       ;
 
   EVLF
       : '  The relationship volume to its limiting planes (LTC)'
       =( SG         = Sign     :' the sign of the interior of the
                           volume for this linear form')
       ;
 
   EVLS
       : '  Defines the outside of a volume (LTC)'
       =( NM         = Name16,
          VL         = Sign     :' the side on which you are',
          ZN         = INTE [1,3] :' the zone relative to the volume')
       ;
 
   EALF
       : ' The linear forms limiting the volume ,
           is an interface to the user (LTC)'
       = (LF(4) = LinFormComp )
       ;
 
   ECNC
       : ' The relationship between the contour steps and the corners
           of the volume. Has to be ordered by volume type (LTC)'
       ;
 
   ECOR
       : ' The corners of the volume, is an interface to the user (LTC)'
       = (CR(4) = Vector )
       ;
 
   ECRP
       : ' The relationship planes to corners for the type .
            Has to be ordered by volume type (LTC)'
       ;
 
   EDGE
       : ' The edges of the volume (LTC)'
       ;
 
   EFAC
       : ' The face list for a GKS Fill Area Set.
           Has to be ordered by volume type (LTC)'
       ;
 
   EFAS
       : ' The faces of the volume as they appear for GKS (LTC)  '
       = (XX  = REAL   :' x of the face array',
          YY  = REAL   :' y of the face array',
          ZZ  = REAL   :' z of the face array')
       ;
 
   ELTY
       : ' The structure of the Fill Area Set, can be computed
           from EFAC (LTC)'
       ;
 
   EQNT
       : ' The contour of the volume as a list of corners (LTC)'
       = (XX  = REAL   :' x of the contour array',
          YY  = REAL   :' y of the contour array',
          ZZ  = REAL   :' z of the contour array')
       ;
 
 
END ESET
 
DEFINE RSET
 
       (EVOL [1,1] -> [1,*] ETYV)
           : ' a volume is of a given type'
           ;
 
       (EVOL [1,1] -> [1,*] EQTY)
           : ' the volume correspond to a module type'
           ;
 
       (ECMT [1,1] -> [0,*] EMAT BY ComPound)
           : ' the compound material'
           ;
 
       (ECMT [1,1] -> [0,*] EMAT BY ELements)
           : ' A compound material is made of different elements'
           ;
 
       (EVOL [0,1] -> [0,1] EMAT)
           : ' The material corresponding to a volume'
           ;
 
       (EVLF [0,1] -> [0,*] EVOL)
           : ' A volume is limited by many planes'
           ;
 
       (EVOL [0,1] -> [0,1] EVLF BY FiRst)
           : ' The first plane associated to the volume'
           ;
 
       (EVOL [0,1] -> [0,1] EVLF BY LaSt)
           : ' The last plane associated to the volume'
           ;
 
       (EVLF [0,1] -> [0,*] ELNF)
           : ' A plane can limit more than one volume'
           ;
 
       (EVLF [0,1] -> [0,*] EALF )
           : ' the order of the planes'
           ;
 
        (ECRP [1,1] -> [1,*] EALF BY P1)
                : 'the first plane for the corner'
                ;
 
        (ECRP [1,1] -> [1,*] EALF BY P2)
                : 'the second plane for the corner'
                ;
 
        (ECRP [1,1] -> [1,*] EALF BY P3)
                : 'the third plane for the corner'
                ;
 
        (ECRP [1,1] -> [1,*] ECOR )
                : 'associate a corner to 3 planes'
                ;
 
        (ECRP [1,1] -> [1,*] ETYV )
                : 'gives the volume type for a particular relationship
                   corners to planes'
                ;
 
        (ETYV [1,1] -> [1,1] ECNC BY FirsT )
                : 'gives the beginning of the relationship for the type
                   inverted from ECNC -> ETYV'
                ;
 
        (ETYV [1,1] -> [1,1] ECNC BY LasT )
                : 'gives the end of the relationship for the type
                   inverted from ECNC -> ETYV'
                ;
 
        (ETYV [1,1] -> [1,1] ECRP BY FirstR )
                : 'gives the beginning of the relationship for the type
                   inverted from ECRP -> ETYV'
                ;
 
        (ETYV [1,1] -> [1,1] ECRP BY LastR )
                : 'gives the end of the relationship for the type
                   inverted from ECRP -> ETYV'
                ;
 
        (ETYV [1,1] -> [1,1] EFAC BY FirstsF )
                : 'gives the beginning of the relationship for the type
                   inverted from EFAC -> ETYV'
                ;
 
        (ETYV [1,1] -> [1,1] EFAC BY LastsF )
                : 'gives the end of the relationship for the type
                   inverted from EFAC -> ETYV'
                ;
 
        (EFAC [1,1] -> [1,*] EALF )
                : 'the plane of the face'
                ;
 
        (EFAC [1,1] -> [1,*] ECOR )
                : 'the corners of the face'
                ;
 
        (EFAC [1,1] -> [1,*] EFAS )
                : 'the corners of the face in the face list'
                ;
 
        (EFAC[1,1] -> [1,*] ETYV )
                : 'gives the volume type for a particular relationship
                   corners to faces'
                ;
 
        (ECNC [1,1] -> [1,*] EQNT )
                : 'the point of the contour'
                ;
 
        (ECNC [1,1] -> [1,*] ECOR )
                : 'the corners of the contour'
                ;
 
        (ECNC [1,1] -> [1,*] ETYV )
                : 'gives the volume type for a particular relationship
                   corners to contour'
                ;
 
        (ELTY [1,1] -> [1,*] EFAS )
                : 'the beginning of the face in the Fill Area Set'
                ;
 
        (ELTY [1,1] -> [1,*] EALF )
                : 'the beginning of the face in the Fill Area Set
                   corresponds to a face'
                ;
 
        (ELTY [1,1] -> [1,*] ETYV )
                : 'the beginning of the face in the Fill Area Set
                   corresponds to a type'
                ;
 
END RSET
 
END SUBSCHEMA
 
 
 
 SUBSCHEMA HCALGEOM
 :'HCAL part of the ALEPH Detector Description System'
 
 
 AUTHOR   'R.Tenchini,L.Silvestris'
 REVIEWER 'A.Sciaba'
 VERSION  '3.0'
 DATE     '08/04/97'
 
 DEFINE ESET
 
 HCAL
      :      'The detector component : HCAL
              Constants characterizing hadron calorimeter globally (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         CmpNam       = Name   'HCAL'         : 'Name of the component',
         NSubcomp     = INTE   3              : 'subcomp:endcapA, barrel,       ,
                                                endcapB',
         NTheta       = INTE   62             : 'number of theta rows',
         NPhi         = INTE   96             : 'max number of phi cols',
         NReg         = INTE   3              : 'phi regions (24,48,96 cols)',
         IThet1       = INTE    4             : 'theta rows  in region 1',
         ItHet2       = INTE   10             : 'theta rows in region 1+2',
         GAp          = Length [2.2,  2.3]    : 'air gap between iron slabs',
         TironF       = Length [ 5.,  5.1]    : 'thickness of iron slabs 1-22',
         TironL       = Length [10., 10.1]    : 'thickness of iron slab 23')
      ;
 
 HCTG
     :     'Trigger configuration for Hcal (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         EndcapPhitrig = INTE 12       :'nb endcap phi trigger regions',
         BarrelPhitrig = INTE 24       :'nb barrel phi trig regions',
         EndcapThettri = INTE  4       :'nb endcap theta trig regions',
         BarrelThettri = INTE  4       :'nb barrel theta trig regions',
         TrigMat(3,62) = INTE [0,14]   :'matrix(J,Itheta)
                                         J=1,2,3 --> Hcal,barrel,endcap')
     ;
 
 HSCO
      :      'The detector subcomponents for HCAL
              Constants characterizing the HCAL subcomponents (LTC)'
 
        SIZE 2,2
 
      = (ValRng(2))
      ;
 
 HBAR
      :      'The detector subcomponent : HBAR
              Constants characterizing the barrel hadron calorimeter (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         ScName       = Name  'HBAR'          : 'Name of the subcomponent',
         NSlots       = INTE  24              : 'Number of slots/subcomp.',
         RelPos(3),RelRot(3),
         NpaFro       = INTE 11               : 'number of pads in front stack',
         NpaBck       = INTE 12               : 'number of pads in back  stack',
         NLay         = INTE 23               : 'number of tubes layers')
      ;
 
 HEND
      :      'The detector subcomponent : HEND
              Constants characterizing the endcap hadron calorimeter
              Hcal has 2 endcaps ; endcap A (z>0) and endcap B (z<0)
              each endcap is divided in an inner and an outer part (LTC)'
 
        SIZE 2,2
 
 
      = (ValRng(2),
         ScName       = Name 'HENA'|'HENB'    : 'Name of the subcomponent',
         NSlots       = INTE  6               : 'Number of slots/subcomp.',
         RelPos(3),RelRot(3),SiGn,
         NpaFro       = INTE  11              :'number of pads in front stack',
         NpaBck       = INTE  11              :'number of pads in back  stack',
         NWenmx       = INTE  160             : 'max readout in endcap',
         NLayen(2)    = INTE  7|15            : 'layers in inner/outer endcap')
      ;
 
 HSBA
      :      'Hcal Slot BArrel (LTC)'
 
        SIZE 24,24
 
      =  (ValRng(2),
          ElNum       = INTE [13,24]          : 'Electronics module number',
          SloNam      = Name                  : 'The name of a slot position',
          ModTyp      = Name                  : 'Module type',
          XyzSlo(3)   = Length                : 'Nom. Position of a slot',
          RotSlo(3)   = Angle                 : 'Nom. Rotation of a slot',
          PhiMin      = Angle [0.,6.3000]     : 'Minimum phi angle of slot')
      ;
 
 HSEC
      :      'Hcal Slot EndCap (LTC)'
 
        SIZE 12,12
 
      =  (ValRng(2),
          FrstNu      = INTE [1,12]|[25,36]   :'Electronic number of first
                                                half of module',
          ScndNu      = INTE [1,12]|[25,36]   : 'Electronic number of second
                                                half of module',
          SLonam      = Name                  : 'The name of a slot position',
          ModTyp      = Name                  : 'Module type',
          XyzSlo(3)   = Length                : 'Nom. Position of a slot',
          RotSlo(3)   = Angle                 : 'Nom. Rotation of a slot',
          PhiMin      = Angle [0.,6.3000]     : 'Minimum phi angle of slot')
      ;
 
 HBMT
      :      'HCAL Barrel module type
              Constants characterizing a barrel module type
              2 marks on each face (sides A and B) (STC)'
 
        SIZE 2,2
 
      = (ValRng(2),
         ModTyp       = Name                  : 'Module type',
         XMark(2,2)   = Length                : 'X-Position of align. marks',
         YMark(2,2)   = Length                : 'Y-Position of align. marks')
      ;
 
 HEMT
      :      'Hcal Endcap Module Type
              Constants characterizing a endcap module type (STC)'
 
        SIZE 2,2
 
      = (ValRng(2),
         ModTyp       = Name                  : 'Module type',
         XMark(4,2)   = Length                : 'X-Position of align. marks',
         YMark(4,2)   = Length                : 'Y-Position of align. marks',
         NumInn       = INTE 10               : 'number of double eightfolds in
                                                each inner part layer',
         NumOut       = INTE 20               : 'number of double eightfolds in
                                                each outer part layer')
      ;
 
 HBPM
      :      'Hcal Barrel Physical module
              Constants characterizing a HCAL physical module (LTC)'
 
        SIZE 24,24
 
      =  (ValRng(2),HwName,
          ModTyp      = Name                  : 'Module type')
      ;
 
 HEPM
      :      'Hcal Endcap Physical Module
              Constants characterizing a HCAL physical module (LTC)'
 
        SIZE 12,12
 
      =  (ValRng(2),HwName,
          ModTyp      = Name                  : 'Module type')
      ;
 
 HBCP
      :      'Hcal Barrel Corrections for Position
              Constants to describe the HCAL barrel alignment (LTC)'
 
        SIZE 24,24
 
      = (ValRng(2))
      ;
 
 HECP
      :      'Hcal Endcap Corrections for Position
              Constants to describe the HCAL endcap alignment (LTC)'
 
        SIZE 12,12
 
      = (ValRng(2))
      ;
 
 HBGE
      :      'Hcal Barrel Geometry
              Constants to describe the HCAL barrel geometry (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         YbarmI       = Length [297.3,297.4]  : 'min barrel radius',
         YbarmX       = Length [468.4,468.5]  : 'max barrel radius',
         ZbarmX       = Length [365.3,365.5]  : 'zmax of barrel',
         IronTh       = Length [0.5,0.6]      : 'thickness of iron sheet
                                                 before layer1',
         Radius       = Length [298.89,298.91]: 'radius at layer 1',
         PhiOff       = Angle [0.,0.]         : 'phi offset of barrel')
      ;
 
 HEGE
      :      'Hcal Endcap Geometry
              Constants to describe the HCAL Endcap geometry (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         ZendmI(2)   = Length [315.0,365.4]   : 'zmin of inner/outer endcap',
 
         ZendmX(2)   = Length [365.3,483.4]   : 'zmax of inner/outer endcap',
 
         RendmI(2)   = Length [ 45.0, 45.1]   : 'rmin of inner/outer endcap',
 
         RendmX(2)   = Length [209.9,435.0]   : 'rmax of inner/outer endcap')
      ;
 
 HBDE
      :      'Hcal Barrel module DEscription
              Constants to describe the geometry of a barrel module.
              Layer widths in old HNG2 from 75.374 to 117.237 will be
               given by a suitable algorithm (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         Lengthnot1   = Length [13.0,13.1]    : 'length(z)  of notch # 1',
         Widthnot1    = Length [33.0,33.1]    : 'width  of notch # 1',
         Lengthnot2   = Length [13.0,13.1]    : 'length(z) of notch # 2',
         Widthnot2    = Length [5.9,6.1]      : 'width of notch # 2',
         Lengthnot3   = Length [1.0,99.0]    : 'length(z) of notch # 3',
         Widthnot3    = Length [1.0,99.0]     : 'width of notch # 3',
         DeadWid      = Length [2.0,3.0]      : 'dead zone width on
                                                 module border',
         Type2        = INTE 4                : 'Number of eightfolds of
                                                type 2 in barrel layer',
         Type3        = INTE 4                : 'Number of eightfolds of
                                                type 3 in barrel layer',
         NeiBor       = INTE 1                : 'Number of eightfolds of
                                                type3 on module border',
         N8Tube(23)   = INTE [9,14]           : 'number of 8-tubes
                                                in layers 1-23',
         Tubes8(23,2) = INTE [-1,13]          : '8tubes in layers 1-23
                                                befores spac 1,2',
         SPacba(23,2) = Length [0.,8.0]       : 'width of spacer 1,2
                                                in layers 1-23')
      ;
 
 HEDE
      :      'Hcal Endcap module Description
              Constants to describe the geometry of a endcap module (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         WallTh      = Length [2.5,3.0]       : 'thickness of sextant
                                                outer wall',
         SpacTh      = Length [2.0,2.5]       : 'Thickness of iron
                                                spacers in sextant',
         DistSp      = Length [82.0,84.0]     : 'Distance between iron
                                                spacers in sextant',
         FrstPl      = Length [5.0,5.1]       : 'position of first streamer
                                                plane in endcap')
      ;
 
 HTID
      :   'Hcal Tubes Inner Description (LTC)\
           # of Constants to describe the non sensible parts inside the tubes\
           # of row (=1)'
 
           STATIC
 
      = (tubIntrBarrel    = Length [2.0,2.1]: 'tube intrusion barrel ',
         tubIntrEndCap(2) = Length [2.0,2.9]: 'tube intrusion endcap \
                                         1=read_out side 2.9,2=blind side 2.0',
         tubCapInnEndC    = Length [2.0,2.1]: 'inner side endcap tube cap',
         tubCapOutEndC    = Length [0.5,0.5]: 'outer side endcap tube cap',
         PlugCardRdSdEC   = Length [3.6,3.7]: 'plastic plug+card support \
                                               read_out side endcap',
         PlugcardblEC(2)= Length [2.5,2.6]: 'plastic plug +card support \
                                          blind side endcap 2.6=30 deg. \
                                                            2.5= 0 deg',
         PlugcdonRdSdEC   = Length [1.7,1.8]: 'plastic plug card support only\
                                               read_out side endcap',
         PlugcdonBlSdEC   = Length [1.4,1.5]: 'plastic plug card support only \
                                               blind side endcap',
         Plugcardbarrel   = Length [2.5,2.6]: 'plastic plug +card support \
                                                barrel',
         PlugcdOnBarrel   = Length [1.4,1.5]: 'plastic plug card support only \
                                                barrel')
       ;
 
 HTUE
      :  'Hcal Exceptions in TUbes  Description (LTC)\
         # of Constants to describe not standard Tubes inside HCAL\
         # of Row (=1)'
 
           STATIC
 
       =(ModNoStanBar(2)= INTE [6,7]  : 'modules with not-standard sequence \
                                         of 8fold in barrel',
         Num4Ty8Fd      = INTE   3    : '# of 8fold of type 4 for modules 6-7',
         Num3Ty8Fd      = INTE   1    : '# of 8fold of type 3 for modules 6-7',
         Num2Ty8Fd      = INTE   4    : '# of 8fold of type 2 for modules 6-7',
         ModSpFrPlBar(2)= INTE [15,22]: 'barrel modules with a reduced \
                                         first plane',
         Num8FdKilBar   = INTE   2    : '# of 8fold killed in the first plane\
                                         of modules 15 et 22',
         LayNoStanEC    = INTE   8    : 'endcap layer with not-standard \
                                        sequence of double_8fold',
         Num0DegD8FdEC  = INTE   3    : '# of Double_8fold with 0 degrees\
                                        inclination in endcap_layer',
         NumFramD8FdEC  = INTE   5    :'# of Double_8fold between two \
                                        iron frames in endcap')
     ;
  
    HNGR
     :  'Hcal noise regions  (LTC)\
          Number of phi bins \
          Number of theta bins'
         STATIC
      = (  NoiseRegion(96)    = INTE [0,*]     : 'Code for Noise Region'
        )
 
     ;
 
 HVBA
     :      'HV channels to submodules/planes relationship for HCAL barrel.
             NR=run number\
             Number of channels \
             Number of rows (=6)'
       SIZE 6,6
       STATIC

     = (caenBArrel(40) = INTE             : 'submodule*100+plane')
      ;

 HVEA
     :      'HV channels to submodules/planes relationship for HCAL Endcap A.
             NR=run number\
             Number of channels \
             Number of rows (=4)'
       SIZE 4,4
       STATIC

     = (caenEndcapA(40) = INTE            : 'submodule*100+plane')
      ;

 HVEB
     :      'HV channels to submodules/planes relationship for HCAL Endcap B.
             NR=run number\
             Number of channels \
            Number of rows (=4)'
       SIZE 4,4
       STATIC

     = (caenEndcapB(40) = INTE            : 'submodule*100+plane')
      ; 

 
 END ESET
 
 
 DEFINE RSET
 
 (HSCO [1,1] -> [1,1] HCAL)
             : 'The HCAL has 3 subcomponents'
             ;
 
 
 (HSCO [1,1] -> [1,1] HBAR|
             -> [1,1] HEND
                      BY ScType)
             : 'The HCAL subcomponents are of different type'
             ;
 
 
 (HSBA [1,1] -> [24,24] HBAR)
             : 'Hcal barrel has 24 slots'
             ;
 
 
 
 (HSEC [1,1] -> [6,6] HEND)
             : 'each Hcal endcap has 6 slots (sextants)'
             ;
 
 
 (HBMT [1,1] -> [1,1] HSBA)
             : 'In a given slot there is one module type'
             ;
 
 
 (HEMT [1,1] -> [1,1] HSEC)
             : 'In a given slot there is one module type'
             ;
 
 
 (HBPM [0,1] -> [0,1] HSBA)
             : 'A physical module sits in one slot'
             ;
 
 
 (HEPM [0,1] -> [0,1] HSEC)
             : 'A physical module sits in one slot'
             ;
 
 
 (HBPM [0,1] -> [0,*] HBMT)
             : 'A physical module is of a given module type'
             ;
 
 
 (HEPM [0,1] -> [0,*] HEMT)
             : 'A physical module is of a given module type'
             ;
 
 
 (HBCP [0,1] -> [0,*] HSBA)
             : 'alignment Correction for a slot Position'
             ;
 
 
 (HECP [0,1] -> [0,*] HSEC)
             : 'alignment Correction for a slot Position'
             ;
 
 
 (HBGE [1,1] -> [1,1] HBAR)
             : '                     '
             ;
 
 
 (HEGE [1,1] -> [1,1] HEND)
             : '                     '
             ;
 
 
 (HBDE [1,1] -> [1,1] HBMT)
             : '                     '
             ;
 
 
 (HEDE [1,1] -> [1,1] HEMT)
             : '                     '
             ;
 
 
 END RSET
 
 
END SUBSCHEMA
 
 
 SUBSCHEMA ITCGEOM
 
 : 'ITC part of the ALEPH Detector Description. Contains only banks
    whose contents are fixed or whose contents are derived from
    offline calibration (periodic changes).'
 
 AUTHOR   'R.Beuselinck,J.Sedgbeer'
 REVIEWER 'B.Bloch,F.Ranjard,D.Casper'
 VERSION  '4.0'
 DATE     '05/03/97'
 
 
 DEFINE ESET
 
 ITCC
      :      'The detector component : ITC.
              Constants characterizing the ITC globally. (STC) '
 
        SIZE 1,1
 
      = (ValRng(2),
         CmpNam       = Name   'IT  '      : 'Name of the component',
         NLayer       = INTE   8           : 'Number of ITC layers',
         WLenmx       = Length [95.0,105.] : 'Max.active half length of\
                                              ITC wire',
         WSagit       = Length [0.,0.0400] : 'Sag of ITC wire')
      ;
 
 ISCO
      :      'ITC Subcomponent. This bank is only there to have the
              same structure for all detector components.
              (Fixed). (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         ScName       = Name   'ISCO'      : 'Itc Subcomponent')
      ;
 
 ISLO
      :      'ITC slot position.
              (Fixed).(LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         XyzSlo(3)    = Length [-5.000,5.] : 'Nominal position of slot',
         RotSlo(3)    = Angle  [-0.500,.5] : 'Nominal rotation of slot')
      ;
 
 IPMO
      :      'ITC physical module.
              Constants characterising the ITC physical module.
              (Fixed). (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         HwName)
      ;
 
 ILYR
      :      'ITC layer.   (Fixed). (LTC)'
 
        SIZE 8,8
 
      = (ValRng(2),
         LayerNumber  = INTE   [1,8]       : 'Layer number',
         NWiril       = INTE   96|144      : 'Number of wires in layer',
         RWiril       = Length [16.1,26.03]: 'Radius of layer',
         PhOfil       = INTE   0|1         : 'Phi offset of layer',
         CHigil       = Length [.790,.99]  : 'Cell height in layer')
      ;
 
 IGEO
      :      'Itc passive material  Geometry
              Constants to describe the Itc passive components geometry.
              (Fixed). (LTC)'
 
        SIZE 9,9
 
 
      = (ValRng(2),
         NAme         = CH16                  : 'name of component',
         RInn         = Length [12.0,30.300]  : 'inner radius of component  ',
         ROut         = Length [13.0,30.300]  : 'outer radius of component  ',
         ZmiN         = Length [0.0,240.000]  : 'zmin of component',
         ZmaX         = Length [100.,240.000] : 'zmaxt of component ')
      ;
 
 IALI
      :      'ITC alignment. Values derived from offline
              calibration - may change periodically. (STC)
              Used for pre-1997 alignment'
 
        SIZE 1,1
 
      = (ValRng(2),
         DelXyz(3)  = Length [-1.0000,1.]    : 'Correction to nom. slot pos.',
         DelRot(3)  = Angle  [-6.300000,6.3] : 'Correction to nom. slot rot.')
      ;
 
 INLI
      :      'ITC new alignment. Values derived from offline
              calibration - may change periodically. Replaces
              IALI for 1997-style alignment. (STC)'
 
        SIZE 1,1
 
      = (ValRng(2),
         DelXyz(3)  = Length [-1.0000,1.]    : 'Correction to nom. slot pos.',
         DelRot(3)  = Angle  [-6.300000,6.3] : 'Correction to nom. slot rot.')
      ;
 
 ISUR
      :      'ITC survey parameters. Survey of ITC fiducials. To be defined.(LTC)'
 
        SIZE 1,1
 
      = (ValRng(2))
      ;
 
 IMAT
       :      'Additional ITC  material constants, NR=setup no.(LTC)\
               number of words/material\
               number of materials  '
            STATIC
 
       = (MatName      = CH16              : 'Material Name',
          AtomWeight   = REAL [0.,300.000]  : 'atomic weight',
          AtomNumber   = REAL [0.,110.000]  : 'atomic number',
          DEnsity      = REAL [0.,20.000000]: 'density [gr/cm**3]',
          RadLength    = REAL [0.000,*]     : 'radiation length [cm]',
          AbsLength    = REAL [0.000,*]     : 'absorption length [cm]')
       ;
 
 IMIX
      :      'definition of ITC mixtures, NR=setup no. (LTC) \
              number of words/mixture\                                                                                            
              number of mixtures'                                                                                                 
          STATIC                                                                                                                  
                                                                                                                                  
      = (MixtureName   = CH16               : 'mixture name',                                                                     
         MixtureType   = CHA4               : 'mixture type ELEM or MATE',                                                        
         DEnsity       = REAL               : 'density of mixture of 
                                              ELEMents or fac 
                                              (dens=fac*dens)for MATErials')
      ;                                                                                                                           
                                                                                                                                  
 IMMA
      :      'definition of materials used in ITC mixtures, NR=setup no. (LTC)\
              number of words/material\                                                                                           
              number of materials'                                                                                                
           STATIC                                                                                                                 
                                                                                                                                  
      = (IMIXrownumber = INTE               : 'IMIXrownumber',
         MAterialname  = CH16               : 'material name',                                                                    
         thickness     = REAL               : 'thickness of the material')                                                        
      ;                                                                                                                           
                                                                                                                                  
 IPCO
      :     'definition of ITC  polycones, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VolumindexValue    = INTE           : 'volume index in IVOL',
         ZBvalue            = REAL           : 'Z of dimension change',
         RmiN               = REAL           : 'Rminimum',
         RmaX               = REAL           : 'Rmaximum ')
      ;
 
 ITME
      :      'definition of ITC  tracking media, NR=setup no. (LTC) \
              number of words/tracking medium\
              number of tracking media'
           STATIC
 
      = (TrackmedNam         = CH16            : 'tracking medium name',
         MAterialname        = CH16            : 'material name',
         tmaxFD              = REAL            : 'max angle due to field',
         dmaxMS              = REAL            : 'max displacement due to
                                                  X-scattering',
         DEemax              = REAL            : 'max fractional energy loss',
         EPsil               = REAL            : 'tracking precision',
         STmin               = REAL            : 'min step due to X-scat
                                                  or eloss',
         isVOl               = INTE            : '=0 no sensitive,
                                                  =1 sensitive',
         GPar                = INTE            : '=0 no SGPA, =1 SGPA',
         ifieldFLag          = INTE            : '=0 no mag.field,
                                                  =1 no unif.B,
                                                  =3 uniform B')
      ;
 
 IVOL
      :     'definition of ITC  volumes, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VOlumename         = CHA4           : 'volume name',
         SHapename          = CHA4           : 'shapename',
         ITMErownumber      = INTE           : 'ITME row number ',
         NumofParame        = INTE           : 'number of parameters',
         PArameters(5)      = REAL           : 'list of parameters')
      ;
 
 IPOS
      :      'positions of ITC  volumes, NR=setup no. (LTC)\
              number of words/positions\
              number of positions'
           STATIC
 
      = (DAuthername        = CHA4           : 'volume name of the
                                                volume to be positioned',
         MOthername         = CHA4           : 'volume name of the mother ',
         FLag               = CHA4           : 'geant flag ONLY or MANY',
         SROTrownumber      = INTE           : 'SROT row number
                                               (rotation matrix no.) or 0',
         CopyNumber         = INTE           : 'copy number of the volume
                                               volume placed',
         POsition(3)        = REAL           : 'x y z position of the volume')
       ;
 
 ITOR
      :      'ordering of ITC  volumes, NR=setup no. (LTC)\
              number of words/ordering\
              number of orderings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the
                                                volume to be ordered',
         AxisNumber         = INTE           : 'Axis number along which to order')
       ;
 
 IATT
      :      'attribute setting of ITC  volumes, NR=setup no. (LTC)\
              number of words/setting\
              number of settings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the volume
                                                which attribute to be set',
         AttributeName      = CHA4           : 'attribute name to be set',
         AttributeValue     = INTE           : 'Value of attribute ')
       ;
 
 END ESET
 
 
 DEFINE RSET
 
 (ISCO [1,1] -> [1,1] ITCC)
             : 'The ITC has one subcomponent'
             ;
 
 (ISLO [1,1] -> [1,1] ISCO)
             : 'The ITC has one slot'
             ;
 
 (IPMO [0,1] -> [0,1] ISLO)
             : 'A physical module sits in the slot'
             ;
 
 (ILYR [1,1] -> [1,8] IPMO)
             : 'An ITC layer belongs to the physical module'
             ;
 
 (IGEO [1,1] -> [1,1] IPMO)
             : 'The physical module has a geometry description'
             ;
 
 (IALI [1,1] -> [1,1] ISLO)
             : 'ITC alignment corrections'
             ;

 (INLI [1,1] -> [1,1] ISLO)
             : 'ITC alignment corrections'
             ;
 
 (ISUR [1,1] -> [1,1] IPMO)
             : 'ITC survey Parameters'
             ;
 
 END RSET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA LCALGEOM
 :'LCAL part of the ALEPH Detector Description'

 AUTHOR   'P. Hansen,B.Bloch'
 REVIEWER 'F. Jacot'
 VERSION  '2.1'
 DATE     '05/03/97'


 DEFINE ESET

 LCAL
      :      'The detector component : LCAL
              Constants characterizing the luminosity calorimeter globally.
              (LTC)'

        SIZE 1,1

      = (ValRng(2),
         CmpNam    = Name   'LC  '          : 'Name of the component',
         NSubcomp  = INTE   2               : 'Number of subcomponents',
         ZDis      = Length [262.00,263.00] : 'Nominal distance from O',
         RInner    = Length [9.90,10.10]    : 'Inner radius',
         ROuter    = Length [51.90,52.10]   : 'Outer radius',
         ZLength   = Length [44.00,48.00]   : 'Length',
         TInner    = Length [0.90,1.10]     : 'Inner wall thickness',
         TOuter    = Length [0.70,0.90]     : 'Outer wall thickness',
         GInner    = Length [0.40,0.60]     : 'Inner air gap',
         GOuter    = Length [0.50,0.70]     : 'Outer air gap',
         GBetween  = Length [0.30,0.50]     : 'Air gap between modules',
         SDiameter = Length [0.00,0.80]     : 'Front plate screws diameter',
         SLength   = Length [0.00,5.00]     : 'Front plate screws length',
         SPhidiff  = Angle  [0.,1.5708]     : 'Delta phi between screws',
         SRadius   = Length [10.,16.00]     : 'Radius position of screws',
         SMinphi   = Angle  [-1.5708,0.]    : 'Minimun phi of inner screw' ,
         SmaXphi   = Angle  [0.00,1.5708]   : 'Maximum phi of inner screw',
         BInfy     = Length [0.,10.000]     : 'Side screw band y min ',
         BSupy     = Length [0.,10.000]     : 'Side screw band y max  ',
         BXaverage = Length [0.,10.000]     : 'Average x of side band' ,
         Base1lead = Length [0.,5.0000]     : 'Base of lead cut triangle 1',
         Height1   = Length [0.,10.000]     : 'Height of lead cut triangle 1',
         Median1   = Angle  [0.,1.5708]     : 'Median angle of lead cut 1',
         Base2lead = Length [0.,5.0000]     : 'Base of lead cut triangle 2',
         Height2   = Length [0.,10.000]     : 'Height of lead cut triangle 2',
         Median2   = Angle  [0.,1.5708]     : 'Median angle of lead cut 2',
         LeadDim(3)= Length [0.,30.00]      : 'Lead absorber xyz dimensions',
         NsciDim(3)= Length [0.,30.00]      : 'NE110 Scint. xyz dimensions',
         LeadPos(3)= Length [0.,400.00]     : 'Lead front face xyz position',
         NsciPos(3)= Length [0.,400.00]     : 'NE110 front face xyz position')
      ;

 LSCO
      :      'The detector subcomponent : LCAL
              Constants characterizing a LCAL subcomponent globally. (LTC) '
 
        SIZE 2,2
 
      = (ValRng(2),
         CName        = Name 'EB  '|'EA  ' : 'Name of the subcomponent',
         NSlots       = INTE 2             : 'Number of slots/subcomp.',
         RelPos(3)    = Length             : 'Nom position rel to ALEPH',
         RelRot(3)    = Length             : 'Nom rotation rel to ALEPH',
         SiGn)
      ;
 
 LSLO
      :      'LCAL slot position. (LTC)'
 
        SIZE 4,4
 
      =  (ValRng(2),
          SloNam      = Name          : 'The name of a slot position',
          XyzSlo(3)   = Length        : 'Nom. Position of a slot',
          RotSlo(3)   = Angle         : 'Nom. Rotation of a slot',
          XyzMrk(3)   = Length        : 'Nom. Position of survey mark',
          TheMrk      = Angle         : 'Nom. Theta of backplane',
          PhiMrk      = Angle         : 'Nom. Phi of backplane')
      ;
 
 LALI
      :      'LCAL slot alignment corrections (STC)'
 
        SIZE 4,4
 
      =  (ValRng(2),
          DXyzsl(3)   = Length        : 'Deviation from nominal position',
          DRotsl(3)   = Angle         : 'Deviation from nominal rotation',
          DxMark(3)   = Length        : 'Dev. of survey mark position',
          DThMrk      = Angle         : 'Dev. from nom. Theta',
          DPhMrk      = Angle         : 'Dev. from nom. Phi')
      ;
 
 LMTY
      :      'LCAL module type
              Constants characterizing a LCAL module type. (LTC)'
 
        SIZE 1,1
 
      =  (ValRng(2),
          ModTyp   = Name                 : 'Module type',
          NStor    = INTE 3               : 'Nb of storeys',
          NLayer(3)= INTE [9,20]          : 'Nb of wire-planes/storey',
          NPlane   = INTE 38              : 'Nb of wire-planes',
          NWire    = INTE 20              : 'Nb of wires/plane',
          N2trig   = INTE 2               : 'Nb of wire trigger signals',
          NRows    = INTE 30              : 'Nb of tower-rows',
          NTrig    = INTE 12              : 'Nb of tower trigger sectors',
          FroLen   = Length [4.20,4.40]   : 'Length of front plate',
          FRalen   = Length [0.47,0.49]   : 'Length of front pl. in r.l.',
          BakLen   = Length [2.90,3.10]   : 'Length of back plate',
          BRalen   = Length [1.60,1.80]   : 'Length of back pl. in r.l.',
          STolen(3)= Length [7.700,17.500] : 'Length of storey',
          RAdlen(3)= Length [4.70,10.70]   : 'Length of storey in r.l.',
          DeadTb   = Length [1.00,2.00]   : 'Dead wire support length',
          CadCam   = Length [.0000,.1000] : 'Cadcam unit for pad sizes',
          BAlum    = Length [1.00,2.00]   : 'Length of back Al plate',
          BSpace   = Length [0.50,1.50]   : 'Length of back air space',
          RDowel   = Length [1.90,2.10]   : 'Radius of dowel',
          XDowel   = Length [4.50,5.00]   : 'X of dowel',
          YDowel   = Length [47.00,48.00] : 'Y of dowel')
      ;
 
 
 LRWG
      :      'LCAL tower-row geometry in a module. (LTC)'
 
        SIZE 30,30
 
      =  (ValRng(2),
          LRow    = INTE [1,30]       : 'Row number in module',
          LCol    = INTE [1,6]        : 'First column number',
          NCol    = INTE [1,16]       : 'Number of columns',
          LAmp(16)= INTE [0,12]       : 'Trigger sector assignment')
      ;
 
 LWRG
      :      'LCAL standard wire geometry in a module plane. (LTC)'
 
        SIZE 20,20
 
      =  (ValRng(2),
          LWire  =  INTE [1,20]           : 'Wire number',
          TubWid =  Length 2.00|2.50      : 'Width of a tube',
          Dead2  =  Length [0.00,1.90]    : 'Dead space every 2nd plane',
          XLow   =  Length [0.00,50.00]   : 'Low abs(x) boundary of tube',
          YLow   =  Length [0.00,50.00]   : 'Low abs(y) bound of tube',
          YHigh  =  Length [10.00,50.00]  : 'High abs(y) bound of tube')
      ;
 
 
 LLAY
      :      'LCAL plane geometry in a module. (LTC)'
 
        SIZE 38,38
 
      =  (ValRng(2),
          LPlan  =  INTE [1,38]        : 'Plane number in module',
          PSize  =  INTE [44,51]       : 'Pad size in Cadcam units',
          POffs  =  Length [0.00,3.00] : 'Pad offset from y-axis')
      ;
 
 LPMO
      :      'LCAL physical module
              Constants characterizing a LCAL physical module. (LTC)'
 
        SIZE 4,4
 
      =  (ValRng(2),
          HwName      = Name               : 'Name of module')
      ;

 LDRE

      :      'LCAL dead regions. (LTC)'

        SIZE 1,1

      = (ValRng(2),
         DeadTb   = Length [1.00,2.00] :'Dead distance from Al end',
         Dead2    = Length [1.00,2.00] :'Dead distance for fuses',
         DeadSupp = Length [0.00,1.00] :'Dead zone around wire support',
         XLsupp(4) = Length [0.00,45.00]:'Low X edge of inside supports',
         XHsupp(4) = Length [0.00,45.00]:'High X edge of inside supports',
         YSupp(4) = Length [0.00,45.00]:'Low X edge of inside supports',
         Y0cut = Length [16.00,18.00]:  'Intercept of inner pad cut',
         DYcut = Length [0.70,0.90]:    'Slope of inner pad cut',
         Y5cut = Length [9.00,10.00]:   'Pad cutoff at tube 5',
         DistWire = Length [0.200,0.500]: 'Distance wire plane to pad plane')
      ;

LDST
      :      'Lcal dead storey list (STC)\
              number of words / storey\
              number of storeys'
              SIZE 0,*
 
      = (ValRng(2),
         Address = INTE [1,2016]  : 'Address = ICOL + (IROW-1)*16 + (MODU-1)*512',
         Storey  = INTE [1,3]     : 'Storey')
      ;
 
 
 LDWP
      :      'Lcal dead wire plane list (STC) \
              number of words / plane\
              number of planes'
              SIZE 0,152
 
      = (ValRng(2),
         Address = INTE [1,230]  : 'Address = IPLAN + (MODU-1)*64')
      ;
 
 LCPG
      :      'LCAL corrections to plane geometry'

        SIZE 38,38

      =  (ValRng(2),
          LPlan  =  INTE [1,38]           : 'Plane number in module',
          PSize  =  REAL [-0.1000,0.1000] : 'Pad size correction in cm',
          POffs  =  REAL [-0.1000,0.1000] : 'Pad x-offset correction in cm')
      ;


 END ESET
 
 DEFINE RSET
 
 (LSCO [1,1] -> [1,2] LCAL)
             : 'The LCAL has two subcomponents'
             ;
 
 (LSLO [1,1] -> [1,2] LSCO)
             : 'A LCAL subcomponent has 2 slots'
             ;
 
 (LSLO [1,1] -> [1,4] LMTY)
             : 'In a given slot there is one module type'
             ;
 
 (LALI [1,1] -> [1,1] LSLO)
             : 'Index of slot position'
             ;
 
 (LRWG [1,1] -> [1,30] LMTY)
             : 'A module has 30 tower-rows'
             ;
 
 (LWRG [1,1] -> [1,20] LMTY)
             : 'A module has 20 wire-tubes in each plane'
             ;
 
 (LLAY [1,1] -> [1,38] LMTY)
             : 'A module has 38 planes'
             ;
 
 (LPMO [0,1] -> [0,1] LSLO)
             : 'A physical module sits in one slot'
             ;
 
  (LPMO [0,1] -> [0,*] LMTY)
             : 'A physical module is of a given module type'
             ;
 
 END RSET
 
 
 END SUBSCHEMA


 SUBSCHEMA MUONGEOM
 
 :'MUON part of the ALEPH Detector Description'
 
 
 AUTHOR   'A. Antonelli, F. Bossi, P. Campana, G. Capon'
 REVIEWER 'P.Campana'
 VERSION  '1.3'
 DATE     '10/06/91'
 
 
 DEFINE ESET
 
 MUOG
      :      'The detector component : MUON
              Constants characterizing the muon chambers globally. (LTC)'
 
 
        SIZE 1,1
      = (ValRng(2),
         NSubcom      = INTE   3              : 'Number of subcomponents :\
                                                 Endcap,Barrel,Middle angle',
         WId8tub      = Length [ 8.00,10.00]  : 'Width of a 8-tube',
         HEi8tu       = Length [ 0.10, 1.50]  : 'Thickness of tubes plane',
         TUbcel       = Length [ 0.10, 1.50]  : 'Tube cell dimension',
         ACtZon       = Length [ 0.10, 1.50]  : 'Active zone of a tube cell',
         SPa88tu      = Length [ 0.10, 1.50]  : 'Space between two 8-tubes',
         DIstspac     = Length [40.00,60.00]  : 'Pitch location of spacers',
         DEadzone     = Length [ 0.10, 1.50]  : 'Dead zone around spacers',
         XpitStr      = Length [ 0.10, 1.00]  : 'Pitch of X-strips',
         YpitStr      = Length [ 0.10, 1.50]  : 'Pitch of Y-strips',
         SEgfir       = Length [ 0.00, 1.00]  : 'Least active track segment',
         ETub         = Length [ 0.00, 1.00]  : 'Tube efficiency ',
         EXstr        = Length [ 0.00, 1.00]  : 'X strip efficiency',
         EYstr        = Length [ 0.00, 1.00]  : 'Y strip efficiency',
         X1mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Xclus=1',
         X2mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Xclus=2',
         X3mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Xclus=3',
         X4mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Xclus=4',
         Y1mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Yclus=1',
         Y2mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Yclus=2',
         Y3mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Yclus=3',
         Y4mul        = Length [ 0.00, 1.00]  : 'Rel. amount of Yclus=4')
      ;
 
 MGSC
      :     'The detector subcomponents for MUON. (LTC)'
        SIZE 3,3
      ;
 
 
 MBAG
      :      'Constants characterizing the barrel muon chambers.(LTC)'
 
        SIZE 1,1
      = (ValRng(2),
         SUbcomp      = Name 'BA  '           : 'Name of the subcomponent',
         NBoxid       = INTE   12             : 'Number of ideal slots',
         THicbox      = Length [  5.0, 10.0]  : 'Thickness of muon box',
         LEngbox      = Length [100.0,800.0]  : 'Length of ideal box',
         R1box        = Length [100.0,800.0]  : 'Dist. of ideal int. box',
         R2box        = Length [100.0,800.0]  : 'Dist. of ideal ext. box',
         PlanDist     = Length [  1.0, 10.0]  : 'Dist. between planes',
         Y1off        = Length [  1.0, 10.0]  : 'Plane 1 strip offset',
         Y2off        = Length [  1.0, 10.0]  : 'Plane 2 strip offset')
 
      ;
 
 
 MBTG
      :      'Constants characterizing the types
              in the barrel muon chambers.(LTC)'
 
        SIZE 10,10
      = (ValRng(2),
         NAme         = Name                  : 'Name of the type',
         ZBox         = Length [100.0,800.0]  : 'Z length of the box',
         RBox         = Length [100.0,800.0]  : 'R-phi length of the box',
         nY1str       = INTE   [0,600]        : 'Numb. of y str. in box',
         nY2str       = INTE   [0,600]        : 'Numb. of y str.of toot',
         NXstri       = INTE   [0,600]        : 'Numb. of X strips',
         ZTnotch      = Length [  0.0,200.0]  : 'Z dim. of the notch',
         RTnotch      = Length [  0.0,200.0]  : 'R-phi dim. of the notch',
         W16pitc      = Length [ 10.0, 20.0]  : 'Pitch of 16 Y strips')
 
      ;
 M1TG
      :      'Constants characterizing the types
              in the barrel muon chambers.(LTC)'
 
        SIZE 10,10
      = (ValRng(2),
         X2stri       = INTE   [0,600]        : 'Numb. of X str. in pl.2')
 
      ;
 
 
 MBSG
      :      'Constants characterizing the slots in the
              barrel muon chambers.(STC)'
 
        SIZE 34,34
      = (ValRng(2),
         Nome         = Name                  : 'Name of the slot',
         VOlume       = INTE [0,12]           : 'Slot volume number ',
         ZC           = Length [-300.0,300.0] : 'Center of slot in Z',
         RC           = Length [0.0,600.0]    : 'Dist. from origin to\
                                                 first tube plane',
         DEltac       = Length [-100.0,100.0] : 'Displac. respect to the\
                                                 slot   bisector',
         T1offs       = Length [0.0,20.0]     : 'Offset of tubes pl. 1',
         T2offs       = Length [0.0,20.0]     : 'Offset of tubes pl. 2',
         TAstr        = INTE [0,12]           : 'Astros type',
         NAstr        = INTE [100,200] | 0    : 'Astros number')
 
       ;
 
 
 M1SG
      :      'Constants characterizing the slots in the
              barrel muon chambers.(STC)'
 
        SIZE 34,34
      = (ValRng(2),
         Y1offset     = Length [ 0.,100.]     : ' Y offset in pl. 1',
         Y2offset     = Length [ 0.,100.]     : ' Y offset in pl. 2')

       ;

 MBBG
      :      'Constants characterizing the electronic configurations
              in the barrel muon chambers.(LTC)'
 
        SIZE 26,26
      = (ValRng(2),
         B1           = INTE [0,10]           : 'Bus typ in Astr ty1',
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,600]          : 'Hig add in Astr ty1',
         B2           = INTE [0,10]           : 'Bus typ in Astr ty2',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         B3           = INTE [0,10]           : 'Bus typ in Astr ty3',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         B4           = INTE [0,10]           : 'Bus typ in Astr ty4',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         P3sl         = INTE [0,600]          : 'Point. slot Astr3',
         P4sl         = INTE [0,600]          : 'Point. slot Astr4',
         B5           = INTE [0,10]           : 'Bus typ in Astr ty5',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         B6           = INTE [0,10]           : 'Bus typ in Astr ty6',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6')
 
       ;
 
 
 MECG
      :      'Constants characterizing the end cap muon chambers.(LTC)'
 
        SIZE 1,1
      = (ValRng(2),
         SUbco        = Name   'EC  '         : 'Name of the slot',
         NSlot        = INTE    16            : 'Number of slots',
         ZIplan       = Length [ 0.0,10.0]    : 'Z dist of lay 1',
         ZEplan       = Length [ 0.0,10.0]    : 'Z dist of lay 2',
         PlanDist     = Length [ 0.0,10.0]    : 'Dist betw planes',
         THickbox     = Length [ 0.0,10.0]    : 'Thickn.of box',
         DeadZone     = Length [ 0.0,10.0]    : 'D.Z.along the tubes',
         XOffset      = Length [ 0.0,10.0]    : 'Tub offs in x direc')
 
       ;
 
 
 
 
 MESG
      :      'Constants characterizing the slots in the
              endcap muon chambers.(STC)'
 
        SIZE 16,16
      = (ValRng(2),
         NOme         = Name                  : 'Name of the slot',
         XCenter      = Length [-350.0,350.0] : 'Center of slot in X',
         YCenter      = Length [-350.,350.0]  : 'Center of slot in Y',
         ZCslot       = Length [-550.0,550.0] : ' Z of closest plane',
         TAstr        = INTE [0,12]           : 'Astros type',
         NAstr        = INTE [100,200]        : 'Astros number')
 
       ;
 
 M2SG
      :      'Constants characterizing the slots in the
              endcap muon chambers.(STC)'
 
        SIZE 16,16
      = (ValRng(2),
         Y1offset     = Length [ 0. , 100.]  : 'Y offset of pl.1',
         Y2offset     = Length [ 0. , 100.]  : 'Y offset of pl.2',
         X1offset     = Length [ 0. , 100.]  : 'X offset of pl.1',
         X2offset     = Length [ 0. , 100.]  : 'X offset of pl.2')
 
       ;
 
 METG
      :      'Constants characterizing the types
              in the end cap muon chambers.(LTC)'
 
        SIZE 8,8
      = (ValRng(2),
         NAme         = Name                       : 'Name of the type',
         XBox         = Length [100.0,800.0]       : 'X length of the box',
         YBox         = Length [100.0,800.0]       : 'Y length of the box',
         X1stof       = Length [  0.0, 10.0]       : 'X str. offs plan 1',
         X2stof       = Length [  0.0, 10.0]       : 'X str. offs plan 2',
         YStoff       = Length [  0.0, 10.0]       : 'Y str. offs',
         PItch        = Length [ 10.0, 20.0]       : 'Pitch of 16 Ystr.',
         NXstr        = INTE [100,700]             : 'Numb of X str.',
         NYstr        = INTE [100,600]             : 'Numb of Y str.',
         NPrser       = INTE [  0, 50]             : 'No.of tube Lengt 1',
         Neda2        = INTE [  0, 50]             : 'No.of diff lengths',
         Neda1        = INTE [  0, 50]             : 'No.of single 8 tubes',
         LEngth(25)   = Length [ 50.0,650.0] | 0.  : 'List of tub lengths')
 
       ;
 
 
 
 MEBG
      :      'Constants characterizing the electronic configurations
              in the end cap muon chambers. (STC)'
 
        SIZE 22,22
      = (ValRng(2),
         B1           = INTE [0,10]           : 'Bus typ in Astr ty1',
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,700]          : 'Hig add in Astr ty1',
         B2           = INTE [0,10]           : 'Bus typ in Astr ty2',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         B3           = INTE [0,10]           : 'Bus typ in Astr ty3',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         B4           = INTE [0,10]           : 'Bus typ in Astr ty4',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         B5           = INTE [0,10]           : 'Bus typ in Astr ty5',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         B6           = INTE [0,10]           : 'Bus typ in Astr ty6',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6')
 
       ;
 
 
 MMAG
      :      'Constants characterizing the middle angle muon chambers.(LTC)'
 
        SIZE 1,1
      = (ValRng(2),
         SUbcomp      = Name   'MA  '         : 'Name of the subcomponent',
         NSlot        = INTE 38               : 'No. of slots',
         Z0plan       = Length [ 0.0,10.0]    : 'Dist to first plane',
         PlanDist     = Length [ 0.0,10.0]    : 'Dist between planes',
         THbox10      = Length [ 0.0,20.0]    : 'Thick.of box 10',
         PItc16t      = Length [ 0.0,20.0]    : 'Pitch of 16 str.',
         DeadSpa      = Length [ 0.0,10.0]    : 'Dead zone at tube end',
         Z10plan      = Length [ 0.0,10.0]    : 'Dist to pl.1 of box 10',
         TBox         = Length [ 0.0,20.0]    : 'Thick.of boxes')
 
 
       ;
 
 
 MMTG
     :      'Constants characterizing the types
              in the middle angle muon chambers. (LTC)'
 
        SIZE 9,9
      = (ValRng(2),
         NAme         = Name                  : 'Name of the type',
         ZBox         = Length [100.0,200.0]  : 'Z dim of the box',
         RBox         = Length [100.0,500.0]  : 'R-phi dim of the box',
         ZTnotch      = Length [  0.0,100.0]  : 'Z dim of the notch',
         RTnotch      = Length [  0.0,100.0]  : 'R-phi dim of the notch',
         NXstr        = INTE [100,200]        : 'No. of X strips')
 
 
       ;
 
 
 MMSG
     :      'Constants characterizing the slots
              in the middle angle muon chambers. (LTC)'
 
        SIZE 39,39
      = (ValRng(2),
         NOme         = Name                  : 'Name of the slot',
         L1yof        = Length [0.0,15.0]     : 'Pl.1 lef off y str.',
         L2yof        = Length [0.0,15.0]     : 'Pl.2 lef off y str.',
         R1yof        = Length [0.0,15.0]     : 'Pl.1 rig off y str.',
         R2yof        = Length [0.0,15.0]     : 'Pl.2 rig off y str.',
         TLof         = Length [0.0,15.0]     : 'Tube lef offs',
         RLof         = Length [0.0,15.0]     : 'Tube rig offs',
         NYstr        = INTE [100,400]        : 'No of y str.',
         X1of         = Length [0.0,15.0]     : 'Pl.1 off x str.',
         X2of         = Length [0.0,15.0]     : 'Pl.2 off x str.',
         DZflg        = INTE [-1,1]           : 'Orient along Z',
         ZC           = Length [-500.0,500.0] : 'Z centre of box',
         RC           = Length [400.0,600.0]  : 'R dis to first pla',
         DEltc        = Length [-100.0,100.0] : 'Displac. respect to the\
                                                 slot   bisector',
         NAstr        = INTE [100,200]        : 'No of astros',
         TAstr        = INTE [0,10]           : 'Astros type no.',
         OSlot        = INTE [0,30]           : 'Offs to slot no.',
         VOlu         = INTE [0,12]           : 'Box vol. no.')
 
 
       ;
 
 
 MMBG
       :      'Constants characterizing the electronic configurations
               in the middle angle muon chambers. (LTC)'
 
        SIZE 25,25
      = (ValRng(2),
         NOme         = Name                  : 'Name of the slot',
         B1           = INTE [ 0,20]          : 'List of bus type 1',
         O1           = INTE [ 0,20]          : 'List of off type 1',
         B2           = INTE [ 0,20]          : 'List of bus type 2',
         O2           = INTE [ 0,20]          : 'List of off type 2',
         B3           = INTE [ 0,20]          : 'List of bus type 3',
         O3           = INTE [ 0,20]          : 'List of off type 3',
         B4           = INTE [ 0,20]          : 'List of bus type 4',
         O4           = INTE [ 0,20]          : 'List of off type 4',
         B5           = INTE [ 0,20]          : 'List of bus type 5',
         O5           = INTE [ 0,20]          : 'List of off type 5',
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,600]          : 'Hig add in Astr ty1',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6',
         L7           = INTE [0,600]          : 'Low add in Astr ty7',
         U7           = INTE [0,600]          : 'Hig add in Astr ty7',
         L8           = INTE [0,600]          : 'Low add in Astr ty8',
         U8           = INTE [0,600]          : 'Hig add in Astr ty8',
         L9           = INTE [0,600]          : 'Low add in Astr ty9',
         U9           = INTE [0,600]          : 'Hig add in Astr ty9',
         L0           = INTE [0,600]          : 'Low add in Astr ty10',
         U0           = INTE [0,600]          : 'Hig add in Astr ty10')
 
 
      ;
 
 M3BG
       :      'Constants characterizing the electronic configurations
               in the middle angle muon chambers. (LTC)'
 
        SIZE 25,25
      = (ValRng(2),
         B1           = INTE [ 0,20]          : 'List of bus type 1',
         O1           = INTE [ 0,20]          : 'List of off type 1',
         B2           = INTE [ 0,20]          : 'List of bus type 2',
         O2           = INTE [ 0,20]          : 'List of off type 2',
         B3           = INTE [ 0,20]          : 'List of bus type 3',
         O3           = INTE [ 0,20]          : 'List of off type 3',
         B4           = INTE [ 0,20]          : 'List of bus type 4',
         O4           = INTE [ 0,20]          : 'List of off type 4',
         B5           = INTE [ 0,20]          : 'List of bus type 5',
         O5           = INTE [ 0,20]          : 'List of off type 5',
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,600]          : 'Hig add in Astr ty1',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6',
         L7           = INTE [0,600]          : 'Low add in Astr ty7',
         U7           = INTE [0,600]          : 'Hig add in Astr ty7',
         L8           = INTE [0,600]          : 'Low add in Astr ty8',
         U8           = INTE [0,600]          : 'Hig add in Astr ty8',
         L9           = INTE [0,600]          : 'Low add in Astr ty9',
         U9           = INTE [0,600]          : 'Hig add in Astr ty9',
         L0           = INTE [0,600]          : 'Low add in Astr ty10',
         U0           = INTE [0,600]          : 'Hig add in Astr ty10',
         LA           = INTE [0,600]          : 'Low add in Astr ty11',
         UA           = INTE [0,600]          : 'Hig add in Astr ty11',
         LB           = INTE [0,600]          : 'Low add in Astr ty12',
         UB           = INTE [0,600]          : 'Hig add in Astr ty12') 
 
      ;
 
 MSLO
      :       'MUON Slot. (LTC)'
 
        SIZE 1,1
 
      = (Name                 = CH16          : 'Name of a slot')
      ;
 
 MBLG
      :      'Constants characterizing the electronic configurations
              in the barrel muon chambers. (LTC)'
 
        SIZE 24,24
      = (ValRng(2),
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,600]          : 'Hig add in Astr ty1',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6')
 
       ;
 
 
 MELG
      :      'Constants characterizing the electronic configurations
              in the end cap muon chambers. (LTC)'
 
        SIZE 22,22
      = (ValRng(2),
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,700]          : 'Hig add in Astr ty1',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6') 
       ;
 
 
 
 MMLG
       :      'Constants characterizing the electronic configurations
               in the middle angle muon chambers.(LTC)'
 
        SIZE 24,24
      = (ValRng(2),
         L1           = INTE [0,600]          : 'Low add in Astr ty1',
         U1           = INTE [0,600]          : 'Hig add in Astr ty1',
         L2           = INTE [0,600]          : 'Low add in Astr ty2',
         U2           = INTE [0,600]          : 'Hig add in Astr ty2',
         L3           = INTE [0,600]          : 'Low add in Astr ty3',
         U3           = INTE [0,600]          : 'Hig add in Astr ty3',
         L4           = INTE [0,600]          : 'Low add in Astr ty4',
         U4           = INTE [0,600]          : 'Hig add in Astr ty4',
         L5           = INTE [0,600]          : 'Low add in Astr ty5',
         U5           = INTE [0,600]          : 'Hig add in Astr ty5',
         L6           = INTE [0,600]          : 'Low add in Astr ty6',
         U6           = INTE [0,600]          : 'Hig add in Astr ty6',
         L7           = INTE [0,600]          : 'Low add in Astr ty7',
         U7           = INTE [0,600]          : 'Hig add in Astr ty7',
         L8           = INTE [0,600]          : 'Low add in Astr ty8',
         U8           = INTE [0,600]          : 'Hig add in Astr ty8',
         L9           = INTE [0,600]          : 'Low add in Astr ty9',
         U9           = INTE [0,600]          : 'Hig add in Astr ty9',
         L0           = INTE [0,600]          : 'Low add in Astr ty10',
         U0           = INTE [0,600]          : 'Hig add in Astr ty10',
         LA           = INTE [0,600]          : 'Low add in Astr ty11',
         UA           = INTE [0,600]          : 'Hig add in Astr ty11',
         LB           = INTE [0,600]          : 'Low add in Astr ty12',
         UB           = INTE [0,600]          : 'Hig add in Astr ty12')
 
 END  ESET
 
 DEFINE RSET
 
 (MGSC [1,1] -> [1,1] MUOG)
             : 'The MUON detector has  subcomponents'
             ;
 
 (MGSC [1,1] -> [1,1] MBAG |
             -> [1,1] MMAG |
             -> [1,1] MECG
                           BY ScType)
             : 'MUOG has 3 subcomponent types'
             ;
 
 (MBSG [1,1]  -> [1,34] MBAG
                          BY K1mbag)
             : ' Pointer to MBAG bank'
             ;
 
 (MBSG [1,1] -> [0,10] MBTG
                          BY K2mtbg)
             : 'Pointer to barrel slot type'
             ;
 
 (MMSG [1,1] ->  [1,39] MMAG
                          BY K1mmag)
             : ' Pointer to MMAG bank'
             ;
 
 (MMSG [1,1] -> [0,9] MMTG
                          BY K2mmtg)
             : 'Pointer to middle angle slot type'
             ;
 
 (MESG [1,1]  -> [1,16] MECG
                          BY K1mecg)
             : ' Pointer to MECG bank'
             ;
 
 (MESG [1,1] -> [0,8] METG
                          BY K2metg)
             : 'Pointer to middle angle slot type'
             ;
 
  (MSLO [1,1]   -> [1,*] MGSC)
                : 'The MUON subcomponents are composed of slots'
                ;
 
 
 END RSET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA PMGEOM
 :'Geometrical description of the Passive Material like beam pipe, coil etc'
 
 
 AUTHOR   'B. Bloch-Devaux'
 REVIEWER 'F.Ranjard'
 VERSION  '1.6'
 DATE     '09/06/95'
 
 
 DEFINE ATTRIBUTE
 
 
 END ATTRIBUTE
 
 
 DEFINE ESET
 
 PTYP
      :      'A Passive Material Component
              Generalized entity set to control the branch
              to the different passive material components. (LTC)'
 
        SIZE 4,4
 
      = (ValRng(2))
      ;
 
 BPG1
      :    'Beam pipe   Geometry
            Constants to describe the Beam pipe and related components geometry
              (LTC)'
 
        SIZE 7,7
 
      = (ValRng(2),
         NAme(4)      = CHA4                  : 'name of component',
         RInn         = Length [0.0,8.00000]  : 'inner radius of component  ',
         ROut         = Length [6.0,8.50000]  : 'outer radius of component  ',
         ZMax         = Length [50.0,315.00]  : 'zmax of component',
         ZWidth       = Length [0.0,5.00]     : 'width along z of component ',
         NMate        = INTE [0,100]          : 'index of material it is made of')
      ;
 
 BMAT
       :     'Additional BEAM pipe material constants, NR=setup no.(LTC)\
               number of words/material\
               number of materials  '
            STATIC
 
       = (MatName      = CH16               : 'Material Name',
          AtomWeight   = REAL [0.,300.000]  : 'atomic weight',
          AtomNumber   = REAL [0.,110.000]  : 'atomic number',
          DEnsity      = REAL [0.,20.000000]: 'density [gr/cm**3]',
          RadLength    = REAL [0.000,*]     : 'radiation length [cm]',
          AbsLength    = REAL [0.000,*]     : 'absorption length [cm]')
       ;
 
 BMEL
      :  'definition of elements used in BEAM pipe mixtures, NR=setup no.(LTC)\
              number of words/element\
              number of elements '
           STATIC
 
      = (BMIXrownumber = INTE               : 'BMIXrownumber',
         ELementname   = CH16               : 'element name',
         NUmbofatomol  = INTE               : 'number of atoms per mol')
      ;
 
 BMIX
      :      'definition of BEAM pipe mixtures, NR=setup no. (LTC) \
              number of words/mixture\
              number of mixtures'
          STATIC
 
      = (MixtureName   = CH16               : 'mixture name',
         MixtureType   = CHA4               : 'mixture type ELEM or MATE',
         DEnsity       = REAL               : 'density of mixture of
                                              ELEMents or fac
                                              (dens=fac*dens)for MATErials')
      ;
 
 BMMA
      : 'definition of materials used in BEAM pipe mixtures, NR=setup no.(LTC)\
              number of words/material\
              number of materials'
           STATIC
 
      = (BMIXrownumber = INTE               : 'BMIXrownumber',
         MAterialname  = CH16               : 'material name',
         thickness     = REAL               : 'thickness of the material')
      ;
 
 BTME
      :    'definition of BEAM pipe tracking media, NR=setup no. (LTC) \
            number of words/tracking medium\
            number of tracking media'
           STATIC
 
      = (TrackmedNam         = CH16            : 'tracking medium name',
         MAterialname        = CH16            : 'material name',
         tmaxFD              = REAL            : 'max angle due to field',
         dmaxMS              = REAL            : 'max displacement due to
                                                  X-scattering',
         DEemax              = REAL            : 'max fractional energy loss',
         EPsil               = REAL            : 'tracking precision',
         STmin               = REAL            : 'min step due to X-scat
                                                  or eloss',
         isVOl               = INTE            : '=0 no sensitive,
                                                  =1 sensitive',
         GPar                = INTE            : '=0 no BGPA, =1 BGPA',
         ifieldFLag          = INTE            : '=0 no mag.field,
                                                  =1 no unif.B,
                                                  =3 uniform B')
      ;
 
 BVOL
      :     'definition of BEAM pipe volumes, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VOlumename         = CHA4           : 'volume name',
         SHapename          = CHA4           : 'shapename',
         BTMErownumber      = INTE           : 'BTME row number ',
         NumofParame        = INTE           : 'number of parameters',
         PArameters(5)      = REAL           : 'list of parameters')
      ;
 
 BPCO
      :     'definition of BEAM pipe polycones, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VolumindexValue    = INTE           : 'volume index in BVOL',
         ZBvalue            = REAL           : 'Z of dimension change',
         RmiN               = REAL           : 'Rminimum',
         RmaX               = REAL           : 'Rmaximum ')
      ;
 
 BROT
      :   'definition of BEAM pipe rotation matrices, NR=setup no. (LTC)\
           number of words/rotation\
           number of rotations'
          STATIC
 
      = (Theta1             = REAL           : 'azimuthal angle for axis 1',
         Phi1               = REAL           : 'polar angle for axis 1',
         Theta2             = REAL           : 'azimuthal angle for axis 2',
         Phi2               = REAL           : 'polar angle for axis 2',
         Theta3             = REAL           : 'azimuthal angle for axis 3',
         Phi3               = REAL           : 'polar angle for axis 3')
      ;
 
 BPOS
      :      'positions of BEAM pipe volumes, NR=setup no. (LTC)\
              number of words/positions\
              number of positions'
           STATIC
 
      = (DAuthername        = CHA4           : 'volume name of the
                                                volume to be positioned',
         MOthername         = CHA4           : 'volume name of the mother ',
         FLag               = CHA4           : 'geant flag ONLY or MANY',
         BROTrownumber      = INTE           : 'BROT row number
                                               (rotation matrix no.) or 0',
         CopyNumber         = INTE           : 'copy number of the volume
                                               volume placed',
         POsition(3)        = REAL           : 'x y z position of the volume')
       ;
 
 BPOR
      :      'ordering of BEAM pipe volumes, NR=setup no. (LTC)\
              number of words/ordering\
              number of orderings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the
                                                volume to be ordered',
         AxisNumber         = INTE           : 'Axis number along which to order')
       ;
 
 BATT
      :      'attribute setting of BEAM pipe volumes, NR=setup no. (LTC)\
              number of words/setting\
              number of settings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the volume
                                                which attribute to be set',
         AttributeName      = CHA4           : 'attribute name to be set',
         AttributeValue     = INTE           : 'Value of attribute ')
       ;
 
 COG1
      :  'COIL      Geometry
          Constants to describe the overall cryostat and coil geometry . (LTC)'
 
        SIZE 11,11
 
      = (ValRng(2),
         NAme(4)      = CHA4                  : 'name of component',
         RInn         = Length 0.|[247.,295.000] : 'inner radius of component  ',
         DR           = Length [0.0,44.0000]  : 'radial thickness of component  ',
         ZMin         = Length [0.00,350.00]  : 'zmin of component',
         ZWidth       = Length [0.0,351.]     : 'width along z of component ',
         NMate        = INTE [0,100]          : 'index of material it is made of')
      ;
 
 QUG1
      :      'Quadrupoles   Geometry
              Constants to describe the Quadrupoles and related components geometry
              (LTC) '
 
        SIZE 4,4
 
    = (ValRng(2),
       NAme(4)      = CHA4                  : 'name of component',
       RInn         = Length [5.0,7.00000]  : 'inner radius of component  ',
       ROut         = Length [20.,30.0000]  : 'outer radius of component  ',
       ZmiN         = Length [309.0,400.0]  : 'zmin of component',
       X0           = Length [-3.00,3.00]   : 'shift of component axis along x direction',
       Y0           = Length [-3.00,3.00]   : 'shift of component axis along y direction',
       NMate        = INTE [0,100]          : 'index of material it is made of')
      ;
 
 PMG1
      :      'Passive material Geometry
              Constants to describe the Passive Material related
              components geometry. (LTC)'
 
        SIZE 80,80
 
      = (ValRng(2),
         NAme(4)      = CHA4                  : 'name of component',
         RInn         = Length [45.0,240.00]  : 'inner radius of component  ',
         ROut         = Length [200.,250.00]  : 'outer radius of component  ',
         ZmiN         = Length [239.99,315.0] : 'zmin of component',
         ZmaX         = Length [240.0,365.40]  : 'zmax of component ',
         PhmN         = Angle  [-6.28318,6.28318] : 'phi min of component',
         PhmX         = Angle  [-6.28318,6.28318] : 'phi max of component',
         NMate        = INTE [0,100]          : 'index of material it is made of')
      ;
 
 PMAT
       :      'Additional SAMBA material constants, NR=setup no.(LTC)\
               number of words/material\
               number of materials  '
            STATIC
 
       = (MatName      = CH16               : 'Material Name',
          AtomWeight   = REAL [0.,300.000]  : 'atomic weight',
          AtomNumber   = REAL [0.,110.000]  : 'atomic number',
          DEnsity      = REAL [0.,20.000000]: 'density [gr/cm**3]',
          RadLength    = REAL [0.000,*]     : 'radiation length [cm]',
          AbsLength    = REAL [0.000,*]     : 'absorption length [cm]')
       ;
 
 PMEL
      :      'definition of elements used in SAMBA mixtures, NR=setup no.(LTC)\
              number of words/element\
              number of elements '
           STATIC
 
      = (PMIXrownumber = INTE               : 'PMIXrownumber',
         ELementname   = CH16               : 'element name',
         NUmbofatomol  = INTE               : 'number of atoms per mol')
      ;
 
 PMIX
      :      'definition of SAMBA mixtures, NR=setup no. (LTC) \
              number of words/mixture\
              number of mixtures'
          STATIC
 
      = (MixtureName   = CH16               : 'mixture name',
         MixtureType   = CHA4               : 'mixture type ELEM or MATE',
         DEnsity       = REAL               : 'density of mixture of
                                              ELEMents or fac
                                              (dens=fac*dens)for MATErials')
      ;
 
 PMMA
      :    'definition of materials used in SAMBA mixtures, NR=setup no.(LTC) \
            number of words/material\
            number of materials'
           STATIC
 
      = (PMIXrownumber = INTE               : 'PMIXrownumber',
         MAterialname  = CH16               : 'material name',
         thickness     = REAL               : 'thickness of the material')
      ;
 
 PTME
      :      'definition of SAMBA tracking media, NR=setup no. (LTC) \
              number of words/tracking medium\
              number of tracking media'
           STATIC
 
      = (TrackmedNam         = CH16            : 'tracking medium name',
         MAterialname        = CH16            : 'material name',
         tmaxFD              = REAL            : 'max angle due to field',
         dmaxMS              = REAL            : 'max displacement due to
                                                  X-scattering',
         DEemax              = REAL            : 'max fractional energy loss',
         EPsil               = REAL            : 'tracking precision',
         STmin               = REAL            : 'min step due to X-scat
                                                  or eloss',
         isVOl               = INTE            : '=0 no sensitive,
                                                  =1 sensitive',
         GPar                = INTE            : '=0 no PGPA, =1 PGPA',
         ifieldFLag          = INTE            : '=0 no mag.field,
                                                  =1 no unif.B,
                                                  =3 uniform B')
      ;
 
 PGPA
      :     'definition of some parameters of SAMBA tracking media,
             NR=setup no. (LTC)\
             number of words/parameter\
             number of redefined parameters'
          STATIC
 
      = (PTMErownumber       = INTE            : 'PTME row number',
         PArametername       = CHA8            : 'parameter name',
         ParameterValue      = REAL            : 'parameter value')
      ;
 
 PVOL
      :     'definition of SAMBA volumes, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VOlumename         = CHA4           : 'volume name',
         SHapename          = CHA4           : 'shapename',
         PTMErownumber      = INTE           : 'PTME row number ',
         NumofParame        = INTE           : 'number of parameters',
         PArameters(5)      = REAL           : 'list of parameters')
      ;
 
 PROT
      :     'definition of SAMBA rotation matrices, NR=setup no. (LTC)\
             number of words/rotation\
             number of rotations'
          STATIC
 
      = (Theta1             = REAL           : 'azimuthal angle for axis 1',
         Phi1               = REAL           : 'polar angle for axis 1',
         Theta2             = REAL           : 'azimuthal angle for axis 2',
         Phi2               = REAL           : 'polar angle for axis 2',
         Theta3             = REAL           : 'azimuthal angle for axis 3',
         Phi3               = REAL           : 'polar angle for axis 3')
      ;
 
 PPOS
      :      'positions of SAMBA volumes, NR=setup no. (LTC)\
              number of words/positions\
              number of positions'
           STATIC
 
      = (DAuthername        = CHA4           : 'volume name of the
                                                volume to be positioned',
         MOthername         = CHA4           : 'volume name of the mother ',
         FLag               = CHA4           : 'geant flag ONLY or MANY',
         PROTrownumber      = INTE           : 'PROT row number
                                               (rotation matrix no.) or 0',
         CopyNumber         = INTE           : 'copy number of the volume
                                               volume placed',
         POsition(3)        = REAL           : 'x y z position of the volume')
       ;
 
 PMOR
      :      'ordering of SAMBA volumes, NR=setup no. (LTC)\
              number of words/ordering\
              number of orderings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the
                                                volume to be ordered',
         AxisNumber         = INTE           : 'Axis number along which to order')
       ;
 
 PATT
      :      'attribute setting of SAMBA volumes, NR=setup no. (LTC)\
              number of words/setting\
              number of settings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the volume
                                                which attribute to be set',
         AttributeName      = CHA4           : 'attribute name to be set',
         AttributeValue     = INTE           : 'Value of attribute ')
       ;
 
 END ESET
 
 
 
 DEFINE RSET
 
 
 (PTYP [1,1] -> [1,1] BPG1|
             -> [1,1] COG1|
             -> [1,1] QUG1|
             -> [1,1] PMG1
                      BY PmTyp)
             : 'The passive materials are of different types'
             ;
 
 
 
 
 END RSET
 
END SUBSCHEMA
 
 
 SUBSCHEMA SCALGEOM
 :'SCAL Detector GEOMetry Description'
 
 
 AUTHOR   'B.Bloch  '
 REVIEWER 'B.Bloch,F.Ranjard'
 VERSION  '1.6'
 DATE     '09/06/95'
 
 
 DEFINE ESET
 
 
   SMAT
       :      'Additional SCAL material constants, NR=setup no.(LTC)\
               number of words/material\
               number of materials  '
            STATIC
 
       = (MatName      = CH16              : 'Material Name',
          AtomWeight   = REAL [0.,300.000]  : 'atomic weight',
          AtomNumber   = REAL [0.,110.000]  : 'atomic number',
          DEnsity      = REAL [0.,20.000000]: 'density [gr/cm**3]',
          RadLength    = REAL [0.000,*]     : 'radiation length [cm]',
          AbsLength    = REAL [0.000,*]     : 'absorption length [cm]')
       ;
 
 SMEL
      :      'definition of elements used in SCAL mixtures, NR=setup no.(LTC)\
              number of words/element\
              number of elements '
           STATIC
 
      = (SMIXrownumber = INTE               : 'SMIXrownumber',
         ELementname   = CH16              : 'element name',
         NUmbofatomol  = INTE               : 'number of atoms per mol')
      ;
 
 SMIX
      :      'definition of SCAL mixtures, NR=setup no. (LTC) \
              number of words/mixture\
              number of mixtures'
          STATIC
 
      = (MixtureName   = CH16               : 'mixture name',
         MixtureType   = CHA4               : 'mixture type ELEM or MATE',
         DEnsity       = REAL               : 'density of mixture of
                                              ELEMents or fac
                                              (dens=fac*dens)for MATErials')
      ;
 
 SMMA
      :      'definition of materials used in SCAL mixtures, NR=setup no. (LTC)\
              number of words/material\
              number of materials'
           STATIC
 
      = (SMIXrownumber = INTE               : 'SMIXrownumber',
         MAterialname  = CH16               : 'material name',
         thickness     = REAL               : 'thickness of the material')
      ;
 
 STME
      :      'definition of SCAL tracking media, NR=setup no. (LTC) \
              number of words/tracking medium\
              number of tracking media'
           STATIC
 
      = (TrackmedNam         = CH16            : 'tracking medium name',
         MAterialname        = CH16            : 'material name',
         tmaxFD              = REAL            : 'max angle due to field',
         dmaxMS              = REAL            : 'max displacement due to
                                                  X-scattering',
         DEemax              = REAL            : 'max fractional energy loss',
         EPsil               = REAL            : 'tracking precision',
         STmin               = REAL            : 'min step due to X-scat
                                                  or eloss',
         isVOl               = INTE            : '=0 no sensitive,
                                                  =1 sensitive',
         GPar                = INTE            : '=0 no SGPA, =1 SGPA',
         ifieldFLag          = INTE            : '=0 no mag.field,
                                                  =1 no unif.B,
                                                  =3 uniform B')
      ;
 
 SGPA
      :     'definition of some parameters of SCAL tracking media,
             NR=setup no. (LTC)\
             number of words/parameter\
             number of redefined parameters'
          STATIC
 
      = (STMErownumber       = INTE            : 'STME row number',
         PArametername       = CHA8            : 'parameter name',
         ParameterValue      = REAL            : 'parameter value')
      ;
 
 SVOL
      :     'definition of SCAL volumes, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VOlumename         = CHA4           : 'volume name',
         SHapename          = CHA4           : 'shapename',
         STMErownumber      = INTE           : 'STME row number ',
         NumofParame        = INTE           : 'number of parameters',
         PArameters(5)      = REAL           : 'list of parameters')
      ;
 
 SROT
      :     'definition of SCAL rotation matrices, NR=setup no. (LTC)\
             number of words/rotation\
             number of rotations'
          STATIC
 
      = (Theta1             = REAL           : 'azimuthal angle for axis 1',
         Phi1               = REAL           : 'polar angle for axis 1',
         Theta2             = REAL           : 'azimuthal angle for axis 2',
         Phi2               = REAL           : 'polar angle for axis 2',
         Theta3             = REAL           : 'azimuthal angle for axis 3',
         Phi3               = REAL           : 'polar angle for axis 3')
      ;
 
 SPOS
      :      'positions of SCAL volumes, NR=setup no. (LTC)\
              number of words/positions\
              number of positions'
           STATIC
 
      = (DAuthername        = CHA4           : 'volume name of the
                                                volume to be positioned',
         MOthername         = CHA4           : 'volume name of the mother ',
         FLag               = CHA4           : 'geant flag ONLY or MANY',
         SROTrownumber      = INTE           : 'SROT row number
                                               (rotation matrix no.) or 0',
         CopyNumber         = INTE           : 'copy number of the volume
                                               volume placed',
         POsition(3)        = REAL           : 'x y z position of the volume')
       ;
 
 SIOR
      :      'ordering of SCAL volumes, NR=setup no. (LTC)\
              number of words/ordering\
              number of orderings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the
                                                volume to be ordered',
         AxisNumber         = INTE           : 'Axis number along which to order')
       ;
 
 SATT
      :      'attribute setting of SCAL volumes, NR=setup no. (LTC)\
              number of words/setting\
              number of settings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the volume
                                                which attribute to be set',
         AttributeName      = CHA4           : 'attribute name to be set',
         AttributeValue     = INTE           : 'Value of attribute ')
       ;
 
 SIGO
       :      'General geometry   constants, NR=setup no.(LTC)\
               number of words/description\
               number of descriptions  '
            STATIC
 
       = (NumberofModules = INTE [1,2]     : 'Number of Modules\
                                              1=ModA [+z],2=ModB [-z]',
          NumofRadialbins = INTE [1,16]    : 'Number of radial bins',
          NumofPhibins    = INTE [1,32]    : 'Number of Phi bins',
          NumofZstacks    = INTE [1,12]    : 'Number of Z stacks',
          RadiusmiNimum   = REAL [5.65, *] : 'miNimum sensitive radius [cm]',
          RadiusmaXimum   = REAL [5.65, *] : 'maXimum sensitive radius [cm]',
          PhiShift(3)     = REAL           : 'Phi shift in triplet [degree]',
          Z0position      = REAL           : 'Z position of first sensitive\
                                              layer [cm]',
          ZWidth          = REAL           : 'Z Width of a standard layer [cm]',
          Zwidth1         = REAL           : 'Z Width of first layer   [cm]',
          Zwidth2         = REAL           : 'Z Width of last layer   [cm]',
          ZwFront         = REAL           : 'Z Width of front material[cm]',
          ZwBack          = REAL           : 'Z Width of back material[cm]',
          OVerlap         = REAL           : 'crystal overlap [cm]',
          RadFirst        = REAL           : 'number of Rad length before first layer',
          RadLength       = REAL           : 'number of Rad length per layer')
       ;
 
 SALG
       :      'Alignment constants, NR=setup no.(LTC)\
               number of words/module\
               number of modules  '
            STATIC
 
       = (DeviationX      = REAL           : 'Deviation from nominal x',
          DeviationY      = REAL           : 'Deviation from nominal y',
          DeviationZ      = REAL           : 'Deviation from nominal z',
          GapopenX        = REAL           : 'Gap opening along x')
       ;
 
 SALI
       :      'Alignment constants, NR=setup no.(DROP)
               Obsolete bank - see SALG\
               number of words/module\
               number of modules  '
            STATIC
 
       = (XDeviation      = REAL           : 'Deviation from nominal x',
          YDeviation      = REAL           : 'Deviation from nominal y',
          ZDeviation      = REAL           : 'Deviation from nominal z',
          DeviationAng(3) = REAL           : 'Rotation angles',
          DeviationPhi(3) = REAL           : 'Deviation from nominal phi\
                                              of triplet planes')
       ;
 
 STYP
       :      'SICAL Plane types, NR=setup no.(LTC)\
               number of words/slot\
               number of slots  '
            STATIC
 
       = (SlotName        = CHA4           : 'Slot name of plane',
          ManufactName(2) = CHA4           : 'Manufacturer name of plane',
          PlaneType       = INTE   [1,3]   : 'Plane type     ')
       ;
 
 SINT
       :      'SICAL Internal plates measurements, NR=setup no.(LTC)\
               number of words/plate\
               number of plates  '
            STATIC
 
       = (ManufactName(2) = CHA4           : 'Manufacturer name of plate',
          TubePos(2)      = REAL           : 'Tube Reference position (x,y)',
          XtalRad(16)     = REAL           : 'Xtal Reference radius (16 crystals per plane) ')
       ;
 
 
 END ESET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA SATRGEOM
         : 'SATR part of the ALEPH detector description'
 
AUTHOR   'H. Burkhardt','H. Meinhard'
VERSION  '1.05'
REVIEWER 'B. Bloch'
DATE     '19-Mar-1991'
 
 
DEFINE ESET
 
SATR
     : 'The detector component: SATR
    Constants describing the SATR globally. (LTC)'
 
        SIZE 1,1
 
     = (ValRng(2),
    CmpNam          = Name 'SA'         :'Component name',
    numSUbcomp      = INTE 4            :'Number of subcomponents',
    numLAyer        = INTE 9            :'Number of layers per subcomp',
    numSeCtors      = INTE 8            :'Number of sectors per layer',
    numWIres        = INTE 14           :'Number of wires per sector',
    InnerRadius     = REAL [9.650,10.]  :'Inner radius of SATR',
    OuterRadius     = REAL [28.500,29.] :'Outer radius of SATR',
    InnerradiussaMo = REAL [9.800,10.]  :'Inner radius of SAMO',
    OuterradiussaMo = REAL [23.500,24.] :'Outer radius of SAMO',
    WireDistance    = REAL [.990,1.01]  :'Wire distance',
    LayerDistance   = REAL [1.850,1.95] :'Layer distance',
    LayerOffset     = REAL [.850,.95]   :'Offset of last layer',
    GroundLathick   = REAL [0.280,0.32] :'Layer Ground plate thickness',
    EpoxyThickness  = REAL [.0900,.11]  :'Epoxy thickness',
    BrassThickness  = REAL [.0290,.04]  :'Brass tube wall thickness',
    OuterThickness  = REAL [.1400,.16]  :'Epoxy outer wall thickness',
    GroundDevthick  = REAL [2.500,3.5]  :'Device ground plate thickness',
    ModuleType(8)   = INTE [1,3]        :'gas chan.: 1 rigth, 2 no , 3 left',
    DeadzoneLeft(3) = REAL [0.000,2.]   :'Dead zone left sector type i edge',
    DeadzoneRight(3)= REAL [0.000,2.]   :'Dead zone right sector type i edge',
    numCRates       = INTE 3            :'Number of crates',
    numCArds        = INTE 24           :'Number of cards per crate',
    numTDc          = INTE 16           :'Number of TDC-channels per card',
    PindowelRadius  = REAL [0.000,2.]   :'Alu Dowel/spacer radius',
    PindowelPosition= REAL [25.500,28.8]:'Radial position of dowel center',
    numPinDowel     = INTE 6            :'number of dowels per side',
    PindowelAngle(4)= REAL [0.000,360.] :'Angular position of each dowel',
    BoxxmiN         = REAL [28.800,54.] :'x min position of electronic box',
    BoxxmaX         = REAL [28.800,54.] :'x max position of electronic box',
    BoxYheight      = REAL [0.000,40.]  :' Y dimension of electronic box ',
    BoxZdepth       = REAL [0.000,20.]  :' Z dimension of electronic box ',
    BrassdowelRad   = REAL [0.000,2.]   :' Radius of brass core of dowels',
    MorePindowan(6) = REAL [0.000,360.] :' More pin dowel angles',
    dowelFeetRad    = REAL [0.000,2.]   :' Radius of brass dowel feet',
    dowelFeetDepth  = REAL [0.000,2.]   :' Depth of brass dowel feet')
     ;
 
SLAY
     : 'Constants characterizing the SATR layers. (LTC)'
 
        SIZE 9,9
 
     = (ValRng(2)   = Date               :'Beginning & expiration date',
    LayerNumber     = INTE [1,9]         :'Layer number',
    AzimuthStart    = Angle[0.,6.2900]   :'Azimuth start angle of first sector',
    ModeofOring     = INTE [0,*]         :'Mode of oring in layer')
     ;
 
 
 END ESET
 
 
 END SUBSCHEMA

 
SUBSCHEMA TPCGEOM
 :'TPC part of the ALEPH Detector Description'
 
 
 AUTHOR   'R. Richter,R.Johnson,D.Casper'
 REVIEWER 'F.Loverre'
 VERSION  '2.1'
 DATE     '06/03/97'
 
 
 DEFINE ESET
 
 
 TBDR
  :      'B field direction in global system (=direction of main
          homogenous component). The vector is unit vector.
          Used for 1997-style alignment.\
          Nb of columns \
          Nb of rows '
 
        STATIC
        SIZE 1,1
 
     =  (
         BXcomp      = REAL [-1.,1.]  : 'B_x component',
         BYcomp      = REAL [-1.,1.]  : 'B_y component',
         BZcomp      = REAL [-1.,1.]  : 'B_z component')
        ; 

 
 TNLO
  :      'TPC slot (sector) position (needs 36 entries) (STC),
          new version for tracking upgrade. Contains information
          of previous TSLO and TRZS bank.'  
 
        SIZE 36,36
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         SloNam      = Name           : 'The name of a slot position',
         SlonmB      = INTE [1,36]    : 'Slotnumber in Reconstruction',
         SuperSect   = INTE [1,12]
                     : 'Sector Number (given type) = "Supersector-nr" ',
         rDsSlo      = Length [-0.5000,0.5000]
                     : 'Displacement of Sector in r - direction',
         AdsSlo      = Angle  [-0.01000,0.01000]
                     : 'Phi-displ. of Sect. (rot. about center of TPC)',
         RotSlo      = Angle  [-0.01000,0.01000]
                     : 'Rot. of Sector about its center in order to
                        align its symm. axis with the radial line. (The center of a
                        sector is on its symm. axis at a radius which is the
                        arithmetic mean of its two outermost padrow radii)',
         rDsrZ      = Length [*,*]     : 'Displacement of Sector in z - direction',
         RotrZ      = Angle  [-0.01000,0.01000] 
                    : 'Rot. of Sector about its center in the                  
                       Rz-plane in order to align its symm. axis
                       with the radial line. (The center of a            
                       sector is on its symm. axis at a radius which is the                   
                       arithmetic mean of its two outermost padrow radii',                    
         AdsrZ      = Angle  [-0.01000,0.01000] 
                    : 'Del-displ. of Sect. (rot. about radial line)')
     ;

 TCGD
      :      'The detector component : TPC
              Constants characterizing the TPC globally. 
              IMPORTANT: IF YOU CHANGE ANY CONSTANTS IN THIS
              BANK, PLEASE MAKE SURE NOT TO ROUND OFF ANY
              CONSTANTS WHEN REINTRODUCING THE BANK IN THE
              DATABASE BY WAY OF DATA CARDS!!  The precision
              indicated below should be respected. (LTC)'
 
        SIZE 1,1
 
     =  (ValRng(2)   = Date           : 'Beginning & expiration date',
         CmpNam      = Name   'TP  '      : 'Name of the component',
         NSubcomp    = INTE    2          : 'number of subcomponents',
         NofCro      = INTE   21          : 'Number of TPC crowns',
         NofEhp      = INTE    2          : 'Number of edge half pads',
         AngSpa      = Angle  [1.0000000, 1.1] : 'Angular spacing
                                     of identical sector types (60 deg)',
         RadStp      = Length [6.390,6.41] : 'Radial pad step',
         PadHgt      = Length [2.990,3.01] : 'Pad height',
         PadSep      = Length [0.660000,0.68] : 'Pad sep. (center-cent)',
         PadWid      = Length [0.590000,0.63] : 'Pad width',
         GapWid      = Length [0.040000,0.06] : 'Gap width',
         EdgpWd      = Length [0.280000,0.30] : 'Edge (half-) pad width',
         EdgSep      = Length [0.500000,0.60] : 'Separation of
                                           last full pad to 1. edge pad',
         BorDis      = Length [1.000000,1.20] : 'Distance of outer edge
                                of edge pad from sector separation line',
         TpdStp      = Length [6.390,6.41]  : 'Trigger pad rad. step',
         TpdHgt      = Length [0.620,0.64]  : 'Trigger pad height',
         WirPit      = Length [0.3900,0.41]  : 'Sense wire spacing',
         WirDia      = Length [0.0000,0.003] : 'Sense wire diameter',
         TpcOut(3)   = Length [25.00,250.] : '3 outer dimensions of
                                             TPC- cylinder',
         TpcThk(3)   = Length [0.50,30.0]  : '3 wall thicknesses of TPC',
         WhlThk      = Length [7.00,9.0]   : 'Thickness (in z) of rib of
                                     sector support structure ("wheel")',
         WhlEqv      = Length [1.00,2.0]   : 'Equiv. thickness of rib',
         WhlWid      = Length [6.00,7.0]   : 'Width of rib',
         WhlKin(3)   = Length [1.00,9.0]   : '3 offsets for rib-kinks',
         FraThi      = Length [0.700000,1.10] : 'Thickness of frame on
                                               which wires are mounted')
      ;
 
 
 
 TSCO
      :      'The detector subcomponent : TPC
              Constants characterizing a TPC-ENDPLATE globally
              (needs 2 entries) (STC)
              Used for pre-1997 alignment'
 
        SIZE 2,2
 
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         ScName      = Name 'A   '|'B   '  : 'Name of a TPC endplate',
         NSlots      = INTE 18        : 'Number of slots/subcomp.',
         RelPos(3)   = Length [-300.0000,300.]
                     : 'Position of center of Endpl. in Global Syst.',
         AdsZax      = Angle  [-0.01000,0.01000]
                     : 'Phi displ. of Endplate (rot. ab. center
                        perpendicular to z - axis)',
         AdsXpl      = Angle  [-0.01000,0.01000]
                     : '"Theta displ." of Endpl. (rot. perp. to x-ax.)',
         AdsYpl      = Angle  [-0.01000,0.01000]
                     : '"Theta displ." of Endpl. (rot. perp. to y-ax.)',
         SiGn        = INTE [-1,1]        : ' SiGn of some thing')
      ;
 
 
 TPOS:     'Position of tracking chambers (TPC+ITC) in
            ALEPH coordinates.(STC)
            Used for pre-1997 alignment'
 
        SIZE 1,1
 
     = (ValRng(2),
        TransLation(3) = Length [-10.00,10.]     :'Position of the ITC,TPC centers in the ALEPH
                                                   coordinate system. (x,y,z)',
        RotaTion(3)    = Angle [-.1000,.1]       :'Orientation of the TPC in the ALEPH
                                                   coordinate system.  Three angles
                                                   in radians (phix, phiy, phiz). The orientation of
                                                   the chambers is produced first by rotating about
                                                   the x axis by phix, then about the new y axis by
                                                   phiy, then about the new z axis by phiz.  To go
                                                   from the TPC+ITC system to the ALEPH system, first
                                                   apply the rotation and then the translation.\
                                                   Xg= M*Xt + TransLation, where Xg and Xt are vectors
                                                   in the global ALEPH system and the tracking chamber
                                                   system, respectively, and M is a matrix obtained from
                                                   RotaTion.');

 TNCO
      :      'The new detector subcomponent : TPC
              Constants characterizing a TPC-ENDPLATE globally
              (needs 2 entries).  Used for 1997-style calib.
              (STC)'
 
        SIZE 2,2
 
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         ScName      = Name 'A   '|'B   '  : 'Name of a TPC endplate',
         NSlots      = INTE 18        : 'Number of slots/subcomp.',
         RelPos(3)   = Length [-300.0000,300.]
                     : 'Position of center of Endpl. in Global Syst.',
         AdsZax      = Angle  [-0.01000,0.01000]
                     : 'Phi displ. of Endplate (rot. ab. center
                        perpendicular to z - axis)',
         AdsXpl      = Angle  [-0.01000,0.01000]
                     : '"Theta displ." of Endpl. (rot. perp. to x-ax.)',
         AdsYpl      = Angle  [-0.01000,0.01000]
                     : '"Theta displ." of Endpl. (rot. perp. to y-ax.)',
         SiGn        = INTE [-1,1]        : ' SiGn of some thing')
      ;
 
 
 TNOS:     'New position of tracking chambers (TPC+ITC) in
            ALEPH coordinates.  Used for 1997-style alignment.
            (STC)'
 
        SIZE 1,1
 
     = (ValRng(2),
        TransLation(3) = Length [-10.00,10.]     :'Position of the ITC,TPC centers in the ALEPH
                                                   coordinate system. (x,y,z)',
        RotaTion(3)    = Angle [-.1000,.1]       :'Orientation of the TPC in the ALEPH
                                                   coordinate system.  Three angles
                                                   in radians (phix, phiy, phiz). The orientation of
                                                   the chambers is produced first by rotating about
                                                   the x axis by phix, then about the new y axis by
                                                   phiy, then about the new z axis by phiz.  To go
                                                   from the TPC+ITC system to the ALEPH system, first
                                                   apply the rotation and then the translation.\
                                                   Xg= M*Xt + TransLation, where Xg and Xt are vectors
                                                   in the global ALEPH system and the tracking chamber
                                                   system, respectively, and M is a matrix obtained from
                                                   RotaTion.');
 
 TSLO
  :      'TPC slot (sector) position (needs 36 entries) (STC)
          Used for pre-1997 alignment'
 
        SIZE 36,36
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         SloNam      = Name           : 'The name of a slot position',
         SlonmB      = INTE [1,36]    : 'Slotnumber in Reconstruction',
         SuperSect   = INTE [1,12]
                     : 'Sector Number (given type) = "Supersector-nr" ',
         rDsSlo      = Length [-0.5000,0.5000]
                     : 'Displacement of Sector in r - direction',
         AdsSlo      = Angle  [-0.01000,0.01000]
                     : 'Phi-displ. of Sect. (rot. about center of TPC)',
         RotSlo      = Angle  [-0.01000,0.01000]
                     : 'Rot. of Sector about its center in order to
         align its symm. axis with the radial line. (The center of a
         sector is on its symm. axis at a radius which is the
         arithmetic mean of its two outermost padrow radii)')
     ;
 
 
 
 TMTY
      :      'TPC module type (sector type)
              Constants characterizing the 3 types of active elements:
              pads, triggerpads and wires. (LTC)'
 
        SIZE 3,3
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         Modtyp      = Name                 : 'Module type',
         PhiPos      = Angle  [0.,0.530000] : 'Phi-Position of
                           first slot of this sector type (0 or 30 deg)',
         RFpdrw      = Length [39.,101.000] : 'Rad. of 1. padrow (cntr)',
         NtPdrw      = INTE 9|12            : 'Number of padrows',
         PadspR(12)  = INTE 0|[55,160]      : 'Equiv. num. of full
                           pads/padrow (2 edge-pads are counted as 1)',
         RTpdrw      = Length [42.,104.000] : 'Rad. of 1. Tpadrow(cntr)',
         NtpTrw      = INTE  8|11           : 'Number of Tpadrows',
         TppRow(11)  = INTE  0|2|4          : 'Number of Tpads/tpadrow',
         AngTpa      = Angle  [0.1,0.300000]: 'Phi-Pos of boundary
                               between 1. and 2. Tpad w.r.t. symm. axis',
         TpdCst(4)   = INTE   [1,32]        : 'Constants for computation
                                               of Tpad channel#',
         PWire1      = Length [32.000,96.0] : 'Position of 1. wire',
         NtWire      = INTE   156|196       : 'Number of sense wires',
         ElwirF      = INTE   [3,8]         : 'Number of the sense wire
       on the first electr. channel (connected to all preceding wires)',
         ElwirL      = INTE   [156,194]     : 'Number of the sense wire
       on the last  electr. channel (connected to all subsequent wires)',
         WirFir(4)   = INTE   [0,190]       : 'number of the first
                            sense wire in a REGION (c.f. SUBR. TGEWIR)')
      ;
 
 
 
 TPMO
      :      'TPC physical module (sector)
              Characteristics of the physical TPC - modules
              (individual sectors)
              (needs 3 * 14 entries)
              (LTC) '
 
        SIZE 42,42
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         HwName      = Name        : 'Name of Hardw. mod. (like "W8")')
      ;
 
 
 
 TSGM
      :      'The TPC sector fiducial area geometry
             (needs 3 entries). (LTC)'
 
        SIZE 3,3
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date',
         NtCorn      = INTE 4|5    : 'Number of corners of fid. area
                                      on either side of the symm. axis
                                     (first coord. is at bottom/center)',
         XCorn(5)    = Length [0.,200.0000]  : 'X-Coord. of the corners',
         YCorn(5)    = Length [0.,200.0000]  : 'Y-Coord. of the corners')
      ;
 
 
 TALI
      :      'The TPC slot alignment information. (LTC)'
 
        SIZE 1,1
 
     =  (ValRng(2)   = Date           : 'Beg. & expiration date')
      ;
 
 
 TDEF
      :      'The TPC hardware deficiencies/physical module. (LTC)'
 
        SIZE 1,1
 
      = (ValRng(2))
      ;
 
 
 END ESET
 
 DEFINE RSET
 
 (TSCO [0,1] -> [0,2] TCGD)
             : 'The TPC has two subcomponents'
             ;

 (TNCO [0,1] -> [0,2] TCGD)
             : 'The TPC has two subcomponents'
             ;
 
 (TPOS [0,1] -> [0,1] TCGD)
             : 'TPOS contains the ITC/TPC position'
             ;
 
 (TPOS [0,1] -> [0,1] ITCC)
             : 'TPOS contains the ITC/TPC position'
             ;
 
 (TNOS [0,1] -> [0,1] TCGD)
             : 'TPOS contains the ITC/TPC position'
             ;
 
 (TNOS [0,1] -> [0,1] ITCC)
             : 'TPOS contains the ITC/TPC position'
             ;
 
 (TSLO [1,1] -> [1,*] TSCO)
             : 'A TPC subcomponent has many slots'
             ;
 
 (TSLO [1,1] -> [0,*] TMTY)
             : 'In a given slot there is one module type'
             ;
 
 (TALI [1,1] -> [1,1] TSLO)
             : 'For each slot there is alignment information'
             ;
 
 (TDEF [1,1] -> [1,1] TPMO)
             : 'Deficiencies for each physical module'
             ;
 
 (TPMO [0,1] -> [0,1] TSLO)
             : 'A physical module sits in one slot'
             ;
 
 (TPMO [1,1] -> [0,*] TMTY)
             : 'A physical module is of a given module type'
             ;
 
 (TSGM [1,1] -> [1,1] TMTY )
             : 'The wire lengths belong to a TPC sector'
             ;
 
 (TNLO [1,1] -> [1,*] TNCO)
             : 'A TPC subcomponent has many slots'
             ;
 
 (TNLO [1,1] -> [0,*] TMTY)
             : 'In a given slot there is one module type'
             ;
 
 
 END RSET
 
 
 END SUBSCHEMA
 
 
 SUBSCHEMA VDETGEOM                             
 :'VDET part of the ALEPH Detector Description' 
                                                
                                  
 AUTHOR   'J.Rothberg,A.Bonissent,G.Taylor'
 REVIEWER 'J.Rothberg'
 VERSION  '3.5'     
 DATE     '09/10/95'
                                                
                                                
 DEFINE ESET       
                             
 VDLA
      :      'VDet LAyers (NR=setup code)\
              Number of columns \
              Number of rows = number of layers'

           STATIC

           SIZE 1,1

  
= (RAdius    = REAL [*,*]    :' RWVDLA  Perp dist bet z axis and wafer plane',
TiltAngle    = REAL [*,*]    :' WATILT  Wafer Tilt Angle (rad)',
ORientation  = INTE [*,*]    :' IORIEN  Wafer Orientation =1 rphi in, =2 z in')
;

 VRDO
      :      'ReaDOut Configuration (NR=setup code)\
              Number of columns \
              Number of rows = 1'
 
           STATIC
 
           SIZE 1,1
 
 
=(nRdstrZ = INTE [*,*] :' NRDSTZ  Nmbr of rdout strips per wafer, z side',
nRdstrP   = INTE [*,*] :' NRDSTP  Nmbr of rdout strips per wafer, r-phi side',
rdFrqZ    = INTE [*,*] :' NREFRZ  Readout strip frequency, z side',
rdFrqP    = INTE [*,*] :' NREFRP  Readout strip freq, r-phi side',
OffsetZ   = INTE [*,*] :' NOFRDZ  Offst (phys strps)  1st rdout strp,z',
OffsetP   = INTE [*,*] :' NOFRDP  Offst (phys strps)  1st rdout strp,r-phi',
rdStchZ   = INTE [*,*] :' NZRSSC  Nmbr of rdout strips per strip chnl, z ',
elchDirZ  = INTE [*,*] :' IECORZ  Elec chan nmbr dir (z) w.r.t. strip chnls',
elchDirP  = INTE [*,*] :' IECORP  Elec chan nmbr dir (r-phi) wrt strip chnls',
elchElmZ  = INTE [*,*] :' NZEROM  Nmbr of elec chnls per elec module, z',
elchElmP  = INTE [*,*] :' NPEROM  Nmbr of elec chnls per elec module, r-phi',
WafrBits  = INTE [2,3] :' NWFBIT  Nmbr of bits for wafer in hit addresses')
;
 
 VSLT
      :      'Vdet SLoTs (NR=setup code)\
              Number of columns \
              Number of rows = number of slots'

           STATIC

           SIZE 1,1

=(LAyerno = INTE [*,*]  :' JJLAYF(JFAC)  Layer number for slot/face JFAC',
PHiface   = REAL [*,*]  :' PHIOFF(JFAC)  phi(rad) face nrml (out frm z axis)',
SlotStat  = INTE [*,*]  :' ISSFLG(JFAC)  Face serial num (=0 if empty slot)')
;

 VWGE
      :      'Vdet Wafer GEometry (NR=setup code)\
              Number of columns \
              Number of rows = 1'

           STATIC

           SIZE 1,1

=(SizeA     = REAL [*,*]    :'WSIZEA   Wafer dimen(cm) along a direction',
SizeB       = REAL [*,*]    :'WSIZEB   Wafer dimen(cm) along b direction',
NumbsZ      = INTE [*,*]    :'NZSTRP   Nmbr of phys strips per wafer, z', 
NumbsP      = INTE [*,*]    :'NPSTRP   Nmbr of phys strips per wafer, r-phi', 
PitchZ      = REAL [*,*]    :'STPITZ   Phys strip pitch (cm), z side',
PitchP      = REAL [*,*]    :'STPITP   Phys strip pitch (cm), r-phi side',
LengthZ     = REAL [*,*]    :'STLENZ   Strip length(cm), z side',
LengthP     = REAL [*,*]    :'STLENP   Strip length(cm), r-phi side',
minAZ       = REAL [*,*]    :'AMNSRZ   Min a coord(cm) sens region, z', 
minAP       = REAL [*,*]    :'AMNSRP   Min a coord(cm) sens region, r-phi', 
minBZ       = REAL [*,*]    :'BMNSRZ   Min b coord(cm) sens region, z ',
minBP       = REAL [*,*]    :'BMNSRP   Min b coord(cm) sens region, r-phi', 
WaferThick  = REAL [*,*]    :'WTHICK   Wafer thickness (cm)')
;

 VZPW
      :      'Vdet Z Positions, Wafer (NR=setup code)\
              Number of columns \
              Number of rows = number of wafers in face'

           STATIC

           SIZE 1,1

        =(ZCoord     = REAL [*,*]   :'WAFERZ(IWFF)  z coord(cm) of wafer-in-face IWFF')
        ;
 VATT
      :      'attribute setting of VDET volumes, NR=setup no. (LTC)\
              number of words/setting\
              number of settings'
           STATIC
 
      = (VOlumename         = CHA4           : 'volume name of the volume
                                                which attribute to be set',
         AttributeName      = CHA4           : 'attribute name to be set',
         AttributeValue     = INTE           : 'Value of attribute ')
       ;
 
 VMAT                                                                                                                           
       :      'Additional material constants, NR=setup no.(LTC)\                                                                  
               number of words/material\                                                                                          
               number of materials  '                                                                                             
            STATIC                                                                                                                
                                                                                                                                  
       = (MatName      = CH16              : 'Material Name',                                                                     
          AtomWeight   = REAL [0.,300.000]  : 'atomic weight',                                                                    
          AtomNumber   = REAL [0.,110.000]  : 'atomic number',                                                                    
          DEnsity      = REAL [0.,20.000000]: 'density [gr/cm**3]',                                                               
          RadLength    = REAL [0.000,*]     : 'radiation length [cm]',                                                            
          AbsLength    = REAL [0.000,*]     : 'absorption length [cm]')                                                           
       ;                                                                                                                          
                                                                                                                                  
 VMEL                                                                                                                             
      :      'definition of elements used in mixtures, NR=setup no.(LTC)\                                                         
              number of words/element\                                                                                            
              number of elements '                                                                                                
           STATIC                                                                                                                 
                                                                                                                                  
      = (VMIXrownumber = INTE               : 'VMIXrownumber',                                                                    
         ELementname   = CH16              : 'element name',                                                                      
         NUmbofatomol  = INTE               : 'number of atoms per mol')                                                          
      ;                                                                                                                           
                                                                                                                                  
 VMIX                                                                                                                             
      :      'definition of mixtures, NR=setup no. (LTC) \                                                                        
              number of words/mixture\                                                                                            
              number of mixtures'                                                                                                 
          STATIC                                                                                                                  
                                                                                                                                  
      = (MixtureName   = CH16               : 'mixture name',                                                                     
         MixtureType   = CHA4               : 'mixture type ELEM or MATE',                                                        
         DEnsity       = REAL               : 'density of mixture of 
                                              ELEMents or fac 
                                              (dens=fac*dens)for MATErials')
      ;                                                                                                                           
                                                                                                                                  
 VMMA                                                                                                                             
      :      'definition of materials used in mixtures, NR=setup no. (LTC)\                                                       
              number of words/material\                                                                                           
              number of materials'                                                                                                
           STATIC                                                                                                                 
                                                                                                                                  
      = (VMIXrownumber = INTE               : 'VMIXrownumber',                                                                    
         MAterialname  = CH16               : 'material name',                                                                    
         thickness     = REAL               : 'thickness of the material')                                                        
      ;                                                                                                                           
                                                                                                                                  
 VTME                                                                                                                             
      :      'definition of tracking media, NR=setup no. (LTC) \                                                                  
              number of words/tracking medium\                                                                                    
              number of tracking media'                                                                                           
           STATIC                                                                                                                 
                                                                                                                                  
      = (TrackmedNam         = CH16            : 'tracking medium name',                                                          
         MAterialname        = CH16            : 'material name',                                                                 
         tmaxFD              = REAL            : 'max angle due to field',                                                        
         dmaxMS              = REAL            : 'max displacement due to 
                                                  X-scattering',                                                  
         DEemax              = REAL            : 'max fractional energy loss',                                                    
         EPsil               = REAL            : 'tracking precision',                                                            
         STmin               = REAL            : 'min step due to X-scat 
                                                  or eloss',                                                  
         isVOl               = INTE            : '=0 no sensitive, 
                                                  =1 sensitive',
         GPar                = INTE            : '=0 no VGPA, =1 VGPA',                                                           
         ifieldFLag          = INTE            : '=0 no mag.field, 
                                                  =1 no unif.B,                                                  
                                                  =3 uniform B')                                                                  
      ;                                                                                                                           
                                                                                                                                  
 VGPA                                                                                                                             
      :     'definition of some parameters of tracking media,  
             NR=setup no. (LTC)\                                                  
             number of words/parameter\                                                                                           
             number of redefined parameters'                                                                                      
          STATIC                                                                                                                  
                                                                                                                                  
      = (VTMErownumber       = INTE            : 'VTME row number',                                                               
         PArametername       = CHA8            : 'parameter name',                                                                
         ParameterValue      = REAL            : 'parameter value')                                                               
      ;                                                                                                                           
                                                                                                                                  
 VVOL                                                                                                                             
      :     'definition of VDET volumes, NR=setup no. (LTC)\                                                                      
             number of words/volume\                                                                                              
             number of volumes'                                                                                                   
          STATIC                                                                                                                  
                                                                                                                                  
      = (VOlumename         = CHA4           : 'volume name',                                                                     
         SHapename          = CHA4           : 'shapename',                                                                       
         VTMErownumber      = INTE           : 'VTME row number ',                                                                
         NumofParame        = INTE           : 'number of parameters',                                                            
         PArameters(5)      = REAL           : 'list of parameters')                                                              
      ;                                                                                                                           
                                                                                                                                  
 VROT                                                                                                                             
      :     'definition of VDET rotation matrices, NR=setup no. (LTC)\                                                            
             number of words/rotation\                                                                                            
             number of rotations'                                                                                                 
          STATIC                                                                                                                  
                                                                                                                                  
      = (Theta1             = REAL           : 'azimuthal angle for axis 1',                                                      
         Phi1               = REAL           : 'polar angle for axis 1',                                                          
         Theta2             = REAL           : 'azimuthal angle for axis 2',                                                      
         Phi2               = REAL           : 'polar angle for axis 2',                                                          
         Theta3             = REAL           : 'azimuthal angle for axis 3',                                                      
         Phi3               = REAL           : 'polar angle for axis 3')                                                          
      ;                                                                                                                           
                                                                                                                                  
 VPOS                                                                                                                             
      :      'positions of VDET volumes, NR=setup no. (LTC)\                                                                      
              number of words/positions\                                                                                          
              number of positions'                                                                                                
           STATIC                                                                                                                 
                                                                                                                                  
      = (DAuthername        = CHA4           : 'volume name of the 
                                                volume to be positioned',
         MOthername         = CHA4           : 'volume name of the mother ',                                                      
         FLag               = CHA4           : 'geant flag ONLY or MANY',                                                         
         VROTrownumber      = INTE           : 'VROT row number 
                                               (rotation matrix no.) or 0',
         CopyNumber         = INTE           : 'copy number of the volume
                                               volume placed',
         POsition(3)        = REAL           : 'x y z position of the volume')                                                    
       ;                                                                                                                          
                                                                                                                                  
 VPHO
        : 'VDET pulseheight structure description bank NR=setup code(LTC)\
           number of words /row\
           number of rows = 1'
           STATIC
           SIZE 1,1
 
         = (PhOffset    = INTE [0,*]         :' Pulseheight offset in VPLH',
            SideStrips  = INTE [0,100]       :' Number of strips readout
                                                on either side of hit',
            AdcBits     = INTE [0,100]       :' Number of bits in ADC',
            DataBits    = INTE [0,100]       :' Number of bits in readout')
            ;
                                                                                                                                  
 VCOD                                                                                                                             
      :      'Constants characterizing a VDET-layer globally.NR=setup code (LTC)\ 
              Number of Columns\                                                                                                  
              Number of layers'                                                                                                   
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 1,1                                                                                                                  
                                                                                                                                  
      = (NumbofFaces        = INTE [1,15]     : 'Numb. of existing faces 
                                                 per layer',             
         ORientation        = INTE [1,2]      : 'Orientation of side:    
                                                 1 --> z ext r-phi int   
                                                 2 --> z int r-phi ext',       
         NumbofSlots        = INTE [1,15]     : 'Maximum number of slots/layer')        
      ;                                                                                                                           
                                                                                                                                  
 VWGM                                                                                                                             
      :      'The VDET Wafer geometry (Zed and Phy).NR=setup code (MTC)\
              Number of Columns\                                                                                                  
              Number of wafer geometries'                                                                                         
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 4,4                                                                                                                  
                                                                                                                                  
      = (NStrips(2)  = INTE   [200,1500]     : 'Num. readout strips',                                                             
         DImension(2)= Length [5.00,5.20]    : 'Wafer ext dimension',                                                             
         ReadStrip(2)= INTE   [1,4]          : 'Readout strip frequency',                                                         
         ReadPitch(2)= Length [.0060,.0400]  : 'Readout strip pitch',                                                             
         StripLen(2) = Length [4.00,5.20]    : 'Strip length',                                                                    
         ThicKness   = Length [.020,.040]    : 'Wafer thickness',                                                                 
         ApplVolt    = REAL   [20.0,200.0]   : 'Applied voltage ',                                                                
         DeplVolt    = REAL   [20.0,200.0]   : 'Depletion voltage ')                                                              
     ;                                                                                                                            
                                                                                                                                  
 VDEP
      :      'GALEPH Vdet electronic parameters, NR=setup code \
              Number of items\
              Number of rows=1 '
           STATIC
 
      = (ElecMobil    = REAL [100.,10000.] : 'Electron mobility in silicon',
         HoleMobil    = REAL [100.,10000.] : 'Hole mobility in silicon',
         ElecTemp     = REAL [0.0100,0.1000]: 'Electron temperature in silicon',
         HoleTemp     = REAL [0.0100,0.1000]: 'Hole temperature in silicon',
         HallEMobil   = REAL [0.,10000.]   : 'Hall electron mobility in silicon',
         HallHMobil   = REAL [0.,10000.]   : 'Hall hole mobility in silicon',
 
         ApplVolt    = REAL   [20.0,200.0]   : 'Applied voltage ',                                                                
         DeplVolt    = REAL   [20.0,200.0]   : 'Depletion voltage ',                                                              

         SignalThres(2)  = INTE [0,20]          : 'Threshold in acquisition(Count)\
                                                      , for each view',      
         ElectCount(2)   = REAL [0.,2000.]      : 'Electrons per count\
                                                      , for each view',      
         NoiseLatclu      = INTE [0,5]            : 'Strips to cluster on each side',        
         NoiseCorr(2)     = INTE [0,5]            : 'Strips for correlation noise \
                                                        for each view',   
         SignalNeigh(2)   = INTE [0,5]            : 'Nb of neighbours to take \
                                                     into account for signal \
                                                     for each view',   
         NoiseElec(2)       = REAL [0.0,100.0]      : 'Noise sigma in electron/pF \
                                                      , for each view',      
         ParalNoise(2)      = REAL [0.0,5000.]      : 'Parallel noise sigma in electron \
                                                      , for each view',      
         InputCapac(2)      = REAL [0.0,100.0]      : 'Input capacitance in picofarad\
                                                      , for each view',      
         MultFact        = INTE [1,*]    : 'Multiplication factor before \
                                            conversion from electrons to Gev ',
         ElectinGev      = REAL [1.E-9,9.E-9]    : 'Gev equivalent at an electron ')
       ;                                                                                                                          

 VDSI                                                                                                                             
      :      'Vdet read out sign, NR=setup code (MTC)\                                                                            
              Number of words/wafer\                                                                                              
              Number of unique wafers by readout'                                                                                 
           STATIC                                                                                                                 
                                                                                                                                  
           SIZE 1,1                                                                                                               
                                                                                                                                  
      = (RSign(4,2)     = INTE[-1,1]         : 'Read out sign by wafer,layer')                                                    
                                                                                                                                  
      ;                                                                                                                           
                                                                                                                                  
 VELE                                                                                                                             
      :      'Minivertex GALEPH parameters. NR>0 (DROP) \                                                                               
              Number of electronic features\                                                                                      
              Number of electronic type '                                                                                         
           STATIC                                                                                                                 
                                                                                                                                  
           SIZE 1,1                                                                                                               
                                                                                                                                  
      = (SignalThres  = INTE [0,20]        : 'Threshold in acquisition (Count)',                                                  
         ElectCount   = REAL [0.,2000.]    : 'Electrons per count',                                                               
         NoiseLatclu  = INTE [0,5]         : 'Strips to cluster on each side',                                                    
         NoiseCorr    = INTE [0,5]         : 'Strips for correlation noise ',                                                     
         NoiseElec    = REAL [0.0,100.0]   : 'Noise sigma in electron/pF',                                                        
         ParalNoise   = REAL [0.0,5000.]   : 'Parallel noise sigma in 
                                              electron',                                                  
         InputCapac   = REAL [0.0,100.0]   : 'Input capacitance in picofarad')                                                    
       ;                                                                                                                          
                                                                                                                                  
 VDCC
      :      'GALEPH Vdet coupling coefficients, NR=setup code \
              Number of items\
              Number of rows=nb of neighbours taken into consideration '
           STATIC
 
      = (VIew             = INTE [1,2]    : 'View',
         DIstance         = INTE[1,20]    : 'Distance (in pitch) between the two neighbours',
         SignalCoupling   = REAL [0.0,1.0]: 'Capacitive coupling for signal',
         NoiseCoupling    = REAL [0.0,1.0]: 'Capacitive coupling for noise')
      ;

 VDEL                                                                                                                             
      :      'electronic parameters used in GALEPH. NR=0 (DROP)
              **_A.Bonissent_23/11/94_**
              This bank is used by the old Galeph and will be dropped in the future \                   
              Number of words/setup code\                                                                                      
              Number of setup code '                                                                                         
           STATIC                                                                                                                 
                                                                                                                                  
           SIZE 3,3                                                                                                               
                                                                                                                                  
      = (SignalThres(2)  = INTE [0,20]          : 'Threshold in acquisition (Count)\
                                                   1=Munich,2=Pisa',                                                  
         ElectCount(2)   = REAL [0.,2000.]      : 'Electrons per count
                                                   1=Munich,2=Pisa',                                                               
         NoiseLatclu     = INTE [0,5]            : 'Strips to cluster on each side',  
         NoiseCorr       = INTE [0,5]            : 'Strips for correlation noise ',   
         NoiseElec       = REAL [0.0,100.0]      : 'Noise sigma in electron/pF',      
         ParalNoise      = REAL [0.0,5000.]      : 'Parallel noise sigma in 
                                                    electron',                                                  
         InputCapac      = REAL [0.0,100.0]      : 'Input capacitance in picofarad',
         PulseheightCut  = REAL                  : 'pulseheight cut',                                                    
         ElectinGev      = REAL [1.E-9,9.E-9]    : 'Gev equivalent at an electron ')
       ;                                                                                                                          
                                                                                                                                  
 VPAR
      :      'GALEPH Hall electron parameters, NR=1 (LTCGAL) \
              Number of items\
              Number of rows '
           STATIC
 
           SIZE 1,1
 
      = (ElecMobil    = REAL [100.,10000.] : 'Electron mobility in silicon',
         HoleMobil    = REAL [100.,10000.] : 'Hole mobility in silicon',
         ElecTemp     = REAL [0.0100,0.1000]: 'Electron temperature in silicon',
         HoleTemp     = REAL [0.0100,0.1000]: 'Hole temperature in silicon',
         HallEMobil   = REAL [0.,10000.]   : 'Hall electron mobility in silicon',
         HallHMobil   = REAL [0.,10000.]   : 'Hall hole mobility in silicon',
         ElectinGev   = REAL [1.E-9,9.E-9] : 'Gev equivalent at an electron ')
       ;
 
 VCPC                                                                                                                             
      :      'The VDET capacitive coupling constants for the signal                                                                
              on the phi side . NR=setup code(LTCGAL)\                                                                                            
              Number of coupling capacitances considered\                                                                         
              Number of wafer geometries'                                                                                         
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 4,4                                                                                                                  
                                                                                                                                  
      = (pCapcou0      = REAL   [0.,20.000]   : 'Coupling at 0 P pitch',                                                          
         pCapcou1      = REAL   [0.,20.000]   : 'Coupling at 1 P pitch',                                                          
         pCapcou2      = REAL   [0.,20.000]   : 'Coupling at 2 P pitch',                                                          
         pCapcou3      = REAL   [0.,20.000]   : 'Coupling at 3 P pitch',                                                          
         pCapcou4      = REAL   [0.,20.000]   : 'Coupling at 4 P pitch',                                                          
         pCapcou5      = REAL   [0.,20.000]   : 'Coupling at 5 P pitch',                                                          
         pCapcou6      = REAL   [0.,20.000]   : 'Coupling at 6 P pitch',                                                          
         pCapcou7      = REAL   [0.,20.000]   : 'Coupling at 7 P pitch')                                                          
      ;                                                                                                                           
                                                                                                                                  
 VCZC                                                                                                                             
      :      'The VDET capacitive coupling costants for the signal                                                                
              on the Z side .NR=setup code (LTCGAL)\                                                                                               
              Number of coupling capacitances considered\                                                                         
              Number of wafer geometries'                                                                                         
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 4,4                                                                                                                  
                                                                                                                                  
      = (zCapcou0      = REAL   [0.,20.000]   : 'Coupling at 0 Z pitch',                                                          
         zCapcou1      = REAL   [0.,20.000]   : 'Coupling at 1 Z pitch',                                                          
         zCapcou2      = REAL   [0.,20.000]   : 'Coupling at 2 Z pitch',                                                          
         zCapcou3      = REAL   [0.,20.000]   : 'Coupling at 3 Z pitch',                                                          
         zCapcou4      = REAL   [0.,20.000]   : 'Coupling at 4 Z pitch',                                                          
         zCapcou5      = REAL   [0.,20.000]   : 'Coupling at 5 Z pitch',                                                          
         zCapcou6      = REAL   [0.,20.000]   : 'Coupling at 6 Z pitch',                                                          
         zCapcou7      = REAL   [0.,20.000]   : 'Coupling at 7 Z pitch')                                                          
      ;                                                                                                                           
                                                                                                                                  
 VNPC                                                                                                                             
      :      'The VDET noise coupling costants on the phi side.NR=setup code (LTCGAL)\
              Number of noise coupling constant considered\                                                                       
              Number of wafer geometries'                                                                                         
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 4,4                                                                                                                  
                                                                                                                                  
      = (pNoicou0      = REAL   [0.,20.000]    : 'Coupling at 0 P pitch',                                                         
         pNoicou1      = REAL   [-20.000,0.]   : 'Coupling at 1 P pitch',                                                         
         pNoicou2      = REAL   [-20.000,0.]   : 'Coupling at 2 P pitch',                                                         
         pNoicou3      = REAL   [-20.000,0.]   : 'Coupling at 3 P pitch')                                                         
      ;                                                                                                                           
                                                                                                                                  
 VNZC                                                                                                                             
      :      'The VDET noise coupling costants on the z side.NR=setup code (LTCGAL)\                                                              
              Number of noise coupling constant considered\                                                                       
              Number of wafer geometries'                                                                                         
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 4,4                                                                                                                  
                                                                                                                                  
      = (zNoicou0      = REAL   [0.,20.000]    : 'Coupling at 0 Z pitch',                                                         
         zNoicou1      = REAL   [-20.000,0.]   : 'Coupling at 1 Z pitch',                                                         
         zNoicou2      = REAL   [-20.000,0.]   : 'Coupling at 2 Z pitch',                                                         
         zNoicou3      = REAL   [-20.000,0.]   : 'Coupling at 3 Z pitch')                                                         
      ;                                                                                                                           
                                                                                                                                  
 VGMD                                                                                                                             
      :      'The VDET slot geometry.NR=setup code (LTC)\                                                                                      
              Number of Columns\                                                                                                  
              Number of slot geometries'                                                                                          
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 4,4                                                                                                                  
                                                                                                                                  
      = (NWaffs       = INTE 4                : 'Numb. of wafers',                                                                
         SideDim(2)   = Length                : 'Side z,r-phi dimens',                                                            
         NCeram       = INTE 2                : 'Numb. of ceramics')                                                              
      ;                                                                                                                           
                                                                                                                                  
 VDSL                                                                                                                             
      :      'VDET slot (side) position .NR=setup code (LTC)\                                                                                  
              Number of Columns\                                                                                                  
              Number of slots'                                                                                                    
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 1,27                                                                                                                 
                                                                                                                                  
      = (SlotId       = INTE   [1,27]         : 'physical Slot number ',                                                          
         DisPhi       = Angle                 : 'Displacement of Side in phi')                                                    
                                                                                                                                  
      ;                                                                                                                           
                                                                                                                                  
 VDME                                                                                                                             
      :      'The mechanical support of VDET. NR=setup code (LTC)\                                                                             
              Number of Columns\                                                                                                  
              Number of mechanic supports'                                                                                        
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 2,2                                                                                                                  
                                                                                                                                  
      = (ApothemLen(2)= Length [8.000,13.000] : 'Radius of inner layer',                                                          
         TIltAngle(2) = Angle                 : 'Tilt angle (rad)inner layer',                                                    
         ThicknessRing= Length [.1000,1.000]  : 'Thick. of ring',                                                                 
         ExteRing(2)  = Length [-20.00,20.00] : 'Int-ext negative rings',                                                         
         CaBlethick   = Length [.0100,.1000]  : 'Cable equivalent thickness',                                                     
         CfAngle      = Angle                 : 'Carbon fiber tube angle',                                                        
         CfRadius     = Length [.10,1.00]     : 'Carbon fiber tube radius',                                                       
         CfThickness  = Length [.010,.100]    : 'Carbon fiber tube thick.',                                                       
         SectorAngle  = Angle                 : 'Angle covered by a sector',                                                      
         GapAngle     = Angle                 : 'Angle covered by a gap',                                                         
         SectorPosit  = Angle                 : 'Angular position of the                                                          
                                                 first sector',                                                                   
         NumberSector = INTE   [0,3]          : 'Number of sector installed')                                                     
      ;                                                                                                                           
                                                                                                                                  
 VZEW                                                                                                                             
      :      'The ideal wafers and ceramic position in the                                                                        
              physical module. NR=setup code (LTC)\
              Number of Columns\                                                                                                  
              Number of possible positionings'                                                                                    
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 2,2                                                                                                                  
                                                                                                                                  
      = (WafPos(4)    = Length [-20.000,20.000]  : 'Wafer posit. in Z',                                                           
         CerPos(2)    = Length [-20.000,20.000]  : 'Ceramic posit. in Z')                                                         
      ;                                                                                                                           
                                                                                                                                  
 VHYB                                                                                                                             
      :      'The features of hybrids for VDET.NR=setup code (LTC)\                                                                           
              Number of hybrids features\                                                                                         
              Number of rows (=1)'                                                                                                
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 1,1                                                                                                                  
                                                                                                                                  
      = (LengthOne    = Length [1.00,5.20]    : 'Z side width',                                                                   
         LengthTwo    = Length [0.10,5.20]    : 'Z side position',                                                                
         LengthtHree  = Length [1.00,5.20]    : 'Phi side width',                                                                 
         LengthFour   = Length [0.10,3.20]    : 'Connection length',                                                              
         LengthfIve   = Length [1.00,5.20]    : 'Connection width',                                                               
         CErathickness= Length [.010,0.050]   : 'Thick. of ceramic',                                                              
         ChipWidth    = Length [.10,2.00]     : 'Width of CAMEX',                                                                 
         ChipLength   = Length [.10,2.00]     : 'Length of CAMEX',                                                                
         ChipThickness= Length [.010,0.050]   : 'Thick. of CAMEX',                                                                
         Spacer(2)    = Length [.010,0.200]   : 'Thick. of spacer')                                                               
      ;                                                                                                                           
                                                                                                                                  
 VDAL                                                                                                                             
      :      'The features of the allumina for VDET.NR=setup code (LTC)\
              Number of Columns\                                                                                                  
              Number of rows (=1)'                                                                                                
        STATIC                                                                                                                    
                                                                                                                                  
        SIZE 1,1                                                                                                                  
                                                                                                                                  
      = (IntRadius    = Length [5.00,15.00]   : 'Internal radius',                                                                
         OutRadius    = Length [5.00,15.00]   : 'Outer radius',                                                                   
         LengthAlum   = Length [10.00,50.00]  : 'Length allumina',                                                                
         ThickCar     = Length [0.001,.100]   : 'Thickness carbon',                                                               
         ThickAlum    = Length [0.001,.100]   : 'Thickness alluminum')                                                            
      ;                                                                                                                           
                                                                                                                                  
 VPCO
      :     'definition of VDET shape polycones, NR=setup no. (LTC)\
             number of words/volume\
             number of volumes'
          STATIC
 
      = (VolumindexValue    = INTE           : 'volume index in VVOL',
         ZBvalue            = REAL           : 'Z of dimension change',
         RmiN               = REAL           : 'Rminimum',
         RmaX               = REAL           : 'Rmaximum ')
      ;
 
 VDRL
       :      'Radiation length within a face, NR=setup no.(LTC)\
               number of words/area\
               number of areas  '
            STATIC

       = (ULower   = REAL [-15.,15.]  : 'Lower bound of area in -ve U coord system.
                                         For historical reasons this bank stores
                                         data in term of -U !',
          UHigher  = REAL [-15.,15.]  : 'Higher bound of area in -ve U coord system.
                                         For historical reasons this bank stores
                                         data in term of -U !',
          WLower   = REAL [-15.,15.]  : 'Lower bound of area in W coord system',
          WHigher  = REAL [-15.,15.]  : 'Higher bound of area in W coord system',
          RadLeng  = REAL [0.000,*]   : 'radiation length [cm]',
          IntGeom  = INTE [0,1000]    : 'Flag given which coordinate 
                                         system the above area is in.\
                                         1 = Face ( 4 wafer) system\
                                         2 = Module ( 2 wafer) system\
                                         3 = Face ( 4 wafer) system inner layer\
                                         4 = Face ( 4 wafer) system outer layer')
       ;

 VRLD
       :      'Constants affecting multiple scattering, NR=setup no.(LTC)\
               number of words/region\
               number of regions  '
            STATIC

      = (meanBpipRad         = REAL       : 'radius of the middle of the beam pipe',
         BpipradLength       = REAL       : 'radiation length of the beam pipe',
         ItcinnerRad         = REAL       : 'radius of the middle of the ITC inner wall',
         ItcradLength        = REAL       : 'radiation length seen by the ITC inner wall',
         ItcGasradlen        = REAL       : 'radiation length in the ITC gas per cm',
         meanItcTpcrad       = REAL       : 'radius of a wall between ITC and TPC',
         TpcradLength        = REAL       : 'radiation length seen by this wall',
         TpcGasradlen        = REAL       : 'radiation length in the TPC gas per cm',
         ZInnVdetEndc        = REAL       : 'abs(Z) of discs for inner VDET endcaps',
         ZOutVdetEndc        = REAL       : 'abs(Z) of discs for outer VDET endcaps',
         InnradInnEndc       = REAL       : 'radius inner hole of inner endcap disc',
         InnradOutEndc       = REAL       : 'radius inner hole of outer endcap disc',
         OutradInnEndc       = REAL       : 'outer radius of inner endcap disc',
         OutradOutEndc       = REAL       : 'outer radius of outer endcap disc',
         ScatparInEndc       = REAL       : 'rad len inner VDET endcap disc',
         ScatparOuEndc       = REAL       : 'rad len outer VDET endcap disc')

      ;

VFMC
      :      'VDET Face Module Content, NR=Setup code\
              Number of words/face\
              Number of faces installed'

           STATIC

      = (NAme        = CHA4         : 'Face label identifier', 
         FaceNumber  = INTE [1,99]  : 'Face number identifier',
         ModuleNeg   = INTE [1,99]  : 'Serial number of module at Z<0(side B)',
         ModulePos   = INTE [1,99]  : 'Serial number of module at Z>0(side A)')
      ;

 VUEC 
      :       'VDET unbonded channels at the beginning, NR=setup code\
               Number of words per module type\
               Number of module types =1'

           STATIC

      = (NfirstZ  = INTE[0,1023] : 'electronic channel number \
                                    of the first connected amplifier Z view',
         NfirstP  = INTE[0,1023] : 'electronic channel number \
                                    of the first connected amplifier phi view')
      ;

 VXCH 
      :      ' VDET extra readout channels at the beginning, NR=setup code\
               Number of words per module type\
               Number of module types =1'

           STATIC

      = (NfirstZ  = INTE[0,1023] : 'readout channel number \
                                    of the first valid channel, Z view',
         NfirstP  = INTE[0,1023] : 'readout channel number \
                                    of the first valid channel, phi view')
      ;


 END ESET 

 DEFINE RSET
 
 
 (VDSL [1,1] -> [1,*] VCOD)
             : 'A VDET subcomponent has many slots'
             ;
 
 
 (VDSL [1,1] -> [1,*] VGMD )
             : 'The Geometrical description of the slot is unique'
             ;
 
 
 (VDSL [1,1] -> [1,*] VDME )
             : 'There are several mechanical description of the sectors'
             ;
 
 
 (VDSL [1,1] -> [1,*] VELE )
             : 'There are several electronic description of the sectors'
             ;
 
 
 (VGMD [1,1] -> [1,*] VZEW )
             : 'The wafer ideal position in the face is not unique'
             ;
 
 
 (VGMD [1,1] -> [1,*] VWGM )
             : 'The wafer geometrical description is not unique'
             ;
 
 
 (VCPC [1,1] -> [1,*] VWGM )
             : 'The coupling capacitances on the phi side'
             ;
 
 (VCZC [1,1] -> [1,*] VWGM )
             : 'The coupling capacitances on the zed side'
             ;
 
 (VNPC [1,1] -> [1,*] VWGM )
             : 'The noise capacitances on the phi side'
             ;
 
 (VNZC [1,1] -> [1,*] VWGM )
             : 'The noise capacitances on the zed side'
             ;
 
 END RSET

 END SUBSCHEMA    

  
SUBSCHEMA ONLINE
 
  : 'Gives a single view of the complete detector.
 
     25-OCT-1988 Changes to rationalise our method of dealing with the ROC-Slot
     relationships. Slots are now in linked lists. These lists have a 1 to
     "n" relationship to a ROC. Thus:
        1 Slot can have 1 ROC associated with it,
        1 Slot can have "n" ROCs associated with it,
        n Slots can have 1 ROC associated to them,
        n Slots can have n ROCs associated to them.
 
     15-NOV-1989 Changes to describe detectors with one ROC per
     subcomponent / component.
        n Slots can have 0 ROCs associated with it - then use subcomp
        or component ROC.
 
      1-APR-1989 Added FB column as index to first Fastbus Segment in
     Fastbus Database.
 
     17-JUL-1989 Relationship to ADBS Geometry database changes as MUON
     ESET is superseded by MUOG, MSCO by MGSC.
 
     31-AUG-1989 Change OCOM and OSLO sizes.

     21-MAY-1990 Update links to VDET description.

     14-MAR-1991 Relationship OCOM -> VEDE changed to OCOM -> VCOD
     VEDE is obsolete.

     17-May-1995 Add OBUN which contains bunch train info needed by JULIA.

    '
 
  AUTHOR   'A.Belk,F.Ranjard'
  REVIEWER 'F.Jacot,F.Ranjard'
  VERSION  '1.9'
  DATE     '09/06/95'
 
 
  DEFINE ATTRIBUTE
 
         EB    = INTE [0,*]   : 'Index to first EB in EB table';
 
         TS    = INTE [0,*]   : 'Index to first TS in TS table';
 
         ROC   = INTE [0,*]   : 'Index to first ROC in ROC table';
 
         SD    = INTE [0,*]   : 'Index to first SCD in SD table';
 
         FB    = INTE [0,*]   : 'Index to highest Fastbus Segment for detector';
 
  END ATTRIBUTE
 
  DEFINE ESET
 
    OALE
        :       'The Detector'
 
          SIZE 1,1
 
        = (Name                 = CH16        : 'Name of detector',
           NComps               = INTE [0,*]  : 'Number of components',
           EB,
           TS,
           SD,
           FB   )
        ;
 
    OBUN
      :      'Bunch train. NR=0 (STC)\
              Number of words/run\
              Number of runs'
        STATIC
     
      = (FirstRun       = INTE         : 'First run',
         NumberofWagons = INTE         : 'number of wagons',
         DistanceWagons = INTE         : 'distance between wagons in ns')
      ;
     
    OCOM
        :       'Detector Components'
 
           SIZE 20,20
 
        = (Name                 = CH16        : 'Name of Component',
           NSubcomps            = INTE [0,*]  : 'Number of Subcomponents',
           FirstSubcomp         = INTE [0,*]  : 'ID of first Subcomponent',
           EB,
           TS,
           ROC,
           SD,
           FB   )
        ;
 
    OSCO
 
        :       'Detector Subcomponents'
 
           SIZE 36,50
 
        = (Name                 = CH16        : 'Name of Subcomponent',
           NSlots               = INTE [0,*]  : 'Number of Slots',
           FirstSlot            = INTE [0,*]  : 'ID of first Slot',
           EB,
           TS,
           ROC,
           SD,
           FB   )
        ;
 
    OSLO
        :       'Detector Slot'
 
           SIZE 220,220
 
        = (Name                 = CH16        : 'Name of a slot',
           Prev                 = INTE [0,*]  : 'Prev Slot in ROC Group',
           Next                 = INTE [0,*]  : 'Next Slot in ROC Group',
           ROC,
           SD,
           FB   )
        ;
 
  END ESET
 
  DEFINE RSET
 
  (OCOM [1,1]   -> [1,*] OALE)
                : 'Aleph is composed of components'
                ;
 
  (OSCO [1,1]   -> [1,*] OCOM)
                : 'The components are composed of subcomponents'
                ;
 
  (OSLO [1,1]   -> [1,*] OSCO)
                : 'The subcomponents are composed of slots'
                ;
 
  (OALE [1,1]   -> [1,*] ALPH)
                : 'Online Aleph table is related to the Geometry description'
                ;
 
 (OCOM [1,1] -> [1,1] ECGN|
             -> [1,1] HCAL|
             -> [1,1] ITCC|
             -> [1,1] LCAL|
             -> [1,1] MUOG|
             -> [1,1] SATR|
             -> [1,1] TCGD|
             -> [1,1] VCOD
                      BY Cmptyp)
             : 'The components are of different types'
             ;
 
 (OSCO [1,1] -> [1,1] ESCO|
             -> [1,1] HSCO|
             -> [1,1] ISCO|
             -> [1,1] LSCO|
             -> [1,1] MGSC|
             -> [1,1] SSCO|
             -> [1,1] TSCO|
             -> [1,1] VCOD
                      BY Scotyp)
             : 'The subcomponents are of different types'
             ;
 
 (OSLO [1,1] -> [1,1] ESLO|
             -> [1,1] HSLO|
             -> [1,1] ISLO|
             -> [1,1] LSLO|
             -> [1,1] MSLO|
             -> [1,1] SSLO|
             -> [1,1] TSLO|
             -> [1,1] VDSL
                      BY Slotyp)
             : 'The slots are of different types'
             ;
 
END RSET
 
 
END SUBSCHEMA
 
 
SUBSCHEMA ONLINEXTRA
 
  : 'ONLINE Extra tables not in ADBS YET'
 
  AUTHOR   'A.Belk'
  REVIEWER ''
  VERSION  '1.1'
  DATE     '18/7/89'
 
 
  DEFINE ESET
 
    HSLO
        :       'HCAL Slot. (LTC)'
 
           SIZE 1,1
 
        = (Name                 = CH16          : 'Name of a slot')
        ;
 
    SSCO
 
        :       'SATR Subcomponent. (LTC)'
 
           SIZE 1,1
 
        = (Name                 = CH16          : 'Name of a subcomponent')
        ;
 
    SSLO
        :       'SATR Slot. (LTC)'
 
           SIZE 1,1
 
        = (Name                 = CH16          : 'Name of a slot')
        ;
 
 
  END ESET
 
  DEFINE RSET
 
  (HSLO [1,1]   -> [1,*] HSCO)
                : 'The HCAL subcomponents are composed of slots'
                ;
 
  (SSCO [1,1]   -> [1,*] SATR)
                : 'The SATR is composed of subcomponents'
                ;
 
  (SSLO [1,1]   -> [1,*] SSCO)
                : 'The SATR subcomponents are composed of slots'
                ;
 
END RSET
 
 
END SUBSCHEMA
 
 
 SUBSCHEMA BcalRunConsts
 : 'Description of Data Base banks containing Bcal data'
 
 AUTHOR   'E. Fernandez'
 REVIEWER ''
 VERSION  '1.0'
 DATE     '08/06/89'
 
    DEFINE ATTRIBUTE
 
    Gain
             = REAL [0.00,1.00]
             : 'Relative phototube gains';
 
    Scale
             = REAL [0.00,300.00]
             : 'Conversion constant for ADC counts to Energy (in GeV)' ;
 
   END ATTRIBUTE
 
 DEFINE ESET
 
  BCON
             : 'Constants for energy calibration. (LTC)\
                number of constants\
                number of sets (=1)'
             SIZE 1,1
              = (ValRng(2),Scale(4),Gain(20))
              ;
 
  END ESET
 
END SUBSCHEMA

 
 SUBSCHEMA BcalOffConsts
 : 'Description of banks containing offline constants for Bcal'
     
 AUTHOR   'P.Morawitz'
 REVIEWER ' '
 VERSION  '1.0'
 DATE     '02/11/98'
     
    DEFINE ATTRIBUTE
        
    PadNo
             = INTE [0,15]
             : 'Pad number';

    NsilChan 
             = INTE [1,8]
             : 'Silicon ADC channel';
 
    NMultilplx      
             = INTE [1,16] 
             : 'Multiplexer number';

    NSilicon
             = INTE [1,3]
             : 'Silicon ADC';

    Ninfo1  
             = INTE [0,5] 
             :  'Mapping' ;

    Ninfo2  
             = INTE [0,3]
             :  'Mapping' ;
           
   END ATTRIBUTE
     
 DEFINE ESET


   BD1B
      : 'Bcal Database bank, mapping the multiplexer number (1-16) to pad
      number\
      Number of columns\
      Number of rows'

      SIZE 16,16
      STATIC

      = (MUltiplexer = NMultilplx : 'Multiplexer number', 
         PAdcode     = PadNo      : 'Pad code')
      ;

   BD2B
      : 'Bcal Database bank, Mapping ADC-Channel-Event to Module-Wafer-Pad
         Mapping between
         [N_SIL_ADC][N_MULTPX][N_SIL_ADC_CH] and [N_MOD][N_PADS][N_WAFERS]\
         Number of columns\
         Number of rows'
      SIZE 384,384 
      STATIC

      = (SilaCh = NsilChan   :   'Number of Silicon ADC channel', 
         MUltpx = NMultilplx : 'Multiplexer number',
         SIladc = NSilicon   :   'Silicon ADC',
	 InFo   = Ninfo1     :   'Mapping')
      ;
         
   BD3B     
      : 'Bcal Database bank, Mapping ADC-Channel-Event to Module-Wafer-Pad
         Part 2\
         Number of columns\
         Number of rows'
       SIZE 24,24
       STATIC

      = (SilaCh = NsilChan :   'Number of Silicon ADC channel', 
         SIladc = NSilicon :   'Silicon ADC',
	 InFo   = Ninfo2   :   'Mapping')
      ;
             
 END ESET
     
END SUBSCHEMA

 
 SUBSCHEMA CaloOffConsts
 : 'Description of Calobject constant banks'
 
 AUTHOR   'A. Bonissent'
 VERSION  '1.2'
 DATE     '30/01/91'
 
 DEFINE ESET
 
 COPT
      :      'Calo options (STC)\
              Number of columns\
              Number of rows '
           STATIC
 
      = (OptionCode   = INTE [1,4]         : ' 1=determine parameters\
                                               2=associate tpc&hcal\
                                               3=same as 2, plus verification\
                                               4=same as 2 plus hcal tubes association,
                                                 creation of the general track-ecal-hclu-hpattern
                                                 relation table chrl, which will be used for the
                                                 creation of the pot bank pcrl    ',
         PtLow        = REAL [0.,45.00]    : 'Cut on pt for low energy hclusters',
         PtHi         = REAL [0.,45.00]    : 'Cut on pt for high energy hclusters',
         EhLim        = REAL [0.,45.00]    : 'Limit between low and hig energy hclusters',
         PMip         = REAL [0.,45.00]    : 'Threshold on Pt/Rms for Mip tracks',
         DLim         = REAL [0.,1.6]      : 'Max angle for ecal-hcal association',
         MUdep        = REAL [0.,15.00]    : 'Average calorimetric Energy deposited by a Muon')
     ;
 
 END ESET
 
 END SUBSCHEMA
 

 SUBSCHEMA BomOffConsts
 : 'BOM Offline Constants'
 
 AUTHOR   'R.Forty'
 VERSION  '1.1'
 DATE     '03/02/92'
 
 DEFINE ESET
 
 
 BOMD
      :      'BOM calibration constants\
              Number of columns =  \
              Number of rows = 1'

           STATIC

           SIZE 1,1
 
      = (DS = REAL [0.,10000.]  : 'Data storage scale',
         CS = REAL [0.,10000.]  : 'Current storage scale',
         PC = REAL [0.,100.]    : 'BOM position calibration',
         O1 = REAL [-100.,100.] : 'X position offset',
         O2 = REAL [-100.,100.] : 'X angle offset',
         O3 = REAL [-100.,100.] : 'Y position offset',
         O4 = REAL [-100.,100.] : 'Y angle offset')
      ;

 BOMG
      :      'BOM gain parameters\
              Number of columns =  \
              Number of rows = 8'

           STATIC

           SIZE 8,8
 
      = (G0 = REAL [-100.,100.] : 'Constant term',
         G1 = REAL [-1.,1.]     : 'Linear term',
         G2 = REAL [-1.,1.]     : 'Quadratic term')
      ;

 BOMM
      :      'BOM corrector magnet parameters\
              Number of columns =  \
              Number of rows = 8'

           STATIC

           SIZE 8,8
 
      = (MC = REAL [-10.,10.] : 'Calibration constants',
         MA = INTE [1,29]     : 'Address',
         MX = INTE [1,2]      : 'Type')
      ;

 BOMO
      :      'BOM optics parameters\
              Number of columns =  \
              Number of rows = 32'

           STATIC

           SIZE 32,32
 
      = (IR = REAL [-1000.,1000.] : 'Transformation IP to BOM',
         RI = REAL [-1000.,1000.] : 'Transformation BOM to IP',
         B1 = REAL [-1000.,1000.] : 'Transformation BOM to corrector 1',
         B2 = REAL [-1000.,1000.] : 'Transformation BOM to corrector 2',
         B3 = REAL [-1000.,1000.] : 'Transformation BOM to corrector 3',
         B4 = REAL [-1000.,1000.] : 'Transformation BOM to corrector 4',
         I1 = REAL [-1000.,1000.] : 'Transformation IP to corrector 1',
         I2 = REAL [-1000.,1000.] : 'Transformation IP to corrector 2',
         I3 = REAL [-1000.,1000.] : 'Transformation IP to corrector 3',
         I4 = REAL [-1000.,1000.] : 'Transformation IP to corrector 4')
      ;

 BOMA
      :      'BOM calibration for ALPHA\
              Number of columns \
              Number of rows '

           STATIC

           SIZE 1,*
 
      = (RU = INTE [0,*]       : 'First run number for calibration',
         XO = REAL [*,*]       : 'X offset for beam position',
         XS = REAL [*,*]       : 'X slope for beam position',
         YO = REAL [*,*]       : 'Y offset for beam position',
         YS = REAL [*,*]       : 'Y slope for beam position',
         FL = INTE [-9,9]      : 'Data quality flag for run')
      ;


 END ESET

 END SUBSCHEMA

 
 SUBSCHEMA EcalOffConsts
 : 'E/GAMMA RECONSTRUCTION CONSTANTS'
 
 AUTHOR   'A. Bonissent,JP.Albanese,D.Pallin ,R.Edgecock'
 REVIEWER '            '
 VERSION  '1.5'
 DATE     '06/10/92'
 
 
 DEFINE ESET
 
 ECNS
      :      'Ec reconstruction constants (STC)\
              Number of constants\
              Number of row = 1'
 
      = (ValRng(2) ,
         EnergyHigh   = REAL [0,*]         : 'High E threshold per storey',
         EnergyLow    = REAL [0,*]         : 'Low E threshold per storey',
         ClustThresh  = REAL [0,*]         : 'E thresh per cluster',
         StepSize     = REAL [0,*]         : 'Step size for trk. extrapolation',
         StepMinSize  = REAL [0,*]         : 'Min. step size for trk.extrap.',
         PMinimum     = REAL [0,*]         : 'momentum thres. for trk. extrap.',
         EnfmiP(3)    = REAL [0,*]         : 'Energy loss per stack for MIP',
         ERrorsq(3)   = REAL [0,*]         : 'Error square for energy loss per satck',
         EchmIn       = REAL [0,*]         : 'Minimum chisquare for MIP',
         PRmip        = REAL [0,*]         : 'Minimum probability for MIP',
         EtoMin       = REAL [0,*]         : 'Energy above which a cluster is not a minion candidate',
         EerenB2(2)   = REAL [0,*]         : 'Data for energy/momentum balance between ecal and tpc',
         MipElectron  = REAL [0,*]         : 'Mip/electron ratio in Ecal',
         Ratio1       = REAL [0,3.00]      : 'Pion/electron ratio in ecal stack 1',
         Ratio2       = REAL [0,3.00]      : 'Pion/electron ratio in ecal stack 2',
         Ratio3       = REAL [0,3.00]      : 'Pion/electron ratio in ecal stack 3',
         RatioGlobal  = REAL [0,3.00]      : 'Pion/electron ratio in ecal average on all stacks',
         CalibFactor  = REAL [0,1.00]      : 'Correction for energy lost in EcCluster/storeys' )
      ;
 
 EPCC
      :      'Ecal Parameters for the
              Clustering Correction. 
              NR=setup code or run number \
              number of constants\
              number of rows = 1'
 
      = (ValRng(2) ,
         BaDefault(2) = REAL [-2.0000,2.0000]  : 'Barrel Default parameters',
         EnDefault(2) = REAL [-2.0000,2.0000]  : 'Endcap Default parameters',
         S2points(4)  = REAL [0.0,100.0]       : 'Stack 2 threshold points',
         S3points(3)  = REAL [0.0,100.0]       : 'Stack 3 threshold points',
         PntParas(12) = REAL [-2.0000,2.0000]  : 'Parameters at each Point' )
      ;
 
 
 END ESET
 
 END SUBSCHEMA
 
 
SUBSCHEMA EflowOffConsts
 :'Constants used in the Eflow code'
 
 AUTHOR   'MN. Minard, M. Pepe'
 VERSION  '2.1'
 DATE     '11/01/90'
 
 DEFINE ESET
 
 EFZC
      :  'Values of polar angles used to define geometrical zones
          for Eflow calculations.
          NR=0 (LTC)\
          Number of words/row\
          Number of rows=1'
        STATIC
 
      = (T1           = REAL   [0.00,90.00]   : 'Theta 1 (in degrees)',
         T2           = REAL   [0.00,90.00]   : 'Theta 2',
         T3           = REAL   [0.00,90.00]   : 'Theta 3',
         T4           = REAL   [0.00,90.00]   : 'Theta 4',
         T5           = REAL   [0.00,90.00]   : 'Theta 5',
         T6           = REAL   [0.00,90.00]   : 'Theta 6')
      ;
 
 EFTD
      :  'Values of cuts used for track selection.
          NR=0 (LTC)\
          Number of words per row\
          Number of rows'
        STATIC
 
      = (NPoints      = INTE  [0,40]        : 'Minimum number of points',
         D0           = REAL  [0.00,1000.]  : 'Maximum value for ABS(D0)',
         Z0           = REAL  [0.00,1000.]  : 'Maximum value for ABS(Z0)',
         PMin         = REAL  [0.00,100.]   : 'Minimum momentum in GeV',
         PmaX         = REAL  [0.00,100.]   : 'Maximum momentum in GeV',
         CG           = REAL  [0.00,100.]   : 'Maximum normalized Chisqr')
      ;
 
 END ESET
 
 
 END SUBSCHEMA
 
 
  SUBSCHEMA EgamOffConsts
    :    'Gammas analysis banks'
 
    AUTHOR    'J.Badier,J.C.Brient'
    VERSION   '1.3'
    DATE      '05/11/91'
 
    DEFINE ESET
 
    EGST
         :    'Configuration probability of a cluster. (LTC)\
               Number of words per class and angle\
               Number of classes and angles'
         SIZE 24,24
         STATIC
 
         = (PRob(10)     = REAL [.0,9.000]    :  'Probability')
 
         ;
 
    EGVP
         :    'Overlap radiation length density. (LTC)\
               Number of words per half module\
               Number of half modules'
         SIZE 48,48
         STATIC
 
         = (RadLen(16)  = REAL [.0,60.000]    :  'Radiation length (cm)')
 
         ;
 
    EGPA
         :    'Shower parametrisation. (LTC)\
               Number of words per shower type\
               Number of showers types'
         SIZE 5,5
         STATIC
 
         = (AoverB(2)     = REAL [.0,7.000]     :  'Alpha/Beta param.',
           OneovB(2)     = REAL [0.000,5.000] :  '1./Beta param.',
           Sigma(6)      = REAL [0.,.300]      :  'Estimated errors')
 
         ;
 
    EGMD
         :    'Materialisation depth parametrisation. (LTC)\
               Number of words per coefficient\
               Number of coefficients'
         SIZE 4,4
         STATIC
 
         = (ParaM(4) = REAL [-10.000000,20.000000] : 'Parameter')
 
         ;
 
    EGTH
         :    'Remarkable theta angles. (LTC)\
               Number of words per hemisphere\
               Number of hemispheres'
         SIZE 2,2
         STATIC
 
         = (Theta1    = REAL [.0,3.0000]    :  'Beam endcap edge exit',
           Theta2    = REAL [.0,3.0000]    :  'Beam endcap edge entrance',
           Theta3    = REAL [.0,3.0000]    :  'Coil endcap edge exit',
           Theta4    = REAL [.0,3.0000]    :  'Barrel edge exit',
           Theta5    = REAL [.0,3.0000]    :  'Coil endcap edge entrance',
           Theta6    = REAL [.0,3.0000]    :  'Barrel edge exit')
 
         ;
 
    ECLK
         :    'Longitudinal leakage correction. (LTC)\
               Number of words per incidence angle\
               Number of incidence angles'
         SIZE 6,6
         STATIC
 
         = (CorR(6) = REAL [0.00000,0.05000] : 'Correction term')
 
         ;
 
    EGNC
         :    'Neutral clusters anlysis parameters. (LTC)\
               Number of words\
               Number of options'
         SIZE 1,1
         STATIC
 
         = (PTit     = REAL [.0, .1000] :  'Small positive real',
           EnrmIn    = REAL [.0,100.00] :  'Cluster minimum energy',
           EnrmAx    = REAL [.0,100.00] :  'Cluster maximum energy',
           Cut1      = REAL [.0,1.0000] :  'First longitudinal cut',
           Cut2      = REAL [.0,1.0000] :  'Second longitudinal cut',
           RdlBar    = REAL [.0,1.0000] :  'Matter before barrel',
           RdlEnc    = REAL [.0,1.0000] :  'Matter before endcap',
           Ovlp1     = REAL [.0,20.00]  :  'Overlap correction #1',
           Ovlp2     = REAL [.0,20.00]  :  'Overlap correction #2',
           Ovlp3     = REAL [.0,20.00]  :  'Overlap correction #3',
           Ovlp4     = REAL [.0,20.00]  :  'Overlap correction #4',
           Ovlp5     = REAL [.0,20.00]  :  'Overlap correction #5',
           CompCut   = REAL [.0,1.00]   :  'Compactness cut',
           ComMean   = REAL [.0,1.00]   :  'Compactness mean value',
           Tower4    = REAL [.0,1.25]   :  '4th tower estimation',
           TowNorm   = REAL [.0,10.00]  :  '4th tow. estimator norm',
           SecGamm   = REAL [.0,1.00]   :  'Second gamma compact. cut',
           Barcor0   = REAL [.0,1.00]   :  'Barycenter correction #0',
           Barcor1   = REAL [.0,1.00]   :  'Barycenter correction #1',
           SatCoef   = REAL [.0,.10000] :  'Saturation coefficient')
 
         ;
 
    EGRP
         :    'Gamma Reconstruction parameters. (GAMPEC) (LTC)\
               Number of columns\
               Number of rows'
         SIZE 1,1
         STATIC
 
         = (SmEnergy = REAL [.000,.1000] :  'Minimum Storey Energy',
            Clust1   = REAL [.000,.500]  :  'Opening cluster stack 1',
            Clust2   = REAL [.000,.500]  :  'Opening cluster stack 2',
            Clust3   = REAL [.000,.500]  :  'Opening cluster stack 3',
            ClUse    = REAL [.000,.500]  :  'Global Cluster energy',
            DiSt     = REAL [.00,20.00]  :  'Min Distance charge track',
            stack12  = REAL [-1.0,2.0]   :  'Gamma pattern stack 1/2',
            stack23  = REAL [-1.0,2.0]   :  'Gamma pattern stack 2/3')


  END ESET
 
  END SUBSCHEMA
 
 
SUBSCHEMA ElecOffConsts
: ' Parameters for ELectron IDENTification'
 
AUTHOR        'A. Falvard'
VERSION       '1.0'
DATE          '27/07/89'
 
DEFINE ESET
 
EIBP
      :       'Electron Identification: parameters. (LTC)\
               Number of words/row\
               Number of rows'
           STATIC
 
      = (E4sP   = REAL [0.000,1.0]  :   'Fraction of TPC Momentum\
                                         in 2x2 storeys',
        EqlR    = REAL [0.100,0.30] :  'ECAL resolution at 1 GeV',
        Rdc1    = REAL [-1000.0,1000.] : 'Lower cut for R2',
        Rdc2    = REAL [-1000.0,1000.] : 'Upper cut for R2',
        Bet1    = REAL [0.,1.0000]    :   'First parameter for beta/alpha',
        Bet2    = REAL [0.,1.0000]    :   'Second parameter for beta/alpha',
        Sbe1    = REAL [0.,1.0000]    :   'First par. for sigma beta/alpha',
        Sbe2    = REAL [0.,1.0000]    :   'Second par. for sigma beta/alpha',
        Usa1    = REAL [0.,1.0000]    :   'First parameter for 1/alpha',
        Usa2    = REAL [0.,1.0000]    :   'Second parameter for 1/alpha',
        SsaO    = REAL [0.,1.0000]    :   'First par. for sigma 1/alpha',
        SsaT    = REAL [0.,1.0000]    :   'Second par. for sigma 1/alpha',
        Rdc3    = REAL [-1000.0,1000.] : 'Lower cut for R3',
        Rdc4    = REAL [-1000.0,1000.] : 'Upper cut for R3',
        Rdc5    = REAL [-1000.0,1000.] : 'Lower cut for R4',
        Rdc6    = REAL [-1000.0,1000.] : 'Upper cut for R4',
        Rdc7    = REAL [-1000.0,1000.] : 'Lower cut for R1',
        Rdc8    = REAL [-1000.0,1000.] : 'Upper cut for R1',
        Cba1    = REAL [0.,1.0000]    :   'First correction b/a',
        Cba2    = REAL [0.,1.0000]    :   'Second correction b/a',
        Cua3    = REAL [0.,1.0000]    :   'First correction 1/a',
        Cua4    = REAL [0.,1.0000]    :   'Second correction 1/a',
        Cba5    = REAL [0.,1.0000]    :   'First correction s(b/a)',
        Cba6    = REAL [0.,1.0000]    :   'Second correction s(b/a)',
        Cua7    = REAL [0.,1.0000]    :   'First correction s(1/a)',
        Cua8    = REAL [0.,1.0000]    :   'Second correction s(1/a)',
        SatU    = REAL [-1.,1.00000]    :   'Saturation parameter')
      ;
END ESET
END SUBSCHEMA
 
 
 SUBSCHEMA FitOffConsts
 : 'Description of Fitting constants banks'
 
 AUTHOR   'LL. Garrido, R. Johnson, D. Cinabro'
 VERSION  '1.1'
 DATE     '11/12/90'

 
 DEFINE ESET
 
 
 FCON
      :      'Tpc linking Constants (STC)\
              Number of parameters \
              1    '
      = (ValRng(2) ,
         DelMin = REAL [0.00,6.30]  : 'Window in phi for linking search',
         ChiP   = REAL [0.0,*]    : 'Chi max between the parameters',
 
         Chi0   = REAL [0.0,*]    : 'Chi max to accept candidate at first trial',
         Chi1   = REAL [0.0,*]    : 'Chi max permited without rejecting a coordinate',
         Chi2   = REAL [0.0,*]    : 'Chi max final to accept candidate')
         ;
 
 FGTP
      :     'Track quality cuts.\
             Number of cuts \
             Number of cut sets '
          STATIC

      = (NumTpc = INTE [0,21]  : 'Minimum number of TPC hits.',
         CosTh  = REAL [0.,1.] : 'Maximum |Cos(Theta)|.',
         D0max  = REAL [0.,10.]: 'Maximum D0.',
         Z0max  = REAL [0.,50.]: 'Maximum Z0.',
         BeamE  = REAL [0.,2.] : 'Maximum fraction of beam energy.')
       ;


 END ESET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA HcalCalConsts
 : 'HCAL Towers Calibration constants'
 
 AUTHOR   'F. Ligabue','R. Tenchini'
 VERSION  '1.1'
 DATE     '12/07/89'
 
 DEFINE ESET
 
 
H1EC
      :      'Hc Region 1 Energy calibration factor. (LTC)\
              Number of phi regions\
              Number of theta regions'
           STATIC
 
      = (CaliBfactor(4)    = REAL [0.00,*]     : 'Phi regions')
      ;
 
H2EC
      :      'Hc Region 2 Energy calibration factor. (LTC)\
              Number of phi regions\
              Number of theta regions'
           STATIC
 
      = (CaliBfactor(8)    = REAL [0.00,*]     : 'Phi regions')
      ;
 
H3EC
      :      'Hc Region 3 Energy calibration factor. (LTC)\
              Number of phi regions\
              Number of theta regions'
           STATIC
 
      = (CaliBfactor(16)   = REAL [0.00,*]     : 'Phi regions')
      ;
H4EC
      :      'Hc Region 4 Energy calibration factor. (LTC)\
              Number of phi regions\
              Number of theta regions'
           STATIC
 
      = (CaliBfactor(4)    = REAL [0.00,*]     : 'Phi regions')
      ;
 END ESET
 END SUBSCHEMA
 
 
 SUBSCHEMA HcalOffConsts
 :'HCAL constants used in offline programs'
 
 
 AUTHOR   'G. Capon,G.Bagliesi,L.Silvestris'
 REVIEWER 'A.Sciaba'
 VERSION  '3.2'
 DATE     '19/08/98'
 
 
 DEFINE ESET
 
 HTUS
      :      'Hcal TUbe Shift constants (LTC)\
              Number of words/row  \
              Parameter Values '
           STATIC
 
      = (TubE          = REAL [2.5,*]     :'Total tube shift in barrel', 
         TubShift(4)   = REAL [0.5,4.5]   :'Shift of sensitive part \
                                             of eightfold I in barrel')
      ;
 
 HCOS
      :      'Hcal COStants.NR=Geant version,or 1 (LTCGAL)\
              Number of words/row  \
              Parameter Values '
           STATIC
 
      = (TubAct        = REAL [0.8,1.] :'Thickness of active zone in\ 
                                         streamer tube layers    ',
         TubStreamer   = REAL [1.4,* ] :'Total thickness of streamer\
                                         tube layer              ',
         ObscurLength  = REAL [0.44,*] :'Obscuration length of\ 
                                         streamer                ',
         MaxTrack      = REAL [10.,*]  :'Maximum lenght of a track\
                                         element                 ',
         EfficendcapA  = REAL [0.5,1.] :'Tube efficiency endcap A',
         EFficBarrel   = REAL [0.5,1.] :'Tube efficiency barrel  ',
         EfficendcapB  = REAL [0.5,1.] :'Tube efficiency endcap B',
         ConverFactor  = REAL [0.001,1.] :'Conversion factor adc \
                                         to energy          ',
         InductFactor  = REAL [0.1  ,*] :'Induction factor        ',
         MaxAdc        = REAL [1.,3500.]:'Maximum charge in streamer\
                                         response')
      ;
 
HDRC
      :      'Digital pattern Run Conditions . (LTC)\
              Number of words/row\
              Number of rows'
           STATIC
 
      = (DmaX   = REAL [0.0,300.0]  : 'Max  dist. in same layer\
                                                same module',
        DmaY   = REAL [0.0,300.0]  : 'Max  dist. in diff. layer\
                                                same module',
        BmaX   = REAL [0.0,300.0]  : 'Max  dist. in same layer\
                                               diff. module',
        BmaY   = REAL [0.0,300.0]  : 'Max  dist. in diff. layer\
                                               diff. module',
        CmaX   = REAL [0.0,300.0]  : 'Max  dist. in same layer\
                               diff. module esternal border',
        CmaY   = REAL [0.0,300.0]  : 'Max  dist. in diff. layer\
                               diff. module esternal border',
        Phi1   = REAL [0.000,3.142]  : 'Phi max to link ECLU to Patterns',
        Ysz1   = REAL [0.000,3.142]  : 'Y/Z max to link ECLU to Patterns',
        Phi2   = REAL [0.000,3.142]  : 'Phi max to link HCLU to Patterns',
        Ysz2   = REAL [0.000,3.142]  : 'Y/Z max to link HCLU to Patterns',
        Dma0   = REAL [0.0,300.0]  : 'Max distance to include satellites',
        KiSp   = INTE [1,2]      : 'Superpattern Kind (1=E, 2=H)',
 
        Nsa1   = INTE [0,500] : 'Max # of hits for a satellite',
 
        Nsa2   = INTE [0,500] : 'Max # of hits for Pattern suppression')
      ;

 HGEA
      :      'Hcal GEAnttracking cuts.NR=GEANT version (LTCGAL)\
              Number of cuts/row\
              Numberof rows'
           STATIC

      = (BcutM        = REAL :'kinetic energy cut for muon Bremsstrahlung',
         PpcutM       = REAL :'total energy cut for e+e-pair prod. by muons',
         DcutM        = REAL :'kinetic energy cut for muon or hadron delta rays',
         CutHad       = REAL :'kinetic energy cut for charged hadrons',
         CutNeu       = REAL :'kinetic energy cut for neutral hadrons',
         CutMuo       = REAL :'kinetic energy cut for muons',
         CutGam       = REAL :'kinetic energy cut for gammas',
         CutEle       = REAL :'kinetic energy cut for electrons')
      ; 

 HTRE
      :      'Hcal streamer Tube REsponse;
              Histogram to describe the streamer response.NR=1 (LTCGAL)'
 
      = (ValRng(2),
         NumBins      = INTE  100              : 'nb of bins for histo
                                                  of streamer response',
         LowEdge      = REAL [0.,0.5]          : 'low edge of streamer histo',
         UppEdge      = REAL [10.,10.5]        : 'upp edge of streamer histo',
         StrResp(100) = REAL [0.,3200.]        : 'histo values of
                                                 streamer response')
      ;
 
 HETC
      :      'HCal Tube Constants
              Constants to describe the tubes. (LTC)'
 
      = (ValRng(2),
        NumTubEi      = INTE 8                : 'number of tubes in eightfold',
        EiWidth       = Length [8.0,8.50]     : 'width of eightfold tube',
        DeiWid        = Length [16.00,17.00]  : 'Double eightfold width',
        TubeGap       = Length [1.0,1.1]      : 'size of streamer tube gap',
        Ei2Pos        = Length [8.00,9.00]    : 'position of second eightfold
                                                 in double eightfold',
        InsZone       = Length [2.9,3.5]      : 'size of insensitive zone
                                                 at tube end',
        Ltsens(4)     = Length [569.0,703.0]  : 'length of sensitive part
                                                 of eightfold I in barrel',
         LtubIn(4,10)= Length [ 20.0,180.0]   : 'tubes length in inner
                                                 endcap',
         LtubOu(4,20)= Length [ 90.0,410.0]   : 'tubes length in outer
                                                 endcap',
         AnglIn(10)  = Angle  [-1.0000,1.0000] : 'tubes profile angle
                                                  in inner endcap',
         AnglOut(20) = Angle  [-1.0000,1.0000] : 'tubes profile angle
                                                  in outer endcap')
      ;
 
 HCCO
      :      'HCal COnstants
              Constants used in the offline programs. (LTC)'
 
      = (ValRng(2),
         TheRef       =Angle[.721516,.721517] : 'theta reference value for
                                                theta boundaries computation',
         RadRef       = Length [377.0,377.1]  : 'radial reference value for
                                                theta boundaries computation',
         ZRef         = Length [394.4,394.6]  : 'Z reference value for
                                                theta boundaries computation',
         NBboun       = INTE 18               : 'number of theta boundaries
                                                in half barrel',
         NEboun       = INTE 15               : 'number of theta boundaries
                                                in half endcap')
      ;

HCOL
     :      'Average HCAL online calibration constant.
             NR=0\
             Number of constants \
             Number of HCAL setups'
       STATIC

     = (OnlConst = REAL [0.000,*]     : 'online constant')
      ;

HIFC
      :      'Online HCAL InterModule Calibration constants.
              NR=HC setup code\
              Number of Modules \
              Number of rows (=1)'
           STATIC
 
      = (OnlBarConst(24)    = REAL [0.000,*]     : 'Barrel equalization Constants',
         OnlEndcConst(12)   = REAL [0.000,*]     : 'Endcap equalization Constants')
      ;

HSCA
      :      'Versions of HCAL calibration banks to be used
              in the MC.
              NR=0\
              Number of constants \
              Number of HCAL setups'
           STATIC   

      = (VersionhToc      = INTE [0,99999]  : 'Version of HT0C',
         VersionhStd      = INTE [0,99999]  : 'Version of HSTD',
         VersionhImc      = INTE [0,99999]  : 'Version of HIMC')

      ;

HIMM
      :      'MC HCAL InterModule Calibration constants.
              NR=HC setup code\
              Number of Modules \
              Number of rows (=1)'
           STATIC
 
      = (McBarConst(24)    = REAL [0.000,*]     : 'Barrel equalization Constants',
         McEndcConst(12)   = REAL [0.000,*]     : 'Endcap equalization Constants')
      ;

 
 END ESET
 
 END SUBSCHEMA


SUBSCHEMA ItcOffConsts

: 'ITC constants used in offline SW. Some of these banks may be
   updated periodically to reflect changes in running conditions
   etc. which affect the ITC.'

AUTHOR    'I.R.Tomalin,J.Sedgbeer,R.Beuselinck'
VERSION   '2.2'
DATE      '13/08/98'
REVIEWER  'J.Sedgbeer'

DEFINE ESET

 ICUT
     :   'Cuts used during ITC track finding. (STC)'

       SIZE 1,1

     = (ValRng(2),
       RadMin(3)   = REAL [.00,*]  :'Min. track radius allowed in ITC\
                                    stand-alone tracking (passes 1,2 &\
                                    3).(Passes 1 & 2 not yet implemented).',
       SigmaRphi(3)= REAL [.00,*]  :'No. of standard deviations allowed on\
                                    coordinate errors in r-phi during \
                                    tracking (passes 1,2 & 3). (Passes \
                                    1 & 2 not yet implemented).',
       SigmaZ(2)   = REAL [.00,*]  :'No. of std. deviations allowed on\
                                    coord. errors in z in (1) TPC \
                                    extension & (2) ITC stand-alone\
                                    tracking',
       MinLink(2)  = INTE [3,7]    :'Min. no. of links allowed on track in\
                                    (1) TPC extension and (2) ITC stand-\
                                    alone phases of track finding.',
       JumpDesired(2)= INTE [0,4]  :'Max. no. of layers over which to \
                                   allow a link to jump in (1) TPC\
                                   extension and (2) ITC stand-alone\
                                    phases of track finding.',
       JumpCpu(2)    = INTE [0,4]    :'Same as Jump1, but can be made smaller\
                                    if program is taking too much CPU time.',
       UseZ(2)     = INTE [0,1]    :'= 1 if z-info is to be used in \
                                   tracking; = 0 otherwise, in (1) TPC\
                                    extension and (2) ITC stand-alone\
                                    tracking.',
       ConfLevel(2)= REAL [.000,1.] :'Confidence level cut applied to \
                                    track fit on ITC coords for (1) TPC\
                                    extension and (2) ITC stand-alone\
                                    tracking.',
       ChisqPoint  = REAL [.000,*]  :'Min. Chi**2 contribution of a coord.\
                                    to the total, for it to be considered\
                                    for elimination during TPC extension\
                                    phase.',
       ConfAdd   = REAL [.00000,1.] :'Confidence level cut applied to\
                                     increase in CHI**2 on adding ITC\
                                     coords to TPC track.',
       MAtch       = INTE [0,1]    :'=1 if matching to be attempted of\
                                    unextended TPC and ITC stand-alone\
                                    tracks. =0 otherwise.',
       DelMin      = REAL [0.,3.1420]:'Max. diff in phis of matched tracks at\
                                     ITC-TPC boundary',
       ChisqM      = REAL [.00,*]   :'Max. CHI**2 for comparison of helix\
                                    params of matched tracks',
       Chisq1      = REAL [.00,*]   :'Max. CHI**2 for final track fit for\
                                     matched tracks (IOPT=1 in UFITMS)',
       Chisq4      = REAL [.00,*]   :'Max. CHI**2 for final track fit for\
                                    matched tracks (IOPT=4 in UFITMS)')
     ;

 IDTC
      :      'Itc Drift-Time relation for 1989-1992 data.
              (Use bank IDRP for 1993 onwards and for Monte Carlo). (STC)'

        SIZE 8,8

      = (ValRng(2),
         LayerNumber  = INTE   [1,8] : 'ITC layer number',
         TimeOffset   = REAL   [*,*] : 'Fine tune time offset for\
                                        this layer (wrt gross offset\
                                        in IRFE bank) in ns.',
         COeffs(3)    = REAL   [*,*] : 'Polynomial coeffs. for this layer\
                                        s.t. dist = c1*t * c2*t**2 + c3*t**3 \
                                        where the time, t, is in ns. \
                                        Note there is no constant term.')
      ;

 IDRP
      :      'Itc Drift-time Relation Parametrisation for 1993-1996.
              For 1997 onwards use IDSP bank.
              IDRP has one row per ITC layer. (STC)'

        SIZE 8,8

      = (ValRng(2),
         TimeOffset   = REAL   [*,*] : 'Fine tune time offset for\
                                        this layer (wrt gross offset\
                                        in IRFE bank) in ns.',
         COeffs(5)    = REAL   [*,*] : 'Polynomial coeffs. for this layer\
                                        s.t. dist = c1*t * c2*t**2 + .......\
                                        where the time, t, is in ns. \
                                        Note there is no constant term.')
      ;
 IDSP
      :      'Itc Drift-time Spline Parametrisation for 1997 onwards.
              IDSP has one row per ITC layer. (STC)'

        SIZE 8,8

      = (ValRng(2),
         TimeOffset   = REAL   [*,*] : 'Fine tune time offset for\
                                        this layer (wrt gross offset\
                                        in IRFE bank) in ns.',
         COeffs(5)    = REAL   [*,*] : 'Cubic Spline coeffs. for this layer.')
      ;

 IEWP
      :      'Itc Epsilon (correction) to Wire Phi coord.
              Correction to be subtracted. (STC)'

        SIZE 8,8

      = (ValRng(2),
         LayerNumber  = INTE   [1,8]  : 'Layer number',
         NWiril       = INTE   96|144 : 'Number of wires in layer',
         DeltaPhi(144)= REAL   [*,*]  : 'Phi correction to sense wire.')
      ;

 IET0
      :      'Itc Epsilon (correction) to timing offset, T0.
              Correction to be subtracted. (STC)'

        SIZE 8,8

      = (ValRng(2),
         LayerNumber    = INTE   [1,8]  : 'Layer number',
         NWiril         = INTE   96|144 : 'Number of wires in layer',
         DeltaTzero(144)= REAL   [*,*]  : 'Time correction to Tzero.')
      ;

 IEDD
      :      'Itc Epsilon (correction) to Drift Distance. (STC)'

        SIZE 8,8

      = (ValRng(2),
         LayerNumber  = INTE  [1,8]    : 'Itc layer number.',
         NumberBins   = INTE  [0,50]   : 'Number of bins in table',
         BinWidth     = REAL  [.000,*] : 'Bin Width.',
         BinLow       = REAL  [*,*]    : 'Low edge of table, Dmin',
         BinHigh      = REAL  [*,*]    : 'High edge of table, Dmax.\
                                          Hi = Lo+(Num.bins-1)*width',
         DeltaDist(50)= REAL  [*,*]    : 'Correction to drift distance (cm)\
                                          as a func. of fractional drift dist.\
                                          (fraction of the half cell width \
                                          from -1 to +1 ) in N bins.')
      ;

 IRES
      :      'ITC R-Phi resolution vs. cell position relation (STC)'

        SIZE 8,8

      = (ValRng(2),
         LayerNumber  = INTE   [1,8] : 'ITC layer number',
         COeffs(3)    = REAL   [*,*] : 'Polynomial coeffs. (cm.) for this layer\
                                        s.t. RES = c1 + c2*f * c3*f**2.\
                                        Where f is the distance of the coord.
                                        out from the sense wire divided by the
                                        cell half-width.')
      ;

 IRRF
      :      'ITC R-Phi Resolution for Fitting. Bank has 8 rows, 1 for each
              ITC layer. (STC)'

        SIZE 8,8

      = (ValRng(2),
         MaXresol     = REAL [0.,0.1000] : 'Max. allowed resolution (cm)',
         MiNresol     = REAL [0.,0.1000] : 'Min. allowed resolution (cm)',
         CoeffsPos(5) = REAL [*,*]       : 'Coefficients of resolution on positive
                                            side of cell s.t.\
                                            Res = C1 + C2*D + C3*D**2 + C4*D**3
                                                     + C5*D**4\
                                            where D=drift distance as a fraction
                                            of the half cell width. N.B. Dist D MUST BE
                                            POSITIVE.',
         CoeffsNeg(5) = REAL [*,*]       : 'Coefficients of resolution on negative
                                             side of cell.')
      ;

 ITKP
      :      'ITC Tracking parameters. (LTC)'

        SIZE 1,1

      = (ValRng(2),
         SigmaRphi    = Length [0.,0.0500] : 'Sigma R-phi (cm)',
         SigmaZ       = Length [0.,50.00]  : 'Sigma Z (cm.)',
         RScatt       = Length [25.,35.00] : 'Av. Scattering radius \
                                              between ITC and TPC',
         RadLen       = REAL   [0.0,0.100] : 'Av. Radiation Length\
                                              between ITC and TPC')
      ;

 IZSC
      :      'ITC Z scalar parameters. For old MC data only.
              (Fixed). (LTC)'

        SIZE 1,1

      = (ValRng(2),
         LeastCount   = REAL   [0.0,4.00]  : 'TDC least count (ns.)',
         OFfset       = REAL   [0.0,512.0] : 'Z timing offset (ns.)',
         EXpansion    = REAL   [0.,500.00] : 'Time expansion for layer 8',
         DeLay        = REAL   [0.0,500.0] : 'Time delay B end of layer 8',
         CountLo      = INTE   [0,512]     : 'Min. allowed TDC count',
         CountHi      = INTE   [0,512]     : 'Max. allowed TDC count',
         SiGma        = REAL   [0.,50.00]  : 'Sigma of time diff. (ns.)',
         PulseLength  = REAL   [0.,500.0]  : 'Pulse length of Z coinc.\
                                              for layer8 ',
         ZPulse       = REAL   [0.0,200.0] : 'Z timing clock pulse (ns.)',
         ThetaStart   = REAL   [0.,5000.0] : 'Start time of first theta\
                                              bin (ns.)',
         ThetaWidth   = REAL   [0.,100.00] : 'Theta bin width (ns.)')
      ;

 ICAE
      :      'Itc CAbling Errors. List of swapped channels (wire-numbers)
              for R-phi TDC readout.(STC)'

        SIZE 8,8

      = (ValRng(2),
         WireAssumed = INTE [1,960]     :' Assumed wire number of channel.',
         WireCorrect = INTE [1,960]     :'Correct wire number of channel.')
      ;

 ISFE
      :      'Itc Status of Front End, i.e indicates if data from ITC
              readout is O.K. to be used.(STC)'

        SIZE 1,1

      = (ValRng(2),
         RphiTdc   = INTE [0,1] :'R-phi TDC data: =0 not to be used, =1 O.K.',
         ZTdc      = INTE [0,1] :'Z TDC data: = 0 not to be used, =1 O.K.')
      ;

 IZRS
      :      'ITC Z coordinate resolution constants.(STC)'

        SIZE 8,8

      = (ValRng(2),
         LayerNumber  = INTE   [1,8] : 'ITC layer number',
         COeffs(2)    = REAL   [*,*] : 'True Z res. Coeffs. for this layer\
                                        s.t. RES = c1 + c2*z**2.',
         TrackingRes  = REAL   [*,*] : 'Z res. used in track finding and\
                                        fitting, i.e. this value used in\
                                        making ITCO bank.')
      ;

 IEFF
      :      'ITC wire inefficiency parameters. (STC)\
              # of words per row\
              # of rows'

        STATIC

      = (MaXimumineff = REAL [0.,1.0000] : 'Wire inefficiency for tracks\
                                            at theta=90 degrees.')

      ;

 IZNL
      :      'ITC S-bend parameterisation coefficients. This parmeterisation
              replaces the coefficients in IZFE which are no longer used
              from 1992. (STC)\
              # of words per row\
              # of rows'

        STATIC

      = (LayerOffset(8) = REAL [-10.,10.000] : 'Layer offset to measured Z',
         Sbend1         = REAL [-10.,10.000] : 'Linear term in Z',
         Sbend2         = REAL [0.,20.000]   : 'Amplitude of SIN term in Z',
         Sbend3         = REAL [90.,300.0]   : 'Period of SIN term in Z')

      ;

 END ESET

 DEFINE RSET


 (IZSC [1,1] -> [1,1] ITCC)
             : 'ITC Z scalar parameters'
             ;

 END RSET

END SUBSCHEMA
 

SUBSCHEMA ItcRunConsts

: 'ITC constants which come from the Online in the Run record
   of the data and are to be used in the reconstruction of the
   data.'

AUTHOR    'J.Sedgbeer,R.Beuselinck'
VERSION   '2.0'
DATE      '23/08/95'
REVIEWER  'W.Cameron'

DEFINE ESET

 IXCR
      :      'ITC trigger processor configuration. This bank replaces
              ITCL and ITRM from 1992. It may appear in the run header
              record and/or on the database. (STC)\
              # of words per row\
              # of rows'

        STATIC

      = (InnerHits    = INTE [0,4]   : 'min. no. of layers hit in layers 1-4',
         OuterHits    = INTE [0,4]   : 'min. no. of layers hit in layers 5-8',
         MaskHits     = INTE [0,8]   : 'min. no. of layers hit in layers 1-8',
         TrackThresh  = INTE [0,192] : 'track threshhold',
         BacktoBack   = INTE [0,3]   : 'no. of wires either side of opposite\
                                        wire for back to back trigger',
         TrackClust   = INTE [0,1]   : 'Track clustering flag, 1=ON',
         HitClust     = INTE [0,1]   : 'Hit clustering flag, 1=ON',
         ProcessorId  = INTE [0,1]   : 'Trigger processor used, 0=RPP, 1=SPP',
         HitThresh(8) = INTE [0,144] : 'Hit threshhold per layer',
         BackVeto(8)  = INTE [0,144] : 'Background veto per layer',
         FanoutMode   = INTE [1,2]   : 'Select mode for defining fanout of\
                                        the ITC masks for coincidence with\
                                        the calorimeter sectors. This defines\
                                        16 "side bits".\
                                        NB: only mode 1 has been implemented.')

      ;

 IXBW
      :      'List of Bad Wires not used in ITC trigger. This may appear in
              the run header and/or in the database. (STC)\
              # of words per row\
              # of rows'

        STATIC

      = (WireNumber = INTE [1,960] : 'Bad wire treated as always ON by\
                                      the trigger processor.')

      ;

 IWST
      :      'ITC wire status for non-standard wires.
              N.B. this bank may be found in the run header record
              and/or the database. (STC)'

        SIZE 960,960

      = (ValRng(2),
         IWire        = INTE   [1,960] : 'Wire number',
         FLag         = INTE   [1,4]   : 'Status flag for readout.\
                                          1 = completely dead\
                                          2 = B-end dead (no Z data)\
                                          3 = A-end dead (no Z readout,\
                                              drift time from B-end)\
                                          4 = drift time TDC dead but\
                                              Z readout exists')
      ;

 IRFE
      :      'Itc R-phi Front-End parameters.
              N.B. this bank may be found in the run header record
              and/or the database. (STC)'

        SIZE 1,1

      = (ValRng(2),
         BinWidth      = REAL   [0.0,3.00]    : 'R-phi TDC bin width (ns.)',
         ZeroBin       = INTE   [0,511]       : 'TDC bin corresponding to\
                                                 sense wire. (Drift dist = 0)',
         DriftVelocity = REAL   [.0025,0.007] : 'Rough drift velocity (cm/ns)',
         MaxCluster(8) = INTE   [0,144]       : 'Max. cluster size allowed in\
                                               layer i in raw data. Online cut',
         CountLo(8)    = INTE   [0,511]       : 'Min. allowed TDC count in\
                                                 layer i. Offline cut.',
         CountHi(8)    = INTE   [0,511]       : 'Max. allowed TDC count in\
                                                 layer i. Offline cut.')
      ;

 IZFE
      :      'Itc Z Front-End parameters.
              N.B. this bank may be found in the run header record
              and/or the database. From 1992 the S-bend parameters
              are taken from the IZNL bank instead.(STC)'

        SIZE 1,1

      = (ValRng(2),
         BinWidth     = REAL   [0.0,8.00]  : 'Z TDC bin width (ns.)',
         ZeroBin      = INTE   [0,1023]    : 'TDC bin corresponding to Z=0',
         EXpansion    = REAL   [50.,200.00]: 'Time expansion for layer 8',
         Sbend1       = REAL   [-3.,3.00]  : 'S-bend parameter 1. The s-bend\
                                              parameters describe the relation\
                                              between z and the time difference\
                                              NB: See bank header',
         Sbend2       = REAL   [0.,20.00]  : 'S-bend parameter 2.',
         Sbend3       = REAL   [100.,300.0]: 'S-bend parameter 3.',
         OnOff        = INTE   0|1         : 'S-bend correction flag.\
                                              0 = to be done offline\
                                              1 = done online',
         CountLo(8)   = INTE   [0,1023]    : 'Min. allowed TDC count on\
                                              layer i. Offline cut.',
         CountHi(8)   = INTE   [0,1023]    : 'Max. allowed TDC count on\
                                              layer i. Offline cut.')
      ;

END ESET

 DEFINE RSET

 (IWST [1,1] -> [1,1] IPMO)
             : 'The physical module has a dead wire map'
             ;

 (IRFE [1,1] -> [1,1] ITCC)
             : 'ITC R-phi Front End parameters'
             ;

 (IZFE [1,1] -> [1,1] ITCC)
             : 'ITC Z Front End parameters'
             ;

 END RSET

END SUBSCHEMA
 

SUBSCHEMA LcalOffConsts
 :'Constants used in the Lcal code'
 
 AUTHOR   'P.Hansen, M.N.Minard'
 REVIEWER 'P.Hansen'
 VERSION  '5.2'
 DATE     '16/06/97'


 
 DEFINE ESET
 
 LCSH
      :      'Parameters for e-gamma showers in LCAL. (before 1994) (LTC)'
              SIZE 1,1
 
      = (ValRng(2),
         P1        = REAL   [0.000,2.000]   : 'Parameter 1 (see LCSHOW)',
         P2        = REAL   [0.000,1.000]   : 'Parameter 2',
         P3        = REAL   [0.000,1.000]   : 'Parameter 3',
         D1        = REAL   [0.000,1.000]   : 'dP1/dlogE ',
         D2        = REAL   [-1.000,1.000]   : 'dP2/dlogE ',
         D3        = REAL   [0.000,2.000]   : 'dP3/dlogE ',
         ECrit     = REAL   [0.000,0.100]    : 'Energy sampling unit (GeV)',
         EMip      = REAL   [0.00,1.00]     : 'Mip energy/sampling (ECrit)',
         EThr      = REAL   [0.0,3.0]       : 'Thresh. for shower (ECrit)',
         Z1        = REAL   [69.00,70.00]   : 'Z of light material',
         Z2        = REAL   [75.00,76.00]   : 'Z of heavy material',
         R1        = REAL   [1.700,1.800]   : 'Rad.length of light mat.',
         R2        = REAL   [1.100,1.200]   : 'Rad.length of heavy mat.')
      ;
 
 LSHO
      :      'Parameters for e-gamma showers in LCAL (after 1994) (LTC)'
              SIZE 1,1
 
      = (ValRng(2),
         P1        = REAL   [0.000,10.000]   : 'Parameter 1 (see LCSHOW)',
         P2        = REAL   [0.000,10.000]   : 'Parameter 2',
         P3        = REAL   [0.000,10.000]   : 'Parameter 3',
         P4        = REAL   [0.000,10.000]   : 'Parameter 4',
         P5        = REAL   [0.000,1.000]    : 'Parameter 5',
         P6        = REAL   [0.000,1.000]    : 'Parameter 6',
         P7        = REAL   [0.000,1.000]    : 'Parameter 7',
         P8        = REAL   [0.000,1.000]    : 'Parameter 8',
         D1        = REAL   [0.000,2.000]    : 'dP1/dlogE ',
         D2        = REAL   [-2.000,2.000]   : 'dP2/dlogE ',
         D3        = REAL   [0.000,2.000]    : 'dP3/dlogE ',
         D4        = REAL   [-2.000,2.000]   : 'dP4/dlogE ',
         D5        = REAL   [0.000,2.000]    : 'dP5/dlogE ',
         ECrit     = REAL   [0.0000,0.1000]  : 'Energy sampling unit (GeV)',
         EMip      = REAL   [0.00,1.00]      : 'Mip energy/sampling (ECrit)',
         EThr      = REAL   [0.0000,0.100]   : 'Thresh. for shower param (GeV)',
         SConst    = REAL   [0.000,0.100]    : 'Constant DE/E fluctuation',
         SSampl    = REAL   [0.000,0.100]    : 'Extra DE/E = SSampl/sqrt(E) fluctuation',
         Z1        = REAL   [69.00,70.00]    : 'Z of light material',
         Z2        = REAL   [75.00,76.00]    : 'Z of heavy material',
         R1        = REAL   [1.700,1.800]    : 'Rad.length of light mat.',
         R2        = REAL   [1.100,1.200]    : 'Rad.length of heavy mat.')
      ;
 
 LCEL
      :      'Constants characterizing the LCAL signals. (LTC)'
              SIZE 1,1
 
      = (ValRng(2),
         AdcMev       = REAL  [1.00,8.00]   : 'raw MeV per ADC count',
         THresh(3)    = INTE  [1,1000]      : 'Thresh for readout (AM))',
         SNoise(3)    = REAL  [0.0,100.0]   : 'Incoherent storey noise (AM)',
         TNoise(3)    = REAL  [0.0,100.0]   : 'Incoherent trigger noise (AM)',
         WNoise(3)    = REAL  [0.0,100.0]   : 'Incoherent wire noise (AM)',
         CNoise       = REAL  [0.00,100.00]   : 'Coherent noise level (AM)',
         GVar         = REAL  [0.000,0.100] : 'Sigma of gain variations',
         TRig         = INTE  [0,100000]    : 'Trigger threshold (MeV)')
      ;
 
 LCRE
      :      'Constants used for LCAL reconstruction. (LTC)'
              SIZE 1,1
 
      = (ValRng(2),
         SAtrlimit    = REAL [.080,.100]    : 'max thetha of SATR region (rad)',
         ECluster     = REAL [.050,10.000]    : 'Cut on cluster energy (GeV)',
         ESeed        = REAL [.020,1.000]    : 'Cut on cluster seed tower (GeV)',
         ETower       = REAL [.020,1.000]    : 'Cut on cluster tower (GeV)',
         EnergyError  = REAL [.10,.30]      : 'Standar energy error (Gev)**1/2',
         ELeak        = REAL [.00,1.00]     : 'Cut on storey 3 fraction',
         E4towers     = REAL [.100,1.000]   : 'Expected fraction in 4 central towers',
         D4towers     = REAL [.000,1.000]   : 'Expected rms of 4-tower fraction',
         E9towers     = REAL [.100,1.000]   : 'Expected fraction in 9 central towers',
         D9towers     = REAL [.000,1.000]   : 'Expected rms of 9-tower fraction',
         EIdent       = REAL [.00,100.00]   : 'Cut on electron candidate energy (GeV)',
         NEar(8)      = INTE [-500,500]     : 'Offsets of 8 neighbor cell addresses',
         Z1dist       = REAL [262.00,300.00]: 'Average z of storey 1',
         Z2dist       = REAL [262.00,300.00]: 'Average z of storey 2',
         Z3dist       = REAL [262.00,300.00]: 'Average z of storey 3',
         ZCdist       = REAL [262.00,300.00]: 'Reference z of clusters',
         NDepth       = INTE [1,38]         : 'Reference plane number',
         SRef         = REAL [2.8000,3.2000]: 'Reference pad size',
         S1radl       = REAL [0.00,30.00]   : 'Average s of storey 1 (rl)',
         S2radl       = REAL [0.00,30.00]   : 'Average s of storey 2 (rl)',
         S3radl       = REAL [0.00,30.00]   : 'Average s of storey 3 (rl)',
         DTrack       = REAL [0.00,10.00]   : 'Cut on track-cluster distance',
         MCarlo       = REAL [0.000,1.000]  : 'GeV/count for MC digitizations')
      ;
 
 LECA
      :      'Lcal module calibration constants (LTC) \
              number of words / module\
              number of modules'
              SIZE 4,4
 
      = (ValRng(2),
         StoreyCal  = REAL [0.500,1.500]  : 'Beam energy/Raw Storey sum',
         WireCal    = REAL [0.500,1.500]  : 'Beam energy/Raw wire sum')
      ;
 
 LACC
      :      'Pad columns used for Bhabha acceptance. (LTC)\
              number of words / pad column\
              number of pad columns'
              SIZE 16,16
 
      = (ValRng(2),
         LOwrow(2) = INTE [0,15]  : 'Lowest accepted row\
                                     (loose,strict)',
         HIrow(2)  = INTE [0,15]  : 'Highest accepted row\
                                     (loose,strict)',
         LeftRight(2) = INTE [0,3]   : 'Acceptance flag (loose,strict):\
                                       0: Inside\
                                       1: Leftmost\
                                       2: Rightmost\
                                       3: Outside')
      ;
 
 BHAB
             : 'Cuts used for Bhabha event selection.
                NR=run number. (STC)\
                number of words / selection\
                number of selections'
                SIZE 7,*
 
             = (ValRng(2),
                MEthod      = INTE [0,15]      :'Method for defining acc.',
                QualFlag    = INTE [0,1]       :'Track quality',
                EnergyHi    = REAL[10.0,50.0]  :'High energy threshold',
                EnergyLo    = REAL[10.0,50.0]  :'Low energy threshold',
                DPhimin     = REAL[0.0000,3.1416] :'Min phi separation',
                XLow(2)     = REAL[-1.00,24.50]  :'Min distance to inner X edge',
                YLow(2)     = REAL[-1.00,24.50]  :'Min distance to inner Y edge',
                CosphLo(2)  = REAL[0.000,1.001]:'Min abs(cos(phi))',
                CosphHi(2)  = REAL[0.000,1.001]:'Max abs(cos(phi))',
                ThetaLo(2)  = REAL[0.0000,0.2000]:'Min theta',
                ThetaHi(2)  = REAL[0.0000,0.2000]:'Max theta',
                CrossSect   = REAL[0.00,100.00]:'Cross-section (nb)',
                ERror       = REAL[0.00,100.00]:'Systematic error (nb)',
                GEnerator   = INTE[0,*]        :'Generator code',
                ESum        = REAL[20.00,200.00]:'Energy sum threshold',
                Egenerator  = REAL[10.00,100.00]:'Beam energy')
             ;

 BHZ0
             : 'Cuts used for Bhabhas in LEPII calibration runs
                NR=run number. (STC)\
                number of words / selection\
                number of selections'
                SIZE 7,*
 
             = (ValRng(2),
                MEthod      = INTE [0,15]         : 'Method for defining acc.',
                QualFlag    = INTE [0,1]          : 'Track quality',
                EnergyHi    = REAL[10.0,50.0]     : 'High energy threshold',
                EnergyLo    = REAL[10.0,50.0]     : 'Low energy threshold',
                DPhimin     = REAL[0.0000,3.1416] : 'Min phi separation',
                XLow(2)     = REAL[-1.00,24.50]   : 'Min distance to inner X edge',
                YLow(2)     = REAL[-1.00,24.50]   : 'Min distance to inner Y edge',
                CosphLo(2)  = REAL[0.000,1.001]   : 'Min abs(cos(phi))',
                CosphHi(2)  = REAL[0.000,1.001]   : 'Max abs(cos(phi))',
                ThetaLo(2)  = REAL[0.0000,0.2000] : 'Min theta',
                ThetaHi(2)  = REAL[0.0000,0.2000] : 'Max theta',
                CrossSect   = REAL[0.00,100.00]   : 'Cross-section (nb)',
                ERror       = REAL[0.00,100.00]   : 'Systematic error (nb)',
                GEnerator   = INTE[0,*]           : 'Generator code',
                ESum        = REAL[20.00,200.00]  : 'Energy sum threshold',
                Egenerator  = REAL[10.00,100.00]  : 'Beam energy')
             ;

 LCCA
      :      '<Edata-Emc>/Emc in % at generic sites. (LTC) \
                number of words / padrow\
                number of padrows'
              SIZE 15,15
 
      = (ValRng(2),
         DIfference(16)  = INTE  [-100,100]    : 'Edata-Emc at column j')
      ;

 LFIP
      :      'Lcal t0 timing \
              Number of words/run \
              Number of rows'
           STATIC
 
      = (TI(4)        = REAL   :' timing of mudule \
                                    1 - 4      ' )
      ;

 LSNT
      :      'Lcal Scintillator constants\
              Number of attributes/row\
              Number of rows'
              SIZE 1,1
 
      = (ValRng(2),
         AttLen     = REAL  [0.0,1000.0]  : 'Attenuation const of scint',
         RadLen     = REAL  [0.0,10.0]    : 'Length in rl of scint and lead',
         Fluct      = REAL  [0.0,2.0]     : 'Fluctuation in energy dep',
         Thresh     = REAL  [0.000,0.010] : 'Detection threshold in GeV',
         MipMev     = REAL  [0.0,10.0]    : 'MeV per mip',
         MevAdc(4)  = REAL  [0.0,3000.0]  : 'Adc counts per MeV in each device',
         SAturation = INTE [1000,10000]   : 'Adc saturation',
         NScint     = INTE [4,4]          : 'Number of scintillators',
         NPm        = INTE [2,2]          : 'Photomultipliers per scintillator')
      ;

 
 END ESET
 END SUBSCHEMA

 
 SUBSCHEMA McOffConsts
 :'Contains the constants used in the MC program'
 
 
 AUTHOR   'B. Bloch '
 VERSION  '1.3'
 DATE     '20/09/89'
 
 
 DEFINE ESET
 
 GDEC
      :      'Particle decay modes and branching ratios.
              This will in the future hopefully be taken
              directly from the PDG data-base. (LTC)\
              Number of words/row\
              Number of rows'
           STATIC
 
      = (PartNumb     = INTE [1,100]       : 'Geant Particle Number',
         BranRat(6)   = REAL [0.,100.000]  : 'Bran. ratios (up to 6)',
         DecMode(6)   = INTE [0,999999]    : 'Decay modes ')
      ;
 
 
 PART
      :      'PARTicle type. (LTC)\
              Number of words/particle\
              Number of particles'
           STATIC
 
      = (GeantNumb    = INTE [1,100]       : 'Geant particle number\
                                              Particle 1-52 are used by
                                              Geant. Dont change the
                                              number of existing part.',
         NAme(3)      = CHA4               : 'Particle name',
         COde         = INTE [1,6] | 100   : 'Geant tracking code\
                                              1 = Photons\
                                              2 = Electrons\
                                              3 = Neutr. Hadrons + \
                                                  Neutrinos\
                                              4 = Charged Hadrons\
                                              5 = Muons\
                                              6 = Geantinos\
                                            100 = Not tracked particle',
         MAss         = REAL[0.,500.000000]: 'Particle Mass',
         CHarge       = REAL [-2.00,2.00]  : 'Particle Charge',
         LifeTime     = REAL [*,*]         : 'Particle Lifetime',
         MassWidth    = REAL[0.,100.000000]: 'Particle Mass Width',
         ANtiparticle = INTE [1,800]       : 'Corresponding antipart number')
      ;
 
 
 END ESET
 
 
 END SUBSCHEMA
 

SUBSCHEMA MUONEfficiency
 : 'Descripton of HCAL and MUON tube efficiency banks'

 AUTHOR    'D. Cinabro'
 REVIEWER  'R. Tenchini,G.Capon,G.taylor'
 VERSION   '3.1'
 DATE      '15/01/93'

 DEFINE ESET


 HBAD
      :        'List of runs from 1992 onwards that are bad for muon id
                because of a hardware problem with the HCAL
                or muon chambers\  
                Number of bad runs.\
                Number of columns (1).'
             STATIC

      = (RunNumber = INTE [1,*] : 'Run bad for muon identification.')
      ;

 HDTE
      :         'HCAL tube efficiency for a running period
                 using a module numbering scheme.
                 Period 1 is Monte Carlo for Galeph versions >= 238.
                 Period 2 is Monte Carlo for Galeph versions >= 242.
                 Period 90 is 1990 data. 
                 Period 91 is 1991 data. etc\
                 Number of double planes.\
                 Number of modules (36).'
              STATIC

      = (DblEffic(12) = REAL [0.0,2.0] : 'Double plane efficiency\
                                          Rows  1:6  are endcap A\
                                          Rows  7:30 are barrel\
                                          Rows 31:36 are endcap B')
      ;

 HZEF
      :         'Z dependence of the HCAL tube efficiency in the barrel for a 
                 running period using a module numbering scheme.
                 Period 90 is 1990 data. 
                 Period 91 is 1991 data. etc\
                 Number of special groups of double planes.\
                 Number of parameters (5).'
              STATIC

      = (ModNumber = INTE [1,36] : 'Barrel Module number [1:24].',
         DoubPlane = INTE [1,12] : 'Highest double plane number [inclusive]
                                    for which efficiency applies.',
         CoEff(3)  = REAL [*,*]  : 'Coefficients of the 2nd order 
                                    polynomial used to describe the 
                                    Z dependence of the efficiency.')
      ;


 MCPE
      :         'Bank number 90 Muon Chamber plane efficiency for 1990.
                 Bank number 91 Muon Chamber plane efficiency for 1991.etc
                 Old scheme had (6=11=1990),(7=12=1991).\
                 Number of planes.\
                 Number of Chambers.(88)'
               STATIC

      = (PlaneEff(2) = REAL [0.0,1.0] : 'Efficiencies of Muon Chamber planes\
                                         0.0 means chamber is not installed.')
      ;
     

 END ESET

 END SUBSCHEMA


SUBSCHEMA SatrOffConsts
 
  : 'Description of BOS banks for calibration constants and cuts for
     the Small Angle Tracking Device'
 
  AUTHOR        'H. Meinhard'
  REVIEWER      ' '
  VERSION       '1.07'
  DATE          '08/03/91'
 
  DEFINE ESET
 
  SCLB
        : 'SATR calibration constants (STC)\
           number of words / row\
           number of rows'
        SIZE 1,1
        = (ValRng(2) = Date :'Beginning & expiration date',
           NumberWords = INTE [72,72]: 'Number of words for bit pattern',
           ChanBitpatt(72) = INTE [*,*] : 'Bit pattern indicating whether
                             channel works (1 = works, 0 = doesnt work);
                             Numbering of channels: (Crate-1)*384 +
                             (Card-1)*16 + (TDC-1). Lowest five bits of
                             channel number give the bit position, higher
                             bits + 1 are the Channel Bit pattern word to
                             be used.',
           TimeOffset = REAL [-1.000E-6,1.000E-6]: 'Time offset for conversion
                             from TDC time to drift time:
                             drift time = time offset - TDC time',
           NumberofxtPoints = INTE [2,100]: 'Number of points in table for the
                             drift time to distance relation',
           TimeBegin = REAL [-1.000E-6,1.000E-6]: 'Abszissa (time) of first
                             point in x-t-relation',
           TimeDelta = REAL [-1.000E-6,1.000E-6]: 'Time distance between
                             two adjacent points in x-t-relation table',
           TimeSpace(100) = REAL [0.000,.510]: 'Ordinates (drift distances)
                             of points in x-t-relation table',
           timCorrcRate(6) = REAL [-1.000E-6,1.000E-6]: 'Time correction for
                             TDC crates',
           timCorrcHan = REAL [-1.000E-6,1.000E-6]: 'Time correction for TDC
                             channels connected to single wire')
        ;
 
  SRKT
        : 'SATR reconstruction cuts (STC)\
           number of words / row\
           number of rows'
        SIZE 1,1
        = (ValRng(2) = Date :'Beginning & expiration date',
           TimeLower = REAL [-1.000E-6,1.000E-6]: 'Lower cut on TDC times
                             in routine SRDIST',
           TimeUpper = REAL [-1.000E-6,1.000E-6]: 'Upper cut on TDC times
                             in routine SRDIST',
           RadiusUpper = REAL [0.000,.510]: 'Upper cut on drift distance
                             in routine SRDIST',
           SIgma = REAL [0.000,.1]: 'Assumed spatial resolution of wire
                             tubes',
           DeltaTheta = REAL [0.00E-3,2.50E-3]: 'Theta window width for
                             search for wire parallels in SRWPAR',
           DeadzoneSector = REAL [0.000,.500]: 'Inefficient zone between
                             adjacent sectors per sector',
           DeadzoneGaschannel = REAL [0.000,3.000]: 'Inefficient zone between
                             sectors with gas channel per sector',
           NumParallels = INTE [3,9]: 'Minimal number of wire parallels
                             to perform any track fit',
           CHisquprob = REAL [0.000,0.100]: 'Chisquare probability that
                             a track fit must have at most in order to
                             be accepted',
           CutonTrackdist = REAL [0.00,100.00]: 'Cut on the distance between
                             two tracks, divided by the error of the distance.
                             Used in routine SRSELO to decide whether two
                             tracks are close together or not',
           CutonPatchdist = REAL [0.00,50.00]: 'Same as CutonTrackdist for
                             patch parameters',
           ZoneWidth = REAL [0.500,1.5]: 'Zone width for cluster / patch
                             finding',
           SigTwoarm = REAL [0.000,.5]: 'Assumed spatial resolution of wire
                             tubes, only for 2-arm fits',
           MinPar2 = INTE [3,9]: 'Minimum number of wire parallels per side
                             for 2-arm fits',
           MaxDrop2 = INTE [0,18]: 'Maximum number of wire parallels to drop
                             if 2-arm fit unsuccessful')
        ;
 
  SWTU
        : 'SATR wire tube sectors with swapped sockets (STC)\
           number of words / row\
           number of rows'
        SIZE 0, *
        = (ValRng(2) = Date :'Beginning & expiration date',
           SIde = INTE [1,2]:'Side of wire tube sector',
           LAye = INTE [1,9]:'Layer of wire tube sector',
           SeCt = INTE [1,8]:'Wire tube sector number')
        ;
 
  END ESET

END SUBSCHEMA


SUBSCHEMA ScalOffConsts
 :'Constants used in the Sical code'

 AUTHOR   'B.Bloch-Devaux '
 REVIEWER '   '
 VERSION  '3.9'
 DATE     '18/10/99'



 DEFINE ESET


 SCOR
      :      'Corrections to cluster position . (LTC)\
              number of words / row   \
              number of rows'
              STATIC

  =  (RCorr(3)     = REAL [0.0000,*]   : 'Corrections to R position',
      PhCorr(4)    = REAL [0.00000,*]  : 'Corrections to Phi position')
      ;


 SREC
      :      'Constants used for Sical reconstruction. (LTC)\
              number of words / row   \
              number of rows'
              STATIC
                                                                           
  =  (ECluster     = REAL [.010,50.000] : 'Cut on cluster energy (GeV)',
      ESeed        = REAL [.010,1.000]  : 'Cut on cluster seed pad energy(Gev)',
      EPad         = REAL [.010,1.000]  : 'Cut on cluster pad energy,(Gev)')
      ;

 SECA
      :      'Sical module calibration constants
              NR= Run number (STC)\
              number of words / module\
              number of modules'
              STATIC

      = (PadCal  = REAL [0.500,1.500]  : 'Beam energy/Raw Pad sum')
      ;

 SECB
      :      'Sical module calibration constants per bunch
              NR= Run number (STC)\
              number of words / bunch\
              number of bunches'
              STATIC

      = (EcalA  = REAL [0.500,3.500]  : 'Beam energy/Raw Pad sum Side A ',
         EcalB  = REAL [0.500,3.500]  : 'Beam energy/Raw Pad sum Side B ')
      ;

 SECT
      :      'Sical module gain correction per bunch
              NR= Run number (STC)\
              number of words / bunch\
              number of bunches'
              STATIC

      = (CorA  = REAL [0.500,2.000]  : 'gain Correction Side A ',
         CorB  = REAL [0.500,2.000]  : 'gain Correction Side B ')
      ;

 SIBX
             : 'Cuts used to define box of out-of-time events during 95 SCAN.
                NR=run number. (STC)\
                number of words / definition\
                number of definitions'
              STATIC

         = (DeltaR      = REAL [0.,10.]     :'Lowest square Radial width\
                                             for clusters in box',
            EnergyC     = REAL[ 0.,1.0000]  :'highest Energy in % of\
                                             Beam energy  for Cluster in box')
            ;

 SNOI
             : 'Cuts used to identify electronic noise clusters
                NR=run number. (STC)\
                number of words / type of noise\
                number of noise types'
              STATIC

         = (EnFracmin  = REAL [0.,1.00]   :'Min energy fraction in one plane',
            RadVarmin  = REAL [0.,100.00] :'Min radial variance',
            typNumPad  = REAL [0.,100.00] :'typical number of pads')
            ;
                              
 SHLD
             : 'Hold timing curve parametrization.
                NR=run number. (STC)\
                number of words / parameter\
                number of parameters'
              STATIC

         = (ParaM      = REAL [-100.,100.]     :'ParaMeter value ')
            ;

  SBHA
             : 'Cuts used for Bhabha event selection.
                NR=run number. (STC)\
                number of words / selection\
                number of selections'
              STATIC

         = (MEthod      = INTE [0,15]         :'Method for defining acc.',
            EnergyS     = REAL[ 0.,1.000]     :'Single side energy threshold\
                                               in % of Beam energy',
            EnergyT     = REAL[ 0.,1.0000]    :'Total energy threshold in \
                                               % of CMS energy',
            DphimiN     = REAL[0.0000,360.]   :'Min phi separation in deg',
            DphimaX     = REAL[0.0000,360.]   :'Max phi separation in deg',
            TInner      = INTE [1,   16]      :'Innermost Radius fidu side',
            TOuter      = INTE [1,   16]      :'Outermost radius fidu side',
            LInner      = INTE [1,   16]      :'Innermost radius non fidu side',
            LOuter      = INTE [1,   16]      :'Outermost radius non fidu side',
            phiVmiN     = INTE [1,   32]      :'Min Phi bin fidu side Vert',
            phiVmaX     = INTE [1,   32]      :'Max Phi bin fidu side Vert',
            phiHmiN     = INTE [1,   32]      :'Min Phi bin fidu side Hor',
            phiHmaX     = INTE [1,   32]      :'Max Phi bin fidu side Hor',
            AsymCut     = REAL[-1.0000,1.0]   :'Asymmetry cut value',
            CrossSect   = REAL[0.,500.0000]   :'Cross-section (nb)',
            XsectVer    = REAL[0.,500.0000]   :'Cross-section Vert (nb)',
            XsectHor    = REAL[0.,500.0000]   :'Cross-section Horiz(nb)',
            ERror       = REAL[0.,1.00000]    :'Systematic error (absolute)',
            SystTheo    = REAL[0.,1.00000]    :'Systematic theory error (absolute)',
            GEnerator   = INTE[0,*]           :'Generator code',
            EGenerator  = REAL[10.00,100.00]  :'Beam energy')
            ;

 SCAX
      :      'Scal Calibration constants of AmpleX channels
              NR= Run number (STC)\
              number of words / amplex\
              number of amplexes'
              STATIC

      = (ChaCal(16)  = REAL [*,*]  : 'relative Channel Calib ')
      ;

 SCGA
             : 'Scal Gain calibration constants per pad
                NR=run number. (STC)\
                number of words / pad\
                number of pads '
              STATIC

             = (PhiIndex    = INTE [0,31]      :'Phi Index of amplex',
                ZlayIndex   = INTE [0,11]      :'Z layer index of amplex',
                FitAvgain(3)= REAL [*,*]       :'Fit average gain coeff',
                FitavG(2)   = REAL [*,*]       :'Fit average coeff')
             ;

 SCPH
             : 'Scal Gain calibration correction per phi pad
                NR=run number. (STC)\
                number of words / module\
                number of module '
              STATIC

          = (PhiCorr(32) = REAL [0.,2.]     :'Gain correction for Phi pad')
             ;
 SCBP
             : 'Scal gain Calibration correction per Bunch and Phi pad 
                NR=run number. (STC)\
                number of words / module\
                number of module '
              STATIC

          = (PhiCorr(32) = REAL [0.,2.]     :'Gain correction for Phi pad')
             ;

 SCFD
             : 'Scal Gain calibration fudge factors
                NR=run number. (STC)\
                number of words / correction\
                number of correction'
              STATIC

             = (FUdge       = REAL [0.,1.]     :'Fudge factor correction...')
             ;

 SDCO
      :      'Corrections to dead pad energy  . (LTC)\
              number of words / plane \
              number of planes'
              STATIC

  =  (IPlane       = INTE [1,12]       : 'Plane number',
      ECorr(3)     = REAL [-1.,1.]  : 'Coefficients of parabolic extrapolation\
                                        from adjacent energies')
      ;
                        


 END ESET
 END SUBSCHEMA
 

 SUBSCHEMA TpcOffConsts
 : 'Description of TPC constants banks'
 
 AUTHOR   'W. Wiedenmann,A.Bonissent,S.Monteil'
 REVIEWER 'W.Wiedenmann'
 VERSION  '6.3'
 DATE     '05/07/2000'
 
 DEFINE ESET

TRZS                                                                            
      :      'TPC z coordinate corrections                                      
              due to sector missalignments in the                               
              Rz plane, calibrated with VDET.\
              number of word/slot\
              number of slots'                                  
                                                                                
        STATIC
                                                                                
      = (SlonmB      = INTE [1,36]               : 'Slotnumber in Reconstruction',         
         rDsSlo      = Length [*,*]              : 'Displacement of Sector in z - direction',               
         RotSlo      = Angle  [-0.01000,0.01000] : 'Rot. of Sector about its center in the                  
                                                    Rz-plane in order to align its symm. axis
                                                    with the radial line. (The center of a            
                                                    sector is on its symm. axis at a radius which is the                   
                                                    arithmetic mean of its two outermost padrow radii',                    
         AdsSlo      = Angle  [-0.01000,0.01000] : 'Del-displ. of Sect. (rot. about radial line)')          
      ;                                                                         
                                                                                

TZCV  :      'TPC z coordinate corrections
              calibrated by VDET,
              for TPC sides A (row 1) and B (row 2). (STC)'
      = (ValRng(2),
         AlphA   = REAL   [*,*]           : 'Correction to drift velocity',
         BetA    = REAL   [*,*]           : 'Correction for angular z bias')
      ;


 TDBS :  'TPC bad sector list, giving for
          each run range a list of sectors not
          running at the full voltage.  This is
          used to know whether the dE/dx bit can
          be tested, for example. (STC)\
          Number of columns per run range.\
          Number of run ranges.'

        = (ValRng(2),
           SectorId    = INTE [1,36]       : 'Sector number of the
                                              chamber which is not
                                              at full voltage');           

  TLCP
      :      'Tpc Laser Correction Parameters
              due to field distortions.  This bank is obsolete
              and is only used for 1989/1990 data. (STC)\
              Number of theta bins x 2 ( side A , B ) \
              Number of coeff. in expansion (6) times 2 (trim coils
              off or on)'
 
      = (    ValRng(2),
             A1           = REAL  [*,*]        :'coeff. side A, 30deg',
             A2           = REAL  [*,*]        :'coeff. side A, 39deg',
             A3           = REAL  [*,*]        :'coeff. side A, 67deg',
             B1           = REAL  [*,*]        :'coeff. side B, 30deg',
             B2           = REAL  [*,*]        :'coeff. side B, 39deg',
             B3           = REAL  [*,*]        :'coeff. side B, 67deg'
        )
      ;

  TLFC
      :      'Tpc Laser Field Correction parameters
              due to field distortions. This bank replaces
              TLCP, which becomes obsolete starting with
              1991 data. (STC)\
              Number of theta bins x # phi bins * 2 ( side A , B ) \
              Number of coeff. in expansion (6)'
 
      = (    ValRng(2),
             A0(3)        = REAL  [*,*]        :'coeff. side A, 18deg\
                                                 Index refers to phi',
             A1(3)        = REAL  [*,*]        :'coeff. side A, 30deg',
             A2(3)        = REAL  [*,*]        :'coeff. side A, 39deg',
             A3(3)        = REAL  [*,*]        :'coeff. side A, 67deg',
             B0(3)        = REAL  [*,*]        :'coeff. side B, 18deg',
             B1(3)        = REAL  [*,*]        :'coeff. side B, 30deg',
             B2(3)        = REAL  [*,*]        :'coeff. side B, 39deg',
             B3(3)        = REAL  [*,*]        :'coeff. side B, 67deg'
        );
 
 
 TERN
     :    'Tpc pad response width and coordinate error
           based on pre fitted tracks parameters and corrections
           for half pads  (STC)\
           Number of columns \
           Number of rows'
 
        = (ValRng(2),
          P2 = REAL [0.00,*] : 'Dependance of the pad response width on pad
                                crossing angle for 2 pad clusters',
          P3 = REAL [0.00,*] : 'Dependance of the pad response width on pad
                                crossing angle for 3 pad clusters',
          PB = REAL [0.0000,*] : 'Dependance of the pad response width on
                                  wire crossing angle Beta',
          U0 = REAL [0.0000,*] : 'Contribution to R-Phi Coordinate error for
                                  radial tracks',
          UA = REAL [0.0000,*] : 'Additional contribution to R-Phi Coordinate
                                  error for non-radial tracks',
          U1 = REAL [0.0000,*] : 'Contribution to R-Phi Coordinate error due to
                                  drift length constant term',
          U2 = REAL [*,*] : 'Contribution to R-Phi Coordinate error due to drift
                             length linear term',
          U3 = REAL [0.000000000,*] : 'Contribution to R-Phi Coordinate error
                                       due to drift length quadratic term',
          Z0 = REAL [0.0000,*] : 'Contribution to Z Coordinate error for
                                  perpendicular tracks',
          ZL = REAL [0.0000,*] : 'Additional contribution to Z Coordinate error
                                  due to track dip angle',
          E1 = REAL [0.000,*] : 'Multiplier for Coordinate errors for
                                 coordinates with one half pad',
          E2 = REAL [0.000,*] : 'Multiplier for Coordinate errors for
                                 coordinates with two half pads',
          D1 = REAL [0.000,*] : 'Multiplier for half pad charge for clusters
                                 with one half pad',
          D2 = REAL [0.000,*] : 'Multiplier for first half pad charge for
                                 clusters with two half pads',
          D3 = REAL [0.000,*] : 'Multiplier for second half pad charge for
                                 clusters with two half pads',
          H0 = REAL [*,*] : 'Empirical shift of coordinates with two half pads
                             constant term',
          H1 = REAL [*,*] : 'Empirical shift of coordinates with two half pads
                             linear term',
          HM = REAL [*,*] : 'Empirical shift of coordinates with two half pads
                             exponential multiplier',
          HP = REAL [*,*] : 'Empirical shift of coordinates with two half pads
                             exponential power',
          HC = REAL [*,*] : 'Empirical shift of coordinates with two half pads
                             cutoff')
       ;
 
 
 TGTN:    'Measurements of TPC gas attenuation.(STC)\
           Number of columns\
           Number of rows'
       = (ValRng(2),
          DateFlush       = INTE [0,99999999] :'Date of last full flush of the TPC',
          DateAttenuation = INTE [0,99999999] :'Date of last full-scale attenuation measurement
                                                as done approximately every month at gas building',
          ATenuation      = INTE [0,1000]     :'Measured attenuation in 0/00 per metre (manual)',
          OXygen          = INTE [0,1000000]  :'ppm of oxygen at time of measurement',
          H2O             = INTE [0,1000000]  :'ppm of water vapours at time of measurement');
 
 
 T1FC :      'Tpc magnetic Field Corrections, table 1.
              Table of grid specifications, with one
              row for each of 3 coordinate axes.(STC)\
              Number of words per row\
              Number of rows=3 (r,phi,z)'
       = (ValRng(2),
          LowLimit      = REAL [*,*]        : 'Lower limit of grid',
          UpperLimit    = REAL [*,*]        : 'Upper limit of grid',
          StepSize      = REAL [*,*]        : 'Step size between points',
          NumPoints     = INTE [*,*]        : 'Number of grid points');
 
 
 TCON
      :      'Tpc reconstruction CONstants.  This bank is
              for the most part obsolete and will go away eventually. (DROP)\
              Number of parameters    \
              1 '
 
      = (ValRng(2),
         DV            = REAL [0.00000,*]  : 'Obsolete',
         SampleLength  = REAL [0.000,*]    : 'Obsolete',
         TimeOffset    = REAL [*,*]        : 'Obsolete',
         ResponseWidth = REAL [0.000,*]    : 'Pad response width for Monte Carlo (cm)',
         SigmaD        = REAL [0.000,*]    : 'Diffusion constant for pad response \
                                              width for Monte Carlo (cm/sqrt(cm))',
         OmegaTau      = REAL [0.000,*]    : 'omega*tau/B for PRW parameterization in Monte Carlo')
      ;
 
 
 TCRC
      :      'Tpc CooRdinate Cuts  (STC)\
              Number of parameters    \
              1 '
 
      = (ValRng(2),
         AlgNumber     = INTE [1,*]        : 'default algorithm choice (see TCOALG for details)',
         AlgParm       = REAL [0.000,*]    : 'default algorithm parm. (see TCOALG for details)',
         EdgeCut       = INTE [0,*]        : 'maximum number of pads in cluster if 2 half pads are included',
         ProxCut       = REAL [0.000,*]    : 'cut on how far in r*phi outside of a subcluster the coordinate\
                                              can be, where the subcluster boundaries are the centers of the\
                                              first and last pads (units are pad spacings (.67cm))')
      ;
 
 TLCT :      'Tpc cLuster CuTs  (STC)\
              Number of parameters\
              1'
 
      = (ValRng(2),
         TimeAlg(2)   = INTE [1,3]         : 'Prefered algorithm to use for time of pad subpulse.
                                              Index 1 refers to small dip angle region.
                                              Index 2 refers to large dip angle region.\
                                              Current choices:\
                                                   1 = average of leading & trailing threshold crossings\
                                                       (with correction for pulse-shape and wires).\
                                                   2 = average of leading & trailing threshold crossings\
                                                       (without correction for pulse-shape).  Julia versions\
                                                       upto 285 used this by default.
                                                   3 = average of leading & trailing threshold crossings\
                                                       (with correction for pulse-shape and without wires)',
         ChargeAlg(2) = INTE [1,2]         : 'Prefered algorithm type for charge of a pad subpulse.\
                                              Index 1 refers to small dip angle region.\
                                              Index 2 refers to large dip angle region.\
                                              Current choices:\
                                                   1 = parabola algorithm,\
                                                   2 = total area under the pulse.',
         OverlapOpt(2)= INTE [0,1]         : 'Set to 1 to allow parabola algorithm to be used
                                              in case of pulse overlap even though area or
                                              leading edge algorithms are prefered according
                                              to ChargeAlg and TimeAlg.',
         AlgForce     = INTE [0,1]         : 'Set to 1 to ignore overlap problems and use the
                                              prefered algorithm in any case.',
         PulseThresh  = INTE [1,*]         : 'Threshold in TPLANA used to define the beginning and end of a subpulse',
         PeakCut      = INTE [11,*]        : 'Definition of sufficient size for a peak in TPLANA.
                                              The (maximum ph of the peak)*10/(PeakCut) must be greater than the
                                              minimum ph of the adjacent valleys in order for a peak to
                                              become a subpulse.',
         DefMax(3)     = REAL [0.000,*]     : 'Definition of a peak in TPLANA.  For a relative maximum to
                                              be considered a significant maximum, the (peak sample ph)*10/F(cost)
                                              must be greater than the minimum ph of the valley on each side,
                                              where/ F(x)= INT(10*(DefMax(1)+exp((x-DefMax(2))/DefMax(3)))),/
                                              and cost is the cosine of the polar angle of the cluster',
         MinimumLen   = INTE [1,*]         : 'Minimum number of samples over threshold in TPLANA for valid subpulse',
         TanLdivide   = REAL [0.00,*]      : 'Division point in tan(lambda) in TPLANA.  For each cluster, tan(lambda)
                                              is calculated assuming a straight line from the origin to the padrow
                                              center with z given by the first sample of the cluster.  Whether it is larger
                                              than TanLdivide affects the choice of charge and time estimators.',
         miNPulses    = INTE [2,19]        : 'minimum number of pads in a cluster',
         maXPulses    = INTE [3,20]        : 'maximum number of pads in a cluster',
         mInBuckets   = INTE [3,49]        : 'minimum length in buckets of a cluster',
         mAxBuckets   = INTE [4,50]        : 'maximum length in buckets of a cluster',
         ZWindow(2)   = REAL [1,*]         : 'Z window for TPLANA in buckets.  Two subpulses are associated if their peaks
                                              line up in time within ZWindow and they overlap each other in time.
                                              The actual window is equal to ZWindow(1)+tan(lambda)*ZWindow(2)',
         FullAnalysis = INTE [0,20]        : 'Maximum number of pads in subcluster to do full analysis in TCLPAT.
                                              For subclusters larger than this cut no attempt will be made to
                                              make a separation into individual peaks in r*phi.',
         RphiMxdef    = INTE [11,*]        : 'Definition of minimum in R*Phi in TCLPAT.  If the ph of the
                                              valley subpulse times RphiMxdef/10 is less than that of the
                                              peak subpulses on each side, then the minimum is significant',
         PeakMin      = INTE [1,*]         : 'Cut to define a breakpoint in r*phi in TCLPAT.  If a peak is
                                              PeakMin or more times both adjacent minima, then the peak can
                                              can be used to form a separate subcluster',
         OverCut      = INTE [0,*]         : 'Cut on pulse height overlap with adjacent pulses in TCLPAT.
                                              A subcluster is marked bad if its first and last subpulses overlap
                                              excessively with adjacent pulses.  The ratio of sample pulse heights
                                              in the adjacent pulse to the corresponding pulse height in the edge subpulse
                                              within the overlap region must be less than OverCut',
         OverParm     = INTE [1,*]         : 'Length of overlap required before making the cut OverCut',
         QuicKanalysis= INTE [0,1]         : 'Set to 1 for quick cluster analysis (no attempt to separate
                                              contributions from multiple tracks)',
         FractThresh  = REAL [0.00,1.]     : 'Fractional threshold for constant-fraction threshold used
                                              to find leading and trailing edges of the subpulse',
         MaxSaturate  = INTE [0,*]         : 'Maximum number of saturated samples allowed in a subpulse',
         ZMinimum     = REAL [0.00,220.]   : 'Minimum distance of a cluster from the sector end of the
                                              TPC volume');

  TCCN
      :      'Tpc Chain-finding CoNstants
              used by track finder (STC)\
              Number of parameters    \
              1 '
 
      = (ValRng(2),
         ZTol         = REAL [0.0000,*]        : 'Max chisq for link helix (dz) test',
         Z0tol        = REAL [0.0000,*]        : 'Max value of Z0 for accepting link',
         D0tol        = REAL [0.0000,180.]     : 'Max value of D0 for accepting link',
         RhoMin       = REAL [0.0000,*]        : 'Min radius of curvature for accepting link',
         DWmax        = REAL [0.0000,*]        : 'Max r-phi window for finding hits',
         DZmax        = REAL [0.0000,*]        : 'Max z window for finding hits',
         WS           = REAL [0.0000,*]        : 'Nominal r-phi road width for finding hits',
         ZS           = REAL [0.0000,*]        : 'Nominal z road width for finding hits',
         MaxGap       = INTE [0,19]            : 'Max no of successive padrows with no\
                                                  found hit for local search',
         dZNext       = REAL [0.0000,*]        : 'Max difference in z between successive\
                                                  padrows for accepting hit in link',
         dPhiNext     = REAL [0.0000,*]        : 'Max diff. in phi between successive\
                                                  padrows for accepting hit in link',
         CUrvtol      = REAL [0.0000,*]        : 'Max chisq of curvature diff. between\
                                                  successive links to accept in chain',
         DipTol       = REAL [0.0000,*]        : 'Obsolete',
         MAxtry       = INTE [0,17]            : 'Max number of hits that can be\
                                                  rejected in fitting chain',
         SigmaW       = REAL [0.0000,*]        : 'Obsolete',
         SFactor      = REAL [0.0000,*]        : 'Obsolete',
         SigmaZ       = REAL [0.0000,*]        : 'Obsolete',
         ChimaxC      = REAL  [0.0000,*]       : 'Max value of chisq per degree of\
                                                  freedom for r-phi circle fit',
         ChimaxL      = REAL  [0.0000,*]       : 'Max value of chisq per degree of\
                                                  freedom for s-z line fit',
         ChimaxJ      = REAL  [0.0000,*]       : 'Obsolete',
         ChimaxS      = REAL  [0.0000,*]       : 'Obsolete')
      ;
 
 
 TTRC
      :      'Tpc Tracking Constants (STC)\
              Number of parameters \
              1    '
      = (ValRng(2),
         Ntry  = INTE [1,10]       : 'Number of attempts to find chains',
         Ngap  = INTE [0,21]       : 'Number of pad rows a chain may jump over',
         Nmin  = INTE [3,21]       : 'minimal number of coordinates for chain',
         DwinP = REAL [0.00,0.6]   : 'Window width in phi for triplet search',
         DwinZ = REAL [0.00,100.0] : 'Window width in z for triplet search',
         RoadR = REAL [0.00,10.0]  : 'Road width in (r*phi) for chaining',
         RoadZ = REAL [0.00,10.0]  : 'Road width in z for chaining',
         ChiCh = REAL [0.00,100.0] : 'Max. chi**2/(deg.freedom) for a chain',
         SigUp = REAL [0.00,100.0] : 'Max. num. stand.dev. for up,down links',
         SigTp = REAL [0.00,100.0] : 'Max. num. stand.dev. for spiral links in TPC',
         SigIt = REAL [0.00,100.0] : 'Max. num. stand.dev. for links through ITC',
         LoMax = INTE [0,21]       : 'Max. num. points to be thrown away in fit',
         MinKin= INTE [6,21]       : 'Min. num. points for kink search',
         Dchi2 = REAL [0.00,100.0] : 'Min. delta chi**2 to accept a kink',
         Pmin  = REAL [0.0000,1.]  : 'Minimal fit probability to accept a track')
         ;
 
 
TC1X :      'TPC dE/dx calibration for event reconstruction.
             These constants are used by JULIA to produce the
             POT bank TEXS from TPC raw data (STC)\
             Number of columns \
             Number of rows'
  = (ValRng(2),
     NoRmalization    = REAL [0.00000,*] :'Overall gain normalization,
                                           such that minimum ionization=1.0',
     SectorNorm(2,36) = REAL [0.000,*]   :'Sector to sector relative normalization
                                           There are two values per sector, in case that
                                           it is necessary to divide each sector into
                                           two regions.',
     TruncPoint       = INTE [30,80]     :'Percentage used for truncated mean',
     SLope            = REAL [0.000,*]   :'Slope for correction of dE/dx with
                                           sample length.  dE/dx= (charge)/dx/(1+SLope*loge(dx))',
     AdsorPtion       = REAL [0.000,0.2] :'Not used (see TC5X)');
 

 TCGX :  'TPC dE/dx Global calibration,(NR=run number). 
          Replace old TC2X bank \
          number of words / run range \
          number of run ranges'
       STATIC
      
      = (FirstRun         = INTE            : 'first run of the range',
         LastRun          = INTE            : 'last run of the range',  
         NoRmalization    = REAL [0.000,*]  :'overall dE/dx NoRmalization coefficient'
        );


 TCSX :  'TPC dE/dx Sector relative gain map, these constants
          are to adjust the dE/dx to be uniform over all 36 TPC sectors.(NR=run number).
          Replace old TC2X bank.\
          Number of columns \
          Number of rows'
       STATIC
     
      = (SectorNorm(36) = REAL [0.000,*]  :'Corrections to tune the sector to sector normalization.');
 

 TC2X :  'TPC dE/dx tuned calibration. These constants are to adjust the dE/dx calibration of existing
          POT output. They are used to calculate the calibrated truncated mean and the estimated
          resolution on the truncated mean (as found on the MDST), starting with data from the TEXS bank 
          as produced by JULIA. (DROP)
          Replaced by TCGX and TCSX.\
             Number of columns \
             Number of rows'
  = (ValRng(2),
     NoRmalization  = REAL [0.000,*]  :'Correction to the overall normalization
                                           This is needed in order to fine-tune the
                                           calibration between when the POT is written
                                           and when the MDST is written.',
     SectorNorm(36) = REAL [0.000,*]  :'Corrections to tune the sector to
                                           sector normalization.',
     AdsorPtion     = REAL [0.000,0.1]:'Correction to tune the z dependence',
     ResParms(4)    = REAL [*,*]      :'Parameterization of the dE/dx resolution\
                                           SIGMA= P1*((L/N)**P3 * N**P2)\
                                           L= track length\
                                           N= number of samples',
     SLope          = REAL [*,*]      :'Slope for correction of dE/dx with average  sample length.
                                        dE/dx= (charge)/dx/(1+SLope*loge(dx))');
 

 
TC3X :   'TPC constants for dE/dx analysis.   These constants
          are used to calculate chi**2 values for particle
          type hypotheses, based on the truncated mean and
          error on the truncated mean, as one finds on the MDST.(STC)\
             Number of columns \
             Number of rows'
  = (ValRng(2),
     NoRmalization     = REAL [0.000,*] :'Correction to the overall normalization
                                          This is needed in order to fine-tune the
                                          calibration between when the MDST is written
                                          and the actual analysis is done.  It is used
                                          only when MDST data is read.',
     PaRameters(7)     = REAL [*,*]     :'Parameters for specification of the measured
                                          dependence of the dE/dx on particle velocity.
                                          This is a modified Bethe-Bloch curve.');


TC4X :   'TPC constants for dE/dx analysis.  This bank contains
          the parameterization of the expect resolution and expected
          dE/dx as a function of number of samples, velocity, etc.\
          Number of words per bin\
          Number of bins in log(sample length)'
  = (ValRng(2),
     MiNimum        = REAL [0.000,2.] :'Minimum of range in log(sample
                                        length)',
     MaXimum        = REAL [0.000,2.] :'Maximum of range in log(sample
                                        length)',
     ResPrm(7)      = REAL [*,*]      :'Parameterization of resolution',
     IonPrm(10)     = REAL [*,*]      :'Parameterization of the dE/dx');
 
TC5X :   'TPC calibration for dE/dx reconstruction.  This bank 
          contains detailed corrections to be applied in JULIA.\
           Number of columns\
           Number of rows'
  = (ValRng(2),
     NonlinCor(5)    = REAL [*,*]     :'Constants for corrections
                                        made in TRKELS\
                                        1: drift length correction\
                                        2: gain renormalization\ 
                                        3: nonlinear correction\
                                        4: not used\
                                        5: not used',
     P0(3)           = REAL [*,*]     :'Polynomial for correction
                                        of dE/dx as a function of
                                        wire number in each sector type',
     P1(3)           = REAL [*,*]     :'2nd cofficient of polynomial',  
     P2(3)           = REAL [*,*]     :'3rd cofficient of polynomial',  
     P3(3)           = REAL [*,*]     :'4th cofficient of polynomial');
 
TC6X :   'TPC calibration for dE/dx reconstruction.  This bank
          contains corrections to the sector gain as a function
          of the distance from the sector edge, to be applied
          in JULIA. The row number is the sector type (1->3)\
           Number of columns\
           Sector type'
  = (ValRng(2),
     AmPlitude    = REAL [0.0000,*]      :'Amplitude of the correction\
                                       Correction=1-AMP*EXP(Y/TAU)',
     TaU          = REAL [0.0000,*]      :'Decay rate of the correction');


 TC7X:  'TPC calibration for dE/dx reconstruction.  This bank
         contains constants needed to implement the use of
         below-threshold wire hits. NR = setup code or run number \
         Number of columns\
         Number of rows'
      = (ValRng(2),
         BelowThresh   = INTE [0,1]      : 'Set to 1 to reject wire hits which are below TPD threshold',
         LowerTrunc    = INTE [0,25]     : 'Lower truncation fraction in percent for truncated mean',
         MinAdc        = REAL [0.0,100.] : 'Value to which pulse height of below-threshold hits is set',
         ZAlow         = REAL [0.0,100.] : 'Distance beyond TPC central membrane up to which track
                                            intersections will still be counted',
         MaxZero       = INTE [2,100]    : 'Maximum number of track intersection with zero hits on a wire for
                                            that wire to be considered live',
         MaxRun        = INTE [1,100]    : 'Maximum number of hits in a row below TPD threshold',
         Reserve1      = INTE [0,10]     : 'Max distance from pulse end to track with null hit, in units of 0.25cm',
         Reserve2      = INTE [0,1]      : 'Set to one to cause program to try to split double-peaked pulses',
         Reserve3      = INTE [0,50]     : '10*Minimum ratio of peak to valley pulse heights for split pulses',
         Reserve4      = INTE [0,100]    : '100*fractional threshold in TRDWP3 for pulse begin and end times',
         Reserve5      = INTE [0,10]     : 'Not used',
         Reserve6      = INTE [0,10]     : 'Not used',
         Reserve7      = INTE [0,10]     : 'Not used',
         Reserve8      = INTE [0,10]     : 'Not used'
         );
 

 TWRC:    'Cuts and parameters for TPC wire reduction.(STC)\
           Number of columns\
           Number of rows'
     =(ValRng(2),
       MaxValley    = INTE [1,10    ]     : 'Maximum valley allowed in a good wire pulse.',
       TPack        = REAL [1.0,32.]      : 'Power of two to multiply by before
                                             packing pulse time into a hit
                                             word in TWIR or TWRR.',
       MaxSaturate  = INTE [0,*]          : 'Maximum number of saturated samples
                                             allowed before total pulse height
                                             is set to saturation when packing
                                             pulse height in to TWIR or TWRR.',
       RmsMax       = REAL [0.00,255.]    : 'Maximum RMS width squared of a good wire pulse.',
       AvgLen       = REAL [1.0,255.]     : 'Expected maximum length of a wire pulse
                                             at zero dip angle and zero drift length.',
       SlopeDip     = REAL [0.00,10.]     : 'Slope of increase in pulse length with
                                             dip angle.',
       SlopeZ       = REAL [0.0000,10.]   : 'Slope of increase in pulse length with
                                             drift.');

 
  TWTC:    'Cuts and parameters for TPC dE/dx reconstruction.(STC)\
           Number of columns\
           Number of rows'
     =(ValRng(2),
       FiducialCut  = REAL [0.0,25.]      : 'Required distance from end
                                             of wire for a track match.',
       MaxTurn      = REAL [0.00,6.3]     : 'Maximum turning angle of
                                             track from its origin to
                                             wire crossing point.',
       CtMin        = REAL [0.00,1.]      : 'Minimum cosine of angle of track
                                             with wire in x-y plane.',
       ZWindow      = REAL [0.0,25.]      : 'Window (cm) in z for adding wire
                                             hits to tracks.',
       SPacing      = REAL [0.0,25.]      : 'Minimum distance between two wire
                                             pulses in z (cm).  This must be greater
                                             than ZWindow.',
       MinSamp      = INTE [1,340]        : 'Minimum number of dE/dx samples on
                                             a track for the track to be entered in TEXS.',
       MaxDx        = REAL [0.000,1.]     : 'Maximum change in dx from one sample to the
                                             next allowed for using Taylor expansion for
                                             the logarithm.',
       MaxtrK       = INTE [1,4]          : 'Maximum number of tracks allowed to match a hit
                                             before the hit is rejected completely.',
       ResMax       = REAL [0.00,220.]    : 'Maximum residual between a good hit and a track.',
       ZcutSat      = REAL [0.00,220.]    : 'Minimum z between a hit and any earlier hit
                                             which is saturated.');
 
 TRSC:    'Parameterization of the TPC coordinate
           resolution in terms of RMS cluster width
           and length. (STC)\
           Number of words per interval\
           Number of intervals'
     =(ValRng(2),
       XRp          = REAL [-1.000,*]     : 'For r*phi, the points in X
                                             (RMS width) where parameterization
                                             changes (start of interval).
                                             Set to -1. for a null entry.',
       BRp(3)       = REAL [*,*]          : 'Coefficients of parameterization for
                                             this interval for r*phi.',
       XZ           = REAL [-1.000,*]     : 'Same as XRp, but for z (RMS length)',
       BZ(3)        = REAL [*,*]          : 'Same as BRp, but for z');
 
 T0GL:    'T0 corrections for reconstruction of
           TPC raw data.(STC)\
           Number of columns\
           Number of rows'
     =(ValRng(2),
       GlobalT0     = REAL [-512.00,512.] : 'Constant time offset for
                                             conversion from buckets to
                                             drift length, in microseconds.\
                                             drift_len=(buckets*T-GlobalT0)*v_drift,
                                             where T is the clock period.',
       Alg1         = REAL [-2.0000,2.]   : 'Algorithm dependent constant to
                                             correct the time.  If the algorithm
                                             puts the time in the middle of the
                                             pulse, then Alg1 equals -0.5 times
                                             the pulse length at zero drift, in
                                             microseconds.\
                                             Alg1 is for the leading & trailing edge
                                             with pulse-shape corrections.',
       Alg2         = REAL [-2.0000,2.]   : 'Correction for algorithm using average
                                             of leading and trailing edge times.',
       Alg3         = REAL [-2.0000,2.]   : 'Correction for algorithm using pulse shape
                                             correction but no wires.',
       AlgW         = REAL [-2.0000,2.]   : 'Correction for wire pulse time algorithm
                                             (weighted average of sample times).',
       OFfsettot0   = REAL [12.,16.]      :  'offset to globalT0 for 4 bunches\
                                              T08bunches=globalT0 - offset');
 
 T0RL:    'Calibration of relative T0 from
           one TPC sector to another.(STC)\
           Number of words per sector\
           Number of rows'
     =(ValRng(2),
       TPads        = REAL [-1.0000,1.]   : 'Relative T0 for pads for
                                             each sector, in microseconds.
                                             This is defined as for GlobalT0 in
                                             bank T0GL.',
       TWires       = REAL [-1.0000,1.]   : 'Relative T0 for wires for
                                             each sector, in microseconds.');
 
 TPRF :     'Tpc Pad Response Function as measured from data.
             All rows with the same value of DriftLength should be contiguous, and
             for a given drift length, the BField should increase with
             row number.(STC)\
             Number of columns \
             Number of measurements'
       = (ValRng(2),
          BField          = REAL [0.00,16.]  : 'Magnetic field at which the measurement
                                                was made, in kilogauss',
          DriftLength     = REAL [0.00,220.] : 'Average drift length for the measurement, in cm',
          ResponseWidth   = REAL [0.0000,4.] : 'Square of the measured pad response width, in
                                                units of TPC pad width squared')
      ;

TDXC :      'Constants used for TPC dE/dx monitoring in Julia \
              Number of columns  \
              Number of rows'
 
           STATIC
 
      = (minFiredWires   = INTE        :'Minimum % of fired wires',
         maxabsCosTet    = REAL        :'Maximum Abs(cos(teta))',
         minTpcCoor      = INTE        :'Minimum nb. of TPC coordinates',
         mintrFitPr      = REAL        :'Minimum track fit probability',
         MIntrmom        = REAL        :'Minimum track momentum',
         MaXtrmom        = REAL        :'Maximun track momentum',
         minGoodTr       = INTE        :'Minimum nb. of good tracks',
         LowerInt        = REAL        :'Lower interval dE/dx value',
         UpperInt        = REAL        :'Upper interval dE/dx value',
         maxD0           = REAL        :'Maximum D0 of tracks',
         maxZ0           = REAL        :'Maximum Z0 of tracks',
         minWireTr       = INTE        :'Minimum wires on track');
 
 TPHV :  'TPC dE/dx Global calibration for High Voltage,(NR=run number).\ 
          number of columns \
          number of rows'
       STATIC
      
      = (FirstRun         = INTE        : 'first run of the range',
         LastRun          = INTE        : 'last run of the range',  
         Parameter1       = REAL [*,*]  : 'First Parameter of High Voltage dependence',
         Parameter2       = REAL [*,*]  : 'Second Parameter of High Voltage dependence'
        );

 TCPX :  'TPC dE/dx Global calibration for Pressure,(NR=run number).\ 
          number of columns \
          number of rows'
       STATIC
      
      = (FirstRun         = INTE        : 'first run of the range',
         LastRun          = INTE        : 'last run of the range',  
         Parameter1       = REAL [*,*]  : 'First Parameter of Pressure dependence',
         Parameter2       = REAL [*,*]  : 'Second Parameter of Pressure dependence');

TP1X :      'TPC pad dE/dx calibration for event reconstruction.
             These constants are used by JULIA to produce the
             POT bank TPXS from TPC raw data (STC)\
             Number of columns \
             Number of rows'
             STATIC
  = ( NoRmalization  = REAL [0.00000,*] :'Overall gain normalization,
                                          such that minimum ionization=1.0',
      SectorNorm(36) = REAL [0.000,*]   :'Sector to sector relative normalization.',
      TruncPoint     = INTE [30,80]     :'Percentage used for truncated mean',
      MinSamples     = INTE [0,21]      :'Minimum number of samples required');
 
TP3X :   'TPC constants for pad dE/dx analysis. These constants
          are used to calculate chi**2 values for particle
          type hypotheses, based on the truncated mean and
          error on the truncated mean, as one finds on the MDST.(STC)\
             Number of columns \
             Number of rows'
             STATIC

     = ( NoRmalization  = REAL [0.000,*] :'Correction to the overall normalization
                                           This is needed in order to fine-tune the
                                           calibration between when the MDST is written
                                           and the actual analysis is done.  It is used
                                           only when MDST data is read.');
 
TP4X :   'TPC constants for pad dE/dx analysis.  This bank contains
          the parameterization of the expect resolution and expected
          dE/dx as a function of number of samples, velocity, etc.\
          Number of words per bin\
          Number of bins in log(sample length)'
          STATIC
     = ( MiNimum    = REAL [0.000,2.] :'Minimum of range in log(sample length)',
         MaXimum    = REAL [0.000,2.] :'Maximum of range in log(sample length)',
         ResPrm(7)  = REAL [*,*]      :'Parameterization of resolution',
         IonPrm(10) = REAL [*,*]      :'Parameterization of the dE/dx');
 
 TLCE
     :    'Tpc coordinate error parameterization
           based on pre-fitted track parameters, for
           coordinates without (row 1) and with (row 2)
           corrections for Landau fluctuation  (STC)\
           Number of columns \
           Number of rows'
           STATIC
       = (U0 = REAL [0.0000,*]      : 'Contribution to R-Phi Coordinate error for
                                       radial tracks',
          UR = REAL [0.0000,*]      : 'Additional contribution to R-Phi Coordinate
                                       error for non-radial tracks with pca>0',
          UL = REAL [0.0000,*]      : 'Additional contribution to R-Phi Coordinate
                                       error for non-radial tracks with pca<0',
          U1 = REAL [0.0000,*]      : 'Contribution to R-Phi Coordinate error due to
                                       drift length constant term',
          U2 = REAL [*,*]           : 'Contribution to R-Phi Coordinate error due to drift
                                       length linear term',
          U3 = REAL [0.000000000,*] : 'Contribution to R-Phi Coordinate error
                                       due to drift length quadratic term',
          Z0 = REAL [0.0000,*]      : 'Contribution to Z Coordinate error for
                                       perpendicular tracks',
          Z1 = REAL [0.0000,*]      : 'Contribution to Z Coordinate error
                                       due to track dip angle linear term',
          Z2 = REAL [0.0000,*]      : 'Contribution to Z Coordinate error
                                       due to track dip angle quadratic term',
          E1 = REAL [0.0000,*]      : 'Empirical factor multiplying R-Phi error for coordinates
                                       containing one half pad',
          E2 = REAL [0.0000,*]      : 'Empirical factor multiplying R-Phi error for coordinates
                                       containing two half pads')
       ;

 TWCO
     :    'Tpc Wire COordinate and landau correction
           parameters (STC)\
           Number of columns \
           Number of rows'
           STATIC
        = (FLat     =   REAL [0.,*]  :  'Flat region of the pad response, in
                                         units of pad height',
           SiGma    =   REAL [0.,*]  :  'Width of the Gaussian fall-off of
                                         pad response outside the flat region
                                         in units of pad height',
           UFlag    =   INTE [0,1]   :  'Flag to enable correction of R-Phi coordinates
                                         for Landau fluctuations (1=do correction)',
           ZFlag    =   INTE [0,1]   :  'Flag to enable correction of Z coordinates
                                         for Landau fluctuations (1=do correction)',
           Error0   =   REAL [*,*]   :  'Wire Z coordinate error, constant term',
           Error1   =   REAL [*,*]   :  'Wire Z coordinate error, drift dependent term',
           Corr0    =   REAL [*,*]   :  'Wire Z coordinate correction, constant term',
           Corr1    =   REAL [*,*]   :  'Wire Z coordinate correction, linear term',
           Corr2    =   REAL [*,*]   :  'Wire Z coordinate correction, quadratic term',
           Corr3    =   REAL [*,*]   :  'Wire Z coordinate correction, cubic term')
       ;
 
 TSHP:    'Pulse-shape correction coefficients for TPC z
           reconstruction with pads. (STC)\
           Number of columns\
           Number of rows'
        STATIC
     =(COnstCoeffs(6) = REAL [*,*] : 'Coefficients for Gaussian plus quadratic
                                       function of abs(tan lambda) for moment-independent
                                       term of correction',
       SLopeCoeffs(6) = REAL [*,*] : 'Coefficients for Gaussian plus quadratic
                                       function of abs(tan lambda) for correction
                                       term proportional to first moment of pulse.',
       LandauCoeffs(4) = REAL [*,*] : 'Coefficients for Landau correction.');

 TNRN
     :    'TPC correction constants for one half-pad clusters\
           Number of columns \
           Number of rows'
           STATIC
 
        = (
          H0 = REAL [*,*] : 'Empirical shift of coordinates with one half pad
                             constant term',
          H1 = REAL [*,*] : 'Empirical shift of coordinates with one half pad
                             linear term',
          H2 = REAL [*,*] : 'Empirical shift of coordinates with one half pad
                             quadratic term',
          HL = REAL [*,*] : 'Empirical shift of coordinates with one half pad
                             low cutoff',
          HH = REAL [*,*] : 'Empirical shift of coordinates with one half pad
                             high cutoff')
       ;

 TPSM :  'TPC hits smearing matrix,(NR=setup code)\
          number of words per view\
          number of views (=2)'
       STATIC
      
      = (Sigma(231)       = REAL            : 'smearing matrix 21x21, in \
                                               triangular form'
        );

TP5X :   'TPC constants for pad dE/dx analysis. These constants
          are used to calculate chi**2 values for particle
          type hypotheses, based on the truncated mean and
          error on the truncated mean, as one finds on the MDST.
          Only available for LEP1 (STC)\
             Number of columns \
             Number of rows'
             STATIC

     = ( NoRmalization  = REAL [0.000,*] :'Correction to the overall normalization
                                           This is needed in order to fine-tune the
                                           calibration between when the MDST is written
                                           and the actual analysis is done.  It is used
                                           only when MDST data is read.')
     ;
 
TP6X :   'TPC constants for pad dE/dx analysis.  This bank contains
          the parameterization of the expect resolution and expected
          dE/dx as a function of number of samples, velocity, etc...
          Only available for LEP1 (STC)\
          Number of words per bin\
          Number of bins in log(sample length)'
          STATIC
     = ( MiNimum    = REAL [0.000,2.] :'Minimum of range in log(sample length)',
         MaXimum    = REAL [0.000,2.] :'Maximum of range in log(sample length)',
         ResPrm(7)  = REAL [*,*]      :'Parameterization of resolution',
         IonPrm(10) = REAL [*,*]      :'Parameterization of the dE/dx')
     ;

 TPAZ
  :      'Change in sector alignment in z due to
          TPD timing offsets.
          For each sector (36) there are up to 
          4 parameters.'   
        STATIC
 
        SIZE 36,36
 
     =  (SlonmB        = INTE [1,36]    : 'Slotnumber in Reconstruction',
         RotPer        = Length [*,*]    : 'Rotation around axis perpendicular to sector',
         RotX          = Length [*,*]    : 'Rotation around sector x axis',
         RotY          = Length [*,*]    : 'Rotation around sector y axis',
         RTrans        = Length [*,*]    : 'Translation in radial direction of sector centre')
     ;

 TPRZ
  :      'TPD timing offsets : 
          For each sector (36) there are up to 
          21 (= number of TPD) entries.'   
        STATIC
 
        SIZE 36,36
 
     =  (SlonmB        = INTE [1,36]    : 'Slotnumber in Reconstruction',
         ZOffset(21)   = Length [*,*]   : 'z-offset for each TPD (1-21) in 
                                           this sector') 
     ;

 TPWZ
  :      'TPD timing offsets for wire TPDs : 
          For each sector (36) there are up to 
          3 (= number of TPD) entries.'   
        STATIC
 
        SIZE 36,36
 
     =  (SlonmB        = INTE [1,36]    : 'Slotnumber in Reconstruction',
         ZOffset(3)   = Length [*,*]    : 'z-offset for each wire TPD (1-3)
                                           in this sector') 
     ;


 END ESET

 END SUBSCHEMA

 
 SUBSCHEMA KinkBanks
 : 'Description of Kink banks'
 
 AUTHOR   'P. Rensing'
 REVIEWER 'to be selected'
 VERSION  '1.0'
 DATE     '96/02/15'

 DEFINE ESET
 
 YKSP
      :      'Kink Species List \
              Number of columns\
              Number of rows'
           STATIC
 
      = (ParentMass   = REAL : 'Parent mass',
         DaughterMass = REAL : 'charged daughter mass',
         NeutralMass  = REAL : 'neutral daughter mass',
         MassCut      = REAL : 'mass window for physics output',
         TagCut       = REAL : 'mass window for VDET kink tagging')
       ;  

 YKCI
      :      'Kink Package Cut Initialization \
              Number of columns\
              Number of rows (=1)'
           STATIC
 
      = (FidInnerRadius = REAL : 'Inner fiducial radius',
         FidOuterRadius = REAL : 'Outer fiducial radius',
         FidZ           = REAL : 'Maximum abs(z) of fiducial volume',
         MissXY         = REAL : 'Cut on miss dist. in xy plane',
         MissZ          = REAL : 'Cut on miss dist. along z',
         NumberBadHits  = INTE : 'Cut on number of bad hits',
         ChargeCut      = REAL : 'Early cut on tracks having same charge',
         TrackChi2      = REAL : 'Track chi**2 cut (per dof)',
         VertexChi2     = REAL : 'Vertex chi**2 cut',
         VDetChi2       = REAL : 'VDet hit chi**2 cut',
         Vdet2Chi2      = REAL : 'Cut on VDet hit 2nd chi**2 - best chi**2')
       ;  
  
 END ESET
 
 END SUBSCHEMA
                
 
 SUBSCHEMA TpcLaserCal
 :'Contains the measurements of the laser deflection angles in
   TPC sides A and B (error of 0.01 degree) and the results of
   the laser calibration runs'
 
 AUTHOR   'E.Lancon,F.Ranjard,D.Casper'
 REVIEWER 'W.Wiedenmann'
 VERSION  '2.1'
 DATE     '05/07/2000'
 
 
DEFINE ESET
 
 TLAA
      :      'Tpc Laser Azimuthal deflection Angles [degrees]
              1st row:  for side A,
              2nd row:  for side B. (LTC)\
              Number of angles/side\
              Number of rows (=2)'
 
        SIZE 2,2
        STATIC
 
 
     =  (Azi1     = REAL  [320.0,340.0]   : 'nominal 330 degree ray)',
         Azi2     = REAL  [80.0,100.0]    : 'nominal  90 degree ray)',
         Azi3     = REAL  [200.0,220.0]   : 'nominal 210 degree ray)')
      ;
 
 
 TLPA
      :      'Tpc Laser Polar deflection Angles [degrees]
              Precision of 0.01 degree.
              Five angles per phi-plane (i.e. per row)
              1st row:  5 polar angles for phi 324 degrees in side A,
              2nd row:  5 polar angles for phi  84 degrees in side A,
              3rd row:  5 polar angles for phi 204 degrees in side A,
              4th row:  5 polar angles for phi 336 degrees in side B,
              5th row:  5 polar angles for phi  96 degrees in side B,
              6th row:  5 polar angles for phi 216 degrees in side B. (LTC)\
              Number of angles\
              Number of rows (=6)'
 
        SIZE 6,6
        STATIC
 
 
     =  (Pol1   = REAL  [16.00,20.00]     : 'nominal 18 degree ray',
         Pol2   = REAL  [28.00,32.00]     : 'nominal 30 degree ray',
         Pol3   = REAL  [37.00,42.00]     : 'nominal 39 degree ray',
         Pol4   = REAL  [66.00,70.00]     : 'nominal 67 degree ray',
         Pol5   = REAL  [88.00,92.00]     : 'nominal 90 degree ray')
      ;
 
 
 
 TDVV
      :      'mean Tpc Drift Velocity Vector [cm/musec],
              row number = 1 : for side A with pads,
                         = 2 : for side A with wires,
                         = 3 : for side A with a combined method,
              row number = 4 : for side B with pads,
                         = 5 : for side B with wires,
                         = 6 : for side B with a combined method (DROP)'
 
        SIZE 1,6
 
 
     =  (ValRng(2)   = Date  [1,999999]       : 'Validity Range',
         VelXcomp    = REAL  [-0.1000,0.1000] : 'Drift velocity component in X dir.',
         VelYcomp    = REAL  [-0.1000,0.1000] : 'Drift velocity component in Y dir.',
         VelZcomp    = REAL  [0.0000,10.0000] : 'Drift velocity component in Z dir.',
         ErrXcomp    = REAL  [0.0000,0.2000]  : 'Error for X-component',
         ErrYcomp    = REAL  [0.0000,0.2000]  : 'Error for Y-component',
         ErrZcomp    = REAL  [0.0000,0.2000]  : 'Error for Z-component',
         JulVer      = REAL  [0.00,999.00]    : 'Julia Version number',
         CHisquared  = REAL  [0.,9999.00]     : 'CHisquared for the fit',
         NumFreed    = INTE  [0,999]          : 'Number of degrees of freedom',
         PRessure    = REAL  [0.0,1200.0]     : 'gas PRessure [hPa]',
         TemPerature = REAL  [0.0,50.0]       : 'gas TemPerature [C]')
      ;
 
 TDFV
      :     'TPC final drift velocity, NR=run 
             Used for pre-1997 alignment\
             number of words / run range \
             number of run ranges'
         STATIC
      
      = (FirstRun              = INTE     : 'first run of the range',
         LastRun               = INTE     : 'last run of the range',  
         driftVelsideA(3)      = REAL     : 'vx,vy,vz side A drift velocity [cm/musec]',
         driftVelsideB(3)      = REAL     : 'vx,vy,vz side B drift velocity [cm/musec]')
      ;

 TVXY
      :     'TPC vx,vy drift velocity, NR=run \
             number of words / side \
             number of sides , if 1 side it is valid for side A and side B
             with the opposite sign'
         STATIC

      = (DriftX                = REAL     : 'vx drift velocity [cm/musec]',
         DriftY                = REAL     : 'vy drift velocity [cm/musec]',
         ErrorX                = REAL     : 'error on vx',
         ErrorY                = REAL     : 'error on vy' )
      ;   
 
 
 TCUC
      :      'Tpc CUrvature-correction Coefficients,
              1st row side A, 2nd row side B (STC)'
 
        SIZE 1,2
 
 
     =  (ValRng(2)   = Date  [1,999999]        : 'Validity Range',
         Coef0       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=0, m= 0',
         Coef1       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=1, m=-1',
         Coef2       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=1, m= 0',
         Coef3       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=1, m= 1',
         Coef4       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=2, m=-1',
         Coef5       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=2, m= 0',
         Coef6       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=2, m= 1',
         Coef7       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=3, m=-1',
         Coef8       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=3, m= 0',
         Coef9       = REAL  [-2.E-4,2.00E-4]  : 'Coefficient for l=3, m= 1',
         Ecoef0      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=0, m= 0',
         Ecoef1      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=1, m=-1',
         Ecoef2      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=1, m= 0',
         Ecoef3      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=1, m= 1',
         Ecoef4      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=2, m=-1',
         Ecoef5      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=2, m= 0',
         Ecoef6      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=2, m= 1',
         Ecoef7      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=3, m=-1',
         Ecoef8      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=3, m= 0',
         Ecoef9      = REAL  [-2.E-4,2.00E-4]  : 'Error for coeff. l=3, m= 1',
         CHisquared  = REAL  [0.,9909.00]      : 'CHisquared for the fit',
         NumFreed    = INTE  [0,999]           : 'Number of degrees of freedom')
      ;
 
 TNFV
      :     'TPC new, final drift velocity, NR=run.
             Used for 1997-style calibration.\
             number of words / run range \
             number of run ranges'
         STATIC

      = (FirstRun              = INTE     : 'first run of the range',
         LastRun               = INTE     : 'last run of the range',
         DeltaVel              = REAL     : 'drift velocity scale factor',
         DeltaT0               = REAL     : 'T0 offset from T0GL (buckets)',
         DvelWire              = REAL     : 'drift velocity change due to wire z coreections',      
         SPare(3)              = REAL     : 'Spare locations')
      ;
 
 
END ESET
 
 
 
 DEFINE RSET
 
 
 END RSET
 
 
 END SUBSCHEMA

 
 SUBSCHEMA VdetOffConsts
 :'Banks for VDET hit reconstruction and track refitting'
 
 
 AUTHOR   'A.Bonissent,G.Sguazzoni'
 REVIEWER 'A.Bonissent'
 VERSION  '8.1'
 DATE     '06/10/2000'

 
 DEFINE ESET
 

 VTRP   : 'VDET track link and refit parameter bank NR=setup code (LTC)\
           number of parameters\                                                
           number of rows (=1)'                                                 
           STATIC    
                                                           
           SIZE 1,1                                                             
                                                                                
         = (MaxClust   = INTE [0,10000]        :'Maximum number of clusters considered for linking per layer',
            MaxLink    = INTE [0,10000]        :'Maximum number of links considered between layers',      
            FrftNum    = INTE [0,100000]       :'Bank number of new FRFT and FRTL banks created',
            SigmaCut   = REAL [0.,100.]        :'Number of sigma defining the search area',
            ChiCut     = REAL [0.,1000.]       :'Maximum chisquared for a preliminary link',
            ChiIncr    = REAL [0.,100.]        :'Chisquared reduction per layer for tracks without a hit on 
                                                 a specific layer',               
            MinPulse   = REAL [-10.0,100000.]  :'Minimum pulse size to share a hit',     
            BigError   = REAL [0.,100000.]     :'Error attributed to "missing" hit',
            USignoise  = REAL [0.,1000.]       :'Signal/noise on U side',
            WSignoise  = REAL [0.,1000.]       :'Signal/noise on W side',
            UPeak      = REAL [0.,10000.]      :'Most probable cluster\
                                                 pulseheight (angle corrected) U view',
            WPeak      = REAL [0.,10000.]      :'Most probable cluster Ph W view')
            ;                                                                   
 
 VRCN
        : 'VDET hit reconstruction parameter bank NR=setup code (JUL)\
           number of words/region\
           number of regions'
           STATIC

           SIZE 1,1
 
         = (MakevdXy   = INTE [-1,0]         :'Make hit positions (flag)',
            MakevFnl   = INTE [-1,0]         :'Make final pulseheights (flag)',
            MakevHot   = INTE [-1,0]         :'Make hot channel list (flag)',
            MinNchan   = INTE [0,100]        :'Min # channels for CM',
            PUlsemin   = INTE [0,10000]      :'Min single strip pulseheight',
            ClustMin   = INTE [0,10000]      :'Min sum cluster pulseheight',
            MaxUnbnd   = INTE [0,10]         :'Max # of unbonded in cluster',
            MaxSigma   = INTE [0,10000]      :'Max single strip sigma',
            FinalSum   = INTE [0,10000]      :'Min final cluster PH sum',
            HicutCm    = REAL [0.,10.]       :'High side CM sigma cut',
            LocutCm    = REAL [0.,10.]       :'Low side CM sigma cut',
            MaxZocc    = REAL [0.,1.]        :'Max Z strip occupancy',
            MaxPocc    = REAL [0.,1.]        :'Max Phi strip occupancy')
            ;

 VHOT
        : 'VDET hot strip list bank NR=Run# (JUL)\
           number of words/hot strip (=1)\
           number of hot strips'
           STATIC

           SIZE 1,*
 
         = (HotAddr    = INTE [0,*]         :'Hot strip address+occupancy')
            ;

 VALC
      :      'Local alignement constants for a single wafer NR=Run# (STC)
              Used for pre-1997 alignment \
              Number of Columns\
              Number of rows'
        STATIC

        SIZE 1,108
 
      = (WaferIdent   = INTE   [0,*]      : 'Wafer identifier',
         TRanslat(3)  = Length [-1.,1.]   : 'Local  transl. vector',
         ROtation(3)  = Angle  [-.1,.1]   : 'Local  rotat. angles',
         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance matrix')
      ;
 
 VNLC
      :      'Local alignement constants for a single wafer NR=Run# (STC)
              Used for 1997-style alignment \
              Number of Columns\
              Number of rows'
        STATIC

        SIZE 1,108
 
      = (WaferIdent   = INTE   [0,*]      : 'Wafer identifier',
         TRanslat(3)  = Length [-1.,1.]   : 'Local  transl. vector',
         ROtation(3)  = Angle  [-.1,.1]   : 'Local  rotat. angles',
         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance matrix')
      ;
 
 VAGB
      :      'Global alignement constants for the entire VDET NR=Run# (STC)
              Used for pre-1997 alignment \
              Number of Columns\
              Number of rows'
        STATIC

        SIZE 1,1
 
 
      = (TRanslat(3)  = Length [-1.,1.]   : 'Global transl. vector',
         ROtation(3)  = Angle  [-6.0,6.0]   : 'Global rotat. angles',
         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance matrix')
      ;
 
 VLGB
      :      'Global alignement corrections from Laser spots VDET. NR=0 (LTC)\
              Number of Columns\
              Number of Periods'
        STATIC

        SIZE 1,100
  
      = (FirstRun            = INTE [0,99999] : 'First run in period',
         LastRun             = INTE [0,99999] : 'Last run in period',
         InitialValue(6)     = REAL [*,*] : 'Value of the parameter for the first run in period',
         LinearCoeff(6)      = REAL [*,*] : 'Coefficient for the linear term',
         QuadCoeff(6)        = REAL [*,*] : 'Coefficient for the quadratic term')
      ; 
 VNGB
      :      'Global alignement constants for the entire VDET NR=Run# (STC)
              Used for 1997-style alignment \
              Number of Columns\
              Number of rows'
        STATIC

        SIZE 1,1
 
 
      = (TRanslat(3)  = Length [-1.,1.]   : 'Global transl. vector',
         ROtation(3)  = Angle  [-6.0,6.0]   : 'Global rotat. angles',
         EcovarM(21)  = REAL   [*,*]      : 'Triangular covariance matrix')
      ;

 VTCE
      :  'VDET cluster parameter banks, NR=setup code (LTC)\
              Number of parameters\
              number of rows'
           STATIC
           
           SIZE 1,1                                                    

       = (ErrorLarge = REAL [0.0,99.0] : 'Error on large PH hit',
          ErrorSplit = REAL [0.0,99.0] : 'Error on split hit',
          sigInnerU  = REAL [0.0,99.0] : 'Inner layer overlap U sigma',
          sigOuterU  = REAL [0.0,99.0] : 'Outer layer overlap U sigma',
          sigLinW    = REAL [0.0,99.0] : 'Linear term overlap W sigma',
          sigOffW    = REAL [0.0,99.0] : 'Offset term overlap W sigma',
          UNoise     = REAL [0.0,99.0] : 'U strip noise in offline 
                                          units = 4*gain corrected counts',
          WNoise     = REAL [0.0,99.0] : 'W strip noise in offline 
                                          units = 4*gain corrected counts',
          MultScat   = REAL [0.0,99.0] : 'Multiple scattering constant for
                                            1 VDET layer',
          AmbChi     = REAL [0.0,1000000.0] : 'Chisquared cut for ambiguous
                                            track association',
          DecFac     = REAL [0.0,99.0] : 'Line driver decay factor',
          NomPh      = REAL [0.0,1000000.0] : 'Nominal cluster PH',
          RealHit    = REAL [0.0,1000000.0] : 'Real Hit size')
      ;


 VTPA
      :  'VDET Track extrapolation parameters NR=setup code (LTC)\
              Number of parameters\
              number of rows'
           STATIC
           
           SIZE 1,1                                                    

       = (MinItchits = INTE [0,100] : 'Minimum # of ITC hits to set flag bit',
          MinTpchits = INTE [0,100] : 'Minimum # of TPC hits to set flag bit',
          MaxChisqur = REAL [0.0,99.0] : 'Maximum chisq/DOF to set flag bit',
          numUSigma  = REAL [0.0,99.0] : '# sigma from U edge for extrap+flag',
          numWSigma  = REAL [0.0,99.0] : '# sigma from W edge for extrap+flag')
         
      ;

 VMRE
        : 'VDET region description bank NR=setup code (JUL)\
           number of words/region\
           number of regions'
           STATIC

           SIZE 1,*                                                    

         = (FirstCam   = INTE [0,*]          :'Address of first CAMEX channel',
            LastCam    = INTE [0,*]          :'Address of last CAMEX channel',
            CamexStrip = INTE [1,512]        :'Strip of first CAMEX channel',
            RegionFlag = INTE [0,*]          :'Region flag')
            ;

 VMPC
        : 'VDET peculiar strip description bank NR=setup code (JUL)\
           number of words/strip\
           number of strips'
           STATIC

           SIZE 1,*                                                    
 
         = (CamAdd     = INTE [0,*]          :'Address of CAMEX channel',
            StripFlag  = INTE [0,*]          :'Strip flag')
            ;

 VMGN
        : 'VDET readout module gain bank NR=Run# (JUL)\
           number of words/readout module(wafer)\
           number of readout modules'
           STATIC

           SIZE 1,108                                                    
 
         =( WafAdd        = INTE [0,*]    :'Wafer address',
            FirstChannel  = INTE [0,1000] :'first Readout channel of the gain region\
                                            =0 means first readout channel in module',
            LastChannel   = INTE [0,1000] :'last Readout channel of the gain region\
                                            =9999 means last possible readout channel in module',
            Modtype2   = INTE [0,10]      :'Unused',
            WafGain    = REAL [0.,10.]    :'Relative gain of the region')
            ;

 VDEM
      :      ' VDET efficiency map for MC, NR=setup code (LTC)\
              Number of words/readout line\
              Number of periods'
           STATIC

           SIZE 1,*                                                    

      = ( PRoportion      = INTE [0,100]       : ' Proportion of total 
                                                   time /period ',
          FirstRun        = INTE [0,99999]     : ' First run of period
                                                   NB approximate ',
          LastRun         = INTE [0,99999]     : ' Last run of period
                                                   NB approximate ',
          EFficiency(288) = REAL [0.00,100.00] : 'Eff of detector in 
                                                  region of line')
          ;
 
 ALCB
      :      '(DROP) Aleph Beam position algorithm Constants.(NR=setup code)
              Obsolete bank--see VBPC \
              number of columns \
              number of rows (=1)'
              STATIC
 
  =  (MinptItc     = INTE  : 'Minimum number of points in Itc',
      MinptVdet    = INTE  : 'Minimum number of points in Vdet',
      MinptTpc     = INTE  : 'Minimum number of points in Tpc',
      MintrMoment  = REAL  : 'Minimum track Momentum',
      maxtrD0      = REAL  : 'Maximum track D0',
      maxtrZ0      = REAL  : 'Maximum track Z0',
      maxtrChi2    = REAL  : 'Maximum track Chi2',
      KludgefX     = REAL  : 'Kludge factor on X error',
      KludgefY     = REAL  : 'Kludge factor on Y error',
      KludgefZ     = REAL  : 'Kludge factor on Z error',
      NtrBunch     = INTE  : 'Number of tracks per Bunch',
      minNbtrUpd   = INTE  : 'Minimum Number of tracks in bunch to Update',
      NLoops       = INTE  : 'Number of Loops',
      SigmaCuts(4) = REAL  : 'Sigma Cuts for loop I')
      ;
 
 ALLR   : 'Luminous Region SiZe  NR=Year Flag (1 for 1991, 2 for 1992, ...)
           (DROP) Obsolete bank--see ALRP\
           number of parameters\
           number of rows (=1)'
           STATIC
 
           SIZE 1,1
 
         = (YEar   = INTE [0,*]   :'Year of the run period',
            XSigma = REAL [*,*]   :'Average RMS size in the X direction',
            YSigma = REAL [*,*]   :'Average RMS size in the Y direction',
            ZSigma = REAL [*,*]   :'Average RMS size in the Z direction',
            XRes   = REAL [*,*]   :'Position resolution in the X direction',
            YRes   = REAL [*,*]   :'Position resolution in the Y direction',
            ZRes   = REAL [*,*]   :'Position resolution in the Z direction',
            RelAmp = REAL [*,*]   :'Relative amplitude of gaussians
                                    (first/second);
                                    The X size distribution is modeled
                                    as the sum of 2 gaussians',
            Mean1  = REAL [*,*]   :'Mean of first gaussian',
            Sigma1 = REAL [*,*]   :'Sigma of first gaussian',
            Mean2  = REAL [*,*]   :'Mean of second gaussian',
            Sigma2 = REAL [*,*]   :'Sigma of second gaussian')
            ;

 VDSM   : 'VDET point resolution MC smearing  NR = setup code\
           number of parameters\
           number of rows (=1)'
           STATIC

           SIZE 1,1

         = (USmear = REAL [*,*]   :'Sigma for additional U smearing',
            WSmear = REAL [*,*]   :'Sigma for additional W smearing')
            ;

 VHPP
        :  'Hit position parameters\
            Number of parameters\
            Number of rows (=1)'
           STATIC

        = (SeparationSigma    = REAL [0,*] : 'minimum pulseheight difference
                                              in one strip to split one cluster
                                              into 2 (in sigmas of noise)',
           PositionSigma      = REAL [0,*] : 'min. pulseheight for a strip to
                                              be taken into account for 
                                              clustering (in noise sigmas)',
           WError             = REAL [0,*] : 'first guess for hit position 
                                              resolution w coord.',
           UError             = REAL [0,*] : 'same for u coord.')
        ;

 VSWP
      :      'VDET swapped readout modules\
              Number of words/swap\
              Number of swaps'
           STATIC

      = (FirstRun    = INTE [0,*] : 'beginning of validity range',
         LastRun     = INTE [0,*] : 'end of validity range',
         ChannelAddr = INTE [1,*] : 'Encoded module address,\
                                     as seen by on-line',
         RealAddr    = INTE [1,*] : 'Real module address')
      ;

 ALRP   
      :   'Luminous Region Parameters (NR=0)\
           number of columns\
           number of rows = number of BE setups in ADBR'
           STATIC
 
   = (PeriodName  = INTE [0,*]  : 'Run period/year name',
      WEight      = INTE [0,*]  : 'Weight of this run period;
                                    for LEP1, equals the number of qqbar
                                    events in MAYB/PERF VD runs;
                                    for LEP2, equals zero.',
      ERa         = INTE [0,*]  : 'LEP era; equals 1 for LEP1, 2 otherwise',
      XSigma      = REAL        : 'Average rms size in the x direction',
      YSigma      = REAL        : 'Average rms size in the y direction',
      ZSigma      = REAL        : 'Average rms size in the z direction',
      XRes        = REAL        : 'Position resolution in the x direction',
      YRes        = REAL        : 'Position resolution in the y direction',
      Frac1       = REAL        : 'Fraction of total area in first Gaussian;
                                    the sigma_x distribution is modeled
                                    as the sum of two Gaussians',
      Mean1       = REAL        : 'Mean of first Gaussian',
      Sigma1      = REAL        : 'Sigma of first Gaussian',
      Mean2       = REAL        : 'Mean of second Gaussian',
      Sigma2      = REAL        : 'Sigma of second Gaussian')
      ;
 
 VBWP
      :   'Metachunk luminous region sigma_x resolution (NR=0)\
           number of columns\
           number of rows = number of LEP eras'
           STATIC
 
   = (C1          = REAL        : 'Resolution parameter C1;
                                    the uncertainty**2 on sigma_x is
                                    C1**2 * (sigma_x**2 + C2**2) / NEVE',
      C2          = REAL        : 'Resolution parameter C2 (cm)',
      BIas        = REAL        : 'Offset (cm) to be subtracted from
                                    measured sigma_x values in WIDE')
      ;

 VBPC
      :    'Constants for VBSPOT beam position determination (NR=0) \
            number of columns \
            number of LEP energy regimes'
            STATIC
 
  =  (TauRej       = INTE  : 'tautau rejection
                               (=1 if rejection is to be applied,
                                =0 if not applied)',
      VdetHit      = INTE  : 'VDET hit definition 
                               (=1 to require matched r-phi and z hits
                                   in the same layer,
                                =0 to require r-phi hit)',
      VdetMinhits  = INTE  : 'Minimum number of VDET hits as defined above',
      ItcMinhits   = INTE  : 'Minimum number of ITC hits',
      TpcMinhits   = INTE  : 'Minimum number of TPC hits',
      PMin         = REAL  : 'Minimum track momentum (GeV/c)',
      maxtrD0      = REAL  : 'Maximum track abs(d0) (cm)',
      maxtrZ0      = REAL  : 'Maximum track abs(z0) (cm)',
      maxtrChi2    = REAL  : 'Maximum track Chi2/dof',
      KludgefX     = REAL  : 'Kludge factor on x uncertainty',
      KludgefY     = REAL  : 'Kludge factor on y uncertainty',
      NtrChunk     = INTE  : 'Minimum number of tracks per chunk',
      minNctrUpd   = INTE  : 'Minimum number of tracks in chunk to update',
      SigmaMin     = REAL  : 'Minimum d0 uncertainty (cm) for deweighting
                               tracks in first iteration in VBMFIT',
      NIter        = INTE  : 'Number of iterations in VBMFIT fit',
      SigmaCuts(4) = REAL  : 'Sigma Cuts for VBMFIT iterations')
      ;

 VCAB
      :      'VDET swapped readout modules/cabling errors\
              Number of words/swap\
              Number of swaps'
           STATIC

      = (ApparentLayer = INTE [1,2] : 'Layer from encoded VHLS
                                       cluster address',
         ApparentFace  = INTE [1,15]: 'Face  from encoded VHLS
                                       cluster address',
         ApparentWafer = INTE [1,6] : 'Wafer from encoded VHLS
                                       cluster address',
         ApparentView  = INTE [1,2] : 'View  from encoded VHLS
                                       cluster address',
         TrueLayer     = INTE [1,2] : 'True Layer on which cluster
                                       was detected',   
         TrueFace      = INTE [1,15]: 'True Face on which cluster 
                                       was detected',   
         TrueWafer     = INTE [1,6] : 'True Wafer on which cluster 
                                       was detected',   
         TrueView      = INTE [1,2] : 'True View  on which cluster 
                                       was detected')   

      ;

 VGLB
        : 'Initialization bank for VGLBCM (JUL)\
           number of words/row\
           number of cols'
           STATIC
           SIZE 1,1

         = (NSigmaRoad  = REAL : 'Number of sigmas for search road',
            MinError    = REAL : 'Minimum hit error',
            PhNominal   = REAL : 'Nominal pulse height',
            PhThreshold = REAL : 'Double pulse threshold',
            Phdiff2View = REAL : 'min. pulse height between double 
                                      and other view',
            NullPen     = REAL : 'Penalty for missing hit',
            OnedPen     = REAL : 'Penalty for hit with no hit in other view',
            MaxchiK     = REAL : 'Max. delta chi2 to keep for a single track',
            maxBrutSz   = INTE : 'max. size of comp. to solve by brute force',
            MaxchiF     = REAL : 'max delta chi2/track to keep final soln.',
            UseItc      = INTE : '=1 means accept existing ITC hits as is',
            FootCut     = REAL : 'cut threshold on large footprint tracks',
            PcktrK      = INTE : '=1 means pick input tracks',
            PckSln      = INTE : '=1 means pick best final soln. from list')
            ;

 VHPV   : 'Run periods when the HV bit was unreliable\
           Number of columns\nb of periods'
           STATIC
           SIZE 1,*
         = (FirstRun   = INTE [0,9999999]    :'First run of unreliable period',
            LastRun    = INTE [0,9999999]    :'Last run of unreliable period')
         ;


 VHIV   : 'Run and event number information for bad HV bit information\
           Number of columns\nb of runs'
           STATIC
           SIZE 1,*
         = (RunNumber   = INTE [0,9999999]   :'Run number with unreliable bit',
            FirstE      = INTE [0,9999999]   :'First unreliable event',
            LastE       = INTE [0,9999999]   :'Last unreliable event')
         ;

 VHBV   : 'Bit number to use in different run ranges for VDET HV bit\
           Number of columns\nb of HV bit ranges'
           STATIC
           SIZE 1,*
         = (BitNumber   = INTE [0,31]        :'HV bit number to use',
            FirstR      = INTE [0,9999999]   :'First run of this bit',
            LastR       = INTE [0,9999999]   :'Last run of this bit')
         ;
 
 VDPR
      :      ' VDET Period definition for Chip efficiency map.
              NR=VDET setup code.\
              Number of columns\
              Number of rows = number of time periods'
           STATIC

           SIZE 1,*                                                    

      = ( PeRiod      = INTE [1,100]       : ' The period number (usually the row number)',
          WeighT      = INTE [0,100]       : ' Weight of the time period ',
          Efficiency1 = REAL [0.00,100.00] : 'Default efficiency for view=1',
          Efficiency2 = REAL [0.00,100.00] : 'Default efficiency for view=2')
          ;

 VDCM
      :      ' VDET chip efficiency map for MC\
              Number of columns\
              Number of rows = number of chips with non-default efficiencies'
           STATIC

           SIZE 1,*                                                    

      = ( JLay       = INTE [0,2]       : ' Layer Number',
          IFac       = INTE [1,15]      : ' Face number in Layer',
          IMod       = INTE [1,2]       : ' Module in Face',
          VieW       = INTE [1,2]       : ' View',
          CHip       = INTE [1,8]       : ' Chip number',
          Period     = INTE [1,100]     : ' The period in VDPR bank',
          EffiCiency = REAL [0.0,100.0] : ' Efficiency of the chip')
          ;

 VDIZ
        : 'VDET : improve Z coordinate\
           number of columns \ 
           number of layers (=2)'
           STATIC

           SIZE 2,2                                                    
 
         =( LaYer        = INTE [1,2]    :'Layer',
            QuadTerm     = REAL [*,*]    :'Coeff for the quadratic term in Z',
            ConstTerm(3) = REAL [*,*]    :'Constant term for each wafer in the module')
            ;

 
 END ESET
 
 END SUBSCHEMA
 

SUBSCHEMA ALPHAconstants
:'Constants used in ALPHA for parametrization/calibration of data'
 
AUTHOR   'M. Thulasidas'
REVIEWER 'I.Tomalin'
VERSION  '1.2'
DATE     '03/08/99'
 
DEFINE ESET

 QIPC
      :      'QIPBTAG Calibration constants.
              NR=VDET setup code.\
              Number of columns\
              Number of rows = number of track types'
           STATIC
 
      = (A1exp        = REAL [0.0,1.0]  : 'Area of the first exponential',
         A2exp        = REAL [0.0,1.0]  : 'Area of the second exponential',
         Sigma        = REAL [0.0,3.0]  : 'Width of the Gaussian part',
         S1exp        = REAL [0.0,10.0] : 'Slope of the first exponential',
         S2exp        = REAL [0.0,10.0] : 'Slope of the second exponential')
         ;

 PFIX :      'Used by PPCORR to correct track momenta 
              (sagitta corrections) for residual tracking distortions 
              data processed with JULIA version < 300 
              Provided in 20 cos(theta) bins from -1 to +1. 
              (NR = run number).\
              Number of columns=43\                                    
              Number of rows=2 for different VDET hit requirements'
        = (ValRng(2),
          COrrection(20) = REAL : 'Systematic offset seen in 1/p\
                                   for +ve tracks',
          ERror(20)      = REAL : 'Statistical error on this offset')
       ;

 PNFX :      'Used by PPCORR to correct track momenta 
              (sagitta corrections) for residual tracking distortions 
              data processed with JULIA version > 300 
              Provided in 20 cos(theta) bins from -1 to +1. 
              (NR = run number).\
              Number of columns=43\                                    
              Number of rows=2 for different VDET hit requirements'

       = (ValRng(2),
          COrrection(20) = REAL : 'Systematic offset seen in 1/p\
                                   for +ve tracks',
          ERror(20)      = REAL : 'Statistical error on this offset')
       ;

 QNSM
      :      'QIPBTAG MC Smearing parameters.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types'
           STATIC

      = (P1exp        = REAL [0.0,1.0]  : 'First fraction of tracks to be smeared',
         W1exp        = REAL [0.0,*]  : 'Width of the first exponential',
         P2exp        = REAL [0.0,1.0]  : 'Second fraction of tracks to be smeared',
         W2exp        = REAL [0.0,*]  : 'Width of the second exponential')
      ;

 QNDE
      :      'QIPBTAG MC Deletion parameters.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types'
           STATIC

      = (FRact        = REAL [-1.0,1.0]  : 'Fraction of tracks to be deleted')
      ;

 QTSM
      :      'QIPBTAG MC Smearing parameters, angular dependent.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types * number of theta bins'
           STATIC

      = (P1exp        = REAL [0.0,1.0]  : 'First fraction of tracks to be smeared',
         W1exp        = REAL [0.0,*]  : 'Width of the first exponential',
         P2exp        = REAL [0.0,1.0]  : 'Second fraction of tracks to be smeared',
         W2exp        = REAL [0.0,*]  : 'Width of the second exponential')
      ;

 QTDE
      :      'QIPBTAG MC Deletion parameters, angular dependent.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types * number of theta bins'
           STATIC

      = (FRact        = REAL [-1.0,1.0]  : 'Fraction of tracks to be deleted')
         ;

 QPSM
      :      'QIPBTAG MC Smearing parameters, momentum dependent.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types * number of p bins'
           STATIC

      = (P1exp        = REAL [0.0,1.0]  : 'First fraction of tracks to be smeared',
         W1exp        = REAL [0.0,*]  : 'Width of the first exponential',
         P2exp        = REAL [0.0,1.0]  : 'Second fraction of tracks to be smeared',
         W2exp        = REAL [0.0,*]  : 'Width of the second exponential')
      ;

 QPDE
      :      'QIPBTAG MC Deletion parameters, momentum dependent.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types * number of p bins'
           STATIC

      = (FRact        = REAL [-1.0,1.0]  : 'Fraction of tracks to be deleted')
      ;

 QBSM
      :      'QIPBTAG MC Smearing parameters, angular and momentum dependent.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types * number of p and theta bins'
           STATIC

      = (P1exp        = REAL [0.0,1.0]  : 'First fraction of tracks to be smeared',
         W1exp        = REAL [0.0,*]  : 'Width of the first exponential',
         P2exp        = REAL [0.0,1.0]  : 'Second fraction of tracks to be smeared',
         W2exp        = REAL [0.0,*]  : 'Width of the second exponential')
      ;

 QBDE
      :      'QIPBTAG MC Deletion parameters, angular and momentum dependent.
              NR=100*VDET setup code + option
              (option = 1 for LEP1, 2 for LEP2; 3,4: non standard QIPBTAG).\
              Number of columns\
              Number of rows = number of track types * number of p and theta bins'
           STATIC

      = (FRact        = REAL [-1.0,1.0]  : 'Fraction of tracks to be deleted')
      ;

END ESET

END SUBSCHEMA 


 SUBSCHEMA YV0OffConsts
 : 'Description of V0 VERTEX fit constants banks'
 
 AUTHOR   'M.A. Ciocci,J.Sedgbeer'
 REVIEWER 'D.Casper'
 VERSION  '5.0'
 DATE     '04/03/97'
 
 DEFINE ESET
 
 
 
 YV1C
       :     'V0 Reconstruction Cuts
              NR=1 (LTC) \
              Number of words   \
              Number of rows'
 
       = (ValRng(2),
          IV = INTE [0,2]                  : 'IV=0 use (0,0,0); IV=1 use main\
                                              vertex from PYER; else use vertex\
                                              from bank JSUM',
          O1 = REAL [0.0000,100.0000]      : 'Min value of chi square increase\
                                              constraining  only  one track to\
                                              the main vertex',
          O2 = REAL [0.0000,100.0000]      : 'Min value of chi square increase\
                                              constraining both  tracks to the\
                                              main vertex',
          CT = REAL [0.00,3000.]           : 'Maximum value for distance between\
                                              starting  point of vertex fit',
          PS = REAL [-6.2831,6.2831]       : 'Minimum value for the psi angle',
          RV = REAL [0.0000,200.0000]      : 'Max  value  of  the  V0  vertex\
                                              radius',
          ZV = REAL [0.0000,220.0000]      : 'Max value of the V0 vertex abs(Z)',
          DI = REAL [0.0000,200.0000]      : 'Max value of difference between\
                                              tangent  of dip',
          CF = REAL [0.00,999.0000]        : 'Max value of the V0 fit chi square',
          CS = REAL [-1.000,1.0000]        : 'Minimum value of the cosinus of\
                                              the angle between V0 vertex and\
                                              V0 momentum',
          CO = REAL [0.0000,100.0000]      : 'Minimum value of the chi square\
                                              increase constraining V0 vertex\
                                              V0 vertex to the primary vertex',
          MA = REAL [0.0000,100.0000]      : 'Maximum value of the chi square\
                                              of the mass constraint. N.B. this\
                                              cut not used.',
          PMin = REAL [0.0000,999.0000]    : 'Minimum momentum of daughter\
                                              tracks GeV',
          NTpc = INTE [0,30]               : 'Minimum number of TPC hits \
                                              required on each daughter track',
          CoordsBefore = INTE [0,2]        : 'Test for more than NumberCoords\
                                              before vertex on 0,1 or both\
                                              tracks. CB=0 no test; CB=1 at\
                                              least one track must have < NC\
                                              before vx; CB=2 both tracks must\
                                              have < NC before vx.',
          NumberCoords = INTE [0,99]       : 'Maximum number of coords allowed\
                                              on track before vx.',
          MinimumDist = REAL [0.0000,999.] : 'Require V0 vx to be > MD from
                                              primary vx (cm).',
          DZero   = REAL [0.000,999.]      : 'Maximum value of d0 of the V0\
                                              wrt the primary vertex.',
          ZZero   = REAL [0.000,999.]      : 'Maximum value of Z0 of the V0\
                                              wrt the primary vertex.',
          KWindow = REAL [0.000,9999.]     : 'Keep candidate if pi.pi mass 
                                              within KW GeV of K0 mass',
          LWindow = REAL [0.000,9999.]     : 'Keep candidate if  p.pi mass 
                                              within LW GeV of Lambda mass.',
          GWindow = REAL [0.000,9999.]     : 'Keep candidate if e.e.  mass 
                                              within GW GeV of zero')
 
       ;
 
 YVNC
       :     'Nuclear Interaction Reconstruction Cuts
              NR=0 (LTC) \
              Number of words   \
              Number of rows'
              STATIC
       = (
          O1 = REAL [0.0000,100.0000]      : 'Min value of chi square increase\
                                              constraining  only  one track to\
                                              the main vertex',
          O2 = REAL [0.0000,100.0000]      : 'Min value of chi square increase\
                                              constraining both  tracks to the\
                                              main vertex',
          CT = REAL [0.00,3000.]           : 'Maximum value for distance between\
                                              starting  point of vertex fit',
          PS = REAL [-6.2831,6.2831]       : 'Minimum value for the psi angle',
          RV = REAL [0.0000,200.0000]      : 'Max  value  of  the  V0  vertex radius',
          ZV = REAL [0.0000,220.0000]      : 'Max value of the V0 vertex abs(Z)',
          DI = REAL [0.0000,200.0000]      : 'Max value of difference between\
                                              tangent  of dip',
          C2 = REAL [0.00,999.0000]        : 'Max value of the V0 fit chisquare',
          CS = REAL [-1.000,1.0000]        : 'Minimum value of the cosinus of\
                                              the angle between V0 vertex and\
                                              V0 momentum',
          CO = REAL [0.0000,100.0000]      : 'Minimum value of the chi square\
                                              increase constraining V0 vertex\
                                              V0 vertex to the primary vertex',
          PMin = REAL [0.0000,999.0000]    : 'Minimum momentum of daughter\
                                              tracks GeV',
          NTpc = INTE [0,30]               : 'Minimum number of TPC hits \
                                              required on each daughter track',
          NumberCoords = INTE [0,99]       : 'Maximum number of coords allowed\
                                              on track before vx.',
          MinimumDist = REAL [0.0000,999.] : 'Require V0 vx to be > MD from
                                              primary vx (cm).',
          DZero   = REAL [0.000,999.]      : 'Maximum value of d0 of the V0\
                                              wrt the primary vertex.',
          ZZero   = REAL [0.000,999.]      : 'Maximum value of Z0 of the V0\
                                              wrt the primary vertex.')
 
       ;
 
 YSCT
      :     'Secondary vertex cuts (Row 1=FRFT/0,Row 2=FRFT/2)\
             Number of sets of cuts\
             Number of cut values'
           STATIC
      = (PrimeVrtx = REAL [*,*]        : 'log10(min primary vrtx prob',
         SMin      = REAL [*,*]        : 'min distance along track for vtx',
         Chi2      = REAL [*,*]        : 'maximum pair vertex chi2',
         RLow      = REAL [*,*]        : 'minimum radius for vertex',
         RHigh     = REAL [*,*]        : 'maximum radius for vertex',
         DeltaR    = REAL [*,*]        : 'maximum radial difference to merge',
         ChiFin    = REAL [*,*]        : 'maximum chi2/DOF for final vtx',
         RadMat    = REAL [*,*]        : 'maximum distance from material',
         Spare1    = REAL [*,*]        : 'spare',
         Spare2    = REAL [*,*]        : 'spare')
      ;


 END ESET
 
 END SUBSCHEMA


 SUBSCHEMA YTOPRunConsts
 
 AUTHOR   'G.Lutz'
 REVIEWER 'F. Ranjard'
 VERSION  '1.0'
 DATE     '06/04/92'


 DEFINE ESET
 
  YTGP
      :      'General Parameters for vertex reconstruction YTOPOL package (NR=0)\
              Number of words/row\
              Number of rows'
           STATIC
 
      = (DistanceMax  = REAL  [0.,*]       : 'limit of distance from/between helix for vtx fit\
                                              used in YFMVTR,YFTVTR',
         MinMomentum  = REAL  [0.,*]       : 'minimum required charged track momentum for topology analysis',
         PartProb     = REAL  [0.,*]       : 'particle identification probability cut used in YPIDAS')
   ;

  YTPP
      :      'Primary vertex reconstruction Parameters in YTOPOL package (NR=0)\
              Number of words/row\
              Number of rows'
           STATIC
 
     = ( MinTpchits   = INTE  [0,*]        : 'minimum # of TPC hits required inprimary vertex track',
         NFirststep   = INTE  [0,*]        : 'max # of tracks used for first step of primary\
                                              vertex finding (highest momenta are selected)',
         MinMom       = REAL  [0.,*]       : 'min. mom. for first step of prim. vtx. search',
         MinmomAdd    = REAL  [0.,*]       : 'min.mom. for attaching trks in second step of prim. vtx reconstruction')
   ;

  YTCP
      :      'photon Conversion reconstruction Parameters in YTOPOL package (NR=0)\
              Number of words/row\
              Number of rows'
           STATIC
 
     = ( VtxChisq     = REAL  [0.,*]       : 'max.vertex chisq. in gamma converion reconstr.',
         PtCchisq     = REAL  [0.,*]       : 'max.pointing chisq.in gamma conversion reconstr.',
         minRadialDist= REAL  [0.,*]       : 'min.radial vtx dist.in gamma conversion reconstr.',
         MaxMass      = REAL  [0.,*]       : 'max.invariant mass in gamma conversion reconstr.',
         maxZDist     = REAL  [0.,*]       : 'max.z-dist.of trk. in gamma conversion reconstr.',
         maxNAdd      = INTE  [0,*]        : 'max.add.trks through gamma conversion vertex',
         LowIdprob    = REAL  [0.,*]       : 'min.el.prob. of both gamma conversion tracks',
         HighIdprob   = REAL  [0.,*]       : 'min.el.prob. of at least one gamma conversion track')
   ;

  YTVP
      :      'V0 reconstruction Parameters in YTOPOL package (NR=0)\
              Number of words/row\
              Number of rows'
           STATIC
 
     = ( VtxChisq     = REAL  [0.,*]       : 'max.vertex chisq. in V0 reconstr.',
         PtChisq      = REAL  [0.,*]       : 'max.pointing chisq.in V0 reconstr.',
         DistChisq    = REAL  [0.,*]      : 'min. chisq.vtx dist. in V0 reconstr.',
         MassDev      = REAL  [0.,*]       : 'inv.mass chisq.dev. in V0 reconstr.',
         maxZDist     = REAL  [0.,*]       : 'max.z-dist.of trk. in V0 reconstr.',
         maxNAdd      = INTE  [0,*]        : 'max.add.trks through V0 vertex',
         minPIonprob  = REAL  [0.,*]       : 'min. pion prob. of K0 decay tracks',
         minPRotprob  = REAL  [0.,*]       : 'min. proton prob. of lambda decay track',
         minPJonprob  = REAL  [0.,*]       : 'min. pion prob. of lambda decay track')
      ;
 
 END ESET
 
 END SUBSCHEMA

 
 SUBSCHEMA JULSkelConsts
 : 'JULIA skeleton Constants'
 
 AUTHOR   'E. Lancon'
 REVIEWER ''
 VERSION  '1.0'
 DATE     '06/04/93'
 
 DEFINE ESET
 
  PASC
             : 'PASS0 cuts definitions\
                number of columns\
                number of rows'
             STATIC 

      = (MInpass0   = INTE  : 'Minimum number of "good for pass0" events needed to update
                                the drift velocity',
         MAxpass0   = INTE  : 'Maximum number of "good for pass0" events used to update the
                               drift velocity',
         ElapsTime  = REAL  : 'Maximum allowed elapsed time since last laser measurement to use
                               vz_laser if there are too few events',  
         DeltaT0    = REAL  : 'Maximum allowed absolute value for delta-t0 (mus) in order to update
                               the drift velocity')

              ;
 
  END ESET
 
END SUBSCHEMA
 
 
 SUBSCHEMA TPCSIM
 
 AUTHOR   'P.Janot'
 REVIEWER 'F. Ranjard'
 VERSION  '1.2'
 DATE     '23/11/92'
 
 DEFINE ESET
 
 TEXB
      :   'Tpcsim EXB effect information. (LTC)\
           Number of words/grid configuration\
           Number of grid configurations (=4)'
           STATIC
      = (ChargeRatio     = REAL [0.,1.000000] : 'Charge getting through the gate
                                                 with the NRth grid configurations',
         ShiftTime       = REAL [0.,*]        : 'Mean extra drift time due to ExB
                                                effect in the NRth grid
                                                configuration',
         ExbShift(50)    = REAL [*,*]         : 'ExB Shift of the ionization charge as
                                                a function of its distance to the
                                                the wires')
      ;
 
TMSH
     : 'Tpcsim Mean ExB SHift for the 2 clusters in super broken segment. (LTC)\
        Number of configurations/row\
        Number of steps (=1001)'
        STATIC
 
     = (MeanShift(4) = REAL [*,*] : 'Mean shift of the ionization charge due
                                     to ExB effect for a given track inclination
                                     with respect to the wires, with 4 grid
                                     configurations')
     ;
 
 TIND
      :   'Tpcsim INDices for TLAN bank. (LTC)\
           Number of words/index\
           Number of indices (=100)'
           STATIC
 
      = (IndexN    = INTE [0,*]   : 'Index in TLAN for N primary cluster
                                     case probabilities')
      ;
 
 TWRD
      :   'Tpcsim WoRD numbers in TLAN bank. (LTC)\
           Number of words/word number\
           Number of word numbers (=100)'
           STATIC
 
      = (WordN     = INTE [0,*]   : 'Word number in TLAN for N primary cluster
                                     case probabilities')
      ;
 
 TLAN
      :   'Tpcsim LANdau fluctuation probabilities. (LTC)\
           Number of words/row\
           Number of rows (=34438)'
           STATIC
 
      = (ProbaM    = REAL [0.,1.000000] : 'Probability of producing less than N+M
                                          secondary electrons from N primary
                                          clusters. M between 1 and WordN
                                          given in TWRD. Offset IndexN is given
                                          in TIND ')
      ;
 
 TSIM                                                                           
      :   'Tpcsim Run Conditions,NR=setup code,(TSIM,NR=0 is written on the run record)\
           Number of words/run\                                                 
           Number of runs (=1)'                                                 
           STATIC                                                               
      = (TpcVer  = INTE [205,*]  : 'TPCSIM Version Number (internal)',
         MXtran  = INTE [0 ,*]   : 'Max no of transports in a cluster',         
         ITrcon  = INTE [1 ,4]   : 'Gating mode',                               
         NlShap  = INTE [0 ,*]   : 'Shaping signal length (internal)',                     
         NshpoF  = INTE [0 ,*]   : 'First bin offset of shap. signal (internal)',          
         NPolya  = INTE [0 ,*]   : 'Number of electr. for Polya dist.',         
         NCpad   = INTE [0 ,*]   : 'Number of pads to test',                    
         LThrsh  = INTE [0 ,*]   : 'Threshold wire zero suppression',           
         NpResp  = INTE [0 ,*]   : 'Number of presamples   (wires)',            
         NpOsts  = INTE [0 ,*]   : 'Number of postsamples  (wires)',            
         MInlen  = INTE [0 ,*]   : 'Min. number of samples (wires)',            
         LtHrs2  = INTE [0 ,*]   : 'Threshold pad zero suppression',            
         NprEs2  = INTE [0 ,*]   : 'Number of presamples   (pads )',            
         NposT2  = INTE [0 ,*]   : 'Number of postsamples  (pads )',            
         MiNle2  = INTE [0 ,*]   : 'Min. number of samples (pads )',            
         NWsmax  = INTE [0 ,*]   : 'Max num. samples for wire data red.',       
         DrfVel  = REAL [0.,*]   : 'Drift velocity (cm/ns)',                    
         SigmA   = REAL [0.,*]   : 'Longitudinal diffusion(   )',               
         SigtR   = REAL [0.,*]   : 'Transversal diffusion(   )',                
         TpAnbn  = REAL [0.,*]   : 'Analog  time bin length (ns) (internal)',              
         TpDgbn  = REAL [0.,*]   : 'Digital time bin length (ns)',              
         AMplit  = REAL [0.,*]   : 'Charge amplification factor',               
         CUtoff  = REAL [0.,*]   : 'Cutoff for pad coupling',                   
         eFFcp   = REAL [0.,*]   : 'Pad coupling constant',                     
         SigW    = REAL [0.,*]   : 'Pad coupl. transv. sigma',                  
         SigH    = REAL [0.,*]   : 'Pad coupl. radial  sigma',                  
         HaXcut  = REAL [0.,*]   : 'Half-length of flat PRF (pads)',            
         trEFcp  = REAL [0.,*]   : 'Trigger pad coupling constant',             
         SIgr    = REAL [0.,*]   : 'Trigger pad coupl. transv. sigma',          
         SigarC  = REAL [0.,*]   : 'Trigger pad coupl. radial  sigma',          
         RaXcut  = REAL [0.,*]   : 'Half-length of flat PRF (t. pads)',         
         TcScut  = REAL [0.,*]   : 'Cutoff got trigger pad coupling ',          
         PEdeff  = REAL [0.,*]   : 'Pedestal (mV)',                             
         SPedef  = REAL [0.,*]   : 'Sigma for pedestal variation (mV)',         
         SGadef  = REAL [0.,*]   : 'Sigma for gain variation (%)',              
         SDidef  = REAL [0.,*]   : 'Differential nonlinearity level',           
         WirNrm  = REAL [0.,*]   : 'Wire charge normalization',                 
         PadNrm  = REAL [0.,*]   : 'Pad  charge normalization',                 
         TrgNrm  = REAL [0.,*]   : 'Trigger pad charge normalization',          
         CField  = REAL [0.,*]   : 'Magnetic Field (kGauss) (internal)',                   
         WpScal  = REAL [0.,*]   : 'Scale for wire charge',                     
         ThrZtw  = REAL [0.,*]   : 'For t0 estimator',                          
         ILevel  = CHA4          : 'Simulation level (internal)',                          
         IproG   = CHA4          : 'Program which invoked TPCSIM (internal)')              
      ;                                                                         
                                                                                
                                                                                
 
 END ESET
 
 END SUBSCHEMA
 
 
 SUBSCHEMA HVbitsStatus
    : 'HV bits modifications '

 AUTHOR     'B.Bloch'
 REVIEWER   'B.Bloch'
 VERSION    '1.0'
 DATE       '25/10/98'
                     
 DEFINE ESET
   XHVB
      :'Modifications to HV bit status /
        NR= Period number (STC)\
        Number of Words/modification\
        Number of modifications'
        STATIC 

     = (RUnnumber   = INTE [2001,*] : 'Run number affected ',
        FirstEvt    = INTE [1,*]    : 'First event affected ',
        LastEvt     = INTE [1,*]    : 'Last event affected ',
        HVstatus    = INTE [0,1]    : 'HV status to be set 1=ON,0=Off',
        DEtector    = INTE [1,24]   : 'Detector/item affected \
                                    1= ECAL End A\
                                    2= ECAL End B\
                                    3= ECAL Barrel\
                                    4= LCAL \
                                    5= TPC dE/dx \
                                    6= ITC \
                                    7= SATR \
                                    8= HCAL End A\ 
                                    9= HCAL End B\ 
                                   10= HCAL Barrel\
                                   11= SCAL \
                                   12= BCAL \
                                   13= VDET \
                                   14= Trigger Analog Crates \
                                   15= Trigger Camac Crates \
                                   16= TPC  \
                                   17-23= not attributed \
                                   24 = Beam pick up')

   ;

  END ESET

  END SUBSCHEMA


 SUBSCHEMA X1TriggerDef
 :'definition of the logical functions to verify the 32 trigger bit \
   mask from the fastbus register bit pattern.'
 
 
 AUTHOR   'A.Putzer,M.Wunsch'
 REVIEWER 'N.I. EMAND'
 VERSION  '1.6'
 DATE     '23/03/95'
 
 
 
 DEFINE ESET
 
  X1TV
        : 'Trigger level 1 Threshold values NR=1 (MC)\
         Number of words / Row \
         Number of Rows'
 
      STATIC
      SIZE 1,6
 
      = (ThrType       = CHA4 :
                              'Threshold type name.',
         ThrVal(4)     = INTE [0,*] : '4 thresholds per threshold type
                               HCWI : Number of double planes
                               ECEC : Threshold (MeV)
                               ECBA : Threshold (MeV)
                               ETEC : Threshold (MeV)
                               ETBA : Threshold (MeV)
                               LCEC : Threshold (MeV)'     )
      ;
 
X1TH
      :      'Trigger level 1 thresholds (STC)\
              Number of words/threshold set\
              Number of threshold sets'
 
      = (ValRng(2),
         ThrType      = CHA4               : 'Type of threshold\
                                              ECTR = EcTowerEnergy\
                                              ECWI = EcWireEnergy\
                                              HCTR = HcTowerEnergy\
                                              HCWI = HcWire\
                                              LCTR = LcTowerEnergy\
                                              ECTT = EcTotalTowerEnergy\
                                              ECTW = EcTotalWireEnergy\
                                              HCTT = HcTotalTowerEnergy',
         ThrVal(4)    = INTE [0,99999]    : 'Threshold Values')
      ;
 
 XTTL
      :  'Trigger Logical functions. (STC)
          Each row codes a logical function which defines operations
          on a 72 bit string corresponding to the 72 bit wide pattern
          entering the ECLine modules of the trigger logic.
          The bit strings are either data from the fastbus register
          or predefined bit masks out of table XTMS.
          The result of a logic function is again a 72 bit string. \
          number of word/ logical function \
          number of functions '
 
      SIZE 1,70
 
    =   (ValRng(2),
         FunlvlInd = INTE [0,2]  : 'Function level Index, if 0 this row is
                                    undefined, 1 for level one definition
                                    2 for trigger level two definition.',
         FunNam(2) = CHA4        : 'Name of logical function, either PRxx
                                    if the result is a pretrigger or the
                                    name of a trigger the function is
                                    related to.' ,
         NumOper   = INTE [0,5]  : 'Number of bit string operands',
         Bitstr1   = CHA4        : 'Name of the bit string used
                                    in the logical function. \
                                    Names starting with M are mask bit
                                    pattern contained in table XTMS, \
                                    Names starting with PR are results from
                                    pretrigger logical functions, \
                                    all other names refer to input signals.',
         Logoper1   = CHA4       :
             'Logic operator acting on the enclosing bit strings \
              AND : A bitwise AND \
              OR  : A bitwise OR  \
              MAxx: A bit majority function is performed. \
              If the number of bits in the left string
              is less than xx,
              the  right hand side operand is inverted. \
              ROxx: A bit rotation function is performed. \
              The number of bits indicated by 2*xx is rotated
              by xx bit positions. \
              Sxxx: A bit shift function is performed. \
              The number of bits indicated by xxx is shifted
              xxx bit positions. If xxx is negative the shift is in
              right direction.
              DIxx: A bit discrimination function is performed. \
              The righthand operator is interpreted a integer value. \
              If the value less than xx,
              the right hand side operand is inverted.',
         Bitstr2   = CHA4        : 'Bit string name (see Bitstr1).',
         Logoper2  = CHA4        : 'Logic Operator',
         Bitstr3   = CHA4        : 'Bit string name (see Bitstr1).',
         Logoper3  = CHA4        : 'Logic Operator',
         Bitstr4   = CHA4        : 'Bit string name (see Bitstr1).',
         Logoper4  = CHA4        : 'Logic Operator',
         Bitstr5   = CHA4        : 'Bit string name (see Bitstr1).')
 
      ;
 
 XTMS
      :       'Trigger Mask Strings used in logical operations defined
               in bank XTLF. (STC)\
               Number of words per string \
               Number of bit strings '
        SIZE 1,30
 
      = (ValRng(2),
         MaskId       = INTE [0,30] : 'Mask Identification, if 0 this
                                       row is undefined',
         MaskNam      = CHA4  : 'Bit Mask Name, \
              valid names of bit strings are: \
              The following patterns are predefined bit masks: \
              MTRU: all 72 bits are 1 \
              MFAL: all 72 bits are 0 \
              MITV: ITC veto mask = 0XFF \
              MIBB: ITC back to back bit = 2**12 \
              MTPV: TPC veto mask = 0XF  \
              METE: ECT total energy endcap mask = 0X30 \
              MEWB: ECW total energy barrel mask = 0X4400 \
              MEWT: ECW total energy total mask  = 0X8800 \
              MHTE: HCT total energy endcap mask = 0X3 \
              MEND: Endcap A or B mask bits <0:11>=1,<60:71>=1 \
              MBAR: Barrel mask bits <24:47>=1 \
              MLTA: LCAL A side bits <0 :11>=1 \
              MLTB: LCAL B side bits <12:23>=1 ',
         MaskStrng(3) = INTE [-999999999,999999999]
                                     : 'Mask bit String of 72bits')
    ;
 
 XTBN
      :       'Trigger Bit Names of all 32 trigger bits (STC)\
               Number of words per trigger bit definition \
               Number of level 1 trigger bits'
        SIZE 32,32
 
    =   (ValRng(2),
         TrgdefiN    = INTE [0,32] : 'Trigger defiNed flag, if 0 the bit
                                      is undefined and not used for
                                      triggering.',
         TrgBitnr    = INTE [0,31] : 'Number of trigger bit in the
                                      level 1/2 trigger bit mask.',
         BitMnem(2)  = CHA4        : 'Mnemonic name of this trigger bit',
         BitName(10) = CHA4        : 'Full name of this trigger bit',
         ActDet(5)   = CHA4 'HCAL' | 'ECAL' | 'LCAL' | 'ITC ' | 'TPC '|
                            'SATR' | '    ' :
                                     'Name of active subdetectors needed
                                      to realize this trigger. This
                                      information is deduced from the
                                      XTLF bank.')
      ;
 
 
END ESET
 
DEFINE RSET
 
    (XTBN [0,1] -> [0,1] XTTL BY Lvl1trg)
         : 'Links a trigger bit with corresponding logical function'
    ;
    (XTBN [0,1] -> [0,1] XTTL BY Lvl2trg)
         : 'Links a trigger bit with corresponding logical function
            for the level 2 decision of this trigger'
    ;
 
END RSET
 
END SUBSCHEMA
 
 
 SUBSCHEMA X1AdcCal
 : 'Calibration constants for the ADCs in the trigger system.'
 
 
 AUTHOR   'R. Geiges'
 REVIEWER 'N.I. EMAND'
 VERSION  '1.1'
 DATE     '23/11/89'
 
 
 DEFINE ESET
 
 X1CA
      :  'Trigger level 1 Calibration Constants. (STC)
          The ADCs in the trigger system digitize the mixed analog
          signals which enter the discrimiators of the trigger
          electronics. There are five ADCs to pickup to signals
          from the calorimeters (HCT, HCW, ECT, ECW and LCT).
          72 out of the 96channels of the LeCroy ADCs 1885F are
          actually used, one for each trigger segment. There
          are 4 calibration constants for each channel:
          Pedestal, Slope and the energy scale factor,
          and a conversion factor of threshold counts
          to energy in keV. For the total energy discriminators
          a special entry exists, giving the threshold calibration
          and a threshold offset. \
          Number of calibr. constants per row, \
          Number of rows. '
 
      SIZE 4,22
 
    =   (ValRng(2),
         ConType   = CHA4    : 'Type of calibration constant which
                                follows. The types are:
                                HCTP, HCWP, ECTP, ECWP, LCTP, \
                                HCTS, HCWS, ECTS, ECWS, LCTS, \
                                HCTE, HCWE, ECTE, ECWE, LCTE, \
                                HCTT, HCWT, ECTT, ECWE, LCTT, \
                                ETTT, ETTO. \
                                P = Pedestal, S = Slope, E = Energy
                                scale, T = Threshold, O = Offset.
                                The calibration constants are
                                always stored in this sequence, such
                                that they may be referenced by fixed
                                pointers.',
         CalCons(72) = INTE  :
                               'Calibration Constants \
                                The unit of the threshold scale is
                                keV/discri. counts.          \
                                The unit of pedestal values is ADC
                                counts.                            \
                                The unit for the slopes is
                                pc/counts.                         \
                                The unit for the energy scale is
                                keV/pc, for HCW the scale is
                                dbl planes/pc.                     \
                                The unit for the offset values is
                                keV/dicri. counts.')
 
 
      ;
 
END ESET
 
END SUBSCHEMA
 
 
 SUBSCHEMA BANKDOCU
 :'Description of tables containing information for non-Database tables used
   for documentation purpose only'
 
 
 AUTHOR   'A. Putzer'
 VERSION  '1.0'
 DATE     '06/11/87'
 
 DEFINE ESET
 
 
 COL2
      :      'The table containing information for
              non-DATABASE tables. (DOC)'
 
      = (Name         = CH16                  : 'Name of a column',
         Type         = CHA4                  : 'Type of a column',
         Format       = CHA8                  : 'Format for a column',
         TableInd     = INTE                  : 'Index to the TAB2 table')
      ;
 
 
 
 TAB2
      :      'The table containg a list of all non-DATABASE tables
              ready lized entity set to control the branch
              to the different detector components. (DOC)'
 
      = (Name         = CH16                  : 'Name of a table')
      ;
 
 
 
 END ESET
 
 END SUBSCHEMA
 
 
