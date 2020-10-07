*CD agcons
#include "alcons.h"
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
#if defined(DOC)
     RADEG = Conversion factor from radians to degrees
     DEGRA = Conversion factor from degrees to radians
     D360  =  360. degrees decimal value
     D180  =  180. degrees
     D90   =   90. degrees
     D45   =   45. degrees
     D15   =   15. degrees
     D210  =  210. degrees
     D225  =  225. degrees
     D270  =  270. degrees
     D7P5  =   7.5 degrees
     IAGROT= last rotation matrix number defined
     IAGMAT= last material  number defined
     IAGMED= last tracking medium number defined
     IAGFHB= first rotation matrix number for hcal barrel
              (to be used later for muon chambers)
     LSENV = Maximum number of sensitive volumes possibly defined
      IAGSLV=  last sensitive volume number defined
      IAGSEN(30,2) =array giving the correlation between the sensitive volume
        name  and the level in the geometry tree where to find the slot #
              First index is the name of the sensitive volume
              Second index is the level in the tree where to find the slot #
     LIMVOL= Maximun number of items possibly defined for LAGIMP
      NAGIMP      = number of defined items in LAGIMP
      LAGIMP(1,*) = name of GEANT3 volume
      LAGIMP(2,*) = level of this volume in the volume tree
      LAGIMP(3,*) = detector name, stored in 'IMPA'

#endif
#include "wrkspc.h"
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
C
#if defined(DOC)
      PTAB  (LPTAB) = Dummy array used in geometry routines AGXXXX
#endif
