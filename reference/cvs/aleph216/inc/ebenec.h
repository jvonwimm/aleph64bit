      COMMON/EBENEC/ENCRAT,ENECRA(2),ENECA1(2),EESTYA(3),EESTYB(3),
     1        RATIO1,RATIO2,R11STY,R12STY,
     2        ITRWEB,JFCLEB, KODEEB(4),NREGEB(3),SINCEB,
     3        ENETOT,ENEERR,YCOFIN,YCOERR,PHICOR,
     4        YLIMIT(3)
C
      PARAMETER ( ETHRL = 0.03 , CECT1 = 0.0121 , DISFE = 255. )
      PARAMETER ( CECT2 = 0.1904 , PETIT = .0001 )
      PARAMETER ( YLIM1 = 1.8 , YLIM2 = 3.2 , YLIM3 = 1.3 )
#if defined(DOC)
C!         ECAL Crack cluster COMMON
C
C            ENCRAT = total cluster energy
C            ENECRA = energy in module each side of crack
C            ENECA1 = energy in pad rows adjacent to crack
C            EESTYA = storey energies in max energy module
C            EESTYB = storey energies in min energy module
C            RATIO1 = energy of last pad row/total energy , for
C                module with greater energy
C            RATIO2 = energy of lesser energy module/total cluster
C             energy
C            R11STY = storey energy ratio  1 / 1+2+3
C            R12STY = storey energy ratio  1+2 / 1+2+3
C
C            ENETOT = corrected cluster energy
C            ENEERR = error on corrected energy
C            YCOFIN = corrected cluster local space coordinate
C                     in first wire plane orthogonal to crack.
C            YCOERR = error on corrected space coordinate
C            PHICOR = corrected cluster phi angle
C            IMPC = flag to signify crack or module impact
C
C            YLIMIT = widths of insensitive regions of cracks
C
C            ETHRL = nominal minimum energy threshold
C            CECT1 = constant in pad row to radius relation
C            CECT2 = constant in pad row to radius relation
C            DISFE = ECAL end cap radius parameter
C
#endif
