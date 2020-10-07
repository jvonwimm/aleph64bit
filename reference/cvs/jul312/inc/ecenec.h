      COMMON/ECENEC/ENCRAT,ENECRA(2),ENECA1(2),EESTYA(3),EESTYB(3)
     *       ,RATIO1,RATIO2,R11STY,R12STY,ENETOT,ENEERR,ENHADR
     *       ,YCOFIN,YCOERR,YLIMIT(3),PHICOR,JREGIO,IFLGCM,IFLGGD
C
      PARAMETER ( ETHCM = 0.5, ETHR1 = 0.5, ETHRL = 0.03,
     +            YCSEC = 4.6, ECECS = -0.0014, CECT1 = 0.0121,
     +            DISFB = 186.8, DISFE = 255., CECT2 = 0.1904,
     +            PHIHM = 0.2618, PHIOF = 0.0327 )
#if defined(DOC)
C!         ECAL Crack cluster COMMON
C!
C!           ENCRAT = total cluster energy
C!           ENECRA = energy in module each side of crack
C!           ENECA1 = energy in pad rows adjacent to crack
C!           EESTYA = storey energies in max energy module
C!           EESTYB = storey energies in min energy module
C!           RATIO1 = energy of last pad row/total energy , for
C!               module with greater energy
C!           RATIO2 = energy of lesser energy module/total cluster
C!            energy
C!           R11STY = storey energy ratio  1 / 1+2+3
C!           R12STY = storey energy ratio  1+2 / 1+2+3
C!           ENETOT = corrected cluster energy
C!           ENEERR = error on corrected energy
C!           YCOFIN = corrected cluster local space coordinate
C!                    in first wire plane orthogonal to crack.
C!           YCOERR = error on corrected space coordinate
C!           YLIMIT = widths of insensitive regions of cracks
C!           PHICOR = corrected cluster phi angle
C!           ENHADR = associated hadron energy.Not currently used.
C!           JREGIO = region parameter for end cap or barrel cracks
C!           IFLGCM = flag to signify crack or module impact
C!           IFLGGD = flag to signify use or not use clster correction
C!           ETHRL = nominal minimum energy threshold
C!           ETHCM = energy threshold used in crack or module decision
C!           ETHR1 = RATIO1 threshold parameter
C!           YCSEC = distance of second pad row from crack
C!           ECECS = end cap energy correction extrapolation factor
C!           CECT1 = constant in pad row to radius relation
C!           CECT2 = constant in pad row to radius relation
C!           DISFB = ECAL barrel radius parameter
C!           DISFE = ECAL end cap radius parameter
C!           PHIHM = ECAL module half phi angle
C!           PHIOF = ECAL barrel - end cap phi offset.
C!
#endif
