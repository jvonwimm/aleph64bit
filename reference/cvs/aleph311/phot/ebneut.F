      SUBROUTINE EBNEUT( ICLN , IOPT , IFOT , PHOT , CORE , IER )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     04/10 89
C! Analysis of a neutral cluster.
CKEY PHOTONS NEUTRAL CLUSTER ANALYSIS / USER
C
C   ICLN is the number of the cluster as defined in JULIA ( row number
C   in ECLU or ECOB ) and in the POT ( row number in PECO or PEOB ).
C   IOPT(6) is an array of control words defining the corrections to be
C   applied to the energies estimations.
C   IFOT(19) and PHOT(20) are respectively integer and real arrays
C   containing the results of the analysis.This information is calculate
C   using only the storeys of the cluster ICLN.
C   IER is an error code.
C
C   Input     : ICLN    Cluster number.
C               IOPT    Corrections control words.
C                       IOPT(N)  =    0 : JULIA method.
C                                     1 : No correction.
C                                    >1 : Other method to be defined.
C                       N = 1  :  Overlap correction.
C                       N = 2  :  Crack correction.
C                       N = 3  :  Leakage correction.
C                       N = 4  :  Clusterisation correction.
C                       N = 5  :  Saturation correction.
C                       N = 6  :  Hadronic energy estimation.
C
C Output :
C          IFOT Integer output array.
C
C               1   Theta index of the barycenter tower.
C               2   Phi index of the barycenter tower.
C               3   Subcomponent number( 1 to 3 ).
C               4   Module number( 1 to 12 ).
C               5   Region number( 1 to 4 ).
C               6   1 or 2 if limit of region.
C               7   1 if endcap crack in the overlap.
C               8   1 , 2 , 3 or 4 if near a crack.
C               9   1 to 6:barrel/endcap overlap, 7:endcap/luminometer.
C              10   Cluster extension covering several regions.
C                       = 0 for 1 region,
C                       = 1 for 2 regions (NR and NR+1), the greatest
C                          signal belonging to region NR+1,
C                       = 2 for 2 regions (NR and NR+1), the greatest
C                           signal belonging to region NR,
C              11   Overlap and crack in the secondary detector.
C              12   Cluster covering two modules (MD and MD+1) : crack.
C                       in the same detector.
C                       = 1 the main module is MD+1.
C                       = 2 the main module is MD.
C              13   Cluster extension covering two detectors : overlap.
C                       = 1 the barrel is the main detector,
C                       = 2 the endcap is the main detector,
C                       = 3 there is an overlap with the luminometer
C              14   Concerns the cluster analysis method.
C                       = 0 , 9 : Standard.
C                       = 10,19 : Crack between modules.
C                       = 20,29 : Overlap between detectors.
C                       = 30.39 : Crack in the overlap.
C                       = 40,49 : Dead storey.
C              15   Validity of the two longitudinal estimators.
C                       The first one ( PHOT(7) ) is the starting depth
C                       of the shower.The second one ( PHOT(8) ) is the
C                       deviation of the shower shape from the mean
C                       expected shape. Such quantities may be
C                       uncalculable :
C                        = 0 : The two identificators may be used.
C                        = 1 : Only the first one may be used.
C                        = 2 : Only the second one may be used.
C                        = 3 : The two identificators are uncalculable.
C              16   Concerns the nature of the hypothesis
C                        = 0 : The cluster is unambiguously identified
C                        as an electromagnetic object.
C                        = 1 : The cluster is unambiguously identified
C                        as an hadronic object.
C                        = 2 : The cluster identification is ambiguous.
C              17   Concerns the transverse identification.
C                        It depends of the nature of the hypothesis.
C                        If IFOT(7) is 0 or 2 ( electromagnetic ) :
C                        = 0 : Single isolated gamma.
C                        = 1 : Unresolved multi gammas.
C                        = 2 : One gamma extracted from a multi gammas.
C                        = 3 : Two gammas extracted from the cluster.
C                        If IFOT(7) is 1 ( hadronic ) :
C                        = 0 : Purely hadronic cluster.
C                        = 1 : Mixed cluster.
C                        = 2 : One gamma extracted from the cluster.
C              18   Nature of the first identified particle.
C                        =  1    Isolated gamma
C                        =  2    Gamma from multi gammas cluster.
C                        =  3    Gamma from identified Pi0.
C                        = 10    Unresolved gamma-gamma.
C                        = 17    ECAL fraction of a neutral hadron.
C              19   Nature of the second identified particle.
C                        =  0    No second particle.
C                        =  2    Gamma from multi gammas neutral cluster
C                        =  3    Gamma from identified Pi0.
C                        = 12    Residual e.m. energy.
C                        = 17    ECAL fraction of a neutral hadron.
C
C          PHOT Real output array.
C
C               1   Raw Energy (Gev)
C               2   Proportion of raw energy in stack 1. "Stack 1" is
C                   defined properly for the overlap region according to
C                   the tower number.
C               3   Proportion of raw energy in stack 2. "Stack 2" is
C                   defined properly for the overlap region according to
C                   the tower number.
C               4   Energy (Gev) corrected for geometrical effects
C                   (cracks,detector,overlap, dead storeys,leakage,
C                   clusterisation) accordingly to the IOPT array.
C               5   Barycenter : Theta coordinate in radians.
C               6   Barycenter : Phi coordinate in radians.
C               7   First longitudinal estimator : materialisation
C                   depth in radiation lengthes.
C               8   Second longitudinal estimator : deviation from the
C                   mean shape. The distribution of this variable is a
C                   normal law for the photonic showers.
C               9   Crack estimator 1 : Main module energy.
C              10   Crack estimator 2 : Energy of the row adjacent to
C                                       the crack.
C              11   Crack estimator 3 : Energy ratio stack 1+2 / 1+2+3
C              12   First transverse estimator : proportion of energy in
C                   the four central towers.
C              13   Second transverse estimator : deviation from the mea
C                   shape of a single photon. The distribution is a
C                   normal law for an isolated gamma.
C              14   Best estimate of the first particle Energy (Gev)
C                   All corrections defined in IOPT have been applied.
C              15   Teta line of flight of the first particle.
C              16   Phi line of flight of the first particle.
C              17   Best estimate of the second particle Energy (Gev)
C                   All corrections defined in IOPT have been applied.
C              18   Teta line of flight of the second particle.
C              19   Phi line of flight of the second particle.
C              20   Corrected energy in the hadronic hypothesis.
C
C         CORE      Applied corrections accordingly to IOPT.
C
C         IER       Error code.
C                   = 0  No error.
C                   > 0  Fatal error.
C                   < 0  Overlap or crack.
C                   = 1  Missing bank.
C                   = 2  Too many storeys
C                   = 3  No storey in this cluster.
C                   = 4  Energy < EMIN = .05
C                   = 5  Energy > EMAX = 80.
C                   = 6  Petal inner edge
C                   = 7  Pathologic cluster.
C                   = -1 Truncated endcap , 3 stacks.
C                   = -2 Truncated endcap , 2 stacks.
C                   = -3 Overlap near endcap , 2 stacks.
C                   = -4 Overlap near endcap , 3 stacks.
C                   = -5 Overlap near barrel.
C                   = -6 Truncated barrel , 2 stacks.
C                   = -7 Truncated barrel , 3 stacks.
C                   =-11 Crack.
C
C   BANKS :
C     INPUT   : NONE
C     OUTPUT  : NONE
C
C   Called by USER
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION IOPT(*) , IFOT(*) , PHOT(*) , CORE(*)
      PARAMETER ( MXST = 200 )
      DIMENSION INDX(3,MXST) , ESTO(MXST) , MODU(3,2) , ECLS(3,3)
      DIMENSION STAC(3) , CRAC(5) , EVLP(5) , ITMX(2) , CLUR(3,3,3)
C   Initialise the CORE , IFOT , PHOT  arrays.
      DO 1 I = 1 , 6
        CORE(I) = 1.
        PHOT(I) = 0.
        IFOT(I) = 0
    1 CONTINUE
      DO 2 I = 7 , 19
        PHOT(I) = 0.
        IFOT(I) = 0
    2 CONTINUE
      PHOT(20) = 0.
C   Put the storeys of the cluster ICLN in the ESTO array.
      CALL EBPREP( MXST ,ICLN , NUST , INDX , ESTO , IER )
      IF( IER .NE. 0 ) GO TO 98
C   Calculate the stacks content of each module and the extension code
C   of the cluster over several modules and regions.
      CALL EBOIDP( NUST , INDX , ESTO , IFOT(10) , MODU , ECLS )
      IF( NUST .EQ. 0 ) GO TO 102
C   Search if there are storeys in the overlap and compute
C   the correction to the energy.
      IF( IFOT(13) .NE. 0 ) THEN
        CALL EBCLAP( IOPT(1) , NUST , INDX , ESTO , EVLP , IVLP )
      ELSE
        IVLP = 0
      ENDIF
C   Compute the barycenter and the central tower region code.
      CALL EBARYC( NUST,INDX,ESTO,IFOT(1),PHOT(5) )
      IF( IFOT(1) .EQ. 0 ) GO TO 102
C   Three stacks content with the EBSLIM convention.
      CALL EBSTAK( IFOT(9) , MODU , ECLS , STAC )
C   Raw energy.
      PHOT(1) = STAC(1) + STAC(2) + STAC(3)
C   Proportion of raw energy in stacks 1 and 2.
      PHOT(2) = STAC(1) / PHOT(1)
      PHOT(3) = STAC(2) / PHOT(1)
C   Add overlap correction to the stack 1 of the endcap.
      IF( IVLP .GT. 0 ) THEN
        IFOT(14) = 20
        TVLP = 0.
        DO 3 I = 1 ,5
          TVLP = TVLP + EVLP(I)
    3   CONTINUE
        CALL EBVLAP( IFOT(9) , IFOT(3) , TVLP , STAC )
        CORE(1) = 1. + TVLP / PHOT(1)
      ELSE
        IFOT(14) = 0
      ENDIF
C   Longitudinal treatment.
      CALL EBLONG(STAC,PHOT(5),PHOT(6),IFOT(16),IFOT(15),PHOT(7),JER)
      IF( JER .GT. 0 ) GO TO 104
C   Leakage correction.
      CALL EBLEAK(IOPT(3),STAC,PHOT(5),CORE(3))
C   Clusterisation correction.
      CALL EBCOCL(IOPT(4),NUST,PHOT(1),CORE(4))
C   Crack?
      IF( ( IFOT(8) .NE. 0 .OR.  IFOT(12) .NE. 0 ) .AND.
     +      IFOT(9) .EQ. 0 .AND. IFOT(13) .EQ. 0 ) THEN
C   Crack correction.
C   Storey max. index.
        EMX = 0.
        IMX = 1
        DO 4 IST = 1 , NUST
          ENER = ESTO(IST)
          IF( ESTO(IST) .GT. EMX ) THEN
            EMX = ESTO(IST)
            IMX = IST
          ENDIF
    4   CONTINUE
C   Tower containing the storey max.
        ITMX(1) = INDX(1,IMX)
        ITMX(2) = INDX(2,IMX)
C   Storeys surrounding the tower max.
        CALL EBGRID( NUST,INDX,ESTO,ITMX(1),ITMX(2),CLUR)
C   Crack analysis.
        CALL EBRACK(ITMX,CLUR,ECLS,IFOT(10),CRAC,IFLG)
        IF( IFLG .GT. 0 .AND.
     +   IOPT(2) .EQ. 0 ) CORE(2) = CRAC(1) / PHOT(1)
        IF( IFLG .GE. 0 )  IFOT(14) = 10 + IFLG
C   Cracks estimators.
        DO 6 I = 3 , 5
          PHOT(I+6) = CRAC (I)
    6   CONTINUE
      ELSE
        IFLG = 0
      ENDIF
C   Transverse analysis.
      IF( IFLG .LT. 2 ) THEN
C   The longitudinal identification is valid.
C   Extract gammas.
        CALL EBTRAN( NUST,INDX,ESTO,PHOT(5),IFOT(17),PHOT(12) )
        IF( IFOT(18) .EQ. 0 ) GO TO 102
        IF( IFOT(16) .NE. 1 )THEN
          IF( IFLG .EQ. 1 ) THEN
C   Near a crack the azimuth is corrected.
            PHOT(16) = CRAC(2)
            PHOT(19) = CRAC(2)
          ENDIF
C   Saturation correction.
          CALL EBSATU(IOPT(5),IFOT(18),PHOT(14),COR5A)
          CALL EBSATU(IOPT(5),IFOT(19),PHOT(17),COR5B)
          ETOT = PHOT(14) + PHOT(17)
          PHOT(14) = PHOT(14) * COR5A
          PHOT(17) = PHOT(17) * COR5B
          IF( ETOT .GT. .0001 ) CORE(5) = (PHOT(14) + PHOT(17)) / ETOT
        ELSE
C   Hadronic clusters treatment.
          CALL EBHADR(IOPT(6),NUST,INDX,ESTO,PHOT(1),IFOT(17),PHOT(12))
          CORE(6) = ( PHOT(14) + PHOT(17) ) / PHOT(1)
        ENDIF
      ELSE
        IFOT(15) = 0
        IFOT(16) = 2
        IFOT(17) = 0
        IFOT(18) = 1
        IFOT(19) = 0
        PHOT(14) = PHOT(1)
        PHOT(15) = PHOT(5)
        PHOT(16) = CRAC(2)
        PHOT(17) = 0.
        PHOT(18) = 0.
        PHOT(19) = 0.
      ENDIF
C   Apply the energies corrections.
      CORR = CORE(1) * CORE(2) * CORE(3) * CORE(4)
      PHOT(4) = PHOT(1) * CORR
      PHOT(14) = PHOT(14) * CORR
      PHOT(17) = PHOT(17) * CORR
      CALL EBCOHD( PHOT , CORH )
      PHOT(20) = PHOT(4) * CORH
      IER = JER
   98 CONTINUE
      RETURN
C
  102 IER = 7
      GO TO 98
  104 IER = 6
      GO TO 98
      END
#endif
