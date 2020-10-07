*DK tsdedx
      SUBROUTINE TSDEDX(NCLUS)
C-----------------------------------------------------------------------
C!  Routine to return the clusters formed along a track element
C
C  Called from: TSTEER
C  Calls:       WBANK, TBLOCH, RNDM, AUGXYZ, TPRIMA,
C               TPNCLU, TPDMOM, TPDPAR, RANNOR
C
C  Inputs:   /TRAKEL/     --Primary track parameters for this track elt.
C            /TPCONS/     --POIMAX, the cutoff for treating single
C                                   primaries
C                         --CKNMIN, Ar ionization energy
C                         --WRKFUN, Ar work function
C                         --CFANO,  Fano factor for finding cluster
C                                   size variation
C                         --DELCLU, step size for delta simulation
C            /TPGEOM/     --ZTPCMX, the position of the TPC endplate
C
C  Outputs:  PASSED:      --NCLUS,  number of clusters formed
C                         --IRET,   return code indicating whether
C                                   track was processed
C            BANKS:       --id IDCLUS, cluster bank
C                           IW(IDCLUS+1) = num words per cluster
C                           IW(IDCLUS+2) = num of clusters
C                           KIDCL = IDCLUS + 2 + (KCLUS-1)*IW(IDCLUS+1)
C                              RW(KIDCL+1) = X of cluster
C                              RW(KIDCL+2) = Y of cluster
C                              RW(KIDCL+3) = Z of cluster
C                              RW(KIDCL+4) = time of cluster
C                              IW(KIDCL+5) = num of electrons in cluster
C-----------------------------------------------------------------------
C
C Modifications :
C         1. P. Janot   23 Mar 1988
C            Also process charged particles with |charge| > 1.
C         2. D. Cowen    6 Sep 1988
C            Use R. Johnson's dEdx function TBLOCH instead of TPAVIP.
C         3. D. Cowen    1 Nov 1988
C            Add Mer's correction for the determination of the time
C            of delta rays.
C         4. D. Cowen    22Feb 1989
C            Add error return for WBANK calls.
C ----------------------------------------------------------------------
C
#include "bcs.h"
#include "alcons.h"
#include "trakel.h"
#include "tpcbos.h"
#include "hiscom.h"
#include "tpcond.h"
#include "tpcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
C
      LOGICAL LDBD1,LDBD2,LDBD3,LDBDF
C
      DIMENSION XX(3),CC(3),PRMVC(3)
C
      DATA ICALL,IPRCL/0,0/
      DATA MXCLDF,MXCLEX,NWPRCL/10000,5000,5/
C
C  Debug levels
C
      ICALL = ICALL + 1
      LDBD1 = ( NTPCDD .GE. 1 .AND. ICALL .LE. NCALDD )
      LDBD2 = ( NTPCDD .GE. 2 .AND. ICALL .LE. NCALDD )
      LDBD3 = ( NTPCDD .GE. 3 .AND. ICALL .LE. NCALDD )
C
      IF ( LDBD2 ) WRITE(6,100) (X(I),VECT(I),I=1,3),SEGLEN,TOF,
     *                            ABSMOM,AMASS,CHARGE,NTRK
C
C  Open the BOS work bank which will contain the cluster information.
C  Number of words = 2 + no. words/cluster * default no. of clusters.
C
      MXCLUS = MXCLDF
      NDWRDS = 2 + NWPRCL*MXCLUS
      CALL WBANK(IW,IDCLUS,NDWRDS,*999)
C
C-----------------------------------------------------------------------
C
C  First get the parameters to do dedx for this track ( bubble 4.3.1.1 )
C
C-----------------------------------------------------------------------
C
C
C  Get gamma and beta of the track.
C
      GAMMA = SQRT(1. + ABSMOM*ABSMOM/(AMASS*AMASS))
      BETA =  SQRT( GAMMA*GAMMA - 1. ) / GAMMA
      ZBETASQ = (CHARGE**2/BETA**2)
      IF(ZBETASQ.GT.100.)ZBETASQ = 100.
C
C  Get Poisson average for number of primary ionizing interactions per
C  unit length
C
      BG     = BETA*GAMMA
      POISAV = TBLOCH(BG) * CHARGE**2
C
      IF ( LDBD3 ) WRITE(6,101) POISAV
      IF ( LDBD1 ) CALL HF2(IHDEDX+1,ALOG10(GAMMA),POISAV,1.)
C
C  For the case in which the Poisson average is too large, we rescale
C  the number of electrons which make up a primary.  The size of the
C  cluster generated is then multiplied by the factor with which we had
C  to reduce the Poisson average.
C
      NEL = INT(POISAV/POIMAX + 1.)
      POISAV = POISAV/FLOAT(NEL)
C
C-----------------------------------------------------------------------
C
C  Generate primary interactions ( bubble 4.3.1.2 )
C
C-----------------------------------------------------------------------
C
C  TOTDIS keeps track of how far we've gone from the start point.
C  NPRIM keeps track of which primary we have
C  NCLUS keeps track of the cluster number.
C
      TOTDIS = 0.
      NPRIM = 0
      NCLUS = 0
      LDBDF = LDBD1
C
C  Start processing a primary
C
 1    NPRIM = NPRIM + 1
      IPRCL = IPRCL + 1
C
      LDBD1 = ( LDBD1 .AND. IPRCL .LE. NCALDD )
      LDBD3 = ( LDBD3 .AND. IPRCL .LE. NCALDD )
C
C  Get the distance from the last primary generated and the total path
C  length up to this point.
C
      DIST =  -ALOG(RNDM(R))/POISAV
      TOTDIS = TOTDIS + DIST
      IF ( LDBD3 ) WRITE(6,102) DIST,TOTDIS
C
C  If we've exhausted the allowed length, fill in the cluster bank
C  header, close it off, and quit
C
 2    IF ( TOTDIS .GT. SEGLEN ) THEN
C
           IF ( LDBD2 ) WRITE(6,115) (TOTDIS-DIST),DIST,SEGLEN,NCLUS,
     *        (XX(K),K=1,3),(CC(K),K=1,3)
           IF ( LDBDF )
     *        CALL HF2(IHDEDX+5,ALOG10(ABSMOM),FLOAT(NCLUS)/SEGLEN,1.)
C
           IW(IDCLUS+1) = NWPRCL
           IW(IDCLUS+2) = NCLUS
C
           NDWRDS = 2 + NWPRCL*NCLUS
           CALL WBANK(IW,IDCLUS,NDWRDS,*999)
C
           RETURN
C
      ENDIF
C
C  Get the position XX at which the primary was formed and the direction
C  cosines CC of the track element at that position
C
           CALL AUGXYZ(X,VECT,ABSMOM,CHARGE,TOTDIS,CC,XX)
           IF ( LDBD3 ) WRITE(6,108) (XX(K),K=1,3),(CC(K),K=1,3)
C
C  Check to see that we're still in the TPC
C
           IF ( ABS(XX(3)) .GE. ZTPCMX ) THEN
              TOTDIS = SEGLEN + 1.
              GOTO 2
           ENDIF
C
C  Determine the energy of the primary.  If it is below the cutoff for
C  consideration of a delta ray, we set PRIMaryEnergy = 0 and calculate
C  cluster size directly; otherwise, we keep the calculated value of
C  PRIME and treat this primary in detail.
C
         PRIME = TPRIMA(POISAV,ZBETASQ)
C
C-----------------------------------------------------------------------
C
C  Form clusters ( bubble 4.3.1.3 )
C
C-----------------------------------------------------------------------
C
C  If this primary is not a delta,
C  position a cluster at the creation point of the primary with cluster
C  size independent of the primary energy and rescaled according to the
C  number of non-delta electrons in the primary.
C
      IF ( PRIME .EQ. 0. ) THEN
C
           NCLUS = NCLUS + 1
           IF ( LDBD3 ) WRITE(6,105) NCLUS
C
C  Check to see if we need to extend BOS bank
C
           IF ( NCLUS .GT. MXCLUS ) THEN
              MXCLUS = MXCLUS + MXCLEX
              NDWRDS = 2 + NWPRCL*MXCLUS
              CALL WBANK(IW,IDCLUS,NDWRDS,*999)
           ENDIF
C
           INDEX = IDCLUS + 2 + (NCLUS-1)*5
C
C  Put the cluster at the position of the primary
C
           RW(INDEX+1) = XX(1)
           RW(INDEX+2) = XX(2)
           RW(INDEX+3) = XX(3)
C
C  Get the time for this cluster.
C
           RW(INDEX+4) = TOF + TOTDIS/(CLGHT*BETA)
           IF ( LDBD3 ) WRITE(6,107) RW(INDEX+4)
C
C  Get cluster size and rescale
C
           CALL TPNCLU(IW(INDEX+5))
           IW(INDEX+5) = NEL*IW(INDEX+5)
C
           IF ( LDBD1 ) CALL HF1(IHDEDX+6,FLOAT(IW(INDEX+5)),1.)
           IF ( LDBD3 ) WRITE(6,106) IW(INDEX+5)
C
      ELSE
C
C  ******************** D E L T A - R A Y S ****************************
C
C  Get the momentum of the delta from kinematics
C
         CALL TPDMOM(ABSMOM,CC,AMASS,GAMMA,PRIME,PRIMP,PRMVC)
C
         IF ( LDBD3 ) WRITE(6,150)
         IF ( LDBD3 ) WRITE(6,103) PRIMP,PRIME,(PRMVC(J),J=1,3)
         IF ( LDBD1 ) CALL HF1(IHDEDX+2,ALOG10(PRIME),1.)
C
C  Our delta-ray model is to assume that the clusters formed by the
C  passage of the delta will be formed on a cylinder whose radius DLRAD
C  and length DLZLN depend on the delta ray energy and momentum (and
C  some ad hoc assumptions).
C  DLBET = average beta=(v/c) for this primary
C
         CALL TPDPAR(PRIMP,PRMVC,DLRAD,DLZLN,DLBET)
C
         IF ( LDBD1 ) THEN
            CALL HF1(IHDEDX+3,DLZLN*SIGN(1.,PRMVC(3)),1.)
            CALL HF1(IHDEDX+4,DLRAD,1.)
            IF ( LDBD3 ) WRITE(6,109) DLRAD,DLZLN,1./PRMVC(3),DLBET
         ENDIF
C
C  We wish to take steps of length DELCLU in Z, and deposit the correct
C  fraction of the delta-ray energy at each step.  If the deposited
C  energy for steps of length DELCLU is less than the ionization
C  potential CKNMIN, we adjust the step size up so that this delta will
C  actually generate some clusters.  We end up with a step length DLSTP
C  and energy dep/step (i.e., per cluster) DLCLE
C
C  If the delta path length along z is smaller than DELCLU the make z pa
C  length equal to DELCLU so that all the energy is deposited in a singl
C  cluster.
C
         IF (DLZLN.LT.DELCLU) DLZLN = DELCLU
         DLCLE = (PRIME/DLZLN)*DELCLU
         DLSTP = DELCLU
C
         IF ( DLCLE .LT. CKNMIN ) THEN
            DLCLE = 2.0*CKNMIN
            DLSTP = DLCLE*DLZLN/PRIME
         ENDIF
C
         IF ( LDBD3 ) WRITE(6,110) DLCLE
C
C  Set num of clusters from delta and length of delta Z-path covered
C
         NDLCL = 0
         DLCLN = 0.
C
C  Now actually start getting clusters from this delta
C
 21      NDLCL = NDLCL + 1
         NCLUS = NCLUS + 1
         IF ( LDBD3 ) WRITE(6,105) NCLUS
C
C  Check to see if we need to extend BOS bank
C
         IF ( NCLUS .GT. MXCLUS ) THEN
            MXCLUS = MXCLUS + MXCLEX
            NDWRDS = 2 + NWPRCL*MXCLUS
            CALL WBANK(IW,IDCLUS,NDWRDS,*999)
         ENDIF
C
         INDEX = IDCLUS + 2 + (NCLUS-1)*5
C
C  Take a step along path of delta
C
         DLCLN = DLCLN + DLSTP
         IF ( LDBD3 ) WRITE(6,111) DLCLN
C
C  If the step will overshoot the cylinder length, rescale the cluster
C  size so that this last cluster will take up the remainder of the
C  energy of the delta.
C
         IF ( DLCLN .GT. DLZLN ) THEN
            DLCLE = DLCLE*((DLZLN-DLCLN)/DLSTP + 1.)
            DLCLN = DLZLN
         ENDIF
C
C  Get the cluster position.  In the X-Y plane, we generate randomly a
C  point on a circle of radius DLRAD and using this point as a
C  displacement from the position of the primary's formation.  In the
C  Z-direction, we take the appropriate step.
C
         PHIDC =  TWOPI*RNDM(Q)
         RW(INDEX+1) = XX(1) + DLRAD*COS(PHIDC)
         RW(INDEX+2) = XX(2) + DLRAD*SIN(PHIDC)
C
         RW(INDEX+3) = XX(3) + DLCLN*SIGN(1.,PRMVC(3))
C
         IF ( LDBD3 ) WRITE(6,113) (RW(INDEX+J),J=1,3)
C
C  Check that we're still in the TPC
C
         IF ( ABS(RW(INDEX+3)) .GT. ZTPCMX ) THEN
            NCLUS = NCLUS - 1
            GOTO 22
         ENDIF
C
C  Get the time of formation of the cluster
C
         DTOCL = TOTDIS + ABS( DLCLN/PRMVC(3) )
         RW(INDEX+4) = TOF + TOTDIS/(CLGHT*DLBET) + ABS(DLCLN/PRMVC(3))
         IF ( LDBD3 ) WRITE(6,107) RW(INDEX+4)
C
C  Get cluster size.  This is given by the total number
C  of electrons produced (computed from the work function)
C
         CLUSIZ = (DLCLE - CKNMIN)/WRKFUN
C
C  Find the number of electrons in the cluster.  If this primary has
C  lost so much energy that it can no longer form secondaries, consider
C  the primary as the last cluster.  Otherwise, compute the size of the
C  cluster from the Fano formula.
C
         IF ( CLUSIZ .LE. 0 ) THEN
            IW(INDEX+5) = NEL
C
         ELSE
C
            CALL RANNOR(PN,DUM)
C
            PN = PN*SQRT(CFANO*CLUSIZ)
            IW(INDEX+5) = NEL * MAX( INT(CLUSIZ + PN + .5), 0 )
C
         ENDIF
C
           IF ( LDBD3 ) WRITE(6,106) IW(INDEX+5)
           IF ( LDBD1 ) CALL HF1(IHDEDX+6,FLOAT(IW(INDEX+5)),1.)
C
C  Get next cluster for this delta ray
C
           IF ( DLCLN .NE. DLZLN ) GOTO 21
C
C  If we get here, we've exceeded the cylinder length; i.e., we're done
C  with this delta.
C
 22        IF ( LDBD3 ) WRITE(6,114) DLCLN,NDLCL,DLZLN,
     *                   DLRAD,(RW(INDEX+K),K=1,3)
C
      ENDIF
C
C  Go and generate the next primary.
C
      GOTO 1
C
C-----------------------------------------------------------------------
 100  FORMAT(//,'              START TRACK ELEMENT',
     *       //,' DEDX STARTS WITH   X(I) ,  COSX(I) ',3(/,18X,2(F9.3)),
     *       /, ' TRACK ELEMENT LENGTH (cm)  : ',F8.3,
     *       /, ' TIME OF FLIGHT   (nanosec) : ',E12.6,
     *       /, ' MOMENTUM     (Mev)         : ',F9.3,
     *       /, ' MASS (Mev), CHARGE , TRACK : ',F8.3,2X,F4.1,2X,I4)
 101  FORMAT(/, ' POISSON MEAN               : ',F8.3)
 102  FORMAT(/, ' DISTANCE TO PRIMARY (cm)   : ',F8.5,
     *       /, ' TOTAL DISTANCE  (cm)       : ',F8.4)
 103  FORMAT(/, ' PRIMARY P , E  (Mev)       : ',2(E11.5,2X),
     *       /, ' MOMENTUM VECTOR : ',3(2X,F8.4))
 105  FORMAT(/, ' CLUSTER NUMBER             : ',I6)
 106  FORMAT(/, ' CLUSTER SIZE               : ',I6)
 107  FORMAT(/, ' CLUSTER CREATION TIME      : ',F8.5)
 108  FORMAT(/, ' COORDS OF PRIMARY  (cm)    : ',3(F10.4,2X),
     *       /, ' DIRECTION OF TRACK         : ',3(F10.4,2X))
 109  FORMAT(/, ' RADIUS FOR DELTA DIST (cm) : ',F7.4,
     *       /, ' LENGTH FOR DELTA DIST (cm) : ',F8.4,
     *       /, ' DL/DZ FOR DELTA            : ',F8.3,
     *       /, ' AVERAGE GAMMA OF DELTA     : ',F8.3)
 110  FORMAT(/, ' ENERGY LOST/CLUSTER  (Mev) : ',F8.6)
 111  FORMAT(/, ' Z LENGTH OF DELTA    (cm)  : ',F8.4)
 112  FORMAT(/, ' CLUSTER CIRCLE COORDS      : ',2(F9.4,2X))
 113  FORMAT(/, ' CLUSTER COORDINATES        : ',3(F10.4,2X))
 114  FORMAT(//,'            FINAL NUMBERS FOR DELTA-RAY',
     *       //,' TOTAL LENGTH   (cm)        : ',F8.3,
     *       /, ' NUMBER OF CLUSTERS         : ',I6,
     *       /, ' CYLINDER LENGTH (cm)       : ',F8.3,
     *       /, ' CYLINDER RADIUS (cm)       : ',F8.4,
     *       /, ' LAST CLUSTER COORDS        : ',3(F10.4,2X))
 115  FORMAT(//,'            FINAL NUMBERS FOR TRACK ELEMENT',
     *       //,' LAST PRIMARY LENGTH (cm)   : ',F9.4,
     *       /, ' DISTANCE TO NEXT PRIM (cm) : ',F9.4,
     *       /, ' ELEMENT LENGTH   (cm)      : ',F9.4,
     *       /, ' NUMBER OF CLUSTERS         : ',I6,
     *       /, ' LAST PRIMARY COORDS        : ',3(F10.4,2X),
     *       /, ' DIR COSINES AT END         : ',3(F10.4,2X))
 150  FORMAT(//, '               START  DELTA-RAY',/)
C_______________________________________________________________________
C
  999 WRITE(6,'(/'' +++TSDEDX+++ Insufficient BOS array space!''/)')
      RETURN
      END
