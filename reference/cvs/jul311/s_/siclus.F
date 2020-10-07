      SUBROUTINE SICLUS
C----------------------------------------------------------------
C!  - Clustering in Scal
C!
C!   Author   : John Rander and B.Bloch-Devaux 911029
C!              modified 27/10/97  B.Bloch for time dependent calib
C!              modified 03/06/98  B.Bloch extend LMAXL to 400, compute
C!              quality flag for each cluster
C!
C!   Input : 'SPDA'
C!   Ouput : 'SCLS','SMAP'
C!
C!   Description:
C!   ===========
C!     A cluster consists of Pads
C!     A pad can join a cluster if it contains energy larger than low
C!     threshold and sharing a common side, edge or corner with another
C!     such pad in the cluster.
C!     A cluster must include at least one pad above high threshold.
C!     Finally the clusters below a cluster energy threshold
C!     are suppressed.
C?
C?     Create a pad map SMAP:
C?        IW(KSMAP + LMHLEN + pad_address) = SPDA row number
C?
C?     DO for all pad_data
C?        IF the pad is not already used
C?        THEN apply high threshold condition on pad
C?             Initialise attributes of a cluster
C?             Fill pad_data,cluster relations
C?             DO for all neighbour pads
C?                IF the pad may join the cluster
C?                THEN update cluster attributes
C?                     Fill pad_data,cluster relations
C?                     Start a new loop over neighbours
C?                ENDIF
C?             ENDDO
C?             Resume parent loop, if not finished
C?        ENDIF
C?     ENDDO
C?     Create SCLS
C?     Apply an energy cut on each cluster
C?     Rewrite Pad_data,cluster relation
C?     Normalise cluster attributes
C?     Correct Cluster quantities for Dead pads if any
C?     Compute partial energy sums for assymetries
C?     From 1997 correct for time dependent gain if not already done in SIPREDA
C?     Copy cluster attributes into SCLS
C?     compute quality flag for each cluster
C?     Shrink SCLS to actual size
C?
C?
C!======================================================================
#ifndef DOC
      PARAMETER(LSMAP = 12288)
#include "smapjj.h"
#include "spdajj.h"
#include "sinamc.h"
#include "sclsjj.h"
#include "srecjj.h"
#include "sibun.h"
#include "bcs.h"
      PARAMETER(JULSI=17)
#include "rcurnt.h"
#include "rlunit.h"
#include "rflags.h"
#include "alcons.h"
      PARAMETER( LMAXC = 20, LMAXL =400)
C LMAXC : Maximum number of clusters
C LMAXL : Maximum number of pads / cluster
C
      PARAMETER( NMOD=2,NSTKSI=12,NPHISI=32,NRADSI=16)
      LOGICAL KFLIP,LFLAG
      INTEGER GTSTUP
      EXTERNAL GTSTUP
      DIMENSION LIST0(LMAXL+12),LIST1(LMAXL+12)
      DIMENSION PHIC1(LMAXC),DPHIC1(LMAXC)
      DIMENSION PHIC2(LMAXC),DPHIC2(LMAXC)
      DIMENSION ECLU(LMAXC),RACLU(LMAXC),PHCLU(LMAXC),WCLU(LMAXC)
      DIMENSION DRACL(LMAXC),DPHCL(LMAXC),ZCLU(LMAXC)
      DIMENSION NPACL(LMAXC),IPCLU(LMAXC,LMAXL),NOVFL(LMAXC)
      DIMENSION ISTO(12),KVOIS(12),IVOI(3,10,2),ERLUM(5,3),PHCL(3)
      REAL SIDCOR,SIPHMOD
      EXTERNAL SIDCOR,SIPHMOD
      DIMENSION KDEAD(50),IZDEA(50),ESUM(3),DP(3) 
      DATA ISTO / 3,4,5,6,7,8,9,10,11,12,1,2/
      DATA IFOI/ 0/,EMIN2,EMIN1,EMIN3/0.030,0.250,0./
      DATA VARM / 0.23 /
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF ( IFOI.EQ.0) THEN
        IFOI = 1
C Get energy thresholds for clustering
        KSREC = IW(NASREC)
        IF ( KSREC.GT.0) THEN
          EMIN1 = RTABL(KSREC,1,JSREES)
          EMIN2 = RTABL(KSREC,1,JSREEP)
          EMIN3 = RTABL(KSREC,1,JSREEC)
        ENDIF
        IF(JDBDRF(JULSI).GE.1) WRITE(LOUTRL,777) EMIN1,
     $                         EMIN2,EMIN3
 777    FORMAT(' == SICLUS: Constants for reconstruction: ',
     $  'seed  pad  cluster  energies ',/,40X,3F6.3)
      ENDIF
C Drop cluster bank if there
      IND = NDROP('SCLS',0)
C Storey data
      KSPDA = IW(NASPDA)
      IF(KSPDA.LE.0)                               GOTO 999
      NPAD = LROWS(KSPDA)
      IF(NPAD.LE.0)                                GOTO 999
C Initialise cluster attributes
      NCLUS = 0
      CALL VZERO(NPACL(1),LMAXC)
      CALL VZERO(IPCLU(1,1),LMAXC*LMAXL)
      CALL VZERO(NOVFL(1),LMAXC)
      CALL VZERO( ECLU(1),LMAXC)
      CALL VZERO( ZCLU(1),LMAXC)
      CALL VZERO( WCLU(1),LMAXC)
      CALL VZERO(RACLU(1),LMAXC)
      CALL VZERO(PHCLU(1),LMAXC)
      CALL VZERO(DRACL(1),LMAXC)
      CALL VZERO(DPHCL(1),LMAXC)
      CALL VZERO(PHIC1(1),LMAXC)
      CALL VZERO(PHIC2(1),LMAXC)
      CALL VZERO(DPHIC1(1),LMAXC)
      CALL VZERO(DPHIC2(1),LMAXC)
C
C Create SMAP
      LEN = LMHLEN + LSMAP*LSMAPA
      CALL AUBOS('SMAP',0,LEN,KSMAP,IGARB)
      IF(IGARB.EQ.2)                               GOTO 997
      IW(KSMAP+1) = LSMAPA
      IW(KSMAP+2) = LSMAP
C put on T list
      CALL BLIST(IW,'T+','SMAP')
C restore KSPDA
      KSPDA = IW(NASPDA)
C fill SMAP
      DO 100 ITOW = 1,NPAD
        IADDR = ITABL(KSPDA,ITOW,JSPDAD)
        IW(KSMAP+LMHLEN+IADDR) = ITOW
  100 CONTINUE
C
C   Loop over all pads (to be efficient we start at stack 3 after 6 X0)
C   Pad order: Mod1(A-side)(Stack=1(IPHI=1,IR=1 to 16)(IPHI=2, etc.)
C   NPADS=2*(12*(32*(16))) = 12288
C
      DO 10 IMOD =1,NMOD
        DO 101 IST = 1, NSTKSI
          KST = ISTO(IST)
          DO 102 IPH = 1,NPHISI
            DO 103 IRA = 1,NRADSI
              IF ( NCLUS .EQ.LMAXC) GO TO 103
C - find pad number associated with indexes and find its energy
              CALL SIENCZ(IPAD,IMOD,KST,IPH,IRA)
              ITOW = ITABL(KSMAP,IPAD,1)
              IF (ITOW.GT.0) THEN
                KLL  = KROW(KSPDA,ITOW)
C                 LMAP = IW(KLL+JSPDAD)
                EPAD = RW(KLL+JSPDEN)
C====================================================================
C   STEP 1 : Find a cluster seed... 1st Pad above EMIN1
C--------------------------------------------------------------------
C==>   Apply highest minimum energy cut :  Epad > EMIN1
C--------------------------------------------------------------------
                IF(EPAD.LE.EMIN1) GO TO 103
C Tower already used ?
                IF(IW(KLL+JSPDSC).NE.0) GOTO 103
C               if NOT => Start cluster, initalize attributes
C
C=============== First pad of new cluster ===========================
                NCLUS=NCLUS+1
                IPCLU(NCLUS,1)=IPAD
                NOVFL(NCLUS)= IW(KLL+JSPDOV)+NOVFL(NCLUS)
                NPACL(NCLUS)=1
C Get pad center coordinates
      CALL SIITOR(IMOD,KST,IPH,IRA,SIZED,PHID,RADS)
C=> Weight of this pad is its energy
                WW=EPAD
                WCLU(NCLUS)=WCLU(NCLUS)+WW
                ECLU(NCLUS)=ECLU(NCLUS)+EPAD
      ZCLU(NCLUS)=ZCLU(NCLUS)+SIZED*WW
      PHIS=PHID*DEGRA
                RACLU(NCLUS)= RACLU(NCLUS)+RADS*WW
                DRACL(NCLUS)= DRACL(NCLUS)+RADS*RADS*WW
                PHIC1(NCLUS)= PHIC1(NCLUS)+PHIS*WW
                DPHIC1(NCLUS)= DPHIC1(NCLUS)+PHIS*PHIS*WW
                PHY2 = MOD(PHIS+PI,TWOPI)
                PHIC2(NCLUS)= PHIC2(NCLUS)+PHY2*WW
                DPHIC2(NCLUS)= DPHIC2(NCLUS)+PHY2*PHY2*WW
                IW(KLL+JSPDSC) = NCLUS
                LAST = ITOW
C====================================================================
C
C===>   Then Check neighboring pads : they are to be added to cluster
C       If above minimum condition and once their neighbors are verified
C--------------------------------------------------------------------
C       Get the pad's  neighbors (Fill KVOIS array)
C
                CALL SIVOIS(IPAD,KVOIS,NVOIS)
C
C==>     Now Loop over neighbors and see if they have >EMIN2 and if
C        they are already on a list... if not add them to LIST0
C----------------------------------------------------------------
                KFLIP=.TRUE.
                NLIST0=0
                DO 20 IV =  1,NVOIS
                  IDV=KVOIS(IV)
                  ITOX = ITABL(KSMAP,IDV,1)
                  IF (ITOX.EQ.0) GO TO 20
                  KLL  = KROW(KSPDA,ITOX)
                  EPAD =RW(KLL+JSPDEN)
                  IDEA =IW(KLL+JSPDSM)
C--------------------------------------------------------------------
C    (1) Apply lower minimum energy condition : E(neighbor) > EMIN2
C        or dead pad
                  IF(EPAD.GT.EMIN2 .OR. IDEA.GT.0) THEN
C    (2) Check if neighbor already in same cluster
C--------------------------------------------------------------------
                    IF(IW(KLL+JSPDSC).NE.0) GOTO 20
C     if OK => Add it to 1st temporary list
                    IF ( NLIST0.LT.LMAXL+12) THEN
                      NLIST0=NLIST0+1
                      LIST0(NLIST0)=IDV
                    ENDIF
                  ENDIF
   20           CONTINUE
C====================================================================
C   STEP 2 : Use Temporary Lists LIST0 and LIST1 of pad neighbors
C            to find all connected pads in cluster
C
C       Check if both scratch lists empty => work on cluster done
   77           IF(NLIST0.EQ.0.AND.NLIST1.EQ.0) GOTO 103
                IF (NPACL(NCLUS).EQ.LMAXL) GO TO 103
C====================================================================
C            START OF LOCAL LOOP
C Begin by looping over current list and add completed neighbors to clus
C      Start by clearing other scratch list
                IF(KFLIP) THEN
                  NLIST1=0
                  NN=NLIST0
                ELSE
                  NLIST0=0
                  NN=NLIST1
                ENDIF
C   loop over neighbors : a dead pad can be the neighbour of a hit pad
C   but cannot have a dead neighbour
                DO 30 IVN =  1,NN
                  IF(KFLIP) THEN
                    IDVN=LIST0(IVN)
                  ELSE
                    IDVN=LIST1(IVN)
                  ENDIF
                  ITAX = ITABL(KSMAP,IDVN,1)
                  IF (ITAX.EQ.0) GO TO 30
                  KLD  = KROW(KSPDA,ITAX)
                  IDED = IW(KLD+JSPDSM)
C
C  Get list of local neighbor's-neighbors and add them to NEW SCRATCH LI
C
                  CALL SIVOIS(IDVN,KVOIS,NVOIS)
C
C  Loop over local neighbor's-neighbors and check conditions 1) and 2)
                  DO 31 IVNN =  1,NVOIS
                    IDVNN=KVOIS(IVNN)
                    ITOX = ITABL(KSMAP,IDVNN,1)
                    IF (ITOX.EQ.0) GO TO 31
                    KLL  = KROW(KSPDA,ITOX)
                    EPAD =RW(KLL+JSPDEN)
                    IDEB =IW(KLL+JSPDSM)
C--------------------------------------------------------------------
C   (1) Apply lower minimum energy condition : E(neighbor) > EMIN2
C        or accept live-dead association
                    IF(IDEB.EQ.0 .AND. EPAD.GT.EMIN2) GOTO 35
                    IF(IDEB.GT.0 .AND. IDED.EQ.0) GOTO 35
                    GO TO 31
   35               CONTINUE
C   (2) Check if local neighbor's-neighbor already in any other cluster
C       or on scratch lists
C--------------------------------------------------------------------
                    IF(IW(KLL+JSPDSC).NE.0) GOTO 31
C---------------------------------------------
C   not already in a cluster; is on scratch lists?
C    if not => Add it to 2nd scratch list
C---------------------------------------------
                    LFLAG = .FALSE.
                    DO 36 I =  1,NLIST1
                      IF(IDVNN.EQ.LIST1(I)) THEN
                        LFLAG=.TRUE.
                        GOTO 37
                      ENDIF
   36               CONTINUE
   37               IF(LFLAG) GOTO 31
                    DO 38 I =  1,NLIST0
                      IF(IDVNN.EQ.LIST0(I)) THEN
                        LFLAG=.TRUE.
                        GOTO 39
                      ENDIF
   38               CONTINUE
   39               IF(LFLAG) GOTO 31
C       KFLIP is flip-flop indicating which is current work list
                    IF(KFLIP) THEN
                      IF ( NLIST1.LT.LMAXL+12) THEN
                        NLIST1=NLIST1+1
                        LIST1(NLIST1)=IDVNN
                      ENDIF
                    ELSE
                      IF ( NLIST0.LT.LMAXL+12) THEN
                        NLIST0=NLIST0+1
                        LIST0(NLIST0)=IDVNN
                      ENDIF
                    ENDIF
   31             CONTINUE
                  IF ( NPACL(NCLUS).LT.LMAXL) THEN
C
C    Add neighbour pad (member of previous scratch list) to cluster
C
C       First decode its address
C
                    CALL SIDCOZ(IDVN,MODL,ISTK,JPHI,KR)
C
C================= Additional pad added to cluster ==================
                    NPACL(NCLUS)=NPACL(NCLUS)+1
                    ITOX = ITABL(KSMAP,IDVN,1)
                    IPCLU(NCLUS,NPACL(NCLUS))=IDVN
                    KLL  = KROW(KSPDA,ITOX)
                    KLA  = KROW(KSPDA,LAST)
                    EPAD =RW(KLL+JSPDEN)
C Get pad center coordinates
      CALL SIITOR(MODL,ISTK,JPHI,KR,SIZED,PHID,RADS)
C    Pad weight is the energy
                    WW=EPAD
                    NOVFL(NCLUS)= IW(KLL+JSPDOV)+NOVFL(NCLUS)
                    WCLU(NCLUS)=WCLU(NCLUS)+WW
                    ECLU(NCLUS)=ECLU(NCLUS)+EPAD
      ZCLU(NCLUS)=ZCLU(NCLUS)+SIZED*WW
                    RACLU(NCLUS)=RACLU(NCLUS)+RADS*WW
                    DRACL(NCLUS)=DRACL(NCLUS)+RADS*RADS*WW
      PHIS=PHID*DEGRA
                    PHIC1(NCLUS)= PHIC1(NCLUS)+PHIS*WW
                    DPHIC1(NCLUS)= DPHIC1(NCLUS)+PHIS*PHIS*WW
                    PHY2 = MOD(PHIS+PI,TWOPI)
                    PHIC2(NCLUS)= PHIC2(NCLUS)+PHY2*WW
                    DPHIC2(NCLUS)= DPHIC2(NCLUS)+PHY2*PHY2*WW
                    IW(KLL+JSPDSC) = NCLUS
                    IW(KLA+JSPDSP) = ITOX
                    LAST = ITOX
                  ENDIF
C====================================================================
   30           CONTINUE
C---------------------------------------------------
C STEP 3 : Exchange first and second scratch lists
C          and loop again
C---------------------------------------------------
                IF(KFLIP) THEN
                  KFLIP=.FALSE.
                ELSE
                  KFLIP=.TRUE.
                ENDIF
C     Loop over new list
                GOTO 77
C============== go back to start of local loop  =====================
              ENDIF
  103       CONTINUE
  102     CONTINUE
  101   CONTINUE
   10 CONTINUE
C=========== OPERATIONS PERFORMED ON FINAL CLUSTERS FOUND ===========
      IF(NCLUS.LT.1) GOTO 999
C Make room for the cluster bank
      LEN = NCLUS*LSCLSA + LMHLEN
      CALL AUBOS('SCLS',0,LEN,KSCLS,IGARB)
      IF(IGARB .EQ. 2)                             GOTO 998
      IF(IGARB .NE. 0) KSPDA = IW(NASPDA)
      IW(KSCLS + LMHCOL) = LSCLSA
C     CALL BLIST(IW,'T+','SCLS')
      KSPDA = IW(NASPDA)
C For each cluster
      NCL = 0
      DO 520 JCL = 1,NCLUS
C
C If the cluster is below threshold
        IF(ECLU(JCL).LT.EMIN3) THEN
C
C Reset cluster pointer and next_storey in SPDA
          IPAD = IPCLU(JCL,1)
          ITOW = ITABL(KSMAP,IPAD,1)
          DO 501 IT=1,NPACL(JCL)
            KLSD = KROW(KSPDA,ITOW)
            IW(KLSD+JSPDSC) = 0
            ITOW = IW(KLSD+JSPDSP)
            IW(KLSD+JSPDSP) = 0
  501     CONTINUE
          GOTO 520
        ENDIF
C
C Else reorder cluster pointer and next_storey in SPDA
        NCL = NCL+1
        IPAD = IPCLU(JCL,1)
        ITOW = ITABL(KSMAP,IPAD,1)
        DO 502 IT=1,NPACL(JCL)
          KSPD = KROW(KSPDA,ITOW)
          IW(KSPD+JSPDSC) = NCL
          ITOW = IW(KSPD+JSPDSP)
  502   CONTINUE
C  Keep unnormalized quantities , may be needed later
        WCL = WCLU(JCL)
        RCL = RACLU(JCL)
        DRCL = DRACL(JCL)
        P1CL = PHIC1(JCL)
        P2CL = PHIC2(JCL)
        DP1CL = DPHIC1(JCL)
        DP2CL = DPHIC2(JCL)
        ZCL = ZCLU(JCL)
C  Renormalize cluster quantities
        EINV=0.0
        IF(WCLU(JCL).GT.0.0) EINV=1./WCLU(JCL)
        ZCLU(JCL)=ZCLU(JCL)*EINV
        RACLU(JCL)=RACLU(JCL)*EINV
        DRACL(JCL)=ABS(DRACL(JCL)*EINV-RACLU(JCL)**2 )
        PHIC1(JCL)=PHIC1(JCL)*EINV
        PHIC2(JCL)=PHIC2(JCL)*EINV
        DPHIC1(JCL)=ABS(DPHIC1(JCL)*EINV-PHIC1(JCL)**2 )
        DPHIC2(JCL)=ABS(DPHIC2(JCL)*EINV-PHIC2(JCL)**2 )
        PHIC2(JCL)=PHIC2(JCL)-PI
        IF ( DPHIC1(JCL).LT.DPHIC2(JCL)) THEN
          PHCLU(JCL) = PHIC1(JCL)
          DPHCL(JCL) = DPHIC1(JCL)
        ELSE
          PHCLU(JCL) = PHIC2(JCL)
          DPHCL(JCL) = DPHIC2(JCL)
        ENDIF
        IF(PHCLU(JCL).LT.0.0) PHCLU(JCL)=TWOPI+PHCLU(JCL)
        IF(PHCLU(JCL).GT.TWOPI) PHCLU(JCL)=PHCLU(JCL)-TWOPI
C Get pad address corresponding to barycenter
        CALL SIXTOI(RACLU(JCL),PHCLU(JCL),ZCLU(JCL),IB,JB,KB,DR,DP,IOK)
        IMD = 1
        IF ( ZCLU(JCL).LT.0.) IMD = 2
C
C   Are there any dead pads in the neighborhood? if not ,no correction
C
        CALL SIDVOI(NCL,IMD,IB,JB,KDEAD,IZDEA,NDEAD)
        IF ( NDEAD.LE.0) GO TO 521
C  for each dead pad , compute the energy sums from IPH-1 to IPH+1
C  ar R = Rdead , in layers IZ-1 to IZ +1
C  then compute dead pad energy and update cluster quantities
C  loop on dead pads
        ICORD = 0
        DO 503 ID = 1,NDEAD
           IDEAD = KDEAD(ID)
           IZDEAD = IZDEA(ID)
           CALL VZERO(ESUM,3)
C  get energy sums to correct for dead pad missing energy
           CALL SIDSUM(NCL,IDEAD,ESUM)
C  get energy correction EDEAD , Update SPDA
           EDEAD = SIDCOR(IZDEAD,ESUM)
           ITOW = ITABL(KSMAP,IDEAD,1)
           IF (ITOW.LE.0) GO TO 503
           KLL = KROW(KSPDA,ITOW)
           RW(KLL+JSPDEN) = EDEAD
C  update cluster quantities ( unnormalised) if EDEAD > Emin2
           IF ( EDEAD.LE.EMIN2 ) GO TO 503
           ICORD = ICORD + 1
           CALL SIDCOZ(IDEAD,IMD,IST,IPH,IRD)
           CALL SIITOR (IMD,IST,IPH,IRD,Z,PHI,RAD)
           PHI = PHI*DEGRA
           PHI2 = PHI+PI
           IF ( PHI2.GT.TWOPI) PHI2 = PHI2-TWOPI
           WCL = WCL+ EDEAD
           RCL = RCL+ EDEAD*RAD
           P1CL = P1CL+ EDEAD*PHI
           P2CL = P2CL+ EDEAD*PHI2
           ZCL = ZCL+ EDEAD*Z
           DRCL = DRCL+ EDEAD*RAD*RAD
           DP1CL= DP1CL+EDEAD*PHI*PHI
           DP2CL= DP2CL+EDEAD*PHI2*PHI2
 503    CONTINUE
C  Renormalize cluster quantities if corrections made
        IF ( ICORD.LE.0) GO TO 521
        EINV=0.0
        IF(WCL.GT.0.0) EINV=1./WCL
        ZCLU(JCL)=ZCL*EINV
        RCL = RCL*EINV
        ECLU(JCL)= WCL
        RACLU(JCL)=RCL
        DRACL(JCL)=ABS(DRCL*EINV-RCL**2 )
        P1CL = P1CL*EINV
        P2CL = P2CL*EINV
        PHIC1(JCL)= P1CL
        PHIC2(JCL)= P2CL
        DPHIC1(JCL)=ABS(DP1CL*EINV-P1CL**2 )
        DPHIC2(JCL)=ABS(DP2CL*EINV-P2CL**2 )
        PHIC2(JCL)=PHIC2(JCL)-PI
        IF ( DPHIC1(JCL).LT.DPHIC2(JCL)) THEN
          PHCLU(JCL) = PHIC1(JCL)
          DPHCL(JCL) = DPHIC1(JCL)
        ELSE
          PHCLU(JCL) = PHIC2(JCL)
          DPHCL(JCL) = DPHIC2(JCL)
        ENDIF
        IF(PHCLU(JCL).LT.0.0) PHCLU(JCL)=TWOPI+PHCLU(JCL)
        IF(PHCLU(JCL).GT.TWOPI) PHCLU(JCL)=PHCLU(JCL)-TWOPI
 521  CONTINUE
C
C Fill SCLS data
C
C Special 1997 and later,in case the bunch dependent calib was not applied
       ibunf =0
       IF ( GTSTUP('SI',IRUNRC).GE.7) THEN
          if ( ibune.le.0) then
C renormalize cluster and corresponding pads energies if not yet done 
             ibunf = 1
             if ( dracl(jcl).lt.varm ) ibunf = 2
             ibune = ibunf
             eclub = 0.
C             eclu(jcl) = eclu(jcl)*ecorbun(imd,ibunf)
             IPAD  = ITABL(KSMAP,IPCLU(JCL,1),1)
 541         CONTINUE
             KSP = KROW(KSPDA,IPAD)
             IA = ITABL(KSPDA,IPAD,JSPDAD)
             CALL SIDCOZ(IA,MODU,IST,IPH,IRD)
             phcor = 1.
             if ( ibunf.eq.2) then
C  get bunch 1 correction aplied, bunch 2 correction to be applied
               ibune = 0
               phcor1 = siphmod(MODU,IPH)
               ibune = ibunf
               phcor2 = siphmod(MODU,IPH)
               phcor = phcor2/phcor1
             endif
             ecorb = RW(KSP+JSPDEN)*phcor*ecorbun(imd,ibunf)
             RW(KSP+JSPDEN) = ecorb
             eclub = eclub + ecorb
             IPAD  = ITABL(KSPDA,IPAD,JSPDSP)
             IF (IPAD.GT.0) GO TO 541
             eclu(jcl) = eclub
             ibune = 0 
          endif
       ENDIF
      IW(KSCLS + LMHROW) = NCL
        KCLU = KROW(KSCLS,NCL)
        RW(KCLU+JSCLEC) = ECLU(JCL)
        RW(KCLU+JSCLRC) = RACLU(JCL)
        RW(KCLU+JSCLZC) = ZCLU(JCL)
        RW(KCLU+JSCLPC) = PHCLU(JCL)
        XX =ATAN(RACLU(JCL)/ZCLU(JCL))
        IF (XX.LT.0.) XX = XX+PI
        RW(KCLU+JSCLTC) =  XX
        RW(KCLU+JSCLWP) = DPHCL(JCL)
        RW(KCLU+JSCLWR) = DRACL(JCL)
        IW(KCLU+JSCLSP) = ITABL(KSMAP,IPCLU(JCL,1),1)
        IW(KCLU+JSCLOV) = NOVFL(JCL)
C     Correct Radius and Phi for pad curvature and barycenter method
        RAD = RACLU(JCL)
        PHI = PHCLU(JCL)
C        IMD = IFIX(0.5*(3.-SIGN(1.,ZCLU(JCL))))
CCC   CALL SICLCO(RAD,PHI,ZCLU(JCL),IR,JP,ISZ)
C convert to local indices
          CALL SIXTOI(RAD ,PHI ,ZCLU(JCL),IRD,IPH,IST,DRA,DP ,IOK)
C Get triplet
          ITP = MOD(IST-1,3)+1
          DPH = DP(ITP)
C move to local system in SICAL from ARS
          CALL SIXTOL(IRD,IPH,IST,IMD,DRA,DPH,RLO,PLO,ZLO)
C apply correction to r and phi
          CALL SICLC2 ( RLO,PLO,DRA,DPH,RCOR,PCOR)
C move to ARS system from SICAL
          CALL SILTOX ( RCOR,PCOR,IST,IMD,RAD, PHI)
        RW(KCLU+JSCLCR) = RAD
        RW(KCLU+JSCLCP) = PHI
        XX =ATAN(RAD/ZCLU(JCL))
        IF (XX.LT.0.) XX = XX+PI
        RW(KCLU+JSCLCT) =  XX
C      Get uncorrected energy
      CALL SIESUM(JCL,ERAW,2)
      RW(KCLU+JSCLER) =  ERAW
C Prepare partial Energy sums for Luminosity cuts
      CALL SIXTOI(RAD,PHI,ZCLU(JCL),IR,JP,ISZ,RACL,PHCL,IOK)
      IF ( IR.GT.0) THEN
        IF ( IR.GT.1) IW(KCLU+JSCLR3) = IR-1
        IW(KCLU+JSCLR3+1) = IR
        IF ( IR.LT.16) IW(KCLU+JSCLR3+2) = IR+1
        CALL SILVOI(IMD,IR,JP,IVOI)
        CALL SILSUM(NCL,IVOI,ERLUM)
        RW(KCLU+JSCLE1  ) = ERLUM(1,1)
        RW(KCLU+JSCLE1+1) = ERLUM(2,1)
        RW(KCLU+JSCLE1+2) = ERLUM(3,1)
        RW(KCLU+JSCLE1+3) = ERLUM(4,1)
        RW(KCLU+JSCLE1+4) = ERLUM(5,1)
        RW(KCLU+JSCLE2  ) = ERLUM(1,2)
        RW(KCLU+JSCLE2+1) = ERLUM(2,2)
        RW(KCLU+JSCLE2+2) = ERLUM(3,2)
        RW(KCLU+JSCLE2+3) = ERLUM(4,2)
        RW(KCLU+JSCLE2+4) = ERLUM(5,2)
        RW(KCLU+JSCLE3  ) = ERLUM(1,3)
        RW(KCLU+JSCLE3+1) = ERLUM(2,3)
        RW(KCLU+JSCLE3+2) = ERLUM(3,3)
        RW(KCLU+JSCLE3+3) = ERLUM(4,3)
        RW(KCLU+JSCLE3+4) = ERLUM(5,3)
      ENDIF
  520 CONTINUE
C Shrink SCLS to real size
      IW(KSCLS + LMHROW) = NCL
      LEN = NCL*LSCLSA + LMHLEN
      CALL AUBOS('SCLS',0,LEN,KSCLS,IGARB)
      IF(IGARB .EQ. 2)                              GOTO 998
C check for quality flag
      CALL SICLEAN(0,ifl)
C====================================================================
      GO TO 999
  997 CALL RERROR ('SICLUS', -1, 'no room for SMAP')
      GO TO 999
  998 CALL RERROR ('SICLUS', -2, 'no room for SCLS')
  999 RETURN
      END
#endif
