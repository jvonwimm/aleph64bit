      SUBROUTINE GAMPEX(IPECO,EMIGA,LTOTGA,NNGA,GAMVE,GAMCE,IRTFG)
C.----------------------------------------------------------------------
CKEY GAMPACK PHOTON PECO / USER
C     J.C.Brient  - A.Rouge   Creation 1/10/91
C! Finds photon in PECO cluster
C  Reconstruct photon with A.Rouge algorithm (peak finding)
C   Input :
C            IPECO is the PECO cluster number
C            EMIGA is the min. energy for a photon
C            LTOTGA is the total length of GAMVE
C                   in the user program:
C                   GAMVE should be dimensioned to (20,NPHOT) and
C                   GAMCE should be dimensioned to (20,NPHOT) and
C                   LTOTGA = 20*NPHOT
C                   in GAMPEK: GAMVE is dimensioned to (20,*)
C                   the maximum number of photons will be: LTOTGA/20
C   Output:
C
C            NNGA  Number of photons
C
C            GAMVE(I  , Photon number)  Photon Array
C                 (1,     Energy    |   Best estimates
C                 (2,     Theta     |   assuming only
C                 (3,     Phi       |   1 gamma
C                 (4,     Ad-hoc  energy correction (multiplicative)
C                 (5,     FLAG border region endcap
C                 (6,     F4
C                 (7,     Normalized estim. for substructure
C                 (8,     FLOAT( NST1 + 100*NST2 + 10000*NST3)
C                         where  nstX is the # of storey in stack X
C                 (9,     Estack1/ETOT
C                 (10,    Estack2/ETOT
C                 (11,    FLOAT( MORS1 + 10*MORS2 + 100*MORS3)
C                         where  morsX is = 1 if there a dead storey
C                         in central 3 x 3 matrix in stack X
C                 (12,    Eraw
C                 (13,    EBNEUT flag ECAL crack
C                 (14,    itheta peak
C                 (15,    jphi   peak
C                 (16,    min.dist. to ch.track with barycen.
C                 (17,    theta  barycenter  (from the vertex)
C                 (18,    phi    barycenter  (from the vertex)
C                 (19,    radius of the shower from barycenter
C                 (20,    -1 for isolated germination storeys
C                         Egerm(storey)-Eneighb./sqrt(Eneighb.)
C
C            GAMCE(I  ,   Cluster number)  Output array from BULOS
C                 (1,     Small sigma of cluster in Eigenframe
C                 (2,     Large sigma of cluster in Eigenframe
C                 (3,     Third moment of cluster in direction of ALAMS
C                 (4,     Third moment of cluster in direction of ALAML
C                 (5,     Mass of cluster if two photon cluster
C               * Item 6-8 not yet implemented
C                 (6,     Corrected small sigma for one photon cluster
C                 (7,     Corrected large sigma for one photon cluster
C                 (8,     Corrected mass of cluster for one photon
C                 (9,     QE of 1st Gam for two photon cluster
C                (10,     QX of 1st Gam for two photon cluster
C                (11,     QY of 1st Gam for two photon cluster
C                (12,     QZ of 1st Gam for two photon cluster
C                (13,     QE of 2nd Gam for two photon cluster
C                (14,     QX of 2nd Gam for two photon cluster
C                (15,     QY of 2nd Gam for two photon cluster
C                (16,     QZ of 2nd Gam for two photon cluster
C                (17,     float(Error code)
C                (18,     float(Warning code)
C                (19,
C                (20,     gamma-gamma mass
C
C
C          IRTFG  Return code
C                 =  1 OK but .GE. 1 storey number out of range
C                 =  0 EVERYTHING OK
C                 = -1 missing bank (PEST,ETDI,PCRL,PECO)
C                 = -2 # of PFRF track > NXM
C                 = -3 # of storeys on cluster PECO > NSTRMX
C                 = -4 # of cluster found > NKLUMX
C                 = -5 # of gammas  found > NFOMAX
C                 = -6 1 gamma contain > NSTGAX
C                 = -7 ECAL geometry package not initialized before
C                      calling GAMPEK
C                 = -8 problem in EBNEUT initialization
C
C   Calls: BGETDS,EBCDRG,ECLEAK,GXTRAN,GAFORM,GAGRID
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "pestjj.h"
#include "etdijj.h"
#include "pfrfjj.h"
#include "deadst.h"
#include "comcux.h"
#include "ecoxa.h"
#include "gafakp.h"
#include "gacorm.h"
#include "gastin.h"
      PARAMETER ( IDPK = 2 ** 16 )
      PARAMETER ( NXM    = 200 )
      PARAMETER ( PSATU  = 0.00078)
C --- max number of STOREYS per PECO cluster
      PARAMETER ( NSTRMX = 1000   )
      PARAMETER ( NST3RX = 3*NSTRMX)
C --- max number of cluster per PECO cluster
      PARAMETER ( NKLUMX = 50     )
C --- max number of STOREYS per PHOTON
      PARAMETER ( NSTGAX = 500    )
C
#include "alcons.h"
C input dim
      PARAMETER (NCOLGA = 20)
      DIMENSION GAMVE(NCOLGA,*)
      DIMENSION GAMCE(NCOLGA,*)
C
C--for EBCDRG
      DIMENSION IKOD(9) , BARY(2)
C
C for GAGRID
      DIMENSION LISTOR(3,NSTGAX),ESTOR(NSTGAX)
      DIMENSION LST33(3,6,3)
C
C for GXMCLM
      DIMENSION LCLMST(NSTGAX) , CMINFO(16)
C
C for EBGRID
      DIMENSION GRILE(3,3,3)
C
C for GXTRAN
      DIMENSION ES33(3,3)
C
C--for GAFORM
      DIMENSION LSTK(3,NSTGAX) , ESTK(NSTGAX)
C
C--for ECLEAK
      DIMENSION STACE(3),EVLP(5)

C arrays ch.track
      DIMENSION  IPTPRF(NXM)  , LTRP(NXM) , POINT(3) , QEXP(6,NXM)
C
C arrays storeys
      DIMENSION  ESTO1(9,NSTRMX) , ISTO1(9,NSTRMX)
      DIMENSION  ESTO2(9,NSTRMX) , ISTO2(9,NSTRMX)
      DIMENSION  ESTO3(9,NSTRMX) , ISTO3(9,NSTRMX)
      DIMENSION  INDEX(3,NST3RX) , ENDEX(NST3RX)
      DIMENSION  LSTGA(NST3RX)
      EQUIVALENCE(ESTO1,ISTO1) , (ESTO2,ISTO2) , (ESTO3,ISTO3)
C
C arrays clusters
      DIMENSION NSTCL (NKLUMX)   ,  ISTCL(NKLUMX), IGERM(4,NKLUMX)
      DIMENSION KCLNOK(NKLUMX)   ,  QGAM(4,NKLUMX)
      DIMENSION CLUST(24,NKLUMX) ,  ICLUST(24,NKLUMX)
      DIMENSION RAIE(NKLUMX)     ,  EBAR12(NKLUMX)
      EQUIVALENCE (CLUST,ICLUST)
C arrays fake
      DIMENSION  ESTO (9) ,        ISTO (9)
      DIMENSION  EESTO(9) ,        IISTO(9)
      EQUIVALENCE(ESTO ,ISTO )
      EQUIVALENCE(EESTO,IISTO)
C arrays storeys pour memorisation voisinage chaque storey :
      DIMENSION INDSTO(2,3*NST3RX)
      LOGICAL    YETSEE(NST3RX*3),FRONTI , ARISTO
      LOGICAL    INTOF4
      LOGICAL    DANSF4
C Numeros PEST de storeys dans la grille EBGRID
      DIMENSION  KGRID(3,3,3)
      DIMENSION ET33(3,3)
C Numeros PEST de storeys dans les quatres tours centrales
      DIMENSION KF4GRI(2,2,3) , KF4TAB(2,2,3)
C --
C --
      LOGICAL   GAMOK(NKLUMX)
C Memorisation storeys dans matrice 3*3 autour du pic dans 4 tours cent:
      DIMENSION KLUM33(3,3,3,NKLUMX),KLUM22(2,2,3,NKLUMX)
C Arguments pour calcul de fake EM:
      DIMENSION TABEM (17,NFPHOT),TABOUT(6,NFPHOT)
      EQUIVALENCE (XDUM,IDUM)
C Arguments de passage pour fake hadronique:
      DIMENSION TABHAD(19)
c Storeys sur la ligne de vol de la (des) trace(s);
      PARAMETER (NMXFIR=50)
      INTEGER STOFIR(3,NMXFIR)
C --
C -----------------------------------------------------------------
C     Clusterization arrays
C------------------------------------------------------------------
C  Storeys
C----------
C            ESTO1~ISTO1   logical stack1
C            ESTO2~ISTO2   logical stack2
C            ESTO3~ISTO3   logical stack3
C
C  first index :
C
C   1      energy
C   2      x barycenter
C   3      y barycenter
C   4      z barycenter
C   5      IT  theta #
C   6      JF  phi   #
C   7      ST  physical stack #
C   8      cluster #
C   9      pointer to PEST
C
C   Clusters :
C-------------
C
C     CLUST~ICLUST
C
C    first index :
C
C    1  x barycenter stack 1
C    2  y     ""  ""
C    3  z
C    4  energy     stack 1
C
C    5 6 7 8  idem stack 2
C
C    9 10 11 12  idem stack 3
C
C    13 14 15 16  idem whole cluster
C
C    17 flag : 1 charged  2 overlap
C
C    18 19 20  storeys $ stacks 1 2 3
C
C    21 stack 1 barycenter distance to the nearest charged track
C
C    22  idem stack 2
C
C    23  idem stack 3
C    24  idem whole cluster
C
C    Quality flag preparation
C-----------------------------
C
C      IGERM
C
C    first index
C
C
C    1  index of the highest energy storey stack 1
C
C    2  index of the highest energy storey stack 2
C
C    3  index of the highest energy storey stack 3
C
C    4  stack number of the germination storey
C
C
C  Preparation of overlap flag
C-----------------------------
C
C    EBAR12
C
C    Energy in barrel  stacks 1 + 2
C
C arrays photons
C---------------
C
C   QGAM     px py pz e (origin at vertex)
C
C  vertex
      DIMENSION   DELTA (3)
C
C arrays reconstruction parameters
      DIMENSION CUTECL(3)
C
C arrays photon direction
      DIMENSION PNGE(3) , PCOGR(3)
      DIMENSION LBOREG(6)
C
      SAVE NFOMAX
      LOGICAL VOISTO
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C DATA FOR ECLEAK
      DATA MTC/0/
      DATA STACE/0.,0.,0./
      DATA LBOREG /8,9,24,25,40,41/
#include "bmacro.h"
C
C Initializations
C
      IF(FIRST) THEN
        JECGN  = IW(NAMIND('ECGN'))
        IF ( JECGN.LE.0 ) THEN
          IRTFG = - 7
          GOTO 9999
        ENDIF
C
C --- max number of photon per peco cluster
C
        NFOMAX = LTOTGA / NCOLGA
C
C
C     EBNEUT Initialization
C
      CALL EBINIT(JBADIER)
      IF(JBADIER.NE.0) THEN
         WRITE(6,*)' GAMPEX: EBNEUT not initialized'
         IRTFG = -8
         GO TO 9999
      ENDIF
C
C?   Get basic parameters
C
        CALL DEFOVE
C
        FIRST  = .FALSE.
      ENDIF
C
C - next entry   -------------------------------------
C
        CUTENER     = RECPAR(1)
        CUTECL (1)  = RECPAR(2)
        CUTECL (2)  = RECPAR(3)
        CUTECL (3)  = RECPAR(4)
        CUTECLOK    = RECPAR(5)
        CUTRKX      = RECPAR(6)
        KOK12       = NINT(RECPAR(7))
        KOK23       = NINT(RECPAR(8))
C   stack 2
        CUTDIST2 = CUTRKX+1.
C   stack 3
        CUTDIST3 = CUTRKX+2.
C
C
C
      IRTFG = 0
      NNGA=0
      NCLNOK = 0
      CALL VZERO(GAMVE,LTOTGA)
      CALL VZERO(GAMCE,LTOTGA)
      CALL VZERO(IGERM,NKLUMX*4)
C
      CALL VZERO(GAMFL ,28)
      CALL VZERO(GFL   ,10*NKLUMX)
      CALL VZERO(PF    , 5*NFPHO1)
      CALL VZERO(GAMORE,21*NFPHO1)
C
C
C
C min. number of storeys stack 1 and stack 2
C ------------------------------------------
C
C POT banks
C ---------
      JPEST = IW( NAMIND('PEST') )
      IF( JPEST .LE. 0 ) THEN
        IRTFG = -1
        GO TO 9999
      ENDIF
      NPEST = LROWS(JPEST)
      JETDI = IW( NAMIND('ETDI') )
      IF( JETDI .LE. 0 ) THEN
        IRTFG = -1
        GO TO 9999
      ENDIF
      IF ( NPEST.GT.3*NST3RX) THEN
        IRTFG = -1
        GO TO 9999
      ENDIF
      NETDI = LROWS(JETDI)
      JPECO = IW( NAMIND('PECO') )
      IF( JPECO .LE. 0 ) THEN
        IRTFG = -1
        GO TO 9999
      ENDIF
      NPECO = LROWS(JPECO)
      JPCRL = IW( NAMIND('PCRL') )
      IF( JPCRL .LE. 0 ) THEN
        IRTFG = -1
        GO TO 9999
      ENDIF
      NPCRL= LROWS ( JPCRL )
      NPFRF=0
      JPFRF = IW( NAMIND('PFRF') )
      IF(JPFRF.GT.0) NPFRF = LROWS(JPFRF)
C-----------------------------------
C flag ch. track in the PECO cluster
C ----------------------------------
      IF(NPFRF.GT.NXM) THEN
        IRTFG = -2
        GO TO 9999
      ENDIF
      DO I = 1 , NPFRF
        LTRP(I) = 0
      ENDDO
      DO I = 1 , NPCRL
        IPC = ITABL(JPCRL,I,2)
        IF(IPC.EQ.IPECO) THEN
          ITRK = ITABL(JPCRL,I,3)
          IF(ITRK.GT.0) THEN
            LTRP(ITRK) = 1
          ENDIF
        ENDIF
      ENDDO
C
      NCTK = 0
      DO I = 1 , NPFRF
        IF(LTRP(I).EQ.1) THEN
          NCTK = NCTK + 1
          IPTPRF(NCTK) = I
        ENDIF
      ENDDO
C----------------------------------
C Extrapolate linked charged tracks
C ---------------------------------
      DO I = 1 , NCTK
        IPT=IPTPRF(I)
        IPTP = JPFRF + 2 + (IPT-1) * IW(JPFRF+1) + 1
        CALL EXPFRF(RESTK1,ZESTK1,RW(IPTP),QEXP(1,I),IRRET)
        IF(IRRET.EQ.0)THEN
          DO K=1,2
            QEXP(K,I)=99999.
            QEXP(K+3,I)=0.
          ENDDO
          QEXP(3,I)=0.
          QEXP(6,I)=1.
        ELSE
C       NORMALIZE DIRECTION
          EXPNOR=0.
          DO K=4,6
            EXPNOR=EXPNOR+QEXP(K,I)**2
          ENDDO
          EXPNOR=SQRT(EXPNOR)
          DO  K=4,6
            QEXP(K,I)=QEXP(K,I)/EXPNOR
          ENDDO
        ENDIF
      ENDDO
C
C ----------------------------------------
C Collect  the storeys for clusterization
C ----------------------------------------
      ISTFND1 = 0
      ISTFND2 = 0
      ISTFND3 = 0
C
      NX = 0
      DO 1 ITX = 1 , NPEST
        IPC  = ITABL(JPEST,ITX,5)
        IF(IPC.NE.IPECO)          GO TO 1
        ITO  = ITABL(JPEST,ITX,4)
        IF(ITO.LE.0)              GO TO 1
        KS   = ITABL(JPEST,ITX,1)
        IWIN = ITABL(JETDI,ITO,JETDTL)
        IT   = IWIN / IDPK
        JF   = (IWIN - IT * IDPK) / 4
        IF ( IT.LT.1.OR.IT.GT.228) GO TO 1
        IF ( JF.LT.1.OR.JF.GT.384) GO TO 1
        IF(NX .EQ. 0 ) THEN
          NX = NX + 1
          CALL EMDTOW(IT,JF,ISCO,IMDO,IRG)
        ENDIF
C ------------------
C Loop over stacks
C ------------------
        CALL ECRWRG(IT,IRSS,MXCOL)
        IF(IT .GT. ITHTO ) THEN
          IRTFG=1
          GO TO 1
        ENDIF
        IF(JF .GT. MXCOL ) THEN
          IRTFG=1
          GO TO 1
        ENDIF

        CALL ESRBC('ALEPH',IT,JF,KS,POINT)
        ENSTO = FLOAT (ITABL(JETDI,ITO,JETDTL+KS)) / 1000000.
        ENSTO = ENSTO * ECETDI(IT,JF)
        IF(ENSTO.LT.CUTENER) GO TO 1
C-------------------------------
C      Logical stack number
C------------------------------
        KSLG=KS
        IF( KS.LT.3 .AND. ( IT.GE.ITOV1.AND.IT.LE.ITOV2 .OR.
     >          IT.GE.ITOV4.AND.IT.LE.ITOV3   ) )
     >          KSLG = KSLG + 1
C
        IF(KSLG.EQ.1)THEN
          ISTFND1=ISTFND1+1
          IF(ISTFND1.GT.NSTRMX) THEN
            IRTFG = -3
            GO TO 9999
          ENDIF
          ESTO1(1,ISTFND1) = ENSTO
          ESTO1(2,ISTFND1) = POINT(1)
          ESTO1(3,ISTFND1) = POINT(2)
          ESTO1(4,ISTFND1) = POINT(3)
          ISTO1(5,ISTFND1) = IT
          ISTO1(6,ISTFND1) = JF
          ISTO1(7,ISTFND1) = KS
          ISTO1(8,ISTFND1) = 0
          ISTO1(9,ISTFND1) = ITX
        ELSEIF(KSLG.EQ.2)THEN
          ISTFND2=ISTFND2+1
          IF(ISTFND2.GT.NSTRMX) THEN
            IRTFG = -3
            GO TO 9999
          ENDIF
          ESTO2(1,ISTFND2) = ENSTO
          ESTO2(2,ISTFND2) = POINT(1)
          ESTO2(3,ISTFND2) = POINT(2)
          ESTO2(4,ISTFND2) = POINT(3)
          ISTO2(5,ISTFND2) = IT
          ISTO2(6,ISTFND2) = JF
          ISTO2(7,ISTFND2) = KS
          ISTO2(8,ISTFND2) = 0
          ISTO2(9,ISTFND2) = ITX
        ELSEIF(KSLG.EQ.3)THEN
          ISTFND3=ISTFND3+1
          IF(ISTFND3.GT.NSTRMX) THEN
            IRTFG = -3
            GO TO 9999
          ENDIF
          ESTO3(1,ISTFND3) = ENSTO
          ESTO3(2,ISTFND3) = POINT(1)
          ESTO3(3,ISTFND3) = POINT(2)
          ESTO3(4,ISTFND3) = POINT(3)
          ISTO3(5,ISTFND3) = IT
          ISTO3(6,ISTFND3) = JF
          ISTO3(7,ISTFND3) = KS
          ISTO3(8,ISTFND3) = 0
          ISTO3(9,ISTFND3) = ITX
        ENDIF
    1 CONTINUE
C
C-----------------------------------------------------------
C find the vertex of the event
C ---------------------------------------------------------
C
      CALL EVTVER(DELTA(1),DELTA(2),DELTA(3))
C
C----------------------------------------------------------
      IF(ISTFND2  .LE.  0 ) THEN
        GO TO 9999
      ENDIF
C
C               +========================+
C               ||    Clusterisation    ||
C               +========================+
C ---------------------------------------------------------
C First reorder ESTO(1,2,3) according to decreasing energies
C ----------------------------------------------------------
      IF(ISTFND1 .GT. 0 ) CALL SORTRQ(ESTO1,9,ISTFND1,-1)
      CALL SORTRQ(ESTO2,9,ISTFND2,-1)
      IF(ISTFND3 .GT. 0 ) CALL SORTRQ(ESTO3,9,ISTFND3,-1)
C --
C ---------------------------------
C Pointeurs storeys dans esto1,2,3
C ----------------------------------
      CALL VZERO(INDSTO,2*NST3RX)
      DO IST = 1,ISTFND1
         ITX = ISTO1(9,IST)
         INDSTO(1,ITX) = 1
         INDSTO(2,ITX) = IST
      ENDDO
      DO IST = 1,ISTFND2
         ITX = ISTO2(9,IST)
         INDSTO(1,ITX) = 2
         INDSTO(2,ITX) = IST
      ENDDO
      DO IST = 1,ISTFND3
         ITX = ISTO3(9,IST)
         INDSTO(1,ITX) = 3
         INDSTO(2,ITX) = IST
      ENDDO
C
C --
C
C -----------
C Stack # 1
C -----------
      NCLU = 0
      DO 6 I = 1 , ISTFND1
        DO  J =1 , I-1
          IF (  VOISTO(ISTO1(5,I),ISTO1(5,J) ) ) THEN
            ISTO1(8,I) = ISTO1(8,J)
            GO TO 6
          ENDIF
        ENDDO
C----------------------------------------------
C   Isolated  storey , open cluster
C        if  Energy  > Threshold
C        and check charged tracks distance
C ---------------------------------------------
        KS = ISTO1(7,I)
        IF ( ESTO1(1,I) .LT. CUTECL(KS) ) GO TO 6
        IF (NCLU.GE.NKLUMX) GOTO 888
        NCLU = NCLU + 1
        ISTO1(8,I) = NCLU
        IGERM(1,NCLU) = I
        IGERM(4,NCLU) = 1
C--------------------
C Charged cluster ?
C -------------------
        TRD = 999999.
        DO   J = 1 , NCTK
          TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),ESTO1(2,I)) )
        ENDDO
        IF ( TRD .LE. CUTRKX) ISTO1(8,I) = -ISTO1(8,I)
    6 CONTINUE
C------------
C Stack # 2
C ----------
      DO 16 I = 1 , ISTFND2
C---------------------------------------------
C   Look first for a neighbour in stack #1
C---------------------------------------------
        DO J = 1 , ISTFND1
          IF(ISTO1(8,J).NE.0)THEN
            IF( VOISTO ( ISTO2(5,I),ISTO1(5,J) ) ) THEN
              NCLUFD = ISTO1(8,J)
              ISTO2(8,I) = NCLUFD
              NCLUFD = IABS(NCLUFD)
              IF( IGERM(2,NCLUFD).EQ.0 ) IGERM(2,NCLUFD) = I
              GO TO 16
            ENDIF
          ENDIF
        ENDDO
C------------------------------------------------
C       Look for a neighbour in stack # 2
C       if none in stack # 1
C------------------------------------------------
        DO  J =1 , I-1
          IF (  VOISTO ( ISTO2(5,I),ISTO2(5,J) ) ) THEN
            NCLUFD = ISTO2(8,J)
            ISTO2(8,I) = NCLUFD
            NCLUFD = IABS(NCLUFD)
            IF( IGERM(2,NCLUFD).EQ.0 ) IGERM(2,NCLUFD) = I
            GO TO 16
          ENDIF
        ENDDO
C-------------------------------------------------
C   Isolated  storey , open cluster
C        if Energy  > Threshold
C        and check charged tracks distance
C -----------------------------------------------
        KS = ISTO2(7,I)
        IF ( ESTO2(1,I) .LT. CUTECL(KS) ) GO TO 16
        IF (NCLU.GE.NKLUMX) GOTO 888
        NCLU = NCLU + 1
        ISTO2(8,I) = NCLU
        IGERM(2,NCLU) = I
        IGERM(4,NCLU) = 2
C-------------------
C Charged cluster ?
C ------------------
        TRD = 999999.
        DO   J = 1 , NCTK
          TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),ESTO2(2,I)) )
        ENDDO
        IF ( TRD .LE. CUTDIST2) ISTO2(8,I) = -ISTO2(8,I)
   16 CONTINUE
C-------------------------------
C Second pass in stack 1
C unassociated storeys only
C -----------------------------
      DO 36 I = 1 , ISTFND1
        IF( ISTO1(8,I) .EQ.0 )THEN
          DO J = 1 , ISTFND2
            IF(ISTO2(8,J).NE.0)THEN
              IF( VOISTO ( ISTO1(5,I),ISTO2(5,J) ) ) THEN
                NCLUFD= ISTO2(8,J)
                ISTO1(8,I) = NCLUFD
                NCLUFD = IABS(NCLUFD)
                IF ( IGERM(1,NCLUFD).EQ.0 ) IGERM(1,NCLUFD) = I
                GO TO 36
              ENDIF
            ENDIF
          ENDDO
        ENDIF
   36 CONTINUE
C-------------
C   Stack # 3
C-------------
      DO 26 I = 1 , ISTFND3
C----------------------------------------
C  look first in stack 2 for a neighbour
C----------------------------------------
        DO J = 1 , ISTFND2
          IF(ISTO2(8,J).NE.0)THEN
            IF( VOISTO ( ISTO3(5,I),ISTO2(5,J) ) )THEN
              NCLUFD = ISTO2(8,J)
              ISTO3(8,I) = NCLUFD
              NCLUFD = IABS(NCLUFD)
              IF( IGERM(3,NCLUFD).EQ.0 ) IGERM(3,NCLUFD) = I
              GO TO 26
            ENDIF
          ENDIF
        ENDDO
C----------------------------------
C look for a neighbour in stack # 3
C ---------------------------------
        DO  J =1 , I-1
          IF (  VOISTO ( ISTO3(5,I),ISTO3(5,J) ) ) THEN
            NCLUFD = ISTO3(8,J)
            ISTO3(8,I) = NCLUFD
            NCLUFD = IABS(NCLUFD)
            IF( IGERM(3,NCLUFD).EQ.0 ) IGERM(3,NCLUFD) = I
            GO TO 26
          ENDIF
        ENDDO
C----------------------------------------------
C   Isolated  storey , open cluster
C        if Energy  > Threshold
C        and check charged tracks distance
C ---------------------------------------------
        KS = ISTO3(7,I)
        IF ( ESTO3(1,I) .LT. CUTECL(KS) ) GO TO 26
        IF (NCLU.GE.NKLUMX) GOTO 888
        NCLU = NCLU + 1
        ISTO3(8,I) = NCLU
        IGERM(3,NCLU) = I
        IGERM(4,NCLU) = 3
C----------------------------------------------
C Charged cluster ?
C ---------------------------------------------
        TRD = 999999.
        DO   J = 1 , NCTK
          TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),ESTO3(2,I)) )
        ENDDO
        IF ( TRD .LE. CUTDIST3) ISTO3(8,I) = -ISTO3(8,I)
   26 CONTINUE
C---------------------------
C Second pass in stack 2
C unassociated storeys only
C --------------------------
      DO 46 I = 1 , ISTFND2
        IF( ISTO2(8,I) .EQ.0 )THEN
          DO J = 1 , ISTFND3
            IF(ISTO3(8,J).NE.0)THEN
              IF( VOISTO ( ISTO2(5,I),ISTO3(5,J) ) ) THEN
                ISTO2(8,I) = ISTO3(8,J)
                NCLUFD= ISTO3(8,J)
                ISTO2(8,I) = NCLUFD
                NCLUFD = IABS(NCLUFD)
                IF ( IGERM(2,NCLUFD).EQ.0 ) IGERM(2,NCLUFD) = I
                GO TO 46
              ENDIF
            ENDIF
          ENDDO
        ENDIF
   46 CONTINUE
C--------------------------
C clusters global variables
C -------------------------
C
      DO I = 1 , NCLU
        EBAR12(I) = 0.
        DO J = 17,20
          ICLUST(J,I) = 0
        ENDDO
        DO J = 1,16
          CLUST (J,I) = 0.
        ENDDO
        DO J = 21,24
          CLUST(J,I) = 999999.
        ENDDO
      ENDDO
      DO I = 1 , ISTFND1
        LCLU = ISTO1(8,I)
        IF (LCLU.NE.0) THEN
          IF(LCLU.LT.0)THEN
            LCLU = -LCLU
            ICLUST(17,LCLU) = 1
          ENDIF
C-----------------------------
C Flag hidden part of end cap
C ----------------------------
            IF( ISTO1(7,I) .EQ. 1 .AND. (
     &          ( ISTO1(5,I).GE.ITOV1 .AND. ISTO1(5,I).LE.ITOV2 ) .OR.
     &          ( ISTO1(5,I).GE.ITOV4 .AND. ISTO1(5,I).LE.ITOV3 ) ) )
     &                    ICLUST(17,LCLU) =MOD(ICLUST(17,LCLU),2) + 2
          IF( ISTO1(7,I).LT.3 .AND. ISTO1(5,I).GT.ITOV2 .AND.
     &        ISTO1(5,I).LT.ITOV4 ) EBAR12(LCLU) =
     &                                      EBAR12(LCLU) + ESTO1(1,I)
          ICLUST(18,LCLU) = ICLUST(18,LCLU) + 1
          CLUST(4,LCLU) = CLUST(4,LCLU) + ESTO1(1,I)
          CLUST(16,LCLU) = CLUST(16,LCLU) + ESTO1(1,I)
          CLUST(1,LCLU) = CLUST(1,LCLU) + ESTO1(1,I)*ESTO1(2,I)
          CLUST(2,LCLU) = CLUST(2,LCLU) + ESTO1(1,I)*ESTO1(3,I)
          CLUST(3,LCLU) = CLUST(3,LCLU) + ESTO1(1,I)*ESTO1(4,I)
          CLUST(13,LCLU) = CLUST(13,LCLU) + ESTO1(1,I)*ESTO1(2,I)
          CLUST(14,LCLU) = CLUST(14,LCLU) + ESTO1(1,I)*ESTO1(3,I)
          CLUST(15,LCLU) = CLUST(15,LCLU) + ESTO1(1,I)*ESTO1(4,I)
        ENDIF
      ENDDO
C
      DO I = 1 , ISTFND2
        LCLU = ISTO2(8,I)
        IF (LCLU.NE.0) THEN
          IF(LCLU.LT.0)THEN
            LCLU = -LCLU
            ICLUST(17,LCLU) = 1
          ENDIF
C -------------------------------
C flag the hidden part of end cap
C -------------------------------
            IF( ISTO2(7,I) .EQ. 1 .AND. (
     &        ( ISTO2(5,I).GE.ITOV1 .AND. ISTO2(5,I).LE.ITOV2 )  .OR.
     &        ( ISTO2(5,I).GE.ITOV4 .AND. ISTO2(5,I).LE.ITOV3 ) ) )
     &                    ICLUST(17,LCLU) =MOD(ICLUST(17,LCLU),2) + 2
          IF( ISTO2(7,I).LT.3 .AND. ISTO2(5,I).GT.ITOV2 .AND.
     &        ISTO2(5,I).LT.ITOV4 ) EBAR12(LCLU) =
     &                                      EBAR12(LCLU) + ESTO2(1,I)
          ICLUST(19,LCLU) = ICLUST(19,LCLU) + 1
          CLUST(8,LCLU) = CLUST(8,LCLU) + ESTO2(1,I)
          CLUST(16,LCLU) = CLUST(16,LCLU) + ESTO2(1,I)
          CLUST(5,LCLU) = CLUST(5,LCLU) + ESTO2(1,I)*ESTO2(2,I)
          CLUST(6,LCLU) = CLUST(6,LCLU) + ESTO2(1,I)*ESTO2(3,I)
          CLUST(7,LCLU) = CLUST(7,LCLU) + ESTO2(1,I)*ESTO2(4,I)
          CLUST(13,LCLU) = CLUST(13,LCLU) + ESTO2(1,I)*ESTO2(2,I)
          CLUST(14,LCLU) = CLUST(14,LCLU) + ESTO2(1,I)*ESTO2(3,I)
          CLUST(15,LCLU) = CLUST(15,LCLU) + ESTO2(1,I)*ESTO2(4,I)
        ENDIF
      ENDDO
C
      DO I = 1 , ISTFND3
        LCLU = ISTO3(8,I)
        IF (LCLU.NE.0) THEN
          IF(LCLU.LT.0)THEN
            LCLU = -LCLU
            ICLUST(17,LCLU) = 1
          ENDIF
          ICLUST(20,LCLU) = ICLUST(20,LCLU) + 1
          CLUST(12,LCLU) = CLUST(12,LCLU) + ESTO3(1,I)
          CLUST(16,LCLU) = CLUST(16,LCLU) + ESTO3(1,I)
          CLUST(9,LCLU) = CLUST(9,LCLU) + ESTO3(1,I)*ESTO3(2,I)
          CLUST(10,LCLU) = CLUST(10,LCLU) + ESTO3(1,I)*ESTO3(3,I)
          CLUST(11,LCLU) = CLUST(11,LCLU) + ESTO3(1,I)*ESTO3(4,I)
          CLUST(13,LCLU) = CLUST(13,LCLU) + ESTO3(1,I)*ESTO3(2,I)
          CLUST(14,LCLU) = CLUST(14,LCLU) + ESTO3(1,I)*ESTO3(3,I)
          CLUST(15,LCLU) = CLUST(15,LCLU) + ESTO3(1,I)*ESTO3(4,I)
        ENDIF
      ENDDO
C------------
C Barycenters
C -----------
      DO I = 1,NCLU
        IF ( CLUST(4,I).GT.0.01 ) THEN
          CLUST(1,I) = CLUST(1,I) / CLUST(4,I)
          CLUST(2,I) = CLUST(2,I) / CLUST(4,I)
          CLUST(3,I) = CLUST(3,I) / CLUST(4,I)
          TRD = 999999.
          DO   J = 1 , NCTK
            TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),CLUST(1,I)) )
          ENDDO
          CLUST(21,I) = TRD
          IF( MOD( ICLUST(17,I) , 2 ) .EQ. 0 ) THEN
            RAYON = SQRT(CLUST(1,I)**2+CLUST(2,I)**2+CLUST(3,I)**2)
            IF ( TRD .LE. CUTRKX * RAYON / RESTK1 )
     &            ICLUST(17,I) = ICLUST(17,I) + 1
          ENDIF
        ENDIF
        IF ( CLUST(8,I).GT.0.01 ) THEN
          CLUST(5,I) = CLUST(5,I) / CLUST(8,I)
          CLUST(6,I) = CLUST(6,I) / CLUST(8,I)
          CLUST(7,I) = CLUST(7,I) / CLUST(8,I)
          TRD = 999999.
          DO   J = 1 , NCTK
            TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),CLUST(5,I)) )
          ENDDO
          CLUST(22,I) = TRD
          IF( MOD( ICLUST(17,I) , 2 ) .EQ. 0 ) THEN
            RAYON = SQRT(CLUST(5,I)**2+CLUST(6,I)**2+CLUST(7,I)**2)
            IF ( TRD .LE. CUTRKX * RAYON / RESTK1 )
     X            ICLUST(17,I) = ICLUST(17,I) + 1
          ENDIF
        ENDIF
        IF ( CLUST(12,I).GT.0.01 ) THEN
          CLUST(9,I) = CLUST(9,I) / CLUST(12,I)
          CLUST(10,I) = CLUST(10,I) / CLUST(12,I)
          CLUST(11,I) = CLUST(11,I) / CLUST(12,I)
          TRD = 999999.
          DO   J = 1 , NCTK
            TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),CLUST(9,I)) )
          ENDDO
          CLUST(23,I) = TRD
          IF( MOD( ICLUST(17,I) , 2 ) .EQ. 0 ) THEN
            RAYON = SQRT(CLUST(9,I)**2+CLUST(10,I)**2+CLUST(11,I)**2)
              IF ( TRD .LE. CUTRKX * RAYON / RESTK1 )
     &           ICLUST(17,I) = ICLUST(17,I) + 4
          ENDIF
        ENDIF
        IF ( CLUST(16,I).GT.0.01 ) THEN
          CLUST(13,I) = CLUST(13,I) / CLUST(16,I)
          CLUST(14,I) = CLUST(14,I) / CLUST(16,I)
          CLUST(15,I) = CLUST(15,I) / CLUST(16,I)
          TRD = 999999.
          DO   J = 1 , NCTK
            TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),CLUST(13,I)) )
          ENDDO
          CLUST(24,I) = TRD
        ENDIF
      ENDDO
C---------------------------------
C storeys info . for each cluster
C INDEX ENDEX and LSTGA
C --------------------------------
      DO I = 1 , NKLUMX
        NSTCL(I)=0
        ISTCL(I)=0
      ENDDO
C
      DO I = 1 , NCLU
        IF(I.EQ.1)THEN
          ISTCL(I) = 1
        ELSE
          ISTCL(I) = ISTCL(I-1) + NSTCL(I-1)
        ENDIF
        IFREE = ISTCL(I)
        DO J = 1,ISTFND1
          IF(IABS(ISTO1(8,J)) .EQ. I)THEN
            INDEX(1,IFREE) = ISTO1(5,J)
            INDEX(2,IFREE) = ISTO1(6,J)
            INDEX(3,IFREE) = ISTO1(7,J)
            LSTGA(IFREE)   = ISTO1(9,J)
            ENDEX(IFREE)   = ESTO1(1,J)
            IFREE = IFREE +1
            NSTCL(I) =NSTCL(I) + 1
          ENDIF
        ENDDO
C
        DO J = 1,ISTFND2
          IF(IABS(ISTO2(8,J)) .EQ. I)THEN
            INDEX(1,IFREE) = ISTO2(5,J)
            INDEX(2,IFREE) = ISTO2(6,J)
            INDEX(3,IFREE) = ISTO2(7,J)
            LSTGA(IFREE)   = ISTO2(9,J)
            ENDEX(IFREE)   = ESTO2(1,J)
            IFREE = IFREE +1
            NSTCL(I) =NSTCL(I) + 1
          ENDIF
        ENDDO
C
        DO J = 1,ISTFND3
          IF(IABS(ISTO3(8,J)) .EQ. I)THEN
            INDEX(1,IFREE) = ISTO3(5,J)
            INDEX(2,IFREE) = ISTO3(6,J)
            INDEX(3,IFREE) = ISTO3(7,J)
            LSTGA(IFREE)   = ISTO3(9,J)
            ENDEX(IFREE)   = ESTO3(1,J)
            IFREE = IFREE +1
            NSTCL(I) =NSTCL(I) + 1
          ENDIF
        ENDDO
C
      ENDDO
C
      DO I = 1 ,  NCLU
         RAYON=0.
         RAX=0.
         DO K = 1,3
           RAYON = RAYON + (CLUST(12+K,I)-DELTA(K))**2
           RAX   = RAX   + CLUST(12+K,I)**2
         ENDDO
         RAYON=SQRT(RAYON)
         RAX  =SQRT(RAX)
         RAIE(I)=RAX
         QGAM (4,I) = CLUST(16,I)
         DO K = 1,3
           QGAM(K,I) = CLUST(16,I) * (CLUST(12+K,I)-DELTA(K))/RAYON
         ENDDO
      ENDDO
C--------------------------------------------------------
C
C     List of good photons candidates
C
C KOK12 = 1 if stack 1/2 ok for gammas
C KOK23 = 1 if stack 2/3 ok for gammas
C at least CUTECLOK GeV in energy
C ------------------------------------------------------
C
      NCLNOK = 0
      DO I = 1,NCLU
C----------------------------
C Initialize Gamma flag
C--------------------------
        GAMOK(I) = .FALSE.
C
        MOD1   =  MOD(ICLUST(17,I),2)
C
        EST1 = CLUST(4,I)
        EST2 = CLUST(8,I)
        EST3 = CLUST(12,I)
        ECOXX  = EST1 + EST2
        NST3 = ICLUST(20,I)
        NST2 = ICLUST(19,I)
        NST1 = ICLUST(18,I)
        IST12  = 0
        IF(NST1 .GT. 0 .AND. NST2 .GT. 0 ) IST12 = 1
        IST23  = 0
        IF(NST2 .GT. 0 .AND. NST3 .GT. 0 ) IST23 = 1
        IGSTO = 0
        IF(IST12 .EQ. 1 .AND. KOK12 .EQ. 1) IGSTO=1
        IF(IST23 .EQ. 1 .AND. KOK23 .EQ. 1) IGSTO=1

        EGGG   = QGAM(4,I)

        IF( MOD1   .EQ.  0                   .AND.
     &   ECOXX      .GT.  CUTECLOK            .AND.
     &   IGSTO      .EQ.  1                   .AND.
     &   EGGG       .GT.  EMIGA                    )  THEN
           NCLNOK = NCLNOK + 1
           KCLNOK ( NCLNOK ) = I
C---------------------------------
C   Set gamma flag
C----------------------------------
           GAMOK(I) = .TRUE.
        ENDIF
      ENDDO
C
  999 CONTINUE
C
      NNGA = NCLNOK
      IF( NNGA .LE. 0 ) GO TO 9999
C
C
C-------------------------------------
C    create dead storey list
C-------------------------------------
      CALL BGETDS
C------------------------------------
C
C     Loop over photon candidates
C     Corrections and flags
C------------------------------------
C
      DO  2000 I = 1 , NNGA
C
        IF(I  .GT.  NFOMAX) THEN
          IRTFG = -5
          GO TO 9999
        ENDIF
C
C----------------------------------------
C    Uncorrected quantities
C    Energy EGRAW
C    Theta Phi from vertex THGRAW PHGRAW
C    Theta Phi From origin THCOGR PHCOGR
C----------------------------------------
        ICL = KCLNOK(I)
C
        EGRAW = QGAM(4,ICL)
        PPROJ = SQRT( QGAM(1,ICL)**2 + QGAM(2,ICL)**2)
        THGRAW = ATAN2 ( PPROJ , QGAM(3,ICL) )
        PHGRAW = ATAN2 ( QGAM(2,ICL),QGAM(1,ICL) )
        IF (PHGRAW.LE.0.) PHGRAW = PHGRAW + TWOPI
C
        PPROJ = SQRT( CLUST(13,ICL)**2 + CLUST(14,ICL)**2)
        THCOGR = ATAN2 ( PPROJ , CLUST(15,ICL) )
        PHCOGR = ATAN2 ( CLUST(14,ICL),CLUST(13,ICL) )
        IF (PHCOGR.LE.0.) PHCOGR = PHCOGR + TWOPI
        NS1 = ISTCL(ICL)
        NS2 = NS1 + NSTCL(ICL) - 1
        NUST= NSTCL(ICL)
        JST = 0
C----------------------------------------------------
C     Storey information
C         LISTOR ESTOR for analysis "a la EBNEUT"
C         LCLMST       for analysis "a la BULOS"
C         LGASTO       for the PGPC  PEST relation
C----------------------------------------------------
        EST1 = CLUST(4,ICL)
        EST2 = CLUST(8,ICL)
        EST3 = CLUST(12,ICL)
        NST3 = ICLUST(20,ICL)
        NST2 = ICLUST(19,ICL)
        NST1 = ICLUST(18,ICL)
C
        DO J = NS1 , NS2
          JST = JST + 1
          IF(JST .GT. NSTGAX) THEN
            IRTFG = -6
            GO TO 9999
          ENDIF
          LGASTO(I,JST) = LSTGA(J)
          LCLMST( JST ) = LSTGA(J)
          LISTOR(1,JST) = INDEX(1,J)
          LISTOR(2,JST) = INDEX(2,J)
          LISTOR(3,JST) = INDEX(3,J)
          ESTOR ( JST ) = ENDEX(J)
        ENDDO
        NNSTGA(I) = JST
C-------------------------------------------------------------
C     Energy fractions in the logical stacks
C-------------------------------------------------------------
        R1 = -1.
        R2 = -1.
        ETTT = EST1 + EST2 + EST3
        IF(ETTT .GT. 0.001) THEN
          R1 = EST1/ETTT
          R2 = EST2/ETTT
        ENDIF
C----------------------------------------------------------
C       Use EBCDRG for crack flag
C
        CALL EBCDRG(THCOGR,PHCOGR,IKOD(1),IKOD(2),IKOD(6),IKOD(3)
     X                                                   ,IERDRG)
C-----------------------------------------------------------
        ICRK = 0
        IF(IKOD(8) .EQ. 1 .OR. IKOD(8) .EQ. 2 ) ICRK = 1
        IF(IKOD(8) .EQ. 3 .OR. IKOD(8) .EQ. 4 ) ICRK = 1
C
C       " Peak storey"
C
        LM = LVMAX(ESTOR,NUST)
        ITPIC = LISTOR(1,LM)
        JFPIC = LISTOR(2,LM)
        KSPIC = LISTOR(3,LM)
C
C      Flag region limit (can be improved)
C
        IBORFL=0
        DO JL = 1 , 6
          IF(LBOREG(JL) .EQ. ITPIC.OR.LBOREG(JL).EQ.229-ITPIC)
     &    IBORFL = 1
        ENDDO
        ITMAX = ITPIC
        JFMAX = JFPIC
C-------------------------------------------------------------------
C     Leakage and saturation corrections
C     to be put at the end of the routine
C-------------------------------------------------------------------
C
C      Leakage  correction
C
        STACE(1) = EGRAW
        STACE(2) = 0.
        STACE(3) = 0.
        BARY(1) = THCOGR
        BARY(2) = PHCOGR
        MTC = 0
        IFLOV = MOD(ICLUST(17,ICL),4)/2
        COROV = 1.
        IF(IFLOV .NE. 0) THEN
          CALL EBCLAP( MTC,NUST,LISTOR,ESTOR,EVLP,IVLP)
          IF(IVLP.GT.0) THEN
            TVLP = 0.
            DO ITVLP = 1, 5
               TVLP = TVLP + EVLP(ITVLP)
            ENDDO
          ENDIF
          STACE(1) = STACE(1) + TVLP
          COROV = 1.+ TVLP/EGRAW
        ENDIF
C
        CALL ECLEAK(MTC,STACE,BARY,MTC,CORLK)
        CORLK = CORLK * COROV
C
C         correction for saturation
C         only for real data
C
        CALL ABRUEV (NRUN,NEVT)
        CORSAT = 1.
        IF (NRUN.GT.2000) CORSAT  =  1. + PSATU * EGRAW *CORLK
C------------------------------------------------------
C      Construct the 3X3 grid "a la EBNEUT"
C------------------------------------------------------
        CALL EBGRID(NUST,LISTOR,ESTOR,ITPIC,JFPIC,GRILE)
C
        DO K1= 1 , 3
           DO K2 = 1 , 3
             ES33(K1,K2)=0.
             DO K3 = 1 , 3
               ES33(K1,K2)=ES33(K1,K2)+GRILE(K1,K2,K3)
             ENDDO
           ENDDO
        ENDDO

        F4G=-1.
        ESTAM = -10.
C--------------------------------------------------------------------
C     GXTRAN S_curve correction on angles F4 correction on energy
C         THSC and PHSC are the angles  COREF is the estimated F4
C         F4G the measured one
C--------------------------------------------------------------------
        CALL GXTRAN (ITMAX,JFMAX,ES33,EGRAW,
     &                     F4G,ESTAM,THSC,PHSC,COREF,ICOK)
C-------------------------------------------------------------------
C     Not used at the region limits
C     THEGA and PHIGA are angles from ALEPH center
C-------------------------------------------------------------------
        IF(IBORFL .EQ. 0 ) THEN
          F4EST     =  COREF
          THEGA   = THSC
          PHIGA   = PHSC
        ELSE
          F4EST   = F4G
          THEGA   = THCOGR
          PHIGA   = PHCOGR
        ENDIF
C--------------------------------------------------------
C Modification for fake photon (Oct 94)
C-----------------------------------------------------
C Liste des numeros pest des storeys dans la grille ebgrid:
        CALL EBGRIX(NUST,LISTOR,LCLMST,ITPIC,JFPIC,KGRID)
        CALL GF4LST(ESTO1,ESTO2,ESTO3,INDSTO,KGRID,KF4GRI)
C Memorisation dans tableaux generaux:
        CALL UCOPY (KGRID, KLUM33(1,1,1,ICL),3*3*3)
        CALL UCOPY (KF4GRI,KLUM22(1,1,1,ICL),2*2*3)

C--------------------------------------------------------------------
C    Tentative flag on bad photons   (to be studied)
C--------------------------------------------------------------------
C
C    New version from October 94
C
         PRFAK = -1.
C
         ISTVS1 = 0
         NSTVS1 = 0
         NSTVU1 = 0
         EGERM1 = 0.
         ESTVS1 = 0.
         ESUVS1 = 0.
         ECLVS1 = 0.
         IFLVS1 = 0
         IGAMA1 = 0
         NSTVI1 = 0
         ESTVI1 = 0.
         ITHGE1 = 0
         JPHGE1 = 0
         PRFAK1 =-100000.
C
         IF(NST1 .GT. 0) THEN
            IGRSTO = IGERM(1,ICL)
            EGERM1 = ESTO1(1,IGRSTO)
            ITHGE1 = ISTO1(5,IGRSTO)
            JPHGE1 = ISTO1(6,IGRSTO)
            ISTGM1 = IGRSTO
            ISTDB = IGRSTO + 1
            DO IS = ISTDB , ISTFND1
               ICLVS = ISTO1(8,IS)
               IF( ICLVS.NE.0 .AND. ICLVS.NE.ICL ) THEN
                  IF( VOISTO(ISTO1(5,IS),ISTO1(5,IGRSTO)) )THEN
                     NSTVS1 = NSTVS1 + 1
                     EGVSTO = ESTO1(1,IS)
                     ESUVS1 = ESUVS1 + EGVSTO
                     IF ( NSTVS1 .EQ. 1) THEN
                        ISTVS1 = IS
                        ESTVS1 = EGVSTO
                     ENDIF
                  ENDIF
               ELSEIF( ICLVS.NE.0 .AND. ICLVS.EQ.ICL ) THEN
                  IF( VOISTO(ISTO1(5,IS),ISTO1(5,IGRSTO)) )THEN
                     NSTVI1 = NSTVI1 + 1
                     EGVSTO = ESTO1(1,IS)
                     IF (NSTVI1 .EQ. 1) THEN
                        ESTVI1 = EGVSTO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
C
            IF(IGERM(4,ICL).EQ.1.AND.ISTVS1.GT.0) THEN
               PRFAK = (EGERM1 -ESTVS1)/SQRT(EGERM1)
            ENDIF
            IF(ISTVS1.GT.0) THEN
               PRFAK1 = (EGERM1 -ESTVS1)/SQRT(EGERM1)
            ENDIF
         ENDIF
C
C
         ISTVS2 = 0
         NSTVS2 = 0
         NSTVU2 = 0
         EGERM2 = 0.
         ESTVS2 = 0.
         ESUVS2 = 0.
         ECLVS2 = 0.
         IFLVS2 = 0
         IGAMA2 = 0
         NSTVI2 = 0
         ESTVI2 = 0.
         ITHGE2 = 0
         JPHGE2 = 0
         PRFAK2 =-100000.
C
         IF(NST2 .GT. 0) THEN
            IGRSTO = IGERM(2,ICL)
            EGERM2 = ESTO2(1,IGRSTO)
            ITHGE2 = ISTO2(5,IGRSTO)
            JPHGE2 = ISTO2(6,IGRSTO)
            ISTGM2 = IGRSTO
            ISTDB = 1
            IF( IGERM(4,ICL) .NE. 1  ) ISTDB = IGRSTO + 1
            DO IS = ISTDB , ISTFND2
               ICLVS = ISTO2(8,IS)
               IF( ICLVS.NE.0 .AND. ICLVS.NE.ICL ) THEN
                  IF( VOISTO( ISTO2(5,IS),ISTO2(5,IGRSTO ))) THEN
                     NSTVS2 = NSTVS2 + 1
                     EGVSTO = ESTO2(1,IS)
                     ESUVS2 = ESUVS2 + EGVSTO
                     IF(EGVSTO.GT.EGERM2) NSTVU2 = NSTVU2 +1
                     IF( NSTVS2 .EQ.1 ) THEN
                        ISTVS2 = IS
                        ESTVS2 = EGVSTO
                     ENDIF
                  ENDIF
               ELSEIF( ICLVS.NE.0 .AND. ICLVS.EQ.ICL ) THEN
                  IF( VOISTO(ISTO2(5,IS),ISTO2(5,IGRSTO)) )THEN
                     NSTVI2 = NSTVI2 + 1
                     EGVSTO = ESTO2(1,IS)
                     IF (NSTVI2 .EQ. 1) THEN
                        ESTVI2 = EGVSTO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
C
            IF(IGERM(4,ICL).EQ.2.AND.ISTVS2.GT.0) THEN
               PRFAK = (EGERM2 -ESTVS2)/SQRT(EGERM2)
            ENDIF
            IF(ISTVS2.GT.0) THEN
               PRFAK2 = (EGERM2 -ESTVS2)/SQRT(EGERM2)
            ENDIF
         ENDIF
C
         ISTVS3 = 0
         NSTVS3 = 0
         NSTVU3 = 0
         EGERM3 = 0.
         ESTVS3 = 0.
         ESUVS3 = 0.
         ECLVS3 = 0.
         IFLVS3 = 0
         IGAMA3 = 0
         ITHGE3 = 0
         JPHGE3 = 0
         PRFAK3 =-100000.
C
         IF(NST3 .GT. 0) THEN
            IGRSTO = IGERM(3,ICL)
            EGERM3 = ESTO3(1,IGRSTO)
            ITHGE3 = ISTO3(5,IGRSTO)
            JPHGE3 = ISTO3(6,IGRSTO)
            ISTGM3 = IGRSTO
            ISTDB = 1
            IF( IGERM(4,ICL) .EQ. 3 ) ISTDB = IGRSTO + 1
            DO IS = ISTDB , ISTFND3
               ICLVS = ISTO3(8,IS)
               IF( ICLVS.NE.0 .AND. ICLVS.NE.ICL ) THEN
                  IF( VOISTO( ISTO3(5,IS),ISTO3(5,IGRSTO ))) THEN
                     NSTVS3 = NSTVS3 +1
                     EGVSTO = ESTO3(1,IS)
                     ESUVS3 = ESUVS3 + EGVSTO
                     IF(EGVSTO .GT.EGERM3) NSTVU3 = NSTVU3 +1
                     IF(NSTVS3.EQ.1) THEN
                        ISTVS3 = IS
                        ESTVS3 = EGVSTO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            IF(ISTVS3.GT.0) THEN
               PRFAK3 = (EGERM3 -ESTVS3)/SQRT(EGERM3)
            ENDIF
 235        CONTINUE
         ENDIF
C
C-----------------------------------------------------------------------
C     Prepare des quantites utiles pour creer de nouveaux estimateurs:
C     Stack de germination + pour chaque stack:
C     Energie de la storey la plus energique, cluster de son voisin
C     le plus energique, Energie de ce cluster , Energie de la storey
C     flag du cluster et numero de photon du cluster
C     Quantites a sortir pour une etude ulterieure
C-----------------------------------------------------------------------
C
C Some words are added compared to last version
      IF(ISTVS1.NE.0)THEN
         ICLVS1 = IABS(ISTO1(8,ISTVS1))
         ECLVS1 = CLUST(16,ICLVS1)
         IFLVS1 = ICLUST(17,ICLVS1)
         IGAMA1 = 0
         DO IOK = 1,NCLNOK
            IF( ICLVS1.EQ.KCLNOK(IOK) ) THEN
               IGAMA1 = IOK
               GO TO 333
            ENDIF
         ENDDO
 333     CONTINUE
      ENDIF
C
      IF(ISTVS2.NE.0)THEN
         ICLVS2 = IABS(ISTO2(8,ISTVS2))
         ECLVS2 = CLUST(16,ICLVS2)
         IFLVS2 = ICLUST(17,ICLVS2)
         IGAMA2 = 0
         DO IOK = 1,NCLNOK
            IF( ICLVS2.EQ.KCLNOK(IOK) ) THEN
               IGAMA2 = IOK
               GO TO 334
            ENDIF
         ENDDO
 334     CONTINUE
      ENDIF
C
      IF(ISTVS3.NE.0)THEN
         ICLVS3 = IABS(ISTO3(8,ISTVS3))
         ECLVS3 = CLUST(16,ICLVS3)
         IFLVS3 = ICLUST(17,ICLVS3)
         IGAMA3 = 0
         DO IOK = 1,NCLNOK
            IF( ICLVS3.EQ.KCLNOK(IOK) ) THEN
               IGAMA3 = IOK
               GO TO 335
            ENDIF
         ENDDO
 335     CONTINUE
      ENDIF
C
C      ----------------------------------------------------
      DO IG =1,4
         IGAMFL(IG) = IGERM(IG,ICL)
      ENDDO
C
      IG = 4
      IGAMFL(IG+1) = NSTVS1
      IGAMFL(IG+2) = NSTVU1
      GAMFL(IG+3)  = EGERM1
      GAMFL(IG+4)  = ESTVS1
      GAMFL(IG+5)  = PRFAK1
      GAMFL(IG+6)  = ECLVS1
      IGAMFL(IG+7) = IFLVS1
      IGAMFL(IG+8) = IGAMA1
      IG = 12
      IGAMFL(IG+1) = NSTVS2
      IGAMFL(IG+2) = NSTVU2
      GAMFL(IG+3)  = EGERM2
      GAMFL(IG+4)  = ESTVS2
      GAMFL(IG+5)  = PRFAK2
      GAMFL(IG+6)  = ECLVS2
      IGAMFL(IG+7) = IFLVS2
      IGAMFL(IG+8) = IGAMA2
      IG = 20
      IGAMFL(IG+1) = NSTVS3
      IGAMFL(IG+2) = NSTVU3
      GAMFL(IG+3)  = EGERM3
      GAMFL(IG+4)  = ESTVS3
      GAMFL(IG+5)  = PRFAK3
      GAMFL(IG+6)  = ECLVS3
      IGAMFL(IG+7) = IFLVS3
      IGAMFL(IG+8) = IGAMA3
C
      GFL(1,I) = GAMFL(4)
      GFL(2,I) = GAMFL(7)
      GFL(3,I) = GAMFL(9)
      GFL(4,I) = GAMFL(10)
      GFL(5,I) = GAMFL(15)
      GFL(6,I) = GAMFL(17)
      GFL(7,I) = GAMFL(18)
      GFL(8,I) = GAMFL(26)



C New words: variables defined for cracks and overlap
      ECRACK = 0.   ! Energie jouxtant un crack
      EC4CRA = 0.   ! Energie des quatres tours centrales jouxtant crack
      EOVBAR = 0.   ! Energie des storeys pathologiques fin de barrel
      EC4OVB = 0.   ! Energie des quatres tours centrales (Idem dessus)
      EOVCAP = 0.   ! Energie des storeys EndCap cachees par le barrel
      EC4OVC = 0.   ! Energie des quatres tours centrales (Idem dessus)
C
      DO KSTO = 1,NUST
         ISTORE = LCLMST(KSTO)
         CALL GIVSTO(ESTO1,ESTO2,ESTO3,INDSTO,ISTORE,ESTO,KSTACK)
         EFRI   = ESTO(1)
         ITHETA = ISTO(5)
         JPHI   = ISTO(6)
         KSTPHY = ISTO(7)
         DANSF4 = INTOF4(ISTORE,KF4GRI)
         IF (ARISTO(ITHETA,JPHI)) THEN
            ECRACK = ECRACK + EFRI
            IF (DANSF4) EC4CRA = EC4CRA+EFRI
         ENDIF
         CALL OVESTO (ITHETA,KSTPHY,IBAREC)
         IF     (IBAREC.EQ.1) THEN
            EOVBAR = EOVBAR + EFRI
            IF (DANSF4) EC4OVB = EC4OVB + EFRI
         ELSEIF (IBAREC.EQ.2) THEN
            EOVCAP = EOVCAP + EFRI
            IF (DANSF4) EC4OVC = EC4OVC + EFRI
         ENDIF
      ENDDO
C---------------------------------------------
      GFL (9,I) = EC4CRA
C
C *~
C---------------------------------------------------------------------
C  Dead storeys flag
C---------------------------------------------------------------------
        MORS1 = 0
        MORS2 = 0
        MORS3 = 0
C
C DEAD storeys in 3 X 3 matrix
C ----------------------------
        KSTAG = 1
        CALL GAFORM(NUST,LISTOR,ESTOR,LSTK,ESTK,NSTK,KSTAG)
        IF ( NSTK .GT. 0 ) THEN
          LM = LVMAX(ESTK,NSTK)
          ITPIC = LSTK(1,LM)
          JFPIC = LSTK(2,LM)
          CALL GAGRID(ITPIC,JFPIC,KSTAG,LST33)
          MORS1  = MORSTO(ITPIC,JFPIC,LSTK,NSTK,LST33)
        ENDIF
        KSTAG = 2
        CALL GAFORM(NUST,LISTOR,ESTOR,LSTK,ESTK,NSTK,KSTAG)
        IF(NSTK .GT. 0 ) THEN
          LM = LVMAX(ESTK,NSTK)
          ITPIC = LSTK(1,LM)
          JFPIC = LSTK(2,LM)
          CALL GAGRID(ITPIC,JFPIC,KSTAG,LST33)
          MORS2  = MORSTO(ITPIC,JFPIC,LSTK,NSTK,LST33)
        ENDIF
        KSTAG = 3
        CALL GAFORM(NUST,LISTOR,ESTOR,LSTK,ESTK,NSTK,KSTAG)
        IF(NSTK .GT. 0 ) THEN
          LM = LVMAX(ESTK,NSTK)
          ITPIC = LSTK(1,LM)
          JFPIC = LSTK(2,LM)
          CALL GAGRID(ITPIC,JFPIC,KSTAG,LST33)
          MORS3  = MORSTO(ITPIC,JFPIC,LSTK,NSTK,LST33)
        ENDIF
C----------------------------------------------------------------
C    S_curve corrected impact
C----------------------------------------------------------------
         PCOGR(3) = RAIE(ICL)  * COS(THEGA)
         PXYG     = RAIE(ICL)  * SIN(THEGA)
         PCOGR(1) = PXYG  * COS(PHIGA)
         PCOGR(2) = PXYG  * SIN(PHIGA)
C------------------------------------------------------------------
C  Recompute the mimimum distance for the corrected photon
C------------------------------------------------------------------
         TRD=99999.
         DO   J = 1 , NCTK
           TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),PCOGR(1)) )
         ENDDO
         DISNE =  TRD
C------------------------------------------------------------------
C     S_curve corrected momentum
C     ""    ""   Theta Phi      THGCOR   PHGCOR  (official output)
C------------------------------------------------------------------
         DO J1 = 1 ,  3
           PNGE(J1) = PCOGR(J1)-DELTA(J1)
         ENDDO
         RRF=VMOD(PNGE,3)
         DO J1 = 1 ,  3
           PNGE(J1) =  PNGE(J1)/RRF
         ENDDO
C---------------------------------------------------------------------
        PPROJ = SQRT( PNGE(1)**2 + PNGE(2)**2)
        THGCOR = ATAN2 ( PPROJ , PNGE(3) )
        PHGCOR = ATAN2 ( PNGE(2),PNGE(1) )
        IF (PHGCOR.LE.0.) PHGCOR = PHGCOR + TWOPI
C---------------------------------------------------------------------
C
C      Best estimate of the energy assuming a single gamma
C
C---------------------------------------------------------------------
      EC4 = EGRAW * (F4G/F4EST) * CORLK * CORSAT
C---------------------------------------------------------------------
C
C     Fill the Output Vector
C
C---------------------------------------------------------------------
C       Redefinition of crack flag
C
        IFLCL = ICLUST(17,ICL)/2
        IFLOV = MOD(IFLCL,2)
        IFLS3 = IFLCL/2
        IFLS3 = MOD (IFLS3,2)
        IF ( IFLOV.NE.0) THEN
          IF(EBAR12(ICL).GT.CUTECLOK ) THEN
            IFLOV = 1
          ELSEIF( EBAR12(ICL).GT.0.5*CUTECLOK) THEN
            IFLOV = 2
          ELSE
            IFLOV = 3
          ENDIF
        ENDIF
        ICRK = ICRK + 2*IFLS3 + 4*IFLOV
        GAMVE(1,I)  = EC4
        GAMVE(2,I)  = THGCOR
        GAMVE(3,I)  = PHGCOR
        GAMVE(4,I)  = 1.
        GAMVE(5,I)  = FLOAT(IBORFL)
        GAMVE(6,I)  = F4G
        GAMVE(7,I)  = ESTAM
        GAMVE(8,I)  = FLOAT( NST1 + 100*NST2 + 10000*NST3)
        GAMVE(9,I)  = R1
        GAMVE(10,I) = R2
        GAMVE(11,I) = FLOAT( MORS1 + 10*MORS2 + 100*MORS3)
        GAMVE(12,I) = EGRAW
        GAMVE(13,I) = FLOAT(ICRK)
        GAMVE(14,I) = FLOAT(ITMAX)
        GAMVE(15,I) = FLOAT(JFMAX)
        GAMVE(16,I) = DISNE
        GAMVE(17,I) = THGRAW
        GAMVE(18,I) = PHGRAW
        GAMVE(19,I) = RAIE(ICL)
        GAMVE(20,I) = PRFAK
        GAMCE(19,I) = COROV
C---------------------------------------------------------------------
C     Put here ad-hoc corrections on the output vector
C---------------------------------------------------------------------
       GAMVE(4,I) = CORAD94(EC4,THGCOR,PHGCOR)
       GACORA(1,I) = F4EST
       GACORA(2,I) = CORLK
       GACORA(3,I) = CORSAT
C---------------------------------------------------------------------
C      Moment analysis " a la BULOS"
C---------------------------------------------------------------------
        CALL GXMCLM(IPECO,NUST,LCLMST,THEGA,PHIGA,
     &     NIMP,CMINFO,IWARN,IERROR,EGRAW)
         DO JX = 1 , 16
           GAMCE(JX,I)=CMINFO(JX)
         ENDDO
         GAMCE(17,I) = FLOAT(IERROR)
         GAMCE(18,I) = FLOAT(IWARN)
C---------------------------------------------------------------------
C    Compute improved BULOS mass
C---------------------------------------------------------------------
      PZMASS = 0.
      IF(IWARN.NE.2 .AND. IERROR.EQ.0 ) THEN
         PZMASS = SQRT( 2.*( CMINFO(9)*CMINFO(13)
     &          - CMINFO(10)*CMINFO(14) - CMINFO(11)*CMINFO(15)
     &          - CMINFO(12)*CMINFO(16) ) )
      ENDIF
      GAMCE(20,I) = PZMASS
C
C
C---------------------------------------------------------------------
C     Additionnal information : energies on crack overlap, for
C                               energies shared by two photons
C                               see later
C---------------------------------------------------------------------
      GAMORE(1,I) = ECRACK
      GAMORE(2,I) = EC4CRA
      GAMORE(3,I) = EOVBAR
      GAMORE(4,I) = EC4OVB
      GAMORE(5,I) = EOVCAP
      GAMORE(6,I) = EC4OVC

C---------------------------------------------------------------------
C     Calcul probabilite fake hadronique
C---------------------------------------------------------------------
      TABHAD(1) = EGRAW
      TABHAD(2) = THGRAW
      TABHAD(3) = PHGRAW
C Distance au barycentre (pas corrige de S-curve):
      TABHAD(4) = CLUST(24,ICL)
C Numero de trace la plus proche:
      ITCLOS = 0
      TRD=99999.
      DO   J = 1 , NCTK
        TRDJ = TRDIST(QEXP(1,J),PCOGR(1))
        IF (TRDJ.LT.TRD) THEN
           TRD    = TRDJ
           ITCLOS = IPTPRF(J)
        ENDIF
      ENDDO
C Calcul l'energie des storeys sur le passage de l'extrapolation
C de la plus proche trace:
      EINTRA = 0.
      IF (ITCLOS.NE.0) THEN
         CALL TRFIRE (ITCLOS,NMXFIR,NSTOFI,STOFIR,IWRNG)
         IF (NSTOFI.GT.0) THEN
            DO KSTO = 1,NUST
               ISTORE = LCLMST(KSTO)
               CALL GIVSTO(ESTO1,ESTO2,ESTO3,INDSTO,ISTORE,
     &         ESTO,KSTACK)
               ITHETA = ISTO(5)
               JPHI   = ISTO(6)
               KS     = ISTO(7)
               DO 1999 K = 1,NSTOFI
                  IF (KS.    NE.STOFIR(3,K)) GOTO 1999
                  IF (JPHI  .NE.STOFIR(2,K)) GOTO 1999
                  IF (ITHETA.NE.STOFIR(1,K)) GOTO 1999
                  EINTRA = EINTRA + ESTO(1)
1999           CONTINUE
            ENDDO
         ENDIF
      ENDIF
      TABHAD(5) = EINTRA
      TABHAD(6) = FLOAT(NST1)
      TABHAD(7) = FLOAT(NST2)
      TABHAD(8) = FLOAT(NST3)
      TABHAD(9) = R1
      TABHAD(10) = R2
      TABHAD(11) = ESUVS1+ESUVS2+ESUVS3
C Theta et phi du barycentre dans chaque stack:
      DO K = 1,3
         THEBA = -10.
         PHIBA = -10.
         IF ( CLUST(4*(K-1)+4,ICL).GT.0.0) THEN
            X = CLUST( 4*(K-1)+1,ICL ) - DELTA(1)
            Y = CLUST( 4*(K-1)+2,ICL ) - DELTA(2)
            Z = CLUST( 4*(K-1)+3,ICL ) - DELTA(3)
            RAYON = SQRT(X**2+Y**2)
            PHIBA = ATAN2(Y,X)
            IF (PHIBA.LT.0.0) PHIBA = PHIBA+TWOPI
            THEBA = ATAN2(RAYON,Z)
         ENDIF
         TABHAD (11+2*(K-1)+1) = THEBA
         TABHAD (11+2*(K-1)+2) = PHIBA
      ENDDO
      CALL EC4TRA(ITCLOS,EC41,EC42,EC43)
      TABHAD(18) = EC41
      TABHAD(19) = EC42
      CALL GHAFAK (TABHAD,PFAKE,KWARN,D12,D23)
      PF (4,I) = PFAKE
      PF (5,I) = FLOAT(KWARN)
C Additional variables (not in standard output) used for p_fake
C computation:
      GAMORE (9,I) = EINTRA
      GAMORE (10,I) = ESUVS1+ESUVS2+ESUVS3
      GAMORE (11,I) = D12
      GAMORE (12,I) = D23
      GAMORE (13,I) = CLUST(24,ICL)
C--------------------------------------------------------------------
C     Fin probabilite fake hadronique
C--------------------------------------------------------------------
C
C     End of the photon loop
C
 2000 CONTINUE
C
C
C---------------------------------------------------------------------
C     Calcul probabilite fake electromagnetique
C---------------------------------------------------------------------
C regarde l'energie de quatres tours centrales en contact avec
C les autres ec4 : EfrI(nterieur)F4 et EfrE(xterieur)F4
      DO  2001 I = 1 , NNGA
         ICL = KCLNOK(I)
Copie dans kf4gri la matrice 2*2*3 des storeys des 4 tours centrales
         CALL UCOPY (KLUM22(1,1,1,ICL), KF4GRI, 12)
         EFRIF4 = 0.
         EFREF4 = 0.
         DO IYET = 1 , NPEST
            YETSEE(IYET) = .FALSE.
         ENDDO
         DO IT = 1,2
            DO JF = 1,2
               DO 2002 KS = 1,3
                  ISTORE = KF4GRI(IT,JF,KS)
                  IF (ISTORE.EQ.0) GOTO 2002
                  CALL GIVSTO(ESTO1,ESTO2,ESTO3,INDSTO,
     &            ISTORE,ESTO,KSTACK)
                  EFRI   = ESTO(1)
                  FRONTI = .FALSE.
                  DO II = 1,NNGA
                     IF (II.NE.I) THEN
                        ICLVOI = KCLNOK(II)
                        CALL UCOPY(KLUM22(1,1,1,ICLVOI),KF4TAB,12)
                        DO IIT = 1,2
                           DO JJF = 1,2
                              DO 2003 KKS = 1,3
                                 JSTORE = KF4TAB(IIT,JJF,KKS)
                                 IF (JSTORE.EQ.0) GOTO 2003
                                 CALL GIVSTO(ESTO1,ESTO2,ESTO3,
     &                           INDSTO,JSTORE,EESTO,KSTACK)
                                 IF (VOISTO(ISTO(5),IISTO(5))) THEN
                                    FRONTI = .TRUE.
                                    IF (.NOT.YETSEE(JSTORE)) THEN
                                       EFREF4 = EFREF4 + EESTO(1)
                                       YETSEE(JSTORE) = .TRUE.
                                    ENDIF
                                 ENDIF
2003                          CONTINUE
                           ENDDO
                        ENDDO
                     ENDIF
                  ENDDO
                  IF (FRONTI) EFRIF4 = EFRIF4 + EFRI
2002           CONTINUE
            ENDDO
         ENDDO
         GFL (10,I) = EFRIF4
C---------------------------------------------------------------------
C     Additionnal information :(following)
C                               energies shared by two photons
C---------------------------------------------------------------------
         GAMORE(7,I) = EFRIF4
         GAMORE(8,I) = EFREF4

2001  CONTINUE

      DO I = 1,NNGA
         ICL = I
C       E:
         TABEM  (1,I) = GAMVE(1,I)
C       F4:
         TABEM  (2,I) = GAMVE(6,I)
C       Eraw:
         TABEM  (3,I) = GAMVE(12,I)
C       Theta barycentre:
         TABEM  (4,I) = GAMVE(17,I)
C       Phi   barycentre:
         TABEM  (5,I) = GAMVE(18,I)
C       Stack germination:
         XDUM          = GFL  (1,ICL)
         TABEM  (6,I) = FLOAT(IDUM)
C       Storey pic stack1:
         TABEM  (7,I) = GFL  (2,ICL)
C       Storey pic stack2:
         TABEM  (8,I) = GFL  (5,ICL)
C       Prfake stack1:
         TABEM  (9,I) = GFL  (3,ICL)
C       Prfake stack2:
         TABEM  (10,I) = GFL  (6,ICL)
C       Eraw cluster voisin storey pic stack1:
         TABEM  (11,I) = GFL  (4,ICL)
C       Eraw cluster voisin storey pic stack2:
         TABEM  (12,I) = GFL  (7,ICL)
C       Eraw cluster voisin storey pic stack3:
         TABEM  (13,I) = GFL  (8,ICL)
C       Flag EndCap changement de region:
         TABEM  (14,I) = GAMVE(5,ICL)
C       Energie des 4 tours centrales voisines autres 4 tours centrales:
         TABEM  (15,I) = GFL  (10,ICL)
C       Energie des 4 tours centrales voisine d'un crack :
         TABEM  (16,I) = GFL  (9,ICL)
c       Pour technique:
         TABEM  (17,I) = FLOAT(I)
C Additional variables (following):
         GAMORE(14,I) = FLOAT(IDUM)
         GAMORE(15,I) = GFL(2,ICL)
         GAMORE(16,I) = GFL(5,ICL)
         GAMORE(17,I) = GFL(3,ICL)
         GAMORE(18,I) = GFL(6,ICL)
      ENDDO
C-----------------------------------
C Look to fake elm
C--------------------------------------
      CALL GEMFAK (NNGA,TABEM,TABOUT)
      DO I = 1,NNGA
         PF (1,I) = TABOUT (3,I)
         JPARENT = 0
         DO J = 1,NNGA
             IF (J.NE.I) THEN
                IF (GAMVE(12,J).EQ.TABOUT(2,I)) THEN
                   JPARENT = J
                   GOTO 2004
                ENDIF
             ENDIF
         ENDDO
2004     CONTINUE
         PF (2,I) = FLOAT(JPARENT)
         PF (3,I) = TABOUT (1,I)
         GAMORE(19,I) = TABOUT (4,I)
         GAMORE(20,I) = TABOUT (5,I)
         GAMORE(21,I) = TABOUT (6,I)
      ENDDO
C---------------------------------------------------------------------
C     Fin probabilite fake electromagnetique
C---------------------------------------------------------------------
C
C
      GOTO 9999
C
C--  Too many clusters found way-out
C
  888 CONTINUE
      IRTFG = -4
C
 9999 CONTINUE
C
      RETURN
      END
#endif
