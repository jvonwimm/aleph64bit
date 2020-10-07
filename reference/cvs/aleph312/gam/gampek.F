      SUBROUTINE GAMPEK(IPECO,EMIGA,LTOTGA,NNGA,GAMVE,IRTFG)
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
C                   LTOTGA = 20*NPHOT
C                   in GAMPEK: GAMVE is dimensioned to (20,*)
C                   the maximum number of photons will be: LTOTGA/20
C   Output:
C
C            NNGA  Number of photons
C
C            GAMVE(I  , Photon number)  Photon Array
C                 (1-3,    Px , Py , Pz
C                 (4       Energy of the photon
C                 (5,      # of subtruc. found by EBTRAN
C                 (6,      F4
C                 (7,   = FLOAT( ICRK )  , ICRK=1 IF ECAL CRAC
C                 (8,   = FLOAT( NST1 + 100*NST2 + 10000*NST3)
C                       where  nstX is the # of storey in stack X
C                 (9    = Estack1/ETOT
C                 (10,  = Estack2/ETOT
C                 (11,  = FLOAT( MORS1 + 10*MORS2 + 100*MORS3)
C                         where  morsX is = 1 if there a dead storey
C                         in central 3 x 3 matrix in stack X
C                 (12,  = Eraw
C                 (13,  = flag about spark
C                 (14,  = itheta peak
C                 (15,  = jphi   peak
C                 (16,  = min.dist. to ch.track with barycen.
C                 (17,  = theta of photon from EBNEUT (like-S curve)
C                 (18,  = phi   of photon from EBNEUT (like-S curve)
C                 (19-20, = 0. (NOT USED)
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
C                 = -7 ECAL geometry package not initialized before calling
C                      GAMPEK
C
C   Calls: BGETDS,EBARYC,ECLEAK,EBTRAN,GATRAL,GAFORM,GAGRID,ISPARK
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "pestjj.h"
#include "etdijj.h"
#include "pfrfjj.h"
#include "deadst.h"
#include "comcux.h"
#include "ecoxa.h"
      PARAMETER(JAPEX1=1  , JAPEXX =2 , JAPEXY=3 , JAPEXZ=4)
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
C input dim
      PARAMETER (NCOLGA = 20)
      DIMENSION GAMVE(NCOLGA,*)
C
C--for EBARYC
      DIMENSION IKOD(9) , BARY(2)
C
C for GAGRID
      DIMENSION LISTOR(3,NSTGAX),ESTOR(NSTGAX)
      DIMENSION LST33(3,6,3)
C
C for EBTRAN
      DIMENSION NATU(3) , GAMM(8)
C
C--for GAFORM
      DIMENSION LSTK(3,NSTGAX) , ESTK(NSTGAX)
C
C--for ECLEAK
      DIMENSION STACE(3)

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
      DIMENSION NSTCL (NKLUMX)   ,  ISTCL(NKLUMX)
      DIMENSION KCLNOK(NKLUMX)   ,  QGAM(4,NKLUMX)
      DIMENSION CLUST(24,NKLUMX) ,  ICLUST(24,NKLUMX)
      EQUIVALENCE (CLUST,ICLUST)
C
C arrays photons
      DIMENSION PGAMMA(3)   ,  DELTA (3)
C
C arrays reconstruction parameters
      DIMENSION CUTECL(3)
C
      DIMENSION PUGA(4)
C
      LOGICAL VOISTO
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C data for ecleak
      DATA MTC/0/
      DATA STACE/0.,0.,0./
#include "bmacro.h"
C
      IF(FIRST) THEN
        JECGN  = IW(NAMIND('ECGN'))
        IF ( JECGN.LE.0 ) THEN
          IRTFG = - 7
          GOTO 9999
        ENDIF
C
C --- max number of photon per peco cluster
        NFOMAX = LTOTGA / NCOLGA
        NEWPLA = ITABL(JECGN,1,7)
        NGPRUN  = 0
        NGPEVT  = 0
C
C?   Get basic parameters
C
        CALL DEFOVE
        FIRST  = .FALSE.
      ENDIF
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
      NPYER  =  0
      JPYER = IW( NAMIND('PYER') )
      IF(JPYER .GT. 0 ) NPYER = LROWS( JPYER )
C
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
C
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
C
C Loop over the storeys
C ---------------------
      ISTFND1 = 0
      ISTFND2 = 0
      ISTFND3 = 0
C
      NESTOR = 0
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
        IF(NX .EQ. 0 ) THEN
          NX = NX + 1
          CALL EMDTOW(IT,JF,ISCO,IMDO,IRG)
          MODUL = IMDO + (ISCO-1) * 12
        ENDIF
C
C Loop over stacks
C ----------------
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
        IF(ENSTO.LT.CUTENER) GO TO 1
C
C      Logical stack number
C
        KSLG=KS
        IF( KS.LT.3 .AND. ( IT.GE.ITOV1.AND.IT.LE.ITOV2 .OR.
     >          IT.GE.ITOV4.AND.IT.LE.ITOV3   ) )
     >          KSLG = KSLG + 1
C
        NESTOR = NESTOR + 1
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
C
C find the vertex of the event
C ----------------------------
      DO 2 JX =  1 , 3
    2 DELTA(JX) = 0.
      IF(NPYER .LE. 0 ) GO TO 4
      DO 3 I=1,NPYER
        IMVER = ITABL(JPYER,I,JAPEX1)
        IF(IMVER .NE. 1 ) GO TO 3
        DELTA(1) = RTABL(JPYER,I,JAPEXX)
        DELTA(2) = RTABL(JPYER,I,JAPEXY)
        DELTA(3) = RTABL(JPYER,I,JAPEXZ)
        GO TO 4
    3 CONTINUE
    4 CONTINUE
C
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
C
C
C Stack # 1 Highest storey in the first cluster
C ---------------------------------------------
      NCLU = 0
      DO 6 I = 1 , ISTFND1
        DO  J =1 , I-1
          IF (  VOISTO(ISTO1(5,I),ISTO1(5,J) ) ) THEN
            ISTO1(8,I) = ISTO1(8,J)
            GO TO 6
          ENDIF
        ENDDO
C
C   Isolated  storey , open cluster if O.K.
C        and check charged tracks distance
C        Energy  > Threshold
C -------------------------------------------
        KS = ISTO1(7,I)
        IF ( ESTO1(1,I) .LT. CUTECL(KS) ) GO TO 6
        NCLU = NCLU + 1
        ISTO1(8,I) = NCLU
C
C Charged cluster ?
C -----------------
        TRD = 999999.
        DO   J = 1 , NCTK
          TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),ESTO1(2,I)) )
        ENDDO
        IF ( TRD .LE. CUTRKX) ISTO1(8,I) = -ISTO1(8,I)
    6 CONTINUE
C
C Stack # 2
C ---------
      DO 16 I = 1 , ISTFND2
        DO J = 1 , ISTFND1
          IF(ISTO1(8,J).NE.0)THEN
            IF( VOISTO ( ISTO2(5,I),ISTO1(5,J) ) ) THEN
              ISTO2(8,I) = ISTO1(8,J)
              GO TO 16
            ENDIF
          ENDIF
        ENDDO
C       Look for a neighbour in stack # 2
        DO  J =1 , I-1
          IF (  VOISTO ( ISTO2(5,I),ISTO2(5,J) ) ) THEN
            ISTO2(8,I) = ISTO2(8,J)
            GO TO 16
          ENDIF
        ENDDO
C   Isolated  storey , open cluster if O.K.
C        and check charged tracks distance
C        Energy  > Threshold
C -----------------------------------------
        KS = ISTO2(7,I)
        IF ( ESTO2(1,I) .LT. CUTECL(KS) ) GO TO 16
        NCLU = NCLU + 1
        ISTO2(8,I) = NCLU
C
C Charged cluster ?
C -----------------
        TRD = 999999.
        DO   J = 1 , NCTK
          TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),ESTO2(2,I)) )
        ENDDO
        IF ( TRD .LE. CUTDIST2) ISTO2(8,I) = -ISTO2(8,I)
   16 CONTINUE
C
C Second pass in stack 1
C ----------------------
      DO 36 I = 1 , ISTFND1
C
C unassociated storeys only
C -------------------------
        IF( ISTO1(8,I) .EQ.0 )THEN
          DO J = 1 , ISTFND2
            IF(ISTO2(8,J).NE.0)THEN
              IF( VOISTO ( ISTO1(5,I),ISTO2(5,J) ) ) THEN
                ISTO1(8,I) = ISTO2(8,J)
                GO TO 36
              ENDIF
            ENDIF
          ENDDO
        ENDIF
   36 CONTINUE
C
C   Stack # 3
C   ---------
      DO 26 I = 1 , ISTFND3
C
C  look first in stack 2 for a neighbour
C---------------------------------------
        DO J = 1 , ISTFND2
          IF(ISTO2(8,J).NE.0)THEN
            IF( VOISTO ( ISTO3(5,I),ISTO2(5,J) ) )THEN
              ISTO3(8,I) = ISTO2(8,J)
              GO TO 26
            ENDIF
          ENDIF
        ENDDO
C
C look for a neighbour in stack # 3
C ---------------------------------
        DO  J =1 , I-1
          IF (  VOISTO ( ISTO3(5,I),ISTO3(5,J) ) ) THEN
C
C Same cluster as the neighbour
C -----------------------------
            ISTO3(8,I) = ISTO3(8,J)
            GO TO 26
          ENDIF
        ENDDO
C   Isolated  storey , open cluster if O.K.
C        and check charged tracks distance
C        Energy  > Threshold
C ---------------------------------------------
        KS = ISTO3(7,I)
        IF ( ESTO3(1,I) .LT. CUTECL(KS) ) GO TO 26
        NCLU = NCLU + 1
        ISTO3(8,I) = NCLU
C
C Charged cluster or not
C ----------------------
        TRD = 999999.
        DO   J = 1 , NCTK
          TRD = AMIN1 ( TRD , TRDIST(QEXP(1,J),ESTO3(2,I)) )
        ENDDO
        IF ( TRD .LE. CUTDIST3) ISTO3(8,I) = -ISTO3(8,I)
   26 CONTINUE
C
C Second pass in stack 2
C ----------------------
      DO 46 I = 1 , ISTFND2
C
C unassociated storeys only
C -------------------------
        IF( ISTO2(8,I) .EQ.0 )THEN
          DO J = 1 , ISTFND3
            IF(ISTO3(8,J).NE.0)THEN
              IF( VOISTO ( ISTO2(5,I),ISTO3(5,J) ) ) THEN
                ISTO2(8,I) = ISTO3(8,J)
                GO TO 46
              ENDIF
            ENDIF
          ENDDO
        ENDIF
   46 CONTINUE
C
C clusters global variables
C -------------------------
      IF(NCLU.GT.NKLUMX)THEN
        IRTFG = -4
        GO TO 9999
      ENDIF
      DO I = 1 , NCLU
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
          IF ( ICLUST(18,LCLU)+ICLUST(19,LCLU) .EQ. 0 ) THEN
C
C Flag hidden part of end cap
C ---------------------------
            IF( ( ISTO1(5,I).GE.ITOV1 .AND. ISTO1(5,I).LE.ITOV2 )  .OR.
     &          ( ISTO1(5,I).GE.ITOV4 .AND. ISTO1(5,I).LE.ITOV3 ) )
     &          ICLUST(17,LCLU) = ICLUST(17,LCLU) + 2
          ENDIF
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
          IF ( ICLUST(18,LCLU)+ICLUST(19,LCLU) .EQ. 0 ) THEN
C
C flag the hidden part of end cap
C -------------------------------
            IF( ( ISTO2(5,I).GE.ITOV1 .AND. ISTO2(5,I).LE.ITOV2 )  .OR.
     &        ( ISTO2(5,I).GE.ITOV4 .AND. ISTO2(5,I).LE.ITOV3 ) )
     &        ICLUST(17,LCLU) = ICLUST(17,LCLU) + 2
          ENDIF
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
C
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
     X            ICLUST(17,I) = ICLUST(17,I) + 1
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
C              IF ( TRD .LE. CUTRKX * RAYON / RESTK1 )
C    X            ICLUST(17,I) = ICLUST(17,I) + 4
C
C new part here
            IF( TRD .LE. CUTRKX * RAYON / RESTK1 ) THEN
              IF(ICLUST(18,I) .GT. 0 ) THEN
                ICLUST(17,I) = ICLUST(17,I) + 4
              ELSE
                ICLUST(17,I) = ICLUST(17,I) + 1
              ENDIF
            ENDIF
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
C
C storeys info . for each cluster
C --------------------------------
      DO I = 1 , NKLUMX
        NSTCL(I)=0
        ISTCL(I)=0
      ENDDO
      DO I = 1 , NCLU
        IF(I.EQ.1)THEN
          ISTCL(I) = 1
        ELSE
          ISTCL(I) = ISTCL(I-1) + NSTCL(I-1)
        ENDIF
        IFREE = ISTCL(I)
C
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
        IF ( ICLUST(17,I).NE. 4 ) THEN
          RAYON=0.
          DO K = 1,3
            RAYON = RAYON + (CLUST(12+K,I)-DELTA(K))**2
          ENDDO
          RAYON=SQRT(RAYON)
          QGAM (4,I) = CLUST(16,I)
          DO K = 1,3
            PGAMMA(K) = CLUST(16,I) * (CLUST(12+K,I)-DELTA(K))/RAYON
            QGAM(K,I) = PGAMMA(K)
          ENDDO
        ELSE
          QGAM (4,I) = CLUST(4,I) + CLUST(8,I)
          RAYON = 0.
          IF( QGAM (4,I).GT. 0.01 )THEN
            DO K = 1,3
              PGAMMA(K)=CLUST(4,I)*CLUST(K,I)+CLUST(8,I)*CLUST(K+4,I)
              PGAMMA(K) = PGAMMA(K) / QGAM(4,I)
              RAYON = RAYON + (PGAMMA(K)-DELTA(K))**2
            ENDDO
            RAYON = SQRT(RAYON)
            DO K = 1,3
              QGAM(K,I) = QGAM(4,I) * (PGAMMA(K)-DELTA(K)) / RAYON
              PGAMMA(K) = QGAM (K,I)
            ENDDO
          ELSE
            DO K = 1,3
              PGAMMA(K) = 0.
              QGAM(K,I) = 0.
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C KOK12 = 1 if stack 1/2 ok for gammas
C KOK23 = 1 if stack 2/3 ok for gammas
C                        at least CUTECLOK GeV in energy
C ------------------------------------------------------
C
      NCLNOK = 0
      DO I = 1,NCLU
        MOD1   =  MOD(ICLUST(17,I),4)
C        ECOXX  = CLUST(4,I)+CLUST(8,I)+CLUST(12,I)
        ECOXX  = CLUST(4,I)+CLUST(8,I)
        NST3   = ICLUST(20,I)
        NST2   = ICLUST(19,I)
        NST1   = ICLUST(18,I)
        NSTOT  = NST1 + NST2 + NST3
        IST12  = 0
        IF(NST1 .GT. 0 .AND. NST2 .GT. 0 ) IST12 = 1
        IST23  = 0
        IF(NST2 .GT. 0 .AND. NST3 .GT. 0 ) IST23 = 1
        IGSTO = 0
        IF(IST12 .EQ. 1 .AND. KOK12 .EQ. 1) IGSTO=1
        IF(IST23 .EQ. 1 .AND. KOK23 .EQ. 1) IGSTO=1

        EGGG   = QGAM(4,I)

        IF( MOD1   .EQ.  0                   .AND.
     &    ECOXX      .GT.  CUTECLOK            .AND.
     &    IGSTO      .EQ.  1                   .AND.
     &    EGGG       .GT.  EMIGA                    )  THEN
          NCLNOK = NCLNOK + 1
          KCLNOK(NCLNOK) = I
        ENDIF
      ENDDO
C
  999 CONTINUE
C
      NNGA = NCLNOK
      IF( NNGA .LE. 0 ) GO TO 9999
C
      JEVEH = IW(NAMIND('EVEH'))
      NRU   = IW(JEVEH+ 2)
C
      IF(NRU .NE. NGPRUN)  THEN
        CALL BGETDS
      ENDIF
C
C
      DO  2000 I = 1 , NNGA
        IF(I  .GT.  NFOMAX) THEN
          IRTFG = -5
          GO TO 9999
        ENDIF
        ICL = KCLNOK(I)
        EGRAW = QGAM(4,ICL)
        DISA  = CLUST(24,ICL)
        DO JX = 1 , 3
          PUGA(JX) = QGAM(JX,ICL)/EGRAW
        ENDDO
        PUGA(4) = 1.
        NS1 = ISTCL(ICL)
        NS2 = NS1 + NSTCL(ICL) - 1
        NUST= NSTCL(ICL)
        JST = 0
        NST1 = 0
        NST2 = 0
        NST3 = 0
        EST1 = 0.
        EST2 = 0.
        EST3 = 0.
C
        DO J = NS1 , NS2
          JST = JST + 1
          IF(JST .GT. NSTGAX) THEN
            IRTFG = -6
            GO TO 9999
          ENDIF
          LGASTO(I,JST) = LSTGA(J)
          LISTOR(1,JST) = INDEX(1,J)
          LISTOR(2,JST) = INDEX(2,J)
          LISTOR(3,JST) = INDEX(3,J)
          ESTOR(   JST) = ENDEX(J)
          KS = LISTOR(3,JST)
          IF(KS .EQ. 1 ) THEN
            NST1 = NST1 + 1
            EST1 = EST1 + ESTOR(JST)
          ENDIF
          IF(KS .EQ. 2 ) THEN
            NST2 = NST2 + 1
            EST2 = EST2 + ESTOR(JST)
          ENDIF
          IF(KS .EQ. 3 ) THEN
            NST3 = NST3 + 1
            EST3 = EST3 + ESTOR(JST)
          ENDIF
        ENDDO
        NNSTGA(I) = JST
C
        R1 = -1.
        R2 = -1.
        ETTT = EST1 + EST2 + EST3
        IF(ETTT .GT. 0.001) THEN
          R1 = EST1/ETTT
          R2 = EST2/ETTT
        ENDIF
C
        CALL EBARYC(NUST,LISTOR,ESTOR,IKOD,BARY)
C
        ICRK = 0
        IF(IKOD(8) .EQ. 1 .OR. IKOD(8) .EQ. 2 ) ICRK = 1
        IF(IKOD(8) .EQ. 3 .OR. IKOD(8) .EQ. 4 ) ICRK = 1
C
C
        LM = LVMAX(ESTOR,NUST)
        ITPIC = LISTOR(1,LM)
        JFPIC = LISTOR(2,LM)
        KSPIC = LISTOR(3,LM)
C
        ITMAX = ITPIC
        JFMAX = JFPIC
        STACE(1) = EGRAW
C
        CALL ECLEAK(MTC,STACE,BARY,MTC,CORR)
        ECOR  = EGRAW * CORR
C
C correction for saturation
C -------------------------
C         only for real data
C         ------------------
        IF(NRU .GT. 4000) THEN
          ECOR  = ECOR  * ( 1. + PSATU * ECOR)
        ENDIF
C
        CALL EBTRAN(NUST,LISTOR,ESTOR,BARY,NATU,GAMM)
        F4      = GAMM(1)
        ISINGA  = 1
        IF(NATU(2) .EQ. 2  .OR. NATU(3) .EQ. 3 ) ISINGA = 2
C
        ALCOEN = GATRAL(ITPIC,JFPIC,BARY(1),BARY(2))
C
        IF(ISINGA .EQ. 1 ) THEN
          EC4  = F4 * ECOR  / ALCOEN
        ELSE
          EC4 =  ECOR
        ENDIF
C
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
C
C FLAG ABOUT SPARK
C ----------------
        ICLAKA = ISPARK(MODUL,NEWPLA)


        DO JX = 1 , 4
          GAMVE(JX,I) = PUGA(JX) * EC4
        ENDDO
        GAMVE(5,I)  = FLOAT(ISINGA)
        GAMVE(6,I)  = F4
        GAMVE(7,I)  = FLOAT( ICRK )
        GAMVE(8,I)  = FLOAT( NST1 + 100*NST2 + 10000*NST3)
        GAMVE(9,I)  = R1
        GAMVE(10,I) = R2
        GAMVE(11,I) = FLOAT( MORS1 + 10*MORS2 + 100*MORS3)
        GAMVE(12,I) = EGRAW
        GAMVE(13,I) = FLOAT(ICLAKA)
        GAMVE(14,I) = FLOAT(ITMAX)
        GAMVE(15,I) = FLOAT(JFMAX)
        GAMVE(16,I) = DISA
        GAMVE(17,I) = BARY(1)
        GAMVE(18,I) = BARY(2)


 2000 CONTINUE


 9999 CONTINUE

      RETURN
      END
#endif
