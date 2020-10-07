      SUBROUTINE FIXRTRL
CKEY  FILL / INTERNAL
C =============================================================================
C! Recalculate RT/RL
C  Execution activated by the data card RTRL in the users CARDS
C  Called from QFILL
C                                 Author: M.N Minard 10/11/93
C
C-    For Monte-Carlo event
C         - Recalculate RL Julia Version 262-270
C         - Recalculate RT Database <185 and Geom 92
C-    For Data event
C         - Recalculate RT Julia Version 262-270, Db<185, 93 data
C         - Recalculate RT(ec) Julia Version 262-270, Db<185 , 92 data
C =============================================================================
#ifndef DOC
#include "qcde.h"
#include "evehjj.h"
#include "rhahjj.h"
#include "decojj.h"
      EXTERNAL CHAINT,NAMIND
      CHARACTER*4 CHAINT
      CHARACTER*8 PRNAM
      DIMENSION PORI(7),XTRAP(3,3),E4N(3)
      DATA  INUMS ,IFIR / 0,0 /
#include "qmacro.h"
C ------------------------------------------------------------------------------
C  Initialisations and checks to know if the correction has to be done :
      IF ( IFIR.EQ.0) THEN
         NAASIM = NAMIND('ASIM')
         IFIR =1
      ENDIF
C
C-    Check if UPDATE RTRL needed
C
      IF (IW(NAMIND('RTRL')).EQ.0) GO TO 990
        IMC = 0
C
C-    Check JULIA version number used
C-    Check Monte-Carlo version
        KRHAH = IW(NARHAH)
        KEVEH = IW(NAEVEH)
        KEIDT = IW(NAEIDT)
        KFRFT = IW(NAFRFT)
        KPECO = IW(NAPECO)
        KDECO = IW(NADECO)
        KASIM = IW(NAASIM)
        IF (KRHAH.EQ.0.OR.KEVEH.EQ.0) GO TO 990
C
C?   Get Run Number determine year type
C
        IMC = 0
        ITDT = 0
        IRUN = IW(KEVEH+JEVERN)
        IEVT = IW(KEVEH+JEVEEV)
        IF ( IRUN.LE.2000 ) THEN
          IGEOM = 0
          IMC = 1
          IF ( KASIM.NE.0) IGEOM = IW(KASIM+3)
          IF (IGEOM.GT.9200) IMC =2
        ELSE
          IF ( IRUN.GT.10000) ITDT = 1
          IF ( IRUN.GT.20000) ITDT = 2
        ENDIF
C
C-      For preceeding years do nothing in MC & Data
C
        IF ( IMC.EQ.0.AND.ITDT.EQ.0) GO TO 990
C
C-   Get last version of julia/db used for processing this event.
C
        IJVER = 0
        ILVER = 0
        IAVER = 0
        IACOR = 0
        IMVER = MINGTV(DUM)
        NRHAH=LROWS(KRHAH)
        DO 10 IRHAH = 1, NRHAH
          PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
          PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
          IF (PRNAM.EQ.'JULIA') THEN
            IJVER = ITABL(KRHAH,IRHAH,JRHAPV)
            ILVER = ITABL(KRHAH,IRHAH,JRHADV)
          ENDIF
   10   CONTINUE
C
C-   Do the correction ?
C-   Correction made if :
C-                        Julia processing 271 with alephlib 185 and
C
      ICORT = 0
      ICORL = 0
      IF ( IJVER.LT.271 .AND. IJVER.GT.262.AND.IMC.EQ.2) THEN
        ICORL = 1
      ENDIF
      IF (ILVER.LT.185.AND.IMC.EQ.2) THEN
        ICORT = 1
      ENDIF
      IF ( ILVER.LT.185.AND.ITDT.GE.1) THEN
        ICORT = 1
      ENDIF
C
C-    If no correction needed skip fix
C
      IF (ICORT+ICORL.EQ.0) GO TO 990
      IF (INUMS.EQ.0) THEN
       WRITE (IW(6),'(///,5(/,10x,a),///)')
     &'+------------------------------------------------------------+',
     &'|  Data Processed with Electron estimator correction         |',
     &'|      FIXRTRL called to Fix RT,RL Value in QDEIRI           |',
     &'|  !!!! Output banks are  equal to Input banks !!!!          |',
     &'+------------------------------------------------------------+'
       INUMS = 1
      ENDIF
C ------------------------------------------------------------------------------
C-    Extract RT/RL
C
      ICOR = ICORT + ICORL
      IF ( ICOR.GT.0) THEN
      DO IFCHT = KFCHT,KLCHT
C
C-    Check electron extrapolation
C
        IF (XEID(IFCHT)) THEN
C
C-    Keep RT/RL value
C
         RTO = QEIDRI(IFCHT,2)
         RLO = QEIDRI(IFCHT,3)
         EIC = QEIDEC(IFCHT)
C
C-    Take Track parameter
C
         PTT = QP(IFCHT)
         ICH = QCH(IFCHT)
         COST = ABS(QZ(IFCHT)/QP(IFCHT))
C
C-    Take Cluster parameter if any
C
         IREFT =KTN(IFCHT)
         IFRFT = IREFT
         IECL = 0
         IF(KEIDT.NE.0) THEN
           NEIDT = LROWS(KEIDT)
           DO IEL = 1, NEIDT
           ITR  = ITABL(KEIDT,IEL,JEIDFR)
             IF ( ITR.EQ.IREFT ) THEN
               IECL = ITABL(KEIDT,IEL,JEIDPE)
             ENDIF
           ENDDO
           IF ( KPECO.EQ.0) IECL=0
           IF ( KPECO.NE.0.AND.IECL.NE.0) THEN
             PHICL = RTABL(KPECO,IECL,JPECPH)
             THECL = RTABL(KPECO,IECL,JPECTH)
           ENDIF
         ENDIF
         IF ( IECL.EQ.0) THEN
C
C-   Calculate theta/phi angle of cluster
C
         PORI(1) = QX(IFCHT)
         PORI(2) = QY(IFCHT)
         PORI(3) = QZ(IFCHT)
         PORI(6) = ICH
         DO 70 IL =1,3
         PORI (3+IL) = PORI(IL)/QP(IFCHT)
 70      CONTINUE
         PORI ( 7) = QP(IFCHT)
         JFRFT = KROW(KFRFT,IFRFT)
         CP = COS (RW(JFRFT+JFRFP0))
         SP = SIN (RW(JFRFT+JFRFP0))
         D0 = RTABL ( KFRFT,IFRFT,JFRFD0)
         PORI ( 1) = D0*SP
         PORI (2) = - D0*CP
         PORI ( 3) = RTABL ( KFRFT,IFRFT,JFRFZ0)
         CALL QEXTRA(PORI,ICH,XTRAP,IFIR)
         IF (IFIR.NE.0) THEN
           I = IFIR
           PHICL = ATAN2(XTRAP(2,I),XTRAP(1,I))
           RDEL = SQRT(XTRAP(1,I)**2+XTRAP(2,I)**2+XTRAP(3,I)**2)
           THECL = ACOS(XTRAP(3,I)/RDEL)
           IECL = -1
           IF ( PHICL.LT.0) PHICL = PHICL+TWOPI
         ENDIF
       ENDIF
       IF ( IECL.EQ.0) THEN
         RLN = RLO
         RTN = RTO
       ELSE
C
C-      Calculate the row number in theta
C
         SK=1.
         CALL QETHPH(PHICL,THECL,SK,EPHI,ETET)
         TETIFL = ETET
         ISIDE = 1
         IF ( TETIFL.LT.51.OR.TETIFL.GT.178) ISIDE = ISIDE + 2
         IF ( TETIFL.LT.45.OR.TETIFL.GT.188) ISIDE = ISIDE - 1
         IF ( ICORT.NE.0 )THEN
           IDEL = 0
           IF (ITDT.EQ.1.AND.ISIDE.EQ.2) IDEL =1
           IF (ITDT.EQ.2 ) IDEL = 1
           IF (IMC.EQ.2) IDEL =3
           IF ( IDEL.NE.0.AND.RTO.LT.999.) THEN
             RTN = RTO
             CALL QCAF4C(QP(IFCHT),TETIFL,COST,IDEL,ISIDE,RTO,RTN)
           ELSE
             RTN = RTO
           ENDIF
         ELSE
           RLN = RLO
         ENDIF
C
C-       Calculate longitudinal profile
C
        IF ( ICORL.NE.0) THEN
          IDEL = 1
          IF ( RLO.LT.999.) THEN
           CALL QCABSA(RLO,QP(IFCHT),ISIDE,IDEL,RLN)
          ELSE
           RLN = RLO
          ENDIF
        ELSE
          RLN = RLO
        ENDIF
      ENDIF
C
C-    Update electron estimators
C
      RW(IW(KJQDET(IFCHT)+JQDEAE)+2+JEIDR1-1)=RTN
      RW(IW(KJQDET(IFCHT)+JQDEAE)+3+JEIDR1-1)=RLN
      ENDIF

      ENDDO
      ENDIF
 990  CONTINUE
      RETURN
      END
#endif
