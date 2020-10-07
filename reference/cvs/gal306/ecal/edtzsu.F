*DK edtzsu
      SUBROUTINE EDTZSU
C----------------------------------------------------------------
C     O.CALLOT   27-NOV-85
C! Zero suppression in towers
C. - called from ECDIGI                                 this .HLB
C. - calls       EDTCUT,EDTADJ                          this .HLB
C.               VFILL ,VZERO ,UCOPY                    CERNLIB
C----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "ecnamc.h"
#include "ecsize.h"
#include "ectmap.h"
#include "edcond.h"
      PARAMETER (MNTAG=1000,MXTAG=-1)
      DIMENSION LIVOI(2,10)
      EXTERNAL EDTCUT
      INTEGER EDTCUT
#include "bmacro.h"
C ----------------------------------------------------------
C
      DO 2 I=1,LTHET
        JPMIN(I) = MNTAG
        JPMAX(I) = MXTAG
   2  CONTINUE
C
      IF(IDETTM .EQ. 0 ) GOTO 980
      NETTM = LROWS(IDETTM )
      LETTM = LCOLS (IDETTM)
      KETTM = IDETTM + LMHLEN
      NTOT  = 0
C
C  ===  first, select by cut level 1 on storey
C
      DO 10 I=1,NETTM
        JPHI = IW(KETTM+1)
        KTET = IW(KETTM+2)
        DO 20 L=1,3
          IF(IW(KETTM+2+L).GE.EDTCUT(1,L,JPHI,KTET))  GO TO 30
   20   CONTINUE
        GO TO 40
  30    IF(JPMIN(KTET).EQ.MNTAG) THEN
          DO 35 J=1,LPHI
            MAPECT(J,KTET) = 0
  35      CONTINUE
        ENDIF
        MAPECT(JPHI,KTET) = 1
        IF(JPHI.LT.JPMIN(KTET)) JPMIN(KTET) = JPHI
        IF(JPHI.GT.JPMAX(KTET)) JPMAX(KTET) = JPHI
        NTOT = NTOT + 1
   40   KETTM = KETTM + LETTM
   10 CONTINUE
C
      IF(EDZSUP.EQ.'SINGLE CUT') GO TO 500
      IF(EDZSUP.NE.'DOUBLE CUT') GO TO 900
C
C  ===  compute level 2 cuts, and clear level1 if no level2 near...
C
      KETTM = IDETTM + LMHLEN
      DO 50 I=1,NETTM
        JPHI = IW(KETTM+1)
        KTET = IW(KETTM+2)
        IF(MAPECT(JPHI,KTET).EQ.0) GO TO 60
        DO 70 L=1,3
          IF(IW(KETTM+2+L).GE.EDTCUT(2,L,JPHI,KTET)) GO TO 80
   70   CONTINUE
        GO TO 60
   80   MAPECT(JPHI,KTET) = 2
   60   KETTM = KETTM + LETTM
   50 CONTINUE
C
      DO 100 K=1,LTHET
        IF(JPMIN(K).EQ.MNTAG) GO TO 100
        DO 110 J=JPMIN(K),JPMAX(K)
          IF(MAPECT(J,K).NE.1) GO TO 110
          CALL EDTADJ(J,K,NBVOI,LIVOI)
          DO 120 I=1,NBVOI
      IF(JPMIN(LIVOI(2,I)).EQ.MNTAG)   GO TO 120
            IF(MAPECT(LIVOI(1,I),LIVOI(2,I)).EQ.2) GO TO 110
  120     CONTINUE
          MAPECT(J,K) = 0
          NTOT = NTOT - 1
  110   CONTINUE
  100 CONTINUE
C
C  ===  now, output in bank 'ETDI' only tower tagged in MAPECT
C
  500 CONTINUE
      LETDI = 4
      CALL ALBOS( 'ETDI',0, LMHLEN+NTOT*LETDI, JETDI, IGARB)
      IW( JETDI+LMHCOL) = LETDI
      IW( JETDI+LMHROW) = NTOT
      CALL BLIST (IW,'E+','ETDI')
C
C  ===  create bank 'ENDI' to save noise and gain used in digitisation
C
      IF(EDSAVG.EQ.'YES') THEN
        LENDI = 7
        CALL ALBOS('ENDI',0,LMHLEN+NTOT*LENDI,JENDI,IGARB)
        CALL BLIST (IW,'E+','ENDI')
        IW( JENDI+LMHCOL) = LENDI
        IW( JENDI+LMHROW) = NTOT
        KENDI = JENDI + LMHLEN
        IF (IGARB.NE.0) JETDI = IW(NAETDI)
      ENDIF
      KETDI = JETDI + LMHLEN
      KETTM = IDETTM + LMHLEN
      DO 210 K=1,LTHET
        IF(JPMIN(K).EQ.MNTAG) GO TO 210
        DO 220 J=JPMIN(K),JPMAX(K)
          IF(MAPECT(J,K).EQ.0) GO TO 220
  230     JPHI = IW(KETTM+1)
          KTET = IW(KETTM+2)
C
C  === the tower are in IW(KETTM+..) in the good order, but some are ign
C
          IF(J.NE.JPHI.OR.K.NE.KTET) THEN
            KETTM = KETTM + LETTM
            GO TO 230
          ENDIF
          IW(KETDI+1) = ISHFT(KTET,16) + ISHFT(JPHI,2)
          DO 228 L=2,LETDI
            IW(KETDI+L) = IW(KETTM+L+1)
 228      CONTINUE
          IF(EDSAVG.EQ.'YES') THEN
            IW(KENDI+1) = IW(KETDI+1)
            DO 224 L=2,LENDI
              IW(KENDI+L) = IW(KETTM+L+4)
 224        CONTINUE
            KENDI = KENDI + LENDI
          ENDIF
          KETDI = KETDI + LETDI
          KETTM = KETTM + LETTM
  220   CONTINUE
  210 CONTINUE
  980 RETURN
C
  900 WRITE(LOUTIO,1000) EDZSUP
 1000 FORMAT(' +++EDTZSU+++ condition ',A16,' is not implemented.')
      STOP
      END
#endif
