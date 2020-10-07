      SUBROUTINE QPI0DO
CKEY  QPI0DO / USER
C=====================================================================
C! Build pi0 candidates from GAMPEX photons taken in the GAT section +
C of alpha, and refit their Energy/momentum applying pi0 mass        +
C constraint 2*w1*w2(1-cos(theta12))=m(pi0)                          +
C we neglect the error on the angles of the 2 photons and find,      +
C with a Lagrange multipliers approximate solution, the refitted     +
C energies w1, w2 minimizing chi2=((E1-w1)/S1)**2+((E2-w2)/S2)**2    +
C with w1, w2 the refitted energies                                  +
C      E1, E2 the measured energies                                  +
C      S1, S2 the error on these energies                            +
C Results of this computation are stored in common GAMPI0            +
C                                                                    +
C Author J-P Lees  15 - Oct - 1992                                   +
C Modified for ALPHA J-P Lees  25 - Apr - 1994                       +
C                                                                    +
C=====================================================================
#ifndef DOC
#include "qcde.h"
#include "gampi0.h"
      DIMENSION PVEC1(4),PVEC2(4),PPI0(4)
      PARAMETER (MXPECO=500)
      DIMENSION NGPPEC(MXPECO)
      PARAMETER (MXGAM =1000)   ! Maximum number of photons
      PARAMETER (EPI0MA = 200.) ! maximum physical energy for pi0
      PARAMETER (EGAMIN = 0.3 ) ! minimum energy of photons used
C.. Flag for opt histogram filling
      COMMON / HIQPI0 / IHIPI0
#include "qmacro.h"
C-----------------------------------------------------------------------
C.. initialisation
      IQPI0 = 0
      NTPI0 = 0
      CALL VZERO(PI0MOM, 4*MXPI0)
      CALL VZERO(ITYPI0,MXPI0)
      CALL VZERO(IPI0GAM,2*MXPI0)
C
C.. Check gamma section existence
C
      IF(KNGAT.LE.0)THEN
        IQPI0 = 1
        GO TO 999
      ENDIF

C.. First count number of photons per PECO
      CALL VZERO(NGPPEC,MXPECO)
      DO I = KFGAT, KLGAT
        IPEC=0
        IF (XPGAC(I)) THEN
          IPEC  = KPGAPE(I)
        ELSE IF (XPGP(I)) THEN
          IPEC  = KPGPPE(I)
        ELSE IF (XEGP(I)) THEN
          IPEC  = KEGPPE(I)
        ENDIF
        IF (IPEC.LE.0) THEN
          WRITE (IW(6),*)'## QPI0DO## WARNING: IPEC=',IPEC, '<=0!!!'
        ELSEIF (IPEC.LE.MXPECO) THEN
          NGPPEC(IPEC) = NGPPEC(IPEC)+1
        ELSE
          WRITE (IW(6),*)'## QPI0DO## WARNING: IPEC=',IPEC, '>MXPECO!'
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C.. Loop on Photons
      NPI0 = 0
      DO I=KFGAT,KLGAT-1
        DO J=I+1,KLGAT
          PVEC1(1) = QX(I)
          PVEC1(2) = QY(I)
          PVEC1(3) = QZ(I)
          PVEC1(4) = QE(I)
C Photons from PGAC bank :
          IF (XPGAC(I)) THEN
            IPEC1 = KPGAPE(I)
            DIST1 = QPGPDM(I)
C Photons from old PGPC bank ( old datasets ) :
          ELSE IF (XPGP(I)) THEN
            IPEC1 = KPGPPE(I)
            DIST1 = QPGPDM(I)
C Photons from VERY old EGPC bank ( VERY old datasets ) :
          ELSE
            IPEC1 = KEGPPE(I)
            DIST1 = QEGPDM(I)
          ENDIF
          PVEC2(1) = QX(J)
          PVEC2(2) = QY(J)
          PVEC2(3) = QZ(J)
          PVEC2(4) = QE(J)
          IF (XPGAC(J)) THEN
            IPEC2 = KPGAPE(J)
            DIST2 = QPGPDM(J)
          ELSE IF (XPGP(J)) THEN
            IPEC2 = KPGPPE(J)
            DIST2 = QPGPDM(J)
          ELSE
            IPEC2 = KEGPPE(J)
            DIST2 = QEGPDM(J)
          ENDIF
C.. Computes mass and energy
          AMASS = (PVEC1(4)+PVEC2(4))**2-
     +            (PVEC1(1)+PVEC2(1))**2-
     +            (PVEC1(2)+PVEC2(2))**2-
     +            (PVEC1(3)+PVEC2(3))**2
          IF (AMASS.GT.0.) THEN
            AMASS=SQRT(AMASS)
          ELSE
            AMASS=0.
          ENDIF
          EPI0 = PVEC1(4)+PVEC2(4)
C.. Compute pi0 type
          ITYP = 0
          IF (IPEC1.EQ.IPEC2) THEN
            IF (NGPPEC(IPEC1).EQ.2) THEN
              ITYP = 1
            ELSEIF (NGPPEC(IPEC1).GT.2) THEN
              ITYP = 2
            ELSE
              WRITE (IW(6),*)'### QPI0DO ###: ERROR IN PI0 CLASS'
              WRITE (IW(6),*)' please advise J.P.Lees of this problem'
            ENDIF
          ELSE
            IF (NGPPEC(IPEC1).EQ.1.AND.NGPPEC(IPEC2).EQ.1) THEN
              ITYP = 3
            ELSE
              ITYP = 4
            ENDIF
          ENDIF
C
C  A pi0 candidate is any photon-photon pair with invariant mass
C  between mpi0-2*sigma and mpi0+2*sigma
C  mpi0 and sigma are defined in subroutine PI0LIM
C
          CALL PI0LIM(EPI0,AMEAN,SIGM)
          AMMIN = AMEAN-2.*SIGM
          AMMAX = AMEAN+2.*SIGM
C.. Unphysical pi0's ( EPI0 > EPI0MA=200 GEV)
C.. and photons E < 300MeV      are removed
          IF (PVEC1(4).GT.EGAMIN.AND.PVEC2(4).GT.EGAMIN
     +        .AND.EPI0.LT.EPI0MA)  THEN
            IF (IHIPI0.EQ.1) THEN !optional filling of histos
              CALL HF2(9001,EPI0,AMASS,1.)
              IF (ITYP.GT.0) CALL HF2(9001+ITYP,EPI0,AMASS,1.)
            ENDIF
C.. Build pi0 if m(gamma-gamma) at 2 sigma of M pi0
            IF (AMASS.GT.AMMIN.AND.AMASS.LT.AMMAX) THEN
              NPI0 = NPI0+1
C.. for photons in diff PECOS neglect error on angle
              IF (ITYP.GT.2) THEN
                CALL XXP0F1(IRET,PVEC1,PVEC2,PPI0,XMFIT,CHI2)
              ELSE
C.. for photons in same PECOs refit both energies and angle :
                CALL KINEFIT(PVEC1,  PVEC2, PPI0,
     +                       CHI2, PROBP0, IFAIL)
                IRET = 0
                IF (IFAIL.EQ.0) IRET=1 ! The fit worked
              ENDIF
              IF (IRET.EQ.6) WRITE (IW(6),*)' ## QPI0DO ##',
     +          ' WARNING: pi0 fit did not converge, iret=',IRET
              IF (IRET.LE.0.OR.IRET.GT.6) THEN
                WRITE (IW(6),*)' WARNING: pi0 fit failed,iret=',IRET
                CALL VADD(PVEC1,PVEC2,PPI0,4)
                CHI2 = -999.
              ENDIF
C.. Add one row.
              IF (NPI0.LE.MXPI0) THEN
                NTPI0 = NPI0
                CALL UCOPY(PPI0,PI0MOM(1,NTPI0),4)
                CHIPI0(NTPI0)  = CHI2
                IPI0GAM(1,NTPI0) = I
                IPI0GAM(2,NTPI0) = J
                ITYPI0(NTPI0) = ITYP
              ELSE
                IQPI0 = 2
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
 999  RETURN
      END
#endif
