      SUBROUTINE UFVDMS( FIELD, OMEGA, TANL, PHI0, D0, Z0,
     &  R_STOP, R_START, QMS )

C! GET MULTIPLE SCATTERING IN VDET AND BEAM-PIPE IN A REALISTIC
C! APPROXIMATION FOR THE VDET GEOMETRY
C!
C!    AUTHOR:   T. LOHSE   8.12.1990
C!              G. Taylor 23.9.1992
C!              - Add option of allowing a more detailed treatment of th
C!                multiple scattering in the Vdet to be provided
C!                through a VDMS bank 0
C!              D. Casper 6.04.1995
C!              - Rework for energy loss
C!                        2.04.1997
C!              - Protect against very high incidence angle tracks
C!
C!    INPUT:  FIELD  =  MAGNETIC FIELD IN KG (SINGLE PRECISION)
C!               THE REST IS DOUBLE PRECISION
C!            OMEGA  =  CURVATURE OF TRACK  (MUST BE .NE. 0)
C!            TANL   =  TAN(LAMBDA)
C!            PHI0   =  PHI-ANGLE OF TRACK AT ORIGIN
C!            D0     =  IMPACT PARAMETER OF TRACK
C!            Z0     =  Z AT TRACK ORIGIN
C!            R_STOP =  INNER RADIUS TO GO TO
C!                      IF NEGATIVE, FIT STOPS AT ABS(R_STOP)
C!            R_START=  OUTER RADIUS TO START AT
C!            QMS    =  2*2 MULTIPLE SCATTERING MATRIX AT R_START
C!    OUTPUT: QMS    ===>  CHANGED TO MULTIPLE SCATTERING MATRIX
C!                         UP TO R_STOP
C!
C!       ****    EVERYTHING DOUBLE PRECISION    ****
C!  Entry UFVDMM allows specifying mass of particle
C!
C!-------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "vdmsjj.h"
#include "bcs.h"
#include "vrldcom.h"
#include "uftcom.h"
C
      DOUBLE PRECISION OMEGA, TANL, PHI0, D0, Z0, R_STOP, R_START,
     &  QMS(2,2)
C
C----LOCAL VARIABLES
C
      DOUBLE PRECISION D_ARC, S_CURRENT, R_CURRENT, S_PIPE,
     +                 R_MAX, S_INNER, S_WAFER, R_INNER, R_TO_P,
     +                 SCACO, R_WAFER, RADIUS, SXY, XMS, COSV, R_PIPE
      INTEGER DO_ELOSS_IN, ELOSS_FLAG

      DOUBLE PRECISION UFBEBL, UFBERR
      DOUBLE PRECISION M_PION, DE_DX, X_RAD, DE, DE_TOT, SIG_DE, BETA,
     +                  GAMMA, BETA_GAMMA, P_TRK, L_CORR, MASS, MASS_IN,
     +                  DE_OUT, SIG_OUT
      DATA M_PION /0.13956995D0/

#include "bmacro.h"
C
      SXY(RADIUS)= 2.*DASIN(0.5*OMEGA*DSQRT(DABS((RADIUS**2-D0**2)
     &               /(1.-OMEGA*D0))))/OMEGA

C  Default pion mass

      ELOSS_FLAG = 0
      MASS = M_PION
      GOTO 101

      ENTRY UFVDMM( FIELD, OMEGA, TANL, PHI0, D0, Z0,
     &  R_STOP, R_START, MASS_IN, QMS, DO_ELOSS_IN, DE_OUT, SIG_OUT )

      MASS = MASS_IN
      IF(FIXED_OME .OR. FIXED_P) THEN
        ELOSS_FLAG = 0
      ELSE
        ELOSS_FLAG = DO_ELOSS_IN
      ENDIF

      DE_OUT = 0.D0
      SIG_OUT = 0.D0

  101 CONTINUE

C  Initialize multiple scattering parameters

      CALL VRLDGT(IER)
      IF(IER.NE.0) RETURN
C
      R_TO_P = 1000./0.29979/FIELD
      SCACO = 0.0136 * R_TO_P
      SCACO = (SCACO*OMEGA)**2/(1.+TANL**2)

C  Beta of track for dE/dx (assuming pion hypothesis)

      P_TRK = DABS(1.D0/(DBLE(R_TO_P) * OMEGA / DSQRT(1.D0+TANL**2)))
      BETA = P_TRK/DSQRT(P_TRK**2 + MASS**2)
      BETA_GAMMA = P_TRK/MASS
      GAMMA = BETA_GAMMA/BETA

C  Correct multiple scattering for low p tracks using beta

      SCACO = SCACO / BETA**2

      DE_TOT = 0.D0
      SIG_DE = 0.D0

C Start at R_OUTER

      R_CURRENT = R_START
      R_MAX = DABS( D0 - 2.D0 / OMEGA )
      IF ( R_CURRENT .GT. R_MAX ) R_CURRENT = R_MAX - 0.0001
      R_INNER = R_STOP
      IF ( R_INNER .LT. DABS(D0) ) R_INNER = DABS(D0)
      IF ( R_INNER .GT. R_MAX ) R_INNER = R_MAX - 0.0001

C Arc-length to inner and outer radii

      IF(DABS(0.5*OMEGA*DSQRT(DABS((R_CURRENT**2-D0**2)
     &     /(1.-OMEGA*D0)))).GT.0.9999999) RETURN
      S_CURRENT = SXY(R_CURRENT)

      KVDMS=NLINK('VDMS',0)
      IF(KVDMS.NE.0) THEN

C Do a more detailed treatment starting from the VDMS 0 bank
C This treatment also uses a better description of multiple
C scattering in 'thick' materials as given in the particle data book
C Assume for now that the 'VDMS' is ordered in decreasing radii

        DO I=1,LROWS(KVDMS)
          IFLAG=ITABL(KVDMS,I,JVDMFL)
          IF(IFLAG.NE.0) THEN
            R_WAFER=RTABL(KVDMS,I,JVDMRA)
            XMS=RTABL(KVDMS,I,JVDMSG)
            IF(XMS.LT.0.00001) XMS=.00001
            COSV=DBLE(ABS(RTABL(KVDMS,I,JVDMPV)))
            IF (COSV .LT. 0.05) COSV = 0.05

C  Check if inner radius is reached

            IF ( R_CURRENT .LE. R_INNER ) RETURN
            IF ( R_CURRENT .LE. D0 ) RETURN

C  Arc-length to outer radius

            S_CURRENT=SXY(R_CURRENT)
            IF(R_WAFER.LE.R_CURRENT.AND.R_WAFER.GT.R_INNER) THEN
              S_WAFER=SXY(R_WAFER)

C  Assume a tiny but finite width of the cylinder in order to avoid
C  numerical problems

              IF ( S_WAFER .GT. 0.00005 ) S_WAFER = S_WAFER - 0.00005

C  Transport to inner radius

              D_ARC = S_CURRENT - S_WAFER
              D_ARC = D_ARC * DSQRT( 1. + TANL**2 )
              QMS(1,1) = QMS(1,1)
     &          + D_ARC*(QMS(1,2)+QMS(2,1))+D_ARC**2*QMS(2,2)
              QMS(1,2) = QMS(1,2) + D_ARC*QMS(2,2)
              QMS(2,1) = QMS(2,1) + D_ARC*QMS(2,2)

C  Add multiple scattering at inner radius

              XMS = XMS/COSV
              L_CORR = MAX(0.7D0,1.D0+0.038D0*DLOG(XMS))**2
              QMS(2,2)=QMS(2,2)+ XMS * SCACO * L_CORR

C  Calculate energy loss

              IF(XMS.GT.0.)THEN
                DE_DX = UFBEBL(BETA_GAMMA, MASS, 1, ELOSS_FLAG)
                DE = DE_DX * XMS
                SIG_DE = SIG_DE + DE**2 * UFBERR(XMS)
                DE_TOT = DE_TOT + DE
              ENDIF

C  Update the outer radius to inner surface of this layer

              S_CURRENT = S_WAFER
              R_CURRENT = DSQRT( D0**2 + 4./OMEGA**2*(1.-D0*OMEGA)*
     +            DSIN(OMEGA*S_WAFER/2.)**2 )
            ENDIF
          ENDIF
        ENDDO
      ELSE
        CALL ALTELL('UFVDMS: no VDMS bank -> no ms in VDET',0,'RETURN')
      ENDIF

C  Track passed through vdet
C  now transport to beam pipe if necessary

      IF (R_CURRENT.GE.UKRVAC .AND. R_STOP.LT.UKRVAC)  THEN

C  Transport to beam pipe first

        IF ( UKRVAC .LE. ABS(D0) ) THEN
          S_PIPE = 0.
        ELSE
          R_PIPE = UKRVAC
          S_PIPE = SXY(R_PIPE)
        ENDIF
        D_ARC = ( S_CURRENT - S_PIPE ) * DSQRT( 1. + TANL**2 )
        QMS(1,1) = QMS(1,1)
     &    + D_ARC*(QMS(1,2)+QMS(2,1))+D_ARC**2*QMS(2,2)
        QMS(1,2) = QMS(1,2) + D_ARC*QMS(2,2)
        QMS(2,1) = QMS(2,1) + D_ARC*QMS(2,2)

C  Add ms and energy loss in beam pipe

        IF ( S_PIPE .GT. 0. )THEN
          XMS = UKSVAC * DSQRT(1.D0 + TANL**2)*OMEGA*UKRVAC
     +          /DSIN(OMEGA*S_PIPE)/(1.D0-OMEGA*D0)
          IF(XMS.GT.0.)THEN
            L_CORR = MAX(0.7D0,1.D0+0.038D0*DLOG(XMS))**2
          ELSE
            L_CORR = 1.D0
          ENDIF
          QMS(2,2) = QMS(2,2) + XMS*SCACO*L_CORR

          DE_DX = UFBEBL(BETA_GAMMA, MASS, 4, ELOSS_FLAG)
          DE = DE_DX * XMS
          SIG_DE = SIG_DE + DE**2 * UFBERR(XMS)
          DE_TOT = DE_TOT + DE
        ENDIF
        S_CURRENT = S_PIPE
      ENDIF

C  Now transport to inner radius

      IF ( R_INNER .LE. ABS(D0) ) THEN
        S_INNER = 0.
      ELSE
        S_INNER = SXY(R_INNER)
      ENDIF
      D_ARC = ( S_CURRENT - S_INNER ) * DSQRT( 1. + TANL**2 )
      QMS(1,1) = QMS(1,1) + D_ARC*(QMS(1,2)+QMS(2,1))+D_ARC**2*QMS(2,2)
      QMS(1,2) = QMS(1,2) + D_ARC*QMS(2,2)
      QMS(2,1) = QMS(2,1) + D_ARC*QMS(2,2)

      IF(ELOSS_FLAG .GT. 0)THEN
        DE_OUT = DE_TOT
        SIG_OUT = SIG_DE
      ENDIF

      RETURN
      END
#endif
