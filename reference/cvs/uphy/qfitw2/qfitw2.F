************************************************************************

      SUBROUTINE QFITWW(EVTYP,IOPT,MASTOL_C,ITKS,IFTKS,COV,CHISQ,ISTAT)
*     ------------------------------------------------------------------
      PARAMETER    (MAXPRM=13, MAXINT=4)
      CHARACTER*4   EVTYP
      INTEGER       IOPT
      INTEGER       ITKS(4), IFTKS(7), IERR
      REAL          COV( MAXPRM, MAXPRM ), CHISQ
      REAL          MASTOL, MASTOL_C
      INTEGER       FITERR
      LOGICAL       MESSAG
*     ------------------------------
*     Minuit parameters are double precision
      EXTERNAL    CHISQR, KIN, FOURJT, TWOJTL
      DOUBLE PRECISION      PARAM(MAXPRM),STPSIZ(MAXPRM),ARG(10),ZERO
      DOUBLE PRECISION      EMAT( MAXPRM, MAXPRM )
      DOUBLE PRECISION      FMIN, FEDM, ERRDEF, RDCSTR, FULL
      LOGICAL     ISINIT, ISPHYS
      CHARACTER*6 PNAMES(MAXPRM)
      DATA STPSIZ  /MAXPRM*0.0/
      DATA ZERO /0.0/
*     ------------------------------
      COMMON /FP/ FIT(MAXPRM)
      DOUBLE PRECISION FIT
      COMMON /CS/  CCHISQ
      DOUBLE PRECISION CCHISQ
      COMMON /VP/ COPT
      INTEGER COPT
      COMMON /TK/ CITKS
      INTEGER CITKS(4)
      COMMON /J4/ IS4JET
      LOGICAL IS4JET
      COMMON /MT/ DM
      DOUBLE PRECISION  DM
      COMMON /M4/ D4M
      DOUBLE PRECISION   D4M
      COMMON /ST/ STRNGT
      DOUBLE PRECISION  STRNGT
      COMMON /SN/ IFAIL
      INTEGER IFAIL
      COMMON /VC/ P1(4), P2(4), P3(4), P4(4), W1(4), W2(4), ISRPHT(4)
      DOUBLE PRECISION P1, P2, P3, P4, W1, W2, ISRPHT

#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      ISTAT = 0
      ARG(1) = -1.0
      CALL MNEXCM( CHISQR, 'SET PRINT',ARG, 1,      IERR, KIN )
      IF( IOPT.eq.0.and.EVTYP.eq.'4jet' ) THEN
         COPT = 8175  ! Set Default Value
      ELSE IF( IOPT.eq.0 ) THEN
         COPT = 3     ! Set Default Value
      ELSE
         COPT = IOPT  ! set global variable
      ENDIF
      IF( COPT.lt.0.or.MASTOL_C.lt.0.0 ) ISTAT = -2 ! illegal input
      IF( COPT.gt.32767 ) ISTAT = -2
      IF( EVTYP.EQ.'4jet' ) THEN
        IF( ITKS(1)*ITKS(2)*ITKS(3)*ITKS(4).EQ.0 ) THEN
          PRINT *, 'Input ALPHA tracks to QFITWW not defined'
          PRINT *, 'KRUN',KRUN,'; KEVT',KEVT,'; KNEVT',KNEVT
          ISTAT = -2
        ENDIF
      ELSE
        IF( JBIT(COPT,11)*JBIT(COPT,12)*JBIT(COPT,13).NE.0 ) THEN
          PRINT *, 'You are not allowed to vary parameters:'
          PRINT *, '11, 12, or 13 in QFITWW while fitting'
          PRINT *, 'to WW -> 2jlv events'
          ISTAT = -2
        ENDIF
        IF( ITKS(1)*ITKS(2)*ITKS(3).EQ.0 ) THEN
          PRINT *, 'Input ALPHA tracks to QFITWW not defined'
          PRINT *, 'KRUN',KRUN,'; KEVT',KEVT,'; KNEVT',KNEVT
          ISTAT = -2
        ENDIF
      ENDIF
      IFAIL = 0
      IF( ISTAT.NE.-2 ) THEN
         IF( MASTOL_C.eq. 0.0 ) THEN
            MASTOL = 3.0 ! Set Default Value
         ELSE
            MASTOL = MASTOL_C
         ENDIF
         DM = DBLE(MASTOL)
         DO i = 1, 4
            CITKS(i) = ITKS(i)  ! Assign ITKS numbers to global variable
         ENDDO
         IF( EVTYP .eq. '4jet' ) THEN
            CALL FJTPAR( ITKS, PARAM ) ! Init PARAM array for 4jet
            IS4JET = .TRUE.
         ELSE
            CALL TJTPAR( ITKS, PARAM ) ! Init PARAM array for 2jlv
            IS4JET = .FALSE.
         ENDIF
         CALL SETSTP( ITKS, STPSIZ )
         ISINIT = .TRUE.  ! Init PARAM and store measured values in MINUIT
         CALL STOPRM( ISINIT, PARAM ,STPSIZ, MAXPRM )
         ISINIT = .FALSE. ! Init done, dont change measured values anymore
         IF( IFAIL.NE.-1 ) THEN  ! Continue if COVM is not singular.
            CALL RELPAR( COPT )  ! All PARAMs are fixed--release the vars
            ARG(1) = 0.0         ! Skip initialization from here on...
            IF( IS4JET ) THEN
               CALL FOURJT( CITKS,PARAM,P1,P2,P3,P4,W1,W2,ISRPHT )
            ELSE
               CALL TWOJTL( CITKS,PARAM,P1,P2,P3,P4,W1,W2,ISRPHT )
            ENDIF
            IF( JBIT(COPT,15).EQ.1 ) THEN
               CALL QFWFUN( P1,P2,P3,P4,ISRPHT,FULL )
               STRNGT = 1.0
               IF( FULL.GT.1.0 ) STRNGT = 1.0 / FULL**2
               DO WHILE( STRNGT.LT.1.0 )  ! Slowly turn on constraint term
                  IF( IS4JET ) THEN       ! for more accurate convergence
                     CALL MNEXCM( CHISQR,'MIGRAD',ARG,0,IERR,FOURJT )
                  ELSE
                     CALL MNEXCM( CHISQR,'MIGRAD',ARG,0,IERR,TWOJTL )
                  ENDIF
                  IF( IERR.EQ.0 ) THEN      ! if the fit was succesful, then
                     DO I = 1, MAXPRM       ! replace the last starting values
                        PARAM(i) = FIT(i)   ! with the newly fitted parameters.
                     ENDDO                  ! Otherwise, the last starting
                  ENDIF                     ! values are used again.
                  CALL MNEXCM( CHISQR, 'RESTORE', ARG, 0, ITEMP, KIN )
                  CALL STOPRM( ISINIT, PARAM, STPSIZ, MAXPRM )
                  CALL RELPAR( COPT )
                  STRNGT = 2.0 * STRNGT
               ENDDO
               STRNGT = 1.0
            ELSE
               IF(MASTOL.lt.100.0) DM  = 100.0
               D4M = 100.0  ! GeV
               DO WHILE( DM.GT.DBLE(MASTOL).OR.D4M.GT.0.001 )
                  IF( IS4JET ) THEN
                     CALL MNEXCM( CHISQR,'MIGRAD',ARG,0,IERR,FOURJT )
                  ELSE
                     CALL MNEXCM( CHISQR,'MIGRAD',ARG,0,IERR,TWOJTL )
                  ENDIF
                  IF( IERR.EQ.0 ) THEN       ! if the fit was succesful, then
                     DO I = 1, MAXPRM        ! replace the last starting values
                        PARAM(I) = FIT(I)    ! with the newly fitted parameters.
                     ENDDO                   ! Otherwise, the last starting
                  ENDIF                      ! values are used again.
                  CALL MNEXCM( CHISQR, 'RESTORE', ARG, 0, ITEMP, KIN )
                  CALL STOPRM( ISINIT, PARAM, STPSIZ, MAXPRM )
                  CALL RELPAR( COPT )
                  DM  = RDCSTR( DM, MASTOL )
                  D4M = RDCSTR( D4M, 0.001 )
               ENDDO
               DM = DBLE(MASTOL)
               D4M = 0.050
            ENDIF
            IF( IS4JET ) THEN
               CALL MNEXCM( CHISQR, 'MIGRAD', ARG, 0, IERR, FOURJT )
            ELSE
               CALL MNEXCM( CHISQR, 'MIGRAD', ARG, 0, IERR, TWOJTL )
            ENDIF
            CALL MNEMAT( EMAT, MAXPRM ) ! Get error matrix from MINUIT
            CALL MNSTAT( FMIN, FEDM, ERRDEF, NPARI, NPARX, ISTAT )
            CALL MNEXCM( CHISQR, 'RESTORE', ARG, 0, ITEMP, KIN )
            CALL VCTOTR( CITKS, FIT, IFTKS, IS4JET, ISPHYS )
            IF( .NOT.ISPHYS ) ISTAT = -1
            CHISQ = SNGL( CCHISQ )
            DO i = 1, MAXPRM
               DO j = 1, MAXPRM
                  COV(i,j) = SNGL( EMAT(i,j) ) ! RETURN single precison
               ENDDO                           ! error matrix
            ENDDO
            CALL TKERMT( IS4JET, ITKS, IFTKS, FIT, COV )
            CALL QVADD2( IFTKS(5), IFTKS(1), IFTKS(2) )
            CALL QVADD2( IFTKS(6), IFTKS(3), IFTKS(4) )
         ELSE                 ! If user defined COVM matrix is singular
            ISTAT = -2        ! then set illegal argument flag...
         ENDIF
      ENDIF
      IF( ISTAT.EQ.-2 ) THEN    ! If user passed illegal arguments to QFITWW
         DO I = 1, 7            ! then zero out the fitted 4-momenta
            IF( IFTKS(I) .EQ. NULL ) THEN
               IFTKS(I) = KVNEW( DUMMY )
            ELSE
               CALL VZERO( VECTOR, 4 )
               CALL QVSET4( IFTKS(I), VECTOR )
            ENDIF
         ENDDO
      ENDIF
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE QFWMSG
*     ------------------------------------------------------------------
      COMMON /J4/ IS4JET
      LOGICAL IS4JET
      COMMON /MT/ DM
      DOUBLE PRECISION DM
      COMMON /VP/ COPT
      INTEGER COPT
*     ------------------------------
      print *, '***************** QFITWW Configuration ***************'
      IF( IS4JET ) THEN
        print *, 'You have chosen to fit WW -> 4jet events using the'
        print *, 'following parameters:'
        IF( JBIT(COPT, 1).EQ.1 )  print *, '     E_j1'
        IF( JBIT(COPT, 2).EQ.1 )  print *, '     E_j2'
        IF( JBIT(COPT, 3).EQ.1 )  print *, '     E_j3'
        IF( JBIT(COPT, 4).EQ.1 )  print *, '     E_j4'
        IF( JBIT(COPT, 5).EQ.1 )  print *, '     ISRpz'
        IF( JBIT(COPT, 6).EQ.1 )  print *, '     theta_j1'
        IF( JBIT(COPT, 7).EQ.1 )  print *, '     theta_j2'
        IF( JBIT(COPT, 8).EQ.1 )  print *, '     theta_j3'
        IF( JBIT(COPT, 9).EQ.1 )  print *, '     theta_j4'
        IF( JBIT(COPT, 10).EQ.1 ) print *, '     phi_j1'
        IF( JBIT(COPT, 11).EQ.1 ) print *, '     phi_j2'
        IF( JBIT(COPT, 12).EQ.1 ) print *, '     phi_j3'
        IF( JBIT(COPT, 13).EQ.1 ) print *, '     phi_j4'
        IF( JBIT(COPT, 14).EQ.1 ) THEN
          print *, 'You have also requested to define the covariance'
          print *, 'matrix for the measured parameters via QFWERR'
        ENDIF
        IF( JBIT(COPT, 15).EQ.1 ) THEN
          print *, 'Further, you have chosen to provide the constraint'
          print *, 'functions via QFWFUN'
        ENDIF
        IF( JBIT(COPT, 15).EQ.0 ) THEN
          IF( DM.GE.100.0 ) THEN
            print *, 'You have chosen to perform a 4-C fit'
          ELSE
            print *, 'You have chosen to perform a 5-C fit with an'
            print *, 'equal mass constraint of: ', DM
          ENDIF
        ENDIF
      ELSE
        print *, 'You have chosen to fit WW -> 2jlv events using the'
        print *, 'following parameters:'
        IF( JBIT(COPT, 1).EQ.1 )  print *, '     E_j1'
        IF( JBIT(COPT, 2).EQ.1 )  print *, '     E_j2'
        IF( JBIT(COPT, 3).EQ.1 )  print *, '     E_lepton'
        IF( JBIT(COPT, 4).EQ.1 )  print *, '     ISRpz'
        IF( JBIT(COPT, 5).EQ.1 )  print *, '     theta_j1'
        IF( JBIT(COPT, 6).EQ.1 )  print *, '     theta_j2'
        IF( JBIT(COPT, 7).EQ.1 )  print *, '     theta_lepton'
        IF( JBIT(COPT, 8).EQ.1 )  print *, '     phi_j1'
        IF( JBIT(COPT, 9).EQ.1 )  print *, '     phi_j2'
        IF( JBIT(COPT, 10).EQ.1 ) print *, '     phi_lepton'
        IF( JBIT(COPT, 11).EQ.1 ) print *, '     Undefined'
        IF( JBIT(COPT, 12).EQ.1 ) print *, '     Undefined'
        IF( JBIT(COPT, 13).EQ.1 ) print *, '     Undefined'
        IF( JBIT(COPT, 14).EQ.1 ) THEN
          print *, 'You have also requested to define the covariance'
          print *, 'matrix for the measured parameters via QFWERR'
        ENDIF
        IF( JBIT(COPT, 15).EQ.1 ) THEN
          print *, 'Further, you have chosen to provide the constraint'
          print *, 'functions via QFWFUN'
        ENDIF
        IF( JBIT(COPT, 15).EQ.0 ) THEN
          IF( DM.GE.100.0 ) THEN
            print *, 'You have chosen to perform a 1-C fit'
          ELSE
            print *, 'You have chosen to perform a 2-C fit with an'
            print *, 'equal mass constraint of: ', DM
          ENDIF
        ENDIF
      ENDIF
      print *, '******************************************************'
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      DOUBLE PRECISION FUNCTION RDCSTR( CONSTR, LIMIT )
*     ------------------------------------------------------------------
      DOUBLE PRECISION CONSTR
      REAL   LIMIT
*     ------------------------------
      IF(CONSTR.gt.DBLE( LIMIT )) THEN
         RDCSTR  = CONSTR  / 2.0
      ELSE
         RDCSTR  = DBLE( LIMIT )
      ENDIF
      IF( CONSTR.lt.DBLE( LIMIT ) ) CONSTR = DBLE( LIMIT )
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE RELPAR( COPT )
*     ------------------------------------------------------------------
      INTEGER MAXPRM
      PARAMETER( MAXPRM = 13 )
      INTEGER COPT, JBIT, ERROR, i
      DOUBLE PRECISION  ARG(10)
      EXTERNAL CHISQR, KIN
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      DO i = 1, MAXPRM
         IF( JBIT( COPT, i ) .eq. 1 ) THEN
            ARG(1) = FLOAT(i)
            CALL MNEXCM(CHISQR,'RELEASE',ARG,1,ERROR,KIN)
         ENDIF
      ENDDO
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE FJTPAR( ITKS, PARAM )
*     ------------------------------------------------------------------
      INTEGER ITKS(4), JET1, JET2, JET3, JET4, MAXINT
      INTEGER MAXPRM
      PARAMETER( MAXPRM = 13 )
      DOUBLE PRECISION PARAM(*), ISRPZ
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      JET1    = ITKS(1)
      JET2    = ITKS(2)
      JET3    = ITKS(3)
      JET4    = ITKS(4)
      ISRPZ = 0.0

      PARAM(1)  = QE( JET1 )
      PARAM(2)  = QE( JET2 )
      PARAM(3)  = QE( JET3 )
      PARAM(4)  = QE( JET4 )
      PARAM(5)  = ISRPZ
      PARAM(6)  = QQPIH - ACOS( QCT(JET1) )
      PARAM(7)  = QQPIH - ACOS( QCT(JET2) )
      PARAM(8)  = QQPIH - ACOS( QCT(JET3) )
      PARAM(9)  = QQPIH - ACOS( QCT(JET4) )
      PARAM(10) = QPH( JET1 )
      PARAM(11) = QPH( JET2 )
      PARAM(12) = QPH( JET3 )
      PARAM(13) = QPH( JET4 )
*     ------------------------------------------------------------------
      RETURN
      END


************************************************************************

      SUBROUTINE TJTPAR( ITKS, PARAM )
*     ------------------------------------------------------------------
      INTEGER ITKS(4), JET1, JET2, LEPTON, MaxInt
      INTEGER MAXPRM
      PARAMETER( MAXPRM = 13 )
      DOUBLE PRECISION PARAM(*), ISRPZ
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      JET1   = ITKS(1)
      JET2   = ITKS(2)
      LEPTON = ITKS(3)
      ISRPZ  = 0.0

      PARAM(1)  = QE( JET1 )
      PARAM(2)  = QE( JET2 )
      PARAM(3)  = QE( LEPTON )
      PARAM(4)  = ISRPZ
      PARAM(5)  = QQPIH - ACOS( QCT( JET1 ) )
      PARAM(6)  = QQPIH - ACOS( QCT( JET2 ) )
      PARAM(7)  = QQPIH - ACOS( QCT(LEPTON) )
      PARAM(8)  = QPH( JET1 )
      PARAM(9)  = QPH( JET2 )
      PARAM(10) = QPH(LEPTON)
      PARAM(11) = 0.0  ! dummy
      PARAM(12) = 0.0  ! dummy
      PARAM(13) = 0.0  ! dummy
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE SETSTP( ITKS, STPSIZ )
*     ------------------------------------------------------------------
      INTEGER MAXPRM
      PARAMETER( MAXPRM = 13 )
      DOUBLE PRECISION STPSIZ(MAXPRM), COV(MAXPRM,MAXPRM)
      INTEGER ITKS(4)
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------

      IF( JBIT(COPT,14).EQ.1 ) THEN
        CALL QFWERR( ITKS, COV )
      ELSE
        CALL QFWDER( ITKS, COV )
      ENDIF

      DO I = 1, MAXPRM
            STPSIZ(I) = SQRT( COV(I,I) )
      ENDDO
*     ------------------------------------------------------------------
      RETURN
      END


************************************************************************

      SUBROUTINE STOPRM( ISINIT, PARAM, STPSIZ, MAXPRM)
*     ------------------------------------------------------------------
      DOUBLE PRECISION  ZERO
      LOGICAL ISINIT
      PARAMETER( ZERO = 0.0 )
      CHARACTER*6  PNAME
      DOUBLE PRECISION  PARAM(*), STPSIZ(*)
      DOUBLE PRECISION  ARG(10)
      INTEGER MAXPRM, ERROR, I
      EXTERNAL CHISQR, KIN
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      DO I = 1, MAXPRM
         CALL MNPARM( I, PNAME, PARAM(i), STPSIZ(i), ZERO, ZERO, ERROR )
         IF(ERROR.ne.0) PRINT *,' problem defining PARAMETER ', I
      ENDDO
      IF( ISINIT ) THEN  ! store measured PARAMETERs in CHISQR
         ARG(1) = 1.0    ! and check if COVM is singular
         CALL MNEXCM( CHISQR, 'CALL FCN', ARG, 1, ERROR, KIN )
      ENDIF
      DO I = 1, MAXPRM        ! fix all PARAMETERs
         ARG(1)=I
         CALL MNEXCM( CHISQR, 'FIX', ARG, 1, ERROR, KIN )
      ENDDO
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE CHISQR( NUMPAR, GRAD, CHISQU, PARAM, ARG, CALC )
*     ------------------------------------------------------------------
      PARAMETER( MAXPRM=13 )
      COMMON /FP/ FIT(MAXPRM)
      DOUBLE PRECISION   FIT
      COMMON /CS/  CCHISQ
      DOUBLE PRECISION   CCHISQ
      COMMON /VP/ COPT
      INTEGER  COPT
      COMMON /TK/ CITKS
      INTEGER  CITKS(4)
      COMMON /J4/ IS4JET
      LOGICAL IS4JET
      COMMON /MT/ DM
      DOUBLE PRECISION   DM
      COMMON /M4/ D4M
      DOUBLE PRECISION   D4M
      COMMON /ST/ STRNGT
      DOUBLE PRECISION  STRNGT
      COMMON /SN/ IFAIL
      INTEGER IFAIL
      COMMON /VC/ P1(4), P2(4), P3(4), P4(4), W1(4), W2(4), ISRPHT(4)
      DOUBLE PRECISION P1, P2, P3, P4, W1, W2, ISRPHT
      EXTERNAL CALC
      INTEGER NUMPAR, ARG
      DOUBLE PRECISION PARAM(MAXPRM), GRAD(MAXPRM), MEASUR(MAXPRM)
      DOUBLE PRECISION COV(MAXPRM,MAXPRM), INVCOV(MAXPRM,MAXPRM)
      DOUBLE PRECISION CHISQU, TERM
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      IF( ARG .EQ. 1.0 ) THEN  ! init, store measured params and sigmas
         DO i = 1, MAXPRM
            MEASUR(i) = PARAM(i)
         ENDDO
         IF( JBIT(COPT,14).EQ.1 ) THEN
           CALL QFWERR( CITKS, COV )
           CALL INVRSE( COV, INVCOV, IFAIL ) ! if COV is singular
         ELSE                                ! then IFAIL is set to -1
           CALL QFWDER( CITKS, COV )         ! and the fit is stopped
           CALL INVRSE( COV, INVCOV, IFAIL ) ! by QFITWW
         ENDIF
      ELSE
        CHISQU = 0.0
        CALL CALC( CITKS,PARAM,P1,P2,P3,P4,W1,W2,ISRPHT )

        DO I = 1, MAXPRM
          DO J = 1, MAXPRM
            IF( JBIT(COPT,I).EQ.1.AND.JBIT(COPT,J).EQ.1 ) THEN
              TERM = ( PARAM(I)-MEASUR(I) ) * INVCOV(I,J) *
     *               ( PARAM(J)-MEASUR(J) )
              CHISQU = CHISQU + TERM
            ENDIF
          ENDDO
        ENDDO

        IF( JBIT(COPT,15).EQ.1 ) THEN
          CALL QFWFUN( P1,P2,P3,P4,ISRPHT,TERM )
          CHISQU = CHISQU + STRNGT * TERM
        ELSE
          CALL QFWDFN( P1,P2,P3,P4,ISRPHT,DM,D4M,TERM  )
          CHISQU = CHISQU + TERM
        ENDIF

        DO i = 1, MAXPRM
            FIT(i) = PARAM(i)
        ENDDO
        CCHISQ = CHISQU
      ENDIF
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE QFWDER( ITKS, COV )
*     ------------------------------------------------------------------
      PARAMETER( MAXPRM = 13 )
      DOUBLE PRECISION COV(13,13)
      INTEGER  ITKS(4)
      COMMON /J4/ IS4JET
      LOGICAL IS4JET
      DOUBLE PRECISION LPTENG
      LOGICAL ISELEC, ISMUON
      INTEGER LEPTON
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      DO I = 1, MAXPRM
        DO J = 1, MAXPRM
          IF(I.NE.J) COV(I,J) = 0.0
          IF(I.EQ.J) COV(I,J) = 1.0
        ENDDO
      ENDDO

      IF( IS4JET ) THEN
        JET1 = ITKS(1)
        JET2 = ITKS(2)
        JET3 = ITKS(3)
        JET4 = ITKS(4)

        COV(1,1) = SQRT(QE(JET1))**2
        COV(2,2) = SQRT(QE(JET2))**2
        COV(3,3) = SQRT(QE(JET3))**2
        COV(4,4) = SQRT(QE(JET4))**2
        COV(5,5) = (1.5)**2                     ! Average initial state
        IF(QELEP.gt.180.) COV(5,5) = (2.0)**2   ! radiated photon energy
        COV(6,6) = (10.0 / ( QQRADP*SQRT(QP(JET1)) ))**2 !jet axis ERRORs,
        COV(7,7) = (10.0 / ( QQRADP*SQRT(QP(JET2)) ))**2 !10 degrees/root(P)
        COV(8,8) = (10.0 / ( QQRADP*SQRT(QP(JET3)) ))**2
        COV(9,9) = (10.0 / ( QQRADP*SQRT(QP(JET4)) ))**2
        COV(10,10) = (10.0 / ( QQRADP*SQRT(QP(JET1)) ))**2
        COV(11,11) = (10.0 / ( QQRADP*SQRT(QP(JET2)) ))**2
        COV(12,12) = (10.0 / ( QQRADP*SQRT(QP(JET3)) ))**2
        COV(13,13) = (10.0 / ( QQRADP*SQRT(QP(JET4)) ))**2
      ELSE
        JET1 = ITKS(1)
        JET2 = ITKS(2)
        LEPTON = ITKS(3)
        ISELEC = KEFOTY( LEPTON ).EQ.1
        ISMUON = KEFOTY( LEPTON ).EQ.2
        LPTENG = QE(LEPTON)

        COV(1,1) = SQRT( QE(JET1) )**2
        COV(2,2) = SQRT( QE(JET2) )**2
        IF( ISELEC ) THEN                             !  18%/root(E) for e-
           COV(3,3) = (0.18 * SQRT(LPTENG))**2
        ELSEIF( ISMUON ) THEN                             !  0.1%*Pt for mu-
           COV(3,3) = (0.001*SQRT(QX(LEPTON)**2+QY(LEPTON)**2))**2
        ENDIF      !  superceded later by ITKS FRFT ERROR matrix IF available
        COV(4,4) = (1.5)**2     !  Average initial state radiated photon energy
        IF(QELEP.gt.180.) COV(4,4) = (2.0)**2
        COV(5,5) = (10.0 / ( QQRADP*SQRT(QP(JET1)) ))**2 ! 10 degrees/root(P)
        COV(6,6) = (10.0 / ( QQRADP*SQRT(QP(JET2)) ))**2 ! jet axis ERRORs,
        COV(7,7) = (0.01)**2
        COV(8,8) = (10.0 / ( QQRADP*SQRT(QP(JET1)) ))**2
        COV(9,9) = (10.0 / ( QQRADP*SQRT(QP(JET2)) ))**2
        COV(10,10) = (0.0005)**2
        IF( XFRF(LEPTON) ) THEN
          COV(7,7) = SQRT( QFRFEM( LEPTON, 2, 2) )**2
          COV(10,10) = SQRT( QFRFEM( LEPTON, 3, 3) )**2
          IF( ISMUON ) THEN
            COV(3,3) = QFRFEM(LEPTON,1,1)
            COV(3,3) = (SQRT(COV(3,3))/ABS(QFRFIR(LEPTON))*LPTENG)**2
          ENDIF
        ENDIF
        COV(11,11)= 1.0  ! dummy
        COV(12,12)= 1.0  ! dummy
        COV(13,13)= 1.0  ! dummy
      ENDIF

*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE QFWDFN( P1,P2,P3,P4,ISRPHT,DM,D4M,CHISQ )
*     ------------------------------------------------------------------
      PARAMETER( MAXPRM = 13 )
      DOUBLE PRECISION CHISQ, SIGP, SIGM, TERM, WMASS1, WMASS2, RSTMAS
      DOUBLE PRECISION DM, D4M, EBAL, ELEP
      INTEGER X, Y, Z
      DOUBLE PRECISION P1(4),P2(4),P3(4),P4(4),W1(4),W2(4),ISRPHT(4)
      COMMON /J4/ IS4JET
      LOGICAL IS4JET
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------

      CHISQ = 0.0
      SIGP = D4M
      SIGM = DM

      X = 1
      Y = 2
      Z = 3
      DO I = 1, 4
        W1(I) = P1(I)+P2(I)
        W2(I) = P3(I)+P4(I)
      ENDDO

      ELEP = QELEP
      IF( QELEP.EQ.170.0 ) ELEP = 170.283
      IF( QELEP.EQ.172.0 ) ELEP = 172.297

      EBAL  = ELEP - ISRPHT(4) - W1(4) - W2(4)
      TERM  = EBAL / SIGP
      CHISQ = CHISQ + TERM**2

      IF( IS4JET ) THEN
        TERM  = 0 - P1(X) - P2(X) - P3(X) - P4(X)
        TERM  = TERM / SIGP
        CHISQ = CHISQ + TERM**2

        TERM  = 0 - P1(Y) - P2(Y) - P3(Y) - P4(Y)
        TERM  = TERM / SIGP
        CHISQ = CHISQ + TERM**2

        TERM  = 0 - P1(Z) - P2(Z) - P3(Z) - P4(Z) - ISRPHT(Z)
        TERM  = TERM / SIGP
        CHISQ = CHISQ + TERM**2
      ENDIF

      WMASS1 = RSTMAS( W1 )
      WMASS2 = RSTMAS( W2 )
      IF( SIGM .lt. 100.0 ) THEN              ! constrain W masses
        TERM = ( WMASS1 - WMASS2) / SIGM       ! equal within
        CHISQ = CHISQ + TERM**2                ! certain tolarence
      ENDIF
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

*     CALCulate inverse of covariance matrix

      SUBROUTINE INVRSE( COV, INVCOV, IFAIL )
      DOUBLE PRECISION COV(13,13), INVCOV(13,13), DET
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------------------------------------------

      DO I = 1, 13
        DO J = 1, 13
          INVCOV(I,J) = COV(I,J)
        ENDDO
      ENDDO

      CALL DFACT( 13, INVCOV, 13, IR, IFAIL, DET, JFAIL )
      IF( IFAIL.NE.-1 ) THEN
        CALL DFINV( 13, INVCOV, 13, IR )
      ELSE
        PRINT *, 'ERROR:  QFWERR user defined covariance matrix'
        PRINT *, '        for measured parameters in QFITWW'
        PRINT *, '        is singular--fit stopped'
        PRINT *, 'KNEVT',KNEVT,';  KRUN',KRUN,';  KEVT',KEVT
      ENDIF

*     ------------------------------------------------------------------
      RETURN
      END

***********************************************************************
************************************************************************


*     CALCulate 4 vectors, masses, E balance from fit PARAMETERs

      SUBROUTINE FOURJT( JET, PARAM, JET1, JET2, JET3, JET4, W1, W2,
     +                                                          ISRPHT )
*     ------------------------------------------------------------------
      PARAMETER( MAXPRM = 13 )
      DOUBLE PRECISION PARAM(MAXPRM)
      DOUBLE PRECISION JET1(4),JET2(4),JET3(4),JET4(4), ISRPHT(4)
      DOUBLE PRECISION W1(4),W2(4)
      INTEGER JET(*)
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      JET1(1) = PARAM(1) *QBETA(JET(1))* COS(PARAM(6)) * COS(PARAM(10))
      JET1(2) = PARAM(1) *QBETA(JET(1))* COS(PARAM(6)) * SIN(PARAM(10))
      JET1(3) = PARAM(1) *QBETA(JET(1))* SIN(PARAM(6))
      JET1(4) = PARAM(1)
      JET2(1) = PARAM(2) *QBETA(JET(2))* COS(PARAM(7)) * COS(PARAM(11))
      JET2(2) = PARAM(2) *QBETA(JET(2))* COS(PARAM(7)) * SIN(PARAM(11))
      JET2(3) = PARAM(2) *QBETA(JET(2))* SIN(PARAM(7))
      JET2(4) = PARAM(2)
      JET3(1) = PARAM(3) *QBETA(JET(3))* COS(PARAM(8)) * COS(PARAM(12))
      JET3(2) = PARAM(3) *QBETA(JET(3))* COS(PARAM(8)) * SIN(PARAM(12))
      JET3(3) = PARAM(3) *QBETA(JET(3))* SIN(PARAM(8))
      JET3(4) = PARAM(3)
      JET4(1) = PARAM(4) *QBETA(JET(4))* COS(PARAM(9)) * COS(PARAM(13))
      JET4(2) = PARAM(4) *QBETA(JET(4))* COS(PARAM(9)) * SIN(PARAM(13))
      JET4(3) = PARAM(4) *QBETA(JET(4))* SIN(PARAM(9))
      JET4(4) = PARAM(4)
      DO I = 1,4
           W1(I) = JET1(I) + JET2(I)
      ENDDO
      DO I = 1,4
           W2(I) = JET3(I) + JET4(I)
      ENDDO
      ISRPHT(1) = 0.0
      ISRPHT(2) = 0.0
      ISRPHT(3) = PARAM(5)
      ISRPHT(4) = ABS(PARAM(5))
*     ------------------------------------------------------------------
      RETURN
      END

***********************************************************************

*     CALCulate 4 vectors, masses, E balance from fit PARAMETERs

      SUBROUTINE TWOJTL( JET, PARAM, JET1, JET2, LEPTON, NUTRNO,
     +                    WHADRN, WLEPTN, ISRPHT )
*     ------------------------------------------------------------------
      DOUBLE PRECISION PARAM(*)
      DOUBLE PRECISION JET1(4),JET2(4),LEPTON(4),NUTRNO(4), ISRPHT(4)
      DOUBLE PRECISION WHADRN(4),WLEPTN(4)
      INTEGER JET(*)
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      JET1(1) = PARAM(1) *QBETA(JET(1))* COS(PARAM(5)) * COS(PARAM(8))
      JET1(2) = PARAM(1) *QBETA(JET(1))* COS(PARAM(5)) * SIN(PARAM(8))
      JET1(3) = PARAM(1) *QBETA(JET(1))* SIN(PARAM(5))
      JET1(4) = PARAM(1)
      JET2(1) = PARAM(2) *QBETA(JET(2))* COS(PARAM(6)) * COS(PARAM(9))
      JET2(2) = PARAM(2) *QBETA(JET(2))* COS(PARAM(6)) * SIN(PARAM(9))
      JET2(3) = PARAM(2) *QBETA(JET(2))* SIN(PARAM(6))
      JET2(4) = PARAM(2)
      LEPTON(1) = PARAM(3)             * COS(PARAM(7)) * COS(PARAM(10))
      LEPTON(2) = PARAM(3)             * COS(PARAM(7)) * SIN(PARAM(10))
      LEPTON(3) = PARAM(3)             * SIN(PARAM(7))
      LEPTON(4) = PARAM(3)
      DO I = 1,4
           WHADRN(I) = JET1(I) + JET2(I)
      ENDDO
      DO I = 1,3
           NUTRNO(I) = - ( WHADRN(I) + LEPTON(I) )
      ENDDO
      NUTRNO(3) = NUTRNO(3) - PARAM(4)
      NUTRNO(4) = SQRT( NUTRNO(1)**2+NUTRNO(2)**2+NUTRNO(3)**2 )
      DO I = 1,4
           WLEPTN(I) = LEPTON(I) + NUTRNO(I)
      ENDDO
      ISRPHT(1) = 0.0
      ISRPHT(2) = 0.0
      ISRPHT(3) = PARAM(4)
      ISRPHT(4) = ABS( PARAM(4) )
*     ------------------------------------------------------------------
      RETURN
      END

***********************************************************************

*     CALCulate 4 vectors, masses, E balance from fit PARAMETERs

      SUBROUTINE KIN( PARAM,JET1,JET2,JET3,JET4,W1,W2,ISRPHT )
*     ------------------------------------------------------------------
      DOUBLE PRECISION PARAM(*)
      DOUBLE PRECISION JET1(4),JET2(4),JET3(4),JET4(4), ISRPHT(4)
      DOUBLE PRECISION W1(4),W2(4)
*     ------------------------------
*     ------------------------------------------------------------------
      RETURN
      END

***********************************************************************

      DOUBLE PRECISION FUNCTION RSTMAS(VECTOR)

*     ------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VECTOR(4)
*     ------------------------------
      RSTMAS = VECTOR(4)**2 - (VECTOR(1)**2+VECTOR(2)**2+VECTOR(3)**2)
      RSTMAS = SIGN( SQRT(ABS(RSTMAS)), RSTMAS)
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      SUBROUTINE VCTOTR( ITKS, FIT, IFTKS, ISFOURJT, ISPHYS )

*     ------------------------------------------------------------------
      INTEGER NULL
      PARAMETER( NULL = 0 )
      DOUBLE PRECISION FIT(*)
      INTEGER IFTKS(7)
      DOUBLE PRECISION P1(4), P2(4), P3(4), P4(4), P5(4), P6(4), P7(4)
      DOUBLE PRECISION RSTMAS
      LOGICAL ISFOURJT, ISNENG, ISPHYS
      REAL VECTOR(4)
      INTEGER FLAG, ITKS(4)
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      DO i = 1, 7
            IF( IFTKS(i) .eq. NULL ) THEN
                  IFTKS(i) = KVNEW( dummy )
            ELSE
                  CALL VZERO( VECTOR, 4 )
                  CALL QVSET4( IFTKS(i), VECTOR )
            ENDIF
      ENDDO
      IF( ISFOURJT ) THEN
            CALL FOURJT( ITKS, FIT, P1, P2, P3, P4, P5, P6, P7)
      ELSE
            CALL TWOJTL( ITKS, FIT, P1, P2, P3, P4, P5, P6, P7)
      ENDIF
      ISNENG = P1(4).lt.0.0.or.P2(4).lt.0.0.or.P3(4).lt.0.0.or.
     +         P4(4).lt.0.0.or.P5(4).lt.0.0.or.P6(4).lt.0.0.or.
     +         P7(4).lt.0.0
      ISPHYS = .not.ISNENG
      IF( ISPHYS ) THEN
            DO i = 1, 4
                  VECTOR(i) = SNGL( P1(i) )
            ENDDO
            CALL QVSET4( IFTKS(1), VECTOR )
            DO i = 1, 4
                  VECTOR(i) = SNGL( P2(i) )
            ENDDO
            CALL QVSET4( IFTKS(2), VECTOR )
            DO i = 1, 4
                  VECTOR(i) = SNGL( P3(i) )
            ENDDO
            CALL QVSET4( IFTKS(3), VECTOR )
            DO i = 1, 4
                  VECTOR(i) = SNGL( P4(i) )
            ENDDO
            CALL QVSET4( IFTKS(4), VECTOR )
            DO i = 1, 4
                  VECTOR(i) = SNGL( P5(i) )
            ENDDO
            CALL QVSET4( IFTKS(5), VECTOR )
            DO i = 1, 4
                  VECTOR(i) = SNGL( P6(i) )
            ENDDO
            CALL QVSET4( IFTKS(6), VECTOR )
            DO i = 1, 4
                  VECTOR(i) = SNGL( P7(i) )
            ENDDO
            CALL QVSET4( IFTKS(7), VECTOR )
      ENDIF
*     ------------------------------------------------------------------
      RETURN
      END

***********************************************************************

      SUBROUTINE TKERMT( IS4JET, ITKS, IFTKS, PARAM, ERRMAT )

*     ------------------------------------------------------------------
      LOGICAL IS4JET
      INTEGER ITKS(4), IFTKS(7)
      DOUBLE PRECISION  PARAM(13)
      REAL    P(13), ERRMAT(13,13), VOLD(17,17), VNEW(17,17), M(17,17)
      REAL    BETA(4), V1(4,4), V2(4,4), V3(4,4), V4(4,4)
      REAL    X(4), V1OLD(4,4), V2OLD(4,4), V3OLD(4,4), V4OLD(4,4)
      REAL    V1NEW(4,4), V2NEW(4,4), V3NEW(4,4), V4NEW(4,4)
      REAL    M1(4,4), M2(4,4), M3(4,4), M4(4,4)
      REAL    DPXDE,     DPYDE,     DPZDE,     DEDE
      REAL    DPXDTH, DPYDTH, DPZDTH, DEDTH
      REAL    DPXDPH,   DPYDPH,   DPZDPH,   DEDPH
      REAL    DPXDBT,  DPYDBT,  DPZDBT,  DEDBT
#include "qcde.h"
#include "qmacro.h"
*     ------------------------------
      DO I = 1, 13
        P(I) = SNGL( PARAM(I) )
      ENDDO
      DO I = 1, 4
        DO J = 1, 4
          V1(I,J) = 0.0  ! initialize matricies
          V2(I,J) = 0.0
          V3(I,J) = 0.0
          V4(I,J) = 0.0
        ENDDO
      ENDDO
      DO I = 1, 17
        DO J = 1, 17
          VOLD(I,J) = 0.0  ! initialize matricies
          VNEW(I,J) = 0.0
          M(I,J) = 0.0
        ENDDO
      ENDDO
      DO I = 1, 13
        DO J = 1, 13
          VOLD(I,J) = ERRMAT(I,J)  ! increase size of old error matrix
        ENDDO
      ENDDO
      IF( IS4JET ) THEN
        DO I = 1, 4
          BETA(I) = SNGL( QBETA( ITKS(I) ) )
        ENDDO
        DO I = 1, 4   ! fill transformation matrix
          M(I,I)       =  DPXDE( P(I), BETA(I), P(I+5), P(I+9) )
          M(I,I+4)     =  DPYDE( P(I), BETA(I), P(I+5), P(I+9) )
          M(I,I+8)     =  DPZDE( P(I), BETA(I), P(I+5), P(I+9) )
          M(I,I+12)    =   DEDE( P(I), BETA(I), P(I+5), P(I+9) )
          M(5,17)      = 1.0
          M(I+5,I)     = DPXDTH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+5,I+4)   = DPYDTH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+5,I+8)   = DPZDTH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+5,I+12)  =  DEDTH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+9,I)     = DPXDPH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+9,I+4)   = DPYDPH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+9,I+8)   = DPZDPH( P(I), BETA(I), P(I+5), P(I+9) )
          M(I+9,I+12)  =  DEDPH( P(I), BETA(I), P(I+5), P(I+9) )
        ENDDO
      ELSE
        DO I = 1, 2
          BETA(I) = SNGL( QBETA( ITKS(I) ) )
        ENDDO
        BETA(3) = 1.0  !  lepton is assumed massless in the high e limit
        BETA(4) = 1.0  !  neutrino is assumed massless
        DO I = 1, 3   ! fill transformation matrix
          M(I,I)      =  DPXDE( P(I), BETA(I), P(I+4), P(I+7) )
          M(I,I+4)    =  DPYDE( P(I), BETA(I), P(I+4), P(I+7) )
          M(I,I+8)    =  DPZDE( P(I), BETA(I), P(I+4), P(I+7) )
          M(I,I+12)   =   DEDE( P(I), BETA(I), P(I+4), P(I+7) )
          M(4,17)     = 1.0
          M(I+4,I)    = DPXDTH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+4,I+4)  = DPYDTH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+4,I+8)  = DPZDTH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+4,I+12) =  DEDTH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+7,I)    = DPXDPH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+7,I+4)  = DPYDPH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+7,I+8)  = DPZDPH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I+7,I+12) =  DEDPH( P(I), BETA(I), P(I+4), P(I+7) )
          M(I,4)      = M(I,I)
          M(I,8)      = M(I,I+4)
          M(I,12)     = M(I,I+8)
          M(I,16)     = M(I,I+12)
          M(I+4,4)    = M(I+4,I)
          M(I+4,8)    = M(I+4,I+4)
          M(I+4,12)   = M(I+4,I+8)
          M(I+4,16)   = M(I+4,I+12)
          M(I+7,4)    = M(I+7,I)
          M(I+7,8)    = M(I+7,I+4)
          M(I+7,12)   = M(I+7,I+8)
          M(I+7,16)   = M(I+7,I+12)
        ENDDO
      ENDIF
      DO J = 1, 17  !  Perform transformation to Vnew
        DO K = 1, 17
          DO I = 1, 17
            DO L = 1, 17
              VNEW(J,K) = VNEW(J,K) + M(I,J) * VOLD(I,L) * M(L,K)
            ENDDO                     ! note: transpose
          ENDDO
        ENDDO
      ENDDO
      DO I = 0, 3  ! fill individual ERROR matrices from Vnew
        DO J = 0, 3
          V1(I+1,J+1) = VNEW( 4*I+1, 4*J+1 )
          V2(I+1,J+1) = VNEW( 4*I+2, 4*J+2 )
          V3(I+1,J+1) = VNEW( 4*I+3, 4*J+3 )
          V4(I+1,J+1) = VNEW( 4*I+4, 4*J+4 )
        ENDDO
      ENDDO
      CALL QVSETS( IFTKS(1), V1 )
      CALL QVSETS( IFTKS(2), V2 )
      CALL QVSETS( IFTKS(3), V3 )
      CALL QVSETS( IFTKS(4), V4 )
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

***********************************************************************

      REAL FUNCTION DPXDE( E, BETA, THETA, PHI  )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPXDE = BETA * COS( THETA ) * COS( PHI )
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPXDTH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPXDTH = - ( E * BETA * SIN(THETA) * COS(PHI) )
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPXDPH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPXDPH = - ( E * BETA * COS(THETA) * SIN(PHI) )
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPYDE( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPYDE = BETA * COS(THETA) * SIN(PHI)
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPYDTH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPYDTH = - (E * BETA * SIN(THETA) * SIN(PHI) )
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPYDPH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPYDPH = E * BETA * COS(THETA) * COS(PHI)
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPZDE( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPZDE = BETA * SIN(THETA)
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPZDTH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPZDTH = E * COS(THETA)
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DPZDPH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DPZDPH = 0.0
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DEDE( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DEDE = 1.0
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DEDTH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DEDTH = 0.0
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************

      REAL FUNCTION DEDPH( E, BETA, THETA, PHI )

*     ------------------------------------------------------------------
      REAL E, BETA, THETA, PHI
*     ------------------------------
      DEDPH = 0.0
*     ------------------------------------------------------------------
      RETURN
      END

************************************************************************
