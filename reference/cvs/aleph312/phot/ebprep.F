      SUBROUTINE EBPREP( MXST , ICLN , NUST , INDX , ESTO , IER )
C ----------------------------------------------------
C   AUTHOR   : J. Badier     25/09/89
C! Prepare the analysis of a cluster.
CKEY PHOTONS ARRAY STOREYS / INTERNAL
C
C   The storeys of the cluster ICLN are stored in the INDX and ESTO
C   arrays. The number of storeys has to be greater than MXST.
C   The storeys are searched in the ESDA bank , then if ESDA is missing
C   in the PEST and ETDI banks.
C
C   Input     : MXST    Maximum number of storeys.
C               ICLN    Cluster number.
C
C   Output    : NUST    Number of storeys of the cluster ICLN.
C               INDX(1,IST) Theta index of the storey IST.
C               INDX(2,IST) Phi index of the storey IST.
C               INDX(3,IST) Stack number of the storey IST.
C               ESTO(IST)   Raw content of the storey IST.
C                           IST = 1 , NUST
C               IER     = 0 No error.
C                       = 1 Storeys bank missing.
C                       = 2 More than MXST storeys.
C                       = 3 No storey in the cluster.
C                       = 4 Energy < EMIN
C                       = 5 Energy > EMAX
C
C   BANKS :
C     INPUT   : ESDA or PEST,ETDI.
C     OUTPUT  : NONE
C
C ----------------------------------------------------
#ifndef DOC
      SAVE
      PARAMETER ( PTIT = .001 )
      PARAMETER( EMIN = .050 , EMAX = 100. )
      DIMENSION INDX(3,*) , ESTO(*)
#include "bcs.h"
#include "esdajj.h"
#include "pestjj.h"
#include "etdijj.h"
#include "bmacro.h"
C
      IER = 0
      NUST = 0
      ERAW = 0.
C
      KESDA = IW( NAMIND('ESDA') )
      IF( KESDA .NE. 0 ) THEN
C   JULIA banks
C   Number of storey in ESDA bank.
        NSTO = LROWS( KESDA )
C   Loop over the storeys
        DO 1 ISTO = 1 , NSTO
          IF( ICLN .NE. ITABL(KESDA,ISTO,JESDEC) ) GO TO 1
          IF( NUST .GE. MXST ) GO TO 102
          IT = ITABL(KESDA,ISTO,JESDTJ)
          IF( IT .EQ. 0 ) GO TO 1
          ENRJ = RTABL(KESDA,ISTO,JESDME)
          IF( ENRJ .LE. PTIT ) GO TO 1
          NUST = NUST + 1
          INDX(1,NUST) = IT
          INDX(2,NUST) = ITABL(KESDA,ISTO,JESDFI)
          INDX(3,NUST) = ITABL(KESDA,ISTO,JESDDK)
          ESTO(NUST) = ENRJ
          ERAW = ERAW + ENRJ
    1   CONTINUE
C
      ELSE
C   POT banks
        KPEST = IW( NAMIND('PEST') )
        IF( KPEST .EQ. 0 ) GO TO 101
        KETDI = IW( NAMIND('ETDI') )
        IF( KETDI .EQ. 0 ) GO TO 101
C   Number of storey in PEST bank.
        NSTO = LROWS( KPEST )
C   Loop over the storeys
        DO 2 ISTO = 1 , NSTO
          IF( ICLN .NE. ITABL(KPEST,ISTO,JPESPE) ) GO TO 2
          IF( NUST .GE. MXST ) GO TO 102
          ITDI = ITABL(KPEST,ISTO,JPESET)
          IF( ITDI .EQ. 0 ) GO TO 2
          NUST = NUST + 1
          IWIN = ITABL(KETDI,ITDI,JETDTL)
C   Depack IWIN
          INDX(1,NUST) = IBITS ( IWIN  , 16 , 8 )
          INDX(2,NUST) = IBITS ( IWIN  , 2 , 9 )
          INDX(3,NUST) = ITABL(KPEST,ISTO,JPESKS)
          ESTO(NUST) =  RTABL(KPEST,ISTO,JPESER)
          ERAW = ERAW + ESTO(NUST)
    2 CONTINUE
      ENDIF
      IF( ERAW .LT. EMIN ) GO TO 104
      IF( ERAW .GT. EMAX ) GO TO 105
      IF( NUST .LE. 0 ) GO TO 103
C   Good cluster.
         IER = 0
         GO TO 98
C   Errors
C   Storeys bank missing.
  101 CONTINUE
      IER = 1
      GO TO 98
C   Too many storeys in the cluster.
  102 CONTINUE
      IER = 2
      GO TO 98
C   Zero storeys in the cluster.
  103 CONTINUE
      IER = 3
      GO TO 98
C   Energy < EMIN.
  104 CONTINUE
      IER = 4
      GO TO 98
C   Energy > EMAX.
  105 CONTINUE
      IER = 5
   98 CONTINUE
      RETURN
      END
#endif
