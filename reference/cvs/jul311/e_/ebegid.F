      SUBROUTINE EBEGID
C----------------------------------------------------------------------
C!  - Built photon bank EGID
C!
C!   Author  : J.Badier                    14/07/90
C!   Modified: C.Goy                       18/12/90
C!              Call Bulos
C!   Modified :- E. Lancon              4-MAR-1991
C!              Modify region flag definition
C!   Modified :- E. Lancon             15-MAY-1991
C!              Make it works with both ECLU and PECO
C!
C!   Bank created  EGID
C!   Banks used ECLU ECT1 ERL3
C!   Call to EBNEUT  CLMOMS (ALEPHLIB)
C?
C=======================================================================
#ifndef DOC
      PARAMETER ( MAXP = 2 )
      DIMENSION IOPT(6),IFOT(19),PHOT(23),CORE(6)
      DIMENSION CMINFO(16)
#include "bcs.h"
#include "eclujj.h"
#include "pecojj.h"
#include "ect1jj.h"
#include "ecnamc.h"
#include "egidjj.h"
      DATA IOPT / 6 * 0 /
#include "bmacro.h"
C-----------------------------------------------------------------------
C   Drop the old EGID bank.
      K = NDROP( 'EGID' , 0 )
C
C?   Work on either PECO or ECLU
C
      KECLU = IW(NAMIND('ECLU'))
      KPECO = IW(NAMIND('PECO'))
C   Have we a Cluster Bank bank ?
      IF( KECLU.LE.0 .AND. KPECO.LE.0 ) GOTO 101
C   How many clusters ?
      IF ( KECLU.GT.0 ) THEN
        NCLU = LROWS(KECLU)
      ELSE
        NCLU = LROWS(KPECO)
      ENDIF
C   KAKA FOR MAXP PARTICLE PER ECLU.
      KAKA = LMHLEN + LEGIDA * MAXP * NCLU
C   Prepare EGID bank
      CALL AUBOS('EGID',0,KAKA,KEGID,IGARB)
      IF( IGARB .EQ. 2 ) GO TO 102
C
      IW(KEGID+2) = 0
      IW(KEGID+1) = LEGIDA
      IF ( IGARB.NE.0) THEN
        KECLU = IW(NAMIND('ECLU'))
        KPECO = IW(NAMIND('PECO'))
      ENDIF
      KECT1 = IW(NAECT1)
C Loop over clusters.
      DO 1 ICLN = 1 , NCLU
C   Is it a neutral cluster ?
        IF (KECLU.GT.0) THEN
          JECT1 = KROW(KECT1,ICLN)
          NERL3 = IW(JECT1+JECTER)
          ICH = 0
          IF (NERL3.NE.0) ICH = 1
          IF ( ICH.NE.0) GO TO 1
        ELSE
          ICH =  ITABL(KPECO,ICLN,JPECRB)
          IF( ICH .NE. 0 .AND. ICH .NE. 2 ) GO TO 1
        ENDIF
C   Treatment of the cluster
        CALL EBNEUT( ICLN , IOPT ,IFOT , PHOT , CORE , IER )
        KEGID = IW(NAMIND('EGID'))
        IF( KEGID .EQ. 0 ) GO TO 101
C   First particle.
        NEGID = LROWS(KEGID) + 1
        IW(KEGID+LMHROW) = NEGID
        KRGID = KROW(KEGID,NEGID)
C   Region flag.
        KFLG = 0
        IF( IER .GT. 0 ) KFLG = 10
C   Near a crack ?
        IF( IFOT(14) .EQ. 11 ) KFLG = 1
C   Into a crack ?
        IF( IFOT(14) .EQ. 12 ) KFLG = 2
C   Near overlap ?
        IF( IFOT(9) .EQ. 1 .OR. IFOT(9) .EQ. 6 ) KFLG = 3
C   Into an overlap ?
        IF( IFOT(9) .GE. 2 .AND. IFOT(9) .LE. 5 ) KFLG = 4
        IF( IER .LT. -2 .AND. IER .GT. -6 ) KFLG = 4
        IW( KRGID + JEGIIF ) = KFLG
C   Depth
        IF( IFOT(15) .EQ. 0 .OR. IFOT(15) .EQ. 1 ) THEN
          RW( KRGID + JEGIE1 ) = PHOT(7)
        ELSE
          RW( KRGID + JEGIE1 ) = -99.
        ENDIF
C   Longitudinal deviation
        IF( IFOT(15) .EQ. 0 .OR. IFOT(15) .EQ. 2 ) THEN
          RW( KRGID + JEGIE2 ) = PHOT(8)
        ELSE
          RW( KRGID + JEGIE2 ) = -99.
        ENDIF
C   Compactness
        RW( KRGID + JEGIE3 ) = PHOT(12)
C   Transverse shape
        RW( KRGID + JEGIE4 ) = PHOT(13)
C   Moment analysis
C   General for the cluster
C   ICLN is either ECLU number or PECO number
        IF (IFOT(18) .LT. 16 ) THEN
           ECOR = PHOT(14)+PHOT(17)
        ELSE
           ECOR = PHOT(4)
        ENDIF
        CALL CLMOMS(ICLN,NIMP,CMINFO,IWARN,IERROR,ECOR)
        IF (IERROR .EQ. 0.) THEN
C         Small sigma of cluster in eigenframe
          RW( KRGID + JEGIE5 ) = CMINFO(1)
C         Large sigma of cluster in eigenframe
          RW( KRGID + JEGIE6 ) = CMINFO(2)
C         Third moment of cluster in direction of Small Sigma
          RW( KRGID + JEGIE7 ) = CMINFO(3)
C         Mass of the cluster
          RW( KRGID + JEGIE8 ) = CMINFO(5)
        ENDIF
C   Nature
        IW( KRGID + JEGINA ) = IFOT(18)
C   Energy
        IF( IFOT(18) .LT. 16 ) THEN
          RW( KRGID + JEGICE ) = PHOT (14)
        ELSE
          CALL EBHAHA( IFOT , PHOT , ENHAD )
          RW( KRGID + JEGICE ) = ENHAD
        ENDIF
C   Theta
        RW( KRGID + JEGITH ) = PHOT(15)
C   Phi
        RW( KRGID + JEGIPH ) = PHOT(16)
C   Pointeur PECO
C       IW( KRGID + JEGIPE ) = IW( KPECO )
        IW( KRGID + JEGIPE ) = ICLN
C   Have we a second particle ?
        IF( IFOT(19) .EQ. 0 ) GO TO 1
C   Add one row.
        NEGID = LROWS(KEGID) + 1
        IW(KEGID+LMHROW) = NEGID
        KRGID = KROW(KEGID,NEGID)
C   Quality flag
        IW( KRGID + JEGIIF ) = KFLG
C   Depth
        IF( IFOT(15) .EQ. 0 .OR. IFOT(15) .EQ. 1 ) THEN
          RW( KRGID + JEGIE1 ) = PHOT(7)
        ELSE
          RW( KRGID + JEGIE1 ) = -99.
        ENDIF
C   Longitudinal deviation
        IF( IFOT(15) .EQ. 0 .OR. IFOT(15) .EQ. 2 ) THEN
          RW( KRGID + JEGIE2 ) = PHOT(8)
        ELSE
          RW( KRGID + JEGIE2 ) = -99.
        ENDIF
C   Compactness
        CALL EBGAGA( 1 , GAM1 , GAM2 )
        RW( KRGID + JEGIE3 ) = GAM1
C   Transverse shape
        RW( KRGID + JEGIE4 ) = GAM2
C   Free.
C   Nature
        IF( IFOT(19) .LE. 3 ) THEN
          IW( KRGID + JEGINA ) = IFOT(19) + 2
        ELSE
          IW( KRGID + JEGINA ) = IFOT(19)
        ENDIF
C   Energy
        RW( KRGID + JEGICE ) = PHOT(17)
C   Theta
        RW( KRGID + JEGITH ) = PHOT(18)
C   Phi
        RW( KRGID + JEGIPH ) = PHOT(19)
C   Pointeur PECO
        IW( KRGID + JEGIPE ) = ICLN
    1 CONTINUE
      CALL AUBPRS('EGID')
   98 CONTINUE
      RETURN
  101 CALL RERROR('EBEGID',1,'Not enough space ')
      GO TO 98
  102 CALL RERROR('EBEGID',2,'Not enough BOS space ')
      GO TO 98
      END
C******************************************************************
#endif
