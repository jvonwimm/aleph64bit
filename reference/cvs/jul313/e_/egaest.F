      SUBROUTINE EGAEST
C----------------------------------------------------------------------
C!  - Built photon normalised estimator bank
C!     Author                             M.N Minard 10/02/92
C!
C!   Bank created  EGNE
C!   Banks used ECLU ECT1 ERL3
C!   Data base banks used EF4N , EW1N , EW2N , EL1N , EL2N
C!   call to EF4NOR , EW1NOR , EW2NOR , EL1NOR , EL2NOR
C!   Call to EBNEUT  CLMOMS (ALEPHLIB)
C?
C=======================================================================
#ifndef DOC
      DIMENSION IOPT(6),IFOT(19),PHOT(23),CORE(6)
      DIMENSION CMINFO(16)
#include "bcs.h"
#include "eclujj.h"
#include "pecojj.h"
#include "ecobjj.h"
#include "ect1jj.h"
#include "ecnamc.h"
#include "egnejj.h"
#include "egtejj.h"
      DATA NAEGTE / 0 /
#include "bmacro.h"
C
      IF (NAEGTE.EQ.0) THEN
        NAPECO = NAMIND('PECO')
        NAEGTE = NAMIND('EGTE')
        NAECOB = NAMIND('ECOB')
        NAECLU = NAMIND('ECLU')
        NAECT1 = NAMIND('ECT1')
      ENDIF
C
C   Initialise  threshold
C
      ETHGAM = 0.
      KEGTE = IW(NAEGTE)
      NEGTE = 0
      IF ( KEGTE . NE . 0 ) NEGTE = LROWS(KEGTE)
      DO IEGTE = 1,NEGTE
        IF ( ITABL(KEGTE,IEGTE,2) .EQ.1 ) THEN
          ETHGAM = RTABL(KEGTE,IEGTE,JEGTET)
        ENDIF
      ENDDO
C   Drop the old EGNE bank.
      K = NDROP( 'EGNE' , 0 )
C
C?   Work on either PECO or ECLU
C
      KECLU = IW(NAECLU)
      KPECO = IW(NAPECO)
      KECOB = IW(NAECOB)
C   Have we a Cluster Bank bank ?
      IF( KECLU.LE.0 .AND. KPECO.LE.0 ) GOTO 999
C   How many clusters ?
      IF ( KECLU.GT.0 ) THEN
        NCLU = LROWS(KECLU)
      ELSE
        NCLU = LROWS(KPECO)
      ENDIF
C   KAKA FOR MAXP PARTICLE PER ECLU.
      KAKA = LMHLEN + LEGNEA  * NCLU
C   Prepare EGID bank
      CALL AUBOS('EGNE',0,KAKA,KEGNE,IGARB)
      IF( IGARB .EQ. 2 ) GO TO 102
C
      IW(KEGNE+2) = 0
      IW(KEGNE+1) = LEGNEA
      IF ( IGARB.NE.0) THEN
        KECLU = IW(NAECLU)
        KECOB = IW(NAECOB)
        KPECO = IW(NAPECO)
      ENDIF
      KECT1 = IW(NAECT1)
C Loop over clusters.
      DO 1 ICLN = 1 , NCLU
C
C     Is cluster above threshold
C
        IF (KECLU.GT.0) THEN
          EC = RTABL(KECOB,ICLN,JECOEG)
          THET = RTABL(KECLU,ICLN,JECLT4)
          KCODE = ITABL(KECOB,ICLN,JECORC)
          JECT1 = KROW(KECT1,ICLN)
          NERL3 = IW(JECT1+JECTER)
          ICH = 0
          IF (NERL3.NE.0) ICH = 1
          IF ( ICH.NE.0) GO TO 1
        ELSE
          EC = RTABL(KPECO,ICLN,JPECEC)
          THET = RTABL(KPECO,ICLN,JPECTH)
          KCODE = ITABL(KPECO,ICLN,JPECKD)
          ICH =  ITABL(KPECO,ICLN,JPECRB)
          IF( ICH .NE. 0 .AND. ICH .NE. 2 ) GO TO 1
        ENDIF
C
C   Is it a neutral cluster ?
C
        IF (EC.LT.ETHGAM) GO TO 1
C   Treatment of the cluster - Longitudinal estimator
        CALL EBNEUT( ICLN , IOPT ,IFOT , PHOT , CORE , IER )
        KEGNE = IW(NAMIND('EGNE'))
        IF( KEGNE .EQ. 0 ) GO TO 999
C   Store photon estimator
        NEGNE = LROWS(KEGNE) + 1
        IW(KEGNE+LMHROW) = NEGNE
        KRGNE = KROW(KEGNE,NEGNE)
        CALL ENOL12 (PHOT(7),PHOT(8),EC,THET,ESL1N,ESL2N)
        CALL ENOF4  (PHOT(12),EC,THET,ESF4N)
        IW(KRGNE+JEGNRC) = KCODE
        IW(KRGNE+JEGNNA) = IFOT(18)
        RW(KRGNE+JEGNL1) = ESL1N
        RW(KRGNE+JEGNL2) = ESL2N
        RW(KRGNE+JEGNF4) = ESF4N
C   Moment analysis
C   General for the cluster
C   ICLN is either ECLU number or PECO number

        CALL CLMOMS(ICLN,NIMP,CMINFO,IWARN,IERROR,EC)
        CALL ENOW12(CMINFO(1),CMINFO(2),EC,THET,ESW1N,ESW2N)
        IF (IERROR .EQ. 0.) THEN
          RW(KRGNE+JEGNBM) = CMINFO(5)
          RW(KRGNE+JEGNW1) = ESW1N
          RW(KRGNE+JEGNW2) = ESW2N
        ENDIF
C   Pointeur PECO
        IW( KRGNE + JEGNPE ) = ICLN
    1 CONTINUE
      CALL AUBPRS('EGNE')
   98 CONTINUE
  999 RETURN
  102 CALL RERROR('EGAEST',2,'Not enough BOS space ')
      GO TO 98
      END
#endif
