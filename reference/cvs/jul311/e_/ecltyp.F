      SUBROUTINE ECLTYP
C=======================================================================
C!    Books and fills bank to set cluster type
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-    Modified: M.N  Minard                 04/01/91
C-
C?
C!======================================================================
#ifndef DOC
#include "ecnmsk.h"
#include "ectyjj.h"
#include "eclujj.h"
#include "erl3jj.h"
#include "ect1jj.h"
#include "ecobjj.h"
#include "ehypjj.h"
#include "eidtjj.h"
#include "egidjj.h"
#include "etp1jj.h"
#include "frftjj.h"
#include "bcs.h"
#include "bmacro.h"
C
C- Book ECTY Bank
C
C- Various types of clusters defined:
C
C     -1  isolated cell
C      0  photon or multiphoton
C      1  not electromagnetic neutral
C      2  isolated electron
C      3  not isolated electron
C      4  other charged cluster
C
       KECLU = IW(NAECLU)
       IF ( KECLU.EQ.0) THEN
          GO TO 900
       ENDIF
       NECLU = LROWS(KECLU)
       IF (NECLU.EQ.0) GO TO 900
       NLENG = LMHLEN + NECLU*LECTYA
C
C- Book the bank
C
       CALL AUBOS ('ECTY',0,NLENG,KECTY,IGARB)
       CALL BLIST(IW,'T+','ECTY')
       IW ( KECTY+LMHCOL) = LECTYA
       IW ( KECTY+LMHROW) = NECLU
C
C- Define bank pointers
C
      KECLU = IW(NAECLU)
      KECOB = IW (NAECOB)
      KEIDT = IW (NAEIDT)
      KFRFT = IW (NAFRFT)
      KERL3 = IW (NAERL3)
      KETP1 = IW (NAETP1)
      NAEGID = NAMIND('EGID')
      KEGID = IW (NAEGID)
      NEIDT = 0
      IF (KEIDT.GT.0) NEIDT = LROWS(KEIDT)
C
C- Initialize ECT1 bank
C
       KECT1 = IW(NAECT1)
       IF (KECT1.EQ.0) THEN
          GO TO 900
       ENDIF
C
C- Loop on clusters
C
      DO 100 ICL=1,NECLU
C
C- Look whether it is an isolated cell
C
      JECTY = KROW( KECTY, ICL    )
      JECT1 = KROW( KECT1, ICL    )
      JECLU = KROW( KECLU, ICL    )
      IF ( JECT1 .EQ. 0 ) THEN
         GO TO 100
      ENDIF
C
      CALL ECLTAG(ICL,IGOO,ICH)
C
      IF (ICH.EQ.0) THEN
C
C- Neutral isolated cell
C
        IELM = -1
      ENDIF
      NEGID = LROWS(KEGID)
C
      IF ( ICH.EQ.0 .AND.IGOO.NE.0) THEN
C
C- Find electromagnetic neutral cluster
C
C- Elm shape if  ESTACK3/ETOT<10% AND ESTACK1/ETOT>10% below 1 GeV;
C                above 1 GeV the gamma (or multigamma ) hypothesis
C                is used
C
C
C- Not electromagnetic neutral
C
         IELM = 1
         DO 200 IEGID = 1, NEGID
         IF (ITABL(KEGID,IEGID,JEGIPE).NE.ICL) GO TO 200
C
C- Not electromagnetic neutral
C
         IELM = 1
C
C-       CHECK CLUSTER IS EM
C
         IF(ITABL(KEGID,IEGID,JEGINA).LE.10) IELM = 0
 200     CONTINUE
C
C
C- Charged cluster
C
       ELSEIF(ICH.NE.0) THEN
C
          IELM = 4
C
C-  Flag electron if any
C
          IEDT = 0
          NERL3  = IW (JECT1+JECTER)
 140      JERL3  = KROW(KERL3,NERL3)
          ITRAK  = IW (JERL3+JERLET)
          IECAND = 0
          EMOM   = RTABL(KETP1,ITRAK,JETPPT)
          IXTRAP = ITABL(KETP1,ITRAK,JETPEF)
          CH     = RTABL(KETP1,ITRAK,JETPCH)
          IF ( CH.NE.0..AND.IXTRAP.NE.0) THEN
            DO 300 IIEDT = 1,NEIDT
            IF (ITABL(KEIDT,IIEDT,JEIDFR).NE.ITRAK) GO TO 300
            IEDT = IIEDT
 300        CONTINUE
            IF (IEDT.EQ.0 ) GO TO 208
            JEIDT  = KROW(KEIDT,IEDT)
            IF ( RW ( JEIDT+JEIDR3).GT.-3.  .AND.
     &         RW ( JEIDT+JEIDR3).LT.3.)  THEN
              IF ( RW (JEIDT+JEIDR2).GT.-3.) THEN
C- Not isolated electron
                IELM = 3
                IF ( RW (JEIDT+JEIDR2).LT.3.) THEN
C- Isolated electron
                  IELM = 2
                ENDIF
            ENDIF
          ENDIF
 208    IF ( IW(JERL3+JERLER) .EQ.0) GO TO 210
        NERL3 = IW(JERL3+JERLER)
        GO TO 140
 210    CONTINUE
        ENDIF
        ENDIF
C
C- Fill bank
C
        IW (JECTY+JECTCT) = IELM
C
 100    CONTINUE
C
 900    CONTINUE
C
        RETURN
C
        END

#endif
