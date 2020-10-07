      SUBROUTINE EMFILL(IEMSK,EMIN,DCOST,ITRAC)
C=======================================================================
C!    Fills EMSK bank with minimum ionizing particle
C-
C-    Authors : M.N. Minard , M. Pepe       26/02/89
C-
C-    Input: IEMSK      track index
C-           EMIN (5)   estimated min ionizing energy per stack
C-           DCOST(3,5) direction cosines of extrapolated track/stack
C-           ITRAC      number & addresses of crossed cells
C
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "ecnmsk.h"
#include "esdajj.h"
#include "hsdajj.h"
#include "ectejj.h"
#include "hctejj.h"
#include "bcs.h"
      DIMENSION EMIN(5),DCOST(3,5),ITRAC(5,5),EVIS(5)
      DIMENSION ECUS(4,5) ,SECUS(5) , EVISC(4,5)
      CHARACTER*4 NAME,CHAINT
      DATA NAECTE/0/
#include "bmacro.h"
C
C     Set pointers
C
      IF (NAECTE.EQ.0) THEN
        NAECTE = NAMIND ('ECTE')
        NAHCTE = NAMIND ('HCTE')
        NAEMSK = NAMIND ('EMSK')
      ENDIF
      KECTE = IW(NAECTE)
      KHCTE = IW(NAHCTE)
      NECTE = 0
      IF (KECTE.NE.0) NECTE = LROWS (KECTE)
      NHCTE = 0
      IF (KHCTE.NE.0) NHCTE = LROWS(KHCTE)
      KEMSK = IW(NAEMSK)
      JEMSK = KROW(KEMSK,IEMSK)
C
C
C-    Initialize counter
C
      CALL VZERO ( EVIS ,5 )
      CALL VZERO ( SECUS,5 )
      CALL VZERO ( ECUS ,20)
      CALL VZERO ( EVISC,20)
C
C-    loop on ESDA
C
      KESDA = IW (NAESDA)
      NESDA = 0
      IF (KESDA.GT.0) THEN
        NESDA = LROWS(KESDA)
        DO 10 IESDA = 1,NESDA
          JESDA  = KROW(KESDA,IESDA)
          IK     = IW (JESDA+JESDDK)
          NC     = ITRAC(1,IK)
          IF ( NC.NE.0 ) THEN
            EN     = RW ( JESDA+JESDME)
            ITOW   = IW ( JESDA+JESDIO)
C
C-      Loop on cells crossed by track
C
            DO  5 I = 1,NC
              IF (ITRAC(1+I,IK).EQ.ITOW) THEN
C
C-           Store energy available along track..
C-        ...per stack
C
                EVIS (IK  ) = EVIS(IK)+EN
C
C-        ...per cell
C
                EVISC(I,IK) = EN
C
C-           Loop on ECTE , calculate available energy
C            if ECTE does not exist then NECTE=0
                DO 4 IECTE = 1,NECTE
                  IF ( ITOW.EQ.ITABL(KECTE,IECTE,JECTCA) )THEN
C
C-              Energy already used per cell..
C
                    ECUS (I,IK) = RTABL (KECTE,IECTE,JECTCE)
C
C-           ...per stack
C
                    SECUS(IK  ) = SECUS(IK)+ECUS(I,IK)
                  ENDIF
    4           CONTINUE
              ENDIF
    5       CONTINUE
          ENDIF
   10   CONTINUE
      ENDIF
C
C-    loop on HSDA
C
      KHSDA = IW (NAHSDA)
      NHSDA = 0
      IF (KHSDA.GT.0) THEN
        NHSDA = LROWS(KHSDA)
        DO 30 IHSDA = 1,NHSDA
          JHSDA  = KROW(KHSDA,IHSDA)
          IK     = IW (JHSDA+JHSDSN)
          NC     = ITRAC(1,IK+3)
          IF ( NC.NE.0 ) THEN
            IPHI   = IW ( JHSDA+JHSDPI )
            ITET   = IW ( JHSDA+JHSDTI )
            EN     = RW ( JHSDA+JHSDDE )
            ITOW   = (ITET-1) * 256 + (IPHI-1) * 2 + IK - 1
C
C-      Loop on cell crossed by track
C
            DO 25 I = 1,NC
              IF (ITRAC(1+I,IK+3).EQ.ITOW) THEN
C
C-
                EVIS(IK+3) = EVIS(IK+3)+EN
                EVISC (I,IK+3) = EN
C
C-           Loop on HCTE , calculate available energy
C            if HCTE does not exist NHCTE=0
                DO 24 IHCTE = 1,NHCTE
                  IF ( ITOW.EQ.ITABL(KHCTE,IHCTE,JHCTCA)) THEN
                    ECUS(I,IK+3) = RTABL (KHCTE,IHCTE,JHCTCE)
                    SECUS(IK+3)  = SECUS(IK+3)+ECUS(I,IK+3)
                  ENDIF
   24           CONTINUE
              ENDIF
   25       CONTINUE
          ENDIF
   30   CONTINUE
      ENDIF
      IF(NESDA.EQ.0.AND.NHSDA.EQ.0) GO TO 50
C
C
C-       Now estimate energy to subtract
C
      DO 40 IK=1,5
C
C-       Available energy per stack
C
        ELEFT = EVIS(IK)-SECUS(IK)
        IF(ELEFT.LT.0.01) GO TO 40
C
C-       Compare left over energy to min. ionizing expected deposition
C
        EREM = EMIN (IK)
        IF (ELEFT.LT.EMIN(IK)) EREM = ELEFT
C
C-       Calculate average per cell to be removed
C
        IF(ITRAC(1,IK).GT.0) THEN
          ERCEL = EREM/ITRAC(1,IK)
        ENDIF
C
C-       Loop on track elements
C
        NC = ITRAC(1,IK)
        IF(NC.EQ.0) GO TO 40
        EREMS = EREM
C
        DO 35 IC = 1,NC
          IF ( EVISC(IC,IK).LT.0.001 ) GO TO 35
          ELEFC = EVISC(IC,IK)-ECUS(IC,IK)
          IF (ELEFC.GT.ERCEL) ELEFC=ERCEL
          EREMS = EREMS-ELEFC
C
C-       Fill corresponding ECTE , HCTE BANK
C
C-       Update corresponding HCTE , ECTE cell
C
          KKCTE = IW(NAECTE)
          IF(IK.GT.3) KKCTE = IW(NAHCTE)
          IF (ECUS(IC,IK) .LE. 0.) THEN
            IF (LFRROW(KKCTE).LT.1) THEN
              LENN=IW(KKCTE)+LCOLS(KKCTE)
              NAME = CHAINT (IW(KKCTE-3))
              CALL AUBOS(NAME,0,LENN,KKCTE,IGARB)
              IF(IGARB.EQ.2) GOTO 50
              KECTE = IW(NAECTE)
              KHCTE = IW(NAHCTE)
            ENDIF
            JKCTE = KNEXT(KKCTE)
            IW(JKCTE+ JHCTCA) = ITRAC(1+IC,IK)
            RW(JKCTE+ JHCTCE) = ELEFC
            IW(KKCTE+ LMHROW) = IW(KKCTE+LMHROW)+1
          ELSE
            JKCTE = 0
            DO 33 IKCTE = 1,LROWS(KKCTE)
              IF(ITABL(KKCTE,IKCTE,JHCTCA).EQ.ITRAC(IC+1,IK)) THEN
                JKCTE = KROW(KKCTE,IKCTE)
                RW(JKCTE+JHCTCE) = RW(JKCTE+JHCTCE)+ELEFC
              ENDIF
   33       CONTINUE
          ENDIF
   35   CONTINUE
C-
C
C     Fill energy
C
        IND = (IK-1)*3
        RW ( JEMSK + IND +1 ) = EREM-EREMS
C
C-    Calculate angles
C
        RST = SQRT(DCOST(1,IK)**2+DCOST(2,IK)**2)
        RW(JEMSK+IND+2) = ATAN2(RST,DCOST(3,IK))
        DPHI = ATAN2(DCOST(2,IK),DCOST(1,IK))
        IF ( DPHI.GT.TWOPI) DPHI = DPHI-TWOPI
        IF ( DPHI.LT. 0.)   DPHI = DPHI+TWOPI
        RW ( JEMSK+IND+3) = DPHI
   40 CONTINUE
   50 CONTINUE
      RETURN
      END
#endif
