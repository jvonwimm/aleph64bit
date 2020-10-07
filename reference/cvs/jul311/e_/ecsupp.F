      SUBROUTINE ECSUPP ( IECLU , ISUP )
C----------------------------------------------------------------------
C!  - Flag 'Noisy' Ecal Clusters
C
C   Author:   M.N.Minard   18.9.89
C
C   Output:
C?
C!======================================================================
#ifndef DOC
#include "eclujj.h"
#include "esdajj.h"
#include "pewijj.h"
#include "bcs.h"
      EXTERNAL NAMIND
      DIMENSION EFIL (36) ,ESTACK (3) , IFIL (36)
      DIMENSION NELEM (3) ,NAVE (3)
#include "bmacro.h"
C
C-     Initialise pointers
C
       CALL VZERO ( IFIL ,36 )
       CALL VZERO ( EFIL ,36 )
       CALL VZERO ( ESTACK,3)
       CALL VZERO (NELEM,3)
       EWSUM = 0
       NAEWDI = NAMIND ('EWDI')
       NAEWHE = NAMIND ('EWHE')
       KEWHE = IW (NAEWHE)
       NEWHE = 0
       IF (KEWHE.NE.0) NEWHE = LROWS(KEWHE)
       IF(NEWHE.GT.36.OR.NEWHE.LE.0)GOTO 900
       IF (KEWHE.EQ.0) GO TO 900
       NAECLU = NAMIND ('ECLU')
       KECLU = IW (NAECLU)
       ISUP = 0
       IF ( KECLU.EQ.0) GO TO 900
       NAESDA = NAMIND('ESDA')
       KESDA = IW (NAESDA)
       IF ( KESDA.EQ.0) GO TO 900
       IESDA = ITABL (KECLU,IECLU,JECLES)
 20    IF ( IESDA.EQ.0) GO TO 60
       JESDA = KROW (KESDA , IESDA )
       ITOW = IW(JESDA+JESDIO)
       IK = IW(JESDA+JESDDK)
       ERES = RW(JESDA+JESDME)
       IPHI = IW (JESDA+JESDFI)
       ITETA = IW ( JESDA + JESDTJ)
       CALL ECSCMD ( IPHI,ITETA,IMOD,ISC)
       IGMOD = (ISC-1)*12+IMOD
       IFIL ( IGMOD ) = IFIL ( IGMOD ) + 1
       NELEM (IK) = NELEM (IK) + 1
       EFIL ( IGMOD) = EFIL ( IGMOD ) + ERES
       ESTACK (IK) = ESTACK ( IK ) + ERES
       IESDA = IW( JESDA+JESDES )
       IF ( IESDA.NE.0 ) GO TO 20
 60    CONTINUE
C
C-     Check if wire energy is not negligeable
C
       NMOD = 0
       NCUT = 0
       DO 100 IL =1,36
       IF (EFIL(IL).LT.2.) GO TO 100
C
C-     Look to wire energy
C
       NMOD = NMOD +1
       DO 80 IEWHE = 1,NEWHE
       IF ( ITABL(KEWHE,IEWHE,2).NE.IL) GO TO 80
       EWIR = FLOAT(ITABL(KEWHE,IEWHE,1))/1000000.
       IF (( EWIR.LT.EFIL(IL)*0.2)) NCUT = NCUT + 1
 80    CONTINUE
 100   CONTINUE
       IF ((NCUT.EQ.NMOD).AND.NMOD.NE.0) THEN
          ISUP = 1
          GO TO 900
       ENDIF
C
C-     Now check cluster uniformity
C
       IF (RTABL(KECLU,IECLU,JECLE4).LT.1.5) GO TO 900
       DO 120 IK =1,3
       IF ( NELEM (IK).EQ. 0) GO TO 120
       ESTACK ( IK ) = ESTACK (IK)/NELEM(IK)
 120   CONTINUE
       CALL VZERO (NAVE,3)
       IESDA = ITABL (KECLU,IECLU,JECLES)
       IF ( IESDA.EQ.0) GO TO 140
 130   IK = ITABL ( KESDA,IESDA,JESDDK)
       ERES = RTABL (KESDA,IESDA,JESDME)
       IF ( ABS(ERES-ESTACK(IK)).LT.0.2*ESTACK(IK))NAVE(IK)=NAVE(IK)+1
       IESDA = ITABL ( KESDA,IESDA,JESDES)
       IF ( IESDA.NE.0 ) GO TO 130
 140   CONTINUE
C
C-     Now make diagnostic
C
      DO 210 IK = 1,3
      IF (ESTACK(IK).LE.0.07) GO TO 210
      IF (NELEM(IK).LT.30) GO TO 210
      IF ( NAVE(IK).GT.0.6*NELEM(IK)) THEN
         ISUP = 10
      ENDIF
 210  CONTINUE
 900  CONTINUE
      RETURN
      END
#endif
