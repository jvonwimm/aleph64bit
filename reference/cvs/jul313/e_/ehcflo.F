         SUBROUTINE EHCFLO ( IFLAG , IHSDA , WEIG)
C----------------------------------------------------------------------
C! Give weight to pad energy for energy flow
C
C-   Authors : M.N. Minard , M. Pepe       10/01/90
C-       IFLAG =  0 Initialisation phase
C-             =  1 look for pad weight
C-       IHSDA =  Hcal storey number to use
C-       WEIG  =  Weigth to apply on Pad energy
C?
C!======================================================================
#ifndef DOC
#include "chrljj.h"
#include "hpcojj.h"
#include "hclujj.h"
#include "hsdajj.h"
         PARAMETER (NHCMAX = 100)
         DIMENSION HCUTIL (  NHCMAX )  , SPAT (NHCMAX) ,IDIGH ( 5)
         DIMENSION X (3)
         EXTERNAL NAMIND
#include "bcs.h"
#include "bmacro.h"
         NHCLU = 0
         KHCLU = IW ( NAMIND('HCLU'))
         IF ( KHCLU.NE.0) NHCLU = LROWS ( KHCLU)
         IF ( NHCLU .GT.NHCMAX ) NHCLU = NHCMAX
         IF ( IFLAG.NE.0) GO TO 400
C
C        Determine Hcal clusters to be used
C
         CALL VZERO (HCUTIL,NHCMAX)
         KHCLU = IW (NAMIND('HCLU'))
         KCHRL = IW (NAMIND('CHRL'))
         NCHRL = 0
         IF (KCHRL.NE.0)NCHRL = LROWS(KCHRL)
         NAHPCO = NAMIND ('HPCO')
         KHPCO = IW (NAHPCO)
         NHPCO = 0
         IF (KHPCO.NE.0) NHPCO = LROWS (KHPCO)
C
C-       Pointers to bank telling  digital information status
C
         KEHDS = IW (NAMIND('EHDS'))
         IF ( KEHDS . EQ. 0 ) THEN
            CALL VZERO ( IDIGH , 5 )
         ELSE
            CALL UCOPY ( IW(KEHDS+LMHLEN+1),IDIGH(1),5)
         ENDIF
         DO 10  IHCLU = 1, NHCLU
C-       Determine if cluster is noisy
C
         INOIS = IBITS(ITABL(KHCLU,IHCLU,JHCLNF),0,4)
         IF (INOIS.NE.0) THEN
             HCUTIL(IHCLU) = 0
             GO TO 10
         ENDIF
         HCUTIL(IHCLU) = 1
C
C-       Determine region
C
 10      CONTINUE
         GO TO 900
C
C-       Uses  CELL in E-flow if cluster has been flagged as good
C
C        Search for cluster number
C
 400     KHSDA = IW(NAMIND('HSDA'))
         IHCL =  ITABL ( KHSDA,IHSDA,JHSDCN)
         IF ( IHCL.NE.0) THEN
           WEIG = HCUTIL(IHCL)
         ELSE
           WEIG = 0.
         ENDIF
 900     CONTINUE
         RETURN
         END
#endif
