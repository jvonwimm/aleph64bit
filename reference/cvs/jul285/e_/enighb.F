      SUBROUTINE ENIGHB(NPT,IROSE,IDTPC)
C
C***********************************************************************
C!FIND CLUSTERS NEAR A STOREY                                          *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : J.P. ALBANESE 860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!  MODIFIED : A. BONISSENT  870109 : NEW ENCODING OF THE ROSEVE       *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C           ENIGHB                                                     *
C!                                                                     *
C! TO TEST CONTENT OF A STOREY IN THE ARRAY MAP AND                    *
C! TO FIND THE NEIGBOUR STOREY OF A GIVEN ONE (NPT,IROSE)              *
C!      ACTION>>>                                                      *
C!  USE CLUSTERING ALGORITHM (RECURSIVE SEARCH) BUT STOP AT FIRST LEVEL*
C!   OF TREE SCANNING                                                  *
C FOR EACH CLUSTER FOUND, CALL EINSRL TO CREATE RELATION WITH TRACK    *
C!                                                                     *
C!      INPUT : NPT   = STOREY ADDRESS IN ARRAY,                       *
C!              IROSE = LOCAL GEOMETRY INDEX,                          *
C!              IDTPC = TRACK NUMBER                                   *
C!                                                                     *
C!      OUTPUT : NONE                                                  *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
#include "eclcom.h"
#include "ecnamc.h"
#include "esdajj.h"
#include "bcs.h"
#include "bmacro.h"
C     CONSTANTS : NPST = NUMBER OF STOREYS PER STACK,
C                 NPBAN = NUMBER PER BANANA
C
      NPST=73728
      NPBAN=6144
C
C?-
C? SET STOREY TO ANALYSE
C?
C? Protection against wrong encoding in association
      IF (NPT .EQ. 0) GOTO 999
C
      IADD=NPT
      KESDA=IW(NAESDA)
C   COMPUTE STACK NUMBER (-1)
      K=(NPT-1)/NPST
C   INDEX INSIDE THE STACK
      IKST=NPT-K*NPST
C   BANANA NUMBER (-1)
      IBAN=(IKST-1)/NPBAN
C   POINTER INTO THE SECOND ARRAY (FROM POSITION INSIDE THE BANANA)
      KIB=IARRAY(IKST-IBAN*NPBAN)
C NOW TEST AT EACH LEVEL IF AREA IS OF INTEREST
      IF(KIB.NE.0)THEN
C        COMPUTE # OF THE FIRST STOREY OF THE TOWER IN WHICH WE ARE
         ITOW =IW(KEWLOC+KIB+IBAN)
         IF(ITOW.NE.0)THEN
C           COMPUTE STOREY NUMBER
            ISTO=ITOW+K
            ICCL=ITABL(KESDA,ISTO,JESDEC)
            IF(ICCL.NE.0)THEN
C?
C?             IF STOREY IS HIT, WE NEED NOT SCAN
C?             THE NEIGHBOURING STOREYS,
C?             SINCE THEY BELONG TO THE SAME CLUSTER
C?
               CALL EINSRL(IDTPC,ICCL)
               GO TO 999
            ENDIF
C
         ENDIF
      ENDIF
   10 IROSE=IROSE + 1
      IF (IROSEV(IROSE) .NE. 0) THEN
C?
C? LOOP AS LONG AS THE IROSEV_ARRAY HAS NOT BEEN COMPLETELY SCANNED
C?
         NPT=IADD +IROSEV(IROSE)
         K=(NPT-1)/NPST
         IKST=NPT-K*NPST
         IBAN=(IKST-1)/NPBAN
         KIB=IARRAY(IKST-IBAN*NPBAN)
         IF(KIB.NE.0)THEN
            ITOW =IW(KEWLOC+KIB+IBAN)
            IF(ITOW.NE.0)THEN
               ISTO=ITOW+K
               ICCL=ITABL(KESDA,ISTO,JESDEC)
               IF(ICCL.NE.0)THEN
                  CALL EINSRL(IDTPC,ICCL)
               ENDIF
C
            ENDIF
         ENDIF
C?
C?       END LOOP ON THE IROSEV ARRAY
C?
      GO TO 10
      ENDIF
  999 CONTINUE
      RETURN
      END
#endif
