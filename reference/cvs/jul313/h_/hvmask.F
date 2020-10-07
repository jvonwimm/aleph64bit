      SUBROUTINE HVMASK
C
C----------------------------------------------------------------------
C!   - Reads the HV masks for HCAL from the HSSR Slow Control bank
C!
C!    Author      :- A. Sciaba`  9-APR-1997
C!    Modified    :- M.Cattaneo 23-MAY-1997
C!              Test JHSSR and not NHSSR for existence of HSSR....
C!    Modified    :- A. Sciaba`  2-JUL-1997
C!              Get run number with ABRUEV rather than from rcurnt.h  
C!    Modified    :- A. Sciaba`  6-AUG-1997
C!              Get database from LRGEOM rather than LRCONS
C!
C!    Libraries required: none
C!
C!    Description
C!    ===========
C!    This routine reads the HSSR bank, which cointains the voltage of
C!    every HV channel in HCAL and mu-chambers or the value -3 if a
C!    channel is masked.
C!    It uses the HVBA, HVEA and HVEB database banks to find the
C!    correspondence to the HCAL planes and to store in the
C!    MASKHV(36,37) matrix the masking information.
C!    The first index in MASKHV is the submodule number (1-24 barrel,
C!    25-30 Endcap A, 31-36 Endcap B) and the second index is the plane
C!    number in case of the barrel (1-23); for the endcaps, it is
C!    the plane number in the range (1-7), in the range (8-22)
C!    is the first part of the planes (8-22) and in (23-37) is the
C!    second part of the planes (8-22).
C!
C!======================================================================
C
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "hcmask.h"
      INTEGER IBARR, IENDA, IENDB, ALGTDB
      LOGICAL DATA
      CHARACTER*12 LIST
      DIMENSION IBARR(240), IENDA(160), IENDB(160)
      EXTERNAL ALGTDB, NAMIND
      DATA LIST / 'HVBAHVEAHVEB' /
#include "bmacro.h"
C
C     Initialize the mask vectors
      DO 1 I = 1, 240
         IBARR(I)=1
 1    CONTINUE
      DO 2 I = 1, 160
         IENDA(I)=1
         IENDB(I)=1
 2    CONTINUE
C
      DO 3 I = 1, 36
         DO 4 J = 1, 37
            MASKHV(I,J) = 1
 4       CONTINUE
 3    CONTINUE
C
      DATA=.TRUE.
      CALL ABRUEV(IRUN,IEVT)
      IF(IRUN.LE.2000) DATA=.FALSE.
C
C     Read the HSSR bank for masked HV channels
      NHSSR=NAMIND('HSSR')
      JHSSR=IW(NHSSR)
      IF(JHSSR.EQ.0)THEN
         IF (DATA) CALL
     &        RERROR('HVMASK',1,'HSSR (start-of-run) bank not found')
         GOTO 99
      ENDIF
C
      DO 5 I = 1, 240
         NBA=IW(JHSSR+3+I)
         IF(NBA.EQ.-3)IBARR(I)=0
 5    CONTINUE
C
      DO 6 I = 1, 160
         NB1=IW(JHSSR+3+240+I)
         NB2=IW(JHSSR+3+240+160+I)
         IF(NB1.EQ.-3)IENDA(I)=0
         IF(NB2.EQ.-3)IENDB(I)=0
 6    CONTINUE
C
C     Load from the database the HVBA, HVEA, HVEB banks
C
      IRET=ALGTDB(LRGEOM,LIST,IRUN)
      IF(IRET.EQ.0) THEN
         IF(DATA) CALL
     &        RERROR('HVMASK',2,'HVBA, HVEA or HVEB bank not in DB')
      ENDIF
C
C     Barrel: HVBA
      NHVBA=NAMIND('HVBA')
      JHVBA=IW(NHVBA)
      IF(JHVBA.EQ.0) THEN
         IF(DATA) CALL RERROR('HVMASK',3,
     &        'HV map bank for the barrel HVBA not found')
         GOTO 10
      ENDIF
C
      DO I = 1, 6                 ! loop on the CAEN crates
         DO J = 1, 40             ! loop on the HV channels
            NMASK=ITABL(JHVBA,I,J)
            IF(NMASK.EQ.0) GOTO 7
            IMOD=NMASK/100
            IPLA=NMASK-IMOD*100+1
            IF(IBARR((I-1)*40+J).EQ.0)THEN
               IF(IPLA.EQ.1)THEN
                  MASKHV(IMOD,1)=0
                  MASKHV(IMOD,2)=0
                  MASKHV(IMOD,3)=0
               ELSE
                  MASKHV(IMOD,IPLA)=0
                  MASKHV(IMOD,IPLA+1)=0
                  MASKHV(IMOD,IPLA+2)=0
                  MASKHV(IMOD,IPLA+3)=0
               ENDIF
            ENDIF
 7       ENDDO
      ENDDO
C
C     Endcap A: HVEA
 10   NHVEA=NAMIND('HVEA')
      JHVEA=IW(NHVEA)
      IF(JHVEA.EQ.0) THEN
         IF (DATA) CALL RERROR('HVMASK',4,
     &        'HV map bank for the endcap A HVEA not found')
         GOTO 11
      ENDIF
C
      DO I = 1, 4                 ! loop on the CAEN crates
         DO J = 1, 40             ! loop on the HV channels
            NMASK=ITABL(JHVEA,I,J)
            IF(NMASK.EQ.0) GOTO 8
            IMOD=NMASK/100
            IPLA=NMASK-IMOD*100+1
            IF(IENDA((I-1)*40+J).EQ.0)THEN
               IF(IPLA.EQ.37)THEN
                  MASKHV(IMOD+24,37)=0
               ELSE
                  MASKHV(IMOD+24,IPLA)=0
                  MASKHV(IMOD+24,IPLA+1)=0
               ENDIF
 8          ENDIF
         ENDDO
      ENDDO
C
C     Endcap B: HVEB
 11   NHVEB=NAMIND('HVEB')
      JHVEB=IW(NHVEB)
      IF(JHVEB.EQ.0) THEN
         IF(DATA) CALL RERROR('HVMASK',5,
     &        'HV map bank for the endcap B HVEB not found')
         GOTO 99
      ENDIF
C
      DO I = 1, 4                 ! loop on the CAEN crates
         DO J = 1, 40             ! loop on the HV channels
            NMASK=ITABL(JHVEB,I,J)
            IF(NMASK.EQ.0) GOTO 9
            IMOD=NMASK/100
            IPLA=NMASK-IMOD*100+1
            IF(IENDB((I-1)*40+J).EQ.0)THEN
C     CAEN 4 is different:
               IF(I.EQ.4) THEN
                  IF(IPLA.EQ.29)THEN
                     MASKHV(IMOD+30,29)=0
                  ELSE
                     MASKHV(IMOD+30,IPLA)=0
                     MASKHV(IMOD+30,IPLA+1)=0
                  ENDIF
C     Go back to the general case:
               ELSE
                  IF(IPLA.EQ.37)THEN
                     MASKHV(IMOD+30,37)=0
                  ELSE
                     MASKHV(IMOD+30,IPLA)=0
                     MASKHV(IMOD+30,IPLA+1)=0
                  ENDIF
               ENDIF
            ENDIF
 9       ENDDO
      ENDDO
C
 99   RETURN
      END
#endif
