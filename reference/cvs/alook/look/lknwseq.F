      SUBROUTINE LKNWSEQ(COMAND)
C ------------------------------------------------------------
C! open a new sequential file on input
C ------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkfil.h"
      CHARACTER FILENM*80, FORMA*6, CH, TAPE*9
      CHARACTER*(*) COMAND
      LOGICAL LKOPTI
      INTEGER ALSTGQRY
      PARAMETER (MXTYP=5)
      CHARACTER*4 TYPE,DATYP(MXTYP)
      DATA DATYP /'RAW ','POT ','DST ','MINI','NANO'/

C ----------------------------------------------------------
      TYPE = ' '
      LRUK7 = 9
      KSRUN = NDROP('SRUN',0)
      DO 1 ITYP =1,MXTYP
         IF (LKOPTI(COMAND,DATYP(ITYP))) THEN
            TYPE = DATYP(ITYP)
            CALL LKGTINT (COMAND,1,'  ',NRUN,CH)
            IF (CH.NE.' ' .AND. CH.NE.'I') THEN
               WRITE(LOUT,*) '[Invalid run number]'
               RETURN
            ENDIF
            IRK7 = LKK7RU  (LRUK7,NRUN,TYPE,TAPE,FILENM,FORMA)
            IF (IRK7.EQ.0) THEN
               LF = LNBLNK(FILENM)
               KSRUN = NBANK('SRUN',0,1)
               IW(KSRUN+1) = NRUN
               LASRUN = NRUN
               IF (LKOPTI(COMAND,'NSEQ')) LASRUN = 999999
               LASEVT = 999999
               CALL ABSMAX (LASRUN,LASEVT)
               GOTO 100
            ELSE
               RETURN
            ENDIF
         ENDIF
 1    CONTINUE
C
      IF (LKOPTI(COMAND,'CART')) THEN
         FORMA = 'CART'
      ELSEIF (LKOPTI(COMAND,'MOUNT')) THEN
         FORMA = 'MOUNT'
      ELSEIF (LKOPTI(COMAND,'EPIO')) THEN
         FORMA = 'EPIO'
      ELSEIF (LKOPTI(COMAND,'EDIR')) THEN
         FORMA = 'EDIR'
      ELSEIF (LKOPTI(COMAND,'NATIVE')) THEN
         FORMA = 'NATIVE'
      ENDIF
      CALL LKGTPAR(COMAND,1,'_File: ',FILENM,LF)
C
 100  CONTINUE
C
      IF (LKOPTI(COMAND,'QUERY')) THEN
         IER = ALSTGQRY (FILENM)
         IF (IER.EQ.0) THEN
            WRITE(LOUT,*)'[file ',FILENM(1:LF),' is ',
     &                   'NOT staged]'
         ELSE
            WRITE(LOUT,*)'[file ',FILENM(1:LF),' is ',
     &                   'staged]'
         ENDIF
         RETURN
      ENDIF
C
      SEQNAM=FILENM
#if defined(DEBUG)
      WRITE(LOUT,*) ' LKNWSEQ COMAND= ',COMAND
      WRITE(LOUT,*) ' LKNWSEQ FILENM= ', FILENM(1:LF)
      WRITE(LOUT,*) ' LKNWSEQ FORMA = ', FORMA
#endif
C - open new input file and read it till the 1st event
      CALL LKOPRD(FILENM,FORMA,IER)
      KSRUN = NDROP('SRUN',0)
      END
#endif
