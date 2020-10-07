*DK asread
      SUBROUTINE ASREAD
C ----------------------------------------------------------------------
C. - F.RANJARD - 850329
C! Read asll input files
C. - Read input tape on unit# LGETIO
C. - Read data base
C. - called by    ASIJOB                           from this .HLB
C -  calls        ABRSEL, ABUNIT, BKINJO           from ALEPHLIB
C.                BLIST, MDARD                     from BOS77
C.                ASIRUN                           from this .HLB
C.
C -----------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "asimjj.h"
#include "kincom.h"
      INTEGER ALRUNH
#include "bmacro.h"
C ----------------------------------------------------------------------
C
C - get the RUN record from the input tape if any
C
      IF (MGETJO.GT.0) THEN
  100   CALL ABRSEL ('E','   ' ,JRET)
        IF (JRET.GT.3) THEN
          IF (JRET.EQ.9) THEN
            CALL ALTELL ('ASREAD:  Time limit reached',0,'STOP')
          ELSEIF (JRET.EQ.11) THEN
            CALL ALTELL ('ASREAD:  Error in data cards',0,'STOP')
          ELSEIF (JRET.LE.16) THEN
            CALL ALTELL ('ASREAD:  Cannot open input or output file'
     &                              ,0,'STOP')
          ELSEIF (JRET.EQ.19) THEN
            CALL ALTELL ('ASREAD:  Not enough space for unpacking',0
     &                              ,'STOP')
          ELSEIF (JRET.EQ.17) THEN
            CALL ALTELL ('ASREAD:  Read error-try again',0,'RETURN')
            GOTO 100
          ELSEIF (JRET.EQ.18) THEN
            CALL ALTELL ('ASREAD: Error in decompressing-go on',0
     &                              ,'RETURN')
          ENDIF
        ENDIF
C
C      get logical unit numbers
        CALL ABUNIT (LGETIO,KUNSEL,KUNSE2,LSAVIO,KUTSEL)
C
C      run record
        IF (JRET.EQ.2) THEN
          IF (IPRIJO(15).EQ.1) CALL AUBLIS ('C')
        ELSE
          WRITE(LOUTIO,*) ' No RUNH bank '
          IF (IPRIJO(15).EQ.1) CALL AUBLIS('E')
        ENDIF
C
      ELSE
C       no input file
         CALL BLIST (IW,'C=','RUNRRUNHKRUN')
         IF (MSAVJO.EQ.1) THEN
C          open output file
            CALL AOPEWR (LGETIO,KUTSEL,LSAVIO,IERW)
            IF (IERW.NE.0) THEN
              CALL ALTELL ('ASREAD: Cannot open FILO file',0,'STOP')
            ENDIF
         ENDIF
      ENDIF
C
C   Get PART bank from data base if not read from cards or tape
C
      JPART = IW(NAPART)
      IF (JPART.EQ.0) THEN
        JPART = MDARD(IW,LRDBIO,'PART',0)
        CALL BKFMT ('PART','2I,(I,3A,I,4F,I)')
        WRITE (LOUTIO,*)' +++ASREAD+++ PART bank read from d.b.'
      ENDIF
      IF (JPART.LE.0) THEN
        CALL ALTELL ('ASREAD: NO PART bank ',0,'STOP')
      ENDIF
C
C   Get GDEC decay modes bank from data base if not there
C
      IF (IW(NAMIND('GDEC')).EQ.0) THEN
        IND=MDARD(IW,LRDBIO,'GDEC',0)
        IF (IND.LE.0) CALL ALTELL ('ASREAD: NO GDEC bank ',0,'STOP')
        CALL BKFMT('GDEC','2I,(I,6F,6I)')
      ENDIF
C
C -  build run type and initialize event generator, build 'RUNH'
C    ASRTYP needs the PART bank
C
      IF (IW(NARUNH) .EQ. 0) THEN
         CALL ASRTYP
         IEXPJO = LOFFMC+1
         JRUNH = ALRUNH (IRUNJO,IEXPJO,IPROKI)
         IF (JRUNH.EQ.0) THEN
            CALL ALTELL ('ASIRUN: not enough space to book RUNH ',1,
     &                   'FATAL')
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
