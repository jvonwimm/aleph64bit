      SUBROUTINE ACLOSE( LUNIT ,IER)
C----------------------------------------------------------------------
CKEY ALREAD CLOSE
C   Created by SCHLATTER             21-AUG-1988
C   modified by F.Ranjard            28-NOV-1989
C
C!   Purpose   : Close   ALEPH BOS files
C    Inputs    :  LUNIT      logical unit
C                       = 0  close ALL units opened with AOPEN/AOPENW
C                            stage out all output files if needed.
C
C    Bank      : '+BUF',nr=LUNIT  as created by  BOS77
C
C    Output:      IER = 0    file properly closed
C                      -1    '+BUF' bank does not exist for this unit
C
C    Calls:   CLOSE,BRWND
C----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "alrcom.h"
      CHARACTER*(80) FNLUN
C --------------------------------------------------------------------
      IER=-1
      IF (LUNIT.NE.0) THEN
C           get index of bank '+BUF',nr=LUNIT
C           IF '+BUF',nr=LUNIT does not exist THEN RETURN
        IND=NLINK ('+BUF',LUNIT)
        IF(IND.NE.0) GOTO 11
        GOTO 999
      ENDIF
C
      IND=NAMIND('+BUF')+1
   10 IND=IW(IND-1)
   11 IF(IND.NE.0) THEN
        IER = 0
        LUN = IW(IND-2)
        IOAC = MOD(IW(IND+1),8)
        IOMD = MOD(IW(IND+1)/8,8)
        IOST = MOD(IW(IND+1)/64,8)
#if defined(ARDEB)
        WRITE(IW(6),*) ' ACLOSE: LUN,IOAC,IOMD,IOST= ',
     &                           LUN,IOAC,IOMD,IOST
#endif
C
C - files opened in direct access (IOAC=1) cannot be REWIND
        IF(IOAC.NE.1) THEN
C          rewind BOS file ( to reset BOS pointers )
          IF(LUNIT.eq.0.and.IOMD.eq.1.and.IOST.eq.2) THEN
C          do not call BRWND for epio(IOMD=1) output(IOST=2) files 
C          at end of job (LUNIT=0) to avoid to call cfrew thru epio twice
#if defined(ARDEB)
           WRITE(IW(6),*) ' ACLOSE: EOJob - do not EPRWND output',lun
#endif
          ELSE
            CALL BRWND( LUN )
          ENDIF
C
C          rewind "TEXT" files ( not yet done in BRWND )
          IF (IOMD.EQ.2) REWIND LUN
        ENDIF
C
C - epio files: when using EPIO C (IC=2) rewind the file and close it
C               reset the direct access flag to sequential
        IF (IOMD.EQ.1) THEN
           CALL EPGETW (LUN,33,IC,IST)
           IF (IC.EQ.2) THEN
              IF (IOAC.EQ.1) CALL BRWND(LUN)
              CALL EPGETW (LUN,25,LDES,IST)
#if defined(ARDEB)
              WRITE(IW(6),*) ' ACLOSE: LUN,IOAC,IOMD,IC,LDES= ',
     &                                 LUN,IOAC,IOMD,IC,LDES
#endif 
              IF (LDES.NE.0) CALL CFCLOS (LDES,0)
           ENDIF
           CALL EPSETW (LUN,32,0,IST)
        ENDIF
C
        CLOSE (LUN,IOSTAT=IST)
#if defined(ARDEB)
        WRITE(IW(6),*) ' ACLOSE: CLOSE LUN= ',LUN,' IST= ',IST
#endif 
C
C          drop BOS buffer
        IDRP = NDROP ('+BUF',LUN)
C
C           more UNITs to close ?
        IF(LUNIT.EQ.0) GOTO 10
      ENDIF
C
C     Call ASTAGE to drop any/all staged data disks
C     This will submit output staging jobs to copy to tape
      IF ( LUNIT.EQ.0 ) THEN
        CALL ASTAGE(LUNIT,' ',IRSTG)
        IF ( IER.EQ.0 ) IER = IRSTG
        CALL DROPDK
      END IF
C
 999  CONTINUE
#if defined(ARDEB)
      WRITE (IW(6),*) ' ACLOSE LUNIT= ', LUNIT, ' IER= ',IER
#endif
      END
#endif









