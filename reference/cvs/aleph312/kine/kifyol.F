      SUBROUTINE KIFYOL
C -------------------------------------------------------------------
C - F.Ranjard - 890202
CKEY KINE KINGAL BANK FORMAT  / USER  INTERNAL
C! return old format of KINE and/or FKIN
C  bank KINE and/or FKIN is modified in place :
C  KINE : word(4) will contain the energy of the track defined by KINE
C  FKIN : word(4) will contain the energy of the track defined by KINE
C  the routine can be called for any file (old or new format) :
C  a test is made on the creation date to determine the format.
C  the routine must be called before using the KINE/FKIN bank in order
C  to use the same code for old an new format.
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KIFYOL
C              External References: NAMIND/NLINK(BOS77)
C                                   KIAVER(ALEPHLIB)
C              Comdecks referenced: BCS, FKINJJ, BMACRO, KMACRO
C              Banks referenced: KINE, FKIN, PART
C
C - usage    : CALL KIFYOL
C
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fkinjj.h"
      DATA NKINE /0/
#include "bmacro.h"
#include "kmacro.h"
C ----------------------------------------------------------------------
C
C - get name-indices of KINE, FKIN and the creation date of the file
      IF (NKINE .EQ. 0) THEN
         NKINE = NAMIND ('KINE')
         NFKIN = NAMIND ('FKIN')
         NAPAR = NAMIND ('PART')
         CALL KIAVER (AVER,IPROG)
         WRITE (IW(6),'(/1X,''+++KIFYOL+++ KINGAL ALEPHLIB '',F5.2,I5)')AVER
      ENDIF
C
C - RETURN if AVER .lt. 9.0 because the format is already the old one
      IF (AVER .LT. 9.0) RETURN
C
C - change mass to energy in KINE if KINE banks exist
      JKINE = NKINE+1
 1    JKINE = IW(JKINE-1)
      IF (JKINE .NE. 0) THEN
         ITYP = KINTYP(JKINE)
         RW(KPARVK(JKINE)+4) = SQRT (RW(KPARVK(JKINE)+1)**2+
     &        RW(KPARVK(JKINE)+2)**2+RW(KPARVK(JKINE)+3)**2+
     &        RW(KPARVK(JKINE)+4)**2)
         GOTO 1
      ENDIF
C
C - change energy to mass in FKIN if FKIN bank exists
      JFKIN = IW(NFKIN)
      IF (JFKIN .NE. 0) THEN
         DO 2 N=1,LROWS(JFKIN)
            ITYP  = ITABL(JFKIN,N,JFKIPA)
            KFKIN = KROW(JFKIN,N)
            RW(KFKIN+4) = SQRT (RW(KFKIN+1)**2+RW(KFKIN+2)**2+
     &                          RW(KFKIN+3)**2+RW(KFKIN+4)**2)
  2      CONTINUE
      ENDIF
C
      END
#endif
