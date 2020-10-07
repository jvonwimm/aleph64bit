      SUBROUTINE VBNKDP(ismod)
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / USER
C!       Dump unpacked bonding error banks
C
C - Joe Rothberg, August 1995
C
C
C Input:
C  ISMOD  /I module serial number
C--------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C ------------------------------------------------------------
C argument
      INTEGER ismod
C
#include "bcs.h"
#include "bmacrod.h"
C functions
      INTEGER NLINK
C local variables
      INTEGER ie, j
      INTEGER  kvmbu, nvmbu
      INTEGER ierpar(6)
C ----------------------------------------------------------------
#include "bmacro.h"
C ----------------------------------------------------------
C
C Bank VMBU, unpacked errors
C
        kvmbu  = 0
        kvmbu = NLINK('VMBU',ismod)
        IF(kvmbu .EQ. 0) THEN
          GOTO 999
        ENDIF
C
       nvmbu = LROWS(kvmbu)
C
       WRITE(6,'(/1X,A,I6,4X,A,I4)')
     >   ' Bank VMBU, Module ',ismod,' rows= ',nvmbu
       WRITE(6,'(6X,A)')'  View   Addr1  Addr2   Bond  Fault  Param '
C

       IF(nvmbu .EQ. 0) THEN
         WRITE(6,*) ' VMBU ',ismod,' zero rows '
         GOTO 999
       ENDIF
C
       DO ie = 1, nvmbu
         DO j = 1,6
            ierpar(j) = ITABL(kvmbu,ie,j)
         ENDDO
         WRITE(6,'(1X,I4,6I7)')ie,ierpar
       ENDDO
C
 999   CONTINUE
C
       RETURN
       END
#endif
