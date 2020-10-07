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
      IMPLICIT NONE
C ------------------------------------------------------------
C argument
      INTEGER ismod
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C functions
      INTEGER NLINK
C local variables
      INTEGER ie, j
      INTEGER  kvmbu, nvmbu
      INTEGER ierpar(6)
C ----------------------------------------------------------------
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
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
