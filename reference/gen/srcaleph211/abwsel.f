      SUBROUTINE ABWSEL (LIST)
C --------------------------------------------------------------
CKEY ALPHARD WRITE
C! set write-event flag
C - F.Ranjard - 930720      from H.Albrecht
C - Input : LIST / A1 = one character output list
C           Recommended : 'E' , 'R', 'C'. Forbidden : ' ', 'Z'.
C           This parameter is ignored if the output is an event
C           directory which refers to an input file.
C
C   ABWSEL sets an output flag. Actual output is done during the next
C   call to ABRREC. Thus, there is no danger to call it more than once
C   for one event. For the very last event, ABWEND (see below) has
C   to be called.
C
C ----------------------------------------------------------------
      CHARACTER*1 LIST
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /ABRCOM/ BATCH,INIT,CLOSE1,CLOSE2,FWFILM
     &               ,IUNDAT(2),IUTDAT(5),IUNSEL,IUNSE2,IUTSEL
     &               ,MASKA,MASKW
     &               ,WLIST,TLIST
      LOGICAL BATCH, INIT, CLOSE1, CLOSE2, FWFILM
      CHARACTER*1   TLIST, WLIST
C
      INTEGER IBITC(30)
      DATA    IBITC/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     +        8192,16384,32768,65536,131072,262144,524288,1048576,
     +        2097152,4194304,8388608,16777216,33554432,67108864,
     +        134217728,268435456,536870912/
C --------------------------------------------------------------
      IF (WLIST.NE.' ' .AND. WLIST.NE.LIST) CALL ABWEVE
      WLIST = LIST
C
      END
