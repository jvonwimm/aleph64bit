      SUBROUTINE BBFRDS(LUN,IACM,NRECA,NRECR,NTOT,BUF,NDIM,*,*)
*
*     FORTRAN read / skip
*
*        LUN     = unit
*        IACM    = 0 sequential = 1 direct access    = 2 special
*        NRECA   = actual record number                       updated
*        NRECR   = number of record, to be read
*        NTOT    = length of input                     -      defined
*        BUF     = buffer area for input               -       filled
*        NDIM    = length of area for input
*        *1        return 1 = read error
*        *2        return 2 = end-of-file
*
*     the arguments are input/unchanged except NRECA,NTOT,BUF(.), which
*     at return should contain the record, which is of the form
*                      NTOT,BUF(I),I=1,NTOT)
      INTEGER BUF(NDIM)

      IF(IACM.EQ.0) THEN
*        sequential - FORTRAN version
         NREC=NRECA
         IF(NREC+1.GT.NRECR) THEN
*           rewind because of reverse order
            REWIND LUN
            NREC=0
         END IF
   10    IF(NREC+1.LT.NRECR) THEN
*           read without list is skip (read error destroys counting)
            READ(LUN,END=102)
            NREC=NREC+1
            GOTO 10
         END IF
         READ(LUN,ERR=101,END=102) NTOT,(BUF(I),I=1,NTOT)
         NREC=NREC+1
         NRECA=NREC
      ELSE IF(IACM.EQ.1) THEN
*        direct access - FORTRAN version
         READ(LUN,REC=NRECR,ERR=201) NTOT,(BUF(I),I=1,NTOT)
         NREC=NRECR
         NRECA=NREC
      ELSE IF(IACM.EQ.2) THEN
*        direct access - FORTRAN version
         READ(LUN,REC=NRECR,ERR=201) NTOT,(BUF(I),I=1,NTOT)
         NREC=NRECR
         NRECA=NREC
      END IF

  100 RETURN

*     read error, skip one record

  101 READ(LUN)
      NREC=NREC+1
      NRECA=NREC
  201 RETURN 1

*     end-of-file

  102 RETURN 2
      END
