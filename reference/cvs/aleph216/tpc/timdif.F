      REAL FUNCTION TIMDIF(KDATE1,KTIME1,KDATE2,KTIME2)
C----------------------------------------------------------------------
C Calculate time difference in minutes between two occasions.
C!  Author    :   I. Tomalin  94/06/29
C!   Input:  KDATE1 /I : Date of first occasion in format yymmdd
C!           KTIME1 /I : Time of first occasion in format hhmmss
C!           KDATE2 /I : Date of second occasion in format yymmdd
C!           KTIME2 /I : Time of second occasion in format hhmmss
C!   Output: TIMDIF  /R : Time difference in minutes.
C!   Calls: CERNLIB routine CALDAT for conversion of date to Julian
C!          date format.
C----------------------------------------------------------------------
#ifndef DOC
      DIMENSION KDATE(2),KTIME(2),JULDAT(2),TMIN(2)
C Used by CALDAT.
      CHARACTER*119 DUM
      DIMENSION IDATE(8)
C Define which century we are in.
      PARAMETER(ICENT=19)
C----------------------------------------------------------------------
C Load input numbers into arrays.
      KDATE(1) = KDATE1
      KDATE(2) = KDATE2
      KTIME(1) = KTIME1
      KTIME(2) = KTIME2
C
      DO 35 I = 1,2
C Calculate day, month and year.
        IDATE(1) = MOD(KDATE(I),100)
        IDATE(2) = MOD(KDATE(I)/100,100)
        IDATE(3) = 100*ICENT + KDATE(I)/10000
C Convert to Julian date. (Number of days since 1/1/1).
        CALL CALDAT(101,DUM,IDATE,IER)
        IF (IER.NE.0) THEN
          WRITE(6,25) IER,KDATE(I)
   25     FORMAT(' FATAL ERROR IN TIMDIF: ',2I10)
           CALL EXIT
        END IF
        JULDAT(I) = IDATE(6)
C Calculate hours, minutes and seconds.
        HOUR = FLOAT(KTIME(I)/10000)
        XMIN = FLOAT(MOD(KTIME(I),10000)/100)
        SEC  = FLOAT(MOD(KTIME(I),100))
C Convert to number of minutes since beginning of day.
        TMIN(I) = 60.0*HOUR + XMIN + SEC/60.0
   35 CONTINUE
C
C Calculate time difference in minutes between the two occasions.
      TIMDIF = 24.0*60.0*(JULDAT(2) - JULDAT(1)) +
     +                   (TMIN(2) - TMIN(1))
      END
#endif
