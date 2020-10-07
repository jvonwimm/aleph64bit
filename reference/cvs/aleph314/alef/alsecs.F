      INTEGER FUNCTION ALSECS(DATE,TIME)
C ----------------------------------------------------------------------------
CKEY ALEF DATE TIME
C
C     O. Schneider, CERN/PPE
C     June 11, 1996
C     Modified 27/02/97 (M.Cattaneo): Remove test on century for leap year
C                                     (year 2000 is a leap year)
C
C! Converts an ALEPH date/time into a unique integer 
C  which is a linear function of time.
C
C     The ALEPH date/time is stored for examples in banks RUNH, EVEH, LXDA ...
C
C     Input:   DATE = integer date (YYMMDD or YYYYMMDD)
C     ------   TIME = integer time (HHMMSS)
C                     WARNING: in bank EVEH the time has the format HHMMSSXX
C                              instead of HHMMSS
C
C     Function value: Number of seconds elapsed between Jan 1, 1988 at 0:00:00 
C     --------------- and the specified DATE and TIME
C
C                      A negative function value indicates an error, i.e. 
C                      an invalid date or time, or date in a year before 1988 
C                      or after 2053.
C
C     Examples:  ALSECS(  880101,000000) returns 0
C     ---------  ALSECS(  880101,000017) returns 17
C                ALSECS(19880101,000017) returns 17
C                ALSECS(  960724,140213) returns 270223333
C                ALSECS(19960724,140213) returns 270223333
C                ALSECS(  960724,140313) returns 270223393
C                ALSECS(  531231,235959) returns 2082758399
C                ALSECS(20531231,235959) returns 2082758399
C                ALSECS(  960814,235959) returns 272073599
C                ALSECS(  960814,240000) returns -8 (error)
C                ALSECS(  960815,000000) returns 272073600
C                ALSECS(  960229,000000) returns 257558400
C                ALSECS(  970229,000000) returns -4 (error)
C                ALSECS(      -1,    -1) returns -38 (error)
C
C ----------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER DATE,TIME
C
C     Local variables
C
      INTEGER FIRSTY
      PARAMETER(FIRSTY=1988)
      INTEGER I,YEAR,MONTH,DAY,HOURS,MINS,SECS,ILEAP,DAYS
      INTEGER DAYMONTH(12,0:1)
      DATA DAYMONTH/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 
     &              31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
C                   JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
C
C     Statement function
C
      LOGICAL LEAP
      LEAP(I)=MOD(I,4).EQ.0
C Year 2000 is a leap year!
C      LEAP(I)=MOD(I,4).EQ.0.AND.MOD(I,100).NE.0
C ----------------------------------------------------------------------------
C
C     Decode input arguments
C
      YEAR =DATE/10000
      MONTH=MOD(DATE,10000)/100
      DAY  =MOD(DATE,100)
      HOURS=TIME/10000      
      MINS =MOD(TIME,10000)/100      
      SECS =MOD(TIME,100)
C
C     Determine full year
C
      IF(YEAR.LT.100) THEN 
        YEAR=MOD(YEAR,100)+1900
        IF(YEAR.LT.FIRSTY) YEAR=YEAR+100
      ENDIF
      IF(LEAP(YEAR)) THEN
        ILEAP=1
      ELSE
        ILEAP=0
      ENDIF
C
C     Check input arguments
C
      ALSECS=0
      IF(YEAR.LT.FIRSTY.OR.YEAR.GT.FIRSTY+65)      ALSECS=ALSECS-01
      IF(MONTH.LT.1.OR.MONTH.GT.12)                ALSECS=ALSECS-02
      MONTH=MIN0(MAX0(MONTH,1),12)
      IF(DAY.LT.1.OR.DAY.GT.DAYMONTH(MONTH,ILEAP)) ALSECS=ALSECS-04
      IF(HOURS.LT.0.OR.HOURS.GE.24)                ALSECS=ALSECS-08
      IF(MINS.LT.0.OR.MINS.GE.60)                  ALSECS=ALSECS-16
      IF(SECS.LT.0.OR.SECS.GE.60)                  ALSECS=ALSECS-32
      IF(ALSECS.NE.0) RETURN
C
C     Determine number of days elapsed since Jan 1st of year FIRSTY
C
      DAYS=DAY-1
      DO I=1,MONTH-1
        DAYS=DAYS+DAYMONTH(I,ILEAP)
      ENDDO
      DO I=FIRSTY,YEAR-1
        DAYS=DAYS+365
        IF(LEAP(I)) DAYS=DAYS+1
      ENDDO
C
C     Determine number of seconds elapsed since Jan 1st of year FIRSTY
C
      ALSECS=SECS+60*MINS+3600*HOURS+86400*DAYS
      END
#endif
