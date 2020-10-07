      SUBROUTINE BKRHAW(PRNAM,IPVER,ICVER,NATOU,IFAIL)
C-----------------------------------------------------------------------
C! Fills the RHAH bank ( Run Header Analysis History )
CKEY BOOK RUN HEADER / INTERNAL
C  Called at each beginning of run , in JULIA or ALPHA  ,
C  for each output stream
C  Calls BKRHAB,BKRHAL                              from ALEPHLIB
C  Calls BKFMT                                      from BOS77
C Author     J. Boucrot  25-Sep-1988  , modified 02-Feb-1989
C Input arguments :
C   PRNAM = Name of the calling program : JULIA , ALPHA
C   IPVER = Version number of the calling program
C   ICVER = Correction set version number
C   NATOU = Code for the nature of what is being written :
C              3 = POT           4 = DST           5 = MiniDST
C              6 = MicroDST      7 = NanoDST
C Output argument :
C   IFAIL = 0 if all OK
C         = 1 if RHAH bank could not be booked / enlarged
C         = 2 if no RUNH bank , or wrong input arguments
C Input bank needed :  RUNH
C Modified: P. Comas  8-JUN-1994, change RHAH format so that last word
C                                 is character to store the computer typ
C ----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
      COMMON / NAMRHA / NARHAH,NARHOH,NARUNH,NAAJOB,
     +                  NAKJOB,NAKRUN
      EXTERNAL NAMIND
      CHARACTER LIST*1,LISTA*2,LISTW*2,PRNAM*8
      CHARACTER*4 NAMD(5)
      PARAMETER  ( IPOT = 3 , INANO = 7 , ISEL = 10 )
      LOGICAL FIRST
      DATA NAMD / 'POT ' , 'DST ' , 'MINI' , 'MICR' , 'NANO' /
      DATA FIRST / .TRUE. /
      DATA  IOLR  / -1  /
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
C-----------------------------------------------------------------------
C Define 'RHAH' format , and Namindices on first call :
        IF (FIRST) THEN
           FIRST=.FALSE.
           CALL BKFMT('RHAH','2I,(2A,9I,A)')
           NARHAH=NAMIND('RHAH')
           NARHOH=NAMIND('RHOH')
           NARUNH=NAMIND('RUNH')
           NAAJOB=NAMIND('AJOB')
           NAKJOB=NAMIND('KJOB')
           NAKRUN=NAMIND('KRUN')
        ENDIF
C Check input parameters and bank :
        IFAIL=2
        INDN=MOD(NATOU,ISEL)
        IF (INDN.LT.IPOT.OR.INDN.GT.INANO) GO TO 999
C Use 'RUNR' if 'RUNH' absent :
        JRUNH=IW(NARUNH)
        IF (JRUNH.GT.0) THEN
           NRUN=IW(JRUNH+JRUNRN)
        ELSE
           JRUNR=IW(NAMIND('RUNR'))
           IF (JRUNR.EQ.0) GO TO 999
           NRUN=IW(JRUNR+2)
        ENDIF
        INDX=INDN-IPOT+1
        IFAIL=1
C
C When JULIA reads a RAW data tape or a GALEPH output , the RHAH bank
C doesn't exist. It must be created on first call of this routine
C for the current output stream , and filled with the information
C concerning previous steps .
C When this routine is called from ALPHA , RHAH may not exist if one
C reads e.g. a SIMDST or a KINGAL output .
C
        JRHAH=IW(NARHAH)
        IF (JRHAH.EQ.0) THEN
           CALL BKRHAB(JRHAH)
           IF (JRHAH.EQ.0) GO TO 999
C Check if this is the first call for this run ; if not , the bank RHAH
C must be restored to its status on first call :
        ELSE
           LSROW=LROWS(JRHAH)
           IF (NRUN.EQ.IOLR) THEN
              IW(JRHAH+LMHROW)=LSROW-1
              CALL AUBPRS('RHAH')
           ENDIF
        ENDIF
        IOLR=NRUN
C
C Enlarge the bank 'RHAH' with the new row corresponding to the
C characteristics of the output stream currently being written :
C
        CALL BKRHAL(PRNAM,IPVER,ICVER,NATOU,IFAIL)
        IF (IFAIL.NE.0) GO TO 999
C
        IFAIL=0
 999    RETURN
        END
