      SUBROUTINE BKCARI(INTN,NDIG,COUT)
C-----------------------------------------------------------------------
C! Converts an integer into character string
CKEY BOOK CHARACTER INTEGER / USER
C  J. Boucrot       28-Sep-1988    modified 12-Jan-1989
C  Converts an integer with the restriction IABS(INTN) < 2**32-1),
C    into the corresponding character  string COUT of NDIG characters
C    IF NDIG > nb of digits of the integer , COUT is filled with '0'
C    in front of the meaningful digits :
C        e.g. INTN = 89     NDIG = 5  --->  COUT = '00089'
C-----------------------------------------------------------------------
#ifndef DOC
       CHARACTER*1 CHAR
       CHARACTER*(*) COUT
       PARAMETER ( NDMAX=10, INMAX = 1286608615  )
C-----------------------------------------------------------------------
C IOASC is the code for character representation of 0 in the machine :
       IOASC=ICHAR('0')
C
       IK=0
       COUT=' '
       IF (NDIG.LE.0.OR.NDIG.GT.NDMAX) NDIG=NDMAX
C Is there a minus sign in front  ?
       IF (INTN.LT.0) THEN
          IK=1
          COUT(IK:IK)='-'
          ISTN=-INTN
       ELSE
          ISTN=INTN
       ENDIF
C Padding of COUT with '0' by default :
       NDOU=NDIG
       IF (IK.EQ.1.) NDOU=NDIG+1
       DO 1 IDI=IK+1,NDOU
 1     COUT(IDI:IDI)='0'
C Check for wrong inputs :
       IF (ISTN.GT.INMAX) GO TO 900
       IF (NDIG.LT.NDMAX) THEN
          INNMX=10**(NDIG+1)-1
          IF (ISTN.GT.INNMX) GO TO 900
       ENDIF
C Fill COUT :
C
       DO 10 IC=NDIG,1,-1
          IK=IK+1
          IDIG=ISTN/(10**(IC-1))
          IASC=IDIG+IOASC
          COUT(IK:IK)=CHAR(IASC)
          ISTN=ISTN-IDIG*10**(IC-1)
 10    CONTINUE
C
 900   LE=MAX0(LNBLNK(COUT),NDOU)
       COUT=COUT(1:LE)
C
 999   RETURN
       END
#endif
