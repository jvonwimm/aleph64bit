      REAL FUNCTION EVOLUM(VOLNAM,SC)
C.----------------------------------------------------------------------
CKEY ECALDES VOLUME / USER
C     H.Videau      creation 13/02/88
C! Computes ECAL volumes volume
C  Computes the volume of an ECAL volume given by its name VOLNAM,
C  and its subcomponent SC.
C   Input :
C           VOLNAM  name                  ch16
C           SC      subcomponent number   integer
C   Output:
C           EVOLUM
C   Calls: EVOLFC, AVOLUM
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
C
C  Input variables
      INTEGER SC
      CHARACTER*(*) VOLNAM
C  Locales
      INTEGER LEFACE,LEPOIN,PTRFCE(12)
      REAL PLANES(4,12),XFACE(50),YFACE(50),ZFACE(50)
C  External
      REAL AVOLUM
      EXTERNAL AVOLUM
C
      LEFACE=0
      CALL EVOLPL(VOLNAM,SC,0,LEFACE,PLANES)
      IF(LEFACE.EQ.0) GO TO 999
      CALL EVOLFC(VOLNAM,LEFACE,PLANES,PTRFCE,LEPOIN,XFACE,YFACE,ZFACE)
      EVOLUM=AVOLUM(LEFACE,PLANES,PTRFCE,
     &            LEPOIN,XFACE,YFACE,ZFACE)
 999  CONTINUE
      END
#endif
