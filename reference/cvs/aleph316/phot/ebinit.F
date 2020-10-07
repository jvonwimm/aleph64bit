      SUBROUTINE EBINIT (IRET)
C ----------------------------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C!  Banks initialisation.
CKEY PHOTONS GAMMA INITIALISATION / USER
C
C  OUTPUT : IRET / I  return code
C                     (=0 means OK, =1 means D.B. missing banks)
C
C   BANKS :
C     INPUT   : EGST
C               EGVP
C               EGPA
C               EGMD
C               EGTH
C               ECNS
C               ECLK
C     OUTPUT  : NONE
C
C   Calls  NAMIND , JUNIDB , MDARD
C   Called by USER.
C-----------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
      EXTERNAL NAMIND , JUNIDB , MDARD
C   Read Data Base.
      LUN = JUNIDB (0)
      KEGST = IW(NAMIND( 'EGST' ) )
      IF( KEGST .EQ. 0 ) THEN
         IND = MDARD( IW , LUN , 'EGST' , 0 )
         IF (IND .EQ. 0) GOTO 998
      ENDIF
      KEGVP = IW(NAMIND( 'EGVP' ) )
      IF( KEGVP .EQ. 0 ) THEN
         IND = MDARD( IW , LUN , 'EGVP' , 0 )
         IF (IND .EQ. 0) GOTO 998
      ENDIF
      KEGPA = IW(NAMIND( 'EGPA' ) )
      IF( KEGPA .EQ. 0 ) THEN
         IND = MDARD (IW,LUN,'EGPA',0)
         IF (IND.EQ.0) GOTO 998
      ENDIF
      KEGMD = IW(NAMIND( 'EGMD' ) )
      IF( KEGMD .EQ. 0 ) THEN
         IND = MDARD (IW,LUN,'EGMD',0)
         IF (IND.EQ.0) GOTO 998
      ENDIF
      KEGTH = IW(NAMIND( 'EGTH' ) )
      IF( KEGTH .EQ. 0 ) THEN
         IND = MDARD (IW,LUN,'EGTH',0)
         IF (IND.EQ.0) GOTO 998
      ENDIF
      KECNS = IW(NAMIND( 'ECNS' ) )
      IF( KECNS .EQ. 0 ) THEN
         IND = MDARD (IW,LUN,'ECNS',1)
         IF (IND.EQ.0) GOTO 998
      ENDIF
      KECLK = IW(NAMIND( 'ECLK' ) )
      IF( KECLK .EQ. 0 ) THEN
         IND = MDARD (IW,LUN,'ECLK',0)
         IF (IND.EQ.0) GOTO 998
      ENDIF
      IRET = 0
      RETURN
C
C - data base bank is missing
C
 998  IRET = 1
      END
#endif
