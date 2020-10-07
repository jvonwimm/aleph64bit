      SUBROUTINE EGTTKS(NTRAK,IER)
C
C***********************************************************************
C!GET ACCESS TO TRACKS                                                 *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : ETP1                                                   *
C!    OUTPUT  : EFET                                                   *
C!    CREATED : EFET                                                   *
C!                                                                     *
C!     SUBROUTINE EGTTKS (GET TRACKS)                                  *
C!     LINK TO BOS BANK ETP1 (LAST TPC POINT)                          *
C!     GET NUMBER OF TRACKS                                            *
C!     GET NUMBER OF WORDS PER TRACK                                   *
C!     CREATE BANK EFET FOR ECAL EXIT POINTS OF EXTRAPOLATED TRKS      *
C!                                                                     *
C!                                                                     *
C!       IER = return code ( 0 if normal)                              *
C!                                                                     *
C***********************************************************************
C
#ifndef DOC
#include "bcs.h"
#include "ecnamc.h"
#include "etp1jj.h"
#include "rlunit.h"
#include "bmacro.h"
      IER=0
      KETP1=IW(NAETP1)
      IF(KETP1.EQ.0)GO TO 997
      NTRAK=LROWS(KETP1)
C?
C?    PREPARE BANK FOR EXIT POINTS OF EXTRAPOLATED TRACKS
C?
C?   CREATE BANK AND PUT IT IN LIST E
C?
      LBKL=IW(KETP1)
      CALL AUBOS('EFET',0,LBKL,KEFET,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      CALL BLIST(IW,'E+','EFET')
C?
C?   GET INDEX, JUST IN CASE AUBOS MAKES A GARBAGE COLLECTION
C?
      KETP1=IW(NAETP1)
C?
C?  COPY BANK CONTENT
C?
      DO 1 I=1,LBKL
    1 IW(KEFET+I)=IW(KETP1+I)
      RETURN
C
C  CALL EXIT WILL BE REPLACED BY STANDARD TREATMENT OF ERRORS
C   TO COME LATER
C
  997 CALL RERROR('EGTTKS',1,' ETP1 BANK DOES NOT EXIST' )
      IER=1
      RETURN
  998 CALL RERROR('EGTTKS',2,' exit')
      IER=1
      RETURN
      END
#endif
