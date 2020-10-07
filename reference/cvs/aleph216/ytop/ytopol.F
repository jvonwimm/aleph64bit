      SUBROUTINE YTOPOL
C
C----------------------------------------------------------*
C!    Steering routine for TOPOLOGY reconstruction
CKEY YTOP MAIN
C!    Author :     M. Bosman     20/09/91
C!    Modified  :  G. Lutz   30/03/92
C!    Modified  :  G. Lutz    1/10/92
C!
C!    Description
C!    ===========
C!    This routine calls the new version of
C!    the steering routine of the YTOP package
C!
C!---------------------------------------------------------*
#ifndef DOC
      CALL YTOPNW
      RETURN
      END
#endif
