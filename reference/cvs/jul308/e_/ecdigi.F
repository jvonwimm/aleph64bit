      SUBROUTINE ECDIGI(XXX,IFLAG,ISTAD,IROSE)
C
C***********************************************************************
C!FIND STOREY WHERE POINT IS                                           *
C!                                                                     *
C!                                                                     *
C!  AUTHOR   : J.P. ALBANESE 860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!    ECDIGI                                                           *
C!    INPUT  : XXX(3): POSITION OF TRACK POINT                         *
C!                                                                     *
C!    OUTPUT : IFLAG :                                                 *
C!                    2  IF IN ECAL                                    *
C!                    #2 IF OUT OF ECAL                                *
C!             ISTAD                                                   *
C!                                                                     *
C! -FIND GIVEN X,Y,Z IN ABSOLUTE COORDINATES,THE IJK STOREY_ADDRES     *
C!                                                                     *
C!  TRANSFORM IT INTO ENCODED ADDRESS                                  *
C!         BY CALLING THE NORMAL ENCODING ROUTINE                      *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
      CHARACTER*120 MESAG
C
      REAL XXX(*)
C?
C?  FIND STOREY IJK
C?
      CALL EX3IJK(XXX,I,J,K,NTA,IFLAG,KSC,MODUL)
          IF ( K .EQ. 0 .AND. IFLAG. EQ. 2) THEN
      WRITE(MESAG,900) (XXX(J),J=1,3),I,J
 900  FORMAT(3E13.5,2(1X,I4))
      CALL RERROR('ECDIGI',1,'Error;K.eq.0 and iflag.eq.2; XXX,I,J ->')
      CALL RERROR('ECDIGI',2,MESAG)
            IFLAG = 0
          ENDIF
      IF(IFLAG.EQ.2)THEN
C?
C?     IF INSIDE OF ECAL, GET ENCODED ADDRESS
C?
         CALL ECINIT(I,J,K,ISTAD,IROSE)
      ELSE
C?
C?     ELSE, EXIT WITH ADDRESS 0
C?
         ISTAD=0
      ENDIF
      RETURN
      END
#endif
