      SUBROUTINE ECALSL(CLAS1,CLAS2,CLAS3)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS1-->3
C! Classes 1,2 and 3 code.
C-
C   Input  : None
C   Output : CLAS1  = Class 1 logical flag
C            CLAS2  = Class 2 logical flag
C            CLAS3  = Class 3 logical flag
C-
C   Called by   : SELEVT
C   Calls  : ECALWR,HCALEN
C   Input banks : None
C-
C                                            Author: M. Talby March 91
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
      LOGICAL CLAS1,CLAS2,CLAS3
C --
      CLAS1 = .FALSE.
      CLAS2 = .FALSE.
      CLAS3 = .FALSE.
C --
C   Get Ecal wire energy and Hcal pads energy
C --
      CALL ECALWR(IMODUL,ECAPEA,ECALBE,ECAPEB)
      CALL HCALEN(EHCAL)
      ECALW = ECAPEA+ECALBE+ECAPEB
      ECALT = ECALW+EHCAL
      IF(IMODUL.GT.2) CLAS1 = .TRUE.
      IF(ECALT.GT.15.) CLAS2 = .TRUE.
      IF(ECAPEA.GT.2. .AND. ECAPEB.GT.2. .OR. ECALBE.GT.6.)
     &  CLAS3 = .TRUE.
C --
       RETURN
       END
#endif
