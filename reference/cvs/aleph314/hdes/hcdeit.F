             LOGICAL FUNCTION HCDEIT(ITUB,ILAY,IMOD,IPOR)
C------------------------------------------------------------------------
C
CKEY HCALDES HCAL STATUS TUBE / USER
C!   If .TRUE. the tube is dead
C!
C!                                     Author:G.Catanesi 01/08/89
C!
C!    INPUT:
C!           ITUB/I  :Tube #
C!           ILAY/I  :Layer #
C!           IMOD/I  :Module #
C!           IPOR/I  :Portion #
C!
C------------------------------------------------------------------------
C
#ifndef DOC
#include "hcgeom.h"
#include "hcdetu.h"
C
               HCDEIT = .FALSE.
C
               IF(IPOR.EQ.LPBAR)THEN
C
C    Barrel Case
                  IF(MH8FDB(IMOD,ILAY).EQ.0)RETURN
C
C   Find the eightfold #
                  CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
C   Check the corresponding bit in the look_up table
                  IF(IHEIF.NE.0)THEN
                     HCDEIT = BTEST(MH8FDB(IMOD,ILAY),IHEIF)
                  ENDIF
               ELSE
C
C   EndCap Case
                  IF(IPOR.EQ.LPECB)IMOD = IMOD +LPHCES
C   Find the eightfold #
                  CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
                  IF(IHEIF.NE.0)THEN
                     IDTU = IHEIF/IDHEI
                     IF(MH8FDE(IMOD,ILAY,IDTU).EQ.0)RETURN
C
C   Check the corresponding bit in the look_up table
                     HCDEIT = BTEST(MH8FDE(IMOD,ILAY,IDTU),IDHEI)
                  ENDIF
               ENDIF
C
               RETURN
               END
#endif
