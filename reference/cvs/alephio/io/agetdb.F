      INTEGER FUNCTION AGETDB(LIST,IRUN)
C --------------------------------------------------------------------
CKEY ALEF BANK DA / USER
C! Load  list LIST of Bank names from the Data Base for run IRUN
C - F.Ranjard - 881209                modified - 890803
C  Input  :   LIST  = list of banks names to be accessed from data base
C                     single name or list of names
C             IRUN  = current run number
C
C  Ouput : AGETDB = > 0  if existing bank(s) are still valid
C                   = 0  if error occurs ( no valid bank found)
C                   < 0  if one or more existing banks were reloaded
C
C  The routine assumes that the data base has the logical unit #
C  JUNIDB(0) and calls ALGTDB for this unit #.
C  for details look at ALGTDB
C--------------------------------------------------------------
#ifndef DOC
      CHARACTER*(*)  LIST
      INTEGER ALGTDB
C ----------------------------------------------------------------------
C - logical unit # of the official data base
      LBASE = JUNIDB(0)
C - calls ALGTDB with this unit #
      IRET = ALGTDB (LBASE,LIST,IRUN)
C - return value is the ALGTDB return value
      AGETDB= IRET
C
      END
#endif
