C! JOB-PARAMETERS
      COMMON /RJOBCO/ATIMRJ,FINARJ
      LOGICAL FINARJ
      REAL ATIMRJ
#if defined(DOC)
C ATIMRJ  = time remaining at forced termination of program.
C FINARJ  = set .true. if Read in BOS native format (user request)
C
#endif
