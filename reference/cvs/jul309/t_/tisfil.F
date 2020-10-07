      SUBROUTINE TISFIL(IRLST,LEN,IOFRB,ICLDR,IPLST,IORD,NISLE)
C
C--------------------------------------------------------------------
C! Fill TPC island pointers
C!
C!    Author:    R. Johnson    25-11-86
C!    Modified:  R. Johnson    18-12-86
C!
C!    Input:
C!       - LEN         /I     Number of islands + number of pointers
C!       - IOFRB       /I     Offset to row beginning in TPAD
C!       - ICLDR       /I     Pointer to island list directory
C!       - IPLST       /I     Pointer to pulse list for each island
C!       - IORD        /I     Ordering flag for each island
C!       - NISLE       /I     Number of islands
C!    Output
C!       - IRLST(ip)   /I     Row list of island pointers to TPAD hits
C!       - ICLDR       /I     These should be zero upon exit
C!       - IPLST       /I     This should be zero upon exit
C!
C!    Called by TISLND
C!
C!    Description
C!    ===========
C!    IRLST points to the beginning of a row list in the 'TISL' bank,
C!    which is of length LEN.  Using IPLST and ICLDR, the TLIPAK list
C!    routines are used to go through the linked list for each of
C!    NISLE islands (clusters of connected pad pulses), pull out the
C!    pointers to TPAD, and fill it all into IRLST.
C!----------------------------------------------------------------------
#ifndef DOC
C
      DIMENSION IRLST(*),IPLST(*),IORD(*)
C
      CHARACTER TEXT*95
C
C++   Loop over the clusters
C
      IOF=0
      IPCL=ICLDR
      DO 50 I=1,NISLE
        CALL TLNEXT(IPCL,IC)
        IF (IC.EQ.0) THEN
          TEXT= 'Cluster directory is incorrect.  This is a '//
     &          'serious program bug in the use of the list package'
          CALL REPORT('TISFIL',TEXT(1:93),1)
        ENDIF
        IPNT=IPLST(IC)
        IF (IPNT.EQ.0) THEN
          TEXT= 'Cluster list is incorrect.  This is a '//
     &          'serious program bug in the use of the list package'
          CALL REPORT('TISFIL',TEXT(1:88),1)
        ENDIF
        IOF=IOF+1
C
C++     Loop over the pulses in the cluster
C
        NPUL=0
   30   CONTINUE
          CALL TLNEXT(IPNT,IHIT)
          IF (IPNT.EQ.0) GO TO 40
          NPUL=NPUL+1
C
C++       Fill the TPAD pointer into IRLST
C
          IRLST(IOF+NPUL)=IHIT+IOFRB
          GO TO 30
   40   CONTINUE
C
C++     Fill the number of pulses into the cluster header
C
        CALL MVBITS(NPUL,0,16,IRLST(IOF),0)
C
C++     Fill the ordering flag into the cluster header
C
        CALL MVBITS(IORD(IC),0,1,IRLST(IOF),31)
C
C++     Scratch this pulse list
C
        CALL TLSCRA(IPLST(IC))
        IOF=IOF+NPUL
   50 CONTINUE
C
C++   Scratch the cluster list
C
      CALL TLSCRA(ICLDR)
      RETURN
      END
#endif
