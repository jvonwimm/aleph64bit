*CD hccoun
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
#if defined(DOC)
C!     HCCOUN used to accumulate statistics
       NHCC01  average number of track elements in HCAL
       NHCC02  average number of tube segments in HCAL
       NHCC03  average number of hit streamer tubes
       NHCC04  average number of hit storeys
       NHCC05  total number of events
       HCEAVE  average deposited energy
       HCANST  average number of streamers
       HCEPOR  energy deposited in one event per portion
       FHCDEB  HCAL debug flag
       FHCDB1  HCAL debug flag and ICHCJO(1)=1
       FHCDB2  HCAL debug flag and ICHCJO(1)=2
#endif
