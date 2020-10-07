C!  E-Flow common
       COMMON / PVECTR / PJETA(11,20)
#if defined(DOC)
C      PJETA(1 ,J )   sum Ex
C      PJETA(2 ,J )   sum Ey
C      PJETA(3 ,J )   sum Ez
C      PJETA(4 ,J )   sum E
C      PJETA(5 ,J )   sum Ex ** 2
C      PJETA(6 ,J )   sum Ey ** 2
C      PJETA(7 ,J )   sum Ez ** 2
C      PJETA(8 ,J )   sum E  ** 2
C      PJETA(9 ,J )   sum Ex * Ey
C      PJETA(10,J )   sum Ex * Ez
C      PJETA(11,J )   sum Ey * Ez
C      PJETA(I ,1 )   sum Tracks
C      PJETA(I ,2 )   sum Ecal 1+2 inside mask
C      PJETA(I ,3 )   sum Ecal 3   inside mask
C      PJETA(I ,4 )   sum Hcal 1+2 inside mask
C      PJETA(I ,5 )   sum Ecal 1+2 total
C      PJETA(I ,6 )   sum Ecal 3   total
C      PJETA(I ,7 )   sum Hcal 1+2 total
C      PJETA(I ,8 )   sum Lcal 1+2 total
C      PJETA(I ,9 )   sum Lcal 3   total
C      PJETA(I ,10)   calculated muon energy in Ecal 1+2
C      PJETA(I ,11)   calculated muon energy in Ecal 3
C      PJETA(I ,12)   calculated muon energy in Hcal 1+2
C      PJETA(I ,13->17) not filled
C      PJETA(I ,18)   fitted energy using masks
C      PJETA(I ,19)   fitted energy using calorimeters
#endif
