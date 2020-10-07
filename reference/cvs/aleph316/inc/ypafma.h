C! YTOP particle masses
      PARAMETER(    JPAFEP=1,JPAFEM=2,JPAFMP=3,JPAFMM=4,
     &              JPAFPP=5,JPAFPM=6,JPAFKP=7,JPAFKM=8,
     &              JPAFPR=9,JPAFPB=10,JPAFPH=11,JPAFPZ=12,
     &              JPAFKZ=13,JPAFLA=14,JPAFLB=15   )
      COMMON/YPMASS/ YPMASS(20)
#if defined(DOC)
C   EP  1... POSITRONS    EM  2.... ELECTRONS
C   MP  3... MU+          MM  4.... MU-
C   PP  5... PION+        PM  6.... PION-
C   KP  7... KAON+        KM  8.... KAON-
C   PR  9... PROTON       PB 10.... ANTIPROTON
C
C   PH 11... PHOTONS      PZ 12.... PIZERO
C   KZ 13... KZERO        LA 14....LAMBDA
C   LB 15... LAMBDA_BAR      16....
C      17...                 18....
C      19...                 20....
C
#endif
