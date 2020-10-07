C
C.....Number of layers
      DATA NIVMAX/3/
C
C.....NN architecture : NEURON
      DATA(NEURON(I),I=1,3)/17,15,1/          
C
C
C.....minima and maxima of inputs : XMIN2, XMAX2
      DATA (XMIN2(I),I=1,17)/
     & -.17878802E+01, -.13035367E+01, -.15114757E+01, -.66058314E+00,
     & -.21228276E+00, -.28886290E+01, -.19590091E+01, -.21126399E+01, 
     & -.23632243E+01, -.25632112E+01, -.23192554E+01, -.24127309E+01,
     & -.21746206E+01, -.11999340E+01, -.85097092E+00, -.10349016E+01, 
     & -.16418309E+01/      
       DATA (XMAX2(I),I=1,17)/
     &  .39265420E+01,  .57588797E+01,  .45660772E+01,  .37444193E+01,
     &  .55548695E+02,  .32459576E+01,  .29207892E+01,  .26964285E+01,
     &  .28329160E+01,  .55544009E+01,  .43118777E+01,  .46079202E+01,
     &  .22331281E+01,  .36614766E+01,  .53453350E+01,  .30199020E+01,
     &  .15124191E+01/
C
C
C.....Mean values of 17 Inputs
      DATA (SOMOY2(I),I=1,17)/
     &  .36850685E+00,  .12209305E+00,  .23178412E+00,  .59459949E+01,
     &  .19631374E+02,  .36523960E+02,  .35859612E+02,  .42498463E+02,
     &  .51826061E+02,  .15209167E+02,  .12541833E+02,  .59103382E+02,
     &  .47061849E+00,  .20921126E+00, -.72639418E+00, -.49243331E+00,
     &  .37966181E-01/
C
C
C.....Standard deviations of 17 Inputs
      DATA (SOM2(I),I=1,17)/
     &  .38255524E-01,  .87173199E-02,  .22798900E-01,  .81020340E+02,
     &  .83816855E+04,  .14442581E+03,  .27425766E+03,  .31461407E+03,
     &  .38276074E+03,  .30730413E+02,  .24765804E+02,  .12581903E+03,
     &  .42714518E-01,  .30393194E-01,  .10335371E+00,  .24048354E+00,
     &  .39956185E+00/ 
C
C
C......Weights : WPOIDS
C      NN weights for : Input layer --> Hidden layer
       DATA ((WPOIDS(1,I,J),J=1,15),I=1,18)/
C      Weights between Input 1 and Hidden layer
     &  .74199E+00,-.98955E+01, .30008E+01,-.20583E+01,-.57630E+00,
     & -.11353E+01,-.16902E+01, .81922E+01, .53889E+00, .65878E+01,
     & -.11037E+01, .30608E-01, .24368E+01, .46402E+01, .21448E+01,
C      Weights between Input 2 and Hidden layer 
     &  .86540E+00,-.98725E+00, .10556E+02, .58671E+01,-.62248E+01,
     &  .18809E+01,-.38898E+01,-.11853E+01, .61751E+00, .14155E+02,
     & -.13767E+01, .25457E-01, .57288E+01, .56733E+01, .19620E+01, 
C      Weights between Input 3 and Hidden layer 
     &  .17008E+01,-.13128E+02, .10038E+02,-.10698E+00,-.35606E+01, 
     &  .63900E+01,-.38023E+01,-.30545E+01, .12081E+01, .79051E+00,
     & -.25438E+01, .23152E-01, .49405E+01, .65872E+01, .42784E+01,
C      Weights between Input 4 and Hidden layer
     & -.17297E+00,-.30835E+00, .38291E+01,-.93205E+00,-.44797E+01,
     & -.25096E+01, .22096E+00,-.28933E+01,-.12903E+00, .70138E+00,
     &  .21316E+00, .15845E-03,-.17554E+01, .10324E+01,-.66251E+00,
C      Weights between Input 5 and Hidden layer
     & -.87239E+00, .41478E+01,-.53250E+01, .56107E+00, .18196E+01, 
     &  .44534E+01, .14185E+01, .24914E+01,-.59659E+00,-.35550E+01,
     &  .13487E+01,-.81723E-01,-.36879E+01,-.24059E+01,-.19429E+01,
C      Weights between Input 6 and Hidden layer
     & -.36749E+00, .46605E+01, .26889E+01,-.12504E+01, .20578E+01,
     & -.85403E+00, .58667E+00, .39840E+01,-.28501E+00,-.20838E+01,
     &  .41757E+00,-.17336E-01, .40713E+01,-.48631E+01,-.48596E+00,
C      Weights between Input 7 and Hidden layer
     & -.63662E+00, .64321E+01,-.24162E+00,-.12142E+02, .74486E+00,
     & -.62296E+01, .31958E+01,-.12950E+02,-.44434E+00, .26298E+01,
     &  .95109E+00, .36311E-03,-.16144E+01,-.91771E+01,-.19126E+01,
C      Weights between Input 8 and Hidden layer
     & -.55248E+00, .75524E+00,-.85446E+00,-.41035E+01, .21426E+01,
     & -.13483E+02, .20432E+01,-.67454E+01,-.38565E+00,-.77063E+01,
     &  .10348E+01, .99298E-02,-.28892E+01,-.27059E+01,-.26482E+01,
C      Weights between Input 9 and Hidden layer
     & -.80208E+00, .12468E+02, .40483E+01, .21865E+01,-.19380E-01,
     & -.10731E+02, .15623E+01,-.61102E+01,-.61666E+00,-.15895E+01,
     &  .12348E+01,-.30557E-02,-.57779E+01,-.58024E+01,-.26149E+01,
C      Weights between Input 10 and Hidden layer
     & -.37295E+00,-.29719E+01, .25702E+01, .19893E+01, .17823E+01,
     & -.14674E+01, .28257E+00,-.19455E+01,-.29861E+00,-.31825E+01,
     &  .38399E+00,-.12712E-01, .66273E+00, .11190E+02,-.73613E+00,
C      Weights between Input 11 and Hidden layer
     &  .10337E-01, .19727E+01, .38472E+01, .50442E+01, .31529E+01,
     & -.24117E+01,-.40652E-01, .63114E+01, .60378E-02,-.17397E+01,
     &  .19872E-02, .21616E-02, .37831E-01, .18711E+01, .46479E+00,
C      Weights between Input 12 and Hidden layer
     & -.29012E-01,-.34797E+01, .11450E+01, .55249E+01, .35649E+01,
     & -.31497E+01, .11683E+01, .61526E+01, .34026E-01,-.20983E+01,
     &  .26128E-01,-.13060E-01,-.56643E+01,-.14444E+01,-.63819E+00,
C      Weights between Input 13 and Hidden layer
     & -.56566E+00, .64802E+00,-.29604E+01,-.67427E-02, .39468E+01,
     & -.46739E+01, .24447E+01, .12992E+01,-.39752E+00,-.78204E+01,
     &  .98036E+00, .22361E-01,-.44904E+01,-.77109E+01,-.16987E+01,
C      Weights between Input 14 and Hidden layer
     & -.43428E-01, .45276E+01, .19948E+01, .48743E+01,-.31750E+01,
     & -.62720E+01, .14124E+00,-.43766E+01,-.54843E-01,-.27474E+00,
     &  .11022E+00,-.75075E-02, .47014E+01,-.32370E+00,-.47357E+00,
C      Weights between Input 15 and Hidden layer
     &  .46049E+00, .18792E+01,-.45167E-01,-.13781E+02, .38789E+01, 
     &  .11264E+02, .18766E+01, .62580E+00, .32704E+00,-.83107E+01,
     & -.75651E+00,-.11956E-01,-.36158E+00,-.76282E+01, .11803E+01,
C      Weights between Input 16 and Hidden layer
     &  .19660E+00,-.31799E+01,-.12412E+02, .57437E+01,-.20244E+01, 
     &  .18213E+01,-.17687E+01,-.45484E+01, .12732E+00, .83538E+01,
     & -.32610E+00,-.10867E-01,-.22443E+01, .33517E+01, .82710E+00,
C      Weights between Input 17 and Hidden layer
     &  .12138E-01, .27494E+01,-.62786E+00, .81127E+01,-.46744E+01, 
     &  .42433E+01,-.14803E+01, .51507E+01,-.30493E-01, .20859E+01,
     & -.12520E+00,-.20757E-01, .98611E+00,-.17594E-01, .15695E+00,
C      Weights between threshold Input and Hidden layer
     &  .10774E+01,-.53768E+01, .51731E+01, .17110E+01,-.27104E+01, 
     &  .16321E+01,-.18195E+01,-.18637E+01, .78413E+00, .31518E+01,
     & -.17260E+01,-.58562E-01, .49083E+01, .30612E+01, .25541E+01/ 
C
C
C......NN weights for : Hidden layer --> Output layer
       DATA (WPOIDS(2,I,1),I=1,16)/
C      Weight between Hidden neuron 1 and Output layer
     &  -.10775E+00,
C      Weight between Hidden neuron 2 and Output layer
     &   .10323E+01,
C      Weight between Hidden neuron 3 and Output layer
     &  -.92766E+00,
C      Weight between Hidden neuron 4 and Output layer
     &  -.13255E+01,
C      Weight between Hidden neuron 5 and Output layer
     &   .33544E+00,
C      Weight between Hidden neuron 6 and Output layer
     &  -.26078E+01,
C      Weight between Hidden neuron 7 and Output layer
     &   .27941E+00,
C      Weight between Hidden neuron 8 and Output layer
     &  -.11770E+01,
C      Weight between Hidden neuron 9 and Output layer
     &  -.12503E+00,
C      Weight between Hidden neuron 10 and Output layer
     &  -.10998E+01,
C      Weight between Hidden neuron 11 and Output layer
     &   .17184E+00,
C      Weight between Hidden neuron 12 and Output layer
     &   .82094E-01,
C      Weight between Hidden neuron 13 and Output layer
     &  -.69280E+00,
C      Weight between Hidden neuron 14 and Output layer
     &  -.83323E+00,
C      Weight between Hidden neuron 15 and Output layer
     &  -.34006E+00,
C      Weight between threshold Hidden neuron and Output layer
     &  -.28438E+01/
C
