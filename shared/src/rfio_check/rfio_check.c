#include <fcntl.h>
#include <shift.h>
main(argc,argv)
int argc;
char *argv[];
{
int irc,retcode;
if (argc != 2)exit(-1);
irc=rfio_open(argv[1],O_RDONLY,0);
retcode=rfio_errno;
if (retcode == 0) irc=rfio_close(irc);
exit (retcode);
}
