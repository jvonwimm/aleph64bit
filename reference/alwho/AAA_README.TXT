The files in this directory are what is needed to run alwho on aloha, both 
interactively and via the World Wide Web server.

The Makefile_WWW makes alwho_query_prog, which is invoked by the alwho_query 
CGI script via the WWW server.

The Makefile_alwho makes alwho_call, which is invoked by the interactive script
$ALEPH/script/alwho.

Both scripts expect to find the executables in the directory $ALEPH/bin.

The file alwho.database is the alwho database, which is automatically updated
from the master file on VXCERN once per night.
