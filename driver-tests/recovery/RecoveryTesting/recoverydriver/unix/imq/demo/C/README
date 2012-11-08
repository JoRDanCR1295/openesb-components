C Client Examples For Sun Java(tm) System Message Queue C-API


This directory contains C client examples to help you get started
using the Message Queue C-API. 

The Message Queue C-API allows clients written in the C/C++ programming 
languages to use Message Queue directly. The Message Queue C-API client 
runtime library communicates directly with the Message Queue broker.

The Message Queue C-API client runtime library mqcrt and the API header 
files are located in the following directories:

Solaris: /opt/SUNWimq/lib 
         /opt/SUNWimq/lib/sparcv9 (64-bit Solaris SPARC)
         /opt/SUNWimq/lib/amd64   (64-bit Solaris x86)
         /opt/SUNWimq/include

Linux:   /opt/sun/mq/lib
         /opt/sun/mq/include

#####hpux-dev#####
HP-UX:   /opt/sun/mq/lib
         /opt/sun/mq/include

Windows: IMQ_HOME\lib
         IMQ_HOME\include

         where IMQ_HOME is the root Message Queue installation directory.


To build the examples, use a supported compiler (see Message Queue C-API 
documentation).

For example, to build the example program Producer in the directory 
producer_consumer,

On Solaris:

CC  -compat=5 -mt -DSOLARIS -I/opt/SUNWimq/include -o Producer \
    -L/opt/SUNWimq/lib -lmqcrt Producer.c

for 64-bit on Solaris SPARC,

CC  -compat=5 -mt -xarch=v9 -DSOLARIS -I/opt/SUNWimq/include -o Producer \
    -L/opt/SUNWimq/lib/sparcv9 -lmqcrt Producer.c

for 64-bit on Solaris x86,

CC  -compat=5 -mt -xarch=amd64 -DSOLARIS -I/opt/SUNWimq/include -o Producer \
    -L/opt/SUNWimq/lib/amd64 -lmqcrt Producer.c



On Linux: 

g++ -DLINUX -D_REENTRANT -I/opt/sun/mq/include -o Producer \
    -L/opt/sun/mq/lib -lmqcrt Producer.c

#####hpux-dev#####
On HP-UX:

/opt/aCC/bin/aCC Producer.c +DAportable -I/opt/sun/mq/include \ 
    -L/opt/sun/private/lib -L/opt/sun/mq/lib -lnss3 -lmqcrt \ 
    -Wl,+b,/opt/sun/private/lib -o Producer


On Windows:

to compile,

cl /c /MD -DWIN32 -I%IMQ_HOME%\include Producer.c

to link,

link Producer.obj /NODEFAULTLIB msvcrt.lib /LIBPATH:%IMQ_HOME%\lib mqcrt.lib


The following is a list of directories which contain the C client examples:

. producer_consumer
. producer_async_consumer
. request_reply

Please see individual source files for descriptions of the examples.

You should start the Message Queue broker before running the examples.
