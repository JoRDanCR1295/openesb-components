
@(#)README	1.4 03/22/05
================================================================================

mqping utility

Description
-----------
The mqping utility is similar to the Unix ping utility in some regards.  
With mqping, messages are sent to and received from a running broker.
The utility measures the round trip time.  The utility allows the user
to control the size of the message, the destination type, delivery mode
and send interval.

Files
-----
mqping.java		Source file for this example.
*.class			Prebuilt Java class files for this example.
README			This file.

Configuring the environment
---------------------------
To recompile or run this example, you need to set CLASSPATH
to include at least:
    jms.jar
    imq.jar
    directory containing this example

A detailed guideline on setting CLASSPATH is found in the README
file in the jms demo subdirectory as well as in the "Quick Start
Tutorial" in the Sun Java(tm) System Message Queue Developer's Guide.

The following are examples for setting CLASSPATH on the different
platforms. These commands are run from the directory containing
this example.

On Solaris:
    setenv CLASSPATH /usr/share/lib/jms.jar:/usr/share/lib/imq.jar:.

On Windows:
    set CLASSPATH=%IMQ_HOME%\lib\jms.jar;%IMQ_HOME%\lib\imq.jar;.

On Linux:
    setenv CLASSPATH /opt/sun/mq/share/lib/jms.jar:
	/opt/sun/mq/share/lib/imq.jar:.

#####hpux-dev#####
On HP-UX:
      export CLASSPATH=/opt/sun/mq/share/lib/jms.jar:/opt/sun/mq/share/lib/imq.jar:.

Note that it is assumed that the above export command is run on
BASH shell



Building the example
--------------------
Run the following:

    javac mqping.java

Running the example
-------------------
In the simplest form the mqping utility can be run with no options:

    java mqping

This utility takes the following arguments:
      -t dest_type    Specify the optional destination type.  Valid values
                      are 't' or 'q'.  Default: 'q'
      -r              Optionally indicate the message is persistent.  
                      Not specifying this option indicates the message 
                      should not be persisted.
      -s size         Specify the optional size of the messages in bytes.
                      Default: 1024 
      -i delay        The interval (in seconds) between successive 
                      transmissions.  Default: 0 (no delay)

 By default mqping will connect to the broker running on localhost:7676.
 You can use -DimqAddressList attribute to change the host, port and 
 transport:
 
 	java -DimqAddressList=mq://<host>:<port>/jms mqping

