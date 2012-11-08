================================================================================
@(#)README	1.3 06/16/05
================================================================================

JMX Management and Monitoring API examples

Description
-----------
This directory contain examples that demonstrate the Sun Java(tm) System 
Message Queue broker JMX Management and Monitoring API. The examples consist 
of separate applications each showing how to monitor or manage the broker using
the JMX API.

Files
-----
SimpleClient.java	Source file for application that displays the broker 
			instance name and version.
ListDestinations.java	Source file for application that lists destinations
			and their state.
MQClusterMonitor.java	Source file for application that is a HA cluster
			monitor.
MQConnectDialog.java	Source file for connect dialog that is used by
			some of the JMX example applications.
MQLogViewer.java	Source file for application that is a JMX listener
			for log events.
*.class			Prebuilt Java class files for these examples.
README			This file.


Configuring the environment
---------------------------
To recompile or run this example, you need to set CLASSPATH
to include at least:
    imqjmx.jar
    directory containing this example

The following are examples for setting CLASSPATH on the different
platforms.

On Solaris:
    setenv CLASSPATH /usr/share/lib/imqjmx.jar:.

On Windows:
    set CLASSPATH=%IMQ_HOME%\lib\imqjmx.jar;.

On Linux:
    setenv CLASSPATH /opt/sun/mq/share/lib/imqjmx.jar:.

The examples above assume JDK 1.5 is used. If JDK 1.4 or
earlier is used, additional jar files containing the JMX
classes need to be added. This is still TBD.

Building the example
--------------------
Compile the sample programs individually if you wish, or all at 
once by using the command

    javac *.java

Running the example
-------------------
The examples can be run using the java command e.g.
 
    java <classname>

For example,

    java SimpleClient

Here is a description of each of the examples:

SimpleClient
-------------
This application displays the broker's instance
name and version.

Example usage:
    java SimpleClient

ListDestinations
----------------
This application displays the current list of destinations
on the broker and their state.

Example usage:
    java ListDestinations

