================================================================================
@(#)README	1.7 03/22/05
================================================================================

QueueBrowser example

Description
-----------
This example illustrates how to write a GUI application that uses a
JMS QueueBrowser to browse the contents of a queue. It also illustrates 
the use of the Message Queue monitoring API to get the list of destinations
from the Message Queue broker.

Some basic info on this example:
- Subscribes to the "mq.metrics.detination_list" topic to get the
  list of destinations from imqbrokerd and places them in a menu
  (only works with EE edition).
- Creates a QueueBrowser for the particular queue specified.
- Lists messages on the queue, and displays the contents of a
  selected message.
- Uses javax.swing for GUI components.

Files
-----
QBrowser.java		Source file for this example.
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

    javac QBrowser.java

Running the example
-------------------
By default QBrowser will connect to the broker running on localhost:7676.
You can use -DimqAddressList attribute to change the host, port and 
transport:
 
 	java -DimqAddressList=mq://<host>:<port>/jms QBrowser

QBrowser will connect to the broker specified. The menu associated
with the "Queue Name" text field will populate with queue names
from the specified broker.

Select a queue from the menu, or type in a queue and click Browse.
QBrowser will list all messages in the queue.

Select a message and click "Details" to see the message contents
(properties, body, etc). Or double click.
          
If you don't have any queues with messages on them, try running
the jms/SenderToQueue example to send a few messages to a queue.
For example:

    [ Run from the jms demo subdirectory ]
    java SenderToQueue myqueue 5

QBrowser updates it's list of destinations every few minutes.

