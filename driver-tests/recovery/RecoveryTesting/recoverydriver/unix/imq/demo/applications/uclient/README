================================================================================
@(#)README	1.1 06/01/05
================================================================================

UniversalClient example

Description
-----------
The UniversalClient example is a basic 'client' application that uses the JMS 1.1 APIs.
It uses JMS Message Producer and Consumer to send and receive message.
UniversalClient.java does not use JNDI and relies on
the feature by which a broker can "auto-create" the destinations 
desired by the user. The bulk of the application deals with the 
user interface and the JMS related code is fairly straightforward.


Files
-----
UniversalClient.java		Source file for this example.
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
export CLASSPATH=/opt/sun/mq/share/lib/jms.jar:
           /opt/sun/mq/share/lib/imq.jar:.

Note that it is assumed that the above export command is run on
BASH shell


Building the example
--------------------
Run the following:

    javac UniversalClient.java

Running the example
-------------------
Run the following:

    java UniversalClient

The application comes up not connected to any jms session. 
To connect to  a broker, bring down the "Actions" menu and
select the "Connect ..." menu item. A dialog will appear to
provide broker hostname, port, username, password, clientID
etc

To disconnect from a given broker host: port bring down the
"Actions" menu and select the "Disconnect" menu item

Once the application is connected, one can send message using
a message producer. To create a producer bring down the "Actions" 
menu and select the "Send Message..." menu item. The Send Message
dialog box will come up, where destination name,type and message 
size,ttl etc can be specified while sending message.

To stop message sender bring down the "Actions" menu and select
the "Stop Message Sender" menu item

To receive message bring down the "Actions" menu and select the
"Receive Message..." menu item. The receive message dialog box
will come up where destination name, message consumer type and
message selector can be specified.

To stop message receiver bring down the "Actions" menu and select
the "Stop Message Receiver" menu item.

The received message are displayed on a Message Table. To see the
details of a message double click on a row of a message table or
select a row of a message table and click on "Message Details" button.

To start auto scrolling of message table i.e to always see the last
message received, drag the message table vertical scroll bar at the
bottom. To stop auto scrolling drag the message table vertical scroll
bar else where.

To clear existing messages in the message table bring down the "Actions"
menu and select the "Clear Messages" menu item.

The status bar at the bottom shows Message Queue Client Runtime
Connection Notification And Connection related log


To exit from the application, bring down the "console" menu and
click "exit"


