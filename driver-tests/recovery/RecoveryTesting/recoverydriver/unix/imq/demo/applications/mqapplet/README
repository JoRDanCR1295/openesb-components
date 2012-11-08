================================================================================
@(#)README	1.7 05/08/07
================================================================================

JMS Applet example

Description
-----------
This example illustrates how JMS API can be used in a Java applet.
MQApplet is a simple chat applet. It can be used with any Java enabled
browser.

Files
-----
MQApplet.java   Source file for this example.
mqapplet.html   HTML Source file for the applet.
README          This file.
build.xml       Build rules (for Apache Ant or Jakarta Ant).

Configuring the environment
---------------------------
To recompile or run this example, you may need to modify the build.xml
file. Please change the value of the "libdir" property as appropriate.
It should point to the directory that contains the "imq.jar" and
"jms.jar" files.

Building the example
--------------------

* Set the JAVA_HOME environment variable to point to the JDK.
* Simply run the Jakarta ant build tool or Apache Ant to build the applet.

For example:
In Solaris sparc, you could use the Apache Ant (1.5.4) from /usr/sfw/bin
Type: ant 

The build script will automatically create directories "install" and "build". 

The applet compilation process is a bit different from the typical
standalone Message Queue clients. It needs to do the following things 
Which have been created automatically under directory "install". 

* Compile MQApplet.java into MQApplet.class
* Bundle MQApplet.class into a jar file -> mqapplet.jar
* Copy the imq.jar and jms.jar files to a local directory.
* Generate a self signed certificate using keytool.
* Sign mqapplet.jar, imq.jar and jms.jar using this certificate.
  Applets must be signed if they need to do anything non-trivial.
  Otherwise you will get security exceptions at runtime.

Running the example applet
--------------------------

The build script places all the files necessary for running this
applet into a new directory named "install". Copy all the contents of
this directory somewhere under a web server's document root. This
includes -

    * mqapplet.html
    * Signed Jar files : mqapplet.jar, imq.jar, jms.jar

After this, the applet can be run by simply pointing the browser to
the "mqapplet.html" file.

After the applet is loaded, the web browser (Java plugin) should open
a dialog box asking for permission to run a signed applet. Click on
the "Run"  button.

Bring up the Message Queue broker by typing "imqbrokerd -tty"
When the applet starts running it will not automatically connect to
a Message Queue broker. Please specify the broker's address and click 
on the "Connect" button. 
Note: default broker's address will be prompted 
imqAddressList: mq://localhost:7676
You could modify "localhost" to the host name where the broker was brought up 

Running as a standalone application
-----------------------------------

The MQApplet.class(build/MQApplet.class) can also be used as a 
standalone application. It presents the same UI as the applet. To run 
this example as a standalone application, you need to set CLASSPATH 
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


After setting the CLASSPATH, simply run -
    java MQApplet

Obviously, you don't need the signed jar files for running as
a standalone application.

