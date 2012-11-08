
Welcome to the Apache Camel JBI service engine project for OpenESB
------------------------------------------------------------------

This project consists of a JBI Service engine named "camel-jbi-se" that can be 
used to run Apache Camel applications in a JBI Platform and a corresponding 
netbeans plugin which can be used to develop Apache Camel application as a 
JBI Service unit in Netbeans IDE to deploy it to the "camel-jbi-se" service engine
running in OpenESB.

Apache Camel is a POJO Routing Engine and Mediation Framework. Please visit
http://activemq.apache.org/camel/ for more details about Apache Camel.

The Apache Camel version used in this project is 1.5.0.

The Apache Camel license is in the file camel-jbi-se/camel-libs/LICENCE.txt
The Apache Camel Notice is in the file camel-jbi-se/camel-libs/NOTICE.txt

How to use the camel-jbi-se service engine
------------------------------------------
1. Install the camel-jbi-se.zip file on OpenESB V2.
2. Install org-openesb-components-camelse-nb-plugin.nbm in Netbeans 6.1 IDE
3. Create and deploy service units using Camel JBI Module project
   ( File->New Project...[SOA->Camel JBI Module]) and the Composite Application
   tools in Netbeans 6.1 IDE

See demo video at http://wiki.open-esb.java.net/Wiki.jsp?page=CamelSE

For more details visit wiki page http://wiki.open-esb.java.net/Wiki.jsp?page=CamelSE

How to build the project
------------------------
This project and its correpsonding netbeans plugin project requires Netbeans 6.1
and JBI CDK (http://wiki.open-esb.java.net/Wiki.jsp?page=JbiComponentDevTools).

The following are the steps to build the project from Netbeans 6.1

1. Install Netbeans 6.1 
2. Install JBI CDK in Netbeans 6.1
3. Open this project and the netbeans plugin project from Netbeans and build.

--

Thank you.

Srinivasan Chikkala
 Email : Srinivasan.Chikkala@sun.com
 Blog  : http://blogs.sun.com/schikkala/
 Open ESB Community (http://open-esb.org)



