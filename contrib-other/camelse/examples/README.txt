
This directory contains netbeans projects for various scenarios of 
Camel applications running in OpenESB. 

Open the Composite appliation project in each application and its required 
JBI Module projects to build, deploy and test the examples from Netbeans 6.1

Examples
--------

NOTE: before build and deploy these projects, 
0. Make sure that you have installed GlassfishESB (or latest OpenESB) as some of
   the examples such as "c2db" below uses latest jbi components.
1. fix the camel home for the camel jbi modules 
    (project node -> properties -> camel category)
2. Edit the AppRouteBuilder.java file in each CamelJBIModule projects 
   to fix the projectDir path if exists to the path where you extracted project.

file2file -> demonstrates running a camel app in OpenESB
             camel(file)->camel(file)

jbi2file  -> demonstrates messaage exchange from jbi to camel app 
             jbi(soap bc) -> camel(file)

jbi2pojo  -> demonstrates message exchange from jbi to a POJO in camel app
             jbi(soap bc) -> camel(bean)

file2jbi  -> demonstrates message exchange from jbi to camel and vice versa using
             jbi engines(bpel)
             camle(file) -> jbi(bpel)->jbi(camel)->camel(file)

jbi2jbi  -> demonstrates the message exchange from jbi to camel and vice versa
            using just the jbi binding components.
            jbi(soap bc)  -> camel -> jbi(file bc)

c2db -> demonstrates the database bc access through camel. 
        jbi(soap bc) -> camel -> jbi (database bc) 

        Before running this example, make sure that the database table is 
        created and the connection to the database is working. You can use the 
        create-table.sql file in the DBCamelJBIModule to create the table.
        
        After deploying the composite app, run the tests DeleteTestCase, 
        VerfiyDeleteTestCase,InsertTestCase,VerifyInsertTestCase,UpdateTestCase,
        VerifyUpdateTestCase in that order and check the database for changes.



