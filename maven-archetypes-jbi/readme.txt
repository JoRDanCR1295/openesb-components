This directory is a parent project form two maven archetype that can be used to create maven
based projects for developing JBI Components (Service Engines and Binding
Components). The maven projects created using these archetypes are complete
projects which are ready to build and test ( unit and integration test ) a
working JBI Component. The maven project created for the JBI Component will have
a maven project metadata (pom.xml), ant build scripts,  sample code that
implements either BC or SE,  sample junit test and integration test code. The
project can be buildable using maven build lifecycle or ant build lifecycle to
completely build and test the component. The sample source code and the source
structure for component implementation is same as that explained in the article
JBI Examples - Developing JBI Components
(https://open-esb.dev.java.net/public/jbi-comp-examples/Developing_JBI_Components.html)

once these projects are installed, the archetypes will be ready to be used from
the local repository.

To build maven-archetype projects and install into the local repository, do this

cd open-jbi-components/maven-archetypes-jbi
smvn install

to use the installed maven archetpes build from the above projects to create jbi
component projects use the following commands

For creating Service Engine project
---------------------------------------------------

smvn archetype:create                                       
   -DarchetypeGroupId=open.jbi.components.maven.archetype   
   -DarchetypeArtifactId=maven-archetype-jbi-se             
   -DarchetypeVersion=0.1                                   
   -DgroupId=<your.component.group.id>                      
   -DartifactId=<yourEngineName>                            
   -DpackageName=<your.component.package>                   
   -Dversion=<your project version>


For creating Binding Component project
----------------------------------------------------------

mvn archetype:create                                       
   -DarchetypeGroupId=open.jbi.components.maven.archetype   
   -DarchetypeArtifactId=maven-archetype-jbi-bc             
   -DarchetypeVersion=0.1                                   
   -DgroupId=<your.component.group.id>                      
   -DartifactId=<yourBindingName>                           
   -DpackageName=<your.component.package>                   
   -Dversion=<your project version>


    * groupId :  Group id of the new jbi component maven project that you are
creating.
    * artifactId: Artifact id of the new project. Its value will be used to
create the project directory. This value will be also used as a JBI component
name in generated sample code and the jbi installation descriptor for the component.
    * packageName:  A dot separated name for the java package name that will be
created under source root and where the component specific classes will be
added. It also will be used as  package name for the Component and Bootstrap
classes in the installation descriptor. If you omit the packageName, the groupId
value will be used as a package name. So, make sure that the group id is a
qualified package name if you omit the packageName.
    * version:  Version for your component project. for example, 1.0-SNAPSHOT


see http://blogs.sun.com/schikkala/entry/maven_archetypes_for_jbi_component  for
more details.

