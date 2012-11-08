/*
 * SEPluginProjectGenerator.java
 *
 */

package org.openesb.components.camelse.nb.plugin.project;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.charset.Charset;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.queries.FileEncodingQuery;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.ProjectGenerator;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;
import org.openide.modules.SpecificationVersion;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Creates a Deployment Plugin Project for Service Engine.
 * @author chikkala
 */
public class SEPluginProjectGenerator {

    private File mPrjDir;
    private String mPrjName;
    private String mSUName;
    private String mSUDesc;
    private String mSUTarget;
    
    private String mCamelHome;


    public SEPluginProjectGenerator() {
        this.mSUName = SEPluginProjectProperties.JBI_SU_NAME_VALUE;
        this.mSUDesc = SEPluginProjectProperties.JBI_SU_DESCRIPTION_VALUE;
        this.mSUTarget = SEPluginProjectProperties.JBI_SU_TARGET_NAME_VALUE;
    }

    public File getProjectDirectory() {
        return this.mPrjDir;
    }

    public String getProjectName() {
        return this.mPrjName;
    }

    public String getSUName() {
        return this.mSUName;
    }

    public void setSUName(String suName) {
        this.mSUName = suName;
    }

    public String getSUDescription() {
        return this.mSUDesc;
    }

    public void setSUDescription(String suDesc) {
        this.mSUDesc = suDesc;
    }

    public String getSUTarget() {
        return this.mSUTarget;
    }

    public void setSUTarget(String suTarget) {
        this.mSUTarget = suTarget;
    }

    public String getCamelHome() {
        return mCamelHome;
    }

    public void setCamelHome(String camelHome) {
        this.mCamelHome = camelHome;
    }
    
    private void createPrimaryConfigurationData(AntProjectHelper prjHelper) {

        Element data = prjHelper.getPrimaryConfigurationData(true);
        Document doc = data.getOwnerDocument();
        
        Element nameEl = doc.createElementNS(SEPluginProjectType.PROJECT_CONFIGURATION_NAMESPACE, "name"); // NOI18N
        nameEl.appendChild(doc.createTextNode(this.getProjectName()));
        data.appendChild(nameEl);
        
        
        Element minant = doc.createElementNS(SEPluginProjectType.PROJECT_CONFIGURATION_NAMESPACE, "minimum-ant-version"); // NOI18N
        minant.appendChild(doc.createTextNode(SEPluginProjectProperties.MINIMUM_ANT_VERSION)); // NOI18N
        data.appendChild(minant);
        
        Element sourceRoots = doc.createElementNS(SEPluginProjectType.PROJECT_CONFIGURATION_NAMESPACE,"source-roots");  //NOI18N
        Element srcRoot = doc.createElementNS(SEPluginProjectType.PROJECT_CONFIGURATION_NAMESPACE,"root");   //NOI18N
        srcRoot.setAttribute("id", SEPluginProjectProperties.SRC_DIR);   //NOI18N
        sourceRoots.appendChild(srcRoot);
        data.appendChild(sourceRoots);
        
        Element testRoots = doc.createElementNS(SEPluginProjectType.PROJECT_CONFIGURATION_NAMESPACE,"test-roots");  //NOI18N
        Element testRoot = doc.createElementNS(SEPluginProjectType.PROJECT_CONFIGURATION_NAMESPACE,"root");   //NOI18N
        testRoot.setAttribute("id", SEPluginProjectProperties.TEST_DIR);   //NOI18N
        testRoots.appendChild(testRoot);
        data.appendChild(testRoots);
        
        prjHelper.putPrimaryConfigurationData(data, true);
            
        
    }

    private void createProjectPrivateProperties(AntProjectHelper prjHelper) {

        EditableProperties ep = prjHelper.getProperties(AntProjectHelper.PRIVATE_PROPERTIES_PATH);

        //TODO:  add any project private properties here.
        // ep.setProperty("application.args", ""); // NOI18N
        prjHelper.putProperties(AntProjectHelper.PRIVATE_PROPERTIES_PATH, ep);
    }

    private String getCamelVersion(String camelHome) {
        // System.err.println("Looking for camel version in " + camelHome);
        String camelVersion = "1.5.0"; // current supported version.
        if ( camelHome != null && camelHome.trim().length() > 0 ) {
            File camelHomeFile = new File(camelHome);
            File camelLibDir = new File(camelHomeFile, "lib");
            if ( camelLibDir.exists() && camelLibDir.isDirectory()) {
                // System.err.println("Camel Lib Dir exists");
                File[] libs = camelLibDir.listFiles(new FilenameFilter() {

                    public boolean accept(File dir, String name) {
                       // System.err.println("Checking file for core " + name);
                        if ( name.startsWith("camel-core-") && name.endsWith(".jar")) {
                           // System.err.println("Found camel core jar");
                            return true;
                        } else {
                            return false;
                        }
                    }                    
                });
                if ( libs != null ) {
                    String coreLib = libs[0].getName();
                    int beginIdx = "camel-core-".length();
                    camelVersion = coreLib.substring(beginIdx, beginIdx + 5);
                  //  System.err.print("CamelVersion derived " + camelVersion);
                } else {
                  //  System.err.println("Camel Libs list is NULL");
                }
            }
        }
        return camelVersion;
    }
    
    private void createCamel130ClassPath(EditableProperties ep) {
        
        ep.setProperty(SEPluginProjectProperties.LIBS_CAMEL_CLASSPATH, new String[]{
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/camel-core-1.3.0.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/camel-spring-1.3.0.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-2.5.1.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/camel-jaxb-1.3.0.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/jaxb-api-2.1.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/jaxb-impl-2.1.3.jar:",            
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/camel-xmlbeans-1.3.0.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/commons-logging-1.0.4.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/stax-api-1.0-2.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/activation-1.1.jar"
        }); // NOI18N                
    }

    private void createCamel140ClassPath(EditableProperties ep) {
        
        ep.setProperty(SEPluginProjectProperties.LIBS_CAMEL_CLASSPATH, new String[]{
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/apache-camel-1.4.0.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-aop-2.5.5.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-beans-2.5.5.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-context-2.5.5.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-core-2.5.5.jar:",            
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/jaxb-api-2.1.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/jaxb-impl-2.1.6.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/commons-logging-1.1.1.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/stax-api-1.0-2.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/activation-1.1.jar"
        }); // NOI18N                
    }

    private void createCamel150ClassPath(EditableProperties ep) {
        
        ep.setProperty(SEPluginProjectProperties.LIBS_CAMEL_CLASSPATH, new String[]{
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/apache-camel-1.5.0.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-aop-2.5.5.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-beans-2.5.5.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-context-2.5.5.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/spring-core-2.5.5.jar:",            
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/jaxb-api-2.1.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/jaxb-impl-2.1.6.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/commons-logging-1.1.1.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/stax-api-1.0-2.jar:",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/activation-1.1.jar",
            "${" + SEPluginProjectProperties.CAMEL_HOME + "}/lib/optional/log4j-1.2.14.jar"
        }); // NOI18N                
    }
    
    private void createCamelLibClassPath(EditableProperties ep) {
        
        String camelHome = getCamelHome();
        if ( camelHome == null || camelHome.trim().length() == 0 ) {
            camelHome = "c:/apache/apache-camel-1.5.0";
        }
        ep.setProperty(SEPluginProjectProperties.CAMEL_HOME, camelHome); // NOI18N
        
        String camelVersion = getCamelVersion(camelHome);
        System.err.println(" #### Camel Version " + camelVersion);        
        if ( "1.3.0".equals(camelVersion)) {
            createCamel130ClassPath(ep);
        } else if ( "1.4.0".equals(camelVersion) ){
            createCamel140ClassPath(ep);
        } else {
            createCamel150ClassPath(ep);
        }
    }
    
    private void createProjectProperties(AntProjectHelper prjHelper) {

        String manifestFile = null;
        String mainClass = "org.apache.camel.spring.Main";
        String name = this.getProjectName();
        
        EditableProperties ep = prjHelper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);

        ep.setProperty(SEPluginProjectProperties.SRC_DIR, SEPluginProjectProperties.SRC_DIR_VALUE);
        ep.setComment(SEPluginProjectProperties.SRC_DIR, new String[]{"# service unit source directory "}, false); // NOI18N
        ep.setProperty(SEPluginProjectProperties.BUILD_DIR, SEPluginProjectProperties.BUILD_DIR_VALUE);

        ep.setProperty(SEPluginProjectProperties.BUILD_DIR, SEPluginProjectProperties.BUILD_DIR_VALUE);

        ep.setProperty(SEPluginProjectProperties.JBI_SU_ZIP, SEPluginProjectProperties.JBI_SU_ZIP_VALUE);

        ep.setProperty(SEPluginProjectProperties.JBI_SU_NAME, getSUName());
        ep.setProperty(SEPluginProjectProperties.JBI_SU_DESCRIPTION, getSUDescription());
        ep.setProperty(SEPluginProjectProperties.JBI_SU_TARGET_NAME, getSUTarget());

        // create javac related properties here.
        
        createCamelLibClassPath(ep);
        
        ep.setProperty("test.src.dir", "test"); // NOI18N
        ep.setProperty("dist.dir", "dist"); // NOI18N
        ep.setProperty("dist.jar", "${dist.dir}/" + PropertyUtils.getUsablePropertyName(name) + ".jar"); // NOI18N
        
        ep.setProperty("javac.classpath", new String[]{
            "${" + SEPluginProjectProperties.LIBS_CAMEL_CLASSPATH + "}"
        }); // NOI18N
        
        ep.setProperty("build.sysclasspath", "ignore"); // NOI18N
        ep.setProperty("run.classpath", new String[] { // NOI18N
            "${javac.classpath}:", // NOI18N
            "${build.classes.dir}", // NOI18N
        });
        ep.setProperty("debug.classpath", new String[] { // NOI18N
            "${run.classpath}", // NOI18N
        });        
        ep.setProperty("jar.compress", "false"); // NOI18N

        ep.setProperty("main.class", mainClass == null ? "" : mainClass); // NOI18N
            
        ep.setProperty("javac.compilerargs", ""); // NOI18N
        SpecificationVersion sourceLevel = getDefaultSourceLevel();
        ep.setProperty("javac.source", sourceLevel.toString()); // NOI18N
        ep.setProperty("javac.target", sourceLevel.toString()); // NOI18N
        ep.setProperty("javac.deprecation", "false"); // NOI18N
        ep.setProperty("javac.test.classpath", new String[] { // NOI18N
            "${javac.classpath}:", // NOI18N
            "${build.classes.dir}:", // NOI18N
            "${libs.junit.classpath}:", // NOI18N
            "${libs.junit_4.classpath}",  //NOI18N
        });
        ep.setProperty("run.test.classpath", new String[] { // NOI18N
            "${javac.test.classpath}:", // NOI18N
            "${build.test.classes.dir}", // NOI18N
        });
        ep.setProperty("debug.test.classpath", new String[] { // NOI18N
            "${run.test.classpath}", // NOI18N
        });

        ep.setProperty("build.generated.dir", "${build.dir}/generated"); // NOI18N
        ep.setProperty("meta.inf.dir", "${src.dir}/META-INF"); // NOI18N
        
        ep.setProperty("build.dir", "build"); // NOI18N

        ep.setProperty("build.classes.dir", "${build.dir}/classes"); // NOI18N
        ep.setProperty("build.test.classes.dir", "${build.dir}/test/classes"); // NOI18N
        ep.setProperty("build.test.results.dir", "${build.dir}/test/results"); // NOI18N
        ep.setProperty("build.classes.excludes", "**/*.java,**/*.form"); // NOI18N
        ep.setProperty("dist.javadoc.dir", "${dist.dir}/javadoc"); // NOI18N
        ep.setProperty("platform.active", "default_platform"); // NOI18N

        ep.setProperty("run.jvmargs", ""); // NOI18N


        ep.setProperty(SEPluginProjectProperties.JAVADOC_PRIVATE, "false"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_NO_TREE, "false"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_USE, "true"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_NO_NAVBAR, "false"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_NO_INDEX, "false"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_SPLIT_INDEX, "true"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_AUTHOR, "false"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_VERSION, "false"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_WINDOW_TITLE, ""); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_ENCODING, "${"+SEPluginProjectProperties.SOURCE_ENCODING+"}"); // NOI18N
        ep.setProperty(SEPluginProjectProperties.JAVADOC_ADDITIONALPARAM, ""); // NOI18N
        Charset enc = FileEncodingQuery.getDefaultEncoding();
        ep.setProperty(SEPluginProjectProperties.SOURCE_ENCODING, enc.name());
        if (manifestFile != null) {
            ep.setProperty("manifest.file", manifestFile); // NOI18N
        }
        
        // save properties to file.
        prjHelper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, ep);
    }

    public AntProjectHelper createProject(File prjDir, String prjName) throws IOException {
        AntProjectHelper prjHelper = null;
        
        this.mPrjDir = prjDir;
        this.mPrjName = prjName;
        if (SEPluginProjectProperties.JBI_SU_NAME_VALUE.equals(this.getSUName())) {
            // default value. so set the su name to project name.
            String suName = PropertyUtils.getUsablePropertyName(this.getProjectName());
            this.setSUName(suName);
        }
        
        FileObject prjDirFO = createProjectDir(this.getProjectDirectory());
        
        prjHelper = ProjectGenerator.createProject(prjDirFO, SEPluginProjectType.TYPE);
 
        FileObject srcFolder = FileUtil.createFolder(prjDirFO, SEPluginProjectProperties.SRC_DIR_VALUE); // NOI18N
        FileObject testFolder = FileUtil.createFolder(prjDirFO, "test"); // NOI18N
        
        createPrimaryConfigurationData(prjHelper);
        createProjectProperties(prjHelper);
        createProjectPrivateProperties(prjHelper);
        
        // create su jbi.xml
        // SEPluginProjectProperties.createDefaultSUDescriptor(srcFolder);
        
        //TODO:  create any service unit specifc default artifacts here.
        
        Project p = ProjectManager.getDefault().findProject(prjDirFO);
        ProjectManager.getDefault().saveProject(p);
        
        return prjHelper;
    }

    private static FileObject createProjectDir(File dir) throws IOException {
        FileObject dirFO;
        if (!dir.exists()) {
            //Refresh before mkdir not to depend on window focus, refreshFileSystem does not work correctly
            refreshFolder(dir);
            if (!dir.mkdirs()) {
                throw new IOException("Can not create project folder."); //NOI18N
            }
            refreshFileSystem(dir);
        }
        dirFO = FileUtil.toFileObject(dir);
        assert dirFO != null : "No such dir on disk: " + dir; // NOI18N
        assert dirFO.isFolder() : "Not really a dir: " + dir; // NOI18N
        return dirFO;
    }

    private static void refreshFileSystem(final File dir) throws FileStateInvalidException {
        File rootF = dir;
        while (rootF.getParentFile() != null) {
            rootF = rootF.getParentFile();
        }
        FileObject dirFO = FileUtil.toFileObject(rootF);
        assert dirFO != null : "At least disk roots must be mounted! " + rootF; // NOI18N
        dirFO.getFileSystem().refresh(false);
    }

    private static void refreshFolder(File dir) {
        while (!dir.exists()) {
            dir = dir.getParentFile();
        }
        FileObject fo = FileUtil.toFileObject(dir);
        if (fo != null) {
            fo.getChildren();
            fo.refresh();
        }
    }
    
    //------------ Used by unit tests -------------------
    private static SpecificationVersion defaultSourceLevel;
    
    private static SpecificationVersion getDefaultSourceLevel () {
        if (defaultSourceLevel != null) {
            return defaultSourceLevel;
        }
        else {
            JavaPlatform defaultPlatform = JavaPlatformManager.getDefault().getDefaultPlatform();
            SpecificationVersion v = defaultPlatform.getSpecification().getVersion();
            if (v.equals(new SpecificationVersion("1.6")) || v.equals(new SpecificationVersion("1.7"))) {
                // #89131: these levels are not actually distinct from 1.5. - xxx not true, but may be acceptable to have 1.5 as default
                return new SpecificationVersion("1.5");
            } else {
                return v;
            }
        }
    }
    
    /**
     * Unit test only method. Sets the default source level for tests
     * where the default platform is not available.
     * @param version the default source level set to project when it is created
     *
     */
    public static void setDefaultSourceLevel (SpecificationVersion version) {
        defaultSourceLevel = version;
    }    
}
