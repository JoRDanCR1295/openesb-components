/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport.project;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;
import org.netbeans.modules.jbi.apisupport.JbiAdminSettings;
import org.netbeans.modules.jbi.apisupport.common.JbiComponentDescriptor;
import org.netbeans.modules.jbi.apisupport.common.JbiDescriptorFactory;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 * @author chikkala, j2seproject
 */
public class JbiCompProjectProperties {
        
    // Special properties of the project
    public static final String JAVA_PLATFORM = "platform.active"; // NOI18N
    
    // Properties stored in the PROJECT.PROPERTIES    
    public static final String DIST_DIR = "dist.dir"; // NOI18N
    public static final String DIST_JAR = "dist.jar"; // NOI18N
    public static final String JAVAC_CLASSPATH = "javac.classpath"; // NOI18N
    public static final String RUN_CLASSPATH = "run.classpath"; // NOI18N
    public static final String RUN_JVM_ARGS = "run.jvmargs"; // NOI18N
    public static final String RUN_WORK_DIR = "work.dir"; // NOI18N
    public static final String DEBUG_CLASSPATH = "debug.classpath"; // NOI18N
    public static final String JAR_COMPRESS = "jar.compress"; // NOI18N
    public static final String MAIN_CLASS = "main.class"; // NOI18N
    public static final String JAVAC_SOURCE = "javac.source"; // NOI18N
    public static final String JAVAC_TARGET = "javac.target"; // NOI18N
    public static final String JAVAC_TEST_CLASSPATH = "javac.test.classpath"; // NOI18N
    public static final String JAVAC_DEBUG = "javac.debug"; // NOI18N
    public static final String JAVAC_DEPRECATION = "javac.deprecation"; // NOI18N
    public static final String JAVAC_COMPILER_ARG = "javac.compilerargs";    //NOI18N
    public static final String RUN_TEST_CLASSPATH = "run.test.classpath"; // NOI18N
    public static final String BUILD_DIR = "build.dir"; // NOI18N
    public static final String BUILD_CLASSES_DIR = "build.classes.dir"; // NOI18N
    public static final String BUILD_TEST_CLASSES_DIR = "build.test.classes.dir"; // NOI18N
    public static final String BUILD_TEST_RESULTS_DIR = "build.test.results.dir"; // NOI18N
    public static final String BUILD_CLASSES_EXCLUDES = "build.classes.excludes"; // NOI18N
    public static final String DIST_JAVADOC_DIR = "dist.javadoc.dir"; // NOI18N
    public static final String NO_DEPENDENCIES="no.dependencies"; // NOI18N
    public static final String DEBUG_TEST_CLASSPATH = "debug.test.classpath"; // NOI18N
    
    
    public static final String JAVADOC_PRIVATE="javadoc.private"; // NOI18N
    public static final String JAVADOC_NO_TREE="javadoc.notree"; // NOI18N
    public static final String JAVADOC_USE="javadoc.use"; // NOI18N
    public static final String JAVADOC_NO_NAVBAR="javadoc.nonavbar"; // NOI18N
    public static final String JAVADOC_NO_INDEX="javadoc.noindex"; // NOI18N
    public static final String JAVADOC_SPLIT_INDEX="javadoc.splitindex"; // NOI18N
    public static final String JAVADOC_AUTHOR="javadoc.author"; // NOI18N
    public static final String JAVADOC_VERSION="javadoc.version"; // NOI18N
    public static final String JAVADOC_WINDOW_TITLE="javadoc.windowtitle"; // NOI18N
    public static final String JAVADOC_ENCODING="javadoc.encoding"; // NOI18N
    public static final String JAVADOC_ADDITIONALPARAM="javadoc.additionalparam"; // NOI18N

    public static final String TEST_BASE_DIR = "test.dir"; // NOI18N   
    public static final String TEST_BASE_DIR_VALUE = "test"; // NOI18N
    
    public static final String TEST_DIR = "test.src.dir"; // NOI18N   
    public static final String TEST_DIR_VALUE = "test/java"; // NOI18N
    
    public static final String SRC_DIR = "src.dir"; // NOI18N
    public static final String SRC_DIR_VALUE = "src/java"; // NOI18N
    
    public static final String CONF_DIR = "conf.dir"; // NOI18N
    public static final String CONF_DIR_VALUE = "src/conf"; // NOI18N
    
    public static final String BUILD_TEST_DIR = "build.test.dir"; // NOI18N
    public static final String BUILD_TEST_DIR_VALUE = "${" + BUILD_DIR + "}/" + "test"; // NOI18N
    
    public static final String BUILD_PREDIST_DIR = "build.predist.dir"; // NOI18N
    public static final String BUILD_PREDIST_DIR_VALUE = "${" + BUILD_DIR + "}/" + "predist"; // NOI18N
    
    public static final String BUILD_PREDIST_LIB_DIR = "build.predist.lib.dir"; // NOI18N
    public static final String BUILD_PREDIST_LIB_DIR_VALUE = "${" + BUILD_PREDIST_DIR + "}/" + "lib"; // NOI18N
    
    public static final String BUILD_PREDIST_JAR = "build.predist.jar"; // NOI18N
    public static final String BUILD_PREDIST_JAR_VALUE = "${" + BUILD_PREDIST_DIR + "}/" + "component.jar"; // NOI18N
        
    public static final String AS_INSTANCE_ID = "jbi.as.instance.id"; // NOI18N
    public static final String AS_HOME = "jbi.as.home"; // NOI18N
    public static final String JBI_HOME = "jbi.home"; // NOI18N
    public static final String JBI_HOST = "jbi.host"; // NOI18N
    public static final String JBI_PORT = "jbi.port"; // NOI18N
    public static final String JBI_USERNAME = "jbi.username"; // NOI18N
    public static final String JBI_PASSWORD = "jbi.password"; // NOI18N    
    
    public static final String JBI_ANT_TASKS_CLASSPATH = "jbi.ant.tasks.classpath"; // NOI18N  
    public static final String JBI_ANT_TASKS_JEE5SDK_CLASSPATH_VALUE = "${jbi.as.home}/lib/ant/lib/jbi-ant-tasks.jar"; // NOI18N
    public static final String JBI_ANT_TASKS_GLASSFISH_V2_CLASSPATH_VALUE = "${jbi.as.home}/jbi/lib/jbi-ant-tasks.jar"; // NOI18N
    public static final String GLASSFISH_ANT_TASKS_CLASSPATH_VALUE = "${jbi.as.home}/lib/sun-appserv-ant.jar"; // NOI18N  

    public static final String JBI_LIB_CLASSPATH = "libs.jbi.classpath"; // NOI18N  
    public static final String JBI_LIB_CLASSPATH_VAR = "${" + JBI_LIB_CLASSPATH + "}"; // NOI18N  

    public static final String WSDL4J_LIB_CLASSPATH = "libs.wsdl4j.162.classpath"; // NOI18N  
    public static final String WSDL4J_LIB_CLASSPATH_VAR = "${" + WSDL4J_LIB_CLASSPATH + "}"; // NOI18N  
    
    // public static final String LIB_JAXRPC_16_CLASSPATH_VAR = "${libs.jaxrpc16.classpath}"; // NOI18N  
    public static final String LIB_JAXWS_21_CLASSPATH_VAR = "${libs.jaxws21.classpath}"; // NOI18N
        
    public static final String JBI_COMPONENT_TYPE = "jbi.component.type"; // NOI18N
    public static final String SERVICE_ENGINE_COMP_TYPE = "service-engine"; // NOI18N
    public static final String BINDING_COMPONENT_COMP_TYPE = "binding-component"; // NOI18N    
    public static final String JBI_COMPONENT_NAME = "jbi.component.name"; // NOI18N
    public static final String JBI_COMPONENT_DESC = "jbi.component.description"; // NOI18N
    public static final String JBI_COMPONENT_CLASS = "jbi.component.class"; // NOI18N
    public static final String JBI_COMPONENT_BT_CLASS = "jbi.component.bootstrap.class"; // NOI18N
    
    public static final String JBI_INSTALL_WITH_PARAMS = "jbi.install.with.params"; //NOI18N
    public static final String JBI_INSTALL_WITH_PARAMS_VALUE = "false"; //NOI18N
    public static final String JBI_INSTALL_PARAMS_FILE = "jbi.install.params.file"; //NOI18N
    public static final String JBI_INSTALL_PARAMS_FILE_VALUE = "install-params.properties"; //NOI18N
    
    public static final String JBI_SOAP_BC_NAME = "jbi.soap.binding.name"; // NOI18N
    public static final String JBI_SOAP_BC_NAME_VALUE = "sun-http-binding"; // NOI18N    
    public static final String TEST_SA_ENABLED = "testSA.enabled"; // NOI18N
    public static final String TEST_SA_ENABLED_VALUE = "true"; // NOI18N
    public static final String TEST_SA_NAME = "testSA.name"; // NOI18N
    public static final String TEST_SA_NAME_SUFFIX = "_TestSA"; // NOI18N
    
    public static final String PROJECT_TEST_SA = "project.testSA"; // NOI18N
    public static final String PROJECT_TEST_SA_VALUE = "test/testSA"; // NOI18N

    public static final String PROJECT_TEST_SA_BUILD_DIR = "project.testSA.build.dir"; // NOI18N
    public static final String PROJECT_TEST_SA_BUILD_DIR_VALUE = "${" + BUILD_TEST_DIR + "}" + "/" + "${" + TEST_SA_NAME + "}" ; // NOI18N
    
    public static final String REF_TEST_SA_ZIP = "reference.testSA.zip"; // NOI18N
    public static final String REF_TEST_SA_ZIP_VALUE = "${" + PROJECT_TEST_SA_BUILD_DIR + "}" + "/dist/" + "${" + TEST_SA_NAME + "}.zip" ; // NOI18N
    
//    public static final String OLD_JBI_TEST_JUNIT_SRC_DIR = "test-sys-prop.junit.src.dir"; //NOI18N
//    public static final String OLD_JBI_TEST_JUNIT_SRC_DIR_VALUE = "${basedir}/${test.src.dir}"; //NOI18N
//    public static final String OLD_JBI_TEST_JUNIT_RESULTS_DIR = "test-sys-prop.junit.results.dir"; //NOI18N
//    public static final String OLD_JBI_TEST_JUNIT_RESULTS_DIR_VALUE = "${build.test.results.dir}"; //NOI18N
    
    public static final String TEST_SYS_PROP_TEST_SRC_DIR = "test-sys-prop.test.src.dir"; //NOI18N
    public static final String TEST_SYS_PROP_TEST_SRC_DIR_VALUE = "${basedir}/${test.src.dir}"; //NOI18N
    public static final String TEST_SYS_PROP_TEST_RESULTS_DIR = "test-sys-prop.test.results.dir"; //NOI18N
    public static final String TEST_SYS_PROP_TEST_RESULTS_DIR_VALUE = "${build.test.results.dir}"; //NOI18N
    
        
//    public static final String JBI_API_JAR = "jbi.jar"; // NOI18N
    
    public static final String PROJECT_JBI_DEPLOY_PLUGIN = "project.jbi.deploy.plugin"; // NOI18N
    public static final String PROJECT_JBI_DEPLOY_PLUGIN_VALUE = "./deploy-plugin"; // NOI18N
     
    // Properties stored in the PRIVATE.PROPERTIES
    public static final String APPLICATION_ARGS = "application.args"; // NOI18N
    public static final String JAVADOC_PREVIEW="javadoc.preview"; // NOI18N

    public static final String JBI_COMP_LIB_CLASSPATH = "jbi.component.lib.classpath";
    public static final String JBI_COMP_SHARED_LIB_CLASSPATH = "jbi.component.sharedlibs.classpath";
    
    // Well known paths
    public static final String[] WELL_KNOWN_PATHS = new String[] {            
            "${" + JAVAC_CLASSPATH + "}", 
            "${" + JAVAC_TEST_CLASSPATH  + "}", 
            "${" + RUN_CLASSPATH  + "}", 
            "${" + RUN_TEST_CLASSPATH  + "}", 
            "${" + BUILD_CLASSES_DIR  + "}", 
            "${" + BUILD_TEST_CLASSES_DIR  + "}", 
            "${" + JBI_COMP_SHARED_LIB_CLASSPATH  + "}"
    };
    
    // Prefixes and suffixes of classpath
    public static final String LIBRARY_PREFIX = "${libs."; // NOI18N
    public static final String LIBRARY_SUFFIX = ".classpath}"; // NOI18N
    // XXX looks like there is some kind of API missing in ReferenceHelper?
    public static final String ANT_ARTIFACT_PREFIX = "${reference."; // NOI18N
    
    public static final String JBI_SLIB_PREFIX="jbi.slib.";
    public static final String JBI_SLIB_SUFFIX=".classpath";
            
    // jbi.slib.xyz.classpath
    // jbi.component.sharedlibs.classpath=${jbi.slib.xyz.classpath}:${jbi.slib.123.classpath}
    
    /**
     * default constructor
     */
    public JbiCompProjectProperties() {        
    }
            
    public static String getAntPropertyName( String property ) {
        
        if ( property != null && 
             property.startsWith( "${" ) && // NOI18N
             property.endsWith( "}" ) ) { // NOI18N
            return property.substring( 2, property.length() - 1 ); 
        }
        else {
            return property;
        }
    }
                    
    public static String[] getComponentClassPathElements(String runtimeJarPath, String libClassPath, String jbiLibJarPath) {
        
        String componentJar = new File(runtimeJarPath).getName();
        String libPrefix = "lib/";        
        ArrayList list = new ArrayList();
        list.add(componentJar);
        
        if ( libClassPath == null || libClassPath.trim().length() == 0 ) {
            // no libraries
            return (String[])list.toArray(new String[list.size()]);
        }        
        String jbiLibJar = new File(jbiLibJarPath).getName();
        // convert classpath to array
        String[] libJars = Util.getLibraryList(libClassPath);
        // remove jbi.jar if exists.
        for ( int i=0; i < libJars.length; ++i ) {
            if ( libJars[i].equals(jbiLibJar)) {
                continue;
            }
            list.add(libPrefix + libJars[i]);
        }
        // append this to the list and return Array
        
        return (String[])list.toArray(new String[list.size()]);
    }
        
    public static JbiComponentDescriptor getJbiXmlModelFromConfigDir(UpdateHelper updateHelper) {
        JbiComponentDescriptor compDesc = null;
        try {
            
            FileObject prjDirFO = updateHelper.getAntProjectHelper().getProjectDirectory();
            String confDir = JbiCompProjectProperties.CONF_DIR_VALUE;
            FileObject metaInfDirFO = FileUtil.createFolder(prjDirFO, confDir + "/META-INF");
            FileObject jbiXmlFO = metaInfDirFO.getFileObject("jbi.xml");                        
            if ( jbiXmlFO != null ) {
                compDesc =  JbiDescriptorFactory.getInstance().createJbiComponentDescriptor(jbiXmlFO);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return compDesc;
    }
    
    public static void loadFromJbiXml(UpdateHelper updateHelper, EditableProperties projectProps) {
        try {
            
            FileObject prjDirFO = updateHelper.getAntProjectHelper().getProjectDirectory();
            String confDir = projectProps.getProperty(CONF_DIR);
            if ( confDir == null ) {
                confDir = JbiCompProjectProperties.CONF_DIR_VALUE;
            }
            FileObject metaInfDirFO = FileUtil.createFolder(prjDirFO, confDir + "/META-INF");
            FileObject jbiXmlFO = metaInfDirFO.getFileObject("jbi.xml");
            
            String compName = projectProps.getProperty(JBI_COMPONENT_NAME);
            String compType = projectProps.getProperty(JBI_COMPONENT_TYPE);
            
            String desc = projectProps.getProperty(JBI_COMPONENT_DESC);            
            String rtClass = projectProps.getProperty(JBI_COMPONENT_CLASS);
            String btClass = projectProps.getProperty(JBI_COMPONENT_BT_CLASS);
            
            if ( jbiXmlFO == null ) {
                HashMap tokenMap = new HashMap();
                tokenMap.put("JBI_COMP_NAME", compName);
                jbiXmlFO = Util.createJbiXmlFileFromTemplate(metaInfDirFO, compType, tokenMap );
            }
            
            if ( jbiXmlFO != null ) {
                JbiComponentDescriptor compDesc =
                    JbiDescriptorFactory.getInstance().createJbiComponentDescriptor(jbiXmlFO);
                if (compDesc != null ) {
                        projectProps.setProperty(JBI_COMPONENT_DESC, compDesc.getDescription());
                        projectProps.setProperty(JBI_COMPONENT_CLASS, compDesc.getComponentClassName());
                        projectProps.setProperty(JBI_COMPONENT_BT_CLASS, compDesc.getBootstrapClassName());                        
                }
            }            
        } catch (IOException ex) {
            ex.printStackTrace();
        }        
    }
    
    public static FileObject getJbiXmlFileObject(UpdateHelper updateHelper, EditableProperties projectProps) {
        FileObject jbiXmlFO = null;
        try {            
            FileObject prjDirFO = updateHelper.getAntProjectHelper().getProjectDirectory();
            String confDir = projectProps.getProperty(CONF_DIR);
            if ( confDir == null ) {
                confDir = JbiCompProjectProperties.CONF_DIR_VALUE;
            }
            FileObject metaInfDirFO = FileUtil.createFolder(prjDirFO, confDir + "/META-INF");
            jbiXmlFO = metaInfDirFO.getFileObject("jbi.xml");
            
            String compName = projectProps.getProperty(JBI_COMPONENT_NAME);
            String compType = projectProps.getProperty(JBI_COMPONENT_TYPE);
            
            if ( jbiXmlFO == null ) {
                HashMap tokenMap = new HashMap();
                tokenMap.put("JBI_COMP_NAME", compName);
                jbiXmlFO = Util.createJbiXmlFileFromTemplate(metaInfDirFO, compType, tokenMap );
            }
            
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return jbiXmlFO;
    }
    
    public static JbiComponentDescriptor getJbiComponentDescriptor(FileObject jbiXmlFO) {
        JbiComponentDescriptor compDesc = 
                JbiDescriptorFactory.getInstance().createJbiComponentDescriptor(jbiXmlFO);
        return compDesc;
    }
    
    public static void saveToJbiXml(UpdateHelper updateHelper, EditableProperties projectProps) {
        
        try {
            
            FileObject prjDirFO = updateHelper.getAntProjectHelper().getProjectDirectory();
            String confDir = projectProps.getProperty(CONF_DIR);
            if ( confDir == null ) {
                confDir = JbiCompProjectProperties.CONF_DIR_VALUE;
            }
            FileObject metaInfDirFO = FileUtil.createFolder(prjDirFO, confDir + "/META-INF");
            FileObject jbiXmlFO = metaInfDirFO.getFileObject("jbi.xml");
            
            String compName = projectProps.getProperty(JBI_COMPONENT_NAME);
            String compType = projectProps.getProperty(JBI_COMPONENT_TYPE);
            String desc = projectProps.getProperty(JBI_COMPONENT_DESC);
            
            String rtClass = projectProps.getProperty(JBI_COMPONENT_CLASS);
            String btClass = projectProps.getProperty(JBI_COMPONENT_BT_CLASS);
            
            if ( jbiXmlFO == null ) {
                HashMap tokenMap = new HashMap();
                tokenMap.put("JBI_COMP_NAME", compName);
                jbiXmlFO = Util.createJbiXmlFileFromTemplate(metaInfDirFO, compType, tokenMap );
            }
            
            if ( jbiXmlFO != null ) {
                JbiComponentDescriptor compDesc =
                    JbiDescriptorFactory.getInstance().createJbiComponentDescriptor(jbiXmlFO);
                if (compDesc != null ) {
                    PropertyEvaluator propEval =  updateHelper.getAntProjectHelper().getStandardPropertyEvaluator();
                    String runtimeJarPath = propEval.getProperty(BUILD_PREDIST_JAR);
                    String libClassPath = propEval.getProperty(JAVAC_CLASSPATH);
                    String jbiJarPath = propEval.getProperty(JBI_LIB_CLASSPATH);
                    String[] compClassPaths = getComponentClassPathElements(runtimeJarPath, libClassPath, jbiJarPath);
                    
                    compDesc.setName(compName);
                    compDesc.setDescription(desc);
                    
                    compDesc.setComponentClassName(rtClass);
                    compDesc.setBootstrapClassName(btClass);
                    
                    compDesc.setComponentClassPath(compClassPaths);
                    compDesc.setBootstrapClassPath(compClassPaths);
                    
                    JbiDescriptorFactory.getInstance().saveJbiDescriptor(compDesc, jbiXmlFO);
                } else {
                    throw new IOException("Invalid jbi.xml: Empty or invalid xml in jbi.xml" );
                }
            }            
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
    /**
     * will set the value to empty string if the value is null;
     */
    public static void setEditableProperty(EditableProperties ep, String name, String value) {
        String nonNullValue =  (value != null) ? value : "";
        ep.setProperty(name, nonNullValue);
    }
    
    public static void savePropertiesToFile(String propFilePath, Properties props) {
        if ( props == null || propFilePath == null ) {
            return ;
        }
        
        File propsFile = new File(propFilePath);   
        FileOutputStream os = null;
        try {
            os = new FileOutputStream(propsFile);
            props.store(os, "");
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            if ( os != null ) {
                try {
                    os.close();
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }
        }
    }
    
    public static void updateJbiAdminProperties(JbiAdminSettings jbiSettings, EditableProperties ep) {
        
       setEditableProperty(ep, JbiCompProjectProperties.AS_INSTANCE_ID,  jbiSettings.getAppServerId()); // NOI18N
        
       setEditableProperty(ep, JbiCompProjectProperties.AS_HOME,
                jbiSettings.getAppServerHome()); // NOI18N
        
//        setEditableProperty(ep, JbiCompProjectProperties.JBI_HOME, jbiSettings.getJbiHome()); // NOI18N
        
        setEditableProperty(ep, JbiCompProjectProperties.JBI_HOST, jbiSettings.getHost()); // NOI18N
        
        setEditableProperty(ep, JbiCompProjectProperties.JBI_PORT, jbiSettings.getPort()); // NOI18N
        
        setEditableProperty(ep, JbiCompProjectProperties.JBI_USERNAME, jbiSettings.getUsername()); // NOI18N
        
        setEditableProperty(ep, JbiCompProjectProperties.JBI_PASSWORD, jbiSettings.getPassword()); // NOI18N  
        
        // set the ant tasks classpath for default, javaeesdk u1,2 or jbi in glassfish v2 or javaeesdk u3
        if ( jbiSettings.isOpenESB2x()) {
            ep.setProperty(JbiCompProjectProperties.JBI_ANT_TASKS_CLASSPATH, new String[] { // NOI18N
                JbiCompProjectProperties.GLASSFISH_ANT_TASKS_CLASSPATH_VALUE +":", // NOI18N
                JbiCompProjectProperties.JBI_ANT_TASKS_GLASSFISH_V2_CLASSPATH_VALUE
            });
            System.out.println("Setting jbi ant tasks classpath for OpenESB2x");
        } else if ( jbiSettings.UNKNOWN_SERVER_ID.equalsIgnoreCase(jbiSettings.getAppServerId()) ) {
            // default settings
            ep.setProperty(JbiCompProjectProperties.JBI_ANT_TASKS_CLASSPATH, new String[] { // NOI18N
                JbiCompProjectProperties.GLASSFISH_ANT_TASKS_CLASSPATH_VALUE +":", // NOI18N
                JbiCompProjectProperties.JBI_ANT_TASKS_GLASSFISH_V2_CLASSPATH_VALUE +":", // NOI18N
                JbiCompProjectProperties.JBI_ANT_TASKS_JEE5SDK_CLASSPATH_VALUE // NOI18N
            });
            System.out.println("Setting jbi ant tasks classpath for UNKNOWN Server");
        } else {
            //javaeesdk u1, u2 settings
            ep.setProperty(JbiCompProjectProperties.JBI_ANT_TASKS_CLASSPATH, new String[] { // NOI18N
                JbiCompProjectProperties.JBI_ANT_TASKS_JEE5SDK_CLASSPATH_VALUE // NOI18N
            });
            System.out.println("Setting jbi ant tasks classpath for javaeesdk u1 u2");
        }        
    }

    public static boolean fixJbiProjectProperties(EditableProperties ep) {
        
        boolean fixed = false;
        
        if ( !SRC_DIR_VALUE.equals(ep.getProperty(SRC_DIR)) ) {
            ep.setProperty(SRC_DIR, SRC_DIR_VALUE);
            fixed = true;
        }
        
        if ( !CONF_DIR_VALUE.equals(ep.getProperty(CONF_DIR)) ) {
            ep.setProperty(CONF_DIR, CONF_DIR_VALUE);
            fixed = true;
        }
        
        if ( !TEST_DIR_VALUE.equals(ep.getProperty(TEST_DIR)) ) {
            ep.setProperty(TEST_DIR, TEST_DIR_VALUE);
            fixed = true;
        }
        
        ep.setProperty(BUILD_TEST_DIR, BUILD_TEST_DIR_VALUE);
        
        ep.setProperty(BUILD_PREDIST_DIR, BUILD_PREDIST_DIR_VALUE); 
        ep.setProperty(BUILD_PREDIST_LIB_DIR, BUILD_PREDIST_LIB_DIR_VALUE);
        ep.setProperty(BUILD_PREDIST_JAR, BUILD_PREDIST_JAR_VALUE);
        
       // ep.setProperty(REF_TEST_SA_ZIP,  "${build.dir}/test/testSA/dist/testSA.zip" );  
        
        if (ep.getProperty(JBI_INSTALL_WITH_PARAMS) == null ) {
            ep.setProperty(JBI_INSTALL_WITH_PARAMS, JBI_INSTALL_WITH_PARAMS_VALUE);
        }
        ep.setProperty(JBI_INSTALL_PARAMS_FILE, JBI_INSTALL_PARAMS_FILE_VALUE);
        
        if ( ep.getProperty(JBI_COMP_SHARED_LIB_CLASSPATH) == null) {
            ep.setProperty(JBI_COMP_SHARED_LIB_CLASSPATH, "");
        }
        
        if ( ep.getProperty(JBI_COMP_LIB_CLASSPATH) == null) {
            String javacClassPath = ep.getProperty(JAVAC_CLASSPATH);
            ep.setProperty(JAVAC_CLASSPATH, 
                new String[] { 
                    JBI_LIB_CLASSPATH_VAR + ":",
                    "${" + JBI_COMP_SHARED_LIB_CLASSPATH + "}:",
                    "${" + JBI_COMP_LIB_CLASSPATH + "}"
                });
            // filter the javacClassPath for jbi.lib.classpath
            String[] paths = PropertyUtils.tokenizePath(javacClassPath);
            List<String> pathList = new ArrayList<String>();
            for (String path : paths ) {
                if (!JBI_LIB_CLASSPATH_VAR.equals(path)) {
                    pathList.add(path + ":");
                }
            }
            ep.setProperty(JBI_COMP_LIB_CLASSPATH, pathList.toArray(new String[0]));            
        }
        
        return fixed;
    }
            
    public static void updateJbiSpecificProperties(EditableProperties ep, 
            String projectName, String componentName, String componentType) {
        
        ep.setProperty(JBI_COMPONENT_TYPE, componentType);
        
        ep.setProperty(JAVAC_CLASSPATH, 
                new String[] { 
            JBI_LIB_CLASSPATH_VAR + ":",
            "${" + JBI_COMP_SHARED_LIB_CLASSPATH + "}:",
            "${" + JBI_COMP_LIB_CLASSPATH + "}"
        });
        
        ep.setProperty("javac.test.classpath", new String[] { // NOI18N
            "${javac.classpath}:", // NOI18N
            "${build.classes.dir}:", // NOI18N
            "${libs.junit.classpath}:", // NOI18N
            LIB_JAXWS_21_CLASSPATH_VAR
        });
        
        ep.setProperty(JBI_COMP_LIB_CLASSPATH, 
            new String[] { 
                WSDL4J_LIB_CLASSPATH_VAR
            });
        
        
        if ( ep.getProperty(JBI_COMP_SHARED_LIB_CLASSPATH) == null) {
            ep.setProperty(JBI_COMP_SHARED_LIB_CLASSPATH, "");
        }
        
        ep.setProperty(CONF_DIR, CONF_DIR_VALUE); // NOI18N
        
        ep.setProperty(BUILD_TEST_DIR, BUILD_TEST_DIR_VALUE);
        
        ep.setProperty(BUILD_PREDIST_DIR, BUILD_PREDIST_DIR_VALUE); 
        ep.setProperty(BUILD_PREDIST_LIB_DIR, BUILD_PREDIST_LIB_DIR_VALUE);
        ep.setProperty(BUILD_PREDIST_JAR, BUILD_PREDIST_JAR_VALUE);
        
        ep.setProperty(DIST_JAR,
            "${" + DIST_DIR + "}/" +
            PropertyUtils.getUsablePropertyName(projectName) + ".zip"); // NOI18N
        
        ep.setProperty(JBI_COMPONENT_NAME,
                PropertyUtils.getUsablePropertyName(componentName)); 
        
//        ep.setProperty(JBI_COMPONENT_DESC, compDesc); 
//        ep.setProperty(JBI_COMPONENT_CLASS, compClass); 
//        ep.setProperty(JBI_COMPONENT_BT_CLASS, btClass); 
        
        ep.setProperty(JBI_INSTALL_WITH_PARAMS, JBI_INSTALL_WITH_PARAMS_VALUE);
        ep.setProperty(JBI_INSTALL_PARAMS_FILE, JBI_INSTALL_PARAMS_FILE_VALUE);
        
        ep.setProperty(JBI_SOAP_BC_NAME,  JBI_SOAP_BC_NAME_VALUE );
        
        //// if ( SERVICE_ENGINE_COMP_TYPE.equalsIgnoreCase(componentType)) {
            ep.setProperty(TEST_SA_ENABLED,  TEST_SA_ENABLED_VALUE );
        //// }
        
        ep.setProperty(TEST_SA_NAME,  componentName + TEST_SA_NAME_SUFFIX ); 
        
        ep.setProperty(PROJECT_TEST_SA,  PROJECT_TEST_SA_VALUE ); 
        // ep.setProperty(PROJECT_TEST_SA_BUILD_DIR,  PROJECT_TEST_SA_BUILD_DIR_VALUE );
        // ep.setProperty(REF_TEST_SA_ZIP,  REF_TEST_SA_ZIP_VALUE ); 
    
        ep.setProperty(TEST_SYS_PROP_TEST_SRC_DIR,  TEST_SYS_PROP_TEST_SRC_DIR ); 
        ep.setProperty(TEST_SYS_PROP_TEST_RESULTS_DIR,  TEST_SYS_PROP_TEST_RESULTS_DIR_VALUE ); 
        
    }
    
    
}
