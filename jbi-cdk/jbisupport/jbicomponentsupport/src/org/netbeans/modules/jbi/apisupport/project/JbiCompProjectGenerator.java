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
import java.io.IOException;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.modules.jbi.apisupport.JbiAdminSettings;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.ProjectGenerator;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.modules.SpecificationVersion;
import org.openide.util.NbBundle;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import java.util.List;

/**
 * Creates a JbiCompProject for developing the jbi service engine or binding component.
 * @author chikkala
 */
public class JbiCompProjectGenerator {
    static final String MINIMUM_ANT_VERSION = "1.6.5";    
    
    private File mPrjDir;
    private String mPrjName;
    private List mPrjRefList;
    private String mCompType = JbiCompProjectProperties.SERVICE_ENGINE_COMP_TYPE;
    private String mCompName;
    private String mCompDesc;
    private String mServerID;
    
    private JbiCompProjectGenerator() {}
    
    public JbiCompProjectGenerator(File prjDir, String prjName, String compType, String compName) {
        this.mPrjDir = prjDir;
        this.mPrjName = prjName;
        this.mCompType = compType;
        this.mCompName = compName;
    }
    
    
    public AntProjectHelper createProject() throws IOException {
        
        AntProjectHelper prjHelper = null;
        
        FileObject prjDirFO = createProjectDir(this.getProjectDirectory());
        
        prjHelper = ProjectGenerator.createProject(prjDirFO, JbiCompProjectType.TYPE);
        
        createPrimaryConfigurationData(prjHelper);
        createProjectProperties(prjHelper);
        createProjectPrivateProperties(prjHelper);
        
        FileObject srcFolder = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.SRC_DIR_VALUE); // NOI18N
        FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.TEST_DIR_VALUE); // NOI18N
        
        Project p = ProjectManager.getDefault().findProject(prjDirFO);
        ProjectManager.getDefault().saveProject(p);
        
        return prjHelper;
    }
    
    
    public File getProjectDirectory() {
        return this.mPrjDir;
    }
    
    public String getProjectName() {
        return this.mPrjName;
    }
    
    public List getProjectReferences() {
        return this.mPrjRefList;
    }
    
    public void setProjectReferences(List prjRefList) {
        this.mPrjRefList = prjRefList;
    }
    
    public String getTargetServerID() {
        return this.mServerID;
    }
    
    public void setTargetServerID(String serverID) {
        this.mServerID = serverID;
    }
    
    public String getComponentType() {
        return this.mCompType;
    }
    
    public String getComponentName() {
        return this.mCompName;
    }
    
    public String getComponentDescription() {
        return this.mCompDesc;
    }
    
    public void setComponentDescription(String desc) {
        this.mCompDesc = desc;
    }
    
    private void createPrimaryConfigurationData(AntProjectHelper prjHelper) {
        
        Element data = prjHelper.getPrimaryConfigurationData(true);
        Document doc = data.getOwnerDocument();
        
        Element nameEl = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, "name"); // NOI18N
        nameEl.appendChild(doc.createTextNode(this.getProjectName()));
        data.appendChild(nameEl);
        
        Element compTypeEl = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, JbiCompProjectType.COMP_TYPE_EL); // NOI18N
        compTypeEl.appendChild(doc.createTextNode(this.getComponentType()));
        data.appendChild(compTypeEl);
        
        Element minant = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, "minimum-ant-version"); // NOI18N
        minant.appendChild(doc.createTextNode(MINIMUM_ANT_VERSION)); // NOI18N
        data.appendChild(minant);
        
        Element sourceRoots = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,"source-roots");  //NOI18N
        Element srcRoot = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,"root");   //NOI18N
        srcRoot.setAttribute("id", JbiCompProjectProperties.SRC_DIR);   //NOI18N
        sourceRoots.appendChild(srcRoot);
        data.appendChild(sourceRoots);
        
        Element testRoots = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,"test-roots");  //NOI18N
        Element testRoot = doc.createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE,"root");   //NOI18N
        testRoot.setAttribute("id", JbiCompProjectProperties.TEST_DIR);   //NOI18N
        testRoots.appendChild(testRoot);
        data.appendChild(testRoots);
        
        prjHelper.putPrimaryConfigurationData(data, true);
    }
    
    private void createProjectProperties(AntProjectHelper prjHelper ) {
        
        EditableProperties ep = prjHelper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        
        ep.setProperty(JbiCompProjectProperties.SRC_DIR, JbiCompProjectProperties.SRC_DIR_VALUE); // NOI18N
        ep.setProperty(JbiCompProjectProperties.TEST_DIR, JbiCompProjectProperties.TEST_DIR_VALUE); // NOI18N
        
        ep.setProperty("dist.dir", "dist"); // NOI18N
        ep.setComment("dist.dir", new String[] {"# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_dist.dir")}, false); // NOI18N
        ep.setProperty("dist.jar", "${dist.dir}/" + PropertyUtils.getUsablePropertyName(this.getProjectName()) + ".jar"); // NOI18N
        ep.setProperty("javac.classpath", new String[0]); // NOI18N
        ep.setProperty("build.sysclasspath", "ignore"); // NOI18N
        ep.setComment("build.sysclasspath", new String[] {"# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_build.sysclasspath")}, false); // NOI18N
        ep.setProperty("run.classpath", new String[] { // NOI18N
            "${javac.classpath}:", // NOI18N
            "${build.classes.dir}", // NOI18N
        });
        ep.setProperty("debug.classpath", new String[] { // NOI18N
            "${run.classpath}", // NOI18N
        });
        ep.setProperty("jar.compress", "false"); // NOI18N
        
        ep.setProperty("javac.compilerargs", ""); // NOI18N
        ep.setComment("javac.compilerargs", new String[] {
            "# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_javac.compilerargs"), // NOI18N
        }, false);
        SpecificationVersion sourceLevel = getDefaultSourceLevel();
        ep.setProperty("javac.source", sourceLevel.toString()); // NOI18N
        ep.setProperty("javac.target", sourceLevel.toString()); // NOI18N
        ep.setProperty("javac.deprecation", "false"); // NOI18N
        ep.setProperty("javac.test.classpath", new String[] { // NOI18N
            "${javac.classpath}:", // NOI18N
            "${build.classes.dir}:", // NOI18N
            "${libs.junit.classpath}", // NOI18N
        });
        ep.setProperty("run.test.classpath", new String[] { // NOI18N
            "${javac.test.classpath}:", // NOI18N
            "${build.test.classes.dir}", // NOI18N
        });
        ep.setProperty("debug.test.classpath", new String[] { // NOI18N
            "${run.test.classpath}", // NOI18N
        });
        
        ep.setProperty("build.generated.dir", "${build.dir}/generated"); // NOI18N
        ep.setProperty("meta.inf.dir", "${conf.dir}/META-INF"); // NOI18N
        
        ep.setProperty("build.dir", "build"); // NOI18N
        ep.setComment("build.dir", new String[] {"# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_build.dir")}, false); // NOI18N
        ep.setProperty("build.classes.dir", "${build.dir}/classes"); // NOI18N
        ep.setProperty("build.test.dir", "${build.dir}/test"); // NOI18N
        ep.setProperty("build.test.classes.dir", "${build.dir}/test/classes"); // NOI18N
        ep.setProperty("build.test.results.dir", "${build.dir}/test/results"); // NOI18N
        ep.setProperty("build.classes.excludes", "**/*.java,**/*.form"); // NOI18N
        ep.setProperty("dist.javadoc.dir", "${dist.dir}/javadoc"); // NOI18N
        ep.setProperty("platform.active", "default_platform"); // NOI18N
        
        ep.setProperty("run.jvmargs", ""); // NOI18N
        ep.setComment("run.jvmargs", new String[] {
            "# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_run.jvmargs"), // NOI18N
            "# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_run.jvmargs_2"), // NOI18N
            "# " + NbBundle.getMessage(JbiCompProjectGenerator.class, "COMMENT_run.jvmargs_3"), // NOI18N
        }, false);
        
        ep.setProperty(JbiCompProjectProperties.JAVADOC_PRIVATE, "false"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_NO_TREE, "false"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_USE, "true"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_NO_NAVBAR, "false"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_NO_INDEX, "false"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_SPLIT_INDEX, "true"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_AUTHOR, "false"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_VERSION, "false"); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_WINDOW_TITLE, ""); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_ENCODING, ""); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_ADDITIONALPARAM, ""); // NOI18N
        
        // create jbi specific properties
        createJbiProperties(ep, prjHelper);
        
        // create jbi shared library classpath properties
        // ep.setProperty(JbiCompProjectProperties.JBI_COMP_LIB_CLASSPATH, ""); // NOI18N
        // ep.setProperty(JbiCompProjectProperties.JBI_COMP_SHARED_LIB_CLASSPATH, ""); // NOI18N
        
        prjHelper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, ep);
        
    }
    
    private void createProjectPrivateProperties(AntProjectHelper prjHelper) {
        
        EditableProperties ep = prjHelper.getProperties(AntProjectHelper.PRIVATE_PROPERTIES_PATH);
        
        //TODO add the jbi.install.params.file path here
        // ep.setProperty("application.args", ""); // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVAC_DEBUG, "true");  // NOI18N
        ep.setProperty(JbiCompProjectProperties.JAVADOC_PREVIEW, "true"); // NOI18N
        
        prjHelper.putProperties(AntProjectHelper.PRIVATE_PROPERTIES_PATH, ep);
    }
    
    private void createJbiProperties(EditableProperties ep, AntProjectHelper prjHelper) {
        
        String projectName = this.getProjectName();
        String compName = this.getComponentName();
        String compType = this.getComponentType();
        String targetServerID = this.mServerID;
        
        JbiCompProjectProperties.updateJbiSpecificProperties(ep, projectName, compName, compType);
        JbiAdminSettings jbiAdminSettings = null;
        JbiAdminSettings[] list = JbiAdminSettings.JbiAdminSettingsImpl.getAllJbiAdminSettings();
        
        for ( int i=0; i < list.length; ++i) {
            String serverID = list[i].getAppServerId();
            if (serverID.equals(targetServerID)) {
                jbiAdminSettings = list[i];
                break;
            }
        }
        if ( jbiAdminSettings == null ) {
            jbiAdminSettings = JbiAdminSettings.JbiAdminSettingsImpl.getJbiAdminSettings();
        }
        JbiCompProjectProperties.updateJbiAdminProperties(jbiAdminSettings, ep);
        
    }
    
    private static FileObject createProjectDir(File dir) throws IOException {
        FileObject dirFO;
        if(!dir.exists()) {
            //Refresh before mkdir not to depend on window focus, refreshFileSystem does not work correctly
            refreshFolder(dir);
            if (!dir.mkdirs()) {
                throw new IOException("Can not create project folder.");   //NOI18N
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
    
    private static SpecificationVersion getDefaultSourceLevel() {
        if (defaultSourceLevel != null) {
            return defaultSourceLevel;
        } else {
            JavaPlatform defaultPlatform = JavaPlatformManager.getDefault().getDefaultPlatform();
            return defaultPlatform.getSpecification().getVersion();
        }
    }
    
    /**
     * Unit test only method. Sets the default source level for tests
     * where the default platform is not available.
     * @param version the default source level set to project when it is created
     *
     */
    public static void setDefaultSourceLevel(SpecificationVersion version) {
        defaultSourceLevel = version;
    }
    
}


