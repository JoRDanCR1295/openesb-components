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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.classpath.GlobalPathRegistry;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ant.AntArtifact;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.netbeans.modules.jbi.apisupport.project.classpath.ClassPathProviderImpl;
import org.netbeans.modules.jbi.apisupport.project.classpath.JbiCompProjectClassPathExtender;
import org.netbeans.modules.jbi.apisupport.project.queries.CompiledSourceForBinaryQuery;
import org.netbeans.modules.jbi.apisupport.project.queries.JavadocForBinaryQueryImpl;
import org.netbeans.modules.jbi.apisupport.project.queries.SourceLevelQueryImpl;
import org.netbeans.modules.jbi.apisupport.project.queries.UnitTestForSourceQueryImpl;
import org.netbeans.modules.jbi.apisupport.project.ui.JbiCompLogicalViewProvider;
import org.netbeans.modules.jbi.apisupport.project.ui.customizer.CustomizerProviderImpl;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.spi.java.project.support.ui.BrokenReferencesSupport;
import org.netbeans.spi.project.AuxiliaryConfiguration;
import org.netbeans.spi.project.SubprojectProvider;
import org.netbeans.spi.project.ant.AntArtifactProvider;
import org.netbeans.spi.project.support.ant.AntProjectEvent;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.AntProjectListener;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.GeneratedFilesHelper;
import org.netbeans.spi.project.support.ant.ProjectXmlSavedHook;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.netbeans.spi.project.ui.PrivilegedTemplates;
import org.netbeans.spi.project.ui.ProjectOpenedHook;
import org.netbeans.spi.project.ui.RecommendedTemplates;
import org.openide.ErrorManager;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Lookup;
import org.openide.util.Mutex;
import org.openide.util.Utilities;
import org.openide.util.lookup.Lookups;
import org.w3c.dom.Element;
import org.w3c.dom.Text;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Jbi Component project class
 * @author chikkala, j2seproject
 */
public final class JbiCompProject implements Project, AntProjectListener {
    
    private static final Icon JBI_PROJECT_ICON =
            new ImageIcon(
            Utilities.loadImage("org/netbeans/modules/jbi/apisupport/resources/images/JBI.png")); // NOI18N
    
    private static final Icon SE_PROJECT_ICON =
            new ImageIcon(
            Utilities.loadImage("org/netbeans/modules/jbi/apisupport/resources/images/ServiceEngine.png")); // NOI18N

    private static final Icon BC_PROJECT_ICON =
            new ImageIcon(
            Utilities.loadImage("org/netbeans/modules/jbi/apisupport/resources/images/BindingComponent.png")); // NOI18N
    
    
    private final AuxiliaryConfiguration aux;
    private final AntProjectHelper helper;
    private final PropertyEvaluator eval;
    private final ReferenceHelper refHelper;
    private final GeneratedFilesHelper genFilesHelper;
    private final Lookup lookup;
    private final UpdateHelper updateHelper;
////CHK    private MainClassUpdater mainClassUpdater;
    private SourceRoots sourceRoots;
    private SourceRoots testRoots;
        
    public JbiCompProject(AntProjectHelper helper) throws IOException {
        this.helper = helper;
        eval = createEvaluator();
        aux = helper.createAuxiliaryConfiguration();
        refHelper = new ReferenceHelper(helper, aux, eval);
        genFilesHelper = new GeneratedFilesHelper(helper);
        this.updateHelper = new UpdateHelper(this, this.helper, this.aux, this.genFilesHelper,
                UpdateHelper.createDefaultNotifier());
        
        lookup = createLookup(aux);
        helper.addAntProjectListener(this);
    }
    
    /**
     * Returns the project directory
     * @return the directory the project is located in
     */
    public FileObject getProjectDirectory() {
        return helper.getProjectDirectory();
    }
    
    public String toString() {
        return "JbiCompProject[" + getProjectDirectory() + "]"; // NOI18N
    }
    
    private PropertyEvaluator createEvaluator() {
        // XXX might need to use a custom evaluator to handle active platform substitutions... TBD
        // It is currently safe to not use the UpdateHelper for PropertyEvaluator; UH.getProperties() delegates to APH
        return helper.getStandardPropertyEvaluator();
    }
    
    PropertyEvaluator evaluator() {
        return eval;
    }
    
    ReferenceHelper getReferenceHelper() {
        return this.refHelper;
    }
    
    UpdateHelper getUpdateHelper() {
        return this.updateHelper;
    }
    
    public Lookup getLookup() {
        return lookup;
    }
    
    public AntProjectHelper getAntProjectHelper() {
        return helper;
    }
    
    private Lookup createLookup(AuxiliaryConfiguration aux) {
        SubprojectProvider spp = refHelper.createSubprojectProvider();
        return Lookups.fixed(new Object[] {
            new Info(),
            aux,
            helper.createCacheDirectoryProvider(),
            spp,
            new JbiCompActionProvider( this, this.updateHelper ),
            new JbiCompLogicalViewProvider(this, this.updateHelper, evaluator(), spp, refHelper),
            new CustomizerProviderImpl(this, this.updateHelper, evaluator(), refHelper, this.genFilesHelper),
            new ClassPathProviderImpl(this.helper, evaluator(), getSourceRoots(),getTestSourceRoots()), //Does not use APH to get/put properties/cfgdata
            new CompiledSourceForBinaryQuery(this.helper, evaluator(),getSourceRoots(),getTestSourceRoots()), //Does not use APH to get/put properties/cfgdata
            new JavadocForBinaryQueryImpl(this.helper, evaluator()), //Does not use APH to get/put properties/cfgdata
            new AntArtifactProviderImpl(),
            new ProjectXmlSavedHookImpl(),
            new ProjectOpenedHookImpl(),
            new UnitTestForSourceQueryImpl(getSourceRoots(),getTestSourceRoots()),
            new SourceLevelQueryImpl(evaluator()),
            new JbiCompSources(this.helper, evaluator(), getSourceRoots(), getTestSourceRoots()),
            new JbiCompSharabilityQuery(this.helper, evaluator(), getSourceRoots(), getTestSourceRoots()), //Does not use APH to get/put properties/cfgdata
            new JbiCompFileBuiltQuery(this.helper, evaluator(),getSourceRoots(),getTestSourceRoots()), //Does not use APH to get/put properties/cfgdata
            new RecommendedTemplatesImpl(this.updateHelper),
            new JbiCompProjectClassPathExtender(this, this.updateHelper, eval,refHelper),
            this, // never cast an externally obtained Project to JbiCompProject - use lookup instead
            new JbiCompProjectOperations(this)
        });
    }
    
    public void configurationXmlChanged(AntProjectEvent ev) {
        if (ev.getPath().equals(AntProjectHelper.PROJECT_XML_PATH)) {
            // Could be various kinds of changes, but name & displayName might have changed.
            Info info = (Info)getLookup().lookup(ProjectInformation.class);
            info.firePropertyChange(ProjectInformation.PROP_NAME);
            info.firePropertyChange(ProjectInformation.PROP_DISPLAY_NAME);
        }
    }
    
    public void propertiesChanged(AntProjectEvent ev) {
        // currently ignored (probably better to listen to evaluator() if you need to)
    }
    
    // Package private methods -------------------------------------------------
    
    /**
     * Returns the source roots of this project
     * @return project's source roots
     */
    public synchronized SourceRoots getSourceRoots() {
        if (this.sourceRoots == null) { //Local caching, no project metadata access
            this.sourceRoots = new SourceRoots(this.updateHelper, evaluator(), getReferenceHelper(), "source-roots", false, "src.{0}{1}.dir"); //NOI18N
        }
        return this.sourceRoots;
    }
    
    public synchronized SourceRoots getTestSourceRoots() {
        if (this.testRoots == null) { //Local caching, no project metadata access
            this.testRoots = new SourceRoots(this.updateHelper, evaluator(), getReferenceHelper(), "test-roots", true, "test.{0}{1}.dir"); //NOI18N
        }
        return this.testRoots;
    }
    
    File getTestClassesDirectory() {
        String testClassesDir = evaluator().getProperty(JbiCompProjectProperties.BUILD_TEST_CLASSES_DIR);
        if (testClassesDir == null) {
            return null;
        }
        return helper.resolveFile(testClassesDir);
    }
    
    /** Store configured project name. */
    public void setName(final String name) {
        ProjectManager.mutex().writeAccess(new Mutex.Action() {
            public Object run() {
                Element data = helper.getPrimaryConfigurationData(true);
                // XXX replace by XMLUtil when that has findElement, findText, etc.
                NodeList nl = data.getElementsByTagNameNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, "name");
                Element nameEl;
                if (nl.getLength() == 1) {
                    nameEl = (Element) nl.item(0);
                    NodeList deadKids = nameEl.getChildNodes();
                    while (deadKids.getLength() > 0) {
                        nameEl.removeChild(deadKids.item(0));
                    }
                } else {
                    nameEl = data.getOwnerDocument().createElementNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, "name");
                    data.insertBefore(nameEl, /* OK if null */data.getChildNodes().item(0));
                }
                nameEl.appendChild(data.getOwnerDocument().createTextNode(name));
                helper.putPrimaryConfigurationData(data, true);
                return null;
            }
        });
    }
    
    public boolean fixProjectBuildProperties() throws IOException {
        boolean fixed = false;
        EditableProperties storedProps = 
            this.helper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        if ( storedProps.getProperty(JbiCompProjectProperties.CONF_DIR) == null ) {
            fixed = true;
            EditableProperties updatedProps = this.updateHelper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
            System.out.println("#### STORING MODIFIED Old Project Build Properties");
            // System.out.println("##### Updated Conf.dir " + updatedProps.getProperty(JbiCompProjectProperties.CONF_DIR));
            this.updateHelper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, updatedProps);
        }
        return fixed;
    }
        
    public boolean addSourceRootInProjectXml(SourceRoots roots, String root, String rootName, String rootProp) throws IOException {
        boolean fixed = true;
        System.out.println("#####!!!!! Fixing the old roots");     
        String[] rootLabels = roots.getRootNames();
        String[] rootProps = roots.getRootProperties();
        URL[] rootURLs = roots.getRootURLs();
        URL[] newURLs = new URL[rootURLs.length +1];
        String[] newLabels = new String[rootURLs.length +1];
        int i=0;
        for (;i< rootURLs.length; ++i) {            
            newURLs[i] = rootURLs[i];
            newLabels[i] = roots.getRootDisplayName(rootLabels[i], rootProps[i]);
            if ( rootName.equals(rootLabels[i]) && 
                 rootProp.equals(rootProps[i]) ) {
                System.out.println("#### This project already has the the " + rootName + " as root.");
                return false;
            }
        }
        File f = FileUtil.toFile(this.getProjectDirectory());
        newURLs[i] = JbiCompProjectUtil.getRootURL(f,root);
        newLabels[i] = roots.getRootDisplayName(rootName, rootProp);
        roots.putRoots(newURLs, newLabels);
        return fixed;
    }
    
    public boolean fixOldSourceRootInProjectXml() throws IOException {
        boolean srcFixed = false;
        boolean testFixed = false;
        srcFixed = addSourceRootInProjectXml(this.getSourceRoots(), "src/", "src", "src.src.dir");
        testFixed = addSourceRootInProjectXml(this.getTestSourceRoots(), "test/", "test", "test.test.dir");
        if ( srcFixed || testFixed ) {
            try {
                ProjectManager.getDefault().saveProject(JbiCompProject.this);
            } catch (IllegalArgumentException ex) {
                //                ex.printStackTrace();
            } catch (IOException ex) {
                //                ex.printStackTrace();
                //                ErrorManager.getDefault().notify(ex);
            }
            return true;
        } else {
            return false;
        }
        
    }
    
    public void refreshProjectBuildScripts() throws IOException {
        
        if (updateHelper.isCurrent()) {
            //Refresh build-impl.xml only for current version of jbicomponent project
            genFilesHelper.refreshBuildScript(
                    GeneratedFilesHelper.BUILD_IMPL_XML_PATH,
                    JbiCompProject.class.getResource("resources/build-impl.xsl"),
                    false);
            genFilesHelper.refreshBuildScript(
                    GeneratedFilesHelper.BUILD_XML_PATH,
                    JbiCompProject.class.getResource("resources/build.xsl"),
                    false);
            // regenerate jbiadmin build scripts
            // genFilesHelper.
            FileObject buildImplFO =  getProjectDirectory().getFileObject(GeneratedFilesHelper.BUILD_IMPL_XML_PATH);
            FileObject buildImplDir = buildImplFO.getParent();
            
            FileObject jbiAdminXmlFO = FileUtil.createData(buildImplDir, "jbi_admin.xml");
            URL jbiAdminXmlURL = JbiCompProject.class.getResource("resources/jbi_admin.xml");
            Util.copyFile(jbiAdminXmlFO, jbiAdminXmlURL);
            
            FileObject jbiAdminImplXmlFO = FileUtil.createData(buildImplDir, "jbiadmin-impl.xml");
            URL jbiAdminImplXmlURL = JbiCompProject.class.getResource("resources/jbiadmin-impl.xml");
            Util.copyFile(jbiAdminImplXmlFO, jbiAdminImplXmlURL);
        }
    }
    
    // Private innerclasses ----------------------------------------------------
    
    private final class Info implements ProjectInformation {
        
        private final PropertyChangeSupport pcs = new PropertyChangeSupport(this);
        private Icon mPrjIcon = null;
        Info() {}
        
        void firePropertyChange(String prop) {
            pcs.firePropertyChange(prop, null, null);
        }
        
        public String getName() {
            return PropertyUtils.getUsablePropertyName(getDisplayName());
        }
        
        public String getDisplayName() {
            return (String) ProjectManager.mutex().readAccess(new Mutex.Action() {
                public Object run() {
                    Element data = updateHelper.getPrimaryConfigurationData(true);
                    // XXX replace by XMLUtil when that has findElement, findText, etc.
                    NodeList nl = data.getElementsByTagNameNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, "name"); // NOI18N
                    if (nl.getLength() == 1) {
                        nl = nl.item(0).getChildNodes();
                        if (nl.getLength() == 1 && nl.item(0).getNodeType() == Node.TEXT_NODE) {
                            return ((Text) nl.item(0)).getNodeValue();
                        }
                    }
                    return "???"; // NOI18N
                }
            });
        }
        
        private Icon getProjectIcon() {
            if ( mPrjIcon != null ) {
                return mPrjIcon;
            }
            
            Object iconObj = ProjectManager.mutex().readAccess(new Mutex.Action() {
                public Object run() {
                    Element data = updateHelper.getPrimaryConfigurationData(true);
                    // XXX replace by XMLUtil when that has findElement, findText, etc.
                    NodeList nl = data.getElementsByTagNameNS(JbiCompProjectType.PROJECT_CONFIGURATION_NAMESPACE, 
                            JbiCompProjectType.COMP_TYPE_EL); // NOI18N
                    if (nl.getLength() == 1) {
                        nl = nl.item(0).getChildNodes();
                        if (nl.getLength() == 1 && nl.item(0).getNodeType() == Node.TEXT_NODE) {
                            String compType = ((Text) nl.item(0)).getNodeValue();
                            Icon compIcon = null;
                            if (JbiCompProjectType.ENGINE_TYPE.equalsIgnoreCase(compType) ) {
                                compIcon = SE_PROJECT_ICON;
                            } else if ( JbiCompProjectType.BINDING_TYPE.equalsIgnoreCase(compType) ) {
                                compIcon = BC_PROJECT_ICON;
                            } else {
                                System.out.println("XXX JbiCompProject component-type found was not se or bc " + compType);
                            }
                            return compIcon;
                        }
                    }
                    System.out.println("XXX JbiCompProject did not find component type");
                    return null; // NOI18N
                }
            });
            
            if ( iconObj != null && iconObj instanceof Icon ) {
                mPrjIcon = (Icon)iconObj;
            } else {
                mPrjIcon = JBI_PROJECT_ICON;
            }
            
            return mPrjIcon;
        }
        
        public Icon getIcon() {
            Icon prjIcon =  getProjectIcon();
            if ( prjIcon == null ) {
              prjIcon = JBI_PROJECT_ICON;
            }
            return prjIcon;
        }
        
        public Project getProject() {
            return JbiCompProject.this;
        }
        
        public void addPropertyChangeListener(PropertyChangeListener listener) {
            pcs.addPropertyChangeListener(listener);
        }
        
        public void removePropertyChangeListener(PropertyChangeListener listener) {
            pcs.removePropertyChangeListener(listener);
        }
        
    }
    
    private final class ProjectXmlSavedHookImpl extends ProjectXmlSavedHook {
        
        ProjectXmlSavedHookImpl() {}
        
        protected void projectXmlSaved() throws IOException {
            //May be called by {@link AuxiliaryConfiguration#putConfigurationFragment}
            //which didn't affect the jbicompproject
            refreshProjectBuildScripts();
        }
        
    }
    
    private final class ProjectOpenedHookImpl extends ProjectOpenedHook {
        
        ProjectOpenedHookImpl() {}
        
        protected void projectOpened() {
            // Check up on build scripts.
            try {                
                boolean propsFixed = fixProjectBuildProperties();
                if ( propsFixed ) {
                    // then fix the source roots
                    fixOldSourceRootInProjectXml();
                }
                refreshProjectBuildScripts();
                
            } catch (IOException e) {
                ErrorManager.getDefault().notify(ErrorManager.INFORMATIONAL, e);
            }
            
            // register project's classpaths to GlobalPathRegistry
            ClassPathProviderImpl cpProvider = (ClassPathProviderImpl)lookup.lookup(ClassPathProviderImpl.class);
            GlobalPathRegistry.getDefault().register(ClassPath.BOOT, cpProvider.getProjectClassPaths(ClassPath.BOOT));
            GlobalPathRegistry.getDefault().register(ClassPath.SOURCE, cpProvider.getProjectClassPaths(ClassPath.SOURCE));
            GlobalPathRegistry.getDefault().register(ClassPath.COMPILE, cpProvider.getProjectClassPaths(ClassPath.COMPILE));
            
            //register updater for component, bootstrap classes
            //the updater is active only on the opened projects
////CHK            compClassUpdater = new ComponentClassUpdater(JbiCompProject.this, eval, updateHelper,
////                    cpProvider.getProjectClassPaths(ClassPath.SOURCE)[0], JbiCompProjectProperties.COMPONENT_CLASS);
            
            // Make it easier to run headless builds on the same machine at least.
            ProjectManager.mutex().writeAccess(new Mutex.Action() {
                public Object run() {
                    EditableProperties ep = updateHelper.getProperties(AntProjectHelper.PRIVATE_PROPERTIES_PATH);
                    File buildProperties = new File(System.getProperty("netbeans.user"), "build.properties"); // NOI18N
                    ep.setProperty("user.properties.file", buildProperties.getAbsolutePath()); //NOI18N
                    updateHelper.putProperties(AntProjectHelper.PRIVATE_PROPERTIES_PATH, ep);
                    try {
                        ProjectManager.getDefault().saveProject(JbiCompProject.this);
                    } catch (IOException e) {
                        ErrorManager.getDefault().notify(e);
                    }
                    return null;
                }
            });
            JbiCompLogicalViewProvider physicalViewProvider = (JbiCompLogicalViewProvider)
            JbiCompProject.this.getLookup().lookup(JbiCompLogicalViewProvider.class);
            if (physicalViewProvider != null &&  physicalViewProvider.hasBrokenLinks()) {
                BrokenReferencesSupport.showAlert();
            }
        }
        
        protected void projectClosed() {
            // System.out.println("#$#$#$#$#$#$##### PROJECT CLOSED #######");
            // Probably unnecessary, but just in case:
            try {
                ProjectManager.getDefault().saveProject(JbiCompProject.this);
            } catch (IllegalArgumentException ex) {
//                ex.printStackTrace();                
            } catch (IOException ex) {
//                ex.printStackTrace();
//                ErrorManager.getDefault().notify(ex);
            }
            
            // unregister project's classpaths to GlobalPathRegistry
            ClassPathProviderImpl cpProvider = (ClassPathProviderImpl)lookup.lookup(ClassPathProviderImpl.class);
            GlobalPathRegistry.getDefault().unregister(ClassPath.BOOT, cpProvider.getProjectClassPaths(ClassPath.BOOT));
            GlobalPathRegistry.getDefault().unregister(ClassPath.SOURCE, cpProvider.getProjectClassPaths(ClassPath.SOURCE));
            GlobalPathRegistry.getDefault().unregister(ClassPath.COMPILE, cpProvider.getProjectClassPaths(ClassPath.COMPILE));
////CHK            if (compClassUpdater != null) {
////                compClassUpdater.unregister();
////                compClassUpdater = null;
////            }
            
        }
        
    }    
    /**
     * Exports the main JAR as an official build product for use from other scripts.
     * The type of the artifact will be {@link AntArtifact#TYPE_JAR}.
     */
    private final class AntArtifactProviderImpl implements AntArtifactProvider {
        
        public AntArtifact[] getBuildArtifacts() {
            return new AntArtifact[] {
                helper.createSimpleAntArtifact(JavaProjectConstants.ARTIFACT_TYPE_JAR, "dist.jar", evaluator(), "jar", "clean"), // NOI18N
            };
        }
        
    }
    
    private static final class RecommendedTemplatesImpl implements RecommendedTemplates, PrivilegedTemplates {
        RecommendedTemplatesImpl(UpdateHelper helper) {
            this.helper = helper;
        }
        
        private UpdateHelper helper;
        
        // List of primarily supported templates
        
        private static final String[] APPLICATION_TYPES = new String[] {
            "java-classes",         // NOI18N
            "java-beans",           // NOI18N
            "oasis-XML-catalogs",   // NOI18N
            "XML",                  // NOI18N
            "ant-script",           // NOI18N
            "ant-task",             // NOI18N
            "wsdl",                 // NOI18N
            "junit",                // NOI18N
            "simple-files"          // NOI18N
        };
        
        private static final String[] LIBRARY_TYPES = new String[] {
            "java-classes",         // NOI18N
            "java-beans",           // NOI18N
            "oasis-XML-catalogs",   // NOI18N
            "XML",                  // NOI18N
            "ant-script",           // NOI18N
            "ant-task",             // NOI18N
            "wsdl",                 // NOI18N
            "junit",                // NOI18N
            "simple-files"          // NOI18N
        };
        
        private static final String[] PRIVILEGED_NAMES = new String[] {
            "Templates/Classes/Class.java", // NOI18N
            "Templates/Classes/Package", // NOI18N
            "Templates/Classes/Interface.java" // NOI18N
        };
        
        public String[] getRecommendedTypes() {
            
            EditableProperties ep = helper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
            // if the project has no main class, it's not really an application
            boolean isLibrary = ep.getProperty(JbiCompProjectProperties.MAIN_CLASS) == null || "".equals(ep.getProperty(JbiCompProjectProperties.MAIN_CLASS)); // NOI18N
            return isLibrary ? LIBRARY_TYPES : APPLICATION_TYPES;
        }
        
        public String[] getPrivilegedTemplates() {
            return PRIVILEGED_NAMES;
        }
        
    }
        
}
