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

package org.netbeans.modules.jbi.apisupport.project.ui.customizer;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;
import javax.swing.ButtonModel;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.ListCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.queries.CollocationQuery;
import org.netbeans.modules.jbi.apisupport.JbiAdminSettings;
import org.netbeans.modules.jbi.apisupport.common.JbiComponentDescriptor;
import org.netbeans.modules.jbi.apisupport.common.JbiDescriptorFactory;
import org.netbeans.modules.jbi.apisupport.common.JbiSharedLibrary;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProject;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectUtil;
import org.netbeans.modules.jbi.apisupport.project.SourceRoots;
import org.netbeans.modules.jbi.apisupport.project.UpdateHelper;
import org.netbeans.modules.jbi.apisupport.project.classpath.ClassPathSupport;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.GeneratedFilesHelper;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.netbeans.spi.project.support.ant.ui.StoreGroup;
import org.openide.DialogDisplayer;
import org.openide.ErrorManager;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.modules.SpecificationVersion;
import org.openide.util.Mutex;
import org.openide.util.MutexException;
import org.openide.util.NbBundle;

/**
 * @author chikkala, j2seproject
 */
public class CustomizerUIModel extends JbiCompProjectProperties {
    
    //Hotfix of the issue #70058
    //Should be removed when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel
    private static final Integer BOOLEAN_KIND_TF = new Integer( 0 );
    private static final Integer BOOLEAN_KIND_YN = new Integer( 1 );
    private static final Integer BOOLEAN_KIND_ED = new Integer( 2 );
    private Integer javacDebugBooleanKind;
    private Integer javadocPreviewBooleanKind;             
     
    ClassPathSupport cs;
        
    // SOURCE ROOTS
    // public static final String SOURCE_ROOTS = "__virtual_source_roots__";   //NOI18N
    // public static final String TEST_ROOTS = "__virtual_test_roots__"; // NOI18N
                        
    // MODELS FOR VISUAL CONTROLS
    
    // CustomizerSources
    DefaultTableModel SOURCE_ROOTS_MODEL;
    DefaultTableModel TEST_ROOTS_MODEL;
    ComboBoxModel JAVAC_SOURCE_MODEL;
     
    // CustomizerLibraries
    //JC DefaultListModel JAVAC_CLASSPATH_MODEL;
    DefaultListModel JBI_COMP_LIB_CLASSPATH_MODEL;
    DefaultListModel JAVAC_TEST_CLASSPATH_MODEL;
    DefaultListModel RUN_CLASSPATH_MODEL;
    DefaultListModel RUN_TEST_CLASSPATH_MODEL;
    ComboBoxModel PLATFORM_MODEL;
    ListCellRenderer CLASS_PATH_LIST_RENDERER;
    ListCellRenderer PLATFORM_LIST_RENDERER;
    ListCellRenderer JAVAC_SOURCE_RENDERER;
    
    // CustomizerCompile
    ButtonModel JAVAC_DEPRECATION_MODEL; 
    ButtonModel JAVAC_DEBUG_MODEL;
    ButtonModel NO_DEPENDENCIES_MODEL;
    Document JAVAC_COMPILER_ARG_MODEL;
    
    // CustomizerCompileTest
                
    // CustomizerJar
    Document DIST_JAR_MODEL; 
    Document BUILD_CLASSES_EXCLUDES_MODEL; 
    ButtonModel JAR_COMPRESS_MODEL;
                
    // CustomizerJavadoc
    ButtonModel JAVADOC_PRIVATE_MODEL;
    ButtonModel JAVADOC_NO_TREE_MODEL;
    ButtonModel JAVADOC_USE_MODEL;
    ButtonModel JAVADOC_NO_NAVBAR_MODEL; 
    ButtonModel JAVADOC_NO_INDEX_MODEL; 
    ButtonModel JAVADOC_SPLIT_INDEX_MODEL; 
    ButtonModel JAVADOC_AUTHOR_MODEL; 
    ButtonModel JAVADOC_VERSION_MODEL;
    Document JAVADOC_WINDOW_TITLE_MODEL;
    ButtonModel JAVADOC_PREVIEW_MODEL; 
    Document JAVADOC_ADDITIONALPARAM_MODEL;

    // CustomizerRun
    Document MAIN_CLASS_MODEL;
    Document APPLICATION_ARGS_MODEL;
    Document RUN_JVM_ARGS_MODEL;
    Document RUN_WORK_DIR_MODEL;
    // Jbi CustomizerRun models
    Document AS_INSTANCE_ID_MODEL;
    Document AS_HOME_MODEL;
//    Document JBI_HOME_MODEL;
    Document JBI_HOST_MODEL;
    Document JBI_PORT_MODEL;
    Document JBI_USERNAME_MODEL;
    Document JBI_PASSWORD_MODEL;
    
    DefaultListModel JBI_ANT_TASKS_CLASSPATH_MODEL;
    
    Document JBI_COMPONENT_NAME_MODEL;
    Document JBI_COMPONENT_TYPE_MODEL;
    Document JBI_COMPONENT_DESC_MODEL;
    Document JBI_COMPONENT_CLASS_MODEL;
    Document JBI_COMPONENT_BT_CLASS_MODEL;
    
    Document JBI_INSTALL_PARAMS_FILE_MODEL;
    ButtonModel JBI_INSTALL_WITH_PARAMS_MODEL;
    
    InstallParamEditor.InstallParamTableModel INSTALL_PARAMS_MODEL; // init this model from the installParamEditor.
    
    // CustomizerTest
    
    ButtonModel TEST_SA_ENABLED_MODEL;

    // Private fields ----------------------------------------------------------    
    private JbiCompProject project;
    private HashMap properties;    
    private UpdateHelper updateHelper;
    private PropertyEvaluator evaluator;
    private ReferenceHelper refHelper;
    private GeneratedFilesHelper genFileHelper;
    
    private StoreGroup privateGroup; 
    private StoreGroup projectGroup;
    private StoreGroup uiGroup;
    
    private Properties additionalProperties;    
    
    String mCompCLDelegate = "";
    String mBootstrapCLDelegate = "";
    
    List<JbiSharedLibrary> mSLibs;
    SharedLibrariesEditor.SharedLibTableModel SLIBS_TABLE_MODEL; // init this model from the SharedLibrariesEditor.
    
    JbiCompProject getProject() {
        return project;
    }
    
    /** Creates a new instance of CustomizerUIModel and initializes it */
    public CustomizerUIModel( JbiCompProject project, UpdateHelper updateHelper, PropertyEvaluator evaluator, ReferenceHelper refHelper, GeneratedFilesHelper genFileHelper ) {
        this.project = project;
        this.updateHelper  = updateHelper;
        this.evaluator = evaluator;
        this.refHelper = refHelper;
        this.genFileHelper = genFileHelper;
        this.cs = new ClassPathSupport( evaluator, refHelper, updateHelper.getAntProjectHelper(), WELL_KNOWN_PATHS, LIBRARY_PREFIX, LIBRARY_SUFFIX, ANT_ARTIFACT_PREFIX );
                
        privateGroup = new StoreGroup();
        projectGroup = new StoreGroup();
        uiGroup = new StoreGroup();
        additionalProperties = new Properties();
        
        init(); // Load known properties        
    }

    /** Initializes the visual models 
     */
    private void init() {
        
        CLASS_PATH_LIST_RENDERER = new JbiCompClassPathUi.ClassPathListCellRenderer( evaluator );
        
        // CustomizerSources
        SOURCE_ROOTS_MODEL = JbiCompSourceRootsUi.createModel( project.getSourceRoots() );
        TEST_ROOTS_MODEL = JbiCompSourceRootsUi.createModel( project.getTestSourceRoots() );        
                
        // CustomizerLibraries
        EditableProperties projectProperties = updateHelper.getProperties( AntProjectHelper.PROJECT_PROPERTIES_PATH );                
        
//JC        JAVAC_CLASSPATH_MODEL = ClassPathUiSupport.createListModel( cs.itemsIterator( (String)projectProperties.get( JAVAC_CLASSPATH )  ) );
        JBI_COMP_LIB_CLASSPATH_MODEL = ClassPathUiSupport.createListModel( cs.itemsIterator( (String)projectProperties.get( JBI_COMP_LIB_CLASSPATH )  ) );
        JAVAC_TEST_CLASSPATH_MODEL = ClassPathUiSupport.createListModel( cs.itemsIterator( (String)projectProperties.get( JAVAC_TEST_CLASSPATH ) ) );
        RUN_CLASSPATH_MODEL = ClassPathUiSupport.createListModel( cs.itemsIterator( (String)projectProperties.get( RUN_CLASSPATH ) ) );
        RUN_TEST_CLASSPATH_MODEL = ClassPathUiSupport.createListModel( cs.itemsIterator( (String)projectProperties.get( RUN_TEST_CLASSPATH ) ) );
        PLATFORM_MODEL = PlatformUiSupport.createPlatformComboBoxModel (evaluator.getProperty(JAVA_PLATFORM));
        PLATFORM_LIST_RENDERER = PlatformUiSupport.createPlatformListCellRenderer();
        JAVAC_SOURCE_MODEL = PlatformUiSupport.createSourceLevelComboBoxModel (PLATFORM_MODEL, evaluator.getProperty(JAVAC_SOURCE));
        JAVAC_SOURCE_RENDERER = PlatformUiSupport.createSourceLevelListCellRenderer ();
                
        // CustomizerCompile
        JAVAC_DEPRECATION_MODEL = projectGroup.createToggleButtonModel( evaluator, JAVAC_DEPRECATION );
                
        //Hotfix of the issue #70058
        //Should use the StoreGroup when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel
        Integer[] kind = new Integer[1];
        JAVAC_DEBUG_MODEL = createToggleButtonModel( evaluator, JAVAC_DEBUG, kind);
        javacDebugBooleanKind = kind[0];
        
        NO_DEPENDENCIES_MODEL = projectGroup.createInverseToggleButtonModel( evaluator, NO_DEPENDENCIES );
        JAVAC_COMPILER_ARG_MODEL = projectGroup.createStringDocument( evaluator, JAVAC_COMPILER_ARG );
        
        // CustomizerJar
        DIST_JAR_MODEL = projectGroup.createStringDocument( evaluator, DIST_JAR );
        BUILD_CLASSES_EXCLUDES_MODEL = projectGroup.createStringDocument( evaluator, BUILD_CLASSES_EXCLUDES );
        JAR_COMPRESS_MODEL = projectGroup.createToggleButtonModel( evaluator, JAR_COMPRESS );
        
        // CustomizerJavadoc
        JAVADOC_PRIVATE_MODEL = projectGroup.createToggleButtonModel( evaluator, JAVADOC_PRIVATE );
        JAVADOC_NO_TREE_MODEL = projectGroup.createInverseToggleButtonModel( evaluator, JAVADOC_NO_TREE );
        JAVADOC_USE_MODEL = projectGroup.createToggleButtonModel( evaluator, JAVADOC_USE );
        JAVADOC_NO_NAVBAR_MODEL = projectGroup.createInverseToggleButtonModel( evaluator, JAVADOC_NO_NAVBAR );
        JAVADOC_NO_INDEX_MODEL = projectGroup.createInverseToggleButtonModel( evaluator, JAVADOC_NO_INDEX ); 
        JAVADOC_SPLIT_INDEX_MODEL = projectGroup.createToggleButtonModel( evaluator, JAVADOC_SPLIT_INDEX );
        JAVADOC_AUTHOR_MODEL = projectGroup.createToggleButtonModel( evaluator, JAVADOC_AUTHOR );
        JAVADOC_VERSION_MODEL = projectGroup.createToggleButtonModel( evaluator, JAVADOC_VERSION );
        JAVADOC_WINDOW_TITLE_MODEL = projectGroup.createStringDocument( evaluator, JAVADOC_WINDOW_TITLE );
        //Hotfix of the issue #70058
        //Should use the StoreGroup when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel        
        JAVADOC_PREVIEW_MODEL = createToggleButtonModel ( evaluator, JAVADOC_PREVIEW, kind);
        javadocPreviewBooleanKind = kind[0];
        
        JAVADOC_ADDITIONALPARAM_MODEL = projectGroup.createStringDocument( evaluator, JAVADOC_ADDITIONALPARAM );
        // CustomizerRun
        MAIN_CLASS_MODEL = projectGroup.createStringDocument( evaluator, MAIN_CLASS ); 
        APPLICATION_ARGS_MODEL = privateGroup.createStringDocument( evaluator, APPLICATION_ARGS );
        RUN_JVM_ARGS_MODEL = projectGroup.createStringDocument( evaluator, RUN_JVM_ARGS );
        RUN_WORK_DIR_MODEL = privateGroup.createStringDocument( evaluator, RUN_WORK_DIR );
        // Jbi Customizer Run
        AS_INSTANCE_ID_MODEL = projectGroup.createStringDocument( evaluator, AS_INSTANCE_ID ); 
        AS_HOME_MODEL = projectGroup.createStringDocument( evaluator, AS_HOME ); 
//        JBI_HOME_MODEL = projectGroup.createStringDocument( evaluator, JBI_HOME ); 
        JBI_HOST_MODEL = projectGroup.createStringDocument( evaluator, JBI_HOST ); 
        JBI_PORT_MODEL = projectGroup.createStringDocument( evaluator, JBI_PORT ); 
        JBI_USERNAME_MODEL = projectGroup.createStringDocument( evaluator, JBI_USERNAME ); 
        JBI_PASSWORD_MODEL = projectGroup.createStringDocument( evaluator, JBI_PASSWORD ); 
        
        JBI_ANT_TASKS_CLASSPATH_MODEL = ClassPathUiSupport.createListModel( 
                cs.itemsIterator( (String)projectProperties.get( JBI_ANT_TASKS_CLASSPATH )  ) );
        
        JBI_COMPONENT_NAME_MODEL = projectGroup.createStringDocument( evaluator, JBI_COMPONENT_NAME);        
        JBI_COMPONENT_TYPE_MODEL = projectGroup.createStringDocument( evaluator, JBI_COMPONENT_TYPE); 
        
        loadFromJbiXml(this.updateHelper, projectProperties);
        
        JBI_COMPONENT_DESC_MODEL = projectGroup.createStringDocument( evaluator, JBI_COMPONENT_DESC);
        JBI_COMPONENT_CLASS_MODEL = projectGroup.createStringDocument( evaluator, JBI_COMPONENT_CLASS);
        JBI_COMPONENT_BT_CLASS_MODEL = projectGroup.createStringDocument( evaluator, JBI_COMPONENT_BT_CLASS);
        
        JBI_INSTALL_PARAMS_FILE_MODEL = projectGroup.createStringDocument( evaluator, JBI_INSTALL_PARAMS_FILE); 
        JBI_INSTALL_WITH_PARAMS_MODEL = projectGroup.createToggleButtonModel( evaluator, JBI_INSTALL_WITH_PARAMS );
        
        INSTALL_PARAMS_MODEL = null; // init this from the InstallParamEditor in the CustomizerRun        
        
        TEST_SA_ENABLED_MODEL = projectGroup.createToggleButtonModel( evaluator, TEST_SA_ENABLED );
        
        try {
            FileObject jbiXmlFO = getJbiXmlFileObject(updateHelper, projectProperties);
            JbiComponentDescriptor jbiCompDescriptor = getJbiComponentDescriptor(jbiXmlFO);
            this.mCompCLDelegate = jbiCompDescriptor.getComponentCLDelegation();
            this.mBootstrapCLDelegate = jbiCompDescriptor.getBootstrapCLDelegation();
            
            // init sharedlibrary model
            initSharedLibraryModel(projectProperties);
            
        } catch (Exception ex ) {
            ex.printStackTrace();
        }
    }
    
    private void initSharedLibraryModel(EditableProperties projectProperties) {
        
        SLIBS_TABLE_MODEL = new SharedLibrariesEditor.SharedLibTableModel(); 
        
        this.mSLibs = new ArrayList<JbiSharedLibrary>();
        String slibsClassPath = projectProperties.getProperty(JBI_COMP_SHARED_LIB_CLASSPATH);
        String[] slibPropRefs = PropertyUtils.tokenizePath( slibsClassPath == null ? "": slibsClassPath );
        for ( String propRef : slibPropRefs ) {
            String classPath = evaluator.evaluate(propRef);
            this.mSLibs.add(
                    JbiSharedLibrary.JbiSharedLibraryImpl
                    .createFromPropertyReference(propRef, classPath));
        }
        SLIBS_TABLE_MODEL.load(this.mSLibs);
    }
    private void clearAllSharedLibraryClassPaths(EditableProperties projectProperties) {
        Set<String> propSet = new HashSet<String>();
        for ( String prop : projectProperties.keySet() ) {
            if (prop.startsWith(JbiSharedLibrary.PROP_PREFIX) && prop.endsWith(JbiSharedLibrary.PROP_SUFFIX)) {
                propSet.add(prop);
            }
        }
        for ( String prop : propSet) {
            projectProperties.remove(prop);
        }                
        projectProperties.setProperty(JBI_COMP_SHARED_LIB_CLASSPATH, new String[0]);
    }
    private void saveSharedLibraryModel(EditableProperties projectProperties) {
        if (SLIBS_TABLE_MODEL != null && SLIBS_TABLE_MODEL.isEdited()) {
            // remove all slib classpaths 
            clearAllSharedLibraryClassPaths(projectProperties);
            this.mSLibs = SLIBS_TABLE_MODEL.getSharedLibs();
            List<String> slibsCPValue = new ArrayList<String>();
            for (JbiSharedLibrary slib : this.mSLibs) {
                String slibProp = slib.getClassPathProperty();
                String slibClassPath = slib.getClassPath();
                slibsCPValue.add("${" + slibProp + "}:");
                projectProperties.setProperty(slibProp, slibClassPath);
            }
            projectProperties.setProperty(JBI_COMP_SHARED_LIB_CLASSPATH, slibsCPValue.toArray(new String[0]));
        } else {
            System.out.println("### Shared library classpath not modified");
        }
    }
    public void save() {
        try {                        
            // Store properties 
            Boolean result = (Boolean) ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction() {
                final FileObject projectDir = updateHelper.getAntProjectHelper().getProjectDirectory();
                public Object run() throws IOException {
                    if ((genFileHelper.getBuildScriptState(GeneratedFilesHelper.BUILD_IMPL_XML_PATH,
                        JbiCompProject.class.getResource("resources/build-impl.xsl"))
                        & GeneratedFilesHelper.FLAG_MODIFIED) == GeneratedFilesHelper.FLAG_MODIFIED) {  //NOI18N
                        if (showModifiedMessage (NbBundle.getMessage(CustomizerUIModel.class,"TXT_ModifiedTitle"))) {
                            //Delete user modified build-impl.xml
                            FileObject fo = projectDir.getFileObject(GeneratedFilesHelper.BUILD_IMPL_XML_PATH);
                            if (fo != null) {
                                fo.delete();
                            }
                        }
                        else {
                            return Boolean.FALSE;
                        }
                    }
                    storeProperties();
                    return Boolean.TRUE;
                }
            });
            // and save the project
            if (result == Boolean.TRUE) {
                ProjectManager.getDefault().saveProject(project);
            }
        } 
        catch (MutexException e) {
            ErrorManager.getDefault().notify((IOException)e.getException());
        }
        catch ( IOException ex ) {
            ErrorManager.getDefault().notify( ex );
        }
    }
    
    
        
    private void storeProperties() throws IOException {
        // Store special properties
        
        // Modify the project dependencies properly        
        resolveProjectDependencies();
        
        // Encode all paths (this may change the project properties)
//JC        String[] javac_cp = cs.encodeToStrings( ClassPathUiSupport.getIterator( JAVAC_CLASSPATH_MODEL ) );
        String[] jbi_comp_lib_cp = cs.encodeToStrings( ClassPathUiSupport.getIterator( JBI_COMP_LIB_CLASSPATH_MODEL ) );
        String[] javac_test_cp = cs.encodeToStrings( ClassPathUiSupport.getIterator( JAVAC_TEST_CLASSPATH_MODEL ) );
        String[] run_cp = cs.encodeToStrings( ClassPathUiSupport.getIterator( RUN_CLASSPATH_MODEL ) );
        String[] run_test_cp = cs.encodeToStrings( ClassPathUiSupport.getIterator( RUN_TEST_CLASSPATH_MODEL ) );
                
        // Store source roots
        storeRoots( project.getSourceRoots(), SOURCE_ROOTS_MODEL );
        storeRoots( project.getTestSourceRoots(), TEST_ROOTS_MODEL );
                
        // Store standard properties
        EditableProperties projectProperties = updateHelper.getProperties( AntProjectHelper.PROJECT_PROPERTIES_PATH );        
        EditableProperties privateProperties = updateHelper.getProperties( AntProjectHelper.PRIVATE_PROPERTIES_PATH );
        
        // Assure inegrity which can't shound not be assured in UI
        if ( !JAVADOC_NO_INDEX_MODEL.isSelected() ) {
            JAVADOC_SPLIT_INDEX_MODEL.setSelected( false ); // Can't split non existing index
        }
                                
        // Standard store of the properties
        projectGroup.store( projectProperties );        
        privateGroup.store( privateProperties );
        
        // if check box to include test sa in the test is unchecked, 
        // then remove the TEST_SA_ENABLED property.
        if (TEST_SA_ENABLED_MODEL.isSelected()) {
            projectProperties.setProperty(TEST_SA_ENABLED, TEST_SA_ENABLED_VALUE); // true
        } else {
            projectProperties.remove(TEST_SA_ENABLED);
        }
        
        //Hotfix of the issue #70058
        //Should use the StoreGroup when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel
        //Save javac.debug
        privateProperties.setProperty(JAVAC_DEBUG, encodeBoolean (JAVAC_DEBUG_MODEL.isSelected(), javacDebugBooleanKind));
                
        //Hotfix of the issue #70058
        //Should use the StoreGroup when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel
        //Save javadoc.preview
        privateProperties.setProperty(JAVADOC_PREVIEW, encodeBoolean (JAVADOC_PREVIEW_MODEL.isSelected(), javadocPreviewBooleanKind));
                
        // Save all paths
//JC        projectProperties.setProperty( JAVAC_CLASSPATH, javac_cp );
        projectProperties.setProperty( JBI_COMP_LIB_CLASSPATH, jbi_comp_lib_cp );
        projectProperties.setProperty( JAVAC_TEST_CLASSPATH, javac_test_cp );
        projectProperties.setProperty( RUN_CLASSPATH, run_cp );
        projectProperties.setProperty( RUN_TEST_CLASSPATH, run_test_cp );
        
        //Handle platform selection and javac.source javac.target properties
        PlatformUiSupport.storePlatform (projectProperties, updateHelper,PLATFORM_MODEL.getSelectedItem(), JAVAC_SOURCE_MODEL.getSelectedItem());
                                
        // Handle other special cases
        if ( NO_DEPENDENCIES_MODEL.isSelected() ) { // NOI18N
            projectProperties.remove( NO_DEPENDENCIES ); // Remove the property completely if not set
        }

        if ( getDocumentText( RUN_WORK_DIR_MODEL ).trim().equals( "" ) ) { // NOI18N
            privateProperties.remove( RUN_WORK_DIR ); // Remove the property completely if not set
        }
                
        storeAdditionalProperties(projectProperties);
        
        relativiseInstallParamsFileProperty(getProject().getProjectDirectory(), projectProperties);
                
        String[] jbi_ant_task_cp = cs.encodeToStrings( ClassPathUiSupport.getIterator( JBI_ANT_TASKS_CLASSPATH_MODEL ) );
        projectProperties.setProperty( JBI_ANT_TASKS_CLASSPATH, jbi_ant_task_cp );
        
//        System.out.println(" Updating the project prop for jbi ant classpath");
//        for ( int i=0; i < jbi_ant_task_cp.length; ++i) {
//            System.out.println("jbi ant path ele: " + jbi_ant_task_cp[i]);
//        }
                        
        // save sharedl library model to project properties
        saveSharedLibraryModel(projectProperties);
        
        // Store the property changes into the project
        updateHelper.putProperties( AntProjectHelper.PROJECT_PROPERTIES_PATH, projectProperties );
        updateHelper.putProperties( AntProjectHelper.PRIVATE_PROPERTIES_PATH, privateProperties );                
        
        
        if ( INSTALL_PARAMS_MODEL != null && INSTALL_PARAMS_MODEL.isEdited()) {
            String paramsFile = getDocumentText(JBI_INSTALL_PARAMS_FILE_MODEL);
            Properties params = INSTALL_PARAMS_MODEL.getParams();
            savePropertiesToFile(paramsFile, params);
        }
        
        saveToJbiXml(updateHelper, projectProperties);
        
        try {
            FileObject jbiXmlFO = getJbiXmlFileObject(updateHelper, projectProperties);
            JbiComponentDescriptor jbiCompDescriptor = getJbiComponentDescriptor(jbiXmlFO);
            jbiCompDescriptor.setComponentCLDelegation(this.mCompCLDelegate);
            jbiCompDescriptor.setBootstrapCLDelegation(this.mBootstrapCLDelegate);
            String[] slibNames = JbiSharedLibrary.JbiSharedLibraryImpl.getSharedLibraryNames(this.mSLibs);
            jbiCompDescriptor.setSharedLibraryNames(slibNames);
            JbiDescriptorFactory.getInstance().saveJbiDescriptor(jbiCompDescriptor, jbiXmlFO);
        } catch (Exception ex ) {
            ex.printStackTrace();
        }
        
    }
  
    private void storeAdditionalProperties(EditableProperties projectProperties) {
        for (Iterator i = additionalProperties.keySet().iterator(); i.hasNext();) {
            String key = (String) i.next();
            projectProperties.put(key, (String) additionalProperties.get(key));
        }
    }
    
    private static void relativiseInstallParamsFileProperty(FileObject prjDir, EditableProperties projectProperties) {
        String value = projectProperties.getProperty(JbiCompProjectProperties.JBI_INSTALL_PARAMS_FILE);
        File prjDirFile = FileUtil.toFile(prjDir);
        File installFile = new File(value);
        String relativePath = PropertyUtils.relativizeFile(prjDirFile, installFile);
        projectProperties.setProperty(JbiCompProjectProperties.JBI_INSTALL_PARAMS_FILE, relativePath);
    }
        
    private static String getDocumentText( Document document ) {
        try {
            return document.getText( 0, document.getLength() );
        }
        catch( BadLocationException e ) {
            return ""; // NOI18N
        }
    }
    
    /** Finds out what are new and removed project dependencies and 
     * applyes the info to the project
     */
    private void resolveProjectDependencies() {
            
        // Create a set of old and new artifacts.
        Set oldArtifacts = new HashSet();
        EditableProperties projectProperties = updateHelper.getProperties( AntProjectHelper.PROJECT_PROPERTIES_PATH );        
//JC        oldArtifacts.addAll( cs.itemsList( (String)projectProperties.get( JAVAC_CLASSPATH ) ) );
        oldArtifacts.addAll( cs.itemsList( (String)projectProperties.get( JBI_COMP_LIB_CLASSPATH ) ) );
        oldArtifacts.addAll( cs.itemsList( (String)projectProperties.get( JAVAC_TEST_CLASSPATH ) ) );
        oldArtifacts.addAll( cs.itemsList( (String)projectProperties.get( RUN_CLASSPATH ) ) );
        oldArtifacts.addAll( cs.itemsList( (String)projectProperties.get( RUN_TEST_CLASSPATH ) ) );
                   
        Set newArtifacts = new HashSet();
//JC        newArtifacts.addAll( ClassPathUiSupport.getList( JAVAC_CLASSPATH_MODEL ) );
        newArtifacts.addAll( ClassPathUiSupport.getList( JBI_COMP_LIB_CLASSPATH_MODEL ) );
        newArtifacts.addAll( ClassPathUiSupport.getList( JAVAC_TEST_CLASSPATH_MODEL ) );
        newArtifacts.addAll( ClassPathUiSupport.getList( RUN_CLASSPATH_MODEL ) );
        newArtifacts.addAll( ClassPathUiSupport.getList( RUN_TEST_CLASSPATH_MODEL ) );
                
        // Create set of removed artifacts and remove them
        Set removed = new HashSet( oldArtifacts );
        removed.removeAll( newArtifacts );
        Set added = new HashSet(newArtifacts);
        added.removeAll(oldArtifacts);
        
        // 1. first remove all project references. The method will modify
        // project property files, so it must be done separately
        for( Iterator it = removed.iterator(); it.hasNext(); ) {
            ClassPathSupport.Item item = (ClassPathSupport.Item)it.next();
            if ( item.getType() == ClassPathSupport.Item.TYPE_ARTIFACT ||
                    item.getType() == ClassPathSupport.Item.TYPE_JAR ) {
                refHelper.destroyReference(item.getReference());
            }
        }
        
        // 2. now read project.properties and modify rest
        EditableProperties ep = updateHelper.getProperties( AntProjectHelper.PROJECT_PROPERTIES_PATH );
        boolean changed = false;
        
        for( Iterator it = removed.iterator(); it.hasNext(); ) {
            ClassPathSupport.Item item = (ClassPathSupport.Item)it.next();
            if (item.getType() == ClassPathSupport.Item.TYPE_LIBRARY) {
                // remove helper property pointing to library jar if there is any
                String prop = item.getReference();
                prop = prop.substring(2, prop.length()-1);
                ep.remove(prop);
                changed = true;
            }
        }
        File projDir = FileUtil.toFile(updateHelper.getAntProjectHelper().getProjectDirectory());
        for( Iterator it = added.iterator(); it.hasNext(); ) {
            ClassPathSupport.Item item = (ClassPathSupport.Item)it.next();
            if (item.getType() == ClassPathSupport.Item.TYPE_LIBRARY && !item.isBroken()) {
                // add property to project.properties pointing to relativized 
                // library jar(s) if possible                
                String prop = cs.getLibraryReference( item );
                prop = prop.substring(2, prop.length()-1); // XXX make a PropertyUtils method for this!
                String value = relativizeLibraryClasspath(prop, projDir);
                if (value != null) {
                    ep.setProperty(prop, value);
                    ep.setComment(prop, new String[]{
                        // XXX this should be I18N! Not least because the English is wrong...
                        "# Property "+prop+" is set here just to make sharing of project simpler.",
                        "# The library definition has always preference over this property."}, false);
                    changed = true;
                }
            }
        }
        if (changed) {
            updateHelper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, ep);
        }
    }
        
    /**
     * Tokenize library classpath and try to relativize all the jars.
     * @param property library property name ala "libs.someLib.classpath"
     * @param projectDir project dir for relativization
     * @return relativized library classpath or null if some jar is not collocated
     */
    private String relativizeLibraryClasspath(String property, File projectDir) {
        String value = PropertyUtils.getGlobalProperties().getProperty(property);
        // bugfix #42852, check if the classpath property is set, otherwise return null
        if (value == null) {
            return null;
        }
        String[] paths = PropertyUtils.tokenizePath(value);
        StringBuffer sb = new StringBuffer();
        for (int i=0; i<paths.length; i++) {
            File f = updateHelper.getAntProjectHelper().resolveFile(paths[i]);
            if (CollocationQuery.areCollocated(f, projectDir)) {
                sb.append(PropertyUtils.relativizeFile(projectDir, f));
            } else {
                return null;
            }
            if (i+1<paths.length) {
                sb.append(File.pathSeparatorChar);
            }
        }
        if (sb.length() == 0) {
            return null;
        } else {
            return sb.toString();
        }
    }
    
    private void storeRoots( SourceRoots roots, DefaultTableModel tableModel ) throws MalformedURLException {
        Vector data = tableModel.getDataVector();
        URL[] rootURLs = new URL[data.size()];
        String []rootLabels = new String[data.size()];
        for (int i=0; i<data.size();i++) {
            File f = (File) ((Vector)data.elementAt(i)).elementAt(0);
            rootURLs[i] = JbiCompProjectUtil.getRootURL(f,null);            
            rootLabels[i] = (String) ((Vector)data.elementAt(i)).elementAt(1);
        }
        roots.putRoots(rootURLs,rootLabels);
    }
            
    /* This is used by CustomizerWSServiceHost */
    public void putAdditionalProperty(String propertyName, String propertyValue) {
        additionalProperties.setProperty(propertyName, propertyValue);
    }
    
    private static boolean showModifiedMessage (String title) {
        String message = NbBundle.getMessage(CustomizerUIModel.class,"TXT_Regenerate");
        JButton regenerateButton = new JButton (NbBundle.getMessage(CustomizerUIModel.class,"CTL_RegenerateButton"));
        regenerateButton.setDefaultCapable(true);
        regenerateButton.getAccessibleContext().setAccessibleDescription (NbBundle.getMessage(CustomizerUIModel.class,"AD_RegenerateButton"));
        NotifyDescriptor d = new NotifyDescriptor.Message (message, NotifyDescriptor.WARNING_MESSAGE);
        d.setTitle(title);
        d.setOptionType(NotifyDescriptor.OK_CANCEL_OPTION);
        d.setOptions(new Object[] {regenerateButton, NotifyDescriptor.CANCEL_OPTION});        
        return DialogDisplayer.getDefault().notify(d) == regenerateButton;
    }
    
    //Hotfix of the issue #70058
    //Should be removed when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel
    private static String encodeBoolean (boolean value, Integer kind) {
        if ( kind == BOOLEAN_KIND_ED ) {
            return value ? "on" : "off"; // NOI18N
        }
        else if ( kind == BOOLEAN_KIND_YN ) { // NOI18N
            return value ? "yes" : "no";
        }
        else {
            return value ? "true" : "false"; // NOI18N
        }
    }
    
    //Hotfix of the issue #70058
    //Should be removed when the StoreGroup SPI will be extended to allow false default value in ToggleButtonModel
    private static JToggleButton.ToggleButtonModel createToggleButtonModel (final PropertyEvaluator evaluator, final String propName, Integer[] kind) {
        assert evaluator != null && propName != null && kind != null && kind.length == 1;
        String value = evaluator.getProperty( propName );
        boolean isSelected = false;
        if (value == null) {
            isSelected = true;
        }
        else {
           String lowercaseValue = value.toLowerCase();
           if ( lowercaseValue.equals( "yes" ) || lowercaseValue.equals( "no" ) ) { // NOI18N
               kind[0] = BOOLEAN_KIND_YN;
           }
           else if ( lowercaseValue.equals( "on" ) || lowercaseValue.equals( "off" ) ) { // NOI18N
               kind[0] = BOOLEAN_KIND_ED;
           }
           else {
               kind[0] = BOOLEAN_KIND_TF;
           }

           if ( lowercaseValue.equals( "true") || // NOI18N
                lowercaseValue.equals( "yes") ||  // NOI18N
                lowercaseValue.equals( "on") ) {  // NOI18N
               isSelected = true;                   
           } 
        }
        JToggleButton.ToggleButtonModel bm = new JToggleButton.ToggleButtonModel();
        bm.setSelected(isSelected );
        return bm;
    }
            
}
