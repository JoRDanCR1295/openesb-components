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


package org.netbeans.modules.jbi.apisupport.project.wizard;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JEditorPane;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.StyledDocument;
import org.netbeans.modules.jbi.apisupport.JbiAdminSettings;
import org.netbeans.modules.jbi.apisupport.common.JbiComponentDescriptor;
import org.netbeans.modules.jbi.apisupport.common.JbiDescriptorFactory;
import org.netbeans.modules.jbi.apisupport.common.TemplateUtil;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.netbeans.modules.jbi.apisupport.common.wizard.ProjectWizardModel;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProject;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectGenerator;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileStateInvalidException;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.ui.support.ProjectChooser;
import org.openide.text.FilterDocument;
import org.openide.text.IndentEngine;

/**
 * This Wizard model contains the data to generate a jbi component project for developing
 * a jbi component ( service engine or binding component )
 *
 * @author  chikkala
 */
public abstract class DeployPluginWizardModel extends ProjectWizardModel {
    
    public static final String DEPLOY_PLUGIN_WIZARD_TYPE = "DEPLOY_PLUGIN_WIZARD_TYPE";
    
    public static final String SERVICE_ENGINE_COMP_TYPE = "service-engine";
    public static final String BINDING_COMPONENT_COMP_TYPE = "binding-component";
    
    public static final String SAMPLE_COMP_PACKAGE = "com.sample.component";
    public static final String SAMPLE_SE_PACKAGE = "com.sample.engine";
    public static final String SAMPLE_BC_PACKAGE = "com.sample.binding";
    
    public static final String SAMPLE_COMP_NAME = "JbiComponent";
    public static final String SAMPLE_SE_NAME = "ServiceEngine";
    public static final String SAMPLE_BC_NAME = "BindingComponent";
    
    private String mCompType;
    private String mName;
    private String mDescription;
    private String mPackageName;
    
    /** Creates a new instance of ComponentProjectWizardModel */
    public DeployPluginWizardModel(WizardDescriptor wiz) {
        this(wiz, DEPLOY_PLUGIN_WIZARD_TYPE );
    }
    /** Creates a new instance of ComponentProjectWizardModel */
    public DeployPluginWizardModel(WizardDescriptor wiz, String wizType) {
        super(wiz,wizType);
    }
    
    public static DeployPluginWizardModel createDeployPluginWizardModel(
            WizardDescriptor wiz, String compType) {
        DeployPluginWizardModel wizModel =  null;
        if ( DeployPluginWizardModel.BINDING_COMPONENT_COMP_TYPE.equals(compType)) {
            wizModel =  new BCDeployPluginWizardModel(wiz);
            wizModel.setComponentType(compType);
        } else if (DeployPluginWizardModel.SERVICE_ENGINE_COMP_TYPE.equals(compType)) {
            wizModel =  new SEDeployPluginWizardModel(wiz);
            wizModel.setComponentType(compType);
        }        
        return wizModel;
    }
    
    public void initializeOthers() {
        WizardDescriptor wiz = this.getWizardDescriptor();
        
        String templateName = "DeployPlugin";
        
        DataObject templateDO = this.getTemplateDataObject();
        if ( templateDO != null ) {
            templateName = templateDO.getName();
        }
        
        this.setTargetName(templateName);
        this.setProjectName(templateName);
        //TODO fix it
        if ( this.getComponentType() == null  ) {
            this.setComponentType(SERVICE_ENGINE_COMP_TYPE);
        }
        
        if ( this.getComponentName() == null ) {
            initDescription(templateName);
        }
        
        if ( this.getPackageName() == null ) {
            initPackageName(templateName);
        }
        
    }
    
    public void instantiateOthers() throws IOException {
        createDeployPluginProject();
    }
    
    public void uninitializeOthers() {
        WizardDescriptor wiz = this.getWizardDescriptor();
    }
    
    public static String getComponentDescription(String compName, String type) {
        
        String desc = "";
        
        if ( JbiComponentProjectWizardModel.SERVICE_ENGINE_COMP_TYPE.equals(type)) {
            desc = "Description of service engine : " + compName ;
        } else if (JbiComponentProjectWizardModel.BINDING_COMPONENT_COMP_TYPE.equals(type)) {
            desc = "Description of binding component : " + compName ;
        } else {
            desc = "Description of Jbi Component : " + compName;
        }
        
        return desc;
    }
    
    public static String getComponentName(String displayName, String type) {
        
        StringBuffer builder = new StringBuffer();
        boolean firstLetter = true;
        for (int i=0; i< displayName.length(); i++) {
            char c = displayName.charAt(i);
            if ((!firstLetter && Character.isJavaIdentifierPart(c)) || (firstLetter && Character.isJavaIdentifierStart(c))) {
                firstLetter = false;
                builder.append(c);
            }
        }
        
        String compName = JbiComponentProjectWizardModel.SAMPLE_COMP_NAME;
        
        if ( builder.length() == 0 ) {
            if ( JbiComponentProjectWizardModel.SERVICE_ENGINE_COMP_TYPE.equals(type)) {
                compName = JbiComponentProjectWizardModel.SAMPLE_SE_NAME;
            } else if (JbiComponentProjectWizardModel.BINDING_COMPONENT_COMP_TYPE.equals(type)) {
                compName = JbiComponentProjectWizardModel.SAMPLE_BC_NAME;
            }
        } else {
            compName = builder.toString();
        }
        return compName;
        
    }
    
    
    public static String getPackageName(String displayName, String type) {
        
        StringBuffer builder = new StringBuffer();
        boolean firstLetter = true;
        for (int i=0; i< displayName.length(); i++) {
            char c = displayName.charAt(i);
            if ((!firstLetter && Character.isJavaIdentifierPart(c)) || (firstLetter && Character.isJavaIdentifierStart(c))) {
                firstLetter = false;
                if (Character.isUpperCase(c)) {
                    c = Character.toLowerCase(c);
                }
                builder.append(c);
            }
        }
        
        String packageName = JbiComponentProjectWizardModel.SAMPLE_COMP_PACKAGE;
        
        if ( builder.length() == 0 ) {
            if ( JbiComponentProjectWizardModel.SERVICE_ENGINE_COMP_TYPE.equals(type)) {
                packageName = JbiComponentProjectWizardModel.SAMPLE_SE_PACKAGE;
            } else if (JbiComponentProjectWizardModel.BINDING_COMPONENT_COMP_TYPE.equals(type)) {
                packageName = JbiComponentProjectWizardModel.SAMPLE_BC_PACKAGE;
            }
        } else {
            packageName = builder.toString();
        }
        return packageName;
    }
    
    /**
     * Getter for property componentType.
     * @return Value of property componentType.
     */
    public String getComponentType() {
        return this.mCompType;
    }
    
    /**
     * Setter for property componentType.
     * @param componentType New value of property componentType.
     */
    public void setComponentType(String componentType) {
        this.mCompType = componentType;
    }
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getComponentName() {
        return this.mName;
    }
    
    /**
     * Setter for property name.
     * @param name New value of property name.
     */
    public void setComponentName(String name) {
        this.mName = name;
    }
    
    /**
     * Getter for property description.
     * @return Value of property description.
     */
    public String getComponentDescription() {
        return this.mDescription;
    }
    
    /**
     * Setter for property description.
     * @param description New value of property description.
     */
    public void setComponentDescription(String description) {
        this.mDescription = description;
    }
    
    /**
     * Getter for property packageName.
     * @return Value of property packageName.
     */
    public String getPackageName() {
        return mPackageName;
    }
    
    /**
     * Setter for property packageName.
     * @param packageName New value of property packageName.
     */
    public void setPackageName(String packageName) {
        this.mPackageName = packageName;
    }
    
    protected void initDescription(String projectName ) {
        
        String type = this.getComponentType();
        String name = getComponentName("", type);
        String desc = getComponentDescription(name, type);
        
        this.setComponentName(name);
        this.setComponentDescription(desc);
    }
    
    protected void initPackageName(String projectName ) {
        
        String type = this.getComponentType();
        String packageName = getPackageName(projectName, type);
        this.setPackageName(packageName);
    }
    
    protected abstract void createPluginResourceBundle(FileObject prjDirFO, FileObject pkgFolder) throws IOException;
    protected abstract void createPluginLayerXml(FileObject prjDirFO, FileObject pkgFolder) throws IOException;
    protected abstract void createPluginManifest(FileObject prjDirFO) throws IOException;
    protected abstract void createProjectMetadata(FileObject prjDirFO) throws IOException;
    protected abstract void createProjectSpecificArtifacts(FileObject prjDirFO) throws IOException;
    
    private void createDeployPluginProject() throws IOException {
        FileObject prjDirFO = createProjectCommonArtifacts();
        createProjectSpecificArtifacts(prjDirFO);
    }
    /**
     * return project direcotry file object
     */
    private FileObject createProjectCommonArtifacts() throws IOException {
        
        File prjDirFile = this.getProjectDirectory();
        if (prjDirFile != null) {
            prjDirFile = FileUtil.normalizeFile(prjDirFile);
        }
        
        String prjName = this.getProjectName();
        List prjRefList = new ArrayList();
        String compType = this.getComponentType();
        String compName = this.getComponentName();
        
        FileObject prjDirFO = null;
        prjDirFO = createProjectDir(prjDirFile);
        
        FileUtil.createFolder(prjDirFO, "test");
        
        FileObject srcFolder = FileUtil.createFolder(prjDirFO, "src");
        String packageName = this.getPackageName();
        FileObject pkgFolder = this.createPackage(srcFolder, packageName);
        
        createPluginResourceBundle(prjDirFO, pkgFolder);
        createPluginLayerXml(prjDirFO, pkgFolder);
        createPluginManifest(prjDirFO);
        
        createProjectMetadata(prjDirFO);
        
        Project p = ProjectManager.getDefault().findProject(prjDirFO);
        
        File prjParentFile = null;
        
        // BCProjectWizard sets the "rememberProjectsFolder" to false
        boolean setProjectsFolder = true;
        WizardDescriptor wizDesc = this.getWizardDescriptor();
        if ( wizDesc != null ) {
            Object value = wizDesc.getProperty("DoNotSetProjectsFolder");
            if ( value != null ) {
                setProjectsFolder = false;
            }
        }
        if (setProjectsFolder) {
            prjParentFile = (prjDirFile != null) ? prjDirFile.getParentFile() : null;
            if (prjParentFile != null && prjParentFile.exists()) {
                ProjectChooser.setProjectsFolder(prjParentFile);
            }
        }
        
        this.addCreatedFileObject(prjDirFO);
        
        return prjDirFO;
        
    }
    
    protected void addJbiCompInfoTokens(Map tokenMap) {
        
        tokenMap.put("JBI_COMP_NAME", this.getComponentName());
        tokenMap.put("JBI_COMP_DESC", this.getComponentDescription());
    }
    
    protected FileObject createJbiXml(FileObject prjDirFO, String jbiXmlTemplatePath, Map tokenMap ) throws IOException {
        
        FileObject confRoot = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.CONF_DIR_VALUE);
        // create meta-inf dir
        FileObject metaInfFolder = FileUtil.createFolder(confRoot, "META-INF");
        
        FileObject jbiXmlFO = createFromTemplateFileObject(metaInfFolder, jbiXmlTemplatePath, null, tokenMap);
        return jbiXmlFO;
    }
    
    protected FileObject createPackage(FileObject srcRoot, String packageName) throws IOException {
        FileObject pkgFolder = srcRoot;
        
        if ( packageName != null ) {
            String pkgFileName = packageName.replace('.', '/');
            pkgFolder = FileUtil.createFolder( srcRoot, pkgFileName );
        }
        return pkgFolder;
    }
    
    protected StringBuffer readFromFO(FileObject fo) {
        InputStream inS = null;
        StringWriter writer = new StringWriter();
        PrintWriter out = new PrintWriter(writer);
        
        try {
            inS = fo.getInputStream();
            InputStreamReader reader = new InputStreamReader(inS);
            BufferedReader inReader = new BufferedReader(reader);
            
            for (String line = null; (line = inReader.readLine()) != null ;) {
                out.println(line);
            }
            out.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            TemplateUtil.closeInputStream(inS);
            TemplateUtil.closeWriter(out);
            TemplateUtil.closeWriter(writer);
            
        }
        
        return writer.getBuffer();
    }
    
    protected StyledDocument createDocument() {
        EditorKit kit = JEditorPane.createEditorKitForContentType("text/x-java"); // NOI18N
        if (kit == null) {
            kit = new DefaultEditorKit();
        }
        Document doc = kit.createDefaultDocument();
        if (doc instanceof StyledDocument) {
            return (StyledDocument)doc;
        }
        return new FilterDocument(doc);
    }
    
    protected StringBuffer toFormattedJavaSource(StringBuffer javaSrc) {
        StringWriter sw = new StringWriter();
        StyledDocument doc = createDocument();
        IndentEngine indentator = IndentEngine.find(doc);
        PrintWriter pw = new PrintWriter(indentator.createWriter(doc, 0, sw));
        //    PrintWriter pw = new PrintWriter(sw);
        try {
            pw.print(javaSrc.toString());
        } catch (Exception ex ) {
            ex.printStackTrace();
        } finally {
            pw.close();
        }
        return sw.getBuffer();
    }
    
    /**
     * create verbatim. no token replacement
     */
    protected FileObject createFromTemplate(FileObject folder, String jbiTemplatePath) {
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject templateFO = jbiSupportFolder.getFileObject(jbiTemplatePath);
        FileObject newFO = null;
        if ( templateFO != null ) {
            try {
                String name = templateFO.getName();
                // System.out.println("Copying the file " + name);
                newFO = FileUtil.copyFile(templateFO, folder, name);
            } catch (IOException ex) {
                Util.logDebug(ex);
                ex.printStackTrace();
            }
        } else {
            System.out.println("Template not found " + jbiTemplatePath);
        }
        return newFO;
    }
    
    protected FileObject createFromJavaTemplate(FileObject destFO,
            String jbiTemplatePath, String name) throws IOException {
        
        FileObject fo = createFromTemplateDataObject(destFO, jbiTemplatePath, name);
        
        DataObject dataObj = null;
        FileLock outLock = null;
        OutputStream outS = null;
        InputStream inS = null;
        
        try {
            dataObj = DataObject.find(fo);
//            SourceCookie srcCookie = (SourceCookie) dataObj.getCookie(SourceCookie.class);
//            SourceElement srcEl = srcCookie.getSource();
            // String javaSrc = srcEl.toString();
            StringBuffer javaSrc = readFromFO(fo);
            javaSrc = toFormattedJavaSource(javaSrc);
            // System.out.println(javaSrc);
            inS = new ByteArrayInputStream(javaSrc.toString().getBytes());
            outLock = fo.lock();
            outS = fo.getOutputStream(outLock);
            FileUtil.copy(inS, outS);
        } catch ( Exception ex) {
            ex.printStackTrace();
        } finally {
            TemplateUtil.closeInputStream(inS);
            TemplateUtil.closeOutputStream(outS);
            if ( outLock != null ) {
                outLock.releaseLock();
            }
        }
        return fo;
    }
    
    protected FileObject createFromTemplateDataObject(FileObject destFO,
            String jbiTemplatePath, String name ) throws IOException {
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject templateFO = jbiSupportFolder.getFileObject(jbiTemplatePath);
        DataObject templateDO = null;
        DataObject newDO = null;
        FileObject newFO = null;
        
        if ( templateFO == null ) {
            return null;
        }
        String newName = ( name != null ) ? name : templateFO.getName();
        try {
            templateDO = DataObject.find(templateFO);
            newDO = templateDO.createFromTemplate(DataFolder.findFolder(destFO),newName);
            newFO = newDO.getPrimaryFile();
        } catch (Exception ex) {
            ex.printStackTrace();
            newFO = FileUtil.copyFile(templateFO, destFO, newName);
        }
        return newFO;
    }
    
    protected FileObject createFromTemplateFileObject(FileObject destFO,
            String jbiTemplatePath, String name, Map tokenMap ) throws IOException {
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject templateFO = jbiSupportFolder.getFileObject(jbiTemplatePath);
        DataObject templateDO = null;
        DataObject newDO = null;
        FileObject newFO = null;
        
        if ( templateFO == null ) {
            System.out.println("createFromTemplateFileObject: template not found " + jbiTemplatePath);
            return null;
        }
        String newName = ( name != null ) ? name : templateFO.getName();
        
        HashMap map = new HashMap();
        Map defMap = TemplateUtil.createDefaultTokenMap(newName, templateFO.getExt());
        map.putAll(defMap);
        map.putAll(tokenMap);
        
        newFO = TemplateUtil.createFromTemplate(templateFO, map, destFO, newName);
        
        return newFO;
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
}
