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

import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import org.netbeans.modules.jbi.apisupport.common.TemplateUtil;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;

/**
 * This Wizard model contains the data and methods to generte the jbi component
 * project with Binding Component artifacts
 *
 * @author  chikkala
 */
public class SEDeployPluginWizardModel extends DeployPluginWizardModel {
    
    private final static String ZIP_PRJ_PKG_PATH = "org/netbeans/modules/jbi/sample/engine/project";
    
    private final static String PRJ_CONFIG_NS_PREFIX = "http://www.netbeans.org/ns/jbimodules/"; // NOI18N    
    
    /** set to true if the model need to generate the sample artifacts for SE generated in SE project */
    private boolean mIsSampleCode = false; 
    
    private String mProjectTypeNS;
    private String mPrjPrivateNS;
        
    /** Creates a new instance of ComponentProjectWizardModel */
    public SEDeployPluginWizardModel(WizardDescriptor wiz) {
        this(wiz, DEPLOY_PLUGIN_WIZARD_TYPE );
    }
    /** Creates a new instance of ComponentProjectWizardModel */
    public SEDeployPluginWizardModel(WizardDescriptor wiz, String wizType) {
        super(wiz,wizType);
    }
    
    /**
     * Setter for property name.
     * @param name New value of property name.
     */
    @Override
    public void setComponentName(String name) {
        super.setComponentName(name);
    }
    
    public String getProjectTypeNS() {
        if ( this.mProjectTypeNS == null ) {
            return getDefProjectTypeNS();
        } else {
            return this.mProjectTypeNS;
        }
    }
    
    public void setProjectTypeNS(String prjTypeNS) {
        this.mProjectTypeNS = prjTypeNS;
    }

    public static String getDefProjectTypeNS(String compName) {
        return PRJ_CONFIG_NS_PREFIX + compName + "/1";
    }
    
    public String getDefProjectTypeNS() {
        String compName = this.getComponentName();
        if ( compName == null || compName.trim().length() == 0 ) {
            compName = SAMPLE_SE_NAME;
        }
        return getDefProjectTypeNS(compName);
    }
        
    public String getProjectPrivateNS() {
        if ( this.mPrjPrivateNS == null ) {
            return getDefProjectPrivateNS();
        } else {
            return this.mPrjPrivateNS;
        }
    }
    
    public void setProjectPrivateNS(String prjPrivateNS) {
        this.mPrjPrivateNS = prjPrivateNS;
    }
    
    public static String getDefProjectPrivateNS(String compName) {
        return PRJ_CONFIG_NS_PREFIX + compName + "/private/1";
    }
    
    public String getDefProjectPrivateNS() {
        String compName = this.getComponentName();
        if ( compName == null || compName.trim().length() == 0 ) {
            compName = SAMPLE_SE_NAME;
        }
        return getDefProjectPrivateNS(compName);
    }
    /**
     *  set to true if the model need to generate the sample artifacts for SE generated in SE project
     */
    public void setSampleCode(boolean sample) {
        this.mIsSampleCode = sample;
    }
    
    public boolean isSampleCode() {
        return this.mIsSampleCode;
    }
    
    protected void createProjectSpecificArtifacts(FileObject prjDirFO) throws IOException {
        
        FileObject srcFolder = FileUtil.createFolder(prjDirFO, "src");
        String packageName = this.getPackageName();
        FileObject pkgFolder = this.createPackage(srcFolder, packageName);
                
        try {
            createProjectFromPluginArchive(prjDirFO);
        } catch (Exception ex) {
          ex.printStackTrace();
        }
    }
    
    
    protected void createPluginResourceBundle(FileObject prjDirFO, FileObject pkgFolder) throws IOException {
        String pluginBundleTemplatePath = "codegen/components/engine/plugin/Bundle.properties";
        
        String compName = this.getComponentName();
        String pluginName = this.getProjectName();
        String shortDesc = "Service engine deployment plugin for " + compName;
        String longDesc = "Deployment plugin for creating service units related to this service engine in composite application";
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        tokenMap.put("PLUGIN_NAME", pluginName);
        tokenMap.put("PLUGIN_SHORT_DESC", shortDesc);
        tokenMap.put("PLUGIN_LONG_DESC", longDesc);
        tokenMap.put("JBI_COMP_NAME", compName);
        
        FileObject bundleFO = createFromTemplateFileObject(pkgFolder, pluginBundleTemplatePath, null, tokenMap);
    }
    
    protected void createPluginLayerXml(FileObject prjDirFO, FileObject pkgFolder) throws IOException {
        
        String layerXmlTemplatePath = "codegen/components/engine/plugin/layer.xml";
        String pkgName = this.getPackageName();
        String compName = this.getComponentName();
        String compDesc = this.getComponentDescription();
        String pkgPath = pkgName.replace('.', '/');
                        
        Map<String, String> tokenMap = new HashMap<String, String>();
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("JBI_COMP_DESC", compDesc);
        tokenMap.put("BASE_PACKAGE", pkgName);
        tokenMap.put("BASE_PACKAGE_PATH", pkgPath);
                
        FileObject bundleFO = createFromTemplateFileObject(pkgFolder, layerXmlTemplatePath, null, tokenMap);
        
    }
    
    protected void createPluginManifest(FileObject prjDirFO) throws IOException {
        String manifestTemplatePath = "codegen/components/engine/plugin/manifest.mf";
        
        String packageName = this.getPackageName();
        String pkgPath = packageName.replace('.', '/');
        
        String codeNameBase = packageName;
        String layerXmlPath = pkgPath + "/layer.xml";
        String i18nBundlePath = pkgPath + "/Bundle.properties";
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        tokenMap.put("CODE_NAME_BASE", codeNameBase);
        tokenMap.put("LAYER_XML_PATH", layerXmlPath);
        tokenMap.put("LAYER_I18N_BUNDLE_PATH", i18nBundlePath);
        
        FileObject bundleFO = createFromTemplateFileObject(prjDirFO, manifestTemplatePath, null, tokenMap);
    }
    
    protected void createProjectMetadata(FileObject prjDirFO) throws IOException {
        String projectXmlTemplatePath = "codegen/components/engine/plugin/project.xml";
        
        String projectPropsTemplatePath = "codegen/components/engine/plugin/project.properties";
        String platformPropsTemplatePath = "codegen/components/engine/plugin/platform.properties";
        
        String projectXmlPath = AntProjectHelper.PROJECT_XML_PATH;
        String nbPrjFolderPath = (new File(projectXmlPath)).getParent();
        FileObject nbPrjFolder = FileUtil.createFolder(prjDirFO, nbPrjFolderPath);
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        String packageName = this.getPackageName();
        tokenMap.put("CODE_NAME_BASE", packageName);
        
        createFromTemplateFileObject(nbPrjFolder, projectPropsTemplatePath, null, tokenMap);
        createFromTemplateFileObject(nbPrjFolder, platformPropsTemplatePath, null, tokenMap);
        
        FileObject prjXmlFO = createFromTemplateFileObject(nbPrjFolder, projectXmlTemplatePath, null, tokenMap);
        
    }
        
    protected void createProjectFromPluginArchive(FileObject prjDirFO) throws IOException {

        String packageName = this.getPackageName();

        FileObject srcFolderFO = FileUtil.createFolder(prjDirFO, "src");
        DataFolder srcFolder = DataFolder.findFolder(srcFolderFO);
        FileObject pkgFolderFO = this.createPackage(srcFolderFO, packageName);
        DataFolder pkgFolder = DataFolder.findFolder(pkgFolderFO);

        String zipFilePath = "codegen/components/engine/plugin/engine-plugin.zip";
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject zipFO = jbiSupportFolder.getFileObject(zipFilePath);                
        FileObject zipRootFO = Util.getArchiveRoot(zipFO);
        
        if ( zipRootFO == null ) {
            System.out.println("ArchiveRoot was not found for zip file");
            return;
        }
        
        String zipPrjPkgPath = "org/netbeans/modules/jbi/sample/engine/project";
        
        FileObject zipSrcFolderFO = zipRootFO.getFileObject("src");
        FileObject zipPrjFolderFO = zipSrcFolderFO.getFileObject(zipPrjPkgPath);
        
        try {
           createMetaInfServicesCode(srcFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
           createProjectPackageCode(pkgFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
           createPrjResourceCode(pkgFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
            createPrjCustomizerPackageCode(pkgFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        try {
            createPrjNodePackageCode(pkgFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }        
        
        try {
           createPrjWizardPackageCode(pkgFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }        
                
    } 

    protected void createMetaInfServicesCode(FileObject srcFolderFO, FileObject zipSrcFolderFO) throws IOException {
        FileObject zipMetaInfSvcFolderFO = zipSrcFolderFO.getFileObject("META-INF/services");
        DataFolder zipMetaInfSvcFolder = DataFolder.findFolder(zipMetaInfSvcFolderFO);

        FileObject metaInfFolderFO = FileUtil.createFolder(srcFolderFO, "META-INF");
        DataFolder metaInfFolder = DataFolder.findFolder(metaInfFolderFO);

        zipMetaInfSvcFolder.createFromTemplate(metaInfFolder);
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        String packageName = this.getPackageName();
        
        tokenMap.put("BASE_PACKAGE", packageName);
        FileObject servicesFO = srcFolderFO.getFileObject("META-INF/services");
        if ( servicesFO != null ) {
            for(Enumeration en = servicesFO.getData(false); en.hasMoreElements();) {
                FileObject data = (FileObject) en.nextElement();
                TemplateUtil.replaceTokens(data, tokenMap);
            }                        
        } else {
            System.out.println("META-INF/services not found in src");
        }
    }
    
    protected void createProjectPackageCode(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {
        DataFolder pkgFolder = DataFolder.findFolder(pkgFolderFO);
        
        FileObject zipPrjFolderFO = zipSrcFolderFO.getFileObject(ZIP_PRJ_PKG_PATH);
        DataFolder zipPrjFolder = DataFolder.findFolder(zipPrjFolderFO);
        
        zipPrjFolder.createFromTemplate(pkgFolder);
        
        // update specific files for tokens 
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        
        String pkgName = this.getPackageName();
        String projectType = pkgName + ".project.type";
        String prjTypeNS = this.getProjectTypeNS();
        String prjPrivateNS = this.getProjectPrivateNS();
                
        tokenMap.put("PROJECT_TYPE", projectType);
        tokenMap.put("PROJECT_TYPE_NS", prjTypeNS);
        tokenMap.put("PROJECT_PRIVATE_NS", prjPrivateNS);
        
        FileObject prjFolderFO = pkgFolderFO.getFileObject("project");
        FileObject javaFO = prjFolderFO.getFileObject("SEPluginProjectType.java");        
        TemplateUtil.replaceTokens(javaFO, tokenMap);    
        
        tokenMap = new HashMap<String, String>();        
        String pkgPath = pkgName.replace('.', '/');
        String compName = this.getComponentName();
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("BASE_PACKAGE_PATH", pkgPath);        
        javaFO = prjFolderFO.getFileObject("SEPluginProjectProperties.java");        
        TemplateUtil.replaceTokens(javaFO, tokenMap);   
        
        tokenMap = new HashMap<String, String>();
        String custPackage = pkgName + ".project.customizer";
        tokenMap.put("CUSTOMIZER_PACKAGE", custPackage);
        javaFO = prjFolderFO.getFileObject("SEPluginProject.java");        
        TemplateUtil.replaceTokens(javaFO, tokenMap);   

        tokenMap = new HashMap<String, String>();
        String nodePackage = pkgName + ".project.node";
        tokenMap.put("NODE_PACKAGE", nodePackage);
        javaFO = prjFolderFO.getFileObject("SEPluginProjectLogicalViewProvider.java");        
        TemplateUtil.replaceTokens(javaFO, tokenMap);   
        
    }    

    protected void createPrjResourceCode(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {

        FileObject prjFolderFO = pkgFolderFO.getFileObject("project");

        Map<String, String> tokenMap = new HashMap<String, String>();
        String prjTypeNS = this.getProjectTypeNS();
        
        tokenMap.put("PROJECT_TYPE_NS", prjTypeNS);
        
        FileObject prjResFolderFO = prjFolderFO.getFileObject("resources");
        FileObject javaFO = prjResFolderFO.getFileObject("build.xsl");        
        TemplateUtil.replaceTokens(javaFO, tokenMap);    
        
        javaFO = prjResFolderFO.getFileObject("build-impl.xsl");
        TemplateUtil.replaceTokens(javaFO, tokenMap);
        
    }        
    
    protected void createPrjCustomizerPackageCode(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {
        FileObject prjFolderFO = pkgFolderFO.getFileObject("project");
        DataFolder prjFolder = DataFolder.findFolder(prjFolderFO);
        
        FileObject zipPrjCustFolderFO = zipSrcFolderFO.getFileObject(ZIP_PRJ_PKG_PATH + "/customizer");
        DataFolder zipPrjCustFolder = DataFolder.findFolder(zipPrjCustFolderFO);
        
        FileObject prjCustFolderFO = prjFolderFO.getFileObject("customizer");
        Map<String, String> tokenMap = new HashMap<String, String>();
        String pkgName = this.getPackageName();
        tokenMap.put("BASE_PACKAGE", pkgName);
        FileObject javaFO = prjCustFolderFO.getFileObject("SEPluginProjectCustomizerModel.java");
        TemplateUtil.replaceTokens(javaFO, tokenMap);   
        
        tokenMap = new HashMap<String, String>();
        String pkgPath = pkgName.replace('.', '/');
        tokenMap.put("BASE_PACKAGE_PATH", pkgPath);        
        FileObject formFO = prjCustFolderFO.getFileObject("CustomizerGeneral.form");
        TemplateUtil.replaceTokens(formFO, tokenMap);   
        formFO = prjCustFolderFO.getFileObject("CustomizerPackage.form");
        TemplateUtil.replaceTokens(formFO, tokenMap);   
        
    }        

    protected void createPrjNodePackageCode(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {
        FileObject prjFolderFO = pkgFolderFO.getFileObject("project");
        DataFolder prjFolder = DataFolder.findFolder(prjFolderFO);
        
        FileObject zipPrjNodeFolderFO = zipSrcFolderFO.getFileObject(ZIP_PRJ_PKG_PATH + "/node");
        DataFolder zipPrjNodeFolder = DataFolder.findFolder(zipPrjNodeFolderFO);
        
        FileObject prjNodeFolderFO = prjFolderFO.getFileObject("node");
        Map<String, String> tokenMap = new HashMap<String, String>();
        String pkgName = this.getPackageName();
        tokenMap.put("BASE_PACKAGE", pkgName);
        FileObject javaFO = prjNodeFolderFO.getFileObject("SEPluginProjectNode.java");
        TemplateUtil.replaceTokens(javaFO, tokenMap);           
        
    }        

    protected void createPrjWizardPackageCode(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {
        FileObject prjFolderFO = pkgFolderFO.getFileObject("project");
        DataFolder prjFolder = DataFolder.findFolder(prjFolderFO);
        
        FileObject zipPrjWizFolderFO = zipSrcFolderFO.getFileObject(ZIP_PRJ_PKG_PATH + "/wizard");
        DataFolder zipPrjWizFolder = DataFolder.findFolder(zipPrjWizFolderFO);
        
        FileObject prjWizFolderFO = prjFolderFO.getFileObject("wizard");
        Map<String, String> tokenMap = new HashMap<String, String>();
        String pkgName = this.getPackageName();
        tokenMap.put("BASE_PACKAGE", pkgName);
        
        StringBuffer suArtifactsListBuff = new StringBuffer();
        suArtifactsListBuff.append("{");
        if (this.isSampleCode()) { 
            suArtifactsListBuff.append("\"xsltmap.properties\",");
            suArtifactsListBuff.append("\"goodbye.xsl\",");
            suArtifactsListBuff.append("\"hello.xsl\",");
            suArtifactsListBuff.append("\"Greetings.wsdl\"");            
        } else {
            suArtifactsListBuff.append("\"SampleWSDL.wsdl\"");
        }
        suArtifactsListBuff.append("};");
        
        tokenMap.put("SU_ARTIFACTS_LIST", suArtifactsListBuff.toString());
        
        FileObject javaFO = prjWizFolderFO.getFileObject("SEPluginProjectWizardIterator.java");
        TemplateUtil.replaceTokens(javaFO, tokenMap);   
        
        tokenMap = new HashMap<String, String>();
        String pkgPath = pkgName.replace('.', '/');
        tokenMap.put("BASE_PACKAGE_PATH", pkgPath);        
        FileObject formFO = prjWizFolderFO.getFileObject("SEPluginProjectWizardPanelVisual.form");
        if ( formFO == null ) {
            System.out.println("#### Form File Object not found");
            // use  FileUtil.findBrother(formFO, pkgPath);
        }
        TemplateUtil.replaceTokens(formFO, tokenMap);   
        
        if ( this.isSampleCode()) {
            fixPrjWizardResPkgCodeForSample(pkgFolderFO, zipSrcFolderFO);
        }
    }        
    
    protected void fixPrjWizardResPkgCodeForSample(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {
        FileObject prjFolderFO = pkgFolderFO.getFileObject("project");
                
        FileObject zipTempPrjWizDirFO = 
                zipSrcFolderFO.getFileObject("org/netbeans/modules/jbi/sample/providerse/project/wizard");
        FileObject zipTempPrjWizResDirFO = zipTempPrjWizDirFO.getFileObject("resources");
        DataFolder zipTempPrjWizResDir = DataFolder.findFolder(zipTempPrjWizResDirFO);
        
        FileObject prjWizFolderFO = prjFolderFO.getFileObject("wizard");
        DataFolder prjWizFolder = DataFolder.findFolder(prjWizFolderFO);
        
        FileObject prjWizResFolderFO = prjWizFolderFO.getFileObject("resources");        
                
        prjWizResFolderFO.delete();
        zipTempPrjWizResDir.createFromTemplate(prjWizFolder);
        prjWizResFolderFO = prjWizFolderFO.getFileObject("resources");
        
        // update Greetings.wsdl and the jbi.xml tokens
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        String compName = this.getComponentName();
        tokenMap.put("JBI_COMP_NAME", compName);
        FileObject xmlFO = prjWizResFolderFO.getFileObject("Greetings.wsdl");
        TemplateUtil.replaceTokens(xmlFO, tokenMap);      
        
        xmlFO = prjWizResFolderFO.getFileObject("jbi.xml");
        TemplateUtil.replaceTokens(xmlFO, tokenMap);           
    }
    
}
