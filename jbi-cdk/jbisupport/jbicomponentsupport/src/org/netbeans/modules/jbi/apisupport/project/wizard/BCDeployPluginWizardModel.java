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
public class BCDeployPluginWizardModel extends DeployPluginWizardModel {
    
    WSDLExtInfo mWsdlExtInfo;
        
    /** Creates a new instance of ComponentProjectWizardModel */
    public BCDeployPluginWizardModel(WizardDescriptor wiz) {
        this(wiz, DEPLOY_PLUGIN_WIZARD_TYPE );
    }
    /** Creates a new instance of ComponentProjectWizardModel */
    public BCDeployPluginWizardModel(WizardDescriptor wiz, String wizType) {
        super(wiz,wizType);
    }
    
    /**
     * Setter for property name.
     * @param name New value of property name.
     */
    @Override
    public void setComponentName(String name) {
        super.setComponentName(name);
        this.getWSDLExtInfo().setComponentName(getComponentName());
    }
    
    public WSDLExtInfo getWSDLExtInfo() {
        
        if ( this.mWsdlExtInfo == null ) {
            this.mWsdlExtInfo = new WSDLExtInfo();
        }
        return this.mWsdlExtInfo;
    }
            
    protected void createProjectSpecificArtifacts(FileObject prjDirFO) throws IOException {
        
        FileObject srcFolder = FileUtil.createFolder(prjDirFO, "src");
        String packageName = this.getPackageName();
        FileObject pkgFolder = this.createPackage(srcFolder, packageName);
        FileObject resFolder = FileUtil.createFolder(pkgFolder, "resources");
                
        String wsdlExtXSDTemplatePath = this.getWSDLExtInfo().getXSDFileTemplate(); // "codegen/components/binding/plugin/bc-wsdl-ext.xsd";
        String wsdlExtNamespace = this.getWSDLExtInfo().getNamespace();
        String wsdlExtXSDFileName = this.getWSDLExtInfo().getXSDFileName();
        
        Map tokenMap = this.getWSDLExtInfo().getWsdlExtXSDFileTokenMap();
        
        FileObject wsdlExtFO = createFromTemplateFileObject(pkgFolder, wsdlExtXSDTemplatePath, wsdlExtXSDFileName, tokenMap);
        
//        createFromTemplate(resFolder, "codegen/components/binding/plugin/binding.png");
//        createFromTemplate(resFolder, "codegen/components/binding/plugin/operation.png");
//        createFromTemplate(resFolder, "codegen/components/binding/plugin/inMessage.png");
//        createFromTemplate(resFolder, "codegen/components/binding/plugin/outMessage.png");
//        createFromTemplate(resFolder, "codegen/components/binding/plugin/fault.png");
//        createFromTemplate(resFolder, "codegen/components/binding/plugin/port.png");

        try {
            createProjectFromPluginArchive(prjDirFO);
        } catch (Exception ex) {
          ex.printStackTrace();
        }
    }
    
    
    protected void createPluginResourceBundle(FileObject prjDirFO, FileObject pkgFolder) throws IOException {
        String pluginBundleTemplatePath = "codegen/components/binding/plugin/Bundle.properties";
        
        String compName = this.getComponentName();
        String pluginName = this.getProjectName();
        String shortDesc = "Binding component deployment plugin for " + compName;
        String longDesc = "Deployment plugin for adding wsdl extensions related to this binding component to the wsdl editor";
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        tokenMap.put("PLUGIN_NAME", pluginName);
        tokenMap.put("PLUGIN_SHORT_DESC", shortDesc);
        tokenMap.put("PLUGIN_LONG_DESC", longDesc);
        tokenMap.put("JBI_COMP_NAME", compName);
        
        FileObject bundleFO = createFromTemplateFileObject(pkgFolder, pluginBundleTemplatePath, null, tokenMap);
    }
    
    protected void createPluginLayerXml(FileObject prjDirFO, FileObject pkgFolder) throws IOException {
        
        String layerXmlTemplatePath = "codegen/components/binding/plugin/layer.xml";
        String pkgName = this.getPackageName();
        String compName = this.getComponentName();
        String compDesc = this.getComponentDescription();
        
        String wsdlExtNamespace = this.getWSDLExtInfo().getNamespace();
        String wsdlExtXSDFileName = this.getWSDLExtInfo().getXSDFileName();
        String wsdlExtNamespacePrefix = this.getWSDLExtInfo().getNamespacePrefix();
        
        String wsdlExtI18NBundle = pkgName + ".Bundle";
        
        String pkgPath = pkgName.replace('.', '/');
        String iconUrlBase = "nbresloc:/" + pkgPath + "/resources/";
        
        String bindingExtIconUrl = iconUrlBase + "binding-ext.png";
        String operationExtIconUrl = iconUrlBase + "operation-ext.png";
        String inputExtIconUrl = iconUrlBase + "input-ext.png";
        String outputExtIconUrl = iconUrlBase + "output-ext.png";
        String faultExtIconUrl = iconUrlBase + "fault-ext.png";
        String portExtIconUrl = iconUrlBase + "port-ext.png";
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("JBI_COMP_DESC", compDesc);
        tokenMap.put("WSDL_EXT_NS_PREFIX", wsdlExtNamespacePrefix);
        tokenMap.put("WSDL_EXT_NAMESPACE", wsdlExtNamespace);
        tokenMap.put("WSDL_EXT_FILE_NAME", wsdlExtXSDFileName);
        tokenMap.put("WSDL_EXT_I18N_BUNDLE", wsdlExtI18NBundle);
        tokenMap.put("BINDING_EXT_ICON_URL", bindingExtIconUrl);
        tokenMap.put("OPERATION_EXT_ICON_URL", operationExtIconUrl);
        tokenMap.put("INPUT_EXT_ICON_URL", inputExtIconUrl);
        tokenMap.put("OUTPUT_EXT_ICON_URL", outputExtIconUrl);
        tokenMap.put("FAULT_EXT_ICON_URL", faultExtIconUrl);
        tokenMap.put("PORT_EXT_ICON_URL", portExtIconUrl);
        
        FileObject bundleFO = createFromTemplateFileObject(pkgFolder, layerXmlTemplatePath, null, tokenMap);
        
    }
    
    protected void createPluginManifest(FileObject prjDirFO) throws IOException {
        String manifestTemplatePath = "codegen/components/binding/plugin/manifest.mf";
        
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
        String projectXmlTemplatePath = "codegen/components/binding/plugin/project.xml";
        
        String projectPropsTemplatePath = "codegen/components/binding/plugin/project.properties";
        String platformPropsTemplatePath = "codegen/components/binding/plugin/platform.properties";
        
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

        String zipFilePath = "codegen/components/binding/plugin/binding-plugin.zip";
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject zipFO = jbiSupportFolder.getFileObject(zipFilePath);                
        FileObject zipRootFO = Util.getArchiveRoot(zipFO);
        
        if ( zipRootFO == null ) {
            System.out.println("ArchiveRoot was not found for zip file");
            return;
        }
                
        FileObject zipSrcFolderFO = zipRootFO.getFileObject("src");
        FileObject zipResourcesFolderFO = zipSrcFolderFO.getFileObject("org/netbeans/modules/wsdlextensions/sample/jmxbinding/resources");
        FileObject zipTemplateFolderFO = zipSrcFolderFO.getFileObject("org/netbeans/modules/wsdlextensions/sample/jmxbinding/template");
        FileObject zipValidatorFolderFO = zipSrcFolderFO.getFileObject("org/netbeans/modules/wsdlextensions/sample/jmxbinding/validator");
                
        DataFolder zipResourcesFolder = DataFolder.findFolder(zipResourcesFolderFO);
        zipResourcesFolder.copy(pkgFolder);

        
        try {
           createMetaInfServicesCode(srcFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
        try {
           createModelPackageCode(pkgFolderFO, zipSrcFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        try {
            createTemplatePackageCode(pkgFolderFO, zipTemplateFolderFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        try {
            createValidatorPackageCode(pkgFolderFO, zipValidatorFolderFO);
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
    protected void createModelPackageCode(FileObject pkgFolderFO, FileObject zipSrcFolderFO) throws IOException {
        FileObject zipModelFolderFO = zipSrcFolderFO.getFileObject("org/netbeans/modules/wsdlextensions/sample/jmxbinding/model");
        DataFolder zipModelFolder = DataFolder.findFolder(zipModelFolderFO);
        DataFolder pkgFolder = DataFolder.findFolder(pkgFolderFO);
        zipModelFolder.createFromTemplate(pkgFolder);
        
        String extNamespace = this.getWSDLExtInfo().getNamespace();
        if ( this.getWSDLExtInfo().isJMXExtensionSchema() ) {
            // the code is already confirmed to jmx extension schema
            return;
        }
        // modify the namespace with sample.binding.model
        FileObject modelFolderFO = pkgFolderFO.getFileObject("model");
        DataFolder modelFolder = DataFolder.findFolder(modelFolderFO);
        FileObject zipTempModelFolderFO = zipSrcFolderFO.getFileObject("org/netbeans/modules/wsdlextensions/sample/binding/model");        
        DataFolder zipTempModelFolder = DataFolder.findFolder(zipTempModelFolderFO);
        
        Map<String, String> tokenMap = new HashMap<String, String>();
        
        String packageName = this.getPackageName();
        String wsdlExtNamespace = this.getWSDLExtInfo().getNamespace();
        String wsdlExtNamespacePrefix = this.getWSDLExtInfo().getNamespacePrefix();
                
        tokenMap.put("BASE_PACKAGE", packageName);
        tokenMap.put("WSDL_EXT_NAMESPACE", wsdlExtNamespace);
        tokenMap.put("WSDL_EXT_NS_PREFIX", wsdlExtNamespacePrefix);
        
        String fileName = "ExtConstants.java";
        FileObject templateFO = zipTempModelFolderFO.getFileObject(fileName);        
        FileObject resultFO = FileUtil.createData(modelFolderFO, fileName);
        TemplateUtil.updateFromTemplate(templateFO, tokenMap, resultFO);
                
        fileName = "ExtAttribute.java";
        templateFO = zipTempModelFolderFO.getFileObject(fileName);        
        resultFO = FileUtil.createData(modelFolderFO, fileName);
        TemplateUtil.updateFromTemplate(templateFO, tokenMap, resultFO);

        fileName = "PortExt.java";
        templateFO = zipTempModelFolderFO.getFileObject(fileName);        
        resultFO = FileUtil.createData(modelFolderFO, fileName);
        TemplateUtil.updateFromTemplate(templateFO, tokenMap, resultFO);
                
    }
    protected void createValidatorPackageCode(FileObject pkgFolderFO, FileObject zipValidatorFolderFO) throws IOException {
        DataFolder zipValidatorFolder = DataFolder.findFolder(zipValidatorFolderFO);
        zipValidatorFolder.createFromTemplate(DataFolder.findFolder(pkgFolderFO));

        FileObject validatorFolderFO = FileUtil.createFolder(pkgFolderFO, "validator");

        Map<String, String> tokenMap = new HashMap<String, String>();

        String packageName = this.getPackageName();
        String modelPkg = packageName + ".model";
        System.out.println("Model package : " + modelPkg);
        tokenMap.put("MODEL_PACKAGE", modelPkg);
        FileObject javaFO = validatorFolderFO.getFileObject("WSDLExtValidator.java");
        TemplateUtil.replaceTokens(javaFO, tokenMap);

        tokenMap = new HashMap<String, String>();

        String wsdlExtNamespace = this.getWSDLExtInfo().getNamespace();
        tokenMap.put("WSDL_EXT_NAMESPACE", wsdlExtNamespace);

        String pkgPath = packageName.replace('.', '/');
        String wsdlExtXsdPath = "/" + pkgPath + "/" + 
                this.getWSDLExtInfo().getXSDFileName() + ".xsd";

        tokenMap.put("WSDL_EXT_XSD_PATH", wsdlExtXsdPath);

        javaFO = validatorFolderFO.getFileObject("WSDLExtValidatorSchemaFactory.java");
        TemplateUtil.replaceTokens(javaFO, tokenMap);

    }    

    protected void createTemplatePackageCode(FileObject pkgFolderFO, FileObject zipTemplateFolderFO) throws IOException {

        // DataFolder zipTemplateFolder = DataFolder.findFolder(zipTemplateFolderFO);
        // zipTemplateFolder.createFromTemplate(DataFolder.findFolder(pkgFolder));

        FileObject templateFolderFO = FileUtil.createFolder(pkgFolderFO, "template");
        DataFolder templateFolder = DataFolder.findFolder(templateFolderFO);
        // create WSDLExtTemplateProvider.java in template folder
        FileObject javaFO = zipTemplateFolderFO.getFileObject("WSDLExtTemplateProvider.java");
        DataObject javaDO = DataObject.find(javaFO);

        javaDO.createFromTemplate(templateFolder);

        // create Bundle.properties and template.xml from the layer xml

        String wsdlExtNamespace = this.getWSDLExtInfo().getNamespace();
        String wsdlExtNamespacePrefix = this.getWSDLExtInfo().getNamespacePrefix();
        String compName = this.getComponentName();

        Map<String, String> tokenMap = new HashMap<String, String>();

        tokenMap.put("UPPER_CASE_JBI_COMP_NAME", compName.toUpperCase());
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("WSDL_EXT_NS_PREFIX", wsdlExtNamespacePrefix);
        // tokenMap.put("WSDL_EXT_NAMESPACE", wsdlExtNamespaceAsProp);

        String extTemplateBundleTemplatePath = "codegen/components/binding/plugin/ExtTemplateBundle.properties";
        FileObject bundleFO = createFromTemplateFileObject(templateFolderFO, 
            extTemplateBundleTemplatePath, "Bundle", tokenMap);
        
        tokenMap = new HashMap<String, String>();
        
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("WSDL_EXT_NS_PREFIX", wsdlExtNamespacePrefix);
        tokenMap.put("WSDL_EXT_NAMESPACE", wsdlExtNamespace);
        // add extension attribute elements
        tokenMap.put("DEF_LOCATION_URL", "REPLACE_WITH_ACTUAL_URL");
        // add jmx bc address extension element attribute defaults
        tokenMap.put("DEF_USERNAME", "admin");
        tokenMap.put("DEF_PASSWORD", "adminadmin");
        tokenMap.put("DEF_SERVICE_URL", "service:jmx:rmi:///jndi/rmi://localhost:8686/jmxrmi");
        tokenMap.put("DEF_MBEAN", 
        "com.sun.jbi.sample.jmxbc:jmxbc-ep-address=" + compName + "/greetings");

        String extTemplateXmlTemplatePath = this.getWSDLExtInfo().getTemplateXMLTemplate();
        FileObject templateXmlFO = createFromTemplateFileObject(templateFolderFO, 
            extTemplateXmlTemplatePath, "template", tokenMap);
    }
}
