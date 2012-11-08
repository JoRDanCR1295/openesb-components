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
import java.util.Set;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.modules.jbi.apisupport.JbiAdminSettings;
import org.netbeans.modules.jbi.apisupport.common.TemplateUtil;
import org.netbeans.modules.jbi.apisupport.common.Util;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProject;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;

/**
 * This Wizard model contains the data and methods to generte the jbi component
 * project with Service Engine artifacts
 *
 * @author  chikkala
 */
public class SEProjectWizardModel extends JbiComponentProjectWizardModel {
    
    /** Creates a new instance of ComponentProjectWizardModel */
    public SEProjectWizardModel(WizardDescriptor wiz) {
        this(wiz, COMP_PRJ_WIZARD_TYPE );
    }
    /** Creates a new instance of ComponentProjectWizardModel */
    public SEProjectWizardModel(WizardDescriptor wiz, String wizType) {
        super(wiz,wizType);
    }
    
    protected void createProjectSpecificArtifacts(FileObject prjDirFO) throws IOException {
        // createServiceEngineProjectArtifacts(prjDirFO);
        createEngineSpecificArtifacts(prjDirFO);
        updateEngineSpecificTestArtifacts(prjDirFO);
        createSEPluginProject(prjDirFO);
    }
    
    protected void updateEngineSpecificTestArtifacts(FileObject prjDirFO) {
        Map<String,String> tokenMap = new HashMap<String,String>();
        
        String httpBCPort = "12010";  // this could be ${DefaultHttpPort}        
        String compName = this.getComponentName();
        JbiAdminSettings jbiAdminSettings =
                JbiAdminSettings.JbiAdminSettingsImpl.getJbiAdminSettings(this.getTargetServerID());
        String host = jbiAdminSettings.getHost();
        
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("HTTP_BC_HOST", host);
        tokenMap.put("HTTP_BC_PORT", httpBCPort);
        tokenMap.put("JUNIT", "");
        FileObject testDirFO = prjDirFO.getFileObject(JbiCompProjectProperties.TEST_BASE_DIR_VALUE);
        for(Enumeration en = testDirFO.getData(true); en.hasMoreElements();) {
            FileObject dataFO = (FileObject) en.nextElement();
            TemplateUtil.replaceTokens(dataFO, tokenMap);
        }                  
    }
    
    private void createEngineTestArtifacts(FileObject prjDirFO ) throws IOException {
        
        String testSAJbiXmlTemplatePath = "codegen/components/engine/test/testSA_jbi.xml";
        String testSUJbiXmlTemplatePath = "codegen/components/engine/test/testSU_jbi.xml";
        String engineTestPropsTPath = "codegen/components/engine/test/engineTest.properties";
        String testWSDLTemplatePath = "codegen/components/engine/test/EchoSoapBinding.wsdl";
        
        String compName = this.getComponentName();
        String seEndpointName = compName + "_JBIPort";
        String soapAddressLocation = "http://localhost:12010/" + compName + "/echo";
        
        String testSAName = compName + "_TestSA";
        String testSUName = compName + "_TestSU";
        
        Map<String,String> tokenMap = new HashMap<String,String>();
        
        tokenMap.put("SE_ENDPOINT_NAME", seEndpointName);
        tokenMap.put("SOAP_ADDRESS_LOCATION", soapAddressLocation);
        
        tokenMap.put("TEST_SA_NAME", testSAName);
        tokenMap.put("TEST_SU_NAME", testSUName);
        tokenMap.put("TEST_SU_TARGET_COMP", JbiCompProjectProperties.JBI_SOAP_BC_NAME_VALUE);
        
        FileObject tempFO = null;
        
        FileObject testDirFO = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.TEST_BASE_DIR_VALUE);
        FileObject saMetaInfFolder = FileUtil.createFolder(testDirFO, "testSA/src/META-INF");
        tempFO = createFromTemplateFileObject(saMetaInfFolder, testSAJbiXmlTemplatePath, "jbi", tokenMap);
        
        FileObject suMetaInfFolder = FileUtil.createFolder(testDirFO, "testSA/testSU/src/META-INF");
        tempFO = createFromTemplateFileObject(suMetaInfFolder, testSUJbiXmlTemplatePath, "jbi", tokenMap);
        
        FileObject suSrcFolder = FileUtil.createFolder(testDirFO, "testSA/testSU/src");
        tempFO = createFromTemplateFileObject(suSrcFolder, testWSDLTemplatePath, "Echo", tokenMap);
        
        FileObject javaTestDirFO = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.TEST_DIR_VALUE);
        FileObject testFolder = FileUtil.createFolder(javaTestDirFO, "enginetest");
        tempFO = createFromTemplateFileObject(testFolder, engineTestPropsTPath, "test", tokenMap);
        
    }
    
    private void updateEngineEchoServiceArtifacts(FileObject prjDirFO ) throws IOException {
        
        String compName = this.getComponentName();
        String seEndpointName = compName + "_JBIPort";
        String soapAddressLocation = "http://localhost:12010/" + compName + "/echo";
        
        Map<String,String> tokenMap = new HashMap<String,String>();
        
        tokenMap.put("SE_ENDPOINT_NAME", seEndpointName);
        tokenMap.put("SOAP_ADDRESS_LOCATION", soapAddressLocation);
        
        FileObject srcRoot = prjDirFO.getFileObject(JbiCompProjectProperties.SRC_DIR_VALUE);
        FileObject pkgFolder = createPackage(srcRoot, this.getPackageName());
        
        // create wsdl files
        String echoWsdlTemplatePath = "codegen/components/engine/Echo.wsdl";
        FileObject echoWsdlFO = createFromTemplateFileObject(pkgFolder, echoWsdlTemplatePath, "Echo", tokenMap);
        
        // update the ServiceEngineLifeCycle.java with the correct JBIEndpoint
        //readFromFO
        FileObject lcFO = pkgFolder.getFileObject("ServiceEngineLifeCycle.java");
        if (lcFO == null ) {
            System.out.println("CAN NOT FIND the ServiceEngineLifeCycle.java FO in " + pkgFolder.getPath());
            return;
        }
        
        TemplateUtil.replaceTokens(lcFO, tokenMap);
    }
    
    private void createServiceEngineProjectArtifacts(FileObject prjDirFO ) throws IOException {
        // unpack the standard files
        String engineArtifactsZipTemplatePath = "codegen/components/engine/engine-artifacts.zip";
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject engineArtifactsZipFO = jbiSupportFolder.getFileObject(engineArtifactsZipTemplatePath);
        Util.unZipArchive(engineArtifactsZipFO.getInputStream(), prjDirFO);
        
        // create specific files with customization
        
        FileObject srcRoot = prjDirFO.getFileObject(JbiCompProjectProperties.SRC_DIR_VALUE);
        // create package
        FileObject pkgFolder = createPackage(srcRoot, this.getPackageName());
        
        // create java files
        String btClassTemplatePath = "codegen/components/engine/ServiceEngineInstaller.java";
        String lcClassTemplatePath = "codegen/components/engine/ServiceEngineLifeCycle.java";
        String suMgrClassTemplatePath = "codegen/components/engine/ServiceEngineSUManager.java";
        String rtClassTemplatePath = "codegen/components/engine/ServiceEngineRuntime.java";
        
        String msgExHandlerClassTemplatePath = "codegen/components/engine/ServiceEngineMessageExchangeHandler.java";
        String msgExHandlerFactoryClassTemplatePath = "codegen/components/engine/ServiceEngineMessageExchangeHandlerFactory.java";
        String echoClassTemplatePath = "codegen/components/engine/Echo.java";
        
        FileObject btClassFO = createFromJavaTemplate(pkgFolder, btClassTemplatePath, null);
        FileObject lcClassFO = createFromJavaTemplate(pkgFolder, lcClassTemplatePath, null);
        FileObject suMgrClassFO = createFromJavaTemplate(pkgFolder, suMgrClassTemplatePath, null);
        FileObject rtClassFO = createFromJavaTemplate(pkgFolder, rtClassTemplatePath, null);
        
        FileObject mxHandlerClassFO = createFromJavaTemplate(pkgFolder, msgExHandlerClassTemplatePath, null);
        FileObject mxHandlerFactoryClassFO = createFromJavaTemplate(pkgFolder, msgExHandlerFactoryClassTemplatePath, null);
        
        FileObject echoClassFO = createFromJavaTemplate(pkgFolder, echoClassTemplatePath, null);
        
        // create jbi.xml
        String jbiXmlTemplatePath = "codegen/components/engine/jbi.xml";
        Map map = createJbiXmlTokenMap(rtClassFO.getName(), btClassFO.getName());
        FileObject jbixmlFO = createJbiXml(prjDirFO, jbiXmlTemplatePath, map);
        
        this.addCreatedFileObject(jbixmlFO);
        this.addCreatedFileObject(lcClassFO);
        this.addCreatedFileObject(rtClassFO);
//        this.addCreatedFileObject(btClassFO);
//        this.addCreatedFileObject(suMgrClassFO);
        
        this.addFileObjectForOpen(jbixmlFO);
        this.addFileObjectForOpen(lcClassFO);
        this.addFileObjectForOpen(rtClassFO);
//        this.addFileObjectForOpen(btClassFO);
//        this.addFileObjectForOpen(suMgrClassFO);
        
        // create test files
        
        createEngineTestArtifacts(prjDirFO);
        
        updateEngineEchoServiceArtifacts(prjDirFO);
        
    }
    
    private void createEngineSpecificArtifacts(FileObject prjDirFO) throws IOException {
        
        DataFolder prjDir = DataFolder.findFolder(prjDirFO); 
        FileObject srcDirFO = prjDirFO.getFileObject(JbiCompProjectProperties.SRC_DIR_VALUE);
        // create package
        FileObject pkgDirFO = createPackage(srcDirFO, this.getPackageName());
        DataFolder pkgDir = DataFolder.findFolder(pkgDirFO);
        
        String seArtifactsZipTemplatePath = "codegen/components/engine/se-artifacts.zip";
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject seArtifactsZipFO = jbiSupportFolder.getFileObject(seArtifactsZipTemplatePath);
        FileObject zipRoot = Util.getArchiveRoot(seArtifactsZipFO);

        // copy test source
        FileObject zipTestFolderFO = zipRoot.getFileObject("test");
        DataFolder zipTestFolder = DataFolder.findFolder(zipTestFolderFO);
        try {
            zipTestFolder.copy(prjDir);
        } catch (Exception ex) {
            ex.printStackTrace();
            // continue
        }
        
        // copy source
        FileObject zipPkgDirFO = zipRoot.getFileObject("src/java/com/sun/jbi/sample/engine");
                
        for(Enumeration en = zipPkgDirFO.getData(false); en.hasMoreElements();) {
            FileObject dataFO = (FileObject) en.nextElement();
            DataObject data = DataObject.find(dataFO);
            data.createFromTemplate(pkgDir);
        }          
                
        FileObject btClassFO = pkgDirFO.getFileObject("ProviderSEInstaller.java");
        FileObject lcClassFO = pkgDirFO.getFileObject("ProviderSEComponentLifeCycle.java");
        FileObject suMgrClassFO = pkgDirFO.getFileObject("ProviderSESUManager.java");
        FileObject rtClassFO = pkgDirFO.getFileObject("ProviderSERuntime.java");
        
        // create jbi.xml
        String jbiXmlTemplatePath = "codegen/components/common/jbi.xml";
        Map map = createJbiXmlTokenMap(rtClassFO.getName(), btClassFO.getName());
        FileObject jbixmlFO = createJbiXml(prjDirFO, jbiXmlTemplatePath, map);
        
        this.addCreatedFileObject(jbixmlFO);
        this.addCreatedFileObject(lcClassFO);
        this.addCreatedFileObject(rtClassFO);
        
        this.addFileObjectForOpen(jbixmlFO);
        this.addFileObjectForOpen(lcClassFO);
        this.addFileObjectForOpen(rtClassFO);
        
        // createWsdlExtArtifacts(prjDirFO, pkgDirFO);
        
    }
    
    private void createSEPluginProject(FileObject prjDirFO ) throws IOException {
        
        WizardDescriptor wizDesc = null;
        wizDesc = new WizardDescriptor(new WizardDescriptor.ArrayIterator());
        SEDeployPluginWizardModel pluginWizModel =
                (SEDeployPluginWizardModel) DeployPluginWizardModel.createDeployPluginWizardModel(wizDesc, SERVICE_ENGINE_COMP_TYPE);
        pluginWizModel.initialize();
        
        wizDesc.putProperty("DoNotSetProjectsFolder", Boolean.TRUE); // don't remember this folder as current projects folder
        wizDesc.putProperty("setAsMain", Boolean.FALSE); // don't set this project as a main project.
        
        File parentDir = this.getProjectDirectory();
        String prjName = this.getProjectName();
        pluginWizModel.setProjectDirectory(new File(parentDir, "deploy-plugin" ) );
        
        pluginWizModel.setProjectName("JBI Deployment Plugin[" + prjName + "]"); //TODO i18n for formatting
        
        pluginWizModel.setPackageName(this.getPackageName());
        pluginWizModel.setComponentName(this.getComponentName());
        pluginWizModel.setComponentDescription(this.getComponentDescription());
                
        pluginWizModel.setSampleCode(true);
        
        Set set = pluginWizModel.instantiate();
        getCreatedFileObjectSet().addAll(set);
        
        Project p = ProjectManager.getDefault().findProject(prjDirFO);
        JbiCompProject jbiCompPrj = p.getLookup().lookup(JbiCompProject.class);
        AntProjectHelper prjHelper = jbiCompPrj.getAntProjectHelper();
        EditableProperties ep = prjHelper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        ep.setProperty(JbiCompProjectProperties.PROJECT_JBI_DEPLOY_PLUGIN,
                JbiCompProjectProperties.PROJECT_JBI_DEPLOY_PLUGIN_VALUE );
        prjHelper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, ep);
        
    }    
}
