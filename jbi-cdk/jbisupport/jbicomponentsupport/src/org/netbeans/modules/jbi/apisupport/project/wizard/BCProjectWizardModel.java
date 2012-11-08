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
 * project with Binding Component artifacts
 *
 * @author  chikkala
 */
public class BCProjectWizardModel extends JbiComponentProjectWizardModel {
        
    /** Creates a new instance of ComponentProjectWizardModel */
    public BCProjectWizardModel(WizardDescriptor wiz) {
        this(wiz, COMP_PRJ_WIZARD_TYPE );
    }
    /** Creates a new instance of ComponentProjectWizardModel */
    public BCProjectWizardModel(WizardDescriptor wiz, String wizType) {
        super(wiz,wizType);
    }
    
    protected void createProjectSpecificArtifacts(FileObject prjDirFO) throws IOException {
        // createBindingComponentProjectArtifacts(prjDirFO);
        try {
            createBindingSpecificArtifacts(prjDirFO);
            updateBindingSpecificTestArtifacts(prjDirFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        createBCPluginProject(prjDirFO);
    }
    
    protected void updateBindingSpecificTestArtifacts(FileObject prjDirFO) {
        Map<String,String> tokenMap = new HashMap<String,String>();
        
        String engineTarget = "ServiceEngine";
        String compName = this.getComponentName();
        
        JbiAdminSettings jbiAdminSettings =
                JbiAdminSettings.JbiAdminSettingsImpl.getJbiAdminSettings(this.getTargetServerID());
        String host = jbiAdminSettings.getHost();
        String port = jbiAdminSettings.getJMXRMIPort();
        String username = jbiAdminSettings.getUsername();
        String password = jbiAdminSettings.getPassword();
        
        
        tokenMap.put("JBI_COMP_NAME", compName);
        tokenMap.put("ENGINE_TARGET", engineTarget);
        
        tokenMap.put("JMX_HOST", host);
        tokenMap.put("JMX_PORT", port);
        tokenMap.put("JMX_USERNAME", username);
        tokenMap.put("JMX_PASSWORD", password);
        tokenMap.put("JUNIT", "");
        
        FileObject testDirFO = prjDirFO.getFileObject(JbiCompProjectProperties.TEST_BASE_DIR_VALUE);
        for(Enumeration en = testDirFO.getData(true); en.hasMoreElements();) {
            FileObject dataFO = (FileObject) en.nextElement();
            TemplateUtil.replaceTokens(dataFO, tokenMap);
        }                  
    }
    
    private void createBindingTestArtifacts(FileObject prjDirFO ) throws IOException {
        
        String testWSDLTemplatePath = "codegen/components/binding/test/EchoJMXBinding.wsdl";
        String bindingTestPropsTPath = "codegen/components/binding/test/bindingTest.properties";
        
        String serviceEngineName = "ServiceEngine";
        
        String bindingName = this.getComponentName();
        
        String jmxAddressMBean = bindingName + "/" + serviceEngineName + "/echo";
        
        JbiAdminSettings jbiAdminSettings =
                JbiAdminSettings.JbiAdminSettingsImpl.getJbiAdminSettings(this.getTargetServerID());
        String host = jbiAdminSettings.getHost();
        String port = jbiAdminSettings.getJMXRMIPort();
        String username = jbiAdminSettings.getUsername();
        String password = jbiAdminSettings.getPassword();
        String jmxAddressUrl = "service:jmx:rmi:///jndi/rmi://" + host + ":" + port + "/jmxrmi";
        
        Map<String,String> tokenMap = new HashMap<String,String>();
        
        tokenMap.put("JMX_ENDPOINT_ADDRESS_MBEAN", jmxAddressMBean);
        tokenMap.put("JMX_ENDPOINT_ADDRESS_URL", jmxAddressUrl);
        tokenMap.put("JMX_ENDPOINT_ADDRESS_USERNAME", username);
        tokenMap.put("JMX_ENDPOINT_ADDRESS_PASSWORD", password);
        
        FileObject tempFO = null;
        
        FileObject javaTestDirFO = FileUtil.createFolder(prjDirFO, JbiCompProjectProperties.TEST_DIR_VALUE);
        FileObject testFolder = FileUtil.createFolder(javaTestDirFO, "bindingtest");
        tempFO = createFromTemplateFileObject(testFolder, bindingTestPropsTPath, "test", tokenMap);
        
        
    }
    
    private void updateBindingEchoServiceArtifacts(FileObject prjDirFO ) throws IOException {
        
        String serviceEngineName = "ServiceEngine";
        String seEndpointName = serviceEngineName + "_JBIPort";
        
        String bindingName = this.getComponentName();
        
        String jmxAddressMBean = bindingName + "/" + serviceEngineName + "/echo";
        
        Map<String,String> tokenMap = new HashMap<String,String>();
        
        tokenMap.put("SE_ENDPOINT_NAME", seEndpointName);
        tokenMap.put("JMX_ENDPOINT_ADDRESS_MBEAN", jmxAddressMBean);
        
        FileObject srcRoot = prjDirFO.getFileObject(JbiCompProjectProperties.SRC_DIR_VALUE);
        FileObject pkgFolder = createPackage(srcRoot, this.getPackageName());
        
        // create wsdl files
//        String echoWsdlTemplatePath = "codegen/components/common/EchoJMXBinding.wsdl";
//        FileObject echoWsdlFO = createFromTemplateFileObject(pkgFolder, echoWsdlTemplatePath, "Echo", tokenMap);
        
        // update the BindingComponentLifeCycle.java with the correct JBIEndpoint
        
        FileObject lcFO = pkgFolder.getFileObject("BindingComponentLifeCycle.java");
        if (lcFO == null ) {
            System.out.println("CAN NOT FIND the BindingComponentLifeCycle.java FO in " + pkgFolder.getPath());
            return;
        }
        
        TemplateUtil.replaceTokens(lcFO, tokenMap);
    }
    
    
    private void createWsdlExtArtifacts(FileObject prjDirFO, FileObject pkgFolder ) throws IOException {
        WSDLExtInfo wsdlExtInfo = new WSDLExtInfo();
        
        wsdlExtInfo.setComponentName(this.getComponentName());       
        wsdlExtInfo.setNamespace(WSDLExtInfo.JMX_WSDL_EXT_NS);
        wsdlExtInfo.setJMXExtensionSchema(true);
        wsdlExtInfo.setXSDFileName("WSDLExtension");
        FileObject wsdlExtFO = createFromTemplateFileObject(
                pkgFolder.getFileObject("wsdlext"),
                wsdlExtInfo.getXSDFileTemplate(), 
                wsdlExtInfo.getXSDFileName(), 
                wsdlExtInfo.getWsdlExtXSDFileTokenMap());
        
//        this.addCreatedFileObject(wsdlExtFO);
//        this.addFileObjectForOpen(wsdlExtFO);
    }
    
    private void createBindingSpecificArtifacts(FileObject prjDirFO) throws IOException {
        
        DataFolder prjDir = DataFolder.findFolder(prjDirFO); 
        FileObject srcDirFO = prjDirFO.getFileObject(JbiCompProjectProperties.SRC_DIR_VALUE);
        // create package
        FileObject pkgDirFO = createPackage(srcDirFO, this.getPackageName());
        DataFolder pkgDir = DataFolder.findFolder(pkgDirFO);
        
        String bcArtifactsZipTemplatePath = "codegen/components/binding/bc-artifacts.zip";
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject bcArtifactsZipFO = jbiSupportFolder.getFileObject(bcArtifactsZipTemplatePath);
        FileObject zipRoot = Util.getArchiveRoot(bcArtifactsZipFO);
                
        FileObject zipTestFolderFO = zipRoot.getFileObject("test");
        DataFolder zipTestFolder = DataFolder.findFolder(zipTestFolderFO);
        try {
            zipTestFolder.copy(prjDir);
        } catch (Exception ex) {
            ex.printStackTrace(); 
            //continue
        }
        
        // copy source
                
        FileObject zipPkgDirFO = zipRoot.getFileObject("src/java/com/sun/jbi/sample/binding");
        
        // copy wsdlext pacakge source
        FileObject zipWsdlExtDirFO = zipPkgDirFO.getFileObject("wsdlext");
        DataFolder zipWsdlExtDir = DataFolder.findFolder(zipWsdlExtDirFO);
        zipWsdlExtDir.createFromTemplate(pkgDir);
        // copy base package source
        for(Enumeration en = zipPkgDirFO.getData(false); en.hasMoreElements();) {
            FileObject dataFO = (FileObject) en.nextElement();
            DataObject data = DataObject.find(dataFO);
            data.createFromTemplate(pkgDir);
        }          
        
        // update the wsdlext package imports in base package source        
        Map<String, String> tokenMap = new HashMap<String,String>();
        String wsdlExtPackageName = this.getPackageName() + ".wsdlext";
        tokenMap.put("WSDLEXT_PACKAGE", wsdlExtPackageName);
        for(Enumeration en = pkgDirFO.getData(false); en.hasMoreElements();) {
            FileObject dataFO = (FileObject) en.nextElement();
            TemplateUtil.replaceTokens(dataFO, tokenMap);
        }          
        
        FileObject btClassFO = pkgDirFO.getFileObject("JMXBindingInstaller.java");
        FileObject lcClassFO = pkgDirFO.getFileObject("JMXBindingComponentLifeCycle.java");
        FileObject suMgrClassFO = pkgDirFO.getFileObject("JMXBindingSUManager.java");
        FileObject rtClassFO = pkgDirFO.getFileObject("JMXBindingRuntime.java");
        
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
    private void createBindingComponentProjectArtifacts(FileObject prjDirFO ) throws IOException {
        
        String bindingArtifactsZipTemplatePath = "codegen/components/binding/binding-artifacts.zip";
        FileObject jbiSupportFolder = Util.getJBISupportFolder();
        FileObject bindingArtifactsZipFO = jbiSupportFolder.getFileObject(bindingArtifactsZipTemplatePath);
        Util.unZipArchive(bindingArtifactsZipFO.getInputStream(), prjDirFO);
        
        FileObject srcRoot = prjDirFO.getFileObject(JbiCompProjectProperties.SRC_DIR_VALUE);
        // create package
        FileObject pkgFolder = createPackage(srcRoot, this.getPackageName());
        
        // create java files
        
        String btClassTemplatePath = "codegen/components/binding/BindingComponentInstaller.java";
        String lcClassTemplatePath = "codegen/components/binding/BindingComponentLifeCycle.java";
        String suMgrClassTemplatePath = "codegen/components/binding/BindingComponentSUManager.java";
        String rtClassTemplatePath = "codegen/components/binding/BindingComponentRuntime.java";
//        String msgRecvClassTemplatePath = "codegen/components/binding/BindingComponentMessageReceiver.java";
        String jmxEndpointMBeanClassTemplatePath = "codegen/components/binding/JMXEndpointMBean.java";
        String jmxEndpointMBeanImplClassTemplatePath = "codegen/components/binding/JMXEndpointMBeanImpl.java";
        String jmxEndpointMgrClassTemplatePath = "codegen/components/binding/JMXEndpointManager.java";
        
        
        FileObject btClassFO = createFromJavaTemplate(pkgFolder, btClassTemplatePath, null);
        FileObject lcClassFO = createFromJavaTemplate(pkgFolder, lcClassTemplatePath, null);
        FileObject suMgrClassFO = createFromJavaTemplate(pkgFolder, suMgrClassTemplatePath, null);
        FileObject rtClassFO = createFromJavaTemplate(pkgFolder, rtClassTemplatePath, null);
//        FileObject msgRecvClassFO = createFromJavaTemplate(pkgFolder, msgRecvClassTemplatePath, null);
        FileObject jmxEndpointMBeanClassFO = createFromJavaTemplate(pkgFolder, jmxEndpointMBeanClassTemplatePath, null);
        FileObject jmxEndpointMBeanImplClassFO = createFromJavaTemplate(pkgFolder, jmxEndpointMBeanImplClassTemplatePath, null);
        FileObject jmxEndpointMgrClassFO = createFromJavaTemplate(pkgFolder, jmxEndpointMgrClassTemplatePath, null);
        
        // create jbi.xml
        String jbiXmlTemplatePath = "codegen/components/binding/jbi.xml";
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
        
        createWsdlExtArtifacts(prjDirFO, pkgFolder);
        
        createBindingTestArtifacts(prjDirFO);
        
        updateBindingEchoServiceArtifacts(prjDirFO);
    }
    
    
    private void createBCPluginProject(FileObject prjDirFO ) throws IOException {
        WizardDescriptor wizDesc = null;
        wizDesc = new WizardDescriptor(new WizardDescriptor.ArrayIterator());
        BCDeployPluginWizardModel pluginWizModel =
                (BCDeployPluginWizardModel) DeployPluginWizardModel.createDeployPluginWizardModel(wizDesc, BINDING_COMPONENT_COMP_TYPE);
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
        
        pluginWizModel.getWSDLExtInfo().setNamespace(WSDLExtInfo.JMX_WSDL_EXT_NS);
        pluginWizModel.getWSDLExtInfo().setJMXExtensionSchema(true);
        
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
