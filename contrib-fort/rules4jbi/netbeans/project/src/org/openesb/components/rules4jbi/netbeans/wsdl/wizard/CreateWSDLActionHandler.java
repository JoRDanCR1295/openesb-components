/*
 * @(#)CreateWSDLActionHandler.java        $Revision: 1.5 $ $Date: 2008/12/17 23:21:34 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard;

import java.io.File;
import org.openesb.components.rules4jbi.netbeans.wsdl.WSDLBuilder;
import org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table.BusinessObjectTableRecord;
import java.awt.Dialog;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import org.netbeans.api.project.Project;

import org.openide.DialogDisplayer;
import org.openide.WizardDescriptor;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;

import org.openesb.components.rules4jbi.netbeans.util.FileObjectSaver;
import org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table.BusinessObjectsTableModel;
import org.openesb.components.rules4jbi.shared.classloader.ClassLoaderFactory;
import org.openesb.components.rules4jbi.shared.config.Configuration;
import org.openesb.components.rules4jbi.shared.config.ServiceUnitDescriptor;
import java.io.IOException;
import java.util.ArrayList;
import javax.wsdl.WSDLException;
import org.netbeans.api.project.ProjectManager;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.util.Mutex;
import org.openide.util.MutexException;

/**
 * This class is responsible for the WSDL creation for the project corresponding to
 * the <code>DirectoryManager</code> passed to its <code>handleWSDLCreation()</code> method.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.5 $ $Date: 2008/12/17 23:21:34 $
 * 
 * @since 0.1
 */
public final class CreateWSDLActionHandler {
    
    private static final Logger logger = Logger.getLogger(CreateWSDLActionHandler.class.getName());

    private static final CreateWSDLActionHandler INSTANCE = new CreateWSDLActionHandler();
    
    private CreateWSDLActionHandler() {}

    public static CreateWSDLActionHandler getInstance() {
        return INSTANCE;
    }
    
    public void handleWSDLCreation(final DirectoryManager directoryManager) throws WSDLException, IOException {
        final Project currentProject =
                ProjectManager.getDefault().findProject(directoryManager.getProjectDirectory());
        
        final FileObject descriptionsDirectory = directoryManager.getDescriptionsDirectory();
        
        logger.fine("Creating WSDL document in " + FileUtil.getFileDisplayName(descriptionsDirectory));
        
        final FileObject configFile = directoryManager.getConfigFile();
        
        final FileObject jbiFile = directoryManager.getOrCreateDescriptorFile();
        
        WSDLWizardDescriptor wizardDescriptor = new WSDLWizardDescriptor(currentProject);
        
        wizardDescriptor.putProperty(Constants.PROPERTY_CONFIG_FILE_LOCATION,
                FileUtil.getFileDisplayName(configFile));
        wizardDescriptor.putProperty(Constants.PROPERTY_DESCRIPTIONS_DIRECTORY_LOCATION,
                FileUtil.getFileDisplayName(descriptionsDirectory));
        wizardDescriptor.putProperty(Constants.PROPERTY_JBI_FILE_LOCATION,
                FileUtil.getFileDisplayName(jbiFile));
        
        logger.fine("Final properties: " + wizardDescriptor.getProperties().toString());
        
        Dialog dialog = DialogDisplayer.getDefault().createDialog(wizardDescriptor);
        
        dialog.setVisible(true);
        dialog.toFront();

        if (wizardDescriptor.getValue() == WizardDescriptor.FINISH_OPTION) {
            logger.fine("WSDL wizard completed successfully");

            Map<String, Object> properties = wizardDescriptor.getProperties();

            logger.fine("User provided values: " + properties + "\n");

            final String wsdlFileName = (String) properties.get(Constants.PROPERTY_WSDL_FILE_NAME)
                    + Constants.WSDL_FILE_EXTENSION;
            
            final Configuration configuration = currentProject.getLookup().lookup(Configuration.class);
            
            directoryManager.setWSDLFileName(wsdlFileName);
            
            configuration.setWSDLFile(wsdlFileName);
            
            final FileObject wsdlFile = directoryManager.getOrCreateWSDLFile();
            
            final String definitionsName = (String) properties.get(Constants.PROPERTY_DEFINITIONS_NAME);
            final String targetNamespace = (String) properties.get(Constants.PROPERTY_TARGET_NAMESPACE);
            final String portTypeName = (String) properties.get(Constants.PROPERTY_PORT_TYPE_NAME);
            final String serviceName = (String) properties.get(Constants.PROPERTY_SERVICE_NAME);
            final String portName = (String) properties.get(Constants.PROPERTY_PORT_NAME);

            final String partnerLinkTypeName = (String) properties.get(Constants.PROPERTY_PARTNER_LINK_TYPE_NAME);
            final String partnerLinkRoleName = (String) properties.get(Constants.PROPERTY_PARTNER_LINK_ROLE_NAME);
            
            final ServiceUnitDescriptor suDescriptor = new ServiceUnitDescriptor(
                    targetNamespace, portTypeName, partnerLinkTypeName, partnerLinkRoleName);

            logger.fine("Created service unit descriptor: " + suDescriptor);
            FileObjectSaver.save(suDescriptor, jbiFile);

            
            BusinessObjectsTableModel tableModel =
                    (BusinessObjectsTableModel) properties.get(Constants.PROPERTY_BUSINESS_OBJECTS);

            List<BusinessObjectTableRecord> tableData = tableModel.getTableData();

            logger.fine("Table data: " + tableData);

            WSDLBuilder builder = new WSDLBuilder(targetNamespace);

            builder.definitions(definitionsName)
                    .portType(portTypeName)
                    .service(serviceName)
                    .port(portName)
                    .partnerLink(partnerLinkTypeName, partnerLinkRoleName);
            

            List<String> classes = new ArrayList<String>();
            
            logger.fine("Initializing class loader");

            File classesDirectory = FileUtil.toFile(directoryManager.getOrCreateClassesDirectory());

            File librariesDirectory = FileUtil.toFile(directoryManager.getLibrariesDirectory());
            
            logger.fine("Classes directory URL: " + classesDirectory.toURI().toURL().toString());
            logger.fine("Libraries directory URL: " + librariesDirectory.toURI().toURL().toString());

            final ClassLoader classLoader =
                    ClassLoaderFactory.createServiceUnitClassLoader(classesDirectory, librariesDirectory);

            logger.fine("Classloader obtained successfully");
            
            for (BusinessObjectTableRecord tableRecord : tableData) {
                logger.fine("Processing table record: " + tableRecord);

                /*
                 * The obvious approach to obtain the classloader:
                 *
                 * FileObject javaSourceFile =
                 *         directoryManager.getJavaSourceFile(businessObjectTableRecord.getType());
                 * ClassPath classPath = ClassPath.getClassPath(javaSourceFile, ClassPath.COMPILE);
                 * ClassLoader classLoader = classPath.getClassLoader(false);
                 *
                 * didn't work, because the classes loaded by this classloader didn't have the
                 * @XmlRootElement annotation. Therefore we have to use the URL classloader obtained
                 * above.
                 */

                try {
                    Class<?> clazz = classLoader.loadClass(tableRecord.getType());

                    final String loadedClassName = clazz.getName();

                    logger.fine("Sucessfully loaded class: " + loadedClassName);

                    if (!classes.contains(loadedClassName)) {
                        classes.add(loadedClassName);
                    }

                    if (tableRecord.getDirection().isInput()) {
                        builder.inputObject(clazz, tableRecord.getCardinality().intValue());
                        
                    } else {
                        builder.outputObject(clazz, tableRecord.getCardinality().intValue());
                    }

                } catch (ClassNotFoundException e) {
                    logger.warning("Could not load the class: " + e.getMessage());
                }
            }

            configuration.setClasses(classes);
            
            saveWSDL(builder.createWSDL(), wsdlFile);
            
            logger.fine("Saving current configuration");
            FileObjectSaver.save(configuration, configFile);
            
            openFileInEditor(jbiFile);
            openFileInEditor(wsdlFile);
        }
    }
    
    private void saveWSDL(final Definition wsdl, final FileObject fileObject) throws WSDLException, IOException {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        final WSDLWriter writer = wsdlFactory.newWSDLWriter();

        try {
            ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {

                public Void run() throws IOException {

                    FileLock lock = null;
                    OutputStream outputStream = null;

                    try {
                        lock = fileObject.lock();

                        outputStream = fileObject.getOutputStream(lock);

                        writer.writeWSDL(wsdl, outputStream);

                    } catch (Exception e) {
                        logger.warning("Failed to save the WSDL file: " + e.getMessage());

                        throw new IOException("Failed to save the WSDL file", e);

                    } finally {
                        if (outputStream != null) {
                            try {
                                outputStream.close();

                            } catch (IOException e) {
                                logger.fine("Failed to properly close the output stream: " + e.getMessage());
                            }
                        }

                        if (lock != null) {
                            lock.releaseLock();
                        }
                    }

                    return null;
                }
            });

        } catch (MutexException e) {
            throw (IOException) e.getException();
        }
    }
    
    private void openFileInEditor(FileObject file) {
        final String fileName = file.getNameExt();
        
        try {
            DataObject dataObject = DataObject.find(file);

            EditorCookie editorCookie = dataObject.getCookie(EditorCookie.class);

            if (editorCookie != null) {
                logger.finer("Opening " + fileName);

                editorCookie.open();

            } else {
                logger.fine("Could not open " + fileName);
            }

        } catch (DataObjectNotFoundException e) {
            logger.fine("Could not find data object for file " + fileName);
        }
    }
}
