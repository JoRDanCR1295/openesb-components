/*
 * @(#)BusinessObjectsWizardPanel.java        $Revision: 1.3 $ $Date: 2008/12/17 23:21:34 $
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
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import org.openide.WizardDescriptor;
import org.openide.WizardValidationException;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

import org.netbeans.api.project.Project;

import org.openesb.components.rules4jbi.shared.classloader.ClassLoaderFactory;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;
import org.openesb.components.rules4jbi.netbeans.wsdl.schema.BusinessObject;
import org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table.BusinessObjectTableRecord;
import org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table.BusinessObjectsTableModel;
import org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table.BusinessObjectsVisualPanel;
import org.openesb.components.rules4jbi.shared.classloader.BusinessObjectsNotFoundException;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/12/17 23:21:34 $
 * 
 * @since 0.1
 */
public class BusinessObjectsWizardPanel extends AbstractWizardPanel {
    
    private static final Logger logger = Logger.getLogger(BusinessObjectsWizardPanel.class.getName());
    
    private BusinessObjectsVisualPanel component = null;

    private final Project project;

    public BusinessObjectsWizardPanel(Project project) {
        this.project = project;
    }
    
    @Override
    public BusinessObjectsVisualPanel getComponent() {
        if (component == null) {
            component = new BusinessObjectsVisualPanel(project);
            
            component.putClientProperty("WizardPanel_contentSelectedIndex", 0);
            
//            component.addDocumentListener(this);
        }
        
        return component;
    }

    @Override
    public String getName() {
        
        return NbBundle.getMessage(BusinessObjectsVisualPanel.class, "BusinessObjectsPanel.name");
    }

    @Override
    public void validate() throws WizardValidationException {
        List<BusinessObjectTableRecord> tableData = getComponent().getDataModel().getTableData();

        List<String> inputObjectTypes = new ArrayList<String>();
        List<String> outputObjectTypes = new ArrayList<String>();
        Set<String> allTypes = new HashSet<String>();
        
        boolean emptyTypeFound = false;
        boolean inputObjectFound = false;
        boolean outputObjectFound = false;
        
        String offendingInputObjectType = null;
        String offendingOutputObjectType = null;

        for (BusinessObjectTableRecord tableRecord : tableData) {
            final String currentType = tableRecord.getType();
            
            if (currentType.trim().equals("")) {
                emptyTypeFound = true;
                break;
            }
            
            if (tableRecord.getDirection().isInput()) {
                inputObjectFound = true;
                
                if (inputObjectTypes.contains(currentType)) {
                    offendingInputObjectType = currentType;
                    
                    break;
                    
                } else {
                    inputObjectTypes.add(currentType);
                }
                
            } else {
                outputObjectFound = true;
                
                if (outputObjectTypes.contains(currentType)) {
                    offendingOutputObjectType = currentType;

                    break;

                } else {
                    outputObjectTypes.add(currentType);
                }
            }
            
            allTypes.add(currentType);
        }

        if (emptyTypeFound) {
            logger.fine("Found an empty type in table");
            
            throw new WizardValidationException(getComponent(),
                    "Found an empty type in table",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class, "empty.type.in.table.message"));
        }
        
        if (offendingInputObjectType != null) {
            logger.fine("Input object " + offendingInputObjectType + " is entered multiple times");
            
            throw new WizardValidationException(getComponent(),
                    "Input object " + offendingInputObjectType + " is entered multiple times",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class,
                            "multiple.same.input.objects.message", offendingInputObjectType));
        }
        
        if (offendingOutputObjectType != null) {
            logger.fine("Output object " + offendingOutputObjectType + " is entered multiple times");
            
            throw new WizardValidationException(getComponent(),
                    "Output object " + offendingOutputObjectType + " is entered multiple times",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class,
                            "multiple.same.output.objects.message", offendingOutputObjectType));
        }
        
        if (!inputObjectFound) {
            logger.fine("No input object found");
            
            throw new WizardValidationException(getComponent(), "No input object found",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class, "no.input.object.message"));
        }
        
        if (!outputObjectFound) {
            logger.fine("No output object found");
            
            throw new WizardValidationException(getComponent(), "No output object found",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class, "no.output.object.message"));
        }
        
        validateTypes(allTypes);
        
        logger.fine("Business objects table validated successfully");
    }
    
    private void validateTypes(Set<String> types) throws WizardValidationException {
        
        ClassLoader classLoader = null;
        try {
            logger.fine("Initializing class loader");

            final DirectoryManager directoryManager = project.getLookup().lookup(DirectoryManager.class);
            
            File classesDirectory = FileUtil.toFile(directoryManager.getOrCreateClassesDirectory());

            File librariesDirectory = FileUtil.toFile(directoryManager.getLibrariesDirectory());
            
            logger.fine("Classes directory URL: " + classesDirectory.toURI().toURL().toString());
            logger.fine("Libraries directory URL: " + librariesDirectory.toURI().toURL().toString());

            classLoader = ClassLoaderFactory.createServiceUnitClassLoader(classesDirectory, librariesDirectory);
            
            logger.fine("Classloader obtained successfully");
            
        } catch (MalformedURLException e) {
            /* This situation should probably never happen */
            
            logger.fine("Could not obtain the classloader");

            throw new WizardValidationException(getComponent(), "Could not obtain the classloader",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class, "no.classloader.message"));
            
        } catch (BusinessObjectsNotFoundException e) {
            logger.fine("Could not obtain the classloader; no business objects found");

            throw new WizardValidationException(getComponent(), "Could not obtain the classloader",
                    NbBundle.getMessage(BusinessObjectsWizardPanel.class, "no.classloader.message"));
        }
        
        for (String type : types) {
            try {
                Class<?> clazz = classLoader.loadClass(type);

                final String loadedClassName = clazz.getName();
                
                logger.fine("Sucessfully loaded class: " + loadedClassName);

                if (!BusinessObject.containsNoArgConstructor(clazz)) {
                    logger.fine("No-arg constructor not found");

                    throw new WizardValidationException(getComponent(), "No-arg constructor not found",
                            NbBundle.getMessage(BusinessObjectsWizardPanel.class,
                            "noarg.constructor.not.found.message", type));
                    
                }

                if (!BusinessObject.containsXmlRootElementAnnotation(clazz)) {
                    logger.fine("@XmlRootElement annotation not found");

                    throw new WizardValidationException(getComponent(), "@XmlRootElement annotation not found",
                            NbBundle.getMessage(BusinessObjectsWizardPanel.class,
                            "XmlRootElement.annotation.not.found.message", type));
                }

                if (!BusinessObject.belongsToNamespace(clazz)) {
                    logger.fine("Namespace not found");

                    throw new WizardValidationException(getComponent(), "Namespace not found",
                            NbBundle.getMessage(BusinessObjectsWizardPanel.class,
                            "namespace.not.found.message", type));
                }

            } catch (ClassNotFoundException e) {
                logger.fine("Could not load the class " + type);

                throw new WizardValidationException(getComponent(), "Could not load the class " + type,
                            NbBundle.getMessage(BusinessObjectsWizardPanel.class,
                            "could.not.load.class.message", type));
            }
        }
    }
    
    @Override
    protected boolean checkValidity() {
        
        /*
         * Validation for this panel is done by the table model itself,
         * and final validation is done in the validate() method,
         * after the user presses the next button.
         */

        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().setDataModel(
                (BusinessObjectsTableModel) settings.getProperty(Constants.PROPERTY_BUSINESS_OBJECTS));
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_BUSINESS_OBJECTS, getComponent().getDataModel());
    }
}
