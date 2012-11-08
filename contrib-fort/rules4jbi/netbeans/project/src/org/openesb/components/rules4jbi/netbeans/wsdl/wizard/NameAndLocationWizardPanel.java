/*
 * @(#)NameAndLocationWizardPanel.java        $Revision: 1.2 $ $Date: 2008/11/10 04:45:34 $
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

import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;
import org.openesb.components.rules4jbi.netbeans.util.Validator;
import java.io.File;
import java.util.ResourceBundle;
import javax.swing.event.DocumentEvent;
import org.openide.WizardDescriptor;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/10 04:45:34 $
 * 
 * @since 0.1
 */
public class NameAndLocationWizardPanel extends AbstractWizardPanel {
    
    private ResourceBundle resourceBundle = NbBundle.getBundle(NameAndLocationWizardPanel.class);
    
    private NameAndLocationVisualPanel component = null;
    
    @Override
    public NameAndLocationVisualPanel getComponent() {
        if (component == null) {
            component = new NameAndLocationVisualPanel();
            
            component.putClientProperty("WizardPanel_contentSelectedIndex", 2);
            
            component.addDocumentListener(this);
        }
        
        return component;
    }

    @Override
    public String getName() {
        return resourceBundle.getString("NameAndLocationPanel.name");
    }

    @Override
    public boolean isFinishPanel() {
        return true;
    }
    
    @Override
    protected boolean checkValidity() {
        if (!Validator.isValidFileName(getComponent().getFileName())) {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.invalid.filename.error.message"));
            
            return false;
        }

        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void insertUpdate(DocumentEvent event) {
        super.insertUpdate(event);

        updateCreatedFiles();
    }
    
    @Override
    public void removeUpdate(DocumentEvent event) {
        super.removeUpdate(event);
        
        updateCreatedFiles();
    }
    
    private void updateCreatedFiles() {
        if (wsdlFileLocation != null) {
            updateWSDLFileLocation(getComponent().getFileName());
            getComponent().setFirstCreatedFile(wsdlFileLocation.toString());
        }
    }
    
    private int wsdlFileNameStart = 0;
    
    private int wsdlFileNameEnd = 0;

    private StringBuilder wsdlFileLocation = null;
    
    void initWSDLFileLocation(String destinationDirectory, String fileName) {
        wsdlFileLocation = new StringBuilder();
        
        wsdlFileLocation.append(destinationDirectory);
        
        wsdlFileLocation.append(File.separator);
        
        wsdlFileNameStart = wsdlFileLocation.length();
        
        wsdlFileLocation.append(fileName);
        
        wsdlFileNameEnd = wsdlFileLocation.length();
        
        wsdlFileLocation.append(Constants.WSDL_FILE_EXTENSION);
    }

    void updateWSDLFileLocation(String newFileName) {
        if (wsdlFileLocation != null) {
            wsdlFileLocation.replace(wsdlFileNameStart, wsdlFileNameEnd, newFileName);
            wsdlFileNameEnd = wsdlFileLocation.lastIndexOf(Constants.WSDL_FILE_EXTENSION);
        }
    }

    String getWSDLFileLocation() {
        return wsdlFileLocation.toString();
    }
    
    private void clearWSDLFileLocation() {
        wsdlFileLocation = null;
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().setFileName((String) settings.getProperty(Constants.PROPERTY_WSDL_FILE_NAME));
        
        initWSDLFileLocation(
                (String) settings.getProperty(Constants.PROPERTY_DESCRIPTIONS_DIRECTORY_LOCATION),
                (String) settings.getProperty(Constants.PROPERTY_WSDL_FILE_NAME)
        );
        
        getComponent().clearCreatedFiles();
        getComponent().addCreatedFile(wsdlFileLocation.toString());
        getComponent().addCreatedFile((String) settings.getProperty(Constants.PROPERTY_JBI_FILE_LOCATION));
        
        
        getComponent().clearModifiedFiles();
        getComponent().addModifiedFile((String) settings.getProperty(Constants.PROPERTY_CONFIG_FILE_LOCATION));
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_WSDL_FILE_NAME, getComponent().getFileName());
        
        clearWSDLFileLocation();
    }
}
