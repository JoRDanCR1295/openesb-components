/*
 * @(#)NameAndLocationWizardPanel.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

package org.openesb.components.rules4jbi.netbeans.project.wizard;

import org.openesb.components.rules4jbi.netbeans.util.Validator;
import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;
import java.io.File;
import java.util.ResourceBundle;
import javax.swing.event.DocumentEvent;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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
        if (!Validator.isValidFileName(getComponent().getProjectName())) {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.invalid.project.name.error.message"));
            
            return false;
        }
        
        final File projectDirectory = FileUtil.normalizeFile(new File(getComponent().getCreatedFolder()));
        
        if (projectDirectory.exists()) {
            File[] projectDirectoryContent = projectDirectory.listFiles();
            
            if (projectDirectoryContent != null && projectDirectoryContent.length > 0) {
                showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.project.folder.exists.error.message"));
                
                return false;
            }
        }
        
        final File projectDirectoryLocation = projectDirectory.getParentFile();
        
        if (projectDirectoryLocation == null || !projectDirectoryLocation.exists()
                || !projectDirectoryLocation.canWrite())
        {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.cannot.create.project.folder.error.message"));
            
            return false;
        }

        if (FileUtil.toFileObject(projectDirectoryLocation) == null) {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.invalid.project.folder.error.message"));
            
            return false;
        }

        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void insertUpdate(DocumentEvent event) {
        updateCreatedFolder();
        
        super.insertUpdate(event);
    }
    
    @Override
    public void removeUpdate(DocumentEvent event) {
        updateCreatedFolder();
        
        super.removeUpdate(event);
    }
    
    private void updateCreatedFolder() {
        final String projectLocation = getComponent().getProjectLocation();
        final String projectName = getComponent().getProjectName();
        
//        if (!projectLocation.equals("") && !projectName.equals("")) {
            getComponent().setCreatedFolder(projectLocation + File.separatorChar + projectName);
//        }
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().setProjectName((String) settings.getProperty(Constants.PROPERTY_PROJECT_NAME));
        getComponent().setProjectLocation((String) settings.getProperty(Constants.PROPERTY_PROJECT_LOCATION));
        getComponent().setCreatedFolder((String) settings.getProperty(Constants.PROPERTY_PROJECT_FOLDER));
        getComponent().setAsMainProject((Boolean) settings.getProperty(Constants.PROPERTY_SET_AS_MAIN));
        
        getComponent().selectWholeProjectName();
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_PROJECT_NAME, getComponent().getProjectName());
        settings.putProperty(Constants.PROPERTY_PROJECT_LOCATION, getComponent().getProjectLocation());
        settings.putProperty(Constants.PROPERTY_PROJECT_FOLDER, getComponent().getCreatedFolder());
        settings.putProperty(Constants.PROPERTY_SET_AS_MAIN,
                getComponent().isSetAsMainProject() ? Boolean.TRUE : Boolean.FALSE);
    }
}
