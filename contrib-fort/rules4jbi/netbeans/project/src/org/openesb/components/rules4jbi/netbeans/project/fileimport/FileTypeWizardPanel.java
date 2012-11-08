/*
 * @(#)FileTypeWizardPanel.java        $Revision: 1.1 $ $Date: 2008/10/25 22:02:56 $
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

package org.openesb.components.rules4jbi.netbeans.project.fileimport;

import java.util.ResourceBundle;

import org.openide.WizardDescriptor;
import org.openide.util.NbBundle;

import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/10/25 22:02:56 $
 * 
 * @since 0.3
 */
public class FileTypeWizardPanel extends AbstractWizardPanel {
    
    private ResourceBundle resourceBundle = NbBundle.getBundle(FileTypeWizardPanel.class);
    
    private FileTypeVisualPanel component = null;
    
    @Override
    public FileTypeVisualPanel getComponent() {
        if (component == null) {
            component = new FileTypeVisualPanel();
            
            component.putClientProperty("WizardPanel_contentSelectedIndex", 0);
            
            component.setFileTypes(FileType.values());
            component.selectFirstFileType();
        }
        
        return component;
    }

    @Override
    public String getName() {
        return resourceBundle.getString("FileTypePanel.name");
    }

    @Override
    protected boolean checkValidity() {
        
        /*
         * This panel always validates, because there is only a limited set of values
         * to choose from, which are all valid. One of these values is always selected.
         */

        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().selectFileType((FileType) settings.getProperty(Constants.PROPERTY_FILE_TYPE));
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_FILE_TYPE, getComponent().getSelectedFileType());
    }
}
