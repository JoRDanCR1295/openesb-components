/*
 * @(#)NameAndLocationWizardPanel.java        $Revision: 1.1 $ $Date: 2008/10/25 22:02:57 $
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

import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;

import org.openide.WizardDescriptor;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

import net.jcip.annotations.GuardedBy;

import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/10/25 22:02:57 $
 * 
 * @since 0.3
 */
public class NameAndLocationWizardPanel extends AbstractWizardPanel {
    
    @GuardedBy("this")
    private FileType currentFileType = FileType.RULESET;

    private ResourceBundle resourceBundle = NbBundle.getBundle(NameAndLocationWizardPanel.class);
    
    private NameAndLocationVisualPanel component = null;
    
    private synchronized FileType getCurrentFileType() {
        return currentFileType;
    }

    private synchronized void setCurrentFileType(FileType currentFileType) {
        this.currentFileType = currentFileType;
    }
    
    @Override
    public NameAndLocationVisualPanel getComponent() {
        if (component == null) {
            component = new NameAndLocationVisualPanel(this);
            
            component.putClientProperty("WizardPanel_contentSelectedIndex", 1);
            
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
    
    void handleBrowseButtonPressed() {
        JFileChooser chooser = new JFileChooser();
        FileUtil.preventFileChooserSymlinkTraversal(chooser, null);
        chooser.setDialogTitle(resourceBundle.getString("NameAndLocationWizardPanel.fileChooser.title"));
        chooser.setApproveButtonText(resourceBundle.getString("NameAndLocationWizardPanel.fileChooser.buttonText"));
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        chooser.setMultiSelectionEnabled(false);

        chooser.setFileFilter(getCurrentFileType().getFileFilter());
        chooser.setAcceptAllFileFilterUsed(false);

        String currentFilePath = getComponent().getFilePath();
        if (currentFilePath.length() > 0) {
            File file = new File(currentFilePath);

            if (file.exists()) {
                chooser.setSelectedFile(file);
            }
        }

        if (JFileChooser.APPROVE_OPTION == chooser.showOpenDialog(getComponent())) {
            File selectedFile = chooser.getSelectedFile();
            getComponent().setFilePath(FileUtil.normalizeFile(selectedFile).getAbsolutePath());
        }
    }
    
    @Override
    protected boolean checkValidity() {

        final String filePath = getComponent().getFilePath();

        if (filePath.trim().equals("")) {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.filepath.empty.error.message"));
            
            return false;
        }
        
        final File fileToImport = new File(filePath);
        
        if (!fileToImport.exists()) {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.file.does.not.exist.error.message"));
            
            return false;
        }
        
        if (FileUtil.toFileObject(fileToImport) == null) {
            showErrorMessage(resourceBundle.getString("NameAndLocationWizardPanel.invalid.file.error.message"));

            return false;
        }
        
        final FileType selectedFileType = getCurrentFileType();
        
        if (!selectedFileType.getFileFilter().accept(fileToImport)) {
            showErrorMessage(NbBundle.getMessage(NameAndLocationWizardPanel.class,
                            "NameAndLocationWizardPanel.invalid.file.type.error.message",
                            selectedFileType.getDisplayName()));
            
            return false;
        }
        
        showErrorMessage(null);
        return true;
    }
    
    @Override
    public void readSettings(WizardDescriptor settings) {
        getComponent().setFilePath((String) settings.getProperty(Constants.PROPERTY_IMPORT_FILE_PATH));
        
        setCurrentFileType((FileType) settings.getProperty(Constants.PROPERTY_FILE_TYPE));
        
        super.readSettings(settings);
    }

    public void storeSettings(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_IMPORT_FILE_PATH, getComponent().getFilePath());
    }
}
