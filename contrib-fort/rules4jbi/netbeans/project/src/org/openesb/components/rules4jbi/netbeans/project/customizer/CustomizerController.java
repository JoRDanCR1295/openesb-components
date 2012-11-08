/*
 * @(#)CustomizerController.java        $Revision: 1.4 $ $Date: 2008/12/17 23:21:35 $
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

package org.openesb.components.rules4jbi.netbeans.project.customizer;

import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;

import org.openide.DialogDescriptor;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;
import org.openide.util.NbBundle;

import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;

import org.openesb.components.rules4jbi.shared.GlobalConstants;
import org.openesb.components.rules4jbi.shared.config.Configuration;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileAlreadyExistsException;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileType;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.ImportFileActionHandler;
import org.openesb.components.rules4jbi.netbeans.project.nodes.RulesEngineNodeBuilder;
import org.openesb.components.rules4jbi.netbeans.project.nodes.RulesNodeBuilder;
import org.openesb.components.rules4jbi.netbeans.util.RulesEngineProvidersFinder;
import org.openesb.components.rules4jbi.netbeans.util.chooser.FileChooserDialogDescriptor;
import org.openesb.components.rules4jbi.netbeans.util.chooser.FileChooserPanel;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/12/17 23:21:35 $
 * 
 * @since 0.1
 */
class CustomizerController {
    
    private static final Logger logger = Logger.getLogger(CustomizerController.class.getName());
    
    private final GeneralPanel generalPanel;
    
    private final RulesEnginePanel rulesEnginePanel;
    
    private final Project project;
    
    private final Configuration configuration;
    
    private final DirectoryManager directoryManager;
    
    private final Node rulesEngineNode;
    
    CustomizerController(Project project) {
        this.project = project;

        directoryManager = project.getLookup().lookup(DirectoryManager.class);

        configuration = project.getLookup().lookup(Configuration.class);
        logger.fine("Retrieved project configuration: " + configuration);
        
        rulesEngineNode = new RulesEngineNodeBuilder(project).createNode();
        
        generalPanel = new GeneralPanel();
        rulesEnginePanel = new RulesEnginePanel(this);
        
        generalPanel.setProjectFolder(FileUtil.getFileDisplayName(project.getProjectDirectory()));
        generalPanel.setServiceEngineType(GlobalConstants.SERVICE_ENGINE_NAME);
        
        rulesEnginePanel.setProviderURIs(
                RulesEngineProvidersFinder.getInstance().getRuleServiceProviderURIs());
        
        rulesEnginePanel.setProviderClassNames(
                RulesEngineProvidersFinder.getInstance().getRuleServiceProviderClassNames());

        rulesEnginePanel.clearSelectedItems();
        
        rulesEnginePanel.selectProvider(configuration.getRuleServiceProvider());
        rulesEnginePanel.selectProviderClass(configuration.getRuleServiceProviderClass());
        rulesEnginePanel.setRulesetFileName(configuration.getRulesetFile());
        
        updateRulesEngineLibraries();
    }
    
    private void updateRulesEngineLibraries() {
        
        assert rulesEngineNode != null;
        
        Node[] nodes = rulesEngineNode.getChildren().getNodes(true);
        
        assert nodes != null;
        
        rulesEnginePanel.setLibraries(nodes);
        
        if (nodes.length > 0) {
            rulesEnginePanel.selectFirstLibrary();
            
            rulesEnginePanel.enableRemoveButton();
            
        } else {
            rulesEnginePanel.disableRemoveButton();
        }
    }

    GeneralPanel getGeneralPanel() {
        return generalPanel;
    }

    RulesEnginePanel getRulesEnginePanel() {
        return rulesEnginePanel;
    }

    void handleBrowseButtonPressed() {
        logger.finer("Browse button pressed");
        
        String title = NbBundle.getMessage(CustomizerController.class,
                "CustomizerController.ruleFileChooserDialog.title");
        
        final Node rootNode = new RulesNodeBuilder(project).createNode();

        FileChooserDialogDescriptor dialogDescriptor =
                new FileChooserDialogDescriptor(new FileChooserPanel(rootNode, title), title);
        
        String currentRulesetFileName = rulesEnginePanel.getRulesetFileName();
        
        logger.finer("Current ruleset file: " + currentRulesetFileName);

        Node currentNode = null;

        if (!currentRulesetFileName.equals("")) {
            Node[] rootChildNodes = rootNode.getChildren().getNodes(true);
            logger.finer("Searching following nodes: " + Arrays.toString(rootChildNodes));
            
            for (Node rootChild : rootChildNodes) {
                if (currentRulesetFileName.equals(rootChild.getDisplayName())) {
                    currentNode = rootChild;
                    break;
                }
            }
        }
        
        dialogDescriptor.clearSelection();
        
        if (currentNode != null) {
            dialogDescriptor.selectNode(currentNode);
        }
        
        Object result = DialogDisplayer.getDefault().notify(dialogDescriptor);

        if (DialogDescriptor.OK_OPTION.equals(result)) {
            logger.fine("Rule file selection confirmed");

            FileObject selectedFile = dialogDescriptor.getSelectedFile();
            
            if (selectedFile == null) {
                throw new AssertionError("Selected file cannot be null");
            }
            
            logger.fine("Selected rule file: " + selectedFile.getNameExt());
            
            rulesEnginePanel.setRulesetFileName(selectedFile.getNameExt());
        }
    }

    void handleImportButtonPressed() {
        logger.fine("Importing rules engine library into project "
                + project.getLookup().lookup(ProjectInformation.class).getDisplayName());

        try {
            final FileObject fileToImport =
                    ImportFileActionHandler.getInstance().handleFileImport(directoryManager, FileType.ENGINE);
            
            if (fileToImport != null) {
                updateRulesEngineLibraries();
                
                rulesEnginePanel.selectLibrary(rulesEngineNode.getChildren().findChild(fileToImport.getName()));
            }
            
        } catch (FileAlreadyExistsException e) {
            logger.severe("Failed to import the selected file: " + e.getMessage());

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(CustomizerController.class, "file.already.exists.error.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);
            
        } catch (IOException e) {
            logger.severe("Failed to import the selected file: " + e.getMessage());

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(CustomizerController.class, "file.import.failed.error.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);
        }
    }
    
    void handleRemoveButtonPressed() {
        Node selectedLibrary = rulesEnginePanel.getSelectedLibrary();
        
        FileObject fileObject = selectedLibrary.getLookup().lookup(DataObject.class).getPrimaryFile();
        
        final String fileName = fileObject.getNameExt();
        
        logger.fine("Deleting file " + fileName);
        
        NotifyDescriptor descriptor = new NotifyDescriptor.Confirmation(
                NbBundle.getMessage(CustomizerController.class, "delete.file.confirmation.message", fileName),
                NbBundle.getMessage(CustomizerController.class, "delete.file.confirmation.title"),
                NotifyDescriptor.YES_NO_OPTION);

        Object result = DialogDisplayer.getDefault().notify(descriptor);
        
        if (NotifyDescriptor.YES_OPTION == result) {
            try {
                fileObject.delete();

                logger.fine("Successfully deleted file " + fileName);

                updateRulesEngineLibraries();

            } catch (IOException e) {
                logger.severe("Failed to delete the file: " + e.getMessage());
            }
            
        } else {
            logger.fine("File deletion canceled");
        }
    }
    
    void handleOkButtonPressed() {
        logger.fine("Saving configuration changes");
        
        configuration.setRuleServiceProvider(rulesEnginePanel.getSelectedProvider());
        configuration.setRuleServiceProviderClass(rulesEnginePanel.getSelectedProviderClass());
        configuration.setRulesetFile(rulesEnginePanel.getRulesetFileName());
    }

    void handleWindowClosing() {
        logger.fine("Window closing");
    }
    
    void handleWindowClosed() {
        logger.fine("Window closed");
    }
}
