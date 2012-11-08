/*
 * @(#)FileChooserDialogDescriptor.java        $Revision: 1.2 $ $Date: 2008/11/04 18:48:12 $
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

package org.openesb.components.rules4jbi.netbeans.util.chooser;

import java.util.Arrays;
import java.util.List;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.openide.DialogDescriptor;
import org.openide.explorer.ExplorerManager;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.nodes.Node;

import org.openesb.components.rules4jbi.netbeans.util.Validator;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/04 18:48:12 $
 * 
 * @since 0.1
 */
public class FileChooserDialogDescriptor extends DialogDescriptor implements PropertyChangeListener {

    private final FileChooserPanel fileChooserPanel;
    
    private final String[] allowedFileExtensions;

    public FileChooserDialogDescriptor(FileChooserPanel fileChooserPanel, String title,
            String... allowedFileExtensions)
    {
        
        /* Modal dialog with OK/Cancel buttons */
        super(fileChooserPanel, title);
        
        this.fileChooserPanel = fileChooserPanel;
        this.allowedFileExtensions = allowedFileExtensions;
        
        /* Disables the OK button initially */
        setValid(false);

        fileChooserPanel.addExplorerManagerPropertyChangeListener(this);
    }
    
    public void propertyChange(PropertyChangeEvent event) {
        if (ExplorerManager.PROP_SELECTED_NODES.equals(event.getPropertyName())) {
            Node[] nodes = (Node[]) event.getNewValue();

            /* we use single selection mode; should not happen */
            if (nodes.length != 1) {
                setValid(false);
                return;
            }

            DataObject dataObject = nodes[0].getLookup().lookup(DataObject.class);

            boolean valid = dataObject != null && !(dataObject instanceof DataFolder);
            
            if (valid && allowedFileExtensions.length > 0) {
                final FileObject fileObject = dataObject.getPrimaryFile();
                final String fileName = fileObject.getName();
                final String fileExtension = fileObject.getExt();
                
                valid = false;
                
                for (String allowedFileExtension : allowedFileExtensions) {
                    if (fileExtension.equals(allowedFileExtension)) {
                        valid = true;
                        
                        break;
                    }
                }
                
                if (valid && ("java".equals(fileExtension) || "class".equals(fileExtension))) {
                    valid = !"package-info".equals(fileName);
                }
                
                if (valid && "java".equals(fileExtension)) {
                    valid = Validator.isValidJavaSourceFile(fileObject);
                }
            }

            setValid(valid);
        }
    }
    
    public void clearSelection() {
        fileChooserPanel.clearSelection();
    }
    
    public void showAllFiles() {
        fileChooserPanel.expandAllNodes();
    }
    
    public void hideRootNode() {
        fileChooserPanel.hideRootNode();
    }
    
    public void selectNode(Node node) {
        fileChooserPanel.setSelectedNode(node);
    }
    
    public Node getSelectedNode() {
        return fileChooserPanel.getSelectedNode();
    }
    
    public FileObject getSelectedFile() {
        Node selectedNode = fileChooserPanel.getSelectedNode();
        
        if (selectedNode == null) {
            return null;
        }
        
        DataObject dataObject = selectedNode.getLookup().lookup(DataObject.class);
            
        if (dataObject == null) {
            return null;
        }
        
        return dataObject.getPrimaryFile();
    }
}
