/*
 * @(#)ImportFileContextAwareAction.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:26 $
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

package org.openesb.components.rules4jbi.netbeans.project.actions;

import java.io.IOException;
import java.util.logging.Logger;

import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileUtil;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileAlreadyExistsException;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileType;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.ImportFileActionHandler;

/**
 * An action for importing files into the currently selected project.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:26 $
 * 
 * @since 0.3
 */
final class ImportFileContextAwareAction extends AbstractContextAwareAction {
    
    private static final long serialVersionUID = -2449366228226639442L;

    private static final Logger logger = Logger.getLogger(ImportFileContextAwareAction.class.getName());

    private final FileType fileType;
    
    ImportFileContextAwareAction(Lookup context) {
        super(NbBundle.getMessage(ImportFileContextAwareAction.class, "import.file.action.name",
                (context.lookup(FileType.class) != null ? context.lookup(FileType.class).getName()
                                                        : "File")), context);
        
        fileType = context.lookup(FileType.class);
        
        logger.finer("This action imports files of type: "
                + (fileType == null ? "all" : fileType.getDisplayName()));
    }
    
    @Override
    protected void performAction(final DirectoryManager directoryManager) {
        logger.fine("Importing file into the currently selected project: " 
                + FileUtil.getFileDisplayName(directoryManager.getProjectDirectory()));

        try {
            ImportFileActionHandler.getInstance().handleFileImport(directoryManager, fileType);
            
        } catch (FileAlreadyExistsException e) {
            logger.severe("Failed to import the selected file: " + e.getMessage());

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(ImportFileContextAwareAction.class, "file.already.exists.error.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);

        } catch (IOException e) {
            logger.severe("Failed to import the selected file: " + e.getMessage());

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(ImportFileContextAwareAction.class, "file.import.failed.error.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);
        }
    }
    
    @Override
    public AbstractContextAwareAction createContextAwareInstance(Lookup context) {
        return new ImportFileContextAwareAction(context);
    }
}
