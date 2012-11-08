/*
 * @(#)ImportFileMainProjectAction.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:26 $
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
import org.openide.util.NbBundle;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileAlreadyExistsException;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.ImportFileActionHandler;

/**
 * An action for importing files into the current main project.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:26 $
 * 
 * @see AbstractMainProjectAction
 * @since 0.3
 */
public final class ImportFileMainProjectAction extends AbstractMainProjectAction {
    
    private static final long serialVersionUID = -2449366228226639442L;

    private static final Logger logger = Logger.getLogger(ImportFileMainProjectAction.class.getName());
    
    @Override
    protected void performAction(final DirectoryManager directoryManager) {
        logger.fine("Importing file into the main project "
                + FileUtil.getFileDisplayName(directoryManager.getProjectDirectory()));

        try {
            ImportFileActionHandler.getInstance().handleFileImport(directoryManager, null);
            
        } catch (FileAlreadyExistsException e) {
            logger.severe("Failed to import the selected file: " + e.getMessage());

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(ImportFileMainProjectAction.class, "file.already.exists.error.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);

        } catch (IOException e) {
            logger.severe("Failed to import the selected file: " + e.getMessage());

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(ImportFileMainProjectAction.class, "file.import.failed.error.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);
        }
    }

    @Override
    public String getName() {
        return NbBundle.getMessage(ImportFileMainProjectAction.class, "import.file.action.name", "File");
    }

    @Override
    protected String iconResource() {
        return "org/openesb/components/rules4jbi/netbeans/resources/importFile.png";
    }
}
