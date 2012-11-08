/*
 * @(#)ImportFileActionHandler.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:20 $
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

import java.awt.Dialog;
import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.logging.Logger;

import org.openide.DialogDisplayer;
import org.openide.WizardDescriptor;
import org.openide.cookies.EditorCookie;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;

import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;

/**
 * This class is responsible for importing a file into the project corresponding to
 * the <code>DirectoryManager</code> passed to its <code>handleFileImport()</code> method.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:20 $
 * 
 * @since 0.3
 */
public final class ImportFileActionHandler {
    
    private static final Logger logger = Logger.getLogger(ImportFileActionHandler.class.getName());

    private static final ImportFileActionHandler INSTANCE = new ImportFileActionHandler();
    
    private ImportFileActionHandler() {}

    public static ImportFileActionHandler getInstance() {
        return INSTANCE;
    }
    
    public FileObject handleFileImport(final DirectoryManager directoryManager,
            final FileType preselectedFileType) throws IOException
    {
        final Project currentProject =
                ProjectManager.getDefault().findProject(directoryManager.getProjectDirectory());

        logger.fine("Importing file into project "
                + ProjectUtils.getInformation(currentProject).getDisplayName());
        
        logger.fine("Preselected filetype is: " + preselectedFileType);
        
        ImportFileWizardDescriptor descriptor = new ImportFileWizardDescriptor(preselectedFileType);
        
        Dialog dialog = DialogDisplayer.getDefault().createDialog(descriptor);
        
        dialog.setVisible(true);
        dialog.toFront();
        
        if (descriptor.getValue() == WizardDescriptor.FINISH_OPTION) {
            logger.fine("Import file wizard completed successfully");

            Map<String, Object> properties = descriptor.getProperties();

            logger.finer("User provided values: " + properties + "\n");
            
            final FileType fileType = (FileType) properties.get(Constants.PROPERTY_FILE_TYPE);

            final String filePath = (String) properties.get(Constants.PROPERTY_IMPORT_FILE_PATH);

            logger.fine("Importing " + fileType.getDisplayName() + " '" + filePath + "'");

            final FileObject fileToImport = FileUtil.toFileObject(new File(filePath));
            
            switch (fileType) {
            case RULESET:
                openFileInEditor(copyFile(fileToImport, directoryManager.getRulesDirectory()));
                
                break;
                
            case XML_SCHEMA:
                openFileInEditor(copyFile(fileToImport, directoryManager.getSchemasDirectory()));
                
                break;
                
            case LIBRARY:
                copyFile(fileToImport, directoryManager.getLibrariesDirectory());
                
                break;

            case ENGINE:
                copyFile(fileToImport, directoryManager.getRulesEngineDirectory());
                
                break;
                
            default:
                
                throw new AssertionError("Unexpected file type");
            }
            
            directoryManager.refreshDirectoryStructure();
            
            return fileToImport;
        }
        
        return null;
    }
    
    private FileObject copyFile(FileObject sourceFile, FileObject destinationDirectory) throws IOException {
        
        if (destinationDirectory.getFileObject(sourceFile.getNameExt()) != null) {
            throw new FileAlreadyExistsException(sourceFile.getNameExt() + " already exists in "
                    + FileUtil.getFileDisplayName(destinationDirectory));
        }

        return FileUtil.copyFile(sourceFile, destinationDirectory, sourceFile.getName());
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
