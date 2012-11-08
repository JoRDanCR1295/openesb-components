/*
 * @(#)Rules4JBIProjectOperations.java        $Revision: 1.5 $ $Date: 2008/11/12 08:26:24 $
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

package org.openesb.components.rules4jbi.netbeans.project;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.CopyOperationImplementation;
import org.netbeans.spi.project.DeleteOperationImplementation;
import org.netbeans.spi.project.MoveOperationImplementation;
import org.netbeans.spi.project.ProjectState;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.5 $ $Date: 2008/11/12 08:26:24 $
 * 
 * @since 0.1
 */
public class Rules4JBIProjectOperations implements DeleteOperationImplementation, CopyOperationImplementation, MoveOperationImplementation {

    private static final Logger logger = Logger.getLogger(Rules4JBIProjectOperations.class.getName());
    
    private final Project project;
    
    public Rules4JBIProjectOperations(Project project) {
        this.project = project;
    }

    public List<FileObject> getMetadataFiles() {
        List<FileObject> files = new ArrayList<FileObject>();

        DirectoryManager directoryManager = project.getLookup().lookup(DirectoryManager.class);
        
        files.add(directoryManager.getMetadataDirectory());
        files.add(directoryManager.getFakeMetadataDirectory());
        files.add(directoryManager.getFakeBuildScriptFile());
        
        //TODO: do we need to return the project directory itself?
//        files.add(project.getProjectDirectory());
        
        return files;
    }

    public List<FileObject> getDataFiles() {
        List<FileObject> files = new ArrayList<FileObject>();
        
        DirectoryManager directoryManager = project.getLookup().lookup(DirectoryManager.class);
        
        files.add(directoryManager.getRulesDirectory());
        files.add(directoryManager.getSourceDirectory());
        files.add(directoryManager.getSchemasDirectory());
        files.add(directoryManager.getDescriptionsDirectory());
        files.add(directoryManager.getLibrariesDirectory());
        files.add(directoryManager.getRulesEngineDirectory());
        
        return files;
    }

    public void notifyDeleting() throws IOException {
        DirectoryManager directoryManager = project.getLookup().lookup(DirectoryManager.class);
        
        directoryManager.deleteGeneratedFiles();
    }

    public void notifyDeleted() throws IOException {
        ProjectState state = project.getLookup().lookup(ProjectState.class);
        
        if (state != null) {
            state.notifyDeleted();
        }
    }
    
    public void notifyCopying() throws IOException {
        // do nothing
    }

    public void notifyCopied(Project arg0, File arg1, String arg2) throws IOException {
        //TODO: set the new name, if we support name different from project root folder
    }

    public void notifyMoving() throws IOException {
        notifyDeleting();
    }

    public void notifyMoved(Project arg0, File arg1, String arg2) throws IOException {
        //TODO: same considerations as in notifyCopied
    }
}
