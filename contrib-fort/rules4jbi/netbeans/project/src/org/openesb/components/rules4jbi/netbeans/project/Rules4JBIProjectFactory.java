/*
 * @(#)Rules4JBIProjectFactory.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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
import org.openesb.components.rules4jbi.netbeans.util.FileObjectSaver;
import org.openesb.components.rules4jbi.shared.config.Configuration;

import java.io.IOException;
import java.util.logging.Logger;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.spi.project.ProjectFactory;
import org.netbeans.spi.project.ProjectState;
import org.openide.filesystems.FileObject;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class Rules4JBIProjectFactory implements ProjectFactory {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIProjectFactory.class.getName());

    public boolean isProject(final FileObject projectDirectory) {
        return projectDirectory.getFileObject(DirectoryManager.METADATA_DIR) != null;
    }

    public Project loadProject(FileObject projectDirectory, ProjectState projectState) throws IOException {
        return isProject(projectDirectory) ? new Rules4JBIProject(projectDirectory, projectState) : null;
    }

    public void saveProject(final Project project) throws IOException, ClassCastException {
        final String projectDisplayName = ProjectUtils.getInformation(project).getDisplayName();
        
        logger.fine("Saving project " + projectDisplayName);
        
        final Configuration configuration = project.getLookup().lookup(Configuration.class);
        
        final DirectoryManager directoryManager = project.getLookup().lookup(DirectoryManager.class);

        FileObjectSaver.save(configuration, directoryManager.getConfigFile());
        
        logger.fine("Project '" + projectDisplayName + "' saved successfully");
    }
}
