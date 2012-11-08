/*
 * @(#)Rules4JBIProjectOpenedHook.java        $Revision: 1.2 $ $Date: 2008/11/10 04:45:34 $
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
import org.openesb.components.rules4jbi.shared.config.Configuration;
import java.io.IOException;
import java.util.logging.Logger;

import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.classpath.GlobalPathRegistry;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.ui.ProjectOpenedHook;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/10 04:45:34 $
 * 
 * @see org.netbeans.spi.project.ui.ProjectOpenedHook
 * @since 0.1
 */
public final class Rules4JBIProjectOpenedHook extends ProjectOpenedHook {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIProjectOpenedHook.class.getName());
    
    private final Project project;

    public Rules4JBIProjectOpenedHook(Project project) {
        this.project = project;
    }

    @Override
    protected void projectOpened() {
        logger.fine("Opening project");
        
        final DirectoryManager directoryManager = project.getLookup().lookup(DirectoryManager.class);
        
        final Configuration configuration = project.getLookup().lookup(Configuration.class);
        
        /* we need to synchronize directory manager with the stored WSDL file name */
        directoryManager.setWSDLFileName(configuration.getWSDLFile());
        
        Rules4JBIClassPathProvider classPathProvider =
                project.getLookup().lookup(Rules4JBIClassPathProvider.class);
        
        if (classPathProvider == null) {
            throw new IllegalStateException("Project's lookup does not contain a classpath provider");
        }

        GlobalPathRegistry.getDefault().register(ClassPath.BOOT,
                new ClassPath[] {classPathProvider.getBootClassPath()});

        GlobalPathRegistry.getDefault().register(ClassPath.SOURCE,
                new ClassPath[] {classPathProvider.getSourceClassPath()});
        
        logger.finest("Project opened");
    }

    @Override
    protected void projectClosed() {
        logger.fine("Closing project");
        
        /* try to save the project, but only if it was not deleted */
        if (project.getProjectDirectory().isValid()) {
            try {
                ProjectManager.getDefault().saveProject(project);
                
            } catch (IOException e) {
                logger.fine("Failed to save project: " + e.getMessage());
            }
        }
        
        Rules4JBIClassPathProvider classPathProvider =
                project.getLookup().lookup(Rules4JBIClassPathProvider.class);

        if (classPathProvider == null) {
            throw new IllegalStateException("Project's lookup does not contain a classpath provider");
        }

        GlobalPathRegistry.getDefault().unregister(ClassPath.SOURCE,
                new ClassPath[]{classPathProvider.getSourceClassPath()});
        
        GlobalPathRegistry.getDefault().unregister(ClassPath.BOOT,
                new ClassPath[]{classPathProvider.getBootClassPath()});
            
        logger.finest("Project closed");
    }
}
