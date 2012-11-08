/*
 * @(#)Rules4JBIProject.java        $Revision: 1.2 $ $Date: 2009/01/25 21:00:54 $
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
import org.openesb.components.rules4jbi.netbeans.project.customizer.Rules4JBICustomizerProvider;
import org.openesb.components.rules4jbi.shared.config.Configuration;
import org.openesb.components.rules4jbi.shared.config.InvalidConfigurationException;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.ProjectState;

import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.Mutex;
import org.openide.util.lookup.Lookups;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @since 0.1
 */
public class Rules4JBIProject implements Project {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIProject.class.getName());
    
    private final FileObject projectDirectory;
            
    private final ProjectState projectState;
    
    private final Lookup lookup;
    
    private final DirectoryManager directoryManager;
    
    public Rules4JBIProject(FileObject projectDirectory, ProjectState projectState) {
        logger.fine("Creating new Rules4JBI Project");
        
        this.projectDirectory = projectDirectory;
        this.projectState = projectState;
        
        directoryManager = new DirectoryManager(projectDirectory);
        
        lookup = createLookup();
    }
    
    private Lookup createLookup() {
        assert directoryManager != null;
        
        return Lookups.fixed(
                this,
                projectState,
                directoryManager,
                new Rules4JBISources(this),
                new Rules4JBICustomizerProvider(this),
                new Rules4JBIActionProvider(this),
                new Rules4JBIProjectInformation(this),
                new Rules4JBILogicalViewProvider(this),
                new Rules4JBIProjectOperations(this),
                new Rules4JBITemplates(),
                new Rules4JBISourceLevelQueryImplementation(this),
                new Rules4JBIFileBuiltQueryImplementation(directoryManager),
                new Rules4JBISourceForBinaryQueryImplementation(this),
                new Rules4JBISharabilityQueryImplementation(directoryManager),
                new Rules4JBIClassPathProvider(this),
                new Rules4JBIAuxiliaryConfiguration(),
                new Rules4JBIProjectOpenedHook(this),
                new Rules4JBIAntArtifactProvider(this),
                
                loadConfiguration()
        );
    }
    
    private Configuration loadConfiguration() {
        assert directoryManager != null;
        
        final Configuration config = ProjectManager.mutex().readAccess(new Mutex.Action<Configuration>() {

            public Configuration run() {
                final FileObject configFileObject = directoryManager.getConfigFile();

                assert configFileObject != null;

                InputStream inputStream = null;
                try {
                    inputStream = configFileObject.getInputStream();

                    return Configuration.load(inputStream);

                } catch (IOException e) {
                    logger.fine("Failed to open the configuration file: " + e.getMessage());

                    projectState.markModified();
                    return new Configuration();

                } catch (InvalidConfigurationException e) {
                    logger.fine("Could not parse the configuration file: " + e.getMessage());

                    projectState.markModified();
                    return new Configuration();

                } finally {
                    if (inputStream != null) {
                        try {
                            inputStream.close();

                        } catch (IOException e) {
                            logger.fine("Failed to properly close the input stream: " + e.getMessage());
                        }
                    }
                }
            }
        });
        
        logger.fine("Initial configuration: " + config);

        config.addPropertyChangeListener(new PropertyChangeListener() {

            public void propertyChange(PropertyChangeEvent evt) {
                logger.log(Level.FINE, "Property \"{0}\" changed from \"{1}\" to \"{2}\"",
                        new Object[] {evt.getPropertyName(), evt.getOldValue(), evt.getNewValue()});

                Rules4JBIProject.this.projectState.markModified();
            }
        });
        
        return config;
    }
    
    public Lookup getLookup() {
        return lookup;
    }

    public FileObject getProjectDirectory() {
        return projectDirectory;
    }

    public DirectoryManager getDirectoryManager() {
        return directoryManager;
    }
}
