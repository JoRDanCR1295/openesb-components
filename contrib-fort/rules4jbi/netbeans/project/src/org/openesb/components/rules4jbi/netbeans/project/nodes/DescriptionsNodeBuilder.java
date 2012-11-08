/*
 * @(#)DescriptionsNodeBuilder.java        $Revision: 1.2 $ $Date: 2008/11/12 08:26:25 $
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

package org.openesb.components.rules4jbi.netbeans.project.nodes;

import java.awt.Image;
import java.io.IOException;
import java.util.logging.Logger;

import org.openide.filesystems.FileChangeAdapter;
import org.openide.filesystems.FileEvent;
import org.openide.filesystems.FileRenameEvent;
import org.openide.util.Utilities;

import org.netbeans.api.project.Project;

import org.openesb.components.rules4jbi.shared.config.Configuration;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.directory.RuntimeIOException;
import org.openesb.components.rules4jbi.netbeans.util.FileObjectSaver;

/**
 * Builder for the node that contains the files that describe
 * the current service unit - <code>jbi.xml</code> and the <code>.wsdl</code> file.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/12 08:26:25 $
 * 
 * @since 0.3
 */
public final class DescriptionsNodeBuilder extends DirectoryNodeBuilder {

    private static final String DESCRIPTIONS_NODE_DISPLAY_NAME = "Service Descriptions";
    
    private static final Image DESCRIPTIONS_BADGE =
            Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/descriptionsBadge.png");
    
    private static final Logger logger = Logger.getLogger(DescriptionsNodeBuilder.class.getName());
    
    private final DirectoryManager directoryManager;
    
    private final Configuration configuration;

    public DescriptionsNodeBuilder(Project project) {
        super(project.getLookup().lookup(DirectoryManager.class).getDescriptionsDirectory());
        
        directoryManager = project.getLookup().lookup(DirectoryManager.class);
        configuration = project.getLookup().lookup(Configuration.class);
        
        displayName(DESCRIPTIONS_NODE_DISPLAY_NAME);
        badge(DESCRIPTIONS_BADGE);
        addExtension("wsdl");
        addExtension("xml");
        disablePrivilegedTemplates();
        
        addFileChangeListener(new FileChangeAdapter() {

            @Override
            public void fileDeleted(FileEvent fe) {
                final String deletedFileName = fe.getFile().getNameExt();
                
                logger.fine("Deleted file '" + deletedFileName + "'");
                
                if (deletedFileName.equals(directoryManager.getWSDLFileName())) {
                    logger.finer("Deleted current WSDL file");
                    
                    changeWSDLFileName("");
                }
            }

            @Override
            public void fileRenamed(FileRenameEvent fe) {
                final String oldName = fe.getName() + "." + fe.getExt();
                final String newName = fe.getFile().getNameExt();

                logger.finer("File '" + oldName + "' renamed to '" + newName + "'");
                
                if (oldName.equals(directoryManager.getWSDLFileName())) {
                    logger.finer("Renamed current WSDL file");

                    changeWSDLFileName(newName);
                }
            }
        });
    }
    
    private void changeWSDLFileName(final String newName) {
        logger.finest("WSDL file name changed to '" + newName + "'");
        
        directoryManager.setWSDLFileName(newName);
        configuration.setWSDLFile(newName);

        try {
            FileObjectSaver.save(configuration, directoryManager.getConfigFile());

        } catch (IOException e) {
            throw new RuntimeIOException("Failed to save configuration file", e);
        }
    }
}
