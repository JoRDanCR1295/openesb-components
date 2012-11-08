/*
 * @(#)Rules4JBISharabilityQueryImplementation.java        $Revision: 1.5 $ $Date: 2008/11/12 08:26:24 $
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
import java.util.logging.Logger;
import org.netbeans.api.queries.SharabilityQuery;
import org.netbeans.spi.queries.SharabilityQueryImplementation;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.5 $ $Date: 2008/11/12 08:26:24 $
 * 
 * @since 0.1
 */
public class Rules4JBISharabilityQueryImplementation implements SharabilityQueryImplementation {
    
    private static final Logger logger = Logger.getLogger(Rules4JBISharabilityQueryImplementation.class.getName());

    private final DirectoryManager directoryManager;

    public Rules4JBISharabilityQueryImplementation(DirectoryManager directoryManager) {
        this.directoryManager = directoryManager;
    }
    
    public int getSharability(File file) {
        logger.fine("Checking sharability of " + file.getAbsolutePath());
        
        final FileObject fileObject = FileUtil.toFileObject(file);

        final FileObject projectDirectory = directoryManager.getProjectDirectory();
        if (!projectDirectory.equals(fileObject) && !FileUtil.isParentOf(projectDirectory, fileObject)) {
            return SharabilityQuery.UNKNOWN;
        }
        
        if (projectDirectory.equals(fileObject)) {
            return SharabilityQuery.MIXED;
        }

        final FileObject sourceDirectory = directoryManager.getSourceDirectory();
        if (sourceDirectory.equals(fileObject) || FileUtil.isParentOf(sourceDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }
        
        final FileObject rulesDirectory = directoryManager.getRulesDirectory();
        if (rulesDirectory.equals(fileObject) || FileUtil.isParentOf(rulesDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }

        final FileObject descriptionsDirectory = directoryManager.getDescriptionsDirectory();
        if (descriptionsDirectory.equals(fileObject) || FileUtil.isParentOf(descriptionsDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }
        
        final FileObject schemasDirectory = directoryManager.getSchemasDirectory();
        if (schemasDirectory.equals(fileObject) || FileUtil.isParentOf(schemasDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }
        
        final FileObject librariesDirectory = directoryManager.getLibrariesDirectory();
        if (librariesDirectory.equals(fileObject) || FileUtil.isParentOf(librariesDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }

        final FileObject rulesEngineDirectory = directoryManager.getRulesEngineDirectory();
        if (rulesEngineDirectory.equals(fileObject) || FileUtil.isParentOf(rulesEngineDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }
        
        final FileObject metadataDirectory = directoryManager.getMetadataDirectory();
        if (metadataDirectory.equals(fileObject) || FileUtil.isParentOf(metadataDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }
        
        final FileObject fakeMetadataDirectory = directoryManager.getFakeMetadataDirectory();
        if (fakeMetadataDirectory.equals(fileObject) || FileUtil.isParentOf(fakeMetadataDirectory, fileObject)) {
            return SharabilityQuery.SHARABLE;
        }

        final FileObject fakeBuildScriptFile = directoryManager.getFakeBuildScriptFile();
        if (fakeBuildScriptFile.equals(fileObject)) {
            return SharabilityQuery.SHARABLE;
        }

        return SharabilityQuery.NOT_SHARABLE;
    }
}
