/*
 * @(#)Rules4JBISourceLevelQueryImplementation.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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
import java.util.logging.Logger;
import org.netbeans.spi.java.queries.SourceLevelQueryImplementation;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class Rules4JBISourceLevelQueryImplementation implements SourceLevelQueryImplementation {

    private static String DEFAULT_SOURCE_LEVEL = "1.6";
    
    private static final Logger logger =
            Logger.getLogger(Rules4JBISourceLevelQueryImplementation.class.getName());

    private final Rules4JBIProject project;

    public Rules4JBISourceLevelQueryImplementation(Rules4JBIProject project) {
        this.project = project;
    }
    
    public String getSourceLevel(FileObject javaFile) {
        logger.fine("Evaluating source level of file " + FileUtil.getFileDisplayName(javaFile));
        
        DirectoryManager directoryManager = project.getDirectoryManager();

        final FileObject sourceDirectory = directoryManager.getSourceDirectory();
        
        //TODO: do we need to provide source level also for the src directory, or just for the files undeneath?
        if (!sourceDirectory.equals(javaFile) && !FileUtil.isParentOf(sourceDirectory, javaFile)) {
            return null;
        }
        
        return DEFAULT_SOURCE_LEVEL;
    }
}
