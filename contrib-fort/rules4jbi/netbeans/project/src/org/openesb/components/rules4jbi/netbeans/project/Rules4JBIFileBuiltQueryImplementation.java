/*
 * @(#)Rules4JBIFileBuiltQueryImplementation.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.api.queries.FileBuiltQuery;
import org.netbeans.spi.queries.FileBuiltQueryImplementation;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public final class Rules4JBIFileBuiltQueryImplementation implements FileBuiltQueryImplementation {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIFileBuiltQueryImplementation.class.getName());

    private final DirectoryManager directoryManager;

    public Rules4JBIFileBuiltQueryImplementation(DirectoryManager directoryManager) {
        this.directoryManager = directoryManager;
    }
    
    public /* synchronized */ FileBuiltQuery.Status getStatus(FileObject file) {
        logger.fine("Checking built status of " + FileUtil.getFileDisplayName(file));
        
        final FileObject sourceDirectory = directoryManager.getSourceDirectory();
        if (!FileUtil.isParentOf(sourceDirectory, file)) {
            return null;
        }
        
        return new AlwaysBuiltFileBuiltQueryStatus();
    }
    
    private static class AlwaysBuiltFileBuiltQueryStatus implements FileBuiltQuery.Status {

        public boolean isBuilt() {
            return true;
        }

        public void addChangeListener(ChangeListener listener) {}
        
        public void removeChangeListener(ChangeListener listener) {}
    }
}
