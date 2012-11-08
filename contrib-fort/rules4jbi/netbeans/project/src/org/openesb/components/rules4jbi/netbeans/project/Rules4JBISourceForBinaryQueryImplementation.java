/*
 * @(#)Rules4JBISourceForBinaryQueryImplementation.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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
import java.net.URL;
import java.util.logging.Logger;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.spi.java.queries.SourceForBinaryQueryImplementation;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public final class Rules4JBISourceForBinaryQueryImplementation implements SourceForBinaryQueryImplementation {

    private static final Logger logger =
            Logger.getLogger(Rules4JBISourceForBinaryQueryImplementation.class.getName());
    
    private final Rules4JBIProject project;
    
    /** The only result that we recognize - source directory for the classes directory. */
    private SourceForBinaryQuery.Result cachedResult = null;

    public Rules4JBISourceForBinaryQueryImplementation(Rules4JBIProject project) {
        this.project = project;
    }
    
    public SourceForBinaryQuery.Result findSourceRoots(URL binaryRoot) {
        logger.fine("Finding source roots for binary root " + binaryRoot.toString());
        
        FileObject binaryRootFileObject = URLMapper.findFileObject(binaryRoot);
                
        logger.fine("Binary root file object: " + FileUtil.getFileDisplayName(binaryRootFileObject));
        
        final DirectoryManager directoryManager = project.getDirectoryManager();

        if (!directoryManager.classesDirectoryExists()) {
            return null;
        }

        final FileObject sourceDirectory = directoryManager.getSourceDirectory();

        final FileObject classesDirectory = directoryManager.getOrCreateClassesDirectory();

        if (classesDirectory.equals(binaryRootFileObject)) {
            logger.finer("Returning the source directory");
            
            synchronized (this) {
                if (cachedResult == null) {
                    cachedResult = new SingleRootSourceForBinaryQueryResult(sourceDirectory);
                }
            }
            
            return cachedResult;
        }

        logger.fine("Source roots not found");
        return null;
    }
    
    private static class SingleRootSourceForBinaryQueryResult implements SourceForBinaryQuery.Result {

        private final FileObject root;

        public SingleRootSourceForBinaryQueryResult(FileObject root) {
            this.root = root;
        }
        
        public FileObject[] getRoots() {
            return new FileObject[] {root};
        }
        
        public void addChangeListener(ChangeListener listener) {}

        public void removeChangeListener(ChangeListener listener) {}
    }
}
