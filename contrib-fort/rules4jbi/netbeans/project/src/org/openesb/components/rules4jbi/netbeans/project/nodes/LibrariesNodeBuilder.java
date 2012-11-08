/*
 * @(#)LibrariesNodeBuilder.java        $Revision: 1.4 $ $Date: 2008/11/12 08:26:25 $
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

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileType;
import org.openide.util.Utilities;

import org.netbeans.api.project.Project;

/**
 * Builder for the "Business Objects Libraries" node.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/11/12 08:26:25 $
 * 
 * @since 0.3
 */
public final class LibrariesNodeBuilder extends DirectoryNodeBuilder {

    private static final String LIBRARIES_NODE_DISPLAY_NAME = "Business Objects Libraries";
    
    private static final Image LIBRARIES_BADGE =
            Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/librariesBadge.png");

    public LibrariesNodeBuilder(Project project) {
        super(project.getLookup().lookup(DirectoryManager.class).getLibrariesDirectory());
        
        displayName(LIBRARIES_NODE_DISPLAY_NAME);
        badge(LIBRARIES_BADGE);
        addExtension("jar");
        supportImport(FileType.LIBRARY, project.getLookup().lookup(DirectoryManager.class));
        disablePrivilegedTemplates();
    }
}
