/*
 * @(#)SchemasNodeBuilder.java        $Revision: 1.6 $ $Date: 2008/11/12 08:26:25 $
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

import org.openide.util.Utilities;

import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ui.PrivilegedTemplates;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.fileimport.FileType;

/**
 * Builder for the "XML Schema Files" node.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.6 $ $Date: 2008/11/12 08:26:25 $
 * 
 * @since 0.3
 */
public final class SchemasNodeBuilder extends DirectoryNodeBuilder {

    private static final String SCHEMAS_NODE_DISPLAY_NAME = "XML Schema Files";
    
    private static final Image SCHEMAS_BADGE =
            Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/schemasBadge.png");

    public SchemasNodeBuilder(Project project) {
        super(project.getLookup().lookup(DirectoryManager.class).getSchemasDirectory());
        
        displayName(SCHEMAS_NODE_DISPLAY_NAME);
        badge(SCHEMAS_BADGE);
        addExtension("xsd");
        supportImport(FileType.XML_SCHEMA, project.getLookup().lookup(DirectoryManager.class));
        showSubfolders();
        privilegedTemplates(new SchemasNodePrivilegedTemplates());
    }
    
    private static class SchemasNodePrivilegedTemplates implements PrivilegedTemplates {

        private static final String[] PRIVILEGED_TEMPLATES = new String[] {
            "Templates/XML/XmlSchema.xsd",
            "Templates/Other/Folder",
        };

        public String[] getPrivilegedTemplates() {
            return PRIVILEGED_TEMPLATES;
        }
    }
}
