/*
 * @(#)RulesNodeBuilder.java        $Revision: 1.4 $ $Date: 2008/11/12 08:26:25 $
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
 * Builder for the "Ruleset Files" node.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/11/12 08:26:25 $
 * 
 * @since 0.3
 */
public final class RulesNodeBuilder extends DirectoryNodeBuilder {

    private static final String RULES_NODE_DISPLAY_NAME = "Ruleset Files";
    
    private static final Image RULES_BADGE =
            Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/rulesBadge.png");

    public RulesNodeBuilder(Project project) {
        super(project.getLookup().lookup(DirectoryManager.class).getRulesDirectory());
        
        displayName(RULES_NODE_DISPLAY_NAME);
        badge(RULES_BADGE);
        supportImport(FileType.RULESET, project.getLookup().lookup(DirectoryManager.class));
        privilegedTemplates(new RulesNodePrivilegedTemplates());
    }
    
    private static class RulesNodePrivilegedTemplates implements PrivilegedTemplates {

        private static final String[] PRIVILEGED_TEMPLATES = new String[] {
            "Templates/XML/XMLDocument.xml",
            "Templates/Other/file"
        };

        public String[] getPrivilegedTemplates() {
            return PRIVILEGED_TEMPLATES;
        }
    }
}
