/*
 * @(#)Rules4JBILogicalViewProvider.java        $Revision: 1.2 $ $Date: 2008/10/25 22:02:56 $
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

import org.netbeans.api.project.Project;
import org.openesb.components.rules4jbi.netbeans.project.nodes.ProjectNode;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.openide.nodes.Node;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/10/25 22:02:56 $
 * 
 * @since 0.1
 */
public class Rules4JBILogicalViewProvider implements LogicalViewProvider {
    
    private final Project project;

    public Rules4JBILogicalViewProvider(Project project) {
        this.project = project;
    }

    public Node createLogicalView() {
        return new ProjectNode(project);
    }

    public Node findPath(Node root, Object target) {
        //TODO: implement
        return null;
    }
}
