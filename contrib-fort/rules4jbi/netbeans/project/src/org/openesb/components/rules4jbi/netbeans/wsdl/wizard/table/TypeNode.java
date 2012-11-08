/*
 * @(#)TypeNode.java        $Revision: 1.2 $ $Date: 2008/11/10 04:45:34 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

import java.awt.Image;
import java.util.List;

import org.openide.nodes.AbstractNode;
import org.openide.nodes.ChildFactory;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.Utilities;

import org.openesb.components.rules4jbi.netbeans.project.nodes.ProjectNode;

/**
 * Node representing types that are selectable either from the source packages
 * node or from the business objects libraries node.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/10 04:45:34 $
 * 
 * @since 0.3
 */
public class TypeNode extends AbstractNode {

    public TypeNode(final Node sourcePackages, final Node librariesNode) {
        super(Children.create(new ChildFactory<ProjectNode.Type>() {
            
            @Override
            protected boolean createKeys(List<ProjectNode.Type> toPopulate) {
                toPopulate.add(ProjectNode.Type.SOURCES);
                toPopulate.add(ProjectNode.Type.LIBRARIES);
        
                return true;
            }

            @Override
            protected Node createNodeForKey(ProjectNode.Type key) {
        
                switch (key) {
                case SOURCES:
                    return sourcePackages;
                    
                case LIBRARIES:
                    return librariesNode;
                
                default:
                    throw new AssertionError("Wrong node key specified");
                }
            }
            
        }, false));
    }
    
    @Override
    public Image getIcon(int type) {
        return Utilities.loadImage(
                "org/openesb/components/rules4jbi/netbeans/resources/projectIcon.png");
    }

    @Override
    public Image getOpenedIcon(int type) {
        return getIcon(type);
    }

    @Override
    public String getDisplayName() {
        return "Types";
    }
}
