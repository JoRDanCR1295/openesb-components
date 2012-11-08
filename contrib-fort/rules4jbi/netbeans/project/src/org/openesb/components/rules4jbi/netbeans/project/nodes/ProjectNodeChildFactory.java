/*
 * @(#)ProjectNodeChildFactory.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:25 $
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

import java.util.List;

import org.openide.nodes.ChildFactory;
import org.openide.nodes.Node;

import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.spi.java.project.support.ui.PackageView;

/**
 * Factory that supplies child nodes of the project node.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:25 $
 * 
 * @see ProjectNode
 * @since 0.1
 */
public class ProjectNodeChildFactory extends ChildFactory<ProjectNode.Type> {
    
    private final Project project;

    public ProjectNodeChildFactory(Project project) {
        this.project = project;
    }

    @Override
    protected boolean createKeys(List<ProjectNode.Type> toPopulate) {
        toPopulate.add(ProjectNode.Type.DESCRIPTIONS);
        toPopulate.add(ProjectNode.Type.RULES);
        toPopulate.add(ProjectNode.Type.SCHEMAS);
        toPopulate.add(ProjectNode.Type.SOURCES);
        toPopulate.add(ProjectNode.Type.LIBRARIES);
        toPopulate.add(ProjectNode.Type.ENGINE);
        
        return true;
    }

    @Override
    protected Node createNodeForKey(ProjectNode.Type key) {
        
        switch (key) {
               
        case SOURCES:
            
            /* Never returns null */
            Sources sources = ProjectUtils.getSources(project);

            SourceGroup[] javaSourceGroups = sources.getSourceGroups(JavaProjectConstants.SOURCES_TYPE_JAVA);

            if (javaSourceGroups == null || javaSourceGroups.length != 1) {
                throw new IllegalStateException("Project's sources were not correctly initialized");
            }

            return PackageView.createPackageView(javaSourceGroups[0]);

        case RULES:
            return new RulesNodeBuilder(project).createNode();

        case ENGINE:
            return new RulesEngineNodeBuilder(project).createNode();
            
        case SCHEMAS:
            return new SchemasNodeBuilder(project).createNode();
            
        case LIBRARIES:
            return new LibrariesNodeBuilder(project).createNode();

        case DESCRIPTIONS:
            return new DescriptionsNodeBuilder(project).createNode();
            
        default:
            throw new AssertionError("Wrong node key specified");
        }
    }
}
