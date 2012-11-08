/*
 * @(#)ProjectNode.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:25 $
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
import java.util.ResourceBundle;

import javax.swing.Action;

import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ActionProvider;
import org.netbeans.spi.project.ui.support.CommonProjectActions;
import org.netbeans.spi.project.ui.support.ProjectSensitiveActions;

import org.openesb.components.rules4jbi.netbeans.project.actions.ActionsFactory;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.util.lookup.Lookups;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;

/**
 * Root node of the project, shown in the project explorer view.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:25 $
 * 
 * @since 0.1
 */
public class ProjectNode extends AbstractNode {
    
    public enum Type { RULES, SOURCES, SCHEMAS, LIBRARIES, DESCRIPTIONS, ENGINE }

    private ResourceBundle resourceBundle = NbBundle.getBundle(ProjectNode.class);
    
    private final Project project;

    public ProjectNode(Project project) {
        
        /*
         * To make the project-sensitive actions work, we need to put the project
         * into the lookup of the root node; the presence of a directory manager
         * identifies this project as a rules4jbi project.
         */
        super(Children.create(new ProjectNodeChildFactory(project), false),
                Lookups.fixed(project, project.getLookup().lookup(DirectoryManager.class)));
        
        this.project = project;
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
        return project.getProjectDirectory().getName();
    }

    @Override
    public Action[] getActions(boolean context) {
        if (context) {
            return super.getActions(true);
            
        } else {
            return new Action[] {
                CommonProjectActions.newFileAction(),
                null,
                ProjectSensitiveActions.projectCommandAction(
                        ActionProvider.COMMAND_BUILD,
                        resourceBundle.getString("ProjectNode.build.action.name"),
                        null),
                ProjectSensitiveActions.projectCommandAction(
                        ActionProvider.COMMAND_REBUILD,
                        resourceBundle.getString("ProjectNode.clean.and.build.action.name"),
                        null),
                ProjectSensitiveActions.projectCommandAction(
                        ActionProvider.COMMAND_CLEAN,
                        resourceBundle.getString("ProjectNode.clean.action.name"),
                        null),
                null,
                ActionsFactory.importFileAction(),
                ActionsFactory.compileSchemasAction(),
                ActionsFactory.createWSDLAction(),
                null,
                CommonProjectActions.setAsMainProjectAction(),
                CommonProjectActions.closeProjectAction(),
                null,
                CommonProjectActions.renameProjectAction(),
                CommonProjectActions.moveProjectAction(),
                CommonProjectActions.copyProjectAction(),
                CommonProjectActions.deleteProjectAction(),
                null,
                CommonProjectActions.customizeProjectAction()
            };
        }
    }
}
