/*
 * @(#)Rules4JBIProjectInformation.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

import java.beans.PropertyChangeListener;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.openide.util.Utilities;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @see org.netbeans.api.project.ProjectInformation
 * @since 0.1
 */
public final class Rules4JBIProjectInformation implements ProjectInformation {
    
    private static final Icon PROJECT_ICON =
            new ImageIcon(Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/projectIcon.png"));

    private final Project project;

    public Rules4JBIProjectInformation(Project project) {
        this.project = project;
    }

    public Project getProject() {
        return project;
    }
    
    public String getName() {
        return project.getProjectDirectory().getName();
    }
    
    public String getDisplayName() {
        return project.getProjectDirectory().getName();
    }

    public Icon getIcon() {
        return PROJECT_ICON;
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        //do nothing; name, displayName, and projectIcon won't change
    }
    
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        //do nothing; name, displayName, and projectIcon won't change
    }
}
