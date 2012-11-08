/*
 * @(#)Rules4JBISources.java        $Revision: 1.4 $ $Date: 2009/01/14 02:53:10 $
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

import java.awt.Image;
import java.beans.BeanInfo;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.spi.project.support.GenericSources;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataFolder;
import org.openide.nodes.Node;
import org.openide.util.Utilities;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2009/01/14 02:53:10 $
 * 
 * @since 0.1
 */
public final class Rules4JBISources implements Sources {
    
    private static final String GENERIC_SOURCE_GROUP_NAME = "generic";
    
    private static final String JAVA_SOURCE_GROUP_NAME = "java";
    
    private static final String JAVA_SOURCE_GROUP_DISPLAY_NAME = "Source Packages";
    
    private static Image PACKAGE_BADGE =
            Utilities.loadImage("org/openesb/components/rules4jbi/netbeans/resources/packageBadge.gif");

    private final Rules4JBIProject project;
    
    private Icon normalIcon = null;
    
    private Icon openedIcon = null;

    public Rules4JBISources(Rules4JBIProject project) {
        this.project = project;
    }

    public SourceGroup[] getSourceGroups(String type) {
        if (Sources.TYPE_GENERIC.equals(type)) {
            return new SourceGroup[] {
                GenericSources.group(project, project.getProjectDirectory(), GENERIC_SOURCE_GROUP_NAME,
                        ProjectUtils.getInformation(project).getDisplayName(), null, null)
            };
            
        } else if (JavaProjectConstants.SOURCES_TYPE_JAVA.equals(type)) {
            
            /*
             * We need a directory that we know exists in the project's directory structure
             * to retrieve the platform specific folder icon from it;
             * we will use the source directory because we need it later anyway.
             */
            final FileObject sourceDirectory = project.getDirectoryManager().getSourceDirectory();
            
            synchronized(this) {
                if (normalIcon == null || openedIcon == null) {
                    final Node nodeDelegate = DataFolder.findFolder(sourceDirectory).getNodeDelegate();

                    normalIcon = new ImageIcon(Utilities.mergeImages(
                            nodeDelegate.getIcon(BeanInfo.ICON_COLOR_16x16), PACKAGE_BADGE, 7, 7));

                    openedIcon = new ImageIcon(Utilities.mergeImages(
                            nodeDelegate.getOpenedIcon(BeanInfo.ICON_COLOR_16x16), PACKAGE_BADGE, 7, 7));
                }
            }
            
            return new SourceGroup[] {
                
                /*
                 * Due to a bug in GenericSources.group() method, icons have to be in reversed order
                 * as the API states, i.e., firt goes the opened icon, than the normal icon.
                 */
                GenericSources.group(project, sourceDirectory, JAVA_SOURCE_GROUP_NAME,
                        JAVA_SOURCE_GROUP_DISPLAY_NAME, openedIcon, normalIcon)
            };
            
        } else {
            return new SourceGroup[0];
        }
    }
    
    public void addChangeListener(ChangeListener listener) {}
    
    public void removeChangeListener(ChangeListener listener) {}
}
