/*
 * @(#)FileType.java        $Revision: 1.3 $ $Date: 2008/11/12 08:26:20 $
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

package org.openesb.components.rules4jbi.netbeans.project.fileimport;

import java.io.File;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.openide.util.Utilities;

/**
 * Supported file types that can be imported into the project.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/12 08:26:20 $
 * 
 * @since 0.3
 */
public enum FileType {
    
    RULESET("Ruleset", "Ruleset File", new ImageIcon(Utilities.loadImage(
            "org/openesb/components/rules4jbi/netbeans/resources/rules.gif")),
            new FileFilter() {

                @Override
                public boolean accept(File file) {
                    return file != null;
                }

                @Override
                public String getDescription() {
                    return "All Files";
                }
            }),
    
    XML_SCHEMA("XML Schema", "XML Schema File", new ImageIcon(Utilities.loadImage(
            "org/openesb/components/rules4jbi/netbeans/resources/schemas.png")),
            new FileNameExtensionFilter("XML Schema", "xsd")),
    
    LIBRARY("Library", "Business Objects Library", new ImageIcon(Utilities.loadImage(
            "org/openesb/components/rules4jbi/netbeans/resources/libraries.gif")),
            new FileNameExtensionFilter("JAR File", "jar")),

    ENGINE("Library", "Rules Engine Library", new ImageIcon(Utilities.loadImage(
            "org/openesb/components/rules4jbi/netbeans/resources/libraries.gif")),
            new FileNameExtensionFilter("JAR File", "jar"));
            
    private final String name;
            
    private final String displayName;

    private final Icon icon;
    
    private final FileFilter fileFilter;
    
    private FileType(String name, String displayName, Icon icon, FileFilter fileFilter) {
        this.name = name;
        this.displayName = displayName;
        this.icon = icon;
        this.fileFilter = fileFilter;
    }

    public String getName() {
        return name;
    }
    
    public String getDisplayName() {
        return displayName;
    }
    
    public Icon getIcon() {
        return icon;
    }

    public FileFilter getFileFilter() {
        return fileFilter;
    }
}
