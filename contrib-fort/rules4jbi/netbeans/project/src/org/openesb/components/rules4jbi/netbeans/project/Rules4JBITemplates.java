/*
 * @(#)Rules4JBITemplates.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

import org.netbeans.spi.project.ui.PrivilegedTemplates;
import org.netbeans.spi.project.ui.RecommendedTemplates;

/**
 * This class defines file templates that are part of the new submenu in the project's context menu
 * (implementation of <code>PrivilegedTemplates</code>), and categories of file templates that the user
 * is allowed to select from the new file wizard (implementation of <code>RecommendedTemplates</code>).
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @see org.netbeans.spi.project.ui.PrivilegedTemplates
 * @see org.netbeans.spi.project.ui.RecommendedTemplates
 * @since 0.1
 */
final class Rules4JBITemplates implements PrivilegedTemplates, RecommendedTemplates {
    
    private static final String[] PRIVILEGED_TEMPLATES = new String[] {
        "Templates/Classes/Class.java",
        "Templates/Classes/Package",
        "Templates/Classes/package-info.java",
//        "Templates/XML/XMLDocument.xml",
//        "Templates/Other/file"
    };
    
    private static final String[] RECOMMENDED_TYPES = new String[] {
        "java-classes",
        "oasis-XML-catalogs",
        "XML",
        "simple-files"
    };

    public String[] getPrivilegedTemplates() {
        return PRIVILEGED_TEMPLATES;
    }
    
    public String[] getRecommendedTypes() {
        return RECOMMENDED_TYPES;
    }
}
