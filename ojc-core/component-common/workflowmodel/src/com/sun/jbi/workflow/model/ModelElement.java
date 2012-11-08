/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ModelElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model;

import java.util.List;
import javax.xml.namespace.QName;
import org.apache.xmlbeans.XmlObject;
/* 
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License. 
 * 
 * You can obtain a copy of the license at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License. 
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * If applicable add the following below this CDDL HEADER, 
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: Portions Copyright 
 * [year] [name of copyright owner] 
 */
/* 
 * 
 * 
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

public interface ModelElement {
    
    public static final String XPATH10 = "urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0";
    public static final String XPATH20 =  "urn:oasis:names:tc:wsbpel:2.0:sublang:xpath2.0";    
    /**
     * Returns enclosing parent ModelElement
     * @return The enclosing parent ModelElement, null if it is the
     * root ModelElement
     */
    ModelElement getParent ();
    
    /**
     * return the node name
     * delegate to corresponding method on dom Node
     */
    String getNodeName();
    
    /**
     * return the qualified name of this element.
     * qualified name will have namespaceURI from where this element is coming from and local name 
     * of this element.
     */
    QName getQualifiedName();
    
    /**
     * return a list of child ModelElement
     */
    List<ModelElement> getChildren();
    
    /**
     * Return the xpath expression which identifies
     * this model element in workflow definition
     */
    XPathInfo getXPathInfo();
    
    /**
     * return the xml bean object which represents this ModelElement
     */
    XmlObject getDelegate();
    
}
