/*
 * @(#)Descriptor.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
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

package org.openesb.components.rules4jbi.shared.classloader;

/**
 * Type descriptors of chosen Java types as they are stored in compiled classes.
 * A type descriptor consists of the internal name of the class/interface/annotation/enum
 * preceded by 'L' and followed by a semicolon.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @since 0.4
 */
final class Descriptor {
    
    static final String STRING = "Ljava/lang/String;";
    
    static final String XML_ROOT_ELEMENT = "Ljavax/xml/bind/annotation/XmlRootElement;";
    
    static final String XML_TYPE = "Ljavax/xml/bind/annotation/XmlType;";

    static final String XML_ACCESSOR_TYPE = "Ljavax/xml/bind/annotation/XmlAccessorType;";

    static final String XML_ELEMENT = "Ljavax/xml/bind/annotation/XmlElement;";

    static final String XML_ATTRIBUTE = "Ljavax/xml/bind/annotation/XmlAttribute;";
    
    static final String XML_ACCESS_TYPE = "Ljavax/xml/bind/annotation/XmlAccessType;";
    
    private Descriptor() {}
}
