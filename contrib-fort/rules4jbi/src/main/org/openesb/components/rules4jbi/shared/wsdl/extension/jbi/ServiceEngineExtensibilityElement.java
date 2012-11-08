/*
 * @(#)ServiceEngineExtensibilityElement.java        $Revision: 1.1 $ $Date: 2008/11/24 12:49:33 $
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

package org.openesb.components.rules4jbi.shared.wsdl.extension.jbi;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * This class represents jbi wsdl binding extension used to identify service engines.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/24 12:49:33 $
 * 
 * @see JSR 208, Section 5.5.6.1
 * @since 0.1
 */
public class ServiceEngineExtensibilityElement implements ExtensibilityElement, Serializable {
    
    public static final String PREFERRED_NAMESPACE_PREFIX = "jbi";
    
    public static final String NAMESPACE_URI = "http://java.sun.com/xml/ns/jbi/binding/service+engine";
    
    public static final String LOCAL_NAME = "binding";
    
    public static final QName QUALIFIED_NAME = new QName(NAMESPACE_URI, LOCAL_NAME);

    private static final long serialVersionUID = -2449366228226639442L;
    
    private QName elementType = QUALIFIED_NAME;
    
    private Boolean required = null;
    
    public QName getElementType() {
        return elementType;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }
}
