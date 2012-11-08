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
 * @(#)BPELExtElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext.bpel.impl;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * Generic implementation class for BPEL extensibility elements.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
class BPELExtElementImpl implements ExtensibilityElement,
        Serializable {

    private static final long serialVersionUID = 1L;

    protected QName _elementType;
    protected boolean _required;
    
    public QName getElementType() {
        return _elementType;
    }

    public Boolean getRequired() {
        return _required;
    }

    public void setElementType(QName elementType) {
        _elementType = elementType;
    }

    public void setRequired(Boolean required) {
        _required = required;
    }
}
