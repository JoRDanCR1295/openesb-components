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
 * @(#)SAPFmOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.impl.WSDLExtensibleElementImpl;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public abstract class SAPOperation extends WSDLExtensibleElementImpl implements Serializable {
    
   public QName getElementType() {
        return fieldElementType;
    }

    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    public Boolean getRequired() {
        return fieldRequired;
    }

    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    public void setSAPOperationInput(WSDLInput input) {
        mInput = input;
    }
    
    public WSDLInput getSAPOperationInput() {
        return mInput;
    }
    
    public void setSAPOperationOutput(WSDLOutput output) {
        mOutput = output;
    }
    
    public WSDLOutput getSAPOperationOutput() {
        return mOutput;
    }
    
    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(SAPOperation.class);
    private static final Logger mLogger = Messages.getLogger(SAPOperation.class); 
    
    private QName fieldElementType = null;
    private Boolean fieldRequired = Boolean.FALSE;
    private WSDLInput mInput = null;
    private WSDLOutput mOutput = null;

}
