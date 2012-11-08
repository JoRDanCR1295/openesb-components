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
 * @(#)WSDLMessageImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.WSDLMessage;
import org.w3c.dom.Element;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;

import javax.wsdl.Message;
import javax.wsdl.Part;

/**
 * WSDL Message implementation in the target BPEL engine. 
 * @author Sun Microsystems
 * @version 
 */
public class WSDLMessageImpl implements WSDLMessage {
    
    private WSMessage mDelegate;
    private Message mModel;

    /** Creates a new instance of WSDLMessageImpl */
    public WSDLMessageImpl(Message model, WSMessage delegate) {
    	if (model == null) {
    		throw new NullPointerException("WSDL message model is null");
    	}
    	if (delegate == null) {
    		throw new NullPointerException("WSDL message data is null");
    	}
        mDelegate = delegate;   
        mModel = model;
    }
    
    /**
     * Returns the part value.
     * @return  the part string value.
     */
    public String getPart(String partName) {
        String result = null;
        Element part = mDelegate.getPart(partName);
        if (part != null) {
            result = DOMHelper.createXmlString(part);
            if (result == null) {
                result = "";
            }
        }
        return result;
    }
    
    /**
     * Returns part names of the WSDLMessage.
     */
    public String[] getParts() {
        Map parts = mModel.getParts();
        String[] partsNames = new String[parts.size()];
        int i = 0;
        for (Iterator iter = parts.values().iterator(); iter.hasNext();) {
            Part part = (Part) iter.next();
            partsNames[i] = part.getName();
            i++;
        } 
        return partsNames;
    }    
}
