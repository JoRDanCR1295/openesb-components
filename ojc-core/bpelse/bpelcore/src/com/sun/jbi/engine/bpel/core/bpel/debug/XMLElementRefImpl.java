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
 * @(#)XMLElementRefImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import java.util.List;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.XMLElementRef;

import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLElement;

/**
 *
 * @author zgursky
 */
public class XMLElementRefImpl implements XMLElementRef {
    private XMLElement mXMLElement;
    
    /** Creates a new instance of XMLNodeRefImpl */
    public XMLElementRefImpl(XMLElement xmlElement) {
        mXMLElement = xmlElement;
    }

    public String getLocalName() {
        return mXMLElement.getLocalName();
    }

    public String getNameAttribute() {
        XMLAttribute attr = ((XMLElement)mXMLElement).getAttribute("name");
        if (attr != null) {
            return attr.getValue();
        } else {
            return null;
        }
    }
    
    public int getChildrenCount() {
        int count = 0;
        List children = mXMLElement.getChildren();
        if (children != null) {
            for (Object child : children) {
                if (child instanceof XMLElement) {
                    count++;
                }
            }
        }
        return count;
    }

    public XMLElementRef getChild(int index) {
        int curIndex = 0;
        List children = mXMLElement.getChildren();
        if (children != null) {
            for (Object child : children) {
                if (child instanceof XMLElement) {
                    if (curIndex == index) {
                        return new XMLElementRefImpl((XMLElement)child);
                    }
                    curIndex++;
                }
            }
        }
        return null;
    }
    
}
