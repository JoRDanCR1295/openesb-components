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
 * @(#)XMLNodeSelectionModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model.selection;

import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;

/**
 *
 * @author Sun Microsystems
 */
public interface XMLNodeSelectionModel {
    
    /**
     * Get the document whose selection is managed by this
     */
    XMLDocument getOwnerDocument();
    
    /**
     * select a XMLNode in selection model. It will fire event to notify all
     * registered XMLNodeSelectionListener
     * @param source XMLNode
     */
    void setSelected(XMLNode[] source);
    
    /**
     * get currently selected XMLNode in this selection model.
     */
    XMLNode[] getSelected();
    
    /**
     * add selection listener
     * @param l XMLNodeSelectionListener
     */
    void addXMLNodeSelectionListener(XMLNodeSelectionListener l);
    
    /**
     * remove selection listener
     * @param l XMLNodeSelectionListener
     */
    void removeXMLNodeSelectionListener(XMLNodeSelectionListener l);
    
}
