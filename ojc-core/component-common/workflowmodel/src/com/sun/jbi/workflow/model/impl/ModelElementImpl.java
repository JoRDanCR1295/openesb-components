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
 * @(#)ModelElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.impl;

import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.TaskElement;
import com.sun.jbi.workflow.model.XPathInfo;
import com.sun.jbi.workflow.model.utl.ModelUtil;

import java.util.Collections;
import java.util.List;
import javax.xml.namespace.QName;
import org.apache.xmlbeans.XmlObject;

/**
 *
 * 
 */
public class ModelElementImpl implements ModelElement, TaskElement {
    
    private ModelElement mParent;
    
    private XmlObject mDelegate;
    
    /** Creates a new instance of ModelElementImpl */
    public ModelElementImpl(XmlObject delegate, ModelElement parent) {
        this.mDelegate = delegate;
        this.mParent = parent;
    }

    public ModelElement getParent() {
        return this.mParent;
    }

    public String getNodeName() {
        return this.mDelegate.getDomNode().getNodeName();
    }

    public QName getQualifiedName() {
        QName qName = new QName(this.mDelegate.getDomNode().getNamespaceURI(), this.mDelegate.getDomNode().getLocalName());
        
        return qName;
    }

    public List<ModelElement> getChildren() {
        return Collections.EMPTY_LIST;
    }

    public XPathInfo getXPathInfo() {
        return ModelUtil.generateXpathInfo(this);
    }

    public XmlObject getDelegate() {
        return this.mDelegate;
    }

    public Task getTask() {
        // TODO Auto-generated method stub
       Task root = null;
       ModelElement search = this;
       while (search != null && !(search  instanceof Task)) {
           search = search.getParent();
       }
       if (search != null) {
           root = (Task) search;
       }
       return root;
    }
}
