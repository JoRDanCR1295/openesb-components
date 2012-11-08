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
 * @(#)TaskModelFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.common.model.impl;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.common.model.TaskInput;
import com.sun.jbi.engine.workflow.common.model.TaskModelFactory;
import com.sun.jbi.engine.workflow.common.model.TaskOutput;
import com.sun.jbi.engine.workflow.common.model.TaskPrincipal;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 *
 *
 */
public class TaskModelFactoryImpl extends TaskModelFactory {
    
    
    
    /** Creates a new instance of TaskModelFactoryImpl */
    public TaskModelFactoryImpl() {
    }
    

    public TaskPrincipal createPrincipal(String name, TaskPrincipal.PrincipalType principalType) {
        TaskPrincipal principal = null;
        if (principalType == TaskPrincipal.PrincipalType.User) {
            principal =  new TaskUserPrincipalImpl(name);
        } else if (principalType == TaskPrincipal.PrincipalType.Group) {
            principal =  new TaskGroupPrincipalImpl(name);
        }
        return principal;
    }
    
    public TaskPrincipal createDefaultPrincipal () {
        TaskPrincipal principal =  new TaskUserPrincipalImpl(TaskPrincipal.DEFAULT_PRINCIPAL);
        return principal;
    }

    public TaskInput createTaskInput(Source source) throws WorkflowException {
        //TODO change source to DOMSource
        DOMResult result = new DOMResult();
        try {
        Transformer transformer = TransformerFactory.newInstance().newTransformer();
        transformer.transform(source, result);
        Node node = result.getNode();
        
        Document normalDoc = null;
        if (node instanceof Document) {
            normalDoc = (Document) node;
        } else {
            normalDoc = ((Element) node).getOwnerDocument();
        }

        Element normalRoot = normalDoc.getDocumentElement();
            
        return new TaskInputImpl(normalRoot);
        } catch(Exception ex) {
        	throw new WorkflowException(ex);
        }
    }

    public TaskOutput createTaskOutput(Source source) throws WorkflowException {
       DOMSource domSrc = (DOMSource) source;
       Node node = domSrc.getNode();
       Document normalDoc = null;
       if (node instanceof Document) {
           normalDoc = (Document) node;
       } else {
           normalDoc = ((Element) node).getOwnerDocument();
       }

       Element normalRoot = normalDoc.getDocumentElement();
           
       return new TaskOutputImpl(normalRoot);       
       
    }
    
    public  TaskInput createTaskInput(Element source) throws WorkflowException {
        return new TaskInputImpl(source);
    }
    
    public TaskOutput createTaskOutput(Element source)throws WorkflowException {
        return new TaskOutputImpl(source);
    }
    

}
