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
 * @(#)ModelFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model;

import java.io.File;
import java.io.IOException;

import org.apache.xmlbeans.XmlException;
import org.w3c.dom.Node;

import com.sun.jbi.workflow.model.impl.TaskImpl;
import com.sun.jbi.workflow.model.utl.Messages;
import com.sun.jbi.workflow.model.xmlbeans.TaskDocument;

public class ModelFactory {

    private static final Messages MESSAGES = 
        Messages.getMessages(ModelFactory.class);
    private static ModelFactory mInstance;
    
    /**
     * Parse wf file and returns the required Tasks model
     * @param fileName The file name of the wf file
     * @return Tasks model
     */
    private ModelFactory () {
        
    }
    
    public static ModelFactory getInstance () {
        if (mInstance == null) {
            mInstance = new ModelFactory ();
        }
        return mInstance;
    }
    
    
    public com.sun.jbi.workflow.model.Task getTaskModel (String fileName) throws ModelException {
        File wfFile  = new File (fileName);
        TaskDocument taskDoc = null;
        try {
            taskDoc = TaskDocument.Factory.parse( new File( fileName ) );
        } catch (XmlException e) {
            // TODO Auto-generated catch block
            throw new ModelException (MESSAGES.getString("ModelFactory.Parse_xmlException", fileName));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            throw new ModelException (MESSAGES.getString("ModelFactory.IOException", fileName));

        }
        com.sun.jbi.workflow.model.Task modelTask = new TaskImpl (taskDoc.getTask(), wfFile.getParent());
        return modelTask;     
    }
    
    public com.sun.jbi.workflow.model.Task getTaskModel (Node node) throws ModelException {
        TaskDocument taskDoc = null;
        try {
            taskDoc = TaskDocument.Factory.parse(node);
        } catch (XmlException e) {
            // TODO Auto-generated catch block
            throw new ModelException (MESSAGES.getString("ModelFactory.Parse_xmlException", node.getBaseURI()));
        }         
        
        com.sun.jbi.workflow.model.Task modelTasks = new TaskImpl (taskDoc.getTask(), node.getBaseURI());
        return modelTasks;     
    }


}
