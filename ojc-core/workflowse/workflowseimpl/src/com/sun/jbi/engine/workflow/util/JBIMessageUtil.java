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
 * @(#)JBIMessageUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.util;

import java.util.Map;

import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.clientapi.operations.ClientOperationsHelper;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;

public class JBIMessageUtil {

    public static Element makeJBIMessage(Map<String, Element> partsMap, Operation operation)
            throws WorkflowException {
        // TODO Auto-generated method stub
        WrapperBuilder wrapperBuilder = ClientOperationsHelper.newWrapperBuilder();
        Document normalDoc = null;
        try {
            normalDoc = XmlUtil.createDocument(true);
            Message msg = operation.getOutput().getMessage();           
            wrapperBuilder.initialize(normalDoc, msg, operation.getOutput().getName());
            for (Map.Entry<String, Element> entry : partsMap.entrySet()) {
                String partName = entry.getKey();
                Element partEl = entry.getValue();
                Part part = msg.getPart(partName);
                if (part != null) {
                    if (part.getElementName() != null) {
                        wrapperBuilder.addPart(part.getName(), partEl);
                    } else if (part.getTypeName() != null) {
                        wrapperBuilder.addPart(part.getName(), partEl.getChildNodes());
                    }
                }
    
            }
            normalDoc = wrapperBuilder.getResult();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw new WorkflowException("FailToCreateWrapperBuilder", I18n.loc(
                    "WLM-6068: Failed to create wrapper builder {0}", e.getMessage()), e);
    
        }
        return normalDoc.getDocumentElement();
    }
    
    
    

    public static Element makeJBIMessageWithNodeList(Map<String, NodeList> partsMap, Operation operation)
            throws WorkflowException {
        // TODO Auto-generated method stub
        WrapperBuilder wrapperBuilder = ClientOperationsHelper.newWrapperBuilder();
        Document normalDoc = null;
        try {
            normalDoc = XmlUtil.createDocument(true);
            Message msg = operation.getInput().getMessage();
            wrapperBuilder.initialize(normalDoc, msg, operation.getInput().getName());
            for (Map.Entry<String, NodeList> entry : partsMap.entrySet()) {
                String partName = entry.getKey();
                NodeList partNodes = entry.getValue();
                Part part = msg.getPart(partName);
                if (part != null) {
                    wrapperBuilder.addPart(part.getName(), partNodes);
                }
    
            }
            normalDoc = wrapperBuilder.getResult();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw new WorkflowException("FailToCreateWrapperBuilder", I18n.loc(
                    "WLM-6068: Failed to create wrapper builder {0}", e.getMessage()), e);
    
        }
        return normalDoc.getDocumentElement();
    }
    
    
    public static Element makeEmptyJBIMessage(Operation operation)
            throws WorkflowException {
        // TODO Auto-generated method stub
        WrapperBuilder wrapperBuilder = ClientOperationsHelper.newWrapperBuilder();
        Document normalDoc = null;
        try {
            normalDoc = XmlUtil.createDocument(true);
            Message msg = operation.getOutput().getMessage();
            wrapperBuilder.initialize(normalDoc, msg, operation.getOutput().getName());
            Map<String, Part> partsMap = msg.getParts();
            for (Map.Entry<String, Part> entry : partsMap.entrySet()) {
                String partName = entry.getKey();
                Part part = entry.getValue();
                if (part != null) {
                    Text empty = normalDoc.createTextNode("");
                    wrapperBuilder.addPart(part.getName(), XmlUtil.newSingleNodeList(empty));
                }
    
            }
            normalDoc = wrapperBuilder.getResult();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw new WorkflowException("FailToCreateWrapperBuilder", I18n.loc(
                    "WLM-6068: Failed to create wrapper builder {0}", e.getMessage()), e);
    
        }
        return normalDoc.getDocumentElement();
    }
    
    
    public static boolean isElement (Part part) {
        if (part.getElementName() != null) {
            return true;
        }
        return false;
    } 

}
