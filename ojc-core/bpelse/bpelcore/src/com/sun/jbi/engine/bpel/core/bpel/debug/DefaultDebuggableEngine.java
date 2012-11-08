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
 * @(#)DefaultDebuggableEngine.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.wsdl.Message;
import javax.wsdl.Part;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathContextFactory;
import org.apache.commons.jxpath.Pointer;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELPartnerLink;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELVariable;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebuggableEngine;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.SchemaViolationException;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.XpathExpressionException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMFactory;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * Implementation of DebuggableEngine
 * 
 * @author Sun Microsystems
 * @version 
 * 
 */
public class DefaultDebuggableEngine implements DebuggableEngine {
    private ActivityUnitImpl act;

    private BPELElement el;

    private static final JXPathContextFactory JXPATH_FACTORY = JXPathContextFactory.newInstance();

    /**
     * Creates a new instance
     */
    public DefaultDebuggableEngine(ActivityUnit pc) {
        act = (ActivityUnitImpl) pc;
        el = act.getStaticModelActivity();
    }

    public String[] getVariables() {
        // TODO Auto-generated method stub
        Context context = act.getContext();
        Set runtimeVars = context.getRuntimeVariables().keySet();
        ArrayList contsArray = new ArrayList(runtimeVars.size());

        for (Iterator it = runtimeVars.iterator(); it.hasNext();) {
            RVariable rv = (RVariable) it.next();
            contsArray.add(rv.getName());
        }
        String[] result = new String[contsArray.size()];
        result = (String[]) contsArray.toArray(result);
        return result;
    }

    public String getContainerDataAsString(String containerName) {
        String result = "";
        Context context = act.getContext();
        RVariable variableModel = (RVariable) com.sun.bpel.model.BPELHelper.getMatchingVariable(
                containerName, el);

        // Not sure when variableModel will be null
        if (variableModel == null) {
            return result;
        }

        RuntimeVariable runtimeVariable = (RuntimeVariable) context
                .getRuntimeVariable(variableModel);
        // Uninitialized variable
        if (runtimeVariable == null) {
            return result;
        }

        if (variableModel.getWSDLMessageType() != null) {
            // WSDL Message
            WSMessage variableData = runtimeVariable.getWSMessage();
            if (variableData != null) {
                result = variableData.toString();
                // Should we return null and show it as a null
                // to the user?
                if (result == null) {
                    result = "";
                }
            }
        } else {
            // XSD Type

            Object variableData = runtimeVariable.getXSDVariableData();
            if (variableData != null) {
                if (variableData instanceof Element) {
                    result = DOMHelper.createXmlString((Element) variableData);
                } else if (variableData instanceof Document) {
                    result = DOMHelper.createXmlString(((Document) variableData)
                            .getDocumentElement());
                } else {
                    result = variableData.toString();
                }
            }
        }

        return result;
    }

    public BPELVariable getVariable(String variableName) {
        BPELVariable result = null;
        Context context = act.getContext();
        RVariable variableModel = (RVariable) com.sun.bpel.model.BPELHelper.getMatchingVariable(
                variableName, el);

        // Not sure when variableModel will be null
        if (variableModel == null) {
            return result;
        }

        RuntimeVariable runtimeVariable = (RuntimeVariable) context
                .getRuntimeVariable(variableModel);
        // Uninitialized variable
//        if (runtimeVariable == null) {
//            return result;
//        }
        result = new BPELVariableImpl(variableModel, runtimeVariable);
        return result;

    }
    
    public String[] getPartnerLinks() {
        final Context context = act.getContext();
        final Collection pLinks = context.getRuntimePartnerLinks().values();
        
        final String[] result = new String[pLinks.size()];
        int counter = 0;
        
        for (Iterator it = pLinks.iterator(); it.hasNext();) {
            final RuntimePartnerLink link = (RuntimePartnerLink) it.next();
            result[counter++] = link.getStaticModel().getName();
        }
        
        return result;
    }
    
    public BPELPartnerLink getPartnerLink(final String name) {
        final Context context = act.getContext();
        final Collection pLinks = context.getRuntimePartnerLinks().values();
        
        for (Iterator it = pLinks.iterator(); it.hasNext();) {
            final RuntimePartnerLink link = (RuntimePartnerLink) it.next();
            
            if (link.getStaticModel().getName().equals(name)) {
                return new BPELPartnerLinkImpl(link.getStaticModel(), link);
            }
        }
        
        return null;
    }
    
    public String evaluate(String xpathExpression) throws XpathExpressionException {
        Context context = act.getContext();
        String result = "";
        xpathExpression = Utility.convertXmlSpecialChars(xpathExpression);
        try {

            // TODO: The variable scope that needs to be passed here is the activity's
            // context -passing null for now
            // TODO: one of the paramters is a BPEL element, so we need to pass that instead
            // of null. The context should also be passed instaed of the resolver
            Object value = Utility.getXpathExpressionValue(xpathExpression, null, el, context);
            if (value instanceof Node) {
                Node srcNode = (Node) value;
                result = DOMHelper.createXmlString((Element) value);
            } else {
                return value.toString();
            }
        } catch (Exception e) {
            throw new XpathExpressionException(e.getMessage());
        }
        return result;
    }

    public void changeVariableMessageTypeValue(String variableName, String partName, String xpath,
            String value) throws XpathExpressionException, SchemaViolationException {
        if (value == null) {
            return;
        }
        Context context = act.getContext();
        RVariable variableModel = (RVariable) com.sun.bpel.model.BPELHelper.getMatchingVariable(
                variableName, el);

        if (variableModel == null) {
            return;
        }
        RuntimeVariable runtimeVariable = (RuntimeVariable) context
                .getRuntimeVariable(variableModel);

        // Uninitialized variable
        if (runtimeVariable == null) {
            return;
        }
        Message vMeg = variableModel.getWSDLMessageType();
        if (vMeg != null) {
            // WSDL Message
            Part partModel = vMeg.getPart(partName);
            WSMessage variableData = runtimeVariable.getWSMessage();
            if (variableData != null) {
                Element part = variableData.getPart(partName);
                if (part == null) {
                	part = variableData.createPart(partName);
                }
                Map<String, String> prefixMap = new HashMap<String, String>();
                getPrefixMap(part, prefixMap);
                setValue(part, value, xpath, prefixMap);
                context.setRuntimeVariable(runtimeVariable);
                StateContext stateCtx = context.getStateContext();
                context.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                        runtimeVariable, act.getBranchId());
            }
        } else {
            throw new SchemaViolationException(variableName + " is not a WSDLMessage variable");
        }
    }

    public void changeVariableMessageTypeValue(String variableName, String partName, String value)
            throws SchemaViolationException {
        if (value == null) {
            return;
        }
        Context context = act.getContext();
        RVariable variableModel = (RVariable) com.sun.bpel.model.BPELHelper.getMatchingVariable(
                variableName, el);

        if (variableModel == null) {
            return;
        }
        RuntimeVariable runtimeVariable = (RuntimeVariable) context
                .getRuntimeVariable(variableModel);

        // Uninitialized variable
        if (runtimeVariable == null) {
            return;
        }
        Message vMeg = variableModel.getWSDLMessageType();
        if (vMeg != null) {
            // WSDL Message
            Part partModel = vMeg.getPart(partName);
            WSMessage variableData = runtimeVariable.getWSMessage();
            if (variableData != null) {
                Element part = variableData.getPart(partName);
                if (part == null) {
                	part = variableData.createPart(partName);
                }
                Map<String, String> prefixMap = new HashMap<String, String>();
                try {
                    setValue(part, value, "/", prefixMap);
                } catch (XpathExpressionException e) {
                    // TODO Auto-generated catch block
                    throw new SchemaViolationException(variableName + " part:" + partName
                            + " is not a simple type");
                }
                context.setRuntimeVariable(runtimeVariable);
                StateContext stateCtx = context.getStateContext();
                context.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                        runtimeVariable, act.getBranchId());
            }
        } else {
            throw new SchemaViolationException(variableName + " is not a WSDLMessage variable");
        }
    }

    private void getPrefixMap(Element part, Map<String, String> prefixMap) {
        // TODO Auto-generated method stub
        prefixMap.put(part.getPrefix(), part.getNamespaceURI());
        NodeList children = part.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child instanceof Element) {
                getPrefixMap((Element) child, prefixMap);
            }
        }
    }

    public void changeVariableSchemaTypeValue(String variableName, String value)
            throws SchemaViolationException {
        if (value == null) {
            return;
        }
        Context context = act.getContext();
        RVariable variableModel = (RVariable) com.sun.bpel.model.BPELHelper.getMatchingVariable(
                variableName, el);
        if (variableModel == null) {
            return;
        }
        RuntimeVariable runtimeVariable = (RuntimeVariable) context
                .getRuntimeVariable(variableModel);

        // Uninitialized variable
        if (runtimeVariable == null) {
            return;
        }
        if (variableModel.getWSDLMessageType() != null) {
            throw new SchemaViolationException(variableName + "is not a XSD type variable");
        }
        Object variableData = runtimeVariable.getXSDVariableData();
        if (variableData != null) {
            if (variableModel.isBoolean()) {
                runtimeVariable.setXSDVariableData(new Boolean(value));
            } else if (variableModel.isString()) {
                runtimeVariable.setXSDVariableData(value);
            } else if (variableModel.isNumber()) {
                if (variableData instanceof BigDecimal) {
                    runtimeVariable.setXSDVariableData(new BigDecimal(value));
                } else if (variableData instanceof Byte) {
                    runtimeVariable.setXSDVariableData(new Byte(value));
                } else if (variableData instanceof Double) {
                    runtimeVariable.setXSDVariableData(new Double(value));
                } else if (variableData instanceof Float) {
                    runtimeVariable.setXSDVariableData(new Float(value));
                } else if (variableData instanceof Integer) {
                    runtimeVariable.setXSDVariableData(new Integer(value));
                } else if (variableData instanceof Long) {
                    runtimeVariable.setXSDVariableData(new Long(value));
                } else if (variableData instanceof Short) {
                    runtimeVariable.setXSDVariableData(new Short(value));
                } else {
                    throw new SchemaViolationException(variableName + "is not a Simple XSD type variable");
                }

            }
            context.setRuntimeVariable(runtimeVariable);
            StateContext stateCtx = context.getStateContext();
            context.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                    runtimeVariable, act.getBranchId());
        }
       
    }

    public void changeVariableSchemaTypeValue(String variableName, String xpath, String value)
            throws XpathExpressionException, SchemaViolationException {
        if (value == null) {
            return;
        }
        Context context = act.getContext();
        RVariable variableModel = (RVariable) com.sun.bpel.model.BPELHelper.getMatchingVariable(
                variableName, el);
        if (variableModel == null) {
            return;
        }
        RuntimeVariable runtimeVariable = (RuntimeVariable) context
                .getRuntimeVariable(variableModel);

        // Uninitialized variable
        if (runtimeVariable == null) {
            return;
        }
        if (variableModel.getWSDLMessageType() != null) {
            throw new SchemaViolationException(variableName + "is not a XSD type variable");
        }
        Object variableData = runtimeVariable.getXSDVariableData();
        if (variableData != null) {
            Element bean = null;
            if (variableData instanceof Document) {
                bean = ((Document) variableData).getDocumentElement();
            } else if (variableData instanceof Element) {
                bean = (Element) variableData;
            } else {
                throw new SchemaViolationException(variableName + "is not a complex XSD type variable");
            }
            Map<String, String> prefixMap = new HashMap<String, String>();
            getPrefixMap(bean, prefixMap);            
            setValue (bean,value,xpath, prefixMap);
            context.setRuntimeVariable(runtimeVariable);
            StateContext stateCtx = context.getStateContext();
            context.getProcessInstance().getPersistenctMgr().updateState(stateCtx, 
                    runtimeVariable, act.getBranchId());
        }
    }

    private void setValue(Object bean, String val, String xpath, Map<String, String> prefixMap)
            throws XpathExpressionException, SchemaViolationException {
        String simXpath = xpath;
        DOMFactory domFactory = new DOMFactory();
        JXPathContext ctx = newJXPathContext(bean);
        ctx.setFactory(domFactory);
        if (prefixMap.size() > 0) {
            Set<Map.Entry<String, String>> mappings = prefixMap.entrySet();
            for (Map.Entry<String, String> mapping : mappings) {
                ctx.registerNamespace(mapping.getKey(), mapping.getValue());
            }
        }
        Pointer pointer = ctx.getPointer(simXpath);
        Object node = pointer.getNode();
        if (node instanceof Element) {
            if (!isLeaf((Element) pointer.getNode())) {
                throw new SchemaViolationException("failed to set value--xpath  " + xpath
                        + " doesn't locate a text node or attribute node");
            }
        }
        try {
            ctx.setValue(simXpath, val);
        } catch (Exception e) {
            throw new XpathExpressionException(e.getMessage());
        }
    }

    private String formatXpath(String toQuery) {
        String targetQuery = toQuery;
        if (toQuery.endsWith("text()")) {
            int position = toQuery.indexOf("text()");
            targetQuery = toQuery.substring(0, position - 1);
        } else {
            char[] charArray = toQuery.toCharArray();
            if (charArray[charArray.length - 1] == ']') {
                int position = charArray.length - 2;
                while (charArray[position] != '[') {
                    position--;
                }
                targetQuery = toQuery.substring(0, position);
            }
        }
        return targetQuery;
    }

    private boolean isLeaf(Element node) {
        if (!node.hasChildNodes() && (!node.hasAttributes())) {
            return true;
        }
        if (node.hasChildNodes() && (node.getFirstChild() instanceof Text)) {
            return true;
        }
        return false;
    }

    /**
     * Create a new JXPathContext
     * 
     * @param bean
     *            Context value
     * 
     * @return The new context
     */
    private JXPathContext newJXPathContext(Object bean) {
        JXPathContext context = JXPATH_FACTORY.newContext(null, bean);
        context.setLenient(true);
        return context;
    }
}
