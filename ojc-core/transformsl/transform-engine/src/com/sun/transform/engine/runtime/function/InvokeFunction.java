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
 * @(#)InvokeFunction.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime.function;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.wsdl.Operation;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.WrapperUtil;
import com.sun.jbi.common.util.Util;
import com.sun.transform.I18n;
import com.sun.transform.engine.model.Invocation;
import com.sun.transform.engine.runtime.ProcessInstance;
import com.sun.transform.engine.runtime.WSMessage;
import com.sun.transform.engine.runtime.impl.Invoker;

/**
 * A utility providing extension functions to invoke web services and access 
 * {@link javax.jbi.messaging.NormalizedMessage NormalizedMessage} properties
 * from within a stylesheet.
 * <p>
 * More information can be found at the following wiki pages:
 * <ul>
 *      <li><a href="http://wiki.open-esb.java.net/Wiki.jsp?page=TransformSL.InvokeExtFxn">
 *          invoke() wiki</a></li>
 *      <li><a href="http://wiki.open-esb.java.net/Wiki.jsp?page=TransformSL.NMPropertyExtFxn">
 *          getNMProperty() + setNMProperty() wiki</a></li>
 * </ul>
 *  
 * @author Kevan Simpson
 */
public class InvokeFunction {
    private static final String TRUE = Boolean.TRUE.toString();
    private static final String FALSE = Boolean.FALSE.toString();
    private static Logger mLogger = Logger.getLogger(InvokeFunction.class.getName());

    
    /**
     * Fetches an NM property from the inbound exchange's NM.
     * @param obj A object representing the context of XsltSE, set on the
     *            stylesheet as a parameter named &quot;xsltse&quot;.
     * @param property The name of the property to fetch.
     * @return The property value or an empty string, if the NM cannot be resolved.
     * @throws Exception if an error occurs accessing NM properties.
     */
    public static String getNMProperty(Object obj, String property) throws Exception {
        return updateNMProperty(obj, property, null, null, true);
    }

    /**
     * Fetches an NM property using the specified variable to resolve the NM.
     * @param obj A object representing the context of XsltSE, set on the
     *            stylesheet as a parameter named &quot;xsltse&quot;.
     * @param property The name of the property to fetch.
     * @param variable The inputVariable of a transformmap-configured activity.
     * @return The property value or an empty string, if the NM cannot be resolved.
     * @throws Exception if an error occurs accessing NM properties.
     */
    public static String getNMProperty(Object obj, String property, String variable) throws Exception {
        return updateNMProperty(obj, property, null, variable, true);
    }

    /**
     * Sets an NM property on the inbound exchange's NM.
     * @param obj A object representing the context of XsltSE, set on the
     *            stylesheet as a parameter named &quot;xsltse&quot;.
     * @param property The name of the property to set.
     * @param value The value to set.
     * @return If the NM is resolved and the property set, returns <code>true</code>, otherwise <code>false</code>.
     * @throws Exception if an error occurs accessing NM properties.
     */
    public static boolean setNMProperty(Object obj, String property, String value) throws Exception {
        return Boolean.valueOf(updateNMProperty(obj, property, value, null, false)).booleanValue();
    }

    /**
     * Sets an NM property using the specified variable to resolve the NM.
     * @param obj A object representing the context of XsltSE, set on the
     *            stylesheet as a parameter named &quot;xsltse&quot;.
     * @param property The name of the property to set.
     * @param value The value to set.
     * @param variable The inputVariable of a transformmap-configured activity.
     * @return If the NM is resolved and the property set, returns <code>true</code>, otherwise <code>false</code>.
     * @throws Exception if an error occurs accessing NM properties.
     */
    public static boolean setNMProperty(Object obj, String property, String value, String variable) throws Exception {
        return Boolean.valueOf(updateNMProperty(obj, property, value, variable, false)).booleanValue();
    }
    
    private static String updateNMProperty(Object obj, String property, String value,
                                           String variable, boolean get) throws Exception {
        try {
            if (Util.isEmpty(property)) {
                return get ? "" : FALSE;  // don't need the NM for an empty/null property key
            }
            else if (obj == null) {
                throw new IllegalArgumentException(I18n.loc(
                        "TRANSL-6023: Illegal arguments passed to InvokeFunction.{0}(Object, String{1}): {2}, {3}, {4}",
                        get ? "getNMProperty" : "setNMProperty", 
                        get ? "[, String]" : ", String[, String]",
                        String.valueOf(obj), property, String.valueOf(variable)));
            }
            else if (!(obj instanceof Invoker)) {
                throw new IllegalArgumentException(I18n.loc(
                        "TRANSL-6024: Engine context parameter is the wrong type: {0}",
                        obj.getClass().getName()));
            }
            
            Invoker invoker = (Invoker) obj;
            WSMessage wsm = findMessage(invoker, variable);
            if (wsm != null) {
                if (get) {
                    return wsm.getProperty(property);
                }
                else {
                    wsm.setProperty(property, value);
                    return TRUE;
                }
            }
            else if (!get && !Util.isEmpty(variable)) {
                // no WSM only means variable not registered yet
                // so we'll store the properties if variable is specified
                invoker.getProcess().getVariableContext()
                        .storeMessageProperty(variable, property, value);
                return TRUE;
            }
            else {
                return get ? "" : FALSE;
            }
        }
        catch (Exception e) {
            String err = I18n.loc(
                    "TRANSL-6048: InvokeFunction.{0} failed: {1}", 
                    get ? "getNMProperty" : "setNMProperty", e.getMessage());
            log().log(Level.SEVERE, err, e);
            throw new Exception(err, e);
        }
    }

    private static WSMessage findMessage(Invoker invoker, String variable) {
        ProcessInstance proc = invoker.getProcess();
        // assume enclosing process 'inputVariable' if no arg is passed
        String var = (Util.isEmpty(variable)) 
                ? proc.getProcessDef().getInvocation().getInputVariable()
                : variable;
        
        return (WSMessage) proc.getVariableContext().getVariable(var);
    }
    
    /**
     * Invokes a web service with one message part in the request and response. 
     * <p>
     * {@link InOnly} invocations return the {@link ExchangeStatus} as a 
     * {@link Text} node while {@link InOut} invocations return the response 
     * as an {@link Element}.
     * 
     * @param obj A object representing the context of XsltSE, set on the
     *            stylesheet as a parameter named &quot;xsltse&quot;.
     * @param invokeName The name of the invocation configuration in transformmap.
     * @param input The input to the invoke.
     * @return an {@link Element} or {@link Text} node.
     * @throws IllegalArgumentException if passed parameters are <code>null</code> or invalid.
     * @throws RuntimeException if the configuration for <code>invokeName</code> is missing.
     * @throws MessagingException if the invocation fails during send.
     */
    public static Node invoke(Object obj, String invokeName, Node input) throws Exception {
        try {
            if (obj == null || Util.isEmpty(invokeName) || input == null) {
                throw new IllegalArgumentException(I18n.loc(
                        "TRANSL-6023: Illegal arguments passed to InvokeFunction.{0}(Object, String{1}): {2}, {3}, {4}",
                        "invoke", ", Node",
                        String.valueOf(obj), invokeName, String.valueOf(input)));
            }
            else if (!(obj instanceof Invoker)) {
                throw new IllegalArgumentException(I18n.loc(
                        "TRANSL-6024: Engine context parameter is the wrong type: {0}",
                        obj.getClass().getName()));
            }
            
            Invoker invoker = (Invoker) obj;
            Invocation invoke = invoker.lookupInvoke(invokeName);
            if (invoke == null) {
                throw new RuntimeException(I18n.loc(
                        "TRANSL-6025: Missing invoke configuration: {0}", invokeName));
            }

            EndpointInfo info = invoke.getInfo();
            Operation op = invoke.getOperation();
            ProcessInstance proc = invoker.getProcess();
            MessageExchange mex = invoker.createExchange(info, op),
                            consumer = proc.getMessageExchange();
            // convert input node(s)
            DOMSource src = new DOMSource(input);
            invoker.registerInvocationVariables(proc.getVariableContext(), invoke, src);
            WSMessage wsm = (WSMessage) 
                    proc.getVariableContext().getVariable(invoke.getInputVariable());
            invoker.setInput(mex, wsm);
            // systemics
            invoker.setUniqueId(mex, invokeName, consumer);
            invoker.propagateSystemics(consumer, mex);
            
            if (invoker.sendSync(mex)) {
                // TODO log response
                if (mex instanceof InOut) {
                    InOut inOut = (InOut) mex;
                    // complete the exchange
                    invoker.completeInOut(invoker.getProcess(), inOut, invoke);
                    // extract jbi:part elements
                    Element[] elems = extractData(inOut, false);
                    if (elems.length == 1) {
                        return elems[0];
                    }
                    else {
                        // TODO log lack of multi-part message support
                    }
                }
                else {  // InOnly, other patterns already eliminated
                    ExchangeStatus status = invoker.completeInOnly(
                            invoker.getProcess(), (InOnly) mex, invoke);
                    return convertStatus(input.getOwnerDocument(), status);
                }
            }
            else {
                throw new MessagingException(I18n.loc(
                        "TRANSL-6056: Send failed for {0}, exchange id: {1}", 
                        invokeName, mex.getExchangeId()));
            }
            
            return convertStatus(input.getOwnerDocument(), 
                                 ExchangeStatus.ERROR);
        }
        catch (Exception e) {
            String err = I18n.loc(
                    "TRANSL-6048: InvokeFunction.{0} failed: {1}", "invoke", e.getMessage());
            log().log(Level.SEVERE, err, e);
            throw new Exception(err, e);
        }
    }
    
    private static Element[] extractData(MessageExchange msg, boolean inMsgFlag) 
            throws Exception {
        Element[] parts = ExchangeUtil.extractParts(msg, inMsgFlag);
        int len = parts.length;
        // strip content from jbi:parts
        Element[] elems = new Element[len];
        for (int i = 0; i < len; i++) {
            elems[i] = WrapperUtil.unwrapPartElement(parts[i]);
        }

        return elems;
    }
    private static Text convertStatus(Document doc, ExchangeStatus status) {
        return doc.createTextNode(String.valueOf(status));
    }
    
    private static Logger log() {
        return mLogger;
    }
}
