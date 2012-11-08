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
 * @(#)FromEvaluatorFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathException;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.bpel.model.Copy;
import com.sun.bpel.model.From;
import com.sun.bpel.model.Literal;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.To;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;

/**
 *
 *
 *
 * @author Sun Microsystems
 */
public class FromEvaluatorFactory {
    private static final Logger LOGGER = Logger.getLogger(Utility.class.getName());

	/*
	 * private constructor
	 */
	private FromEvaluatorFactory(){};

	/**
	 *
	 * @param from
	 * @return
	 */
	public static FromEvaluator getFromEvaluator(From from) {

		if (from.getBPELPartnerLink() != null) {
            return new FromPartnerLinkEvaluator();
        } else if (from.getLiteral() != null) {
            // <from><literal>literal value</literal></from>
            return new FromLiteralEvaluator();
        } else if (from.getBPELVariable() != null) {
//          <from variable="NCName"/>
//          <from variable="NCName" part="NCName"?/>
        	// <from variable="NCName" property="QName"?/>
            return new FromVariableEvaluator();
        } else {
            /*
             * <from expressionLanguage="anyURI"?>expression</from>
             * For example,
             * <from expressionLanguage="anyURI"?>$var.part</from>
             * <from expressionLanguage="anyURI"?>$var.part/query</from>
             * <from expressionLanguage="anyURI"?>$var.part/step1/step2[$var2]</from>
             * <from expressionLanguage="anyURI"?>concat($var.part/query, 'some string')</from>
             */
            return new FromExpressionEvaluator();
        }
	}


	/**
	 *
	 * @author Sun Microsystems
	 */
    public interface FromEvaluator {
        /**
         * @param from
         * @param ctx
         * @param to We need the To only for the partner link assignments. We need to determine
         * if we want to query the JBI framework for the Service EndPoint or use the internal
         * structure of the RuntimePartnerLink.
         * @return
         * @throws Exception
         */
        Object evaluateFrom(From from, Context ctx, To to) throws Exception;
    }

    /**
     *
     * @author Sun Microsystems
     */
    private static class FromVariableEvaluator implements FromEvaluator {

        public Object evaluateFrom(From from, Context ctx, To to) throws Exception {
            VariableScope variableScope = (VariableScope) ctx;

            RVariable variable = ((RVariableElement) from).getRVariable();
            RuntimeVariable varData = variableScope.getRuntimeVariable(variable);

            if (varData == null) {
                throw Utility.uninitializedVariableError(variable.getName());
            }

            // <from variable="NCName" property="QName"/>
            if (!Utility.isEmpty(from.getProperty())) {
                String propName = from.getProperty();
                return Utility.evaluateProperty(from, variable.getName(), propName, varData);
            }

            if (varData.getVariableDef().getMessageType() == null) {
                // <from variable="NCName"/>
                // the variable is of xsd type
                if (varData.getXSDVariableData() == null) {
                    throw Utility.uninitializedVariableError(variable.getName());
                }
                return varData.getXSDVariableData();
            }
            WSMessage message = varData.getWSMessage();
            if (message == null) {
                throw Utility.uninitializedVariableError(varData.getVariableDef().getName());
            }
            // <from variable="NCName" nmProperty="normalizedMsgProperty"?/>
            if (!Utility.isEmpty(from.getNMProperty())) {
                String nmProperty = from.getNMProperty();

                WSMessage wsMessage = varData.getWSMessage();
                return wsMessage.getNMProperty(nmProperty);
            }
            // <from variable="NCName" part="NCName"?/>
            String partName = from.getPart();
            if (!Utility.isEmpty(partName)) {
                if (message.getPart(partName) == null) {
                    Utility.uninitializedVariableError(variable.getName(), partName);
                }
                Element partVal = message.getPart(partName);
                // check to see if we need to transfer an attachment

                // First check if there is an attachment and then see if this part requires an attachment copy.
                // This incurs less expense than checking the part directly.
                if (message.getAttachments().size() != 0) {
                    String attrBinaryCopy = ((Copy)from.getParent()).getBinaryCopy();
                    NodeList ns = partVal.getElementsByTagNameNS("http://www.w3.org/2004/08/xop/include", "Include");
                    if (ns != null && ns.getLength() != 0) {
                        // There can be only one xop:Include element under the part
                        Element xopElement = (Element) ns.item(0);
                        if (ns.getLength() > 1) {
                            // TODO should we be strict and throw exception or pick the first xop:Include tag??
                            // throw new RuntimeException("Unsupported feature: expects only one xop:Include tag");
                        }
                        String hRefVal = xopElement.getAttribute("href");
                        DataHandler dHdlr = message.getAttachment(hRefVal);
                        if (dHdlr == null) {
                            String errMsg = I18n.loc(
                                    "BPCOR-6178: attempt to access the value of an uninitialized variable part \"{0}.{1}\"",
                                    variable.getName(), partName);
                            errMsg += I18n.loc("BPCOR-7133: Missing attachment referred by {0}", hRefVal);
                            throw new StandardException(StandardException.Fault.UninitializedVariable, errMsg);

                        }
                        Utility.AttachmentWrapper attachWrap = new Utility.AttachmentWrapper();
                        attachWrap.mDHdlr = dHdlr;
                        attachWrap.mXopElement = xopElement;
                        attachWrap.mAttchName = hRefVal;
                        attachWrap.mBinaryCopy = (attrBinaryCopy != null)? attrBinaryCopy : Copy.BINARY_COPY_ENUM_VALS[1];
                        return attachWrap;
                    }
                } else {
                    return message.getPart(partName);
                }

            }
            // <from variable="NCName"/> - only variable is specified
            return message;

        }
    }

    /**
     *
     * @author Sun Microsystems
     */
    private static class FromExpressionEvaluator implements FromEvaluator {

        public Object evaluateFrom(From from, Context ctx, To to) throws Exception {
            JXPathContext jxpathContext = Utility.createJXPathContext(from, ctx, null, null);
            try {
                return jxpathContext.iteratePointers(from.getExpression());
            } catch (JXPathException ex) {
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6115: Exception while evaluating the xpath {0}. " +
                        "Associated BPEL artifact is: {1}", ex.getMessage(), from));
                throw ex;
            }
        }
    }

    /**
     *
     *
     *
     * @author Sun Microsystems
     */
    private static class FromLiteralEvaluator implements FromEvaluator {

        public Object evaluateFrom(From from, Context ctx, To to) throws Exception {
            Literal lit = from.getLiteral();
            if (lit.getEII() != null) {
                // There shouldn't be a need for copying the EII.

                // update value in case it contains application variables
                Element litEII = RApplicationVariablesHelper.replaceAppVar2Value(lit.getEII(),
                        ctx.getProcessInstance().getBPELProcessManager().getApplicationVariables());
                
                return litEII;
            } else {
            	String value = lit.getValue();

                // update value in case it contains application variables
                value = RApplicationVariablesHelper.replaceAppVar2Value(value,
                        ctx.getProcessInstance().getBPELProcessManager().getApplicationVariables());

                QName qname = Utility.getQNamefromBPELElement(lit, value);
                return (qname == null) ? value : qname;
            }
        }
    }

    /**
     *
     *
     *
     * @author Sun Microsystems
     */
    private static class FromPartnerLinkEvaluator implements FromEvaluator {

        public Object evaluateFrom(From from, Context ctx, To to) throws Exception {
            PartnerLinkScope partnerLinkScope = (PartnerLinkScope) ctx;
            PartnerLink fromPLink = from.getBPELPartnerLink();
            String roleVal = from.getEndPointReference();

            if (roleVal == null) {
                // roleValue is required attribute
                return null;
            }
            Object fromEPR;

            BPELProcessManager procMgr = ctx.getProcessInstance().getBPELProcessManager();
            fromEPR = procMgr.getEndPointReference(partnerLinkScope, fromPLink, roleVal.equals(PartnerLink.ATTR.MY_ROLE));

            return fromEPR;
        }
    }
}
