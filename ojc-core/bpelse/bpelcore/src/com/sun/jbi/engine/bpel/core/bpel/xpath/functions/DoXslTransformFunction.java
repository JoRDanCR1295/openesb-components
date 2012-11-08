/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
 /*
  * $Id: DoXslTransformFunction.java,v 1.11 2010/02/04 02:51:51 fkieviet Exp $
  *
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import com.sun.bpel.model.util.Utility;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine.TransformEngine;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELXPathContext;
import com.sun.jbi.engine.bpel.core.bpel.util.XslCache;
import com.sun.transform.api.Xslt2Support.CompiledXslt2;
import com.sun.transform.api.Xslt2Support.Xslt2;
import java.io.StringWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.DocumentFragment;

/**
 * Implementation of BPEL's doXslTransform function.
 * 
 * @author Kevan Simpson
 */
public class DoXslTransformFunction implements Function {
    // for Xslt 1.0
    private static DocumentBuilder mBuilder = null;
    
    private String mSrvcUnitName;
    private XslCache mXslCache;
    
    static {
        try {
            DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
            fac.setNamespaceAware(true);
            fac.setValidating(false);
            mBuilder = fac.newDocumentBuilder();
        }
        catch (ParserConfigurationException pce) {
            
        }
    }
    
    public DoXslTransformFunction(String suName, XslCache xslCache){
        mSrvcUnitName = suName;
        mXslCache = xslCache;
    }
    
    /** @see org.apache.commons.jxpath.Function#invoke(org.apache.commons.jxpath.ExpressionContext, java.lang.Object[]) */
    public Object invoke(ExpressionContext context, Object[] parameters) {
        if (!(context.getJXPathContext() instanceof BPELXPathContext)) {
            return null;
        }
        
        BPELXPathContext ctxXPath = (BPELXPathContext) context.getJXPathContext();
        TransformEngine version = ctxXPath.getTransformEngine();
        String stylesheet = validateStylesheet(context, parameters);
        
        //set thread context class loader to SU
        ctxXPath.getClassLoaderContext().switchClassLoader(mSrvcUnitName,
                SwitchType.service_classloader);

        try {
            return (version == TransformEngine.XSLT_2_0) 
                    ? invokeXslt2(stylesheet, parameters)
                    : invokeXslt1(parameters, stylesheet);
        } 
        catch (StandardException e) {
        	// if it is StandardException propagate it
        	throw e;
        } 
        catch (Exception e) {
			// Convert to a runtime exception and propagate it. It will treated
			// as a system exception.
			throw new BPELException(e);
		} 
        finally {
            ctxXPath.getClassLoaderContext().switchClassLoader(mSrvcUnitName,
                    SwitchType.context_classloader);
        }
    }

    private Object invokeXslt1(Object[] parameters, String stylesheet)
            throws TransformerException {
        Transformer xsl =
                ((Templates) mXslCache.getStylesheet(stylesheet)).newTransformer();
        Source input = convertInput(parameters[1]);
        // set parameters
        int len = parameters.length;
        if (len > 2) {
            for (int i = 2; i < len; i += 2) {
                xsl.setParameter(String.valueOf(parameters[i]),
                        BPWSFunctions.convertParam(parameters[(i + 1)]));
            }
        }

        if ("xml".equals(xsl.getOutputProperty(OutputKeys.METHOD))) {
            DocumentFragment docFrag = newDocument().createDocumentFragment();
            DOMResult output = new DOMResult(docFrag);
            xsl.transform(input, output);
            return docFrag.getFirstChild();
        } else {
            StringWriter writer = new StringWriter();
            Result output = new StreamResult(writer);
            xsl.transform(input, output);
            return writer.toString();
        }
    }
    
    protected Object invokeXslt2(String stylesheet, Object[] parameters) throws Exception {
        Source payload = convertInput(parameters[1]);
        CompiledXslt2 comp = (CompiledXslt2) mXslCache.getStylesheet(stylesheet);
        // sets transform input as well...
        Xslt2 xsl = comp.newTransformer();

        // set parameters
        int len = parameters.length;
        if (len > 2) {
            for (int i = 2; i < len; i += 2) {
                xsl.setParam(String.valueOf(parameters[i]), 
                             BPWSFunctions.convertParam(parameters[(i + 1)]));
            }
        }

        return xsl.doTransform(payload);
    }
    
    private Source convertInput(Object src) {
        Object node = BPWSFunctions.convertParam(src);
        if (!(node instanceof Element)) {
        	throw new StandardException(StandardException.Fault.XsltInvalidSource);
        }
    	return new DOMSource((Node) node);
    }
    
    private String validateStylesheet(ExpressionContext context, Object[] args) {
        // should not need to validate args count, static analysis should have caught...
        if (args[0] == null || !(args[0] instanceof String) || ((String) args[0]).length() == 0) {
        	throw new StandardException(StandardException.Fault.XsltStylesheetNotFound);
        }
    	
        String uri = (String) args[0];
        try {
            // determine root path of deployed stylesheet
            String baseURI = null;
            if (context.getJXPathContext() instanceof BPELXPathContext) {
                baseURI = ((BPELXPathContext) context.getJXPathContext()).getBaseURI();
            }
            return Utility.parseDoXslTransform(uri, baseURI);
        }
        catch (Exception use) {
        	throw new StandardException(StandardException.Fault.XsltStylesheetNotFound, use);
        }
    }

    private Document newDocument() {
        synchronized (mBuilder) {
            return mBuilder.newDocument();
        }
    }
}
