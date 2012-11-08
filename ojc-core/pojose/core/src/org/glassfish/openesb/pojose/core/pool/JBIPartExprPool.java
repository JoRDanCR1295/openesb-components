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
 * @(#)JBIPartExprPool.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */


package org.glassfish.openesb.pojose.core.pool;

import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.XMLConstants;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.glassfish.openesb.pojose.core.util.Constants;
import org.glassfish.openesb.pojose.core.util.Util;

/**
 *
 * @author gpatil
 */
public class JBIPartExprPool extends AbstractPool{
    private static volatile JBIPartExprPool instance = null;
    
    private JBIPartExprPool(){
        super();
    }
    
    @Override
    protected XPathExpression create() {
        try {
            XPathFactory xpFactory = XPathFactory.newInstance();
            XPath xpath = xpFactory.newXPath();
            xpath.setNamespaceContext(new MyNamespaceContext());
            XPathExpression expr = xpath.compile("//jbi:message/jbi:part"); //NOI18N
            return expr;
        } catch (XPathExpressionException ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        } 
        return null;
    }

    public synchronized void release(XPathExpression o) {
        if (o != null){
            //((XPathExpression)o).reset();
            super.release((Object) o);
        }
    }
    
    @Override
    public XPathExpression acquire(){
        return (XPathExpression) super.acquire();
    }
    
    public synchronized static JBIPartExprPool getInstance(){
        if (instance == null){
            instance = new JBIPartExprPool();
        }
        return instance;
    }
    
    static class MyNamespaceContext implements NamespaceContext {

        public String getNamespaceURI(String prefix) {
            if (prefix == null) {
                throw new NullPointerException("Null prefix");//NOI18N
            } else if ("jbi".equals(prefix)) {
                return Constants.JBI_WRAPPER_NS; //NOI18N
            }
            return XMLConstants.NULL_NS_URI;
        }

        // This method isn't necessary for XPath processing.
        public String getPrefix(String uri) {
            throw new UnsupportedOperationException();
        }

        // This method isn't necessary for XPath processing either.
        public Iterator getPrefixes(String uri) {
            throw new UnsupportedOperationException();
        }
    }    
}
