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
 * @(#)DocumentBuilderPool.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */


package org.glassfish.openesb.pojose.core.pool;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;
import org.glassfish.openesb.pojose.core.util.Util;

/**
 *
 * @author gpatil
 */
public class DocumentBuilderPool extends AbstractPool{
    private static volatile DocumentBuilderPool instance = null;
    
    private DocumentBuilderPool(){
        super();
    }
    
    @Override
    protected DocumentBuilder create() {
        try {
            DocumentBuilderFactory docFact = DocumentBuilderFactory.newInstance();
            docFact.setNamespaceAware(true);
            // See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4915524
            // Does not seem to fix for node created by Http BC/JAX-WS
            // com.sun.xml.messaging.saaj.soap.SOAPDocumentImpl vs com.sun.org.apache.xerces.internal.dom.DocumentImpl
            // docFact.setAttribute("http://apache.org/xml/features/dom/defer-node-expansion", Boolean.FALSE);
            DocumentBuilder db = docFact.newDocumentBuilder();
            return db;
        } catch (ParserConfigurationException ex) {
            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    public synchronized void release(DocumentBuilder o) {
        if (o != null){
            ((DocumentBuilder)o).reset();
            super.release((Object) o);
        }
    }
    
    @Override
    public DocumentBuilder acquire(){
        return (DocumentBuilder) super.acquire();
    }
    
    public synchronized static DocumentBuilderPool getInstance(){
        if (instance == null){
            instance = new DocumentBuilderPool();
        }
        return instance;
    }
}
