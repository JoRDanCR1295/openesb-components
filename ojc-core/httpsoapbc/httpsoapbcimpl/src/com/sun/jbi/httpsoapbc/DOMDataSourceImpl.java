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
 * @(#)DOMDataSourceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

import javax.activation.DataSource;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;


/**
 * DataSource implementation for XML Documents.
 * 
 */
class DOMDataSourceImpl implements DataSource {
    private static Messages cMessages = Messages.getMessages(DOMDataSourceImpl.class);
    private final TransformerFactory mTransformerFactory = TransformerFactory.newInstance();
    private final Document mDocument;
    private final QName mName;
    
    public DOMDataSourceImpl(QName name, Document data) {
        if (data == null) {
            throw new NullPointerException("data");
        }
        if (name == null) {
            throw new NullPointerException("name");
        }
        mDocument = data;
        mName = name;
    }
        
    public String getContentType() {
        return "text/xml";
    }
    
    public InputStream getInputStream() throws UnsupportedEncodingException {
        ByteArrayInputStream inStream = null;
        try {
            ByteArrayOutputStream outStream = new ByteArrayOutputStream();
            Transformer transformer;
            Source source = new DOMSource(mDocument);
            Result result = new StreamResult(outStream);

            transformer = mTransformerFactory.newTransformer();
            transformer.transform(source, result);
            
            inStream = new ByteArrayInputStream(outStream.toByteArray());
        } catch (Exception e) {
            throw new RuntimeException(e.getLocalizedMessage());
        }
        return inStream;
    }
    
    public String getName() {
        return mName.toString();
    }
    
    public OutputStream getOutputStream() throws IOException {
        throw new IOException("This DataSource implementation is read-only; output stream not supported.");
    }
}
