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
 * @(#)StringDataSourceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.internationalization.Messages;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;

import javax.activation.DataSource;
import javax.xml.namespace.QName;

/**
 * DataSource implementation for Strings.
 */
class StringDataSourceImpl implements DataSource {
    private static Messages mMessages = Messages.getMessages(StringDataSourceImpl.class);
    private final String mData;
    private final QName mName;
    
    public StringDataSourceImpl(QName name, String data) {
        mData = data;
        mName = name;
    }
        
    public String getContentType() {
        return "text/plain";
    }
    
    public InputStream getInputStream() throws UnsupportedEncodingException {
        return new ByteArrayInputStream(mData.getBytes("UTF-8"));
    }
    
    public String getName() {
        return mName.toString();
    }
    
    public OutputStream getOutputStream() throws IOException {
        throw new IOException("This DataSource implementation is read-only; output stream not supported.");
    }
}
