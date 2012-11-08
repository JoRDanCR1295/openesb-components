/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.openesb.components.camelse.camel;

import java.io.Reader;
import java.io.StringReader;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.activation.DataHandler;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.apache.camel.Exchange;
import org.apache.camel.util.ExchangeHelper;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.apache.camel.impl.DefaultMessage;
import org.w3c.dom.Document;

/**
 *
 * @author chikkala
 */
public class JBIBridgeMessage extends DefaultMessage {
    private static final Logger LOG = Logger.getLogger(JBIBridgeMessage.class.getName());
    private NormalizedMessage jbiMessage;

    public JBIBridgeMessage() {
    }

    public JBIBridgeMessage(NormalizedMessage jbiMsg) {
        this.jbiMessage = jbiMsg;
    }

    @Override
    public String toString() {
        return "JBIBridgeMessage[" + jbiMessage + "]";
    }

    public NormalizedMessage getJBIMessage() {
        return jbiMessage;
    }

    public void setJBIMessage(NormalizedMessage jbiMsg) {
        this.jbiMessage = jbiMsg;
    }

    @Override
    public JBIBridgeExchange getExchange() {
        return (JBIBridgeExchange) super.getExchange();
    }

    @Override
    public JBIBridgeMessage newInstance() {
        return new JBIBridgeMessage();
    }

    @Override
    public void setHeader(String name, Object value) {
        super.setHeader(name, value);
        if ( this.jbiMessage != null ) {
            this.jbiMessage.setProperty(name, value);
        }
    }

    @Override
    public Object removeHeader(String name) {
        Object value = null;
        value = super.removeHeader(name);        
        if ( value != null && this.jbiMessage != null ) {
            Object jbiValue = this.jbiMessage.getProperty(name);
            if ( jbiValue != null ) {
                this.jbiMessage.setProperty(name, null);
            }
        }
        return value;
    }

    @Override
    public void setHeaders(Map<String, Object> headers) {
        super.setHeaders(headers);
        if ( this.jbiMessage != null ) {
            for (String name : headers.keySet() ) {
                this.jbiMessage.setProperty(name, headers.get(name));
            }
        }
    }
    
    @Override
    protected void populateInitialHeaders(Map<String, Object> map) {
        if (this.jbiMessage == null) {
            return;
        }
        Set<String> propNames = jbiMessage.getPropertyNames();
        for (String name : propNames) {
            Object obj = jbiMessage.getProperty(name);
            map.put(name, obj);
        }
    }

    @Override
    public void removeAttachment(String id) {
        super.removeAttachment(id);
        if ( this.jbiMessage != null ) {
            try {
                this.jbiMessage.removeAttachment(id);
            } catch (MessagingException ex) {
               LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

    @Override
    public void addAttachment(String id, DataHandler content) {
        super.addAttachment(id, content);
        if ( this.jbiMessage != null ) {
            try {
                this.jbiMessage.addAttachment(id, content);
            } catch (MessagingException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

    @Override
    public void setAttachments(Map<String, DataHandler> attachments) {
        super.setAttachments(attachments);
        if ( this.jbiMessage != null ) {
            for ( String id : attachments.keySet()) {
                try {
                    this.jbiMessage.addAttachment(id, attachments.get(id));
                } catch (MessagingException ex) {
                    Logger.getLogger(JBIBridgeMessage.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
    }

    @Override
    protected void populateInitialAttachments(Map<String, DataHandler> map) {
        if (this.jbiMessage == null) {
            return;
        }        
        Set<String> attchNames = jbiMessage.getAttachmentNames();
        for (String attchName : attchNames) {
            DataHandler attchObj = jbiMessage.getAttachment(attchName);
            map.put(attchName, attchObj);
        }        
    }

    @Override
    public Object getBody() {
        // this calls create body to get the body from jbiMessage if not null.
        Object obj = super.getBody(); 
        if ( obj == null || obj instanceof Source) {
            return obj;
        }
        // convert the obj to Source object
        
        if (obj != null) {
            if ( obj instanceof String ) {
                obj = RuntimeHelper.createDOMSource(new StringReader((String)obj));
            } else {
                Exchange exchange = this.getExchange();
                if ( exchange != null ) {
                    obj = ExchangeHelper.convertToType(exchange, Reader.class, obj);
                    obj = RuntimeHelper.createDOMSource((Reader)obj);
                }
            }
        }
        return obj;
    }

    @Override
    public void setBody(Object body) {
        try {
            if (jbiMessage != null) {
                if ( body == null ) {
                    jbiMessage.setContent(null);
                    return;
                }                
                if (body instanceof Source) {
                    jbiMessage.setContent(RuntimeHelper.sourceToDOMSource((Source) body));
                } else if (body instanceof Document ) {
                    jbiMessage.setContent(new DOMSource((Document)body));
                } else {
                    StringReader reader = new StringReader(body.toString());
                    jbiMessage.setContent(RuntimeHelper.createDOMSource(reader));
                }
            } else {
                super.setBody(body);
            }
        } catch (Exception ex) {
            RuntimeHelper.logError(ex);
            super.setBody(body);
        }
    }

    @Override
    protected Object createBody() {
        if (jbiMessage != null) {
            Source content = jbiMessage.getContent();
            if ( content != null ) {
                return RuntimeHelper.sourceToDOMSource(content);
            } else {
                return null;
            }
        } else {
            return super.createBody();
        }
    }
}
