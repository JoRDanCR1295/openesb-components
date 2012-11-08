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
 */

/*
 * @(#)MQBCOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.mqbc.extservices.QueueAccessOptions;

/**
 *
 */
public final class MQBCOperation
        implements ExtensibilityElement, Serializable, Deflatable {

    private static final long serialVersionUID = 1L;

    public static final String ATTR_QUEUE_NAME = "queueName";
    public static final String ATTR_QUEUE_OPEN_OPTIONS = "queueOpenOptions";
    public static final String ATTR_TRANSACTION = "transaction";
    public static final String ATTR_POLLINGINTERVAL = "pollingInterval";
   
    QName fieldElementType = MQConstants.QNAME_OPERATION;
    private MQInput input;
    private MQOutput output;
    private MQFault fault;

    private boolean fieldRequired = false;
    private String queueName = "";
    private int queueOpenOptions;
    private boolean isTransacted;
    private long pollingInterval = 1000L;
    
    
    /** Creates a new instance of MQOperation */
    public MQBCOperation()  {
    }
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        // NO-OP
    }

    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = clean(required);
    }
    
     public String getQueueName() {
        return clean(queueName);
    }

    public void setQueueName(String val) {
        queueName = clean(val);
    }
    
     public boolean getTransaction() {
        return clean(isTransacted);
    }

    public void setTransaction(boolean val) {
        isTransacted = clean(val);
    }
    
    public int getQueueOpenOptions() {
        return clean(queueOpenOptions);
    }

    public void setQueueOpenOptions(int val) {
        queueOpenOptions = clean(val);
    }
    
     public long getPollingInterval() {
        return clean(pollingInterval);
    }

    public void setPollingInterval(long val) {
        pollingInterval = clean(val);
    }
    
    /**
     * Set the Input element properties for current MQ BC Operation
     */
    public void setMQOperationInput(MQInput input) {
        this.input = input;
    }
    
    /**
     * Get the Input element properties for current MQ BC Operation
     */
    public MQInput getMQOperationInput() {
        return input;
    }

    /**
     * Set the Output element properties for current MQ BC Operation
     */
    public void setMQOperationOutput(MQOutput output) {
        this.output = output;
    }
    
    /**
     * Get the Input element properties for current MQ BC Operation
     */
    public MQOutput getMQOperationOutput() {
        return output;
    }

    public void setMQOperationFault(MQFault fault) {
        this.fault = fault;
    }
    
    public MQFault getMQOperationFault() {
        return fault;
    }
    /**
     * Serialize to the PrintWriter.
     *
     * @param pw The serialization outlet.
     * @param context User-defined context.
     *
     * @throws com.sun.jbi.mqbc.extensions.Deflatable.DeflateException if any
     * problems occur during the serialization process.
     */
    public void deflate(PrintWriter pw, Context context)
            throws DeflateException {
        
        Object namespace = context.getContext("namespace");
        Object prefix = context.getContext("prefix");
        
        QueueAccessOptions options = new QueueAccessOptions(queueOpenOptions);
        String optionsString = null;
        try {
            optionsString = options.toOptionString();
        } catch (IOException e) {
            throw new Deflatable.DeflateException(e);
        }
        
        String isTransactedString =
                (isTransacted ? "XATransaction" : "NoTransaction");

        final boolean printNamespace = (namespace != null);
        
        pw.print("<");
        pw.print(prefix.toString());
        pw.print(":operation ");
        
        if (printNamespace) {
            pw.print("xml:");
            pw.print(prefix.toString());
            pw.print("=");
            pw.print("\"");
            pw.print(namespace.toString());
            pw.print("\" ");
        }

        if (fieldRequired) {
            Object defContext = context.getContext(Definition.class);
            if (defContext != null) {
                Definition def = (Definition) defContext;
                try {
                    DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        String.valueOf(fieldRequired),
                        def,
                        pw);
                } catch (WSDLException e) {
                    throw new Deflatable.DeflateException(e);
                }
            }
        }

        pw.print(ATTR_QUEUE_NAME + "=" + "\"" + queueName + "\"");
        pw.print(ATTR_QUEUE_OPEN_OPTIONS + "=" + "\"" + optionsString + "\"");
        pw.print(ATTR_TRANSACTION + "=" + "\"" + isTransactedString + "\"");
        pw.print(ATTR_POLLINGINTERVAL + "=" + "\"" + pollingInterval + "\"");
        
        pw.println("/>");
    }

    private String clean(String value) {
        if (value == null) {
            value = "";
        }
        return value.trim();
    }
    
    private Boolean clean(Boolean value) {
        return (value == null ? false : value);
    }
    
    private Long clean(Long value) {
        return (value == null ? 0L : value);
    }
    
    private Integer clean(Integer value) {
        return (value == null ? 0 : value);
    }
}
