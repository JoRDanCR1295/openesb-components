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
 * @(#)$Id: MQBCRedelivery.java,v 1.1 2008/11/20 20:58:45 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

/**
 * Represents an MQ Redelivery element contained in a WSDL operation element.
 *
 * @author Noel.Ang@sun.com
 */
public class MQBCRedelivery
        implements ExtensibilityElement, Serializable, Deflatable {

    public MQBCRedelivery() {
    }

    public void setElementType(QName typeName) {
        if (typeName != null) {
            type = typeName;
        }
    }

    public QName getElementType() {
        return type;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public Boolean getRequired() {
        return required;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int retries) {
        count = Math.max(0, retries);
    }

    public int getDelay() {
        return delay;
    }

    public void setDelay(int milli) {
        delay = Math.max(0, milli);
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = clean(target);
    }

    /**
     * Serialize.
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

        final boolean printNamespace = (namespace != null);

        pw.print("<");
        pw.print(prefix.toString());
        pw.print(":redelivery ");

        if (printNamespace) {
            pw.print("xml:");
            pw.print(prefix.toString());
            pw.print("=");
            pw.print("\"");
            pw.print(namespace.toString());
            pw.print("\" ");
        }

        if (required) {
            Object defContext = context.getContext(Definition.class);
            if (defContext != null) {
                Definition def = (Definition) defContext;
                try {
                    DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                            String.valueOf(required),
                            def,
                            pw);
                } catch (WSDLException e) {
                    throw new Deflatable.DeflateException(e);
                }
            }
        }

        if (!"".equals(target)) {
            pw.print(ATTR_TARGET + "=" + "\"" + target + "\"");
        }

        if (count > 0) {
            pw.print(ATTR_COUNT + "=" + "\"" + count + "\"");
        }

        if (delay > 0) {
            pw.print(ATTR_DELAY + "=" + "\"" + delay + "\"");
        }

        pw.println("/>");
    }

    private String clean(String value) {
        if (value == null) {
            value = "";
        }
        return value.trim();
    }

    static final String ATTR_TARGET = "target";
    static final String ATTR_COUNT = "count";
    static final String ATTR_DELAY = "delay";

    private volatile QName type = MQConstants.QNAME_REDELIVERY;
    private volatile Boolean required = Boolean.FALSE;
    private volatile int count;
    private volatile int delay;
    private volatile String target = "";
}
