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
 * MockNormalizedMessageBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import javax.activation.DataHandler;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;
import javax.xml.transform.Source;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;


/**
 * No need to create a Mock interface for this class because
 * the class already has the necessary accessor methods.
 */
public class MockNormalizedMessageBasic implements NormalizedMessage, Cloneable {
    private Map properties = new HashMap();
    private Map attachments = new HashMap();
    private Source content = null;

    public void addAttachment(String id, DataHandler dataHandler)
        throws MessagingException {
        attachments.put(id, dataHandler);
    }

    public Source getContent() {
        return this.content;
    }

    public javax.activation.DataHandler getAttachment(String id) {
        return (DataHandler) attachments.get(id);
    }

    public Set getAttachmentNames() {
        if (attachments == null) {
            return Collections.EMPTY_SET;
        } else {
            return attachments.keySet();
        }
    }

    public void removeAttachment(String string) throws MessagingException {
    }

    public void setContent(Source source) throws MessagingException {
        this.content = source;
    }

    public void setProperty(String name, Object value) {
        if (value == null) {
            if (properties != null) {
                properties.remove(name);
            }
        } else {
            properties.put(name, value);
        }
    }

    public void setSecuritySubject(Subject subject) {
    }

    public Set getPropertyNames() {
        if (properties != null) {
            return Collections.unmodifiableSet(properties.keySet());
        }

        return Collections.EMPTY_SET;
    }

    public Object getProperty(String name) {
        return properties.get(name);
    }

    public Subject getSecuritySubject() {
        return null;
    }
}
