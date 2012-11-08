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
 * @(#)WSMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import java.util.Map;
import java.util.Set;

import javax.activation.DataHandler;
import javax.wsdl.Message;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.bpel.model.meta.RVariable;
import java.util.List;
import javax.wsdl.Part;


/**
 * Interface to an object that implements a Web Service Message instance. A Web Service Message
 * consists of a list of named parts.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface WSMessage {
    /**
     * Get one of the parts of this message
     *
     * @param part name of the message part
     *
     * @return object representing that message part
     */
    public Element getPart(String part);

    /**
     * Created a new part with the specified part name. Replaces the old part 
     * if a part with that part name exists already. 
     * 
     * @param partName
     * @return the part element just created.
     */
    public Element createPart(String partName);
    
    /**
     * Does a deep copy of itself. Any changes made to the copy should 
     * not affect the original and vice versa.
     *
     * @return WSMessage The deep copy
     */
    public WSMessage copy();
    
    public Document getDocument(); //JBIW11WrappedMessage    
    public Element getElement(); //JBIW11WrappedMessage

    public QName getMessageType();
    
    public Message getWSDLMessage();
    
    /**
     * The IMAs (Receive\Pick\Event Handlers\Invoke) will mark the external incoming message
     * as external reference.
     */
    public void setAsExternalReference();
    
    /**
     * Add internal reference to the same message
     * 
     * @param variable
     */
    public void addInternalReference(RVariable variable);
    
    /**
     * Remove internal reference to the message 
     * @param variable
     */
    public void removeInternalReference(RVariable variable);
    
    
    /**
     * Returns if the message is referenced only by the passed variable.
     * Note: For the messages that hold external reference, this will return false.
     * 
     * @param variable
     * @return
     */
    public boolean isOnlyReference(RVariable variable);
    
    /**
     * Returns true if all the parts of the JBIMessageImpl have been initialized.
     * @return
     */
    public boolean allPartsInitialized();

    /**
     * Returns the list of non initialized parts or null.
     * @return
     */
    public List<Part> getNonInitializedParts();
    
    Object getNMProperty(String propertyKey);
    
    void setNMProperty(String key, Object val);
    
    Map<String, Object> getNMProperties();
    
    DataHandler getAttachment(String name);
    
    void setAttachment(String name, DataHandler dHdlr);
    
    Set<String> getAttachments();
}
