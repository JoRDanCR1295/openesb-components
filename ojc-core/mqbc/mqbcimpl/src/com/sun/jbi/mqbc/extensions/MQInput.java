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
 * @(#)MQInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author rchen
 * @author Noel.Ang@sun.com
 */
public class MQInput implements  Serializable {
    
    private static final long serialVersionUID = 2L;
   
    private final List<MQBCHeader> headers = new ArrayList<MQBCHeader>();
    private volatile MQBCBody mMessagebody;
   
    /** Creates a new instance of MQOutput */
    public MQInput() {
    }
      
    public MQBCBody getMQMessage() {
        return mMessagebody ;
    }

    public void setMQMessage(MQBCBody body) {
        mMessagebody= body;
    }
    
    public MQBCHeader[] getMQHeaders() {
        synchronized (headers) {
            return headers.toArray(new MQBCHeader[headers.size()]);
        }
    }
    
    public void addMQHeader(MQBCHeader header) {
        MQBCHeader replacee = null;
        synchronized (headers) {
            // Remove header if it already exists.
            // Can't instead use List.contains because even if I override
            // MQBCHeader.equals, a correct implementation of it would take
            // into account all of an MQBCHeader's properties. In this search,
            // I want to ignore all of them except its descriptor property.
            for (MQBCHeader aHeader : headers) {
                if (aHeader.getDescriptor() == header.getDescriptor()) {
                    replacee = aHeader;
                    break;
                }
            }
            headers.remove(replacee); // null removal supported by ArrayList.
            headers.add(header);
        }
    }
}
