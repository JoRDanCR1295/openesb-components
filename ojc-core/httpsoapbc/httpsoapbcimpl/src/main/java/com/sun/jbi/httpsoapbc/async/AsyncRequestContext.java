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
 *
 * @author Alexander Lomov
 *
 * Copyright 2011 Open-ESB Community.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.async;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.OperationMetaData;
import java.util.List;
import java.util.Map;
import javax.jbi.messaging.InOut;
import net.java.hulp.measure.Probe;

public class AsyncRequestContext {
    private InOut inout;
    private MessagingChannel channel;    
    private OperationMetaData metadata;
    private Endpoint endpoint;
    private Map<String, List<String>> headers;

    private Probe probe;

    public AsyncRequestContext(InOut inout, MessagingChannel channel, OperationMetaData metadata, Endpoint endpoint){
        this.inout = inout;
        this.channel = channel;        
        this.metadata = metadata;
        this.endpoint = endpoint;
    }
    
    public MessagingChannel getChannel() {
        return channel;
    }

    public void setChannel(MessagingChannel channel) {
        this.channel = channel;
    }

    public InOut getInout() {
        return inout;
    }

    public void setInout(InOut inout) {
        this.inout = inout;
    }
    
    public OperationMetaData getMetadata() {
        return metadata;
    }

    public void setMetadata(OperationMetaData metadata) {
        this.metadata = metadata;
    }

    public Endpoint getEndpoint() {
        return endpoint;
    }

    public void setEndpoint(Endpoint endpoint) {
        this.endpoint = endpoint;
    }

    public Map<String, List<String>> getResponseHeaders() {
        return headers;
    }

    public void setResponseHeaders(Map<String, List<String>> headers){
        this.headers = headers;
    }

    public Probe getProbe() {
        return probe;
    }

    public void setProbe(Probe probe) {
        this.probe = probe;
    }
}
