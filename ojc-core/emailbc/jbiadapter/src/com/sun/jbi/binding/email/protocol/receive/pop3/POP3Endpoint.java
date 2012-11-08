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
 * @(#)POP3Endpoint.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.receive.pop3;


import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Address;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Binding;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.config.AppConfig;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class POP3Endpoint extends EmailBCEndpoint {
    private POP3Address pop3Address = null;
    private POP3Binding pop3Binding = null;

    public POP3Endpoint(ManagerContext ctx, EndpointInfo info, AppConfig appConfig) {
        super(ctx, info, appConfig);
    }

    public POP3Address getPOP3Address() {
        return pop3Address;
    }

    public void setPOP3Address(POP3Address pop3Address) {
        this.pop3Address = pop3Address;
    }

    public POP3Binding getPOP3Binding() {
        return pop3Binding;
    }

    public void setPOP3Binding(POP3Binding pop3Binding) {
        this.pop3Binding = pop3Binding;
    }
}
