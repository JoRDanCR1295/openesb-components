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
 * @(#)IMAPEndpoint.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.receive.imap;


import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPAddress;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPBinding;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.config.AppConfig;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class IMAPEndpoint extends EmailBCEndpoint {
    private IMAPAddress imapAddress = null;
    private IMAPBinding imapBinding = null;

    public IMAPEndpoint(ManagerContext ctx, EndpointInfo info, AppConfig appConfig) {
        super(ctx, info, appConfig);
    }

    public IMAPAddress getIMAPAddress() {
        return imapAddress;
    }

    public void setIMAPAddress(IMAPAddress imapAddress) {
        this.imapAddress = imapAddress;
    }

    public IMAPBinding getIMAPBinding() {
        return imapBinding;
    }

    public void setIMAPBinding(IMAPBinding imapBinding) {
        this.imapBinding = imapBinding;
    }
}
