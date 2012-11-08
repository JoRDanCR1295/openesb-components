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
 * @(#)FaultMessage.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.api;

import javax.jbi.messaging.Fault;

/**
 * Fault message a POJO service can throw from its @Operation method.
 *
 * Fault message is sent as is, make sure to wrap the message payload with appropriate
 * JBI WSDL 1.1 wrapper elemenets, if expected by the consuming service.
 *
 * New instance of Fault can be got from this.jbiCtx.getMessageExchange().createFault()
 *
 * Utility methods on Context.createFaultMessage(...) may also be used to get Fault instance
 * populated FaultMessage instance.
 * 
 * @author gpatil
 */
public class FaultMessage extends MessageException {
    private Fault fault;

    public FaultMessage(){
    }

    public FaultMessage(Fault fault){
        this.fault = fault;
    }

    public Fault getFault() {
        return fault;
    }

    public void setFault(Fault fault) {
        this.fault = fault;
    }
}
