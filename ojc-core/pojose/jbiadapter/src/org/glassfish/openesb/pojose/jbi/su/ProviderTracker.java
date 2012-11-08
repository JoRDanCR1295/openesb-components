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
 * @(#)ProviderTracker.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi.su;

import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;

/**
 *
 * @author gpatil
 */
public interface ProviderTracker {
    public MessageExchange getProvisioningMsgEx();
    public boolean isProvisioningExchangeActive();
    
    /**
     * Returns true if @Operation method execution, and @OnReply, @OnFault or
     * @OnError method execution for all the response messages from the services
     * consumed asynchronously is over.
     *
     * @return true when it is ok to execute @OnDone method.
     */
    public boolean canExecuteOnDone();
    public boolean isOperationExecuted();
    public Object getPOJOInstance();
    public POJOClassMetadata getPOJOMetadata();
    public Exception getException();
    public boolean tryAcquireLockForOnDoneExec();

    public void releaseLockForOnDoneExec();
    public void setPOJOMetadata(POJOClassMetadata meta);
    public void setPOJOInstance(Object pojo);
    public void setException(Exception pojo);
    public void setExecutedOperation();

    /**
     * For InOut ME pattern only. When "out" message is sent to the NMR for
     * provisioning ME.
     *
     */
    public void setSentProviderResponse();

    public void setReceivedDone(MessageExchange consME);

    public void setExecutedOnDone();

    /**
     * After executing @OnReply method using response message.
     * Outstanding reply messages counter will be decremented by one.
     * 
     * @param consME
     */
    public void setExecutedOnReply(MessageExchange consME);

    /**
     * After executing @OnFault method using response fault message.
     * Outstanding reply messages counter will be decremented by one.
     *
     * @param consME
     */
    public void setExecutedOnFault(MessageExchange consME);

    /**
     * After executing @OnError method using response error message.
     * Outstanding reply messages counter will be decremented by one.
     *
     * @param consME
     */
    public void setExecutedOnError(MessageExchange consME);

    /**
     * After Done or Error status is recieved from the NMR for provisioning ME.
     * 
     * Also removes {@link ProviderTracker ProviderTracker} from the list in
     * MasterTracker.
     * 
     */
    public void setDoneWithProvisioning();
}
