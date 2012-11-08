/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.jbi;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

/**
 *
 * @author radval
 */
public class DummyInOut extends DummyMessageExchange implements InOut {

	private NormalizedMessage mInMessage;
	
	private NormalizedMessage mOutMessage;
	
    public NormalizedMessage getInMessage() {
        return this.mInMessage;
    }

    public NormalizedMessage getOutMessage() {
        return this.mOutMessage;
    }

    public void setInMessage(NormalizedMessage arg0) throws MessagingException {
        this.mInMessage = arg0;
    }

    public void setOutMessage(NormalizedMessage arg0) throws MessagingException {
        this.mOutMessage = arg0;
    }

}
