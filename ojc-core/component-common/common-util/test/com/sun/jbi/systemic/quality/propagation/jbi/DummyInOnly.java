/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.jbi;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

/**
 *
 * @author radval
 */
public class DummyInOnly extends DummyMessageExchange implements InOnly {

    public NormalizedMessage getInMessage() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void setInMessage(NormalizedMessage arg0) throws MessagingException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
