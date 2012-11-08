/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * DefaultMessageExchangeHandler.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import it.imolinfo.jbi4ejb.Logger;import it.imolinfo.jbi4ejb.LoggerFactory;import it.imolinfo.jbi4ejb.jbi.Messages;import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class DefaultMessageExchangeHandler extends AbstractMessageExchangeHandler {
	/** The logger. */    private static final Logger LOG = LoggerFactory.getLogger( DefaultMessageExchangeHandler.class);        private static final Messages MESSAGES = Messages.getMessages( DefaultMessageExchangeHandler.class);   
    /** Creates a new instance of AbstractMessageExchangeHandler */
    public DefaultMessageExchangeHandler() {
        super();
    }
    
    protected void processError(Exception ex) {
        
        System.out.println("Default MessageExchange Handler processing Error");
        ex.printStackTrace();
        System.out.println("Default MessageExchange Handler processed Error");
    }
    
    protected void processDone() {
        
        System.out.println("Default MessageExchange Handler processed DONE");
    }
    
    protected  void processFault(Fault fault) {
        
        System.out.println("Default MessageExchange Handler processing FAULT");
        System.out.println(fault.toString());
        System.out.println("Default MessageExchange Handler processed FAULT");
    }
    
    protected void processMessage() {
        
        System.out.println("Default MessageExchange Handler processing Message");
        NormalizedMessage inMsg = this.getMessageExchange().getMessage(IN_MESSAGE);
        NormalizedMessage outMsg = this.getMessageExchange().getMessage(IN_MESSAGE);
        System.out.println("IN MESSAGE ::::::: \n" + inMsg );
        System.out.println("OUT MESSAGE ::::::: \n" + outMsg );
        System.out.println("Default MessageExchange Handler processed Message");
    }
    
    protected void validateMessageExchange() throws MessagingException {
        MessageExchange msgExchange = this.getMessageExchange();
        
        if ( this.getMessageExchange() == null ) {        	String msg=MESSAGES.getString("EJB000215_MessageExchange_Object_null_in_DefaultMessageExchageHandler");            LOG.error(msg);            throw new MessagingException(msg);           	 
        }
    }
    
}
