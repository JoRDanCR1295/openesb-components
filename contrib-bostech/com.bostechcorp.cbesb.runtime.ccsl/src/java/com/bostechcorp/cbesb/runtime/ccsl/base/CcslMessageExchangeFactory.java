/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: CcslMessageExchangeFactory.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.base;

import java.net.URI;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.RobustInOnly;
import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;

public class CcslMessageExchangeFactory implements MessageExchangeFactory {
	MessageExchangeFactory realMessageExchangeFactory;
	Log log;
	
	CcslMessageExchangeFactory(MessageExchangeFactory real, Log log) {
		realMessageExchangeFactory = real;
		this.log = log;
	}

	public MessageExchange createExchange(URI arg0) throws MessagingException {
//		CcslMessageExchange cm = new CcslMessageExchange(realMessageExchangeFactory.createExchange(arg0), log);
//		return cm;
		return realMessageExchangeFactory.createExchange(arg0);
	}

	public MessageExchange createExchange(QName arg0, QName arg1)
			throws MessagingException {
//		CcslMessageExchange cm = new CcslMessageExchange(realMessageExchangeFactory.createExchange(arg0, arg1), log);
//		return cm;
		return realMessageExchangeFactory.createExchange(arg0, arg1);		
	}

	public InOnly createInOnlyExchange() throws MessagingException {
//		CcslInOnly cm = new CcslInOnly(realMessageExchangeFactory.createInOnlyExchange(), log);
//		return cm;
		return realMessageExchangeFactory.createInOnlyExchange();		
	}

	public InOptionalOut createInOptionalOutExchange()
			throws MessagingException {
//		CcslInOptionalOut cm = new CcslInOptionalOut(realMessageExchangeFactory.createInOptionalOutExchange(), log);
//		return cm;
		return realMessageExchangeFactory.createInOptionalOutExchange();		
	}

	public InOut createInOutExchange() throws MessagingException {
//		CcslInOut cm = new CcslInOut(realMessageExchangeFactory.createInOutExchange(), log);
//		return cm;
		return realMessageExchangeFactory.createInOutExchange();		
	}

	public RobustInOnly createRobustInOnlyExchange() throws MessagingException {
//		CcslRobustInOnly cm = new CcslRobustInOnly(realMessageExchangeFactory.createRobustInOnlyExchange(), log);
//		return cm;
		return realMessageExchangeFactory.createRobustInOnlyExchange();		
	}

}
